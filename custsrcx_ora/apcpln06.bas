        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN06                             *~
            *  Creation Date     - 05/02/96                             *~
            *  Last Modified Date- 04/15/2014                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Primary Planning and Forcasting *~
            *                      Program Utility                      *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                                                           *~  
            *  Special Comments  - (APCPLN1B) - Subroutine to Lookup    *~
            *                                   and Display Planning    *~
            *                                   Tables.                 *~
            *                      (APCPLN0B) - Subroutine to Calc      *~
            *                                   Current Planning Dates. *~
            *                      (APCPLN9B) - Build Sort Index for    *~
            *                                   (APCPLNDT).             *~
            *                      (APCPL11B) - Assign Seq. No.s to a   *~
            *                                   Dept for Prod. Day      *~
            *                      (APCPL1DB) - Debug Report of Planning*~
            *                                   Array Before Analysis.  *~
            *                      (APCPL2DB) - Debug Report of Planning*~
            *                                   Array After Analysis    *~
            *                                                           *~
            *       (AWD015)       (AWDPLN9B) - Build Sort Index for    *~
            *                                   Special Shapes using    *~
            *                                   Tool Sets               *~
            *                                                           *~
            *       Departments  - (095) - Stock Scheduled Production   *~
            *                      (099) - Miscellaneous Product        *~
            *                      (100) - Used Only for Overflow       *~
            *                      (102) - Finished Goods Stock Inv.    *~
            *                      (104) - Finished Windows Special Shp *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * Notes(1)The Sorted Values in DPT$() are obtained from     *~
            *         (APCPLNUC) and Expanded for Seven Days, "If" the  *~
            *         Bucket Has a Capacity and meets the Selection     *~
            *         Criteria Entered on the 'Selection Screen'.       *~
            *                                                           *~
            *      (2)The Process is Driven By Department, A Department *~
            *         at a time is Processed where applicable for each  *~
            *         Line Item on the Sales Order.                     *~
            *                                                           *~
            *Plan   - DPT$() - (8) = Dept   Prod Day, Proc, Shift       *~
            *                         DDD      d       PP     SS        *~
            *                        Subscript to Capacity Buckets      *~
            *Display- DPT$() - (8) = Dept   Proc, Shift Prod Day        *~
            *                         DDD    PP     SS     d            *~
            *                        Subscript to Capacity Buckets      *~
            *         CAP()  -     = Manhour Capacity Buckets Sub DPT$()*~
            *                                                           *~
            *         UNT%() -     = Unit Capacity Buckets Sub DPT$()   *~
            *                                                           *~
            *         PP$( , ) (3) = (DPT$() Sub, 1% to 20%) Models     *~
            *                                                           *~
            *         PU%( , )     = PP$() Sub to Product Buckets for   *~
            *                        DPT$(). Max. 20 and by Definition  *~
            *                        Bucket (20) = Others               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * Notes(3)The Sorted Values in D_DP$() are Associated with  *~
            *         Each Department and the Sort Code from Shift (1). *~
            *         The subscript for the Start and End for each      *~
            *         Department in DPT$() is Stored for Processing.    *~
            *                                                           *~
            *         D_DP$()- (3) = Department Code  ( 1 to 3 )        *~
            *         D_DP$()- (15)= Department Sort Code ( 4 to 20 )   *~
            *         D_SB%(K%,J%) = K% = Subscript for Department      *~
            *                        J% = 1% The Sub Script for Starting*~
            *                                Day of Department in DPT$()*~
            *                                (Not Changed)              *~
            *                                                           *~
            *         *** ------->   J% = 2% The Sub Script for 1st     *~
            *                                Open Day in DPT$() for Dept*~
            *                                (When Bucket is Empty the )*~
            *                                (Value is Incremented to  )*~
            *                                (the Next Bucket, and can )*~
            *                                (Not Exceed 'J%'          )*~
            *                                                           *~
            *                        J% = 3% Then Ending Sub-Script in  *~
            *                                DPT$() for Department      *~
            *                                (Not Changed)              *~
            *                                                           *~
            *                        J% = 4% The Start Day Factor for   *~
            *                                the Department, Process,   *~
            *                                Shift. 'FACTOR%'           *~
            *                                                           *~
            *         D_WT(K%,S%)  = K% = Subscript for Department      *~
            *                        S% = Shift Subscript               *~
            *                        Contains the Decimal Goal Percent. *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * Notes(4)The Results of the Analysis of each Line Item is  *~
            *         Stored in (APCPLNSD), in addition the UPMH and    *~
            *         the Sort Seq. No. for each product is obtained    *~
            *         from (APCPLNDP). The Values are Department        *~
            *         specific.                                         *~
            *                                                           *~
            *         SD$()- (10)  = Contains the Dept,Proc,Shft,Model  *~
            *                        Detail for 'Each' Department Assoc.*~
            *                        with a S.O. Line Item.             *~
            *                        (Created for Specific Product by  )*~
            *                        (Analysis in 'APCPLN6B'           )*~
            *                                                           *~
            *         SDU(),SDS%() = Contains the UPMH and the Product  *~
            *                        Sort Code Assoc. with each SD$()   *~
            *                        Entry.(Values obtained from       )*~
            *                        ( APCPLNDP by 'APCPLN6B'          )*~
            *                                                           *~
            *         LL$()    (10)= Load, St Day, Comp Day, Load Day   *~
            *                        LLLLL   SD       CD        LD      *~
            *-----------------------------------------------------------*~
            * Controls - PLN_MAX% = Loaded Max Size for Planning Sched. *~
            *                       PLAN% = 550% Set Max Size           *~
            *            DPT_MAX% = Loaded Max Number of Department     *~
            *                       Set-Up with Capcacity Values.       *~
            *                       DEPART% = 40% Set Max Departments   *~
            *            LD_MAX%  = Loaded Max Number of Loads Planned  *~
            *                       LOAD% = 120% Set Max No. of Loads   *~
            *            P_MAX%   = Loaded Max No. of Details in        *~
            *                       (APCPLNSD) Assoc. with each Depart. *~
            *                       DTL_MAX% = 6% Set Max No. of Process*~
            *                                     and Shift Records     *~
            *            SHIFTS%  = Set Value for Max No. of Shifts     *~
            *                       Supported. Must be in Sync. with    *~
            *                       DTL_MAX%. Set to Six (6).           *~
            *            SCREEN%  = Max Number of Entries for Display   *~
            *                       Screen's. (SC_MAX%)                 *~
            *                       CC$() and DD$() Need to be DIM Twice*~
            *                       the Value of SCREEN% (240)          *~
            *-----------------------------------------------------------*~
            * Rules    - LOAD_FLG% - Flag that indicates a Load has been*~
            *                        Previously Planned. It will Check  *~
            *                        the Load for Assigned S.O. only    *~
            *                        Status Code '02', All others will  *~
            *                        be skipped. Only change the Dates  *~
            *                        Completion and Load if the Addition*~
            *                        S.O. cause the Dates to be Later.  *~
            *-----------------------------------------------------------*~
            * Notes(5) Special Mods made for UPS and Stock S.O.         *~
            *          1. Stock Sales Orders are assigned a Warranty No.*~
            *          2. Pulls from Inventory Do not have a Warranty   *~
            *             No. assigned.                                 *~
            *          3. Special UPS_FLAG% is Set for Special Orders   *~
            *             which do not deduct Hours from UPMH Capacity. *~
            *          4. Special HOWSHIP Codes for UPS S.O.'s          *~
            *             - 01,02,03,09,11,12,20,21,25                  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/30/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/31/97 ! Mod to 'Not' Assign a Warranty Number to ! RHH *~
            *          !   any Product Pull'd from Stock.         !     *~
            * 01/31/97 ! Special Note, all Stock Product is Assign! RHH *~
            *          !   ed a Warranty Number. (For Later Use)  !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/14/98 ! Y2K                                      ! LDJ *~
            * 05/05/98 ! (EWD001) - Mod to add the Warranty No.   ! RHH *~
            *          !  and Barcode (18) to the new Cross-Ref   !     *~
            *          !  file for Sales Order Look-up and Track  !     *~
            * 05/25/98 ! (EWD002) - Mod to add new Wood Surround  ! RHH *~
            *          !  dt_group$ code obtained from (APCPLNSC) !     *~
            * 10/20/98 ! (EWD003) - Mod for Glass Warranty Parts  ! RHH *~
            * 11/10/98 ! (EWD004) - Mods for Private Label and    ! RHH *~
            *          !  and Configurations                      !     *~
            * 12/15/98 ! (EWD005) - Mods to increase sort codes   ! RHH *~
            *          !  from 12 to 15. pl_sort$ (APCPLN9B)      !     *~
            * 03/31/00 ! (EWD006) - Mods to opening data files    ! RHH *~
            *          !  use EWDOPEN.                            !     *~
            * 04/05/00 ! (EWD007) - Mod to set wood Surround Code ! RHH *~
            * 08/21/00 ! (EWD008) - Mods for new UPS Code = 35    ! RHH *~
            * 07/10/01 ! (EWD009) - Mod for Special Shapes Stock  ! RHH *~
            *          !            will be in department '104'   !     *~
            * 07/26/01 ! (EWD010) - Mod for 215 Dallas, use       ! CMG *~
            *          !            warranty number with 'D'.     !     *~
            * 12/28/01 ! (EWD011) - Mod for date for 2002.        ! CMG *~
            * 01/10/02 ! (EWD012) - Mods for New 'UPS' Howship    ! RHH *~
            *          !              Codes                       !     *~
            * 11/22/02 ! (EWD013) - Mods for Effective Units on   ! CMG *~
            *          !              display screen.             !     *~
            * 06/25/03 ! (EWD014) - Mods to update Oracle with    ! CMG *~
            *          !              Warranty ID Numbers         !     *~
            * 03/17/04 ! (AWD015) - Mods for Special Shapes New   ! RHH *~
            *          !              Sort Routine. (AWDPLN9B)    !     *~
            * 04/23/04 ! (AWD016) - Mod to eliminate duplicate    ! CMG *~
            *          !              Warranties.                 !     *~
            * 05/13/04 ! (AWD017) - Mod to front load woodsurround! CMG *~
            *          !            to prevent overflow           !     *~
            * 08/26/04 ! (AWD018) - Mod to plan all loads instead ! CMG *~
            *          !            one load at a time to pull up !     *~
            *          !            all tempered                  !     *~
            * 12/01/04 ! (AWD019) - Mod to fix that if only plan  ! CMG *~
            *          !        tempered order then header not    !     *~
            *          !        updated.                          !     *~
            * 03/23/05 ! (AWD020) - Mod to have stock not as stat ! CMG *~
            *          !               08, make 03                !     *~
            * 03/23/05 ! (AWD021) - Mod to make buying the farm 30! CMG *~
            * 04/05/05 ! (AWD022) - Mod to APCPLNSD               ! CMG *~
            * 04/06/05 ! (AWD023) - Mod to give 044 and 054 same  ! CMG *~
            *          !     prd date as primary department       !     *~
            * 06/16/05 ! (AWD024) - New file for overflow         ! CMG *~
            * 03/12/06 ! (AWD025) - mods for NE                   ! CMG *~
            * 05/15/06 ! (AWD026) - mods for new APCPLNWT and     ! CMG *~
            *          !              AWDPLNCD                    !     *~
            * 06/15/06 ! (AWD027) - mod to turn off 102 for now   ! CMG *~
            * 09/27/07 ! (AWD028) - mods for 267 flex             ! CMG *~
            * 01/28/2008! (AWD029) mod for casing & SDL in wd srt ! CMG *~
            * 03/19/08 ! (AWD030) - mod for casing sort           ! CMG *~
            * 11/24/2008! (AWD031) - mod for lowes stock sku      ! CMG *~
            *01/08/2009! (AWD032) - mod for oracle                ! DES *~
            *09/30/2009! (AWD033) - mod for not rolling support   ! CMG *~
            *10/23/2009! (AWD034) - mods to add unitid and itemid ! CMG *~
            *          !     to APCPLNWT and oracle DTS_PLAN_DETAIL!CMG *~
            *10/29/2009! (AWD035) - Mods to set Ultra Flag        ! CMG *~
            *03/19/2010! (AWD036) - mod to special first pass chck! CMG *~
            *06/03/2011! (AWD037) - add orcl usr & pswd lookup    ! CMG *~
            *11/15/2011! (AWD038) - mod to look up tempered warr  ! CMG *~
            *07/01/2013! (AWD039) - OGO change ecn 2013-032       ! CMG *~
            *09/02/2013! (AWD040) - mods for thermal warranty num ! CMG *~
            *04/15/2014! (AWD041) - mods for lamn                 ! CMG *~
            *04/14/2015! (IM8017) - mod for sku number            ! CMG *~
            *10/19/2016! CR00671  - Change Sort Code for Duralite ! PWW *~
            *04/18/2017! CR00459  - Set planning indicators       ! RDB *~
            *05/08/2017!          - Add the user ID indicator chgs! RDB *~
            *10/24/2018! CR1726   - Write Ply Gem trigger         ! RDB *~
            *10/29/2018! CR1726   - Adjust order line time        ! RDB *~
            *12/20/2108! CR1726   - Include Dev and ordrattr field! RDB *~
            *02/20/2109! CR1934   - Expand SKU size to 10         ! RDB *~
            *03/06/2019!          - Stop A loads in PG trigger    ! RDB *~
            *05/14/2019! CR2023   - Add the A loads back to triggr! RDB *~
            *11/02/2021! CR2756   - eCat no longer valid for print! RDB *~ 
            *************************************************************
                     
        dim                              /* (APCPLNUC) - FILE          */~
            filename$8, dt_bar$18,            /* by EWDOPEN            */~
            rhh$5, rh1$10, dt_index$30,  /* Debug Values               */~
            d1$32, d2$10, d3$10, d4$10,  /* Debug Values               */~
            d5$32, d6$10, d7$10, d8$10,  /* Debug Values               */~
            ss$(6%)40, sk$(100%)79,      /* Debug Text                 */~
            bg_yr$2, bg_year$4,          /* Production Year            */~
            bg_wk$2, bg_dte$6,bg_date$10,/* Production Week            */~
            bg_dept$3,  bg_dept_d$30,    /* Beginning Department Code  */~
            bg_proc$2,  bg_proc_d$30,    /* Beginning Process Code     */~
            bg_shft$2,  bg_shft_d$30,    /* Beginning Shift Code       */~
            bg_day$1,   bg_day_d$9,      /* Starting Production Day    */~
            bg_day_dte$10,               /* Calculated Date for BEG Day*/~
            ed_dept$3,  ed_dept_d$30,    /* Ending    Department Code  */~
            ed_proc$2,  ed_proc_d$30,    /* Ending    Process Code     */~
            ed_shft$2,  ed_shft_d$30,    /* Ending    Shift Code       */~
            ed_day$2,   ed_day_d$9,      /* Ending Production Day      */~
            ed_day_dte$10,               /* Calculated Date for END Day*/~
            pln_dte$6, pln_day$2,        /* Production Week Prod Day   */~
            jdate1$7,                    /* Use for Julian Date Calc   */~
            current$13                   /*                            */
    
        dim                              /* (Program) - Variables      */~
            cur_yr$2, prv_yr$2,          /* Current and Previous Year  */~
            cur_wk$2, cur_dy$1,          /* Current Week and Day       */~
            cur_dte$6, cur_date$10,      /* Calc of Prod. Date         */~
            ent_yr$2,                    /* Entry Year                 */~
            ent_wk$2, ent_dy$1,          /* Entry Week and Day         */~
            ent_dte$6, ent_date$10,      /* Entry Calc of Prod. Date   */~
            days$(14%)9,                 /* Screen and Days of Week    */~
            tab$(10%)10,                 /* Save Code Table Names      */~
            desc$30,                     /* TABLE VALUE, DESCRIPTION   */~
            code$15,                     /* Use To Look-Up Table Code  */~
            date$8, rpt_time$8,          /* Current Date               */~
            date_ufmt$8,                 /* Today's date as CCYYMMDD   */~
            readkey$25, gen$24,          /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            ty$2,                        /* (EWD009) Glass Code        */~
            userid$3,                    /* Current User Id            */~
            apcwood_code$3,              /* Wood Code    (AWD017)      */~
            wood_sort$1,                 /* Sort Wood    (AWD017)      */~
            specialmull$1,               /* (AWD030) casing sort       */~
            plng_pgm$8,                  /* Program name currenlty plng*/~
            plnguser$3                   /* Planning user ID           */

        dim f2%(30%),                    /* = 0 if the file is open    */~
            f1%(30%),                    /* = 1 if READ was successful */~
            fs%(30%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(30%)20                 /* Text from file opening     */
                                         /* Planning Arrays            */
        dim dpt$(550%)8, sv_root$8,      /* Dept,Day,Proc,Shft-Sub Scrp*/~
            cap(550%),   sv_root1$8,     /* DPT$() Capacity Buckets    */~
            unt%(550%),  plan$30,        /* DPT$() Unit/Windows Bucket */~
            ef_unt%(550%),               /* Effective Units   (EWD013) */~
            plan_line$18, spc$45,        /* S.O. Line Items Planned    */~
            plan_line1$12,               /* Display Skipped Lines      */~
            pp$(550%,20%)3,              /* DPT$() Product/Mod Buckets */~
            pu%(550%,20%),               /* DPT$() Product/Mod Units   */~
            d_dp$(40%)18,                /* Save Depart Codes (EWD005) */~
            d_sb%(40%,4%),               /* Store Department Postion   */~
            d_wt(40%,6%),                /* Store Department UPMH Wt   */~
            sd$(6%)10%, sv_dept$3,       /* Save Dept,Proc,Shift,Model */~
            sdu(6%), sv_month$2,         /* Save UPMH's                */~
            sds%(6%), sv_shft$2,         /* Save Product Seq No.       */~
            ll$(120%)11,                 /* Loads, Start Day, Comp Day,*/~
                                         /* Load Day                   */~
            hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            line1$30,                    /* Display Header             */~
            h1$25, h2$11,                /* Column Headings - Left Side*/~
            h3$25, h4$11,                /* Column Headings - Right Sid*/~
            sel_d$2, sel_s$2,            /* Display Day/Shift Selection*/~
            sel_d_dte$10, sel_s_d$15,    /* Display Day/Shift Selection*/~
            cc$(240%)25,                 /* Description Half of Screen */~
            dd$(240%)11, pl_unta%(7%),   /* Value Half of Screen       */~
            pl_unte%(7%),                /* Cap Eff Unit  (EWD013)     */~
            pl_unts(7%), wrk$25,         /* Sched Hours for 7 Days     */~
            pl_key$11, pl_day$1,         /* Load Capacity Key          */~
            b_dte$10, e_dte$10,          /* Beg/End Date               */~
            ld_key$13, ld_load$5,        /* Load Key and Number        */~
            ld_day$1, ld_status$2,       /* Start Day and Load Status  */~
            ld_dtp1$10, lx_dtp1$10,      /* Load Production Date       */~
            ld_dtp2$10, lx_dtp2$10,      /* Load Completion Date       */~
            ld_dtp3$10, lx_dtp3$10,      /* Truck Load Date for Load   */~
            ld_sister$2,                 /* Sister Company Code  EWD010*/~
            lx_status$2, sc_st$2,        /*                            */~
            xx_st$2,                     /* (EWD003) - Glass Warranty  */~
            sc_key$27, sd_key$23,        /* Scheduling Keys            */~
            sav_sd_key$23,               /* SavKey-PrevRec (AWD023)    */~
            sc_sav$10, sd_key1$23,       /* Sales Order and Line Item  */~
            sc_special$10,               /* Special Flags    (AWD016)  */~
            sc_rec$128, dt_rec$256,      /* Schedule and Detail Records*/~
            ad_key$33, ad_rec1$64,       /* Primary Key Audit File     */~
            ad_rec$64, ad_time$8,        /* Planning Audit Record      */~
            dt_key$23, dt_part$25,       /* Detail Primary Key         */~
            sc_part$25,                  /* Sc Part Number (AWD035)    */~
            sav_part$25, dt_ref$8,       /* Save for Shape Stock EWD001*/~
            sp_part$25, spec_part$25,    /* (EWD009)                   */~
            dt_sort$5, stk_key1$32,      /* Product Sort Code          */~
            dt_special$10, sc$1,         /* Special Flags              */~
            load_group$17,               /* Load,D Seq,Drop,Cust Sort  */~
            dt_ln_item$2,                /* S.O. Line Item             */~
            dt_item_no$4,                /* Line Item Counter          */~
            dt_so_line$10,               /* S.O. and Line Item         */~
            dt_cust$9, dt_txt$10,        /* Customer Code              */~
            pl_sort$12,sort_key$60,      /* New Sort Index             */~
            pl_sort1$3, pl_sorts$15,     /* More Sort Codes (EWD005)   */~
            dt_shft$2, dt_proc$2,        /* Shift and Process Codes    */~
            dt_st$2, or_status$2,        /* Production Status Code     */~
            dt_qty$4, dt_time$8,         /* Total Quantity of S.O.     */~
            warranty$8, page$16,         /* Store (000) Warranty No.   */~
            dt_group$1,                  /* (EWD002) - Group Code      */~
                                         /*   used for configurations  */~
            dt_config$2,                 /* Configuration Code (EWD004)*/~
            dt_prv$2,                    /* Private Label Code (EWD004)*/~
/*AWD026*/  wt_rec$128,                  /* (EWD001) - Warranty C/R    */~
            wt_key$8,                    /* APCPLNWT readkey (AWD016)  */~
/*AWD026*/  wt_key2$18,                  /* APCPLNWT readkey 2         */~
            or_hows$2, hows$50, sav_so$8,/* How Ship, Codes, S.O.      */~
            store_key$18,                /* STORE (000)         (EWD010)*/~
            bck_key$25,                  /* BCKMASTR key  CR2756        */~     
            bck_user_entered$3,          /* eCat user entry user check  */~
/*AWD031*/  sku$10                       /* Lowes sku number    CR1934  */

        dim                              /*  (EWD014)                  */~
            bar_key$18,                  /* New Barcode File Readkey   */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            stmt3$250,                   /* Second Query String        */~
            stmt4$250,                   /* Second Query String        */~
            stmt5$250,                   /* Second Query String        */~
            stmt6$250,                   /* Second Query String        */~
            stmt7$250,                   /* Second Query String        */~
            stmt8$250,                   /* Second Query String        */~
            error$256,                   /* Error String               */~
            field$256,                   /* Query Return Field         */~
            name$50,                     /* Field Name Returned        */~
            oradate$10                   /* ORA Date                   */

        dim over_key$27,                 /* Overflow Key   (AWD024)    */~
            def_desc$56                  /* AWDDEFIN Descr (AWD025)    */

        dim                              /* (AWD026)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            dt_sub_part$20,              /* New Sub Part No.           */~
            dt_sub_info$20,              /* New Sub Info Fields (9)+11 */~
            casing$1,                    /* Casing/Bullnose (AWD029)   */~
            sdl$1                        /* SDL (AWD029)               */

        dim act_pln%(7%)                 /* Actual Primary Dept Plan   */
                                         /* Each Day Roll Over (AWD033)*/

        dim itemid$10, unitid$10         /* (AWD034) itemid and unitid */

        dim awdschdt_key2$18,            /* (AWD038) barcode awdschdt  */~
            awdschdt_ref$8               /* (AWD038) awdschdt warr     */

        dim schema$8                     /* Schema                     */
        dim cmg$40

        dim warrther$18,                 /* (AWD040) thermal lookup    */~
            warrtherId$9,                /* (AWD040) thermal warranty  */~
            descr$30,                    /* CR00671                    */~
            intercept$2                  /* CR00671                    */

        dim tr_rec$256,                  /* Trigger record             */~
            tr_key$11,                   /* Trigger key                */~
            filetype$20,                 /* Trigger file type          */~
            transmit$1,                  /* 0 not sent, 1 sent         */~
            pgdate$6,                    /* Trigger date               */~
            time$6, timeord$6,           /* Trigger time               */~
            filetype2$20,                /* Trigger file type          */~
            salesorder$8,                /* Sales Order Number         */~
            linenbr$3,                   /* Sales Order Line Number    */~
            upddte$6,                    /* Updated Trigger status date*/~
            updtime$6,                   /* Updated Trigger status time*/~
            filler1$180                  /* Filler space               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Planning Master Utility (** Analysis **)"
            pname$ = "APCPLN06 - Rev: A1.00"

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
            * #1  ! APCPLNUC ! Planning Master Unit Capacity File       *~
            * #2  ! APCPLNDP ! Planning Master Department File          *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #4  ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #5  ! APCPLNSC ! Planning Master Scheduling File          *~
            * #6  ! APCPLNSD ! Planning S.O. Schedule Dept Detail       *~
            * #7  ! APCPLNDT ! Production Master Detail File            *~
            * #8  ! APCPLNAD ! Production Scanning Audit File           *~
            * #9  ! APCPLNOR ! Plannin S.O. Header Master               *~
            * #10 ! APCPLXXX ! Sort Planning Data for Analysis          *~
            * #11 ! STORNAME ! Master Store Definition File (000)       *~
            * #12 ! APCSTOCK ! Master Stock Inventory File              *~
            * #13 ! DTHOLDFL !                                          *~
            * #14 ! BCKLINES ! Sales Order Line item File (EWD004)      *~
/*AWD026*/  * #15 ! APCPLNWT ! Master Warranty Cross-Ref File (EWD001)  *~
            * #16 ! SYSFILE2 ! MASTER STORE FILE               (EWD010) *~
            * #18 ! AWDBARCD ! Warranty Rec to Update ORACLE   (EWD014) *~
            * #20 ! OVERFLOW ! Flowflow file listing                    *~
            * #21 ! AWDDEFIN ! Number Starting Number definions(AWD025) *~
/*AWD026*/  * #22 ! AWDPLNCD ! New Master Warranty Cross-Dock file      *~
            * #23 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #24 ! AWDSCHDT ! NC Tempered Warranty Numbers             *~
            * #25 ! WARRTHER ! Thermal Warranty Numbers        (AWD040) *~
/*AWD026*/  * #63 ! BCKSUBPT ! New Sub Part Number File                 *~
/*CR1726*/  * #26 ! PGSCHDTR ! Ply Gem Schedule Trigger file            *~
/*CR1726*/  * #27 ! PGORLNTR ! Ply Gem Order Line Trigger file          *~
/*CR1726*/  * #28 ! PGORATTR ! Ply Gem Attribute Trigger file           *~
/*CR2756*/  * #29 ! BCKMASTR ! Backlog master file                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNUC",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 11,                      ~
                        alt key 1, keypos =  7, keylen = 11

            select #2,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #4,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #5,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  24,  keylen =  10,                     ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33
/* (AWD022) - Mod to key and reclen */
            select #6,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   1,  keylen =  23

            select #7,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #8,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  19,  keylen =  33,                     ~
                        alt key 1, keypos =  1, keylen = 33

            select #9,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =   1,  keylen =  51,                     ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup
/* (EWD013) - Change recsize from 37 to 39                              */
            select #10, "APCPLXXX",                                      ~
                        varc,     indexed,  recsize =   39,              ~
                        keypos =   1,  keylen =  25

            select #11, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #12, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =    70,             ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =    8, keylen =  32

       /* <AWD032> */
            select #13, "DTHOLDFL",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
       /* </AWD032> */

                                                    /* (EWD004) Begin */
            select #14, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

                                                    /* (EWD004) End   */

                                                    /* (EWD001) Begin */
            select #15, "APCPLNWT",                                      ~
/*(AWD026)*/            varc,     indexed, recsize = 128,                ~
                        keypos =    1, keylen = 8,                       ~
                        alt key  1, keypos =  9, keylen = 10, dup,       ~
                            key  2, keypos =  9, keylen = 18
                                                    /* (EWD001) End   */

                                                    /* (EWD010) Begin */
            select #16, "SYSFILE2"                                        ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 20
                                                    /* (EWD010) End   */

           select #18, "AWDBARCD"                                        ~
                        varc,     indexed, recsize = 124,                ~
                        keypos =    1, keylen = 18,                      ~
                        alt key  1, keypos =  19, keylen = 8

            select #20, "OVERFLOW"                                       ~
                        varc,     indexed, recsize = 124,                ~
                        keypos =    1, keylen = 20

            select #21, "AWDDEFIN",                                      ~
                        varc,   indexed,       recsize =  64,            ~
                        keypos =  1,   keylen  =   8

            select #22, "AWDPLNCD",                                      ~
/*(AWD026)*/            varc,     indexed, recsize = 128,                ~
                        keypos =    1, keylen = 8,                       ~
                        alt key  1, keypos =  9, keylen = 10, dup,       ~
                            key  2, keypos =  9, keylen = 18,            ~
                            key  3, keypos = 92, keylen =  8, dup

           select #23,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20
/* (AWD038) */
           select #24, "AWDSCHDT",                                       ~
                        varc,     indexed,  recsize = 1012,              ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   29, keylen =  28,         ~
                            key  2, keypos =   39, keylen =  18,         ~
                            key  3, keypos =   21, keylen =   8


/* (AWD036) */
            select #25, "WARRTHER",                                       ~
                        varc,     indexed, recsize = 256,                 ~
                        keypos = 1,    keylen = 30,                       ~
                        alt key  1, keypos =    4, keylen =  18,          ~
                            key  2, keypos =   22, keylen =   9


            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup
/*CR1726 */
            select #26, "PGSCHDTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
/*CR1726 */
            select #27, "PGORLNTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
/*CR1726 */
            select #28, "PGORATTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
/*CR2756 */ 
      
            select #29, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup                            
                            

            call "SHOSTAT" ("Initialization")
                                                    /* (EWD006)       */
            filename$ = "APCPLNUC" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNDP" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNLD" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNSC" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNSD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNDT" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNAD" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNOR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "STORNAME" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error

            REM filename$ = "APCSTOCK"  call "EWDOPEN" (#12, filename$, err%)
            REM if err% <> 0% then gosub open_error

            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 100%, rslt$(12%))

            call "OPENCHCK" (#13, fs%(13%), f2%(13%), 500%, rslt$(13%))

            filename$ = "BCKLINES" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNWT" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
                                                       /*  (EWD010)     */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
                                                         /* (EWD006)    */
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            if fs%(10%) <> 1% then goto L03060
               call "FILEBGON" (#10)

L03060:

REM  call "OPENCHCK" (#18, fs%(18%), f2%(18%), 0%, rslt$(18%))

REM  Note Do I really want to delete??  What if program fails??
REM  For Now I am going to say not to delete but I will leave code if I
REM  need to put it back in.
REM                   if fs%(18%) <> 1% then goto L03080
REM                     call "FILEGBON" (#18)

REM   L03080:
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 100%, rslt$(18%))

            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 100%, rslt$(20%))

/* (AWD025) */
            filename$ = "AWDDEFIN" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error

/* (AWD026) - begin */

            call "OPENCHCK" (#22, fs%(22%), f2%(22%), 100%, rslt$(22%))

/*(AWD037) */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error

/*(AWD038) */
            filename$ = "AWDSCHDT" : call "EWDOPEN" (#24, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#25, fs%(25%), f2%(25%), 100%, rslt$(25%))

            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error

/* (AWD026) - end   */

/* CR1726*/ filename$ = "PGSCHDTR" : call "EWDOPEN" (#26, filename$, err%)
            if err% <> 0% then gosub open_error

/* CR1726*/ filename$ = "PGORLNTR" : call "EWDOPEN" (#27, filename$, err%)
            if err% <> 0% then gosub open_error            
            
/* CR1726*/ filename$ = "PGORATTR" : call "EWDOPEN" (#28, filename$, err%)
            if err% <> 0% then gosub open_error
            
/* CR2756*/ filename$ = "BCKMASTR" : call "EWDOPEN" (#29, filename$, err%)
            if err% <> 0% then gosub open_error
                        
              mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)

            tab$(1%) = "PLAN DEPT" : tab$(2%) = "PLAN PROC"
            tab$(3%) = "MODEL    " : tab$(4%) = "PLAN SHFT"
            tab$(5%) = "PLANFRONT" : tab$(6%) = "SOS SKU"    /* (IM8017) */

            days$(1%) = "MONDAY   "  : days$( 8%) = "MONDAY   "
            days$(2%) = "TUESDAY  "  : days$( 9%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY"  : days$(10%) = "WEDNESDAY"
            days$(4%) = "THURSDAY "  : days$(11%) = "THURSDAY "
            days$(5%) = "FRIDAY   "  : days$(12%) = "FRIDAY   "
            days$(6%) = "SATURDAY "  : days$(13%) = "SATURDAY "
            days$(7%) = "SUNDAY   "  : days$(14%) = "SUNDAY   "

            date$ = date                     /* Set The Current Date   */
            oradate$ = date
            call "DATEFMT" (date$,x%,date_ufmt$)
            call "DATFMTC" (oradate$)

            ss$(1%) = "Note - 'Stop The Show' Cannot Continue  "
            ss$(2%) = "       Planning/Analysis. The Data is   "
            ss$(3%) = "       not Accurate and May lead to     "
            ss$(4%) = "       Invalid Conclusions. Stop. Stop, "
            ss$(5%) = "  C a l l   S y s t e m   S u p p o r t "
            ss$(6%) = "                                        "
        REM Set the Special Codes for UPS
                                                    /* (EWD008) Code=35 */
            hows$ = "***       * *           * *       ************    "
                                                  /* (EWD012) New Codes */
                                                  /* 36 thru 46         */

            ups_flag% = 0%                          /* Flag for Special */
                                                    /* Sales Orders     */

            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)
 
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 11%              /*  (AWD017)  */
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 13% then gosub assign_seq_no
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            plng_err%  = 0%
            anal_update% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 30% then anal_update% = 1%
                  if keyhit%  = 30% then gosub Plng_Ind
                                /* chk schema against opposition location     */
                    if plng_err% = 1%  then editpg1
                  if keyhit%  = 30% then gosub Plng_Set
                              /* set the planning indicator to 1 by schema    */
                    if plng_err% = 1%  then editpg1
                  if keyhit%  = 10% then gosub plan_analysis
                                                        /* (AWD021) */
                  if keyhit%  = 30% then gosub plan_analysis
                  if keyhit%  = 13% then gosub assign_seq_no
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 11% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11160

        REM *************************************************************~
            *        D i s p l a y   S c r e e n   E d i t              *~
            *************************************************************

        display_mode
            init(" ") sel_d$, sel_d_dte$, sel_s$, sel_s_d$
        editpg2
            gosub L53110
            gosub L53310
            kk% = 0%
            lastfieldnr% = 0%
            gosub'102(0%)                          /* Selection's Only */
                  if keyhit%  = 16% and anal_update% = 1%     ~
                    then gosub Plng_Reset     /* Set schema planning indicator to 0 on exit */
                  if keyhit%  = 16% then return
                  if keyhit% <>  0% then       editpg2
                     dept% = 0%             /* Check to see if a       */
                     lin%  = cursor%(1%)    /* Department was Selected */
                     col%  = cursor%(2%)
                     dept% = lin% - 6%
                     if dept% < 1% then goto L12190
                     if col% > 39% then dept% = dept% + 15%
L12190:              keyhit% = 0%
                     if dept% < 1% then goto L12230
                     goto editpg2

L12230:     dept% = 0%
            goto editpg2
        return

        REM   *******************************************************
        REM   * Check, set and reset planning indicators            *
        REM   *******************************************************

Plng_Ind:         /*  Checking to see if NC and TX planning at the same time  */
            init(" ") gen$, def_desc$, plng_pgm$, plnguser$
            plng_err% = 0%
            ncplan% = 0%
            txplan% = 0%
            ncendpln58% = 0%
            txendpln58% = 0%

            if schema% = 1% then goto L15030
            /* if NC planning, check texas indicator else check nc indicator  */

            str(gen$,1%,8%)  = "NCPLAN"
               /* set to get nc planning indicator */

            read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15020
                FMT XX(01),  CH(01)

            ncplan$ = str(def_desc$,1%,1%)
            convert ncplan$ to ncplan%, data goto L15020

               /* Find user ID who set indicator */
            str(gen$,1%,8%)  = "NCUSERID"
            read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15021
                FMT XX(01),  CH(03)
            plnguser$ = str(def_desc$,1%,3%)

            if ncplan$ = "1" then plng_pgm$ = "APCPLN06"
            if ncplan$ = "1" then goto L15010
            /* if schema is texas and planning happening by nc, display error */

            str(gen$,1%,8%)  = "NCENDPLN"
                  /* set to get nc planning 58 indicator too */

            read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15020
                FMT XX(01),  CH(01)


            ncendpln58$ = str(def_desc$,1%,1%)
            convert ncendpln58$ to ncendpln58%, data goto L15020

            str(gen$,1%,8%)  = "NCENDID"
            read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15021
                FMT XX(01),  CH(03)
            plnguser$ = str(def_desc$,1%,3%)

            if ncendpln58$ = "1" then plng_pgm$ = "EWDPLN58"
            if ncendpln58$ = "1" then goto L15010
            /* if schema is texas and planning happening by nc, display error */
        return

L15010:     plng_err% = 1%
            call "SHOSTAT" ("NC is Currently Planning in Program "&plng_pgm$& ~
                " by "& plnguser$)
            stop
        return

L15020:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading NC Planning Indicator--> "&gen$)
          stop
        return

L15021:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading NC Planning User ID--> "&gen$)
          stop
        return

L15030:    str(gen$,1%,8%)  = "TXPLAN"   /* set to get tx planning indicator  */

           read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15050
              FMT XX(01),  CH(01)

           txplan$ = str(def_desc$,1%,1%)
           convert txplan$ to txplan%, data goto L15050

           str(gen$,1%,8%)  = "TXUSERID"
           read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15051
                FMT XX(01),  CH(03)
           plnguser$ = str(def_desc$,1%,3%)

           if txplan$ = "1" then plng_pgm$ = "APCPLN06"
           if txplan$ = "1" then goto L15040
               /* if schema is nc and planning happening by tx, display error */

           str(gen$,1%,8%)  = "TXENDPLN"
                                  /* set to get tx planning 58 indicator too */

           read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15050
                 FMT XX(01),  CH(01)

           txendpln58$ = str(def_desc$,1%,1%)
           convert txendpln58$ to txendpln58%, data goto L15050

           str(gen$,1%,8%)  = "TXENDID"
           read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L15051
                FMT XX(01),  CH(03)
           plnguser$ = str(def_desc$,1%,3%)

           if txendpln58$ = "1" then plng_pgm$ = "EWDPLN58"
           if txendpln58$ = "1" then goto L15040
               /* if schema is nc and planning happening by tx, display error */
        return

L15040:     plng_err% = 1%
            call "SHOSTAT" ("TX is Currently Planning in Program "&plng_pgm$& ~
                 " by "& plnguser$)
            stop
        return

L15050:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading TX Planning Indicator--> "&gen$)
          stop
        return

L15051:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading TX Planning User Id--> "&gen$)
          stop
        return          /* End of planning indicator check   */

Plng_Set:           /* Set planning indicator for schema currently planning  */
            plng_err% = 0%

            if schema% = 1% then goto L15070        /* if NC then set TX  */

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXPLAN"           /* For TX plan flag set  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15060
               FMT XX(01),  CH(01)
            txplan$ = "1"
            str(def_desc$,1%,1%) = txplan$

            put #21, using def_fmt1, def_desc$

            rewrite #21

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXUSERID"           /* For TX plan user ID  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15061
               FMT XX(01),  CH(03)
            txuserid$ = userid$
            str(def_desc$,1%,3%) = txuserid$

            put #21, using def_fmt1, def_desc$

            rewrite #21
        return

L15060:   plng_err% = 1%
          call "SHOSTAT" ("Error - Loading TX Planning Indicator--> "&def_desc$)
          stop
        return

L15061:   plng_err% = 1%
          call "SHOSTAT" ("Error - Loading TX Planning User ID--> "&def_desc$)
          stop
        return

L15070:     init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCPLAN"           /* For NC plan flag set  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15080
                 FMT XX(01),  CH(01)
            ncplan$ = "1"
            str(def_desc$,1%,1%) = ncplan$

            put #21, using def_fmt1, def_desc$

            rewrite #21

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCUSERID"           /* For NC plan user ID  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15081
                 FMT XX(01),  CH(03)
            ncuserid$ = userid$
            str(def_desc$,1%,3%) = ncuserid$

            put #21, using def_fmt1, def_desc$

            rewrite #21
        return

L15080:   plng_err% = 1%
          call "SHOSTAT" ("Error - Loading NC Planning Indicator--> "&def_desc$)
          stop
        return

L15081:   plng_err% = 1%
          call "SHOSTAT" ("Error - Loading NC Planning User ID--> "&def_desc$)
          stop
        return               /* End of planning set indicator   */


Plng_Reset:    /* Reset planning indicator for schema on exit  */
            anal_update% = 0%

            if schema% = 1% then goto L15100

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXPLAN"      /* reset for TX plan indicator  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15090
                 FMT XX(01),  CH(01)
            txplan$ = "0"
            str(def_desc$,1%,1%) = txplan$

            put #21, using def_fmt1, def_desc$

            rewrite #21

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXUSERID"      /* reset for TX plan user ID  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15091
                 FMT XX(01),  CH(03)
            txuserid$ = "   "
            str(def_desc$,1%,3%) = txuserid$

            put #21, using def_fmt1, def_desc$

            rewrite #21
        return

L15090:   call "SHOSTAT" ~
               ("Error - Resetting TX Planning Indicator--> "&def_desc$)
          stop
        return

L15091:   call "SHOSTAT" ~
               ("Error - Resetting TX Planning User ID--> "&def_desc$)
          stop
        return

L15100:     init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCPLAN"  /* reset for NC plan indicator  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15110
                 FMT XX(01),  CH(01)
            ncplan$ = "0"
            str(def_desc$,1%,1%) = ncplan$

            put #21, using def_fmt1, def_desc$

            rewrite #21

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCUSERID"  /* reset for NC plan user ID  */
            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L15111
                 FMT XX(01),  CH(03)
            ncuserid$ = "   "
            str(def_desc$,1%,3%) = ncuserid$

            put #21, using def_fmt1, def_desc$

            rewrite #21
        return

L15110:   call "SHOSTAT" ~
               ("Error - Reset NC Planning Indicator--> "&def_desc$)
          stop
        return

L15111:   call "SHOSTAT" ~
               ("Error - Reset NC Planning User ID--> "&def_desc$)
          stop
        return

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

L19060: %+---------------------------------------------------------------~
        ~--------------+
L19080: %!(Planning Error Rpt)         APC Building Products             ~
        ~              !
L19100: %!########@########                                              ~
        ~              !
L19120: %!Load !Sales Order! Line! Mod!CL!Gl!Lt!Hg!Sc!Lk!Width!Height!Clm~
        ~r !WWid ! Dept!
L19140: %!-----!-----------!-----!----!--!--!--!--!--!--!-----!------!---~
        ~--!-----!-----!
L19160: %!#####! ########  ! ##  ! ###!# !##!##!##!# !# ! ####! ###  ! ##~
        ~# ! ### ! ### !
        print_hdr
            if rh% > 20% then print using L19060
            print page
            init(" ") rpt_time$
            call "TIME" (rpt_time$)
            print using L19060
            print using L19080
            print using L19100, date$, rpt_time$
            print using L19120
        return

        print_report
          if plan_sk% = 0% then return

          select printer(134)
          for rh% = 1% to plan_sk%
            if rh% = 1% or rh% = 51% then gosub print_hdr
            print using L19140
            print using L19160, str(sk$(rh%),1%,5%),    /* Load Number */ ~
                               str(sk$(rh%),6%,8%),    /* Sales Order */ ~
                               str(sk$(rh%),14%,2%),   /* Line Item   */ ~
                               str(sk$(rh%),16%,3%),   /* Model Code  */ ~
                               str(sk$(rh%),19%,1%),   /* Color Code  */ ~
                               str(sk$(rh%),20%,2%),   /* Glass Code  */ ~
                               str(sk$(rh%),22%,2%),   /* Liting Code */ ~
                               str(sk$(rh%),24%,2%),   /* Hinge Code  */ ~
                               str(sk$(rh%),26%,1%),   /* Screen Code */ ~
                               str(sk$(rh%),27%,1%),   /* Lock Code   */ ~
                               str(sk$(rh%),28%,4%),   /* Width Size  */ ~
                               str(sk$(rh%),32%,3%),   /* Height Size */ ~
                               str(sk$(rh%),35%,3%),   /* Wood/Fact/CL*/ ~
                               str(sk$(rh%),38%,3%),   /* Depth       */ ~
                               str(sk$(rh%),51%,3%)    /* Department  */

          next rh%
          print using L19060
          close printer
        return


        assign_seq_no
             call "APCPL11B"
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
         "Enter a Valid Planning Production Year or <Return> = Current?",~
         "Enter a Valid Planning Production Week or <Return> = Current?",~
         "Enter a Valid Beginning Department Code or (ALL)?            ",~
         "Enter a Valid Ending Department Code or (All)?               ",~
         "Enter a Valid Beginning Process Code or (AA) = All?          ",~
         "Enter a Valid Ending Process Code or (AA) = All?             ",~
         "Enter a Valid Beginning Shift Code or (AA) = All?            ",~
         "Enter a Valid Ending Shift Code or (AA) = All?               ",~
         "Enter a Valid Starting Production Day or (A) = All?          ",~
         "Enter a Valid Ending Production Day or (A) = All?            ",~
         "Sort Wood Surround At Beginning?                             "
/* (AWD017) */
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, bg_yr$, bg_year$,~
                      bg_wk$, bg_dept$, bg_dept_d$, ed_dept$, ed_dept_d$,~
                      bg_proc$, bg_proc_d$, ed_proc$, ed_proc_d$,        ~
                      bg_shft$, bg_shft_d$, ed_shft$, ed_shft_d$,        ~
                      bg_day$, bg_day_d$, ed_day$, ed_day_d$, current$,  ~
                      bg_day_dte$, ed_day_dte$, jdate1$, pln_date$,      ~
                      pln_dte$, pln_day$, bg_date$, dpt$(), pp$(), ll$(),~
                      cc$(), dd$(), sv_month$, sv_dept$, sv_shft$,       ~
                      d_dp$(), sd$(), warranty$, pl_sort$, sort_key$,    ~
                      rhh$, rh1$, sk$(), sav_so$, or_hows$, pl_sort1$,   ~
                      pl_sorts$, wood_sort$
                                                  /* (AWD017)         */
                                                  /* (EWD005)         */

/*(AWD026) */
            init(" ") flag$, pgm$, so_inv$, item_no$, bcksubpt_rec$,     ~
                      flds$(), info_flds$(), dt_sub_part$, dt_sub_info$, ~
                      wt_key2$


            init(" ") error$, field$, name$, ty$
            shapes% = 0%
            mat cap  = zer
            mat unt% = zer
            mat ef_unt% = zer                     /*  (EWD013)        */
            mat pu%  = zer
            mat d_sb% = zer
            mat d_wt  = zer
            mat sdu   = zer
            mat sds%  = zer

            plan%    = 550%                        /* PLN_MAX% Loaded */
            depart%  = 40%                         /* DPT_MAX% Loaded */
            load%    = 120%                        /* LD_MAX%  Loaded */
            dtl_max% = 6%                          /* P_MAX%   Loaded */
            shifts%  = 6%
            screen%  = 120%                        /* SC_MAX% Display */
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

        build_display
            call "SHOSTAT" ("(4)-Building Analysis Display")
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 600%, rslt$(10%))
            plan_line$  = "Plan Items: XXXXXX"
            plan_line1$ = "Skipped: XXX"
            convert plan_line% to str(plan_line$,13%,6%), pic(######)
            convert plan_sk% to str(plan_line1$,10%,3%), pic(###)
            for i% = 1% to pln_max%
                init(" ") wrk$
                str(wrk$,1%,3%) = str(dpt$(i%),1%,3%)       /* Depart  */
                str(wrk$,4%,2%) = str(dpt$(i%),5%,2%)       /* Process */
                str(wrk$,6%,2%) = str(dpt$(i%),7%,2%)       /* Shift   */
                str(wrk$,8%,1%) = str(dpt$(i%),4%,1%)       /* Day     */
                w_hit% = 0%                     /* At Least one Record */
                for k% = 1% to 20%
                    convert pp$(i%,k%) to rhh%, data goto L30130

                       str(wrk$,9%,3%) = pp$(i%,k%)         /* Model   */
                       w_hit% = 1%                         /* (EWD013) */
                       write #10, using L30125, wrk$, cap(i%), unt%(i%),  ~
                                               pu%(i%,k%), ef_unt%(i%),   ~
                                               eod goto L30130
L30125:                    FMT CH(25), PD(14,4), BI(2), BI(2), BI(2)
L30130:         next k%
                if w_hit% <> 0% then goto L30155
                   str(wrk$,9%,3%) = "   "                     /* None */
                   write #10, using L30125, wrk$, cap(i%), unt%(i%), 0%,  ~
                                            ef_unt%(i%),  eod goto L30130
                                                        /*  (EWD013)   */
L30155:     next i%
            call "SHOSTAT" ("(5)-Formating Analysis Display")
              pln_max% = 0% : dpt_max% = 0%
              p_max%   = 0%
              i%       = 0% : k%       = 0%
           init(" ") dpt$(), readkey$, sv_dept$, d_dp$(), sv_root$, pp$()
           mat cap   = zer
           mat unt%  = zer
           mat ef_unt% = zer                       /*   (EWD013)   */
           mat pu%   = zer
           mat d_sb% = zer
        build_nxt                                 /* (EWD013)            */
           read #10,key > readkey$, using L30220, readkey$, rh1, rh2%,    ~
                                          rh3%, cg1%, eod goto build_done
L30220:       FMT CH(25), PD(14,4), BI(2), BI(2), BI(2)
           if sv_root$ = str(readkey$,1%,8%) then goto L30265
              sv_root$ = str(readkey$,1%,8%)
              j% = 0%
              i% = i% + 1% : if i% > plan% then i% = plan%
              dpt$(i%) = str(readkey$,1%,8%) /* Save the Sorted Record */
              cap(i%)  = rh1                 /* for Planning an Analysi*/
              unt%(i%) = rh2%                /* May contain the Values */
              ef_unt%(i%) = cg1%            /* Eff unit    (EWD013)   */

L30265:       j% = j% + 1%                   /* Rebuild Product Buckets*/
              pp$(i%,j%) = str(readkey$,9%,3%)  /* Based on New Sort   */
              pu%(i%,j%) = rh3%
                                             /* Rebuild Department Subs*/
           if sv_dept$ = str(readkey$,1%,3%) then goto L30315
              sv_dept$ = str(readkey$,1%,3%) /* Save Department Code   */
              k% = k% + 1% : if k% > depart% then k% = depart%
              str(d_dp$(k%),1%,3%) = sv_dept$ /* Store Department Code */
              d_sb%(k%,1%) = i%              /* Starting Sub-Script for*/
                                             /* Dept,Proc,Shift,Day    */
L30315:       d_sb%(k%, 3%) = i%             /* Ending Sub-Subscript   */
                 gosub debug1
              goto build_nxt                 /* for Dept in DPT$()     */
        build_done
           pln_max% = i%                    /* Max for Planning Week   */
           dpt_max% = k%                    /* Max No of Departments   */
           d_sb%(k%,3%) = i%                /* Ending Sub-Script for   */
           call "FILEBGON" (#10)            /* Dept in DPT$()          */
           gosub debug1
*       RHH
*       RHH  CALL "SHOSTAT" ("(14)-Finished Building Display")
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
        return

                                         /* DPT$() - Sort - Department */
                                         /*               - Process    */
                                         /*               - Shift      */
                                         /* Total for Each Day but only*/
        update_capacity                  /* do Once. (APCPLNUC) - File */
           call "SHOSTAT" ("Updating Capacity Buckets")
           init(" ") sv_root$, sv_root1$
           for i% = 1% to pln_max%
             if sv_root$ = str(dpt$(i%),1%,7%) then goto L30445
                gosub update_cap
L30445:      if sv_root1$ = str(dpt$(i%),1%,8%) then goto L30490
             k% = 1%                        /* Default Day (1)         */
             convert str(dpt$(i%),8%,1%) to k%, data goto L30460
L30460:
             sv_root1$ = str(dpt$(i%),1%,8%)/* Save so that not Counted*/
                                            /* Twice.                  */
                                            /* Build Weekly Buckets    */
             pl_unts(k%)  = pl_unts(k%)  + cap(i%)  /* Remain Capacity */
             pl_unta%(k%) = pl_unta%(k%) + unt%(i%) /* Total Production*/
             pl_unte%(k%) = pl_unte%(k%) + ef_unt%(i%) /*  (EWD013)    */
L30490:    next i%                                  /* Units for Day   */
           gosub update_cap                         /* Finish Last Dept*/
        return

        update_cap
           if i% = 1% then goto L30570
           str(pl_key$,1%,7%)  = sv_root$        /* Dept, Proc, Shift, */
           str(pl_key$,8%,2%)  = bg_yr$          /* Production Year    */
           str(pl_key$,10%,2%) = bg_wk$          /* Production Week    */
           read #1,hold,key 1% = pl_key$, eod goto L30560
              put #1, using L30545, pl_unts(), pl_unta%(), pl_unte%()
L30545:         FMT POS(31), 7*PD(14,4), 7*BI(2), 7*BI(2)
                                                 /*  (EWD013)          */
           rewrite #1
           goto L30570
L30560:      errormsg$="(5)Error-Updating Dept Capacity 'UC'-> "&pl_key$
             gosub error_prompt
L30570:    sv_root$ = str(dpt$(i%),1%,7%)        /* Init Department and*/
           mat pl_unts  = zer                    /* reset Capacity and */
           mat pl_unta% = zer                    /* Production Units   */
           mat pl_unte% = zer                    /* Effective (EWD013) */
        return                                   /* Buckets.           */

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput_sc
          if sc_pqty% = 0% then goto L31230
        REM  CONVERT SC_PQTY% TO RHH$, PIC(#####)
        REM  CALL "SHOSTAT" (Inventory Pull Qty = " & RHH$ )
        REM  STOP
             beg_dt%  = sc_mqty% + 1%        /* Do Pulls from Inventory*/
             end_dt%  = sc_mqty% + sc_pqty%  /* Finished Goods First   */
             gosub stock_sort                /* DT_SORT$ - Load        */
             dt_proc$ = "01"
             dt_shft$ = "01"
             pln_day$ = str(ll$(ld%),6%,2%)   /* Based on Load Start Dy*/
             gosub calc_date                  /* Includes Factor       */
REM             dt_dept$ = "102"                 /* Finished Goods Invent.*/
             dt_dept$ = sv_dept$
             dt_upmh  = 1.0                   /* Set Unit Per Manhour  */
             dt_st$   = "06"                  /* Sched Finished Stock  */
                                              /* Pull from Inventory   */
             if sv_dept$ = "101" then dt_st$ = "07"
             gosub dataput

                                              /* (EWD009)              */
L31230:   if sc_pqty1% = 0% then goto L31460     /* Do Pulls from Inv. */
             beg_dt% = sc_mqty% + sc_pqty% + 1%  /* for Special Shape  */
                                                 /* Products and Frames*/

        REM  CONVERT SC_PQTY1% TO RHH$, PIC(#####)
        REM  CALL "SHOSTAT" (Shape Pull Qty = " & RHH$ )
        REM  STOP

             end_dt% = sc_tqty%
             dt_sort$ = "99999"                /* (AWD025)*/
REM             SAV_PART$ = DT_PART$           /* (AWD025)*/
REM             INIT(" ") DT_PART$             /* (AWD025)*/
REM          GOSUB CHECK_SPECIAL_SHAPES_STOCK  /* (AWD025)*/
                                 /* DT_SORT$ - LOAD       */
REM             GOSUB STOCK_SORT               /* (AWD025)*/
REM             DT_PART$ = SAV_PART$           /* (AWD025)*/
             dt_proc$ = "01"
             dt_shft$ = "01"
             pln_day$ = str(ll$(ld%),6%,2%)   /* Based on Load Start Dy*/
             gosub calc_date                  /* Includes Factor       */
             dt_dept$ = "104"                 /* Finished Goods Shapes */
             dt_upmh  = 1.0                   /* Set Unit Per Manhour  */
             dt_st$   = "07"                  /* Sched Shape Pull's    */
             gosub dataput                    /* Update Planned S.O.   */
                                              /* Line Item             */
L31460:
             xx_st$ = "03"                    /* (EWD003) - Begin      */
             gosub glass_warranty
             if glass_warranty% = 1% then xx_st$ = "16"
                                              /* (EWD003) - End        */
             read #5,hold,key = dt_so_line$, eod goto L31510
                put #5, using L31480, xx_st$, date  /*Planned Line Item*/
L31480:           FMT POS(110), CH(2), CH(6)
             rewrite #5
             goto L31540
L31510:      errormsg$="(4)Error-Updating S.O. Ln Items 'SC'-> "&sc_key$
             gosub error_prompt

L31540:      init(" ") sd_key1$               /* (APCPLNSD) - Purge    */
             str(sd_key1$,1%,10%) = dt_so_line$ /* Del S.O. Line Items */
L31560:      read #6,hold,key > sd_key1$, using L31580, sd_key1$,         ~
                                                       eod goto L31620
L31580:         FMT CH(23)
             if sc_sav$ <> str(sd_key1$,1%,10%) then return
                delete #6
                goto L31560                    /* Delete Next Detail    */
L31620: return

        dataput_dt                         /* Skip Dept (100) Overflow */
          if sv_dept$ = "100" then return     /* SKIP OVERFLOW         */
REM IF SV_DEPT$ = "102" THEN RETURN     /* SKIP FINISHED/SHAPES  */
/* (CR982) to allow hoppers to plan to department 102 */
          if sv_dept$ = "102" and sc_mqty% =  0% then return
          if sv_dept$ = "101" and sc_pqty% <> 0% then return
          if sv_dept$ = "103" and sc_pqty% <> 0% then return
REM          if sv_dept$ = "104" then return     /* Default 'QTY%' Sub -  */
          beg_dt%  = qty%                     /* from 'SCHED_ORDER'    */
          end_dt%  = qty%                     /* Routine. (1) at a Time*/
          if tqty% = 1% then goto L31730       /* Room in Bucket to do  */
             beg_dt% = 1%                     /* all 'Makes' in One (1)*/
             end_dt% = tqty%                  /* Pass.                 */
L31730:   convert sds%(l%) to dt_sort$, pic(00000)
          dt_proc$    = str(dpt$(k%),5%,2%)
          dt_shft$    = str(dpt$(k%),7%,2%)
          convert ld_day% to pln_day$, pic(00)
          gosub calc_date
          dt_dept$ = sv_dept$
          dt_st$   = "04"                     /* Status Sched Make     */
          if sv_dept$ = "101" then dt_st$ = "07"
                                              /*  (AWD020)             */
REM          if str(dt_so_line$,1%,1%) = "S" then dt_st$ = "08"

        dataput                               /* (APCPLNDT) - File     */
          init(" ") dt_time$, dt_rec$
          call "TIME" (dt_time$)
                                                        /* (EWD005)    */
          pl_sorts$   = str(d_dp$(dpt%),4%,15%)         /* Load Sort   */
                                                        /* Same for Ln */

REM          if str(dt_so_line$,1%,8%) <> "09001829" then goto notOrder
REM            call "SHOSTAT" ("SO Number " dt_so_line$)
REM            stop
REM notOrder
          for upd% = beg_dt% to end_dt%
              glass_warranty% = 0%                  /* (EWD003)        */
              init(" ") dt_rec$
              convert upd% to dt_item_no$, pic(0000)
              str(dt_rec$,1%,17%)  = load_group$
              str(dt_rec$,18%,2%)  = dt_ln_item$
              str(dt_rec$,20%,4%)  = dt_item_no$
              str(dt_rec$,24%,10%) = dt_so_line$
              str(dt_rec$,34%,4%)  = dt_item_no$
              str(dt_rec$,38%,4%)  = dt_qty$        /* Total Quantity  */
              str(dt_rec$,42%,3%)  = dt_dept$       /*                 */
              str(dt_rec$,45%,2%)  = dt_proc$       /* Need            */
              str(dt_rec$,47%,6%)  = pln_dte$       /* Calculated      */
              str(dt_rec$,53%,6%)  = pln_dte$       /* Calculated      */
              str(dt_rec$,59%,3%)  = dt_dept$       /*                 */
              str(dt_rec$,62%,2%)  = dt_proc$       /* Need            */
              str(dt_rec$,64%,2%)  = dt_st$         /* Set to Status   */
        REM - Build Sort Index Last                 /* DT_INDEX$       */
        REM - Assign Warranty Reference No          /* DT_REF$         */
              str(dt_rec$,104%,2%) = dt_shft$       /* Shift Code      */
              str(dt_rec$,106%,5%) = dt_sort$       /* Prod/Stock Sort */
              str(dt_rec$,111%,5%) = "00000"        /* Seq. Assigned   */
              str(dt_rec$,116%,8%) = dt_time$       /* Scan Time       */
              str(dt_rec$,124%,9%) = dt_cust$       /* Customer Code   */
        REM - BUILD Values                          /* Sale Price Per  */
                                                    /* Window Unit Net */
              put str(dt_rec$,133%,56%), using L32140, dt_sale, 0, 0, 0,  ~
                                                      0, 0, dt_upmh
L32140:           FMT 7*PD(14,4)
              str(dt_rec$,189%,25%)= dt_part$              /* Part No. */
              sc$ = str(dt_part$,11%,1%)                   /* Scr Code */
              str(dt_rec$,214%,1%) = "0"                   /* Not Sash */
              if sc$ = "4" then str(dt_rec$,214%,1%) = "1" /* Top Sash */
              if sc$ = "5" then str(dt_rec$,214%,1%) = "2" /* Bot Sash */
              if sc$ = "6" then str(dt_rec$,214%,1%) = "3" /* Fix Glass*/
/*(AWD039)*/  if sc$ = "7" then str(dt_rec$,214%,1%) = "4" /* OGO Glass*/
              str(dt_rec$,215%,1%) = str(dt_special$,8%,1%) /*Part Y/N */
              str(dt_rec$,216%,1%) = "0"                    /* N/A     */
              if str(dt_special$,5%,1%) = "Y" then          /* Sample  */~
                                      str(dt_rec$,216%,1%) = "1"
              if str(dt_special$,6%,1%) = "Y" then          /* Display */~
                                      str(dt_rec$,216%,1%) = "2"
              str(dt_rec$,217%,3%) = "000"                  /* N/A     */
              if str(dt_special$,4%,1%) = "N" then goto L32320
                 gosub set_wood_surround              /* (EWD007)      */

L32320:       str(dt_rec$,220%,10%) = dt_special$     /* 1=Tempered Gls*/
                                                      /* 2=Diamond Grid*/
                                                      /* 3=Spec Liting */
                                                      /* 4=Wood/Surr   */
                                                      /* 5=Sample Prod */
                                                      /* 6=Display Prod*/
                                                      /* 7=Ups Prod    */
                                                      /* 8=Parts       */
                                                      /* 9=Cottage/Orie*/
                                                      /* 10=Sash's     */
              str(dt_rec$,230%,2%) = bg_yr$    /* Planned Production YR*/
              str(dt_rec$,232%,2%) = bg_wk$    /* Planned Production WK*/
              str(dt_rec$,234%,2%) = pln_day$  /* Planned Prod. Day YY */
              str(dt_rec$,236%,4%) = dt_txt$   /* Line Item Text       */
              str(dt_rec$,240%,1%) = dt_group$ /* (EWD002) Group Code  */
              str(dt_rec$,241%,2%) = dt_config$/* (EWD004) Config/Prv  */
              str(dt_rec$,243%,2%) = dt_prv$   /* Private Label Code   */
/* (AWD031) */
              str(dt_rec$,245%,10%) = sku$
/* (AWD035) */
/* CR1934              str(dt_rec$,254%,1%) = "0"                      */
/*CR00671     if ultra% = 1% then str(dt_rec$,254%,1%) = "1"           */
/*CR00671 + */
              model$ = str(dt_part$,1%,3%)
              ty$ =    str(dt_part$,5%,2%)
              gosub lookup_intercept
/* CR1934 Remove     if intercept% = 3% then str(dt_rec$,254%,1%)="1"  */
/*CR00671 - */

        REM - Build Free Space
              str(dt_rec$,255%,2%) = " "
                                               /* (EWD004) - End       */
        REM - Build Index (DT_INDEX$)
                                             /* (AWD015)For Spec Shapes*/
REM              IF DT_DEPT$ <> "043" THEN GOTO L32400
              if schema% = 1% and dt_dept$ <> "043" then goto L32400
              if schema% = 2% and (dt_dept$ <> "002" and dt_dept$ <> "003") ~
                 then goto L32400

                 specialmull$ = " "          /* (AWD030) */
                 call "AWDPLN9B" (dt_rec$,   /* (APCPLNDT) Record      */~
                                  sort_key$, /* Output Index Built     */~
                                  specialmull$, /* Not used here (AWD030) */~
                                  #3 )       /* (GENCODES)             */
                 goto L32420

L32400:
              call "APCPLN9B" ( "0",         /* Set Flag for Data File */~
                                pl_sorts$,   /* Sort Code From APCPLNUC*/~
                                dt_rec$,     /* (APCPLNDT) Record      */~
                                bcksubpt_rec$, /* BCKSUBPT Record Foam */~
                                sort_key$,   /* Output Index Built     */~
                                #3 )         /* (GENCODES)             */

L32420:                                      /* (AWD015) - End         */
              str(dt_rec$,66%,30%) = str(sort_key$,1%,30%)
/* (AWD026) - beg */
REM              init(" ") so_inv$, item_no$
REM              str(so_inv$,1%,8%)  = str(dt_rec$,24%,8%)
REM              str(item_no$,1,2%)  = str(dt_rec$,32%,2%)
REM              gosub lookup_sub_part          /* (AWD026)  */
/* (AWD026) - end */
              gosub assign_warranty          /* Warranty No per Barcode*/
              gosub glass_warranty           /* (EWD003) - 10/20/98    */
              if glass_warranty% = 1% then str(dt_rec$,64%,2%) = "16"

        REM - (End) APCPLNDT Create
              dt_key$ = str(dt_rec$,24%,23%) /* See if already Created */
              read #7,hold,key = dt_key$, eod goto L32600
                 delete #7                   /* Purge Old 'DT' Record  */
L32600:       put #7, using L32610, dt_rec$
L32610:          FMT CH(256)
              write #7, eod goto L32660

              message$ = dt_key$
              gosub update_dtholdfl

              gosub update_audit
REM              GOSUB UPDATE_ORACLE             /*   (EWD014)           */
              gosub update_warranty_file
              if (schema% = 2% or schema% = 5%)  ~
                  then gosub load_pg_trigger         /* CR1726  */
              if (schema% = 2% or schema% = 5%)  ~
                  then gosub load_pg_order_trigger   /* CR1726  */
              if (schema% = 2% or schema% = 5%)  ~
                  then gosub load_pg_attr_trigger    /* CR1726  */
L32640:   next upd%
        return
L32660:     errormsg$="(1)Error-Updating Prod Detail 'DT'---> "&dt_key$
            gosub error_prompt
            goto L32640                      /* Return to Loop         */

        glass_warranty                       /* (EWD003) - Begin       */
            glass_warranty% = 0%
            if len(dt_part$) > 18 then return
               if str(dt_part$,5%,4%) <> "WARR" then return
                  glass_warranty% = 1%       /* Treat as Loaded        */
        return                               /* (EWD003) - End         */

        dataput_ld_or                        /* (APCPLNLD) - File      */
            gosub convert_dates              /* 1st Update Load Date'S */
REM            if plan_line% = 0% then return   /* No S.O.'s Planned      */
            if plan_load% = 0% then return   /* No S.O.'s for Load Pln */


            read #4,hold,key = ld_load$, using L32770, lx_dtp1$,          ~
                                         lx_dtp2$, lx_dtp3$, lx_status$, ~
                                         eod goto L32890
L32770:        FMT POS(70), 3*CH(8), CH(2)
            if load_flg% = 0% then goto L32830
               ld_dtp1$ = lx_dtp1$
               if ld_dtp2$ < lx_dtp2$ then ld_dtp2$ = lx_dtp2$
               if ld_dtp3$ < lx_dtp3$ then ld_dtp3$ = lx_dtp3$

L32830:     if lx_status$ < "04" then lx_status$ = "03"
               put #4, using L32860, ld_dtp1$, ld_dtp2$, ld_dtp3$,        ~
                                    lx_status$, date
L32860:               FMT POS(70), 3*CH(8), CH(2), CH(8)
            rewrite #4

            return                                      /*  (AWD019)   */
REM            goto L32920                              /*  (AWD019)   */
L32890:      errormsg$="(2)Error-Updating Load Dates 'LD'----> "&ld_load$
             gosub error_prompt

L32920:     init(" ") sc_key$               /* 2nd Update all S.O. on  */
            str(sc_key$,1%,5%) = ld_load$   /* the Completed Load      */
        dataput_or                          /* (APCPLNSC) - File       */
            read #5,key 1% > sc_key$, using L32960, sc_key$,eod goto L33080
L32960:        FMT POS(7), CH(27)
            if str(sc_key$,1%,5%) <> ld_load$ then return
                                            /* (APCPLNOR) - File       */
               read #9,hold,key 4% = str(sc_key$,18%,8%), using L33010,   ~
                                            or_status$, eod goto L33090
L33010:           FMT POS(60), CH(2)
               if or_status$ > "03" then goto L33060
                  put #9,using L33040, "03", date
L33040:             FMT POS(60), CH(2), CH(6)
               rewrite #9
L33060:        str(sc_key$,26%,2%) = "99"   /* Last S.O. Line Item     */
               goto dataput_or
L33080: return
L33090:      errormsg$="(3)Error-Updating S.O. Header's 'OR'-> "&sc_key$
             gosub error_prompt
             goto L33060

        convert_dates
            init(" ") ld_dtp1$, ld_dtp2$, ld_dtp3$
            pln_day$ = str(ll$(ld%),6%,2%)   /* Set Production Date    */
            gosub calc_date                  /* Date Adj for Factor    */
            ld_dtp1$ = pln_dte$
            pln_day$ = str(ll$(ld%),8%,2%)  /* Set Completion Date     */
            gosub calc_date                 /* Date Adj for Factor     */
            ld_dtp2$ = pln_dte$
            pln_day$ = str(ll$(ld%),10%,2%) /* Set Truck Load Date     */
            gosub calc_date                 /* Date Adj for Factor     */
            ld_dtp3$ = pln_dte$
        return
REM modify here to lookup barcode & warranty from awdschdt
        assign_warranty                     /* Only (1) One Warranty   */
            wt% = 0%                        /* (AWD026)  */
            init(" ") dt_key$, dt_ref$      /* Number Assigned to a    */
            str(dt_key$,1%,18%) = str(dt_rec$,24%,18%)   /* Barcode.   */
REM APCPLNDT #7
            read #7,key > dt_key$, using L33310, dt_key$, dt_ref$,        ~
                                                eod goto assign_next
L33310:        FMT POS(24), CH(23), POS(96), CH(8)
            if str(dt_key$,1%,18%) = str(dt_rec$,24%,18%) then goto L33370
        assign_next
               if dt_dept$ = "103" then goto skip_Stock
               if dt_dept$ = "101" then goto stock_ne_pull   /*(AWD025) */
               if sc_pqty1% <> 0% then goto stock_ne_pull    /*(AWD025) */
               if sc_pqty%  <> 0% then goto stock_ne_pull    /*(AWD025) */
/*(AWD027) turn off 102 for now */
REM        if dt_dept$ = "102" then goto stock_ne_pull   /*(AWD025) */
REM        if sc_pqty%  <> 0% then goto stock_ne_pull    /*(AWD025) */
/* (AWD038) */
skip_Stock:
               gosub check_awdschdt
               if awdschdt% = 1% then goto write_wt
/* (\AWD038) */
/* (AWD040) */
               gosub check_warrther
               if warrther% = 1% then goto write_wt
/* (\AWD040) */
               warranty% = warranty% + 1%
               if dallas% = 1% then goto assign_dallas   /*  (EWD010) - Beg */
               convert warranty% to dt_ref$, pic(00000000)
               goto write_wt
             assign_dallas
               str(dt_ref$,1%,1%) = "D"
               convert warranty% to str(dt_ref$,2%,7%), pic(0000000)
             write_wt                                   /*  (EWD010)  - End */
               wt% = 15%                                /* (AWD026) */
               gosub check_warranty
               gosub update_apcplnwt        /* (EWD001) - Warranty     */


L33370:     str(dt_rec$,96%,8%) = dt_ref$
        return

        stock_ne_pull                    /*(AWD025)*/
            swarranty% = swarranty% + 1%
            str(dt_ref$,1%,1%) = "S"
            convert swarranty% to str(dt_ref$,2%,7%), pic(0000000)
            str(dt_rec$,96%,8%) = dt_ref$
            wt% = 22%                          /* (AWD026) */
            gosub update_awdplncd              /* (AWD026) */
        return
/* (AWD038) */
        check_awdschdt
          awdschdt% = 0%
          init(" ") awdschdt_key2$, awdschdt_ref$
          awdschdt_key2$ = str(dt_rec$,24%,18%)   /* Barcode.   */
          read #24, key 2% = awdschdt_key2$, using AWDSCHDT_FMT, awdschdt_ref$,~
                           eod goto check_awdschdt_done
AWDSCHDT_FMT:    FMT POS(21), CH(08)

              if awdschdt_ref$ <= " " then goto check_awdschdt_done

              dt_ref$ = awdschdt_ref$
              awdschdt% = 1%
        check_awdschdt_done
        return
/* (\AWD038) */

/* (AWD040) */
        check_warrther
          warrther% = 0%
          init(" ") warrther$, warrtherId$
          warrther$ = str(dt_rec$,24%,18%)   /* Barcode.   */
          read #25, key 1% = warrther$, using WARRTHER_FMT, warrtherId$, ~
                           eod goto check_awdschdt_done
WARRTHER_FMT:    FMT POS(22), CH(08)

              if warrtherId$ <= " " then goto check_warrther_done

              dt_ref$ = warrtherId$
              warrther% = 1%
        check_warrther_done
        return
/* (\AWD040) */


        update_apcplnwt                     /* (EWD001) Warranty Begin */
            gosub check_warranty_barcode    /* (AWD026)  */
            init(" ") wt_rec$               /* Cross-Ref File Tracking */
            str(wt_rec$,1%,8%)   = dt_ref$
            str(wt_rec$,9%,18%)  = str(dt_rec$,24%,18%)
            str(wt_rec$,27%,25%) = str(dt_rec$,189%,25%)
            str(wt_rec$,52%,20%) = str(dt_sub_part$,1%,20%)
            str(wt_rec$,72%,20%) = str(dt_sub_info$,1%,20%)
/* (AWD034) add itemid and unitid to APCPLNWT */
            str(wt_rec$,92%,10%) = itemid$
            str(wt_rec$,102%,10%) = unitid$
            str(wt_rec$,112%,17%) = "      "

            if wt% <> 22% then goto wt_finished

               str(wt_rec$,92%,8) = "99999999"
               str(wt_rec$,100%,29%) = " "

wt_finished:


            put #wt%, using L33380, wt_rec$
L33380:       FMT CH(128)                          /* (AWD026) */
            write #wt%, eod goto update_apcplnwt_done

        update_apcplnwt_done
        return                               /* (EWD001) - End         */

        update_awdplncd                     /* (AWD026)  */
            gosub update_apcplnwt
         return

        check_warranty                      /* (EWD001) (AWD016)       */
            init(" ") wt_key$, wt_rec$      /* Cross-Ref File Tracking */
            str(wt_key$,1%,8%)  = dt_ref$

            read #wt%, key = wt_key$, using L33380, wt_rec$,              ~
                                  eod goto no_warranty
                  if str(wt_rec$,9%,10%) = str(dt_rec$,24%,10%)            ~
                       then goto no_warranty
                  gosub previous_plan_error
                  goto exit_program
        no_warranty
        return                               /* (EWD001) - End  (AWD016) */

/* (AWD026)  - begin  */
        check_warranty_barcode
             init(" ") wt_key2$
             str(wt_key2$,1%,18%) = str(dt_rec$,24,18%)
             read #wt%, hold, key 2% = wt_key2$, eod goto bcde_warr_done
                       delete #wt%
         bcde_warr_done
         return

/* (AWD026)  - end  */

        update_audit
            init(" ") ad_rec$, ad_time$, ad_key$
            call "TIME" (ad_time$)
            str(ad_rec$,1%,18%) = str(dt_rec$,24%,18%)    /* Barcode   */
            str(ad_rec$,19%,6%) = str(dt_rec$,47%,6% )    /* Prod Date */
            str(ad_rec$,25%,3%) = str(dt_rec$,42%,3% )    /* Department*/
            str(ad_rec$,28%,2%) = str(dt_rec$,45%,2% )    /* Process   */
            str(ad_rec$,30%,2%) = str(dt_rec$,104%,2%)    /* Shift Code*/
            str(ad_rec$,32%,2%) = str(dt_rec$,64%,2% )    /* Status    */
            str(ad_rec$,34%,18%)= str(dt_rec$,24%,18%)    /* Barcode   */
            str(ad_rec$,52%,8%) = ad_time$                /* Time Stamp*/
            str(ad_rec$,60%,3%) = userid$                 /* User Id   */
            str(ad_rec$,63%,2%) = "  "                    /* Filler    */
            ad_key$ = str(ad_rec$,19%,33%)
            read #8,hold,key = ad_key$, using L33550, ad_rec1$,           ~
                                                        eod goto L33540
               delete #8
L33540:     write #8, using L33550, ad_rec$, eod goto L33570
L33550:         FMT CH(64)
        return
L33570:   errormsg$="(3)Error-Upd Planning Audit? "&str(ad_rec$,1%,18%)
          gosub error_prompt
        return

        set_wood_surround                           /* (EWD007)       */
            convert str(dt_part$,20%,3%) to rhh%, data goto L33600

            convert str(dt_part$,23%,3%) to rhh%, data goto L33610
        return
L33600:     str(dt_rec$,217%,3%) = str(dt_part$,20%,3%)
        return
L33610:     str(dt_rec$,217%,3%) = str(dt_part$,23%,3%)
        return
                                                    /* (EWD007)       */


                                                    /*  (EWD014)      */
        update_oracle
REM            call "SHOSTAT" ("Updating Database, Barcode -> " & str(bar_key$,1%,18%) )
            gosub oracle_flush
            init(" ") stmt1$, stmt2$
            str(stmt1$,1%,40%)  = "UPDATE MSSQL.WARRANTY SET WARRANTYID = '"
            str(stmt1$,41%,27%) = dt_ref$ & "' WHERE BARCODE = '"
            str(stmt1$,68%,20%) = str(bar_key$,1%,18%) & "'"
            gosub oracle_exec
/* (AWD040) */
            if oci_err% >= 0% then return  /* Record Updated */
            if unitid$ = "0000000000" then return
            gosub oracle_flush
            str(stmt1$,1%,40%)  = "INSERT INTO MSSQL.WARRANTY (UNITID, BARC"
            cmg$ = str(stmt1$,1%,40%)
            str(stmt1$,41%,36%) = "ODE, WARRANTYID) VALUES (" & unitid$ & ","
            cmg$ = str(stmt1$,41%,36%)
            str(stmt1$,77%,32%) = "'" & bar_key$ & "','" & dt_ref$ & "')"
            cmg$ = str(stmt1$,77%,32%)
            gosub oracle_exec
        return

        oracle_connect
            init(" ") user$, pass$, server$
REM            USER$   = "MSSQL"
REM            PASS$   = "MSSQL"
            gosub get_user_pswd       /* (AWD037) */

            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)
REM           IF OCI_ERR% => 0% THEN RETURN
REM           ERRORMSG$ = "YOU ARE NOT CONNECTED TO ORACLE, CONTACT SYSTEMS!!!!"
REM           GOSUB ERROR_PROMPT
        return


        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

                                                    /*  (EWD014)      */
/* <AWD032> */
        update_dtholdfl
          dt_bar$ = str(dt_rec$,24,18)
          dt_key$ = str(dt_rec$,24,23)
          read #13, hold, key = dt_key$, eod goto skip_delete

            delete #13
L33700:        FMT  CH(256), CH(10), CH(10)
skip_delete:
            write #13, using L33700, dt_rec$, itemid$, unitid$
          dt_bar$ = str(dt_rec$,24,18)
        return

        update_apcplndt
          des_cnt = 0
          dt_key$ = "000000000000000000000"

        read_loop
            read #13, hold, key > dt_key$, hold, using L33700, dt_rec$, ~
                       itemid$, unitid$, eod goto end_update


            dt_key$ = str(dt_rec$,24,23)
            dt_part$ = str(dt_rec$,189%,25%)
            dt_load$ = str(dt_rec$,1,5)
            dt_drop_seq$ = str(dt_rec$,6,5)
            dt_drop$ = str(dt_rec$,11,2)
            dt_cus_sort$ = str(dt_rec$,13,5)
            dt_ln_item$ = str(dt_rec$,18,2)
            dt_item_no$ = str(dt_rec$,20,4)
            dt_so_line$ = str(dt_rec$,24,10)
            dt_bar$ = str(dt_rec$,24,18)
            dt_dept$ = str(dt_rec$,42,3)
            dt_proc$ = str(dt_rec$,45,2)
            dt_date$ = str(dt_rec$,47,6)
            dt_dte$ = str(dt_rec$,53,6)
REM     DT_DATE$ = PLN_DTE$
REM     DT_DTE$  = PLN_DTE$
            dt_special$ = str(dt_rec$,220,10)
            dt_st$ = str(dt_rec$,64,2)
            dt_index$ = str(dt_rec$,66,30)
            dt_shft$ = str(dt_rec$,104,2)
            dt_sort$ = str(dt_rec$,106,5)
            dt_time$ = str(dt_rec$,116,8)
            dt_cust$ = str(dt_rec$,124,9) 
REM     DT_TXT$  = STR(DT_REC$,236,4)    /* LINE ITEM TXT*/
            dt_sku$  = str(dt_rec$,245,10)   /* Line Item Txt  CR1934 */
REM     DT_TXT$  = STR(DT_REC$,236,4)
        /*--------------------------------*/
            get str(dt_rec$,236,4), using bin4, dt_txt%
bin4:       FMT BI(4)

            convert dt_txt% to dt_txt$, pic (0000000000)
            dt_prv$    = str(dt_rec$,243,2)
            dt_group$  = str(dt_rec$,240,1)
            dt_config$ = str(dt_rec$,241,2)
            sc$        = str(dt_rec$,214,1)
            dt_ref$    = str(dt_rec$,096,8)
        /*--------------------------------*/
            get str(dt_rec$,133%,56%), using L32140,         ~
                        dt_sale, a, b, c, d, e, dt_upmh

            convert dt_sale to dt_sale$, pic (########.####-)
            convert dt_upmh to dt_upmh$, pic (########.####-)
            get dt_date$, using L68020, dt_date%
L68020:     FMT PD(11,1)

            convert dt_date% to date1$, pic (00000000)
            get dt_dte$, using L68020, dt_dte%
            convert dt_dte% to date2$, pic (00000000)
            bg_dte$ = ent_dte$
            pln_day$ = ent_dy$
            gosub calc_date
            bg_yr% = dt_date% / 10000%
            convert bg_yr% to tmp_yr$, pic (####)

            get dt_rec$, using dtyr, bg_yr%
dtyr:       FMT POS(230), BI(2)

            bg_wk$ = str(dt_rec$,232,2)
            pln_day$ = str(dt_rec$,234,2)
REM     CONVERT BG_YR$  TO BG_YR%, DATA GOTO SKIP_YEAR

skip_year:  convert bg_yr%  to bg_yr$, pic (00)

            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, stmt6$, ~
                      stmt7$, stmt8$
            stmt1$  = "DELETE FROM DTS_PLAN_DETAIL WHERE " & ~
              "BARCODE = '" & dt_bar$ & "' AND PLAN_DEPARTMENT = '" & ~
              dt_dept$ & "' AND PROCESS_CODE = '" & dt_proc$ & "'"

            gosub oracle_flush
            gosub oracle_exec

REM            call "SHOSTAT" ("Updating Database, DT Barcode -> " & dt_bar$ )

/* (AWD034) mod to add itemid and unitid to sql stmt */
            if oci_err% <> 0% then gosub oracle_error1

REM                    ....+....1....+....2....+....3....+....4....+....5

          stmt1$    = "INSERT INTO DTS_PLAN_DETAIL (BARCODE, " & ~
                 "PLAN_DEPARTMENT, PROCESS_CODE, LOAD_NUMBER, " & ~
                 "DROP_SEQUENCE, PROD_DROP_NO, PRODUCTION_DATE, " & ~
                 "CUST_SORT_CODE, PROD_ITEM_NUMBER, SO_LINE_ITEM, " & ~
                 "SCANNED_DATE, PRODUCTION_DEPARTMENT, " & ~
                 "PRODUCTION_PROCESS, PRODUCTION_STATUS"

          stmt2$    = ", SORT_INDEX, WARRANTY_REMAKE_NUMBER, " & ~
                 "SCHED_SHIFT_CODE, PRODUCTION_SORT, " & ~
                 "PRODUCTION_SEQUENCE, SCANNED_TIME, " & ~
                 "CUSTOMER_CODE, SALES_ORDER_PRICE, " & ~
                 "MATERIAL_COST, LABOR_COST, LABOR_OVERHEAD, " & ~
                 "FREIGHT_COST, VINYL_DISCOUNT, " & ~
                 "UNIT_PER_MAN_HOUR, MFG_PART_NUMBER,"

          stmt3$    = " SASH, MFG_PART, SAMPLE, WOOD_SURROUND_CODE, " & ~
                 "SPECIAL_PRODUCTS, PRODUCTION_YEAR, " & ~
                 "PRODUCTION_WEEK, PRODUCTION_DAY, " & ~
                 "PRODUCTION_TEXT, GROUP_CODE, CONFIG_CODE, " & ~
                 "PRIVATE_LABEL_CODE, SKU, ITEMID, UNITID) " &  ~
                 "VALUES ('" & dt_bar$ & "','" & dt_dept$ & "','" & ~
                 dt_proc$ & "','" & dt_load$ & "','" & dt_drop_seq$


          stmt4$    = "','" & dt_drop$ & "',TO_DATE("  & "'" & date1$ & ~
                 "','yyyymmdd'),'" & dt_cus_sort$ & "','" &        ~
                 dt_item_no$  & "','" & dt_ln_item$ & "',TO_DATE('" & ~
                 date2$ & "','yyyymmdd'),'" & dt_dept$ & "','" &     ~
                 dt_proc$ & "','" & dt_st$ & "',"

          stmt5$ =    "'" & dt_index$ & "','" & dt_ref$ & "','" & ~
                 dt_shft$ & "','" & dt_sort$ & "','00000','" & ~
                 dt_time$ & "','" & dt_cust$ & "', " & dt_sale$ & ~
                 ",0.00,0.00,0.00,0.00,0.00," & dt_upmh$ &  ~
                 ",'" & dt_part$ & "'"

          stmt6$ =    ",'" & sc$ & "','" & ~
                 str(dt_rec$,215,1) & "','"  & ~
                 str(dt_rec$,216,1) & "','" &  ~
                 str(dt_rec$,217,3) & "','" & dt_special$ & ~
                 "'," & tmp_yr$ & ",'" & bg_wk$ & "','" & ~
                 pln_day$ & "'," &  ~
                 dt_txt$ & ",'" &  ~
                 dt_group$ & "','" & dt_config$ & "','" & ~
                 dt_prv$ & "','" & dt_sku$ & "'," & itemid$ & ~
                 "," & unitid$ & ")"


            init(" ") stmt7$, stmt8$
            gosub oracle_flush
            gosub oracle_exec1

            if oci_err% <> 0% then gosub oracle_error1

            delete #13

            des_cnt = des_cnt + 1
            if des_cnt < 500 then goto read_loop
              des_cnt = 0
              stmt1$ = "COMMIT"
              gosub oracle_flush
              gosub oracle_exec

              goto read_loop
end_update:

            stmt1$ = "COMMIT"
            gosub oracle_flush
            gosub oracle_exec
        return

        oracle_exec1
            oci_err% = 0%
            call "EXEC1" (stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                          stmt6$, stmt7$, stmt8$, oci_err%)
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                      stmt6$, stmt7$, stmt8$

        return

        oracle_error1
REM            OCI_ERR% = 0%
            init(" " ) error$
            call "ERROR" (error$)

            call "SHOSTAT" ("ERROR --> " & error$)

        return


/* <AWD032> */
        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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
              on fieldnr% gosub L40260,         /* Production Year      */~
                                L40260,         /* Production Week      */~
                                L40250,         /* Beg Department       */~
                                L40250,         /* End Department       */~
                                L40250,         /* Beg Process          */~
                                L40250,         /* End Process          */~
                                L40250,         /* Beg Shift            */~
                                L40250,         /* End Shift            */~
                                L40250,         /* Beg Day              */~
                                L40250,         /* End Day              */~
/*  (AWD017)  */                L40250          /* Sort Wood ?          */

              goto L40280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40260:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Planning Production Year  :",                ~
               at (04,30), fac(lfac$(1%)), bg_year$             , ch(04),~
                                                                         ~
               at (05,02), "Planning Production Week  :",                ~
               at (05,30), fac(lfac$(2%)), bg_wk$               , ch(02),~
               at (05,40), fac(hex(84)), bg_date$               , ch(10),~
               at (05,55), fac(hex(84)), current$               , ch(13),~
                                                                         ~
               at (06,02), "Beginning Department Code :",                ~
               at (06,30), fac(lfac$(3%)), bg_dept$             , ch(03),~
               at (06,40), fac(hex(84)), bg_dept_d$             , ch(30),~
                                                                         ~
               at (07,02), "Ending Department Code    :",                ~
               at (07,30), fac(lfac$(4%)), ed_dept$             , ch(03),~
               at (07,40), fac(hex(84)), ed_dept_d$             , ch(30),~
                                                                         ~
               at (08,02), "Beginning Process Code    :",                ~
               at (08,30), fac(lfac$(5%)), bg_proc$             , ch(02),~
               at (08,40), fac(hex(84)), bg_proc_d$             , ch(30),~
                                                                         ~
               at (09,02), "Ending Process Code       :",                ~
               at (09,30), fac(lfac$(6%)), ed_proc$             , ch(02),~
               at (09,40), fac(hex(84)), ed_proc_d$             , ch(30),~
                                                                         ~
               at (10,02), "Beginning Shift Code      :",                ~
               at (10,30), fac(lfac$(7%)), bg_shft$             , ch(02),~
               at (10,40), fac(hex(84)), ed_shft_d$             , ch(30),~
                                                                         ~
               at (11,02), "Ending Shift Code         :",                ~
               at (11,30), fac(lfac$(8%)), ed_shft$             , ch(02),~
               at (11,40), fac(hex(84)), ed_shft_d$             , ch(30),~
                                                                         ~
               at (12,02), "Starting Production Day   :",                ~
               at (12,30), fac(lfac$(9%)), bg_day$              , ch(01),~
               at (12,40), fac(hex(84)), bg_day_dte$            , ch(10),~
               at (12,52), fac(hex(84)), bg_day_d$              , ch(09),~
                                                                         ~
               at (13,02), "Ending Production Day     :",                ~
               at (13,30), fac(lfac$(10%)), ed_day$             , ch(02),~
               at (13,40), fac(hex(84)), ed_day_dte$            , ch(10),~
               at (13,52), fac(hex(84)), ed_day_d$              , ch(09),~
                                                                         ~
               at (14,02), "Wood Sort?                :",                ~
               at (14,30), fac(lfac$(11%)), wood_sort$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% < 6% or keyhit% > 9% then goto L40880
                  tab% = keyhit% - 5%
                  gosub display_codes
                  goto L40070

L40880:        if keyhit% <> 15 then goto L40920
                  call "PRNTSCRN"
                  goto L40070

L40920:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            apc$   = "Planning Master Utility (** Analysis **)"

        if edit% = 2% then L41130     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "                                       "
            pf$(2) = "(4)Previous Field  (7)Process Codes     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                   (9)Shift Codes       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ff09ffffffffffff1000)
            if fieldnr% = 1% then L41090
                str(pf$(3%),40%) = " " : str(pfkeys$,13%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41090:     if fieldnr% > 1% then L41110
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41110: return

L41130: if fieldnr% > 0% then L41220  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "  (10)Analysis                         "
            pf$(2) = "                   (7)Process Codes     " &        ~
                     "  (30)Analysis Update  (15)Print Screen"
            pf$(3) = "                   (9)Shift Codes       " &        ~
                     "  (13)Assign Seq No's  (16)Exit Program"
                                                        /* (AWD021) */
            pfkeys$ = hex(01ffffffff0607ff090affff0dff0f10001e)
        return
L41220:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *          D i s p l a y   A n a l y s i s   D a t a        *~
            *************************************************************

        deffn'102(fieldnr%)
L42050:     gosub set_pf2
            accept                                                       ~
               at (01,02), fac(hex(84)), line1$                 , ch(30),~
               at (01,63), fac(hex(84)), page$                  , ch(16),~
                                                                         ~
               at (02,02), fac(hex(84)), plan_line$             , ch(18),~
                                                                         ~
               at (02,21), fac(hex(94)), hdr$                   , ch(40),~
                                                                         ~
               at (02,65), fac(hex(84)), plan_line1$            , ch(12),~
                                                                         ~
               at (03,02), "Begin Shift     :",                          ~
               at (03,20), fac(hex(84)), bg_shft$               , ch(02),~
               at (03,40), "End Shift       :",                          ~
               at (03,60), fac(hex(84)), ed_shft$               , ch(02),~
                                                                         ~
               at (04,02), "Begin Prod Day  :",                          ~
               at (04,20), fac(hex(84)), bg_day$                , ch(02),~
               at (04,25), fac(hex(84)), bg_day_dte$            , ch(10),~
               at (04,40), "End Prod Day    :",                          ~
               at (04,60), fac(hex(84)), ed_day$                , ch(02),~
               at (04,65), fac(hex(84)), ed_day_dte$            , ch(10),~
                                                                         ~
               at (05,02), "Display Prod Day:",                          ~
               at (05,20), fac(lfac$(1%)), sel_d$               , ch(02),~
               at (05,25), fac(hex(84)), sel_d_dte$             , ch(10),~
               at (05,40), "Display Shift   :",                          ~
               at (05,60), fac(lfac$(1%)), sel_s$               , ch(02),~
               at (05,65), fac(hex(84)), sel_s_d$               , ch(15),~
                                                                         ~
               at (06,02), fac(hex(a4)), h1$                    , ch(25),~
               at (06,28), fac(hex(a4)), h2$                    , ch(11),~
               at (06,40), fac(hex(a4)), h3$                    , ch(25),~
               at (06,68), fac(hex(a4)), h4$                    , ch(11),~
                                                                         ~
               at (07,02), fac(hex(84)), cc$(1% + kk%)          , ch(25),~
               at (07,28), fac(hex(84)), dd$(1% + kk%)          , ch(11),~
               at (07,40), fac(hex(84)), cc$(16%+ kk%)          , ch(25),~
               at (07,68), fac(hex(84)), dd$(16%+ kk%)          , ch(11),~
                                                                         ~
               at (08,02), fac(hex(84)), cc$(2% + kk%)          , ch(25),~
               at (08,28), fac(hex(84)), dd$(2% + kk%)          , ch(11),~
               at (08,40), fac(hex(84)), cc$(17%+ kk%)          , ch(25),~
               at (08,68), fac(hex(84)), dd$(17%+ kk%)          , ch(11),~
                                                                         ~
               at (09,02), fac(hex(84)), cc$(3% + kk%)          , ch(25),~
               at (09,28), fac(hex(84)), dd$(3% + kk%)          , ch(11),~
               at (09,40), fac(hex(84)), cc$(18%+ kk%)          , ch(25),~
               at (09,68), fac(hex(84)), dd$(18%+ kk%)          , ch(11),~
                                                                         ~
               at (10,02), fac(hex(84)), cc$(4% + kk%)          , ch(25),~
               at (10,28), fac(hex(84)), dd$(4% + kk%)          , ch(11),~
               at (10,40), fac(hex(84)), cc$(19%+ kk%)          , ch(25),~
               at (10,68), fac(hex(84)), dd$(19%+ kk%)          , ch(11),~
                                                                         ~
               at (11,02), fac(hex(84)), cc$(5% + kk%)          , ch(25),~
               at (11,28), fac(hex(84)), dd$(5% + kk%)          , ch(11),~
               at (11,40), fac(hex(84)), cc$(20%+ kk%)          , ch(25),~
               at (11,68), fac(hex(84)), dd$(20%+ kk%)          , ch(11),~
                                                                         ~
               at (12,02), fac(hex(84)), cc$(6% + kk%)          , ch(25),~
               at (12,28), fac(hex(84)), dd$(6% + kk%)          , ch(11),~
               at (12,40), fac(hex(84)), cc$(21%+ kk%)          , ch(25),~
               at (12,68), fac(hex(84)), dd$(21%+ kk%)          , ch(11),~
                                                                         ~
               at (13,02), fac(hex(84)), cc$(7% + kk%)          , ch(25),~
               at (13,28), fac(hex(84)), dd$(7% + kk%)          , ch(11),~
               at (13,40), fac(hex(84)), cc$(22%+ kk%)          , ch(25),~
               at (13,68), fac(hex(84)), dd$(22%+ kk%)          , ch(11),~
                                                                         ~
               at (14,02), fac(hex(84)), cc$(8% + kk%)          , ch(25),~
               at (14,28), fac(hex(84)), dd$(8% + kk%)          , ch(11),~
               at (14,40), fac(hex(84)), cc$(23%+ kk%)          , ch(25),~
               at (14,68), fac(hex(84)), dd$(23%+ kk%)          , ch(11),~
                                                                         ~
               at (15,02), fac(hex(84)), cc$(9% + kk%)          , ch(25),~
               at (15,28), fac(hex(84)), dd$(9% + kk%)          , ch(11),~
               at (15,40), fac(hex(84)), cc$(24%+ kk%)          , ch(25),~
               at (15,68), fac(hex(84)), dd$(24%+ kk%)          , ch(11),~
                                                                         ~
               at (16,02), fac(hex(84)), cc$(10%+ kk%)          , ch(25),~
               at (16,28), fac(hex(84)), dd$(10%+ kk%)          , ch(11),~
               at (16,40), fac(hex(84)), cc$(25%+ kk%)          , ch(25),~
               at (16,68), fac(hex(84)), dd$(25%+ kk%)          , ch(11),~
                                                                         ~
               at (17,02), fac(hex(84)), cc$(11%+ kk%)          , ch(25),~
               at (17,28), fac(hex(84)), dd$(11%+ kk%)          , ch(11),~
               at (17,40), fac(hex(84)), cc$(26%+ kk%)          , ch(25),~
               at (17,68), fac(hex(84)), dd$(26%+ kk%)          , ch(11),~
                                                                         ~
               at (18,02), fac(hex(84)), cc$(12%+ kk%)          , ch(25),~
               at (18,28), fac(hex(84)), dd$(12%+ kk%)          , ch(11),~
               at (18,40), fac(hex(84)), cc$(27%+ kk%)          , ch(25),~
               at (18,68), fac(hex(84)), dd$(27%+ kk%)          , ch(11),~
                                                                         ~
               at (19,02), fac(hex(84)), cc$(13%+ kk%)          , ch(25),~
               at (19,28), fac(hex(84)), dd$(13%+ kk%)          , ch(11),~
               at (19,40), fac(hex(84)), cc$(28%+ kk%)          , ch(25),~
               at (19,68), fac(hex(84)), dd$(28%+ kk%)          , ch(11),~
                                                                         ~
               at (20,02), fac(hex(84)), cc$(14%+ kk%)          , ch(25),~
               at (20,28), fac(hex(84)), dd$(14%+ kk%)          , ch(11),~
               at (20,40), fac(hex(84)), cc$(29%+ kk%)          , ch(25),~
               at (20,68), fac(hex(84)), dd$(29%+ kk%)          , ch(11),~
                                                                         ~
               at (21,02), fac(hex(84)), cc$(15%+ kk%)          , ch(25),~
               at (21,28), fac(hex(84)), dd$(15%+ kk%)          , ch(11),~
               at (21,40), fac(hex(84)), cc$(30%+ kk%)          , ch(25),~
               at (21,68), fac(hex(84)), dd$(30%+ kk%)          , ch(11),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L43250            /* First    */
L43220:           kk% = 0%
                  goto L42050

L43250:        if keyhit% <> 3% then goto L43300            /* Last      */
L43260:           x% = int(sc_max% / 30%)
                  kk% = (x%*30%)
                  goto L42050

L43300:        if keyhit% <> 4% then goto L43360            /* Previous */
                  if kk% < 31% then goto L43220
                  kk% = kk% - 30%
                  if kk% <= 1% then goto L43220
                  goto L42050

L43360:        if keyhit% <> 5% then goto L43410            /* Next     */
                  kk% = kk% + 30%
                  if kk% < sc_max% then goto L42050
                  goto L43260

L43410:        if keyhit% <> 8% then goto L43450
                  dept% = 999%
                  goto L42050

L43450:        if keyhit% <> 15 then goto L43490
                  call "PRNTSCRN"
                  goto L42050

L43490:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return
                                               /* Build Screen of Data */
        set_pf2                                /* for Departments      */
          init(" ") dt_time$
          call "TIME" (dt_time$)
          line1$ = dt_time$ & " @ " & date$ & " (APCPLN06)"
          page$  = "Page: 001 of 001"
          lfac$(1%) = hex(84)                  /* Position Cusrsor to  */
          if dept% <> 0% then goto set_pf3     /* Detail Screen        */
            if keyhit% > 1% and keyhit% < 6% then goto L43920   /* Page */

            lfac$(1%) = hex(81)                /* Change Day/Shift     */
            sc_max% = dpt_max%                 /* Max No. Dept's       */
                                               /*  (EWD013) Change Scrn*/
            h1$ = "Dpt < Description >EF UNT"                  /* (25) */
            h2$ = "RCap. Units"                                /* (11) */
            h3$ = "Dpt < Description >EF UNT"                  /* (25) */
            h4$ = "RCap. Units"                                /* (11) */

            init(" ") cc$(), dd$(), sv_root$, sv_dept$, inpmessage$
            hdr$   = "  D e p a r t m e n t   D i s p l a y   "
            inpmessage$ = "Select Dept. or Change Prod. Day or Shift and ~
        ~Press <Return>?"
            dpt% = 0%
            for i% = 1% to pln_max%             /* Test Dept,Proc,Shift*/
                if sv_dept$ = str(dpt$(i%),1%,3%) then goto L43770
                   gosub load_scr1
L43770:         if sv_root$ = dpt$(i%) then goto L43900
                   sv_root$ = dpt$(i%)
                d% = 1%                             /* Production Day  */
                convert str(sv_root$,8%,1%) to d%, data goto L43840
                s% = 1%                             /* Production Shift*/
                convert str(sv_root$,6%,2%) to s%, data goto L43830
L43830:
L43840:         if sel_d% = 0% then goto L43860      /* All Days        */
                   if sel_d% <> d% then goto L43900  /* Specified Day   */
L43860:         if sel_s% = 0% then goto L43880      /* All Shifts      */
                   if sel_s% <> s% then goto L43900  /* Specified Shift */
L43880:         capacity = capacity + cap(i%)       /* Remain Capacity */
                units%   = units%   + unt%(i%)      /* Total Prod Units*/
                ef_units% = ef_units% + ef_unt%(i%) /*   (EWD013)      */
L43900:    next i%                                  /* Dept,Proc,Shift */
           gosub load_scr1                          /* Day             */
L43920:     pf$(1%) = "Select Dept & Press <Return> for Departm" &       ~
                      "ent Detail             (15)Print Screen"
            pf$(2%) = "(2)First  (3)Last   (4)Previous   (5)Nex" &       ~
                      "t   (8)Load Analysis   (16)Exit Screen "
            pfkeys$ = hex(0102030405ffff08ffffffffffff0f1000)
            gosub check_scr
            x% = int(sc_max%/30.0) + 1%
            convert x% to str(page$,14%,3%), pic(000)
            x% = int(kk%/30.0) + 1%
            convert x% to str(page$,7%,3%), pic(000)
        return

        load_scr1                                  /* For a Department */
            if i% = 1% then goto L44140
               dpt% = dpt% + 1%                      /* (30) to a Page */
               if dpt% > screen% then dpt% = screen% /* Max (40) Dept's*/
               str(cc$(dpt%),1%,3%) = sv_dept$
               convert int(capacity) to str(dd$(dpt%),1%,5%), pic(#####)
               convert units% to str(dd$(dpt%),7%,5%), pic(#####)
               code$ = sv_dept$
               tab% = 1% : gosub check_code
               str(cc$(dpt%),5%,15%) = str(desc$,1%,15%)
               convert ef_units% to str(cc$(dpt%),20%,5%), pic(#####)
L44140:     init(" ") sv_dept$, sv_root$
            sv_dept$ = str(dpt$(i%),1%,3%)          /* Department      */
            capacity = 0.0 : units% = 0%            /* Day             */
            ef_units% = 0%                          /*  (EWD013)       */
        return

        load_department
            d% = 1%                                 /* Production Day  */
            convert str(sv_root$,8%,1%) to d%, data goto L44250
            s% = 1%                                 /* Production Shift*/
            convert str(sv_root$,6%,2%) to s%, data goto L44240
L44240:
L44250:     if sel_d% = 0% then goto L44270          /* All Days        */
               if sel_d% <> d% then return          /* Specified Day   */
L44270:     if sel_s% = 0% then goto L44290          /* All Shifts      */
               if sel_s% <> s% then return          /* Specified Shift */
L44290:     for j% = 1% to 20%
              if len(pp$(i%,j%)) <> 3 then return
              if sc_max% = 0% then goto L44350
              for jj% = 1% to sc_max%
               if str(cc$(jj%),1%,3%) = pp$(i%,j%) then goto L44460
              next jj%
L44350:       sc_max% = sc_max% + 1%
              if sc_max% > screen% then sc_max% = screen%
              jj% = sc_max%
              str(cc$(jj%),1%,3%) = pp$(i%,j%)
              code$ = pp$(i%,j%)
              if code$ <> "999" then goto L44430
                 str(cc$(jj%),5%,21%) = "(ALL) Other Models"
                 goto L44460
L44430:       tab% = 3% : gosub check_code
              str(cc$(jj%),5%,21%) = str(desc$,1%,21%)
        REM - on Screen
L44460:       x% = 0%
              convert str(dd$(jj%),7%,5%) to x%, data goto L44480
L44480:
              convert (pu%(i%,j%) + x%) to str(dd$(jj%),7%,5%),pic(#####)
            next j%
        return

        set_pf3
            if dept% = 999% then goto set_pf4
               if keyhit% > 1% and keyhit% < 6% then goto L44720
               sc_max% = 0%
               h1$ = "Mod <--- Description --->"               /* (25) */
               h2$ = "      Units"                             /* (11) */
               h3$ = "Mod <--- Description --->"               /* (25) */
               h4$ = "      Units"                             /* (11) */
            inpmessage$ = "Display of Department Detail. Press <Return> t~
        ~o Continue?"

            hdr$   = "Detail for Dept. ("& str(cc$(dept%),5%,21%)&")"
            sc_max% = 0%       /* Load Product for Selected Department */
            init(" ") cc$(), dd$(), sv_root$
            for i% = d_sb%(dept%,1%) to d_sb%(dept%,3%)
                sv_root$ = str(dpt$(i%),1%,8%)
                gosub load_department
            next i%

L44720:     x% = int(sc_max%/30.0) + 1%
            convert x% to str(page$,14%,3%), pic(000)
            x% = int(kk%/30.0) + 1%
            convert x% to str(page$,7%,3%), pic(000)
            pf$(1%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "(2)First  (3)Last  (4)Previous  (5)Next " &       ~
                      "                                       "
            pfkeys$ = hex(0102030405ffffffffffffffffff0fff00)
            gosub check_scr
        return

        set_pf4
            if keyhit% > 1% and keyhit% < 6% then goto L45270
               call "SHOSTAT" ("Building Truck Load Display")

            h1$ = " Load <-- Description -->"                  /* (25) */
            h2$ = "Prod Date  "                                /* (11) */
            h3$ = "Comp Date   Load Date    "                  /* (25) */
            h4$ = "Load Value "                                /* (11) */
            hdr$   = " T r u c k   L o a d s   D i s p l a y  "
            inpmessage$ = "Display Truck Scheduling Data. Press <Return> ~
        ~to Continue?"
            ll% = 0% : sc_max% = 0% : lmod% = 16% : total = 0.0
            init(" ") cc$(), dd$(), sv_root$, desc$
            if ld_max% = 0% then goto L45270
            for ld% = 1% to ld_max%
                if sel_d% = 0% then goto L45030          /* All Days    */
                   convert str(ll$(ld%),6%,2%) to d%, data goto L45010
L45010:
                   if sel_d% <> d% then goto L45240
L45030:         ll% = ll% + 1%
                if mod(ll%,lmod%) <> 0% then goto L45080
                   ll% = ll% + 15%
                   lmod% = ll% + 15%

L45080:         if (ll%/2%) > screen% then ll% = screen% * 2%
                read #4,key = str(ll$(ld%),1%,5%), using L45110, desc$,   ~
                                                           eod goto L45120
L45110:            FMT POS(16), CH(30)
L45120:         str(cc$(ll%),1%,5%)  = str(ll$(ld%),1%,5%)
                str(cc$(ll%),7%,19%) = str(desc$,1%,19%)
                gosub convert_dates
                call "DATFMTC" (ld_dtp1$)
                call "DATFMTC" (ld_dtp2$)
                call "DATFMTC" (ld_dtp3$)
                str(dd$(ll%),1%,10%)      = ld_dtp1$ /* Production Date */
                str(cc$(ll%+15%),1%,10%)  = ld_dtp2$ /* Completion Date */
                str(cc$(ll%+15%),13%,10%) = ld_dtp3$ /* Load Date       */
                gosub calc_load_value
                convert ld_value to dd$(ll%+15%),pic(#######.##-)
                total = round(total + ld_value, 2)
L45240:     next ld%
            convert total to str(inpmessage$,66%,12%), pic($#######.##-)

L45270:     x% = mod(ll%,30%)
            sc_max% = ll% + x%
            x% = int(sc_max%/30.0) + 1%
            convert x% to str(page$,14%,3%), pic(000)
            x% = int(kk%/30.0) + 1%
            convert x% to str(page$,7%,3%), pic(000)
            pf$(1%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "(2)First  (3)Last  (4)Previous  (5)Next " &       ~
                      "                                       "
            pfkeys$ = hex(0102030405ffffffffffffffffff0fff00)
            gosub check_scr
        return

        check_scr
            if sc_max% > 30% then goto L45480
               gosub no_fst
               gosub no_nxt
               gosub no_lst
               gosub no_prv
               return
L45480:      if kk% >= 30% then goto L45510
                gosub no_fst
                gosub no_prv
L45510:      if (kk% + 30%) <= sc_max% then goto L45530
                gosub no_lst
L45530:      if kk% <= (sc_max% - 30%) then goto L45550
                gosub no_nxt
L45550: return
        no_fst
            str(pf$(2%),1%, 8%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_nxt
            str(pf$(2%),33%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_lst
            str(pf$(2%),11%,8%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prv
            str(pf$(2%),20%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        calc_load_value
            init(" ") sc_key$ : ld_value = 0.0
            str(sc_key$,1%,5%) = str(ll$(ld%),1%,5%)
L45720:     read #5,key 1% > sc_key$, using L45740, sc_key$, sc_price,    ~
                                                   eod goto L45780
L45740:        FMT POS(7), CH(27), POS(76), PD(14,4)
            if str(sc_key$,1%,5%) <> str(ll$(ld%),1%,5%) then return
            ld_value = ld_value + sc_price
            goto L45720
L45780: return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50210,         /* Planning Prod. Year   */ ~
                              L50340,         /* Planning Prod. Week   */ ~
                              L50860,         /* Beginning Department  */ ~
                              L51050,         /* Ending Department Code*/ ~
                              L51240,         /* Beginning Process Code*/ ~
                              L51430,         /* Ending Process Code   */ ~
                              L51620,         /* Beginning Shift Code  */ ~
                              L51810,         /* Beginning Shift Code  */ ~
                              L52000,         /* Starting Production Dy*/ ~
                              L52160,         /* Ending Production Day */ ~
/* (AWD017) */                L53000          /* Sort Wood             */

            return

L50210: REM Planning Production Year
            leap_yr% = 0%
            if bg_year$ <> " " then goto L50250
               bg_year$ = str(date_ufmt$,1%,4%)
L50250:     call "NUMTEST" (bg_year$,1995,2399,errormsg$,0,bg_year)
            if errormsg$ > " " then L50300
            bg_yr% = bg_year
            bg_yr$ = bin(bg_yr%,2)
            if mod(bg_yr%,4%) = 0% then leap_yr% = 1%
        return
L50300:     errormsg$ = "(Error) - Invalid Production Year??"
            init(" ") bg_year$
        return

L50340: REM Planning Production Week         BG_WK$, BG_DTE$, BG_DATE$
            ent_yr$ = bg_yr$
            ent_wk$ = bg_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

           if pl_e% <> 0% then goto L50820
           bg_wk$   = ent_wk$
           bg_dte$  = ent_dte$
           bg_date$ = ent_date$
           x% = val(ent_yr$,2)           /* ENTRY YEAR    */

           y% = val(cur_yr$,2)           /* CURRENT YEAR  */

           zz% = (x% - y%) * 52%

           x% = 0% : y% = 0%
           convert ent_wk$ to x%, data goto L50820     /* ENTRY WEEK    */

           convert cur_wk$ to y%, data goto L50650     /* CURRENT WEEK  */
L50650:
           x% = x% + zz%

           current$ = "Current: +XXX"
           if x% <> y% then goto L50720
              str(current$,11%,3%) = "000"
              goto L50770
L50720:    if x% > y% then goto L50760
              str(current$,10%,1%) = "-"                /* Previous Wk */
              convert (y%-x%) to str(current$,11%,3%), pic(000)
              goto L50770                                /* FUTURE WK   */
L50760:    convert (x%-y%) to str(current$,11%,3%), pic(000)
L50770:    sv_month$ = str(bg_date$,1%,2%)
           if edit% = 1% then return
              gosub L52000                               /*             */
              gosub L52160                               /*             */
        return
L50820:    errormsg$ = "(Error) - Invalid Production Week (1 thru 53)?"
           init(" ") bg_wk$, bg_dte$, bg_date$, sv_month$
        return

L50860: REM Beginning Department Code             BG_DEPT$, BG_DEPT_D$
            if bg_dept$ <> " " then goto L50920
L50880:        bg_dept$ = "ALL"
               bg_dept_d$ = "(All) - Departments"
               return

L50920:     if str(bg_dept$,1%,1%) = "A" then goto L50880
            convert bg_dept$ to x%, data goto L50960

            convert x% to bg_dept$, pic(000)
L50960:     code$ = bg_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L51010
            bg_dept_d$ = desc$
        return
L51010:     errormsg$ = "(Error) - Invalid Department Code ??"
            init(" ") bg_dept$, bg_dept_d$
        return

L51050: REM Ending Department Code                ED_DEPT$, ED_DEPT_D$
            if ed_dept$ <> " " then goto L51110
L51070:        ed_dept$ = bg_dept$
               ed_dept_d$ = bg_dept_d$
               return

L51110:     if str(bg_dept$,1%,1%) = "A" then goto L51070
            convert ed_dept$ to x%, data goto L51150

            convert x% to ed_dept$, pic(000)
L51150:     code$ = ed_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L51200
            ed_dept_d$ = desc$
        return
L51200:     errormsg$ = "(Error) - Invalid Department Code ??"
            init(" ") ed_dept$, ed_dept_d$
        return

L51240: REM Beginning Process Code                BG_PROC$, BG_PROC_D$
            if bg_proc$ <> " " then goto L51300
L51260:        bg_proc$ = "AA"
               bg_proc_d$ = "(All) - Process Codes"
               return

L51300:     if str(bg_proc$,1%,1%) = "A" then goto L51260
            convert bg_proc$ to x%, data goto L51340

            convert x% to bg_proc$, pic(00)
L51340:     code$ = bg_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L51390
            bg_proc_d$ = desc$
        return
L51390:     errormsg$ = "(Error) - Invalid Processing Code ??"
            init(" ") bg_proc$, bg_proc_d$
        return

L51430: REM Ending Process Code                   ED_PROC$, ED_PROC_D$
            if ed_proc$ <> " " then goto L51490
L51450:        ed_proc$ = bg_proc$
               ed_proc_d$ = bg_proc_d$
               return

L51490:     if str(bg_proc$,1%,1%) = "A" then goto L51450
            convert ed_proc$ to x%, data goto L51530

            convert x% to ed_proc$, pic(00)
L51530:     code$ = ed_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L51580
            ed_proc_d$ = desc$
        return
L51580:     errormsg$ = "(Error) - Invalid Processing Code ??"
            init(" ") ed_proc$, ed_proc_d$
        return

L51620: REM Beginning Shift Code                  BG_SHFT$, BG_SHFT_D$
            if bg_shft$ <> " " then goto L51680
L51640:        bg_shft$ = "AA"
               bg_shft_d$ = "(All) - Shifts"
               return

L51680:     if str(bg_shft$,1%,1%) = "A" then goto L51640
            convert bg_shft$ to x%, data goto L51720

            convert x% to bg_shft$, pic(00)
L51720:     code$ = bg_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L51770
            bg_shft_d$ = desc$
        return
L51770:     errormsg$ = "(Error) - Invalid Shift Code ??"
            init(" ") bg_shft$, bg_shft_d$
        return

L51810: REM Ending Shift Code                     ED_SHFT$, ED_SHFT_D$
            if ed_shft$ <> " " then goto L51870
L51830:        ed_shft$ = bg_shft$
               ed_shft_d$ = bg_shft_d$
               return

L51870:     if str(bg_shft$,1%,1%) = "A" then goto L51830
            convert ed_shft$ to x%, data goto L51910

            convert x% to ed_shft$, pic(00)
L51910:     code$ = ed_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L51960
            ed_shft_d$ = desc$
        return
L51960:     errormsg$ = "(Error) - Invalid Shift Code ??"
            init(" ") ed_shft$, ed_shft_d$
        return

L52000: REM Beginning Production Day           BG_DAY$, BG_DAY_D$
            if bg_day$ <> " " then goto L52040
               bg_day$ = "1"

L52040:     convert bg_day$ to bg_day%, data goto L52120

            if bg_day% < 1% or bg_day% > 7% then goto L52120
               bg_day_d$ = days$(bg_day%)
            pln_day$ = bg_day$

            call "DATE" addr("G+",bg_dte$, (bg_day%-1%),bg_day_dte$,err%)
            call "DATFMTC" (bg_day_dte$)
REM         GOSUB CALC_DATE
REM         BG_DAY_DTE$ = PLN_DATE$
        return
L52120:     errormsg$ = "(Error) - Invalid Production Start Day??"
            init(" ") bg_day$, bg_day_d$, bg_day_dte$
        return

L52160: REM Ending Production Day              ED_DAY$, ED_DAY_D$
            if ed_day$ <> " " then goto L52210
               ed_day% = bg_day% + 6%
               convert ed_day% to ed_day$, pic(00)

L52210:     convert ed_day$ to ed_day%, data goto L52330

            convert ed_day% to ed_day$, pic(00)

            if ed_day% < 1% or ed_day% > 14% then goto L52330
               ed_day_d$ = days$(ed_day%)
               if bg_day% > ed_day% then goto L52330
               if (ed_day% - bg_day%) > 6% then goto L52330
               pln_day$ = ed_day$
            call "DATE" addr("G+",bg_dte$, (ed_day%-1%),ed_day_dte$,err%)
            call "DATFMTC" (ed_day_dte$)
REM               GOSUB CALC_DATE
REM               ED_DAY_DTE$ = PLN_DATE$
        return
L52330:     errormsg$ = "(Error) - Invalid Production Ending Day??"
            init(" ") ed_day$, ed_day_d$, ed_day_dte$
        return

        calc_date                           /* BG_DTE$  = Input  UnForm*/
           julian_days% = 365%              /* Julian Days in Year     */
REM           IF LEAP_YR% = 1% THEN JULIAN_DAYS% = 366%   /* LEAP YEAR    */
                                            /* PLN_DAY$ = Input  Day   */
                                            /* PLN_DATE = Output Format*/
                                          /* Specified Production Week */
           call "DATE" addr("GJ", str(bg_dte$,,6%), str(jdate1$,,5%), x%)
                                          /* Convert to Julian Date Fmt*/
           call "DATJULCV" (jdate1$)    /* Convert from Packed to Ascii*/
           convert str(jdate1$,5%,3%) to j1%, data goto L52480
L52480:
           convert pln_day$ to dd%, data goto L52630
                                          /* Calculate the Date Assoc. */
           j1% = j1% + (dd% - 1%)         /* with Specified Prod Day   */
           if j1% <= julian_days% then goto L52570
                                                       /*  (EWD011)    */
REM              CONVERT (BG_YR% + 1%) TO STR(JDATE1$,1%,4%), PIC(0000)
              convert bg_yr% to str(jdate1$,1%,4%), pic(0000)

              j1% = j1% - julian_days%

L52570:    convert j1% to str(jdate1$,5%,3%), pic(000)
           call "DATJULCV" (jdate1$)    /* Convert back to Packed */
                                          /* Begin with Production*/
                                           /* Date and   */
           call "DATE" addr("JG", str(jdate1$,,5%), str(pln_dte$,,6%), x%)
           pln_date$ = pln_dte$
           call "DATFMTC" (pln_date$)
        return
L52630:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           init(" ") pln_date$, pln_dte$, pln_day$
        return
L53000: REM Sort Wood?                         SORT_WOOD$
            if wood_sort$  = " " then wood_sort$ = "Y"
            if wood_sort$ <> "Y" and wood_sort$ <> "N" then goto L53090

        return
L53090:     errormsg$ = "(Error) - Invalid Wood Sort??"
            init(" ") wood_sort$
        return


        REM *************************************************************~
            *      E d i t   F o r   A n a l y s i s   D e t a i l      *~
            *-----------------------------------------------------------*~
            * Analysis Detail Product Screen Edits                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53110,         /* Production Day        */ ~
                              L53310          /* Production Shift      */

L53110: REM Change the Production Day          SEL_D$
            sel_d% = 0%
            if sel_d$ <> " " then goto L53170
               sel_d$ = "AA"
               sel_d_dte$ = "ALL-DAYS"

L53170:     if sel_d$ = "AA" then return
               convert sel_d$ to sel_d%, data goto L53260

            if sel_d% < bg_day% or sel_d% > ed_day% then goto L53260

            pln_day$ = sel_d$

            call "DATE" addr("G+",bg_dte$, (sel_d%-1%),sel_d_dte$,err%)
            call "DATFMTC" (sel_d_dte$)

REM            gosub calc_date
REM            sel_d_dte$ = pln_date$
        return
L53260:     errormsg$ = "(Error) - Invalid Selection Day??"
            init(" ") sel_d$, sel_d_dte$
            gosub error_prompt
        return

L53310: REM Change the Production Shift        SEL_S$
            sel_s% = 0%
            if sel_s$ <> " " then goto L53370
               sel_s$ = "AA"
               sel_s_d$ = "(All) - Shifts"

L53370:     if sel_s$ = "AA" then return
               convert sel_s$ to sel_s%, data goto L53480

               if bg_shft$ = "AA" then goto L53430
            if sel_s$ < bg_shft$ or sel_s$ > ed_shft$ then goto L53480

L53430:     code$ = sel_s$
            tab% = 4% : gosub check_code
            sel_s_d$ = str(desc$,1%,15%)
            if code% = 0% then goto L53480
        return
L53480:     errormsg$ = "(Error) - Invalid Selection Shift?"
            init(" ") sel_s$, sel_s_d$
            gosub error_prompt
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        check_code
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = tab$(tab%)
           str(readkey$,10%,15%)  = code$
           read #3,key = readkey$, using L60050, desc$, eod goto L60065
L60050:        FMT POS(25), CH(30)
           code% = 1%
        return
L60065:    errormsg$ = "(Error) - Invalid Code Value Entered ??"
        return

        display_codes
            call "APCPLN1B" (tab%, #3)
        return

        stock_sort                      /* Bin Location for 'Pull' Part*/
            stk_key1$ = all(hex(00))
            str(stk_key1$,1%,25%) = dt_part$
            read #12,key 1% > stk_key1$, using L60125, stk_key1$,         ~
                                                      eod goto L60145
L60125:       FMT POS(08), CH(32)
            if str(stk_key1$,1%,25%) = dt_part$ then                     ~
                                   dt_sort$ = str(stk_key1$,26%,5%)      ~
                                   else dt_sort$ = "99999"
L60145: return

        REM *************************************************************~
            *           L o a d   C a p a c i t y   D a t a             *~
            *************************************************************

        load_capacity                          /* (APCPLXXX)-File      */
            call "SHOSTAT" ("(1)-Loading Dept's Capacities")
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 600%, rslt$(10%))
            init(" ") pl_key$                  /* Need (BG_DAY%) and   */
            str(pl_key$,1%,2%) = bg_yr$        /* (ED_DAY%) from Input */
            str(pl_key$,3%,2%) = bg_wk$
*       RHH
*       RHH  CALL "SHOSTAT" (" (1) - Begin Load Capacity")
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
        load_cap_nxt                           /* (APCPLNUC) - Capacity*/
            read #1,key > pl_key$, using L60245, pl_key$, pl_sort$,       ~
                   pl_day$, pl_unts(), pl_unta%(), pl_unte%(), pl_sort1$, ~
                                                  eod goto load_cap_done
L60245:        FMT CH(11), POS(18), CH(12), CH(1), 7*PD(14,4), 7*BI(2),   ~
                   7*BI(2), POS(123), CH(3)
                                               /* (EWD005)               */
                                               /* (EWD013)               */
            if str(pl_key$,1%,2%) <> bg_yr$ then goto load_cap_done
            if str(pl_key$,3%,2%) <> bg_wk$ then goto load_cap_done
            if bg_dept$ = "ALL" then goto L60275
                 if str(pl_key$,7%,3%) < bg_dept$ or                     ~
                    str(pl_key$,7%,3%) > ed_dept$ then  goto load_cap_nxt
L60275:     if bg_proc$ = "AA" then goto L60290
                 if str(pl_key$,10%,2%) < bg_proc$ or                    ~
                    str(pl_key$,10%,2%) > ed_proc$ then goto load_cap_nxt
L60290:     if bg_shft$ = "AA" then goto L60310
                 if str(pl_key$,5%,2%) < bg_shft$ or                     ~
                    str(pl_key$,5%,2%) > ed_shft$ then  goto load_cap_nxt
                                               /* Calc Factor, Unique  */
L60310:     factor% = 0%                       /* By Dept,Proc,Shift   */
            convert pl_day$ to factor%, data goto L60340 /*(APCPLNUC)*/
                                               /* Day Factor Unique to */
            factor% = factor% - 1%             /* Dept,Proc,Shift for  */
            if factor% < 1% then factor% = 0%  /* Prod Week (APCPLNUC) */

L60340:                                        /* Check the Planning   */
            for i% = 1% to 7%                  /* Screen Range of Days */
              rh2% =(i% + factor%)             /* Adjust the Day       */
        REM   IF RH2% < BG_DAY% OR RH2% > ED_DAY% THEN GOTO 60405/*Skip*/
              convert i% to i$, pic(0)
              str(wrk$,1%,3%) = str(pl_key$,7%,3%)       /* Dept Code  */
              str(wrk$,4%,1%) = i$                       /* Day 1 - 7  */
              str(wrk$,5%,2%) = str(pl_key$,10%,2%)      /* Proc Code  */
              str(wrk$,7%,2%) = str(pl_key$,5%,2%)       /* Shift Code */
              str(wrk$,9%,12%) = pl_sort$                /* Sort Code  */
              str(wrk$,21%,3%) = pl_sort1$               /* (EWD005)   */
                                                         /* (EWD013)   */
              write #10, using L60400, wrk$, pl_unts(i%), pl_unta%(i%),   ~
                                      factor%, " ", pl_unte%(i%),         ~
                                      eod goto L60410
L60400:           FMT CH(25), PD(14,4), BI(2), BI(1), CH(1), BI(2)
L60405:     next i%
L60410:     goto load_cap_nxt
                 call "SHOSTAT" ("Error - Sorting Capacity - "&wrk$)
                 stop
                 goto L60405                    /* Return to Flow       */

        load_cap_done
           call "SHOSTAT" ("(2)-Formating Dept's Capacities")
              pln_max% = 0% : dpt_max% = 0% : m100% = 0%
              ld_max%  = 0% : p_max%   = 0% : m102% = 0%
              i%       = 0% : k%       = 0% : m104% = 0%
           init(" ") dpt$(), readkey$, sv_dept$, d_dp$(), pp$()
*       RHH
*       RHH  CALL "SHOSTAT" (" (2) - Unload Work - Map Memory")
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
        load_pln_dpt                                      /*  (EWD013)   */
           read #10,key > readkey$, using L60505, readkey$, rh1, rh2%,    ~
                                          rh3%, cg1%,                ~
                                          eod goto load_pln_done
L60505:       FMT CH(25), PD(14,4), BI(2), BI(1), XX(1), BI(2)
           i% = i% + 1% : if i% > plan% then i% = plan%
           if sv_dept$ = str(readkey$,1%,3%) then goto load_pln_nxt
              sv_dept$ = str(readkey$,1%,3%) /* Save Department Code   */
              pl_sort$ = str(readkey$,9%,12%) /* Save Sort Code 1st Shf*/
              pl_sort1$= str(readkey$,21%,3%) /* (EWD005)              */
*       RHH
*       RHH  CALL "SHOSTAT" (" (3) - Begin New Dept" & SV_DEPT$)
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
              k% = k% + 1% : if k% > depart% then k% = depart%
              if sv_dept$ = "100" then m100% = k% /* Overflow Dept     */
              if sv_dept$ = "102" then m102% = k% /* Stock Finished    */
              if sv_dept$ = "104" then m104% = k% /* Stock Shapes      */

              str(d_dp$(k%),1%,3%) = sv_dept$ /* Store Department Code */
              str(d_dp$(k%),4%,12%) = pl_sort$            /* Sort Code */
              str(d_dp$(k%),16%,3%) = pl_sort1$           /* (EWD005)  */

              d_sb%(k%,1%) = i%              /* Starting Sub-Script Dpt*/
              d_sb%(k%,2%) = i%              /* Sub-Script 1st Opn Day */
              d_sb%(k%,4%) = rh3%            /* Store Start Day Factor */
                                             /* for Dept,Proc,Shift    */
              gosub load_upmh               /* Load and Convert Pcnt's */
        load_pln_nxt                        /* to a Decimal Value      */
           dpt$(i%) = str(readkey$,1%,8%)   /* Save the Sorted Record  */
           cap(i%)  = rh1                   /* for Planning an Analysis*/
           unt%(i%) = rh2%                  /* May contain the Values  */
           ef_unt%(i%) = cg1%               /* (EWD013)                */
           d_sb%(k%,3%) = i%                /* Ending Sub-Script for   */
              gosub debug                   /* Department Capacities   */
           goto load_pln_dpt                /* from Last Planning Run  */
        load_pln_done
           pln_max% = i%                    /* Max for Planning Week   */
           dpt_max% = k%                    /* Max No of Departments   */
           d_sb%(k%,3%) = i%                /* Ending Sub-Script for   */
           call "FILEBGON" (#10)            /* Department Capacities   */
           if analysis% = 1% then gosub load_store     /* Warranty No. */
           if analysis% = 1% then gosub load_store_stock /*(AWD025)*/
           gosub debug
*       RHH
*       RHH  CALL "SHOSTAT" (" (4) - Done Loading Capacity")
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
        return

        load_upmh                           /* Get Goal Percentage     */
            init(" ") gen$, desc$           /* Value Dept, Month, Shift*/
            for j% = 1% to shifts%          /* SHIFTS% = Max No. of    */
              d_wt(k%,j%) = 1.0             /*           Shifts Allowed*/
            next j%
            j% = 1%
            str(gen$,1%,9%)  = "PLAN UPMH"       /* Table File with the*/
            str(gen$,10%,4%) = sv_dept$  & "-"   /* Pcnt's for each Dpt*/
            str(gen$,14%,3%) = sv_month$ & "-"   /* By Month, Shift    */
        load_upmh_nxt
            read #3,key > gen$, using L60780, gen$, desc$,                ~
                                             eod goto load_upmh_done
L60780:        FMT CH(24), CH(6)
            if str(gen$,10%,3%) <> sv_dept$ then return
            if str(gen$,14%,2%) <> sv_month$ then return
            wt = 100.0                           /* Assume 100 Percent */
            convert str(desc$,1%,6%) to wt, data goto L60805
L60805:
            wt = round(wt/100.0, 4)              /* Calc Weighted UPMH */
            d_wt(k%,j%) = wt                     /* Save the Value for */
            j% = j% + 1%                         /* each Shift.        */
            if j% > shifts% then return          /* Note - Max Shifts  */
            goto load_upmh_nxt                   /*        is (3)      */
        load_upmh_done
        return

        load_store
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "WARRANTY"        /* (AWD025)*/

            read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60890
def_fmt:          FMT XX(08),  CH(08)

            warranty$ = str(def_desc$,1%,8%)
            convert warranty$ to warranty%, data goto L60875
L60875:
        return
L60890:     call "SHOSTAT" ("Error - Loading Warranty --> "&def_desc$)
            stop
        return

        update_store
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "WARRANTY"          /* (AWD025) */

            read #21,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60955

            convert warranty% to str(def_desc$,1%,8%), pic(00000000)
            put #21, using def_fmt1, def_desc$
def_fmt1:         FMT POS(09), CH(56)
            rewrite #21
        return
L60955:     call "SHOSTAT" ("Error - Updating Warranty --> "&def_desc$)
            stop
        return
                                                      /*  (EWD010) - Begin  */
        load_store_dallas
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "DWARRANT"         /*(AWD025) */

            read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60970

            convert str(def_desc$,2%,7%) to warranty%, data goto L60965
L60965
        return
L60970:     call "SHOSTAT" ("Error - Loading Dallas Warranty --> "&def_desc$)
            stop
        return

        update_store_dallas
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "DWARRANT"               /* (AWD025) */

            read #21,hold,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60975

            convert warranty% to str(def_desc$,2%,7%), pic(0000000)
            put #21, using def_fmt1, def_desc$

            rewrite #21
        return
L60975:     call "SHOSTAT" ("Error - Updating Dallas Warranty --> "&def_desc$)
            stop
        return
                                                      /*  (EWD010) - End  */

                                                      /*  (EWD025) - Begin  */
        load_store_stock
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "SWARRANT"

            read #21,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60980

            convert str(def_desc$,2%,7%) to swarranty%, data goto L60985
L60985
        return
L60980:     call "SHOSTAT" ("Error - Loading Stock Warranty --> "&def_desc$)
            stop
        return

        update_store_stock
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "SWARRANT"

            read #21,hold,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60990

            convert swarranty% to str(def_desc$,2%,7%), pic(0000000)
            put #21, using def_fmt1, def_desc$

            rewrite #21
        return
L60990:     call "SHOSTAT" ("Error - Updating Stock Warranty --> "&def_desc$)
            stop
        return
                                                      /*  (EWD025) - End  */
        REM *************************************************************~
            *           P l a n n i n g   A n a l y s i s               *~
            *************************************************************

        special_display
            spc$ = "S.O. Line Items Planned:[XXXXX] Skipped:[XXX]"
            convert plan_line% to str(spc$,26%,5%), pic(#####)

            convert plan_sk% to str(spc$,42%,3%), pic(###)

            print at(03,17);hex(84);spc$

        return

        plan_analysis
           pass% = 0%                                  /* (AWD016) */
           timeord$ = time                             /* CR1726 */

           plan$ = "(3)-Planning Analysis of Loads"
                                                    /*  (EWD010)       */
           analysis%, dallas% = 0%                  /* Analyize Only   */
REM             gosub oracle_connect                   /*  (EWD014)       */
             if keyhit% <> 30% then goto L61090        /* (AWD021) */
                gosub analysis_prompt               /* Analysis/Update */
                if comp% = 0% then gosub Plng_Reset  /* if abort, reset flag  */
                if comp% = 0% then goto L61295
                   analysis% = 1%
                   plan$ = "(3)-Planning Analysis Update!"

L61090:    gosub load_capacity

           goto plan_analysis_first                     /* (AWD018) */
        plan_not_temp                                   /* (AWD018) */
REM this is now used to plan pre-planned flex items 267 on 413 for example
           pass% = 1%                                   /* (AWD018) */
           goto plan_analysis_first                     /* (AWD028) */
        plan_rest                                       /* (AWD028) */
           pass% = 2%                                   /* (AWD028) */
plan_analysis_first                                     /* (AWD018) */
           call "SHOSTAT" (plan$)
           if pass% = 0% then plan_line% = 0%           /* (AWD028) */
           if pass% = 0% then plan_sk% = 0%
           init(" ") ld_key$, b_dte$, e_dte$
           b_dte$ = bg_day_dte$     : e_dte$ = ed_day_dte$
           call "DATUFMTC" (b_dte$) : call "DATUFMTC" (e_dte$) 
           str(ld_key$,1%,8%) = b_dte$           /* (APCPLNLD) - File  */
*       RHH
*       RHH  CALL "SHOSTAT" (" (6) - Begin Load Analysis")
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
        plan_next                  /*  (EWD010) - Add Sister Code in Read */
           read #4,key 1% > ld_key$, using L61170, ld_key$, ld_status$,   ~
                                              ld_day$, ld_sister$,        ~
                                                         eod goto plan_done
L61170:       FMT POS(3), CH(13), POS(94), CH(2), POS(108), CH(1), CH(2)
           if str(ld_key$,1%,8%) > e_dte$ then goto plan_done
        REM   Check all Loads for Un-Planned Product Until Staged
              if ld_status$ > "13" then goto plan_next
              load_flg% = 0%
              if ld_status$ <> "02" then load_flg% = 1% /* Prv Planned */
              ld_load$ = str(ld_key$,9%,5%)
              if ld_sister$ <> "02" then goto L61175  /*  (EWD010) - Beg */
                 dallas% = 1%
                 gosub load_store_dallas            /*  (EWD010) - End */

L61175:          gosub plan_load                    /* When Load Complete */
              if analysis% = 0% then goto plan_next
                                                     /*  (AWD018)        */
                                                     /*  (AWD018)        */
                 gosub dataput_ld_or             /* Set Production Date*/
REM                 IF PASS% = 1% THEN GOSUB DATAPUT_LD_OR
                                                     /*  (AWD018)   */

                                                 /* and Update S.O.    */
                                                 /* Line Items         */
           goto plan_next
        plan_done
*       RHH
*       RHH  CALL "SHOSTAT" ("(12a) Finished Planning Loads")
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
           if pass% = 0% then goto plan_not_temp  /* (AWD016) */
           if pass% = 1% then goto plan_rest      /* (AWD028) */
           gosub build_display                   /* Build Display      */
           gosub print_report
           if analysis% = 0% then goto L61290
            if dallas% = 0% then goto L61230     /*  (EWD010) - Beg  */
               gosub update_store_dallas
               gosub update_store_stock          /* (AWD025)   */
               goto L61235                       /*  (EWD010) - End  */
L61230:    gosub update_store                 /* Re-Set Warranty No.*/
           gosub update_store_stock           /* (AWD025) */
L61235:    gosub update_capacity              /* Update (APCPLNUC)  */
L61290:    gosub display_mode
L61295: return clear all
        goto inputmode

        plan_load                                /* (APCPLNSC) - File  */
           init(" ") sc_key$, sc_sav$
           sc_tqty% = 0% : sc_mqty% = 0% : sc_pqty% = 0% : sc_pqty1% = 0%
           sc_price = 0.0 : pln_qty% = 0%
           plan_load% = 0%
           str(sc_key$,1%,5%) = ld_load$
REM        GOTO PLAN_LD_NXT                             /* (AWD016) */
REM     PLAN_NOT_TEMP                                   /* (AWD016) */
REM        PASS% = 1%                                   /* (AWD016) */
           init(" ") sc_key$, sc_sav$
           str(sc_key$,1%,5%) = ld_load$
*       RHH
*       RHH  CALL "SHOSTAT" ("(7)-Begin Planning Load-"&LD_LOAD$)
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
        plan_ld_nxt
           read #5,key 1% > sc_key$, using L61385, sc_key$, dt_part$,     ~
                            sc_tqty%,sc_mqty%, sc_pqty%, sc_pqty1%,      ~
                            sc_price, sc_st$, sc_special$, eod goto plan_ld_done
L61385:       FMT POS(7), CH(27), CH(25), POS(68), 4*BI(2), PD(14,4),    ~
                  POS(110), CH(2), POS(118), CH(10)     /* (AWD016) */
        REM - Check for Special Sales Orders
              gosub check_specials

           pln_qty% = sc_mqty%                /* Set Planning Quantity */
           if str(sc_key$,1%,5%) <> ld_load$ then goto plan_ld_done
           if sc_st$ <> "02" then goto plan_ld_nxt /* Only Assigned SO */
REM              IF PASS% = 0% AND STR(SC_SPECIAL$,1%,1%) <> "Y"           ~
                       THEN GOTO PLAN_LD_NXT   /* (AWD016) */
                                               /* (AWD017) */
                                               /* (AWD018) */

/* (AWD029) begin */
              init(" ") so_inv$, item_no$, casing$, sdl$
              str(so_inv$,1%,8%)  = str(sc_key$,18%,8%)
              str(item_no$,1,2%)  = str(sc_key$,26%,2%)
              gosub lookup_sub_part          /* (AWD026)  */
              casing$ = str(dt_sub_part$,6,1)
              sdl$    = str(dt_sub_part$,8,1)
/* (AWD029) end   */

/* (AWD035) */
              sc_part$ = dt_part$   /* Part Number  */
/* (AWD028) */
              gosub check_specials_first_pass
              if pass% = 0% and first_special% = 1% then goto plan_ld
              if pass% <> 0% and first_special% = 1% then goto plan_ld_nxt

              gosub check_sd
              if pass% = 1% and sd% = 1% then goto plan_ld
              if pass% <> 1% and sd% = 1% then goto plan_ld_nxt

              if pass% = 2% then goto plan_ld

                       goto plan_ld_nxt

plan_ld:

/* (AWD028) */
              plan_line% = plan_line% + 1%
              plan_load% = plan_load% + 1%
              if mod(plan_line%,10%) <> 0 then goto L61450
                 gosub special_display

L61450:       if analysis% = 0% then goto L61535
                 get #5, using L61460, sc_rec$
L61460:            FMT CH(128)
                 dt_special$ = str(sc_rec$,118%,10%)   /* Special Flags*/
                 dt_part$    = str(sc_rec$,34%,25%)    /* Part Number  */
                 convert sc_tqty% to dt_qty$, pic(0000)/* Last Barcode */
                                                       /* Field (4)    */
                 load_group$ = str(sc_rec$,7%,17%)     /* Load,Drop Seq*/
                                                       /* Drop, CusSort*/
                 dt_ln_item$ = str(sc_rec$,32%,2%)     /* S.O. Ln Item */
                 dt_so_line$ = str(sc_rec$,24%,10%)    /* Barcode 1 & 2*/
                 dt_cust$    = str(sc_rec$,59%,9%)     /* Customer     */
                 dt_txt$     = str(sc_rec$,100%,4%)    /* Line Item Txt*/
                 dt_sale     = sc_price
                 if sc_tqty% <> 0% then       /* Convert to Unit Price */~
                                 dt_sale  = round(sc_price / sc_tqty%, 2)
                 dt_group$   = str(sc_rec$,128%,1%)    /* (EWD002) save*/
                                                 /* Wood S/F Group Code*/
                 gosub lookup_private_label      /* (EWD004)           */

L61535:       sc_sav$ = str(sc_key$,18%,10%)     /* S.O. and Line Item */
              gosub plan_order                   /* Schedule Line Item */
              if plan_dept% <> 0% then goto L61580
                 plan_sk% = plan_sk% + 1%
                 if plan_sk% > 100% then plan_sk% = 100%
                 str(sk$(plan_sk%),1%,5%)   = ld_load$   /* Load No.   */
                 str(sk$(plan_sk%),6%,10%)  = sc_sav$    /* S.O.-LItem */
                 str(sk$(plan_sk%),16%,25%) = dt_part$   /* Part No.   */
                 str(sk$(plan_sk%),41%,22%) = sd_key$    /*Dept/Prc/Shf*/
L61580:       gosub sched_pulls
              if analysis% = 1% then gosub dataput_sc
              if analysis% = 1% then gosub update_or         /*  (AWD019) */
              goto plan_ld_nxt
        plan_ld_done
*       RHH
*       RHH  CALL "SHOSTAT" ("(12)-Finished Planning Load-"&LD_LOAD$)
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH

REM           IF PASS% = 0% THEN GOTO PLAN_NOT_TEMP  /* (AWD016) */

           for ld% = 1% to ld_max%
               if ld_load$ = str(ll$(ld%),1%,5%) then goto L61650
           next ld%                              /* Update the Comp Dte*/
                                                 /* LD_DAY% includes   */
                                                 /* Factor for Departm */
L61650:    convert ld_day% to str(ll$(ld%),8%,2%), pic(00)

           rh2% = ld_day% + 1%                   /* Compute the Load   */
        REM   IF RH2% > 7% THEN RH2% = 7%        /* Date for Shipping  */
           convert rh2% to str(ll$(ld%),10%,2%),pic(00) /* Load Day    */

        return

        plan_order                                /* (APCPLNSD) - File */
           init(" ") sd_key$, sv_dept$, sd$(), sav_sd_key$
           mat sdu = zer : mat sds% = zer
           mat act_pln% = zer                     /* (AWD033) */
           how_pln% = 0%                          /* (AWD033) */
           plan_dept% = 0%
           j% = 0%                                /* Process all Rec's */

           dim sav_day$1
           init(" ") sav_day$                     /* (AWD023) savday   */
           str(sd_key$,1%,10%) = sc_sav$          /* for a S.O, Line   */
*       RHH
*       RHH  CALL "SHOSTAT" ("(8)-Plan S.O. Line Item-"&SC_SAV$)
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
        plan_ord_nxt                              /* Item No.          */
           read #6,key > sd_key$, using L61760, sd_key$, sd_unts, sd_seq%,~
                                               eod goto plan_ord_done
L61760:       FMT CH(23), PD(14,4), BI(1)

                                          /* SD key 1,10 = SO and Line */
           if str(sd_key$,1%,10%) <> sc_sav$ then goto plan_ord_done
        plan_ord_dept
                                         /* First time through set sv_dept */
              if j% = 0% then sv_dept$ = str(sd_key$,12%,3%)
              if sv_dept$ <> str(sd_key$,12%,3%) then goto plan_ord_line
                 j% = j% + 1% : if j% > dtl_max% then j% = dtl_max%
                 sd$(j%)  = str(sd_key$,12%,10%)  /* Load Dept, Process*/
                                                  /* Shift, and Model  */
                 sdu(j%)  = sd_unts               /* (APCPLNDP) UPMH   */
                 sds%(j%) = sd_seq%               /* And Product Sort  */
                 sav_sd_key$ = sd_key$            /* (AWD023) */
                 goto plan_ord_nxt
        plan_ord_line
              gosub sched_order                   /* Process the Curr  */
REM IF STR(SD_KEY$,23%,1%) = "0" THEN SAV_DAY$ = STR(DPT$(K%),4%,1%)

/* 0 = Primary Dept ! = 267 moved to dept 049  */
              if str(sav_sd_key$,11%,1%) = "0" then             ~
                        sav_day$ = str(dpt$(k%),4%,1%)
              if str(sav_sd_key$,11%,1%) = "!" then             ~
                        sav_day$ = str(dpt$(k%),4%,1%)

              init(" ") sd$()                     /* 'Department' then */
              mat sdu = zer : mat sds% = zer      /* Process Another   */
              goto plan_ord_dept
        plan_ord_done                             /* Just a Single Line*/
           if j% <> 0% then gosub sched_order     /* Item is Complete  */
        return                                    /* Finish Last Dept  */

        sched_order
*       RHH
*       RHH  CALL "SHOSTAT" ("(9)-Sched Line Item to Dept-"&SV_DEPT$)
*       RHH  STOP
*       RHH  CLOSE WS
*       RHH
           plan_dept% = plan_dept% + 1%     /* Plan at least (1) Dept  */
           p_max% = j%                      /* Max. No. of Process and */
           j% = 0%                          /* Shifts Per Department   */
           if pln_qty% = 0% then return     /* No Makes for Line Item  */
        sched_dept
           for i% = 1% to dpt_max%          /* Locate the Sub-Script   */
               if str(d_dp$(i%),1%,3%) = sv_dept$ then goto L61950
                                            /* Department Found        */
           next i%
           goto L62115                       /* Department Not Found,   */
                                            /* No Capacities Defined   */

L61950:    dpt% = i%                        /* Set Depart Sub-Script   */
           for qty% = 1% to pln_qty%        /* Sched Each Window       */
                                            /* Dept 'Start' Sub Script */
                                            /* To Dept 'End' Sub Script*/
                                            /* for all Dept's Assoc.   */
                                            /* with Product.           */
              day% = 0%                     /* (AWD033)  */

              for k% = d_sb%(dpt%,1%) to d_sb%(dpt%,3%)
                                            /* See If Capacity Exists  */
                  day% = day% + 1%
                  if cap(k%) <= 0.0 then goto L62110
                                            /* Insert Code for Plan    */
                                            /* Day analysis (LD_DAY$)  */
                  if str(dpt$(k%),4%,1%) < ld_day$ then goto L62110

                  rh1 = 0.0                 /* Find Applicable UPMH for*/
                  for l% = 1% to p_max%     /* Proper Process/Shift    */
                      if str(sd$(l%),4%,4%) <> str(dpt$(k%),5%,4%) then  ~
                                           goto L62105 /* Check Next one*/
                         wt = round(d_wt(dpt%,l%) * sdu(l%), 4)
                         dt_upmh = wt       /* Unit per Manhour Used   */
                                            /* Chk Total First QTY%= 1%*/
                         rh1 = 0.0
                         if qty% > 1% then goto L62075
                            rh1 = (pln_qty% * wt)
                                                     /* (AWD023) -     */
                                                     /* force prod day */
                            init(" ") cmg$

                            cmg$ = dpt$(k%)

/* (AWD033) force support to be planned as primary */
/* either by piece or total line                   */
                            if str(sav_sd_key$,11%,1%) = "1" and ~
                                 how_pln% = 1% then goto L62075

                            left% = 0%
                            left% = act_pln%(day%) - 1%
                            if str(sav_sd_key$,11%,1%) = "1" and         ~
                                  left% < 0% then goto L62110

                            if str(sav_sd_key$,11%,1%) = "1" and         ~
                                      (str(dpt$(k%),4%,1%) < sav_day$ )  ~
                                      and how_pln% = 0%                  ~
                                                            then goto L62110

                            if (cap(k%) - rh1) > 0.0 then goto L62130
                                            /* Continue in One's Loop  */

L62075:                     rh1 = 0.0
                                                     /* (AWD023) -     */
                                                     /* force prod day */

                            init(" ") cmg$

                            cmg$ = dpt$(k%)

                            left% = 0%
                            left% = act_pln%(day%) - 1%
                            if str(sav_sd_key$,11%,1%) = "1" and         ~
                                  left% < 0% then goto L62110

                            if str(sav_sd_key$,11%,1%) = "1" and         ~
                                      (str(dpt$(k%),4%,1%) < sav_day$ )  ~
                                      and how_pln% = 0%                  ~
                                                       then goto L62110

                            init(" ") cmg$

                            convert cap(k%) to cmg$, pic(########.#####-)

                            if (cap(k%) - wt) > 0.0 then goto L62130
                                            /* Found but Bucket Full   */
                               cap(k%) = 0.0       /* Bucket is Full   */
                               d_sb%(dpt%,2%) = d_sb%(dpt%,2%) + 1%
                               goto L62110
L62105:           next l%                     /* Start Department Over */
L62110:       next k%                         /* Note- QTY% Intact     */
L62115: REM - If You Reach here an Error Occurred   /* No Bucket Found */
              gosub set_overflow                    /* for Product     */
              goto sched_dept                       /* ??????????????? */
L62130: REM - Process Hit Available Production Found
              gosub process_hit
        REM - For Analysis Update Now Create (APCPLNDT) Record
              if analysis% = 1% then gosub dataput_dt
              if tqty% <> 1% then return      /* Finished with a Depart*/
           next qty%
        return

        process_hit
           tqty% = 1%
           if rh1 < .001 then goto L62195      /* Process One (1) Window */
              tqty% = pln_qty%
              wt    = rh1
L62195:    ld_day% = 1%                       /* Need for Completion Dte*/
           convert str(dpt$(k%),4%,1%) to ld_day%, data goto L62205
L62205:
           factor% = d_sb%(dpt%,4%)           /* Factor for Department */
           ld_day% = (ld_day% + factor%)


REM (AWD033) means primary dept
REM          planned one window at a time
REM 0 = Primary Dept ! = 267 moved to dept 049
            if str(sav_sd_key$,11%,1%) = "0" and rh1 < .001  ~
                          then how_pln% = 1%

            if str(sav_sd_key$,11%,1%) = "!" and rh1 < .001  ~
                          then how_pln% = 1%
REM (AWD033) try to keep track of number of units planned by day if
REM          primary dept
REM 0 = primary dept ! = 267 moved to dept 049
            if str(sav_sd_key$,11%,1%) = "0" then      ~
               act_pln%(ld_day%) = act_pln%(ld_day%) + 1%

            if str(sav_sd_key$,11%,1%) = "!" then      ~
               act_pln%(ld_day%) = act_pln%(ld_day%) + 1%

            if str(sav_sd_key$,11%,1%) = "1" then      ~
                act_pln%(ld_day%) = act_pln%(ld_day%) - 1%

        REM - Set WT to Zero (0.0) When there is a Special UPS S.O.
            if ups_flag% <> 0% then goto L62245 /* Do Not Deduct        */
        REM
           cap(k%)  = round( cap(k%) - wt, 4)   /* Deduct from Bucket  */
        REM
L62245:    unt%(k%) = unt%(k%) + tqty%          /* Add to Daily Qty's  */
           gosub calc_eff                       /*  (EWD013)           */
           ef_unt%(k%) = round(ef_unt%(k%) + ef_unit,0)
                                                /*  (EWD013)           */

           for pp% = 1% to 19%                  /* See if Model Exists */
               convert pp$(k%,pp%) to rhh%, data goto L62270

               goto L62280
L62270:           pp$(k%,pp%) = str(sd$(l%),8%,3%) /* Store Model if   */
                  goto L62305                       /* Product Bucket   */
L62280:        if pp$(k%,pp%) = str(sd$(l%),8%,3%) then goto L62305
           next pp%
           pp% = 20%                          /* The Other Bucket,After*/
           pp$(k%,pp%) = "999"                /* (999) = Misc Bucket   */

L62305:    pu%(k%,pp%) = pu%(k%,pp%) + tqty%  /* 1st (19) Models       */

           if ld_max% = 0% then goto L62335
           for ld% = 1% to ld_max%                    /* Start Day Set */
               if ld_load$ = str(ll$(ld%),1%,5%) then goto L62365
           next ld%
L62335:    ld_max% = ld_max% + 1%
           if ld_max% > load% then ld_max% = load%
           ld% = ld_max%
           str(ll$(ld%),1%,5%) = ld_load$
           convert ld_day% to str(ll$(ld%),6%,2%),pic(00) /* Start Day */
                                                          /* for Load  */
L62365
*       RHH   CONVERT PP% TO ROY1$, PIC(###)
*       RHH   CONVERT LD_MAX% TO ROY2$, PIC(###)
*       RHH   CALL "SHOSTAT"                                            ~
*       RHH      ("DEPT= "&SV_DEPT$&"PROD= "&ROY1$&" LOADS= "&ROY2$)
*       RHH   STOP
*       RHH   CLOSE WS
                           /* Scheduled Dept 'Must' Equal Planned Dept */
               if sv_dept$ = str(dpt$(k%),1%,3%) then return
                  gosub debug_display

        return

        set_overflow                          /* Put Into Overflow     */
*       RHH
        call "SHOSTAT" ("(11)-Overflow="&SC_SAV$&" Dept="&SV_DEPT$)
        stop
*       RHH  CLOSE WS
*       RHH
            gosub write_overflow              /* (AWD024)              */
            sv_dept$ = "100"                  /* Department. No Bucket */
            p_max% = 1%                       /* Available             */
            str(sd$(1%),1%,7%) = "1000101"    /* Save Original Model   */
            sdu(1%) = .1                      /* Note Model Left as is */
            sds%(1%) = 0%                     /* No Sort Code          */
            pln_qty% = (pln_qty% - qty%) + 1%
        return
        write_overflow                              /* (AWD024) - BEG */
           init(" ") over_key$
           over_cnt% = 0%
           str(over_key$,1%,27%) = sc_key$
           read #20, hold, key = over_key$, eod goto not_overflow

                get #20, using L62505, over_cnt%
L62505:                FMT POS(31), BI(4)

                over_cnt% = over_cnt% + 1%

                delete #20

        not_overflow
           put #20, using L62510, sc_key$, sv_dept$, over_cnt%

L62510:                FMT CH(27), CH(03), BI(4)
           write #20
        return                                        /* (AWD024) - END */


        sched_pulls
            if sc_pqty% = 0% then goto L62520
REM               sv_dept$ = "102"
               pln_qty% = sc_pqty%
               goto L62540
L62520:     if sc_pqty1% = 0% then return
               sv_dept$ = "104"
               pln_qty% = sc_pqty1%

L62540:     p_max% = 1%                       /* Single Dept           */
            str(sd$(1%),1%,3%) = sv_dept$     /* Set Department        */
            str(sd$(1%),4%,4%) = "0101"       /* Det Proc, Shift       */
            str(sd$(1%),8%,3%) = str(dt_part$,1%,3%) /* Set Model Code */
            sdu(1%) = .1                      /* Note Model Left as is */
            sds%(1%) = 0%                     /* No Sort Code          */
            gosub sched_dept
        return

        analysis_prompt
           comp% = 2%
           hdr$ = "******* Planning Analysis Update *******"
           msg$(1%) = " - - - -  A r e   Y o u   S u r e  - - - - "
           msg$(2%) = " Press Any (PF) Key to Continue, or Press  "
           msg$(3%) = " <Return> to Abort Analysis Update?        "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                /* (EWD006)            */
        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                /* (EWD006)            */
        lookup_private_label                    /* (EWD004) - Begin    */
           init(" ") readkey$, dt_config$, dt_prv$, sku$
           str(readkey$,1%,16%) = str(dt_so_line$,1%,8%)
           dt_prv% = 0%
           convert str(dt_so_line$,9%,2%) to dt_prv%, data goto L62610

           convert dt_prv% to str(readkey$,17%,3%), pic(###)
           read #14,key = readkey$, using L62600, itemid$, unitid$,     ~
                                          dt_group$, dt_config$,        ~
                                          dt_prv$, sku$, eod goto L62610
                                                     /* (AWD031) */
                                                     /* (AWD034) */
L62600:       FMT POS(255), CH(10), CH(10), POS(279), CH(1), CH(2),     ~
                   CH(2), POS(290), CH(10)     /* CR 1934 */

/* (IM8017) */
                gosub checkskuTable
/* CR2756 eCat check */        
                gosub check_ecat_sku
                if ecat% = 1% then sku$ = " "                

L62610: return                                  /* (EWD004) - End      */
/* (IM8017) */
        checkskuTable
            init(" ") code$
            code$ = sku$
            tab% = 6%
            gosub check_code
            if code% = 1% then sku$ = " "
        return
/* (IM8017) */


        REM *************************************************************~
            * New eCat validate check     CR2756                        *~
            *************************************************************
            
        check_ecat_sku
               init(" ") bck_key$
               ecat% = 0%
               
               str(bck_key$,1%,9%)   = str(sc_rec$,59%,9%)
               str(bck_key$,10%,16%) = str(sc_rec$,24%,8%)
      
               read #29,key = bck_key$, using L62625, bck_user_entered$, ~
                                                  eod goto L62630
L62625:          FMT POS(836), CH(03)

                  if bck_user_entered$ = "ECT" then ecat% = 1%
L62630:
        return

        update_warranty_file                    /*  (EWD014)           */
                                                /*  Only Write One Record   */
             if str(bar_key$,1%,18%) = str(dt_rec$,24%,18%) then return

             init(" ")bar_key$
             str(bar_key$,1%,18%) = str(dt_rec$,24%,18%)

             read #18, hold, key = bar_key$, eod goto no_barcode
                  delete #18
        no_barcode

             put #18, using L62650, bar_key$, str(dt_rec$,96%,8%),      ~
                                       itemid$, unitid$

/* (AWD040) */

L62650:           FMT CH(18), CH(8), CH(10), CH(10)

             write #18
        return                                  /*   (EWD014)          */
        
/************************************************************************/
/* Load the Schedule Header trigger for ATLas extract later.  Only 1    */
/* record in the trigger file with 0 transmit status needed.  CR1726    */
/************************************************************************/
        load_pg_trigger
            init(" ") filetype$, transmit$, filetype2$, salesorder$, linenbr$, ~
                      pgdate$, time$, upddte$, updtime$, filler1$            
                                 
            str(tr_key$,1%,8%) = str(dt_key$,1%,8%)
            str(tr_key$,9%,2%) = str(dt_key$,9%,2%)

            read #26, key 2% >= tr_key$,   ~
                   using L62700, filetype$, transmit$, pgdate$, time$,       ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,      eod goto new_trigger

              goto trigger_process
               
            nxt_trigger   
               read #26, using L62700, filetype$, transmit$, pgdate$, time$, ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,      eod goto new_trigger
trigger_process:                     
                  if salesorder$ <> str(dt_key$,1%,8%) or      ~
                     linenbr$ <> str(dt_key$,9%,2%)  then goto new_trigger
                     
                  if transmit$ = "0"  then  goto trigger_done
               goto nxt_trigger      
                  
            new_trigger
               filetype$ = "SCHDHDR"
               transmit$ = "0"
               str(pgdate$,1%,6%) = date
               time$ = time
               filetype2$ = "SCHDHDR"
               salesorder$ = str(dt_key$,1%,8%)
               linenbr$  = str(dt_key$,9%,2%)
               upddte$ = " "
               updtime$ = " "
               str(filler1$,1%,2%) = "04"
               str(filler1$,3%,10%) = "APCPLN06"
               put #26, using L62700, filetype$, transmit$, pgdate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,      ~
                            updtime$, filler1$
            
L62700:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), ~
                           CH(3), CH(6), CH(6), CH(180)
                           
               write #26
               
        trigger_done
        return

/************************************************************************/
/* Load the Order Line trigger for ATLas extract later.  Only 1         */
/* record in the trigger file with 0 transmit status needed.  CR1726    */
/************************************************************************/
        load_pg_order_trigger
            init(" ") filetype$, transmit$, filetype2$, salesorder$, linenbr$, ~
                      pgdate$, time$, upddte$, updtime$, filler1$    
                      
            str(tr_key$,1%,8%) = str(dt_key$,1%,8%)
            str(tr_key$,9%,2%) = str(dt_key$,9%,2%)

            read #27, key 2% >= tr_key$,   ~
                   using L62705, filetype$, transmit$, pgdate$, time$,       ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,     eod goto new_or_trigger

              goto trigger_or_process
               
            nxt_or_trigger   
               read #27, using L62705, filetype$, transmit$, pgdate$, time$, ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,    eod goto new_or_trigger
trigger_or_process:                     
                  if salesorder$ <> str(dt_key$,1%,8%) or      ~
                     linenbr$ <> str(dt_key$,9%,2%)  then goto new_or_trigger
                     
                  if transmit$ = "0"  then  goto trigger_or_done
               goto nxt_or_trigger      
                               
            new_or_trigger
               filetype$ = "ORDERLINE"
               transmit$ = "0"
               str(pgdate$,1%,6%) = date
               time$ = timeord$                     /* set one time at start */
               filetype2$ = "ORDERLINE"
               salesorder$ = str(dt_key$,1%,8%)
               linenbr$  = str(dt_key$,9%,2%)
               upddte$ = " "
               updtime$ = " "
               str(filler1$,1%,2%) = "04"
               str(filler1$,3%,10%) = "APCPLN06"
               str(filler1$,13%,9%) = str(dt_rec$,124%,9%) 
               put #27, using L62705, filetype$, transmit$, pgdate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,      ~
                            updtime$, filler1$
            
L62705:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), ~
                           CH(3), CH(6), CH(6), CH(180)
                           
               write #27
               
        trigger_or_done
        return  

/************************************************************************/
/* Load the Order Attributes trigger for ATLas extract later.  Only 1   */
/* record in the trigger file with 0 transmit status needed.  CR1726    */
/************************************************************************/
        load_pg_attr_trigger
            init(" ") filetype$, transmit$, filetype2$, salesorder$, linenbr$, ~
                      pgdate$, time$, upddte$, updtime$, filler1$    
            
            str(tr_key$,1%,8%) = str(dt_key$,1%,8%)
            str(tr_key$,9%,2%) = str(dt_key$,9%,2%)

            read #28, key 2% >= tr_key$,   ~
                   using L62710, filetype$, transmit$, pgdate$, time$,       ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,   eod goto new_attr_trigger

              goto trigger_attr_process
               
            nxt_attr_trigger   
               read #28, using L62710, filetype$, transmit$, pgdate$, time$, ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, filler1$,   eod goto new_attr_trigger
trigger_attr_process:                     
                  if salesorder$ <> str(dt_key$,1%,8%) or      ~
                     linenbr$ <> str(dt_key$,9%,2%)  then goto new_attr_trigger
                     
                  if transmit$ = "0"  then  goto trigger_attr_done
               goto nxt_attr_trigger      
                  
            new_attr_trigger
               filetype$ = "ORDERATTR"
               transmit$ = "0"
               str(pgdate$,1%,6%) = date
               time$ = time
               filetype2$ = "ORDERATTR"
               salesorder$ = str(dt_key$,1%,8%)
               linenbr$  = str(dt_key$,9%,2%)
               upddte$ = " "
               updtime$ = " "
               str(filler1$,1%,2%) = "04"
               str(filler1$,3%,10%) = "APCPLN06"
               put #28, using L62710, filetype$, transmit$, pgdate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,      ~
                            updtime$, filler1$
            
L62710:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), ~
                           CH(3), CH(6), CH(6), CH(180)
                           
               write #28
               
        trigger_attr_done
        return  
        
        debug
        REM   RETURN
            call "APCPL1DB" (pln_max%,i%,k%, dpt$(i%), cap(i%), unt%(i%),~
                             str(d_dp$(k%),1%,3%), str(d_dp$(k%),4%,12%),~
                             d_sb%(k%,1%), d_sb%(k%,2%), d_sb%(k%,3%),   ~
                             d_sb%(k%,4%), d_wt(k%,1%), d_wt(k%,2%),     ~
                             d_wt(k%,3%), d_wt(k%,4%) )
        return

        debug1
        REM   RETURN
            call "APCPL2DB" (pln_max%,i%,k%, dpt$(i%), cap(i%), unt%(i%),~
                             str(d_dp$(k%),1%,3%), d_sb%(k%,1%),         ~
                             d_sb%(k%,3%), pp$(i%,j%), pu%(i%,j%) )
        return

        debug_display
            init(" ") wrk$, d1$, d2$, d3$, d4$, d5$, d6$, d7$, d8$
            d1$ = "(Error) - DPT% = XXX    K% = XXX"
            wrk$ = "Process Dept=XXX XXX"
L62775:     inpmessage$ = "PRESS RETURN TO CONTINUE?"
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            convert dpt% to str(d1$,18%,3%), pic(###)

            convert k% to str(d1$,30%,3%), pic(###)

            str(wrk$,14%,3%) = sv_dept$
            str(wrk$,18%,3%) = str(dpt$(k%),1%,3%)
            convert cap(k%) to d2$, pic(######.##-)

            convert unt%(k%) to d3$, pic(#########-)

            convert pu%(k%,pp%) to d4$, pic(#########-)

            convert ld_day% to d5$, pic(#########-)
            str(d6$,6%,5%) = ld_load$
            d7$ = sc_sav$

            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (04,25), fac(hex(94)), d1$                    , ch(32),~
                                                                         ~
               at (06,02), "Work Values =             :",                ~
               at (06,30), fac(hex(84)), wrk$                   , ch(25),~
                                                                         ~
               at (07,02), "Capacity Values =         :",                ~
               at (07,30), fac(hex(84)), d2$                    , ch(10),~
                                                                         ~
               at (08,02), "Unit Values =             :",                ~
               at (08,30), fac(hex(84)), d3$                    , ch(10),~
                                                                         ~
               at (09,02), "Product Unit Values =     :",                ~
               at (09,30), fac(hex(84)), d4$                    , ch(10),~
                                                                         ~
               at (10,02), "Production Load Day =     :",                ~
               at (10,30), fac(hex(84)), d5$                    , ch(10),~
                                                                         ~
               at (11,02), "Production Load =         :",                ~
               at (11,30), fac(hex(84)), d6$                    , ch(10),~
                                                                         ~
               at (12,02), "Sales Order / Line Item = :",                ~
               at (12,30), fac(hex(84)), d7$                    , ch(10),~
                                                                         ~
               at (15,20), fac(hex(84)), ss$(1%)                , ch(40),~
               at (16,20), fac(hex(84)), ss$(2%)                , ch(40),~
               at (17,20), fac(hex(84)), ss$(3%)                , ch(40),~
               at (18,20), fac(hex(84)), ss$(4%)                , ch(40),~
               at (19,20), fac(hex(84)), ss$(5%)                , ch(40),~
               at (20,20), fac(hex(84)), ss$(6%)                , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L63080
                  call "PRNTSCRN"
                  goto L62775

L63080:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        check_specials
            if sav_so$ = str(sc_key$,18%,8%) then return
               sav_so$ = str(sc_key$,18%,8%)
            init(" ") or_hows$               /* (EWD008) Added New Flag */
            ups_flag% = 0% : or_hows% = 30%
            read #9, key = sav_so$, using L63130, or_hows$,eod goto L63150
L63130:        FMT POS(92), CH(2)
            convert or_hows$ to or_hows%, data goto L63140
L63140:
            if str(hows$,or_hows%,1%) = "*" then ups_flag% = 1%
                                             /* (EWD008) Test New Flag  */
L63150: return
                                             /* EWD009)                 */
        check_special_shapes_stock
            sp_part$ = sav_part$

            call "EWDSHAPE" (shapes%, fram%, sp_part$, spec_part$, #3)

            dt_part$ = spec_part$

        return

                                                 /* (EWD009)           */
        calc_eff                                 /* (EWD013)           */
           ef_unit = 0.00
           ef_err% = 0%
           call "APCPLNEF" ( dt_part$,   /* Part Number                */~
                             tqty%,      /* Number of planning units IN*/~
                             ef_unit,    /* Number of effective unitOUT*/~
                             ef_err%,    /* Error Code              OUT*/~
                             #3)         /* FILE = (GENCODES)          */

           if ef_err% <> 0% then ef_unit = 0.00

        return                                   /* (EWD013)           */

        check_warranty_file                      /* (EWD014)           */
             init(" ") bar_key$, sav_so$, so_inv$
             read #18, hold, key > bar_key$, eod goto no_oracle_update

REM          GOSUB ORACLE_PROMPT
REM          IF COMP% = 0% THEN GOTO NO_ORACLE_UPDATE
REM                GOSUB ORACLE_CONNECT
REM                     IF OCI_ERR% >= 0% THEN GOTO L63250
REM                        GOSUB ORACLE_NO_CONNECT
REM                        RETURN
             goto L63250
        check_warranty_next
             read #18, hold, eod goto no_oracle_update

/* (AWD040) */
L63250:         get #18, using L62650, bar_key$, dt_ref$, itemid$, unitid$
                gosub update_oracle
                  delete #18

                  so_inv$ = str(bar_key$,1%,8%)

                  if str(bar_key$,1%,1%) <> "0" then goto check_warranty_next
                  gosub check_so_header
                  goto check_warranty_next
        no_oracle_update
        return

        check_so_header
           if sav_so$ = so_inv$ and sav_so$ <> " " then return
           sav_so$ = so_inv$
           init(" ") stmt1$
           gosub oracle_flush
           stmt1$ = "UPDATE MSSQL.ORDERMASTER SET STATE = 'prod', " &~
                    "DATESTATECHANGE = to_date('" & oradate$ & "', "&~
                    "'MM-DD-YYYY') WHERE SALESORDER = '" & so_inv$ & "'"

           gosub oracle_exec
        return



        oracle_prompt
           comp% = 2%
           hdr$ = "*******  ORACLE  Analysis Update *******"
           msg$(1%) = " - - - Do You Want To Update Oracle  - - - "
           msg$(2%) = " Press Any (PF) Key to Continue, or Press  "
           msg$(3%) = " <Return> to Abort Oracle Update?          "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        oracle_no_connect
           comp% = 2%
           hdr$ = "***************************************"
           msg$(1%) = " - - - You are NOT connect to Oracle - - - "
           msg$(2%) = "      Contact Systems For Support.         "
           msg$(3%) = "      Press <Return> to Exit??             "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return                                    /*  (EWD014)        */

        schema_error
           comp% = 2%
           hdr$ = "***************************************"
           msg$(1%) = " - - - Can not find user schema      - - - "
           msg$(2%) = "      Contact Systems For Support.         "
           msg$(3%) = "      Press <Return> to Exit??             "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return                            /* AWD025*/


        previous_plan_error                            /* (AWD016) */
           comp% = 2%
           hdr$ = "***************************************"
           msg$(1%) = "PLANNING ERROR " & str(dt_rec$,189%,25%)
           msg$(2%) = "Warr Num is already Assigned " & dt_ref$
           msg$(3%) = "Press <Return> to Exit?? " & str(dt_rec$,24%,18%)
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return                                    /*  (EWD014)        */

        check_specials_first_pass                 /*   (AWD017)       */
           first_special% = 1%

/* (AWD036) take out ultra, still lookup ultra so flag in APCPLNDT is set */
/* be sure to check ultra first so ultra flag is set if */
/* other specials exist like temp*/
           gosub lookup_ultra              /* (AWD035)              */
REM        IF ULTRA% = 1% THEN RETURN      /* PUT ULTRA IN FIRSTPASS*/
/* (AWD041) */
           gosub lookup_lamn
            if lamn% = 1% then return
/* (\AWD041) */
REM SC_SPECIAL$,1,1 = TEMPERED
           if str(sc_special$,1%,1%) = "Y" then return

/* (AWD036) move to after wood_sort flag check */
REM        IF CASING$ = "1" OR CASING$ = "2" THEN RETURN /* (AWD029) */
REM        IF SDL$ = "1" THEN RETURN                     /* (AWD029) */

           if wood_sort$ <> "Y" then goto not_wood

           if casing$ = "1" or casing$ = "2" then return /* (AWD029) */
           if sdl$ = "1" then return                     /* (AWD029) */
           if str(sc_special$,4%,1%) <> "Y" then goto not_wood
           init(" ") apcwood_code$
           if len(dt_part$) = 22% then apcwood_code$ = str(dt_part$,20%,3%) ~
                                  else apcwood_code$ = str(dt_part$,23%,3%)

           code$ = str(dt_part$,1%,3%)
           str(code$,4%,3%) = apcwood_code$
           tab%  = 5% : gosub check_code
           if code% = 0% then goto not_wood
           return

not_wood:
           first_special% = 0%
        return                                    /*  (AWD017)        */


        update_or                                    /* (AWD019)   */

               read #9,hold,key 4% = str(dt_so_line$,1%,8%), using L33015,   ~
                                            or_status$, eod goto update_or_error
L33015:           FMT POS(60), CH(2)
               if or_status$ > "03" then goto update_or_done
                  put #9,using L33045, "03", date
L33045:             FMT POS(60), CH(2), CH(6)
               rewrite #9

        update_or_done
        return
        update_or_error
             errormsg$="(3)Error-Updating S.O. Header's 'OR'-> "&dt_so_line$
             gosub error_prompt
        return                                    /* (AWD019)  */

/* (AWD026) -- begin */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(),           ~
                      dt_sub_part$, dt_sub_info$
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"
            err1% = 0%

            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)

order_converted:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:
            convert item_no% to item_no$, pic(###)

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */

                if err1% = 0% then goto set_sub_part
                   str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
                   str(bcksubpt_rec$,132%,20%) = "00000000000000000000"
           set_sub_part
               dt_sub_part$ = str(bcksubpt_rec$,48%,20%)

               dt_sub_info$ = str(bcksubpt_rec$,132%,20%)


            return

/* (AWD026) -- END */

/* (AWD028) -- begin */
        check_sd
           sd% = 0%
           init (" ") sd_key$
           str(sd_key$,1%,10%) = str(sc_key$,18%,10%) /* for a S.O, Line   */
                                                      /* Item No.          */
           read #6,key > sd_key$, using L63100, sd_key$,eod goto check_sd_done
L63100:       FMT CH(23)

                                          /* SD key 1,10 = SO and Line */
           if str(sd_key$,1%,10%) <> str(sc_key$,18%,10%)                   ~
                                                  then goto check_sd_done
           if str(sd_key$,11%,1%) <> "!" then goto check_sd_done
                  sd% = 1%
        check_sd_done
        return
/* (AWD028) -- End   */

/* (AWD035) */
        lookup_ultra
            init(" ") gen$, desc$
            ultra% = 0%
            str(gen$,1%,9%)  = "PLANULTRA"      /* ALL GLASS By Model */
            str(gen$,10%,5%) = str(sc_part$,1,3) & "**"
            read #3,key = gen$, eod goto lookup_ultra_gls
                      ultra% = 1%
              return
        lookup_ultra_gls
            str(gen$,1%,9%)  = "PLANULTRA"    /* Specific Glass */
            str(gen$,10%,5%) = str(sc_part$,1,3) & str(sc_part$,5,2)
            read #3,key = gen$, eod goto ultra_done
                      ultra% = 1%
        ultra_done
        return
/* (/AWD035) */
/* (AWD041) */
        lookup_lamn
            init(" ") gen$, desc$
            lamn% = 0%
            str(gen$,1%,9%)  = "PLAN LAMN"
            str(gen$,10%,2%) = str(sc_part$,5%,2%)
            read #3,key = gen$, eod goto lamn_done
                 lamn% = 1%
        lamn_done
        return
/* (/AWD041) */

/* (AWD037) beg */
        get_user_pswd
            call "READ100" (#23, "ORACLE PASSWORD", f1%(23%))   /* SYSFILE2 */
            if f1%(23%) <> 0% then get #23 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD037) END */

/*CR00671 + */
        lookup_intercept
          if str(dt_sub_part$,17%,1%) <> "0" then goto subpart_intercept

          init(" ") gen$, descr$, intercept$
          intercept% = 1%
          str(gen$,1,9)  = "INTERCEPT"
          str(gen$,10,3) = model$
          str(gen$,13,2) = ty$                      /* Glass Code */

           read #3, key = gen$, using L61131, descr$,  ~
                                   eod goto no_intercept_glass

               convert str(descr$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(gen$,1,9)  = "INTERCEPT"
          str(gen$,10,3) = model$
          str(gen$,13,2) = str(ty$,1,1) & "*"

           read #3, key = gen$, using L61131, descr$,  ~
                                   eod goto no_intercept_all

               convert str(descr$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") gen$
           str(gen$,1,9)  = "INTERCEPT"
           str(gen$,10,3) = model$
           str(gen$,13,2) = "**"

           read #3, key = gen$, using L61131, descr$,  ~
                                   eod goto intercept_done

               convert str(descr$,1,2) to intercept%, data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return
L61131:        FMT POS(25), CH(30)
        subpart_intercept
          convert str(dt_sub_part$,17%,1%) to intercept%, data goto badsubpart_intercept
          goto intercept_done
        badsubpart_intercept
         str(dt_sub_part$,17%,1%) = "0"
         goto lookup_intercept

/*CR00671 - */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
    
        exit_program

            call "SHOSTAT" ("Updating Database, Please Wait")
            gosub oracle_connect
               if oci_err% >= 0% then goto oracle_connected
                  gosub oracle_no_connect
                  end
oracle_connected:
            gosub check_warranty_file

            gosub update_apcplndt
REM         close #13
REM         call "FILEBGON" (#13)            /* Department Capacities   */
            call "SHOSTAT" ("One Moment Please")

            end



