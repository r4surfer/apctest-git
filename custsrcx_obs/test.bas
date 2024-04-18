        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLA45                             *~
            *  Creation Date     - 11/18/98                             *~
            *  Last Modified Date- 07/19/99                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program Creates and Updates     *~
            *                      the 'Glass Database' for the new     *~
            *                      Glass Remake System. This Utility    *~
            *                      also Explodes the Glass on the       *~
            *                      Screen.                              *~
            *                                                           *~
            *  File Naming - Description                                *~
            *  -----------   -----------------------------------------  *~
            *    Work        <APCPLNWK> - Used for Sorting Glass        *~
            *    Labels      MMDD@<Dpt> or <ALL> - Regular Labels       *~
            *                MMDD@<RMK> - For All Remake Labels         *~
            *                MMDD@<RM1> - For Remakes Productuin(EWD006)*~
            *                MMDD@<RM2> - For Remakes In-House  (EWD006)*~
            *                MMDD@<TMP> - Special and Tempered Glass    *~
            *                                                           *~
            *    Batches     @GLSGED@ = GED, @GLSBIL@ = Bilco - Regular *~
            *    (Both)      @RMKGDK@ = GED, @RMKBLK@ = Bilco - Remake  *~
            *    (Production)@RMKGD1@ = GED, @RMKBL1@ = Bilco - Remake  *~
            *    (In-House)  @RMKGD2@ = GED, @RMKBL2@ = Bilco - Remake  *~
            *                                                           *~
            *New(APCPLC45) - Subroutine for Creating Glass File Using   *~
            *   (APCGS5SB)   the GED Sort. The file is created in       *~
            *                the Bilco Glass Format and Processed by    *~
            *                the Bilco Glass Optimizer.                 *~
            *                                                           *~
            *New(APCPLB45) - Subroutine for Creating Glass File which   *~
            *   (APCGS4SB)   will be Processed by the GED Glass         *~
            *                Optimizer. All Quantities are One (1) in   *~
            *                Item Record.                               *~
            *New(APCPLD45) - Subroutine for Analyzing both Scheduled    *~
            *                and completed glass. Also checks Re-Make   *~
            *                Glass.                                     *~
            *                Item Record.                               *~
            *                                                           *~
            *New(EWDPLN72) - New Program and Subroutine for printing,   *~
            *                only Glass remake labels on the Zebra      *~
            *                printers. Uses subroutine (EWDPLA72) to    *~
            *                print labels.                              *~
            *                                                           *~ 
            * GED Glass Tables                                          *~
            *            (GED XXX  ) - GED Glass System Model Codes     *~
            *            (GED X0   ) - GED GLASS MODELS                 *~
            *                                                           *~
            *            (GED X1   ) - BILCO GLASS MODELS (GED SORT)    *~
            *                                                           *~
            *            (GED 000  ) - GED Spacer Id's Table            *~
            *                          Field #2 Header, No Decimal      *~
            *            (GED 001  ) - GED Glass Type Code Table        *~
            *                          (00-29) - Single Strength        *~
            *                          (50-79) - Double Strength        *~
            *                           Note - DS Pos (16) Double (Only)*~
            *            (GED 002  ) -  GED Glass Thickness & Spacer    *~
            *                           Pos ( 1 - 6) = Thickness        *~
            *                           Pos ( 9 - 6) = Single Strength  *~
            *                           Pos (17 - 6) = Double Strength  *~
                                        Note - Decimal Included         *~
            *            (GED 003  ) -  GED Glass Liting/Grid Cross Ref *~
            *                           Pos (1 - 10) Follwed by "-" and *~
            *                                    Long Description       *~
            *            (GED 004  ) - GED Width and Height Adjustment  *~
            *                          Pos (1 - 6) = Width Adjustment   *~
            *                          Pos (8 - 6) = Height Adjustment  *~
            *            (GED 005  ) - GED Glass Special Shape Codes    *~
            *                          Note - Default Code              *~
            *                           "1-STD" = Standard Rectangle    *~
            * System Glass Tables                                       *~
            *            (GLASS01  ) - GLASS QUANTITIES FOR TOP/BOT     *~
            *            (GLASS02  ) - TABLE FOR TOP GLASS ONLY         *~
            *            (GLASS03  ) - TABLE FOR TWO BOTTOMS            *~
            *            (GLASS04  ) - Table for no Glass Required      *~
            *            (GLASS05  ) - TABLE FOR '8' SERIES WITH GLASS  *~
            *  Not Used->(GLASS06  ) - TABLE TO SKIP NO DETAIL LABELS   *~
            *  Not Used->(GLASS07  ) - TABLE FOR SLIDERS                *~
            *            (GLASS08  ) - TABLE - SPECIAL SHAPES NO GLASS  *~
            *            (GLASS09  ) - TABLE - DEPT'S FOR BILCO MACHINE *~
            *            (GLASS10  ) - TABLE - CLMR Specified Top-Bot   *~
            *            (PLAN TEMP) - All Valid Temered Glass Codes    *~
            *            (PLAN DBLE) - All Double Strength Glass Codes  *~
            *            (PLANGLOUT) - Skip Glass, Purchased Outside    *~
            *                          for the specified 'LITING' Codes.*~
            *            (PLANARGON) - Check Table for Glass Codes with *~
            *                          Argon Gas. Table only has the    *~
            *                          Glass Codes with Argon Gas. (G1) *~
            *                          is the GED Code for Argon.       *~
            *            (PLAN REMK) - Reason Codes for Remake Glass    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *  Top Glass Type  Phantom      Bot Glass Type  Phantom     *~
            *  - Standard        2008       - Standard        3008      *~
            *  - Std Oriel       2203       - Std Oriel       3203      *~
            *  - Std Cottage     2103       - Std Cottage     3103      *~
            *  - Picture         2008                                   *~
            *  - H.S & Patio     2008       - H.S. Patio      3008      *~
            *                               - Hopper          3008      *~
            *  - 1/3-1/3-1/3     2108       - 1/3-1/3-1/3     3108      *~
            *  - 1/4-1/2-1/4     2008       - 1/4-1/2-1/4     3008      *~
            *  - Spec. Oriel     20xx       - Spec. Oriel     30xx      *~
            *  - Spec. Cottage   20xx       - Spec. Cottage   30xx      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/18/96 ! New Program for (APC) - LAST MOD DATE    ! RHH *~
            *          !   Contains all the Modification that are !     *~
            *          !   in APCGLS05, and the following         !     *~
            *          ! Special Mod to Create a New Sort order   !     *~
            *          ! Uses Priority Codes 1% to 50%. Dept,Glass!     *~
            *          ! Strength, and Spacer are the Elements.   !     *~
            * 10/06/96 ! New Planning Version of Glass            ! RHH *~
            * 01/03/97 ! Mode for UPS and Back Orders Modes to    ! RHH *~
            *          !   RM_NUM$. GOSUB LOOKUP_SO               !     *~
            * 01/28/97 ! Add new Barcode field to Label           ! JBF *~
            * 02/12/97 ! Mode for Change GLASS_DTE$ to Glass Prod ! RHH *~
            *          !   Date. Date Glass is Made and Completed !     *~
            * 04/08/97 ! Mods to change the sort for remake glass ! RHH *~
            *          !   Disabled Sort Code line (60240) also   !     *~
            *          !   Removed Seq No. from Sort 60256,60272  !     *~
            * 06/04/97 ! Mods to add two (2) new Departments to   ! RHH *~
            *          !   the sort. (049) and (052).             !     *~
            * 06/12/97 ! Mods to also change error code to '51'   ! RHH *~
            *          !   and "E" this affects both Subs.        !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 01/14/98 ! Mod to (APCPLB45) and (APCPLC45) to Add  ! RHH *~
            *          !   new Subroutine Called 'CHECK_GLASS'.   !     *~
            *          !   New Tables (PLANGLOUT) and (PLANARGON) !     *~
            *          !   LITING Codes and Glass Argon Gass Codes!     *~
            * 02/02/98 ! Mod to Skip Liting Code = 88% at         ! RHH *~
            *          !   BUILD_GED_KEY                          !     *~
            * 02/26/98 ! Mod to correct Label TEXT$() for Glass   ! RHH *~
            *          !   Label Printing.                        !     *~
            * 03/16/98 ! Mods to fix Sandwich problem in BILCO    ! RHH *~
            *          !   glass 'APCPLC45'. Also created new     !     *~
            *          !   report for Special Tempered Glass      !     *~
            * 03/24/98 ! Y2K                                      ! LDJ *~
            * 04/07/98 ! Correction made to GEN_RPT1 to fix Lookup! RHH *~
            *          !   must use read next logic because of Dup!     *~
            * 04/10/98 ! Mod to use New Table with the Constant   ! RHH *~
            *          !   values used to calculate the height    !     *~
            *          !   adjustment for Specified CLMR.         !     *~
            * 06/04/98 ! (EWD001) Mod to Split Remake Glass into  ! RHH *~
            *          !   Two (2) Groups. Remakes and Glass House!     *~
            * 06/18/98 ! (EWD002) Mod to Stock- Clear (01) Glass  ! RHH *~
            *          !   glass only. Type (01).                 !     *~
            * 07/27/98 ! (EWD003) Fix Special Liting Problem put  ! RHH *~
            *          !   liting codes between (83% - 88%) at the!     *~
            *          !   with sort code '049'. Also mods for    !     *~
            *          !   special Tempered Glass Report. Create  !     *~
            *          !   labels for Tempered Glass 'MMDD@TMP'   !     *~
            *          !   Mods to Sub (gen_rpt1).                !     *~
            * 10/16/98 ! (EWD004) Mod to create new sort sequence ! RHH *~
            * 12/07/98 ! (EWD005) Mods for Glass Re-make System   ! RHH *~       
            *          !   put (IND) on glass label for Indy's    !     *~       
            * 01/11/99 ! (EWD006) Differnt Label File Names       ! RHH *~       
            *          !   Mod to Purge to Save Stock             !     *~
            * 02/23/99 ! (EWD007) Mod to add New Departments to   ! RHH *~
            *          !   glass sort.                            !     *~
            * 03/25/99 ! (EWD008) Mod to add New Special Glass    ! RHH *~
            *          !   utility (EWDPLN66) to Eliminate Manual !     *~
            *          !   labels.                                !     *~
            * 04/06/99 ! (EWD009) Mod for new special Liting Code ! RHH *~
            *          !   assoc. with new products.  Temporary   !     *~
            *          !   put at the end. Code 'A0'              !     *~
            * 04/12/99 ! (EWD010) Mod to (APCPLD45) for new glass ! RHH *~
            *          !   oven tracking. Using Id (OV?)          !     *~
            * 04/27/99 ! (EWD011) Mod for Glass Code "E7"         ! RHH *~
            * 07/19/99 ! (EWD012) Mod to print Glass Re-Make Label! RHH *~
            *          !   on Zebra Printer. Use remake_label% to !     *~
            *          !   turn-off old label format.             !     *~              
            * 08/10/99 ! (EWD013) Mod to Correct problem with new ! RHH *~
            *          !   model (121). Problem is that no sort   !     *~
            *          !   code was found.                        !     *~              
            *************************************************************

        dim                              /* FILE = AMTBOMCD            */~
            view$3, mode$5,              /* TOP/BOT                    */~
            model$3,                     /* Special Glass Models       */~
            tab$(10%)9,                  /* Glass Table Names          */~
            mod$(10%,100%)3,             /* STORE MODELS ASSOC TABLES  */~
            mod%(10%),                   /* NO. ENTRIES EACH TABLE     */~
            ws$(100%)9,                  /* Stock Sizes Width          */~
            hs$(100%)8,                  /* Stock Sizes Height         */~
            stk_desc$30, stock$9,        /* Stock Description  W & H   */~
            desc$30,                     /* USED FOR SORT TEST         */~
            phantom$25                   /* Phantom Designator         */

        dim                              /* FILE = APCPLNDT            */~
            ld_key$5,                    /* LOAD NUMBER                */~
            save_part$25,                /* PART NUMBER                */~
            dt_rec$256,                  /* Detail Record              */~
            dt_key$23,                   /* Primary Key - Load Number  */~
            dt_key1$57,                  /* Alt - Bar Code Key         */~
            dt_load$5, rm_ky$33,         /* Production Load No         */~
            dt_bar$18, rm_bar$9,         /* Production Barcode         */~
            dt_date$6, rm_num$3,         /* Production Date Planned    */~
            dt_ref$8,  rm_key$33,        /* New Warranty Number        */~
            dt_shft$2, rm_st$1,          /* Production Shift Code      */~
            dt_seq$5,                    /* Department Sequence No.    */~
            dt_cust$9,                   /* Customer Code              */~
            dt_part$25, dt_desc$32,      /* Part Number                */~
            dt_sash$1, or_hows$2,        /* Sash 0=No,1=Top,2=Bot,3=FGO*/~
            dt_prt$1,                    /* Part (Y)es or (N)o         */~
            dt_samp$1,                   /* Sample 0=No,1=Samp,2-Disp  */~
            dt_wood$3,                   /* W/S 000=N/A,Code Value     */~
            dt_special$10,               /* Special Flags              */~
            dt_txt$4,text$(2%)70,txt$40, /* Detail Text Id             */~
            dt_gls$1,                    /* Glass Yes or No            */~
            hg$2,                        /* HINGE CODE                 */~
            hgl$15,                      /* Hinge Code Left Discript   */~
            hgr$15,                      /* Hinge Code Right Descript  */~
            ty$2,                        /* Glass Type Code            */~
            ty_s$3,                      /* Glass Short Description    */~
            ty_l$30,                     /* Glass Long Description     */~
            cl$1, cl_s$2, cl_l$6,        /* Glass Code                 */~
            lt$2,                        /* Liting Code                */~
            l_lt$6,                      /* Liting Left Descr          */~
            r_lt$6,                      /* Liting Right Descr         */~
            sc$1,                        /* Screen Code                */~
            lk$1                         /* Lock Code                  */

        dim                              /* FILE - (APCPLNWK)          */~
            lab_ged$66,                  /* GED Primary Key            */~
            lab_rec$190,                 /* Label Rec Alt Key 1 (1-38) */~
            lab_fil$9,                   /* Label Filler Area          */~
            wd$7,                        /* Actual Width               */~
            ht$6,                        /* Actual Height              */~
            filler$24                    /* RECORD FILLER              */

        dim                              /* FILE - LABEL PRINT FILES   */~
            ff$8,                        /* Label Print File Name      */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6,                    /* Volume Name = 'CARLOS'     */~
            a1$2, a2$7, a3$8, a4$10,     /*                            */~
            a5$3, a6$8, a7$10, a8$2,     /*                            */~
            a9$5, a10$16, a11$3, a12$3,  /*                            */~
            a13$40, a14$9,  a15$3, a16$6,/*                            */~
            a17$1                        /*                            */

                                         /* (EWD012)                   */ 
        dim gl_key$14,                   /* Glass re-make Label Databas*/~
            gl_sort$5,                   /* Glass Sort Code            */~
            gl_run$3,                    /* Glass Run Number for Day   */~
            gl_date$10                   /* Formatted Production Date  */
                                         /* (EWD012)                   */
 
        dim                              /* File - (APCPLNGR)          */~
            rm_dept$3, rm_seq$5,         /* Glass Remake Reports       */~
            rm_model$3, rm_load$5,       /*                            */~
            rm_gls$2, rm_mut$8,          /*                            */~
            rm_wd$7, rm_ht$6, rm_so$8,   /*                            */~
            rm_reason$2, rm_tot$10,      /*                            */~
            rm_reason_d$30,              /*                            */~
            title$30, company$25,        /*                            */~
            rpt_time$8, rm_cnt$37,       /*                            */~
            rm_rec$256                   /*                            */

        dim                              /* (Program Variables)        */~
            size$4,                      /* Batch File Size            */~
            bg_date$10, bg_dte$10,       /* Beginning Prod Date    (Y2K, LDJ)*/~
            ed_date$10, ed_dte$10,       /* Ending Production Date (Y2K, LDJ)*/~
            rp_sel$1, rp_sel_d$30,       /* Glass Report Selections    */~
            sav_mod$3,                   /* Product Line Model         */~
            glass_dte$8,                 /* Glass Completion Date Form */~
            glass_dte1$8,                /* Glass Completion Date Unfor*/~
            sze$30,                      /* Save Eights                */~
            sz$100,                      /* Save Sixteenths            */~
            wd1$9,                       /* Calculated Width           */~
            wd2$9,                       /* CLMR FOR SCREEN            */~
            ht1$8,                       /* Calculated Height          */~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            scr$(10%)40, rpt$(10%)40,    /* Screen Text Messages       */~
            scr_dte$8,                   /* Glass Production Date FORM */~
            scr_dte1$8,                  /* Glass Prod. Date Unform    */~
            dd$9, dd1$3, num$,           /* Label Day of Week          */~
            scr_sel$1,                   /* Glass Process Selection    */~
            scr_dept$3,                  /* Glass Department Selection */~
            scr_shft$2, scr_shft_d$30,   /* Glass Department Selection */~
            scr_msg$30,                  /* Screen - Report Selection  */~
            scr_msg1$30,                 /* Screen - Product Line      */~
            scr_load$5, scr_desc$30,     /* Screen - Load Number       */~
            descr$30,   stk_so$8,        /* Use for GENCODES Look-Up   */~
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

        dim f2%(25%),                    /* = 0 if the file is open    */~
            f1%(25%),                    /* = 1 if READ was successful */~
            fs%(25%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(25%)20                 /* Text from file opening     */

        dim                              /* FILE - @GLSGED@ - File     */~
            hdr$40,                      /* Askuser Header             */~
            msg$(3%)79,                  /* Askuser Messages           */~
            file$20, bat$3,              /* Name of Batch file         */~
            cut$(10%)55,                 /* TOP A AN B, BOT A AN B     */~
            txt$(4%)50,                  /* Screen '102 Header Text    */~
            tty$2,                       /* GED Glass Type Code        */~
            temp$1,skip$1,               /* Blank No, * = Yes          */~
            sandwich$10,                 /* Glass Sandwich             */~
            spacer$6,                    /* Sort Spacer Thickness      */~
            space_d$10,                  /* Spacer Id                  */~
            t_k$6,                       /* Thickness                  */~
            s_s$6,                       /* Single Strength Spacer     */~
            s_d$6,                       /* Double Strength Spacer     */~
            w_adj$6, h_adj$6,            /* Width and Height Adjustment*/~
            muttin$8, lits$1, mut$8,     /* Muttin Code Vert/Horiz     */~
            ged_cnt$5,                   /* Counter for Batches        */~
            t_err$(10%)25,               /* Error Text Message         */~
            so$8, ged$(10%)20,           /* Sales Order Number         */~
            width_d$8,                   /* Save Width Calc in Decimal */~
            height_d$8,                  /* Save Height Calc Decimal   */~
            ged_bilco$1,ged_desc$20,     /* 0 = GED, 1 = Bilco         */~
            g_b$1, pg_dte$6, pg_dte1$6,  /* 0 = GED ONLY,1 = BILCO ONLY*/~
            pg_dte2$6, rm_flag$1,        /* Remake Purge Date          */~
            ss$(60%)11, ss$2, xx$2,      /* Sort Code Array            */~
            sort$11                      /* Used for Sorting           */

        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run (EWD007)    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$32
            apc$   = "(New)Planning Glass Processing Utility   "
            pname$ = "APCPLA45 - Rev: R7.00 - 12/09/98"

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
            * #1  ! AMTBOMCD ! Master Equation File                     *~
            * #2  ! APCPLNDT ! Master Planning Detail File (APCPIECE)   *~
            * #3  ! GENCODES ! Master System Table File                 *~
            * #4  ! APCEQUAT ! Equation and Part Cross Reference File   *~
            * #5  ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #6  ! APCPLNWK ! Glass Work File (Labels)                 *~
            * #7  ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * #8  ! APCPLNGR ! Glass Sched/Remake File                  *~
            * #9  ! TEXTFILE ! MASTER TEXT FILE                         *~
            * #10 ! @GLSGED@ ! Glass Batch File for GED Glass System    *~
            * #11 ! @GLSBIL@ ! Glass Batch File for Bilco Glass System  *~
            * #12 ! @RMKGD1@ ! Remake Batch File for GED Glass Production*~
            * #13 ! @RMKBL1@ ! Remake Batch File for Bilco Glass Production*~
            * #14 ! APCPLNOR ! Planning S.O. Header History             *~
            * #15 ! MMDD@DPT ! Glass Label Print File                   *~
            * #16 ! @RMKGD2@ ! Remake Batch GED Glass House (EWD001)    *~
            * #17 ! @RMKBL2@ ! Remake Batch Bilco Glass House (EWD001)  *~
            * #18 ! APCPLNDP ! Planning Master Dept File      (EWD003)  *~ 
            * #19 ! BCKLINES ! S.O. Line Item Detail Information        *~
            * #20 ! APCPLNSA ! sales Analysis Control File              *~
            * #21 ! @RMKGDK@ ! Remake Batch File for GED Glass Both     *~
            * #22 ! @RMKBLK@ ! Remake Batch File for Bilco Glass Both   *~
            * #25 ! EWDGLSLB ! Remake Label Database            (EWD012)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #2,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos  =  53, keylen =  51,         ~
                            key  3, keypos  =   1, keylen =  23, dup,    ~
                            key  4, keypos  =  96, keylen =   8, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4,  "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #5,   "APCPLNLD",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   11, keylen =    5,                    ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos  =   1, keylen =  15

            select #6,  "APCPLNWK",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =   66

            select #7,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #8,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21

            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #10, "@GLSGED@", consec, recsize = 220

            select #11, "@GLSBIL@", consec, recsize = 165

            select #12, "@RMKGD1@", consec, recsize = 220

            select #13, "@RMKBL1@", consec, recsize = 165

            select #14, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =   170,             ~
                        keypos =    1, keylen =  51,                     ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos  =  70, keylen =   8, dup,    ~
                            key  3, keypos  =  78, keylen =   8, dup,    ~
                            key  4, keypos  =  52, keylen =   8,         ~
                            key  5, keypos  =  36, keylen =   16, dup

            select #15, "MMDD@DPT", consec, recsize = 172

            select #16, "@RMKGD2@", consec, recsize = 220 /* (EWD001) */

            select #17, "@RMKBL2@", consec, recsize = 165 /* (EWD001) */ 

            select #18, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos  =   4, keylen =  12,         ~
                            key  3, keypos  =   1, keylen =  15

            select #19, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 10,   keylen =  19

            select #20, "APCPLNSA",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen =  17,                     ~
                        alt key  1, keypos  =     1, keylen = 19, dup
                                                       /* (EWD006)     */ 
            select #21, "@RMKGDK@", consec, recsize = 220

            select #22, "@RMKBLK@", consec, recsize = 165
                                                       /* (EWD006)     */
                                                       /* (EWD012)     */ 
            select #25, "EWDGLSLB",                                      ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =  1,   keylen =  14,                     ~
                        alt key  1, keypos  =    15, keylen = 12, dup
                                                       /* (EWD012) End */

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),500%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),  0%, rslt$(14%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),  0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),  0%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%),  0%, rslt$(20%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%),500%, rslt$(25%))
                                                        /* (EWD012)     */

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
                                               /* (EWD005) - Begin */
            scr$(1%) = "****************************************"
            scr$(2%) = "*       New Glass Processing           *"
            scr$(3%) = "*      ----------------------          *"
            scr$(4%) = "* (1) - Calc. and Explode Glass Sizes  *"
            scr$(5%) = "* (2) - Create Glass Labels/Batches    *"
            scr$(6%) = "* (3) - Proc Re-Make Glass (All)       *"
            scr$(7%) = "* (4) - Proc Re-Make Glass (Production)*"
            scr$(8%) = "* (5) - Proc Re-Make Glass (In-House)  *"
            scr$(9%) = "* (6) - Proc INDY Glass                *"
            scr$(10%)= "****************************************"
                                               /* (EWD005) - End   */  
            rpt$(1%) = "****************************************"
            rpt$(2%) = "*       New Glass Reporting            *"
            rpt$(3%) = "*      ----------------------          *"
            rpt$(4%) = "* (1) - Scanned Glass Re-Make's        *"
            rpt$(5%) = "* (2) - Scheduled Glass Re-Make's      *"
            rpt$(6%) = "* (3) - Scheduled Glass                *"
            rpt$(7%) = "* (4) - Completed Glass Report - Date  *"
            rpt$(8%) = "* (5) - Completed Re-Make Glass - Date *"
            rpt$(9%) = "*                                      *"
            rpt$(10%)= "****************************************"

        REM - NEAREST 8TH INCH
           sze$ = "1/81/43/81/25/83/47/8         "

        REM - NEAREST 16TH OF AN INCH
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "

                        /* (GLASS01)  - GLASS QUANTITIES FOR TOP/BOT    */
                        /* (GLASS02)  - TABLE FOR TOP GLASS ONLY        */
                        /* (GLASS03)  - TABLE FOR TWO BOTTOMS           */
                        /* (GLASS04)  - Table for no Glass Required     */
                        /* (GLASS05)  - TABLE FOR '8' SERIES WITH GLASS */
                        /* (GLASS06)  - TABLE TO SKIP NO DETAIL LABELS  */
                        /* (GLASS07)  - TABLE FOR SLIDERS               */
                        /* (GLASS08)  - TABLE - SPECIAL SHAPES NO GLASS */
                        /* (GLASS09)  - TABLE - Bilco Glass Depts       */

            tab$(1%) = "GLASS01  "   : tab$(6%) = "GLASS06  "
            tab$(2%) = "GLASS02  "   : tab$(7%) = "GLASS07  "
            tab$(3%) = "GLASS03  "   : tab$(8%) = "GLASS08  "
            tab$(4%) = "GLASS04  "   : tab$(9%) = "GLASS09  "
            tab$(5%) = "GLASS05  "
            tab_max% = 9%

            gosub load_stock                 /* Used by CHECK_STOCK    */
            gosub load_tables                /* Load all Glass Tables  */

            t_err$(1%) = "Err-1 No Equation Found  "
            t_err$(2%) = "Err-2 C.M.Rail Calc Error"
            t_err$(3%) = "Err-3 C.M.Rail Entered   "
            t_err$(4%) = "Err-4 GED Overall Thick  "
            t_err$(5%) = "Err-5 GED Sandwich Find  "
            t_err$(6%) = "Err-6 GED Spacer Descr.  "
            t_err$(7%) = "Err-7 GED No. Lits Error "
            t_err$(8%) = "Err-8 GED Hinge Code Err "
            t_err$(9%) = "Err-9 GED Muttin Error   "
            t_err$(10%) = " "

            ged$( 1%) = "OV Thick =XXXXXX    "
            ged$( 2%) = "Sandwich =XXXXXXXXXX"
            ged$( 3%) = "Spacer D.=XXXXXXXXXX"
            ged$( 4%) = "No. Lits =X         "
            ged$( 5%) = "Muttin Cd=XXXXXXXX  "

            ged$( 6%) = "OV Thick =XXXXXX    "
            ged$( 7%) = "Sandwich =XXXXXXXXXX"
            ged$( 8%) = "Spacer D.=XXXXXXXXXX"
            ged$( 9%) = "No. Lits =X         "
            ged$(10%) = "Muttin Cd=XXXXXXXX  "
                                                  /* (EWD004) - Begin     */    
            ss$( 1%) = "023DSSP17TB"  
            ss$( 2%) = "008DSSP17TB"  
            ss$( 3%) = "008SSSP19TB"  
            ss$( 4%) = "047DSSP17TB"  
            ss$( 5%) = "048DSSP17TB"  
            ss$( 6%) = "028DSSP17TT"  
            ss$( 7%) = "028DSSP17BB"  
            ss$( 8%) = "052DSSP17TB"                          
            ss$( 9%) = "049DSSP17TB"  
            ss$(10%) = "006DSSP17TB"
  
            ss$(11%) = "007DSSP17TB"  
            ss$(12%) = "036DSSP15TT"  
            ss$(13%) = "036DSSP15BB"  
            ss$(14%) = "048DSSP15TT"  
            ss$(15%) = "048DSSP15BB"  
            ss$(16%) = "047DSSP15TB"  
            ss$(17%) = "008DSSP15TB"  
            ss$(18%) = "048SSSP19TB"  
            ss$(19%) = "048SSSP17TT"                          
            ss$(20%) = "048SSSP17BB"
 
            ss$(21%) = "047SSSP17TB"  
            ss$(22%) = "047SSSP19TB"  
            ss$(23%) = "008SSSP17TB"  
            ss$(24%) = "033SSSP17TB"  
            ss$(25%) = "033DSSP17TB"
            ss$(26%) = "033SSSP19TB"  
            ss$(27%) = "033DSSP15TB"  
            ss$(28%) = "028SSSP19TT"  
            ss$(29%) = "028SSSP19BB"  
            ss$(30%) = "049SSSP19TB"
                                                     /* (EWD007)     */
            ss$(31%) = "002DSSP17TB"
            ss$(32%) = "002SSSP19TB" 
                                      
            ss$(33%) = "052SSSP19TB"  
            ss$(34%) = "006SSSP19TB"  
            ss$(35%) = "007SSSP19TB"  
            ss$(36%) = "036SSSP17TT"  
            ss$(37%) = "036SSSP17BB"
            ss$(38%) = "043SSSP17TB"  
            ss$(39%) = "043SSSP19TB"  
            ss$(40%) = "043DSSP17TB"  
            ss$(41%) = "043DSSP15TB"
  
            ss$(42%) = "033SSSP15TT"  
            ss$(43%) = "033SSSP15BB"
                                                      /* (EWD007)   */  
            ss$(44%) = "023Tempered"  
            ss$(45%) = "042Tempered"
            ss$(46%) = "043Tempered"  
            ss$(47%) = "999Tempered"  
            ss$(48%) = "Obsecure   "  
            ss$(49%) = "Diamond    "  
            ss$(50%) = "Stock      "
            ss$(51%) = "Errors     "
                                      
            ss_max% = 47%
   
                                               /* (EWD004) - End   */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% and fieldnr% = 1% then inputmode_r
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%
            goto editpg1
        inputmode_a
            txt$(2%) =                                                   ~
                    "*(Calculate) Glass Cuts for Manufactured Products*"
            for fieldnr% = 1% to  1%
L10330:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10450
L10350:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then goto inputmode
                      if keyhit% <>  4% then       L10440
L10380:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10350
                         if fieldnr% = 1% then L10330
                         goto L10380
L10440:               if keyhit% <> 0% then       L10350
L10450:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10350
            next fieldnr%
            goto editpg2

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub begin_process
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub calc_data
                  if keyhit%  = 16% then goto  inputmode
                  if keyhit% <>  0% then       editpg2
L11310:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% > 1% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11360:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11360
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11360
                  lastfieldnr% = fieldnr%
            goto L11310


        REM *************************************************************~
            *       I N P U T   M O D E   F O R   R E P O R T S         *~
            *************************************************************

        inputmode_r
            gosub initialize_variables
            txt$(2%) =                                                   ~
                    "* (Report) Current Glass Status for MFG Products *"
            for fieldnr% = 1% to  2%
L12300:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12420
L12320:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then goto inputmode
                      if keyhit% <>  4% then       L12410
L12360:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L12320
                         if fieldnr% = 1% then L12300
                         goto L12360
L12410:               if keyhit% <> 0% then       L12320
L12420:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12320
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   R E P O R T S          *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto  inputmode
                  if keyhit% <>  0% then       editpg3
L14110:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% > 1% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L14160:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L14160
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L14160
                  lastfieldnr% = fieldnr%
            goto L14110

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
             if scr_sel% <> 1% then goto L19110
                return clear all
                goto inputmode_a

L19110:      call "SHOSTAT" ("Creating "& scr_msg$)
                                               /* (EWD005) - Begin    */
             if scr_sel% = 2% or scr_sel% = 6% then gosub create_data    ~
                               else gosub create_rmk_data
                                               /* (EWD005) - End      */
             remake_label% = 0%                /* (EWD012) - Use to   */
                                               /*   off Old Label File*/
             gosub print_labels
             gosub prompt_run_no               /* (EWD012)            */

             gosub prompt_user
             if comp% <> 0%  then goto L19190
                gosub create_batch
                gosub prompt_user_done
L19190:      gosub delete_work
             keyhit% = 0%
        return clear all
        goto inputmode

        prompt_user
            comp% = 2%
            hdr$ = "** Glass Optimizer Batches **"
            if ged_bilco$ = "0" then                                     ~
             msg$(1%)="Do you wish to Create Both GED & Bilco Glass     "
            if ged_bilco$ = "1" then                                     ~
             msg$(1%)="Do you wish to Create GED Glass Batches Only?    "
            if ged_bilco$ = "2" then                                     ~
             msg$(1%)="Do you wish to Create Bilco Glass Batches Only?  "
            msg$(2%)="             O P T I M I Z A T I O N             "
            msg$(3%)="Press <RETURN> To Create ,  Any (PF) Key To Exit."
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        prompt_user_done
            comp% = 2%
            hdr$ = "** Glass Optimizer Batches **"
            msg$(1) = "Transmit File Name (@GLSGED@) Number of Batches"  ~
                     & " Equals ("&bat$&")"

            msg$(2) = "             O P T I M I Z A T I O N             "
            msg$(3) = "Press <RETURN> or Any (PF) Key To Continue....   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        prompt_no_data
            comp% = 2%
            hdr$ = "***** Glass Processing *****"
          msg$(1%)= "( No Data ) Found on System for Specified Selection"
          msg$(2%)= "                   G l a s s                     "
          msg$(3%)= "Press <RETURN> or Any (PF) Key To Continue....   "
          call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return clear all
        goto inputmode

        print_report
            gosub select_printer
            gosub gen_rpt                           /* (EWD005)        */
            gosub close_printer
        return clear all
        goto inputmode

        select_printer
            pageno% = 0%
            lcnt%    = 99%
            title$ = rp_sel_d$
            call "FMTTITLE" (title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            company$ = "Ellison Window and Door"
            call "SETPRNT" ("APCGLS", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCGLS", " ", 0%, 1%)
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

        deffn'053(fieldnr%)
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
         "Enter 1=Calc,2=Lab/Bat,3,4,5=ReMake (All),(Prod),(In-house),6=INDY?",~
         "Enter Machine Selection, 0=Both, 1=GED, 2=Bilco? Def. = '0'? ",~
         "Enter a Valid Planning Production Date?                      ",~
         "Enter a Valid Department Selection or All?                   ",~
         "Enter a Valid Shift Selection or 'AA' = All Shifts?          ",~
         "Enter the Completion Date for the Glass?                     ",~
         "Enter a Production Load Number?                              "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28290
                inpmessage$ = edtmessage$
                return

L28290
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Manufacture Part Number?                       "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28420
                inpmessage$ = edtmessage$
                return

L28420
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter a Beginning and Ending Production Date for Report?     ",~
         "Enter a Valid report Selection 1=Scheduled,2=Complete,3=Remake"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_dte$, scr_dte1$,       ~
                      dt_key1$, scr_sel$, size$, scr_msg$, scr_dept$,    ~
                      scr_msg1$, scr_shft$, scr_shft_d$, glass_dte$,     ~
                      glass_dte1$, ld_key$, scr_load$, scr_desc$,        ~
                      ged_bilco$, ged_desc$, save_part$, dt_key$,        ~
                      dt_desc$, dt_part$, dt_desc$, cut$(), ged$(),      ~
                      bg_date$, bg_dte$, ed_date$, ed_dte$, rp_sel$,     ~
                      rp_sel_d$, gl_key$, gl_sort$, gl_run$, gl_date$
                                                       /* (EWD012)     */
            txt$(1%) =                                                   ~
                "**************************************************"
            txt$(2%) =                                                   ~
                "*   (New) Glass Processing and Batch Creation    *"
            txt$(3%) =                                                   ~
                "*       ( E W D ) G l a s s   S y s t e m        *"
            txt$(4%) =                                                   ~
                "**************************************************"

            ff$      = "MMDD@DPT"
            volume$  = "CARLOS"
            library$ = "APCDATA "
                                                       /* (EWD012)    */
            gl_run%  = 0%
            gl_sort% = 0%
                                                       /* (EWD012)    */ 
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
            model$   = str(dt_rec$,189%,3%)     /* Model/Product       */
            dt_load$ = str(dt_rec$,1%,5%)       /* Production Load No. */
            dt_bar$  = str(dt_rec$,24%,18%)     /* Barcode Value       */
            dt_dept$ = str(dt_rec$,42%,3%)      /* Prod Seq. No        */
            dt_date$ = str(dt_rec$,47%,6%)      /* Production Date     */
            dt_ref$  = str(dt_rec$,96%,8%)      /* Warranty Number     */
            dt_shft$ = str(dt_rec$,104%,2%)     /* Shift Code          */
            dt_seq$  = str(dt_rec$,111%,5%)     /* Dept Seq. No.       */
            dt_cust$ = str(dt_rec$,124%,9%)     /* Customer Code       */
            dt_part$ = str(dt_rec$,189%,25%)    /* Part Number         */
            dt_sash$ = str(dt_rec$,214%,1%)     /* Sash 1=T,2=B,3=FGO  */
            dt_prt$  = str(dt_rec$,215%,1%)     /* MFG Part Y or N     */
            dt_samp$ = str(dt_rec$,216%,1%)     /* Sample Y or N       */
            dt_wood$ = str(dt_rec$,217%,3%)     /* Wood Surround Code  */
            dt_special$ = str(dt_rec$,220%,10%) /* Special Prod Flags  */
            dt_txt$  = str(dt_rec$,236%,4%)     /* Line Item Text Id   */
            dt_gls$ = "Y"
            if str(dt_part$,5%,2%) = "00" then dt_gls$ = "N"
        return

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

L35040:    FMT CH(04),                             /* Label DeF    ( 4)*/~
               CH(02), CH(01),                     /* Glass Type   ( 3)*/~
               CH(07), CH(01),                     /* Grid/Liting  ( 8)*/~
               CH(08), CH(01),                     /* Sales Order  ( 9)*/~
               CH(10), CH(01),                     /* Calc Width   (11)*/~
               CH(03), CH(01),                     /* Model Code   ( 4)*/~
               CH(08), CH(01),                     /* Prod Date    ( 9)*/~
               CH(10), CH(01),                     /* Calc Height  (11)*/~
               CH(02), CH(01),                     /* Shift Code   ( 3)*/~
               CH(05), CH(01),                     /* Seq No       ( 6)*/~
               CH(15), CH(01),                     /* Window WidxHgt16)*/~
               CH(03), CH(01),                     /* Remake Count ( 4)*/~
               CH(03), CH(01),                     /* Depart Code  ( 4)*/~
               CH(40), CH(01),                     /* Text         (41)*/~
               CH(09), CH(01),                     /* Glass Barcode(10)*/~
               CH(03), CH(01),                     /* Top or Bot   ( 4)*/~
               CH(06), CH(01),                     /* Color        ( 7)*/~
               CH(09), CH(01),                     /* Glass Barcode(10)*/~
               CH(01), CH(01),                     /* Contor Grid  (02)*/~
               CH(01)                              /* End of Label ( 1)*/
                                                                          
                                                   /* (EWD012) - Begin */
                                                   /* (EWDGLSLB) Labels*/
L35050:    FMT CH(06),                             /* Prod. Date       */~
               CH(03),                             /* Run/Batch No.    */~
               CH(05),                             /* sort Number      */~
               CH(09),                             /* Glass Barcode    */~
               CH(03),                             /* Glass Re-Make No */~
               CH(02),                             /* Glass Type       */~
               CH(07),                             /* Grid/Liting      */~
               CH(08),                             /* Sales Order      */~
               CH(10),                             /* Calc Width       */~
               CH(03),                             /* Model Code       */~
               CH(10),                             /* Calc Height      */~
               CH(02),                             /* Shift Code       */~
               CH(05),                             /* Seq No           */~
               CH(15),                             /* Window WidxHgt   */~
               CH(03),                             /* Depart Code      */~
               CH(40),                             /* Text             */~
               CH(03),                             /* Top or Bot       */~
               CH(06),                             /* Color            */~
               CH(01),                             /* Contor Grid      */~
               CH(29)                              /* Filler Area      */
                                                   /* (EWD012) - End   */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40220,         /* Process Selection */   ~
                                L40220,         /* 0=Both,1=GED,2=Bil*/   ~
                                L40210,         /* Production Date   */   ~
                                L40210,         /* Department Code   */   ~
                                L40210,         /* Shift Code        */   ~
                                L40200,         /* Glass Production  */   ~
                                L40210          /* Load Number       */
              goto L40240

L40200:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40220:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Process Selection(1-6):",                    ~
               at (04,26), fac(lfac$(1%)), scr_sel$             , ch(01),~
               at (04,30), fac(lfac$(1%)), size$                , ch(04),~
               at (04,40), fac(hex(84)), scr_msg$               , ch(30),~
               at (05,02), "Machine-Both,GED,Bilco:",                    ~
               at (05,26), fac(lfac$(2%)), ged_bilco$           , ch(01),~
               at (05,40), fac(hex(84)), ged_desc$              , ch(20),~
               at (06,02), "Planned Prod. Date    :",                    ~
               at (06,26), fac(lfac$(3%)), scr_dte$             , ch(08),~
               at (07,02), "Department Code, (A)ll:",                    ~
               at (07,26), fac(lfac$(4%)), scr_dept$            , ch(03),~
               at (07,40), fac(hex(84)), scr_msg1$              , ch(30),~
               at (08,02), "Shift Code or AA = All:",                    ~
               at (08,26), fac(lfac$(5%)), scr_shft$            , ch(02),~
               at (08,40), fac(hex(84)), scr_shft_d$            , ch(30),~
               at (09,02), "Glass Production Date :",                    ~
               at (09,26), fac(lfac$(6%)), glass_dte$           , ch(08),~
               at (10,02), "Load Number or Blank  :",                    ~
               at (10,26), fac(lfac$(7%)), scr_load$            , ch(05),~
               at (10,40), fac(hex(84)),   scr_desc$            , ch(30),~
                                                                         ~
               at (11,21), fac(hex(84)), scr$(1%)               , ch(40),~
               at (12,21), fac(hex(84)), scr$(2%)               , ch(40),~
               at (13,21), fac(hex(84)), scr$(3%)               , ch(40),~
               at (14,21), fac(hex(84)), scr$(4%)               , ch(40),~
               at (15,21), fac(hex(84)), scr$(5%)               , ch(40),~
               at (16,21), fac(hex(84)), scr$(6%)               , ch(40),~
               at (17,21), fac(hex(84)), scr$(7%)               , ch(40),~
               at (18,21), fac(hex(84)), scr$(8%)               , ch(40),~
               at (19,21), fac(hex(84)), scr$(9%)               , ch(40),~
               at (20,21), fac(hex(84)), scr$(10%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
                                                  /* (EWD008) - Begin */
               if keyhit% <> 7% then goto L40600
                  run$ = "EWDPLN66"
                  gosub run_program
                  goto L40070
                                                  /* (EWD008) - End   */
                                                  /* (EWD005) - Begin */ 
L40600:        if keyhit% <> 9% then goto L40700
                  run$ = "EWDPLN64"
                  gosub run_program
                  goto L40070
                                                  /* (EWD005) - End   */
L40700:        if keyhit% <> 10% then goto L40730
                  gosub analysis
                  goto L40070

L40730:        if keyhit% <> 12% then goto L40770
                  gosub purge_data
                  goto L40070

L40770:        if keyhit% <> 15% then goto L40810
                  call "PRNTSCRN"
                  goto L40240

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1                       /* (EWD005) Mod - PF(9)    */
        if edit% = 2% then L41000     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      " (9)Glass Search       (14)Print Report"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "(10)Glass Analysis     (15)Print Screen"
            pf$(3%) = "                 (7)Special Glass       " &       ~
                      "(12)Glass Purge        (16)Exit Program"
            pfkeys$ = hex(01ffff04ffff07ff090aff0cff0e0f1000)
            if fieldnr% = 1% then L40960
                str(pf$(1%),64%) = " "  :  str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),64%) = " "  :  str(pfkeys$,16%,1%) = hex(ff)
                str(pf$(3%),18%,18%) = " " : str(pfkeys$,7%,1%) = hex(ff)
                                                /* (EWD008)           */
L40960:     if fieldnr% > 1% then L40980
                str(pf$(2%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40980:     return

L41000: if fieldnr% > 0% then L41090  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      " (9)Glass Search                       "
            pf$(2%) = "                                        " &       ~
                      "(10)Glass Analysis     (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "(12)Glass Purge        (16)Print Labels"
            pfkeys$ = hex(01ffffffffffffff090aff0cffff0f1000)
            return
L41090:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Calculate Sizes for Screen Display                        *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42080          /* Stock Part Number */

              goto L42095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42095:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
                                                                         ~
               at (07,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,02), "Manufactured Window :",                      ~
               at (08,25), fac(lfac$(1%)), dt_part$             , ch(25),~
               at (09,12), fac(hex(84)), dt_desc$               , ch(32),~
                                                                         ~
               at (10,02), fac(hex(84)), cut$(1%)               , ch(55),~
               at (11,02), fac(hex(84)), cut$(2%)               , ch(55),~
               at (12,02), fac(hex(84)), cut$(3%)               , ch(55),~
               at (13,02), fac(hex(84)), cut$(4%)               , ch(55),~
               at (14,02), fac(hex(84)), cut$(5%)               , ch(55),~
                                                                         ~
               at (10,58), fac(hex(84)), ged$(1%)               , ch(20),~
               at (11,58), fac(hex(84)), ged$(2%)               , ch(20),~
               at (12,58), fac(hex(84)), ged$(3%)               , ch(20),~
               at (13,58), fac(hex(84)), ged$(4%)               , ch(20),~
               at (14,58), fac(hex(84)), ged$(5%)               , ch(20),~
                                                                         ~
               at (16,02), fac(hex(84)), cut$(6%)               , ch(55),~
               at (17,02), fac(hex(84)), cut$(7%)               , ch(55),~
               at (18,02), fac(hex(84)), cut$(8%)               , ch(55),~
               at (19,02), fac(hex(84)), cut$(9%)               , ch(55),~
               at (20,02), fac(hex(84)), cut$(10%)              , ch(55),~
                                                                         ~
               at (16,58), fac(hex(84)), ged$(6%)               , ch(20),~
               at (17,58), fac(hex(84)), ged$(7%)               , ch(20),~
               at (18,58), fac(hex(84)), ged$(8%)               , ch(20),~
               at (19,58), fac(hex(84)), ged$(9%)               , ch(20),~
               at (20,58), fac(hex(84)), ged$(10%)              , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42345
                  call "PRNTSCRN"
                  goto L42095

L42345:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42420     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L42420: if fieldnr% > 0% then L42465  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Calculate   "
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L42465:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Optimizer Batch Name(s)                                   *~
            *************************************************************

        batch_header
            gosub set_pf3
L43080:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
               at (08,02), "Batch Description :",                        ~
               at (08,25), fac(hex(81)), file$                  , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L43310
                  call "PRNTSCRN"
                  goto L43080

L43310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
               txt$(2%) =                                                ~
                    "*  (Optimization) Batch Processing Description   *"
            inpmessage$ = "Enter Description for Optimization Batches"
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Process Data"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)

            return



        REM *************************************************************~
            *    A N A L Y S I S   A N D   P U R G E   U T I L I T Y    *~
            *************************************************************

        analysis
                                            /* (EWD010) -Oven Tracking*/
            call "APCPLD45" (1%,            /* Called From (APCPLA45) */ ~
                             "000",         /* Dept Not Applicable    */ ~
                             scr_shft$,     /* Shift Selection        */ ~ 
                             #8 )           /* File (APCPLNGR)        */
                                            /* (EWD010) - Tracking    */
        return

        purge_data
                                               /* Only Purge Completed */
                                               /* Glass Two (2) Weeks  */
                                               /* Old, No Re-Makes     */
            init(" ") rm_ky$, pg_dte$, pg_dte1$, rm_cnt$, pg_dte2$,      ~
                      rm_flag$
            rm_ky$ = all(hex(00))
            rm_cnt$ = "Checked [XXXXXXXX] Deleted [xxxxxxxx]"
            cnt% = 0% : cnt1% = 0%
            call "SHOSTAT" ("Purging Glass Data")
            pg_dte$ = date : err% = 0%
            call "DATE" addr("G+",pg_dte$,-30%,pg_dte1$,err%)
            call "DATE" addr("G+",pg_dte$,-7%,pg_dte2$,err%)
        REM  Purge Override
        REM  rhh$ = "11/27/98"
        REM  call "DATUNFMT" (rhh$)
        REM  pg_dte2$ = str(rhh$,1%,6%)

        REM str(rm_ky$,1%,6%) = pg_dte1$        /* (EWD005) Keep 30 days*/
                                                /* of stock glass       */
        purge_nxt
            read #8,hold,key 1% > rm_ky$, using L44250, rm_ky$, rm_flag$, ~
                                                      eod goto purge_done
L44250:        FMT POS(7), CH(27), POS(163), CH(1)
            cnt% = cnt% + 1%
            if mod(cnt%,50%) <> 0% then goto L44320
               convert cnt% to str(rm_cnt$,10%,8%), pic(########)

               convert cnt1% to str(rm_cnt$,29%,8%), pic(########)

               print at(03,22);hex(84);rm_cnt$;

L44320:     if str(rm_ky$,1%,6%) > pg_dte2$ then goto purge_done
                                         /* Save Stock for 30 Days   */
               if rm_flag$ = "S" and str(rm_ky$,1%,6%) > pg_dte1$ then ~
                                                 goto purge_nxt  
               delete #8                 /* Delete all Prior to Date */
               cnt1% = cnt1% + 1%
               goto purge_nxt
        purge_done
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'070(1%, fieldnr%)
              gosub set_pf5
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L45150,         /* Stock Part Number */   ~
                                L45160          /* Report Selection  */

              goto L45180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L45150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L45160:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L45180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
                                                                         ~
               at (07,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,02), "Beginning Prod Date :",                      ~
               at (08,25), fac(lfac$(1%)), bg_date$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (08,40), "Ending Prod. Date   :",                      ~
               at (08,66), fac(lfac$(1%)), ed_date$             , ch(10),       /* (Y2K, LDJ) */~
                                                                         ~
               at (09,02), "Report Type         :",                      ~
               at (09,25), fac(lfac$(2%)), rp_sel$              , ch(01),~
               at (09,40), fac(hex(84)), rp_sel_d$              , ch(30),~
                                                                         ~
               at (11,21), fac(hex(84)), rpt$(1%)               , ch(40),~
               at (12,21), fac(hex(84)), rpt$(2%)               , ch(40),~
               at (13,21), fac(hex(84)), rpt$(3%)               , ch(40),~
               at (14,21), fac(hex(84)), rpt$(4%)               , ch(40),~
               at (15,21), fac(hex(84)), rpt$(5%)               , ch(40),~
               at (16,21), fac(hex(84)), rpt$(6%)               , ch(40),~
               at (17,21), fac(hex(84)), rpt$(7%)               , ch(40),~
               at (18,21), fac(hex(84)), rpt$(8%)               , ch(40),~
               at (19,21), fac(hex(84)), rpt$(9%)               , ch(40),~
               at (20,21), fac(hex(84)), rpt$(10%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L45600
                  call "PRNTSCRN"
                  goto L45180

L45600:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf5
        if edit% = 2% then L45750     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L45750: if fieldnr% > 0% then L45840  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L45840:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50170,         /* Glass Process Selectio*/ ~
                              L50470,         /* Mach 0=Both,1=GED,2=Bi*/ ~
                              L50660,         /* Planned Production Dat*/ ~
                              L50940,         /* Department Code or All*/ ~
                              L51090,         /* Shift Code or AA = All*/ ~
                              L51220,         /* Glass Completion DTE  */ ~
                              L51400          /* Load Number           */
            return

L50170: REM Process Selection                     SCR_SEL$
            if size$ <> " " then goto L50200
               size$ = "0100"                    /* Default Batch Size */
L50200:     convert size$ to size%, data goto L50420

            convert size% to size$, pic(####)

            if size% > 200% then goto L50420
            scr_sel% = 0%
            if scr_sel$ <> " " then goto L50280
               scr_sel$ = "1"
L50280:     convert scr_sel$ to scr_sel%, data goto L50380
                                                 /* (EWD005) 12/02/98 */
            if scr_sel% < 1% or scr_sel% > 6% then goto L50380
            scr_msg$ = str(scr$(scr_sel% + 3%),9%,30%)
            if scr_sel% <> 1% then return
               init(" ") scr_sel$, scr_msg$, ged_bilco$, ged_desc$,      ~
                         size$, scr_dte$, scr_dept$, scr_msg1$,          ~
                         scr_shft_d$, glass_dte$, scr_load$, scr_desc$
               gosub begin_process
        return
L50380:     errormsg$ = "(Error) - Invalid Process Selection (1 thru 6)?"
            gosub error_prompt
            init(" ") scr_sel$, size$, scr_msg$
        return
L50420:     errormsg$ = "(Error) - Invalid Size Specification."
            gosub error_prompt
            init(" ") scr_sel$, size$, scr_msg$
        return

L50470: REM Machine Flag                          GED_BILCO$
           init(" ") ged_desc$
           if ged_bilco$ <> " " then goto L50510
              ged_bilco$ = "0"                     /* Default to 'GED' */
L50510:    if ged_bilco$ = "0" then ged_desc$ = "Both GED & Bilco  "
           if ged_bilco$ = "1" then ged_desc$ = "GED Glass Machine "
           if ged_bilco$ = "2" then ged_desc$ = "Bilco Glass Machine"
           if len(ged_desc$) < 5 then goto L50610
        REM IF SCR_SEL% = 3% AND GED_BILCO$ = "0" THEN GOTO 50610
                                                   /* (EWD005)         */ 
           if scr_sel% = 2% or scr_sel% = 6% then return
              init(" ") scr_dte$, scr_dept$, scr_msg1$, scr_shft$,       ~
                        scr_shft_d$, glass_dte$, scr_load$, scr_desc$
              gosub begin_process
        return
L50610:    errormsg$= "(Error)-Invalid Glass Machine Selection (0,1,2)?"
           gosub error_prompt
           init(" ") ged_bilco$, ged_desc$
        return

L50660: REM Production Date                       SCR_DTE$, SCR_DTE1$
           date% = 0%
           call "DATEOK" (scr_dte$, date%, errormsg$ )
           if errormsg$ <> " " then return
              scr_dte1$ = scr_dte$
              call "DATUNFMT" (scr_dte1$)
              dt_key1$ = all(hex(00))
              str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
              read #2,key 1% > dt_key1$, using L50760, dt_key1$,          ~
                                                      eod goto L50890
L50760:         FMT POS(47), CH(57)
              if str(scr_dte1$,1%,6%) <> str(dt_key1$,1%,6%) then        ~
                                                              goto L50890
        REM Remake Production Date
           call "DATE" addr("GD",str(scr_dte1$,1%,6%),dd$,date%)
           dd1$ = dd$ : num$ = "7"
           if dd1$ = "MON" then num$ = "1"
           if dd1$ = "TUE" then num$ = "2"
           if dd1$ = "WED" then num$ = "3"
           if dd1$ = "THU" then num$ = "4"
           if dd1$ = "FRI" then num$ = "5"
           if dd1$ = "SAT" then num$ = "6"

        return
L50890:    errormsg$ = "No Data on File for Specified Production Date"
           gosub error_prompt
           init(" ") scr_dte$, scr_dte1$
        return

L50940: REM Department Selection                  SCR_DEPT$
            if scr_dept$ <> " " then goto L50990
               scr_dept$ = "ALL"
               scr_msg1$ = "(A)LL Process all Dept's"
               goto L51020
L50990:     gosub lookup_dept
            if dept% = 0% then goto L51040

L51020: REM ff$   = str(scr_dte1$,3%,4%) & "@" & scr_dept$
            ff$ = str(scr_dte$,1,2) & str(scr_dte$,4,2) & "@" & scr_dept$
        return
L51040:     errormsg$ = "(Error) - Invalid Department Selection?"
            gosub error_prompt
            init(" ") scr_dept$, scr_msg1$
        return

L51090: REM Shift Selection                       SCR_SHFT$
            if scr_shft$ <> " " then goto L51140
               scr_shft$ = "AA"
               scr_shft_d$ = "(AA) Process all Shift's"
               return
L51140:     gosub lookup_shift
            if shft% = 0% then goto L51170
        return
L51170:     errormsg$ = "(Error) - Invalid Shift Selection?"
            gosub error_prompt
            init(" ") scr_shft$, scr_shft_d$
        return

L51220: REM Glass Production Date                 GLASS_DTE$,GLASS_DTE1$
           if scr_sel% = 2% then goto L51260
           if scr_sel% = 6% then goto L51260
                                                  /* (EWD005)          */
              init(" ") scr_dept$, scr_msg1$, scr_load$, glass_dte$
              return
L51260:    date% = 0%
           if glass_dte$ <> " " then goto L51300
              goto L51350

L51300:    call "DATEOK" (glass_dte$, date%, errormsg$ )
           if errormsg$ <> " " then return
              glass_dte1$ = glass_dte$
              call "DATUNFMT" (glass_dte1$)
        return
L51350:    errormsg$ = "(Error) - Invalid Glass Production Date??"
           gosub error_prompt
           init(" ") glass_dte$,glass_dte1$
        return

L51400: REM LOAD NUMBER
           if scr_sel% <> 1% then goto L51450
              init(" ") scr_dept$, scr_msg1$, scr_load$, glass_dte$,     ~
                        scr_desc$, stk_so$
              return
L51450:    if scr_load$ <> " " then goto L51500
L51460:       if scr_sel$ = "6" then goto L51570   /* (EWD005) Req.   */
              scr_load$ = "ALL  "
              scr_desc$ = "(ALL) - Loads"
              return

L51500:    if str(scr_load$,1%,3%) = "ALL" then goto L51460
           ld_key$ = all(hex(00))
           ld_key$ = scr_load$
           read #5,key = ld_key$, using L51540, scr_desc$, eod goto L51560
L51540:       FMT POS(16), CH(30)
        return
L51560:    errormsg$ = "(Error) - Invalid Load Number Entered?"
           gosub error_prompt
           init(" ") scr_load$, scr_desc$, stk_so$
        return                                    /* (EWD005) - Begin */
L51570:    errormsg$ = "(Error) - Valid Load Number is Required?"
           gosub error_prompt
           init(" ") scr_load$, scr_desc$, stk_so$
        return
                                                  /* (EWd005) - End   */
        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51670          /* Manufactured Part     */

            return

L51670: REM Manufactured Part                     DT_PART$
            init(" ") cut$(), ged$(), dt_desc$, model$, dt_dept$
            if dt_part$ <> " " then goto L51750

               dt_desc$ = " "
               dt_desc$ = hex(06) & "Select a Valid Stock Part"
               call "GETCODE" (#7,dt_part$, dt_desc$, 0%, 1.32, f1%(7))

L51750:     dt_desc$ = "( Non-Stock Part )"
            read #7,key = dt_part$, using L51770,dt_desc$, eod goto L51790
L51770:        FMT POS(26), CH(32)

L51790:     if len(dt_part$) < 19 then goto L51850
               gosub format_screen
               model$ = str(dt_part$,1%,3%)
               if str(model$,1%,1%) = "8" then dt_dept$ = "008"
               if str(model$,1%,1%) = "9" then goto L51850
        return
L51850:     errormsg$ = "(Error) - Invalid Part Number for Calculation?"
            gosub error_prompt
            init(" ") dt_part$, dt_desc$
        return

        format_screen
              cut$(1%) =                                                 ~
                 "Top(1) Wid =            Hght =          Clmr-         "
              cut$(2%) =                                                 ~
                 "   (2) Wid =            Hght =          Clmr-         "
              cut$(3%) =                                                 ~
                 "   (3) Wid =            Hght =          Clmr-         "
              cut$(4%) =                                                 ~
                 "   (4) Wid =            Hght =          Clmr-         "
              cut$(5%) =                                                 ~
                 "   (5) Wid =            Hght =          Clmr-         "
              cut$(6%) =                                                 ~
                 "Bot(1) Wid =            Hght =          Clmr-         "
              cut$(7%) =                                                 ~
                 "   (2) Wid =            Hght =          Clmr-         "
              cut$(8%) =                                                 ~
                 "   (3) Wid =            Hght =          Clmr-         "
              cut$(9%) =                                                 ~
                 "   (4) Wid =            Hght =          Clmr-         "
              cut$(10%) =                                                ~
                 "   (5) Wid =            Hght =          Clmr-         "
                                                /* Width  = Pos(14,9) */
                                                /* Height = Pos(32,8) */
                                                /* Clmr   = Pos(46,9) */
            ged$( 1%) = "OV Thick =XXXXXX    "
            ged$( 2%) = "Sandwich =XXXXXX    "
            ged$( 3%) = "Spacer D.=XXXXXXXXXX"
            ged$( 4%) = "No. Lits =X         "
            ged$( 5%) = "Muttin Cd=XXXXXXXX  "

            ged$( 6%) = "OV Thick =XXXXXX    "
            ged$( 7%) = "Sandwich =XXXXXX    "
            ged$( 8%) = "Spacer D.=XXXXXXXXXX"
            ged$( 9%) = "No. Lits =X         "
            ged$(10%) = "Muttin Cd=XXXXXXXX  "

        return

        lookup_dept
           init(" ") readkey$ : dept% = 0%
           str(readkey$,1%,9%)   = "PLAN DEPT"
           str(readkey$,10%,15%) = scr_dept$
           read #3,key = readkey$, using L52330, scr_msg1$,eod goto L52350
L52330:        FMT POS(25), CH(30)
           dept% = 1%
L52350: return

        lookup_shift
           init(" ") readkey$ : shft% = 0%
           str(readkey$,1%,9%)   = "PLAN SHFT"
           str(readkey$,10%,15%) = scr_shft$
          read #3,key = readkey$, using L52420, scr_shft_d$,eod goto L52440
L52420:        FMT POS(25), CH(30)
           shft% = 1%
L52440: return

        check_bilco
            g_b$ = "0"                                   /* GED Only   */
            if ged_bilco$ = "2" then g_b$ = "1"          /* Bilco Only */
            if ged_bilco$ <>  "0" then return            /* Both       */
            for i% = 1% to mod%(9%)
                if dt_dept$ = mod$(9%,i%) then goto L52540
            next i%
        return
L52540:     g_b$ = "1"                                   /* Bilco Only */
        return

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53060,         /* Gegin/End Prod Date   */ ~
                              L53490          /* Report Selection      */
            return

L53060: REM Production Date                       BG_DATE$, BG_DTE$
           if bg_date$ <> " " then goto L53100
              bg_date$ = date$                                                  /* (Y2K, LDJ) */

L53100:    date% = 0%
           call "DATEOKC" (bg_date$, date%, errormsg$ )                         /* (Y2K, LDJ) */            
           if errormsg$ <> " " then return
              bg_dte$ = bg_date$
              call "DATUFMTC" (bg_dte$)                                         /* (Y2K, LDJ) */
        REM Production Date                       ED_DATE$, ED_DTE$
           date% = 0%
           if ed_date$ <> " " then goto L53270
              ed_date$ = bg_date$

L53270:    call "DATEOKC" (ed_date$, date%, errormsg$ )                         /* (Y2K, LDJ) */
           if errormsg$ <> " " then return
              ed_dte$ = ed_date$
              call "DATUFMTC" (ed_dte$)                                         /* (Y2K, LDJ) */
              if ed_dte$ < bg_dte$ then goto L53440
        return
           errormsg$ = "(Error) No Data for Specified Production Date?"
           gosub error_prompt
           init(" ") bg_date$, bg_dte$, ed_date$, ed_dte$
        return
L53440:    errormsg$ = "(Error) Invalid Ending Production Date?      "
           gosub error_prompt
           init(" ") bg_date$, bg_dte$, ed_date$, ed_dte$
        return

L53490: REM Report Selection                        RP_SEL$, RP_SEL_D$
            init(" ") rp_sel_d$
            if rp_sel$ <> " " then goto L53540
               rp_sel$ = "1"

L53540:     if rp_sel$ = "1" then rp_sel_d$ = "Scanned Glass Re-Make's  "
            if rp_sel$ = "2" then rp_sel_d$ = "Scheduled Glass Re-Make's"
            if rp_sel$ = "3" then rp_sel_d$ = "Scheduled Glass          "
            if rp_sel$ = "4" then rp_sel_d$ = "Completed Glass          "
            if rp_sel$ = "5" then rp_sel_d$ = "Completed Glass Re-Make's"
            if len(rp_sel_d$) < 5 then goto L53610
        return
L53610:    errormsg$ = "(Error) Invalid Report Selection (1 Thru 5)? "
           gosub error_prompt
           init(" ") rp_sel$, rp_sel_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~--------------+
L55060: %!---------------------------------------------------------------~
        ~--------------!
L55080: %!######## @ ########        #########################           ~
        ~    Page: ### !
L55100: %!Begin Date: ##########                                         ~
        ~              !
                                                                                /* (Y2K, LDJ) */
L55120: %!End Date  : ##########  ##############################         ~
        ~              !
                                                                                /* (Y2K, LDJ) */
        REM - Detail
L55150: %! Barcode !Dpt!SeqNo!Mod!Load !Gl! Grid   !Width  !Height! S. O.~
        ~  !Reason Desc!
L55170: %!#########!###!#####!###!#####!##!########!#######!######!######~
        ~##!###########!
L55190: %!---------!---!-----!---!-----!--!--------!-------!------!------~
        ~--!-----------!

L55220: %!Report Totals : ##########                                     ~
        ~              !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        lookup_so
            if str(rm_num$,3%,1%) <> "0" then return  /* Skip Re-Makes */
            init(" ") or_hows$
            read #14,key 4% = so$, using L60021, or_hows$, eod goto L60060
L60021:        FMT POS(92), CH(2)
                                                 /* Set for Backorders */
            if or_hows$ = "21" then str(rm_num$,1%,1%)= "9"
                                                 /* Set for UPS S.O.'s */
            if or_hows$ = "01" or or_hows$ = "02" or or_hows$ = "03" or  ~
               or_hows$ = "11" or or_hows$ = "25" then                   ~
               str(rm_num$,1%,1%) = "0"
            if or_hows$ = "04" or or_hows$ = "06" then rm_num$ = "SAM"
            if or_hows$ = "05" then rm_num$ = "DIS"
            if str(lab_rec$,69%,1%) = "4" then rm_num$ = "TSO"
            if str(lab_rec$,69%,1%) = "5" then rm_num$ = "BSO"
            if str(lab_rec$,69%,1%) = "6" then rm_num$ = "FGO"
                                            /* (EWD005) - Indy Labels */
            if or_hows$ = "20" then rm_num$ = "IND"
                                            /* (EWD005)               */ 
        return
L60060:     or_hows$ = "EE"
        return

        create_rmk_data                    /* (EWD006) for Labels      */
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            init(" ") ff$, scr_dte$     
            init(hex(00)) rm_key$
                                           /* Set All Dates to todays  */
            glass_dte$ = date$             /* Date for re-make runs    */
            scr_dte$   = date$
            scr_dte1$  = scr_dte$          /* Production Date is 'Now' */
            call "DATUNFMT" (scr_dte1$)

            str(ff$,1%,2%) = str(date$,1%,2%)
            str(ff$,3%,2%) = str(date$,4%,2%)
            str(ff$,5%,4%) = "@RMK"
            if scr_sel$ = "4" then str(ff$,5%,4%) = "@RM1" /* (EWD006) */
            if scr_sel$ = "5" then str(ff$,5%,4%) = "@RM2" /* (EWD006) */

            rm_flag% = 1%                  /* (EWD005) Set for Re-makes*/ 
            ged_cnt% = 0%
            str(rm_key$,1%,1%) = "0"       /* Only Status '0' Remakes  */
        create_rmk_nxt
            read #8,key 3% > rm_key$, using L60123, lab_ged$, lab_rec$,  ~
                                                eod goto create_rmk_done
L60123:        FMT CH(66), CH(190)
            rm_key$    = str(lab_ged$,13%,21%)
            rm_st$     = str(rm_key$,1%,1%)
            rm_reason$ = str(lab_ged$,34%,2%)
            if rm_st$ <> "0" then goto create_rmk_done
                                                /* (EWD005) Selections */
               rm_reason% = 0% : reason_flag% = 0% /* Default Production*/
               convert rm_reason$ to rm_reason%, data goto L60125
L60125:
               if rm_reason% > 25% and rm_reason% < 31% then             ~
                                                       reason_flag% = 1% 
               if scr_sel$ = "3" then goto L60130
                  if scr_sel$ = "4" and reason_flag% = 0% then goto L60130 
                     if scr_sel$ = "5" and reason_flag% = 1% then goto L60130
                        goto create_rmk_nxt

L60130:     gosub remake_key
                                                 /* Note (APCPLNGR) is */
                                                 /* not changed at all */
                                                 /* (EWD005) - Done    */ 
            write #6, using L60123, lab_ged$, lab_rec$, eod goto L60156
            goto create_rmk_nxt
        create_rmk_done
            if ged_cnt% = 0% then gosub prompt_no_data
        return
L60156:     call "SHOSTAT" ("Err Remake Update Key ---> "& rm_key$)
               stop
            call "SHOSTAT" ("Sales Order No. -> "&str(lab_rec$,97%,8%))
               stop
            close ws
        return

        remake_key                           /* (EWD005) Build Glass   */
            ged_cnt% = ged_cnt% + 1%         /* work Record            */
            convert ged_cnt% to ged_cnt$, pic(#####)

            str(lab_ged$,1%,2%)  = "00"      /* Production Sort Code   */
            if reason_flag% = 1 then str(lab_ged$,1%,2%) = "01"
                                             /* In-House Sort Code at  */
                                             /* then end for both      */
            str(lab_ged$,3%,6%)  = str(lab_rec$,156%,6%) /* Spacer Thil*/

            str(lab_ged$,9%,5%)  = "00000"   /* Seq No. not Applicable */
            str(lab_ged$,14%,3%) = str(lab_rec$,6%,3%)   /* Model Code */
            str(lab_ged$,17%,5%) = "00000"   /* Seq No. not Applicable */
            str(lab_ged$,22%,7%) = str(lab_rec$,84%,7%)  /* Window Wid */
            str(lab_ged$,29%,6%) = str(lab_rec$,91%,6%)  /* Window High*/
            str(lab_ged$,35%,2%) = str(lab_rec$,11%,2%)  /* Glass Type */
            str(lab_ged$,37%,1%) = str(lab_rec$,9%,1%)   /* Color Code */
            str(lab_ged$,38%,1%) = "0"                   /* 0=TOP,1=BOT*/
            if str(lab_rec$,45%,1%) = "B" then str(lab_ged$,38%,1%) = "1"
            str(lab_ged$,39%,6%) = str(lab_rec$,162%,6%) /* Overall Thi*/
            str(lab_ged$,45%,8%) = str(lab_rec$,168%,8%) /* Muttin     */
            str(lab_ged$,53%,9%) = str(rm_key$,10%,9%)   /* Glass Bar  */
            str(lab_ged$,62%,5%) = ged_cnt$              /* Record Cnt */
            dt_dept$ = str(lab_rec$,183%,3%)
            gosub check_bilco
            if ged_bilco$ = "0" then g_b$ ="2"/* Set for Both Bilco/GED*/
            str(lab_rec$,189%,1%) = g_b$      /* Re-Set Bidge Flag     */
            str(lab_rec$,190%,1%) = " "       /* Clear INDY Flag       */
        return                                /* if it is set          */ 

        create_data                           /* Create Glass Cut Data */
            call "SHOSTAT" ("Calculating Glass Cuts")
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work

            ged_cnt% = 0%
            rm_num$  = "000"                  /* (EWD005) All Glass    */
                                              /* Starts at '000'       */ 
            rm_flag% = 0%                     /* (EWD005) Pass to Subs */ 
            init(" ") filler$, sav_mod$       /* '0%' = Mot Re-make Run*/
            dt_key1$ = all(hex(00))
            str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)    /* Prod. Date */
            read #2,key 1% > dt_key1$, using L60330, dt_rec$,             ~
                                                     eod goto create_next
            goto L60339
        create_next
            if scr_sel% <> 1% then goto L60327      /* Screen Explosion */
               if ct% <> 0% then return
                  errormsg$ = "(Error) - Unable to Calculate Glass?"
                  gosub error_prompt
                  return

L60327:     read #2, using L60330, dt_rec$, eod goto create_done
L60330:        FMT CH(256)
            if str(dt_rec$,42%,3%) = "009" or str(dt_rec$,42%,3%) = "046"~
                              then goto create_next /* BAY/BOW & MULLS */
L60339:     gosub dataload
            if dt_date$ <> str(scr_dte1$,1%,6%) then goto create_done
            if len(dt_part$) < 19 then goto create_next
            if dt_prt$ = "Y" then goto create_next     /* Skip Parts   */
            if dt_gls$ = "N" then goto create_next     /* No Glass N/A */
            if scr_dept$ = "ALL" then goto L60363       /* All Depart's */
               if dt_dept$ <> scr_dept$ then goto create_next
               goto L60369
L60363:     gosub check_support
            if supp% = 1% then goto create_next        /* Skip Support */
L60369:     if dt_dept$ = "102" or dt_dept$ = "104" then goto create_next
            if scr_shft$ = "AA" then goto L60378        /* All Shifts   */
               if scr_shft$ <> dt_shft$ then goto create_next
L60378:     if str(scr_load$,1%,3%) = "ALL" then goto L60387 /*All Loads*/
               if dt_load$ <> scr_load$ then goto create_next
               goto calc_data
L60387:     if str(dt_load$,1%,1%) = "A" then goto create_next
        calc_data
            if scr_sel% = 1% then                                       ~
                            call "SHOSTAT" ("Calculating Glass Cuts")
            init(" ") view$
            t1% = 1% : ct% = 0%                        /* No. of Tops  */
            b1% = 1%                                   /* No. of Bots  */
            gosub check_thickness
            for i% = 1% to mod%(4%)            /* No Glass Required    */
              if mod$(4%,i%) = model$ then goto create_next
            next i%
            if dt_dept$ <> "008" then goto L60438  /*Check Casement Dept*/
               for i% = 1% to mod%(5%)         /* Check Valid 800's */
                   if mod$(5%,i%) = model$ then goto L60435
               next i%
               goto create_next
L60435:       gosub case_glass                    /*Multiple Lits 800's*/
L60438:     gosub convert_fields                  /* Top Glass 1st     */
            gosub check_patio                     /* Number of Pannels */
            if sc$ = "5" then t1% = 0%            /* No Top Glass      */
                                                  /* Process TOP Glass */ 
            if t1% = 0% then goto L60516          /*No Top Glass Needed*/
               view$ = "TOP" :  cal% = 1%         /* Process All Top   */
               gosub lookup_phantom               /* Glass             */
               ct% = 0%                           /* Init Cut Counter  */
               for k% = 1% to t1%
                   ct% = ct% + 1%
                   gosub update_work
               next k%                            /* Now See IF Top    */
               for i% = 1% to mod%(2%)            /* Glass Only        */
                   if mod$(2%,i%) = model$ then goto create_next
               next i%
                                              /* Special-Top Sash Only */
               if sc$ = "4" or sc$ ="6" then goto create_next
                                                  /* Process BOT Glass */
L60516:        if b1% = 0% then goto create_next  /* No Bottom Glass   */
                  view$ = "BOT" : cal% = 1%       /* Process Bottom Gls*/
                  if sc$ = "5" then goto L60534
                     for i% = 1% to mod%(3%)   /* Check Two Bottoms */
                         if mod$(3%,i%) = model$ then b1% = b1% + 1%
                     next i%
L60534:           if model$ <> "312" then goto L60540 /* TWO (2) BOTTOMS*/
                     if hg$ = "42" or hg$ = "37" then b1% = 2%
L60540:           gosub lookup_phantom
                  ct% = 5%
                  for k% = 1% to b1%        /* Multiple Bottoms Gls */
                      ct% = ct% + 1%
                      gosub update_work
                  next k%
            goto create_next
        create_done
            if ged_cnt% = 0% then gosub prompt_no_data
        return

        update_work
            init(" ") lab_ged$, lab_rec$
            ged_cnt% = ged_cnt% + 1%
            convert ged_cnt% to ged_cnt$, pic(#####)
            if err% <> 0% then goto create_error_rec
            gosub calc_wd                     /* Ctr line Meeting Rail*/
            gosub calc_wd_ht      /* Calculated Width/Height Long Form */
            gosub build_ged_key
            gosub check_stock               /* Check to see If Stock   */

            if err% <> 0% then goto create_error_rec
            if scr_sel% <> 1% then goto L60657
               str(cut$(ct%),14%,9%) = wd1$              /* For Screen */
               str(cut$(ct%),32%,8%) = ht1$
               str(cut$(ct%),46%,9%) = wd2$
               if ct% <> 1% then goto L60636
                  str(ged$(1%),11%,6%) = t_k$
                  str(ged$(2%),11%,10%)= sandwich$
                  str(ged$(3%),11%,10%)= spacer$
                  str(ged$(4%),11%,1%) = lits$
                  str(ged$(5%),11%,8%) = muttin$
L60636:        if ct% <> 6% then goto L60654
                  str(ged$(6%),11%,6%) = t_k$
                  str(ged$(7%),11%,10%)= sandwich$
                  str(ged$(8%),11%,10%)= spacer$
                  str(ged$(9%),11%,1%) = lits$
                  str(ged$(10%),11%,8%)= muttin$
L60654:        return
L60657:     gosub update_record                  /* ONLY VALID LABELS */
        return

        create_error_rec
            if scr_sel% <> 1% then goto L60678
               errormsg$ = t_err$(err%)
               return
L60678:     str(lab_rec$,1%,1%)  = "E"
            str(lab_rec$,2%,5%)  = "99999"
            str(lab_rec$,7%,25%) = dt_part$
            convert err% to str(lab_rec$,32%,1%), pic(#)

            call "SHOSTAT" ("Error = " & str(lab_rec$,32%,1%) )
            stop

            str(lab_ged$,1%,2%)  = "51"             /* Error Sort Code */
            str(lab_ged$,3%,6%)  = "ERROR*"
            str(lab_ged$,9%,5%)  = "*" & model$ & "*"
            str(lab_ged$,14%,2%) = ty$
            str(lab_ged$,16%,1%) = cl$
            str(lab_ged$,17%,5%) = dt_seq$
            str(lab_ged$,22%,5%) = ged_cnt$
            str(lab_ged$,27%,8%) = so$
            str(lab_ged$,35%,3%) = dt_dept$
            str(lab_ged$,38%,25%)= " "
            init(" ") wd1$, ht1$, wd2$
            goto L60927                             /* UPDATE BUFFER    */

        update_record
            gosub check_bilco                 /* Check for Bridge File */
            str(lab_rec$,1%,5%)   = dt_load$            /* Load No.    */
            str(lab_rec$,6%,3%)   = model$              /* Product Code*/
            str(lab_rec$,9%,2%)   = cl$ & " "           /* Color Code  */
            str(lab_rec$,11%,2%)  = ty_s$               /* Glass Type  */
            if str(view$,1%,1%)   = "T" then            /* Liting Descr*/~
                                       str(lab_rec$,13%,6%) = l_lt$      ~
                                  else str(lab_rec$,13%,6%) = r_lt$
            str(lab_rec$,19%,9%)  = wd1$                /* Calc Width  */
            str(lab_rec$,28%,8%)  = ht1$                /* Calc Height */
            str(lab_rec$,36%,9%)  = wd2$                /* CLMR Width  */
            str(lab_rec$,45%,1%)  = str(view$,1%,1%)    /* Top/Bot     */
            str(lab_rec$,46%,4%)  = dt_txt$
            str(lab_rec$,50%,9%)  = lab_fil$
            str(lab_rec$,59%,25%) = dt_part$
            str(lab_rec$,84%,7%)  = wd$                 /* WINDOW WD   */
            str(lab_rec$,91%,6%)  = ht$                 /* WINDOW HT   */

            str(lab_rec$,97%,8%)   = so$          /*Save Sales Order No*/
            str(lab_rec$,105%,10%) = space_d$     /* Store Spacer Desc */
            str(lab_rec$,115%,10%) = sandwich$    /* Glass Sandwich Typ*/
            str(lab_rec$,125%,8%)  = width_d$     /* Calc Width Decimal*/
            str(lab_rec$,133%,8%)  = height_d$    /* Calc Height Decim */
            str(lab_rec$,141%,6%)  = w_adj$       /* Width Adjustment  */
            str(lab_rec$,147%,6%)  = h_adj$       /* Height Adjustment */
            str(lab_rec$,153%,1%)  = temp$        /* Tempered Flag '*' */
        REM STR(LAB_REC$,154%,2%)  = SORT$        /**Glass GED Sort Cod*/
            str(lab_rec$,156%,6%)  = spacer$      /**Glass Spacer Size */
            str(lab_rec$,162%,6%)  = t_k$         /**Glass Overall Thic*/
            str(lab_rec$,168%,8%)  = muttin$      /**Glass Muttin Code */
            str(lab_rec$,168%,8%)  = str(lab_ged$,45%,8%)
        REM STR(LAB_REC$,176%,5%)  = DT_SEQ$      /**Dept Seq No.      */
                                                  /* Note - Seq No.    */
                                                  /* could have leading*/
                                                  /* X, Y, or Z Sort   */
            str(lab_rec$,181%,2%)  = dt_shft$     /* Dept Shift Code   */
            str(lab_rec$,183%,3%)  = dt_dept$     /* Department Code   */
            if rm_num$ = "000" then str(rm_num$,1%,1%) = num$
                                         /* Set Production day of week */
            str(lab_rec$,186%,3%)  = rm_num$      /* Remake Number     */
            str(lab_rec$,189%,1%)  = g_b$         /* 0=GED,1=Bilco Only*/
            str(lab_rec$,190%,1%)  = " "          /* Rec Length (190)  */
                                                  /* (EWD005) Indy Flag*/
            if scr_sel$ = "6" then str(lab_rec$,190%,1%) = "1"
                                                  /* Place Spec at End */
            xx$ = str(lab_ged$,1%,2%)
            if xx$ <> "50" and xx$ <> "51" then goto L60879
               ss$ = str(lab_ged$,1%,2%)
               goto L60912
                                                  /* (EWD004) - Begin  */   
L60879:     if skip$ = "*" or temp$ = "*" then goto L60888
               goto L60909

L60888:     if temp$ <> "*" then goto L60900

               str(lab_ged$,1%,2%) = "47"
               if dt_dept$ = "023" then str(lab_ged$,1%,2%) = "44"
               if dt_dept$ = "042" then str(lab_ged$,1%,2%) = "45"
               if dt_dept$ = "043" then str(lab_ged$,1%,2%) = "46"
               str(lab_ged$,9%,1%) = "W"

               goto L60918
      
L60900:        if skip$ = "*" then str(lab_ged$,1%,2%) = "49"
               if skip$ = "*" then str(lab_ged$,9%,1%) = "Y"
               goto L60918
                                                   
L60909:     gosub get_sort
            call "SHOSTAT" ("Sort Code = " & ss$ & " sort = " &sort$)
            stop
 
L60912:     str(lab_ged$,1%,2%) = ss$             /* (EWD004) - End    */
                                           
                                                  /* Final Sort Code & */
L60918:     str(lab_rec$,154%,2%)  = ss$          /* Dept Seq. Mod.    */
            str(lab_rec$,176%,5%)  = str(lab_ged$,9%,5%)

L60927:     write #6, using L60930, lab_ged$, lab_rec$, eod goto L60936
L60930:       FMT CH(66), CH(190)                 /* Rec Len = 256     */
        return
L60936:     call "SHOSTAT" ("Err Write Lab (GED Key) -> "&               ~
                                 str(lab_ged$,1%,30%) ) : stop
            call "SHOSTAT" ("Sales Order No. -> "&str(dt_rec$,24%,8%) )
            stop
            close ws
        return

        convert_fields
            if save_part$ = dt_part$ then return
               save_part$ = dt_part$
            init(" ") model$, cl$, ty$, lt$, hg$, sc$, lk$
            s_width = 0.0 : s_height = 0.0 : s_clmr = 0.0
            model$    = str(dt_part$,1%,3%)            /* Model Number */
            cl$       = str(dt_part$,4%,1%)            /* Color        */
            ty$       = str(dt_part$,5%,2%)            /* Glass        */
            lt$       = str(dt_part$,7%,2%)            /* Liting       */
            hg$       = str(dt_part$,9%,2%)            /* Hinge        */
            sc$       = str(dt_part$,11%,1%)           /* Screen       */
            lk$       = str(dt_part$,12%,1%)           /* Locks        */
            gosub std_wd_ht                        /* WD$ - Width Prt  */
                                                   /* HT$ - Height Prt */
            a1 = 0.0 : a2 = 0.0
            convert str(dt_part$,13%,3%) to a1, data goto L61005
L61005:
            convert str(dt_part$,16%,1%) to a2, data goto L61011
L61011:
            s_width = a1 + (a2/8.0)                /* Decimal Width    */
            a1 = 0.0 : a2 = 0.0
            convert str(dt_part$,17%,2%) to a1, data goto L61023
L61023:
            convert str(dt_part$,19%,1%) to a2, data goto L61029
L61029:
            s_height = a1 + (a2/8.0)               /* Decimal Height   */
            gosub lookup_color
            gosub lookup_glass
            gosub lookup_grid
            gosub lookup_hinge
            gosub lookup_temp
            gosub lookup_double
            a1 = 0.0 : a2 = 0.0
            if len(dt_part$) < 22 then return
               convert str(dt_part$,20%,2%) to a1, data goto L61074

               convert str(dt_part$,22%,1%) to a2, data goto L61068
L61068:
               s_clmr = a1 + (a2/8.0)
L61074:     if scr_sel% = 1% then dt_wood$ = "000"
            if dt_wood$ <> "000" then s_clmr = 0.0
            if s_clmr <= 8.0 then s_clmr = 0.0
        return

        lookup_reason
            init(" ") readkey$, rm_reason_d$
            str(readkey$,1%,9%)   = "PLAN REMK"
            str(readkey$,10%,15%) = rm_reason$
            read #3,key = readkey$, using L61107, rm_reason_d$,           ~
                                                           eod goto L61110
L61107:        FMT POS(25), CH(30)
L61110: return

        check_patio                              /* Note - GLASS01   */
            if model$ <> "312" then goto L61115  /* One Top, One Bot */
               if hg$ <> "42" then goto L61115
                  t1% = 1% : b1% = 2%

L61115:     if model$ <> "313" then goto L61120  /* Two Tops, One Bot*/
               t1% = 2% : b1% = 1%

L61120:     if model$ <> "314" then goto L61125  /* Two Tops, Two Bots*/
               t1% = 2% : b1% = 2%
L61125: return
        lookup_color                                  /* Look Up Color */
            init(" ") descr$, readkey$, cl_l$, cl_s$
            str(readkey$,1%,9%)   = "COLOR    "
            str(readkey$,10%,15%) = cl$
            read #3,key = readkey$,using L61131, descr$,eod goto L61140
L61131:        FMT POS(25), CH(30)
            cl_l$ = str(descr$,6%,6%)                 /* Long Descript */
            cl_s$ = str(descr$,1%,2%)                 /* Short Descript*/
L61140: return
        lookup_glass                                 /* Look Up GLASS  */
            init(" ") readkey$, descr$, ty_s$, ty_l$
            ty% = 0%
            str(readkey$,1%,9%)   = "GLASS    "
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, using L61131, descr$, eod goto L61179
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            ty_l$ = descr$                            /* Long Descript */
            ty_s$ = str(descr$,1%,p%-2%)              /* Short Descript*/
            convert ty$ to ty%, data goto L61179

L61179: return

        lookup_grid                                   /* Look Up Grid  */
            lt% = 0%
            init(" ") descr$, readkey$, l_lt$, r_lt$
            str(readkey$,1%,9%)   = "LITING   "
            str(readkey$,10%,15%) = lt$
            read #3,key = readkey$,using L61131, descr$,eod goto L61221
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            l_lt$ = str(descr$,1%,p%-2%) & "   "  /* LEFT MUTTIN - TOP */
            r_lt$ = str(descr$,p%+2%,6%) & "   "  /* RIGHT MUTTIN - BOT*/
            convert lt$ to lt%,data goto L61221

        return  
L61221:     lt%    = 100%                         /* (EWD009) Alpha's  */
            if lt$ = "A0" then lt% = 101%         /* (EWD009)          */
        return

        lookup_hinge                                  /* Look Up Hinge */
            init(" ") hgl$, hgr$, descr$, readkey$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hg$
            read #3,key = readkey$,using L61131, descr$,eod goto L61263
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            hgl$ = str(descr$,1%,p% - 1%)    /* Left Side Description  */
            hgr$ = str(descr$,p% + 2%)       /* Right Side Description */
            if model$ <> "830" and model$ <> "883" then return
               p1% = pos(descr$ = "/")
               if p1% = 0% then return
               hgl$ = str(descr$,p1%-1%,3%)
L61263: return
        lookup_temp
            init(" ") readkey$, temp$
            str(readkey$,1%,9%) = "PLAN TEMP"
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto L61284
               temp$ = "*"
L61284: return
        lookup_double
            gls_double% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN DBLE"
            str(readkey$,10%,15%) = ty$
            read #3,key = readkey$, eod goto L61308
               gls_double% = 1%
L61308: return

        std_wd_ht /* Convert Standard Width/Height to Fraction in 8'ths*/
                  /* F0% = Width Fraction, F1% = Height Fraction       */
                  /* WD$ = Width & Fraction, HT$ = Height & Fraction   */
           str(wd$,1%,3%) = str(dt_part$,13%,3%)         /* Width  (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(dt_part$,17%,2%)         /* Height (2) */
           f0% = 0% : f1% = 0%                      /* Build Fractions */
           convert str(dt_part$,16%,1%) to f0%,data goto L61350 /*Width */

           convert str(dt_part$,19%,1%) to f1%,data goto L61350 /*Height*/

           goto L61353
L61350:      f0% = 8% : f1% = 8%
L61353:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%
           str(wd$,4%,1%) = " "          /* Build Width with Fraction  */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
        return

        calc_wd_ht /* Convert Calculated Width/Height With Fract. 16'th*/
                   /* WD1$ = Width & Fract(9), HT1$ = Height & Fract(8)*/
           calc = width                          /* Convert Width  (3) */
           gosub convert_sixteen : wd1$ = "         "
           convert a% to str(wd1$,1%,3%), pic(###)

           if b% = 0% then goto L61401            /* Check For Fraction */
              str(wd1$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L61401:    calc = height                         /* Convert Height (2) */
           gosub convert_sixteen : ht1$ = "        "
           convert a% to str(ht1$,1%,2%), pic(##)

           if b% = 0% then goto L61419            /* Check For Fraction */
              str(ht1$,4%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L61419:    init(" ") width_d$, height_d$
           x = round(width,4)
           convert width to width_d$, pic(00#.####)
           x = round(height,4)
           convert height to height_d$, pic(00#.####)
        return
        calc_wd   /* Convert Center Line Meeting Rail to Long Form     */
           init(" ") wd2$                            /* CLMR For Glass */
           if s_clmr < 1 then return
           calc = s_clmr                             /* Decimal CLMR   */
           gosub convert_sixteen
           convert a% to str(wd2$,1%,3%),pic(###)    /* Size (3) CLMR  */

           if b% = 0% then goto L61464                /* Check Fraction */
              str(wd2$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L61464: return

        convert_sixteen
            calc = round( calc, 4 )
            a% = int(calc)
            b% = int((calc - a%) * 10000)
            if b% = 0% then goto L61500                 /****************/
               d% = int(b%/625)                        /* Conversion of*/
               if mod(b%,625) <> 0 then d% = d% + 1%   /* Decimals to  */
                  b% = d%                              /*  Sixteen's   */
                  if b% <> 16% then goto L61500         /****************/
                     a% = a% + 1% : b% = 0%         /* A% = WHOLE PART */
L61500: return

        lookup_phantom                       /* Special Code for Model */
           err% = 0%
           if view$ <> "TOP" then goto L61533 /* 830, 883               */
              phantom$ = "2008"
              if str(hgl$,1%,2%) = "OR"  then phantom$ = "2203"
              if str(hgl$,1%,2%) = "CO"  then phantom$ = "2103"
              if str(hgl$,1%,3%) = "1/4" then phantom$ = "2008"
              if str(hgl$,1%,3%) = "1/3" then phantom$ = "2108"
              goto L61548
L61533:    phantom$ = "3008"
           if str(hgl$,1%,2%) = "OR"  then phantom$ = "3203"
           if str(hgl$,1%,2%) = "CO"  then phantom$ = "3103"
           if str(hgl$,1%,3%) = "1/4" then phantom$ = "3008"
           if str(hgl$,1%,3%) = "1/3" then phantom$ = "3108"
L61548: REM                       /* CAL% 1%=W/H, 2%=W Only, 3%=H Only */
            call "APCCALSB" (cal%,                 /* Calc Type 1%,2%,3*/~
                             dt_part$,             /* Part Number      */~
                             phantom$,             /* Phantom Designato*/~
                             width,                /* Exact width      */~
                             height,               /* Exact Height     */~
                             #1,                   /* AMTBOMCD Equation*/~
                             err% )                /* Error Code 0%-Ok */
            if err% <> 0% then goto L61584
            if str(hgl$,1%,2%) = "CO" or str(hgl$,1%,2%) = "OR" then     ~
                                            gosub calc_clmr_1
        return
L61584:     err% = 1%                              /* Equation Error   */
        return

        calc_clmr_1                            /* Both - Cottage/Oriel */
          if s_clmr < 1.0 then return
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = "GLASS10  "
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$, using L61600, descr$,                ~
                                                          eod goto L61620
L61600:        FMT POS(25), CH(30)
            t_clmr = 0.0 : b_clmr = 0.0 : tb_clmr = 0.0
            convert str(descr$,1%,8%) to t_clmr, data goto L61625

            convert str(descr$,12%,8%) to b_clmr, data goto L61625

            convert str(descr$,22%,8%) to tb_clmr, data goto L61615
L61615
            if view$ = "TOP" then                           /* GLASS   */~
               height =((s_height/2.0) - t_clmr)                         ~
                                  - (((s_height/2.0) + tb_clmr) - s_clmr)
            if view$ = "BOT" then                           /* GLASS   */~
                height =((s_height/2.0) - b_clmr)                        ~
                                  + (((s_height/2.0) + tb_clmr) - s_clmr)
L61620: return
L61625:      err% = 3%
        return

        check_stock                    /* Check Stock Clear Glass Only */
           init(" ") stock$, lab_fil$
           if ty$ <> "01" then return  /* (EWD002) - 06/18/98          */
           for i% = 1% to stk_max%
               if ws$(i%) = wd1$ and hs$(i%) = ht1$ then goto L61800
           next i%
        return
L61800:    stock$, str(lab_fil$,1%,5%) = "STOCK"
           if ty$ = "01" then sandwich$ = "IG3CLS3CLS"
           if ty$ = "04" then sandwich$ = "IG3CLS3LE"
           if ty$ = "05" then sandwich$ = "IG3CLS3LEA"
        REM  SPACER$  = ".99999    "
        REM  SPACE_D$ = "SP20      "
        REM  STR(LAB_GED$,1%,2%) = "50"
        REM  STR(LAB_GED$,3%,6%) = ".99999"
        return

        create_batch                           /* Create Glass Batches */
            gosub batch_header
            if keyhit% = 1% then return
            if keyhit% = 16% then return
            if keyhit% = 14% then goto L61848
               goto create_batch
L61848:     if scr_sel% = 0% then return
               if ged_bilco$ <> "0" and ged_bilco$ <> "1" then goto L61896
               call "APCPLB45" (size%,    /* Specified Batch Size      */~
                               scr_sel$,  /* Screen Selection (EWD006) */~
                               glass_dte$,/* Glass Production Date     */~
                               scr_dte$,  /* Planned Production Date   */~
                               scr_dte1$, /* Planned Production Unforma*/~
                               file$,     /* Name of Optimized File    */~
                               bat$,      /* Number of Batches Created */~
                               rm_flag%,  /* Remake Flag               */~
                               #3,        /* (GENCODES) TABLES         */~
                               #6,        /* (APCPLNWK) Label Detail Fl*/~
                               #8,        /* (APCPLNGR) Remake Glass   */~
                               #10,       /* (@GLSGED@) Optimizer File */~
                               #21,       /* (@RMKGDK@) Both (EWD006)  */~ 
                               #12,       /* (@RMKGD1@) Production     */~
                               #16 )      /* (@RMKGD2@) In House       */
                                          /* (EWD001) - Mod 06/04/98   */
               if ged_bilco$ = "1" then return
                                          /* ( Bilco Glass Cutter )    */
L61896:        call "APCPLC45" (size%,    /* Specified Batch Size      */~
                               scr_sel$,  /* Screen Selection (EWD006) */~
                               glass_dte$,/* Glass Production Date     */~
                               scr_dte$,  /* Planned Production Date   */~
                               scr_dte1$, /* Planned Production Unforma*/~
                               file$,     /* Name of Optimized File    */~
                               bat$,      /* Number of Batches Created */~
                               rm_flag%,  /* Remake Flag               */~
                               #6,        /* (APCPLNWK) Label Detail Fl*/~
                               #8,        /* (APCPLNGR) Remake Glass   */~
                               #11,       /* (@GLSBIL@) Optimizer File */~
                               #22,       /* (@RMKBLK@) Both (EWD006)  */~
                               #13,       /* (@RMKBL1@) Production     */~
                               #3,        /* (GENCODES) TABLES         */~
                               #17)       /* (@RMKBL2@) In House       */
                                          /* (EWD001) - Mod 06/04/98   */
        return

        load_stock
            init(" ") ws$(), hs$()
            readkey$ = all(hex(00)) : stk_max% = 0%
            str(readkey$,1%,9%) = "GLASS GED"
            read #3,key > readkey$, using L61965, readkey$, stk_desc$,    ~
                                                 eod goto load_done
            goto L61968
        load_next
            read #3, using L61965, readkey$,stk_desc$, eod goto load_done
L61965:         FMT CH(24), CH(30)
L61968:     if str(readkey$,1%,9%) <> "GLASS GED" then goto load_done
               stk_max% = stk_max% + 1%
               ws$(stk_max%) = str(stk_desc$,3%,9%)
               hs$(stk_max%) = str(stk_desc$,15%,8%)
               goto load_next
        load_done
        return

        case_glass
            sc$ = str(dt_part$,11%,1%)
            str(readkey$,1%,9%)   = "GLASS01  "
            str(readkey$,10%,3%)  = model$
            str(readkey$,13%,12%) = str(dt_part$,9%,2%)
            read #3,key = readkey$, using L61131, descr$, eod goto L62031
               convert str(descr$,1%,2%) to t1%, data goto L62013
L62013:
               convert str(descr$,4%,2%) to b1%, data goto L62019
L62019:
            if sc$ <> "4" and sc$ <> "5" and sc$ <> "6" then return
               t1% = 1%
               b1% = 1%
L62031: return

        load_tables
           call "SHOSTAT" ("Loading Glass Tables")
           mat mod% = zer
           init(" ") mod$()
           for i% = 1% to tab_max%
               readkey$ = " "
               str(readkey$,1%,9%)  = tab$(i%)
L62058:        read #3,key > readkey$,using L62061,readkey$,eod goto L62076
L62061:           FMT CH(24)
               if tab$(i%) <> str(readkey$,1%,9%) then goto L62076
                  mod%(i%) = mod%(i%) + 1%
                  mod$(i%,mod%(i%)) = str(readkey$,10%,3%)
                  goto L62058
L62076:    next i%
        return

        check_thickness                            /* Thickness/Spacer */
           init(" ") readkey$, t_k$, s_s$, s_d$
           str(readkey$,1%,9%)   = "GED 002  "
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, using L62100, descr$, eod goto L62115
L62100:       FMT POS(25), CH(30)
           t_k$ = str(descr$,1%,6%)          /* Thickness of Spacer    */
           s_s$ = str(descr$,9%,6%)          /* Single Strength Spacer */
           s_d$ = str(descr$,17%,6%)         /* Double Strength Spacer */
        return
L62115:    t_k$="ERR-01" : s_s$="ERR-01" : s_d$="ERR-01" : err%=4%
        return

        build_ged_key                            /* NOTE - 'GLS SORT'  */
            gosub calc_double                    /*  TABLE NOT USED    */
            gosub check_ged_glass
            spacer$ = s_s$                       /* Single Strength    */
            if double% = 1% then spacer$ = s_d$  /* Double Strength    */
                                                 /* (EWD009) - Begin   */
                                                 /* Related to Liting  */
            init(" ") lab_ged$, skip$            /* Code Values        */
            if (lt%-93%) > 0% then skip$ = "*"
                                                 /* (EWD003) - 07/27/98*/
            if lt% > 82% and lt% < 89% then skip$ = "*"
                                                 /* (EWD009) - End     */

            str(lab_ged$,1%,1%)  = "0"           /* SPECIAL SORT FLAG  */
            str(lab_ged$,2%,1%)  = "0"           /* TOP/BOT FLAG       */
            str(lab_ged$,3%,6%)  = spacer$       /* Spacer Thickness   */
            str(lab_ged$,9%,5%)  = dt_seq$       /* Glass Sort Code    */
        REM (Begin) Floating Sort Area - ( 14 Thru 38 )
            str(lab_ged$,14%,3%) = model$        /* Model Code         */
            str(lab_ged$,17%,5%) = dt_seq$       /* Sequence Number    */
            str(lab_ged$,22%,7%) = wd$           /* Window Width       */
            str(lab_ged$,29%,6%) = ht$           /* Window Height      */
            str(lab_ged$,35%,2%) = ty$           /* Glass Type Code    */
            str(lab_ged$,37%,1%) = cl$           /* Color Code         */
            str(lab_ged$,38%,1%) = "0"           /* 0 = TOP, 1 = BOT   */
            if str(view$,1%,1%) = "B" then str(lab_ged$,38%,1%) = "1"
            if str(model$,1%,1%) = "3" then gosub chg_sort1
        REM (End) of Floating Sort Area
            str(lab_ged$,39%,6%) = t_k$          /* Overall Thickness  */
            so$ = str(dt_bar$,1%,8%)
            gosub get_spacer_desc                /* SPACE_D$ Descript  */
            gosub get_ged_adjust                 /* W_ADJ, H_ADJ       */
            gosub get_muttin                     /* MUTTIN$, LITS$,    */
                                                 /* VERT%, HORZ%       */
            gosub check_ged_special              /* Check Spec. Shapes */
            str(rm_bar$,1%,8%) = dt_ref$       /* 0-4 = TOP, 5-9 = BOT */
            convert (ct% -1%) to str(rm_bar$,9%,1%), pic(#)

            str(lab_ged$,45%,8%) = muttin$       /* Vertical/Horizontal*/
            str(lab_ged$,53%,9%) = rm_bar$       /* Record Counter     */
            str(lab_ged$,62%,5%) = ged_cnt$      /* RECORD COUNTER ??  */
                                            /* CORRECTION FOR 'CO' AND */
                                            /* OB GLASS TYPES          */
                                            /* (EWD011) - 04/27/99     */
            if ty$ <> "02" and ty$ <> "14" and ty$ <> "E7" then return
               if str(view$,1%,1%) <> "B" then return
                  if ty$ = "02" then sandwich$ = "IG3OB3CL"
                  if ty$ = "14" then sandwich$ = "IG3CL3CL"
                  if ty$ = "E7" then sandwich$ = "IG3TP3CL" 
        return
                                            /* (EWD011) - 04/27/99     */
        chg_sort1                          /* SPECIAL SORT VINYL PATIO */
            str(lab_ged$,14%,3%) = model$        /* Model Code         */
            str(lab_ged$,17%,7%) = wd$           /* Window Width       */
            str(lab_ged$,24%,6%) = ht$           /* Window Height      */
            str(lab_ged$,30%,2%) = ty$           /* Glass Type Code    */
            str(lab_ged$,32%,1%) = cl$           /* COLOR              */
            str(lab_ged$,33%,1%) = "0"           /* 0 = TOP, 1 = BOT   */
            if str(view$,1%,1%) = "B" then str(lab_ged$,33%,1%) = "1"
            str(lab_ged$,34%,5%) = dt_seq$       /* Sequence Number    */
        return

        calc_double                      /* Calc Double Strength Glass */
          double% = 0%
          x = 0.0 : y = 0.0 : z = 0.0
          if gls_double% = 1% then goto L62346

          convert str(wd1$,1%,3%) to x, data goto L62313
L62313:
          convert str(ht1$,1%,2%) to y, data goto L62319
L62319:
          if str(wd1$,7%,1%) = "/" then x = x + 1.0
          if str(ht1$,6%,1%) = "/" then y = y + 1.0
          z = (x * y)/ 144.0
          if z > 20.0 then goto L62346               /* Square Feet     */
          if x > 60.0 then goto L62346               /* Width Greater   */
          if y > 60.0 then goto L62346               /* Height Greater  */
          if (x + y) > 100.0 then goto L62346        /* Tot United Inch */
        return
L62346:   double% = 1%
        return

        check_ged_glass                            /* Thickness/Spacer */
           init(" ") readkey$, sandwich$, tty$
           tty$ = ty$ : tty% = 0%
           convert tty$ to tty%, data goto L62367
L62367:  
           if double% = 0% then goto L62388
              if tty% = 0% then goto L62388
                 if tty% > 50% then goto L62388
                    tty% = tty% + 50%
                    convert tty% to tty$, pic(00)

L62388:    str(readkey$,1%,9%)   = "GED 001  "
           str(readkey$,10%,15%) = tty$
           read #3,key = readkey$, using L62397, desc$, eod goto L62412
L62397:       FMT POS(25), CH(30)
           sandwich$ = str(desc$,1%,10%)
           tty$ = ty$
           if tty% = 89% then temp$ = "*" /* SPECIAL GLASS AT END */
        return
L62412:   sandwich$ = "ERR-02 ***" : err% = 5%
        return

        get_spacer_desc                      /* Get Spacer Description */
           init(" ") space_d$, readkey$
           str(readkey$,1%,9%)   = "GED 000  "
           str(readkey$,10%,15%) = str(spacer$,2%,5%)  /* Skip Decimal */
           read #3,key = readkey$, using L62436, space_d$, eod goto L62442
L62436:       FMT POS(25), CH(10)
        return
L62442:    space_d$ = "ERR-03 ***" : err% = 6%
        return

        get_ged_adjust                       /* Get Width/Height Adj   */
           w_adj, h_adj = 0.0                          /*-O = No Adj.  */
           init(" ") w_adj$, h_adj$, readkey$          /*-T = Top Adj. */
           str(readkey$,1%,9%)   = "GED 004  "         /*-B = Bot Adj. */
           str(readkey$,10%,15%) = model$ & "-" & str(view$,1%,1%)
           read #3,key = readkey$, using L62469, desc$, eod goto L62499
L62469:       FMT POS(25), CH(30)
                                                   /* Adjustment Found */
              w_adj$ = str(desc$,1%,6%) : h_adj$ = str(desc$,8%,6%)
              convert w_adj$ to w_adj, data goto L62481
L62481:
              convert h_adj$ to h_adj, data goto L62487

L62487:    if w_adj < 0.0 then convert w_adj to w_adj$, pic(-0.###)      ~
                          else convert w_adj to w_adj$, pic(0.####)
           if h_adj < 0.0 then convert h_adj to h_adj$, pic(-0.###)      ~
                          else convert h_adj to h_adj$, pic(0.####)
L62499: return

        get_muttin
            vert% = 0% : horz% = 0% : er% = 0%
            if str(dt_part$,7%,2%) <> "00" then goto L62523
               muttin$ = "        " : lits$ = "0"
               return

L62523:     call "APCGSLIT" ( dt_part$,         /* MFG Part Number     */~
                              muttin$,          /* Grid Vert/Horiz Code*/~
                              lits$,            /* No. of Lits         */~
                              str(view$,1%,1%), /* T or B              */~
                              vert%,            /* Number of Verticals */~
                              horz%,            /* Number of Horizontal*/~
                              #3,               /* (GENCODES)          */~
                              er% )             /* Error Code          */
            if er% = 0% then return
               if er% = 1% then err% = 7%       /* GED Lits Error      */
               if er% = 2% then err% = 8%       /* GED Hinge Error     */
               if er% = 3% then err% = 9%       /* GED Muttin Error    */
        return

        check_ged_special
            for i% = 1% to mod%(8%)
                if model$ <> mod$(8%,i%) then goto L62586
                   str(lab_ged$,1%,2%) = "45"      /* Put at the End   */
                   if str(model$,1%,1%) = "7" then                       ~
                                              str(lab_ged$,1%,2%) = "46"
                   i% = mod%(8%) + 1%              /* Exit Loop        */
L62586:     next i%
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#6,mode$, 500%, f2%)
            if f2% <> 0% then goto L62619
        return
L62619:     call "SHOSTAT" ("Error - Cannot Open (APCPLNWK)") : stop
        return
        delete_work
            call "FILEBGON" (#6)
        return

        get_sort                                 /* (EWD004) - Begin    */   
            vv$ = str(view$,1%,1%)
            str(sort$,1%,3%) = dt_dept$
            str(sort$,4%,2%) = "SS"
            if double% = 1% then str(sort$,4%,2%) = "DS"
            str(sort$,6%,4%) = str(space_d$,1%,4%)
            str(sort$,10%,2%) = "TB"
            if dt_dept$ <> "048" then goto L62673   /* New Construction */
                                                    /* (EWD013)         */
               if str(model$,1%,1%) <> "7" and str(model$,1%,1%) <> "1"    ~
                                                         then goto L62760
                                                    /* (EWD013)         */
L62664:           str(sort$,10%,2%) = "TT"
                  if vv$ = "B" then str(sort$,10%,2%) = "BB"
                  goto L62760
L62673:     if dt_dept$ = "028" then goto L62664    /* Welded Sash      */
            if dt_dept$ = "036" then goto L62664    /* Vinyl Prime 712  */
            if dt_dept$ <> "023" then goto L62694   /* Vinyl Patio      */
               i% = 1%
               goto L62775

L62694:     if dt_dept$ <> "042" then goto L62760   /* Hinged Patio     */
               i% = 45%
               goto L62775

L62760:     for i% = 1% to ss_max%
                if ss$(i%) = sort$ then goto L62775
            next i%
            i% = 51%

L62775:     convert i% to ss$, pic(00)

        return
                                                 /* (EWD004) - End       */     
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_support
           supp% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #3,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return

        open_label_file
            ff$ = "@royal@"
            open nodisplay #15, output, space = 100%,                    ~
                 dpack = 100%, ipack = 100%, file = ff$,                 ~
                 library = library$, volume = volume$, blocks = 5%
        return

        build_label
            if str(lab_ged$,1%,2%) = "51" then goto error_label
            a1$ = ty$                      /* (02)     Glass Type      */
            a2$ = mut$                     /* (06)     Grid/Liting Vode*/
            a3$ = so$                      /* (08)     S.O. Number     */
            a4$ = wd1$ & " "               /* (09)     Calc_width      */
            a5$ = model$                   /* (03)     Model Code      */
            a6$ = scr_dte$                 /* (08)     Production Date */
            a7$ = ht1$ & "  "              /* (09)     Calc Height     */
            a8$ = dt_shft$                 /* (02)     Shift Code      */
            a9$ = dt_seq$                  /* (05)     Dept Seq No.    */
            a10$= wd$ & "X" & ht$          /* (15)     Growth          */
            a11$= rm_num$                  /* (03)     Remake Counter  */
            a12$= dt_dept$                 /* (03)     Department Code */
            a13$= txt$                     /* (40)     40 Char. Text   */
            a14$= rm_bar$                  /* (09)     Glass Barcode   */
            a15$= view$                    /* (03)     View Top/Bot    */
            a16$= cl_l$                    /* (06)     Window Color    */
        REM A17$= LK$                      /* (01)     Contour Grid    */
                                           /* Record Length = 167 ??   */
        return

        error_label
            call "SHOSTAT" ("err " & lab_ged$)
            stop

            a1$ = "**"                     /* (02)     Glass Type      */
            a2$ = "ERR=  "                 /* (06)     Grid/Liting Vode*/
            str(a2$,6%,1%) = str(lab_rec$,32%,1%)    /* Set Error Code */
            a3$ = str(lab_ged$,27%,8%)     /* (08)     S.O. Number     */
            a4$ = "*********"              /* (09)     Calc_width      */
            a5$ = str(lab_rec$,7%,3%)      /* (03)     Model Code      */
            a6$ = "********"               /* (08)     Production Date */
            a7$ = "*********"              /* (09)     Calc Height     */
            a8$ = "**"                     /* (02)     Shift Code      */
            a9$ = str(lab_ged$,17%,5%)     /* (05)     Dept Seq No.    */
            a10$= "***************"        /* (15)     Growth          */
            a11$= "ERR"                    /* (03)     Remake Counter  */
            a12$= str(lab_ged$,35%,3%)     /* (03)     Department Code */
                                           /* (40)     40 Char. Text   */
            a13$= "MODEL = " & str(lab_rec$,7%,25%) & " ******"
            a14$= "000000000"              /* (09)     Glass Barcode   */
            a15$= "***"                    /* (03)     View Top/Bot    */
            a16$= "ERROR*"                 /* (06)     Window Color    */
            a17$= " "                      /* (01)     Contour Grid    */
                                           /* Record Length = 155 ??   */
        return

        print_labels
            call "SHOSTAT" ("Printing Glass Labels")
                                              /* (EWD012) - New Version */
            if remake_label% = 1% then goto L63000 
               gosub open_label_file
L63000:     init(" ") lab_ged$, lab_rec$
        print_labels_nxt
            read #6,key > lab_ged$, using L63015, lab_ged$, lab_rec$,     ~
                                                 eod goto print_done
L63015:        FMT CH(66), CH(190)
            cl$ = str(lab_rec$,9%,1%)
            gosub lookup_color
            view$ = "TOP"
            if str(lab_rec$,45%,1%) = "B" then view$ = "BOT"
            l_lt$    = str(lab_rec$,13%,6%)    /* Top=Left,Bot=Right   */
            so$      = str(lab_rec$,97%,8%)
            wd1$     = str(lab_rec$,19%,9%)
            dt_dept$ = str(lab_rec$,183%,3%)
            ht1$     = str(lab_rec$,28%,8%)
            dt_shft$ = str(lab_rec$,181%,2%)
            dt_seq$  = str(lab_rec$,176%,5%)
            rm_num$  = str(lab_rec$,186%,3%)
            gosub lookup_so

            model$   = str(lab_rec$,6%,3%)
            dt_txt$  = str(lab_rec$,46%,4%)
            wd$      = str(lab_rec$,84%,7%)
            ht$      = str(lab_rec$,91%,6%)
            lk$      = str(lab_rec$,70%,1%)        /* Test for Contour */
            p% = pos("0123456789" = lk$ )          /* Grid.            */
            a17$ = " "
            if p% = 0% then a17$ = "C"
            nbr_line% = 0%
            init(" ") txt$, text$()
            call "APCPLTXT" (#9, dt_txt$, text$(), nbr_line%)
            txt$     = str(text$(2%),1%,40%)
            rm_bar$  = str(lab_ged$,53%,9%)
            ty$      = str(lab_rec$,11%,2%)
            gosub change_muttin
            gosub build_label
            gosub print_lab
            goto print_labels_nxt
        print_done
            if remake_label% = 1% then return      /* (EWD012)         */ 
               a16$ = "EndEnd"
               gosub print_lab
               gosub print_lab
               close #15
        return

        print_lab
           if remake_label% = 1% then goto L63200  /* (EWD012) -New Ver*/
              put #15,using L35040,"{1``",         /* Label DeF    ( 4)*/~
                               a1$, "`",           /* Glass Type   ( 3)*/~
                               a2$, "`",           /* Grid/Liting  ( 7)*/~
                               a3$, "`",           /* Sales Order  ( 9)*/~
                               a4$, "`",           /* Calc Width   (11)*/~
                               a5$, "`",           /* Model Code   ( 4)*/~
                               a6$, "`",           /* Prod Date    ( 9)*/~
                               a7$, "`",           /* Calc Height  (11)*/~
                               a8$, "`",           /* Shift Code   ( 3)*/~
                               a9$, "`",           /* Seq No       ( 6)*/~
                              a10$, "`",           /* Wind Wid & Hg(16)*/~
                              a11$, "`",           /* Remake Count ( 4)*/~
                              a12$, "`",           /* Depart Code  ( 4)*/~
                              a13$, "`",           /* Text         (41)*/~
                              a14$, "`",           /* Glass Barcode(10)*/~
                              a15$, "`",           /* Top or Bot   ( 4)*/~
                              a16$, "`",           /* Color        ( 7)*/~
                              a14$, "`",           /* Glass Barcode(10)*/~
                              a17$, "`",           /* C = CONTOR GRD(2)*/~
                               "}"                 /* End of Label  (1)*/

           write #15, eod goto L63210
                                                   /* (EWD012) - Begin */
L63200:    if scr_sel% > 2% and scr_sel% < 6% then gosub create_label_data

        return
L63210:    errormsg$ = "(Error)-Printing Re-make Label-- "& a14$
           gosub error_prompt
        return
                                                   /* (EWD012) - End   */ 
        change_muttin
            lt% = 0%
            lt$ = str(lab_rec$,65%,2%)
            convert lt$ to lt%, data goto L63234
L63234:
            mut$ = " "                         /* SWITCH VERT - HORZ */
            muttin$ = str(lab_ged$,45%,8%)     /*            TO      */
            if len(muttin$) < 5 then goto L63258
               xx% = pos(muttin$ = "x")
               cc% = pos(muttin$ = "C")
               ii% = (cc% - xx%)
            mut$ = str(muttin$,xx%+1%,ii%) & "x" & str(muttin$,1%,xx%-1%)
L63258:     if lt% > 82% then mut$ = l_lt$
        return

        gen_rpt
            call "SHOSTAT" ("Creating Glass Report")
            rm_tot% = 0% : rp_sel% = 0%
            init(" ") rm_ky$, rm_tot$
            convert rp_sel$ to rp_sel%, data goto L63282
L63282:                                            /* Set Start Date   */
            if rp_sel% > 3% then str(rm_ky$,1%,6%) = str(bg_dte$,1%,6%)
        gen_rpt_nxt
            if rp_sel% > 3% then goto L63303
               read #8,key 3% > rm_ky$, using L63309, rm_rec$,            ~
                                                  eod goto gen_rpt_done
               goto L63315
L63303:     read #8,key 1% > rm_ky$, using L63309, rm_rec$,               ~
                                                  eod goto gen_rpt_done
L63309:        FMT CH(256)

L63315:     if rp_sel% < 4% then rm_ky$ = str(rm_rec$,13%,21%)           ~
                            else rm_ky$ = str(rm_rec$,7%,27%)

            rm_st$ = str(rm_rec$,13%,1%)
            if rm_st$ = "2" and rp_sel% < 4% then goto gen_rpt_done

            if rm_st$ <> "0" then goto L63342
               if rp_sel% = 1% then goto print_data /* Scanned Re-Make */
                  goto gen_rpt_nxt
L63342:     if rm_st$ <> "1" then goto L63357
               if rp_sel% = 3% then goto print_data /* Scheduled Glass */
               if rp_sel% = 2% and str(rm_rec$,32%,2%) <> "00" then      ~
                                    goto print_data /* Scheduled Re-Mak*/
                  goto gen_rpt_nxt
L63357:     if rp_sel% = 4% then goto print_data    /* Completed Glass */
            if rp_sel% = 5% and str(rm_rec$,32%,2%) <> "00" then         ~
                                    goto print_data /* Completed Re-Mak*/
               goto gen_rpt_nxt

        print_data
            if rp_sel% < 4% then goto L63384
               if str(rm_ky$,1%,6%) > str(ed_dte$,1%,6%) then            ~
                                                        goto gen_rpt_done
L63384:        rm_bar$    = str(rm_rec$,22%,9%)
               rm_dept$   = str(rm_rec$,249,3%)
               rm_seq$    = str(rm_rec$,242,5%)
               rm_model$  = str(rm_rec$,72%,3%)
               rm_load$   = str(rm_rec$,67%,5%)
               rm_gls$    = str(rm_rec$,77%,2%)
               rm_mut$    = str(rm_rec$,234%,8%)
               rm_wd$     = str(rm_rec$,150%,7%)
               rm_ht$     = str(rm_rec$,157%,6%)
               rm_reason$ = str(rm_rec$,34%,2%)
               rm_so$     = str(rm_rec$,163%,8%)
               gosub lookup_reason
               rm_tot% = rm_tot% + 1%
               gosub print_dtl
               goto gen_rpt_nxt
        gen_rpt_done
            if rm_tot% = 0% then return
            convert rm_tot% to rm_tot$, pic(##########)
            print using L55060
            print using L55220, rm_tot$
            print using L55040
        return

        print_header
            init(" ") rpt_time$
            call "TIME" (rpt_time$)
            pageno% = pageno% + 1%
            if lcnt% <> 99% then print using L55040
            print page
            print using L55040
            print using L55080, date$, rpt_time$, company$, pageno%
            print using L55100, bg_date$
            print using L55120, ed_date$, title$
            print using L55060
            print using L55150
            print using L55190
            lcnt% = 7%
        return

        print_dtl
            if lcnt% > 58% then gosub print_header
            print using L55170, rm_bar$, rm_dept$, rm_seq$, rm_model$,    ~
                               rm_load$, rm_gls$,  rm_mut$, rm_wd$,      ~
                               rm_ht$, rm_so$, str(rm_reason_d$,1%,11%)
            lcnt% = lcnt% + 1%
        return
        Run_Program:
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        return

        create_label_data                         /* (EWD012) EWDGLSLB */
            if gl_run% = 0% then gosub get_run_no
               gl_sort% = gl_sort% + 1%
               convert gl_sort% to gl_sort$, pic(00000)

            put #25, using L35050, scr_dte1$,   /* Production Date     */~
                                   gl_run$,     /* Batch Run No        */~
                                   gl_sort$,    /* Sort Number         */~
                                   a14$,        /* Glass Barcode       */~
                                   a11$,        /* Glass Remake Number */~
                                   a1$,         /* Glass Type code     */~
                                   a2$,         /* Grid/Liting Code    */~
                                   a3$,         /* Customer Sales Order*/~
                                   a4$,         /* Calculated Width    */~
                                   a5$,         /* Model Code          */~
                                   a7$,         /* Calculated Height   */~
                                   a8$,         /* shift Code          */~
                                   a9$,         /* Production Seq. No. */~
                                   a10$,        /* Actual Wind Wid/Hght*/~
                                   a12$,        /* Department Code     */~
                                   a13$,        /* Glass Text          */~
                                   a15$,        /* View Top or Bot     */~
                                   a16$,        /* Color               */~
                                   a17$,        /* Contour Grid        */~
                                   " "          /* Filler Area         */
            write #25, eod goto L63500 
        return
L63500:     errormsg$ = "(Error) Writing re-Make Glass Label Data"
            gosub error_prompt
        return

        get_run_no
            gl_run$ = "000"
            str(gl_key$,1%,6%) = scr_dte1$
        get_run_nxt 
               read #25,key > gl_key$, using L63510, gl_key$,            ~
                                                    eod goto get_run_done
L63510:           FMT CH(14)
               if str(gl_key$,1%,6%) <> scr_dte1$ then goto get_run_done
                  gl_run$ = str(gl_key$,7%,3%) /* Find Last Run No Used */
                  goto get_run_nxt
        get_run_done
            gl_run% = 1%
            convert gl_run$ to gl_run%, data goto L63520
L63520:
            gl_run% = gl_run% + 1%
            convert gl_run% to gl_run$, pic(000)
            gl_sort% = 0%
        return

        prompt_run_no
            if scr_sel% < 3% or scr_sel% > 5% then return
            if remake_label% = 1% then return         /* (EWD012) -    */

            comp% = 2%                           
            hdr$ = "** Glass Re-Make Run Number**"
            msg$(1%)="Run Date = xx/xx/xxxx  Run Number = xxx            "
            gl_date$ = scr_dte1$
            call "DATFMTC" (gl_date$)
            str(msg$(1%),12%,10%) = gl_date$
            str(msg$(1%),37%,3%)  = gl_run$

            msg$(2%)="Write Down Run Date and Run number to print Labels."
            msg$(3%)="Press <RETURN> To Continue, Any (PF) Key To Exit.  "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
            if comp% <> 0% then goto exit_program
        return
                                                     /* (EWD012)       */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#6)
            end
