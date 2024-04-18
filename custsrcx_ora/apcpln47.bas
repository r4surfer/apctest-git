        REM *************************************************************~
            *     ***** TEST VERSION IN -- (APCPLN51)  ******           *~
            *  Program Name      - APCPLN47                             *~
            *  Creation Date     - 12/17/96                             *~
            *  Last Modified Date- 09/22/2015                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modified By       - Christie Gregory                     *~
            *                                                           *~
            *  Description       - This Program Creates the Screen      *~
            *                      labels and the Screen report.        *~
            *                                                           *~
            * System Screen Tables - Old Tables Not Used                *~
            *        (SCREEN01) - Screen Models for Two Labels          *~
            *        (SCREEN02) - Models with No Screen                 *~
            *        (SCREEN03) - Special Screen 1/3,1/3,1/3            *~
            *        (GLASS05)  - Table for '8' Series with Screen      *~
            *                                                           *~
            * New    Screen Tables                                      *~
            *        (SCREEN04) - Screen Master Control Table A thru E  *~
            *                     MMM - C     MMM = Model Code          *~
            *                                  -  = At Position (5)     *~
            *                                  C  = Code (A thru E)     *~
            *        (GLASS01 ) - Screen Quantities Only D's            *~
            *                     TT-BB       TT  = Not Applicable      *~
            *                                  -  = At Position (27)    *~
            *                                 BB  = No of Screens       *~
            *        (SCREEN06) - Specified Screen Constant             *~
            *                     HH.HHHH  - FF.FFFF                    *~
            *                                 HH.HHHH = Half Scr Const. *~
            *                                   -     = At Position (10)*~
            *                                 FF.FFFF = Full Scr Const. *~
            *        (SCREENBFM) - Screen Master Control by Product to  *~
            *                      send to BayForm.                     *~
            *                      MMM     -  DDDD                      *~
            *                                 MMM  = Product Model      *~
            *                                 DDDD = Description        *~
            *                                                           *~
            * (Old)  (SCREEN05) - Department Batch Size and Sort        *~
            *                     CC = BBB S -< Dept Description >      *~
            *                                 CC  = Sub Script to IDX$()*~
            *                                 BBB = No. for Batch Size  *~
            *                                 S   = Sort Code(A-Z)(0-9) *~
            *                                 < > = Dept Description    *~
            *                                                           *~
            *        (SCREEN07) - Department Batch Size and Sort        *~
            *                     CCC - DDD  BBBB                       *~
            *                                 CCC = Sub Script by Order *~
            *                                       Read into Array     *~
            *                                 DDD = Department Code     *~
            *                                 BBBB= Batch Size for Dept *~
            *                                                           *~
            * Notes - Need Hinge Code for Codes (C) and (D)             *~
            *       - Code (E) = No Screen                              *~
            *       - When Specified Use tabel (SCREEN06)               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *  Screen Array  = SS$()20                                  *~
            *   Position ( 1 for  1) = Screen Control Code              *~
            *            ( 2 for  2) = NO = Normal, CO = Cottage,       *~
            *                          OR = Oriel, SP = Specified       *~
            *            ( 4 for  1) = H = Half, F = Full Screen        *~
            *                          4 = 1/4,1/2,1/4 - 3 = 1/3,1/3,1/3*~
            *            ( 5 for  4) = Width Phantom                    *~
            *            ( 9 for  4) = Height Phantom or (TABL)         *~
            *            (13 for  4) = CB Cut Length Phantom            *~
            *            (17 for  4) = CB Location Phantom /(TABL)(CTRS)*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/17/96 ! New Program for (APC) - LAST MOD DATE    ! RHH *~
            *          !   Contains all the Modification that are !     *~
            *          !   in APCGLS06, and the following         !     *~
            * 12/17/96 ! New Planning Version of Screens          ! RHH *~
            * 06/06/97 ! Modes for New Family of windows and Two  ! RHH *~
            *          !   ne Dept's (049) and (052).             !     *~
            * 07/07/97 ! Skip the following Dept's for Screen's   ! RHH *~
            *          !   (023), (042), (056) Will not be Passed !     *~
            *          !   to Screen Dept. Sub 'CHECK_SUPPORT'    !     *~
            * 08/27/97 ! New Sort and Select Routine for screens. ! RHH *~
            *          !   Sort is in Conjunction with Production !     *~
            *          !   departments. Uses DT_INDEX$ - APCPLNDT !     *~
            * 01/19/98 ! Mod to Assign Schedule Number from the . ! RHH *~
            *          !   Table (PLANSCHED). For the Linealmate  !     *~
            *          !   Saws in the Screen Department.         !     *~
            * 01/27/00 ! (EWD001) Mods for Samples/Displays/ Lit  ! RHH *~
            * 04/24/00 !   " "    Additional Mods for new code    ! RHH *~
            *          !          '027' Partsw range (012 thru 026)!    *~
            * 02/08/00 ! (EWD002) Mods to build a work file to    ! CMG *~
            *          !          send to BayForm by Product.     !     *~
            * 08/16/01 ! (EWD003) Mods for extending Part Wood    ! CMG *~
            *          !            Code to '035'.                !     *~
            * 12/11/01 ! (EWD004) Mods for new products 421 and   ! CMG *~
            *          !            431.                          !     *~
            * 02/08/02 ! (EWD005) Mods to build a work file to    ! CMG *~
            *          !          send to RiteScreen by Product.  !     *~
            * 06/01/02 ! (EWD006) Mod change company name         ! CMG *~
            * 06/12/02 ! (EWD007) Mod change sort for Rite Screen ! CMG *~
            * 09/17/02 ! (EWD008) Fix for Special Shapes Grid Code! CMG *~
            * 04/26/04 ! (EWD009) mod to consolidate the screen   ! CMG *~
            *          !           center bar report              !     *~
            * 01/13/05 ! (AWD010) Mod to fix descriptions of last ! CMG *~
            *          !          model printed on screen report  !     *~
            * 01/24/07 ! (AWD011) Mod to add EOF character to     ! CMG *~
            *          !          FTP bayform file                !     *~
            * 04/10/07 ! (AWD012) mod to change bronze scrns to WH! CMG *~
            *07/18/2008! (AWD013) mod to add screen audit file    ! DES *~
            *12/24/2008! (AWD014) mod for ASM screen              ! CMG *~
            *03/09/2009! (AWD015) mod for extruted full screen    ! CMG *~
            *04/13/2009! (AWD016) mod not to send extruded half   ! CMG *~
            *          ! screen to ASM for 267 only               !     *~
            *03/26/2010! (AWD017) mod for screen remakes          ! DES *~
            *04/12/2010! (AWD018) mod for CUSTOM Size Door        ! CMG *~
            *08/02/2010! (AWD020) mod for new door screen only    ! CMG *~
            *08/18/2010! (AWD021) mod to force roll form for order! CMG *~
            *          !   entered with wrong screen type         !     *~
            *09/30/2010! (AWD022) store skey and ss for reference ! CMG *~
            *01/25/2011! (AWD023) add sub part for "wired" screen ! DES *~
            *04/18/2011! (AWD024) mod to create awdplnsr if create! CMG *~
            *          ! data for specific load or dept           !     *~
            *06/04/2012! (AWD025) mod for locking half screen     ! CMG *~
            *03/14/2013! (AWD026) mod for NTX                     ! CMG *~
            *07/02/2013! (AWD027) mod to lock codes with no screen! CMG *~
            *          !          OGO change ecn 2013-032         !     *~
            *05/07/2014! (AWD028) mod to NOT pull or update       ! PWW *~
            *          !          inventory if a wired screen     !     *~
            *06/02/2014! (AWD029) mod for operable shapes leg ht  ! CMG *~
            *08/28/2014! (AWD030) fix for AWD028 above.           ! PWW *~
            *04/13/2014! (IM5733) turn on Linealmate for ASM      ! MES *~
            *09/22/2015! (SR65703) mod to fix extruded half screen! CMG *~
            *          !    as option size                        !     *~
            *09/29/2016! (CR688)  mod for color L versus D locking! MES *~
            *03/17/2017! (SR79967) skip screen qty for doors      ! CMG *~
            *04/05/2017! (SR80152) add V for no screen for doors  ! MES *~
            *06/05/2017! (CR 986) new screen mesh numbers; total  ! RDB *~
            *          !          in FTPBAYFM file                ! RDB *~
            *07/20/2017! (CR1013) remove GSO Wel references for   ! RDB *~
            *          !          remake screens, fix pgm freeze  !     *~   
            *09/13/2019! (CR2217) flex screen mod                 ! MES *~
            *08/06/2020! (CR2653) Allow not complete SO to be     ! RDB *~
            *          !          a batch - addon sales orders    !     *~
            *************************************************************
   
        dim                              /* FILE = AMTBOMCD            */~
            ht2$9,                       /* Center Bar Location        */~
            phantom$25                   /* Phantom Designator         */

        dim scrn_rec$160             /* AWD013 */
        dim temp1$64, temp2$61       /* AWD013 */

        dim                              /* FILE = APCPLNDT            */~
            dt_rec$256,                  /* Detail Record              */~
            dt_key1$57,                  /* Alt - Bar Code Key         */~
            dt_dept$3,                   /* Department Code            */~
            dt_proc$2,                   /* proc                       */~
            dt_date$8,                   /* date                       */~
            dt_barcode$18,               /* barcode                    */~
            dt_load$5,                   /* Load Number                */~
            dt_part$25, dt_wood$3,       /* Prod/Comp Seq. (0) or (1)  */~
            dt_desc$30, dt_so$8,         /* Prod/Comp Date (0) or (1)  */~
            dt_seq$5, dt_index$30,       /* Production Sort Index      */~
            dt_shft$2, scr_shft$2,       /* DESCRIPTION                */~
            dt_special$10,               /* S.O. Special Flags         */~
            dt_ref$8,                    /* REF / WARRANTY             */~
            dt_samp$1,                   /* 0=NO, 1=SAMP, 2=DISP       */~
            dt_status$1,                 /*                            */~
            scr_shft_d$30,               /*                            */~
            dt_sku$9                     /* Sku Number David           */

        dim                              /* FILE - (APCPLNW1)          */~
            wrk_key$60,                  /* WRK PRIMARY KEY            */~
            wrk_rec$60,                  /* WORK RECORD                */~
            wrk_rec1$128,                /* WORK RECORD                */~
            sav_rec$60,                  /* WORK RECORD         EWD009 */~
            wrk_fil$46,                  /* FILLER                     */~
            lab_key$65,                  /* Gl and Sc Primary Key      */~
            lab_rec$200,                 /* Gl and Sc Label Record     */~
/*EWD002*/  bay_rec$100,                 /* BayForm Send Record        */~
            lab_fil$9,                   /*                            */~
            wd$7,                        /* Actual Width               */~
            ht$6,                        /* Actual Height              */~
            sav_wd$7,                    /* Actual Width       EWD009  */~
            sav_ht$6,                    /* Actual Height      EWD009  */~
            txt_flag$1,                  /* Special Text Exists        */~
            sav_part$25                  /* Save Part Number           */


        dim                              /* (Program Variables)        */~
            wt$3,                        /* Window Type Code           */~
            wcode$1,                     /* Window Code (A) thru (E)   */~
            wcode1$1,                    /* Roll Form Code  (AWD014)   */~
            sav_screen$25,               /* Used by 'CHECK_SCREEN'     */~
            skey$10,                     /* Equation Type Key          */~
            skey$(999%)10,               /* (AWD019) look up code      */~
            savSKey$10,                  /* (AWD019) Sav skey          */~
            sqty$2,                      /* Screen Quantity-No. Screens*/~
            sqty1$2,                     /* Door Screen Qty  (SR79967) */~
            special$1,                   /* '0'=N/A, '1'=Specified Clmr*/~
            ph$(4%)4,                    /* Possible (4) Phantoms/Scr  */~
            err$(10%)60,                 /* Pre-defined Error Messages */~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            qty$5,                       /* QUANTITY                   */~
            grid$25,                     /* Description of Grid        */~
            screen_dte$8,                /* Screen Comp Date Formated  */~
            screen_dte1$8,               /* Screen Comp Date Unformated*/~
            sze$(10%)3,                  /* Save Eights                */~
            sze1$(20%)5,                 /* Save Sixteenths            */~
            wd1$9,                       /* Calculated Width           */~
            wd2$9,                       /* CLMR FOR SCREEN            */~
            ht1$8,                       /* Calculated Height          */~
            sav_wd1$9,                   /* Calculated Width     EWD009*/~
            sav_ht1$8,                   /* CLMR FOR SCREEN      EWD009*/~
            readkey$30,                  /* GENCODES Look-Up Key       */~
/*AWD014*/  scr$(10%)40, ss$(999%)20,    /* Screen Text Messages       */~
            scr_dte$8,                   /* Screen Completion Date FORM*/~
            scr_dte1$8,                  /* Screen Comp. Date Unform   */~
            scr_code$1,                  /* Screen Report Selection    */~
            scr_dept$3, scr_prod$1,      /* Screen Department Code     */~
            scr_msg$30,                  /* Screen - Report Selection  */~
            scr_msg1$30, l_txt$25,       /* Screen - Product Line      */~
            scr_load$5, scr_desc$30,     /* Screen - Load Number       */~
            scr_inv$1, scr_pdesc$30,     /* Inv Pull                   */~
            scr_so$8,                    /* CR2653                     */~
            sched$3,                     /* Linealmate Schedule No.    */~
            descr$30,   stk_so$8,        /* Use for GENCODES Look-Up   */~
            batch_amt$4,                 /* Batch Sizes from Table     */~
            dept_scr$1,                  /* Dept Sort Code             */~
            sav_dept_scr$1,              /* Dept Sort Code             */~
            dept$(300%)3,                /* Store Department Codes     */~
/*(EWD002)*/byfdept$(300%)3,             /* Store BayForm Depart Codes */~
/*(EWD005)*/rtedept$(300%)3,             /* Store RteScrn Depart Codes */~
            sort$50,                     /* Store Sort Codes 0-9,A-Z   */~
            batch%(300%,6%),             /* Batch Sorting Array        */~
/*(EWD002)*/byfbatch%(300%,6%),          /* BayForm Batch Sorting Array*/~
/*(EWD005)*/rtebatch%(300%,6%),          /* RteScrn Batch Sorting Array*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            rpt_time$8,                  /* Report Time                */~
            title$25,                    /* Report Title               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
/*(EWD002)*/script$8,                    /* Script to send file to Bayfm*/~
/*(EWD005)*/script_rte$8                 /* Script to send file to RteSn*/

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
            rslt$(64%)20                 /* Text from file opening     */

        dim                              /* FILE - (TEXTFILE)          */~
            textid$4                     /* S.O. TEXTID$               */

        dim                              /* FILE - (NEW LABEL VARS)    */~
            model$3,                     /* MODEL CODE                 */~
            cl$1, cl_l$2, color$6,       /* COLOR CODE                 */~
            gl$2,                        /* GLASS CODE                 */~
            lt$2, co_or$8,               /* LITING CODE                */~
            hg$2, hnge$6, hg_l$8, hh$8,  /* HINGE CODE                 */~
            sc$1, sc_l$4, sc_r$20,       /* SCREEN CODES               */~
            lk$1,                        /* LOCK CODES                 */~
            width$4,                     /* WIDTH                      */~
            height$3,                    /* HEIGHT                     */~
            clmr$3,                      /* CENTER LINE MEETING RAIL   */~
            wallw$3,                     /* WALL WIDTH                 */~
            extruded$1                   /* Extruded or roll form (AWD015)*/

        dim                              /* FILE - Screen Explosion    */~
            cut$(4%)62,                  /* Max (4) Screens            */~
            txt$(3%)50                   /* Screen '102 Header Text    */



       dim  dd$9, dd1$3, num$1,          /* Label Day of Week          */~
            sr_key$12,                   /* AWDPLNSR Key               */~
            sr_key1$47,                  /* AWDPLNSR Key               */~
            sr_rec$(2)256,               /* AWDPLNSR Record            */~
            sr_model$3,                  /* SR Model Number            */~
            pull_dte$6,                  /* Today date if pulled       */~
            start_pull$6                 /* First Pull Date            */

        dim                                                              ~
            scr_batch$20,                /* batch name                 */~
            scr_batch2$20,               /* batch name                 */~
            inv_key$19,                  /* inventory readkey          */~
            sav_inv$19,                  /* save inventory key         */~
            sr_prod_date$8, /* yyyymmdd format */                        ~
            sr_process$1,                                                ~
            sr_batch$20,                                                 ~
            sr_batch_num$5,                                              ~
            sr_dept$3,                                                   ~
            sr_barcode$9,                                                ~
            sr_remake_num$3,                                             ~
            sr_seq$5,                                                    ~
            sr_model_num$3,                                              ~
            sr_color$1,                                                  ~
            sr_screen_code$1,                                             ~
            sr_screen_type$1,                                            ~
            sr_screen_hf$1,                                              ~
            sr_width,   /*    number(14,4) default 0 not null, */        ~
            sr_height,  /*    number(14,4) default 0 not null, */         ~
            sr_cb_len,  /*    number(14,4) default 0 not null, */         ~
            sr_cb_loc,  /*    number(14,4) default 0 not null, */         ~
            sr_width$,   /*    number(14,4) default 0 not null, */        ~
            sr_height$,  /*    number(14,4) default 0 not null, */        ~
            sr_cb_len$,  /*    number(14,4) default 0 not null, */        ~
            sr_cb_loc$,  /*    number(14,4) default 0 not null, */        ~
            sr_width_frac$10,                                            ~
            sr_height_frac$10,                                           ~
            sr_cb_len_frac$10,                                           ~
            sr_cb_loc_frac$10,                                           ~
            sr_part$25,                                           ~
            sr_status$1,                                                 ~
            sr_cut_pull$1,                                               ~
            sr_sales_order$8,                                            ~
            sr_wnd_width$10,                                             ~
            sr_wnd_height$10,                                            ~
            sr_comp_date$8, /* yyyymmdd format */                        ~
            sr_comp_time$5, /* hh:mm format */                           ~
            sr_id_num$3,                                                 ~
            sr_prod_time$5, /* hh:mm format */                           ~
            sr_shift$2,                                                  ~
            sr_reason$3,                                                 ~
            sr_rmk_id$3,                                                 ~
            sr_org_dte$8, /* yyyymmdd format */                          ~
            sr_sku$9,                                                    ~
            sr_skey$15,                                                  ~
            sr_ss$20,                                                    ~
            sr_options$4,                                                ~
            sr_sub_part$20,                                              ~
            date1$50,                    /* connection string          */~
            stmt1$250,                   /* connection string          */~
            stmt2$250,                   /* connection string          */~
            stmt3$250,                   /* connection string          */~
            stmt4$250,                   /* connection string          */~
            stmt5$250,                   /* connection string          */~
            stmt6$250,                   /* connection string          */~
            stmt7$250,                   /* connection string          */~
            stmt8$250,                   /* connection string          */~
            server$25,                   /* connection string          */~
            user$25,                     /* user name to connect       */~
            pass$25,                     /* password to connect        */~
            scr_type$1,                  /* screen type roll extruded  */~
            scr_hf$1,                    /* screen half full           */~
            scr_pull$1,                  /* screen pull or cut         */~
            scr_num$10                   /* screen record number       */

/* (AWD029) */
        dim flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            dim1es$10, dim2es$, dim3es$


        dim sp_key$11, sp_rec$256, message$256

        dim schema$8,                    /* Schema (AWD026)            */~
            volume$6

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "(New)Planning Screen Processing Utility  "
            pname$ = "APCPLN47 - Rev: 06.04"

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
            * #1 ! AMTBOMCD ! Master Equation File                      *~
            * #2 ! APCPLNDT ! Production Master Detail File             *~
            * #3 ! GENCODES ! Master System Table File                  *~
            * #4 ! AMTBOMIF ! Lineal Equation Info File                 *~
            * #5 ! APCPLNW1 ! Screen Work File for (Reports)            *~
            * #6 ! APCPLNW2 ! Screen Work File for (Labels)             *~
            * #7 ! HNYMASTR ! Inventory Master File                     *~
            * #8 ! APCCUTEQ ! Lineal Mate Equation Definitions          *~
            * #9 ! TEXTFILE ! Master Text File                          *~
            * #10! BCKLINES ! Sales Order Line Items                    *~
            * #11! EWDBAYWK ! BayForm Work File (REPORTS)               *~
            * #12! EWDBAYW1 ! BayForm Work File (LABELS)                *~
            * #13! EWDBAYFM ! BayForm Actual Send File                  *~
            * #14! EWDRTEWK ! RiteScreen Work File (REPORTS)            *~
            * #15! EWDRTEW1 ! RiteScreen Work File (LABELS)             *~
            * #16! EWDRTESN ! RiteScreen Actual Send File               *~
            * #18! SCRNAUDT ! Screen Audit File (AWD013)                *~
            * #20! AWDPLNSR ! Screen Production File                    *~
            * #21! SCRINV   ! Screen Inventory Master File              *~
            * #22! SCRMDL   ! Screen Inventory Model Master File        *~
            * #63! BCKSUBPT ! Sub Part file                             *~
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

            select #4,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~



            select #5,  "APCPLNW1",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =   60

            select #6,  "APCPLNW2",                                      ~
                        varc,     indexed,  recsize =   200,             ~
                        keypos =    1, keylen =   65

/*(EWD009)*/ select #17, "APCPLNW3",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =   60

            select #7,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #8,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #10, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19
/* (EWD002) - Begin */
            select #11, "EWDBAYWK",                                      ~
                        varc,     indexed,  recsize = 128  ,             ~
                        keypos =    1, keylen = 60

            select #12, "EWDBAYW1",                                      ~
                        varc,     indexed,  recsize = 100  ,             ~
                        keypos =    1, keylen = 60

            select #13, "EWDBAYFM", varc, consec,  recsize = 100
/* (EWD002) - End   */

/* (EWD005) - Begin */
            select #14, "EWDRTEWK",                                      ~
                        varc,     indexed,  recsize = 128  ,             ~
                        keypos =    1, keylen = 60

            select #15, "EWDRTEW1",                                      ~
                        varc,     indexed,  recsize = 100  ,             ~
                        keypos =    1, keylen = 60

            select #16, "EWDRTESN", varc, consec,  recsize = 100
/* (EWD005) - End   */

            select #18,  "SCRNAUDT",                                     ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos =    1, keylen =   16,                    ~
                        alt key  1, keypos =   17, keylen =  36, dup,    ~
                            key  2, keypos  =  40, keylen =  13, dup,    ~
                            key  3, keypos  =   9, keylen =   8, dup


             select #19, "AWDPLNW4",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =   60

/* @@@@@
key         field    column
1           date     1-8          ch(8)
1 4         dept     9-11         ch(3)
1 4         seq     12-16         ch(5)
2           barcode 17-34         ch(18)
2           dept2   35-37         ch(3)
2           proc    38-39         ch(2)
2 3         S.O.    40-47         ch(8)
2 3         S.O. line # 48-53     ch(2)
            model$  53-55         ch(3)
            cl_l    56-57         ch(2)
            sc_l    58-61         ch(4)
            wd1     62-70         ch(9)
            ht1     71-78         ch(8)
            cb      79-87         ch(9)
            wd      88-94         ch(7)
            ht      95-100        ch(6)
            received 101-101      ch(1) N/Y
            in house 102-102      ch(1) I/B/R
            filler 103-128        ch(26)
*/

            select #20,  "AWDPLNSR",                                     ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =   42, keylen =   12,                    ~
                        alt key  1, keypos =    7, keylen =  47,         ~
                            key  2, keypos  = 163, keylen =  13,         ~
                            key  3, keypos =   1, keylen =  53,          ~
                            key  4, keypos = 205, keylen =  12, dup


            select #21, "SCRINV"  ,                                      ~
                        varc,     indexed,  recsize = 1012,              ~
                        keypos = 11,    keylen = 19,                     ~
                        alt key  1, keypos =    1, keylen =  10

            select #22, "SCRINMDL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 13,                      ~
                        alt key  1, keypos =   11, keylen =  3, dup


            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),   0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),   0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),   0%, rslt$(4%))
            call "OPENCHCK" (#7,  fs%(7%),  f2%(7%),   0%, rslt$(7%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),   0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%),  f2%(9%),   0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 500%, rslt$(18%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 500%, rslt$(20%))

            call "OPENCHCK" (#21, fs%(21%), f2%(21%),  0%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%),  0%, rslt$(22%))
            call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))


            if f2%(12%) <> 0% then L09000
               call "FILEBGON" (#12)
L09000:        call "OPENCHCK" (#12, fs%(12%), f2%(12%), 200%, rslt$(12%))


/* (AWD026) */
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)
/* (\AWD026) */

REM         VOLUME$ = "CARLO2"
REM         IF SCHEMA% = 2% THEN VOLUME$ = "NE2"
REM         OPEN NODISPLAY #13, OUTPUT, SPACE = 100%,                    ~
REM             DPACK   = 100%, IPACK = 100%, FILE = "EWDBAYFM",         ~
REM             LIBRARY = "FTPBAYFM", VOLUME = VOLUME$, BLOCKS = 5%


/*EWD005*/  call "OPENCHCK" (#15, fs%(15%), f2%(15%),  0%, rslt$(15%))
            if f2%(15%) <> 0% then L09500
               call "FILEBGON" (#15)
L09500:        call "OPENCHCK" (#15, fs%(15%), f2%(15%), 200%, rslt$(15%))

REM         OPEN NODISPLAY #16, OUTPUT, SPACE = 100%,                    ~
REM             DPACK   = 100%, IPACK = 100%, FILE = "EWDRTESN",         ~
REM             LIBRARY = "FTPRTESN", VOLUME = VOLUME$, BLOCKS = 5%


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

            scr$( 1%) = "****************************************"
            scr$( 2%) = "*        Consolidated Screen           *"
            scr$( 3%) = "*                                      *"
            scr$( 4%) = "* (1) - Calculate MFG Part Screen Sizes*"
            scr$( 5%) = "* (2) - Screen Report/Labels           *"
            scr$( 6%) = "* (3) - Linealmate                     *"
            scr$( 7%) = "* (4) - Create Screen Remake Batch     *"
            scr$( 8%) = "*                                      *"
            scr$( 9%) = "****************************************"

        REM - NEAREST 8TH INCH    - NEAREST 16TH INCH
            sze$(1%)  = "1/8"    : sze1$( 1%) = " 1/16"
            sze$(2%)  = "1/4"    : sze1$( 2%) = " 1/8 "
            sze$(3%)  = "3/8"    : sze1$( 3%) = " 3/16"
            sze$(4%)  = "1/2"    : sze1$( 4%) = " 1/4 "
            sze$(5%)  = "5/8"    : sze1$( 5%) = " 5/16"
            sze$(6%)  = "3/4"    : sze1$( 6%) = " 3/8 "
            sze$(7%)  = "7/8"    : sze1$( 7%) = " 7/16"
            sze$(8%)  = "ERR"    : sze1$( 8%) = " 1/2 "
            sze$(9%)  = "   "    : sze1$( 9%) = " 9/16"
                                   sze1$(10%) = " 5/8 "
                                   sze1$(11%) = "11/16"
                                   sze1$(12%) = " 3/4 "
                                   sze1$(13%) = "13/16"
                                   sze1$(14%) = " 7/8 "
                                   sze1$(15%) = "15/16"
                                   sze1$(16%) = "     "


            gosub loadPhantoms          /* (AWD019) */

REM            ss$( 1%) = "ANOH4001400300000000"
REM            ss$( 2%) = "ANOF4101410340004200"
REM            ss$( 3%) = "ACOH4001401300000000"
REM            ss$( 4%) = "ACOF4101410340004210"
REM            ss$( 5%) = "AORH4001402300000000"
REM            ss$( 6%) = "AORF4101410340004220"
REM            ss$( 7%) = "ASPH4001TABL00000000"      /* SCREEN06 - LEFT */
REM            ss$( 8%) = "ASPF410141034000TABL"      /* SCREEN06 - RIGHT*/

REM            ss$( 9%) = "BNOH4001400300000000"
REM            ss$(10%) = "BNOF410141034000CTRS"

REM            ss$(11%) = "CNO44001400300000000"      /* 1/4, 1/2, 1/4   */
REM            ss$(12%) = "CNO34004400300000000"      /* 1/3, 1/3, 1/3   */

REM            ss$(13%) = "DNOH4001400300000000"      /* GLASS01         */
REM            ss$(14%) = "DNOF4101410340004200"
REM            ss$(15%) = "DCOH4001401300000000"      /* (EWD004) - added*/
REM            ss$(16%) = "DCOF4101410340004210"      /* phantom lookups */
REM            ss$(17%) = "DORH4001402300000000"
REM            ss$(18%) = "DORF4101410340004220"

/* Roll form formulas */
REM            ss$(21%) = "FNOH5001500300000000"
REM            ss$(22%) = "FNOF0000000000000000"    /* no full screen yet*/
REM            ss$(23%) = "FCOH5001501300000000"
REM            ss$(24%) = "FCOF0000000000000000"    /* no full screen yet*/
REM            ss$(25%) = "FORH5001502300000000"
REM            ss$(26%) = "FORF0000000000000000"    /* no full screen yet*/
REM            ss$(27%) = "FSPH5001TABL00000000"
REM            ss$(28%) = "FSPF0000000000000000"    /* no full screen yet*/

REM            ss$(29%) = "GNOH5001500300000000"
REM            ss$(30%) = "GNOF0000000000000000"    /* no full screen yet*/

REM            ss$(31%) = "HNO45001500300000000"
REM            ss$(32%) = "HNO35004500300000000"

REM            ss$(33%) = "INOH5001500300000000"
REM            ss$(34%) = "INOF0000000000000000"    /* no full screen yet*/
REM            ss$(35%) = "ICOH5001501300000000"
REM            ss$(36%) = "ICOF0000000000000000"    /* no full screen yet*/
REM            ss$(37%) = "IORH5001502300000000"
REM            ss$(38%) = "IORF0000000000000000"    /* no full screen yet*/



REM            ss_max% = 38%                          /* (AWD014) */


            err$(1%) =                                                   ~
           "Err(1) - MFG Not found in Table (SCREEN04) ??               "
            err$(2%) =                                                   ~
           "Err(2) - Screen Quantities not found in Table (GLASS01) ??  "
            err$(3%) =                                                   ~
           "Err(3) - Screen Special Constant not found in (SCREEN06) ?? "
            err$(4%) =                                                   ~
           "Err(4) - Unable to Load Specified Phantom Number ??         "
            err$(5%) =                                                   ~
           "Err(5) - Screen Equation Not on File ??                     "
            err$(6%) =                                                   ~
           "Err(6) - No Equation and No Data on File ??                 "
            err$(7%) =                                                   ~
           "                                                            "

            script$     = "FTPBAYFM"                /* (EWD002) */
            if schema% = 2% then script$ = "NTXBAYFM"
            script_rte$ = "FTPRTESN"                /* (EWD005) */
            if schema% = 2% then script_rte$ = "NTXRTESN"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
REM            gosub oracle_connect
REM            if oci_err% < 0 then goto exit_program

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
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
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%
            goto editpg1
        inputmode_a
            for fieldnr% = 1% to  1%
L10280:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10400
L10300:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10380
L10330:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10300
                         if fieldnr% = 1% then L10280
                         goto L10330
L10380:               if keyhit% = 16% then goto exit_program
                      if keyhit% <> 0% then       L10300
L10400:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10300
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
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
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
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
             on scr_code% gosub option1, option2, option3

        return clear all
        goto inputmode


        option1
           return clear all
           goto inputmode_a


        option2
          call "SHOSTAT" ("Creating "& scr_msg$)
          gosub get_batch_name /* AWD017 */
          if keyhit% <> 14% then goto begin_process
          gosub create_data
        return

        option3
          gosub optimize

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
         "Enter (1)Calculate Screen Sizes, (2)Screen Report/Labels.    ",~
         "Enter the Planned Production Date Associated with Screens.   ",~
         "Enter a Valid Dept Reference Code (1 thru 15) or All.        ",~
         "Enter a Valid Sales Order Number.                            ",~
         "Enter a Valid Load Number.                                   ",~
         "Enter 'Y' or 'N' to Pull from Inventory                      "

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
         "Enter a Valid Stock or Manufactured Part Number?              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr_code$, scr_msg$,       ~
                      scr_dte$, scr_dte1$, scr_dept$, scr_msg1$,         ~
                      screen_dte$, screen_dte1$, scr_load$, scr_desc$,   ~
                      dt_part$, dt_desc$, scr_shft$, scr_shft_d$, cut$(),~
                      scr_batch$, scr_batch2$,                           ~
                      dt_wood$, scr_pull$, scr_pdesc$, scr_inv$,         ~
                      dim1es$, dim2es$, dim3es$, scr_so$

            txt$(1) =                                                    ~
                "**************************************************"
            txt$(2) =                                                    ~
                "* (Calculate) Screen Size for Manufactured Parts *"
            txt$(3) =                                                    ~
                "**************************************************"
            gosub load_tables
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
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210,         /* Screen's Selection*/   ~
                                L40200,         /* Production Date   */   ~
                                L40200,         /* Department Code   */   ~
                                L40200,         /* SO Number         */   ~
                                L40200,         /* Load Number       */   ~
                                L40200          /* Pull Inventory    */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

/* CR2653 change out shift for sales order number */
L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Screen Selections (1-4) :",                  ~
               at (06,30), fac(lfac$(1%)), scr_code$            , ch(01),~
               at (06,40), fac(hex(84)), scr_msg$               , ch(30),~
               at (07,02), "Planned Production Date :",                  ~
               at (07,30), fac(lfac$(2%)), scr_dte$             , ch(08),~
               at (08,02), "Department Code, or All :",                  ~
               at (08,30), fac(lfac$(3%)), scr_dept$            , ch(03),~
               at (08,40), fac(hex(84)), scr_msg1$              , ch(30),~
               at (09,02), "Sales Order Number      :",                  ~
               at (09,30), fac(lfac$(4%)), scr_so$              , ch(08),~
               at (10,02), fac(hex(84)), l_txt$                 , ch(25),~
               at (10,30), fac(lfac$(5%)), scr_load$            , ch(05),~
               at (10,40), fac(hex(84)), scr_desc$              , ch(30),~
               at (11,02), "Pull From Screen Inv    :",                  ~
               at (11,30), fac(lfac$(6%)), scr_inv$             , ch(01),~
               at (11,40), fac(hex(84)), scr_pdesc$             , ch(30),~
                                                                         ~
               at (12,21), fac(hex(84)), scr$(1%)               , ch(40),~
               at (13,21), fac(hex(84)), scr$(2%)               , ch(40),~
               at (14,21), fac(hex(84)), scr$(3%)               , ch(40),~
               at (15,21), fac(hex(84)), scr$(4%)               , ch(40),~
               at (16,21), fac(hex(84)), scr$(5%)               , ch(40),~
               at (17,21), fac(hex(84)), scr$(6%)               , ch(40),~
               at (18,21), fac(hex(84)), scr$(7%)               , ch(40),~
               at (19,21), fac(hex(84)), scr$(8%)               , ch(40),~
               at (20,21), fac(hex(84)), scr$(9%)               , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40600
                  gosub display_departments
                  goto L40230

L40600:        if keyhit% <> 15 then goto L40640
                  call "PRNTSCRN"
                  goto L40230

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
           l_txt$ = "Production Load or Blank:"
           if  scr_code% = 3% then l_txt$ = "Schedule No. for Lineal :"

        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffffff0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40790:     if fieldnr% > 1% then L40810
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                       (16)Print Data  "
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                 (9)Display Dept's      " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffff09ffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Stock Parts Glass Entry                                   *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41180          /* Stock Part Number */

              goto L41210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
                                                                         ~
               at (04,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(3%)               , ch(50),~
                                                                         ~
               at (07,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,02), "MFG Part Number   :",                        ~
               at (08,25), fac(lfac$(1%)), dt_part$             , ch(25),~
/*(AWD029)*/   at (08,52), fac(lfac$(1%)), dim1es$              , ch(10),~
/*(AWD029)*/   at (08,64), fac(lfac$(1%)), dim2es$              , ch(10),~
               at (09,25), fac(hex(84)), dt_desc$               , ch(32),~
                                                                         ~
               at (10,10), fac(hex(84)), cut$(1%)               , ch(62),~
               at (11,10), fac(hex(84)), cut$(2%)               , ch(62),~
               at (13,10), fac(hex(84)), cut$(3%)               , ch(62),~
               at (14,10), fac(hex(84)), cut$(4%)               , ch(62),~
                                                                         ~
               at (16,02), "Screen Material   :",                        ~
               at (16,25), fac(hex(84)), scr_type$              , ch(01),~
                                                                         ~
               at (17,02), "Phantom(1)        :",                        ~
               at (17,25), fac(hex(84)), ph$(1)                 , ch(04),~
                                                                         ~
               at (18,02), "Phantom(2)        :",                        ~
               at (18,25), fac(hex(84)), ph$(2)                 , ch(04),~
                                                                         ~
               at (19,02), "Phantom(3)        :",                        ~
               at (19,25), fac(hex(84)), ph$(3)                 , ch(04),~
                                                                         ~
               at (20,02), "Phantom(4)        :",                        ~
               at (20,25), fac(hex(84)), ph$(4)                 , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41480
                  call "PRNTSCRN"
                  goto L41210

L41480:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L41630     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L41630: if fieldnr% > 0% then L41720  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Calc Data   "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L41720:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50150,         /* Screen's Selection    */ ~
                              L50340,         /* Production Date       */ ~
                              L50550,         /* Department Code       */ ~
                              L50885,         /* SO Number             */ ~
                              L50930,         /* Load Number           */ ~
                              L51030          /* Pull flag             */
            return

L50150: REM Screen's Selection                    SCR_CODE$
            scr_code% = 0%
            if scr_code$ <> " " then goto L50190
               scr_code$ = "1"
L50190:     convert scr_code$ to scr_code%, data goto L50290

            if scr_code% < 1% or scr_code% > 4% then goto L50290
            scr_msg$ = str(scr$(scr_code% + 3%),9%,30%)
            if scr_code% = 4% then goto screen_remakes

REM         IF SCR_CODE% <> 2% THEN GOTO L50250
REM L50230
REM         GOSUB GET_BATCH_NAME
REM         IF KEYHIT% =  1% THEN GOSUB STARTOVER
REM         IF KEYHIT% = 16% THEN GOTO EXIT_PROGRAM
REM         IF KEYHIT% <> 14% THEN GOTO L50230
REM L50250
            if scr_code% <> 1% then return
               init(" ") scr_dept$, scr_msg1$, scr_load$, scr_desc$,     ~
                         scr_dte$, scr_dte1$, screen_dte$, screen_dte1$, ~
                         scr_shft$, scr_shft_d$, scr_so$
               gosub begin_process
        return
L50290:     errormsg$ = "(Error) - Invalid Screen's Selection?"
            gosub error_prompt
            init(" ") scr_code$, scr_msg$
        return

L50340: REM Planned Production Date               SCR_DTE$, SCR_DTE1$
           date% = 0%
           call "DATEOK" (scr_dte$, date%, errormsg$ )

           if errormsg$ <> " " then return
              scr_dte1$ = scr_dte$
              scr_batch$ = scr_dte$ & " PRODUCTION "
              call "DATUNFMT" (scr_dte1$)
              dt_key1$ = all(hex(00))
              str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
              read #2,key 1% > dt_key1$, using L50440, dt_key1$,          ~
                                                      eod goto L50500
L50440:         FMT POS(47), CH(57)
              if str(scr_dte1$,1%,6%) <> str(dt_key1$,1%,6%) then        ~
                                                             goto L50500
              screen_dte$  = scr_dte$
              screen_dte1$ = scr_dte1$

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
L50500:    errormsg$ = "(Error) - No Data for Specified Date?"
           gosub error_prompt
           init(" ") scr_dte$, scr_dte1$, dt_key1$
        return

L50550: REM Department Code                       SCR_DEPT$, SCR_MSG1$
            if scr_dept$ <> "  " then goto L50610
L50570:        scr_dept$ = "ALL"
               scr_dept% = 0%
               scr_msg1$ = "(All) Departments "
               return
L50610:     if str(scr_dept$,1%,1%) = "A" then goto L50570
            convert scr_dept$ to scr_dept%, data goto L50700

            convert scr_dept% to scr_dept$, pic(000)

            gosub lookup_dept
            if dept% = 0% then goto L50700
            scr_msg1$ = descr$
        return
L50700:     errormsg$ = "(Error) Invalid Department Code? "
            gosub error_prompt
            init(" ") scr_dept$, scr_msg1$
        return

L50750: REM Department Shift Code               SCR_SHFT$, SCR_SHFT_D$
           if scr_shft$ <> " " then goto L50800
L50770:       scr_shft$ = "AA"
              scr_shft_d$ = "(All) Shifts"
              return
L50800:    if str(scr_shft$,1%,1%) = "A" then goto L50770
              init(" ") readkey$, scr_shft_d$
              str(readkey$,1%,9%) = "PLAN SHFT"
              str(readkey$,10%,15%) = scr_shft$
              read #3,key = readkey$, using L50860, scr_shft_d$,          ~
                                                   eod goto L50880
L50860:          FMT POS(25), CH(30)
        return
L50880:    errormsg$ = "(Error) Invalid Department Shift Code?"
           gosub error_prompt
           init(" ") scr_shft$, scr_shft_d$
        return
        
L50885: REM Sales Order Number  scr_so$   CR2653
           if scr_so$ <> " " and scr_so$ <> "ALL" then goto L50886
           scr_so$ = "ALL"
           return
L50886:    str(sp_key$,01,08) = scr_so$
           str(sp_key$,09,01) = " "
           str(sp_key$,10,02) = "  "
           read #63,key > sp_key$,using BKSUBPT, sp_rec$, eod goto L50889
              if str(sp_rec$,1%,8%) <> scr_so$ then goto L50889
           return

L50889:    errormsg$ = "(Error) Invalid Sales Order?"
           gosub error_prompt
           init(" ") scr_so$
        return

L50930: REM LOAD NUMBER
           if scr_code% = 3% then goto sched_edit

           if len(scr_load$) < 3 then return
           convert scr_load$ to scr_load%, data goto L51010

           convert scr_load% to scr_load$, pic(00000)
           goto L51050
L51010:    convert str(scr_load$,2%,4%) to scr_load%, data goto L51150

           convert scr_load% to str(scr_load$,2%,4%), pic(0000)

L51050:    dt_key3$ = all(hex(00))
           str(dt_key3$,1%,6%) = scr_load$
           read #2,key 3% > dt_key3$, using L51090, dt_key3$,   stk_so$,  ~
                                                           eod goto L51150
L51090:       FMT CH(23), CH(8)
           if scr_load$ <> str(dt_key3$,1%,5%) then goto L51150
              scr_desc$ = "Standard Load S.O. = "& stk_so$
           if str(scr_load$,1%,1%) = "S" then                            ~
              scr_desc$ = "Stock Load S.O. = "& stk_so$
        return
L51150:    errormsg$ = "(Error) - Invalid Standard Load Number?"
           if str(scr_load$,1%,1%) = "S" then                            ~
              errormsg$ = "(Error) - Invalid Stock Load Number?"
           gosub error_prompt
           init(" ") scr_load$, scr_desc$, stk_so$, dt_key3$
        return
L51030: REM PULL FROM INVENTORY FLAG                SCR_INV$, SCR_PDESC$
            if scr_inv$ = " " then scr_inv$ = "Y"
            if scr_inv$ <> "Y" and scr_inv$ <> "N" then goto L51930
            scr_pdesc$ = "Pull from Screen Inventory"

            if scr_inv$ = "N" then scr_pdesc$ = "Don't Pull From Inventory"
        return
L51930:    errormsg$ = "(Error) Invalid Pull Selection 'Y' or 'N'?"
           gosub error_prompt
           init(" ") scr_inv$, scr_pdesc$
        return

        sched_edit
            gosub lookup_schedule

            convert scr_load$ to sched%, data goto L51300

            if sched% < 100% or sched% > 999% then goto L51300
            scr_desc$ = "Default Batch Size = 100"
        return
L51300:     errormsg$ = "(Error) - Invalid Schedule Number?"
            gosub error_prompt
            init(" ") scr_load$, scr_desc$, stk_so$, dt_key3$
        return

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51410          /* Stock Part Number     */

            return

L51410: REM Stock Part Number                     DT_PART$
            init(" ") cut$()
            if dt_part$ <> " " then goto L51480
               dt_desc$ = " "
               dt_desc$ = hex(06) & "Select a Valid Stock Part"
               call "GETCODE" (#7,dt_part$, dt_desc$, 0%, 1.32, f1%(7%))

L51480:     dt_desc$ = "( Non Stock Product!! )"
            read #7,key = dt_part$, using L51500,dt_desc$,eod goto L51510
L51500:        FMT POS(26), CH(32)
L51510:     if len(dt_part$) < 19 then goto L51580
                                                /* No Screen Skip Part */
            if str(dt_part$,11%,1%) = "0" then goto L51580
            if str(dt_part$,11%,1%) = "V" then goto L51580 /*(SR80152)*/
               scr_dept$ = "ALL"
               scr_shft$ = "AA"

/* (AWD029) */
               dim1es, dim2es, dim3es = 0.00
               convert dim1es$ to dim1es, data goto bad_dim1
bad_dim1:
               convert dim2es$ to dim2es, data goto bad_dim2
bad_dim2:
               convert dim3es$ to dim3es, data goto bad_dim3
bad_dim3:

               convert dim1es to dim1es$, pic(######0.00)
               convert dim2es to dim2es$, pic(######0.00)
               convert dim3es to dim3es$, pic(######0.00)
/*(\AWD029)*/
            gosub L51630
        return
L51580:     init(" ") dt_part$, model$, dt_load$, dt_desc$, cut$()

            errormsg$ = "(Error) - Invalid Part Number Entry."
        return

L51630: REM CUT$() Display Cut Sizes              CUT$()
              cut$(1%) =                                                 ~
         "(1) WD =           HT =          CB =           CL =          "
              cut$(2%) =                                                 ~
         "(2) WD =           HT =          CB =           CL =          "
              cut$(3%) =                                                 ~
         "(3) WD =           HT =          CB =           CL =          "
              cut$(4%) =                                                 ~
         "(4) WD =           HT =          CB =           CL =          "
                                                /* Width  = Pos(10,9) */
                                                /* Height = Pos(25,8) */
                                                /* Cb     = Pos(39,9) */
                                                /* Cl     = Pos(54,8) */
        return

screen_remakes   /* <AWD017> */
REM CR1036        scr_rmk_loc$ = "W"
REM CR1036        if scr_code% = 2% then scr_rmk_loc$ = "G"
REM CR1036        if userid$ = "ASM" then scr_rmk_loc$ = "G"
REM CR1036        if userid$ = "APN" then scr_rmk_loc$ = "W"
REM CR1036        if userid$ = "SCN" then scr_rmk_loc$ = "W"
REM scr_batch$ = date$ & " Remake       "
    init(" ") scr_batch$
screen_loop
        gosub batch_header
        if keyhit% = 1% then goto end_remake
        if keyhit% = 16% then goto end_program
        if keyhit% <> 14% then goto screen_loop
        print at (21,02), "PROCESSING FILE                               "
REM CR1036        if scr_rmk_loc$ <> "G" and scr_rmk_loc$ <> "W" then        ~
REM CR1036        goto screen_remakes
        init(" ") inpmessage$
        inpmessage$ = "                                         "
        ws_loc$ = "3"
REM CR1036         if scr_rmk_loc$ = "G" then ws_loc$ = "2"
        init(hex(00)) sr_key1$, sr_rec$()
        str(sr_key1$,1,1) = ws_loc$
next_remake:
REM bad_num
          read #20%, hold, key 1% > sr_key1$, using sr_fmt1, sr_rec$(),   ~
                                                    eod goto end_remake
        sr_key1$     =  str(sr_rec$(),7,47)
    /* @@@ */
    /* if not remake GSO or remake WEL skip */
    /* only add for selected location  2=gso. 3=wel  */
        if str(sr_key1$,1,1) <> ws_loc$ then goto end_remake
        /* if status not zero, skip                            */
        if str(sr_rec$(),163,1) <> "0" then goto next_remake
REM         /* is reason code not "000" to "099" it isn't a remake */
REM !        if str(sr_rec$(),223,1) <> "0" then goto next_remake
        /* skip those in a batch already                       */
REM     if str(sr_rec$(),176,1) =  "R" then goto next_remake
        if str(sr_rec$(),52,2)  = "00" then goto next_remake
        str(sr_rec$(),14,20) = scr_batch$      /* Batch Name         */
        str(sr_rec$(),34,5) = "99999"          /* Batch Number (const) */
        str(sr_rec$(),176,1)  = "R"
        str(sr_rec$(),163,1)  = "1"
        delete #20%
        write #20%, using sr_fmt1, sr_rec$()
    goto next_remake
end_remake:
        goto end_ora_remake    /* (CR1036) Stop loop.  Oracle not needed */
        sr_process$ = ws_loc$
        sr_date$      = "20010911"
        sr_batch$   = "                    "
        sr_batch_num$ = "     "
        sr_dept$    = "   "
        sr_barcode$ = "         "
        sr_remake$  = "   "
        init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                  stmt6$, stmt7$, stmt8$
        stmt1$ = "SELECT PROCESS, DATE, BATCH, BATCH_NUM, DEPT, BARCODE," & ~
             " REMAKE_NUM FROM MFG.SCREEN WHERE PROCESS = '" &  sr_process$ & ~
             "' AND STATUS = '0' AND SUBSTR(REMAKE_NUM,2,2) > '00'" &  ~
             "' AND (DATE > to_date('" & sr_date$ & "','YYYYMMDD')" & ~
             " OR (DATE = to_date('" & sr_date$ & "','yyyymmdd') AND" & ~
             " BATCH > '" & sr_batch$ & "') OR " & ~
             "(DATE = to_date('" & sr_date$ & "','yyyymmdd')"
        stmt2$ = " AND BATCH = '" & sr_batch$ & "' AND BATCH_NUM > '" &   ~
             sr_batch_num$ & "') OR " &                               ~
             "(DATE = to_date('" & sr_date$ & "','yyyymmdd') AND" & ~
             " BATCH = '" & sr_batch$ & "' AND BATCH_NUM = '" &        ~
             sr_batch_num$ & "' AND DEPT > '" & sr_dept$ & "') OR " &  ~
             "(DATE = to_date('" & sr_date$ & "','yyyymmdd') AND" & ~
             " BATCH = "
        stmt3$ = "'" & sr_batch$ & "' AND BATCH_NUM = '" &        ~
             sr_batch_num$ & "' AND DEPT = '" & sr_dept$ & "' AND " & ~
             "BARCODE > '" & sr_barcode$ & "') OR " &                  ~
             "(DATE = to_date('" & sr_date$ & "','yyyymmdd') AND" & ~
             " BATCH = '" & sr_batch$ & "' AND BATCH_NUM = '" &        ~
             sr_batch_num$ & "' AND DEPT = '" & sr_dept$ & "' AND " &   ~
             "BARCODE = '" & sr_barcode$ & "' AND REMAKE_NUM > "
        stmt4$ = "'" & sr_remake_num$ & "' ORDER BY DATE, BATCH, BATCH_NUM," & ~
             " DEPT, BARCODER, REMAKE"

        /* process, date, batch, bat_num, dept, bar, remake_num */
REM        GOSUB ORACLE_QUERY
next_ora_remake:
REM        gosub oracle_fetch
            if oci_err% < 0% then goto next_ora_remake
            if oci_err% = 100% then goto end_ora_remake
            for field_num% = 1%  to no_fields%
                gosub oracle_getfield
                if field_num% = 1%     then sr_process$    = field$
                if field_num% = 2%     then sr_date$       = field$
                if field_num% = 3%     then sr_batch$      = field$
                if field_num% = 4%     then sr_batch_num$  = field$
                if field_num% = 5%     then sr_dept$       = field$
                if field_num% = 6%     then sr_barcode$    = field$
                if field_num% = 7%     then sr_remake_num$ = field$
            next field_num%

        /* if not remake GSO or remake WEL skip */
        /* only add for selected location  2=gso. 3=wel  */
        /* if status not zero, skip                            */

        if sr_status$ <> "0" then goto next_ora_remake
        if str(sr_remake_num$,2,2)  = "00" then goto next_ora_remake
        stmt1$ = "UPDATE MFG.SCREENS SET BATCH = " & sr_batch$ &      ~
         ", BATCH_NUM = '99999', CUT_PULL = 'R', STATUS = '1'" & ~
         " WHERE BARCODE = '" & sr_barcode$ & "' AND REMAKE_NUM ='" & ~
         sr_remake$ & "'"
        gosub oracle_exec1

        goto next_ora_remake

end_ora_remake:
        return clear all
        goto inputmode
REM     return

        batch_header
            gosub set_pf3
REM L43080:
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
       at (03,16), "***************************************************", ~
       at (04,16), "*                                                 *", ~
       at (05,16), "*  Create Screen Remake Batch                     *", ~
       at (06,16), "*                                                 *", ~
       at (07,16), "***************************************************", ~
               at (09,08), "Batch Name:",                        ~
               at (09,25), fac(hex(81)), scr_batch$             , ch(20),~
                                                                         ~
                                                                         ~
       at (13,16), "Note: Please note that the 'Batch Name' will be  "  ,~
       at (14,16), "      used and necessary when you are ready to   "  ,~
       at (15,16), "      print the screen remake labels.            "  ,~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
REM L43310:
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
            inpmessage$ = "Enter Batch Name and Location             "
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (14)Create Batch"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0eff1000)

            return

        get_batch_name
            gosub set_pf4
REM L43480:
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
       at (03,16), "***************************************************", ~
       at (04,16), "*                                                 *", ~
       at (05,16), "*  Enter Batch Name                               *", ~
       at (06,16), "*                                                 *", ~
       at (07,16), "***************************************************", ~
               at (09,08), "Batch Name:",                        ~
               at (09,25), fac(hex(81)), scr_batch2$            , ch(20),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
            inpmessage$ = "Enter Batch Name                          "
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (14)Continue    "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0eff1000)

            return

        display_departments
           readkey$ = " "
           str(readkey$,1%,9%) = "SCREEN05 "
           descr$ =hex(06) & "Dept Batch/Sort/Descr Info"
           call "PLOWCODE" (#3, readkey$, descr$, 9%, .30, f1%(2%))
        return

        lookup_dept
           dept% = 0%
           init(" ") readkey$, descr$
           if scr_dept$ = "066" then return      /* (EWD001)        */
                                                 /* Skip Literature */
           str(readkey$,1%,9%) = "PLAN DEPT"
           str(readkey$,10%,15%) = scr_dept$
           read #3,key = readkey$, using L51910, descr$
L51910:       FMT POS(25), CH(30)
           dept% = 1%
        return

        lookup_bayform                         /* Find out if product to */
           byfmodel% = 0%                      /* send to bayform  EWD002*/
REM           if sc$ = "A" or sc$ = "J" then return         /* (AWD014) */
           if scr_dept$ <> "ALL" or scr_load$ <> " " then return

/*(AWD015) do not send 267 or 219 SCO extruded half screen J */

REM           if model$ = "267" and sc$ = "J" then return
REM           if model$ = "219" and sc$ = "J" then return


           init(" ") readkey$, descr$
           str(readkey$,1%,9%) = "SCREENBFM"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, using L51910, descr$,   ~
                                            eod goto L51990
/* (AWD014) */
/* only send models with an 'R'oll Form to bayform if screen */
/* code is B or C for full and hall roll form                */
               if str(descr$,30,1) <> "R" then goto not_roll

                    if sc$ <> "B" and sc$ <> "C" then return
not_roll:
           byfmodel% = 1%
L51990: return

        lookup_ritescreen                    /* Find out if product to   */
           rtemodel% = 0%                    /* send to ritescreen EWD005*/
           if scr_dept$ <> "ALL" or scr_load$ <> " " then return
           init(" ") readkey$
           str(readkey$,1%,9%) = "SCREENRSN"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, eod goto L51995
           rtemodel% = 1%
L51995: return

/* <AWD013> */
        lookup_barcode
           barmodel% = 0%              /* barcode if not bfm or rte */
                                 /* Extruded screen do not go to ASM */
REM           if sc$ = "A" or sc$ = "J" then return         /* (AWD014) */

/*(AWD015) do not send 267 or 219 SCO extruded half screen J */

REM           if model$ = "267" and sc$ = "J" then return
REM           if model$ = "219" and sc$ = "J" then return

           init(" ") readkey$, descr$
           str(readkey$,1%,9%) = "SCREENBFM"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, using L51910, descr$,   ~
                                          eod goto L52000
               if str(descr$,30,1) <> "R" then goto not_roll1

                    if sc$ <> "B" and sc$ <> "C" then return
not_roll1:

           barmodel% = 1%
       return

L52000:    str(readkey$,1%,9%) = "SCREENRSN"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, eod goto L52005
           barmodel% = 2%
L52005:    return
/* </AWD013> */

        lookup_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #3,key = readkey$, using L52010, sched$,                 ~
                                                           eod goto L52040
L52010:       FMT POS(25), CH(3)
            scr_load$ = sched$
        return
L52040:    call "SHOSTAT" ("Error- Assigning Schedule Number?")
           stop
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                      /* Report Header */
L55050: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-+

L55090: %!########@########                                  ############~
        ~#############                                           Page: ###~
        ~ !

L55130: %! Production Date: ########                        Screen Consol~
        ~idation Report                               Department Code: ###~
        ~ !

L55170: %! Production Date: ########                        Screen Center~
        ~Bar Cut Report                               Department Code: ###~
        ~ !

L55135: %! Production Date: ########                        Screen Consol~
        ~idation Report (BAYFORM)                     Department Code: ###~
        ~ !

L55175: %! Production Date: ########                        Screen Center~
        ~Bar Cut Report (BAYFORM)                     Department Code: ###~
        ~ !

REM L55140:
        %! Production Date: ########                        Screen Consol~
        ~idation Report (BAYFORM)                     Department Code: ###~
        ~ !

L55180: %! Production Date: ########                        Screen Center~
        ~Bar Cut Report (BAYFORM)                     Department Code: ###~
        ~ !

L55185: %! Production Date: ########                        Screen Consol~
        ~idation Report (RITESCREEN)                  Department Code: ###~
        ~ !

L55190: %! Production Date: ########                        Screen Center~
        ~Bar Cut Report (RITESCREEN)                  Department Code: ###~
        ~ !

L55210: %!                                                               ~
        ~                                                                 ~
        ~ !

L55250: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-!
                                                              /* Screen*/
L55290: %!Mod! Qty !Cl!  Width       Height !<----- Type ------->!Cb Cut ~
        ~Ln!<Center Bar Cut Location>!<------ Hinge ----->!  Window Size  ~
        ~ !
                                                              /* SCREEN*/
L55330: %!---!-----!--!---------------------!--------------------!-------~
        ~--!-------------------------!--------------------!---------------~
        ~-!
                                                              /* SCREEN*/
L55370: %!###!#####!##!######### BY ########!####################!#######~
        ~##!#########################!####################!####### X #####~
        ~#!
                                                              /* Totals*/
L55410: %! Batch No.: ###   Tot Screens: #####   Tot Half Screens: ##### ~
        ~ Tot Full Screens: #####  Tot Cot/Oriel: ####  Tot Samp/Disp: ###~
        ~ !
                                                              /* Labels*/
L55450: %# ### ## ####-########-CL######### ######## #####
L55460: %#########X######## CB######### A:#######X###### #

        REM - Test Labels (len = 49)
L55490: %XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

        REM - Printer Equation Errors
L55520: %S.O. ######## Seq:##### #########################

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header                         /* GENERIC REPORT HEADING */
          if lcnt% <> 99% then print using L55050
          pageno% = pageno% + 1%
          print page
          print using L55050
          print using L55090, date$, rpt_time$, title$, pageno%
          if f% = 11% then goto L55600
          if f% = 14% then goto L55620
          if pass% = 0% then print using L55130, scr_dte$, dt_dept$       ~
                        else print using L55170, scr_dte$, dt_dept$
          goto L55610

L55600:   if pass% = 0% then print using L55135, scr_dte$, dt_dept$       ~
                        else print using L55175, scr_dte$, dt_dept$
          goto L55610
L55620:   if pass% = 0% then print using L55185, scr_dte$, dt_dept$       ~
                        else print using L55190, scr_dte$, dt_dept$
          goto L55610

L55610:   print using L55250
          print using L55290
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 60% then gosub print_header

             print using L55330
             print using L55370, model$, qty$, cl_l$, sav_wd1$, sav_ht1$, ~
                             sc_r$, wd2$, grid$, hnge$, sav_wd$, sav_ht$
             lcnt% = lcnt% + 2%
        return

        print_total
          if f% = 11% then goto L60210                 /* (EWD002) */
          if f% = 14% then goto L60215                 /* (EWD005) */
          print using L55250
          print using L55210
          print using L55410, dd%, batch%(dd%,2%), batch%(dd%,3%),       ~
                                  batch%(dd%,4%), batch%(dd%,5%),        ~
                                  batch%(dd%,6%)
          print using L55050
          lcnt% = 99%
        return
L60210:   print using L55250                        /* (EWD002) - Begin */
          print using L55210
          print using L55410, dd%, byfbatch%(dd%,2%), byfbatch%(dd%,3%), ~
                                  byfbatch%(dd%,4%), byfbatch%(dd%,5%),  ~
                                  byfbatch%(dd%,6%)
          print using L55050
          lcnt% = 99%
        return                                       /* (EWD002) - End */
L60215:   print using L55250                         /* (EWD005) - Begin */
          print using L55210
          print using L55410, dd%, rtebatch%(dd%,2%), rtebatch%(dd%,3%), ~
                                  rtebatch%(dd%,4%), rtebatch%(dd%,5%),  ~
                                  rtebatch%(dd%,6%)
          print using L55050
          lcnt% = 99%
        return                                       /* (EWD005) - End */

        print_labels_detail
          if str(lab_rec$,6%,3%) = "ERR" then goto L60220
      /* <AWD013> */
      gosub lookup_barcode
REM        barmodel% = 0 for in house, 1 for bfm, 2 for rte

REM      Write #18.... @@@@@
          init(" ") scrn_rec$
          str(scrn_rec$,01,08) = dt_date$
          str(scrn_rec$,09,03) = dt_dept$
          str(scrn_rec$,12,05) = dt_seq$
          str(scrn_rec$,17,18) = dt_barcode$
          str(scrn_rec$,35,03) = dt_dept$
          str(scrn_rec$,38,02) = dt_proc$
          str(scrn_rec$,40,08) = dt_so$
          str(scrn_rec$,48,01) = "0"
          str(scrn_rec$,49,04) = dt_so_line$
          str(scrn_rec$,101,1) = "N"
          str(scrn_rec$,102,1) = "I"
          if barmodel% = 1% then str(scrn_rec$,102,1) = "B"
          if barmodel% = 2% then str(scrn_rec$,102,1) = "R"
          str(scrn_rec$,53,3) = str(dt_part$,1,3)
          str(scrn_rec$,56,2) = cl_l$
          str(scrn_rec$,58,4) = sc_l$
          str(scrn_rec$,62,9) = wd1$
          str(scrn_rec$,71,8) = ht1$
          str(scrn_rec$,79,9) = wd2$
          str(scrn_rec$,88,7) = wd$
          str(scrn_rec$,95,6) = ht$
          str(scrn_rec$,103,9) = ht2$
          str(scrn_rec$,112,1) = txt_flag$
          str(scrn_rec$,113,1) = view$
          str(scrn_rec$,114,1) = "0"  /* remake  */
          str(scrn_rec$,115,1) = "0"  /* status  */
          str(scrn_rec$,116,25) = dt_part$

          temp1$ = str(scrn_rec$,01,64)
          temp2$ = str(scrn_rec$,65,64)

          write #18, using SCRNAUDT, scrn_rec$, eod goto SCRNAUDT

SCRNAUDT: FMT CH(160)

      /* </AWD013> */
             print using L55450, str(view$,1%,1%), model$, cl_l$, sc_l$,  ~
                                co_or$, ht2$ , dt_so$, dt_seq$
             print using L55460, wd1$, ht1$, wd2$, wd$, ht$, txt_flag$
             print
        return

L60220:   print using L55520, dt_so$, dt_seq$, str(lab_rec$,11%,25%)
          print using L55520, dt_so$, dt_seq$, str(lab_rec$,11%,25%)
          print
        return

        convert_fields
            if sav_part$ = dt_part$ then return
            init(" ") special$, ph$(), sav_part$
            s_width  = 0.0               /* Window Width in Decimal   */
            s_height = 0.0               /* Window Height in Decimal  */
            s_clmr   = 0.0               /* Center Bar in Decimal     */
            s_half   = 0.0               /* Half Screen Calc Constant */
            s_full   = 0.0               /* Full Screen Calc Constant */
            special$ = "0"               /* '0'=Ctr Bar Not Specified */
            model$  = str(dt_part$,1%,3%)             /* Model Number */
            cl$     = str(dt_part$,4%,1%)             /* Color        */

/* (AWD012)  only change color if casement */
            if cl$ = "3" and dt_dept$ = "008" then cl$ = "2"
/* (AWD012/) */
            gl$     = str(dt_part$,5%,2%)             /* Glass        */
            lt$     = str(dt_part$,7%,2%)             /* Liting       */
            hg$     = str(dt_part$,9%,2%)             /* Hinge        */
            sc$     = str(dt_part$,11%,1%)            /* Screen       */
            lk$     = str(dt_part$,12%,1%)            /* Locks        */
            width$  = str(dt_part$,13%,4%)            /* Width        */
            height$ = str(dt_part$,17%,3%)            /* Height       */
            clmr$   = str(dt_part$,20%,3%)            /* CLMR         */
            wallw$  = str(dt_part$,23%,3%)            /* WALLWIDT     */
            sav_part$ = dt_part$
            gosub std_wd_ht                    /* Window WD$ & HT$    */
            gosub lookup_color                 /* CL_L$ & COLOR$      */
            gosub lookup_hinge                 /* HG_L$ & HH$         */
            gosub lookup_screen                /* SC_R$ & SC_L$       */
                                               /* Convert Width/Height*/
               a1, a2 = 0.0                    /* and Ctr Bar to Decim*/
               convert str(width$,1%,3%) to a1, data goto L60385
L60385:
               convert str(width$,4%,1%) to a2, data goto L60395
L60395:
               s_width = a1 + (a2/8.0)         /* Decimal Width       */
               a1, a2 = 0.0
               convert str(height$,1%,2%) to a1, data goto L60415
L60415:
               convert str(height$,3%,1%) to a2, data goto L60425
L60425:
               s_height = a1 + (a2/8.0)        /* Decimal Height      */
               a1, a2 = 0.0
            if len(dt_part$) < 22 then goto L60495
            if hg$ < "70" or hg$ > "97" then goto L60495
               convert str(clmr$,1%,2%) to a1, data goto L60470

               convert str(clmr$,3%,1%) to a2, data goto L60460
L60460:
               s_clmr = a1 + (a2/8.0)
L60470:        if len(dt_part$) > 22 then goto L60485
                     if dt_wood$ <> "000" and dt_wood$ <> "   "      ~
                           then s_clmr = 0.0
L60485:        if s_clmr <= 8.0 then s_clmr = 0.0
               if s_clmr > 0.0 then gosub get_special /* Specified CB  */
L60495:
                                              /* (AWD014) */
            ss1% = 1%
            /* (AWD019) */
REM            if sc$ = "B" or sc$ = "C" then ss1% = 21%
            if sc$ = "B" or sc$ = "C" then ss1% = rp%

         REM                                  /* Find Phantom's for Calc*/
/* (AWD019) */
         init(" ") savSKey$
         savSKey$ = skey$
REM         str(skey$,6,1) = cl$       /* Look for Specific color first */
         if str(dt_part$,11%,1%) = "D" then goto lockingScreen
/* (CR688) */
REM         if str(dt_part$,1%,3%) = "S25" then goto skip_cl_chk
REM         if str(dt_part$,1%,3%) = "S26" then goto skip_cl_chk
REM         if str(dt_part$,1%,3%) = "S27" then goto skip_cl_chk
REM         if str(dt_part$,1%,3%) = "S35" then goto skip_cl_chk
REM         if str(dt_part$,1%,3%) = "E02" then goto skip_cl_chk
REM         if str(dt_part$,1%,3%) = "E03" then goto skip_cl_chk

         for ss% = ss1% to ss_max%
REM             if str(ss$(ss%),1%,4%) = skey$ then goto L60545
             if str(skey$(ss%),1%,6%) = str(skey$,1%,6%) then goto L60545
         next ss%
REM skip_cl_chk:
        /* Second don't look for specific color */
        init(" ") skey$
        skey$ = savSKey$
            for ss% = ss1% to ss_max%
REM                if str(ss$(ss%),1%,4%) = skey$ then goto L60545
REM this is needed so it doesn't match the first five characters with a color in
REM   then sixth position
                if str(skey$(ss%),6%,1%) <> " " then goto nextSS
                if str(skey$(ss%),1%,5%) = str(skey$,1%,5%) then goto L60545
nextSS
            next ss%
/* (AWD019\) */
        error% = 4%                          /* Product Error for MFG  */
        if scr_code% <> 1% then return
           errormsg$ = err$(error%)
           gosub error_prompt
           init(" ") errormsg$
        return
lockingScreen
         init(" ") savSKey$
         savSKey$ = skey$
         str(skey$,6,1) = cl$           /* Look for Specific color first */
         str(skey$,7%,1%) = "L"
         for ss% = ss1% to ss_max%
             if str(skey$(ss%),1%,7%) = str(skey$,1%,7%) then goto L60545
         next ss%

        /* Second don't look for specific color */
        init(" ") skey$
        skey$ = savSKey$
        str(skey$,6,1) = "L"
        for ss% = ss1% to ss_max%
REM                if str(ss$(ss%),1%,4%) = skey$ then goto L60545
REM this is needed so it doesn't match the first five characters with a color in
REM   then sixth position
             if str(skey$(ss%),6%,1%) <> "L" then goto nextSS_A
             if str(skey$(ss%),1%,6%) = str(skey$,1%,6%) then goto L60545
nextSS_A
         next ss%


L60545:
REM            ph$(1%) = str(ss$(ss%),5%,4%)     /* Cut Width Calc       */
REM            ph$(2%) = str(ss$(ss%),9%,4%)     /* Cut Height Calc      */
REM            ph$(3%) = str(ss$(ss%),13%,4%)    /* CB Cut Length        */
REM            ph$(4%) = str(ss$(ss%),17%,4%)    /* CB Cut Location      */
       /* (AWD019) */
            ph$(1%) = str(ss$(ss%),1%,4%)     /* Cut Width Calc        */
            ph$(2%) = str(ss$(ss%),5%,4%)     /* Cut Height Calc       */
            ph$(3%) = str(ss$(ss%),9%,4%)     /* CB Cut Length         */
            ph$(4%) = str(ss$(ss%),13%,4%)    /* CB Cut Location       */

        return                                /* Note '0000' = N/A     */

        get_special                           /* Specified Meeting Rail*/
/* (AWD014) */
            if sc$ = "B" or sc$ = "C" then goto get_special1
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = "SCREEN06 "
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$, using L60600, descr$, eod goto L60640
L60600:        FMT POS(25), CH(30)
/*CR2217*/
             convert str(descr$,1%,9%) to s_half, data goto L60610
L60610:
             convert str(descr$,11%,9%) to s_full, data goto L60620
L60620:
            if sc$<> "W" and sc$ <> "X" then goto notFlex
             convert str(descr$,21%,9%) to s_half, data goto L60630
L60630:
notFlex:              

            special$ = "1"                    /* Specified Ctr Bar     */
            str(skey$,2%,2%) = "SP"           /* 'SP' = Specified      */
        return
L60640:     error% = 3%
            if scr_code% <> 1% then return
            errormsg$ = err$(error%)
            gosub error_prompt
            init(" ") errormsg$
        return

        get_special1
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = "SCREEN08 "
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$, using L60600, descr$, eod goto L60645

            convert str(descr$,1%,9%) to s_half, data goto L60615
L60615:
            convert str(descr$,11%,9%) to s_full, data goto L60625
L60625:
            special$ = "1"                    /* Specified Ctr Bar     */
            str(skey$,2%,2%) = "SP"           /* 'SP' = Specified      */
        return
L60645:     error% = 3%
            if scr_code% <> 1% then return
            errormsg$ = err$(error%)
            gosub error_prompt
            init(" ") errormsg$
        return


        check_support                                 /* SUPPORT DEPTS */
            init(" ") readkey$ : supp% = 0%
            str(readkey$,1%,9%)   = "PLAN SUPP"
            str(readkey$,10%,15%) = dt_dept$
            read #3,key = readkey$, eod goto L60710
            supp% = 1%
        return                     /* No Screen Processing for Dept's  */
                                   /* David take out dept 056  */
                                   /*(AWD018) */
L60710:     REM if dt_dept$ = "023" or dt_dept$ = "042"  then supp% = 1%
            if dt_dept$ = "042"  then supp% = 1%
            if door% = 0% and dt_dept$ = "023" then supp% = 1%
            if door% = 0% and dt_dept$ = "020" then supp% = 1%
                                   /* (AWD018\) */
        return

        lookup_color                                  /* Look Up Color */
            init(" ") readkey$, descr$, cl_l$, color$
            str(readkey$,1%,9%)   = "COLOR    "
            str(readkey$,10%,15%) = cl$
            read #3,key = readkey$, using L60755, descr$,eod goto L60775
L60755:        FMT POS(25), CH(30)
            cl_l$   = str(descr$,1%,2%)
            color$  = str(descr$,6%,6%)
        return
L60775:     color$ = "N/A"
        return

        lookup_hinge                                  /* Look Up Hinge */
            triple% = 0%
            init(" ") readkey$, descr$, hnge$, hg_l$, hh$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hg$
            read #3,key = readkey$, using L60755, descr$, eod goto L60865
            p% = pos(descr$ = "-")
            hnge$ = str(descr$,p%+2%)
            if str(descr$,p%+2%,2%) = "  " then                          ~
                    hnge$ = str(descr$,1%,p%-1%)
            hg_l$ = str(descr$,1%,p%-2%)
            if str(descr$,p%-4%,3%) = "1/3" then triple% = 1%
            if str(hg_l$,1%,2%) = "CO" then hh$ = "COTTAGE"
            if str(hg_l$,1%,2%) = "OR" then hh$ = "ORIEL  "
        return
L60865:     hnge$ = "(Error) - Invalid Hinge Code?"
        return

        lookup_screen                                /* Look Up SCREEN */
            init(" ") readkey$, descr$, sc_l$, sc_r$
            str(readkey$,1%,9%)   = "SCREEN   "
            str(readkey$,10%,15%) = sc$
            read #3,key = readkey$, using L60755, descr$, eod goto L60925
            p% = pos(descr$ = "-")
            sc_r$ = str(descr$,1%,20%)
            sc_l$ = str(descr$,1%,p%-2%)
        return
L60925:     sc_r$ = "(error) - Invalid Screen Code?"
        return

        std_wd_ht             /* Convert Standard Width and Height    */
                              /* F0%       - Fract. New Part Width    */
                              /* F1%       - Fract. New Part Height   */
                              /* WD$   - Width & Fract (7) in 8'ths   */
                              /* HT$   - Height & Fract(6) in 8'ths   */
           str(wd$,1%,3%)  = str(dt_part$,13%,3%)   /* Width Part (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(dt_part$,17%,2%)    /* Height Part (2)*/

           f0%, f1% = 0%                            /* Set Fractions  */
           convert str(dt_part$,16%,1%) to f0%, data goto L61015 /*WID */

           convert str(dt_part$,19%,1%) to f1%, data goto L61015 /*HGT */

           goto L61020
L61015:      f0%, f1% = 8%
L61020:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%

           str(wd$,4%,1%) = " "          /* Build Width with Fraction */
           str(wd$,5%,3%) = sze$(f0%)    /* in 8'ths                  */

           str(ht$,3%,1%) = " "          /* Build Height with Fraction*/
           str(ht$,4%,3%) = sze$(f1%)    /* in 8'ths                  */
        return

        calc_wd_ht             /* CONVERT STANDARD WIDTH AND HEIGHT */
                              /* WD1$  - REPLACEMENT WIDTH & FRACT (9)*/
                              /* HT1$  - REPLACEMENT HEIGHT & FRACT(8)*/
           calc = sav_width1                  /* CALCULATED WIDTH (3) */
           gosub convert_sixteen
           wd1$ = "       "
           convert a% to str(wd1$,1%,3%),pic(###)   /* WIDTH (3)      */

           if b% = 0% then goto L61125
              str(wd1$,5%,5%) = sze1$(b%)           /* SET FRACTIONS  */

L61125:    calc = sav_height1
           gosub convert_sixteen
           ht1$ = "       "
           convert a% to str(ht1$,1%,2%), pic(##)   /* HEIGHT (2)     */

           if b% = 0% then goto L61160
              str(ht1$,4%,5%) = sze1$(b%)
L61160: return

        calc_wd                                   /* Convert CenterBar */
           wd2$ = "         "                     /* CB Cut            */
           ht2$ = "       "                       /* Center Bar Loc    */
           if sav_width2 > 0.0 then goto L61195
              return                              /* Not Applicable    */
L61195:    calc = sav_width2                      /* Convert Center Bar*/
           gosub convert_sixteen

           convert a% to str(wd2$,1%,3%),pic(###) /* Center Bar Cut   */

           if b% = 0% then goto L61235
              str(wd2$,5%,5%) = sze1$(b%)         /* Set Fractions    */

L61235:    calc = sav_height2                     /* Calc Ctr Loc     */
           gosub convert_sixteen
           convert a% to str(ht2$,1%,2%), pic(##) /* Height (2)       */

           if b% = 0% then goto L61265
              str(ht2$,4%,5%) = sze1$(b%)
L61265: return

        convert_sixteen
              calc = round( calc, 4 )
              a% = int(calc)
              b% = int((calc - a%) * 10000)
              if b% = 0% then goto L61325               /****************/
              d% = 0%                                  /*              */
L61305:       d% = d% + 1%                             /* Conversion of*/
              if d% > 16% then goto L61330              /* Decimals to  */
                 if b% > d% * 0625 then goto L61305     /*  SIXTEEN'S   */
                 b% = d%                               /****************/
L61325:          if b% <> 16% then goto L61335
L61330:       a% = a% + 1% : b% = 0%       /* A% = WHOLE PART          */
L61335: REM   CALC = (A% * 100) + B%       /* B% = FRACTION (0 THRU 15 */
        return

        create_data                           /* Screen Consolidation */
REM            mat byfbatch% = zer               /*  (EWD002)  */
REM            mat rtebatch% = zer               /*  (EWD005)  */
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            init(" ") dt_key1$, dt_rec$, sav_part$, sav_screen$, phantom$
            dt_key1$ = all(hex(00))
            str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
            read #2,key 1% > dt_key1$, using L61425, dt_rec$,             ~
                                                eod goto create_data_done
            goto L61435
        create_data_nxt
           rec_count = rec_count + 1
           if MOD(rec_count,1000) <> 0 then goto rec_cont
           convert rec_count to rec_count$, pic (########)
           call "SHOSTAT" ("Processing ..."&rec_count$&hex(0D))

        rec_cont
/*          if rec_count <> 6921 then goto rec_cont2  */
/*          rec_count$ = rec_count$                   */
        rec_cont2
            if scr_code% <> 1% then goto L61420
                if ct% <> 0% then return        /* MFG Calculation     */
                   errormsg$ = err$(6%)
                   gosub error_prompt
                   return
L61420:     read #2, using L61425, dt_rec$, eod goto create_data_done
L61425:        FMT CH(256)

L61435:     if str(dt_rec$,47%,6%) <> str(scr_dte1$,1%,6%) then          ~
                                                   goto create_data_done
            dt_load$ = str(dt_rec$,1%,5%)
            dt_part$ = str(dt_rec$,189,25%)

            dt_sku$  = str(dt_rec$,245,9)
            if scr_code% <> 2% then goto not_patio_check
               if str(dt_part$,1,3) = "353" and                ~
                  (dt_sku$ <> "99736" and dt_sku$ <> "332744") ~
                            then goto create_data_nxt
not_patio_check:
            if len(dt_part$) < 19 then goto create_data_nxt
            if len(scr_load$) < 5 then goto L61480
               if scr_load$ <> dt_load$ then goto create_data_nxt
                  goto L61490

L61480:     if str(dt_load$,1%,1%) = "A" then goto create_data_nxt

L61490:     dt_dept$    = str(dt_rec$,42%,3%)
                                                /* (EWD001)           */
            gosub check_samples                 /* Skip Parts         */
                                                /* (EWD003)           */
            if s_d% > 11% and s_d% < 29% then goto create_data_nxt
                                                /* (EWD001)           */

/*(AWD018) */
REM door% = 0% not a door
REM door% = 1% dp 35 door
REM door% = 2% dp 50 door
REM door% = 3% screen only for door
            door% = 0%
            dp% = 0%
            gosub isDoor
REM  IF DOOR% = 0% THEN GOTO NOTDOORLOGIC
REM ASM now to build bronze lite duty screens
REM  IF STR(DT_PART$,4,1) = "3" THEN GOTO CREATE_DATA_NXT  /* NO BRONZE */

REM  IF DOOR% = 2% THEN GOTO CREATE_DATA_NXT  /* (NO HEAVY DUTY SCRNS) */
REM  IF DP% = 50% THEN GOTO CREATE_DATA_NXT   /* 2 IS HEAVY DUTY SCRN */
REM  IF STR(DT_PART$,11,1) = "2" AND DOOR% <> 3% THEN GOTO CREATE_DATA_NXT
REM  IF STR(DT_PART$,11,1) = "8" AND DOOR% = 3% THEN GOTO CREATE_DATA_NXT

REM NOTDOORLOGIC
/*(AWD018\)*/

 /* <AWD013> */
            get dt_rec$, using DATEFMT, dt_date

DATEFMT:    FMT POS(47), PD(11,1)
            convert dt_date to dt_date$, pic (00000000)
            dt_proc$    = str(dt_rec$,45%,2%)
            dt_barcode$ = str(dt_rec$,24%,18%)
            dt_so_line$ = str(dt_rec$,32,2)
 /* </AWD013> */
REM            if str(dt_barcode$,1,8) <> "07141616"                    ~
                    then goto create_data_nxt
REM            if dt_barcode$ <> "032716670100010001" then goto notBarcode
REM                  call "SHOSTAT" ("BARCODE " & dt_barcode$)
REM                  stop
REM notBarcode


            dt_shft$    = str(dt_rec$,104%,2%)
            dt_seq$     = str(dt_rec$,111%,5%)
            dt_so$      = str(dt_rec$,24%,8%)
            dt_index$   = str(dt_rec$,66%,30%)
            dt_ref$     = str(dt_rec$,96%,8%)
            dt_samp$    = str(dt_rec$,216%,1%)
            dt_wood$    = str(dt_rec$,217%,3%)
            dt_txt$     = str(dt_rec$,236%,4%)
            dt_special$ = str(dt_rec$,220%,10%)  /* 9%,1% = Cot/Oriel  */
                                                 /* 5%,1% = Sample     */
                                                 /* 6%,1% = Display    */
            so_inv$  = str(dt_barcode$,1%,8%)    /* (AWD029) */
            item_no$ = str(dt_barcode$,9%,2%)    /* (AWD029) */
            gosub lookup_subpart                 /* (AWD029) */
        calc_data
        if scr_code% <> 1% then goto L61500
            door% = 0%               /* Add this logic only is selection is 1 */
            dp% = 0%                 /* So skey$ will be set correctly in     */
            gosub isDoor             /* check_screen                          */

L61500:
            gosub check_screen
            if check% = 0% then goto create_data_nxt
               if scr_code% <> 1% then goto L61585
                  call "SHOSTAT" ("Calculating Screen Size")

L61585:        ct% = 0%
               gosub check_sort            /* Skip Support Departments */
               if scr_code% = 1% then dd% = 1%       /* Explode Screen */
               if dd% = 0% then goto create_data_nxt

               if scr_dept$ = "ALL" then goto L61625 /* All Departments */
                  if scr_dept$ <> dt_dept$ then                          ~
                                                  goto create_data_nxt
/* CR2653  switch to sales order */
/*         if scr_shft$ = "AA" then goto L61640   All Shifts      */
/*                  if scr_shft$ <> dt_shft$ then goto create_data_nxt   */
L61625:        if scr_so$ = "ALL" then goto L61640
               if scr_so$ <> dt_so$ then goto create_data_nxt

L61640:        gosub convert_fields
               sav_width1  = 0.0 : sav_width2  = 0.0
               sav_height1 = 0.0 : sav_height2 = 0.0

               cal% = 2%                       /* Calc the Cut Width   */
               str(phantom$,1%,4%) = ph$(1%)
               gosub lookup_phantom
               sav_width1 = width

               cal% = 3%                       /* Calc the Cut Height  */
               str(phantom$,1%,4%) = ph$(2%)
               if str(ph$(2%),1%,4%) = "TABL" then gosub spec_half       ~
                                              else gosub lookup_phantom
               sav_height1 = height

               cal% = 2%                       /* Calc CB Length(Width)*/
               if str(skey$,1%,1%) = "B" then cal% = 3%  /* Height Calc*/
               str(phantom$,1%,4%) = ph$(3%)
               gosub lookup_phantom
               sav_width2 = width
               if str(skey$,1%,1%) = "B" then sav_width2 = height

               cal% = 3%                       /* Calc CB Location(High*/
               if ph$(4%) <> "CTRS" then goto L61775
                  sav_height2 = round(sav_width1 / 2.0, 2)
                  goto L61795

L61775:        str(phantom$,1%,4%) = ph$(4%)
               if str(ph$(4%),1%,4%) = "TABL" then gosub spec_full       ~
                                              else gosub lookup_phantom
               sav_height2 = height
L61795:        gosub lookup_bayform            /*  (EWD002)  */
               gosub lookup_ritescreen         /*  (EWD005)  */
               for ik% = 1% to sqty%
                   gosub batch_sort
                   gosub update_work
                   gosub update_work_cb
               next ik%

            goto create_data_nxt
        create_data_done
REM            IF SCR_CODE% = 3% THEN GOTO OPTIMIZE
               f% = 5%                                 /* (EWD002)  */
               fc% = 17%
               gosub init_data
               pass% = 0% : gosub print_report_screen
               gosub init_data
               pass% = 1% : gosub print_report_screen_cb  /* (EWD009) */
               f% = 11%                                /* (EWD002)  */
               fc% = 19%
               gosub init_data
               pass% = 0% : gosub print_report_screen  /* (EWD002)  */
               gosub init_data
               pass% = 1% : gosub print_report_screen_cb  /* (EWD002)  */
               f% = 14%                                /* (EWD005)  */
               gosub init_data
               pass% = 0% : gosub print_report_screen  /* (EWD005)  */
               gosub init_data
               pass% = 1% : gosub print_report_screen  /* (EWD005)  */

               gosub print_labels_screen
               gosub delete_work
        return

        init_data
            init(" ") wrk_rec$, wd$, ht$, wd1$, ht1$, model$, ~
                      qty$, cl_l$, sav_wd1$, sav_ht1$, sc_r$, ~
                      wd2$, grid$, hnge$, sav_wd$, sav_ht$

            wrk_qty% = 0
        return
                                                      /* (EWD001)       */
        check_samples
            s_d% = 0%
            if len(dt_part$) < 20 then goto LS2      /* Quick Test      */
            convert str(dt_part$,20%,3%) to s_d%, data goto LS1

            if s_d% < 1% or s_d% > 80% then goto LS1   /* Not Samp/Disp  */
                                                       /*   (EWD008)     */
            if str(dt_part$,7%,2%) > "99" then goto LS1

        return                                       /* Code Found      */
LS1:        convert str(dt_part$,23%,3%) to s_d%, data goto LS2

            if s_d% < 1% or s_d% > 80% then goto LS2
                                                     /* Code Found      */
        return
LS2:        s_d% = 0%
        return
                                                       /* (EWD001)      */

        optimize
            size% = 100%
            scr_prod$ = "1"
            call "APCPLA47" ( size%,                      /* BATCH SIZE*/~
                              sched%,                     /* Start Sch */~
                              scr_dte1$,                   /* Prod Date */~
                              scr_dept$,                  /* DEPARTMENT*/~
                              scr_prod$,                  /* Prod Line */~
                              #3,                         /* GENCODES  */~
                              #8,                         /* APCCUTEQ  */~
                              #1,                         /* AMTBOMCD  */~
                              #4,                          /* AMTBOMIF  */~
                              #20 )              /* (IM5733) AWDPLNSR  */
                                         /* Special Sub. for or Bilco  */

               gosub delete_work
        return

        check_sort                          /* Exit with Subscript for */
            dd% = 0%                        /* Specified Department    */
            gosub check_support
            if supp% = 1% then return
            for dd% = 1% to dept_max%
                if dt_dept$ = dept$(dd%) then return
            next dd%
        dd% = 0%
        return

        batch_sort
           if scr_code% = 1% then return       /* Count Full Windows   */
           if byfmodel% = 1% then goto L62000  /* (EWD002) */
           if rtemodel% = 1% then goto L63000  /* (EWD005) */
           batch%(dd%,2%) = batch%(dd%,2%) + 1%
                                               /* Check Against Limit  */
           if batch%(dd%,1%) = batch%(dd%,2%) then dept$(dd%) = "XXX"
                                               /* Count Half Screens   */
                                               /* and Full Screens     */
           if sc$ = "2" then batch%(dd%,4%) = batch%(dd%,4%) + 1%        ~
                        else batch%(dd%,3%) = batch%(dd%,3%) + 1%
           if str(dt_special$,9%,1%) = "Y" then                          ~
               batch%(dd%,5%) = batch%(dd%,1%) + 1%   /* Cottage/Oriel */
           if str(dt_special$,5%,1%) = "Y" then                          ~
               batch%(dd%,6%) = batch%(dd%,6%) + 1%   /* Sample        */
           if str(dt_special$,6%,1%) = "Y" then                          ~
               batch%(dd%,6%) = batch%(dd%,6%) + 1%   /* Display       */
        return
L62000:    byfbatch%(dd%,2%) = byfbatch%(dd%,2%) + 1%  /* (EWD002) - Beg */
                                               /* Check Against Limit  */
           if byfbatch%(dd%,1%) = byfbatch%(dd%,2%) then dept$(dd%) = "XXX"
                                               /* Count Half Screens   */
                                               /* and Full Screens     */
           if sc$ = "2" then byfbatch%(dd%,4%) = byfbatch%(dd%,4%) + 1%  ~
                        else byfbatch%(dd%,3%) = byfbatch%(dd%,3%) + 1%
           if str(dt_special$,9%,1%) = "Y" then                          ~
               byfbatch%(dd%,5%) = byfbatch%(dd%,1%) + 1% /*Cottage/Oriel*/
           if str(dt_special$,5%,1%) = "Y" then                          ~
               byfbatch%(dd%,6%) = byfbatch%(dd%,6%) + 1%   /* Sample   */
           if str(dt_special$,6%,1%) = "Y" then                          ~
               byfbatch%(dd%,6%) = byfbatch%(dd%,6%) + 1%   /* Display   */
        return                                         /* (EWD002) - End */
L63000:    rtebatch%(dd%,2%) = rtebatch%(dd%,2%) + 1%  /* (EWD005) - Beg */
                                               /* Check Against Limit  */
           if rtebatch%(dd%,1%) = rtebatch%(dd%,2%) then dept$(dd%) = "XXX"
                                               /* Count Half Screens   */
                                               /* and Full Screens     */
           if sc$ = "2" then rtebatch%(dd%,4%) = rtebatch%(dd%,4%) + 1%  ~
                        else rtebatch%(dd%,3%) = rtebatch%(dd%,3%) + 1%
           if str(dt_special$,9%,1%) = "Y" then                          ~
               rtebatch%(dd%,5%) = rtebatch%(dd%,1%) + 1% /*Cottage/Oriel*/
           if str(dt_special$,5%,1%) = "Y" then                          ~
               rtebatch%(dd%,6%) = rtebatch%(dd%,6%) + 1%   /* Sample    */
           if str(dt_special$,6%,1%) = "Y" then                          ~
               rtebatch%(dd%,6%) = rtebatch%(dd%,6%) + 1%   /* Display   */
        return                                         /* (EWD005) - End */

        update_work
            init(" ") wrk_key$, lab_rec$, wrk_rec1$
            wrk_fil$ = "               "
            lab_fil$ = "         "
            if error% <> 0% then goto create_error_rec
            f% = 5%                                    /* (EWD002) */
            if byfmodel% = 1% then f% = 11%            /* (EWD002) */
REM            if byfmodel% = 1% then f% = 20%            /* (EWD002) */

            if rtemodel% = 1% then f% = 14%            /* (EWD005) */


            str(wrk_key$,1%,1%) = "0"                  /* Unused Codes */
                                      /* 1st - Alpha Sort Code by Dept */
                                      /* 2nd - Product Code (Model)    */
                                      /* 3rd - Screen Color Code       */
            str(wrk_key$,2%,1%) = str(sort$,dd%,1%) /* Sort Code       */
            str(wrk_key$,3%,3%) = dt_dept$          /* Department      */
            str(wrk_key$,6%,30%)= dt_index$         /* Planning Sort   */

            gosub calc_wd                              /* WD2$/ HT2$   */
            gosub calc_wd_ht                           /* WD1$/ HT1$   */
            str(wrk_key$,36%,9%) = wd2$                /* CB Cut       */
            str(wrk_key$,45%,1%) = sc$                 /* Screen Type  */
            str(wrk_key$,46%,2%) = hg$                 /* Hinge Code   */
            str(wrk_key$,48%,3%) = model$              /* Model Code   */
            str(wrk_key$,51%,9%) = ht2$                /* CB Location  */
            str(wrk_key$,60%,1%) = cl$                 /* Color        */
            if scr_code% <> 1% then goto update_rec
               ct% = ct% + 1%
               str(cut$(ct%),10%,9%) = wd1$            /* Cut Width    */
               str(cut$(ct%),25%,8%) = ht1$            /* Cut Height   */
               str(cut$(ct%),39%,9%) = wd2$            /* CB Cut Length*/
               str(cut$(ct%),54%,8%) = ht2$            /* CB Location  */
               return

        update_rec
            read #f%,hold,key = wrk_key$, using L62260, wrk_qty%,         ~
                                                      eod goto create_rec
L62260:       FMT POS(61), BI(2)
            wrk_qty% = wrk_qty% + 1%
            put #f%, using L62260, wrk_qty%
            rewrite #f%
            goto L62345
        create_rec
            wrk_qty% = 1%
            str(wrk_rec1$,1%,60%) = wrk_key$
            put str(wrk_rec1$,61%,2%), using L62305, wrk_qty%
L62305:       FMT BI(2)
            str(wrk_rec1$,63%,7%) = wd$
            str(wrk_rec1$,70%,6%) = ht$
            str(wrk_rec1$,76%,9%) = wd1$         /* Calc. Width  */
            str(wrk_rec1$,85%,8%) = ht1$         /* Calc. Height */
            str(wrk_rec1$,93%,36%)= wrk_fil$
            str(wrk_rec1$,93%,5%) = dt_seq$

            write #f%, using L62340, wrk_rec1$
L62340:       FMT CH(128)
L62345:     gosub update_label

            if f% = 11% then gosub update_awdplnsr
/* (AWD024) */
            if f% = 5%  then gosub update_awdplnsr
        return

        update_awdplnsr
              f% = 20%
              init(" ") sr_model$
              sr_model$ = model$

              if dt_dept$ = "007" and model$ = "267"                    ~
                      then sr_model$ = "215"

              gosub lookup_inventory
REM              IF INV% = 1% THEN RETURN /*DO NOT COUNT IN BATCH IF PULL*/
              gosub calc_wd                              /* WD2$/ HT2$   */
              gosub calc_wd_ht                           /* WD1$/ HT1$   */
              gosub std_wd_ht                            /* Window WD/HT */

              init(" ") sr_key$, sr_rec$()
              str(sr_key$,1,8)  = dt_ref$             /* Warranty Number */
              convert ik% to str(sr_key$,9,1), pic(0) /* Screen Count  */
              str(sr_key$,10,3) = num$ & "00"           /* Remake Counter*/
/*@@@*/
              if scr_code% = 2% then scr_batch$ = scr_batch2$
              read #f%, hold, key = sr_key$, using sr_fmt, sr_rec$(),     ~
                                                         eod goto no_sr
sr_fmt:       FMT 2*CH(256)

/* <AWD017> */
            dt_status$ = str(sr_rec$(),163,1)
            if scr_code% = 2% and dt_status$ <> "1" then return
/* </AWD017> */
                  delete #f%

        no_sr
          sr_barcode$ = "         "
          sr_remake$  = "   "
          init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                    stmt6$, stmt7$, stmt8$
          sr_barcode$  = dt_ref$             /* Warranty Number */
          convert ik% to str(sr_barcode$,9,1), pic(0) /* Screen Count  */
          sr_remake_num$ = num$ & "00"           /* Remake Counter*/
/*@@@*/
          if scr_code% = 2% then scr_batch$ = scr_batch2$
          stmt1$ = "SELECT BARCODE, REMAKE_NUM, STATUS, MODEL_NUM " & ~
                   "FROM MFG.SCREEN WHERE BARCODE = '" & sr_barcode$ & ~
                   "' AND REMAKE_NUM = '" & sr_remake_num$ & "'"
REM             GOSUB ORACLE_QUERY
            if oci_err% < 0% then goto no_ora_sr
            if oci_err% = 100% then goto no_ora_sr
REM            GOSUB ORACLE_FETCH
            if oci_err% < 0% then goto no_ora_sr
              if oci_err% = 100% then goto no_ora_sr
              for field_num% = 1%  to no_fields%
REM                  GOSUB ORACLE_GETFIELD
                  if field_num% = 1%  then sr_barcode$ = field$
                  if field_num% = 2%  then sr_remake_num$ = field$
                  if field_num% = 3%  then sr_status$ = field$
                  if field_num% = 4%  then sr_model_num$ = field$
              next field_num%

            dt_status$ = sr_status$
            stmt1$ = "DELETE FROM MFG.SCREEN WHERE BARCODE = '" & ~
              sr_barcode$ & "' AND REMAKE_NUM = '" & sr_remake_num$ & "'"
REM            GOSUB ORACLE_EXEC1
        no_ora_sr

             str(sr_rec$(),1,6)   = str(dt_rec$,47,6) /* DT Prod Date    */
             str(sr_rec$(),7,1)   = "1"             /* Prod =1 Remake = 2*/
             str(sr_rec$(),8,6)   = str(dt_rec$,47,6) /* DT Prod Date    */
             str(sr_rec$(),14,20) = scr_batch$      /* Batch Name        */
skip_upd
             convert dd% to str(sr_rec$(),34,5), pic(00000) /* Sort      */
             str(sr_rec$(),39,3) = dt_dept$        /* Department Code    */
             str(sr_rec$(),42,8) = dt_ref$         /* Warranty Number Barcode*/
             str(sr_rec$(),50,1) = str(sr_key$,9,1) /* Screen Count      */
             str(sr_rec$(),51,3) = num$ & "00"     /* remake number      */
             str(sr_rec$(),54,5) = dt_seq$         /* Production Seq     */
             str(sr_rec$(),59,3) = sr_model$       /* Model           */
             str(sr_rec$(),62,1) = str(dt_part$,4,1)  /* Color           */
             str(sr_rec$(),63,1) = str(dt_part$,11,1) /* Screen          */
             str(sr_rec$(),64,1) = scr_type$          /* Type Ext or Roll*/
             str(sr_rec$(),65,1) = scr_hf$            /* Half or Full    */

                                                      /*Screen Calc Width*/
             put str(sr_rec$(),66,8), using pd_fmt, sav_width1
pd_fmt:              FMT PD(14,4)
                                                      /*Screen Cal Height*/
             put str(sr_rec$(),74,8), using pd_fmt, sav_height1
                                                      /*Screen Cal CB Len*/
             put str(sr_rec$(),82,8), using pd_fmt, sav_width2
                                                      /* Screen Cal CB Loc*/
             put str(sr_rec$(),90,8), using pd_fmt, sav_height2

             str(sr_rec$(),98,10)  = wd1$           /* Frac Calc Width   */
             str(sr_rec$(),108,10) = ht1$           /* Frac Calc Height  */
             str(sr_rec$(),118,10) = wd2$           /* Frac Calc CB Len  */
             str(sr_rec$(),128,10) = ht2$           /* Frac Calc CB Loc  */
             str(sr_rec$(),138,25) = dt_part$       /* Part Number       */
             str(sr_rec$(),163,1)  = "1"            /* Scheduled in Batch*/
             str(sr_rec$(),164,8)  = dt_ref$        /* Barcode           */
             str(sr_rec$(),172,1)  = str(sr_key$,9,1) /* Screen Count    */
             str(sr_rec$(),173,3)  = num$ & "00"    /* remake number     */
             str(sr_rec$(),176,1)  = scr_pull$      /* Pull or Cut       */
             str(sr_rec$(),177,8)  = dt_so$         /* Sales Order       */
             str(sr_rec$(),185,10) = wd$            /* Window Width Frac */
             str(sr_rec$(),195,10) = ht$            /* Window Height Frac*/
             str(sr_rec$(),234,9)  = dt_sku$        /* ASM - Lowes or not*/
/* (AWD022) */
             str(sr_rec$(),243,15) = skey$(ss%)     /*Store for Reference*/
             str(sr_rec$(),258,20) = ss$(ss%)       /*Store for Reference*/

/* <AWD023> */
             str(sp_key$,01,08) = dt_so$
REM             STR(SP_KEY$,09,01) = " "
REM             STR(SP_KEY$,10,02) = DT_SO_LINE$
REM             IF STR(SP_KEY$,10,01) = "0" THEN STR(SP_KEY$,10,01) = " "
             dt_so_line% = 0%
             convert dt_so_line$ to dt_so_line%, data goto bad_so_line
bad_so_line:
             convert dt_so_line% to str(sp_key$,9%,3%), pic(###)
             str(sr_rec$(),278,20) = "                                 "
             read #63,key = sp_key$,using BKSUBPT, sp_rec$, eod goto not_found

             str(sr_rec$(),278,20) = str(sp_rec$,48,20)
BKSUBPT:       FMT CH(256)
REM              MESSAGE$ = SP_KEY$ & " / " & STR(SP_REC$,48,20)
REM          IF STR(SP_REC$,62,1) > "0" THEN ~
REM          CALL "LOGFILE" (MESSAGE$)
             str(sr_rec$(),278,20) = str(sp_rec$,48,20) /* Sub Part      */
not_found:
/* </AWD023> */
             put #f%, using sr_fmt1, sr_rec$()
             write #f%, eod goto sr_write_error
sr_fmt1:         FMT 2*CH(256)
DATEFMT2:        FMT POS(53), PD(11,1)

REM             DES$ = STR(DT_REC$,47,6)

             get dt_rec$, using DATEFMT, sr_prod_date
             convert sr_prod_date TO sr_prod_date$, pic (00000000)
             get dt_rec$, using DATEFMT2, sr_comp_date
             convert sr_comp_date TO sr_comp_date$, pic (00000000)

             sr_comp_time$     = str(dt_rec$,116,8)
             sr_process$       = "1"             /* Prod =1 Remake = 2 */
             sr_batch$         = scr_batch$      /* Batch Name         */
             convert dd% to sr_batch_num$, pic(00000) /* Sort        */
             sr_dept$          = dt_dept$        /* Department Code    */

REM          SR_BARCODE$       = DT_REF$       /* WARRANTY NUMBER BARCODE*/
REM          CONVERT IK% TO STR(SR_BARCODE$,9,1), PIC(0) /* SCREEN COUNT */
REM @@@@@@   SR_BARCODE$       = STR(SR_KEY$,9,1) /* SCREEN COUNT      */
REM          SR_REMAKE_NUM$    = NUM$ & "00"     /* REMAKE NUMBER      */

             sr_seq$           = dt_seq$         /* Production Seq     */
             sr_model_num$     = sr_model$       /* Model           */
             sr_color$         = str(dt_part$,4,1)  /* Color           */
             sr_screen_code$   = str(dt_part$,11,1) /* Screen          */
             sr_screen_type$   = scr_type$          /* Type Ext or Roll*/
             sr_screen_hf$     = scr_hf$            /* Half or Full    */
             sr_width          = sav_width1
             sr_height         = sav_height1
             sr_cb_len         = sav_width2
             sr_cb_loc         = sav_height2
             sr_shift$         = "  "

             sr_width_frac$        = wd1$           /* Frac Calc Width   */
             sr_height_frac$       = ht1$           /* Frac Calc Height  */
             sr_cb_len_frac$       = wd2$           /* Frac Calc CB Len  */
             sr_cb_loc_frac$       = ht2$           /* Frac Calc CB Loc  */
             sr_part$              = dt_part$       /* Part Number       */
             sr_status$            = "1"            /* Scheduled in Batch*/
             sr_cut_pull$          = scr_pull$      /* Pull or Cut       */
             sr_sales_order$       = dt_so$         /* Sales Order       */
             sr_wnd_width$         = wd$            /* Window Width Frac */
             sr_wnd_height$        = ht$            /* Window Height Frac*/
             sr_sku$               = dt_sku$        /* ASM - Lowes or not*/
             sr_skey$              = skey$(ss%)     /*Store for Reference*/
             sr_ss$                = ss$(ss%)       /*Store for Reference*/
             sr_sub_part$          = str(sp_rec$,48,20) /* Sub Part */
/* </AWD023> */
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                  stmt6$, stmt7$, stmt8$
/*..+....1....+....2....+....3....+....4....+....5....+....6....+....7....+*/
            convert sr_width to sr_width$, pic (-##########.0000)
            convert sr_height to sr_height$, pic (-##########.0000)
            convert sr_cb_len to sr_cb_len$, pic (-##########.0000)
            convert sr_cb_loc to sr_cb_loc$, pic (-##########.0000)
/*..+....1....+....2....+....3....+....4....+....5....+....6....+....7....+*/
             stmt1$ = "INSERT INTO MFG.SCREEN (PROD_DATE, PROCESS, BATCH," & ~
                      " BATCH_NUM, DEPT, BARCODE, REMAKE_NUM, SEQ," & ~
              " MODEL_NUM, COLOR, SCREEN_CODE, SCREEN_TYPE," & ~
                      " SCREEN_HF, WIDTH, HEIGHT, CB_LEN, CB_LOC," & ~
                      " WIDTH_FRAC, HEIGHT_FRAC, CB_LEN_FRAC, CB_LOC_FRAC, "
             stmt2$ = " PART_NUMBER, STATUS, CUT_PULL, SALES_ORDER," & ~
                      " WND_WIDTH, WND_HEIGHT, COMP_DATE, COMP_TIME," & ~
                      " ID_NUM, PROD_TIME, SHIFT, REASON, RMK_ID, ORG_DTE," & ~
                      " SKU, SKEY, SS, OPTIONS, SUB_PART) VALUES ("
             date1$ = "NULL,'"
             if sr_prod_date$ > "   " then                  ~
              date1$ = "to_date('" & sr_prod_date$ & "','YYYYMMDD'),'"
             stmt2$ = stmt2$ & date1$ & sr_process$ & ~
                      "','" & sr_batch$ & "','" & sr_batch_num$ & "',"
             stmt3$ = "'" & sr_dept$ & "','" & sr_barcode$ & "','" & ~
                      sr_remake_num$ & "','" & sr_seq$ & "','" &     ~
                      sr_model_num$ & "','" & sr_color$ & "','" &    ~
                      sr_screen_code$ & "','" & sr_screen_type$ & "','" & ~
                      sr_screen_hf$ & "'," & sr_width$ & "," & sr_height$ & ~
                      "," & sr_cb_len$ & "," & sr_cb_loc$ & ",'" & ~
                      sr_width_frac$ & "','" & sr_height_frac$ & "','" & ~
                      sr_cb_len_frac$ & "','" & sr_cb_loc_frac$ & "','" & ~
                      sr_part$ & "','" & sr_status$ & "',"
             stmt4$ = "'" & sr_cut_pull$ & "','" & sr_sales_order$ & "','" & ~
                      sr_wnd_width$ & "','" & sr_wnd_height$ & "',"
             date1$ = "NULL"
REM          IF SR_COMP_DATE$ > "        " THEN                        ~
REM              DATE1$ = "TO_DATE('" & SR_COMP_DATE$ & "','YYYYMMDD')"
             stmt4$ = stmt4$ & date1$ & ","
             date1$ = "NULL"
REM          IF SR_COMP_TIME$ > "        " THEN                 ~
REM              DATE1$ = "'" & SR_COMP_TIME$ & "'"
             stmt4$ = stmt4$ & date1$
             stmt4$ = stmt4$ & ",'" & sr_id_num$ & "',"
             date1$ = "NULL"
             if sr_prod_time$ > "        " then                        ~
                 date1$ = "'" & sr_prod_time$ & "'"
             stmt4$ = stmt4$ & date1$
             stmt4$ = stmt4$ & ",'" &  sr_shift$ & "','" & sr_reason$ & ~
               "','" & sr_rmk_id$ & "',"
             date1$ = "NULL,'"
             sr_org_dte$ = " "  /* CMG-added this so would not show up in un-used */
                                /* During compile                                 */
REM          IF SR_ORG_DTE$ > "        " THEN                        ~
REM              DATE1$ = "TO_DATE('" & SR_ORG_DTE$ & "','YYYYMMDD'),'"
             stmt5$ = date1$ & sr_sku$ & "','" & sr_skey$ & "','" &  ~
               sr_ss$ & "','" & sr_options$ & "','" & sr_sub_part$ & "')"

REM             GOSUB ORACLE_EXEC1
             cnt = cnt + 1
             if cnt < 100 then goto skip_commit
             stmt1$ = "commit"
             cnt = 0
REM             GOSUB ORACLE_EXEC1
skip_commit:
        return
        sr_write_error
            errormsg$ = "Error - Writing Record AWDPLNSR!!"
            gosub error_prompt
        return

                                                     /*  (EWD009) -  BEG */
        update_work_cb
            fc% = 17%
            if byfmodel% = 1% then fc% = 19%
REM            IF BYFMODEL% = 1% OR RTEMODEL% = 1% THEN RETURN
            if rtemodel% = 1% then return

            init(" ") wrk_key$, lab_rec$, wrk_rec1$
            wrk_fil$ = "               "
            lab_fil$ = "         "
            if error% <> 0% then goto create_error_rec
            str(wrk_key$,1%,1%) = "0"                  /* Unused Codes */
                                      /* 1st - Alpha Sort Code by Dept */
                                      /* 2nd - Product Code (Model)    */
                                      /* 3rd - Screen Color Code       */
            str(wrk_key$,2%,1%) = str(sort$,dd%,1%) /* Sort Code       */
            str(wrk_key$,3%,3%) = dt_dept$          /* Department      */
            str(wrk_key$,6%,30%)= " "

            gosub calc_wd                              /* WD2$/ HT2$   */
            gosub calc_wd_ht                           /* WD1$/ HT1$   */
REM            if len(wd2$) < 2 then return
            if len(wd2$) < 2 then goto no_center_bar_len
            str(wrk_key$,36%,9%) = wd2$                /* CB Cut       */
no_center_bar_len
            str(wrk_key$,45%,1%) = sc$                 /* Screen Type  */
            str(wrk_key$,46%,2%) = hg$                 /* Hinge Code   */
            str(wrk_key$,48%,3%) = dt_dept$            /* Model Code   */
REM            str(wrk_key$,51%,9%) = ht2$                /* CB Location  */
            str(wrk_key$,60%,1%) = cl$                 /* Color        */
            if scr_code% <> 1% then goto update_rec_cb
REM               ct% = ct% + 1%
REM               str(cut$(ct%),10%,9%) = wd1$            /* Cut Width   */
REM               str(cut$(ct%),25%,8%) = ht1$            /* Cut Height  */
REM               str(cut$(ct%),39%,9%) = wd2$            /*CB Cut Length*/
REM               str(cut$(ct%),54%,8%) = ht2$            /* CB Location */
               return

        update_rec_cb
            read #fc%,hold,key = wrk_key$, using L62260, wrk_qty%,        ~
                                                    eod goto create_rec_cb

            wrk_qty% = wrk_qty% + 1%
            put #fc%, using L62260, wrk_qty%
            rewrite #fc%
        return
        create_rec_cb
            wrk_qty% = 1%
            str(wrk_rec1$,1%,60%) = wrk_key$
            put str(wrk_rec1$,61%,2%), using L62305, wrk_qty%

            str(wrk_rec1$,63%,7%) = wd$
            str(wrk_rec1$,70%,6%) = ht$
            str(wrk_rec1$,76%,9%) = wd1$         /* Calc. Width  */
            str(wrk_rec1$,85%,8%) = ht1$         /* Calc. Height */
            str(wrk_rec1$,93%,36%)= wrk_fil$
            write #fc%, using L62340, wrk_rec1$
        return


                                                     /* (EWD009) -  END  */

        create_error_rec
            if scr_code% <> 1% then goto L62380
               errormsg$ = err$(error%)
               return
L62380:     str(wrk_key$,1%,5%)  = "99999"
            str(wrk_key$,6%,5%)  = "ERR" & "00"
            convert error% to str(wrk_key$,9%,2%), pic(00)

            str(wrk_key$,11%,25%) = dt_part$
            str(wrk_key$,38%,3%) = "ERR"
            convert wrk_qty% to str(wrk_key$,41%,5%),pic(00000)

            gosub errormsg
            return
REM            goto update_rec

        update_label                               /* (EWD002) */
            if byfmodel% = 1% then goto update_bayform_label
                                                   /* (EWD005) */
            if rtemodel% = 1% then goto update_bayform_label
            str(lab_rec$,1%,60%) = wrk_key$
            convert wrk_qty% to str(lab_rec$,61%,5%),pic(00000)
            str(lab_rec$,66%,1%)  = " "            /* VIEW$ Not Applic */
            str(lab_rec$,67%,3%)  = model$
            if str(dt_special$,9%,1%) = "Y" then                         ~
               str(lab_rec$,70%,6%)  = "Co-Or/"    /* Cottage/Oriel    */
            if str(dt_special$,5%,1%) = "Y" then                         ~
               str(lab_rec$,76%,2%)  = "Sp"        /* Sample           */
            if str(dt_special$,6%,1%) = "Y" then                         ~
               str(lab_rec$,76%,2%)  = "Dp"        /* Display          */
            str(lab_rec$,78%,8%)  = dt_so$
            str(lab_rec$,86%,5%)  = dt_seq$
            str(lab_rec$,91%,9%)  = wd1$
            str(lab_rec$,100%,8%) = ht1$
            str(lab_rec$,108%,9%) = wd2$
            str(lab_rec$,117%,7%) = wd$
            str(lab_rec$,124%,6%) = ht$
            str(lab_rec$,130%,4%) = dt_txt$
            str(lab_rec$,134%,1%) = cl$                      /* Color  */
            str(lab_rec$,135%,2%) = hg$
            str(lab_rec$,137%,1%) = sc$                      /* Screen */
            str(lab_rec$,138%,25%)= dt_part$                 /* PART NO*/
            str(lab_rec$,163%,8%) = dt_ref$                  /* WARRANT*/
            str(lab_rec$,171%,1%) = dt_samp$                 /* 0,1,2  */
            str(lab_rec$,172%,5%) = dt_load$                 /* LOAD NO*/
            str(lab_rec$,177%,24%)= " "
        /* <AWD013> */
            str(lab_rec$,181%,2%)= dt_proc$
            str(lab_rec$,183%,18%)= dt_barcode$
        /* </AWD013> */

            write #6, using L62570, lab_rec$, eod goto L62580
L62570:       FMT CH(200)
        return
L62580:     errormsg$ = "(ERR) KEY -> " & str(lab_rec$,1%,60%)
            gosub error_prompt
            init(" ") errormsg$
        return

        update_bayform_label
            ff% = 12%
                                                   /* (EWD005) */
            if rtemodel% = 1% then ff% = 15%
            gosub lookup_color
            gosub lookup_hinge
            gosub lookup_screen
            gosub lookup_text
            if ff% <> 12% then goto ritescrn_rec   /* (EWD007) */
            str(lab_rec$,1%,3%)  = model$
            convert wrk_qty% to str(lab_rec$,4%,5%),pic(00000)
            str(lab_rec$,9%,3%)  = dt_dept$
            str(lab_rec$,12%,5%)  = dt_seq$
            goto no_ritescrn_rec
        ritescrn_rec                               /* (EWD007) - BEG */
            str(lab_rec$,1%,3%)  = dt_dept$
            str(lab_rec$,4%,5%)  = dt_seq$
            convert wrk_qty% to str(lab_rec$,9%,5%),pic(00000)
            str(lab_rec$,14%,3%)  = model$
        no_ritescrn_rec                            /* (EWD007) - END */
            str(lab_rec$,17%,1%) = " "             /* VIEW$ Not Applic */
            str(lab_rec$,18%,8%) = dt_so$
            str(lab_rec$,26%,2%) = cl_l$                    /* Color  */
            str(lab_rec$,28%,4%) = sc_l$                    /* Screen */

            if str(dt_special$,9%,1%) = "Y" then                         ~
               str(lab_rec$,32%,6%)  = "Co-Or/"    /* Cottage/Oriel    */
            if str(dt_special$,5%,1%) = "Y" then                         ~
               str(lab_rec$,38%,2%)  = "Sp"        /* Sample           */
            if str(dt_special$,6%,1%) = "Y" then                         ~
               str(lab_rec$,38%,2%)  = "Dp"        /* Display          */

            str(lab_rec$,40%,9%) = wd1$            /* Screen Cut Width */
            str(lab_rec$,49%,8%) = ht1$            /* Screen Cut Height*/
            str(lab_rec$,57%,9%) = wd2$            /* CB Cut Length    */
            str(lab_rec$,66%,9%) = ht2$            /* CB Location      */
                                                   /* Measured Top Down*/
            str(lab_rec$,75%,7%) = wd$             /* Act Window Width */
            str(lab_rec$,82%,6%) = ht$             /* Act Window Height*/
            str(lab_rec$,88%,1%) = txt_flag$       /* Text or Not      */
            str(lab_rec$,89%,2%) = sc$             /* (AWD015) */

           init(" ") lab_key$
           lab_key$ = str(lab_rec$,1%,10%)             /* (AWD015) */
           read #ff%, hold, key = lab_key$, eod goto write_bayfm_rec

                delete #ff%

        write_bayfm_rec
           put #ff% using L62575, lab_rec$

           write #ff%,  eod goto L62590
L62575:       FMT CH(100)
        return
L62590:     errormsg$ = "(ERR) KEY -> " & str(lab_rec$,1%,16%)
            gosub error_prompt
            init(" ") errormsg$
        return


        lookup_inventory
            inv%, inv_qty%, tot_pull% = 0%
            init(" ") inv_key$, sav_inv$, scr_num$, scr_type$, ~
                      scr_hf$, scr_pull$, pull_dte$, start_pull$
            scr_pull$ = "C"
            scr_type$ = "R"
            if sc$ = "A" or sc$ = "J" and door% = 0% then scr_type$ = "E"
            scr_hf$ = "H"
            if sc$ = "2" or sc$ = "A" and door% = 0% then scr_hf$ = "F"

            if scr_type$ = "R" and (sc$ <> "B" and sc$ <> "C") ~
                            then gosub lookup_extruded

            str(inv_key$,1,1) = cl$
            str(inv_key$,2,1) = scr_type$
            str(inv_key$,3,1) = scr_hf$
            put str(inv_key$,4,8), using pd_fmt, sav_width1

            put str(inv_key$,12,8), using pd_fmt, sav_height1

            sav_inv$ = inv_key$

            read #21, key = inv_key$, using inv_fmt, scr_num$,  ~
                               inv_qty%, tot_pull%, pull_dte$,  ~
                               start_pull$, eod goto no_inv

                if inv_qty% <= 0% then return

inv_fmt:             FMT CH(10), POS(60), BI(04), POS(68), BI(04), ~
                         CH(06), CH(06)

            init(" ") inv_key$
            str(inv_key$,1,10) = scr_num$
            str(inv_key$,11,3) = sr_model$

            read #22, key = inv_key$, eod goto no_inv

                inv%      = 1%
                scr_pull$ = "P"
REM                dd%       = 999%


           if scr_inv$ = "N" then return
/*<AWD028>+ */
/*         str(sp_key$,01,08) = dt_so$~
           str(sp_key$,09,01) = " "~
           str(sp_key$,10,02) = dt_so_line$~
           if str(sp_key$,10,01) = "0" then str(sp_key$,10,01) = " "~
           read #63,key = sp_key$,using BKSUBPT, sp_rec$, eod goto cont_updt~
           if str(sp_rec$,62,1) = "2" then return     <AWD028> */
/* change of heart here.. we already have bcksubpt_rec$ so lets just use that*/
/*         if str(bcksubpt_rec$,62,1) = "2" then return     <AWD028> */
           if str(bcksubpt_rec$,62,1) = "2" then no_update  /*<AWD030> */
           if str(bcksubpt_rec$,62,1) = "3" then no_update  /*<CR 986> */
           if str(bcksubpt_rec$,62,1) = "4" then no_update  /*<CR 986> */
           if str(bcksubpt_rec$,62,1) = "9" then no_update  /*<CR 986> */                              

cont_updt:
/*<AWD028>- */

                 gosub update_inv
        no_inv
        return

        update_inv
             read #21, hold, key = sav_inv$, eod goto no_update


             inv_qty%  = inv_qty% - 1%
             tot_pull% = tot_pull% + 1%

             pull_dte$ = date
             if start_pull$ = " " or start_pull$ = hex(000000000000) ~
                            then start_pull$ = date

             put #21, using inv_update, inv_qty%, tot_pull%, ~
                       pull_dte$, start_pull$
inv_update:        FMT POS(60), BI(04), POS(68), BI(04),     ~
                                        CH(06), CH(06)

             rewrite #21, eod goto no_update

        return
        no_update
         inv% = 0%
         scr_pull$ = "C"
        return


        lookup_extruded
              init(" ") readkey$
              str(readkey$,1%,9%) = "EXTRUDED "
              str(readkey$,10%,15%) = model$ & scr_hf$

              read #3,key = readkey$,  eod goto not_extruded

                      scr_type$ = "E"
        not_extruded
            if door% = 1% and sc$ = "2" then scr_type$ = "E"

            if door% = 2% and dp% = 50%               ~
                                 then scr_type$ = "E"

            if door% = 3% and sc$ = "8"                ~
                                 then scr_type$ = "E"
            if door% = 3% and dp% = 50% and sc$ = "2" ~
                                 then scr_type$ = "E"
/* (SR65703) */
            if len(dt_part$) >= 19 and sc$ = "J" then scr_type$ = "E"
        return

        lookup_phantom                           /* 1%=Width and Height*/
            err% = 0%                            /* 2%=Width Only      */
            width = 0.0 : height = 0.0           /* 3%=Height Only     */
            if str(phantom$,1%,4%) = "0000" then goto L62670    /* N/A  */

            call "APCCALSB" (cal%,                 /* CALC TYPE 1%,2%,3*/~
                             dt_part$,             /* PART NUMBER      */~
                             dim1es,               /*Leg Height (AWD029)*/~
                             dim2es,               /*Leg Height (AWD029)*/~
                             dim3es,               /*Leg Height (AWD029)*/~
                             phantom$,             /* PHANTOM DESIGNATO*/~
                             width,                /* EXACT WIDTH      */~
                             height,               /* EXACT HEIGHT     */~
                             #1,                   /* AMTBOMCD EQUATION*/~
                             err% )                /* ERROR CODE 0%-OK */
            if err% <> 0% then error% = 5%         /* Equation Error   */
L62670:     init(" ") phantom$
            if scr_code% <> 1% then return
               if err% = 0% then return
                  errormsg$ = err$(error%)
                  gosub error_prompt
                  init(" ") errormsg$
        return

        spec_half                                    /* MOD - 06/04/97 */
             height =((s_height/2.0) - s_half)                           ~
                                             + ((s_height/2.0) - s_clmr )
             init(" ") phantom$
        return
        spec_full
             height =((s_height/2.0) - s_full)                           ~
                                            - ((s_height/2.0)  - s_clmr )
             init(" ") phantom$
        return

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        lookup_text                           /* Look Up Text Id       */
            if textid$ = dt_txt$ then return
            init(" ") txt_flag$, textid$
            textid$ = dt_txt$
            gosub'099(textid$)
            if txt% = 1% then txt_flag$ = "*"
        return

        print_report_screen_cb
            init(" ") date$, rpt_time$, title$
            lcnt% = 99%   :  pageno% = 0%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "SETPRNT" (" ","SC02", 0%, 0%)
            select printer (134)
            call "COMPNAME" (12%, title$, 0%)
            call "SHOSTAT" ("Printing Center Bar Report")


            init(" ") sav_dept_scr$, wrk_key$
            dd% = 0%
            read #fc%,key > wrk_key$, using L62955, wrk_rec$, wrk_qty%,   ~
                      wd$, ht$, wd1$, ht1$, eod goto report_done_screen
            dd% = 1%
            sav_dept_scr$ = str(wrk_rec$,2%,1%)

            goto L62970
        print_next_screen_cb
            read #fc%, using L62955, wrk_rec$, wrk_qty%, wd$, ht$,        ~
                                 wd1$, ht1$, eod goto report_done_screen_cb


L62970:
            dept_scr$  = str(wrk_rec$,2%,1%)        /* Dept Sort Code  */
            dt_dept$   = str(wrk_rec$,3%,3%)        /* Dept Code       */
            model$     = str(wrk_rec$,48%,3%)       /* Model Code      */
            sc$        = str(wrk_rec$,45%,1%)       /* Screen Code     */
            cl$        = str(wrk_rec$,60%,1%)       /* Color Code      */
            wd2$       = str(wrk_rec$,36%,9%)       /* CB Cut Length   */
            hg$        = str(wrk_rec$,46%,2%)       /* Hinge Code      */
            ht2$       = str(wrk_rec$,51%,9%)       /* CB Location     */

            convert wrk_qty% to qty$, pic(#####)

            if str(wrk_rec$,6%,3%) <> "ERR" then goto L63090
               cl_l$  = "**"
               sc_r$  = "***** (Error) ******"
               wd1$   = "*********"
               ht1$   = "********"
               wd2$   = "*********"
               grid$  = str(wrk_rec$,11%,25%)
               hnge$  = "No Equation Found"
               goto L63165

L63090:     if sav_dept_scr$ = dept_scr$ then goto L63115
               dd% = pos(sort$ = sav_dept_scr$)
               gosub print_total
               sav_dept_scr$ = dept_scr$

L63115:     gosub lookup_color
            gosub lookup_screen
            gosub lookup_hinge
            init(" ") grid$                      /* Set Center Bar Loc */
            grid$ = "        *** **/**        "  /* APC MOD - 08/26/97 */
            if len(ht2$) > 1 then str(grid$,9%,9%) = ht2$

L63165:

            sav_wd1$ = wd1$
            sav_ht1$ = ht1$
            sav_wd$  = wd$
            sav_ht$  = ht$

            gosub print_detail

            goto print_next_screen_cb
        report_done_screen_cb
            dd% = pos(sort$ = sav_dept_scr$)
            gosub print_total
            call "SETPRNT" (" ","SC02", 0%, 1%)
        return


                                       /* Pass% = 0% Reg. Screen Report*/
        print_report_screen            /* Pass% = 1% Center Bar Report */
            init(" ") date$, rpt_time$, title$
            lcnt% = 99%   :  pageno% = 0%
            wrk_qty1% = 0%                          /*  (EWD009)   */
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            if pass% = 0% then call "SETPRNT" (" ","SC01", 0%, 0%)       ~
                          else call "SETPRNT" (" ","SC02", 0%, 0%)
            select printer (134)
REM            title$ = "Ellison Windows and Doors"
            call "COMPNAME" (12%, title$, 0%)       /*  (EWD006)  */
            if pass% = 0% then                                           ~
               call "SHOSTAT" ("Printing Consolidated Report for Screen")~
               else call "SHOSTAT" ("Printing Center Bar Report")


            init(" ") sav_dept_scr$, wrk_key$
            dd% = 0%
            read #f%,key > wrk_key$, using L62955, wrk_rec$, wrk_qty%,    ~
                      wd$, ht$, wd1$, ht1$, eod goto report_done_screen
            dd% = 1%
            sav_dept_scr$ = str(wrk_rec$,2%,1%)

            sav_wd$ = wd$                               /*  (EWD009)  */
            sav_ht$ = ht$                               /*  (EWD009)  */
            sav_wd1$ = wd1$                               /*  (EWD009)  */
            sav_ht1$ = ht1$                               /*  (EWD009)  */
            sav_rec$ = wrk_rec$                           /*  (EWD009)  */

            goto L62965
        print_next_screen
            read #f%, using L62955, wrk_rec$, wrk_qty%, wd$, ht$,         ~
                         wd1$, ht1$, wrk_rec1$, eod goto report_done_screen
L62955:       FMT CH(60), BI(2), CH(7), CH(6), CH(9), CH(8), CH(36)

L62965:                                                     /* (EWD009) */
                                                             /*  Model */
            if str(wrk_rec$,48%,3%) <> str(sav_rec$,48%,3%)           ~
                        then goto print_data
                                                             /*  Color */
            if str(wrk_rec$,60%,1%) <> str(sav_rec$,60%,1%)           ~
                        then goto print_data
                                                            /* Screen */
            if str(wrk_rec$,45%,1%) <> str(sav_rec$,45%,1%)           ~
                        then goto print_data
                                                            /* Hinge  */
            if str(wrk_rec$,46%,2%) <> str(sav_rec$,46%,2%)           ~
                        then goto print_data

            if sav_wd$ <> wd$ or sav_ht$ <> ht$ then goto print_data
               if sav_wd1$ <> wd1$ or sav_ht1$ <> ht1$ then goto print_data
               wrk_qty1% = wrk_qty1% + wrk_qty%
               goto print_next_screen

print_data:
            dept_scr$  = str(sav_rec$,2%,1%)        /* Dept Sort Code  */
            dt_dept$   = str(sav_rec$,3%,3%)        /* Dept Code       */
            model$     = str(sav_rec$,48%,3%)       /* Model Code      */
            sc$        = str(sav_rec$,45%,1%)       /* Screen Code     */
            cl$        = str(sav_rec$,60%,1%)       /* Color Code      */
            wd2$       = str(sav_rec$,36%,9%)       /* CB Cut Length   */
            hg$        = str(sav_rec$,46%,2%)       /* Hinge Code      */
            ht2$       = str(sav_rec$,51%,9%)       /* CB Location     */




            if pass% = 0% then goto L63025
               if len(wd2$) < 2 then goto print_next_screen
                                                    /* Center Bar Only */
L63025:     convert wrk_qty1% to qty$, pic(#####)

            if str(wrk_rec$,6%,3%) <> "ERR" then goto L63085
               cl_l$  = "**"
               sc_r$  = "***** (Error) ******"
               wd1$   = "*********"
               ht1$   = "********"
               wd2$   = "*********"
               grid$  = str(wrk_rec$,11%,25%)
               hnge$  = "No Equation Found"
               goto L63145

L63085:     if sav_dept_scr$ = dept_scr$ then goto L63110
               dd% = pos(sort$ = sav_dept_scr$)
               gosub print_total
               sav_dept_scr$ = dept_scr$

L63110:     gosub lookup_color
            gosub lookup_screen
            gosub lookup_hinge
            init(" ") grid$                      /* Set Center Bar Loc */
            grid$ = "        *** **/**        "  /* APC MOD - 08/26/97 */
            if len(ht2$) > 1 then str(grid$,9%,9%) = ht2$

L63145:


            gosub print_detail
            sav_wd$ = wd$                               /*  (EWD009)  */
            sav_ht$ = ht$                               /*  (EWD009)  */
            sav_wd1$ = wd1$                               /*  (EWD009)  */
            sav_ht1$ = ht1$                               /*  (EWD009)  */
            sav_rec$ = wrk_rec$                           /*  (EWD009)  */
            wrk_qty1% = 0%                                /*  (EWD009)  */
            wrk_qty1% = wrk_qty1% + wrk_qty%

            goto print_next_screen
        report_done_screen
/* No screens to print */
            if wrk_qty1% = 0% then return
REM            wrk_qty1% = wrk_qty1% + 1%
            convert wrk_qty1% to qty$, pic(#####)         /*(EWD009)- BEG*/

            sav_wd$ = wd$                               /*  (EWD009)  */
            sav_ht$ = ht$                               /*  (EWD009)  */
            sav_wd1$ = wd1$                               /*  (EWD009)  */
            sav_ht1$ = ht1$                               /*  (EWD009)  */
            sav_rec$ = wrk_rec$                           /*  (EWD009)  */
            wrk_qty1% = 0%                                /*  (EWD009)  */

            dept_scr$  = str(sav_rec$,2%,1%)        /* Dept Sort Code  */
            dt_dept$   = str(sav_rec$,3%,3%)        /* Dept Code       */
            model$     = str(sav_rec$,48%,3%)       /* Model Code      */
            sc$        = str(sav_rec$,45%,1%)       /* Screen Code     */
            cl$        = str(sav_rec$,60%,1%)       /* Color Code      */
            wd2$       = str(sav_rec$,36%,9%)       /* CB Cut Length   */
            hg$        = str(sav_rec$,46%,2%)       /* Hinge Code      */
            ht2$       = str(sav_rec$,51%,9%)       /* CB Location     */

            gosub lookup_color                      /*  (AWD010)       */
            gosub lookup_screen                     /*  (AWD010)       */
            gosub lookup_hinge                      /*  (AWD010)       */
            init(" ") grid$                      /* Set Center Bar Loc */
            grid$ = "        *** **/**        "  /* APC MOD - 08/26/97 */
            if len(ht2$) > 1 then str(grid$,9%,9%) = ht2$


            gosub print_detail                            /*(EWD009- END */
            dd% = pos(sort$ = sav_dept_scr$)
            gosub print_total
            if pass% = 0% then call "SETPRNT" (" ","SC01", 0%, 1%)       ~
                          else call "SETPRNT" (" ","SC02", 0%, 1%)
        return

        print_labels_screen
            init(" ") lab_key$, lab_rec$
            call "SETPRNT" (" ","SC03", 0%, 0%)
            select printer (134)
            call "SHOSTAT" ("Printing Piece Labels for Screen'S.")
            for i% = 1% to 6%
                print using L55490
                print using L55490
                print
            next i%

            read #6,key > lab_key$, using L63265, lab_rec$,               ~
                                              eod goto screen_labels_done
            goto L63270
        screen_labels_next
            read #6, using L63265, lab_rec$, eod goto screen_labels_done
L63265:       FMT CH(200)
L63270:     view$      = str(lab_rec$,66%,1%)       /* View            */
            model$     = str(lab_rec$,67%,3%)       /* Model Code      */
            co_or$     = str(lab_rec$,70%,8%)       /* Cot/Or-Samp,Disp*/
            dt_so$     = str(lab_rec$,78%,8%)       /* Sales Order     */
            dt_seq$    = str(lab_rec$,86%,5%)       /* Seq Number      */
 /* <AWD013> */
            dt_dept$    = str(lab_rec$,3%,3%)
            dt_proc$    = str(lab_rec$,181%,2%)
            dt_barcode$ = str(lab_rec$,183%,18%)
            dt_so_line$ = str(lab_rec$,191,2)
            dt_part$    = str(lab_rec$,138%,25%)      /* PART NO*/
 /* </AWD013> */
            wd1$       = str(lab_rec$,91,9%)        /* Width           */
            ht1$       = str(lab_rec$,100%,8%)      /* Height          */
            wd2$       = str(lab_rec$,108%,9%)      /* CB Cut Length   */
            wd$        = str(lab_rec$,117%,7%)      /* Window Width    */
            ht$        = str(lab_rec$,124%,6%)      /* Window Height   */
            dt_txt$    = str(lab_rec$,130%,4%)      /* Text Id.        */
            cl$        = str(lab_rec$,134%,1%)      /* Color Code      */
            hg$        = str(lab_rec$,135%,2%)      /* Hinge Code      */
            sc$        = str(lab_rec$,137%,1%)      /* Screen Code     */
            ht2$       = str(lab_rec$,51% ,9%)      /* CB Location     */
            
            gosub lookup_color
            gosub lookup_hinge
            gosub lookup_screen
            gosub lookup_text
            gosub print_labels_detail
            goto screen_labels_next
        screen_labels_done
            call "SETPRNT" (" ","SC03", 0%, 1%)
        return

        load_tables
           call "SHOSTAT" ("Loading Screen Batch/Sort Codes")
           sort$ = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           /*  BATCH%(DD%,1%) -> Batch Size Limit from Table       */
           /*  BATCH%(DD%,2%) -> Screen Counter                    */
           /*  BATCH%(DD%,3%) -> Half Screen Counter               */
           /*  BATCH%(DD%,4%) -> Full Screen Counter               */
           /*  BATCH%(DD%,5%) -> Cottage/Oriel Counter             */
           /*  BATCH%(DD%,6%) -> Sample/Display Counter            */
           /*  DEPT$()              -> Dept. Code                  */

           init(" ") readkey$, dept$(), byfdept$()
           mat batch% = zer
           dept_max% = 0% : dd% = 0%
           str(readkey$,1%,9%)   = "SCREEN07 "
        load_next
           read #3,key > readkey$,using L63490, readkey$, descr$,         ~
                                                         eod goto L63535
L63490:       FMT CH(24), CH(30)
           if str(readkey$,1%,9%) <> "SCREEN07 " then goto L63535
              if str(readkey$,10%,3%) = "000" then goto load_next
              dd% = dd% + 1%
              dept$(dd%)   = str(descr$,1%,3%)     /* Store Dept Code */
              byfdept$(dd%)   = str(descr$,1%,3%)  /* (EWD002)        */
              rtedept$(dd%)   = str(descr$,1%,3%)  /* (EWD005)        */
              batch_amt$   = str(descr$,6%,4%)     /* Store Batch Size*/
              convert batch_amt$ to batch%(dd%,1%),data goto L63545

              byfbatch%(dd%,1%) = batch%(dd%,1%)   /* (EWD002)        */
              rtebatch%(dd%,1%) = batch%(dd%,1%)   /* (EWD005)        */
              goto load_next
L63535:    dept_max% = dd%
        return
L63545:    errormsg$ = "(Error) Problem Initializing Batch Array?"
           gosub error_prompt
           goto exit_program
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#5,mode$, 500%, f2%)
               if f2% <> 0% then goto L63665
            call "WORKOPN2" (#6,mode$, 500%, f2%)
               if f2% <> 0% then goto L63680
            call "WORKOPN2" (#11,mode$, 500%, f2%)        /* (EWD002) */
               if f2% <> 0% then goto L63685
            call "WORKOPN2" (#14,mode$, 500%, f2%)        /* (EWD005) */
               if f2% <> 0% then goto L63690
            call "WORKOPN2" (#17,mode$, 500%, f2%)        /*  (EWD009) */
               if f2% <> 0% then goto L63670
            call "WORKOPN2" (#19,mode$, 500%, f2%)        /*  (EWD009) */
               if f2% <> 0% then goto L63675
        return
L63665:     errormsg$ = "Error - Cannot Open (APCPLNW1)"
            gosub error_prompt
        return
L63680:     errormsg$ = "Error - Cannot Open (APCPLNW2)"
            gosub error_prompt
        return
L63685:     errormsg$ = "Error - Cannot Open (EWDBAYWK)"
            gosub error_prompt
        return
L63690:     errormsg$ = "Error - Cannot Open (EWDRTEWK)"
            gosub error_prompt
        return
L63670:     errormsg$ = "Error - Cannot Open (APCPLNW3)"
            gosub error_prompt
        return                                  /*  (EWD009) */
L63675:     errormsg$ = "Error - Cannot Open (APCPLNW4)"
            gosub error_prompt
        return
        delete_work
            call "FILEBGON" (#5)
            call "FILEBGON" (#6)
            call "FILEBGON" (#11)
            call "FILEBGON" (#14)
            call "FILEBGON" (#17)               /*  (EWD009) */
            call "FILEBGON" (#19)
        return

        check_screen                     /* Done once per MFG Product  */
            if sav_screen$ = dt_part$ then return
            init(" ") readkey$, desc$, wt$, wcode$, skey$, sqty$, sc$,   ~
                      model$, sav_screen$, hg$, wcode1$, sqty1$

            sav_screen$ = dt_part$
            model$ = str(dt_part$,1%,3%)
            check% = 0% : error% = 0% : sqty% = 1%
            hg$ = str(dt_part$,9%,2%)          /* Set Hinge Code       */
            sc$ = str(dt_part$,11%,1%)         /* Set Screen Code      */
                                               /* Valid Screen Options */
                                               /* (AWD014) */
REM            p% = pos("12389ABCJ" = sc$)
/* (AWD027) add code 7 OGO */
/* (SR80152) add code V */
            p% = pos("04567EKOVYZ"= sc$)        /* codes with no screen */
REM            if p% = 0% then return
            if p% <> 0% then return

/* (AWD019) */
/* (AWD021) moved up to force R or E with wcode and wcode1 */
            str(skey$,4%,1%) = "H"             /* Set to Half Screen   */
            if sc$ = "2" then str(skey$,4%,1%) = "F"  /* Full Screen   */
/* (AWD014) */
            if sc$ = "A" then str(skey$,4%,1%) = "F"  /* Full Screen   */
            if sc$ = "B" then str(skey$,4%,1%) = "F"  /* Full Screen   */
/* Has to be half screen on Door Screen Only  - For David */
REM IF MODEL$ = "353" THEN STR(SKEY$,4%,1%) = "H"
REM IF MODEL$ = "363" THEN STR(SKEY$,4%,1%) = "H"
REM IF MODEL$ = "358" THEN STR(SKEY$,4%,1%) = "H"           /*(AWD018)*/
REM IF MODEL$ = "368" THEN STR(SKEY$,4%,1%) = "H"           /*(AWD018)*/
REM IF MODEL$ = "A46" THEN STR(SKEY$,4%,1%) = "H"
            if door% <> 0% then str(skey$,4%,1%) = "H"      /*(AWD018)*/

            scr_hf$   = str(skey$,4%,1%)
            scr_type$ = "R"
            gosub lookup_extruded
            str(skey$,5%,1%) = scr_type$
/* (AWD019\) */

            str(readkey$,1%,9%)   = "SCREEN04 " /* Scrn Master Control */
            str(readkey$,10%,15%) = model$      /* Product Code        */
            read #3,key = readkey$,using L63795, wt$, wcode$, wcode1$,   ~
                                                     eod goto L63950
L63795:       FMT POS(25), CH(3), XX(3), CH(1),/* WT$ - Window Type    */~
                  POS(46), CH(1)
                                               /* WCODE$ - A thru E    */
/* (AWD014) */
            if wcode$ = "E" then return        /* No Screen Required   */
            if sc$ <> "B" and sc$ <> "C" then goto not_rollform
REM                 if wcode$ =  "D" then wcode$ = "F"
REM                 if wcode$ <> "D" and wcode$ <> "F" then wcode$ = "E"
                    wcode$ = wcode1$


not_rollform:
            if wcode1$ <> " " and scr_type$ = "R"     ~
                     then wcode$ = wcode1$            /*(AWD021)*/

            str(skey$,1%,1%) = wcode$          /* Set Window Type Code */
            str(skey$,2%,2%) = "NO"            /* Set to (NO)rmal      */
            gosub lookup_hinge                 /* Check Hinge Code     */
REM            if wcode$ <> "A" then goto L63850   /* 1st Check Type 'A' */
                                               /* (EWD004) Added 'D'  */

                                                       /* (AWD014) */
            if wcode$ <> "A" and wcode$ <> "D" and wcode$ <> "F"    and  ~
                     wcode$ <> "I"     then goto L63850
               if str(hh$,1%,2%) = "CO" or str(hh$,1%,2%) = "OR" then    ~
                                      str(skey$,2%,2%) = str(hh$,1%,2%)

L63850:     str(skey$,4%,1%) = "H"             /* Set to Half Screen   */
            if sc$ = "2" then str(skey$,4%,1%) = "F"  /* Full Screen   */
/* (AWD014) */
            if sc$ = "A" then str(skey$,4%,1%) = "F"  /* Full Screen   */
            if sc$ = "B" then str(skey$,4%,1%) = "F"  /* Full Screen   */
            if sc$ = "X" then str(skey$,4%,1%) = "F"  /* CR2217  */
            if sc$ = "W" then str(skey$,6%,1%) = "F"  /*CR2217*/
            if sc$ = "X" then str(skey$,6%,1%) = "F"  /*CR2217*/
            
/* Has to be half screen on Door Screen Only  - For David */
            if model$ = "353" then str(skey$,4%,1%) = "H"
            if model$ = "363" then str(skey$,4%,1%) = "H"
            if model$ = "358" then str(skey$,4%,1%) = "H"      /*(AWD018)*/
            if model$ = "368" then str(skey$,4%,1%) = "H"      /*(AWD018)*/
            if model$ = "A46" then str(skey$,4%,1%) = "H"
            if door% <> 0% then str(skey$,4%,1%) = "H"         /*(AWD018)*/

            if wcode$ <> "C" and wcode$ <> "G"  then goto L63885
               sqty% = 1%
               gosub check_screen01
               str(skey$,4%,1%) = "4"          /* Special 1/4,1/2,1/4  */
               if triple% <> 0% then str(skey$,4%,1%) = "3" /* Special */
REM               str(skey$,5%,1%) = "R"      /*this is b/c the roll form*/
                                        /* phantom numbers are in system */
                                                /* Multiple Screens     */
L63885:     if wcode$ <> "D" and wcode$ <> "I" then goto L63940
                                                /* (EWD004) 421 & 431   */
                                                /* Can have full screen */
REM STR(SKEY$,4%,1%) = "H"          /* HALF SCREEN ONLY    */

               init(" ") readkey$, desc$
               str(readkey$,1%,9%)   = "GLASS01  " /* Screen Quantities*/
               str(readkey$,10%,3%)  = model$      /* Product Code     */
               str(readkey$,13%,12%) = hg$         /* Hinge Code       */
               read #3,key = readkey$, using L63920, sqty$, sqty1$,   ~
                                         eod goto L63940 /*(SR79967)*/
L63920:           FMT POS(28), CH(2), POS(41), CH(02)

               if door% <> 0 then sqty$ = sqty1$       /* (SR79967) */

               convert sqty$ to sqty%, data goto L63930

L63930:
            if sqty% = 0% then return          /* No Screen Required   */
L63940:
         check% = 1%
        return
L63950:     error% = 1%                       /* Product not on File   */
            if scr_code% <> 1% then return
               errormsg$ = err$(error%)
               gosub error_prompt
               init(" ") errormsg$
        return
            error% = 2%                  /* Screen QTY's not on File   */
            if scr_code% <> 1% then return
               errormsg$ = err$(error%)
               gosub error_prompt
               init(" ") errormsg$
        return

        check_screen01
               init(" ") readkey$
               str(readkey$,1%,9%)   = "SCREEN01 " /* Screen Quantities*/
               str(readkey$,10%,3%)  = model$      /* Product Code     */
               read #3,key = readkey$, eod goto no_screen01

                        sqty% = 2%
        no_screen01
        return

        write_bayform_file
            init(" ") dt_dept$, dt_model$    /* (AWD014) */
            f1% = 12%
            f2% = 13%
            gosub open_bayform_work

            if ritescreen% = 1% then f1% = 15%
            if ritescreen% = 1% then f2% = 16%

            lab_rec$, bay_rec$ = all(hex(20))
            for i% = 1% to 6%
                str(bay_rec$,1%,49%) =                                 ~
                     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                gosub write_rec
                gosub write_rec
                bay_rec$ = all(hex(20))
                gosub write_rec
            next i%
            lblcnt% = 0%                                /* CR 986 */
        bayform_file_next
           read #f1%, hold, key > lab_rec$, using L62575, lab_rec$,    ~
                                               eod goto write_bayform_done

           delete #f1%

           str(bay_rec$,1%,100%) = all(hex(20))
           str(bay_rec$,1%,1%)  = str(lab_rec$,17%,1%)
           str(bay_rec$,2%,1%)  = " "

                                                   /*  (EWD007)  */
           if f1% <> 12% then                                          ~
           str(bay_rec$,3%,3%)  = str(lab_rec$,14%,3%)                 ~
           else                                                        ~
           str(bay_rec$,3%,3%)  = str(lab_rec$,1%,3%)



           init(" ") extruded$,sc$                    /* (AWD015) */
           sc$ = str(lab_rec$,89%,2%)
           p% = 0%
           p% = pos("2AGJMQ" = sc$)
           if p% <> 0% then extruded$ = "E"
           if f1% <> 12% then goto not_bayform
                     /* Department 002 */
             if str(lab_rec$,9%,3%) = "002" then extruded$ = "E"
                     /* Models 432 and 204 screen only for dept 002 */
             if str(lab_rec$,1%,3%) = "432" then extruded$ = "E"
             if str(lab_rec$,1%,3%) = "204" then extruded$ = "E"

not_bayform
/* (AWD015) */
          if sc$ = "J" and str(lab_rec$,9%,3%) = "007" then extruded$ = " "
          if sc$ = "J" and str(lab_rec$,9%,3%) = "049" then extruded$ = " "


           str(bay_rec$,2%,1%) = extruded$

/* (AWD014) 12 = bayform */
/* Make Department 007 model 215 when sending to bayform / ASM */
/* because 007 and 049 both have 267 models and it will not be */
/* Packaged correctly coming from ASM Screens                  */

           if f1% = 12% then dt_dept$  = str(lab_rec$,9%,3%)
           if f1% = 12% then dt_model$ = str(lab_rec$,1%,3%)
           if dt_dept$ = "007" and dt_model$ = "267"                     ~
                   then str(bay_rec$,3%,3%) = "215"

           str(bay_rec$,6%,1%)  = " "
           str(bay_rec$,7%,2%)  = str(lab_rec$,26%,2%)
           str(bay_rec$,9%,1%)  = " "
           str(bay_rec$,10%,4%) = str(lab_rec$,28%,4%)
           str(bay_rec$,14%,1%) = " "
           str(bay_rec$,15%,8%) = str(lab_rec$,32%,8%)
           str(bay_rec$,23%,3%) = " CL"
           str(bay_rec$,26%,9%) = str(lab_rec$,66%,9%)
           str(bay_rec$,35%,1%) = " "
           str(bay_rec$,36%,8%) = str(lab_rec$,18%,8%)
           str(bay_rec$,44%,1%) = " "
                                                   /*  (EWD007)  */
           if f1% <> 12% then                                          ~
           str(bay_rec$,45%,5%)  = str(lab_rec$,4%,5%)                  ~
           else                                                        ~
           str(bay_rec$,45%,5%)  = str(lab_rec$,12%,5%)

           gosub write_rec
           str(bay_rec$,1%,100%) = all(hex(20))
           str(bay_rec$,1%,9%)  = str(lab_rec$,40%,9%)
           str(bay_rec$,10%,1%) = "X"
           str(bay_rec$,11%,8%) = str(lab_rec$,49%,8%)
           str(bay_rec$,19%,3%) = "  CB"
           str(bay_rec$,22%,9%) = str(lab_rec$,57%,9%)
           str(bay_rec$,31%,3%) = " A:"
           str(bay_rec$,34%,7%) = str(lab_rec$,75%,7%)
           str(bay_rec$,41%,1%) = "X"
           str(bay_rec$,42%,6%) = str(lab_rec$,82%,6%)
           str(bay_rec$,48%,1%) = " "
           str(bay_rec$,49%,1%) = str(lab_rec$,88%,1%)
           gosub write_rec
           str(bay_rec$,1%,100%) = all(hex(20))
           gosub write_rec
           lblcnt% = lblcnt% + 1%                                /* CR 986 */
        goto bayform_file_next
        write_bayform_done
           str(bay_rec$,1%,100%) = "EOF"            /* (AWD011) */
           gosub write_rec                          /* (AWD011) */
           convert lblcnt% to lblcnt$, pic(000000000)            /* CR 986 */
           str(bay_rec$,1%,100%) = "  TOTAL LABELS  " & lblcnt$  /*CR 986 */
           gosub write_rec 
           call "FILEBGON" (#f1%)
        return

        write_rec
           write #f2%, using L62575, bay_rec$, eod goto L62590
        return

        send_bayform_file
           comp% = 2%         :  j% = dept_max%          :  bayform% = 0%
           hdr$ = "*********** (ATTENTION) *************"
           msg$(1%) = " You created a file to send to BayForm !!  "
           msg$(2%) = "    Do You Want to Send it Now ??          "
           msg$(3%) = "Press PF30 to Send or Any Other Key to Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if comp% = 30% then goto ftp_bayfm
                goto check_ritescreen
ftp_bayfm:
           bayform% = 1%
           gosub write_bayform_file
           call "SHOSTAT" (" SENDING BAYFORM INFORMATION!! ")
           fx1% = 0% : fx2% = 0%
           call "LINK" addr(script$, fx1%, fx2%)     /* Call Script HERE */
           if fx1%    > 0% then goto ftp_err
              bayform% = 0%
              goto check_ritescreen
ftp_err:   errormsg$ = "The file did not FTP to BayForm, Contact Systems!!"
           gosub error_prompt
           init(" ") errormsg$
              goto check_ritescreen

        send_ritescreen_file
           comp% = 2%         :  j% = dept_max%          :  ritescreen% = 0%
           hdr$ = "*********** (ATTENTION) *************"
           msg$(1%) = " You created a file to send to RiteScreen !!  "
           msg$(2%) = "    Do You Want to Send it Now ??          "
           msg$(3%) = "Press PF30 to Send or Any Other Key to Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if comp% = 30% then goto ftp_ritesn
                goto end_program
ftp_ritesn:
           ritescreen% = 1%
           gosub write_bayform_file
           call "SHOSTAT" (" SENDING RITESCREEN INFORMATION!! ")
           fx1% = 0% : fx2% = 0%
           call "LINK" addr(script_rte$, fx1%, fx2%) /* Call Script HERE */
           if fx1%    > 0% then goto ftp_err_rte
              ritescreen% = 0%
              goto end_program
ftp_err_rte:
           errormsg$ = "The file did not FTP to BayForm, Contact Systems!!"
           gosub error_prompt
           init(" ") errormsg$
              goto end_program


errormsg:
           comp% = 2%
           hdr$ = "**************** (ERROR) **********************"
           msg$(1%) = " Barcode --> " & dt_barcode$ & "  Seq --> " & dt_seq$
           msg$(2%) = "  Dept -->  " & dt_dept$ & " Part --> " & dt_part$
           msg$(3%) = "         Press Any Key to Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        isDoor
           door% = 0%
           dp% = 0%
REM    IF STR(DT_PART$,1,3) = "358" THEN GOTO SCREENDOOR
           init(" ") readkey$, descr$
           str(readkey$,1%,9%) = "PLAN DOOR"
           str(readkey$,10%,15%) = str(dt_part$,1%,3%)
           read #3,key = readkey$, using L51910, descr$,   ~
                                            eod goto noDoor

REM    IF STR(DT_PART$,1,3)="388" THEN GOTO NOTDOORYET
                  door% = 1%
                  convert str(descr$,29,2) to dp%, data goto noDoor

                  if dp% = 50% then door% = 2%

        noDoor
         gosub screenDoor
        return
       notDoorYet
        door% = 0%
       return
       screenDoor                                      /* (AWD020) */
REM  DP% = 0%
        init(" ") readkey$, descr$
        str(readkey$,1%,9%) = "SCRNDOOR"
        str(readkey$,10%,15%) = str(dt_part$,1%,3%)
        read #3, key = readkey$, using L51910, descr$,   ~
                                           eod goto noScreenDoor

           dp% = 35%
           convert str(descr$,3%,2%) to dp%, data goto badDp
badDp:
           door% = 3%            /* For Screen Only */
       return
       noScreenDoor
REM        IF STR(DT_PART$,1%,3%) = "358" THEN DOOR% = 3%
       return                                            /* (AWD020) */

        /* (AWD019) */
        loadPhantoms
          init(" ") readkey$, descr$, ss$(), skey$()
          ss% = 0%
          rp% = 0%                    /* Beginning of Roll Form Phantoms */
          str(readkey$,1%,9%) = "SCRNPHANT"
        phantonNext
          read #3,key > readkey$, using phantFMT, readkey$, descr$,     ~
                                                   eod goto phantonDone

phantFMT:         FMT  CH(24), CH(30)

               if str(readkey$,1%,9%) <> "SCRNPHANT" then goto phantonDone
               ss% = ss% + 1%
               skey$(ss%) = str(readkey$,10%,6%)
               ss$(ss%)   = str(descr$,1,16)

               if rp% = 0% and str(readkey$,10%,1%) = "F" then rp% = ss%
               goto phantonNext
        phantonDone
          ss_max% = ss%
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            j% = 0%
            for j% = 1% to dept_max%
              if byfbatch%(j%,2%) <> 0% then goto send_bayform_file
            next j%
        check_ritescreen
            j% = 0%
            for j% = 1% to dept_max%
              if rtebatch%(j%,2%) <> 0% then goto send_ritescreen_file
            next j%
        end_program
            call "FILEBGON" (#12)
            call "FILEBGON" (#13)
            call "FILEBGON" (#15)                         /* (EWD005)    */
            call "FILEBGON" (#16)                         /* (EWD005)    */
        end

REM ##################################################

        oracle_connect
            init(" ") user$, pass$, server$
            user$   = "mfg"
            pass$   = "mfg"
            server$ = " "
            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)
        return

        oracle_flush
         return
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_query
         return
            oci_err% = 0%
            call "FLUSH" (oci_err%)
            call "QUERY1" (stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                          stmt6$, stmt7$, stmt8$, oci_err%)
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                      stmt6$, stmt7$, stmt8$

        return

        oracle_exec1
         return
            oci_err% = 0%
            call "FLUSH" (oci_err%)
            call "EXEC1" (stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                          stmt6$, stmt7$, stmt8$, oci_err%)
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                      stmt6$, stmt7$, stmt8$
        return

        oracle_fetch
         return
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
        return

        oracle_getfield
         return
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$, message$
            init(hex(00)) field$
            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)

        return

cnv_error:
        message$ = "error"
            call "LOGFILE" (message$)
        return

        open_bayform_work
            volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"
            open nodisplay #13, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = "EWDBAYFM",         ~
                library = "FTPBAYFM", volume = volume$, blocks = 5%

            open nodisplay #16, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = "EWDRTESN",         ~
                library = "FTPRTESN", volume = volume$, blocks = 5%
        return



/* (AWD029) */
        lookup_subpart
             init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$()
             dim1es, dim2es, dim3es = 0.00
               so_inv%, item_no%, suberr1% = 0%

               flag$ = "0"
               pgm$  = "1"



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
                          suberr1%)      /* Error Code                 */

            get bcksubpt_rec$ using dimFmt, dim1es, dim2es
dimFMT:               FMT POS(153), PD(14,4), PD(14,4)


            if suberr1% = 0% then return



            str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            errormsg$ = "AWDBKSUB ERROR = "&so_inv$ & " Line= " & item_no$
            gosub error_prompt
            suberr1% = 0%

        return

/*(\AWD029) */