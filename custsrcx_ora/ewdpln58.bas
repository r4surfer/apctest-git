        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN58                             *~
            *  Creation Date     - 09/07/98                             *~
            *  Last Mod Date     - 09/19/2019                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Analysis Special Scheduled Glass and *~
            *                      Grid. Data Trapped at S.O. Entry.    *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SCH1), (PLAN SCH2)             *~
            *                                                           *~
            *  Subroutine Used   - (EWDPLA58) Display Customer Info     *~
            *                      (APCPLN3B) Calculate New Due Date    *~
            *                      (EWDGLSSB) Calculate Glass Cuts      *~
            *                      (EWDPLB58) Verify Delete             *~
            *                      (EWDPLC58) Print Labels on Zebra     *~
            *                                                           *~
            *  Special Comments  - gen_rpt1 - Create Tempered Glass Rpt *~
            *                      print_dtl- Print Report Detail       *~
            *                                                           *~
            *                      gen_labels     - Print Glas Labels   *~
            *                      print_lab_temp - Write to file       *~
            *                      build_lab_temp - Build Label Record  *~
            *                                                           *~
            *                      update_specials  - Place or Release  *~
            *                      editScheduleHld  - Check Records     *~
            *                      modifyScheduleHld- Modify Records    *~
            *                      update_apcplnor  - Update            *~
            *                      update_apcplnsc  - Update            *~
            *                                                           *~
            *                      delete_specials - Delete Type '0's   *~
            *                                                           *~
            *                      release_specials - Release to Plan   *~
            *                      update_apcplnor_rel - Update         *~
            *                      update_apcplnsc_rel - Update         *~
            *                                                           *~
            *  sp_flag% = (1%) Place Items On Order                     *~
            *             (2%) Receive Items On Order                   *~
            *             (3%) Release Items On-Order                   *~
            *             (4%) Delete Special Orders (Type = '0' Only)  *~
            *                                                           *~
            *  rpt%     = (0%) Regular Report based on Selections       *~
            *             (1%) Tempered Glass Labels Only               *~
            *                                                           *~
            *  report%  = (0%) Glass Report/Labels from Display Screen  *~
            *             (1%) Glass Reports/Labels from Main Screen    *~
            *                                                           *~
            *                                                           *~
            *             single strength                 spType% = 1%  *~
            *             double strength                 spType% = 2%  *~
            *             tempered double strength        spType% = 3%  *~
            *             SDL Single strength             spType% = 4%  *~
            *             SDL Double strength             spType% = 5%  *~
            *             SDL Temp Double strength        spType% = 6%  *~
            *             Triple Strength > 25SqFt 3/16   spType% = 7%  *~
            *             Door 5/32 Triple Strength       spType% = 8%  *~
            *             Laminate Glass                  spType% = 9%  *~
            *             Laminate Strengthened Glass     spType% =10%  *~
            *             Single Glazed Tempered          spType% =11%  *~
            * Laminate Cannot have 5/32 or 3/16 (2015/07/09)            */~
            *             Laminate 5/32                   spType% =12%  *~
            *             Laminate Strengthened 5/32      spType% =13%  *~
            *             Laminate 3/16                   spType% =14%  *~
            *             Laminate Strengthened 3/16      spType% =15%  *~
            *             STC 1/8 and 3/16                spType% =16%  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/16/98 ! (New) Program                            ! RHH *~
            * 09/28/98 ! (EWD001) - Activate the Freezing of Sales! RHH *~
            *          !   Orders for Tempered Glass              !     *~
            * 10/13/98 ! (EWD002) - Preeserve Status of orig S.O. ! RHH *~
            * 11/06/98 ! (EWD003) - Mods for Private Label        ! RHH *~
            * 12/08/98 ! (EWD004) - Mod to Verify Delete          ! RHH *~
            * 01/22/99 ! (EWD005) -Mod change Dimension of gdd$() ! RHH *~
            *          !   to (12) and add Rack File (EWDPLNRK)   ! RHH *~
            * 06/24/99 ! (EWD006) - Mod to fix Grid bug           ! RHH *~
            * 07/21/99 ! (EWD007) - Mod to fix stock tempered     ! RHH *~
            *          !            when other ordered glass is   !     *~
            *          !            released. The stock tempered  !     *~
            *          !            needs to be deleted from      !     *~
            *          !            the special glass Database.   !     *~
            * 11/09/99 ! (EWD008) - Switch Labels to Zebra Printer! RHH *~
            * 12/16/99 ! (EWD009) - New codes for Sample Repair(SR)!RHH *~
            *          !            Back Order (BO) and new Open  !     *~
            * 06/12/01 ! (EWD010) - Mod to subtract 7 days from   ! CMG *~
            *          !            due day because of Lowe's     !     *~
            *          !            extented time.                !     *~
            * 06/18/02 ! (EWD011) - Mod to change company name    ! TLM *~
            * 06/14/02 ! (EWD012) - Mod to add 2 special codes to ! CMG *~
            *          !            display screen                !     *~
            * 06/14/02 ! (EWD013) - Mod to add begin and end cut- ! CMG *~
            *          !            off to screen.                !     *~
            * 07/10/03 ! (EWD014) - Mod to add 'W' for 3/4 inch   ! CMG *~
            *          !            grid.                         !     *~
            * 09/04/03 ! (EWD015) - Mod to create Temp File.      ! CMG *~
            * 02/25/04 ! (EWD017) - Mod to add how ship for screen! CMG *~
            *          !               criteria                   !     *~
            * 07/15/04 ! (EWD018) - Mod to put (RO) on screen for ! CMG *~
            *          !               hows '21'.                 !     *~
            * 11/16/05 ! (AWD019) - Mod for Cardinal Glass File   ! CMG *~
            * 01/01/05 ! (PAR000) - CR347 Mod for new sub part    ! CMG *~
            * 03/16/06 ! (AWD020) - modification for North East   ! CMG *~
            *05/30/2007! (AWD021) - modification for 18mm         ! DES *~
            *08/15/2007! (AWD022) - mod for Impact window         ! CMG *~
            *08/29/2008! (AWD023) - mod to look at gridcolor & SDL! CMG *~
            *08/30/2008! (AWD024) - mod for SDL Shapes order file ! CMG *~
            *10/28/2008! (AWD025) - mod to add extra field to     ! CMG *~
            *          !      EWDPLA58 call                       !     *~
            *11/24/2009! (AWD026) - mod for ultra intercept       ! CMG *~
            *03/17/2010! (AWD027) - mod for ritescreen patio doors! CMG *~
            *03/31/2010! (AWD028) - mod for tempered order file   ! CMG *~
            *03/15/2011! (AWD029) - mod for various intercept     ! CMG *~
            *11/28/2011! (AWD030) - mods for new tempered process ! CMG *~
            *12/15/2011! (AWD031) - modification for tempered PO  ! CMG *~
            *05/18/2012! (AWD032) - modification for triple pane  ! CMG *~
            *12/03/2012! (AWD033) - modifications for valance lbl ! CMG *~
            *03/25/2013! (AWD034) - mod for lowe270 combo gls code! CMG *~
            *06/11/2013! (AWD035) SGP laminate glass              ! CMG *~
            *08/30/2013! (AWD036) - mod for thermal on hold &     ! CMG *~
            *          !    assign warranty numbers               !     *~
            *03/17/2014! (AWD037) - mod for NC lamin              ! CMG *~
            *04/30/2014! (AWD038) mod to check for casement model ! CMG *~
            *07/21/2014! (AWD039) mods for lite lift balance      ! CMG *~
            *12/09/2014! (AWD040) mods use HLDSCHED not EWDSCHED  ! CMG *~
            *05/15/2015! (IM8022) mod for glass remakes           ! CMG *~
            *06/05/2015! (SR64679) large ntx windows made duraseal! CMG *~
            *09/23/2015! (SR69095) thermal DateStateCahgne fix    ! MES *~
            *10/21/2015! (SR70007) mod to correct litelift balance! CMG *~
            *          !   ordering                               !     *~
            *11/24/2015! (SR70932) Order Date & Due Date Toggle   ! CMG *~
            *04/12/2016! (SR74117) mod for lite lift balances     ! CMG *~
            *04/03/2017! (CR893) lookup parent cutoff when custome! CMN *~
            *          !   CV0999 & add region to screen          !     *~
            *04/11/2017! (CR929) ECR# 2017-032 130 2 & 3 lite sldr! CMN *~
            *04/25/2017! (CR459) Prevent NC and NTX accessing     ! RDB *~
            *          !    warranty at the same time while plng  !     *~
            *05/08/2017! Add user information to file             ! RDB *~
            *09/27/2017! (CR1114) Tempered glass order filter     ! RDB *~
            *          !   for selection.                         !     *~
            *11/29/2017! (CR1173) Mods for STC Larson Window      ! CMN *~
            *07/31/2018! CR1578 Set DIM1ES for displays           ! RDB *~
            *10/08/2018! CR1720 Add TX to CR1114 change           ! RDB *~
            *05/01/2019! (CR1988) mod for Triple, Quad, STC glass ! CMN *~
            *06/03/2019! (CR2055) Remove TX Duralite Logic        ! CMN *~
            *06/03/2019! (CR2056) Additional Fields in Cardinal   ! CMN *~
            *          !    EDI file                              !     *~
            *09/16/2019! (CR2237) TX Freezing when ordering temper! CMN *~
            *09/19/2019! (CR2245) Create Flex Screen Order Data   ! CMN *~
            *10/11/2019! (CR2190) Add glass text to label         ! RDB *~
            *11/23/2020! (SR98672 CR2717) Increase display        ! RDB *~
            *11/25/2020! (CR2620) Triple pane chg and layout      ! RDB *~
            *12/14/2020! (CR2732) Increase display pages          ! RDB *~
            *01/14/2021! (CR2747) STC issue PO lookup             ! RDB *~
            *02/19/2021! (CR2779) Set key for Customer lookup     ! RDB *~
			*10/18/2022! (CR3178) New clay color code             ! RDB *~	
            *11/30/2022! (CR3198) Paint hardcode 0 for sales order! RDB *~	
            *04/14/2023! CR3287 Obscure glass strgth for Cardinal ! RDB *~			
            *************************************************************

        dim                                                              ~
            title$45, company$40,        /* Analysis Title and Time    */~
            beg_dte$6, beg_date$10,      /* Beg/End Delivery Date      */~
            end_dte$6, end_date$10,      /*                            */~
            beg_cut$2, end_cut$2,        /* Beg/End Cut-Off   (EWD013) */~
            beg_cut_d$35, end_cut_d$35,  /* Beg/End Cut-Off DESC(EWD013)*/~
            cutoff_a$2, cutoff_b$,       /* Customer Cut-Off    (EWD013)*/~
            beg_how$2, end_how$2,        /* Customer How Ship   (EWD017)*/~
            beg_how_d$35, end_how_d$35,  /* How Ship Description(EWD017)*/~
            x$10, pf_txt$20,             /* Date Buffer                */~
            sc_status$1, sc_status_d$30, /* Special Analysis Process Cd*/~
            sc_type$1, sc_type_d$30,     /* Analysis Type Process Code */~
            sc_process$1,sc_process_d$35,/* Process Selection Codes    */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            readkey1$50,                 /* GENCODES Lookup 1          */~
            sp_status_dte$6,             /* Special Analysis Status Dte*/~
            sp_status_date$8,            /* Formated                   */~
            sp_status$1, sp_st$1,        /* Special Analysis Process Cd*/~
            sp_type$1, or_st$2,          /* Analysis Type Process Code */~
            sp_route$5, or_hows$2,       /* Route code                 */~
            sp_cutoff$2,                 /* cut off Day 1 thru 7       */~
            sp_cust$9,                   /* Customer Code              */~
            sp_so$8,                     /* Sales order number         */~
            sp_ln$2,                     /* S.O. Line item No.         */~
            sp_ln_item$4,                /* Line Item Piece            */~
            sp_due$6, sp_due_date$8,     /* Sales order Due Date       */~
            sp_part$25, sp_part_d$32,    /* Part Number                */~
            sp_qty$4,                    /* line Item Quantity         */~
            sp_stat$2,                   /* (PLAN STAT) Planning       */~
            sp_usr$3,                    /* Last Mod User Id           */~
            sp_dte$6, sp_date$8,         /* Last Mod Date              */~
            sp_time$8,                   /* Last Mod Time              */~
            sp_text$4,                   /* Line Item Text Id          */~
            sp_primary$3,                /* Primary Dept       (EWD015)*/~
            sp_unitid$10,                /* WW UnitID (AWD030)         */~
            sp_warr$9,                   /* Glass Warranty ID          */~
            sp_fil$19,                   /* Filler Area                */~
            hdFil$145,                   /* Filler Area                */~
            sp_lam$1,                    /* Laminate Type L/S (AWD037) */~
            sp_lamFl$1,                  /* Lamn Flag         (AWD037) */~
            sp_igstr1$1,                 /*SandwichCodeStrength(CR1173)*/~
            sp_igstr2$1,                 /*SandwichCodeStrength(CR1173)*/~
            sp_sand1$2,                  /* First Sandwich Code(EWD015)*/~
            sp_sand2$2,                  /* Sec Sandwich Code  (EWD015)*/~
            sp_sand3$2,                  /* Sec Sandwich Code  (AWD032)*/~
            sandwich$(3%)30,             /* Data to write File (EWD015)*/~
            csandwich$(3%)30,            /* Cardinal Sandwich   (AWD019)*/~
            sp_lits$4,                   /* Number of Lites    (EWD015)*/~
            tp_glass$2,                  /* Tempered Glass     (EWD015)*/~
            tp_color$1,                  /* Tempered Color     (EWD015)*/~
            tp_liting$2,                 /* Tempered Liting    (AWD030)*/~
            sp_temp$2,                   /* Tempered Field     (EWD015)*/~
            sp_temp1$3,                  /* Tempered Field     (EWD015)*/~
            sp_temp2$2,                  /* Tempered Field     (EWD015)*/~
            sp_rack$1,                   /* Rack Type '0' or '1'EWD015*/~
            spType$3,                    /* Spacer Type (AWD028)       */~
            strength$1,                  /* Glass Strength (AWD028)    */~
            tpSand$10,                   /* Tempered Sand  (AWD028)    */~
            sp_view$1,                   /* Top/Bot View       (EWD015)*/~
            tp_comma$1,                  /* File Delimiter     (EWD015)*/~
            tp_item$4,                   /* Tempered Item      (EWD015)*/~
            tp_model$3,                  /* Tempered Model     (EWD015)*/~
            tp_so$8,                     /* Tempered So        (EWD015)*/~
            tp_ln$2,                     /* Tempered Line      (EWD015)*/~
            tp_view$3,                   /* Tempered View      (EWD015)*/~
            tp_size$20,                  /* Tempered Size      (EWD015)*/~
            tp_type$30,                  /* Tempered Glas Type (EWD015)*/~
            tp_ctype$30,                 /* Cardinal Glass Type(AWD019)*/~
            tp_lite$1,                   /* Tempered Lites     (EWD015)*/~
            tp_spacer$10,                /* Tempered Spacer    (EWD015)*/~
            tp_sp_th$10,                 /* Temp Spacer Thick  (EWD015)*/~
            sav_spacer$10,               /* Tempered Spacer    (EWD015)*/~
            sp_temp_key$100,             /* Read key Sort File (EWD015)*/~
            t_k$6,                       /* Thickness          (EWD015)*/~
            sd1$24, sd2$24,              /* Beg/End Date Screen Text   */~
            hdKey$40,  savHd$36,         /* HLDSCHED Key (AWD027)      */~
            hdKey1$25,                   /* HLDSCHED KEY1 (SR70932)    */~
            hdRec$256,                   /* HLDSCHED Rec (AWD027)      */~
            sav_ord$8, sav_order$17,     /* Save Sales Order           */~
            sav_line$10,                 /* Sales order Line item      */~
            sav_bar$18,                  /* Saved Barcode              */~
            dfltdue$8, fob$20,           /* Calcualted new Due Date    */~
            newdue$6,                    /* new due date               */~
            bck_key$25,                  /* BCKMASTR, BCKLINES         */~
            txt$(10%)30,                 /* Shostat Text               */~
            or_rec$170,                  /* APCPLNOR Record            */~
            or_po$16,                    /* APCPLNOR Po Number (AWD027)*/~
			or_prefix$1,                 /* APCPLNOR prefix sales order*/~
            sc_key$10,                   /* (APCPLNSC) Primary         */~
            sc_rec$128,                  /* APCPLNSC Record            */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8,                      /* Sales Order       (EWD003) */~
            s_ln$3,                      /*** Line Item (3)   (EWD003) */~
            s_prv$30,                    /* Private Label Name(EWD003) */~
            s_1$2,                       /* Private Label Code(EWD003) */~
            code$10, tab$(40%)9,         /* Tables for Lookup          */~
            h1$12, h2$12, h3$12, h4$12,  /* Summary Screen Display     */~
            h5$12, h6$12, h7$25, h8$25,  /*                            */~
            h9$12, h10$12,               /*  (CR893)                   */~
            h_sd$79,                     /* Screen Detail Header       */~
            sel$(6%)20,                  /* Display Text               */~
            textid$4,                    /* Text Id's                  */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            jdate$7,                     /* Julian Date       (EWD010) */~
            updSchedSt$1,                /* (AWD037) Updt EWDSCHED, HLDSCHED */~
            updStatus$2,                 /* (AWD037) Updt Planning file*/~
            plng_pgm$8,                  /* Program name currenlty planning */~
            plnguser$3                   /* Planning user ID           */


        dim ctt$(10%,3%)9, dept$3,       /* 1-5=Top, 6-10=Bot          */~
            gdd$(12%)10, view$3,         /* 1-6=Top, 7-12=Bot (EWD005) */~
            ajj$(10%,2%)6,               /* 1-5=Top, 6-10=Bot          */~
            dcc$(10%,2%)8,               /* 1-5=Top, 6-10=Bot          */~
            wd$7, wd1$9,                 /* Actual Width               */~
            ht$6, ht1$8,                 /* Actual Height              */~
            rk_barcode$9, rk_seq$        /* Glass/Rack Barcode(EWD005) */

        dim dt$(3000%)100,                 /* Analysis Display           */~
            sd$(3000%)100,                /* Screen Detail              */~
            cc$(3000%)1,                  /* Selection                  */~
            po$(3000%)16,                 /* Save Purchase Order number */~
            unitid$(3000%)10,             /* WW unit id (AWD030)        */~
            subpart$(3000%)20,            /* Subpart (Awd031)           */~
            dim1es(3000%),                /* Dim1es                     */~
            dim2es(3000%),                /* Dim2es                     */~
            sp_warr$(3000%)9,             /* Warranty ID                */~
            sp_num$(3000%)3               /* Remake Number              */

        dim                              /* FILE - LABEL PRINT FILES   */~
            muttin$8, lits$1, mut$8,     /* Muttin Code Vert/Horiz     */~
                                         /* (EWD006)                   */~
            t1$3, chg$1,                 /* Model Code                 */~
            t2$6,                        /* Color Code                 */~
            t3$15,                       /* Glass Description          */~
            t4$6,                        /* Liting                     */~
            t5$20,                       /* Width and Height           */~
            t6$3,                        /* View Top/Bot               */~
            t7$11,                       /* Sales Order Line Item      */~
            t8$8,                        /* Due Date                   */~
            t9$1,                        /* Contour Grid               */~
            t10$5,                       /* (AWD026) ultra             */~
            t12$40,                      /* Glass Text      CR2190     */~
            ff$8,                        /* Label Print File Name      */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6,                    /* Volume Name = 'CARLOS'     */~
            l_lt$6, lt$2,                /* Liting Left Descr          */~
            r_lt$6,                      /* Liting Right Descr         */~
            cl$1                         /* Color Code         (EWD014)*/


        dim                              /* (PAR000)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            field1$1,                    /* New Part Field 1 GRDTYPE   */~
            field2$1,                    /* New Part Field 2 GRDSIZE   */~
            field3$1,                    /* New Part Field 3 GRDCOLOR  */~
            field4$1,                    /* New Part Field 4 HARDWARE  */~
            field5$1,                    /* New Part Field 5 FOAM      */~
            field6$1,                    /* Casing        (AWD023)     */~
            field7$1,                    /* Sample Color  (AWD023)     */~
            field8$1,                    /* EXT Grid Type (AWD023)     */~
            field9$1,                    /* EXT Grid Size (AWD023)     */~
            field10$1,                   /* INT Grid Color (AWD023)     */~
            field11$1,                   /* EXT Grid COLOR (AWD023)     */~
            field17$1,                   /* EXT Grid COLOR (AWD030)     */~
            subpart$20,                  /* Subpart Number   AWD022    */~
            infopart$20                  /* Info Part Number AWD022    */

        dim                              /* (AWD019)                   */~
            header1$15,                  /* Header Information 1       */~
            header2$15,                  /* Header Information 2       */~
            header3$15,                  /* Header (AWD028)            */~
            header4$30,                  /* Header (AWD028)            */~
            header5$15                   /* Header (AWD028)            */

        dim                              /* (AWD024) SHAPES SDL        */~
            sh_model$3,                  /* Model Number               */~
            sh_lt$2,                     /* Liting Code                */~
            tt$(10%)25, ttt$(10%)8,      /* Screen Prompt Text         */~
            sh_qty$4,                    /* Window Quantity for Shapes */~
            sh$(10%)10, sh(10%),         /* Entered Data Conv to Decima*/~
            shc$(10%)10, shc(10%),       /* Calculated Sizes Dec to Fra*/~
            shd(10%),                    /* Shape Part No Sizes Decimal*/~
            sh_glass$2, sh_glass_d$30,   /* Glass Code and Sandwich    */~
            sh_config$2, sh_config_d$30, /* Configuration Code         */~
            sh_config_seq$2,             /* Config Code Sequence       */~
            sh_flags$(10%)3,             /* Prompt Data(YN) Type(WHRDN)*/~
            sh_fields$7, sh_bridge$7,    /*                            */~
            sh_entry$7,                  /* Entered Fields             */~
            sh_face$4,                   /* Glass Facing cod           */~
            sh_position$7,               /* Position of Value          */~
            shape_cross$2,               /* Cross Ref Code             */~
            sh_codes$8                   /* Part No Un Pack            */

        dim                              /*                            */~
            cust_rec$256,                /* Flat File                  */~
            ct_rec$256, ct_price$10,     /* Custom Record              */~
            ct_price1$7,                 /* Bridge File Price          */~
            sh_type$1,                   /* Grid Type                  */~
            contour$1,                   /* Contour Y or N             */~
            sh_grd_size$6,               /* Grid Size                  */~
            sh_thickness$6,              /* Glass Thickness            */~
            face_flag$1,                 /* Glass Facing               */~
            sh_width$10,                 /* Shape Width                */~
            sh_height$10,                /* Shape Height               */~
            sh_radius$10,                /* Shape Radius               */~
            sh_leg1$10,                  /* Shape Leg1                 */~
            sh_leg2$10,                  /* Shape Leg1                 */~
            sh_leg3$10,                  /* Shape Leg1                 */~
            sh_leg4$10,                  /* Shape Leg1                 */~
            sh_leg5$10,                  /* Shape Leg1                 */~
            sh_grid_color$6,             /* Shape Grid Color           */~
            radius_flag$10,              /* Shape Radius Flag          */~
            field_flag$1,                /* Field Flag for File        */~
            field_value$10,              /* Field Value for File       */~
            sz$100,                      /* Save Sixteenths            */~
            sze$30,                      /* Save Eights                */~
            text$(2%)70,txt$40,          /* Detail Text Id             */~
            txtid$4                      /* Text ID                    */


        dim f2%(64%),                    /* = 0 if the file is open    */~
            filename$8,                  /* File Name for Open         */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            axd$64,                      /* Open File                  */~
            rslt$(64%)20                 /* Text from file opening     */

        dim schema$8                     /* Schema                     */

        dim write_rec$(4)256             /* Record to Write            */

        dim ultra$1                      /* Ultra Flag (AWD026)        */

/* (AWD027) */
        dim wrteRec$(4)256,              /* RiteScreen File Record     */~
            phantom$4,                   /* Phantom Number             */~
            hdWidth$10,                  /* Screen Width               */~
            hdHeight$10,                 /* Screen Height              */~
            ph$(4%)4,                    /* Possible (4) Phantoms/Scr  */~
            seqCnt$10,                   /* Sequence counter           */~
            series$16,                   /* Door Order Series          */~
            style$10,                    /* Door Orderr Style          */~
            drDesc$50,                   /* Door Description           */~
            clr$2,                       /* Door Color                 */~
            clrDesc$6,                   /* Color Description          */~
            lot$10,                      /* Lot                        */~
            bin$10,                      /* Bin                        */~
            po$16,                       /* PoNumber                   */~
            barcode$18,                  /* Barcode Number             */~
            balerr$5,                    /* Balerr                     */~
            balance$8,                   /* Bottom Balance             */~
            total_weight$15              /* Total Sash Weight          */~

/* (AWD029) */
        dim intercept$2,                 /* Intercept                  */~
            sav_intercept$2,             /* Save Intercept for racks   */~
            interdesc$(9%)5,             /* Intercept Descriptions     */~
            model$3, ty$2,               /* Model and Glass Type       */~
            hg$2,                        /* (CR929) hinge              */~
            hnge$6, hg_l$8, hh$8,        /* HINGE CODE (CR2245)        */~
            sc$1,                        /* (CR2245) screen            */~
            spTypeDesc$(999%)30,         /* SP Type Descriptions       */~
            sav_rack$1                   /* Save Rack                  */

        dim logMessage$256

/* (AWD030) */

        dim awdschdt_key0$20,           /* AWDSCHDT Readkey 0          */~
            awdschdt_key1$28,           /* AWDSCHDT Readkey 1          */~
            awdschgl_key0$9             /* AWDSCHGL Readkey 0          */


        dim gen$8,                       /* AWDDEFIN Number definition */~
            def_desc$56,                 /* Seq Number                 */~
            warrantyid$8,                /* Warrantid sequence number  */~
            mfgid$10                     /* Mfg ID sequence number     */

        dim sc_order_num$5               /* Screen order number        */

        dim hldLoadKey$5,                /* HLDLOAD readkey            */~
            hldRec$256                   /* Hld Record                 */

        dim store_key$18,                /* SYSFILE readkey            */~
            nxtOrder$5                   /* Next Order Number          */

        dim spErr$3                      /* Err Code                   */
/* (AWD030) */
        dim dt_warr$(99%)8,       ~
            gl_warr$(500%)9

        dim gl_order$5,           ~
            gl_order_seq$5,       ~
            gl_warr$9,            ~
            gl_gls$2,             ~
            gl_sand$20,           ~
            gl_intercept$2,       ~
            gl_type$3,            ~
            gl_rack$3,            ~
            sav_rack_gl$3,        ~
            gl_sand1$30,          ~
            gl_sand2$30,          ~
            gl_sand3$30,     /*(AWD032)*/     ~
            gl_spc$10,            ~
            gl_spc_thk$10,        ~
            gl_part$25,           ~
            gl_wd1$10,            ~
            gl_ht1$10,            ~
            gl_bar$18,            ~
            gl_err$3,             ~
            gl_ord_flg$1,         ~
            gl_strength$1,        ~
            gl_poNumber$16,       ~
            gl_washer$10         /*(CR2056) Tempered Washer */

        dim spcr$(20%)4,          ~
            spcr_dec(20%),        ~
            cut_txt$55,           ~
            gls_txt$(5%)50,       ~
            cut_err$(10%)3,       ~
            cut_warr$(10%)10,     ~
            cut_wd1$(10%)10,      ~
            cut_ht1$(10%)10,      ~
            cut_wd1_dec(10%),     ~
            cut_ht1_dec(10%),     ~
            cut_wd1_dec$(10%)10,  ~
            cut_ht1_dec$(10%)10,  ~
            cut_ged$(10%)10,      ~
            cut_gls$(2%)2,        ~
            cut_sand$(2%)20,               /* (AWD032) */ ~
            cut_spc$(2%)10,       ~
            cut_spc_dec$(2%)10,   ~
            cut_sand1$(2%)30,     ~
            cut_sand2$(2%)30,     ~
            cut_sand3$(2%)30,             /* (AWD032) */  ~
            cut_ord_flg$(2%)1
/* (\AWD030) */
/* (AWD031) */
        dim poRec$(8)253,                     /* PO Record         */~
            poKey0$48,                        /* primary key       */~
            poKey1$10,                        /* Po Key1           */~
            poStatus$1,                       /* PO Status         */~
            poStatDate$6,                     /* PO Status date    */~
            poOrdType$1,                      /* PO Order Type     */~
            poOrderNum$5,                     /* PO Order Number   */~
            poOrderSeq$5,                     /* PO Order Seq      */~
            poRack$20,                        /* PO Rack           */~
            poWarrantyid$9,                   /* PO Warranty ID    */~
            poWarrantyseq$1,                  /* PO Warranty Seq   */~
            poCardinalType$20,                /* Cardinal Gls Type */~
            poOraclePart$40,                  /* Oracle Part Num   */~
            poSandwich$20,                    /* IG Sandwich Code  */~
            poSO$8,                           /* IG Sales Order    */~
            poLn$2,                           /* IG SO Line        */~
            poWidth$10,                       /* IG Width          */~
            poHeight$10,                      /* IG Height         */~
            poStrength$1,                     /* IG Strength       */~
            poNumber$16                       /* IG PO Number      */

/* (\AWD031) */
/* (AWD036) */
        dim warrTher$18,                 /*                            */~
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
            oradate$10                   /* OraDate                    */
/* (\AWD036) */


/* (IM8022) */
        dim                                                              ~
            hldrmkKey$52,                /* Remake Readkey             */~
            sp_num$3,                    /* Remake Number              */~
            gr_key$33,                   /* APCPLNGR readkey           */~
            gr_rec$(4%)128               /* APCPLNGR record            */

/* (CR893) */
        dim                                                              ~
            or_region$2,                 /* OR Region                  */~
            or_cust$9,                   /* OR Cust                    */~
            pcust$9,                     /* Parent Customer            */~
            pso$8,                       /* Parent SO                  */~
            pcutoff$2,                   /* Parent Cutoff              */~
            pregion$2                    /* Parent Region              */

/* (CR1114) */
        dim                                                               ~
            filterso$1,                  /* Filter sales order flag     */~
            mfg_plant$1,                 /* MFG Plant                   */~
            inv_plant$1,                 /* INV Plant                   */~
            so_1st$1,                    /* Identify cross dock type    */~
            hdpart$25,                   /* Part number to check length */~
            painted$30,                  /* Painted Colors              */~
            painted(30),                 /* Painted                     */~
            XDock$9                      /* InfoPart data for crossdock */

        dim scr_date$6,                  /* Screen Date                 */~
            skey$10,                     /* SCREEN lookup key           */~
            savSKey$10,                  /* Sav skey                    */~
            skey$(999%)10,               /* Phantom look up code        */~
            ss$(999%)20,                 /* Screen Phantom              */~
            scr_type$1,                  /* screen roll-form/extruded   */~
            scr_hf$1,                    /* screen half/full            */~
            wt$3,                        /* Window Type Code            */~
            wcode$1,                     /* Window Code (A) thru (E)    */~
            wcode1$1,                    /* Roll Form Code              */~
            sqty$2,                      /* Screen Quantity-No. Screens */~
            sqty1$2,                     /* Door Screen Qty             */~
            width$4,                     /* WIDTH                       */~
            height$3,                    /* HEIGHT                      */~
            clmr$3,                      /* CENTER LINE MEETING RAIL    */~
            wallw$3,                     /* WALL WIDTH                  */~
            schsrKey$18,                 /* AWDSCHSR Key                */~
            scr_time$6,                  /* Screen Time                 */~
            scr_cnt$10                   /* Screen Counter              */





        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Special Analysis of Glass & Liting "
            pname$ = "EWDPLN58 - Rev: R7.00"

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
            * #1  ! EWDSCHED ! Special Temp Glass and Liting            *~
            * #2  ! AMTBOMCD ! Master Equation File                     *~
            * #3  ! HLDSCRMK ! Special Temp Glass Remake        (IM8022)*~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! APCPLNOR ! Planning Header Histroy                  *~
            * #6  ! APCPLNSC ! Planning Master Schedule File            *~
            * #7  ! CUSTOMER ! Customer Master File                     *~
            * #8  ! TXTFILE  ! Sales Master Text File                   *~
            * #9  ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #10 ! APCPLNGR ! (New) Glass Remake File         (IM8022) *~
            * #11 ! BCKMASTR !                                          *~
            * #12 ! BCKLINES ! S.O. Detail                     (EWD003) *~
            * #15 ! EWDPLNRK ! Master Glass Rack File          (EWD005) *~
            * #18 ! AWDSTEMP ! Tempered Sort File              (EWD015) *~
            * #19 ! AWDTEMP  ! Tempered FTP File               (EWD015) *~
            * #20 ! AWDSRPT  ! Sort File for Labels and Report (EWD015) *~
            * #21 ! AWDTEMP1 ! Mod for Cardinal Glass          (AWD019) *~
            * #25 ! AMTBOMIF ! Field Definitions for Item Number        *~
            * #26 ! HLDSCHED ! Hold Schedule file              (AWD027) *~
            * #28 ! AWDSCHDT ! Detail by unitid and barcode    (AWD030) *~
            * #29 ! AWDDEFIN ! Number Starting Number definions(AWD030) *~
            * #30 ! HLDLOAD  ! Next Order (load) number        (AWD030) *~
            * #31 ! SYSFILE2 ! System File                              *~
            * #32 ! AWDSCHGL ! Schedule Glass File                      *~
            * #33 ! AWDPLNTP ! Tempered Sort File                       *~
            * #34 ! ORACLEPO ! Oracle PO data transfer file    (AWD031) *~
            * #35 ! WARRTHER ! Thermal Warranty Numbers        (AWD036) *~
            * #36 ! AWDBARHD ! File to update warranty         (AWD036) *~
            * #37 ! AWDBAL   ! Balance Data File               (AWD039) *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "EWDSCHED",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,   keylen = 38,                       ~
                        alt key 1, keypos =  16, keylen = 23

            select #2,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42


            select #3,  "HLDSCRMK",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,   keylen = 52,                       ~
                        alt key 1, keypos =  16, keylen = 37,            ~
                            key 2, keypos =  27, keylen = 28

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #5,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #6,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #7,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup

            select #8,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #9, "AMTBOMIF",                                       ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            select #10, "APCPLNGR",                                      ~
/*IM8022*/              varc,     indexed,  recsize = 512,               ~
                        keypos = 22,   keylen = 12,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33,             ~
                            key 3, keypos = 13, keylen = 21

            select #11, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #12, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19
                                                   /* (EWD005) - Begin */
            select #15, "EWDPLNRK",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14
                                                   /* (EWD005) End     */
                                                   /* (EWD015) - Begin */
            select #18, "AWDPLNWK",                                       ~
/*AWD019*/              varc,     indexed, recsize = 256,                 ~
                        keypos = 1,    keylen = 91

            select #19, "AWDTEMP",                                        ~
                        varc,     indexed, recsize = 128,                 ~
                        keypos = 1,    keylen = 42

/* (AWD019) */
            select #21, "AWDTEMP1",                                       ~
                        consec, recsize = 192  /* (CR2056) 128 - 192 */

/* (AWD019) */

            select #20, "AWDPLNW1",                                       ~
                        varc,     indexed, recsize = 280,                 ~
                        keypos = 1,    keylen = 85

                                                   /*  (EWD015) - END  */

/* (AWD022) */
            select #22, "AWDTHERMA",                                      ~
                                consec , recsize = 1024


            select #23, "AWDDALLAS",                                      ~
                                consec , recsize = 1024

/* (AWD024) */
            select #24, "@SHAPE8@", consec, recsize = 256

            select #25, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize = 120,               ~
                        keypos = 1,    keylen = 32
/* (AWD027) */
            select #26, "HLDSCHED",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  40,                     ~
                        alt key  1, keypos =   16, keylen =  25,         ~
                            key  2, keypos =   27, keylen =  16

            select #27, "AWDRTESC",                                      ~
                                consec , recsize = 1024


            select #28, "AWDSCHDT",                                      ~
                        varc,     indexed,  recsize = 1012,              ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   29, keylen =  28,         ~
                            key  2, keypos =   39, keylen =  18,         ~
                            key  3, keypos =   21, keylen =   8

            select #29, "AWDDEFIN",                                      ~
                        varc,   indexed,       recsize =  64,            ~
                        keypos =  1,   keylen  =   8

            select #30, "HLDLOAD",                                       ~
                        varc,   indexed,       recsize = 256,            ~
                        keypos =  1,   keylen  =   5

            select #31, "SYSFILE2"                                        ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 20


            select #32, "AWDSCHGL",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =   11, keylen =   9,                     ~
                        alt key  1, keypos =    1, keylen =  19


            select #33, "AWDPLNTP",                                       ~
                        varc,     indexed, recsize = 384,                 ~
                        keypos = 1,    keylen = 91

            select #34, "ORACLEPO",                                       ~
                        varc,     indexed, recsize = 1024,                ~
                        keypos = 1,    keylen = 48,                       ~
                        alt key  1, keypos =   39, keylen =  10

/* (AWD036) */
            select #35, "WARRTHER",                                       ~
                        varc,     indexed, recsize = 256,                 ~
                        keypos = 1,    keylen = 30,                       ~
                        alt key  1, keypos =    4, keylen =  18,          ~
                            key  2, keypos =   22, keylen =   9

            select #36, "AWDBARHD"                                       ~
                        varc,     indexed, recsize = 124,                ~
                        keypos =    1, keylen = 18,                      ~
                        alt key  1, keypos =  19, keylen = 8

            select #37, "AWDBAL",                                        ~
                                consec , recsize = 1024

            select #38, "BALDATA",                                       ~
                        varc,     indexed, recsize = 124,                ~
                        keypos =    1, keylen = 18

/*(CR2245) */
            select #39, "AWDSCHSR",              /*FLEX SCREEN DATA */   ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    10, keylen =  18,                    ~
                        alt key  1, keypos =    1, keylen =  27

            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup


             call "SHOSTAT" ("Opening Files, One Moment Please")


                                                   /* (EWD009) - Begin */
            filename$ = "AMTBOMCD" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HLDSCRMK" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE"  : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPLNRK" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
                                                          /* (EWD009) End*/
/* PAR000 */

            filename$ = "AMTBOMIF" : call "EWDOPEN" (#25, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HLDSCHED" : call "EWDOPEN" (#26, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "AWDDEFIN" : call "EWDOPEN" (#29, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SYSFILE2" : call "EWDOPEN" (#31, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error


                                                            /* (EWD015)  */

REM         call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),  100%, rslt$(1%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%), 100%, rslt$(28%))
            call "OPENCHCK" (#30, fs%(30%), f2%(30%), 100%, rslt$(30%))
            call "OPENCHCK" (#32, fs%(32%), f2%(32%), 100%, rslt$(32%))

REM Used for debug
REM            call "OPENCHCK" (#33, fs%(33%), f2%(33%), 100%, rslt$(33%))
/* (AWD031) */
            call "OPENCHCK" (#34, fs%(34%), f2%(34%), 100%, rslt$(34%))
            call "OPENCHCK" (#35, fs%(35%), f2%(35%), 100%, rslt$(35%))
            call "OPENCHCK" (#36, fs%(36%), f2%(36%), 100%, rslt$(36%))
            call "OPENCHCK" (#38, fs%(38%), f2%(38%), 100%, rslt$(38%))
            call "OPENCHCK" (#39, fs%(39%), f2%(39%), 100%, rslt$(39%))

            mat f1% = zer

            wrk%, wrk1% = 0%                                /* (EWD015) */
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            oradate$ = date
            call "DATEFMT" (date$)
            call "DATFMTC" (oradate$)
            scr_date$ = date                              /* (CR2245) */
            ret% = 0%
            call "COMPNAME" (12%, company$, ret%)         /* (EWD011) */
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            init(" ") txt$(), tab$(), ff$, volume$, library$
            tab$(1%) = "PLAN SCH1"
            tab$(2%) = "PLAN SCH2"
            tab$(3%) = "COLOR    "
            tab$(4%) = "GLASS    "
            tab$(5%) = "LITING   "
            tab$(6%) = "PLAN CUTO"                           /* (EWD013) */
            tab$(7%) = "PLAN SAND"                           /* (EWD015) */
            tab$(8%) = "TEMP GED "                           /* (EWD015) */
            tab$(10%)= "PLAN HOWS"                           /* (EWD017) */
            tab$(11%)= "PLANCSAND"                           /* (AWD019) */
            tab$(12%)= "GRDCOLOR "                           /* (AWD023) */
            tab$(13%)= "SCREEN04 "                           /* (AWD027) */
            tab$(14%)= "RITESCRN "                           /* (AWD027) */
            tab$(15%)= "GED 001  "
            tab$(16%)= "ORACLPART"                            /* (AWD031)*/
            tab$(17%)= "PLAN CONT"
            tab$(18%)= "GEDTOPBOT"
            tab$(19%)= "GED 001"
            tab$(20%)= "PLANBILCO"                           /* (AWD032) */
            tab$(21%)= "VALANCE  "                           /* (AWD033) */
            tab$(22%)= "GLASS05  "                           /* (AWD038) */
            tab$(23%)= "GLASS07  "                           /* (CR929)  */
            tab$(24%)= "SCREEN01 "                           /* (CR2245) */
            tab$(25%)= "SCREEN06 "                           /* (CR2245) */
            tab$(26%)= "SCREEN08 "                           /* (CR2245) */
            tab$(27%)= "SCRNPHANT"                           /* (CR2245) */
            tab$(28%)= "EXTRUDED "                           /* (CR2245) */
            tab$(29%)= "HINGE    "                           /* (CR2245) */
            tab$(30%)= "PLAN STC "                           /* CR2747   */

            ff$      = "MMDD@DPT"
            volume$  = "CARLOS"
            library$ = "APCDATA "

            txt$(1%) = "  Placing Specials On-Order   "
            txt$(2%) = "  Receive Specials On-Order   "
            txt$(3%) = "Releasing Specials to Planning"
            txt$(4%) = "Deleting Selected Special Ords"
            txt$(5%) = "                              "
            txt$(6%) = "Move Orders Back to Sel 0     "
            txt$(7%) = "Move Orders Back to Sel 2     "

            cut_off% = 1%                                    /* (EWD010) */
            tp_comma$ = ","                                  /* (EWD015) */

* (AWD020) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)

/*(AWD027) */
            ph$(1%) = "0000"                                 /* (CR2245) */
            ph$(2%) = "0000"                                 /* (CR2245) */
            ph$(3%) = "0000"
            ph$(4%) = "0000"

/* (CR1114)  */
            init(" ") painted$
            painted$ = "5,I,J,K,L,M,N,O,P"

/* (AWD029) */
            gosub load_interdesc
            init(" ") spTypeDesc$()

/* (AWD030) */
            init(" ") spcr$()
            mat spcr_dec = zer
            gosub load_ged000

REM                                   10        20        30
REM                           123456789012345678901234567890
            spTypeDesc$(1) = "single strength"
            spTypeDesc$(2) = "double strength"
            spTypeDesc$(3) = "tempered double strength"
            spTypeDesc$(4) = "SDL Single strength"
            spTypeDesc$(5) = "SDL Double strength"
            spTypeDesc$(6) = "SDL Temp Double strength"
            spTypeDesc$(7) = "Triple Strength>25SqFt 3/16"
            spTypeDesc$(8) = "Door 5/32 Triple Strength"
            spTypeDesc$(9) = "Laminate Glass"
            spTypeDesc$(10) = "Laminate Strengthened"
            spTypeDesc$(11) = "Single Glazed"
            spTypeDesc$(12) = "Laminate 5/32"
            spTypeDesc$(13) = "Laminate Strengthened 5/32"
            spTypeDesc$(14) = "Laminate 3/16"
            spTypeDesc$(15) = "Laminate Strengthened 3/16"
            spTypeDesc$(997) = "NO SANDWICH IN PLANCSAND "
            spTypeDesc$(998) = "SPECIAL LITE GRID"
            spTypeDesc$(999) = "NOT TEMPERED/ERRORS"

        REM - NEAREST 8TH INCH
           sze$ = "1/81/43/81/25/83/47/8         "
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

            for fieldnr% = 1% to 10%
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
            plng_err% = 0%
L11120:     gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  9% then gosub processDataHld
                  if keyhit%  = 10%  and sc_status$ = "0" then gosub Plng_Ind
                    if plng_err% = 1%  then editpg1
                  if keyhit%  = 10% and sc_status$ = "0" then gosub Plng_Set
                    if plng_err% = 1%  then editpg1
                  if keyhit%  = 10% then gosub processDataHld
                  if keyhit%  = 14% then gosub processDataHld
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
REM L11120:
            fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 10% then editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

                                                 /* Analyize (HLDSCHED)*/
        processDataHld                           /* Production Data    */
            if rmk% = 1% then goto processDataRmk    /* (IM8022) */
            numOfErr% = 0%
            report% = 1%                         /* No Screeen Display */
            validOrder% = 1%
            paint% = 0%
            init(" ") hdKey$, hdKey1$,  dt$(), po$(), savHd$, unitid$(),~
                      subpart$(), sp_warr$(), sp_num$(), sp_due$
            hdKey$, hdKey1$ = all(hex(00))            /* (SR74117) */
            mat dim1es = zer
            mat dim2es = zer
            cnt% = 0% : dt% = 0% : dt_max% = 0%

/* (SR70932) */
            gosub set_key
REM STR(HDKEY$,1%,6%) = BEG_DTE$
REM STR(HDKEY$,7%,1%) = SC_STATUS$
REM STR(HDKEY$,8%,1%) = SC_TYPE$

            call "SHOSTAT" ("Searching (HLDSCHED) Data")
        processDataHldNxt
            gosub readHldData             /* (SR70932) */
            if hlddata% = 0% then goto processHldDone

/* (AWD037) */
            if sc_status$ = "3" then gosub readOrderNum
            if sc_status$ = "3" and validOrder% = 0% then goto processDataHldNxt
/* (\AWD037) */

/* (SR70932) */
REM IF SAVHD$ = STR(HDKEY$,1%,36%) THEN GOTO PROCESSDATAHLDNXT
REM SAVHD$ = STR(HDKEY$,1%,36%)    /* ONLY LOAD LINE ITEM */
            if savhd$ = str(hdRec$,1%,36%) then goto processdatahldnxt
                 savhd$ = str(hdRec$,1%,36%)
/*(CR893)*/
            pcust$ = str(hdRec$,18%,9%)
            pso$   = str(hdRec$,27%,9%)
            paint% = 0%
            if str(pcust$,1%,6%) = "CV0999" then paint% = 1%
            if paint% = 1% then gosub paintCust

            if str(beg_cut$,1%,2%) = "AL" then goto noHdCutOff
               gosub check_cutoff
               if cutoff% = 0% then goto processDataHldNxt
            noHdCutOff
/* (SR70932) - changed from hdKey to hdRec */
            sav_ord$ = str(hdRec$,27%,8%)
            gosub check_apcplnor
            if str(beg_how$,1%,2%) = "AL" then goto noHldHowShip
               if str(or_hows$,1%,2%) < str(beg_how$,1%,2%) or  ~
                  str(or_hows$,1%,2%) > str(end_how$,1%,2%)     ~
                  then goto processDataHldNxt
            noHldHowShip

               gosub load_screen_dt
               goto processDataHldNxt

        processHldDone
            dt_max% = dt%
            if keyhit% =  9% then gosub gen_labels
            if keyhit% = 10% then gosub display_analysis
            if keyhit% = 14% then gosub gen_rpt1
        return clear all
        goto inputmode

/* (SR70932) */
        set_key
          hdKey$ = all(hex(00))                    /* (SR74117) */
          if orderDate% = 1% then goto set_key1
          str(hdKey$,1%,6%) = beg_dte$
          str(hdKey$,7%,1%) = sc_status$
          str(hdKey$,8%,1%) = sc_type$
        return
        set_key1
           hdKey1$ = all(hex(00))
           str(hdKey1$,1%,1%) = sc_status$
           str(hdKey1$,2%,1%) = sc_type$
        return

        readHldData
           hlddata% = 0%
           if orderDate% =  0% then gosub readHldData0
           if orderDate% <> 0% then gosub readHldData1
        return

        readHldData0           /* (SR74117) had hdKey1 instead of hdKey */
           read #26, key > hdKey$, using L19500, hdRec$,                ~
                                              eod goto readHldData0Done
L19500:       FMT CH(256)

           str(hdKey$,1%,40%) = str(hdRec$,1%,40%)

REM if str(hdKey$,27%,08%) <> "08678877" then goto NoStop
REM call "SHOSTAT" ("Read Rec SO -->  " & str(hdKey$,27%,08%) ) stop
REM IF STR(HDKEY$,1%,6%) > END_DTE$ THEN GOTO PROCESSHLDDONE

REM NoStop
           if str(hdKey$,1%,6%) > end_dte$ then goto readHldData0Done
           if str(hdKey$,7%,1%) <> sc_status$ then goto readHldData0
           if str(hdKey$,8%,1%) <> sc_type$ then goto readHldData0
              hlddata% = 1%
        readHldData0Done
        return

        readHldData1
           read #26, key 1% > hdKey1$, using L19500, hdRec$,             ~
                                              eod goto readHldData1Done

           str(hdKey1$,1%,25%) = str(hdRec$,16%,25%)
           sp_due$ = str(hdRec$,43%,6%)
REM IF STR(HDKEY1$,1%,1%) <> SC_STATUS$ THEN GOTO PROCESSHLDDONE
           if str(hdKey1$,1%,1%) <> sc_status$ then goto readHldData1Done
           if str(hdKey1$,2%,1%) <> sc_type$ then goto readHldData1
           if sp_due$ < beg_dte$ or sp_due$ > end_dte$ then              ~
                                                   goto readHldData1
              hlddata% = 1%
        readHldData1Done
        return

/* (CR1114) + Filter Check on tempered glass for NC; checking painted */
        CheckFilter
            filterso$ = "N"
            init(" ") XDock$, mfg_plant$, inv_plant$, so_1st$, color$, hdpart$
            painted% = 0%
            mat painted = zer

            XDock$  = str(bcksubpt_rec$,132%,9%)
            mfg_plant$ = str(XDock$,7%,1%)    /* looking for NC */
            inv_plant$ = str(XDock$,5%,1%)    /* looking for TX */
            so_1st$ = str(hdRec$,27%,1%)      /* sales order first char */
            hdpart$ = str(hdRec$,49%,25%)     /* part number read  */

            if len(hdpart$) < 19 then filterso$ = "Y"   /* no parts needed */
                if filterso$ = "Y" then return
           if str(hdRec$,49%,1%) = "9" and so_1st$ <> "C" then filterso$ = "Y"
                if filterso$ = "Y" then return
            if str(hdRec$,18%,9%) = "BA111" and so_1st$ <> "C" then filterso$="Y"
                if filterso$ = "Y" then return

            color$  = str(hdRec$,52%,1%)      /* color field for paint check */
            search painted$ = color$ to painted()
            painted% = int(painted())

            if painted% > 0% and so_1st$ <> "D"  then filterso$ = "Y"
                if filterso$ = "Y" then return
            if painted% = 0% and so_1st$ <> "A" and    ~
               (mfg_plant$ = "1" and inv_plant$ = "4")  ~
                then filterso$ = "Y"
            if painted% = 0% and so_1st$ <> "B" and    ~
               (mfg_plant$ = "4" and inv_plant$ = "1")  ~
                then filterso$ = "Y"                         /* CR1720 */

        return
/* (CR1114)  - */

/* (SR70932\) */
        load_screen_dt
/*(AWD027) */
/*(AWD030) */
            if sc_status$ = "3" then goto skipDataload
/*(AWD035)*/
            gosub dataloadHld

/* CR1114 filter NC tempered glass selection sales orders */
            filterso$ = "N"
/* CR1720 */
            if sc_type$ = "1" and sc_process$ = "0"  ~
                 then  gosub CheckFilter

                 
REM  IF FILTERSO$ = "Y" THEN GOTO PROCESSDATAHLDNXT      /* (CR2237) */
            if filterso$ = "Y" then return               /* (CR2237) */

REM  SAV_ORD$ = SP_SO$
REM  GOSUB CHECK_APCPLNOR
skipDataload:
                      /* cutoff Day (CR893) dont override special logic  */
            if paint% = 1% then sp_cutoff$ = pcutoff$

            if or_st$ > "01" and or_st$ < "90" then sp_cutoff$ = "**"
            if or_st$ = "99" then sp_cutoff$ = "HH"  /* Credit Hold    */
            if or_hows$ = "20" then sp_cutoff$ = "RR" /* Back Order Rep*/
            if or_hows$ = "90" then sp_cutoff$ = "RR" /*        Repair */
                                                      /* (EWD009) Beg  */
            if or_hows$ = "26" then sp_cutoff$ = "BO" /* Back Order Rep*/
            if or_hows$ = "27" then sp_cutoff$ = "BO" /* Back Order Rep*/
            if or_hows$ = "29" then sp_cutoff$ = "BO" /* Back Order Rep*/
            if or_hows$ = "25" then sp_cutoff$ = "BO" /* Back Order Rep*/

            if or_hows$ = "22" then sp_cutoff$ = "SR" /* Sample Repair */

                                                      /* (EWD009) End  */

            if or_hows$ = "23" then sp_cutoff$ = "FX" /* (EWD012)      */

                                                      /* (EWD018)      */
            if or_hows$ = "21" then sp_cutoff$ = "RO" /* Rush Order Rep*/
            if or_hows$ = "52" then sp_cutoff$ = "LO" /* Rush Order Rep*/
            if str(sp_cust$,1%,6%) = "SA0999" then sp_cutoff$ = "SA"
                                                      /* (EWD012)      */

            dt% = dt% + 1%
            if dt% > 3000 then dt% = 3000%              /* Analysis Date  */
                                                       /*(CR893)*/
            str(dt$(dt%),1%,4%)  = str(sp_status_date$,1%,2%) &     ~
                                   str(sp_status_date$,4%,2%)
            str(dt$(dt%),6%,2%) = sp_cutoff$         /* cutoff Day     */
            str(dt$(dt%),9%,2%)  = or_region$        /* region         */
             if paint% = 1% then str(dt$(dt%),9%,2%) = pregion$
            str(dt$(dt%),12%,5%) = sp_route$         /* Route code     */
            str(dt$(dt%),18%,6%) = sp_cust$          /* Customer Code  */
            str(dt$(dt%),25%,8%) = sp_so$            /* cust Sales Ord */
            str(dt$(dt%),34%,2%) = sp_ln$            /* Line Item      */
            str(dt$(dt%),37%,8%) = sp_due_date$      /* Due Date       */
            str(dt$(dt%),46%,25%)= sp_part$          /* part No        */
            str(dt$(dt%),72%,4%) = sp_qty$           /* Quantity       */
/*(AWD037) show L for Laminate or S for SGP beside of Qty*/
            if sc_type$ = "B" then str(dt$(dt%),77%,1%) = sp_lam$
            if sc_type$ = "D" then str(dt$(dt%),77%,1%) = sp_lam$
            str(dt$(dt%),78%,4%) = sp_ln_item$       /* Line Item       */
/* (AWD027) save po Number$ */
            po$(dt%) = or_po$
            unitid$(dt%) = sp_unitid$                /*WW Unitid (AWD030)*/
            subpart$(dt%) = subpart$                 /*Subpart (AWD031)  */
                                                     /*(AWD030)          */
            dim1es(dt%) = dim1es
            dim2es(dt%) = dim2es
/* (IM8022)  save warrantyid and remake num */
            if rmk% <> 1% then goto screenDT_RMK
              sp_warr$(dt%)    = sp_warr$
              sp_num$(dt%)     = sp_num$

screenDT_RMK:

            if sp_status$ <> "2" then return         /* Only Tempered */
            if sp_type$ <> "1" and sp_type$ <> "B" then return

            if spErr$ = "000" then return
            if spErr$ <= "   " then return
            if spErr$ = hex(000000) then return
REM Else display error code on the screen
REM  STR(DT$(DT%),10%,8%) = "ERR " & SPERR$
              str(dt$(dt%),6%,8%) = "ERR " & spErr$       /* (CR893) */
REM For Now Do not count 998 Grid As numOfErr
                if spErr$ <> "998" then numOfErr% = numOfErr% + 1%
        return


        load_screen_gl
            convert str(gl_warr$,9,1) to gl%, data goto noWarr
            gl% = gl% + 1%

            cut_err$(gl%)  = gl_err$
            cut_warr$(gl%) = gl_warr$

            convert gl_wd1_dec to cut_wd1_dec$(gl%), pic(#####.####)
            convert gl_ht1_dec to cut_ht1_dec$(gl%), pic(#####.####)

            if gl% <> 1% and gl% <> 6% then return
              cut% = 1%
              if gl% = 6% then cut% = 2%
              cut_ord_flg$(cut%) = gl_ord_flg$
              cut_gls$(cut%)     = gl_gls$
              cut_sand$(cut%)    = gl_sand$
              cut_spc$(cut%)     = gl_spc$
              cut_spc_dec$(cut%) = gl_spc_thk$
              if str(cut_sand$(cut%),1%,3%) = "ERR" then  ~
                  str(cut_sand$(cut%),4%,7%) = " "
              if str(cut_spc$(cut%),1%,3%) = "ERR" then  ~
                  str(cut_spc$(cut%),4%,7%) = " "

              cut_sand1$(cut%) = gl_sand1$
              cut_sand2$(cut%) = gl_sand2$
              cut_sand3$(cut%) = gl_sand3$   /* (AWD032) */

noWarr:
        return


        readOrderNum                                 /* #32 AWDSCHGL */
         validOrder% = 1%
         gosub dataloadHld

         if str(sc_order_num$,1%,3%) = "ALL" then return
         init(" ") awdschgl_key0$
         awdschgl_key0$ = warrantyid$

readOrderNumNext:
          read #32, key > awdschgl_key0$, using AWDSCHGL_FMT2,           ~
                               awdschgl_key0$, eod goto readOrderNumDone

              if str(awdschgl_key0$,1%,8%) <> warrantyid$ then         ~
                                                goto readOrderNumDone
              gosub dataloadGl

              if gl_order$ = sc_order_num$ then return
              goto readOrderNumNext
        readOrderNumDone
          validOrder% = 0%
        return

/* (IM8022) */
        processDataRmk
          init(" ") hldrmkKey$, hldRec$, dt$(), po$(), savHd$, unitid$(), ~
                    subpart$()
          mat dim1es = zer
          mat dim2es = zer
          cnt% = 0% : dt% = 0% : dt_max% = 0%

          str(hldrmkKey$,1%,6%) = beg_dte$
          str(hldrmkKey$,7%,1%) = sc_status$
          str(hldrmkKey$,8%,1%) = sc_type$

          call "SHOSTAT" ("Searching (HLDSCRMK) Data")
        processDataRmkNxt
          read #3,key > hldrmkKey$, using HLDSCRMK, hldrmkKey$,        ~
                                               eod goto processRmkDone
HLDSCRMK:        FMT CH(52)

          if str(hldrmkKey$,1%,6%) > end_dte$ then goto processRmkDone

          if str(hldrmkKey$,7%,1%) <> sc_status$ then goto processDataRmkNxt
          if str(hldrmkKey$,8%,1%) <> sc_type$ then goto processDataRmkNxt

          if str(beg_cut$,1%,2%) = "AL" then goto noRmkCutOff
            gosub check_cutoff
            if cutoff% = 0% then goto processDataRmkNxt
        noRmkCutOff

          sav_ord$ = str(hldrmkKey$,27%,8%)
          gosub check_apcplnor
          if str(beg_how$,1%,2%) = "AL" then goto noRmkHowShip
            if str(or_hows$,1%,2%) < str(beg_how$,1%,2%) or  ~
               str(or_hows$,1%,2%) > str(end_how$,1%,2%)     ~
                 then goto processDataRmkNxt
        noRmkHowShip

          gosub load_screen_dt
        goto processDataRmkNxt

        processRmkDone
          dt_max% = dt%
          if keyhit% =  9% then gosub gen_labels
          if keyhit% = 10% then gosub display_analysis
          if keyhit% = 14% then gosub gen_rpt1
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
         "Enter a Valid Analysis Process Code from (PLAN SCH1)?        ",~
         "Enter a Valid Analysis Type of Process Code from (PLAN SCH2)?",~
         "Enter a Update Code '0'= Selected, '1'= All Except Selected? ",~
         "Enter a Valid Beginning Date?                                ",~
         "Enter a Valid Ending Date?                                   ",~
         "Enter a Valid Beginning Customer Cut-Off?                    ",~
/*EWD013*/"Enter a Valid Ending Customer Cut-Off?                       ",~
         "Enter a Valid Beginning Customer How Ship or AL?             ",~
/*EWD017*/"Enter a Valid Ending Customer How Ship or AL?                ",~
         "Enter a Valid Order Number or ALL                            "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_date$, beg_dte$,       ~
                      end_date$, end_dte$, x$, sc_status$, sc_status_d$, ~
                      sc_type$, sc_type_d$, sc_process$, sc_process_d$,  ~
                      cc$(), dt$(), beg_cut$, end_cut$, beg_cut_d$,      ~
                      end_cut_d$, beg_how$, beg_how_d$, end_how$,        ~
                      end_how_d$, hdFil$, sd$(), sc_order_num$, sp_lam$, ~
                      sp_lamFl$, field$, error$, logmessage$, pso$,      ~
                      pcutoff$, pregion$, pcust$, or_cust$,              ~
                      or_region$  /*(CR893)*/
                                                          /* (EWD017) */
            rpt% = 0% : sp_flag% = 0%

            init (hex(ff)) textid$
            call "TXTFUTIL" (#8, f2%(8%), "INTL", textid$)
            sd1$="Beg Date-Special Orders"
            sd2$="End Date-Special Orders"
            report% = 0%
            labels% = 0%
            gls_dtl% = 0%
            init(" ") sp_rack$, ultra$

REM 0% = Order Date 1% = DueDate
            OrderDate% = 0%                       /* (SR70932) */
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

/*(AWD027)*/
        dataloadHld                             /* #26 HLDSCHED */

          if rmk% = 0% then gosub loadData
          if rmk% = 1% then gosub loadRmk

/*(AWD037)*/if sp_lam$ = "S" then sp_lamFl$ = "L"

          if str(sp_primary$,1%,3%) = "   " then sp_primary$ = "099"
          sp_status_date$ = sp_status_dte$
          call "DATEFMT" (sp_status_date$)

          sp_due_date$ = sp_due$
          call "DATEFMT" (sp_due_date$)

          sp_date$ = sp_dte$
          call "DATEFMT" (sp_date$)

          init(" ") so_inv$, item_no$
          so_inv$  = sp_so$                        /* Set SO         */
          item_no$ = sp_ln$                        /* Set Line       */

          gosub lookup_sub_part                    /* Lookup Sub Part*/
          field1$ = str(flds$(12%),1%,1%)          /* Grdtype        */
          field2$ = str(flds$(13%),1%,1%)          /* GrdSize        */
          field3$ = str(flds$(14%),1%,1%)          /* GrdColor       */
          field4$ = str(flds$(15%),1%,1%)          /* Hardware       */
          field5$ = str(flds$(16%),1%,1%)          /* Foam           */
          field6$ = str(flds$(17%),1%,1%)          /* Casing         */
          field7$ = str(flds$(18%),1%,1%)          /* Sample Color   */
          field8$ = str(flds$(19%),1%,1%)          /* Ext Grid Type  */
          field9$ = str(flds$(20%),1%,1%)          /* Ext Grid size  */
          field10$ = str(flds$(21%),1%,1%)         /* Int Grid Color */
          field11$ = str(flds$(22%),1%,1%)         /* Ext Grid Color */
          field17$ = str(flds$(28%),1%,1%)         /* Spacer Type    */

          sp_qty% = 0%
          convert sp_qty$ to sp_qty%, data goto bad_spQtyHld

bad_spQtyHld:

/*(CR2245)*/
         barcode$ = sp_so$ & sp_ln$ & sp_ln_item$ & sp_qty$
/* (AWD037) */
REM Dont load glass warranty info for type 9 Screen or C balances
          if sp_type$ = "9" then return
          if sp_type$ = "C" then return             /* (AWD039) */
          if sp_type$ = "E" then return             /* (CR2245) */
/*(CR2245)*/
REM BARCODE$ = SP_SO$ & SP_LN$ & SP_LN_ITEM$ & SP_QTY$
            if sp_status$ <> "0" then gosub dataloadWarranty

        return
/*(AWD027\)*/
/* (AWD30) */
        dataloadWarranty                       /* #28 AWDSCHDT */
        if sp_type$ <> "1" and sp_type$ <> "B" then return /* Only Tempered */
        init(" ") warrantyid$

          read #28,key 2% = barcode$, using AWDSCHDT_FMT4, warrantyid$, ~
                          spErr$, wd$, ht$, eod goto noDataloadWarranty
AWDSCHDT_FMT4: FMT POS(21), CH(8), POS(57), CH(03), POS(60), CH(07), CH(06)

        noDataloadWarranty
        return

        dataloadGl                            /* AWDSCHGL */
           init(" ")csandwich$()
           get #32, using AWDSCHGL_FMT1, gl_order$,                       ~
                                         gl_order_seq$,                   ~
                                         gl_warr$,                        ~
                                         gl_sand$,                        ~
                                         gl_intercept$,                   ~
                                         gl_type$,                        ~
                                         gl_rack$,                        ~
                                         gl_sand1$, /*CGLSTYPE 1/8 CLEAR*/~
                                         gl_sand2$, /*CGLSTYPE 1/8 CLEAR*/~
                                         gl_spc$,                         ~
                                         gl_spc_thk$,                     ~
                                         gl_part$,                        ~
                                         gl_wd1$,                         ~
                                         gl_ht1$,                         ~
                                         gl_wd1_dec,                      ~
                                         gl_ht1_dec,                      ~
                                         gl_bar$,                         ~
                                         gl_err$,                         ~
                                         gl_ord_flg$,                     ~
                                         gl_gls$,                         ~
                                         gl_strength$,                    ~
                                         gl_sand3$  /* (AWD032) */

           if gl_ord_flg$ <= " " then gl_ord_flg$ = "1"
           if gl_gls$ <= " " then gl_gls$ = str(sp_part$,5%,2%)

           csandwich$(1%) = gl_sand1$
           csandwich$(2%) = gl_sand2$
           csandwich$(3%) = gl_sand3$            /*(AWD032) */

           if rmk% = 0% then return             /* (IM8022) */

           init(" ") gl_order$, gl_order_seq$
        return

        loadData
          init(" ") sp_status_date$, sp_due_date$, sp_date$, sp_lamFl$

          get #26, using L35500, sp_status_dte$,  /* Spec Anal St     */~
                                 sp_status$,      /* (PLAN SCH1)      */~
                                 sp_type$,        /* (PLAN SCH2)      */~
                                 sp_cutoff$,      /* Cut Off Day 1-7  */~
                                 sp_route$,       /* Route code       */~
                                 sp_status$,      /* (PLAN SCH1)      */~
                                 sp_type$,        /* (PLAN SCH2)      */~
                                 sp_cust$,        /* Customer Code    */~
                                 sp_so$,          /* Sales order No.  */~
                                 sp_ln$,          /* S.O. Line item No*/~
                                 sp_ln_item$,     /* Line Item Piece  */~
                                 sp_status$,      /* (PLAN SCH1)      */~
                                 sp_type$,        /* (PLAN SCH2)      */~
                                 sp_due$,         /* Sales order Due  */~
                                 sp_part$,        /* Part Number      */~
                                 sp_qty$,         /* line Item Qty    */~
                                 sp_stat$,        /* (PLAN STAT) Plann*/~
                                 sp_usr$,         /* Last Mod User Id */~
                                 sp_dte$,         /* Last Mod Date    */~
                                 sp_time$,        /* Last Mod Time    */~
                                 sp_text$,        /* Line Item Text Id*/~
                                 sp_primary$,     /* Primary DepEWD015*/~
                                 sp_unitid$,      /* Unitid           */~
                                 sp_lam$,         /* LaminType(AWD037)*/~
                                 hdFil$           /* Filler Area      */
        return

        loadRmk
          init(" ") sp_status_date$, sp_due_date$, sp_date$, sp_lamFl$

          get #3,  using L35600, sp_status_dte$,  /* Spec Anal St     */~
                                 sp_status$,      /* (PLAN SCH1)      */~
                                 sp_type$,        /* (PLAN SCH2)      */~
                                 sp_cutoff$,      /* Cut Off Day 1-7  */~
                                 sp_route$,       /* Route code       */~
                                 sp_status$,      /* (PLAN SCH1)      */~
                                 sp_type$,        /* (PLAN SCH2)      */~
                                 sp_cust$,        /* Customer Code    */~
                                 sp_so$,          /* Sales order No.  */~
                                 sp_ln$,          /* S.O. Line item No*/~
                                 sp_ln_item$,     /* Line Item Piece  */~
                                 sp_warr$,        /* Warranty Number  */~
                                 sp_num$,         /* Rmk Number       */~
                                 sp_status$,      /* (PLAN SCH1)      */~
                                 sp_type$,        /* (PLAN SCH2)      */~
                                 sp_due$,         /* Sales order Due  */~
                                 sp_part$,        /* Part Number      */~
                                 sp_qty$,         /* line Item Qty    */~
                                 sp_stat$,        /* (PLAN STAT) Plann*/~
                                 sp_usr$,         /* Last Mod User Id */~
                                 sp_dte$,         /* Last Mod Date    */~
                                 sp_time$,        /* Last Mod Time    */~
                                 sp_text$,        /* Line Item Text Id*/~
                                 sp_primary$,     /* Primary Dep      */~
                                 sp_unitid$,      /* Unitid           */~
                                 sp_lam$,         /* Laminate Type    */~
                                 hdFil$           /* Filler Area      */

        return

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

L35500:     FMT                          /* (HLDCHED) File             */~
                CH(6),                   /* sp_status_dte$ Spec Anal St*/~
                CH(1),                   /* sp_status$ (PLAN SCH1)     */~
                CH(1),                   /* sp_type$ (PLAN SCH2)       */~
                CH(2),                   /* sp_cutoff$ Cut Off Day 1-7 */~
                CH(5),                   /* sp_route$ Route code       */~
                CH(1),                   /* sp_status$ (PLAN SCH1)     */~
                CH(1),                   /* sp_type$ (PLAN SCH2)       */~
                CH(9),                   /* sp_cust$ Customer Code     */~
                CH(8),                   /* sp_so$ Sales order number  */~
                CH(2),                   /* sp_ln$ S.O. Line item No.  */~
                CH(4),                   /* sp_ln_item$ Line Item Piece*/~
                CH(1),                   /* sp_status$ (PLAN SCH1)     */~
                CH(1),                   /* sp_type$ (PLAN SCH2)       */~
                CH(6),                   /* sp_due$ Sales order Due Dat*/~
                CH(25),                  /* sp_part$ Part Number       */~
                CH(4),                   /* sp_qty$ line Item Quantity */~
                CH(2),                   /* sp_stat$ (PLAN STAT) Plann */~
                CH(3),                   /* sp_usr$ Last Mod User Id   */~
                CH(6),                   /* sp_dte$ Last Mod Date      */~
                CH(8),                   /* sp_time$ Last Mod Time     */~
                CH(4),                   /* sp_text$ Line Item Text Id */~
                CH(3),                   /* sp_primary$ Primary Dept   */~
                CH(10),                  /* Unitid                     */~
                CH(1),                   /* Laminate Type              */~
                CH(142)                  /* sp_fil$ Filler Area        */

L35600:     FMT                          /* (HLDCHED) File             */~
                CH(6),                   /* sp_status_dte$ Spec Anal St*/~
                CH(1),                   /* sp_status$ (PLAN SCH1)     */~
                CH(1),                   /* sp_type$ (PLAN SCH2)       */~
                CH(2),                   /* sp_cutoff$ Cut Off Day 1-7 */~
                CH(5),                   /* sp_route$ Route code       */~
                CH(1),                   /* sp_status$ (PLAN SCH1)     */~
                CH(1),                   /* sp_type$ (PLAN SCH2)       */~
                CH(9),                   /* sp_cust$ Customer Code     */~
                CH(8),                   /* sp_so$ Sales order number  */~
                CH(2),                   /* sp_ln$ S.O. Line item No.  */~
                CH(4),                   /* sp_ln_item$ Line Item Piece*/~
                CH(9),                   /* Warranty                   */~
                CH(3),                   /* Number                     */~
                CH(1),                   /* sp_status$ (PLAN SCH1)     */~
                CH(1),                   /* sp_type$ (PLAN SCH2)       */~
                CH(6),                   /* sp_due$ Sales order Due Dat*/~
                CH(25),                  /* sp_part$ Part Number       */~
                CH(4),                   /* sp_qty$ line Item Quantity */~
                CH(2),                   /* sp_stat$ (PLAN STAT) Plann */~
                CH(3),                   /* sp_usr$ Last Mod User Id   */~
                CH(6),                   /* sp_dte$ Last Mod Date      */~
                CH(8),                   /* sp_time$ Last Mod Time     */~
                CH(4),                   /* sp_text$ Line Item Text Id */~
                CH(3),                   /* sp_primary$ Primary Dept   */~
                CH(10),                  /* Unitid                     */~
                CH(1),                   /* Laminate Type              */~
                CH(142)                  /* sp_fil$ Filler Area        */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40150:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,          /* beg_date$          */~
                                L40160,          /* end_date$          */~
                                L40160,          /* Analysis Process   */~
                                L40160,          /* Process Type Code  */~
                                L40160,          /* Process Selection  */~
                                L40160,          /* Beg Cut-Off EWD013 */~
                                L40160,          /* End Cut-Off EWD013 */~
                                L40160,          /* Beg HowShip EWD017 */~
                                L40160,          /* End HowShip EWD017 */~
                                L40160           /* Order Numbe AWD030 */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Special Selection Code :",                   ~
               at (03,27), fac(lfac$(1%)), sc_status$           , ch(01),~
               at (03,40), fac(hex(84)),   sc_status_d$         , ch(30),~
                                                                         ~
               at (04,02), "Special Type Code      :",                   ~
               at (04,27), fac(lfac$(2%)), sc_type$             , ch(01),~
               at (04,40), fac(hex(84)),   sc_type_d$           , ch(30),~
                                                                         ~
               at (05,02), "Special Process Code   :",                   ~
               at (05,27), fac(lfac$(3%)), sc_process$          , ch(01),~
               at (05,40), fac(hex(84)),   sc_process_d$        , ch(35),~
                                                                         ~
               at (06,02), fac(hex(84)),   sd1$                 , ch(24),~
               at (06,27), fac(lfac$(4%)), beg_date$            , ch(10),~
                                                                         ~
               at (07,02), fac(hex(84)),   sd2$                 , ch(24),~
               at (07,27), fac(lfac$(5%)), end_date$            , ch(10),~
/* (EWD013) - Beg */                                                     ~
               at (08,02), "Customer Cut-Off Code A:",                   ~
               at (08,27), fac(lfac$(6%)), beg_cut$             , ch(02),~
               at (08,40), fac(hex(84)),   beg_cut_d$           , ch(35),~
                                                                         ~
               at (09,02), "Customer Cut-Off Code B:",                   ~
               at (09,27), fac(lfac$(7%)), end_cut$             , ch(02),~
               at (09,40), fac(hex(84)),   end_cut_d$           , ch(35),~
/* (EWD013) - End */                                                     ~
/* (EWD017) - Beg */                                                     ~
               at (10,02), "Customer How-Ship Code :",                   ~
               at (10,27), fac(lfac$(8%)), beg_how$             , ch(02),~
               at (10,40), fac(hex(84)),   beg_how_d$           , ch(35),~
                                                                         ~
               at (11,02), "Customer How-Ship Code :",                   ~
               at (11,27), fac(lfac$(9%)), end_how$             , ch(02),~
               at (11,40), fac(hex(84)),   end_how_d$           , ch(35),~
/* (EWD017) - End */                                                     ~
/* (AWD030) - Beg */                                                     ~
               at (12,02), "Tempered Order Number  :",                   ~
               at (12,27), fac(lfac$(10%)), sc_order_num$       , ch(05),~
/* (AWD030) - End */                                                     ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 11 then goto L40200
                  if orderDate% = 0% then orderDate% = 1%    ~
                   else orderDate% = 0%
/* only all remakes by scan date */
                  if rmk% = 1% then orderDate% = 0%
                  goto L40150


L40200:        if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
/* (SR70932) */
/* orderDate% = 0% then Date-Special */
/* orderDate% = 1% then Due-Date     */
        if orderDate% <> 0% then goto setDueDate
           sd1$="Beg Date-Special Orders:"
           sd2$="End Date-Special Orders:"
             goto dateSet
setDueDate:
           sd1$="Beg Due-Date Orders    :"
           sd2$="End Due-Date Orders    :"
dateSet:

        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(11)Toggle Spec/Due    (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffff0bffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),19,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                  (9)Print Labels       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Select Data        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_analysis
            k% = 0%
L41000:     gosub set_pf2
            accept                                                       ~
               at (01,02), fac(hex(84)), sel$(1%)               , ch(15),~
               at (01,64), fac(hex(84)), pageno$                , ch(16),~
               at (02,02), fac(hex(84)), sel$(2%)               , ch(15),~
               at (02,64), fac(hex(84)), sel$(4%)               , ch(16),~
               at (03,02), fac(hex(84)), sel$(3%)               , ch(15),~
               at (03,64), fac(hex(84)), sel$(5%)               , ch(16),~
                                                                         ~
               at (02,18), fac(hex(a4)), title$                 , ch(45),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,04), fac(hex(a4))  , h1$                  , ch(04),~
               at (05,09), fac(hex(a4))  , h2$                  , ch(02),~
               at (05,12), fac(hex(a4))  , h10$                 , ch(03),~
               at (05,16), fac(hex(a4))  , h3$                  , ch(05),~
               at (05,22), fac(hex(a4))  , h4$                  , ch(06),~
               at (05,29), fac(hex(a4))  , h5$                  , ch(08),~
               at (05,38), fac(hex(a4))  , h6$                  , ch(02),~
               at (05,41), fac(hex(a4))  , h7$                  , ch(07),~
               at (05,49), fac(hex(a4))  , h8$                  , ch(25),~
               at (05,75), fac(hex(a4))  , h9$                  , ch(04),~
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (06,04), fac(hex(84))  , dt$(k% + 1%)         , ch(77),~
                                                                         ~
               at (07,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (07,04), fac(hex(84))  , dt$(k% + 2%)         , ch(77),~
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (08,04), fac(hex(84))  , dt$(k% + 3%)         , ch(77),~
                                                                         ~
               at (09,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (09,04), fac(hex(84))  , dt$(k% + 4%)         , ch(77),~
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (10,04), fac(hex(84))  , dt$(k% + 5%)         , ch(77),~
                                                                         ~
               at (11,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (11,04), fac(hex(84))  , dt$(k% + 6%)         , ch(77),~
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (12,04), fac(hex(84))  , dt$(k% + 7%)         , ch(77),~
                                                                         ~
               at (13,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (13,04), fac(hex(84))  , dt$(k% + 8%)         , ch(77),~
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 9%)         , ch(01),~
               at (14,04), fac(hex(84))  , dt$(k% + 9%)         , ch(77),~
                                                                         ~
               at (15,02), fac(hex(81))  , cc$(k% + 10%)        , ch(01),~
               at (15,04), fac(hex(84))  , dt$(k% + 10%)        , ch(77),~
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 11%)        , ch(01),~
               at (16,04), fac(hex(84))  , dt$(k% + 11%)        , ch(77),~
                                                                         ~
               at (17,02), fac(hex(81))  , cc$(k% + 12%)        , ch(01),~
               at (17,04), fac(hex(84))  , dt$(k% + 12%)        , ch(77),~
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 13%)        , ch(01),~
               at (18,04), fac(hex(84))  , dt$(k% + 13%)        , ch(77),~
                                                                         ~
               at (19,02), fac(hex(81))  , cc$(k% + 14%)        , ch(01),~
               at (19,04), fac(hex(84))  , dt$(k% + 14%)        , ch(77),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41040            /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080            /* Last      */
L41060:           x% = int(val_max% / 14%)
                  k% = (x%*14%)
                  if (k% + 1%) > val_max% then k% = k% - 14%
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 15% then goto L41020
                  k% = k% - 14%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41120             /* Next     */
                  k% = k% + 14%
                  if k% < val_max% then goto L41000
                  goto L41060

L41120:        if keyhit% <> 7% then goto L41110
                  report%, procBal% = 0%
/* (AWD024) */
/*(AWD027) */
                  if sc_type$ = "1" then goto temp_file
/* (AWD037) */
                  if sc_type$ = "B" then goto temp_file
                  if sc_type$ = "8" then goto shape_file
                  if sc_type$ = "9" then goto screen_file
                  if sc_type$ = "C" then goto balance_file /*(AWD039)*/
                  if sc_type$ = "5" then goto temp_file    /*(IM8022)*/
                  if sc_type$ = "D" then goto temp_file    /*(IM8022)*/
                  if sc_type$ = "E" then goto screen_file  /*(CR2245)*/

                  gosub build_file
                  goto L41000
temp_file:
                  labels% = 0%
                  gosub build_file_temp
                  goto L41000

shape_file:
                  gosub build_file_shape
                  goto L41000
screen_file:
                  gosub buildFileScreen
                  goto L41000

balance_file:
                  gosub buildFileBalance
                  goto L41000

L41110:        if keyhit% <> 9% then goto L41115
                  report% = 0%                 /* Based on Selection  */
                  if sc_type$ = "1" then goto temp_labels
/* (AWD037) */
                  if sc_type$ = "B" then goto temp_labels
                  gosub gen_labels
                  goto L41000
/* (AWD030)  */
temp_labels:
                  labels% = 1%
                  gosub build_file_temp
                  labels% = 0%
                  goto L41000


L41115:        if keyhit% <> 0% then goto L41130
                  gosub display_detail
                  goto L41000

L41130:        if keyhit% <> 10% then goto L41132
                  gosub update_specials

L41132:       if keyhit% <> 11% then goto L41135
                  gosub update_specials

L41135:        if keyhit% <> 12% then goto L41140
                  call "EWDPLB58" (switch%)    /* (EWD004) Begin     */
                  if switch% = 0% then goto L41000
                     sp_flag% = 4%
                     gosub delete_specials
                                               /* (EWD004) End       */

/* (AWD030) */
L41140:        if keyhit% <> 13% then goto L41145
                 if sc_status$ = "3" then goto createPO
                  if numOfErr% <> 0% then            ~
                   errormsg$ = "Error - Can't assign an Order Number, ~
                               ~Correct Errors"
                  if numOfErr% <> 0 then goto L41000
REM GOSUB CREATEORDERDETAIL
                   goto createOrderDetail
REM GOSUB ASSIGNORDERNUMBER
REM GOSUB WRITEORDER

L41145:        if keyhit% <> 14% then goto L41150
                  report% = 0%                 /* Based on Selection */
                  gosub gen_rpt1

L41150:        if keyhit% <> 15 then goto L41155
                  call "PRNTSCRN"
                  goto L41000

L41155:        if keyhit% = 16% and sc_status$ = "0" then gosub Plng_Reset
               if keyhit% <> 16% then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf2
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, h8$, h9$, h10$,x$
            dsp_msg$=                                                     ~
             "Use 'X' or 'E' Dsply or Edit Dtl Info, followed by <Return>?"
            str(dsp_msg$,62%,15%) = "Total [ xxxxx ]"
            convert dt_max% to str(dsp_msg$,70%,5%), pic(#####)

            sel$(1%) = "Beg: xxxxxxxxxx"
            sel$(2%) = "End: xxxxxxxxxx"
            sel$(3%) = "Typ: xxxxxxxxx "
            sel$(4%) = "Date: xxxxxxxxxx"
            sel$(5%) = "Time: xxxxxxxx  "
            str(sel$(1%),6%,10%) = beg_date$
            str(sel$(2%),6%,10%) = end_date$
            p% = pos(sc_type_d$ = "-")
            str(sel$(3%),6%,9%) = str(sc_type_d$,p% + 1%,9%)
            x$ = date
            call "DATFMTC" (x$)
            str(sel$(4%),7%,10%) = x$
            init(" ") x$
            call "TIME" (x$)
            str(sel$(5%),7%,8%) = str(x$,1%,8%)
            p% = pos(sc_status_d$ = "-")
            title$ = "Display of Items that are " &                      ~
                      str(sc_status_d$,p%+1%,18%)
            pageno$ = "Page: XXX of XXX"         /* k% = Array Values CR2732 */
            h1$  = "Proc"
            h2$  = "CO"
            h10$ = "RGN"           /* (CR893) */
            h3$  = "Route"
            h4$  = "Cust. "
            h5$  = "< S.O. >"
            h6$  = "Ln"
            h7$  = "Due Dte"
            h8$  = "<----- Part Number ----->"
            h9$  = " Qty"

            val_max% = dt_max%
            if val_max% > (3000% - 14%) then val_max% = 3000% - 14%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 14% )
            if mod(x,14) <> 0 then yy% = yy% + 1%

            xx% = (k% / 14%) +1%
            if xx% > yy% then xx% = yy%
/* CR2732 page count up to 3 digits */
            convert xx% to str(pageno$,7%,2%), pic(###) /* Current Page*/

            convert yy% to str(pageno$,14%,2%), pic(###)/* Total Pages */

               pf_txt$ = "(11)Place On Order  "
            if sc_type$ = "1" or sc_type$ = "B" then                     ~
               pf_txt$ = "(11)PlOnOdr(WarrAsg)"
            if sc_status$ = "2" then                                     ~
               pf_txt$ = "(11)Recived Specials"
            if sc_status$ = "2" and sc_type$ = "1" then                  ~
               pf_txt$ = "(11)Put Back On Order"
            if sc_status$ = "4" then                                     ~
               pf_txt$ = "(11)Release Specials"
            if sc_status$ = "Z" then pf_txt$ = "                    "

            pf$(1) = "(2)First           (5)Next              " &        ~
                               pf_txt$
            str(pf$(1%),64%,16%) = "(14)Print Report"
            pf$(2) = "(3)Last            (7)Build Temp File   " &        ~
                     "(12)Delete(Sel Only)   (15)Print Screen"

REM                   0        1         2         3         4
REM                   1234567890123456789012345678901234567890
            pf$(3) = "(4)Previous        (9)Print Labels      " &        ~
                     "(13)Create Order       (16)Exit Display"
                                                          /*  (EWD015)  */
            pfkeys$ = hex(ff02030405ff07ff09ff0b0c0d0e0f1000)
            if sc_status$ = "Z" then str(pfkeys$,11%,1%) = hex(ff)
            if sc_status$ = "0" then goto L41850
               str(pf$(2%),40%,21%) = " " : str(pfkeys$,12%,1%)=hex(ff)
               if userid$ <> "CMG" and userid$ <> "CGN" and userid$ <> "RDB"  ~
                  and userid$ <> "RBN" ~
                  and userid$ <> "CG1"  and userid$ <> "MES"and  ~
                      userid$ <> "PWW" then goto L41850
REM IF SC_STATUS$ <> "2" THEN GOTO L41850
REM IF SC_STATUS$ = "4" OR SC_STATUS$ = "Z" THEN GOTO L41850
                 str(pf$(2%),40%,21%) = " (10)Move back "
                 str(pfkeys$,10%,1%)  =hex(0a)
L41850:
/* (AWD030) */
/* (AWD037) */
            if sc_status$ <> "2" then gosub removePO_Option
            if sc_status$ = "2" and                  ~
               (sc_type$ <> "1" and sc_type$ <> "B")  ~
                  then gosub removePO_Option
/* (\AWD037) */
/* (AWD031)*/
REM IF SC_TYPE$ <> "1" THEN GOTO NOTTEMPLBLS
            if sc_type$ <> "1" and sc_type$ <> "B" then goto notTempLbls
               if sc_status$ = "3" then goto notTempLbls
               if sc_status$ = "4" then goto notTempLbls
               if sc_status$ = "Z" then goto notTempLbls
                  str(pf$(3),20%,20%) = " "
                  str(pfkeys$,9%,1%) = hex(ff)
notTempLbls:
            if sc_status$ <> "3" then goto noOraclePO
               str(pf$(3%),40%,20%) = " (13)Create PO "
               str(pfkeys$,13%,1%)=hex(0d)
noOraclePO:
/* (AWD022) */
            if sc_type$ <> "6" and sc_type$ <> "7" then goto L41855
               str(pf$(2),20%,20%) = "(7) Build Order File "

L41855:
/* (AWD022/) */

/* (AWD024) */
            if sc_type$ = "8" then str(pf$(2),20%,20%) =          ~
                                                   "(7)Build Shape File   "

/*(AWD027)*/
            if sc_type$ = "9" then str(pf$(2),20%,20%) =           ~
                                                   "(7)Build RiteScrn File"
/* (AWD039) */
            if sc_type$ = "C" then str(pf$(2),20%,20%) =            ~
                                                   "(7)Build Balance File "
/* (CR2245) */
            if sc_type$ = "E" then str(pf$(2),20%,20%) =            ~
                                                     "(7)Build Screen File"

            gosub check_screen
            if sc_status$ <> "0" then goto order_file_dsply
               str(pf$(2%),20%,21%) = " " : str(pfkeys$,7%,1%)=hex(ff)
               str(pf$(3%),20%,21%) = " " : str(pfkeys$,9%,1%)=hex(ff)

order_file_dsply:
            return
/* (AWD037) */
removePO_Option:
        str(pf$(3%),40%,20%) = " " : str(pfkeys$,13%,1%)=hex(ff)
return
/* (\AWD037) */

        check_screen
            if val_max% > 14% then goto L41860
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41860:      if k% >= 14% then goto L41870
                gosub no_first
                gosub no_prev
L41870:      if (k% + 14%) <= val_max% then goto L41880
                gosub no_last
L41880:      if k% <= (val_max% - 14%) then goto L41900
                gosub no_next
L41900: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return



        REM *************************************************************~
            *  D I S P L A Y   S U M M A R Y   S C R E E N  G L A S S   *~
            *-----------------------------------------------------------*~
            * Display Glass Screen                                      *~
            *************************************************************

        display_analysis_glass
            /* lfac$(fieldnr%) = hex(81)   return          Upper Only */
            /* lfac$(1%) = hex(82)                           Numeric */
            lfac$(1%)  = hex(81)
L42000:     gosub set_pf3
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,16), fac(hex(84)), gls_txt$(1%)           , ch(50),~
               at (04,16), fac(hex(84)), gls_txt$(2%)           , ch(50),~
               at (05,16), fac(hex(84)), gls_txt$(3%)           , ch(50),~
               at (06,16), fac(hex(84)), gls_txt$(4%)           , ch(50),~
                                                                         ~
               at (07,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (09,02), fac(hex(84)), cut_txt$               , ch(55),~
                 /* TOP      */                                          ~
                 /* ERROR CODE */                                        ~
               at (10,02), fac(hex(84)), cut_err$(1%)           , ch(03),~
               at (11,02), fac(hex(84)), cut_err$(2%)           , ch(03),~
               at (12,02), fac(hex(84)), cut_err$(3%)           , ch(03),~
               at (13,02), fac(hex(84)), cut_err$(4%)           , ch(03),~
               at (14,02), fac(hex(84)), cut_err$(5%)           , ch(03),~
                 /* WARRANTY */                                          ~
               at (10,07), fac(hex(84)), cut_warr$(1%)          , ch(09),~
               at (11,07), fac(hex(84)), cut_warr$(2%)          , ch(09),~
               at (12,07), fac(hex(84)), cut_warr$(3%)          , ch(09),~
               at (13,07), fac(hex(84)), cut_warr$(4%)          , ch(09),~
               at (14,07), fac(hex(84)), cut_warr$(5%)          , ch(09),~
                 /* WIDTH    */                                          ~
               at (10,18), fac(lfac$(2%)), cut_wd1_dec$(1%)     , ch(10),~
               at (11,18), fac(lfac$(2%)), cut_wd1_dec$(2%)     , ch(10),~
               at (12,18), fac(lfac$(2%)), cut_wd1_dec$(3%)     , ch(10),~
               at (13,18), fac(lfac$(2%)), cut_wd1_dec$(4%)     , ch(10),~
               at (14,18), fac(lfac$(2%)), cut_wd1_dec$(5%)     , ch(10),~
                 /* HEIGHT    */                                         ~
               at (10,30), fac(lfac$(2%)), cut_ht1_dec$(1%)     , ch(10),~
               at (11,30), fac(lfac$(2%)), cut_ht1_dec$(2%)     , ch(10),~
               at (12,30), fac(lfac$(2%)), cut_ht1_dec$(3%)     , ch(10),~
               at (13,30), fac(lfac$(2%)), cut_ht1_dec$(4%)     , ch(10),~
               at (14,30), fac(lfac$(2%)), cut_ht1_dec$(5%)     , ch(10),~
                 /* BOTTOM   */                                          ~
                 /* ERROR CODE */                                        ~
               at (16,02), fac(hex(84)), cut_err$(6%)           , ch(03),~
               at (17,02), fac(hex(84)), cut_err$(7%)           , ch(03),~
               at (18,02), fac(hex(84)), cut_err$(8%)           , ch(03),~
               at (19,02), fac(hex(84)), cut_err$(9%)           , ch(03),~
               at (20,02), fac(hex(84)), cut_err$(10%)          , ch(03),~
                 /* WARRANTY */                                          ~
               at (16,07), fac(hex(84)), cut_warr$(6%)          , ch(09),~
               at (17,07), fac(hex(84)), cut_warr$(7%)          , ch(09),~
               at (18,07), fac(hex(84)), cut_warr$(8%)          , ch(09),~
               at (19,07), fac(hex(84)), cut_warr$(9%)          , ch(09),~
               at (20,07), fac(hex(84)), cut_warr$(10%)         , ch(09),~
                 /* WIDTH    */                                          ~
               at (16,18), fac(lfac$(2%)), cut_wd1_dec$(6%)     , ch(10),~
               at (17,18), fac(lfac$(2%)), cut_wd1_dec$(7%)     , ch(10),~
               at (18,18), fac(lfac$(2%)), cut_wd1_dec$(8%)     , ch(10),~
               at (19,18), fac(lfac$(2%)), cut_wd1_dec$(9%)     , ch(10),~
               at (20,18), fac(lfac$(2%)), cut_wd1_dec$(10%)    , ch(10),~
                 /* HEIGHT    */                                         ~
               at (16,30), fac(lfac$(2%)), cut_ht1_dec$(6%)     , ch(10),~
               at (17,30), fac(lfac$(2%)), cut_ht1_dec$(7%)     , ch(10),~
               at (18,30), fac(lfac$(2%)), cut_ht1_dec$(8%)     , ch(10),~
               at (19,30), fac(lfac$(2%)), cut_ht1_dec$(9%)     , ch(10),~
               at (20,30), fac(lfac$(2%)), cut_ht1_dec$(10%)    , ch(10),~
                                                                         ~
               at (10,48), fac(hex(84)), cut_ged$(1%)           , ch(10),~
               at (11,48), fac(hex(84)), cut_ged$(2%)           , ch(10),~
               at (12,48), fac(hex(84)), cut_ged$(3%)           , ch(10),~
               at (13,48), fac(hex(84)), cut_ged$(4%)           , ch(10),~
               at (14,48), fac(hex(84)), cut_ged$(5%)           , ch(10),~
                                                                         ~
               at (16,48), fac(hex(84)), cut_ged$(6%)           , ch(10),~
               at (17,48), fac(hex(84)), cut_ged$(7%)           , ch(10),~
               at (18,48), fac(hex(84)), cut_ged$(8%)           , ch(10),~
               at (19,48), fac(hex(84)), cut_ged$(9%)           , ch(10),~
               at (20,48), fac(hex(84)), cut_ged$(10%)          , ch(10),~
                                                                         ~
                                                                         ~
               at (10,60), fac(lfac$(1%)), cut_ord_flg$(1%)      , ch(01),~
               at (11,60), fac(lfac$(1%)), cut_gls$(1%)          , ch(02),~
               at (11,70), fac(hex(84)), cut_sand$(1%)           , ch(10),~
               at (12,60), fac(lfac$(1%)), cut_spc$(1%)          , ch(10),~
               at (12,72), fac(hex(84)), cut_spc_dec$(1%)        , ch(08),~
               at (13,60), fac(hex(84)), str(cut_sand1$(1%),1,10), ch(10),~
               at (14,60), fac(hex(84)), str(cut_sand2$(1%),1,10), ch(10),~
/*AWD032*/     at (15,60), fac(hex(84)), str(cut_sand3$(1%),1,10), ch(10),~
                                                                         ~
               at (16,60), fac(lfac$(1%)), cut_ord_flg$(2%)      , ch(01),~
               at (17,60), fac(lfac$(1%)), cut_gls$(2%)          , ch(02),~
               at (17,70), fac(hex(84)), cut_sand$(2%)           , ch(10),~
               at (18,60), fac(lfac$(1%)), cut_spc$(2%)          , ch(10),~
               at (18,72), fac(hex(84)), cut_spc_dec$(2%)        , ch(08),~
               at (19,60), fac(hex(84)), str(cut_sand1$(2%),1,10), ch(10),~
               at (20,60), fac(hex(84)), str(cut_sand2$(2%),1,10), ch(10),~
/*AWD032*/     at (21,60), fac(hex(84)), str(cut_sand3$(2%),1,10), ch(10),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 0% then goto L42010
                  gosub validate_gls

L42010:        if keyhit% <> 9% then goto L42020
                  lfac$(2%) =  hex(81)
                  goto L42000

L42020:        if keyhit% <> 10% then goto L42030
                  goto L43010    /* end of display_detail */

L42030:        if keyhit% <> 16% then goto L42040
                  gosub datasave_analysis_glass
                  keyhit% = 10%
                  init(" ") cc$()
                  goto processDataHld
L42040:

               goto L42000


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf3
            init(" ") gls_txt$()
            gls_txt$(1%) = "Customer    : " & str(dt$(dt%),18%,6%)

            gls_txt$(2%) = "SO & Line   : " & str(dt$(dt%),25%,8%) &     ~
                           " " & str(dt$(dt%),34%,2%)
            gls_txt$(3%) = "Part Number : " & str(dt$(dt%),46%,25%)
            gls_txt$(4%) = " "
            inpmessage$ = "Correct Errors and Save or PF<10> to ~
                          ~Exit w/out saving"
            init(" ") pf$()
REM         pfkeys$ = hex(ff02030405ff07ff09ff0b0c0d0e0f1000)

            pf$(1) = "                  (9)Modify Sizes       " &        ~
                     "                  (10)Exit w/out Saving"
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Changes"
            pfkeys$ = hex(ffffffffffffffff090affffffffff1000)
        return


        REM *************************************************************~
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_detail
            for dt% = 1% to dt_max%
                if cc$(dt%) = "X" then goto L43000
                if cc$(dt%) = "E" then goto L43100
            next dt%
            goto L43010                          /* No Selection Found */

L43000:     sp_so$ = str(dt$(dt%),25%,8%)        /* Sales order        */
            sp_ln$ = str(dt$(dt%),34%,2%)        /* Line Item          */
            rk_barcode$ = "         "            /* (EWD005) - N/A     */
            rk_seq$     = "     "                /* (EWD005) - N/A     */
            dim1es  = dim1es(dt%)                /* CR1578  */

       call "EWDPLA58" (1%,              /* o%=Info Only, 1%=Info+Glass*/~
                        sp_so$,          /* Sales Order Number         */~
                        sp_ln$,          /* Sales order Line Item      */~
                        rk_barcode$,     /* Glass/Rack Barcode(EWD005) */~
                        rk_seq$,         /* Production Seq. No(EWD005) */~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",                                             ~
                        " ",             /* (AWD025)                   */~
                        subpart$,        /* Subpart number             */~
                        dim1es,          /* (AWD039)                   */~
                        dim2es,          /* (AWD039)                   */~
                        dim3es,          /* (AWD039)                   */~
                        #5,              /* (APCPLNOR) Planning Header */~
                        #6,              /* (APCPLNSC) Planning Ln Item*/~
                        #7,              /* (CUSTOMER) Customer Master */~
                        #4,              /* (GENCODES) Master Tables   */~
                        #9,              /* (AMTBOMIF) Master Validity */~
                        #8,              /* (TXTFILE ) Sales Text File */~
                        #2,              /* (AMTBOMCD) Master Equation */~
                        #12,             /* (BCKLINES) S.O. Dtl(EWD003)*/~
                        #15,             /* (EWDPLNRK) - (EWD005)      */~
                        er% )            /* 0% = Ok, Non-Zero = Error  */
L43010: init(" ") cc$()
        return

REM Display Detail Warranty IDs

L43100:

        if sc_status$ <> "2" then goto L43010

        gosub format_screen_gls
        init(" ") dt_warr$(), gl_warr$()
        convert str(dt$(dt%),72%,4%) to sp_qty%

        gosub load_warranties

        gosub load_pieces

        gosub display_analysis_glass
       return


       load_warranties
         for loop% = 1% to sp_qty%
           convert loop% to sp_ln_item$, pic(0000)
           init(" ") barcode$
           str(barcode$,1%,8%)  = str(dt$(dt%),25%,8%)
           str(barcode$,9%,2%)  = str(dt$(dt%),34%,2%)
REM could this use 0001 and then update all?
REM STR(BARCODE$,11%,4%) = SP_LN_ITEM$
          str(barcode$,11%,4%) = str(dt$(dt%),78%,4%)
REM STR(BARCODE$,11%,4%) = "0001"
          str(barcode$,15%,4%) = str(dt$(dt%),72%,4%)

          read #28, key 2% = barcode$, using AWDSCHDT_FMT1, warrantyid$, ~
                                 spErr$, eod goto edit_awdschdt_done

AWDSCHDT_FMT1:        FMT POS(21), CH(8), POS(57), CH(03)

           dt_warr$(loop%) = warrantyid$  /* Save all warrantyids */
         next loop%
       return

       load_pieces
         warrantyCnt% = 0%
         for loop% = 1% to sp_qty%
           init(" ") awdschgl_key0$
           awdschgl_key0$ = dt_warr$(loop%)

edit_awdschdt_next:
           read #32, key > awdschgl_key0$, using AWDSCHGL_FMT2,          ~
                                awdschgl_key0$, eod goto edit_awdschdt_done

AWDSCHGL_FMT2:       FMT POS(11), CH(09)

             if str(awdschgl_key0$,1%,8%) <> dt_warr$(loop%) then        ~
                                                goto edit_awdschdt_done


             gosub dataloadGl
             if sc_status$ = "3" then goto checkOrderNumber

             warrantyCnt% = warrantyCnt% + 1%
             gl_warr$(warrantyCnt%) = str(awdschgl_key0$,1%,9%)
             if loop% = 1% then gosub load_screen_gl  /*Only load 1st one*/
                                                /* remaining are repeated*/
           goto edit_awdschdt_next              /* quantities            */
         edit_awdschdt_done
         next loop%
       return
       checkOrderNumber
         if gl_order$ = sc_order_num$ then validOrder% = 1%
         if validOrder% = 1% then return
       goto edit_awdschdt_next


       format_screen_gls
         init(" ") errormsg$, cut_txt$, cut_ged$(), cut_err$(),  ~
                   cut_warr$(), cut_wd1_dec$(), cut_ht1_dec$(),  ~
                   cut_sand$(), cut_spc$(), cut_spc_dec$(),      ~
                   cut_sand1$(), cut_sand2$(), cut_ord_flg$(),   ~
                   cut_gls$(), cut_sand3$(), cut_wd1$(), cut_ht1$()
         mat cut_wd1_dec = zer
         mat cut_ht1_dec = zer
REM screen setup
REM                       1         2         3         4         5         6
REM               23456789012345678901234567890123456789012345678901234567890
REM String setup
REM                        1         2         3         4         5         6
REM               123456789012345678901234567890123456789012345678901234567890
              cut_txt$ =                                                 ~
                 "ERR  WARRANTY   WIDTH       HEIGHT            "

            cut_ged$( 1%) = "Order Gls="
            cut_ged$( 2%) = "Sandwich ="
            cut_ged$( 3%) = "Spacer   ="
            cut_ged$( 4%) = "Cardinal ="
            cut_ged$( 5%) = "Cardinal ="

            cut_ged$( 6%) = "Order Gls="
            cut_ged$( 7%) = "Sandwich ="
            cut_ged$( 8%) = "Spacer   ="
            cut_ged$( 9%) = "Cardinal ="
            cut_ged$(10%) = "Cardinal ="


            lfac$() = all(hex(84))

        return


        validate_gls
          init(" ") sp_sand1$, sp_sand2$, errormsg$, spErr$, sp_sand3$
REM Assume Error has been resolved
          spErr$ = "000"
REM not top
          if cut_warr$(1%) = " " then goto validate_sand2
REM Set order flag
          if cut_ord_flg$(1%) = "0" or cut_ord_flg$(1%) = "1"         ~
                                                   then goto val_ig_1
            errormsg$ = "Invalid Order Flag 1"
            cut_err$(1%) = spErr$
          goto L42000
val_ig_1:
REM 0 means not to order gls!!
          for err_loop% = 1% to 5%
            if cut_err$(err_loop%) = "   " then goto top_err_loop_done
            if cut_err$(err_loop%) <> "000" and cut_ord_flg$(1%) = "0"  ~
                                 then cut_err$(err_loop%) = "000"
          next err_loop%
top_err_loop_done:
         if cut_ord_flg$(1%) = "0" then goto validate_sand2
REM Validate Glass code
         tab% = 15%
         code$ = cut_gls$(1%)
         gosub check_code
         if code% = 1% then goto validate_ig_sand1
            spErr$ = "999"
            errormsg$ = "Invalid IG Code 1"
            cut_err$(1%) = spErr$
          goto L42000
validate_ig_sand1:
          str(cut_sand$(1%),1%,10%) = str(desc$,1%,10%)
REM Set strength and both sandwich codes
/* (AWD032) */
          if str(cut_sand$(1%),1%,2%) <> "TG" then goto not_trip1
            tab% = 20%
            code$ = cut_gls$(1%) & "T"
            gosub check_code
            strength$ = str(desc$,1%,1%)
            sp_sand1$ = str(desc$,2%,2%)
            sp_sand2$ = str(desc$,10%,2%)
            sp_sand3$ = str(desc$,18%,2%)
            if code% = 0% then goto strength1Sandwich  /* (CR1988) */
REM  GOTO TRIP1_SET                                    /* (CR1988) */
          goto strength1Done                           /* (CR1988) */
not_trip1:
           strength$ = str(cut_sand$(1%),3%,1%)
           sp_sand1$ = str(cut_sand$(1%),4%,2%)
           sp_sand2$ = str(cut_sand$(1%),7%,2%)
REM TRIP1_SET                                         /* (CR1988) */
strength1Sandwich:
           strength$ = str(cut_sand$(1%),3%,1%)       /* (CR1988) */
           sp_sand1$ = str(cut_sand$(1%),4%,2%)       /* (CR1988) */
           sp_sand2$ = str(cut_sand$(1%),7%,2%)       /* (CR1988) */

strength1Done:                                        /* (CR1988) */
           gosub validate_sand_temp
REM set cardinal sandwich
           cut_sand1$(1%) = csandwich$(1%)
           cut_sand2$(1%) = csandwich$(2%)
/*(AWD032)*/
           cut_sand3$(1%) = csandwich$(3%)
           if spErr$ = "000" then goto validate_spc1
              errormsg$ = "Invalid IG Sandwich 1"
              cut_err$(1%) = spErr$
           goto L42000
validate_spc1:
REM Validate Spacer ie SP15
           for i_loop% = 1% to spcr%
             if cut_spc$(1%) = spcr$(i_loop%) then goto spc1_found
           next i_loop%
              spErr$ = "999"
              errormsg$ = "Invalid Spacer 1"
              cut_err$(1%) = spErr$
          goto L42000
spc1_found:
REM set spacer decimal from spacer
          convert spcr_dec(i_loop%) to cut_spc_dec$(1%), pic(0.########)
          cut_spc_dec$(1%) = str(cut_spc_dec$(1%),2%,6%)
REM Update error code
          for err_loop% = 1% to 5%
            if cut_err$(err_loop%) <> "   " then cut_err$(err_loop%) = spErr$
          next err_loop%

validate_sand2:
REM no bottom
          if cut_warr$(6%) = " " then return
           spErr$ = "000"
REM Set order flag
          if cut_ord_flg$(2%) = "0" or cut_ord_flg$(2%) = "1"      ~
                                                  then goto val_ig_2
             errormsg$ = "Invalid Order Flag 2"
             cut_err$(1%) = spErr$
          goto L42000
val_ig_2:
          for err_loop% = 6% to 10%
            if cut_err$(err_loop%) = "   " then goto bot_err_loop_done
            if cut_err$(err_loop%) <> "000" and cut_ord_flg$(2%) = "0"     ~
                                 then cut_err$(err_loop%) = "000"
          next err_loop%
bot_err_loop_done:
REM IF CUT_ERR$(2%) <> "000" AND CUT_ORD_FLG$(2%) = "0"  THEN CUT_ERR$(6%) = "000"
          if cut_ord_flg$(2%) = "0" then return

REM Validate Glass code
          tab% = 15%
          code$ = cut_gls$(2%)
          gosub check_code
          if code% = 1% then goto validate_ig_sand2
             spErr$ = "999"
             errormsg$ = "Invalid IG Code 2"
             cut_err$(1%) = spErr$
          goto L42000
validate_ig_sand2:
          str(cut_sand$(2%),1%,10%) = str(desc$,1%,10%)
/* (AWD032) */
          if str(cut_sand$(2%),1%,2%) <> "TG" then goto not_trip2
             tab% = 20%
             code$ = cut_gls$(1%) & "B"
             gosub check_code
             strength$ = str(desc$,1%,1%)
             sp_sand1$ = str(desc$,2%,2%)
             sp_sand2$ = str(desc$,10%,2%)
             sp_sand3$ = str(desc$,18%,2%)
             if code% = 0% then goto strength2Sandwich   /* (CR1988) */
REM GOTO TRIP2_SET
             goto strength2Done                           /* (CR1988) */
not_trip2:
           strength$ = str(cut_sand$(2%),3%,1%)
           sp_sand1$ = str(cut_sand$(2%),4%,2%)
           sp_sand2$ = str(cut_sand$(2%),7%,2%)
REM TRIP2_SET
strength2Sandwich:
           strength$ = str(cut_sand$(1%),3%,1%)       /* (CR1988) */
           sp_sand1$ = str(cut_sand$(1%),4%,2%)       /* (CR1988) */
           sp_sand2$ = str(cut_sand$(1%),7%,2%)       /* (CR1988) */

strength2Done:                                        /* (CR1988) */
           gosub validate_sand_temp
           cut_sand1$(2%) = csandwich$(1%)
           cut_sand2$(2%) = csandwich$(2%)
           cut_sand3$(2%) = csandwich$(3%)
           if spErr$ = "000" then goto validate_spc2
              errormsg$ = "Invalid IG Sandwich 2"
              cut_err$(6%) = spErr$
              goto L42000
validate_spc2:
           for i_loop% = 1% to spcr%
             if cut_spc$(2%) = spcr$(i_loop%) then goto spc2_found
           next i_loop%
              spErr$ = "999"
              errormsg$ = "Invalid Spacer 2"
              cut_err$(6%) = spErr$
              goto L42000
spc2_found:
           convert spcr_dec(i_loop%) to cut_spc_dec$(2%), pic(0.########)
           cut_spc_dec$(2%) = str(cut_spc_dec$(2%),2%,6%)
           for err_loop% = 6% to 10%
              if cut_err$(err_loop%) <> "   " then cut_err$(err_loop%) = spErr$
           next err_loop%
REM validated
        return

        datasave_analysis_glass
          init(" ") awdschgl_key0$, spErr$, warrantyid$,barcode$
          spErr$ = "000"
          warrantyid$ = gl_warr$(1%)
          for loop% = 1% to warrantyCnt%
            awdschgl_key0$ = gl_warr$(loop%)
            if warrantyid$ <> str(awdschgl_key0$,1%,8%)           ~
                            then gosub datasave_awdschdt

            read #32, hold, key = awdschgl_key0$, using AWDSCHGL_FMT4,    ~
                                    barcode$, eod goto datasave_gl_done


              convert str(awdschgl_key0$,9%,1%) to gl%, data goto bad_warr1
bad_warr1:
              gl%  = gl% + 1%
              cut% = 1%
              if gl% > 5% then cut% = 2%
              put #32, using AWDSCHGL_FMT3, cut_sand$(cut%),       ~
                                            cut_sand1$(cut%),      ~
                                            cut_sand2$(cut%),      ~
                                            cut_spc$(cut%),        ~
                                            cut_spc_dec$(cut%),    ~
                                            cut_err$(gl%),         ~
                                            cut_ord_flg$(cut%),    ~
                                            cut_gls$(cut%),        ~
                                            cut_sand3$(cut%) /* (AWD032) */

               rewrite #32

              if cut_err$(gl%) <> "000" and cut_err$(gl%) <> "   "  ~
                             then spErr$ = cut_err$(gl%)

datasave_gl_done:
          next loop%
          gosub datasave_awdschdt

          return
REM GOTO L43010    /* GO BACK TO DISPLAY_ANALYSIS */

        datasave_awdschdt
REM could be at some point changed to use warrantyid
REM READ #28, HOLD, KEY 3% = WARRANTYID$, EOD GOTO DATASAVE_ANAL_GLASS_DONE
          read #28, hold, key 2% = barcode$, eod goto datasave_anal_glass_done

            put #28, using AWDSCHDT_FMT3, spErr$

            rewrite #28

            warrantyid$ = str(awdschgl_key0$,1%,8%)

          datasave_anal_glass_done
          return




AWDSCHGL_FMT3:     FMT POS(20), CH(20), POS(48), CH(30), CH(30), CH(10), ~
                       CH(10),  POS(207), CH(03), CH(01), CH(02), POS(214),~
                       CH(30)
AWDSCHGL_FMT4:     FMT POS(189), CH(18)


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$, code$
            on fieldnr% gosub L50000,                 /* sc_status$   */  ~
                              L50100,                 /* sc_type$     */  ~
                              L50200,                 /* sc_process$  */  ~
                              L50300,                 /* beg_date$    */  ~
                              L50400,                 /* end-date$    */  ~
                              L50500,                 /* beg cut-off  */  ~
                              L50600,                 /* end cut-off  */  ~
                              L50700,                 /* Beg how EWD017*/ ~
                              L50800,                 /* End How EWD017*/ ~
                              L50900                  /* OrderNum AWD030*/
            return

L50000: Rem Special Selection Code                    sc_status$
            if sc_status$ = " " then sc_status$ = "0"
            tab% = 1%                            /* (PLAN SCH1)      */
            code$ = sc_status$
            gosub check_code
            if code% = 0% then goto L50010
               sc_status_d$ = desc$

REM            sd1$="Beg Date-Special Orders"    /* (SR70932) */
REM            sd2$="End Date-Special Orders"    /* (SR70932) */
            p% = pos(sc_status_d$ = "-")
            str(sd1$,10%,14%) = str(sc_status_d$,p%+1%,14%)
            str(sd2$,10%,14%) = str(sc_status_d$,p%+1%,14%)

        return
L50010:     init(" ") sc_status$, sc_status_d$
            errormsg$ = "(Error) Invalid Special Selection Code?"
            gosub error_prompt
REM            sd1$="Beg Date-Special Orders"    /* (SR70932) */
REM            sd2$="End Date-Special Orders"    /* (SR70932) */
            orderDate% = 0%                      /* (SR70932) */
        return

L50100: Rem Special Type Code                          sc_type$
            rmk% = 0%
            if sc_type$ = " " then sc_type$ = "1"
            tab% = 2%                            /* (PLAN SCH2)      */
            code$ = sc_type$
            gosub check_code
REM IF SC_TYPE$ = "5" THEN CODE% = 0%          /*  (EWD015)  */
            if sc_type$ = "5" or sc_type$ = "D" then rmk% = 1% /*(IM8022)*/
/* SR64679 + */
            gls_lamn% = 0%
            if sc_type$ = "B" or sc_type$ = "D" then gls_lamn% = 1%
/* SR64679 - */
            if code% = 0% then goto L50110
               sc_type_d$ = desc$
        return
L50110:     init(" ") sc_type$, sc_type_d$
            errormsg$ = "(Error) Special Type Code?"
            gosub error_prompt
        return

L50200: Rem Special Selection Process Code             sc_process$
            if sc_process$ = " " then sc_process$ = "0"
            if sc_process$ = "0" then                                  ~
               sc_process_d$ = "Only Process 'X's Selected?"
            if sc_process$ = "1" then                                  ~
               sc_process_d$ = "Only Process 'Blank' Selections?"
            p% = pos("01" = sc_process$)
            if p% = 0% then goto L50210
        return
L50210:     init(" ") sc_process$, sc_process_d$
            errormsg$ = "(Error) Invalid Process Selection Code?"
            gosub error_prompt
        return

L50300: REM Beginning search Date                    BEG_DTE$, BEG_DATE$
            if beg_date$ <> " " then goto L50310
               beg_date$ = date

L50310:     date% = 0%                       /* Formatted with Cent   */
            call "DATEOKC" (beg_date$, date%, errormsg$)

            if date% = 0% then goto L50320
            x$ = beg_date$
            call "DATUFMTC"(x$)              /* Unformatted with Cent */
            beg_dte$ = str(x$,1%,6%)
        return
L50320:     init(" ") beg_dte$, beg_date$, x$
            errormsg$ = "(Error) Invalid Begining Re-make Date?"
            gosub error_prompt
        return

L50400: REM Ending search Make Date              END_DTE$, END_DATE$
            if end_date$ <> " " then goto L50410
               end_date$ = beg_date$
L50410:     date% = 0%                       /* Formatted with Cent    */
            call "DATEOKC" (end_date$, date%, errormsg$)
            if date% = 0% then goto L50420
            x$ = end_date$
            call "DATUFMTC"(x$)              /* Unformatted with Cent  */
            end_dte$ = str(x$,1%,6%)
            if end_dte$ < beg_dte$ then goto L50420
        return
L50420:     init(" ") end_dte$, end_date$, x$
            errormsg$ = "(Error) Invalid Ending Re-make Date?"
            gosub error_prompt
        return
                                                 /*  (EWD013)  -  BEG  */
L50500: REM Beginning Customer Cut-Off Code A    BEG_CUT$, BEG_CUT_D$
            if beg_cut$ <> " " then goto L50510
               beg_cut$ = "AL"
            return
L50510:     if str(beg_cut$,1%,2%) = "AL" then return
            init(" ") beg_cut_d$, code$
            tab% = 6%
            str(code$,1%,2%) = beg_cut$
            gosub check_code
            if code% <> 1% then goto L50550
            str(beg_cut_d$,1%,35%) = desc$

        return
L50550:     init(" ") beg_cut$, beg_cut_d$
            errormsg$ = "(Error) Invalid Customer Cut-Off Code A?"
            gosub error_prompt
        return

L50600: REM Ending Customer Cut-Off Code B       END_CUT$, END_CUT_D$
            if end_cut$ <> " " then goto L50610
               end_cut$ = "AL"
            return
L50610:     if str(beg_cut$,1%,2%) = "AL" then return
            init(" ") end_cut_d$, code$
            tab% = 6%
            str(code$,1%,2%) = end_cut$
            gosub check_code
            if code% <> 1% then goto L50650
            str(end_cut_d$,1%,35%) = desc$

        return
L50650:     init(" ") end_cut$, end_cut_d$
            errormsg$ = "(Error) Invalid Customer Cut-Off Code B?"
            gosub error_prompt
        return
                                                 /*  (EWD013)  -  END  */
                                                 /*  (EWD017)  -  BEG  */
L50700: REM Beginning Customer How-Ship Code     BEG_HOW$, BEG_HOW_D$
            if beg_how$ <> " " then goto L50710
               beg_how$ = "AL"
            return
L50710:     if str(beg_how$,1%,2%) = "AL" then return
            init(" ") beg_how_d$, code$
            tab% =  10%
            str(code$,1%,2%) = beg_how$
            gosub check_code
            if code% <> 1% then goto L50750
            str(beg_how_d$,1%,35%) = desc$

        return
L50750:     init(" ") beg_how$, beg_how_d$
            errormsg$ = "(Error) Invalid Customer How-Ship Code ?"
            gosub error_prompt
        return

L50800: REM Ending Customer How-Ship Code        END_HOW$, END_HOW_D$
            if end_how$ <> " " then goto L50810
               end_how$ = "AL"
            return
L50810:     if str(beg_how$,1%,2%) = "AL" then return
            init(" ") end_how_d$, code$
            tab% = 10%
            str(code$,1%,2%) = end_how$
            gosub check_code
            if code% <> 1% then goto L50850
            str(end_how_d$,1%,35%) = desc$

        return
L50850:     init(" ") end_how$, end_how_d$
            errormsg$ = "(Error) Invalid Customer How-Ship Code ?"
            gosub error_prompt
        return
                                                 /*  (EWD017)  -  END  */
/*(AWD030)*/
L50900: REM Order Number                         SC_ORDER_NUM$
        if sc_order_num$ <> " " then goto L50910
           sc_order_num$ = "ALL"
        return
L50910: if str(sc_order_num$,1%,3%) = "ALL" then return
        convert sc_order_num$ to sc_order_num%, data goto L50990

        convert sc_order_num% to sc_order_num$, pic(00000)
        init(" ") hldLoadKey$
        hldLoadKey$ = sc_order_num$
        read #30, key = hldLoadKey$, eod goto L50990

        return
L50990:     init(" ") sc_order_num$
            errormsg$ = "(Error) Invalid Order number?"
            gosub error_prompt
        return
/*(\AWD030)*/

        check_code
            code% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = tab$(tab%)
            str(readkey$,10%,15%) = code$
            read #4,key = readkey$, using L54000, desc$,                ~
                                                eod goto check_code_done
L54000:        FMT POS(25), CH(30)
            code% = 1%
        check_code_done
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55000: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55010: %!######## @ ########                      ######################~
        ~##################                              Page:       ### !

L55020: %! Begin Date: ##########                  ######################~
        ~##################                       End Date : ########### !
                                                           /* (Y2K, LDJ) */
L55030: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55040: %!Customer !SalesOrd!Ln!Due Date!<----- Part Number ----->!<---- ~
        ~Description ------------->! Qty !*! T/B ! Cut Width ! Cut Height!

L55050: %!---------!--------!--!--------!-------------------------!------~
        ~--------------------------!-----!-!-----!-----------!-----------!
REM      1         2         3         4         5         6        7
REM      1234567890123456789012345678901234567890123456789012345678901234567890
L55060: %!#########!########!##!########!#########################!######~
        ~##########################!#### !#! ### ! ######### ! ########  !


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        calc_glass_size
            opts% = 0%
            dept$ = "000"
            g_cnt% = 0%
            spType% = 0%

REM IF STR(SP_PART$,1%,1%) = "8" THEN DEPT$ = "008"  /* (AWD038) */
/* (AWD038) */
REM GOSUB CHECK_CASEMENT_MODEL
REM IF CODE% = 1% THEN DEPT$ = "008"
/* (\AWD038) */

            call "EWDGLSSB" (opts%,      /* Options 0% = No Calc       */~
                             sp_part$,   /* MFG Part Number            */~
                             subpart$,   /* (AWD005)                   */~
                             dim1es,     /* (AWD039)                   */~
                             dim2es,     /* (AWD039)                   */~
                             dim3es,     /* (AWD039)                   */~
                             dept$,      /* Department Code            */~
                             ct%,        /* Glass Piece Count          */~
                             ctt$(),     /* 1-5 = Top, 6-10 = Bot      */~
                             gdd$(),     /* 1-6 = Top, 7-12 = Bot      */~
                             ajj$(),     /* Window Adjustment (GED) Top*/~
                             dcc$(),     /* Decimal 1-5=Top, 6-10=Bot  */~
                             wd$,        /* Window width Eights        */~
                             ht$,        /* window Height Eights       */~
                             g_cnt%,     /* Glass Piece Cut Count      */~
                             spType%,    /* Spacer Type (AWD028)       */~
                             #4,         /* (GENCODES) Master Tables   */~
                             #2,         /* (AMTBOMCD) Equations       */~
                             er% )       /* 0%=Ok, Non-Zero = Error    */

/*(AWD028) */
            init(" ") strength$
            if spType% = 1% or spType% = 4% then strength$ = "3"
            if spType% = 2% or spType% = 3% then strength$ = "4"
            if spType% = 5% or spType% = 6% then strength$ = "4"
                            /* OBS Glass - Cardinal does not have 5/32 OBS */
            if spType% = 7% then strength$ = "6"
            if spType% = 8% then strength$ = "5"
/* CR3287 */		
			if spType% = 8% and schema% = 1% and                             ~
              (str(sp_part$,5%,2%) = "AC"  or                                ~
               str(sp_part$,5%,2%) = "28"  or str(sp_part$,5%,2%) = "D3" or  ~
               str(sp_part$,5%,2%) = "Q4"  or str(sp_part$,5%,2%) = "AP")    ~
                   then strength$ = "6"

            if spType% = 9% then strength$ = "4"
            if spType% = 10% then strength$ = "4"
            if spType% = 11% then strength$ = "4"
            if spType% = 16% then strength$ = "6"
            if spType% = 17% then strength$ = "5"           /* (CR1988) */
            if spType% = 18% then strength$ = "6"           /* (CR1988) */

REM IF SPTYPE% = 12% THEN STRENGTH$ = "5"
REM IF SPTYPE% = 13% THEN STRENGTH$ = "5"
REM IF SPTYPE% = 14% THEN STRENGTH$ = "6"
REM IF SPTYPE% = 15% THEN STRENGTH$ = "6"

        return

/* (AWD024) */
        calc_glass_size_shape
           sh_model$ = str(sp_part$,1,3)
           sh_lt$    = str(sp_part$,7,2)

           gosub lookup_shape
           if check% = 0% then goto not_a_shape

           init(" ") shape_cross$
           str(shape_cross$,1,1) = str(sp_part$,7,1)  /*Liting*/
           str(shape_cross$,2,1) = str(sp_part$,10,1) /*Hinge */

           gosub lookup_cross         /* Get Shape Code */
           if check% = 0% then goto not_a_shape

           gosub verify_model_shape   /* Verify Model */
           if check% = 0% then goto not_a_shape

           gosub load_shape_prompts   /* Get Shape Prompts */
           if check% = 0% then goto not_a_shape

           sh_glass$ = str(sp_part$,5,2)   /* Glass */
REM A27$      = STR(SP_PART$,11,1)  /* SCREEN */

           gosub lookup_shape_glass        /* Get Sandwich */
           if check% = 0% then goto not_a_shape

           gosub check_shape_temp

           gosub check_grid_color

           str(sh_face$,1,2) = sh_config$
           str(sh_face$,3,2) = sh_glass$

           sh_qty$ = "   1"
REM SH_QTY% = 1%

           gosub lookup_txtid

           nbr_line% = 0%
           init(" ") txt$, text$()
           call "APCPLTXT" (#8, txtid$, text$(), nbr_line%)
           txt$     = str(text$(2%),1%,40%)       /* Obtain Glass Text  */
                                                  /* From Sales Order   */
                                                  /* Need sh() and sh$()*/
           gosub convert_shape

           for kk% = 1% to 6%
              sh(kk%) = 0.0
              if str(sh_entry$,kk%,1%) = "N" then goto Next_kk
                for ll% = 1% to 4%
                   if str(sh_entry$,kk%,1%) <> str(sh_codes$,ll%,1%) then ~
                                                   goto Next_ll
                      sh(kk%) = shd(ll%)
                      goto Next_kk

Next_ll:           next ll%

Next_kk:           convert sh(kk%) to sh$(kk%), pic(###.####-)

                   if sh(kk%) < 1.0 then sh$(kk%) = "         "

            next kk%

            gosub calculate_special_shape          /* Calculate Glass    */

        not_a_shape

        return

        lookup_txtid
           convert sp_ln$ to sp_ln%, data goto txt_done

           init(" ") readkey$
           str(readkey$,1,16) = sp_so$
           convert sp_ln% to str(readkey$,17,3), pic(###)

           read #12, key = readkey$, eod goto txt_done

             get #12, using txt_fmt, txtid$
txt_fmt:         FMT POS(242), CH(4)

        txt_done
        return


        lookup_shape
           init(" ") readkey$
           check% = 0%
           str(readkey$,1%,9%)   = "PLAN SHAP"
           str(readkey$,10%,15%) = sh_model$
           gosub readGencdsNoDesc
           if found% = 1% then check% = 1%
        lookup_shape_done

        return

        lookup_shape_glass
           init(" ") readkey$, desc$, sh_glass_d$
           check% = 0%
           str(readkey$, 1%,9%)  = "PLN GLASS"
           str(readkey$,10%,15%) = sh_glass$
           gosub readGencdsDesc
           if found% = 0% then goto lookup_shape_glass_done

           sh_glass_d$ = desc$
           sh_sandwich$= str(desc$,1%,12%)
           check% = 1%
           return
        lookup_shape_glass_done
           sh_sandwich$ = "Error - "& sh_glass$ /* Not Defined         */
           sh_glass_d$  = sh_sandwich$
           check% = 1%
        return


       lookup_cross
         init(" ") readkey$, desc$, sh_config$, sh_config_seq$, sh_codes$
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
         gosub readGencdsDesc
         if found% = 0% then goto lookup_cross_done

         sh_config$     = str(desc$,1%,2%)     /* Shape Code           */
         sh_config_seq$ = str(desc$,3%,2%)     /* Sequence code        */
                                                 /* Unpack Codes         */
         str(sh_codes$,1%,1%) = str(desc$,6%,1%)
         str(sh_codes$,2%,1%) = str(desc$,8%,1%)
         str(sh_codes$,3%,1%) = str(desc$,10%,1%)
         str(sh_codes$,4%,1%) = str(desc$,12%,1%)

         check% = 1%
       lookup_cross_done

       return

       lookup_specials                         /* Octagon/Circles     */
         init(" ") readkey$, desc$
         str(readkey$,1%,9%) = "SHAPEXTRA"
         str(readkey$,10%,15%) = sh_model$
         gosub readGencdsDesc
          if found% = 0% then goto lookup_specials_done

         shape_cross$ = str(desc$,1%,2%)

       lookup_specials_done
       return

       check_shape_temp
         init(" ") readkey$, desc$
         temp% = 0%
         str(readkey$,1%,9%)   = "PLAN TEMP"  /* Check Tempered     */
         str(readkey$,10%,15%) = str(sp_part$,5%,2%)
         gosub readGencdsDesc
          if found% = 0% then goto check_shape_temp_done
         temp% = 1%
       check_shape_temp_done
       return


       check_grid_color
         init(" ") readkey$
         check% = 0%
         str(readkey$,1%,9%)   = "EXTGRDCL "
         str(readkey$,10%,15%) = field11$
         gosub readGencdsDesc
          if found% = 0% then goto lookup_grdcolor_done
          p% = pos(desc$ = "-")
          sh_grid_color$ = str(desc$,p%+2%,6%)
       lookup_grdcolor_done

       return

       load_shape_prompts
         init(" ") sh_bridge$, sh_entry$
         check% = 0%
                                 /* 2nd Load Field Prompts           */
                                 /* Text with input criteria         */
                                 /* Flags 1%,1% = Input Y or N       */
                                 /*       2%,1% = What is Entered    */
                                 /*       3%,1% = What is Calculated */
           for i% = 1% to 7%     /* Seven Possible Field Prompts     */
             init(" ") readkey$, desc$
             str(readkey$,1%,9%) = "PLNCNFIG "
             str(readkey$,10%,15%) = sh_config$
             convert i% to str(readkey$,12%,1%), pic(0)

             gosub readGencdsDesc
              if found% = 0% then goto load_shape_prompts_done

             tt$(i%) = str(desc$,1%,25%) /* Field Prompt Description*/
             sh_flags$(i%) = str(desc$,28%,3%)
                                           /* Data input Flags 1, 2,3 */
                                           /* (1) Input Data Y or N   */
                                           /* (2) Input Data Type     */
                                           /* (3) Calc Dat Out        */
                                           /* Set Calculate Field     */
            if str(sh_flags$(i%),3%,1%) = "W" then                     ~
                ttt$(i%) = "Width   "
            if str(sh_flags$(i%),3%,1%) = "H" then                     ~
                ttt$(i%) = "Height  "
            if str(sh_flags$(i%),3%,1%) = "R" then                     ~
                ttt$(i%) = "Radius  "
            if str(sh_flags$(i%),3%,1%) = "N" then                     ~
                ttt$(i%) = " (N/A)  "
            if str(sh_flags$(i%),3%,1%) = "L" then                     ~
                ttt$(i%) = "Leg Hght"
            if str(sh_flags$(i%),3%,1%) = "S" then                     ~
                ttt$(i%) = "LSideLeg"
            if str(sh_flags$(i%),3%,1%) = "T" then                     ~
                ttt$(i%) = "Top  Leg"
            if str(sh_flags$(i%),3%,1%) = "X" then                     ~
                ttt$(i%) = "SlegHght"
            if str(sh_flags$(i%),3%,1%) = "Z" then                     ~
                ttt$(i%) = "RSideLeg"

                                  /* Save Calc Fields for Bridge File */
            str(sh_bridge$,i%,1%) = str(sh_flags$(i%),3%,1%)
                                  /* Save Entry Fields for Calc       */
            str(sh_entry$,i%,1%)  = str(sh_flags$(i%),2%,1%)

           next i%
             check% = 1%
             str(sh_entry$,7%,1%) = "N"


       load_shape_prompts_done

       return

       verify_model_shape
         init(" ") readkey$, desc$
         check% = 0%
         str(readkey$,1%,9%)   = "PLNCNFIGM"   /* Configuration table */
         str(readkey$,10%,3%)  = sh_model$     /* Special Shape Model */
         str(readkey$,13%,2%)  = sh_config$    /* Shape Code          */
         str(readkey$,15%,2%)  = sh_config_seq$/* Shape seq. No. When */
                                                 /* Same Shape two Names*/
         gosub readGencdsDesc
          if found% = 0% then goto verify_model_shape_done

         sh_config_d$ = desc$                  /* Get Shape Descript  */
                                               /* Convert to Integer  */
         convert sh_config$ to sh_config%, data goto verify_model_shape_done


          check% = 1%
       verify_model_shape_done

       return


       convert_shape
                                        /* Converting the dimensions  */
                                        /* from the Part Number.      */
                                        /* Created by window Wizard   */
          a1, a2, shd(1%), shd(2%), shd(3%), shd(4%) = 0.0
          if str(sh_code$,2%,1%) = "?" then goto CS2
          convert str(sp_part$,13%,3%) to a1, data goto CS1
CS1:
          convert str(sp_part$,16%,1%) to a2, data goto CS2
CS2:
          shd(1%) = a1 + (a2/8.0)           /* Decimal Width    */

          a1 = 0.0 : a2 = 0.0
          if str(sh_code$,4%,1%) = "?" then goto CS4
          convert str(sp_part$,17%,2%) to a1, data goto CS3
CS3:
          convert str(sp_part$,19%,1%) to a2, data goto CS4
CS4:
          shd(2%) = a1 + (a2/8.0)           /* Decimal Height   */


          a1 = 0.0 : a2 = 0.0
          if str(sh_code$,6%,1%) = "?" then goto CS6
          convert str(sp_part$,20%,2%) to a1, data goto CS5
CS5:
          convert str(sp_part$,22%,1%) to a2, data goto CS6
CS6:
          shd(3%) = a1 + (a2/8.0)           /* Decimal Leg Height*/

                                              /* Fourth dimention is */
                                              /* in the 1st three digits*/
                                              /* of the Glass Text   */
          a1 = 0.0 : a2 = 0.0
          if str(sh_code$,8%,1%) = "?" then CS8
          convert str(txt$,1%,2%) to a1, data goto CS7
CS7:
          convert str(txt$,3%,1%) to a2, data goto CS8
CS8:
          shd(4%) = a1 + (a2/8.0)           /* Decimal Leg2 Height*/

       return

       calculate_special_shape
        if sh_config% = 64% and sh_config_seq$ = "02"             ~
                                               then shape_cross$ = "C0"


        call "EWDCALSS"   (sh_config%,   /* Shape Code                 */~
                          sh_model$,     /* Model Code                 */~
                          sh(),          /* Data Entry Values          */~
                          shc(),         /* Calculated Values          */~
                          sh_fields$,    /* Label Field Flags          */~
                          sh_position$,  /* Label Value Position       */~
                          sh_entry$,     /* Data Entry Field Name      */~
                          " ",           /* For Debug    dt_bar$       */~
                          shape_cross$,  /* 'SHAPCROSS' Code           */~
                          #4,            /* GENCODES File              */~
                          err% )         /* Error Code 0 = Ok, 0 <> err*/

           for k% = 1% to 7%
               gosub convert_fraction

           next k%

           gosub load_fields

           if err% <> 0% then goto print_error
       return

print_error:  if err% = 1% then                                           ~
                 errormsg$ = "Error Calulating the Width for (" &         ~
                 sh_config$ & ") " & dt_bar$

              if err% = 2% then                                           ~
                 errormsg$ = "Error Calculating the Height for (" &       ~
                 sh_config$ & ") " & dt_bar$

              if err% = 3% then                                           ~
                 errormsg$ = "Error Calculating the Radius for (" &       ~
                 sh_config$ & ") " & dt_bar$

              if err% = 4% then                                           ~
                 errormsg$ = "Error Calculating the Leg for (" &          ~
                 sh_config$ & ") " & dt_bar$

              if err% = 5% then                                           ~
                 errormsg$ = "Error No Data in (SHPHFOFF ) for Product - "~
                 & sh_model$

              if err% = 6% then                                           ~
                 errormsg$ = "Error No Equation for Shape (" & sh_config$ ~
                 & ") "

              if err% = 7% then                                           ~
                 errormsg$ = "Error Updating 'AWDSPECB' for Shape (" &    ~
                 sh_config$ & ") "

              if err% = 8% then                                           ~
                 errormsg$ = "Error Calculating the Diagonal for (" &     ~
                 sh_config$ & ") " & dt_bar$

              gosub error_prompt
        return

        load_fields
           init(" ") sh_width$, sh_height$, sh_radius$, sh_leg1$, ~
                 sh_leg2$, sh_leg3$, sh_leg4$, sh_leg5$, radius_flag$
           flag% = 0%
           for kk% = 1% to 6%

            if str(sh_fields$,kk%,1%) = "N" then goto next_load

            convert str(sh_position$,kk%,1%) to sh_position%,         ~
                                                    data goto next_load

            if str(sh_fields$,kk%,1%) = "W" then                ~
               sh_width$ = shc$(sh_position%)      /* Calc Width */

            if str(sh_fields$,kk%,1%) = "H" then                ~
               sh_height$ = shc$(sh_position%)       /* Calc Height*/

            if str(sh_fields$,kk%,1%) = "R" then                ~
               sh_radius$ = shc$(sh_position%)      /* Calc Radius*/

            if str(sh_fields$,kk%,1%) = "S" then                ~
               sh_leg1$ = shc$(sh_position%) /* Calc Left Side Leg*/

            if str(sh_fields$,kk%,1%) = "Z" then                ~
               sh_leg2$ = shc$(sh_position%) /* Calc Right Side Leg*/

            if str(sh_fields$,kk%,1%) = "T" then                ~
               sh_leg3$ = shc$(sh_position%)   /* Calc Top Leg    */

            if str(sh_fields$,kk%,1%) = "X" then                ~
               sh_leg4$ = shc$(sh_position%) /* Calc Side Leg Height */

            if str(sh_fields$,kk%,1%) = "L" then                ~
               sh_leg5$ = shc$(sh_position%)  /* Calc Leg Height */

            if str(sh_fields$,kk%,1%) = "W" or                  ~
                    str(sh_fields$,kk%,1%) = "H" then goto next_load
                    flag% = flag% + 1%
                    str(radius_flag$,flag%,1%) = str(sh_fields$,kk%,1%)

next_load:
           next kk%
           if radius_flag$ = " " then radius_flag$ = "N"
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
           shc$(k%) = wd1$
        return


        convert_sixteen
            calc = round( calc, 4 )
            a% = int(calc)
            b% = int((calc - a%) * 10000)
            if b% = 0% then goto no_dec                 /****************/
               d% = int(b%/625)                        /* Conversion of*/
               if mod(b%,625) <> 0 then d% = d% + 1%   /* Decimals to  */
                  b% = d%                              /*  Sixteen's   */
                  if b% <> 16% then goto no_dec         /****************/
                     a% = a% + 1% : b% = 0%         /* A% = WHOLE PART */
no_dec: return


        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return


        open_error                                    /* (EWD009)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD009)        */
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        print_header
            init(" ") rpt_time$ : call "TIME" (rpt_time$)
            pageno% = pageno% + 1%
            if lcnt% <> 99% then print using L55000
            print page
            print using L55000
            print using L55010, date$, rpt_time$, company$, pageno%
            print using L55020, beg_date$, title$, end_date$
            print using L55030
            print using L55040
            lcnt% = 5%

        return

        print_dtl
            if sc_type$ = "1" then goto print_dtl_temp /* (EWD015)     */
            if sc_type$ = "B" then goto print_dtl_temp /* (AWD037)     */
            if sc_type$ = "5" then goto print_dtl_temp /* (EWD015)     */
            if sc_type$ = "D" then goto print_dtl_temp /* (AWD037)     */
            if rpt% = 1% then gosub lookup_intercept

            if rpt% = 1% then goto L60130             /* Print labels  */
               if lcnt% > 55% then gosub print_header
               prt_flag% = 0%
               print using L55050
               lcnt% = lcnt% + 1%
L60130:     for kk% = 1% to ct%
                if len(ctt$(kk%,1%)) < 3 then goto L60170
                wd1$ = ctt$(kk%,1%)
                ht1$ = ctt$(kk%,2%)
/* SR64679 + */
                if rpt% = 1% then gosub check_size
REM  IF RPT% = 1% THEN GOSUB CHECKINTERCEPT                   /* (CR2055) */
/* SR64679 - */
                view$ = "TOP"
                chg$ = "N"
                if sp_stat$ > "00" then chg$ = "C"

                if kk% > 5% then view$ = "BOT"
                if rpt% = 1% then goto L60165         /* Print Labels   */

                if prt_flag% <> 0% then goto L60150
                  print using L55060, sp_cust$,sp_so$,sp_ln$,sp_due_date$,~
                                   sp_part$, sp_part_d$,sp_qty$, chg$,    ~
                                   view$, wd1$, ht1$
                prt_flag% = 1%
                goto L60160
L60150:     print using L55060, " ", " ", " ", " ", " ", " ", " ", " ",   ~
                               view$, wd1$, ht1$
L60160:         lcnt% = lcnt% + 1%
L60165:         if rpt% = 1% then gosub print_lab_temp
L60170:     next kk%
        return

        print_dtl_temp                               /* (EWD015)     */
            gosub build_dtl_rpt
        return
        print_dtl_temp_sort
            sc_type$ = " "

            init(" ") sp_temp_key$
            read #20, hold, key > sp_temp_key$, eod goto print_sort_done

                       goto print_sort_first
        print_sort_nxt
            read #20, hold, eod goto print_sort_done

print_sort_first

             gosub init_wrk1
/* (AWD028) */

             get #20, using L60210, intercept$, spType$, sp_rack$,        ~
                                 gdd$(view%),                             ~
                                 gdd$(spac%), tp_model$, wd$, ht$,        ~
                                 tp_glass$, tp_color$, sp_view$, t_k$,    ~
                                 muttin$,sp_warr$, sp_so$, sp_ln$, sp_temp$,~
                                 sp_temp1$, t5$, sandwich$(k%),           ~
                                 sp_lits$, view$, sp_due_date$, sp_part$, ~
                                 sp_part_d$, sp_qty$, chg$, sp_cust$,     ~
                                 wd1$, ht1$, field1$, field2$, field3$,   ~
                                 field4$, field5$, field6$, field7$,      ~
                                 field8$, field9$, field10$, field11$,    ~
                                 sp_so$, sp_ln$


                      delete #20

            if rpt% = 1% then goto print_temp_labels
               if lcnt% > 55% then gosub print_header
               prt_flag% = 0%
               print using L55050
               lcnt% = lcnt% + 1%

                if prt_flag% <> 0% then goto L60155
                   print using L55060,sp_cust$,sp_so$,sp_ln$,sp_due_date$,~
                                    sp_part$, sp_warr$&" - "&sp_part_d$,  ~
                                    sp_qty$, chg$, view$, wd1$, ht1$
                prt_flag% = 1%
                goto L60220
L60155:     print using L55060, " ", " ", " ", " ", " ", " ", " ", " ",   ~
                               view$, wd1$, ht1$
L60220:         lcnt% = lcnt% + 1%
                     goto print_sort_nxt
        print_temp_labels
         if rpt% = 1% then gosub print_lab_temp
                     goto print_sort_nxt
        print_sort_done
        return
        init_wrk1
            init(" ") sp_rack$, gdd$(), gdd$(), sp_part$, wd$, ht$, t_k$, ~
                      muttin$, sp_warr$, sp_so$, sp_ln$, sp_temp$, sp_temp1$,~
                      t5$, sandwich$(), sp_lits$, view$, sp_due_date$,    ~
                      sp_part_d$, sp_qty$, chg$, sp_cust$, wd1$, ht1$,    ~
                      csandwich$(), spType$ /*(AWD019) */ /*(AWD028) */
            init(" ") field1$, field2$, field3$, field4$, field5$,        ~
                      field6$, field7$, field8$, field9$, field10$,       ~
                      field11$, field17$
        return                                      /* (EWD015)      */

        lookup_description
            init(" ") sp_part_d$
            call "APCDESCR" (sp_part$, apc_scr$, apc_prt$, apc_sze$,   ~
                                                           #9, x_er% )
                                              /* (EWD003) - Begin    */
            s_23% = 0%                        /* Find Private Label  */
            s_23m$ = str(sp_part$,1%,3%)
            s_so$  = sp_so$                   /*                     */
            s_ln$  = sp_ln$                   /*                     */
            init(" ") s_prv$, s_1$, s_23$
            prv% = 1%
            call "APCPRZSB" (prv%, s_1$, sp_cust$, s_23m$, s_so$, s_ln$, ~
                           s_prv$, s_23$, s_23%, #7, #4, #12, #12, x_er% )
              if x_er% <> 0% then L60180
              if len(sp_part$) < 18% then L60180
                  str(apc_prt$,1%,8%)   = s_23$
                                                /* MFG Description     */
L60180:     sp_part_d$ = str(apc_prt$,1%,32%)
        return                                 /* (EWD003) - End       */

        gen_rpt1
            mode% = 1% : gosub open_work1
            mode% = 3% : gosub open_work1
            cnt% = 0%
            call "SHOSTAT" ("Special Tempered Glass Report")
            gosub select_printer

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60185  /* Print All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L60190
                                                  /* Only Print 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L60190
                                                  /* Only Print 'Blanks' */
L60185:            if rmk% = 0% then gosub loadHldKey   ~
                      else gosub loadHldRmkKey
                   if rmk% = 0% then gosub loadHldFromScreenDetail ~
                      else gosub loadHldRmkFromScreenDetail
                     if rec% = 0% then goto L60190
                      gosub dataloadHld
                      gosub calc_glass_size
                      gosub lookup_description
                      gosub print_dtl
L60190:     next dt%

           if sc_type$ = "1" then gosub print_dtl_temp_sort
           if sc_type$ = "B" then gosub print_dtl_temp_sort /* (AWD037) */
           print using L55000
           gosub close_printer
           if sc_status$ = "0" then gosub Plng_Reset
        return clear all
        goto inputmode

        select_printer
            init(" ") title$
            p% = pos(sc_type_d$ = "-")
            title$ = str(sc_type_d$,p% + 1%,9%)& " Special Glass Report"
            pageno% = 0%
            lcnt%    = 99%
            call "FMTTITLE" (title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            call "FMTTITLE" (company$, " ", 12%)
            call "SETPRNT" ("EWDGLS", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("EWDGLS", " ", 0%, 1%)
        return

        gen_labels
            mode% = 1% : gosub open_work1
            mode% = 3% : gosub open_work1
            rpt% = 1%
            cnt% = 0%
            call "SHOSTAT" ("Special Tempered Glass Labels")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60200  /* Print All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L61000
                                                  /* Only Print 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L61000
                                                  /* Only Print 'Blanks' */
L60200:            if rmk% = 0% then gosub loadHldKey   ~
                      else gosub loadHldRmkKey
                   if rmk% = 0% then gosub loadHldFromScreenDetail  ~
                      else gosub loadHldRmkFromScreenDetail

                     if rec% = 0% then goto L61000
                      gosub dataloadHld
                      gosub calc_glass_size
                      gosub print_dtl
L61000:     next dt%
            if sc_type$ = "1" then gosub print_dtl_temp_sort
            if sc_type$ = "B" then gosub print_dtl_temp_sort /*(AWD037) */
        return clear all
        goto inputmode

        print_lab_temp                         /* (RHHTEST)           */
           error%  = 0%
           yy_max% = 1%
           convert sp_qty$ to yy_max%, data goto L61010
L61010
           for yy% = 1% to yy_max%
               call "SHOSTAT" ("Building Label ")
               gosub build_lab_temp

REM !INIT(" ") LOGMESSAGE$
REM !LOGMESSAGE$ = "SO -> " & T7$ & "WD-HT -> " & T5$ & " INTER -> " & T10$
REM !CALL "LOGFILE" (LOGMESSAGE$)
                                                    /* (EWD008) - Begin */
               call "EWDPLC58" (t1$,               /* Model        (03)*/~
                                t2$,               /* Color Descr  (06)*/~
                                t3$,               /* Glass Type   (15)*/~
                                t4$,               /* Litin/Grid   (06)*/~
                                t5$,               /* Width & Heig (20)*/~
                                t6$,               /* Top/Bot      (03)*/~
                                t7$,               /* SO/Ln        (11)*/~
                                t8$,               /* SO Due Date  (08)*/~
                                t9$,               /* Contour Grid (01)*/~
                                t10$,              /* (AWD026) Ultra   */~
                                t11$,              /* (AWD030) Warranty*/~
                                t12$,              /* Glass Text CR2190*/~
                                #4,                /* (GENCODES)       */~
                                error%)            /* Return Code      */
                                                   /* (EWD008) - End   */
                if error% <> 0% then gosub L61020

           next yy%
        return
L61020:    errormsg$ = "(Error) Printing Glass Label for " & t7$
           gosub error_prompt
        return

        build_lab_temp
           t1$ = str(sp_part$,1%,3%)                /* Model Code     */
           tab% = 3%
           init(" ") code$ : code$ = str(sp_part$,4%,1%)/* Color      */
           gosub check_code
           t2$ = str(desc$,6%,6%)
           tab% = 4%
           init(" ") code$ : code$ = str(sp_part$,5%,2%)/* Glass      */
           gosub check_code
           t3$ = str(desc$,1%,15%)
           tab% = 5%
           init(" ") code$ : code$ = str(sp_part$,7%,2%)/* Liting     */
           gosub check_code
           p% = pos(desc$ = "-")
           if p% = 0% then p% = 4%
           l_lt$ = str(desc$,1%,p%-2%) & "   "   /* LEFT MUTTIN - TOP */
           r_lt$ = str(desc$,p%+2%,6%) & "   "   /* RIGHT MUTTIN - BOT*/
                                                 /* (EWD006) - Begin  */
           gosub get_muttin
           gosub change_muttin
           t4$ = str(mut$,1%,6%)
                                                 /* (EWD006) - End    */
           t5$ = wd1$ & " x " & ht1$                /* Width & Height */
           t6$ = view$                              /* View           */
           t7$ = sp_so$ & "-" & sp_ln$              /* Sales Ord/Line */
           t8$ = sp_due_date$                       /* Due Date       */
/*(PAR000) */
REM LK$ = STR(SP_PART$,12%,1%)               /* SET LOCK CODE  */
REM CALL "SHOSTAT" ("HERE ")  STOP
           tab% = 12%
           init(" ") code$ : code$ = field3$        /* GRDCOLOR  */
           gosub check_code
           p% = 0%
           p% = pos(desc$ = "-")
           t2$ = str(desc$,p%+2%,6%)
/* (AWD023) - END */

REM P%  = POS("0123456789" = LK$)
           if field1$ = "2" then p% = 1%            /* Contour */
           t9$ = " "
           if p% = 1% then t9$ = "C"                /*Has Contour Grid*/
                                                    /*  (EWD014)      */
           cl$ = " "
REM CL$ = STR(SP_PART$,4%,1%)
           p% = 0%
REM P% = POS("GHIJ" = CL$)
           if field2$ = "2" then p% = 1%            /* 3/4 Inch Grid */
           if p% <> 0% then t9$ = "W"
                                                    /*  (EWD014)      */
/* <AWD021> */
           if field1$ = "2" and field2$ = "1" then t9$ = "E"

/* <AWD023> */
           if field8$ = "1" then t9$ = "S"

/* (AWD033) */
           tab% = 21%
           init(" ") code$ : code$ = str(sp_part$,7%,2%)  /* Liting   */
           gosub check_code
           if code% = 1% then t9$ = "V"   /* Valance Grid */
/* (\AWD033) */


/* (AWD026) */
            t10$ = " "
REM IF SP_RACK$ = "2" OR SP_RACK$ = "3" THEN T10$ = "ULTRA"

            convert intercept$ to intercept%, data goto error4
error4:

            t10$ = interdesc$(intercept%)
/* (AWD030) */
            t11$ = sp_warr$
/* CR2190 */
            init(" ") t12$
           
            gosub lookup_txtid

            nbr_line% = 0%
            init(" ") txt$, text$()
            call "APCPLTXT" (#8, txtid$, text$(), nbr_line%)
            t12$     = str(text$(2%),1%,40%)       /* Obtain Glass Text  */
            
        return

        findUpdateStatus
          init(" ") updSchedSt$, updStatus$
           if sp_flag% <> 1% then goto L61048
              updSchedSt$  = "2"   /* Place items On-Order   */
              updStatus$   = "90"  /* (EWd002) - Do not Chg  */
              return

L61048:    if sp_flag% <> 2% then goto L61050
              updSchedSt$  = "4"   /* Received Items On-Order*/
              updStatus$   = "90"  /* (EWD002) - Do not Chg  */
              return

L61050:    if sp_flag% <> 3% then goto L61052
              updSchedSt$  = "Z"   /*Release Orders to Planning*/
              updStatus$   = "01"  /* (EWD002) - Do not Chg    */
/* (AWD030) */
              return

L61052:    if sp_flag% <> 5% then goto L61053
              updSchedSt$  = "3"   /*Moved to Tempered Order*/
              updStatus$   = "90"
              return

L61053:    if sp_flag% <> 6% then goto L61054
              updSchedSt$  = "0"   /*Moved to Order */
              updStatus$   = "01"
              return

L61054:    if sp_flag% <> 7% then goto L61055
              updSchedSt$  = "2"   /*Moved to Order */
              updStatus$ = "01"
              return
L61055:
        return

/* (AWD036) */
        thermalWarranty
          if sc_status$ <> "0" then return
          init(" ") warrTher$
REM WARRTHER$ = STR(SP_REC$,25%,14%)
REM STR(WARRTHER$,15%,4%) = STR(SP_REC$,70%,4%)
/*  (SR69095) */
           warrTher$ = sav_bar$

          read #35, key 1% = warrTher$, eod goto addTherWarr

              return
        addTherWarr
          gosub load_warranty

          put #35, using warrTherFmt, "003",         /* MFG Plant */ ~
                                   warrTher$,        /* Barcode   */ ~
                                   warrantyid$       /* Warrantyid*/

          write #35

          gosub update_warranty_file
        return
warrTherFmt:   FMT CH(03), CH(18), CH(08)

        update_warranty_file
             if bar_key$ = warrTher$ then return  /*Only Write One Record*/
             sp_unitid$ = unitid$(dt%)
             init(" ")bar_key$
             str(bar_key$,1%,18%) = warrTher$

             read #36, hold, key = bar_key$, eod goto no_barcode
                  delete #36
        no_barcode

             put #36, using L62650, bar_key$, warrantyid$,      ~
                                      unitid$

L62650:           FMT CH(18), CH(8), CH(10)

             write #36
        return

/* (\AWD036) */

/* (AWD027) */
        editScheduleHld                   /* Process Sales Order Line */
                                          /* Items (1) at a time.     */
           if rmk% = 1% then goto editSchedRmk     /* (IM8022)        */
           init(" ") hdKey$, savHd$, sav_ord$, sp_time$, sp_part$, sav_bar$
           hdKey$ = all(hex(00))
           call "TIME" (sp_time$)         /* Same time all Pieces     */
           str(hdKey$,1%,1%)  = sc_status$
           str(hdKey$,2%,1%)  = sc_type$
           str(hdKey$,3%,9%)  = str(dt$(dt%),18%,6%)    /* (CR893) */
           str(hdKey$,12%,8%) = str(dt$(dt%),25%,8%)
           str(hdKey$,20%,2%) = str(dt$(dt%),34%,2%)
REM  STR(HDKEY$,22%,4%) = STR(DT$(DT%),37%,4%)  /* (SR69095) */

           savHd$     = str(hdKey$,1%,21%)   /* Stat, Type, Cust, SO, Lne*/
           sav_ord$   = str(hdKey$,12%,8%)   /* Sales Order           */
           sav_line$  = str(hdKey$,12%,10%)  /* Sales Order/Line Item */
REM SAV_LN_ITM = STR(DT$(DT%),37%,4%) /* LINE QTY # OF(SR69095)*/
REM SAV_QTY    = STR(DT$(DT%),73%,4%) /* LINE QTY    (SR69095) */
           str(sav_bar$,1%,8%)  = sav_ord$
           str(sav_bar$,9%,2%)  = str(dt$(dt%),34%,2%)  /* (CR893) */
           str(sav_bar$,11%,4%) = str(dt$(dt%),78%,4%)
           str(sav_bar$,15%,4%) = str(dt$(dt%),72%,4%)

           hit% = 0%

L66030:    read #26, hold, key 1% > hdKey$, using L19500, hdRec$,       ~
                                                        eod goto L66045
           hdKey$ = str(hdRec$,16%,25%) /*(SR74117) len 22 instead of 21 */
           if savHd$ <> str(hdKey$,1%,21%) then goto L66045
              gosub check_apcplnor               /* See if Planned       */

              sp_st$ = str(hdRec$,7%,1%)
           if sp_flag% = 4% and sp_st$ <> "0" then goto L66030
                                            /* Can Only Delete      */
                                            /* special Sales Orders */

           if sp_flag% = 1% and sp_st$ <> "0" then goto L66030
                                            /* Can only Change      */
                                            /* Special Sales orders */

REM IF SP_FLAG% = 2% AND SP_ST$ <> "2" THEN GOTO L66030
           if sp_flag% = 2% and (sp_st$ <> "2" and sp_st$ <> "3") ~
                                                    then goto L66030
                                            /* Can only Receive     */
                                            /* Placed On-Orders     */

           if sp_flag% = 3% and sp_st$ <> "4" then goto L66030
                                            /* Can only Release     */
                                            /* Received Orders      */

           if sp_flag% = 5% and sp_st$ <> "2" then goto L66030

           if sp_flag% = 6% and (sp_st$ <> "2" and     ~
                sp_st$ <> "Z" and sp_st$ <> "4") then goto L66030

           if sp_flag% = 7% and sp_st$ <> "3" then goto L66030
          REM Debug
          REM call "SHOSTAT" ("(1)Deleting --> " & hdKey$)
          REM stop

           if or_st$ > "01" and or_st$ < "90" and sp_flag% <> 4%      ~
                                       then goto L66045

           if or_st$ = "99" and sp_flag% <> 4% then goto L66045
                                             /* Credit Hold, cannot */
                                             /* Place on Order, but */
                                             /* can be deleted      */
              delete #26
              if sp_flag% <> 4% then gosub modifyScheduleHld
              hit% = 1%                     /* Line Item Modified   */
              if gls_dtl% = 1% then return  /* (AWD030)    */
/* (AWD036) */
              if sc_type$ = "6" then gosub thermalWarranty
          goto L66030                                 /* Read Next */
L66045: return


/* (IM8022) */
        editSchedRmk
          gosub loadHldRmkKey

          read #3, hold, key 2% = hldrmkKey$, using L19500, hdRec$,       ~
                                                      eod goto noEditSchRmk

             delete #3

             if sp_flag% = 4% then return   /* SP_FLAG% = 4% -> Delete */

             gosub findUpdateStatus

             str(hdRec$,7%,1%)  = updSchedSt$
             str(hdRec$,16%,1%) = updSchedSt$
             str(hdRec$,53%,1%) = updSchedSt$
             str(hdRec$,90%,2%) = updStatus$
                                          /* Common to all Three (3)  */
             str(hdRec$,1%,6%)  = date
             str(hdRec$,92%,3%) = userid$
             str(hdRec$,95%,6%) = date
             str(hdRec$,101%,8%) = sp_time$
             put #3, using L19500, hdRec$

             write #3, eod goto rmkWrteErr


             if updSchedSt$ = "Z" then gosub update_apcplngr

        noEditSchRmk
        return
rmkWrteErr
           errormsg$ = "(Error)-Modifying (HLDSCRMK)" & hldrmkKey$
           gosub error_prompt
        return

        update_apcplngr
             init(" ") gr_key$
             str(gr_key$,1%,9%)  = sp_warr$(dt%)
             str(gr_key$,10%,3%) = sp_num$(dt%)
             read #10, hold, key = gr_key$, using APCPLNGR, gr_rec$(),    ~
                                                      eod goto no_glass

APCPLNGR:         FMT 4*CH(128)

                  str(gr_rec$(),13%,1%) = "0"
                  str(gr_rec$(),14%,8%) = sp_time$

                  delete #10

                  put #10, using APCPLNGR, gr_rec$()

                  write #10, eod goto no_glass
        return
        no_glass
            errormsg$ = "(Error) Updating (APCPLNGR)-> " & sp_warr$(dt%)
            gosub error_prompt
        return

/* (\IM8022) */

        modifyScheduleHld                 /* Each Line Item of S.O. */
           gosub findUpdateStatus

           str(hdRec$,7%,1%)  = updSchedSt$
           str(hdRec$,16%,1%) = updSchedSt$
           str(hdRec$,41%,1%) = updSchedSt$
           str(hdRec$,78%,2%) = updStatus$
                                          /* Common to all Three (3)  */
           str(hdRec$,1%,6%)  = date
           str(hdRec$,80%,3%) = userid$
           str(hdRec$,83%,6%) = date
           str(hdRec$,89%,8%) = sp_time$
           put #26, using L19500, hdRec$
        REM debug
        REM call "SHOSTAT" ("(2)Updating --> " & hdKey$ )
        REM stop

           write #26, eod goto L66060
        return
L66060:    errormsg$ = "(Error)-Modifying (HLDSCHED)" & savHd$
           gosub error_prompt
        return

/*(AWD027\)*/

        delete_specials
            sp_flag% = 4%
            call "SHOSTAT" (txt$(sp_flag%))         /* Only Selected    */
            for dt% = 1% to dt_max%
                if cc$(dt%) <> "X" then goto L61065 /* Only Delete 'X's */
                   gosub editScheduleHld            /* Delete all of the*/
L61065:     next dt%                                /* Line item        */
        if sc_status$ = "0" then gosub Plng_Reset
        return clear all
        goto inputmode

        update_specials           /* Process Place On-Order and Release */
            moveOrderBack% = 0%
            if sc_status$ = "0" then sp_flag% = 1% /* Place On-Order    */
            if sc_status$ = "2" then sp_flag% = 2% /* Received Specials */
            if sc_status$ = "2" and sp_type$ = "1" then sp_flag% = 5%
            if sc_status$ = "4" then sp_flag% = 3% /* Release SO To Plan*/
/* (AWD030) */
            if sc_status$ = "3" then sp_flag% = 2% /* Received Specials*/
            if sc_status$ = "2" and keyhit% = 10% then moveOrderBack% = 1%
            if sc_status$ = "Z" and keyhit% = 10% then moveOrderBack% = 1%
            if sc_status$ = "4" and keyhit% = 10% then moveOrderBack% = 1%
            if sc_status$ = "3" and keyhit% = 10% then moveOrderBack% = 2%
            if moveOrderBack% = 1% then sp_flag% = 6%
            if moveOrderBack% = 2% then sp_flag% = 7%

            call "SHOSTAT" (txt$(sp_flag%))
            for dt% = 1% to dt_max%
                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L61100
                                               /* Only Process 'X's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L61100
                                               /* Only process 'Blanks' */
                   gosub editScheduleHld       /* Each Line Item        */
                   if hit% = 0% then goto L61100
        REM debug
        REM call "SHOSTAT" ("(3)Update Planning --> " & sav_ord$ &"  "&sav_line$)
        REM stop
                      gosub update_apcplnor
                      gosub update_apcplnsc
/*(AWD030) don't believe write_awdschdt needs to be called for every status */
/*if it turns out to be so then just remove if stmt                         */
                   if sp_status$ <> "0" then goto L61100
/* (AWD037) */
REM IF SP_TYPE$ <> "1" AND SP_TYPE$ <> "B" THEN GOTO L61100
                   if sp_type$ = "1" or sp_type$ = "B" then goto TemperedUpdate
                   if sp_type$ = "5" or sp_type$ = "D" then goto TemperedUpdate
                      goto L61100
TemperedUpdate:
                   init(" ") sp_part$, subpart$, sp_lam$, sp_lamFl$
                   sp_part$ = str(dt$(dt%),46%,25%)
                   subpart$ = subpart$(dt%)
                   sp_lam$  = str(dt$(dt%),77%,1%)       /* (AWD037) */
                   dim1es   = dim1es(dt%)
                   dim2es   = dim2es(dt%)
                   if sp_lam$ = "S" then sp_lamFl$ = "L" /* (AWD037) */
                   if sp_type$ = "B" then gosub isTemp   /* (AWD037) */
                   if sp_type$ = "D" then gosub isTemp

                   gosub write_awdschdt
L61100:     next dt%                      /* (3%) Release Items On-Order */
            if sp_flag% = 3% then gosub release_specials
        if sc_status$ = "0" and sp_flag% = 1% then gosub Plng_Reset
        return clear all
        goto inputmode


        isTemp
           gosub check_shape_temp
        return


        check_apcplnor                        /* Check Planning Status */
            init(" ") or_st$, or_hows$, or_po$,      /* (AWD027) */ ~
                      or_cust$, or_region$  /*(CR893) */

            read #5,key 4% = sav_ord$, using L61105, or_region$, or_cust$,~
                      or_po$, or_st$, or_hows$, or_prefix$,   eod goto L61108

L61105:        FMT POS(09), CH(02), POS(27), CH(09), POS(36), CH(16), ~
                   POS(60), CH(2), POS(92), CH(2), POS(170), CH(01)

L61108: return

        update_apcplnor         /* Modify (APCPLNOR) Status Freeze     */
REM  RETURN             /* (EWD001) - TEMPORARY (REM)          */
            if sp_flag% <> 1% then return
               read #5,hold,key 4% = sav_ord$, eod goto L61120
L61110:           FMT POS(60), CH(2), CH(6)
               put #5, using L61110, "90", date
               rewrite #5
L61120: return

        update_apcplnsc         /* Modify (APCPLNSC) Status Freeze     */
        REM  return             /* (EWD001) - Temporary (REM)          */
            if sp_flag% <> 1% then return
            read #6,hold,key = sav_line$, eod goto L61140
L61130:        FMT POS(110), CH(2), CH(6)
            put #6, using L61130, "90", date
            rewrite #6
L61140: return

        release_specials
            init(" ") sav_order$, savHd$
            for dt% = 1% to dt_max%
              if sc_process$ = "0" and cc$(dt%) <> "X" then goto L61300
                                             /* Only Process 'X's     */
              if sc_process$ = "1" and cc$(dt%) = "X" then goto L61300
                                             /* Only process 'Blanks' */
               gosub releaseSpecialsHLDSCHED
               if check% = 1% then goto L61310
        REM Debug
        REM call "SHOSTAT" ("(4)All Line Items Released --> " & sav_order$)
        REM stop

                     gosub update_apcplnor_rel /* All of Sales Order */
                     gosub update_apcplnsc_rel
                     gosub update_sales_order  /* Must be Released   */
                     gosub updateHldsched      /* (EWD010) (AWD040)  */
L61300:     next dt%                           /* before release to  */
        return                                 /* Planning           */

L61310:     errormsg$ = "(Error) S.O. Not Released--> "& sav_order$
            gosub error_prompt
            goto L61300

        releaseSpecialsHLDSCHED
         init(" ") hdKey$
         hdKey$ = all(hex(00))
         str(hdKey$,1%,8%)  = str(dt$(dt%),25%,8%)
         if str(hdKey$,1%,8%) = str(sav_order$,10%,8%)             ~
                                             then goto noSoHLDSCHED
                                     /* All Lines on S.O. must */
                                     /* be Released before S.O.*/
REM Dont remove customer used in recalc due data
            str(sav_order$,1%,9%)  = str(dt$(dt%),18%,6%)  /* (CR893) */
            str(sav_order$,10%,8%) = str(dt$(dt%),25%,8%)
            check% = 0%
releaseHLDSCHEDNext:
            read #26, key 2% > hdKey$, using L61215, sp_status$, ~
                             hdKey$, sp_stat$, eod goto noSoHLDSCHED
L61215:     FMT POS(7), CH(1), POS(27), CH(16), POS(78), CH(2)

            if str(sav_order$,10%,8%) <> str(hdKey$,1%,8%)       ~
                                           then goto noSoHLDSCHED
               if sp_status$ <> "Z" and sp_status$ <> "0" then check% = 1%
            goto releaseHLDSCHEDNext
        noSoHLDSCHED
        return


        update_apcplnor_rel
REM  RETURN                           /* (EWD001) - TEMPORARY  */
                             /* (IM8022) do not update status on Remakes */
           if rmk% = 1% then return
           init(" ") newdue$, fob$, sp_cust$, dfltdue$, jdate$
           sp_cust$ = str(sav_order$,1%,9%)
           call "APCPLN3B" (dfltdue$, sp_cust$, fob$, #7, #4)
           gosub check_fob
           call "DATUFMTC" (dfltdue$)

                                                    /* (EWD010) - Begin  */
           if cut_off% <> 1% then goto L61440
           if str(sp_cust$,1%,2%) <> "LO" then goto L61440
             gosub check_plannocut
             if plannocut% = 0% then goto L61440
                                                  /* Convert Curr Date */
           u3%, j1% = 0%                         /* to Julian - JDATE$*/
           call "DATE" addr("GJ", str(dfltdue$,1%,6%), str(jdate$,1%,5%), u3%)
           call "DATJULCV" (jdate$)
           convert str(jdate$,5%,3%) to j1%, data goto L61430

L61430:    j1% = j1% - 7%
           convert j1% to str(jdate$,5%,3%), pic(000)
           call "DATJULCV" (jdate$)
           call "DATE" addr("JG", str(jdate$,1%,5%), str(dfltdue$,1%,6%), u3%)

L61440:                                                /* (EWD010) - End  */

           newdue$ = str(dfltdue$,1%,6%)
                                            /* Calculate a new Due date */
           init(" ") sav_ord$, or_rec$, errormsg$
           sav_ord$ = str(sav_order$,10%,8%)
           read #5,hold,key 4% = sav_ord$, using L61400, or_rec$,        ~
                                                          eod goto L61410
L61400:        FMT CH(170)
               delete #5
           str(or_rec$,1%,8%)  = newdue$           /* New Due Date     */
           str(or_rec$,62%,6%) = date
           str(or_rec$,60%,2%) = "01"              /* (EWD002)         */
           put #5, using L61400, or_rec$
           write #5, eod goto L61420
L61410: return
L61420:     errormsg$ = "(Error) Releasing S.O.-> "& sav_ord$
            gosub error_prompt
        return
        update_apcplnsc_rel
REM RETURN                             /* (EWD001) - TEMPORARY */
                           /* (IM8022) do not update status on Remakes */
            if rmk% = 1% then return
            init(" ") sc_key$, sc_rec$, errormsg$
            sc_key$ = sav_ord$
L61500:     read #6,hold,key > sc_key$, using L61510, sc_rec$,           ~
                                                         eod goto L61520
L61510:        FMT CH(128)
            sc_key$ = str(sc_rec$,24%,10%)
            if str(sc_key$,1%,8%) <> sav_ord$ then                       ~
                                                              goto L61520
               delete #6
               str(sc_rec$,1%,6%)   = newdue$
               str(sc_rec$,110%,2%) = "01"        /* (EWD002)      */
               str(sc_rec$,112%,6%) = date
               put #6, using L61510, sc_rec$
               write #6, eod goto L61530
               goto L61500
L61520: return                                    /* Done           */
L61530:     errormsg$ = "(Error) Releasing (APCPLNSC)-> " & sav_ord$
            gosub error_prompt
            goto L61500

        check_fob
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "PLAN DELV"
            str(readkey$,10%,15%) = str(fob$,1%,2%)
            read #4,key = readkey$, using L61540, desc$,eod goto L61550
L61540:        FMT POS(25), CH(30)
            str(fob$,3%,18%) = "/" & desc$
L61550: return

        update_sales_order                         /* 1st BCKMASTR     */
            init(" ") bck_key$
            bck_key$ = sav_order$
            read #11,hold,key = bck_key$, eod goto L61600

            put #11, using L61560, fob$, newdue$, newdue$
L61560:       FMT POS(442), CH(20), POS(818), 2*CH(6)
            rewrite #11
                                                   /* 2nd BCKLINES    */
            init(" ") bck_key$
            str(bck_key$,1%,8%) = sav_ord$
L61570:     read #12,hold,key > bck_key$, using L61580, bck_key$,        ~
                                                        eod goto L61600
L61580:        FMT POS(10), CH(19)
            if str(bck_key$,1%,8%) <> sav_ord$ then goto L61600
            put #12, using L61590, newdue$, newdue$, newdue$
L61590:        FMT POS(200), 3*CH(6)
            rewrite #12
            goto L61570
L61600: return

        updateHldsched
           init(" ") hdKey$, savHd$, sav_ord$, sp_time$
           hdKey$ = all(hex(00))
           call "TIME" (sp_time$)         /* Same time all Pieces     */

           str(hdKey$,1%,8%)  = str(dt$(dt%),25%,8%)
           str(hdKey$,9%,2%)  = str(dt$(dt%),34%,2%)
           str(hdKey$,11%,4%) = str(dt$(dt%),78%,4%)
           str(hdKey$,15%,1%) = sc_status$
           str(hdKey$,16%,1%) = sc_type$

           read #26, hold, key 2% = hdKey$, using L19500, hdRec$,       ~
                                                        eod goto L61690
            hdKey$ = str(hdRec$,16%,25%)

            delete #26

            str(hdRec$,1%,6%)  = date
            str(hdRec$,43%,6%) = newdue$
            str(hdRec$,80%,3%) = userid$
            str(hdRec$,83%,6%) = date
            str(hdRec$,89%,8%) = sp_time$
            put #26, using L19500, hdRec$

           write #26, eod goto L66060
L61690: return

        get_muttin                               /* (EWD006) - Fix  */
            vert% = 0% : horz% = 0% : er% = 0% : err% = 0%
            if str(sp_part$,7%,2%) <> "00" then goto L61700
               muttin$ = "        " : lits$ = "0"
               return

L61700:     call "APCGSLIT" ( sp_part$,         /* MFG Part Number     */~
                              muttin$,          /* Grid Vert/Horiz Code*/~
                              lits$,            /* No. of Lits         */~
                              str(view$,1%,1%), /* T or B              */~
                              vert%,            /* Number of Verticals */~
                              horz%,            /* Number of Horizontal*/~
                              #4,               /* (GENCODES)          */~
                              er% )             /* Error Code          */
            if er% = 0% then return
               if er% = 1% then err% = 7%       /* GED Lits Error      */
               if er% = 2% then err% = 8%       /* GED Hinge Error     */
               if er% = 3% then err% = 9%       /* GED Muttin Error    */
        return

        change_muttin
            lt% = 0%
            lt$ = str(sp_part$,7%,2%)
            convert lt$ to lt%, data goto L61800
L61800:
            mut$ = " "                         /* SWITCH VERT - HORZ */
            if len(muttin$) < 5 then goto L61810
               xx% = pos(muttin$ = "x")
               cc% = pos(muttin$ = "C")
               ii% = (cc% - xx%)
            mut$ = str(muttin$,xx%+1%,ii%) & "x" & str(muttin$,1%,xx%-1%)
L61810:     if lt% > 82% then mut$ = l_lt$
/* (AWD030) */
            if lt% = 99% then spErr$ = "998"
        return
                                               /* (EWD006) - End of Fix */

        check_plannocut                        /* (EWD010) - Temp       */
            plannocut% = 0%
            init(" ") readkey1$
            str(readkey1$,1%,9%)   = "PLANNOCUT"
            str(readkey1$,10%,15%) = str(sp_cust$,1%,6%)
            read #4,key = readkey1$, eod goto L61850

            plannocut% = 1%
L61850: return

        check_cutoff                           /*  (EWD013)  - Beg      */
            cutoff%, beg_cut%, end_cut%, cutoff_code% = 0%
            init(" ") cutoff_a$, cutoff_b$, readkey1$
REM STR(READKEY1$,1%,9%) = STR(SP_KEY$,16%,9%)
            str(readkey1$,1%,9%) = str(hdKey$,18%,9%)
/* CR2779 */
            if orderDate% = 1% then str(readkey1$,1%,9%) = str(hdKey1$,3%,9%)
            
            if paint% = 1% then str(readkey1$,1%,9%) = pcust$  /*(CR893)*/
        checkPaintCust    /*(CR893)*/
            read #7, key = readkey1$, using L62000, cutoff_a$, cutoff_b$,~
                                                        eod goto no_cutoff
L62000:          FMT POS(860), CH(2), POS(900), CH(2)


            convert beg_cut$ to beg_cut%, data goto no_cutoff

            convert end_cut$ to end_cut%, data goto no_cutoff

            beg_cut% = beg_cut% + 5%
            end_cut% = end_cut% + 5%

            convert cutoff_a$ to cutoff_code%, data goto no_cutoff

            if beg_cut% = cutoff_code% then cutoff% = 1%

            convert cutoff_b$ to cutoff_code%, data goto no_cutoff

            if end_cut% = cutoff_code% then cutoff% = 1%


        no_cutoff
        return                                 /*  (EWD013)  - End      */

        open_file                              /*  (EWD015)             */
            init(" ") library$, volume$, file$
            library$ = "TEMPERED"

* (AWD020) - Next two lines
            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"
            file$   = "AWDTEMP "

            ff% = 19%
            gosub openFile

            call "OPENFILE" (#21%, "IO   ", f2%(21%), rslt$(21%), axd$ )
            if f2%(21%) <> 0% then goto no_21existA
               close #21%

no_21existA:
/* (AWD019) Cardinal File */
            init(" ") library$, volume$, file$
            library$ = "TEMPERED"
            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"
            file$   = "AWDTEMP1"
            if sc_type$ = "B" then file$ = "AWDLAMN1"
            if sc_type$ = "5" then file$ = "AWDRMK1"
            if sc_type$ = "D" then file$ = "AWDRMKL"

            ff% = 21%
            gosub openFile
/* (AWD019) Cardinal File */

        return                                 /*  (EWD015)             */


        open_file_temp                        /* (AWD030)   */
            call "OPENFILE" (#21%, "IO   ", f2%(21%), rslt$(21%), axd$ )
            if f2%(21%) <> 0% then goto no_21existB

               close #21%

no_21existB:
            init(" ") library$, volume$, file$
            library$ = "TEMPERED"
            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"
            file$   = "AWDTEMP1"
            if sc_type$ = "B" then file$ = "AWDLAMN1"
            if sc_type$ = "5" then file$ = "AWDRMK1"        /* (IM8022) */
            if sc_type$ = "D" then file$ = "AWDRLAM"        /* (IM8022) */

            ff% = 21%
            gosub openFile
        return                                 /* (\AWD030)   */

        build_file                                       /*  (EWD015)  */
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            rpt% = 1%
            cnt% = 0%
            been_here% = 99%
            call "SHOSTAT" ("Build EDI File For Special Tempered Glass")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60305  /* Build All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L60400
                                                  /* Only Build 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L60400
                                                  /* Only Build 'Blanks' */
L60305:            gosub loadHldKey
                   gosub loadHldFromScreenDetail
                     if rec% = 0% then goto L60400
/* (AWD022) */
                      if sc_type$ = "6" or sc_type$ = "7"       ~
                                            then goto impact_order

                        gosub calc_glass_size
                        gosub build_dtl
                        goto L60400

impact_order

                        init(" ") so_inv$, item_no$, subpart$, infopart$
                        so_inv$  = sp_so$              /* Set SO         */
                        item_no$ = sp_ln$              /* Set Line       */
                        gosub lookup_sub_part
                        subpart$ = str(bcksubpt_rec$,48%,20%)
                        infopart$ = str(bcksubpt_rec$,132%,20%)

REM CALL "SHOSTAT" ("EWDPLD58" ) STOP

                        call "EWDPLD58" (sp_cust$, sp_so$, sp_ln$,        ~
                                         sp_ln_item$, sp_due$, sp_part$,  ~
                                         sp_qty$, subpart$, infopart$,    ~
                                         sc_type$, #4, #22, #23,          ~
                                         been_here%, err%)

L60400:     next dt%
            if sc_type$ = "6" or sc_type$ = "7" then goto impact_finished

            gosub write_dtl


           return clear all       /* (AWD026) - program was core dumping */
           goto inputmode         /* (AWD026) - program was core dumping */


impact_finished
            ff% = 22%
            if sc_type$ = "7" then ff% = 23%
            gosub write_totals
            gosub write_file_totals
        return clear all
        goto inputmode


        write_record
                put #ff%, using WRITE_FMT, write_rec$()
WRITE_FMT:           FMT 4*CH(256)

                write #ff%

        return

        write_totals
           init(" ") write_rec$()

           write_rec$() = "ORDERTOTALLINES|"
           convert line% to str(write_rec$(),17,2), pic(##)
           write_rec$() = write_rec$() & "||"

              gosub write_record
              line% = 0%
       return

       write_file_totals
           init(" ") write_rec$()

           write_rec$() = "FILETOTALLINES|"
           convert tot_line% to str(write_rec$(),17,4), pic(####)
           write_rec$() = write_rec$() & "||"

              gosub write_record
              tot_line% = 0%
       return


        build_file_shape                                   /*  (AWD024)  */
            gosub open_shapes_file
            gosub write_header
            rpt% = 1%
            cnt% = 0%
            seq_cnt% = 0%
            been_here% = 99%
            call "SHOSTAT" ("Build EDI File For Special Shapes SDL Glass")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60300  /* Build All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L62400
                                                  /* Only Build 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L62400
                                                  /* Only Build 'Blanks' */
L60300:            gosub loadHldKey
                   gosub loadHldFromScreenDetail
                     if rec% = 0% then goto L62400

                      gosub calc_glass_size_shape

                      seq_cnt% = seq_cnt% + 1%
                      gosub format_custom_record

                      gosub build_dtl_shape

L62400:     next dt%

        return clear all
        goto inputmode

        open_shapes_file
            call "OPENFILE" (#24%, "IO   ", f2%(24%), rslt$(24%), axd$ )
            if f2%(24%) <> 0% then goto no_exist
               gosub file_exists
               if comp% <> 16% then goto delete_file
                  call "FILEBGON" addr(#24%)
                  goto no_exist

delete_file:   close #24%
               call "OPENFILE" (#24%,"EXTND",f2%(24%),rslt$(24%),axd$ )
               return

no_exist:   str(rslt$(24%),1%,6%)  = "OUTPTP"
            str(rslt$(24%),7%,8%)  = "00001000"
            str(rslt$(24%),15%,3%) = "100"
            str(rslt$(24%),18%,3%) = "100"
            call "OPENFILE" (#24%,"OUTPT",f2%(24%), rslt$(24%), axd$ )
        return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%) = "The File (@SHAPE8@) Already Exists. "
            msg$(2%) = "             O P T I M I Z A T I O N             "
            msg$(3%) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        write_header
            str(cust_rec$,1%,9%)  = "Quantity "        /* Quantity       */
            str(cust_rec$,10%,1%) = ","

            str(cust_rec$,11%,13%) = "Ordered Seq. "  /* Ordered Seq No. */
            str(cust_rec$,24%,1%)  = ","

            str(cust_rec$,25%,13%) = "Ordered Date "    /* Ordered date  */
            str(cust_rec$,38%,1%) = ","


            str(cust_rec$,39%,8%) = "Sales Ord."        /* Sales order   */
            str(cust_rec$,47%,1%) = ","

            str(cust_rec$,48%,3%) = "Mod"              /* Model Code     */
            str(cust_rec$,51%,1%) = ","

            str(cust_rec$,52%,6%) = "Color "           /* Color Code     */
            str(cust_rec$,58%,1%) = ","

            str(cust_rec$,59%,6%) = "Grd Sz"           /* Grid Size      */
            str(cust_rec$,65%,1%) = ","

            str(cust_rec$,66%,4%) = "Grid"             /* Grid Code      */
            str(cust_rec$,70%,1%) = ","

            str(cust_rec$,71%,5%) = "Glass"            /* Glass Type     */
            str(cust_rec$,76%,1%) = ","
                                                   /* Shape Base (Width) */
            str(cust_rec$,77%,9%) = "Width    "        /* Shape Base Size*/
            str(cust_rec$,86%,1%) = ","
                                                       /* Shape Height   */
            str(cust_rec$,87%,9%) = "Height   "      /* Shape Height Size*/
            str(cust_rec$,96%,1%) = ","
                                                       /* Shape Radius   */
            str(cust_rec$,97%,4%) = "Flag"          /* Name of Field Flag*/
            str(cust_rec$,101%,1%) = ","

            str(cust_rec$,102%,9%) = "GlassSize"     /* Shape Radius Size*/
            str(cust_rec$,111%,1%) = ","

            str(cust_rec$,112%,4%) = "Flag"            /* Name of Field  */
            str(cust_rec$,116%,1%) = ","

            str(cust_rec$,117%,9%) = "GlassSize"       /* Shape Leg Size */
            str(cust_rec$,126%,1%)= ","

            str(cust_rec$,127%,4%)= "Face"             /* Facing L-R     */
            str(cust_rec$,131%,1%)= ","

            str(cust_rec$,132%,9%)= "Gls Thick"        /* Glass Thickness*/
            str(cust_rec$,141%,1%)= ","

            str(cust_rec$,142%,4%)= "Draw"             /* See Drawing Y/N*/
            str(cust_rec$,146%,1%)= ","
                                                      /* FGO Blank or 'F'*/
            str(cust_rec$,147%,3%)="FGO"
            str(cust_rec$,150%,1%)=","

            str(cust_rec$,151%,10%)= "Hub Adj 1 "     /* Hub 1 Adj       */
            str(cust_rec$,161%,1%) = ","

            str(cust_rec$,162%,10%)= "Hub Adj 2 "     /* hub 2 Adj       */
            str(cust_rec$,172%,1%) = ","

            str(cust_rec$,173%,10%)= "Hub Adj 3 "     /* Hub 3 Adj       */
            str(cust_rec$,183%,1%) = ","

            str(cust_rec$,184%,12%)= "  Barcode   "   /* Glass Barcode   */
            str(cust_rec$,196%,1%) = ","

                                                      /* Remake Number   */
            str(cust_rec$,197%,3%) = "RMK"
            str(cust_rec$,200%,1%) = ","
                                                      /* Custom Price    */
            str(cust_rec$,201%,7%) = " Price "
            str(cust_rec$,208%,1%) = ","
                                                      /* Grid Only Flag  */
            str(cust_rec$,209%,1%) = "G"
            str(cust_rec$,210%,1%) = ","
                                                      /* Growth   (45)   */
            str(cust_rec$,211%,45%) = " "
                                                      /* Record = 256    */

            write #24, cust_rec$, eod goto header_error

header_error:
        return


        format_custom_record
           init(" ") ct_rec$
           gosub check_grid_size
           gosub lookup_thickness
           gosub get_facing

           str(ct_rec$,1%,6%) = sp_status_dte$       /* Status Date      */

                                                      /* Counter Seq. No */
           convert seq_cnt% to str(ct_rec$,7%,5%), pic(00000)

                                                      /* Barcode         */
           str(ct_rec$,12%,12%)= sp_so$ & sp_ln$ & str(sp_ln_item$,3,2)

           str(ct_rec$,24%,8%)= sp_so$             /* Sales Order Number */

           str(ct_rec$,32%,3%)= str(sp_part$,1,3)   /* Model Code        */

           str(ct_rec$,35%,6%)= str(sp_part$,4,1)    /* Product Color    */

           str(ct_rec$,41%,3%)= "0" & sh_config$   /* Special Shape Code */

           str(ct_rec$,44%,3%)= str(sh_grd_size$,2,3)  /* Grid Size      */

           str(ct_rec$,47%,2%)= str(sp_part$,7,2)    /* Grid Code        */

           str(ct_rec$,49%,2%)= str(sp_part$,5,2)    /* Glass Code       */

           str(ct_rec$,51%,9%) = sh_width$          /* Glass Base Width  */

           str(ct_rec$,60%,9%)= sh_height$          /* Glass Height      */

           gosub find_radius
           str(ct_rec$,69%,1%)= field_flag$         /* Radius Flag       */

           str(ct_rec$,70%,9%)= field_value$        /* Radius size       */

           gosub find_leg
           str(ct_rec$,79%,1%)= field_flag$         /* Short Leg Flag    */

           str(ct_rec$,80%,9%) = field_value$

           str(ct_rec$,89%,1%)= face_flag$          /* Glass Facing      */

           str(ct_rec$,90%,6%)= sh_thickness$       /* Glass Thickness   */

           str(ct_rec$,96%,1%)= "N"                 /* Drawing Y or N    */

           str(ct_rec$,97%,1%) = "N"                /* F = FGO           */
           if str(sp_part$,11,1) = "6" then str(ct_rec$,97%,1%) = "F"

           str(ct_rec$,98%,27%)= " "                /* Hub 1, 2, 3       */

           str(ct_rec$,125%,1%)= "N"                /* Tempered Y or N   */
           if temp% = 1% then str(ct_rec$,125%,1%) = "Y"

           str(ct_rec$,126%,1%)= "Y"                  /* Grid Only       */

           str(ct_rec$,127%,8%)= "        "        /* Glass Price        */

           str(ct_rec$,135%,1%) = "Y"              /* Price Code Y,N,W   */

           str(ct_rec$,136%,2%) = "00"             /* Remake Reason Code */

           str(ct_rec$,138%,8%) = "        "       /* window Price       */

           str(ct_rec$,146%,8%) = "        "       /* Glass Pct of Price */

           str(ct_rec$,154%,6%) = " "              /* Glass Receive Date */

           str(ct_rec$,160%,8%) = " "              /* Glass Receive Time */

           str(ct_rec$,168%,3%) = "???"            /* Receive User Id    */

                                                   /* Barcode            */
           str(ct_rec$,171%,12%)= sp_so$ & sp_ln$ & str(sp_ln_item$,3,2)
                                                   /* (PAR000)           */
           str(ct_rec$,183%,1%) = contour$         /* Contour Grid Y/N   */

           str(ct_rec$,184%,1%) = "N"              /* Wood Surround (Y/N)*/

           str(ct_rec$,185%,10%)= "           "    /* Pricing Calculation*/

           str(ct_rec$,195%,25%)= "           "    /* MFG Part Number    */

           str(ct_rec$,220%,37%)= " "              /* Filler Area        */

           ct_price = 0.0
           err% = 0%
           call "AWDCALPR"   (been_here%,    /* Initialize Arrays       */~
                              "0",           /* Screen Selection        */~
                              ct_rec$,       /* Custom Data Record      */~
                              ct_price$,     /* Custom Glass Price      */~
                              #4,            /* GENCODES File           */~
                              " ",           /* AWDGLSCT Custom DB      */~
                              " ",           /* APCPLNDT Planning DB    */~
                              err% )         /* Err Code 0 = Ok, 0 <> err*/

           convert ct_price$ to ct_price, data goto L05300

L05300:
           convert ct_price to ct_price1$, pic(###.##-)

        return


        check_grid_size
           sh_type$ = "0"              /* No Grid         */
                                       /* 3/8 Inch Grid   */
           if field2$ = "4" then sh_type$ = "4"
                                       /* 5/8 Inch Grid   */
           if field2$ = "1" then sh_type$ = "3"

                                       /* 3/4 Inch Grid   */
           if field2$ = "2" then sh_type$ = "2"
                                       /* 1 Inch Grid     */
           if field1$ = "2" and          ~
                  field2$ = "3" then sh_type$ = "1"
                                       /* 18 mm           */
           if field1$ = "2" and          ~
                  field2$ = "1" then sh_type$ = "5"


           if field8$ = "1" then sh_type$ = "6"

                                       /* Contour Grid Y/N*/
           contour$ = "N"
           if sh_type$ = "1" then contour$ = "Y"
           if field1$ = "2" then contour$ = "Y"

           if sh_type$ = "1" then sh_grd_size$ = " 1    "
           if sh_type$ = "2" then sh_grd_size$ = " 3/4  "
           if sh_type$ = "3" then sh_grd_size$ = " 5/8  "
           if sh_type$ = "5" then sh_grd_size$ = " 18   "
           if sh_type$ = "6" then sh_grd_size$ = " SDL  "

        return

        lookup_thickness
           init(" ") readkey$, desc$
           str(readkey$, 1%,9%)  = "GED 002  "
           str(readkey$,10%,15%) = str(sp_part$,1,3)

           gosub readGencdsDesc
           if found% = 0% then goto lookup_thickness_done

           sh_thickness$= str(desc$,1%,6%)

        lookup_thickness_done

        return


        get_facing
           init(" ") readkey$, desc$, face_flag$
           str(readkey$,1%,9%)   = "PLN FACE "     /* Shape Facing      */
           str(readkey$,10%,15%) = sh_face$

           gosub readGencdsDesc
           if found% = 0% then goto get_facing_done

           face_flag$ = str(desc$,1%,1%)
        return

        get_facing_done
           face_flag$ = "N"
        return


        build_dtl_shape
            init(" ") cust_rec$

            convert sp_qty$ to sp_qty%, data goto bad_sp_qty

bad_sp_qty:

            convert sp_qty% to sp_qty$, pic(###0)

            str(cust_rec$,1,4)   = sp_qty$                 /* Quantity */
            str(cust_rec$,5,1)   = ","

            str(cust_rec$,6,5)   = str(ct_rec$,7,5)        /* Seq  */
            str(cust_rec$,11,1)  = ","

            str(cust_rec$,12,10) = sp_status_date$        /* Status date */
            str(cust_rec$,21,1)  = ","

            str(cust_rec$,22,8) = str(ct_rec$,24,8)      /* Sales Order */
            str(cust_rec$,30,1) = ","

            str(cust_rec$,31,3) = str(ct_rec$,32,3)      /* Model */
            str(cust_rec$,34,1) = ","

            str(cust_rec$,35,6) = sh_grid_color$         /* Color Desc */
            str(cust_rec$,41,1) = ","

            str(cust_rec$,42,6) = sh_grd_size$          /* Grd Size desc */
            str(cust_rec$,48,1) = ","

            str(cust_rec$,49,4) = str(ct_rec$,47,2)      /* Grid code  */
            str(cust_rec$,53,1) = ","

            str(cust_rec$,54,5) = str(ct_rec$,49,2)      /* Glass Code */
            str(cust_rec$,59,1) = ","

            str(cust_rec$,60,9) = sh_width$              /* Shape Width */
            str(cust_rec$,69,1) = ","                    /* Base */

            str(cust_rec$,70,9) = sh_height$             /* Shape Height */
            str(cust_rec$,79,1) = ","


            gosub find_radius
            if field_flag$ = " " or field_value$ = " " then ~
                                          field_flag$ = "N"
            str(cust_rec$,80,4) = field_flag$       /* Shape Radius Flag */
            str(cust_rec$,84,1) = ","

            str(cust_rec$,85,9) = field_value$      /* Shape Radius Value*/
            str(cust_rec$,94,1) = ","

            gosub find_leg
            if field_flag$ = " " or field_value$ = " " then ~
                                          field_flag$ = "N"
            str(cust_rec$,95,4) = field_flag$          /* Name Of field */
            str(cust_rec$,96,1) = ","

            str(cust_rec$,97,9) = field_value$    /* Name of Field Value*/
            str(cust_rec$,106,1) = ","

            str(cust_rec$,107,4) = face_flag$           /* Facing Flag */
            str(cust_rec$,111,1) = ","

            str(cust_rec$,112,9) = sh_thickness$       /* Glass Thickness*/
            str(cust_rec$,121,1) = ","

            str(cust_rec$,122,4) = str(ct_rec$,96,1)    /* See Drawing */
            str(cust_rec$,126,1) = ","

            str(cust_rec$,127,3) = str(ct_rec$,97,1)    /* FGO */
            str(cust_rec$,130,1) = ","

            str(cust_rec$,131,10) = " "                 /* Hub1 Adj */
            str(cust_rec$,141,1) = ","

            str(cust_rec$,142,10) = " "                 /* Hub2 Adj */
            str(cust_rec$,152,1) = ","

            str(cust_rec$,153,10) = " "                 /* Hub3 Adj */
            str(cust_rec$,163,1) = ","

            str(cust_rec$,164,12) = str(ct_rec$,12,12)  /* Barcode */
            str(cust_rec$,176,1) = ","

            str(cust_rec$,177,3) = " "                  /* Remake Num */
            str(cust_rec$,180,1) = ","

            str(cust_rec$,181,7) = ct_price1$           /* Price */
            str(cust_rec$,188,1) = ","

            str(cust_rec$,189,1) = str(ct_rec$,126,1)   /* Grid Only */
            str(cust_rec$,190,1) = ","

            str(cust_rec$,191,65) = " "

            write #24, cust_rec$, data goto detail_done

detail_done:
        return

        find_radius                                 /* Shape Codes 00, 05, 06 */
                                                    /* 07, 63 No third Value  */
           init(" ") field_flag$, field_value$
           if sh_config$ = "01" then goto find_leg   /* Has a third Value*/
           if sh_config$ = "02" then goto find_leg   /* Has a third Value*/
           if sh_config$ = "03" then goto find_leg   /* Has a third Value*/
           if sh_config$ = "04" then goto find_leg   /* Has a third Value*/
           if sh_config$ = "15" then goto find_leg   /* Has a third Value*/
           if sh_config$ = "25" then goto find_leg   /* Has a third Value*/

           p% = 0%
           p% = pos(radius_flag$ = "R")
           if p% = 0% then goto find_radius_done
           field_flag$ = "R"

           if field_flag$ <> "R"   then goto find_radius_done
           field_value$ = sh_radius$
        return

        find_radius_done
           field_flag$ ="N"
        return

        find_leg                  /* Leg Height */
           init(" ") field_flag$, field_value$
           p% = 0%
           p% = pos(radius_flag$ = "L")
           if p% = 0% then goto find_leg_1
              field_flag$  = "L"
              field_value$ = sh_leg5$
        return

        find_leg_1                /* Left Side Leg */
           p% = 0%
           p% = pos(radius_flag$ = "S")
           if p% = 0% then goto find_leg_2
              field_flag$  = "S"
              field_value$ = sh_leg1$
        return

        find_leg_2               /* Right Side Leg */
           p% = 0%
           p% = pos(radius_flag$ = "Z")
           if p% = 0% then goto find_leg_3
              field_flag$  = "Z"
              field_value$ = sh_leg2$
        return

        find_leg_3               /* Top Leg        */
           p% = 0%
           p% = pos(radius_flag$ = "T")
           if p% = 0% then goto find_leg_4
              field_flag$  = "T"
              field_value$ = sh_leg3$
        return

        find_leg_4              /* Side Leg Height */
           p% = 0%
           p% = pos(radius_flag$ = "X")
           if p% = 0% then goto find_leg_5
              field_flag$  = "X"
              field_value$ = sh_leg4$
        return

        find_leg_5
           field_flag$ = "N"
           field_value$ = " "
        return

                                                      /* (AWD024) - end */

        build_dtl
             init(" ") wd1$, ht1$, t5$, sp_lits$, sp_temp$

             gosub get_muttin
             gosub change_muttin
             gosub check_thickness
             sp_lits$ = "1"
             sp_qty% = 1%
             savType% = spType%                           /* (AWD028) */

             convert sp_qty$ to sp_qty%, data goto L60550
L60550:
             for j% = 1% to sp_qty%
               for n% = 1% to ct%         /* n = individual glass panels */

                  view% = 6%
                  if n% > 5% then view% = 12%    /*view% for spacer desc */
                  spac% = 3%
                  if n% > 5% then spac% = 9%     /*view% for spacer desc */

                  view$ = "TOP"
                  if n% > 5% then view$ = "BOT"  /*n% for number of panel*/
                  if len(ctt$(n%,1%)) < 3 then goto L60590
                  sp_warr$ = warrantyid$              /* (AWD030) */
                  convert (n%-1%) to str(sp_warr$,9%,1%), pic(0)

                  wd1$ = ctt$(n%,1%)
                  ht1$ = ctt$(n%,2%)
                  t5$ = wd1$ & " x " & ht1$            /* Width & Height */
REM                  GOSUB CHECK_ULTRA                       /* (AWD026) */
                  gosub lookup_intercept                    /* (AWD029) */

                  gosub check_size
/* SR64679 */
REM  GOSUB CHECKINTERCEPT                                      /* (CR2055) */
                  gosub lookup_sandwich
/*(AWD028) */
REM  IF P% = 0% THEN GOTO L60590        /* SKIP SS NO TEMP GLS */
                  spType$ = "000"
                  convert spType% to spType$, pic(000)
/*(AWD028/) */
                              /* K = individual pieces or lites of glass */
                  for k% = 1% to 2%
                      convert n%    to sp_temp$, pic(00)
                      convert j%    to sp_temp1$, pic(000)
                      convert k%    to sp_temp2$, pic(00)

                      init(" ") sp_temp_key$
                      str(sp_temp_key$,1%,2%)  = intercept$  /*InterceptType*/
                      str(sp_temp_key$,3%,3%)  = spType$     /* (AWD028) */
                      str(sp_temp_key$,6%,1%)  = sp_rack$    /* RackType0or1*/

                      str(sp_temp_key$,7%,10%) = gdd$(view%) /*SpacerDesc*/
                      str(sp_temp_key$,17%,10%)= gdd$(spac%) /*SpacerThck*/
                      str(sp_temp_key$,27%,3%) = str(sp_part$,1%,3%)

                      str(sp_temp_key$,30%,9%) = wd$
                      str(sp_temp_key$,39%,8%) = ht$
                      str(sp_temp_key$,47%,2%) = str(sp_part$,5%,2%)
                      str(sp_temp_key$,49%,1%) = str(sp_part$,4%,1%)
                      sp_view$ = "0"
                      if str(view$,1%,1%) = "B" then sp_view$ = "1"
                      str(sp_temp_key$,50%,1%) = sp_view$

                      str(sp_temp_key$,51%,6%) = t_k$
                      str(sp_temp_key$,57%,8%) = muttin$
                      str(sp_temp_key$,65%,9%) = sp_warr$
REM  ! STR(SP_TEMP_KEY$,74%,8%) = SP_SO$
REM  ! STR(SP_TEMP_KEY$,82%,2%) = SP_LN$
                      str(sp_temp_key$,84%,2%) = sp_temp$   /* CntGlsPnl*/
                      str(sp_temp_key$,86%,3%) = sp_temp1$  /* Piece */
                      str(sp_temp_key$,89%,2%) = sp_temp2$  /* Piece */

                      read #18, hold, key = sp_temp_key$, eod goto no_temp
                             delete #18
                      no_temp
/*(AWD028) add type */
REM  ! WRITE #18, USING L60500, INTERCEPT$, SPTYPE$, SP_RACK$, ~
                      GDD$(VIEW%),                                ~
                      GDD$(SPAC%), STR(SP_PART$,1%,3%), WD$,      ~
                      HT$,STR(SP_PART$,5%,2%),STR(SP_PART$,4%,1%),~
                      SP_VIEW$, T_K$, MUTTIN$, SP_WARR$, SP_SO$,  ~
                      SP_LN$,SP_TEMP$,SP_TEMP1$, SP_TEMP2$, T5$,  ~
                      SANDWICH$(K%), SP_LITS$, VIEW$, SP_FIL$,    ~
                      WD1$, HT1$, CSANDWICH$(K%), " "

               write #18, using L60500, intercept$,                   ~
                                        spType$,                      ~
                                        sp_rack$,                     ~
                                        gdd$(view%),                  ~
                                        gdd$(spac%),                  ~
                                        str(sp_part$,1%,3%),          ~
                                        wd$,                          ~
                                        ht$,                          ~
                                        str(sp_part$,5%,2%),          ~
                                        str(sp_part$,4%,1%),          ~
                                        sp_view$,                     ~
                                        t_k$,                         ~
                                        muttin$,                      ~
                                        sp_warr$,                     ~
                                        " ",                          ~
                                        " ",                          ~
                                        sp_temp$,                     ~
                                        sp_temp1$,                    ~
                                        sp_temp2$,                    ~
                                        t5$,                          ~
                                        sandwich$(k%),                ~
                                        sp_lits$,                     ~
                                        view$,                        ~
                                        sp_fil$,                      ~
                                        wd1$,                         ~
                                        ht1$,                         ~
                                        csandwich$(k%),               ~
                                        sp_so$,                       ~
                                        sp_ln$,                       ~
                                        nxtOrder$,                    ~
                                        " "                  /* FILLER */
                                            /* (AWD019) */
/* Put Original valuTe back in case of error (AWD028)*/
                    spType% = savType%

                  next k%
L60590:        next n%

L60500:     FMT CH(2), CH(3), CH(1), CH(10), CH(10), CH(3), CH(9), CH(8), ~
                CH(2), CH(1), CH(1), CH(6), CH(8), CH(9), CH(8), CH(2),   ~
                CH(2), CH(3), CH(2), CH(20), CH(30), CH(1), CH(3), CH(7), ~
                CH(9), CH(8), CH(30), CH(08), CH(02), CH(05), CH(43)


             next j%
        return

        check_size                /* (AWD029) change sp_rack to integer */
             sp_rack$ = "1"      /* (AWD019) Change default from 1 to 0*/
             wd, ht = 0.00
             convert dcc$(n%,1%) to wd, data goto L60510

L60510:
             convert dcc$(n%,2%) to ht, data goto L60520

L60520:
            if wd <= 38.0 and ht <= 68.0 then sp_rack$ = "0"
            if wd <= 68.0 and ht <= 38.0 then sp_rack$ = "0"
            if wd <= 6.0 and ht <= 12.0 then sp_rack$ = "1"
            if wd <= 12.0 and ht <= 6.0 then sp_rack$ = "1"

/*(AWD019) Change Rack Sizes */
/* (AWD026) */
/* (AWD029) */
        return

        lookup_sandwich
             init(" ") code$, sp_sand1$, sp_sand2$, sandwich$(), tpSand$, ~
                       sp_igstr1$, sp_igstr2$
             spErr$ = "000"
             temp%     = 2%
             if view$ = "BOT" then temp% = 8%

             sp_sand1$ = str(gdd$(temp%),4%,2%)
             sp_sand2$ = str(gdd$(temp%),7%,2%)
/*(AWD028)*/
             tpSand$ = gdd$(temp%)
             gosub check_special_sand
/*(AWD028) remove not tempered from order file */
/*(AWD030) */
             if temp_spec% = 1% and view$ = "TOP" and topTemp% = 0% ~
                                                then goto notTempered
             if temp_spec% = 1% and view$ = "BOT" and botTemp% = 0% ~
                                                then goto notTempered

             p%  = pos(tpSand$ = "T")
             p1% = pos(tpSand$ = "PR")
             p2% = pos(tpSand$ = "EJ")
             p2% = pos(tpSand$ = "EP")
/*(AWD037) */
             if schema% <> 1% then goto notNCSand

                if p%  <> 0% then goto TempSand
                if p1% <> 0% then goto TempSand
                if p2% <> 0% then goto TempSand
                    goto notTempered

notNCSand:
REM       NOT NC; this logic is for NTX
               if p%  <> 0% then goto TempSand
               if p1% <> 0% then goto TempSand
REM       NTX Skips Lamn
               if p2% <> 0% then goto notTempered

TempSand:
REM Called from display_analysis_glass
validate_sand:

             tab% = 7%              /* PLAN SAND */
             code$ = sp_sand1$
/* (AWD037) */
             if sc_type$ = "B" then code$ = sp_sand1$ & sp_lamFl$
             if sc_type$ = "D" then code$ = sp_sand1$ & sp_lamFl$
             gosub check_code
             str(sandwich$(1%),1%,30%) = str(desc$,1%,30%)

             init(" ") code$
             code$ = sp_sand2$
             gosub check_code
             str(sandwich$(2%),1%,30%) = str(desc$,1%,30%)


/* (AWD019)  */

             tab% = 11%             /* PLANCSAND */
             init(" ") code$ 
             code$ = strength$ & sp_sand1$      /* (AWD028) */
/* (AWD037) */
             if sc_type$ = "B" then code$ = strength$ & sp_sand1$ & sp_lamFl$
             if sc_type$ = "D" then code$ = strength$ & sp_sand1$ & sp_lamFl$
             if spType% = 16% then code$ = sp_igstr1$ & sp_sand1$ /*(CR1173)*/
	 
             gosub check_code
             str(csandwich$(1%),1%,30%) = str(desc$,1%,30%)

             init(" ") code$
             code$ = strength$ & sp_sand2$      /* (AWD028) */
             if spType% = 16% then code$ = sp_igstr2$ & sp_sand2$ /*(CR1173)*/
             gosub check_code
             str(csandwich$(2%),1%,30%) = str(desc$,1%,30%)


             if str(csandwich$(1%),1%,30%) = " " then goto noSandwich
             if str(csandwich$(2%),1%,30%) = " " then goto noSandwich

       return
/*(AWD028) */
noSandwich:
           if p2% <> 0% then goto LamnSand
           spErr$ = "997"
           if str(csandwich$(1%),1%,30%) = " " then ~
               str(csandwich$(1%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$

           if str(csandwich$(2%),1%,30%) = " " then ~
               str(csandwich$(2%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$

        return
LamnSand:
           spErr$ = "000"
           if str(csandwich$(1%),1%,30%) = " " then   ~
               str(csandwich$(1%),1%,30%) = "0" & "-" ~
               & str(sp_part$,5,2) & " " & tpSand$

           if str(csandwich$(2%),1%,30%) = " " then   ~
               str(csandwich$(2%),1%,30%) = "0" & "-" ~
               & str(sp_part$,5,2) & "  " & tpSand$

        return
notTempered:
           spErr$ = "999"
           str(csandwich$(1%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$
           str(csandwich$(2%),1%,30%) = str(sp_part$,5,2) & "  " & tpSand$
        return
/*(AWD028\) */
        check_special_sand
             topTemp%, botTemp% = 0%
             temp_spec% = 0%
             tab% = 8%              /* TEMP GED */
             code$ = str(sp_part$,5%,2%)
             gosub check_code
             if code% = 0% then return
             temp_spec% = 1%
             convert str(desc$,15%,1%) to topTemp%, data goto badTopTemp

badTopTemp:
             sp_igstr1$ = str(desc$,3%,1%)
             sp_sand1$ = str(desc$,4%,2%)
             sp_igstr2$ = str(desc$,6%,2%)
             sp_sand2$ = str(desc$,7%,2%)
             tpSand$ = str(desc$,1,10)

             if view$ <> "BOT" then return
             convert str(desc$,30%,1%) to botTemp%, data goto badBotTemp

badBotTemp:
             sp_igstr1$ = str(desc$,20%,1%)
             sp_sand1$ = str(desc$,21%,2%)
             sp_igstr2$ = str(desc$,23%,2%)
             sp_sand2$ = str(desc$,24%,2%)
             tpSand$ = str(desc$,18,10)
        return

        lookup_sandwich_temp
             init(" ") code$, sp_sand1$, sp_sand2$, sandwich$(), tpSand$,~
                       model$, ty$, sp_sand3$, hg$, sp_igstr1$, sp_igstr2$
             contHead% = 0%
             gedtopbot% = 0%
             combogls%  = 0%
             slider% = 0%
             spErr$ = "000"

             model$ = str(sp_part$,1%,3%)
             ty$    = str(sp_part$,5%,2%)
             hg$    = str(sp_part$,9%,2)
             if ty$ = "LA" then combogls% = 1%
             if ty$ = "LB" then combogls% = 1%
             if ty$ = "LC" then combogls% = 1%
             if ty$ = "LE" then combogls% = 1%
             if ty$ = "RA" then combogls% = 1%
             if ty$ = "RB" then combogls% = 1%
             if ty$ = "RC" then combogls% = 1%
             if ty$ = "RE" then combogls% = 1%
/* (AWD034) */
             if ty$ = "BT" then combogls% = 1%
             if ty$ = "BU" then combogls% = 1%
             if ty$ = "BV" then combogls% = 1%
             if ty$ = "BW" then combogls% = 1%
/* (AWD034) */
/* + (CR1988) */
             if ty$ = "1B" then combogls% = 1%
             if ty$ = "1C" then combogls% = 1%
             if ty$ = "2J" then combogls% = 1%
             if ty$ = "2K" then combogls% = 1%
             if ty$ = "2N" then combogls% = 1%
             if ty$ = "2P" then combogls% = 1%
             if ty$ = "2R" then combogls% = 1%
             if ty$ = "2S" then combogls% = 1%
             if ty$ = "2V" then combogls% = 1%
             if ty$ = "2W" then combogls% = 1%
             if ty$ = "2Z" then combogls% = 1%
             if ty$ = "35" then combogls% = 1%
             if ty$ = "38" then combogls% = 1%
             if ty$ = "39" then combogls% = 1%
             if ty$ = "3R" then combogls% = 1%
             if ty$ = "3S" then combogls% = 1%
             if ty$ = "3V" then combogls% = 1%
             if ty$ = "3W" then combogls% = 1%
             if ty$ = "3Z" then combogls% = 1%
             if ty$ = "45" then combogls% = 1%
             if ty$ = "48" then combogls% = 1%
             if ty$ = "49" then combogls% = 1%
             if ty$ = "4D" then combogls% = 1%
             if ty$ = "4E" then combogls% = 1%
             if ty$ = "4H" then combogls% = 1%
             if ty$ = "4J" then combogls% = 1%
             if ty$ = "4M" then combogls% = 1%
             if ty$ = "4N" then combogls% = 1%
             if ty$ = "4R" then combogls% = 1%
             if ty$ = "4S" then combogls% = 1%
             if ty$ = "5E" then combogls% = 1%
             if ty$ = "5F" then combogls% = 1%
             if ty$ = "5J" then combogls% = 1%
             if ty$ = "5K" then combogls% = 1%
             if ty$ = "5N" then combogls% = 1%
             if ty$ = "5P" then combogls% = 1%
             if ty$ = "5S" then combogls% = 1%
             if ty$ = "5T" then combogls% = 1%
             if ty$ = "5W" then combogls% = 1%
             if ty$ = "5X" then combogls% = 1%
             if ty$ = "6A" then combogls% = 1%
             if ty$ = "6C" then combogls% = 1%
             if ty$ = "6F" then combogls% = 1%
             if ty$ = "6G" then combogls% = 1%
             if ty$ = "6K" then combogls% = 1%
             if ty$ = "6L" then combogls% = 1%
             if ty$ = "6P" then combogls% = 1%
             if ty$ = "6Q" then combogls% = 1%
             if ty$ = "6T" then combogls% = 1%
             if ty$ = "6U" then combogls% = 1%
             if ty$ = "6X" then combogls% = 1%
             if ty$ = "6Y" then combogls% = 1%
             if ty$ = "74" then combogls% = 1%
             if ty$ = "75" then combogls% = 1%
             if ty$ = "78" then combogls% = 1%
             if ty$ = "79" then combogls% = 1%
             if ty$ = "7D" then combogls% = 1%
             if ty$ = "7E" then combogls% = 1%
             if ty$ = "7H" then combogls% = 1%
             if ty$ = "7J" then combogls% = 1%
             if ty$ = "7M" then combogls% = 1%
             if ty$ = "7N" then combogls% = 1%
             if ty$ = "7R" then combogls% = 1%
             if ty$ = "7S" then combogls% = 1%
             if ty$ = "7V" then combogls% = 1%
             if ty$ = "7W" then combogls% = 1%
             if ty$ = "8L" then combogls% = 1%
             if ty$ = "8M" then combogls% = 1%
             if ty$ = "8Q" then combogls% = 1%
             if ty$ = "8R" then combogls% = 1%
             if ty$ = "8U" then combogls% = 1%
             if ty$ = "8V" then combogls% = 1%
             if ty$ = "8Y" then combogls% = 1%
             if ty$ = "8Z" then combogls% = 1%
             if ty$ = "92" then combogls% = 1%
             if ty$ = "93" then combogls% = 1%
             if ty$ = "96" then combogls% = 1%
             if ty$ = "97" then combogls% = 1%
             if ty$ = "9C" then combogls% = 1%
             if ty$ = "9D" then combogls% = 1%
             if ty$ = "9G" then combogls% = 1%
             if ty$ = "9H" then combogls% = 1%
             if ty$ = "9L" then combogls% = 1%
             if ty$ = "9M" then combogls% = 1%
             if ty$ = "9U" then combogls% = 1%
             if ty$ = "9V" then combogls% = 1%
             if ty$ = "9Y" then combogls% = 1%
             if ty$ = "9Z" then combogls% = 1%
             if ty$ = "AT" then combogls% = 1%
             if ty$ = "BA" then combogls% = 1%
             if ty$ = "BH" then combogls% = 1%
             if ty$ = "BJ" then combogls% = 1%
             if ty$ = "BN" then combogls% = 1%
             if ty$ = "BR" then combogls% = 1%
             if ty$ = "DA" then combogls% = 1%
             if ty$ = "DC" then combogls% = 1%
             if ty$ = "DS" then combogls% = 1%
             if ty$ = "DT" then combogls% = 1%
             if ty$ = "DW" then combogls% = 1%
             if ty$ = "DX" then combogls% = 1%
             if ty$ = "EC" then combogls% = 1%
             if ty$ = "ED" then combogls% = 1%
             if ty$ = "EG" then combogls% = 1%
             if ty$ = "EH" then combogls% = 1%
             if ty$ = "EK" then combogls% = 1%
             if ty$ = "EL" then combogls% = 1%
             if ty$ = "ES" then combogls% = 1%
             if ty$ = "ET" then combogls% = 1%
             if ty$ = "EV" then combogls% = 1%
             if ty$ = "EW" then combogls% = 1%
             if ty$ = "GA" then combogls% = 1%
             if ty$ = "GB" then combogls% = 1%
             if ty$ = "GD" then combogls% = 1%
             if ty$ = "GE" then combogls% = 1%
             if ty$ = "GL" then combogls% = 1%
             if ty$ = "GM" then combogls% = 1%
             if ty$ = "GP" then combogls% = 1%
             if ty$ = "GQ" then combogls% = 1%
             if ty$ = "GS" then combogls% = 1%
             if ty$ = "GT" then combogls% = 1%
             if ty$ = "GV" then combogls% = 1%
             if ty$ = "GW" then combogls% = 1%
             if ty$ = "HM" then combogls% = 1%
             if ty$ = "HN" then combogls% = 1%
             if ty$ = "HQ" then combogls% = 1%
             if ty$ = "HR" then combogls% = 1%
             if ty$ = "HT" then combogls% = 1%
             if ty$ = "HU" then combogls% = 1%
             if ty$ = "HW" then combogls% = 1%
             if ty$ = "HX" then combogls% = 1%
             if ty$ = "JK" then combogls% = 1%
             if ty$ = "JL" then combogls% = 1%
             if ty$ = "JN" then combogls% = 1%
             if ty$ = "JP" then combogls% = 1%
             if ty$ = "JR" then combogls% = 1%
             if ty$ = "JS" then combogls% = 1%
             if ty$ = "JU" then combogls% = 1%
             if ty$ = "JV" then combogls% = 1%
             if ty$ = "JX" then combogls% = 1%
             if ty$ = "JY" then combogls% = 1%
             if ty$ = "KB" then combogls% = 1%
             if ty$ = "KD" then combogls% = 1%
             if ty$ = "KR" then combogls% = 1%
             if ty$ = "KS" then combogls% = 1%
             if ty$ = "KU" then combogls% = 1%
             if ty$ = "KV" then combogls% = 1%
             if ty$ = "KX" then combogls% = 1%
             if ty$ = "KY" then combogls% = 1%
             if ty$ = "L3" then combogls% = 1%
             if ty$ = "L4" then combogls% = 1%
             if ty$ = "L6" then combogls% = 1%
             if ty$ = "L7" then combogls% = 1%
             if ty$ = "L9" then combogls% = 1%
             if ty$ = "LF" then combogls% = 1%
/* - (CR1988) */

             tab% = 17%                 /* PLAN CONT */
             init(" ") code$
             code$ = model$
             gosub check_code
             if code% = 1% then convert str(desc$,30%,1%) to contHead%, ~
                                                    data goto badContHead

badContHead:

             if contHead% <> 0% then goto NoSliderCheck
             tab% = 23%                 /* GLASS07   */
             init(" ") code$
             code$ = model$
             gosub check_code
             if code% = 1% then convert str(desc$,30%,1%) to contHead%, ~
                                                    data goto NoSliderCheck

             slider% = 1%
NoSliderCheck:

             tab% = 18%                 /* GEDTOPBOT */
             init(" ") code$
             str(code$,1%,2%) = ty$
             str(code$,3%,1%) = str(view$,1%,1%)
             if combogls% = 0% then goto notCombo  /*if Combo = contHead*/
               str(code$,3%,1%) = "*"
REM CALL "SHOSTAT" ("WINDOW / SLIDER COMBO ") STOP
             if slider% = 0% then gosub windowcombo ~
              else gosub slidercombo

notCombo:
             gosub check_code
             if code% = 1% then gedtopbot% = 1%
             if gedtopbot% = 1% then goto GEDTOPBOT_FOUND

             if combogls% = 0% then goto lookupGED_001

             str(code$,3%,1%) = str(view$,1%,1%)
             gosub check_code
             if code% = 1% then gedtopbot% = 1%
             if gedtopbot% = 1% then goto GEDTOPBOT_FOUND
             goto lookupGED_001

GEDTOPBOT_FOUND:

             gl_ord_flg$ = str(desc$,1%,1%)
             ty$ = str(desc$,3%,2%)
             sp_igstr1$ = str(desc$,8%,1%)        /* (CR1173) */
             sp_sand1$ = str(desc$,9%,2%)
             sp_igstr2$ = str(desc$,11%,1%)
             sp_sand2$ = str(desc$,12%,2%)        /* (CR1173) */
             tpSand$   = str(desc$,6%,10%)
             if str(desc$,6%,2%) = "TG" then goto sandwich_3p /*(AWD032)*/
             goto SPECIAL_GLASS


lookupGED_001:
             tab% = 19%                 /* GED 001 */
             init(" ") code$
             str(code$,1%,2%) = ty$
             gosub check_code
             if code% = 0% then goto nosandwich
             sp_igstr1$ = str(desc$,3%,1%)         /* (CR1173) */
             sp_sand1$ = str(desc$,4%,2%)
             sp_igstr2$ = str(desc$,6%,2%)
             sp_sand2$ = str(desc$,7%,2%)
             tpSand$   = str(desc$,1%,10%)         /* (CR1173) */
             if str(desc$,1%,2%) = "TG" then goto sandwich_3p /*(AWD032)*/
             p%  = pos(tpSand$ = "T")
             p1% = pos(tpSand$ = "PR")
             p2% = pos(tpSand$ = "EJ")
             p2% = pos(tpSand$ = "EP")
/* (AWD037) */
             if schema% <> 1% then goto NotNCTemp
               if p%  <> 0% then goto TempGlss
               if p1% <> 0% then goto TempGlss
               if p2% <> 0% then goto TempGlss
NotNCTemp:
REM       NOT NC; this logic is for NTX
               if p%  <> 0% then goto TempGlss
               if p1% <> 0% then goto TempGlss
REM       NTX Skips Lamn
               if p2% <> 0% then goto notTempered
TempGlss:
SPECIAL_GLASS:


/*(AWD028) REMOVE NOT TEMPERED FROM ORDER FILE */
/* (AWD030) */

REM CALLED FROM DISPLAY_ANALYSIS_GLASS
validate_sand_temp:
             tab% = 7%              /* PLAN SAND */
             code$ = sp_sand1$
/*(AWD037)*/ if sc_type$ = "B" then code$ = sp_sand1$ & sp_lamFl$
             if sc_type$ = "D" then code$ = sp_sand1$ & sp_lamFl$
             gosub check_code
             str(sandwich$(1%),1%,30%) = str(desc$,1%,30%)

             init(" ") code$
             code$ = sp_sand2$
             gosub check_code
             str(sandwich$(2%),1%,30%) = str(desc$,1%,30%)
/* (AWD032) */
             if sp_sand3$ = " " then goto no_validate_trip
               init(" ") code$
               code$ = sp_sand3$
               gosub check_code
               str(sandwich$(3%),1%,30%) = str(desc$,1%,30%)
no_validate_trip:
/* (AWD019)  */
             tab% = 11%             /* PLANCSAND */
             init(" ") code$
             code$ = strength$ & sp_sand1$      /* (AWD028) */
/*(AWD037)*/ if sc_type$ = "B" then code$ = strength$ & sp_sand1$ & sp_lamFl$
             if sc_type$ = "D" then code$ = strength$ & sp_sand1$ & sp_lamFl$
             if spType% = 16% then code$ = sp_igstr1$ & sp_sand1$ /* (CR1173) */
             gosub check_code
             str(csandwich$(1%),1%,30%) = str(desc$,1%,30%)

             init(" ") code$
             code$ = strength$ & sp_sand2$      /* (AWD028) */
             if spType% = 16% then code$ = sp_igstr2$ & sp_sand2$ /* (CR1173) */
             gosub check_code
             str(csandwich$(2%),1%,30%) = str(desc$,1%,30%)
/*(AWD032) */
             if sp_sand3$ = " " then goto no_cvalidate_trip
               init(" ") code$
               code$ = strength$ & sp_sand3$
               gosub check_code
               csandwich$(3%) = str(desc$,1%,30%)
no_cvalidate_trip:
             if str(csandwich$(1%),1%,30%) = " " then goto noSandwich
             if str(csandwich$(2%),1%,30%) = " " then goto noSandwich
/*(AWD032) */
             if sp_sand3$ <> " " and csandwich$(3%) = " " then goto noSandwich
       return
/* (AWD032) */
       windowcombo
REM N% = 1% is really 0 glass and N% = 6% is really 5 glass
         if n% = 1% or n% = 6% then str(code$,4%,1%) = "L"
         if (n% = 2% or n% = 7%) and contHead% = 2% then   ~
                                    str(code$,4%,1%) = "R"
         if (n% = 2% or n% = 7%) and contHead% = 3% then   ~
                                   str(code$,4%,1%) = "M"
         if (n% = 3% or n% = 8%) and contHead% = 3% then   ~
                                   str(code$,4%,1%) = "R"
       return

       slidercombo
         if contHead% = 2% then gosub twinslider
         if contHead% = 3% then gosub tripleslider
       return
       twinslider
REM N% = 1% is really 0 glass and N% = 6% is really 5 glass
         if hg$ = "06" then goto XO_Slider
         if n% <> 1% then goto OXRight    /* hg$ = "07" */
          if ty$ = "BT" then str(code$,4%,1%) = "L" /*LeftTmp*/
          if ty$ = "LB" then str(code$,4%,1%) = "L" /*LeftTmp*/
          if ty$ = "BU" then str(code$,4%,1%) = "L" /*RghtTmp*/
          if ty$ = "RB" then str(code$,4%,1%) = "L" /*RghtTmp*/
        return
OXRight:

         if n% <> 6% then goto OXComplete
          if ty$ = "BT" then str(code$,4%,1%) = "R" /*LeftTmp*/
          if ty$ = "LB" then str(code$,4%,1%) = "R" /*LeftTmp*/
          if ty$ = "BU" then str(code$,4%,1%) = "R" /*RghtTmp*/
          if ty$ = "RB" then str(code$,4%,1%) = "R" /*RghtTmp*/
       OXComplete
       return
       XO_Slider
REM N% = 1% is really 0 glass and N% = 6% is really 5 glass
         if n% <> 1% then goto XORight
          if ty$ = "BT" then str(code$,4%,1%) = "R" /*LeftTmp*/
          if ty$ = "LB" then str(code$,4%,1%) = "R" /*LeftTmp*/
          if ty$ = "BU" then str(code$,4%,1%) = "R" /*RghtTmp*/
          if ty$ = "RB" then str(code$,4%,1%) = "R" /*RghtTmp*/


         return
XORight:
         if n% <> 6% then goto XOComplete
          if ty$ = "BT" then str(code$,4%,1%) = "L" /*LeftTmp*/
          if ty$ = "LB" then str(code$,4%,1%) = "L" /*LeftTmp*/
          if ty$ = "BU" then str(code$,4%,1%) = "L" /*RghtTmp*/
          if ty$ = "RB" then str(code$,4%,1%) = "L" /*RghtTmp*/
       XOComplete
       return
       tripleslider
REM N% = 1% is really 0 glass and N% = 6% is really 5 glass
         if n% <> 6% then goto XOXRight
          if ty$ = "BT" then str(code$,4%,1%) = "L" /*LeftTmp*/
          if ty$ = "LB" then str(code$,4%,1%) = "L" /*LeftTmp*/
          if ty$ = "BU" then str(code$,4%,1%) = "L" /*RghtTmp*/
          if ty$ = "RB" then str(code$,4%,1%) = "L" /*RghtTmp*/
        return
XOXRight:
         if n% <> 7% then XOXComplete
          if ty$ = "BT" then str(code$,4%,1%) = "R" /*LeftTmp*/
          if ty$ = "LB" then str(code$,4%,1%) = "R" /*LeftTmp*/
          if ty$ = "BU" then str(code$,4%,1%) = "R" /*RghtTmp*/
          if ty$ = "RB" then str(code$,4%,1%) = "R" /*RghtTmp*/
       XOXComplete
         REM should be n% = 1%
         if str(code$,4%,1%) = " " then str(code$,4%,1%) = "M"
       return

       sandwich_3p
         tab% = 20%
         code$ = ty$ & str(view$,1%,1%)
         gosub check_code
         sp_sand1$ = str(desc$,2%,2%)
         sp_sand2$ = str(desc$,10%,2%)
         sp_sand3$ = str(desc$,18%,2%)
         goto SPECIAL_GLASS
/* (\AWD032) */


        write_dtl
            k% = 0%
            init(" ") sd$(), h_sd$

            gosub open_file
            gosub write_headers              /* (AWD019) Cardinal Head */
            tp_item% = 0%
            tp_citem% = 0%                   /* (AWD019) Cardinal Item */
            neuSpacer% = 1%                  /* (AWD028) */
            rack% = 0%                       /* (AWD028) */
            patioRack% = 0%                  /* (AWD028) */
                      /* (AWD026) flag to know when ultra rack started */
            ultra_rack% = 0%
            init(" ") sp_temp_key$           /* #18 AWDPLNWK           */
            read #18, hold, key > sp_temp_key$, using L60580, intercept$, ~
                              sp_rack$, tp_spacer$, eod goto write_dtl_done

L60580:             FMT CH(02), XX(3), CH(1), CH(10)

                    sav_spacer$    = tp_spacer$
                    sav_intercept$ = intercept$
                    sav_rack$      = sp_rack$
                       goto write_dtl_first
        write_dtl_nxt
            read #18, hold, eod goto write_dtl_done

write_dtl_first
/*(AWD028) add spType$ */
REM  GET #18, USING L60530, INTERCEPT$, SPTYPE$, SP_RACK$, TP_SPACER$,    ~
                            TP_SP_TH$, TP_MODEL$,  WD$, HT$, TP_GLASS$,   ~
                            TP_COLOR$, SP_VIEW$, T_K$, MUTTIN$, SP_WARR$, ~
                            TP_SO$, TP_LN$, SP_TEMP$, SP_TEMP1$, SP_TEMP2$,~
                            TP_SIZE$, TP_TYPE$, TP_LITE$, TP_VIEW$,       ~
                            SP_FIL$, WD1$, HT1$, TP_CTYPE$   /* (AWD019) */

           get #18, using L60530, intercept$, spType$, sp_rack$, tp_spacer$,~
                               tp_sp_th$, tp_model$,  wd$, ht$, tp_glass$,  ~
                               tp_color$, sp_view$, t_k$, muttin$, sp_warr$,~
                               sp_so$, sp_ln$, sp_temp$, sp_temp1$,         ~
                               sp_temp2$, tp_size$, tp_type$, tp_lite$,     ~
                               tp_view$, sp_fil$, wd1$, ht1$, tp_ctype$,    ~
                               tp_so$, tp_ln$, nxtOrder$
    /* (AWD019) */

                      delete #18

L60530:      FMT CH(2), CH(3), CH(1), CH(10), CH(10), CH(3), CH(9), CH(8), ~
                 CH(2), CH(1), CH(1), CH(6), CH(8), CH(9), CH(8), CH(2),   ~
                 CH(2), CH(3), CH(2), CH(20), CH(30), CH(1), CH(3), CH(7), ~
                 CH(9), CH(8), CH(30), CH(08), CH(02), CH(05)

/* (AWD026) */

             if sp_rack$ = "2" and ultra_rack% = 0% then gosub reset_rack

             if str(tp_spacer$,1%,10%) = str(sav_spacer$,1%,10%)       ~
                                                      then goto same_sp
                tp_item% = tp_item% + 4%
                sav_spacer$ = tp_spacer$
                neuSpacer% = 1%
same_sp

             if intercept$ = sav_intercept$ then goto same_intercept
                tp_item% = tp_item% + 4%
                sav_intercept$ = intercept$
                neuSpacer% = 1%

same_intercept

             if sp_rack$ = sav_rack$ then goto same_rack
                tp_item% = tp_item% + 4%
                sav_rack$ = sp_rack$
                neuSpacer% = 1%
                REM! gosub reset_rack

same_rack

             tp_item% = tp_item% + 1%
             if tp_item% > 180% then tp_item% = 1%
             convert tp_item% to tp_item$, pic(0000)

/* (AWD029) 1 means patio rack */

             if sp_rack$ = "1" then tp_item$ = "    "
REM! IF SP_RACK$ = "3" THEN TP_ITEM$ = "    "    /* (AWD026) */


             write #19, using L60540, tp_comma$, tp_item$, tp_comma$,     ~
                        tp_model$, tp_comma$, tp_so$, tp_comma$,          ~
                        tp_ln$, tp_comma$, sp_temp$, tp_comma$, sp_temp1$,~
                        tp_comma$, sp_temp2$,tp_comma$,tp_view$,tp_comma$,~
                        tp_size$, tp_comma$,tp_type$, tp_comma$, tp_lite$,~
                        tp_comma$, sp_rack$, tp_comma$, intercept$,       ~
                        tp_comma$, tp_spacer$, tp_comma$

L60540:            FMT CH(1), CH(4), CH(1), CH(3), CH(1), CH(8), CH(1),   ~
                       CH(2), CH(1), CH(2), CH(1), CH(3), CH(1), CH(2),   ~
                       CH(1), CH(3), CH(1), CH(20), CH(1), CH(30), CH(1), ~
                       CH(1), CH(1), CH(1), CH(1), CH(2), CH(1), CH(10), CH(1)

/* (AWD019)  */
/* (AWD028) */
              if neuSpacer% = 1% then gosub newSpacer
              if tp_citem% = 104% then gosub newSpacer


              tp_citem% = tp_citem% + 1%
              if tp_citem% > 104% then tp_citem% = 1%

              convert tp_citem% to tp_item$, pic(###0)

REM IF SP_RACK$ = "1" THEN TP_ITEM$ = "    "
REM IF SP_RACK$ = "3" THEN TP_ITEM$ = "    "

              call "STRING" addr("LJ", wd1$, len(wd1$), wd1$)
              call "STRING" addr("LJ", ht1$, len(ht1$), ht1$)

              call "SPCESMSH" (wd1$, 1%)
              call "SPCESMSH" (ht1$, 1%)

              convert intercept$ to intercept%, data goto error2

error2:

REM file length 192 /* (CR2056) CHANGE FILLER TO 98*/
              write #21, using L60560,                          ~
/*Order */            tp_item$ , tp_comma$,                     ~
/*Item  */            tp_model$, tp_comma$,                     ~
/*Series*/            tp_so$, tp_comma$,                        ~
/*Width */            wd1$, tp_comma$,                          ~
                      "x", tp_comma$,                           ~
/*Height*/            ht1$, tp_comma$,                          ~
/*GlsType*/           tp_ctype$, tp_comma$,                     ~
/*Shape*/             tp_comma$,                                ~
/*Qty*/               tp_lite$, tp_comma$, sp_rack$,            ~
                      tp_comma$,                                ~
                      intercept$ & "-" & interdesc$(intercept%),~
                      tp_comma$, sp_warr$, tp_comma$, " "

L60560:                 FMT CH(4), CH(1), CH(3), CH(1), CH(8), CH(1),  ~
                            CH(9), CH(1), CH(1), CH(1), CH(8), CH(1),  ~
                            CH(30), CH(1), CH(1), CH(1), CH(1), CH(1), ~
                            CH(1), CH(08), CH(1), CH(09), CH(1), CH(98)
/* (AWD019) */


                     gosub loadDisplayRack

                     goto write_dtl_nxt
        write_dtl_done
        return
/* (AWD026) */
        reset_rack
          ultra_rack% = 1%
          tp_item% = 0%
          tp_citem% = 0%
        return


        loadDisplayRack
          k% = k% + 1%

REM                0        1         2         3         4         5         6
REM                123456789012345678901234567890123456789012345678901234567890
REM       h_sd$ = "IC TYP R SPAC MDL WAR       WD       X HT       GLS TYP"
          str(sd$(k%),1%,2%)   = intercept$
          str(sd$(k%),4%,3%)   = sp_type$
          str(sd$(k%),8%,1%)   = sp_rack$
          str(sd$(k%),10%,4%)  = tp_spacer$
          str(sd$(k%),15%,3%)  = tp_model$
          str(sd$(k%),19%,9%)  = sp_warr$
          str(sd$(k%),29%,9%)  = wd1$
          str(sd$(k%),38%,1%)  = "x"
          str(sd$(k%),40%,8%)  = ht1$
          str(sd$(k%),49%,15%) = tp_ctype$



        return


/*(AWD028)*/
        newSpacer
            init(" ") header1$, header2$, header3$, header4$, header5$
            str(header1$,1,14) = "Rack XX"
            if rmk% = 1% then str(header1$,1,14) = "Remake XX" /*(IM8022)*/
            str(header2$,1,14) = "Spacer  XXXX  "
            str(header3$,1,14) = "Type    XXX   "

            str(header2$,9,4) = tp_spacer$
            str(header3$,9,3) = spType$

            spType% = 0%
            convert spType$ to spType%, data goto error3

error3:

            str(header4$,1,30) =  spTypeDesc$(spType%)

REM !            IF SP_RACK$ = "1" OR SP_RACK$ = "3" THEN GOTO PATIORACK
            if sp_rack$ = "1" and rmk% = 0% then goto patioRack /* (IM8022) */

REM not patio
              rack% = rack% + 1%
              if rmk% = 0% then convert rack% to str(header1$,6,2), pic(00)
              if rmk% <> 0% then convert rack% to str(header1$,8,2), pic(00)
              goto rackFinished

patioRack:   REM Patio Racks
              patioRack% = patioRack% + 1%
              str(header1$,1,7) = "Picture XX"
              convert patioRack% to str(header1$,9,2), pic(00)

rackFinished:

               gosub write_header_rack
               tp_citem% = 0%
               neuSpacer% = 0%
        return
/*(AWD028\) */

/* (AWD019) */
        write_headers
               init(" ") header1$, header2$
               header1$ = "RELEASE DATE:"
               header2$ = "P.O.#."

               gosub write_header_rec

               init(" ") header1$, header2$
               header1$ = "DELIVERY DATE:"
               header2$ = "VENDOR:"

               gosub write_header_rec


               init(" ") header1$, header2$
               header1$ = "TO:"
               header2$ = "FROM:"

               gosub write_header_rec

               gosub write_item_header

        return

        write_header_rec
               write #21, using HEADER1,                                  ~
                         tp_comma$,                                       ~
                         tp_comma$,                                       ~
                         header1$,                                        ~
                         tp_comma$,                                       ~
                         tp_comma$,                                       ~
                         header2$,                                        ~
                         tp_comma$,                                       ~
                         tp_comma$,                                       ~
                         tp_comma$

HEADER1:             FMT CH(1), CH(1), CH(15), CH(1), CH(1), CH(15),     ~
                         CH(1), CH(1), CH(1)

        return

/* (AWD028) */
        write_header_rack
/* REC LEN 128 => header1$ - Rack #, Picture #, etc */
               write #21, using HEADER3, header1$, tp_comma$, header2$,   ~
                      tp_comma$, header3$, tp_comma$, header4$, tp_comma$,~
                      header5$, tp_comma$, " "

                                            /* (CR2056) CHANGE FILLER TO 97 */
HEADER3:             FMT CH(15), CH(01), CH(15), CH(01), CH(15), CH(01), ~
                         CH(30), CH(01), CH(15), CH(01), CH(97)

        return
/* (AWD028\) */

        write_item_header
               write #21, using HEADER2,                                  ~
                         "ORDER #",     tp_comma$,                        ~
                         "ITEM",        tp_comma$,                        ~
                         "SERIES",      tp_comma$,                        ~
                         "SIZE",        tp_comma$, tp_comma$, tp_comma$,  ~
                         "GLASS TYPE",  tp_comma$,                        ~
                         "SHAPE *",     tp_comma$,                        ~
                         "QTY",         tp_comma$,                        ~
                         "RACK",        tp_comma$,                        ~
                         "INTERCEPT",   tp_comma$,                        ~
                         "WASHER",      tp_comma$,         /* (CR2056) */ ~
                         "ORDER NUMBER", tp_comma$,        /* (CR2056) */ ~
                         "PO NUMBER", tp_comma$,           /* (CR2056) */ ~
                         "SALES ORDER", tp_comma$,         /* (CR2056) */ ~
                         "SO LINE", tp_comma$              /* (CR2056) */

HEADER2:                  FMT   CH(7), CH(1), CH(4), CH(1), CH(6), CH(1), ~
                                CH(4), CH(1), CH(1), CH(1), CH(10), CH(1),~
                                CH(7), CH(1), CH(3), CH(1), CH(4), CH(1), ~
                                CH(9), CH(1),                             ~
/* (CR2056) */                  CH(6), CH(1), CH(12), CH(1), CH(9), CH(1),~
                                CH(11), CH(1), CH(7), CH(1)
        return
/* (AWD019) */

        build_dtl_rpt
            savType% = spType%                           /* (AWD028) */
            for kk% = 1% to ct%
                if len(ctt$(kk%,1%)) < 3 then goto L60175
                sp_warr$ = warrantyid$
                convert (kk%-1%) to str(sp_warr$,9%,1%), pic(0)  /*(AWD030)*/

                wd1$ = ctt$(kk%,1%)
                ht1$ = ctt$(kk%,2%)
                view$ = "TOP"
                chg$ = "N"
                if sp_stat$ > "00" then chg$ = "C"

                if kk% > 5% then view$ = "BOT"
                n% = kk%
                view% = 6%
                if view$ = "BOT" then view% = 12%
                spac% = 3%
                if n% > 5% then spac% = 9%
                convert kk%    to sp_temp$, pic(00)

                sp_view$ = "0"
                if str(view$,1%,1%) = "B" then sp_view$ = "1"

REM GOSUB CHECK_ULTRA                         /* (AWD026) */
                gosub lookup_intercept                      /* (AWD029) */
                gosub check_size
/* SR64679 */
REM GOSUB CHECKINTERCEPT                               /* (CR2055) */
                gosub lookup_sandwich                  /* (AWD028) */
/* (AWD028) do not print clear label but are in file */
REM IF P% = 0% THEN GOTO L60175
                spType$ = "000"
                convert spType% to spType$, pic(000)
/* (AWD028\) */
REM  ! WRITE #20, USING L60210, INTERCEPT$, SPTYPE$, SP_RACK$,      ~
                       GDD$(VIEW%),     ~
                       GDD$(SPAC%), STR(SP_PART$,1%,3%),WD$,HT$,   ~
                       STR(SP_PART$,5%,2%), STR(SP_PART$,4%,1%),   ~
                       SP_VIEW$, T_K$, MUTTIN$,  SP_WARR$, SP_SO$, ~
                       SP_LN$,SP_TEMP$,SP_TEMP1$,T5$,SANDWICH$(K%),~
                       SP_LITS$, VIEW$, SP_DUE_DATE$, SP_PART$,    ~
                       SP_PART_D$, SP_QTY$, CHG$, SP_CUST$, WD1$,  ~
                       HT1$, FIELD1$, FIELD2$, FIELD3$, FIELD4$,   ~
                       FIELD5$, FIELD6$, FIELD7$, FIELD8$, FIELD9$,~
                       FIELD10$,  FIELD11$

             write #20, using L60210, intercept$, spType$, sp_rack$,      ~
                         gdd$(view%), gdd$(spac%), str(sp_part$,1%,3%),   ~
                         wd$, ht$, str(sp_part$,5%,2%), str(sp_part$,4%,1%),~
                         sp_view$, t_k$, muttin$,  sp_warr$, " ", " ",    ~
                         sp_temp$,sp_temp1$,t5$,sandwich$(k%), sp_lits$,  ~
                         view$, sp_due_date$, sp_part$, sp_part_d$, sp_qty$,~
                         chg$, sp_cust$, wd1$, ht1$, field1$, field2$,    ~
                         field3$, field4$, field5$, field6$, field7$,     ~
                         field8$, field9$, field10$, field11$, sp_so$, sp_ln$

L60210:          FMT CH(02), CH(3), CH(1), CH(10), CH(10), CH(3), CH(9),  ~
                     CH(8), CH(02), CH(1),  /* (AWD028) add spType$ */    ~
                     CH(1), CH(6), CH(8), CH(9), CH(8), CH(2), CH(2),     ~
                     CH(3), CH(20), CH(30),CH(1), CH(3), CH(8), CH(25),   ~
                     CH(32), CH(4), CH(1), CH(9), CH(9), CH(8), CH(1),    ~
                     CH(1), CH(1), CH(1), CH(1), CH(1), CH(1), CH(1),    ~
                     CH(1), CH(1), CH(1), CH(08), CH(02)

/* Put Original value back in case of error (AWD028) */
                    spType% = savType%

L60175:     next kk%

        return
                                                   /* (EWD015)         */
        check_thickness                            /* Thickness/Spacer */
           init(" ") readkey$, t_k$
           str(readkey$,1%,9%)   = "GED 002  "
           str(readkey$,10%,15%) = str(sp_part$,1%,3%)

           gosub readGencdsDesc
           if found% = 0% then goto no_thickness

           t_k$ = str(desc$,1%,6%)          /* Thickness of Spacer    */
        return
        no_thickness
           t_k$="ERR-01"
        return                                     /* (EWD015)        */

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#18,mode$, 500%, f2%)
            if f2% <> 0% then goto L62619
            wrk% = 1%
        return
L62619:     call "SHOSTAT" ("Error - Cannot Open (APCPLNWK)") : stop
        return
        open_work1
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#20,mode$, 500%, f2%)
            if f2% <> 0% then goto L62619
            wrk1% = 1%
        return
        delete_work
            if wrk%  = 1% then call "FILEBGON" (#18)
            if wrk1% = 1% then call "FILEBGON" (#20)
        return

        open_work_temp
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#33,mode$, 500%, f2%)
            if f2% <> 0% then goto L62633
            wrk% = 1%
        return
L62633:     call "SHOSTAT" ("Error - Cannot Open (AWDPLNTP)") : stop
        return

        lookup_sub_part                              /* (PAR000) - BEG */
             init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$(), ~
                       subpart$
             dim1es, dim2es, dim3es = 0.00
            flag$ = "0"                  /* Sales Order Info         */
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


            subpart$ = str(bcksubpt_rec$,48%,20%)
            series$  = str(bcksubpt_rec$,169%,16%)
            style$   = str(bcksubpt_rec$,185%,10%)

            get bcksubpt_rec$ using dimFmt, dim1es, dim2es
dimFMT:               FMT POS(153), PD(14,4), PD(14,4)

            if suberr1% = 0% then return
            str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            subpart$ = str(bcksubpt_rec$,48%,20%)

            errormsg$ = "AWDBKSUB ERROR = "&so_inv$ & " Line= " & item_no$
            gosub error_prompt
            suberr1% = 0%

        return

/* (AWD026) */
        check_ultra
           init(" ") readkey$
           ultra% = 0%
           str(readkey$,1%,9%)   = "PLANULTRA"
           str(readkey$,10%,3%)  = str(sp_part$,1,3)
           str(readkey$,13%,2%)  = "**"
           read #4,key = readkey$, eod goto no_ultra_all
              ultra% = 1%
              return
        no_ultra_all
           str(readkey$,1%,9%)   = "PLANULTRA"
           str(readkey$,10%,3%)  = str(sp_part$,1,3)
           str(readkey$,13%,2%)  = str(sp_part$,5,2)
           read #4,key = readkey$, eod goto no_ultra
              ultra% = 1%
        no_ultra
        return

/*(AWD027)*/
        buildFileScreen
            if sc_type$ = "E" then goto buildFlexScreen
            gosub openRitescreenFile
            gosub writeRiteHeader

buildFlexScreen:
            gosub loadPhantoms
            rpt% = 1%
            cnt% = 0%
            seq_cnt% = 0%
            been_here% = 99%
            scr_cnt% = 0%
            call "SHOSTAT" ("Build Screen EDI Data")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L64300  /* Build All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L64400
                                                  /* Only Build 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L64400
                                                  /* Only Build 'Blanks' */

L64300:      gosub loadHldKey
             read_HD_next
                gosub loadHldFromScreenDetail
                   if rec% = 0% then goto L64400

                gosub calcScreenSize
                seq_cnt% = seq_cnt% + 1%
REM    GOSUB SETUPDTL         /* (CR2245) */
REM    GOSUB BUILDDTLSCREEN   /* (CR2245) */
                convert sqty% to sqty$, pic(00)
                gosub writeAWDSCHSR
             goto read_HD_next

L64400:     next dt%

        convert scr_cnt% to scr_cnt$, pic(#########0)
        gosub screen_prompt
        
        return clear all
        goto inputmode

        loadHldKey
           init(" ") hdKey$, savHd$
           hdKey$ = all(hex(00))
           str(hdKey$,1%,1%)  = sc_status$
           str(hdKey$,2%,1%)  = sc_type$
           str(hdKey$,3%,9%)  = str(dt$(dt%),18%,6%)
           str(hdKey$,12%,8%) = str(dt$(dt%),25%,8%)
           str(hdKey$,20%,2%) = str(dt$(dt%),34%,2%)
           savHd$ = str(hdKey$,1,22)
        return
/* (IM8022) */
        loadHldRmkKey
          init(" ") hldrmkKey$
          str(hldrmkKey$,1%,8%)  = str(dt$(dt%),25%,8%)
          str(hldrmkKey$,9%,2%)  = str(dt$(dt%),34%,2%)
          str(hldrmkKey$,11%,4%) = str(dt$(dt%),78%,4%)
          str(hldrmkKey$,15%,9%) = sp_warr$(dt%)
          str(hldrmkKey$,24%,3%) = sp_num$(dt%)
          str(hldrmkKey$,27%,1%) = sc_status$
          str(hldrmkKey$,28%,1%) = sc_type$
        return


        loadHldRmkFromScreenDetail
           rec% = 0%
           read #3,key 2% = hldrmkKey$, eod goto noHLDRmkScreenDetail

             gosub dataloadHld
             rec% = 1%

        noHLDRmkScreenDetail
        return

/* (\IM8022) */
        loadHldFromScreenDetail
           rec% = 0%
           read #26, key 1% > hdKey$, using HDFMT, hdKey$,            ~
                                         eod goto noHLDScreenDetail
HDFMT:         FMT POS(16), CH(25)

             if str(hdKey$,1,21) <> str(savHd$,1,21) then            ~
                                          goto noHLDScreenDetail

             gosub dataloadHld
             rec% = 1%

        noHLDScreenDetail
        return


        openRiteScreenFile
            init(" ") library$, volume$, file$
            volume$  = "CARLO2"
            library$ = "SCREEN"
            file$    = "AWDRTESC"

            ff% = 27%
            gosub openFile
        return

        writeRiteHeader
            init(" ") wrteRec$()
            str(wrteRec$(),1,4)   = "PO#" & tp_comma$
            str(wrteRec$(),5,9)   = "PO Line#" & tp_comma$
            str(wrteRec$(),14,13) = "Request Date" & tp_comma$
            str(wrteRec$(),27,4)  = "Qty" & tp_comma$
            str(wrteRec$(),31,7)  = "Series" & tp_comma$
            str(wrteRec$(),38,12) = "Description" & tp_comma$
            str(wrteRec$(),50,6)  = "Color" & tp_comma$
            str(wrteRec$(),56,13) = "Screen Width" & tp_comma$
            str(wrteRec$(),69,14) = "Screen Height" & tp_comma$
            str(wrteRec$(),83,13) = "Special Mesh" & tp_comma$
            str(wrteRec$(),96,12) = "XB Location" & tp_comma$
            str(wrteRec$(),108,5) = "Lot#" & tp_comma$
            str(wrteRec$(),113,5) = "Bin#" & tp_comma$
            str(wrteRec$(),118,14)= "Customer Name" & tp_comma$
            str(wrteRec$(),132,7) = "Order#" & tp_comma$
            str(wrteRec$(),139,6) = "Line#" & tp_comma$
            str(wrteRec$(),145,8) = "Barcode" & tp_comma$

            gosub writeWrteRec
        return


        writeWrteRec
            write #ff%, using WRTEFMT, wrteRec$(), eod goto writeRiteError
WRTEFMT:            FMT 4*CH(256)

        return


        writeRiteError

        return


        calcScreenSize
          code% = 0%
          init(" ") code$, width$, height$, skey$, sc$, hg$, model$,    ~
                    scr_type$, scr_hf$, wt$, wcode$, wcode1$, hnge$,    ~
                    hg_l$, hh$, sqty$, sqty1$, skey$, savSKey$
                    
          scr_width, scr_height, scr_cb_len, scr_cb_loc, scr_adap_len = 0.00

          sqty% = 1%
          model$ = str(sp_part$,1%,3%)
          hg$ = str(sp_part$,9%,2%)
          sc$ = str(sp_part$,11%,1%)
          width$ = str(sp_part$,13%,4%)
          height$ = str(sp_part$,17%,3%)
          clmr$ = str(sp_part$,20%,3%)
          wallw$ = str(sp_part$,23%,3%)

          gosub set_scrhf
          gosub set_scrtype
          gosub lookupScreen04
          if wcode$ = "E" then return            /* No Screen */

          str(skey$,1%,1%) = wcode$          /* Set Window Type Code */
          str(skey$,2%,2%) = "NO"            /* Set to (NO)rmal      */
          gosub checkhinge

          if wcode$ <> "A" and wcode$ <> "D" and wcode$ <> "F" and  ~
                     wcode$ <> "I" then goto noHinge
             if str(hh$,1%,2%) = "CO" or str(hh$,1%,2%) = "OR" then    ~
                                      str(skey$,2%,2%) = str(hh$,1%,2%)

noHinge:

          str(skey$,4%,1%) = "H"             /* Set to Half Screen   */
          if sc$ = "2" then str(skey$,4%,1%) = "F"  /* Full Screen   */
          if sc$ = "A" then str(skey$,4%,1%) = "F"  /* Full Screen   */
          if sc$ = "B" then str(skey$,4%,1%) = "F"  /* Full Screen   */
          if sc$ = "X" then str(skey$,4%,1%) = "F"
          if sc$ = "W" then str(skey$,6%,1%) = "F"
          if sc$ = "X" then str(skey$,6%,1%) = "F"

          if wcode$ = "C" or wcode$ = "G" then gosub checkscreen01
          if wcode$ = "D" or wcode$ = "I" then gosub checkglass01
          if sqty% = 0% then return

          gosub set_skey

          gosub calcScrnFormula

        return

        set_scrhf
          str(skey$,4%,1%) = "H"             /* Set to Half Screen   */
          if sc$ = "2" then str(skey$,4%,1%) = "F"  /* Full Screen   */
          if sc$ = "A" then str(skey$,4%,1%) = "F"  /* Full Screen   */
          if sc$ = "B" then str(skey$,4%,1%) = "F"  /* Full Screen   */
          scr_hf$   = str(skey$,4%,1%)
        return

        set_scrtype
          scr_type$ = "R"
          tab% = 28%                           /* (EXTRUDED)      */
          code$ = model$ & scr_hf$
          gosub check_code
          if code% = 1% then scr_type$ = "E"
          if len(sp_part$) >= 19 and sc$ = "J" then scr_type$ = "E"
          str(skey$,5%,1%) = scr_type$
        return

        lookupScreen04
          tab% = 13%                           /* (SCREEN04)      */
          code$ = model$
          gosub check_code
          if code% = 0% then return
          wt$ = str(desc$,1%,3%)
          wcode$ = str(desc$,7%,1)
          wcode1$ = str(desc$,22%,1)
            if wcode$ = "E" then return        /* No Screen       */

          if sc$ <> "B" and sc$ <> "C" then goto not_rollform
             wcode$ = wcode1$

not_rollform:
          if wcode1$ <> " " and scr_type$ = "R" then wcode$ = wcode1$

         return

         checkhinge
           triple% = 0%
           tab% = 29%
           code$ = hg$
           gosub check_code
           if code% = 0% then return
              p% = 0%
              p% = pos(desc$ = "-")
              hnge$ = str(desc$,p%+2%)
              if str(desc$,p%+2%,2%) = "  " then hnge$ = str(desc$,1%,p%-1%)
              hg_l$ = str(desc$,1%,p%-2%)
              if str(desc$,p%-4%,3%) = "1/3" then triple% = 1%
              if str(hg_l$,1%,2%) = "CO" then hh$ = "COTTAGE"
              if str(hg_l$,1%,2%) = "OR" then hh$ = "ORIEL  "
         return

         checkscreen01
           sqty% = 1%
           tab% = 24%
           code$ = model$
           gosub check_code
           if code% = 0% then return
              sqty% = 2%
              str(skey$,4%,1%) = "4"          /* Special 1/4,1/2,1/4  */
              if triple% <> 0% then str(skey$,4%,1%) = "3" /* Special */
         return

         checkglass01
           code$ = model$ & hg$
           gosub check_code
           if code% = 0% then return
             sqty$ = str(desc$,4%,2%)
             sqty1$ = str(desc$,16%,2%)
             convert sqty$ to sqty%, data goto noglass01

         noglass01
         return

         set_skey
           s_width = 0.00 : s_height = 0.00 : s_clmr = 0.00 
           s_half = 0.00 : s_full = 0.00
           a1 = 0.00 : a2 = 0.0
           
           
           convert str(width$,1%,3%) to a1, data goto badWidth
badWidth:
           convert str(width$,4%,1%) to a2, data goto badWidth1
badWidth1:
           s_width = a1 + (a2/8.0)         /* Decimal Width       */
           a1, a2 = 0.0
           convert str(height$,1%,2%) to a1, data goto badHeight
badHeight:
           convert str(height$,3%,1%) to a2, data goto badHeight1
badHeight1:
           s_height = a1 + (a2/8.0)        /* Decimal Height      */
           a1, a2 = 0.0
           if len(sp_part$) < 22 then goto noCLMR
            if hg$ < "70" or hg$ > "97" then goto noCLMR
               convert str(clmr$,1%,2%) to a1, data goto badCLMR

               convert str(clmr$,3%,1%) to a2, data goto badCLMR1
badCLMR1:
               s_clmr = a1 + (a2/8.0)
badCLMR:       if len(sp_part$) > 22 then goto noWoodW
                   if wallw$ <> "000" and wallw$ <> "   " then s_clmr = 0.0
noWoodW:       if s_clmr <= 8.0 then s_clmr = 0.0
               if s_clmr > 0.0 then gosub get_special /* Specified CB  */
noCLMR:

           ss1% = 1%
           if sc$ = "B" or sc$ = "C" then ss1% = rp%

                                                 /* Find Phantom's for Calc*/
           init(" ") savSKey$
           savSKey$ = skey$
           for ss% = ss1% to ss_max%
             if str(skey$(ss%),1%,6%) = str(skey$,1%,6%) then goto setScrPhant
           next ss%

           /* Second don't look for specific color */
           init(" ") skey$
           skey$ = savSKey$
           for ss% = ss1% to ss_max%
             if str(skey$(ss%),6%,1%) <> " " then goto nextSS
             if str(skey$(ss%),1%,5%) = str(skey$,1%,5%) then goto setScrPhant
nextSS
           next ss%


         return
setScrPhant:
           ph$(1%) = str(ss$(ss%),1%,4%)     /* Cut Width Calc        */
           ph$(2%) = str(ss$(ss%),5%,4%)     /* Cut Height Calc       */
           ph$(3%) = str(ss$(ss%),9%,4%)     /* CB Cut Length         */
           ph$(4%) = str(ss$(ss%),13%,4%)    /* CB Cut Location       */
         return

         get_special                           /* Specified Meeting Rail*/
            if sc$ = "B" or sc$ = "C" then goto get_special1
            code$ = model$
            gosub check_code
            if code% = 0% then return
               convert str(desc$,1%,9%) to s_half, data goto setScrHalf
setScrHalf:
             convert str(desc$,11%,9%) to s_full, data goto setScrFull
setScrFull:
             if sc$<> "W" and sc$ <> "X" then goto notFlex
              convert str(desc$,21%,9%) to s_half, data goto notFlex

notFlex:
            special$ = "1"                    /* Specified Ctr Bar     */
            str(skey$,2%,2%) = "SP"           /* 'SP' = Specified      */
         return

         get_special1
            code$ = model$
            gosub check_code

            convert str(desc$,1%,9%) to s_half, data goto setScrHalf1
setScrHalf1:
            convert str(desc$,11%,9%) to s_full, data goto setScrFull1
setScrFull1:

            if sc$<> "W" and sc$ <> "X" then goto notFlex1
              convert str(desc$,21%,9%) to s_half, data goto notFlex1

notFlex1:
            special$ = "1"                    /* Specified Ctr Bar     */
            str(skey$,2%,2%) = "SP"           /* 'SP' = Specified      */
         return

         loadPhantoms
           init(" ") readkey$, desc$, ss$(), skey$()
           ss% = 0%
           rp% = 0%                    /* Beginning of Roll Form Phantoms */
           str(readkey$,1%,9%) = "SCRNPHANT"
         phantonNext
           read #4,key > readkey$, using phantFMT, readkey$, desc$,     ~
                                                   eod goto phantonDone

phantFMT:         FMT  CH(24), CH(30)

              if str(readkey$,1%,9%) <> "SCRNPHANT" then goto phantonDone
               ss% = ss% + 1%
               skey$(ss%) = str(readkey$,10%,6%)
               ss$(ss%)   = str(desc$,1,16)

               if rp% = 0% and str(readkey$,10%,1%) = "F" then rp% = ss%
              goto phantonNext
         phantonDone
          ss_max% = ss%
         return

         calcScrnFormula
            cal% = 2%
            phantom$ = ph$(1%)
            gosub lookupPhantom
REM  IF ERR% <> 0% THEN HDWIDTH$ = "ERROR"
            if err% <> 0% then scr_width = -1.00
            if err% <> 0% then return
            scr_width = width

            cal% = 3%
            phantom$ = ph$(2%)
            if str(ph$(2%),1%,4%) = "TABL" then gosub spec_half       ~
                                           else gosub lookupPhantom            
REM   IF ERR% <> 0% THEN HDHEIGHT$ = "ERROR"
            if err% <> 0% then scr_height = -1.00
            if err% <> 0% then return
            scr_height = height
            
            cal% = 2%                       /* Calc CB Length(Width)*/
            if str(skey$,1%,1%) = "B" then cal% = 3%  /* Height Calc*/
            str(phantom$,1%,4%) = ph$(3%)
            gosub lookupPhantom
            scr_cb_len = width
            if str(skey$,1%,1%) = "B" then scr_cb_len = height

            cal% = 3%                       /* Calc CB Location(High)*/
            if ph$(4%) <> "CTRS" then goto noCTRS
               scr_cb_len = round(scr_width / 2.0, 2)
               goto skipCTRSPHANT

noCTRS:     str(phantom$,1%,4%) = ph$(4%)
            if str(ph$(4%),1%,4%) = "TABL" then gosub spec_full       ~
                                           else gosub lookupPhantom
skipCTRSPHANT:                                              

        return


        lookupPhantom                            /* 1%=Width and Height*/
          err% = 0%                              /* 2%=Width Only      */
          width = 0.0 : height = 0.0             /* 3%=Height Only     */
          dim1es = 0.0 : dim2es = 0.0 : dim3es = 0.00
          if str(phantom$,1%,4%) = " " then return           /* N/A  */
          if str(phantom$,1%,4%) = "0000" then return        /* N/A  */

          call "APCCALSB" (cal%,                 /* CALC TYPE 1%,2%,3*/~
                           sp_part$,             /* PART NUMBER      */~
                           dim1es,               /*Leg Height        */~
                           dim2es,               /*Leg Height        */~
                           dim3es,               /*Leg Height        */~
                           phantom$,             /* PHANTOM DESIGNATO*/~
                           width,                /* EXACT WIDTH      */~
                           height,               /* EXACT HEIGHT     */~
                           #2,                   /* AMTBOMCD EQUATION*/~
                           err% )                /* ERROR CODE 0%-OK */

        return
        
        spec_half                                    
           height =((s_height/2.0) - s_half) + ((s_height/2.0) - s_clmr )
           init(" ") phantom$
        return
        spec_full
           height =((s_height/2.0) - s_full) - ((s_height/2.0)  - s_clmr )
           init(" ") phantom$
        return
        

        writeAWDSCHSR
          init(" ") schsrKey$, scr_time$
          schsrKey$ = barcode$
          scr_time$ = time
          
          read #39, hold, key = schsrkey$, eod goto createAWDSCHSR
          
              delete #39
        createAWDSCHSR  
        
            put #39, using AWDSCHSR_FMT,                           ~
                     sp_cust$,             /* Customer Code      */~
                     barcode$,             /* Production Barcode */~
                     sp_part$,             /* Mfg Part           */~
                     sqty$,                /* Screen Qty         */~
                     sp_primary$,          /* Primary Dept       */~
                     scr_type$,            /* Screen Type        */~
                     scr_hf$,              /* Screen H/F         */~
                     scr_width,            /* Screen Width       */~
                     scr_height,           /* Screen Height      */~
                     scr_cb_len,           /* Screen CB Length   */~
                     scr_cb_loc,           /* Screen CB Location */~
                     scr_adap_len,         /* Screen Adaptor Len */~
                     userid$,              /* UserID             */~
                     scr_date$,            /* Date               */~
                     scr_time$,            /* Time               */~
                     skey$                 /* Screen Phanton Key */

              write #39, eod goto badAWDSCHSR
              
              scr_cnt% = scr_cnt% + 1%
        badAWDSCHSR
        return
AWDSCHSR_FMT:   FMT CH(09), CH(18), CH(25), CH(02), CH(03), CH(01), CH(01), ~
                    PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),       ~
                    CH(03), CH(06), CH(06), CH(10)

        setupDtl
          init(" ") barcode$, po$, series$, drDesc$, clr$
          unitInch% = 0%
          
          str(barcode$,1,8)  = sp_so$
          str(barcode$,9,2)  = sp_ln$
          str(barcode$,11,4) = sp_ln_item$
          qty% = 0%
          convert sp_qty$ to qty%, data goto SPQTY
SPQTY:
          convert qty% to str(barcode$,15,4), pic(0000)
          po$ = po$(dt%)

          clr$ = "??"
          if str(sp_part$,4,1) = "2" then clr$ = "WH"
          if str(sp_part$,4,1) = "3" then clr$ = "BZ"
          if str(sp_part$,4,1) = "6" then clr$ = "AL"
          if str(sp_part$,4,1) = "7" then clr$ = "CY"
          if str(sp_part$,4,1) = "V" then clr$ = "CY"  /* CR3178 */				  
          if str(sp_part$,4,1) = "A" then clr$ = "WH"
          if str(sp_part$,4,1) = "B" then clr$ = "WH"
          if str(sp_part$,4,1) = "E" then clr$ = "WH"
          if str(sp_part$,4,1) = "F" then clr$ = "CY"

          tab% = 14%                           /* (RITESCRN)      */
          str(code$,1,3) = str(sp_part$,1,3)
          str(code$,4,1) = str(sp_part$,4,1)
          str(code$,5,1) = str(sp_part$,11,1)
          gosub check_code
            str(drDesc$,1,35) = str(desc$,1,30)
            convert unitInch% to str(drDesc$,36,5), pic(#####)

/* Economy Door */
          if str(sp_part$,1,3) = "311" then goto patioDoor
          if str(sp_part$,1,3) = "315" then goto patioDoor
          if str(sp_part$,1,3) = "316" then goto patioDoor

/* DP 50 Door */
          if str(sp_part$,1,3) = "332" then goto dp50
          if str(sp_part$,1,3) = "333" then goto dp50
          if str(sp_part$,1,3) = "334" then goto dp50
          if str(sp_part$,1,3) = "335" then goto dp50
          if str(sp_part$,1,3) = "336" then goto dp50
          if str(sp_part$,1,3) = "363" then goto dp50
          if str(sp_part$,1,3) = "388" then goto dp50
          if str(sp_part$,1,3) = "368" then goto dp50

/* DP35 Door */
            if str(sp_part$,11,1) = "2" then series$ = "DP50" ~
                 else series$ = "HDP"

        return
        patioDoor
          series$ = "PATIODOOR"
        return
        dp50
          series$ = "DP50"
        return

        buildDtlScreen

            init(" ") wrteRec$()
            str(wrteRec$(),1,2)   = " " & tp_comma$
            convert seq_cnt% to seqCnt$, pic(#########0)

            str(wrteRec$(),3,11)  = seqCnt$ & tp_comma$
            str(wrteRec$(),14,9) = date$ & tp_comma$
            str(wrteRec$(),23,2)  = "1" & tp_comma$

            str(wrteRec$(),25,11)  = str(series$,1,10) & tp_comma$

            str(wrteRec$(),36,51) = str(drDesc$,1,50) & tp_comma$

            str(wrteRec$(),87,3)  = clr$ & tp_comma$

            str(wrteRec$(),90,11) = str(hdWidth$,1,10) & tp_comma$
            str(wrteRec$(),101,11)= str(hdHeight$,1,10) & tp_comma$
            str(wrteRec$(),112,11)= "Char Fiber" & tp_comma$
            str(wrteRec$(),123,2) = " " & tp_comma$         /*XB Location*/
            str(wrteRec$(),125,11)= str(lot$,1,10) & tp_comma$
            str(wrteRec$(),136,11)= str(bin$,1,10) & tp_comma$
            str(wrteRec$(),147,10)= str(sp_cust$,1,9) & tp_comma$
            str(wrteRec$(),157,17)= str(po$,1,16) & tp_comma$
            str(wrteRec$(),174,3) = sp_ln$ & tp_comma$
            str(wrteRec$(),177,19)= barcode$ & tp_comma$


        gosub writeWrteRec
        return

/* (AWD039) */
        buildDtlBal
          init(" ") wrteRec$(), poOraclePart$

          convert seq_cnt% to seqCnt$, pic(#########0)
          str(wrteRec$(),1%,10%)   = sp_cust$ & tp_comma$
          str(wrteRec$(),11%,19%)  = barcode$ & tp_comma$
          str(wrteRec$(),30%,9%)   = date$ & tp_comma$
          str(wrteRec$(),39%,2%)   = "1" & tp_comma$
          str(wrteRec$(),41%,16%)  = series$ & tp_comma$
/*STR(DRDESC$,1%,50%) & TP_COMMA$*/
          str(wrteRec$(),57%,10%)  = style$ & tp_comma$
          str(wrteRec$(),67%,7%)  = clrDesc$ & tp_comma$
          str(wrteRec$(),74%,16%) = total_weight$ & tp_comma$
          str(wrteRec$(),90%,16%) = balance$ & tp_comma$
          str(wrteRec$(),106%,11%) = seqCnt$ & tp_comma$
          str(wrteRec$(),117%,26%) = sp_part$ & tp_comma$

          len% = len(total_weight$)
          call "STRING" addr("LJ", total_weight$, len%)
          str(poOraclePart$,1%,1%) = str(total_weight$,1%,1%)
          str(poOraclePart$,2%,3%) = "58R"
          str(poOraclePart$,5%,2%) = str(balance$,1%,2%)
          str(poOraclePart$,7%,1%) = "W"
          str(poOraclePart$,8%,2%) = str(balance$,7%,2%)
        gosub writeWrteRec
        return
/* (AWD039\) */
/*(AWD027\)*/


/* (AWD039) */
        buildFileBalance
          gosub openbalancefile
          gosub writebalheader
          rpt% = 1%
          cnt% = 0%
          seq_cnt% = 0%
          been_here% = 99%
          procBal% = 1%
          call "SHOSTAT" ("Build EDI File For RiteScreen Patio Doors")

          for dt% = 1% to dt_max%
              cnt% = cnt% + 1%
              if report% = 1% then goto buildAllBal  /* Build All        */

              if sc_process$ = "0" and cc$(dt%) <> "X" then goto BalNext
                                                /* Only Build 'x's     */
              if sc_process$ = "1" and cc$(dt%) = "X" then goto BalNext
                                                /* Only Build 'Blanks' */

BuildAllBal:
           gosub loadHldKey
           read_HD_BAL_next
              gosub loadHldFromScreenDetail
                 if rec% = 0% then goto BalNext

              gosub calcBalanceSize
              gosub updateBalStatus
              seq_cnt% = seq_cnt% + 1%
REM              gosub setupDtl
              if balErr% = 3% or balErr% = 4% then goto read_HD_BAL_next
              if balErr% = 5% then goto read_HD_BAL_next /* (SR70007) */
              tab% = 3%
              init(" ") code$ : code$ = str(sp_part$,4%,1%)/* Color      */
              gosub check_code
              clrDesc$ = str(desc$,6%,6%)
              gosub buildDtlBal
           goto read_HD_BAL_next

BalNext:
        next dt%

        return clear all
        goto inputmode

        updateBalStatus
           str(barcode$,1%,8%)  = sp_so$
           str(barcode$,9%,2%)  = sp_ln$
           str(barcode$,11%,4%) = sp_ln_item$
           str(barcode$,15%,4%) = sp_qty$

           read #38, hold, key = barcode$, eod goto noUpdateBal
               delete #38
noUpdateBal:

          put #38, using BALFMT,                           ~
                         barcode$,            /* barcode */~
                         balerr$,             /* Bal Err */~
                         dec_height,          /* Sash Hgt*/~
                         total_weight,        /* TotalWgt*/~
                         balance$,            /* Balance */~
                         date$,               /* SysDate */~
                         userid$              /* Userid  */

BALFMT:    FMT CH(18), CH(05), PD(14,4), PD(14,4), CH(10), ~
               CH(06), CH(03)

          write #38

          if balErr% = 0% then return

          gosub readHldForDel

          sp_flag% = 3%
          delete #26

          put str(hdRec$,113%,8%) using PD14FMT, total_weight
PD14FMT:           FMT PD(14,4)

          str(hdRec$,122%,10%) = balance$
          gosub modifyScheduleHld
          goto BalNext

        readHldForDel
          recHld% = 0%
          read #26, hold, key 1% = hdKey$, using L19500, hdRec$,       ~
                                           eod goto readHldForDelDone
            recHld% = 1%
        readHldForDelDone
        return

        openBalanceFile
          init(" ") library$, volume$, file$
          volume$  = "CARLO2"
          library$ = "BALANCE"
          file$    = "AWDBAL"

          ff% = 37%
          gosub openFile
        return

        writeBalHeader
            init(" ") wrteRec$()
            str(wrteRec$(),1,6)   = "Cust#" & tp_comma$
            str(wrteRec$(),7,8)   = "Barcode" & tp_comma$
            str(wrteRec$(),15,13) = "Request Date" & tp_comma$
            str(wrteRec$(),28,4)  = "Qty" & tp_comma$
            str(wrteRec$(),32,7)  = "Series" & tp_comma$
            str(wrteRec$(),39,6)  = "Style" & tp_comma$
            str(wrteRec$(),45,6)  = "Color" & tp_comma$
            str(wrteRec$(),51,7)  = "Weight" & tp_comma$
            str(wrteRec$(),58,8)  = "Balance" & tp_comma$
            str(wrteRec$(),66,6)  = "Count" & tp_comma$
            str(wrteRec$(),72,8)  = "PartNum" & tp_comma$
            str(wrteRec$(),80,11) = "OraclePart" & tp_comma$

            gosub writeWrteRec
        return

        calcBalanceSize
          init(" ") total_weight$
          opt%, balErr% = 0%
          total_weight = 0.00

          call "AWDPLE60" (opt%,                 /* OPT TYPE 1%,2%,3%*/~
                           sp_part$,             /* part number      */~
                           subpart$,             /* Subpart          */~
                           dim1es,               /* Dim1ES           */~
                           dim2es,               /* Dim2ES           */~
                           dim3es,               /* Dim3ES           */~
                           dec_height,           /* Dec Height       */~
                           total_weight,         /* Total Weight     */~
                           balance$,             /* Balance          */~
                           balErr% )             /* ERROR CODE 0%-OK */

          convert total_weight to total_weight$, pic(#######0.0#####)
          convert balErr% to balerr$, pic(00000)
        return
/* (\AWD039) */


/* (AWD029) */
        lookup_intercept
/* (AWD117) */
          if field17$ <> "0" then goto subpart_intercept
          init(" ") readkey$, desc$, intercept$, model$, ty$
          model$ = str(sp_part$,1,3)
          ty$    = str(sp_part$,5,2)
          intercept% = 1%                  /* DEFAULT !! */
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = model$
          str(readkey$,13,2) = ty$

           read #4, key = readkey$, using L61540, desc$,  ~
                                   eod goto no_intercept_glass

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = model$
          str(readkey$,13,2) = str(ty$,1,1) & "*"

           read #4, key = readkey$, using L61540, desc$,  ~
                                   eod goto no_intercept_all

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") readkey$
           str(readkey$,1,9)  = "INTERCEPT"
           str(readkey$,10,3) = model$
           str(readkey$,13,2) = "**"

           read #4, key = readkey$, using L61540, desc$,  ~
                                   eod goto intercept_done

               convert str(desc$,1,2) to intercept%, data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return
        subpart_intercept
          convert field17$ to intercept%, data goto badsubpart_intercept
          goto intercept_done
        badsubpart_intercept
         field17$ = "0"
         goto lookup_intercept

        load_interdesc
          init(" ") readkey$, desc$, interdesc$()
          str(readkey$,1,9) = "INTERDESC"
        interdesc_next
          read #4, key > readkey$, using INTERDESC_FMT, readkey$, desc$, ~
                                              eod goto interdesc_done
INTERDESC_FMT:            FMT CH(24), CH(30)
                  if str(readkey$,1,9) <> "INTERDESC" then goto interdesc_done

                  intercept% = 9%
                  convert str(readkey$,10,2) to intercept%,      ~
                                         data goto interdesc_done

                  if intercept% > 9% then goto interdesc_error

                  interdesc$(intercept%) = str(desc$,1,5)
                  goto interdesc_next

         interdesc_done
         return
         interdesc_error
            errormsg$ = "INTERDESC ERROR = Error Loading data from INTERDESC"
            gosub error_prompt
         goto exit_program
/* (AWD029\) */

/* SR64679 + */
        checkIntercept
          if schema% <> 2% then return         /* Only TX                */
          if gls_lamn% = 1% then goto DuraSeal /* NoteTXOnlyUsesDuraBut  */
          if wd > 50 and ht > 50 then goto DuraSeal  /*TX knowsAsSpacer03*/
          if wd > 73 or  ht > 73 then goto DuraSeal
        return
        DuraSeal
          intercept% = 3%
          convert intercept% to intercept$,pic(00)
        return

/* SR64679 - */

/* (AWD030) */
        load_ged000
          init(" ")readkey$, desc$, spcr$()
          spcr% = 0%
          mat spcr_dec = zer
          conv_spcr = 0.00
          str(readkey$,1,9) = "GED 000"
        load_ged000_next
          read #4, key > readkey$, using INTERDESC_FMT, readkey$, desc$, ~
                                            eod goto load_ged000_done

               if str(readkey$,1%,9%) <> "GED 000  " then goto load_ged000_done
               if str(readkey$,10%,5%) = "XXXXX" then goto load_ged000_done

               spcr% = spcr% + 1%
               spcr$(spcr%) = str(desc$,1%,4%)
               convert str(readkey$,10%,5%) to conv_spcr, data goto load_ged000_next

               spcr_dec(spcr%) = (conv_spcr * 0.00001)
               goto load_ged000_next
        load_ged000_done
        return

        write_awdschdt
          init(" ") awdschdt_key0$, awdschdt_key1$, barcode$
          loop_qty% = 0%
          convert str(dt$(dt%),72%,4%) to loop_qty%

          for x_qty% = 1% to loop_qty%        /*loop_qty is the order qty*/
            init(" ") awdschdt_key1$, barcode$, sp_unitid$, mfgid$, ~
                      warrantyid$, spErr$, wd$, ht$
REM Set barcode and unitid
            str(barcode$,1%,8%)  = str(dt$(dt%),25%,8%) /*Sales Order    */
            str(barcode$,9%,2%)  = str(dt$(dt%),34%,2%) /*Line Item      */
            convert x_qty% to str(barcode$,11%,4%), pic(0000) /*Line Item*/
            str(barcode$,15%,4%) = str(dt$(dt%),72%,4%) /* Quantity      */
            sp_unitid$ = unitid$(dt%)
REM Set Readkey
            str(awdschdt_key1$,1,10)  = sp_unitid$
            str(awdschdt_key1$,11,18) = barcode$
                                                      /* #28 AWDSCHDT     */
            read #28, hold, key 1% = awdschdt_key1$, using AWDSCHDT_FMT2, ~
                                     sp_unitid$, mfgid$, warrantyid$,     ~
                                     sp_unitid$, barcode$, spErr$,        ~
                                     wd$, ht$, eod goto awdschdt1_done

REM if warrantyid not blank then this order has already been put on hold
REM big NO NO to assign two warranty numbers
REM              if warrantyid$ <> " " then goto next_x_qty
REM warrantyid check moved down to add_awdschdt !

                   delete #28

        awdschdt1_done
             gosub add_awdschdt
REM NEXT_X_QTY

             gosub write_awdschgl
          next x_qty%
        return

        add_awdschdt
REM assign next warrantyid and mfgid
          if warrantyid$ <> " " then goto next_x_qty
           gosub load_warranty
           gosub load_mfgid
           gosub std_wd_ht
next_x_qty:
           put #28, using AWDSCHDT_FMT2, sp_unitid$, mfgid$, warrantyid$,  ~
                                         sp_unitid$, barcode$, "000", wd$, ~
                                         ht$
AWDSCHDT_FMT2:      FMT CH(10), CH(10), CH(08), CH(10), CH(18), CH(03),    ~
                        CH(07), CH(06)

           write #28
        return



REM this is called from write_awdschgl.  update the barcode or window
REM instead of looking at each in gls panel.  may need to go back and
REM look at each panel
        update_awdschdt
          read #28, hold, key 2% = barcode$, eod goto no_update_awdschdt_barcode

                put #28, using AWDSCHDT_FMT3, spErr$
AWDSCHDT_FMT3:     FMT POS(57), CH(03)

            rewrite #28, eod goto no_update_awdschdt_barcode

        no_update_awdschdt_barcode
        return

        write_awdschgl
REM first find gls size and IG Intercept
           gosub calc_glass_size
           init(" ") field17$
           field17$ = str(subpart$,17%,1%)
           gosub lookup_intercept
REM           call "SHOSTAT" ("Write AWDSCHGL " )
REM           stop
REM for each individual lite create gls record, all data is available except
REM rm_num$ remake number bc production day is the first digit
           for n% = 1% to ct%             /* n = individual glass panels */
             cut% = 1%
             if n% > 5% then cut% = 2%
             view% = 6%
             if n% > 5% then view% = 12%        /* view% for spacer desc */
             spac% = 3%
             if n% > 5% then spac% = 9%         /* view% for spacer desc */

             view$ = "TOP"
             if n% > 5% then view$ = "BOT"
REM if there isn't a size then look for next panel
REM this means there isn't a glass planned needed
                                              /* n% for number of panels */
             if len(ctt$(n%,1%)) < 3 then goto loop_n

             sp_warr$ = warrantyid$
             convert (n%-1%) to str(sp_warr$,9%,1%), pic(0)

             wd1$ = ctt$(n%,1%)
             ht1$ = ctt$(n%,2%)
REM Check & Set rack Size
             gl_ord_flg$ = "1"
             gosub check_size
/* SR64679 */
REM GOSUB CHECKINTERCEPT                                   /* (CR2055) */

/* !!!!!!!! */
REM CALL "SHOSTAT" ("LOOKUP_SANDWICH_TEMP") STOP
             gosub lookup_sandwich_temp

REM Set Sandwich or something to not tempered!
REM IF P% = 0% THEN GOTO L60590              /* SKIP WHEN NOT TEMP GLS */
             spType$ = "000"
             convert spType% to spType$, pic(000)

             if str(sp_part$,7%,2%) = "99" then spErr$ = "998"


             gl_order$ = "00000"
             gl_order_seq$ = "00000"
             init(" ") awdschgl_key0$
             awdschgl_key0$ = sp_warr$

             read #32, hold, key = awdschgl_key0$, eod goto create_awdschgl

                get #32, using AWDSCHGL_FMT1a, gl_order$, gl_order_seq$
AWDSCHGL_FMT1a:           FMT CH(05), CH(05)

                  delete #32

create_awdschgl:

             put #32, using AWDSCHGL_FMT1, gl_order$,      /* Order Number  */~
                                           gl_order_seq$,  /* Sequenc Number*/~
                                           sp_warr$,       /* Gls Barcode   */~
                                           tpSand$,        /* Sandwich code */~
                                           intercept$,     /* Intercept code*/~
                                           spType$,        /* Spacer Type   */~
                                           "  " & sp_rack$,/* Cardinal Rack */~
                                           csandwich$(1%), /* Crdnl Sandwich*/~
                                           csandwich$(2%), /* Crdnl Sandwich*/~
                                           gdd$(view%),    /* Spacer Desc   */~
                                           gdd$(spac%),    /* Spacer Thickn */~
                                           sp_part$,       /* part number   */~
                                           wd1$,           /* Gls Width     */~
                                           ht1$,           /* Gls Height    */~
/*converted in check_size*/                wd,             /* Gls Dec Width */~
/*converted in check_size*/                ht,             /* Gls Dec Height*/~
                                           barcode$,       /* Window Barcode*/~
                                           spErr$,         /* Error Code    */~
                                           gl_ord_flg$,    /* GL order Flg  */~
                                           ty$,            /* Corrected GLS */~
                                           strength$,      /* Gls Strength  */~
                                           csandwich$(3%)  /* Triple (AWD032)*/

             write #32
REM called if no gls size
loop_n:

            if spErr$ <> "000" and gl_ord_flg$ <> "0" then gosub update_awdschdt

           next n%
        return

AWDSCHGL_FMT1:    FMT CH(05), CH(05), CH(09), CH(20), CH(02), CH(03), CH(03), ~
                      CH(30), CH(30), CH(10), CH(10), CH(25), CH(10), CH(10), ~
                      PD(14,4), PD(14,4), CH(18), CH(03), CH(01), CH(02),     ~
                      CH(01), CH(30)

        REM   ***********************************************************
        REM   * Check, set and reset planning indicators                *
        REM   ***********************************************************

Plng_Ind:         /*  Checking to see if NC and TX planning at the same time  */
            init(" ") gen$, def_desc$, plng_pgm$
            plng_err% = 0%
            txplan% = 0%
            txendpln58% = 0%
            ncplan% = 0%
            ncendpln58% = 0%

            if schema% = 1% then goto L60730
            /* if NC planning, check texas indicator else check nc indicator  */

           str(gen$,1%,8%)  = "NCPLAN"
                                        /* set to get nc planning indicator */

            read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60720
                FMT XX(01),  CH(01)

            ncplan$ = str(def_desc$,1%,1%)
            convert ncplan$ to ncplan%, data goto L60720

               /* Find user ID who set indicator */
            str(gen$,1%,8%)  = "NCUSERID"
            read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60721
                FMT XX(01),  CH(03)
            plnguser$ = str(def_desc$,1%,3%)

            if ncplan$ = "1" then plng_pgm$ = "APCPLN06"
            if ncplan$ = "1" then goto L60710
            /* if schema is texas and planning happening by nc, display error */

            str(gen$,1%,8%)  = "NCENDPLN"
                              /* set to get nc end planning 58 indicator too */

            read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60720
                FMT XX(01),  CH(01)

            ncendpln58$ = str(def_desc$,1%,1%)
            convert ncendpln58$ to ncendpln58%, data goto L60720

               /* Find user ID who set indicator */
            str(gen$,1%,8%)  = "NCENDID"
            read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60721
                FMT XX(01),  CH(03)
            plnguser$ = str(def_desc$,1%,3%)

            if ncendpln58$ = "1" then plng_pgm$ = "EWDPLN58"
            if ncendpln58$ = "1" then goto L60710
            /* if schema is texas and planning happening by nc, display error */
        return

L60710:     plng_err% = 1%
            call "SHOSTAT" ("NC is Currently Planning in Program "&plng_pgm$& ~
                " by "& plnguser$)
            stop
        return

L60720:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading NC Planning Indicator--> "&gen$)
          stop
        return

L60721:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading NC Planning User ID--> "&gen$)
          stop
        return

L60730:    str(gen$,1%,8%)  = "TXPLAN"    /* set to get tx planning indicator */

           read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60750
              FMT XX(01),  CH(01)

           txplan$ = str(def_desc$,1%,1%)
           convert txplan$ to txplan%, data goto L60750

              /* Find user ID who set indicator */
            str(gen$,1%,8%)  = "TXUSERID"
            read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60751
                FMT XX(01),  CH(03)
            plnguser$ = str(def_desc$,1%,3%)

           if txplan$ = "1" then plng_pgm$ = "APCPLN06"
           if txplan$ = "1" then goto L60740
               /* if schema is nc and planning happening by tx, display error */

           str(gen$,1%,8%)  = "TXENDPLN"
                               /* set to get tx end planning 58 indicator too */

           read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60750
                 FMT XX(01),  CH(01)

           txendpln58$ = str(def_desc$,1%,1%)
           convert txendpln58$ to txendpln58%, data goto L60750

           str(gen$,1%,8%)  = "TXENDID"
           read #29,key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60751
                FMT XX(01),  CH(03)
           plnguser$ = str(def_desc$,1%,3%)

           if txendpln58$ = "1" then plng_pgm$ = "EWDPLN58"
           if txendpln58$ = "1" then goto L60740
              /* if schema is nc and planning happening by tx, display error */
        return

L60740:     plng_err% = 1%
            call "SHOSTAT" ("TX is Currently Planning in Program "&plng_pgm$& ~
                " by "& plnguser$)
            stop
        return

L60750:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading TX Planning Indicator--> "&gen$)
          stop
        return

L60751:   plng_err% = 1%
          call "SHOSTAT" ("Error - Reading TX Planning User ID--> "&gen$)
          stop
        return         /* End of planning indicator check   */

Plng_Set:            /* Set planning indicator for schema currently planning  */
            plng_err% = 0%

            if schema% = 1% then goto L60770        /* if NC then set TX  */

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXENDPLN"     /* For TX end plan 58 flag set  */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60760
               FMT XX(01),  CH(01)
            txendplan58$ = "1"
            str(def_desc$,1%,1%) = txendplan58$

            put #29, using def_fmt1, def_desc$

            rewrite #29

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXENDID"           /* For TX plan user ID  */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60761
               FMT XX(01),  CH(03)
            txendid$ = userid$
            str(def_desc$,1%,3%) = txendid$

            put #29, using def_fmt1, def_desc$

            rewrite #29
        return

L60760:   plng_err% = 1%
          call "SHOSTAT"  ~
               ("Error - Loading TX End Planning 58 Indicator--> "&def_desc$)
          stop
        return

L60761:   plng_err% = 1%
          call "SHOSTAT"  ~
               ("Error - Loading TX End Planning 58 User ID--> "&def_desc$)
          stop
        return


L60770:     init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCENDPLN"     /* For NC end plan 58 flag set  */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60780
                 FMT XX(01),  CH(01)
            ncendplan58$ = "1"
            str(def_desc$,1%,1%) = ncendplan58$

            put #29, using def_fmt1, def_desc$

            rewrite #29

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCENDID"           /* For NC plan user ID  */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60781
                 FMT XX(01),  CH(03)
            ncendid$ = userid$
            str(def_desc$,1%,3%) = ncendid$

            put #29, using def_fmt1, def_desc$

            rewrite #29
        return

L60780:   plng_err% = 1%
          call "SHOSTAT" ~
               ("Error - Loading NC End Planning 58 Indicator--> "&def_desc$)
          stop
        return

L60781:   plng_err% = 1%
          call "SHOSTAT" ~
               ("Error - Loading NC End Planning 58 User ID--> "&def_desc$)
          stop
        return         /* End of planning set indicator   */


Plng_Reset:    /* Reset planning indicator for schema on exit  */
            if schema% = 1% then goto L60792

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXENDPLN"  /* reset for TX end plan 58 flag */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60790
                 FMT XX(01),  CH(01)
            txendplan58$ = "0"
            str(def_desc$,1%,1%) = txendplan58$

            put #29, using def_fmt1, def_desc$

            rewrite #29

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "TXENDID"      /* reset for TX plan user ID  */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60791
                 FMT XX(01),  CH(03)
            txendid$ = "   "
            str(def_desc$,1%,3%) = txendid$

            put #29, using def_fmt1, def_desc$

            rewrite #29
        return

L60790:     call "SHOSTAT" ~
               ("Error - Resetting TX End Planning 58 Indicator--> "&def_desc$)
            stop
        return

L60791:     call "SHOSTAT" ~
               ("Error - Resetting TX End Planning 58 User ID--> "&def_desc$)
            stop
        return

L60792:     init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCENDPLN"    /* reset for NC end plan 58 flag */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60795
                 FMT XX(01),  CH(01)
            ncendplan58$ = "0"
            str(def_desc$,1%,1%) = ncendplan58$

            put #29, using def_fmt1, def_desc$

            rewrite #29

            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "NCENDID"      /* reset for TX plan user ID  */
            read #29,hold,key = gen$, using def_fmt, def_desc$,      ~
                                                  eod goto L60796
                 FMT XX(01),  CH(03)
            ncendid$ = "   "
            str(def_desc$,1%,3%) = ncendid$

            put #29, using def_fmt1, def_desc$

            rewrite #29
        return

L60795:     call "SHOSTAT" ~
               ("Error - Resetting NC End Planning 58 Indicator--> "&def_desc$)
            stop
        return

L60796:     call "SHOSTAT" ~
               ("Error - Resetting NC End Planning 58 User ID--> "&def_desc$)
            stop
        return

REM      * End of nc txn planning checking *


        load_warranty
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "WARRANTY"

            read #29, hold, key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60890
def_fmt:          FMT XX(08),  CH(08)

            warrantyid$ = str(def_desc$,1%,8%)
            convert warrantyid$ to warrantyid%, data goto L60875
L60875:

            warrantyid% = warrantyid% + 1%
            convert warrantyid% to warrantyid$, pic(00000000)

            str(def_desc$,1%,8%) = warrantyid$

            put #29, using def_fmt1, def_desc$
def_fmt1:         FMT POS(09), CH(56)
            rewrite #29
        return

L60890:     errormsg$ = "Warranty Error = Error Loading data from AWDDEFIN"~
                        & def_desc$
            gosub error_prompt
        return

        load_mfgid
            init(" ") gen$, def_desc$
            str(gen$,1%,8%)  = "MFGID"

            read #29, hold, key = gen$, using def_fmt, def_desc$,        ~
                                                  eod goto L60891

            mfgid$ = str(def_desc$,1%,8%)
            convert mfgid$ to mfgid%, data goto L60876
L60876:

            mfgid% = mfgid% + 1%
            convert mfgid% to mfgid$, pic(0000000000)

            str(def_desc$,1%,8%) = str(mfgid$,3%,8%)

            put #29, using def_fmt1, def_desc$

            rewrite #29
        return

L60891:     errormsg$ = "Warranty Error = Error Loading data from AWDDEFIN"~
                        & def_desc$
            gosub error_prompt
        return

        assignOrderNumber

            init(" ") store_key$, nxtOrder$
            store_key$ = "DEFAULTS.STORE.000"
            read #31,hold,key = store_key$, using STOREFMT, nxtOrder$,      ~
                                                  eod goto noOrderNumber
STOREFMT:        FMT POS(342), CH(5)

             nxtOrder% = 0%
             convert str(nxtOrder$,1%,5%) to nxtOrder%, data goto badOrderNumber

              if nxtOrder% = 99999% then nxtOrder% = 0%
              nxtOrder% = nxtOrder% + 1%

              convert nxtOrder% to str(nxtOrder$,1%,5%), pic(00000)

              put #31, using STOREFMT, nxtOrder$
              rewrite #31
         err% = 0%
         gosub load_prompt
        return


        noOrderNumber
            errormsg$ = "Order Error = Error Loading data from SYSFILE2" ~
                        & store_key$
            gosub error_prompt
        return
        badOrderNumber
            errormsg$ = "Order Error = Error Assigning Order Number SYSFILE2"~
                        & store_key$
            gosub error_prompt
        return
        load_prompt
           comp% = 2%
           hdr$ = "*****  Order Number  *****"
           msg$(1%) = "                                           "
           msg$(2%) = "** Order Number Assigned (" & nxtOrder$ & ") **"
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
/*+(CR2245)*/        
        screen_prompt
           comp% = 2%
           hdr$ = "*****  Flex Screen  *****"
           msg$(1%) = "                                           "
           msg$(2%) = "** Flex Screens Created (" & scr_cnt$ & ") **"
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return        
/*-(CR2245)*/        

        writeOrder
            init(" ") hldLoadKey$, hldRec$
            hldLoadKey$ = nxtOrder$
            read #30, key = hldLoadKey$, eod goto createOrder

               goto orderError

        createOrder
            hldRec$ = hldLoadKey$

            put #30, using ORDERFMT, hldRec$
ORDERFMT:           FMT CH(256)
            write #30
        return
        orderError
            errormsg$ = "Order Error = Error Order already exists in HLDLOAD"~
                       & hldLoadKey$
            gosub error_prompt
        return

        createOrderDetail
            report% = 0%
            gosub build_gls_dtl
            gosub assignOrderNumber
            gosub writeOrder
            gosub sequenceOrder

            call "FILEBGON" (#18)
        return clear all
        goto inputmode

        build_gls_dtl
            gls_dtl% = 1%
            mode% = 1% : gosub open_work      /* This creates #18 workfile */
            mode% = 3% : gosub open_work
            rpt% = 1%
            cnt% = 0%
            been_here% = 99%
            sp_flag% = 5%
            call "SHOSTAT" ("Build Order For Special Tempered Glass")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto build_all  /* Build All        */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L60410
                                                  /* Only Build 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L60410
                                                  /* Only Build 'Blanks' */
build_all:        if rmk% = 0% then gosub loadHldKey    ~
                      else gosub loadHldRmkKey
read_gls_dtl:
                   if rmk% = 0% then gosub loadHldFromScreenDetail ~
                      else gosub loadHldRmkFromScreenDetail

                     if rec% = 0% then goto L60410

                      gosub find_awdschgl
                      gosub editScheduleHld
                   if rmk% = 1% then goto L60410
                   goto read_gls_dtl
L60410:     next dt%

        return

        find_awdschgl
          init(" ") awdschgl_key0$
          awdschgl_key0$ = warrantyid$

find_awdschgl_next:
          read #32, key > awdschgl_key0$, using AWDSCHGL_FMT2, awdschgl_key0$,~
                                                eod goto find_awdschgl_done

              if str(awdschgl_key0$,1%,8%) <> warrantyid$ then         ~
                                                goto find_awdschgl_done
              gosub dataloadGl
REM if order flag is 0 then do not order or sequence glass
REM put glass on order but don't give a sequence number; so will know what
REM glass was put together
REM              IF GL_ORD_FLG$ = "0" THEN GOTO FIND_AWDSCHGL_NEXT

              gosub build_dtl_gls_file
            goto find_awdschgl_next
        find_awdschgl_done
        return

        build_dtl_gls_file
             init(" ") wd1$, ht1$, t5$, sp_lits$, sp_temp$

             gosub get_muttin
             gosub change_muttin        /* Sort Special Lites to Bottom */
             if spErr$ = "998" then gl_type$ = "998"
             gosub check_thickness
             sp_lits$ = "1"
             sp_qty% = 1%
             nxtOrder$ = "00000"
             view$ = "TOP"
             convert str(gl_warr$,9%,1%) to gl%

             if gl% >= 5% then view$ = "BOT"

             sp_temp$ = "00"
             sp_temp$ = "000"
REM  FOR K% = 1% TO 2%       /* K = INDIVIDUAL PIECES OR LITES OF GLASS */
              convert k%    to sp_temp2$, pic(00)

              init(" ") sp_temp_key$
              str(sp_temp_key$,1%,2%)  = gl_intercept$      /*InterceptTyp*/
              str(sp_temp_key$,3%,3%)  = gl_type$           /* GL Type    */
              str(sp_temp_key$,6%,1%)  = str(gl_rack$,3%,1%)/*RackType0or1*/

              str(sp_temp_key$,7%,10%) = gl_spc$
              str(sp_temp_key$,17%,10%)= gl_spc_thk$
              str(sp_temp_key$,27%,3%) = str(sp_part$,1%,3%)
              str(sp_temp_key$,30%,9%) = wd$
              str(sp_temp_key$,39%,8%) = ht$
              str(sp_temp_key$,47%,2%) = str(sp_part$,5%,2%)
              str(sp_temp_key$,49%,1%) = str(sp_part$,4%,1%)
              sp_view$ = "0"
              if str(view$,1%,1%) = "B" then sp_view$ = "1"
              str(sp_temp_key$,50%,1%) = sp_view$

              str(sp_temp_key$,51%,6%) = t_k$
              str(sp_temp_key$,57%,8%) = muttin$
              str(sp_temp_key$,65%,9%) = gl_warr$
              str(sp_temp_key$,84%,2%) = sp_temp$    /*Count Glass Panels*/
              str(sp_temp_key$,86%,3%) = sp_temp1$              /* Piece */
              str(sp_temp_key$,89%,2%) = sp_temp2$              /* Piece */

              read #18, hold, key = sp_temp_key$, eod goto no_temp_gls
                   delete #18

no_temp_gls:

              write #18, using L60600, gl_intercept$,                ~
                                       gl_type$,                     ~
                                       str(gl_rack$,3%,1%),          ~
                                       gl_spc$,                      ~
                                       gl_spc_thk$,                  ~
                                       str(sp_part$,1%,3%),          ~
                                       wd$,         /*Window Width*/ ~
                                       ht$,        /*Window Height*/ ~
                                       str(sp_part$,5%,2%),          ~
                                       str(sp_part$,4%,1%),          ~
                                       sp_view$,                     ~
                                       t_k$,                         ~
                                       muttin$,                      ~
                                       gl_warr$,                     ~
                                       " ",                          ~
                                       " ",                          ~
                                       sp_temp$,                     ~
                                       sp_temp1$,                    ~
                                       sp_temp2$,                    ~
                                       t5$,                          ~
                                       gl_sand$,                     ~
                                       view$,                        ~
                                       sp_fil$,                      ~
                                       gl_wd1$,                      ~
                                       gl_ht1$,                      ~
                                       csandwich$(1%),               ~
                                       sp_so$,                       ~
                                       sp_ln$,                       ~
                                       gl_gls$,                      ~
                                       " "                  /* FILLER */

REM                  NEXT K%
        return

L60600:            FMT CH(2), CH(3), CH(1), CH(10), CH(10), CH(3), CH(9), ~
                       CH(8), CH(2), CH(1), CH(1), CH(6), CH(8), CH(9),   ~
                       CH(8), CH(2), CH(2), CH(3), CH(2), CH(20), CH(30), ~
                       CH(1), CH(3), CH(7), CH(9), CH(8), CH(30), CH(08), ~
                       CH(02), CH(02), CH(46)


        sequenceOrder
            init(" ") sp_temp_key$, gl_warr$
            sequence% = 0%
        seqOrderNxt
            read #18, hold, key > sp_temp_key$, using L60610, sp_temp_key$,~
                              eod goto seqOrderDone

L60610:             FMT CH(91)

                    gl_warr$ = str(sp_temp_key$,65%,9%)
                    gosub updateSequence
            goto seqOrderNxt
        seqOrderDone
        return

        updateSequence
           init(" ") awdschgl_key0$
           awdschgl_key0$ = gl_warr$

           read #32, hold, key 0% = awdschgl_key0$, eod goto updateSeqDone

              gosub dataloadGl
              delete #32

           gl_order_seq$ = "00000"
           if gl_ord_flg$ = "0" then goto noSeq
           sequence% = sequence% + 1%
           convert sequence% to gl_order_seq$, pic(00000)
NoSeq:
           gl_order$ = nxtOrder$

           put #32, using AWDSCHGL_FMT1, gl_order$,                       ~
                                         gl_order_seq$,                   ~
                                         gl_warr$,                        ~
                                         gl_sand$,                        ~
                                         gl_intercept$,                   ~
                                         gl_type$,                        ~
                                         gl_rack$,                        ~
                                         gl_sand1$,                       ~
                                         gl_sand2$,                       ~
                                         gl_spc$,                         ~
                                         gl_spc_thk$,                     ~
                                         gl_part$,                        ~
                                         gl_wd1$,                         ~
                                         gl_ht1$,                         ~
                                         gl_wd1_dec,                      ~
                                         gl_ht1_dec,                      ~
                                         gl_bar$,                         ~
                                         gl_err$,                         ~
                                         gl_ord_flg$,                     ~
                                         gl_gls$,                         ~
                                         gl_strength$,                    ~
                                         gl_sand3$ /*(AWD032) */

           write #32

        updateSeqDone
        return

        std_wd_ht /* Convert Standard Width/Height to Fraction in 8'ths*/
                  /* F0% = Width Fraction, F1% = Height Fraction       */
                  /* WD$ = Width & Fraction, HT$ = Height & Fraction   */
           str(wd$,1%,3%) = str(sp_part$,13%,3%)         /* Width  (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(sp_part$,17%,2%)         /* Height (2) */
           f0% = 0% : f1% = 0%                      /* Build Fractions */
           convert str(sp_part$,16%,1%) to f0%,data goto L64070   /*Width */

           convert str(sp_part$,19%,1%) to f1%,data goto L64070   /*Height*/

           goto L64080
L64070:      f0% = 8% : f1% = 8%
L64080:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%
           str(wd$,4%,1%) = " "          /* Build Width with Fraction  */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
        return

        build_file_temp
            mode% = 1%  : gosub open_work_temp
            mode% = 3%  : gosub open_work_temp
            rpt% = 1%
            cnt% = 0%
            been_here% = 99%
            numOfLites% = 2%
            if labels% = 1% then numOfLites% = 1%

            call "SHOSTAT" ("Build EDI File For Special Tempered Glass")

            for dt% = 1% to dt_max%
                cnt% = cnt% + 1%
                if report% = 1% then goto L60315  /* Build All           */

                if sc_process$ = "0" and cc$(dt%) <> "X" then goto L60420
                                                  /* Only Build 'x's     */
                if sc_process$ = "1" and cc$(dt%) = "X" then goto L60420
                                                  /* Only Build 'Blanks' */
L60315:         if rmk% = 0% then gosub loadHldKey              ~
                   else gosub loadHldRmkKey

build_file_temp_next:

                if rmk% = 0% then gosub loadHldFromScreenDetail ~
                   else gosub loadHldRmkFromScreenDetail
                if rec% = 0% then goto L60420
                  if rmk% = 0% then gosub build_dtl_temp       ~
                     else gosub build_rmk_dtl_temp

                  if rmk% = 1% then goto L60420
                  goto build_file_temp_next
L60420:     next dt%

          if labels% = 0% then gosub write_dtl_gls
          if labels% = 1% then gosub print_temp_gls_labels

          close #33           /* Close Work File */
        return clear all
        goto inputmode


        build_dtl_temp                               /* tempered production */
          init(" ") awdschgl_key0$
          awdschgl_key0$ = warrantyid$

buildTempDtlNext:                                         /* #32 AWDSCHGL */
         read #32, key > awdschgl_key0$, using AWDSCHGL_FMT2, awdschgl_key0$, ~
                                                eod goto buildTempDtlDone

           if str(awdschgl_key0$,1%,8%) <> warrantyid$ then         ~
                                             goto buildTempDtlDone
           gosub dataloadGl
/* (AWD032) */
           numOfLites% = 1%
           if labels% = 0% then numOfLites% = 2%      /* GL_GLS is glass code */
           if labels% = 0% and str(gl_gls$,1%,1%) = "U" then numOfLites% = 3%
/* CR2620 add criteria to gl_sand3 */
           if gl_sand3$ <> " " and gl_intercept$ = "04" then numOfLites% = 3%

/* (\AWD032) */

           if gl_warr$ = " " then goto buildTempDtlNext

REM IF GL_ORD_FLG$ = "0" AND LABELS% = 0% THEN GOTO BUILDTEMPDTLNEXT
           if gl_ord_flg$ = "0" then goto buildTempDtlNext
/* (AWD037) when second lite is annealed */
           if sc_type$ = "1" and str(gl_sand2$,1%,2%) = "0-" ~
                               then goto buildTempDtlNext

           if rmk% = 0% then gosub lookupOrclePO     /* rmk% = 0 prod glass */
                                                     /* rmk% = 1 rmk  glass */
/* + (CR2056) */
           init(" ") gl_washer$
           gl_washer$ = "IC-0"                             /* Default       */
           if rmk% = 0% then gosub lookupWasher
           if found% = 1% then gl_washer$ = str(desc$,01%,10%)
/* - (CR2056) */
           gosub write_gl_detail                           /* Glass Detail  */

           goto buildTempDtlNext
        buildTempDtlDone

        return

        build_rmk_dtl_temp                           /* tempered remakes    */
          init(" ") awdschgl_key0$
          awdschgl_key0$ = sp_warr$
          str(awdschgl_key0$,10%,1%) = "1"
          if sc_type$ = "D" then str(awdschgl_key0$,10%,1%) = "B"
                                                            /* #32 AWDSCHGL */
          read #32, key = awdschgl_key0$, eod goto buildTempRmkDtlDone

           gosub dataloadGl
           numOfLites% = 1%
           if labels% = 0% then numOfLites% = 2%
           if labels% = 0% and str(gl_gls$,1%,1%) = "U" then numOfLites% = 3%
/* CR2620 add criteria to gl_sand3 */
           if gl_sand3$ <> " " and gl_intercept$ = "04" then numOfLites% = 3%


           if gl_warr$ = " " then goto buildTempDtlNext

           if gl_ord_flg$ = "0" then goto buildTempDtlNext
           if sc_type$ = "1" and str(gl_sand2$,1%,2%) = "0-" ~
                               then goto buildTempDtlNext

           if rmk% = 0% then gosub lookupOrclePO     /* rmk% = 0 prod glass */
                                                     /* rmk% = 1 rmk  glass */
           gosub write_gl_detail                           /* Glass Detail  */

        buildTempRmkDtlDone
        return

        write_gl_detail
         t5$ = gl_wd1$ & " x " & gl_ht1$
         view$ = "TOP"
         convert str(gl_warr$,9%,1%) to gl%

         if gl% >= 5% then view$ = "BOT"
/*(AWD037)*/
         if schema% <> 1% then goto notNCGlDetail
            if str(gl_sand$,4%,2%) <> "EJ" and        ~
               str(gl_sand$,4%,2%) <> "EP" then       ~
                                     goto notNCGlDetail
            if sc_type$ = "B" then numOfLites% = 1%
            if sc_type$ = "D" then numOfLites% = 1%             /* (IM8022) */
notNCGlDetail:
         for lite% = 1% to numOfLites%
/* (AWD037) */
REM ===============
REM THIS MEANS IF IT IS LAMN AND (SCHEMA IS TX OR SCRN TYPE IS TEMPERED)
REM THEN ONLY SECOND LITE

          if str(gl_sand$,4%,2%) = "EJ" and        ~
                  (schema% <> 1% or sc_type$ = "1")~
                                     then lite% = 2%
          if str(gl_sand$,4%,2%) = "EP" and        ~
                  (schema% <> 1% or sc_type$ = "1")~
                                     then lite% = 2%
REM ==========
/* (\AWD037) */
          init(" ") sp_temp_key$
          str(sp_temp_key$,1%,5%)   = gl_order$
          str(sp_temp_key$,6%,5%)   = gl_order_seq$
          str(sp_temp_key$,11%,9%)  = gl_warr$

          convert lite% to sp_temp$, pic(00)
          str(sp_temp_key$,20%,2%) = sp_temp$
          str(sp_temp_key$,22%,70%)= " "
                                                     /* Create #33 AWDPLNTP */
          read #33, hold, key = sp_temp_key$, eod goto noGlDetail

             delete #33

noGlDetail:

          write #33, using L60640, gl_order$,                    ~
                                   gl_order_seq$,                ~
                                   gl_warr$,                     ~
                                   sp_temp$,                     ~
                                   " ",   /* Key Filler */       ~
                                   gl_intercept$,                ~
                                   gl_type$,                     ~
                                   gl_rack$,                     ~
                                   gl_spc$,                      ~
                                   gl_spc_thk$,                  ~
                                   str(gl_part$,1%,3%),          ~
                                   wd$,                          ~
                                   ht$,                          ~
                                   str(gl_part$,5%,2%),          ~
                                   str(gl_part$,4%,1%),          ~
                                   t5$,                          ~
                                   gl_sand$,                     ~
                                   view$,                        ~
                                   gl_wd1$,                      ~
                                   gl_ht1$,                      ~
                                   csandwich$(lite%),            ~
                                   sp_so$,                       ~
                                   sp_ln$,                       ~
                                   str(gl_part$,7%,2%),          ~
                                   field1$,                      ~
                                   field2$,                      ~
                                   field3$,                      ~
                                   field8$,                      ~
                                   sp_part$,                     ~
                                   gl_gls$,                      ~
                                   gl_wd1_dec,                   ~
                                   gl_ht1_dec,                   ~
                                   gl_strength$,                 ~
                                   poNumber$,                    ~
                                   sp_lamFl$,                    ~
                                   gl_washer$,      /*(CR2056)*/ ~
                                   " "                  /* FILLER */

          next lite%
        return
L60640:   FMT CH(05), CH(05), CH(09), CH(02), CH(70), CH(02), CH(03), CH(03),~
              CH(10), CH(10), CH(03), CH(07), CH(06), CH(02), CH(01), CH(20),~
              CH(20), CH(03), CH(10), CH(10), CH(30), CH(08), CH(02), CH(02),~
              CH(01), CH(01), CH(01), CH(01), CH(25), CH(02), PD(14,4),      ~
              PD(14,4), CH(01), CH(16), CH(1), CH(10), CH(66)


        write_dtl_gls
           k% = 0%
           init(" ") sd$(), h_sd$
/* (AWD031)*/
           if keyhit% = 13% then goto noOpenDtlGlsFile

            gosub open_file_temp
            gosub write_headers
noOpenDtlGlsFile:
           tp_item% = 0%
           tp_citem% = 0%
           neuSpacer% = 1%
           rack% = 0%
           patioRack% = 0%
                      /* (AWD026) flag to know when ultra rack started */
           ultra_rack% = 0%
           init(" ") sp_temp_key$        /* #33 AWDPLNTP tempered sort file */
           read #33, hold, key > sp_temp_key$, using L60680, gl_intercept$,~
                              gl_rack$, gl_spc$, eod goto write_dtl_done_gls

L60680:             FMT POS(92), CH(02), XX(03), CH(03), CH(10)

                    sav_spacer$    = gl_spc$
                    sav_intercept$ = gl_intercept$
                    sav_rack_gl$   = gl_rack$
                       goto write_dtl_first_gls
        write_dtl_nxt_gls
           read #33, hold, eod goto write_dtl_done

write_dtl_first_gls:                  /* Read to AWDPLNTP to write AWDTEMP1 */


           get #33, using L60690, gl_order$,                    ~
                                  gl_order_seq$,                ~
                                  gl_warr$,                     ~
                                  sp_temp$,                     ~
                                  gl_intercept$,                ~
                                  gl_type$,                     ~
                                  gl_rack$,                     ~
                                  gl_spc$,                      ~
                                  gl_spc_thk$,                  ~
                                  tp_model$,                    ~
                                  wd$, /*Window Width from dataloadWarranty */ ~
                                  ht$, /*Window Height from dataloadWarranty*/ ~
                                  tp_glass$,                    ~
                                  tp_color$,                    ~
                                  t5$,                          ~
                                  gl_sand$,                     ~
                                  view$,                        ~
                                  gl_wd1$,                      ~
                                  gl_ht1$,                      ~
                                  tp_ctype$,                    ~
                                  sp_so$,                       ~
                                  sp_ln$,                       ~
                                  tp_liting$,                   ~
                                  field1$,                      ~
                                  field2$,                      ~
                                  field3$,                      ~
                                  field8$,                      ~
                                  sp_part$,                     ~
                                  gl_gls$,                      ~
                                  gl_wd1_dec,                   ~
                                  gl_ht1_dec,                   ~
                                  gl_strength$,                 ~
                                  gl_poNumber$,                 ~
                                  sp_lamFl$,                    ~
                                  gl_washer$


                      delete #33

L60690:   FMT CH(05), CH(05), CH(09), CH(02), XX(70), CH(02), CH(03), CH(03), ~
              CH(10), CH(10), CH(03), CH(07), CH(06), CH(02), CH(01), CH(20), ~
              CH(20), CH(03), CH(10), CH(10), CH(30), CH(08), CH(02), CH(02), ~
              CH(01), CH(01), CH(01), CH(01), CH(25), CH(02), PD(14,4),       ~
              PD(14,4), CH(01), CH(16), CH(1), CH(10)
REM (AWD037) test for lamn
             lamn% = 0%
             if gl_type$ = "009" or gl_type$ = "010" then lamn% = 1%
/*(\AWD037) */

REM when is sp_rack 2???
             if gl_rack$ = "2" and ultra_rack% = 0% then gosub reset_rack

         if str(gl_spc$,1%,10%) = str(sav_spacer$,1%,10%) then goto same_sp_temp
                tp_item% = tp_item% + 4%
                sav_spacer$ = gl_spc$
                neuSpacer% = 1%
same_sp_temp:

             if gl_intercept$ = sav_intercept$ then goto same_intercept_temp
                tp_item% = tp_item% + 4%          /* Skip 4 rack slots      */
                sav_intercept$ = gl_intercept$    /* between Spacer Material*/
                neuSpacer% = 1%

same_intercept_temp:

             if gl_rack$ = sav_rack_gl$ then goto same_rack_temp
                tp_item%     = tp_item% + 4%      /* Skip 4 rack slots      */
                sav_rack_gl$ = gl_rack$           /* between Rack types     */
                neuSpacer% = 1%

same_rack_temp:

/*(AWD037)*/ REM IF LAMN% = 1% AND SC_TYPE$ = "1" THEN TP_ITEM% = TP_ITEM% + 1%

             tp_item% = tp_item% + 1%
                            /* Add 1 to leave empty slot for SGP laminate   */
/*(AWD037)*/ if lamn% = 1% and sc_type$ = "B" then tp_item% = tp_item% + 1%

             if tp_item% > 180% then tp_item% = 1%
             convert tp_item% to tp_item$, pic(0000)


             if gl_rack$ = "  1" then tp_item$ = "    "

             if neuSpacer% = 1% then gosub newSpacer_temp
             if tp_citem% = 100% then gosub newSpacer_temp

/*(AWD037)*/ REM IF LAMN% = 1% AND SC_TYPE$ = "1" THEN TP_CITEM% =TP_CITEM% + 1%

             tp_citem% = tp_citem% + 1%
                            /* Add 1 to leave empty slot for SGP laminate   */
/*(AWD037)*/ if lamn% = 1% and sc_type$ = "B" then tp_citem% = tp_citem% + 1%

             if tp_citem% > 100% then tp_citem% = 1%

             convert tp_citem% to tp_item$, pic(###0)


             call "STRING" addr("LJ", gl_wd1$, len(gl_wd1$), gl_wd1$)
             call "STRING" addr("LJ", gl_ht1$, len(gl_ht1$), gl_ht1$)

             call "SPCESMSH" (gl_wd1$, 1%)
             call "SPCESMSH" (gl_ht1$, 1%)

             call "STRING" addr("LJ", gl_rack$, len(gl_rack$), gl_rack$)

             convert gl_intercept$ to intercept%, data goto error2_temp

error2_temp:
/* (AWD031) */
             if keyhit% <> 13% then goto createGlsDtlTempFile
                gosub createPOGlsDtlFile
                goto write_dtl_nxt_gls

createGlsDtlTempFile:
REM file length 128
REM Important do not end file with comma!
              write #21, using L60660,                                      ~
/*Order */         tp_item$ ,      tp_comma$,                               ~
/*Item  */         tp_model$,      tp_comma$,                               ~
/*GlsWar*/         gl_warr$,       tp_comma$,                               ~
/*Width */         gl_wd1$,        tp_comma$,                               ~
                   "x",            tp_comma$,                               ~
/*Height*/         gl_ht1$,        tp_comma$,                               ~
/*GlsType*/        tp_ctype$,      tp_comma$,                               ~
/*Shape*/                          tp_comma$,                               ~
/*Qty*/            "1",            tp_comma$,                               ~
/*Rack*/           gl_rack$,       tp_comma$,                               ~
/*Spacer&Desc*/    gl_intercept$ & "-" & interdesc$(intercept%), tp_comma$, ~
/*!!! (CR2056) */  gl_washer$, tp_comma$,                                   ~
/*OrderNum*/       gl_order$ & "-" & gl_order_seq$, tp_comma$,              ~
/*PONum*/          gl_poNumber$, tp_comma$,                                 ~
/*(CR2056)*/       sp_so$, tp_comma$, sp_ln$,                               ~
/* !!! New len ? */ " "
                                  /* Add 1 to leave empty slot for annealed */
/*(AWD037)*/ if lamn% = 1% and sc_type$ = "1" then tp_item% = tp_item% + 1%
/*(AWD037)*/ if lamn% = 1% and sc_type$ = "1" then tp_citem% = tp_citem% + 1%
                goto write_dtl_nxt_gls

        write_dtl_done_gls
        return

L60660:                 FMT CH(4), CH(1),      /*tp_item*/ ~
                            CH(3), CH(1),      /*model  */ ~
                            CH(9), CH(1),      /*Warr   */ ~
                            CH(10),CH(1),      /*Width  */ ~
                            CH(1), CH(1),      /*X      */ ~
                            CH(10),CH(1),      /*Height */ ~
                            CH(30),CH(1),      /*GlsType*/ ~
                                   CH(1),                  ~
                            CH(1), CH(1),      /*Qty    */ ~
                            CH(3), CH(1),      /*Rack   */ ~
                            CH(08), CH(1),     /*Interce*/ ~
                            CH(10), CH(1),     /* (CR2056) Washer */~
                            CH(11), CH(1),     /*Order&Seq*/~
                            CH(16), CH(1),     /* PO NUMBER*/~
                            CH(08), CH(1),     /* (CR2056) Sales Order */~
                            CH(02), CH(1),     /* (CR2056) SO Line     */~
                            CH(50)             /*filler (CR2056) change to 50*/

        newSpacer_temp                  /* note - called from write_dtl_gls */
            init(" ") header1$, header2$, header3$, header4$, header5$
            str(header1$,1,14) = "Rack XX"
            if rmk% = 1% then str(header1$,1,14) = "Remake XX" /* (IM8022) */
            str(header2$,1,14) = "Spacer  XXXX  "
            str(header3$,1,14) = "Type    XXX   "

            str(header2$,9,4) = gl_spc$
            str(header3$,9,3) = gl_type$

            gl_type% = 0%
            convert gl_type$ to gl_type%, data goto error3_temp

error3_temp:

            str(header4$,1,30) =  spTypeDesc$(gl_type%)
                                                              /* (IM8022) */
            if gl_rack$ = "  1" and rmk% = 0% then goto patioRack_temp

REM not patio
              rack% = rack% + 1%
              if rmk% = 0% then convert rack% to str(header1$,6,2), pic(00)
              if rmk% <> 0% then convert rack% to str(header1$,8,2), pic(00)
              goto rackFinished_temp

patioRack_temp:   REM Patio Racks
              patioRack% = patioRack% + 1%
              str(header1$,1,7) = "Picture XX"
              convert patioRack% to str(header1$,9,2), pic(00)

rackFinished_temp:

REM ! GOSUB WRITE_HEADER_RACK
               if keyhit% <> 13% then gosub write_header_rack
               tp_citem% = 0%
               neuSpacer% = 0%

               if keyhit% = 13% then poNumber$ = " "
        return

        print_temp_gls_labels
           init(" ") sp_temp_key$                          /* #33 AWDPLNTP */
           read #33, hold, key > sp_temp_key$, using L60680, gl_intercept$,   ~
                         gl_rack$, gl_spc$, eod goto print_temp_gls_labels_done

                       goto write_dtl_first_gls_label
        print_dtl_nxt_gls
           read #33, hold, eod goto write_dtl_done

write_dtl_first_gls_label:


           get #33, using L60690, gl_order$,                    ~
                                  gl_order_seq$,                ~
                                  gl_warr$,                     ~
                                  sp_temp$,                     ~
                                  gl_intercept$,                ~
                                  gl_type$,                     ~
                                  gl_rack$,                     ~
                                  gl_spc$,                      ~
                                  gl_spc_thk$,                  ~
                                  tp_model$,                    ~
                                  wd$, /*Window Width from dataloadWarranty */ ~
                                  ht$, /*Window Height from dataloadWarranty*/ ~
                                  tp_glass$,                    ~
                                  tp_color$,                    ~
                                  t5$,                          ~
                                  gl_sand$,                     ~
                                  view$,                        ~
                                  gl_wd1$,                      ~
                                  gl_ht1$,                      ~
                                  tp_ctype$,                    ~
                                  sp_so$,                       ~
                                  sp_ln$,                       ~
                                  tp_liting$,                   ~
                                  field1$,                      ~
                                  field2$,                      ~
                                  field3$,                      ~
                                  field8$,                      ~
                                  sp_part$,                     ~
                                  gl_gls$,                      ~
                                  gl_wd1_dec,                   ~
                                  gl_ht1_dec,                   ~
                                  gl_strength$,                 ~
                                  poNumber$,                    ~
                                  sp_lamFl$


                      delete #33

          gosub print_lab_gls_temp
          goto print_dtl_nxt_gls
        print_temp_gls_labels_done
        return

        print_lab_gls_temp
          error%  = 0%
          call "SHOSTAT" ("Building Label ")
          gosub build_lab_gls_temp

REM  init(" ") logMessage$
REM  logMessage$ = "WARR -> " & t11$ & " SO -> " & t7$ & " WD-HT -> " & t5$ & " Inter -> " & t10$
REM  call "LOGFILE58" (logMessage$)
                                                    /* (EWD008) - Begin */
               call "EWDPLC58" (t1$,               /* Model        (03)*/~
                                t2$,               /* Color Descr  (06)*/~
                                t3$,               /* Glass Type   (15)*/~
                                t4$,               /* Litin/Grid   (06)*/~
                                t5$,               /* Width & Heig (20)*/~
                                t6$,               /* Top/Bot      (03)*/~
                                t7$,               /* SO/Ln        (11)*/~
                                t8$,               /* SO Due Date  (08)*/~
                                t9$,               /* Contour Grid (01)*/~
                                t10$,              /* (AWD026) Ultra   */~
                                t11$,              /* (AWD030) Warranty*/~
                                t12$,              /* CR2190 Glass Text*/~
                                #4,                /* (GENCODES)       */~
                                error%)            /* Return Code      */
                                                   /* (EWD008) - End   */
                if error% <> 0% then gosub L67020

        return
L67020:    errormsg$ = "(Error) Printing Glass Label for " & t7$
           gosub error_prompt
        return

        build_lab_gls_temp
           t1$ = tp_model$                          /* Model Code     */
           tab% = 3%
           init(" ") code$
           code$ = tp_color$                        /* Color      */
           gosub check_code
           t2$ = str(desc$,6%,6%)
           tab% = 4%
           init(" ") code$
           code$ = tp_glass$                        /* Glass      */
           gosub check_code
           t3$ = str(desc$,1%,15%)
           tab% = 5%
           init(" ") code$
           code$ = tp_liting$                       /* Liting     */
           gosub check_code
           p% = pos(desc$ = "-")
           if p% = 0% then p% = 4%
           l_lt$ = str(desc$,1%,p%-2%) & "   "   /* LEFT MUTTIN - TOP */
           r_lt$ = str(desc$,p%+2%,6%) & "   "   /* RIGHT MUTTIN - BOT*/

           gosub get_muttin
           gosub change_muttin
           t4$ = str(mut$,1%,6%)

           t5$ = gl_wd1$ & " x " & gl_ht1$          /* Width & Height */
           t6$ = view$                              /* View           */
           t7$ = sp_so$ & "-" & sp_ln$              /* Sales Ord/Line */
           t8$ = sp_due_date$                       /* Due Date       */

           tab% = 12%
           init(" ") code$
           code$ = field3$                          /* GRDCOLOR  */
           gosub check_code
           p% = 0%
           p% = pos(desc$ = "-")
           t2$ = str(desc$,p%+2%,6%)

           if field1$ = "2" then p% = 1%            /* Contour */
           t9$ = " "
           if p% = 1% then t9$ = "C"                /*Has Contour Grid*/

           cl$ = " "
           p% = 0%
           if field2$ = "2" then p% = 1%            /* 3/4 Inch Grid */
           if p% <> 0% then t9$ = "W"

           if field1$ = "2" and field2$ = "1" then t9$ = "E"

           if field8$ = "1" then t9$ = "S"
           t10$ = " "

           convert gl_intercept$ to intercept%, data goto error4_temp

error4_temp:

           t10$ = interdesc$(intercept%)
           t11$ = gl_warr$
/* CR2190 */
           gosub lookup_txtid

           init(" ") t12$
           nbr_line% = 0%
           init(" ") txt$, text$()
           call "APCPLTXT" (#8, txtid$, text$(), nbr_line%)
           t12$     = str(text$(2%),1%,40%)       /* Obtain Glass Text  */
            
        return

/* (\AWD030) */
/* (AWD031) */
        createPO
          report% = 0%
          gosub build_file_temp

        return clear all
        goto inputmode


        createPOGlsDtlFile

          init(" ") poKey0$, poKey1$, poRec$(), poStatus$, poStatDate$,   ~
             poOrdType$, poOrderNum$, poOrderSeq$, poRack$, poWarrantyid$,~
             poWarrantyseq$, poCardinalType$, poOraclePart$, poSandwich$, ~
             poSO$, poLn$, poWidth$, poHeight$, poStrength$

          poWarrantyid$  = gl_warr$
          poWarrantyseq$ = str(sp_temp$,2%,1%)

          str(poKey1$,1%,9%)  = gl_warr$
          str(poKey1$,10%,1%) = poWarrantyseq$

          read #34, hold, key 1 = poKey1$, eod goto createPOData
            get #34, using POFMT1, poStatus$
POFMT1:          FMT CH(01)
            if poStatus$ <> "S" then return

             delete #34
        createPOData
            gosub updatePOFields

            if poNumber$ <> " " then goto noNeuPONumber
            gosub getPONumber
            str(poNumber$,1,2) = "NC"
            if schema% = 2% then str(poNumber$,1,2) = "TX"
            convert seq% to str(poNumber$,3,6), pic(000000)

noNeuPONumber:
            put #34, using POFMT2, poStatus$,                      ~
                                   poStatDate$,                    ~
                                   poOrdType$,                     ~
                                   poOrderNum$,                    ~
                                   poOrderSeq$,                    ~
                                   poRack$,                        ~
                                   poWarrantyid$,                  ~
                                   poWarrantyseq$,                 ~
                                   poCardinalType$,                ~
                                   poOraclePart$,                  ~
                                   poSandwich$,                    ~
                                   poSo$,                          ~
                                   poLn$,                          ~
                                   poWidth$,                       ~
                                   poHeight$,                      ~
                                   poWidthDec,                     ~
                                   poHeightDec,                    ~
                                   poSqft,                         ~
                                   poStrength$,                    ~
                                   poNumber$,                      ~
                                   poWidthRnd%,                    ~
                                   poHeightRnd%

            write #34, eod goto write_error

POFMT2:   FMT CH(01), CH(06), CH(01), CH(05), CH(05), CH(20), CH(09), CH(01),  ~
              CH(20), CH(40), CH(20), CH(08), CH(02), CH(10), CH(10), PD(14,4),~
              PD(14,4), PD(14,4), CH(01), CH(16), PD(14,4), PD(14,4)

        write_error
        return

        updatePOFields
         poWidthDec, poHeightDec, poSqFt = 0.00
         poWidthRnd%, poHeightRnd%, evenWidth%, evenHeight% = 0%
         poWidthRndDec, poHeightRndDec = 0.00
         poStatus$   = "S"
         poStatDate$ = date
         poOrdType$  = sc_type$
         poOrderNum$ = gl_order$
         poOrderSeq$ = gl_order_seq$
         poRack$     = str(header1$,1%,14%)
         poCardinalType$ = tp_ctype$

         poSO$       = sp_so$
         poLn$       = sp_ln$
         poWidth$    = gl_wd1$
         poHeight$   = gl_ht1$
         poSandwich$ = gl_sand$
         
/* CR2747   */
         tab% = 30%                 /* PLAN STC */
         code$ = gl_gls$
         gosub check_code

         if code% = 0% then goto noSTC

         ps% = 0%
         if poWarrantyseq$ = "1" then ps% = 3%
         if poWarrantyseq$ = "2" then ps% = 6%
         
         hld$ = str(poSandwich$,ps%,1%)
         if str(poSandwich$,ps%,1%) <> gl_strength$                ~
           then poStrength$ = str(poSandwich$,ps%,1%)              ~
           else poStrength$ = gl_strength$
         goto afterSTC
noSTC:
         poStrength$ = gl_strength$
         
afterSTC:         
         init(" ") sp_sand1$
         sp_sand1$ = str(poSandwich$,4%,2%)
         if poWarrantyseq$ = "2" then sp_sand1$ = str(poSandwich$,7%,2%)

/* (AWD032) */
         if str(poSandwich$,1%,2%) <> "TG" then goto no_POTriple
           tab% = 20%
           code$ = gl_gls$ & str(view$,1%,1%)
           gosub check_code
           sp_sand1$ = str(desc$,2%,2%)
           if poWarrantyseq$ = "2" then sp_sand1$ = str(desc$,10%,2%)
           if poWarrantyseq$ = "3" then sp_sand1$ = str(desc$,18%,2%)

no_POTriple:
         tab% = 16%                 /* ORACLPART */
         code$ = poStrength$ & sp_sand1$ & sp_lamFl$
         gosub check_code
         poOraclePart$ = str(desc$,1%,30%)
         if code% = 0% then poOraclePart$ = "NoOraclePart"


         poWidthDec  = gl_wd1_dec
         poHeightDec = gl_ht1_dec

         if sc_type$ = "1" or sc_type$ = "5" then gosub tempRound
         if sc_type$ = "B" or sc_type$ = "D" then gosub lamnRound
        return

        tempRound
REM if the decimal is not zero then add 1 else just take whole number
         poWidthRndDec = poWidthDec - int(poWidthDec)
         poHeightRndDec = poHeightDec - int(poHeightDec)
         if poWidthRndDec > 0.00 then poWidthRnd%  = int(poWidthDec) + 1 ~
              else  poWidthRnd% = int(poWidthDec)
         if poHeightRndDec > 0.00 then poHeightRnd% = int(poHeightDec) + 1 ~
              else poHeightRnd% = int(poHeightDec)

         poSqFt = round((poWidthRnd%*poHeightRnd%) / 144.0,4)

         if poSqFt < 2 then poSqFt = 2

        return

        lamnRound
REM if the decimal is not zero then add 1
         poWidthRndDec = poWidthDec - int(poWidthDec)
         poHeightRndDec = poHeightDec - int(poHeightDec)
         if poWidthRndDec > 0.00 then poWidthRnd%  = int(poWidthDec) + 1 ~
              else  poWidthRnd% = int(poWidthDec)
         if poHeightRndDec > 0.00 then poHeightRnd% = int(poHeightDec) + 1 ~
              else poHeightRnd% = int(poHeightDec)

REM if not even number then round
         evenWidth%  = mod(int(poWidthRnd%),2%)
         evenHeight% = mod(int(poHeightRnd%),2%)
         if evenWidth% <> 0% then poWidthRnd% = poWidthRnd% + 1%
         if evenHeight% <> 0% then poHeightRnd% = poHeightRnd% + 1%

         poSqFt = round((poWidthRnd%*poHeightRnd%) / 144.0,4)

         if poSqFt < 3 then poSqFt = 3

        return
        getPONumber
          seq% = 0%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "SEQUENCE"
          str(readkey$,10%,15%) = "ORCLPO"

          read #4, hold, key = readkey$, using L54000, desc$,           ~
                                                eod goto PODone
           convert str(desc$,1%,10%) to seq%,data goto PODone
           seq% = seq% + 1%

           convert seq% to str(desc$,1,10), pic(0000000000)

           put #4,using L54000, desc$
           rewrite #4, eod goto PODone

        PODone
        return


        lookupOrclePO                           /* #34 ORACLEPO */
          init(" ")poKey1$, poNumber$
          str(poKey1$,1%,9%)  = gl_warr$
          read #34, key 1% > poKey1$, using POFMT3, poKey1$, eod goto noLookupOrclePO
POFMT3:       FMT POS(39), CH(10)


              if str(poKey1$,1,9) <> gl_warr$ then goto noLookupOrclePO
                 get #34, using POFMT4, poNumber$
POFMT4:             FMT POS(184), CH(16)

        noLookupOrclePO
        return

/* (\AWD031) */
/* (AWD036) */
        check_warranty_file
           init (" ") bar_key$, warrantyid$, unitid$, sp_so$, so_inv$
        check_warranty_next
           read #36, hold, key > bar_key$, eod goto check_warranty_done

              if warrantyid$ = " " then gosub oracle_connect

              get #36, using L62650, bar_key$, warrantyid$,      ~
                                      unitid$
              delete #36

              so_inv$ = str(bar_key$,1%,8%)
              gosub update_oracle
              gosub check_so_header
              goto check_warranty_next
          check_warranty_done
          return


          update_oracle
            gosub oracle_flush
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, stmt6$, stmt7$, ~
                      stmt8$, name$
            str(stmt1$,1%,40%)  = "UPDATE MSSQL.WARRANTY SET WARRANTYID = '"
            str(stmt1$,41%,27%) = warrantyid$ & "' WHERE BARCODE = '"
            str(stmt1$,68%,20%) = str(bar_key$,1%,18%) & "'"
            gosub oracle_exec

            if oci_err% >= 0% then return  /* Record Updated */
            gosub oracle_flush
            str(stmt1$,1%,40%)  = "INSERT INTO MSSQL.WARRANTY (UNITID, BARC"
            cmg$ = str(stmt1$,1%,40%)
            str(stmt1$,41%,36%) = "ODE, WARRANTYID) VALUES (" & unitid$ & ","
            cmg$ = str(stmt1$,41%,36%)
            str(stmt1$,77%,32%) = "'" & bar_key$ & "','" & warrantyid$ & "')"
            cmg$ = str(stmt1$,77%,32%)
            gosub oracle_exec
          return


          check_so_header
           if so_inv$ = sp_so$ and so_inv$ <> " " then return

           sp_so$ = so_inv$
           init(" ") stmt1$
           gosub oracle_flush
           stmt1$ = "UPDATE MSSQL.ORDERMASTER SET STATE = 'prod', " &~
                    "DATESTATECHANGE = to_date('" & oradate$ & "', "&~
                    "'MM-DD-YYYY') WHERE SALESORDER = '" & sp_so$ & "'"

           gosub oracle_exec
          return

          oracle_connect
            init(" ") user$, pass$, server$
            gosub get_user_pswd

            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)
             if oci_err% >= 0% then return
             errormsg$ = "YOU ARE NOT CONNECTED TO ORACLE, CONTACT SYSTEMS!!!!"
             gosub error_prompt
        return
        get_user_pswd
            call "READ100" (#31, "ORACLE PASSWORD", f1%(31%))   /* SYSFILE2 */
            if f1%(31%) <> 0% then get #31 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

/* (\AWD036) */
/* (AWD038) */
REM        CHECK_CASEMENT_MODEL
REM          INIT(" ") STATUS$, CODE$
REM          TAB% = 22%     CODE% = 0%
REM          CODE$ = STR(SP_PART$,1%,3%)
REM          GOSUB CHECK_CODE
REM        RETURN
/* (\AWD038) */

/* (AWD039) */
        openFile
          open nodisplay #ff%, output, space = 100%,                  ~
             dpack   = 100%, ipack = 100%, file = file$,              ~
             library = library$, volume = volume$, blocks = 5%

        return
        readGencdsDesc
           found% = 0%
           init(" ") desc$
           found% = 0%
           read #4, key = readkey$, using GENFMT, desc$, eod goto noGencdsDesc
GENFMT:        FMT POS(25), CH(30)
           found% = 1%
noGencdsDesc:
        return

        readGencdsNoDesc
           found% = 0%
           read #4, key = readkey$, eod goto noGencdsNoDesc
           found% = 1%
noGencdsNoDesc:
        return
/*(\AWD039) */
/*(CR893) */
        paintCust
          init(" ") sav_ord$, pcust$, pcutoff$, pregion$
REM          sav_ord$ = "0" & str(pso$,2%,7%)
          sav_ord$ = or_prefix$ & str(pso$,2%,7%)    /* CR3198 */
          gosub check_apcplnor
          pcust$ = or_cust$
          pregion$ = or_region$
          readkey1$ = pcust$
          gosub checkPaintCust
          pcutoff$ = cutoff_a$
        return
/*(CR893)-*/
/*(CR2056)+*/
        lookupWasher
           init(" ") readkey$
           str(readkey$,1%,9%)   = "GLSWASHER"     /*check all spc thickness*/
           str(readkey$,10%,06%) = gl_intercept$ & "****"
           gosub readGencdsDesc
           if found% = 1% then return
           str(readkey$,1%,9%)   = "GLSWASHER"         /*check spc thickness*/
           str(readkey$,10%,06%) = gl_intercept$ & str(gl_spc$,01%,04%)
           gosub readGencdsDesc

        return
/*(CR2056)-*/
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            gosub delete_work
            gosub check_warranty_file
            end




