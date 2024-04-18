        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN77                             *~  
            *  Creation Date     - 03/13/00                             *~
            *  Last Modified Date- 06/19/2017                           *~  
            *  Written By        - Roy H. Hoffman                       *~
            *  Modifications By  -                                      *~
            *                                                           *~
            *  Description       - Energy Star Labels Printing.         *~
            *                      Prod. Date, Dept. No., Shift & Load  *~
            *                      Range Specified.                     *~
            *                      Barcode Range Specific               *~ 
            *                                                           *~
            *  Code Tables Used  - PLNENERGY, PLN DPRTE                 *~
            *                                                           *~
            *    (PLNENERGY)     - GSSCD    GSS = Group Code Category   *~
            *       (EWD009)                 SS = Sort Code within Group*~
            *                                CD = Glass Code (Applic)   *~
            *                                                           *~
            *        Group         Description                          *~
            *        -----         ------------------------------       *~
            *        000          Design Pressure Rating                *~
            *        0??          U-Factor Rating                       *~
            *        1??          Solar Heat Gain Rating                *~
            *        2??          Visable Transmittance Rating          *~
            *        3??          Air Leakage Rating                    *~
            *(EWD009)4??          Solar Heat with 5/8 Inch Grid         *~
            *(EWD009)5??          Solar Heat with 1   Inch Grid         *~
            *(EWD009)6??          Visable Tranmittance 5/8 Inch Grid    *~
            *(EWD009)7??          Visable Transmittance 1  Inch Grid    *~
            *(AWD018)8??          Visable Transmittance SDL Grid        *~
            *(AWD018)9??          Solar Hear SDL Grid                   *~
            *(AWD020)A??          U-Factor 5/8" Grid                    *~
            *(AWD020)B??          U-Factor 1"   Grid                    *~
            *(AWD020)C??          U-Factor SDL  Grid                    *~
            *        D??          U-Factor Florida Contour              *~
            *        E??          SOLAR HEAT Florida Contour            *~
            *        F??          VISABLE TRANSMITTANCE Florida Contour *~
            *   AWD033                                                  *~
            *        G??               Blinds                           *~
            *        H??               Blinds                           *~
            *        I??               Blinds                           *~
            *        J??               3.9mm Temp No Grid U-Factpr      *~
            *        K??               3.9mm Temp No Grid Solar Heat    *~
            *        L??               3.9mm Temp No Grid Visible Trans *~
            *        M??               3.9mm Temp < 1" Grid U-Factor    *~
            *        N??               3.9mm Temp < 1" Grid Solar Heat  *~
            *        O??               3.9mm Temp < 1" Grid Visible Tran*~
            *        P??               3.9mm Temp 1" Cont U-Factor (8mm)*~
            *        Q??               3.9mm Temp 1" Cont SolarHeat(8mm)*~
            *        R??               3.9mm Temp 1" Cont Visible T(8mm)*~
            *        S??               3.9mm Temp SDL U-Factor          *~
            *        T??               3.9mm Temp SDL SolarHeat         *~
            *        U??               3.9mm Temp SDL VisibleTransmittan*~
            *        V??               Foam No Grid U-Factor            *~
            *        W??               Foam No Grid SolarHeat           *~
            *        X??               Foam No Grid VisibleTransmittance*~
            *        Y??               Foam < 1" Grid U-Factor          *~
            *        Z??               Foam < 1" Grid SolarHeat         *~
            *        !??               Foam < 1" Grid VisibleTransmittan*~
            *        @??               Foam SDL U-Factor                *~
            *        #??               Foam SDL SolarHeat               *~
            *        $??               Foam SDL VisibleTransmittance    *~
            *        ^??               Foam 1" Cont U-Factor            *~
            *        &??               Foam 1" Cont SolarHeat           *~
            *        *??               Foam 1" Cont SolarHeat           *~
            *                                                           *~
            *    (PLN DPRTE)     - Wind Zone and Design Pressure        *~
            *      (EWD009)        DDP = Design Pressure                *~
            *                      MPH = Miles per hour +-              *~
            *                                                           *~
            *                                                           *~
            *  Subroutine Used   - EWDPLA77 (Generic Label )            *~
            *                      EWDPLB77 (Energy Star Label)         *~
            *                      EWDPLD77 (Do No Apply Label) CR3059  *~
            *                                                           *~
            *  Special Comments  - EWDPLN75 Program to put data into    *~
            *                      the database (EWDPLNES)              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/11/00 ! (New) Program - Copied & Mod EWDPLN71.   ! RHH *~
            * 09/05/00 ! Mods made to both subroutines for the    ! RHH *~
            *          !   format changes. "CM" and "MFR#140"     !     *~
            * 11/14/00 ! (EWD001) Mods for new Visible Lit Trans  ! RHH *~
            * 12/07/00 ! (EWD002) Mods for label format change.   ! RHH *~
            * 01/22/01 ! (EWD003) Mods to both Subroutines for New! RHH *~
            *          !   labels. New formatted data is now on   !     *~
            *          !   label.                                 !     *~
            * 07/27/01 ! (EWD004) Mods to both Subroutine for New ! RHH *~
            *          !   near bottom with DP and window size    !     *~
            * 03/27/02 ! (EWD005) Mods to take out Reynolds flag. ! CMG *~
            * 06/24/02 ! (EWD006) Mods for the new Florida Wind   ! RHH *~
            *          !   Zone data and design wind Pressure.    !     *~
            * 09/17/02 ! (EWD007) Fix for Special Shapes Grid Code! CMG *~
            * 11/26/02 ! (EWD008) New Energy Sar Label and Format ! RHH *~
            *          ! Special Note lb_nonresvisible$ Contains  !     *~
            *          ! the value for lb_resair$ which is the    !     *~
            *          ! Air Leakage                              !     *~
            * 02/28/03 ! EWD009 Mods for Grid and 1 Inch Grid     ! RHH *~
            *          !   Values. (4) New Group codes added      !     *~
            *          !   4??, 5??, 6??, 7??                     !     *~
            * 06/17/03 ! EWD010 Mods to improve speed             ! RHH *~
            * 07/31/03 ! EWD011 Mods for 3/4 Inch Grid Text.      ! RHH *~
            *          !   Values same as 5/8 Inch Grid.          !     *~
            * 09/04/03 ! EWD012 Mods to Subroutine EWDPLB77 for   ! RHH *~
            *          !   New Energy Start Graph and Logo        !     *~
            * 03/17/04 ! EWD013 Mods to printing Labels. When 'ALL'!RHH *~
            *          !   selected skip the departments          !     *~
            *          !   0, 6, 46, 56. To print those departments!RHH *~
            *          !   Enter department number                !     *~
            * 07/16/04 ! EWD014 Mods to both subroutines to put   ! RHH *~
            *          !   the printed form of the production     !     *~
            *          !   Barcode on the bottom of the label. In !     *~
            *          !   the area with the Manufacturing Info.  !     *~
            *          !   Change both Subroutines.               !     *~
            * 08/12/05 ! EWD015 Mods to move 5 fields for the New ! RHH *~
            *          !   NFRC Format change. Also add the Glass !     *~
            *          !   thickness to the label.                !     *~
            * 08/15/05 ! AWD016 Mod to allow production Mgr's     ! RHH *~
            *          !            print single label when logged!     *~
            *          !            using 'WWW' User Id.          !     *~
            * 10/31/05 ! AWD017 Mod for NFRC Label format change  ! RHH *~
            *          !        remove 'Air Leakage(U.S./I-P) also!     *~
            *          !        make line below www.nfrc.org Bold,!     *~
            *          !        Also put leading '0' in front of  !     *~
            *          !        the numbers printed. Except for   !     *~
            *          !        N/A                               !     *~
            * 01/01/06 ! PAR000 CR347 Mod for new Sub Part Number ! RHH *~
            * 03/03/06 ! (PAR001) Mod for the new production label! RHH *~
            *          !    changes. Size Change 640 to 1024      !     *~
            * 03/08/06 ! (PAR002) Mod to correct problem with the ! RHH *~
            *          !    3/4 inch grid. Was never turned on.   !     *~
            *          !    So add logic used for 5/8 Per Kent W. !     *~
            * 03/20/06 ! (PAR003) Mod to Labels for printing at   ! RHH *~
            *          !    North East.                           !     *~
            * 04/10/06 ! (PAR004) Mod to Create Label test file   ! RHH *~
            *          !    for North East                        !     *~
            * 04/02/08 ! (AWD018) Mods for SDL Grid               ! CMG *~
            * 04/04/08 ! (AWD019) mod to take off wind zone & Pres! CMG *~
            *08/06/2008! (AWD020) Mods for SDL Grid               ! DES *~
            *09/11/2008! (AWD021) Mods for SDL Grid/ENERGY STAR   ! DES *~
            *10/24/2008! (AWD022) mods to remove 1 inch group code! CMG *~
            *          ! b/c Atriums 1 inch is under 1 inch       !     *~
            *12/09/2008! (AWD023) Mods for Lowe's energy star     ! DES *~
            *02/04/2009! (AWD024) Mods for Florida Windows        ! DES *~
            *03/24/2009! (AWD025) mod for '-' instead of N/A      ! CMG *~
            *03/25/2009! (AWD026) mod for new group energy star cd! CMG *~
            *04/01/2009! (AWD027) mod for energy start map all    ! CMG *~
            *                   regions or lower regions          !     *~
            *06/10/2009! (AWD028) mods for Florida Contour Rating ! CMG *~
            *06/15/2009! (AWD029) mods for new cont head and sill ! CMG *~
            *          !    combination glass codes               !     *~
            *07/16/2009! (AWD030) mod for new group map codes     ! CMG *~
            *12/15/2009! (AWD031) mod for models with 3/4 inch gls! CMG *~
            *02/12/2010! (AWD032) mod for brand change if customer! DES *~
            *          !          is BA111.                       ! DES *~
            *03/19/2010! (AWD033) mod for new group ratings       ! CMG *~
            *01/25/2012! (AWD034) mod for combo glass codes       ! CMG *~
            *05/31/2012! (AWD035) mod for CPD Number              ! CMG *~
            *06/05/2012! (AWD036) remove brand for CVP            ! CMG *~
            *09/19/2012! (AWD037) mod for grid                    ! CMG *~
            *11/15/2012! (AWD038) mod for door map                ! CMG *~
            *03/25/2013! (AWD039) mod for combo lowe270 gls       ! CMG *~
            *04/05/2013! (AWD040) mod for TDI and TX Products     ! CMG *~
            *03/26/2014! (AWD041) mod for custTH9050 brand=Thermal! MES *~
            *08/25/2014! (AWD042) mod for to pull series & style  ! PWW *~
            *          !          from bcksubpt.                  !     *~
            *09/09/2014! (AWD043) mod for STC Rateing             ! PWW *~
            *10/01/2014! (AWD044) For BBG set thickness to 1.0000 ! PWW *~
            *11/25/2014! (AWD045) NFRC Reg Changes.               ! PWW *~
            *02/10/2015! (AWD046) Fix for AWD042 above.           ! PWW *~
            *09/08/2015! SR68667  ECR 2015-068. Only print FBC &  ! PWW *~
            *          !          TDI numbers if (G)old Stamped.  !     *~
            *12/10/2015! SR67154  2016 NFRC Reg Changes.          ! PWW *~
            *07/18/2016! CR00532  Increase lb_series$ + 1 char.   ! PWW *~
            *06/19/2017! (CR1007) if patio then glassthick <> 5   ! CMN *~  
            *11/20/2017! CR1173   Larson project do not print     ! RDB *~
            *          !          label if sound windows          !     *~ 
            *01/15/2018! CR1251   Increase lb_series$ for AmerClas! RDB *~ 
            *12/28/2018! CR1828   EWDPRDLB cust code change       ! DES *~
            *05/09/2019! CR2048   Skip print sash glass only; allow RDB *~
            *          !          when barcode entered            !     *~  
            *04/13/2020! CR1066   New DP rating for 150 Casing    ! RDB *~  
            *05/22/2021! CR2835   Add Sequence Number to criteria ! RDB *~        
			*04/11/2022! CR3059   Do Not Apply for Larson         ! RDB *~
            *************************************************************
     
        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            count$22, sku_key$45,        /* Display                    */~
            sc_dept$3, sc_dept_d$30,     /* Department Code & Descr    */~
            sc_prddate$10,               /* Production Date            */~
            sc_load_fr$5, sc_load_to$5,  /* Load Range                 */~
            sc_shift$2,                  /* Shift Code                 */~
            sc_barcode_fr$18,            /* Beginning Barcode          */~
            sc_barcode_to$18,            /* Ending Barcode             */~
            sc_seq_fr$5,                 /* Beginning Sequence (CR2835)*/~
            sc_seq_to$5,                 /* Ending Barcode     (CR2835)*/~   
            lb_key$35, lb_key1$23,       /* Record Key from LB         */~
            lb_rec$(4%)256,              /* LB Record Data     (PAR001)*/~
            lb_prddate$10,               /* Prod. Date from LB         */~
            lb_dept$3,                   /* Dept. No. from LB          */~
            lb_shift$2,                  /* Shift Code from LB         */~
            lb_load$5,                   /* Load Number from LB        */~
            lb_barcode$18,               /* Production Barcode         */~
            filename$8,                  /* Used by EWDOPEN            */~
            sze$60, sze(10%),            /* Save Fractions             */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            b$100                        /* Print Lines for Label      */

                                         /* Energy Star label          */
        dim lb_brand$10,                 /* Brand Name                 */~
/*CR1251*/  lb_series$24, p_series$24,   /* Series Number              */~
            lb_style$24,                 /* Vinyl Window Style         */~
            lb_style2$24,                /* Vinyl Window Style         */~
            lb_glass_op$24,              /* Glass Option               */~
            lb_glass_op1$24,             /* Glass Option 2             */~
            lb_resu$4,                   /* Resident U-Factor          */~
            lb_nonresu$4,                /* Non-Resident U-Factor      */~
            lb_resheat$4,                /* Resident Heat Coeff        */~
            lb_nonresheat$4,             /* Non-Resident Heat Coeff    */~
            lb_resvisible$4,             /* Resident Visible Trans (EWD001)*/~
            lb_nonresvisible$5,          /* Non-Resident Visible Trans (EWD001)*/~
            lb_resair$4,                 /* Resident Air Leakage(EWD008)*/~
            lb_nonresair$4,              /* Non-Res - N-A       (EWD008)*/~
            lb_dp$2,                     /* Design Pressure            */~
            lb_width$7,                  /* Window Width               */~
            lb_height$6,                 /* Window Height              */~
            lb_seq$16,                   /* Sequence Number            */~
            lb_dep_so$24,                /* Department Sales Order     */~
            lb_ld_mod$24,                /* Load Model Number          */~
            lb_pd_dte$24, lb_dte$10,     /* Production Date            */~
            lb_zone$3,                   /* Wind Zone          (EWD006)*/~
            lb_pressure1$6, lb_press1$6, /* Positive Pressure  (EWD006)*/~
            lb_pressure2$6, lb_press2$6, /* Negitive Pressure  (EWD006)*/~
            lb_thick$5,                  /* Glass Thickness    (EWD015)*/~
            lb_txt1$10, lb_txt2$10,      /* Grid text-Glazed text(EWD009)*/~
            lb_cpdnum$30,                /* (AWD035) CPD Number        */~
            stc$3                        /* (AWD036) STC Rateing       */

/* (AWD035) beg */
        dim                                                               ~
            ufactor$10,                      /* UFactor                 */~
            sheat$10,                        /* Solar Heat              */~
            vtranmit$10,                     /* Visible Transmittance   */~
            cpdnumber$30,                    /* Cpd Number              */~
            warehouse$4,                     /* Warehouse               */~
            series$10,                       /* Series                  */~
            style$10,                        /* Style                   */~
            spacercode$10,                   /* SpacerCode              */~
            igthickness$10,                  /* IG THICKNESS            */~
            pane1$5,                         /* Pane1                   */~
            pane2$5,                         /* Pane2                   */~
            pane3$5,                         /* Pane3                   */~
            gridcode$5,                      /* Grid Code               */~
            gridsize$10,                     /* Grid Size               */~
            gapfill1$10,                     /* GapFill1                */~
            gapfill2$10,                     /* GapFill2                */~
            framecode$5,                     /* FrameCode               */~
            sashcode$5                       /* SashCode                */
/* (\AWD035) */

        dim u_gl$(200%)2, u_grp$(200%)3, /* Ufactor Glass and Group    */~
            s_gl$(200%)2, s_grp$(200%)3, /* Solar Heat Glass and Group */~
            v_gl$(200%)2, v_grp$(200%)3, /* Visible Transmittance (EWD001)*/~
            a_gl$(200%)2, a_grp$(200%)3, /* Air Leakage        (EWD008)*/~
            s5_gl$(200%)2, s5_grp$(200%)3,/* Solar Heat 5/8 Grid(EWD009)*/~
            s1_gl$(200%)2, s1_grp$(200%)3,/* Solar Heat 1   Grid(EWD009)*/~
            v5_gl$(200%)2, v5_grp$(200%)3,/* Visible Tran 5/8   (EWD009)*/~
            v1_gl$(200%)2, v1_grp$(200%)3,/* Visible Tran 1     (EWD009)*/~
            vs_gl$(200%)2, vs_grp$(200%)3,/* Visible SDL        (AWD018)*/~
            ss_gl$(200%)2, ss_grp$(200%)3,/* Solar Heat SDL     (AWD018)*/~
            u5_gl$(200%)2, u5_grp$(200%)3,/* U-Facto 5/8"       (AWD020)*/~
            u1_gl$(200%)2, u1_grp$(200%)3,/* U-Facto 1"         (AWD020)*/~
            uf_gl$(200%)2, uf_grp$(200%)3,/* U-Facto Florida    (AWD028)*/~
            sf_gl$(200%)2, sf_grp$(200%)3,/* Solar Heat Florida (AWD028)*/~
            vf_gl$(200%)2, vf_grp$(200%)3,/* Visable Tran Florid(AWD028)*/~
            group$3,                     /* Group Code                 */~
            gl$2,                        /* Glass Code                 */~
            glass$2,                     /* Glass Code     (AWD029)    */~
            gl1$2,                       /* Glass Code 1   (AWD029)    */~
            gl2$2,                       /* Glass Code 2   (AWD029)    */~
            gl3$2,                       /* Glass Code 3   (AWD029)    */~
            model$3,                     /* Model Code                 */~
            part$25,                     /* Part Number                */~
            combo_part$25,               /* Modified Part Number (AWD029)*/~
            sub_part$20,                 /* New Sub Part Number(PAR000)*/~
            grid$2,                      /* Liting Data        (EWD009)*/~
            vv_grp$3,                    /* Lookup Solar-Visibl(EWD009)*/~
            schema$8,                    /* (PAR003) Schema Switch     */~
            e_dept$(10%)3                /* Exclude Departments(EWD013)*/

        dim                              /* (AWD033)                   */~
            tu_gl$(200%)2, tu_grp$(200%)3,/* 3.9 No Grid Ufactor       */~
            ts_gl$(200%)2, ts_grp$(200%)3,/* 3.9 No Grid Solar Heat    */~
            tv_gl$(200%)2, tv_grp$(200%)3,/* 3.9 No Grid Visible Trans */~
            tu5_gl$(200%)2, tu5_grp$(200%)3,/* 3.9 < 1" Ufactor        */~
            ts5_gl$(200%)2, ts5_grp$(200%)3,/* 3.9 < 1" Solar Heat     */~
            tv5_gl$(200%)2, tv5_grp$(200%)3,/* 3.9 < 1" Visible Trans  */~
            tu1_gl$(200%)2, tu1_grp$(200%)3,/* 3.9 1" Cont Ufactor     */~
            ts1_gl$(200%)2, ts1_grp$(200%)3,/* 3.9 1" Cont Solar Heat  */~
            tv1_gl$(200%)2, tv1_grp$(200%)3,/* 3.9 1" Cont Visible tran*/~
            tdu_gl$(200%)2, tdu_grp$(200%)3,/* 3.9 SDL Ufactor         */~
            tds_gl$(200%)2, tds_grp$(200%)3,/* 3.9 SDL Solar Heat      */~
            tdv_gl$(200%)2, tdv_grp$(200%)3,/* 3.9 SDL Visible Trans   */~
            pu_gl$(200%)2, pu_grp$(200%)3,/* Foam Ufactor              */~
            ps_gl$(200%)2, ps_grp$(200%)3,/* Foam Solar Heat           */~
            pv_gl$(200%)2, pv_grp$(200%)3,/* Foam Visible Trans        */~
            pu5_gl$(200%)2, pu5_grp$(200%)3,/* Foam < 1" Ufactor       */~
            ps5_gl$(200%)2, ps5_grp$(200%)3,/* Foam < 1" Solar Heat    */~
            pv5_gl$(200%)2, pv5_grp$(200%)3,/* Foam < 1" Visible Trans */~
            pud_gl$(200%)2, pud_grp$(200%)3,/* Foam SDL Ufactor        */~
            psd_gl$(200%)2, psd_grp$(200%)3,/* Foam SDL Solar Heat     */~
            psv_gl$(200%)2, psv_grp$(200%)3,/* Foam SDL Visible Trans  */~
            pu1_gl$(200%)2, pu1_grp$(200%)3,/* Foam 1" Cont Ufactor    */~
            ps1_gl$(200%)2, ps1_grp$(200%)3,/* Foam 1" Cont Solar Heat */~
            pv1_gl$(200%)2, pv1_grp$(200%)3 /* Foam 1" Cont Visible Tra*/

        dim eg_key$6,                    /* Primary key                */~
            eg_model$3,                  /* Model Code                 */~
            eg_group$3,                  /* Group Code                 */~
            eg_fl$12,                    /* Florida Product Approval   */~
            lb_fl$12,                    /* Design Pressure            */~
            lb_tdi$12,                   /* (AWD040) TDI Number        */~
            eg_fill$44,                  /*                            */~
            eg_dp$2,                     /* Design Pressure            */~
            g022_tdi$12                  /* Group 022 TDI reset  CR1066*/

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

                                         /* (PAR000)                   */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            cuscode$9,                   /* customer number            */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            ctrlflag$1,                  /* CR99999                    */~
            info_flds$(35%)4             /* Additional Info Fields     */

                                         /* (PAR000)                   */
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
                                                       /* (AWD017)      */
            dim apc$40, pname$21                       /* (PAR002)      */
                                                       /* (PAR003)      */
                                                       /* (PAR004)      */
            apc$   = "Generate Energy Star Labels 04/10/2006"
            pname$ = "EWDPLN77 - Rev: 01.00"

            mat f2% = zer
            mat f1% = zer

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! MFENERGY ! Energy Star Label File                   *~
            * #2  ! AWDSKUXR ! Lowes sku cross refference file  AWD023  *~
            * #3  ! EWDPLNEX ! Energy Star Master Database              *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! EWDPRDLB ! Production Label Data File               *~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "MFENERGY", varc, consec, recsize =  100
                                                          /* (EWD004)  */
                                                          /* (EWD006)  */
            select #2,   "AWDSKUXR",                                     ~
                        varc,     indexed,  recsize = 160,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45
                                                          /* (AWD023)  */
/*
 sku#       1- 16 ch(16)
 upc#      17- 36 ch(20)
 part      37- 61 ch(25)
 sub part  62- 81 ch(20)
 model     82- 87 ch(06)
 desc      88-122 ch(35)
filler    123-160 ch(38)
*/
            select #3,  "EWDPLNEX",                                      ~
                        varc,     indexed,  recsize =  80,               ~
                        keypos =    1, keylen =  6

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


                                                   /* (PAR001)         */
            select #5, "EWDPRDLB",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23
                                                    /* (PAR001)         */
/* (AWD035) beg */

            select #6,  "NFRCDATA",                                      ~
                        varc,     indexed,  recsize = 1024,               ~
                        keypos = 1,    keylen = 104,                      ~
                        alt key  1, keypos    =   1, keylen = 24, dup,    ~
                            key  2, keypos    = 105, keylen = 30, dup

            select #7,  "NFRCMDL",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 25,    keylen = 20,                     ~
                        alt key 1, keypos = 1, keylen = 44

            select #8,  "NFRCGLS",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 7
/*(\AWD035) */


                                                        /* (PAR000)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR000)     */

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "AWDSKUXR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPLNEX" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "NFRCDATA" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "NFRCMDL"  : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "NFRCGLS"  : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
                                                           /* (PAR000)  */
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
                                                           /* (PAR000)  */

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
            call "TIME" (time$)

            debug% = 0%

            gosub load_table               /* (PLNENERGY) - Table      */
            gosub lookup_exclude_dept      /* (EWD013)                 */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   6%
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
                  if keyhit%  = 16% then       dataload
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
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

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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
         "Enter a Production Date?                                     ",~
         "Enter a Specific Department Code or 'ALL'?                   ",~
         "Enter a Specific Shift or 'AA' for All?                      ",~
         "Enter a Load Range or 'ALL'?                                 ",~
         "Enter a Barcode Range or 'ALL'?                              ",~
         "Enter a Sequence Range or 'ALL'?                             "  

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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            debug% = 0%                        /* Debug Turned On      */

            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      sc_prddate$, sc_load_fr$, sc_load_to$, sc_shift$,  ~
                      sc_barcode_fr$, sc_barcode_to$, lb_barcode$,       ~
                      lb_thick$, sc_seq_fr$, sc_seq_to$           /* CR2835 */
            lbl% = 0%

            sze$ = "1/81/43/81/25/83/47/8         "
            sze(1%) = .1250
            sze(2%) = .2500
            sze(3%) = .3750
            sze(4%) = .5000
            sze(5%) = .6250
            sze(6%) = .7500
            sze(7%) = .8750
            sze(8%) = .9999
 
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            call "SHOSTAT" ("Printing Energy Star Labels...")
            count$ = "Labels Printed (xxxxx)"
            call "DATUFMTC" (sc_prddate$)
            if debug% = 0% then gosub set_file_name /* Create and Open File */

            been_oth%    = 0%
            been_energy% = 0%
            been_donot%  = 0%                        /* CR3059 */
            lb_key$ = all(hex(00))
            str(lb_key$,1%,6%) = sc_prddate$

          load_next_rec                                     /* (PAR001) */
            read #5, key > lb_key$, using L35040, lb_rec$(),             ~
                                                       eod goto load_done
            lb_key$     = str(lb_rec$(),1%,35%)
                                                            /* (PAR001) */
            lb_prddate$ = str(lb_key$,1%,6%)
            lb_dept$    = str(lb_key$,12%,3%)
            lb_shift$   = str(lb_key$,15%,2%)
            lb_load$    = str(lb_key$,22%,5%)
            lb_seq$     = str(lb_rec$(),311%,5%)            /* CR2835    */
                                                            /* (PAR0001) */
            lb_barcode$ = str(lb_rec$(),278%,18%)

            if lb_prddate$ > sc_prddate$ then goto load_done

            if lb_shift$ <> sc_shift$ and sc_shift$ <> "AA"              ~
                then goto load_next_rec

            if sc_dept$ = "ALL" then goto L30000
               if lb_dept$ <> sc_dept$ then goto load_next_rec
                  goto L30005
                                                        /* (EWD013)    */
L30000:     for ee% = 1% to e_dept_max%
                if lb_dept$ = e_dept$(ee%) then goto load_next_rec
            next ee%
                                                        /* (EWD013)    */
L30005:     if str(sc_load_fr$,1%,3%) <> "ALL" then goto L30010
               if pos("AS" = str(lb_load$,1%,1%)) > 0 then               ~
                                            goto load_next_rec
               goto L30020

L30010:     if (lb_load$ < sc_load_fr$ or lb_load$ > sc_load_to$)        ~
                                                  then goto load_next_rec

L30020:     if (lb_barcode$ < sc_barcode_fr$ or lb_barcode$ > sc_barcode_to$)~
                and str(sc_barcode_fr$,1%,3%) <> "ALL" then              ~
                goto load_next_rec
/* CR2835 */
            if (lb_seq$ < sc_seq_fr$ or lb_seq$ > sc_seq_to$)        ~
                and str(sc_seq_fr$,1%,3%) <> "ALL" then              ~
                goto load_next_rec 
                                                          /* (PAR001)       */
                model$ = str(lb_rec$(),36%,3%)           /* Set Model Code */
                                                         /* (PAR001)       */
                part$  = str(lb_rec$(),523%,25%)         /* Set MFG        */
/* CR2048 */
                if str(sc_barcode_fr$,1%,3%) <> "ALL" then goto L30025
                if str(part$,11%,1%) = "4" then load_next_rec
                if str(part$,11%,1%) = "5" then load_next_rec
                if str(part$,11%,1%) = "6" then load_next_rec
                if str(part$,11%,1%) = "7" then load_next_rec

/* CR1173 Larson project sound windows do not print label  */
L30025:
/* CR3059 Set donotapply flag here and use below.  Customer only in NC */
                donotapply% = 0%
                cuscode$ = str(lb_rec$(),727%,9%)   				
                if cuscode$ = "LA0200" or  cuscode$ = "LA5200" or ~
                   cuscode$ = "LA7200" or  cuscode$ = "LA9200" or ~
                   cuscode$ = "LA8200" or  cuscode$ = "LA6200" then ~
                      donotapply% = 1%
					  
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN STC "
            str(readkey$,10%,3%) = str(part$,5%,2%)
            read #4,key = readkey$, eod goto L30030
			      if donotapply% = 1% then goto L30030  /* Print only Larson */
                  goto load_next_rec
L30030:
/* CR1173 - */
                                                         /* (PAR000)       */
                init(" ") so_inv$, item_no$
                                                         /* (PAR001)       */
                so_inv$  = str(lb_rec$(),278%,8%)        /* Sales Order    */
                                                         /* (PAR001)       */
                item_no$ = str(lb_rec$(),286%,2%)        /* Line Item      */
                gosub lookup_sub_part
                sub_part$ = str(bcksubpt_rec$,48%,20%)
                stc$ = str(bcksubpt_rec$,195%,3%)        /* (AWD043)       */
                                                         /* (PAR000)       */
                part% = len(part$)
                if part% < 19% then goto load_next_rec   /* Skip Parts     */
                gosub convert_size                       /* (AWD040) */
                sc$ = str(part$,11%,1%)
                p% = pos("8" = sc$)                      /* Skip Screen Only*/
                if p% <> 0% then goto load_next_rec

                gosub check_samples
                if ss% > 11% and ss% < 29% then goto load_next_rec
                             /* Skip Sample    */
                                                         /* Parts          */
                gosub check_for_mull                     /* Skip Mull      */
                if mull% = 1% then goto load_next_rec    /* onlys          */
                                                         /* (EWD009)       */
                                                         /* (PAR000)       */
                gosub check_grid                         /* Test for 5/8 or*/
                                                         /* 1 Inch grid    */
                                                         /* gg% = 0%,1%,2%,3%,4% */

/* (AWD033) */
                gt% = 0%
                pl% = 0%
                gosub checkGlThick
                gosub checkPolystrene

                gosub check_cont_head                    /* Test for Cont- */
                                                         /* inous head and */
                                                         /* for No. of labels*/
                                                         /* no_labels%=1%,2%,3% */

/* (AWD029) */
L30060:                                                  /* (EWD009)       */
                gosub format_label

                                                         /* (EWD009)       */
                gosub format_label_new                   /* (AWD035)       */

L30050:
                gosub check_for_lowes_stock
                
                ctrlflag$ = "Y"                 /* CR9999 Y to use XB */
                if lbl% = 0% then ctrlflag$ = "N"  /* first or only 1 label */
/* CR3059 + */
                if schema% <> 1% then goto L30055
/* donotappply set above as Larson customer only */
                if donotapply% = 1% then energy_star% = 0%
L30055:                
/* CR3059  - */                
                
                if energy_star% = 0% then gosub prt_other                    ~
                                     else gosub prt_energy

                lbl% = lbl% + 1%

                                                         /* (RHHTEST)        */
                                                         /* (EWD014)         */
                                                         /* (EWD010)         */
        REM        if lbl% > 15% then goto Load_done
                                                         /* (RHHTEST)        */
                                                         /* (EWD014)         */

                no_label% = no_label% - 1%
/* (AWD029) */
                if no_label% > 0% and combo% = 1% then goto L30060
                if no_label% > 0% then goto L30050
                                                         /* Print Additional */
                                                         /* Labels for Cont. */
                                                         /* Head and Seal    */

                                                         /* (EWD009)         */
                if mod(lbl%,25%) <> 0 then goto load_next_rec
                   convert lbl% to str(count$,17%,5%), pic(#####)

                   call "SHOSTAT" (count$)
            goto load_next_rec

        load_done
            
            if lbl% > 1 then gosub print_blank_label   /*CR99999*/
            
            if debug% = 0% then gosub close_mfenergy   /* Finished        */

            gosub load_results
            goto inputmode

L61550: return 

/* (AWD040) */
        convert_size
          width, height = 0.00
          a1 = 0.0 : a2 = 0.0
          convert str(part$,13%,3%) to a1, data goto CS1
CS1:
          convert str(part$,16%,1%) to a2, data goto CS2
CS2:
          width = a1 + (a2/8.0)           /* Decimal Width    */

          a1 = 0.0 : a2 = 0.0
          convert str(part$,17%,2%) to a1, data goto CS3
CS3:
          convert str(part$,19%,1%) to a2, data goto CS4
CS4:
          height = a1 + (a2/8.0)           /* Decimal Height   */
        return
/* (\AWD040) */

        check_samples
            ss% = 0%
            if str(part$,1%,1%) = "9" then return

            if len(part$) < 20 then goto LS2      /* Quick Test      */
            convert str(part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */
                                                     /*   (EWD007)      */
            if str(part$,7%,2%) > "99" then goto LS1

        return                                       /* Code Found      */
LS1:        convert str(part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
                                                     /* Code Found      */
        return
LS2:        ss% = 0%
        return
                                                     /* (EWD009)        */
                                                     /* (PAR000)        */
        check_grid                                   /* Check for Grid  */
            gg% = 0%                                 /* No Grid         */
            init(" ") grid$
            grid$  = str(part$,7%,2%)                /* Check Liting    */

            if grid$ = "00" then goto GG1            /* No Grid - Exit  */
                                                     /* Check Liting for*/
                                                     /* no Grid 1st digit*/
            p% = pos("ABCDEFGHIJKLMNOPQRSTUVWXYZ" = str(grid$,1%,1%) )
                                                     /* 1st Alpha and the*/
                                                     /* 2nd Zero-No Grid*/
            if p% <> 0% and str(grid$,2%,1%) = "0" then goto GG1

                                                     /* Check Grid Sizr    */
                                                     /* (GRDSIZE) Table    */
                                                     /* 1% = 5/8 Inch Grid */
            if str(sub_part$,2%,1%) = "1" then gg% = 1%

                                                     /* 2% = 3/4 Inch Grid */
REM            if str(sub_part$,2%,1%) = "2" then gg% = 2%
/* (AWD022)  3/4 inch grid is less than 3/4 inch so use 5/8 */
            if str(sub_part$,2,1) = "2" then gg% = 1%

                                                     /* 3% = 1 Inch Grid   */
REM            if str(sub_part$,2%,1%) = "3" then gg% = 3%
/* (AWD022)  1 inch grid is less than 1 inch so use 5/8 */
            if str(sub_part$,2,1) = "3" then gg% = 1%

                                                     /* 4% = 3/8 Inch Grid */
/* (AWD022)  3/8 inch grid is less than 3/ inch so use 5/8 */
REM            if str(sub_part$,2%,1%) = "4" then gg% = 4%
            if str(sub_part$,2%,1%) = "4" then gg% = 1%


                                                     /* 5% = SDL Grid      */
REM         if str(sub_part$,1,1) = "4" then gg% = 5%          /* (AWD018) */
            if str(sub_part$,8,2) = "11" then gg% = 5%         /* (AWD020) */

/* (AWD028) */
/* New gg for coutour Florida windows group codes */

            if (str(part$,1,1) = "F" or str(part$,1,1) = "S")    ~
                and (str(sub_part$,2,1) = "3" and                ~
                     str(sub_part$,1,1) = "2") then gg% = 6%

            if (str(part$,1,1) = "F" or str(part$,1,1) = "S")    ~
                and (str(sub_part$,2,1) = "1" and                ~
                     str(sub_part$,1,1) = "2") then gg% = 6%

/* (AWD031) check gencodes table PLNCNTGRP table to determine */
/* if model has 3/4 inch glass, if so then 1 inch grid */
/* has group code 6                                    */
            cntgrp% = 0%
            if str(sub_part$,2,1) = "3" and                ~
                 str(sub_part$,1,1) = "2" then gosub checkPLNCNTGRP
            if cntgrp% = 1% then gg% = 6%
/* (/AWD031) */

        return
GG1:        gg% = 0%                                 /* No Grid Found   */
        return                                       /* (PAR000)        */

/* (AWD033) */
        checkGlThick
          gt% = 0%              /* This is 5/32 or 3.9 mm glass */
          if str(sub_part$,14,1) = "5" then gt% = 1%
         return


        checkPolystrene
          pl% = 0%              /* This is a polystrene window */
          if str(sub_part$,5,1) = "2" then pl% = 1%
        return
/* (AWD033\) */

        check_cont_head                              /* Check for the No*/
                                                     /* of Labels to Prt*/
            combo%, chs% = 0%                        /* (AWD029) */

            no_label% = 1%                           /* Default is One  */
            init(" ") readkey$, desc$, glass$
            glass$ = str(part$,5,2)                  /* (AWD029) */
            str(readkey$,1%,9%)   = "PLAN CONT"
            str(readkey$,10%,15%) = str(part$,1%,3%)
            read #4,key = readkey$, using L51000, desc$, eod goto CC2

            ch% = 1%
            convert str(desc$,30%,1%) to ch%, data goto CC1
CC1:
            no_label% = ch%

/* (AWD029) */
            chs% = 1%
            if glass$ = "LA" or glass$ = "LE" then goto combo_glass
            if glass$ = "LB" or glass$ = "LC" then goto combo_glass /* (AWD034) */
            if glass$ = "RA" or glass$ = "RE" then goto combo_glass
            if glass$ = "RB" or glass$ = "RC" then goto combo_glass /* (AWD034) */
            if glass$ = "BT" or glass$ = "BU" then goto combo_glass /* (AWD039) */
            if glass$ = "BV" or glass$ = "BW" then goto combo_glass /* (AWD039) */
/* (\AWD029) */  /* (AWD039) */

CC2:    return

/* (AWD029) */
        combo_glass        /* (AWD034) */
            combo% = 1%
            if glass$ <> "LA" then goto combo1
                gl1$ = "C6"
                gl2$ = "B4"
                gl3$ = "B4"
              return
combo1:     if glass$ <> "LB" then goto combo2
                gl1$ = "Q3"
                gl2$ = "Q6"
                gl3$ = "Q6"
              return
combo2:     if glass$ <> "LC" then goto combo3
                gl1$ = "08"
                gl2$ = "A0"
                gl3$ = "A0"
              return
combo3:     if glass$ <> "LE" then goto combo4
                gl1$ = "C5"
                gl2$ = "B3"
                gl3$ = "B3"
              return
combo4:     if glass$ <> "RA" then goto combo5
                gl1$ = "B4"
                gl2$ = "B4"
                gl3$ = "C6"
              return
combo5:     if glass$ <> "RB" then goto combo6
                gl1$ = "Q6"
                gl2$ = "Q3"
                gl3$ = "Q3"
              return
combo6:     if glass$ <> "RC" then goto combo7
                gl1$ = "A0"
                gl2$ = "A0"
                gl3$ = "08"
              return
combo7:     if glass$ <> "RE" then goto combo8
                gl1$ = "B3"
                gl2$ = "B3"
                gl3$ = "C5"
/* (AWD039) */
combo8:     if glass$ <> "BT" then goto combo9
                gl1$ = "AV"
                gl2$ = "AR"
                gl3$ = "AR"
combo9:     if glass$ <> "BU" then goto combo10
                gl1$ = "AR"
                gl2$ = "AR"
                gl3$ = "AV"
combo10:    if glass$ <> "BV" then goto combo11
                gl1$ = "AB"
                gl2$ = "AF"
                gl3$ = "AF"
combo11:    if glass$ <> "BW" then goto combo12
                gl1$ = "AF"
                gl2$ = "AF"
                gl3$ = "AB"
combo12:
/* (\AWD039) */

return             /* (\AWD034) */
/* (\AWD029) */

/* (AWD031) */
        checkPLNCNTGRP
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLNCNTGRP"
            str(readkey$,10%,3%) = str(part$,1%,3%)
            read #4,key = readkey$, eod goto noPLNCNTGRP
                  cntgrp% = 1%
        noPLNCNTGRP
        return

/* (\AWD031) */

check_for_lowes_stock:

/*  #2  AWD016
 sku#       1- 16 ch(16) = key0
 upc#      17- 36 ch(20) = key1
 part      37- 61 ch(25) = key2
 sub part  62- 81 ch(20)
 model     82- 87 ch(06)
 desc      88-122 ch(35)
filler    123-160 ch(38)
*/
            init(" ") sku_key$, low_sku$
            str(sku_key$,01,25) = part$
            str(sku_key$,26,11) = sub_part$
            read #2,key 2 = sku_key$, using AWDSKUXR, low_sku$, eod goto not_lowes
AWDSKUXR:      FMT POS(05), CH(11)
                energy_star% = 2%

not_lowes:
            return
                                                     /* (EWD009)        */

        set_file_name
            init(" ") file$, script$, library$, volume$
                                                    /* (PAR003)        */
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #4,           /* GENCODES                   */~
                           err% )        /* error                      */

            if err% = 0% then goto SS_1
               errormsg$ = "(Error) Schema Lookup Error)"
               gosub error_prompt
               return clear all
               goto inputmode

SS_1:                                                /* (PAR003)        */
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "MFENERGY"
               script$  = "MFENERGY"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */
SS_2:
               file$    = "NEENERGY"
               script$  = "NEENERGY"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
REM            file$   = "MFGTEST"                   /* (RHHTEST        */
REM            script$ = "MFGTEST"                   /* Atrium N.C.     */

                                                     /* (PAR004)        */
        REM    file$   = "NEATEST"                   /* (RHHTEST)       */
        REM    script$ = "NEATEST"                   /* North East      */

            gosub open_file

                                                     /* (EWD014)        */
                                                     /* (PAR003)        */
        return

        open_file                                    /* (PAR003)        */
            call "OPENFILE" (#1, "IO   ", f2%(1%), rslt$(1%), axd$ )
            if f2%(1%) <> 0% then goto L30100
               gosub file_exists
               if comp% <> 16% then goto exit_program
                  call "FILEBGON" (#1)

L30100:    open nodisplay #1, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        close_mfenergy
            lb1% = 0% : lb2% = 0%

            close #1

            call "LINK" addr(script$, lb1%, lb2%)
                if lb1%    > 0% then err% = 4%

            call "FILEBGON" (#1)          /* Scratch 'MFENERGY'   */
                                          /* or 'NEENERGY'        */
        return

        file_exists
          comp% = 2%
          hdr$ = "*** Energy Star File Exists **"
                                          /* (PAR003)             */
          if schema% = 1% then                                          ~
             msg$(1%) = "        The File (MFENERGY) Already Exists.      "~
                          else                                             ~
             msg$(1%) = "        The File (NEENERGY) Already Exists.      "
                                          /* (PAR003)             */
          msg$(2%) = "       E n e r g y   S t a r   L a b e l s       "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
         
        prt_other
           if debug% <> 0% then return
           
           if donotapply% = 1% then goto L32000
                                             /* (AWD017) - Mod          */
                                             /* (EWD014) Prt Barcode    */
           call "EWDPLA77" (been_oth%,       /* Zero (Only 1st Time)    */~
                            lb_brand$,       /* Brand Name              */~
                            lb_series$,      /* Series Number           */~
                            lb_style$,       /* Vinyl Window Style      */~
                            lb_style2$,      /* Vinyl Window Style      */~
                            lb_glass_op$,    /* Glass Option            */~
                            lb_glass_op1$,   /* Glass Option 2          */~
                            lb_resu$,        /* Resident U-Factor       */~
                            lb_nonresu$,     /* Non-Resident U-Factor   */~
                            lb_resheat$,     /* Resident Heat Coeff     */~
                            lb_nonresheat$,  /* Non-Resident Heat Coeff */~
                            lb_resvisible$,  /* Resident Visible(EWD001)*/~
                            lb_nonresvisible$,/* Non-Resident Visible   */~
                            lb_dp$,          /* Design Pressure         */~
                            lb_width$,       /* Window Width            */~
                            lb_height$,      /* Window Height           */~
                            lb_seq$,         /* Sequence Number         */~
                            lb_dep_so$,      /* Department Sales Order  */~
                            lb_ld_mod$,      /* Load Model Number       */~
                            lb_pd_dte$,      /* Production Date         */~
                            lb_zone$,        /* Wind Zone         EWD006*/~
                            lb_pressure1$,   /* Positive Pressure EWD006*/~
                            lb_pressure2$,   /* Negitive Pressure EWD006*/~
                            lb_txt1$,        /* No Grid, 5/8 or 1 EWD009*/~
                            lb_txt2$,        /* Dual Glazed       EWD009*/~
                            lb_barcode$,     /* Prod Barcode      EWD014*/~
                            lb_thick$,       /* Glass Thickness   EWD015*/~
/* (AWD029) changed to combo_part$ from part$                           */~
                            combo_part$,     /* Part                    */~
                            sub_part$,       /* Sub Part                */~
                            lb_cpdnum$,      /* (AWD035) CPD Number     */~
                            #1,              /* (MFENERGY)              */~
                            #2,              /* (AWDSKUXR)              */~
                            #4,              /* (GENCODES)              */~
                            lb_fl$,          /* Florida Approval Code   */~
                            lb_tdi$,         /* (AWD040) TDI Number     */~
                            stc$,            /* (AWD043)                */~
                            ctrlflag$,       /* Control the tear off    */~
                            err%)            /* Return Code             */

        REM   lb_nonresvisible$ contains lb_resair$ Value   /* (EWD008) */
                                             /* (AWD017)                */
                str(msg$(2%),14%,8%) = "EWDPLA77"
                if err% <> 0% then gosub print_error
                goto L32010
/* CR3059 */                
L32000:              
           call "EWDPLD77" (been_donot%,    /* Zero (Only 1st Time)    */~
                            lb_brand$,       /* Brand Name              */~
                            lb_series$,      /* Series Number           */~
                            lb_style$,       /* Vinyl Window Style      */~
                            lb_style2$,      /* Vinyl Window Style      */~
                            lb_glass_op$,    /* Glass Option            */~
                            lb_glass_op1$,   /* Glass Option 2          */~
                            lb_resu$,        /* Resident U-Factor       */~
                            lb_nonresu$,     /* Non-Resident U-Factor   */~
                            lb_resheat$,     /* Resident Heat Coeff     */~
                            lb_nonresheat$,  /* Non-Resident Heat Coeff */~
                            lb_resvisible$,  /* Resident Visible(EWD001)*/~
                            lb_nonresvisible$,/* Non-Resident Visible   */~
                            lb_dp$,          /* Design Pressure         */~
                            lb_width$,       /* Window Width            */~
                            lb_height$,      /* Window Height           */~
                            lb_seq$,         /* Sequence Number         */~
                            lb_dep_so$,      /* Department Sales Order  */~
                            lb_ld_mod$,      /* Load Model Number       */~
                            lb_pd_dte$,      /* Production Date         */~
                            lb_zone$,        /* Wind Zone         EWD006*/~
                            lb_pressure1$,   /* Positive Pressure EWD006*/~
                            lb_pressure2$,   /* Negitive Pressure EWD006*/~
                            lb_txt1$,        /* No Grid, 5/8 or 1 EWD009*/~
                            lb_txt2$,        /* Dual Glazed       EWD009*/~
                            lb_barcode$,     /* Prod Barcode$     EWD014*/~
                            lb_thick$,       /* Glass Thickness   EWD015*/~
                            combo_part$,     /* Part              AWD023*/~
                            sub_part$,       /* Sub Part          AWD023*/~
                            lb_cpdnum$,      /* (AWD035) CPD Number     */~
                            #1,              /* (MFENERGY)              */~
                            #2,              /* (AWDSKUXR)        AWD023*/~
                            #4,              /* (GENCODES)        AWD024*/~
                            lb_fl$,          /* Florida Approval Code   */~
                            lb_tdi$,         /* (AWD040) TDI Number     */~
                            energy_star%,    /* (AWD027) which region   */~
                            door%,           /* (AWD038)                */~
                            stc$,            /* (AWD043)                */~
                            ctrlflag$,       /* Control the tear off    */~                           
                            err%)            /* Return Code             */  
                str(msg$(2%),14%,8%) = "EWDPLD77"
                if err% <> 0% then gosub print_error
L32010: return

        prt_energy

           if debug% <> 0% then return
                                             /* (AWD017) - Mod          */
                                             /* (EWD014) Prt Barcode    */
           call "EWDPLC77" (been_energy%,    /* Zero (Only 1st Time)    */~
                            lb_brand$,       /* Brand Name              */~
                            lb_series$,      /* Series Number           */~
                            lb_style$,       /* Vinyl Window Style      */~
                            lb_style2$,      /* Vinyl Window Style      */~
                            lb_glass_op$,    /* Glass Option            */~
                            lb_glass_op1$,   /* Glass Option 2          */~
                            lb_resu$,        /* Resident U-Factor       */~
                            lb_nonresu$,     /* Non-Resident U-Factor   */~
                            lb_resheat$,     /* Resident Heat Coeff     */~
                            lb_nonresheat$,  /* Non-Resident Heat Coeff */~
                            lb_resvisible$,  /* Resident Visible(EWD001)*/~
                            lb_nonresvisible$,/* Non-Resident Visible   */~
                            lb_dp$,          /* Design Pressure         */~
                            lb_width$,       /* Window Width            */~
                            lb_height$,      /* Window Height           */~
                            lb_seq$,         /* Sequence Number         */~
                            lb_dep_so$,      /* Department Sales Order  */~
                            lb_ld_mod$,      /* Load Model Number       */~
                            lb_pd_dte$,      /* Production Date         */~
                            lb_zone$,        /* Wind Zone         EWD006*/~
                            lb_pressure1$,   /* Positive Pressure EWD006*/~
                            lb_pressure2$,   /* Negitive Pressure EWD006*/~
                            lb_txt1$,        /* No Grid, 5/8 or 1 EWD009*/~
                            lb_txt2$,        /* Dual Glazed       EWD009*/~
                            lb_barcode$,     /* Prod Barcode$     EWD014*/~
                            lb_thick$,       /* Glass Thickness   EWD015*/~
/* (AWD029) changed to combo_part$ from part$                           */~
                            combo_part$,     /* Part              AWD023*/~
                            sub_part$,       /* Sub Part          AWD023*/~
                            lb_cpdnum$,      /* (AWD035) CPD Number     */~
                            #1,              /* (MFENERGY)              */~
                            #2,              /* (AWDSKUXR)        AWD023*/~
                            #4,              /* (GENCODES)        AWD024*/~
                            lb_fl$,          /* Florida Approval Code   */~
                            lb_tdi$,         /* (AWD040) TDI Number     */~
                            energy_star%,    /* (AWD027) which region   */~
                            door%,           /* (AWD038)                */~
                            stc$,            /* (AWD043)                */~
                            ctrlflag$,       /* Control the tear off    */~                           
                            err%)            /* Return Code             */

        REM   lb_nonresvisible$ contains lb_resair$ Value   /* (EWD008) */
                                             /* (AWD017)                */
                str(msg$(2%),14%,8%) = "EWDPLB77"

                if err% <> 0% then gosub print_error

        return


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                            /* (PAR001)                */
L35040:     FMT 4*CH(256)                   /* Label Data     EWDPRDLB */

L35050:     FMT CH(3),                      /* Model Code              */~
                CH(3),                      /* Group Code              */~
                PD(14,4),                   /* Redidential Value       */~
                PD(14,4),                   /* Non-Residential         */~
                CH(2),                      /* Design Pressure         */~
                CH(12),                     /* Design Pressure         */~
                CH(44)                      /* Design Pressure         */

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
              on fieldnr% gosub L40170,          /* sc_prddate$        */~
                                L40160,          /* sc_dept$           */~
                                L40160,          /* sc_shift$          */~
                                L40160,     /* sc_load_fr$,sc_load_to$ */~
                                L40160,     /* sc_barcode_fr$ & _to$   */~
                                L40170      /* sc_seq_fr$ & _to$       */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Production Date  :",                         ~
               at (03,25), fac(lfac$(1%)), sc_prddate$          , ch(10),~
                                                                         ~
               at (04,02), "Dept. Code       :",                         ~
               at (04,25), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,02), "Shift Code       :",                         ~
               at (05,25), fac(lfac$(3%)), sc_shift$            , ch(02),~
                                                                         ~
               at (06,02), "Load No. Range   :",                         ~
               at (06,25), fac(lfac$(4%)), sc_load_fr$          , ch(05),~
               at (06,45), fac(lfac$(4%)), sc_load_to$          , ch(05),~
                                                                         ~
               at (07,02), "Barcode No. Range:",                         ~
               at (07,25), fac(lfac$(5%)), sc_barcode_fr$       , ch(18),~
               at (07,45), fac(lfac$(5%)), sc_barcode_to$       , ch(18),~
                                                                         ~
               at (08,02), "Sequence Range   :",                         ~
               at (08,25), fac(lfac$(6%)), sc_seq_fr$           , ch(05),~
               at (08,45), fac(lfac$(6%)), sc_seq_to$           , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
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
            on fieldnr% gosub L50010,        /* Production Date        */~
                              L50050,        /* Department Code        */~
                              L50080,        /* Shift Code             */~
                              L50090,        /* Load No. Range         */~
                              L50200,        /* Barcode Range          */~
                              L50300         /* Sequence Number Range  */

            return

L50010: Rem Enter a Production Date                sc_prddate$
            call "DATEOKC" (sc_prddate$, 0%, errormsg$)
            return

L50050: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if sc_dept$ <> " " then goto L50055
               sc_dept$ = "ALL"

L50055:     if sc_dept$ <> "ALL" then goto L50060
                sc_dept_d$ = "*** All Departments"
                return
L50060:     gosub check_dept
            if dept% = 0% then goto L50070
               sc_dept_d$ = desc$
        return

L50070:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return

L50080: Rem Enter a Shift Code                      sc_shift$
            if sc_shift$ = " " then sc_shift$ = "AA"
            return

L50090: Rem Enter a Load No. Range               sc_load_fr$, sc_load_to$
            if str(sc_load_fr$,1%,1%) <> " " then goto L50095
               sc_load_fr$ = "ALL  "
               sc_load_to$ = sc_load_fr$

L50095:     if str(sc_load_fr$,1%,3%) <> "ALL" then goto L50098
               sc_load_to$ = "ALL  "
               return
L50098:     if sc_load_to$ = " " then sc_load_to$ = sc_load_fr$
            if sc_load_fr$ > sc_load_to$ then goto L50100
        return
L50100:     errormsg$ = "'TO' Load No. must be > or = 'FROM' Load No."
            gosub error_prompt
            init(" ") sc_load_fr$, sc_load_to$
        return

L50200: Rem Enter a Barcode Range             sc_barcode_fr$, sc_barcode_to$
            if str(sc_barcode_fr$,1%,1%) <> " " then goto L50205
               sc_barcode_fr$ = "ALL Barcodes      "
               sc_barcode_to$ = sc_barcode_fr$

L50205:     if str(sc_barcode_fr$,1%,3%) <> "ALL" then goto L50210
               sc_barcode_to$ = sc_barcode_fr$
                                                   /* (AWD016)        */
               if userid$ = "WWW" then goto L50215
               return

L50210:     if len(sc_barcode_to$) < 3 then                         ~
               sc_barcode_to$ = sc_barcode_fr$

            lb_barcode$ = sc_barcode_fr$
            gosub check_barcode
            if barcode% = 0% then goto L50230

            lb_barcode$ = sc_barcode_to$
            gosub check_barcode
            if barcode% = 0% then goto L50230

            if sc_barcode_fr$ > sc_barcode_to$ then goto L50220
                                                    /* (AWD016)          */
L50215:
            if userid$ <> "WWW" then return
               if str(sc_barcode_fr$,1%,3%) = "ALL" then goto L50250
               if sc_barcode_to$ <> sc_barcode_fr$  then goto L50250
                                                    /* (AWD016)          */
        return
L50220:     errormsg$ = "'TO' Barcode No. must be > or = 'FROM' Barcode No."
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
L50230:     errormsg$ = "(Error) Invalid Barcode, or Not on File"
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
                                                    /* (AWD016)          */
L50250:     errormsg$ = "(Error) Invalid Barcode, Can Only Print Single Label?"
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return

        
/* CR2835 */
L50300:  REM Enter sequence number
            if str(sc_seq_fr$,1%,1%) <> " " then goto L50305
               sc_seq_fr$ = "ALL  "
               sc_seq_to$ = sc_load_fr$

L50305:     if str(sc_seq_fr$,1%,3%) <> "ALL" then goto L50308
               sc_seq_to$ = "ALL  "
               return
L50308:     if sc_seq_to$ = " " then sc_seq_to$ = sc_seq_fr$
            if sc_dept$ = "ALL" then goto L50320
            convert sc_seq_fr$ to seqfr%, data goto  L50315
            convert sc_seq_to$ to seqto%, data goto  L50315
            if seqfr% > seqto% then goto L50310
            if len(sc_seq_fr$) < 5 and sc_seq_fr$ <> "ALL" then ~
                 convert seqfr% to sc_seq_fr$, pic(00000)
            if len(sc_seq_to$) < 5 and sc_seq_to$ <> "ALL" then ~
                 convert seqto% to sc_seq_to$, pic(00000)
        return
        
L50310:     errormsg$ = "'TO' Seq No. must be > or = 'FROM' Seq No."
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return
L50315:     errormsg$ = "Seq No. must be numeric."
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return        
L50320:     errormsg$ = "Dept required with Seq No."
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return
        
        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #4,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return

        check_barcode
            barcode% = 0%
            init(" ") lb_key1$
            str(lb_key1$,1%,18%) = lb_barcode$            /* (PAR001) */
            read #5,key 1% > lb_key1$, using L52000, lb_key1$, eod goto L52010
L52000:        FMT POS(278), CH(23)
            if str(lb_key1$,1%,18%) <> lb_barcode$ then goto L52010
               barcode% = 1%
L52010: return

        lookup_glass
            init(" ") readkey$, desc$, lb_glass_op$, lb_glass_op1$
            glass% = 0%
            if part% < 19% then goto L52110                 /* Skip  */
            if gl$ = "89" or gl$ = "99" then goto L52110    /* Skip  */

            str(readkey$,1%,9%)  = "GLASSES  "
            str(readkey$,10%,2%) = gl$
            read #4,key = readkey$, using L52100, desc$, eod goto L52110
L52100:        FMT POS(25), CH(30)
            p% = pos(desc$ = "/")
            if p% = 0% then lb_glass_op$ = desc$
            if p% = 0% then goto L52105
               lb_glass_op$ = str(desc$,1%, p%-1%)
               lb_glass_op1$ = str(desc$,p%+1%,)

L52105:     glass% = 1%

L52110:     if debug% = 0% then return
               call "SHOSTAT" ("Glass Desc = " & lb_glass_op$) : stop

        return

        lookup_series_style
            init(" ") readkey$, desc$, p_series$, lb_series$, lb_style$, lb_style2$

            if part% < 19% then goto L52120
                                                  /* (PAR001)    */
            p_series$ = str(lb_rec$(),270%,8%)
/*<AWD042>+*/
            pp% = pos(p_series$ = " ")
            if str(bcksubpt_rec$,169%,1%) = " " or                           ~
               str(bcksubpt_rec$,169%,1%) = hex(00) then normal_series_style
            if str(bcksubpt_rec$,185%,1%) = " " or                           ~
               str(bcksubpt_rec$,185%,1%) = hex(00) then normal_series_style
                  p_series$ = str(bcksubpt_rec$,169%,16%)    /* CR1251 */
                  str(readkey$,1%,9%) = "ELLISON03"
/*AWD046*/        str(readkey$,10%,10%) = str(bcksubpt_rec$,185%,10%)
/*AWD046*/        lb_style$ = str(bcksubpt_rec$,185%,10%) 
/*AWD046*/        lb_style2$ = str(bcksubpt_rec$,185%,10%) 
                  lb_series$ = "SERIES: " & p_series$
/*AWD046*/        goto bcksubpt_series_style
/*                goto L52120      AWD046   */
/*<AWD042>-*/
        normal_series_style
            lb_series$ = "SERIES: " & str(p_series$,1%, pp% - 1%)
/*<AWD042>+*/
            str(readkey$,1%,9%) = "ELLISON03"
            pp2% = 12% - pp%
            lb_style2$ = str(p_series$,pp%+1%,pp2%)
            str(readkey$,10%,pp2%) = str(p_series$,pp%+1%,pp2%)
        bcksubpt_series_style
            read #4,key = readkey$, using L52100, desc$, eod goto L52120
            str(lb_style$,1%,24%) = "VINYL " & str(desc$,1%,18%)

            if str(p_series$,1%,4%) = "4000" or str(p_series$,1%,4%) = "4002" ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
            if str(p_series$,1%,4%) = "300 " or str(p_series$,1%,4%) = "330 " ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
            if str(p_series$,1%,4%) = "375 "                                 ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)

            if str(p_series$,1%,3%) = "85 " or str(p_series$,1%,3%) = "95 " ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
            if str(p_series$,1%,3%) = "95 "                                 ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
/*AWD042*/  if str(p_series$,1%,4%) = "330 " or str(p_series$,1%,5%) = "R4000"~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)


L52120:     if debug% = 0% then return
               call "SHOSTAT" ("Brand  = " & lb_brand$)  : stop
               call "SHOSTAT" ("Series = " & lb_series$) : stop
               call "SHOSTAT" ("Style  = " & lb_style$)  : stop
        return

        check_for_mull
            mull% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLANMONLY"
            str(readkey$,10%,15%) = str(part$,1%,3%)
            read #4,key = readkey$, eod goto L52200
               mull% = 1%
L52200: return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
                                                        /* (PAR000)     */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
            flag$ = "0"                  /* Sales Order Info           */
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

            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000                    "
            if err1% = 0% then return

            err1% = 0%
        return
                                                         /* (PAR000)    */
        load_table
           init(" ") readkey$, desc$, u_gl$(), u_grp$(), s_gl$(), s_grp$(),~
                     group$, gl$, v_gl$(), v_grp$(), a_gl$(), a_grp$()
                                               /* (EWD009)               */
           init(" ") s5_gl$(), s5_grp$(), s1_gl$(), s1_grp$(), v5_gl$(), ~
                     v5_grp$(), v1_gl$(), v1_grp$(), gl$
           init(" ") u5_gl$(), u5_gl$(), u1_gl$()     /* <AWD020> */
           init(" ") u5_grp$(), u5_grp$(), u1_grp$()  /* <AWD020> */
           init(" ") uf_gl$(), sf_gl$(), vf_gl$()     /* <AWD028> */
           init(" ") uf_grp$(), sf_grp$(), vf_grp$()  /* <AWD028> */
           init(" ") vs_gl$(), vs_grp$(), ss_gl$(), ss_grp$() /* (AWD018) */
/* (AWD033) */
           init(" ") tu_gl$(), tu_grp$(), ts_gl$(), ts_grp$(), tv_gl$(), ~
              tv_grp$(), tu5_gl$(), tu5_grp$(), ts5_gl$(), ts5_grp$(),   ~
              tv5_gl$(), tv5_grp$(), tu1_gl$(), tu1_grp$(), ts1_gl$(),   ~
              ts1_grp$(), tv1_gl$(), tv1_grp$(), tdu_gl$(), tdu_grp$(),  ~
              tds_gl$(), tds_grp$(), tdv_gl$(), tdv_grp$(), pu_gl$(),    ~
              pu_grp$(), ps_gl$(), ps_grp$(), pv_gl$(), pv_grp$(),       ~
              pu5_gl$(), pu5_grp$(), ps5_gl$(), ps5_grp$(), pv5_gl$(),   ~
              pv5_grp$(), pud_gl$(), pud_grp$(), psd_gl$(), psd_grp$(),  ~
              psv_gl$(), psv_grp$(), pu1_gl$(), pu1_grp$(), ps1_gl$(),   ~
              ps1_grp$(), pv1_gl$(), pv1_grp$()

                                               /* (EWD009)               */
                                               /* (EWD001) Visible Trans */
                                               /* (EWD008) Air Leakage   */
                                               /* (EWD009)               */
                                               /* (EWD009)               */


        return


        format_label
              energy_star% = 0%                        /* Energy Star Flg*/

              if debug% = 0% then goto L60095
                 call "SHOSTAT" ("Part = " & part$)
                 stop
                                                       /* (PAR001)       */
L60095:       gl$ = str(lb_rec$(),527%,2%)             /* Product Glass  */
                                                       /* Code           */
/* (AWD029) */
              init(" ") combo_part$
              combo_part$ = part$
              if combo% = 1% and no_label% = 3% then gl$ = gl3$
              if combo% = 1% and no_label% = 3% then            ~
                                    str(combo_part$,5,2) = gl3$
              if combo% = 1% and no_label% = 2% then gl$ = gl2$
              if combo% = 1% and no_label% = 2% then            ~
                                    str(combo_part$,5,2) = gl2$
              if combo% = 1% and no_label% = 1% then gl$ = gl1$
              if combo% = 1% and no_label% = 1% then            ~
                                    str(combo_part$,5,2) = gl1$
/* (\AWD029) */
                                                       /* (PAR001)       */
              lb_brand$ = str(lb_rec$(),301%,10%)      /* Brand Name     */
          /* <AWD032> */
              cuscode$ = str(lb_rec$(),727%,9%)     
REM           if str(lb_rec$(),628,6) = "BA111 " then        
              if cuscode$ = "BA111    " then          ~
                            lb_brand$ = "Bay/Bow"
          /* </AWD032> */
          /* (AWD036) */
REM           if str(lb_rec$(),628,6) = "CV0999" then               ~
              if cuscode$ = "CV0999   " then          ~
                            lb_brand$ = "********"
          /* (\AWD036) */
          /* (AWD041)*/
REM           if str(lb_rec$(),628,6) = "TH9050" then               ~
              if cuscode$ = "TH9050   " then          ~
          	                lb_brand$ = "THERMAL"
                                                       /*  (EWD005)      */
REM           if str(lb_brand$,1%,2%) = "R." then                        ~
REM                                         lb_brand$ = " REYNOLDS "

/* CR1251 */  if str(bcksubpt_rec$,169%,16%) = "American_Classic" then ~
                  lb_brand$ = "          "
                  
              gosub lookup_series_style
                
                                                       /* Series Name    */
                                                       /* Style          */

              gosub lookup_glass                       /* Glass Option   */

              door% = 0%                               /* (AWD033) */
              gosub lookupDoor                         /* (AWD033) */
              if door% = 1% then str(sub_part$,14%,1%) = "0" /*(CR1007)*/

              gosub lookup_ufactor
                                                       /* Resident U-Fac */
                                                       /* NonRes U-Fac   */
              gosub lookup_heatc
                                                       /* Res Heat Coeff */
                                                       /* Non-Res Heat   */


              gosub lookupIsEnergy                     /* (AWD033) */

              gosub lookup_visible                     /* (EWD001)       */
                                                       /* Res Visible Tran*/
                                                       /* Non-Res Visible Trans */

              gosub lookup_air_leakage                 /* (EWD008)       */
                                                       /* Res Visible Tran*/
                                                       /* Non-Res Visible Trans */
                                                       /* Non-Resnot Applic */
              gosub lookup_design
                                                       /* Design Press   */
                                                       /* Width          */
                                                       /* Width          */

                                                       /* Production Seq */
                                                       /* (PAR001)       */
              lb_seq$    = "SEQUENCE : " &str(lb_rec$(),311%,5%)
                                                       /* Dept/S.O.      */
                                                       /* (PAR001)       */
              lb_dep_so$ = "DEPT:"&str(lb_rec$(),296%,3%)& "  S.O.:" &     ~
                                   str(lb_rec$(),357%,8%)
                                                       /* (PAR001)       */
              lb_ld_mod$ = "LOAD:"&str(lb_rec$(),22%,5%)&    " MOD:" &     ~
                                   model$

              init(" ") lb_dte$                        /* (PAR001)       */
              lb_dte$ = str(lb_rec$(),1%,6%)
              call "DATFMTC" (lb_dte$)
              lb_pd_dte$ = "Product Date: " & lb_dte$
/* (AWD019) take off */
                                                       /* (EWD006)       */
REM              gosub lookup_wind_zone
                                                       /* (EWD006)       */

                                                       /* (EWD009)       */

              gosub lookup_thickness                   /* (EWD015)       */
                                                       /* (PAR000)       */
              init(" ") lb_txt1$, lb_txt2$
                  lb_txt1$ = "*No Grid* "
/* (AWD022) - begin */
                  if gg% = 1% then lb_txt1$ = "*5/8 Grid*"
                  if gg% = 1% and str(sub_part$,2%,1%) = "2" then ~
                                           lb_txt1$ = "*3/4 Grid*"

                  if gg% = 1% and str(sub_part$,2%,1%) = "3" then ~
                                           lb_txt1$ = "* 1  Grid*"

                  if gg% = 1% and str(sub_part$,2%,1%) = "4" then ~
                                           lb_txt1$ = "*3/8 Grid*"

/* (AWD037) */
                  if gg% = 6% then lb_txt1$ = "*5/8 Grid*"
                  if gg% = 6% and str(sub_part$,2%,1%) = "2" then ~
                                           lb_txt1$ = "*3/4 Grid*"

                  if gg% = 6% and str(sub_part$,2%,1%) = "3" then ~
                                           lb_txt1$ = "* 1  Grid*"

                  if gg% = 6% and str(sub_part$,2%,1%) = "4" then ~
                                           lb_txt1$ = "*3/8 Grid*"
/* (/AWD037) */

REM                  if gg% = 2% then lb_txt1$ = "*3/4 Grid*"
REM                  if gg% = 3% then lb_txt1$ = "* 1  Grid*"
REM                  if gg% = 4% then lb_txt1$ = "*3/8 Grid*"
/* (AWD022) - end */
/* (AWD018) */
                  if gg% = 5% then lb_txt1$ = "*SDL Grid*"

                  if gg% = 6% and str(sub_part$,2%,1%) = "3" then ~
                                           lb_txt1$ = "* 1  Grid*"

                  lb_txt2$ = "DualGlazed"
                                                       /* (EWD009)       */
                                                       /* (PAR000)       */
              if debug% = 0% then return
                 call "SHOSTAT" ("Seq. = " & lb_seq$) : stop
                 call "SHOSTAT" ("Dept/S.O. = " & lb_dep_so$) : stop
                 call "SHOSTAT" ("Load/Model = "& lb_ld_mod$) : stop
                 call "SHOSTAT" ("Prod Date = " & lb_pd_dte$) : stop
        return

        lookup_ufactor
              U_Fac = 0.00                      /* (AWD033) */
              lb_resu$    = " - "                    /* Resident U-Fac */
              lb_nonresu$ = " - "                    /* NonRes U-Fac   */
        return
/* (AWD033\) */

        lookup_heatc                                   /* (EWD009)  Mode */
              SHGC = 0.00                              /* (AWD033) */
              lb_resheat$    = " - "                   /* Res Heat Coeff */
              lb_nonresheat$ = " - "                   /* Non-Res Heat   */
        return

/* (AWD033\) */

        lookup_design
              init(" ") eg_key$, lb_dp$, lb_width$, lb_height$, lb_fl$, lb_tdi$
              lb_dp$ = "NA"                            /* Design Press   */
              g022_tdi$ = " "                          /* CR1066 */

              if part% < 19% then goto L60300

              gosub lookup_lamn

              str(eg_key$,1%,3%) = model$
              str(eg_key$,4%,3%) = "000"
              gosub lookup_data
/*SR68667     if eg_model% = 1% then lb_fl$ = eg_fl$               */ 
/*SR68667*/   if eg_model% = 1% and str(lb_rec$(),720%, 1%) = "G"           ~
                                    then lb_fl$ = eg_fl$


              if schema% = 1% then gosub NC_Ratings    /* (AWD041) */
              if schema% = 2% then gosub TX_Ratings
              if eg_model% = 0% then goto L60300


                 lb_dp$ = eg_dp$

                 ww% = int(eg_res)                     /* Whole Width    */
                 hh% = int(eg_nonres)                  /* Whole Height   */

                 ww1 = eg_res - ww%                    /* Width Fraction */
                 hh1 = eg_nonres - hh%                 /* Height Franction*/

                 for f0% = 1% to 8%
                     if sze(f0%) > ww1 then goto LFO
                 next f0%

LFO:             f0% = f0% - 1%
                 for f1% = 1% to 8%
                     if sze(f1%) > hh1 then goto LF1
                 next f1%

LF1:             f1% = f1% - 1%
                 convert ww% to str(lb_width$,1%,3%), pic(###)

                 convert hh% to str(lb_height$,1%,2%), pic(##)

                 str(lb_width$,4%,1%) = " "    /* Build Width with Fraction  */
                 if f0% > 0% then                                         ~
                            str(lb_width$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
                 str(lb_height$,3%,1%) = " "   /* Build Height with Fraction */
                 if f1% > 0% then                                         ~
                            str(lb_height$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
L60300:

/* (AWD040) */
              str(eg_key$,1%,3%) = model$
              str(eg_key$,4%,3%) = "001"
              if gls_lamn% = 1% then str(eg_key$,4%,3%) = "004"  /* Lamn */
              gosub lookup_data
/*SR68667     if eg_model% = 1% then lb_tdi$ = eg_fl$          */ 
/*SR68667*/   if eg_model% = 1% and str(lb_rec$(),720%, 1%) = "G"           ~
                                    then lb_tdi$ = eg_fl$
/* (\AWD040) */
              if g022_tdi$ <> " " then lb_tdi$ = g022_tdi$      /* CR1066 */
              if debug% = 0% then return
                 call "SHOSTAT" ("Design Pressure = " & lb_dp$) : stop
                 call "SHOSTAT" ("Window Width = " & lb_width$) : stop
                 call "SHOSTAT" ("Window Height = " & lb_height$) : stop

        return

        TX_Ratings
           lb_dp$ = "NA"
           g022_tdi$ = " "
           txRate% = 1%
           str(eg_key$,1%,3%) = model$
/* CR1066 */
           if str(sub_part$,6%,1%) = "2"         and   ~
              str(bcksubpt_rec$,169%,3%) = "150" and   ~
             (str(bcksubpt_rec$,185%,2%) = "SH" or     ~
              str(bcksubpt_rec$,185%,3%) = "2SL") then goto TX150Casing
           if gls_lamn% = 1% then goto TXLamn  /* Lamn Glass */
        TX_Next
           convert txRate% to str(eg_key$,4%,3%), pic(000)
           gosub lookup_data
           if eg_model% = 0% and txRate% = 1% then return
           if eg_model% = 0% and txRate% <> 1% then goto TX_Updte
           lb_dp$ = eg_dp$

           txRate% = txRate% + 1%
           if width > eg_res or height > eg_nonres then TX_Next
        return
        TX_Updte
          eg_model% = 1%  /* previous record found */
        return
        TXLamn
          str(eg_key$,4%,3%) = "004"
          gosub lookup_data
          if eg_model% = 0% then return
          
          lb_dp$ = eg_dp$
        return
/* CR1066 */
        TX150Casing
          str(eg_key$,4%,3%) = "022"
          gosub lookup_data
          if eg_model% = 0% then return
          g022_tdi$ = eg_fl$
          lb_dp$ = eg_dp$
        return 
     


/*  (AWD041) */
        NC_Ratings        /* Check Size Value from '000' lookup */
/* CR1066 */
           g022_tdi$ = " "
           if str(sub_part$,6%,1%) = "2"         and   ~
              str(bcksubpt_rec$,169%,3%) = "150" and   ~
             (str(bcksubpt_rec$,185%,2%) = "SH" or     ~
              str(bcksubpt_rec$,185%,3%) = "2SL") then goto NC150Casing
              
           ncRate% = 20%             /* Second NC lookup value  */
           if width > eg_res or height > eg_nonres then NC_Next
           return
        NC_Next
           str(eg_key$,1%,3%) = model$
           convert ncRate% to str(eg_key$,4%,3%), pic(000)
           gosub lookup_data
           if eg_model% = 0% and ncRate% = 20% then return
           if eg_model% = 0% and ncRate% <> 20% then goto NC_Updte
           lb_dp$ = eg_dp$

           ncRate% = ncRate% + 1%
           if width > eg_res or height > eg_nonres then NC_Next
        return
        NC_Updte
          eg_model% = 1%  /* previous record found */
        return
/* CR1066 */
        NC150Casing
          str(eg_key$,4%,3%) = "022"
          gosub lookup_data
          if eg_model% = 0% then return
          g022_tdi$ = eg_fl$
          lb_dp$ = eg_dp$
        return 
        
/* (\AWD041) */

                                                       /* (EWD001)       */
        lookup_visible
              init(" ") eg_key$, lb_resvisible$, lb_nonresvisible$, vv_grp$
              lb_resvisible$    = " - "
/*            lb_nonresvisible$ = " - "       */
/*AWD045 */   lb_nonresvisible$ = "<0.3"
        return



                                                       /* (EWD008)        */
        lookup_air_leakage
              init(" ") eg_key$, lb_resair$, lb_nonresair$
              lb_resair$        = " - "                /* Res Air Leakage */
              lb_nonresair$     = " - "                /* Non-Res Air Leakage */
        return



                                                 /* (EWD008)            */
                                                 /* (EWD006)            */
        lookup_wind_zone
           init(" ") readkey$, desc$, lb_zone$, lb_pressure1$, lb_pressure2$,~
                     lb_press1$, lb_press2$

           lb_zone$ = "N/A"
           lb_pressure1$ = "    NA"
           lb_pressure2$ = "    NA"

           if lb_dp$ = "NA" then goto lookup_wind_zone_done

           str(readkey$,1%,9%)  = "PLN DPRTE"
           str(readkey$,10%,3%) = "0" & lb_dp$
           read #4,key = readkey$, using L60000, readkey$, desc$,         ~
                                         eod goto lookup_wind_zone_done
L60000:        FMT CH(24), CH(30)
              lb_zone$      = str(desc$,1%,3%)       /* Miles Per Hour    */
              lb_press1$    = str(desc$,14%,6%)      /* Positive Pressure */
              lb_press2$    = str(desc$,22%,6%)      /* Negitive Pressure */
                                                     /* Format-Remove the */
                                                     /* leading zero      */
              lb_pressure1$ = " " & str(lb_press1$,1%,1%) & str(lb_press1$,3%,4%)
              lb_pressure2$ = " " & str(lb_press2$,1%,1%) & str(lb_press2$,3%,4%)
        lookup_wind_zone_done
        return
                                                     /* (EWD006)          */

                                                     /* (EWD013)          */

        lookup_exclude_dept
           init(" ") readkey$, desc$, e_dept$()
           ee% = 0% : e_dept_max% = 0%
           str(readkey$,1%,9%)  = "PLAN ENGR"
        lookup_exclude_dept_next
           read #4,key > readkey$, using L60000, readkey$, desc$,         ~
                                         eod goto lookup_exclude_dept_done

           if str(readkey$,1%,9%) <> "PLAN ENGR" then goto lookup_exclude_dept_done
              ee% = ee% + 1%
              e_dept$(ee%) = str(readkey$,10%,3%)

              goto lookup_exclude_dept_next

        lookup_exclude_dept_done
           e_dept_max% = ee%
        return
                                                     /* (EWD013)          */

                                                     /* (EWD001)          */
        lookup_data
           eg_model% = 0%
           read #3,key = eg_key$, using L35050, eg_model$, eg_group$,~
                                        eg_res, eg_nonres, eg_dp$,   ~
                                        eg_fl$, eg_fill$,            ~
                                        eod goto L64010
           eg_model% = 1%
           if debug% = 0% then return
              convert eg_res to rh1$, pic(0.00)

              convert eg_nonres to rh2$, pic(0.00)

              call "SHOSTAT" ("Model = " & eg_model$ & "Group = " &eg_group$) : stop
              call "SHOSTAT" ("Resident = "& rh1$ & "Non-Res = " &  rh2$    ) : stop

L64010: return

                                                    /* (EWD015)     */
        lookup_thickness
           init(" ") readkey$, desc$
           str(readkey$, 1%,9%)  = "GED 002  "
           str(readkey$,10%,15%) = model$
           read #4,key = readkey$, using L52100, desc$,               ~
                                       eod goto lookup_thickness_done

           lb_thick$    = str(desc$,1%,6%)
/*AWD044*/ if str(sub_part$,1%,1%) = "4" then lb_thick$ = "1.0000"

        lookup_thickness_done

        return
                                                    /* (EWD015)    */
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        load_results
           k% = 2%
           hdr$     = "***** Label Generation Results *****"
           msg$(1%) = "This run generated xxxxx label(s)."
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           convert lbl% to str(msg$(1%),20%,5%), pic(####0)
           if lbl% <> 0% then L64100
               msg$(1%) = "NO LABELS GENERATED!!!"
               str(msg$(3%),,13%) = " Press <PF16>"
L64100:    call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
           lbl% = 0%
        return

        print_error
                                                /* (AWD017)            */
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = "Return Code (XXXXXXXX) = "
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)
L64550:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L64550
            return clear all
            goto inputmode
/* (AWD033) */

        lookupDoor
          door% = 0%
          str(readkey$,1%,9%)   = "PLAN DOOR"
          str(readkey$,10%,15%) = str(part$,1%,3%)
          read #4,key = readkey$, eod goto notDoor
             door% = 1%
        notDoor
        return

        lookupIsEnergy
           if U_Fac = 0.00 or SHGC = 0.00 then return

           if door% = 1% then goto isDoor

/*            if U_Fac <= 0.30 then energy_star% = 1%    disable in 2016 */
/*            if U_Fac <= 0.32 and SHGC <= 0.40 then energy_star% = 1%~
              if U_Fac <= 0.35 and SHGC <= 0.30 then energy_star% = 1%~
              if U_Fac <= 0.60 and SHGC <= 0.27 then energy_star% = 1% */
/*AWD045 SR67154 + */ 
              if U_Fac <= 0.27 then energy_star% = 1% /* enable in 2016 */
              if U_Fac <= 0.30 and SHGC <= 0.40 then energy_star% = 1%

/*pww + */    if U_Fac =  0.30 and SHGC >= 0.42 then energy_star% = 1%
              if U_Fac =  0.29 and SHGC >= 0.37 then energy_star% = 1%
/*pww - */    if U_Fac =  0.28 and SHGC >= 0.32 then energy_star% = 1%
              
              if U_Fac <= 0.30 and SHGC <= 0.25 then energy_star% = 1%
              if U_Fac <= 0.40 and SHGC <= 0.25 then energy_star% = 1%
/*AWD045 SR67154 - */
           return
        isDoor

/*         if U_Fac <= 0.32 and SHGC <= 0.30 then energy_star% = 1% */
/*AWD045*/ if U_Fac <= 0.30 and SHGC <= 0.40 then energy_star% = 1%
        return

/* (AWD033\) */
/* (AWD035) beg */
        format_label_new
           init(" ") warehouse$, series$, style$, spacercode$,         ~
              igthickness$, pane1$, pane2$, pane3$, gridcode$,         ~
              gridsize$, gapfill1$, gapfill2$, framecode$, sashcode$,  ~
              lb_cpdnum$

           ufactor, sheat, vtranmit, U_Fac, SHGC = 0.00

           call "AWDPLA64" (part$,sub_part$, ufactor$, sheat$,            ~
                            vtranmit$, cpdnumber$, warehouse$,           ~
                            series$, style$, spacercode$, igthickness$,  ~
                            pane1$, pane2$, pane3$, gridcode$, gridsize$,~
                            gapfill1$, gapfill2$, framecode$, sashcode$, ~
                            #6, #7, #8, #4, err%)

           err% = 0%

           lb_cpdnum$ = cpdnumber$

           convert ufactor$ to ufactor, data goto bad_ufactor

bad_ufactor:

           convert sheat$ to sheat, data goto bad_sheat

bad_sheat:

           convert vtranmit$ to vtranmit, data goto bad_vtranmit

bad_vtranmit:

           if ufactor > 0.00 then convert ufactor to lb_resu$,pic(0.00)
           if sheat > 0.00 then convert sheat to lb_resheat$, pic(0.00)
           if vtranmit > 0.00 then convert vtranmit to lb_resvisible$, pic(0.00)

           U_Fac = ufactor
           SHGC  = sheat
           gosub lookupIsEnergy
        return

/* (\AWD035) */
        lookup_lamn
          gls_lamn% = 0%
          str(readkey$,1%,9%)   = "PLAN LAMN"
          str(readkey$,10%,15%) = str(part$,5%,2%)
          read #4,key = readkey$, eod goto notLamn
             gls_lamn% = 1%
        notLamn
        return
 
        REM *************************************************************~
            *  CR9999    Print a blank label at the end                 *~
            *************************************************************
        print_blank_label
        
        b$ = all(hex(00))
        b$ = "^XA"
        write #1, using L55030, b$, eod goto print_error
        b$ = all(hex(00))
        b$ = "^FD "
        write #1, using L55030, b$, eod goto print_error
        b$ = all(hex(00))      
        b$ = "^XZ"
        write #1, using L55030, b$, eod goto print_error
        
L55030:     FMT CH(80)
        return
        
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
        end

