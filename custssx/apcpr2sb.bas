
        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPR2SB                             *~
            *  Creation Date     - 12/06/93                             *~
            *  Last Modified Date- 02/08/2022 - CMN                     *~
            *  Description       - This Subroutine Calculates the Price *~
            *                      for a Specified Calc Code.           *~
            *                      ( PRICE 003 )                        *~
            *                      XX  = Standard or Special Ref. Code  *~
            *                      YYY = Calculation Method Defined     *~
            *                                                           *~
            *  Special Comments  - (1) A Special MOD for Product Price  *~
            *                          to Determine Finished. Set       *~
            *                          STR(C_CAL$,7%,3%) - to (999).    *~
            *                          When the Size Ref '01' is used   *~
            *                          with Calc Method '019','020' and *~
            *                          '024' for MFG Price.             *~
            *                      (2) DEBUG%(2%) is the Level (2)      *~
            *                          Switch for the debug Display.    *~
            *                          0% = Off, 1% = Level (2) On      *~
            *                      (3) Special Change for Glass Low-E   *~
            *                          Promotions for Specified         *~
            *                          Customers.                       *~
            *                                                           *~
            *  New Debug Screen  - Table = (PRICE 015)                  *~
            *                      XX = 1st Value when not '00' APCPRSUB*~
            *                      YY = 2nd Value when not '00' On      *~
            *                           When = to Ref Value just Ref On *~
            *                           When = "99" All Calc's Turned On*~
            *    When turned on  - Phase's (1) thru (6)                 *~
            *          Phase (1) -  Level-1 Find Data Def. Catalog/Spec *~
            *          Phase (2) -  Level-2 Load Price Calc Definition  *~
            *          Phase (3) -  Level-3 Build Look-Up Scan Key      *~
            *          Phase (4) -  Level-4 Load Link for L_MATCH Scan  *~
            *          Phase (5) -  Level-5 Scan Key Look-Up L_MATCH    *~
            *          Phase (6) -  Level-6 Wood Surround/Factory Mull  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/05/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/04/94 ! Special Mod for Pricing to Correct Change! RHH *~
            *          !   Over to New Liting Table and Codes.    ! RHH *~
            * 06/19/95 ! Mod for Pricing Special Shapes with Low E! RHH *~
            *          !   Glass. Also Put in Place Code to Price ! RHH *~
            *          !   Wood Surround and factory Mulled MFG's ! RHH *~
            *          !   CAL$ = "008" with Ref "29" Special Shap! RHH *~
            *          !          Shapes with Low "E" Glass Charge! RHH *~
            *          !   CAL$ = "009" with Ref "09" Wood/Factory! RHH *~
            *          !          Mull Products. Uses "APC WOOD"  ! RHH *~
            *          !          and "PRICE 012" Tables.         ! RHH *~
            * 03/31/97 ! Mods to Update Pricing for New System and! RHH *~
            *          !   all 'Current' Changes.                 !     *~
            * 04/18/97 ! Mods to Allow for Alpha Code Table Values! RHH *~
            *          !   and double lookup (Lines - 3010 )      !     *~
            * 05/22/97 ! Mods to allow for Special Glass          ! RHH *~
            *          !   Promotions to Override the calculated  !     *~
            *          !   price for a specified customer.        !     *~
            *          !   Entry/Computer date controls the       !     *~
            *          !   Override.  (CALC_GLASS_SPECIAL)        !     *~
            * 06/09/97 ! Mods to fix problem with double locks    ! RHH *~
            *          !   when size greater than 27 1/4 then     !     *~
            *          !   deduct the 7.00 up charge out of the   !     *~
            *          !   price.                                 !     *~
            * 06/16/97 ! Additional Lock Mods. Certain models are ! RHH *~
            *          !   still a 4.00 up charge. Stored in the  !     *~
            *          !   new LK_MOD$() array. Line No. = 6560   !     *~
            * 10/31/97 ! Check for Upgrade to R6.04.03            ! RHH *~
            * 01/22/98 ! Mod for New Wood Surround Codes A00-Z00  ! RHH *~
            * 02/20/98 ! Mod replace LK_MOD$() array with new     ! RHH *~
            *          !   tabel (PRICE 020) Lock Deduction Models! RHH *~
            *          !                                          !     *~
            * 05/14/98 ! Added Wood Surround pricing & other stuff! ERN *~
            *          !   See PRSUB for specifics.               !     *~
            * 06/02/98 ! Fixed Bay/Bow number of lits%            ! ERN *~
            * 09/22/98 ! Reversed temporary mods by ERN.    EWD000! BWS *~
            * 09/22/98 ! Add Calc Method #15;fix minor bugs.EWD001! BWS *~
            * 06/11/99 ! (EWD002) - Mod to Exact U.I. Calc Method ! RHH *~
            *          !    After Addition, truncate throw away   !     *~
            *          !    fractional Part.                      !     *~
            * 07/28/99 ! (EWD003) - Special mod for (312) link    ! RHH *~
            *          !    to calc Number of panels. Note found  !     *~
            *          !    out special calc only loaded for      !     *~
            *          !    primary reference.                    !     *~
            * 08/10/99 ! (EWD004) - Special Mod for (212) and other!RHH *~
            *          !    Models for Fixed Product Price based  !     *~
            *          !    19 digit part Number.                 !     *~
            * 08/17/99 ! (EWD005) - Special Mod for Min Price     ! RHH *~
            *          !    Calculation for Custom Sizes.         !     *~
            * 08/25/99 ! (EWD006) - Special Mods for New Casement ! RHH *~
            *          !    Pricing Catalog.                      !     *~
            * 11/01/99 ! (EWD007) - Mods for New Wood Jamb Pricing! RHH *~
            *          !    wsfm_instruction$- WS, CP, FM. FP, SW !     *~
            *          !       CW                                 !     *~
            *          !    wsfm_nbr_windows% str(wsfm_code$,3%,1%)!    *~
            *          !    top_nbr_windows% str(wsfm_code$,5%,1%)!     *~
            *          !    custom% = 1% then Product Custom Size !     *~
            * 07/18/00 ! (EWD008) - Mod for new lock up cahrge for! RHH *~
            *          !    specific Models 215,216,217,218       !     *~
            *          !    line - L06540                         !     *~
            * 02/05/01 ! (EWD009) - Mod to correct 313 hinge code ! CMG *~
            *          !    for triple door.                      !     *~
            * 02/05/01 ! (EWD010) - Mod to correct New Stainable  ! CMG *~
            *          !    Wood Surround wsfm_instruction$-XP    !     *~
            * 03/30/01 ! (EWD011) - Mod to correct Costal WS -    ! CMG *~
            *          !    4 9/16 Stainable Wd Surr wsfm_instr-XW!     *~
            *          !    , SW, and CW                          !     *~
            * 04/12/02 ! (EWD012) - Mod to add height extra times ! CMG *~
            *          !    for the 421, 431, 441, 451 400 series.!     *~
            * 08/16/02 ! (EWD013) - Mod to take half price for    ! CMG *~
            *          !    sash only grid (liting) codes.        !     *~
            * 08/19/02 ! (EWD014) - Mod to turn on Prairie grid.  ! CMG *~
            *          !    Handle same as diamond grid.          !     *~
            * 09/17/02 ! (EWD015) Fix for Zero Price on Spec Shape! CMG *~
            * 10/28/02 ! (EWD016) Mod for New Patio Door          ! CMG *~
            * 10/28/02 ! (EWD017) Mod for New Cont. Head & Sill   ! CMG *~
            * 11/25/02 ! (EWD018) Mod for Special Shapes Custom   ! CMG *~
            *          !          Size Pricing                    !     *~
            * 07/08/04 ! (EWD019) Mod for Replacement Cot/Oriel   ! CMG *~
            * 08/26/04 ! (AWD020) Mod for new 3-lite patio door   ! CMG *~
            *          !   hinge code and 58 prairie in one sash  !     *~
            * 12/22/04 ! (AWD021) Mod to add a table to price     ! CMG *~
            *          !   certain new construction grid codes    !     *~
            *          !   with no grid                           !     *~
            * 01/21/05 ! (AWD022) Mod to apply cont head sill log ! CMG *~
            *          !         for all factors                  !     *~
            * 02/07/05 ! (AWD023) Mod for new WS and Coastal      ! CMG *~
            *          !         6 9/16 primed wood surround.     !     *~
            * 10/25/05 ! (AWD024) CR347 Mods for new sub part     ! CMG *~
            * 02/08/06 ! (PAR001) mods for part number            ! CMG *~
            * 06/26/06 ! (AWD025) mods for special mull           ! CMG *~
            * 10/10/07 ! (AWD026) mods for perimeter pari         ! CMG *~
            *02/15/2008! (AWD027) mods for simulated divided lite ! DES *~
            *08/27/2009! (AWD028) Mods for Special Grid Prc Promo ! CMG *~
            *02/12/2010! (AWD029) mods for new promo tables       ! CMG *~
            *03/30/2010! (AWD030) third set of promo tables TaxCrd! CMG *~
            *09/22/2010! (AWD031) new set of promo tables         ! CMG *~
            *10/25/2011! (AWD032) mod for new grid codes 'TD' &   ! CMG *~
            *          ! 'TP' tso prairie & tso diamond           !     *~
            *03/08/2012! (AWD033) new set of promo tables         ! CMG *~
            *05/16/2012! (AWD034) mods for 8900 pricing           ! CMG *~
            *02/21/2013! (AWD035) mods for promos                 ! CMG *~
            *07/02/2013! (AWD036) OGO change ecn 2013-032         ! CMG *~
            *07/18/2013! (AWD037) mod for number of panels for hg ! CMG *~
            *          !    code 60 & 61                          !     *~
            *07/29/2014! (AWD038) mod for u-chnl & mull clps      ! CMG *~
            *12/22/2015! (SR67154) mods for NFRC 2016             ! CMG *~
            *12/22/2015! (SR71583) mods for TriplePlus            ! CMG *~
            *03/10/2016! (SR73304) mod for price increase & locks ! CMG *~
            *09/07/2016! (CR601) ECR# 2016-084 450/460/455/465 DH ! CMN *~
            *          !     Northern Energy Star to receive foam !     *~
            *          !     filled frame with no grid            !     *~
            *03/04/2017! (CR839) mod to make prairie price as     ! CMN *~
            *          !           normal grid                    !     *~
            *02/28/2020! (CR2444) mod for 374 patio               ! CMN *~
            *03/04/2020! (CR2451) 3900 PSE Price                  ! CMN *~            
            *02/08/2022! (CR3016) Price Inc - lock deduction      ! CMN *~
            *************************************************************




            sub "APCPR2SB" ( model$,    /* Model                      */~
                             p$,        /* MFG Part Number             */~
                             p1$,       /* MFG Part Number 1           */~
                             pc_c$,     /* Price Catalog Code-PRICE 000*/~
                             pc_cm$,    /* Catalog Method Cde-PRICE 001*/~
                             pc_m$,     /* Model Code '000' = All Model*/~
                             pc_r$,     /* Std/Spc Ref Code  -PRICE 002*/~
                             c_ref$,    /* Cross-Ref Code    -PRICE 011*/~
                             c_cal$,    /* Cross-Ref Calc Codes 3,3,3  */~
                             cuscode$,  /* Customer Code               */~
                             wsfm_instruction$,                          ~
                                        /* Mfg Instruction - c_ref = 9 */~
                             wsfm_nbr_windows%,                          ~
                                        /* Nbr of windows  - c_ref = 9 */~
                             top_nbr_windows%,                           ~
                                        /* Nbr of windows on top       */~
                             nbr_uchannel%,                              ~
                                       /* Number of Uchannel (AWD038)  */~
                             nbr_mclips%,                                ~
                                       /* Number of Mclips   (AWD038)  */~
                             sp_mull%,  /* (AWD025) special mull       */~
                             pp,        /* Accumulative Price Value    */~
                             debug%(),  /* Level (2) - 00=Off,01=On    */~
                             tpp$,      /* TriplePanePackage  (SR67154)*/~
                             nfrc$,     /* NRFC 2016 req      (SR71583)*/~
                             forcedfoam$, /* Forced Foam 2018          */~
                             series$,   /* Series             (SR71583)*/~
                             style$,    /* Stlye              (SR71583)*/~
                             #1,        /* (APCPCMSK) - File           */~
                             #2,        /* (GENCODES) - File           */~
                             #3,        /* (APCPCMST) - File           */~
                             #4,        /* (AWDPCMST) - File           */~
                             err% )     /* 0% = OK, 1% = NO PRICE      */

        dim                              /* (APCPCMST) Price Calc Def. */~
            sav_cat$4,                   /* Save In Comming Catalog    */~
            sav_ccm$2,                   /* Save In Comming Cat. Calc  */~
            sav_mod$16,                  /* Save In Comming Model Code */~
            sav_link$10,                 /* Save Link for Special MFG  */~
            p$25,                        /* MFG Part Number            */~
            p1$20,                       /* MFG Part Number 1 (AWD024) */~
            field1$1,                    /* MFG PART 1 Field 1 (AWD024)*/~
            field2$1,                    /* MFG PART 1 Field 2 (AWD024)*/~
            field3$1,                    /* MFG PART 1 Field 3 (AWD024)*/~
            field4$1,                    /* MFG PART 1 Field 4 (AWD024)*/~
            field5$1,                    /* MFG PART 1 Field 5         */~
            pc_c$4,                      /* Price Catalog Code         */~
            pc_cm$2,                     /* Catalog Method Code        */~
            pc_m$16,                     /* Model Code                 */~
            model$16,                    /* WW Virtual Model           */~
            tpp$2,                       /* Triple Pane Packag(SR67154)*/~
            nfrc$2,                      /* NFRC 2016 required(SR71583)*/~
            forcedfoam$2,                /* Forced Foam 2018           */~
            series$16,                   /* Series            (SR71583)*/~
            style$10,                    /* Style Code        (SR71583)*/~
            c_ref$1,                     /* Cross Ref Code             */~
            c_cal$9,                     /* Cross Ref Calc Codes       */~
            mull_c_cal$3,                /* c_cal$ lookup for mull $   */~
            cal$3, panels$2, row$2,      /* Special Calcs - PRICE 010  */~
            cuscode$9,                   /* Customer Code              */~
            pc_r$2, sav_pc_r$2,          /* Std/Spc Reference Code     */~
            pc_rc$3,                     /* Price Ref. Calc Method     */~
            pc_kdesc$(3%)3,              /* Field Description Codes    */~
            pc_kfld%(3%),                /* Field Definition Codes     */~
            pc_kbeg%(3%),                /* Start Position in Part No. */~
            pc_klen%(3%),                /* Field Length in Part No.   */~
            pc_vtbl$2,                   /* Field Definition Code-Table*/~
            pc_vtblv$(6%)3,              /* Field Table Values         */~
            pc_vdesc$3,                  /* Value Description Codes    */~
            pc_vfmt$2,                   /* Value Field Format Code    */~
            pc_vcalc$2,                  /* Value Calculation Code     */~
            pc_kcalc$2,                  /* Key   Calculation Code     */~
            pc_kunt$2,                   /* Key Unit Conversion Code   */~
            pc_key$5,                    /* Primary Key (APCPCMSK)     */~
            pc_kfil$16,                  /* Definition Filler Area     */~
            pc_k$25,                     /* Price Lookup Key - Section */~
            pc_vl(6%),                   /* Pricing Values             */~
            pc_link$3,                   /* From (PRICE 003)           */~
            p_key$53, s_key$28,          /* Primary Price Key          */~
            d_key$50, ss_key$30,         /* Primary Scan Key           */~
            sav_def$28,                  /* Save Scan Definiton Key    */~
            k_val$3, k_desc$32,          /* Key Lookup Table Values    */~
            tab_val$5, tab_desc$32,      /* Table Key and Description  */~
            ws_no$1,                     /* Wood Surround No Windows   */~
            wsfm_instruction$2,          /* Mfg instruction c-ref = 9  */~
                                         /*   from APC WOOD descrs     */~
            readkey$24, desc$30,         /* Gencodes Lookup Key       */ ~
            b_dte$8, e_dte$8, t_dte$6,   /* Special Promotion Dates   */ ~
            w$4,                         /* Part Width                */ ~
            h$3,                         /* Part Height               */ ~
            c$1,                         /* Part Color                */ ~
            gl$2,                        /* Glass Code                */ ~
            lt$2,                        /* Liting Code               */ ~
            hg$2,                        /* Hinge Code                */ ~
            sc$1,                        /* Screen Code               */ ~
            lk$1,                        /* Locks Code, MODS = $4.00  */ ~
            clmr$3,                      /* Center Line Meeting Rail  */ ~
            rhh1$(10%)10,                /* RHHTEST variables         */ ~
            wall$3, debug%(2%)           /* Wall Width, Debug Switches*/

        dim                              /* Debug Variables            */~
           cursor%(2%), i$(24)80,        /* Screen Display Var's       */~
           txt$35, pname$21, apc$40,     /* Debug Screen Headers       */~
           tt1$(6%)20,                   /* Phase (1) Text Display     */~
           dd1$(6%)25, inp$79,           /* Phase (1) Data Display     */~
           hh1$35, pfkeys$32,            /* Phase (1) Header Display   */~
           tt2$(6%)20, date$8,           /* Phase (2) Text Display     */~
           dd2$(6%)15, xx$5,             /* Phase (2) Data Display     */~
           hh2$35,                       /* Phase (2) Header Display   */~
           tt3$(6%)20,                   /* Phase (3) Text Display     */~
           dd3$(6%)15,                   /* Phase (3) Data Display     */~
           hh3$35,                       /* Phase (3) Header Display   */~
           tt4$(6%)20,                   /* Phase (4) Text Display     */~
           dd4$(6%)15,                   /* Phase (4) Data Display     */~
           hh4$35,                       /* Phase (4) Header Display   */~
           tt5$(6%)20,                   /* Phase (5) Text Display     */~
           dd5$(6%)15,                   /* Phase (5) Data Display     */~
           hh5$35,                       /* Phase (5) Header Display   */~
           tt6$(6%)20,                   /* Phase (5) Text Display     */~
           dd6$(6%)15,                   /* Phase (5) Data Display     */~
           hh6$35                        /* Phase (5) Header Display   */

        dim logmsg$256

        dim mdl_8900$(999%)3

REM     ********************************************************************
REM     S T A R T   O F   P R O C E S S I N G
REM     ********************************************************************

REM     Move part number sub-variables into stand-alone variables

REM            PC_M$    = MODEL$
            c$       = str(p$,4%,1%)                 /* Color        */
            gl$      = str(p$,5%,2%)                 /* Glass        */
            lt$      = str(p$,7%,2%)                 /* Liting       */
            hg$      = str(p$,9%,2%)                 /* Hinge        */
            sc$      = str(p$,11%,1%)                /* Screen       */
            lk$      = str(p$,12%,1%)                /* Locks        */
            w$       = str(p$,13%,4%)                /* Width Open   */
            h$       = str(p$,17%,3%)                /* Height Open  */
            clmr$    = str(p$,20%,3%)                /* CLMR         */
            wall$    = str(p$,23%,3%)                /* WALLWDT      */

            field1$  = str(p1$,1%,1%)                /* Field 1      */
            field2$  = str(p1$,2%,1%)                /* Field 2      */
            field3$  = str(p1$,3%,1%)                /* Field 3      */
            field4$  = str(p1$,4%,1%)                /* Field 4      */
            field5$  = str(p1$,5%,1%)                /* Field 5      */

REM            if pc_r$ <> "01" then goto NotBase
REM            cmg$ = model$
REM            cmg1$ = pc_m$
REM            call "SHOSTAT" ("APCPR2SB BASE -> "& cmg$ & " -- " & cmg1$ ) 
REM            stop
REM NotBase
/* (SR71583) */
            tpp%, nfrc%, ffoam% = 0%
            convert tpp$ to tpp%, data goto badtpp

badtpp:
/* (SR67154) */
            convert nfrc$ to nfrc%, data goto badnfrc

badnfrc:

            convert forcedfoam$ to ffoam%, data goto badFFoam

badFFoam:

            sav_cat$ = pc_c$
            sav_ccm$ = pc_cm$
            sav_mod$ = pc_m$
            if beenherebefore% <> 1% then gosub load_8900_mdl

* First determine if shape or not
            shape% = 0%
            if str(lt$,1%,1%) > "9" then shape% = 1%

            if wsfm_nbr_windows% = 0 then wsfm_nbr_windows% = 1%
            if pc_r$ = "01" and shape% = 0% then custom% = 0%
            if pc_r$ = "04" and shape% = 1% then custom% = 0%
                                                     /* PAR001   */
                                                    /* (EWD007) Init  */
                                                    /* Custom Flag    */

*EWD000     if pc_r$ = "09" then c_ref$ = "D"   /* TEMP 5/20 - ern */

            mull_c_cal$ = " "
/* !!! - Fix color codes */
            if  pc_r$ = "02" and c$ < "C" then L1000
                if c$ = "C" then c$ = "A"       /* Change to color */
                if c$ = "D" then c$ = "B"       /* code that does  */
                if c$ = "E" then c$ = "2"       /* not include     */
                if c$ = "F" then c$ = "6"       /* brass package   */

            new_const% = 0%                     /*  (EWD013)       */
            sash_only% = 0%                     /*  (EWD013)       */
            pass_sash% = 0%                     /*  (EWD013)       */
            sash_p1 =    0%                     /*  (EWD013)       */
            lowe006% =   0%
         /* Models 9XX are Bay/Bow   */
L1000:      pass% = 0%
            if str(p$,1%,1%) = "9" and pc_r$ = "01" then pass% = 1%


        REM **************************************************************
        REM M a i n   L i n e   R o u t i n e
        REM **************************************************************

            init(" ") d_key$, sav_def$, p_key$, s_key$, pc_key$, panels$,~
                      cal$, ss_key$, sav_link$, sav_pc_r$, ws_no$, rhh1$()

            p_key% = 0%   : pc_vtln% = 0% : k_p% = 0% : u3% = 0%
            save_p = 0.0  : save_p1 = 0.0  /* Save Calc'd Prices        */
            save_p2 = 0.0 : p1 = 0.0       /* Save Price Lookup Values  */
            p = pp                         /* Set Current Price to Prev.*/
                                           /* Calc Value                */
        REM - 1st Load the Data Definition (MST) for either Catalog or Special
            gosub scan_def_data
                phase% = 1% : gosub display_debug

            if c_r% = 99% then goto exit_program
                                               /* (EWD007) Remove old Code*/

L01970:     gosub read_calc        /* Load Key and Data Parimeters     */
                phase% = 2% : gosub display_debug

            gosub key_build        /* Build Key, Lookup Data, Get Price*/

            goto exit_program

REM     **********************************************************************
REM     End Of Main Line
REM     **********************************************************************


REM     *---------------------------------------------------------------------*
        key_build
REM     *---------------------------------------------------------------------*
                /* K1%   = Value of Code (PRICE 008)    */
                /*  J%   = Positon within Key           */
                /* PC_K$ = Lookup Key for Opt Prc       */
                /* When applicable, lookup Code in      */
                /*   table - K_VAL$, K_DESC$, K_P%      */

            init(" ") k_val$, k_desc$, pc_k$
            scan% = 0% : k1% = 0% : j% = 1%


            convert pc_kcalc$ to k1%, data goto L02120

L02120:     if k1% = 0% or k1% = 3% then L02480  /* 0%, 3%, - No Key   */

            if k1% = 2% or k1% = 5% or k1% = 8% or k1% = 9% or k1% = 10% ~
                        then scan% = 1%
/* (AWD034) */
            if k1% = 12% then goto L02481
/* (SR67154) */
/* Default nfrcLiting% to something other than 0% or 1% so will not */
/* interfere with SDL or other pricing                              */
            nfrcLiting% = 99%                    /* Set Default to 99  */
            if k1% = 13% then goto keyNFRC       /* k1%=13% on set on REF="16" */
                                                 /* 2% = Min Max U.I.  */
                                                 /* 5% = Unit Value    */
                                                 /* 8% = Min/Max Height*/
                                                 /* 9% = Min/Max Width */
                                                 /* 10%= Min/Max Field */
            if scan% = 1% then goto L02480

            for i% = 1% to 3%         /* Set Key for Exact Match Codes */
                if pc_kdesc$(i%) = "000" then goto L02470      /* Done */
                                                 /* 1% = Tab Part      */
                                                 /* 4% = W/H Open Part */
                                                 /* 6% = Tab Part      */
                                                 /* 7% = Fld Part      */
                                                 /* 11%= W/H Exact Part*/
                    if pc_kbeg%(i%) <= 25% then           /*(AWD024)*/  ~
                    str(pc_k$, j%, pc_klen%(i%) ) =                     ~
                                  str(p$, pc_kbeg%(i%), pc_klen%(i%) )
                    if pc_kbeg%(i%) > 25% then           /*(AWD024) */  ~
                    str(pc_k$, j%, pc_klen%(i%) ) =                     ~
                                  str(p1$,(pc_kbeg%(i%)-25%),pc_klen%(i%) )

                    if i% <> 3% then goto L02340
                        if cal$ <> "007" then goto L02340   /* 312 - Patio */
                            str(pc_k$, j%, pc_klen%(i%) ) = panels$

L02340:             j% = j% + pc_klen%(i%)
                    if k1% <> 1% then goto L02470
                                                  /* Code Table Lookup */
                        if pc_kfld%(i%) < 1% or pc_kfld%(i%) > 7% then L02470
                            tab_1% = 0% : tab_2% = pc_kfld%(i%)

                            if pc_kbeg%(i%) <= 25% then       /*(AWD024) */ ~
                            tab_val$ = str(p$,pc_kbeg%(i%),pc_klen%(i%) )
                            if pc_kbeg%(i%) > 25% then        /*(AWD024) */ ~
                            tab_val$ = str(p1$,(pc_kbeg%(i%)-25%),pc_klen%(i%) )

                            call "APCPR1SB" (tab_1%, tab_2%, tab_val$,     ~
                                             tab_desc$, p%, #2, tab_rec% )
                            k_val$  = tab_val$           /* Code Value       */
                            k_desc$ = tab_desc$          /* Code Description */
                            k_p%    = p%                 /* Position of '-'  */
L02470:     next i%


L02480:     gosub unit_convert
                phase% = 3% : gosub display_debug

            gosub l_match

            if pc_r$ = "03" then gosub calc_glass_special
/*(AWD030)*/
            if pc_r$ = "03" then gosub calc_glass_special1

/*(AWD031)*/
            if pc_r$ = "03" then gosub calc_glass_special2

/*(AWD033)*/
            if pc_r$ = "03" then gosub calc_glass_special3

/* (AWD028) */
            if pc_r$ = "04" then gosub calc_grid_special
/* (AWD029) */
            if pc_r$ = "04" then gosub calc_grid_special1

/* (AWD035) */
            if pc_r$ = "41" then gosub check_sdl_promo

            if pc_r$ = "03" then gosub calc_glass_special4

                phase% = 5% : gosub display_debug
        return
/* (AWD035) */
L02481:
        j% = 1%
        wsfm_tot_windows% = wsfm_nbr_windows% + top_nbr_windows%
        str(pc_k$, j%, pc_klen%(1%) ) =                     ~
                                  str(p$, pc_kbeg%(1%), pc_klen%(1%) )
        j% = j% + pc_klen%(1%)
        convert wsfm_tot_windows% to str(pc_k$,j%,1%),pic(0)
        goto L02480
/*(\AWD034)*/

/* (SR67154) */
        keyNFRC
          gosub nfrc_checks

          goto L02480

/* (\SR67154) */


REM     *--------------------*
        unit_convert                         /* (PRICE 009) Conversion */
REM     *--------------------*
            gosub check_cont_head                 /* (AWD022) */

            ck_unit, w, h, w1, h1 = 0.0
            k2% = 0% : ck_unit$ = " "

            convert str(w$,1%,3%) to w, data goto L02590
L02590:
            convert str(w$,4%,1%) to w1, data goto L02610
L02610:
            convert str(h$,1%,2%) to h, data goto L02630
L02630:
            convert str(h$,3%,1%) to h1, data goto L02650
L02650:
            convert pc_kunt$ to k2%, data goto L02670
L02670:                                   /* Table Values - 00 thru 08 */
            k2% = k2% + 1%
            if k2% <> 2% then goto L02730
               if str(w$,4%,1%) <> "0" then w = w + 1.0
               if str(h$,3%,1%) <> "0" then h = h + 1.0
               goto L02750

L02730:     w = w + w1/8.0
            h = h + h1/8.0

L02750:     on k2% gosub L02780 , L02790 , L02810 , L02830 , L02850 , L02780 , L02870 ,~
                         L02890 , L02910, L02940, L02990

            convert ck_unit to ck_unit$,pic(0000)
L02780:     return

L02790: REM - United Inches Adjusted
            ck_unit = round(w + (h * hp%), 2) : return        /*  (AWD022)  */

L02810: REM - United Inches Exact                    /* (EWD002) Mod */
            rhh1 = round( (w + (h * hp%) ), 4)               /*  (AWD022)  */
            rhh1% = int(rhh1)
            cK_unit = rhh1%
            return
                                                     /* (EWD002) End */
L02830: REM - Square Inch
            ck_unit = round( (w * h) / 12.0, 2) : return

L02850: REM - Square Feet
            ck_unit = round( (w * h) / 144.0, 2) : return

L02870: REM - Width Decimal
            ck_unit = w : return

L02890: REM - Height Decimal
            ck_unit= h  : return

L02910: REM - Total Feet
            ck_unit = round( ((w * 2.0) + (h * 2.0)), 2)
            return

L02940: REM - Under and Over 100 U.I
            gosub L02790                             /* CALC UNITED INCH */
            if ck_unit <= 100.0 then str(pc_k$,1%,4%) = "0100"           ~
                                else str(pc_k$,1%,4%) = "0101"
            return

L02990: REM - Width and Height of Part Number
            w, h = 0.0
            convert w$ to w, data goto L03020
L03020:     convert h$ to h, data goto L03040
L03040:     return


REM        check_cont_head
            cont%, hp% = 1%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN CONT"
            str(readkey$,10%,15%) = sav_mod$
            read #2,key = readkey$, using L08080, desc$, eod goto not_cont

            convert str(desc$,30%,1%) to hp%, data goto not_cont

            cont% = 1%
REM        not_cont
        return

REM     *-------------------------------------*
        l_match                                      /* SCAN% = 0%     */
REM     *-------------------------------------*
            p_key$ = " "
            str(p_key$,1%,1%)   = "A"                /* Only Active    */
            str(p_key$,2%,4%)   = pc_c$              /* Catalog Code   */
            str(p_key$,6%,2%)   = pc_cm$             /* Catalog Method */
            str(p_key$,8%,16%)  = pc_m$              /* Model Code     */
            str(p_key$,24%,2%)  = pc_r$              /* Ref. Code      */
            str(p_key$,26%,3%)  = pc_rc$             /* Ref. Calc Code */
            if lowe006% = 1% then str(p_key$,8%,16%) = "000"

            s_key$ = str(p_key$,1%,28%)

            if scan% = 1% then goto l_scan
               str(p_key$,29%,25%) = pc_k$           /* Match Key      */
               f% = 3%
               if pc_r$ = "01" or pc_r$ = "03" then f% = 4%

               read #f%,key = p_key$, using  L03200, pc_vl(), pc_spc$,      ~
                                                            eod goto L03290
L03200:            FMT POS(62), 6*PD(14,4), CH(3)
               p_key% = 1%
               lowe006% = 0%
               gosub l_value

                if (str(s_key$,26%,3%)="019" or str(s_key$,26%,3%)="020") ~
    /*EWD001*/  and str(s_key$,24%,2%)="01" then str(c_cal$,7%,3%)="999"
                                                    /* (EWD004) - Mod   */
               if str(s_key$,26%,3%)="024" and str(s_key$,24%,2%)="01"    ~
                                            then str(c_cal$,7%,3%)="999"
                                                    /* (EWD004) -       */
   /*EWD001*/ if p_key% = 0% and str(c_cal$,4%,3%) <> "000" and           ~
                  link% = 0% then goto L03290       /* CHECK LINK ONCE  */
                                                    /* ??? Can't Happen */
            return

L03290:     gosub load_link              /* PC_LINK$ - Check Link Code */
                phase% = 4% : gosub display_debug
            if link% <> 2% then return
               return clear all
               goto L01970

REM     *--------------------------------------*
        l_scan                                       /* SCAN% = 1%     */
REM     *--------------------------------------*
        if pass% <> 0% then goto bb_scan             /* BAY/BOW        */

        s_key$ = str(p_key$,1%,28%)                 /* Save Key - (28)*/

        l_scan_nxt
            read #f%,key > p_key$, using L03400 , p_key$, eod goto L03680
L03400:         FMT POS(9), CH(53)
            if str(p_key$,1%,28%) <> s_key$ then goto L03680
                                                    /*  (EWD018)      */
                get #f%, using L03430, pc_vl(), pc_spc$, cal$
L03430:             FMT POS(62), 6*PD(14,4), CH(3), POS(110), CH(3)
                j% = 1% : minn, maxx = 0.0
                pc_k$ = str(p_key$,29%,25%)
                convert str(pc_k$,1%,pc_klen%(1%)) to minn,              ~
                                                          data goto L03480
L03480:
                j% = j% + pc_klen%(1%)
                convert str(pc_k$, j%, pc_klen%(2%)) to maxx,            ~
                                                          data goto L03520
L03520:         if k1% = 10% then goto L03560        /* Standard Scan Sub */
L03530:             if ck_unit >= minn and ck_unit <= maxx then goto L03600
                        goto l_scan_nxt

L03560: /*EWD001*/  if pc_r$ = "01" and str(pc_rc$,2%,2%) = "07"         ~
                            then goto L03530          /* Width/Height Std */
                        if w = minn and h = maxx then goto L03600
                            goto l_scan_nxt

L03600:         if cal$ <> "008" then goto L03650  /* Check Special Shapes   */
                    j% = j% + pc_klen%(2%)         /* Low "E" Glass Upcharge */
                    if str(p$,pc_kbeg%(3%),pc_klen%(3%)) <>                   ~
                            str(pc_k$,j%,pc_klen%(3%)) then goto l_scan_nxt

L03650:         p_key% = 1%                      /* Valid Match Found      */
                gosub l_value                    /* Convert Price Value    */
                return
                                                 /* (EWD006) Casement      */
L03680:   if pc_link$ = "030" or pc_link$ = "032" then link% = 0%
             if link% <> 0% then return
                gosub l_value               /* Sizes. Use Price of    */
                                            /* Last Lookup for Add-On */

               gosub load_link              /* PC_LINK$ - Check Link Code */
                    phase% = 4% : gosub display_debug
               if link% <> 2% then return
                  return clear all
                  goto L01970

REM     *---------------------------------------*
        bb_scan                                      /* BAY - BOW      */
REM     *---------------------------------------*
            bb% =28%
            ss_key$ = str(p_key$,1%,bb%)             /* Save Key - (25)*/
            if pass% = 1% then goto L03850
               bb% = 30%
               str(p_key$,29%,2%) = row$
               ss_key$ = str(p_key$,1%,bb%)

L03850: bb_scan_nxt
            read #f%,key > p_key$, using L03870 , p_key$, eod goto L04100
L03870:         FMT POS(9), CH(53)
            if str(p_key$,1%,bb%) <> ss_key$ then goto L04100
                get #f%, using L03900, pc_vl(), pc_spc$
L03900:             FMT POS(62), 6*PD(14,4), CH(3)
                j% = 1% : minn, maxx = 0.0
                pc_k$ = str(p_key$,29%,25%)
                row$ = str(pc_k$,1%,pc_klen%(1%))
                j% = j% + pc_klen%(1%)

                convert str(pc_k$,j%,pc_klen%(2%)) to minn,               ~
                                                          data goto L03980
L03980:         j% = j% + pc_klen%(2%)
                convert str(pc_k$, j%, pc_klen%(3%)) to maxx,             ~
                                                          data goto L04020
L04020:         if ck_unit >= minn and ck_unit <= maxx then goto L04050
                    goto bb_scan_nxt

L04050:         if pass% = 1% then goto L04100
                    p_key% = 1%

                    gosub l_value

                    return

L04100:     pass% = pass% + 1%           /* Calc Value for Last Entry  */

            gosub load_link              /* PC_LINK$ - Check Link Code */
                phase% = 4% : gosub display_debug

            if link% <> 2% then return
               return clear all
               goto L01970



REM     *---------------------------------------*
        l_value
REM     *---------------------------------------*
                          /* K1% = Key Lookup method (PRICE 008)       */
                          /* K2% = Key Conversion Method (PRICE 009)   */
                          /* K3% = Position of lookup Value            */
                          /* K4% = Value Calculation Code ( 0 thru 7 ) */
                          /* P  = In Coming Price Value                */
                          /* P1 = Lookup Value                         */

            pc_vtbl%, beg%, len% = 0%
            p1 = 0.0
            k3% = 1%
            convert pc_vtbl$ to pc_vtbl%, data goto L04280

L04280:     if pc_vtbl% > 0% and pc_vtbl% < 8% then goto L04290
            if pc_vtbl%  > 20% and pc_vtbl% < 29% then goto L04290
               goto L04490
                                                     /* (EWD007)    */
L04290:     tab_1% = 5% : tab_2% = 0                 /* (PRICE 005) */
            tab_val$ = pc_vtbl$
            call "APCPR1SB" ( tab_1%, tab_2%, tab_val$, tab_desc$,    ~
                                                       p%, #2, tab_rec% )

            convert str(tab_desc$,11%,2%) to beg%, data goto L04360
L04360:     convert str(tab_desc$,14%,2%) to len%, data goto L04380

L04380:     tab_1% = 0% : tab_2% = pc_vtbl%      /* Table for Field */

            if beg% <= 25%   then   /*(AWD024) */                    ~
            tab_val$ = str(p$, beg%, len%)       /* Value for Field */
            if beg% > 25%    then  /*(AWD024) */                     ~
            tab_val$ = str(p1$,(beg%-25%),len%)
                                                 /* (EWD007) Load Value    */
            if tab_2% > 20% then tab_val$ = ws_no$
                                                 /* (EWD007) load WS,CP    */
                                                 /* Multiple               */
            call "APCPR1SB" ( tab_1%, tab_2%, tab_val$, tab_desc$,    ~
                                                       p%, #2, tab_rec% )
            for k3% = 1% to 6%
                if tab_val$ = pc_vtblv$(k3%) and pc_vl(k3%) > 0.0 then goto L04490
            next k3%
/* (AWD034) */
            link% = 0%
            goto L04550                        /* No Match of Value  */


L04490:     save_p1, p1 = pc_vl(k3%)            /* Save Lookup Values      */

            gosub l_calc                        /* P = New Price After Calc*/
            if p_key% = 1% then goto L04540     /* Match Found - Exit      */
 /*EWD001*/ if k4% = 15% then goto L04540       /* Addtl. Link - Don't Save*/
               save_p2, save_p = p              /* No Match Found Save     */
               sav_unit = ck_unit               /* Current Price Found     */
L04540:     return

L04550:     p_key% = 0%                         /* No Price Calc.          */
            return

REM     *-----------------------*
        l_calc                                  /* Calculate Price         */
REM     *-----------------------*

         /* If special calc code indicates a screen only price,     */
         /* always add the value to the existing price              */
            if pc_spc$ = "010" then pc_vcalc$ = "02"

            k4% = 0%
            convert pc_vcalc$ to k4%, data goto L04610

L04610:     kk% = k4% + 1%
            on kk% goto L04660, L04660, L04680, L04710, L04730, L04750, L04770, ~
                        L04790, L04810, L04830, L04860, L04880, L04900, L04930, ~
                        L04940, L04950, L04960, L04970
                              /*EWD001*/
        REM pc_vcalc

L04660: REM - Not Applicable  [err, 1]
            return

L04680: REM - Add to Existing Price [2]
         /* For factory mulling, add $$s for each window over three */
            if pc_r$ <> "09" then L04682
                                                         /* (EWD007)          */
             if wsfm_instruction$ = "WS" or wsfm_instruction$ = "CP"    ~
                                           then goto L04682
             if wsfm_instruction$ = "XP" or wsfm_instruction$ = "XW"    ~
                                           then goto L04682
             if wsfm_instruction$ = "CW" or wsfm_instruction$ = "SW"    ~
                                           then goto L04682
             if wsfm_instruction$ = "PS" or wsfm_instruction$ = "PW"    ~
                                           then goto L04682
/* (AWD038) */
             if mull_c_cal$ = "017" then goto uChannel
             if mull_c_cal$ = "018" then goto woodClips
/* (\AWD038)*/
                                                        /* (EWD007) - Add To */
                                                        /* (EWD010) - ADD TO */
                                                        /* (EWD011) - ADD TO */
                                                        /* (AWD023) - ADD TO */
                over3rate = 12
                if mull_c_cal$ = "005" or mull_c_cal$ = "006" then over3rate = 14
                if mull_c_cal$ = "008" or mull_c_cal$ = "009" then over3rate = 10
/* (EWD011) - Take out standard upcharge for mull '019' because '019' is now */
/*          - Costal WS Stainable                                            */
REM      IF MULL_C_CAL$ = "019" THEN OVER3RATE = 20
                if mull_c_cal$ = "025" or mull_c_cal$ = "026" then over3rate = 20

                temp386% = wsfm_nbr_windows% + top_nbr_windows% - 3%
                if temp386% > 0% then p1 = p1 + (temp386% * over3rate)
                if wsfm_nbr_windows% <> 0% then                     ~
                        p1 = round(p1 / (wsfm_nbr_windows% + top_nbr_windows%), 4)

L04682:
/* (AWD034) */
            if pc_r$ <> "14" then goto L04683
               wsfm_tot_windows% = wsfm_nbr_windows% + top_nbr_windows%
               if wsfm_tot_windows% <= 1% then return
L04683:
                                                          /* (EWD007)           */
            gosub check_sash_only                         /* (EWD013)           */
            if sash_only% = 1% and pc_r$ = "04"                                 ~
                            then p1 = round(p1 / 2.0, 4)  /* (EWD013)           */


            gosub check_lite
            p = round( p + p1, 4)
            return
REM NEW UCHANNEL AND WOOD CLIPS
/* (AWD038)*/
uChannel:

            convert wsfm_nbr_windows%  to nbrofwindows$, pic(##0)
REM            call "SHOSTAT" (" Unchannel WSFM ->" &  nbrofwindows$ )
REM            stop
            ws_no% = 0%
            ws_no% = wsfm_nbr_windows% + top_nbr_windows%
REM            convert ws_no% to nbrofwindows$, pic(##0)
REM            call "SHOSTAT" (" Unchannel WSNO ->" &  nbrofwindows$ )
REM            stop

            p = round(p + ( (p1 / ws_no%) * nbr_uchannel%), 4)
            return

REM do not add mull clips if not in subpart position 6
woodClips:

            convert wsfm_nbr_windows%  to nbrofwindows$, pic(##0)
REM            call "SHOSTAT" (" Unchannel WSFM ->" &  nbrofwindows$ )
REM            stop
            ws_no% = 0%
            ws_no% = wsfm_nbr_windows% + top_nbr_windows%
REM            convert ws_no% to nbrofwindows$, pic(##0)
REM            call "SHOSTAT" (" Unchannel WSNO ->" &  nbrofwindows$ )
REM            stop

            p = round(p + ((p1 / ws_no%) * nbr_mclips%), 4)
            return
/* (\AWD038)*/

L04710: REM - Subtract From Existing Price  [3]
            if nfrcObs% = 1% then p1 = 0.00    /* (SR67154) */
            p = round( p - p1, 4)               : return

L04730: REM - Deduct Percent From Existing Price [4]
            p = round( p - (p*p1), 4)           : return

L04750: REM - Add Percent To Existing Price  [5]
            p  = round( p + (p * p1), 4)        : return

L04770: REM - Fixed Price       [6]
            p  = round( p1, 4)                  : return

L04790: REM - Discount Multiplier  [7]
            p = round( p * p1, 4)               : return

L04810: REM - COST PER UNITED INCH  [8]
                                                   /* (EWD012) */
REM         IF PC_M$ = "421" OR PC_M$ = "431" THEN GOSUB ADD_UNITED
REM         IF PC_M$ = "441" OR PC_M$ = "451" THEN GOSUB ADD_UNITED
                                                   /* (EWD017) */
REM         IF PC_M$ = "450" OR PC_M$ = "470" THEN GOSUB ADD_UNITED
REM         IF PC_M$ = "460" OR PC_M$ = "480" THEN GOSUB ADD_UNITED
REM         IF PC_M$ = "772" OR PC_M$ = "774" THEN GOSUB ADD_UNITED
REM         IF PC_M$ = "773" OR PC_M$ = "775" THEN GOSUB ADD_UNITED
REM            GOSUB CHECK_CONT_HEAD
REM              IF CONT% = 1% THEN GOSUB ADD_UNITED
            united_price = 0.00
            united_price = round(ck_unit * p1, 4)
            p = round( p + (ck_unit * p1), 4)
                                                 /* (EWD005)       */
            gosub check_min_price
            if p_min > p then p = p_min          /* Set to Min Val */

            if pc_r$ = "01" and shape% = 0% then ~
            custom% = 1%                        /* PAR001  */
                                               /* (EWD007) Set   */

            if pc_r$ = "04" and shape% = 1% then ~
            custom% = 1%                        /* PAR001  */
                                                 /* Custom Flag    */
            return
REM        add_united                                 /* (EWD012) */
           ck_unit = 0.0
REM        IF PC_M$ = "421" THEN CK_UNIT = ROUND(W + H + H, 2)
REM        IF PC_M$ = "441" THEN CK_UNIT = ROUND(W + H + H, 2)
REM        IF PC_M$ = "431" THEN CK_UNIT = ROUND(W + H + H + H, 2)
REM        IF PC_M$ = "451" THEN CK_UNIT = ROUND(W + H + H + H, 2)
                                                   /* (EWD017) */
REM        IF PC_M$ = "450" OR PC_M$ = "470"
REM                         THEN CK_UNIT = ROUND(W + H + H, 2)
REM        IF PC_M$ = "772" OR PC_M$ = "774"
REM                         THEN CK_UNIT = ROUND(W + H + H, 2)
REM        IF PC_M$ = "460" OR PC_M$ = "480"
REM                         THEN CK_UNIT = ROUND(W + H + H + H, 2)
REM        IF PC_M$ = "773" OR PC_M$ = "775"
REM                         THEN CK_UNIT = ROUND(W + H + H + H, 2)
REM           CK_UNIT = ROUND(W + (H * HP%),2)
REM        INIT(" ") CMG$
REM        CONVERT CK_UNIT TO CMG$, PIC(########)
        return

        check_cont_head
            cont%, hp% = 1%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN CONT"
            str(readkey$,10%,15%) = sav_mod$
            read #2,key = readkey$, using L08080, desc$, eod goto not_cont

            convert str(desc$,30%,1%) to hp%, data goto not_cont

            cont% = 1%
        not_cont
        return

L04830: REM - COST PER UNITED INCH OVER MAX  [9]
            x = sav_unit - maxx
REM            custom% = 1%                         /* (EWD007) Set   */
            if pc_r$ = "01" and shape% = 0% then ~
            custom% = 1%                        /* PAR001  */
                                               /* (EWD007) Set   */

            if pc_r$ = "04" and shape% = 1% then ~
            custom% = 1%                        /* PAR001  */
                                                 /* Custom Flag    */

                                                 /* Custom Flag    */
            p = round( save_p + (x * p1), 4)    : return

L04860: REM - CALCULATED COST OF FRENCH DOOR [10]
            p = round( (2 * p) + p1, 4)         : return

L04880: REM - NON-STANDARD SIZE ADD TO LAST VALUE [11]
            p = round( save_p + p1, 4)          : return

L04900: REM - Lit Calculation Routine Casement/Bay/Bow [12]
            p = round( p + ( lit% * p1), 4)
            lit% = sav_lit%                     : return

L04930: REM - Wood Surround and Factory Mull Calc  [13] - OBSOLETE
            p = round( p + ( p1 * wf3) + wf1 - wf2, 4)
            return

L04940: REM - Wood Surround  - Take 2  [14]
            p = round( p + (p1 / wsfm_nbr_windows%), 4)
            return

L04950: REM - Add To Price & Allow Additional Link  [15] /*EWD001*/
/* (SR67154) */
            if nfrcLiting% = 0% then p1 = 0.00      /* (SR67154) */

            p = round( p + p1, 4)                       /* -New- */
/*(AWD034)*/
            pp = p
            p_key%, link% = 0%
            str(c_cal$,4%,3%) = "LNK"
            phase% = 5% : gosub display_debug
            return

L04960: REM - New Code [16]

            return

L04970: REM - Allow Additional Link  [17]               /* (AWD034) */

            p_key%, link% = 0%
            str(c_cal$,4%,3%) = "LK2"
            phase% = 5% : gosub display_debug
            return

        check_min_price                              /* (EWD005) Begin */
            p_min = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "PRICE 024"
            str(readkey$,10%,3%) = str(p$,1%,3%)         /* Model Code */
            str(readkey$,13%,1%) = str(p$,4%,1%)         /* Color Code */
            str(readkey$,14%,3%) = str(d_key$,26%,3%)    /* Calc Code  */
            read #2,key = readkey$, using L08080, desc$,                 ~
                                                eod goto check_min_done
            convert str(desc$,1%,8%) to p_min, data goto check_min_done

        check_min_done
        return                                        /* (EWD005) End  */

        check_sash_only                               /* (EWD013) Begin */
            if pc_r$ = "01" and new_const% = 1% then check_sash
            if pc_r$ <> "04" then return              /* Grid Number    */
        check_sash
            init(" ") readkey$, desc$                 /* pricing        */
            str(readkey$,1%,9%)  = "PRICEGRID"
            str(readkey$,10%,2%) = str(lt$,1%,2%)        /* Grid  Code */
            read #2,key = readkey$, eod goto check_sash_done

REM            P1 = ROUND(P1 / 2.0, 4)
            sash_only% = 1%
        check_sash_done
        return

        check_plan_newc
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN NEWC"
            str(readkey$,10%,15%) = str(p$,1%,3%)
            read #2,key = readkey$, eod goto check_newc_done
                                                    /* All New Constr */
                 new_const% = 1%
        check_newc_done
        return
                                                      /* (EWD013) End   */



        check_pricegrdn                           /*  (AWD021) - BEG */
            pricegrdn% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PRICEGRDN"
            str(readkey$,10%,15%) = str(p$,7%,2%)
            read #2,key = readkey$, eod goto check_pricegrdn_done
                                                    /* All New Constr */
                  pricegrdn% = 1%
        check_pricegrdn_done
        return                                   /*  (AWD021) - END */



        check_lite                              /* (EWD013) Begin */
REM      LOGMSG$ = "?? " & PC_R$
REM      CALL "LOGFILE" (LOGMSG$)
REM      IF PC_R$ = "46" THEN GOTO CHECK_SDL /* <AWD027> */
            if pc_r$ <> "04" then return              /* Grid Number    */

            init(" ") readkey$, desc$                 /* pricing        */
            lite = 0.00
            str(readkey$,1%,9%)  = "PRICELITE"
            str(readkey$,10%,4%) = str(gl$,1%,2%) & str(lt$,1%,2%)
            read #2,key = readkey$, using L08080, desc$,           ~
                    eod goto check_lite_done

               convert str(desc$,1%,7%) to lite, data goto L04980

L04980:

               p1 = round(p1 + lite, 4)
           /* <AWD027> */
           return

check_SDL:
            init(" ") readkey$, desc$                 /* pricing        */
            lite = 0.00
            str(readkey$,1%,9%)  = "PRICELITE"
            str(readkey$,10%,2%) = str(lt$,1%,2%)
REM   LOGMSG$ = "** " & READKEY$
            read #2,key = readkey$, using L08080, desc$,           ~
                    eod goto check_lite_done

               convert str(desc$,1%,7%) to lite, data goto L04990
L04990:
             convert p1 to des1$, pic (####0.00)
             convert lite to des2$, pic (####0.00)
REM   LOGMSG$ = LOGMSG$ &  " > " & DES1$ & "  " & DES2$
               p1 = round(p1 * lite, 4)

           /* </AWD027> */
        check_lite_done
REM   CALL "LOGFILE" (LOGMSG$)
        return



        REM *************************************************************~
            *                   L O A D   D A T A                       *~
            *************************************************************

REM     *-------------------------------------*
        scan_def_data                        /* Find Starting Position */
REM     *-------------------------------------*
            no_key% = 0%                     /* 0=Key '>',1=Key '='    */
            link% = 0% : cnt% = 0%           /* for Scan               */
            d_key$ = " " : d% = 25%          /* Length of Scan Key     */
            str(d_key$,1%,1%) = "A"          /* Only Active Data       */
            str(d_key$,2%,4%) = pc_c$        /* Applicable Price Cat.  */
            str(d_key$,6%,2%) = pc_cm$       /* Price Cat. Calc Method */
            str(d_key$,8%,16%)= pc_m$        /* MFG Model Code         */
            str(d_key$,24%,2) = pc_r$        /* Ref Type Code          */
            f% = 3%
            if pc_r$ = "01" or pc_r$ = "03" then f% = 4%

            gosub cross_ref

            if c_r% = 99% then return

            sav_def$ = str(d_key$,1%, d%)    /* Save Scan Definition   */

        scan_def_nxt
            cal$ = " "                       /* Special Calc(PRICE 010)*/
            if no_key% = 0% then             /* File = (APCPCMST)      */~
               read #f%,key > d_key$, using  L05220, d_key$, cal$,          ~
                                                           eod goto L05260 ~
                             else                                        ~
               read #f%,key = d_key$, using  L05220, d_key$, cal$,          ~
                                                            eod goto L05260
L05220:            FMT POS(9), CH(53), POS(110), CH(3)
            if str(d_key$,1%,d%) <> sav_def$ then goto L05260
                if cal$ = "007" then gosub calc_panels
                return

L05260:     cnt% = cnt% + 1%
            if sav_cat$ <> "0000" then goto L05340
               if cnt% <> 1% then goto L05590
REM !!!!! MAY NEED CHANGING FOR VIRTUAL MODEL
                  pc_m$ = "000"                  /* 1st for APC Catalog*/
                  str(sav_def$,8%,16%) = pc_m$   /* Set for All Models */
                  d_key$ = " "
                  str(d_key$,1%,d%) = sav_def$
                  goto scan_def_nxt

L05340:     if cnt% <> 1% then goto L05410
               pc_m$ = "000"                     /* 1st for Special    */
               str(sav_def$,8%,16%) = pc_m$      /* Catalog Set for all*/
               d_key$ = " "                      /* Models             */
               str(d_key$,1%,d%) = sav_def$
               goto scan_def_nxt

L05410:     if cnt% <> 2% then goto L05520
               pc_c$  = "0000"                   /* 2nd for Special   */
               pc_cm$ = "00"                     /* Set to APC Catalog*/
               pc_m$  = sav_mod$                 /* for Model         */
               str(sav_def$,2%,4%) = pc_c$
               str(sav_def$,6%,2%) = pc_cm$
REM !!!!! MAY NEED CHANGING FOR VIRTUAL MODEL  CHANGE SAV_MOD$, PC_M$
               str(sav_def$,8%,16%)= pc_m$
               d_key$ = " "
               str(d_key$,1%,d%) = sav_def$
               goto scan_def_nxt

L05520:     if cnt% <> 3% then goto L05590
               pc_m$ = "000"                     /* 3rd for Special    */
               str(sav_def$,8%,16%) = pc_m$      /* Set to APC Catalog */
               d_key$ = " "                      /* for All Models     */
               str(d_key$,1%,d%) = sav_def$
               goto scan_def_nxt

L05590:     c_r% = 99%
            err% = 1%                          /* Unable to Calc Price */
            return

REM     *-------------------------------*
        cross_ref
REM     *-------------------------------*
       /* (PRICE 011) Special C_R% Code */
       /* Used to Flag Special Calc's   */

            if pc_r$ <> "01" then goto L05680
                lit% = 0% : sav_lit% = 0%       /* Init Flags for No. Lits */

L05680:     c_r% = 0%                           /* Value of Cross Reference*/
            if c_ref$ = "A" then c_r% = 10%     /* Lock Correction-WWW/HHH */
            if c_ref$ = "B" then c_r% = 11%     /* MFG Wind-<019><Link>    */
            if c_ref$ = "C" then c_r% = 12%     /* Screen Deduc-<002><003> */
            if c_ref$ = "D" then c_r% = 13%     /* FctMull <SNG><TWN><TPL> */
            if c_ref$ = "E" then c_r% = 14%     /* (EWD006) Casement       */

            convert c_ref$ to c_r%, data goto L05740
L05740:
            if c_r% = 0% then goto L05790     /* Cross Ref Not Applic    */

            on c_r% goto L05810, L05870, L05980, L06140, L06180, L06310, ~
                         L06350, L06370, L06440, L06540, L06680, L06750, ~
                         L06900, L06920
                                              /* (EWD006) add ref 'E'  */
L05780:     c_r% = 99%                        /* Not Applicable        */
L05790:     return                            /* Clean Exit            */

L05810: REM (1%) Grid Sizes      < None >     < Grid >   - Key
            d% = 28%                          /* Scan Key Length         */
            if c_cal$ = "005006000" then no_key% = 1%

            str(d_key$,26%,3%) = str(c_cal$,1%,3%)
            if lt$ = "00" or lt$ = "97" then return
/*(CR839) IF LT$ = "88" THEN RETURN */                 /* (EWD014)       */
/*(CR839) IF LT$ = "58" THEN RETURN */                 /* (AWD020)       */
/*(CR839) IF LT$ = "57" THEN RETURN */                 /* (AWD026)       */
/*(CR839) IF LT$ = "TD" OR LT$ = "TP" THEN RETURN */   /* (AWD032)       */
            if lt$ = "TD" then return                  /* (AWD032)       */
            if str(p1$,1,1) = "4" then return            /* AWD027 */
            gosub check_pricegrdn                      /* (AWD021)       */
            if pricegrdn% = 1% then return             /* (AWD021)       */

               gosub check_plan_newc                   /* (EWD013)       */
               gosub check_sash_only                   /* (EWD013)       */
               str(d_key$,26%,3%)   = str(c_cal$,4%,3%)  /* With Grid    */
                                                         /* (EWD013)     */
               if pass_sash% <> 0% then return
               if sash_only% = 1% then str(d_key$,26%,3%) = str(c_cal$,1%,3%)
            return

L05870: REM (2%) TWIN/TRIPLE     < Twin >     < Triple > - No Key
             lt% = 1%                           /* Hinge Code Table        */
             call "APCPR1SB" (0%, 5%, hg$, tab_desc$, p%, #2, tab_rec%)
             if tab_rec% = 0% then goto L05780
                if str(tab_desc$,p%+2%,4%) = "TWIN" then lt% = 2%
                if str(tab_desc$,p%+2%,4%) = "TRPL" then lt% = 3%
*EWD000                           goto L05780    /* TEMP 5/20 - ERN   */
                if lt% = 1% then goto L05780
                   no_key% = 1% : d% = 28%
                   if lt% = 2% then str(d_key$,26%,3%)=str(c_cal$,1%,3%) ~
                               else str(d_key$,26%,3%)=str(c_cal$,4%,3%)
                   return

L05980: REM (3%) COT/ORIEL       < Cot  >     < Oriel  > - No Key
             call "APCPR1SB" (0%, 5%, hg$, tab_desc$, p%, #2, tab_rec%)
             co_or% = 0%
             if tab_rec% = 0% then goto L05780
                if str(tab_desc$,1%,2%) = "CO" then co_or% = 1%
                if str(tab_desc$,1%,2%) = "OR" then co_or% = 2%
                if co_or% = 0% then goto L05780          /* Not Applic. */
                   no_key% = 1% : d% = 28%
                if co_or% = 1% then str(d_key$,26%,3%)=str(c_cal$,1%,3%) ~
                               else str(d_key$,26%,3%)=str(c_cal$,4%,3%)

                if co_or% = 1% and str(d_key$,26%,3%)  = "002" then      ~
                   no_key% = 0%
                if co_or% = 2% and str(d_key$,26%,3%)  = "003" then      ~
                   no_key% = 0%
                                                     /*  (EWD019) - Beg */
                if clmr$ = " " or clmr$ = "000" then return

                if co_or% = 1% then str(d_key$,26%,3%)= "006"
                if co_or% = 2% then str(d_key$,26%,3%)= "007"
                                                     /*  (EWD019) - END */

                return

L06140: REM (4%) Special Disc.   < Discount >            - No key
                no_key% = 1% : d% = 28%
                str(d_key$,26%,3%)   = str(c_cal$,1%,3%)
            return

L06180: REM (5%) GRID UP CHARGE  < Grid >                - No Key
            if lt$ <> "00" then goto L06210
               goto L05780                             /* Not Applicable */
L06210:     d% = 28%

/* (CR2444) Begin */
REM IF STR(PC_M$,1%,3%) = "315" OR STR(PC_M$,1%,3%) = "316" THEN NO_KEY% = 1%
                                                     /*   (EWD016) */
REM IF STR(PC_M$,1%,3%) = "335" OR STR(PC_M$,1%,3%) = "336" THEN NO_KEY% = 1%
                                                     /*SKIP PATIO CHECK*/
REM IF STR(PC_M$,1%,2%) <> "31" AND          /* PATIO DOOR */    ~
REM STR(PC_M$,1%,2%) <> "32" AND          /*  (EWD016)  */    ~
REM STR(PC_M$,1%,2%) <> "33" THEN NO_KEY% = 1%

            gosub checkPatio            /* To price by number of panels */
              if isdoor% = 1% then no_key% = numofpanel%    
              if isdoor% = 0% then no_key% = 1%
              
            

/* (CR2444) End */            

            
            if str(pc_m$,1%,3%) = "A41" then no_key% = 0%
            if str(pc_m$,1%,3%) = "4W2" then no_key% = 0%
            if str(pc_m$,1%,3%) = "4W5" then no_key% = 0%
            if str(pc_m$,1%,3%) = "4W8" then no_key% = 0%
            if str(pc_m$,1%,3%) = "4H2" then no_key% = 0%
            if str(pc_m$,1%,3%) = "4H8" then no_key% = 0%

            str(d_key$,26%,3%)   = str(c_cal$,1%,3%)  /* Colonial Grid */
REM         IF LT$ <> "97" THEN RETURN
REM            IF LT$ <> "97" THEN GOTO L06220           /* (EWD014)      */
/* (AWD032) */
             if lt$ = "97" then goto diamond
             if lt$ = "TD" then goto diamond
                goto L06220
diamond:
               no_key% = 1%
               str(d_key$,26%,3%)   = str(c_cal$,4%,3%) /* Diamond Grid*/
L06220:     REM IF LT$ <> "88" THEN RETURN                  /* (EWD014)    */
/*(CR839) IF LT$ = "88" THEN GOTO PRAIRIE */
/*(CR839) IF LT$ = "58" THEN GOTO PRAIRIE */
/*(CR839) IF LT$ = "57" THEN GOTO PRAIRIE */
/* (AWD032) */
/*(CR839) IF LT$ = "TP" THEN GOTO PRAIRIE */
REM            convert no_key% to no_key$,pic(0)
REM            call "SHOSTAT" ("GRID DOOR -> " & no_key$ )
REM            stop

            return
                                                               /*(AWD020)*/
REM IF LT$ <> "88" AND LT$ <> "58" AND LT$ <> "57" THEN RETURN

/*(CR839) PRAIRIE */
/*(CR839) NO_KEY% = 1% */
/*(CR839) STR(D_KEY$,26%,3%) = STR(C_CAL$,7%,3%) */ /* PRAIRIE GRID*/
           return

L06310: REM (6%) Initialize Start of Calc
            d% = 28%
            str(d_key$,26%,3%) = str(c_cal$,1%,3%)

            lt% = 0%
REM            lt% = pos("JML" = lt$)
            if lt% = 0% then return

            str(d_key$,26%,3%) = str(c_cal$,4%,3%)
            lt% = 0%
REM         if str(lt$,2%,1%) = "0" then return
REM         if lt% = 1% then return
REM         str(d_key$,26%,3%) = str(c_cal$,7%,3%)
            return

L06350: REM (7%) Initialize and Special Calc Storm Windows
            return

L06370: REM (8%) Initialize Start of Calc and Set Number of Lits
            d% = 28% : lit% = 0%
            str(d_key$,26%,3%) = str(c_cal$,1%,3%)
            convert str(c_cal$,4%,3%) to lit%, data goto L06410
L06410:     sav_lit% = lit%
            return

L06440: REM (9%) GRID UP-CHG BAY/BOW <GRID><DIAM>        - No Key
            if lt$ = "00" then goto L05780            /* Not Applicable */

            no_key% = 1% : d% = 28%
            str(d_key$,26%,3%)   = str(c_cal$,1%,3%)      /* Colonial Grd*/
            if lt$ = "97" or lt$ = "98" then             ~
                str(d_key$,26%,3%)   = str(c_cal$,4%,3%)  /* Diamond Grid*/
/*(AWD032)*/                                               /* Diamond Grid*/
            if lt$ = "TD" then str(d_key$,26%,3%)   = str(c_cal$,4%,3%)

            if lt$ = "80" or lt$ = "98" then lit% = 2%    /* 2 Flankers  */
                                                          /* (EWD014)    */
                                                          /* Prairie Grid*/
/*(CR839) IF LT$ = "88" THEN STR(D_KEY$,26%,3%)   = STR(C_CAL$,7%,3%) */
                                                           /* (AWD020)   */
/*(CR839) IF LT$ = "58" THEN STR(D_KEY$,26%,3%)   = STR(C_CAL$,7%,3%) */
                                                           /* (AWD026)  */
/*(CR839) IF LT$ = "57" THEN STR(D_KEY$,26%,3%)   = STR(C_CAL$,7%,3) */

/*(AWD032)*/
/*(CR839) IF LT$ = "TP" THEN STR(D_KEY$,26%,3%)   = STR(C_CAL$,7%,3) */
            return

L06540: REM (A) Lock Correction for Width and Height
            if lk$ <> "2" and lk$ <> "4" then return
                lk_size = 27.25                                 /* (EWD008) */
REM                IF PC_M$ = "215" OR PC_M$ = "216" OR PC_M$ = "217" OR
REM                   PC_M$ = "218" THEN LK_SIZE = 35.75
                                                                /* (EWD008) */
/* (SR73304) */
REM                LK_VAL = 7.00
REM                lk_val = 10.50   /* (CR3016) */
                lk_val = 21.00
                pc_kunt$ = "0"
                gosub unit_convert
REM                LK_VAL = 7.00
                lk_val = 10.50  /* (CR3016) */
                lk_val = 21.00
                gosub check_locks
REM                IF LK% = 1% THEN LK_VAL = 4.00
                if lk% = 1% then lk_val = 6.00
                                                  /* (EWD008) Size Variable */
                if str(c_cal$,1%,3%) <> "WWW" then goto L06660
                    if w >= lk_size then p = round(p - lk_val, 2) /*Dec. WD */
                    return
L06660:         if h >= lk_size then p = round(p - lk_val, 2)     /* Dec. HT*/
                return
                                                  /* (EWD008) Size Variable */
L06680: REM (B) Initialize and Set up Possible Link
            d% = 28%
            str(d_key$,26%,3%)   = str(c_cal$,1%,3%)
            str(sav_link$,1%,2%) = "01"
            str(sav_link$,3%,3%) = str(c_cal$,4%,3%)
            str(sav_link$,6%,3%) = str(c_cal$,7%,3%)
            return

L06750: REM (C) Set Up for Screen Deduction
/*EWD001*/  d% = 28%
/* !!! Take away A option */
/* (AWD036) add code 7 */
            sc% = pos("04567" = sc$)         /* Screen Deduct Required */
            if sc% <> 0% then goto L06810
/*EWD001*/      str(d_key$,26%,3%) = "000"   /* Check for Up-Charge    */
/*EWD001*/      no_key% = 1%
                return

L06810:     str(d_key$,26%,3%)   = str(c_cal$,1%,3%)    /* STD Size    */
            str(sav_link$,1%,2%) = "06"
            str(sav_link$,3%,3%) = str(c_cal$,4%,3%)    /* Custom Size */
            str(sav_link$,6%,3%) = str(c_cal$,7%,3%)
            return

L06900: REM (D) Factory Mulling
                                                        /* (EWD007) */
                                               /* (EWD010) - Add XP */
                                               /* (EWD011) - Add XW */
            if wsfm_instruction$ = "WS" or wsfm_instruction$ = "CP"      ~
                                           then goto LWS_1
            if wsfm_instruction$ = "XP" or wsfm_instruction$ = "XW"      ~
                                           then goto LWS_1
            if wsfm_instruction$ = "CW" or wsfm_instruction$ = "SW"      ~
                                           then goto LWS_1
            if wsfm_instruction$ = "PS" or wsfm_instruction$ = "PW"      ~
                                           then goto LWS_1
            /* Determine number of windows; max out at 3 -- */
            /* charges for over 3 are handled when adding   */
            /* up the price (Val Calc Method must be 2)     */
            temp386% = wsfm_nbr_windows% + top_nbr_windows%
            if temp386% > 3% then temp386% = 3%
            temp387% = (temp386% * 3%) - 2%
            if str(c_cal$, temp387%, 3) <> "000" then L06910
                c_r% = 99%
                return
LWS_1:                                                  /* (EWD007)  */
            ws_no% = wsfm_nbr_windows% + top_nbr_windows%
            if ws_no% > 1% then goto LWS_2            /* Single Unit */
               if wsfm_instruction$ = "WS" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "035"           /* Standard  */
               if wsfm_instruction$ = "WS" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "037"           /* Custom    */
               if wsfm_instruction$ = "CP" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "036"           /* Standard  */
               if wsfm_instruction$ = "CP" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "038"           /* Custom    */
                                                        /* (EWD010)  */
               if wsfm_instruction$ = "XP" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "039"           /* Standard  */
               if wsfm_instruction$ = "XP" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "040"           /* Custom    */


               if wsfm_instruction$ = "PS" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "047"           /* Standard  */
               if wsfm_instruction$ = "PS" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "048"           /* Custom    */
                                                        /* (AWD023)  */

               if wsfm_instruction$ = "PW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "049"           /* Standard  */
               if wsfm_instruction$ = "PW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "050"           /* Custom    */
                                                        /* (AWD023)  */

               goto LWS_3

LWS_2:                                             /* Multiple Units */

/* (AWD025)  */
           if sp_mull% = 1% then goto LWS_4
               if wsfm_instruction$ = "WS" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "031"           /* Standard  */
               if wsfm_instruction$ = "WS" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "033"           /* Custom    */
               if wsfm_instruction$ = "CP" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "032"           /* Standard  */
               if wsfm_instruction$ = "CP" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "034"           /* Custom    */
                                                        /* (EWD010)  */
               if wsfm_instruction$ = "XP" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "041"           /* Standard  */
               if wsfm_instruction$ = "XP" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "042"           /* Custom    */
                                                        /* (EWD011)  */
               if wsfm_instruction$ = "XW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "019"           /* Standard  */
               if wsfm_instruction$ = "XW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "020"           /* Custom    */
               if wsfm_instruction$ = "CW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "021"           /* Standard  */
               if wsfm_instruction$ = "CW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "022"           /* Custom    */
               if wsfm_instruction$ = "SW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "023"           /* Standard  */
               if wsfm_instruction$ = "SW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "024"           /* Custom    */


               if wsfm_instruction$ = "PS" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "043"           /* Standard  */
               if wsfm_instruction$ = "PS" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "044"           /* Custom    */
                                                        /* (AWD023)  */

               if wsfm_instruction$ = "PW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "045"           /* Standard  */
               if wsfm_instruction$ = "PW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "046"           /* Custom    */
                                                        /* (AWD023)  */

/* (AWD025) */
                  goto LWS_3


LWS_4:


               if wsfm_instruction$ = "WS" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "131"           /* Standard  */
               if wsfm_instruction$ = "WS" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "133"           /* Custom    */
               if wsfm_instruction$ = "CP" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "132"           /* Standard  */
               if wsfm_instruction$ = "CP" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "134"           /* Custom    */

               if wsfm_instruction$ = "XP" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "141"           /* Standard  */
               if wsfm_instruction$ = "XP" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "142"           /* Custom    */

               if wsfm_instruction$ = "XW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "119"           /* Standard  */
               if wsfm_instruction$ = "XW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "120"           /* Custom    */
               if wsfm_instruction$ = "CW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "121"           /* Standard  */
               if wsfm_instruction$ = "CW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "122"           /* Custom    */
               if wsfm_instruction$ = "SW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "123"           /* Standard  */
               if wsfm_instruction$ = "SW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "124"           /* Custom    */


               if wsfm_instruction$ = "PS" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "143"           /* Standard  */
               if wsfm_instruction$ = "PS" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "144"           /* Custom    */

               if wsfm_instruction$ = "PW" and custom% = 0% then        ~
                  str(d_key$,26%, 3%) = "145"           /* Standard  */
               if wsfm_instruction$ = "PW" and custom% = 1% then        ~
                  str(d_key$,26%, 3%) = "146"           /* Custom    */

/* (AWD025) - end */



LWS_3:         str(c_cal$,1%,3%) = str(d_key$,26%,3%)
               d% = 28%
               no_key% = 1%
               convert ws_no% to ws_no$, pic(#)
               phase% = 6% : gosub display_debug
               return
                                                        /* (EWD007)  */
L06910:     d% = 28%
            str(d_key$,26%, 3%) = str(c_cal$, temp387%, 3%)
            mull_c_cal$         = str(c_cal$, temp387%, 3%)
            no_key% = 1%                                /* Equal Key Lookup*/
            return
                                                        /* (EWD006)        */
L06920: REM (14%) Grid Sizes      < None > < Grid > < Lits >  - Key
            d% = 28% : lit% = 0%                        /* Scan Key Length */
            str(d_key$,26%,3%) = str(c_cal$,1%,3%)
            convert str(c_cal$,7%,3%) to lit%, data goto L06930

L06930:     sav_lit% = lit%
REM            if lt$ = "00" or lt$ = "97" then return
/* (AWD032) */
            if lt$ = "00" or lt$ = "97" or lt$ = "TD" then return
               str(d_key$,26%,3%)   = str(c_cal$,4%,3%)  /* With Grid      */
            return
                                                         /* (EWD006)       */
REM     *------------------------------------------*
        read_calc
REM     *------------------------------------------*
            pc_key$ = all(hex(00))
            str(pc_key$,1%,2%) = str(d_key$,24%,2%) /* Ref Type Code   */
            str(pc_key$,3%,3%) = str(d_key$,26%,3%) /* Ref Calc Method */
            read #1,key = pc_key$, eod goto L07240
            get #1, using  L07070,  pc_r$, pc_rc$, pc_kdesc$(1%),        ~
                                    pc_kdesc$(2%), pc_kdesc$(3%),        ~
                                    pc_kfld%(1%),  pc_kfld%(2%),         ~
                                    pc_kfld%(3%),  pc_kbeg%(1%),         ~
                                    pc_kbeg%(2%),  pc_kbeg%(3%),         ~
                                    pc_klen%(1%),  pc_klen%(2%),         ~
                                    pc_klen%(3%),  pc_vtbl$, pc_vtln%,   ~
                                    pc_vdesc$,                           ~
                                    pc_vtblv$(1%), pc_vtblv$(2%),        ~
                                    pc_vtblv$(3%), pc_vtblv$(4%),        ~
                                    pc_vtblv$(5%), pc_vtblv$(6%),        ~
                                    pc_vfmt$,      pc_vcalc$, pc_kcalc$, ~
                                    pc_kunt$,      pc_kfil$
L07240:     return

L07070: FMT                     /* (APCPCMSK) (Key) Definition File    */~
            CH(02),             /* Std/Spc Reference Code (PRICE 002)  */~
            CH(03),             /* Std/Spc Ref. Calc Cde- (PRICE 003)  */~
            3*CH(03),           /* Key Field Descript   - (PRICE 004)  */~
            3*BI(1),            /* Key Field Def. Code  - (PRICE 005)  */~
            3*BI(1),            /* Key Field Start Pos  - Pos(11,2)    */~
            3*BI(1),            /* Key Field Length     - Pos(14,2)    */~
            CH(02),             /* Value Field Table      (PRICE 005)  */~
            BI(1),              /* Length of Table Code                */~
            CH(03),             /* Value Field Descripti  (PRICE 004)  */~
            6*CH(03),           /* Table Code Values - From Table      */~
            CH(02),             /* Value Format Code      (PRICE 006)  */~
            CH(02),             /* Value Calc Method      (PRICE 007)  */~
            CH(02),             /* Key Calc Method        (PRICE 008)  */~
            CH(02),             /* Key Unit Conversion    (PRICE 009)  */~
            CH(09)              /* Filler Area                         */


REM     *---------------------------------------*
        load_link
REM     *---------------------------------------*
            link% = 1%
            pc_link$ = " "
            tab_val$ = str(s_key$,24%,5%)
            if str(s_key$,26%,3%) <> "019" or str(s_key$,24%,2%) <> "01" ~
                    then goto L07360    /*EWD001*/
                pc_link$ = str(sav_link$,3%,3%)    /* PART - CROSS-REF 6 */
                                                         /* (EWD003)     */
               if pc_link$ = "008" and str(pc_m$,1%,3%) = "312"       ~
                                                      then cal$ = "007"
                                                         /* (EWD003)     */
               if pc_link$ = "008" and str(pc_m$,1%,3%) = "332"       ~
                                                       then cal$ = "007"
                                                         /* (EWD016)     */
                if cal$ = "007" then gosub calc_panels
                pc_c$  = "0000"
                pc_cm$ = "00"
                goto L07490

L07360:     if str(s_key$,26%,3%) <> "020" or str(s_key$,24%,2%) <> "01" ~
                    then goto L07380    /*EWD001*/
                pc_link$ = str(sav_link$,3%,3%)    /* PART - CROSS-REF 1 */
REM                if lt$ <> "00" and lt$ <> "97" then                        ~
                                          pc_link$ = str(sav_link$,6%,3%)
                if lt$ <> "00" and lt$ <> "97"  and lt$ <> "TD" then          ~
                                          pc_link$ = str(sav_link$,6%,3%)
                pc_c$  = "0000"
                pc_cm$ = "00"
                goto L07490
                                                         /* (EWD004)    */
L07380:     if str(s_key$,26%,3%) <> "024" or str(s_key$,24%,2%) <> "01"  ~
                    then goto L07440
                pc_link$ = str(sav_link$,3%,3%)    /* PART - CROSS-REF 1 */
                pc_c$  = "0000"
                pc_cm$ = "00"
                goto L07490

L07440:     if str(s_key$,24%,5%) <> "41003" then goto L07445
               if str(c_cal$,4%,3%) <> "LK2" then pc_link$ = "ZZZ"
               if str(c_cal$,4%,3%) <> "LK2" then goto L07490

L07445:     if str(s_key$,24%,5%) <> "03006" then goto L07450
               lowe006% = 1%
               p = p - united_price
REM               call "SHOSTAT" ("HERE AT 03-006" )  stop
               goto L07455

L07450:     if lowe006% = 0% then goto L07455
               lowe006% = lowe006% + 1%
               goto L07490
                                                         /* (EWD004)    */
L07455:
/* (SR67154) */
            if str(s_key$,24%,5%) = "16000" then cal$ = "001"

            tab_1% = 3% : tab_2% = 0%
            call "APCPR1SB" ( tab_1%, tab_2%, tab_val$, tab_desc$,        ~
                                                       p%, #2, tab_rec% )
            if tab_rec% = 0% then return
                pc_link$ = str(tab_desc$,22%,3%)
                if ffoam% = 1% then goto L07490
                   if str(s_key$,24%,2%) = "45" and pc_link$ = "001" ~
                          then pc_link$ = "ZZZ"

L07490:     if pc_link$ = "ZZZ" then return
                str(d_key$,26%,3%) = pc_link$
                link% = 2%
                return

REM     *----------------------------------------*
        calc_panels
REM     *----------------------------------------*
           panels$ = "02"
           if hg$ = "37" then panels$ = "03"
           if hg$ = "34" then panels$ = "03"           /* (EWD009) */
           if hg$ = "42" then panels$ = "04"
           if hg$ = "39" then panels$ = "03"           /* (AWD020) */
           if hg$ = "60" then panels$ = "03"           /* (AWD037) */
           if hg$ = "60" then panels$ = "03"           /* (AWD037) */
         return


REM     *-------------------------------------------*
        calc_glass_special
REM     *-------------------------------------------*
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 014"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_glass_end
L08080:       FMT POS(25), CH(30)
           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_glass_end
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PRICE 016"   /* Spec Price Glass*/
              str(readkey$,10%,3%)  = str(p$,1%,3%) /* Model Code      */
              str(readkey$,13%,12%) = str(p$,5%,2%) /* Glass Code      */
              read #2,key = readkey$, using L08080 , desc$,                ~
                                                  eod goto calc_glass_end
              t_prc = 0.0
              convert str(desc$,1%,10%) to t_prc, data goto L08220
L08220:
              temp1% = 1%                                   /*EWD001*/
              if pc_rc$ = "002" and sav_lit% > temp1%       /*EWD001*/   ~
                then temp1% = sav_lit%                      /*EWD001*/
              p  = round(p - (p1 * temp1%), 2)              /*EWD001*/
              p1 = t_prc
              p  = round(p + (p1 * temp1%), 2)              /*EWD001*/
        calc_glass_end
        return


REM     *-------------------------------------------*
        calc_glass_special1         /*  (AWD030) */
REM     *-------------------------------------------*
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 037"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_glass_end1

           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_glass_end1
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PRICE 038"   /* Spec Price Glass*/
              str(readkey$,10%,3%)  = str(p$,1%,3%) /* Model Code      */
              str(readkey$,13%,12%) = str(p$,5%,2%) /* Glass Code      */
              read #2,key = readkey$, using L08080 , desc$,                ~
                                                  eod goto calc_glass_end1
              t_prc = 0.0
              convert str(desc$,1%,10%) to t_prc, data goto L08235
L08235:
              temp1% = 1%
              if pc_rc$ = "002" and sav_lit% > temp1%          ~
                then temp1% = sav_lit%
              p  = round(p - (p1 * temp1%), 2)
              p1 = t_prc
              p  = round(p + (p1 * temp1%), 2)
        calc_glass_end1
        return

/* (AWD030\) */

REM     *-------------------------------------------*
        calc_glass_special2         /*  (AWD031) */
REM     *-------------------------------------------*
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 039"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_glass_end2

           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_glass_end2
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PRICE 040"   /* Spec Price Glass*/
              str(readkey$,10%,3%)  = str(p$,1%,3%) /* Model Code      */
              str(readkey$,13%,12%) = str(p$,5%,2%) /* Glass Code      */
              read #2,key = readkey$, using L08080 , desc$,                ~
                                                  eod goto calc_glass_end2
              t_prc = 0.0
              convert str(desc$,1%,10%) to t_prc, data goto L08240
L08240:
              temp1% = 1%
              if pc_rc$ = "002" and sav_lit% > temp1%          ~
                then temp1% = sav_lit%
              p  = round(p - (p1 * temp1%), 2)
              p1 = t_prc
              p  = round(p + (p1 * temp1%), 2)
        calc_glass_end2
        return

/* (AWD031\) */
REM     *-------------------------------------------*
        calc_glass_special3         /*  (AWD033) */
REM     *-------------------------------------------*
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 041"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_glass_end3

           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_glass_end3
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PRICE 042"   /* Spec Price Glass*/
              str(readkey$,10%,3%)  = str(p$,1%,3%) /* Model Code      */
              str(readkey$,13%,12%) = str(p$,5%,2%) /* Glass Code      */
              read #2,key = readkey$, using L08080 , desc$,                ~
                                                  eod goto calc_glass_end3
              t_prc = 0.0
              convert str(desc$,1%,10%) to t_prc, data goto L08241
L08241:
              temp1% = 1%
              if pc_rc$ = "002" and sav_lit% > temp1%          ~
                then temp1% = sav_lit%
              p  = round(p - (p1 * temp1%), 2)
              p1 = t_prc
              p  = round(p + (p1 * temp1%), 2)
        calc_glass_end3
        return

/* (AWD033\) */

REM     *-------------------------------------------*
        calc_grid_special       /* (AWD028) Begin */
REM     *-------------------------------------------*
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 033"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_grid_end1

           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_glass_end
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PRICE 034"   /* Spec Price Glass*/
              str(readkey$,10%,3%)  = str(p$,1%,3%) /* Model Code      */
              read #2,key = readkey$, using L08080 , desc$,                ~
                                                  eod goto calc_grid_end1
              t_prc = 0.0
              convert str(desc$,1%,10%) to t_prc, data goto L08230
L08230:
              temp1% = 1%
              p  = round(p - p1, 2)
              p1 = t_prc
              p  = round(p + p1, 2)
        calc_grid_end1
        return

/* (\AWD028) */

REM     *-------------------------------------------*
        calc_grid_special1       /* (AWD029) Begin */
REM     *-------------------------------------------*
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 035"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_grid_end

           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_glass_end
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PRICE 036"   /* Spec Price Glass*/
              str(readkey$,10%,3%)  = str(p$,1%,3%) /* Model Code      */
              read #2,key = readkey$, using L08080 , desc$,                ~
                                                  eod goto calc_grid_end
              t_prc = 0.0
              convert str(desc$,1%,10%) to t_prc, data goto L08225
L08225:
              temp1% = 1%
              p  = round(p - p1, 2)
              p1 = t_prc
              p  = round(p + p1, 2)
        calc_grid_end
        return

/* (\AWD029) */

REM     *-------------------------------------------*
        check_sdl_promo          /* (AWD035) Begin */
REM     *-------------------------------------------*
           for sdl% = 1% to max_8900_mdls%
             if str(p$,1%,3%) = mdl_8900$(sdl%) then goto check_sdl_promo_tbls
           next sdl%

           return

check_sdl_promo_tbls:
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 043"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_grid_end

           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_SDL_end


              p  = round(p - p1, 2)
              t_prc = p1
              t_prc = round(t_prc / 2, 4)
              p1 = t_prc
              p  = round(p + p1, 2)
              p_key% = 1%
        calc_SDL_end
        returN

        load_8900_mdl
         init(" ") readkey$
         mdls%, max_8900_mdls% = 0%
         str(readkey$,1%,13%) = "ELLISON048900"
        load_8900_nxt
         read #2,key > readkey$, using GEN_FMT, readkey$, desc$, eod goto not_cont
GEN_FMT:      FMT CH(24), CH(30)

            if str(readkey$,1,13) <> "ELLISON048900" then goto load_8900_done
              mdls% = mdls% + 1%
              mdl_8900$(mdls%) = str(desc$,1,3)
              goto load_8900_nxt
        load_8900_done
         beenherebefore% = 1%
         max_8900_mdls% = mdls%
        return

REM     *-------------------------------------------*
        calc_glass_special4
REM     *-------------------------------------------*
           init(" ") readkey$, desc$, t_dte$, b_dte$, e_dte$
           str(readkey$,1%,9%)   = "PRICE 044"/* Special Price Control */
           str(readkey$,10%,15%) = cuscode$   /* Specified Customer    */
           read #2,key = readkey$, using L08080 , desc$,                   ~
                                                eod goto calc_glass_end4

           b_dte$ = str(desc$,1%,8%)  : call "DATUNFMT" (b_dte$)
           e_dte$ = str(desc$,12%,8%) : call "DATUNFMT" (e_dte$)
           t_dte$ = date
           if t_dte$ < str(b_dte$,1%,6%) or t_dte$ > e_dte$ then         ~
                                                      goto calc_glass_end4
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PRICE 045"   /* Spec Price Glass*/
              str(readkey$,10%,3%)  = str(p$,1%,3%) /* Model Code      */
              str(readkey$,13%,12%) = str(p$,5%,2%) /* Glass Code      */
              read #2,key = readkey$, using L08080 , desc$,                ~
                                                  eod goto calc_glass_end4
              t_prc = 0.0
              convert str(desc$,1%,10%) to t_prc, data goto L08245
L08245:
              temp1% = 1%
              if pc_rc$ = "002" and sav_lit% > temp1%                    ~
                then temp1% = sav_lit%
              p  = round(p - (p1 * temp1%), 2)
              p1 = t_prc
              p  = round(p + (p1 * temp1%), 2)
        calc_glass_end4
        return

/* (\AWD035) */
REM     *----------------------------------------*
        check_locks
REM     *----------------------------------------*
            init(" ") readkey$ : lk% = 0%
            str(readkey$,1%,9%) = "PRICE 021"
            str(readkey$,10%,15%) = pc_m$
            read #2,key = readkey$, eod goto L08500
            lk% = 1%
L08500:     return

/* (SR67154) */
REM     *----------------------------------------*
        nfrc_checks
REM     *----------------------------------------*
REM        CALL "SHOSTAT" ("NFRC CHECKS ")  STOP
/* nfrc_checks only called when ref$ = "16" -- ref$ variable => pc_r$       */
          nfrcLiting%, nfrcObs% = 0%         /* cal$ = "000" is add to      */
                                             /* cal$ = "001" is deduct from */
REM          IF CAL$ <> "000" THEN GOTO NOT8300_450_460
          if cal$ <> "000" then goto not8300  /* (CR601) */
           if str(series$,1%,4%) = "150 " or series$ = "160 "           ~
                                              then nfrcLiting% = 1%
/*CR601*/ if str(series$,1%,4%) = "450 " or series$ = "460 "           ~
                                              then nfrcLiting% = 1%
/*CR700*/ if str(series$,1%,4%) = "8700" then nfrcLiting% = 1%


REM IF STR(SERIES$,1%,4%) <> "8300" AND   /* (CR601) */  ~
    STR(SERIES$,1%,4%) <> "450 " AND     ~
    STR(SERIES$,1%,4%) <> "460 " THEN GOTO NOT8300_450_460
            if str(series$,1%,4%) <> "8300" then goto not8300     /* (CR601) */

            if lt$ <> "00" and lt$ <= "99" then nfrcLiting% = 1%
            if lt$ > "99" then gosub checkCUSTGRID
        return
REM   NOT8300_450_460   /* (CR601) */
not8300:
        if cal$ <> "001" then goto notNorthernNFRC
          if str(series$,1%,4%) <> "150 " and       ~
             str(series$,1%,4%) <> "160 " then goto notNorthernNFRC
           if str(style$,1%,2%) <> "SH" and         ~
              str(style$,1%,2%) <> "SL" then goto notNorthernNFRC

            gosub checkTMP
            if tmp% = 1% then goto setnfrcOBS    /* Do not deduct on tempered */
              gosub checkDBL
              gosub checkOBS
              if obs% = 1% then goto setnfrcOBS
        notNorthernNFRC
        return
        setnfrcOBS
          nfrcobs% = 1%
        return



        checkCUSTGRID             /* Check CUSTGRID for Shapes */
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "CUSTGRID"
          str(readkey$,10%,15%) = lt$
          read #2,key = readkey$, using L09400, desc$,             ~
                                         eod goto notCUSTGRID

            if str(desc$,1%,2%) <> "00" then nfrcLiting% = 1%
        notCUSTGRID
        return
        checkTMP
          tmp% = 0%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PLAN TEMP"
          str(readkey$,10%,15%) = gl$
          read #2,key = readkey$, eod goto notTMP

            tmp% = 1%
        notTMP
        return
        checkOBS
          obs% = 0%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PRICE OBS"
          str(readkey$,10%,15%) = gl$
          read #2,key = readkey$, eod goto notOBS

            obs% = 1%
        notOBS
        return
        checkDBL
          dbl% = 0%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PLAN DBLE"
          str(readkey$,10%,15%) = gl$
          read #2,key = readkey$, eod goto notDBL

            dbl% = 1%
        notDBL
        return
/* (CR2444) begin */        
/* this check if for patio side lite or door that */
/* does not price by number of panels             */
        checkPatio
          isdoor% = 0%
          numofpanel% = 0%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PLAN DOOR"
          str(readkey$,10%,15%) = pc_m$
          read #2,key = readkey$, using L09400, desc$, eod goto notPatio

              isdoor% = 1%
              
              convert str(desc$,24%,2%) to numofpanel%, data goto notPatio
              
        notPatio
        return
/* (CR2444) end */                

REM     *-------------------------------------------*
        exit_program
REM     *-------------------------------------------*
            if pass_sash% <> 0% then goto finish_exit
            if sash_only% <> 1% or new_const% <> 1% then goto finish_exit
               pass_sash% = 1%
               sash_p1 = p
               goto L1000
        finish_exit
            if sash_only% <> 1% or new_const% <> 1% then goto L65000
               if pass_sash% <> 1% then goto L65000
                  if pc_r$ <> "04" then goto L65000
                  sash_deduct = 0.00
                  sash_deduct = round(p - sash_p1,4)
                  sash_deduct = round(sash_deduct / 2,4)
                  p = p - sash_deduct

L65000:     if p_key% = 1% then pp = p          /* Set Calculated Price */
            if p_key% = 0% and pc_r$ = "01" then err% = 1%  /* No Price */
                                                /* Spec Shape (EWD015)  */
            if p_key% = 0% and pc_r$ = "04" and str(p$,7%,2%) > "99"     ~
                           then err% = 1%
/* (AWD038)*/
REM not required to have uchannel or woodclips charge
            if pc_r$ = "09" and (mull_c_cal$ = "017" or mull_c_cal$ = "017") ~
               and err% = 1% then err% = 0%
            pc_c$  = sav_cat$
            pc_cm$ = sav_ccm$
            pc_m$  = sav_mod$
            end



REM     *********************************************************************
REM     D E B U G   S T U F F
REM     *********************************************************************

REM     *--------------------------*
        display_debug
REM     *--------------------------*
           if debug%(2%) = 0% then return
              if debug%(2%) = 99% then goto L08450  /* All Ref Calcs     */
                 xx% = 1%
                 convert pc_r$ to xx%, data goto L08420
L08420:
                 if debug%(2%) <> xx% then return /*Specified Reference*/

L08450:    gosub set_pf1
L08460:    accept                                                        ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
/*EWD001*/     at (01,28), fac(hex(a4)), apc$                   , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
/*EWD001*/     at (02,27), fac(hex(84)), txt$                   , ch(35),~
                                                                         ~
               at (04,02), fac(hex(a4)), hh1$                   , ch(35),~
               at (05,02), fac(hex(8c)), tt1$(1%)               , ch(20),~
               at (05,24), fac(hex(84)), dd1$(1%)               , ch(10),~
               at (06,02), fac(hex(8c)), tt1$(2%)               , ch(20),~
               at (06,24), fac(hex(84)), dd1$(2%)               , ch(10),~
               at (07,02), fac(hex(8c)), tt1$(3%)               , ch(20),~
               at (07,24), fac(hex(84)), dd1$(3%)               , ch(10),~
               at (08,02), fac(hex(8c)), tt1$(4%)               , ch(20),~
               at (08,24), fac(hex(84)), dd1$(4%)               , ch(10),~
               at (09,02), fac(hex(8c)), tt1$(5%)               , ch(20),~
               at (09,24), fac(hex(84)), dd1$(5%)               , ch(10),~
               at (10,02), fac(hex(8c)), tt1$(6%)               , ch(10),~
               at (10,14), fac(hex(84)), dd1$(6%)               , ch(25),~
                                                                         ~
               at (04,40), fac(hex(a4)), hh2$                   , ch(35),~
               at (05,40), fac(hex(8c)), tt2$(1%)               , ch(20),~
               at (05,62), fac(hex(84)), dd2$(1%)               , ch(10),~
               at (06,40), fac(hex(8c)), tt2$(2%)               , ch(20),~
               at (06,62), fac(hex(84)), dd2$(2%)               , ch(10),~
               at (07,40), fac(hex(8c)), tt2$(3%)               , ch(20),~
               at (07,62), fac(hex(84)), dd2$(3%)               , ch(10),~
               at (08,40), fac(hex(8c)), tt2$(4%)               , ch(20),~
               at (08,62), fac(hex(84)), dd2$(4%)               , ch(10),~
               at (09,40), fac(hex(8c)), tt2$(5%)               , ch(20),~
               at (09,62), fac(hex(84)), dd2$(5%)               , ch(10),~
               at (10,40), fac(hex(8c)), tt2$(6%)               , ch(20),~
               at (10,62), fac(hex(84)), dd2$(6%)               , ch(10),~
                                                                         ~
               at (11,02), fac(hex(a4)), hh3$                   , ch(35),~
               at (12,02), fac(hex(8c)), tt3$(1%)               , ch(20),~
               at (12,24), fac(hex(84)), dd3$(1%)               , ch(10),~
               at (13,02), fac(hex(8c)), tt3$(2%)               , ch(20),~
               at (13,24), fac(hex(84)), dd3$(2%)               , ch(10),~
               at (14,02), fac(hex(8c)), tt3$(3%)               , ch(20),~
               at (14,24), fac(hex(84)), dd3$(3%)               , ch(10),~
               at (15,02), fac(hex(8c)), tt3$(4%)               , ch(20),~
               at (15,24), fac(hex(84)), dd3$(4%)               , ch(10),~
               at (16,02), fac(hex(8c)), tt3$(5%)               , ch(20),~
               at (16,24), fac(hex(84)), dd3$(5%)               , ch(15),~
               at (17,02), fac(hex(8c)), tt3$(6%)               , ch(20),~
               at (17,24), fac(hex(84)), dd3$(6%)               , ch(10),~
                                                                         ~
               at (11,40), fac(hex(a4)), hh4$                   , ch(35),~
               at (12,40), fac(hex(8c)), tt4$(1%)               , ch(20),~
               at (12,62), fac(hex(84)), dd4$(1%)               , ch(15),~
               at (13,40), fac(hex(8c)), tt4$(2%)               , ch(20),~
               at (13,62), fac(hex(84)), dd4$(2%)               , ch(10),~
               at (14,40), fac(hex(8c)), tt4$(3%)               , ch(20),~
               at (14,62), fac(hex(84)), dd4$(3%)               , ch(15),~
               at (15,40), fac(hex(8c)), tt4$(4%)               , ch(20),~
               at (15,62), fac(hex(84)), dd4$(4%)               , ch(15),~
               at (16,40), fac(hex(8c)), tt4$(5%)               , ch(20),~
               at (16,62), fac(hex(84)), dd4$(5%)               , ch(10),~
               at (17,40), fac(hex(8c)), tt4$(6%)               , ch(20),~
               at (17,62), fac(hex(84)), dd4$(6%)               , ch(10),~
                                                                         ~
               at (18,02), fac(hex(a4)), hh5$                   , ch(35),~
               at (19,02), fac(hex(8c)), tt5$(1%)               , ch(20),~
               at (19,24), fac(hex(84)), dd5$(1%)               , ch(15),~
               at (20,02), fac(hex(8c)), tt5$(2%)               , ch(20),~
               at (20,24), fac(hex(84)), dd5$(2%)               , ch(15),~
               at (21,02), fac(hex(8c)), tt5$(3%)               , ch(20),~
               at (21,24), fac(hex(84)), dd5$(3%)               , ch(10),~
               at (22,02), fac(hex(8c)), tt5$(4%)               , ch(20),~
               at (22,24), fac(hex(84)), dd5$(4%)               , ch(10),~
                                                                         ~
               at (18,40), fac(hex(a4)), hh6$                   , ch(35),~
               at (19,40), fac(hex(8c)), tt6$(1%)               , ch(20),~
               at (19,62), fac(hex(84)), dd6$(1%)               , ch(10),~
               at (20,40), fac(hex(8c)), tt6$(2%)               , ch(20),~
               at (20,62), fac(hex(84)), dd6$(2%)               , ch(10),~
               at (21,40), fac(hex(8c)), tt6$(3%)               , ch(20),~
               at (21,62), fac(hex(84)), dd6$(3%)               , ch(10),~
               at (22,40), fac(hex(8c)), tt6$(4%)               , ch(20),~
               at (22,62), fac(hex(84)), dd6$(4%)               , ch(10),~
                                                                         ~
               at (24,02), fac(hex(a4)),   inp$                 , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L09360
                  call "PRNTSCRN"
                  goto L08460

L09360:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
               inp$ = "Press <Return> To Continue, or PF(15) to Print the~
        ~ Screen?"
               apc$ = "*Special Calculation-Debug Scrn*"
               pname$ = "APCPR2SB - Rev: 01.00"
               date$ = date
               call "DATEFMT" (date$)
            if sav_pc_r$ = pc_r$ then goto L09410   /*EWD001 Start*/
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PRICE 002"
            str(readkey$,10%,15%) = pc_r$
            read #2,key = readkey$, using L09400 , desc$, eod goto L09410
L09400:        FMT POS(25), CH(30)
            txt$ = pc_r$ & " - " & desc$
            call "STRING" addr("CT", txt$, 35%)
L09410:     sav_pc_r$ = pc_r$                       /* EWD001 End */

            on phase% goto L09510 , L09770 , L10160, L10490, L10690, L10910

L09510: REM - PHASE (1)
               init(" ") hh1$, hh2$, hh3$, hh4$, hh5$, hh6$,             ~
                         tt1$(), tt2$(), tt3$(), tt4$(), tt5$(), tt6$(), ~
                         dd1$(), dd2$(), dd3$(), dd4$(), dd5$(), dd6$()
               hh1$ = "Level-1 Find Data Def. Catalog/Spec"
               tt1$(1%) = "Cross-Ref PRICE 011:"
                 dd1$(1%) = c_ref$
               tt1$(2%) = "Ref Calc Code-3-3-3:"
                 dd1$(2%) = c_cal$
               tt1$(3%) = "LT%, C-O%, LIT%,Pan:"
                 convert lt% to str(dd1$(3%),1%,2%), pic(00)
                 convert co_or% to str(dd1$(3%),4%,2%), pic(00)
                 convert lit% to str(dd1$(3%),7%,2%), pic(00)
                 str(dd1$(3%),10%,1%) = str(panels$,2%,1%)

               tt1$(4%) = "Key Len, Scan Type :"
                 convert d% to str(dd1$(4%),1%,2%), pic(00)
                 str(dd1$(4%),5%,6%) = "Direct"
                 if no_key% = 0% then str(dd1$(4%),5%,6%) = "Scan >"
               tt1$(5%) = "Count,Spec Calc-010:"
                 convert cnt% to str(dd1$(5%),2%,1%), pic(0)
                 str(dd1$(5%),5%,5%) = "(XXX)"
                 str(dd1$(5%),6%,3%) = cal$
               tt1$(6%) = "Scn Def  :"
                 dd1$(6%) = str(d_key$,1%,d%)
        return
L09770: REM - PHASE (2)
               init(" ") hh2$, hh3$, hh4$, hh5$, hh6$, tt2$(), tt3$(),   ~
                         tt4$(), tt5$(), tt6$(), dd2$(), dd3$(), dd4$(), ~
                         dd5$(), dd6$()
               hh2$ = "Level-2 Load Price Calc Definition "
               tt2$(1%) = "                    "
                   call "APCPR1SB" (4%,0%,pc_kdesc$(1%),tab_desc$,p%,#2, ~
                                                        tab_rec% )
                   tt2$(1%) = str(tab_desc$,1%,19%) & ":"
                   convert pc_kfld%(1%) to xx$, pic(00)
                   call "APCPR1SB" (5%,0%,xx$,tab_desc$,p%,#2,tab_rec%)
                   dd2$(1%) = str(tab_desc$,1%,10%)
               tt2$(2%) = "Key(1) Start-Length:"
                   convert pc_kbeg%(1%) to str(dd2$(2%),1%,2%),pic(00)
                   str(dd2$(2%),5%,1%) = "-"
                   convert pc_klen%(1%) to str(dd2$(2%),9%,2%),pic(00)
               tt2$(3%) = "                    "
                   call "APCPR1SB" (4%,0%,pc_kdesc$(2%),tab_desc$,p%,#2, ~
                                                        tab_rec% )
                   tt2$(3%) = str(tab_desc$,1%,19%) & ":"
                   convert pc_kfld%(2%) to xx$, pic(00)
                   call "APCPR1SB" (5%,0%,xx$,tab_desc$,p%,#2,tab_rec%)
                   dd2$(3%) = str(tab_desc$,1%,10%)
               tt2$(4%) = "Key(2) Start-Length:"
                   convert pc_kbeg%(2%) to str(dd2$(4%),1%,2%),pic(00)
                   str(dd2$(4%),5%,1%) = "-"
                   convert pc_klen%(2%) to str(dd2$(4%),9%,2%),pic(00)
               tt2$(5%) = "                    "
                   call "APCPR1SB" (4%,0%,pc_kdesc$(3%),tab_desc$,p%,#2, ~
                                                        tab_rec% )
                   tt2$(5%) = str(tab_desc$,1%,19%) & ":"
                   convert pc_kfld%(3%) to xx$, pic(00)
                   call "APCPR1SB" (5%,0%,xx$,tab_desc$,p%,#2,tab_rec%)
                   dd2$(5%) = str(tab_desc$,1%,10%)
               tt2$(6%) = "Key(3) Start-Length:"
                   convert pc_kbeg%(3%) to str(dd2$(6%),1%,2%),pic(00)
                   str(dd2$(6%),5%,1%) = "-"
                   convert pc_klen%(3%) to str(dd2$(6%),9%,2%),pic(00)
        return
L10160: REM - PHASE (3)
               init(" ") hh3$, hh4$, hh5$, hh6$, tt3$(), tt4$(), tt5$(), ~
                         tt6$(), dd3$(), dd4$(), dd5$(), dd6$()
               hh3$ = "Level-3 Build Look-Up Scan Key     "
               tt3$(1%) = "Scan Key Lookup-008:"
                   call "APCPR1SB" (8%,0%,pc_kcalc$,tab_desc$,p%,#2,     ~
                                                              tab_rec% )
                   dd3$(1%) = str(tab_desc$,1%,10%)
               tt3$(2%) = "Scn Key Lookup-Type:"
                   dd3$(2%) = "ExactTable"
                   if scan% = 1% then dd3$(2%) = "Min/Max Sn"
               tt3$(3%) = "Code Table Value   :"
                   dd3$(3%) = k_val$
               tt3$(4%) = "Code Table Descript:"
                   dd3$(4%) = k_desc$
               rhh% = 0%
               convert pc_vtbl$ to rhh%, data goto L10330
L10330:
               if k1% = 1% then goto L10430
               if rhh% < 1% or rhh% > 7% then goto L10430
                  call "APCPR1SB" (5%, 0%, pc_vtbl$,tab_desc$,p%,#2,     ~
                                                             tab_rec% )
                  tt3$(3%) = "Code Table Name    :"
                      dd3$(3%) = str(tab_desc$,1%,9%)
                  tt3$(4%) = " "
                  dd3$(4%) = " "

L10430:        tt3$(5%) = "Display Exact Key  :"
                   dd3$(5%) = pc_k$
               tt3$(6%) = "Key Unit Conv Value:"
                   dd3$(6%) = ck_unit$
                   if k2% = 6% then dd3$(6%) = "<Field>"
        return
L10490: REM - PHASE (4)
               init(" ") hh4$, hh5$, hh6$, tt4$(), tt5$(), tt6$(),       ~
                         dd4$(), dd5$(), dd6$()
               hh4$ = "Level-4 Load Link for L_MATCH Scan "
               if scan% = 1% then                                        ~
                  hh4$ = "Level-4 Load Link for L_SCAN Scan  "
               if pass% >= 1% then                          /*EWD001*/   ~
                  hh4$ = "Level-4 Load Link for Bay/Bow Scan "
               tt4$(1%) = "New Scan Key Value :"
                  dd4$(1%) = str(s_key$,1%,15%)
               tt4$(2%) = "1st Link Ref Found :"
               if link% = 2% then tt4$(2%) = "2nd Link Ref Found  "
                  dd4$(2%) = pc_link$
               tt4$(3%) = "Link Ref Desc 1-15 :"
                  dd4$(3%) = str(tab_desc$,1%,15%)
               tt4$(4%) = "Link Ref Desc 16-30:"
                  dd4$(4%) = str(tab_desc$,16%,15%)
               tt4$(5%) = "                    "
               tt4$(6%) = "                    "
        return
L10690: REM - PHASE (5)
               init(" ") hh5$, hh6$, tt5$(), tt6$(), dd5$(), dd6$()
               hh5$ = "Level-5 Scan Key Look-Up L_MATCH   "
               if scan% = 1% then                                        ~
                  hh5$ = "Level-5 Scan Key Look-Up for L_SCAN"
               if pass% >= 1% then                          /*EWD001*/   ~
                  hh5$ = "Level-5 Scan Key Look-Up Bow Scan  "
               tt5$(1%) = "Prim Scan Key 1-25 :"
                  dd5$(1%) = str(p_key$,1%,25%)
               tt5$(2%) = "Min/Max Values Used:"
               if scan% = 0% then tt5$(2%) = "Match Key for LookUp"
                  dd5$(2%) = str(pc_k$,1%,15%)
                  if scan% = 0% then goto L10860
                     str(dd5$(2%),8%,1%) = "/"              /*EWD001*/
                     convert minn to str(dd5$(2%),1%,7%), pic(###.##-)
                     convert maxx to str(dd5$(2%),9%,7%), pic(###.##-)

L10860:        tt5$(3%) = "Price from Look-Up :"
                     convert p1 to dd5$(3%), pic(######.##-)
               tt5$(4%) = "Current Total Price:"
                     convert p to dd5$(4%), pic(######.##-)
        return
L10910: REM - PHASE (6)
               init(" ") hh6$, tt6$(), dd6$()
               hh6$ = "Level-6 Wood Surround/Factory Mull "
               tt6$(1%) = "Wood Code-Equation :"
                  str(dd6$(1%),1%,3%) = str(c_cal$,1%,3%)
                  str(dd6$(1%),6%,1%) = "-"
                  str(dd6$(1%),8%,2%) = ws_no$
               tt6$(2%) = "Factory Mull Charge:"
                  convert wf1 to dd6$(2%), pic(####.####-)
               tt6$(3%) = "Twin/Trpl Deduction:"
                  convert wf2 to dd6$(3%), pic(####.####-)
               tt6$(4%) = "Factory Mull Factor:"
                  convert wf3 to dd6$(4%), pic(####.####-)

        return





