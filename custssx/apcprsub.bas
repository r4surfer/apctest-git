        REM *************************************************************~
            *                  As of (10/12/2016)                       *~
            *   AAA   PPPP    CCC   PPPP   RRRR    SSSS  U   U  BBBB    *~
            *  A   A  P   P  C   C  P   P  R   R  S      U   U  B   B   *~
            *  AAAAA  PPPP   C      PPPP   RRRR    SSS   U   U  BBBB    *~
            *  A   A  P      C   C  P      R  R       S  U   U  B   B   *~
            *  A   A  P       CCC   P      R   R  SSSS    UUU   BBBB    *~
            *                                                           *~
            *         ( Programs ---> APCPRCQT, BCKFASTR, JBPOST1 )     *~
            *           NOTE- (1) Called From Subroutine (APCPR0SB)     *~
            *                 (2) Based on Table PRICE 015 Userid's     *~
            *                     To turn Debug On or Off.              *~
            *                                                           *~
            *         ( Programs ---> Using Subroutine (APCCST0B) which *~
            *                         calls the subroutine (APCPRSUB)   *~
            *                 (1) APCSLS00, APCCST05, APCCST06,         *~
            *                     APCCST08, APCCST09, APCIVAVG.         *~
            *-----------------------------------------------------------*~
            * APCPRSUB - Build the price for the Specified Part using   *~
            *            the Appropriate Routine and the Calc Informat- *~
            *            ion found in the (APCPCMST) File. Also use this*~
            *            File to Calculate The Standard Deductions.     *~
            *            The Following GENCODES Tables are used -       *~
            *                (PRICEPCNT)   Special Price Codes          *~
            *                (PRICECUST)   Special Customer Catalogs    *~
            *                (PRICE 000 thru PRICE 011) Pricing Tables  *~
            *                (PRICE 015) - Debug Security for Testing   *~
            *                (PRICE 016) - Glass Promotion Table        *~
            *                (PRICE 018) - Customer Promotion Table     *~
            *                (PRICE 019) - Product Promotion Charge Tab *~
            *                                                           *~
            * NOTE - (1) ERR% = 0% Part and Catalog Price               *~
            *            ERR% = 1% Part Catalog and Special Price       *~
            *            ERR% = 2% EDI Price                            *~
            *            ERR% = 3% No Price Error Catalog               *~
            *            ERR% = 4% Catalog, Error Special Price         *~
            *            ERR% = 5% Invalid Part Price                   *~
            *            ERR% = 6% Invalid Part Build                   *~
            *                                                           *~
            * 1st - Test For Parts and Component Part                   *~
            *         ERR% = 5% and No Price Is Calculated.             *~
            *                                                           *~
            * 2nd - (STANDARD_DEDUCTION-APCPR5SB) Calulate Std Deduction*~
            *         PART$   - Always is Exact Size After Sub Call     *~
            *         PART_O$ - Always is Opening Size After Sub Call   *~
            *                                                           *~
            *         NOTE (1)- Exact Size is Always Passed (OUT) When  *~
            *                   Subroutine is Finished with Part.       *~
            *              (2)- If Customer has a Special Catalog the   *~
            *                   Flag SP% will be Set to ( 1% ).         *~
            *                                                           *~
            * 3rd - (SET_UP_DEFINITION) for APC Catalog and Calculate   *~
            *       the Price using Subroutine (APCPR2SB) Primary Sub.  *~
            *       Save Ref Type Codes in Priority Sequence            *~
            *       ( SEQ%() ). Save the Accumulative Price Calc in     *~
            *       ( REF_P() ). 'CALCULATE_PRICE' also DISPLAY_DEBUG   *~
            *                                                           *~
            * 4th - (CHECK_EDI-APCPR4SB) See if Customer has Special EDI*~
            *       Price Defined in the APCSKUNO File. If Special EDI  *~
            *       Price found then ( SP% = 2% ) Set Prices for Catalog*~
            *       and Exit the Pricing Routine.                       *~
            *                                                           *~
            * 5th - (SET_UP_DEFINITION) for any Customer Special Price. *~
            *       Calculate the Price using Subroutine (APCPR2SB).    *~
            *       Save Ref Type Codes in Priority Sequence ( SEQ%() ).*~
            *       Save the Accumulative Price Calc in ( REF_P1() ).   *~
            *       'CALCULATE_PRICE' also DISPLAY_DEBUG                *~
            *       Note - No Definition then APC Catalog Used          *~
            *                                                           *~
            * 6th - (CREATE_PRICES) Create discounted Prices (A thru Z) *~
            *       and (0 thru 9). "Q" = Has Special Meaning.          *~
            *       Create is based on Tabel (PRICEPCNT)                *~
            *                                                           *~
            * 7th - (UPDATE_PRICES) Update Customer Price Code File     *~
            *       (CPRPRICE) Only Catalog Price with Price Discounts  *~
            *                  Applied.( A thru Z ) and ( 0 thru 9 )    *~
            *       NOTE - Only when UPD$ = "Y" are Prices Updated      *~
            *                                                           *~
            * 8th - (CHECK_PROMO) Check out both Customer and Beg/End   *~
            *                     Dates 1st. If criteria met continue   *~
            *                     with check for product. If all tests  *~
            *                     are satisified then Add or Subtract   *~
            *                     the appropriate Charge from the       *~
            *                     Calculated price. Either Discounted   *~
            *                     or Special Catalog. (EWD004)          *~
            *                                                           *~
            * 9th - (EXIT_PROGRAM) The Below Variables are Set          *~
            *     - P   -----> Always APC Catalog Price                 *~
            *     - P1  -----> Special Customer Price                   *~
            *       SP% -----> (0%) APC Catalog Price                   *~
            *                  (1%) Special Catalog Price in (P1)       *~
            *                  (2%) Special Customer EDI Price          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/31/94 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 03/31/97 ! Special Mods for Pricing Changes and the ! RHH *~
            *          !   new Unix Box. (HP 9000)                !     *~
            * 05/13/97 ! Special Mod to add the Customer Code to  ! RHH *~
            *          !   the Subroutine call (APCPR2SB)         !     *~
            * 07/28/97 ! Mod - To allow for Special Sales         ! RHH *~
            *          !   Promotions. New Sub 'CHECK_PROMO'      !     *~
            * 10/31/97 ! Check for Upgrade to R6.04.03            ! RHH *~
            * 05/15/98 ! Additional Pricing Capabilities          ! ERN *~
            *          !  - Wood Surround                         !     *~
            *          !  - Factory Mull                          !     *~
            *          !  - Reynolds Plus Option Package          !     *~
            *          !  - Screen Only Pricing (fix)             !     *~
            *          !  - Wood Laminate and Brass Locks         !     *~
            * 09/22/98 ! Don't do Twn/Trpl UpChg if WS/FM. EWD000 ! BWS *~
            * 09/22/98 ! Mods to debug screen; bug fixes.  EWD001 ! BWS *~
            *          !   Add pricing logic for non-item parts.  !     *~
            * 12/18/98 ! Bug fix to debug for promo pricing.EWD002! BWS *~
            * 03/02/99 ! (EWD003) - Mod for Brass Grid (In BWS)   ! BWS *~
            *          !   version not implemented.               !     *~
            * 05/27/99 ! (EWD004) - Mod for promotion change to   ! RHH *~
            *          !   support New Grid Promotion. Valid      !     *~
            *          !   promotions are (1),(2),(4) - No (3)    !     *~
            * 06/04/99 ! Note - These changes need to be put into ! RHH *~
            *          !        Brian Sanders version in BWSDEV   !     *~
            * 06/24/99 ! (EWD005) - Mod to support Customer's who ! RHH *~
            *          !   are on the grid promotion but not on   !     *~
            *          !   promotion (2) for 10% discount,        !     *~
            * 11/01/99 ! (EWD006) - Mods for New Wood Jamb Pricing! RHH *~
            * 01/20/00 ! (EWD007) - Mods for Samp/Disp/Liter      ! RHH *~
            *          !   for literature all Alfa for Width,     !     *~
            *          !   Height and Center line meeting rail    !     *~
            *          !   make exact and opening the same        !     *~
            * 03/17/00 ! (EWD008) - Mod to force pricing to always! RHH *~
            *          !   use exact size                         !     *~
            * 04/17/00 !   Continue Exact Size mod for only New   ! RHH *~
            *          !   Consturction, Hoppers, and Patio Doors !     *~
            * 07/18/00 ! (EWD009) - Mod to fix Coastal Mull       ! RHH *~
            *          !   Pricing.                               !     *~
            * 04/02/01 ! (EWD010) - Mod to Take out WS Code       ! CMG *~
            *          !           "SW - EWD006"                  !     *~
            * 01/11/02 ! (EWD011) - Mod for Samp/Disp 'PRICE 023' ! CMG *~
            *          !            table.  Change from Samp/Disp !     *~
            *          !            code to Samp/Disp/Model Code. !     *~
            * 02/05/02 ! (EWD012) - Mod to turn off Special Sku   ! CMG *~
            *          !            pricing.                      !     *~
            * 09/06/02 ! (EWD013) - Mod to add logic for warranty ! CMG *~
            *          !            pricing.                      !     *~
            * 09/17/02 ! (EWD014) Fix for Special Shapes Grid Code! CMG *~
            * 07/09/04 ! (EWD015) do prc=22 if wood and cont head ! CMG *~
            * 10/25/05 ! (AWD016) CR347 Mods for new sub part     ! CMG *~
            * 06/26/06 ! (AWD017) mods for special mull codes     ! CMG *~
            *02/25/2008! (AWD018) mods for SDL                    ! DES *~
            *07/10/2012! (AWD019) mod to painted doors            ! CMG *~
            *04/22/2013! (AWD020) 5/0 Sash option                 ! CMG *~
            *03/31/2014! (AWD021) Added price model and color tabl! CMG *~
            *          !   overrides AWD019 change                !     *~
            *07/29/2014! (AWD022) mod for uchannel and mull clp   ! CMG *~
            *12/22/2015! (SR67154) mods for NFRC 2016             ! CMG *~
            *12/22/2015! (SR71583) mods for TriplePlus            ! CMG *~
            *04/21/2016! (SR74362) mods for lowes GBW price       ! CMG *~
            *10/21/2106! (CR614) turn off skipping SuperSpacer    ! CMG *~
            *03/04/2020! (CR2451) 3900 PSE Price                  ! CMN *~            
            *03/04/2020! (CR2488) 3900 PSE Price GBW              ! CMN *~            
            *************************************************************

        sub "APCPRSUB"   (part$,         /* Part Number for Price      */~
                          subpart$,      /* (AWD016) Part Number 1     */~
                          size$,         /* (O)pening, (E)xact         */~
                          pc(),          /* Calc Dealer Price Catalog  */~
                          p1,            /* Special Customer Price     */~
                          sp%,           /* Special Price Code 0,1     */~
                          upd$,          /* Update Price Sheets Y or N */~
                          cuscode$,      /* Customer Code              */~
                          err%,          /* Error Return Codes         */~
                          ref$(),        /* Ref. Type Codes Catalog    */~
                          ref1$(),       /* Ref. Type Codes Spec Cat.  */~
                          ref_p(),       /* Ref Prices APC Catalog     */~
                          ref_p1(),      /* Ref Prices Special Cat.    */~
                          sash5_0$,      /* (AWD020) 5/0 Sash option   */~
                          model$,        /* Model                      */~
                          tpp$,          /* TriplePanePackage (SR67154)*/~
                          nfrc$,         /* NRFC 2016 req     (SR71583)*/~
                          forcedfoam$,   /* Forced Foam                */~
                          #1,            /* Channel of (APCPCMST) File */~
                          #8,            /* Channel of (AWDPCMST) File */~
                          #6,            /* Channel of (APCPCMSK) File */~
                          #7,            /* Channel of (APCPCMSD) File */~
                          #2,            /* Channel of (GENCODES) File */~
                          #3,            /* Channel of (CPRPRICE) File */~
                          #4,            /* Channel of (CUSTOMER) File */~
                          #5 )           /* Channel of (APCSKUNO) File */


        dim                                                              ~
            msg$32, txt$(40%)34,         /* Debug Message              */~
            lit_flg$1,                   /* Lit Flag (Y) or (N)(EWD007)*/~
            ss$3,                        /* Sample/Display Code        */~
            model$16,                    /* Virtual Model              */~
            tpp$2,                       /* Triple Pane Packag(SR67154)*/~
            nfrc$2,                      /* NFRC 2016 required(SR71583)*/~
            forcedfoam$2,                /* Forced Foam 2018           */~
            series$16,                   /* Series            (SR71583)*/~
            style$10,                    /* Style Code        (SR71583)*/~
            part$25,                     /* Part NO.(Always Exact Size)*/~
            part_o$25, p$25,             /* Part NO.(Always Opening Sz)*/~
            subpart$20,                  /* Part Number 1    (AWD016)  */~
            gls$2,                       /* (SR71583) Northern NFRC    */~
            liting$2,                    /* (SR71583) Northern NFRC    */~
            foam$1,                      /* (SR71583) Northern NFRC    */~
            northernFoam$1,              /* (SR71583) Northern NFRC    */~
            size$1,                      /* (O)pening, (E)xact         */~
            pc(36%),                     /* Prices (A - Z)(0 - 9)      */~
            code$36,                     /* Price Code TABLE           */~
            dpcnt$5,                     /* Multiplier Pcnt - PRICEPCNT*/~
            readkey$24, desc$30,         /* Gencodes Lookup Key        */~
            upd$1,                       /* Update Price Sheet Flag    */~
            cuscode$9,                   /* Customer Code              */~
            inp$79,                      /* Debug Screen Prompt        */~
            cursor%(2%),                 /* Cursor Locations           */~
            date$8,                      /* Current Date               */~
            i$(24%)80,                   /* Screen Image               */~
            pfkeys$32,                   /*                            */~
            userid$3, debug%(2%)         /* User Id                    */

        dim                                                              ~
            pp$10, ee$1, sav_p1$10,      /* Accum Price, Err Code,Ref P*/~
            pc_key$53,                   /* Pricing Generic Key        */~
            pc_c$4, spc_c$4,             /* Price Catalog Code         */~
            pc_cm$2, spc_cm$2,           /* Price Catalog Calc Code    */~
            pc_m$16,                     /* Part Model Code            */~
            x_c$4, x_cm$2,               /* Use for Set-Up             */~
/*AWD016*/  seq%(60%),                   /* Save Priority Seq of Ref's */~
            pc_r$2,                      /* Reference Type Code        */~
/*EWD001*/  pc_rc$3,                     /* Price Ref. Calc Method     */~
/*AWD016*/  ref$(30%)2,                  /* Ref. Type Codes APC Catalog*/~
/*AWD016*/  ref1$(30%)2,                 /* Ref. Type Codes Spec Cat.  */~
/*AWD016*/  ref_p(30%),                  /* APC Calalog Prices         */~
/*AWD016*/  ref_p1(30%),                 /* Special Catalog Prices     */~
            pc_std%(20%),                /* Std Calc Priority Seq.     */~
            pc_stdc$(20%)1,              /* Std Price Cross-Ref.       */~
            pc_stdr$(20%)9,              /* Std Ref Calc Codes         */~
            pc_spc%(20%),                /* Spc Calc Priority Seq.     */~
            pc_spcc$(20%)1,              /* Spc Price Cross-Ref.       */~
            pc_spcr$(20%)9,              /* Spc Ref Calc Codes         */~
/*AWD016*/  pc_sub%(20%),                /* Sub Part Calc Priority Seq */~
/*AWD016*/  pc_subc$(20%)1,              /* Sub Part Price Cross-Ref.  */~
/*AWD016*/  pc_subr$(20%)9,              /* Sub Part Ref Calc Codes    */~
            c_ref$1,                     /* Cross-Ref Code             */~
            c_cal$9, errormsg$35         /* Cross-Ref Calc Codes       */

        dim                                                              ~
            dte1$8, promo_flag$1,        /* Beginning Promotion Date   */~
            dte2$8, promo_no$1,          /* Ending Promotion Date      */~
            dte3$6,                      /* Current System Date        */~
            pcnt_code$1, descr$30,       /* (EWD004) Use for Spec Cat  */~
/*EWD001*/  promo_text$20                /* Promotional Discount Descr.*/

        dim                              /* Wood Surround/Factory Mull */~
            apcwood_code$3,              /* Extracted APC WOOD         */~
            apcwood_descr$32,            /* Read description APC WOOD  */~
            wsfm_code$30,                /* Code from descr            */~
            wsfm_descr$30,               /* Descr from descr           */~
            wsfm_instruction$2,          /* Mfg instruction            */~
            wsfm_nbr_windows$1,          /* Number of windows (1-3)    */~
            wsfm_nbr_model$1,            /* Nbr windows for pricing 1-3*/~
            top_window$1,                /* Top window type            */~
            top_nbr_windows$1,           /* Number of windows on top   */~
            wsfm_ret$50,                 /* Text version of ret code   */~
            gc$32                        /* GENCODES descriptor        */

        dim sav_p$25, neu_cl$2

        dim logmsg$256

        dim sash5_0$2

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Special Pricing - Debug Screen-11/01/99)"
            pname$ = "APCPRSUB - Rev: 01.00"

        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPCMST ! Pricing Master Definition File           *~
            * #2  ! GENCODES ! System Code Table File                   *~
            * #3  ! CPRPRICE ! Customer Price Code File                 *~
            * #4  ! CUSTOMER ! Customer Master File                     *~
            * #5  ! APCSKUNO ! Master Sku Number File                   *~
            * #6  ! APCPCMSK ! Pricing (Key) and (Value) Definition File*~
            * #7  ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #8  ! AWDPCMST ! Pricing Master Definition File           *~
            *************************************************************~

        REM *************************************************************~
            *     M a i n   L i n e   P r o g r a m   P r o c e s s     *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)          /* Who Did It  */
/* (AWD020) */
            sash5_0%, tpp%, nfrc%, prcerr%, ffoam% = 0%
            convert sash5_0$ to sash5_0%, data goto badsash5_0

badsash5_0:
/* (SR71583) */
            convert tpp$ to tpp%, data goto badtpp

badtpp:
/* (SR67154) */
            convert nfrc$ to nfrc%, data goto badnfrc

badnfrc:

            convert forcedfoam$ to ffoam%, data goto badFFoam

badFFoam:
            init(" ") foam$, northernFoam$, series$, style$, liting$, gls$
            northernFoam$ = "0"
            foam$ = str(subpart$,5%,1%)
            if foam$ = "3" then northernFoam$ = "1"
            if foam$ = "4" then northernFoam$ = "1"
            call "PRCSERST" (str(part$,1%,3%), series$, style$, #2, prcerr%)

            gls$    = str(part$,5%,2%)
            liting$ = str(part$,7%,2%)
/* (\SR67154) */
            u3% = 0% : j% = 0% : j1% = 0% : calc% = 0% : sp% = 0%
            p   = 0.0 : p1 = 0.0 : kk% = 0%
            mat pc = zer    : mat debug% = zer
            mat ref_p = zer : mat ref_p1 = zer

            init(" ") ref$(), ref1$(), spc_c$, spc_cm$, pc_m$, txt$(),   ~
                promo_text$ /*EWD001*/

            if str(subpart$,1%,20%) = " " then                     ~
                  str(subpart$,1%,20%) = "00000000000000000000"

            gosub check_debug
            ee%, err% = 0%
                                                        /* (EWD007)    */
            lit_flg$ = "N"
            if str(part$,1%,3%) = "003" then lit_flg$ = "Y"
                                                        /* (EWD007)    */
/*(AWD019)*/
REM            PAINTED$ = "I,J,K,L,M,N,O,P"
REM            GOSUB CHECK_PAINTED
             gosub check_pricemdlc


        REM ---------------------------------------
        REM - 1st. Check for Parts and Components -
        REM ---------------------------------------
            if len(part$) > 18 then goto L02000          /* (EWD007)   */
               gosub check_virtual              /* (CR2488)) */               
               gosub check_for_nonitem          /* (EWD001)  */
               if p + p1 <> 0 then goto L02410  /* (EWD001)  */
                  if str(part$,5%,4%) = "WARR" then goto L02410
                  if str(part$,5%,5%) = "LABOR" then goto L02410
                  err% = 5%                     /* Product is a Part, No  */
                  goto exit_program             /* Price Calculation Done */

        REM ---------------------------------------
        REM - 1A. Check for Samples and Displays  -
        REM ---------------------------------------
L02000:     gosub check_sample_display
            if ss% = 0% then goto L02150
            if p + p1 <> 0 then goto L02410              /* (EWD007)   */

        REM ------------------------------------
        REM - 2nd. (F) = No Standard Deduction -
        REM ------------------------------------
L02150:     if size$ <> "F" then goto L02170               /* (EWD007) */
               part_o$ = part$               /* Calculate Price using  */
L02170:     gosub standard_deduction         /* the Exact Size         */
            p$ = part_o$                     /* Default (O)pening Size */
                                             /* (EWD008)               */
            gosub check_method
            if method% <> 0% then p$ = part$
                                             /* (EWD008)Price using Exact*/
        REM -------------------------------------------------------------
        REM - 3rd. Always Calculate the APC Catalog Price 1st (Costing) -
        REM -------------------------------------------------------------
            x_c$  = pc_c$
            x_cm$ = pc_cm$
            gosub set_up_definition
            if def% = 0% then goto L02310      /* No Definition Found    */
REM            GOSUB CHECK_VIRTUAL
               pp = 0.0
               gosub calculate_price
               if ee% = 0% then err% = 0% else err% = 3%
               if ee% <> 0% then goto exit_program

        REM ------------------------------------------------------
        REM - 4th. After APC Catalog Price - Check for EDI Price -
        REM ------------------------------------------------------
            gosub check_edi
            if sp% = 2% then goto L02410       /* Done Create Prices      */

        REM ---------------------------------------------------------------
L02310: REM - 5th. Do Special Catalog Price Calc for Customer When Applic -
        REM ---------------------------------------------------------------
            if sp% = 0% then goto L02410     /* No Special Customer Price */
               x_c$  = spc_c$
               x_cm$ = spc_cm$
               gosub set_up_definition
               if def% = 0% then goto L02410
                  pp = 0.0 : calc% = 1%    /* Set Flag for Special Price*/
                  gosub calculate_price    /* Catalog. Definition Exists*/
                  if ee% = 0% then err% = 1% else err% = 4%
                  if ee% <> 0% then goto exit_program

        REM ------------------------------------------------------------
L02410: REM - 6th. Using (PRICEPCNT) Build Prices (A thru Z, 0 thru 9) -
        REM ------------------------------------------------------------
            gosub create_prices

        REM -----------------------------------------------------------
        REM - 7th. When Applic Update Prices (APCPRCQT) or (BCKFASTR) -
        REM -----------------------------------------------------------
            gosub update_prices


            goto exit_program

        REM *************************************************************~
            *       E n d   o f   M a i n   L i n e   P r o g r a m     *~
            *************************************************************
                                                    /* (EWD008)       */
            check_method
               method% = 0%
                                                   /* All Hoppers     */
               if str(part$,1%,3%) = "870" then goto L02450
                                                   /* All Patio Doors */
               if str(part$,1%,1%) = "3" then goto L02450
               init(" ") readkey$, desc$
               str(readkey$,1%,9%)   = "PLAN NEWC"
               str(readkey$,10%,15%) = str(part$,1%,3%)
               read #2,key = readkey$, eod goto L02460
                                                    /* All New Constr */
L02450:           method% = 1%
L02460:     return
                                                    /* (EWD008)       */

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* CPRPRICE - Customer Price  */
L02550:     FMT CH(01),                  /* PRICE CODE - 'C' STANDARD K*/~
                CH(25),                  /* Part Number               K*/~
                CH(21),                  /* Filler                    K*/~
                CH(03),                  /* Last Changed By - User     */~
                CH(06),                  /* Date Last Modified         */~
                36*PD(14,4),             /* APC - (35) Price Codes     */~
                CH(200),                 /* Filler 1                   */~
                CH(156)                  /* Filler 2                   */
                                         /* (APCPCMSD) - CALC DEF FILE */
            FMT CH(04),                  /* Catalog Code               */~
                CH(02),                  /* Catalog Method Code       K*/~
                CH(03),                  /* Model Code                K*/~
                20*BI(1),                /* Std Calc Priority Seq      */~
                20*CH(01),               /* Std Price Cross Ref Code   */~
                20*CH(09),               /* Std Ref Calc Codes 3,3,3   */~
                20*BI(1),                /* Spc Calc Priority Seq      */~
                20*CH(01),               /* Spc Price Cross Ref Code   */~
                20*CH(09),               /* Spc Ref Calc Codes 3,3,3   */~
/*AWD016*/      20*BI(1),                /* Sub Part Calc Priority Seq */~
/*AWD016*/      20*CH(01),               /* Sub Part Price Cross Ref Cde*/~
/*AWD016*/      20*CH(09),               /* Sub Part Ref Calc Codes 3,3,3*/~
/*AWD016*/      CH(99)                   /* Filler Area                */

        REM *************************************************************~
            *               B U I L D   P R I C E   S H E E T S         *~
            *************************************************************

        create_prices                   /* For Price Codes - (A thru Z)*/
            p1 = round( p1, 2)          /* and (0 thru 9). Exclude (Q) */
            do_before% = 1%             /* Grid Done before Discount   */
            gosub check_promo           /* (EWD004) - Do 1st           */

            if p < .01 then goto L02970   /* CODE$ = BIN(I% + 64%, 1%) */
               code$  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
               init(" ") readkey$, dpcnt$
               str(readkey$,1%,9%) = "PRICEPCNT"
               for i% = 1% to 36%
                   dpcnt$ = "0.0  "
                   str(readkey$,10%,15%) = str(code$,i%,1%)
                   read #2,key = readkey$, using  L02900, dpcnt$,          ~
                                                            eod goto L02910
L02900:               FMT POS(25), CH(5)
L02910:            convert dpcnt$ to x, data goto L02920
L02920:
                   pc(i%) = round( x * p, 2)
               next i%
            pc(17%) = -1                /* Set for 'Q' Requires that    */
                                        /* the price be Manually Entered*/
L02970:     do_before% = 0%             /* (EWD004) Do Not do grid      */
            gosub check_promo
            if promo_text$ <> " " and debug%(1%) <> 0%  /*EWD002*/       ~
                then gosub display_debug    /*EWD001*/
        return

        REM *************************************************************~
            *        U P D A T E   P R I C E S   F O R   P A R T        *~
            *************************************************************

        update_prices
            if p < .01 then return              /* No Customer Price   */
               if upd$ = "N" then return        /* Must be From Quote  */

            pc_key$ = " "
            pc_key$ = "C" & part$
            read #3,hold,key = pc_key$, eod goto L03140
            rewrite #3,using L02550 , "C", part$, " ", userid$, date, pc(),~
                " ", " "           /* Update - Record */
        return
L03140:     write #3,using L02550 , "C", part$, " ", userid$, date, pc(),  ~
                                " ", " "           /* Create - Record */
        return

        REM *************************************************************~
            *          S t a n d a r d   D e d u c t i o n              *~
            *************************************************************

        standard_deduction
                                          /* (EWD007) - Literature Chg */
            if lit_flg$ = "N" then goto L03200
               part_o$ = part$
               goto L03210
                                          /* (EWD007) - Not Applicable */
L03200:     call "APCPR5SB" (size$,       /* (E)xact or (O)pening      */~
                             part$,       /* MFG Part Always Exact     */~
                             part_o$,     /* MFG Part Always Opening   */~
                             cuscode$,    /* Customer Code             */~
                             sp%,         /* Special Price Flag        */~
                             pc_c$,       /* Price Catalog Code        */~
                             pc_cm$,      /* Price Catalog Calc Code   */~
                             #8,          /* (APCPCMST) - File         */~
                             #2,          /* (GENCODES) - File         */~
                             err% )       /* Error Non Zero Value      */

L03210:     
            gosub check_virtual           /* (CR2451)                  */
REM            PC_M$ = STR(PART$,1%,3%)      /* MODEL CODE   (CR2451)  */
            if sp% = 0% then return       /* APC Catalog Returned      */
               spc_c$  = pc_c$            /* Save Spec Catalog         */
               spc_cm$ = pc_cm$           /* Save Spec Catalog Calc Cod*/
               pc_c$   = "0000"           /* Re-Set APC Catalog and    */
               pc_cm$  = "00"             /* Calc Code                 */
        return


        REM *************************************************************~
            *          C a l c u l a t e   P r i c e                    *~
            *************************************************************

        check_edi
            return                        /* 1st Check for EDI Customer*/
            call "APCPR4SB" ( part$,      /* MFG Part Number Exact Size*/~
                              cuscode$,   /* Customer Code             */~
                              p1,         /* Special Customer Price    */~
                              #4,         /* (CUSTOMER) - File         */~
                              #5,         /* (APCSKUNO) - File         */~
                              pc_p% )     /* 0% = No EDI Price         */
            if pc_p% = 0% then goto L03560
               sp% = 2%                   /* Special EDI Price         */
L03560: return

        set_up_definition               /* PC_STD%()=STD Priority Seq  */
            def% = 0%                   /* PC_STDC$()=STD Prc Cross-Ref*/
            mat seq% = zer              /* PC_STDR$()=STD Ref Calc Code*/
            pc_key$ = " "               /* PC_SPC%()=SPC Priority Seq  */
                                        /* PC_SPCC$()=SPC Prc Cross-Ref*/
                                        /* PC_SPCR$()=SPC Ref Calc Code*/
            str(pc_key$,1%,4%) = x_c$   /* STD or SPC  Catalog Code    */
            str(pc_key$,5%,2%) = x_cm$  /* STD or SPC Catalog Calc Code*/
            str(pc_key$,7%,16%)= pc_m$  /*           Pricing Model Code*/
            read #7,key = pc_key$, using  L03700, pc_std%(), pc_stdc$(),   ~
                          pc_stdr$(), pc_spc%(), pc_spcc$(), pc_spcr$(), ~
                          pc_sub%(), pc_subc$(), pc_subr$(),  /*AWD016*/ ~
                          eod goto L03820
L03700:         FMT POS(23), 20*BI(1), 20*CH(1), 20*CH(9),               ~
                             20*BI(1), 20*CH(1), 20*CH(9), 20*BI(1),     ~
                             20*CH(1), 20*CH(9)
                                        /* I% = (1  - 20) Std Ref Calcs*/
                                        /*    = (21 - 40) Spc Ref Calcs*/
                                        /* X% = Priority of Calc When  */
            def% = 1%                   /*      Priority Seq Exists    */
            for i% = 1% to 20%
                x% = pc_std%(i%)        /* Standard Calc's Priority Seq*/
                y% = pc_spc%(i%)        /* Special Calc's Priority Seq */
/*AWD016*/      z% = pc_sub%(i%)        /* Sub Part Calc's Priority Seq */
                if x% <> 0% then seq%(x%) = i%       /* Store the Calc */
                if y% <> 0% then seq%(y%) = i% + 20% /* Ref Code in    */
/*AWD016*/      if z% <> 0% then seq%(z%) = i% + 40% /* SUB PART       */
            next i%                                  /* SEQ%() = PC_R$ */
L03820: return

    REM -------------------------------------
        check_virtual
REM          IF LEN(MODEL$) <= 3% THEN RETURN
          if len(model$) <= 3% then goto notVirtual

          init(" ") readkey$
          readkey$ = "VIRTUALMD" & str(model$,1%,15%)
          read #2, key = readkey$, eod goto notVirtual

            pc_m$ = model$            /* (CR2451) */

        return
        notVirtual
          pc_m$, model$ = str(part$,1%,3%)
        return
    REM -------------------------------------

    REM -------------------------------------
        calculate_price
    REM -------------------------------------
            sav_p = 0.0 : sav_p1 = 0.0             /* Using the Defined  */

/*(AWD019)*/
REM            GOSUB CHECK_DOOR
            gosub check_for_wsfm                   /* Check if wood surr */ /* WSFM */
            if wsfm_ret% < 2% then L03900
                ee% = 1%
                return
L03900:     wood_surround% = 0%
                                                   /* (EWD006)             */
                                                   /* (EWD009)             */
        REM    IF WSFM_INSTRUCTION$ = "SW" OR WSFM_INSTRUCTION$ = "CW"          ~
        REM                                            THEN WOOD_SURROUND% = 1%
                                                   /* (EWD006)             */
            if wood_surround% = 1% and wsfm_nbr_windows% > 3% then ee% = 1%
            if wood_surround% = 1% and wsfm_nbr_windows% > 3% then return
/*AWD016*/
            for i% = 1% to 60%                     /* Priority Sequence  */
                if seq%(i%) = 0% then goto L04130  /* Calc Ref N/A       */
                convert seq%(i%) to pc_r$, pic(00) /* Load Calc Ref Code */

                if seq%(i%) > 20% then goto L03950
                   c_ref$ = pc_stdc$(seq%(i%))   /* Std Cross-Ref Code */
                   c_cal$ = pc_stdr$(seq%(i%))   /* Std Ref Calc Codes */
                   goto L03980                   /* XXX, YYY, ZZZ      */

L03950:         if seq%(i%) > 40% then goto L03970

                c_ref$ = pc_spcc$(seq%(i%)-20%)  /* Spc Cross-Ref Code */
                c_cal$ = pc_spcr$(seq%(i%)-20%)  /* Spc Ref Calc Codes */
                                                 /* XXX, YYY, ZZZ      */
                goto L03980

L03970:                                          /* (AWD016)           */
                c_ref$ = pc_subc$(seq%(i%)-40%)  /* Spc Cross-Ref Code */
                c_cal$ = pc_subr$(seq%(i%)-40%)  /* Spc Ref Calc Codes */
                                                 /* XXX, YYY, ZZZ      */

L03980:         gosub calc_price

                if ee% <> 0% then return

                if calc% = 1% then goto L04050
                   j% = j% + 1%         /* Save APC Catalog Ref Calc   */
                   ref$(j%) = pc_r$
                   goto L04090
L04050:         j1% = j1% + 1%          /* Save Spec. Catalog Ref Calc */
                ref1$(j1%) = pc_r$
                                        /* Save Calculated Prices      */
                                        /* 0% = Catalog, 1% = Special  */
L04090:         if calc% = 0% then ref_p(j%) = pp else ref_p1(j1%) = pp
                                        /* Test for Window Price, If   */
                                        /* found Price Complete.       */
                if str(c_cal$,7%,3%) = "999" then goto L04140
L04130:     next i%

L04140:     if calc% = 0% then p = pp else p1 = pp
            if calc% <> 0% then goto L04180
               p = pp
               return

L04180:    REM call "SHOSTAT" ("Calculating Special Catalog Price")  stop
            for prc% = 1% to 30%                 /*(AWD016)*/
                if ref_p1(prc%) <> 0 and prc% <> 30% then goto L04190
                   if prc% = 30% then goto calc_30
REM                   CALL "SHOSTAT" ("REF1$ =  " & REF1$(PRC% - 1%))  STOP

                   if ref1$(prc% - 1%) = "27" or ref1$(prc% - 1%) = "28" then ~
                      ref_p(30%) = ref_p1(prc% - 2%) else                     ~
                      ref_p(30%) = ref_p1(prc% - 1%)

                   if ref1$(prc% - 2%) = "27" or ref1$(prc% - 2%) = "28" then ~
                      ref_p(30%) = ref_p1(prc% - 3%)


                      convert ref_p(30%) to cmg$, pic(########0.00)

REM                      CALL "SHOSTAT" (" P1 =  " & CMG$)   STOP
                      goto finished

                      calc_30
REM                          CALL "SHOSTAT" ("REF1$ =  " & REF1$(PRC%))  STOP

                          if ref_p1(prc%) = 0.00 then prc% = prc% - 1%
                          if ref1$(prc%) = "27" or ref1$(prc%) = "28" then ~
                              ref_p(30%) = ref_p1(prc% - 1)

                          if ref1$(prc%) = "27" or ref1$(prc%) = "28" then ~
                              ref_p(30%) = ref_p1(prc% - 2)
                      convert ref_p(30%) to cmg$, pic(########0.00)
REM                      call "SHOSTAT" (" P1 =  " & cmg$)   stop
                      goto finished

L04190:     next prc%

finished:
        return

REM     --------------------
        calc_price
REM     --------------------
            in% = 0%
            sav_p  = pp                 /* Save Previous As Of Price   */
            sav_p1 = 0.0                /* Init Calc Price Before      */
            woodClips% = 0%

/* ALTER GRID C_R AND RFX CODES IF COLOR INDICATES BRASS PACKAGE */
/* CHANGE TO TURN OFF */
/* CR347 DO NOT PRICE 04 - LITING - IF BRASS GRIDS */
/* DO NOT RUN 46 IF WINDOW HAS NO GRIDS OR IS A SHAPE WITH NO GRIDS */
REM         LOGMSG$ = "== " & PC_R$ & "  " & PART$ & "  " & SUBPART$
REM         CALL "LOGFILE" (LOGMSG$)
/* DO RUN 04 IF SHAPE */
/* AWD018 */ REM IF PC_R$ = "04" AND STR(SUBPART$,8%,1%) = "1"         ~
                               AND STR(P$,7,1) < "A" THEN RETURN

             if pc_r$ = "04" and str(subpart$,8%,2%) = "11"         ~
                               and str(p$,7,1) < "A" then return
/* DO NOT RUN 31 OR 29 IF SHAPE */
REM IF (PC_R$ = "31" OR PC_R$ = "29") AND STR(SUBPART$,8%,1%) = "1" AND STR(P$,7,1) >= "A" THEN RETURN
/* (AWD020) */
/* SPC COTTAGE/ORIEL REF */
            if (pc_r$ = "21" and sash5_0% <> 0%) then return
/* SPC UPCHARGE PRICE & SPC UPCHARGE PRICE(3)*/
            if (pc_r$ = "31" or pc_r$ = "29") and                  ~
/* 8%,2% = SDL */              str(subpart$,8%,2%) = "11" and       ~
                               str(p$,7,1) >= "A" then return
            if pc_r$ = "46" and liting$ = "00" then return
            if pc_r$ = "46" and liting$ > "99" and         ~
               str(part$,8%,1%) = "0" and liting$ <> "V0" then return
            if pc_r$ = "46" and str(subpart$,1%,2%) = "00" then return

/* AWD027 */ /*IF PC_R$ = "04" AND STR(SUBPART$,1%,1%) = "1" THEN RETURN*/
            if pc_r$ = "46" and str(subpart$,1%,2%) = "11" then return
            if pc_r$ = "46" and str(subpart$,1%,2%) = "12" then return
            if pc_r$ = "44" and str(subpart$,4%,1%) = "0" then return

REM SUBPART,1% = GRIDTYPE -- "3" = BRASS
            if pc_r$ = "04" and str(subpart$,1%,1%) = "3" and              ~
                            str(p$,1%,1%) <> "3" then return
            goto L04142


/* CR347 END */
            if pc_r$ = "04" and str(p$,4%,1%) >= "C" and                  ~
                                str(p$,4%,1%) <= "F" then goto L04141     ~
                                        /*EWD001*/   else goto L04142
L04141:         c_cal$ = "005006000"
                c_ref$ = "1"

         /* Alter RF1 if HEAVY DUTY SCREENS                             */
L04142:     if pc_r$ <> "01" then goto L04143       /*EWD001*/
REM             IF PC_M$ = "353" AND STR(P$,11%,1%) = "8" THEN           ~
REM                                             STR(C_CAL$,1%,1%) = "H"
REM             IF (PC_M$ = "312" OR PC_M$ = "313" OR PC_M$ = "314") AND ~
REM               STR(P$,11%,1%) = "2" THEN     STR(C_CAL$,1%,1%) = "H"
REM /*EWD001*/  IF STR(C_CAL$,1%,1%) = "H" THEN STR(C_CAL$,4%,1%) = "H"
REM /*EWD001*/  IF STR(C_CAL$,1%,1%) = "H" THEN STR(C_CAL$,7%,1%) = "H"
REM         ----- REPLACED ABOVE W/MOD IN APCPR2SB FOR C/R = "C" -----

         /* Alter RF# codes if wood surround and ref field 1            */
L04143:     if wood_surround% = 0% or pc_r$ <> "01" then L04150
              for wsfm% = 1% to 7% step 3
                if str(c_cal$,wsfm%, 3) = "000" then goto L04145
                  if wsfm_nbr_model% = 1% then str(c_cal$,wsfm%,1%) = "W"
                  if wsfm_nbr_model% = 2% then str(c_cal$,wsfm%,1%) = "X"
                  if wsfm_nbr_model% = 3% then str(c_cal$,wsfm%,1%) = "Y"
L04145:       next wsfm%

            if debug%(1%) <> 0% then gosub display_debug

L04150:  /* DON'T DO COLOR IF WOOD SURROUND                         */


            if wood_surround% = 1% and pc_r$ = "02" then return

         /* DON'T DO GLASS IF WOOD SURROUND AND GLASS IS CLEAR      */
            if wood_surround% =  1%     and             ~
               pc_r$          = "03"    and             ~
               gls$           = "01"    then return

         /* DON'T DO PC_R$ = 09 IF NOT FACTORY MULL                    */
            if wsfm_instruction$ = " " and pc_r$ = "09" then return

         /* DON'T DO PC_R$ = 22 IF FACTORY MULL       EWD000 - NEW     */
                                                      /* (EWD015)      */
            gosub check_cont_head
            if wsfm_instruction$ <> " " and cont% = 1% then goto price_22
            if wsfm_instruction$ <> " " and pc_r$ = "22" then return
price_22:
         /* IF WE ARE WS/FM AND PC_R$ IS "09" DO SOME ALTERATIONS      */

            if wsfm_instruction$ <> " " and pc_r$ = "09" then L04160
                goto L04200
                                                     /* (EWD006)       */
L04160:     wsfm_nbr_model% = wsfm_nbr_windows%
            if wsfm_instruction$  = "FM" then c_cal$ = "000002003"
            if wsfm_instruction$ <> "FP" then L04170
                c_cal$ = "000008009"        /* replacement window */
                call "APCPR1SB" (0%, 1%, str(p$,1%,3%), gc$, temp16%,   ~
                                 #2, temp18%)
                temp16% = temp16%   /* Out damn spot! */
                if temp18% <> 1% then goto L04200
                    if str(gc$,1%,2%) = "NC" then c_cal$ = "000005006"
                                                      /* (EWD006)     */
L04170:     if wsfm_instruction$      = "SF" then c_cal$ = "000011012"
REM - EWD010   IF WSFM_INSTRUCTION$      = "SW" THEN C_CAL$ = "000017018"
REM - EWD010   IF WSFM_INSTRUCTION$      = "CW" THEN C_CAL$ = "000023024"
REM    IF WSFM_INSTRUCTION$      = "WS" THEN RETURN /* (EWD006) */
REM    IF WSFM_INSTRUCTION$      = "CP" THEN C_CAL$ = "019025026"

L04200:

REM        LOGMSG$ = "= = " & PC_R$ & "  " & SUBPART$
REM        CALL "LOGFILE" (LOGMSG$)
/* (AWD019) */
/* (SR67154) DO NOT CHARGE FOR SUPERSPACER IF NORTHERNENERGY STAR SELECTED */
/* (SR67154) ALWAYS UPCHARGE 150/160 $6.00 FOR FOAM IF NORTHERNENERGY STAR SELECTED*/
/* (SR67154) ONLY UPCHARGE 8300/450 WITH GRID $6.00 FOR FOAM IF NORTHERNENERGY STAR SELECTED */
/* PC_R$ = "45" = FOAM */
REM           IF PC_R$ <> "53" AND PC_R$ <> "16" AND PC_R$ <> "45" THEN GOTO NOSTOP

REM              CALL "SHOSTAT" ("STOP --> " & PC_R$)  STOP
REM NOSTOP
/* (CR614) do not need anymore only remaining check was superspacer */
REM           IF NFRC% = 0% THEN GOTO NOTNORTHERNNFRC
REM             IF PC_R$ = "45" AND NORTHERNFOAM$ = "1" THEN RETURN
/* (SR67154) SUPERSPACER ONLY ON 8900 SO THIS CHECK SHOULD CATCH ALL SUPERSPACER UPCHARGE */
/* (CR614) turn off skipping superspacer */
REM             IF PC_R$ = "53" AND STR(SERIES$,1%,4%) = "8900" THEN RETURN
/* (CR614) */
REM NOTNORTHERNNFRC

            if nfrc% = 0% and pc_r$ = "16" then return
/* (\SR67154) */

/* (SR71583) */
            if tpp% = 0% and pc_r$ = "17" then return
/* (\SR71583) */
            init(" ") sav_p$
            sav_p$ = p$
REM IF PC_R$ = "01" AND PAINTED% <> 0% AND DOOR% <> 0%  THEN STR(P$,4%,1%) = "2"

            if mdlc% = 1% and pc_r$ = "01" then str(p$,4%,1%) = neu_cl$


REM            if pc_r$ <> "01" then goto NotBase
REM            cmg$ = model$
REM            cmg1$ = pc_m$
REM            call "SHOSTAT" ("APCPR2SB BASE -> "& cmg$ & " -- " & cmg1$ ) 
REM            stop
REM NotBase

            call "APCPR2SB"  (model$,   /* Model                       */~
                              p$,       /* MFG Part Default Opening Sz */~
                              subpart$, /* MFG Part Number 1 (AWD016)  */~
                              x_c$,     /* Price Catalog Code          */~
                              x_cm$,    /* Price Catalog Calc Method   */~
                              pc_m$,    /* Model Code                  */~
                              pc_r$,    /* Associated Ref Type Code    */~
                              c_ref$,   /* Cross-Ref Code (PRICE 011)  */~
                              c_cal$,   /* Cross-Ref Calc Codes 3,3,3  */~
                              cuscode$, /* Customer Code               */~
                              wsfm_instruction$,                         ~
                                        /* Mfg instruct - for c_ref = 9*/~
                              wsfm_nbr_model%,                           ~
                                        /* # of windows - for c_ref = 9*/~
                              top_nbr_windows%,                          ~
                                        /* # of windows on top         */~
                              nbr_uchannel%,                             ~
                                       /* Number of Uchannel (AWD022)  */~
                              nbr_mclips%,                               ~
                                       /* Number of Mclips   (AWD022)  */~
                              sp_mull%, /* (AWD017) special mull       */~
                              pp,       /* Accumulative Price          */~
                              debug%(), /* Debug Flags 00%=Off,<>00%=On*/~
                              tpp$,     /* TriplePanePackage  (SR67154)*/~
                              nfrc$,    /* NRFC 2016 req      (SR71583)*/~
                              forcedfoam$, /* Forced Foam 2018         */~
                              series$,  /* Series             (SR71583)*/~
                              style$,   /* Stlye              (SR71583)*/~
                              #6,       /* (APCPCMSK) - File           */~
                              #2,       /* (GENCODES) - File           */~
                              #1,       /* (APCPCMST) - File           */~
                              #8,       /* (AWDPCMST) - File           */~
                              ee% )     /* 0% = Ok                     */
           sav_p1 = round(pp - sav_p,2) /* Price of Individual Ref Calc*/
           in% = 1%
           if debug%(1%) <> 0% then gosub display_debug
           wsfm_nbr_model% = save_nbr_model%
/* (AWD019) */
           p$ = sav_p$
/* (AWD022) */
           if woodClips% = 1% then return

           if pc_r$ <> "09" then return
           if wsfm_instruction$ = "FP" or wsfm_instruction$ = "FM" then goto woodClips
/* (\AWD022) */
        return
/* (AWD022) */
        woodClips
          woodClips% = 1%
          c_cal$ = "017017017"
          gosub L04200

REM do not add mull clips if not in subpart position 6
          if str(subpart$,6%,1%) = "0" then return
          c_cal$ = "018018018"
          gosub L04200
          woodClip% = 0%
        return
/* (\AWD022) */

        check_for_wsfm
            if lit_flg$ = "N" then goto L04300   /* (EWD007) Not Applic */
               wsfm_instruction$ = " "
               wsfm_ret% = 0%
               goto L04310

L04300:     call "APCPRWFB"                                               ~
                   (part$,              /* IN  MFG Part Number          */~
                    apcwood_code$,      /* OUT WS/FM code from part #   */~
                    apcwood_descr$,     /* OUT Uncensored APC WOOD descr*/~
                    wsfm_code$,         /* OUT APC WOOD before the "-"  */~
                    wsfm_descr$,        /* OUT APC WOOD  after the "-"  */~
                    wsfm_instruction$,  /* OUT Manufacturing instruction*/~
                    wsfm_nbr_model$,    /* OUT Pricing nbr for this guy */~
                    wsfm_nbr_windows$,  /* OUT Number of windows        */~
                    top_window$,        /* OUT Def of window(s) on top  */~
                                        /*       " ", H, T, S           */~
                    top_nbr_windows$,   /* OUT Number of windows on top */~
                                        /*       " " if no window on top*/~
                    nbr_uchannel%,      /* Number of Uchannel (AWD022)  */~
                    nbr_mclips%,        /* Number of Mclips   (AWD022)  */~
                    sp_mull%,           /* (AWD017) spec mull flag      */~
                    debug%(),           /* IN  Level (2) - 00=Off,01=On */~
                    #2,                 /* IN  GENCODES File Channel    */~
                    wsfm_ret%,          /* OUT 0% = OK; NOT WS or FM    */~
                                        /*     1% = Wood Surr/Fact Mull */~
                                        /*    >1% = Error               */~
                    wsfm_ret$)          /* OUT Text descr of ret_code   */

L04310:      if wsfm_ret% = 1% then goto L04420
                wsfm_nbr_windows% = 1%
                wsfm_nbr_model%   = 1%
                save_nbr_model%   = 1%
                top_nbr_windows%  = 0%
                return

L04420:     wsfm_nbr_windows% = pos("12345" = wsfm_nbr_windows$)
            if wsfm_nbr_windows% <> 0% then goto L04422
                wsfm_instruction$ = " "
                wsfm_nbr_windows% =  0%
                wsfm_nbr_model%   =  0%
                save_nbr_model%   =  0%
                top_nbr_windows%  =  0%
                wsfm_ret%         = 99%
                return
L04422:     top_nbr_windows% = 0%
            convert top_nbr_windows$ to top_nbr_windows%, data goto L04423
L04423:     wsfm_nbr_model% = 1%
            convert wsfm_nbr_model$  to wsfm_nbr_model%,  data goto L04424
L04424:     save_nbr_model% = wsfm_nbr_model%
            return


REM -----------------
    check_for_nonitem           /* EWD001 - New */
REM -----------------
            p% = 0%
            p% = pos(part$ = "-")
/* substract 5 b/c it starts or begins at position 5 in readkey below*/
            readkey$ = "PRICE 022" & str(part$,5%,(p%-5%))
            read #2, key = readkey$, using L04425, desc$, eod goto ni_end
L04425:         fmt pos(25), ch(30)
            pc_r$ = "20"  :  pc_rc$ = str(desc$,,3%)
            size$ = "F"
        REM    gosub standard_deduction
        REM        if err% <> 0% then goto ni_end

/* + (SR74362) */
            pc_c$ = "0000"
            if str(cuscode$,1%,2%) = "LO" then pc_c$ = str(desc$,27%,4%)
            if str(cuscode$,1%,2%) = "LX" then pc_c$ = str(desc$,27%,4%)
/* - (SR74362) */
            pc_key$ = " "
            str(pc_key$,1%,1%)   = "A"
REM            STR(PC_KEY$,2%,4%)   = "0000"          /* (SR74362) */
            str(pc_key$,2%,4%)   = pc_c$
            str(pc_key$,6%,2%)   = "00"
REM            STR(PC_KEY$,8%,16%)  = STR(PART$,1%,3%)  /* (CR2488) */
            str(pc_key$,8%,16%)  = pc_m$                /* (CR2488) */
            str(pc_key$,24%,2%)  = pc_r$
            str(pc_key$,26%,3%)  = pc_rc$
            str(pc_key$,29%,25%) = str(part$,1%,(p%-1%))

            read #1, key = pc_key$, using L04426, p, eod goto ni_std
L04426:         fmt pos(62), pd(14,4)
                goto ni_end

            ref$(1) = "20"  :  ref_p(1) = p
ni_std:     pc_key$ = " "
            str(pc_key$,1%,1%)   = "A"
            str(pc_key$,2%,4%)   = pc_c$
            str(pc_key$,6%,2%)   = pc_cm$
            str(pc_key$,8%,16%)  = str(part$,1%,3%)
            str(pc_key$,24%,2%)  = pc_r$
            str(pc_key$,26%,3%)  = pc_rc$
            str(pc_key$,29%,25%) = str(part$,1%,(p%-1%))
   REM PC_KEY$ = "A" & PC_C$ & PC_CM$ & STR(PC_M$,1,3) & PC_R$ & PC_RC$ & PART$

            read #1, key = pc_key$, using L04426, p, eod goto ni_spc
            ref$(1) = "20"  :  ref_p(1) = p
ni_spc:     pc_key$ = " "
            str(pc_key$,1%,1%)   = "A"
            str(pc_key$,2%,4%)   = spc_c$
            str(pc_key$,6%,2%)   = spc_cm$
            str(pc_key$,8%,16%)  = str(part$,1%,3%)
            str(pc_key$,24%,2%)  = pc_r$
            str(pc_key$,26%,3%)  = pc_rc$
            str(pc_key$,29%,25%) = str(part$,1%,(p%-1%))
   REM PC_KEY$ = "A" & SPC_C$ & SPC_CM$ & STR(PC_M$,1,3) & PC_R$ & PC_RC$ & PART$

            read #1, key = pc_key$, using L04426, p1, eod goto ni_end
            if p1 <> 0 then sp% = 1%
            ref1$(1) = "20"  :  ref_p1(1) = p1
ni_end:     gosub check_pricewarr                         /*  (EWD013)    */

            return

    REM --------------------------------------------------------------------
        check_pricewarr                                   /*  (EWD013)    */
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PRICEWARR"
            str(readkey$,10%,15%) = str(cuscode$,1%,6) & str(part$,1%,3%)

            read #2,key = readkey$, using L04425, desc$, eod goto no_pricewarr

                 convert str(desc$,1%,6%) to p, data goto no_pricewarr

        no_pricewarr
        return                                             /*  (EWD013)    */
    REM --------------------------------------------------------------------
        check_sample_display                       /* (EWD007)        */
            init(" ") readkey$, desc$, ss$
            ss% = 0%
            if len(part$) < 20 then goto LS2       /* Quick Test      */
            if str(part$,1%,1%) = "9" then goto LS2 /* Bay/Bow        */
            convert str(part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1 /* Not Samp/Disp   */
                                                   /*   (EWD014)      */
            if liting$ > "99" then goto LS1
               ss$ = str(part$,20%,3%)
               goto LS3                            /* Code Found      */
LS1:        convert str(part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
               ss$ = str(part$,23%,3%)
               goto LS3                            /* Code Found      */
LS2:        ss% = 0%                               /* Not a Samp/Disp */
        return
LS3:        str(readkey$,1%,9%)   = "PRICE 023"
            str(readkey$,10%,15%) = ss$ & str(part$,1%,3%)  /*  (EWD011) */

            read #2, key = readkey$, using L04425, desc$, eod goto LS4
            p = 0.0                              /* Replacement Price */
            convert str(desc$,1%,7%) to p, data goto LS2

       return                                               /*  (EWD011) */

LS4:        init(" ") readkey$
            str(readkey$,1%,9%)   = "PRICE 023"
            str(readkey$,10%,15%) = ss$ & "000"             /*  (EWD011) */

            read #2,key = readkey$, using L04425, desc$, eod goto LS2
            p = 0.0                              /* New Const Price   */
            convert str(desc$,1%,7%) to p, data goto LS2

            size$ = "F"
            gosub standard_deduction
                if err% <> 0% then goto LS2
        return

    REM ---------------------------
        display_debug
    REM ---------------------------
           gosub set_pf1
L04430:    accept                                                        ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
                                                                         ~
               at (04,02), fac(hex(94)), msg$                   , ch(32),~
               at (04,55),                                               ~
                  "Debug Display Screen",                                ~
/*(SR67154)*/  at (06,02), "Customer   :",                               ~
/*(SR67154)*/  at (06,15), fac(hex(84)), cuscode$               , ch(09),~
                                                                         ~
               at (07,02), "Part No.   :",                               ~
               at (07,15), fac(hex(84)), p$                     , ch(25),~
                                                                         ~
/*(SR67154)*/  at (08,02), "Sub Part   :",                               ~
/*(SR67154)*/  at (08,15), fac(hex(84)), subpart$               , ch(20),~
                                                                         ~
               at (09,02), "Prc Catalog:",                               ~
               at (09,15), fac(hex(84)), x_c$                   , ch(04),~
                                                                         ~
               at (10,02), "Catalog Cal:",                               ~
               at (10,15), fac(hex(84)), x_cm$                  , ch(02),~
                                                                         ~
               at (11,02), "Model Code :",                               ~
               at (11,15), fac(hex(84)), pc_m$                  , ch(16),~
                                                                         ~
               at (12,02), "Calc Ref Cd:",                               ~
               at (12,15), fac(hex(84)), pc_r$                  , ch(02),~
                                                                         ~
               at (13,02), "Cross-R Cod:",                               ~
               at (13,15), fac(hex(84)), c_ref$                 , ch(01),~
                                                                         ~
               at (14,02), "Cross-R Cal:",                               ~
               at (14,15), fac(hex(84)), str(c_cal$,1%,3%)      , ch(03),~
               at (14,20), fac(hex(84)), str(c_cal$,4%,3%)      , ch(03),~
               at (14,25), fac(hex(84)), str(c_cal$,7%,3%)      , ch(03),~
                                                                         ~
               at (15,02), "Option Prc :",                               ~
               at (15,15), fac(hex(84)), sav_p1$                , ch(10),~
                                                                         ~
               at (16,02), "Accum Price:",                               ~
               at (16,15), fac(hex(84)), pp$                    , ch(10),~
                                                                         ~
    /*EWD001*/ at (18,02), "Promo Calc :",                               ~
    /*EWD001*/ at (18,15), fac(hex(84)), promo_text$            , ch(20),~
                                                                         ~
               at (22,02), "Error Stat :",                               ~
               at (22,15), fac(hex(84)), ee$                    , ch(01),~
    /*EWD001*/ at (22,20), fac(hex(94)), errormsg$              , ch(35),~
                                                                         ~
               at(06,42), "Rf-<--- Description -->-<Calc Prc>",          ~
    /*EWD001*/ at(07,42), fac(hex(84)), txt$(sl%)               , ch(34),~
    /*Start*/  at(08,42), fac(hex(84)), txt$(sl% + 01%)         , ch(34),~
               at(09,42), fac(hex(84)), txt$(sl% + 02%)         , ch(34),~
               at(10,42), fac(hex(84)), txt$(sl% + 03%)         , ch(34),~
               at(11,42), fac(hex(84)), txt$(sl% + 04%)         , ch(34),~
               at(12,42), fac(hex(84)), txt$(sl% + 05%)         , ch(34),~
               at(13,42), fac(hex(84)), txt$(sl% + 06%)         , ch(34),~
               at(14,42), fac(hex(84)), txt$(sl% + 07%)         , ch(34),~
               at(15,42), fac(hex(84)), txt$(sl% + 08%)         , ch(34),~
               at(16,42), fac(hex(84)), txt$(sl% + 09%)         , ch(34),~
               at(17,42), fac(hex(84)), txt$(sl% + 10%)         , ch(34),~
               at(18,42), fac(hex(84)), txt$(sl% + 11%)         , ch(34),~
               at(19,42), fac(hex(84)), txt$(sl% + 12%)         , ch(34),~
    /*EWD001*/ at(20,42), fac(hex(84)), txt$(sl% + 13%)         , ch(34),~
    /* End */  at(21,42), fac(hex(84)), txt$(sl% + 14%)         , ch(34),~
                                                                         ~
               at (24,02), fac(hex(a4)),   inp$                 , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L05100
                  call "PRNTSCRN"
                  goto L04430

L05100:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            errormsg$ = " "                                 /*EWD001*/
            if in% = 0% then str(msg$,1%,14%) = "Before Calc - "         ~
                        else str(msg$,1%,14%) = "After Calc  - "

            if calc% = 0% then str(msg$,15%,17%) = "(APC Catalog)    "   ~
                          else str(msg$,15%,17%) = "(Special Catalog)"

            inp$ = "Press <Return> To Continue, PF(15) to Print the Scree~
        ~n?"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            convert sav_p1 to sav_p1$, pic($#,###.##-)
 /*EWD001*/ if promo_text$ <> " " then convert disc_amt to sav_p1$,      ~
 /*EWD001*/     pic($#,###.##-)
            convert pp to pp$, pic($#,###.##-)
 /*EWD001*/ if promo_text$ <> " " then convert p1 to pp$, pic($#,###.##-)
            convert ee% to ee$, pic(0)
 /*EWD001*/ if ee% = 1% then errormsg$ = "Price Not Found/Unable to Calc"
            if in% <> 0% then gosub format_calc
            if in% <> 0% and kk% > 15% then sl% = sl% + 1%  /*EWD001*/
        return

        format_calc
/*EWD001*/  if promo_text$ <> " " then return    /* KK% = Calc Ref that*/
            init(" ") readkey$, desc$            /*  Price Calculated  */
            str(readkey$,1%,9%)   = "PRICE 002"  /*  for. Last is equal*/
            str(readkey$,10%,15%) = pc_r$        /*  to Total No. Calcs*/
            kk% = kk% + 1%
            read #2,key = readkey$, using L05360 , desc$, eod goto L05370
L05360:        FMT POS(25), CH(20)
L05370:     txt$(kk%) = pc_r$ & "-" & desc$
            convert  pp to str(txt$(kk%),25%,10%), pic($#,###.##-)

        return

        check_debug                        /* Check to see if Debug is */
            init(" ") readkey$             /* turned on for Userid     */
            sl% = 1%                             /*EWD001*/
            str(readkey$,1%,9%)   = "PRICE 015"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L05470 , desc$, eod goto L05530
L05470:        FMT POS(25), CH(30)
                                           /* Set for APCPRSUB Debug   */
            convert str(desc$,1%,2%)  to debug%(1%), data goto L05500
L05500:                                    /* Set for APCPR2SB Debug   */
            convert str(desc$,15%,2%) to debug%(2%), data goto L05520
L05520:
L05530: return

        check_promo
                                           /* (EWD004) - Grid Promotion      */
                                           /* Promo's(1) Dollar amount       */
                                           /*            deduction/Model     */
                                           /*        (2) Percent Amount      */
                                           /*            deduction/Model     */
                                           /*        (3) old                 */
                                           /*        (4) Grid Deduction      */
                                           /*            -10.00 = 01-82      */
                                           /*            - 4.00 = 1 inch Grid*/
            init(" ") readkey$, desc$, dte1$, dte2$, dte3$, promo_no$,   ~
                        txt$()                                  /*EWD001*/
            if len(part$) < 19 then return
            if lit_flg$ = "Y" then goto L06020       /* (EWD007)        */

            str(readkey$,1%,9%)   = "PRICE 018"
            str(readkey$,10%,15%) = str(cuscode$,1%,6%)
            read #2,key = readkey$, using L05610 , desc$, eod goto L06020
L05610:        FMT POS(25), CH(30)
            dte1$     = str(desc$,1%,8%)   /* Start Date of Promotion   */
            dte2$     = str(desc$,12%,8%)  /* End Date of Promotion     */
            promo_no$ = str(desc$,25%,1%)  /* Promotion No. in Customer */
                                           /* Table                     */
                                           /* (EWD005) Save for Test    */
            call "DATUNFMT" (dte1$)
            call "DATUNFMT" (dte2$)
            dte3$ = date
                                           /* See if Promotion in Affect*/
            if dte3$ < str(dte1$,1%,6%) or dte3$ > str(dte2$,1%,6%) then ~
                                                   goto L06020


            init(" ") readkey$, desc$, promo_flag$
            str(readkey$,1%,9%)   = "PRICE 019"
            str(readkey$,10%,15%) = str(part$,1%,3%)
            read #2,key = readkey$, using L05610 , desc$, eod goto L06020
            promo = 0.0
            convert str(desc$,1%,6%) to promo, data goto L06020

            promo_flag$ = str(desc$,12%,1%)
            if promo_flag$ = "2" or promo_flag$ = "4"then goto L05890
                                                  /* (1) - Promotion   */
                                                  /* Deduct Dollar Amt */
               if promo_flag$ <> "1" then return  /* Exit              */

               if p1 > .01 then p1 = round(p1 + promo, 2)
               if p < .01 then goto L05860                      /*EWD001*/
                  for i% = 1% to 36%
                      pc(i%) = round(pc(i%) + promo, 2)
                      if pc(i%) < .01 then pc(i%) = 0.0
                  next i%
               pc(17%) = -1
L05860:        disc_amt = promo                                 /*EWD001*/
               promo_text$ = str(desc$,1%,6%) & " $ Discount"   /*EWD001*/
               return

L05890:     if promo_flag$ = "4" then goto L06100
            if promo_no$ = "4" then return        /* (EWD005)-Not Elig. */
                                                  /* (2) - Promotion    */
            if do_before% = 1% then return
            if p1 < .01 then goto L05940          /* Deduct Pcnt Amount */
               disc_amt = round(p1 * (promo / 100.0), 2)
               p1 = round(p1 + disc_amt, 2)
L05940:     if p < .01 then goto L06000                         /*EWD001*/
               for i% = 1% to 36%
                   disc_amt = round(pc(i%) * (promo / 100.0), 2)
                   pc(i%) = round(pc(i%) + disc_amt, 2)
                   if pc(i%) < .01 then pc(i%) = 0.0
               next i%
            pc(17%) = -1               /* Set for 'Q' Requires that    */
                                       /* the price be Manually Entered*/
L06000:     promo_text$ = str(desc$,1%,6%) & " % Discount"      /*EWD001*/
L06020: return

L06100:   promo = 0.0                         /* (4) - Grid Promotion   */
          if do_before% = 0% then return
          convert liting$ to chk_grid%, data goto L06110

          if chk_grid% > 0% and chk_grid% < 83% then promo = -10.00
                                             /* Check Lock Codes for   */
                                             /* '1' Inch Grid          */
L06110:   chk% = pos("ABCDEFGILMNZ" = str(part$,12%,1%))
                                             /* Set grid Dollar Deduct */
          if chk% > 0% then promo = promo + (-4.00)

          if p1 < .01 then goto L06120
                                             /* Special Catalog        */
          gosub lookup_customer
          p1 = round(p1 + (dsc_pcnt * promo), 2)

L06120:
          if p  > .01 then p  = round(p  + promo, 2)

          disc_amt = promo
          promo_text$ = str(desc$,1%,6%) & " Grid Discount "

        return
                                           /* (EWD004) - End Grid Promo*/
        exit_program
         if sp% = 2% then err% = 2%       /* Set for EDI Price Calc */
        end

        lookup_customer                    /* (EWD004) - New Routine   */
          init(" ") readkey$, descr$, pcnt_code$
          dsc_pcnt = 100
          rhh% = 0%
          read #4,key = cuscode$, using L07000, pcnt_code$, eod goto L07010
L07000:      FMT POS(525), CH(1)

          readkey$ = "PRICECODE" & pcnt_code$
          call "DESCRIBE" (#2, readkey$, descr$, 0%, rhh%)
          convert str(descr$,1%,2%) to dsc_pcnt, data goto L07010

L07010:   dsc_pcnt = dsc_pcnt/100.0
        return                             /* (EWD004) - End of Routine*/



        check_cont_head
         cont% = 0%
         str(readkey$,1%,9%)   = "PLAN CONT"
         str(readkey$,10%,15%) = str(part$,1%,3%)
         read #2,key = readkey$, eod goto not_cont_head
             cont% = 1%
        not_cont_head
        return

/* (AWD019) */
        check_door
          door% = 0%
          str(readkey$,1%,9%)   = "PLAN DOOR"
          str(readkey$,10%,15%) = str(part$,1%,3%)
          read #2,key = readkey$, eod goto not_door
              door% = 1%
        not_door
        return

        check_painted
          painted% = 0%
          mat painted = zer

          search painted$ = str(part$,4%,1%) to painted()
          painted% = int(painted())
        return
/* (\AWD019)*/
/* (AWD021) */
        check_pricemdlc
          mdlc% = 0%
          init(" ") readkey$, desc$, neu_cl$
          str(readkey$,1%,9%)   = "PRICEMDLC"
          str(readkey$,10%,15%) = str(part$,1%,4%)

          read #2, key = readkey$, using L04425, desc$, eod goto noPricmdlc

             mdlc% = 1%
             neu_cl$ = str(desc$,1%,1%)
        noPricmdlc
        return
/* (\AWD021) */



