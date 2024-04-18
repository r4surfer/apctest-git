        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPL41C - Subroutine                *~
            *  Creation Date     - 12/21/98                             *~
            *  Last Modified Date- 06/11/2012                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Allocate Glass Rack Locations        *~
            *                      check_limits - Verifies the Max Width*~
            *                                     and Height Allowd in  *~
            *                                     the rack.             *~
            *                                                           *~
            *  Notes             - Most Racks have Twenty-Five Slots    *~
            *                      on the Top and Bottom. Except        *~
            *                    - The Picture Window Dept. (047). It   *~
            *                      only has Twenty Slots.               *~
            *                    - Departments that use Twenty-Five     *~
            *                      slots (No Bottom)                    *~
            *                      Bay/Bow                  (009) N/A   *~
            *                      Casement                 (008)       *~
            *                      Special Shapes           (043)       *~
            *                      Slider                   (048)       *~
            *                      Vinyl sliding Glass Door (023)       *~
            *                                                           *~
            *  Rack Type Codes   - 0 = Top/Bottom (25)                  *~
            *                      1 = Top Only   (25)                  *~
            *                      2 = Top Only   (20)                  *~
            *                                                           *~
            *  Special Flags     - (1,1) Argon Gas                      *~
            *                      (2,1) Special Glass  No Rack         *~
            *                      (3,1) Tempered Glass No Rack         *~
            *                      (4,1) Diamond Grid   No Rack         *~
            *        (EWD004)      (5,1) Sunclean Glass                 *~
            *        (EWD009)                                           *~
            *  Special Flags     - (1,1) Argon Gas                      *~
            *                      (2,1) Sunclean Glass                 *~
            *                      (3,1) Tempered Glass                 *~
            *                      (4,1) Specials Glass/Grid            *~
            *                      (5,1) Put in (5,1) but in Label Pgm  *~
            *                            puts 'S' in Slot (2,1)         *~
            *                    - Skip liting 83% - 88% and 99%        *~
            *                                                           *~
            *  Tables            - (PLAN RACK)(PLANARGON)(PLAN TEMP)    *~
            *                      (GED 001)(PLANKRYPT)                 *~ 
            * (AWD021)                                                  *~
            *  pass% 1% = Tin Plate                                     *~
            *  pass% 2% = DuraSeal                                      *~
            *  pass% 3% = DuraLite                                      *~
            *  pass% 4% = SuperSpacer                                   *~
            *  pass% 5% = Thin Plate                                    *~
            *  pass% 6% = Ultra Intercept                               *~
            *  pass% 7% = DS  Tin Plate                                 *~
            *  pass% 8% = DS  DuraSeal                                  *~
            *  pass% 9% = DS  DuraLite                                  *~
            *  pass% 10% = DS SuperSpacer                               *~
            *  pass% 11% = DS Thin Plate                                *~
            *  pass% 12% = DS Ultra Intercept                           *~
            *  pass% 13% = Temp  Tin Plate                              *~
            *  pass% 14% = Temp  DuraSeal                               *~
            *  pass% 15% = Temp DuraLite                                *~
            *  pass% 16% = Temp SuperSpacer                             *~
            *  pass% 17% = Temp Thin Plate                              *~
            *  pass% 18% = Temp Ultra Intercept                         *~
            *  pass% 19% = 8900 Triple                                  *~
            *  pass% 20% = VPD1                                         *~
            *  pass% 21% = VPD2                                         *~
            *  pass% 22% = STC                                          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/15/99 ! New Program for (EWD) - Last Mod Date    ! RHH *~
            * 07/19/99 ! (EWD001) special mod to check Size limits! RHH *~
            *          ! for department 048, 008, 047,033         !     *~
            * 10/18/00 ! (EWD002) Mod to put TTTTT for Tempered   ! RHH *~
            *          ! and reserve a slot.                      !     *~
            * 12/20/01 ! (EWD003) Mod to for new 421 and 431 rack ! CMG *~
            *          !          labels.                         !     *~
            * 10/15/02 ! (EWD004) Mod for new sun clean glass to  ! CMG *~
            *          !          show a 'S' on rack label.       !     *~
            * 10/15/02 ! (EWD005) Mod for new dept '051' and '026'! CMG *~
            *          !          cont. head/sill product.        !     *~
            * 10/21/03 ! (EWD006) Mod for Tempered Glass Rack Lbls! CMG *~
            * 10/24/03 ! (EWD007) Mod to use 'PLAN CONT' table    ! CMG *~
            *          !      by model for cont. head/sill instead!     *~
            *          !      of department.                      !     *~
            * 11/05/03 ! (EWD008) Mod to keep specials on Rack Lb ! CMG *~
            * 11/12/03 ! (EWD009) Mod for Double Strength Glass.  ! CMG *~
            * 11/12/03 ! (EWD010) Mod for Obscure Glass.          ! CMG *~
            * 05/04/03 ! (EWD011) Mod for TSO/BSO Cont Head Sill  ! CMG *~
            * 06/02/04 ! (EWD012) Mod to make liting codes 83-87  ! CMG *~
            *          !    and 97-99 to have a 'Z' for spec liting!    *~
            * 04/26/05 ! (AWD013) Mod to change the rack size     ! CMG *~
            * 05/19/05 ! (AWD014) Mod to add U where Sunclean was ! CMG *~
            *          !        for UPS product                   !     *~
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~
            *          !    Change record length for two files    !     *~
            *          !    (APCPLNGR) (EWDPLNGR) to 384. Also    !     *~
            *          !    Change rm_rec$(2%)200 from 256        !     *~
            * 03/16/06 ! (AWD015) Mods for Krypton                ! CMG *~
            * 03/07/07 ! (AWD016) Mod for tempered and ds rack lbs! CMG *~
            *08/12/2008! (AWD017) Mod for sdl                     ! DES *~
            *11/23/2009! (AWD018) mod for ultra intercept         ! CMG *~
            *08/11/2010! (AWD020) mod for patio doors             ! DES *~
            *03/22/2011! (AWD021) mod for intercept changes       ! CMG *~
            *05/01/2012! (AWD022) mod changes for 8900 rack lbls  ! CMG *~
            *06/11/2012! (AWD023) mod to rack limits              ! CMG *~
            *11/14/2014! (AWD024) mod for TX to allow all glass   ! PWW *~
            *          !    to be on a top-rack only.             !     *~
            *05/18/2015! (IM8022) mod to remove awdplngr and lamin! CMG *~
            *10/03/2016! CR00604 mod to seperate Tempered from DS ! PWW *~
            *          !         on Rack Labels.                  !     *~
            *06/19/2017! CR00950 TX sort issue for dept 59 5700.3 ! RDB *~
            *08/09/2017! CR0605  new rack label format for triple ! RDB *~
            *09/18/2017! CR1121  Change the check limit to include! RDB *~
            *          !         the sequence # and not all A's   !     *~
            *          !         Change triple pane label to 25   !     *~
            *05/03/2019! CRPROJ  Add dept 029, 070, 071 to TT BB  ! RDB *~
            *05/13/2019! CR2020  Argon flag for PlyGem            ! RDB *~
            *05/21/2019! CR2035  Set flag 3 to T and not L        ! RDB *~
            *08/05/2019! CR2140  Add dept 072 to TT BB            ! RDB *~
            *09/11/2019! CR2218  Change 072 to 027                ! RDB *~
            *10/26/2019! CR2296  New NC STC rack label            ! RDB *~
            *11/01/2019! CR2322  Make NC dept 053 print top/bottom! RDB *~
            *01/30/2020! CR2406  Error on 3-lite slider dept 053  ! RDB *~
            *02/07/2020! CR2412  Add dept 058 to TT BB            ! RDB *~
            *03/23/2020! CR2474  Add V for customer valance glass ! RDB *~
            *03/17/2021! CR2792  Increase slots for STC except 047! RDB *~
            *04/16/2021| CR2809  Slider moving - dept 005 and 019 ! RDB *~
            *08/20/2021! CR9999  Change dept 019 slider to no botm! RDB *~            
            *************************************************************

        sub "APCPL41C" (warranty$,       /* Warranty Id                */~
                        rk_date$,        /* Production Date            */~
                        rk_dept$,        /* Department Code            */~
                        rk_userid$,      /* Assigned by User           */~
                        rk_hows$,        /* Hows   (AWD014)            */~
                        del%,            /* (AWD047)                   */~
                        #1,              /* (GENCODES) File            */~
                        #2,              /* (APCPLNGR) File    (PAR000)*/~
/*(AWD016)*/            #3,              /* (AWDPLNRK) FILE            */~
                        pass%,           /* Number of times            */~
                        schema%,         /* Schema                     */~
                        rk_err% )        /* Error Code 0% = Ok         */

        dim                                                              ~
            warranty$9, rm_rec$(2%)256,  /* Glass Remake File  (PAR000)*/~
            rm_key$12,                   /* Glass Remake Primary Key   */~
            readkey$25, desc$30,         /* Gencodes Key and Descript  */~
            desc2$30,                    /* Gencodes Description 2     */~
            rm_wd_d$8, rm_ht_d$8,        /* Width/Height Decimal Val   */~
/*AWD022*/  rk_key$12,                   /* Primary Rack Key           */~
            rk_key1$14,                  /* Rack Key #1     EWD011     */~
            rk_key1_new$22,              /* Rack Key #1     AWD021     */~
            rk_barcode$9,                /* Rack/Glass Barcode Unique  */~
            rk_date$6, rk_date_fm$10,    /* Production Date Formatted  */~
            sav_date$6, sav_date_fm$10,  /* Last used date             */~
            rk_time$8,                   /* Time Rack assigned         */~
            rk_number$4, tt_num$4,       /* Rack Number Assigned       */~
            rk_loc$1,                    /* Location 0=Top,1=Bot,2=rack*/~
            rk_slot$2,                   /* top=1 - 25, Bot=1 - 25     */~
            rk_type$1,                   /* Rack Type code 0,1,2       */~
                                         /* Production Sort Seq        */~
            rk_dept$3, sav_dept$3,       /* Production Dept Code       */~
            check_dept$3,                /* (EWD001) Test Limits       */~
            rk_specials$10,              /* Glass Special Flags        */~
            rk_model$3,                  /* Model Code                 */~
            rk_glass$2,                  /* Product Glass              */~
            rk_userid$3,                 /* Assigned By User           */~
            rk_status$1,                 /* Status 0 =Sched, 1 = Comple*/~
            rk_filler$64,                /* Filler Area                */~
            rk_part$25,                  /* Part Number                */~
            rk_sub_part$20,              /* New Sub Part Numbe (PAR000)*/~
            rk_liting$2,                 /* Liting Code                */~
            rk_hinge$2,                  /* Hinge Code                 */~
            rk_screen$1,                 /* Screen Code                */~
/*AWD022*/  rk_top$128,                  /* Rack Record (Top)          */~
/*AWD022*/  rk_bot$128,                  /* Rack Record (Bot)          */~
            rk_hows$2,                   /* How Ship           (AWD014)*/~
            pass$3,                      /* Pass through number        */~
            rk_sash$1,                   /* Sash code                  */~
            rk_rck_type$3                /* Rack Glass Type - Pass (AWD022)*/

        dim f1$23, f2$23
        
        dim patio$1,                     /* Patio Door Flag            */~
            rk_stock$1,                  /* Rack Patio Stock Flag      */~
            rm_patio$1                   /* Glass File patio flag      */

        dim intercept$2,                 /* (AWD021)                   */~
            rk_intercept$5,              /* (AWD021)                   */~
            gls_type$3                   /* (AWD021)                   */

        dim message$256

        REM *************************************************************~
            *            B E G I N   P R O C E S S I N G                *~
            *                                                           *~
            *************************************************************

            no_bottom% = 0%
            rk_type$   = "0"
            if del% = 1% then gosub clear_data
            if del% = 1% then goto exit_sub

            select #5,   "APCWORK5",                                     ~
                        varc,     indexed,  recsize =  512,  /*(IM8022)*/~
                        keypos =    22, keylen =   12

            mode% = 1% : gosub open_work          /* only Open Once  */
            mode% = 3% : gosub open_work
            gosub read_glass

            t_cnt% = 0%                          /* Count Tops          */
            b_cnt% = 0%                          /* count Bots          */
            rk_err% = 0%
/* (AWD022) */
            init(" ") rk_number$, rk_date_fm$, rk_filler$, rk_rck_type$
            convert pass% to rk_rck_type$, pic(000)

            rk_date_fm$ = rk_date$               /* Production Date     */
            call "DATFMTC" (rk_date_fm$)         /* Formatted Prod Date */
            if rk_dept$ = "DDD" then goto update_table
                                                 /* Done Closeout Table */

            if sav_date$ = rk_date$ then goto L00100
               convert pass% to pass$, pic(###)
               call "SHOSTAT" ("Creating Report and Rack Data")
               call "SHOSTAT" ("warranty --> " & warranty$ &              ~
                                  rk_dept$ & " Pass ---> " & pass$)
REM     STOP   /* REPLACE */

               init(" ") readkey$, rk_top$, rk_bot$
               sav_date$ = rk_date$
               sav_date_fm$ = sav_date$
               call "DATFMTC" (sav_date_fm$)
               rk_date_fm$ = sav_date_fm$

               str(readkey$,1%,9%)   = "PLAN RACK"
               str(readkey$,10%,15%) = sav_date_fm$
               read #1,key = readkey$, using L00010, desc$, eod goto L00020
L00010:           FMT POS(25), CH(30)
               tt_num$   = str(desc$,1%,4%)      /* Load Next Rack Number */
               goto L00030

L00020:        tt_num$ = "0000"                  /* Init Rack Number      */

L00030:        convert tt_num$ to rk_number%, data goto L00040
L00040:

L00100:    init(" ") rm_key$                        /* (APCPLNGR) - File   */
           str(rm_key$,1%,8%) = str(warranty$,1%,8%)

L00200: gosub find_glass                         /*  (EWD006)            */
        if glass% <> 1% then goto L65050         /*  (EWD006)            */
/* CR2296 */ 
        rk_part$  = str(rm_rec$(),125%,25%)
        rk_glass$ = str(rk_part$,5%,2%) 
        gosub check_stc
         
/* (AWD018) do not include ultra in second pass */
REM  !        IF STR(RM_REC$(),297%,1%) = "1" AND PASS% = 2% THEN GOTO EXIT_SUB
REM  !        IF STR(RM_REC$(),299%,1%) = "1" AND PASS% = 2% THEN GOTO EXIT_SUB
         if pass% = 1%  and intercept$ <> "01" then goto exit_sub
         if pass% = 7%  and intercept$ <> "01" then goto exit_sub
         if pass% = 13% and intercept$ <> "01" then goto exit_sub /*CR00604 */

         if pass% = 2%  and intercept$ <> "02" then goto exit_sub
         if pass% = 8%  and intercept$ <> "02" then goto exit_sub
         if pass% = 14% and intercept$ <> "02" then goto exit_sub /*CR00604 */

         if pass% = 3%  and intercept$ <> "03" then goto exit_sub
         if pass% = 9%  and intercept$ <> "03" then goto exit_sub
         if pass% = 15% and intercept$ <> "03" then goto exit_sub /*CR00604 */

         if pass% = 4%  and intercept$ <> "04" then goto exit_sub
         if pass% = 10% and intercept$ <> "04" then goto exit_sub
         if pass% = 16% and intercept$ <> "04" then goto exit_sub /*CR00604 */

         if pass% = 5%  and intercept$ <> "05" then goto exit_sub
         if pass% = 11% and intercept$ <> "05" then goto exit_sub
         if pass% = 17% and intercept$ <> "05" then goto exit_sub /*CR00604 */

         if pass% = 6%  and intercept$ <> "06" then goto exit_sub
         if pass% = 12% and intercept$ <> "06" then goto exit_sub
         if pass% = 18% and intercept$ <> "06" then goto exit_sub /*CR00604 */
/* (AWD022) triple pane has to be superspacer */    
         if pass% = 19% and intercept$ <> "04" then goto exit_sub 
/* CR2296 */
         if pass% = 22% and stc% = 0% then goto exit_sub       
         
/* (AWD020) only include VPD in pass 15 & 16 */
        if patio$ <> "0" and pass% < 20% then goto exit_sub
        if patio$ <> "2" and pass% = 20% then goto exit_sub
        if patio$ <> "1" and pass% = 21% then goto exit_sub

        rk_stock$ = patio$
        rk_part$  = str(rm_rec$(),125%,25%)       /* Part Number MFG(PAR000)*/
        rk_model$ = str(rk_part$,1%,3%)         /* Model Code (PAR000)*/
        rk_sub_part$ = str(rm_rec$(),257%,20%)  /* New Sub Part No. (PAR000)*/
        rk_glass$ = str(rk_part$,5%,2%)           /* Glass Code         */
        tripane% = 0%
        gosub check_triplepane                    /* (AWD022)             */

        rk_liting$= str(rk_part$,7%,2%)           /* Liting Code        */
        rk_hinge$ = str(rk_part$,9%,2%)           /* Hinge Code         */
        rk_screen$= str(rk_part$,11%,1%)          /* Screen Code        */
        rk_specials$ ="          "                /* Special Flags      */
        rk_status$   = "0"                        /* Set as Sched       */
        call "TIME" (rk_time$)                    /* Set Time           */
        rm_wd_d$ = str(rm_rec$(),191%,8%)         /* width Decimal (PAR000)*/
        rm_ht_d$ = str(rm_rec$(),199%,8%)         /* Height Decimal(PAR000)*/
        wd = 0.0 : ht = 0.0

        convert rm_wd_d$ to wd, data goto L00230  /* Width Decimal Val  */
L00230
        convert rm_ht_d$ to ht, data goto L00240  /* Height Decimal Val */
L00240
        gosub allocate_rack  

        if skip% <> 0% then goto L00200           /* No Rack            */

/* CR2296 */ if stc% = 1% and schema% = 1% and pass% = 22% ~
                 then str(rk_top$,27%,1%) = "S"
             if stc% = 1% and schema% = 1% and pass% = 22% ~
                 then str(rk_bot$,27%,1%) = "S"
        
        if no_bottom% = 0% then goto L00350
REM           if cont_head% = 1% then goto L00350

                                                  /* Tops Only          */
           write #3, using L00300, rk_top$, eod goto L65000
L00300:       FMT CH(128)
           rk_slot% = rk_slot% + 1%               /* Always One Ahead   */
        goto L00200

L00350:                                           /*   (EWD003)  BEG    */

REM           IF T_CNT% = 0% OR B_CNT% = 0% THEN GOTO L65055    /* (EWD011) */
           init(" ") rk_sash$
           rk_sash$ = str(rk_part$,11%,1%)
           if rk_sash$ <> "4" and rk_sash$ <> "5" and rk_sash$ <> "6" ~
                    then goto not_tso_bso
                          if str(rk_part$,11%,1%) = "4" then t_cnt% = 1%
                          if str(rk_part$,11%,1%) = "4" then b_cnt% = 0%
                          if str(rk_part$,11%,1%) = "6" then t_cnt% = 1%
                          if str(rk_part$,11%,1%) = "6" then b_cnt% = 0%
                          if str(rk_part$,11%,1%) = "5" then t_cnt% = 0%
                          if str(rk_part$,11%,1%) = "5" then b_cnt% = 1%
                             goto L65055

not_tso_bso:
/* CR2406 CR2412 + */
           if ((schema% = 1% and rk_dept$ = "053")           ~
              or (schema% = 2% and rk_dept$ = "058"))        ~           
              and slid3% = 1%                                ~
              and str(rm_rec$(),30%,1%) > "5"                ~
               then goto slid053 ~
               else goto no_slid053
slid053:   
/* reset for the second barcode on the bottom rack label for 3 lite slider */        
           t_cnt% = 0%
           b_cnt% = 1%     
           no_bottom% = 0%
           rk_num% = 0%
           convert str(rk_barcode$,9%,1%) to rk_num%, data goto L00380
           convert rk_num% to str(rk_bot$,12%,1%), pic(0)
           goto L65055
no_slid053:          
/* CR2406 - */
                                     /* (EWD011) */
           rk_barcode$ = str(rk_key$,4%,9%)
           rk_num% = 0%
           convert str(rk_barcode$,9%,1%) to rk_num%, data goto L00380

L00380:    rk_num% = rk_num% + 5%

           str(rk_bot$,4%,8%) = str(rk_barcode$,1%,8%)      /* Set Bot */
           convert rk_num% to str(rk_bot$,12%,1%), pic(0)
/* CR Proj CR2140 CR2218 CR2412 */
           if schema% <> 2%  or ~
             (rk_dept$ = "029" and schema% = 2%) or ~    
             (rk_dept$ = "070" and schema% = 2%) or ~
             (rk_dept$ = "071" and schema% = 2%) or ~
             (rk_dept$ = "027" and schema% = 2%) or ~ 
             (rk_dept$ = "058" and schema% = 2%) or ~             
             (rk_dept$ = "059" and schema% = 2%)         then     /*AWD024*/  ~
              str(rk_bot$,23%,1%) = "1"           /* Set for Bottom       */
                
           write #3, using L00300, rk_top$, eod goto L65000
/*AWD024 + */
/* CR Proj CR2140 CR2218 CR2412 */
           if schema% <> 2% or (schema% = 2% and rk_dept$ = "059") or  ~
             (schema% = 2% and rk_dept$ = "058") or  ~
             (schema% = 2% and rk_dept$ = "029") or  ~
             (schema% = 2% and rk_dept$ = "070") or  ~  
             (schema% = 2% and rk_dept$ = "071") or  ~
             (schema% = 2% and rk_dept$ = "027")     ~
             then goto not_tx             /*AWD024*/
              rk_slot% = rk_slot% + 1%           /*AWD024*/
           if rk_slot% <= rk_max% then goto max_ok
              rk_slot% = 1%                      /* Start Slot (1) New Rack*/
              rk_number% = rk_number% + 1%       /* Increment Rack Number */
              convert rk_number% to rk_number$, pic(0000)
              str(rk_bot$,19%,4%) = rk_number$          /* Rack Number Ass    */
max_ok:       convert rk_slot% to rk_slot$, pic(00) /* Update for Tops Only */
              str(rk_bot$,24%,2%) = rk_slot$            /* Rack Slot No.   */

not_tx:
/*AWD024 - */
           write #3, using L00300, rk_bot$, eod goto L65000


           rk_slot% = rk_slot% + 1%               /* Always One Ahead   */
        goto L00200                               /*   (EWD003)  END    */

        allocate_rack
           skip% = 0%

           gosub check_sdl                       /* <AWD017> */
           gosub check_double                    /* (EWD009) Dble Strength*/
           gosub check_special_glass
           gosub check_tempered
           gosub check_diamond
           gosub check_sunclean                  /* (EWD004) Sunclean     */
           gosub check_obscure                   /* (EWD010) Obscure Glass*/
           gosub check_ups                       /* (AWD014)              */
           gosub check_ultra                     /* (AWD018) */
           gosub check_intercept                 /* (AWD021) */
           gosub check_bottom

/* CR2296 */ if stc% = 1% and schema% = 1% then str(rk_specials$,2%,1%) = "S"

/*(AWD016) begin*/
           gosub check_pass
             if skip% <> 0% then return
/*(AWD016/) end */
           gosub check_slider
/* CR Proj CR2140 CR2218 CR2412 */
           if slid% = 1%                           and ~
           not (schema% = 2% and rk_dept$ = "029") and ~  
           not (schema% = 2% and rk_dept$ = "070") and ~
           not (schema% = 2% and rk_dept$ = "071") and ~  
           not (schema% = 2% and rk_dept$ = "027")     ~
           then no_bottom% = 1%

/* CR2322 NC requested dept 053 to print top/bottom & CR2412 */
/* CR2809 slider move to dept 005 and 019 */
           if (schema% = 1% and slid% = 1% and rk_dept$ = "053") or  ~
              (schema% = 1% and slid% = 1% and rk_dept$ = "005") or  ~
              (schema% = 2% and slid% = 1% and rk_dept$ = "058")     ~ 
                then no_bottom% = 0%
/* CR9999 */                
           if (schema% = 1% and slid% = 1% and rk_dept$ = "019")  ~
                then no_bottom% = 1%
                
           if schema% = 2% then skip_deptNC_check  
           
      /* CR605 change */
           if tripane% = 1% and pass% = 19% then no_bottom% = 1%
           if tripane% = 1% and pass% = 19% then rk_type$ = "3"
      
           if rk_dept$ = "009" or rk_dept$ = "008" or rk_dept$ = "043" or ~
              rk_dept$ = "048" or rk_dept$ = "023" then no_bottom% = 1%
                                                 /* Tops Only (25) Slots  */
             
           if no_bottom% = 1% and rk_type$ = "0" then rk_type$ = "1"
             goto rackDeptCheck
skip_deptNC_check:
REM              if rk_dept$ = "070" then no_bottom% = 1%   /* Casement      */
REM              if rk_dept$ = "029" then no_bottom% = 1%   /* 300/375 Slder */
              if rk_dept$ = "040" then no_bottom% = 1%   /* Patio Alum    */
              if rk_dept$ = "041" then no_bottom% = 1%   /* Patio Vinyl   */
/*(SR80370) if rk_dept$ = "059" then no_bottom% = 1%    5700.3        */             
              if no_bottom% = 1% and rk_type$ = "0" then rk_type$ = "1"
rackDeptCheck:
                                                 /* Do Not allocate Rack  */
              if sav_dept$ = rk_dept$ and sav_pass% = pass% then goto L60000
              sav_dept$ = rk_dept$               /* Start a New Rack      */
              sav_pass% = pass%                  /* Start a New Rack      */
              rk_type$   = "0"                   /* Set to Top/Bot        */
              rk_max%    = 25%                   /* Max No. of (Slots - 1)*/
              no_bottom% = 0%                    /* Set to Top/Bottom     */
              cont_head% = 0%                    /*  (EWD003)             */

              if rk_dept$ <> "047" or schema% = 2% then goto L00390
                 rk_type$   = "2"                /* Top Only (20) Slots   */
                 rk_max%    = 20%                /* Max of 20 Slots       */
                 no_bottom% = 1%                 /* Tops Only, no Bottoms */

/* <AWD020> */
/*L00390:       if pass% < 15% then goto L00400 CR00604  (AWD021) */
L00390:       if pass% < 20% then goto L00400    /* (AWD021) */
              if pass% = 22% then goto L00500    /* CR2296 */
                 rk_type$   = "2"                /* Top Only (20) Slots   */
                 rk_max%    = 20%                /* Max of 20 Slots       */
                 if schema% = 2% then rk_max% = 25%
                 no_bottom% = 1%                 /* Tops Only, no Bottoms */
/* </AWD020> */

L00400:
/* (AWD022) */  
               if pass% <> 19% then goto L00500
                rk_type$ = "3"
                rk_max%  = 25%   /* CR1121 - doesnt count dept slot - CR605*/
                no_bottom% = 1%
L00500:
/* (\AWD022) */
              gosub check_cont                   /*  (EDW007)   */
REM              if cont% = 1% then goto L00410     /*  (EWD007)   */

              goto L60100
L00410:
                 no_bottom% = 1%
                 cont_head% = 1%
              goto L60100

L60000:    if rk_slot% <= rk_max% then goto L60200
L60100:       rk_slot% = 1%                      /* Start Slot (1) New Rack*/
              rk_number% = rk_number% + 1%       /* Increment Rack Number */

L60200:    rk_loc$ = "0"                         /* Set To Top            */
           gosub check_argon
           gosub check_krypton                    /* (AWD015)             */
           convert rk_number% to rk_number$, pic(0000)
                                                 /* Slot Incremented after*/

           convert pass% to pass$, pic(###)
           rk_barcode$  = str(rm_rec$(),22%,9%)/* Glass Barcode (PAR000)*/
REM        IF RK_SLOT% = 0% THEN RK_SLOT% = 1%  /* ???? @@@ */
           convert rk_slot% to rk_slot$, pic(00) /* Update for Tops Only  */

/* CR2406  Moved code */
                           
           gosub create_rack_rec
        return

        create_rack_rec
           init(" " ) rk_rec$
           str(rk_top$,1%,3%)  = rk_rck_type$
         str(rk_top$,4%,9%)  = str(rm_rec$(),22%,9%)/* Glass Barcode (PAR000)*/
           str(rk_top$,13%,6%) = rk_date$            /* Production Date    */
           str(rk_top$,19%,4%) = rk_number$          /* Rack Number Ass    */
           str(rk_top$,23%,1%) = rk_loc$             /* Rack Loc T/B       */
           str(rk_top$,24%,2%) = rk_slot$            /* Rack Slot No.      */
           str(rk_top$,26%,1%) = rk_type$            /* Rack Type Code     */
          str(rk_top$,27%,5%) = str(rm_rec$(),242%,5%)/* Prod Seq No.(PAR000)*/

           check_dept$ = str(rm_rec$(),249%,3%)      /* Dept Code  (PAR000)*/
           gosub check_limits                        /* Max Size for Rack  */

REM        IF PASS% > 4% THEN STR(RK_TOP$,24%,5%) = "000" & RK_SLOT$

REM        IF PATIO$ <> "0" THEN STR(RK_TOP$,24%,5%) = "000" & RK_SLOT$
REM        IF PASS% < 5% OR STR(RK_TOP$,24%,5%) <> "AAAAA" THEN GOTO SKIP_BREAK

           if patio$ = "0" then goto skip_break
          str(rk_top$,27%,5%) = str(rm_rec$(),242%,5%)/* Prod Seq No.(PAR000)*/
skip_break:
           str(rk_top$,32%,3%) = str(rm_rec$(),249%,3%)/* Dept Code(PAR000)*/
           str(rk_top$,35%,10%)= rk_specials$        /* Special Flags      */
           str(rk_top$,45%,3%) = rk_model$           /* Model Code         */
           str(rk_top$,48%,2%) = rk_glass$           /* Glass Code         */
           str(rk_top$,50%,8%) = rk_time$            /* Time Rack Assig    */
           str(rk_top$,58%,3%) = rk_userid$          /* Assigned by User   */
           str(rk_top$,61%,1%) = rk_status$          /* Stat 0 or 1        */
           str(rk_top$,62%,1%) = rk_stock$           /* 0, 1, or 2         */
           str(rk_top$,63%,2%) = intercept$          /* (AWD021)           */
REM           str(rk_top$,65%,64%) = rk_filler$         /* Filler          */
                                                     /* (PAR000)           */
           if str(rm_rec$(),30%,1%) < "5" then t_cnt% = t_cnt% + 1%         ~
                                        else b_cnt% = b_cnt% + 1%
           str(rk_bot$,1%,128%) = str(rk_top$,1%,128%)
/* CR Proj CR2140 CR2218 CR2412 */
           if schema% <> 2%  or ~
             (rk_dept$ = "059" and schema% = 2%) or ~    
             (rk_dept$ = "058" and schema% = 2%) or ~
             (rk_dept$ = "029" and schema% = 2%) or ~
             (rk_dept$ = "070" and schema% = 2%) or ~
             (rk_dept$ = "071" and schema% = 2%) or ~
             (rk_dept$ = "027" and schema% = 2%)    ~
             then     /*AWD024*/ ~
              str(rk_bot$,23%,1%) = "1"           /* Set for Bottom       */

          if str(rm_rec$(),30%,1%) < "5" then rk_rec$ = rk_top$            ~
                                         else rk_rec$ = rk_bot$

        return
/* (AWD021) */
/* pwww   t e m p   need to talk with Christie about this!!   */

            if pass% >=1% and pass% <= 4% then gls_type$ = "000"
            if pass% >=5% and pass% <= 8% then gls_type$ = "001"
            if pass% >=15% and pass% <= 16% then gls_type$ = "002"
            rk_intercept$ = "000" & intercept$

           check_dept$ = str(rm_rec$(),249%,3%)      /* Dept Code  (PAR000)*/
           gosub check_limits                        /* Max Size for Rack  */

REM        IF PASS% > 4% THEN STR(RK_TOP$,24%,5%) = "000" & RK_SLOT$

REM        IF PATIO$ <> "0" THEN STR(RK_TOP$,24%,5%) = "000" & RK_SLOT$
REM        IF PASS% < 5% OR STR(RK_TOP$,24%,5%) <> "AAAAA" THEN GOTO SKIP_BREAK

                                                     /* (PAR000)           */
           if str(rm_rec$(),30%,1%) < "5" then t_cnt% = t_cnt% + 1%         ~
                                        else b_cnt% = b_cnt% + 1%
        return

        check_limits
           if str(rk_specials$,3%,1%) = "T" then                             ~
                                            str(rk_top$,27%,1%) = "T"
                                                     /* (EWD002)           */
           limits$ = "N"

           if schema% = 2% then return               /* NTX No Limit       */
                                                     /* (EWD001) New Test  */
           if check_dept$ = "048" or check_dept$ = "008" then                ~
                                                 goto check_limits_048
           if check_dept$ = "047" or check_dept$ = "033" then                ~
                                                 goto check_limits_047
                                                     /* (EWD001) All Others*/
/* (AWD023) */
REM        IF WD > 50.0 OR HT > 50.0 THEN LIMITS$ = "Y"
REM        IF WD > 31.75 AND HT > 31.75 THEN LIMITS$ = "Y"
REM        IF WD > 31.25 AND HT > 31.25 THEN LIMITS$ = "Y" /* (AWD013) */
        if wd > 54.0 or ht > 54.0 then limits$ = "Y"
        if wd > 35.25 and ht > 35.25 then limits$ = "Y"
/* (\AWD023) */
        /*   if limits$ = "Y" then str(rk_top$,27%,5%) = "AAAAA"  */
           if limits$ = "Y" then str(rk_top$,27%,1%) = "A"    /* CR1121  */

                                                      /* Put in Different  */
                                                      /* Rack.             */
           return
        check_limits_048                              /* (EWD001) and 008  */
           if wd <= 36.0 or ht <= 36.0 then return
              limits$ = "Y"
            /*  if limits$ = "Y" then str(rk_top$,27%,5%) = "AAAAA" */
              if limits$ = "Y" then str(rk_top$,27%,1%) = "A"    /* CR1121 */
           return
        check_limits_047                              /* (EWD001) and 033  */
           if wd <= 61.0 or ht <= 61.0 then return
              limits$ = "Y"
           /*   if limits$ = "Y" then str(rk_top$,27%,5%) = "AAAAA"  */
              if limits$ = "Y" then str(rk_top$,27%,1%) = "A"    /* CR1121 */           
           return

/* <AWD017> */
        check_sdl
           if str(rk_sub_part$,8%,1%) = "1" and str(rk_sub_part$,9%,1%) <> "2" ~
                then str(rk_specials$,6%,1%) = "B"   /* CR2474 bug on SDL */
                              
/* CR2474 Set Valance, cannot be both SDL and Valance */ 
           if str(rk_liting$,1%,1%) = "V"  then str(rk_specials$,6%,1%) = "V"
     
        return
/* </AWD017> */
/* CR2296 */
        check_stc
           init(" ") readkey$, desc$
           stc% = 0%
           str(readkey$,1%,9%) = "PLAN STC"
           str(readkey$,10%,15%) = rk_glass$
           read #1,key = readkey$, eod goto L63010
              stc% = 1%  
L63010: return

        check_argon
/*CR2020*/ if str(rk_sub_part$,7%,1%) = "1" then goto L64005  

           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "PLANARGON"
           str(readkey$,10%,15%) = rk_glass$
           read #1,key = readkey$, eod goto L64010
              str(rk_specials$,1%,1%) = "A"
              goto L64010
              
L64005:    str(rk_specials$,1%,1%) = "A"
L64010: return

        check_special_glass
REM           if rk_glass$ = "89" then str(rk_specials$,2%,1%) = "S"
                                                     /*  (EWD008)  */
           if rk_glass$ = "89" then str(rk_specials$,4%,1%) = "Z"
        return

        check_tempered                         /* (EWD002)            */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "PLAN TEMP"
           str(readkey$,10%,15%) = rk_glass$
           read #1,key = readkey$,using L00010, desc$, eod goto L64050
                 str(rk_specials$,3%,1%) = "T"    /* CR2035 */ 
REM              if str(desc$,1%,1%) = "T" then str(rk_specials$,3%,1%) = "T" ~
REM                                        else str(rk_specials$,3%,1%) = "L"
           if rk_glass$ = "89" then str(rk_specials$,3%,1%) = " "
                                               /* No Slot for Laminate*/
L64050: return                                 /* (EWD002)            */

        check_diamond
REM IF RK_LITING$ = "97" OR RK_LITING$ = "98"THEN STR(RK_SPECIALS$,4%,1%) = "D"
                                                  /*  (EWD008)  */
           if rk_liting$ = "97" or rk_liting$ = "98" then         ~
                                    str(rk_specials$,4%,1%) = "Z"

           rk_liting% = 0%                 /* Special Glass     */
           convert rk_liting$ to rk_liting%, data goto L64055
L64055:
REM IF RK_LITING% > 82% AND RK_LITING% < 89% THEN STR(RK_SPECIALS$,4%,1%) = "D"
REM IF RK_LITING% = 99% THEN STR(RK_SPECIALS$,4%,1%) = "D"

                                                    /*  (EWD008)  */
                                                    /*  (EWD012) - BEG */
REM IF RK_LITING% > 82% AND RK_LITING% < 89% THEN STR(RK_SPECIALS$,4%,1%) = "Z"

           if rk_liting% > 82% and rk_liting% < 87% then          ~
                                    str(rk_specials$,4%,1%) = "Z"

           if rk_liting% > 96% and rk_liting% <= 99% then         ~
                                    str(rk_specials$,4%,1%) = "Z"

REM  IF RK_LITING% = 99% THEN STR(RK_SPECIALS$,4%,1%) = "Z"
                                                    /*  (EWD012) - END */

        return

        check_sunclean                         /* (EWD004)            */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "PLAN SUNC"
           str(readkey$,10%,15%) = rk_glass$
           read #1,key = readkey$,using L00010, desc$, eod goto no_sunclean

              str(rk_specials$,5%,1%) = "S"
        no_sunclean
        return                                 /* (EWD004)            */

        check_cont                             /* (EWD007)            */
           cont% = 0%
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLAN CONT"
           str(readkey$,10%,15%) =  rk_model$
           read #1, key = readkey$, eod goto not_cont
               cont% = 1%
        not_cont
        return                                 /*  (EWD007)            */

        check_slider
/* CR2406 added slid3 and pulling description 2 */        
           slid% = 0%  : slid3% = 0%
           init(" ") readkey$, desc$, desc2$
           str(readkey$,1%,9%)   = "GLASS07"
           str(readkey$,10%,15%) =  rk_model$
           read #1, key = readkey$, using L00015, desc2$, eod goto not_slid
L00015:           FMT POS(57), CH(30)
               slid% = 1%
               if str(desc2$,10%,1%) = "3"  then slid3% = 1%
        not_slid
        return

        check_double                           /* (EWD009)           */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "PLAN DBLE"
           str(readkey$,10%,15%) = rk_glass$
           read #1,key = readkey$,using L00010, desc$, eod goto no_double

              str(rk_specials$,4%,1%) = "D"
        no_double
        return                                 /* (EWD009)           */

        check_obscure                          /* (EWD010)  BEG       */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "GED 001  "
           str(readkey$,10%,15%) = rk_glass$
           read #1,key = readkey$,using L00010, desc$, eod goto no_obscure

              if str(desc$,4%,2%) = "OB" or str(desc$,7%,2%) = "OB" ~
                           then gosub spec_obscure
              if str(desc$,4%,2%) = "OT" or str(desc$,7%,2%) = "OT" ~
                           then gosub spec_obscure

        no_obscure
        return

        spec_obscure
              if str(rk_specials$,1%,1%) <> " " then goto check_3
                    str(rk_specials$,1%,1%) = "O"
                    return
check_3:
              if str(rk_specials$,3%,1%) <> " " then goto check_4
                    str(rk_specials$,3%,1%) = "O"
                    return
check_4:
              if str(rk_specials$,4%,1%) <> " " then goto check_5
                    str(rk_specials$,4%,1%) = "O"
                    return
check_5:
              if str(rk_specials$,5%,1%) <> " " then goto check_obs_done
                    str(rk_specials$,5%,1%) = "O"
                    return


        check_obs_done
        return                                 /* (EWD010)   END      */


        check_ups                              /* (AWD014)            */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "PLAN UPS "
           str(readkey$,10%,15%) = rk_hows$
           read #1,key = readkey$,using L00010, desc$, eod goto no_ups

              str(rk_specials$,5%,1%) = "U"
        no_ups
        return                                 /* (AWD014)            */

        check_ultra                            /* (AWD018) */
REM  !IF STR(RM_REC$(),297%,1%) = "1" THEN STR(RK_SPECIALS$,7%,1%) = "Y"
            if str(rm_rec$(),299%,1%) = "1" then str(rk_specials$,7%,1%) = "Y"

        return

        check_intercept                            /* (AWD021) */
             intercept$ = str(rm_rec$(),307%,2%)
             convert intercept$ to intercept%, data goto lookup_intercept
        return

        check_bottom
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "GLASS01  "
           str(readkey$,10%,15%) = rk_model$
           read #1,key = readkey$, desc$, eod goto no_bottom

              no_bottom% = 1%
        no_bottom
        return
                                               /* (EWD006)   BEGIN    */
        find_glass
           glass% = 0%
        find_glass_nxt
           gosub check_work
           if wrk% <> 1% then return
           init(" ") rk_key$, rk_time$
           str(rk_key$,1%,3%) = rk_rck_type$
           str(rk_key$,4%,9%) = str(rm_rec$(),22%,9%)
           read #3, hold, key = rk_key$, eod goto no_rack_glass

             goto find_glass_nxt                  /* Skip if found only */
                                                  /* Allocate once      */
        return
        no_rack_glass
           glass% = 1%
        return



        read_glass
          init(" ") rm_key$
          rm_key$ = warranty$

        read_glass_next
          read #2, key > rm_key$, using L00210, rm_rec$(),                ~
                   eod goto finished_clear

            rm_key$ = str(rm_rec$(),22%,12%)
            if str(rm_key$,1%,8%) <> warranty$ then goto finished_clear
               put #5, using L00210, rm_rec$()

               write #5, eod goto read_glass_next

            goto read_glass_next

        finished_clear
REM          init(" ") rm_key$
REM          rm_key$ = warranty$
REM        temp_next
REM          read #4, key > rm_key$, using L00210, rm_rec$(),                ~
REM                   eod goto finished_temp
REM            rm_key$ = str(rm_rec$(),22%,12%)
REM            if str(rm_key$,1%,8%) <> warranty$ then goto finished_temp
REM               put #5, using L00210, rm_rec$()

REM               write #5, eod goto temp_next
REM               goto temp_next
REM        finished_temp
        return


        check_work
          wrk% = 0%
          patio$ = "0"
          intercept$ = " "

           read #5,key > rm_key$, using L00210, rm_rec$(), eod goto no_clear
L00210:        FMT 2*CH(256)

           rm_dept$  = str(rm_rec$(),249%,3%)
           rm_patio$ = str(rm_rec$(),300%,1%)
           if rm_patio$ = " "  then rm_patio$ = "0"
           if str(rm_rec$(),249%,3%) = "023" and                          ~
              str(rm_rec$(),300%,1%) = "1" then patio$ = "1"
           if str(rm_rec$(),249%,3%) = "023" and                          ~
              str(rm_rec$(),300%,1%) = "2" then patio$ = "2"

           if str(warranty$,1%,8%) <> str(rm_rec$(),22%,8%) then goto no_clear
           rm_key$ = str(rm_rec$(),22%,12%)
           intercept$ = str(rm_rec$(),307%,2%)
           convert intercept$ to intercept%, data gosub lookup_intercept

        wrk% = 1%
        return
        no_clear
          no_bottom% = 1%      /* For exit */
        return


L65000: rk_err% = 1%
        exit_sub
          gosub delete_work
        end

        update_table
           rk_slot% = 0%
REM           IF PASS% > 4% THEN RK_SLOT% = 1%
           sav_date$ = " "
           convert rk_number% to rk_number$, pic(0000)

           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLAN RACK"
           str(readkey$,10%,15%) = rk_date_fm$
           read #1,hold,key = readkey$, eod goto L65030
              put #1, using L65020, rk_number$
L65020:         FMT POS(25), CH(4)
              rewrite #1
              goto exit_sub                       /* Finished          */

L65030:    put #1, using L65040, readkey$, rk_number$," - Last Rack No. Used"
L65040:      FMT CH(24), CH(4), CH(21)
           write #1, eod goto L65000
           goto exit_sub                          /* Finished          */

L65050: if no_bottom% = 1% then goto exit_sub     /* Single Racks      */
                                                  /* Top/Bottoms       */
L65055:                                           /* (EWD011)          */
           x% = 0%
           x% = t_cnt% + b_cnt%
           if x% = 0% then goto exit_sub          /* No Glass          */

           rk_barcode$ = str(rk_key$,4%,9%)
/*(AWD022) */
           str(rk_top$,1%,3%) = rk_rck_type$
/* CR9999 */
/* CR2406 do not reset barcode on 3 lite slider */
           if (schema% = 1% and rk_dept$ = "053" and slid3% = 1%) or   ~
              (schema% = 2% and rk_dept$ = "058" and slid3% = 1%) then ~
               goto slidskip
           str(rk_top$,4%,9%) = str(rk_barcode$,1%,8%) & "0"/* Set Top */
           str(rk_bot$,4%,9%) = str(rk_barcode$,1%,8%) & "5"/* Set Bot */
slidskip:
           str(rk_top$,23%,1%) = "0"           /* Set for Top          */
/* CR Proj CR2140 CR2218 CR2412 */
           if schema% <> 2%  or ~
             (rk_dept$ = "059" and schema% = 2%) or ~     
             (rk_dept$ = "058" and schema% = 2%) or ~     
             (rk_dept$ = "029" and schema% = 2%) or ~
             (rk_dept$ = "070" and schema% = 2%) or ~
             (rk_dept$ = "071" and schema% = 2%) or ~
             (rk_dept$ = "027" and schema% = 2%)    ~
             then     /*AWD024*/              ~
              str(rk_bot$,23%,1%) = "1"           /* Set for Bottom       */

                                               /* Test Width/Height Max*/

                                               /* Width Height         */
           if t_cnt% <> 0% then goto L65060
/*(AWD022)*/
              str(rk_top$,4%,1%)  = "A"           /* Blank Top         */
              str(rk_top$,5%,8%)  = str(rk_barcode$,1%,8%)
              str(rk_top$,27%,5%) = "XXXXX"       /* Blank Seq No.     */
              str(rk_top$,35%,10%)= "          "  /* Special Flags     */
              str(rk_top$,61%,1%) = "1"           /* Set to Complete   */
                                                  /* Error Both Blank  */
L65060:    if b_cnt% <> 0% then goto L65070
              str(rk_bot$,4%,1%)  = "A"           /* Blank Bottom      */
              str(rk_bot$,5%,8%)  = str(rk_barcode$,1%,8%)
              str(rk_bot$,27%,5%) = "XXXXX"       /* Blank Seq No.     */
              str(rk_bot$,35%,10%)= "          "  /* Special Flags     */
              str(rk_bot$,61%,1%) = "1"           /* set to Complete   */

L65070:
        write #3, using L00300, rk_top$, eod goto L65000
/*AWD024 + */
/* CR Proj CR2140 CR2218 CR2412 */
           if schema% <> 2% or (schema% = 2% and rk_dept$ = "059") or ~
             (rk_dept$ = "058" and schema% = 2%) or ~
             (rk_dept$ = "029" and schema% = 2%) or ~
             (rk_dept$ = "070" and schema% = 2%) or ~    
             (rk_dept$ = "071" and schema% = 2%) or ~
             (rk_dept$ = "027" and schema% = 2%)    ~
               then goto not_tx2             /*AWD024*/
              rk_slot% = rk_slot% + 1%           /*AWD024*/
           if rk_slot% <= rk_max% then goto max_ok2
              rk_slot% = 1%                      /* Start Slot (1) New Rack*/
              rk_number% = rk_number% + 1%       /* Increment Rack Number */
              convert rk_number% to rk_number$, pic(0000)
              str(rk_bot$,19%,4%) = rk_number$          /* Rack Number Ass    */
max_ok2:      convert rk_slot% to rk_slot$, pic(00) /* Update for Tops Only */
              str(rk_bot$,24%,2%) = rk_slot$            /* Rack Slot No.    */

not_tx2:
/*AWD024 - */

        write #3, using L00300, rk_bot$, eod goto L65000

        rk_slot% = rk_slot% + 1%
        goto exit_sub                             /* Finished          */

        clear_data
           if del% <> 1% then return
           rk_key1$ = all(hex(00))

           str(rk_key1$,1%,6%) = rk_date$
        clear_data_nxt
           read #3, hold, key 1% > rk_key1$, using Lxxx10, rk_key1$,       ~
                                    eod goto clear_data_done

Lxxx10:             FMT POS(13), CH(14)

                    if str(rk_key1$,1%,6%) <> str(rk_date$,1%,6%)         ~
                              then goto clear_data_done
                    delete #3
             goto clear_data_nxt
        clear_data_done
        return

        clear_data_awdrack

           if del% <> 1% then return
           init(" ") rk_key1_new$

           rk_key1_new$ = rk_date$
        clear_data_nxt_new
           read #5, hold, key 1% > rk_key1_new$, using Lxxx20, rk_key1_new$,  ~
                                    eod goto clear_data_done_new

Lxxx20:             FMT POS(18), CH(22)

                    if str(rk_key1_new$,1%,6%) <> str(rk_date$,1%,6%)         ~
                              then goto clear_data_done_new
                    delete #5
             goto clear_data_nxt_new
        clear_data_done_new
        return


* (AWD015)
        check_krypton
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "PLANKRYPT"
           str(readkey$,10%,15%) = rk_glass$
           read #1,key = readkey$, eod goto no_krypton
              str(rk_specials$,1%,1%) = "K"
no_krypton:
        return



        check_pass


            skip% = 1%
/* (AWD019) */
REM            IF PASS% = 1% THEN SKIP% = 0%
REM            IF PASS% = 1% THEN RETURN
/* (AWD021)  */
            if pass% = 1% then gosub check_pass1
            if pass% = 2% then gosub check_pass2
            if pass% = 3% then gosub check_pass3
            if pass% = 4% then gosub check_pass4
            if pass% = 5% then gosub check_pass5
            if pass% = 6% then gosub check_pass6
/*CR00604 + */
            if pass% = 7% then gosub check_pass7
            if pass% = 8% then gosub check_pass8
            if pass% = 9% then gosub check_pass9
/*CR00604 - */
            if pass% = 10% then gosub check_pass10
            if pass% = 11% then gosub check_pass11
            if pass% = 12% then gosub check_pass12

/*CR00604*/ if pass% = 13% then gosub check_pass13
            if pass% = 14% then gosub check_pass14
            if pass% = 15% then gosub check_pass15
            if pass% = 16% then gosub check_pass16
            if pass% = 17% then gosub check_pass17
            if pass% = 18% then gosub check_pass18

            if pass% = 19% then gosub check_pass19
            if pass% = 20% then gosub check_pass20
            if pass% = 21% then gosub check_pass21
            
            if pass% = 22% then gosub check_pass22   /* CR2296 */

/* (AWD019) */
REM pass = 1 Non Ultra Glass All
REM pass = 2 Double, Tempered, no ultra
REM pass = 3 All ULTRA
REM pass = 4 ultra Double, Tempered
REM pass = 5 VPD Stock
REM pass = 6 VPD Custom
         return

         check_pass1
            if intercept$ <> "01" then return /*skip*/
            if stc% = 1% and schema% = 1% then return        /* skip CR2218 */
            skip% = 0%
         return
         check_pass2
            if intercept$ <> "02" then return /*skip*/
            if stc% = 1% and schema% = 1% then return        /* skip CR2218 */          
            skip% = 0%
         return
         check_pass3
            if intercept$ <> "03" then return /*skip*/
            if stc% = 1% and schema% = 1% then return        /* skip CR2218 */          
            skip% = 0%
         return
         check_pass4
            if intercept$ <> "04" then return /*skip*/
            if stc% = 1% and schema% = 1% then return        /* skip CR2218 */           
            skip% = 0%
         return
         check_pass5
            if intercept$ <> "05" then return /*skip*/
            if stc% = 1% and schema% = 1% then return        /* skip CR2218 */           
            skip% = 0%
         return
         check_pass6
            if intercept$ <> "06" then return /*skip*/
            if stc% = 1% and schema% = 1% then return        /* skip CR2218 */          
            skip% = 0%
         return
        check_pass7
            if intercept$ <> "01" then return /*skip*/
            if stc% = 1% and schema% = 1% then return       /* skip CR2218 */       
            if str(rk_specials$,4%,1%) = "D" and                          ~
               str(rk_specials$,3%,1%) <> "T" then skip% = 0%
         return
         check_pass8
            if intercept$ <> "02" then return /*skip*/
            if stc% = 1% and schema% = 1% then return       /* skip CR2218 */               
            if str(rk_specials$,4%,1%) = "D" and                          ~
               str(rk_specials$,3%,1%) <> "T" then skip% = 0%
         return
         check_pass9
            if intercept$ <> "03" then return /*skip*/
            if stc% = 1% and schema% = 1% then return       /* skip CR2218 */            
            if str(rk_specials$,4%,1%) = "D" and                          ~
               str(rk_specials$,3%,1%) <> "T" then skip% = 0%
         return
/*CR00604 + */
        check_pass7_TX
            if intercept$ <> "01" then return /*skip*/
            if str(rk_specials$,4%,1%) = "D" then skip% = 0%
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass8_TX
            if intercept$ <> "02" then return /*skip*/
            if str(rk_specials$,4%,1%) = "D" then skip% = 0%
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass9_TX
            if intercept$ <> "03" then return /*skip*/
            if str(rk_specials$,4%,1%) = "D" then skip% = 0%
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
/*CR00604 - */
         check_pass10
            if intercept$ <> "04" then return /*skip*/
            if stc% = 1% and schema% = 1% then return       /* skip CR2218 */               
            if str(rk_specials$,4%,1%) = "D" and                          ~
               str(rk_specials$,3%,1%) <> "T" then skip% = 0%
         return
         check_pass11
            if intercept$ <> "05" then return /*skip*/
            if stc% = 1% and schema% = 1% then return       /* skip CR2218 */              
            if str(rk_specials$,4%,1%) = "D" and                          ~
               str(rk_specials$,3%,1%) <> "T" then skip% = 0%
         return
         check_pass12
            if intercept$ <> "06" then return /*skip*/
            if stc% = 1% and schema% = 1% then return       /* skip CR2218 */                  
            if str(rk_specials$,4%,1%) = "D" and                          ~
               str(rk_specials$,3%,1%) <> "T" then skip% = 0%
         return
/*CR00604 + */
        check_pass13
            if intercept$ <> "01" then return /*skip*/
/*          if str(rk_specials$,4%,1%) = "D" then skip% = 0%             */
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass14
            if intercept$ <> "02" then return /*skip*/
/*          if str(rk_specials$,4%,1%) = "D" then skip% = 0%             */
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass15
            if intercept$ <> "03" then return /*skip*/
/*          if str(rk_specials$,4%,1%) = "D" then skip% = 0%             */
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass16
            if intercept$ <> "04" then return /*skip*/
/*          if str(rk_specials$,4%,1%) = "D" then skip% = 0%             */
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass17
            if intercept$ <> "05" then return /*skip*/
/*          if str(rk_specials$,4%,1%) = "D" then skip% = 0%             */
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass18
            if intercept$ <> "06" then return /*skip*/
/*          if str(rk_specials$,4%,1%) = "D" then skip% = 0%             */
            if str(rk_specials$,3%,1%) = "T" then skip% = 0%
         return
         check_pass19
            if tripane% = 1% then skip% = 0%
         return


/* (AWD020) */
         check_pass20
            if patio$ <> "2" then return /*Skip*/
            rk_type$   = "2"                /* Top Only (20) Slots   */
            rk_max%    = 20%                /* Max of 20 Slots       */
            no_bottom% = 1%                 /* Tops Only, no Bottoms */
            skip% = 0%
         return
         check_pass21
            if patio$ <> "1" then return /*Skip*/
            rk_type$   = "2"                /* Top Only (20) Slots   */
            rk_max%    = 20%                /* Max of 20 Slots       */
            no_bottom% = 1%                 /* Tops Only, no Bottoms */
            skip% = 0%
         return
/* (/AWD020) */

         check_pass22
           if stc% = 0% then return
           rk_type$ = "0"
           rk_max% = 25%       /* CR2792 from 20 to 25 */
           /* CR2792 */
           if rk_dept$ = "047" then rk_max% = 20%
           no_bottom% = 0%
           skip% = 0%
         return

/* (AWD021)  */
        lookup_intercept
          init(" ") readkey$, desc$, intercept$
          intercept% = 1%
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = rk_model$
          str(readkey$,13,2) = rk_glass$

           read #1, key = readkey$, using L00010, desc$,  ~
                                   eod goto no_intercept_glass

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = rk_model$
          str(readkey$,13,2) = str(rk_glass$,1,1) & "*"

           read #1, key = readkey$, using L00010, desc$,  ~
                                   eod goto no_intercept_all

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") readkey$
           str(readkey$,1,9)  = "INTERCEPT"
           str(readkey$,10,3) = rk_model$
           str(readkey$,13,2) = "**"

           read #1, key = readkey$, using L00010, desc$,  ~
                                   eod goto intercept_done

               convert str(desc$,1,2) to intercept%, data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return
/* (AWD021) */
/* (AWD022) */
        check_triplepane
         tripane% = 0%
         init(" ") readkey$
         str(readkey$,1%,9%)   = "PLANTRIPL"
         str(readkey$,10%,15%) = rk_glass$
         read #1,key = readkey$, eod goto notriplepane
              tripane% = 1%
        notriplepane
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"
            call "WORKOPN2" (#5,mode$, 500%, f2%)
            if f2% <> 0% then goto L64485
        return
L64485:     call "SHOSTAT" ("Error - Cannot Open (APCWORK4)") : stop
        return
        delete_work
            call "FILEBGON" (#5)
        return

