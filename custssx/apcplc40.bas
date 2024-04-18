        REM *************************************************************~
            *  Note - Sash's and Parts are Not counted in Totals.       *~
            *         (TT%) is Used for Test. Line No. (2450)           *~
            *                                                           *~
            *  Program Name      - APCPLC40 - Replace Old (APCPLN2B)    *~
            *  Creation Date     - 01/07/97                             *~
            *  Last Modified Date- 08/19/05                             *~
            *  Description       - For a Specified Department Calculate *~
            *                      the Scanned or Not Scanned Units for *~
            *                      Specified Production Day.            *~
            *                                                           *~
            *  Subroutine Used By - (APCSCANN), (APCPLA40), (APCPLB40)  *~
            *                       (APCEFFTB)                          *~
            *                                                           *~
            *  Special Notes - DT_DEPT$ and SC_DTE$ are always passed   *~
            *                  to the Subroutine when called. Department*~
            *                  Code is Required. DT_DEPT$="ALL" is not  *~
            *                  a valid Option. It will not work.        *~
            *                                                           *~
            *                - When P_FLG% = 1% then the Value of       *~
            *                  P_MAX% is Set and also Products are      *~
            *                  loaded into P_MOD$() from Efficiency     *~
            *                  Table. Wood Surround is Loaded and based *~
            *                  on code from 'APC WOOD'(APCEFFTB)        *~
            *                                                           *~
            *                - When P_FLG% = 0% then the Product Models *~
            *                  are Loaded from the Table 'PLAN DEPT'.   *~
            *                  The Value of P_MAX% is set by 'LOAD_PROD'*~
            *                  Wood surround is then base on Model.     *~
            *                  (APCPLA40, APCPLB40)                     *~
            *                                                           *~
            *                - When P_SCAN% = 0% Display is for         *~
            *                                    Not Scanned Units.     *~
            *                  When P_SCAN% = 1% Display is for         *~
            *                                    Scanned Units.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/07/97 ! New Program for (APC) - With all Mods    ! RHH *~
            *          ! Assoc. with (APCPLN2B)                   !     *~
            * 01/19/97 ! Mod to Change the Scan to Key from the   ! RHH *~
            *          !   audit file (APCPLNAD). Big Big         !     *~
            * 01/31/97 ! Mod to Correct Problems and to Add some  ! RHH *~
            *          !   Debug Shostat Statements.              !     *~
            * 08/21/97 ! Mod to Correct Problems and to Add some  ! RHH *~
            *          !   Debug Shostat Statements.              !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/27/98 ! Y2K                                      ! LDJ *~
            * 02/28/00 ! (EWD004) - Mod for change in parts with  ! CMG *~
            *          !            code in CLMR or WALLWIDTH     !     *~ 
            * 03/29/00 ! (EWD005) - Mod to change for Screens Dept! CMG *~
            *          !            with way calcs scanned units. !     *~ 
            * 09/17/02 ! (EWD006) Fix for Special Shapes Grid Code! CMG *~
            * 07/21/04 ! (EWD007) Mod to make dept '054' calc unit! CMG *~
            *          !            just like '044'               !     *~
            * 08/19/05 ! (AWD008) Mod to count wood received units! CMG *~
			* 04/05/23 ! CR3292 Remove 0 dt_charge logic          ! RDB *~
            *************************************************************

            sub "APCPLC40" ( dt_dept$,   /* Specified Department Code  */~
                             sc_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day       */~
                             p_shft$,    /* Shift Code or (AL)         */~
                             sc_load$,   /* Production Load or (ALL)   */~
                             ed_load$,   /* Ending Prod Load           */~
                             p_mod$(),   /* Department Products        */~
                             p_unt%(),   /* Product Units + Samples    */~
                             p_unts%(),  /* Sample Products Only       */~
                             p_untss%(), /* Charged Sash Products Only */~
                             p_untpp%(), /* Charged Part Products Only */~
                             p_val(),    /* Product Dollar Value       */~
                             p_mrp(),    /* (5) Costing Buckets        */~
                             p_max%,     /* Max Number of Products     */~
                             p_flg%,     /* 0% = Load, 1% = No Load    */~
                             p_scan%,    /* 0% = Not SCANNED,1%=Scanned*/~
                             p_screen%,  /* 0% = Yes, 1% = No - Display*/~
                             #1,         /* (APCPLNDP) Master Dept.    */~
                             #2,         /* (APCPLNDT) Prod. Tracking  */~
                             #3,         /* (APCPLNAD) Planning Audit  */~
                             #4 )        /* (GENCDSIN) Master Code Tab */

        dim                              /* Subroutine - Variables     */~
            ad_key$33, ad_rec$64,        /* Audit Key And Record       */~
            ad_st$2, ad_time$8,          /* Audit Status AND Time      */~
            ad_dept$3, ad_proc$2,        /* Audit Dept and Process     */~
            dt_key$24, db$100,           /* (APCPLNDT) - Primary Key   */~
            dt_rec$256, xx_dte$6,        /* Scanning Record, Date      */~
            dt_dept$3, dt_dept_d$30,     /* Department Code            */~
            sc_dte$6,                    /* Specified Production Date  */~
            sc_load$5, ed_load$5,        /* Beg/End Load Number or (ALL*/~
            p_shft$2, dt_shft_d$30,      /* Specified Shift Code       */~
            p_mod$(306%)3,               /* Products for Department    */~
            p_unt%(306%,3%),             /* Scanned Units each Product */~
            p_unts%(306%,3%),            /* Scanned Units Samples Only */~
            p_untss%(306%,3%),           /* Scanned Units Charged Sashs*/~
            p_untpp%(306%,3%),           /* Scanned Units Charged Parts*/~
            p_unt$(306%)6,               /* Screen Display Scanned Unit*/~
            p_val(306%,3%),              /* Scanned Units Dollar Value */~
            p_val$(306%)10,              /* Screen Display Avg S.O. $$ */~
            p_mrp(6%,3%), p_mrp$(6%)10,  /* Mat,Lab,Ovr,Frt,Disc,Total */~
            mrp(5%),                     /* (5) Scanning Buckets Cost  */~
            bg_dte$6, ed_dte$6,          /* Starting/Ending Prod. Date */~
            jdate1$7, jdate2$7,          /* Save Beg/End Julian Date            (Y2K, LDJ)*/~
            dt_date$8,                   /* Starting Production Date   */~
            dt_sale$10, cnt$24,          /* Total Sales Dollars        */~
            dt_shft$2,                   /* Tracking Shift             */~
            ap$2,                        /* 'AM' or 'PM' From time     */~
            model$3,                     /* Model, Screen Code, Product*/~
            dt_sash$1, dt_prt$1,         /* Sash and Part Flags        */~
            dt_part$25,                  /* Part Number                */~
            dt_samp$1, dt_wood$3,        /* Sample                     */~
            readkey$24,                  /* Use for Table Lookup       */~
            hdr$45, t_unit$6, hdr1$40,   /* Screen Header              */~
            cursor%(2%), hdr2$22,        /* Cursor Location for Edit   */~
            i$(24%)80,                   /* SCREEN IMAGE               */~
            pf$(3%)79,                   /* PF Key Description         */~
            testdate$10, testdate2$10,   /* Working Date Variable               (Y2K, LDJ) */~
            pfkeys$32                    /* PF Key Values              */

            sc_max% = 305%               /* Max Entries for Screen - 1%*/
            hdr$ = "Dept. (XXX) Total Scanned Units     (XXXXXX) "
            if p_scan% = 0% then                                         ~
               hdr$ = "Dept. (XXX) Total Not Scanned Units (XXXXXX) "
            hdr2$ = "NOT Scanned date    : "
            if p_scan% <> 0% then hdr2$ = "Scanned Date        : "
            mat p_unt%   = zer       /* Units by Shift */
            mat p_unts%  = zer       /* Units by Shift for Samples     */
            mat p_untss% = zer       /* Units by Shift for Chg Sashs   */
            mat p_untpp% = zer       /* Units by Shift for Chg Parts   */
            mat p_val    = zer       /* Avg S.O. $$ for Units by Shift */
            mat p_mrp    = zer       /* Costing All Products by Shift  */
            if p_flg% = 0% then gosub load_prod /* Skip for Efficiency */
            gosub calc_scanned_units
            k% = 0%
            gosub display_codes
            goto exit_sub

        load_prod
           call "SHOSTAT" ("Loading Products for Dept ("&dt_dept$&")" )
           init(" ") p_mod$(), dt_key$
           p_max% = 0%                             /* Department Spec. */
           str(dt_key$,1%,3%) = dt_dept$           /* by the Call Prog.*/
           str(dt_key$,4%,2%) = "01"               /* Set Process Code */
           read #1,key > dt_key$, using L01330 , dt_key$, eod goto L01470
           goto L01340
        load_prod_nxt
           read #1, using L01330 , dt_key$, eod goto L01450
L01330:        FMT POS(11), CH(12)
L01340:    if str(dt_key$,1%,3%) <> dt_dept$ then goto L01450
              if p_max% = 0% then goto L01400
              for j% = 1% to p_max%
                  if p_mod$(j%) = str(dt_key$,8%,3%) then                ~
                                                     goto load_prod_nxt
              next j%
L01400:       p_max% = p_max% + 1%
              j% = p_max%
              if p_max% > sc_max% then p_max% = sc_max%
              p_mod$(j%) = str(dt_key$,8%,3%)
              goto load_prod_nxt
L01450: p_max% = p_max% + 1%             /* Undefined Product Bucket */
        p_mod$(p_max%) = "999"           /* for - Errors.            */
L01470: return

        calc_scanned_units
            yr% = 0%
            testdate2$ = sc_dte$                                                /* (Y2K, LDJ) */
            call "DATFMTC" (testdate2$,yr%,testdate$)                           /* (Y2K, LDJ) */
            convert str(testdate$,1%,4%) to yr%, data goto L01475               /* (Y2K, LDJ) */
L01475:
            leap_yr% = 365%
            if mod(yr%,4%) = 0% then leap_yr% = 366%
                             /* Windows Scanned from 7 AM of Specified */
                             /* Production Day, Until 6 59 AM of the   */
                             /* Next Production Day. 1st,2nd,3rd Shifts*/
            cnt$ = "Units Checked [ XXXXXX ]"
            init(" ") jdate1$, jdate2$, bg_dte$, ed_dte$, ad_key$
            init(" ") p_unt$(), p_val$(), p_mrp$(), db$
                                         /* Julian date Curr Prod Week */
            call "DATE" addr("GJ", str(sc_dte$,,6%), str(jdate1$,,5%), x%)      /* (Y2K, LDJ) */
                                                         /* CONVERT TO */
                                                         /* JULIAN DATE*/
            call "DATJULCV" (jdate1$)                                           /* (Y2K, LDJ) */
            convert str(jdate1$,5%,3%) to j1%, data goto L01590                 /* (Y2K, LDJ) */
L01590:
                                                 /* Current Production */
            cnt% = 0%                            /* Day Julian Date    */
            j2% = j1% + 1%                       /* Tommorow's/Next Day*/
            jdate2$ = jdate1$                    /* Julian Date        */
            if j2% < leap_yr% then goto L01700
               j2% = 001%
            convert str(jdate2$,1%,4%) to rhh%, data goto L01670                /* (Y2K, LDJ) */
L01670:
            convert (rhh% + 1%) to str(jdate2$,1%,4%), pic(0000)                /* (Y2K, LDJ) */

L01700:     convert j1% to str(jdate1$,5%,3%), pic(000)                         /* (Y2K, LDJ) */

            convert j2% to str(jdate2$,5%,3%), pic(000)                         /* (Y2K, LDJ) */
                                               /* Begin with Production*/
            call "DATJULCV" (jdate1$)                                           /* (Y2K, LDJ) */
            call "DATJULCV" (jdate2$)                                           /* (Y2K, LDJ) */
            call "DATE" addr("JG", str(jdate1$,,5%), bg_dte$, x%)               /* (Y2K, LDJ) */
                                                         /* Date and   */
            call "DATE" addr("JG", str(jdate2$,,5%), ed_dte$, x%)               /* (Y2K, LDJ) */
                                                         /*Next Days   */
                                                         /*Prod. Date  */
        REM INIT(" ") DB$
        REM DB$ = "ToDay = " & BG_DTE$ & " Tommorow = " & ED_DTE$
        REM CALL "SHOSTAT" ( DB$ )
        REM STOP
                                          /* 1st Check Specified Prod. */
           str(ad_key$,1%,6%) = bg_dte$   /* date from 7 AM until Mid. */
           if p_scan% = 2% then dt_dept$ = "000"         /*  (AWD008)  */
           str(ad_key$,7%,3%) = dt_dept$  /* Set Department Code       */
           str(ad_key$,10%,2%) = "01"     /* Set Process Code          */
        calc_scanned_nxt
           read #3,key > ad_key$, using L01880, ad_rec$,                   ~
                                               eod goto calc_scanned_done
L01880:        FMT CH(64)
           ad_key$ = str(ad_rec$,19%,33%)
           cnt% = cnt% + 1%
           if mod(cnt%,50%) <> 0 then goto L01950
              convert cnt% to str(cnt$,17%,6%),pic(######)

              print at(02,29);hex(84);cnt$;
L01950: REM INIT(" ") DB$
        REM DB$ = "AUDIT KEY = "& AD_KEY$ & " STATUS = " &               ~
        REM                                   STR(AD_REC$,32%,2%)
        REM CALL "SHOSTAT" (DB$)
        REM STOP
           ad_st$   = str(ad_rec$,32%,2%)              /* Scan Status  */
           xx_dte$  = str(ad_rec$,19%,6%)              /* Scann Date   */
           ad_time$ = str(ad_rec$,52%,8%)              /* Scan Time    */
           ad_dept$ = str(ad_rec$,25%,3%)              /* Scan Dept    */
           ad_proc$ = str(ad_rec$,28%,2%)              /* Scan Process */
           if xx_dte$ > ed_dte$ then goto calc_scanned_done

           if p_scan% = 2% then goto CHECK_PROC          /*  (AWD008)  */
           if dt_dept$ <> ad_dept$ then goto calc_scanned_nxt
CHECK_PROC:
           if ad_proc$ <> "01"     then goto calc_scanned_nxt

           if p_scan% <> 0% then goto L02130
                                                        /* Not Scanned */
              if ad_st$ > "03" and ad_st$ < "12" then goto L02150
                 goto calc_scanned_nxt
L02130:    if p_scan% <> 1% then goto L02135              /* (AWD008)  */
           if ad_st$ <> "12" then goto calc_scanned_nxt     /*Scanned */
                  goto L02150
L02135:    if p_scan% <> 2% then goto L02150              /*  (AWD008)  */
                if ad_st$ <> "13" then goto calc_scanned_nxt

L02150:    init(" ") dt_key$, dt_rec$, dt_part$
           str(dt_key$,1%,18%) = str(ad_rec$,34%,18%)  /* Barcode      */
           str(dt_key$,19%,3%) = str(ad_rec$,25%,3%)   /* Department   */
           str(dt_key$,22%,2%) = str(ad_rec$,28%,2%)   /* Process Code */
           read #2,key = dt_key$, using L02210 , dt_rec$,                  ~
                                                eod goto calc_scanned_nxt
L02210:       FMT CH(256)                           /* Check Load No.  */
           
           dt_part$ = str(dt_rec$,189%,25%)
           if str(sc_load$,1%,3%) = "ALL" then goto L02260
              if str(dt_rec$,1%,5%) < sc_load$ or                        ~
                 str(dt_rec$,1%,5%) > ed_load$ then goto calc_scanned_nxt

L02260:    if p_scan% <> 0% then goto L02280          /* Scanned Product */
              if str(dt_rec$,64%,2%) > "11" then goto calc_scanned_nxt
L02280: REM - All Creteria Met
        REM INIT(" ") DB$
        REM DB$ = "HIT - DETAIL KEY = " & DT_KEY$ & "  STATUS = " &      ~
        REM                                   STR(DT_REC$,64%,2%)
        REM CALL "SHOSTAT" (DB$)
        REM STOP
        REM -
           dt_charge = 0.0
           get str(dt_rec$,133,8%), using L02370 , dt_charge
L02370:        FMT PD(14,4)

           hr% = 0%
           convert str(ad_time$,1%,2%) to hr%, data goto L02410
L02410:
           ap$ = str(ad_time$,7%,2%)          /* Scanned 'AM' or 'PM'  */
           if xx_dte$ = ed_dte$ then goto L02550
                                              /* Check Current Scan Day*/
                                              /* First. 1st, 2nd and   */
                                              /* part of 3rd shift     */
                                              /* 7 AM to 12 Midnight   */
               if ap$ = "PM" then goto L02590   /* Part of Prev Day Prod.*/
                  if hr% < 7% or hr% = 12% then goto calc_scanned_nxt
                  goto L02590
                                              /* Second Check Next Day */
                                              /* Scanned Data. Last    */
                                              /* part of 3rd Shift.    */
                                              /* Midnight to 6 59 'AM' */
L02550:    if ap$ = "PM" then goto calc_scanned_nxt
           if hr% < 7% or hr% = 12% then goto L02590 /* Midnight to 6 59A*/
              goto calc_scanned_nxt                /* Current Day      */

L02590:    dt_sash$  = str(dt_rec$,214%,1%)   /* Sash Flag 0,1,2,3     */
           dt_prt$   = str(dt_rec$,215%,1%)   /* Part Flag (Y) or (N)  */
           dt_samp$  = str(dt_rec$,216%,1%)   /* Samp Flag 0=No,1=S,2=D*/

           tt% = 1%                               /* TT% = 1% Window   */
           if dt_sash$ <> "0" then tt% = 2%       /* TT% = 2% Sashs    */
           if dt_prt$  = "Y" then tt% = 3%        /* TT% = 3% Parts    */
           gosub check_samples                    /*  (EWD004)         */
           if tt% = 1% then goto L02700             /* Count Windows     */
/* CR3292     if dt_charge = 0 then goto calc_scanned_nxt     Do Not Cnt */
                                                  /* No Charge Sashs   */
                                                  /* and Parts         */
L02700:    model$    = str(dt_rec$,189%,3%)
           dt_shft$  = str(dt_rec$,104%,2%)
           if p_scan% = 2% then goto CALC_RECEIVED          /* (AWD008) */
           if p_flg% = 0% then goto L02780         /* Not Efficiency Rpt*/

            if dt_dept$ <> "044" then goto L02780  /* Wood Surround Dept*/
                 dt_wood$  = str(dt_rec$,217%,3%)
                 if dt_wood$ = "000" then goto calc_scanned_nxt
                    model$ = dt_wood$
                                                   /*  (EWD007) - BEG  */
L02780:    if dt_dept$ <> "054" then goto L02785  /* Wood Surround Dept*/
                 dt_wood$  = str(dt_rec$,217%,3%)
                 if dt_wood$ = "000" then goto calc_scanned_nxt
                    model$ = dt_wood$
                                                   /*  (EWD007) - END  */
                                                  /* Shift that Record */
L02785:    shft% = 1%                             /* was Scanned on.   */
           convert dt_shft$ to shft%, data goto L02800
L02800:
           if shft% < 1% or shft% > 3% then shft% = 1%
           if p_flg% = 0% then goto L02880          /* Not Efficiency Rpt*/

L02880:    get str(dt_rec$,133%,48%), using L02890 , dt_price, mrp()
L02890:        FMT 6*PD(14,4)
                                                  /* For Efficiency    */
                                                  /* Products in 'SAM' */
            for i% = 1% to p_max%                 /* Bucket all Prod.  */
                if p_mod$(i%) <> model$ then goto L03030      /* By Shift*/
                   p_unt%(i%,shft%) = p_unt%(i%,shft%) + 1% /* Units & */
                   p_val(i%,shft%)  = p_val(i%,shft%) + dt_price/*Price*/
                   if dt_samp$ <> "0" then p_unts%(i%,shft%) =           ~
                      p_unts%(i%,shft%) + 1%      /* Total Samples Only*/
                   if tt% = 2% then p_untss%(i%,shft%) =                 ~
                      p_untss%(i%,shft%) + 1%     /* Total Charge Sashs*/
                   if tt% = 3% then p_untpp%(i%,shft%) =                 ~
                      p_untpp%(i%,shft%) + 1%     /* Total Charge Parts*/
                   goto L03080                      /* Now Exit Loop     */
L03030:     next i%
            i% = p_max%                           /* Model Not found   */
            p_unt%(i%,shft%) = p_unt%(i%,shft%) + 1% /* for Department */
            p_val(i%,shft%)  = p_val(i%,shft%) + dt_price

                                                  /* No Hit - Invalid  */
L03080:     for i% = 1% to 5%                     /* By Shift 1% = Mat */
                p_mrp(i%,shft%) = p_mrp(i%,shft%) + mrp(i%) /*2% = Lab */
                p_mrp(6%,shft%) = p_mrp(6%,shft%) + mrp(i%) /*3% = OvHd*/
            next i%                                         /*4% = Frt */
                                                            /*5% = Vdis*/
            goto calc_scanned_nxt

CALC_RECEIVED
            i%    = 1%                                  /* (AWD008)    */
            shft% = 1%                                  /* (AWD008)    */
            p_unt%(i%,shft%) = p_unt%(i%,shft%) + 1%    /* (AWD008)    */
            goto calc_scanned_nxt                       /* (AWD008)    */

        calc_scanned_done
            zz% = 0% : dt_sale = 0.0           /* Check Specified SHFT */
            shft% = 3% : kk% = 1%              /* KK% = Start SHFT Val */
            convert p_shft$ to shft%, data goto L03180
L03180:
            if shft% < 1% or shft% > 3% then shft% = 1%
            if p_shft$ = "AL" then goto L03230   /* Efficiency = All     */
               kk% = shft%                     /* Specific Shift Enter */

L03230:     for i% = 1% to p_max%              /* Check Results for    */
                xx% = 0% : yy = 0.0            /* each Product Scanned */
                for k% = kk% to shft%          /* Sum of Entered Shift */
                    xx% = xx% + p_unt%(i%,k%)  /* Scanned Units        */
                    yy  = yy  + p_val(i%,k%)   /* Scanned S.O. Dollars */
                next k%
                convert xx% to p_unt$(i%), pic(######)  /* Set Screen  */

                zz% = zz% + xx%                /* Save Daily/Shift Tot */
                dt_sale = dt_sale + yy

                if xx% = 0% then goto L03360
                   yy = round( yy / xx%, 2)     /* Average Sales Price */
L03360:         convert yy to p_val$(i%), pic($#####.##-)

            next i%                             /*Total Units and Sales*/
            convert zz% to t_unit$, pic(######) /*Daily by Department  */
                                                /* and Shift           */
            convert dt_sale to dt_sale$,pic(######.##-)

            for i% = 1% to 6%                   /* Bucket (6) is the   */
                yy = 0.0                        /* Total Cost for Dept */
                for k% = kk% to shft%           /* By Shift or All     */
                    yy = yy + p_mrp(i%,k%)
                next k%
                convert yy to p_mrp$(i%), pic(######.##-)
            next i%
        return

        REM *************************************************************~
            *           D I S P L A Y   C A L C   V A L U E S           *~
            *************************************************************

        display_codes
            if p_screen% = 1% then return      /* No Display of Screen */
L03580:     gosub set_pf1
            accept                                                       ~
               at (01,17), fac(hex(84)), hdr$                   , ch(45),~
                                                                         ~
               at (04,02), "Model   Scan Count   Scan Value-Avg",        ~
               at (05,02), "-----   ----------   --------------",        ~
                                                                         ~
               at (04,40), "Model   Scan Count   Scan Value-Avg",        ~
               at (05,40), "-----   ----------   --------------",        ~
                                                                         ~
               at (06,03), fac(hex(84))  , p_mod$(k% + 1%)      , ch(03),~
               at (06,12), fac(hex(84))  , p_unt$(k% + 1%)      , ch(06),~
               at (06,25), fac(hex(84))  , p_val$(k% + 1%)      , ch(10),~
                                                                         ~
               at (06,41), fac(hex(84))  , p_mod$(k% + 18%)     , ch(03),~
               at (06,50), fac(hex(84))  , p_unt$(k% + 18%)     , ch(06),~
               at (06,63), fac(hex(84))  , p_val$(k% + 18%)     , ch(10),~
                                                                         ~
               at (07,03), fac(hex(84))  , p_mod$(k% + 2%)      , ch(03),~
               at (07,12), fac(hex(84))  , p_unt$(k% + 2%)      , ch(06),~
               at (07,25), fac(hex(84))  , p_val$(k% + 2%)      , ch(10),~
                                                                         ~
               at (07,41), fac(hex(84))  , p_mod$(k% + 19%)     , ch(03),~
               at (07,50), fac(hex(84))  , p_unt$(k% + 19%)     , ch(06),~
               at (07,63), fac(hex(84))  , p_val$(k% + 19%)     , ch(10),~
                                                                         ~
               at (08,03), fac(hex(84))  , p_mod$(k% + 3%)      , ch(03),~
               at (08,12), fac(hex(84))  , p_unt$(k% + 3%)      , ch(06),~
               at (08,25), fac(hex(84))  , p_val$(k% + 3%)      , ch(10),~
                                                                         ~
               at (08,41), fac(hex(84))  , p_mod$(k% + 20%)     , ch(03),~
               at (08,50), fac(hex(84))  , p_unt$(k% + 20%)     , ch(06),~
               at (08,63), fac(hex(84))  , p_val$(k% + 20%)     , ch(10),~
                                                                         ~
               at (09,03), fac(hex(84))  , p_mod$(k% + 4%)      , ch(03),~
               at (09,12), fac(hex(84))  , p_unt$(k% + 4%)      , ch(06),~
               at (09,25), fac(hex(84))  , p_val$(k% + 4%)      , ch(10),~
                                                                         ~
               at (09,41), fac(hex(84))  , p_mod$(k% + 21%)     , ch(03),~
               at (09,50), fac(hex(84))  , p_unt$(k% + 21%)     , ch(06),~
               at (09,63), fac(hex(84))  , p_val$(k% + 21%)     , ch(10),~
                                                                         ~
               at (10,03), fac(hex(84))  , p_mod$(k% + 5%)      , ch(03),~
               at (10,12), fac(hex(84))  , p_unt$(k% + 5%)      , ch(06),~
               at (10,25), fac(hex(84))  , p_val$(k% + 5%)      , ch(10),~
                                                                         ~
               at (10,41), fac(hex(84))  , p_mod$(k% + 22%)     , ch(03),~
               at (10,50), fac(hex(84))  , p_unt$(k% + 22%)     , ch(06),~
               at (10,63), fac(hex(84))  , p_val$(k% + 22%)     , ch(10),~
                                                                         ~
               at (11,03), fac(hex(84))  , p_mod$(k% + 6%)      , ch(03),~
               at (11,12), fac(hex(84))  , p_unt$(k% + 6%)      , ch(06),~
               at (11,25), fac(hex(84))  , p_val$(k% + 6%)      , ch(10),~
                                                                         ~
               at (11,41), fac(hex(84))  , p_mod$(k% + 23%)     , ch(03),~
               at (11,50), fac(hex(84))  , p_unt$(k% + 23%)     , ch(06),~
               at (11,63), fac(hex(84))  , p_val$(k% + 23%)     , ch(10),~
                                                                         ~
               at (12,03), fac(hex(84))  , p_mod$(k% + 7%)      , ch(03),~
               at (12,12), fac(hex(84))  , p_unt$(k% + 7%)      , ch(06),~
               at (12,25), fac(hex(84))  , p_val$(k% + 7%)      , ch(10),~
                                                                         ~
               at (12,41), fac(hex(84))  , p_mod$(k% + 24%)     , ch(03),~
               at (12,50), fac(hex(84))  , p_unt$(k% + 24%)     , ch(06),~
               at (12,63), fac(hex(84))  , p_val$(k% + 24%)     , ch(10),~
                                                                         ~
               at (13,03), fac(hex(84))  , p_mod$(k% + 8%)      , ch(03),~
               at (13,12), fac(hex(84))  , p_unt$(k% + 8%)      , ch(06),~
               at (13,25), fac(hex(84))  , p_val$(k% + 8%)      , ch(10),~
                                                                         ~
               at (13,41), fac(hex(84))  , p_mod$(k% + 25%)     , ch(03),~
               at (13,50), fac(hex(84))  , p_unt$(k% + 25%)     , ch(06),~
               at (13,63), fac(hex(84))  , p_val$(k% + 25%)     , ch(10),~
                                                                         ~
               at (14,03), fac(hex(84))  , p_mod$(k% + 9%)      , ch(03),~
               at (14,12), fac(hex(84))  , p_unt$(k% + 9%)      , ch(06),~
               at (14,25), fac(hex(84))  , p_val$(k% + 9%)      , ch(10),~
                                                                         ~
               at (14,41), fac(hex(84))  , p_mod$(k% + 26%)     , ch(03),~
               at (14,50), fac(hex(84))  , p_unt$(k% + 26%)     , ch(06),~
               at (14,63), fac(hex(84))  , p_val$(k% + 26%)     , ch(10),~
                                                                         ~
               at (15,03), fac(hex(84))  , p_mod$(k% + 10%)     , ch(03),~
               at (15,12), fac(hex(84))  , p_unt$(k% + 10%)     , ch(06),~
               at (15,25), fac(hex(84))  , p_val$(k% + 10%)     , ch(10),~
                                                                         ~
               at (15,41), fac(hex(84))  , p_mod$(k% + 27%)     , ch(03),~
               at (15,50), fac(hex(84))  , p_unt$(k% + 27%)     , ch(06),~
               at (15,63), fac(hex(84))  , p_val$(k% + 27%)     , ch(10),~
                                                                         ~
               at (16,03), fac(hex(84))  , p_mod$(k% + 11%)     , ch(03),~
               at (16,12), fac(hex(84))  , p_unt$(k% + 11%)     , ch(06),~
               at (16,25), fac(hex(84))  , p_val$(k% + 11%)     , ch(10),~
                                                                         ~
               at (16,41), fac(hex(84))  , p_mod$(k% + 28%)     , ch(03),~
               at (16,50), fac(hex(84))  , p_unt$(k% + 28%)     , ch(06),~
               at (16,63), fac(hex(84))  , p_val$(k% + 28%)     , ch(10),~
                                                                         ~
               at (17,03), fac(hex(84))  , p_mod$(k% + 12%)     , ch(03),~
               at (17,12), fac(hex(84))  , p_unt$(k% + 12%)     , ch(06),~
               at (17,25), fac(hex(84))  , p_val$(k% + 12%)     , ch(10),~
                                                                         ~
               at (17,41), fac(hex(84))  , p_mod$(k% + 29%)     , ch(03),~
               at (17,50), fac(hex(84))  , p_unt$(k% + 29%)     , ch(06),~
               at (17,63), fac(hex(84))  , p_val$(k% + 29%)     , ch(10),~
                                                                         ~
               at (18,03), fac(hex(84))  , p_mod$(k% + 13%)     , ch(03),~
               at (18,12), fac(hex(84))  , p_unt$(k% + 13%)     , ch(06),~
               at (18,25), fac(hex(84))  , p_val$(k% + 13%)     , ch(10),~
                                                                         ~
               at (18,41), fac(hex(84))  , p_mod$(k% + 30%)     , ch(03),~
               at (18,50), fac(hex(84))  , p_unt$(k% + 30%)     , ch(06),~
               at (18,63), fac(hex(84))  , p_val$(k% + 30%)     , ch(10),~
                                                                         ~
               at (19,03), fac(hex(84))  , p_mod$(k% + 14%)     , ch(03),~
               at (19,12), fac(hex(84))  , p_unt$(k% + 14%)     , ch(06),~
               at (19,25), fac(hex(84))  , p_val$(k% + 14%)     , ch(10),~
                                                                         ~
               at (19,41), fac(hex(84))  , p_mod$(k% + 31%)     , ch(03),~
               at (19,50), fac(hex(84))  , p_unt$(k% + 31%)     , ch(06),~
               at (19,63), fac(hex(84))  , p_val$(k% + 31%)     , ch(10),~
                                                                         ~
               at (20,03), fac(hex(84))  , p_mod$(k% + 15%)     , ch(03),~
               at (20,12), fac(hex(84))  , p_unt$(k% + 15%)     , ch(06),~
               at (20,25), fac(hex(84))  , p_val$(k% + 15%)     , ch(10),~
                                                                         ~
               at (20,41), fac(hex(84))  , p_mod$(k% + 32%)     , ch(03),~
               at (20,50), fac(hex(84))  , p_unt$(k% + 32%)     , ch(06),~
               at (20,63), fac(hex(84))  , p_val$(k% + 32%)     , ch(10),~
                                                                         ~
               at (21,03), fac(hex(84))  , p_mod$(k% + 16%)     , ch(03),~
               at (21,12), fac(hex(84))  , p_unt$(k% + 16%)     , ch(06),~
               at (21,25), fac(hex(84))  , p_val$(k% + 16%)     , ch(10),~
                                                                         ~
               at (21,41), fac(hex(84))  , p_mod$(k% + 33%)     , ch(03),~
               at (21,50), fac(hex(84))  , p_unt$(k% + 33%)     , ch(06),~
               at (21,63), fac(hex(84))  , p_val$(k% + 33%)     , ch(10),~
                                                                         ~
               at (22,03), fac(hex(84))  , p_mod$(k% + 17%)     , ch(03),~
               at (22,12), fac(hex(84))  , p_unt$(k% + 17%)     , ch(06),~
               at (22,25), fac(hex(84))  , p_val$(k% + 17%)     , ch(10),~
                                                                         ~
               at (22,41), fac(hex(84))  , p_mod$(k% + 34%)     , ch(03),~
               at (22,50), fac(hex(84))  , p_unt$(k% + 34%)     , ch(06),~
               at (22,63), fac(hex(84))  , p_val$(k% + 34%)     , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)), pf$(1%)                , ch(79),~
               at (24,02), fac(hex(8c)), pf$(2%)                , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L05130             /* First    */
L05100:           k% = 0%
                  goto L03580

L05130:        if keyhit% <> 3% then goto L05180             /* Last      */
L05140:           x% = int(p_max% / 34%)
                  k% = (x%*34%)
                  goto L03580

L05180:        if keyhit% <> 4% then goto L05240             /* Previous */
                  if k% < 35% then goto L05100
                  k% = k% - 34%
                  if k% <= 1% then goto L05100
                  goto L03580

L05240:        if keyhit% <> 5% then goto L05290             /* Next     */
                  k% = k% + 34%
                  if k% < p_max% then goto L03580
                  goto L05140

L05290:        if keyhit% <> 14% then goto L05330       /* Display Costs */
                  gosub display_costs
                  goto L03580

L05330:        if keyhit% <> 15 then goto L05370
                  call "PRNTSCRN"
                  goto L03580

L05370:        close ws
               call "SCREEN" addr ("C", x%, "I", i$(), cursor%())
               return

        set_pf1
            str(hdr$,8%,3%)  = dt_dept$
            str(hdr$,38%,6%) = t_unit$
            pf$(1%)= "Press <Return> to Continue? -"&"- Shifts = " &     ~
                          p_shft$ & " -- PF(14)Display Costs"
            pf$(2%)= "(2)First     (3)Last        (4)Previous " &        ~
                     "      (5)Next          (15)Print Screen"
            pfkeys$ = hex(ff02030405ffffffffffffffff0e0fff00)
            gosub check_screen
            return

        check_screen
            if p_max% > 34% then goto L05590
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L05590:      if k% >= 34% then goto L05620
                gosub no_first
                gosub no_prev
L05620:      if (k% + 34%) <= p_max% then goto L05640
                gosub no_last
L05640:      if k% <= (p_max% - 34%) then goto L05660
                gosub no_next
L05660: return
        no_first
            str(pf$(2%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(2%),46%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),14%,9%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(2%),29%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *           D I S P L A Y   C O S T   V A L U E S           *~
            *************************************************************

        display_costs
L05850:     gosub set_pf2
            accept                                                       ~
               at (01,21), fac(hex(84)), hdr1$                  , ch(40),~
                                                                         ~
               at (04,02), fac(hex(84))  , hdr2$                , ch(22),~
               at (04,25), fac(hex(84))  , dt_date$             , ch(08),~
                                                                         ~
               at (05,02), "Department Code     : ",                     ~
               at (05,25), fac(hex(84))  , dt_dept$             , ch(03),~
               at (05,40), fac(hex(84))  , dt_dept_d$           , ch(30),~
                                                                         ~
               at (06,02), "Shift Code Selection: ",                     ~
               at (06,25), fac(hex(84))  , p_shft$              , ch(02),~
               at (06,40), fac(hex(84))  , dt_shft_d$           , ch(30),~
                                                                         ~
               at (07,02), "Total Sales         : ",                     ~
               at (07,25), fac(hex(84))  , dt_sale$             , ch(10),~
                                                                         ~
               at (10,02), "Material Costs      : ",                     ~
               at (10,25), fac(hex(84))  , p_mrp$(1%)           , ch(10),~
                                                                         ~
               at (12,02), "Labor Costs         : ",                     ~
               at (12,25), fac(hex(84))  , p_mrp$(2%)           , ch(10),~
                                                                         ~
               at (14,02), "Labor Overhead Cost : ",                     ~
               at (14,25), fac(hex(84))  , p_mrp$(3%)           , ch(10),~
                                                                         ~
               at (16,02), "Freight Costs       : ",                     ~
               at (16,25), fac(hex(84))  , p_mrp$(4%)           , ch(10),~
                                                                         ~
               at (18,02), "Vinyl Discount Cost : ",                     ~
               at (18,25), fac(hex(84))  , p_mrp$(5%)           , ch(10),~
                                                                         ~
               at (20,02), "Total Cost          : ",                     ~
               at (20,25), fac(hex(84))  , p_mrp$(6%)           , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)), pf$(1%)                , ch(79),~
               at (24,02), fac(hex(8c)), pf$(2%)                , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L06300
                  call "PRNTSCRN"
                  goto L05850

L06300:        close ws
               call "SCREEN" addr ("C", x%, "I", i$(), cursor%())
               return

        set_pf2
            gosub lookup_codes
            dt_date$ = sc_dte$
            call "DATEFMT" (dt_date$)
            hdr1$ = "   D e p a r t m e n t a l   C o s t s   "
            pf$(1%)= "Press <Return> to Continue? "

            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pfkeys$ = hex(ffffffffffffffffffffffffffff0fff00)
        return

        lookup_codes                           /* Department and Shift */
            init(" ") readkey$, dt_dept_d$, dt_shft_d$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = dt_dept$
            read #4,key = readkey$, using L06510 , dt_dept_d$,eod goto L06530
L06510:        FMT POS(25), CH(30)

L06530:     str(readkey$,1%,9%)   = "PLAN SHFT"
            str(readkey$,10%,15%) = p_shft$
            read #4,key = readkey$, using L06510 , dt_shft_d$,eod goto L06560
L06560:        if p_shft$ = "AL" then dt_shft_d$ = "(All) - Shift Codes"
        return


                                                       /* (EWD004)      */
        check_samples
            ss% = 0%
            if len(dt_part$) < 20 then return        /* Quick Test      */
            if str(dt_part$,1%,1%) = "9" then return   /* Bay/Bow       */
            convert str(dt_part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */
                                                     /*   (EWD006)      */
            if str(dt_part$,7%,2%) > "99" then goto LS1

            if ss% > 11% and ss% < 30% then tt% = 3%
        return                                       /* Code Found      */
LS1:        convert str(dt_part$,23%,3%) to ss%, data goto LS2

            if ss% > 11% and ss% < 30% then tt% = 3%

                                                      /* Code Found      */ 
LS2:    return
                                                       /* (EWD004)      */
        exit_sub
        end
