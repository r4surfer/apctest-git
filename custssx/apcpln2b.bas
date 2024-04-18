        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN2B                             *~
            *  Creation Date     - 02/12/96                             *~
            *  Last Modified Date- 11/14/97                             *~
            *  Description       - For a Specified Department Calculate *~
            *                      the Scanned Units for a given        *~
            *                      Production day                       *~
            *                                                           *~
            *  Special Notes - PD_DEPT$ and SC_DTE$ are always passed   *~
            *                  to the Subroutine when called. Department*~
            *                  Code is Required. ( Cannot have 'ALL' )  *~
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
            *                  (APCPLN40, APCPLN4B)                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/11/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/11/96 ! Mod to Support New File Layout for Scan. ! RHH *~
            *          !   Part No, Sash, Part, Sample, Wood Surr.!     *~
            *          !   Flags added to file.                   !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/29/98 ! Y2K                                      ! LDJ *~
            *************************************************************

            sub "APCPLN2B" ( pd_dept$,   /* Specified Department Code  */~
                             sc_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day       */~
                             p_shft$,    /* Shift Code or (AL)         */~
                             p_mod$(),   /* Department Products        */~
                             p_unt%(),   /* Product Units + Samples    */~
                             p_unts%(),  /* Sample Products Only       */~
                             p_val(),    /* Product Dollar Value       */~
                             p_mrp(),    /* (5) Costing Buckets        */~
                             p_max%,     /* Max Number of Products     */~
                             p_flg%,     /* 0% = Load, 1% = No Load    */~
                             p_scan%,    /* 0% = Not SCANNED,1%=Scanned*/~
                             p_screen%,  /* 0% = Yes, 1% = No - Display*/~
                             #1,         /* (APCPLNDP) Master Dept.    */~
                             #2,         /* (APCPLNTK) Prod. Tracking  */~
                             #4 )        /* (GENCDSIN) Master Code Tab */

        dim                              /* Subroutine - Variables     */~
            pd_rec$170, xx_dte$6,        /* Scanning Record, Date      */~
            pd_dept$3, pd_dept_d$30,     /* Department Code            */~
            sc_dte$6,                    /* Specified Production Date  */~
            p_shft$2, pd_shft_d$30,      /* Specified Shift Code       */~
            pd_st$2, xx_dept$3,          /* Scanning Status Code       */~
            p_mod$(306%)3,               /* Products for Department    */~
            p_unt%(306%,3%),             /* Scanned Units each Product */~
            p_unts%(306%,3%),            /* Scanned Units Samples Only */~
            p_unt$(306%)6,               /* Screen Display Scanned Unit*/~
            p_val(306%,3%),              /* Scanned Units Dollar Value */~
            p_val$(306%)10,              /* Screen Display Avg S.O. $$ */~
            p_mrp(6%,3%), p_mrp$(6%)10,  /* Mat,Lab,Ovr,Frt,Disc,Total */~
            mrp(5%),                     /* (5) Scanning Buckets Cost  */~
            bg_dte$6, ed_dte$6,          /* Starting/Ending Prod. Date */~
            jdate1$7, jdate2$7,          /* Julian Date Formats                 (Y2K, LDJ) */~
            pd_date$8,                   /* Starting Production Date   */~
            pd_sale$10, cnt$24,          /* Total Sales Dollars        */~
            pd_key$35, pd_shft$2,        /* Tracking Keys              */~
            pd_time$8,                   /* Scanned Time Stamp         */~
            ap$2,                        /* 'AM' or 'PM' From time     */~
            model$3,                     /* Model, Screen Code, Product*/~
            pd_sash$1, pd_prt$1,         /* Sash and Part Flags        */~
            pd_samp$1, pd_wood$3,        /* Sample                     */~
            readkey$24,                  /* Use for Table Lookup       */~
            hdr$40, t_unit$6, hdr1$40,   /* Screen Header              */~
            cursor%(2%), hdr2$22,        /* Cursor Location for Edit   */~
            i$(24%)80,                   /* SCREEN IMAGE               */~
            pf$(3%)79,                   /* PF Key Description         */~
            pfkeys$32                    /* PF Key Values              */

            sc_max% = 306%               /* Max Entries for Screen     */
            hdr$ = "Dept. (XXX) Total Scanned Units (XXXXXX)"
            hdr2$ = "Production Date     : "
            if p_scan% <> 0% then hdr2$ = "Scanned Date        : "
            mat p_unt%  = zer        /* Units by Shift */
            mat p_unts% = zer        /* Units by Shift for Samples     */
            mat p_val   = zer        /* Avg S.O. $$ for Units by Shift */
            mat p_mrp   = zer        /* Costing All Products by Shift  */
            if p_flg% = 0% then gosub load_prod /* Skip for Efficiency */
            gosub calc_scanned_units
            k% = 0%
            gosub display_codes
            goto exit_sub

        load_prod
           call "SHOSTAT" ("Loading Products for Dept ("&pd_dept$&")" )
           init(" ") p_mod$(), pd_key$
           p_max% = 0%
           str(pd_key$,1%,3%) = pd_dept$
           str(pd_key$,4%,2%) = "01"               /* Set Process Code */
           read #1,key > pd_key$, using L01070 , pd_key$, eod goto L01190
           goto L01080
        load_prod_nxt
           read #1, using L01070 , pd_key$, eod goto L01190
L01070:        FMT POS(11), CH(12)
L01080:    if str(pd_key$,1%,3%) <> pd_dept$ then goto L01190
              if p_max% = 0% then goto L01140
              for j% = 1% to p_max%
                  if p_mod$(j%) = str(pd_key$,8%,3%) then                ~
                                                     goto load_prod_nxt
              next j%
L01140:       p_max% = p_max% + 1%
              j% = p_max%
              if p_max% > sc_max% then p_max% = sc_max%
              p_mod$(j%) = str(pd_key$,8%,3%)
              goto load_prod_nxt
L01190: return

        calc_scanned_units   /* Windows Scanned from 7 AM of Specified */
                             /* Production Day, Until 6 59 AM of the   */
                             /* Next Production Day. 1st,2nd,3rd Shifts*/
            cnt$ = "Scanned Units [ XXXXXX ]"
            init(" ") jdate1$, jdate2$, bg_dte$, ed_dte$, pd_key$
            init(" ") p_unt$(), p_val$(), p_mrp$(), pd_key$
                                         /* Julian date Curr Prod Week */
            call "DATE" addr("GJ", str(sc_dte$,,6%), str(jdate1$,,5%), x%)          /* (Y2K, LDJ) */
                                                         /* CONVERT TO */
                                                         /* JULIAN DATE*/
            call "DATJULCV" (jdate1$)                                               /* (Y2K, LDJ) */
            convert str(jdate1$,5%,3%) to j1%, data goto L01310                     /* (Y2K, LDJ) */
L01310:
                                                 /* Current Production */
            cnt% = 0%                            /* Day Julian Date    */
            j2% = j1% + 1%                       /* Tommorow's/Next Day*/
            jdate2$ = jdate1$                    /* Julian Date        */
            if j2% < 367% then goto L01420
               j2% = 001%
            convert str(jdate2$,1%,4%) to rhh%, data goto L01390                    /* (Y2K, LDJ) */
L01390:
            convert (rhh% + 1%) to str(jdate2$,1%,4%), pic(0000)                    /* (Y2K, LDJ) */

L01420:     convert j1% to str(jdate1$,5%,3%), pic(000)                             /* (Y2K, LDJ) */

            convert j2% to str(jdate2$,5%,3%), pic(000)                             /* (Y2K, LDJ) */
            call "DATJULCV" (jdate1$)                                               /* (Y2K, LDJ) */
            call "DATJULCV" (jdate2$)                                               /* (Y2K, LDJ) */
                                               /* Begin with Production*/
            call "DATE" addr("JG", str(jdate1$,,5%), bg_dte$, x%)                   /* (Y2K, LDJ) */
                                                         /* Date and   */
            call "DATE" addr("JG", str(jdate2$,,5%), ed_dte$, x%)                   /* (Y2K, LDJ) */
                                                         /*Next Days   */
                                                         /*Prod. Date  */
        REM CALL "SHOSTAT" ("ToDay = "&BG_DTE$&" Tommorow = "&ED_DTE$)
        REM STOP
           ff% = 3%                       /* Scanned Products          */
           if p_scan% = 0% then ff% = 1%  /* Not Scanned Products      */
                                          /* 1st Check Specified Prod. */
           str(pd_key$,1%,6%) = bg_dte$   /* date from 7 AM until Mid. */
           read #2,key ff% > pd_key$, using L01600, pd_rec$,               ~
                                               eod goto calc_scanned_done
              goto L01610
        calc_scanned_nxt
           read #2, using L01600 , pd_rec$, eod goto calc_scanned_done
L01600:        FMT CH(170)
L01610:    if p_scan% = 0% then xx_dte$ = str(pd_rec$,29%,6%)            ~
                           else xx_dte$ = str(pd_rec$,35%,6%)
           cnt% = cnt% + 1%
           if mod(cnt%,100%) <> 0 then goto L01690
              convert cnt% to str(cnt$,17%,6%),pic(######)

              print at(02,29);hex(84);cnt$;

L01690:    if xx_dte$ > ed_dte$ then goto calc_scanned_done
                                           /* Valid Department Required*/
           xx_dept$ = str(pd_rec$,45%,3%)
           if xx_dept$ <> pd_dept$ then goto calc_scanned_nxt
                                           /* Check Scanning Status    */
                                           /* '12' or Greater Complete */
           pd_st$   = str(pd_rec$,41%,2%)
           if p_scan% <> 0% and pd_st$ < "12" then goto calc_scanned_nxt
                                           /* Scanned Product Only     */
           if p_scan% = 0% and pd_st$ > "11" then goto calc_scanned_nxt
                                           /* Check Not Scanned Prod   */
           pd_time$ = str(pd_rec$,60%,8%)
           hr% = 0%
           convert str(pd_time$,1%,2%) to hr%, data goto L01830
L01830:
           ap$ = str(pd_time$,7%,2%)          /* Scanned 'AM' or 'PM'  */
           if xx_dte$ = ed_dte$ then goto L01970
                                              /* Check Current Scan Day*/
                                              /* First. 1st, 2nd and   */
                                              /* part of 3rd shift     */
                                              /* 7 AM to 12 Midnight   */
               if ap$ = "PM" then goto L02010   /* Part of Prev Day Prod.*/
                  if hr% < 7% or hr% = 12% then goto calc_scanned_nxt
                  goto L02010
                                              /* Second Check Next Day */
                                              /* Scanned Data. Last    */
                                              /* part of 3rd Shift.    */
                                              /* Midnight to 6 59 'AM' */
L01970:    if ap$ = "PM" then goto calc_scanned_nxt
           if hr% < 7% or hr% = 12% then goto L02010 /* Midnight to 6 59A*/
              goto calc_scanned_nxt                /* Current Day     */

L02010:    pd_sash$  = str(pd_rec$,150%,1%)
           pd_prt$   = str(pd_rec$,151%,1%)
           pd_samp$  = str(pd_rec$,152%,1%)

            tt% = 1%                              /* TT% = 1% Window   */
            if pd_sash$ = "Y" then tt% = 2%       /* TT% = 2% Sashs    */
            if pd_prt$  = "Y" then tt% = 3%       /* TT% = 3% Parts    */
            if tt% <> 1% then goto calc_scanned_nxt /* Do Not Count    */
                                                    /* Sashs and Parts */
            model$    = str(pd_rec$,125%,3%)
            pd_shft$  = str(pd_rec$,43%,2%)
            if pd_dept$ <> "044" then goto L02180   /* Wood Surround Dept*/
               if p_flg% = 0% then goto L02180      /* Not Efficiency Rpt*/
                  pd_wood$  = str(pd_rec$,153%,3%)
                  if pd_wood$ = "000" then goto calc_scanned_nxt
                     model$ = pd_wood$
                                                  /* Shift that Record */
L02180:     shft% = 1%                            /* was Scanned on.   */
            convert pd_shft$ to shft%, data goto L02200
L02200:
            if shft% < 1% or shft% > 3% then shft% = 1%
            if p_flg% = 0% then goto L02250         /* Not Efficiency Rpt*/
               if pd_dept$ = "009" then str(model$,3%,1%) = "X"

L02250:     get str(pd_rec$,77%,48%), using L02260 , pd_price, mrp()
L02260:        FMT 6*PD(14,4)
                                                  /* For Efficiency    */
                                                  /* Products in 'SAM' */
            for i% = 1% to p_max%                 /* Bucket all Prod.  */
                if p_mod$(i%) <> model$ then goto L02360      /* By Shift*/
                   p_unt%(i%,shft%) = p_unt%(i%,shft%) + 1% /* Units & */
                   p_val(i%,shft%)  = p_val(i%,shft%) + pd_price/*Price*/
                   if pd_samp$ = "Y" then p_unts%(i%,shft%) =            ~
                      p_unts%(i%,shft%) + 1%      /* SAMPLES ONLY      */
                   goto L02380
L02360:     next i%
              goto calc_scanned_nxt               /* No Hit - Invalid  */
L02380:     for i% = 1% to 5%                     /* By Shift 1% = Mat */
                p_mrp(i%,shft%) = p_mrp(i%,shft%) + mrp(i%) /*2% = Lab */
                p_mrp(6%,shft%) = p_mrp(6%,shft%) + mrp(i%) /*3% = OvHd*/
            next i%                                         /*4% = Frt */
                                                            /*5% = Vdis*/
            goto calc_scanned_nxt
        calc_scanned_done
            zz% = 0% : pd_sale = 0.0           /* Check Specified SHFT */
            shft% = 3% : kk% = 1%              /* KK% = Start SHFT Val */
            convert p_shft$ to shft%, data goto L02480
L02480:
            if shft% < 1% or shft% > 3% then shft% = 1%
            if p_shft$ = "AL" then goto L02530   /* Efficiency = All     */
               kk% = shft%                     /* Specific Shift Enter */

L02530:     for i% = 1% to p_max%              /* Check Results for    */
                xx% = 0% : yy = 0.0            /* each Product Scanned */
                for k% = kk% to shft%          /* Sum of Entered Shift */
                    xx% = xx% + p_unt%(i%,k%)  /* Scanned Units        */
                    yy  = yy  + p_val(i%,k%)   /* Scanned S.O. Dollars */
                next k%
                convert xx% to p_unt$(i%), pic(######)  /* Set Screen  */

                zz% = zz% + xx%                /* Save Daily/Shift Tot */
                pd_sale = pd_sale + yy

                if xx% = 0% then goto L02660
                   yy = round( yy / xx%, 2)     /* Average Sales Price */
L02660:         convert yy to p_val$(i%), pic($#####.##-)

            next i%                             /*Total Units and Sales*/
            convert zz% to t_unit$, pic(######) /*Daily by Department  */
                                                /* and Shift           */
            convert pd_sale to pd_sale$,pic(######.##-)

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
L02880:     gosub set_pf1
            accept                                                       ~
               at (01,21), fac(hex(84)), hdr$                   , ch(40),~
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

               if keyhit% <> 2% then goto L04430             /* First    */
L04400:           k% = 0%
                  goto L02880

L04430:        if keyhit% <> 3% then goto L04480             /* Last      */
L04440:           x% = int(p_max% / 34%)
                  k% = (x%*34%)
                  goto L02880

L04480:        if keyhit% <> 4% then goto L04540             /* Previous */
                  if k% < 35% then goto L04400
                  k% = k% - 34%
                  if k% <= 1% then goto L04400
                  goto L02880

L04540:        if keyhit% <> 5% then goto L04590             /* Next     */
                  k% = k% + 34%
                  if k% < p_max% then goto L02880
                  goto L04440

L04590:        if keyhit% <> 14% then goto L04630       /* Display Costs */
                  gosub display_costs
                  goto L02880

L04630:        if keyhit% <> 15 then goto L04670
                  call "PRNTSCRN"
                  goto L02880

L04670:        close ws
               call "SCREEN" addr ("C", x%, "I", i$(), cursor%())
               return

        set_pf1
            str(hdr$,8%,3%)  = pd_dept$
            str(hdr$,34%,6%) = t_unit$
            pf$(1%)= "Press <Return> to Continue? -"&"- Shifts = " &     ~
                          p_shft$ & " -- PF(14)Display Costs"
            pf$(2%)= "(2)First     (3)Last        (4)Previous " &        ~
                     "      (5)Next          (15)Print Screen"
            pfkeys$ = hex(ff02030405ffffffffffffffff0e0fff00)
            gosub check_screen
            return

        check_screen
            if p_max% > 34% then goto L04890
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L04890:      if k% >= 34% then goto L04920
                gosub no_first
                gosub no_prev
L04920:      if (k% + 34%) <= p_max% then goto L04940
                gosub no_last
L04940:      if k% <= (p_max% - 34%) then goto L04960
                gosub no_next
L04960: return
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
L05150:     gosub set_pf2
            accept                                                       ~
               at (01,21), fac(hex(84)), hdr1$                  , ch(40),~
                                                                         ~
               at (04,02), fac(hex(84))  , hdr2$                , ch(22),~
               at (04,25), fac(hex(84))  , pd_date$             , ch(08),~
                                                                         ~
               at (05,02), "Department Code     : ",                     ~
               at (05,25), fac(hex(84))  , pd_dept$             , ch(03),~
               at (05,40), fac(hex(84))  , pd_dept_d$           , ch(30),~
                                                                         ~
               at (06,02), "Shift Code Selection: ",                     ~
               at (06,25), fac(hex(84))  , p_shft$              , ch(02),~
               at (06,40), fac(hex(84))  , pd_shft_d$           , ch(30),~
                                                                         ~
               at (07,02), "Total Sales         : ",                     ~
               at (07,25), fac(hex(84))  , pd_sale$             , ch(10),~
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

               if keyhit% <> 15 then goto L05600
                  call "PRNTSCRN"
                  goto L05150

L05600:        close ws
               call "SCREEN" addr ("C", x%, "I", i$(), cursor%())
               return

        set_pf2
            gosub lookup_codes
            pd_date$ = sc_dte$
            call "DATEFMT" (pd_date$)
            hdr1$ = "   D e p a r t m e n t a l   C o s t s   "
            pf$(1%)= "Press <Return> to Continue? "

            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pfkeys$ = hex(ffffffffffffffffffffffffffff0fff00)
        return

        lookup_codes                           /* Department and Shift */
            init(" ") readkey$, pd_dept_d$, pd_shft_d$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = pd_dept$
            read #4,key = readkey$, using L05810 , pd_dept_d$,eod goto L05830
L05810:        FMT POS(25), CH(30)

L05830:     str(readkey$,1%,9%)   = "PLAN SHFT"
            str(readkey$,10%,15%) = p_shft$
            read #4,key = readkey$, using L05810 , pd_shft_d$,eod goto L05860
L05860:        if p_shft$ = "AL" then pd_shft_d$ = "(All) - Shift Codes"
        return

        exit_sub
        end
