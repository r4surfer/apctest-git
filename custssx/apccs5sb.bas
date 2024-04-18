        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCCS5SB                             *~
            *  Creation Date     - 10/10/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - Subroutine to Create a Sales Report  *~
            *                      Card for each Salesman.              *~
            *                                                           *~
            *  Code Tables Used  - (SLS CODE4) - Table contains the     *~
            *                                  Sales Report Card Codes. *~
            *                      (SLS CODE5) - The Descriptions Assoc.*~
            *                                  with codes in (SLS CODE4)*~
            *                      (HOME CTRS) - Table contails the     *~
            *                                  Store Codes for all valid*~
            *                                  Home Centers.            *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
            *************************************************************

        sub "APCCS5SB" (#1,#4,b_mnth$,e_mnth$,b_year$,e_year$,b_mlyr$,   ~
                       e_mlyr$,b_lyr$,e_lyr$,per%,rpt_value$,rpt_sum$)
        dim                                                              ~
            b_lyr$6,                     /* START DATE LAST YEAR       */~
            b_mlyr$6,                    /* START DATE THIS MNTH LST YR*/~
            b_mnth$6,                    /* START DATE THIS MONTH      */~
            b_year$6,                    /* START DATE THIS YEAR       */~
            code$2,                      /* Use to Calc Subscript      */~
            cust_code$9, cust_save$9, cs$2,  /* Customer Code          */~
            date$8,                      /* Date for screen display    */~
            e_lyr$6,                     /* END DTE LAST YEAR          */~
            e_mlyr$6,                    /* END DTE THIS MNTH LAST YEAR*/~
            e_mnth$6,                    /* END DTE THIS MONTH         */~
            e_year$6,                    /* END DATE FOR THIS  YEAR    */~
            h_tot$(120%,8%)8,h_tot(120%,8%),  /* Home Center Buckets   */~
            i_tot$(120%,8%)8,i_tot(120%,8%),  /* Independant Buckets   */~
            h_rt$(120%,8%)8,h_rt(120%,8%),  /* Home Center REPORT TOTAL*/~
            i_rt$(120%,8%)8,i_rt(120%,8%),  /* Independant REPORT TOTAL*/~
            g_tot$(6%,6%)8, g_tot(6%,6%),/* COMBINED TOTALS H.C & INDP */~
            oldslsman$4,                 /* SAVE VALUE FO SALESMAN     */~
            p_code$3, s_code$3,          /* Product Sort Code          */~
            postdate$6,                  /* INVOICE POSTING DATE       */~
            prod_d$(20%)20, cc$24,       /* PRODUCT CODE DESCRIPTIONS  */~
            readkey$27,                  /* READ KEY FOR APCSADTL      */~
            rpt_sum$1,                   /* (S)UMMARY OR (D)ETAIL      */~
            title$40,                    /* Report Title               */~
            rpt_value$9,                 /* SPECIFIED SALESMAN OR ALL  */~
            salesman$4,                  /* Salesman Code              */~
            time$8                       /* System Time                */

        dim                                                              ~
            cst(9%),                     /* Costing Values By Line Item*/~
            tcst(6%), cst_d$(6%)10,      /* Line Item Cost Buckets     */~
            mtd(6%), lmtd(6%),           /* Current / Last Year MTD    */~
            ytd(6%), lytd(6%),           /* Current / Last Year YTD    */~
            lyear(6%)                    /* Total Last Year            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Salesman Report Card Report       "
            pname$ = "APCCS5SB - Rev: R6.04"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCSLSDT ! APC Sales Analysis Detail File           *~
            * #4  ! GENCODES ! MASTER TABLE FILE                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

            cst_d$(1%) = "Sls Units "
            cst_d$(2%) = "MFG Sales "
            cst_d$(3%) = "MFG Cost  "
            cst_d$(4%) = "Gross Marg"
            cst_d$(5%) = "Pcnt Marg."
            cst_d$(6%) = "Trans Cost"

            title$ = "   M-E-S Cost Report Card by Salesman   "
            call "SHOSTAT" ( title$ )
            mat h_tot = zer : mat i_tot = zer
            mat h_rt  = zer : mat i_rt  = zer
            mat g_tot = zer
            init (" ") postdate$,readkey$, time$, date$, oldslsman$,     ~
                       salesman$, prod_d$(), h_tot$(), i_tot$(), p_code$,~
                       cust_code$, cust_save$, s_code$, h_rt$(), i_rt$(),~
                       g_tot$()

            gosub load_descript           /* Load Product Descriptions */
                                          /* for the Report by Salesman*/
            select printer(134)
            call "TIME" (time$)
            date$ = date : call "DATEFMT" (date$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            readkey$ = all(hex(00))
            if str(rpt_value$,1%,3%) = "ALL" then goto L01180
               str(readkey$,1%,4%) = str(rpt_value$,1%,4%)
               call "SHOSTAT" ("Processing Salesman ("&                  ~
                     str(readkey$,1%,4%) & ")")

L01180:     read #1,key 4% > readkey$,using L01320 ,postdate$, readkey$,   ~
                             cst(), eod goto end_report
            oldslsman$ = str(readkey$,1%,4%)
            cust_save$ = str(readkey$,5%,9%)      /* Customer Code     */
            s_code$    = str(readkey$,14%,3%)     /* Product Sort Code */
            cust_code$ = cust_save$               /* Customer Code     */
            cs$        = str(cust_code$,1%,2%)    /* 1ST TWO DIGITS    */
            p_code$    = s_code$                  /* Product Sort Code */
            gosub check_home_center
            gosub load_subscript
            goto L01330
        read_loop
            read #1,using L01320 ,postdate$, readkey$, cst(),              ~
                                                      eod goto end_report
L01320:          FMT CH(6), POS(375), CH(27), POS(402), 9*PD(14,4)
L01330:     salesman$  = str(readkey$,1%,4%)      /* Salesman Code     */
            cust_code$ = str(readkey$,5%,9%)      /* Customer Code     */
            cs$        = str(cust_code$,1%,2%)    /* 1ST TWO DIGITS    */
            p_code$    = str(readkey$,14%,3%)     /* Product Sort Code */
            mat tcst = zer
            if str(rpt_value$,1%,3%) = "ALL" then goto L01410
               if salesman$ <> str(rpt_value$,1%,4%) then goto end_report
                                     /* Re-Calc Net Invoice Amount     */
L01410:        tcst(1%) = round(cst(9%), 2)  /* Tot Quantity Shipped   */
               tcst(2%) = round(cst(8%), 2)  /* Tot Sale Price W/Disc  */
               tcst(3%) = round(cst(6%), 2)  /* Tot MFG Cost           */
               tcst(6%) = round(cst(7%), 2)  /* Tot Transportation Cost*/
            if salesman$ = oldslsman$ then goto L01490
               gosub slsman_brk
               call "SHOSTAT" ("Processing Salesman ("& salesman$ & ")")

L01490:     if cust_save$ = cust_code$ then goto L01510
               goto L01520
L01510:     if s_code$ = p_code$ then goto L01590
L01520:        if home% = 0% then gosub build_independant                ~
                             else gosub build_home_ctr
               s_code$    = p_code$
               cust_save$ = cust_code$
               gosub check_home_center
               gosub load_subscript

L01590:     if postdate$ < b_year$ then last_year
            if postdate$ > e_year$ then read_loop
            if postdate$ > e_mnth$ then read_loop
               for i% = 1% to 6%
                   ytd(i%) = round( ytd(i%) + tcst(i%), 2)
               next i%
               if postdate$ < b_mnth$ then goto read_loop
                  for i% = 1% to 6%
                      mtd(i%) = round( mtd(i%) + tcst(i%), 2)
                  next i%
               goto read_loop
        last_year
            if postdate$ < b_lyr$ then goto read_loop
                  for i% = 1% to 6%
                      lyear(i%) = round( lyear(i%) + tcst(i%), 2)
                  next i%
               if postdate$ > e_mlyr$ then goto read_loop
                  for i% = 1% to 6%
                      lytd(i%) = round( lytd(i%) + tcst(i%), 2)
                  next i%
               if postdate$ < b_mlyr$ then goto read_loop
                  for i% = 1% to 6%
                      lmtd(i%) = round( lmtd(i%) + tcst(i%), 2)
                  next i%
        goto read_loop

        end_report                /* Report Ending Routine */
            gosub slsman_brk
            title$ = "M-E-S Cost Report Card For All Salesman "
            oldslsman$ = "ALL "
            gosub page_head
            gosub print_totals
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto rpt_exit

        check_home_center                    /* H.C. Only Lowes and HQ */
            home% = 0%
            if cs$ <> "HQ" and cs$ <> "LO" then return
            init(" ") cc$
            str(cc$,1%,9%)   = "HOME CTRS"           /* GEN Code Table */
            str(cc$,10%,15%) = cust_code$            /* Customer Code  */
            read #4,key = cc$, eod goto L02030
            home% = 1%
L02030: return

        load_subscript                   /* Find Subscript Associated  */
            init(" ") cc$, code$         /* with the Product sort Code */
            str(cc$,1%,9%)   = "SLS CODE4"
            str(cc$,10%,15%) = p_code$               /* Sort Code      */
            read #4,key = cc$, using L02100, code$, eod goto L02110
L02100:         FMT POS(25), CH(2)
L02110:     cnt% = 19%                            /* Misc Product (19) */
            convert code$ to cnt%, data goto L02130 /* Grand Totals (20) */
L02130:
            e_cnt% = (6% * cnt%)              /* ENDING PRODUCT BUCKET */
            b_cnt% = (e_cnt% - 6%) + 1%       /* BEGIN PRODUCT BUCKET  */
                                              /* TOTAL BUCKETS (120%)  */
        return

        slsman_brk
           if home% = 0% then gosub build_independant                    ~
                         else gosub build_home_ctr
           gosub print_summary
           oldslsman$ = salesman$
           mat h_tot  = zer
           mat i_tot  = zer
           mat g_tot  = zer
           init(" ") h_tot$(), i_tot$(), g_tot$()
           mat mtd = zer : mat lmtd = zer
           mat ytd = zer : mat lytd = zer : mat lyear = zer
        return

        build_independant
            x = mtd(1%) + lmtd(1%) + ytd(1%) + lytd(1%) + lyear(1%)
            if x = 0 then goto L02680                /* No Data to Update*/
               k% = 0%
               for i% = b_cnt% to e_cnt%
                   k% = k% + 1%
                 i_tot(i%,1%) = round(i_tot(i%,1%) + mtd(k%), 2)
                 i_tot(i%,2%) = round(i_tot(i%,2%) + lmtd(k%), 2)
                                                   /* (3) MTD Variance */
                 i_tot(i%,4%) = round(i_tot(i%,4%) + ytd(k%), 2)
                 i_tot(i%,5%) = round(i_tot(i%,5%) + lytd(k%), 2)
                                                   /* (6) YTD Variance */
                 i_tot(i%,7%) = round(i_tot(i%,7%) + lyear(k%), 2)
                                                   /* Grand Totals     */
               i_tot(114%+k%,1%) = round(i_tot(114%+k%,1%) + mtd(k%),2)
               i_tot(114%+k%,2%) = round(i_tot(114%+k%,2%) + lmtd(k%),2)
               i_tot(114%+k%,4%) = round(i_tot(114%+k%,4%) + ytd(k%),2)
               i_tot(114%+k%,5%) = round(i_tot(114%+k%,5%) + lytd(k%),2)
               i_tot(114%+k%,7%) = round(i_tot(114%+k%,7%) + lyear(k%),2)
        REM - BUILD REPORT TOTALS       /* 1 thru 6 = Aluminum         */
          if i% < 7% then goto L02660     /* No Aluminum in Grand Totals */
                 i_rt(i%,1%) = round(i_rt(i%,1%) + mtd(k%), 2)
                 i_rt(i%,2%) = round(i_rt(i%,2%) + lmtd(k%), 2)
                                                   /* (3) MTD Variance */
                 i_rt(i%,4%) = round(i_rt(i%,4%) + ytd(k%), 2)
                 i_rt(i%,5%) = round(i_rt(i%,5%) + lytd(k%), 2)
                                                   /* (6) YTD Variance */
                 i_rt(i%,7%) = round(i_rt(i%,7%) + lyear(k%), 2)
                                                   /* Grand Totals     */
                i_rt(114%+k%,1%)  = round(i_rt(114%+k%,1%) + mtd(k%),2)
                i_rt(114%+k%,2%)  = round(i_rt(114%+k%,2%) + lmtd(k%),2)
                i_rt(114%+k%,4%)  = round(i_rt(114%+k%,4%) + ytd(k%),2)
                i_rt(114%+k%,5%)  = round(i_rt(114%+k%,5%) + lytd(k%),2)
                i_rt(114%+k%,7%)  = round(i_rt(114%+k%,7%) + lyear(k%),2)
L02660:        next i%

L02680:     mat mtd = zer : mat lmtd = zer
            mat ytd = zer : mat lytd = zer : mat lyear = zer
        return
        build_home_ctr
            x = mtd(1%) + lmtd(1%) + ytd(1%) + lytd(1%) + lyear(1%)
            if x = 0 then goto L03070                /* No Data to Update*/
               k% = 0%
               for i% = b_cnt% to e_cnt%
                   k% = k% + 1%
                 h_tot(i%,1%) = round(h_tot(i%,1%) + mtd(k%), 2)
                 h_tot(i%,2%) = round(h_tot(i%,2%) + lmtd(k%), 2)
                                                   /* (3) MTD Variance */
                 h_tot(i%,4%) = round(h_tot(i%,4%) + ytd(k%), 2)
                 h_tot(i%,5%) = round(h_tot(i%,5%) + lytd(k%), 2)
                                                   /* (6) YTD Variance */
                 h_tot(i%,7%) = round(h_tot(i%,7%) + lyear(k%), 2)
                                                   /* Grand Totals     */
               h_tot(114%+k%,1%) = round(h_tot(114%+k%,1%) + mtd(k%),2)
               h_tot(114%+k%,2%) = round(h_tot(114%+k%,2%) + lmtd(k%),2)
               h_tot(114%+k%,4%) = round(h_tot(114%+k%,4%) + ytd(k%),2)
               h_tot(114%+k%,5%) = round(h_tot(114%+k%,5%) + lytd(k%),2)
               h_tot(114%+k%,7%) = round(h_tot(114%+k%,7%) + lyear(k%),2)
        REM - BUILD REPORT TOTALS
          if i% < 7% then goto L03050     /* No Aluminum in Grand Totals */
                 h_rt(i%,1%) = round(h_rt(i%,1%) + mtd(k%), 2)
                 h_rt(i%,2%) = round(h_rt(i%,2%) + lmtd(k%), 2)
                                                   /* (3) MTD Variance */
                 h_rt(i%,4%) = round(h_rt(i%,4%) + ytd(k%), 2)
                 h_rt(i%,5%) = round(h_rt(i%,5%) + lytd(k%), 2)
                                                   /* (6) YTD Variance */
                 h_rt(i%,7%) = round(h_rt(i%,7%) + lyear(k%), 2)
                                                   /* Grand Totals     */
                h_rt(114%+k%,1%)  = round(h_rt(114%+k%,1%) + mtd(k%),2)
                h_rt(114%+k%,2%)  = round(h_rt(114%+k%,2%) + lmtd(k%),2)
                h_rt(114%+k%,4%)  = round(h_rt(114%+k%,4%) + ytd(k%),2)
                h_rt(114%+k%,5%)  = round(h_rt(114%+k%,5%) + lytd(k%),2)
                h_rt(114%+k%,7%)  = round(h_rt(114%+k%,7%) + lyear(k%),2)
L03050:        next i%

L03070:     mat mtd = zer : mat lmtd = zer
            mat ytd = zer : mat lytd = zer : mat lyear = zer
        return
        print_summary
          if rpt_sum$ = "S" then return        /* Skip Salesman Detail */

          gosub page_head
          gosub calc_summary

          for i% = 1% to cnt_max%
              if i% = 8% then i% = 19%      /* Do Totals Last          */
                 e_cnt% = (6% * i%)
                 b_cnt% = (e_cnt% - 6%) + 1%
              for k% = b_cnt% to e_cnt%
        REM - ( Home Centers )              /* Current Month to Date   */
                  x = round( (h_tot(k%,1%) / 1000.0), 0)
                  convert x to h_tot$(k%,1%), pic(####,###)
                                            /* Last Year Month to Date */
                  x = round( (h_tot(k%,2%) / 1000.0), 0)
                  convert x to h_tot$(k%,2%), pic(####,###)
                                            /* % of Change MTD         */
                  x = 0.0
                  if h_tot(k%,2%) < .1 then goto L03310
                     x = ( ( ( h_tot(k%,1%) / h_tot(k%,2%)) - 1.0) * 100)
L03310:           h_tot(k%,3%) = round( x, 1)
                  convert h_tot(k%,3%) to h_tot$(k%,3%), pic(##.#-)
                  if h_tot(k%,2%) < .1 and h_tot(k%,1%) > .1 then        ~
                     h_tot$(k%,3%) = "++.++"

                                            /* Current Year to Date    */
                   x = round( (h_tot(k%,4%) / 1000.0), 0)
                   convert x to h_tot$(k%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
                   x = round( (h_tot(k%,5%) / 1000.0), 0)
                   convert x to h_tot$(k%,5%), pic(####,###)
                                            /* % of Change YTD         */
                   x = 0.0
                  if h_tot(k%,5%) < .1 then goto L03460
                  x = ( ( ( h_tot(k%,4%) / h_tot(k%,5%)) - 1.0) * 100)
L03460:           h_tot(k%,6%) = round( x, 1)
                  convert h_tot(k%,6%) to h_tot$(k%,6%), pic(##.#-)
                  if h_tot(k%,5%) < .1 and h_tot(k%,4%) > .1 then        ~
                     h_tot$(k%,6%) = "++.++"

        REM - ( Independants )
                                            /* Current Month to Date   */
                  x = round( (i_tot(k%,1%) / 1000.0), 0)
                  convert x to i_tot$(k%,1%), pic(####,###)
                                            /* Last Year Month to Date */
                  x = round( (i_tot(k%,2%) / 1000.0), 0)
                  convert x to i_tot$(k%,2%), pic(####,###)
                                            /* % of Change MTD         */
                  x = 0.0
                  if i_tot(k%,2%) < .1 then goto L03620
                     x = ( ( ( i_tot(k%,1%) / i_tot(k%,2%)) - 1.0) * 100)
L03620:           i_tot(k%,3%) = round( x, 1)
                  convert i_tot(k%,3%) to i_tot$(k%,3%), pic(##.#-)
                  if i_tot(k%,2%) < .1 and i_tot(k%,1%) > .1 then        ~
                     i_tot$(k%,3%) = "++.++"


                                            /* Current Year to Date    */
                  x = round( (i_tot(k%,4%) / 1000.0), 0)
                  convert x to i_tot$(k%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
                  x = round( (i_tot(k%,5%) / 1000.0), 0)
                  convert x to i_tot$(k%,5%), pic(####,###)
                                            /* % of Change YTD         */
                  x = 0.0
                  if i_tot(k%,5%) < .1 then goto L03780
                     x = ( ( ( i_tot(k%,4%) / i_tot(k%,5%)) - 1.0) * 100)
L03780:           i_tot(k%,6%) = round( x, 1)
                  convert i_tot(k%,6%) to i_tot$(k%,6%), pic(##.#-)
                  if i_tot(k%,5%) < .1 and i_tot(k%,4%) > .1 then        ~
                     i_tot$(k%,6%) = "++.++"

        REM - ( TOTAL CHANGE )
              x = h_tot(k%,4%) + i_tot(k%,4%)  /* CURRENT YTD HOME/IND */
              y = h_tot(k%,5%) + i_tot(k%,5%)  /* PREVIOUS YTD HOME/IND*/
              z = 0.0
              if y < .1 then L03890
                 z = ( ( ( x / y ) - 1.0) * 100)
L03890:       i_tot(k%,8%) = round( z, 1)
              convert i_tot(k%,8%) to i_tot$(k%,8%), pic(##.#-)
              if y < .1 and x > .1 then i_tot$(k%,8%) = "++.++"
            next k%
          next i%
                                              /* BUILD COMBINED TOTALS */
          for k% = 1% to 6%
              g_tot(k%,1%) = h_tot(114%+k%,1%) + i_tot(114%+k%,1%)
              x = round( g_tot(k%,1%) / 1000.0, 0)
              convert x to g_tot$(k%,1%), pic(####,###)

              g_tot(k%,2%) = h_tot(114%+k%,2%) + i_tot(114%+k%,2%)
              x = round( g_tot(k%,2%) / 1000.0, 0)
              convert x to g_tot$(k%,2%), pic(####,###)

              g_tot(k%,4%) = h_tot(114%+k%,4%) + i_tot(114%+k%,4%)
              x = round( g_tot(k%,4%) / 1000.0, 0)
              convert x to g_tot$(k%,4%), pic(####,###)

              g_tot(k%,5%) = h_tot(114%+k%,5%) + i_tot(114%+k%,5%)
              x = round( g_tot(k%,5%) / 1000.0, 0)
              convert x to g_tot$(k%,5%), pic(####,###)

              x = 0.0
              if g_tot(k%,2%) < .1 then goto L04150
              x = ( ( ( g_tot(k%,1%) / g_tot(k%,2%) ) - 1.0) * 100)
L04150:       g_tot(k%,3%) = round( x, 1)
              convert g_tot(k%,3%) to g_tot$(k%,3%),pic(##.#-)
              if g_tot(k%,2%) < .1 and g_tot(k%,1%) > .1 then            ~
                                       g_tot$(k%,3%) = "++.++"
               x = 0.0
               if g_tot(k%,5%) < .1 then goto L04220
               x = ( ( ( g_tot(k%,4%) / g_tot(k%,5%) ) - 1.0) * 100)
L04220:        g_tot(k%,6%) = round( x, 1)
               convert g_tot(k%,6%) to g_tot$(k%,6%),pic(##.#-)
               if g_tot(k%,5%) < .1 and g_tot(k%,4%) > .1 then           ~
                                        g_tot$(k%,6%) = "++.++"
          next k%

          for i% = 1% to cnt_max%
            if i% = 8% then i% = 19%
            e_cnt% = (i% * 6%)
            b_cnt% = (e_cnt% - 6%) + 1%
            if lcntr% > 52% then gosub page_head
            print using L07670, prod_d$(i%)
            j% = 0%
            lcntr% = lcntr% + 1%
            for k% = b_cnt% to e_cnt%
                j% = j% + 1%
            print using L07700, cst_d$(j%) , h_tot$(k%,1%), h_tot$(k%,2%), ~
                             h_tot$(k%,3%), h_tot$(k%,4%), h_tot$(k%,5%),~
                             h_tot$(k%,6%), i_tot$(k%,1%), i_tot$(k%,2%),~
                             i_tot$(k%,3%), i_tot$(k%,4%), i_tot$(k%,5%),~
                             i_tot$(k%,6%), i_tot$(k%,8%)
                lcntr% = lcntr% + 1%
            next k%
            if i% < 19% then print using L07730                            ~
                        else print using L07550
                lcntr% = lcntr% + 1%
          next i%
                                              /* PRINT COMBINED TOTALS */
          if lcntr% > 51% then gosub page_head
          print using L07550
          print using L07550
          print using L07670, "TOTAL (INDP + H.C.) "
          for k% = 1% to 6%
              print using L07760, cst_d$(k%),g_tot$(k%,1%), g_tot$(k%,2%), ~
                            g_tot$(k%,3%), g_tot$(k%,4%), g_tot$(k%,5%), ~
                            g_tot$(k%,6%)
              lcntr% = lcntr% + 1%
          next k%
          print using L07490

        return

        calc_summary
           for i% = 1% to cnt_max%
              if i% = 8% then i% = 19%        /* DO TOTALS LAST        */
              e_cnt% = (6% * i%)
              b_cnt% = (e_cnt% - 6%) + 1%
              k% = b_cnt% + 3%
                                              /* INDEPENDANTS * MTD()  */
         i_tot(k%,1%) = round(i_tot(k%-2%,1%) - i_tot(k%-1%,1%), 2)
         if i_tot(k%-2%,1%) = 0 then goto L04750
         i_tot(k%+1%,1%) = round((i_tot(k%,1%)/i_tot(k%-2%,1%))*100, 2)
                                                             /* LMTD() */
L04750:  i_tot(k%,2%) = round(i_tot(k%-2%,2%) - i_tot(k%-1%,2%), 2)
         if i_tot(k%-2%,2%) = 0 then goto L04790
         i_tot(k%+1%,2%) = round((i_tot(k%,2%)/i_tot(k%-2%,2%))*100, 2)
                                                             /* YTD()  */
L04790:  i_tot(k%,4%) = round(i_tot(k%-2%,4%) - i_tot(k%-1%,4%), 2)
         if i_tot(k%-2%,4%) = 0 then goto L04830
         i_tot(k%+1%,4%) = round((i_tot(k%,4%)/i_tot(k%-2%,4%))*100, 2)
                                                             /* LYTD() */
L04830:  i_tot(k%,5%) = round(i_tot(k%-2%,5%) - i_tot(k%-1%,5%), 2)
         if i_tot(k%-2%,5%) = 0 then goto L04900
         i_tot(k%+1%,5%) = round((i_tot(k%,5%)/i_tot(k%-2%,5%))*100, 2)
                                                             /* LYEAR()*/
         i_tot(k%,7%) = round(i_tot(k%-2%,7%) - i_tot(k%-1%,7%), 2)
         if i_tot(k%-2%,7%) = 0 then goto L04900
         i_tot(k%+1%,7%) = round((i_tot(k%,7%)/i_tot(k%-2%,7%))*100, 2)
L04900:                                       /* HOME CENTERS * MTD()  */
         h_tot(k%,1%) = round(h_tot(k%-2%,1%) - h_tot(k%-1%,1%), 2)
         if h_tot(k%-2%,1%) = 0 then goto L04950
         h_tot(k%+1%,1%) = round((h_tot(k%,1%)/h_tot(k%-2%,1%))*100, 2)
                                                             /* LMTD() */
L04950:  h_tot(k%,2%) = round(h_tot(k%-2%,2%) - h_tot(k%-1%,2%), 2)
         if h_tot(k%-2%,2%) = 0 then goto L04990
         h_tot(k%+1%,2%) = round((h_tot(k%,2%)/h_tot(k%-2%,2%))*100, 2)
                                                             /* YTD()  */
L04990:  h_tot(k%,4%) = round(h_tot(k%-2%,4%) - h_tot(k%-1%,4%), 2)
         if h_tot(k%-2%,4%) = 0 then goto L05030
         h_tot(k%+1%,4%) = round((h_tot(k%,4%)/h_tot(k%-2%,4%))*100, 2)
                                                             /* LYTD() */
L05030:  h_tot(k%,5%) = round(h_tot(k%-2%,5%) - h_tot(k%-1%,5%), 2)
         if h_tot(k%-2%,5%) = 0 then goto L05100
         h_tot(k%+1%,5%) = round((h_tot(k%,5%)/h_tot(k%-2%,5%))*100, 2)
                                                             /* LYEAR()*/
         h_tot(k%,7%) = round(h_tot(k%-2%,7%) - h_tot(k%-1%,7%), 2)
         if h_tot(k%-2%,7%) = 0 then goto L04900
         h_tot(k%+1%,7%) = round((h_tot(k%,7%)/h_tot(k%-2%,7%))*100, 2)
L05100:    next i%
        return

        print_totals
          gosub calc_totals

          for i% = 1% to cnt_max%
              if i% = 8% then i% = 19%      /* Do Totals Last          */
                 e_cnt% = (6% * i%)
                 b_cnt% = (e_cnt% - 6%) + 1%
              for k% = b_cnt% to e_cnt%
        REM - ( Home Centers )              /* Current Month to Date   */
                  x = round( (h_rt(k%,1%) / 1000.0), 0)
                  convert x to h_rt$(k%,1%), pic(####,###)
                                            /* Last Year Month to Date */
                  x = round( (h_rt(k%,2%) / 1000.0), 0)
                  convert x to h_rt$(k%,2%), pic(####,###)
                                            /* % of Change MTD         */
                  x = 0.0
                  if h_rt(k%,2%) < .1 then goto L05310
                     x = ( ( ( h_rt(k%,1%) / h_rt(k%,2%)) - 1.0) * 100)
L05310:           h_rt(k%,3%) = round( x, 1)
                  convert h_rt(k%,3%) to h_rt$(k%,3%), pic(##.#-)
                  if h_rt(k%,2%) < .1 and h_rt(k%,1%) > .1 then          ~
                     h_rt$(k%,3%) = "++.++"

                                            /* Current Year to Date    */
                   x = round( (h_rt(k%,4%) / 1000.0), 0)
                   convert x to h_rt$(k%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
                   x = round( (h_rt(k%,5%) / 1000.0), 0)
                   convert x to h_rt$(k%,5%), pic(####,###)
                                            /* % of Change YTD         */
                   x = 0.0
                  if h_rt(k%,5%) < .1 then goto L05460
                  x = ( ( ( h_rt(k%,4%) / h_rt(k%,5%)) - 1.0) * 100)
L05460:           h_rt(k%,6%) = round( x, 1)
                  convert h_rt(k%,6%) to h_rt$(k%,6%), pic(##.#-)
                  if h_rt(k%,5%) < .1 and h_rt(k%,4%) > .1 then          ~
                     h_rt$(k%,6%) = "++.++"

        REM - ( Independants )
                                            /* Current Month to Date   */
                  x = round( (i_rt(k%,1%) / 1000.0), 0)
                  convert x to i_rt$(k%,1%), pic(####,###)
                                            /* Last Year Month to Date */
                  x = round( (i_rt(k%,2%) / 1000.0), 0)
                  convert x to i_rt$(k%,2%), pic(####,###)
                                            /* % of Change MTD         */
                  x = 0.0
                  if i_rt(k%,2%) < .1 then goto L05620
                     x = ( ( ( i_rt(k%,1%) / i_rt(k%,2%)) - 1.0) * 100)
L05620:           i_rt(k%,3%) = round( x, 1)
                  convert i_rt(k%,3%) to i_rt$(k%,3%), pic(##.#-)
                  if i_rt(k%,2%) < .1 and i_rt(k%,1%) > .1 then          ~
                     i_rt$(k%,3%) = "++.++"


                                            /* Current Year to Date    */
                  x = round( (i_rt(k%,4%) / 1000.0), 0)
                  convert x to i_rt$(k%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
                  x = round( (i_rt(k%,5%) / 1000.0), 0)
                  convert x to i_rt$(k%,5%), pic(####,###)
                                            /* % of Change YTD         */
                  x = 0.0
                  if i_rt(k%,5%) < .1 then goto L05780
                     x = ( ( ( i_rt(k%,4%) / i_rt(k%,5%)) - 1.0) * 100)
L05780:           i_rt(k%,6%) = round( x, 1)
                  convert i_rt(k%,6%) to i_rt$(k%,6%), pic(##.#-)
                  if i_rt(k%,5%) < .1 and i_rt(k%,4%) > .1 then          ~
                     i_rt$(k%,6%) = "++.++"

        REM - ( TOTAL CHANGE )
              x = h_rt(k%,4%) + i_rt(k%,4%)  /* CURRENT YTD HOME/IND */
              y = h_rt(k%,5%) + i_rt(k%,5%)  /* PREVIOUS YTD HOME/IND*/
              z = 0.0
              if y < .1 then L05890
                 z = ( ( ( x / y ) - 1.0) * 100)
L05890:       i_rt(k%,8%) = round( z, 1)
              convert i_rt(k%,8%) to i_rt$(k%,8%), pic(##.#-)
              if y < .1 and x > .1 then i_rt$(k%,8%) = "++.++"
            next k%
          next i%
                                              /* BUILD COMBINED TOTALS */
          for k% = 1% to 6%
              g_tot(k%,1%) = h_rt(114%+k%,1%) + i_rt(114%+k%,1%)
              x = round( g_tot(k%,1%) / 1000.0, 0)
              convert x to g_tot$(k%,1%), pic(####,###)

              g_tot(k%,2%) = h_rt(114%+k%,2%) + i_rt(114%+k%,2%)
              x = round( g_tot(k%,2%) / 1000.0, 0)
              convert x to g_tot$(k%,2%), pic(####,###)

              g_tot(k%,4%) = h_rt(114%+k%,4%) + i_rt(114%+k%,4%)
              x = round( g_tot(k%,4%) / 1000.0, 0)
              convert x to g_tot$(k%,4%), pic(####,###)

              g_tot(k%,5%) = h_rt(114%+k%,5%) + i_rt(114%+k%,5%)
              x = round( g_tot(k%,5%) / 1000.0, 0)
              convert x to g_tot$(k%,5%), pic(####,###)

              x = 0.0
              if g_tot(k%,2%) < .1 then goto L06150
              x = ( ( ( g_tot(k%,1%) / g_tot(k%,2%) ) - 1.0) * 100)
L06150:       g_tot(k%,3%) = round( x, 1)
              convert g_tot(k%,3%) to g_tot$(k%,3%),pic(##.#-)
              if g_tot(k%,2%) < .1 and g_tot(k%,1%) > .1 then            ~
                                       g_tot$(k%,3%) = "++.++"
               x = 0.0
               if g_tot(k%,5%) < .1 then goto L06220
               x = ( ( ( g_tot(k%,4%) / g_tot(k%,5%) ) - 1.0) * 100)
L06220:        g_tot(k%,6%) = round( x, 1)
               convert g_tot(k%,6%) to g_tot$(k%,6%),pic(##.#-)
               if g_tot(k%,5%) < .1 and g_tot(k%,4%) > .1 then           ~
                                        g_tot$(k%,6%) = "++.++"
          next k%

          for i% = 1% to cnt_max%
            if i% = 8% then i% = 19%
            e_cnt% = (i% * 6%)
            b_cnt% = (e_cnt% - 6%) + 1%
            if lcntr% > 52% then gosub page_head
            print using L07670, prod_d$(i%)
            j% = 0%
            lcntr% = lcntr% + 1%
            for k% = b_cnt% to e_cnt%
                j% = j% + 1%
            print using L07700, cst_d$(j%) , h_rt$(k%,1%), h_rt$(k%,2%),   ~
                             h_rt$(k%,3%), h_rt$(k%,4%), h_rt$(k%,5%),   ~
                             h_rt$(k%,6%), i_rt$(k%,1%), i_rt$(k%,2%),   ~
                             i_rt$(k%,3%), i_rt$(k%,4%), i_rt$(k%,5%),   ~
                             i_rt$(k%,6%), i_rt$(k%,8%)
                lcntr% = lcntr% + 1%
            next k%
            if i% < 19% then print using L07730                            ~
                        else print using L07550
                lcntr% = lcntr% + 1%
          next i%
                                              /* PRINT COMBINED TOTALS */
          if lcntr% > 51% then gosub page_head
          print using L07550
          print using L07550
          print using L07670, "TOTAL (INDP + H.C.) "
          for k% = 1% to 6%
              print using L07760, cst_d$(k%),g_tot$(k%,1%), g_tot$(k%,2%), ~
                            g_tot$(k%,3%), g_tot$(k%,4%), g_tot$(k%,5%), ~
                            g_tot$(k%,6%)
              lcntr% = lcntr% + 1%
          next k%
          print using L07490

        return

        calc_totals
           for i% = 1% to cnt_max%
              if i% = 8% then i% = 19%        /* DO TOTALS LAST        */
              e_cnt% = (6% * i%)
              b_cnt% = (e_cnt% - 6%) + 1%
              k% = b_cnt% + 3%
                                              /* INDEPENDANTS * MTD()  */
         i_rt(k%,1%) = round(i_rt(k%-2%,1%) - i_rt(k%-1%,1%), 2)
         if i_rt(k%-2%,1%) = 0 then goto L06750
         i_rt(k%+1%,1%) = round((i_rt(k%,1%)/i_rt(k%-2%,1%))*100, 2)
                                                             /* LMTD() */
L06750:  i_rt(k%,2%) = round(i_rt(k%-2%,2%) - i_rt(k%-1%,2%), 2)
         if i_rt(k%-2%,2%) = 0 then goto L06790
         i_rt(k%+1%,2%) = round((i_rt(k%,2%)/i_rt(k%-2%,2%))*100, 2)
                                                             /* YTD()  */
L06790:  i_rt(k%,4%) = round(i_rt(k%-2%,4%) - i_rt(k%-1%,4%), 2)
         if i_rt(k%-2%,4%) = 0 then goto L06830
         i_rt(k%+1%,4%) = round((i_rt(k%,4%)/i_rt(k%-2%,4%))*100, 2)
                                                             /* LYTD() */
L06830:  i_rt(k%,5%) = round(i_rt(k%-2%,5%) - i_rt(k%-1%,5%), 2)
         if i_rt(k%-2%,5%) = 0 then goto L06900
         i_rt(k%+1%,5%) = round((i_rt(k%,5%)/i_rt(k%-2%,5%))*100, 2)
                                                             /* LYEAR()*/
         i_rt(k%,7%) = round(i_rt(k%-2%,7%) - i_rt(k%-1%,7%), 2)
         if i_rt(k%-2%,7%) = 0 then goto L06900
         i_rt(k%+1%,7%) = round((i_rt(k%,7%)/i_rt(k%-2%,7%))*100, 2)
L06900:                                       /* HOME CENTERS * MTD()  */
         h_rt(k%,1%) = round(h_rt(k%-2%,1%) - h_rt(k%-1%,1%), 2)
         if h_rt(k%-2%,1%) = 0 then goto L06950
         h_rt(k%+1%,1%) = round((h_rt(k%,1%)/h_rt(k%-2%,1%))*100, 2)
                                                             /* LMTD() */
L06950:  h_rt(k%,2%) = round(h_rt(k%-2%,2%) - h_rt(k%-1%,2%), 2)
         if h_rt(k%-2%,2%) = 0 then goto L06990
         h_rt(k%+1%,2%) = round((h_rt(k%,2%)/h_rt(k%-2%,2%))*100, 2)
                                                             /* YTD()  */
L06990:  h_rt(k%,4%) = round(h_rt(k%-2%,4%) - h_rt(k%-1%,4%), 2)
         if h_rt(k%-2%,4%) = 0 then goto L07030
         h_rt(k%+1%,4%) = round((h_rt(k%,4%)/h_rt(k%-2%,4%))*100, 2)
                                                             /* LYTD() */
L07030:  h_rt(k%,5%) = round(h_rt(k%-2%,5%) - h_rt(k%-1%,5%), 2)
         if h_rt(k%-2%,5%) = 0 then goto L07100
         h_rt(k%+1%,5%) = round((h_rt(k%,5%)/h_rt(k%-2%,5%))*100, 2)
                                                             /* LYEAR()*/
         h_rt(k%,7%) = round(h_rt(k%-2%,7%) - h_rt(k%-1%,7%), 2)
         if h_rt(k%-2%,7%) = 0 then goto L06900
         h_rt(k%+1%,7%) = round((h_rt(k%,7%)/h_rt(k%-2%,7%))*100, 2)
L07100:    next i%
        return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L07490
            print using L07580 , date$, time$, title$, oldslsman$
            print using L07550
            print using L07610, per%
            print using L07550
            print using L07640
            print using L07730
            lcntr% = 7%
        return

        load_descript                  /* Load Descriptions Associated */
            cnt% = 0%                  /* with product areas.          */
            init(" ") cc$, prod_d$()
            str(cc$,1%,9%)   = "SLS CODE5"
        load_nxt_sort
            read #4,key > cc$,using L07320,cc$ ,eod goto L07390
L07320:         FMT CH(24)
            if str(cc$,1%,9%) <> "SLS CODE5" then goto L07390
               convert str(cc$,10%,2%) to cnt%, data goto L07350
L07350:
               get #4,using L07370, prod_d$(cnt%)
L07370:          FMT POS(25), CH(20)
            goto load_nxt_sort
L07390: cnt_max% = cnt%
        return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L07490: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

        %!                                                               ~
        ~                                                                !

L07550: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L07580: %!Run ######## @ ########             ###########################~
        ~#############             ( In Thousands )       Salesman: #### !

L07610: %Period: ##           !<---------- H o m e   C e n t e r s ------~
        ~---->!!<---------- I n d e p e n d e n t s ---------->!! Total  !

L07640: %! Description        !Curr MTD!Last MTD!%Chg !Curr YTD!Last YTD!~
        ~%Chg !!Curr MTD!Last MTD!%Chg !Curr YTD!Last YTD!%Chg !!% Change!

L07670: %!####################!        !        !     !        !        !~
        ~     !!        !        !     !        !        !     !!        !

L07700: %!     ##########     !########!########!#####!########!########!~
        ~#####!!########!########!#####!########!########!#####!! ###### !

L07730: %!--------------------!--------!--------!-----!--------!--------!~
        ~-----!!--------!--------!-----!--------!--------!-----!!--------!

L07760: %!     ##########     !########!########!#####!########!########!~
        ~#####!! ------ ! ------ ! --- ! ------ ! ------ ! --- !! ------ !

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        rpt_exit

            end
