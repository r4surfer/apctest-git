        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCSLS4                              *~
            *  Creation Date     - 07/10/95                             *~
            *  Last Modified Date- 08/08/00                             *~
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
            * 07/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 12/19/97 ! Mod to Report Print, start a Bucket (2)  ! RHH *~
            *          ! and print all the Buckets. (CNT_MAX%)    !     *~
            * 04/10/98 ! Mod to Only print Lines with a valid text! RHH *~
            *          !   description, otherwise skip. (EWD)     !     *~  
            * 05/12/00 ! Mod to check home cntr not only LO and HQ! CMG *~
            *          !   customers & bucket 01 is not Aluminum  !     *~  
            *          !   anymore.  (EWD001)                     !     *~  
            * 06/05/00 ! Mod to use linecom for amount instead of ! CMG *~
            *          !     of calculating. (EWD002)             !     *~
            * 08/08/00 ! Mod to take out error code 8 (EWD003)    ! CMG *~
            *************************************************************

        sub "APCSLS4" (#1,#4,#5,b_mnth$,e_mnth$,b_year$,e_year$,b_mlyr$,  ~
                       e_mlyr$,b_lyr$,e_lyr$,per%,rpt_value$,rpt_sum$)
                                        /* (EWD002) - ADD SALESMAN FILE */
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
            h_tot$(20%,8%)8, h_tot(20%,8%), /* Home Center Buckets     */~
            i_tot$(20%,8%)8, i_tot(20%,8%), /* Independant Buckets     */~
            h_rt$(20%,8%)8, h_rt(20%,8%),   /* Home Center REPORT TOTAL*/~
            i_rt$(20%,8%)8, i_rt(20%,8%),   /* Independant REPORT TOTAL*/~
            g_tot$(6%)8, g_tot(6%),      /* COMBINED TOTALS H.C & INDP */~
            oldslsman$4, oldslsname$30,  /* SAVE VALUE FO SALESMAN     */~
            p_code$3, s_code$3,          /* Product Sort Code          */~
            postdate$6,                  /* INVOICE POSTING DATE       */~
            prod_d$(99%)20, cc$24,       /* PRODUCT CODE DESCRIPTIONS  */~
            readkey$27,                  /* READ KEY FOR APCSADTL      */~
            rpt_sum$1,                   /* (S)UMMARY OR (D)ETAIL      */~
            rpttitle$40,                 /* Report Title               */~
            rpt_value$9,                 /* SPECIFIED SALESMAN OR ALL  */~
            salesman$4,                  /* Salesman Code              */~
            time$8,                      /* System Time                */~
            gl_acct$9,                   /* Sales Account Number       */~
/*(EWD003)*/err_code$2                   /* Error Code From EWDSLS00   */            

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Sales Report Card By Salesman     "
            pname$ = "APCSLS43 - Rev: R6.04"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCSLSDT ! APC Sales Analysis Detail File           *~
            * #4  ! GENCODES ! MASTER TABLE FILE                        *~
            * #5  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~            
            * #6  ! SLMMASTR ! Salesman master file                     *~             
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

            rpttitle$ = "Month End Sales Report Card by Salesman "
            call "SHOSTAT" ( rpttitle$ )
            mat h_tot = zer : mat i_tot = zer
            mat h_rt  = zer : mat i_rt  = zer
            mat g_tot = zer
            init (" ") postdate$,readkey$, time$, date$, oldslsman$,     ~
                       salesman$, prod_d$(), h_tot$(), i_tot$(), p_code$,~
                       cust_code$, cust_save$, s_code$, h_rt$(), i_rt$(),~
                       g_tot$(), oldslsname$

            gosub load_descript           /* Load Product Descriptions */
                                          /* for the Report by Salesman*/
            select printer(134)
            call "TIME" (time$)
            date$ = date : call "DATEFMT" (date$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            readkey$ = all(hex(00))
            if str(rpt_value$,1%,3%) = "ALL" then goto L01060
               str(readkey$,1%,4%) = str(rpt_value$,1%,4%)
               call "SHOSTAT" ("Processing Salesman ("&                  ~
                     str(readkey$,1%,4%) & ")")
/* (EWD002) - Begin */
REM L01060      read #1,key 4% > readkey$,using L01210 ,postdate$, invdisc,    ~
REM                              linedisc, lineext, slsname$, readkey$,        ~
REM                                                       eod goto end_report

L01060:     read #1,key 3% > readkey$,using L01210 ,postdate$,err_code$,     ~
                                 gl_acct$,readkey$,linecom, eod goto end_report
                                                     /*  (EWD003)  */

            oldslsman$ = str(readkey$,1%,4%)
            gosub get_slsname                     /* (EWD002) */
            oldslsname$ = slsname$                
            cust_save$ = str(readkey$,5%,9%)      /* Customer Code     */
            s_code$    = str(readkey$,14%,3%)     /* Product Sort Code */
            cust_code$ = cust_save$               /* Customer Code     */
            cs$        = str(cust_code$,1%,2%)    /* 1ST TWO DIGITS    */
            p_code$    = s_code$                  /* Product Sort Code */
            gosub check_home_center
            gosub load_subscript
            goto L01230
        read_loop
            read #1,using L01210 ,postdate$,err_code$,gl_acct$,readkey$,  ~
                                     linecom,eod goto end_report
                                                   /*  (EWD003)  */
 
REM            FMT CH(6),POS(54),CH(02),CH(9),POS(94),CH(27),POS(195),PD(14,4)
L01210:        FMT CH(6),POS(57),CH(02),CH(9),POS(97),CH(27),POS(198),PD(14,4)
L01230:     if str(gl_acct$,1%,8%) = "3650-313" then goto read_loop
            if str(gl_acct$,1%,2%) <> "36" then goto read_loop
            if str(err_code$,1%,2%) = "08" then goto read_loop  /*  (EWD003)  */            
            salesman$  = str(readkey$,1%,4%)      /* Salesman Code     */
            cust_code$ = str(readkey$,5%,9%)      /* Customer Code     */
            cs$        = str(cust_code$,1%,2%)    /* 1ST TWO DIGITS    */
            p_code$    = str(readkey$,14%,3%)     /* Product Sort Code */
            if str(rpt_value$,1%,3%) = "ALL" then goto L01310
               if salesman$ <> str(rpt_value$,1%,4%) then goto end_report
                                     /* Re-Calc Net Invoice Amount     */
                                     /* Price After Line Item Discount */
REM L01310      amount     = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                     /* Price After Order Discount     */
REM             amount     = round(amount * (1.0 - (invdisc/100.0)), 2)

L01310:         amount     = round(linecom, 2)
/* (EWD002) - End */

            if salesman$ = oldslsman$ then goto L01380
               gosub slsman_brk
               call "SHOSTAT" ("Processing Salesman ("& salesman$ & ")")

L01380:     if cust_save$ = cust_code$ then goto L01400
               goto L01410
L01400:     if s_code$ = p_code$ then goto L01480
L01410:        if home% = 0% then gosub build_independant                ~
                             else gosub build_home_ctr
               s_code$    = p_code$
               cust_save$ = cust_code$
               gosub check_home_center
               gosub load_subscript

L01480:     if postdate$ < b_year$ then last_year
            if postdate$ > e_year$ then read_loop
            if postdate$ > e_mnth$ then read_loop
               ytd = round( ytd + amount, 2)
               if postdate$ < b_mnth$ then goto read_loop
               mtd = round( mtd + amount, 2)
               goto read_loop
        last_year
            if postdate$ < b_lyr$ then goto read_loop
                  lyear = round( lyear + amount, 2)
               if postdate$ > e_mlyr$ then goto read_loop
                  lytd  = round( lytd + amount, 2)
               if postdate$ < b_mlyr$ then goto read_loop
                  lmtd  = round( lmtd + amount, 2)
        goto read_loop

        end_report                /* Report Ending Routine */
            gosub slsman_brk
            rpttitle$ = "  M-E-S Report Card For All Salesman    "
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
            read #4,key = cc$, eod goto L01820
            home% = 1%
L01820: return

        load_subscript                   /* Find Subscript Associated  */
            init(" ") cc$, code$         /* with the Product sort Code */
            str(cc$,1%,9%)   = "SLS CODE4"
            str(cc$,10%,15%) = p_code$               /* Sort Code      */
            read #4,key = cc$, using L01890, code$, eod goto L01930
L01890:         FMT POS(25), CH(2)
            cnt% = 19%                            /* Misc Product (19) */
            convert code$ to cnt%, data goto L01930 /* Grand Totals (20) */

L01930: return

        slsman_brk
           if home% = 0% then gosub build_independant                    ~
                         else gosub build_home_ctr
           gosub page_head
           gosub print_summary
           oldslsman$ = salesman$
           gosub get_slsname                       /* (EWD002) */ 
           oldslsname$ = slsname$
           mat h_tot  = zer
           mat i_tot  = zer
           mat g_tot  = zer
           init(" ") h_tot$(), i_tot$(), g_tot$()
           mtd, lmtd, ytd, lytd, lyear = 0.0
        return

        build_independant
            x = mtd + lmtd + ytd + lytd + lyear
            if x = 0 then return                   /* No Data to Update*/
            i_tot(cnt%,1%) = round(i_tot(cnt%,1%) + mtd, 2)
            i_tot(cnt%,2%) = round(i_tot(cnt%,2%) + lmtd, 2)
                                                   /* (3) MTD Variance */
            i_tot(cnt%,4%) = round(i_tot(cnt%,4%) + ytd, 2)
            i_tot(cnt%,5%) = round(i_tot(cnt%,5%) + lytd, 2)
                                                   /* (6) YTD Variance */
            i_tot(cnt%,7%) = round(i_tot(cnt%,7%) + lyear, 2)
                                                   /* Grand Totals     */
            i_tot(20%,1%)  = round(i_tot(20%,1%) + mtd, 2)
            i_tot(20%,2%)  = round(i_tot(20%,2%) + lmtd, 2)
            i_tot(20%,4%)  = round(i_tot(20%,4%) + ytd, 2)
            i_tot(20%,5%)  = round(i_tot(20%,5%) + lytd, 2)
            i_tot(20%,7%)  = round(i_tot(20%,7%) + lyear, 2)
        REM - BUILD REPORT TOTALS
REM       if cnt% = 1% then goto L02390   /* No Aluminum in Grand Totals */
            i_rt(cnt%,1%) = round(i_rt(cnt%,1%) + mtd, 2)
            i_rt(cnt%,2%) = round(i_rt(cnt%,2%) + lmtd, 2)
                                                   /* (3) MTD Variance */
            i_rt(cnt%,4%) = round(i_rt(cnt%,4%) + ytd, 2)
            i_rt(cnt%,5%) = round(i_rt(cnt%,5%) + lytd, 2)
                                                   /* (6) YTD Variance */
            i_rt(cnt%,7%) = round(i_rt(cnt%,7%) + lyear, 2)
                                                   /* Grand Totals     */
            i_rt(20%,1%)  = round(i_rt(20%,1%) + mtd, 2)
            i_rt(20%,2%)  = round(i_rt(20%,2%) + lmtd, 2)
            i_rt(20%,4%)  = round(i_rt(20%,4%) + ytd, 2)
            i_rt(20%,5%)  = round(i_rt(20%,5%) + lytd, 2)
            i_rt(20%,7%)  = round(i_rt(20%,7%) + lyear, 2)
L02390:     mtd, lmtd, ytd, lytd, lyear = 0.0
        return
        build_home_ctr
            x = mtd + lmtd + ytd + lytd + lyear
            if x = 0 then return                   /* No Data to Update*/
            h_tot(cnt%,1%) = round(h_tot(cnt%,1%) + mtd, 2)
            h_tot(cnt%,2%) = round(h_tot(cnt%,2%) + lmtd, 2)
                                                   /* (3) MTD Variance */
            h_tot(cnt%,4%) = round(h_tot(cnt%,4%) + ytd, 2)
            h_tot(cnt%,5%) = round(h_tot(cnt%,5%) + lytd, 2)
                                                   /* (6) YTD Variance */
            h_tot(cnt%,7%) = round(h_tot(cnt%,7%) + lyear, 2)
                                                   /* Grand Totals     */
            h_tot(20%,1%)  = round(h_tot(20%,1%) + mtd, 2)
            h_tot(20%,2%)  = round(h_tot(20%,2%) + lmtd, 3)
            h_tot(20%,4%)  = round(h_tot(20%,4%) + ytd, 2)
            h_tot(20%,5%)  = round(h_tot(20%,5%) + lytd, 2)
            h_tot(20%,7%)  = round(h_tot(20%,7%) + lyear, 2)
        REM - BUILD REPORT TOTALS
REM          if cnt% = 1% then goto L02720   /* No Aluminum in Grand Totals */
            h_rt(cnt%,1%) = round(h_rt(cnt%,1%) + mtd, 2)
            h_rt(cnt%,2%) = round(h_rt(cnt%,2%) + lmtd, 2)
                                                   /* (3) MTD Variance */
            h_rt(cnt%,4%) = round(h_rt(cnt%,4%) + ytd, 2)
            h_rt(cnt%,5%) = round(h_rt(cnt%,5%) + lytd, 2)
                                                   /* (6) YTD Variance */
            h_rt(cnt%,7%) = round(h_rt(cnt%,7%) + lyear, 2)
                                                   /* Grand Totals     */
            h_rt(20%,1%)  = round(h_rt(20%,1%) + mtd, 2)
            h_rt(20%,2%)  = round(h_rt(20%,2%) + lmtd, 2)
            h_rt(20%,4%)  = round(h_rt(20%,4%) + ytd, 2)
            h_rt(20%,5%)  = round(h_rt(20%,5%) + lytd, 2)
            h_rt(20%,7%)  = round(h_rt(20%,7%) + lyear, 2)
L02720:     mtd, lmtd, ytd, lytd, lyear = 0.0
        return
        print_summary           /*  (EWD001) - changed for loop to start at 1 */
          for i% = 1% to cnt_max%           /* Mod - 12/19/97          */
        REM - IF I% = 8% THEN I% = 19%      /* Do Totals Last          */
        REM - ( Home Centers )              /* Current Month to Date   */
              x = round( (h_tot(i%,1%) / 1000.0), 0)
                  convert x to h_tot$(i%,1%), pic(####,###)
                                            /* Last Year Month to Date */
              x = round( (h_tot(i%,2%) / 1000.0), 0)
                  convert x to h_tot$(i%,2%), pic(####,###)
                                            /* % of Change MTD         */
              x = 0.0
              if h_tot(i%,2%) < .1 then goto L02870
                 x = ( ( ( h_tot(i%,1%) / h_tot(i%,2%)) - 1.0) * 100)
L02870:       h_tot(i%,3%) = round( x, 1)
              convert h_tot(i%,3%) to h_tot$(i%,3%), pic(##.#-)
              if h_tot(i%,2%) < .1 and h_tot(i%,1%) > .1 then            ~
                 h_tot$(i%,3%) = "++.++"

                                            /* Current Year to Date    */
              x = round( (h_tot(i%,4%) / 1000.0), 0)
                  convert x to h_tot$(i%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
              x = round( (h_tot(i%,5%) / 1000.0), 0)
                  convert x to h_tot$(i%,5%), pic(####,###)
                                            /* % of Change YTD         */
              x = 0.0
              if h_tot(i%,5%) < .1 then goto L03020
                 x = ( ( ( h_tot(i%,4%) / h_tot(i%,5%)) - 1.0) * 100)
L03020:       h_tot(i%,6%) = round( x, 1)
              convert h_tot(i%,6%) to h_tot$(i%,6%), pic(##.#-)
              if h_tot(i%,5%) < .1 and h_tot(i%,4%) > .1 then            ~
                 h_tot$(i%,6%) = "++.++"

        REM - ( Independants )
                                            /* Current Month to Date   */
              x = round( (i_tot(i%,1%) / 1000.0), 0)
                  convert x to i_tot$(i%,1%), pic(####,###)
                                            /* Last Year Month to Date */
              x = round( (i_tot(i%,2%) / 1000.0), 0)
                  convert x to i_tot$(i%,2%), pic(####,###)
                                            /* % of Change MTD         */
              x = 0.0
              if i_tot(i%,2%) < .1 then goto L03180
                 x = ( ( ( i_tot(i%,1%) / i_tot(i%,2%)) - 1.0) * 100)
L03180:       i_tot(i%,3%) = round( x, 1)
              convert i_tot(i%,3%) to i_tot$(i%,3%), pic(##.#-)
              if i_tot(i%,2%) < .1 and i_tot(i%,1%) > .1 then            ~
                 i_tot$(i%,3%) = "++.++"


                                            /* Current Year to Date    */
              x = round( (i_tot(i%,4%) / 1000.0), 0)
                  convert x to i_tot$(i%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
              x = round( (i_tot(i%,5%) / 1000.0), 0)
                  convert x to i_tot$(i%,5%), pic(####,###)
                                            /* % of Change YTD         */
              x = 0.0
              if i_tot(i%,5%) < .1 then goto L03340
                 x = ( ( ( i_tot(i%,4%) / i_tot(i%,5%)) - 1.0) * 100)
L03340:       i_tot(i%,6%) = round( x, 1)
              convert i_tot(i%,6%) to i_tot$(i%,6%), pic(##.#-)
              if i_tot(i%,5%) < .1 and i_tot(i%,4%) > .1 then            ~
                 i_tot$(i%,6%) = "++.++"

        REM - ( TOTAL CHANGE )
              x = h_tot(i%,4%) + i_tot(i%,4%)  /* CURRENT YTD HOME/IND */
              y = h_tot(i%,5%) + i_tot(i%,5%)  /* PREVIOUS YTD HOME/IND*/
              z = 0.0
              if y < .1 then L03450
                 z = ( ( ( x / y ) - 1.0) * 100)
L03450:       i_tot(i%,8%) = round( z, 1)
              convert i_tot(i%,8%) to i_tot$(i%,8%), pic(##.#-)
              if y < .1 and x > .1 then i_tot$(i%,8%) = "++.++"

          next i%
                                              /* BUILD COMBINED TOTALS */
          g_tot(1%) = h_tot(20%,1%) + i_tot(20%,1%)
          x = round( g_tot(1%) / 1000.0, 0)
              convert x to g_tot$(1%), pic(####,###)

          g_tot(2%) = h_tot(20%,2%) + i_tot(20%,2%)
          x = round( g_tot(2%) / 1000.0, 0)
              convert x to g_tot$(2%), pic(####,###)

          g_tot(4%) = h_tot(20%,4%) + i_tot(20%,4%)
          x = round( g_tot(4%) / 1000.0, 0)
              convert x to g_tot$(4%), pic(####,###)

          g_tot(5%) = h_tot(20%,5%) + i_tot(20%,5%)
          x = round( g_tot(5%) / 1000.0, 0)
              convert x to g_tot$(5%), pic(####,###)

          x = 0.0
          if g_tot(2%) < .1 then goto L03700
             x = ( ( ( g_tot(1%) / g_tot(2%) ) - 1.0) * 100)
L03700:   g_tot(3%) = round( x, 1)
          convert g_tot(3%) to g_tot$(3%),pic(##.#-)
          if g_tot(2%) < .1 and g_tot(1%) > .1 then g_tot$(3%) = "++.++"

          x = 0.0
          if g_tot(5%) < .1 then goto L03770
             x = ( ( ( g_tot(4%) / g_tot(5%) ) - 1.0) * 100)
L03770:   g_tot(6%) = round( x, 1)
          convert g_tot(6%) to g_tot$(6%),pic(##.#-)
          if g_tot(5%) < .1 and g_tot(4%) > .1 then g_tot$(6%) = "++.++"
                         /*  (EWD001) - changed for loop to start at 1 */
          for i% = 1% to cnt_max%             /* Mod - 12/19/97        */
        REM IF I% = 8% THEN I% = 19%
              if len(prod_d$(i%)) < 3 then goto L03775 /*(EWD)-04/10/98*/
            print using L05860
            print using L05890, prod_d$(i%), h_tot$(i%,1%), h_tot$(i%,2%), ~
                             h_tot$(i%,3%), h_tot$(i%,4%), h_tot$(i%,5%),~
                             h_tot$(i%,6%), i_tot$(i%,1%), i_tot$(i%,2%),~
                             i_tot$(i%,3%), i_tot$(i%,4%), i_tot$(i%,5%),~
                             i_tot$(i%,6%), i_tot$(i%,8%)
            print using L05860
            if i% < 19% then print using L05920                            ~
                        else print using L05740
L03775:   next i%
                                              /* PRINT COMBINED TOTALS */
          print using L05740
          print using L05740
          print using L05860
          print using L05950, g_tot$(1%), g_tot$(2%), g_tot$(3%),          ~
                            g_tot$(4%), g_tot$(5%), g_tot$(6%)
          print using L05860
          print using L05680

        return

        print_totals       /*  (EWD001) - changed for loop to start at 1 */
          for i% = 1% to cnt_max%           /* Mod - 12/19/97          */
        REM   IF I% = 8% THEN I% = 19%      /* Do Totals Last          */
        REM - ( Home Centers )              /* Current Month to Date   */
              x = round( (h_rt(i%,1%) / 1000.0), 0)
                  convert x to h_rt$(i%,1%), pic(####,###)
                                            /* Last Year Month to Date */
              x = round( (h_rt(i%,2%) / 1000.0), 0)
                  convert x to h_rt$(i%,2%), pic(####,###)
                                            /* % of Change MTD         */
              x = 0.0
              if h_rt(i%,2%) < .1 then goto L04170
                 x = ( ( ( h_rt(i%,1%) / h_rt(i%,2%)) - 1.0) * 100)
L04170:       h_rt(i%,3%) = round( x, 1)
              convert h_rt(i%,3%) to h_rt$(i%,3%), pic(##.#-)
              if h_rt(i%,2%) < .1 and h_rt(i%,1%) > .1 then              ~
                 h_rt$(i%,3%) = "++.++"

                                            /* Current Year to Date    */
              x = round( (h_rt(i%,4%) / 1000.0), 0)
                  convert x to h_rt$(i%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
              x = round( (h_rt(i%,5%) / 1000.0), 0)
                  convert x to h_rt$(i%,5%), pic(####,###)
                                            /* % of Change YTD         */
              x = 0.0
              if h_rt(i%,5%) < .1 then goto L04320
                 x = ( ( ( h_rt(i%,4%) / h_rt(i%,5%)) - 1.0) * 100)
L04320:       h_rt(i%,6%) = round( x, 1)
              convert h_rt(i%,6%) to h_rt$(i%,6%), pic(##.#-)
              if h_rt(i%,5%) < .1 and h_rt(i%,5%) > .1 then              ~
                 h_rt$(i%,6%) = "++.++"
        REM - ( Independants )
                                            /* Current Month to Date   */
              x = round( (i_rt(i%,1%) / 1000.0), 0)
                  convert x to i_rt$(i%,1%), pic(####,###)
                                            /* Last Year Month to Date */
              x = round( (i_rt(i%,2%) / 1000.0), 0)
                  convert x to i_rt$(i%,2%), pic(####,###)
                                            /* % of Change MTD         */
              x = 0.0
              if i_rt(i%,2%) < .1 then goto L04470
                 x = ( ( ( i_rt(i%,1%) / i_rt(i%,2%)) - 1.0) * 100)
L04470:       i_rt(i%,3%) = round( x, 1)
              convert i_rt(i%,3%) to i_rt$(i%,3%), pic(##.#-)
              if i_rt(i%,2%) < .1 and i_rt(i%,1%) > .1 then              ~
                 i_rt$(i%,3%) = "++.++"

                                            /* Current Year to Date    */
              x = round( (i_rt(i%,4%) / 1000.0), 0)
                  convert x to i_rt$(i%,4%), pic(####,###)
                                            /* Last Year Year to Date  */
              x = round( (i_rt(i%,5%) / 1000.0), 0)
                  convert x to i_rt$(i%,5%), pic(####,###)
                                            /* % of Change YTD         */
              x = 0.0
              if i_rt(i%,5%) < .1 then goto L04620
                 x = ( ( ( i_rt(i%,4%) / i_rt(i%,5%)) - 1.0) * 100)
L04620:       i_rt(i%,6%) = round( x, 1)
              convert i_rt(i%,6%) to i_rt$(i%,6%), pic(##.#-)
              if i_rt(i%,5%) < .1 and i_rt(i%,4%) > .1 then              ~
                 i_rt$(i%,6%) = "++.++"
        REM - ( TOTAL CHANGE )
              x = h_rt(i%,4%) + i_rt(i%,4%)    /* CURRENT YTD HOME/IND */
              y = h_rt(i%,5%) + i_rt(i%,5%)    /* PREVIOUS YTD HOME/IND*/
              z = 0.0
              if y < .1 then L04720
                 z = ( ( ( x / y ) - 1.0) * 100)
L04720:       i_rt(i%,8%) = round( z, 1)
              convert i_rt(i%,8%) to i_rt$(i%,8%), pic(##.#-)
              if y < .1 and x > .1 then h_rt$(i%,8%) = "++.++"

          next i%
                                              /* BUILD COMBINED TOTALS */
          g_tot(1%) = h_rt(20%,1%) + i_rt(20%,1%)
          x = round( g_tot(1%) / 1000.0, 0)
              convert x to g_tot$(1%), pic(####,###)

          g_tot(2%) = h_rt(20%,2%) + i_rt(20%,2%)
          x = round( g_tot(2%) / 1000.0, 0)
              convert x to g_tot$(2%), pic(####,###)

          g_tot(4%) = h_rt(20%,4%) + i_rt(20%,4%)
          x = round( g_tot(4%) / 1000.0, 0)
              convert x to g_tot$(4%), pic(####,###)

          g_tot(5%) = h_rt(20%,5%) + i_rt(20%,5%)
          x = round( g_tot(5%) / 1000.0, 0)
              convert x to g_tot$(5%), pic(####,###)

          x = 0.0
          if g_tot(2%) < .1 then goto L04970
             x = ( ( ( g_tot(1%) / g_tot(2%) ) - 1.0) * 100)
L04970:   g_tot(3%) = round( x, 1)
          convert g_tot(3%) to g_tot$(3%),pic(##.#-)
          if g_tot(2%) < .1 and g_tot(1%) > .1 then g_tot$(3%) = "++.++"

          x = 0.0
          if g_tot(5%) < .1 then goto L05040
             x = ( ( ( g_tot(4%) / g_tot(5%) ) - 1.0) * 100)
L05040:   g_tot(6%) = round( x, 1)
          convert g_tot(6%) to g_tot$(6%),pic(##.#-)
          if g_tot(5%) < .1 and g_tot(4%) > .1 then g_tot$(6%) = "++.++"

                         /*  (EWD001) - changed for loop to start at 1 */
          for i% = 1% to cnt_max%             /* Mod - 12/19/97        */
        REM     IF I% = 8% THEN I% = 19%
              if len(prod_d$(i%)) < 3 then goto L05050 /*(EWD)-04/10/98*/     
            print using L05860
            print using L05890, prod_d$(i%), h_rt$(i%,1%), h_rt$(i%,2%),   ~
                             h_rt$(i%,3%), h_rt$(i%,4%), h_rt$(i%,5%),   ~
                             h_rt$(i%,6%), i_rt$(i%,1%), i_rt$(i%,2%),   ~
                             i_rt$(i%,3%), i_rt$(i%,4%), i_rt$(i%,5%),   ~
                             i_rt$(i%,6%), i_rt$(i%,8%)
            print using L05860
            if i% < 19% then print using L05920                            ~
                        else print using L05740
L05050:   next i%
                                              /* PRINT COMBINED TOTALS */
          print using L05740
          print using L05740
          print using L05860
          print using L05950, g_tot$(1%), g_tot$(2%), g_tot$(3%),          ~
                            g_tot$(4%), g_tot$(5%), g_tot$(6%)
          print using L05860
          print using L05680

        return

        page_head              /* Page Heading Print Routine */
            if oldslsman$ = "ALL " then STR(oldslsname$,1%,20%) = "* All *"
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L05680
            print using L05770 , date$, time$, rpttitle$, oldslsman$,   ~
                                 str(oldslsname$,1%,20%)
            print using L05740
            print using L05800, per%
            print using L05740
            print using L05830
            print using L05920
            lcntr% = 7%
        return

        load_descript                  /* Load Descriptions Associated */
            cnt% = 0%                  /* with product areas.          */
            init(" ") cc$, prod_d$()
            str(cc$,1%,9%)   = "SLS CODE5"
        load_nxt_sort
            read #4,key > cc$,using L05510,cc$ ,eod goto L05580
L05510:         FMT CH(24)
            if str(cc$,1%,9%) <> "SLS CODE5" then goto L05580
               convert str(cc$,10%,2%) to cnt%, data goto L05540
L05540:
               get #4,using L05560, prod_d$(cnt%)
L05560:          FMT POS(25), CH(20)
            goto load_nxt_sort
L05580: cnt_max% = cnt%
        return

        get_slsname                                       /* (EWD002) */
            read #5,key = oldslsman$,using L05590,slsname$, eod goto no_slsname
L05590:            FMT XX(4),CH(30)
        no_slsname
        return                                            /* (EWD002) */
        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L05680: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

        %!                                                               ~
        ~                                                                !

L05740: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L05770: %!Run ######## @ ########             ###########################~
        ~############# (In Thousands) Salesman: #### ####################!

L05800: %!Period: ##          !<---------- H o m e   C e n t e r s ------~
        ~---->!!<---------- I n d e p e n d e n t s ---------->!! Total  !

L05830: %! Description        !Curr MTD!Last MTD!%Chg !Curr YTD!Last YTD!~
        ~%Chg !!Curr MTD!Last MTD!%Chg !Curr YTD!Last YTD!%Chg !!% Change!

L05860: %!                    !        !        !     !        !        !~
        ~     !!        !        !     !        !        !     !!        !

L05890: %!####################!########!########!#####!########!########!~
        ~#####!!########!########!#####!########!########!#####!! ###### !

L05920: %!--------------------!--------!--------!-----!--------!--------!~
        ~-----!!--------!--------!-----!--------!--------!-----!!--------!

L05950: %!TOTAL (INDP + H.C.) !########!########!#####!########!########!~
        ~#####!! ------ ! ------ ! --- ! ------ ! ------ ! --- !! ------ !

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        rpt_exit

            end
