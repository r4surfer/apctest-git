*       ****************************************************************~
*                           ( As of 11/12/97 - RHH for R6.04.03 )      *~
*        APCWGUPD - Subroutine to RECALCULATE the Efficiency Hours for *~
*                   a Specified Department broken down by day. Each    *~
*                   Day is broken down into Regular Hours, Overtime    *~
*                   Hours and Wages for Day.                           *~
*                                                                      *~
*                   SC_DEPT$    = Production Department                *~
*                   SC_YR$      = Production Year YYYY                 *~
*                   SC_WK$      = Production Week                      *~
*                   EF()        = Efficiency Hours by Day              *~
*                   EF_OV()     = Efficiency Overtime Hours by Day     *~
*                   TOT_EF()    = Total Efficiency Hours by Day        *~
*                   WG()        = Wage Dollars for Each Day            *~
*                   ERR%        = 0% = Ok                              *~
*                                                                      *~
*        NOTE - ( NO PAY CODES )  = "ACLNPQX"                          *~
*             - ( EFF HRS CODES ) = "EFRBMYZO0"                        *~
*                                                                      *~
*       ****************************************************************~
*       Y2K modifications made 3/25/98 by ERN                          *~
*       * 03/04/2020 ! CR1894 increase the sc_dept$ to 3          ! RDB*~
*       ****************************************************************

        sub "EWDWGUPD" (sc_dept$,        /* Department Code           */ ~
                        sc_yr$,          /* Production Year YYYY      */ ~
                        sc_wk$,          /* Production Week           */ ~
                        sc_day%,         /* Production Day            */ ~
                        ed_day%,         /* End Production Day        */ ~
                        ef(),            /* Efficiency Hours by Day   */ ~
                        ef_ov(),         /* Efficiency Overtime Hours */ ~
                        tot_ef(),        /* Total Hours by Day        */ ~
                        wg(),            /* Wage Dollars by Day       */ ~
                        #1,              /* EWDEFFPY                  */ ~
                        err% )           /* Error Code 0% = Ok        */

        dim                                                              ~
            ef(7%),                      /* Efficiency Hours by Day    */~
            ef_ov(7%),                   /* Efficiency Overtime Hours  */~
            tot_ef(7%),                  /* Total Hours each Day       */~
            wg(7%),                      /* Total Wages Each Day       */~
            ef_key$15, ef_key_sav$15,    /* Efficiency Prime Keys      */~
            sc_dept$3,                   /* Department Code   CR1894   */~
            sc_yr$4,                     /* Production Year            */~
            sc_wk$2,                     /* Production Week            */~
            i$1                          /* Production Day             */

        REM - Begin Calculations
           call "SHOSTAT" ("Calc Efficiency Hours for Dept ("&sc_dept$&  ~
                                        ")" )

           mat ef = zer                           /* Efficiency Hours  */
           mat ef_ov = zer                        /* Eff Overtime Hrs  */
           mat tot_ef = zer                       /* Total Eff Hours   */
           mat wg = zer                           /* Total Wages Day   */
           err% = 0%

        for i% = sc_day% to ed_day%
           convert i% to i$, pic(#)
           ef_key$     = all(hex(00))
           ef_key_sav$ = all(hex(00))
           str(ef_key$,1%,4%) = sc_yr$
           str(ef_key$,5%,2%) = sc_wk$
           str(ef_key$,7%,1%) = i$
           str(ef_key$,8%,3%) = sc_dept$

           str(ef_key_sav$,1%,10%) = str(ef_key$,1%,10%)
        eff_hrs_nxt
           eff_rate, eff_hrsr, eff_hrso, eff_tot = 0.0
           read #1,key > ef_key$,using L00730, ef_key$, eff_rate, eff_hrsr,  ~
                                               eff_hrso, eff_tot,            ~    
                                               eod goto eff_hrs_done
L00730:       FMT XX(6), CH(15), 4*PD(14,4)
           if str(ef_key_sav$,1%,10%) <> str(ef_key$,1%,10%) then          ~
                                                        goto eff_hrs_done

           ef(i%)     = ef(i%) + eff_hrsr
           ef_ov(i%)  = ef_ov(i%) + eff_hrso
           tot_ef(i%) = tot_ef(i%) + eff_hrsr + eff_hrso
           wg(i%)     = wg(i%) + eff_tot
           goto eff_hrs_nxt
        eff_hrs_done
        next i%

        end