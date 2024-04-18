*       ****************************************************************~
*                           ( As of 11/12/97 - RHH  for R6.04.03       *~
*        APCCSTWG - Subroutine to Calculate the Efficiency Hours for   *~
*                   a Specified Department broken down by day. Each    *~
*                   Day is broken down into Regular Hours, Overtime    *~
*                   Hours and Regular and Overtime Wages. The Employees*~
*                   in the Department are saved in ENO$().             *~
*                                                                      *~
*                   SC_DEPT$    = Production Department                *~
*                   SC_YR$      = Production Year                      *~
*                   SC_WK$      = Production Week                      *~
*                   ENO%        = No. Emp's APC and Temp's in Dept     *~
*                   ENO$()      = Emp No.s in Department               *~
*                                 EF(),EF_OV(), WG1(),WG2()            *~
*                                 (I%,APC%) -  I%  = 1% to 7% Days     *~
*                                           - APC% = 1% APC Emp's      *~
*                                           - APC% = 2% Temp Emp's     *~
*                   EF()        = Eff Hrs by Day APC and Temp by Day   *~
*                   EF_OV()     = Eff Overtime Hrs APC and Temp by Day *~
*                   WG1()       = Wage Dol's by Day Reg. APC and Temp  *~
*                   WG2()       = Wage Dol's by Day Overtime APC/Temp  *~
*                   ERR%        = 0% = Ok                              *~
* ******************************************************************** *~
*	Y2K CHECKED - 4/1/1998 - NOTE!!!!  YOU MUST PASS THE SC_YR IN  *~
*	                         AS A BI(2)!!!!!		       *~
*       DJD							       *~
* **********************************************************************
* *02/13/2019! CR-1894  Increase EMP DEPT size to 3                !DES*
* *04/29/2020! CR2490   Format change for employee number          !RDB*
* **********************************************************************

        sub "APCCSTWG" (sc_dept$,        /* Department Code           */ ~
                        sc_yr$,          /* Production Year           */ ~
                        sc_wk$,          /* Production Week           */ ~
                        eno%,            /* Number of Employees       */ ~
                        eno$(),          /* Employee's In Department  */ ~
                        ef(),            /* Efficiency Hours by Day   */ ~
                        ef_ov(),         /* Efficiency Overtime Hours */ ~
                        wg1(),           /* Wages Each Day Regular    */ ~
                        wg2(),           /* Wages Each Day Overtime   */ ~
                        #1,              /* APCEMPLY                  */ ~
                        #2,              /* APCEMPMT                  */ ~
                        err% )           /* Error Code 0% = Ok        */

        dim ee(7%),                      /* Employee Efficiency Hours  */~
            ef(7%,2%),                   /* Efficiency Hours by Day    */~
            ef_ov(7%,2%),                /* Efficiency Overtime Hours  */~
            da(7%),                      /* Employee Total Hours Week  */~
            wg1(7%,2%),                  /* Wages Each Day Regular     */~
            wg2(7%,2%),                  /* Wages Each Day Overtime    */~
            eno$(300%)8,                 /* Employee Numbers           */~
            e_cd$(7%)1,                  /* Proc Action Codes for Week */~
            ef_key$13, ef_key_sav$13,    /* Efficiency Prime Keys      */~
            sc_dept$3,                   /* Department Code            */~
            sc_yr$2,                     /* Production Year            */~
            sc_wk$2,                     /* Production Week            */~
            mt_key$12,                   /* Alt Key                    */~
            mt_dept$3,                   /* Department Code            */~
            mt_proc$1,                   /* Process Action Code        */~
            e_no$8,                      /* Employee Number            */~
            sav_key$12                   /* Save Alt Key               */

        REM - Begin Calculations

           init(" ") ef_key_sav$, eno$()          /* Init Dept Key     */
           mat ef = zer                           /* Efficiency Hours  */
           mat ef_ov = zer                        /* Eff Overtime Hrs  */
           mat wg1   = zer                        /* Wages Regular     */
           mat wg2   = zer                        /* Wages Overtime    */
           err% = 0% : eno% = 0%

           ef_key$     = all(hex(00))
           ef_key_sav$ = all(hex(00))
           str(ef_key$,1%,3%) = sc_dept$
           str(ef_key$,4%,2%) = sc_yr$
           str(ef_key_sav$,1%,5%) = str(ef_key$,1%,5%)
        eff_hrs_nxt
           read #2,key > ef_key$,using L00740  , ef_key$,                   ~
                                                    eod goto eff_hrs_done
L00740:       FMT CH(13)
           if str(ef_key_sav$,1%,5%) <> str(ef_key$,1%,5%) then          ~
                                                        goto eff_hrs_done
           if str(ef_key$,11%,2%) = sc_wk$ then goto L00790
              goto eff_hrs_nxt
L00790:    str(ef_key_sav$,1%,12%) = str(ef_key$,1%,12%)

           mat da = zer                         /* Emp ALL REG HRS     */
           mat ee = zer                         /* Emp Eff Hrs DEPT    */
           init(" ") e_cd$(), e_no$             /* Employee Proc Act   */

           str(mt_key$,1%,9%)  = str(ef_key$,4%,9%)
           str(mt_key$,10%,1%) = " "
           str(sav_key$,1%,9%) = str(mt_key$,1%,9%)
           e_time, e_reg, e_ovr, e_sick, e_vac, e_hol = 0.0
           read #2,key 1% > mt_key$, using L00920  , mt_dept$, mt_key$,     ~
                                           mt_hours%, mt_min%, mt_proc$, ~
                                                    eod goto eff_total
L00920:         FMT CH(3), CH(10), POS(20), 2*BI(2), POS(27), CH(1)
           goto L00970
        eff_hrs
          read #2, using L00920  , mt_dept$, mt_key$, mt_hours%, mt_min%,   ~
                                          mt_proc$, eod goto eff_total
L00970:   if str(sav_key$,1%,9%) <> str(mt_key$,1%,9%) then              ~
                                                        goto eff_total

             p% = pos("ACLNPX" = mt_proc$)
             if p% <> 0% then goto eff_hrs      /* DON'T COUNT FOR PAY */
                                                /* OR EFFICIENCY HOURS */
                                                /* DD% = PRODUCTION DAY*/
             dd% = 1%
             convert str(mt_key$,10%,1%) to dd%, data goto L01060
L01060:
             e_cd$(dd%) = mt_proc$             /* SAVE ACTION CODE    */
             x = (mt_hours% * 60.0) + mt_min%
             if mt_hours% > 23% or mt_min% > 60% then gosub corrupt_data
                if err% <> 0% then exit_program
             if mt_hours% < 0% or mt_min% < 0%   then gosub corrupt_data
                if err% <> 0% then exit_program

             x% = pos("SVH" = mt_proc$)
             if x% = 0% then goto L01240
             on x% goto L01180, L01200, L01220

L01180:         e_sick = round(e_sick + x, 2) : goto eff_hrs

L01200:         e_vac  = round(e_vac  + x, 2) : goto eff_hrs

L01220:         e_hol  = round(e_hol  + x, 2) : goto eff_hrs

L01240:      e_time = round(e_time + x, 2)
             da(dd%) = round(da(dd%) + x, 2)
             if sc_dept$ <> mt_dept$ then goto eff_hrs
             p% = pos("BMOYZ0" = mt_proc$)      /* CHECK HOURS IN DEPT*/
             if p% = 0% then goto eff_hrs
                ee(dd%) = round(ee(dd%) + x, 2)
                                                    /* EFFICIENCY HOURS*/
                                                    /* BY DAY FOR DEPT */
             goto eff_hrs
        eff_total
REM              e_no$ = str(sav_key$,3%,5%)       /* Employee Number     */
              get str(sav_key$,3%,4%) using PD_FMT, emp_nbr
PD_FMT:            FMT PD(4)
              convert db to e_no$, pic(########)

              apc% = 1%                         /* Active APC Employee */
              if str(e_no$,4%,1%) = "8" then apc% = 2% /* Temp Employee*/
              if eno% = 300% then goto L01500
                 eno% = eno% + 1%
L01390:          eno$(eno%) = e_no$
              key_e_no$ = str(sav_key$,3%,5%)
              read #1,key = key_e_no$, using L01420 , e_pay_rate,              ~
                                                eod goto eff_total_nxt
L01420:          FMT POS(250), PD(14,4)
              if e_pay_rate < .1 then goto eff_total_nxt
L01440:       e_pay_ovr = round(e_pay_rate * 1.5, 2)
              goto L01530
        eff_total_nxt
              e_pay_rate = 7.50
              stop " NO PAY RATE FOR EMPLOYEE ---> " & e_no$
              goto L01440
L01500:       stop " Employee Limit Exceeded Department ( "&sc_dept$&" )"
              goto L01390

L01530:                                  /* CONVERT TOTAL MIN TO HOURS */
              x = round((e_time + e_sick + e_vac + e_hol) / 60.0, 2)
              y = round(x - ((e_sick + e_vac) / 60.0), 2)
              e_time = round(e_time / 60.0, 2)
              reg = 40.0                 /* STANDARD REG HOURS */
              reg = round(reg - (e_hol/60.0),2)
              goto L01670                  /* DISABLE CALC - FOR PAYROLL */
                  if y > reg then goto L01640
                     e_reg = e_time
                     e_ovr = 0.0
                     goto L01670
L01640:           e_ovr = y - reg
                  e_reg = e_time - e_ovr

L01670:    gosub calc_time
           str(ef_key$,1%,12%) = str(ef_key_sav$,1%,12%)
           str(ef_key$,13%,1%) = "7"
           goto eff_hrs_nxt

        eff_hrs_done

        goto exit_program

        calc_time                          /* BREAK DOWN WEEKLY HOURS  */
              zz = reg : z = 0.0
          for i% = 1% to 7%                         /* PRODUCTION DAYS */
              y1 = round( da(i%) / 60.0, 2)         /* ALL DEPT'S      */
              if y1 = 0 then goto L01980
                 z = round(z + y1, 2)               /* RUN TOT ALL DEPT*/
              p% = pos("BMOYZ0" = e_cd$(i%))
              if p% = 0% then goto L01980
                 ee(i%) = round(ee(i%) / 60.0, 2)
                 ef(i%,apc%) = round(ef(i%,apc%) + ee(i%), 2)
                                                   /* Reg Pay Dollars */
              wg1(i%,apc%)= round(wg1(i%,apc%) + (ee(i%) * e_pay_rate),2)
              if z <= zz then goto L01980
                 b = ( z - zz )
                 if b <= ee(i%) then goto L01930
                    b = ee(i%)
                                                /* Adjust for Overtime */
L01930:          ef(i%,apc%)    = round(ef(i%,apc%) - b, 2)
                 ef_ov(i%,apc%) = round(ef_ov(i%,apc%) + b, 2)
                 wg1(i%,apc%) = round(wg1(i%,apc%) - ( b * e_pay_rate ),2)
                 wg2(i%,apc%) = round(wg2(i%,apc%) + ( b * e_pay_ovr  ),2)
                 zz = z
L01980:   next i%
        return

        corrupt_data
            e_no$ = str(sav_key$,3%,5%)             /* Employee Number */
            stop "(Error) Payroll Hours Corrupt"
            stop "Dept. = "&sc_dept$&" Week = " & sc_wk$ & " Employee = "~
                  & e_no$
            err% = 1%
            mat ef = zer                          /* Efficiency Hours  */
            mat ef_ov = zer                       /* Eff Overtime Hrs  */
            mat wg1   = zer                       /* Total Eff Hours   */
            mat wg2   = zer                       /* Total Wages Day   */
        return

        exit_program

        end
