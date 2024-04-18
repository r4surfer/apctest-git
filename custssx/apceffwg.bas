REM   ******************************************************************~
*                           ( As of 06/04/2020 )                       *~
*        APCEFFWG - Subroutine to Calculate the Efficiency Hours for   *~
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
*        NOTE - ( NO PAY CODES )  = "ACLNPQXZ"   - eliminated with ADP *~
*             - ( EFF HRS CODES ) = "EFRBMYO0"   - eliminated with ADP *~
*                                                                      *~
************************************************************************~
*       Y2K modifications made 3/25/98 by ERN                          *~
* 02/11/02 ! (EWD001) Mods for ESS time clock and payroll.       ! CMG *~
* 08/06/07 ! (AWD002) mods for 'Z' Variance Code                 ! CMG *~
*03/23/2012! (AWD003) remove 'D' from efficiency hours           ! DES *~
*04/03/2012! (AWD004) update variance codes                      ! CMG *~
*06/01/2012! (AWD005) remove 'D' from efficiency hours           ! DES *~
*01/07/2013! (AWD006) change for '+' floating holiday            ! CMG *~
*05/12/2016! SR74042  Mod to add Var Cd U 1/2 day vacation.      ! PWW *~
*01/18/2018! (CR1274) mod to correct overtime                    ! CMN *~
*01/30/2019! (CR1821) ADP Increase Emp ID field length           ! DES *~
*03/01/2019! (CR1894) Increase EMP DEPT from 2 to 3 bytes        ! DES *~
*06/04/2020! CR2490   Reformat Employee Number                   ! RDB *~
*06/24/2020! (CR2588) daily PER UPMH DF hours                    ! CMN *~
*09/08/2020! (CR2672) PER overtime                               ! CMN *~
************************************************************************

        sub "APCEFFWG" (sc_dept$,        /* Department Code           */ ~
                        sc_yr$,          /* Production Year YYYY      */ ~
                        sc_wk$,          /* Production Week           */ ~
                        sc_day$,         /* Production Day            */ ~
                        ef(),            /* Efficiency Hours by Day   */ ~
                        ef_ov(),         /* Efficiency Overtime Hours */ ~
                        tot_ef(),        /* Total Hours by Day        */ ~
                        wg(),            /* Wage Dollars by Day       */ ~
                        e_shift$,        /* SHIFT CODE - '00' = ALL   */ ~
                        #1,              /* APCEMPLY                  */ ~
                        #2,              /* APCEMPMT                  */ ~
                        #3,              /* EWDEFFPY                  */ ~
                        #4,              /* ADPEMPMT                  */ ~
                        eff%,            /* Call from eff or apcempvr */ ~
                        err% )           /* Error Code 0% = Ok        */

        dim ee(7%),                      /* Employee Efficiency Hours  */~
            ef(7%),                      /* Efficiency Hours by Day    */~
            ef_ov(7%),                   /* Efficiency Overtime Hours  */~
            da(7%),                      /* Employee Total Hours Week  */~
            tot_ef(7%),                  /* Total Hours each Day       */~
            wg(7%),                      /* Total Wages Each Day       */~
/*CR1274*/  regHrs(7%),                  /* Total Regular Hrs Each Day */~
/*CR1274*/  ovrHrs(7%),                  /* Total Overtime Hrs Each Dy */~
            e_cd$(7%)1,                  /* Proc Action Codes for Week */~
/* EF_KEY$12, EF_KEY_SAV$12,     EFFICIENCY PRIME KEYS      */~
            sc_dept$3, pay_key$15,       /* Department Code            */~
            sc_yr$4, sc_yr_bi$2,         /* Production Year            */~
            sc_wk$2, i$1,                /* Production Week            */~
            sc_day$2,                    /* Production Day             */~
/* MT_KEY$12,                   ALT KEY                    */~
            mt_key$23, mt_key_sav$23,    /* Alt Key                    */~
            mt_dept$3, send_dept$3,      /* Department Code            */~
            mt_date$6, sav_pay$10,       /* Current Production Date    */~
            mt_proc$1,                   /* Process Action Code        */~
            mt_user$3,                   /* MT User         (CR2588)   */~
            e_no$5,                      /* Employee Number            */~
            e_nbr$8,                     /* Employee Nbr Binary Convert*/~
            e_shft$2,                    /* Employee Shift Num   EWD001*/~
            e_dept$3,                    /* Employee Dept Num    EWD001*/~
            hdr$40,                      /* ASKUSER                    */~
            msg$(3%)79,                  /* ASKUSER                    */~
            shift$2, e_shift$2,          /* SHIFT CODES                */~
            paycode$10                   /* ADP Pay Code (CR1274)      */

REM - Begin Calculations
           call "SHOSTAT" ("Calc Efficiency Hours for Dept ("&sc_dept$&  ~
                                        ")" )

REM INIT(" ") EF_KEY_SAV$                  /* INIT DEPT KEY     */
           mat ef = zer                           /* Efficiency Hours  */
           mat ef_ov = zer                        /* Eff Overtime Hrs  */
           mat tot_ef = zer                       /* Total Eff Hours   */
           mat wg = zer                           /* Total Wages Day   */
           mat regHrs = zer              /* (CR1274) Total ADP Hrs Day */
           mat ovrHrs = zer              /* (CR1274) Total ADP OvrTim D*/
           mat ee = zer                         /* Emp Eff Hrs DEPT    */


           err% = 0%

           gosub clear_pay
           mt_key$     = all(hex(00))
           mt_key_sav$ = all(hex(00))
           init(" ") e_no$, mt_proc$
           convert sc_yr$ to temp%, data goto exit_program
           sc_yr_bi$ = bin(temp%, 2)
           mt_proc$ = "0"

           str(mt_key$,1%,3%) = sc_dept$                    /* Dept         */
           str(mt_key$,4%,2%) = sc_yr_bi$                   /* Year         */
           str(mt_key$,6%,2%) = sc_wk$                      /* Week         */
           str(mt_key_sav$,1%,7%) = str(mt_key$,1%,7%)      /* Set Save Key */
           err% = 0%
           mat regHrs = zer            /* (CR1274) Total ADP Hrs Dy Per Emp*/
           mat ovrHrs = zer            /* (CR1274) Total ADP OvrTim Dy Per EMP*/
           e_time, e_reg, e_ovr, e_sick, e_vac, e_hol = 0.0
           mt_pay_rate, mt_shf_diff, e_pay_rate, e_pay_ovr = 0.0 /*(CR2672)*/
                                      /* (CR2588) add user, pay & shft_dff */
           read #4, key > mt_key$, using L00730, mt_date$, mt_key$, mt_dept$, ~
                                mt_hours%, mt_min%, mt_user$, mt_pay_rate,    ~
                                        mt_shf_diff, eod goto eff_total
                                  
                e_no$ = str(mt_key$,8%,5%)                   
                goto eff_hrs                               
        eff_hrs_nxt                                      /* (CR2588) */
           read #4, using L00730, mt_date$, mt_key$, mt_dept$,              ~
                                mt_hours%, mt_min%, mt_user$, mt_pay_rate,  ~
                                mt_shf_diff, eod goto eff_total
REM   MT_HOURS%, MT_MIN%, EOD GOTO EFF_HRS_DONE
L00730:          FMT CH(06), CH(23), CH(03), BI(02), BI(02), POS(37), CH(03), ~
                     POS(96), PD(14,4), XX(02), PD(14,4)

REM eff_hrs is to process next employee after next employee is read or end subroutine
eff_hrs:
           if mt_shf_diff > 9999 then mt_shf_diff = 0.00
           if str(mt_key$,1%,7%) <> str(mt_key_sav$,1%,7%) /*test dept, yr, wk*/~
                                        then goto eff_total
           
           cmg% = val(e_no$,4)
           convert cmg% to cmg$, pic (########)           
REM           if cmg$ = "  203302" then call "SHOSTAT" ("EMP 203302 -> ")
REM           if cmg$ = "  203302" then stop
           
           
           if e_no$ <> str(mt_key$,8%,5%) then goto eff_total 
           paycode$ = str(mt_key$,14%,10%)
           gosub check_shift
           if shift% = 1% then goto L00850
             str(mt_key$,13%,1%) = "7"        /* Day */
             goto eff_hrs_nxt

L00850:

            dd% = 1%
            convert str(mt_key$,13%,1%) to dd%, data goto L01100

L01100:
            e_cd$(dd%) = mt_proc$             /* SAVE ACTION CODE    */
            x = (mt_hours% * 60.0) + mt_min%
            if mt_hours% > 23% or mt_min% > 60% then gosub corrupt_data
              if err% <> 0% then eff_hrs_nxt
            if mt_hours% < 0% or mt_min% < 0%   then gosub corrupt_data
               if err% <> 0% then eff_hrs_nxt

            e_time = round(e_time + x, 2)
            da(dd%) = round(da(dd%) + x, 2)
            ee(dd%) = round(ee(dd%) + x, 2)
                                                    /* EFFICIENCY HOURS*/
                                                    /* BY DAY FOR DEPT */
            if paycode$ = "*rg" then regHrs(dd%) = regHrs(dd%) + x
            if paycode$ = "*OT" then ovrHrs(dd%) = ovrHrs(dd%) + x
                                                            /* (CR2588) */
REM            IF MT_USER$ = "DAF" THEN E_PAY_RATE = MT_PAY_RATE + MT_SHF_DIFF   
/*(CR2672)*/
            if mt_user$ = "DAF" and paycode$ = "*rg" then ~
                             e_pay_rate = mt_pay_rate + mt_shf_diff   
            if mt_user$ = "DAF" and paycode$ = "*OT" then ~
                             e_pay_ovr = mt_pay_rate
                             
          goto eff_hrs_nxt
        eff_total    
           if e_pay_rate > 0.00 then goto L01500
                                                  /* By Employee     */
           read #1,key = e_no$, using L01410 , e_dept$, e_pay_rate,  ~
                         e_eff_date$, e_prev_rate, e_shft$,          ~
                                                eod goto eff_total_nxt
L01410:    FMT CH(3), POS(250), PD(14,4), CH(06), POS(264), PD(14,4), ~
               POS(841), CH(2)

/* (CR2672) */          
           if mt_date$ > e_eff_date$ then goto no_pay_change
             if e_prev_rate > 0.00 then e_pay_rate = e_prev_rate

no_pay_change:
REM           IF E_PAY_RATE < .1 THEN GOTO EFF_TOTAL_NXT
           if e_pay_rate < .1 then e_pay_rate = 13.84   /* set default rate */
             if dt_user$ <> "DAF" then                  /*(CR2588)*/~
                               e_pay_ovr = round(e_pay_rate * 1.5, 2)
           goto L01500
        eff_total_nxt
           e_pay_rate = 13.84
/* CR2490 */
            get str(e_no$,1%,4%), using L12345, e_nbr%
            convert e_nbr% to e_nbr$, pic (######00) 
           stop " NO PAY RATE FOR EMPLOYEE ---> " & e_nbr$

L01500:

           gosub calc_time
REM E_NO$ = STR(MT_KEY$,7%,5%)
           mat regHrs = zer           /* (CR1274) Total ADP Hrs Day Per Emp  */
           mat ovrHrs = zer           /* (CR1274) Total ADP OvrTim Dy Per EMP*/
           if str(mt_key$,1%,7%) <> str(mt_key_sav$,1%,7%) /*test dept, yr, wk*/~
                                        then goto eff_hrs_done           
           goto eff_hrs
        eff_hrs_done
          for i% = 1% to 7%
            tot_ef(i%) = ef(i%) + ef_ov(i%)
          next i%
        goto exit_program

        calc_time                          /* BREAK DOWN WEEKLY HOURS  */
          for i% = 1% to 7%                         /* PRODUCTION DAYS */
REM (CR1274) UPDATE VALUES WITH ADP DATA
            regHrs, ovrHrs = 0.00
            regHrs = round(regHrs(i%) / 60.0, 2)
            ovrHrs = round(ovrHrs(i%) / 60.0, 2)

            ef(i%)    = ef(i%) + regHrs
            ef_ov(i%) = ef_ov(i%) + ovrHrs
            wg(i%)    = round(wg(i%) + ( regHrs * e_pay_rate ), 2)
            wg(i%)    = round(wg(i%) + ( ovrHrs * e_pay_ovr  ), 2)
            gosub update_pay
          next i%
          e_no$ = str(mt_key$,8%,5%)
          mat regHrs = zer            /* (CR1274) Total ADP Hrs Dy Per Emp*/
          mat ovrHrs = zer            /* (CR1274) Total ADP OvrTim Dy Per EMP*/
          e_time, e_reg, e_ovr, e_sick, e_vac, e_hol = 0.0
          mt_pay_rate, mt_shf_diff, e_pay_rate, e_pay_ovr = 0.0
        return

        check_shift
            shift% = 0%
            if e_shift$ = "00" then goto L02090
            read #1,key = e_no$ ,using l02070, shift$, ~
                                                 eod goto L02100

REM READ #1,KEY = E_NO$ ,USING L02070, SHIFT$, EOD GOTO L02100
L02070:            FMT POS(841), CH(2)
               if shift$ <> e_shift$ then return
L02090: shift% = 1%
L02100: return

        corrupt_data
REM E_NO$ = STR(SAV_KEY$,3%,5%)             /* EMPLOYEE NUMBER */
/* CR2490 */
            get str(e_no$,1%,4%), using L12345, e_nbr%
L12345:         FMT BI(4)
            convert e_nbr% to e_nbr$, pic (######00) 
                 
            comp% = 2%
            hdr$ = "*** Efficiency Hours Calc  ***"
REM MSG$(1) = "THE PAYROLL HOURS FOR EMPLOYEE ( "& E_NO$&" )"
/* CR2490 */
            msg$(1) = "The Payroll Hours for Employee ( "& e_nbr$ &" )"  
            msg$(2) = "   A r e   C o r r u p t   No   E f f i e n c y  "
            msg$(3) = "Press <RETURN> To Continue, Fix Immediately ???? "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
            err% = 1%
REM MAT EF = ZER                           /* EFFICIENCY HOURS  */
REM MAT EF_OV = ZER                        /* EFF OVERTIME HRS  */
REM MAT TOT_EF = ZER                       /* TOTAL EFF HOURS   */
REM MAT WG = ZER                           /* TOTAL WAGES DAY   */
        return


        update_pay
           if eff% <> 0% then return
           init(" ") pay_key$, send_dept$, i$
           pay = 0.0
           convert i% to i$, pic(0)

/* RDB sc_dept is 3 characters now and sent from apcempvr and ewdefftb */
           send_dept$ = sc_dept$               /* Make 3 char department */

           pay = 0.0
           call "SHOSTAT" ("Updating Department Wages")
                                         /* CAN NOT USE WG(I) B/C     */
REM PAY = E_PAY_RATE * EE(I%)     /* WG(I) IS WAGES FOR ENTIRE */
REM PAY = ROUND(PAY + (E_PAY_OVR * B),4) /* DEPARTMENT         */
           pay = e_pay_rate * regHrs                         /* (CR1274) */
           pay = round(pay + (e_pay_ovr * ovrHrs) ,4)        /* (CR1274) */
   
           str(pay_key$,1%,4%)  = sc_yr$
           str(pay_key$,5%,2%)  = sc_wk$
           str(pay_key$,7%,1%)  = i$
           str(pay_key$,8%,3%)  = send_dept$
           str(pay_key$,11%,5%)  = e_no$

              read #3,hold,key = pay_key$, eod goto L52000
                 delete #3
L52000:       put  #3, using L52010, mt_date$,   /* Production Date */~
                                     sc_yr$,     /* Production Year */~
                                     sc_wk$,     /* Production Week */~
                                     i$,         /* Production Day  */~
                                     send_dept$, /* Production Dept */~
                                     e_no$,      /* Employee Number */~
                                     e_pay_rate, /* Emp Pay Rate    */~
                                     regHrs,     /* Emp Reg (CR1274)*/~
                                     ovrHrs,     /* Emp Ovr (CR1274)*/~
                                     pay,        /* Emp Pay for Day */~
/*(CR2672)*/                         e_pay_ovr   /* Emp Overtime Rat*/

REM EE(I%),     /* EMP REG. HOURS  */~
REM B,          /* EMP OVERTIME HRS*/~
REM PAY         /* EMP PAY FOR DAY */


            write #3, eod goto L52020
L52030:

        return
L52020:     call "SHOSTAT" ("(Error) Updating ----> " & pay_key$)
            stop
            goto L52030

       clear_pay
         if eff% <> 0% then return
         for i% = 1% to 7%
           init(" ") pay_key$, sav_pay$
           convert i% to i$, pic(0)

/*           send_dept$ = "0" & sc_dept$    RDB sc_dept is 3 chars now */
           send_dept$ = sc_dept$

           str(pay_key$,1%,4%)  = sc_yr$
           str(pay_key$,5%,2%)  = sc_wk$
           str(pay_key$,7%,1%)  = i$
           str(pay_key$,8%,3%)  = send_dept$
           str(sav_pay$,1%,10%) = pay_key$
       clear_pay_next
           read #3,hold,key > pay_key$, using L52100, pay_key$,     ~
                                        eod goto clear_pay_done

           if str(pay_key$,1%,10%) <> sav_pay$ then goto clear_pay_done

             delete #3

         goto clear_pay_next
      clear_pay_done
         next i%
      return

L52010:           FMT                                            ~
                                CH(6),      /* Production Date */~
                                CH(4),      /* Production Year */~
                                CH(2),      /* Production Week */~
                                CH(1),      /* Production Day  */~
                                CH(3),      /* Production Dept */~
                                CH(5),      /* Employee Number */~
                                PD(14,4),   /* Emp Pay Rate    */~
                                PD(14,4),   /* Emp Reg. Hours  */~
                                PD(14,4),   /* Emp OverTime Hrs*/~
                                PD(14,4),   /* Emp Pay for Day */~
/*(CR2672)*/                    PD(14,4)    /* Emp Overtime Rate*/

L52100:          FMT  XX(6), CH(15)               /* Pay Key         */


        exit_program

        end
