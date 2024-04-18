        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    SSSS   CCCC  N   N   SSSS  BBBB    *~
            *  A   A  P   P  C   C  S      C      NN  N  S      B   B   *~
            *  AAAAA  PPPP   C       SSS   C      N N N   SSS   BBBB    *~
            *  A   A  P      C   C      S  C      N  NN      S  B   B   *~
            *  A   A  P       CCC   SSSS    CCCC  N   N  SSSS   BBBBB   *~
            *             ( USED BY - APCEMPVR )                        *~
            *-----------------------------------------------------------*~
            * APCSCNSB - Subroutine version of 'APCSCAN2' Display       *~
            *            Employee Routines. Employee Adjustments        *~
            *            Routine.                                       *~
            *                                                           *~
            *        Note - All Entries Made from this Routine will     *~
            *               Cause a Variance for that Employee for      *~
            *               that Production Week or Day.                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/16/93 ! Original                                 ! RHH *~
            * 07/16/93 ! Mods - Lock Check Efficiency for Day,    ! RHH *~
            *          !        No Variance for Transfers - 5040  !     *~
            * 12/22/93 ! Mods - Put in to Effect Audit Tracking   ! RHH *~
            *          !        Display_Audit Screen an Time Stamp!     *~
            *          !        TOTAL_DAY Routine Line (4220)     !     *~
            * 01/16/96 ! Mods - Use new Subroutine to Calc current! RHH *~
            *          !        Production Week and Date. Validate!     *~
            *          !        Entered Production Week and Date. !     *~
            *          !        (Sub - APCPLN0B)                  !     *~
            * 01/08/97 ! Mods - Disable CHECK_EFF Test Temporary  ! RHH *~
            * 10/29/97 ! Mod  - Add Userid to (APCEMPMT) File, to ! RHH *~
            *          !        Show who made last change.        !     *~
            * 10/08/99 ! Mod  - Inable CHECK_EFF Test.            ! CMG *~
            *          !        (EWD001)                          !     *~
            *02/07/00  ! Mod - To only allow persons in           ! TM2 *~
            *          !       APC SECUR to make changes to       !     *~
            *          !       time after 800 am on current day   !     *~
            *          !       (EWD002)                           !     *~
            * 02/12/02 ! (EWD003) Mod to turn off lock after 8 am ! CMG *~
            * 06/21/06 ! (EWD004) Add MKN for NE                  ! DES *~
            * 12/08/06 ! (AWD005) mod to allow time entry to week ! CMG *~
            *          !            51 and week 52 for hol pay    !     *~
            * 07/03/08 ! (AWD007) mod to allow time entry to week ! DES *~
            *          !            27 for holiday pay            !     *~
            * 12/17/08 ! (AWD008) mod to allow time entry to week ! DES *~
            *          !            week 53 for hol pay           !     *~
            * 04/19/11 ! (AWD009) mod to allow time entry to week ! DES *~
            *          !            week 16 for hol pay           !     *~
            *11/12/2012! (AWD010) mod for weeksemp Sunday to Satur! CMG *~
            *03/30/2013! (AWD011) mod for file lengths            ! CMG *~
            *05/28/2014! (AWD012) Add user LNT                    ! PWW *~
            *01/14/2016! SR72002  mod to allow time entry to week ! PWW *~
            *          !            3 for hol pay MLK.            !     *~
            *01/28/2016! SR71637  mod to make advanced time entry ! PWW *~
            *          !            (holidays & such) dynamic by  !     *~
            *          !            storing them in GENCODES table!     *~
            *          !            "EMP OPEN ". Removed hard code!     *~
            *04/05/2016! SR73889  A fix for SR71637.              ! PWW *~
            *02/13/2019! CR-1894  Increase EMP DEP size to 3 bytes! DES *~
            *05/11/2020! CR2490   Employee number size increase   ! RDB *~
            *************************************************************

        sub "APCSCNSB" (#1, #2, #3, #4)    /* Primary Files       */

        dim eff_key$20,                  /* Efficiency Check Key       */~
            dt_usr$3,                    /* TIME STAMP - USER          */~
            dt_dte$6,                    /* TIME STAMP - DATE          */~
            dt_ts$8,  hh$(5)10,          /* TIME STAMP - TIME          */~
            dt_sys_tme$8,                /* System Time (AWD011)       */~
/* ADP DT_FIL1$77,                                                     */~
            x$8, tt$(30%)8,              /* TIME STAMP                 */~
            t_s$(30%)30, ts$(10%)5,      /* TIME STAMP - SCRIPT        */~
            hdr$40, msg$(3%)79,access$30,/* SECURITY FOR DEPARTMENT    */~
            sc_code$1, msg$30,           /* Same as DT_CODE$           */~
            sc_dept$3, sc_dept_d$30,     /* Parent Dept or Override    */~
            sav_dept$3,                  /* USE TO UPDATE (APCEMPMT)   */~
            e_payg$3,                    /* EMPLOYEE PAY GRADE         */~
            rhh$5, rhh1$5,               /* Manual Time Entry          */~
            rhh_desc$30,                 /* LONG FORM OF TIME          */~
            days$(7%)9,                  /* Days of the Week           */~
            days$9,                      /* Day of the Week            */~
            e_dept$3, parent_dept$3,     /* Employee Parient Department*/~
            dt_dept$3, mt_dept$3,        /* Employee Department Code   */~
            dt_yr$4, mt_yr$4, prv_yr$4,  /* Production Year (YY)       */~
            dt_yr_bi$2, mt_yr_bi$2,      /* Binary fmts                */~
            prv_yr_bi$2,                 /*                            */~
            dt_wk$2, mt_wk$2,            /* Employee Production Week   */~
            dt_day$1,mt_day$1,           /* Employee Production Day    */~
            dt_emp$5,mt_emp$5,           /* Employee Number            */~
            mt_date$6, dt_fil$1,         /* Employee Clock Date        */~
            dt_time$5, c_t$(30)5,        /* Employee Clock Time        */~
            c_t1$(30%)5, am_pm$(30%)2,   /* Employee Clock Time  XX:XX */~
            status$(15%)6,               /* Status Code                */~
            dp$(15%)3,                   /* DEPARTMENT CODE            */~
            c_th%(15%), c_tm%(15%),      /* CLOCK HOURS AND MINUTES    */~
            c_th$(15%)2, c_tm$(15%)2,    /* CLOCK HOURS AND MINUTES    */~
            prod_dte$8,                  /* PRODUCTION WEEK DATE       */~
            dt_code$1, dt_desc$30,       /* Employee Clock Code        */~
            mt_var_in$1, mt_var_ind$30,  /* Clock in Variance Code     */~
            mt_var_out$1,mt_var_outd$30, /* Clock Out Variance Code    */~
            mt_var_day$1,mt_var_dayd$30, /* Variance Code For Day      */~
            mt1$1, mt2$1, mt3$1,         /* IN, OUT, DAY VARIANCE      */~
            mt_proc$1,                   /* Master Process Flag        */~
            mt_user$3,                   /* Last Modfied By            */~
            mt_fil$35,                   /* Filler Area                */~
            sav_key$10, sav_key2$10,     /* Copy of Detail Key Prim    */~
            mt_key$13, mt_key1$10,       /* Master Key PRIMARY / ALT   */~
            dt_key$18, dt_rec$128,       /* Detail Primary Key         */~
            dt_sys_dte$6,                /* System Date (AWD011)       */~
            sav_key1$16, sav_hr$2,       /* Detail Primary Key         */~
            e_key$5,                     /* Employee Master Key        */~
            e_lname$15,                  /* Employee Last Name         */~
            e_fname$10,                  /* Employee First Name        */~
/* ADP - E_FNAMEX$5,                        EMPLOYEE FIRST NAME        */~
            e_init$1,                    /* Employee Init              */~
            e_status$1,                  /* Employee Status A,I,T      */~
            e_itime$1, e_itime_d$4,      /* Clock In Time Code         */~
            e_otime$1, e_otime_d$4,      /* Clock Out Time Code        */~
            e_display$1, e_lunch$1,      /* Display Data On Screen     */~
            e_lock$1,                    /* LOCK CLOCK FLAG            */~
            d_hours$2, d_min$2,          /* Daily Hours and Minutes    */~
            w_hours$2, w_min$2,          /* Weekly Hours and Minutes   */~
            name$30,                     /* Display Name               */~
/*CR2490*/  barcode$8,                   /* Employee Number Bar Code   */~
            vac_days$3,                  /* Employee Vacation Days AVAI*/~
            sick_days$3,                 /* Employee Sick Days AVAIL   */~
            points$6,                    /* Employee Current Points    */~
            errormsg$55,                 /* Error Message Display      */~
            readkey$24,                  /* Generic Key                */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8, etime$8,             /* Date for screen display    */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            userid$3, desc$30,           /* Current User Id            */~
            scr_emp$8                    /* Employee number   CR2490   */

        dim                              /* Subroutine - Variables     */~
            cur_yr$4, cur_yr_bi$2,       /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4, ent_yr_bi$2,        /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8        /* Prod Week Date Form/Unform */

        dim                              /* (EWD002)                   */~
            test_time$4,                 /* Test Time to lock out user */~
            sys_time$8,                  /* System Time                */~
            prev_dy$1,                   /* Previous Day               */~
            prev_wk$2, tst_wk$2          /* Previous Week              */

        dim f2%(5%)
        
        dim schema$8                     /* Schema    (AWD011)         */
        
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCEMPLY ! EMPLOYEE MASTER FILE                     *~
            * #2  ! GENCODES ! SYSTEM MASTER TABLE FILES                *~
            * #3  ! APCEMPMT ! EMPLOYEE MASTER TIME FILE                *~
            * #4  ! APCEMPDT ! EMPLOYEE MASTER TIME DETAIL FILE         *~
            * #5  ! APCEFFTB ! EFFICIENCY MASTER REPORT FILE            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            texas% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #2, schema_err%)
            if schema% = 2% then texas% = 1%            
            date$ = date : u3% = 0%
            str(date$,3%,2%) = "01" : str(date$,5%,2%) = "01"

/* (AWD010) */            
REM            DAYS$(1%) = "MONDAY   "  DAYS$(2%) = "TUESDAY  "
REM            DAYS$(3%) = "WEDNESDAY"  DAYS$(4%) = "THURSDAY "
REM            DAYS$(5%) = "FRIDAY   "  DAYS$(6%) = "SATURDAY "
REM            DAYS$(7%) = "SUNDAY   "

            days$(2%) = "MONDAY   " : days$(3%) = "TUESDAY  "
            days$(4%) = "WEDNESDAY" : days$(5%) = "THURSDAY "
            days$(6%) = "FRIDAY   " : days$(7%) = "SATURDAY "
            days$(1%) = "SUNDAY   "
/* (\AWD010) */            
            
            date$ = date
            mt_user$ = userid$
            test_time$ = "0800"
            test_flag% = 0%
            check_time% = 1%                       /*  (EWD003)  */

        REM ************************************************************
        REM *              M a i n   P r o g r a m                     *
        REM ************************************************************

            gosub display_employee
            goto exit_program

        REM ************************************************************
        REM *           ( E n d )  M a i n   P r o g r a m             *
        REM ************************************************************

        REM *************************************************************~
            * Display This Screen If Barcode is Scanned And No          *~
            * Errors Occur.                                             *~
            *************************************************************

        set_error
            init(" ") mt_var_in$, mt_var_out$, mt_var_day$, dt_emp$,     ~
                      name$, dt_wk$, prod_dte$, dt_day$, days$,          ~
                      sc_dept$, sc_dept_d$, dt_yr$, dt_yr_bi$
            i% = 1% : dt_code$ = "N" : rhh$ = "HHMM" : edit% = 0%
            dt_desc$ = "(N) - Display Data Only"
            rhh_desc$ = "Time in Mil. Format or Hrs/Min"
            on emp% goto L01760, L01780, L01800, L01820, L01840, L01860, L01880, L01900, ~
                         L01920, L01940, L01960, L01980, L02000, L02020, L02040, L02060, ~
                         L02080, L02100, L02120, L02130
L01760:        errormsg$ = "Employee Code Not On-File ??            "
                 return                                  /* EMP% =  1% */
L01780:        errormsg$ = "Employee Code is not an Active Code ??  "
                 return                                  /* EMP% =  2% */
L01800:        errormsg$ = "Daily Clock In Code is Undefined ??     "
                 return                                  /* EMP% =  3% */
L01820:        errormsg$ = "Daily Clock Out Code is Undefined ??    "
                 return                                  /* EMP% =  4% */
L01840:        errormsg$ = "Un-Able to Update Detail ??             "
                 return                                  /* EMP% =  5% */
L01860:        errormsg$ = "Already Clocked in for Specified Time?? "
                 return                                  /* EMP% =  6% */
L01880:        errormsg$ = "(Error)-Invalid Time/Code Entry??"
                 return                                  /* EMP% =  7% */
L01900:        errormsg$ = "(Error)-Salaried Employee. N/A ??"
                 return                                  /* EMP% =  8% */
L01920:        errormsg$ = "(Error)-Access Denied to Depart.?"
                 return                                  /* EMP% =  9% */
L01940:        errormsg$ = "(Error)-Future Production Week ??"
                 return                                  /* EMP% = 10% */
L01960:        errormsg$ = "(Error)-Future Production Day  ??"
                 return                                  /* EMP% = 11% */
L01980:        errormsg$ = "(Error)-C l o c k   L o c k e d??"
                 return                                  /* EMP% = 12% */
L02000:        errormsg$ = "(Error)-Invalid Department Sel ??"
                 return                                  /* EMP% = 13% */
L02020:        errormsg$ = "(Error)-Invalid Production day ??"
                 return                                  /* EMP% = 14% */
L02040:        errormsg$ = "(Error)-Transfer Hours Exceed  ??"
                 return                                  /* EMP% = 15% */
L02060:        errormsg$ = "(Error)-Calc Production Data   ??"
                 return                                  /* EMP% = 16% */
L02080:        errormsg$ = "(Error)-Invalid Julian Day     ??"
                 return                                  /* EMP% = 17% */
L02100:        errormsg$ = "(Efficiency Already Processed) Mods Locked for Day?"
                 return                                  /* EMP% = 18% */
L02120:        errormsg$ = "(Error)-Future Production Year ??"
                 return                                  /* EMP% = 19% */
L02130:        errormsg$ = "(Efficiency Being Processed) Mods Locked for Day?"
                 return                                  /* EMP% = 20% */
         

        exit_program
            end

        REM *************************************************************~
            * Scan Employee Data                                        *~
            *                                                           *~
            *************************************************************

        lookup_employee
            d_hours$, d_min$, w_hours$, w_min$ = "00"
            emp% = 0% : parent_dept$ = " "
/* CR2490 */
            e_key$ = dt_emp$
            read #1,key = e_key$, using  L02350, e_dept$, e_payg$,         ~
                                  e_lname$, e_fname$, e_init$, e_status$,~
                                  e_vac_days%, e_vac_used%,              ~
                                  e_sick_days%, e_sick_used%, points,    ~
                                  e_itime$, e_otime$, e_display$,        ~
                                  e_lunch$, e_lock$, /* e_fnamex$, ADP */~
                                  eod goto L02580
L02350:       FMT CH(3), CH(3), POS(12), CH(15), CH(10), CH(1), POS(152),~
                  CH(1), POS(218), BI(2), POS(226), BI(2),               ~
                  POS(234), BI(2), POS(242), BI(2), POS(308),            ~
                  PD(14,4), POS(1013), 5*CH(01) /*, POS(844), CH(5) ADP */
            mt_dept$     = e_dept$
            parent_dept$ = e_dept$
            if e_payg$ = "180" or e_payg$ = "190" or e_payg$ = "200"     ~
                                                     then goto L02620
            name$ = e_lname$ & ", " & e_fname$ & " " & e_init$ & "."
REM ADP  NAME$ = E_LNAME$ & ", " & E_FNAME$ & E_FNAMEX$ &       ~
                    " " & e_init$ & "."
            if sc_dept$ <> " " then goto L02460
               sc_dept$ = mt_dept$
L02460:     if e_status$ <> "A" then goto L02600
REM  GOSUB LOAD_WINDOWS
               if emp% <> 0% then return
               x% = e_vac_days% - e_vac_used%
                  convert x% to vac_days$, pic(###)

               x% = e_sick_days% - e_sick_used%
                  convert x% to sick_days$, pic(###)

               convert points to points$, pic(##.##-)

        return
L02580:    emp% = 1%                           /* Employee Not On File */
        return
L02600:    emp% = 2%                           /* Employee Not Active  */
        return
L02620:    emp% = 8%                           /* Employee SALARIED    */
        return

        load_windows                             /* E_ITIME$, E_OTIME$ */
            t_in%, t_out%, emp% = 0%
            if e_itime$ = "0" then return
            init(" ") readkey$, e_itime_d$
        REM STR(READKEY$,1%,9%)   = "EMP ITIME"
        REM STR(READKEY$,10%,15%) = E_ITIME$
        REM READ #2,KEY = READKEY$,USING 2490 ,E_ITIME_D$, EOD GOTO 2670
L02720:       FMT POS(25), CH(4)
        REM - SET HOURLY WINDOW FOR CLOCKING IN

              e_itime_d$ = time
              convert str(e_itime_d$,1%,2%) to hr1%, data goto L02980

              convert str(e_itime_d$,3%,2%) to hm1%, data goto L02980

              if hm1% > 44% then hr1% = hr1% + 1%
              hm1% = 0%
              convert hr1% to str(e_itime_d$,1%,2%), pic(00)

              str(e_itime_d$,3%,2%) = "00"

              t_in% = (60%*hr1%) + hm1%            /* CLOCK IN MINUTES */
            init(" ") readkey$, e_otime_d$
            str(readkey$,1%,9%)   = "EMP OTIME"
            str(readkey$,10%,15%) = e_otime$
            read #2,key = readkey$,using L02720 ,e_otime_d$, eod goto L03000

              convert str(e_otime_d$,1%,2%) to hr1%, data goto L03000

              convert str(e_otime_d$,3%,2%) to hm1%, data goto L03000

              t_out% = (60%*hr1%) + hm1%          /* CLOCK OUT MINUTES */
        return
L02980:     emp% = 3%                     /* Clock In Code Not Defined */
        return
L03000:     emp% = 4%                    /* Clock Out Code Not Defined */
        return

        calc_production
/*SR71637*/ init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,    ~
                      ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,    ~
                      cur_yr_bi$, ent_yr_bi$, prv_yr_bi$

/* (AWD010) changed from APCPLN0B to APCEMP0B */            
/*SR71637*/call "AWDEMP0B" ( cur_yr_bi$, /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr_bi$, /* Entry Production Year (IN) */~
                             ent_wk$,    /* Entry Prod Week       (IN) */~
                             ent_dy$,    /* Entry Production Day (OPT) */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr_bi$, /* Previous Year              */~
                             #2,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

            if pl_e% <> 0% then goto L03640
            cur_dy2$ = cur_dy$

            init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,    ~
                      ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,    ~
                      cur_yr_bi$, ent_yr_bi$, prv_yr_bi$

            if dt_yr$ = " " then goto L03040
                convert dt_yr$ to temp%, data goto L03640
                ent_yr$ = dt_yr$
                ent_yr_bi$ = bin(temp%, 2)
L03040:     if dt_wk$  <> " " then ent_wk$ = dt_wk$
            if dt_day$ <> " " then ent_dy$ = dt_day$

/* (AWD010) changed from APCPLN0B to APCEMP0B */            
           call "AWDEMP0B" ( cur_yr_bi$, /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr_bi$, /* Entry Production Year (IN) */~
                             ent_wk$,    /* Entry Prod Week       (IN) */~
                             ent_dy$,    /* Entry Production Day (OPT) */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr_bi$, /* Previous Year              */~
                             #2,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

            if pl_e% <> 0% then goto L03640

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)            
            temp% = val(prv_yr_bi$,2)
            convert temp% to prv_yr$, pic (####)

            emp% = 0% : pass% = 0%
            mt_date$   = ent_dte$                      /* Current Date   */
            mt_yr$    = ent_yr$                        /* Master Year    */
            mt_yr_bi$ = ent_yr_bi$
            dt_yr$    = ent_yr$                        /* Detail Year    */
            dt_yr_bi$ = ent_yr_bi$
            sv_wk%    = 0% : sv_day% = 0%
            dt_wk$    = ent_wk$
            dt_day$   = ent_dy$
            prod_dte$ = ent_dte$
            convert dt_wk$ to sv_wk%, data goto L03340
L03340:
            convert dt_day$ to sv_day%, data goto L03360
L03360:
            wk% = sv_wk% : i% = sv_day%
            days$ = days$(i%)
            if calc% = 1% then return

            dt_time$ = time                         /* Check Edit Flag */
            if edit% = 0% then goto L03460
               dt_time$ = rhh$
               if str(dt_time$,1%,1%) = "A" then goto L03520
                                                 /* No Test Adjustment */
L03460:     convert str(dt_time$,1%,4%) to x%, data goto L03640

            gosub calc_prod_day
               sv_wk%  = wk%
               sv_day% = i%

L03520:     convert wk% to dt_wk$, pic(##)         /* Employee Prod Wk */
            convert i% to dt_day$, pic(#)          /* Employee Prod Day*/
            dt_dept$ = mt_dept$                    /* DEPARTMENT       */
            mt_wk$  = dt_wk$   : mt_day$ = dt_day$
            convert scr_emp$ to e_no%, data goto L03640
/* CR2490 */
            put str(dt_emp$,1%,4%), using L12345, e_no%
L12345:           FMT BI(4)                                 /* CR2490 */
            str(dt_emp$,5%,1%) = " "
REM            dt_emp$ = barcode$ 
            mt_emp$ = dt_emp$ /* Employee Number  */
            dt_key$ = all(hex(00))
            str(dt_key$,1%,2%) = dt_yr_bi$ : str(sav_key2$,1%,2%) = mt_yr_bi$
            str(dt_key$,3%,2%) = dt_wk$    : str(sav_key2$,3%,5%) = mt_emp$
            str(dt_key$,5%,1%) = dt_day$   : str(sav_key2$,8%,2%) = mt_wk$
            str(dt_key$,6%,5%) = dt_emp$   : str(sav_key2$,10%,1%)= mt_day$
            sav_key$ = str(dt_key$,1%,10%)
        return
L03640:     emp% = 16%
        return
            emp% = 17%
        return

        write_detail
           mt1$ = "5" : mt2$ = "6" : mt3$ = "9"
           dt_time%, emp% = 0%
           mt_var_in$ = "5"  : mt_var_out$ = "6" : mt_var_day$ = "9"
           gosub find_in_hour
           gosub convert_time
           if emp% <> 0% then return
           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           str(dt_key$,11%,3%) = dt_dept$
           str(dt_key$,14%,3%) = str(dt_time$,1%,3%)
           sav_key1$ = str(dt_key$,1%,16%)
           read #4,key > dt_key$, using L03820, dt_key$, eod goto L03860
L03820:       FMT CH(18)
              if str(dt_key$,14%,1%) = "A" then goto L03860
              if process% = 1% then goto L03860
                 if str(dt_key$,1%,16%) = sav_key1$ then goto L04030
L03860:    gosub check_lock
           if emp% <> 0% then return

           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           str(dt_key$,11%,3%) = dt_dept$
           str(dt_key$,14%,5%) = dt_time$
           read #4,hold,key = dt_key$, eod goto L03950
              delete #4
L03950:    gosub time_stamp
           put #4,using L03990, dt_yr_bi$, dt_wk$, dt_day$, dt_emp$,         ~
                              dt_dept$, dt_time$, dt_code$, sc_code$,        ~
                              dt_fil$, dt_usr$, dt_dte$, dt_ts$, dt_sys_dte$,~
                              dt_sys_tme$ /*, dt_fil1$ ADP */
                                                                 /* (AWD011) */
L03990:       FMT CH(2), CH(2), CH(1), CH(5), CH(3), CH(5), CH(1), CH(1),   ~
                    CH(1), CH(3), CH(6), CH(8), CH(06), CH(08) 
/*, CH(77) ADP ALSO CHANGED DT_FIL IS CH(1) BECAUSE EXTRA VARIABLE SC_CODE$ */
           write #4, eod goto L04050
        return
L04030:    emp% = 6%
        return
L04050:    emp% = 5%
        return

        convert_time
           if str(dt_time$,1%,1%) = "A" then goto L04190
              convert str(dt_time$,1%,2%) to hr1%, data goto L04050

              convert str(dt_time$,3%,2%) to hm1%, data goto L04050

              dt_time% = (60*hr1%) + hm1%
              if dt_code$ <> "1" then goto L04190
                 if hr1% > sav_hr% then goto L04190
                    if hr1% > 9% then goto L04190
                    convert (hr1% + 24%) to str(dt_time$,1%,2%), pic(00)
L04190:    if str(dt_time$,5%,1%) <> "-" then str(dt_time$,5%,1%) = " "
        return

        total_day
           init(" ") c_t$(), status$(), sav_dept$, dp$(), t_s$(), tt$()
           mat c_th% = zer : mat c_tm% = zer
           tm, tm1, tm2 = 0.0
           rhh%, j1%, j2% = 0%
           dt_hours%, dt_min%, mt_hours%, mt_min% = 0%
           pt_hours%, pt_min% = 0%       /* Daily total of Parent Hrs */
           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           read #4,key > dt_key$, using  L04370, dt_key$, d_code$,         ~
                         dt_usr$, dt_dte$, dt_ts$, eod goto total_done
           goto L04380
        total_next
           read #4,using  L04370, dt_key$, d_code$,                        ~
                         dt_usr$, dt_dte$, dt_ts$, eod goto total_done
L04370:       FMT CH(18), CH(1), XX(2), CH(3), CH(6), CH(8)
L04380:    if sav_key$ <> str(dt_key$,1%,10%) then goto total_done
              if sav_dept$ <> " " then goto L04410
                 sav_dept$ = str(dt_key$,11%,3%)
L04410:       if sav_dept$ = str(dt_key$,11%,3%) then goto L04460
                 gosub update_master
                 sav_dept$ = str(dt_key$,11%,3%)
                 mt_hours%, mt_min% = 0%

L04460:       sgn% = 1%
              j1% = j1% + 1%
                                      /* Build Time Stamp Audit Record */
              str(t_s$(j1%),1%,1%)  = d_code$
              str(t_s$(j1%),6%,3%)  = dt_usr$
              x$ = dt_dte$
              call "DATEFMT" (x$)
              str(t_s$(j1%),10%,8%) = x$
              str(t_s$(j1%),20%,8%) = dt_ts$
              x1%, x2% = 0%
              if d_code$ <> "0" and d_code$ <> "1" then goto L04600
              convert str(dt_key$,14%,2%) to x1%, data goto L04580
L04580:
              goto L04620
L04600:          convert str(dt_key$,15%,1%) to x1%, data goto L04610
L04610:
L04620:       convert str(dt_key$,16%,2%) to x2%, data goto L04630
L04630:
              if x1% >= 24% then x1% = x1% - 24%
              convert x1% to str(tt$(j1%),1%,2%), pic(00)
              convert x2% to str(tt$(j1%),4%,2%), pic(00)
              str(tt$(j1%),3%,1%) = ":"
              if d_code$ <> "0" and d_code$ <> "1" then goto L04720
                 str(tt$(j1%),7%,2%) = "AM"
                 if x1% >= 12% then str(tt$(j1%),7%,2%) = "PM"
                 goto L04750
L04720:       str(tt$(j1%),7%,2%) = "++"
              if str(dt_key$,18%,1%) = "-" then str(tt$(j1%),7%,2%) ="--"

L04750:       c_t$(j1%) = str(dt_key$,14%,5%)
              if mod(j1%,2%) = 0 then goto L04800
                 j2% = j2% + 1%
                 status$(j2%) = " Ok  "
                 dp$(j2%) = sav_dept$
L04800:       on pos("012" = d_code$) goto L04820, L04910, L05090
                                                          /* CLOCK IN  */
L04820:          convert str(c_t$(j1%),1%,2%) to hr1%, data goto L04830
L04830:
                 convert str(c_t$(j1%),3%,2%) to mn1%, data goto L04850
L04850:
                 if tm1 = 0.0 then goto L04880
                    status$(j2%) = "No Out"
L04880:          tm1 = 60 * hr1% + mn1%         /* CLOCK IN MINUTES    */
                 goto total_next
                                                          /* CLOCK OUT */
L04910:          convert str(c_t$(j1%),1%,2%) to hr2%, data goto L04920
L04920:
                 if hr2% >= 24% then hr2% = hr2% - 24%
                 convert hr2% to str(c_t$(j1%),1%,2%), pic(00)

                 convert str(c_t$(j1%),3%,2%) to mn2%, data goto L04970
L04970:
                 tm2 = 60 * hr2% + mn2%         /* CLOCK OUT MINUTES   */
                 if tm1 > 0.0 then goto L05040
                    status$(j2%) = "No In "
                    tm = 0.0
                    goto L05200

L05040:          if tm2 <= tm1 then tm2 = tm2 + 1440   /* ADD 24 HOURS */
                 tm  = tm2 - tm1                /* TOTAL CLOCK MINUTES */
                 tm1, tm2 = 0.0
              goto L05200
                                                /* ADJUSTED TIME       */
L05090:       convert str(c_t$(j1%),2%,1%) to hr1%, data goto L05100
L05100:
              convert str(c_t$(j1%),3%,2%) to mn1%, data goto L05120
L05120:
              tm = (60*hr1%) + mn1%
              if str(c_t$(j1%),5%,1%) = "-" then sgn% = -1%
              status$(j2%) = "Adj + "
              if sgn% = -1% then status$(j2%) = "Adj - "
              j1% = j1% + 1%
              c_t$(j1%) = "     "

L05200:       c_th%(j2%) = sgn% * ( int(tm/60.0) )
              c_tm%(j2%) = sgn% * ( mod(tm,60.0) )
              dt_hours% = dt_hours% + c_th%(j2%)
              dt_min%   = dt_min%   + c_tm%(j2%)
              mt_hours% = mt_hours% + c_th%(j2%)
              mt_min%   = mt_min%   + c_tm%(j2%)
              if e_dept$ <> str(dt_key$,11%,3%) then goto total_next
                 pt_hours% = pt_hours% + c_th%(j2%)
                 pt_min%   = pt_min%   + c_tm%(j2%)
              goto total_next
        total_done
          dt_hours% = dt_hours% + int(dt_min%/60.0)
          dt_min%   = mod(dt_min%,60.0)

          pt_hours% = pt_hours% + int(pt_min%/60.0)
          pt_min%   = mod(pt_min%,60.0)
                                               /* CHECK HOURS VALIDITY */
          if dt_hours% >= 0% and dt_hours% < 24% then goto L05400
             dt_hours%, mt_hours%, dt_min%, mt_min% = 0%
             rhh% = 1%
L05400:
          convert dt_hours% to d_hours$, pic(##)    /* DAILY TOTALS    */
          convert dt_min% to d_min$, pic(##)        /* ALL DEPARTMENTS */
        return

        update_master
            p% = pos("N" = dt_code$)    /* (EWD002) - If display only do not update*/
            if p% <> 0% then return
            mt_hours% = mt_hours% + int(mt_min%/60.0) /* BY DEPARTMENT */
            mt_min%   = mod(mt_min%,60.0)
            emp%, f1% = 0%
            mt_proc$ = "0" : mt_fil$ = "   " : code$ = "0"
            mt_key$ = all(hex(00))
            str(mt_key$,1%,3%)  = sav_dept$
            str(mt_key$,4%,10%) = sav_key2$

            read #3,hold,key = mt_key$, using L05560, mt1$, mt2$, mt3$,    ~
                                                           eod goto L05620
L05560:        FMT POS(24), 3*CH(1)
               put #3, using L05580, mt_hours%, mt_min%
L05580:           FMT POS(20), 2*BI(2)
               f1% = 1%
               gosub check_time  /*  (EWD002)  */
               goto write_master

L05620:     put #3, using  L05660, sav_dept$, mt_yr_bi$, mt_emp$, mt_wk$,~
                    mt_day$, mt_date$, mt_hours%, mt_min%,               ~
                    mt_var_in$, mt_var_out$, mt_var_day$, mt_proc$,      ~
                    userid$ /*, mt_fil$ ADP */
L05660:     FMT CH(3), CH(2), CH(5), CH(2), CH(1), CH(6), 2*BI(2), 4*CH(1),~
                   CH(3) /*, CH(35) ADP */ /* (AWD011) */
        write_master
               if dt_code$ = "0" then gosub check_in
               if dt_code$ = "1" then gosub check_out
               if dt_code$ = "2" then gosub check_adj
                                      /* Remove Variance for Transfers */
        REM    IF SC_CODE$ = "3" OR SC_CODE$ = "4" OR SC_CODE$ = "5"     ~
        REM        THEN GOSUB CHECK_ADJ
            put #3, using L05760 , userid$
L05760:        FMT POS(28), CH(3)
            if f1% = 0% then write #3, eod goto L05800                     ~
                        else rewrite #3
        return
L05800:    emp% = 5%
        return

        check_lock
           if dt_code$ <> "0" then return        /* ONLY CHECK IN      */
           if process% = 1% then return          /* TIME STAMP SCREEN  */
           if t_in% = 0% then return             /* NO WINDOW SPECIFIED*/
        REM IF E_LOCK$ = "N" THEN GOTO 5120      /* NO LOCK            */
           goto L05930
              if dt_time% >= (t_in% - 7%) then goto L05930
                 emp% = 12%
        return
                                                 /* ROUND UP CLOCK IN  */
L05930:    if dt_time% >= (t_in% - 15%) and dt_time% <= t_in% then       ~
                                                                goto L05960
        return
L05960:    dt_time% = t_in%
           dt_time$ = e_itime_d$
        return

        check_in
           if t_in% = 0% then L06050
                                                     /* CHECK LATE IN  */
           if dt_time% < (t_in% + 3%) then goto L06050
              code$ = "2" : goto L06080
L06050:    put #3, using  L06060, "0"
L06060:       FMT POS(24), CH(1)
        return
L06080:    put #3, using L06090, code$, "9"
L06090:       FMT POS(24), CH(1), POS(26), CH(1)
        return

        check_out
           if t_out% = 0% then goto L06180
                                                     /* CHECK EARLY OUT*/
           if dt_time% >= (t_out% - 7%) then goto L06170
              code$ = "3" : goto L06240
L06170:    if mt1$ <> "0" then goto L06210
L06180:       put #3, using  L06190, "0", "0"
L06190:         FMT POS(25), CH(1), CH(1)
              return
L06210:       put #3, using  L06220, "0"
L06220:         FMT POS(25), CH(1)
        return
L06240:    put #3, using L06250, code$, "9"
L06250:       FMT POS(25), CH(1), CH(1)
        return

        check_adj                           /* ALWAYS SET HRS VARIANCE */
           put #3, using L06300, "8", "0"          /* SET HOURS VARIANCE */
L06300:       FMT POS(26), CH(1), CH(1)
        return

        display_employee
            gosub set_pf2

L06360:     accept                                                       ~
               at (01,02),                                               ~
                  "Employee Time Entry, Adjustment, and Display Screen", ~
               at (02,02), "Today:",                                     ~
               at (02,09), fac(hex(84)), date$                  , ch(08),~
               at (02,66), "Time :",                                     ~
               at (02,73), fac(hex(84)), etime$                 , ch(08),~
               at (03,15), fac(hex(94)), errormsg$              , ch(55),~
                                                                         ~
               at (04,02), "Employee Number         :",                  ~
               at (04,30), fac(lfac$), scr_emp$                 , ch(08),~
               at (04,40), fac(hex(84)), name$                  , ch(30),~
                                                                         ~
               at (05,02), "Production Week (1-52)  :",                  ~
               at (05,30), fac(lfac$), dt_wk$                   , ch(02),~
               at (05,35), fac(lfac$), dt_yr$                   , ch(04),~
               at (05,40), fac(hex(84)), prod_dte$              , ch(08),~
                                                                         ~
               at (06,02), "Production Day(1 thru 7):",                  ~
               at (06,30), fac(lfac$), dt_day$                  , ch(01),~
               at (06,40), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (07,02), "Clk Code (N,0,1,2,3,4,5):",                  ~
               at (07,30), fac(lfac$), dt_code$                 , ch(01),~
               at (07,40), fac(hex(84)), dt_desc$               , ch(30),~
                                                                         ~
               at (08,02), "Time(HHMM)/Adjust(A0HMM):",                  ~
               at (08,30), fac(lfac$), rhh$                     , ch(05),~
               at (08,40), fac(hex(84)), rhh_desc$              , ch(30),~
                                                                         ~
               at (09,02), "Parent Dept/Trans Dept. :",                  ~
               at (09,30), fac(lfac$), sc_dept$                 , ch(03),~
               at (09,40), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (10,02), fac(hex(a4)), hdr1$                  , ch(15),~
               at (10,19), fac(hex(a4)), hdr2$                  , ch(15),~
               at (10,36), fac(hex(a4)), hdr3$                  , ch(07),~
               at (10,44), fac(hex(a4)), hdr4$                  , ch(09),~
               at (10,55), fac(hex(a4)), hdr5$                  , ch(08),~
               at (10,65), fac(hex(a4)), hdr6$                  , ch(06),~
                                                                         ~
               at (11,07), fac(hex(84)), c_t1$(i%)              , ch(05),~
               at (11,13), fac(hex(84)), am_pm$(i%)             , ch(02),~
               at (11,24), fac(hex(84)), c_t1$(i%+1%)           , ch(05),~
               at (11,30), fac(hex(84)), am_pm$(i%+1%)          , ch(02),~
               at (11,39), fac(hex(84)), c_th$(i%)              , ch(02),~
               at (11,47), fac(hex(84)), c_tm$(i%)              , ch(02),~
               at (11,56), fac(hex(84)), status$(i%)            , ch(06),~
               at (11,67), fac(hex(84)), dp$(i%)                , ch(03),~
                                                                         ~
               at (12,07), fac(hex(84)), c_t1$(i%+2%)           , ch(05),~
               at (12,13), fac(hex(84)), am_pm$(i%+2%)          , ch(02),~
               at (12,24), fac(hex(84)), c_t1$(i%+3%)           , ch(05),~
               at (12,30), fac(hex(84)), am_pm$(i%+3%)          , ch(02),~
               at (12,39), fac(hex(84)), c_th$(i%+1%)           , ch(02),~
               at (12,47), fac(hex(84)), c_tm$(i%+1%)           , ch(02),~
               at (12,56), fac(hex(84)), status$(i%+1%)         , ch(06),~
               at (12,67), fac(hex(84)), dp$(i%+1%)             , ch(03),~
                                                                         ~
               at (13,07), fac(hex(84)), c_t1$(i%+4%)           , ch(05),~
               at (13,13), fac(hex(84)), am_pm$(i%+4%)          , ch(02),~
               at (13,24), fac(hex(84)), c_t1$(i%+5%)           , ch(05),~
               at (13,30), fac(hex(84)), am_pm$(i%+5%)          , ch(02),~
               at (13,39), fac(hex(84)), c_th$(i%+2%)           , ch(02),~
               at (13,47), fac(hex(84)), c_tm$(i%+2%)           , ch(02),~
               at (13,56), fac(hex(84)), status$(i%+2%)         , ch(06),~
               at (13,67), fac(hex(84)), dp$(i%+2%)             , ch(03),~
                                                                         ~
               at (14,07), fac(hex(84)), c_t1$(i%+6%)           , ch(05),~
               at (14,13), fac(hex(84)), am_pm$(i%+6%)          , ch(02),~
               at (14,24), fac(hex(84)), c_t1$(i%+7%)           , ch(05),~
               at (14,30), fac(hex(84)), am_pm$(i%+7%)          , ch(02),~
               at (14,39), fac(hex(84)), c_th$(i%+3%)           , ch(02),~
               at (14,47), fac(hex(84)), c_tm$(i%+3%)           , ch(02),~
               at (14,56), fac(hex(84)), status$(i%+3%)         , ch(06),~
               at (14,67), fac(hex(84)), dp$(i%+3%)             , ch(03),~
                                                                         ~
               at (15,07), fac(hex(84)), c_t1$(i%+8%)           , ch(05),~
               at (15,13), fac(hex(84)), am_pm$(i%+8%)          , ch(02),~
               at (15,24), fac(hex(84)), c_t1$(i%+9%)           , ch(05),~
               at (15,30), fac(hex(84)), am_pm$(i%+9%)          , ch(02),~
               at (15,39), fac(hex(84)), c_th$(i%+4%)           , ch(02),~
               at (15,47), fac(hex(84)), c_tm$(i%+4%)           , ch(02),~
               at (15,56), fac(hex(84)), status$(i%+4%)         , ch(06),~
               at (15,67), fac(hex(84)), dp$(i%+4%)             , ch(03),~
                                                                         ~
               at (16,07), fac(hex(84)), c_t1$(i%+10%)          , ch(05),~
               at (16,13), fac(hex(84)), am_pm$(i%+10%)         , ch(02),~
               at (16,24), fac(hex(84)), c_t1$(i%+11%)          , ch(05),~
               at (16,30), fac(hex(84)), am_pm$(i%+11%)         , ch(02),~
               at (16,39), fac(hex(84)), c_th$(i%+5%)           , ch(02),~
               at (16,47), fac(hex(84)), c_tm$(i%+5%)           , ch(02),~
               at (16,56), fac(hex(84)), status$(i%+5%)         , ch(06),~
               at (16,67), fac(hex(84)), dp$(i%+5%)             , ch(03),~
                                                                         ~
               at (17,07), fac(hex(84)), c_t1$(i%+12%)          , ch(05),~
               at (17,13), fac(hex(84)), am_pm$(i%+12%)         , ch(02),~
               at (17,24), fac(hex(84)), c_t1$(i%+13%)          , ch(05),~
               at (17,30), fac(hex(84)), am_pm$(i%+13%)         , ch(02),~
               at (17,39), fac(hex(84)), c_th$(i%+6%)           , ch(02),~
               at (17,47), fac(hex(84)), c_tm$(i%+6%)           , ch(02),~
               at (17,56), fac(hex(84)), status$(i%+6%)         , ch(06),~
               at (17,67), fac(hex(84)), dp$(i%+6%)             , ch(03),~
                                                                         ~
               at (18,07), fac(hex(84)), c_t1$(i%+14%)          , ch(05),~
               at (18,13), fac(hex(84)), am_pm$(i%+14%)         , ch(02),~
               at (18,24), fac(hex(84)), c_t1$(i%+15%)          , ch(05),~
               at (18,30), fac(hex(84)), am_pm$(i%+15%)         , ch(02),~
               at (18,39), fac(hex(84)), c_th$(i%+7%)           , ch(02),~
               at (18,47), fac(hex(84)), c_tm$(i%+7%)           , ch(02),~
               at (18,56), fac(hex(84)), status$(i%+7%)         , ch(06),~
               at (18,67), fac(hex(84)), dp$(i%+7%)             , ch(03),~
                                                                         ~
               at (19,02), "Clock In Variance Code :",                   ~
               at (19,30), fac(hex(84)), mt_var_in$             , ch(01),~
               at (19,40), fac(hex(84)), mt_var_ind$            , ch(30),~
                                                                         ~
               at (20,02), "Clock Out Variance Code:",                   ~
               at (20,30), fac(hex(84)), mt_var_out$            , ch(01),~
               at (20,40), fac(hex(84)), mt_var_outd$           , ch(30),~
                                                                         ~
               at (21,02), "Daily Variance Code    :",                   ~
               at (21,30), fac(hex(84)), mt_var_day$            , ch(01),~
               at (21,40), fac(hex(84)), mt_var_dayd$           , ch(30),~
                                                                         ~
               at (23,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(1)               , ch(79),~
                                                                         ~
               keys(hex(000107090e0f10)), key(keyhit%)

        close ws

               if keyhit% <> 1% then goto L07710
                  goto display_employee

L07710:        if keyhit% <> 7% then goto L07750
                  gosub display_audit
                  goto L06360

L07750:        if keyhit% <> 14% and keyhit% <> 0% then goto L07810
                  errormsg$ = " "
                  gosub process_data
                  if keyhit% = 0% then goto L07890
                  goto L06360

L07810:        if keyhit% <> 9% then goto L07850
                  gosub display_codes
                  goto L06360

L07850:        if keyhit% <> 15% then L07880
                  call "PRNTSCRN"

L07880:        if keyhit% = 16% then goto exit_program
L07890:           init(" ") c_t1$(), am_pm$(), c_th$(), c_tm$(),         ~
                            status$(), dp$(), c_t$()
                  i% = 1%
                  goto L06360

L07940:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
            init(" ") c_t$(), dt_emp$, dt_wk$, dt_day$, c_th$(), c_tm$(),~
                      prod_dte$, errormsg$, c_t1$(), status$(), days$,   ~
                      d_hours$, d_min$, name$, am_pm$(), dt_time$,       ~
                      dt_code$, dt_desc$, rhh$, mt_var_in$, mt_var_out$, ~
                      mt_var_day$, mt_var_ind$, mt_var_outd$,            ~
                      mt_var_dayd$, mt_dept$, mt_yr$, mt_wk$, mt_day$,   ~
                      mt_emp$, mt_date$, mt1$, mt2$, mt3$, sc_dept$,     ~
                      sc_dept_d$, sc_code$, e_payg$, sav_dept$, dp$(),   ~
                      rhh1$, rhh_desc$, dt_fil$, etime$, dt_yr$,         ~
                      mt_yr_bi$, dt_yr_bi$, scr_emp$
            test_flag% = 0%
            call "TIME" (etime$)
            date$ = date : call "DATEFMT" (date$)
            i% = 1% : dt_code$ = "N" : rhh$ = "HHMM" : edit% = 0%
            dt_desc$ = "(N) - Display Data Only"
            rhh_desc$ = "Time in Mil. Format or Hrs/Min"
            process% = 1%
            inpmessage$ =                                                ~
              "Enter Applicable Employee Data, <Return> to Edit. PF(9) Di~
        ~splay Clock Codes"
            hdr1$ = " Clock In Time "
            hdr2$ = "Clock Out Time "
            hdr3$ = " Hours "
            hdr4$ = " Minutes "
            hdr5$ = " Status "
            hdr6$ = " Dept "
            lfac$ = hex(81)
            pf$(1) = "(1)Select Employee   (14)Process Data   " &        ~
                     "(15)Print Screen       (16)Exit Screen "
        return

        display_codes
            msg$ = "Press <Return> to Continue.   "

L08310:     accept                                                       ~
               at (01,29),                                               ~
                  "Employee Time Clock Edit Codes ",                     ~
               at (03,10),                                               ~
         "(N) - This Code is For Display of Employee Data, No Updating", ~
               at (04,10),                                               ~
         "      of Employee Data will Occurr.                         ", ~
               at (05,10),                                               ~
         "(0) - This Code is Used for Creating a 'Clock In' Time Entry", ~
               at (06,10),                                               ~
         "      The Time Stamp is made for the Time Specified.        ", ~
               at (07,10),                                               ~
         "      (Military Format. ie. 0700 = 7:00 AM )                ", ~
               at (08,10),                                               ~
         "(1) - This Code is Used for Creating a 'Clock Out'Time Entry", ~
               at (09,10),                                               ~
         "      The Time Stamp is made for the Time Specified.        ", ~
               at (10,10),                                               ~
         "      (Military Format. ie. 1445 = 2:45 PM )                ", ~
               at (11,10),                                               ~
         "(2) - This Code is Used for Making Hours or Minutes Adjust- ", ~
               at (12,10),                                               ~
         "      ments to an Employee's Time for a Production Day.     ", ~
               at (13,10),                                               ~
         "      0010- = Subt (10) Minutes, 0100  = Add (1) Hour, Etc. ", ~
               at (14,10),                                               ~
         "(3) - This Code Transfers an Employee from One Dept to anoth", ~
               at (15,10),                                               ~
         "      er. The Employee will be Clocked Out of the Parent    ", ~
               at (16,10),                                               ~
         "      Dept. and (1) Min Later Clocked into the Spec. Dept.  ", ~
               at (17,10),                                               ~
         "(4) - This Code will Clock the Employee Out of the Dept.    ", ~
               at (18,10),                                               ~
         "      Specified. Must be Used When (3) Above Used.          ", ~
               at (19,10),                                               ~
         "(5) - This Code will Transfer 'Hours' or 'Min' Specified.   ", ~
               at (20,10),                                               ~
         "      From the Parent Dept. to the Dept. Specified. The     ", ~
               at (21,10),                                               ~
         "      Employee's Hours will remain Unchanged.               ", ~
               at (23,10), fac(hex(a4)), msg$                   , ch(30),~
         keys(hex(000f)), key(keyhit%)

               if keyhit% <> 15% then goto L08790
                  call "PRNTSCRN"
                  goto L08310

L08790:  close ws
         call "SCREEN" addr("C" , u3%, "I", i$(), cursor%())
        return

        process_data
           gosub check_update                     /* CAN NOT MODIFY    */
           if update% = 0% then return            /* SELF              */

REM           barcode$ = dt_emp$                  /* CR2490 */
           convert scr_emp$ to e_no%, data goto L11890
           put str(dt_emp$,1%,4%), using L12345, e_no%
           str(dt_emp$,5%,1%) = " "
           
           gosub lookup_employee                  /* Get Employee Info */
           if emp% = 0% then goto L08920
              gosub set_error
              return
L08920:    calc% = 0%
           if dt_wk$ <> " " then calc% = 1%     /* Get Production Data */
              gosub calc_production             /* CURRENT WEEK/DAY    */

           dt_key$ = all(hex(00))
           str(dt_key$,1%,2%) = dt_yr_bi$ : mt_yr$ = dt_yr$
                                            mt_yr_bi$ = dt_yr_bi$
           str(dt_key$,3%,2%) = dt_wk$    : mt_wk$ = dt_wk$
           str(dt_key$,5%,1%) = dt_day$   : mt_day$= dt_day$
           str(dt_key$,6%,5%) = dt_emp$   : mt_emp$= dt_emp$

           str(sav_key2$,1%,2%) = dt_yr_bi$
           str(sav_key2$,3%,5%) = dt_emp$
           str(sav_key2$,8%,2%) = dt_wk$
           str(sav_key2$,10%,1%)= dt_day$
           sav_key$ = dt_key$
           dt_dept$ = mt_dept$
           gosub load_access
           if emp% = 0% then goto L09120              /* Codes = N,0,1,2 */
              gosub set_error                       /* Dept Security   */
              return
L09120:    mt_date$ = prod_dte$
           call "DATEFMT" (prod_dte$)
           dt_day% = 0%
           convert dt_day$ to dt_day%, data goto L09170

L09170:    if dt_day% > 0% and dt_day% < 8% then goto L09210
              emp% = 14%
              gosub set_error
              return
L09210:    days$ = days$(dt_day%)
           gosub lookup_dept
           if emp% = 0% then goto L09260
              gosub set_error
              return
L09260:    REM if dt_yr$ > cur_yr$ then emp% = 19%
           if emp% = 0% then goto L09300
              gosub set_error
              return
/*9300:    if dt_yr$ < cur_yr$ then goto L09350                       */
L09300:    if dt_yr$ < cur_yr$ then goto L09390   /*SR71637           */
           if dt_wk$ < cur_wk$ then goto L09390   /*SR71637           */
           if dt_wk$ > cur_wk$ then emp% = 10%
/*SR72002  * * * Removed all hard coding with  SR71637 * * *  */
           gosub check_emp_open              /*SR71637        */

           if emp% = 0% then goto L09350
              gosub set_error
              return
L09350:    if dt_day$ > cur_dy2$ then emp% = 11%

/*SR72002  * * * Removed all hard coding with  SR71637 * * *  */
           gosub check_emp_open              /*SR71637        */

           if emp% = 0% then goto L09390
              gosub set_error
              return
L09390:    if dt_code$ = "N" then goto L09440
              gosub special_edits
              if emp% = 0% then goto L09440
                 gosub set_error
                 return
L09440:    if keyhit% = 0% then return
           if edit% = 0% then goto L09600
           gosub check_time                /*  (EWD002)  */
/* (CR1489) */           
REM IF SC_CODE$ = "0" OR SC_CODE$ = "1" OR SC_CODE$ = "2" THEN GOSUB WRITE_DETAIL
REM IF SC_CODE$ = "3" THEN GOSUB TRANSFER_TO_DEPT
REM IF SC_CODE$ = "4" THEN GOSUB TRANSFER_OUT_DEPT
REM IF SC_CODE$ = "5" THEN GOSUB TRANSFER_HOURS
/* (CR1489) END */
              if emp% = 0% then goto L09550
                 gosub set_error
                 return
L09550:       gosub total_day
              gosub update_master
              if emp% = 0% then goto L09600
                 gosub set_error
                 return
L09600:    gosub total_day

           if j2% > 0% then goto L09650
              errormsg$ = "No Data for Production Day Specified"
              return
L09650:    if dt_code$ <> "N" then lfac$ = hex(84)
           for i% = 1% to j1%
              c_t1$(i%) = str(c_t$(i%),1%,2%) &":"& str(c_t$(i%),3%,2%)
              am_pm$(i%) = "AM"
              if str(c_t$(i%),1%,2%) >= "12" then am_pm$(i%) = "PM"
              if str(c_t$(i%),1%,1%) = "A" or str(c_t$(i%),1%,1%) = " "  ~
                                            then am_pm$(i%) = "AJ"
           next i%
           for i% = 1% to j2%
              convert c_th%(i%) to c_th$(i%), pic(##)
              convert c_tm%(i%) to c_tm$(i%), pic(##)
           next i%
           i% = 1%
           errormsg$ = "Total Hours: "& d_hours$ &"  Minutes: "&d_min$
           if rhh% = 1% then                                             ~
              errormsg$ = "(Error) - Total Hours for Employee Not Valid?"
           gosub load_variance
        return

        special_edits
           emp% = 0%
           sc_code$ = dt_code$
          /* EWD004 */ 
           if userid$ = "MKN" then goto L10010
           if userid$ = "LNT" or userid$ = "PWW" then goto L10010/*<AWD012>*/
           if userid$ = "MVK" or userid$ = "APA" then goto L10010
           gosub check_eff
           if eff% <> 0% then goto L10160

L10010:    if dt_code$ = "0" then dt_desc$ = "(0) - Clock In Time Code "
           if dt_code$ = "1" then dt_desc$ = "(1) - Clock Out Time Code"
           if dt_code$ = "2" then dt_desc$ = "(2) - Employee Time Adj  "
           if dt_code$ = "3" then dt_desc$ = "(3) - Transfer To a Dept "
           if dt_code$ = "4" then dt_desc$ = "(4) - Transfer OutOf Dept"
           if dt_code$ = "5" then dt_desc$ = "(5) - Transfer Hours Dept"
           if dt_code$ <> "2" and dt_code$ <> "5" then goto L10020
              str(rhh$,1%,1%) = "A"
              convert str(rhh$,2%,3%) to x%, data goto L10140

              convert x% to str(rhh$,2%,3%), pic(000)
              goto L10090
L10020:    convert rhh$ to x%, data goto L10140

           convert x% to rhh$, pic(0000-)
           if sc_code$ <> "3" then goto L10090
              convert (x%+1%) to rhh1$, pic(0000-)

              str(rhh1$,5%,1%) = " "
L10090:    edit% = 1%
           dt_time$ = rhh$
           if str(rhh$,5%,1%) <> "-" then str(rhh$,5%,1%) = " "
           gosub convert_time_stamp
        return
L10140:    emp% = 7%
        return
L10160:    if str(eff_key$,1%,1%) = "0" then emp% = 20%             ~
                                        else emp% = 18%
        return

        convert_time_stamp
           if str(rhh$,1%,1%) = "A" then goto L10320
              rhh_desc$ = "Time Stamp = XX:XX AM"
              convert str(rhh$,1%,2%) to x%, data goto L10370

               str(rhh_desc$,17%,2%) = str(rhh$,3%,2%)
               if x% < 12% then goto L10300
                  if x% = 12% then goto L10280
                     x% = (x%-12%)
L10280:           str(rhh_desc$,20%,2%) = "PM"

L10300:    convert x% to str(rhh_desc$,14%,2%), pic(00)
        return
L10320:    rhh_desc$ = "Time = X Hrs and XX Min.-Plus "
           str(rhh_desc$,8%,1%)  = str(rhh$,2%,1%)
           str(rhh_desc$,18%,2%) = str(rhh$,3%,2%)
           if str(rhh$,5%,1%) = "-" then str(rhh_desc$,26%,5%)="Minus"
        return
L10370:    emp% = 7%
        return

        load_variance                                  /* IN, OUT, DAY */
            init(" ") readkey$, mt_var_ind$, mt_var_outd$, mt_var_dayd$
            mt_key1$ = sav_key2$
            read #3,key 1% = mt_key1$, using L10450, mt_var_in$,          ~
                             mt_var_out$, mt_var_day$, eod goto L10570
L10450:         FMT POS(24), 3*CH(1)
            str(readkey$,1%,9%)   = "EMP VARCD"
            str(readkey$,10%,15%) = mt_var_in$
            read #2,key = readkey$,using L10500,mt_var_ind$,              ~
                                                       eod goto L10510
L10500:       FMT POS(25), CH(30)
L10510:     str(readkey$,10%,15%) = mt_var_out$
            read #2,key = readkey$,using L10500,mt_var_outd$,             ~
                                                       eod goto L10550
            str(readkey$,10%,15%) = mt_var_day$
L10550:     read #2,key = readkey$,using L10500,mt_var_dayd$,             ~
                                                       eod goto L10570
L10570: return

        lookup_dept
            emp% = 0%
            p% = pos("N012" = dt_code$)
            if p% <> 0% then goto L10650
               if sc_dept$ = e_dept$ then goto L10730
               goto L10670
L10650:     sc_dept$ = e_dept$

L10670:     init(" ") sc_dept_d$, readkey$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = sc_dept$
            read #2,key = readkey$,using L10710,sc_dept_d$, eod goto L10730
L10710:       FMT POS(25), CH(30)
L10720: return
L10730:     emp% = 13%
        return

        transfer_to_dept
            dt_code$ = "1"                    /* CLOCK OUT OF PARENT   */
            dt_dept$ = mt_dept$
            gosub check_time                  /*  (EWD002)  */
            gosub write_detail
            dt_code$ = "0"                    /* CLOCK INTO OTHER DEPT */
            dt_dept$ = sc_dept$
            dt_time$ = rhh1$
            gosub check_time            /*  (EWD002)  */
            gosub write_detail
        return

        transfer_out_dept
            dt_code$ = "1"                   /* CLOCK OUT OF OTHER DEPT */
            dt_dept$ = sc_dept$
            gosub check_time                 /*  (EWD002)  */
            gosub write_detail
        return

        transfer_hours
            gosub total_day                /* CALC HOURS for Parent Dept*/
            y1%, y2% = 600%                /* for Specified Day         */
            x% = (pt_hours% * 60%) + pt_min%          /* Convert to Min */
            convert str(dt_time$,2%,1%) to y1%, data goto L10970
L10970:
            convert str(dt_time$,3%,2%) to y2%, data goto L10990
L10990:
            x1% =(y1% * 60%) + y2%                    /* Convert to Min */
            if x1% < x% then goto L11050
               emp% = 15%
               return

L11050:     dt_code$ = "2"                 /* SUBTRACT TIME FROM PARENT */
            dt_dept$ = mt_dept$
            str(dt_time$,5%,1%) = "-"
            gosub check_time                /*  (EWD002)  */
            gosub write_detail
            dt_code$ = "2"
            dt_dept$ = sc_dept$            /* ADD TIME TO OTHER DEPT    */
            str(dt_time$,5%,1%) = " "
            gosub check_time               /*  (EWD002)  */
            gosub write_detail
        return

        load_security
            init(" ") readkey$, access$
            security% = 0%
            str(readkey$,1%,9%)   = "EMPDETAIL"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L11210, access$, eod goto L11200
            security% = 1%
L11200: return

            init(" ") readkey$, access$
            security% = 0%
            str(readkey$,1%,9%)   = "EMP SECUR"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L11210, access$, eod goto L11230
L11210:        FMT POS(25), CH(30)
            security% = 1%
L11230: return

        load_access
            init(" ") readkey$, access$
            j% = 1% : access% = 1%
            gosub load_security
            if security% = 1% then return
            str(readkey$,1%,9%)   = "EMPACCESS"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L11340, access$, eod goto L11400
L11340:        FMT POS(25), CH(30)

            for kk% = 1% to 10%
                convert str(access$,j%,3%) to x%, data goto L11400
                if parent_dept$ = str(access$,j%,3%) then goto L11410
                j% = j% + 3%
            next kk%
L11400: goto access_denied
L11410: return

/*  (EWD002)  -- Begin  */
        check_time
            if check_time% <> 0% then return                 /*  (EWD003)  */
            if dt_code$ = "5" then test_time$ = "0730"          ~
                              else test_time$ = "0800"
            if security% = 1% then return
            if test_flag% = 1% then goto L11440
            test_flag% = 1%
            cur_dy%, cur_wk%, cur_yr% = 0%
            convert cur_dy$ to cur_dy%
            convert cur_wk$ to cur_wk%
            convert cur_yr$ to cur_yr%

            cur_dy% = cur_dy% - 1%
            tst_wk% = cur_wk% - 1%
            if cur_dy% <> 0% then goto L11420
               cur_dy% = 7% 
               cur_wk% = cur_wk% - 1%
L11420:     if cur_wk% <> 0% then goto L11430
               cur_wk% = 52%
               cur_yr% = cur_yr% - 1%

L11430:     convert cur_dy% to prev_dy$, pic(#)
            convert cur_wk% to prev_wk$, pic(##)
            convert tst_wk% to tst_wk$, pic(##)
            convert cur_yr% to cur_yr$, pic(####)

L11440:     sys_time$ = time

            if dt_day$ = "5" and tst_wk$ = prev_wk$ then prev_dy$ = "5"
            if dt_day$ = "6" and tst_wk$ = prev_wk$ then prev_dy$ = "6"

            if dt_wk$ = cur_wk$ and dt_day$ = cur_dy$ and            ~
                                         dt_yr$ = cur_yr$ then return
            if dt_yr$ <= cur_yr$ and dt_wk$ < prev_wk$ then goto update_denied
            if dt_yr$ <= cur_yr$ and dt_wk$ <= cur_wk$ and           ~
                             dt_day$ < prev_dy$ then goto update_denied
            if str(sys_time$,1%,4%) > test_time$ then goto update_denied
        return

        update_denied
            emp% = 9% : access% = 0%
            comp% = 2%
            hdr$ = "*** Access Denied to Update ***"
            msg$(1) = "You Do Not Have Access to Update Employee                 "
            msg$(2) = "      D e p a r t m e n t   S e c u r i t y               "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.            "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        goto display_employee      

/*  (EWD002)   --  End  */

        access_denied
            emp% = 9% : access% = 0%
            comp% = 2%
            hdr$ = "*** Access Denied to Dept ***"
            msg$(1) = "You Do Not Have Access to Dept/Employee Selected?"
            msg$(2) = "      D e p a r t m e n t   S e c u r i t y      "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        calc_prod_day
           if dt_code$ <> "1" then return
           if process% = 1% then return
           if str(dt_time$,1%,1%) = "A" then return
           zz% = 901%
           convert str(dt_time$,1%,4%) to zz%, data goto L11590
L11590:
           if zz% > 900% then return
              sv_wk% = wk% : sv_day% = i%

L11630:       convert wk% to dt_wk$, pic(##)
              convert i% to dt_day$, pic(#)

/* CR2490 */
              convert scr_emp$ to e_no%, data goto L11890
              put str(dt_emp$,1%,4%), using L12345, e_no%
              str(dt_emp$,5%,1%) = " "
REM              dt_emp$ = barcode$
              dt_key$ = all(hex(00))
              str(dt_key$,1%,2%) = dt_yr_bi$
              str(dt_key$,3%,2%) = dt_wk$
              str(dt_key$,5%,1%) = dt_day$
              str(dt_key$,6%,5%) = dt_emp$
              read #4,key > dt_key$, using L11740, dt_rec$,               ~
                                                  eod goto L11800
L11740:          FMT CH(128)           /* (AWD011) */
              if str(dt_rec$,1%,10%) <> str(dt_key$,1%,10%) then         ~
                                                  goto L11800
              if pass% = 0% then goto L11890
              if pass% = 1% then return

L11800:       pass% = pass% + 1%                 /* CHECK PREVIOUS DAY */
              if pass% = 2% then goto L11890
                 i% = i% - 1%
                 if i% <> 0% then goto L11860
                    i% = 7%
                    wk% = wk% - 1%
L11860:          if wk% = 0% then wk% = 52%
              goto L11630

L11890:   wk% = sv_wk%
          i%  = sv_day%
        return

        find_in_hour                             /* LAST CLOCK IN TIME */
          if dt_code$ <> "1" then return
             sav_hr$ = " " : sav_hr% = -1%
             dt_key$ = all(hex(00))
             str(dt_key$,1%,10%) = sav_key$

L11990:      read #4,key > dt_key$, using L12010, dt_rec$,                ~
                                    eod goto L12090
L12010:         FMT CH(128)                /* (AWD011) */
             dt_key$ = str(dt_rec$,1%,18%)
             if str(dt_rec$,1%,10%) <> sav_key$ then goto L12090
                if str(dt_rec$,19%,1%) <> "0" then goto L11990
                   sav_hr$ = str(dt_rec$,14%,2%)
                   convert sav_hr$ to sav_hr%, data goto L12090

                   goto L11990
L12090: return

/* (EWD001) */

        check_eff
            goto L12250
L12180:     eff% = 1%
            for i% = 0% to 2%
              eff_key$ = all(hex(00))
              convert i% to str(eff_key$,1%,1%), pic(#)
              str(eff_key$,2%,2%) = dt_wk$
              str(eff_key$,4%,1%) = dt_day$
              read #5,key > eff_key$, using L12210,eff_key$,eod goto L12240
L12210:          FMT CH(20)
              if str(eff_key$,2%,2%) = dt_wk$ and                        ~
                 str(eff_key$,4%,1%) = dt_day$ then goto L12260
L12240:     next i%
L12250: eff% = 0%
L12260: return

/* (EWD001) */

        time_stamp
            dt_usr$ = userid$
            dt_dte$ = date
            dt_sys_dte$ = date          /* (AWD011) */
            dt_ts$ = " "
REM CALL "TIME" (DT_TS$)
            call "TIME" (dt_sys_tme$)    /* (AWD011) */
REM DT_FIL1$ = "   "      ADP

/* (AWD011) */   
            init(" ") dt_ts$
            dt_ts$ = time

            if texas% = 0% then goto badTimeSystem
            cmg% = 0%                        /* Check for 00 Midnight */
            if str(dt_ts$,1,2) = "00" then str(dt_ts$,1,2) = "24"
            convert dt_ts$ to cmg%, data goto badTimeSystem
                                       /* Use six  (6) zeros to subst */
                                       /* bc length of dt_time$ = 8   */
               cmg% = cmg% - 1000000   /* Substract 1 hour from time */

            convert cmg% to dt_ts$, pic(00000000)

badTimeSystem:
            
            h, m = 0
            convert str(dt_ts$,1,2) to h, data goto badTSh
            
badTSh:            
            convert str(dt_ts$,3,2) to m, data goto badTSm
badTSm:            
            dt_ts$ = "  :   AM"
            if h >= 12 then str(dt_ts$,6,3) = " PM"
            if h >  12 then h = h - 12
            if h  =  0 then h = 12
            convert h to str(dt_ts$,,2), pic(##)
            convert m to str(dt_ts$,4,2), pic(00)
        return

        display_audit
            gosub load_security
            if security% = 0% then return

            gosub set_pf3

            accept                                                       ~
               at (01,02),                                               ~
                  "Employee Time Entry, Audit Display ",                 ~
               at (02,02), "Today:",                                     ~
               at (02,09), fac(hex(84)), date$                  , ch(08),~
               at (02,66), "Time :",                                     ~
               at (02,73), fac(hex(84)), etime$                 , ch(08),~
               at (03,15), fac(hex(94)), errormsg$              , ch(55),~
                                                                         ~
               at (04,02), "Employee Number         :",                  ~
               at (04,30), fac(hex(84)), scr_emp$               , ch(08),~
               at (04,40), fac(hex(84)), name$                  , ch(30),~
                                                                         ~
               at (05,02), "Production Week (1-52)  :",                  ~
               at (05,30), fac(hex(84)), dt_wk$                 , ch(02),~
               at (05,40), fac(hex(84)), prod_dte$              , ch(08),~
                                                                         ~
               at (06,02), "Production Day(1 thru 7):",                  ~
               at (06,30), fac(hex(84)), dt_day$                , ch(01),~
               at (06,40), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (08,02), fac(hex(a4)), hh$(1%)                , ch(10),~
               at (08,15), fac(hex(a4)), hh$(2%)                , ch(07),~
               at (08,25), fac(hex(a4)), hh$(3%)                , ch(10),~
               at (08,40), fac(hex(a4)), hh$(4%)                , ch(10),~
               at (08,60), fac(hex(a4)), hh$(5%)                , ch(08),~
                                                                         ~
               at (09,04), fac(hex(84)), str(t_s$(1%),1%,5%)    , ch(05),~
               at (09,17), fac(hex(84)), str(t_s$(1%),6%,3%)    , ch(03),~
               at (09,26), fac(hex(84)), str(t_s$(1%),10%,8%)   , ch(08),~
               at (09,41), fac(hex(84)), str(t_s$(1%),20%,8%)   , ch(08),~
               at (09,60), fac(hex(84)), tt$(1%)                , ch(08),~
                                                                         ~
               at (10,04), fac(hex(84)), str(t_s$(2%),1%,5%)    , ch(05),~
               at (10,17), fac(hex(84)), str(t_s$(2%),6%,3%)    , ch(03),~
               at (10,26), fac(hex(84)), str(t_s$(2%),10%,8%)   , ch(08),~
               at (10,41), fac(hex(84)), str(t_s$(2%),20%,8%)   , ch(08),~
               at (10,60), fac(hex(84)), tt$(2%)                , ch(08),~
                                                                         ~
               at (11,04), fac(hex(84)), str(t_s$(3%),1%,5%)    , ch(05),~
               at (11,17), fac(hex(84)), str(t_s$(3%),6%,3%)    , ch(03),~
               at (11,26), fac(hex(84)), str(t_s$(3%),10%,8%)   , ch(08),~
               at (11,41), fac(hex(84)), str(t_s$(3%),20%,8%)   , ch(08),~
               at (11,60), fac(hex(84)), tt$(3%)                , ch(08),~
                                                                         ~
               at (12,04), fac(hex(84)), str(t_s$(4%),1%,5%)    , ch(05),~
               at (12,17), fac(hex(84)), str(t_s$(4%),6%,3%)    , ch(03),~
               at (12,26), fac(hex(84)), str(t_s$(4%),10%,8%)   , ch(08),~
               at (12,41), fac(hex(84)), str(t_s$(4%),20%,8%)   , ch(08),~
               at (12,60), fac(hex(84)), tt$(4%)                , ch(08),~
                                                                         ~
               at (13,04), fac(hex(84)), str(t_s$(5%),1%,5%)    , ch(05),~
               at (13,17), fac(hex(84)), str(t_s$(5%),6%,3%)    , ch(03),~
               at (13,26), fac(hex(84)), str(t_s$(5%),10%,8%)   , ch(08),~
               at (13,41), fac(hex(84)), str(t_s$(5%),20%,8%)   , ch(08),~
               at (13,60), fac(hex(84)), tt$(5%)                , ch(08),~
                                                                         ~
               at (14,04), fac(hex(84)), str(t_s$(6%),1%,5%)    , ch(05),~
               at (14,17), fac(hex(84)), str(t_s$(6%),6%,3%)    , ch(03),~
               at (14,26), fac(hex(84)), str(t_s$(6%),10%,8%)   , ch(08),~
               at (14,41), fac(hex(84)), str(t_s$(6%),20%,8%)   , ch(08),~
               at (14,60), fac(hex(84)), tt$(6%)                , ch(08),~
                                                                         ~
               at (15,04), fac(hex(84)), str(t_s$(7%),1%,5%)    , ch(05),~
               at (15,17), fac(hex(84)), str(t_s$(7%),6%,3%)    , ch(03),~
               at (15,26), fac(hex(84)), str(t_s$(7%),10%,8%)   , ch(08),~
               at (15,41), fac(hex(84)), str(t_s$(7%),20%,8%)   , ch(08),~
               at (15,60), fac(hex(84)), tt$(7%)                , ch(08),~
                                                                         ~
               at (16,04), fac(hex(84)), str(t_s$(8%),1%,5%)    , ch(05),~
               at (16,17), fac(hex(84)), str(t_s$(8%),6%,3%)    , ch(03),~
               at (16,26), fac(hex(84)), str(t_s$(8%),10%,8%)   , ch(08),~
               at (16,41), fac(hex(84)), str(t_s$(8%),20%,8%)   , ch(08),~
               at (16,60), fac(hex(84)), tt$(8%)                , ch(08),~
                                                                         ~
               at (17,04), fac(hex(84)), str(t_s$(9%),1%,5%)    , ch(05),~
               at (17,17), fac(hex(84)), str(t_s$(9%),6%,3%)    , ch(03),~
               at (17,26), fac(hex(84)), str(t_s$(9%),10%,8%)   , ch(08),~
               at (17,41), fac(hex(84)), str(t_s$(9%),20%,8%)   , ch(08),~
               at (17,60), fac(hex(84)), tt$(9%)                , ch(08),~
                                                                         ~
               at (18,04), fac(hex(84)), str(t_s$(10%),1%,5%)   , ch(05),~
               at (18,17), fac(hex(84)), str(t_s$(10%),6%,3%)   , ch(03),~
               at (18,26), fac(hex(84)), str(t_s$(10%),10%,8%)  , ch(08),~
               at (18,41), fac(hex(84)), str(t_s$(10%),20%,8%)  , ch(08),~
               at (18,60), fac(hex(84)), tt$(10%)               , ch(08),~
                                                                         ~
               at (19,04), fac(hex(84)), str(t_s$(11%),1%,5%)   , ch(05),~
               at (19,17), fac(hex(84)), str(t_s$(11%),6%,3%)   , ch(03),~
               at (19,26), fac(hex(84)), str(t_s$(11%),10%,8%)  , ch(08),~
               at (19,41), fac(hex(84)), str(t_s$(11%),20%,8%)  , ch(08),~
               at (19,60), fac(hex(84)), tt$(11%)               , ch(08),~
                                                                         ~
               at (20,04), fac(hex(84)), str(t_s$(12%),1%,5%)   , ch(05),~
               at (20,17), fac(hex(84)), str(t_s$(12%),6%,3%)   , ch(03),~
               at (20,26), fac(hex(84)), str(t_s$(12%),10%,8%)  , ch(08),~
               at (20,41), fac(hex(84)), str(t_s$(12%),20%,8%)  , ch(08),~
               at (20,60), fac(hex(84)), tt$(12%)               , ch(08),~
                                                                         ~
               at (21,04), fac(hex(84)), str(t_s$(13%),1%,5%)   , ch(05),~
               at (21,17), fac(hex(84)), str(t_s$(13%),6%,3%)   , ch(03),~
               at (21,26), fac(hex(84)), str(t_s$(13%),10%,8%)  , ch(08),~
               at (21,41), fac(hex(84)), str(t_s$(13%),20%,8%)  , ch(08),~
               at (21,60), fac(hex(84)), tt$(13%)               , ch(08),~
                                                                         ~
               at (22,04), fac(hex(84)), str(t_s$(14%),1%,5%)   , ch(05),~
               at (22,17), fac(hex(84)), str(t_s$(14%),6%,3%)   , ch(03),~
               at (22,26), fac(hex(84)), str(t_s$(14%),10%,8%)  , ch(08),~
               at (22,41), fac(hex(84)), str(t_s$(14%),20%,8%)  , ch(08),~
               at (22,60), fac(hex(84)), tt$(14%)               , ch(08),~
                                                                         ~
               at (23,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(1)               , ch(79),~
                                                                         ~
               keys(hex(0001ffff0f10)), key(keyhit%)

               if keyhit% <> 15% then L13610
                  call "PRNTSCRN"

L13610:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf3
            ts$(1%) = " In  "  : ts$(4%) = "T-To "
            ts$(2%) = " Out "  : ts$(5%) = "T-Out"
            ts$(3%) = " Adj "  : ts$(6%) = "T-Hrs"

            hh$(1%) = "Entry Code"
            hh$(2%) = "User Id"
            hh$(3%) = "Entry Date"
            hh$(4%) = "Entry Time"
            hh$(5%) = "Time/Adj"

            call "TIME" (etime$)
            date$ = date : call "DATEFMT" (date$)
            inpmessage$ = "Enter Return to Continue?"
            pf$(1) = "(1)Start Over                           " &        ~
                     "(15)Print Screen       (16)Exit Screen "
            for k% = 1% to 30%
                x% = -1%
                convert str(t_s$(k%),1%,1%) to x%, data goto L13840
L13840:
                if x% = -1% then goto L13870
                   str(t_s$(k%),1%,5%) = ts$(x%+1%)
L13870:     next k%
        return

        check_update
            update% = 1%
            readkey$ = " "
            str(readkey$,1%,9%)   = "EMP USERS"
            str(readkey$,10%,15%) = dt_emp$
            read #2,key = readkey$,using L13960, desc$,eod goto L13990
L13960:        FMT POS(25), CH(30)
            if str(desc$,1%,3%) <> userid$ then return
        update% = 0%
L13990: return

/*SR71637 + */ 
        check_emp_open
            readkey$ = " "
            str(readkey$,1%,9%)   = "EMP OPEN "
        next_emp_open
            read #2,key > readkey$,using N13960, readkey$,      ~
                                   eod goto N13990
N13960:        FMT POS(1), CH(24)
            if str(readkey$,1%,9%) <> "EMP OPEN " then N13990
               if dt_yr$ <> str(readkey$,10%,4%) then goto next_emp_open
               if dt_wk$ <> str(readkey$,14%,2%) then goto next_emp_open
               if dt_day$ <> str(readkey$,16%,1%) then goto next_emp_open
               emp% = 0%
N13990: return
/*SR71637 - */
