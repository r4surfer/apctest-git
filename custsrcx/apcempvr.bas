         REM *************************************************************~
            *   Special Mod - REM'S at Lines 50960 and 50980            *~
            *                                                           *~
            *   AAA   PPPP    CCC   EEEEE  M   M  PPPP   V   V  RRRR    *~
            *  A   A  P   P  C   C  E      MM MM  P   P  V   V  R   R   *~
            *  AAAAA  PPPP   C      EEEE   M M M  PPPP   V   V  RRRR    *~
            *  A   A  P      C   C  E      M   M  P       V V   R   R   *~
            *  A   A  P       CCC   EEEEE  M   M  P        V    R    R  *~
            *             ( USES SUBROUTINE 'APCSCNSB' )                *~
            *-----------------------------------------------------------*~
            * APCEMPVR - Employee Time Clock Utility Program to Display *~
            *            and Edit Daily Production Code Variance's. By  *~
            *            Department and Production Week. With Report.   *~
            *                                                           *~
            *    Notes - Valid Daily Var Codes. (0,1,2,3,4,5,6,7,8,9)   *~
            *              ( A Thru Z ) are Invalid                (P%) *~
            *                                                           *~
            *            Valid Daily Process Codes. (0, A Thru Z ) (P1%)*~
            *              ( P1% = 0% is Only Valid when P% = 0% )      *~
            *                                                           *~
            *            The Modification and Updating of Variance Codes*~
            *            Can "Only" be done when Using Selection (1)    *~
            *            Selection only "Open Variance Codes".          *~
            *************************************************************~
            *            ( Subroutine - STORE_DATA_S )                  *~
            *    The Automatic Creation of Time for a Missing Day is    *~
            *    "Only Done" When Production Week Equals Current Week.  *~
            *    When Production Day Equals Current Day, and the        *~
            *    Current Time is "After 3 PM or 1500 Hours."            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/01/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/20/95 ! Mod to put Dept Shift Code on Screen     ! RHH *~
            *          !   and Change Variances by Shift.         !     *~
            * 03/24/95 ! Mod to (APCEFFWG) for Shift Codes.       ! RHH *~
            *          !   No Pay Codes - 'ACLNPQX'               !     *~
            *          !   Efficiency Hrs - 'BMYZO0EFR'           !     *~
            *          !   Not Valid Temps - '0 thru 9 'BDHJSVRQ' !     *~
            *          !   Update Employee - 'SVXYZEF' Position   !     *~
            *          !   is Important?                          !     *~
            * 01/16/96 ! Mod to use new subroutine to calculate   ! RHH *~
            *          !   the current production year, week, day !     *~
            *          !   and validate the entered production    ! RHH *~
            *          !   year, week, and day. (Sub-APCPLN0B)    !     *~
            * 03/01/96 ! Mod to Support the New Points File, and  ! RHH *~
            *          !   to Calculate the Correct Points.       !     *~
            * 10/30/97 ! Mod put the Userid in the (APCEMPMT) file! RHH *~
            *          !   to show who made the last change. Also !     *~
            *          !   show the 'CLOCK_IN' time with the name ! RHH *~
            *          !   on the variance screen.                !     *~
            * 11/08/97 ! Special Mod for Mellissa (MVK).          ! RHH *~
            * 03/26/98 ! Y2K modifications                        ! ERN *~
            * 03/10/99 ! (EWD001) - Security Modes for MVK and LBH! RHH *~
            *          !   and LLJ.                               !     *~
            * 02/17/00 ! (EWD002) - Mod not to allow team leader  ! CMG *~
            *          !   to update variances after 8 am for     !     *~
            *          !   previous day.                          !     *~
            * 04/10/01 ! (EWD003) - Mod to take out only hr seeing! CMG *~
            *          !   efficiency wages.                      !     *~
            * 07/02/01 ! (EWD004) Mod so that can handle 1/2 sick ! CMG *~
            *          !        days.                             !     *~
            * 02/12/02 ! (EWD005) Mod to turn off lock after 8 am ! CMG *~
            * 12/09/02 ! (EWD006) Mod to turn on 1 1/2 points for ! CMG *~
            *          !        12 hours shift employees.         !     *~
            * 01/21/05 ! (AWD009) Mod so that can handle 1/2 sick ! CMG *~
            *          !        days.                             !     *~
            * 03/29/05 ! (AWD010) Mod so Melissa Bristow can look ! CMG *~
            *          !  employee time by shift for all dept but !     *~
            *          !  not see pay information EMP SECUR       !     *~
            * 12/19/05 ! (AWD011) Mod so Melissa Bristow can look ! CMG *~
            *          !  employee time by shift for all dept but !     *~
            *          !  not see pay information EMP SECUR       !     *~
            * 06/21/06 ! (EWD012) - Security Modes for MKN        ! DES *~
            *11/26/2007! (EWD013) - fix vac prob w/multi line upd ! DES *~
            *04/16/2008! (EWD014) - add code for 1.5pts w/Vac     ! DES *~
            *06/20/2008! (EWD015) - add 1, 1.25 & 1.5pts w/pay    ! DES *~
            *06/24/2008! (AWD016) - combine codes as per M. Kiger ! DES *~
            *07/03/2008! (AWD017) - allow holiday pay for week 27 ! DES *~
            *10/23/2008! (AWD018) - add new leave variance codes  ! DES *~
            *11/25/2008! (AWD019) - allow holiday pay for week 48 ! DES *~
            *12/17/2008! (AWD020) - allow holiday pay for week 53 ! DES *~
            *06/30/2009! (AWD021) - allow holiday pay for week 26 ! DES *~
            *09/24/2009! (aWD022) - add new leave variance code   ! DES *~
            *03/02/2010! (AWD023) - fix pay codes                 ! DES *~
            *03/24/2010! (AWD024) - Add 1/4 & 1/2 w/ no pay       ! DES *~
            *06/14/2010! (AWD025) - Add "M" Military Leave (no pay! DES *~
            *07/13/2010! (AWD026) - Add more varience codes       ! DES *~
            *10/05/2010! (AWD027) - More varience codes yet again ! DES *~
            *12/21/2010! (AWD028) - Add 1.5 day vacation back     ! DES *~
            *01/27/2011! (AWD029) - Add Imclement weather codes   ! DES *~
            *02/09/2011! (AWD030) - Remove 1.5 day vacation again ! DES *~
            *03/25/2011! (AWD031) - Add FMLA/WC "-"               ! DES *~
            *04/19/2011! (AWD032) - allow holiday pay for week 16 ! DES *~
            *07/08/2011! (AWD033) - don't change pt dte on sick   ! DES *~
            *01/20/2012! (AWD034) - Change "Q" to pay             ! DES *~
            *01/25/2012! (AWD035) - Change "T" to 4 hours pay     ! DES *~
            *08/08/2012! (AWD036) - display system time as well as! CMG *~
            *          !   new rounded time                       !     *~
            *12/03/2012! (AWD037) mod for weeksemp Sunday to Satur! CMG *~
            *01/07/2013! (AWD038) mods for variance code '+'      ! CMG *~
            *02/08/2013! (AWD039) mod to use NTX                  ! CMG *~            
            *03/30/2013! (AWD040) mod for file lengths            ! CMG *~
            *12/11/2013! (AWD041) mod to change vac and sick earn ! CMG *~
            *          !  to allow half day entries               !     *~            
            *05/28/2014! (AWD042) Add user LNT.                   ! PWW *~
            *09/19/2014! (AWD043) Update if "+" Floating Holiday. ! PWW *~
            *03/05/2015! (AWD044) Added 1/2 Day"}" Sick(Paid) and ! PWW *~
            *          !          1 Day"{" Sick(Paid).            !     *~
            *01/14/2016! SR72002  mod to allow time entry to week ! PWW *~
            *          !            3 for hol pay MLK.            !     *~
            *01/26/2016! SR71317  Added Drs Note "=" no point(no  ! PWW *~
            *          !          pay) and School Note ";" no     !     *~
            *          !          point(no pay).                  !     *~
            *02/02/2016! SR71637  mod to make advanced time entry ! PWW *~
            *          !            (holidays & such) dynamic by  !     *~
            *          !            storing them in GENCODES table!     *~
            *          !            "EMP OPEN ". Removed hard code!     *~
            *03/22/2016! SR71860  Add "[" 2 pts No Call/No Show   ! PWW *~
            *          !                                          !     *~
            *04/04/2016! SR73889  A fix for SR71637.              ! PWW *~
            *07/20/2016! SR76090  Remove point for "X".           ! PWW *~
            *01/30/2019! CR-1821 ADP Increase Emp ID field length ! DES *~
            *03/13/2019! CR-1828 In crease the EMP DEPT to 3 bytes! DES *~
            *03/11/2020! CR2467  Allow user entry of 2 char dept  ! RDB *~
            *05/07/2020! CR2490  Increase employee number size    ! RDB *~
            *************************************************************

        dim                                                              ~
            dt_key$18, sav_key1$13,      /* Primary Key                */~
            dt_usr$3,                    /* User Id                    */~
/* ADP DT_REC$40,                                                      */~
            dt_dte$6, dt_code$1,         /* Time Stamp - Date          */~
            dt_ts$8, clock_in$8,         /* Time Stamp - Time          */~
            dt_sys_dte$6,                /* System Date (AWD040)       */~
            system_in$8,                 /* System Time (AWD036)       */~
            am_pm$2,                     /* AM PM       (AWD036)       */~
/* ADP DT_FIL1$85,                          TIME STAMP - FILLER (2)    */~
            codes$20, r_h$70,            /* USE FOR INVALID CODES      */~
            pt_code$1, pt_reason$30,     /* Update Points Codes        */~
            ptcdeupd$1,                  /* ADP Update Code            */~
            ptnotes$50,                  /* ADP Update Notes           */~
            ptupdtuser$3,                /* ADP Update User            */~
            access$30, hdr$40,msg$(3%)79,/* SECURITY DEPARTMENT        */~
            sav_dept$3, save_dte$8,      /* SAVE DEPARTMENT CODE/DATE  */~
            sc_dept$3, sc_dept_d$30,     /* SCREEN DEPARTMENT          */~
            sc_wk$2, sc_wk_dte$8,        /* SCREEN WEEK                */~
            sc_day$1,                    /* SCREEN PRODUCTION DAY      */~
            sv_wk$2, sv_day$1,           /* CURRENT WEEK AND DAY       */~
            days$(7%)9,                  /* DAYS OF THE WEEK           */~
            days$9,                      /* DAY OF THE WEEK            */~
            date$8,                      /* SCREEN DATE                */~
            xx_dept$3, emp_shft$2,       /* Need for New Points File   */~
            mt_dept$3, mt_dept_d$30,     /* Department Code            */~
            mt_dept_sav$30,              /* Department DESCR           */~
            mt_wk$2,                     /* Production Week            */~
            mt_day$1,                    /* Production Day             */~
            mt_date$6, mt_date_dte$8,    /* Current Date (PRODUCTION)  */~
            mt_yr$4, prv_yr$4, sc1_yr$4, /* Current Year - PREVIOUS YR */~
            sc_yr$4,                     /*                            */~
            mt_yr_bi$2, prv_yr_bi$2,     /* Binary fmts                */~
            mt_emp$5,                    /* EMPLOYEE NUMBER            */~
            mt_hours$2,                  /* DAILY HOURS EMPLOYEE       */~
            mt_min$2,                    /* DAILY MINUTES EMPLOYEE     */~
            hours$5,                     /* HOURS AND MINUTES          */~
            mt_var_in$1,                 /* CLOCK IN VARIANCE          */~
            mt_var_ind$15,               /*                            */~
            mt_var_out$1,                /* CLOCK OUT VARIANCE         */~
            mt_var_outd$15,              /*                            */~
            mt_var_day$1,                /* DAILY VARIANCE             */~
            mt_var_dayd$15,              /*                            */~
            mt_var_desc$15,              /* DAILY VARIANCE DESCRIPT    */~
            mt_proc$1,                   /* PROCESS/CHANGE FLAG        */~
            mt_procd$15,                 /*                            */~
            mt_user$3,                   /* Last Modified By           */~
/* ADP MT_FIL$35,                           FILLER AREA                */~
/* ADP MT_REC$32,                           APCEMPMT - RECORD          */~
            jdate$5,                     /* Julian Date                */~
            mt_key$13, mt_key1$10,       /* Master Key(s)              */~
            sav_key$13, sav_emp$7,       /* Save Master Key, Emp/Wk    */~
            sel$1, sel_d$30,             /* Variance Selection         */~
            e_key$11,                    /* Employee Master Key        */~
            e_status$1,                  /* Employee Status Code A,I,T */~
            e_days$7,                    /* Employee WORK DAYS (1 - 7) */~
            hdr1$1,                      /* Modify Process Variance Cd */~
            hdr2$1,                      /* Production Day             */~
            hdr3$3,                      /* Department Code            */~
            hdr4$5,                      /* Employee Number            */~
            hdr5$19,                     /* Last Name,First & Clock In */~
            hdr6$5,                      /* I/O/D - Variance Codes     */~
            hdr7$12,                     /* Daily Variance Descript    */~
            hdr8$12,                     /* Process Variance Descript  */~
            hdr9$5,                      /* Daily Hours                */~
            hdr10$8,                     /* (AWD036) System Time       */~
            cv$(500%)1,                  /* Variance codes             */~
            cd$(500%)1,                  /* Modified Variance Codes    */~
            dd$(500%)1,                  /* PRODUCTION DAYS            */~
            dp$(500%)3,                  /* DEPARTMENT CODES           */~
            em$(500%)8,                  /* EMPLOYEE NUMBERS   CR2049  */~
            vu(500%),                    /* vac used sum   EWD013      */~
            su(500%),                    /* sick used sum  EWD013      */~
            fu(500%),                    /* float used sum  AWD043     */~
            nn$(500%)23, name$20,        /* EMPLOYEE NAMES    CR2490   */~
            sf%(500%),                   /* hours for shift            */~
            vv$(500%)5,                  /* I/O/D - Variance Codes     */~
            dv$(500%)15,                 /* Daily Variance Description */~
            di$(500%)15,                 /* Process Action Description */~
            hh$(500%)5,                  /* DAILY EMPLOYEE HOURS       */~
            sh$(500%)8,                  /* DAILY SYSTEM HOURS (AWD036)*/~
            sk$(500%)13,                 /* SAVE KEY TO - APCEMPMT     */~
            lf$(500%)1,                  /* Control Name Display       */~
            e_dept$3, m_key$10,mm_key$18,/* EMPLOYEE PARENT DEPT       */~
            e_lname$15,                  /* EMPLOYEE LAST NAME         */~
            e_fname$10,                  /* EMPLOYEE FIRST NAME        */~
            e_init$1,                    /* EMPLOYEE MIDDLE INITIAL    */~
            e_payg$3,                    /* EMPLOYEE PAY GRADE         */~
            title$40,                    /* REPORT TITLE               */~
            runtime$8,                   /* REPORT RUN TIME            */~
            readkey$50,                  /* Generic Key                */~
            descr$60,                    /* LOOKUP DESCRIPTION         */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            lfac$1,                      /* Edit Area                  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        dim ef(7%), ef$(7%)10,           /* Efficiency Hours           */~
            ef_ov(7%), ef_ov$(7%)10,     /* Efficiency Overtime Hours  */~
            tot_ef(7%),                  /* TOTAL EFFICIENCY HOURS     */~
            wg(7%), wg$(7%),             /* TOTAL WAGES                */~
            tot_ef$(7%)10,               /* Total Efficiency Hours Day */~
            eff_text$31,                 /* Efficiency Screen Text     */~
            hdr$(5%)25,                  /* Efficiency Headers         */~
            e_shift$2, shift$2,          /* Department Shift Code      */~
            e_shift_d$30                 /* Shift Description          */

        dim                              /* Subroutine - Variables     */~
            cur_yr$4, cur_yr_bi$2,       /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4,  ent_yr_bi$2,      /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8        /* Prod Week Date Form/Unform */

        dim                              /* (EWD002)                   */~
            test_time$4,                 /* Test Time to lock out user */~
            sys_time$8,                  /* System Time                */~
            prev_dy$1,                   /* Previous Day               */~
            prev_wk$2, tst_wk$2           /* Previous Week              */

        dim schema$8                     /* Schema                     */        
        
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Employee Time Clock Variance Maintenance"
            pname$ = "APCEMPVR - Rev: R6.04"

        REM *************************************************************

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
            * #1  ! APCEMPLY ! Employee Master File                     *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! APCEMPMT ! Employee Master Time File                *~
            * #4  ! APCEMPDT ! Employee Master Time Detail File         *~
            * #5  ! APCEMPPT ! APC Employee Points Audit File           *~
            * #6  ! APCEFFTB ! APC EFFICIENCY MASTER FILE               *~
            * #8  ! APCEMPXX ! TIME DETAIL FILE CONVERTED               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "APCEMPLY",                                     ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    7, keylen =    5,                    ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos =   12, keylen =  26, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,   "APCEMPMT",                                     ~
                        varc,     indexed,  recsize =  128,  /* ADP */   ~
                        keypos =    1, keylen =   13,                    ~
                        alt key  1, keypos =    4, keylen =  10, dup

            select #4,  "APCEMPDT",                                      ~
                        varc,     indexed,  recsize =  128, /* AWD040 */~
                        keypos =    1, keylen =  18,                    ~
                        alt key  1, keypos =  114, keylen =  06, dup

            select #5,   "APCEMPPT",                                     ~
                        varc,     indexed,  recsize =  128,  /*ADP*/     ~
                        keypos =    7, keylen =   16,                    ~
                        alt key  1, keypos =    1, keylen =  22

            select #6,  "EWDEFFPY",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  7,   keylen =  15,                     ~
                        alt key  1, keypos  =     1, keylen = 21
                        
            select #15, "ADPEMPMT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 7,    keylen =  23,                     ~
                        alt key 1, keypos = 10, keylen = 23,             ~
                            key 2, keypos =  1, keylen = 29,             ~
                            key 3, keypos = 90, keylen =  6, dup                                   

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),500%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),500%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))


            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date : u3% = 0%
            check_time% = 1%                       /*  (EWD005)  */
            str(date$,3%,2%) = "01" : str(date$,5%,2%) = "01"

/* (AWD037) */
/* (CR1489) */
            days$(1%) = "MONDAY   " : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY" : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   " : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "
            date$ = date
            call "DATEFMT" (date$)

            hdr1$ = "C"                      /* Variance Code To Change*/
            hdr2$ = "D"                      /* Production Day         */
            hdr3$ = "Dep"                     /* Department Code        */
            hdr4$ = "EmpNo"                  /* Employee Number        */
            hdr5$ = "Last Name    -Adj Time"
            hdr6$ = "I/O/D"
            hdr7$ = "Daily Code Desc"
            hdr8$ = "Process Action "
            hdr9$ = "Hours"
REM            hdr10$ = "Clk Time"              /* (AWD036) CR2490 */
            
/* (AWD039) */            
            init(" ") schema$    :  schema% = 0%
            call "SCHEMA" (schema$, schema%, #2, schema_err%)            

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 13% then gosub time_clock
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 11% then gosub scan_data
                  if keyhit%  = 13% then gosub time_clock
                  if keyhit%  = 14% then gosub print_report
REM IF KEYHIT%  = 16% THEN GOSUB UPDATE_DATA
                  if keyhit%  = 16% then gosub exit_program /* (CR1489) */
                  if keyhit% <>  0% then       editpg1
                  if e_max% <> 0% then goto editpg1

L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            rpt% = 1%
            gosub select_printer
            gosub scan_data
            gosub print_totals
            gosub report_totals
            print using L55040
            close printer
        return clear all
        goto inputmode

        select_printer
            call "SHOSTAT" ("Printing Report")
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
            title$ = "  Employee Time Clock Variance Report   "
            t_hrs%, t_min% = 0%
            t_hrs_p%, t_min_p% = 0%
            t_hrs_d%, t_min_d% = 0%
            t_hrs_e%, t_min_e% = 0%
            tot_d, tot_e = 0.0
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%

         return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Department Code, or for 'Report Only' (AA)=ALL?",~
         "Enter a Valid Shift Code, 01, 02, 03, etc or 00 = 'ALL'?     ",~
         "Enter a Valid Production Week,(1-52),Blank=Curr Wk,for 'Report ~
        ~Only' (AA)=ALL?",                                                ~
         "Enter a Valid Production Day (1-7), Blank=Curr Day, or (A) = AL~
        ~L Prod. Days?",                                                  ~
         "Enter Selection: 1-Open,2-Closed,3-No In,4-No Out,5-ALL Codes,6~
        ~-CHK Employee's"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, mt_dept$, mt_dept_d$,      ~
                      mt_wk$, mt_date_dte$, mt_day$, sel$, sel_d$,       ~
                      cd$(), dd$(), dp$(), em$(), nn$(), dv$(), mt_date$,~
                      mt_yr$, jdate$, days$, cv$(), mt_key$, sav_key$,   ~
                      hh$(), mt_emp$, mt_hours$, mt_min$, hours$,        ~
                      mt_var_in$, mt_var_out$, mt_var_day$,mt_var_desc$, ~
                      mt_proc$, sc_dept$, sc_dept_d$,                    ~
                      sc_wk$, sc_wk_dte$, sc_day$, e_key$, e_status$,    ~
                      sk$(), title$, runtime$, name$, sav_emp$, di$(),   ~
                      vv$(), mt_key1$, sav_dept$, save_dte$, m_key$,     ~
                      mt_dept_sav$, access$, hdr$, msg$(), e_dept$,      ~
                      e_lname$, e_fname$, e_init$, pt_code$, pt_reason$, ~
                      sc1_yr$, e_shift$, e_shift_d$, shift$, lf$(), sh$(),~
                      ptcdeupd$, ptnotes$, ptupdtuser$
                      
            for didx = 1 to 500
               vu(didx) = 0
               su(didx) = 0
               fu(didx) = 0                      /*AWD043 */
            next didx
            
            rpt% = 0%
            lfac$ = hex(84) : e% = 1% : e_max% = 0%
            init(hex(84)) lf$()

            test_time$ = "0800"
            test_flag% = 0%

        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #3, using L35040, mt_dept$, mt_yr_bi$, mt_emp$, mt_wk$,  ~
                    mt_day$, mt_date$, mt_hours%, mt_min%, mt_var_in$,   ~
                    mt_var_out$, mt_var_day$, mt_proc$, mt_user$
/* ADP , MT_FIL$ */
            temp% = val(mt_yr_bi$,2)
            convert temp% to mt_yr$, pic (####)
            mt_date_dte$     = mt_date$
            str(rhh$,1%,3%)  = mt_dept$
            str(rhh$,4%,2%)  = mt_yr_bi$
            str(rhh$,6%,5%)  = mt_emp$
            str(rhh$,11%,2%) = mt_wk$
            str(rhh$,13%,1%) = mt_day$

            mt_date_dte$ = mt_date$
            call "DATEFMT" (mt_date_dte$)
            init(" ") readkey$, mt_dept_d$, mt_var_desc$, mt_procd$
            str(readkey$,1%,9%) = "EMP DEPT "
            str(readkey$,10%,15%) = mt_dept$
            read #2,key = readkey$, using L30150,mt_dept_d$,eod goto L30160
L30150:         FMT POS(25), CH(30)
L30160:     str(readkey$,1%,9%) = "EMP VARCD"
            str(readkey$,10%,15%) = mt_var_day$
            read #2,key = readkey$, using L30230,mt_var_desc$,            ~
                                                           eod goto L30240
            str(readkey$,10%,15%) = mt_proc$
            read #2,key = readkey$, using L30230,mt_procd$,               ~
                                                           eod goto L30240
L30230:         FMT POS(25), CH(15)
L30240:     convert mt_hours% to mt_hours$, pic(00)
            convert mt_min%   to mt_min$,   pic(00)
            hours$ = mt_hours$ & ":" & mt_min$
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (APCEMPMT)          */
L35040: FMT CH(03),                      /* Employee Department Code   */~
            CH(02),                      /* Production Year (YY)       */~
            CH(05),                      /* Employee Number            */~
            CH(02),                      /* Employee Production Week   */~
            CH(01),                      /* Employee Production Day    */~
            CH(06),                      /* Production Date            */~
            BI(2),                       /* Employee Hours for Day     */~
            BI(2),                       /* Employee Minutes for Day   */~
            CH(01),                      /* Clock In Variance          */~
            CH(01),                      /* Clock Out Variance         */~
            CH(01),                      /* Daily Variance             */~
            CH(01),                      /* Emp Record Changed         */~
            CH(03)                      /* Last Modified By           */
/* ADP CH(35)                        FILLER AREA    (AWD040)    */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1 : lfac$ = hex(84) : mod% = 0%
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Department Code, ALL */~
                                L40210,         /* Dept Shift Code 00=al*/~
                                L40200,         /* Production Week, ALL */~
                                L40200,         /* Production Day or All*/~
                                L40210          /* Variance Type Select */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

/* CR2490  Remove Clk_Time and make em 8 then adjust screen */
L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Department Code, (ALL):",                    ~
               at (03,30), fac(lfac$(1%)), sc_dept$             , ch(03),~
               at (03,40), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (04,02), "Dept Shift Code 00=All:",                    ~
               at (04,30), fac(lfac$(2%)), e_shift$             , ch(02),~
               at (04,40), fac(hex(84)), e_shift_d$             , ch(30),~
                                                                         ~
               at (05,02), "Production Week, 1-52:",                     ~
               at (05,30), fac(lfac$(3%)), sc_wk$               , ch(02),~
               at (05,40), fac(hex(84)), sc_wk_dte$             , ch(08),~
               at (05,50), fac(lfac$(3%)), sc1_yr$              , ch(04),~
                                                                         ~
               at (06,02), "Production Day(1 - 7):",                     ~
               at (06,30), fac(lfac$(4%)), sc_day$              , ch(01),~
               at (06,40), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (07,02), "Variance Selection   :",                     ~
               at (07,30), fac(lfac$(5%)), sel$                 , ch(01),~
               at (07,40), fac(hex(84)), sel_d$                 , ch(30),~
                                                                         ~
               at (09,02), fac(hex(a4)), hdr1$                  , ch(01),~
               at (09,04), fac(hex(a4)), hdr2$                  , ch(01),~
               at (09,07), fac(hex(a4)), hdr3$                  , ch(03),~
               at (09,11), fac(hex(a4)), hdr4$                  , ch(08),~
               at (09,21), fac(hex(a4)), hdr5$                  , ch(25),~
               at (09,44), fac(hex(a4)), hdr6$                  , ch(05),~
/* CR2490      at (09,41), fac(hex(a4)), hdr10$                 , ch(08),  */ ~
               at (09,50), fac(hex(a4)), hdr7$                  , ch(12),~
               at (09,63), fac(hex(a4)), hdr8$                  , ch(12),~
               at (09,76), fac(hex(a4)), hdr9$                  , ch(05),~
                                                                         ~
               at (10,02), fac(lfac$),   cd$(e%  )              , ch(01),~
               at (10,04), fac(hex(84)), dd$(e%  )              , ch(01),~
               at (10,07), fac(hex(84)), dp$(e%  )              , ch(03),~
               at (10,11), fac(hex(84)), em$(e%  )              , ch(08),~
               at (10,21), fac(lf$(e%)), nn$(e%  )              , ch(25),~
               at (10,44), fac(hex(84)), vv$(e%  )              , ch(05),~
/* CR2490      at (10,41), fac(hex(84)), sh$(e%  )              , ch(08),  */ ~
               at (10,50), fac(hex(84)), dv$(e%  )              , ch(12),~
               at (10,63), fac(hex(84)), di$(e%  )              , ch(12),~
               at (10,76), fac(hex(84)), hh$(e%  )              , ch(05),~
                                                                         ~
               at (11,02), fac(lfac$),   cd$(e%+1%)             , ch(01),~
               at (11,04), fac(hex(84)), dd$(e%+1%)             , ch(01),~
               at (11,07), fac(hex(84)), dp$(e%+1%)             , ch(03),~
               at (11,11), fac(hex(84)), em$(e%+1%)             , ch(08),~
               at (11,21), fac(lf$(e%+1%)), nn$(e%+1%)          , ch(25),~
               at (11,44), fac(hex(84)), vv$(e%+1%)             , ch(05),~
/* CR2490      at (11,41), fac(hex(84)), sh$(e%+1%)             , ch(08),  */ ~
               at (11,50), fac(hex(84)), dv$(e%+1%)             , ch(12),~
               at (11,63), fac(hex(84)), di$(e%+1%)             , ch(12),~
               at (11,76), fac(hex(84)), hh$(e%+1%)             , ch(05),~
                                                                         ~
               at (12,02), fac(lfac$),   cd$(e%+2%)             , ch(01),~
               at (12,04), fac(hex(84)), dd$(e%+2%)             , ch(01),~
               at (12,07), fac(hex(84)), dp$(e%+2%)             , ch(03),~
               at (12,11), fac(hex(84)), em$(e%+2%)             , ch(08),~
               at (12,21), fac(lf$(e%+2%)), nn$(e%+2%)          , ch(25),~
               at (12,44), fac(hex(84)), vv$(e%+2%)             , ch(05),~
/* CR2490      at (12,41), fac(hex(84)), sh$(e%+2%)             , ch(08),  */ ~
               at (12,50), fac(hex(84)), dv$(e%+2%)             , ch(12),~
               at (12,63), fac(hex(84)), di$(e%+2%)             , ch(12),~
               at (12,76), fac(hex(84)), hh$(e%+2%)             , ch(05),~
                                                                         ~
               at (13,02), fac(lfac$),   cd$(e%+3%)             , ch(01),~
               at (13,04), fac(hex(84)), dd$(e%+3%)             , ch(01),~
               at (13,07), fac(hex(84)), dp$(e%+3%)             , ch(03),~
               at (13,11), fac(hex(84)), em$(e%+3%)             , ch(08),~
               at (13,21), fac(lf$(e%+3%)), nn$(e%+3%)          , ch(25),~
               at (13,44), fac(hex(84)), vv$(e%+3%)             , ch(05),~
/* CR2490      at (13,41), fac(hex(84)), sh$(e%+3%)             , ch(08),  */ ~
               at (13,50), fac(hex(84)), dv$(e%+3%)             , ch(12),~
               at (13,63), fac(hex(84)), di$(e%+3%)             , ch(12),~
               at (13,76), fac(hex(84)), hh$(e%+3%)             , ch(05),~
                                                                         ~
               at (14,02), fac(lfac$),   cd$(e%+4%)             , ch(01),~
               at (14,04), fac(hex(84)), dd$(e%+4%)             , ch(01),~
               at (14,07), fac(hex(84)), dp$(e%+4%)             , ch(03),~
               at (14,11), fac(hex(84)), em$(e%+4%)             , ch(08),~
               at (14,21), fac(lf$(e%+4%)), nn$(e%+4%)          , ch(25),~
               at (14,44), fac(hex(84)), vv$(e%+4%)             , ch(05),~
/* CR2490      at (14,41), fac(hex(84)), sh$(e%+4%)             , ch(08),  */ ~
               at (14,50), fac(hex(84)), dv$(e%+4%)             , ch(12),~
               at (14,63), fac(hex(84)), di$(e%+4%)             , ch(12),~
               at (14,76), fac(hex(84)), hh$(e%+4%)             , ch(05),~
                                                                         ~
               at (15,02), fac(lfac$),   cd$(e%+5%)             , ch(01),~
               at (15,04), fac(hex(84)), dd$(e%+5%)             , ch(01),~
               at (15,07), fac(hex(84)), dp$(e%+5%)             , ch(03),~
               at (15,11), fac(hex(84)), em$(e%+5%)             , ch(08),~
               at (15,21), fac(lf$(e%+5%)), nn$(e%+5%)          , ch(25),~
               at (15,44), fac(hex(84)), vv$(e%+5%)             , ch(05),~
/* CR2490      at (15,41), fac(hex(84)), sh$(e%+5%)             , ch(08),  */ ~
               at (15,50), fac(hex(84)), dv$(e%+5%)             , ch(12),~
               at (15,63), fac(hex(84)), di$(e%+5%)             , ch(12),~
               at (15,76), fac(hex(84)), hh$(e%+5%)             , ch(05),~
                                                                         ~
               at (16,02), fac(lfac$),   cd$(e%+6%)             , ch(01),~
               at (16,04), fac(hex(84)), dd$(e%+6%)             , ch(01),~
               at (16,07), fac(hex(84)), dp$(e%+6%)             , ch(03),~
               at (16,11), fac(hex(84)), em$(e%+6%)             , ch(08),~
               at (16,21), fac(lf$(e%+6%)), nn$(e%+6%)          , ch(25),~
               at (16,44), fac(hex(84)), vv$(e%+6%)             , ch(05),~
/* CR2490      at (16,41), fac(hex(84)), sh$(e%+6%)             , ch(08),  */ ~
               at (16,50), fac(hex(84)), dv$(e%+6%)             , ch(12),~
               at (16,63), fac(hex(84)), di$(e%+6%)             , ch(12),~
               at (16,76), fac(hex(84)), hh$(e%+6%)             , ch(05),~
                                                                         ~
               at (17,02), fac(lfac$),   cd$(e%+7%)             , ch(01),~
               at (17,04), fac(hex(84)), dd$(e%+7%)             , ch(01),~
               at (17,07), fac(hex(84)), dp$(e%+7%)             , ch(03),~
               at (17,11), fac(hex(84)), em$(e%+7%)             , ch(08),~
               at (17,21), fac(lf$(e%+7%)), nn$(e%+7%)          , ch(25),~
               at (17,44), fac(hex(84)), vv$(e%+7%)             , ch(05),~
/* CR2490      at (17,41), fac(hex(84)), sh$(e%+7%)             , ch(08),  */ ~
               at (17,50), fac(hex(84)), dv$(e%+7%)             , ch(12),~
               at (17,63), fac(hex(84)), di$(e%+7%)             , ch(12),~
               at (17,76), fac(hex(84)), hh$(e%+7%)             , ch(05),~
                                                                         ~
               at (18,02), fac(lfac$),   cd$(e%+8%)             , ch(01),~
               at (18,04), fac(hex(84)), dd$(e%+8%)             , ch(01),~
               at (18,07), fac(hex(84)), dp$(e%+8%)             , ch(03),~
               at (18,11), fac(hex(84)), em$(e%+8%)             , ch(08),~
               at (18,21), fac(lf$(e%+8%)), nn$(e%+8%)          , ch(25),~
               at (18,44), fac(hex(84)), vv$(e%+8%)             , ch(05),~
/* CR2490      at (18,41), fac(hex(84)), sh$(e%+8%)             , ch(08),  */ ~
               at (18,50), fac(hex(84)), dv$(e%+8%)             , ch(12),~
               at (18,63), fac(hex(84)), di$(e%+8%)             , ch(12),~
               at (18,76), fac(hex(84)), hh$(e%+8%)             , ch(05),~
                                                                         ~
               at (19,02), fac(lfac$),   cd$(e%+9%)             , ch(01),~
               at (19,04), fac(hex(84)), dd$(e%+9%)             , ch(01),~
               at (19,07), fac(hex(84)), dp$(e%+9%)             , ch(03),~
               at (19,11), fac(hex(84)), em$(e%+9%)             , ch(08),~
               at (19,21), fac(lf$(e%+9%)), nn$(e%+9%)          , ch(25),~
               at (19,44), fac(hex(84)), vv$(e%+9%)             , ch(05),~
/* CR2490      at (19,41), fac(hex(84)), sh$(e%+9%)             , ch(08),  */ ~
               at (19,50), fac(hex(84)), dv$(e%+9%)             , ch(12),~
               at (19,63), fac(hex(84)), di$(e%+9%)             , ch(12),~
               at (19,76), fac(hex(84)), hh$(e%+9%)             , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41720
                  call "PRNTSCRN"
                  goto L40230

L41720:        if keyhit% <>  2 then goto L41770
                  e% = 1%
                  gosub set_pf1
                  goto L40230

L41770:        if keyhit% <> 3 then goto L41830
L41780:           x% = (int(e_max%/10%) * 10%)+ 1% /* TOP OF LAST PAGE */
                  e% = x%
                  gosub set_pf1
                  goto L40230

L41830:        if keyhit% <> 4 then goto L41910
                  if e_max% = 0% then goto L41910
                  x% = e% - 10%                    /* PREVIOUS PAGE    */
                  e% = x%
                  if e% < 1% then e% = 1%
                  gosub set_pf1
                  goto L40230

L41910:        if keyhit% <> 5 then goto L41980
                  x% = e% + 10%                    /* NEXT PAGE        */
                  if x% > e_max% then goto L41780
                  e% = x%
                  gosub set_pf1
                  goto L40230

L41980:        if keyhit% <> 6 then goto L42010
                  gosub efficiency

L42010:        if keyhit% <> 9 then goto L42080
                  gosub set_pf2
                  lfac$ = hex(81)
                  init(hex(84)) lfac$()
                  mod% = 1%
                  goto L40230

L42080:        if keyhit% <> 10 then goto L42160
                  gosub lookup_variance
                  goto L40230

L42160:        if mod% = 0% then goto L42190
                  gosub edit_action

L42190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L42400     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (6)Efficiency          " &        ~
                     "(13)Time Entry         (16)Exit Program"
            pfkeys$ = hex(01ffff04ff06ffffffffff0c0dff0f1000)
            if fieldnr% = 1% then L42340
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42340:     if fieldnr% > 1% then L42360
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42360:     if fieldnr% > 2% then goto L42380
                str(pf$(3),18,15) = " "  :  str(pfkeys$, 6,1) = hex(ff)
L42380:     return

L42400: if fieldnr% > 0% then L42800  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over  (4)Previous              " &        ~
                     "(10)Lookup Variance    (14)Print Report"
            pf$(2) = "(2)First       (5)Next     (6)Efficiency" &        ~
                     "(11)Select Data        (15)Print Screen"
            pf$(3) = "(3)Last        (9)Modify Variances      " &        ~
                     "(13)Time Entry         (16)Exit Program" /* (CR1489) */
            pfkeys$ = hex(010203040506ffff090a0bff0d0e0f1000)

            if pf_flag% = 0% then goto L42420
               str(pf$(3),15%,20%)  = " " : str(pfkeys$,9%,1%) = hex(ff)
L42420:     if e_max% <> 0% then goto L42570
               str(pf$(1),15%,15%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
               str(pf$(2),15%,10%)  = " " : str(pfkeys$,5%,1%) = hex(ff)
               str(pf$(2),1%,10%)   = " " : str(pfkeys$,2%,1%) = hex(ff)
               str(pf$(3),15%,20%)  = " " : str(pfkeys$,9%,1%) = hex(ff)
               str(pf$(3),1%,10%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
               str(pf$(3),64%)      = " " : str(pfkeys$,16%,1%)= hex(ff)
               str(pf$(1),40%,20%)  = " " : str(pfkeys$,10%,1%)= hex(ff)
               return
L42570:     str(pf$(1),64%)     = " " : str(pfkeys$,14%,1%) = hex(ff)
            str(pf$(2),40%,17%) = " " : str(pfkeys$,11%,1%) = hex(ff)
            str(pf$(2),28%,13%) = " " : str(pfkeys$, 6%,1%) = hex(ff)
            if sel$ = "1" then goto L42660
               str(pf$(3),15%,20%)  = " " : str(pfkeys$,9%,1%) = hex(ff)
/*  CR2490     str(pf$(3),64%)      = " " : str(pfkeys$,16%,1%)= hex(ff)  */
               inpmessage$ = " "
               goto L42700

L42660:     if e_max% = 0% then goto L42700
            inpmessage$ = "Select PF(9) to Modify Action Variance, Follow~
        ~ed by <Return> When Finished?"

L42700:     if e% > 1% then goto L42730              /* CHECK FIRST     */
               str(pf$(2),1%,10%)   = " " : str(pfkeys$,2%,1%) = hex(ff)
                                                    /* CHECK NEXT/LAST */
L42730:     if (e% + 10%) <= e_max% then goto L42770
               str(pf$(2),15%,10%)  = " " : str(pfkeys$,5%,1%) = hex(ff)
               str(pf$(3),1%,10%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
                                                    /* CHECK PREVIOUS  */
L42770:     if e% > 1% then  goto L42790
               str(pf$(1),15%,15%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
L42790:     return
L42800: set_pf2                      /*  Edit Mode - Enabled    */
            if e_max% = 0% then goto L42850
            inpmessage$ = "Position Cursor, and Enter Variance Action Cod~
        ~es. <Return> When Finished?"

L42850:     pf$(1) = "(1)Start Over                           " &        ~
                     "(10)Lookup Variance                    "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Department Code or All*/ ~
                              L50390,         /* Dept Shift Code       */ ~
                              L50560,         /* Production Week       */ ~
                              L51120,         /* Production Day        */ ~
                              L51440          /* Variance Selection    */
            return

L50150: REM DEPARTMENT CODE                       SC_DEPT$
/* CR2467 access issue */
            if str(sc_dept$,1%,1%) ="A" then goto L50189
            convert sc_dept$ to x%, data goto L50189
            convert x% to sc_dept$, pic(000)                 
L50189:     gosub load_access
            if errormsg$ <> " " then return
            if sc_dept$ <> " " then goto L50230
L50190:        sc_dept$ = "ALL"
               sc_dept_d$ = "(ALL)-All Depart's Report-Only"
               if security% = 0% then goto L50350
               return
L50230:     if str(sc_dept$,1%,1%) = "A" then goto L50190
            convert sc_dept$ to x%, data goto L50350

            convert x% to sc_dept$, pic(000)

            init(" ") readkey$, sc_dept_d$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = sc_dept$
            read #2,key = readkey$, using L50330, sc_dept_d$,             ~
                                                           eod goto L50350
L50330:        FMT POS(25), CH(30)
        return
L50350:     errormsg$ = "(Error) - Invalid Department Code?"
            sc_dept$, sc_dept_d$ = " "
        return

L50390: REM Department Shift Codes
           if e_shift$ <> " " then goto L50450
L50410:       e_shift$ = "00"
              e_shift_d$ = "(ALL) - Shifts "
              return

L50450:     if e_shift$ = "00" then goto L50410
            init(" ") readkey$, e_shift_d$
            str(readkey$,1%,9%)   = "EMP SHIFT"
            str(readkey$,10%,15%) = e_shift$
            read #2,key = readkey$,using L50500,e_shift_d$, eod goto L50520
L50500:         FMT POS(25), CH(30)
        return
L50520:     errormsg$ = "(Error) - Invalid Shift Code?"
            init(" ") e_shift$, e_shift_d$
        return

L50560: REM PRODUCTION WEEK                       MT_WK$
           init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,     ~
                     ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,     ~
                     cur_yr_bi$, ent_yr_bi$, prv_yr_bi$

           if str(sc_wk$,1%,1%) <> "A" then goto L50630
              sc_wk$ = "AA"
              sc_wk_dte$ = "ALL- Rpt"

L50630:    if sc1_yr$ = " " then goto L50640
                ent_yr$ = sc1_yr$
                convert ent_yr$ to temp%, data goto L51000
                ent_yr_bi$ = bin(temp%, 2)
L50640:    if sc_wk$ <> " " and sc_wk$ <> "AA" then ent_wk$ = sc_wk$
           if sc_day$ <> " " then ent_dy$ = sc_day$
           pl_e% = 0%

/* (AWD037) changed from AWDPLN0B to AWDEMP0B */
           call "AWDEMP0B" (  cur_yr_bi$, /* Current Production Year    */~
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

            if pl_e% <> 0% then goto L51000

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)
            temp% = val(prv_yr_bi$,2)
            convert temp% to prv_yr$, pic (####)

            mt_date$  = ent_dte$                      /* Current Date   */
            mt_yr$    = ent_yr$
            mt_yr_bi$ = ent_yr_bi$
            sc1_yr$   = ent_yr$
            sv_wk$    = ent_wk$
            sv_day$   = ent_dy$
            wk% = 0%
            convert sv_wk$ to wk%, data goto L51000

            convert sv_day$ to i%, data goto L51000

            if sc_wk$ <> "AA" then sc_wk$  = sv_wk$
            if sc_day$ <> "A" then sc_day$ = sv_day$
            if sc_day$ <> "A" then days$   = days$(i%)
            if sc_wk$ <> "AA" then sc_wk_dte$ = ent_date$
REM IF SC1_YR$ > CUR_YR$ THEN GOTO 51070
            if sc1_yr$ < cur_yr$ then goto L50990 
REM IF SC_WK$ > CUR_WK$ THEN GOTO 51030
L50990: return
L51000:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, sc1_yr$, days$, sc1_yr$
        return
           errormsg$ = "(Error) - Future Production Week,Less than Equal ~
        ~to ("& cur_wk$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc1_yr$
        return
           errormsg$ = "(Error) - Future Production Year,Less than Equal ~
        ~to ("& cur_yr$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc1_yr$
        return

L51120: REM PRODUCTION DAY
           if sc_day$ <> " " then goto L51150
              sc_day$ = "A"
L51150:    if sc_day$ <> "A" then goto L51190
              days$ = "All_Prod "
              return

L51190:    convert sc_day$ to x%, data goto L51320

           convert cur_dy$ to y%, data goto L51320

           if x% < 1% or x% > 7% then goto L51320
           days$ = days$(x%)

REM IF SC1_YR$ > CUR_YR$ THEN GOTO L51390
           
           if sc1_yr$ < cur_yr$ then goto L51300
           if sc_wk$ < cur_wk$ then goto L51300  /*SR73889 */
           if sc_wk$ = cur_wk$ and sc1_yr$ = cur_yr$ and x% <= y%    ~
                                    then goto L51300
/*SR72002  * * * Removed all hard coding with  SR71637 * * *  */
           emp% = 1%
           gosub check_emp_open              /*SR71637        */
           if emp% = 0% then return
           if sc_wk$ <> cur_wk$ then goto N51390
           if x% > y% then goto L51320  /*SR73889 */
           goto L51390  /*SR73889 */

N51300:    if x% > y% then goto L51350

L51300:    days$ = days$(x%)
           gosub check_time           /*  (EWD002)    */
        return
L51320:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           init(" ") sc_day$, days$
        return
L51350:    errormsg$ = "(Error) - Future Production Day,Less than Equal t~
        ~o (" & cur_dy$ & ")"
           init(" ") sc_day$, days$
        return
L51390:    errormsg$ = "(Error) - Future Production Year,Less than Equal ~
        ~to ("& cur_yr$ & ")"
           init(" ") sc_day$, days$
        return

N51390:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"   
           init(" ") sc_day$, days$
        return

L51440: REM Variance Selection                         SEL$
           gosub check_time           /*  (EWD002)    */
           if sel$ <> " " then goto L51470
              sel$ = "5"
L51470:    convert sel$ to x%, data goto L51600

           if x% < 1% or x% > 6% then goto L51600
              if sel$ = "1" then sel_d$ = "(Open)-Employee Var (ALL)"
              if sel$ = "2" then sel_d$ = "(Closed)-Employee Var All"
              if sel$ = "3" then sel_d$ = "No Clock In              "
              if sel$ = "4" then sel_d$ = "No Clock Out             "
              if sel$ = "5" then sel_d$ = "(All) - Variance Codes   "
              if sel$ = "6" then sel_d$ = "Check Emp for Clock Entry"
           if sel$ <> "6" then return
              if sc_dept$ = "ALL" then goto L51600
              if sc_wk$   = "AA" then goto L51600
        return
L51600:    errormsg$ = "(Error) - Invalid Selection Code Entered?"
           init(" ") sel$, sel_d$
        return

/*  (EWD002)  -- Begin  */
        check_time
            if check_time% <> 0% then return                 /*  (EWD005)  */
            pf_flag% = 0%
            if security% = 1% then return
            if test_flag% = 1% then goto L11440
            test_flag% = 1%
            cur_dy%, cur_wk%, cur_yr%, tst_wk% = 0%
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

            if sc_day$ = "5" and tst_wk$ = prev_wk$ then prev_dy$ = "5"
            if sc_day$ = "6" and tst_wk$ = prev_wk$ then prev_dy$ = "6"

            if sc_wk$ = cur_wk$ and sc_day$ = cur_dy$ and            ~
                                         sc1_yr$ = cur_yr$ then return
            if sc_yr$ <= cur_yr$ and sc_wk$ < prev_wk$ then pf_flag% = 1%
            if sc_yr$ <= cur_yr$ and sc_wk$ <= cur_wk$ and           ~
                             sc_day$ < prev_dy$ then pf_flag% = 1%
            if str(sys_time$,1%,4%) > test_time$ then pf_flag% = 1%
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55080: %! ######## @ ########                        ###################~
        ~#####################                                Page: #### !

        %!                                                               ~
        ~                                                                !

L55140: %! Department: ### ##############################                ~
        ~                                      Production Date: ######## !

L55170: %!EmpNo!<------ Name ------>!Day!<-- Clock In Var -->!<-- Clock O~
        ~ut Var ->!<--- Daily Var ---->!<- Process Action ->!Hrs Mn !Dept!

L55200: %!#####!####################! # ! (#) ###############! (#) ######~
        ~#########! (#) ###############! (#) ###############!### ## ! ## !

L55230: %!-----!--------------------!---!--------------------!-----------~
        ~---------!--------------------!--------------------!-------!----!

L55260: %!#####! Total for Week (##)!   ! DATE:   ########   !           ~
        ~         !Total Hours: #######!Total Pay Hours --->!#######!    !

L55290: %!*****!Report Total for Department: ( ### #######################~
        ~#######) Total Pay Hours(########) Efficiency -->!########!    !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55140, sc_dept$, sc_dept_d$, sc_wk_dte$
           print using L55060
           print using L55170
           lcntr% = 5%
           mt_dept_sav$ = mt_dept_d$
        return

        print_detail
            if sav_key$ <> " " then goto L60100       /* Department Brk */
               sav_key$ = mt_key$
L60100:     if str(sav_key$,1%,5%) = str(mt_key$,1%,5%) then goto L60130
               gosub print_totals
               gosub report_totals
               gosub print_header                    /* Department Code*/
               sav_key$ = mt_key$                    /*   Change       */

L60130:     if sav_emp$ <> " " then goto L60140       /* Employee/Week  */
               goto L60155
L60140:     if str(sav_emp$,1%,5%) = str(mt_key$,6%,5%) and              ~
               str(sav_emp$,6%,2%) = str(mt_key$,11%,2%) then goto L60170
               gosub print_totals
L60155:        str(sav_emp$,1%,5%) = str(mt_key$,6%,5%)
               str(sav_emp$,6%,2%) = str(mt_key$,11%,2%)

L60170:     gosub calc_detail
        return

        print_totals
            y = round(t_hrs% / 60.0, 2)
            convert y to t_hrs$, pic(###.##-)       /* TOTAL HOURS     */

            y = round(t_hrs_p% / 60.0, 2)
            convert y to t_hrs_p$, pic(###.##-)     /* TOTAL PAY HOURS */

            gosub calc_prod_date
            print using L55230
            print using L55260, str(sav_emp$,1%,5%), str(sav_emp$,6%,2%), ~
                                                    save_dte$, t_hrs$,   ~
                                                    t_hrs_p$
            t_hrs%, t_min%, t_hrs_p%, t_min_p% = 0%
            x = round(t_hrs_d% / 60.0, 2)
            y = round(t_hrs_e% / 60.0, 2)
            tot_d = round(tot_d + x, 2)
            tot_e = round(tot_e + y, 2)

            t_hrs_d%, t_min_d%, t_hrs_e%, t_min_e% = 0%
            lcntr% = lcntr% + 2%
        return

        report_totals
            convert tot_d to t_hrs_d$, pic(####.##-)

            convert tot_e to t_hrs_e$, pic(####.##-)

            print using L55230
            print using L55290, str(sav_key$,1%,3%), mt_dept_sav$,        ~
                                                    t_hrs_d$, t_hrs_e$
            t_hrs_d%, t_min_d% = 0%
            t_hrs_e%, t_min_e% = 0%
            tot_d, tot_e = 0.0
            lcntr% = lcntr% + 2%
        return

        calc_prod_date
           pl_e% = 0%
           init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,     ~
                     ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,     ~
                     cur_yr_bi$, ent_yr_bi$, prv_yr_bi$

           ent_wk$ = str(sav_emp$,6%,2%)

           if sc1_yr$ <> " " then ent_yr$ = sc1_yr$

           if sc1_yr$ = " " then goto L60210
                convert sc1_yr$ to temp%, data goto L60210
                ent_yr$ = sc1_yr$
                ent_yr_bi$ = bin(temp%, 2)

L60210:    call "AWDPLN0B" ( cur_yr_bi$, /* Current Production Year    */~
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

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)
            temp% = val(prv_yr_bi$,2)
            convert temp% to prv_yr$, pic (####)

           save_dte$ = ent_dte$
           call "DATEFMT" (save_dte$)
        return

        scan_data
            gosub check_time           /*  (EWD002)    */
            p% = pos("346" = sel$)
            if p% = 0% then goto L60510
               gosub check_employees
               goto scan_done

L60510:     if keyhit% <> 11 then goto L60545
               if sc_dept$ = "AA" then goto L60530
               if sc_wk$ = "AA" then goto L60530
               goto L60545
L60530:           errormsg$ = "(Error) Must Use Report Selection?"
                  return

L60545:     call "SHOSTAT" ("Scanning Time Clock Entries")
           dept% = -1%
           mt_key$ = all(hex(00))
           end_of_file% = 0%
        check_dept
           if end_of_file% = 1% and sc_dept$ <> "ALL" then goto scan_done
           if sc_dept$ <> "ALL" then goto L60600
              dept% = dept% + 1%
              if dept% > 30% then goto scan_done
              convert dept% to str(mt_key$,1%,3%), pic(000)
              goto L60610
L60600:    str(mt_key$,1%,3%) = sc_dept$

L60610:    sav_dept$ = str(mt_key$,1%,3%)
           str(mt_key$,4%,2%) = mt_yr_bi$
           save_key$ = " "
        check_emp
           str(mt_key$,11%,3%)  = " "
           read #3,key > mt_key$, using L60640, mt_key$, eod goto scan_end
L60640:       FMT CH(13)
           if str(mt_key$,4%,2%) <> mt_yr_bi$ then scan_done
           if sc_dept$ = "ALL" then goto L60665
              if sc_dept$ <> str(mt_key$,1%,3%) then goto scan_done
                 goto L60675
L60665:    if sav_dept$ <> str(mt_key$,1%,3%) then goto scan_end

L60675:    str(save_key$,1%,10%) = str(mt_key$,1%,10%)
           str(mt_key$,11%,3%)  = " "
           if sc_wk$ <> "AA" then str(mt_key$,11%,2%) = sc_wk$
        check_emp_next
           read #3,key > mt_key$, using L60710, mt_key$, mt_var_in$,      ~
                                  mt_var_out$, mt_var_day$, mt_proc$,    ~
                                                       eod goto scan_end
L60710:        FMT CH(13), POS(24), 4*CH(1)
           if str(save_key$,1%,10%) <> str(mt_key$,1%,10%) then            ~
                                                           goto check_emp
           gosub check_shift
           if shift% = 0% then goto check_emp_next

           if sc_wk$ = "AA" then goto L60750
              if sc_wk$ <> str(mt_key$,11%,2%) then goto check_emp_next
L60750:    if sc_day$ = "A" then goto L60760
              if sc_day$ <> str(mt_key$,13%,1%) then goto check_emp_next
L60760:    gosub check_data
           goto check_emp_next

        scan_done
            e_max% = e% - 1%
            e% = 1%
        return

        scan_end
           end_of_file% = 1%
           goto check_dept

        check_data
            p% = pos("123456789" = mt_var_day$)         /* SET DAY VAR */
REM P1% = POS("ABCDEFGHIJKLMNOPQRSTUVWXYZ" = MT_PROC$)
REM P1% = POS("ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^*~&" = MT_PROC$)
REM P1% = POS("ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^*~&()" = MT_PROC$)
/* (AWD038) - ADD + (AWD044) ADDED {} */
/*AWD030    P1% = POS("ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^*~&()-+}{" = MT_PROC$) */
/*SR71317   P1% = POS("ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^*~&()-+}{=;" = MT_PROC$)*/
/*SR71860 */p1% = pos("ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^*~&()-+}{=;[" = mt_proc$)
            x% = pos("12345" = sel$)
            on x% goto L60845, L60865, L60885, L60900, L60910
L60845:           if p1% <> 0% then return                   /* CLOSED */
                  if p% = 0% then return                /* NO VARIANCE */
                  goto L60910

L60865:           if p1% <> 0% then goto L60910
                  if p% = 0% then goto L60910
                  return                       /* Still Open */

L60885:           if mt_var_in$ = "5" then goto L60910
                  return
                                                /* No Clock Out Time   */
L60900:        if mt_var_out$ = "6" then goto L60910
                  return
L60910: gosub store_data
        return

        load_employee
        hrsft% = 0% /* AWD016 */
            read #1,key = mt_emp$, using L60940, e_dept$, e_lname$,       ~
                                   e_fname$, e_init$, eod goto L60960
L60940:        FMT CH(3), POS(12), CH(15), CH(10), CH(1)
            name$ = e_lname$ & ", " & str(e_fname$,1%,1%) & ". " &         ~
                                                       str(e_init$) & "."
/* <AWD016> set hrsft% to hours per shift  */
REM IF SHIFT$ = "00" THEN RETURN
           hrsft% = 12%
           
           if shift$ = "00" then hrsft% = 8%
           if shift$ = "01" or shift$ = "02" or shift$ = "03" then hrsft% = 8%
           if shift$ = "07" or shift$ = "08" or shift$ = "09" then hrsft% = 8%
           if shift$ = "15" then hrsft% = 8%
REM IF SHIFT$ = "05" OR SHIFT$ = "06" OR SHIFT$ = "07" THEN HRSFT% = 8%           
        return
/* </AWD016> */
L60960:     e_lname$ = "(ERROR) - EMPLOYEE"
        return

        check_shift
            shift% = 0%
            mt_emp$ = str(mt_key$,6%,5%)
            read #1,key = mt_emp$, using L60995, shift$, eod goto L61020
L60995:        FMT POS(841), CH(2)

L61010:    if e_shift$ = "00" then goto L61015            /* All Shifts */
              if shift$ = "00" then return
              if shift$ <> e_shift$ then return
L61015: shift% = 1%
L61020: return

        store_data
            if rpt% = 0% then goto L61055
               gosub print_detail
               return

L61055:     gosub dataload
REM E% = E% + 1%
            str(sk$(e%),1%,13%)  = rhh$
            cd$(e%) = " " : cv$(e%) = mt_var_day$
            dd$(e%) = mt_day$
            dp$(e%) = mt_dept$
            e_no% = val(mt_emp$,4)
            convert e_no% to em$(e%), pic (########)
REM            em$(e%) = mt_emp$
            vv$(e%) = mt_var_in$ &"/"& mt_var_out$ &"/"& mt_var_day$
            dv$(e%) = mt_var_desc$
            di$(e%) = mt_procd$
            hh$(e%) = hours$
            gosub load_employee
            sf%(e%) = hrsft% /* AWD016 */
            nn$(e%) = name$
            gosub load_clock_in
            str(nn$(e%),14%,12%) = " " & clock_in$       /* CR2490 */
            sh$(e%) = system_in$     /* (AWD036) */
            lf$(e%) = hex(84)
REM IF MT_VAR_IN$  <> "0" THEN LF$(E%) = HEX(94)
REM IF MT_VAR_OUT$ <> "0" THEN LF$(E%) = HEX(94)
            if userid$ = "MVK" then lf$(e%) = hex(84)   /* (EWD001)  */
            if userid$ = "LNT" then lf$(e%) = hex(84)   /* (EWD042)  */
            if userid$ = "MKN" then lf$(e%) = hex(84)   /* (EWD012)  */
            if userid$ = "LBH" then lf$(e%) = hex(84)
            if userid$ = "LLJ" then lf$(e%) = hex(84)
            if userid$ = "APA" then lf$(e%) = hex(84)
            if userid$ = "EDW" then lf$(e%) = hex(84)
            e% = e% + 1%
            if e% > 500% then e% = 500%
        return

        store_data_in
REM E% = E% + 1%
            str(sk$(e%),1%,13%)  = mt_key$
            mt_var_in$ = "5" : mt_var_out$ = "6" : mt_var_day$ = "7"
            mt_day$  = str(mt_key$,13%,1%)
            mt_dept$ = str(mt_key$,1%,3%)
            mt_emp$  = str(mt_key$,6%,5%)
            mt_var_desc$ = "- No Clock In -"
            if sel$ = "4" then mt_var_desc$ = "- No Clock Out -"
            mt_proc$ = "0"
            hours$ = "??:??"
            cd$(e%) = " " : cv$(e%) = mt_var_day$
            dd$(e%) = mt_day$
            dp$(e%) = mt_dept$
            em$(e%) = mt_emp$
            vv$(e%) = mt_var_in$ &"/"& mt_var_out$ &"/"& mt_var_day$
            dv$(e%) = mt_var_desc$
            di$(e%) = mt_procd$
            hh$(e%) = hours$
            gosub load_employee
            sf%(e%) = hrsft% /* AWD016 */
            nn$(e%) = name$
            gosub load_clock_in
            str(nn$(e%),11%,9%) =  " " & clock_in$
            sh$(e%) = system_in$     /* (AWD036) */
            lf$(e%) = hex(84)
            if mt_var_in$  <> "0" then lf$(e%) = hex(94)
            if mt_var_out$ <> "0" then lf$(e%) = hex(94)
            if userid$ = "MVK" then lf$(e%) = hex(84)   /* (EWD001)   */
            if userid$ = "LNT" then lf$(e%) = hex(84)   /* (EWD042)  */
            if userid$ = "MKN" then lf$(e%) = hex(84)   /* (EWD012)  */
            if userid$ = "LBH" then lf$(e%) = hex(84)
            if userid$ = "LLJ" then lf$(e%) = hex(84)
            if userid$ = "APA" then lf$(e%) = hex(84)
            if userid$ = "EDW" then lf$(e%) = hex(84)
            e% = e% + 1%
            if e% > 500% then e% = 500%
        return

        load_clock_in
            init(" ") dt_key$, clock_in$, sav_key1$, dt_code$, system_in$
            str(dt_key$,1%,2%) = str(mt_key$,4%,2%)     /* Prod Year   */
            str(dt_key$,3%,2%) = str(mt_key$,11%,2%)    /* Prod Week   */
            str(dt_key$,5%,1%) = str(mt_key$,13%,1%)    /* Prod Day    */
            str(dt_key$,6%,5%) = str(mt_key$,6%,5%)     /* Employee Cod*/
            str(dt_key$,11%,3%)= str(mt_key$,1%,3%)     /* Department  */
            sav_key1$ = str(dt_key$,1%,13%)
        load_clock_nxt
            read #4,key > dt_key$, using L61370, dt_key$, dt_code$,       ~
                                                eod goto L61410
L61370:        FMT CH(18), CH(1)
            if sav_key1$ <> str(dt_key$,1%,13%) then goto L61410
               if dt_code$ <> "0" then goto load_clock_nxt
                  get #4, using L61390, clock_in$
L61390:             FMT POS(31), CH(8)
REM SYSTEM_IN$ = CLOCK_IN$
             gosub convert_clock_time
             str(clock_in$,1%,2%) = str(dt_key$,14%,2%)
             str(clock_in$,4%,2%) = str(dt_key$,16%,2%)
        return
L61410:     clock_in$ = "No Clock"
        return

        convert_clock_time
          init(" ") am_pm$, system_in$
          hr% = 0%
          mn% = 0%
          am_pm$ = str(clock_in$,7%,2%)

          convert str(clock_in$,1%,2%) to hr%, data goto bad_clk_hr

bad_clk_hr:
          convert str(clock_in$,4%,2%) to mn%, data goto bad_clk_mn

bad_clk_mn:

          if am_pm$ = "PM" and hr% <> 12% then hr% = hr% + 12%

          convert hr% to str(system_in$,1%,2%), pic(00)

          str(system_in$,3%,1%) = ":"
          convert mn% to str(system_in$,4%,2%), pic(00)
          str(system_in$,7%,2%) = am_pm$
        return



        load_variance                                  /* IN, OUT, DAY */
            init(" ") readkey$, mt_var_ind$, mt_var_outd$, mt_var_dayd$, ~
                      mt_procd$
            str(readkey$,1%,9%)   = "EMP VARCD"
            str(readkey$,10%,15%) = mt_var_in$
            read #2,key = readkey$,using L61460,mt_var_ind$,              ~
                                                           eod goto L61465
L61460:       FMT POS(25), CH(15)
L61465:     str(readkey$,10%,15%) = mt_var_out$
            read #2,key = readkey$,using L61460,mt_var_outd$,             ~
                                                           eod goto L61485

L61485:     str(readkey$,10%,15%) = mt_var_day$
            read #2,key = readkey$,using L61460,mt_var_dayd$,             ~
                                                           eod goto L61505

L61505:     str(readkey$,10%,15%) = mt_proc$
            read #2,key = readkey$,using L61460,mt_procd$,                ~
                                                           eod goto L61525

L61525: return

        edit_action
           init(" ") readkey$
           str(readkey$,1%,9%) = "EMP VARCD"
           for i% = e% to (e%+10%)
             if i% > e_max% then goto L61640
             if cd$(i%) = " " then goto L61640
                di$(i%) = "Error- Inv Code"
                codes$ = "0123456789"           /* NOT VALID EDIT CODE */
                if str(em$(i%),1%,1%) = "A" then                         ~
                        codes$ = "0123456789DHJSVQ" /*NOT VALID TEMPS*/

                p% = pos( codes$ = cd$(i%) )
                if p% <> 0% then goto L61635    /* INVALID PROCESS CODE */
                str(readkey$,10%,15%) = cd$(i%)
                read #2,key = readkey$, using L61615, di$(i%),            ~
                                                          eod goto L61635
L61615:            FMT POS(25), CH(15)
                                                /* (EWD004) - Add code 'T' */
                                                /* (AWD009) - Add code 'U' */
                                                /* (AWD011) - Add code 'W' */
                                                /* (AWD014) - Add code 'M' */
                                                /* (AWD015) - Add code 'G' */
                                                /* (AWD015) - Add code 'I' */
                                                /* (AWD015) - Add code 'K' */
REM P% = POS("SVXYZEFTUWM"=CD$(I%))   /* NEED TO UPDATE EMP   */
REM P% = POS("SVXYZEFTUWMGIK"=CD$(I%))   /* AWD015               */
REM     +---------------------------------------------------------------+
REM     | AWD016 Merge G&K, X&Z and M&W.                                |
REM     | This leaves ACKLNOPWZ free?                                   |
REM     +---------------------------------------------------------------+
REM P% = POS("SVXYEFTUWGIR*" = CD$(I%))   /* EMPLOYEE MASTER CHGS */
REM P% = POS("SVXYEFTUWGIR*CZ" = CD$(I%))   /* AWD024 */
REM P% = POS("SVXYEFTUWGIR*CZQ" = CD$(I%))   /* AWD026 */
REM P% = POS("SVXYEFTUWGIR*CZQ()" = CD$(I%))   /* AWD043 */
REM P% = POS("SVXYEFTUWGIR*CZQ()+{}" = CD$(I%))/* AWD044 */
                p% = pos("SVXYEFTUWGIR*CZQ()+{}[" = cd$(i%))/* SR71860 */
                if p% <> 0% then gosub edit_employee
                goto L61640
L61635:            cd$(i%) = " "
L61640:    next i%
           mod% = 0%
        return

        edit_employee
/* READ #1,KEY = EM$(I%), USING L61685, E_VAC_DAYS%, E_VAC_USED%,  ~
   E_SICK_DAYS%, E_SICK_USED%, E_PTS_TOT,  ~
    EOD GOTO L61780                                                         */
/*AWD043*/read #1,key = em$(i%), using L61685, e_vac_days%, e_vac_used%,  ~
                                 e_sick_days%, e_sick_used%, e_pts_tot,  ~
                                 e_float_days%, e_float_used%, eod goto L61780

/* 1685 FMT POS(218), BI(2), POS(226), BI(2), POS(234), BI(2),      ~
            POS(242), BI(2), POS(308), PD(14,4)                             */
L61685:      FMT POS(218), BI(2), POS(226), BI(2), POS(234), BI(2),      ~
                 POS(242), BI(2), POS(308), PD(14,4), POS(757), BI(2),   ~
                 POS(765), BI(2)

/* <EWD013> */
          gosub pop_emp_data
/* </EWD013> */
          sick_days% = 0%                          /*  (EWD004)  */
REM SICK_DAYS% = (E_SICK_DAYS% * 10%)        /*  (EWD004)  */
          sick_days% = e_sick_days%               /*  (AWD041)  */

          
          vac_days% = 0%                          /*  (AWD009)  */
REM VAC_DAYS% = (E_VAC_DAYS% * 10%)        /*  (AWD009)  */
          vac_days% = e_vac_days%                /*  (AWD041)  */

/*AWD043 + */
          float_days% = 0%          
          float_days% = e_float_days%            /*AWD043 */

          if p% <> 19% then goto L61800
             if (e_float_used% + fu_tot + 10%) <= float_days% then goto L61795
                cd$(i%) = " "
                di$(i%) = "No Float Days?? "
                return
/*AWD043 - */
          
L61800
REM IF P% <> 1% THEN GOTO L61720
          if p% <> 1% and p% <> 20% then goto L61720        /* AWD044  */
REM IF (E_SICK_USED% + 10%) <= SICK_DAYS% THEN RETURN
/* EWD013 */ if (e_sick_used% + su_tot + 10%) <= sick_days% then goto L61794
                cd$(i%) = " "
                di$(i%) = "No Sick Days?? "
                return
L61720:   if p% <> 7% and p% <> 21% then goto L61725 /*(EWD004),(AWD044)*/
REM IF (E_SICK_USED% + 5%) <= SICK_DAYS% THEN RETURN
/* EWD013 */ if (e_sick_used% + su_tot + 5%) <= sick_days% then goto L61792
                cd$(i%) = " "
                di$(i%) = "No Sick Days?? "
                return                             /*  (EWD004)  */
L61725:   if p% <> 2% and p% <> 16% then goto L61750
             if (e_vac_used% + 10%) <= vac_days% then return
REM IF SF%(I%) = 12% THEN GOTO L61728 /* REMOVE 12 HR CALC */
      /* <AWD028> add 12 hrcalc back */
/* EWD013 */ if (e_vac_used% + vu_tot + 10%) <= vac_days% then goto L61790
                cd$(i%) = " "
                di$(i%) = "No Vac. Days?? "
                return

L61728:    if (e_vac_used% + vu_tot + 15%) <= vac_days% then goto L61791
                cd$(i%) = " "             /* 15 = 1.5 day */
                di$(i%) = "No Vacation Days?? "
                return                             /*  (AWD011)  */

L61750:   if p% <> 8% then goto L61755               /* (AWD009) */
REM IF (E_VAC_USED% + 5%) <= VAC_DAYS% THEN RETURN
/* EWD013 */ if (e_vac_used% + vu_tot + 5%) <= vac_days% then goto L61788
                cd$(i%) = " "
                di$(i%) = "No Vacation Days?? "
                return                             /*  (AWD009)  */

L61755:   if p% <> 9% then goto L61760             /* (AWD011) */
             if (e_vac_used% + 10%) <= vac_days% then return
REM IF SF%(I%) = 12% THEN GOTO L61758
/* EWD013 */ if (e_vac_used% + 10%) <= vac_days% then goto L61790
                cd$(i%) = " "             /* 10 = 1 day */
                di$(i%) = "No Vacation Days?? "
                goto L61760                        /*  (AWD011)  */

REM L61758   IF P% <> 16% THEN GOTO L61760              /* (EWD014) */
L61758:   if (e_vac_used% + 10%) <= vac_days% then goto L61760
                cd$(i%) = " "             /* 10 = 1.0 day */
                di$(i%) = "No Vacation Days?? "
                goto L61760                        /*  (AWD011)  */

L61760:   if p% <> 11% then goto L61770               /* (AWD027) */
          if (e_vac_used% + vu_tot + 10%) <= vac_days% then goto L61790
              cd$(i%) = " "
              di$(i%) = "No Vacation Days?? "
              return                             /*  (AWD027)  */
L61770:
REM       IF P% =  3% THEN X = E_PTS_TOT + (SF%(I%) / 8.0) /* AWD016 X&Z MERGE*/
          
          if p% =  3% then x = e_pts_tot + 1.0 /* AWD016 X&Z merge*/
          if p% =  4% then x = e_pts_tot + .5   /* AUTO ADD 1/2 PT (Y)  */
          if p% =  5% then x = e_pts_tot +  .25 /* AUTO ADD 1/4 PT (E)  */
          if p% =  6% then x = e_pts_tot +  .75 /* AUTO ADD 3/4 PT (F)  */

          if p% =  9% then x = e_pts_tot + (sf%(i%) / 8.0) /* AWD016 M&W merge*/
          if p% = 10% then x = e_pts_tot + (sf%(i%) / 8.0) /* AWD016 G&K merge*/
          if p% = 12% then x = e_pts_tot + 1.5 /* AWD016 X&Z merge*/
          if p% = 13% then x = e_pts_tot + 1.25 /* AWD022 */
          if p% = 22% then x = e_pts_tot + 2.00 /* SR71860 */

          if p% = 14% then x = e_pts_tot + .25  /* AUTO ADD 1/4 PT (C)  */
          if p% = 15% then x = e_pts_tot + .5   /* AUTO ADD 1/2 PT (Z)  */
          if p% = 18% then x = e_pts_tot + 1.0  /* Inclement weather 1 pt */

          if schema% = 2% then goto skp_dpt      /* (AWD039) */
          if dp$(i%) = "056" or dp$(i%) = "060" then goto dept_56_60
          if dp$(i%) = "035" or dp$(i%) = "061" then goto dept_56_60
          if dp$(i%) = "029" then goto dept_56_60
skp_dpt:          
          if x >  3.9 then di$(i%) = "(4) or More Pts"
          if x >  5.9 then di$(i%) = "(6) or More Pts"
          if x >  8.9 then di$(i%) = "(9) or More Pts"
          if x > 11.9 then di$(i%) = "(12) or More Pts"
        return
dept_56_60:
          if x >  2.9 then di$(i%) = "(3) or More Pts"
          if x >  4.9 then di$(i%) = "(5) or More Pts"
          if x >  6.9 then di$(i%) = "(7) or More Pts"
          if x >  8.9 then di$(i%) = "(9) or More Pts"
        return

L61780:   cd$(i%) = " "
          di$(i%) = "Cannot Edit ?? "
        return

/* <EWD013> */
L61788: vac_add  = 5
        sick_add = 0
        float_add = 0                     /*AWD043*/
        gosub push_emp_data
        return
L61790: vac_add  = 10
        sick_add = 0
        float_add = 0                     /*AWD043*/
        gosub push_emp_data
        return
L61791: vac_add  = 15
        sick_add = 0
        float_add = 0                     /*AWD043*/
        gosub push_emp_data
        return
L61792: vac_add  = 0
        sick_add = 5
        float_add = 0                     /*AWD043*/
        gosub push_emp_data
        return
L61794: vac_add  = 0
        sick_add = 10
        float_add = 0                     /*AWD043*/
        gosub push_emp_data
        return
/*AWD043 + */
L61795: vac_add  = 0
        sick_add = 0 
        float_add = 10
        gosub push_emp_data
        return
/*AWD043 - */

pop_emp_data  /* em$(i%) */
        didx = 0
    vu_tot = 0
    su_tot = 0
    fu_tot = 0
        gosub find_emp
    if didx > 0 then vu_tot = vu(didx)
    if didx > 0 then su_tot = su(didx)
    if didx > 0 then fu_tot = fu(didx)        /*AWD043*/
    return

push_emp_data  /* em$(i%) */
        didx = 0
        gosub find_emp
    if didx > 0 then vu(didx) = vu(didx) + vac_add
    if didx > 0 then su(didx) = su(didx) + sick_add
    if didx > 0 then fu(didx) = fu(didx) + float_add   /*AWD043 */
    return

find_emp  /* find first occurance of emp # */
    for dx = 1 to 500
        if em$(dx) <> em$(i%) then goto find_nxt
        didx = dx
            dx = 501
find_nxt: next dx
    return
/* </EWD013> */

        update_data
            call "SHOSTAT" ("Updating Modified Variance Codes")
            for i% = 1% to e_max%
               if cd$(i%) = " " then goto L61865
               read #3,hold,key = sk$(i%), eod goto L61865
                  put #3, using L61830, cd$(i%), userid$
L61830:             FMT POS(27), CH(1), CH(3)
                  rewrite #3
                                                /* (EWD004) - Add code 'T' */
                                                /* (AWD009) - Add code 'U' */
                                                /* (AWD011) - Add code 'W' */
                                                /* (EWD014) - Add code 'M' */
                                                /* (EWD015) - Add code 'G' */
                                                /* (EWD015) - Add code 'I' */
                                                /* (EWD015) - Add code 'K' */
REM P% = POS("VSXYZEFTUW" = CD$(I%))   /* EMPLOYEE MASTER CHGS */
REM P% = POS("VSXYZEFTUWMGIK" = CD$(I%))   /* EMPLOYEE MASTER CHGS */
REM     +---------------------------------------------------------------+
REM     | AWD016 MERGE G&K, X&Z AND M&W.                                |
REM     | ALSO REVERSE ORDER OF VS TO SV TO MATCH EDIT_EMPLOYEE LOGIC   |
REM     | THIS LEAVES ACKLNOPWZ FREE?                                   |
REM     +---------------------------------------------------------------+
REM P% = POS("SVXYEFTUWGIR*" = CD$(I%))   /* EMPLOYEE MASTER CHGS */
REM P% = POS("SVXYEFTUWGIR*CZ" = CD$(I%))   /* AWD024 */
REM P% = POS("SVXYEFTUWGIR*CZQ" = CD$(I%))   /* AWD026 */
REM P% = POS("SVXYEFTUWGIR*CZQ()" = CD$(I%))   /* AWD043 */
REM P% = POS("SVXYEFTUWGIR*CZQ()+{}" = CD$(I%))
             p% = pos("SVXYEFTUWGIR*CZQ()+{}[" = cd$(i%)) /* SR71860 */
             if p% <> 0% then gosub update_employee
L61850:     next i%
        return clear all
        goto inputmode
L61865:     init(" ") r_h$
            r_h$ = "<--- VARIANCE NOT UPDATED --->"
            r_h$ = r_h$ & " FOR  EMPLOYEE " & str(sk$(i%),6%,5%)
            call "SHOSTAT" (r_h$)
            call "PAUSE" addr(200%)
            goto L61850

        update_employee
            read #1,hold,key = em$(i%), eod goto L62175
              if p% = 2% then goto L61915
              if p% = 8% then goto L61915
              if p% = 9% then goto L61915
              if p% = 16% then goto L61915
              if p% = 11% then goto L61915
            goto L61950       /*AWD043*/
L61915:
            get #1,using L61920, e_vac_used%, e_vac_used$
L61920:          FMT POS(226), BI(2), CH(6)
/* <AWD012> */
REM IF P% = 2% THEN E_VAC_USED% = E_VAC_USED% + ((SF%(I%) / 8) * 10)
              if p% = 2% then e_vac_used% = e_vac_used% + 10%
/* </AWD012> */
              if p% = 8% then e_vac_used% = e_vac_used% + 5%
REM IF P% = 9% THEN E_VAC_USED% = E_VAC_USED% + ((SF%(I%) / 8) * 10)
              if p% = 9% then e_vac_used% = e_vac_used% + 10%
              if p% = 16% then e_vac_used% = e_vac_used% + 10%
              if p% = 11% then e_vac_used% = e_vac_used% + 10%
 
              e_vac_used$ = date
            
            put #1,using L61920, e_vac_used%, e_vac_used$
            rewrite #1

              if p% = 9% then goto L62005
            return
               
/*AWD043  */               

L61950:     if p% <> 19% then goto L61955
               get #1,using N61920, e_float_used%, e_float_used$
N61920:          FMT POS(765), BI(2), CH(6)
               e_float_used% = e_float_used% + 10%
               e_float_used$ = date
               
            put #1,using N61920, e_float_used%, e_float_used$
            rewrite #1
            return
/*AWD043  */

                                  /* (EWD004) (AWD044) */
                                  
L61955:     if p% <> 1% and p% <> 7%  and p% <> 20% and                   ~
                            p% <> 21% then goto L62005
            get #1,using L61965, e_sick_used%, e_sick_used$
L61965:          FMT POS(242), BI(2), CH(6)
                                                 /* (EWD004) */
/* (AWD044) */  
                                               
              if p% = 1% or p% = 20% then e_sick_used% = e_sick_used% + 10% 
              if p% = 7% or p% = 21% then e_sick_used% = e_sick_used% + 5%  
               	
/* (AWD044\) */               	
              e_sick_used$ = date
            
            put #1,using L61965, e_sick_used%, e_sick_used$
                                                  /* ADD ONE (1) POINT */
            rewrite #1
            
              p% = pos("ST{}" = cd$(i%))  /* AWD033, AWD044 dont update pts_dte */
              if p% <> 0% then return

            read #1,hold,key = em$(i%), eod goto L62175

L62005:     get #1, using L62015, e_pts_late, e_pts_mis, e_pts_day,       ~
                                 e_pts_tot, e_pts_dte$, emp_shft$
L62015:       FMT POS(284), 4*PD(14,4), CH(6), POS(841), CH(2)
              if p% <> 12% then goto L62025                       /* Code R */
               e_pts_day = e_pts_day + 1.5
               pt_code$ = "2"
               xyz = 1.5
               pt_reason$ = "(1 1/2) Point Sick/Absent (Var)"
             goto L62160
               
L62025:      if p% <> 13% then goto L62030  /* AWD022 */         /* Code * */
               e_pts_day = e_pts_day + 1.25
               pt_code$ = "2"
               xyz = 1.25
               pt_reason$ = "(1 1/4) Point Sick/Absent (Var)"
             goto L62160
               
L62030:      if p% <> 3% then goto L62055                        /* Code X */
               e_pts_day = e_pts_day + 1.0
               xyz = 1.0
               pt_code$ = "2"
               pt_reason$ = "(1) Point Sick/Absent    (Var)"
/*SR76090*/  goto L62160
              
             if sf%(i%) = 8% then goto L62160       /* sf%(i%)->shift hours */
               e_pts_day = e_pts_day + 0.5  /* if 12hrs 1.5 ot 1.0 */
               xyz = 1.5
               pt_reason$ = "(1 1/2) Point Sick/Absent (Var)"
             goto L62160
               
L62055:      if p% <> 4% then goto L62085                        /* Code Y */
               e_pts_late = e_pts_late + .5
               xyz = .5
               pt_code$ = "0"
               pt_reason$ = "1/2 Point Late           (Var)"
             goto L62160
               
L62085:      if p% <> 5% then goto L62115                        /* Code E */
               e_pts_mis = e_pts_mis + .25
               xyz = .25
               pt_code$ = "1"
               pt_reason$ = "1/4 Point Misc.          (Var)"
             goto L62160
               
L62115:      if p% <> 6% then goto L62121                        /* Code F */
               e_pts_mis = e_pts_mis + .75
               xyz = .75
               pt_code$ = "1"
               pt_reason$ = "3/4 Point Misc.          (Var)"
             goto L62160
               
L62121:      if p% <> 9% then goto L62122                        /* Code W */
               e_pts_day = e_pts_day + 1.0
               xyz = 1.0
               pt_code$ = "2"
               pt_reason$ = "(1    ) Point Day Vacation (Var)"

             if sf%(i%) = 8% then goto l62160      /* sf%(i%) ->shift hours */
               e_pts_day = e_pts_day + 0.5
               xyz = 1.5
               pt_reason$ = "(1 1/2) point day vacation (var)"
             goto L62160
               
L62122:      if p% <> 10% then goto L62140                       /* Code G */
               e_pts_day = e_pts_day + 1.0
               xyz = 1.0
               pt_code$ = "2"
               pt_reason$ = "(1    ) Point Absent w/pay(Var)"

             if sf%(i%) = 8% then goto L62160      /* sf%(i%) ->shift hours */
               e_pts_day = e_pts_day + 0.5
               xyz = 1.5
               pt_reason$ = "(1 1/2) Point Absent w/pay(Var)"
             goto L62160
/* <AWD024> */
L62140:      if p% <> 15% then goto L62150                       /* Code Z */
               e_pts_mis = e_pts_mis + .5
               xyz = .5
               pt_code$ = "1"
               pt_reason$ = "1/2 Point Late  (No Pay)"
             goto L62160
               
L62150:      if p% <> 14% then goto L62160                       /* Code C */
               e_pts_mis = e_pts_mis + .25
               xyz = .25
               pt_code$ = "1"
               pt_reason$ = "1/4 Point Misc. (No Pay)"
             goto L62160
               
/* </AWD024> */
L62160:      if p% <> 18% then goto L62165                       /* Code ) */
               e_pts_day = e_pts_day + 1.0
               xyz = 1.0
               pt_code$ = "2"
               pt_reason$ = "(1    ) Point Inclement weather"
/*SR71860 + */ 
L62165:      if p% <> 22% then goto L62170  /* SR71860 */         /* Code [ */
               e_pts_day = e_pts_day + 2.00
               pt_code$ = "2"
               xyz = 2.00
               pt_reason$ = "(2) Point No Call/No Show (Var)"
/*SR71860 - */
L62170:     e_pts_dte$ = date
            e_pts_tot = round(e_pts_late + e_pts_mis + e_pts_day, 2)
            
            gosub update_points

            put #1, using L62015, e_pts_late, e_pts_mis, e_pts_day,       ~
                                 e_pts_tot, e_pts_dte$
            rewrite #1
L62175: return

        update_points
            ptpoint = 0.00
            xx_dept$ = dp$(i%)
            write #5, using L62210, date, xx_dept$, emp_shft$, mt_yr_bi$,~
                      str(sk$(i%),12%,2%), dd$(i%), em$(i%), pt_code$,   ~
                      pt_reason$, xyz, userid$, ptcdeupd$, ptpoint,      ~
                      ptnotes$, ptupdtuser$, " ", eod goto L62220
L62210:       FMT CH(6), CH(3), CH(2), CH(2), CH(2), CH(1), CH(5), CH(1),~
                  CH(30), PD(14,4), CH(3), CH(01), PD(14,4), CH(50),     ~
                  CH(03) /* CH(10) ADP */
L62220:     read #1,hold,key = em$(i%), eod goto L62175
        return

        lookup_variance
           readkey$ = all(hex(00))
           readkey$ = "EMP VARCD"
           descr$ =hex(06)&"Select a Valid Action Variance Code (A - Z)"
           call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2))
        return

        check_employees
           call "SHOSTAT" ("Checking Employee's Clock Entries")
           inc% = 0%
           e_key$ = all(hex(00))
           str(e_key$,1%,3%) = sc_dept$
        check_emp_nxt
           read #1, key 1% > e_key$, using L62310, e_key$, e_status$,     ~
                             shift$, e_days$, eod goto check_emp_done
L62310:       FMT CH(11), POS(152), CH(1), POS(841), CH(2), POS(1018),   ~
                  CH(7)
           if e_status$ <> "A" then goto check_emp_nxt
           e_payg$ = str(e_key$,4%,3%)
           if e_payg$ = "180" or e_payg$ = "190" or e_payg$ = "200" then ~
                                                 goto check_emp_nxt
           shfterr% = 0%                          /* (PWW - add) */
           if shift$ = "00" then gosub shifterr   /* (PWW - add) */
           if shfterr% = 1% then goto L62545      /* (PWW - add) */
           
           if sc_dept$ <> str(e_key$,1%,3%) then goto check_emp_done
           if e_shift$ = "00" then goto L62365            /* All Shifts */
              if shift$ = "00" then goto check_emp_next
              if shift$ <> e_shift$ then goto check_emp_nxt

L62365:    days% = 0%
L62370:    if sc_day$ <> "A" then goto L62395
              days% = days% + 1%
              convert days% to day$, pic(#)
              if day$ <= sv_day$ then goto L62410
                 goto check_emp_nxt
L62395:    day$ = sc_day$
           convert day$ to days%, data goto L62405
L62405:
L62410:    if str(e_days$,days%,1%) = "N" then goto check_emp_nxt
           mt_key$ = all(hex(00))
           str(mt_key$,1%,3%) = sc_dept$
           str(mt_key$,4%,2%) = mt_yr_bi$
           str(mt_key$,6%,5%) = str(e_key$,7%,5%)
           str(mt_key$,11%,2%)= sc_wk$
           str(mt_key$,13%,1%)= day$
           read #3,key = mt_key$, using L62455, mt_var_in$, mt_var_out$,  ~
                         mt_var_day$, mt_proc$, eod goto L62490
L62455:       FMT POS(24), 4*CH(01)
              if sel$ <> "3" and sel$ <> "4" then goto L62475
                 gosub check_data

L62475:       if sc_day$ = "A" then goto L62370
              goto check_emp_nxt

L62490:    if sel$ <> "3" and sel$ <> "4"  then goto L62510
              gosub store_data_in                    /* NO CLOCK IN    */
              goto check_emp_nxt

L62510:    gosub create_clock_entry
           goto check_emp_nxt

        check_emp_done
           if sel$ <> "3" and sel$ <> "4" then goto L62545
              return

L62545: return clear all
           convert inc% to inc$, pic(000)

           call "SHOSTAT" ("Created ("&inc$&") Entries for Employee's")
           call "PAUSE" addr(500%)
        goto inputmode
        

        shifterr                                          /* (PWW - add) */
           call "SHOSTAT" ("Invalid Shift for Emp ("&str(e_key$,7%,5%)&    ~
                          ~"). Please Contact HR")        /* (PWW - add) */
           stop                                           /* (PWW - add) */
           shfterr% = 1%                                  /* (PWW - add) */
        return
        
        create_clock_entry
REM MT_DATE$ = DATE
        dt_sys_dte$ = date
        
            put #3, using L62600, mt_key$, mt_date$, 8%, 0%, "5", "6",    ~
                                 "7", "0", userid$, "   "
L62600:       FMT CH(13), CH(6), 2*BI(2), 4*CH(1), CH(3), CH(3)
            write #3, eod goto L62680

            str(mm_key$,1%,2%) = str(mt_key$,4%,2%)
            str(mm_key$,3%,2%) = str(mt_key$,11%,2%)
            str(mm_key$,5%,1%) = str(mt_key$,13%,1%)
            str(mm_key$,6%,5%) = str(mt_key$,6%,5%)
            str(mm_key$,11%,2%)= str(mt_key$,1%,3%)
            str(mm_key$,14%,5%)= "A800 "
            gosub time_stamp                            /* (AWD040) */
            put #4, using L62660, mm_key$, "2", "6 ", dt_usr$, dt_dte$,   ~
                                  dt_ts$, dt_sys_dte$ /* , dt_fil1$ ADP */
L62660:       FMT CH(18), CH(1), CH(2), CH(3), CH(6), CH(8), CH(06) 
/*, CH(85) ADP */

            write #4, eod goto L62680

            inc% = inc% + 1%
L62680: return

        load_security
            init(" ") readkey$, access$
            security% = 0%
            str(readkey$,1%,9%)   = "EMP SECUR"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L62720, access$, eod goto L62730
L62720:        FMT POS(25), CH(30)
            security% = 1%
L62730: return

        load_access
            init(" ") readkey$, access$
            j% = 1%  : access% = 1%
            gosub load_security
            if security% = 1% then return

            if userid$ = "MCB" then goto L62820        /* (AWD010)  */
            str(readkey$,1%,9%)   = "EMPACCESS"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L62785, access$, eod goto L62815
L62785:        FMT POS(25), CH(30)
            for kk% = 1% to 10%
                convert str(access$,j%,3%) to x%, data goto L62815
                if sc_dept$ = str(access$,j%,3%) then goto L62820
                j% = j% + 3%
            next kk%
L62815: goto access_denied
L62820: return

        access_denied
            access% = 0%
            errormsg$ = "(ERROR) - ACCESS TO DEPARTMENT DENIED?"
            comp% = 2%
            hdr$ = "*** Access Denied to Dept ***"
            msg$(1) = "You Do Not Have Access to Dept/Employee Selected?"
            msg$(2) = "      D e p a r t m e n t   S e c u r i t y      "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        calc_detail
            m_key$ = str(mt_key$,4%,10%)
            read #3,key 1% = m_key$, using L62915, m_key$, eod goto L63035
            goto L62920
        calc_next
            read #3, using L62915, m_key$, eod goto L63035
L62915:        FMT POS(4), CH(10)
L62920:     if str(mt_key$,4%,10%) <> m_key$ then goto L63035
            gosub dataload
            if lcntr% > 57% then gosub print_header
               gosub load_employee
               gosub load_variance
               print using L55230
               print using L55200, mt_emp$, name$, mt_day$,               ~
                     mt_var_in$, mt_var_ind$,                            ~
                     mt_var_out$, mt_var_outd$,                          ~
                     mt_var_day$, mt_var_dayd$,                          ~
                     mt_proc$, mt_procd$, mt_hours$, mt_min$, mt_dept$
               lcntr% = lcntr% + 2%
               x = (mt_hours% * 60.0) + mt_min%
               t_hrs% = round(t_hrs% + x, 2)
REM P% = POS("ACLNPQX" = MT_PROC$)          /* NO PAY CODES */
REM P% = POS("LNPSTXZ" = MT_PROC$)             /* NO PAY CODES */
/* AWD023 */ /* P% = POS("LNPRSTX$&^@" = MT_PROC$) */    /* NO PAY CODES */
/* AWD025 */ /* P% = POS("LNPRSTX$&^@M" = MT_PROC$) */    /* NO PAY CODES */
/* AWD026 */ /* P% = POS("LNPRSTX$&^@MABQ" = MT_PROC$) */  /* NO PAY CODES */
/* AWD034      p% = pos("LNPRSTX$&^@MAB" = mt_proc$)          NO PAY CODES */
/* SR71317 */  p% = pos("LNPRSTX$&^@MAB=;" = mt_proc$)     /* NO PAY CODES */
/* SR71860 */  p% = pos("LNPRSTX$&^@MAB=;[" = mt_proc$)     /* NO PAY CODES */
/* AWD03? */   if p% <> 6% then goto L63020   /* 1/2 day sick */
               x = 240.0
               p% = 0%
L63020:
/* 09306 wk 13 @ */
               if p% <> 0% then goto calc_next
                  t_hrs_p% = round(t_hrs_p% + x, 2)
               if str(mt_key$,1%,3%) <> mt_dept$ then goto calc_next
                  t_hrs_d% = round(t_hrs_d% + x, 2)
               p% = pos("MOYZ0EFR" = mt_proc$)     /* EFFICIENCY HOURS*/
               if p% = 0% then goto calc_next
                  t_hrs_e% = round(t_hrs_e% + x, 2)
               goto calc_next
L63035: return

        time_clock 
            call "APCSCNSB" (#1, #2, #3, #4) 
        return clear all
        goto inputmode

        efficiency
           sc_yr$ = mt_yr$
           err% = 0%
           eff% = 1%
           call "APCEFFWG" (sc_dept$, sc_yr$, sc_wk$, sc_day$, ef(), ef_ov(),~
                       tot_ef(), wg(), e_shift$, #1, #3, #6, #15, eff%, err% ) 

           gosub display_efficiency

        return clear all
        goto inputmode

        display_efficiency
            inpmessage$ = "Press <Return> To Continue, or PF(15)Print Scr~
        ~een "
                                                      /* (EWD003) - Allow */
                                                      /* to see wages     */
                                                      /* (EWD001)      */
REM IF USERID$ <> "RHH" AND USERID$ <> "KMP" AND USERID$ <> "MVK"~
REM AND USERID$ <> "LBH" AND USERID$ <> "LLJ"~
REM AND USERID$ <> "APA" AND USERID$ <> "CMG"~
REM THEN MAT WG = ZER

                                                      /* (EWD001)      */
            init(" ") ef$(), ef_ov$(), tot_ef$(), wg$()
            w, x, y, z = 0.0
            for i% = 1% to 7%
                x = x + ef(i%)
                y = y + ef_ov(i%)
                z = z + tot_ef(i%)
                w = w + wg(i%)
                convert ef(i%) to ef$(i%), pic(######.##-)
                convert ef_ov(i%) to ef_ov$(i%), pic(######.##-)
                convert tot_ef(i%) to tot_ef$(i%), pic(######.##-)
                convert wg(i%) to wg$(i%), pic(######.##-)
            next i%
            convert x to tot_ef$, pic(######.##-)
            convert y to tot_ov$, pic(######.##-)
            convert z to tot_ef_ov$, pic(######.##-)
            convert w to ww$, pic(######.##-)


            etime$ = " "
            call "TIME" (etime$)
            hdr$(1) = "Day of Week"                     /* Lenght (11) */
            hdr$(2) = "Efficiency Hours"                /* Lenght (16) */
            hdr$(3) = "Overtime Hours"                  /* Lenght (14) */
            hdr$(4) = "Total Eff Hours"                 /* Lenght (15) */
            hdr$(5) = "Total Wage"                      /* Lenght (10) */
            eff_text$ = "Production Week (XX) - XX/XX/XX"
            str(eff_text$,18%,2%) = sc_wk$
            str(eff_text$,24%,8%) = sc_wk_dte$

            accept                                                       ~
               at (01,22),                                               ~
                  "A P C  Production Efficiency Reporting",              ~
               at (02,02), "Today:",                                     ~
               at (02,09), fac(hex(84)), date$                  , ch(08),~
               at (02,25), fac(hex(84)), eff_text$              , ch(31),~
               at (02,66), "Time :",                                     ~
               at (02,73), fac(hex(84)), etime$                 , ch(08),~
                                                                         ~
               at (04,28), fac(hex(94)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (06,02), fac(hex(a4)), hdr$(1)                , ch(11),~
               at (06,16), fac(hex(a4)), hdr$(2)                , ch(16),~
               at (06,35), fac(hex(a4)), hdr$(3)                , ch(14),~
               at (06,52), fac(hex(a4)), hdr$(4)                , ch(15),~
               at (06,70), fac(hex(a4)), hdr$(5)                , ch(10),~
                                                                         ~
               at (07,03), "Monday",                                     ~
               at (07,19), fac(hex(84)), ef$(2)                 , ch(10),~
               at (07,37), fac(hex(84)), ef_ov$(2)              , ch(10),~
               at (07,53), fac(hex(84)), tot_ef$(2)             , ch(10),~
               at (07,70), fac(hex(84)), wg$(2)                 , ch(10),~
                                                                         ~
               at (09,03), "Tuesday",                                    ~
               at (09,19), fac(hex(84)), ef$(3)                 , ch(10),~
               at (09,37), fac(hex(84)), ef_ov$(3)              , ch(10),~
               at (09,53), fac(hex(84)), tot_ef$(3)             , ch(10),~
               at (09,70), fac(hex(84)), wg$(3)                 , ch(10),~
                                                                         ~
               at (11,03), "Wednesday",                                  ~
               at (11,19), fac(hex(84)), ef$(4)                 , ch(10),~
               at (11,37), fac(hex(84)), ef_ov$(4)              , ch(10),~
               at (11,53), fac(hex(84)), tot_ef$(4)             , ch(10),~
               at (11,70), fac(hex(84)), wg$(4)                 , ch(10),~
                                                                         ~
               at (13,03), "Thursday",                                   ~
               at (13,19), fac(hex(84)), ef$(5)                 , ch(10),~
               at (13,37), fac(hex(84)), ef_ov$(5)              , ch(10),~
               at (13,53), fac(hex(84)), tot_ef$(5)             , ch(10),~
               at (13,70), fac(hex(84)), wg$(5)                 , ch(10),~
                                                                         ~
               at (15,03), "Friday",                                     ~
               at (15,19), fac(hex(84)), ef$(6)                 , ch(10),~
               at (15,37), fac(hex(84)), ef_ov$(6)              , ch(10),~
               at (15,53), fac(hex(84)), tot_ef$(6)             , ch(10),~
               at (15,70), fac(hex(84)), wg$(6)                 , ch(10),~
                                                                         ~
               at (17,03), "Saturday",                                   ~
               at (17,19), fac(hex(84)), ef$(7)                 , ch(10),~
               at (17,37), fac(hex(84)), ef_ov$(7)              , ch(10),~
               at (17,53), fac(hex(84)), tot_ef$(7)             , ch(10),~
               at (17,70), fac(hex(84)), wg$(7)                 , ch(10),~
                                                                         ~
               at (19,03), "Sunday",                                     ~
               at (19,19), fac(hex(a4)), ef$(1)                 , ch(10),~
               at (19,37), fac(hex(a4)), ef_ov$(1)              , ch(10),~
               at (19,53), fac(hex(a4)), tot_ef$(1)             , ch(10),~
               at (19,70), fac(hex(a4)), wg$(1)                 , ch(10),~
                                                                         ~
               at (20,19), fac(hex(84)), tot_ef$                , ch(10),~
               at (20,37), fac(hex(84)), tot_ov$                , ch(10),~
               at (20,53), fac(hex(84)), tot_ef_ov$             , ch(10),~
               at (20,70), fac(hex(84)), ww$                    , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               keys(hex(00010f10)), key(keyhit%)

               if keyhit% <> 15% then L63655
                  call "PRNTSCRN"

L63655:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        time_stamp
            dt_usr$ = userid$
            dt_dte$ = date
            dt_ts$ = " "
            call "TIME" (dt_ts$)
REM DT_FIL1$ = "   "
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end

/*SR71637 + */
        check_emp_open
            readkey$ = " "
            str(readkey$,1%,9%)   = "EMP OPEN "
        next_emp_open
            read #2,key > readkey$,using N13960, readkey$,      ~
                                   eod goto N13990
N13960:        FMT POS(1), CH(24)
            if str(readkey$,1%,9%) <> "EMP OPEN " then N13990
               if sc1_yr$ <> str(readkey$,10%,4%) then goto next_emp_open
               if sc_wk$ <> str(readkey$,14%,2%) then goto next_emp_open
               if sc_day$ <> str(readkey$,16%,1%) then goto next_emp_open
               emp% = 0%
N13990: return
/*SR71637 - */
