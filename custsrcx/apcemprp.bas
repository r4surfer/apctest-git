REM         *************************************************************~
            *                                                           *~
            *  Program Name      - APCEMPRP                             *~
            *  Creation Date     - 04/01/93                             *~
            *  Last Modified Date- 12/11/2013                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Employee Time Clock utility program  *~
            *                      to print time clock oriented reports,*~
            *                      for a selected production week.      *~
            *                                                           *~
            *  Selections        - (1) Check for Open Variances.        *~
            *                      (2) Vacation, Sick, Points Report    *~
            *                      (3) Weekly Hours By Employee         *~
            *                      (4) Employee Time Clock Detail       *~
            *                      (5) Daily Time Log                   *~
            *                      (6) Weekly Hours Temp Emp's          *~
            *                                                           *~
            *  Code Tables Used  - (EMP SECUR) Time Clock Master        *~
            *                                  Security File gives      *~
            *                                  Access to all Depts and  *~
            *                                  functions.               *~
            *                      (EMPACCESS) Time Clock Access To     *~
            *                                  Team Leaders for Specific*~
            *                                  Departments.             *~
            *                    - (EMP VARCD) Time Clock Employee      *~
            *                                  Variance Codes.          *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/01/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/16/96 ! Mod to use new subroutine to calculate   ! RHH *~
            *          !   the current production year, week, day !     *~
            *          !   and validate the entered production    !     *~
            *          !   year, week, and day. (Sub-APCPLN0B)    !     *~
            * 10/30/97 ! Mod to                                   ! RHH *~
            * 03/25/98 ! Y2K modifcations                         ! ERN *~
            * 05/01/00 ! Mod to build new file EWDPYROL to import ! CMG *~
            *          !  into Excel to send to Atrium. (EWD001)  !     *~
            * 02/13/01 ! Mod to build new file EWDPYTMP to import ! CMG *~
            *          !  into Excel to send to Atrium. (EWD002)  !     *~
            * 06/01/01 ! Mod to exclude sick hours in total pay   ! CMG *~
            *          !  hours.                        (EWD003)  !     *~
            * 07/02/01 ! (EWD004) Mod so that can handle 1/2 sick ! CMG *~
            *          !        days.                             !     *~
            * 08/21/01 ! (EWD005) Additional mods for temp payroll! CMG *~
            *          !            file - add address1 & pay rate!     *~
            * 02/11/02 ! (EWD006) Mods for ESS time clock and     ! CMG *~
            *          !            payroll.                      !     *~
            * 11/25/02 ! (EWD007) Mods to add extra 12 hour shifts! CMG *~
            *          !            for EES.                      !     *~
            * 12/18/02 ! (EWD008) Mod to add 'Z' variance code    ! CMG *~
            *          !            for no pay.                   !     *~
            * 08/12/04 ! (EWD009) Mod to give emp in dept 56 with ! CMG *~
            *          !          shift 11,12,13,or 14 overtime at!     *~
            *          !          36 hours                        !     *~
            * 01/21/05 ! (AWD010) Mod so that can handle 1/2 sick ! CMG *~
            *          !        days.                             !     *~
            * 02/15/05 ! (AWD011) Mod to elimate Easysoft         ! CMG *~
            * 08/10/05 ! (AWD012) Mod to add new variance code W  ! SML *~
            *          !          Unscheduled Vacatn day w/Point  !     *~
            * 03/27/06 ! (AWD013) allow dept 55 to be processed in! CMG *~
            *          !         payroll reporting                !     *~
            * 02/07/07 ! (AWD014) omit death, holiday & jury time ! DES *~
            *          !         from overtime hours              !     *~
            *05/15/2008! (AWD015) 1 1/2 points with pay "M"       ! DES *~
            *10/23/2008! (AWD017) new leave codes                 ! DES *~
            *02/27/2010! (AWD018) report 5 printing when shouldn't! DES *~
            *03/24/2010! (AWD019) add new no-pay codes "C" & "Z"  ! DES *~
            *07/13/2010! (AWD020) add new no-pay codes "ABIQ"     ! DES *~
            *10/06/2010! (AWD021) mod for new report options      ! CMG *~
            *02/16/2011! (AWD022) filter exempt emp. from rpt 3.  ! DES *~
            *08/10/2012! (AWD023) remove dept 57 from report 3    ! DES *~
            *11/12/2012! (AWD024) mod for weeksemp Sunday to Satur! CMG *~
            *01/07/2013! (AWD025) mods for variance code '+'      ! CMG *~
            *01/11/2013! (AWD026) mod to use NTX                  ! CMG *~
            *03/30/2013! (AWD027) mod for file lengths            ! CMG *~
            *12/11/2013! (AWD028) mod to change vac and sick earn ! CMG *~
            *          !  to allow half day entries               !     *~
            *03/06/2014! (AWD029) mod to prevent crash when an    ! PWW *~
            *          !  employee has two floating holidays.     !     *~
            *04/14/2014! (AWD030) mod to allow for three          ! PWW *~
            *          !  floating holidays.                      !     *~
            *02/11/2015! (AWD031) mod to fix 1/2 days vacation &  ! PWW *~
            *          !  1/2 day sick accumulations.             !     *~
            *02/12/2015! (AWD032) mod to add dept #87 & #99.      ! PWW *~
            *03/05/2015! (AWD033) Added 1/2 Day"}" Sick(Paid) and ! PWW *~
            *          !          1 Day"{" Sick(Paid). Also sep-  !     *~
            *          ! erate out Jury,Death & Sick Paid.        !     *~
            *01/14/2016! SR71972  Fix issue with Floating Holiday ! PWW *~
            *          !          and Family Death on Payroll     !     *~
            *          !          Report.                         !     *~
            *01/26/2016! SR71639  Seperate Float Holiday column   ! PWW *~
            *01/26/2016! SR71317  Added Drs Note "=" no point(no  ! PWW *~
            *          !          pay) and School Note ";" no     !     *~
            *          !          point(no pay).                  !     *~
            *01/26/2016! SR71971  Add another Floating Holiday.   ! PWW *~
            *03/02/2016! SR71639a Fix for SR71639 from above. Hol-! PWW *~
            *          !          iday <> match export files. Add-!     *~
            *          !          ed S & T back into report.      !     *~
            *03/25/2016! SR71860  Add "[" 2 pts No Call/No Show   ! PWW *~
            *01/29/2019! CR-1821  ADP Increase Emp Id field length! DES *~
            *02/22/2019! CR-1894  Increase size of EMP DEPT to 3  ! DES *~
            *************************************************************
 
        dim des_key$22,                                                  ~
            dr(8%,15%), dov(8%,15%),     /* Weekly Reg and Overtime Hrs*/~
            dr$10, dov$10,               /*                            */~
            dd$(1000%)3, quote$1,          /* Departments                */~
            var_check$5, py_rec$256,     /* SEL (3) - (WK NO)          */~
            access$30, hdr$40,           /* Security Check             */~
            msg$(3%)79,                  /* Security Check             */~
            e_shft$2,                    /* Employee Shift     (EWD006)*/~
            e_reg$10,e_ovr$10,e_sick$10, /* SELECTION (3) HOURS        */~
            e_jury$10,e_death$10,        /* AWD014                     */~
            w_jury$10,w_death$10,        /* AWD014                     */~
            e_vac$10,e_hol$10,e_tot$10,  /*                            */~
            e_sick_p$10,w_sick_p$10   ,  /* AWD033                     */~
            e_add1$30,                   /* Employee Address1  (EWD005)*/~
            scr$(11%)70,                 /* Screen Report Options      */~
            sav_dept$3, save_dept$3,     /* SELECTION (2) DEPARTMENT   */~
            sc_dept$3, sc_dept_d$30,     /* SCREEN DEPARTMENT          */~
            sc_wk$2, sc_wk_dte$8,        /* SCREEN WEEK                */~
            sc_day$1,                    /* SCREEN PRODUCTION DAY      */~
            sv_wk$2, sv_day$1,           /* CURRENT WEEK AND DAY       */~
            dept$(999%)3,                /* Store Department Codes     */~
            days$(7%)9,                  /* DAYS OF THE WEEK           */~
            days$9,                      /* DAY OF THE WEEK            */~
            date$8,                      /* SCREEN DATE                */~
            code$1,                      /* Used in LOOKUP_VARIANCE    */~
            mt_dept$3, mt_dept_d$30,     /* Department Code            */~
            mt_wk$2,                     /* Production Week            */~
            mt_day$1,                    /* Production Day             */~
            mt_date$6, mt_date_dte$8,    /* Current Date (PRODUCTION)  */~
            mt_yr$4, prv_yr$4, sc_yr$4,  /* Current Year - PREVIOUS    */~
            cur_yr_bi$2, prv_yr_bi$2,    /* Binary reps of years       */~
            ent_yr_bi$2, mt_yr_bi$2,     /*                            */~
            mt_emp$,                     /* EMPLOYEE NUMBER            */~
            mt_hours$2,                  /* DAILY HOURS EMPLOYEE       */~
            mt_min$2,                    /* DAILY MINUTES EMPLOYEE     */~
            hours$5,                     /* HOURS AND MINUTES          */~
            lunch$8, time$10,            /* LUNCH AND TIME FOR RPT (5) */~
            in$(10%)10, out$(10%)10,     /* TIME STAMPS                */~
            mt_var_in$1,                 /* CLOCK IN VARIANCE          */~
            mt_var_ind$15,               /*                            */~
            mt_var_out$1,                /* CLOCK OUT VARIANCE         */~
            mt_var_outd$15,              /*                            */~
            mt_var_day$1,                /* DAILY VARIANCE             */~
            mt_var_dayd$15,              /*                            */~
            mt_var_desc$15,              /* DAILY VARIANCE DESCRIPT    */~
            mt_proc$1,                   /* PROCESS/CHANGE FLAG        */~
            mt_procd$15,                 /*                            */~
            mt_user$3, mt_name$15,       /* Last Mod By                */~
/* ADP  MT_FIL$35,                          FILLER AREA                */~
/* ADP  MT_REC$32,                          APCEMPMT - RECORD          */~
            jdate$5,                     /* Julian Date                */~
            mt_key$13,                   /* Master Key(s)              */~
            sav_key$10, sav_emp$5,       /* Save Master Key, Emp No.   */~
            sel$1, sel_d$30,             /* Variance Selection         */~
            hol$1, hol_d$16,             /* Number of Holidays Sel (3) */~
            e_key$11, e_key1$5,          /* Employee Master Key        */~
            e_status$1,                  /* Employee Status Code A,I,T */~
            title$40,                    /* REPORT TITLE               */~
            runtime$8,                   /* REPORT RUN TIME            */~
            readkey$50, desc$30,         /* Generic Key                */~
            c_t$(30%)5, c_t1$(30%)8,     /* CLOCK TIMES                */~
            status$(30%)14, dp$(30%)3,   /* CLOCK STATUS / Department  */~
            c_th%(30%), c_tm%(30%),      /* CLOCK HOURS AND MINUTES    */~
            c_th$(30%)2, c_tm$(30%)2,    /* CLOCK HOURS AND MINUTES    */~
            status1$(30%)14,             /* Status For Dept Variance   */~
            l_usr$(30%)16,               /* Last Mod By                */~
            dt_key$18, d_code$1,         /* DETAIL KEY AND CODE        */~
            dt_rec$128, dt_usr$3,        /* Detail Record              */~
            dt_wk$2, dt_day$1,           /* DETAIL WEEK AND DAY        */~
            sc_emp$5, sc_emp_d$30,       /* Screen Employee Number     */~
            time_txt$11,                 /* TIME TEXT DAILY TOTAL EMP  */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            name$20                      /* Emp Name last, first       */

        dim                              /* Subroutine - Variables     */~
            cur_yr$4,                    /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4,                    /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8,       /* Prod Week Date Form/Unform */~
            float_key$8, float_sym$1     /* <AWD030>                   */

        dim f2%(50%),                     /* = 0 if the file is open    */~
            f1%(50%),                     /* = 1 if READ was successful */~
            fs%(50%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(50%)20                  /* Text from file opening     */

/* (EWD001)  */
        dim py_key$12,                   /* EWDPYROL ReadKey           */~
            py_key1$14,                  /* AWDPYROL ReadKey           */~
            py_dept$6,                   /* Employee Dept              */~
            py_corp_dept$4,              /* Employee Corporate Dept    */~
            py_no$6,                     /* Employee Number            */~
            py_code$3,                   /* Company Code               */~
            py_batch$1,                  /* Company Batch ID           */~
            py_name$20,                  /* Employee Name              */~
            py_reg_hrs$14,               /* Regular Emp Hours (AWD011) */~
            py_ovr_hrs$14,               /* Regular Overtime  (AWD011) */~
            py_hol_hrs$14,               /* Holiday Hours     (AWD011) */~
/*SR71639*/ py_fol_hrs$14,               /* Float H Hours     SR71639  */~
/*AWD033+*/ py_jur_hrs$14,               /* Jury    Hours     (AWD011) */~
            py_dth_hrs$14,               /* Death   Hours     (AWD011) */~
/*AWD033-*/ py_sic_hrs$14,               /* Sick    Hours     (AWD011) */~
            py_tot_hrs$14,               /* Total Hours       (AWD011) */~
            py_pay_rate$14               /* Pay Rate          (AWD011) */

        dim library$8                    /* File Library Information   */
        dim schema$8                     /* Schema                     */
        dim file$8                       /* File Name                  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Employee Time Clock Reporting Utility   "
            pname$ = "APCEMPRP - Rev: R6.04"

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
            * #5  ! APCEMPWK ! APC Employee Work File                   *~
            * #6  ! USERCLMS ! Master User Id File                      *~
            * #7  ! EWDPYROL ! File to send to Atrium                   *~
            * #8  ! APCPYROL ! Employee Payroll Data                    *~
            * #9  ! APCEMPPR ! Employee Payroll Data                    *~
            * #10 ! AWDPAYSV ! File to save pay rate information        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "APCEMPLY",                                     ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    7, keylen =    5,                    ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos =   12, keylen =  26, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "APCEMPMT",                                     ~
                        varc,     indexed,  recsize =  128,   /*ADP*/    ~
                        keypos =    1, keylen =   13,                    ~
                        alt key  1, keypos =    4, keylen =  10, dup, ~
                            key  2, keypos =   82, keylen =   6, dup

            select #4,  "APCEMPDT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  18,                     ~
                        alt key 1, keypos = 114, keylen =  6, dup /*ADP */

            select #6,  "USERLCMS",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

            select #7,  "EWDPYROL",                                      ~
                        varc, indexed,  recsize = 128,                   ~
                        keypos =   5, keylen =   12,                     ~
                        alt key  1, keypos =   1, keylen =  16


            select #8,  "AWDPYROL",                                      ~
                        varc, indexed,  recsize = 256,                   ~
                        keypos =   7, keylen =   23,                     ~
                        alt key  1, keypos =   1, keylen =  22
                        
            select #18, "AESPYROL",                                      ~
                        varc, indexed,  recsize = 256,                   ~
                        keypos =   7, keylen =   23,                     ~
                        alt key  1, keypos =   1, keylen =  22                        

            select #9,  "APCEMPPR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos = 1,    keylen =  5

            select #10, "AWDPAYSV",                                      ~
                        varc, indexed,  recsize = 80,                    ~
/*AWD033                varc, indexed,  recsize = 104,               */  ~
                        keypos =   1, keylen =   22

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 300,               ~
/*                      keypos =    1, keylen = 07                 */    ~
/*<AWD030>              keypos =    1, keylen = 08                 */    ~
/*<CR1894> */           keypos =    1, keylen = 09

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),500%, rslt$(4%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),500%, rslt$(6%))
        REM CALL "OPENOLIB" (#6, "SHARE", F2%(6%), RSLT$(6%), AXD$)
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),500%, rslt$(10%))
            call "WORKOPEN" (#50, "IO   ", 10%, f2%(50))


/* check for NE */
            schema_err%, schema% = 0%
            write_cnt% = 0%
            quote$ = hex(22)
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #2, schema_err%)

            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"

            if schema% = 1% then library$ = "APCDATA"
            if schema% = 2% then library$ = "NEDATA"


/* (EWD001) - Begin  */
            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%,  rslt$(7%))
            if f2%(7%) <> 0% then goto L00100
                  call "FILEBGON" addr(#7)
L00100
            open nodisplay #7, output, file = "EWDPYROL",         ~
                 library = library$, volume = volume$

REM              LIBRARY = "APCDATA", VOLUME = "CARLO2"
REM            OPEN NODISPLAY #7, OUTPUT, FILE = "EWDPYROL",         ~
REM                 LIBRARY = "TESTDATA", VOLUME = "EWD2"
            CLOSE #7

            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
/* (EWD001) - End  */



            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
/*(AWD024)*/
            days$(1%) = "MONDAY   "  : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY"  : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   "  : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "
/*(CR1489) */
REM DAYS$(1%) = "SUNDAY   "
REM DAYS$(2%) = "MONDAY   "  DAYS$(3%) = "TUESDAY  "
REM DAYS$(4%) = "WEDNESDAY"  DAYS$(5%) = "THURSDAY "
REM DAYS$(6%) = "FRIDAY   "  DAYS$(7%) = "SATURDAY "
/*(\AWD024) */


            date$ = date
            call "DATEFMT" (date$)

/*(AWD021) Screen Modifications */
REM                             10        20        30        40        50
REM                     60        70
REM                     1234567890123456789012345678901234567890123456789012345
REM                     678901234567890
            scr$(1%) = "******************************************************~
                       ~***************"
            scr$(2%) = "*                          Report Selections          ~
                       ~              *"
            scr$(3%) = "*                                                     ~
                       ~              *"
            scr$(4%) = "*(1) - Check for Open Variance's.    (8) - Weekly Hour~
                       ~s TX AES      *"
            scr$(5%) = "*(2) - Vacation, Sick, Points.                        ~
                       ~              *"
            scr$(6%) = "*(3) - Weekly Hours by Employee                       ~
                       ~              *"
            scr$(7%) = "*(4) - Employee Time Clock Detail                     ~
                       ~              *"
            scr$(8%) = "*(5) - Daily Time Log                                 ~
                       ~              *"
            scr$(9%) = "*(6) - Weekly Hours Temp Employees                    ~
                       ~              *"
            scr$(10%)= "*(7) - Weekly Hours ESS  Employees                    ~
                       ~              *"   /* (EWD006) */
            scr$(11%)= "******************************************************~
                       ~***************"

            var_check$ = "WK NO"          /* (EWD001)  */
REM            py_code$ = "N7L"              /* (EWD001)  */
            py_batch$ = "1"               /* (EWD001)  */
            gosub load_depts

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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto  exit_program
                  if keyhit% <>  0% then       editpg1

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
            dept% = 0%
            hol_prnt% = 0%                                    /* (AWD025) */
            gosub select_printer
            if sel$ = "1" then gosub scan_data
            if sel$ = "2" then gosub vacation_points
            if sel$ = "3" then gosub weekly_hours
            if sel$ = "4" then gosub scan_data
            if sel$ = "5" then gosub daily_log
            if sel$ = "6" then gosub weekly_hours
            if sel$ = "7" then gosub weekly_hours              /* (EWD006)  */
            if sel$ = "8" then gosub weekly_hours              /* (AWD021)  */
            print using L55040
            close printer
            if fx% <> 0% then CLOSE #fx%
            gosub float_holiday                                /* (AWD025)  */
        return clear all
        goto inputmode

        select_printer
            call "SHOSTAT" ("Printing Report")
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
            if sel$ = "1" then                                           ~
               title$ = "     Open Time Clock Variance Report    "
            if sel$ = "2" then                                           ~
               title$ = "Vacation Days, Sick Days, Points Report "
            if sel$ = "3" then                                           ~
               title$ = "       Weekly Hours by Employee         "
            if sel$ = "4" then                                           ~
               title$ = "   Employee Time Clock Daily Detail     "
            if sel$ = "5" then                                           ~
               title$ = "    Daily Time Stamp Detail Report      "
            if sel$ = "6" then                                           ~
               title$ = "       Weekly Hours Temp Employees      "
            if sel$ = "7" then                                           ~
               title$ = "       Weekly Hours ESS Employees       "
/* (AWD021) */
            if sel$ = "8" then                                           ~
               title$ = "       Weekly Hours TEMP TX AES Employees       "
            if sel$ = "9" then                                           ~
               title$ = "       Weekly Hours TX AES Employees       "
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
         "Enter a Valid Report Selection? ( 1 thru 6 )                 ",~
         "Enter a Valid Department Code or (AA)= All?                  ",~
         "Enter a Valid Production Week,(1-52),(AA)=All(Blank=Current)?",~
         "Enter a Valid Production Day (1-7), (A)ll, (Blank=Current)?  ",~
         "Enter a Valid Employee Number, or Default = ALL.             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, mt_dept$, mt_dept_d$,      ~
                      mt_wk$, mt_date_dte$, mt_day$, sel$, sel_d$,       ~
                      mt_date$, hol$, hol_d$, e_key1$, sav_dept$,        ~
                      mt_yr$, jdate$, days$, mt_key$, sav_key$, dt_key$, ~
                      mt_emp$, mt_hours$, mt_min$, hours$, dt_wk$,       ~
                      mt_var_in$, mt_var_out$, mt_var_day$,mt_var_desc$, ~
                      mt_proc$, sc_dept$, sc_dept_d$,                    ~
                      sc_wk$, sc_wk_dte$, sc_day$, e_key$, e_status$,    ~
                      title$, runtime$, name$, sav_emp$, dt_day$,        ~
                      c_t$(), c_t1$(), c_th$(), c_tm$(), status$(),      ~
                      time_txt$, sc_emp$, sc_emp_d$, sc_yr$
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
/*, MT_FIL$ ADP*/
            temp% = val(mt_yr_bi$,2)
            convert temp% to mt_yr$, pic (####)
            mt_date_dte$ = mt_date$
            call "DATEFMT" (mt_date_dte$)
            init(" ") readkey$, mt_dept_d$, mt_var_desc$, mt_procd$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) =  mt_dept$
            read #2,key = readkey$, using L30150,mt_dept_d$,eod goto L30160
L30150:         FMT POS(25), CH(30)
L30160:     code$ = mt_var_day$ : gosub lookup_variance
            mt_var_desc$ = desc$
            code$ = mt_proc$    : gosub lookup_variance
            mt_procd$ = desc$
            convert mt_hours% to mt_hours$, pic(00)
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
            CH(02),                      /* Production Year (YYYY)     */~
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
            CH(03)                       /* Last Modified By Userid    */
REM ADP CH(35)                       /* FILLER AREA    (AWD027)    */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210,         /* Report Selection     */~
                                L40200,         /* Number of Holidays   */~
                                L40200,         /* Department Code, ALL */~
                                L40200,         /* Production Week, ALL */~
                                L40200,         /* Production Day or All*/~
                                L40200          /* Employee Number      */
              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Report Selection     :",                     ~
               at (03,30), fac(lfac$(1%)), sel$                 , ch(01),~
               at (03,40), fac(hex(84)), sel_d$                 , ch(30),~
                                                                         ~
               at (04,02), "Department Code, (ALL):",                    ~
               at (04,30), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (05,02), "Production Week, 1-52:",                     ~
               at (05,30), fac(lfac$(3%)), sc_wk$               , ch(02),~
               at (05,40), fac(hex(84)), sc_wk_dte$             , ch(08),~
               at (05,50), fac(lfac$(3%)), sc_yr$               , ch(04),~
                                                                         ~
               at (06,02), "Production Day(1 - 7):",                     ~
               at (06,30), fac(lfac$(4%)), sc_day$              , ch(01),~
               at (06,40), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (07,02), "Employee No. or ALL  :",                     ~
               at (07,30), fac(lfac$(5%)), sc_emp$              , ch(05),~
               at (07,40), fac(hex(84)), sc_emp_d$              , ch(20),~
                                                                         ~
               at (10,05), fac(hex(84)), scr$(1%)               , ch(70),~
               at (11,05), fac(hex(84)), scr$(2%)               , ch(70),~
               at (12,05), fac(hex(84)), scr$(3%)               , ch(70),~
               at (13,05), fac(hex(84)), scr$(4%)               , ch(70),~
               at (14,05), fac(hex(84)), scr$(5%)               , ch(70),~
               at (15,05), fac(hex(84)), scr$(6%)               , ch(70),~
               at (16,05), fac(hex(84)), scr$(7%)               , ch(70),~
               at (17,05), fac(hex(84)), scr$(8%)               , ch(70),~
               at (18,05), fac(hex(84)), scr$(9%)               , ch(70),~
               at (19,05), fac(hex(84)), scr$(10%)              , ch(70),~
               at (20,05), fac(hex(84)), scr$(11%)              , ch(70),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40730
                  call "PRNTSCRN"
                  goto L40230

L40730:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40920     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40880
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40880:     if fieldnr% > 1% then L40900
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40900:     return

L40920: if fieldnr% > 0% then L41010  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L41010:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Report Selection      */ ~
                              L50450,         /* Department Code or All*/ ~
                              L50730,         /* Production Week       */ ~
                              L51350,         /* Production Day        */ ~
                              L51660          /* Employee Number       */
            return

L50150: REM Report Selection                           SEL$
           security% = 0%
           if sel$ <> " " then goto L50190
              sel$ = "1"
L50190:    convert sel$ to sel%, data goto L50380

           if sel% < 1% or sel% > 8% then goto L50380   /* (AWD026) */
              if sel$ = "1" then sel_d$ = "Check for Open Variance's"
              if sel$ = "2" then sel_d$ = "Vacation,Sick,Points Rpt."
              if sel$ = "3" then sel_d$ = "Weekly Hours by Employee "
              if sel$ = "4" then sel_d$ = "Employee Time Clock Det'l"
              if sel$ = "5" then sel_d$ = "Daily Time Log           "
              if sel$ = "6" then sel_d$ = "Weekly Hours Temp Emp's  "
              if sel$ = "7" then sel_d$ = "Weekly Hours ESS Emp's   "
/* (AWD021) */
              if sel$ = "8" then sel_d$ = "Wk Hrs TX AES Emp's  "

              reg = 40.0
              gosub load_security
              pp% = pos("136" = sel$)
              if pp% = 0% then return
                 if security% = 0% then goto L50410
                 if edit% = 1% then goto L50350   /* Default All Dept's */
        return
L50350:    gosub L50450
           fieldnr% = 2%
        return
L50380:    errormsg$ = "(Error) - Invalid Report Selection Entered?"
           init(" ") sel$, sel_d$
        return
L50410:    errormsg$ = "(Error) - Access to Report Selection Denied??"
           init(" ")sel$, sel_d$
        return

L50450: REM Department Code                       SC_DEPT$
            sc_dept% = 0%
            if sel$ = "1" or sel$ = "3" or sel$ = "6" then               ~
                                                      sc_dept$ = "AL"
            if sel$ = "7" then sc_dept$ = "060"         /*  (EWD006)  */
            if sel$ = "8" and  sc_dept$ <> "057" and  ~
                               sc_dept$ <> "069" then  sc_dept$ = "069"
REM         if sel$ = "8" then sc_dept$ = "069"         /*  (AWD021)  */
            if sc_dept$ <> " " then goto L50540
L50500:        sc_dept$ = "ALL"
               sc_dept_d$ = "(ALL)-All Depart's Report-Only"
               if security% = 0% then goto L50690
               return
L50540:     if str(sc_dept$,1%,1%) = "A" then goto L50500
               convert sc_dept$ to sc_dept%, data goto L50630
               convert sc_dept% to sc_dept$, pic(000)

               gosub load_access
               if access% <> 1% then goto L50660
                  gosub lookup_dept
        return
L50630:     errormsg$ = "(Error) - Invalid Department Code?"
            init(" ") sc_dept$, sc_dept_d$
        return
L50660:     errormsg$ = "(Error) - Access To Department Denied?"
            init(" ") sc_dept$, sc_dept_d$
        return
L50690:     errormsg$ = "(Error) - Access To All Departments Denied?"
            init(" ") sc_dept$, sc_dept_d$
        return

L50730: REM Production Week                       MT_WK$
            if sel$ = "2" then sc_wk$ = "AA"

           init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,     ~
                     ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,     ~
                     cur_yr_bi$, ent_yr_bi$, prv_yr_bi$

           if str(sc_wk$,1%,1%) <> "A" then goto L50830
              sc_wk$ = "AA"
              sc_wk_dte$ = "ALL- Rpt"

L50830:    if sc_yr$ = " " then goto L50831
                ent_yr$ = sc_yr$
                convert ent_yr$ to temp%, data goto L51200
                ent_yr_bi$ = bin(temp%, 2)
L50831:    if sc_wk$ <> " " and sc_wk$ <> "AA" then ent_wk$ = sc_wk$
           if sc_day$ <> " " then ent_dy$ = sc_day$
/*(AWD024) change to AWDEMP0B */
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

            if pl_e% <> 0% then goto L51200

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)
            temp% = val(prv_yr_bi$,2)
            convert temp% to prv_yr$, pic (####)

            mt_date$ = ent_dte$                      /* Current Date   */
            mt_yr$  = ent_yr$
            sc_yr$  = ent_yr$
            sv_wk$  = ent_wk$
            sv_day$ = ent_dy$
            wk% = 0%
            convert sv_wk$ to wk%, data goto L51200

            convert sv_day$ to i%, data goto L51200

            if sc_wk$ <> "AA" then sc_wk$  = sv_wk$
            if sc_day$ <> "A" then sc_day$ = sv_day$
            if sc_day$ <> "A" then days$   = days$(i%)
            if sc_wk$ <> "AA" then sc_wk_dte$ = ent_date$
            if sc_yr$ > cur_yr$ then goto L51300
            if sc_yr$ < cur_yr$ then goto L51190
            if sc_wk$ = "AA" then return
               if sc_wk$ > cur_wk$ and sc_wk$ <> "52" then goto L51230
L51190: return
L51200:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, sc_yr$, days$, sc_yr$
        return
L51230:    errormsg$ = "(Error) - Future Production Week,Less than Equal ~
        ~to ("& cur_wk$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
           errormsg$ = "(Error) - Inv Prod. Week (1 thru 52),AA - N/A?"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L51300:    errormsg$ = "(Error) - Future Production Year,Less than Equal ~
        ~to ("& cur_yr$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return

L51350: REM Production Day
           if sel$ = "5" then goto L51400
           if sel$ = "1" then goto L51400
           if sel$ = "4" then goto L51400
              goto L51410
L51400:    if sc_day$ <> " " then goto L51420
L51410:       sc_day$ = "A"
L51420:    if sc_day$ <> "A" then goto L51460
              days$ = "All_Prod "
              return

L51460:    convert sc_day$ to x%, data goto L51580

           convert cur_dy$ to y%, data goto L51580

           if x% < 1% or x% > 7% then goto L51580

           if sc_yr$ < cur_yr$ then goto L51560
           if sc_wk$ <> cur_wk$ then goto L51560
           if x% > y% then goto L51610

L51560:    days$ = days$(x%)
        return
L51580:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           init(" ") sc_day$, days$
        return
L51610:    errormsg$ = "(Error) - Future Production Day,Less than Equal t~
        ~o (" & sv_day$ & ")"
           init(" ") sc_day$, days$
        return

L51660: REM Employee Number                         SC_EMP$
            if sc_emp$ <> " " then goto L51710
L51680:        sc_emp$   = "ALL  "
               sc_emp_d$ = "(ALL) - Employee's"
               return
L51710:     if str(sc_emp$,1%,3%) = "ALL" then goto L51680
            mt_emp$ = sc_emp$
            gosub load_employee
            if name% = 0% then goto L51770
               sc_emp_d$ = name$
        return
L51770:     errormsg$ = "(Error) - Invalid Employee Number?"
            init(" ") sc_emp$, sc_emp_d$
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

L55110: %! Production Week: ##   ########                         #######~
        ~#########                                                       !

L55140: %! Production Week: ##   ########    Prod Day: (#) #########     ~
        ~               Department: (###) ############################## !

L55170: %!                                                               ~
        ~                                                                !
                                              /* Report Selcection (1) */
L55200: %!<--- Department ------->!Prod WeeK!Prod Day!Employee! Last Name~
        ~,First, Mid ! In ! Out ! Day ! Daily Description    ! Hrs   Min !
L55220: %!### ################### !   ##    !    #   !  ##### ! #########~
        ~########### ! #  !  #  !  #  ! #################### ! ###    ## !
L55240: %!------------------------!---------!--------!--------!----------~
        ~------------!----!-----!-----!----------------------!-----------!
                                              /* Report Selection (2)  */
L55270: %!<--- Department ---->!Emply!Last Name, First Mid!Vc-Days Dte  !~
        ~Vc-Used Date!Sk-Days Dte !Sk-Used Date!Late!Mis!Day!Tot!Pts Dte !
L55290: %!### #################!#####!####################!##.#-########!~
        ~#.#-########!#.#-########!#.#-########!#.# !#.#!#.#!#.#!########!
L55320: %!---------------------!-----!--------------------!-------------!~
        ~------------!------------!------------!----!---!---!---!--------!
                                              /* Report Selection (3)  */
/* <AWD014> */
L55370: %!     !                    !     !          !               !   ~
        ~         !                !Jury/Fam. Death!                     !
/* </AWD014> */
L55360: %!Emply!Last Name, First Mid! Dept!Reg Hours !Overtime Hours ! Si~
        ~ck Hours ! Vacation Hours !/Holiday Hours ! Employee Total Hours!
L55380: %!#####!####################! ### !##########!   ##########  ! ##~
        ~######## !   ##########   !  ##########   !      ##########     !

L55410: %!-----!--------------------!-----!----------!---------------!---~
        ~---------!----------------!---------------!---------------------!

                                              /* Report Selection (4)  */
L55450: %!Emply!Last Name, First Mid!Week!Day!Clock In!Clock Ot! Hrs Min!~
        ~Daily Total!Dpt! Clk Variance !Dept Variance ! Last Modified By !
L55470: %!#####!####################! ## ! # !########!########! ##  ## !~
        ~###########!###!##############!##############! ################ !

L55500: %!-----!--------------------!----!---!--------!--------!--------!~
        ~-----------!---!--------------!--------------!------------------!
                                              /* Report Selection (5)  */
L55530: %!Emply!Last Name, First Mid!Day! Clock In !Clock Out ! Lunch!Hrs~
        ~ Min!  Clock Variance  !  Dept Variance   !  Last Modified By   !
L55550: %!#####!####################! # !##########!##########!######!## ~
        ~ ## ! # ###############! # ###############! ###-############### !
L55570: %!-----!--------------------!---!----------!----------!------!---~
        ~----!------------------!------------------!---------------------!

/* (AWD025) */
L55580: %!   N O    F L O A T I N G    H O L I D A Y    U S E D          ~
        ~                                                                !
/*5390: %!Emply!Last Name, First Mid! Dept!                            ~*/
/*<AWD030>*/
L55390: %!Emply!Last Name, First Mid! Dept!F!                            ~
        ~                                                                !
/*5391: %!-----!--------------------!-----!                            ~*/
/*<AWD030>*/
L55391: %!-----!--------------------!-----!-!                            ~
        ~                                                                !

/*5395: %!#####!####################! ### !                            ~*/
/*<AWD030>*/
L55395: %!#####!####################! ### !#!                            ~
        ~                                                                !



        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        lookup_user
           if mt_user$ = "EMP" then goto L60050
           init(" ") mt_name$
           read #6,key = mt_user$, using L60040, mt_name$, eod goto L60050
L60040:       FMT POS(4), CH(15)
        return
L60050:    mt_user$ = "EMP"
           mt_name$ = "Time Clock System"
        return

        lookup_variance
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "EMP VARCD"
           str(readkey$,10%,15%) = code$
           read #2,key = readkey$, using L60095, desc$, eod goto L60100
L60095:       FMT POS(25), CH(30)
L60100: return

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55170
           print using L55060
           lcntr% = 4%
           if sel$ <> "1" then goto L60170
              print using L55200
              goto L60230
L60170:    if sel$ <> "2" then goto L60185
              print using L55270
              goto L60230                                   /*  (EWD006)  */
L60185:    if sel$ <> "3" and sel$ <> "6" and sel$ <> "7" then goto L60215
              print using L55110, sc_wk$, sc_wk_dte$, hol_d$
/* <AWD014> */
              print using L55060
REM           PRINT USING L55360
             if hol_prnt% = 1% then goto hol_header
              print using L55370
              print using L55360
REM           LCNTR% = LCNTR% + 2%
              lcntr% = lcntr% + 4%
              goto L60230
/* <AWD014> */
hol_header:
              print using L55390
              lcntr% = lcntr% + 2%
              goto L60230
L60215:    if sel$ <> "4" then goto L60235
              print using L55450

L60230:       lcntr% = lcntr% + 1%
L60235:    if sel$ <> "5" then goto L60265
              print using L55140, sc_wk$, sc_wk_dte$, sc_day$, days$,     ~
                                 sc_dept$, sc_dept_d$
              print using L55060
              print using L55530
              lcntr% = lcntr% + 3%
L60265: return

        print_detail_1
            if lcntr% > 57% then gosub print_header
               if sav_dept$ <> " " then goto L60295
                  goto L60305
L60295:        if sav_dept$ = mt_dept$ then goto L60315
                  gosub print_header
L60305:           sav_dept$ = mt_dept$

L60315:        print using L55240
               print using L55220, mt_dept$, mt_dept_d$, mt_wk$, mt_day$, ~
                     mt_emp$, name$,                                     ~
                     mt_var_in$, mt_var_out$, mt_var_day$, mt_var_dayd$, ~
                     mt_hours$, mt_min$
               lcntr% = lcntr% + 2%
        return

        print_detail_2
            if lcntr% > 57% then gosub print_header
               if sav_dept$ <> " " then goto L60375
                  goto L60385
L60375:        if sav_dept$ = mt_dept$ then goto L60395
                  gosub print_header
L60385:           sav_dept$ = mt_dept$

L60395:        print using L55320
/* (AWD028) */
               print using L55290, mt_dept$, mt_dept_d$, mt_emp$, name$,  ~
                     e_vac_days, e_vac_days$, e_vac_used, e_vac_used$, ~
                     e_sick_days, e_sick_days$, e_sick_used,            ~
                     e_sick_used$, e_pts_late, e_pts_mis, e_pts_day,     ~
                     e_pts_tot, e_pts_dte$
               lcntr% = lcntr% + 2%


        return

        print_detail_3
            if lcntr% > 57% then gosub print_header
               print using L55410
               print using L55380, mt_emp$, name$, dd$(1), e_reg$, e_ovr$,~
                                  e_sick$, e_vac$, e_hol$, e_tot$
               lcntr% = lcntr% + 2%
               if dt_max% = 1% then return
                  for i% = 1% to dt_max%
                     if lcntr% > 57% then gosub print_header
                        convert dr(8%,i%) to dr$, pic(######.##-)
                        convert dov(8%,i%) to dov$, pic(######.##-)
                        print using L55410
                        print using L55380, " ", " ", dd$(i%), dr$, dov$, ~
                                  " ", " ", " ", " "
                        lcntr% = lcntr% + 2%
                  next i%
        return

        print_detail_4
            if lcntr% > 57% then gosub print_header
               print using L55500
               if i% <> 1% then goto L60575
                  print using L55470, mt_emp$, name$, dt_wk$, dt_day$,    ~
                        c_t1$(i%), c_t1$(i% + 1%), c_th$(k%), c_tm$(k%), ~
                        time_txt$ , dp$(k%), status$(k%), status1$(k%),  ~
                        l_usr$(k%)
                  goto L60595
L60575:           print using L55470, " "    , " "  , " "   , " "    ,    ~
                        c_t1$(i%), c_t1$(i% + 1%), c_th$(k%), c_tm$(k%), ~
                        " "       , dp$(k%), status$(k%), status1$(k%),  ~
                        l_usr$(k%)
L60595:        lcntr% = lcntr% + 2%
        return

        print_detail_5
            if lcntr% > 57% then gosub print_header
               print using L55570
               lcntr% = lcntr% + 1%
               max% = in%
               if max% < out% then max% = out%
               for i% = 1% to max%  /* In Case of Mult In/Outs for Day */
                   print using L55550, mt_emp$, name$, mt_day$, in$(i%),  ~
                                    out$(i%), lunch$, mt_hours$, mt_min$,~
                                    mt_var_day$, mt_var_dayd$, mt_proc$, ~
                                    mt_procd$, mt_user$, mt_name$
                   lcntr% = lcntr% + 1%
                   if i% <> 1% then goto L60690
                      init(" ") mt_emp$, name$, lunch$, mt_hours$,       ~
                                mt_min$, mt_var_day$, mt_var_dayd$,      ~
                                mt_proc$, mt_procd$, mt_user$, mt_name$
L60690:        next i%
        in% = 0% /* <AWD018> */
        return

        scan_data
           count% = 0% : dept% = 0% : end_of_file% = 0%
           mt_key$ = all(hex(00))
        check_dept
           if end_of_file% = 1% and str(sc_dept$,1%,1%) <> "A" then      ~
                                                           goto scan_done
           if str(sc_dept$,1%,1%) <> "A" then goto L60760
              dept% = dept% + 1%                   /* Get Next Depart  */
              if dept% > dept_max% then goto scan_done
                 mt_dept$ = dept$(dept%)
                 goto L60770
L60760:    mt_dept$ = str(sc_dept$,1%,3%)            /* Set From Input */

L60770:    sav_dept$ = mt_dept$                    /* Save Department  */
           call "SHOSTAT" ("Checking Entries for Department ("&sav_dept$&~
                                     ")" )
           str(mt_key$,1%,3%) = mt_dept$           /* Set Department   */
           convert mt_yr$ to temp%, data goto L60771
L60771:    mt_yr_bi$ = bin(temp%, 2)
           str(mt_key$,4%,2%) = mt_yr_bi$           /* Set Year         */
           if sc_emp$ <> "ALL" then str(mt_key$,6%,5%) = sc_emp$         ~
                               else str(mt_key$,6%,8%) = " "
           init(" ") save_key$
        check_emp                                  /* (APCEMPMT) File  */
           str(mt_key$,11%,3%)  = " "
           read #3,key > mt_key$, using L60825, mt_key$, eod goto scan_end
L60825:       FMT CH(13)
           if str(mt_key$,4%,2%) <> mt_yr_bi$ then scan_end
           if str(sc_dept$,1%,1%) = "A" then goto L60855
              if str(sc_dept$,1%,3%) <> str(mt_key$,1%,3%) then          ~
                                                           goto scan_done
                 goto L60865
L60855:    if sav_dept$ <> str(mt_key$,1%,3%) then goto scan_end

L60865:    str(save_key$,1%,10%) = str(mt_key$,1%,10%)
           str(mt_key$,11%,3%)  = " "
           if sc_wk$ <> "AA" then str(mt_key$,11%,2%) = sc_wk$
        check_emp_next
           read #3,key > mt_key$, using L60900, mt_key$, mt_var_in$,      ~
                                  mt_var_out$, mt_var_day$, mt_proc$,    ~
                                  mt_user$, eod goto scan_end
L60900:        FMT CH(13), POS(24), 4*CH(1), CH(3)
           if str(save_key$,1%,10%) <> str(mt_key$,1%,10%) then            ~
                                                           goto check_emp
           if sc_wk$ = "AA" then goto L60925
              if sc_wk$ <> str(mt_key$,11%,2%) then goto check_emp_next
L60925:    if sc_day$ = "A" then goto L60935
              if sc_day$ <> str(mt_key$,13%,1%) then goto check_emp_next
L60935:    if sc_emp$ = "ALL" then goto L60950
              if sc_emp$ <> str(mt_key$,6%,5%) then goto scan_end

L60950:    if sel$ <> "4" then goto L60995
              code$ = mt_var_day$ : gosub lookup_variance
              mt_var_dayd$ = desc$
              code$ = mt_proc$    : gosub lookup_variance
              mt_procd$ = desc$
              gosub lookup_user
              gosub employee_detail
              goto check_emp_next

L60995:    gosub check_data
           goto check_emp_next

        scan_done
           if sel$ = "4" then return
              str(var_check$,1%,2%) = sc_wk$
              if count% = 0% then str(var_check$,4%,2%) = "OK"
              call "SHOSTAT" ("Open Var. Status for Week - "&var_check$ )
              call "PAUSE" addr(300%)
        return

        scan_end
           end_of_file% = 1%
           goto check_dept

        check_data
            p% = pos("123456789" = mt_var_day$)         /* SET DAY VAR */
REM         p1% = pos("ABCDEFGHIJKLMNOPQRSTUVWXYZ" = mt_proc$)
REM         p1% = pos("ACDEFGHIJKLMNOPQRS@TUVWXYZ!#$%&@^~" = mt_proc$)
REM         p1% = pos("ACDEFGHIJKLMNOPQRSTUVWXYZ!#$%&^~B@" = mt_proc$)
/* (AWD025) - ADD +  (AWD033) Added {} */
/*          p1% = pos("ACDEFGHIJKLMNOPQRSTUVWXYZ!#$%&^~@()-*B^+{}" = mt_proc$)*/
/* SR71317  - ADD =; */
/*          p1% = pos("ACDEFGHIJKLMNOPQRSTUVWXYZ!#$%&^~@()-*B^+{}=;" = mt_proc$)*/
/* SR71860  - ADD [  */
            p1% = pos("ACDEFGHIJKLMNOPQRSTUVWXYZ!#$%&^~@()-*B^+{}=;[" = mt_proc$)
            if p1% <> 0% then return                         /* CLOSED */
            if p% = 0% then return                      /* NO VARIANCE */
               gosub dataload
               gosub load_employee
               gosub load_variance
               gosub print_detail_1
               count% = count% + 1%
        return

        load_employee
            init(" ") name$ : name% = 0%
            read #1,key = mt_emp$, using L61150, e_lname$, e_fname$,      ~
                                     e_init$, py_corp_dept$, eod goto L61175
L61150:        FMT POS(12), CH(15), CH(10), CH(01), POS(744), CH(04)
            name$ = e_lname$ & "," & str(e_fname$,1%,1%) & "." &         ~
                                                       str(e_init$) & "."
            name% = 1%
        return
L61175:     e_lname$ = "(Error) - Employee"
        return

        load_variance                                  /* IN, OUT, DAY */
            init(" ") readkey$, mt_var_ind$, mt_var_outd$, mt_var_dayd$, ~
                      mt_procd$
            code$ = mt_var_in$ : gosub lookup_variance
            mt_var_ind$  = desc$
            code$ = mt_var_out$ : gosub lookup_variance
            mt_var_outd$ = desc$
            code$ = mt_var_day$ : gosub lookup_variance
            mt_var_dayd$ = desc$
            code$ = mt_proc$   : gosub lookup_variance
            mt_procd$    = desc$
        return

        vacation_points
           call "SHOSTAT" ("Creating Employee VAC/SICK/PTS Report")
           e_key$ = all(hex(00)) : sav_dept$, save_dept$ = " "
           if str(sc_dept$,1%,1%) <> "A" then                            ~
                                  str(e_key$,1%,3%) = str(sc_dept$,1%,3%)
        vacation_next
           read #1,key 1% > e_key$, using L61295, e_key$, e_status$,      ~
                                                 eod goto vacation_done
L61295:       FMT CH(11), POS(152), CH(1)
           if e_status$ <> "A" then goto vacation_next
           e_payg$ = str(e_key$,4%,3%)
           if e_payg$ = "180" or e_payg$ = "190" or e_payg$ = "200"      ~
                                               then goto vacation_next
           if str(sc_dept$,1%,1%) = "A" then goto L61340
              if str(sc_dept$,1%,3%) <> str(e_key$,1%,3%) then           ~
                                                    goto vacation_next

L61340:    if sc_emp$ = "ALL" then goto L61355
              if sc_emp$ <> str(e_key$,7%,5%) then goto vacation_next

L61355:    get #1, using L61380, e_vac_days%, e_vac_days$, e_vac_used%,   ~
                                e_vac_used$, e_sick_days%, e_sick_days$, ~
                                e_sick_used%, e_sick_used$, e_pts_late,  ~
                                e_pts_mis, e_pts_day, e_pts_tot,         ~
                                e_pts_dte$
L61380:      FMT POS(218), BI(2), CH(6), BI(2), CH(6), BI(2), CH(6),     ~
                           BI(2), CH(6), POS(284), 4*PD(14,4), CH(6)

             e_sick_used = 0.0                         /*  (EWD004)  */
             e_sick_used = (e_sick_used% / 10)         /*  (EWD004)  */

             e_vac_used = 0.0
             e_vac_used = (e_vac_used% / 10)         /*  (AWD010)  */

             e_vac_days = 0.0
             e_vac_days = (e_vac_days% / 10)         /*  (AWD028)  */

             e_sick_days = 0.0
             e_sick_days = (e_sick_days% / 10)         /*  (AWD028)  */


             call "DATEFMT" (e_vac_days$)
             call "DATEFMT" (e_vac_used$)
             call "DATEFMT" (e_sick_days$)
             call "DATEFMT" (e_sick_used$)
             call "DATEFMT" (e_pts_dte$)

             if save_dept$ = str(e_key$,1%,3%) then goto L61470
                save_dept$ = str(e_key$,1%,3%)
                mt_dept$ = save_dept$
                mt_dept_d$ = " "
                readkey$ = all(hex(00))
                str(readkey$,1%,9%)   = "EMP DEPT "
                str(readkey$,10%,15%) =  mt_dept$
                read #2,key = readkey$, using L61465, mt_dept_d$,         ~
                                                           eod goto L61470
L61465:            FMT POS(25), CH(30)
L61470:      mt_emp$ = str(e_key$,7%,5%)
             gosub load_employee
             gosub print_detail_2
             goto vacation_next
        vacation_done
        return

        weekly_hours
           call "SHOSTAT" ("Creating Employee Weekly Hours Report")
           gosub OPEN_FILE
           
/* (AWD025) */ float_hol% = 0%
           f% = 7%                                   /* (EWD002)   */
REM IF SEL$ = "6" THEN F% = 8%                /* (EWD002)   */
                                                     /* (EWD005)   */
           w_tot, w_reg, w_ovr, w_sick, w_vac, w_hol, e_pay_rate = 0.0
           e_no$ = all(hex(00))
        weekly_hrs_next                                 /*  (EWD005)  */
           read #1,key > e_no$, using L61540, e_dept$, e_payg$, e_no$,    ~
                                     e_add1$, e_status$, e_pay_rate,      ~
                                     py_corp_dept$, e_shft$, eod goto weekly_done
L61540:       FMT CH(03), CH(03), CH(05), POS(38), CH(30), POS(152), CH(1), ~
                  POS(250), PD(14,4), POS(744), CH(04), POS(841), CH(2)

           if sel$ <> "3" then goto L61565
/* <AWD022> */
/* (AWD026) */
           if e_dept$ = "057" and schema% <> 2% then goto weekly_hrs_next
           read #9,key = e_no$, using L61542, pay_stat$, eod goto L61542
           if pay_stat$ = "E" then goto weekly_hrs_next
           if pay_stat$ = "N" then goto weekly_hrs_next /* remove when ready*/
/* IF E_DEPT$ <> "87" AND E_DEPT$<> "99" AND E_DEPT$ <> "31"  AWD032  ~
      AND PAY_STAT$ = "N" THEN GOTO WEEKLY_HRS_NEXT   NOT READY FOR THIS YET*/
/*AWD032   IF E_DEPT$ <> "99" THEN GOTO L61542                NOT READY YET */
/*AWD032   IF E_NO$ <> "01093" AND E_NO$ <> "01490"                   ~
                                    THEN GOTO WEEKLY_HRS_NEXT  NOT READY YET*/
                                    
L61542:    FMT POS(28), CH(01)
/* </AWD022> */
                                                         /* Skip Temps */
              if str(e_no$,1%,1%) = "A" then goto weekly_hrs_next
/* (AWD026) */
              if schema% = 2% then goto L61590
              if e_dept$ = "060" or e_dept$ = "056" then goto weekly_hrs_next 
              if e_dept$ = "069" then goto weekly_hrs_next /* (AWD021) */
              if e_dept$ = "057" then goto weekly_hrs_next   /* (AWD023) */
              goto L61590
L61565:    if sel$ <> "6" then goto L61570                  /* EWD006  */
              if str(e_no$,1%,1%) <> "A" then goto weekly_hrs_next
/* (AWD026) */
              if schema% = 2% then goto L61590
              if e_dept$ = "060" or e_dept$ = "056" then goto weekly_hrs_next
              if e_dept$ = "069" then goto weekly_hrs_next   /* (AWD021) */
              if e_dept$ = "057" then goto weekly_hrs_next   /* (AWD021) */
              goto L61590

L61570:    if sel$ <> "7" then goto L61575         /* (AWD021)*/
/* (AWD026) */
              if schema% = 2% then goto L61590
              if e_dept$ <> "060" and e_dept$ <> "056" then goto weekly_hrs_next
              goto L61590
                                                            /* EWD006  */
L61575:    if sel$ <> "8" then goto L61590              /* (AWD021) */
/* (AWD026) */
              if schema% = 2% then goto L61590
              if e_dept$ = "057" then goto L61590
              if e_dept$ <> "069" then goto weekly_hrs_next
              goto L61590                               /* (/AWD021) */

L61590:    if e_status$ <> "A" then goto weekly_hrs_next
           if e_payg$ = "180" or e_payg$ = "190" or e_payg$ = "200" then ~
                                                     goto weekly_hrs_next
           if sc_emp$ = "ALL" then goto L61605
              if sc_emp$ <> e_no$ then goto weekly_hrs_next

L61605:    init(" ") dd$()                        /* STORE DEPARTMENTS */
           mat dr = zer : mat dov = zer           /* REG HRS, OVERTIME */
           dt_max% = 1%                           /* BEG WITH PARENT   */
           dd$(1%) = e_dept$                      /* STORE PARENT DEPT */

           convert mt_yr$ to temp%, data goto L61655
L61655:    mt_yr_bi$ = bin(temp%, 2)
           mt_key$ = all(hex(00))
           str(mt_key$,1%,2%) = mt_yr_bi$
           str(mt_key$,3%,5%) = e_no$
           str(mt_key$,8%,2%) = sc_wk$
           str(mt_key$,10%,1%)= " "
           sav_key$ = mt_key$
           e_time, e_reg, e_ovr, e_sick, e_sick_p, e_vac, e_hol, e_fol = 0.0
/* <AWD014> */
           e_jury  = 0.00
           e_sum   = 0.00
           e_death = 0.00
           init(" ") e_jury$, e_death$
/* </AWD014> */
           init(" ") e_reg$, e_ovr$, e_sick$, e_vac$, e_hol$, e_tot$
           mt_emp$ = e_no$
           read #3,key 1% > mt_key$, using L61685, mt_dept$, mt_key$,     ~
                                           mt_hours%, mt_min%, mt_proc$, ~
                                                    eod goto weekly_total
L61685:         FMT CH(3), CH(10), POS(20), 2*BI(2), POS(27), CH(1)
           goto L61710
        weekly_next
          read #3, using L61685, mt_dept$, mt_key$, mt_hours%, mt_min%,   ~
                                          mt_proc$, eod goto weekly_total
L61710:   if sav_key$ <> str(mt_key$,1%,09%) then goto weekly_total

REM P% = POS("ACLNPXZ" = MT_PROC$)     /*  (EWD008)           */
REM P% = POS("ACLNPXZMGIK" = MT_PROC$)     /*  (AWD015)(AWD016)   */
REM P% = POS("ACLNPXZM" = MT_PROC$)     /*  (AWD015)(AWD016)   */
REM P% = POS("ACLNPRXZM$&^@" = MT_PROC$) /*  (AWD017)   */
REM P% = POS("ACLNPRXZM$&^@CZ" = MT_PROC$) /*  (AWD019)   */
REM P% = POS("ABCLNPRXZM$&^@()ST" = MT_PROC$) /*  (AWD020)   */
REM P% = POS("ABCLNPRXZM$&^@()ST=;" = MT_PROC$) /*  SR71317    */
REM P% = POS("ABCLNPRXZM$&^@()=;" = MT_PROC$) /*  SR71639A   */
             p% = pos("ABCLNPRXZM$&^@()=;[" = mt_proc$) /*  SR71860    */
             if p% <> 0% then goto weekly_next  /* DON'T COUNT FOR PAY */

             gosub calc_indexs
             x = (mt_hours% * 60.0) + mt_min%            /*  (EWD004)  */
/* <AWD014> */
/* <AWD015> */
             if mt_proc$ <> "M" and mt_proc$ <> "Q" then goto L61715
             if mt_proc$ = "M" then  ~
             e_vac  = round(e_vac  + 720.0, 2)
             if mt_proc$ = "Q" then  ~
             e_vac  = round(e_vac  + 480.0, 2)
             goto L61810

L61715:
             if mt_proc$ <> "J" then goto L61720
             e_jury  = round(e_jury  + x, 2)
             goto L61810

L61720:
             if mt_proc$ <> "D" then goto L61730
             e_death  = round(e_death  + x, 2)
             goto L61810

L61730:
/* </AWD014> */
              if mt_proc$ <> "S" and mt_proc$ <> "T" then goto L61759
                if mt_proc$ = "S" then e_sick = round(e_sick + x, 2)
                if mt_proc$ <> "T" then goto L61810
                   e_sick = round(e_sick + 240.0, 2)
                   x = (x - 240.0)
/*AWD031           e_time = round(e_time + x, 2)    */
/*AWD031        goto L61810    */
/*AWD031*/      goto L61795

/* <AWD033> + */
L61759:       if mt_proc$ <> "{" and mt_proc$ <> "}" then goto L61760
                if mt_proc$ = "{" then e_sick_p = round(e_sick_p + x, 2)
                if mt_proc$ <> "}" then goto L61810
                   e_sick_p = round(e_sick_p + 240.0, 2)
/*AWD033*/         x = (x - 240.0)                         
/*AWD031           e_time = round(e_time + x, 2)    */
/*AWD031        goto L61810    */
/*AWD031*/      goto L61795
/* <AWD033> - */
L61760:      if mt_proc$ <> "V" and mt_proc$ <> "W" then goto L61775
                                                        /*  (AWD012)  */
                e_vac  = round(e_vac  + x, 2)
                goto L61810
L61775:      if mt_proc$ <> "U" then goto L61785        /*  (AWD010)  */
REM E_VAC  = ROUND(E_VAC  + X, 2)
                e_vac = round(e_vac + 240.0 ,2)
                x = (x - 240.0)                      /* Note 240 = 4 hours */
/*AWD031        e_time = round(e_time + x, 2)           for 1/2 days       */
/*AWD031        goto L61810    */
/*AWD031*/      goto L61795

L61785: 
REM  IF MT_PROC$ <> "H" THEN GOTO L61790
/* (AWD025) */
             if mt_proc$ <> "H" and mt_proc$ <> "+" then goto L61790
/* (AWD025) */  if mt_proc$ = "+" then gosub write_temp_file
/*SR71639*/     if mt_proc$ = "+" then e_fol  = round(e_fol  + x, 2)
                if mt_proc$ = "H" then e_hol  = round(e_hol  + x, 2)
REM DR(DD%,DT%) = ROUND(DR(DD%,DT%) + X, 2)
                goto L61810
L61790:      if mt_proc$ <> "I" then goto L61795
                e_vac  = round(e_vac  + x, 2)
                goto L61810
L61795:      e_time = round(e_time + x, 2)
             dr(dd%,dt%) = round(dr(dd%,dt%) + x, 2)

L61810:      goto weekly_next
        weekly_total
             gosub load_employee
             reg = 40.0                  /*  (EWD006)  */
/* (AWD026) */
             if schema% = 2% then normal_hrs

             if str(e_dept$,1%,3%) = "060" and str(e_shft$,1%,2%) = "11"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "060" and str(e_shft$,1%,2%) = "12"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "060" and str(e_shft$,1%,2%) = "13"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "060" and str(e_shft$,1%,2%) = "14"   ~
                                 then reg = 36.0
                                               /*  (EWD009)  -- Beg */
             if str(e_dept$,1%,3%) = "056" and str(e_shft$,1%,2%) = "11"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "056" and str(e_shft$,1%,2%) = "12"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "056" and str(e_shft$,1%,2%) = "13"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "056" and str(e_shft$,1%,2%) = "14"   ~
                                 then reg = 36.0
                                               /*  (EWD009)  -- END */
                                               /*  (AWD021)  -- Beg */
             if str(e_dept$,1%,3%) = "069" and str(e_shft$,1%,2%) = "11"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "069" and str(e_shft$,1%,2%) = "12"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "069" and str(e_shft$,1%,2%) = "13"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "069" and str(e_shft$,1%,2%) = "14"   ~
                                 then reg = 36.0
                                               /*  (AWD021)  -- END */
                                               /*  (AWD021)  -- Beg */
             if str(e_dept$,1%,3%) = "057" and str(e_shft$,1%,2%) = "11"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "057" and str(e_shft$,1%,2%) = "12"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "057" and str(e_shft$,1%,2%) = "13"   ~
                                 then reg = 36.0
             if str(e_dept$,1%,3%) = "057" and str(e_shft$,1%,2%) = "14"   ~
                                 then reg = 36.0
                                               /*  (AWD021)  -- END */
normal_hrs:
                                         /* CONVERT TOTAL MIN TO HOURS */
                                         /* (EWD003) - Take out sick time */

REM X = ROUND((E_TIME + E_SICK + E_VAC + E_HOL) / 60.0, 2)
REM Y = ROUND(X - ((E_SICK + E_VAC) / 60.0), 2)
/* <AWD014> */
REM X = ROUND((E_TIME + E_VAC + E_HOL) / 60.0, 2)
REM Y = ROUND(X - ((E_VAC) / 60.0), 2)
/*AWD033 X = ROUND((E_TIME + E_VAC + E_HOL + E_JURY + E_DEATH) / 60.0, 2)*/
/*AWD033 Y = ROUND(X - ((E_VAC + E_HOL + E_JURY + E_DEATH) / 60.0), 2) */
/*AWD033 X = ROUND((E_TIME + E_VAC + E_HOL + E_JURY + E_DEATH + E_SICK_P) / 60.0, 2)*/
/*AWD033 Y = ROUND(X - ((E_VAC + E_HOL + E_JURY + E_DEATH + E_SICK_P) / 60.0), 2)*/

/*SR71639 */  x = round((e_time + e_vac + e_hol + e_fol + e_jury + e_death    ~
                                                + e_sick_p) / 60.0, 2)
/*SR71639 */  y = round(x - ((e_vac + e_hol + e_fol + e_jury + e_death +      ~
                                              e_sick_p) / 60.0), 2)
              e_jury = round(e_jury / 60.0, 2)
              e_death = round(e_death / 60.0, 2)
/* </AWD014> */
              e_time = round(e_time / 60.0, 2)
              e_sick = round(e_sick / 60.0, 2)    
/*AWD033 */   e_sick_p = round(e_sick_p / 60.0, 2)
              e_vac  = round(e_vac  / 60.0, 2)
              e_hol  = round(e_hol  / 60.0, 2)
/*SR71639 */  e_fol  = round(e_fol  / 60.0, 2)
              if y > reg then goto L61885
                 e_reg = e_time
                 e_ovr = 0.0
                 goto L61900
L61885:       e_ovr = y - reg
              e_reg = e_time - e_ovr

L61900:    convert e_reg to  e_reg$,  pic(######.##-)
           convert e_ovr to  e_ovr$,  pic(######.##-)
           convert e_sick to e_sick$, pic(######.##-)
           convert e_vac to  e_vac$,  pic(######.##-)
/* <AWD014> need to show jury & family death under holiday column */
/*AWD033 E_SUM = E_HOL + E_JURY + E_DEATH  */
/*AWD033 E_SUM = E_HOL + E_JURY + E_DEATH + E_SICK_P  */
/*SR71639*/e_sum = e_hol + e_jury + e_death + e_sick_p + e_fol
REM  CONVERT E_HOL TO  E_HOL$,  PIC(######.##-)

           convert e_sum to  e_hol$,  pic(######.##-)
           convert x to  e_tot$,  pic(######.##-)    /* <AWD014> */

           convert e_death to  e_death$,  pic(######.##-)
           convert e_jury  to  e_jury$,   pic(######.##-)
           w_jury  = round(w_jury  + e_jury,  2)
           w_death = round(w_death + e_death, 2)
           w_reg = round(w_reg + e_reg, 2)
           w_ovr = round(w_ovr + e_ovr, 2)
           w_sick= round(w_sick + e_sick, 2)
           w_vac = round(w_vac + e_vac, 2)
/*AWD033  W_HOL = ROUND(W_HOL + E_HOL, 2)     */
/*AWD033*/ w_hol = round(w_hol + e_sum, 2)
           w_tot = round(w_tot + x, 2)

           gosub calc_time

           gosub print_detail_3
           if sel$ = "3" then gosub write_payroll    /* (EWD001)   */
           if sel$ = "6" then gosub write_payroll    /* (EWD002)   */
           if sel$ = "7" then gosub write_payroll    /* (EWD006)   */
           if sel$ = "8" then gosub write_payroll    /* (EWD006)   */
           goto weekly_hrs_next

        weekly_done
           convert w_reg to  w_reg$,  pic(######.##-)
           convert w_ovr to  w_ovr$,  pic(######.##-)
           convert w_sick to w_sick$, pic(######.##-)
           convert w_vac to  w_vac$,  pic(######.##-)
/* <AWD014> need to show jury & family death under holiday column */
/* W_SUM = W_HOL + W_JURY + W_DEATH                       */ 
/*SR71639a*/w_sum = w_hol                          
           convert w_sum to  w_hol$,  pic(######.##-)
REM CONVERT W_HOL TO  W_HOL$,  PIC(######.##-)
/* <AWD014> */
           convert w_tot to  w_tot$,  pic(######.##-)
/* <AWD014> */
           convert w_death to  w_death$,  pic(######.##-)
           convert w_jury  to  w_jury$,   pic(######.##-)
/* </AWD014> */
        print using L55410
        print using L55380, "*****", " Weekly Total Hours ", "***",        ~
                    w_reg$, w_ovr$, w_sick$, w_vac$, w_hol$, w_tot$
        return

/* (EWD001) - Begin  */
        write_payroll   /* Write EWDPYROL to import into excel for Atrium   */
          init(" ") py_no$, py_dept$, py_name$, py_code$
/* (AWD021) */
          py_code$ = "N7M"
          if dd$(1) = "060" then py_code$ = "N7L"
          if dd$(1) = "056" then py_code$ = "N7L"
          if dd$(1) = "057" then py_code$ = "N7L"
          if dd$(1) = "069" then py_code$ = "N7L"
/* (/AWD021) */
/* (AWD026) */
          if schema% = 2% then py_code$ = "KEB"
          py_reg_hrs, py_ovr_hrs, py_vac_hrs, py_hol_hrs, py_tot_hrs = 0.0
/*AWD033*/py_jur_hrs, py_dth_hrs, py_sic_hrs, py_fol_hrs = 0.0  /*SR71639 */
          init(" ") py_reg_hrs$, py_ovr_hrs$, py_vac_hrs$, py_tot_hrs$,   ~
                    py_pay_rate$
          py_no$     = "0" & e_no$
          py_dept$   = "000" & dd$(1)
          py_name$   = name$
          if dt_max% <> 1% then goto L61910
          py_reg_hrs = e_reg
          py_ovr_hrs = e_ovr
          py_vac_hrs = e_vac
/* <AWD014> */
REM       PY_HOL_HRS = E_HOL
/*AWD033  py_hol_hrs = e_sum */
/*AWD033+*/py_hol_hrs = e_hol
/*SR71639*/py_fol_hrs = e_fol
           py_jur_hrs = e_jury
           py_dth_hrs = e_death
/*AWD033-*/py_sic_hrs = e_sick_p
/* </AWD014> */
          py_tot_hrs = x

          gosub write_data
          gosub write_new_data

          return
L61910:   for i% = 1% to dt_max%
                 init(" ") py_dept$
                 py_dept$ = "000" & dd$(i%)
                 py_reg_hrs, py_ovr_hrs, py_vac_hrs, py_hol_hrs, ~
                 py_tot_hrs = 0.0
                 py_jur_hrs, py_dth_hrs, py_sic_hrs, py_fol_hrs = 0.0  /*SR71639 */
                 py_reg_hrs = dr(8%,i%)
                 py_ovr_hrs = dov(8%,i%)
/*SR71972        PY_TOT_HRS = X     PWWW  SOMETHING WRONG HERE!!!            */
                 py_tot_hrs = py_reg_hrs + py_ovr_hrs             
/* <AWD014> */
REM              if i% = 1% then py_hol_hrs = e_hol
                 if i% <> 1% then goto skip_first_dept
                    py_vac_hrs = e_vac
                    py_hol_hrs = e_hol
/*SR71639 */        py_fol_hrs = e_fol
                    py_jur_hrs = e_jury
                    py_dth_hrs = e_death
                    py_sic_hrs = e_sick_p
/*SR71972 */        py_tot_hrs = py_reg_hrs + py_ovr_hrs + py_vac_hrs +       ~
                      py_hol_hrs + py_fol_hrs + py_jur_hrs + py_dth_hrs +     ~
                      py_sic_hrs
                    
            skip_first_dept
/* <AWD014> */
                 gosub write_data
                 gosub write_new_data           /*  (AWD011)  */

             next i%
        return

        write_data                  /* (EWD002) - Change to 'f%'  */
/*
  IF PY_DEPT$ = "000020" THEN RETURN
  IF PY_DEPT$ = "000055" OR PY_DEPT$ = "000017" THEN RETURN
  IF PY_DEPT$ = "000018" OR PY_DEPT$ = "000019" THEN RETURN
*/
* AWD013 take out next two lines
REM IF PY_DEPT$ = "000020" OR PY_DEPT$ = "000055" THEN RETURN
REM IF PY_DEPT$ = "000019" THEN RETURN
          init(" ") py_key$
          str(py_key$,1%,6%) = py_no$
          str(py_key$,6%,6%) = py_dept$
          read #f%,hold,key = py_key$, eod goto write_data_done
                  delete #f%

        write_data_done
REM IF F% = 8% THEN GOTO WRITE_TMP          /*   (EWD005)   */
/* write new file w/ pay rate here */
          str(des_key$,01,6) = py_no$
          str(des_key$,07,6) = py_dept$
          str(des_key$,13,4) = sc_yr$
          str(des_key$,19,3) = py_code$
          str(des_key$,22,1) = py_batch$
          des_wk$ = sc_wk$
          convert sc_wk$ to des_wk, data goto cnv_skip
cnv_skip: convert des_wk to des_wk$, pic (00)
          str(des_key$,17,2) = des_wk$
          read #10, key = des_key$, hold, eod goto cnv_error
          delete #10

cnv_error: write #10%, using XYZ,                                          ~
                                py_no$,         /*  Employee Number      */~
                                py_dept$,       /*  Employee Department  */~
                                sc_yr$,         /*  run year             */~
                                des_wk$,        /*  run week             */~
                                py_code$,       /*  Company Code         */~
                                py_batch$,      /*  Company Batch ID     */~
                                py_reg_hrs,     /*  Emp. Regular Hours   */~
                                py_ovr_hrs,     /*  Emp. Overtime Hours  */~
                                py_vac_hrs,     /*  Emp. Vacation Hours  */~
                                py_hol_hrs,     /*  Emp. Holiday Hours   */~
/*AWD033                        py_jur_hrs,         Emp. Jury    Hours   */~
/*AWD033                        py_dth_hrs,         Emp. Death   Hours   */~
/*AWD033                        py_sic_hrs,         Emp. Sick    Hours   */~
                                py_tot_hrs,     /*  Emp. Total Hours     */~
                                e_pay_rate,     /*  Emp. Base Pay Rate   */~
                                "          "    /*  Filler               */
XYZ:    FMT CH(6), CH(6), CH(4), CH(2), CH(3), CH(1), 6*PD(14,4), CH(10)

          put #f%, using L55400, py_code$,       /*  Company Code         */~
                                py_batch$,      /*  Company Batch ID     */~
                                py_no$,         /*  Employee Number      */~
                                py_dept$,       /*  Employee Department  */~
                                py_name$,       /*  Emp. Last First MI   */~
                                py_reg_hrs,     /*  Emp. Regular Hours   */~
                                py_ovr_hrs,     /*  Emp. Overtime Hours  */~
                                py_vac_hrs,     /*  Emp. Vacation Hours  */~
                                py_hol_hrs,     /*  Emp. Holiday Hours   */~
/*AWD033                        py_jur_hrs,         Emp. Jury    Hours   */~
/*AWD033                        py_dth_hrs,         Emp. Death   Hours   */~
/*AWD033                        py_sic_hrs,         Emp. Sick    Hours   */~
                                py_tot_hrs,     /*  Emp. Total Hours     */~
                                e_add1$,        /*  Emp. Address 1       */~
                                e_pay_rate      /*  Emp. Base Pay Rate   */

L55400:       FMT  CH(03), CH(01), CH(06), CH(06), CH(20), 5*PD(14,4),    ~
                   CH(30), PD(14,4)
          write #f%, eod goto write_data_error
        return
REM     write_tmp                                /*   (EWD005)   */

          put #f%, using L55405, py_code$,       /*  Company Code         */~
                                py_batch$,      /*  Company Batch ID     */~
                                py_no$,         /*  Employee Number      */~
                                py_dept$,       /*  Employee Department  */~
                                py_name$,       /*  Emp. Last First MI   */~
                                py_reg_hrs,     /*  Emp. Regular Hours   */~
                                py_ovr_hrs,     /*  Emp. Overtime Hours  */~
                                py_vac_hrs,     /*  Emp. Vacation Hours  */~
                                py_hol_hrs,     /*  Emp. Holiday Hours   */~
                                py_tot_hrs,     /*  Emp. Total Hours     */~
                                e_add1$,        /*  Emp. Address 1       */~
                                e_pay_rate      /*  Emp. Base Pay Rate   */

L55405:       FMT  CH(03), CH(01), CH(06), CH(06), CH(20), 5*PD(14,4),    ~
                   CH(30), PD(14,4)

          write #f%, eod goto write_data_error
        return                                   /*   (EWD005)   */


        write_data_error
          errormsg$ = "EOD on EWDPYROLL " & py_key$
        return
/* (EWD001) - End */



        write_new_data         /* (AWD012) - New File, try to elimate easysoft */
* AWD013 take out next three lines
REM IF PY_DEPT$ = "000020" THEN RETURN
REM IF PY_DEPT$ = "000055" OR PY_DEPT$ = "000017" THEN RETURN
REM IF PY_DEPT$ = "000018" OR PY_DEPT$ = "000019" THEN RETURN

           convert py_reg_hrs to py_reg_hrs$, pic(#########.####)
           convert py_ovr_hrs to py_ovr_hrs$, pic(#########.####)
           convert py_vac_hrs to py_vac_hrs$, pic(#########.####)
           convert py_hol_hrs to py_hol_hrs$, pic(#########.####)
/*SR71639*/convert py_fol_hrs to py_fol_hrs$, pic(#########.####)
/*AWD033+*/convert py_jur_hrs to py_jur_hrs$, pic(#########.####)
           convert py_dth_hrs to py_dth_hrs$, pic(#########.####)
/*AWD033-*/convert py_sic_hrs to py_sic_hrs$, pic(#########.####)
           convert py_tot_hrs to py_tot_hrs$, pic(#########.####)
           convert e_pay_rate to py_pay_rate$, pic(#########.####)

          if write_cnt% > 0% then goto skip_header
          if schema% = 1% then gosub NC_HEADER ~
                          else gosub OTHER_HEADER
                         
          write #fx%, using L62015, py_rec$

skip_header:
          init(" ") py_key1$
          str(py_key1$,1%,6%) = py_no$
          str(py_key1$,7%,1%) = ","
          str(py_key1$,7%,6%) = py_dept$
          str(py_key1$,13%,1%) = ","
          read #fx%,hold,key = py_key1$, eod goto write_new_data_done
                  delete #fx%

write_new_data_done:

         write_cnt% = write_cnt% + 1%
        
         if schema% = 1% then gosub NC_RECORD ~
                         else gosub OTHER_RECORD       

          put #fx%, using L62015, py_rec$
L62015:   FMT CH(256)

REM PUT #8, USING L62010, PY_CODE$,       /*  COMPANY CODE         */~
    "|",            /* COMMA                 */~
    PY_BATCH$,      /*  COMPANY BATCH ID     */~
    "|",            /* COMMA                 */~
    PY_NO$,         /*  EMPLOYEE NUMBER      */~
    "|",            /* COMMA                 */~
    PY_DEPT$,       /*  EMPLOYEE DEPARTMENT  */~
    "|",            /* COMMA                 */~
    PY_NAME$,       /*  EMP. LAST FIRST MI   */~
    "|",            /* COMMA                 */~
    PY_REG_HRS$,    /*  EMP. REGULAR HOURS   */~
    "|",            /* COMMA                 */~
    PY_OVR_HRS$,    /*  EMP. OVERTIME HOURS  */~
    "|",            /* COMMA                 */~
    PY_VAC_HRS$,    /*  EMP. VACATION HOURS  */~
    "|",            /* COMMA                 */~
    PY_HOL_HRS$,    /*  EMP. HOLIDAY HOURS   */~
    "|",            /* COMMA                 */~
    PY_TOT_HRS$,    /*  EMP. TOTAL HOURS     */~
    "|",            /* COMMA                 */~
    E_ADD1$,        /*  EMP. ADDRESS 1       */~
    "|",            /* COMMA                 */~
    PY_PAY_RATE$,   /*  EMP. BASE PAY RATE   */~
    "|"             /* COMMA                 */

REM  L62010   FMT  CH(03), CH(1), CH(01), CH(1), CH(06), CH(1), CH(06), CH(1),~
     CH(20), CH(1), CH(14), CH(1), CH(14), CH(1), CH(14), CH(1),~
     CH(14), CH(1), CH(14), CH(1), CH(30), CH(1), CH(14), CH(1)
     
          write #fx%, eod goto write_new_data_error

        return
        
        NC_HEADER
         init(" ") py_rec$
         py_rec$ = "PY_CO,PY_BATCH"                                        & ~
                    ",PY_EMP_NO,PY_DEPT,PY_EMP_NAME,PY_REG_HRS,PY_OVR_HRS" & ~
                    ",PY_VAC_HRS,PY_HOL_HRS,PY_FOL_HRS,PY_JUR_HRS,PY_DTH_" & ~
                    "HRS,PY_SIC_HRS,PY_TOT_HRS,PAY_RATE,CORP_DEPT,"        
        return
        
        OTHER_HEADER
         init(" ") py_rec$
         py_rec$ = "PY_CO,PY_BATCH"                                        & ~
                    ",PY_EMP_NO,PY_DEPT,PY_EMP_NAME,PY_REG_HRS,PY_OVR_HRS" & ~
                    ",PY_VAC_HRS,PY_HOL_HRS,PY_FOL_HRS,PY_JUR_HRS,PY_DTH_" & ~
                    "HRS,PY_SIC_HRS,PY_TOT_HRS,PAY_RATE,"        
        return
        NC_RECORD
         init(" ") py_rec$
         py_rec$ =  py_code$ & "," & py_batch$ & "," & py_no$ & ~
               "," & py_dept$ & "," & quote$ & py_name$ & quote$ & "," &    ~
               py_reg_hrs$ & "," & py_ovr_hrs$ & "," & py_vac_hrs$ & "," & ~
/*AWD033*/     py_hol_hrs$ & "," & py_fol_hrs$ & "," & py_jur_hrs$ & "," & ~
               py_dth_hrs$ & "," & py_sic_hrs$ & "," & py_tot_hrs$ & "," & ~
               py_pay_rate$ & "," & py_corp_dept$ & ","        
        return
        OTHER_RECORD
         init(" ") py_rec$
         py_rec$ =  py_code$ & "," & py_batch$ & "," & py_no$ & ~
               "," & py_dept$ & "," & quote$ & py_name$ & quote$ & "," &    ~
               py_reg_hrs$ & "," & py_ovr_hrs$ & "," & py_vac_hrs$ & "," & ~
/*AWD033*/     py_hol_hrs$ & "," & py_fol_hrs$ & "," & py_jur_hrs$ & "," & ~
               py_dth_hrs$ & "," & py_sic_hrs$ & "," & py_tot_hrs$ & "," & ~
               py_pay_rate$ & ","         
        return


        write_new_data_error
          errormsg$ = "EOD on AWDPYROLL " & py_key1$
        return
/* (AWD012) - End */




        calc_indexs
                                         /* DD$( ) - DEPARTMENTS      */
                                         /* DR(Y,X)- REGULAR TIME     */
                                         /* DOV(Y,X)- OVER TIME       */
                                         /* X = DEPARTMENT (1) PARENT */
                                         /* Y = DAY OF THE WEEK       */
          dd% = 1%
          convert str(mt_key$,10%,1%) to dd%, data goto L62095
L62095:
          for i% = 1% to dt_max%
             if mt_dept$ = dd$(i%) then goto L62130
          next i%
          dt_max% = dt_max% + 1%
          i% = dt_max%
          dd$(i%) = mt_dept$
L62130:   dt% = i%
        return

        calc_time                          /* BREAK DOWN WEEKLY HOURS  */
          if dt_max% = 1% then goto L62225  /* REG AND OVERTIME BY DEPT */
          zz = e_reg : z1 = 0.0
          zz = 40.0 /* ???? */
          for i% = 1% to 7%                         /* PRODUCTION DAYS */
            for k% = 1% to dt_max%                  /* DEPARTMENTS     */
              if dr(i%,k%) = 0 then goto L62215
              yy = round( dr(i%,k%) / 60.0, 2)
              dr(8%,k%) = round(dr(8%,k%) + yy, 2)
              z1 = round(z1 + yy, 2)
              if z1 <= zz then goto L62215
                 b = (z1 - zz)
                 dr(8%,k%) = round(dr(8%,k%) - b, 2)       /* DEPT REG */
                 dov(8%,k%) = round(dov(8%,k%) + b, 2)       /*DEPT OVR*/
                 zz = z1
L62215:     next k%
          next i%
L62225: return

        total_day
           str(sav_key$,1%,2%) = str(mt_key$,4%,2%)   /* Production Yr */
           str(sav_key$,3%,2%) = str(mt_key$,11%,2%)  /* Production Wk */
           str(sav_key$,5%,1%) = str(mt_key$,13%,1%)  /* Production Dy */
           str(sav_key$,6%,5%) = str(mt_key$,6%,5%)   /* Employee No.  */
           init(" ") c_t$(), status$(), c_t1$(), c_th$(), c_tm$() ,dp$(),~
                     save_dept$, status1$(), l_usr$()
           mat c_th% = zer : mat c_tm% = zer
           tm, tm1, tm2 = 0.0
           j1%, j2% = 0%
           mt_hours%, mt_min% = 0%
           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           read #4,key > dt_key$, using  L62330, dt_key$, d_code$,        ~
                                             dt_usr$, eod goto total_done
           goto L62335
        total_next
           read #4,using  L62330, dt_key$, d_code$, dt_usr$,              ~
                                                   eod goto total_done
L62330:       FMT CH(18), CH(1), XX(2), CH(3)
L62335:    if sav_key$ <> str(dt_key$,1%,10%) then goto total_done

              save_dept$ = str(dt_key$,11%,3%)
              sgn% = 1%
              j1% = j1% + 1%
              c_t$(j1%) = str(dt_key$,14%,5%)
              if mod(j1%,2%) = 0 then goto L62455
                 j2% = j2% + 1%
REM IF J2% > 1% THEN GOTO 62420
                 if d_code$ = "2" then goto L62420
                    str(status$(j2%),1%,2%)   = mt_var_day$ & " "
                    str(status$(j2%),3%,12%)  = str(mt_var_dayd$,1%,12%)
                    str(status1$(j2%),1%,2%)  = mt_proc$ & " "
                    str(status1$(j2%),3%,12%) = str(mt_procd$,1%,12%)
                    str(l_usr$(j2%),1%,4%)    = mt_user$ & " "
                    str(l_usr$(j2%),5%,12%)   = str(mt_name$,1%,12%)
                    dp$(j2%) = save_dept$
                    goto L62455
L62420:          dp$(j2%) = save_dept$
                 status$(j2%)  = "   Normal     "
                 status1$(j2%) = "              "
                 mt_user$ = dt_usr$
                 gosub lookup_user
                 str(l_usr$(j2%),1%,4%)  = mt_user$ & "-"
                 str(l_usr$(j2%),5%,12%) = str(mt_name$,1%,12%)
L62455:       on pos("012" = d_code$) goto L62465, L62510, L62600
                                                          /* CLOCK IN  */
L62465:          convert str(c_t$(j1%),1%,2%) to hr1%, data goto L62470
L62470:
                 convert str(c_t$(j1%),3%,2%) to mn1%, data goto L62480
L62480:
                 if tm1 = 0.0 then goto L62495
                    status$(j2%) = " No Clock Out "
L62495:          tm1 = 60 * hr1% + mn1%         /* CLOCK IN MINUTES    */
                 goto total_next
                                                          /* CLOCK OUT */
L62510:          convert str(c_t$(j1%),1%,2%) to hr2%, data goto L62515
L62515:
                 if hr2% >= 24% then hr2% = hr2% - 24%
                 convert hr2% to str(c_t$(j1%),1%,2%), pic(00)

                 convert str(c_t$(j1%),3%,2%) to mn2%, data goto L62540
L62540:
                 tm2 = 60 * hr2% + mn2%         /* CLOCK OUT MINUTES   */
                 if tm1 > 0.0 then goto L62575
                    status$(j2%) = " No Clock In  "
                    tm = 0.0
                    goto L62660

L62575:          if tm2 <= tm1 then tm2 = tm2 + 1440   /* ADD 24 HOURS */
                 tm  = tm2 - tm1                /* TOTAL CLOCK MINUTES */
                 tm1, tm2 = 0.0
              goto L62660
                                                /* ADJUSTED TIME       */
L62600:       convert str(c_t$(j1%),2%,1%) to hr1%, data goto L62605
L62605:
              convert str(c_t$(j1%),3%,2%) to mn1%, data goto L62615
L62615:
              tm = (60*hr1%) + mn1%
              if str(c_t$(j1%),5%,1%) = "-" then sgn% = -1%
              status$(j2%)  = " Adjustment ++"
              status1$(j2%) = "              "
              if sgn% = -1% then status$(j2%) = " Adjustment --"
              j1% = j1% + 1%
              c_t$(j1%) = "     "

L62660:       c_th%(j2%) = sgn% * ( int(tm/60.0) )
              c_tm%(j2%) = sgn% * ( mod(tm,60.0) )
              mt_hours% = mt_hours% + c_th%(j2%)
              mt_min%   = mt_min%   + c_tm%(j2%)
              goto total_next
        total_done
          mt_hours% = mt_hours% + int(mt_min%/60.0)
          mt_min%   = mod(mt_min%,60.0)
          time_txt$ = "XX Hr XX Mn"
          convert mt_hours% to str(time_txt$,1%,2%), pic(##)
          convert mt_min% to str(time_txt$,7%,2%) , pic(##)
        return

        employee_detail
            gosub total_day
            if j2% = 0% then return
            for i% = 1% to j1%
              c_t1$(i%) = str(c_t$(i%),1%,2%) &":"& str(c_t$(i%),3%,2%)
              str(c_t1$(i%),7%,2%) = "AM"
              if str(c_t$(i%),1%,2%) >= "12" then                        ~
                                         str(c_t1$(i%),7%,2%) = "PM"
              if str(c_t$(i%),1%,1%) = "A" or str(c_t$(i%),1%,1%) = " "  ~
                                         then str(c_t1$(i%),7%,2%) = "AJ"
            next i%
            for i% = 1% to j2%
                convert c_th%(i%) to c_th$(i%), pic(##)
                convert c_tm%(i%) to c_tm$(i%), pic(##)
            next i%
           mt_emp$ = str(mt_key$,6%,5%)
           dt_wk$  = str(mt_key$,11%,2%)
           dt_day$ = str(mt_key$,13%,1%)

           gosub load_employee
           k% = 0%
           for i% = 1% to j1% step 2
             k% = k% + 1%
             gosub print_detail_4
           next i%

        return

        lookup_dept
            init(" ") readkey$, sc_dept_d$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = sc_dept$
            read #2,key = readkey$, using L62895, sc_dept_d$,             ~
                                                           eod goto L62905
L62895:        FMT POS(25), CH(30)
        return
L62905:     errormsg$ = "(Error) Department not on File?"
            sc_dept_d$ = errormsg$
        return

        load_security
            init(" ") readkey$, access$
            security% = 0%
            str(readkey$,1%,9%)   = "EMP SECUR"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L62955, access$, eod goto L62965
L62955:        FMT POS(25), CH(30)
            security% = 1%
L62965: return

        load_access                          /* Check Access to a Dept */
            init(" ") readkey$, access$      /* for a Specific Userid  */
            j% = 1%  : access% = 1%
            if security% = 1% then return    /* Automatically have     */
                                             /* Access to all Depts    */
            str(readkey$,1%,9%)   = "EMPACCESS"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L63020, access$,                ~
                                                   eod goto access_denied
L63020:        FMT POS(25), CH(30)
            for kk% = 1% to 10%
                convert str(access$,j%,3%) to x%, data goto access_denied

                if sc_dept% = x% then return
                   j% = j% + 3%
            next kk%
        access_denied
            comp% = 2% : access% = 0%
            hdr$ = "*** Access Denied to Dept ***"
            msg$(1) = "You Do Not Have Access to Dept/Employee Selected?"
            msg$(2) = "      D e p a r t m e n t   S e c u r i t y      "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        daily_log
           if dept% > dept_max% then goto daily_finished
           if str(sc_dept$,1%,1%) <> "A" then goto L63135
              dept% = dept% + 1%
              if dept% > dept_max% then goto daily_finished
                 mt_dept$ = dept$(dept%)
                 goto L63145
L63135:       mt_dept$ = str(sc_dept$,1%,3%)
              dept% = 99%                         /* Check Against the */
L63145:    e_key$ = all(hex(00))                  /* Employee Master   */
           str(e_key$,1%,3%) = mt_dept$           /* Set Dept Code     */
        daily_next                                /* File (APCEMPLY)   */
           read #1,key 1% > e_key$, using L63170, e_key$, e_status$,      ~
                                                 eod goto daily_log
L63170:       FMT CH(11), POS(152), CH(1)
           if e_status$ <> "A" then goto daily_next     /* Only Active */
           e_payg$ = str(e_key$,4%,3%)            /* Skip Salary Emp'S */
           if e_payg$ = "180" or e_payg$ = "190" or e_payg$ = "200"      ~
                                               then goto daily_next

           if mt_dept$ <> str(e_key$,1%,3%) then goto daily_next
           if str(sc_emp$,1%,3%) = "ALL" then goto L63220
              if sc_emp$ <> str(e_key$,7%,5%) then goto daily_next

L63220:       mt_emp$ = str(e_key$,7%,5%)
              in% = 0% : out% = 0% : max% = 0%
              init(" ") in$(), out$(), lunch$, sav_key$
              gosub load_employee
              dt_key$ = all(hex(00))
              convert mt_yr$ to temp%, data goto L63250
L63250:       mt_yr_bi$ = bin(temp%, 2)
              str(dt_key$,1%,2%) = mt_yr_bi$
              str(dt_key$,3%,2%) = sc_wk$
              if sc_day$ = "A" then str(dt_key$,5%,1%) = "1"             ~
                               else str(dt_key$,5%,1%) = sc_day$
              if str(sc_emp$,1%,3%) = "ALL"  then                        ~
                                             str(dt_key$,6%,5%) = mt_emp$~
                                    else str(dt_key$,6%,5%) = sc_emp$
              sav_key$ = str(dt_key$,1%,10%)
        daily_detail_nxt                    /* Search (APCEMPDT)       */
              read #4,key > dt_key$, using L63300, dt_rec$,               ~
                                           eod goto daily_detail_done
L63300:          FMT CH(128)            /* (AWD027) */
              dt_key$ = str(dt_rec$,1%,18%)
              if sav_key$ <> str(dt_key$,1%,10%) then                    ~
                                                goto daily_detail_done
              d_code$ = str(dt_rec$,19%,1%)
              if d_code$ <> "2" then goto L63360    /* Skip Adjustments */
                                                   /* Except Lunch     */
                 if str(dt_rec$,14%,5%) = "A030-" then                  ~
                    lunch$ = " 30 MIN "


                 if str(dt_rec$,14%,5%) <> "A045-" then                  ~
                                                   goto daily_detail_nxt
                    lunch$ = " 45 MIN "
                    goto daily_detail_nxt

L63360:          hr1%, hm1% = 0%
                 time$ = "XX : XX AM"
                 convert str(dt_rec$,14%,2%) to hr1%, data goto L63375
L63375:
                 convert str(dt_rec$,16%,2%) to hm1%, data goto L63385
L63385:
                 if hr1% > 24% then hr1% = hr1% - 24%
                 if hr1% >= 12% then str(time$,9%,2%) = "PM"
                 if hr1% > 12% then hr1% = hr1% - 12%
                 convert hr1% to str(time$,1%,2%), pic(##)

                 convert hm1% to str(time$,6%,2%), pic(00)
              if d_code$ <> "0" then goto L63440
                 in% = in% + 1%                     /* Set In Time     */
                 in$(in%) = time$
                 goto daily_detail_nxt
L63440:       if d_code$ <> "1" then goto daily_detail_nxt
                 out% = out% + 1%                   /* Set Out Time    */
                 out$(out%) = time$
                 goto daily_detail_nxt
        daily_detail_done
              if in% <> 0% then goto L63480
                 goto daily_done                    /* No Data For Day */

L63480:       hr1% = 0% : hm1% = 0%
              mt_key$ = all(hex(00))
              convert mt_yr$ to temp%, data goto L63500
L63500:       mt_yr_bi$ = bin(temp%, 2)
              str(mt_key$,1%,2%)  = mt_yr_bi$           /* Prod Year   */
              str(mt_key$,3%,5%)  = str(sav_key$,6%,5%) /* Employee No */
              str(mt_key$,8%,2%)  = sc_wk$              /* Prod Week   */
              str(mt_key$,10%,1%) = str(sav_key$,5%,1%) /* Prod Day    */
              read #3,key 1% = mt_key$, using L63520, mt_hours%, mt_min%, ~
                          mt_var_day$, mt_proc$, mt_user$, eod goto L63545
L63520:          FMT POS(20), 2*BI(2), POS(26), 2*CH(1), CH(3)
              mt_day$ = str(sav_key$,5%,1%)
              hr1% = hr1% + mt_hours%
              hm1% = hm1% + mt_min%

L63545:       hr1% = hr1% + int(hm1%/60.0)
              hm1% = mod(hm1%,60.0)
              convert hr1% to mt_hours$, pic(##)
              convert hm1% to mt_min$, pic(##)
              code$ = mt_var_day$ : gosub lookup_variance
              mt_var_dayd$ = desc$
              code$ = mt_proc$    : gosub lookup_variance
              mt_procd$ = desc$
              gosub lookup_user
              gosub print_detail_5
        daily_done
             init(" ") dt_key$
             if sc_day$ <> "A" then goto L63645     /* Single Day Only  */
                convert str(sav_key$,5%,1%) to dx%, data goto L63615
L63615:
                if dx% = 7% then goto L63645        /* Check Employee   */
                   convert (dx%+1%) to str(sav_key$,5%,1%), pic(#)

                   str(dt_key$,1%,10%) = sav_key$  /* Reset Detail Key */
                   goto daily_detail_nxt
L63645:      if sc_emp$ <> "ALL" then goto daily_finished /* One Emp   */
                goto daily_next                    /* Get Next Employee*/
                                                   /* For Spec. Dept.  */
        daily_finished
        return

        load_depts
            init(" ") readkey$, dept$()
            dept% = 0% : dept_max% = 0%
            str(readkey$,1%,9%) = "EMP DEPT "
        load_depts_nxt
            read #2,key > readkey$, using L63710, readkey$,               ~
                                                 eod goto load_depts_done
L63710:        FMT CH(24)
            if str(readkey$,1%,9%) <> "EMP DEPT " then                   ~
                                                     goto load_depts_done
               x% = 999%
/* L63730:        convert str(readkey$,10%,3%) to x%, data goto L63730 */
               convert str(readkey$,10%,3%) to x%, data goto load_depts_nxt

               if x% > 994% then goto load_depts_done
                  dept% = dept% + 1%
                  dept$(dept%) = str(readkey$,10%,3%)
                  goto load_depts_nxt
        load_depts_done
            dept_max% = dept%
        return

/* (AWD025) */
        write_temp_file
          float_sym$ = "1"                            /*<AWD030>*/
          float_hol% = float_hol% + 1%
REM WRITE #50, USING TEMP_FMT, E_DEPT$, E_NO$, EOD GOTO L63735
/*<AWD030>*/ write #50, using TEMP_FMT, e_dept$, e_no$, float_sym$,     ~
                        eod goto L63735
REM TEMP_FMT:          FMT CH(02), CH(05)
TEMP_FMT:          FMT CH(03), CH(05), CH(1)        /*<AWD030>*/
        return

/*<AWD029> & <AWD030> */
L63735: float_key$ = e_dept$ & e_no$ & "2"
        read #50, key = float_key$, using TEMP_FMT, e_dept$, e_no$,     ~
                        float_sym$, eod goto add_2_float
        float_key$ = e_dept$ & e_no$ & "3"
        read #50, key = float_key$, using TEMP_FMT, e_dept$, e_no$,     ~
                        float_sym$, eod goto add_3_float
        float_key$ = e_dept$ & e_no$ & "4"
/*SR71971 */
        read #50, key = float_key$, using TEMP_FMT, e_dept$, e_no$,     ~
                        float_sym$, eod goto add_4_float
        float_key$ = e_dept$ & e_no$ & "5"
/*SR71971 */
        read #50, key = float_key$, using TEMP_FMT, e_dept$, e_no$,     ~
                        float_sym$, eod goto add_5_float
        float_key$ = e_dept$ & e_no$ & "6"
/*SR71971 */
        read #50, key = float_key$, using TEMP_FMT, e_dept$, e_no$,     ~
                        float_sym$, eod goto add_6_float
        goto float_error
        
add_2_float:
        float_sym$ = "2"
        write #50, using TEMP_FMT, e_dept$, e_no$, float_sym$,          ~
                   eod goto float_error
        return
        
add_3_float:
        float_sym$ = "3"
        write #50, using TEMP_FMT, e_dept$, e_no$, float_sym$,          ~
                   eod goto float_error
        return
/*SR71971 */
add_4_float:
        float_sym$ = "4"
        write #50, using TEMP_FMT, e_dept$, e_no$, float_sym$,          ~
                   eod goto float_error
        return

/*SR71971 */
add_5_float:
        float_sym$ = "5"
        write #50, using TEMP_FMT, e_dept$, e_no$, float_sym$,          ~
                   eod goto float_error
        return

/*SR71971 */
add_6_float:
        float_sym$ = "6"
        write #50, using TEMP_FMT, e_dept$, e_no$, float_sym$,          ~
                   eod goto float_error
        return

float_error:
        hdr$ = "*** Floating Holiday Error ***"
        msg$(1) = "Employee Number "&e_no$&" has more than 6 Floating"
        msg$(2) = "  Holidays. Please correct and Request Report again." 
        msg$(3) = "  Press any key to abort."
        call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        goto exit_program
/*<AWD029> & <AWD030> */
        
        float_holiday
          hol_prnt% = 1%
          p% = pos("3678" = sel$)
          if p% = 0% then return

          call "SHOSTAT" ("Printing Floating Holiday Report")
          if float_hol% = 0% then goto no_float_holiday
          select printer (134)
          pageno% = 0%
          lcntr% = 99%
          readkey$ = " "
          title$ = "       Floating Holiday Report          "
          if sel$ = "6" then                                           ~
             title$ = "       Floating Hol Temp Employees          "
          if sel$ = "8" then                                           ~
             title$ = "       Floating Hol TEMP TX AES Employees   "
          if sel$ = "9" then                                           ~
             title$ = "       Floating Hol TX AES Employees        "
          gosub print_float_holiday
        return
        return
        no_float_holiday
          print using L55040
          print using L55580
        print_finished
          print using L55040
          close printer
        return

        print_float_holiday
           if lcntr% > 57% then gosub print_header
           read #50, key > readkey$, using TEMP_FMT, e_dept$, mt_emp$,  ~
/*<AWD030>*/              float_sym$, eod goto print_finished
           readkey$ = e_dept$ & mt_emp$ & float_sym$      /*<AWD030>*/
           gosub load_employee
           print using L55391
           print using L55395, mt_emp$, name$, e_dept$, float_sym$ /*<AWD030>*/
        goto print_float_holiday
/* (AWD025) */

        OPEN_FILE
          write_cnt% = 0%
          fx% = 8%
          file$ = "AWDPYROL"
        
          if sel$ = "7" then goto OPEN_AES
        
            call "OPENCHCK" (#fx%, fs%(fx%), f2%(fx%), 0%,  rslt$(fx%))
            if f2%(fx%) <> 0% then goto L00120
                  call "FILEBGON" addr(#fx%)
L00120
            open nodisplay #fx%, output, file = "AWDPYROL",         ~
                 library = library$, volume = volume$

REM LIBRARY = "APCDATA", VOLUME = "CARLO2"
REM OPEN NODISPLAY #8, OUTPUT, FILE = "AWDPYROL",         ~
REM LIBRARY = "TESTDATA", VOLUME = "EWD2"

            close #fx%
            call "OPENCHCK" (#fx%, fs%(fx%), f2%(fx%), 0%, rslt$(fx%))
        return
        OPEN_AES
          fx% = 18%
          file$ = "AESPYROL"        

            call "OPENCHCK" (#fx%, fs%(fx%), f2%(fx%), 0%,  rslt$(fx%))
            if f2%(fx%) <> 0% then goto L00125
                  call "FILEBGON" addr(#fx%)
L00125
            open nodisplay #fx%, output, file = "AESPYROL",         ~
                 library = library$, volume = volume$

REM LIBRARY = "APCDATA", VOLUME = "CARLO2"
REM OPEN NODISPLAY #8, OUTPUT, FILE = "AWDPYROL",         ~
REM LIBRARY = "TESTDATA", VOLUME = "EWD2"

            close #fx%
            call "OPENCHCK" (#fx%, fs%(fx%), f2%(fx%), 0%, rslt$(fx%))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "FILEBGON" (#50)         /* (AWD025) */
            close #7
            call "SHOSTAT" ("One Moment Please")

            end


