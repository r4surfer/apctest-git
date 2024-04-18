        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCVACRP copied from APCEMPRP        *~
            *  Creation Date     - 05/23/2006                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *  Description       - Update Vacation time for employees   *~
            *                      who have been employerd 18+ months   *~
            *                      and are hourly. This should be done  *~
            *                      each 6 months.                       *~
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
            *---WHEN-----+--------------WHAT----------------------+-WHO-*~
            * 05/23/2006 ! New Program for vacation hours         ! DES *~
            * 02/12/2019 ! CR-1894 Emp Dept field size change     ! DES *~
            *************************************************************

        dim                                                              ~
            dr(8%,15%), dov(8%,15%),     /* Weekly Reg and Overtime Hrs*/~
            dr$10, dov$10,               /*                            */~
            /*---------------------------------------------------------*/~
            UpdateFlag$1,                /* Update vacation time? {Y|N}*/~
            EligFlag$1,                  /* Gets additional vac   {Y|N}*/~
            NewFlag$1,                   /* New person (12-18 months)  */~
            CurEmpNbr$5,                 /* Current Employee Number    */~
            Temp1%, Temp2%, Temp3,       /* Temporary work variables   */~
	    SumMonths%,                  /* Sum Hrs for 6 month period */~
	    MonthsSrvc%,                 /* MOnths of Service          */~
	    HireDate$8,                  /* Hire date w/out century    */~
	    RunDate$10,                  /* start date                 */~
	    RunDateS$8,                  /* start date, no century     */~
	    new_vac_days%,               /* adjusted vacation days     */~
	    Fyy%, Fmm%, Fdd%, Tyy%, Tmm%, Tdd%, /* from and to yy/mm/dd*/~
	    Tyy$4, Tmm$2, Tdd$2,                /* from and to yy/mm/dd*/~
            Edd%, Emm%, DaysInMonth%(12%),                               ~
            /*---------------------------------------------------------*/~
            dd$(50%)2,                   /* Departments                */~
            var_check$5,                 /* SEL (3) - (WK NO)          */~
            access$30, hdr$40,           /* Security Check             */~
            msg$(3%)79,                  /* Security Check             */~
            e_shft$2,                    /* Employee Shift     (EWD006)*/~
            e_reg$10,e_ovr$10,e_sick$10, /* SELECTION (3) HOURS        */~
            e_vac$10,e_hol$10,e_tot$10,  /*                            */~
            e_add1$30,                   /* Employee Address1  (EWD005)*/~
            scr$(11%)40,                 /* Screen Report Options      */~
            sav_dept$3, save_dept$3,     /* SELECTION (2) DEPARTMENT   */~
            sc_dept$3, sc_dept_d$30,     /* SCREEN DEPARTMENT          */~
            sc_wk$2, sc_wk_dte$8,        /* SCREEN WEEK                */~
            sc_day$1,                    /* SCREEN PRODUCTION DAY      */~
            sv_wk$2, sv_day$1,           /* CURRENT WEEK AND DAY       */~
            dept$(999%)3,                 /* Store Department Codes     */~
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
            mt_fil$3,                    /* FILLER AREA                */~
            mt_rec$32,                   /* APCEMPMT - RECORD          */~
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
            status$(30%)14, dp$(30%)2,   /* CLOCK STATUS / Department  */~
            c_th%(30%), c_tm%(30%),      /* CLOCK HOURS AND MINUTES    */~
            c_th$(30%)2, c_tm$(30%)2,    /* CLOCK HOURS AND MINUTES    */~
            status1$(30%)14,             /* Status For Dept Variance   */~
            l_usr$(30%)16,               /* Last Mod By                */~
            dt_key$18, d_code$1,         /* DETAIL KEY AND CODE        */~
            dt_rec$40, dt_usr$3,         /* Detail Record              */~
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
            userid$3                     /* Current User Id            */

        dim                              /* Subroutine - Variables     */~
            cur_yr$4,                    /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4,                    /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8        /* Prod Week Date Form/Unform */

        dim f2%(8%),                     /* = 0 if the file is open    */~
            f1%(8%),                     /* = 1 if READ was successful */~
            fs%(8%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(8%)20                  /* Text from file opening     */

/* (EWD001)  */
        dim py_key$12,                   /* EWDPYROL ReadKey           */~
            py_key1$14,                  /* AWDPYROL ReadKey           */~
            py_dept$6,                   /* Employee Dept              */~
            py_no$6,                     /* Employee Number            */~
            py_code$3,                   /* Company Code               */~
            py_batch$1,                  /* Company Batch ID           */~
            py_name$20,                  /* Employee Name              */~
            py_reg_hrs$14,               /* Regular Emp Hours (AWD011) */~
            py_ovr_hrs$14,               /* Regular Overtime  (AWD011) */~
            py_hol_hrs$14,               /* Holiday Hours     (AWD011) */~
            py_tot_hrs$14,               /* Total Hours       (AWD011) */~
            py_pay_rate$14               /* Pay Rate          (AWD011) */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Employee Vacation Time Update Utility   "
            pname$ = "APCVACRP - Rev: R1.00"

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
            * #6  ! USERCLMS ! Master User Id File                      *~
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
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   13,                    ~
                        alt key  1, keypos =    4, keylen =  10, dup

            select #6,   "USERLCMS",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

                                    
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),500%, rslt$(6%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            days$(1%) = "MONDAY   " : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY" : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   " : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "

            date$ = date
            RunDate$ = date
            call "DATEFMT" (date$)
            call "DATFMTC" (RunDate$)
            datec$ = date
            call "DATFMTC" (datec$)

	    convert str(datec$,1,2) to Tmm%
	    convert str(datec$,4,2) to Tdd%
	    convert str(datec$,7,4) to Tyy%
            DaysInMonth%(01%) = 31%
            DaysInMonth%(02%) = 28%
            DaysInMonth%(03%) = 31%
            DaysInMonth%(04%) = 30%
            DaysInMonth%(05%) = 31%
            DaysInMonth%(06%) = 30%
            DaysInMonth%(07%) = 31%
            DaysInMonth%(08%) = 31%
            DaysInMonth%(09%) = 30%
            DaysInMonth%(10%) = 31%
            DaysInMonth%(11%) = 30%
            DaysInMonth%(12%) = 31%
            scr$(1%) = "***********************************"
            scr$(2%) = "*          Report Selections      *"
            scr$(3%) = "*                                 *"
            scr$(4%) = "*(1) - Run Test Report (No Update)*"
            scr$(5%) = "*(2) - Run Report with Update.    *"
            scr$(6%) = "***********************************"

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

            for fieldnr% = 1% to 4%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
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
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
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
            gosub select_printer
            gosub vacation_report
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
            title$ = "Vacation Update Report"                   
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
         "Enter a Valid Report Selection? ( 1 thru 2 )                 ",~
         "Enter a Valid Department Code or (AA)= All?                  ",~
         "Enter a Valid Date to run for.                               ",~
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
                      mt_proc$, mt_fil$, mt_rec$, sc_dept$, sc_dept_d$,  ~
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
                    mt_var_out$, mt_var_day$, mt_proc$, mt_user$, mt_fil$
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
            CH(03),                      /* Last Modified By Userid    */~
            CH(03)                       /* Filler Area                */

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
                                L40200,         /* Department Code, ALL */~
                                L40200,         /* Month                */~
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
               at (05,02), "Date to run for       :",                    ~
               at (05,30), fac(lfac$(3%)), RunDate$             , ch(10),~
                                                                         ~
               at (06,02), "Employee No. or ALL  :",                     ~
               at (06,30), fac(lfac$(4%)), sc_emp$              , ch(05),~
               at (06,40), fac(hex(84)), sc_emp_d$              , ch(20),~
                                                                         ~
               at (10,23), fac(hex(84)), scr$(1%)               , ch(35),~
               at (11,23), fac(hex(84)), scr$(2%)               , ch(35),~
               at (12,23), fac(hex(84)), scr$(3%)               , ch(35),~
               at (13,23), fac(hex(84)), scr$(4%)               , ch(35),~
               at (14,23), fac(hex(84)), scr$(5%)               , ch(35),~
               at (15,23), fac(hex(84)), scr$(6%)               , ch(35),~
               at (16,23), fac(hex(84)), scr$(7%)               , ch(35),~
               at (17,23), fac(hex(84)), scr$(8%)               , ch(35),~
               at (18,23), fac(hex(84)), scr$(9%)               , ch(35),~
               at (19,23), fac(hex(84)), scr$(10%)              , ch(35),~
               at (20,23), fac(hex(84)), scr$(11%)              , ch(35),~
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
			      L50800,         /* Date                  */ ~
                              L51660          /* Employee Number       */
            return

L50150: REM Report Selection                           SEL$
           security% = 0%
           if sel$ <> " " then goto L50190
              sel$ = "1"
L50190:    convert sel$ to sel%, data goto L50380

           UpdateFlag$ = "N"       /* Update vacation time? {Y|N}*/
           if sel% < 1% or sel% > 2% then goto L50380   
              if sel$ = "2" then UpdateFlag$ = "Y"                     
           reg = 40.0
           gosub load_security
           pp% = pos("136" = sel$)
           if pp% = 0% then return
           if security% = 0% then goto L50410
           if edit% = 1% then goto L50350   /* Default All Dept's */
        return

L50350:    gosub L50450
REM        fieldnr% = 2%
        return

L50380:    errormsg$ = "(Error) - Invalid Report Selection Entered?"
           init(" ") sel$, sel_d$
        return

L50410:    errormsg$ = "(Error) - Access to Report Selection Denied??"
           init(" ")sel$, sel_d$
        return

L50450: REM Department Code                       SC_DEPT$
            sc_dept% = 0%
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

L50800:
REM*        Test data for START DATE
	call "DATEOKC" (RunDate$, 0%, errormsg$)
	if errormsg$ <> " " then return
REM     call "DATUFMTC" (RunDate$)
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

L55170: %!                                                               ~
        ~                                                                !

L55270: %!<--- Department ------>!Emply!Last Name, First Mid!Vc-Days Dte~
        ~!Vc-Used Dte!Nw-Days Dte!Hire Dte!Month Svc!                 ~ 
        ~    !
L55290: %!### ###################!#####!####################!##-########~
        ~!##-########!##-########!########!  ###    !                 ~
        ~    !
L55320: %!-----------------------!-----!--------------------!-----------~
        ~!-----------!-----------!--------!---------!-----------------~  
	~----!

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
           print using L55270
           lcntr% = lcntr% + 1%

L60265: return

        print_detail
            if lcntr% > 57% then gosub print_header
               if sav_dept$ <> " " then goto L60375
                  goto L60385
L60375:        if sav_dept$ = mt_dept$ then goto L60395
                  gosub print_header
L60385:           sav_dept$ = mt_dept$

L60395:        print using L55320
               print using L55290, mt_dept$, mt_dept_d$, mt_emp$, name$,  ~
                     e_vac_days%, e_vac_days$, e_vac_used%, e_vac_used$, ~
                     new_vac_days%, RundateS$, HireDate$,                   ~   
                     MonthsSrvc%                     
               lcntr% = lcntr% + 2%

        return

        load_employee
            init(" ") name$ : name% = 0%
            read #1,key = mt_emp$, using L61150, e_lname$, e_fname$,      ~
                                                  e_init$, eod goto L61175
L61150:        FMT POS(12), CH(15), CH(10), CH(1)
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

        vacation_report
           call "SHOSTAT" ("Creating Employee VAC/SICK/PTS Report")
           e_key$ = all(hex(00)) : sav_dept$, save_dept$ = " "
           if str(sc_dept$,1%,1%) <> "A" then                            ~
                                  str(e_key$,1%,3%) = str(sc_dept$,1%,3%)
        vacation_next
           read #1,hold ,key 1% > e_key$, using L61295, e_key$, e_status$,~
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

L61355:    get #1, using L61380, e_hire_days$,                           ~
                                 e_vac_days%, e_vac_days$, e_vac_used%,  ~
                                e_vac_used$, e_sick_days%, e_sick_days$, ~
                                e_sick_used%, e_sick_used$, e_pts_late,  ~
                                e_pts_mis, e_pts_day, e_pts_tot,         ~
                                e_pts_dte$
L61380:      FMT POS(194), CH(6),                                        ~
                 POS(218), BI(2), CH(6), BI(2), CH(6), BI(2), CH(6),     ~
                           BI(2), CH(6), POS(284), 4*PD(14,4), CH(6)

             e_vac_used% = (e_vac_used% / 10)         /*  (AWD010)  */
             
	     HireDate$ = e_hire_days$
             call "DATFMTC" (e_hire_days$)
             call "DATEFMT" (HireDate$)
             call "DATEFMT" (e_vac_days$)
             call "DATEFMT" (e_vac_used$)

             NewFlag$ = "N"
             if e_vac_days$ = "        " then NewFlag$ = "Y"

             SumMonths% = 0%
             gosub calculate_months_of_service
             if EligFlag$ = "N" then goto vacation_next
      /*  if months < 18, not prev date, else 3+ months diff */

            RunDateS$ = str(RunDate$,1,6) 
            str(RunDateS$,7,2) = str(RunDate$,9,2)

             /* Temp2% is the remainder of the sum of months divided by 6 */
             Temp1% = SumMonths% / 6%
             Temp2% = SumMonths% - (Temp1% * 6%)

	     /* if Temp2% is not 0%, than it is not a six month aniversary */
             if Temp2% <> 0% and SumMonths% < 19% then goto vacation_next 
	     MonthsSrvc% = SumMonths%

	     /* add 5 days vacation!!! */
REM          new_vac_days% = e_vac_days% + 5%
             new_vac_days% = e_vac_days% + 5% - e_vac_used%

             /* Update file with new vacation time */
             if sel% <> 2%  then goto L61450
 
	     RunDateU$ = RunDate$
             call "DATUFMTC" (RunDateU$)
             put #1 using L61425, new_vac_days%, RunDateU$      
L61425:      FMT POS(218), BI(2), CH(6)

             rewrite #1

L61450:
             /* six month aniversary & eighteen or more months of service */
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
             gosub print_detail
             goto vacation_next
        vacation_done
        return

        calculate_months_of_service
            EligFlag$ = "N"                                               
            convert str(e_hire_days$,1,2) to Fmm%
            convert str(e_hire_days$,4,2) to Fdd%
            convert str(e_hire_days$,7,4) to Fyy%
	    convert str(RunDate$,1,2) to Tmm%
            convert str(RunDate$,4,2) to Tdd%
            convert str(RunDate$,7,4) to Tyy%
            Temp3% = (Tyy% - Fyy%) * 12%
            Temp3% = Temp3% + Tmm% - Fmm%
	    Edd% = Tdd%
	    Emm% = Tmm%

	    if Fdd% > Tdd% then Temp3% = Temp3% - 1
            SumMonths% = Temp3% 
            if SumMonths% > 18% then goto calculate_over18 
            if SumMonths% < 12% then goto calculate_exit    

/* if employee has 12 to 18 months of servic+ months are Elig */
  	    if Fdd% > Tdd% then goto calculate_skip0
/* it in the +0 to +6 day range of the aniversary? */  
	    if Tdd% > (Fdd% + 6%) then goto calculate_exit
	    EligFlag$ = "Y"
            goto calculate_exit

calculate_skip0:
/* adust date to look at previous month and add days in month */
            if Tmm% = 1% then Tmm% = 13%
            Emm% = Emm% - 1%
            Edd% = Edd% + DaysInMonth%(Emm%)
/* it out of the adjusted month */
            if Emm% <> Fmm% then goto calculate_exit
/* it in the +0 to +6 day range of the aniversary? */  
	    if Edd% > (Fdd% + 6%) then goto calculate_exit
	    if Edd% < Fdd% then goto calculate_exit
	    EligFlag$ = "Y"

calculate_skip1:
            goto calculate_exit

/* if employee has 18+ months and first 7 days of Jan or July, they are Elig */
calculate_over18:
            /* date in e_vac_days$ must not be for current run */
            RunDateS$ = str(RunDate$,1,6) 
            str(RunDateS$,7,2) = str(RunDate$,9,2)
            if RunDateS$ = e_vac_days$ then goto calculate_exit
 
	    /* only run for 1/1 & 7/1 */
	    if Emm% <> 7% and Emm <> 1% then goto calculate_over18_skip1  
	    if Edd% <> 1% then goto calculate_over18_skip1  

	    /* for 18+ monthers, must be 1/1 or 7/1 for run */
	    if Emm% > 3% then goto calculate_over18_skip1
	    Emm% = 1%
	    Edd% = 1%
	    goto calulate_over18_cont

calculate_over18_skip1:
	    if str(RunDate$,1,2) > "09" then goto calculate_over18_skip2 
	    Emm% = 7%
	    Edd% = 1%
	    goto calulate_over18_cont

calculate_over18_skip2:
            Emm% = 1%
	    Edd% = 1%
            Eyy% = Tyy% + 1%

calulate_over18_cont:
            if (Tmm% <> 1% and Tmm% <> 7%) or Tdd% > 7% then goto calculate_exit

	    EligFlag$ = "Y"

calculate_exit:
        return

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
L63730:        convert str(readkey$,10%,3%) to x%, data goto load_depts_done

               if x% > 994% then goto load_depts_done
                  dept% = dept% + 1%
                  dept$(dept%) = str(readkey$,10%,3%)
                  goto load_depts_nxt
        load_depts_done
            dept_max% = dept%
        return

TOTAL_DAY:
	return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


