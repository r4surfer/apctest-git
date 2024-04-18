        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   EEEEE  M   M  PPPP   PPPP   TTTTT   *~
            *  A   A  P   P  C   C  E      MM MM  P   P  P   P    T     *~
            *  AAAAA  PPPP   C      EEEE   M M M  PPPP   PPPP     T     *~
            *  A   A  P      C   C  E      M   M  P      P        T     *~
            *  A   A  P       CCC   EEEEE  M   M  P      P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCEMPPT - Employee Time Clock Utility Program to Display *~
            *            Edit and Modify Points.                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/01/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/30/97 ! Mod - Check out for the new release of   ! RHH *~
            *          !       Caelus.                            !     *~
            * 03/25/98 ! Y2K modifications                        ! ERN *~
            * 03/10/99 ! (EWD001) Mod's for Security Activate MVK,! RHH *~
            *          !       LBH, LLJ.                          !     *~ 
            * 06/21/06 ! (EWD002) Mod's for Security Activate MKN ! DES *~
            * 07/24/06 ! (EWD003) Mod's for Security Activate LRN ! CMG *~
            *11/12/2012! (AWD004) mod for weeksemp Sunday to Satur! CMG *~     
            *05/28/2014! (AWD005) Add user LNT.                   ! PWW *~
            *02/11/2019! CR-1894  Increase the size of EMP DEPT   ! DES *~
            *03/11/2020! CR2467  Allow user entry of 2 char dept  ! RDB *~
            *05/07/2020! CR2490  Increase size for Employee Number! RDB *~
            *************************************************************

        dim                                                              ~
            pt_key$16,                   /* Primary Key                */~
            pt_no$5, pt_shft$2,          /* Employee Number            */~
            pt_dept$3, pt_dept_d$30,     /* Department Code            */~
            pt_wk$2, pt_yr$,             /* Production Week            */~
            pt_day$1,                    /* Production Day             */~
            pt_code$1, pt_code_d$30,     /* Pts Cde 0=Late,1=Misc,3=Day*/~
            pt_dte$6, pt_date$8,         /* Date Entry Made            */~
            pt_usr$3,                    /* User Who Made Change       */~
            pt_reason$30,                /* Reason for Change          */~
            pt_pts$8,                    /* Point Entry Made +, -      */~
            pt_fil$10, xx_dept$3,        /* Filler Area                */~
            access$30, hdr$40,msg$(3)79, /* SECURITY DEPARTMENT        */~
            sc_dept$3, sc_dept_d$30,     /* SCREEN DEPARTMENT          */~
            sc_wk$2, sc_wk_dte$8,        /* SCREEN WEEK                */~
            sc_day$1, sc_yr$4,           /* SCREEN PRODUCTION DAY      */~
            sc_empno$8,                  /* Screen employee nbr CR2490 */~
            sv_day$1,                    /* Current Day                */~
            days$(7)9,                   /* DAYS OF THE WEEK           */~
            days$9,                      /* DAY OF THE WEEK            */~
            date$8,                      /* SCREEN DATE                */~
            prv_yr$2,                    /* CURRENT AND PREVIOUS YEAR  */~
            bg_date$10, bg_dte$6,        /* Beg/End Prod Date          */~
            ed_date$10, ed_dte$6,        /* Beg/End Prod Date          */~
            bg_dept$3, ed_dept$3,        /* Beg/End Dept Code          */~
            bg_dept_d$30, ed_dept_d$30,  /* Beg/End Descriptions       */~
            beg_emp$5, beg_emp_name$30,  /* Beginning Employee Name    */~
            end_emp$5, end_emp_name$30,  /* Beginning Employee Name    */~
            e_lname$15,                  /* EMPLOYEE LAST NAME         */~
            e_fname$10,                  /* EMPLOYEE FIRST NAME        */~
            e_init$1,                    /* EMPLOYEE MIDDLE INITIAL    */~
            e_dept$3, x$3, y$10,         /* Employee Department Code   */~
            e_pts_late$6,                /* Employee Late Points       */~
            e_pts_mis$6,                 /* Employee Misc Points       */~
            e_pts_day$6,                 /* Employee Daily Points      */~
            e_pts_tot$6,                 /* Employee Total Points      */~
            e_pts_dte$8, dept_d$30,      /* Employee Date of Last PTS  */~
            bg_shft$2, bg_shft_d$30,     /* Beg/End Shift Codes        */~
            ed_shft$2, ed_shft_d$30,     /* Beg/End Shift Codes        */~
            save_shft$2, shft_d$30,      /* Report Formatting          */~
            save_dept$3, save_emp$5,     /* Report Formatting          */~
            title$40,                    /* REPORT TITLE               */~
            runtime$8,                   /* REPORT RUN TIME            */~
            readkey$50,                  /* Generic Key                */~
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
            cur_yr$4, cur_yr_bi$2,       /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4, ent_yr_bi$2,       /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8        /* Prod Week Date Form/Unform */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Employee Time Clock Points Maintenance  "
            pname$ = "APCEMPPT - Rev: R6.04"

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
            * #3  ! APCEMPPT ! Employee Master Time File                *~
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

            select #3,   "APCEMPPT",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    7, keylen =   16,                    ~
                        alt key 1, keypos =   1, keylen = 22
REM                     varc,     indexed,  recsize =   73,

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)


            days$(1%) = "MONDAY   "  : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY"  : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   "  : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "
/*(CR1489) */
REM DAYS$(2%) = "MONDAY   "  DAYS$(3%) = "TUESDAY  "
REM DAYS$(4%) = "WEDNESDAY"  DAYS$(5%) = "THURSDAY "
REM DAYS$(6%) = "FRIDAY   "  DAYS$(7%) = "SATURDAY "
REM DAYS$(1%) = "SUNDAY   "
/* (\AWD004) */


            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 7%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 14% then goto inputmode_a
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
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 14% then goto inputmode_a
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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
            *       I N P U T   M O D E   F O R   R E P O R T           *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_a
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L12100:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12220
L12120:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12200
L12150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L12120
                         if fieldnr% = 1% then L12100
                         goto L12150
L12200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12120
L12220:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   R E P O R T            *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L12390:     zz%      = cursor%(1%) - 2%
            if zz% = 1% or zz% = 2% then fieldnr% = 1%
            if zz% = 3% or zz% = 4% then fieldnr% = 2%
            if zz% = 5% or zz% = 6% then fieldnr% = 3%
            if zz% = 7% or zz% = 8% then fieldnr% = 4%
            if fieldnr% < 1% or fieldnr% > 4% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12480:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12480
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12480
                  lastfieldnr% = fieldnr%
            goto L12390

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub select_printer
            gosub generate_report
            close printer
        return clear all
        goto inputmode

        select_printer
            call "SHOSTAT" ("Creating Points Audit Report")
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
            title$ = "      Employee Points Audit Report      "
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'052(fieldnr%)
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
         "Enter a Valid Department Code?                               ",~
         "Enter a Valid Production Week (1-52), Blank = Current Week?  ",~
         "Enter a Valid Production Day (1-7), Blank = Current Day?     ",~
         "Enter a Valid Employee Number?                               ",~
         "Enter a Valid Points Code, 0 = Late, 1 = Misc, 2 = Day?      ",~
         "Enter the Applicable Description for Points Change?          ",~
         "Enter the Points Change, with Plus or Minus                  "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28300
                inpmessage$ = edtmessage$
                return

L28300
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Beginning/Ending Production Date?              ",~
         "Enter a Valid Beginning/Ending Department Code?              ",~
         "Enter a Valid Beginning/Ending Shift Code or 'AL' = ALL?     ",~
         "Enter a Valid Beginning/Ending Employee Number or ALL?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, pt_no$, pt_dept$,          ~
                      pt_dept_d$, pt_wk$, pt_day$, pt_code$, pt_dte$,    ~
                      pt_usr$, pt_reason$, pt_pts$, pt_fil$, sc_yr$,     ~
                      sc_wk_dte$, sc_day$, sc_dept$, sc_dept_d$, days$,  ~
                      bg_dept$, bg_dept_d$, ed_dept$, ed_dept_d$, name$, ~
                      bg_date$, bg_dte$, ed_date$, ed_dte$, pt_code_d$,  ~
                      e_pts_late$, bg_shft$, ed_shft$, sc_wk$, sc_yr$,   ~
                      e_pts_mis$, e_pts_day$, e_pts_tot$, e_pts_dte$,    ~
                      beg_emp$, beg_emp_name$, end_emp$, end_emp_name$,  ~
                      bg_shft_d$, ed_shft_d$, pt_date$, sc_empno$
            rpt% = 0%
            rec% = 0%
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
            rec% = 0%
            if rpt% = 1% then goto L30180
               init(" ") pt_key$
               str(pt_key$,1%,3%)  = sc_dept$
               str(pt_key$,4%,2%)  = pt_shft$
               str(pt_key$,6%,2%)  = sc_yr$
               str(pt_key$,8%,2%)  = sc_wk$
               str(pt_key$,10%,1%) = sc_day$
               str(pt_key$,11%,5%) = pt_no$
               str(pt_key$,16%,1%) = pt_code$
               sav_key$ = pt_key$
               read #3,key = pt_key$, eod goto L30320
L30180:     get #3, using L35040, pt_dte$, pt_dept$, pt_shft$, pt_yr$,    ~
                                 pt_wk$, pt_day$, pt_no$, pt_code$,      ~
                                 pt_reason$, pt_pts, pt_usr$, pt_fil$
            pt_date$ = pt_dte$
            call "DATEFMT" (pt_date$)
            init(" ") pt_dept_d$, pt_pts$
            gosub load_dept
            pt_dept_d$ = dept_d$
            convert pt_pts to pt_pts$, pic(##.##-)
            if pt_code$ = "0" then pt_code_d$ = "Late Points        "
            if pt_code$ = "1" then pt_code_d$ = "Misc Points        "
            if pt_code$ = "2" then pt_code_d$ = "Day Points         "
            if pt_code$ = "3" then pt_code_d$ = "Correction to Point"
            rec% = 1%
L30320: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_it
        dataput
            pt_dept$ = sc_dept$
            pt_yr$   = sc_yr$
            convert sc_yr$ to pt_yr%               /* CR1894 */
            pt_wk$   = sc_wk$
            pt_day$  = sc_day$
            pt_usr$  = userid$
            pt_key$ = " "
            str(pt_key$,1%,3%)  = sc_dept$
            str(pt_key$,4%,2%)  = pt_shft$
            str(pt_key$,6%,2%)  = sc_yr$
            str(pt_key$,8%,2%)  = sc_wk$
            str(pt_key$,10%,1%) = sc_day$
            str(pt_key$,11%,5%) = pt_no$
            str(pt_key$,16%,1%) = pt_code$
            if rec% = 1% then pt_key$ = sav_key$
            read #3,hold,key = pt_key$, eod goto L31260
                delete #3

            if keyhit% = 12% then goto L31320
L31260:         put #3, using L35040, pt_dte$, pt_dept$, pt_shft$, pt_yr%,~
                                     pt_wk$, pt_day$, pt_no$, pt_code$,  ~
                                     pt_reason$, pt_pts, pt_usr$, pt_fil$

            write #3, eod goto L31340
            gosub update_employee
L31320: return clear all
        goto inputmode
L31340:     errormsg$ = "(Error) = While Updateing Points for (" &       ~
                         sc_empno$ & ")?"       /* CR2490 */
        return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (APCEMPPT)          */
L35040: FMT CH(06),                      /* Date Changes Made          */~
            CH(03),                      /* Department Code            */~
            CH(02),                      /* Employee Shift Code        */~
            BI(02),                      /* Production Year Code       */~
            CH(02),                      /* Production Week            */~
            CH(01),                      /* Production Day             */~
            CH(05),                      /* Employee Code              */~
            CH(01),                      /* Points Code 0, 1, 2, 3     */~
            CH(30),                      /* Description of Change      */~
            PD(14,4),                    /* Points + or -              */~
            CH(3),                       /* User that Made Change      */~
            CH(10)                       /* Filler Area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40230,         /* Department Code      */~
                                L40230,         /* Production Week      */~
                                L40230,         /* Production Day       */~
                                L40240,         /* Employee Number      */~
                                L40240,         /* Points Code          */~
                                L40220,         /* Reason Description   */~
                                L40240          /* Points               */

              goto L40260

L40220:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40240:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Department Code      :",                     ~
               at (03,25), fac(lfac$(1%)), sc_dept$             , ch(03),~
               at (03,35), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (04,02), "Production Week, 1-52:",                     ~
               at (04,25), fac(lfac$(2%)), sc_wk$               , ch(02),~
               at (04,35), fac(hex(84)), sc_wk_dte$             , ch(08),~
               at (04,45), fac(lfac$(2%)), sc_yr$               , ch(04),~
                                                                         ~
               at (05,02), "Production Day(1 - 7):",                     ~
               at (05,25), fac(lfac$(3%)), sc_day$              , ch(01),~
               at (05,35), fac(hex(84)), days$                  , ch(09),~
               at (05,50), fac(hex(84)), pt_date$               , ch(08),~
                                                                         ~
               at (06,02), "Employee Number      :",                     ~
               at (06,25), fac(lfac$(4%)), sc_empno$            , ch(08),~
               at (06,35), fac(hex(84)), name$                  , ch(30),~
                                                                         ~
               at (07,02), "Points Code 0, 1, 2  :",                     ~
               at (07,25), fac(lfac$(5%)), pt_code$             , ch(01),~
               at (07,35), fac(hex(84)), pt_code_d$             , ch(30),~
                                                                         ~
               at (08,02), "Description of Change:",                     ~
               at (08,35), fac(lfac$(6%)), pt_reason$           , ch(30),~
                                                                         ~
               at (09,02), "Points Plus or Minus :",                     ~
               at (09,35), fac(lfac$(7%)), pt_pts$              , ch(08),~
                                                                         ~
               at (12,02), "Late Points : ",                             ~
               at (12,16), fac(hex(84)), e_pts_late$            , ch(06),~
                                                                         ~
               at (14,02), "Misc Points : ",                             ~
               at (14,16), fac(hex(84)), e_pts_mis$             , ch(06),~
                                                                         ~
               at (16,02), "Day  Points : ",                             ~
               at (16,16), fac(hex(84)), e_pts_day$             , ch(06),~
                                                                         ~
               at (18,02), "Total Points: ",                             ~
               at (18,16), fac(hex(84)), e_pts_tot$             , ch(06),~
                                                                         ~
               at (18,30), "Date of Last Points: ",                      ~
               at (18,51), fac(hex(84)), e_pts_dte$             , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 6% then goto L40880
                  table% = 1%                      /* Department Codes */
                  gosub display_codes
                  goto L40070

L40880:        if keyhit% <> 9% then goto L40920
                  gosub rolodex
                  goto L40070

L40920:        if keyhit% <> 15 then goto L40960
                  call "PRNTSCRN"
                  goto L40070

L40960:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            rpt% = 0%
        if edit% = 2% then L41190     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (14)print report"
            pf$(2) = "                 (6)Display Dept's      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Rolodex             " &        ~
                     "(12)Delete Rec.        (16)Exit Program"
            pfkeys$ = hex(01ffff04ff06ffff09ffff0cff0e0f1000)
            if fieldnr% = 1% then L41130
                str(pf$(1%),64%)  = " " : str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),64%)  = " " : str(pfkeys$,16%,1%) = hex(ff)
L41130:     if fieldnr% > 1% then L41150
               str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41150:     if rec% = 1% then return
               str(pf$(3%),40%,16%) = " " : str(pfkeys$,12%,1%) = hex(ff)
            return

L41190: if fieldnr% > 0% then L41310  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (6)Display Dept's      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Rolodex             " &        ~
                     "(12)Delete Rec.        (16)Update Data "
            pfkeys$ = hex(01ffffffff06ffff09ffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(3%),40%,16%) = " " : str(pfkeys$,12%,1%) = hex(ff)
            return

L41310:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               R E P O R T   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
L41480:       gosub set_pf2

              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41600,         /* Beg/End Prod. Date   */~
                                L41600,         /* Beg/End Dept Code    */~
                                L41600,         /* Beg/End Shift Code   */~
                                L41600          /* Employee Number - All*/
              goto L41630

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41600:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41630:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Beginning Prod. Date  :",                    ~
               at (03,25), fac(lfac$(1%)), bg_date$             , ch(10),~
                                                                         ~
               at (04,02), "Ending Prod. Date     :",                    ~
               at (04,25), fac(lfac$(1%)), ed_date$             , ch(10),~
                                                                         ~
               at (05,02), "Beginning Dept. Code :",                     ~
               at (05,25), fac(lfac$(2%)), bg_dept$             , ch(03),~
               at (05,35), fac(hex(84)), bg_dept_d$             , ch(30),~
                                                                         ~
               at (06,02), "Ending Dept. Code    :",                     ~
               at (06,25), fac(lfac$(2%)), ed_dept$             , ch(03),~
               at (06,35), fac(hex(84)), ed_dept_d$             , ch(30),~
                                                                         ~
               at (07,02), "Beginning Shift Code :",                     ~
               at (07,25), fac(lfac$(3%)), bg_shft$             , ch(02),~
               at (07,35), fac(hex(84)), bg_shft_d$             , ch(30),~
                                                                         ~
               at (08,02), "Ending Shift Code    :",                     ~
               at (08,25), fac(lfac$(3%)), ed_shft$             , ch(02),~
               at (08,35), fac(hex(84)), ed_shft_d$             , ch(30),~
                                                                         ~
               at (09,02), "Beginning Employee No:",                     ~
               at (09,25), fac(lfac$(4%)), beg_emp$             , ch(05),~
               at (09,35), fac(hex(84)), beg_emp_name$          , ch(30),~
                                                                         ~
               at (10,02), "Ending Employee No   :",                     ~
               at (10,25), fac(lfac$(4%)), end_emp$             , ch(05),~
               at (10,35), fac(hex(84)), end_emp_name$          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 6% then goto L42130
                  table% = 1%                      /* Department Codes */
                  gosub display_codes
                  goto L41480

L42130:        if keyhit% <> 9% then goto L42170
                  gosub rolodex
                  goto L41480

L42170:        if keyhit% <> 15 then goto L42210
                  call "PRNTSCRN"
                  goto L41480

L42210:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
           rpt% = 1%
        if edit% = 2% then L42410     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                 (6)Display Dept's      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Rolodex             " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff06ffff09ffffffffff0f1000)
            if fieldnr% = 1% then L42370
               str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L42370:     if fieldnr% > 1% then L42390
               str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42390:     return

L42410: if fieldnr% > 0% then L42510  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (6)Display Dept's      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Rolodex             " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffff06ffff09ffffffff0e0f1000)
            return

L42510:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50180,         /* Department Code or All*/ ~
                              L50390,         /* Production Week       */ ~
                              L50870,         /* Production Day        */ ~
                              L51100,         /* Employee Number       */ ~
                              L51330,         /* Points Code           */ ~
                              L51530,         /* Description of Points */ ~
                              L51560          /* Points Given          */

            return

L50180: REM Department Code                       SC_DEPT$
/* CR2467 access issue */
            if str(sc_dept$,1%,1%) ="A" then goto L50179
            convert sc_dept$ to x%, data goto L50179
            convert x% to sc_dept$, pic(000)
L50179:     gosub load_access
            if access% = 0% then goto L50240
               return clear all
               goto inputmode

L50240:     if sc_dept$ <> " " then goto L50260
               goto L50350
L50260:     convert sc_dept$ to x%, data goto L50350

            convert x% to sc_dept$, pic(000)

            pt_dept$ = sc_dept$
            gosub load_dept
            if dept% = 0% then goto L50350
            sc_dept_d$ = dept_d$
        return
L50350:     errormsg$ = "(Error) - Invalid Department Code?"
            init(" ") sc_dept$, sc_dept_d$
        return

L50390: REM Production Week                       MT_WK$
           init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,     ~
                     ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$
           
           if sc_yr$  <> " " then ent_yr$ = sc_yr$
           if sc_wk$  <> " " then ent_wk$ = sc_wk$
           if sc_day$ <> " " then ent_dy$ = sc_day$

           if ent_yr$ = " " then goto L50400
                convert ent_yr$ to ent_yr%, data goto L50750
                ent_yr_bi$ = bin(ent_yr%, 2)

/* (AWD004) change from AWDPLN0B to AWDEMP0B */
L50400:    call "AWDEMP0B" ( cur_yr_bi$, /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr_bi$, /* Entry Production Year (IN) */~
                             ent_wk$,    /* Entry Prod Week       (IN) */~
                             ent_dy$,    /* Entry Production Day (OPT) */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #2,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = OK                    */

            if pl_e% <> 0% then goto L50750

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)

            sc_wk_dte$ = ent_dte$
            sc_yr$     = ent_yr$
            sc_wk$     = ent_wk$
            sv_day$    = ent_dy$
            wk% = 0% : i% = 0%
            convert sc_wk$ to wk%, data goto L50750

            convert sv_day$ to i%, data goto L50750

           call "DATEFMT" (sc_wk_dte$)
           if sc_yr$ > cur_yr$ then goto L50820
           if sc_wk$ > cur_wk$ then goto L50780
        return
L50750:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50780:    errormsg$ = "(Error) - Future Production Week,Greater than Equ~
        ~al to ("& cur_wk$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50820:    errormsg$ = "(Error) - Future Production Year,Greater than Equ~
        ~al to ("& cur_yr$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return

L50870: REM Production Day
           if sc_day$ <> " " then goto L50920
              sc_day$ = sv_day$

           gosub L50390                        /* Verify Production Day */
L50920:    convert sc_day$ to x%, data goto L51020

           if x% < 1% or x% > 7% then goto L51020
           if sc_day$ > cur_dy$ then goto L51050

           days$ = days$(x%)
           y% = 0%
           call "DATE" addr("G+", ent_dte$, x%-1%, pt_dte$, y% )
           pt_date$ = pt_dte$
           call "DATEFMT" (pt_date$)
        return
L51020:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           init(" ") sc_day$, days$
        return
L51050:    errormsg$ = "(Error) - Future Production Day,Greater than Equa~
        ~l to (" & cur_dy$ & ")"
           init(" ") sc_day$, days$
        return

L51100: REM Employee Number         PT_NO$   sc_empno$  CR2490
L12345:             FMT BI(4)                /* CR2490  */
           if sc_empno$ <> " " then goto L51130
              goto L51260
L51130:    if str(sc_empno$,1%,1%) <> "A" then goto L51180
              convert str(sc_empno$,2%,7%) to sc_empno%, data goto L51260

              convert sc_empno% to str(sc_empno$,2%,4%), pic(#####000)
              goto L51220
L51180:    convert sc_empno$ to sc_empno%, data goto L51260

REM           convert pt_no% to pt_no$, pic(00000)
           put str(pt_no$,1%,4%) using L12345, sc_empno%
           str(pt_no$,5%,1%) = " "
               
L51220:    gosub load_employee
REM        x$ = "0" & e_dept$
           x$ = e_dept$  /* CR-1894 */
           if sc_dept$ <> x$ then goto L51290
        return
L51260:    errormsg$ = "(Error) - Invalid Employee Number?"
           init(" ") pt_no$, name$, e_dept$, sc_empno$
        return
L51290:    errormsg$ = "(Error) - Departmet Code Conflict??"
           init(" ") pt_no$, name$, e_dept$, sc_empno$
        return

L51330: REM Points Code                                PT_CODE$
            pt_code% = 0%
            convert pt_code$ to pt_code%, data goto L51360
L51360:
            convert pt_code% to pt_code$, pic(0)

            if pt_code% > 3% then goto L51490
               if pt_code$ = "0" then pt_code_d$ = "Late Points        "
               if pt_code$ = "1" then pt_code_d$ = "Misc Points        "
               if pt_code$ = "2" then pt_code_d$ = "Day Points         "
               if pt_code$ = "3" then pt_code_d$ = "Correction to Point"
            if edit% <> 1% then return
               gosub dataload
               if rec% = 0% then return
                  fieldnr% = 9%
        return
L51490:     init(" ") pt_code$, pt_code_d$
            errormsg$ = "(Error) - Invalid Points Selection? "
        return

L51530: REM PointS Description                         PT_REASON$
        return

L51560: REM Points                                     PT_PTS$
            pt_pts = 0.
            convert pt_pts$ to pt_pts, data goto L51590
L51590:
            convert pt_pts to pt_pts$, pic(#####.#-)

            if abs(pt_pts) < .25 or abs(pt_pts) > 1.0 then goto L51640
        return
L51640:     init(" ") pt_pts$
            errormsg$ = "(Error) - Invalid Points? "
        return

        REM *************************************************************~
            *                 R E P O R T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on REPORT 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51830,         /* Beg/End Prod. Date    */ ~
                              L52060,         /* Beg/End Prod. Dept    */ ~
                              L52480,         /* Beg/End Prod. Shift   */ ~
                              L52800          /* EMPLOYEE BEG/END      */

            return

L51830: REM Beginning Prod Date                   BG_DATE$
           init(" ") bg_dte$
           if bg_date$ <> " " then goto L51870
              bg_date$ = date
L51870:    call "DATEOKC" (bg_date$, date%, errormsg$)
           if errormsg$ <> " " then return
           y$ = bg_date$
           call "DATUFMTC" (y$)
           bg_dte$ = str(y$,1%,6%)
           goto L51950                        /* Check Ending Week      */
        return
                                                   /* 2nd Ending       */
L51950: if ed_date$ <> " " then goto L51990
            ed_date$ = bg_date$
            ed_dte$  = bg_dte$
            return
L51990:    call "DATEOKC" (ed_date$, date%, errormsg$)
           if errormsg$ <> " " then return
           y$ = ed_date$
           call "DATUFMTC" (y$)
           ed_dte$ = str(y$,1%,6%)
        return

L52060: REM Beginning Dept Code                   BG_DEPT$
            if bg_dept$ <> " " then goto L52100
               goto L52210

L52100:     pt_dept$ = bg_dept$
            gosub load_dept
            if dept% = 0% then goto L52210
            bg_dept_d$ = dept_d$
            gosub load_access
            if access% = 0% then goto L52190
               return clear all
               goto inputmode

L52190:     goto L52250
        return
L52210:     errormsg$ = "(Error) - Invalid Beginning Department Code?"
            init(" ") bg_dept$, ed_dept$, bg_dept_d$, ed_dept_d$
        return

L52250:     if ed_dept$ <> " " then goto L52290
               ed_dept$ = bg_dept$
               ed_dept_d$ = bg_dept_d$
               return
L52290:     pt_dept$ = ed_dept$
            gosub load_dept
            if dept% = 0% then goto L52390
            ed_dept_d$ = dept_d$
            gosub load_access
            if access% = 0% then goto L52370
               return clear all
               goto inputmode
L52370:     if bg_dept$ <> ed_dept$ then goto L52420
        return
L52390:     errormsg$ = "(Error) - Invalid Ending Department Code?"
            init(" ") bg_dept$, ed_dept$, bg_dept_d$, ed_dept_d$
        return
L52420:     if userid$ = "MVK" or userid$ = "RHH" or userid$ = "LLJ" or  ~
               userid$ = "LBH" or userid$ = "APA" or userid$ = "EDW" or ~
/* EWD002 */   userid$ = "MKN" or userid$ = "LNT"  /*<AWD005>*/      ~
               then return

/* (EWD003) */
        if userid$ = "CGN" or userid$ = "LRN" then return
        
                                              /* (EWD001)           */
            errormsg$ = "(Error) - Only One (1) Dept. at a Time?"
            init(" ") bg_dept$, ed_dept$, bg_dept_d$, ed_dept_d$
        return

L52480: REM Beginning/Ending Shift Code             BG_SHFT$,ED_SHFT$
            if bg_shft$ <> " " then goto L52530
L52500:        bg_shft$   = "AL"
               bg_shft_d$ = "(ALL) - Shifts"
               return
L52530:     if str(bg_shft$,1%,1%) = "A" then goto L52500
            if bg_shft$ < "00" or bg_shft$ > "03" then goto L52600
            pt_shft$ = bg_shft$
            gosub load_shft
            bg_shft_d$ = shft_d$
            goto L52640
        return
L52600:     errormsg$ = "(Error) - Invalid Shift Code?"
            init(" ") bg_shft$, bg_shft_d$, ed_shft$, ed_shft_d$
        return

L52640:     if ed_shft$ <> " " then goto L52680
L52650:        ed_shft$   = bg_shft$
               ed_shft_d$ = bg_shft_d$
               return
L52680:     if str(ed_shft$,1%,1%) = "A" then goto L52650
            if ed_shft$ < "00" or ed_shft$ > "03" then goto L52750
            if bg_shft$ > ed_shft$ then goto L52750
            pt_shft$ = ed_shft$
            gosub load_shft
            ed_shft_d$ = shft_d$
        return
L52750:     errormsg$ = "(Error) - Invalid Shift Code?"
            init(" ") bg_shft$, bg_shft_d$, ed_shft$, ed_shft_d$
        return


L52800: REM Beginning/Ending Employee               BEG_EMP$,END_EMP$
           if beg_emp$ <> " " then goto L52870
L52820:       beg_emp$ = "ALL"
              beg_emp_name$ = "(ALL) Employee's in Dept."
              init(" ") end_emp$, end_emp_name$
              return
                                               /* 1st Check Beginning  */
L52870:    if str(beg_emp$,1%,1%) = "A" then goto L52820
           pt_no$ = beg_emp$
           gosub load_employee
           if errormsg$ <> " " then goto L52960
           x$ = "0" & e_dept$
           if x$ <> sc_dept$ then goto L53000
              beg_emp_name$ = name$
              goto L53050                      /* Check Ending Employee */
        return
L52960:    errormsg$ = "(Error) - Invalid Employee Number?"
           init(" ") beg_emp$, beg_emp_name$, end_emp$, end_emp_name$,   ~
                     e_dept$, name$
        return
L53000:    errormsg$ = "(Error) - Employee Department Conflict??"
           init(" ") beg_emp$, beg_emp_name$, end_emp$, end_emp_name$,   ~
                     e_dept$, name$
        return

L53050:    if end_emp$ <> " " then goto L53100
              end_emp$ = beg_emp$
              end_emp_name$ = beg_emp_name$
              return

L53100:    pt_no$ = end_emp$
           gosub load_employee
           if errormsg$ <> " " then goto L52960
           x$ = "0" & e_dept$
           if x$ <> sc_dept$ then goto L53000
              end_emp_name$ = name$
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
        ~                                                     Shift: ##  !

L55170: %!EmpNo!<------ Name ------>!Week!Day!Code!<------- Description -~
        ~------->!  Date  ! Points ! User Id !<-------- Comments ------->!

L55200: %!#####!####################! ## ! # ! #  !######################~
        ~########!########! ###### !   ###   !                           !

L55230: %!-----!--------------------!----!---!----!----------------------~
        ~--------!--------!--------!---------!---------------------------!

L55260: %!     !                    !    !   !    ! Employee's Point Tota~
        ~l As of !########! ###### !         !                           !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55140, pt_dept$, pt_dept_d$, pt_shft$
           print using L55060
           print using L55170
           lcntr% = 5%
        return

        print_detail
           if lcntr% > 57% and lcntr% <> 99% then gosub print_header
           if save_dept$ = pt_dept$ then goto L60250
              gosub print_total
              save_dept$ = pt_dept$
              save_shft$ = pt_shft$
              gosub print_header
              goto L60270

L60250:    if save_emp$ = pt_no$ then goto L60360
              gosub print_total
L60270:       gosub load_employee
              if save_shft$ <> pt_shft$ then gosub print_header
              save_emp$ = pt_no$

           print using L55230
           print using L55200, pt_no$, name$, pt_wk$, pt_day$, pt_code$,  ~
                              pt_reason$, pt_date$, pt_pts$, pt_usr$
           goto L60390
        REM - Always use after Name Change
L60360:    print using L55230
           print using L55200, " "   , " "  , pt_wk$, pt_day$, pt_code$,  ~
                              pt_reason$, pt_date$, pt_pts$, pt_usr$
L60390:    lcntr% = lcntr% + 2%
           pt_total = round( pt_total + pt_pts, 2)
        return

        print_total
           if lcntr% = 99% then return
           if lcntr% = 5% and pageno% = 1% then return
           convert pt_total to pt_total$, pic(##.##-)

           print using L55230
           print using L55260, date$, pt_total$
           lcntr% = lcntr% + 2%
           pt_total = 0.0
        return

        load_employee
            read #1,key = pt_no$, using L60590, e_dept$, e_lname$,        ~
                          e_fname$, e_init$, e_pts_late, e_pts_mis,      ~
                          e_pts_day, e_pts_tot, e_pts_dte$, pt_shft$,    ~
                          eod goto L60690
L60590:        FMT CH(3), POS(12), CH(15), CH(10), CH(1), POS(284),      ~
                   4*PD(14,4), CH(6), POS(841), CH(2)
            name$ = e_lname$ & "," & str(e_fname$,1%,1%) & "." &         ~
                                                       str(e_init$) & "."
            convert e_pts_late to e_pts_late$, pic(##.##-)
            convert e_pts_mis  to e_pts_mis$ , pic(##.##-)
            convert e_pts_day  to e_pts_day$ , pic(##.##-)
            convert e_pts_tot  to e_pts_tot$ , pic(##.##-)
            call "DATEFMT" (e_pts_dte$)
        return
L60690:     errormsg$ = "(Error) - Invalid Employee Number?"
        return

        update_employee
          read #1,hold,key = pt_no$, eod goto L60870
            get #1, using L60760, e_pts_late, e_pts_mis, e_pts_day,       ~
                                 e_pts_tot, e_pts_dte$
L60760:       FMT POS(284), 4*PD(14,4), CH(6)
          if pt_code$ = "0" then e_pts_late = round(e_pts_late+ pt_pts,2)
          if pt_code$ = "1" then e_pts_mis  = round(e_pts_mis + pt_pts,2)
          if pt_code$ = "2" then e_pts_day  = round(e_pts_day + pt_pts,2)
          if pt_code$ = "3" then e_pts_mis  = round(e_pts_mis + pt_pts,2)
          e_pts_tot = round(e_pts_late + e_pts_mis + e_pts_day, 2)
          e_pts_dte$ = date

          put #1, using L60760, e_pts_late, e_pts_mis, e_pts_day,         ~
                               e_pts_tot, e_pts_dte$
          rewrite #1
L60870: return

        load_access
            if userid$ = "MVK" or userid$ = "MKN" or                   ~
            userid$ = "LNT" then goto L61050         /*<AWD005>*/

/* (EWD003) */
            if userid$ = "CMG" or userid$ = "CGN" then goto L61050
            if userid$ = "RDB" or userid$ = "RBN" then goto L61050
                                             /* (EWD001)               */
            init(" ") access$, xx_dept$      /* Last Two (2) Digits of */
REM         xx_dept$ = str(sc_dept$,2%,2%)   /* of Dept Code for Now   */
            xx_dept$ = str(sc_dept$,1%,3%)   /* CR-1894                */
            j% = 1% : access% = 0%
            str(readkey$,1%,9%)   = "EMPACCESS"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L60990, access$, eod goto L61040
L60990:        FMT POS(25), CH(30)
            for kk% = 1% to 10%
                if xx_dept$ = str(access$,j%,3%) then goto L61050
                j% = j% + 3%
            next kk%
L61040: goto access_denied
L61050: return

        access_denied
            access% = 1%
            comp% = 2%
            hdr$ = "*** Access Denied to Dept ***"
            msg$(1) = "You Do Not Have Access to Dept/Employee Selected?"
            msg$(2) = "      D e p a r t m e n t   S e c u r i t y      "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        generate_report
            rpt% = 1%
            init(" ") pt_key$, save_dept$, save_emp$, save_shft$
            str(pt_key$,1%,3%) = bg_dept$
        generate_next
            read #3,key > pt_key$, using L61240, pt_dte$, pt_key$,        ~
                                                   eod goto generate_done
L61240:        FMT CH(6), CH(16)
            if str(pt_key$,1%,3%) > ed_dept$ then goto generate_done
            if pt_dte$ < bg_dte$ or pt_dte$ > ed_dte$ then               ~
                                                      goto generate_next
            if bg_shft$ = "AL" then goto L61310
               if str(pt_key$,4%,2%) < bg_shft$ or                       ~
                  str(pt_key$,4%,2%) > ed_shft$ then goto generate_next
L61310:     if beg_emp$ = "ALL" then goto scan_data
               if str(pt_key$,11%,5%) < beg_emp$ or                      ~
                  str(pt_key$,11%,5%) > end_emp$ then goto generate_next
        scan_data
            gosub dataload
            gosub print_detail
            goto generate_next

        generate_done
            gosub print_total
            print using L55040
        return

        display_codes
            call "APCPLN1B" (table%, #2)
        return

        load_shft
            init(" ") readkey$, shft_d$
            str(readkey$,1%,9%) = "PLAN SHFT"
            str(readkey$,10%,15%) = pt_shft$
            read #2,key = readkey$, using L61530, shft_d$, eod goto L61540
L61530:        FMT POS(25), CH(30)
L61540: return

        load_dept
            dept% = 0%
            init(" ") readkey$, dept_d$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = pt_dept$
            read #2,key = readkey$, using L61620, dept_d$,eod goto L61640
L61620:         FMT POS(25), CH(30)
            dept% = 1%
L61640: return

        rolodex
                 if rpt% = 0% then call "APCROLSB" (sc_dept$)
                 if rpt% = 1% then call "APCROLSB" (bg_dept$)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
