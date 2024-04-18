        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   EEEEE  M   M  PPPP   EEEEE  DDDD    *~
            *  A   A  P   P  C   C  E      MM MM  P   P  E      D   D   *~
            *  AAAAA  PPPP   C      EEEE   M M M  PPPP   EEEE   D   D   *~
            *  A   A  P      C   C  E      M   M  P      E      D   D   *~
            *  A   A  P       CCC   EEEEE  M   M  P      EEEEE  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCEMPED - New program for editing the Employee Master    *~
            *            file (APCEMPLY). Allows for the Updating,      *~
            *            Editing, and Reporting of Employee Information.*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/01/93 ! Original                                 ! RHH *~
            * 09/10/93 ! Mod - Add Department to the Rolodex      ! RHH *~
            *          !       Screen                             !     *~
            * 03/17/94 ! Mod - Add Shift Code and Lunch Code to   ! RHH *~
            *          !       Second Screen                      !     *~
            * 04/22/96 ! Mod - Add Support for APCEMPPR/APCEMPBN  ! JBF *~
            * 03/25/98 ! Y2K modifications                        ! ERN *~
            * 10/19/98 ! (EWD001) Replaced embedded Rolodex w/call! BWS *~
            *          !       to PROCLINK for APCROLEX.          !     *~
            * 03/10/99 ! (EWD002) Mod Only MVK, LBH, LLJ Can      ! RHH *~
            *          !       delete Employees.                  !     *~
            * 04/09/01 ! (EWD003) Mod Only MVK, MHW, APA, LLJ Can ! CMG *~
            *          !        change employee Status.           !     *~
            * 07/02/01 ! (EWD004) Mod so that can handle 1/2 sick ! CMG *~
            *          !        days.                             !     *~
            * 09/25/01 ! (EWD005) Mod so that ID that can change  ! CMG *~
            *          !        emp status is controled by table  !     *~
            *          !        'EMP STATS' because of mult change!     *~
            * 01/03/03 ! (EWD006) Mod to only allow managers to   ! CMG *~
            *          !            print report.                 !     *~
            * 01/21/05 ! (AWD007) Mod so that can handle 1/2 sick ! CMG *~
            *          !        days.                             !     *~
            * 06/17/05 ! (AWD008) Mod for Security; not displaying! CMG *~
            *          !     certain fields.                      !     *~
            * 06/21/05 ! (AWD009) Add MKN for NE                  ! DES *~
            * 03/06/07 ! (AWD010) Remove lunch codes              ! DES *~
            * 03/15/07 ! (AWD011) Log point changes               ! DES *~
            * 05/20/08 ! (AWD012) 1 1/2 points w/ pay             ! DES *~
            * 10/06/08 ! (AWD013) allow only HR to delete         ! DES *~
            *04/01/2009! (AWD014) reinstall lunch codes           ! DES *~
            *10/07/2009! (AWD015) add EEOC Group                  ! DES *~
            *04/21/2010! (AWD016) security changes                ! DES *~
            *12/11/2013! (AWD017) mod to change vac and sick earn ! CMG *~
            *          !  to allow half day entries               !     *~
            *01/24/2014! (AWD018) mod to move title to bottom.    ! PWW *~
            *          ! Added New Corp Dept # where Title used to!     *~
            *          ! be. Replaced Cur pay Grade with New Job  !     *~
            *          ! code. Converted e_mon_hrs[] to CH(104)to !     *~
            *          ! store the new Job Code & new Corp Dept # !     *~
            *05/28/2014! (AWD019) Add user LNT.                   ! PWW *~
            *08/25/2014! (AWD020) added levels of security        ! CMG *~
            *10/22/2014! (AWD021) mod to move next review date    ! PWW *~
            *          ! to Next Incr field and remove Next Incr  !     *~
            *          ! & Last Review Date to make room for      !     *~
            *          ! Floating Holidays. Also fixed issue with !     *~
            *          ! Sick & Vac days not initialized correctly!     *~
            *01/11/2015! (AWD022) Fix Sec Levels.                 ! PWW *~
            *07/31/2015! SR67402  Add user "EAM" to security.     ! PWW *~
            *09/25/2015! SR69149  Added Termination Code and new  ! PWW *~
            *          !          GENCODES "EMP TREMC".           !     *~
            *01/15/2016! SR71853  Don't show pay info if EMP SECUR! PWW *~
            *          !          has other than "0" flag.        !     *~
            *01/26/2016! SR71971  Allow 6 Floating Holidays.      ! PWW *~
            *          !                                          !     *~
            *08/17/2017! ADP Proj Allow blank marriage status     ! RDB *~ 
            *08/28/2017! Add regina, kim, & remove melisa from del! CMN *~
            *10/04/2017! Changes for ADP                          ! CMN *~
            *01/29/2019! CR-1821 ADP Increase Emp ID field length ! DES *~
            *03/01/2019! CR-1894 Increase EMP DEPT to 3 bytes     ! DES *~
            *05/01/2020! CR2490 Increase employee number size     ! RDB *~
            *06/02/2020! CR2577  Error on title no longer valid   ! RDB *~
            *06/22/2020! CR2611 Correct layout issue from CR1821  ! RDB *~
            *07/01/2020! CR2621 Add C to Company Status - Dayforce! RDB *~
            *************************************************************

        dim valid_fld$(6)47              /* (AWD022) */
        dim                                                              ~
            access$30,hdr$40,msg$(3)79,  /* DEPARTMENT ACCESS          */~
            status$1,                    /* (A)CTIVE,(I)NACTIVE,(T)ERM */~
                                         /* (C)ONTRACTOR      CR2621   */~
            status_d$30,                 /* DESCRIPTION                */~
            readkey$50,                  /* Table Key                  */~
            desc$32,                     /* Table Description          */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            aud_date$5,                  /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(50)1,                  /* Field Attribute Characters */~
            lfac2$(50)1,                 /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            runtime$8,                   /* Run Time for Report        */~
            userid$3                     /* Current User Id            */

        dim                              /* (APCEMPLY) - File          */~
            e_dept$3,e_dept_d$30,        /* Department/Product Line    */~
            e_title$4,e_title_d$30        /* Job Title                 */

        dim                                                              ~
            e_job_code$6,e_job_code_d$30,  /* Emp New Job Code         */~
            e_corp_dept$4,e_corp_dept_d$30,/* Emp New Corp Dept #      */~
/*SR69149*/ e_term_code$3,e_term_code_d$30,/* Emp New Termination Code */~
/*SR69149*/ e_free_area1$7, descr$30,     /* New Free String area     */~
            e_free_area2$68                /* AWD021                   */

        dim                                                              ~
            e_payg$3,e_payg_d$30,        /* Emp Pay Grade              */~
            e_no$5,                      /* Emp Number                 */~
            scre_no$8,                   /* Screen Numeric Entry       */~
            e_lname$15,                  /* Emp Last Name              */~
            e_fname$10,                  /* Emp First Name             */~
            e_init$1,                    /* Emp Middle Initial         */~
            e_addr1$30,                  /* Emp Address Line (1)       */~
            e_addr2$30,                  /* Emp Address Line (2)       */~
            e_city$18,                   /* Emp City                   */~
            e_state$2,                   /* Emp State Code             */~
            e_zip$9,                     /* Emp Zip Code               */~
            e_hacd$3,                    /* Emp Home Area Code         */~
            e_hphone$7,                  /* EMP Home Phone Number      */~
            e_bdte$6, e_bdte1$10,        /* Emp Birth Date             */~
            e_ssn$9,                     /* Emp Social Security No.    */~
            e_status$1,e_status_d$30,    /* Emp (A)ctive,(T)erm,(I)nac */~
                                         /* (C)ontractor   CR2621      */~
            sav_status$1,                /* Emp Save Status    (EWD003)*/~
            e_msd$1,e_msd_d$30,          /* Emp (M)arried,(S)ingle,(D) */~
            e_econtact$30,               /* Emp Emergency Contact      */~
            e_eacd$3,                    /* Emp Emergency Area Code    */~
            e_ephone$7,                  /* Emp Emergency Phone Number */~
            e_hire_dte$6,e_hire_dte1$8,  /* Emp Hire Date              */~
            e_term_dte$6,e_term_dte1$8,  /* Emp Termination Date       */~
            e_lrev_dte$6,e_lrev_dte1$8,  /* Emp Last Review Date       */~
            e_nrev_dte$6,e_nrev_dte1$8,  /* Emp Next Review Date       */~
/*AWD021*/  e_float_dayss$4,             /* Emp Float    Days Acrued   */~
/*AWD021*/  e_float_days$6,e_float_days1$8,/* Emp Float  Days As Of DTE*/~
/*AWD021*/  e_float_usedd$4,             /* Emp Float    Days Used     */~
/*AWD021*/  e_float_used$6,e_float_used1$8,/* Emp Float  Days Used DTE */~
            e_vac_dayss$4,               /* Emp Vacation Days Acrued   */~
            e_vac_days$6,e_vac_days1$8,  /* Emp Vacation Days As Of DTE*/~
/*AWD007*/  e_vac_usedd$4,               /* Emp Vacation Days Used     */~
            e_vac_used$6,e_vac_used1$8,  /* Emp Vacation Days Used DTE */~
            e_sick_dayss$4,              /* Emp Sick Days              */~
            e_sick_days$6,e_sick_days1$8,/* Emp Sick Days as of Date   */~
/*EWD005*/  e_sick_usedd$4,              /* Emp Sick Days Used         */~
            e_sick_used$6,e_sick_used1$8,/* Emp Sick Days Used As Of DT*/~
            e_pay_rate$10,               /* Emp Pay Rate (Current)     */~
            e_pay_rte$6,e_pay_rte1$8,    /* Emp Pay Rate Effective Date*/~
            e_pay_ratep$10,              /* Emp Pay Rate (Previous)    */~
            e_pay_rtep$6,e_pay_rtep1$8,  /* Emp Pay Rate Previous Date */~
            e_pay_rten$6,e_pay_rten1$8,  /* Emp Next Pay Rate Inc Date */~
            e_pts_late$8,                /* Emp Late Points            */~
            e_pts_mis$8,                 /* Emp Miscelaneous Points    */~
            e_pts_day$8,                 /* Emp Points for a Day       */~
            e_pts_tot$8,                 /* Emp Total Points           */~
            e_pts_dte$6,e_pts_dte1$8,    /* Emp Date of Last Points    */~
            e_wk_hrs(52),                /* Emp Weekly Hours Total     */~
            e_mon_hrs(13),               /* Emp Monthly Hours Total    */~
            e_fil1$4,                    /* Filler Area (1)            */~
            e_comm$(4)40,                /* Emp Comments               */~
            e_itime$1, e_itime_d$30,     /* Emp Clock In Time Normal   */~
            e_otime$1, e_otime_d$30,     /* Emp Clock Out Time Normal  */~
            e_display$1,                 /* Emp Clock Display (Y/N)    */~
            e_lunch$1,                   /* DEDUCT LUNCH - 45 MIN      */~
            e_lock$1,                    /* LOCK TERM FOR CLOCK IN     */~
            e_days$7,                    /* WORK DAYS 1 THRU 7         */~
            e_shift$2, e_shift_d$30,     /* Shift Code                 */~
            e_lun$1, e_lunch_d$30,       /* Lunch Code                 */~
            e_days_d$30,                 /* WORK DAYS DESCRIPTION      */~
            msk_hphone$14,               /* FORMAT HOME PHONE          */~
            msk_ephone$14,               /* FORMAT EMERG PHONE         */~
            msk_ssn$11,                  /* FORMAT SSN                 */~
            type$1, type_d$30,           /* Report Type Selection      */~
            sort_type$12,                /* Report SORT                */~
            sel$1, sel_d$40,             /* Report Selection           */~
            beg_value$15,beg_value_d$30, /* Report Selection Value     */~
            end_value$15,end_value_d$30, /* Report Selection Value     */~
            beg_shift$2, beg_shift_d$30, /* Report Selection Shift     */~
            end_shift$2, end_shift_d$30, /* Report Selection Shift     */~
            e_rec$(4)256,                /* Emp Record                 */~
            pay_grade_view$1,            /* SR71853                    */~
            e_pay_status$1               /* Employee Pay Status        */

        dim pr_status$1,                 /* APCEMPPR Status AWD008     */~
            sec_lvl$1                    /* Level Of Security          */

        dim f2%(7),                      /* = 0 if the file is open    */~
            f1%(7),                      /* = 1 if READ was successful */~
            fs%(7),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(7)20                   /* Text from file opening     */

        dim field$32, from$32, to$32, aud_key$8, aud_key2$8
        
        dim hold_dte$6, hold_late$8, hold_day$8, hold_tot$8, hold_mis$8

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 04/01/93 Employee Master File Edit Util."
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
            * #01 ! APCEMPLY ! Employee Master File                     *~
            * #02 ! GENCODES ! Control System Codes File                *~
            * #03 ! APCEMPPR ! Employee Payroll Detail File AWD008      *~
            * #04 ! APCAUDLG ! Employee Payroll Changes Audit Log AWD011*~
            * #05 ! APCEMPEX ! Employee Master Shadow File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos = 7,    keylen =  5,                      ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos =   12, keylen =  26, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3,  "APCEMPPR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos = 1,    keylen =  5

            select #4,  "APCAUDLG",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  8

            select #5,  "APCEMPEX",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen =  5

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),200%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02),      0%,  0%, rslt$(02))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 100%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 500%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%), f2%(5%),200%, rslt$(5%))

            f1%(1), f1%(2%), f1%(3%) = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            aud_date$ = date
            call "DATUFMTC" (aud_date$)
            date$ = date
            time$ = TIME
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

* EWD001    H1$ = "EMP NO."
*  BEGIN    H2$ = "LAST NAME      "
*           H3$ = "FIRST NAME"
*           H4$ = "I"
*           H5$ = "HOME PHONE    "
* EWD001    H6$ = "EMERG PHONE   "
*  END      H7$ = " DEPART. "

REM  GOSUB LOAD_SECURITY
REM  GOSUB LOAD_SECURITY_HR                /* (AWD008)  */
            sec_lvl$ = "0"

            e_days_d$ = "MON,TUE,WED,THR,FRI,SAT,SUN"

          /* sec_lvl$   0 = none (higher = more access)      */
          /*            1 = EMPACCESS (if dept OK)           */
          /*            2 = EMP SECUR                        */
          /*            3 = EMP GEN                          */
          /*            4 = EMP HR                           */
          /*            5 = user DELETE ACCESS               */
REM         security level is the subscript for the valid function array.
REM         by definition, security level 0 has not authorization.
REM         valid_fld$(2) = "     x xxxxxx xxxx     x              xxxxx"
REM         valid_fld$(0) = "....+....1....+....2....+....3....+....4..."
/*AWD022*/  valid_fld$(1) = "                                      xxxxxxxxx"
            valid_fld$(2) = "                                       xxxxxxxx"
            valid_fld$(3) = "xxxxxxxxxxxxx xxxx     x              xxxxxxxxx"
            valid_fld$(4) = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
            valid_fld$(5) = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            for fieldnr% = 1% to  47%
L10110:         gosub'051(fieldnr%,1%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 14% then gosub print_report
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%,1%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130: REM FIELDNR% = CURSOR%(1%) - 2%
            gosub find_field
            if fieldnr% < 1% or fieldnr% > 47% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%,2%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             C A L C   F I E L D   N U M B E R             *~
            *-----------------------------------------------------------*~
            * EDIT DATA                                                 *~
            *************************************************************

        find_field
            row% = cursor%(1%) : col% = cursor%(2%)

            if row% > 3% then goto L12140
               if col% < 40% then fieldnr% = 1%
               if col% > 39% then fieldnr% = 2%
               return

L12140:     if row% > 4% then goto L12190
               if col% < 42% then fieldnr% = 3%
               if col% > 41% and col% < 53% then fieldnr% = 4%
               if col% > 52% then fieldnr% = 5%
               return
L12190:     if row% > 5% then goto L12220
               fieldnr% = 6%
               if col% > 60% then fieldnr% = 7%
               return

L12220:     if row% > 6% then goto L12250
               fieldnr% = 8% : return

L12250:     if row% > 7% then goto L12310
               if col% < 45% then fieldnr% = 9%
               if col% > 44% and col% < 48% then fieldnr% = 10%
               if col% > 47% then fieldnr% = 11%
               return

L12310:     if row% > 8% then goto L12380
               if col% < 29% then fieldnr% = 12%
               if col% > 28% and col% < 40% then fieldnr% = 13%
               if col% > 39% and col% < 62% then fieldnr% = 14%
               if col% > 61% then fieldnr% = 15%
               return

L12380:     if row% > 9% then goto L12440
               if col% < 29% then fieldnr% = 16%
               if col% > 28% and col% < 40% then fieldnr% = 17%
               if col% > 39% then fieldnr% = 18%
               return

L12440:     if row% > 10% then goto L12470
               fieldnr% = 19% : return

L12470:     if row% > 11% then goto L12500
               fieldnr% = 20% : return

L12500:     if row% > 12% then goto L12550
               if col% < 40% then fieldnr% = 21%
               if col% > 39% then fieldnr% = 22%
               return

L12550:     if row% > 13% then goto L12600
/*AWD021 */    if col% < 28% then fieldnr% = 23%
               if col% > 27% and col% < 40% then fieldnr% = 24%
               if col% > 39% and col% < 66% then fieldnr% = 25%
/*AWD021 */    if col% > 65% then fieldnr% = 26%
               return

L12600:     if row% > 14% then goto L12670
               if col% < 28% then fieldnr% = 27%
               if col% > 27% and col% < 40% then fieldnr% = 28%
               if col% > 39% and col% < 66% then fieldnr% = 29%
               if col% > 65% then fieldnr% = 30%
               return

L12670:     if row% > 15% then goto L12740
               if col% < 28% then fieldnr% = 31%
               if col% > 27% and col% < 40% then fieldnr% = 32%
               if col% > 39% and col% < 66% then fieldnr% = 33%
               if col% > 65% then fieldnr% = 34%
               return

L12740:     if row% > 16% then goto L12770
               fieldnr% = 35% : return

L12770:     if row% > 17% then goto L12840
/*SR71853*/    if pay_grade_view$ = "N" then goto skip_pay_fields
               if col% < 30% then fieldnr% = 36%
               if col% > 30% and col% < 41% then fieldnr% = 37%
               if col% > 40% and col% < 71% then fieldnr% = 38%
               if col% > 70% then fieldnr% = 39%
        skip_pay_fields                             /*SR71853 */
               return

L12840:     if row% > 18% then goto L12890
               if col% < 41% then fieldnr% = 40%
               if col% > 40% then fieldnr% = 41%
               return

L12890:     if row% > 19% then goto L12891
               if col% < 24% then fieldnr% = 42%
               if col% > 23% and col% < 40% then fieldnr% = 43%
               if col% > 39% and col% < 55% then fieldnr% = 44%
               if col% > 54% then fieldnr% = 45%
               return
               
L12891:     if row% > 20% then return
               if col% < 43% then fieldnr% = 46%                         ~
                             else fieldnr% = 47%   /*SR69149 */

        return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%, edit%)
            enabled% = 0%
            if fieldnr% = 1 then enabled% = 1%
REM IF EDIT% = 2% THEN RETURN
            if sec_lvl$  = "0" then return   /* AWD016 */
            if sec_lvl$  = "1" and           /* AWD016 */       ~
               str(valid_fld$(1),fieldnr%,1%) <> "x" then return
            if sec_lvl$  = "2" and           /* AWD016 */       ~
               str(valid_fld$(2),fieldnr%,1%) <> "x" then return
            if sec_lvl$  = "3" and           /* AWD016 */       ~
               str(valid_fld$(3),fieldnr%,1%) <> "x" then return
            if sec_lvl$  = "4" and           /* AWD016 */       ~
               str(valid_fld$(4),fieldnr%,1%) <> "x" then return
            if sec_lvl$  = "5" and           /* AWD016 */       ~
               str(valid_fld$(5),fieldnr%,1%) <> "x" then return
REM IF SEC_LVL$  = "1" AND           /* AWD016 */       ~
REM FIELDNR% < 39% THEN RETURN
REM VALID_FLD$(2) = "     XXXXXXXX          X              XXXXX"
REM IF SEC_LVL$  = "2" AND           /* AWD016 */       ~
REM FIELDNR% < 39% AND FIELDNR% <> 24% THEN RETURN

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
         "Enter Employee Number.                                       ",~
         "Enter Employee Department or Product Line Code.              ",~
         "Enter Employee Last Name.                                    ",~
         "Enter Employee First Name.                                   ",~
         "Enter Employee Middle Initial.                               ",~
         "Enter Employee Address Line (1).                             ",~
         "Enter Employee Corporate Department Number.                  ",~
         "Enter Employee Address Line (2).                             ",~
         "Enter Employee City.                                         ",~
         "Enter Employee State Code.                                   ",~
         "Enter Employee Zip Code.                                     ",~
         "Enter Employee Home Phone Area Code.                         ",~
         "Enter Employee Home Phone Number.                            ",~
         "Enter Employee Birth Date.                                   ",~
         "Enter Employee Social Security Number.                       ",~
         "Enter Employee Emergency Phone Area Code.                    ",~
         "Enter Employee Emergency Phone Number.                       ",~
         "Enter Employee Emergency Contact.                            ",~
         "Enter Employee Family Status (M)arried,(S)ingle,(D)ivorced.  ",~
/* CR2621  "Enter Employee Company Status(A)ctive,(I)nactive,(T)erminated",*/~
         "Enter Employee Company Status(A)ctive,(I)nactive,(T)erminated,(C)ontractor",~
         "Enter Employee Hire Date.                                    ",~
         "Enter Employee Termination Date.                             ",~
         "Enter Employee Floating Days Acrued.                         ",~
         "Enter Employee Floading Days as of Date.                     ",~
         "Enter Employee Floating Days Used.                           ",~
         "Enter Employee Floating Days Used as of Date.                ",~
         "Enter Employee Vacation Days Acrued.                         ",~
         "Enter Employee Vacation Days as of Date.                     ",~
         "Enter Employee Vacation Days Used.                           ",~
         "Enter Employee Vacation Days Used as of Date.                ",~
         "Enter Employee Sick Days Acrued.                             ",~
         "Enter Employee Sick Days as of Date.                         ",~
         "Enter Employee Sick Days Used.                               ",~
         "Enter Employee Sick Days Used as of Date.                    ",~
         "Enter Employee Job Code.                                     ",~
         "Enter Employee Current Pay Rate.                             ",~
         "Enter Employee Current Pay Rate Date.                        ",~
         "Enter Employee Previous Pay Rate.                            ",~
         "Enter Employee Previous Pay Rate Date.                       ",~
         "Enter Employee Next Review Date.                             ",~
         "Enter Employee Date of Last Points.                          ",~
         "Enter Employee Late Points.                                  ",~
         "Enter Employee Misc. Points.                                 ",~
         "Enter Employee Day Type Points.                              ",~
         "Enter Employee Total Points.                                 ",~
         "Enter Employee Tempoary Job Title.                           ",~
         "Enter Employee Termination Code.                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, emp_key$, e_dept$,         ~
                      e_dept_d$, e_payg$, e_payg_d$, e_no$, e_lname$,    ~
/* </AWD018> */       e_title$, e_title_d$,e_job_code$,e_job_code_d$,    ~
                      e_free_area1$,e_corp_dept$,e_corp_dept_d$,         ~
/* </AWD018> */       e_free_area2$,e_term_code$,e_term_code_d$,         ~
                      e_fname$, e_init$, e_addr1$, e_addr2$, e_city$,    ~
                      e_state$, e_zip$, e_hacd$, e_hphone$, e_bdte$,     ~
                      e_bdte1$, e_ssn$, e_status$, e_msd$, e_econtact$,  ~
                      e_eacd$, e_ephone$, e_hire_dte$, e_hire_dte1$,     ~
                      e_term_dte$, e_term_dte1$, e_lrev_dte$,            ~
                      e_lrev_dte1$, e_nrev_dte$, e_nrev_dte1$,           ~
/*AWD021*/            e_float_dayss$, e_float_days$, e_float_days1$,     ~
/*AWD021*/            e_float_usedd$, e_float_used$, e_float_used1$,     ~
                      e_vac_dayss$, e_vac_days$, e_vac_days1$,           ~
                      e_vac_usedd$, e_vac_used$, e_vac_used1$,           ~
                      e_sick_dayss$, e_sick_days$, e_sick_days1$,        ~
                      e_sick_usedd$, e_sick_used$, e_sick_used1$,        ~
                      e_pay_rate$, e_pay_rte$, e_pay_rte1$, e_pay_ratep$,~
                      e_pay_rtep$, e_pay_rtep1$, e_pay_rten$,            ~
                      e_pay_rten1$, e_pts_late$, e_pts_mis$, e_pts_day$, ~
                      e_pts_tot$, e_pts_dte$, e_pts_dte1$,               ~
                      e_title$, e_comm$(), e_days$, e_rec$(), e_msd_d$,  ~
                      e_status_d$, beg_value$, beg_value_d$, end_value$, ~
                      end_value_d$, sel$, sel_d$, desc$, type$, type_d$, ~
                      e_pay_status$, status$, status_d$, e_itime$,       ~
                      e_itime_d$, e_otime$, e_otime_d$, e_display$,      ~
                      e_lunch$, e_lock$, access$, e_shift$, e_lun$,      ~
                      beg_shift$, beg_shift_d$, end_shift$, end_shift_d$,~
                      sav_status$, pr_status$, scre_no$ 
/* CR2490 */
                      
           mat e_wk_hrs  = zer
           mat e_mon_hrs = zer
           rpt%, rec% = 0%
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
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
            sav_pay_rate, sav_pay_ratep = 0.0
            rec% = 0%
            if rpt% = 1% then goto L30110                /* REPORT MODE */
               read #1,key = e_no$, eod goto L30930

L30110:     get #1, using L35060, e_dept$, e_payg$, e_no$, e_lname$,         ~
                    e_fname$, e_init$, e_addr1$, e_addr2$, e_city$, e_state$,~
                    e_zip$, e_hacd$, e_hphone$, e_bdte$, e_ssn$, e_status$,  ~
                    e_msd$, e_econtact$, e_eacd$, e_ephone$, e_hire_dte$,    ~
                    e_term_dte$, e_lrev_dte$, e_nrev_dte$, e_vac_days%,      ~
                    e_vac_days$, e_vac_used%, e_vac_used$, e_sick_days%,     ~
                    e_sick_days$, e_sick_used%, e_sick_used$, e_pay_rate,    ~
                    e_pay_rte$, e_pay_ratep, e_pay_rtep$, e_pay_rten$,       ~
                    e_pts_late, e_pts_mis, e_pts_day, e_pts_tot, e_pts_dte$, ~
                    e_wk_hrs(), e_job_code$, e_corp_dept$, e_term_code$,     ~
                    e_free_area1$, e_float_days%, e_float_days$,             ~
                    e_float_used%, e_float_used$, e_free_area2$, e_shift$,   ~
                    e_lun$, e_pay_status$, e_fil1$, e_title$, e_comm$(),     ~
                    e_itime$, e_otime$, e_display$, e_lunch$, e_lock$, e_days$

            gosub check_security                /* AWD016 */
            gosub edit_page_2                   /* Page Two Data       */
            gosub lookup_dept                   /* Department Code     */
            gosub lookup_title                  /* Department Code     */
            gosub lookup_corp_dept              /* New Corp Dept Code  */
/*SR69149*/ gosub lookup_term_code              /* New Termination Code*/
            gosub lookup_job_code               /* New Job Code       */
            
            gosub L51910                         /* Family Status Code  */
            gosub L52040                         /* Company Status Code */
            gosub load_pr_status                 /* AWD008              */
                                                /* Birth Date Convert  */
            if sec_lvl$ > "2" then                        ~
            e_bdte1$ = e_bdte$           : call "DATFMTC" (e_bdte1$)
                                                /* Hire Date Convert   */
            e_hire_dte1$ = e_hire_dte$   : call "DATEFMT" (e_hire_dte1$)
                                                /* Termination Date    */
            e_term_dte1$ = e_term_dte$   : call "DATEFMT" (e_term_dte1$)
                                                /* Last Review Date    */
            e_lrev_dte1$ = e_lrev_dte$   : call "DATEFMT" (e_lrev_dte1$)
                                                /* Next Review Date    */
            e_nrev_dte1$ = e_nrev_dte$   : call "DATEFMT" (e_nrev_dte1$)
/*AWD021 + */
                                                   /* Float    Days Acrued*/
            e_float_days = 0.0                                /* (AWD017) */
            e_float_days = e_float_days%                       /* (AWD017) */
            e_float_days = (e_float_days / 10)                 /* (AWD017) */
            convert e_float_days to e_float_dayss$, pic(00.0)  /* (AWD017) */


REM CONVERT E_FLOAT_DAYS% TO E_FLOAT_DAYSS$, PIC(00)
            e_float_days1$ = e_float_days$   : call "DATEFMT" (e_float_days1$)
                                                /* Float    Days Used  */
REM CONVERT E_FLOAT_USED% TO E_FLOAT_USEDD$, PIC(00) /* (AWD007) */
            e_float_used = 0.0                                /* (AWD007) */
            e_float_used = e_float_used%                       /* (AWD007) */
            e_float_used = (e_float_used / 10)                 /* (AWD007) */
            convert e_float_used to e_float_usedd$, pic(00.0)  /* (AWD007) */

            e_float_used1$ = e_float_used$   : call "DATEFMT" (e_float_used1$)
/*AWD021 - */
                                                /* Vacation Days Acrued*/
            e_vac_days = 0.0                                /* (AWD017) */
            e_vac_days = e_vac_days%                       /* (AWD017) */
            e_vac_days = (e_vac_days / 10)                 /* (AWD017) */
            convert e_vac_days to e_vac_dayss$, pic(00.0)  /* (AWD017) */


REM CONVERT E_VAC_DAYS% TO E_VAC_DAYSS$, PIC(00)
            e_vac_days1$ = e_vac_days$   : call "DATEFMT" (e_vac_days1$)
                                                /* Vacation Days Used  */
REM CONVERT E_VAC_USED% TO E_VAC_USEDD$, PIC(00)   /* (AWD007) */
            e_vac_used = 0.0                                /* (AWD007) */
            e_vac_used = e_vac_used%                       /* (AWD007) */
            e_vac_used = (e_vac_used / 10)                 /* (AWD007) */
            convert e_vac_used to e_vac_usedd$, pic(00.0)  /* (AWD007) */

            e_vac_used1$ = e_vac_used$   : call "DATEFMT" (e_vac_used1$)

                                                /* Sick Days Acrued    */
            e_sick_days = 0.0                                /* (AWD017) */
            e_sick_days = e_sick_days%                       /* (AWD017) */
            e_sick_days = (e_sick_days / 10)                 /* (AWD017) */
            convert e_sick_days to e_sick_dayss$, pic(00.0)  /* (AWD017) */

REM CONVERT E_SICK_DAYS% TO E_SICK_DAYSS$, PIC(00)
            e_sick_days1$ = e_sick_days$ : call "DATEFMT" (e_sick_days1$)
                                                /* Sick Days Used      */
            e_sick_used = 0.0                                /* (EWD005) */
            e_sick_used = e_sick_used%                       /* (EWD005) */
            e_sick_used = (e_sick_used / 10)                 /* (EWD005) */
            convert e_sick_used to e_sick_usedd$, pic(00.0)  /* (EWD005) */

            e_sick_used1$ = e_sick_used$ : call "DATEFMT" (e_sick_used1$)
                                                /* Pay Rate Current    */
                                                /* AND PREVIOUS WITH   */
REM IF SECURITY_HR% >= 1% THEN GOTO L30610   /* SECURITY            */
            sav_pay_rate = e_pay_rate
            sav_pay_ratep = e_pay_ratep

            goto L30610
            if sec_lvl$ <> "2" then goto L30610   /* AWD016              */
               e_pay_rate = 0.0
               e_pay_ratep = 0.0
                                                    /* (AWD008)  - BEG */
L30610:
REM IF USERID$ <> "MVK" AND USERID$ <> "CMG" THEN GOTO NO_PAY_INFO
REM IF SECURITY_HR% = 0% AND (PR_STATUS$ = "E" OR PR_STATUS$ = "N")
/*SR71853*/ if pay_grade_view$ = "N" then goto no_pay_info
            if sec_lvl$ = "0" and (pr_status$ = "E" or pr_status$ = "N") ~
                               then goto no_pay_info
            convert e_pay_rate to e_pay_rate$, pic(######.##-)
            if sec_lvl$ > "2" then                        ~
            e_pay_rte1$ = e_pay_rte$ : call "DATEFMT" (e_pay_rte1$)
                                                /* Pay Rate Previous   */
            convert e_pay_ratep to e_pay_ratep$, pic(######.##-)
            if sec_lvl$ > "2" then                        ~
            e_pay_rtep1$ = e_pay_rtep$ : call "DATEFMT" (e_pay_rtep1$)
no_pay_info

                                                    /* (AWD008)  - END */
                                                /* Next Pay Inc Date   */
            e_pay_rten1$ = e_pay_rten$ : call "DATEFMT" (e_pay_rten1$)
                                                /* Points Conversion   */
            convert e_pts_late to e_pts_late$, pic(####.##-)
            convert e_pts_mis  to e_pts_mis$ , pic(####.##-)
            convert e_pts_day  to e_pts_day$ , pic(####.##-)
            convert e_pts_tot  to e_pts_tot$ , pic(####.##-)
                                                /* Date of Last Points */
            e_pts_dte1$ = e_pts_dte$ : call "DATEFMT" (e_pts_dte1$)

            hold_dte$  = e_pts_dte$
            hold_late$ = e_pts_late$
            hold_mis$  = e_pts_mis$
            hold_day$  = e_pts_day$
            hold_tot$  = e_pts_tot$

            sav_status$ = e_status$
            msk_hphone$ = "(   )    -    "
            msk_ephone$ = "(   )    -    "
            msk_ssn$    = "   -  -    "
            if len(e_hacd$) = 0 then goto L30800
               str(msk_hphone$,2%,3%)  = e_hacd$
L30800:     if len(e_hphone$) = 0 then goto L30830
               str(msk_hphone$,7%,3%)  = str(e_hphone$,1%,3%)
               str(msk_hphone$,11%,4%) = str(e_hphone$,4%,4%)
L30830:     if len(e_eacd$) = 0 then goto L30850
               str(msk_ephone$,2%,3%)  = e_eacd$
L30850:     if len(e_ephone$) = 0 then goto L30880
               str(msk_ephone$,7%,3%)  = str(e_ephone$,1%,3%)
               str(msk_ephone$,11%,4%) = str(e_ephone$,4%,4%)
L30880:     if len(e_ssn$) = 0 then goto L30920
            hold_ssn$ = e_ssn$
            if sec_lvl$ < "3" then e_ssn$ = "         "
               str(msk_ssn$,1%,3%)  = str(e_ssn$,1%,3%)
               str(msk_ssn$,5%,2%)  = str(e_ssn$,4%,2%)
               str(msk_ssn$,8%,4%)  = str(e_ssn$,6%,4%)
L30920:     rec% = 1%
L30930: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_it
/* <AWD013> */
            if sec_lvl$ <> "5" then goto L31100

            comp% = 2%
            hdr$ = " *** Delete Verification *** "
            msg$(1) = "Are you sure you want to Delete this Employee?   "
            msg$(2) = "                                                 "
            msg$(3) = "Press <F14> to Delete or Any PF Key To Continue. "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
           
            if comp% <> 14% then goto L31390
            access% = 1%
            goto L31120

/* </AWD013> */

L31100:       gosub delete_msg
              goto L31390

L31120: dataput                             /* EWD003 - ADD NEXT 5 Statements */
REM IF USERID$ = "MVK" OR USERID$ = "DRH" THEN GOTO L31130
REM IF USERID$ = "LLJ" OR USERID$ = "APA" THEN GOTO L31130
REM IF USERID$ = "RAD" OR USERID$ = "SP1" THEN GOTO L31130
            gosub emp_stats                 /*  EWD005  - Check table for ID */
            if emp_stats% = 1% then goto L31130           /*  EWD005         */

               if rec% = 0% then goto L31130
               e_status$ = sav_status$
L31130:
            read #1,hold,key = e_no$, eod goto L31170
               delete #1

L31170:     if keyhit% = 12% then goto L31390
     
REM IF SECURITY_HR% >= 1% THEN GOTO L31210
            if sec_lvl$ < "3" then e_ssn$ = hold_ssn$
            if sec_lvl$ > "3" then goto L31210   /* AWD016 */
               e_pay_rate  = sav_pay_rate
               e_pay_ratep = sav_pay_ratep

L31210:     put #1, using L35060, e_dept$, e_payg$, e_no$, e_lname$,         ~
                    e_fname$, e_init$, e_addr1$, e_addr2$, e_city$, e_state$,~
                    e_zip$, e_hacd$, e_hphone$, e_bdte$, e_ssn$, e_status$,  ~
                    e_msd$, e_econtact$, e_eacd$, e_ephone$, e_hire_dte$,    ~
                    e_term_dte$, e_lrev_dte$, e_nrev_dte$, e_vac_days%,      ~
                    e_vac_days$, e_vac_used%, e_vac_used$, e_sick_days%,     ~
                    e_sick_days$, e_sick_used%, e_sick_used$, e_pay_rate,    ~
                    e_pay_rte$, e_pay_ratep, e_pay_rtep$, e_pay_rten$,       ~
                    e_pts_late, e_pts_mis, e_pts_day, e_pts_tot, e_pts_dte$, ~
                    e_wk_hrs(), e_job_code$, e_corp_dept$, e_term_code$,     ~
                    e_free_area1$, e_float_days%, e_float_days$,             ~
                    e_float_used%, e_float_used$, e_free_area2$, e_shift$,   ~
                    e_lun$, e_pay_status$, e_fil1$, e_title$, e_comm$(),     ~
                    e_itime$, e_otime$, e_display$, e_lunch$, e_lock$, e_days$

     
            gosub LOG_CHANGES                   /* <AWD011> */

            write #1, eod goto L31420
L31390: return clear all
        goto inputmode

L31420:     stop "(ERROR) - WHILE UPDATING ----> " & e_no$
        return clear all
        goto inputmode

        REM *************************************************************~
            *              F O R M A T   S T A T E M E N T S            *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

L35060: FMT                              /* (APCEMPLY) - File          */~
            CH(03),                      /* Department/Product Line    */~
            CH(03),                      /* Emp Pay Grade              */~
            CH(05),                      /* Emp Number                 */~
            CH(15),                      /* Emp Last Name              */~
            CH(10),                      /* Emp First Name             */~
            CH(01),                      /* Emp Middle Initial         */~
            CH(30),                      /* Emp Address Line (1)       */~
            CH(30),                      /* Emp Address Line (2)       */~
            CH(18),                      /* Emp City                   */~
            CH(02),                      /* Emp State Code             */~
            CH(09),                      /* Emp Zip Code               */~
            CH(03),                      /* Emp Home Area Code         */~
            CH(07),                      /* EMP Home Phone Number      */~
            CH(06),                      /* Emp Birth Date             */~
            CH(09),                      /* Emp Social Security No.    */~
            CH(01),                      /* Emp (A)ctive,(T)erm,(I)nac */~
                                         /* (C)ontractor   CR2621      */~
            CH(01),                      /* Emp (M)arried,(S)ingle,(D) */~
            CH(30),                      /* Emp Emergency Contact      */~
            CH(03),                      /* Emp Emergency Area Code    */~
            CH(07),                      /* Emp Emergency Phone Number */~
            CH(06),                      /* Emp Hire Date              */~
            CH(06),                      /* Emp Termination Date       */~
            CH(06),                      /* Emp Last Review Date       */~
            CH(06),                      /* Emp Next Review Date       */~
            BI(02),                      /* Emp Vacation Days Acrued   */~
            CH(06),                      /* Emp Vacation Days As Of DTE*/~
            BI(02),                      /* Emp Vacation Days Used     */~
            CH(06),                      /* Emp Vacation Days Used DTE */~
            BI(02),                      /* Emp Sick Days              */~
            CH(06),                      /* Emp Sick Days as of Date   */~
            BI(02),                      /* Emp Sick Days Used         */~
            CH(06),                      /* Emp Sick Days Used As Of DT*/~
            PD(14,4),                    /* Emp Pay Rate (Current)     */~
            CH(06),                      /* Emp Pay Rate Effective Date*/~
            PD(14,4),                    /* Emp Pay Rate (Previous)    */~
            CH(06),                      /* Emp Pay Rate Previous Date */~
            CH(06),                      /* Emp Next Pay Rate Inc Date */~
            PD(14,4),                    /* Emp Late Points            */~
            PD(14,4),                    /* Emp Miscelaneous Points    */~
            PD(14,4),                    /* Emp Points for a Day       */~
            PD(14,4),                    /* Emp Total Points           */~
            CH(06),                      /* Emp Date of Last Points    */~
            52*PD(14,4),                 /* Emp Weekly Hours Total     */~
/*AWD021*/  CH(06),                      /* Emp Job Code               */~
/*AWD021*/  CH(04),                      /* Emp Corp Dept              */~
/*SR69149*/ CH(03),                      /* Emp Termination Code       */~
/* CR2611 fix layout issue caused with CR1821   -----  change 07 to 06 */~
/*SR69149*/ CH(06),                      /* New Free Area1             */~
/*AWD021*/  BI(02),                      /* Emp Float    Days Acrued   */~
/*AWD021*/  CH(06),                      /* Emp Float    Days As Of DTE*/~
/*AWD021*/  BI(02),                      /* Emp Float    Days Used     */~
/*AWD021*/  CH(06),                      /* Emp Float    Days Used DTE */~
                                                                         ~
                                                                         ~
/*AWD021*/  CH(68),                      /* New Free Area2             */~
            CH(2),                       /* Emp Shift Code             */~
            CH(1),                       /* Emp Lunch Code             */~
            CH(1),                       /* Emp Pay Status             */~ 
            CH(4),                       /* Filler                     */~
            CH(4),                       /* Emp Title                  */~
            4*CH(40),                    /* Emp Comments               */~
            CH(01),                      /* Emp Clock In Time Code     */~
            CH(01),                      /* Emp Clock Out Time Code    */~
            CH(01),                      /* Time Clock Display Code    */~
            CH(01),                      /* DEDUCT 45 MIN LUNCH Y/N    */~
            CH(01),                      /* LOCK TERMINAL Y/N          */~
            CH(07)                       /* Days of the Week Y or N    */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)

              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40580,         /* Employee Number   */   ~
                                L40580,         /* Department Code   */   ~
                                L40570,         /* Last Name         */   ~
                                L40570,         /* First Name        */   ~
                                L40570,         /* Middle Init       */   ~
                                L40570,         /* Addr1             */   ~
                                L40570,         /* Title             */   ~
                                L40570,         /* Addr2             */   ~
                                L40570,         /* City              */   ~
                                L40570,         /* State             */   ~
                                L40580,         /* Zip Code          */   ~
                                L40580,         /* Home Area Code    */   ~
                                L40580,         /* Home Phone No.    */   ~
                                L40570,         /* Birth Date        */   ~
                                L40580,         /* Social Sec. No.   */   ~
                                L40580,         /* Emergency Area Cd */   ~
                                L40580,         /* Emergency Phone No*/   ~
                                L40570,         /* Emergency Contact */   ~
                                L40570,         /* Family Status     */   ~
                                L40570,         /* Company Status    */   ~
                                L40570,         /* Hire Date (20)    */   ~
                                L40570,         /* Termination Date  */   ~
/*AWD021*/                      L40580,         /* Float    Days Acr */   ~
/*AWD021*/                      L40570,         /* Float    Days DTE */   ~
/*AWD021*/                      L40580,         /* Float    Days Used*/   ~
/*AWD021*/                      L40570,         /* Float    Used DTE */   ~
                                L40580,         /* Vacation Days Acr */   ~
                                L40570,         /* Vacation Days DTE */   ~
                                L40580,         /* Vacation Days Used*/   ~
                                L40570,         /* Vacation Used DTE */   ~
                                L40580,         /* Sick Days         */   ~
                                L40570,         /* Sick Days Date    */   ~
                                L40580,         /* Sick Days Used    */   ~
                                L40570,         /* Sick Days Used DTE*/   ~
                                L40570,         /* Employee Job Code */   ~
                                L40570,         /* Current Pay Rate  */   ~
                                L40570,         /* Curr Pay Rate Date*/   ~
                                L40570,         /* Prev Pay Rate (35)*/   ~
                                L40570,         /* Prev Pay Rate Date*/   ~
/*AWD021*/                      L40570,         /* Next Review Date  */   ~
                                L40570,         /* Date of Last Pts  */   ~
                                L40580,         /* Points Late       */   ~
                                L40580,         /* Points Misc.      */   ~
                                L40580,         /* Points Mid Day    */   ~
                                L40580,         /* Points Total      */   ~
                                L40570,         /* Temporary Job Title*/  ~
/*SR69149 */                    L40570          /* Termination Code   */
              goto L40600

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40570:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40580:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40600: REM ACCEPT_DISPLAY
            gosub set_pf1
/* CR2490 employee number increase */
            accept                                                       ~
               at (01,02),                                               ~
                  "Employee Master File Edit Utility - Page (1)",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Employee Number     :",                      ~
               at (03,25), fac(lfac$( 1)), scre_no$             , ch(08),~
               at (03,40), "Dept:",                                      ~
               at (03,46), fac(lfac$( 2)), e_dept$              , ch(03),~
               at (03,50), fac(hex(84)), e_dept_d$              , ch(29),~
                                                                         ~
               at (04,02), "Name (Last,First,I.):",                      ~
               at (04,25), fac(lfac$( 3)), e_lname$             , ch(15),~
/*ADP*/        at (04,42), fac(lfac$( 4)), e_fname$             , ch(15),~
               at (04,58), fac(lfac$( 5)), e_init$              , ch(01),~
                                                                         ~
               at (05,02), "Address Line (1)    :",                      ~
               at (05,25), fac(lfac$( 6)), e_addr1$             , ch(30),~
                                                                         ~
               at (05,56), "Corp Dept No.:",                             ~
               at (05,71), fac(lfac$( 7)), e_corp_dept$         , ch(04),~
                                                                         ~
               at (06,02), "Address Line (2)    :",                      ~
               at (06,25), fac(lfac$( 8)), e_addr2$             , ch(30),~
               at (06,58), fac(hex(84)), e_corp_dept_d$         , ch(20),~
                                                                         ~
               at (07,02), "City, State, Zip    :",                      ~
               at (07,25), fac(lfac$( 9)), e_city$              , ch(18),~
               at (07,45), fac(lfac$(10)), e_state$             , ch(02),~
               at (07,48), fac(lfac$(11)), e_zip$               , ch(09),~
                                                                         ~
               at (08,02), "Home Phone Number   :",                      ~
               at (08,25), fac(lfac$(12)), e_hacd$              , ch(03),~
               at (08,29), fac(lfac$(13)), e_hphone$            , ch(07),~
               at (08,40), "B-Date :",                                   ~
               at (08,49), fac(lfac$(14)), e_bdte1$             , ch(10),~
               at (08,62), "SSN No.",                                    ~
               at (08,70), fac(lfac$(15)), e_ssn$               , ch(09),~
                                                                         ~
               at (09,02), "Emergency Phone No  :",                      ~
               at (09,25), fac(lfac$(16)), e_eacd$              , ch(03),~
               at (09,29), fac(lfac$(17)), e_ephone$            , ch(07),~
               at (09,40), "Contact:",                                   ~
               at (09,49), fac(lfac$(18)), e_econtact$          , ch(30),~
                                                                         ~
               at (10,02), "Family Status(M,S,D):",                      ~
               at (10,25), fac(lfac$(19)), e_msd$               , ch(01),~
               at (10,40), fac(hex(84)), e_msd_d$               , ch(30),~
                                                                         ~
               at (11,02), "Compy Stat.(A,I,T,C):",                      ~
               at (11,25), fac(lfac$(20)), e_status$            , ch(01),~
               at (11,40), fac(hex(84)), e_status_d$            , ch(30),~
                                                                         ~
               at (12,02), "Hire Date           :",                      ~
               at (12,25), fac(lfac$(21)), e_hire_dte1$         , ch(08),~
                                                                         ~
               at (12,40), "Termination Date    :",                      ~
               at (12,63), fac(lfac$(22)), e_term_dte1$         , ch(08),~
                                                                         ~
               at (13,02), "FloatDays Earned/DTE:",                      ~
               at (13,25), fac(lfac$(23)), e_float_dayss$       , ch(04),~
               at (13,30), fac(lfac$(24)), e_float_days1$       , ch(08),~
                                                                         ~
               at (13,40), "FloatDays Used/DTE  :",                      ~
               at (13,63), fac(lfac$(25)), e_float_usedd$       , ch(04),~
               at (13,68), fac(lfac$(26)), e_float_used1$       , ch(08),~
                                                                         ~
/*AWD017*/     at (14,02), "Vac. Days Earned/DTE:",                      ~
               at (14,25), fac(lfac$(27)), e_vac_dayss$         , ch(04),~
               at (14,30), fac(lfac$(28)), e_vac_days1$         , ch(08),~
                                                                         ~
               at (14,40), "Vac. Days Used/DTE  :",                      ~
/*AWD007*/     at (14,63), fac(lfac$(29)), e_vac_usedd$         , ch(04),~
               at (14,68), fac(lfac$(30)), e_vac_used1$         , ch(08),~
                                                                         ~
/*AWD017*/     at (15,02), "Sick Days Earned/DTE:",                      ~
               at (15,25), fac(lfac$(31)), e_sick_dayss$        , ch(04),~
               at (15,30), fac(lfac$(32)), e_sick_days1$        , ch(08),~
                                                                         ~
               at (15,40), "Sick Days Used/DTE  :",                      ~
/*EWD005*/     at (15,63), fac(lfac$(33)), e_sick_usedd$        , ch(04),~
               at (15,68), fac(lfac$(34)), e_sick_used1$        , ch(08),~
                                                                         ~
               at (16,02), "New Job Code    :",                          ~
               at (16,20), fac(lfac$(35)), e_job_code$          , ch(06),~
               at (16,40), fac(hex(84)), e_job_code_d$          , ch(30),~
                                                                         ~
               at (17,02), "Cur Pay Rate/DTE:",                          ~
               at (17,20), fac(lfac$(36)), e_pay_rate$          , ch(10),~
               at (17,31), fac(lfac$(37)), e_pay_rte1$          , ch(08),~
                                                                         ~
               at (17,40), "Prev Pay Rate/DTE:",                         ~
               at (17,60), fac(lfac$(38)), e_pay_ratep$         , ch(10),~
               at (17,71), fac(lfac$(39)), e_pay_rtep1$         , ch(08),~
                                                                         ~
               at (18,02), "Next Review Date:",                          ~
               at (18,20), fac(lfac$(40)), e_nrev_dte1$         , ch(08),~
                                                                         ~
               at (18,40), "Date of Last Pts :",                         ~
               at (18,60), fac(lfac$(41)), e_pts_dte1$          , ch(08),~
                                                                         ~
               at (19,02), "Points Late:",                               ~
               at (19,15), fac(lfac$(42)), e_pts_late$          , ch(08),~
                                                                         ~
               at (19,24), "Misc:",                                      ~
               at (19,30), fac(lfac$(43)), e_pts_mis$           , ch(08),~
                                                                         ~
               at (19,40), "Day :",                                      ~
               at (19,46), fac(lfac$(44)), e_pts_day$           , ch(08),~
                                                                         ~
               at (19,55), "Total:",                                     ~
               at (19,62), fac(lfac$(45)), e_pts_tot$           , ch(08),~
                                                                         ~
               at (20,02), "Temp Job Title:",                            ~
               at (20,18), fac(lfac$(46)), e_title$             , ch(04),~
               at (20,23), fac(hex(84)), e_title_d$             , ch(20),~
                                                                         ~
               at (20,44), "Term Code:",                                 ~
               at (20,55), fac(lfac$(47)), e_term_code$         , ch(03),~
               at (20,59), fac(hex(84)), e_term_code_d$         , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               
               if keyhit% <> 8% then goto L41820
                  if fieldnr% = 7 then gosub display_departments
                  if fieldnr% = 35 then gosub display_jobs
/*SR69149 */      if fieldnr% = 47 then gosub display_term_codes
                  goto L40600
                  
L41820:        if keyhit% <> 2% then goto L41830
                  gosub page_2
                  inpmessage$ = edtmessage$
                   goto L40600

L41830:        if keyhit% <> 9% then goto L41860
                  gosub rolodex

L41860:        if keyhit% <> 15 then L41890
                  call "PRNTSCRN" : goto L40600

L41890:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L42120     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Report      "
            pf$(2) = "(2)Page (2)      (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Rolodex             " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(0102ff04ffffffff09ffffffff0e0f1000)
            if fieldnr% <> 7 then goto L41900      
            str(pf$(1),41,15) = "(8)Lookup Dept "
            str(pfkeys$,8,1) = hex(08)
            goto L41910
L41900:     if fieldnr% <> 33 then goto L41905
            str(pf$(1),41,15) = "(8)Lookup Jobs "
            str(pfkeys$,8,1) = hex(08)
/*SR69149 + */
L41905:     if fieldnr% <> 47 then goto L41910
            str(pf$(1),41,15) = "(8)Lookup Terms"
            str(pfkeys$,8,1) = hex(08)
/*SR69149 - */
L41910:     if fieldnr% = 1% then L42060
                str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(3),18,15) = " "  :  str(pfkeys$, 9,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42060:     if fieldnr% > 2% then L42080
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42080:     if rec% = 1% then goto L42100
                str(pf$(2),1%,15) = " "  :  str(pfkeys$, 2,1) = hex(ff)
L42100:
            if sec_lvl$ > "0" then return       /* AWD016 */
                str(pf$(2), 1%,12%) = " "
                str(pfkeys$,02,1) = hex(ff)
            return

L42120: if fieldnr% > 0% then L42230  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (12)Delete      "
            pf$(2) = "(2)Page (2)                             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(0102ffffffffffffffffff0cffff0f1000)
            if rec% = 1% then goto L42220
                str(pf$(1),64%)   = " "
                str(pfkeys$,12,1) = hex(ff)
L42220:
            if sec_lvl$ > "0" then return       /* AWD016 */
                str(pf$(2), 1%,12%) = " "
                str(pfkeys$,02,1) = hex(ff)
            return
L42230:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                       "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            if fieldnr% <> 7 then goto N41900
            str(pf$(1),41,15) = "(8)Lookup Dept "
            str(pfkeys$,8,1) = hex(08)
            goto N41910
N41900:     if fieldnr% <> 35 then goto N41905
            str(pf$(1),41,15) = "(8)Lookup Jobs "
            str(pfkeys$,8,1) = hex(08)
N41905:     if fieldnr% <> 47 then goto N41910
            str(pf$(1),41,15) = "(8)Lookup Terms"
            str(pfkeys$,8,1) = hex(08)
N41910
            return
            
        display_departments
           readkey$ = " "
           str(readkey$,1%,9%) = "EMP CORPD"
           descr$ =hex(06) & "New Corp Dept Numbers     "
           call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2%))
           if f1%(2%) <> 0 then e_corp_dept$ = str(readkey$,10,4)
        return
/*SR69149 + */
        display_term_codes
           readkey$ = " "
           str(readkey$,1%,9%) = "EMP TERMC"
           descr$ =hex(06) & "New Terminations Codes    "
           call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2%))
           if f1%(2%) <> 0 then e_term_code$ = str(readkey$,10,4)
        return
/*SR69149 - */
        display_jobs
           readkey$ = " "
           str(readkey$,1%,9%) = "EMP JOBCD"
           descr$ =hex(06) & "New Job Codes             "
           call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2%))
           if f1%(2%) <> 0 then e_job_code$ = str(readkey$,10,6)
        return

        REM *************************************************************~
            *        E m p l o y e e   S c r e e n   P a g e  (2)       *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
            gosub set_pf2
            gosub set_page_2_secure
REM IF SECURITY_HR% >= 1% THEN GOSUB SET_PAGE_2_DEFAULT
            if sec_lvl$ > "3" then gosub set_page_2_default /* AWD016 */
L42410:     accept                                                       ~
               at (01,02),                                               ~
                  "Employee Master File Edit Utility - Page (2)",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Comment Line (1):",                          ~
               at (06,20), fac(lfac2$(01)), e_comm$(1%)         , ch(40),~
                                                                         ~
               at (07,02), "Comment Line (2):",                          ~
               at (07,20), fac(lfac2$(02)), e_comm$(2%)         , ch(40),~
                                                                         ~
               at (08,02), "Comment Line (3):",                          ~
               at (08,20), fac(lfac2$(03)), e_comm$(3%)         , ch(40),~
                                                                         ~
               at (09,02), "Comment Line (4):",                          ~
               at (09,20), fac(lfac2$(04)), e_comm$(4%)         , ch(40),~
                                                                         ~
               at (10,02), "Daily Clock In Time :",                      ~
               at (10,25), fac(lfac2$(05)), e_itime$            , ch(01),~
               at (10,40), fac(hex(84)), e_itime_d$             , ch(30),~
                                                                         ~
               at (11,02), "Daily Clock Out Time:",                      ~
               at (11,25), fac(lfac2$(06)), e_otime$            , ch(01),~
               at (11,40), fac(hex(84)), e_otime_d$             , ch(30),~
                                                                         ~
               at (12,02), "Clock Display (Y/N) :",                      ~
               at (12,25), fac(lfac2$(07)), e_display$          , ch(01),~
                                                                         ~
               at (13,02), "Deduct Lunch (Y/N)  :",                      ~
               at (13,25), fac(lfac2$(08)), e_lunch$            , ch(01),~
                                                                         ~
               at (14,02), "Lock Clock (Y/N)    :",                      ~
               at (14,25), fac(lfac2$(09)), e_lock$             , ch(01),~
                                                                         ~
               at (15,02), "Production Work Days:",                      ~
               at (15,25), fac(lfac2$(10)), e_days$             , ch(07),~
               at (15,40), fac(hex(84)), e_days_d$              , ch(30),~
                                                                         ~
               at (16,02), "Employee Shift Code :",                      ~
               at (16,25), fac(lfac2$(11)), e_shift$            , ch(02),~
               at (16,40), fac(hex(84)), e_shift_d$             , ch(30),~
                                                                         ~
               at (17,02), "Employee Lunch Code :",                      ~
               at (17,25), fac(lfac2$(12)), e_lun$             , ch(01), ~
               at (17,40), fac(hex(84)), e_lunch_d$            , ch(30), ~
                                                                         ~
               at (18,02), "Employee Pay Status :",                      ~
               at (18,25), fac(lfac2$(13)), e_pay_status$      , ch(01), ~
                                                                         ~
                                                                         ~                                                                         
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L42990
                  return

L42990:        if keyhit% <> 15 then L43020
                  call "PRNTSCRN" : goto L42410

L43020:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
         inpmessage$ = "Enter Applicable Employee Comments.            "
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "(2)Page (1)      (4)Page (4)            " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Page (3)                             " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01020304ffffffffffffffffffff0f1000)
REM IF SECURITY% = 1% THEN GOTO L43190
            if sec_lvl$ > "3" then goto L43190  /* AWD016 */
                str(pf$(2),18%,12%) = " "
                str(pfkeys$,04,1) = hex(ff)
            if sec_lvl$ > "0" then goto L43190  /* AWD016 */
                str(pf$(3), 1%,12%) = " "
                str(pfkeys$,03,1) = hex(ff)
L43190: return

set_page_2_default
           lfac2$(01) = hex(80)
           lfac2$(02) = hex(80)
           lfac2$(03) = hex(80)
           lfac2$(04) = hex(80)
           lfac2$(05) = hex(81)
           lfac2$(06) = hex(81)
           lfac2$(07) = hex(81)
           lfac2$(08) = hex(81)
           lfac2$(09) = hex(81)
           lfac2$(10) = hex(81)
           lfac2$(11) = hex(81)
           lfac2$(12) = hex(81)
        return

set_page_2_secure
           lfac2$(01) = hex(8c)
           lfac2$(02) = hex(8c)
           lfac2$(03) = hex(8c)
           lfac2$(04) = hex(8c)
           lfac2$(05) = hex(8c)
           lfac2$(06) = hex(8c)
           lfac2$(07) = hex(8c)
           lfac2$(08) = hex(8c)
           lfac2$(09) = hex(8c)
           lfac2$(10) = hex(8c)
           lfac2$(11) = hex(8c)
           lfac2$(12) = hex(8c)
        return

        REM *************************************************************~
            *               R e p o r t   S c r e e n                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%)
            gosub set_pf3
L43290:     accept                                                       ~
               at (01,02),                                               ~
                  "Employee Master File Edit Utility - Report Screen",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Type(S,D):",                          ~
               at (06,20), fac(hex(81)), type$                  , ch(01),~
               at (06,40), fac(hex(84)), type_d$                , ch(30),~
                                                                         ~
               at (07,02), "Employee's A,I,T:",                          ~
               at (07,20), fac(hex(81)), status$                , ch(01),~
               at (07,40), fac(hex(84)), status_d$              , ch(30),~
                                                                         ~
               at (08,02), "Report Selection:",                          ~
               at (08,20), fac(hex(81)), sel$                   , ch(01),~
               at (08,40), fac(hex(84)), sel_d$                 , ch(30),~
                                                                         ~
               at (09,02), "Beginning Value :",                          ~
               at (09,20), fac(hex(81)), beg_value$             , ch(15),~
               at (09,40), fac(hex(84)), beg_value_d$           , ch(30),~
                                                                         ~
               at (10,02), "Ending Value    :",                          ~
               at (10,20), fac(hex(81)), end_value$             , ch(15),~
               at (10,40), fac(hex(84)), end_value_d$           , ch(30),~
                                                                         ~
               at (11,02), "Beginning Shift :",                          ~
               at (11,20), fac(hex(81)), beg_shift$             , ch(02),~
               at (11,40), fac(hex(84)), beg_shift_d$           , ch(30),~
                                                                         ~
               at (12,02), "Ending Shift    :",                          ~
               at (12,20), fac(hex(81)), end_shift$             , ch(02),~
               at (12,40), fac(hex(84)), end_shift_d$           , ch(30),~
                                                                         ~
               at (14,20),                                               ~
                  "(1) - Report in Employee Number Sequence",            ~
               at (16,20),                                               ~
                  "(2) - Report in Department/Prod Sequence",            ~
               at (18,20),                                               ~
                  "(3) - Report in Emp Last Name   Sequence",            ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L43810
                  call "PRNTSCRN" : goto L43290

L43810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf3
         inpmessage$="Enter the Applic. Report Data (S)ummary, (D)etail?"
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffffffff0e0f1000)
        return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50520,         /* Employee Number        */~
                              L50760,         /* Department Code        */~
                              L50860,         /* Last Name              */~
                              L50920,         /* First Name             */~
                              L50980,         /* Middle Initial         */~
                              L51010,         /* Address Line (1)       */~
                              L51008,         /* New Corp Dept#         */~
                              L51040,         /* Address Line (2)       */~
                              L51070,         /* City Code              */~
                              L51100,         /* State Code             */~
                              L51130,         /* Zip Code               */~
                              L51230,         /* Home Phone Area Code   */~
                              L51330,         /* Home Phone Number      */~
                              L51430,         /* Employee Birth Date    */~
                              L51570,         /* Social Security No.    */~
                              L51680,         /* Emergency Area Code    */~
                              L51780,         /* Emergency Phone No.    */~
                              L51880,         /* Emergency Contact      */~
                              L51910,         /* Family Status Code     */~
                              L52040,         /* Company Status Code    */~
                              L52170,         /* Employee Hire Date     */~
                              L52310,         /* Employee Termination DT*/~
/*AWD021*/                    N52730,         /* Float    Days Accrued  */~
/*AWD021*/                    N52830,         /* Float    Days As of DTE*/~
/*AWD021*/                    N52970,         /* Float    Days Used     */~
/*AWD021*/                    N53070,         /* Float    Used as of DTE*/~
                              L52730,         /* Vacation Days Accrued  */~
                              L52830,         /* Vacation Days As of DTE*/~
                              L52970,         /* Vacation Days Used     */~
                              L53070,         /* Vacation Used as of DTE*/~
                              L53210,         /* Sick Days Earned       */~
                              L53310,         /* Sick Days As of DTE    */~
                              L53450,         /* Sick Days Used         */~
                              L53550,         /* Sick Used as of DTE    */~
                              L53760,         /* Emp New Job Code Req   */~
                              L53790,         /* Current Pay Rate       */~
                              L53900,         /* Current Pay Rate Date  */~
                              L54040,         /* Previous Pay Rate      */~
                              L54150,         /* Previous Pay Rate Date */~
                              L52590,         /* Next Revie Date        */~
                              L54430,         /* Date of Last Points    */~
                              L54570,         /* Late Points            */~
                              L54670,         /* Miscelaneous Points    */~
                              L54770,         /* Daily Points           */~
                              L54870,         /* Total Points           */~
                              L51000,         /* Temp Job Title         */~
/*SR69149 */                  L51020          /* Termination Code       */
            return

L50520: REM Test for Employee Number              E_NO$  CR2490 scre_no$ change
            if scre_no$ <> " " then goto L50550
               goto L50720
L50550:     e_no% = 0%
REM            if str(e_no$,1%,1%) = "A" then goto L50610     /* Part Time */
               convert scre_no$ to e_no%, data goto L50720
L12345:             FMT BI(4)                                 /* CR2490 */
            put str(e_no$,1%,4%), using L12345, e_no%
            str(e_no$,5%,1%) = " "  
               goto L50640
REM L50610:     convert str(e_no$,2%,4%) to e_no%, data goto L50720

REM            convert e_no% to str(e_no$,2%,4%), pic(0000)
L50640:     gosub dataload
            gosub load_access
            if access% = 0% then goto L50690
               goto inputmode

L50690:     if rec% = 0% then return
               fieldnr% = 47%
        return
L50720:     errormsg$ = "(Error) - Invalid Employee Number?(Req.)"
            e_no$ = " "
        return

L50760: REM Test for Department Code              E_DEPT$,E_DEPT_D$
            if e_dept$ <> " " then goto L50790
               goto L50820
L50790:     gosub lookup_dept
            if dept% = 0% then goto L50820
            gosub check_security /* AWD016 */
        return
L50820:     errormsg$ ="(Error) - Invalid Department/Product Line?(Req.)"
            e_dept$, e_dept_d$ = " "
        return

L50860: REM Test for Last Name                    E_LNAME$
            if e_lname$ <> " " then return
               errormsg$ = "(Error) - Last Name is Required Field?"
               e_lname$ = " "
        return

L50920: REM Test for First Name                   E_FNAME$
            if e_fname$ <> " " then return
               errormsg$ = "(Error) - First Name is Required Field?"
               e_fname$ = " "
        return

L50980: REM Test for Middle Initial               E_INIT$
        return

L51000: REM Test for title                        E_TITLE$
            if e_title$ <> "    " then goto L51005
            return                              /* CR2577 */
            errormsg$ ="(Error) - Invalid Job Title?(Req.)"
            e_title$, e_title_d$ = " "
        return
L51005: gosub lookup_title     
        return
        
        
/* </AWD018> */
L51008: REM Test for New Corp Dept#               E_CORP_DEPT$
            if e_corp_dept$ <> "    " then goto L51009
            errormsg$ ="(Error) - Invalid Corp. Dept. No.?(Req.)"
            e_corp_dept$, e_corp_dept_d$ = " "
        return
L51009: gosub lookup_corp_dept 
        return        
/* </AWD018> */

/*SR69149 + */ 
L51020: REM Test for New Term Code#               E_TERM_CODE$
            if e_term_code$ = "   " then goto L51029
            if e_term_code$ <> "   " then goto L51029
            errormsg$ ="(Error) - Invalid Term. Code. No.?(Req.)"
            e_term_code$, e_term_code_d$ = " "
        return
L51029: gosub lookup_term_code 
        return        
/*SR69149 - */ 

L51010: REM Test for Address Line (1)             E_ADDR1$
        return

L51040: REM Test for Address Line (2)             E_ADDR2$
        return

L51070: REM Test for City Code                    E_CITY$
        return

L51100: REM Test for State Code                   E_STATE$
        return

L51130: REM Test for Zip Code                     E_ZIP$
           if e_zip$ <> " " then goto L51160
              return
L51160:    x% = len(e_zip$)
           if x% < 5% then goto L51190
        return
L51190:    errormsg$ = "(Error) - Invalid Zip Code Must be > (5)?"
           e_zip$ = " "
        return

L51230: REM Test for Home Phone Area Code         E_HACD$
           if e_hacd$ <> " " then goto L51260
              return
L51260:    x% = len(e_hacd$)
           if x% <> 3% then goto L51290
        return
L51290:    errormsg$ = "(Error) - Invalid Home Phone Area Code?"
           e_hacd$ = " "
        return

L51330: REM Test for Home Phone Number            E_HPHONE$
           if e_hphone$ <> " " then goto L51360
              return
L51360:    x% = len(e_hphone$)
           if x% <> 7% then goto L51390
        return
L51390:    errormsg$ = "(Error) - Invalid Home Phone Number?"
           e_hphone$ = " "
        return

L51430: REM Test for Birth Date                   E_BDTE$, E_BDTE1$
          if e_bdte1$ <> " " then goto L51480
             e_bdte$, e_bdte1$ = " "
             return

L51480:   x$ = e_bdte1$
          date% = 0%
          call "DATEOKC" (x$, date%, errormsg$)
          if date% = 0% then return
          e_bdte1$ = x$
          call "DATUFMTC" (x$)
          e_bdte$ = x$
        return

L51570: REM Test for Social Security No.          E_SSN$
           if e_ssn$ <> " " then goto L51600
              return
L51600:    x% = len(e_ssn$)
           if x% <> 9% then goto L51630
        return
L51630:    errormsg$ = "(Error) - Invalid Social Security Number?"
           e_ssn$ = " "
        return


L51680: REM Test for Emergency Area Code          E_EACD$
           if e_eacd$ <> " " then goto L51710
              return
L51710:    x% = len(e_eacd$)
           if x% <> 3% then goto L51740
        return
L51740:    errormsg$ = "(Error) - Invalid Emergency Phone Area Code?"
           e_eacd$ = " "
        return

L51780: REM Test for Emergency Phone No.          E_EPHONE$
           if e_ephone$ <> " " then goto L51810
              return
L51810:    x% = len(e_ephone$)
           if x% <> 7% then goto L51840
        return
L51840:    errormsg$ = "(Error) - Invalid Emergency Phone Number?"
           e_ephone$ = " "
        return

L51880: REM Test for Emergency Contact            E_ECONTACT$
        return

L51910: REM Test Family Status                    E_MSD$
            e_msd_d$ = " "
            p% = pos("MSD " = str(e_msd$))        /* ADP  */
/* IF E_MSD$ <> " " THEN GOTO L51950      ADP  */
            if p% <> 0%  then goto L51950
               goto L52000
L51950:     if e_msd$ = "M" then e_msd_d$ = "(M)arried"
            if e_msd$ = "S" then e_msd_d$ = "(S)ingle "
            if e_msd$ = "D" then e_msd_d$ = "(D)ivorced"
            if e_msd$ = " " then e_msd_d$ = "Unknown"
            if e_msd_d$ = " " then goto L52000
        return
L52000:     errormsg$ = "(Error) - Invalid Marriage Status Code (Req.)?"
            e_msd$, e_msd_d$ = " "
        return

L52040: REM Test Company Status                   E_STATUS$
            e_status_d$ = " "
            if e_status$ <> " " then goto L52080
               goto L52130
L52080:     if e_status$ = "A" then e_status_d$ = "(A)ctive    "
            if e_status$ = "I" then e_status_d$ = "(I)nactive  "
            if e_status$ = "T" then e_status_d$ = "(T)erminated"
            if e_status$ = "C" then e_status_d$ = "(C)ontractor"  /* CR2621 */
            if e_status_d$ = " " then goto L52130
        return
L52130:     errormsg$ = "(Error) - Invalid Status Code (Req.)?"
            e_status$, e_status_d$ = " "
        return

L52170: REM Test for Hire Date                  E_HIRE_DTE$,E_HIRE_DTE1$
          if e_hire_dte1$ <> " " then goto L52220
             e_hire_dte$, e_hire_dte1$ = " "
             return

L52220:   x$ = e_hire_dte1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_hire_dte1$ = x$
          call "DATUNFMT" (x$)
          e_hire_dte$ = str(x$,1%,6%)
        return

L52310: REM Test for Termination Date           E_TERM_DTE$,E_TERM_DTE1$
          if e_term_dte1$ <> " " then goto L52360
             e_term_dte$, e_term_dte1$ = " "
             return

L52360:   x$ = e_term_dte1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_term_dte1$ = x$
          call "DATUNFMT" (x$)
          e_term_dte$ = str(x$,1%,6%)
        return

L52450: REM Test for Last Review Date           E_LREV_DTE$,E_LREV_DTE1$
          if e_lrev_dte1$ <> " " then goto L52500
             e_lrev_dte$, e_lrev_dte1$ = " "
             return

L52500:   x$ = e_lrev_dte1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_lrev_dte1$ = x$
          call "DATUNFMT" (x$)
          e_lrev_dte$ = str(x$,1%,6%)
        return

L52590: REM Test for Next Review Date           E_NREV_DTE$,E_NREV_DTE1$
          if e_nrev_dte1$ <> " " then goto L52640
             e_nrev_dte$, e_nrev_dte1$ = " "
             return

L52640:   x$ = e_nrev_dte1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_nrev_dte1$ = x$
          call "DATUNFMT" (x$)
          e_nrev_dte$ = str(x$,1%,6%)
        return

L52730: REM Test for Vacation Days Acrued       E_VAC_DAYSS$
            e_vac_days% = 0%
/*AWD021*/  e_vac_days = 0
            if e_vac_dayss$ <> " " then goto L52770
               goto L52790
L52770:
/*(AWD017)*/
REM CONVERT E_VAC_DAYSS$ TO E_VAC_DAYS%, DATA GOTO L52790
            convert e_vac_dayss$ to e_vac_days, data goto L52790

L52790:
REM CONVERT E_VAC_DAYS% TO E_VAC_DAYSS$, PIC(00)
            convert e_vac_days to e_vac_dayss$, pic(00.0)

            e_vac_days% = (e_vac_days * 10)

            result% = 0.0
            result% = MOD(e_vac_days%, 5%)
            if result% <> 0.0 then goto L52795

        return
L52795:   errormsg$ = "Invalid vacation days earned!"
        return

L52830: REM Test for Vacation Days as of        E_VAC_DAYS$,E_VAC_DAYS1$
          if e_vac_days1$ <> " " then goto L52880
             e_vac_days$, e_vac_days1$ = " "
             return

L52880:   x$ = e_vac_days1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_vac_days1$ = x$
          call "DATUNFMT" (x$)
          e_vac_days$ = str(x$,1%,6%)
        return

L52970: REM Test for Vacation Days Used         E_VAC_USEDD$
            e_vac_used% = 0%
/*AWD021*/  e_vac_used = 0
            if e_vac_usedd$ <> " " then goto L53010
               goto L53030
                                                        /* (AWD007) */
L53010:

REM            convert e_vac_usedd$ to e_vac_used%, data goto L53030
            convert e_vac_usedd$ to e_vac_used, data goto L53030

L53030:
REM CONVERT E_VAC_USED% TO E_VAC_USEDD$, PIC(00)
            convert e_vac_used to e_vac_usedd$, pic(00.0)

            e_vac_used% = (e_vac_used * 10)    /*  (AWD007)  */

            result% = 0.0                       /*  (AWD007)  */
            result% = MOD(e_vac_used%, 5%)     /*  (AWD007)  */

            if result% <> 0.0 then goto L53020  /*  (AWD007)  */
        return
L53020:   errormsg$ = "Invalid vacation days used!" /*  (AWD007)  */
          init(" ") e_vac_usedd$                /*  (AWD007)  */
        return

        return

L53070: REM Test for Vacation Days Used as of   E_VAC_USED$,E_VAC_USED1$
          if e_vac_used1$ <> " " then goto L53120
             e_vac_used$, e_vac_used1$ = " "
             return

L53120:   x$ = e_vac_used1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_vac_used1$ = x$
          call "DATUNFMT" (x$)
          e_vac_used$ = str(x$,1%,6%)
        return

/*<AWD021> +*/
N52730: REM Test for Float Days Acrued       E_FLOAT_DAYSS$
            e_float_days% = 0%
/*AWD021*/  e_float_days = 0
            if e_float_dayss$ <> " " then goto N52770
               goto N52790
N52770:
                    
REM CONVERT E_FLOAT_DAYSS$ TO E_FLOAT_DAYS%, DATA GOTO N52790
            convert e_float_dayss$ to e_float_days, data goto N52790

N52790:
REM CONVERT E_FLOAT_DAYS% TO E_FLOAT_DAYSS$, PIC(00)
            convert e_float_days to e_float_dayss$, pic(00.0)

/* IF E_FLOAT_DAYS < 0 OR E_FLOAT_DAYS > 3 THEN GOTO N52795   */
/*SR71971*/ if e_float_days < 0 or e_float_days > 6 then goto N52795
            e_float_days% = (e_float_days * 10)    /*  (AWD007)  */

            result% = 0.0                       /*  (AWD007)  */
            result% = MOD(e_float_days%, 5%)     /*  (AWD007)  */

            if result% <> 0.0 then goto N52795  /*  (AWD007)  */

        return
N52795:   errormsg$ = "Invalid float days earned!"
        return

N52830: REM Test for Float Days as of        E_FLOAT_DAYS$,E_FLOAT_DAYS1$
          if e_float_days1$ <> " " then goto N52880
             e_float_days$, e_float_days1$ = " "
             return

N52880:   x$ = e_float_days1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_float_days1$ = x$
          call "DATUNFMT" (x$)
          e_float_days$ = str(x$,1%,6%)
        return

N52970: REM Test for Float Days Used         E_FLOAT_USEDD$
            e_float_used% = 0%
/*AWD021*/  e_float_used = 0
            if e_float_usedd$ <> " " then goto N53010
               goto N53030

N53010:

REM CONVERT E_FLOAT_USEDD$ TO E_FLOAT_USED%, DATA GOTO N53030
            convert e_float_usedd$ to e_float_used, data goto N53030

N53030:
REM CONVERT E_FLOAT_USED% TO E_FLOAT_USEDD$, PIC(00)
            convert e_float_used to e_float_usedd$, pic(00.0)

/* IF E_FLOAT_USED < 0 OR E_FLOAT_USED > 3 THEN GOTO N53020 */
/*SR71971*/ if e_float_used < 0 or e_float_used > 6 then goto N53020
            e_float_used% = (e_float_used * 10)    /*  (AWD007)  */

            result% = 0.0                       /*  (AWD007)  */
            result% = MOD(e_float_used%, 5%)     /*  (AWD007)  */

            if result% <> 0.0 then goto N52795  /*  (AWD007)  */

        return
N53020:   errormsg$ = "Invalid Float days used!"
          init(" ") e_float_usedd$
        return

        return

N53070: REM Test for Float Days Used as of   E_FLOAT_USED$,E_FLOAT_USED1$
          if e_float_used1$ <> " " then goto N53120
             e_float_used$, e_float_used1$ = " "
             return

N53120:   x$ = e_float_used1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_float_used1$ = x$
          call "DATUNFMT" (x$)
          e_float_used$ = str(x$,1%,6%)
        return
/*<AWD021> -*/

L53210: REM Test for Sick Days Earned           E_SICK_DAYSS$
            e_sick_days% = 0%
/*AWD021*/  e_sick_days = 0
            if e_sick_dayss$ <> " " then goto L53250
               goto L53270
L53250:
/*(AWD017)*/
REM CONVERT E_SICK_DAYSS$ TO E_SICK_DAYS%, DATA GOTO L53270
            convert e_sick_dayss$ to e_sick_days, data goto L53270


L53270:
REM CONVERT E_SICK_DAYS% TO E_SICK_DAYSS$, PIC(00)
            convert e_sick_days to e_sick_dayss$, pic(00.0)

            e_sick_days% = (e_sick_days * 10)

            result% = 0.0
            result% = MOD(e_sick_days%, 5%)
            if result% <> 0.0 then goto L53215

        return
L53215:   errormsg$ = "Invalid sick days earned!"

        return


L53310: REM Test for Sick Days as of          E_SICK_DAYS$,E_SICK_DAYS1$
          if e_sick_days1$ <> " " then goto L53360
             e_sick_days$, e_sick_days1$ = " "
             return

L53360:   x$ = e_sick_days1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_sick_days1$ = x$
          call "DATUNFMT" (x$)
          e_sick_days$ = str(x$,1%,6%)
        return

L53450: REM Test for Sick Days Used             E_SICK_USEDD$
            e_sick_used% = 0%
/*AWD021*/  e_sick_used = 0
            if e_sick_usedd$ <> " " then goto L53490
               goto L53510                          /* (EWD004) */
L53490:     convert e_sick_usedd$ to e_sick_used, data goto L53510

L53510:     convert e_sick_used to e_sick_usedd$, pic(00.0)

            e_sick_used% = (e_sick_used * 10)   /*  (EWD004)  */

            result% = 0.0                       /*  (EWD004)  */
            result% = MOD(e_sick_used%, 5%)     /*  (EWD004)  */

            if result% <> 0.0 then goto L53520  /*  (EWD004)  */
        return
L53520:   errormsg$ = "Invalid sick days used!" /*  (EWD004)  */
          init(" ") e_sick_used$                /*  (EWD004)  */
        return

L53550: REM Test for Sick Days Used as of     E_SICK_USED$,E_SICK_USED1$
          if e_sick_used1$ <> " " then goto L53600
             e_sick_used$, e_sick_used1$ = " "
             return

L53600:   x$ = e_sick_used1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_sick_used1$ = x$
          call "DATUNFMT" (x$)
          e_sick_used$ = str(x$,1%,6%)
        return

L53690: REM Test for Pay Grade                E_PAYG$, E_PAYG_D$
REM  IF E_DEPT$ > "16" AND E_DEPT$ < "21" THEN E_PAYG$ = "200"

            gosub lookup_payg
            if payg% = 0% then goto L53750
        return
L53750:     errormsg$ = "(Error) - Invalid Pay Grade? Req."
            e_payg$, e_payg_d$ = " "
        return
        
L53760: REM Test for New Job Code        E_JOB_CODE$, E_JOB_CODE_D$

            gosub lookup_job_code
            if job_code% = 0% then goto L53770
        return
L53770:     errormsg$ = "(Error) - Invalid Job Code? Req."
            e_job_code$, e_job_code_d$ = " "
        return        

L53790: REM Test for Current Pay Rate           E_PAY_RATE$
            e_pay_rate = 0.0
REM IF E_DEPT$ > "16" AND E_DEPT$ < "21" THEN GOTO L53860
            if e_pay_rate$ <> " " then goto L53840
               goto L53860
L53840:     convert e_pay_rate$ to e_pay_rate, data goto L53860

L53860:     convert e_pay_rate to e_pay_rate$, pic(######.##-)

        return

L53900: REM Test for Current Pay Rate Date    E_PAY_RTE$,E_PAY_RTE1$
          if e_pay_rte1$ <> " " then goto L53950
             e_pay_rte$, e_pay_rte1$ = " "
             return

L53950:   x$ = e_pay_rte1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_pay_rte1$ = x$
          call "DATUNFMT" (x$)
          e_pay_rte$ = str(x$,1%,6%)
        return

L54040: REM Test for Previous Pay Rate          E_PAY_RATEP$
            e_pay_ratep = 0.0
REM IF E_DEPT$ > "16" AND E_DEPT$ < "21" THEN GOTO L54110
            if e_pay_ratep$ <> " " then goto L54090
               goto L54110
L54090:     convert e_pay_ratep$ to e_pay_ratep, data goto L54110

L54110:     convert e_pay_ratep to e_pay_ratep$, pic(######.##-)

        return

L54150: REM Test for Previous Pay Rate Date   E_PAY_RTEP$,E_PAY_RTEP1$
          if e_pay_rtep1$ <> " " then goto L54200
             e_pay_rtep$, e_pay_rtep1$ = " "
             return

L54200:   x$ = e_pay_rtep1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_pay_rtep1$ = x$
          call "DATUNFMT" (x$)
          e_pay_rtep$ = str(x$,1%,6%)
        return

L54290: REM Test for Next Pay Rate Date      E_PAY_RTEN$,E_PAY_RTEN1$
          if e_pay_rten1$ <> " " then goto L54340
             e_pay_rten$, e_pay_rten1$ = " "
             return

L54340:   x$ = e_pay_rten1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_pay_rten1$ = x$
          call "DATUNFMT" (x$)
          e_pay_rten$ = str(x$,1%,6%)
        return

L54430: REM Test for Date Last Points        E_PTS_DTE$,E_PTS_DTE1$
          if e_pts_dte1$ <> " " then goto L54480
          e_pts_dte$, e_pts_dte1$ = " "
        return

L54480:   x$ = e_pts_dte1$
          date% = 0%
          call "DATEOK" (x$, date%, errormsg$)
          if date% = 0% then return
          e_pts_dte1$ = x$
          call "DATUNFMT" (x$)
          e_pts_dte$ = str(x$,1%,6%)
        return

L54570: REM Test Point Late                   E_PTS_LATE$
            e_pts_late = 0.0
            if e_pts_late$ <> " " then goto L54610
               goto L54630
L54610:     convert e_pts_late$ to e_pts_late, data goto L54630

L54630:     convert e_pts_late to e_pts_late$, pic(####.##-)
        goto L54870
        return

L54670: REM Test Point MISC                   E_PTS_MIS$
            e_pts_mis = 0.0
            if e_pts_mis$ <> " " then goto L54710
               goto L54730
L54710:     convert e_pts_mis$ to e_pts_mis, data goto L54730

L54730:     convert e_pts_mis to e_pts_mis$, pic(####.##-)
        goto L54870
        return

L54770: REM Test Point Day                    E_PTS_DAY$
            e_pts_day = 0.0
            if e_pts_day$ <> " " then goto L54810
               goto L54830
L54810:     convert e_pts_day$ to e_pts_day, data goto L54830

L54830:     convert e_pts_day to e_pts_day$, pic(####.##-)
        goto L54870
        return

L54870: REM Test Point Total                  E_PTS_TOT$
            e_pts_tot = 0.0
            e_pts_tot = round(e_pts_late + e_pts_mis + e_pts_day, 2)
            convert e_pts_tot to e_pts_tot$, pic(####.##-)
            if e_pts_tot$ <> " " then goto L54930
               goto L54950
L54930:     convert e_pts_tot$ to e_pts_tot, data goto L54950

L54950:     convert e_pts_tot to e_pts_tot$, pic(####.##-)
        return
        

        /* <AWD011> */
REM         +---------------------------------------------------------------+
REM         | Check fields to be logged for changes AWD011                  |
REM         +---------------------------------------------------------------+
LOG_CHANGES:
        if e_pts_dte$  = hold_dte$  then goto skip_dte
          field$ = "Points Date"
          from$  = hold_dte$
          to$    = e_pts_dte$
        gosub WRITE_AUDIT

skip_dte:  
        if e_pts_late$ = hold_late$ then goto skip_late
          field$ = "Points Late"
          from$  = hold_late$
          to$    = e_pts_late$
        gosub WRITE_AUDIT

skip_late: 
        if e_pts_mis$  = hold_mis$  then goto skip_mis
          field$ = "Points Misc."
          from$  = hold_mis$
          to$    = e_pts_mis$
        gosub WRITE_AUDIT

skip_mis:  
        if e_pts_day$  = hold_day$  then goto skip_day
          field$ = "Points Day"
          from$  = hold_day$
          to$    = e_pts_day$
        gosub WRITE_AUDIT

skip_day:  
        if e_pts_tot$  = hold_tot$  then goto skip_tot
          field$ = "Points Total"
          from$  = hold_tot$
          to$    = e_pts_tot$
        gosub WRITE_AUDIT

skip_tot:  

        return

REM         +---------------------------------------------------------------+
REM         | Write audit trail for point changes  AWD011                   |
REM         +---------------------------------------------------------------+
WRITE_AUDIT:
/* get last key (first 9's compliment key) */
        init(" ") readkey$
        str(readkey$,1%,9%)   = "AUDIT LOG"
        str(readkey$,10%,15%) = "000000000000000"
        read #2,hold,key = readkey$,using L55000,aud_key$, eod goto L55010
        
L55000:     FMT POS(25), CH(8)

        convert aud_key$ to aud_key%
        aud_key% = aud_key% + 1%
        convert aud_key% to aud_key$, pic (00000000)
        rewrite #2,using L55000, aud_key$
        
        goto L55020
L55010:
        aud_key$ = "00000001"
        write #2,using L55015, readkey$,  aud_key$, " "
            
L55015:     FMT CH(24), CH(8), CH(96)

L55020:
        convert aud_key$ to aud_key
        write #4, using APCAUDLG, aud_key, aud_date$, time$, userid$, ~
                  e_no$, field$, from$, to$, "  ", eod goto WRITE_AUDIT
        return
APCAUDLG: FMT PD(9), CH(5), CH(8), CH(3), CH(5), 3*CH(32), CH(3)
/* </AWD011> */

        REM *************************************************************~
            *                                                           *~
            *           I m a g e   S t a t e m e n t s                 *~
            *                                                           *~
            *                                                           *~
            *************************************************************

                                              /* Header Formats Summary*/
L55080: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L55120: %! ######## @ ########                                   Employee~
        ~ Master File Report                                    Page: ### ~
        ~  !
L55150: %!                                                            ###~
        ~############                                        ############ ~
        ~  !
L55180: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--!

                                              /* Header Formats Detail */
L55230: %+---------------------------------------------------------------~
        ~--------------+

L55260: %! ######## @ ########      Employee Master File Report          ~
        ~    Page: ### !
L55280: %!                               ###############                 ~
        ~ ############ !

                                                     /* Detail Formats */
L55320: %! Employee No. : #####        Dept./Prod.: ### #################~
        ~############# !
L55340: %! Corp Dept:################### Job Code:###### ################~
        ~############# !
L55360: %! ############### ############## # Home Phone: ##############   ~
        ~              !
L55380: %! ##############################   Emergency : ##############   ~
        ~              !
L55400: %! ##############################   Contact   : #################~
        ~############# !
L55420: %! ################## ##  ######### Birth Dte : ########## SSN: #~
        ~##########    !
L55440: %! Family Status: # ############### Company Status  : # #########~
        ~######        !
L55460: %! Init Hire Date   : ########      Termination Date: ########   ~
        ~              !
L55480: %! Last Review Date : ########      Next Review Date: ########   ~
        ~              !
L55500: %!  Vac Days Earned : ## as of ########   Vac Days Used: ## as of~
        ~ ########     !
L55520: %! Sick Days Earned : ## as of ########  Sick Days Used: ### as of~
        ~ ########    !
L55540: %! Job Code:###### ############################# Pay Rate: ######~
        ~####  ########!
L55560: %! Prev. Pay Rate: ########## Date: ########  Next Increase Date:~
        ~ ########     !
L55580: %! Late Points : ########   Misc Points  : ########              ~
        ~              !
L55600: %! Day  Points : ########   Total Points : ########  Date of Last~
        ~ Pts: ########!
L55620: %!---------------------------------------------------------------~
        ~--------------!
L55640: %!***************************************************************~
        ~**************!
L55670: %! Comment (1):  ########################################        ~
        ~              !
L55690: %! Comment (2):  ########################################        ~
        ~              !
L55710: %! Comment (3):  ########################################        ~
        ~              !
L55730: %! Comment (4):  ########################################        ~
        ~              !
L55750: %! Daily Clock In Time: ##########  Daily Clock Out Time: #######~
        ~###           !
L55770: %! Display Data on Time Clock: #    Deduct Lunch: #  Lock Clock: ~
        ~#             !
L55790: %! Production Work Days: Mon:#  Tues:#  Wed:#  Thur:#  Fri:#  Sat~
        ~:#  Sun:#     !
L55810: %! Emp Shift Code: ## ####################                       ~
        ~             !
L55840: %!E No.!Last Name       First      M!Address Line (1)    !City   ~
        ~          St Zip Code ! Department !Birth Date!Hire DTE!Term DTE ~
        ~Sf!
L55880: %!-----!----------------------------!--------------------!-------~
        ~----------------------!------------!----------!--------!---------~
        ~--!
L55920: %!#####!############### ########## #!####################!#######~
        ~######### ## #########!############!##########!########!######## ~
        ~##!

        REM *************************************************************~
            *                                                           *~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *                                                           *~
            *                                                           *~
            *************************************************************
   
        page_2
            gosub'102(1%)                          /* Employee Page (2) */
            gosub edit_page_2
            if keyhit% = 1% then gosub startover
            if keyhit% = 2% then goto L60144
            if keyhit% = 3% and sec_lvl$ > "0" then gosub page_3
            if keyhit% = 4% then gosub page_4
            if keyhit% = 16% then gosub dataput
            goto page_2
L60144: return

        page_3
            call "APCEMP1B"  (e_no$,              /* Employee Page (3) */~
                              e_lname$,                                  ~
                              e_fname$,           /* ADP               */~
                              e_init$,                                   ~
                              #2,                 /*GENCODES (AWD008)  */~
                              #3,                 /*APCEMPPR (AWD008)  */~
                              #5,                 /*APCEMPEX (AWD015)  */~
                              sec_lvl$     )      /*         (AWD016)  */
        return

        page_4
            call "APCEMP2B"  (e_no$)               /* Employee Page (4) */

        return

        select_printer
            call "SHOSTAT" ("Printing Report")
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
        return

        print_report
            gosub check_security
REM IF SECURITY% <> 1% THEN GOTO ACCESS_DENIED    /* (EWD006)  */
            if sec_lvl$ = "0" then goto access_denied    /* (AWD016)  */
L60360:     gosub'103(1%)
            if keyhit% = 1% then gosub startover
            if keyhit% = 16% then goto exit_program
            if keyhit% = 14% then goto L60432
            errormsg$ = " "
            gosub check_data
            goto L60360

L60432:     if check% = 0% then goto L60360
            gosub select_printer
            rpt% = 1%
            emp_key$ = all(hex(00))
            if str(beg_value$,1%,3%) <> "ALL" then emp_key$ = beg_value$

            gosub select_printer
            if sel% = 1% then                       /* EMPLOYEE NUMBER */~
               read #1,key > emp_key$, eod goto report_end
            if sel% = 2% then                       /* DEPARTMENT CODE */~
               read #1,key 1% > emp_key$, eod goto report_end
            if sel% = 3% then                       /* NAME SORT       */~
               read #1,key 2% > emp_key$, eod goto report_end
            goto L60576
        report_next
            read #1, eod goto report_end
L60576:     gosub dataload
                                                     /* TYPE - (A,I,T) */
            if status$  = " " then goto L60621
               if status$ <> e_status$ then goto report_next

L60621:     if str(beg_value$,1%,3%) = "ALL" then goto L60711
               if sel% <> 1% then goto L60657         /* EMPLOYEE NUMBER*/
                  if e_no$ > str(end_value$,1%,5%) then goto report_end
                  goto L60711
L60657:        if sel% <> 2% then goto L60693         /* DEPARTMENT CODE*/
                  if e_dept$ > str(end_value$,1%,3%) then goto report_end
                  goto L60711
                                                     /* LAST NAME SORT */
L60693:        if e_lname$ > end_value$ then goto report_end

L60711:     if str(beg_shift$,1%,1%) = "A" then goto L60774
               if e_shift$ <> " " then goto L60747
                  goto L60774

L60747:        if e_shift$ < beg_shift$ then goto report_next
               if e_shift$ > end_shift$ then goto report_next

L60774:     if type$ = "D" then gosub print_detail                       ~
                           else gosub print_detail_s

            goto report_next

        report_end
            if type$ = "D" then print using L55230                        ~
                           else print using L55080

            close printer
        return clear all
        goto inputmode

        print_header
            pageno% = pageno% + 1%
            if lcntr% <> 99% then print using L55230
            runtime$ = " "
            call "TIME" (runtime$)
            print page
            print using L55230
            print using L55260, date$, runtime$, pageno%
            print using L55280, type_d$, sort_type$
            lcntr% = 3%
        return

        print_header_s
            pageno% = pageno% + 1%
            if lcntr% <> 99% then print using L55080
            runtime$ = " "
            call "TIME" (runtime$)
            print page
            print using L55080
            print using L55120, date$, runtime$, pageno%
            print using L55150, type_d$, sort_type$
            print using L55180
            print using L55840
            lcntr% = 5%
        return

        print_detail
            if lcntr% > 50% then gosub print_header
            e_pay_rate$, e_pay_ratep$ = " "            /* Always Zero  */

            print using L55620                          /* SEPERATOR -- */
            print using L55320, e_no$, e_dept$, e_dept_d$
            print using L55340, e_title_d$, e_job_code$, e_job_code_d$
            print using L55360, e_lname$, e_fname$, e_init$, msk_hphone$
            print using L55380, e_addr1$, msk_ephone$
            print using L55400, e_addr2$, e_econtact$
            print using L55420, e_city$, e_state$, e_zip$, e_bdte1$,      ~
                               msk_ssn$
            print using L55440, e_msd$, e_msd_d$, e_status$, e_status_d$
            print using L55640                          /* SEPERATOR ** */
            print using L55460, e_hire_dte1$, e_term_dte1$
            print using L55480, e_lrev_dte1$, e_nrev_dte1$
            print using L55640                          /* SEPERATOR ** */
            print using L55500, e_vac_dayss$, e_vac_days1$, e_vac_usedd$, ~
                               e_vac_used1$
            print using L55520, e_sick_dayss$, e_sick_days1$,             ~
                               e_sick_usedd$, e_sick_used1$
            print using L55640                          /* SEPERATOR ** */
            print using L55540, e_payg$, e_payg_d$, e_pay_rate$,          ~
                               e_pay_rte1$
            print using L55560, e_pay_ratep$, e_pay_rtep1$, e_pay_rten1$
            print using L55640                          /* SEPERATOR ** */
            print using L55580, e_pts_late$, e_pts_mis$
            print using L55600, e_pts_day$,  e_pts_tot$, e_pts_dte1$
            print using L55640                          /* Seperator ** */
            print using L55670, e_comm$(1%)
            print using L55690, e_comm$(2%)
            print using L55710, e_comm$(3%)
            print using L55730, e_comm$(4%)
            print using L55750, str(e_itime_d$,1%,10%),                   ~
                               str(e_otime_d$,1%,10%)
            print using L55770, e_display$, e_lunch$, e_lock$
            print using L55790, str(e_days$,1%,1%), str(e_days$,2%,1%),   ~
              str(e_days$,3%,1%), str(e_days$,4%,1%), str(e_days$,5%,1%),~
              str(e_days$,6%,1%), str(e_days$,7%,1%)
            gosub get_shift
        /* AWD010 */
            print using L55810, e_shift$, str(e_shift_d$,1%,20%),         ~
                               e_lun$,   str(e_lunch_d$,1%,15%)
            lcntr% = lcntr% + 29%
        return

        print_detail_s
            if lcntr% > 58% then gosub print_header_s
            print using L55880
            print using L55920, e_no$, e_lname$, e_fname$, e_init$,       ~
                               e_addr1$, e_city$, e_state$, e_zip$,      ~
                               str(e_dept_d$,1%,12%), e_bdte1$,          ~
                               e_hire_dte1$, e_term_dte1$, e_shift$
            lcntr% = lcntr% + 2%
        return

        lookup_dept                         /* Department/Product Line */
            dept% = 0%
            init(" ") readkey$, e_dept_d$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = e_dept$
            read #2,key = readkey$,using L61683,e_dept_d$, eod goto L61710
L61683:       FMT POS(25), CH(32)
            dept% = 1%
        return
L61710:     errormsg$ = "(Error) - Invalid Department/Product Line Code?"
            init(" ") e_dept$, e_dept_d$
        return

        lookup_title
            title% = 0%
            init(" ") readkey$, e_title_d$
        if e_title$ = "    " then e_title$ = "0000"
            str(readkey$,1%,9%)   = "EMP TITLE"
            str(readkey$,10%,15%) = e_title$
            read #2,key = readkey$,using L61683,e_title_d$, eod goto L61750
            title% = 1%
        return
L61750:     /* errormsg$ = "(Error) - Invalid Job Title?"  CR2577 */
            init(" ") e_title$, e_title_d$
        return
        
/* <AWD018> */
        lookup_corp_dept
            corp_dept% = 0%
            init(" ") readkey$, e_corp_dept_d$
        if e_corp_dept$ = "      " then e_corp_dept$ = "000000"
            str(readkey$,1%,9%)   = "EMP CORPD"
            str(readkey$,10%,15%) = e_corp_dept$
            read #2,key = readkey$,using L61683,e_corp_dept_d$, eod goto L61760
            corp_dept% = 1%
        return
L61760:     errormsg$ = "(Error) - Invalid Corp Dept No.?"
            init(" ") e_corp_dept$, e_corp_dept_d$
        return
        
/*SR69149 + */
        lookup_term_code
            term_code% = 0%
            init(" ") readkey$, e_term_code_d$
        if e_term_code$ = "   " then goto lookup_term_code_end
            str(readkey$,1%,9%)   = "EMP TERMC"
            str(readkey$,10%,15%) = e_term_code$
            read #2,key = readkey$,using L61683,e_term_code_d$, eod goto L61780
        lookup_term_code_end
            term_code% = 1%
        return
L61780:     errormsg$ = "(Error) - Invalid Term Code No.?"
            init(" ") e_term_code$, e_term_code_d$
        return
/*SR69149 - */
        lookup_job_code
            job_code% = 0%
            init(" ") readkey$, e_job_code_d$
        if e_job_code$ = "      " then e_job_code$ = "000000"
            str(readkey$,1%,9%)   = "EMP JOBCD"
            str(readkey$,10%,15%) = e_job_code$
            read #2,key = readkey$,using L61683,e_job_code_d$, eod goto L61770
            job_code% = 1%
        return
L61770:   REM  errormsg$ = "(Error) - Invalid Job Code?"
            init(" ") e_job_code$, e_job_code_d$
        return
/* <AWD018> */

        lookup_payg                         /* Employee Pay Grade      */
            payg% = 0%
            init(" ") readkey$, e_payg_d$
            str(readkey$,1%,9%)   = "EMP PAYG "
            str(readkey$,10%,15%) = e_payg$
            read #2,key = readkey$,using L61800,e_payg_d$, eod goto L61827
L61800:       FMT POS(25), CH(32)
            payg% = 1%
        return
L61827:     errormsg$ = "(Error) - Invalid Employee Pay Grade?"
            init(" ") e_payg$, e_payg_d$
        return

        edit_page_2                   /* E_ITIME$, E_OTIME$, E_DISPLAY$*/
            shift% = 0%
L61881:     init(" ") readkey$, e_itime_d$
            str(readkey$,1%,9%)   = "EMP ITIME"
            str(readkey$,10%,15%) = e_itime$
            read #2,key = readkey$,using L61917,e_itime_d$, eod goto L61935
L61917:       FMT POS(25), CH(32)
        goto L61953
L61935:     e_itime$ = "3"
            goto L61881
L61953:
L61962:     init(" ") readkey$, e_otime_d$
            str(readkey$,1%,9%)   = "EMP OTIME"
            str(readkey$,10%,15%) = e_otime$
            read #2,key = readkey$,using L61917,e_otime_d$, eod goto L62007
        goto L62034
L62007:     e_otime$ = "2"
            goto L61962

L62034: if e_display$ <> "Y" and e_display$ <> "N" then e_display$ = "Y"
        if e_lunch$ <> "Y" and e_lunch$ <> "N" then e_lunch$ = "N"
        if e_lock$ <> "Y" and e_lock$ <> "N" then e_lock$ = "N"

        for i% = 1% to 7%
           if str(e_days$,i%,1%) <> "Y" and str(e_days$,i%,1%) <> "N" ~
                                        then str(e_days$,i%,1%) = "Y"
        next i%
        
        if e_pay_status$ <> "E" and e_pay_status$ <> "N" and   /* ADP */ ~ 
           e_pay_status$ <> "H" then e_pay_status$ = "X" 
           
REM Check Shift Code
        get_shift
           if e_shift$ <> " " then goto L62151
L62133:       e_shift$ = "00"

L62151:     init(" ") readkey$, e_shift_d$
            str(readkey$,1%,9%)   = "EMP SHIFT"
            str(readkey$,10%,15%) = e_shift$
            read #2,key = readkey$,using L61917,e_shift_d$, eod goto L62133
        if shift% = 1% then return
REM Check Lunch Code
           if e_lun$ <> " " then goto L62241
L62223:       e_lun$ = "0"

L62241:     init(" ") readkey$, e_lunch_d$
            str(readkey$,1%,9%)   = "EMP LUNCH"
            str(readkey$,10%,15%) = e_lun$
            read #2,key = readkey$,using L61917,e_lunch_d$, eod goto L62223

        return

/* </AWD010> */

        lookup_name
            lname% = 0%
            read #1,key 2% > emp_key$, using L62340, e_lname$, e_fname$,  ~
                                        eod goto L62358
L62340:        FMT POS(11), CH(15), CH(10)
            lname% = 1%
L62358: return

        check_data
            errormsg$ = " "
            shift%, check% = 0%
            status_d$ = "All APC Employee's          "
            if type$ <> " " then goto L62430
               type$ = "D"
L62430:     if type$ = "D" then type_d$ = "( D e t a i l )"
            if type$ = "S" then type_d$ = "(S u m m a r y)"
            if type$ = "S" or type$ = "D" then goto L62493
               errormsg$ = "(Error) - Invalid Report Type. (S) or (D)"
               type$, type_d$ = " "
               return

L62493:     if status$ = " " then goto L62565
             if status$ = "A" then                                       ~
                status_d$ = "Only - Active Employee's    "
             if status$ = "I" then                                       ~
                status_d$ = "Only - Inactive Employee's  "
             if status$ = "T" then                                       ~
                status_d$ = "Only - Terminated Employee's"
/* CR2621 */
             if status$ = "C" then                                       ~
                status_d$ = "Only - Contract Employee's  "
                
L62565:     sel% = 1%
            errormsg$ = " "
            convert sel$ to sel%, data goto L62592
L62592:
            convert sel% to sel$, pic(#)
            if sel% > 0% and sel% < 4% then goto L62655
               errormsg$ = "(Error) - Invalid Report Selection- 1, 2, 3"
               sel$, sel_d$ = " "
               return

L62655:     if sel% <> 1% then goto L63024
               sort_type$ = "Employee No."
               sel_d$ = "Report in Employee No Sequence"
               if beg_value$ <> " " then goto L62700
                  goto L62709
L62700:        if str(beg_value$,1%,3%) <> "ALL" then goto L62745
L62709:           beg_value$ = "ALL" : beg_value_d$ = "(ALL) Employee's"
                  end_value$, end_value_d$ = " "
                  goto L62934

L62745:           e_no$ = str(beg_value$,1%,5%)
                  convert e_no$ to e_no%, data goto L62961

                  convert e_no% to e_no$, pic(00000)
                  gosub dataload
                  if rec% = 0% then goto L62961
                  beg_value$ = e_no$
                  beg_value_d$ = e_lname$ & ", " & e_fname$
                  if end_value$ <> " " then goto L62862
                     end_value$ = beg_value$
                     end_value_d$ = beg_value_d$
                     goto L62934

L62862:           e_no$ = str(end_value$,1%,5%)
                  convert e_no$ to e_no%, data goto L62988

                  convert e_no% to e_no$, pic(00000)
                  gosub dataload
                  if rec% = 0% then goto L62988
                  end_value$ = e_no$
                  end_value_d$ = e_lname$ & ", " & e_fname$
L62934:     check% = 1%
            gosub check_shift
        return
L62961:     errormsg$ = "(Error) - Invalid Beginning Employee Number?"
            goto L62997

L62988:     errormsg$ = "(Error) - Invalid Ending Employee Number?"
L62997:     init(" ") beg_value$, beg_value_d$, end_value$,end_value_d$
        return
                                              /* Department/Prod Seq */
L63024:     if sel% <> 2% then goto L63393
               sort_type$ = "Last Name   "
               sel_d$ = "Report in Department Sequence "
               if beg_value$ <> " " then goto L63069
                  goto L63078
L63069:        if str(beg_value$,1%,3%) <> "ALL" then goto L63114
L63078:           beg_value$ = "ALL" : beg_value_d$ = "(ALL) Departments"
                  end_value$, end_value_d$ = " "
                  goto L63303

L63114:           e_dept$ = str(beg_value$,1%,3%)
                  convert e_dept$ to e_dept%, data goto L63330

                  convert e_dept% to e_dept$, pic(000)
                  gosub lookup_dept
                  if dept% = 0% then goto L63330
                  beg_value$ = e_dept$
                  beg_value_d$ = e_dept_d$
                  if end_value$ <> " " then goto L63231
                     end_value$ = beg_value$
                     end_value_d$ = beg_value_d$
                     goto L63303

L63231:           e_dept$ = str(end_value$,1%,3%)
                  convert e_dept$ to e_dept%, data goto L63357

                  convert e_dept% to e_dept$, pic(000)
                  gosub lookup_dept
                  if dept% = 0% then goto L63357
                  end_value$ = e_dept$
                  end_value_d$ = e_dept_d$
L63303:     check% = 1%
            gosub check_shift
        return
L63330:     errormsg$ = "(Error) - Invalid Beginning Department Code?"
            goto L63366

L63357:     errormsg$ = "(Error) - Invalid Ending Department Code?"
L63366:     init(" ") beg_value$, beg_value_d$, end_value$,end_value_d$
        return

L63393:                                          /* Last Name Sequence */
               sort_type$ = "Department  "
               sel_d$ = "Report in Last Name Sequence  "
               if beg_value$ <> " " then goto L63438
                  goto L63447
L63438:        if str(beg_value$,1%,3%) <> "ALL" then goto L63483
L63447:           beg_value$ = "ALL" : beg_value_d$ = "(ALL) Employee's "
                  end_value$, end_value_d$ = " "
                  goto L63636

L63483:           emp_key$ = all(hex(00))
                  emp_key$ = beg_value$
                  gosub lookup_name
                  if lname% = 0% then goto L63663
                  beg_value$   = e_lname$
                  beg_value_d$ = e_lname$ & ", " & e_fname$
                  if end_value$ <> " " then goto L63582
                     end_value$   = beg_value$
                     end_value_d$ = beg_value_d$
                     goto L63636

L63582:           emp_key$ = all(hex(00))
                  emp_key$ = end_value$
                  gosub lookup_name
                  if lname% = 0% then goto L63690
                  end_value$   = e_lname$
                  end_value_d$ = e_lname$ & ", " & e_fname$
L63636:     check% = 1%
            gosub check_shift
        return
L63663:     errormsg$ = "(Error) - Invalid Beginning Last Name Value?"
            goto L63699

L63690:     errormsg$ = "(Error) - Invalid Ending Last Name Value?"
L63699:     init(" ") beg_value$, beg_value_d$, end_value$,end_value_d$
        return

        check_shift
            if beg_shift$ <> " " then goto L63780
L63744:        beg_shift$ = "A "
               beg_shift_d$ = "(All) - Shift Codes "
               end_shift$, end_shift_d$ = " "
               return
L63780:     if str(beg_shift$,1%,1%) = "A" then goto L63744
               shift% = 1%
               e_shift$ = beg_shift$
               gosub get_shift
               beg_shift_d$ = e_shift_d$
               shift% = 0%
            if end_shift$ <> " " then goto L63870
               end_shift$ = beg_shift$
               end_shift_d$ = beg_shift_d$
               return
L63870:        shift% = 1%
               e_shift$ = end_shift$
               gosub get_shift
               end_shift_d$ = e_shift_d$
               shift% = 0%
        return

/* <AWD016> */
check_security
          /* sec_lvl$   0 = none (higher = more access)      */
          /*            1 = EMPACCESS (if dept OK)           */
          /*            2 = EMP SECUR                        */
          /*            3 = EMP GEN                          */
          /*            4 = EMP HR                           */
          /*            5 = user MVK                         */

/*SR71853*/ gosub load_security
            sec_lvl$ = "5"
            if userid$ = "RHF" then return
            if userid$ = "RFN" then return
            if userid$ = "KEF" then return
            if userid$ = "KFI" then return            
            if userid$ = "LGT" then return
            if userid$ = "LNT" then return        /*<AWD019>*/            
            if userid$ = "EAM" then return        /*SR #67402 */
            if userid$ = "CMG" then return
            if userid$ = "CGN" then return
            if userid$ = "RDB" then return        /* during ADP */
            if userid$ = "RBN" then return        /* during ADP */

            sec_lvl$ = "4"
            init(" ") readkey$
            str(readkey$,1%,9%)  = "EMP HR   "
            str(readkey$,10%,3%) = userid$
            read #2,key = readkey$, eod goto not_emp_hr
            return
not_emp_hr:
            sec_lvl$ = "3"
            str(readkey$,1%,9%)  = "EMP GEN  "
            str(readkey$,10%,3%) = userid$
            read #2,key = readkey$, eod goto not_emp_gen
            return
not_emp_gen:
            sec_lvl$ = "2"
            init(" ") readkey$, access$
            j% = 1% : access% = 0%
            gosub load_security
            if security% = 1% then return

            sec_lvl$ = "1"
            str(readkey$,1%,9%)   = "EMPACCESS"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L64590, access$, eod goto fini
            for kk% = 1% to 10%
                convert str(access$,j%,3%) to x%, data goto fini
                if e_dept$ = str(access$,j%,3%) then return
                j% = j% + 3%
            next kk%
fini:       sec_lvl$ = "0"
            gosub access_denied
            return clear all
            goto inputmode
/* </AWD016> */

        rolodex
          call "PROCLINK" ("APCROLEX","        ","      ",0%,0%)
        return clear all
        goto inputmode

        load_security
            init(" ") readkey$, access$, pay_grade_view$
            security% = 0%
            str(readkey$,1%,9%)   = "EMP SECUR"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L64460, access$, eod goto L64480
L64460:       FMT POS(25), CH(30)
            security% = 1%
/*SR71853*/ if str(access$,1%,1%) <> "0" then pay_grade_view$ = "N"
L64480: return

        load_access
            init(" ") readkey$, access$
            j% = 1% : access% = 0%
            gosub load_security
            if security% = 1% then return

            str(readkey$,1%,9%)   = "EMPACCESS"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L64590, access$, eod goto L64650
L64590:        FMT POS(25), CH(30)
            for kk% = 1% to 10%
                convert str(access$,j%,3%) to x%, data goto L64650
                if e_dept$ = str(access$,j%,3%) then goto L64660
                j% = j% + 3%
            next kk%
L64650:     return
            goto access_denied
L64660: security% = 1%  /* AWD011 */
        return

        emp_stats                         /* EWD005 - Add emp_stats routine */
           init(" ") readkey$
           emp_stats% = 0%
            str(readkey$,1%,9%)   = "EMP STATS"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, eod goto noemp_stats

            emp_stats% = 1%
        noemp_stats
        return

        access_denied
            comp% = 2%
            hdr$ = "*** Access Denied to Dept ***"
            msg$(1) = "You Do Not Have Access to Dept/Employee Selected?"
            msg$(2) = "      D e p a r t m e n t   S e c u r i t y      "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
            access% = 1%
            sec_lvl$ = "0"
        return

        delete_msg
            comp% = 2%
            hdr$ = "** Access Denied to Delete **"
            msg$(1) = "You Do Not Have Access to the Delete Function?   "
            msg$(2) = "      O n l y  (H. R.)  C a n   D e l e t e      "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
            access% = 1%
        return

                                                     /* (AWD008)   - Beg */
        load_security_hr
            init(" ") readkey$, access$
            security_hr% = 0%
            str(readkey$,1%,9%)   = "EMP HR   "
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L64460, access$, eod goto no_hr
            security_hr% = 1%
        no_hr
            str(readkey$,1%,9%)  = "EMP GEN  "
            str(readkey$,10%,3%) = userid$
            read #2,key = readkey$, using L64460, access$, eod goto no_hr_2
            security_hr% = 2%
        no_hr_2
        return

        load_pr_status
            read #3, key = e_no$, using L64670, pr_status$, eod goto ~
                                no_pr_status

L64670:                FMT POS(28) , CH(01)
        no_pr_status
        return
        

        
                                                            /* (AWD008) */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


