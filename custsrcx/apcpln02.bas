        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN02                             *~
            *  Creation Date     - 07/01/94                             *~
            *  Last Modified Date- 08/18/99                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program Defines the Seven       *~
            *                      Day Capacity for a Specified         *~
            *                      Production Year and Week for,        *~
            *                      Department, Process, Shift           *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                      (PLAN SORT) - Production Sort Codes  *~
            *                                                           *~
            *  Special Comments  - (APCPLN1B) - Subroutine to Lookup    *~
            *                                   and Display Planning    *~
            *                                   Tables.                 *~
            *                      (AWDPLN0B) - Subroutine to Calc      *~
            *                                   Current Planning Dates. *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/27/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/31/97 ! Mod for New Sort Default '02678LGN9A'    ! RHH *~
            * 11/13/97 ! Mod for Upgrade to New Release R6.04.03  ! RHH *~
            * 03/09/98 ! Y2K Changes                              ! LDJ *~
            * 12/16/98 ! (EWD001) Mod to Sort Codes 12 to 15      ! RHH *~ 
            * 08/18/99 ! (EWD002) - Mod to replace 'OPENCHCK'     ! RHH *~
            *          !            with new 'EWDOPEN' Subroutine !     *~
            *          !            to improve HP speed.          !     *~
            * 11/27/00 ! (EWD003) Mod to correct write of sort    ! CMG *~
            *          !              codes 12 to 15              !     *~     
            *02/11/2019! CR01894  Increase EMP DEPT size.         ! DES *~ 
            *01/02/2020! CR2371  Year 2020 issues with capacity   ! CMG *~
            *************************************************************

        dim                              /* (APCPLNUC) - FILE          */~
            filename$8,                  /* Used By EWDOPEN    (EWD002)*/~
            pl_yr$2,    pl_wk$2,         /* Production Year and Week   */~
            pl_year$4,                   /* Production Year - Display       (Y2K, LDJ) */~
            pl_shft$2,  pl_shft_d$30,    /* Planning Shift Cd-PLAN SHFT*/~
            pl_dept$3,  pl_dept_d$30,    /* Planning Dept Cd- PLAN DEPT*/~
            pl_proc$2,  pl_proc_d$30,    /* Planning Proc Cd- PLAN PROC*/~
/*EWD003*/  pl_sort$(12)1, pl_sort1$(3)1,/* Prod Sort Codes  -PLAN SORT*/~
            pl_sorts$15, pl_sorts_d$79,  /* Production Sort (EWD001)   */~
            pl_day$1,   pl_day_d$9,      /* Production Start Day       */~
            pl_unts(7%),pl_unts$(7%)10,  /* Production Capacity        */~
            pl_unta%(7%),pl_unta$(7%)5,  /* Production Capacity        */~
            pl_untp%(7%),pl_untp$(7%)5,  /* Production Capacity        */~
            t1(7%), t2%(7%), t3%(7%),    /* Calculate totals Dept      */~
            pl_dte$6, pl_date$8,         /* Prod Week Start Date       */~
            pl_key$11, sav_key$11,       /*                            */~
            pl_key1$11, cnt$25,          /*                            */~
            current$13, sav_dept$3,      /*                            */~
            pl_fill$19                   /* Filler Area                */


        dim                              /* (Program) - Variables      */~
            hdr$40, msg$(3%)79,          /* Askuser - Var's            */~
            cur_yr$2, prv_yr$2,          /* Current and Previous Year  */~
            cur_year$4, prv_year$4,      /* Display versions of above       (Y2K, LDJ) */~
            cur_wk$2, cur_dy$1,          /* Current Week and Day       */~
            cur_dte$6, cur_date$8,       /* Calc of Prod. Date         */~
            ent_yr$2,                    /* Entry Year                 */~
            ent_year$4,                  /* Entry Year 4 display            (Y2K, LDJ) */~
            ent_wk$2, ent_dy$1,          /* Entry Week and Day         */~
            ent_dte$6, ent_date$8,       /* Entry Calc of Prod. Date   */~
            day$(14%)9,                  /* Calc Production Day        */~
            scr$(12%)50, days$(7%)9,     /* Screen and Days of Week    */~
            tab$(10%)10,                 /* Save Code Table Names      */~
            desc$30,                     /* Table Value, Description   */~
            code$15,                     /* Use To Look-Up Table Code  */~
            title$40, date$8,            /* Report Title               */~
            datecc$8,                    /* Today's Date as YYYYMMDD        (Y2K, LDJ) */~
            rp_yr$2,                     /* Report Planning Year       */~
            rp_year$4,                   /* Report Year for Display         (Y2K, LDJ) */~
            rp_wk$2, rp_dte$6, rp_date$8,/* Report Production Week     */~
            bg_yr$2, ed_yr$2,            /* Copy From/To Year          */~
            bg_year$4, ed_year$4,        /* as above 4 display              (Y2K, LDJ) */~
            bg_wk$2, ed_wk$2,            /* Copy From/To Week          */~
            bg_wk_d$8, ed_wk_d$8,        /* Copy From/To Week Date     */~
            bg_dept$3, bg_dept_d$30,     /* Report Beg Department      */~
            ed_dept$3, ed_dept_d$30,     /* Report Ending Department   */~
            bg_proc$2, bg_proc_d$30,     /* Report Beg Process Code    */~
            ed_proc$2, ed_proc_d$30,     /* Report End Process Code    */~
            bg_shft$2, bg_shft_d$30,     /* Report Beg Shift Code      */~
            ed_shft$2, ed_shft_d$30,     /* Report End Shift Code      */~
            fr_yr$2, to_yr$2,            /* Copy From/To Prod Year     */~
            fr_year$4, to_year$4,        /* As above for display            (Y2K, LDJ) */~
            fr_wk$2, to_wk$2,            /* Copy From/To Prod Week     */~
            pl_rec$128,                  /* Planning Units Capacity Rec*/~
            emp_key$10,                  /* Employee Master Lookup Key */~
            emp_status$1,                /* Employee Status            */~
            emp_shft$2,                  /* Employee Shift Code        */~
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

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = " Planning Master Unit Capacity Edit/Rept "
            pname$ = "APCPLN02 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNUC ! Planning Master Unit Capacity File       *~
            * #2  ! APCPLNDP ! Planning Master Department File          *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #4  ! APCPLNUC ! Planning Master Unit Capacity File - Copy*~
            * #5  ! APCEMPLY ! Employee Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNUC",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 11,                      ~
                        alt key 1, keypos =  7, keylen = 11

            select #2,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #4,  "APCPLNUC",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 11,                      ~
                        alt key 1, keypos =  7, keylen = 11

            select #5,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =  7,   keylen =  5,                      ~
                        alt key 1, keypos =  1, keylen = 11, dup,        ~
                            key 2, keypos = 12, keylen = 26, dup

            call "SHOSTAT" ("Initialization")

                                                        /* (EWD002)    */
            filename$ = "APCPLNUC" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNUC" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEMPLY" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
                                                        /* (EWD002)    */ 
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)

            tab$(1%) = "PLAN DEPT" : tab$(2%) = "PLAN PROC"
            tab$(3%) = "MODEL    " : tab$(4%) = "PLAN SHFT"
            tab$(5%) = "PLAN UNIT" : tab$(6%) = "PLAN DATE"
            tab$(7%) = "PLAN STAT" : tab$(8%) = "PLAN SORT"

            day$( 1%) = "Monday   "
            day$( 2%) = "Tuesday  "
            day$( 3%) = "Wednesday"
            day$( 4%) = "Thursday "
            day$( 5%) = "Friday   "
            day$( 6%) = "Saturday "
            day$( 7%) = "Sunday   "
            day$( 8%) = "Monday   "
            day$( 9%) = "Tuesday  "
            day$(10%) = "Wednesday"
            day$(11%) = "Thursday "
            day$(12%) = "Friday   "
            day$(13%) = "Saturday "
            day$(14%) = "Sunday   "

          days$(1%) = "MONDAY   "
          days$(2%) = "TUESDAY  "
          days$(3%) = "WEDNESDAY"
          days$(4%) = "THURSDAY "
          days$(5%) = "FRIDAY   "
          days$(6%) = "SATURDAY "
          days$(7%) = "SUNDAY   "

          date$ = date                       /* Set The Current Date   */
          call "DATEFMT" (date$,x%,datecc$)                                   /* (Y2K, LDJ) */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 8%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% and fieldnr% = 1% then report_input
                      if keyhit% = 11% and fieldnr% = 1% then copy_input
                      if keyhit% <> 0% then       L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  = 12% then gosub delete_record
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 3%
            zz% = cursor%(1%)           /* Edit Planning Bucket Values */
            if zz% < 11% then goto L11210
               yy% = cursor%(2%)
               if yy% < 47% then fieldnr% = 8%
               if yy% > 45% and yy% < 57% then fieldnr% = 9%
               if yy% > 57% then fieldnr% = 10%

L11210:     if fieldnr% < 1% or fieldnr% > 10% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11250:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11250
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11250
                  lastfieldnr% = fieldnr%
            goto L11130
        REM *************************************************************~
            *      I N P U T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        report_input
            gosub initialize_variables

            for fieldnr% = 1% to 8%
L12080:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12200
L12100:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12180
L12130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L12100
                         if fieldnr% = 1% then L12080
                         goto L12130
L12180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12100
L12200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12100
            next fieldnr%

        REM *************************************************************~
            *       E D I T   M O D E   R E P O R T   S C R E E N       *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L13110:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 8% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13160:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13160
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13160
                  lastfieldnr% = fieldnr%
            goto L13110

        REM *************************************************************~
            *      I N P U T   M O D E   C o p y   S c r e e n          *~
            *************************************************************

        copy_input
            gosub initialize_variables

            for fieldnr% = 1% to 10%
L14080:         gosub'071(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L14200
L14100:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L14180
L14130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L14100
                         if fieldnr% = 1% then L14080
                         goto L14130
L14180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L14100
L14200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14100
            next fieldnr%

        REM *************************************************************~
            *       E D I T   M O D E   C o p y   S c r e e n           *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub copy_data
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg3
L15110:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 10% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'071(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L15160:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15160
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L15160
                  lastfieldnr% = fieldnr%
            goto L15110

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
            call "SHOSTAT" ("Printing Report")
            title$ = " Planning Master Units Capacity Report  "
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
        deffn'061(fieldnr%)
        deffn'071(fieldnr%)
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
         "Enter a Valid Planning Production Year or <Return> = Current?",~
         "Enter a Valid Planning Production Week or <Return> = Current?",~
         "Enter a Valid Planning Department Code?                      ",~
         "Enter a Valid Planning Process Code?                         ",~
         "Enter a Valid Planning Shift Code?                           ",~
         "Enter Planning Production Week 'Start Day', (1 thru 7)?      ",~
         "Enter Production Sort Codes for Reports?                     ",~
         "Enter Planning Capacities for Each Day (1 thru 7) of Week?   ",~
         "Enter No. of Planned Windows for Each Day (1 thru 7) of Week?",~
         "Enter No. of Produced Windows for Each Day (1 thru 7) of Week"

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28330
                inpmessage$ = edtmessage$
                return

L28330
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter the Planning Production Year??                         ",~
         "Enter the Planning Production Week??                         ",~
         "Enter a Beginning Department Code or ALL??                   ",~
         "Enter a Ending Department Code??                             ",~
         "Enter a Beginning Process Code or AA for (ALL)??             ",~
         "Enter a Ending Process Code??                                ",~
         "Enter a Beginning Shift Code or AA for (ALL)??               ",~
         "Enter a Ending Shift Code??                                  "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28530
                inpmessage$ = edtmessage$
                return

L28530
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter the 'Copy From' Production Year ??                     ",~
         "Enter the 'Copy To' Production Year??                        ",~
         "Enter the 'Copy From' Production Week??                      ",~
         "Enter the 'Copy To' Production Week??                        ",~
         "Enter the 'Copy From' Department Code or (All)??             ",~
         "Enter the 'Copy To' Department Code or (All)??               ",~
         "Enter the 'Copy From' Process Code or (All)??                ",~
         "Enter the 'Copy To' Process Code or (All)??                  ",~
         "Enter the 'Copy From' Shift Code or (All)??                  ",~
         "Enter the 'Copy To' Shift Code or (All)??                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, pl_yr$, pl_year$, pl_wk$,  ~
                      pl_shft$, pl_dept$, pl_proc$, pl_sorts$, pl_day$,  ~
                      pl_dte$, pl_date$, pl_dept_d$, pl_proc_d$,         ~
                      pl_shft_d$, pl_sorts_d$, pl_unts$(), pl_unta$(),   ~
                      pl_untp$(), rp_yr$, rp_year$, rp_wk$, rp_dte$, rp_date$,     ~
                      bg_dept$, bg_dept_d$, ed_dept$, ed_dept_d$,        ~
                      bg_proc$, bg_proc_d$, ed_proc$, ed_proc_d$,        ~
                      bg_shft$, bg_shft_d$, ed_shft$, ed_shft_d$,        ~
                      current$, fr_yr$, fr_year$, to_yr$, to_year$, fr_wk$, to_wk$, pl_rec$, ~
                      pl_sort$(), pl_day_d$, bg_yr$, bg_year$, ed_yr$, ed_year$, bg_wk$,     ~
                      bg_wk_d$, ed_wk$, ed_wk_d$, emp_key$, emp_shft$,   ~
                      emp_status$, pl_sort1$()                                               /* (Y2K, LDJ) */
            rec%   = 0%
            total% = 0%
            copy%  = 0%
            mat pl_unts  = zer
            mat pl_unta% = zer
            mat pl_untp% = zer
            for i% = 1% to 7%
                pl_unts$(i%) = "     0.00 "
                pl_unta$(i%) = "    0"
                pl_untp$(i%) = "    0"
            next i%
        REM                !-- 6        !-- 19        !-- 33    !-- 43
        REM                v            v             v         v
          scr$( 1%)= "!Production Day !  Capacity  ! Planned ! Produced!"
          scr$( 2%)= "!---------------!------------!---------!---------!"
          scr$( 3%)= "!(1) Monday     ! XXXXXXXXXX !  XXXXX  !  XXXXX  !"
          scr$( 4%)= "!(2) Tuesday    !            !         !         !"
          scr$( 5%)= "!(3) Wednesday  !            !         !         !"
          scr$( 6%)= "!(4) Thursday   !            !         !         !"
          scr$( 7%)= "!(5) Friday     !            !         !         !"
          scr$( 8%)= "!(6) Saturday   !            !         !         !"
          scr$( 9%)= "!(7) Sunday     !            !         !         !"
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
            pl_key$ = " "
            str(pl_key$,1%,2%)  = pl_yr$
            str(pl_key$,3%,2%)  = pl_wk$
            str(pl_key$,5%,2%)  = pl_shft$
            str(pl_key$,7%,3%)  = pl_dept$
            str(pl_key$,10%,2%) = pl_proc$
            read #1,hold,key = pl_key$, eod goto L30420
        dataload_rpt
            init(" ") pl_sorts$, pl_sorts_d$
            get #1, using L35040, pl_yr$, pl_wk$, pl_shft$, pl_dept$,     ~
                                 pl_proc$, pl_shft$, pl_yr$, pl_wk$,     ~
                                 pl_sort$(), pl_day$, pl_unts(),         ~
                                 pl_unta%(),pl_untp%(), pl_dte$,         ~
                                 pl_sort1$(), pl_fill$

            convert val(pl_yr$,2) to pl_year$, pic(0000)                    /* (Y2K, LDJ) */
            rec% = 1%
            for xx% =1% to 12%
                str(pl_sorts$,xx%,1%) = pl_sort$(xx%)
            next xx%
                                                   /* (EWD001)          */
            for xx% =13% to 15%
                str(pl_sorts$,xx%,1%) = pl_sort1$(xx%-12%)
            next xx%
                                                   /* (EWD001)          */
            gosub L50155                           /* Production Week   */
            gosub L50395                           /* Department Codes  */
            gosub L50475                           /* Process Codes     */
            gosub L50555                           /* Shift Codes       */
            gosub L50690                           /* Prod Sort Codes   */
            gosub L50740                           /* Start Day Prod    */

            for i% = 1% to 7%
                convert pl_unts(i%) to pl_unts$(i%),  pic(######.##-)

                convert pl_unta%(i%) to pl_unta$(i%), pic(#####)

                convert pl_untp%(i%) to pl_untp$(i%), pic(#####)

            next i%
L30420: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_record
            call "SHOSTAT" ("Deleting Dept ("&pl_dept$&") Shift ("&      ~
                                              pl_shft$&")" )
            goto L31130
        dataput
            call "SHOSTAT" ("Updating Dept ("&pl_dept$&") Shift ("&      ~
                                              pl_shft$&")" )
L31130:     pl_key$ = " "
            str(pl_key$,1%,2%)  = pl_yr$
            str(pl_key$,3%,2%)  = pl_wk$
            str(pl_key$,5%,2%)  = pl_shft$
            str(pl_key$,7%,3%)  = pl_dept$
            str(pl_key$,10%,2%) = pl_proc$
            read #1,hold,key = pl_key$, eod goto L31230
               delete #1
               if keyhit% = 12% then goto L31290

L31230:     put #1, using L35040, pl_yr$, pl_wk$, pl_shft$, pl_dept$,     ~
                                 pl_proc$, pl_shft$, pl_yr$, pl_wk$,     ~
                                 pl_sort$(), pl_day$, pl_unts(),         ~
                                 pl_unta%(),pl_untp%(), pl_dte$,         ~
                                 pl_sort1$(), pl_fill$

            write #1, eod goto L31310
L31290: return clear all
        goto inputmode
L31310:     errormsg$ = "(Error)-Unable to Update Dept. Capacities? " &  ~
                        pl_key$
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (APCPLNUC)          */
L35040: FMT CH(02),                      /* Planning Production Year   */~
            CH(02),                      /* Planning Production Week   */~
            CH(02),                      /* Shift Code        PLAN SHFT*/~
            CH(03),                      /* Department Code   PLAN DEPT*/~
            CH(02),                      /* Process Code      PLAN PROC*/~
            CH(02),                      /* Shift Code        PLAN SHFT*/~
            CH(02),                      /* Planning Production Year   */~
            CH(02),                      /* Planning Production Week   */~
            12*CH(01),                   /* Production Sort Codes      */~
            CH(01),                      /* Plan Prod. Wk Start Day 1-7*/~
            7*PD(14,4),                  /* Plan Sched. Capacity 7 Days*/~
            7*BI(2),                     /* Plan Actual Windows Sched. */~
            7*BI(2),                     /* Plan Windows Produced 7 Day*/~
            CH(08),                      /* Plan Prod Date. Monday     */~
/*EWD003*/  3*CH(01),                    /* Codes 13,14,15    (EWD001) */~
            CH(03)                       /* Filler Area                */

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
              on fieldnr% gosub L40260,         /* Production Year      */~
                                L40260,         /* Production Week      */~
                                L40260,         /* Department Code      */~
                                L40260,         /* Process Code         */~
                                L40260,         /* Shift Code           */~
                                L40260,         /* Prod. Start Day      */~
                                L40250,         /* Prod. Sort Codes     */~
                                L40260,         /* Capacity             */~
                                L40260,         /* Planned              */~
                                L40260          /* Produced             */
              goto L40280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40260:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40280:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Planning Production Year  :",                ~
               at (04,30), fac(lfac$(1%)), pl_year$             , ch(04),       /* (Y2K, LDJ) */~
                                                                         ~
               at (05,02), "Planning Production Week  :",                ~
               at (05,30), fac(lfac$(2%)), pl_wk$               , ch(02),~
               at (05,40), fac(hex(84)), pl_date$               , ch(08),~
               at (05,55), fac(hex(84)), current$               , ch(13),~
                                                                         ~
               at (06,02), "Planning Department Code  :",                ~
               at (06,30), fac(lfac$(3%)), pl_dept$             , ch(03),~
               at (06,40), fac(hex(84)), pl_dept_d$             , ch(30),~
                                                                         ~
               at (07,02), "Planning Process Code     :",                ~
               at (07,30), fac(lfac$(4%)), pl_proc$             , ch(02),~
               at (07,40), fac(hex(84)), pl_proc_d$             , ch(30),~
                                                                         ~
               at (08,02), "Planning Shift Code       :",                ~
               at (08,30), fac(lfac$(5%)), pl_shft$             , ch(02),~
               at (08,40), fac(hex(84)), pl_shft_d$             , ch(30),~
                                                                         ~
               at (09,02), "Production Week Start Day :",                ~
               at (09,30), fac(lfac$(6%)), pl_day$              , ch(01),~
               at (09,40), fac(hex(84)), pl_day_d$              , ch(09),~
                                                                         ~
               at (10,02), "Production Sort Codes     :",                ~
               at (10,30), fac(lfac$(7%)), pl_sorts$            , ch(15),~
               at (11,02), fac(hex(84)), pl_sorts_d$            , ch(79),~
               at (12,16), fac(hex(84)), scr$( 1%)              , ch(50),~
               at (13,16), fac(hex(84)), scr$( 2%)              , ch(50),~
               at (14,16), fac(hex(84)), scr$( 3%)              , ch(50),~
               at (15,16), fac(hex(84)), scr$( 4%)              , ch(50),~
               at (16,16), fac(hex(84)), scr$( 5%)              , ch(50),~
               at (17,16), fac(hex(84)), scr$( 6%)              , ch(50),~
               at (18,16), fac(hex(84)), scr$( 7%)              , ch(50),~
               at (19,16), fac(hex(84)), scr$( 8%)              , ch(50),~
               at (20,16), fac(hex(84)), scr$( 9%)              , ch(50),~
                                                                         ~
               at (14,34), fac(lfac$(8%)), pl_unts$( 1%)        , ch(10),~
               at (15,34), fac(lfac$(8%)), pl_unts$( 2%)        , ch(10),~
               at (16,34), fac(lfac$(8%)), pl_unts$( 3%)        , ch(10),~
               at (17,34), fac(lfac$(8%)), pl_unts$( 4%)        , ch(10),~
               at (18,34), fac(lfac$(8%)), pl_unts$( 5%)        , ch(10),~
               at (19,34), fac(lfac$(8%)), pl_unts$( 6%)        , ch(10),~
               at (20,34), fac(lfac$(8%)), pl_unts$( 7%)        , ch(10),~
                                                                         ~
               at (14,49), fac(lfac$(9%)), pl_unta$( 1%)        , ch(05),~
               at (15,49), fac(lfac$(9%)), pl_unta$( 2%)        , ch(05),~
               at (16,49), fac(lfac$(9%)), pl_unta$( 3%)        , ch(05),~
               at (17,49), fac(lfac$(9%)), pl_unta$( 4%)        , ch(05),~
               at (18,49), fac(lfac$(9%)), pl_unta$( 5%)        , ch(05),~
               at (19,49), fac(lfac$(9%)), pl_unta$( 6%)        , ch(05),~
               at (20,49), fac(lfac$(9%)), pl_unta$( 7%)        , ch(05),~
                                                                         ~
               at (14,59), fac(lfac$(10)), pl_untp$( 1%)        , ch(05),~
               at (15,59), fac(lfac$(10)), pl_untp$( 2%)        , ch(05),~
               at (16,59), fac(lfac$(10)), pl_untp$( 3%)        , ch(05),~
               at (17,59), fac(lfac$(10)), pl_untp$( 4%)        , ch(05),~
               at (18,59), fac(lfac$(10)), pl_untp$( 5%)        , ch(05),~
               at (19,59), fac(lfac$(10)), pl_untp$( 6%)        , ch(05),~
               at (20,59), fac(lfac$(10)), pl_untp$( 7%)        , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 8% then goto L41030
                  gosub calc_capacity
                  goto L41090

L41030:        if keyhit% < 6% or keyhit% > 10% then goto L41090
                  tab% = keyhit% - 5%
                  if tab% = 5% then tab% = 8%
                  gosub display_codes
                  goto L40070

L41090:        if keyhit% <> 11 then goto L41130
                  if edit% <> 2% then goto L41130
                  gosub total_all
                  goto L40070

L41130:        if keyhit% <> 15 then goto L41170
                  call "PRNTSCRN"
                  goto L40070

L41170:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            apc$   = " Planning Master Unit Capacity (*Input*) "
            if total% = 0% then goto L41260
               fieldnr% = 0%
               edit% = 2%
               apc$   = " Planning Master Unit Capacity (*Total*) "
L41260:
        if edit% = 2% then L41430     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (14)Print Report"
            pf$(2) = "(4)Previous Field  (7)Process Codes     " &        ~
                     "(10)Sort Codes         (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(11)Copy Data          (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ff090a0bffff0e0f1000)
            if fieldnr% = 1% then L41390
                str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),40%) = " " : str(pfkeys$,11%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41390:     if fieldnr% > 1% then L41410
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41410: return

L41430: if fieldnr% > 0% then L41570  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (12)Delete Rec. "
            pf$(2) = "                   (7)Process Codes     " &        ~
                     "(10)Sort Codes         (15)Print Screen"
            pf$(3) = "                   (8)Calc Capacity     " &        ~
                     "(11)Total All Shifts   (16)Update Data "
            pfkeys$ = hex(01ffffffff060708090a0b0cff0e0f1000)
            if rec% = 1% then goto L41530
                str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41530:     if total% = 0% then goto L41560
                str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41560: return
L41570:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
L42070:       gosub set_pf2
              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42230,         /* Planning Year        */~
                                L42230,         /* Planning Week        */~
                                L42230,         /* Beg Department Code  */~
                                L42230,         /* End Department Code  */~
                                L42230,         /* Beg Process Code     */~
                                L42230,         /* End Process Code     */~
                                L42230,         /* Beg Shift Code       */~
                                L42230          /* End Shift Code       */

              goto L42260

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,57), "(Report) Today:",                            ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Planning Production Year  :",                ~
               at (04,30), fac(lfac$(1%)), rp_year$             , ch(04),       /* (Y2K, LDJ) */~
                                                                         ~
               at (05,02), "Planning Production Week  :",                ~
               at (05,30), fac(lfac$(2%)), rp_wk$               , ch(02),~
               at (05,40), fac(hex(84)), rp_date$               , ch(08),~
                                                                         ~
               at (06,02), "Beginning Department Code :",                ~
               at (06,30), fac(lfac$(3%)), bg_dept$             , ch(03),~
               at (06,40), fac(hex(84)), bg_dept_d$             , ch(30),~
                                                                         ~
               at (07,02), "Ending Department Code    :",                ~
               at (07,30), fac(lfac$(4%)), ed_dept$             , ch(03),~
               at (07,40), fac(hex(84)), ed_dept_d$             , ch(30),~
                                                                         ~
               at (08,02), "Beginning Process Code    :",                ~
               at (08,30), fac(lfac$(5%)), bg_proc$             , ch(02),~
               at (08,40), fac(hex(84)), bg_proc_d$             , ch(30),~
                                                                         ~
               at (09,02), "Ending Process Code       :",                ~
               at (09,30), fac(lfac$(6%)), ed_proc$             , ch(02),~
               at (09,40), fac(hex(84)), ed_proc_d$             , ch(30),~
                                                                         ~
               at (10,02), "Beginning Shift Code      :",                ~
               at (10,30), fac(lfac$(7%)), bg_shft$             , ch(02),~
               at (10,40), fac(hex(84)), bg_shft_d$             , ch(30),~
                                                                         ~
               at (11,02), "Ending Shift Code         :",                ~
               at (11,30), fac(lfac$(8%)), ed_shft$             , ch(02),~
               at (11,40), fac(hex(84)), ed_shft_d$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% < 6% or keyhit% > 10% then goto L42770
                  tab% = keyhit% - 5%
                  if tab% = 5% then tab% = 8%
                  gosub display_codes
                  goto L42070

L42770:        if keyhit% <> 15 then goto L42810
                  call "PRNTSCRN"
                  goto L42260

L42810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            apc$   = " Planning Master Unit Capacity (*Report*)"

        if edit% = 2% then L43020     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes                         "
            pf$(2) = "(4)Previous Field  (7)Process Codes     " &        ~
                     "(10)Sort Codes         (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ff090affffff0e0f1000)
            if fieldnr% = 1% then L42980
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42980:     if fieldnr% > 1% then L43000
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43000: return

L43020: if fieldnr% > 0% then L43110  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (14)Print Report"
            pf$(2) = "                   (7)Process Codes     " &        ~
                     "(10)Sort Codes         (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffff0607ff090aff0cff0e0f1000)
        return
L43110:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes
            call "APCPLN1B" (tab%, #3)
        return

        REM *************************************************************~
            *    C O P Y   P R O D U C T I O N   W E E K   S C R E E N  *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub set_pf4

              gosub'070(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              gosub L46230                      /* Beg Prod Year        */
                                               /* End Prod Year        */
                                               /* Beg Prod Week        */
                                               /* End Prod Week        */
                                               /* Beg Department       */
                                               /* End Department       */
                                               /* Beg Process          */
                                               /* End process          */
                                               /* Beg Shift            */
                                               /* End End Shift        */
              goto L46260

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L46230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L46260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,57), "( Copy ) Today:",                            ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Copy From Production Year :",                ~
               at (04,30), fac(lfac$(1%)), bg_year$             , ch(04),       /* (Y2K, LDJ) */~
                                                                         ~
               at (05,02), "Copy To Production Year   :",                ~
               at (05,30), fac(lfac$(2%)), ed_year$             , ch(04),       /* (Y2K, LDJ) */~
                                                                         ~
               at (06,02), "Copy From Production Week :",                ~
               at (06,30), fac(lfac$(3%)), bg_wk$               , ch(02),~
               at (06,40), fac(hex(84)), bg_wk_d$               , ch(08),~
               at (06,55), fac(hex(84)), current$               , ch(13),~
                                                                         ~
               at (07,02), "Copy To Production Week   :",                ~
               at (07,30), fac(lfac$(4%)), ed_wk$               , ch(02),~
               at (07,40), fac(hex(84)), ed_wk_d$               , ch(08),~
                                                                         ~
               at (08,02), "Copy From Department Code :",                ~
               at (08,30), fac(lfac$(5%)), bg_dept$             , ch(03),~
               at (08,40), fac(hex(84)), bg_dept_d$             , ch(30),~
                                                                         ~
               at (09,02), "Copy To Department Code   :",                ~
               at (09,30), fac(lfac$(6%)), ed_dept$             , ch(03),~
               at (09,40), fac(hex(84)), ed_dept_d$             , ch(30),~
                                                                         ~
               at (10,02), "Copy From Process Code    :",                ~
               at (10,30), fac(lfac$(7%)), bg_proc$             , ch(02),~
               at (10,40), fac(hex(84)), bg_proc_d$             , ch(30),~
                                                                         ~
               at (11,02), "Copy To Process Code      :",                ~
               at (11,30), fac(lfac$(8%)), ed_proc$             , ch(02),~
               at (11,40), fac(hex(84)), ed_proc_d$             , ch(30),~
                                                                         ~
               at (12,02), "Copy From Shift Code      :",                ~
               at (12,30), fac(lfac$(9%)), bg_shft$             , ch(02),~
               at (12,40), fac(hex(84)), bg_shft_d$             , ch(30),~
                                                                         ~
               at (13,02), "Copy To Shift Code        :",                ~
               at (13,30), fac(lfac$(10%)), ed_shft$            , ch(02),~
               at (13,40), fac(hex(84)), ed_shft_d$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L46890
                  call "PRNTSCRN"
                  goto L46260

L46890:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
            apc$   = " Planning Master Unit Capacity (*Copy*)  "

        if edit% = 2% then L47100     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "(4)Previous                             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L47060
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L47060:     if fieldnr% > 1% then L43000
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

L47100: if fieldnr% > 0% then L47190  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Copy Data   "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)
        return
L47190:
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
            on fieldnr% gosub L50100,         /* Planning Prod. Year   */ ~
                              L50155,         /* Planning Prod. Week   */ ~
                              L50395,         /* Department Code       */ ~
                              L50475,         /* Process Code          */ ~
                              L50555,         /* Shift Code            */ ~
                              L50740,         /* Production Start Day  */ ~
                              L50690,         /* Production Sort Codes */ ~
                              L50825,         /* Production Capacities */ ~
                              L50895,         /* Production Planned    */ ~
                              L50965          /* Production Produced   */
            return

L50100: REM Planning Production Year
            if pl_year$ <> " " then goto L50115                             /* (Y2K, LDJ) */
               pl_year$ = str(datecc$,1%,4%)                                /* (Y2K, LDJ) */          
L50115:     convert pl_year$ to pl_yr%, data goto L50135                    /* (Y2K, LDJ) */

REM       if pl_yr% < 1994% or pl_yr% >= 2020% then goto L50135  /* CR2371 */
            pl_yr$ = bin(pl_yr%,2)                                          /* (Y2K, LDJ) */
        return
L50135:     errormsg$ = "(Error) - Invalid Production Year??"
            init(" ") pl_year$                                              /* (Y2K, LDJ) */
        return

L50155: REM Planning Production Week         PL_WK$, PL_DTE$, PL_DATE$
            ent_yr$ = pl_yr$
            ent_wk$ = pl_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */
           
           if pl_e% <> 0% then goto L50375
           convert val(cur_yr$,2) to cur_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(ent_yr$,2) to ent_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(prv_yr$,2) to prv_year$,pic(0000)                        /* (Y2K, LDJ) */
           pl_wk$   = ent_wk$
           pl_dte$  = ent_dte$
           pl_date$ = ent_date$
           convert ent_year$ to x%, data goto L50260     /* ENTRY YEAR    */    /* (Y2K, LDJ) */
L50260:
           convert cur_year$ to y%, data goto L50270     /* CURRENT YEAR  */    /* (Y2K, LDJ) */
L50270:
           zz% = (x% - y%) * 52%

           x% = 0% : y% = 0%
           convert ent_wk$ to x%, data goto L50375     /* ENTRY WEEK    */

           convert cur_wk$ to y%, data goto L50305     /* CURRENT WEEK  */
L50305:
           x% = x% + zz%

           current$ = "Current: +XXX"
           if x% <> y% then goto L50340
              str(current$,11%,3%) = "000"
              goto L50370
L50340:    if x% > y% then goto L50360
              str(current$,10%,1%) = "-"                /* Previous Wk */
              convert (y%-x%) to str(current$,11%,3%), pic(000)
              goto L50370                                /* FUTURE WK   */
L50360:    convert (x%-y%) to str(current$,11%,3%), pic(000)

L50370: return
L50375:    errormsg$ = "(Error) - Invalid Production Week (1 thru 53)?"
           init(" ") pl_wk$, pl_dte$, pl_date$
        return

L50395: REM Department Code                       PL_DEPT$, PL_DEPT_D$
            if pl_dept$ <> " " then goto L50415
               pl_dept$ = "000"

L50415:     convert pl_dept$ to x%, data goto L50430

            convert x% to pl_dept$, pic(000)
L50430:     code$ = pl_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L50455
            pl_dept_d$ = desc$
        return
L50455:     errormsg$ = "(Error) - Invalid Department Code ??"
            init(" ") pl_dept$, pl_dept_d$
        return

L50475: REM Processing Code                       PL_PROC$, PL_PROC_D$
            if pl_proc$ <> " " then goto L50495
               pl_proc$ = "01"

L50495:     convert pl_proc$ to x%, data goto L50510

            convert x% to pl_proc$, pic(00)
L50510:     code$ = pl_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L50535
            pl_proc_d$ = desc$
        return
L50535:     errormsg$ = "(Error) - Invalid Processing Code ??"
            init(" ") pl_proc$, pl_proc_d$
        return

L50555: REM Planning Shift Code                   PL_SHFT$, PL_SHFT_D$
            if pl_shft$ <> " " then goto L50575
               pl_shft$ = "01"

L50575:     convert pl_shft$ to x%, data goto L50590

            convert x% to pl_shft$, pic(00)
L50590:     code$ = pl_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L50655
            pl_shft_d$ = desc$
            if edit% = 2% then return
               gosub check_dept
               if check% = 0% then goto L50670
               if rec% = 1% then return
               gosub dataload
               if rec% = 0% then return
               fieldnr% = 11%
        REM    REC% = 0%
        return
L50655:     errormsg$ = "(Error) - Invalid Shift Code ??"
            init(" ") pl_shft$, pl_shft_d$
        return
L50670:     errormsg$ = "(Error) - No Product Defined for Dept,Proc,Shift"
            init(" ") pl_shft$, pl_shft_d$
        return

L50690: REM Production Sort Codes                 PL_SORTS$,PL_SORTS_D$
            if str(pl_sorts$,1%,1%) <> " " then goto L50705
               str(pl_sorts$,1%,10%) = "02678LGN9A"
L50705:     gosub build_sort
            if sort% <> 0% then goto L50720
            if edit% = 1% and rec% = 0% then gosub calc_capacity
        return
L50720:     errormsg$ = "(Error) - Invalid Production Sort Code??"
            init(" ") pl_sort$(),pl_sorts_d$, pl_sorts$
        return

L50740: REM Planning Production Day            PL_DAY$, PL_DAY_D$
            if pl_day$ <> " " then goto L50760
               pl_day$ = "1"

L50760:     convert pl_day$ to pl_day%, data goto L50805

            if pl_day% < 1% or pl_day% > 7% then goto L50805
               pl_day_d$ = days$(pl_day%)
            for i% = 0% to 6%
                str(scr$(3% + i%),6%,9%) = day$(pl_day% + i%)
            next i%

        return
L50805:     errormsg$ = "(Error) - Invalid Production Start Day??"
            init(" ") pl_day$, pl_day_d$
        return

L50825: REM Planning Production Capacity          PL_UNTS(), PL_UNTS$()
            mat pl_unts = zer
            for i% = 1% to 7%
                convert pl_unts$(i%) to pl_unts(i%), data goto L50865

                convert pl_unts(i%) to pl_unts$(i%), pic(######.##-)
            next i%
        return
L50865:     errormsg$ = "(Error) - Invalid Capacity Value for - "        ~
                        & days$(i%)
            init(" ") pl_unts$()
            mat pl_unts = zer
        return

L50895: REM Planning Production Planned           PL_UNTA%(), PL_UNTA$()
            mat pl_unta% = zer
            for i% = 1% to 7%
                convert pl_unta$(i%) to pl_unta%(i%), data goto L50935

                convert pl_unta%(i%) to pl_unta$(i%), pic(#####)
            next i%
        return
L50935:     errormsg$ = "(Error) - Invalid Planned Value for - "         ~
                        & days$(i%)
            init(" ") pl_unta$()
            mat pl_unta% = zer
        return

L50965: REM Planning Production Produced          PL_UNTP%(), PL_UNTP$()
            mat pl_untp% = zer
            for i% = 1% to 7%
                convert pl_untp$(i%) to pl_untp%(i%), data goto L51005

                convert pl_untp%(i%) to pl_untp$(i%), pic(#####)
            next i%
        return
L51005:     errormsg$ = "(Error) - Invalid Produced Window Value for -"  ~
                        & days$(i%)
            init(" ") pl_untp$()
            mat pl_untp% = zer
        return

        check_dept
            check% = 0%
        REM CHECK% = 1%
            init(" ") sav_key$, pl_key$
            str(pl_key$,1%,3%) = pl_dept$
            str(pl_key$,4%,2%) = pl_proc$
            str(pl_key$,6%,2%) = pl_shft$
            sav_key$ = pl_key$
            read #2,key > pl_key$, using L51080, pl_key$, eod goto L51100
L51080:        FMT POS(11), CH(12)
            if str(sav_key$,1%,7%) <> str(pl_key$,1%,7%) then goto L51100
            check% = 1%
        return
L51100:     init(" ") sav_key$, pl_key$
        return

        build_sort
            init(" ") pl_sort$(), pl_sort1$()      /* (EWD001)       */
 
            sort% = 0%
            pl_sorts_d$ = "Sort: "
            xx% = len(pl_sorts$)
            for i% = 1% to xx%
               if str(pl_sorts$,i%,1%) = " " then goto L51180
               if i% > 12% then goto L51110
                  pl_sort$(i%) = str(pl_sorts$,i%,1%)
                  code$ = pl_sort$(i%)
                  goto L51120
L51110:        pl_sort1$(i%-12%) = str(pl_sorts$,i%,1%)
               code$ = pl_sort1$(i%-12%)

L51120:        tab%  = 8% : gosub check_code
               if code% <> 0% then goto L51170
                  sort% = 1%
                  return
L51170:        p% = pos(desc$ = "-")
               if p% = 0% then p% = 6%
               pl_sorts_d$ = pl_sorts_d$ & str(desc$,1%,p%-1%) & ","
            next i%
        return
L51180: str(pl_sorts$,i%) = "          "
        return

        REM *************************************************************~
            *               R e p o r t   E d i t s                     *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52080,         /* Planning Week         */ ~
                              L52135,         /* Production Week       */ ~
                              L52340,         /* Beg Department Code   */ ~
                              L52445,         /* End Department Code   */ ~
                              L52575,         /* Beginning Process Code*/ ~
                              L52680,         /* Ending Process Code   */ ~
                              L52810,         /* Beginning Shift Code  */ ~
                              L52915          /* Ending Shift Code     */
        return

L52080: REM Planning Production Year              RP_YR$
            if rp_year$ <> " " then goto L52095
               rp_yr$ = str(datecc$,1%,4%)                                      /* (Y2K, LDJ) */
L52095:     convert rp_year$ to rp_yr%, data goto L52115                        /* (Y2K, LDJ) */

REM       if rp_yr% < 1994% or rp_yr% > 2019% then goto L52115    /* CR2371 */
            rp_yr$ = bin(rp_yr%,2)                                              /* (Y2K, LDJ) */
        return
L52115:     errormsg$ = "(Error) - Invalid Production Year??"
            init(" ") rp_year$
        return

L52135: REM Planning Production Week               RP_WK$, RP_DATE$
            ent_yr$ = rp_yr$
            ent_wk$ = rp_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

           if pl_e% <> 0% then goto L52315
           rp_wk$   = ent_wk$
           rp_dte$  = ent_dte$
           rp_date$ = ent_date$

           convert ent_wk$ to x%, data goto L52245     /* ENTRY WEEK    */
L52245:
           convert cur_wk$ to y%, data goto L52315     /* CURRENT WEEK  */
           convert val(cur_yr$,2) to cur_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(ent_yr$,2) to ent_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(prv_yr$,2) to prv_year$,pic(0000)                        /* (Y2K, LDJ) */
           current$ = "Current: +XX"
           if x% <> y% then goto L52280
              str(current$,11%,2%) = "00"
              goto L52310
L52280:    if x% > y% then goto L52300
              str(current$,10%,1%) = "-"                /* Previous Wk */
              convert (y%-x%) to str(current$,11%,2%), pic(00)
              goto L52310
L52300:    convert (x%-y%) to str(current$,11%,2%), pic(00) /*Future Wk*/

L52310: return
L52315:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           init(" ") rp_wk$, rp_dte$, rp_date$
        return


L52340: REM Beg Department Code                   BG_DEPT$,BG_DEPT_D$
            if bg_dept$ <> " " then goto L52360
               bg_dept$ = "ALL"

L52360:     if str(bg_dept$,1%,1%) <> "A" then goto L52385
               bg_dept$ = "ALL"
               bg_dept_d$ = "(ALL) Planning Departments"
               return

L52385:     convert bg_dept$ to x%, data goto L52425

            convert x% to bg_dept$, pic(000)
            code$ = bg_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L52425
            bg_dept_d$ = desc$
        return
L52425:     errormsg$ = "(Error) - Invalid Beginning Department Code ??"
            init(" ") bg_dept$, bg_dept_d$
        return

L52445: REM End Department Code                   ED_DEPT$,ED_DEPT_D$
            if ed_dept$ <> " " then goto L52465
               ed_dept$ = bg_dept$

L52465:     if str(bg_dept$,1%,1%) <> "A" then goto L52490
               ed_dept$ = "ALL"
               ed_dept_d$ = "(ALL) Planning Departments"
               return

L52490:     convert ed_dept$ to x%, data goto L52555

            convert x% to ed_dept$, pic(000)
            code$ = ed_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L52555
            ed_dept_d$ = desc$
            convert bg_dept$ to y%, data goto L52530
L52530:
            if x% < y% then goto L52555
            if copy% = 0% then return
               if x% <> y% then goto L52555
        return
L52555:     errormsg$ = "(Error) - Invalid Ending Department Code ??"
            init(" ") ed_dept$, ed_dept_d$
        return

L52575: REM Beginning Process Code                BG_PROC$, BG_PROC_D$
            if bg_proc$ <> " " then goto L52595
               bg_proc$ = "AA"

L52595:     if str(bg_proc$,1%,1%) <> "A" then goto L52620
               bg_proc$ = "AA"
               bg_proc_d$ = "(ALL) Valid Process Codes"
               return

L52620:     convert bg_proc$ to x%, data goto L52660

            convert x% to bg_proc$, pic(00)
            code$ = bg_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L52660
            bg_proc_d$ = desc$
        return
L52660:     errormsg$ = "(Error) - Invalid Beginning Processing Code ??"
            init(" ") bg_proc$, bg_proc_d$
        return

L52680: REM Ending Process Code                   ED_PROC$, ED_PROC_D$
            if ed_proc$ <> " " then goto L52700
               ed_proc$ = bg_proc$

L52700:     if str(bg_proc$,1%,1%) <> "A" then goto L52725
               ed_proc$ = "AA"
               ed_proc_d$ = "(ALL) Valid Process Codes"
               return

L52725:     convert ed_proc$ to x%, data goto L52790

            convert x% to ed_proc$, pic(00)
            code$ = ed_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L52790
            ed_proc_d$ = desc$
            convert bg_proc$ to y%, data goto L52765
L52765:
            if x% < y% then goto L52790
            if copy% = 0% then return
               if x% <> y% then goto L52790
        return
L52790:     errormsg$ = "(Error) - Invalid Ending Processing Code ??"
            init(" ") ed_proc$, ed_proc_d$
        return

L52810: REM Beginning Shift Code                  BG_SHFT$, BG_SHFT_D$
            if bg_shft$ <> " " then goto L52830
               bg_shft$ = "AA"

L52830:     if str(bg_shft$,1%,1%) <> "A" then goto L52855
               bg_shft$ = "AA"
               bg_shft_d$ = "(ALL) Planning Shift Codes"
               return

L52855:     convert bg_shft$ to x%, data goto L52895

            convert x% to bg_shft$, pic(00)
            code$ = bg_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L52895
            bg_shft_d$ = desc$
        return
L52895:     errormsg$ = "(Error) - Invalid Beginning Shift Code ??"
            init(" ") bg_shft$, bg_shft_d$
        return

L52915: REM Ending Shift Code                     ED_SHFT$, ED_SHFT_D$
            if ed_shft$ <> " " then goto L52935
               ed_shft$ = bg_shft$

L52935:     if str(bg_shft$,1%,1%) <> "A" then goto L52960
               ed_shft$ = "AA"
               ed_shft_d$ = "(ALL) Planning Shift Codes"
               return

L52960:     convert ed_shft$ to x%, data goto L53015

            convert x% to ed_shft$, pic(00)
            code$ = ed_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L53015
            ed_shft_d$ = desc$
            convert bg_shft$ to y%, data goto L53000
L53000:
            if x% < y% then goto L53015
        return
L53015:     errormsg$ = "(Error) - Invalid Ending Shift Code ??"
            init(" ") ed_shft$, ed_shft_d$
        return

        REM *************************************************************~
            *                C o p y   E d i t s                        *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54090,         /* Copy From Prod Year   */ ~
                              L54145,         /* Copy To Prod Year     */ ~
                              L54200,         /* Copy From Prod Week   */ ~
                              L54430,         /* Copy To Production Wk */ ~
                              L52340,         /* Beg Department Code   */ ~
                              L52445,         /* End Department Code   */ ~
                              L52575,         /* Beginning Process Code*/ ~
                              L52680,         /* Ending Process Code   */ ~
                              L52810,         /* Beginning Shift Code  */ ~
                              L52915          /* Ending Shift Code     */
        return

L54090: REM Copy From Production Year             BG_YR$
            if bg_year$ <> " " then goto L54105                                 /* (Y2K, LDJ) */
               bg_year$ = str(datecc$,1%,4%)                                    /* (Y2K, LDJ) */
L54105:     convert bg_year$ to bg_yr%, data goto L54125                        /* (Y2K, LDJ) */

REM      if bg_yr% < 1994% or bg_yr% > 2019% then goto L54125    /* CR2371 */
            bg_yr$ = bin(bg_yr%,2)                                              /* (Y2K, LDJ) */
        return
L54125:     errormsg$ = "(Error) - Copy From Production Year??"
            init(" ") bg_year$                                                  /* (Y2K, LDJ) */
        return

L54145: REM Copy To Production Year               ED_YR$
            if ed_year$ <> " " then goto L54160                                 /* (Y2K, LDJ) */
               ed_year$ = str(datecc$,1%,4%)                                    /* (Y2K, LDJ) */
L54160:     convert ed_year$ to ed_yr%, data goto L54180                        /* (Y2K, LDJ) */

            if ed_yr% < bg_yr% then goto L54180
            ed_yr$ = bin(ed_yr%,2)                                              /* (Y2K, LDJ) */
        return
L54180:     errormsg$ = "(Error) - Copy To Production Year??"
            init(" ") ed_year$                                                  /* (Y2K, LDJ) */
        return

L54200: REM Copy From Production Week              BG_WK$, BG_WK_D$
            ent_yr$ = bg_yr$
            ent_wk$ = bg_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

           if pl_e% <> 0% then goto L54410
           bg_wk$   = ent_wk$
           bg_wk_d$ = ent_date$

           convert ent_wk$ to x%, data goto L54410     /* Entry Week    */

           convert cur_wk$ to y%, data goto L54410     /* Current Week  */
           convert val(cur_yr$,2) to cur_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(ent_yr$,2) to ent_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(prv_yr$,2) to prv_year$,pic(0000)                        /* (Y2K, LDJ) */
           current$ = "Current: +XX"
           if ent_yr$ = cur_yr$ then goto L54360
              if ent_yr$ > cur_yr$ then goto L54345
                 wk% = ((52% - x%) + y%) * (-1%)
                 goto L54395
L54345:    wk% = ((52% - y%) + x%)                      /* Future Year */
           goto L54395                                   /* Week        */

L54360:    if x% <> y% then goto L54375
              wk% = 0%                                  /* Current Wk  */
              goto L54395
L54375:    if x% > y% then goto L54390
              wk% = (y% - x%) * (-1%)                   /* Previous Wk */
              goto L54395
L54390:    wk% = (x% - y%)
L54395:    convert wk% to str(current$,10%,3%), pic(-00) /*Future Wk*/

        return
L54410:    errormsg$ = "(Error) - Invalid Copy From Production Week??"
           init(" ") bg_wk$, bg_wk_d$, current$
        return

L54430: REM Copy To Production Week                ED_WK$, ED_WK_D$
            ent_yr$ = ed_yr$
            ent_wk$ = ed_wk$
            init(" ") ent_dy$, ent_dte$, ent_date$
           call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr$,    /* Entry Production Year      */~
                             ent_wk$,    /* Entry Prod Week            */~
                             ent_dy$,    /* Entry Production Day       */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr$,    /* Previous Year              */~
                             #3,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

           if pl_e% <> 0% then goto L54585
           ed_wk$   = ent_wk$
           ed_wk_d$ = ent_date$

           convert ent_wk$ to x%, data goto L54585     /* Entry Week    */

           convert cur_wk$ to y%, data goto L54585     /* Current Week  */
           convert val(cur_yr$,2) to cur_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(ent_yr$,2) to ent_year$,pic(0000)                        /* (Y2K, LDJ) */
           convert val(prv_yr$,2) to prv_year$,pic(0000)                        /* (Y2K, LDJ) */
           if bg_yr$ = ed_yr$ then goto L54570
              if ed_wk$ > bg_wk$ then goto L54585
                 return

L54570:    if ed_wk$ < bg_wk$ then goto L54585
           copy% = 1%
        return
L54585:    errormsg$ = "(Error) - Invalid Copy To Production Week??"
           init(" ") ed_wk$, ed_wk_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
        %!                                                               ~
        ~                                                                !
L55100: %!######## @ ########                         ###################~
        ~#####################                                Page: #### !
L55120: %!Production Year: #### Production Week: ##   Date: ########     ~
        ~                                                                !
L55140: %!Beg Deptartment Code: ### ##############################       ~
        ~     Ending Department Code: ### ############################## !
L55160: %!Beg Process Code    :  ## ##############################       ~
        ~     Ending Process Code   :  ## ############################## !
L55180: %!Beg Shift Code      :  ## ##############################       ~
        ~     Ending Shift Code     :  ## ############################## !

L55210: %!Department: ###  ##############################                ~
        ~              ##################################################!
L55230: %!Process   :  ##  ##############################                ~
        ~              ##################################################!
L55250: %!Shift     :  ##  ##############################                ~
        ~              ##################################################!
L55270: %!Prod. Sort:   ############### #################################~
        ~#######       ##################################################!
L55290: %!Start Day :   #  #########    #################################~
        ~#######       ##################################################!
L55310: %!                                                               ~
        ~              ##################################################!
L55330: %!                                                               ~
        ~              ##################################################!
L55350: %!                                                               ~
        ~              ##################################################!
L55370: %!                                                               ~
        ~              ##################################################!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55100, date$, runtime$, title$, pageno%
           print using L55120, rp_year$, rp_wk$, rp_date$                       /* (Y2K, LDJ) */
           print using L55140, bg_dept$, bg_dept_d$, ed_dept$, ed_dept_d$
           print using L55160, bg_proc$, bg_proc_d$, ed_proc$, ed_proc_d$
           print using L55180, bg_shft$, bg_shft_d$, ed_shft$, ed_shft_d$
           lcntr% = 6%
        return

        print_detail
           if sav_dept$ <> pl_dept$ then gosub print_header
              sav_dept$ = pl_dept$               /* New Page each Dept */
           if lcntr% > 55% then gosub print_header
              for i% = 1% to 7%
                str(scr$(2% + i%),19%,10%) = pl_unts$(i%)
                str(scr$(2% + i%),33%,5%)  = pl_unta$(i%)
                str(scr$(2% + i%),43%,5%)  = pl_untp$(i%)
              next i%
              print using L55060
              print using L55210, pl_dept$, pl_dept_d$,   scr$(1%)
              print using L55230, pl_proc$, pl_proc_d$,   scr$(2%)
              print using L55250, pl_shft$, pl_shft_d$,   scr$(3%)
              print using L55270, pl_sorts$, str(pl_sorts_d$,1%,40%),     ~
                                 scr$(4%)
              print using L55290, pl_day$, pl_day_d$,                     ~
                                 str(pl_sorts_d$,41%,39%), scr$(5%)
              print using L55310,                         scr$(6%)
              print using L55330,                         scr$(7%)
              print using L55350,                         scr$(8%)
              print using L55370,                         scr$(9%)
              lcntr% = lcntr% + 10
        return

        generate_report
           init(" ") pl_key$, sav_dept$
           if str(bg_dept$,1%,3%) = "ALL" then goto gen_nxt
              str(pl_key$,1%,3%)  = bg_dept$
           if bg_proc$ = "AA" then goto gen_nxt
              str(pl_key$,4%,2%)  = bg_proc$
           if bg_shft$ = "AA" then goto gen_nxt
              str(pl_key$,6%,2%)  = bg_shft$
              str(pl_key$,8%,2%)  = rp_yr$
        REM   STR(PL_KEY$,10%,2%) = RP_WK$      /* LEAVE OUT FOR SCAN */
        gen_nxt
           read #1,key 1% > pl_key$, using L60560, pl_key$,               ~
                                                  eod goto gen_done
L60560:       FMT POS(7), CH(11)
           if bg_dept$ = "ALL" then goto L60590
              if str(pl_key$,1%,3%) > ed_dept$ then goto gen_done
L60590:    if bg_proc$ = "AA" then goto L60610
              if str(pl_key$,4%,2%) > ed_proc$ then goto gen_nxt
L60610:    if bg_shft$ = "AA" then goto L60630
              if str(pl_key$,6%,2%) > ed_shft$ then goto gen_nxt
L60630:    if str(pl_key$,8%,2%) <> rp_yr$ then goto gen_nxt
           if str(pl_key$,10%,2%) <> rp_wk$ then goto gen_nxt
              gosub dataload_rpt
              gosub print_detail
            goto gen_nxt
        gen_done
            if lcntr% = 99% then return                     /* NO DATA */
            print using L55040
        return

        check_code
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = tab$(tab%)
           str(readkey$,10%,15%)  = code$
           read #3,key = readkey$, using L60800, desc$,                   ~
                                                           eod goto L60830
L60800:        FMT POS(25), CH(30)
           code% = 1%
        return
L60830:    errormsg$ = "(Error) - Invalid Code Value Entered ??"
        return

        copy_data
            call "SHOSTAT" ("Copy Data From Week ("&bg_wk$&") to Week ("&~
                             ed_wk$&")")
            cnt% = 0%
            cnt$ = "Records Copied = [XXXXX]"
            pl_key$ = " "
            str(pl_key$,1%,2%) = bg_yr$
            str(pl_key$,3%,2%) = bg_wk$
            if bg_shft$ = "AA" then goto copy_next
               str(pl_key$,5%,2%) = bg_shft$
            if bg_dept$ = "ALL" then goto copy_next
               str(pl_key$,7%,3%) = bg_dept$
                                           /* Skip Beg Process Key    */
                                           /* Because of Greater Than */
        copy_next
            read #1,hold,key > pl_key$, using L61030, pl_key$,            ~
                                                       eod goto copy_done
L61030:         FMT CH(11)
            if str(pl_key$,1%,2%) <> bg_yr$ then goto copy_done
            if str(pl_key$,3%,2%) <> bg_wk$ then goto copy_done
            if bg_shft$ = "AA" then goto L61080
               if str(pl_key$,5%,2%) <> bg_shft$ then goto copy_next
L61080:     if bg_dept$ = "ALL" then goto L61100
               if str(pl_key$,7%,3%) <> bg_dept$ then goto copy_next
L61100:     if bg_proc$ = "AA" then goto L61130
               if str(pl_key$,10%,2%) <> bg_proc$ then goto copy_next

L61130:     get #1, using L61140, pl_rec$
L61140:           FMT CH(128)
            cnt% = cnt% + 1%
            if mod(cnt%,5%) <> 0 then goto L61200
               convert cnt% to str(cnt$,19%,5%), pic(#####)
               print at(03,28); hex(84);cnt$;

L61200:     str(pl_rec$,1%,2%)  = ed_yr$
            str(pl_rec$,3%,2%)  = ed_wk$
            str(pl_rec$,14%,2%) = ed_yr$
            str(pl_rec$,16%,2%) = ed_wk$
            if ed_shft$ = "AA" then goto L61270
               str(pl_rec$,5%,2%)  = ed_shft$
               str(pl_rec$,12%,2%) = ed_shft$
L61270:     if ed_dept$ = "ALL" then goto L61290
               str(pl_rec$,7%,3%) = ed_dept$
L61290:     if ed_proc$ = "AA" then goto L61310
               str(pl_rec$,10%,2%) = ed_proc$
L61310:     pl_key1$ = str(pl_rec$,1%,11%)
            if pl_key$ = pl_key1$ then goto copy_next
               mat pl_unta% = zer
               mat pl_untp% = zer
               put str(pl_rec$,87%,28%), using L61370, pl_unta%(),        ~
                                                      pl_untp%()
L61370:          FMT 7*BI(2), 7*BI(2)

            read #1,hold,key = pl_key1$, eod goto L61410
               delete #1
L61410:     put #1, using L61140, pl_rec$
            write #1, eod goto L61480
            goto copy_next
        copy_done

        return clear all
            goto copy_input
L61480:     call "SHOSTAT" ("(Error) - Copying --->  " & pl_key1$ )
            stop : close ws
            goto copy_next

        calc_capacity                    /* Calculate Capacity Hours   */
            init(" ") emp_key$           /* For a Department (Adjusted)*/
            call "SHOSTAT" ("Checking Department Capacity")

            cnt% = 0%
            str(emp_key$,1%,3%) = str(pl_dept$,1%,3%)
        calc_next
            read #5,key 1% > emp_key$, using L61610, emp_key$,emp_status$,~
                                          emp_shft$, eod goto calc_done
L61610:        FMT CH(11), POS(152), CH(1), POS(841), CH(2)
            if str(emp_key$,1%,3%) <> str(pl_dept$,1%,3%) then           ~
                                                          goto calc_done
            if emp_status$ <> "A" then goto calc_next
            if emp_shft$ <> pl_shft$ then goto calc_next
               cnt% = cnt% + 1%
               goto calc_next
        calc_done
            calc = (cnt% * 8.0) - ((cnt% * 8.0) * .05)
            for i% = 0% to 6%
              k% = pl_day% + i%
              if k% = 6% or k% = 7% then goto L61770
                 k% = i% + 1%
                 pl_unts(k%) = calc
                 convert pl_unts(k%) to pl_unts$(k%), pic(######.##-)

L61770:     next i%
        return

        total_all
            call "SHOSTAT" ("Getting Totals for all Shifts")

            init(" ") pl_key$
            mat t1  = zer
            mat t2% = zer
            mat t3% = zer
            str(pl_key$,1%,2%)  = pl_yr$
            str(pl_key$,3%,2%)  = pl_wk$
            str(pl_key$,5%,2%)  = "01"
            str(pl_key$,7%,3%)  = pl_dept$
            str(pl_key$,10%,2%) = pl_proc$
            read #1,key = pl_key$, using L62000, pl_key$, pl_unts(),      ~
                                      pl_unta%(), pl_untp%(),            ~
                                      eod goto total_done
            goto L62001
        total_nxt
            read #1,key > pl_key$, using L62000, pl_key$, pl_unts(),      ~
                                      pl_unta%(), pl_untp%(),            ~
                                      eod goto total_done
L62000:        FMT CH(11), POS(31), 7*PD(14,4), 7*BI(2), 7*BI(2)
L62001: call "SHOSTAT" ("KEY ----> "& pl_key$)
           stop " " : close ws

            if str(pl_key$,1%,2%) <> pl_yr$ then goto total_done
            if str(pl_key$,3%,2%) <> pl_wk$ then goto total_done
            if str(pl_key$,7%,3%) <> pl_dept$ then goto total_nxt
            if str(pl_key$,10%,2%) <> pl_proc$ then goto total_nxt
               for i% = 1% to 7%
                   t1(i%)  = t1(i%)  + pl_unts(i%)
                   t2%(i%) = t2%(i%) + pl_unta%(i%)
                   t3%(i%) = t3%(i%) + pl_untp%(i%)
               next i%
               goto total_nxt
        total_done
            for i% = 1% to 7%
                convert t1(i%)  to pl_unts$(i%), pic(######.##-)
                convert t2%(i%) to pl_unta$(i%), pic(#####)
                convert t3%(i%) to pl_untp$(i%), pic(#####)
            next i%
            total% = 1%
            pl_shft$ = "AA"
            pl_shft_d$ = "(Totals) for All Shifts"
        return

                                                           /* (EWD002)  */
        open_error
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                           /* (EWD002)  */ 
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
