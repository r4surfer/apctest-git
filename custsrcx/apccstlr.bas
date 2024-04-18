        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    CCC    SSSS  TTTTT  L      RRRR    *~
            *  A   A  P   P  C   C  C   C  S        T    L      R   R   *~
            *  AAAAA  PPPP   C      C        S      T    L      RRRR    *~
            *  A   A  P      C   C  C   C      S    T    L      R R     *~
            *  A   A  P       CCC    CCC   SSSS     T    LLLLL  R  R    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCCSTLR - Calculate the average Hourly Rate Per          *~
            *            Department Based on Specified Data.            *~
            *                                                           *~
            *    NOTE  - Uses Subroutine (APCCSTWG)                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/15/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/10/95 ! Mods for Year End Roll Over. Added BG_YR$! RHH *~
            *          !   ED_YR$ to Selection Screen?            !     *~
            * 11/11/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 05/06/98 ! Y2K Modifications                        !     *~
            *02/13/2019! CR-1894  Increase EMP DEPT size to 3     ! DES *~
            *************************************************************

        dim                                                              ~
            blank_date$10,               /* Packed blank date          */~
            dept$3, dept_d$30,           /* Department Code            */~
            bg_wk$2,bg_dte$6,bg_wk_dte$10, /* Beg Production Week      */~
            ed_wk$2,ed_dte$6,ed_wk_dte$10, /* End Production Week      */~
            bg_yr$4, ed_yr$4, cur_yr$4,  /* Begin/End Production Year  */~
            bg_yr_bi$2, ed_yr_bi$2,      /* Packed                     */~
            cur_yr_bi$2,                 /*                            */~
            days$(7%)9,                  /* DAYS OF THE WEEK           */~
            days$9,                      /* DAY OF THE WEEK            */~
            date$10,                     /* SCREEN DATE                */~
            jdate$7,                     /* Julian Date                */~
            mt_date$6, mt_yr$4, prv_yr$4,/* For Current Calc           */~
            mt_yr_bi$2, prv_yr_bi$2,     /* Packed                     */~
            sv_wk$2, sv_day$1,           /* For Current Calc           */~
            e_key$3,                     /* Primary Key (APCCSTLR) File*/~
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
            wdate1$10, wdate2$10,        /* temp date variables        */~
            userid$3                     /* Current User Id            */

        dim l_dept$3,                    /* Department Code            */~
            l_emp$8, t_emp$8, x_emp$8,   /*                            */~
            l_emp$(1000%)8,              /* Total Active Employees     */~
          l_rhrs$10,t_rhrs$10,x_rhrs$10, /* Total Reg Hours for Period */~
          l_ohrs$10,t_ohrs$10,x_ohrs$10, /* Total Overtime Hrs Period  */~
          l_rpay$10,t_rpay$10,x_rpay$10, /* Total Reg Pay for Period   */~
          l_opay$10,t_opay$10,x_opay$10, /* Total Overtime Pay Period  */~
            l_ahr$10,hours$10,wages$10,  /* Calc Avg Hourly Rate Period*/~
            l_bdte$6, l_bdte1$10,        /* Date of Beg Prod Week      */~
            l_edte$6, l_edte1$10,        /* Date of End Prod Week      */~
            l_date$6, l_date1$10,        /* Date of Last Calc          */~
            l_fil$1                      /* Filler Area                */

        dim sc_dept$3,                   /* Department Code            */~
            sc_yr$4, sc_yr_bi$2,         /* Production Year            */~
            sc_wk$2,                     /* Production Week            */~
            eno$(500%)5,                 /* Number of Employee's       */~
            ef(7%,2%),                   /* Eff Hours by Day Regular   */~
            ef_ov(7%,2%),                /* Eff Hours by Day Overtime  */~
            wg1(7%,2%),                  /* Wages Each Day Regular     */~
            wg2(7%,2%)                   /* Wages each Day Overtime    */

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
            apc$   = "(New) Calculate Average Hourly Rate-Dept"
            pname$ = "APCCSTLR - Rev: R6.04"

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
            * #1  ! GENCODES ! Master Code Table File                   *~
            * #2  ! APCEMPMT ! Employee Master Time Clock File          *~
            * #4  ! APCEMPLY ! Employee Master File                     *~
            * #5  ! APCCSTLR ! Department Average Hourly Rate           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #2,   "APCEMPMT",                                     ~
                        varc,     indexed,  recsize =  128,  /*ADP*/     ~
                        keypos =    1, keylen =   13,                    ~
                        alt key  1, keypos =    4, keylen =  10, dup

            select #4,   "APCEMPLY",                                     ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    7, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos =   12, keylen =  26, dup

            select #5,   "APCCSTLR",                                     ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =   3

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),100%, rslt$(5%))

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
            str(date$,3%,2%) = "01" : str(date$,5%,2%) = "01"

            days$(1%) = "MONDAY   " : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY" : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   " : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "
            x% = 94%                /* Calculation for Production Week */
            date$   = date
            wdate1$ = date
            call "DATFMTC" (wdate1$, wdate%, wdate2$)
       REM  cur_yr$ = str(date$,1%,2%)                 /* Current Year */
            cur_yr$ = str(wdate2$,1,4)
            convert cur_yr$ to wdate%
            cur_yr_bi$ = bin(wdate%, 2)
            
            convert cur_yr$ to x%, data goto L09190
L09190:
            x% = x% - 1%
            convert x% to prv_yr$, pic(0000)
            prv_yr$ = bin(x%, 2)

            call "DATFMTC" (date$)
            str(date$,1%,2%) = "01" : str(date$,4%,2%) = "01"
            call "DATUFMTC" (date$)
            call "DATE" addr("GD", date$, days$, x% )
            for i% = 1% to 7%
                if days$ = days$(i%) then goto L09280
            next i%
L09280:     factor% = (i% - 2%)              /* (I%) = 1ST DAY OF YEAR */
                                             /* Production Week Factor */

            date$ = date
            call "DATFMTC" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 14% then gosub print_report
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
                  if keyhit%  = 16% then gosub process_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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
            gosub select_printer
            gosub generate_report
            gosub close_printer
        return clear all
        goto inputmode

        select_printer
            call "SHOSTAT" ("Printing Report")
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            call "SETPRNT" ("APCR", " ", 0%, 0%)
            pageno% = 0%
            lcnt% = 99%
            title$ = " Calculated Average Labor Rate by Dept. "
        return

        close_printer
            print using L55040
            call "SETPRNT" ("APCR", " ", 0%, 1%)
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
         "Enter a Valid Department Code or (A) for All Departments?    ",~
         "Enter a Valid Beginning Production Week?                     ",~
         "Enter a Valid Ending Production Week?                        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, dept$, dept_d$, bg_wk$,    ~
                      bg_dte$, bg_wk_dte$, ed_wk$, ed_dte$, ed_wk_dte$,  ~
                      readkey$, sv_wk$, sv_day$, e_key$, l_dept$, l_emp$,~
                      l_emp$(), l_rhrs$, l_ohrs$, l_rpay$, l_opay$,      ~
                      l_ahr$, l_bdte$, l_edte$, l_date$, l_fil$,         ~
                      bg_yr$, ed_yr$
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
            get #5, using L35040, l_dept$, l_emp%, l_rhrs, l_ohrs, l_rpay,~
                                 l_opay, t_emp%, t_rhrs, t_ohrs, t_rpay, ~
                                 t_opay, l_ahr, l_bdte$, l_edte$,        ~
                                 l_date$, l_fil$
            x_emp% = l_emp% + t_emp%
            x_rhrs = l_rhrs + t_rhrs
            x_ohrs = l_ohrs + t_ohrs
            x_rpay = l_rpay + t_rpay
            x_opay = l_opay + t_opay
            hours = round(x_rhrs + x_ohrs, 2)
            wages = round(x_rpay + x_opay, 2)
            convert x_emp% to x_emp$,  pic(########)
            convert x_rhrs to x_rhrs$, pic(######.##-)
            convert x_ohrs to x_ohrs$, pic(######.##-)
            convert x_rpay to x_rpay$, pic(######.##-)
            convert x_opay to x_opay$, pic(######.##-)
            convert hours  to hours$ , pic(######.##-)
            convert wages  to wages$ , pic(######.##-)

            convert l_emp% to l_emp$,  pic(########)
            convert l_rhrs to l_rhrs$, pic(######.##-)
            convert l_ohrs to l_ohrs$, pic(######.##-)
            convert l_rpay to l_rpay$, pic(######.##-)
            convert l_opay to l_opay$, pic(######.##-)
            convert t_emp% to t_emp$,  pic(########)
            convert t_rhrs to t_rhrs$, pic(######.##-)
            convert t_ohrs to t_ohrs$, pic(######.##-)
            convert t_rpay to t_rpay$, pic(######.##-)
            convert t_opay to t_opay$, pic(######.##-)
            convert l_ahr  to l_ahr$,  pic(######.##-)
            l_bdte1$ = l_bdte$
            l_edte1$ = l_edte$
            l_date1$ = l_date$
            call "DATFMTC" (l_bdte1$)
            call "DATFMTC" (l_edte1$)
            call "DATFMTC" (l_date1$)

            init(" ") readkey$, dept_d$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = str(l_dept$,1%,3%)
            read #1,key = readkey$, using L30470, dept_d$, eod goto L50270
L30470:        FMT POS(25), CH(30)
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            l_dept$ = sc_dept$
            l_ahr = 0.0
            x = l_rpay + l_opay + t_rpay + t_opay             /* Wages */
            y = l_rhrs + l_ohrs + t_rhrs + t_ohrs             /* Hours */
            if y < .01 then goto L31120
               l_ahr = round( (x / y), 2)
L31120:     l_date$ = date
            read #5,hold,key = l_dept$, eod goto L31150
               delete #5
L31150:     put #5, using L35040, l_dept$, l_emp%, l_rhrs, l_ohrs, l_rpay,~
                                 l_opay, t_emp%, t_rhrs, t_ohrs, t_rpay, ~
                                 t_opay, l_ahr, l_bdte$, l_edte$,        ~
                                 l_date$, l_fil$
            write #5, eod goto L31210
        return
L31210:    stop "(Error) - Updating (APCCSTLR) File Department-" &l_dept$
        return


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (APCCSTLR)          */
L35040: FMT CH(03),                      /* Department Code            */~
            BI(2),                       /* Number of Active Emp's     */~
            PD(14,4),                    /* Total Reg Hours            */~
            PD(14,4),                    /* Total Overtime Hours       */~
            PD(14,4),                    /* Total Reg Pay Dollars      */~
            PD(14,4),                    /* Total Overtime Pay Dollars */~
            BI(2),                       /* No. of Temp Emp's          */~
            PD(14,4),                    /* Total Reg Hours Temp's     */~
            PD(14,4),                    /* Total Overtime Hours Temp's*/~
            PD(14,4),                    /* Total Reg Pay Dollars Temps*/~
            PD(14,4),                    /* Total Overtime Pay Dol Temp*/~
            PD(14,4),                    /* Current Avg Hourly Rate    */~
            CH(06),                      /* Beg Prod. Week             */~
            CH(06),                      /* End Prod. Week             */~
            CH(06),                      /* DATE OF LAST CALC          */~
            CH(5)                        /* Filler Area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,         /* Department Code, ALL */~
                                L40180,         /* Beg Production Week  */~
                                L40180          /* End Production Week  */
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(39),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Department Code or (A)ll :",                 ~
               at (03,30), fac(lfac$( 1)), dept$                , ch(03),~
               at (03,40), fac(hex(84)), dept_d$                , ch(30),~
                                                                         ~
               at (04,02), "Begin Production Week/Yr :",                 ~
               at (04,30), fac(lfac$( 2)), bg_wk$               , ch(02),~
               at (04,40), fac(lfac$( 2)), bg_yr$               , ch(04),~
               at (04,50), fac(hex(84)), bg_wk_dte$             , ch(10),~
                                                                         ~
               at (05,02), "End Production Week/Yr   :",                 ~
               at (05,30), fac(lfac$( 3)), ed_wk$               , ch(02),~
               at (05,40), fac(lfac$( 3)), ed_yr$               , ch(04),~
               at (05,50), fac(hex(84)), ed_wk_dte$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40510
                  call "PRNTSCRN"
                  goto L40210

L40510:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40710     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)print report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40670
                str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40670:     if fieldnr% > 1% then L40690
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40690:     return

L40710: if fieldnr% > 0% then L40810  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Calculate   "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L40810:
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
            on fieldnr% gosub L50130,         /* Department Code or All*/ ~
                              L50560,         /* Beg Production Week   */ ~
                              L50950          /* End Production Week   */
            return

L50130: REM Department Code                       DEPT$
            if dept$ <> " " then goto L50180
L50150:        dept$   = "A "
               dept_d$ = "(A)ll Departments"
              return
L50180:     if str(dept$,1%,1%) = "A" then goto L50150
            convert dept$ to x%, data goto L50290

            convert x% to dept$, pic(000)

            init(" ") readkey$, dept_d$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = dept$
            read #1,key = readkey$, using L50270, dept_d$, eod goto L50290
L50270:        FMT POS(25), CH(30)
        return
L50290:     errormsg$ = "(Error) - Invalid Department Code?"
            dept$, dept_d$ = " "
        return

        current_week
            scale% = 0%
            mt_date$ = date                          /* Current Date   */
            wdate1$  = date
            call "DATFMTC" (wdate1$, wdate%, wdate2$)
        REM mt_yr$   = str(mt_date$,1%,2%)           /* Current Year   */
            mt_yr$   = str(wdate2$ ,1%,4%)
            convert mt_yr$ to wdate%
            mt_yr_bi$ = bin(wdate%, 2)
            call "DATE" addr("GJ", mt_date$, jdate$, x% ) /* CALC PROD */
            get jdate$ using L50380, jdate%
L50380:         FMT PD(9,0)
            convert jdate% to jdate$, pic(0000000)
            j1% = 0% : convert str(jdate$,5%,3%) to j1%, data goto L50390
L50390:                                      /* Factor for Prod Year   */
            wk% = (j1% + factor%)/ 7%        /* Calculate Prod Week    */
            if wk% <> 0% then goto L50470
               wk% = 52%
               scale% = 1%
           REM str(mt_date$,1%,2%) = prv_yr$
               wdate1$ = mt_date$
               call "DATFMTC" (wdate1$)
               str(wdate1$,7,4) = prv_yr$
               call "DATUFMTC" (wdate1$)
               mt_date$  = wdate1$
               mt_yr$    = prv_yr$
               mt_yr_bi$ = prv_yr_bi$

L50470:     call "DATE" addr("GD", mt_date$, days$, x% )
            for i% = 1% to 7%
                if days$ = days$(i%) then goto L50510
            next i%
L50510:     x% = 0%
            convert wk% to sv_wk$, pic(##)   /* Current Production Week*/
            convert i% to sv_day$, pic(#)    /* Current Production Day */
        return

L50560: REM Beginning Production Week           BG_WK$
           if bg_wk$ <> " " then goto L50590
              bg_wk$  = "01"

L50590: REM Calc Production Date
           gosub current_week
           convert bg_wk$ to x%, data goto L50870

           convert x% to bg_wk$, pic(##)

           convert sv_wk$ to y%, data goto L50870

           if x% < 1% or x% > 52% then goto L50870

           if x% > y% then goto L50710                   /* FUTURE WEEK */
              goto L50770
L50710:    if cur_yr$ = bg_yr$ then goto L50900
              bg_yr$    = prv_yr$
              bg_yr_bi$ = prv_yr_bi$
              scale% = 1%
          REM str(mt_date$,1%,2%) = bg_yr$
               wdate1$ = mt_date$
               call "DATFMTC" (wdate1$)
               str(wdate1$,7,4) = bg_yr$
               call "DATUFMTC" (wdate1$)
               mt_date$  = wdate1$
               mt_yr$    = bg_yr$
               mt_yr_bi$ = bg_yr_bi$


L50770:    convert x% to bg_wk$, pic(##)
           y% = (x% * 7%) - factor% + scale%

      REM  str(jdate$,1%,2%) = str(mt_date$,1%,2%)
           wdate1$ = mt_date$
           call "DATFMTC" (wdate1$)
           str(jdate$,1%,4%) = str(wdate1$, 7,4)     /* YYYY   */
           convert y% to str(jdate$,5%,3%), pic(000) /* YYYYDDD*/
           convert jdate$ to wdate%
           put jdate$ using L50775, wdate%
L50775:      FMT PD(9,0)
           call "DATE" addr("JG", jdate$, bg_dte$, x%)
           bg_wk_dte$ = bg_dte$
           call "DATFMTC" (bg_wk_dte$)
           bg_yr$ = str(bg_wk_dte$,7%,4%)
           convert bg_yr$ to wdate%
           bg_yr_bi$ = bin(wdate%, 2)
           l_bdte$ = bg_dte$
        return

L50870:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           init(" ") bg_wk_dte$, bg_wk$, bg_dte$, l_bdte$, bg_yr$
        return
L50900:    errormsg$ = "(Error) - Future Production Week,Less than Equal ~
        ~to ("& sv_wk$ & ")"
           init(" ") bg_wk_dte$, bg_wk$, bg_dte$, l_bdte$, bg_yr$
        return

L50950: REM Ending Production Week                  ED_WK$
           if ed_wk$ <> " " then goto L50980
              ed_wk$ = sv_wk$
L50980:    gosub current_week
           convert ed_wk$ to x%, data goto L51270

           convert x% to ed_wk$, pic(##)

           convert sv_wk$ to y%, data goto L51270

           if x% < 1% or x% > 52% then goto L51270

           if ed_wk$ < bg_wk$ then goto L51270
           if x% > y% then goto L51100                   /* FUTURE WEEK */
              goto L51160
L51100:    if cur_yr$ = ed_yr$ then goto L51290
               ed_yr$    = prv_yr$
               ed_yr_bi$ = prv_yr_bi$
               scale% = 1%
         REM   str(mt_date$,1%,2%) = ed_yr$
               wdate1$ = mt_date$
               call "DATFMTC" (wdate1$)
               str(wdate1$,7,4) = ed_yr$
               call "DATUFMTC" (wdate1$)
               mt_date$  = wdate1$
               mt_yr$    = ed_yr$
               mt_yr_bi$ = ed_yr_bi$

L51160:    convert x% to ed_wk$, pic(##)
           y% = (x% * 7%) - factor% + scale%

       REM str(jdate$,1%,2%) = str(mt_date$,1%,2%)
           wdate1$ = mt_date$
           call "DATFMTC" (wdate1$)
           str(jdate$,1%,4%) = str(wdate1$, 7,4)     /* YYYY   */
           convert y% to str(jdate$,5%,3%), pic(000) /* YYYYDDD*/
           convert jdate$ to wdate%
           put jdate$ using L50775, wdate%
           call "DATE" addr("JG", jdate$, ed_dte$, x%)
           ed_wk_dte$ = ed_dte$
           call "DATFMTC" (ed_wk_dte$)
           ed_yr$ = str(ed_wk_dte$,7%,4%)
           convert ed_yr$ to wdate%
           ed_yr_bi$ = bin(wdate%, 2)
           l_edte$ = ed_dte$
           if bg_yr$ <> ed_yr$ then goto L51320
        return

L51270:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
             goto L51340
L51290:    errormsg$ = "(Error) - Future Production Week,Less than Equal ~
        ~to ("& sv_wk$ & ")"
             goto L51340
L51320:    errormsg$ = "(Error) - Cannot Span Years. Begin/End Year Must ~
        ~be Equal?"
L51340:      init(" ") ed_wk_dte$, ed_wk$, ed_dte$, l_edte$, ed_yr$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55080: %! ########## @ ########                      ###################~
        ~#####################                                Page: #### !

L55110: %!Dept: ### ##############################    APC Emp's  :     ##~
        ~###  Temp Emp's  :     #####                   Emp's:     ##### !
L55130: %!                                            APC Reg Hrs: ######~
        ~#### Temp Reg Hrs: ########## Tot: ##########                   !
L55150: %!                                            APC Ovr Hrs: ######~
        ~#### Temp Ovr Hrs: ########## Tot: ##########  Hours: ##########!
L55170: %!                                            APC Reg Pay: ######~
        ~#### Temp Reg Pay: ########## Tot: ##########                   !
L55190: %!                                            APC Ovr Pay: ######~
        ~#### Temp Ovr Pay: ########## Tot: ##########  Wages: ##########!

L55220: %!Beg Date: ########## End Date: ##########   Calc Date  :   ####~
        ~######                                        Hourly: ##########!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcnt% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           lcnt% = 2%
        return

        print_detail
           if lcnt% > 57% then gosub print_header
           print using L55060
           print using L55110, l_dept$, dept_d$, l_emp$, t_emp$, x_emp$
           print using L55130, l_rhrs$, t_rhrs$, x_rhrs$
           print using L55150, l_ohrs$, t_ohrs$, x_ohrs$, hours$
           print using L55170, l_rpay$, t_rpay$, x_rpay$
           print using L55190, l_opay$, t_opay$, x_opay$, wages$
           print using L55220, l_bdte1$, l_edte1$, l_date1$, l_ahr$
           lcnt% = lcnt% + 7%
        return

        generate_report
            e_key$ = all(hex(00))
        generate_next
            read #5,key > e_key$, using L60290, e_key$,                   ~
                                                   eod goto generate_done
L60290:        FMT CH(3)
            gosub dataload
            gosub print_detail
            goto generate_next

        generate_done

        return

        process_data
        REM sc_yr$ = str(l_bdte$,1%,2%)
            wdate1$ = l_bdte$
            call "DATFMTC" (wdate1$)
            sc_yr$ = str(wdate1$, 7, 4)
            convert sc_yr$ to wdate%
            sc_yr_bi$ = bin(wdate%, 2)
            if str(dept$,1%,1%) = "A" then goto L60430
               sc_dept$ = dept$
               goto L60520
L60430:     init(" ") readkey$
            str(readkey$,1%,9%)   = "EMP DEPT "
        process_nxt_dept
            if str(dept$,1%,1%) <> "A" then goto process_done
            read #1,key > readkey$, using L60490, readkey$,               ~
                                                    eod goto process_done
L60490:        FMT CH(24)
            if str(readkey$,1%,8%) <> "EMP DEPT" then goto process_done
            sc_dept$ = str(readkey$,10%,3%)
L60520:     convert bg_wk$ to bg_wk%
            l_rhrs, l_ohrs, l_rpay, l_opay, l_ahr = 0.0
            t_rhrs, t_ohrs, t_rpay, t_opay = 0.0
            x_emp% = 1%
            init(" ") l_emp$()
            goto L60600
        process_nxt_wk
            bg_wk% = bg_wk% + 1%
L60600:     if bg_wk% > 52% then goto process_nxt_wk_done
            convert bg_wk% to sc_wk$, pic(##)
            if sc_wk$ > ed_wk$ then goto process_nxt_wk_done
            call "SHOSTAT" ("Calc for Dept = " & sc_dept$ & "  Week = "  ~
                                & sc_wk$ )
               gosub calculate
               goto process_nxt_wk
        process_nxt_wk_done
            l_emp%, t_emp% = 0%
            for i% = 1% to x_emp%
REM             if str(l_emp$(i%),1%,1%) <> "A" then l_emp% = l_emp% + 1%~
                                                else t_emp% = t_emp% + 1%
              if str(l_emp$(i%),4%,1%) <> "A" then l_emp% = l_emp% + 1%~
                                                else t_emp% = t_emp% + 1%
            next i%
               gosub dataput                      /* UPDATE DEPARTMENT */
               goto process_nxt_dept
        process_done

        return clear all
        goto inputmode

        calculate
             err% = 0%
        call "APCCSTWG" (sc_dept$,       /* Department Code           */ ~
                         sc_yr_bi$,      /* Production Year           */ ~
                         sc_wk$,         /* Production Week           */ ~
                         eno%,           /* Number of Employees       */ ~
                         eno$(),         /* Employee's In Department  */ ~
                         ef(),           /* Efficiency Hours by Day   */ ~
                         ef_ov(),        /* Efficiency Overtime Hours */ ~
                         wg1(),          /* Wages Each Day Regular    */ ~
                         wg2(),          /* Wages Each Day Overtime   */ ~
                         #4,             /* APCEMPLY                  */ ~
                         #2,             /* APCEMPMT                  */ ~
                         err% )          /* Error Code 0% = Ok        */

           for i% = 1% to 7%             /* 1ST Add Data for Week     */
                                         /* Permenant Employee's      */
              l_rhrs = round( l_rhrs + ef(i%,1%), 2)
              l_ohrs = round( l_ohrs + ef_ov(i%,1%), 2)
              l_rpay = round( l_rpay + wg1(i%,1%), 2)
              l_opay = round( l_opay + wg2(i%,1%), 2)
                                         /* Temporary Employee's      */
              t_rhrs = round( t_rhrs + ef(i%,2%), 2)
              t_ohrs = round( t_ohrs + ef_ov(i%,2%), 2)
              t_rpay = round( t_rpay + wg1(i%,2%), 2)
              t_opay = round( t_opay + wg2(i%,2%), 2)
           next i%
           if x_emp% = 1% then l_emp$(1%) = eno$(1%)
           for k% = 1% to eno%           /* 2ND CALC TOTAL EMPLOYEES  */
              for l% = 1% to x_emp%
                  if l_emp$(l%) = eno$(k%) then goto L61140
              next l%
              x_emp% = x_emp% + 1%
              l_emp$(x_emp%) = eno$(k%)
L61140:    next k%
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
