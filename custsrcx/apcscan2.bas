        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    SSSS   CCCC   AAA   N    N   222   *~
            *  A   A  P   P  C   C  S      C      A   A  NN   N  2   2  *~
            *  AAAAA  PPPP   C       SSS   C      AAAAA  N N  N     2   *~
            *  A   A  P      C   C      S  C      A   A  N  N N    2    *~
            *  A   A  P       CCC   SSSS    CCCC  A   A  N   NN  222222 *~
            *                                                           *~
            *  ( APCSCNSB ) -> USES THE SAME TIME ENTRY SCREEN AS       *~
            *                  ( DISPLAY_EMPLOYEE )                     *~
            *-----------------------------------------------------------*~
            * APCSCAN2 - Program Allows User Clock In or Clock Out      *~
            *            Using Barcoding or Data Entry                  *~
            *                                                           *~
            *     Note - (1) The Lunch Deduction (45) Minutes is Only   *~
            *                Done When the Employee Scans 'Out' after   *~
            *                Six Hours. Not Checked for Entries Made    *~
            *                from the 'Emp Time' Screen.                *~
            *            (2) The Window Locking and Rounding Check is   *~
            *                Only Done when the Employee Scans his Card.*~
            *                Not Checked for Entries Made from the      *~
            *                'Emp Time' Screen.                         *~
            *            (3) All Entries Made from the 'Emp Time'       *~
            *                Screen, will Cause a Variance for that     *~
            *                Employee for that Production Week and Day. *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/01/93 ! Original                                 ! RHH *~
            * 07/17/93 ! MOD - NO VARIANCE FOR TRANSFERS - ( 7171)! RHH *~
            * 12/22/93 ! Mod - Put in Time Stamp Audit for File   ! RHH *~
            *          !       TIME_STAMP Routine. Display is in  !     *~
            *          !       (APCSCNSB) Routine Used by APCEMPVR!     *~
            * 03/22/94 ! Mod - Put in Lunch deduction Code E_LUN$ ! RHH *~
            *          !       and Deductions LD$()               !     *~
            * 03/31/94 ! Mod - New routine to Check Week and Day  ! RHH *~
            *          !       CHECK_WK_DAY                       !     *~
            * 01/15/96 ! Mod - To Current production Week Problem.! RHH *~
            *          !       Remove the Opening and Closing of  !     *~
            *          !       files after each employee.         !     *~
            * 02/27/96 ! Mod - Correct Prob of going back a Prod. ! RHH *~
            *          !       Day. Only go back for Shift '3'    !     *~
            * 10/30/97 ! Mod - Add User Id to (APCEMPMT) to show  ! RHH *~
            *          !       who made last Modification.        !     *~
            *          !       Added Lunch Deductions for upto an !     *~
            *          !       hour.                              !     *~
            * 03/27/98 ! Y2K modifications                        ! ERN *~
            * 07/20/01 ! (EWD001) - Mod to fix sick days          ! CMG *~
            * 08/13/03 ! (EWD002) - Mod to use emp actual clock in! CMG *~
            *          !            time in emp master.           ! CMG *~
            * 01/21/05 ! (AWD003) - Mod to fix vacation days      ! CMG *~
            * 02/28/07 ! (AWD004) - Mod to not give hrs for Z var ! DES *~
            *02/12/2019! CR-1894 Increase Emp Dept size           ! DES *~
            *05/12/2020! CR2490  Increase Employee Number size    ! RDB *~
            *************************************************************

        dim                                                              ~
            dt_usr$3,                    /* Time Stamp - User Id       */~
            dt_dte$6,                    /* Time Stamp - Date          */~
            dt_ts$8,                     /* Time Stamp - Time          */~
            dt_fil1$3,                   /* Time Stamp - Filler Area   */~
            txt$(20%)40,                 /* TEST DATA                  */~
            wandchar$1,                  /*                            */~
            sc_code$1,                   /* Same as DT_CODE$           */~
            sc_dept$3, sc_dept_d$30,     /* Parent Dept or Override    */~
            sav_dept$3,                  /* USE TO UPDATE (APCEMPMT)   */~
            e_payg$3,                    /* EMPLOYEE PAY GRADE         */~
            rhh$5, pfkeys$32, rhh1$5,    /* Manual Time Entry          */~
            rhh_desc$30,                 /* LONG FORM OF TIME          */~
            days$(7%)9,                  /* Days of the Week           */~
            days$9,                      /* Day of the Week            */~
            e_dept$3,                    /* Employee Parient Department*/~
            dt_dept$3, mt_dept$3,        /* Employee Department Code   */~
            dt_yr$4, mt_yr$4,            /* Production Year (YY)       */~
            dt_wk$2, mt_wk$2,            /* Employee Production Week   */~
            dt_day$1,mt_day$1,           /* Employee Production Day    */~
            dt_emp$5,mt_emp$5,           /* Employee Number            */~
            mt_date$6, dt_fil$1,         /* Employee Clock Date        */~
            dt_time$5, c_t$(30%)5,       /* Employee Clock Time        */~
            status$(15%)6, x_time$5,     /* Status Code                */~
            dp$(15%)2,                   /* DEPARTMENT CODE            */~
            c_th%(15%), c_tm%(15%),      /* CLOCK HOURS AND MINUTES    */~
            prod_dte$8,                  /* PRODUCTION WEEK DATE       */~
            dt_code$1, dt_desc$30,       /* Employee Clock Code        */~
            mt_var_in$1,                 /* Clock in Variance Code     */~
            mt_var_out$1,                /* Clock Out Variance Code    */~
            mt_var_day$1,                /* Variance Code For Day      */~
            mt1$1, mt2$1, mt3$1,         /* IN, OUT, DAY VARIANCE      */~
            mt_proc$1,                   /* Master Process Flag        */~
            mt_fil$3,                    /* Filler Area                */~
            sav_key$10, sav_key2$10,     /* Copy of Detail Key Prim    */~
            mt_key$13, mt_key1$10,       /* Master Key PRIMARY / ALT   */~
            dt_key$18, dt_rec$40,        /* Detail Primary Key         */~
            sav_key1$15, sav_hr$2,       /* Detail Primary Key         */~
            xx$(7%)31,                   /* Display Screen Text        */~
            in$(7%)31,                   /* Clock in Message           */~
            ot$(7%)31,                   /* Clock Out Message          */~
            er$(7%)31,                   /* Clock Error Message        */~
            e_key$5,                     /* Employee Master Key        */~
            e_lname$15,                  /* Employee Last Name         */~
            e_fname$10,                  /* Employee First Name        */~
            e_fnamex$5,                  /* Employee First Name        */~
            e_init$1,                    /* Employee Init              */~
            e_status$1, etime$8,         /* Employee Status A,I,T      */~
            e_itime$1, e_itime_d$4,      /* Clock In Time Code         */~
            e_otime$1, e_otime_d$4,      /* Clock Out Time Code        */~
            e_display$1,                 /* Display Data On Screen     */~
            e_lun$1, ld$(15%)3, lun$3,   /* Lunch Deduction Code/Ded   */~
            e_lunch$1, e_lock$1,         /* LUNCH AND LOCK FLAGS       */~
            d_hours$2, d_min$2,          /* Daily Hours and Minutes    */~
            w_hours$2, w_min$2,          /* Weekly Hours and Minutes   */~
            name$30, emp_shft$2,         /* Display Name               */~
/* CR2490 */~
            barcode$8,                   /* Employee Number Bar Code   */~
            vac_days$4,                  /* Employee Vacation Days AVAI*/~
            sick_days$4,                 /* Employee Sick Days AVAIL   */~
            points$6,                    /* Employee Current Points    */~
            errormsg$40,                 /* Error Message Display      */~
            readkey$24,                  /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            userid$3                     /* Current User Id            */~

        dim                              /* Subroutine - Variables     */~
            cur_yr$4, cur_yr_bi$2,       /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4,  prv_yr$4,         /* Julian Year and Day YYDDD  */~
            ent_yr_bi$2, prv_yr_bi$2,    /*                            */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8        /* Prod Week Date Form/Unform */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

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
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos = 7,    keylen =  5,                      ~
                        alt key 1, keypos =  1, keylen = 11, dup,        ~
                            key 2, keypos = 12, keylen = 26, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  24

            select #3,  "APCEMPMT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  13,                     ~
                        alt key 1, keypos =  4, keylen = 10, dup

            select #4,  "APCEMPDT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen =  17,                     ~
                        alt key 1, keypos = 114, keylen = 6, dup

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 100%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 100%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
                                                   /* LUNCH DEDUCTIONS */
            ld$(1%) = "045"  : ld$(5%) = "025" : ld$(9%)  = "045"
            ld$(2%) = "010"  : ld$(6%) = "030" : ld$(10%) = "050"
            ld$(3%) = "015"  : ld$(7%) = "035" : ld$(11%) = "055"
            ld$(4%) = "020"  : ld$(8%) = "040" : ld$(12%) = "060"

            call "EXTRACT" addr("ID", userid$)
            u3% = 0%
            days$(1%) = "MONDAY   " : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY" : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   " : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "
                                    /* Calculation for Production Week */
            date$ = date

            in$(1%) = "    IIIIIIII   NNN     NN      "
            in$(2%) = "    IIIIIIII   NNNN    NN      "
            in$(3%) = "       II      NN NN   NN      "
            in$(4%) = "       II      NN  NN  NN      "
            in$(5%) = "       II      NN   NN NN      "
            in$(6%) = "    IIIIIIII   NN    NNNN      "
            in$(7%) = "    IIIIIIII   NN     NNN      "

            ot$(1%) = " OOOOOO    UU     UU  TTTTTTTT "
            ot$(2%) = "OOOOOOOO   UU     UU  TTTTTTTT "
            ot$(3%) = "OO    OO   UU     UU     TT    "
            ot$(4%) = "OO    OO   UU     UU     TT    "
            ot$(5%) = "OO    OO   UU     UU     TT    "
            ot$(6%) = "OOOOOOOO   UU     UU     TT    "
            ot$(7%) = " OOOOOO      UUUUU       TT    "

            er$(1%) = "EEEEEEEE   RRRRRRR    RRRRRRR  "
            er$(2%) = "EEEEEEEE   RRRRRRRR   RRRRRRRR "
            er$(3%) = "EE         RR    RRR  RR    RRR"
            er$(4%) = "EEEEEE     RRRRRRRR   RRRRRRRR "
            er$(5%) = "EE         RRRRRRR    RRRRRRR  "
            er$(6%) = "EEEEEEEE   RR    RR   RR    RR "
            er$(7%) = "EEEEEEEE   RR     RR  RR     RR"

            toggle% = 0%
            gosub toggle
        REM ************************************************************
        REM *              M a i n   P r o g r a m                     *
        REM ************************************************************

        main
            gosub mainmenu
            if keyhit% = 16% and userid$ <> "EMP" then exit_program
            errormsg$ = " "
            gosub lookup_employee                 /* VALIDATE EMPLOYEE */
            if emp% <> 0% then goto L02520

            calc% = 0%
            dt_code$ = sc_code$            /* SET CODE - 0, 1, 2      */
            gosub calc_production          /* PRODUCTION DAY DATA      */
            if emp% <> 0% then goto L02520

            dt_code$ = sc_code$            /* SET FROM TOGGLE          */
            gosub write_detail             /* WRITE TIME STAMP (IN/OUT)*/
            if emp% <> 0% then goto L02520

            gosub total_day                /* TOTAL HR/MIN DAY DISPLAY */
            gosub check_lunch
            gosub update_master            /* UPDATE PROD. DAY TOTALS  */
            if emp% <> 0% then goto L02520

            gosub total_week               /* DISPLAY TOTALS (WEEK)    */
            gosub display_screen           /* EMPLOYEE DISPLAY SCREEN  */
            goto main
L02520: gosub display_error
        goto main

        REM ************************************************************
        REM *           ( E n d )  M a i n   P r o g r a m             *
        REM ************************************************************

         mainmenu
        REM *************************************************************~
            * Display Main Clock Screen. ALLow For Scan 'Only' for Id   *~
            *  ( SC9 )                                                  *~
            *************************************************************

            gosub set_pf1
            accept                                                       ~
               at (04,02), "Today:",                                     ~
               at (04,09), fac(hex(84)), date$                  , ch(10),~
               at (01,25), fac(hex(84)), xx$(1%)                , ch(31),~
               at (02,25), fac(hex(84)), xx$(2%)                , ch(31),~
               at (03,25), fac(hex(84)), xx$(3%)                , ch(31),~
               at (04,25), fac(hex(84)), xx$(4%)                , ch(31),~
               at (05,25), fac(hex(84)), xx$(5%)                , ch(31),~
               at (06,25), fac(hex(84)), xx$(6%)                , ch(31),~
               at (07,25), fac(hex(84)), xx$(7%)                , ch(31),~
               at (08,02), fac(hex(a4)), line2$                 , ch(79),~
               at (09,25), fac(hex(94)), errormsg$              , ch(40),~
                                                                         ~
               at (10,02), "Employee Bar Code Number",                   ~
               at (10,25), fac(hex(81)), barcode$               , ch(08),~
               at (10,36), fac(hex(99)), wandchar$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if barcode$ <> "99999" then goto L02940
                  keyhit% = 3%
                  barcode$ = " "

L02940:        if keyhit% <> 3% then goto L02980
                  gosub toggle
                  goto mainmenu

L02980:        if keyhit% <> 15% then L03010
                  call "PRNTSCRN"

L03010:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
            init(" ") barcode$, wandchar$, date$, etime$, rhh$, rhh1$,   ~
                      rhh_desc$, dt_time$, txt$(), x_time$
            process% = 0% : dt_time% = 0% : lunch_flag% = 0%
            date$ = date : call "DATFMTC" (date$)
            call "TIME" (etime$) : edit% = 0%
*       RHH
            inpmessage$ = "Scan Badge or Use Toggle Card to Toggle Betwee~
        ~n 'IN' or 'OUT' Screen?"
            pf$(1%) = "                                        " &       ~
                      "                                       "
            pf$(2%) = "                           Use Card to T" &       ~
                      "oggle In/Out           (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(ffff03ffffffffffffffffffff0e0f1000)
            if userid$ <> "EMP" then goto L03240
               str(pf$(2%),60%) = " " : str(pfkeys$,15%,1%) = hex(ff)
               str(pf$(3%),60%) = " "
L03240: return

        toggle
            errormsg$ = " "
            if toggle% = 1% then goto L03320
               mat xx$ = in$ : toggle% = 1% : sc_code$ = "0"
               dt_fil$ = sc_code$
               return
L03320:     mat xx$ = ot$ : toggle% = 0% : sc_code$ = "1"
            dt_fil$ = sc_code$
        return

        REM *************************************************************~
            * Display This Screen If Barcode is Scanned And No          *~
            * Errors Occur.                                             *~
            *************************************************************

        display_screen
            print at(13,20);hex(84);"Employee Name   : ";name$
            print at(15,20);hex(84);"Daily Pay Hours : ";d_hours$;"   "; ~
                                    "Minutes : ";d_min$
            print at(16,20);hex(84);"Weekly Pay Hours: ";w_hours$;"   "; ~
                                    "Minutes : ";w_min$
            if e_display$ = "N" then goto L03510
               print at(17,20);hex(84);"Vac Days Avail : ";vac_days$
               print at(18,20);hex(84);"Sick Days Avail: ";sick_days$
               print at(19,20);hex(84);"Points Given   : ";points$
L03510:     if toggle% = 1% then call "PAUSE" addr(300%) /* IN Screen  */
            if toggle% = 0% then call "PAUSE" addr(500%) /* Out Screen */
        return

        display_error
            print at(13,20);hex(84);er$(1%)
            print at(14,20);hex(84);er$(2%)
            print at(15,20);hex(84);er$(3%)
            print at(16,20);hex(84);er$(4%)
            print at(17,20);hex(84);er$(5%)
            print at(18,20);hex(84);er$(6%)
            print at(19,20);hex(84);er$(7%)
            call "PAUSE" addr(200%)

            init(" ") mt_var_in$, mt_var_out$, mt_var_day$, dt_emp$,     ~
                      name$, dt_wk$, prod_dte$, dt_day$, days$,          ~
                      sc_dept$, sc_dept_d$
            i% = 1% : dt_code$ = "N" : rhh$ = "HHMM" : edit% = 0%
            dt_desc$ = "(N) - Display Data Only"
            rhh_desc$ = "Time in Mil. Format or Hrs/Min"
            on emp% goto L03730, L03750, L03770, L03790, L03810, L03830, L03850, L03870, ~
                         L03890, L03910, L03930, L03950, L03970, L03990, L04010, L04030
L03730:        errormsg$ = "Employee Code Not On-File ??            "
                 return                                  /* EMP% =  1% */
L03750:        errormsg$ = "Employee Code is not an Active Code ??  "
                 return                                  /* EMP% =  2% */
L03770:        errormsg$ = "Daily Clock In Code is Undefined ??     "
                 return                                  /* EMP% =  3% */
L03790:        errormsg$ = "Daily Clock Out Code is Undefined ??    "
                 return                                  /* EMP% =  4% */
L03810:        errormsg$ = "Un-Able to Update Detail ??             "
                 return                                  /* EMP% =  5% */
L03830:        errormsg$ = "Already Clocked in for Specified Time?? "
                 return                                  /* EMP% =  6% */
L03850:        errormsg$ = "(Error)"
                 return                                  /* EMP% =  7% */
L03870:        errormsg$ = "(Error)-Salaried Employee. N/A ??"
                 return                                  /* EMP% =  8% */
L03890:        errormsg$ = "(Error)"
                 return                                  /* EMP% =  9% */
L03910:        errormsg$ = "(Error)"
                 return                                  /* EMP% = 10% */
L03930:        errormsg$ = "(Error)"
                 return                                  /* EMP% = 11% */
L03950:        errormsg$ = "(Error)-C l o c k   L o c k e d??"
                 return                                  /* EMP% = 12% */
L03970:        errormsg$ = "(Error)"
                 return                                  /* EMP% = 13% */
L03990:        errormsg$ = "(Error)"
                 return                                  /* EMP% = 14% */
L04010:        errormsg$ = "(Error)"
                 return                                  /* EMP% = 15% */
L04030:        errormsg$ = "(Error)-Calc Production Data   ??"
                 return                                  /* EMP% = 16% */
               errormsg$ = "(Error)-Invalid Julian Day     ??"
                                                         /* EMP% = 17% */
        return

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end

        REM *************************************************************~
            * Scan Employee Data                                        *~
            *                                                           *~
            *************************************************************

        lookup_employee
            d_hours$ = "00" : d_min$ = "00"
            w_hours$ = "00" : w_min$ = "00"

            lun$ = "045" : emp% = 0%
/* CR2490 */
REM            e_key$ = barcode$
            convert barcode$ to e_no%, data goto L04570
L12345:             FMT BI(4)                                 /* CR2490 */
            put str(e_key$,1%,4%), using L12345, e_no%
            str(e_key$,5%,1%) = " "
            
            read #1,key = e_key$, using  L04310, e_dept$, e_payg$,         ~
                                  e_lname$, e_fname$, e_init$, e_status$,~
                                  e_vac_days%, e_vac_used%,              ~
                                  e_sick_days%, e_sick_used%, points,    ~
                                  emp_shft$, e_lun$, e_itime$, e_otime$, ~
                                  e_display$, e_lunch$, e_lock$,         ~
                                  e_fnamex$,                             ~
                                  eod goto L04570
L04310:       FMT CH(3), CH(3), POS(12), CH(15), CH(10), CH(1), POS(152),~
                  CH(1), POS(218), BI(2), POS(226), BI(2),               ~
                  POS(234), BI(2), POS(242), BI(2), POS(308),            ~
                  PD(14,4), POS(841), CH(02), CH(01), POS(1013), 5*CH(01),~
		  POS(844), CH(5)
            mt_dept$ = e_dept$
            if e_payg$ = "180" or e_payg$ = "190" or e_payg$ = "200"     ~
                                                     then goto L04610
            x% = 0%
            x  = 0.0
            convert e_lun$ to x%, data goto L04400
L04400:
            lun$ = ld$(x% + 1%)        /* Set Employee Lunch Deduction */
            name$ = e_lname$ & ", " & e_fname$ & e_fnamex$ &            ~
                    " " & e_init$ & "."
            if sc_dept$ <> " " then goto L04450
               sc_dept$ = mt_dept$
L04450:     if e_status$ <> "A" then goto L04590
               gosub load_windows
               if emp% <> 0% then return

                                                   /*  (AWD003)  - Beg */
               e_vac_used = 0.0
               e_vac_used = (e_vac_used% / 10)
               x  = (e_vac_days% - e_vac_used)

REM            x% = e_vac_days% - e_vac_used%
REM               convert x% to vac_days$, pic(###)
                  convert x  to vac_days$, pic(##.#)
                                                   /*  (AWD003)  - Beg */

                                                 /* (EWD001) - BEGIN */
               e_sick_used = 0.0
               e_sick_used = (e_sick_used% / 10)

REM            x% = e_sick_days% - e_sick_used%
               x  = (e_sick_days% - e_sick_used)

REM               convert x% to sick_days$, pic(###)
                  convert x  to sick_days$, pic(##.#)
                                                 /* (EWD001) - END */
               convert points to points$, pic(##.##-)

        return
L04570:    emp% = 1%                           /* Employee Not On File */
        return
L04590:    emp% = 2%                           /* Employee Not Active  */
        return
L04610:    emp% = 8%                           /* Employee SALARIED    */
        return

        load_windows                             /* E_ITIME$, E_OTIME$ */
            t_in%, t_out%, emp% = 0%
            if e_itime$ = "0" then return
            init(" ") readkey$, e_itime_d$
              FMT POS(25), CH(4)
        REM - SET HOURLY WINDOW FOR CLOCKING IN
                                                    /*  (EWD002) BEG */
            str(readkey$,1%,9%)   = "EMP ITIME"
            str(readkey$,10%,15%) = e_itime$
            read #2,key = readkey$,using L04880 ,e_itime_d$, eod goto L04960
*       RHH
              convert str(e_itime_d$,1%,2%) to hr1%, data goto L04960

              convert str(e_itime_d$,3%,2%) to hm1%, data goto L04960



              t_in% = (60%*hr1%) + hm1%            /* CLOCK IN MINUTES */

                                                    /*  (EWD002) END */
            init(" ") readkey$, e_otime_d$
            str(readkey$,1%,9%)   = "EMP OTIME"
            str(readkey$,10%,15%) = e_otime$
            read #2,key = readkey$,using L04880 ,e_otime_d$, eod goto L04980
L04880:         FMT POS(25), CH(4)

              convert str(e_otime_d$,1%,2%) to hr1%, data goto L04980

              convert str(e_otime_d$,3%,2%) to hm1%, data goto L04980

              t_out% = (60%*hr1%) + hm1%          /* CLOCK OUT MINUTES */
        return
L04960:     emp% = 2%
        return
L04980:     emp% = 4%                    /* Clock Out Code Not Defined */
        return

        calc_production
            init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,    ~
                      ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,    ~
                      cur_yr_bi$, ent_yr_bi$, prv_yr_bi$
            init(" ") sav_key$, sav_key1$, sav_key2$, dt_yr$, dt_wk$,    ~
                      dt_day$

           if dt_yr$  <> " " then ent_yr$ = dt_yr$
           if dt_yr$  =  " " then goto L05000
                ent_yr$ = dt_yr$
                convert ent_yr$ to temp%, data goto L05670
                ent_yr_bi$ = bin(temp%, 2)
L05000:    if dt_wk$  <> " " then ent_wk$ = dt_wk$
           if dt_day$ <> " " then ent_dy$ = dt_day$


           call "AWDPLN0B" ( cur_yr_bi$, /* Current Production Year    */~
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


            if pl_e% <> 0% then goto L05670

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)            
            temp% = val(prv_yr_bi$,2)
            convert temp% to prv_yr$, pic (####)

*       RHH

            emp% = 0% : pass% = 0%
            mt_date$ = ent_dte$                      /* Current Date   */
            mt_yr$  = ent_yr$                        /* Master Year    */
                convert mt_yr$ to temp%, data goto L05670
                mt_yr_bi$ = bin(temp%, 2)
            dt_yr$  = ent_yr$                        /* Detail Year    */
                convert dt_yr$ to temp%, data goto L05670
                dt_yr_bi$ = bin(temp%, 2)
            sv_wk%  = 0% : sv_day% = 0%
            dt_wk$  = ent_wk$
            dt_day$ = ent_dy$
            prod_dte$ = ent_dte$
            convert dt_wk$ to sv_wk%, data goto L05350
L05350:
            convert dt_day$ to sv_day%, data goto L05370
L05370:
            wk% = sv_wk% : i% = sv_day%

            days$ = days$(i%)
            if calc% = 1% then return

            dt_time$ = time                         /* Check Edit Flag */
*       RHH
            if edit% = 0% then goto L05490
               dt_time$ = rhh$
               if str(dt_time$,1%,1%) = "A" then goto L05550
                                                 /* No Test Adjustment */
L05490:     convert str(dt_time$,1%,4%) to x%, data goto L05690

            gosub calc_prod_day
               sv_wk%  = wk%
               sv_day% = i%

L05550:     convert wk% to dt_wk$, pic(##)         /* Employee Prod Wk */
            convert i% to dt_day$, pic(#)          /* Employee Prod Day*/
            dt_dept$ = mt_dept$                    /* DEPARTMENT       */
            mt_wk$  = dt_wk$   : mt_day$ = dt_day$
/* CR2490 */
            convert barcode$ to e_no%, data goto L05670
            put str(dt_emp$,1%,4%), using L12345, e_no%
            str(dt_emp$,5%,1%) = " "
REM           dt_emp$ = barcode$ 
            mt_emp$ = dt_emp$ /* Employee Number  */
            dt_key$ = all(hex(00))
            str(dt_key$,1%,2%) = dt_yr_bi$ : str(sav_key2$,1%,2%) = mt_yr_bi$
            str(dt_key$,3%,2%) = dt_wk$    : str(sav_key2$,3%,5%) = mt_emp$
            str(dt_key$,5%,1%) = dt_day$   : str(sav_key2$,8%,2%) = mt_wk$
            str(dt_key$,6%,5%) = dt_emp$   : str(sav_key2$,10%,1%)= mt_day$
            sav_key$ = str(dt_key$,1%,10%)
        return
L05670:     emp% = 16%
        return
L05690:     emp% = 17%
        return

        write_detail
           mt1$ = "5" : mt2$ = "6" : mt3$ = "9"
           dt_time%, emp% = 0%
           mt_var_in$ = "5"  : mt_var_out$ = "6" : mt_var_day$ = "9"
           gosub find_in_hour           /* WHEN CLOCKING OUT FIND THE  */
                                        /* LAST (IN HOUR) TIME STAMP   */
           gosub convert_time
           if emp% <> 0% then return
           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           str(dt_key$,11%,3%) = dt_dept$
           str(dt_key$,14%,3%) = str(dt_time$,1%,3%)
           sav_key1$ = str(dt_key$,1%,16%)
           read #4,key > dt_key$, using L05860, dt_key$, eod goto L05900
L05860:       FMT CH(18)
              if str(dt_key$,14%,1%) = "A" then goto L05900
              if process% = 1% then goto L05900
                 if str(dt_key$,1%,16%) = sav_key1$ then goto L06070
L05900:    gosub check_lock
           if emp% <> 0% then return

           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           str(dt_key$,11%,3%) = dt_dept$
           str(dt_key$,14%,5%) = dt_time$
           read #4,hold,key = dt_key$, eod goto L05990
              delete #4
L05990:    gosub time_stamp
           put #4,using L06030, dt_yr_bi$,dt_wk$,dt_day$, dt_emp$, dt_dept$,  ~
                              dt_time$, dt_code$, sc_code$, dt_fil$,     ~
                              dt_usr$, dt_dte$, dt_ts$, dt_fil1$
L06030:       FMT 2*CH(2), CH(1), CH(5), CH(3), CH(5), CH(1), CH(1),     ~
                  CH(1), CH(3), CH(6), CH(8), CH(3)
           write #4, eod goto L06090
        return
L06070:    emp% = 6%
        return
L06090:    emp% = 5%
        return

        convert_time
           if str(dt_time$,1%,1%) = "A" then goto L06230
              convert str(dt_time$,1%,2%) to hr1%, data goto L06090

              convert str(dt_time$,3%,2%) to hm1%, data goto L06090

              dt_time% = (60*hr1%) + hm1%
              if dt_code$ <> "1" then goto L06230
                 if hr1% > sav_hr% then goto L06230
                    if hr1% > 9% then goto L06230
                    convert (hr1% + 24%) to str(dt_time$,1%,2%), pic(00)
L06230:    if str(dt_time$,5%,1%) <> "-" then str(dt_time$,5%,1%) = " "
        return

        total_day
           init(" ") c_t$(), status$(), sav_dept$, dp$()
           mat c_th% = zer : mat c_tm% = zer
           tm, tm1, tm2 = 0.0
           j1%, j2% = 0%
           dt_hours%, dt_min%, mt_hours%, mt_min% = 0%
           pt_hours%, pt_min% = 0%       /* Daily total of Parent Hrs */
           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           read #4,key > dt_key$, using  L06400, dt_key$, d_code$,         ~
                                                      eod goto total_done
           goto L06410
        total_next
           read #4,using  L06400, dt_key$, d_code$, eod goto total_done
L06400:       FMT CH(18), CH(1)
L06410:    if sav_key$ <> str(dt_key$,1%,10%) then goto total_done
              if sav_dept$ <> " " then goto L06440
                 sav_dept$ = str(dt_key$,11%,3%)
L06440:       if sav_dept$ = str(dt_key$,11%,3%) then goto L06490
                 gosub update_master
                 sav_dept$ = str(dt_key$,11%,3%)
                 mt_hours%, mt_min% = 0%

L06490:       sgn% = 1%
              j1% = j1% + 1%
              c_t$(j1%) = str(dt_key$,14%,5%)
              if mod(j1%,2%) = 0 then goto L06560
                 j2% = j2% + 1%
                 status$(j2%) = " Ok  "
                 dp$(j2%) = sav_dept$
L06560:       on pos("012" = d_code$) goto L06580, L06670, L06850
                                                          /* CLOCK IN  */
L06580:          convert str(c_t$(j1%),1%,2%) to hr1%, data goto L06590
L06590:
                 convert str(c_t$(j1%),3%,2%) to mn1%, data goto L06610
L06610:
                 if tm1 = 0.0 then goto L06640
                    status$(j2%) = "No Out"
L06640:          tm1 = 60 * hr1% + mn1%         /* CLOCK IN MINUTES    */
                 goto total_next
                                                          /* CLOCK OUT */
L06670:          convert str(c_t$(j1%),1%,2%) to hr2%, data goto L06680
L06680:
                 if hr2% >= 24% then hr2% = hr2% - 24%
                 convert hr2% to str(c_t$(j1%),1%,2%), pic(00)

                 convert str(c_t$(j1%),3%,2%) to mn2%, data goto L06730
L06730:
                 tm2 = 60 * hr2% + mn2%         /* CLOCK OUT MINUTES   */
                 if tm1 > 0.0 then goto L06800
                    status$(j2%) = "No In "
                    tm = 0.0
                    goto L06960

L06800:          if tm2 <= tm1 then tm2 = tm2 + 1440   /* ADD 24 HOURS */
                 tm  = tm2 - tm1                /* TOTAL CLOCK MINUTES */
                 tm1, tm2 = 0.0
              goto L06960
                                                /* ADJUSTED TIME       */
L06850:       convert str(c_t$(j1%),2%,1%) to hr1%, data goto L06860
L06860:
              convert str(c_t$(j1%),3%,2%) to mn1%, data goto L06880
L06880:
              tm = (60*hr1%) + mn1%
              if str(c_t$(j1%),5%,1%) = "-" then sgn% = -1%
              status$(j2%) = "Adj + "
              if sgn% = -1% then status$(j2%) = "Adj - "
              j1% = j1% + 1%
              c_t$(j1%) = "     "

L06960:       c_th%(j2%) = sgn% * ( int(tm/60.0) )
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

          convert dt_hours% to d_hours$, pic(##)    /* DAILY TOTALS    */
          convert dt_min% to d_min$, pic(##)        /* ALL DEPARTMENTS */
        return

        update_master
            mt_hours% = mt_hours% + int(mt_min%/60.0) /* BY DEPARTMENT */
            mt_min%   = mod(mt_min%,60.0)
            emp% = 0% : f1% = 0%
            mt_proc$ = "0" : mt_fil$ = "   " : code$ = "0"
            mt_key$ = all(hex(00))
            str(mt_key$,1%,3%)  = sav_dept$
            str(mt_key$,4%,10%) = sav_key2$
            read #3,hold,key = mt_key$, using L07270, mt1$, mt2$, mt3$,    ~
                                                           eod goto L07330
L07270:        FMT POS(24), 3*CH(1)
               put #3, using L07290, mt_hours%, mt_min%
L07290:           FMT POS(20), 2*BI(2)
               f1% = 1%
               goto write_master

L07330:     put #3, using  L07370, sav_dept$, mt_yr_bi$, mt_emp$, mt_wk$,   ~
                    mt_day$, mt_date$, mt_hours%, mt_min%,               ~
                    mt_var_in$, mt_var_out$, mt_var_day$, mt_proc$,      ~
                    userid$, mt_fil$
L07370:   FMT CH(03), CH(2), CH(5), CH(2), CH(1), CH(6), 2*BI(2), 4*CH(1),~
                   CH(3), CH(3)
        write_master
               if dt_code$ = "0" then gosub check_in
               if dt_code$ = "1" then gosub check_out
               if dt_code$ = "2" then gosub check_adj
               put #3, using L07440 , userid$
L07440:          FMT POS(28), CH(3)
               if f1% = 0% then write #3, eod goto L07480                  ~
                           else rewrite #3
        return
L07480:    emp% = 5%
        return

        check_lock
           if dt_code$ <> "0" then return        /* ONLY CHECK IN      */
           if process% = 1% then return          /* TIME STAMP SCREEN  */
           if t_in% = 0% then return             /* NO WINDOW SPECIFIED*/
        REM IF E_LOCK$ = "N" THEN GOTO 7910      /* NO LOCK            */
           goto L07620

              if dt_time% >= (t_in% - 7%) then goto L07620
                 emp% = 12%
        return
                                                 /* ROUND UP CLOCK IN  */
L07620:    if dt_time% >= (t_in% - 15%) and dt_time% <= t_in% then       ~
                                                                goto L07650
        return
L07650:    dt_time% = t_in%
           dt_time$ = e_itime_d$
        return

        check_in
           if t_in% = 0% then goto L07740
                                                     /* CHECK LATE IN  */
           if dt_time% < (t_in% + 3%) then goto L07740
              code$ = "2" : goto L07770
L07740:    put #3, using  L07750, "0"
L07750:       FMT POS(24), CH(1)
        return
L07770:    put #3, using L07780, code$, "9"
L07780:       FMT POS(24), CH(1), POS(26), CH(1)
        return

        check_out
           if t_out% = 0% then goto L07900
           if lunch_flag% = 0% then goto L07860
              dt_time% = dt_time_sav%
                                                     /* CHECK EARLY OUT*/
L07860:    if dt_time% >= (t_out% - 7%) then goto L07890
              code$ = "3" : goto L07960
                                                     /* CHECK LATE OUT */
L07890:    if mt1$ <> "0" then goto L07930
L07900:       put #3, using  L07910, "0", "0"
L07910:         FMT POS(25), CH(1), CH(1)
              return
L07930:       put #3, using  L07940, "0"
L07940:         FMT POS(25), CH(1)
        return
L07960:    put #3, using L07970, code$, "9"
L07970:       FMT POS(25), CH(1), CH(1)
        return

        check_adj                           /* ALWAYS SET HRS VARIANCE */
           put #3, using L08020, "8", "0"          /* SET HOURS VARIANCE */
L08020:       FMT POS(26), CH(1), CH(1)
        return

        check_lunch
           if dt_code$ <> "1" then return   /* ONLY WHEN PUNCHING OUT  */
           if e_lunch$ = "N" then return
           if dt_hours% < 6% then return
              lunch_flag% = 1%              /* SAVE THE ACTUAL CLOCK   */
              dt_time_sav% = dt_time%       /* OUT TIME                */

              dt_code$ = "2"
              dt_time$ = "A" & lun$ & "-"   /* Lunch Deduction - Mod   */
        REM   DT_TIME$ = "A045-"            /* DEDUCTION FOR LUNCH     */
              gosub write_detail
              dt_code$ = sc_code$
              gosub total_day
        return

        total_week
           mt_key1$ = all(hex(00))
           str(mt_key1$,1%,9%) = str(sav_key2$,1%,9%)
           x%,y% = 0%
           read #3,key 1% > mt_key1$, using  L08300, mt_key1$, mt_hours%,  ~
                            mt_min%, t_proc$, eod goto total_wk_done
           goto L08310
        total_wk_next
           read #3, using L08300, mt_key1$, mt_hours%, mt_min%, t_proc$,   ~
                                                   eod goto total_wk_done
L08300:          FMT POS(4), CH(10), POS(20), 2*BI(2), POS(27), CH(1)
L08310:    if str(mt_key1$,1%,10%) <> str(sav_key2$,1%,10%) then           ~
                                                       goto total_wk_done
REM          p% = pos("ACLNX" = t_proc$)
/*- AWD004 -- remove Z from hours ----------*/  
             p% = pos("ACLNXZSTP" = t_proc$)
             if p% <> 0% then goto total_wk_next       /* NO PAY HOURS */
                x% = x% + mt_hours%
                y% = y% + mt_min%
             goto total_wk_next
        total_wk_done
             x% = x% + int(y%/60.0)             /* CONVERT TO HOURS  */
             y% = mod(y%,60.0)                  /* LEFT OVER MINUTES */
           convert x% to w_hours$, pic(##)
           convert y% to w_min$, pic(##)
        return

        calc_prod_day
           if dt_code$ <> "1" then return         /* SKIP FOR CLOCK IN */
           if process% = 1% then return           /* SKIP IN (APCEMPVR)*/
           if str(dt_time$,1%,1%) = "A" then return  /* SKIP FOR ADJUST*/
           if emp_shft$ = "01" then return

           zz% = 901%
           convert str(dt_time$,1%,4%) to zz%, data goto L08530
L08530:
           if zz% > 900% then return
              sv_wk% = wk% : sv_day% = i%

L08570:       convert wk% to dt_wk$, pic(##)
              convert i% to dt_day$, pic(#)
/* CR2490 */
              convert barcode$ to e_no%, data goto L08830
              put str(dt_emp$,1%,4%), using L12345, e_no%
              str(dt_emp$,5%,1%) = " "
REM              dt_emp$ = barcode$
              dt_key$ = all(hex(00))
              str(dt_key$,1%,2%) = dt_yr_bi$
              str(dt_key$,3%,2%) = dt_wk$
              str(dt_key$,5%,1%) = dt_day$
              str(dt_key$,6%,5%) = dt_emp$
              read #4,key > dt_key$, using L08680 , dt_rec$,               ~
                                                  eod goto L08740
L08680:          FMT CH(40)
              if str(dt_rec$,1%,10%) <> str(dt_key$,1%,10%) then         ~
                                                  goto L08740
              if pass% = 0% then goto L08830
              if pass% = 1% then return

L08740:       pass% = pass% + 1%                 /* CHECK PREVIOUS DAY */
              if pass% = 2% then goto L08830
                 i% = i% - 1%
                 if i% <> 0% then goto L08800
                    i% = 7%
                    wk% = wk% - 1%
L08800:          if wk% = 0% then wk% = 52%
              goto L08570

L08830:   wk% = sv_wk%
          i%  = sv_day%
        return

        find_in_hour                             /* LAST CLOCK IN TIME */
          if dt_code$ <> "1" then return
             sav_hr$ = " " : sav_hr% = -1%
             dt_key$ = all(hex(00))
             str(dt_key$,1%,10%) = sav_key$
L08920:      read #4,key > dt_key$, using L08930 , dt_rec$, eod goto L09010
L08930:         FMT CH(40)
             dt_key$ = str(dt_rec$,1%,18%)
             if str(dt_rec$,1%,10%) <> sav_key$ then goto L09010
                if str(dt_rec$,19%,1%) <> "0" then goto L08920
                   sav_hr$ = str(dt_rec$,14%,2%)
                   convert sav_hr$ to sav_hr%, data goto L09010

                   goto L08920
L09010: return

        time_stamp
            dt_usr$ = userid$
            dt_dte$ = date
            init(" ") dt_ts$, dt_fil1$
            call "TIME" (dt_ts$)
*       RHH
        return

