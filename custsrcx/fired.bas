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
            * 09/07/07 ! (AWD005) - mod for out user              ! CMG *~
            * 09/18/09 ! (AWD006) - Mod to take 30 minutes lunch  ! DES *~
            *03/31/2010! (AWD007) mod to only allow dept 56 & 60  ! CMG *~
            *          !  to login as AES.  this will be controlle!     *~
            *          !  by user emp & em2                       !     *~
            *05/27/2010! (AWD008) mod to remove 4 hours if var cde! CMG *~
            *          !      is T                                !     *~
            *10/06/2010! (AWD009) mod to TX Time Clocks           ! CMG *~
            *03/08/2011! (AWD010) mod to check all 2nd and 3rd Shft!CMG *~
            *          !          time for user TXEMP             !     *~
            *01/20/2012! (AWD011) mod to TX Time Clocks (dept 57) ! DES *~
            *05/30/2012! (AWD012) mod to add rounding rules       ! CMG *~
            *12/03/2012! (AWD013) mod for weeksemp Sunday to Satur! CMG *~
            *01/23/2013! (AWD014) mod for Texas in & out user TEP ! CMG *~
            *          !           & TOU                          !     *~
            *03/30/2013! (AWD015) mod for file lengths            ! CMG *~    
            *06/06/2013! (AWD016) mod for check for 7 mins past   ! CMG *~
            *          !    clock out time                        !     *~
            *12/11/2013! (AWD017) mod to change vac and sick earn ! CMG *~
            *          !  to allow half day entries               !     *~
            *11/04/2014! (AWD018) mod to turn off NTX Lunch       ! CMG *~
            *11/30/2015! SR69760  mod to display floating holiday ! PWW *~
            *06/06/2016! SR73396  Remove hardcoding for dept      ! PWW *~
            *  /  /    !          Lunch & use e-Lunch instead.    !     *~
            *02/12/2019! CR-1894  Increase Emp Dept size          ! DES *~
            *05/11/2020! CR2490   Increas Employee Number size    ! RDB *~
            *************************************************************

        dim                                                              ~
            dt_usr$3,                    /* Time Stamp - User Id       */~
            dt_dte$6,                    /* Time Stamp - Date          */~
            dt_ts$8,                     /* Time Stamp - Time          */~
            dt_sys_tme$8,                /* System Time (AWD015)       */~
            dt_sys_dte$6,                /* System Date (AWD015)       */~
/* ADP  DT_FIL1$85,                         TIME STAMP - FILLER AREA   */~
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
            dt_system$4,                 /* System Time at punch (AWD012)*/~
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
/* ADP MT_FIL$35,                           FILLER AREA  (AWD015)      */~
            sav_key$10, sav_key2$10,     /* Copy of Detail Key Prim    */~
            mt_key$13, mt_key1$10,       /* Master Key PRIMARY / ALT   */~
            dt_key$18, dt_rec$128,       /* Detail Primary Key  AWD015 */~
            sav_key1$16, sav_hr$2,       /* Detail Primary Key         */~
            xx$(7%)31,                   /* Display Screen Text        */~
            in$(7%)31,                   /* Clock in Message           */~
            ot$(7%)31,                   /* Clock Out Message          */~
            er$(7%)31,                   /* Clock Error Message        */~
            np1$(7%)60,                  /* No Punch 1                 */~
            np2$(7%)60,                  /* No Punch 2                 */~
            np3$(7%)60,                  /* No Punch 3                 */~
            e_key$5,                     /* Employee Master Key        */~
            e_lname$15,                  /* Employee Last Name         */~
            e_fname$10,                  /* Employee First Name        */~
/* ADP E_FNAMEX$5,                          EMPLOYEE FIRST NAME        */~
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
            barcode$8,                   /* Employee Number Bar Code   */~
            vac_days$6,                  /* Employee Vacation Days AVAI*/~
            sick_days$6,                 /* Employee Sick Days AVAIL   */~
/*SR69760*/ float_days$6,                /* Employee Float Days AVAIL  */~
            points$6,                    /* Employee Current Points    */~
            errormsg$60,                 /* Error Message Display      */~
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

        dim f2%(10%),                    /* = 0 if the file is open    */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

            mat f2% = con

/* (AWD012) */
        dim low_time%(5%),     ~
            high_time%(5%),     ~
            punch_time%(5%)

/* (AWD014) */
        dim schema$8                     /* Schema                     */
        
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
                        keypos = 1,    keylen =  18,                     ~
                        alt key 1, keypos = 114, keylen =  6, dup

            select #5,  "NOINPCH"                                        ~
                                consec , recsize = 100

            select #6,  "NOOUTPCH"                                       ~
                                consec , recsize = 100

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 100%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 100%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
/* (AWD014) */
            texas% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #2, schema_err%)
            
                                                   /* LUNCH DEDUCTIONS */
            ld$(1%) = "045"  : ld$(5%) = "025" : ld$(9%)  = "045"
            ld$(2%) = "010"  : ld$(6%) = "030" : ld$(10%) = "060" /* changed from 50 to 60 */
            ld$(3%) = "015"  : ld$(7%) = "035" : ld$(11%) = "055"
            ld$(4%) = "020"  : ld$(8%) = "040" : ld$(12%) = "060"

            call "EXTRACT" addr("ID", userid$)
            if userid$ = "TXE" then texas% = 1%
            if userid$ = "TEP" then texas% = 1%
            if userid$ = "TOU" then texas% = 1%
              
            u3% = 0%
/* (AWD013)*/            
            days$(2%) = "MONDAY   " : days$(3%) = "TUESDAY  "
            days$(4%) = "WEDNESDAY" : days$(5%) = "THURSDAY "
            days$(6%) = "FRIDAY   " : days$(7%) = "SATURDAY "
            days$(1%) = "SUNDAY   "
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

            np1$(1%) = "                NNN     NN     OOOOOO                       "
            np1$(2%) = "                NNNN    NN    OOOOOOOO                      "
            np1$(3%) = "                NN NN   NN    OO    OO                      "
            np1$(4%) = "                NN  NN  NN    OO    OO                      "
            np1$(5%) = "                NN   NN NN    OO    OO                      "
            np1$(6%) = "                NN    NNNN    OOOOOOOO                      "
            np1$(7%) = "                NN     NNN     OOOOOO                       "

            np2$(1%) = "      TTTTTTTT   IIIIIIII    MMM      MM   EEEEEEEE         "
            np2$(2%) = "      TTTTTTTT   IIIIIIII    MMMM   MMMM   EEEEEEEE         "
            np2$(3%) = "         TT         II       MM MM MM MM   EE               "
            np2$(4%) = "         TT         II       MM  MM   MM   EEEEEE           "
            np2$(5%) = "         TT         II       MM       MM   EE               "
            np2$(6%) = "         TT      IIIIIIII    MM       MM   EEEEEEEE         "
            np2$(7%) = "         TT      IIIIIIII    MM       MM   EEEEEEEE         "

            np3$(1%) = "  PPPPPPP    UU     UU   NNN     NN     CCCC     HH    HH   "
            np3$(2%) = "  PP    PP   UU     UU   NNNN    NN   CC    CC   HH    HH   "
            np3$(3%) = "  PP    PP   UU     UU   NN NN   NN   CC         HHHHHHHH   "
            np3$(4%) = "  PPPPPPP    UU     UU   NN  NN  NN   CC         HHHHHHHH   "
            np3$(5%) = "  PP         UU     UU   NN   NN NN   CC         HH    HH   "
            np3$(6%) = "  PP         UU     UU   NN    NNNN   CC    CC   HH    HH   "
            np3$(7%) = "  PP           UUUUU     NN     NNN     CCCC     HH    HH   "

/* (AWD012) decmial format  dec_min% = round((tst_min%/60)*100,2) */
            low_time%(1%)   = 0
            low_time%(2%)   = 13
            low_time%(3%)   = 38
            low_time%(4%)   = 63
            low_time%(5%)   = 88

            high_time%(1%)  = 12
            high_time%(2%)  = 37
            high_time%(3%)  = 62
            high_time%(4%)  = 87
            high_time%(5%)  = 98

            punch_time%(1%) = 00
            punch_time%(2%) = 15
            punch_time%(3%) = 30
            punch_time%(4%) = 45
            punch_time%(5%) = 00

            gosub open_punch_file
            toggle% = 0%
            if userid$ = "OUT" then toggle% = 1%
            if userid$ = "TOU" then toggle% = 1%   /* (AWD014) */
            gosub toggle
        REM ************************************************************
        REM *              M a i n   P r o g r a m                     *
        REM ************************************************************

        main
            if sc_code$ = "0" and noinpch%  = 1% then goto nopunchlogic
            if sc_code$ = "1" and nooutpch% = 1% then goto nopunchlogic
            emp% = 0%
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

        nopunchlogic
          gosub nopunchmainmenu
          if keyhit% = 16% then goto exit_program
        goto nopunchlogic


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
               at (09,15), fac(hex(94)), errormsg$              , ch(60),~
                                                                         ~
               at (10,02), "Employee Bar Code Number",                   ~
               at (10,25), fac(hex(81)), barcode$               , ch(08),~
               at (10,35), fac(hex(99)), wandchar$              , ch(01),~
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
                  if sc_code$ = "0" and noinpch%  = 1% then goto nopunchmainmenu
                  if sc_code$ = "1" and nooutpch% = 1% then goto nopunchmainmenu
                  goto mainmenu

L02980:        if keyhit% <> 15% then L03010
                  call "PRNTSCRN"

L03010:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
            init(" ") barcode$, wandchar$, date$, etime$, rhh$, rhh1$,   ~
                      rhh_desc$, dt_time$, txt$(), x_time$, dt_system$
            process% = 0% : dt_time% = 0% : lunch_flag% = 0% : dt_system% = 0%
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


         nopunchmainmenu
        REM *************************************************************~
            * Display No Time Punch                                     *~
            *  ( SC9 )                                                  *~
            *************************************************************

            gosub set_pf2
            accept                                                       ~
               at (01,10), fac(hex(84)), np1$(1%)                , ch(60),~
               at (02,10), fac(hex(84)), np1$(2%)                , ch(60),~
               at (03,10), fac(hex(84)), np1$(3%)                , ch(60),~
               at (04,10), fac(hex(84)), np1$(4%)                , ch(60),~
               at (05,10), fac(hex(84)), np1$(5%)                , ch(60),~
               at (06,10), fac(hex(84)), np1$(6%)                , ch(60),~
               at (07,10), fac(hex(84)), np1$(7%)                , ch(60),~
                                                                         ~
               at (09,10), fac(hex(84)), np2$(1%)                , ch(60),~
               at (10,10), fac(hex(84)), np2$(2%)                , ch(60),~
               at (11,10), fac(hex(84)), np2$(3%)                , ch(60),~
               at (12,10), fac(hex(84)), np2$(4%)                , ch(60),~
               at (13,10), fac(hex(84)), np2$(5%)                , ch(60),~
               at (14,10), fac(hex(84)), np2$(6%)                , ch(60),~
               at (15,10), fac(hex(84)), np2$(7%)                , ch(60),~
                                                                         ~
               at (17,10), fac(hex(84)), np3$(1%)                , ch(60),~
               at (18,10), fac(hex(84)), np3$(2%)                , ch(60),~
               at (19,10), fac(hex(84)), np3$(3%)                , ch(60),~
               at (20,10), fac(hex(84)), np3$(4%)                , ch(60),~
               at (21,10), fac(hex(84)), np3$(5%)                , ch(60),~
               at (22,10), fac(hex(84)), np3$(6%)                , ch(60),~
               at (23,10), fac(hex(84)), np3$(7%)                , ch(60),~
                                                                         ~
                                                                         ~
               at (24,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 3% then goto L03980
                  gosub toggle
                  if sc_code$ = "0" and noinpch%  = 1% then goto nopunchmainmenu
                  if sc_code$ = "1" and nooutpch% = 1% then goto nopunchmainmenu

                  goto mainmenu

L03980:
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
            init(" ") barcode$, wandchar$, date$, etime$, rhh$, rhh1$,   ~
                      rhh_desc$, dt_time$, txt$(), x_time$, dt_system$
            process% = 0% : dt_time% = 0% : lunch_flag% = 0% : dt_system% = 0%
            date$ = date : call "DATFMTC" (date$)
            call "TIME" (etime$) : edit% = 0%
            inpmessage$ = "No Time Punch allowed at this time."

            pfkeys$ = hex(ffff03ffffffffffffffffffff0e0f1000)
        return

        REM *************************************************************~
            * Display This Screen If Barcode is Scanned And No          *~
            * Errors Occur.                                             *~
            *************************************************************

        display_screen
            print at(12,20);hex(84);"Employee Name   : ";name$
            print at(14,20);hex(84);"Daily Pay Hours : ";d_hours$;"   "; ~
                                    "Minutes : ";d_min$
            print at(15,20);hex(84);"Weekly Pay Hours: ";w_hours$;"   "; ~
                                    "Minutes : ";w_min$
            if e_display$ = "N" then goto L03510
/*SR69760*/    print at(16,20);hex(84);"Float Days Avail : ";float_days$
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
            call "PAUSE" addr(100%)

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
L03850:        errormsg$ = "(Error) Not Allowed to Clock In/Out at AES"
                 return                                  /* EMP% =  7% */
L03870:        errormsg$ = "(Error)-Salaried Employee. N/A ??"
                 return                                  /* EMP% =  8% */
L03890:        errormsg$ = "(Error) Only Allowed to Clock In/Out at AES"
                 return                                  /* EMP% =  9% */
L03910:        errormsg$ = "(Error) Only Allowed to Clock In/Out at TX AES"
                 return             /* (AWD009) */       /* EMP% = 10% */
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
/*SR69760*/                       e_float_days%, e_float_used%,          ~
                                  emp_shft$, e_lun$, e_itime$, e_otime$, ~
                                  e_display$, e_lunch$, e_lock$,         ~
                                  eod goto L04570
L04310:       FMT CH(3), CH(3), POS(12), CH(15), CH(10), CH(1), POS(152),~
                  CH(1), POS(218), BI(2), POS(226), BI(2),               ~
                  POS(234), BI(2), POS(242), BI(2), POS(308),            ~
/*                PD(14,4), POS(841), CH(02), CH(01), POS(1013), 5*CH(01),*/~
/*                POS(844), CH(5)                                         */~
/*SR69760*/       PD(14,4), POS(767), BI(2), POS(775), BI(2), POS(841),  ~
                  CH(02), CH(01), POS(1013), 5*CH(01) /*,POS(844), CH(5) ADP*/
                  
            mt_dept$ = e_dept$
            if e_payg$ = "180" or e_payg$ = "190" or e_payg$ = "200"     ~
                                                     then goto L04610
            x% = 0%
            x  = 0.0
            convert e_lun$ to x%, data goto L04400
L04400:
            lun$ = ld$(x% + 1%)        /* Set Employee Lunch Deduction */
REM NAME$ = E_LNAME$ & ", " & E_FNAME$ & E_FNAMEX$ & " " & E_INIT$ & "." ADP

            name$ = e_lname$ & ", " & e_fname$ & " " & e_init$ & "."                    
            if sc_dept$ <> " " then goto L04450
               sc_dept$ = mt_dept$
L04450:     if e_status$ <> "A" then goto L04590
               gosub load_windows
               if emp% <> 0% then return

/* (AWD017) */               
               e_vac_days = 0.0
               e_vac_days = (e_vac_days% / 10)
               
                                                   /*  (AWD003)  - Beg */
               e_vac_used = 0.0
               e_vac_used = (e_vac_used% / 10)
REM X  = (E_VAC_DAYS% - E_VAC_USED)
               x  = (e_vac_days - e_vac_used)

REM X% = E_VAC_DAYS% - E_VAC_USED%
REM CONVERT X% TO VAC_DAYS$, PIC(###)
                  convert x  to vac_days$, pic(##.#-)
                                                   /*  (AWD003)  - Beg */

/*(AWD017) */
               e_sick_days = 0.00
               e_sick_days = (e_sick_days% / 10)
                                                                  
                                                 /* (EWD001) - BEGIN */
               e_sick_used = 0.0
               e_sick_used = (e_sick_used% / 10)

REM X% = E_SICK_DAYS% - E_SICK_USED%
REM X  = (E_SICK_DAYS% - E_SICK_USED)
               x  = (e_sick_days - e_sick_used)

REM CONVERT X% TO SICK_DAYS$, PIC(###)
                  convert x  to sick_days$, pic(##.#-)
/*SR69760 + */
               e_float_days = 0.00
               e_float_days = (e_float_days% / 10)
                                                                  
                                                 /* (EWD001) - BEGIN */
               e_float_used = 0.0
               e_float_used = (e_float_used% / 10)

REM X% = E_SICK_DAYS% - E_SICK_USED%
REM X  = (E_SICK_DAYS% - E_SICK_USED)
               x  = (e_float_days - e_float_used)

REM CONVERT X% TO SICK_DAYS$, PIC(###)
                  convert x  to float_days$, pic(##.#-)
/*SR69760 - */
                                                 /* (EWD001) - END */
               convert points to points$, pic(##.##-)

/* (AWD007) */
REM IF TOGGLE% <> 0% THEN RETURN       /* Only scan in */
               if schema% = 2% then return           /* schema% = 2 NTX */
               if userid$ = "CMG" then return
               if userid$ = "PWW" then return
               if userid$ = "DSD" then return
               if userid$ = "RDB" then return
               if userid$ = "OUT" then return
               if userid$ = "TOU" then return     /* (AWD014) */
               if e_dept$ = "056" then goto checkAES
               if e_dept$ = "060" then goto checkAES
/*(AWD009)*/
               if e_dept$ = "057" then goto checkAESTX  /* AWD011 */
               if e_dept$ = "059" then goto checkAESTX
               if e_dept$ = "066" then goto checkAESTX
               if e_dept$ = "069" then goto checkAESTX
               if e_dept$ = "092" then goto checkAESTX
/* (/AWD009) */
/* All other departments */
                 if userid$ <> "EMP" then goto L04620
        return
checkAES:
                 if userid$ <> "EM2" then goto L04630
        return
/*(AWD009)*/
checkAESTX:
                 if userid$ <> "TXE" then goto L04635
        return

L04570:    emp% = 1%                           /* Employee Not On File */
        return
L04590:    emp% = 2%                           /* Employee Not Active  */
        return
L04610:    emp% = 8%                           /* Employee SALARIED    */
        return
/*(AWD007)*/
L04620:    emp% = 7%                           /* Only Clock in at AWD  */
        return
L04630:    emp% = 9%                           /* Only Clock in at AES  */
        return
L04635:    emp% = 10%                          /* Only Clock in at TX AES*/
        return
/*(AWD007\)*/

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


              if hr1% = 0% then hr1% = 24%

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

/* (AWD013) changed from AWDPLN0B to AWDEMP0B */
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
                dt_yr$  = ent_yr$                    /* Detail Year    */
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

            dt_time$ = time     /* Check Edit Flag */

REM  IF USERID$ <> "TXE" THEN GOTO NOTTXEMP
            if texas% = 0% then goto notTXEmp
            txhr% = 0%                     /* Check for 00 Midnight */
            if str(dt_time$,1,2) = "00" then str(dt_time$,1,2) = "24"
            convert dt_time$ to txhr%, data goto badTime
                                       /* Use three(3) zeros to subst */
                                       /* bc length of dt_time$ = 5   */
               txhr% = txhr% - 1000    /* Substract 1 hour from time   */

            convert txhr% to dt_time$, pic(00000)

badTime:

*       RHH
notTXEmp:
            if edit% = 0% then goto L05490
               dt_time$ = rhh$
               if str(dt_time$,1%,1%) = "A" then goto L05550
                                                 /* No Test Adjustment */
L05490:        convert str(dt_time$,1%,4%) to x%, data goto L05690

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
L05860:       FMT CH(17)
              if str(dt_key$,14%,1%) = "A" then goto L05900
              if process% = 1% then goto L05900
                 if str(dt_key$,1%,16%) = sav_key1$ then goto L06070
L05900:    gosub check_lock
           if emp% <> 0% then return

           dt_key$ = all(hex(00))
           str(dt_key$,1%,10%) = sav_key$
           str(dt_key$,11%,3%) = dt_dept$
           str(dt_key$,14%,5%) = dt_time$
           hold_key$ = dt_key$   /* store key to remove rec on dept29_lunch */
           read #4,hold,key = dt_key$, eod goto L05990
              delete #4
L05990:    gosub time_stamp
           put #4,using L06030, dt_yr_bi$,dt_wk$,dt_day$, dt_emp$, dt_dept$,  ~
                              dt_time$, dt_code$, sc_code$, dt_fil$,          ~
                              dt_usr$, dt_dte$, dt_ts$, dt_sys_dte$,          ~
                              dt_sys_tme$ /*, dt_fil1$ */
L06030:       FMT 2*CH(2), CH(1), CH(5), CH(3), CH(5), CH(1), CH(1),     ~
                    CH(1), CH(3), CH(6), CH(8), CH(06), CH(08)
/* ADP , CH(77)  */
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
              c_t$(j1%) = str(dt_key$,13%,5%)
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
            mt_proc$ = "0" : code$ = "0"
REM ADP MT_FIL$ = "   "            
            mt_key$ = all(hex(00))
            str(mt_key$,1%,3%)  = sav_dept$
            str(mt_key$,4%,10%) = sav_key2$
            read #3,hold,key = mt_key$, using L07270, mt1$, mt2$, mt3$,    ~
                                              d29_lun$,  eod goto L07330
L07270:        FMT POS(24), 3*CH(1), POS(33), CH(1)
               put #3, using L07290, mt_hours%, mt_min%
L07290:           FMT POS(20), 2*BI(2)
               f1% = 1%
               goto write_master

L07330:     put #3, using  L07370, sav_dept$, mt_yr_bi$, mt_emp$, mt_wk$,   ~
                    mt_day$, mt_date$, mt_hours%, mt_min%,               ~
                    mt_var_in$, mt_var_out$, mt_var_day$, mt_proc$,      ~
                    userid$ /* , MT_FIL$ ADP */
L07370:   FMT CH(3), CH(2), CH(5), CH(2), CH(1), CH(6), 2*BI(2), 4*CH(1),~
                   CH(3) /*, CH(35) ADP */   /* (AWD015) */
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
/*(AWD012) process rounding for both clock in and clock out*/
REM IF DT_CODE$ <> "0" THEN RETURN        /* ONLY CHECK IN      */

           if process% = 1% then return          /* TIME STAMP SCREEN  */
           if t_in% = 0% then return             /* NO WINDOW SPECIFIED*/
REM IF E_LOCK$ = "N" THEN GOTO 7910      /* NO LOCK            */
           goto L07630                           /* (AWD012) */
REM GOTO L07620

              if dt_time% >= (t_in% - 7%) then goto L07620
                 emp% = 12%
        return
                                                 /* ROUND UP CLOCK IN  */
L07620:    if dt_time% >= (t_in% - 15%) and dt_time% <= t_in% then       ~
                                                                goto L07650
        return
/* (AWD012) */
L07630:
           tst_hr%, tst_min%, dec_min% = 0%
           convert str(dt_time$,1%,2%) to tst_hr%, data goto bad_sys_time

           convert str(dt_time$,3%,2%) to tst_min%, data goto bad_sys_time
           dec_min% = round((tst_min%/60)*100,2)      /* Convert to Decimal Time */
           for c% = 1% to 5%
             if dec_min% >= low_time%(c%) and dec_min% <= high_time%(c%) ~
                     then goto L07660
           next c%

L07650:    dt_time% = t_in%
           dt_time$ = e_itime_d$
        return
L07660:
          if c% <> 5% then goto no_round_hr
             tst_hr% = tst_hr% + 1%
REM IF TST_HR% >= 24% THEN TST_HR% = 0%

no_round_hr:

          convert tst_hr% to str(dt_time$,1%,2%), pic(00)
          convert punch_time%(c%) to str(dt_time$,3%,2%), pic(00)

          convert dt_time$ to dt_time%, data goto badtime
        return
bad_sys_time:
        return
/* (\AWD012) */
        check_in
           if t_in% = 0% then goto L07740
                                                     /* CHECK LATE IN  */
/* (AWD012) change to system time instead of rounded time*/                                                     
REM IF DT_TIME% < (T_IN% + 3%) THEN GOTO L07740
           if dt_system% < (t_in% + 3%) then goto L07740
              code$ = "2" : goto L07770
L07740:    put #3, using  L07750, "0"
L07750:       FMT POS(24), CH(1)
        return
L07770:    put #3, using L07780, code$, "9", "0"
L07780:       FMT POS(24), CH(1), POS(26), CH(1), POS(33), CH(1)
        return

        check_out
/* check for check_in from lunch */
           if t_out% = 0% then goto L07900
           if lunch_flag% = 0% then goto L07860
              dt_time% = dt_time_sav%
                                                     /* CHECK EARLY OUT*/
L07860:    
/* (AWD012) change to system time instead of rounded time*/
REM IF DT_TIME% >= (T_OUT% - 7%) THEN GOTO L07890
           if dt_system% >= (t_out% - 7%) then goto L07890
              code$ = "3" : goto L07960
                                                     /* CHECK LATE OUT */
L07890:    if (t_out% + 7%) >= dt_system% then goto L07920
              code$ = "4" : goto L07960

L07920:       if mt1$ <> "0" then goto L07930
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
/* (AWD014) */
REM IF SCHEMA% = 2% THEN E_LUNCH$ = "Y"              /* (AWD018) */
/* do not use texas% b/c one of the texas logins is still nc data */           
           if schema% <> 1% then goto not_NC_lunch 
/* IF E_DEPT$ = "29" THEN E_LUNCH$ = "Y"                                    */
/* IF E_DEPT$ = "35" THEN E_LUNCH$ = "Y"    AWD006                          */
/*SR73396   IF E_DEPT$ = "61" THEN E_LUNCH$ = "Y"    AWD006                 */
/*SR73396   IF E_DEPT$ = "61" AND E_LUNCH$ = "Y" AND E_LUN$ <> "5"        ~
                               THEN E_LUNCH$ = "N"    AWD006                */
           
not_NC_lunch:
           if e_lunch$ = "N" then return
           if dt_hours% < 6% then return
              lunch_flag% = 1%              /* SAVE THE ACTUAL CLOCK   */
              dt_time_sav% = dt_time%       /* OUT TIME                */
              dt_code$ = "2"
              dt_time$ = "A" & lun$ & "-"   /* Lunch Deduction - Mod   */
REM DT_TIME$ = "A045-"            /* DEDUCTION FOR LUNCH     */
/* (AWD014) */
REM IF SCHEMA% = 2% THEN DT_TIME$ = "A030-"     /* DEDUCTION FOR LUNCH      */

/*SR73396  IF SCHEMA% = 1% AND E_DEPT$ = "29" THEN       ~
              DT_TIME$ = "A030-"               DEDUCTION FOR LUNCH          */
/*SR73396  IF SCHEMA% = 1% AND E_DEPT$ = "35" THEN       ~
              DT_TIME$ = "A030-"               DEDUCTION FOR LUNCH          */
/*SR73396  IF SCHEMA% = 1% AND E_DEPT$ = "61" THEN       ~
              DT_TIME$ = "A030-"               DEDUCTION FOR LUNCH          */
              gosub write_detail
              dt_code$ = sc_code$
              gosub total_day
        return

        dept29_lunch
/* for later when firm lunch times */
           if schema% <> 1% then return
           if e_dept$ <> "029" then return
/* get lunch sched, if near lunch calc length */
            init(" ") readkey$, e_otime_d$
            str(readkey$,1%,9%)   = "EMP LTIME"
            str(readkey$,10%,15%) = e_ltime$
            read #2,key = readkey$,using L08100 ,e_ltime_d$, eod goto L08199
L08100:         FMT POS(25), CH(4)
            convert e_ltime_d$ to l_ltime%, data goto L08199
            convert str(e_ltime_d$,1,2) to l_hr%, data goto L08199
            convert str(e_ltime_d$,3,2) to l_mn%, data goto L08199
            start% = (l_hr% * 60) + l_mn%
REM START% = 720%
           len%   = 30%
           gosub convert_time
           if dt_time% < start% then goto L08199
REM IF DT_TIME% > (START% + 90%) THEN GOTO L08199
REM IF DT_TIME% < START% THEN DT_TIME% = DT_TIME% + 1440%
           if dt_time% > (start% + len%) then len% = dt_time% - start%
/* if length > lun$, use it */
           convert len% to lun$, pic (000)
           read #4,hold,key = hold_key$, eod goto L08198
           delete #4
L08198:

              lunch_flag% = 1%              /* SAVE THE ACTUAL CLOCK   */
              dt_time_sav% = dt_time%       /* OUT TIME                */
              dt_code$ = "2"
              dt_time$ = "A" & lun$ & "-"   /* Lunch Deduction - Mod   */
              gosub write_detail
              dt_code$ = sc_code$
              gosub total_day
L08199:
        return

        total_week
           mt_key1$ = all(hex(00))
           str(mt_key1$,1%,x10%) = str(sav_key2$,1%,10%)
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
REM P% = POS("ACLNX" = T_PROC$)
/*- AWD004 -- remove Z from hours ----------*/
REM P% = POS("ACLNXZSTP$&R^@" = T_PROC$)
             p% = pos("ACLNXZSP$&R^@" = t_proc$)
             if p% <> 0% then goto total_wk_next       /* NO PAY HOURS */
             p% = pos("T" = t_proc$)
             if p% <> 0% then gosub minusHours         /* (AWD008) */
                x% = x% + mt_hours%
                y% = y% + mt_min%
             goto total_wk_next
        total_wk_done
             x% = x% + int(y%/60.0)             /* CONVERT TO HOURS  */
             y% = mod(y%,60.0)                  /* LEFT OVER MINUTES */
           convert x% to w_hours$, pic(##)
           convert y% to w_min$, pic(##)
        return

        minusHours                                      /* (AWD008) BEGIN */
           zz% = (mt_hours% * 60) + mt_min%
           zz% = zz% - 240%                             /* Remove 4 hours */
           mt_hours% = int(zz%/60.0)
           mt_min%   = mod(zz%,60.0)
        return                                         /* (AWD008) END */

        calc_prod_day
           if dt_code$ <> "1" then return         /* SKIP FOR CLOCK IN */
           if process% = 1% then return           /* SKIP IN (APCEMPVR)*/
           if str(dt_time$,1%,1%) = "A" then return  /* SKIP FOR ADJUST*/
           if emp_shft$ = "01" then return

           zz% = 901%
           convert str(dt_time$,1%,4%) to zz%, data goto L08530
L08530:
/* (AWD010) */
REM IF ZZ% > 900% THEN RETURN
REM IF ZZ% > 900% AND USERID$ <> "TXE" THEN RETURN
REM NOTE after 9 am check for clock in day
              if zz% > 900% and texas% = 0% then return

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
L08680:          FMT CH(128)              /* (AWD015) */
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
          if dt_code$ <> "1" then return         /* dt_code$ = "1" then punchout */
             sav_hr$ = " " : sav_hr% = -1%
             dt_key$ = all(hex(00))
             str(dt_key$,1%,10%) = sav_key$
L08920:      read #4,key > dt_key$, using L08930 , dt_rec$, eod goto L09010
L08930:         FMT CH(128)       /* (AWD015) */
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
            dt_sys_dte$ = date
            init(" ") dt_ts$, dt_sys_tme$, dt_tme_err$
REM CALL "TIME" (DT_TS$)     /* (AWD015) */
            call "TIME" (dt_sys_tme$)    /* (AWD015) */
*       RHH
/* (AWD012) */
/* (AWD015) */   
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
            
            init(" ") dt_system$
            dt_system% = 0%
            dt_system$ = dt_ts$
            
            convert str(dt_system$,1%,2%) to sys_hr1%, data goto bad_sys_hr

bad_sys_hr:

           convert str(dt_system$,3%,2%) to sys_hm1%, data goto bad_sys_min
bad_sys_min:

            dt_system% = (60%*sys_hr1%) + sys_hm1%      /* CLOCK IN MINUTES */ 

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

        open_punch_file
          noinpch%  = 0%
          nooutpch% = 0%
          f2%(5%) = 0%
* First check to see if file exists; if so then end only run once
          call "OPENOLIB" (#5, "VALID", f2%(5%), rslt$(5%), " ")
REM IF F2%(5%) = 0% THEN END                                 /* file found  */
             if f2%(5%) = 0% then noinpch%  = 1%

* Second if no file open in OUTLIB or SESDBASE
REM CALL "OPENOLIB" (#5, "OUTPT", F2%(5%), RSLT$(5%), " ")

          f2%(6%) = 0%
* First check to see if file exists; if so then end only run once
          call "OPENOLIB" (#6, "VALID", f2%(6%), rslt$(6%), " ")
REM IF F2%(6%) = 0% THEN END                                 /* FILE FOUND  */
             if f2%(6%) = 0% then nooutpch% = 1%

* Second if no file open in OUTLIB or SESDBASE
REM CALL "OPENOLIB" (#6, "OUTPT", F2%(6%), RSLT$(6%), " ")
        return
