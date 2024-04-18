        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCEMP1B                             *~
            *  Creation Date     - 04/16/96                             *~
            *  Last Modified Date- 06/17/05                            *~
            *  Description       - This Program provides Employee P/R   *~
            *                      Entry/Update and Inquiry.            *~
            *                                                           *~
            *  Special Comments  - Subroutine for APCEMPED              *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/16/96 ! New Program for (APC) - Last Mod Date    ! JBF *~
            * 05/0798  ! Modify edit to allow blank fields        ! ERN *~
            * 10/03/01 ! Mod to change Job Title on Screen to     ! CMG *~
            *          !    'Manager Name'        (EWD001)        !     *~
            * 06/17/05 ! Mod for security; not to display certain ! CMG *~
            *          !   fields (AWD002)                        !     *~
            * 05/22/06 ! Mod for government mandated EEOC changes ! DES *~
            *          !          (AWD009)                        !     *~
            *10/07/2009! EEOC Group Code             (AWD010)     ! DES *~
            *04/22/2010! Add security as in apcemped (AWD011)     ! DES *~
            *05/01/2020! CR2490 Increase employee number size     ! RDB *~
            *************************************************************

        sub "APCEMP1B"   (e_no$,         /* Employee No.               */~
                          e_lname$,      /* Employee Last Name         */~
                          e_fname$,      /* Employee First Name        */~
                          e_init$,       /* Employee Middle Initial    */~
                          #2,            /* GENCODES    (AWD002)       */~
                          #3,            /* APCEMPPR    (AWD002)       */~
                          #5,            /* APCEMPEX    (AWD010)       */~
                          sec_lvl$     ) /*             (AWD011)       */

        dim e_gender$1,                  /* Employee Gender            */~
            e_race$1,                    /* Employee Race              */~
            e_no$5,                      /* Employee number key        */~
            scre_no$8,                   /* Screen employee number 2490*/~
            e_wh_st_stat$1,              /* State Withholding Status   */~
            e_wh_st_exempt$1,            /* State Withholding Exemption*/~
            e_wh_fd_stat$1,              /* Fed.  Withholding Status   */~
            e_wh_fd_exempt$1,            /* Fed.  Withholding Exemption*/~
            e_pay_stat$1,                /* Payroll Status             */~
            e_pay_freq$1,                /* Payroll Frequency          */~
            e_class$2,                   /* Employee Classification    */~
            e_pay_grade$2,               /* Pay Grade                  */~
            e_i9_rev$1,                  /* I-9 Reverification Flag    */~
            e_i9_rev_dte$8,              /* I-9 Reverification Date    */~
            e_job_title$25,              /* Employee Job Title         */~
            e_cross_train$(4%)25,        /* X-Trained Job Titles (4)   */~
            e_userid$3,                  /* Userid of Last Modifier    */~
            e_mod_dte$8,                 /* Last Mod. Date             */~
            e_fill1$6,                   /* APCEMPPR Filler Area       */~
            e_lname$15,                  /* Employee Last Name         */~
            e_fname$15,                  /* Employee First Name        */~
            e_init$1,                    /* Employee Middle Initial    */~
            pay_rate$8,                  /* Employee Pay Rate (Base)   */~
            shift_diff$8,                /* Employee Shift Differential*/~
            st_add$8,                    /* Add. State   Withholding   */~
            fd_add$8,                    /* Add. Federal Withholding   */~
            name_desc$28,                /* Name String Field (Display)*/~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$60,                 /* Error message              */~
            i$(24%)80,                   /* Detail Line(10) Array Area */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            progid$18,                   /* Screen Line #2 Program ID  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
        /* AWD009 start */                                           ~
            e_jobeeoc$2,                 /* EEOC job type              */~
            e_grpeeoc$3,                 /* EEOC job type              */~
            e_jobeeoc%,                  /* EEOC job type              */~
            e_military$4,                /* Military status            */~
            EeocDesc$25,                 /* EEOC Job Type Descriptions */~
            EeocGDesc$25,                 /* EEOC Job Type Descriptions */~
            RaceDesc$25,                 /* EEOC Race Description      */~
            MilDesc$25,                  /* Veteran Status Description */~
            ex_rec$256,                  /* shadow file record         */~
        /* AWD009 end   */                                           ~
            userid$3                     /* Current User Id            */

        dim readkey$50,                  /* Table Key     (AWD008)     */~
            access$30                    /* DEPARTMENT ACCESS  (AWD008)*/

        dim tmp_no$5                     /* Employee No.               */

        dim f2%(3%),                                                     ~
            f1%(3%),                                                     ~
            fs%(4%),                                                     ~
            rslt$(4%)20

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.10.00 05/07/98 Edit Enhancement Project   "
        REM *************************************************************
            mat f2% = con

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            tmp_no$ = e_no$
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Press (RETURN)."&~
                           "                                   "
            progid$ = "APCEMP1B: " & str(cms2v$,,8)

REM         gosub load_security_hr                /* (AWD002)  */
            security_hr% = 0%
        /* security_hr = 1 = edit, 2 = view only */
            des$ = sec_lvl$
            if sec_lvl$ = "1" then security_hr% = 2%
            if sec_lvl$ = "3" then security_hr% = 2%
            if sec_lvl$ = "4" then security_hr% = 1%
            if sec_lvl$ = "5" then security_hr% = 1%

        /* AWD009 start */
            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#04, fs%(04),      0%,  0%, rslt$(04))
        /* AWD009 end   */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode

            gosub initialize_variables

            inpmessage$ = edtmessage$

            gosub dataload

            goto editpg1


        REM goto L11240

            for fieldnr% = 1% to 1%
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit% =  1% then gosub startover
                     if keyhit% = 16% then exit_sub
                     if keyhit% <> 0%           then L10130
            gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " "        then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1

            inpmessage$ = edtmessage$

            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit% =  1%       then gosub startover
                if keyhit% = 14%       then gosub dataput
                if keyhit% = 16%       then exit_sub
                if keyhit% <> 0%       then editpg1

            inpmessage$ = "Make changes and then Press (RETURN)."

L11200:     gosub'101(1, 2%)            /* Display & Accept Screen     */
                if keyhit% =  1%       then gosub startover
                if keyhit% <> 0%       then L11200

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "    then L11200

            goto editpg1


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, e_gender$, e_race$,        ~
                e_wh_st_stat$, e_wh_st_exempt$, e_wh_fd_stat$, e_class$, ~
                e_wh_fd_exempt$, e_pay_stat$, e_pay_freq$, e_pay_grade$, ~
                e_i9_rev$,e_i9_rev_dte$, e_job_title$, e_userid$,        ~
                e_mod_dte$, e_cross_train$()
/*AWD009*/  init(" ") e_jobeeoc$, e_miltary$
            init(" ") e_grpeeoc$, ex_rec$
            init(" ") EeocGDesc$
            init(" ") EeocDesc$
            e_wh_st_add = 0.00 : e_wh_fd_add  = 0.00
            e_pay_rate  = 0.00 : e_shift_diff = 0.00
            st_add      =  round(e_wh_st_add, 2)
            convert st_add       to st_add$,     pic(####0.00)

            fd_add      =  round(e_wh_fd_add, 2)
            convert fd_add       to fd_add$,     pic(####0.00)

                                         /* (AWD002) - BEG */
            pay_rate    =  round(e_pay_rate, 2)
            shift_diff  =  round(e_shift_diff, 2)

REM            if userid$ <> "MVK" and userid$ <> "CMG" then goto no_pay_info1
REM         if security_hr% = 0% and (e_pay_stat$ = "E" or e_pay_stat$ = "N") ~
                               then goto no_pay_info1
REM         if sec_lvl$ <> "1" then goto no_pay_info1 /* AWD011 */
            if security_hr% < 1% then goto no_pay_info1

            convert pay_rate     to pay_rate$,   pic(####0.00)


            convert shift_diff   to shift_diff$, pic(####0.00)

no_pay_info1
                                         /* (AWD002) - END */

            name_desc$  =  e_fname$ & " " & e_init$ & ". " & e_lname$
            edit% = 0%
            return

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

            if u3%   = 1% then return
            return clear all
            security_hr% = 0%
        /* security_hr = 1 = edit, 2 = view only */
            if sec_lvl$ = "1" then security_hr% = 2%
            if sec_lvl$ = "3" then security_hr% = 2%
            if sec_lvl$ = "4" then security_hr% = 1%
            if sec_lvl$ = "5" then security_hr% = 1%
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            rec% = 0%
        init(" ") ex_rec$, e_grpeeoc$
            read #5,key = e_no$, using APCEMPEX, ex_rec$,  eod goto L30350
        e_grpeeoc$ = str(ex_rec$,6,3)
            goto L30358
L30350:
        init(" ") ex_rec$, e_grpeeoc$
            str(ex_rec$,1,5) = e_no$
            write #5, using APCEMPEX, ex_rec$
APCEMPEX: FMT CH(256)
L30358:
            read #3,key = e_no$, eod goto L30490

REML30358:
            get #3, using L35030,                                         ~
                     e_no$,              /* Employee No.               */~
                     e_gender$,          /* Employee Gender            */~
                     e_race$,            /* Employee Race              */~
                     e_wh_st_stat$,      /* State Withholding Status   */~
                     e_wh_st_exempt$,    /* State Withholding Exemption*/~
                     e_wh_st_add,        /* State Withholding Addition */~
                     e_wh_fd_stat$,      /* Fed.  Withholding Status   */~
                     e_wh_fd_exempt$,    /* Fed.  Withholding Exemption*/~
                     e_wh_fd_add,        /* Fed.  Withholding Addition */~
                     e_pay_stat$,        /* Payroll Status             */~
                     e_pay_freq$,        /* Payroll Frequency          */~
                     e_class$,           /* Employee Classification    */~
                     e_pay_grade$,       /* Pay Grade                  */~
                     e_pay_rate,         /* Pay Rate (Base)            */~
                     e_shift_diff,       /* Shift Differential         */~
                     e_i9_rev$,          /* I-9 Reverification Flag    */~
                     e_i9_rev_dte$,      /* I-9 Reverification Date    */~
                     e_job_title$,       /* Employee Job Title         */~
                     e_cross_train$(),   /* X-Trained Job Titles (4)   */~
                     e_userid$,          /* Userid of Last Modifier    */~
                     e_mod_dte$,         /* Last Mod. Date             */~
                     e_jobeeoc$,         /* eeoc job type              */~
                     e_military$         /* military status Area       */

            sav_pay_grade$ = e_pay_grade$
            sav_pay_rate   = e_pay_rate
            sav_shift_diff = e_shift_diff
            if e_i9_rev_dte$   = " " then L30360
            call "DATEFMT" (e_i9_rev_dte$)

L30360:     st_add             = round(e_wh_st_add, 2)
            convert st_add     to st_add$,     pic(####0.00)

            fd_add             = round(e_wh_fd_add, 2)
            convert fd_add     to fd_add$,     pic(####0.00)

                                         /* (AWD002) - BEG */
            pay_rate           = round(e_pay_rate, 2)
            shift_diff         = round(e_shift_diff, 2)

REM         if userid$ <> "MVK" and userid$ <> "CMG" then goto no_pay_info
REM         if security_hr% = 0% and (e_pay_stat$ = "E" or e_pay_stat$ = "N") ~
                               then goto no_pay_info
REM         if ok% <> 1% then goto no_pay_info
REM         if sec_lvl$ <> "1" then goto no_pay_info  /* AWD011 */
            if security_hr% < 1% then goto no_pay_info1
            convert pay_rate   to pay_rate$,   pic(####0.00)


            convert shift_diff to shift_diff$, pic(####0.00)
no_pay_info
                                         /* (AWD002) - END */

        REM EEOC Race
        if e_race$ = "  " then goto L30390
            readkey$ = all(hex(00))
            readkey$ = "EEOC RACE" & e_race$
            call "DESCRIBE" (#2, readkey$, RaceDesc$, 0%, f1%(2))

L30390: REM EEOC Job Type
        if e_jobeeoc$ = "  " then goto L30395
            readkey$ = all(hex(00))
            convert e_jobeeoc$ to e_jobeeoc%
            convert e_jobeeoc% to e_jobeeoc$, pic (00)
            readkey$ = "APCEEOCJT" & e_jobeeoc$
            call "DESCRIBE" (#2, readkey$, EeocDesc$, 0%, f1%(2))

L30395: REM EEOC Job Type
        if e_grpeeoc$ = "   " then goto L30400
            readkey$ = all(hex(00))
            readkey$ = "APCEEOCGR" & e_grpeeoc$
            call "DESCRIBE" (#2, readkey$, EeocGDesc$, 0%, f1%(2))

L30400: REM  Veteran Status
        if e_military$ = "    " then goto L30430
            readkey$ = all(hex(00))
            readkey$ = "APCVETSTA" & e_military$
            call "DESCRIBE" (#2, readkey$, MilDesc$, 0%, f1%(2))

L30430:     rec% = 1%

L30490:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ( "Updating APCEMPPR Data" )
            read #3,hold,key = e_no$, eod goto L31110

            delete #3

L31110:     if e_i9_rev_dte$ = " " then L31140
            call "DATUNFMT" (e_i9_rev_dte$)

L31140:     convert st_add$     to e_wh_st_add,  data goto L31160

L31160:     convert fd_add$     to e_wh_fd_add,  data goto L31180

L31180:
REM         if security_hr% = 0% and (e_pay_stat$ = "E" or e_pay_stat$ = "N") ~
                               then goto L31220
            e_pay_grade$ = sav_pay_grade$
            e_pay_rate   = sav_pay_rate
            e_shift_diff = sav_shift_diff
REM         if sec_lvl$ = "2" then goto L31220
            if security_hr% <> 1% then goto L31220
            convert pay_rate$   to e_pay_rate,   data goto L31200

L31200:     convert shift_diff$ to e_shift_diff, data goto L31220

L31220:     e_userid$  = userid$
            e_mod_dte$ = date$
            put #3, using L35030,                                         ~
                     e_no$,              /* Employee No.               */~
                     e_gender$,          /* Employee Gender            */~
                     e_race$,            /* Employee Race              */~
                     e_wh_st_stat$,      /* State Withholding Status   */~
                     e_wh_st_exempt$,    /* State Withholding Exemption*/~
                     e_wh_st_add,        /* State Withholding Addition */~
                     e_wh_fd_stat$,      /* Fed.  Withholding Status   */~
                     e_wh_fd_exempt$,    /* Fed.  Withholding Exemption*/~
                     e_wh_fd_add,        /* Fed.  Withholding Addition */~
                     e_pay_stat$,        /* Payroll Status             */~
                     e_pay_freq$,        /* Payroll Frequency          */~
                     e_class$,           /* Employee Classification    */~
                     e_pay_grade$,       /* Pay Grade                  */~
                     e_pay_rate,         /* Pay Rate (Base)            */~
                     e_shift_diff,       /* Shift Differential         */~
                     e_i9_rev$,          /* I-9 Reverification Flag    */~
                     e_i9_rev_dte$,      /* I-9 Reverification Date    */~
                     e_job_title$,       /* Employee Job Title         */~
                     e_cross_train$(),   /* X-Trained Job Titles (4)   */~
                     e_userid$,          /* Userid of Last Modifier    */~
                     e_mod_dte$,         /* Last Mod. Date             */~
                     e_jobeeoc$,         /* eeoc job type              */~
                     e_military$         /* military status            */

            write #3, eod goto L31540
            init(" ") ex_rec$
            read #5, key = e_no$, hold, using APCEMPEX, ex_rec$
            str(ex_rec$,6,3) = e_grpeeoc$
            rewrite #5, using APCEMPEX, ex_rec$

            if e_i9_rev_dte$ = " " then L31530
            call "DATEFMT" (e_i9_rev_dte$)

L31530:     goto L31560
L31540:     call "SHOSTAT" ( "ERROR - Unable to Update APCEMPPR"  ) : stop

L31560: return clear all
        goto editpg1

        REM *************************************************************~
            *               F O R M A T    S T A T E M E N T S          *~
            *************************************************************
L35030:         FMT  CH(05),             /* Employee No.               */~
                     CH(01),             /* Employee Gender            */~
                     CH(01),             /* Employee Race              */~
                     CH(01),             /* State Withholding Status   */~
                     CH(01),             /* State Withholding Exemption*/~
                     PD(14,4),           /* State Withholding Addition */~
                     CH(01),             /* Fed.  Withholding Status   */~
                     CH(01),             /* Fed.  Withholding Exemption*/~
                     PD(14,4),           /* Fed.  Withholding Addition */~
                     CH(01),             /* Payroll Status             */~
                     CH(01),             /* Payroll Frequency          */~
                     CH(02),             /* Employee Classification    */~
                     CH(02),             /* Pay Grade                  */~
                     PD(14,4),           /* Pay Rate (Base)            */~
                     PD(14,4),           /* Shift Differential         */~
                     CH(01),             /* I-9 Reverification Flag    */~
                     CH(08),             /* I-9 Reverification Date    */~
                     CH(25),             /* Employee Job Title         */~
                     4*CH(25),           /* X-Trained Job Titles (4)   */~
                     CH(03),             /* Userid of Last Modifier    */~
                     CH(08),             /* Last Mod. Date             */~
                     CH(02),             /* eeoc job type              */~
                     CH(04)              /* military status            */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)

            gosub set_pf1

            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
/* <AWD011> */
            if security_hr% = 1% and fieldnr% > 0% then gosub L40140
            if security_hr% = 2% and fieldnr% > 0% then gosub L40142
REM         if security_hr% > 0% and fieldnr% > 0% then gosub L40140
REM         if fieldnr% > 0% then gosub L40140
/* </AWD011> */
            goto accept_screen
                lfac$(fieldnr%) = hex(80)  :  return   /* Up / Low   */
L40140:         lfac$(1%)       = hex(81)              /* Upper Only */
                lfac$(2%)       = hex(81)  :  return   /* Upper Only */
L40142:         lfac$(1%)       = hex(8C)              /* Upper Only */
                lfac$(2%)       = hex(8C)  :  return   /* Upper Only */
                lfac$(fieldnr%) = hex(82)  :  return   /* Numeric    */

        accept_screen
REM         if e_no$ < "00000" then e_no$ = tmp_no$
            e_no$ = tmp_no$
/* CR2490 */
L12345:             FMT BI(4)
            get str(tmp_no$,1%,4%), using L12345, scre_no%
            convert scre_no% to scre_no$, pic(#######0)
            accept                                                       ~
                at (01,02),                                              ~
                   "Employee Master Payroll Information"        ,        ~
                                                                         ~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                                                                         ~
                at (02,02), fac(hex(94)),   errormsg$           , ch(60),~
                at (02,63), fac(hex(8c)),   progid$             , ch(18),~
                                                                         ~
                at (03,02), "Employee No.      :"               ,        ~
                at (03,22), fac(hex(84)),   scre_no$            , ch(08),~
                at (03,39), fac(hex(84)),   name_desc$          , ch(28),~
                                                                         ~
                at (04,02), "Gender            :"               ,        ~
                at (04,22), fac(lfac$(1%)), e_gender$           , ch(01),~
                                                                         ~
                at (05,02), "Race              :"               ,        ~
                at (05,22), fac(lfac$(1%)), e_race$             , ch(01),~
                at (05,55), fac(hex(84)), RaceDesc$             , ch(25),~
                                                                         ~
                at (06,02), "Pay Status        :"               ,        ~
                at (06,22), fac(lfac$(1%)), e_pay_stat$         , ch(01),~
                                                                         ~
                at (06,30), "EEOC Job Type     :"               ,        ~
                at (06,50), fac(lfac$(1%)), e_jobeeoc$          , ch(02),~
                at (06,55), fac(hex(84)), EeocDesc$             , ch(25),~
                                     ~
                at (07,02), "Pay Frequency     :"               ,        ~
                at (07,22), fac(lfac$(1%)), e_pay_freq$         , ch(01),~
                                                                         ~
                at (07,30), "Veteran Status    :"               ,        ~
                at (07,50), fac(lfac$(1%)), e_military$         , ch(04),~
                at (07,55), fac(hex(84)), MilDesc$              , ch(23),~
                                         ~
                at (08,02), "Classification    :"               ,        ~
                at (08,22), fac(lfac$(1%)), e_class$            , ch(02),~
                                                                         ~
                at (08,30), "EEOC Group Code   :"               ,        ~
                at (08,50), fac(lfac$(1%)), e_grpeeoc$          , ch(03),~
                at (08,55), fac(hex(84)), EeocGDesc$             , ch(25),~
                                                                         ~
                at (09,02), "Pay Grade         :"               ,        ~
                at (09,22), fac(lfac$(2%)), e_pay_grade$        , ch(02),~
                                                                         ~
                at (10,02), "Pay Rate (Base)   :"               ,        ~
                at (10,22), fac(lfac$(2%)), pay_rate$           , ch(08),~
                                                                         ~
                at (11,02), "Shift Differential:"               ,        ~
                at (11,22), fac(lfac$(2%)), shift_diff$         , ch(08),~
                                                                         ~
                at (13,02), "State   W/H Status:"               ,        ~
                at (13,22), fac(lfac$(1%)), e_wh_st_stat$       , ch(01),~
                at (13,27), "Exemptions:"                       ,        ~
                at (13,39), fac(lfac$(1%)), e_wh_st_exempt$     , ch(01),~
                at (13,44), "Additional:"                       ,        ~
                at (13,56), fac(lfac$(1%)), st_add$             , ch(08),~
                                                                         ~
                at (14,02), "Federal W/H Status:"               ,        ~
                at (14,22), fac(lfac$(1%)), e_wh_fd_stat$       , ch(01),~
                at (14,27), "Exemptions:"                       ,        ~
                at (14,39), fac(lfac$(1%)), e_wh_fd_exempt$     , ch(01),~
                at (14,44), "Additional:"                       ,        ~
                at (14,56), fac(lfac$(1%)), fd_add$             , ch(08),~
                                                                         ~
                at (16,02), "US Citizen        :"               ,        ~
                at (16,22), fac(lfac$(1%)), e_i9_rev$           , ch(01),~
                at (16,27), "EAD Expires on:"                       ,        ~
                at (16,45), fac(lfac$(1%)), e_i9_rev_dte$       , ch(08),~
                                                                         ~
/* (EWD001) */  at (17,02), "Manager Name      :"               ,        ~
                at (17,22), fac(lfac$(1%)), e_job_title$        , ch(25),~
                                                                         ~
/*              at (18,02), "Cross Trained In  :"               ,     */ ~
                at (18,02), "Supervisor Name   :"               ,        ~
                at (18,22), fac(lfac$(1%)), e_cross_train$(1%)  , ch(25),~
                at (18,52), fac(lfac$(1%)), e_cross_train$(2%)  , ch(25),~
                                                                         ~
                at (19,22), fac(lfac$(1%)), e_cross_train$(3%)  , ch(25),~
                at (19,52), fac(lfac$(1%)), e_cross_train$(4%)  , ch(25),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15% then L40960
                     call "PRNTSCRN"

                     goto accept_screen
L40960:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pf1
        if edit% = 2% then L41150     /*  Input Mode             */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Return      "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

            if fieldnr% = 1% then L41130
              str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L41130:     return

L41150: if fieldnr% > 0% then L41250  /*  Edit Mode - Select Field */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                      (14)Update Data "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Return      "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
                                     /*  Edit Mode - Enabled    */
L41250:     pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                                      "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            if security_hr% <> 1% then return
            gosub field_edits
            return

        field_edits

        REM Employee No.                        E_NO$
   REM      if security_hr% = 2% then edit_pay_rate
   REM      if edit% = 2% then L50180
   REM      gosub dataload
   REM  return


        REM Gender                              E_GENDER$
            if e_gender$ = " "      then L50240
            if e_gender$ = "F"      then L50240
            if e_gender$ = "M"      then L50240
            errormsg$    = "Invalid GENDER  [M or F]"
            return

L50240: REM Race                                E_RACE$
        /*
            if e_race$ = " "        then L50330
            if e_race$ = "W"        then L50330
            if e_race$ = "B"        then L50330
            if e_race$ = "H"        then L50330
            if e_race$ = "A"        then L50330
            if e_race$ = "O"        then L50330
            if e_race$ = "I"        then L50330
            if e_race$ = "N"        then L50330
            if e_race$ = "P"        then L50330
            if e_race$ = "2"        then L50330
            errormsg$  = "Invalid RACE  [W B H A O I N P 2]"
        */
        /* AWD009 start */
            if e_race$ <> " " then goto L50250
               readkey$ = all(hex(00))
               readkey$ = "EEOC RACE"
               descr$ =hex(06)&"Select a Valid EEOC Race Code (Required)"
               call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2))
               e_race$ = str(readkey$,10%,4%)
L50250:     readkey$ = all(hex(00))
            readkey$ = "EEOC RACE" & e_race$
            call "DESCRIBE" (#2, readkey$, RaceDesc$, 0%, f1%(2))
            if f1%(2) = 0 then goto L50260
        goto L50330
L50260:   errormsg$ = "(Error) - Invalid Veteran Code? (Required)"
          init(" ") e_race$, RaceDesc$
        /* AWD009 end   */
            return

L50330: REM EEOC Job Type
            if e_jobeeoc$ <> " " then goto L50335
               readkey$ = all(hex(00))
               readkey$ = "APCEEOCJT"
               descr$ =hex(06)&"Select a Valid EEOC Code (Required)"
               call "PLOWCODE" (#2, readkey$, descr$, 9%, .25, f1%(2))
               e_jobeeoc$ = str(readkey$,10%,2%)
L50335:     readkey$ = all(hex(00))
            convert e_jobeeoc$ to e_jobeeoc%, data goto L50338
            convert e_jobeeoc% to e_jobeeoc$, pic (00)
            readkey$ = "APCEEOCJT" & e_jobeeoc$
            call "DESCRIBE" (#2, readkey$, EeocDesc$, 0%, f1%(2))
            if f1%(2) = 0 then goto L50338
        goto L50360
L50338:   errormsg$ = "(Error) - Invalid EEOC Code? (Required)"
          init(" ") e_jobeeoc$, EeocDesc$
        return

L50360: REM EEOC Group Code
            if e_grpeeoc$ <> " " then goto L50365
               readkey$ = all(hex(00))
               readkey$ = "APCEEOCGR"
               descr$ =hex(06)&"Select a Valid EEOC Code (Required)"
               call "PLOWCODE" (#2, readkey$, descr$, 9%, .25, f1%(2))
               e_grpeeoc$ = str(readkey$,10%,3%)
L50365:     readkey$ = all(hex(00))
            readkey$ = "APCEEOCGR" & e_grpeeoc$
            call "DESCRIBE" (#2, readkey$, EeocGDesc$, 0%, f1%(2))
            if f1%(2) = 0 then goto L50368
        goto L50370
L50368:   errormsg$ = "(Error) - Invalid EEOC Code? (Required)"
          init(" ") e_grpeeoc$, EeocGDesc$
        return

L50370: REM Veteran Status
            if e_military$ <> " " then goto L50374
               readkey$ = all(hex(00))
               readkey$ = "APCVETSTA"
               descr$ =hex(06)&"Select a Valid Veteran Code (Required)"
               call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2))
               e_military$ = str(readkey$,10%,4%)
L50374:     readkey$ = all(hex(00))
            readkey$ = "APCVETSTA" & e_military$
            call "DESCRIBE" (#2, readkey$, MilDesc$, 0%, f1%(2))
            if f1%(2) = 0 then goto L50375
        goto L50380
L50375:   errormsg$ = "(Error) - Invalid Veteran Code? (Required)"
          init(" ") e_military$, MilDesc$
        return

L50380: REM Pay Status                          E_PAY_STAT$
            if e_pay_stat$ = " "    then L50410
            if e_pay_stat$ = "A"    then L50410
            if e_pay_stat$ = "E"    then L50410
            if e_pay_stat$ = "H"    then L50410
            if e_pay_stat$ = "N"    then L50410
            errormsg$      = "Invalid PAY STATUS  [A, E, H, N]"


L50410: REM Pay Frequency                       E_PAY_FREQ$
            if e_pay_freq$ = " "    then L50470
            if e_pay_freq$ = "W"    then L50470
            if e_pay_freq$ = "S"    then L50470
            errormsg$      = "Invalid PAY FREQUENCY [W or S]"


L50470: REM Classification                      E_CLASS$
            if e_class$ = " "       then L50570
            if e_class$ = "FT"      then L50570
            if e_class$ = "PT"      then L50570
            if e_class$ = "TP"      then L50570
            if e_class$ = "PR"      then L50570
            if e_class$ = "CS"      then L50570
            if e_class$ = "TA"      then L50570
            errormsg$   = "Invalid CLASSIFICATION  [FT, PT, TP, PR, CS, TA]"


L50570: REM Pay Grade                           E_PAY_GRADE$
REM         if security_hr% = 2% then return
            if security_hr% <> 1% then return
            if e_pay_grade$ = " "   then edit_pay_rate
            if e_pay_grade$ < "03"  then L50600
            if e_pay_grade$ < "15"  then edit_pay_rate
L50600:     errormsg$       = "Invalid PAY GRADE  [03 - 14]"
            return

        REM Pay Rate                            E_PAY_RATE
        edit_pay_rate
            if security_hr% <> 1% then return
            convert pay_rate$ to pay_rate,  data goto L50700
            if pay_rate < 0 or pay_rate > 999.99 then L50700
            convert pay_rate  to pay_rate$, pic (####0.00)
            goto edit_shift_diff

L50700:     errormsg$ = "Invalid PAY RATE  [Number: 0 to 999.99]"
            return

        REM Shift Differential                  E_SHIFT_DIFF
        edit_shift_diff
REM         if security_hr% = 2% then return
            if security_hr% <> 1% then return
            convert shift_diff$ to shift_diff,  data goto L50800
            if shift_diff < 0 or shift_diff > 999.99 then L50800
            convert shift_diff  to shift_diff$, pic (####0.00)
            if security_hr% = 2% then return
            goto edit_wh_st_stat
L50800:     errormsg$ = "Invalid SHIFT DIFFERENTIAL  [Number: 0 to 999.99]"
            return

        REM State W/H Status                    E_WH_ST_STAT$
        edit_wh_st_stat
            if security_hr% <> 1% then return
            if e_wh_st_stat$ = " "  then L50900
            if e_wh_st_stat$ = "M"  then L50900
            if e_wh_st_stat$ = "S"  then L50900
            errormsg$        = "Invalid STATE W/H STATUS  [M or S]"
            return

L50900: REM State W/H Exemptions                E_WH_ST_EXEMPT$
            if security_hr% <> 1% then return
            if e_wh_st_exempt$ = " " then edit_wh_st_add
            convert e_wh_st_exempt$ to e_wh_st_exempt%, data goto L50960
            if e_wh_st_exempt% < 0 or e_wh_st_exempt% > 9    then L50960
            convert e_wh_st_exempt% to e_wh_st_exempt$, pic (#)
            goto edit_wh_st_add
L50960:     errormsg$ = "Invalid State EXEMPTIONS  [Number: 0 to 9]"
            return

        REM State W/H Additional Amount         E_WH_ST_ADD
        edit_wh_st_add
            if security_hr% <> 1% then return
            convert st_add$ to st_add,  data  goto L51060
            if st_add < 0 or st_add > 9999.99 then L51060
            convert st_add  to st_add$, pic (####0.00)
            goto edit_wh_fd_stat
L51060:     errormsg$ = "Invalid ADDITIONAL STATE W/H  [Number: 0 to 999.99]"
            return

        REM Federal W/H Status                  E_WH_FD_STAT$
        edit_wh_fd_stat
            if security_hr% <> 1% then return
            if e_wh_fd_stat$ = " "  then L51160
            if e_wh_fd_stat$ = "M"  then L51160
            if e_wh_fd_stat$ = "S"  then L51160
            errormsg$        = "Invalid FEDERAL W/H STATUS  [M or S]"
        return

L51160: REM Federal W/H Exemptions              E_WH_FD_EXEMPT$
            if security_hr% <> 1% then return
            if e_wh_fd_exempt$ = " " then edit_wh_fd_add
            convert e_wh_fd_exempt$ to e_wh_fd_exempt%, data goto L51220
            if e_wh_fd_exempt% < 1 or e_wh_fd_exempt% > 9    then L51220
            convert e_wh_fd_exempt% to e_wh_fd_exempt$, pic (#)
            goto edit_wh_fd_add
L51220:     errormsg$ = "Invalid FEDERAL EXEMPTIONS  [Number: 0 to 9]"
        return

        REM Federal W/H Additional Amount       E_WH_FD_ADD
        edit_wh_fd_add
            if security_hr% <> 1% then return
            convert fd_add$ to fd_add,  data  goto L51320
            if fd_add < 0 or fd_add > 9999.99 then L51320
            convert fd_add  to fd_add$, pic (####0.00)
            goto edit_i9_rev
L51320:     errormsg$ = "Invalid ADDITIONAL FEDERAL W/H  [Number 0 - 9999.99]"
        return

        REM I-9 Reverification                  E_I9_REV$
        edit_i9_rev
            if security_hr% <> 1% then return
            if e_i9_rev$ = " "       then L51420
            if e_i9_rev$ = "Y"       then L51420
            if e_i9_rev$ = "N"       then L51420
            errormsg$    = "Invalid I-9 REVERIFICATION  [Y or N]"
        return

L51420: REM I-9 Reverification Date             E_I9_REV_DTE$
            if security_hr% <> 1% then return
            if e_i9_rev_dte$ = " "   then L51480
                call "DATEOK" (e_i9_rev_dte$, date%, errormsg$)
                if date% <> 0% then L51480
                    errormsg$ = "Invalid I-9 VERIFICATION DATE  [Date]"
L51480: return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************

                                                     /* (AWD008)   - Beg */
        load_security_hr
            init(" ") readkey$, access$
            security_hr% = 0%
            str(readkey$,1%,9%)   = "EMP HR   "
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L51500, access$, eod goto no_hr
L51500:       FMT POS(25), CH(30)
            security_hr% = 1%
        no_hr
            str(readkey$,1%,9%)   = "EMP GEN  "
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, using L51500, access$, eod goto no_hr_2
            security_hr% = 2%
        no_hr_2
        return

                                                            /* (AWD008) */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub
            call "SHOSTAT" ("One Moment Please")

        end


