        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT   CCC    III   N   N  PPPP           *~
            *    J    B   B    T    C   C    I    NN  N  P   P          *~
            *    J    BBBB     T    C        I    N N N  PPPP           *~
            *  J J    B   B    T    C   C    I    N  NN  P              *~
            *   J     BBBB     T     CCC    III   N   N  P              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTCINP  - Manage labor time cards, including Job labor   *~
            *            value amounts and job movement details.        *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/19/86 ! ORIGINAL                                 ! HES *~
            * 08/18/86 ! Minor fixes                              ! MJB *~
            * 10/28/86 ! JBSTATUS format change/ PLANRTE logic    ! HES *~
            * 04/28/87 ! Get Labor Class Overhead % from STCLABOR ! JIM *~
            * 06/16/87 ! Standard Costing Changes                 ! ERN *~
            * 08/04/87 ! fixed ROLL_DETAIL sub, upped MAX 40 to 99! HES *~
            * 05/06/88 ! Now calls subroutine to purge WCOUTs     ! HES *~
            * 06/29/88 ! Removed entry of W/C Movement AND NOW    ! RJM *~
            *          !   calls JBACTSUB for movement entry.     !     *~
            *          !   Screen change; Removed movement fields !     *~
            * 06/30/88 ! Fixed Labor Class Default for Screen     ! RJM *~
            * 10/10/88 ! MAJOR UPGRADE.  File format changes, new ! ERN *~
            *          !  'philosophy'.  Lots of cleanup.         !     *~
            * 03/28/89 ! Added line 14265, fixes bug in delete    ! RJM *~
            * 09/18/89 ! Suppressed input of ovrhd bucket name    ! JDH *~
            *          !  when there is no overhead value.        !     *~
            * 11/27/91 ! PRR 12072 Added checks to make sure that ! SID *~
            *          !   the First Time Card Date from screen 1 !     *~
            *          !   is >= the Time Card Date from screen 2 !     *~
            * 04/10/92 ! PRR 12171.  Removed 'RN' as auto SSA.    ! JDH *~
            *          ! PRR 12168.  Fixed Restricted Display.    !     *~
            * 04/28/92 ! Minor Mod for Payroll and SFC seperation ! JBK *~
            * 06/21/93 ! PRR 11730.  Fixed problem w/Hour checking! JDH *~
            * 11/09/94 ! Refixed above problem w/Hour checking    ! RJH *~
            * 02/08/95 ! PRR 13347 - CDA JT 02/04 transactions may! RJH *~
            *          !   now be viewed w/o changing CDA status. !     *~
            * 08/24/95 ! REMOVE PRR 13347 - the implimentation of ! RJH *~
            *          !   above PRR had undesirable effects.     !     *~
            * 08/14/97 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buckets$(12)10,              /* Cost Bucket IDs            */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            date$8,                      /* Date For Screen Display    */~
            day1$1,                      /* Line Item sort flag        */~
            days$(7)9,                   /* Day of week literals       */~
            deflt_class$4,               /* Default Labor Class        */~
            dept_class$4,                /* Department Labor Class     */~
            dept_oh_acct$9,              /*            Overhead Acct   */~
            dept_use_rate$1,             /*            Use Dept Rate?  */~
            emp$12,                      /* Employee Code              */~
            emp_class$4,                 /*          Labor Class       */~
            emp_dept$4,                  /*          Department        */~
            emp_name$32,                 /*          Name              */~
            emp_shift$1,                 /*          Shift             */~
            emp_stat$1,                  /*          Payroll Status    */~
            emp_oh_acct$9,               /*          Overhead Account  */~
            errormsg$79,                 /* Error Message              */~
            h$(490)1,                    /* Holiday Markers            */~
            header$(5)79,                /* Header for Screen          */~
            hfac$(2)1,                   /* Header Display Facs        */~
            i$2,                         /* Edited line number         */~
            i$(24)80,                    /* Screen Image               */~
            inuser$3,                    /* Last Mod By                */~
            lastemp$12,                  /* Last Employee Code         */~
            lbr_acct$9,                  /* Labor Acct (WIP offset)    */~
                                         /* Labor Distribution...      */~
            ld_actv$(99)4,               /*   Route Activity Code      */~
            ld_base(99), ld_base$8,      /*   Base Rate (memo only)    */~
            ld_cda%(99),                 /*   CDA Transaction Number   */~
            ld_cda_prompt$16,            /*   Screen Prompt            */~
            ld_cda$7,                    /*   Screen Display           */~
            ld_class$(99)4,              /*   Labor Class              */~
            ld_dept$(99)4,               /*   Department Code          */~
            ld_etype$(99)12,             /*   Earnings Type            */~
            ld_ext(99), ld_ext$8,        /*   Line Ext (Hrs * Rate)    */~
            ld_hrs$(99)5, ld_hrs(99),    /*   Hours                    */~
            ld_job$(99)8,                /*   Job Number               */~
            ld_job_descr$32,             /*   Job or Task Descr        */~
            ld_lbkt$(99)10,              /*   Direct Labor Bucket      */~
            ld_lbr_acct$(99)9,           /*   Labor Account            */~
            ld_oh(99), ld_oh$8,          /*   Overhead Dollars         */~
            ld_obkt$(99)10,              /*   Overhead Bucket          */~
            ld_oh_acct$(99)9,            /*   Overhead Account         */~
            ld_ot$(99)1,                 /*   Overtime?                */~
            ld_rate(99), ld_rate$8,      /*   Earnings Rate            */~
            ld_shift$(99)1,              /*   Shift                    */~
            ld_start$(99)5,              /*   Start Time               */~
            ld_stop$(99)5,               /*   Stop  Time               */~
            ld_task$(99)6,               /*   Time Card Task           */~
            ld_type$(99)8,               /*   Line Item Type           */~
            ld_use_dept$(99)1,           /*   Use Dept Rates?          */~
            ld_wc$(99)4,                 /*   Work Center              */~
                                                                         ~
            lfac$(22)1,                  /* Field Attribute Characters */~
            lfac1$(20)1,                 /* Field Attribute Characters */~
            line$(400)4,                 /* Line Number For Reference  */~
            line2$79,                    /* Second screen line         */~
            message$79,                  /* Input Message              */~
            mfac$(400)1,                 /* Field Attribute Characters */~
            nrs(8),                      /* Numeric Array              */~
            oh_acct$9,                   /* Overhead Acct              */~
            outdate$8,                   /* JBACTSUB- Date             */~
            outemp$12,                   /*           Employee         */~
            outjob$8,                    /*           Job Number       */~
            outtime$8,                   /*           Time             */~
            p%(1),                       /* Receiver for SEARCH        */~
            part$25,                     /* Job Part Number            */~
            packed$245,                  /* Work Variable              */~
            pc_key$25,                   /* PlowCode- key              */~
            pc_hdr$(2)79,                /*           column headers   */~
            pc_inex(1), pc_inex$(1)1,    /*           include/exclude  */~
            pc_map(4),                   /*           I/O mapping      */~
            pfdescr$(3)79,               /* Description Of PFkeys      */~
            pf$(3)79,                    /* Same as above              */~
            pfkeys$32,                   /* PF Keys enabled string     */~
            put_cursor_here$1,           /* A real dummy variable      */~
            rate_prompt$12,              /* Screen Rate Prompt         */~
            readkey$60,                  /* Work Variable              */~
            set$8, setid$4,              /* Cost Set IDs               */~
            sfcdate$8,                   /* SFC Date                   */~
            start$10,                    /* Time Cards On Or After     */~
            startdate$10,                /* Time Cards On Or After     */~
            stats$(5)9,                  /* List Of Possible Stats     */~
            sum$(3)8, sumfac$1,          /* Rate Display Prompts       */~
            sum_hdr$( 4)8,               /* Summary Headers            */~
            sum_hrs$(12)5,               /* Summary of Hours           */~
            sum_val$( 4)8,               /* Summary of Values          */~
            sw_actv$1,                   /* Switchs- Enable Activity?  */~
            sw_chk_hrs$1,                /*          Check Hours?      */~
            sw_etype$1,                  /*          Enable Earn Type? */~
            sw_mod_rate$1,               /*          Rates Modifiable? */~
            sw_mod_ratef$1,              /*                            */~
            sw_reset_rate$1,             /*          Auto Reset Rates? */~
            sw_wc$1,                     /*          Enable WC?        */~
            sw_user$(15)3,               /*          Rates for only... */~
                                         /* Time Card Variables...     */~
            tc_by$3,                     /*   Entered By               */~
            tc_date$10, tc_dateu$10,     /*   Date                     */~
            tc_dept$4,                   /*   Default Department Code  */~
            tc_hol$1, tc_hol_old$1,      /*   Holiday Flag             */~
            tc_in$5,                     /*   Clock-in Time            */~
            tc_nrs(8),                   /*   Line Summary Numbers     */~
            tc_out$5,                    /*   Clock-out Time           */~
            tc_shift$1,                  /*   Default Shift            */~
            tc_smry$(400)80,             /*   Summary Screen Array     */~
            tc_status$1,                 /*   Status (C/N/P/$)         */~
                                                                         ~
            temp$6,                      /* 6-character temp variable  */~
            temp1$10,                    /*10,character temp variable  */~
            temp2$10,                    /*10,character temp variable  */~
            temp5$5,                     /* 5-character temp variable  */~
            tfac$(20)1,                  /* Field Attribute Characters */~
            time0$5,                     /* Work Variable              */~
            time_in$5,                   /* Time Work Variable         */~
            title$79,                    /* Title For Screen           */~
            xtitle$18,                   /* Title For Screen           */~
            userid$3                     /* You Know                   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! JBMASTR2 ! Level 2 Job Master File                  *~
            * # 3 ! HNYMASTR ! Inventory Master (Descriptions)          *~
            * # 4 ! WCMASTR  ! Work Center Master File                  *~
            * # 5 ! PERMASTR ! Personnel Master File                    *~
            * # 6 ! GENCODES ! General System Codes File                *~
            * # 7 ! RTEMASTR ! Production Routings File                 *~
            * # 8 ! JBCROSS2 ! Additional Job Info                      *~
            * # 9 ! JBTCBUFF ! Time Card Master File (TIF)              *~
            * #10 ! JBTCBUF2 ! Time Card Details File (TIF)             *~
            * #11 ! USERINFO ! User Information File                    *~
            * #12 ! SYSFILE2 ! System Control File                      *~
            * #13 ! EMPMASTR ! Employee File Master Records.            *~
            * #14 ! JBTCMSTR ! Time Card Master File                    *~
            * #15 ! JBTCLINE ! Time Card details File                   *~
            * #16 ! EMPEARN1 ! Employee Earnings File                   *~
            * #17 ! JBTCCDES ! Time Card Task Codes                     *~
            * #18 ! PRLDEPTF ! Payroll Department Master File           *~
            * #19 ! JBSTATUS ! Job Status Tracking File                 *~
            * #20 ! CDAAUDJT ! CDA Time Card Audit File                 *~
            * #22 ! BOMMASTR ! Bill Of Materials Relationship File      *~
            * #23 ! ENGMASTR ! Engineering Master File                  *~
            * #30 ! STCnnnnL ! Std Costing Labor Standards (STCLABOR)   *~
            * #31 ! DUMMY    ! For PlowCode                             *~
            *-----+----------+------------------------------------------*~
            * where 'nnnn' is a unique cost set identifier generated by *~
            * the call to 'STCFOPEN'.                                   *~
            *************************************************************

            select #1,  "JBMASTR2",                                      ~
                        varc, indexed, recsize = 1300,                   ~
                        keypos = 1, keylen = 8

            select #3,  "HNYMASTR",                                      ~
                        varc, indexed, recsize = 900,                    ~
                        keypos = 1, keylen = 25

            select #4,  "WCMASTR",                                       ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2, keylen = 5,                          ~
                        alt key 1, keypos =  1 , keylen = 6

            select #5,  "PERMASTR",                                      ~
                        varc, indexed, recsize = 950,                    ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #6,  "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =  1, keylen = 24

            select #7,  "RTEMASTR",                                      ~
                        varc, indexed, recsize = 400,                    ~
                        keypos =  5, keylen =  31,                       ~
                        alt key  1, keypos = 1, keylen = 35

            select #8,  "JBCROSS2",                                      ~
                        varc, indexed, recsize = 94,                     ~
                        keypos =29, keylen = 19,                         ~
                        alternate key 1, keypos = 1 , keylen = 47,       ~
                                  key 2, keypos = 48, keylen = 47

            select #9,  "JBTCBUFF",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 8, keylen = 18,                         ~
                        alt key 1, keypos = 2, keylen = 18,              ~
                            key 2, keypos = 1, keylen = 19

            select #10, "JBTCBUF2",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 25

            select #11, "USERINFO",                                      ~
                       varc, indexed, recsize = 150,                     ~
                       keypos = 1, keylen = 3

            select #12, "SYSFILE2",                                      ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 20

            select #13, "EMPMASTR",                                      ~
                       varc, indexed, recsize = 136,                     ~
                       keypos = 1, keylen = 12,                          ~
                       alt key  1, keypos = 70, keylen =  1, dup

            select #14, "JBTCMSTR",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 8, keylen = 18,                         ~
                        alt key 1, keypos = 2, keylen = 18,              ~
                            key 2, keypos = 1, keylen = 19

            select #15, "JBTCLINE",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 25

            select #16, "EMPEARN1",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 15,                         ~
                        alt key 1, keypos = 16, keylen = 28

            select #17, "JBTCCDES",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 6

            select #18, "PRLDEPTF",                                      ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 4

            select #19, "JBSTATUS",                                      ~
                        varc, indexed, recsize = 200,                    ~
                        keypos = 1, keylen = 12,                         ~
                        alt key 1, keypos =  21, keylen = 44,            ~
                            key 2, keypos =  29, keylen = 36

            select #20, "CDAAUDJT",                                      ~
                        varc, indexed, recsize = 256,                    ~
                        keypos =   17, keylen =  19,                     ~
                        alt key  1, keypos =    5, keylen =  31,         ~
                            key  2, keypos =    1, keylen =  35

            select #22, "BOMMASTR",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos =  26, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 56

            select #23, "ENGMASTR" ,                                     ~
                        varc, indexed, recsize = 2015,                   ~
                        keypos = 1, keylen = 29


            select #30, "STCLABOR", varc, indexed, recsize = 323,        ~
                        keypos = 1, keylen = 4

            select #31, "LARRY", varc, consec, recsize = 4

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1 ,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#3 ,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#4 ,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#5 ,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#6 ,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#7 ,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#8 ,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#9 ,   0%, 0%, 100%, " ")
            call "OPENCHCK" (#10,   0%, 0%, 200%, " ")
            call "OPENCHCK" (#11,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#12,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#13,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#14,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#15,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#16,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#17,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#18,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#19,   0%, 0%, 200%, " ")
            call "OPENCHCK" (#20, cda%, 0%,   0%, " ")
            call "OPENCHCK" (#22,   0%, 0%,   0%, " ")
            call "OPENCHCK" (#23,   0%, 0%,   0%, " ")

            stc% = 0% : errormsg$ = " "
            call "STCFOPEN" /* Open STCLABOR in 'Shared' mode          */~
                (" ", "xxxSxx", #12, errormsg$, #30,#30,#30,#30,#30,#30)
            if errormsg$ <> " " then stc% = 1% /* Can't use STCLABOR */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

*        Setup screen date, get User ID, and test posting date...
            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)
            str(line2$,62) = " JBTCINP: " & str(cms2v$,,8)
            call "READ100" (#11, userid$, f1%)
            if f1% = 1% then L09075
                errormsg$ = "User ID not on file."
                goto abort_program
L09075:     get #11 using L09080, sfcdate$
L09080:         FMT POS(34), CH(6)
            call "WHICHPER" (#12, sfcdate$, thisperiod%)
            if thisperiod% <> 0% then L09115
                errormsg$ = "Invalid Posting Date."
                goto abort_program

*        Get default direct labor bucket...
L09115:     buckets% = 2%
            call "STCSETID" (buckets%, #12, set$, setid$, buckets$())
            if buckets% > 0% then L09160
                errormsg$ = "No Current Cost Set on file"
             abort_program
                call "ASKUSER" (2%, "ABORT CONDITION",                   ~
                          "Time Card entry Cannot continue because:",    ~
                          errormsg$, "Press RETURN to return to menu...")
                goto L65000
L09160:     get #12 using L09165, dflt_labr%, dflt_oh%
L09165:         FMT POS(443), 2*BI(1)

*        Set up some descriptions...
            call "DATEFMT" (sfcdate$)
            days$(1) = "Sunday"   : days$(2) = "Monday"
            days$(3) = "Tuesday"  : days$(4) = "Wednesday"
            days$(5) = "Thursday" : days$(6) = "Friday"
            days$(7) = "Saturday"

            stats$(1) = "Unknown"
            stats$(2) = "Unposted"
            stats$(3) = "Posted"
            stats$(4) = "Payroll"
            stats$(5) = "CDA"

            header$(1) = "       Card     Day     Hol       Clock Clock"&~
                         " Direct Indirect Non-P/R            "
            header$(2) = "Ref#" & hex(ac) & "  Date  " & hex(ac) &       ~
                         "of Week  " & hex(ac) & " ? " & hex(ac)&        ~
                         "Shift" & hex(ac) & " In  " & hex(ac) &         ~
                         " Out " & hex(ac) & " Hours" & hex(ac) &        ~
                         "  Hours " & hex(ac) & " Hours " & hex(ac) &    ~
                         "Status"
            header$(4) = "Line" & hex(ac) & "Start" & hex(8cac)         &~
                         "Stop " & hex(8cac)                            &~
                         "Hours" & hex(ac) & "  Type  "                 &~
                         hex(ac) & "OT?" & hex(8cac) & "Job Nmbr"       &~
                         hex(ac) & " WC " & hex(ac) & "Actv" & hex(8cac)&~
                         " Task " & hex(8cac) & "Dept" & hex(ac)        &~
                         "Shift" & hex(8c)
            header$(5) = "Labor Distribution  (Line xx)"

            sum_hdr$(1) = "Reglr"
            sum_hdr$(2) = " OT  "
            sum_hdr$(3) = "Total"
            sum_hdr$(4) = "  Value "

*        Get Holiday Schedule, If Possible...
            init("N") h$()
            call "READ100" (#12, "HOLIDAY SCHEDULE", f1%)
            if f1% = 0% then L09405
                get #12, using L09375, packed$
L09375:              FMT XX(20), CH(245)
                hexunpack packed$ to h$()
                for i% = 1% to 490%
                     if h$(i%) <> "0" then h$(i%) = "Y" else h$(i%) = "N"
                next i%

L09405:     for i% = 1% to 400%
                convert i% to line$(i%), pic(###)
                str(line$(i%),4) = ")"
            next i%

            xtitle$ = " JBTCINP: " & str(cms2v$,1,8)

*        See if User is a Module Administrator or not...
            call "CMSMACHK" ("SFC", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%

*        Load Program Switchs Record...
            call "READ100" (#12, "SWITCHS.JBTCINP", f1%)
            if f1% = 1% then L09500
                put #12 using L09490, "SWITCHS.JBTCINP", " ", "YNNYYY",    ~
                                    " ", " "
L09490:             FMT CH(20), CH(45), CH(6), CH(229), CH(200)
                write #12
L09500:     get #12 using L09510, sw_user$(), sw_mod_rate$, sw_reset_rate$,~
                                sw_chk_hrs$, sw_etype$, sw_wc$, sw_actv$
L09510:         FMT POS(21), 15*CH(3), 6*CH(1)
            sw_mod_ratef$ = sw_mod_rate$
            gosub test_switchs_for_user

*        Check to see if CMS payroll Installed
            call "PRLEXTSB" ("   ", prl_on%)     /* 1=PRL On, 2=SFC On */

        REM *************************************************************~
            *         D I S P L A Y   A L L   C A R D S                 *~
            * --------------------------------------------------------- *~
            * PROGRAM FOCAL POINT.                                      *~
            *************************************************************

        main
            init(" ") emp$, emp_name$, tc_smry$()
            init(" ") tc_date$, tc_dateu$, tc_status$, tc_in$, tc_out$,  ~
                      tc_shift$, tc_hol$, tc_dept$, sum_hrs$(),          ~
                      sum_val$()
            mfac$() = all(hex(9c))
            maxcards%, insert%, emp_base = 0

*        Get First Card Date and Employee Code to Display...
            for fieldnr% = 1% to 2%
                gosub'050(fieldnr%)
                     if enabled% = 0% then L10145
L10090:         gosub'100(fieldnr%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then       L10125
                        if fieldnr% <> 2% then L10125
                        fieldnr% = 1%
L10115:                 gosub'050(fieldnr%)
                        goto L10090
L10125:              if keyhit%  = 10% then gosub see_cs
                     if keyhit%  = 16% then L65000
                     if keyhit%  = 17% then gosub mod_switchs
                     if keyhit%  = 17% then       L10115
                     if keyhit%  = 32% then L65000
                     if keyhit% <>  0% then       L10090
L10145:         gosub'150(fieldnr%)
                     if errormsg$ <> " " then L10090
                next fieldnr%
                if maxcards% = 0% then inputmode

        all_cards_summary
           errormsg$ = " "  /* Time Cards Summary Screen */
           message$  =                                                   ~
                    "To MODIFY a Time Card, Tab to Line and Press RETURN"
           insert% = 0%

L10200:     gosub'100(0%)
              if keyhit% =   1% then gosub startover
              if keyhit% =   2% then base% = 0%
              if keyhit% =   3% then base% = maxcards% - 9%
              if keyhit% =   4% then base% = base% - 9%
              if keyhit% =   5% then base% = base% + 9%
              if keyhit% =   6% then base% = base% - 1%
              if keyhit% =   7% then base% = base% + 1%
                             base% = max(0%, min(base%,maxcards%-11%))
              if keyhit% =  11% then inputmode
              if keyhit% =  16% then main
              if keyhit% =  32% then L65000
              if keyhit% <>  0% then L10200

              fieldnr% = cursor%(1) - 8%
              if fieldnr% < 0% or fieldnr% > 11% then L10200
              card%    = min(base% + fieldnr%, maxcards%)
              if card% = 0% then L10200
              fieldnr% = card% - base%
              gosub L29000          /* Clear Variables for Load */
              tc_date$ = str(tc_smry$(card%),7,8)
              gosub'151(1%)        /* Test time card date      */
              goto all_cards_summary

        mod_switchs       /* Allow user to change program switchs      */
            save_fieldnr% = fieldnr%
L10330:     lastfieldnr% = 0%
L10335:     gosub'106(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub reset_switchs
                  if keyhit%  =  9% then       exit_switchs
                  if keyhit%  = 16% then       save_switchs
                  if keyhit% <>  0% then       L10335
L10360:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  7% then mod_switchs
            if fieldnr% = lastfieldnr% then L10330
            gosub'056(fieldnr%)         /* Set Input Message           */
L10380:     gosub'106(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit% <>  0% then L10380
            gosub'156(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L10380
                  lastfieldnr% = fieldnr%
            goto L10360

        reset_switchs     /* Set switches to what's stored on file     */
            call "READ100" (#12, "SWITCHS.JBTCINP", f1%)
            get #12 using L10435, sw_user$(), sw_mod_rate$,sw_reset_rate$,~
                               sw_chk_hrs$, sw_etype$, sw_wc$, sw_actv$
L10435:         FMT POS(21), 15*CH(3), 6*CH(1)
            sw_mod_ratef$ = sw_mod_rate$
            gosub test_switchs_for_user
            return

        save_switchs
            call "READ101" (#12, "SWITCHS.JBTCINP", f1%)
            put #12 using L10435, sw_user$(),sw_mod_ratef$,sw_reset_rate$,~
                               sw_chk_hrs$, sw_etype$, sw_wc$, sw_actv$
            sw_mod_rate$ = sw_mod_ratef$
            rewrite #12

        exit_switchs
            keyhit%  = 17%
            fieldnr% = save_fieldnr%
        test_switchs_for_user
            sw_display_rate% = 0%
            if str(sw_user$()) <> " " then L10535
                sw_display_rate% = 1%
                goto L10550
L10535:     search str(sw_user$()) = str(userid$) to p%() step 3
            if p%(1) <> 0% then sw_display_rate% = 1%                    ~
                           else sw_mod_rate$     = "N"
L10550:     return   /* Goes back to selection screen */


        see_cs
            pc_key$ = "C" & all(hex(00))
            pc_hdr$(1) = "  Employee Code   Time Card"
            pc_map(1) = 8.12  :  pc_map(2) = 1
            pc_map(3) = 2.061 :  pc_map(4) = 17
            call "PLOWCODE" (#9, pc_key$, " ", 9001%, 2.80, f1%,         ~
                             pc_hdr$(), 0, 0, pc_inex(), pc_inex$(),     ~
                             "D", "Y", #31, pc_map())
            return


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * HANDLES INPUT OF NEW TIME CARD                            *~
            *************************************************************

        inputmode:
            insert% = 1%

            gosub L29000  /* Clear Variables For Input */
            maxlines%    = 0%
            errormsg$    = " "
            if maxcards% = 400% then all_cards_summary
            card%        = maxcards% + 1%

L11150:     for fieldnr% = 1% to 5%
                gosub'051(fieldnr%, 1%)
                     if enabled% = 0% then L11340
L11180:         gosub'110(fieldnr%)
                     if keyhit% <> 16% or fieldnr% > 1% then L11220
                          fieldnr% = 99%
                          goto L11360
L11220:              if keyhit% = 32% and fieldnr% = 1% then L65000
                     if keyhit% =  1% then gosub start_card_over
                     if keyhit% <> 2% then L11270
                          gosub restart_card
                          goto L11150
L11270:              if keyhit% <> 4% then L11330
L11280:                   if fieldnr% < 2 then L11180
                          fieldnr% = fieldnr% - 1%
                          gosub'051(fieldnr%, 1%)
                          if enabled% <> 0% then L11180
                          goto L11280
L11330:              if keyhit% <> 0% then L11180
L11340:         gosub'151(fieldnr%)
                     if errormsg$ <> " " then L11180
L11360:         next fieldnr%

            if keyhit% <> 16% then add_time_card_lines
                gosub restart_card
                goto all_cards_summary

        add_time_card_lines
*        Get Line Item Data...
            if maxlines%  = 99% then editmode
            c%, maxlines% = maxlines% + 1%
            gosub enter_distribution
            if keyhit% <> 16% then goto add_time_card_lines
                gosub restart_distribution
                c%, maxlines% = maxlines% - 1%
                gosub sum_distr
                goto editmode

        enter_distribution
            init (" ")  part$, ld_job_descr$, errormsg$, ld_rate$,       ~
                        ld_ext$, ld_oh$, ld_base$, ld_cda_prompt$,       ~
                        ld_cda$
            for fieldnr% = 1% to 12%
                gosub'052(fieldnr%, 1%)
                     if enabled% = 0% then L11750
L11590:         gosub'120(fieldnr%)
                     if keyhit% = 16% and fieldnr% = 1% then return
                     if keyhit% =  1% then gosub start_card_over
                     if keyhit% <> 2% then L11650
                          gosub restart_distribution
                          goto enter_distribution
L11650:              if keyhit% <> 4% then L11710
L11660:                   if fieldnr% < 2% then L11590
                          fieldnr% = fieldnr% - 1%
                          gosub'052(fieldnr%, 2%)
                          if enabled% <> 0% then L11590
                          goto L11660
L11710:              if keyhit% <> 6% then L11740
                          gosub prevline
                          goto L11750
L11740:              if keyhit% <> 0% then L11590
L11750:         gosub'152(fieldnr%, 1%)
                     if errormsg$ <> " " then L11590
                next fieldnr%
                return

        restart_distribution:
            init(" ") ld_start$(c%), ld_stop$(c%), ld_job$(c%),          ~
                      ld_class$(c%), ld_etype$(c%), ld_task$(c%),        ~
                      ld_hrs$(c%), ld_rate$, ld_ext$, ld_oh$,            ~
                      ld_wc$(c%), ld_actv$(c%), ld_type$(c%),            ~
                      ld_use_dept$(c%), ld_ot$(c%), ld_shift$(c%),       ~
                      ld_dept$(c%), ld_lbr_acct$(c%), ld_oh_acct$(c%),   ~
                      ld_lbkt$(c%), ld_obkt$(c%), ld_base$
            ld_base(c%), ld_rate(c%), ld_ext(c%), ld_oh(c%), ld_hrs(c%), ~
            ld_oh_rate, ld_cda%(c%) = 0
            ld_lbkt%, ld_obkt% = 0%
            gosub sum_distr
            return

        restart_card:
            init(" ") tc_date$, tc_dateu$, tc_status$, tc_in$, tc_out$,  ~
                      tc_shift$, tc_hol$, tc_dept$, sum_hrs$(),          ~
                      sum_val$()
            init(" ") ld_job$(), ld_task$(), ld_job_descr$, ld_type$(),  ~
                      ld_start$(), ld_stop$(), ld_hrs$(), ld_rate$,      ~
                      ld_dept$(), ld_class$(), ld_etype$(), ld_ot$(),    ~
                      ld_shift$(), ld_lbkt$(), ld_obkt$(), ld_wc$(),     ~
                      ld_actv$(), inuser$, message$, ld_use_dept$(),     ~
                      ld_lbr_acct$(), ld_oh_acct$(), ld_base$
            item%, ld_lbkt%, ld_obkt% = 0%
            ld_oh_rate  = 0
            mat ld_hrs  = zer
            mat ld_base = zer
            mat ld_rate = zer
            mat ld_ext  = zer
            mat ld_oh   = zer
            mat ld_cda% = zer
            return

        prevline
            if c%=1% then return
                  on fieldnr% goto  L12260,      /* Job/Task Number     */~
                                    L12280,      /* Start/Stop, Hours   */~
                                    L12300,      /* Department          */~
                                    L12320,      /* Labor Class         */~
                                    L12330,      /* Earnings Type       */~
                                    L12340,      /* Overtime?           */~
                                    L12350,      /* Shift               */~
                                    L12360,      /* Hourly rate         */~
                                    L12370,      /* Direct Bucket       */~
                                    L12380,      /* Overhead Bucket     */~
                                    L12390,      /* Work Center         */~
                                    L12400       /* Activity Code       */
                  return

L12260:         ld_job$   (c%) = ld_job$   (c%-1)
                ld_task$  (c%) = ld_task$  (c%-1) : return
L12280:         ld_start$ (c%) = ld_start$ (c%-1)
                ld_stop$  (c%) = ld_stop$  (c%-1)
L12300:         ld_hrs$   (c%) = ld_hrs$   (c%-1) : return
                ld_dept$  (c%) = ld_dept$  (c%-1) : return
L12320:         ld_class$ (c%) = ld_class$ (c%-1) : return
L12330:         ld_etype$ (c%) = ld_etype$ (c%-1) : return
L12340:         ld_ot$    (c%) = ld_ot$    (c%-1) : return
L12350:         ld_shift$ (c%) = ld_shift$ (c%-1) : return
L12360:         ld_rate   (c%) = ld_rate   (c%-1)
                ld_base   (c%) = ld_base   (c%-1)
                call "CONVERT" (ld_rate(c%), 2.4, ld_rate$)
                call "CONVERT" (ld_base(c%), 2.2, ld_base$) : return
L12370:         ld_lbkt$  (c%) = ld_lbkt$  (c%-1) : return
L12380:         ld_obkt$  (c%) = ld_obkt$  (c%-1) : return
L12390:         ld_wc$    (c%) = ld_wc$    (c%-1) : return
L12400:         ld_actv$  (c%) = ld_actv$  (c%-1) : return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles Operation Of Edit Mode For Data Entry Screens.    *~
            *************************************************************

        editmode:
            errormsg$ = " "  /* Summary Screen This Card */
            message$  = "To Modify Time Card, Tab To Field OR Distribut"&~
                        "ion Line and Press RETURN."
L13100:     gosub'110(0%)
                if keyhit% =  1% then gosub start_card_over
                if keyhit% =  2% then item% = 0%
                if keyhit% =  3% then item% = maxlines%-8
                if keyhit% =  4% then item% = item% - 7%
                if keyhit% =  5% then item% = item% + 7%
                if keyhit% =  6% then item% = item% - 1%
                if keyhit% =  7% then item% = item% + 1%
                                      item%=max(0%,min(item%,maxlines%-8))
                if keyhit% =  0% then L13260
                if keyhit% = 10% then reset_line_rates
                if keyhit% = 11% then add_time_card_lines
                if keyhit% = 12% then L13410
                if keyhit% = 28% then delete_all_detail
                if keyhit% = 16% then datasave
                goto L13100

L13260:     fieldnr% = cursor%(1) - 3%
                if fieldnr% > 5% then L13400        /* Line Item        */
                if fieldnr% < 2% then L13100

*        Edit header...
            gosub'051(fieldnr%, 2%)
                if enabled% = 0% then L13360
L13330:     gosub'111(fieldnr%)
                if keyhit%  =  1% then gosub start_card_over
                if keyhit% <>  0% then L13330
L13360:     gosub'151(fieldnr%)
                if errormsg$  <> " " then L13330
                if cursor%(1) <> fieldnr% + 3% then L13260
                goto editmode

L13400
*        Edit one of the lines...
L13410:     fieldnr% = cursor%(1) - 11%
            if fieldnr% < 0% or fieldnr% > 8% then L13100
            c%    = min(item% + fieldnr%, maxlines%)
            if c% = 0% then L13100
            fieldnr% = c% - item%
            if keyhit% = 12% then delete_detail
            if keyhit% <> 0% then L13100

        edit_detail
            message$ = "To Modify Displayed Values (Distribution Only),"&~
                       " Tab to Field and Press RETURN."

            gosub load_distr_line

L13550:     errormsg$ = " "
            fieldnr%, last_fieldnr% = 0%
            gosub'121(0%)
                if keyhit%  =  1% then gosub start_card_over
                if keyhit%  =  2% then c% = 1%
                if keyhit%  =  3% then c% = max(1%, maxlines%)
                if keyhit%  =  4% then c% = max(1%,c%-10)
                if keyhit%  =  5% then c% = max(1%,min(c%+10,maxlines%))
                if keyhit%  =  6% then c% = max(1%,c%-1%)
                if keyhit%  =  7% then c% = min(maxlines%,c%+1%)
                if keyhit%  = 10% then gosub reset_rate
                if keyhit%  = 10% then gosub sum_distr
                if keyhit%  = 16% then editmode
                if keyhit% <>  0% then edit_detail
L13660:     fieldnr% = cursor%(1) - 11%
            if fieldnr%   <  1% or fieldnr% > 7% then L13550
            if cursor%(2) < 40% or fieldnr% = 1% then L13720
                fieldnr% = fieldnr% + 6%
                if fieldnr% > 11% then fieldnr% = fieldnr% - 1%

L13720:     if fieldnr% = last_fieldnr% then L13550

            gosub'052(fieldnr%, 2%)
                if enabled% = 0% then edit_detail
L13750:     gosub'121(fieldnr%)
                if keyhit%  =  1% then gosub start_card_over
                if keyhit% <>  0% then L13750
            gosub'152(fieldnr%, 2%)
                if errormsg$ <> " " then L13750
                last_fieldnr% = fieldnr%
                goto L13660

        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            * --------------------------------------------------------- *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        delete_detail
            message$ = "To DELETE Flashing Line, press ENTER.  To" &     ~
                       " abort delete, press PF-1."
            gosub'112(fieldnr%)
                  if keyhit%  = 1% then editmode
                  if keyhit% <> 0% then delete_detail
            if c% = maxlines% then L14170
                roll% = 1%
                for temp% = c% to maxlines% - 1%
                     gosub roll_detail
                next temp%
L14170:     c% = maxlines%
            gosub restart_distribution
            maxlines% = maxlines% - 1%
            item% = max(0%, min(item%, maxlines%-8%))
            gosub sum_distr
            goto editmode

        roll_detail
                ld_job$     (temp%) = ld_job$     (temp%+roll%)
                ld_task$    (temp%) = ld_task$    (temp%+roll%)
                ld_type$    (temp%) = ld_type$    (temp%+roll%)
                ld_start$   (temp%) = ld_start$   (temp%+roll%)
                ld_stop$    (temp%) = ld_stop$    (temp%+roll%)
                ld_hrs$     (temp%) = ld_hrs$     (temp%+roll%)
                ld_hrs      (temp%) = ld_hrs      (temp%+roll%)
                ld_dept$    (temp%) = ld_dept$    (temp%+roll%)
                ld_class$   (temp%) = ld_class$   (temp%+roll%)
                ld_etype$   (temp%) = ld_etype$   (temp%+roll%)
                ld_ot$      (temp%) = ld_ot$      (temp%+roll%)
                ld_shift$   (temp%) = ld_shift$   (temp%+roll%)
                ld_lbkt$    (temp%) = ld_lbkt$    (temp%+roll%)
                ld_obkt$    (temp%) = ld_obkt$    (temp%+roll%)
                ld_wc$      (temp%) = ld_wc$      (temp%+roll%)
                ld_actv$    (temp%) = ld_actv$    (temp%+roll%)
                ld_ext      (temp%) = ld_ext      (temp%+roll%)
                ld_oh       (temp%) = ld_oh       (temp%+roll%)
                ld_rate     (temp%) = ld_rate     (temp%+roll%)
                ld_base     (temp%) = ld_base     (temp%+roll%)
                ld_lbr_acct$(temp%) = ld_lbr_acct$(temp%+roll%)
                ld_oh_acct$ (temp%) = ld_oh_acct$ (temp%+roll%)
                ld_cda%     (temp%) = ld_cda%     (temp%+roll%)
        return

        delete_all_detail:
            message$ = "To DELETE ALL LINES, press ENTER.  To return" &  ~
                       " without Delete, press PF-1."
            gosub'112(9999%)
                  if keyhit%  = 1% then editmode
                  if keyhit% <> 0% then delete_detail
            maxlines% = 0%
            gosub clear_line_item_variables
            sum_val$(), sum_hrs$() = " "
            goto editmode


        REM *************************************************************~
            *             M I S C   R O U T I N E S                     *~
            * --------------------------------------------------------- *~
            * The supporting cast...                                    *~
            *************************************************************

        test_time    /* Pass TIME0$- HH:MM or HH.hh, get back TIME0$   */
                     /* formatted HH:MM, TIME0, and ERRORMSG$          */
            errormsg$ = " "
            time0     = 0
            convert time0$ to time0, data goto L17170
                if time0 >= 0 and time0 < 24 then L17160
                if time0 >= 0                then L17170
                     errormsg$ = "Decimal Time must be between 0 and" &  ~
                                 " 23.99"
                     return
L17160:         time0$ = "fmt"
L17170:     call "TIMEOK" (time0$, time0, errormsg$)
            return

        deffn'101 (time_in$)   /* Converts TIME_IN$ (HH:MM) to decimal */
                               /* time placed in DEC_TIME.             */
            convert str(time_in$,1,2) to hrs
            convert str(time_in$,4,2) to mins
            dec_time = hrs + (mins/60)
            return


        extend_line:      /* Calculate extensions for line C%.  Note-  */
                          /*   uses overhead rate, so better have it!  */
            ld_ext(c%), ld_oh(c%) = 0
            if str(ld_type$(c%),,1) = "N" then L17312
                ld_ext(c%) = round(ld_hrs(c%) * ld_rate(c%), 2)
                ld_oh (c%) = round(ld_ext(c%) * ld_oh_rate , 2)
L17312:     ld_ext$, ld_oh$ = " "
            if ld_ext(c%) <> 0 then                                      ~
                                call "CONVERT" (ld_ext(c%), 2.2, ld_ext$)
            if ld_oh (c%) <> 0 then                                      ~
                                call "CONVERT" (ld_oh (c%), 2.2, ld_oh$ )
            return


        sum_distr
            mat tc_nrs = zer
            sum_hrs$(), sum_val$() = " "

            if maxlines% < 1% then L17820
            for i% = 1% to maxlines%
                temp$     = str(ld_type$(i%),,1) & str(ld_ot$(i%),,1)
                temp1$    = str(ld_type$(i%),,1)
                if temp$  = "D " then tc_nrs(1) = tc_nrs(1) + ld_hrs(i%)
                if temp$  = "DY" then tc_nrs(2) = tc_nrs(2) + ld_hrs(i%)
                if temp1$ = "D"  then tc_nrs(3) = tc_nrs(3) + ld_ext(i%)
                if temp1$ = "D"  then tc_nrs(4) = tc_nrs(4) + ld_oh (i%)
                if temp$  = "I " then tc_nrs(5) = tc_nrs(5) + ld_hrs(i%)
                if temp$  = "IY" then tc_nrs(6) = tc_nrs(6) + ld_hrs(i%)
                if temp1$ = "N " then tc_nrs(7) = tc_nrs(7) + ld_hrs(i%)
            next i%
            tc_nrs(8) = tc_nrs(1) + tc_nrs(2) + tc_nrs(5) + tc_nrs(6) +  ~
                        tc_nrs(7)
            temp = tc_nrs(1) : gosub L17740 : sum_hrs$( 1) = temp5$
            temp = tc_nrs(2) : gosub L17740 : sum_hrs$( 2) = temp5$
            temp = tc_nrs(1) + tc_nrs(2)
                               gosub L17740 : sum_hrs$( 3) = temp5$
            temp = tc_nrs(5) : gosub L17740 : sum_hrs$( 4) = temp5$
            temp = tc_nrs(6) : gosub L17740 : sum_hrs$( 5) = temp5$
            temp = tc_nrs(5) + tc_nrs(6)
                               gosub L17740 : sum_hrs$( 6) = temp5$
            temp = tc_nrs(7) : gosub L17740 : sum_hrs$( 7) = temp5$
                                             sum_hrs$( 9) = temp5$
            temp = tc_nrs(1) + tc_nrs(5) + tc_nrs(7)
                               gosub L17740 : sum_hrs$(10) = temp5$
            temp = tc_nrs(2) + tc_nrs(6)
                               gosub L17740 : sum_hrs$(11) = temp5$
            temp = tc_nrs(8)
                               gosub L17740 : sum_hrs$(12) = temp5$
            goto L17760
L17740:         if temp <> 0 then L17743
                     temp5$ = " "
                     return
L17743:         temp5$ = "fmt"
                call "TIMEOK" (temp5$, temp, " ")
                return
L17760:     if tc_nrs(3) <> 0 then                                       ~
                             call "CONVERT" (tc_nrs(3), 2.2, sum_val$(1))
            if tc_nrs(4) <> 0 then                                       ~
                             call "CONVERT" (tc_nrs(4), 2.2, sum_val$(2))
            temp = tc_nrs(3) + tc_nrs(4)
            if temp <> 0 then call "CONVERT" (temp, 2.2, sum_val$(4))
L17820:     return


        load_dept_info:
            init(" ") dept_use_rate$, dept_oh_acct$, dept_class$,        ~
                      oh_acct$, deflt_class$, lbr_acct$
            get #18 using L17930, lbr_acct$, dept_class$, dept_oh_acct$,  ~
                                 dept_use_rate$
L17930:         FMT POS(35), CH(9), CH(4), CH(9), CH(1)
            oh_acct$     = emp_oh_acct$
            deflt_class$ = emp_class$
            if dept_use_rate$ <> "Y" then L18000
                oh_acct$     = dept_oh_acct$
                deflt_class$ = dept_class$
                goto L18020
L18000:     if oh_acct$      = " " then oh_acct$      = dept_oh_acct$
            if deflt_class$  = " " then deflt_class$  = dept_class$
L18020: return


        load_distr_line:
           gosub'152(1%, 1%)             /* Job-Task Descr, Type...    */
           gosub'152(3%, 1%)             /* Department Code            */
           gosub'152(4%, 1%)             /* Labor Class (for OH Rate)  */
           gosub extend_line
           call "CONVERT" (ld_rate(c%), 2.4, ld_rate$)
           call "CONVERT" (ld_base(c%), 2.2, ld_base$)
           ld_cda_prompt$, ld_cda$ = " "
           if cda% <> 1% then return
               ld_cda_prompt$ = "CDA Trans #"
               convert ld_cda%(c%) to ld_cda$, pic(#######)
               if ld_cda%(c%) = 0% then ld_cda$ = "Not CDA"
           return


        call_jbactsub:
            outemp$  = emp$
            outdate$ = tc_date$

            if mode% = 1% then L18280
                outjob$, outtime$ = " "
                outhrs = 0
                goto L18320
L18280:     outjob$  = ld_job$  (c%)
            outtime$ = ld_start$(c%)
            outhrs   = ld_hrs   (c%)

L18320:     call "JBACTSUB" (#1,#19,#3,#4,#5,#6,#7,#8,#22,#23,#12,       ~
                             outjob$, outemp$, outdate$, outtime$,       ~
                             outhrs)
            return


        reset_line_rates:
            gosub reset_line_rates_too
            goto editmode

        reset_line_rates_too
            if maxlines% = 0% then return
                call "SHOSTAT" ("Resetting Line Rates...")
                for c% = 1% to maxlines%
                    gosub'152(4%,1%)               /* Get O/H Rate     */
                    gosub reset_rate               /* Call JBTCRATE    */
                next c%
                gosub sum_distr
                return



        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves Data On File After Input/Editing.                   *~
            *************************************************************
        datasave

*        First check header hours against distribution...
            if sw_chk_hrs$ = "N" or maxlines% = 0% then L19340

            tc_in, tc_out, hdr_hrs = 0
            if tc_in$ = " " or tc_out$ = " " then L19160
                call "TIMEOK" (tc_in$ , tc_in , errormsg$)
                call "TIMEOK" (tc_out$, tc_out, errormsg$)
                if tc_out < tc_in then tc_out = tc_out + 24
                hdr_hrs = tc_out - tc_in
L19160:     if abs(hdr_hrs - tc_nrs(8%) ) < 0.0001  then L19340
                if sw_chk_hrs$ <> "E" then L19250
                     u3% = 2%
                     call "ASKUSER" (u3%, "HOURS OUT OF BALANCE",        ~
                          "The hours per the time card header",          ~
                          "were not correctly distributed.",             ~
                          "Press RETURN to return to data entry...")
                     goto editmode

L19250:              u3% = 2%
                     call "ASKUSER" (u3%, "HOURS OUT OF BALANCE",        ~
                       "Header hours do not equal distribution hours.",  ~
                       "Press RETURN to return to DATA ENTRY, -OR-",     ~
                       "Press PF-16 to SAVE time card AS IS...")
                     if u3% = 16% then L19340
                     if u3% =  0% then editmode else L19250

L19340:     call "SHOSTAT" ("Saving Time Card")

*        Clear buffer, save new info...
            readkey$ = all(hex(00))
            str(readkey$,,18) = str(emp$) & str(tc_dateu$)
            call "DELETE" (#9 , readkey$, 18%)
            call "DELETE" (#10, readkey$, 18%)

            gosub L31000    /* Write New Time Card To Files */
            lastemp$     = emp$
            mfac$(card%) = hex(86) /* Show changed ones bright */
            maxcards%    = max(maxcards%,card%)
            if insert%   = 1% then inputmode else all_cards_summary


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets defaults and enables fields for the TC Summary Scrn. *~
            *************************************************************

        deffn'050(fieldnr%)
            enabled% = 1% : message$ = " "
            on fieldnr% gosub       L20120,         /* START DATE       */~
                                    L20230          /* EMPLOYEE CODE    */
            return

L20120
*        Default/Enable for START DATE
            message$ = "Enter date of Oldest Time Card you wish to see" &~
                       " or edit (01/01/1901 insures all)."
            if startdate$ = " " or startdate$ = blankdate$ then L20180
                enabled%  = 0%
                return
L20180:     call "DATE" addr("G+", date, -30%, startdate$, u3%)
            if u3% <> 0% then startdate$ = "19010101"
            call "DATFMTC" (startdate$)
            return

L20230
*        Default/Enable for EMPLOYEE NUMBER
            message$ = "Enter a blank or partial value to search for" &  ~
                       " Employee Number."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE TIME CARD HEADER.*~
            *************************************************************

            deffn'051(fieldnr%, edit%)
                  enabled% = 1% : message$ = " "
                  on fieldnr% gosub L21150,         /* Time Card Date   */~
                                    L21190,         /* Clock In/Out     */~
                                    L21230,         /* Holiday?         */~
                                    L21280,         /* Department       */~
                                    L21330          /* Shift            */
                  return

L21150
*        Default/Enable for TIME CARD DATE
            message$ = "Enter the Date for this Time Card."
            return

L21190
*        Default/Enable for CLOCK IN/OUT TIMES
            message$ = "Enter the Clock-In and Clock-Out times."
            return

L21230
*        Default/Enable for HOLIDAY?
            message$ = "Is this a holiday? (Y/N)"
            if edit% = 1% then enabled% = 0%
            return

L21280
*        Default/Enable for DEFAULT DEPARTMENT
            message$ = "Enter default department code."
            if edit% = 1% then enabled% = 0%
            return

L21330
*        Default/Enable for DEFAULT SHIFT
            message$ = "Enter the default Shift worked, (1 - 4)"
            if edit% = 1% then enabled% = 0%
            if tc_shift$ = " " then tc_shift$ = "1"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   D E T A I L     *~
            * --------------------------------------------------------- *~
            * Sets Defaults and Enables Fields For The Page 2 Of Input. *~
            *************************************************************

            deffn'052(fieldnr%, edit%)
                enabled% = 1% : message$ = " "
                on fieldnr% gosub   L22220,      /* Job or Task         */~
                                    L22270,      /* Start/Stop, Hours   */~
                                    L22340,      /* Department          */~
                                    L22410,      /* Labor Class         */~
                                    L22490,      /* Earnings Type       */~
                                    L22540,      /* Overtime?           */~
                                    L22620,      /* Shift               */~
                                    L22710,      /* Hourly Rate         */~
                                    L22870,      /* Gross Bucket        */~
                                    L23010,      /* Ovrhd Bucket        */~
                                    L23150,      /* Work Center         */~
                                    L23210       /* Activity Code       */
            return

L22220
*        Default/Enable for JOB -or- TASK
            message$ = "Enter Job Number -OR- Task Code. Enter partial" &~
                       " value to search for code."
            return

L22270
*        Default/Enable for START/STOP & HOURS
            message$ = "Enter Start and Stop Times -OR- Hours to charge."
            if edit% = 2% or ld_start$(c%) <> " " then return
                 if c% = 1% then ld_start$(c%) = tc_in$ else             ~
                                 ld_start$(c%) = ld_stop$(c%-1%)
            return

L22340
*        Default/Enable for Department Code
            message$ = "Enter Department Code."
            if edit% = 2% then return
                if ld_dept$(c%) = " " then ld_dept$(c%) = tc_dept$
                if ld_dept$(c%) > " " then enabled%     = 0%
                return

L22410
*        Default/Enable for LABOR CLASS
            message$="Labor Class defines the Overhead %.  Enter a"    & ~
                     " partial value to search file."
            if edit% = 2% then return
                enabled% = 0%
                if ld_class$(c%) = " " then ld_class$(c%) = deflt_class$
                return

L22490
*        Default/Enable for EARNINGS TYPE
            message$ = "This defines the Pay Rate.  Enter blank or a"  & ~
                       " valid EARN TYPE."
            if edit% = 1% and sw_etype$ = "N" then enabled% = 0%
            return

L22540
*        Default/Enable for OVERTIME?
            message$ = "Enter 'Y' if the time on this line is overtime."
            if edit% = 2% then return
                ld_ot$(c%) = " "
                if tc_nrs(8) - tc_nrs(7) <= 8 then enabled%   = 0%       ~
                                              else ld_ot$(c%) = "Y"
                return

L22620
*        Default/Enable for SHIFT
            message$ = "Specify Shift (1-4)."
            if edit% = 2% then return
                enabled% = 0%
                if ld_shift$(c%) = " " then ld_shift$(c%) = tc_shift$
                if ld_shift$(c%) < "1"  or  ld_shift$(c%) > "4" then     ~
                                                     ld_shift$(c%) = "1"
                return

L22710
*        Default/Enable for HOURLY RATE
            reset% = 0%
            message$ = "Enter Hourly Rate for this Distribution Line."
            if sw_mod_rate$ = "N" then enabled% = 0%
            if edit% = 2% then L22840 else L22750
        reset_rate:  reset% = 1%
L22750:         call "JBTCRATE" (emp$, ld_etype$(c%), ld_dept$(c%),      ~
                                 ld_shift$(c%), ld_ot$(c%), tc_hol$,     ~
                                 ld_use_dept$(c%), ld_base(c%),          ~
                                 ld_rate(c%), #13, #18, #16)
                if ld_rate(c%)  = 0 then enabled% = 1% else enabled% = 0%
                if sw_mod_rate$ = "N" then enabled% = 0%
                call "CONVERT" (ld_rate(c%), 2.4, ld_rate$)
                call "CONVERT" (ld_base(c%), 2.2, ld_base$)
                gosub extend_line
                if reset% = 1% then return
L22840:     if enabled% = 1% then call "SPCSMASH" (ld_rate$)
            return

L22870
*        Default/Enable for Gross Bucket
            message$ = "Enter Cost bucket to place Pay into."
            if edit% = 1% then enabled% = 0%
            if ld_lbkt$(c%) <> " " then return
                ld_lbkt% = dflt_labr%
                call "READ100" (#30, str(ld_class$(c%)), f1%)
                if f1% = 0% then L22960
                     get #30, using L22950, ld_lbkt%
L22950:                   FMT  POS(59), BI(1)
L22960:         if ld_lbkt% < 1% or ld_lbkt% > 12%                       ~
                          then ld_lbkt$(c%) = " "                        ~
                          else ld_lbkt$(c%) = buckets$(ld_lbkt%)
                return

L23010
*        Default/Enable for Overhead Bucket
            message$ = "Enter Cost bucket to place Overhead into."
            if edit% = 1% then enabled% = 0%
            if ld_obkt$(c%) <> " " then return
                ld_obkt% = dflt_oh%
                call "READ100" (#30, str(ld_class$(c%)), f1%)
                if f1% = 0% then L23100
                     get #30, using L23090, ld_obkt%
L23090:                   FMT  POS(60), BI(1)
L23100:         if ld_obkt% < 1% or ld_obkt% > 12%                       ~
                          then ld_obkt$(c%) = " "                        ~
                          else ld_obkt$(c%) = buckets$(ld_obkt%)
                     return

L23150
*        Default/Enable for WORK CENTER
            message$ = "Enter Work Center."
            if edit% = 2% then return
                if sw_wc$ = "N" then enabled% = 0%
                if str(ld_type$(c%),,1) <> "D" then enabled% = 0%
                return

L23210
*        Default/Enable for ACTIVITY CODE
            message$ = "Enter Activity Code."
            if edit% = 2% then return
                if sw_actv$ = "N" then enabled% = 0%
                if str(ld_type$(c%),,1) <> "D" then enabled% = 0%
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'056(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L24170,         /* Restrict Display       */~
                              L24200,         /* Modify Rates?          */~
                              L24230,         /* Auto-Reset Rates?      */~
                              L24260,         /* Check Distr Hours      */~
                              L24290,         /* Enable Earn Type?      */~
                              L24320,         /* Enable WC?             */~
                              L24350          /* Enable Activity?       */
            return

L24170: REM Def/Enable Restrict Value Display To-  SW_USER$
            message$ = "Enter User IDs that can see rates.  Leave" &     ~
                       " blank to display for everyone."
            return

L24200: REM Def/Enable Are Rates Modifiable?       SW_MOD_RATE$
            message$ = "Are Hourly Rates modifiable? (Y/N)."
            return

L24230: REM Def/Enable Auto-Reset Rates?           SW_RESET_RATE$
            message$ = "Should rates be automatically reset when" &      ~
                       " related fields are changed? (Y/N)."
            return

L24260: REM Def/Enable Check Distribution Hours?   SW_CHK_HRS$
            message$ = "Check total distr against TC start-stop times?"& ~
                       " (Err/Warn/Nocheck)."
            return

L24290: REM Def/Enable Enable Earnings Type?       SW_ETYPE$
            message$ = "Enable Earnings Type field in input mode? (Y/N)."
            return

L24320: REM Def/Enable Enable Work Center?         SW_WC$
            message$ = "Enable Work Center field in input mode? (Y/N)."
            return

L24350: REM Def/Enable Enable Activity Code?       SW_ACTV$
            message$ = "Enable WC Activity field in input mode? (Y/N)."
            return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK                                      *~
            *************************************************************
            init(" ") tc_date$, tc_dateu$, tc_status$, tc_in$, tc_out$,  ~
                      tc_shift$, tc_hol$, tc_dept$, sum_hrs$(),          ~
                      sum_val$()
            call "ALLFREE"
        clear_line_item_variables:
            init(" ") ld_job$(), ld_task$(), ld_job_descr$, ld_type$(),  ~
                      ld_start$(), ld_stop$(), ld_hrs$(), ld_rate$,      ~
                      ld_dept$(), ld_class$(), ld_etype$(), ld_ot$(),    ~
                      ld_shift$(), ld_lbkt$(), ld_obkt$(), ld_wc$(),     ~
                      ld_actv$(), inuser$, message$, ld_use_dept$(),     ~
                      ld_lbr_acct$(), ld_oh_acct$(), ld_base$
            item%, ld_lbkt%, ld_obkt% = 0%
            mat ld_hrs  = zer
            mat ld_base = zer
            mat ld_rate = zer
            mat ld_ext  = zer
            mat ld_oh   = zer
            mat ld_cda% = zer
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover:   /* Allow user opportunity to Start Over.          */
            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
                return clear all
                goto main

        start_card_over:
            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
                return clear all
                if card% > maxcards% then gosub restart_card
                if insert% = 1% then inputmode
                goto all_cards_summary


        REM *************************************************************~
            *             L O A D   O L D   R E P O R T E D             *~
            * --------------------------------------------------------- *~
            * Loads Old Reported From Disk Files.                       *~
            *************************************************************

        load_this_card:
            maxlines%, file% = 0%

*        Try and load the Header Record...
            readkey$ = str(emp$) & str(tc_smry$(card%),,6)
            call "READ100" (#9, readkey$, f1%)  /* Try Buffer */
            if f1% = 0% then L30170
                call "SHOSTAT" ("Recalling Details From Buffer")
                file% = 9%
                goto L30220

L30170:     call "READ100" (#14, readkey$, f1%)  /* Try Master */
            if f1% = 0% then return
                file% = 14%
                call "SHOSTAT" ("Recalling Details From Master File")

L30220:     get #file% using L30260, tc_status$, tc_dateu$,               ~
                                    tc_in$, tc_out$,                     ~
                                    tc_nrs(), tc_shift%, tc_hol$,        ~
                                    tc_dept$, tc_by$
L30260:         FMT CH(1), CH(6), POS(26), 2*CH(5), 8*PD(14,4), BI(1),   ~
                    CH(1), CH(4), CH(3)
            tc_hol_old$ = tc_hol$
            convert tc_shift% to tc_shift$, pic(#)
            tc_date$ = tc_dateu$ : call "DATFMTC" (tc_date$)

*        Now load the line items...
            str(readkey$,19) = all(hex(00))
            file% = file% + 1%

L30350:     call "PLOWNEXT" (#file%, readkey$, 18%, f1%)
            if f1% = 1% then L30390
                gosub sum_distr
                return
L30390:     c%, maxlines% = maxlines% + 1%
            get #file% using L30490,                                      ~
                    ld_start$(c%), ld_job$(c%), ld_task$(c%),            ~
                    ld_stop$(c%), ld_hrs$(c%), ld_class$(c%),            ~
                    ld_etype$(c%),                                       ~
                    ld_rate(c%), ld_hrs(c%), ld_ext(c%), ld_oh(c%),      ~
                    ld_wc$(c%), ld_actv$(c%), ld_type$(c%), ld_base(c%), ~
                    ld_use_dept$(c%), ld_ot$(c%), ld_shift%,             ~
                    ld_dept$(c%), ld_lbr_acct$(c%), ld_oh_acct$(c%),     ~
                    ld_lbkt%, ld_obkt%, ld_cda%(c%)
L30490:              FMT POS(20), CH(5), XX(1), CH(8), CH(6), CH(5),     ~
                         CH(5), CH(4), CH(12), 4*PD(14,4), 2*CH(4),      ~
                         CH(1), PD(14,4), 2*CH(1), BI(1), CH(4), 2*CH(9),~
                         2*BI(1), BI(4)
            if ld_shift% < 1% or ld_shift% > 4% then ld_shift% = 1%
            convert ld_shift% to ld_shift$(c%), pic(#)
            if ld_lbkt% < 1% or ld_lbkt% >buckets% then ld_lbkt% = 2%
            ld_lbkt$(c%) = buckets$(ld_lbkt%)
            if ld_obkt% < 1% or ld_obkt% >buckets% then ld_obkt% = 3%
            ld_obkt$(c%) = buckets$(ld_obkt%)
            if ld_type$(c%) = "D" then ld_type$(c%) = "Direct"
            if ld_type$(c%) = "I" then ld_type$(c%) = "Indirect"
            if ld_type$(c%) = "N" then ld_type$(c%) = "Non-P/R "
            goto L30350


L31000: REM *************************************************************~
            *       W R I T E   R E P O R T E D   T O   F I L E S       *~
            * --------------------------------------------------------- *~
            * SAVES GOODIES IN A SAFE PLACE.                            *~
            *************************************************************

*        Here we gooooo....
            if inuser$ = " " then inuser$ = userid$
            gosub sum_distr
            if maxlines% = 0% then L31370

*        Do lines first...
            for c% = 1% to maxlines%
               search str(buckets$()) = str(ld_lbkt$(c%)) to p%() step 10
               ld_lbkt% = (p%(1)+9%) / 10%
               if ld_lbkt% < 1% or ld_lbkt% > buckets% then ld_lbkt% = 2%
               search str(buckets$()) = str(ld_obkt$(c%)) to p%() step 10
               ld_obkt% = (p%(1)+9%) / 10%
               if ld_obkt% < 1% or ld_obkt% > buckets% then ld_obkt% = 3%
               day1$ = " "
               if (tc_in$ <> " " and ld_start$(c%) <> " ") and           ~
                                  ld_start$(c%) < tc_in$ then day1$ = "+"
               shift% = 1% : convert ld_shift$(c%) to shift%,            ~
                                                         data goto L31260
L31260:        write #10 using L31480,                                    ~
                     emp$, tc_dateu$, day1$, ld_start$(c%), c%,          ~
                     ld_job$(c%), ld_task$(c%), ld_stop$(c%),            ~
                     ld_hrs$(c%), ld_class$(c%), ld_etype$(c%),          ~
                     ld_rate(c%), ld_hrs(c%), ld_ext(c%), ld_oh(c%),     ~
                     ld_wc$(c%), ld_actv$(c%), str(ld_type$(c%),,1),     ~
                     ld_base(c%), ld_use_dept$(c%), ld_ot$(c%),          ~
                     shift%, ld_dept$(c%), ld_lbr_acct$(c%),             ~
                     ld_oh_acct$(c%), ld_lbkt%, ld_obkt%, ld_cda%(c%)," "
            next c%

L31370
*        Now save the Time Card Header...
            shift% = 1% : convert tc_shift$ to shift%, data goto L31390
L31390:     write #9 using L31770,                                        ~
                     "N", tc_dateu$, emp$, tc_dateu$, tc_in$, tc_out$,   ~
                     tc_nrs(), shift%, tc_hol$, tc_dept$, inuser$,       ~
                     userid$, maxlines%, " "
            c%    = card%
            file% = 9%
            gosub update_tc_smry
            return

L31480: FMT CH(12),                      /* Employee Number            */~
            CH( 6),                      /* Time Card Date             */~
            CH( 1),                      /* Sorting field              */~
            CH( 5),                      /* Start Time                 */~
            BI( 1),                      /* Sequence Number            */~
            CH( 8),                      /* Job Number                 */~
            CH( 6),                      /* Time Card Date             */~
            CH( 5),                      /* Stop Time                  */~
            CH( 5),                      /* Hours (HH:MM)              */~
            CH( 4),                      /* Labor Class                */~
            CH(12),                      /* Earn Type                  */~
            PD(14,4),                    /* Earnings Rate              */~
            PD(14,4),                    /* Hours                      */~
            PD(14,4),                    /* Pay Amount (Hrs * Rate)    */~
            PD(14,4),                    /* Overhead Amount            */~
            CH( 4),                      /* Work Center                */~
            CH( 4),                      /* Activity Code              */~
            CH( 1),                      /* Line Item Type (D, I, N)   */~
            PD(14,4),                    /* Base Rate                  */~
            CH( 1),                      /* Use Dept Rate?             */~
            CH( 1),                      /* Overtime flag (Y/blank)    */~
            BI( 1),                      /* Shift                      */~
            CH( 4),                      /* Department                 */~
            CH( 9),                      /* Labor Account              */~
            CH( 9),                      /* Overhead Account           */~
            BI( 1),                      /* Labor Bucket               */~
            BI( 1),                      /* Overhead Bucket            */~
            BI( 4),                      /* CDA Trans Number           */~
            CH(55)                       /* Filler                     */

L31770: FMT CH( 1),                      /* Status                     */~
            CH( 6),                      /* Time Card Date             */~
            CH(12),                      /* Employee Code              */~
            CH( 6),                      /* Time Card Date             */~
            CH( 5),                      /* Start Time                 */~
            CH( 5),                      /* Stop Time                  */~
            8*PD(14,4),                  /* Summary Numbers            */~
            BI( 1),                      /* Shift                      */~
            CH( 1),                      /* Holiday?                   */~
            CH( 4),                      /* Department Code            */~
            CH( 3),                      /* Original User              */~
            CH( 3),                      /* Last Modified By           */~
            BI( 2),                      /* Number of line items       */~
            CH(87)                       /* POSTING INFO & FILLER      */


        REM *************************************************************~
            *           L O A D   A L L   T I M E   C A R D S           *~
            * --------------------------------------------------------- *~
            * Load all existing Time Cards for this employee into memry *~
            *************************************************************

        load_all_cards:
            call "SHOSTAT" ("Loading Time Cards...")
            tc_smry$() = " "
            maxcards%  =  0%
            file%      = 14%  : gosub L35140    /* Master first */
            file%      =  9%  : gosub L35140    /* Buffer next  */
            goto L35330

L35140
*        Find out what's out there...
            readkey$ = str(emp$) & start$
            call "READ100" (#file%, readkey$, f1%)
            if f1% = 1% then L35210

L35190:     call "PLOWNEXT" (#file%, readkey$, 12%, f1%)
            if f1% = 0% then return
L35210:         search tc_smry$() = str(readkey$,13,6) to p%() step 80
                if p%(1) > 0% then c% = (p%(1) + 79%) / 80%              ~
                              else c%, maxcards% = maxcards% + 1%
                gosub update_tc_smry
                if maxcards% < 400% then L35190
                    call "ASKUSER" (2%, "DISPLAY FULL",                  ~
                           "Unable to load all time cards for employee.",~
                           "Reset First Card Date to see more cards.",   ~
                           "Press RETURN to continue...")
                    return clear

L35330
*        Finish up the paper work...
            if maxcards% > 1% then                                       ~
                            call "SORT" addr (tc_smry$(), maxcards%, 80%)
            if maxcards% > 0% then str(mfac$(),,maxcards%) = all(hex(8e))
            base% = max(0%, maxcards% - 11%)
            return


        update_tc_smry:
*        Loads TC header data from the buffer identified by FILE% into
*        TC_SMRY$(C%).
            tc_smry$(c%) = " "
            get #file% using L35730,                                      ~
                     str(tc_smry$(c%),72, 9),      /* Status           */~
                     str(tc_smry$(c%), 1, 6),      /* TC Date          */~
                     str(tc_smry$(c%),36, 5),      /* Clock In         */~
                     str(tc_smry$(c%),42, 5),      /* Clock Out        */~
                     nrs(), temp%,                 /* Hrs, $s, Shift   */~
                     str(tc_smry$(c%),26, 3)       /* Holiday?         */
L35730:         FMT CH(1), CH(6), POS(26), 2*CH(5), 8*PD(14,4), BI(1),   ~
                    CH(1)
            str(tc_smry$(c%), 7, 8) = str(tc_smry$(c%), 1, 6)
            call "DATEFMT" (str(tc_smry$(c%), 7, 8))
            call "DAY" addr(str(tc_smry$(c%), 1, 6), day%)
            str(tc_smry$(c%),16, 9) = days$(day%)
            nrs(1) = nrs(1) + nrs(2)               /* Direct Hours     */
            if nrs(1) = 0 then L35830
                str(tc_smry$(c%),49,5) = "fmt"
                call "TIMEOK" (str(tc_smry$(c%),49,5), nrs(1), " ")
L35830:     nrs(5) = nrs(5) + nrs(6)               /* Indirect Hours   */
            if nrs(5) = 0 then L35870
                str(tc_smry$(c%),57,5) = "fmt"
                call "TIMEOK" (str(tc_smry$(c%),57,5), nrs(5), " ")
L35870:     if nrs(7) = 0 then L35900               /* Non-P/R Hours    */
                str(tc_smry$(c%),65,5) = "fmt"
                call "TIMEOK" (str(tc_smry$(c%),65,5), nrs(7), " ")
L35900:     convert temp%  to str(tc_smry$(c%),32, 1), pic(#)
            if str(tc_smry$(c%),26,3) = "Y"                              ~
                                   then str(tc_smry$(c%),26, 3) = "Yes"  ~
                                   else str(tc_smry$(c%),26, 3) = "   "
            temp% = pos(" NP$C" = str(tc_smry$(c%),72,1))
            if temp% = 0% then temp% = 1%
            str(tc_smry$(c%),72,9) = stats$(temp%)
            return


        REM *************************************************************~
            *          L I N E   S U M M A R Y   S C R E E N            *~
            * --------------------------------------------------------- *~
            * Selection Criteria and Time Card Listing.                 *~
            *************************************************************

        deffn'100(fieldnr%)
            if fieldnr% = 0% then L40280
                pfdescr$(1) = "(1)Start Over    (4)Edit Start Date      "~
                            & "                      (13)Instructions"
                pfdescr$(2) = "(10)List Time Cards Open by CDA          "~
                            & "                      (15)Print Screen"
                pfdescr$(3) = "(17)Modify Program Switchs               "~
                            & "                      (16)Exit Program"
                pfkeys$ = hex(000104ff0d0f10110affffffffffffff)
                if admin% = 1% then L40150
                     str(pfdescr$(3),,26) = " "
                     str(pfkeys$,8,1) = hex(ff)
                if cda% <> 1% then L40150
                     str(pfdescr$(2),,50) = " "
                     str(pfkeys$,9,1) = hex(ff)
L40150:         str(pfdescr$(3),63,1) = hex(84)
                if fieldnr% = 1% then str(pfdescr$(1),18,20) = " "

                init (hex(9c)) lfac$(), hfac$()
                tran (header$(2), hex(9cac)) replacing
                title$ = "Last Employee Updated: " & lastemp$
                str(title$,62) = xtitle$
                init (hex(84)) lfac1$()
                on fieldnr% gosub L40250,           /* Start Date       */~
                                  L40250            /* Employee Number  */
                goto L40530
L40250:               lfac1$(fieldnr%) = hex(81)
                      return

L40280:         pfdescr$(1) = "(1)Start Over                      (11)En"~
                            & "ter New Time Card    (13)Instructions "
                pfdescr$(2) = "(2)First Card     (4/6)Prev Cards        "~
                            & "                     (15)Print Screen "
                pfdescr$(3) = "(3)Last Card      (5/7)Next Cards        "~
                            & "                     (16)Next Employee"
                pfkeys$ = hex(20010203040605070bff0d0f1000ffff)
                str(pfdescr$(3),62,1) = hex(84)
                if base% > 0% then L40390
                    str(pfdescr$(2),19,15) = " "    /* Prev Screen Off */
                    str(pfkeys$,3,1), str(pfkeys$,5,2) = hex(ff)
L40390:         if base% + 11% < maxcards% then L40420
                    str(pfdescr$(3), 1,33) = " "    /* Next Screen Off */
                    str(pfkeys$,4,1), str(pfkeys$,7,2) = hex(ff)
L40420:         if maxcards% > 12% then L40450
                    str(pfdescr$(2),,15) = " "      /* First-Last  Off */
                    str(pfdescr$(3),,15) = " "      /* First-Last  Off */
L40450:         init (hex(8c)) lfac$(), hfac$(1)
                init (hex(84)) lfac1$() : hfac$(2) = hex(ac)
                tran (header$(2), hex(ac9c)) replacing

                for i% = 1% to 11%
                     u3% = base% + i%
                     if tc_smry$(u3%) = " " then lfac$(i%) = hex(9c)
                next i%
                title$ = "Employee Time Cards Summary Screen"
L40530:         str(title$,62) = xtitle$

L40550:     accept                                                       ~
               at (01,02), "Manage Labor Time Cards",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), title$                  ,ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "First Time Card:",                           ~
               at (04,19), fac(lfac1$(1)),  startdate$          , ch(10),~
               at (05,02), "Employee Number:",                           ~
               at (05,19), fac(lfac1$(2)),  emp$                , ch(12),~
               at (05,33), fac(hex(8c)),    emp_name$           , ch(32),~
                                                                         ~
               at (07,02), fac(hfac$(1)),   header$(1)           ,ch(79),~
               at (08,02), fac(hfac$(2)),   header$(2)           ,ch(79),~
                                                                         ~
               at (09,02), fac(hex(81)), put_cursor_here$,               ~
                                                                         ~
               at (09,02), fac(mfac$(base%+ 1)), line$(base%+ 1) ,ch(04),~
               at (10,02), fac(mfac$(base%+ 2)), line$(base%+ 2) ,ch(04),~
               at (11,02), fac(mfac$(base%+ 3)), line$(base%+ 3) ,ch(04),~
               at (12,02), fac(mfac$(base%+ 4)), line$(base%+ 4) ,ch(04),~
               at (13,02), fac(mfac$(base%+ 5)), line$(base%+ 5) ,ch(04),~
               at (14,02), fac(mfac$(base%+ 6)), line$(base%+ 6) ,ch(04),~
               at (15,02), fac(mfac$(base%+ 7)), line$(base%+ 7) ,ch(04),~
               at (16,02), fac(mfac$(base%+ 8)), line$(base%+ 8) ,ch(04),~
               at (17,02), fac(mfac$(base%+ 9)), line$(base%+ 9) ,ch(04),~
               at (18,02), fac(mfac$(base%+10)), line$(base%+10) ,ch(04),~
               at (19,02), fac(mfac$(base%+11)), line$(base%+11) ,ch(04),~
                                                                         ~
               at (09,07), fac(lfac$( 1)),  str(tc_smry$(base% + 1%),7), ~
               at (10,07), fac(lfac$( 2)),  str(tc_smry$(base% + 2%),7), ~
               at (11,07), fac(lfac$( 3)),  str(tc_smry$(base% + 3%),7), ~
               at (12,07), fac(lfac$( 4)),  str(tc_smry$(base% + 4%),7), ~
               at (13,07), fac(lfac$( 5)),  str(tc_smry$(base% + 5%),7), ~
               at (14,07), fac(lfac$( 6)),  str(tc_smry$(base% + 6%),7), ~
               at (15,07), fac(lfac$( 7)),  str(tc_smry$(base% + 7%),7), ~
               at (16,07), fac(lfac$( 8)),  str(tc_smry$(base% + 8%),7), ~
               at (17,07), fac(lfac$( 9)),  str(tc_smry$(base% + 9%),7), ~
               at (18,07), fac(lfac$(10)),  str(tc_smry$(base% +10%),7), ~
               at (19,07), fac(lfac$(11)),  str(tc_smry$(base% +11%),7), ~
                                                                         ~
               at (21,02), fac(hex(a4)), message$                ,ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)             ,ch(79),~
                     keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13% then L41050
                  call "MANUAL" ("JBTCINP")
                  goto L40550

L41050:        if keyhit% <> 15% then L41090
                  call "PRNTSCRN"
                  goto L40550

L41090:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               return


        REM *************************************************************~
            *          L I N E   S U M M A R Y   S C R E E N            *~
            * --------------------------------------------------------- *~
            * SCREEN FOR EDITING PAGE 3 OF DOCUMENT.                    *~
            *************************************************************

        deffn'110(fieldnr%)
            if fieldnr% = 0% then L42220

                pfdescr$(1) = "(1)Start Over    (4)Previous Field       "~
                            & "                    (13)Instructions  "
                pfdescr$(2) = "                 (9)Report Work Center Mo"~
                            & "vement              (15)Print Screen  "
                pfdescr$(3) = "                                         "~
                            & "                    (16)Summary Screen"
                pfkeys$ = hex(000104090d0f202010ffffffffffffffff)
                mode% = 0%
                str(pfdescr$(3),58,1) = hex(84)
                if fieldnr% <> 1% then L42095
                     str(pfdescr$(1%),18%,18%) = " "
                     str(pfkeys$,3%,1%) = hex(ff)
                     goto L42100
L42095:         if fieldnr% > 1% then str(pfdescr$(3),58)  = " "
L42100:         init (hex(9c)) tfac$(), lfac$(), hfac$()
                tran (header$(4), hex(9cac)) replacing
                init (hex(84)) lfac1$()
                goto L42175

            deffn'111(fieldnr%)
                init(hex(8c)) lfac1$(), lfac$(), tfac$()
                pfdescr$(1) = "(1)Start Over                            "~
                            & "                     (13)Instructions "
                pfdescr$(2) = "                                         "~
                            & "                     (15)Print Screen "
                pfdescr$(3) = "                                         "~
                            & "                                      "
                pfkeys$ = hex(00010dff0fffffffffffffffffffffff)

L42175:         on fieldnr% gosub L42205,           /* Time Card Date   */~
                                  L42205,           /* Clock In/Out     */~
                                  L42205,           /* Holiday?         */~
                                  L42205,           /* Dflt Department  */~
                                  L42205            /* Dflt Shift       */
                goto L42430
L42205:               lfac1$(fieldnr%) = hex(81)   /* Upper Case       */
                      return

L42220:         pfdescr$(1) = "(1)Start Over    (9)WC Movement   (10)Res"~
                            & "et Rates- All Lines   (13)Instructions"
                pfdescr$(2) = "(2)First Line (4/6)Prev Lines     (11)Add"~
                            & " Line Item            (15)Print Screen"
                pfdescr$(3) = "(3)Last Line  (5/7)Next Lines  (12/28)Del"~
                            & "ete Line/Delete All   (16)Save Data   "
                pfkeys$ = hex(0001020304060507090b0c0d0f101c0aff)
                if cda% <> 1% then L42270
                     str(pfdescr$(1),64) = "(14)Review CDA"
                     str(pfkeys$,17,1)   = hex(0e)
L42270:         mode% = 0%
                str(pfdescr$(3),62,1) = hex(84)
                if item% > 0% then L42295
                    str(pfdescr$(2),,30) = " "       /* Shut Off Prevs */
                    str(pfkeys$,3,1), str(pfkeys$,5,2) = hex(ff)
L42295:         if item% + 8% < maxlines% then L42315
                    str(pfdescr$(3),,29) = " "       /* Shut Off Nexts */
                    str(pfkeys$,4,1), str(pfkeys$,7,2) = hex(ff)

L42315:         init (hex(86)) lfac1$()
                init (hex(84)) lfac$()
                init (hex(86)) tfac$()
                init (hex(8c)) hfac$(1)
                init (hex(ac)) hfac$(2)
                tran (header$(4), hex(ac9c)) replacing
                goto L42430

            deffn'112(fieldnr%)    /* Delete Mode */
                init (hex(8c)) lfac$(), hfac$(1)
                init (hex(84)) lfac1$()
                hfac$(2) = hex(ac)
                if fieldnr% = 9999% then lfac$()         = all(hex(94))  ~
                                    else lfac$(fieldnr%) = hex(94)
                tfac$() = lfac$()
                pfdescr$(1) = "(1)Cancel Delete Request                 "~
                            & "                     (13)Instructions "
                pfdescr$(2) = "                                         "~
                            & "                     (15)Print Screen "
                pfdescr$(3) = "                                         "~
                            & "                                      "
                pfkeys$ = hex(00010d0fffffffffffffffffffffffff)

L42430: for i% = 1% to 8%
             if item% + i% <= maxlines% then L42450
                  lfac$(i%) = hex(9c)
                  tfac$(i%) = hex(9c)
L42450: next i%

        title$ = "Employee: " & emp$ & " " & emp_name$
        str(title$,62) = xtitle$
        sum$(1) = "Direct" : sum$(2) = "Overhead" : sum$(3) = "* Totals"
        sumfac$ = hex(ac)
        sum_hdr$(4)  = "  Value "
        if sw_display_rate% <> 0% then L42474
            sum_hdr$(4), sum_val$(), sum$() = " "
            sumfac$                         = hex(9c)
L42474: accept                                                           ~
               at (01,02), "Manage Labor Time Cards",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Time Card Date",                             ~
               at (04,18), fac(lfac1$(1)),  tc_date$            , ch(10),~
               at (05,02), "Clocked In-Out",                             ~
               at (05,18), fac(lfac1$(2)),  tc_in$              , ch(05),~
               at (05,24), "-",                                          ~
               at (05,26), fac(lfac1$(2)),  tc_out$             , ch(05),~
               at (06,02), "Holiday?",                                   ~
               at (06,18), fac(lfac1$(3)),  tc_hol$             , ch(01),~
               at (07,02), "Default Depart",                             ~
               at (07,18), fac(lfac1$(4)),  tc_dept$            , ch(04),~
               at (08,02), "Default Shift",                              ~
               at (08,18), fac(lfac1$(5)),  tc_shift$           , ch(01),~
                                                                         ~
               at (04,44), fac(hex(ac)),    sum_hdr$(1)         , ch(05),~
               at (04,50), fac(hex(ac)),    sum_hdr$(2)         , ch(05),~
               at (04,56), fac(hex(ac)),    sum_hdr$(3)         , ch(05),~
               at (04,73), fac(sumfac$),    sum_hdr$(4)         , ch(08),~
                                                                         ~
               at (05,35), "Direct",                                     ~
               at (05,44), fac(hex(8c)),    sum_hrs$(1)         , ch(05),~
               at (05,50), fac(hex(8c)),    sum_hrs$(2)         , ch(05),~
               at (05,56), fac(hex(8c)),    sum_hrs$(3)         , ch(05),~
               at (05,63), fac(hex(8c)),    sum$(1)             ,        ~
               at (05,73), fac(hex(8c)),    sum_val$(1)         , ch(08),~
                                                                         ~
               at (06,35), "Indirect",                                   ~
               at (06,44), fac(hex(8c)),    sum_hrs$(4)         , ch(05),~
               at (06,50), fac(hex(8c)),    sum_hrs$(5)         , ch(05),~
               at (06,56), fac(hex(8c)),    sum_hrs$(6)         , ch(05),~
               at (06,63), fac(hex(8c)),    sum$(2)             ,        ~
               at (06,73), fac(hex(8c)),    sum_val$(2)         , ch(08),~
                                                                         ~
               at (07,35), "Non-P/R ",                                   ~
               at (07,44), fac(hex(ac)),    sum_hrs$(7)         , ch(05),~
               at (07,50), fac(hex(ac)),    sum_hrs$(8)         , ch(05),~
               at (07,56), fac(hex(ac)),    sum_hrs$(9)         , ch(05),~
               at (07,73), fac(sumfac$),    sum_val$(3)         , ch(08),~
                                                                         ~
               at (08,35), "* Totals",                                   ~
               at (08,44), fac(hex(8c)),    sum_hrs$(10)        , ch(05),~
               at (08,50), fac(hex(8c)),    sum_hrs$(11)        , ch(05),~
               at (08,56), fac(hex(8c)),    sum_hrs$(12)        , ch(05),~
               at (08,63), fac(hex(8c)),    sum$(3)             ,        ~
               at (08,73), fac(hex(8c)),    sum_val$(4)         , ch(08),~
                                                                         ~
               at (12,02), fac(hex(81)), put_cursor_here$,               ~
                                                                         ~
               at (10,02), fac(hfac$(1)),   header$(3)          , ch(79),~
               at (11,02), fac(hfac$(2)),   header$(4)          , ch(79),~
                                                                         ~
               at (12,02), fac(tfac$( 1)),  line$  (item%+ 1)   , ch(04),~
               at (13,02), fac(tfac$( 2)),  line$  (item%+ 2)   , ch(04),~
               at (14,02), fac(tfac$( 3)),  line$  (item%+ 3)   , ch(04),~
               at (15,02), fac(tfac$( 4)),  line$  (item%+ 4)   , ch(04),~
               at (16,02), fac(tfac$( 5)),  line$  (item%+ 5)   , ch(04),~
               at (17,02), fac(tfac$( 6)),  line$  (item%+ 6)   , ch(04),~
               at (18,02), fac(tfac$( 7)),  line$  (item%+ 7)   , ch(04),~
               at (19,02), fac(tfac$( 8)),  line$  (item%+ 8)   , ch(04),~
                                                                         ~
               at (12,07), fac(lfac$( 1)), ld_start$  (item%+ 1), ch(05),~
               at (13,07), fac(lfac$( 2)), ld_start$  (item%+ 2), ch(05),~
               at (14,07), fac(lfac$( 3)), ld_start$  (item%+ 3), ch(05),~
               at (15,07), fac(lfac$( 4)), ld_start$  (item%+ 4), ch(05),~
               at (16,07), fac(lfac$( 5)), ld_start$  (item%+ 5), ch(05),~
               at (17,07), fac(lfac$( 6)), ld_start$  (item%+ 6), ch(05),~
               at (18,07), fac(lfac$( 7)), ld_start$  (item%+ 7), ch(05),~
               at (19,07), fac(lfac$( 8)), ld_start$  (item%+ 8), ch(05),~
                                                                         ~
               at (12,14), fac(lfac$( 1)), ld_stop$   (item%+ 1), ch(05),~
               at (13,14), fac(lfac$( 2)), ld_stop$   (item%+ 2), ch(05),~
               at (14,14), fac(lfac$( 3)), ld_stop$   (item%+ 3), ch(05),~
               at (15,14), fac(lfac$( 4)), ld_stop$   (item%+ 4), ch(05),~
               at (16,14), fac(lfac$( 5)), ld_stop$   (item%+ 5), ch(05),~
               at (17,14), fac(lfac$( 6)), ld_stop$   (item%+ 6), ch(05),~
               at (18,14), fac(lfac$( 7)), ld_stop$   (item%+ 7), ch(05),~
               at (19,14), fac(lfac$( 8)), ld_stop$   (item%+ 8), ch(05),~
                                                                         ~
               at (12,21), fac(lfac$( 1)),  ld_hrs$   (item%+ 1), ch(05),~
               at (13,21), fac(lfac$( 2)),  ld_hrs$   (item%+ 2), ch(05),~
               at (14,21), fac(lfac$( 3)),  ld_hrs$   (item%+ 3), ch(05),~
               at (15,21), fac(lfac$( 4)),  ld_hrs$   (item%+ 4), ch(05),~
               at (16,21), fac(lfac$( 5)),  ld_hrs$   (item%+ 5), ch(05),~
               at (17,21), fac(lfac$( 6)),  ld_hrs$   (item%+ 6), ch(05),~
               at (18,21), fac(lfac$( 7)),  ld_hrs$   (item%+ 7), ch(05),~
               at (19,21), fac(lfac$( 8)),  ld_hrs$   (item%+ 8), ch(05),~
                                                                         ~
               at (12,27), fac(lfac$( 1)),  ld_type$  (item%+ 1), ch(08),~
               at (13,27), fac(lfac$( 2)),  ld_type$  (item%+ 2), ch(08),~
               at (14,27), fac(lfac$( 3)),  ld_type$  (item%+ 3), ch(08),~
               at (15,27), fac(lfac$( 4)),  ld_type$  (item%+ 4), ch(08),~
               at (16,27), fac(lfac$( 5)),  ld_type$  (item%+ 5), ch(08),~
               at (17,27), fac(lfac$( 6)),  ld_type$  (item%+ 6), ch(08),~
               at (18,27), fac(lfac$( 7)),  ld_type$  (item%+ 7), ch(08),~
               at (19,27), fac(lfac$( 8)),  ld_type$  (item%+ 8), ch(08),~
                                                                         ~
               at (12,37), fac(lfac$( 1)),  ld_ot$    (item%+ 1), ch(01),~
               at (13,37), fac(lfac$( 2)),  ld_ot$    (item%+ 2), ch(01),~
               at (14,37), fac(lfac$( 3)),  ld_ot$    (item%+ 3), ch(01),~
               at (15,37), fac(lfac$( 4)),  ld_ot$    (item%+ 4), ch(01),~
               at (16,37), fac(lfac$( 5)),  ld_ot$    (item%+ 5), ch(01),~
               at (17,37), fac(lfac$( 6)),  ld_ot$    (item%+ 6), ch(01),~
               at (18,37), fac(lfac$( 7)),  ld_ot$    (item%+ 7), ch(01),~
               at (19,37), fac(lfac$( 8)),  ld_ot$    (item%+ 8), ch(01),~
                                                                         ~
               at (12,41), fac(lfac$( 1)),  ld_job$   (item%+ 1), ch(08),~
               at (13,41), fac(lfac$( 2)),  ld_job$   (item%+ 2), ch(08),~
               at (14,41), fac(lfac$( 3)),  ld_job$   (item%+ 3), ch(08),~
               at (15,41), fac(lfac$( 4)),  ld_job$   (item%+ 4), ch(08),~
               at (16,41), fac(lfac$( 5)),  ld_job$   (item%+ 5), ch(08),~
               at (17,41), fac(lfac$( 6)),  ld_job$   (item%+ 6), ch(08),~
               at (18,41), fac(lfac$( 7)),  ld_job$   (item%+ 7), ch(08),~
               at (19,41), fac(lfac$( 8)),  ld_job$   (item%+ 8), ch(08),~
                                                                         ~
               at (12,50), fac(lfac$( 1)),  ld_wc$    (item%+ 1), ch(04),~
               at (13,50), fac(lfac$( 2)),  ld_wc$    (item%+ 2), ch(04),~
               at (14,50), fac(lfac$( 3)),  ld_wc$    (item%+ 3), ch(04),~
               at (15,50), fac(lfac$( 4)),  ld_wc$    (item%+ 4), ch(04),~
               at (16,50), fac(lfac$( 5)),  ld_wc$    (item%+ 5), ch(04),~
               at (17,50), fac(lfac$( 6)),  ld_wc$    (item%+ 6), ch(04),~
               at (18,50), fac(lfac$( 7)),  ld_wc$    (item%+ 7), ch(04),~
               at (19,50), fac(lfac$( 8)),  ld_wc$    (item%+ 8), ch(04),~
                                                                         ~
               at (12,55), fac(lfac$( 1)),  ld_actv$  (item%+ 1), ch(04),~
               at (13,55), fac(lfac$( 2)),  ld_actv$  (item%+ 2), ch(04),~
               at (14,55), fac(lfac$( 3)),  ld_actv$  (item%+ 3), ch(04),~
               at (15,55), fac(lfac$( 4)),  ld_actv$  (item%+ 4), ch(04),~
               at (16,55), fac(lfac$( 5)),  ld_actv$  (item%+ 5), ch(04),~
               at (17,55), fac(lfac$( 6)),  ld_actv$  (item%+ 6), ch(04),~
               at (18,55), fac(lfac$( 7)),  ld_actv$  (item%+ 7), ch(04),~
               at (19,55), fac(lfac$( 8)),  ld_actv$  (item%+ 8), ch(04),~
                                                                         ~
               at (12,61), fac(lfac$( 1)),  ld_task$   (item%+1), ch(06),~
               at (13,61), fac(lfac$( 2)),  ld_task$   (item%+2), ch(06),~
               at (14,61), fac(lfac$( 3)),  ld_task$   (item%+3), ch(06),~
               at (15,61), fac(lfac$( 4)),  ld_task$   (item%+4), ch(06),~
               at (16,61), fac(lfac$( 5)),  ld_task$   (item%+5), ch(06),~
               at (17,61), fac(lfac$( 6)),  ld_task$   (item%+6), ch(06),~
               at (18,61), fac(lfac$( 7)),  ld_task$   (item%+7), ch(06),~
               at (19,61), fac(lfac$( 8)),  ld_task$   (item%+8), ch(06),~
                                                                         ~
               at (12,69), fac(lfac$( 1)),  ld_dept$   (item%+1), ch(04),~
               at (13,69), fac(lfac$( 2)),  ld_dept$   (item%+2), ch(04),~
               at (14,69), fac(lfac$( 3)),  ld_dept$   (item%+3), ch(04),~
               at (15,69), fac(lfac$( 4)),  ld_dept$   (item%+4), ch(04),~
               at (16,69), fac(lfac$( 5)),  ld_dept$   (item%+5), ch(04),~
               at (17,69), fac(lfac$( 6)),  ld_dept$   (item%+6), ch(04),~
               at (18,69), fac(lfac$( 7)),  ld_dept$   (item%+7), ch(04),~
               at (19,69), fac(lfac$( 8)),  ld_dept$   (item%+8), ch(04),~
                                                                         ~
               at (12,76), fac(lfac$( 1)),  ld_shift$  (item%+1), ch(01),~
               at (13,76), fac(lfac$( 2)),  ld_shift$  (item%+2), ch(01),~
               at (14,76), fac(lfac$( 3)),  ld_shift$  (item%+3), ch(01),~
               at (15,76), fac(lfac$( 4)),  ld_shift$  (item%+4), ch(01),~
               at (16,76), fac(lfac$( 5)),  ld_shift$  (item%+5), ch(01),~
               at (17,76), fac(lfac$( 6)),  ld_shift$  (item%+6), ch(01),~
               at (18,76), fac(lfac$( 7)),  ld_shift$  (item%+7), ch(01),~
               at (19,76), fac(lfac$( 8)),  ld_shift$  (item%+8), ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
                   keys(pfkeys$), key (keyhit%)

               if keyhit% <> 9% then L43345
                  gosub call_jbactsub
                  goto L42474

L43345:        if keyhit% <> 13% then L43365
                  call "MANUAL" ("JBTCINP")
                  goto L42474

L43365:        if keyhit% <> 14% then L43385
                  call "CDAQJT" (emp$, tc_dateu$)
                  goto L42474

L43385:        if keyhit% <> 15% then L43405
                  call "PRNTSCRN"
                  goto L42474

L43405:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               return

        REM *************************************************************~
            *          I N P U T  /  E D I T  S C R E E N   2           *~
            * --------------------------------------------------------- *~
            * MANAGES THE LINE DETAIL.                                  *~
            *************************************************************

        deffn'120(fieldnr%)
            init(hex(8c)) lfac$()
            pfdescr$(1) = "(1)Start Over                    (6)Same "    ~
                        & "as Previous Line      (13)Instructions"
            pfdescr$(2) = "(2)Restart Line                  (8)See R"    ~
                        & "eported Routing       (15)Print Screen"
            pfdescr$(3) = "(4)Prev Field                    (9)Work "    ~
                        & "Center Movement                       "
            pfkeys$ = hex(000102040608090d0fffffffffffffffff)
            mode% = 1%
            if c% > 1% then L44190
                str(pfdescr$(1),34,26) = " "    /* Shut Off PF-6       */
                str(pfkeys$,5,1) = hex(ff)
L44190:     if fieldnr% <> 1% then L44240
                str(pfdescr$(3), 1,14) = " "    /* Shut Off PF-4       */
                str(pfkeys$,4,1)    = hex(ff)
                str(pfdescr$(3),63) = hex(84) & "(16)Edit Mode"
                str(pfkeys$,10,1)    = hex(10)
L44240:     if ld_job$(c%) <> " " then L44270
                str(pfdescr$(2),34,26) = " "    /* Shut Off WC display */
                str(pfkeys$,6,1)       = hex(ff)
L44270:         goto L44600

        deffn'121(fieldnr%)
            if fieldnr% > 0% then L44510
            message$ = "To Modify Displayed Values (Distribution Only),"&~
                       " Tab to Field and Press RETURN."
            init(hex(86)) lfac$()
            pfdescr$(1) = "(1)Start Over                    (8)See R"    ~
                        & "eported Routing     (13)Instructions  "
            pfdescr$(2) = "(2)First Line   (4/6)Prev Lines  (9)Work "    ~
                        & "Center Movement     (15)Print Screen  "
            pfdescr$(3) = "(3)Last Line    (5/7)Next Lines (10)Reset"    ~
                        & " Line Item Rate     (16)Card Summary  "
            pfkeys$ = hex(000102040506070dff0f100308090aff)
            mode% = 1%
            if c% > 1% then L44460
                str(pfdescr$(2),15,17) = " "  /* Shut Off Prev Lines */
                str(pfdescr$(2), 1,15) = " "  /* Shut Off First Line */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L44460:     if c% < maxlines% then L44490
                str(pfdescr$(3), 1,32)   = " "   /* Shut Off Next Line */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
                                 str(pfkeys$,12,1) = hex(ff)
L44490:         goto L44600

L44510:     init(hex(8c)) lfac$()
            pfdescr$(1) = "(1)Start Over                            "    ~
                        & "                    (13)Instructions  "
            pfdescr$(2) = "                                         "    ~
                        & "                    (15)Print Screen  "
            pfdescr$(3) = "                                         "    ~
                        & "                                      "
            pfkeys$ = hex(00010d0e0fffffffffffffffffffffff)

L44600:           init(hex(8c)) lfac1$()
                  title$ = "Employee: " & emp$ & " " & emp_name$
                  str(title$,62) = xtitle$
                  convert c% to str(header$(5),27,2), pic(##)

                  on fieldnr% gosub L44790,      /* Job or Task Code    */~
                                    L44790,      /* Start/Stop, Hours   */~
                                    L44790,      /* Department Code     */~
                                    L44790,      /* Labor Class Code    */~
                                    L44790,      /* Earnings Type       */~
                                    L44790,      /* Overtime?           */~
                                    L44790,      /* Shift               */~
                                    L44800,      /* Hourly Rate         */~
                                    L44790,      /* Gross Cost Bucket   */~
                                    L44790,      /* Ovrhd Cost Bucket   */~
                                    L44790,      /* Work Center         */~
                                    L44790       /* Activity Code       */
                  goto L44820
                          lfac$(fieldnr%) = hex(80) : return  /* UpLoW */
L44790:                   lfac$(fieldnr%) = hex(81) : return  /* UPPER */
L44800:                   lfac$(fieldnr%) = hex(82) : return  /* ####s */

L44820: lfac$(21)   = lfac$(1)
        if lfac$(1) = hex(86) then lfac$(21) = hex(84)
        lfac$(22)   = lfac$(2)
        if lfac$(2) = hex(86) then lfac$(22) = hex(84)
        sum$(1) = "Direct" : sum$(2) = "Overhead" : sum$(3) = "* Totals"
        sumfac$ = hex(ac)
        sum_hdr$(4)  = "  Value "
        rate_prompt$ = "Hourly Rate"
        if sw_display_rate% <> 0% then L44870
            sum_hdr$(4), sum_val$(), ld_ext$, ld_oh$, ld_base$,          ~
            rate_prompt$, sum$()                                    = " "
            sumfac$, lfac$(8) = hex(9c)

L44870: accept                                                           ~
               at (01,02), "Manage Labor Time Cards",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Time Card Date",                             ~
               at (04,18), fac(lfac1$(1)),  tc_date$            , ch(10),~
               at (05,02), "Clocked In-Out",                             ~
               at (05,18), fac(lfac1$(2)),  tc_in$              , ch(05),~
               at (05,24), "-",                                          ~
               at (05,26), fac(lfac1$(2)),  tc_out$             , ch(05),~
               at (06,02), "Holiday?",                                   ~
               at (06,18), fac(lfac1$(3)),  tc_hol$             , ch(01),~
               at (07,02), "Default Depart",                             ~
               at (07,18), fac(lfac1$(4)),  tc_dept$            , ch(04),~
               at (08,02), "Default Shift",                              ~
               at (08,18), fac(lfac1$(5)),  tc_shift$           , ch(01),~
                                                                         ~
               at (04,44), fac(hex(ac)),    sum_hdr$(1)         , ch(05),~
               at (04,50), fac(hex(ac)),    sum_hdr$(2)         , ch(05),~
               at (04,56), fac(hex(ac)),    sum_hdr$(3)         , ch(05),~
               at (04,73), fac(sumfac$),    sum_hdr$(4)         , ch(08),~
                                                                         ~
               at (05,35), "Direct",                                     ~
               at (05,44), fac(hex(8c)),    sum_hrs$(1)         , ch(05),~
               at (05,50), fac(hex(8c)),    sum_hrs$(2)         , ch(05),~
               at (05,56), fac(hex(8c)),    sum_hrs$(3)         , ch(05),~
               at (05,63), fac(hex(8c)),    sum$(1)             ,        ~
               at (05,73), fac(hex(8c)),    sum_val$(1)         , ch(08),~
                                                                         ~
               at (06,35), "Indirect",                                   ~
               at (06,44), fac(hex(8c)),    sum_hrs$(4)         , ch(05),~
               at (06,50), fac(hex(8c)),    sum_hrs$(5)         , ch(05),~
               at (06,56), fac(hex(8c)),    sum_hrs$(6)         , ch(05),~
               at (06,63), fac(hex(8c)),    sum$(2)             ,        ~
               at (06,73), fac(hex(8c)),    sum_val$(2)         , ch(08),~
                                                                         ~
               at (07,35), "Non-P/R ",                                   ~
               at (07,44), fac(hex(ac)),    sum_hrs$(7)         , ch(05),~
               at (07,50), fac(hex(ac)),    sum_hrs$(8)         , ch(05),~
               at (07,56), fac(hex(ac)),    sum_hrs$(9)         , ch(05),~
               at (07,73), fac(sumfac$),    sum_val$(3)         , ch(08),~
                                                                         ~
               at (08,35), "* Totals",                                   ~
               at (08,44), fac(hex(8c)),    sum_hrs$(10)        , ch(05),~
               at (08,50), fac(hex(8c)),    sum_hrs$(11)        , ch(05),~
               at (08,56), fac(hex(8c)),    sum_hrs$(12)        , ch(05),~
               at (08,63), fac(hex(8c)),    sum$(3)             ,        ~
               at (08,73), fac(hex(8c)),    sum_val$(4)         , ch(08),~
                                                                         ~
               at (11,02), fac(hex(ac)),    header$(5)          , ch(79),~
                                                                         ~
               at (12,20), fac(hex(81)), put_cursor_here$,               ~
                                                                         ~
               at (12,03), "Job -or- Task",                              ~
               at (12,20), fac(lfac$(1)),  ld_job$ (c%)         , ch(08),~
               at (12,30), fac(lfac$(21)), ld_task$(c%)         , ch(06),~
               at (12,38), fac(hex(8c)),   ld_job_descr$        , ch(32),~
               at (12,71), fac(hex(8c)),   ld_type$(c%)         , ch(08),~
                                                                         ~
               at (13,03), "Start-Stop,Hours",                           ~
               at (13,20), fac(lfac$(2)),  ld_start$(c%)        , ch(05),~
               at (13,26), "-",                                          ~
               at (13,28), fac(lfac$(22)), ld_stop$(c%)         , ch(05),~
               at (13,35), fac(lfac$(22)), ld_hrs$ (c%)         , ch(05),~
                                                                         ~
               at (14,03), "Department Code",                            ~
               at (14,20), fac(lfac$(3)),  ld_dept$(c%)         , ch(04),~
                                                                         ~
               at (15,03), "Labor Class",                                ~
               at (15,20), fac(lfac$(4)),  ld_class$(c%)        , ch(04),~
                                                                         ~
               at (16,03), "Earnings Type",                              ~
               at (16,20), fac(lfac$(5)),  ld_etype$(c%)        , ch(12),~
                                                                         ~
               at (17,03), "Overtime? (Y/ )",                            ~
               at (17,20), fac(lfac$(6)),  ld_ot$(c%)           , ch(01),~
                                                                         ~
               at (18,03), "Shift",                                      ~
               at (18,20), fac(lfac$(7)),  ld_shift$(c%)        , ch(01),~
                                                                         ~
               at (13,42), fac(hex(8c)) ,  rate_prompt$,                 ~
               at (13,57), fac(lfac$(8)),  ld_rate$             , ch(10),~
               at (13,70), fac(hex(8c)) ,  ld_base$             , ch(08),~
                                                                         ~
               at (14,42), "Gross Bucket",                               ~
               at (14,57), fac(lfac$(9)),  ld_lbkt$(c%)         , ch(10),~
               at (14,70), fac(hex(8c)),   ld_ext$              , ch(08),~
                                                                         ~
               at (15,42), "Ovrhd$/Bucket",                              ~
               at (15,57), fac(lfac$(10)), ld_obkt$(c%)         , ch(10),~
               at (15,70), fac(hex(8c)),   ld_oh$               , ch(08),~
                                                                         ~
               at (17,42), "Work Center",                                ~
               at (17,57), fac(lfac$(11)),  ld_wc$(c%)          , ch(04),~
                                                                         ~
               at (18,42), "Activity Code",                              ~
               at (18,57), fac(lfac$(12)), ld_actv$(c%)         , ch(04),~
                                                                         ~
               at (19,42), fac(hex(8c)), ld_cda_prompt$,                 ~
               at (19,57), fac(hex(8c)), ld_cda$,                        ~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)            , ch(79),~
                    keys(pfkeys$), key (keyhit%)

               if keyhit% <> 8% then L45980
                  outjob$ = ld_job$(c%)
                  call "JBSEEACT" (outjob$, #19, #4, #3, #1, #7, #8)
                  goto L44820

L45980:        if keyhit% <> 9% then L46020
                  gosub call_jbactsub
                  goto L44820

L46020:        if keyhit% <> 13% then L46060
                  call "MANUAL" ("JBTCINP")
                  goto L44820

L46060:        if keyhit% <> 15% then L46100
                  call "PRNTSCRN"
                  goto L44820

L46100:        if keyhit% <> 14% then L46140
                  call "CDAQJT" (emp$, tc_dateu$)
                  goto L44820

L46140:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               return

        REM *************************************************************~
            *               S W I T C H S   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'106(fieldnr%, edit%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L48220,         /* Restrict Display  */   ~
                                L48220,         /* Modify Rates?     */   ~
                                L48220,         /* Auto-Reset Rates? */   ~
                                L48220,         /* Check Distr Hours */   ~
                                L48220,         /* Enable Earn Type? */   ~
                                L48220,         /* Enable WC?        */   ~
                                L48220          /* Enable Activity?  */
              goto L48250
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L48220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L48250:     accept                                                       ~
               at (01,02),                                               ~
                  "Time Card Entry: Manage Program Switchs",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Restrict Value Display To:",                 ~
               at (06,30), fac(lfac$( 1)), sw_user$( 1)         , ch(03),~
               at (06,34), fac(lfac$( 1)), sw_user$( 2)         , ch(03),~
               at (06,38), fac(lfac$( 1)), sw_user$( 3)         , ch(03),~
               at (06,42), fac(lfac$( 1)), sw_user$( 4)         , ch(03),~
               at (06,46), fac(lfac$( 1)), sw_user$( 5)         , ch(03),~
               at (06,50), fac(lfac$( 1)), sw_user$( 6)         , ch(03),~
               at (06,54), fac(lfac$( 1)), sw_user$( 7)         , ch(03),~
               at (06,58), fac(lfac$( 1)), sw_user$( 8)         , ch(03),~
               at (06,62), fac(lfac$( 1)), sw_user$( 9)         , ch(03),~
               at (06,66), fac(lfac$( 1)), sw_user$(10)         , ch(03),~
               at (06,70), fac(lfac$( 1)), sw_user$(11)         , ch(03),~
               at (06,74), fac(lfac$( 1)), sw_user$(12)         , ch(03),~
               at (06,78), fac(lfac$( 1)), sw_user$(13)         , ch(03),~
                                                                         ~
               at (07,02), "Are Rates Modifiable?",                      ~
               at (07,30), fac(lfac$( 2)), sw_mod_ratef$        , ch(01),~
                                                                         ~
               at (08,02), "Auto-Reset Rates?",                          ~
               at (08,30), fac(lfac$( 3)), sw_reset_rate$       , ch(01),~
                                                                         ~
               at (09,02), "Check Distribution Hours?",                  ~
               at (09,30), fac(lfac$( 4)), sw_chk_hrs$          , ch(01),~
                                                                         ~
               at (10,02), "Enable Earnings Type?",                      ~
               at (10,30), fac(lfac$( 5)), sw_etype$            , ch(01),~
                                                                         ~
               at (11,02), "Enable Work Center?",                        ~
               at (11,30), fac(lfac$( 6)), sw_wc$               , ch(01),~
                                                                         ~
               at (12,02), "Enable Activity Code?",                      ~
               at (12,30), fac(lfac$( 7)), sw_actv$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L48640
                  call "MANUAL" ("NEWSCRN ") : goto L48250

L48640:        if keyhit% <> 15 then L48670
                  call "PRNTSCRN" : goto L48250

L48670:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               return

        set_pf1
        if edit% = 2% then L48860     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L48820
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L48820:     if fieldnr% > 2% then L48840
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L48840:     return

L48860: if fieldnr% > 0% then L48950  /*  Edit Mode - Select Fld */
            message$ = "To Modify a field, position cursor and press" &  ~
                       " RETURN."
            pf$(1) = "(1)Reset Switchs                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L48950:                              /*  Edit Mode - Enabled    */
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Tests Data for the Items on Page 1.                       *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120,         /* Start Date       */~
                                    L50210          /* Employee         */
                  return

L50120
*        Test data for START DATE
            call "DATEOKC" (startdate$, 0%, errormsg$)
            if errormsg$ <> " " then return
                start$ = startdate$
                call "DATUFMTC" (start$)
                if start$ <= date then return
                     errormsg$ = "Sorry, Date can't be after today"
                     return

L50210
*        Test data for EMPLOYEE NUMBER
            call "GETEMPL" (#5, emp$, emp_name$, 1%, f1%)
            if f1% <> 0% then L50260
                if prl_on% = 1% then                                     ~
                     errormsg$ = "Employee Code Not In Personnel File"   ~
                else errormsg$ = "Employee Code Not In SFC Employee File"
                return
L50260:     get #5, emp_stat$
            if emp_stat$ = "C" then L50300
                errormsg$ = "Employee Is Not Active"
                return
L50300:     call "READ100" (#13, emp$, f1%)
            if f1% <> 0% then L50340
                if prl_on% = 1% then errormsg$ = "Employee Code Not In" &~
                                             " Payroll Master File"      ~
                else                 errormsg$ = "Employee Code Not In" &~
                                             " SFC Employee Master File"
                return
L50340:     get #13 using L50360, emp_class$, emp_dept$, emp_oh_acct$,    ~
                                 emp_base, emp_shift%
L50360:         FMT POS(13), CH(4), POS(92), CH(4), CH(9),               ~
                    POS(121), PD(14,4), BI(1)
            if emp_shift% < 1% or emp_shift% > 4% then emp_shift% = 1%
            convert emp_shift% to emp_shift$, pic(#)
            gosub load_all_cards
            errormsg$ = " "
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE TIME CARD HEADER.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub       L51075,         /* Time Card Date   */~
                                    L51245,         /* Clock In/Out     */~
                                    L51455,         /* Holiday?         */~
                                    L51500,         /* Department       */~
                                    L51535          /* Shift            */
                     return

L51075
*        Test Data for TIME CARD DATE
            call "DATEOKC" (tc_date$, 0%, errormsg$)
            if errormsg$ <> " " then return
            temp1$ = tc_date$   : call "DATUFMTC" (temp1$)
            temp2$ = startdate$ : call "DATUFMTC" (temp2$)

            if temp2$ <= temp1$ then L51095
               errormsg$ = "The Time Card Date may not be prior to "     ~
                          & startdate$ : return

L51095:     tc_dateu$ = tc_date$
            call "DATUFMTC" (tc_dateu$)
            if tc_dateu$ <= date then L51125
                errormsg$ = "Sorry, Date can't be after today"
                return

L51125:     search tc_smry$() = str(tc_dateu$,,6) to p%() step 80
            if p%(1) = 0% then L51202
                card% = (p%(1) + 79%) / 80%
                gosub restart_card
                gosub load_this_card
                if file% = 0% then L51202
                if str(tc_status$,,1) <> "C" then L51195
L51160:              temp% = 2%
                     call "ASKUSER" (temp%, "OPEN BY CDA",               ~
                        "Time Card is open for CDA Postings.  Editing",  ~
                        "will close the card to further CDA postings.",  ~
                        "Press RETURN to Continue, PF-1 to Reselect...")
                     if temp% = 0% then L51195
                     if temp% = 1% then return else L51160
L51195:         return clear all
                goto editmode

L51202:     readkey$ = str(emp$) & str(tc_dateu$,,6)
            call "READ100" (#9, readkey$, f1%)   /* JBTCBUFF */
              if f1% <> 0% then L51207
            call "READ100" (#14, readkey$, f1%)  /* JBTCMSTR */
              if f1% = 0% then L51210
L51207:       errormsg$ = "Timecard for this date exists" : return

L51210:     card% = maxcards% + 1%     /* Add a new time card          */
            tc_dept$  = emp_dept$
            tc_shift$ = emp_shift$
            call "PIPINDEX" (#12, tc_dateu$, index%, ret%)
            if ret% = 0% then tc_hol$ = h$(index%) else tc_hol$ = "N"
            return

L51245
*        Test Data for TIME CARD START/STOP TIMES
            if tc_in$ = " " and tc_out$ = " " then return

            if tc_in$ = " " then L51280
                time0$ = tc_in$
                gosub test_time : if errormsg$ <> " " then return
                tc_in$ = time0$ : tc_in = time0
L51280:     if tc_out$ = " " then L51305
                time0$ = tc_out$
                gosub test_time : if errormsg$ <> " " then return
                tc_out$ = time0$ : tc_out = time0

L51305:     if tc_in$ = " " or tc_out$ = " " then return

            if tc_in <> tc_out then L51335
                errormsg$ = "Sorry, times can't be the same."
                return

L51335
*       * Test that lines are not outside of time card times...
            if tc_out < tc_in then tc_out = tc_out + 24
            if maxlines% = 1% then L51445
                for i% = 1% to maxlines%
                   if ld_start$(i%) = " " or ld_stop$(i%)= " " then L51440
                     gosub'101 (ld_start$(i%)) :  ld_start = dec_time
                     gosub'101 (ld_stop$ (i%)) :  ld_stop  = dec_time
                     if ld_start >= tc_in then L51385
                         ld_start = ld_start + 24
                         ld_stop  = ld_stop  + 24
L51385:              if ld_stop <  ld_start then ld_stop = ld_stop + 24
                     if ld_start < tc_in or ld_start >= tc_out then L51420

                     if ld_stop <= tc_in or ld_stop >   tc_out then L51420

                     goto L51440

L51420:              convert i% to i$, pic (##)
                     errormsg$ = "Excludes time entered for line item "  ~
                                                                    &  i$
                     return
L51440:         next i%
L51445:     return

L51455
*        Test Data for HOLIDAY
            if pos("YN" = tc_hol$) > 0% then L51475
                errormsg$ = "Enter 'Y' or 'N'"
                return
L51475:     if tc_hol$ = tc_hol_old$ then return
                if sw_reset_rate$ = "Y" then gosub reset_line_rates_too
                tc_hol_old$ = tc_hol$
                return

L51500
*        Test Data for DEFAULT DEPARTMENT CODE
            if tc_dept$ = " " then return
            call "GETCODE" (#18, tc_dept$, " ", 0%, 0.30, f1%)
            if f1% = 1% then return
                errormsg$ = "Please specify a valid department code"
                return

L51535
*        Test data for Shift
            if pos("1234" = tc_shift$) <> 0% then return
                errormsg$ = "Shift must be 1 - 4"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2. (Labor Detail)        *~
            *************************************************************

            deffn'152(test%, edit%)
            errormsg$ = " "
            on test%     gosub      L52220,      /* Job -or- Task Code  */~
                                    L52530,      /* Start/Stop, Hours   */~
                                    L53320,      /* Department Code     */~
                                    L53450,      /* Labor Class         */~
                                    L53670,      /* Earnings Type       */~
                                    L53830,      /* Overtime?           */~
                                    L53910,      /* Shift               */~
                                    L54000,      /* Earnings Rate       */~
                                    L54070,      /* Gross Bucket        */~
                                    L54140,      /* Ovrhd Bucket        */~
                                    L54240,      /* Work Center         */~
                                    L54340       /* Activity Code       */
                     return

L52220
*        Test Data for JOB -or- TASK CODE
            part$, ld_job_descr$ = " "
            if ld_job$(c%) = " " then L52380
                call "GETCODE" (#1, ld_job$(c%), ld_job_descr$,          ~
                                                              1%, 0, f1%)
                if f1% = 1% then L52300
                     errormsg$ = "Job not on file"
                     return
L52300:         get #1, using L52310, part$, temp$
L52310:              FMT XX(57), CH(25), XX(70), CH(6)
                if temp$ = " " or temp$ = blankdate$ then L52350
                     errormsg$ = "Job Closed On " & temp$
                     call "DATEFMT" (str(errormsg$,15,8))
L52350:         ld_task$(c%) = " "
                ld_type$(c%) = "Direct"
                goto L52480
L52380:     REM Test for Time Card Task Code...
                call "GETCODE" (#17, ld_task$(c%), ld_job_descr$,        ~
                                                              1%, 0, f1%)
                if f1% = 1% then L52440
                     errormsg$ = "Task Code not on file"
                     return
L52440:         get #17 using L52450, ld_type$(c%)
L52450:              FMT POS(37), CH(1)
                if ld_type$(c%) = "Y" then ld_type$(c%) = "Indirect"     ~
                                      else ld_type$(c%) = "Non-P/R "
L52480:     REM If in EDIT MODE, resum lines
                gosub extend_line        /* Clear extensions for tasks */
                if edit% = 2% then sum_distr
                return

L52530
*        Test data for START/STOP TIMES, HOURS
            start, stopped, hours, in% = 0
            if ld_start$(c%) = " " then L52590
                time0$ = ld_start$(c%)
                gosub test_time : if errormsg$ <> " " then return
                ld_start$(c%) = time0$ : start = time0  : in% = in% + 4%
L52590:     if ld_stop$ (c%) = " " then L52630
                time0$ = ld_stop$(c%)
                gosub test_time : if errormsg$ <> " " then return
                ld_stop$(c%) = time0$ : stopped = time0 : in% = in% + 2%
L52630:     if ld_hrs$(c%) = " " or in% = 6% then L52720
                time0$ = ld_hrs$(c%)
                gosub test_time : if errormsg$ <> " " then return
                ld_hrs$(c%) = time0$ : hours = time0    : in% = in% + 1%

            if in% = 1% or in% = 3% or in% = 5% or in% = 6% then L52720
                errormsg$ = "Enter Start & Stop, Start or Stop and" &    ~
                            " Hours, or just Hours"
                return
L52720:     if in% = 3% then start = stopped - hours
                if start < 0 then start = 24 + start
            if in% = 5% then stopped = start + hours
                if stopped >=  24 then stopped = stopped - 24
            if in% = 6% then hours =  stopped - start
                if hours < 0 then hours = 24 + hours
            if hours > 0 then L52810
                errormsg$ = "Start time cannot equal Stop time"
                return
L52810:     ld_hrs$(c%) = "fmt"
            call "TIMEOK" (ld_hrs$(c%), hours, errormsg$)
            ld_hrs (c%) = hours
            if in% = 1% then L52900
                ld_start$(c%) = "fmt"
                call "TIMEOK" (ld_start$(c%), start  , errormsg$)
                ld_stop$ (c%) = "fmt"
                call "TIMEOK" (ld_stop$ (c%), stopped, errormsg$)

L52900
*        Check that times are within time card's range and that they
*        do not overlap another line's times...
            if in% = 1% then L53280

            if tc_in$ = " " or tc_out$ = " " then L53070
                gosub'101 (tc_in$ ) : tc_in  = dec_time
                gosub'101 (tc_out$) : tc_out = dec_time
                if tc_out  <  tc_in then tc_out  = tc_out  + 24
                if start   >= tc_in then L53010
                    start   = start   + 24
                    stopped = stopped + 24
L53010:         if stopped < start then stopped = stopped + 24
                if start   < tc_in or start   > tc_out then L53050
                if stopped < tc_in or stopped > tc_out then L53050
                goto L53070
L53050:              errormsg$ = "Times not within Clock In and Out"
                     return
L53070:     gosub'101 (ld_start$(c%)) : start   = dec_time
            gosub'101 (ld_stop$ (c%)) : stopped = dec_time
            if stopped < start then stopped = stopped + 24
            if maxlines% = 1% then L53280
                for i% = 1% to maxlines%
                   if i% = c% then goto L53260
                   if ld_start$(i%) = " " or ld_stop$(i%)= " " then L53260
                     gosub'101 (ld_start$(i%)) :  ld_start = dec_time
                     gosub'101 (ld_stop$ (i%)) :  ld_stop  = dec_time
                     if ld_stop <  ld_start then ld_stop = ld_stop + 24
                     if start   >= ld_start and start  <  ld_stop        ~
                                                               then L53230
                     if stopped >  ld_start and stopped <= ld_stop       ~
                                                               then L53230
                     goto L53260

L53230:              convert i% to i$, pic (##)
                     errormsg$ = "Overlaps time on line item " & i$
                     return
L53260:         next i%

L53280:         gosub extend_line
                gosub sum_distr
                return

L53320
*        Test Data for DEPARTMENT CODE
            call "GETCODE" (#18, ld_dept$(c%), " ", 0%, 0.30, f1%)
            if f1% = 1% then L53370
                errormsg$ = "Please specify a valid department code"
                return
L53370:     gosub load_dept_info
            if sw_reset_rate$ = "N" or edit% <> 2% then L53410
                gosub reset_rate
                gosub sum_distr
L53410:     ld_lbr_acct$(c%) = lbr_acct$
            ld_oh_acct$ (c%) = oh_acct$
            return

L53450
*        Test data for LABOR CLASS
            ld_oh_rate = 0
            if ld_class$(c%) = " " then L53620

            readkey$ = "LBR CLASS" & ld_class$(c%)
            call "PLOWCODE" (#6, readkey$, " ", 9%, 0.30, f1%)
            if f1% <> 0% then L53540
                errormsg$ = "Invalid Labor Class"
                return
L53540:     ld_class$(c%) = str(readkey$,10)
            call "READ100" (#30, str(ld_class$(c%)), f1%)
            if f1% <> 0% then L53590
                errormsg$ = "Labor Class Not Defined In Current Cost Set"
                return
L53590:     get #30 using L53600, ld_oh_rate
L53600:         FMT POS(61), PD(14,4)
            ld_oh_rate = ld_oh_rate * .01
L53620:     if edit% <> 2% then return
                gosub extend_line
                gosub sum_distr
                return

L53670
*        Test Data for EARNINGS TYPE
            if ld_etype$(c%) = " " and edit% = 1% then return
            if ld_etype$(c%) = " " then L53780

            readkey$ = str(emp$) & ld_etype$(c%)
            call "PLOWALTS" (#16, readkey$, 1%, 24%, f1%)
            if f1% <> 0% then L53770
                call "PLOWCODE" (#16, readkey$, " ", 12%, 1.00, f1%)
                if f1% <> 0% then L53770
                     errormsg$ = "Invalid Earnings Type"
                     return
L53770:     ld_etype$(c%) = str(readkey$,13)
L53780:     if sw_reset_rate$ = "Y" then gosub reset_rate
            gosub extend_line
            gosub sum_distr
            return

L53830
*        Test Data for OVERTIME?
            if pos("Y " = ld_ot$(c%)) > 0% then L53870
                errormsg$ = "Please enter 'Y' or blank."
                return
L53870:     if sw_reset_rate$ = "Y" then gosub reset_rate
            gosub sum_distr
            return

L53910
*        Test Data for SHIFT
            if pos("1234" = ld_shift$(c%)) > 0% then L53950
                errormsg$ = "Shift must be between '1' and '4'."
                return
L53950:     if sw_reset_rate$ = "N" then return
                gosub reset_rate
                gosub sum_distr
                return

L54000
*        Test Data for EARNINGS RATE
            call"NUMTEST"(ld_rate$, 0, 9999,errormsg$, -2.4, ld_rate(c%))
            if errormsg$ <> " " then return
                gosub extend_line
                gosub sum_distr
                return

L54070
*        Test Data for Gross Pay Bucket
            if ld_lbkt$(c%) = " " then L54110
                search str(buckets$()) = str(ld_lbkt$(c%)) to p%() step 10
                if p%(1) <> 0% then return
L54110:              errormsg$ = "Invalid Bucket ID"
                     return

L54140
*        Test Data for Overhead Bucket
            if ld_obkt$ (c%) <> " " then L54180
                if ld_class$(c%) = " " then return
                if ld_obkt$ (c%) = " " then return  /* was THEN 54200 */
L54180:         search str(buckets$()) = str(ld_obkt$(c%)) to p%() step 10
                if p%(1) <> 0% then return
                     errormsg$ = "Invalid Bucket ID"
                     return


L54240
*        Test Data for WORK CENTER
            if ld_wc$(c%) = "VEND" then return
            if ld_wc$(c%) = " "    then return
            if ld_wc$(c%) = "?"    then ld_wc$(c%) = " "
            call "GETCODE" (#4, ld_wc$(c%), " ", 0%, 0, f1%)
            if f1% <> 0% then return
                if ld_wc$(c%) = " " then return
                     errormsg$ = "Invalid entry for Work Center"
                     return

L54340
*        Test Data for ACTIVITY CODE
            if ld_actv$(c%) = " " then return
                if ld_actv$(c%) = "?" then ld_actv$(c%) = " "
                readkey$ = "WC ACTVTY" & ld_actv$(c%)
                call "PLOWCODE" (#6, readkey$, " ", 9%, 0.3, f1%)
                if f1% = 1% then ld_actv$(c%) = str(readkey$,10) else    ~
                            errormsg$ = "Invalid Entry For Activity Code"
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'156(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L56170,         /* Restrict Display       */~
                              L56200,         /* Modify Rates?          */~
                              L56250,         /* Auto-Reset Rates?      */~
                              L56300,         /* Check Distr Hours      */~
                              L56350,         /* Enable Earn Type?      */~
                              L56400,         /* Enable WC?             */~
                              L56450          /* Enable Activity?       */
            return

L56170: REM Test for Restrict Value Display To-   SW_USER$
            return

L56200: REM Test for Are Rates Modifiable?        SW_MOD_RATEF$
            if pos("YN" = sw_mod_ratef$) > 0% then return
                errormsg$ = "Please enter either 'Y' or 'N'"
                return

L56250: REM Test for Auto-Reset Rates?            SW_RESET_RATE$
            if pos("YN" = sw_reset_rate$) > 0% then return
                errormsg$ = "Please enter either 'Y' or 'N'"
                return

L56300: REM Test for Check Distribution Hours?    SW_CHK_HRS$
            if pos("EWN" = sw_chk_hrs$) > 0% then return
                errormsg$ = "Please enter 'E', 'W', or 'N'"
                return

L56350: REM Test for Enable Earnings Type?        SW_ETYPE$
            if pos("YN" = sw_etype$) > 0% then return
                errormsg$ = "Please enter either 'Y' or 'N'"
                return

L56400: REM Test for Enable Work Center?          SW_WC$
            if pos("YN" = sw_wc$) > 0% then return
                errormsg$ = "Please enter either 'Y' or 'N'"
                return

L56450: REM Test for Enable Activity Code?        SW_ACTV$
            if pos("YN" = sw_actv$) > 0% then return
                errormsg$ = "Please enter either 'Y' or 'N'"
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, Inc., Spokane, Wa, embodying sub-  *~
            * stantial creative efforts and confidential information.   *~
            * Unauthorized use, copying, decompiling, translating,      *~
            * disclosure, or transfer of it is prohibited.              *~
            * Copyright (C) 1988, an unpublished work by CAELUS, Inc.   *~
            * Spokane, WA.  ALL RIGHTS RESERVED.                        *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            end
