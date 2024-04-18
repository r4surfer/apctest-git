        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCCST07                             *~
            *  Creation Date     - 10/10/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - This Program Creates the Cost of     *~
            *                      Sales Analysis Reports Using the     *~
            *                      New APC Costing System. The file     *~
            *                      (APCSLSDT) has Nine (9) new Costing  *~
            *                      Fields including FOB.                *~
            *                                                           *~
            *  Code Tables Used  - (XXXXX XXX) - Code Table             *~
            *                                                           *~
            *  Special Comments  - (APCCS4SB)- By Shipto Account        *~
            *                      (APCCS5SB)- By Salesman Code, Report *~
            *                                  Card.                    *~
            *  Audit Report - Report Selection (3)  Sort Code Required  *~
            *                 RPT_VALUE$ = CCC/MMM                      *~
            *                    CCC - Sales Analysis Sort Code (Req.)  *~
            *                                                           *~
            *                    MMM - Optional-Only Select Data for the*~
            *                                   Specified Model         *~
            *                 RPT_SUM$                                  *~
            *                    "D" - Report All Product Costs         *~
            *                    "S" - Report Only Zero Cost Products   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
            *************************************************************

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES PRIMARY KEY       */~
            beg_mnth$6,                  /* START DATE FOR CUR FISCAL P*/~
            beg_year$6,                  /* START FOR CUR FISCAL YEAR  */~
            beg_mnthlyr$6,               /* START DATE FOR THIS PERIOD */~
            title$40, runtime$8,         /* LAST YEAR                  */~
            beg_lst_year$6,              /*START DATE FOR PRV FISCAL YR*/~
            chk_mnth$6,                  /* TEST VARIABLE FOR START DTE*/~
            cst(9%), ss$(10)40,          /* Costing Elements           */~
            w_cst(9%), cost$10,          /* Costing Work Elements      */~
            t_cst(9%),                   /* Costing Audit Totals       */~
            wrk_key$38,                  /* Costing Work Key           */~
            part$25, code$3, scr$10,     /* Audit Report Var's         */~
            m_code$3,                    /* Audit Specific Model       */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            end_mnth$6,                  /* ENDING DATE FOR CUR PERIOD */~
            end_year$6,                  /* END DATE FOR CUR FISCAL YR */~
            end_mnthlyr$6,               /* END DATE FOR THIS PERIOD   */~
                                         /*                  LAST YEAR */~
            end_lst_year$6,              /* END DATE FOR PRV FISCAL YR */~
            b_mnth$8, b_dte$8,           /* Beginning Date Override    */~
            e_mnth$8, e_dte$8,           /* Ending Date Override       */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            mode$5,                      /* For Work File              */~
            period$2,                    /* Period                     */~
            postdate$6,                  /* Invoice Post Date          */~
            rpt_value$9, rpt_value_d$30, /* SPECIFIED,SLS,ACCT,CUST    */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            rpttype$1, rpttype_d$25,     /* Report Type and Description*/~
            rpt_sum$1, rpt_sum_d$25,     /* Summary or Detail          */~
            sonum$9, customer$9,         /* Sales Order/Invoice Number */~
            today$6,                     /* TODAY'S DATE               */~
            userid$3                     /* Current User Id            */~

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */


/* Y2K */
        dim b_mnth10$10,                 /* 10 char input date          */~
            e_mnth10$10,                 /* 10 char input date          */~
            b_dte10$,                    /* 10 Char date                */~
            e_dte10$,                    /* 10 Char Date                */~
            begindate$8,                 /* 8 Char Begin Date (YYYYMMDD)*/~
            enddate$8                    /* 8 Char End DAte (YYYYMMDD)  */
/* Y2K */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Cost of Sales Analysis Reports    "
            pname$ = "APCCST07 - Rev: R6.04"

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
            * #1  ! APCSLSDT ! APC Sales Analysis Detail File           *~
            * #3  ! SYSFILE2 ! Caelus Management System Information     *~
            * #4  ! GENCODES ! GENERAL SYSTEM CODES FILE                *~
            * #5  ! APCCSTWK ! COST AUDIT WORK FILE                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCSLSDT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =   15, keylen =  20,                     ~
                        alt key  1, keypos =  331, keylen =  36,         ~
                            key  2, keypos =  340, keylen =  27,         ~
                            key  3, keypos =    7, keylen =  28,         ~
                            key  4, keypos =  375, keylen =  27,         ~
                            key  5, keypos =  76,  keylen = 25, dup

            select #3,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #5,  "APCCSTWK",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  36                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),100%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            ss$( 1%) = "***********************************"
            ss$( 2%) = "*     Report Type Selections      *"
            ss$( 3%) = "*                                 *"
            ss$( 4%) = "* (1) Cost Analysis by Customer   *"
            ss$( 5%) = "*                                 *"
            ss$( 6%) = "* (2) Cost Analysis by Salesman   *"
            ss$( 7%) = "*                                 *"
            ss$( 8%) = "* (3) Cost Analysis Audit Report  *"
            ss$( 9%) = "*                                 *"
            ss$(10%) = "***********************************"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
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
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
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
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub open_files

            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,             /* Report Type      */  ~
                              L20170,             /* Period           */  ~
                              L20250,             /* Specified Value  */  ~
                              L20280,             /* Summary/Detail   */  ~
                              L20310              /* Beg/End Override */
            return
L20140: REM Def/Enable Report Type                 RPTTYPE$
            return

L20170: REM Def/Enable Period                      PERIOD$
            call "READ100" (#3,"FISCAL DATES",f1%(3))
                 if f1%(3) <> 1% then L20230
            get #3 using L20210,period%
L20210:         FMT POS(159),BI(2)
            convert period% to period$,pic(00)
L20230:     return

L20250: REM Def/Enable Report Value                RPT_VALUE$
            return

L20280: REM Def/Enable Summary Detail              RPT_SUM$
            return

L20310: REM Def/Enable Beg/End Date Override       B_MNTH$, E_MNTH$
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
         "Enter Report By (1)Analyis Customer,(2)Report Card,(3)Audit? ",~
         "Enter Period 1 - 12 or 14 - 16?                              ",~
         "Enter the Applicable Salesman or Customer. (ALL)             ",~
         "Enter an (S) for Summary Report Only, (D) for Detail Report? ",~
         "Enter a Valid Beg/End Date to Override Period, or N/A?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, period$, rpttype$,         ~
                      rpt_value$, readkey$, rpt_sum$, rpt_sum_d$,        ~
                      rpttype_d$, b_mnth$, e_mnth$, b_dte$, e_dte$,      ~
                      rpt_value_d$, code$, m_code$
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

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            convert period$ to period%, data goto L31660
            convert period% to period$,pic(00)
            gencdekey$ = all(hex(20))
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = period$
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L31660
            get #4 using L31140,beg_mnth$,end_mnth$
L31140:         FMT POS(25),CH(6),XX(2),CH(6)
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = "01"
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L31660
            get #4 using L31200,beg_year$
L31200:         FMT POS(25),CH(6)
            gencdekey$ = all(hex(20))
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = "12"
            call "READ100" (#4,gencdekey$,f1%(4%))
                if f1%(4%) <> 1% then L31660
            get #4 using L31270,end_year$
L31270:         FMT POS(33),CH(6)
            str(gencdekey$,1%,9%) = "PRVPERIOD"
            str(gencdekey$,10%,2%) = period$
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L31660
            get #4 using L31330,beg_mnthlyr$,end_mnthlyr$
L31330:         FMT POS(25),CH(6),XX(2),CH(6)
            str(gencdekey$,10%,2%) = "01"
            call "READ100" (#4,gencdekey$,f1%(4))
               if f1%(4%) <> 1% then L31660
            get #4 using L31380,beg_lst_year$
L31380:         FMT POS(25),CH(6)
            str(gencdekey$,10%,2%) = "12"
            call "READ100" (#4,gencdekey$,f1%(4))
               if f1%(4%) <> 1% then L31660
            get #4 using L31270,end_lst_year$
            convert rpttype$ to rpttype%, data goto L31530

            if str(b_mnth$,1%,1%) = "N" then goto L31510

/* Y2K */

               

               /* assuming the b_dte10$ is still unformated!            */
               /* Call the fmt routine with the unformatted date        */
               /* and pass it the optional parameters                   */
               /* we can then use the return value YYYYMMDD             */
               call "DATFMTC" (b_dte10$, date%, begindate$)
               call "DATFMTC" (e_dte10$, date%, enddate$)

               /* The old code would be YYMMDD                          */
               /* The dates being passed to the subs will now be packed */
               /* so we don't have to increase the string size          */

               /* ----------------------------------------------------- */
               /* Handle Begin Date - Last Year Value                   */
               /* ----------------------------------------------------- */
               call "DATFMTC" (beg_mnthlyr$, date%, workdate8$)
               str(workdate8$,5%,4%) = str(begindate$,5%,4%)
               call "DATECONV" (workdate8$)
               beg_mnthlyr$ = workdate8$

               rem beg_mnth$ = str(b_dte$,1%,6%)
               rem str(beg_mnthlyr$,3%,4%) = str(b_dte$,3%,4%)

               /* ----------------------------------------------------- */
               /* Handle End Date - Last Year Value                     */
               /* ----------------------------------------------------- */
               call "DATFMTC" (end_mnthlyr$, date%, workdate8$)
               str(workdate8$,5%,4%) = str(enddate$,5%,4%)
               call "DATECONV" (workdate8$)
               end_mnthlyr$ = workdate8$

               rem end_mnth$ = str(e_dte$,1%,6%)
               rem str(end_mnthlyr$,3%,4%) = str(e_dte$,3%,4%)


/* Y2K */

L31510:     on rpttype% goto by_customer, by_salesman, audit_report

L31530:        return
        by_customer
            call "APCCS4SB" (#1, #4, beg_mnth$, end_mnth$, beg_year$,    ~
                             end_year$, beg_mnthlyr$, end_mnthlyr$,      ~
                             beg_lst_year$, end_lst_year$, period%,      ~
                             rpt_value$, rpt_sum$ )
            return clear all
            goto inputmode
        by_salesman
            call "APCCS5SB" (#1, #4, beg_mnth$, end_mnth$, beg_year$,    ~
                             end_year$, beg_mnthlyr$, end_mnthlyr$,      ~
                             beg_lst_year$, end_lst_year$, period%,      ~
                             rpt_value$, rpt_sum$ )
L31660:  return clear all
         goto inputmode

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
              on fieldnr% gosub L40200,         /* Report Type       */   ~
                                L40200,         /* Period            */   ~
                                L40190,         /* Report Value      */   ~
                                L40190,         /* Summary/Detail    */   ~
                                L40190          /* BEG/END MONTH     */
              goto L40220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40200:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Type (1-3) :",                        ~
               at (06,25), fac(lfac$(1%)), rpttype$             , ch(01),~
               at (06,40), fac(hex(84)), rpttype_d$             , ch(25),~
                                                                         ~
               at (07,02), "Accounting Period :",                        ~
               at (07,25), fac(lfac$(2%)), period$              , ch(02),~
                                                                         ~
               at (08,02), "Specified Value   :",                        ~
               at (08,25), fac(lfac$(3%)), rpt_value$           , ch(09),~
               at (08,40), fac(hex(84)), rpt_value_d$           , ch(30),~
                                                                         ~
               at (09,02), "(S)ummary/(D)etail:",                        ~
               at (09,25), fac(lfac$(4%)), rpt_sum$             , ch(01),~
               at (09,40), fac(hex(84)), rpt_sum_d$             , ch(25),~
                                                                         ~
               at (10,02), "Beg/End Date OverR:",                        ~
               at (10,25), fac(lfac$(5%)), b_mnth$              , ch(10),~
               at (10,40), fac(lfac$(5%)), e_mnth$              , ch(10),~
                                                                         ~
               at (11,23), fac(hex(84)), ss$( 1%)               , ch(35),~
               at (12,23), fac(hex(84)), ss$( 2%)               , ch(35),~
               at (13,23), fac(hex(84)), ss$( 3%)               , ch(35),~
               at (14,23), fac(hex(84)), ss$( 4%)               , ch(35),~
               at (15,23), fac(hex(84)), ss$( 5%)               , ch(35),~
               at (16,23), fac(hex(84)), ss$( 6%)               , ch(35),~
               at (17,23), fac(hex(84)), ss$( 7%)               , ch(35),~
               at (18,23), fac(hex(84)), ss$( 8%)               , ch(35),~
               at (19,23), fac(hex(84)), ss$( 9%)               , ch(35),~
               at (20,23), fac(hex(84)), ss$(10%)               , ch(35),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then L40690
                  call "PRNTSCRN" : goto L40220

L40690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40840
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40840:     if fieldnr% > 1% then L40860
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffff0c0dff0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,                  /* Report Type   */~
                              L50280,                  /* Period        */~
                              L50540,                  /* Report Value  */~
                              L50920,                  /* Summary/Detail*/~
                              L51060                   /* Beg/End Over  */
            return

L50150: REM Test for Report Type                  RPTTYPE$
            rpttype% = 0%
            if rpttype$ >= "1" and rpttype$ <= "3" then L50210
               errormsg$ = "Report Type Must be '1, 2, or 3' ?"
               init(" ") rpttype$, rpttype_d$
               return
L50210:     convert rpttype$ to rpttype%, data goto L50220
L50220:
            if rpttype% = 1% then rpttype_d$ = "Cost Analysis by Cust"
            if rpttype% = 2% then rpttype_d$ = "Costing Report Card  "
            if rpttype% = 3% then rpttype_d$ = "Cost Analysis Audit  "
        return

L50280: REM Test for Period                       PERIOD$
            chk_mnth$ = all(hex(20))
            today$ = all(hex(20)) : today$ = date
            convert period$ to period%, data goto L50370
            if period% < 1% or period% > 16% then errormsg$ =            ~
               "Period must be Between '1' and '16'."
            if period% = 13% then errormsg$ = "Period (13) is Invalid."
            if errormsg$ <> " " then return
            goto L50390
L50370:     errormsg$ = "Conversion Error for Period."
            return
L50390:     gencdekey$ = all(hex(20))
            convert period% to period$,pic(00)
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = period$
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L50500
            get #4 using L50460,chk_mnth$
L50460:         FMT POS(25),CH(6)
            if chk_mnth$ > today$ then errormsg$ = "Invalid Start Date "&~
                                 "Check 'CURPERIOD' table to Correct."
            return
L50500:     errormsg$ = "CURPERIOD Table does not Exist in GENCODES " &  ~
                        "Please Correct and Retry."
            return

L50540: REM Test for Report Value                 RPT_VALUE$
            if rpt_value$ <> " " then goto L50600
               if rpttype% <> 3% then goto L50590
                  errormsg$ = "(Error)-Invalid Selection for Report type"
                  goto L50890
L50590:        str(rpt_value$,1%,3%) = "ALL"
L50600:        if str(rpt_value$,1%,3%) <> "ALL" then goto L50640
                  rpt_value_d$ = "(All) Applicable Data "
                  return

L50640:     on rpttype% goto L50660, L50720, L50780

L50660: REM - (1) BY SHIP TO CUSTOMER CODE
               rpt_value_d$ = "Analysis of Specific Customer "
               if len(rpt_value$) = 6 then return
               errormsg$ = "(Error) - Invalid Customer Code?"
               goto L50890

L50720: REM - (2) BY SALESMAN CODE
               rpt_value_d$ = "Report Card Specific Salesman "
               if len(rpt_value$) = 4 then return
               errormsg$ = "(Error) - Invalid Salesman Code?"
               goto L50890

L50780: REM - (3) COSTING AUDIT REPORT
               m_code% = 0%
               code$ = str(rpt_value$,1%,3%)
               gosub lookup_sales
               if errormsg$ <> " " then return
               if len(rpt_value$) < 4 then return
                  m_code$ = str(rpt_value$,5%,3%)
                  str(rpt_value_d$,19%,12%) = " MODEL (" & m_code$ & ")"
                  convert m_code$ to m_code%, data goto L50870
L50870:
        return
L50890:   init(" ") rpt_value$, rpt_value_d$
        return

L50920: REM - SUMMARY OR DETAIL
            if rpt_sum$ <> " " then goto L50950
               rpt_sum$ = "D"
L50950:     if rpt_sum$ <> "D" and rpt_sum$ <> "S" then goto L51020
               if rpt_sum$ = "D" then rpt_sum_d$ = "Detail Report  "
               if rpt_sum$ = "S" then rpt_sum_d$ = "Summary Only   "
          if rpttype% <> 3% then return
            if rpt_sum$ = "D" then rpt_sum_d$ = "Report All Product Cost"
            if rpt_sum$ = "S" then rpt_sum_d$ = "Report Only Zero Costs "
        return
L51020:       errormsg$= "(Error) - Invalid Entry, Either S or D?"
              init(" ") rpt_sum$, rpt_sum_d$
        return


/* Y2K */
L51060: REM Beg/End Date Override               B_MNTH$, E_MNTH$
              a_date% = 0%
              if b_mnth10$ <> " " then goto L51120
L51090:          b_mnth10$ = "N/A       "
                 e_mnth10$ = "N/A       "
                 return
L51120:       if str(b_mnth10$,1%,1%) = "N" then goto L51090
                 call "DATEOK" (b_mnth10$, date%, errormsg$)
                 if date% = 0% then goto L51230
                 call "DATEOK" (e_mnth10$, date1%, errormsg$)
                 if date1% = 0% then goto L51230

              b_dte10$ = b_mnth10$ : call "DATUNFMT" (b_dte10$)
              b_dte$   = str(b_dte10$,1%,8%)

              e_dte10$ = e_mnth10$ : call "DATUNFMT" (e_dte10$)
              e_dte$   = str(e_dte10$,1%,8%)

              if b_dte$ > e_dte$ then L51230

              a_date% = 1%
        return
L51230:     init(" ") b_mnth$, e_mnth$, b_dte$, e_dte$, b_mnth10$, e_mnth10$
            errormsg$ = "(Error) - Invalid Date Range Selection?"
        return
/* Y2K */


        REM *************************************************************~
            *               F O R M A T   S T A T E M E N T S           *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55070: %! ######## @ ########                    #######################~
        ~#################                         (APCCST07) PAGE: #### !

L55100: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55130: %! MFG Part Number         ! MFG Cost !Material! Labor  !Overhead~
        ~!Freight! Vinyl  !Quantity!Tot Price!No Chg! S.O./Inv! Customer !

L55160: %!-------------------------!----------!--------!--------!--------~
        ~!-------!--------!--------!---------!------!---------!----------!

L55190: %!#########################!######.##-!####.##-!####.##-!####.##-~
        ~!###.##-!####.##-!####.##-!#####.##-!##.##-!#########! #########!

L55220: %!Total Sales: $###,###,###.##- Total Units   : ######.##-       ~
        ~                                                                !

L55250: %!Total Cost : $###,###,###.##- Total Material:#######.##- Total ~
        ~Labor:#######.##- Total Over:#######.##- Tot Frt/Vinyl:#####.##-!

L55280: %!Tot No Chg : $###,###,###.##-                                  ~
        ~                                                                !

        REM *************************************************************~
            *      S p e c i a l   P u r g e   S u b r o u t i n e      *~
            *************************************************************

        select_printer
            title$ = "**Cost Analysis Audit Report for ("&code$&")**"
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
        return

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55070, date$, runtime$, title$, pageno%
           print using L55100
           print using L55130
           lcntr% = 4%
        return

        print_detail
           if lcntr% > 58% then gosub print_header
              print using L55160
              print using L55190, part$, w_cst(1%), w_cst(2%),            ~
                                 w_cst(3%), w_cst(4%), w_cst(5%),        ~
                                 w_cst(6%), w_cst(7%), w_cst(8%),        ~
                                 w_cst(9%), sonum$, customer$
              lcntr% = lcntr% + 2%
        return

        print_totals
           if lcntr% > 53% then gosub print_header
              xy = round(t_cst(5%) + t_cst(6%), 2) /* Freight and Vinyl*/
              print using L55100
              print using L55100
              print using L55220, t_cst(8%), t_cst(7%)
              print using L55250, t_cst(1%), t_cst(2%), t_cst(3%),        ~
                                 t_cst(4%), xy
              print using L55280, t_cst(9%)
              print using L55040
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#5,mode$, 500%, f2%)
            if f2% <> 0% then goto L60540
        return
L60540:     call "SHOSTAT" ("Error - Cannot Open (APCCSTWK)") : stop
        return
        delete_work
            call "FILEBGON" (#5)
        return

        open_files
            close #1
            close #3
            close #4

            call "APCPAUSE" (apc%, "APCCST07")
            if apc% <> 0% then goto exit_program

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
        return

        audit_report
            scr$ = "[########]"
            cnt% = 0%
            call "SHOSTAT" ("Selecting Audit Data")
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            readkey$ = " "
        audit_next
            read #1,key 2% > readkey$, using L60850, postdate$, readkey$, ~
                                                    eod goto audit_done
L60850:        FMT CH(6), POS(340), CH(27)
            cnt% = cnt% + 1%
            if mod(cnt%,100) <> 0 then goto L60910
               convert cnt% to str(scr$,2%,8%), pic(########)
               print at(03,36);hex(84);scr$;

L60910:     if str(readkey$,14%,3%) <> code$ then goto audit_next

            if a_date% = 0% then goto L60980         /* Skip Date Check */
               if postdate$ < str(b_dte$,1%,6%) then goto audit_next
               if postdate$ > str(e_dte$,1%,6%) then goto audit_next
                  goto L61080

L60980:     if postdate$ < beg_year$ then goto audit_last_year
               if postdate$ > end_year$ then goto audit_next
               if postdate$ > end_mnth$ then goto audit_next
               if postdate$ < beg_mnth$ then goto audit_next
                  goto L61080
        audit_last_year
            if postdate$ < beg_lst_year$ then goto audit_next
            if postdate$ > end_mnthlyr$ then goto audit_next
            if postdate$ < beg_mnthlyr$ then goto audit_next

L61080:        get #1, using L61090, sonum$, part$, cst()
L61090:          FMT POS(35),CH(8),POS(76), CH(25), POS(402), 9*PD(14,4)
            if m_code% = 0% then goto L61130
               if m_code$ <> str(part$,1%,3%) then goto audit_next
                                             /* Only Specified Product */
L61130:     mat w_cst = zer
            sls_qty = cst(9%)
            if cst(9%) = 0 then cst(9%) = 1.0
            w_cst(1%) = round(cst(6%), 2)                /* Total Cost */
            factor = 1.0
            if cst(8%) < 0 then factor = -1.0
            w_cst(1%) = abs(w_cst(1%)) * factor
            sls_qty = abs(sls_qty) * factor
            convert w_cst(1%) to cost$, pic(######.##-)
            if rpt_sum$ = "D" then goto L61270 /* Detail Print All Cost */
                                             /* Summary Only Zero Cost */
               if w_cst(1%) <> 0 then goto audit_next
               if sls_qty = 0 then goto audit_next

L61270:     gosub update_work
            goto audit_next
        audit_done
            mat t_cst = zer
            code$ = str(rpt_value$,1%,3%)
            call "SHOSTAT" ("Printing Audit Data")
            gosub select_printer
            wrk_key$ = " "
        audit_rpt_nxt
            read #5,key > wrk_key$,using L61380, wrk_key$, w_cst(),       ~
                          sonum$, customer$, eod goto audit_rpt_done
L61380:        FMT CH(36), 9*PD(14,4), CH(9), CH(9)
            part$ = str(wrk_key$,1%,25%)
            gosub print_detail
            gosub update_totals
            goto audit_rpt_nxt
        audit_rpt_done
            gosub print_totals
            close printer
            gosub delete_work
        return clear all
        goto inputmode

        update_totals
            factor = 1.0
            if w_cst(8%) < 0 then factor = -1.0
            for i%= 1% to 6%
              xy = abs(w_cst(i%)*w_cst(7%)) * factor
              t_cst(i%) = round(t_cst(i%) + xy, 2)
            next i%
            t_cst(7%) = round(t_cst(7%) + (abs(w_cst(7%)))*factor, 2)
            t_cst(8%) = round(t_cst(8%) + (abs(w_cst(8%)))*factor, 2)
               xy = abs(w_cst(1%)*w_cst(9%)) * factor
            t_cst(9%) = round(t_cst(9%) + xy, 2)
        return

        update_work
            customer$ = str(readkey$,1%,9%)
            if len(sonum$) > 5 then goto L61690
               str(sonum$,1%,8%) = str(readkey$,17%,8%)
               str(sonum$,9%,1%) = "*"           /* Mark as an Invoice */

L61690:     wrk_key$ = " "
            str(wrk_key$,1%,25%)  = part$
            str(wrk_key$,26%,11%) = str(readkey$,17%,11%)
                                            /* W_CST(1%) =  TOTAL COST */
            w_cst(2%) = round(cst(1%), 2)              /* MATERIAL     */
              w_cst(2%) = abs(w_cst(2%)) * factor
            w_cst(3%) = round(cst(2%), 2)              /* LABOR        */
              w_cst(3%) = abs(w_cst(3%)) * factor
            w_cst(4%) = round(cst(3%), 2)              /* OVERHEAD     */
              w_cst(4%) = abs(w_cst(4%)) * factor
            w_cst(5%) = round(cst(4%), 2)              /* FREIGHT      */
              w_cst(5%) = abs(w_cst(5%)) * factor
            w_cst(6%) = round(cst(5%), 2)              /* VINYL DISC   */
              w_cst(6%) = abs(w_cst(6%)) * factor
            w_cst(7%) = round(sls_qty, 2)              /* QUANTITY     */
              w_cst(7%) = abs(w_cst(7%)) * factor
            w_cst(8%) = round(cst(8%), 2)              /* TOTAL PRICE  */
              w_cst(8%) = abs(w_cst(8%)) * factor
            if cst(8%) = 0 and sls_qty <> 0 then                         ~
               w_cst(9%) = abs(cst(9%)) * factor       /* ZERO SALE    */
                                                       /* COUNT        */
            put #5,using L61910, wrk_key$, w_cst(), sonum$, customer$
L61910:       FMT CH(36), 9*PD(14,4), CH(9), CH(9)
            write #5, eod goto L61940
        return
L61940:     call "SHOSTAT" ("(Error)-Updating-- "&wrk_key$)
            stop
        return

        lookup_sales
            init(" ") readkey$, errormsg$
            str(readkey$,1%,9%)   = "SLS CODE3"
            str(readkey$,10%,15%) = code$
            read #4,key = readkey$, using L62030, desc$, eod goto L62060
L62030:        FMT POS(25), CH(30)
            rpt_value_d$ = desc$
        return
L62060:     errormsg$ = "(Error) - Invalid Code Selection?"
            init(" ") rpt_value$, code$
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
