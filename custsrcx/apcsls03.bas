        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCSLS03                             *~
            *  Creation Date     - 02/10/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - This Program Creates the Sales       *~
            *                      Analysis Reports.                    *~
            *                                                           *~
            *  Code Tables Used  - (XXXXX XXX) - Code Table             *~
            *                                                           *~
            *  Special Comments  - (APCSLS1) - By Salesman              *~
            *                      (APCSLS2) - By Bill To Account       *~
            *                      (APCSLS3) - By Ship To Account       *~
            *                      (APCSLS4) - By Salesman Report Card  *~
            *                      (APCSLS5) - By                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/22/95 ! Mod to Allow for Either a Summay or a    ! RHH *~
            *          !   Detail Report.                         !     *~
            * 07/12/95 ! Mod to for New Report by Salesman. Report! RHH *~
            *          !   Card.                                  !     *~
            * 09/18/95 ! Mod to Sort Home Centers from Largest to ! RHH *~
            *          !   Smallest Sales Volumn. Special Report  !     *~
            *          !   version to Override Beg/End Dates other!     *~
            *          !   than Period.                           !     *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
            * 03/19/98 ! Y2K modifications                        ! ERN *~
            * 07/05/00 ! Mod to use EWDSLSDT instead of APCSLSDT  ! CMG *~
            *          !     (EWD001)                             !     *~
            *************************************************************

        dim                                                              ~
            readkey$50,                  /*                            */~
            beg_mnth$6,                  /* START DATE FOR CUR FISCAL P*/~
            beg_year$6,                  /* START FOR CUR FISCAL YEAR  */~
            beg_mnthlyr$6,               /* START DATE FOR THIS PERIOD */~
                                         /* LAST YEAR                  */~
            beg_lst_year$6,              /* START FOR PRV FISCAL YR    */~
            blankdate$10,                /* Null PD date               */~
            chk_mnth$6,                  /* TEST VARIABLE FOR START DTE*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            end_mnth$6,                  /* ENDING DATE FOR CUR PERIOD */~
            end_year$6,                  /* END DATE FOR CUR FISCAL YR */~
            end_mnthlyr$6,               /* END DATE FOR THIS PERIOD   */~
                                         /*                  LAST YEAR */~
            end_lst_year$6,              /* END DATE FOR PRV FISCAL YR */~
            b_mnth$10, b_dte$10,         /* Beginning Date Override    */~
            e_mnth$10, e_dte$10,         /* Ending Date Override       */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            period$2,                    /* Period                     */~
            rpt_value$9,                 /* SPECIFIED,SLS,ACCT,CUST    */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            rpttype$1, rpttype_d$25,     /* Report Type and Description*/~
            rpt_sum$1, rpt_sum_d$15,     /* Summary or Detail          */~
            tempdate1$10, tempdate2$10,  /* Temp date work vars        */~
            today$6,                     /* TODAY'S DATE               */~
            userid$3                     /* Current User Id            */~

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
            apc$   = "(New) Sales Analysis Reporting          "
            pname$ = "APCSLS03 - Rev: R6.04"

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
            * #1  ! EWDSLSDT ! NEW  APC Sales Analysis Detail File      *~            
            * #3  ! SYSFILE2 ! Caelus Management System Information     *~
            * #4  ! GENCODES ! GENERAL SYSTEM CODES FILE                *~
            * #5  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #6  ! SLMMASTR ! Salesman master file                     *~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
/* (EWD001)  */
            select #1, "EWDSLSDT",                                       ~
                        varc,     indexed,  recsize =  409,              ~
                        keypos =   30, keylen =   20,                    ~
                        alt key 1,keypos =  79, keylen = 45,             ~
                            key 2,keypos =  88, keylen = 36,             ~
                            key 3,keypos =  97, keylen = 27,             ~
                            key 4,keypos =   1, keylen = 49,             ~
                            key 5,keypos =  50, keylen = 29

            select #3,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #6,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4                            
                            
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),100%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))            

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            call "DATUFMTC" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

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
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Report By(1)Sls'man,(2)Acct,(3)Cust,(4)Rpt Card,(5)Spec",~
         "Enter Period 1 - 12 or 14 - 16?                              ",~
         "Enter the Applicable Slsman, Account, or Customer Code. (ALL)",~
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
                      rpttype_d$, b_mnth$, e_mnth$
            b_dte$ = blankdate$
            e_dte$ = blankdate$
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
            convert period$ to period%, data goto L31720
            convert period% to period$,pic(00)
            gencdekey$ = all(hex(20))
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = period$
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L31720
            get #4 using L31140,beg_mnth$,end_mnth$
L31140:         FMT POS(25),CH(6),XX(2),CH(6)
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = "01"
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L31720
            get #4 using L31200,beg_year$
L31200:         FMT POS(25),CH(6)
            gencdekey$ = all(hex(20))
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = "12"
            call "READ100" (#4,gencdekey$,f1%(4%))
                if f1%(4%) <> 1% then L31720
            get #4 using L31270,end_year$
L31270:         FMT POS(33),CH(6)
            str(gencdekey$,1%,9%) = "PRVPERIOD"
            str(gencdekey$,10%,2%) = period$
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L31720
            get #4 using L31330,beg_mnthlyr$,end_mnthlyr$
L31330:         FMT POS(25),CH(6),XX(2),CH(6)
            str(gencdekey$,10%,2%) = "01"
            call "READ100" (#4,gencdekey$,f1%(4))
               if f1%(4%) <> 1% then L31720
            get #4 using L31380,beg_lst_year$
L31380:         FMT POS(25),CH(6)
            str(gencdekey$,10%,2%) = "12"
            call "READ100" (#4,gencdekey$,f1%(4))
               if f1%(4%) <> 1% then L31720
            get #4 using L31270,end_lst_year$
            convert rpttype$ to rpttype%, data goto L31540

            if str(b_mnth$,1%,1%) = "N" then goto L31510
               beg_mnth$ = b_dte$
               tempdate1$ = b_dte$                      /* Packed */
               call "DATFMTC"(tempdate1$, dummy%, tempdate2$)
               str(beg_mnthlyr$,5%,4%) = str(tempdate2$,5%,4%)

               end_mnth$ = e_dte$
               tempdate1$ = e_dte$                      /* Packed */
               call "DATFMTC"(tempdate1$, dummy%, tempdate2$)
               str(end_mnthlyr$,5%,4%) = str(e_dte$,5%,4%)

L31510:     call "DATECONV" (beg_mnth$)
            call "DATECONV" (end_mnth$)
            call "DATECONV" (beg_year$)
            call "DATECONV" (end_year$)
            call "DATECONV" (beg_mnthlyr$)
            call "DATECONV" (end_mnthlyr$)
            call "DATECONV" (beg_lst_year$)
            call "DATECONV" (end_lst_year$)     

            on rpttype% goto by_salesman,by_billto,by_shipto,            ~
                             by_salesman_1, by_home_center

L31540:        return
        by_salesman
                                        /* (EWD001) - ADD CUSTOMER FILE */
            call "APCSLS1" (#1,#4,#5,beg_mnth$,end_mnth$, beg_year$,     ~
                            end_year$, beg_mnthlyr$, end_mnthlyr$,       ~
                            beg_lst_year$, end_lst_year$, period%,       ~
                            rpt_value$, rpt_sum$ )
            return
        by_billto
                                        /* (EWD001) - ADD CUSTOMER FILE */        
            call "APCSLS2" (#1,#4,#5,beg_mnth$,end_mnth$, beg_year$,     ~
                            end_year$, beg_mnthlyr$, end_mnthlyr$,       ~
                            beg_lst_year$, end_lst_year$, period%,       ~
                            rpt_value$, rpt_sum$ )
            return
        by_shipto
                                        /* (EWD001) - ADD CUSTOMER FILE */        
            call "APCSLS3" (#1,#4,#5,beg_mnth$,end_mnth$, beg_year$,     ~
                            end_year$, beg_mnthlyr$, end_mnthlyr$,       ~
                            beg_lst_year$, end_lst_year$, period%,       ~
                            rpt_value$, rpt_sum$ )
L31720:     return
        by_salesman_1
                                        /* (EWD001) - ADD SALESMAN FILE */        
            call "APCSLS4" (#1,#4,#6,beg_mnth$,end_mnth$, beg_year$,     ~
                            end_year$, beg_mnthlyr$, end_mnthlyr$,       ~
                            beg_lst_year$, end_lst_year$, period%,       ~
                            rpt_value$, rpt_sum$ )
            return
        by_home_center
            call "APCSLS5" (#1,#4,beg_mnth$,end_mnth$, beg_year$,        ~
                            end_year$, beg_mnthlyr$, end_mnthlyr$,       ~
                            beg_lst_year$, end_lst_year$, period%,       ~
                            rpt_value$, rpt_sum$ )
            return

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
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Type (1-5) :",                        ~
               at (06,25), fac(lfac$(1%)), rpttype$             , ch(01),~
               at (06,40), fac(hex(84)), rpttype_d$             , ch(25),~
                                                                         ~
               at (07,02), "Accounting Period :",                        ~
               at (07,25), fac(lfac$(2%)), period$              , ch(02),~
                                                                         ~
               at (08,02), "Specified Value   :",                        ~
               at (08,25), fac(lfac$(3%)), rpt_value$           , ch(09),~
                                                                         ~
               at (09,02), "(S)ummary/(D)etail:",                        ~
               at (09,25), fac(lfac$(4%)), rpt_sum$             , ch(01),~
               at (09,40), fac(hex(84)), rpt_sum_d$             , ch(15),~
                                                                         ~
               at (10,02), "Beg/End Date OverR:",                        ~
               at (10,25), fac(lfac$(5%)), b_mnth$              , ch(10),~
               at (10,40), fac(lfac$(5%)), e_mnth$              , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then L40580
                  call "PRNTSCRN" : goto L40220

L40580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40770     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40730
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40730:     if fieldnr% > 1% then L40750
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40750:     return

L40770: if fieldnr% > 0% then L40860  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffff0c0dff0f1000)
            return
L40860:                              /*  Edit Mode - Enabled    */
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
                              L50270,                  /* Period        */~
                              L50530,                  /* Report Value  */~
                              L50710,                  /* Summary/Detail*/~
                              L50830                   /* Beg/End Over  */
            return

L50150: REM Test for Report Type                  RPTTYPE$
            if rpttype$ >= "1" and rpttype$ <= "5" then L50200
               errormsg$ = "Report Type Must be '1,2,3,4, or 5?"
               rpttype$, rpttype_d$ = " "
               return
L50200:     if rpttype$ = "1" then rpttype_d$ = "S.A. by Salesman     "
            if rpttype$ = "2" then rpttype_d$ = "S.A. by Account Code "
            if rpttype$ = "3" then rpttype_d$ = "S.A. by Customer     "
            if rpttype$ = "4" then rpttype_d$ = "Salesman Report Card "
            if rpttype$ = "5" then rpttype_d$ = "Special Home Centers "
            return

L50270: REM Test for Period                       PERIOD$
            chk_mnth$ = all(hex(20))
            today$ = date
            convert period$ to period%, data goto L50360
            if period% < 1% or period% > 16% then errormsg$ =            ~
               "Period must be Between '1' and '16'."
            if period% = 13% then errormsg$ = "Period (13) is Invalid."
            if errormsg$ <> " " then return
            goto L50380
L50360:     errormsg$ = "Conversion Error for Period."
            return
L50380:     gencdekey$ = all(hex(20))
            convert period% to period$,pic(00)
            str(gencdekey$,1%,9%) = "CURPERIOD"
            str(gencdekey$,10%,2%) = period$
            call "READ100" (#4,gencdekey$,f1%(4%))
               if f1%(4%) <> 1% then L50490
            get #4 using L50450,chk_mnth$
L50450:         FMT POS(25),CH(6)
            call "DATECONV" (chk_mnth$)
            if chk_mnth$ > today$ then errormsg$ = "Invalid Start Date "&~
                                 "Check 'CURPERIOD' table to Correct."
            return
L50490:     errormsg$ = "CURPERIOD Table does not Exist in GENCODES " &  ~
                        "Please Correct and Retry."
            return

L50530: REM Test for Report Value                 RPT_VALUE$
            if rpt_value$ <> " " then goto L50560
               str(rpt_value$,1%,3%) = "ALL"
L50560:     if str(rpt_value$,1%,3%) = "ALL" then return
            if rpttype$ <> "1" and rpttype$ <> "4" then goto L50620
               if len(rpt_value$) = 4 then return
               errormsg$ = "(Error) - Invalid Salesman Code."
               rpt_value$ = " "
               return
L50620:     if rpttype$ <> "2" then goto L50670
               if len(rpt_value$) > 5 then return
               errormsg$ = "(Error) - Invalid Account Code."
               rpt_value$ = " "
               return
L50670:        if len(rpt_value$) > 5 then return
               errormsg$ = "(Error) - Invalid Customer Code."
               rpt_value$ = " "
               return
L50710:
        REM - SUMMARY OR DETAIL
              if rpt_sum$ <> " " then goto L50750
                 rpt_sum$ = "D"
L50750:       if rpt_sum$ <> "D" and rpt_sum$ <> "S" then goto L50790
                 if rpt_sum$ = "D" then rpt_sum_d$ = "Detail Report  "
                 if rpt_sum$ = "S" then rpt_sum_d$ = "Summary Only   "
        return
L50790:       errormsg$= "(Error) - Invalid Entry, Either S or D?"
              rpt_sum$, rpt_sum_d$ = " "
        return

L50830: REM Beg/End Date Override               B_MNTH$, E_MNTH$
              if b_mnth$ <> " " then goto L50880
L50850:          b_mnth$ = "N/A     "
                 e_mnth$ = "N/A     "
                 b_dte$ = blankdate$
                 e_dte$ = blankdate$
                 return
L50880:       if str(b_mnth$,1%,1%) = "N" then goto L50850
                 call "DATEOKC" (b_mnth$, date%, errormsg$)
                 if date% = 0% then goto L50980
                 call "DATEOKC" (e_mnth$, date1%, errormsg$)
                 if date1% = 0% then goto L50980
              b_dte$ = b_mnth$ : call "DATUFMTC" (b_dte$)
              e_dte$ = e_mnth$ : call "DATUFMTC" (e_dte$)
              if b_dte$ > e_dte$ then L50980
        return
L50980:     init(" ") b_mnth$, e_mnth$
            b_dte$ = blankdate$
            e_dte$ = blankdate$
        return

        REM *************************************************************~
            *      S p e c i a l   P u r g e   S u b r o u t i n e      *~
            *************************************************************

        open_files
            close #1
            close #3
            close #4

            call "APCPAUSE" (apc%, "APCSLS03")
            if apc% <> 0% then goto exit_program

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
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