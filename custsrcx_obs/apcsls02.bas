        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCSLS02                             *~
            *  Creation Date     - 02/10/95                             *~
            *  Last Modified Date- 11/11/96                             *~
            *  Description       - This Program Creates the Salesman    *~
            *                      Commission Reports.                  *~
            *                                                           *~
            *  Code Tables Used  - (XXXXX XXX) - Code Table             *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/14/95 ! New Mod to Get Totals for Store          !     *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  !     *~
            * 03/19/98 ! Y2K modfications                         ! ERN *~
            * 05/16/00 ! Mod to use 'lineext' instead of 'linecom'! CMG *~
            *          !     in the calc. of 'grsinv' (same value !     *~
            *          !     put in them from create anyway.)     !     *~
            *          !     (EWD0001)                            !     *~
            *************************************************************

        dim                                                              ~
            blankdate$8,                 /* null date                  */~
            customer$9, cust_name$30,    /* CUSTOMER CODE AN NAME      */~
            begdate$6,                   /* BEGINNING DATE FOR THE PER */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            detail$1,                    /* DETAIL FLAG                */~
            edtmessage$79,               /* Edit screen message        */~
            end_dates$(32%)8,            /* PERIOD ENDING DATES        */~
            enddate$6,                                                   ~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invdte$6,                    /* INVOICE DATE               */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            month$2,                     /* Month                      */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$28,                  /* Miscellaneous Read/Plow Key*/~
            rpttitle$60,                 /* Report Title               */~
            savecde$1,                   /* COMM CODE CHECK VALUE      */~
            savedate$10, savecust$9,     /* INV DATE  CHECK VALUE      */~
            saveinv$8,                   /* INVOICE NUMBER CHECK VALUE */~
            savename$30,                 /* CUST NAME CHECK VALUE      */~
            savesls$4,                   /* SALESMAN NUMBER CHECK VALUE*/~
            salesman$4, slsname$30,      /* Salesman Number            */~
            shiptoname$30,               /* SHIPTO CUSTOMER NAME       */~
            start_dates$(17%)8,          /* PERIOD STARTING DATES      */~
            time$8,                      /* System Time                */~
            beg$10, end$10,              /* BEGINNING/ENDING DATES     */~
            userid$3                     /* Current User Id            */~

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim cc$24,                       /* GENCODES DESCRIPTION       */~
            ocode$3,                     /* COMM SORT CODE             */~
            pcode$(40%)3,                /* SORT CODES                 */~
            prod_d$(40)30,               /* SORT DESCRIPTIONS          */~
            invmtd(40%), commtd(40%)     /* SORT CODE AMOUNTS          */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Salesman Commission Reports       "
            pname$ = "APCSLS02 - Rev: R6.04"

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
            * #1  ! APCSLSDT ! A.P.C. Sales Analysis and Commission Det.*~
            * #2  ! GENCODES ! MASTER TABLE FILE                        *~
            * #3  ! SYSFILE2 ! CAELUS MANAGEMENT SYSTEM INFORMATION FILE*~
            * #5  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCSLSDT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =   15, keylen =  20,                     ~
                        alt key  1, keypos =  331, keylen =  36,         ~
                            key  2, keypos =  340, keylen =  27,         ~
                            key  3, keypos =  7,   keylen =  28,         ~
                            key  4, keypos =  375, keylen =  27,         ~
                            key  5, keypos =   76, keylen =  25, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #3,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #5,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$) : ret% = 0%
            call "DATUFMTC" (blankdate$)
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = all(hex(20))
            str(rpttitle$,14%,30%) = "A.P.C. Sales Commission Report"


            str(columnttl$, 1%) = "Beginning Code"
            str(columnttl$,27%) = "Ending Code"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  3%
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
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  3% then editpg1
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
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data

*       * Insert Logic here for data extraction and workfile creation
            gosub open_files
            goto generate_report

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20120,                  /* Period       */ ~
                              L20210                   /* By Salesman  */
            return

L20120: REM Specify Period                         MONTH$
            if month$              <> " " then L20190
            call "READ100" (#3,"FISCAL DATES",f1%(3))
               if f1%(3) <> 1% then L20190
               get #3 using L20170,period%
L20170:            FMT POS(159),BI(2)
               convert period% to month$,pic(##)
L20190:     return

L20210: REM Specify Salesman Code                    SALESMAN$
        REM SALESMAN$ = " "
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the Applicable Account Period for Commission Report?   ",~
         "Enter a Specific Salesman, or (ALL) for all Salesman?        ",~
         "Enter a Specific Customer, or (ALL) for all Customers?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      detail$, month$,plowkey$,savecde$,savecust$,       ~
                      saveinv$,savename$,savesls$,shiptoname$,           ~
                      slsname$, beg$, end$, salesman$,                   ~
                      customer$, cust_name$, prod_d$(), pcode$()
            savedate$ = blankdate$
            invdate$  = blankdate$
            slstot,slsdols,cdedols,cdetot,comper,savecomper,grsinv,      ~
            invcom,invdisc,invtot,linecom, lineext, linedisc = 0.0
            mat invmtd = zer
            mat commtd = zer
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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        generate_report
*       RHH
            call "SHOSTAT" ("Generating Comm Report")
*       RHH
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

            call "READ100" (#3,"FISCAL DATES",f1%(3))
                 if f1%(3%) <> 1% then end_report
                 get #3 using L30190 ,start_dates$(),end_dates$()
L30190:              FMT XX(20), XX(2), 17*CH(8), XX(2),32*CH(8)
            convert month$ to period%,data goto end_report
            begdate$ = start_dates$(period%)
            enddate$ = end_dates$(period% + 15%)
            plowkey$ = all(hex(20))
            invdate$ = blankdate$
            if str(salesman$,1%,1%) = "A" then goto L30270
               str(plowkey$,1%,4%) = salesman$

L30270:     read #1,key 3% > plowkey$, using L30380, invdte$, plowkey$,   ~
                                       shiptoname$, grsinv, invdisc,     ~
                                       linedisc, lineext, slsname$,      ~
                                       linecom, comper,                  ~
                                       eod goto report_total
            goto L30420
        report_loop
            read #1, using L30380, invdte$, plowkey$, shiptoname$,grsinv, ~
                                  invdisc, linedisc, lineext, slsname$,  ~
                                  linecom, comper,                       ~
                                  eod goto report_total
L30380:      FMT CH(6), CH(28), POS(101), CH(30), POS(173), 2*PD(14,4),  ~
                 POS(276), 2*PD(14,4), POS(293), CH(30), PD(14,4),       ~
                 POS(367), PD(14,4)

L30420:     if invdte$ < begdate$ or invdte$ > enddate$ then             ~
                                                         goto report_loop
            lineext = round(lineext, 2)                  /* (EWD0001)  */
            ocode$ = str(plowkey$,6%,3%)                  /* SORT CODE */
            if str(salesman$,1%,1%) = "A" then goto L30480
               if str(plowkey$,1%,4%) <> salesman$ then goto report_total
L30480:     if str(plowkey$,1%,4%) = savesls$ then goto L30520
              call "SHOSTAT" ("Processing Salesman ("&str(plowkey$,1%,4%)~
                       & ")")

L30520:     if str(plowkey$,1%,4%) <> savesls$ then gosub slsman_brk
            if str(plowkey$,5%,1%) <> savecde$ then gosub code_brk
            if str(plowkey$,18%,8%) <> saveinv$ then gosub inv_brk
            savecust$ = str(plowkey$,9%,9%)           /* Customer Code */
            if str(customer$,1%,3%) = "ALL" then goto L30590
               if str(customer$,1%,6%) <> str(plowkey$,9%,6%) then       ~
                                                         goto report_loop
L30590:     savename$ = shiptoname$
            savecomper = comper
            savedate$ = str(invdte$,1%,6%)
            invcom = round( lineext + invcom, 2)        /* (EWD0001)  */
            slscom = round( lineext + slscom, 2)        /* (EWD0001)  */
                                    /* RE-CALC NET INVOICE AMOUNT     */
                                    /* PRICE AFTER LINE ITEM DISCOUNT */
            grsinv  = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                    /* (EWD0001)  */
                                    /* PRICE AFTER ORDER DISCOUNT     */
            grsinv  = round(grsinv * (1.0 - (invdisc/100)), 2)

            invtot = round( invtot + grsinv, 2)
        goto report_loop
        inv_brk
            if saveinv$ <> " " then L30750
               goto L30880
L30750:     if lcntr% > 55% then gosub page_head
            if str(customer$,1%,3%) = "ALL" then goto L30820
               if str(customer$,1%,6%) = str(savecust$,1%,6%) then       ~
                                                               goto L30820
                  invcom = 0.0
                  invtot = 0.0
                  goto L30880
L30820:     call "DATFMTC" (savedate$)
            print using L60180 ,savesls$,savecust$,savename$,savedate$,   ~
                               saveinv$,invtot,savecde$,savecomper,      ~
                               invcom
            gosub build_totals
            lcntr%    = lcntr% + 1%
L30880:     saveinv$  = str(plowkey$,18%,8%)
            savedate$ = str(invdte$,1%,6%)
            cdetot    = round(cdetot + invcom, 2)
            cdedols   = round(cdedols + invtot, 2)
            invcom    = 0.0
            invtot    = 0.0
        return
        code_brk
           if savecde$ <> " " then L30990
              gosub inv_brk
              goto L31100
L30990:    gosub inv_brk
           if lcntr% < 55% then L31030
              gosub page_head
              goto L31050
L31030:    print using L60240
           lcntr% = lcntr% + 1%
L31050:    print using L60200  ,savecde$,cdedols,cdetot
           print
           lcntr% = lcntr% + 2%
           slsdols = round( slsdols + cdedols, 2)
           slstot  = round( slstot + cdetot, 2)
L31100:    cdedols = 0.0
           cdetot = 0.0
           savecde$ = str(plowkey$,5%,1%)
        return
        slsman_brk
            if savesls$ <> " " then L31180
               gosub code_brk
               goto L31270
L31180:     gosub code_brk
        REM IF LCNTR% < 55% THEN 30760
        REM    GOSUB PAGE_HEAD
        REM    GOTO 30770
            print using L60240
            print using L60220  ,savesls$,slsdols,slstot
            print
        REM LCNTR% = LCNTR% + 3%
            gosub page_head
L31270:     savesls$ = str(plowkey$,1%,4%)
            slstot = 0.0
            slsdols = 0.0
        return

        report_total
            str(plowkey$,1%,4%)  = hex(ff)
            str(plowkey$,5%,1%)  = hex(ff)
            str(plowkey$,18%,8%) = hex(ff)
            gosub slsman_brk
            gosub print_totals
        goto end_report

        end_report                /* Report Ending Routine */
            print skip(2)
            print using L60290     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "APCSLS02"
            print using L60110,month$, rpttitle$, pcntr%
            print
            print using L60140, "%"
            print using L60160
            lcntr% = 5%
            return

        print_params           /* Print Page Zero */
            print page
            tran(i$(), hex(208c2084208620ac))replacing
            print using L60270, rpttitle$
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,                /* Period      */  ~
                                L40170,                /* By Salesman */  ~
                                L40170                 /* Customer    */
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40180:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Period            :",                        ~
               at (05,25), fac(lfac$(1%)), month$               , ch(02),~
                                                                         ~
               at (06,02), "Salesman, or (ALL):",                        ~
               at (06,25), fac(lfac$(2%)), salesman$            , ch(04),~
                                                                         ~
               at (07,02), "Customer, or (ALL):",                        ~
               at (07,25), fac(lfac$(3%)), customer$            , ch(09),~
               at (07,40), fac(hex(84)), cust_name$             , ch(30),~
                                                                         ~
               at (10,02), "Beginning Date:",                            ~
               at (10,20), fac(hex(84)), beg$                   , ch(10),~
               at (10,40), "Ending Date:",                               ~
               at (10,55), fac(hex(84)), end$                   , ch(10),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L40520
                  call "PRNTSCRN" : goto L40200

L40520:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40710     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40680
                str(pf$(3%),64%)  = " " : str(pfkeys$,16%,1%) = hex(ff)
            if fieldnr% > 1% then L40690
L40680:         str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40690:     return

L40710: if fieldnr% > 0% then L40800  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40800:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50130,                 /* Period       */  ~
                              L50330,                 /* By Salesman  */  ~
                              L50450                  /* Customer     */
            return

L50130: REM Test for Month                        FMMONTH$
            convert month$ to month%, data goto L50300
            if month% < 1% then errormsg$  = "Period Must Be > than 0 "
            if month% = 13% then errormsg$ = "Period (13) is not Valid"
            if month% > 16% then errormsg$ = "Period Must Be < than 17"
            if errormsg$ <> " " then return
            call "READ100" (#3,"FISCAL DATES",f1%(3))
                 if f1%(3%) <> 1% then goto L50300
                 get #3 using L50220 ,start_dates$(),end_dates$()
L50220:              FMT XX(20), XX(2), 17*CH(8), XX(2),32*CH(8)
            convert month$ to p%, data goto L50300
            beg$ = start_dates$(p%)
            end$ = end_dates$(p% + 15%)
            call "DATFMTC" (beg$)
            call "DATFMTC" (end$)
            return

L50300:     errormsg$ = "Conversion Error On Period"
            return

L50330: REM Specified Salesman                    SALESMAN$
            if str(salesman$,1%,1%) <> "A" then goto L50370
               salesman$ = "ALL "
               return
L50370:     convert salesman$ to salesman%, data goto L50410

            convert salesman% to salesman$, pic(0000)
        return
L50410:     errormsg$ = "(Error) - Invalid Salesman Code."
            salesman$ = " "
        return

L50450: REM Specified Customer                    CUSTOMER$
            if customer$ <> " " then goto L50480
               goto L50490
L50480:     if str(customer$,1%,1%) <> "*" then goto L50520
L50490:        customer$ = "ALL      "
               cust_name$ = "(ALL) Customers"
               return
L50520:     if str(customer$,1%,3%) = "ALL" then goto L50490
            read #5,key = customer$,using L50540,cust_name$,eod goto L50560
L50540:         FMT POS(253), CH(30)
        return
L50560:     errormsg$ = "(Error) - Invalid Customer Code?"
            customer$, cust_name$ = " "
        return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %Run ########## @ ########            ###########################~
        ~#################################                 ########:RPTID

*       * Header Line 2
L60110: %Period: ##                           ###########################~
        ~#################################                     PAGE: ####

L60140: % Salesman      Cust Num            Customer                Inv D~
        ~ate    Inv No        Inv Amt      Comm CDE #   Comm Earned
L60160: % --------      --------    ------------------------------ ------~
        ~----  --------    -------------   ----------  ------------
L60180: %   ####        #########   ############################## ######~
        ~####  ########    $#,###,###.##-   #  #.####   $#,###,###.##-
L60200: %               Subtotal for Code #                              ~
        ~                $###,###,###.##-             $###,###,###.##-
L60220: %                  Total for Salesman  ####                      ~
        ~            $###,###,###,###.##-         $###,###,###,###.##-
L60240: %                                                                ~
        ~            ---------------------       ---------------------
        %** Report Title for page 0
L60270: %                                     ###########################~
        ~#################################
L60290: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        open_files
            close #1
            close #2
            close #3
            call "APCPAUSE" (apc%, "APCSLS02")
            if apc% <> 0% then goto exit_program

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
        return

        build_totals
            if str(customer$,1%,3%) = "ALL" then return
            for cnt% = 1% to 40%
                if pcode$(cnt%) <> ocode$ then L60510
                   invmtd(cnt%) = round( invmtd(cnt%) + invtot, 2)
                   commtd(cnt%) = round( commtd(cnt%) + invcom, 2)
                   cnt% = 40%
                   goto L60570
L60510:         if pcode$(cnt%) <> " " then L60570
                   pcode$(cnt%) = ocode$
                   gosub load_description
                   invmtd(cnt%) = round( invtot, 2)
                   commtd(cnt%) = round( invcom, 2)
                   cnt% = 40%
L60570:     next cnt%
        return

        load_description
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE3"
            str(cc$,10%,15%) = ocode$
            read #2,key = cc$, using L60650 ,prod_d$(cnt%), eod goto L60660
L60650:         FMT POS(25), CH(20)
L60660:     str(prod_d$(cnt%),21%,6%) = " (" & ocode$ & ")"
        return

        print_totals
            if str(customer$,1%,3%) = "ALL" then return
               t_inv, t_com = 0.0
L60720: %       <-- Sort Code Description --->       Invoice Totals      ~
        ~ Commission Totals
L60740: %       ------------------------------       --------------      ~
        ~ -----------------
L60760: %       ##############################        ####,###.##-       ~
        ~   ####,###.##-

            gosub page_head
            print using L60720
            print using L60740
            for xcnt% = 1% to 40%
                if pcode$(xcnt%) <> " " then L60860
                   xcnt% = 40%
                   goto L60900
L60860:         print using L60760,prod_d$(xcnt%)  ,invmtd(xcnt%),        ~
                                                            commtd(xcnt%)
                t_inv = round( t_inv + invmtd(xcnt%), 2)
                t_com = round( t_com + commtd(xcnt%), 2)
L60900:     next xcnt%
           print using L60740
          print using L60760,"****** Customer Totals *******",t_inv,t_com
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
