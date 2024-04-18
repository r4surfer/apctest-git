        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC     A     CCC   RRRR    *~
            *  H   H  NN  N  Y   Y  C   C  C   C   A A   C   C  R   R   *~
            *  HHHHH  N N N   YYY   C      C      AAAAA  C      R R     *~
            *  H   H  N  NN    Y    C   C  C   C  A   A  C   C  R   R   *~
            *  H   H  N   N    Y     CCC    CCC   A   A   CCC   R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCACR - This program will analyze the cycle count      *~
            *            resource needs using the HNYCCMST file for     *~
            *            user selected session time periods.            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/23/92 ! Original                                 ! RJH *~
            * 11/17/92 ! Corrected possible convert error.        ! JDH *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 05/04/93 ! Use Count Factor Field from HNYCCMST or  ! RJH *~
            *          !  or HNYMASTER or CATEGORY to analyze     !     *~
            *          !  resource's Hours.                       !     *~
            *          ! Add a Minimum Hours per Part field.      !     *~
            *          ! If the default Count Factor was used in  !     *~
            *          !  the analysis, Display a User Message.   !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        dim                                                              ~
            category$4,                  /* Part Category Code         */~
            ccperiod$3,                  /* Cycle Count Period         */~
            cnthours(1000),              /* Person-Hours Master Array  */~
            cntperday$10,                /* Counts per Day             */~
            cntpermonth$10,              /* Counts per Month           */~
            cntperweek$10,               /* Counts per Week            */~
            cntperyear$10,               /* Counts per Year            */~
            cntperiod$3,                 /* Counts per Day             */~
            cntquan$10,                  /* Quantity of parts/Session  */~
            cntquan(1000),               /* Period Count Master Array  */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            countrate$10,                /* Count Rate per hour        */~
            counttotal$10,               /* Counts Total               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hourperday$10,               /* Counts per Day             */~
            hourpermonth$10,             /* Counts per Month           */~
            hourperweek$10,              /* Counts per Week            */~
            hourperyear$10,              /* Counts per Year            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            minhours$10,                 /* Minimum Person-hours       */~
            partstrlot$44,               /* Part/Store/Lot Key         */~
            periodtotal$10,              /* Period Total Quan to Count */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report Identifier          */~
            rpttitle$80,                 /* Report Title               */~
            session1$3,                  /* Session Number 1           */~
            session2$3,                  /* Session Number 2           */~
            session3$3,                  /* Session Number 3           */~
            session4$3,                  /* Session Number 4           */~
            seshour1$10,                 /* Session Counting Hours     */~
            seshour2$10,                 /* Session Counting Hours     */~
            seshour3$10,                 /* Session Counting Hours     */~
            seshour4$10,                 /* Session Counting Hours     */~
            sesprcnt1$5,                 /* Session Percent Count      */~
            sesprcnt2$5,                 /* Session Percent Count      */~
            sesprcnt3$5,                 /* Session Percent Count      */~
            sesprcnt4$5,                 /* Session Percent Count      */~
            sesquan1$10,                 /* Session Count              */~
            sesquan2$10,                 /* Session Count              */~
            sesquan3$10,                 /* Session Count              */~
            sesquan4$10,                 /* Session Count              */~
            sessionhdr$60,               /* Accept screen session headr*/~
            time$8,                      /* System Time                */~
            userid$3                     /* Current User Id            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
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
            * #01 ! HNYMASTR ! Inventory Part Master File               *~
            * #02 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #09 ! CATEGORY ! Inventory Category Descriptions          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90,  keylen = 4, dup

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup


            select #09, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 0%, rslt$(09%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            ret% = ret%

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "               Resource Analysis Report"
            rptid$ = "HNY052"
            sessionhdr$ = "Trial Session Number   (1)  (2)  (3)  (4)"

            str(columnttl$, 1) = " "
            str(columnttl$,27) = " "

            str(line2$,62) = "HNYCCACR: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
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
L11120:     if cursor%(1%) <> 7%  then L11126
                  fieldnr% = 1%
                  if cursor%(2) > 34 then fieldnr% = 2%
                  if cursor%(2) > 39 then fieldnr% = 3%
                  if cursor%(2) > 44 then fieldnr% = 4%
                  goto L11130
L11126:     if cursor%(1%) <> 8%  then  L11128
                  fieldnr% = 5%  : goto L11130
L11128:     if cursor%(1%) <> 9%  then fieldnr% = 0%                     ~
                                  else fieldnr% = 6%
L11130:     if fieldnr% < 1% or fieldnr% >  6% then editpg1
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

           /* Build the Master Array for which each element represents */
           /* the total Quantity of parts in inventory for a counting  */
           /* period.  ie. Array is TotalPeriodQuan(Period)            */
            gosub build_master_array

           /* ** Print the Report ** */
            gosub generate_report

            if dfltcntr% > 0% then gosub default_used_message
            goto inputmode

           /* *****  END  Extract Data Sub **** */


        build_master_array

            call "SHOSTAT" ("Processing Data")
            counter% = 0% : mincntperiod = 99999999 : maxcntperiod = 0
            dfltcntr% = 0%
            plowkey$ = all(hex(00))

            call "READ104" (#03, plowkey$, f1%(03))
            goto L13630

        loop_master      /* Loop Poin */
            call "READNEXT" (#03, f1%(03))
L13630:     if f1%(03) = 0% then return
            get #03  using L35060, partstrlot$, cntperiod$, count_factor
            cntperiod = 0
            convert cntperiod$ to cntperiod, data goto L13670
L13670:     cntperiod% = int(cntperiod)
            if cntperiod% = 0% then goto loop_master

            /* ** Get Quantity ** */
            readkey$ = partstrlot$
            call "READ100" (#02, readkey$, f1%(02))
            if f1%(02) <> 0% then L13770
                   call "SHOSTAT" ("NO HNYQUAN RECORD TO MATCH HNYCCMST")
                   goto loop_master
L13770:     get #02  using L35100, quan

            /* ** Calc. Total Counts per Day ** */
            if quan < 1 then  quan = 1
            cntperday = quan / cntperiod
            totcntperday = totcntperday + cntperday
            call "CONVERT" (cntperday, 4.4, cntperday$)
            counter% = counter% + 1%

            if mincntperiod > cntperiod then  mincntperiod = cntperiod
            if maxcntperiod < cntperiod then  maxcntperiod = cntperiod

            call "CONVERT" (cntperiod, 0.01, cntperiod$)

            if f1%(03) = 0% then return

            cntquan(cntperiod%) = cntquan(cntperiod%) + quan

            if count_factor <= 0.0 then gosub check_category_count_factor
            if count_factor >  0.0 then L13980
                 count_factor = countrate  :  dfltcntr% = dfltcntr% + 1%
            if count_factor <= 0.0 then goto loop_master

L13980:     temphours = quan/count_factor
            if temphours < minhours then temphours = minhours
            cnthours(cntperiod%) = cnthours(cntperiod%) +  temphours


            goto loop_master

          /* *** End BUILD_MASTER_WORKFIL0 Sub Function *** */

        check_category_count_factor
            count_factor = 0
            call "READ100" (#1, str(partstrlot$,,25%), f1%(1%))
            if f1%(1%) = 0% then return
            get #1 using L14200, category$, count_factor
            if count_factor > 0.0 then return
            call "READ100" (#9, category$, f1%(9%))
            if f1%(9%) = 0% then return
            get #9 using L14210, count_factor
            return
L14200: FMT POS(90), CH(4), POS(119), PD(14,4)   /* Hnymastr */
L14210: FMT POS(53), PD(14,4)                    /* Category */

        default_used_message
            dfltcntr = dfltcntr%
            call "CONVERT" (dfltcntr, -0.01, sesquan1$)

            keyhit% = 2%
            call "ASKUSER" (keyhit%, "**** COUNT RATE DEFAULT ****",     ~
                " The Default Count Rate was used " & hex(80) & sesquan1$~
                &  " times.",                                            ~
                "Press Any Key to Continue")

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Session Number 1       */~
                              L20200,         /* Session Number 2       */~
                              L20300,         /* Session Number 3       */~
                              L20400,         /* Session Number 4       */~
                              L20500,         /* Count rate per hour    */~
                              L20600          /* Minimum Manhours       */
            return
L20100: REM Def/Enable Session Number 1              SESSION1$


            return

L20200: REM Def/Enable Session Number 2              SESSION2$


            return

L20300: REM Def/Enable Session Number 3              SESSION3$

            return

L20400: REM Def/Enable Session Number 4              SESSION4$


            return

L20500: REM Def/Enable Count Rate                    COUNTRATE$


            return

L20600: REM Def/Enable Minimum Person-hours per Part   MINHOURS$
            if minhours$ > " " then return
            minhours$ = "0.25"
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
         "Enter Session Number 1 Period (days)                         ",~
         "Enter Session Number 2 Period (days)                         ",~
         "Enter Session Number 3 Period (days)                         ",~
         "Enter Session Number 4 Period (days)                         ",~
         "Enter Default Hourly Count rate (Pieces per hour)            ",~
         "Enter Minimum Number of Person-hours per Part                "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, ccperiod$, cntquan$,       ~
                      session1$,  session2$, session3$,  session4$,      ~
                      countrate$, minhours$
            mincntperiod, maxcntperiod, totcntperday, counttotal,        ~
            sesquantot1, sesquantot2, sesquantot3, sesquantot4,          ~
            cntperweektot, cntpermonthtot, cntperyeartot, cntperdaytot = 0

            mat cntquan  = zer
            mat cnthours = zer

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

            quantot, hourstot, hoursperdaytot = 0
            call "SHOSTAT" ("PRINTING REPORT NOW")

            /* ** Begin Report Loop  ** */

            maxcntperiod% = int(maxcntperiod)

          /* ** START Report Loop ***/
            for cntperiod% = 1% to maxcntperiod%

            periodtotal = cntquan(cntperiod%)
            if periodtotal = 0 then goto L30750   /* ie. NEXT CNTPERIOD */

            lcntr% = lcntr% + 1%
            if lcntr% < 57 then L30350
                gosub page_head   :  gosub column_head

L30350:     quantot = quantot + periodtotal
            cntperiod = cntperiod%
            cntperday = periodtotal / cntperiod

            sesquan1 = cntperday * session1
            sesquan2 = cntperday * session2
            sesquan3 = cntperday * session3
            sesquan4 = cntperday * session4

            sesprcnt1 = 100 * session1 / cntperiod
            sesprcnt2 = 100 * session2 / cntperiod
            sesprcnt3 = 100 * session3 / cntperiod
            sesprcnt4 = 100 * session4 / cntperiod

            cntperweek  = cntperday * 7.0
            cntpermonth = cntperday * 30.0
            cntperyear  = cntperday * 365.0

            call "CONVERT" (cntperiod  , 0.01, cntperiod$  )
            call "CONVERT" (periodtotal, 0.01, periodtotal$)
            call "CONVERT" (sesquan1   ,  2.2, sesquan1$   )
            call "CONVERT" (sesquan2   ,  2.2, sesquan2$   )
            call "CONVERT" (sesquan3   ,  2.2, sesquan3$   )
            call "CONVERT" (sesquan4   ,  2.2, sesquan4$   )
            call "CONVERT" (sesprcnt1  ,  1.1, sesprcnt1$  )
            call "CONVERT" (sesprcnt2  ,  1.1, sesprcnt2$  )
            call "CONVERT" (sesprcnt3  ,  1.1, sesprcnt3$  )
            call "CONVERT" (sesprcnt4  ,  1.1, sesprcnt4$  )
            call "CONVERT" (cntperday  , 0.01, cntperday$  )
            call "CONVERT" (cntperweek , 0.01, cntperweek$ )
            call "CONVERT" (cntpermonth, 0.01, cntpermonth$)
            call "CONVERT" (cntperyear , 0.01, cntperyear$ )

            if sesprcnt1 <= 100.0 then goto L30550
                sesprcnt1$ = "-N/A-"
                sesquan1   = 0.0
                sesquan1$  = "     -----"

L30550:     if sesprcnt2 <= 100.0 then goto L30600
                sesprcnt2$ = "-N/A-"
                sesquan2   = 0.0
                sesquan2$  = "     -----"

L30600:     if sesprcnt3 <= 100.0 then goto L30645
                sesprcnt3$ = "-N/A-"
                sesquan3   = 0.0
                sesquan3$  = "     -----"

L30645:     if sesprcnt4 <= 100.0 then goto L30695
                sesprcnt4$ = "-N/A-"
                sesquan4   = 0.0
                sesquan4$  = "     -----"

L30695:     call "RJUSTIFY" (cntperiod$)
            print using L60190, cntperiod$, periodtotal$,                 ~
                            sesquan1$, sesprcnt1$, sesquan2$, sesprcnt2$,~
                            sesquan3$, sesprcnt3$, sesquan4$, sesprcnt4$,~
                           cntperday$,cntperweek$,cntpermonth$,cntperyear$

            gosub add_to_totals
L30750:     next cntperiod%

            /* *** End Print Loop *** */

            gosub print_totals
            gosub print_hourly
            goto end_report

        add_to_totals
            counttotal = counttotal + periodtotal
            sesquantot1 = sesquantot1 + sesquan1
            sesquantot2 = sesquantot2 + sesquan2
            sesquantot3 = sesquantot3 + sesquan3
            sesquantot4 = sesquantot4 + sesquan4

            cntperweektot  = cntperweektot + cntperweek
            cntpermonthtot = cntpermonthtot + cntpermonth
            cntperyeartot  = cntperyeartot  + cntperyear
            cntperdaytot   = cntperdaytot  + cntperday

            return

        column_head
            call "RJUSTIFY" (session1$)
            call "RJUSTIFY" (session2$)
            call "RJUSTIFY" (session3$)
            call "RJUSTIFY" (session4$)

            print using L60135, session1$, session2$, session3$, session4$
            print using L60155
            print using L60175
            lcntr% = lcntr% + 3%
            return

        print_totals
            call "CONVERT" (counttotal, 0.01, counttotal$)
            call "CONVERT" (sesquantot1   ,  2.2, sesquan1$   )
            call "CONVERT" (sesquantot2   ,  2.2, sesquan2$   )
            call "CONVERT" (sesquantot3   ,  2.2, sesquan3$   )
            call "CONVERT" (sesquantot4   ,  2.2, sesquan4$   )
            call "CONVERT" (cntperdaytot  , 0.01, cntperday$  )
            call "CONVERT" (cntperweektot , 0.01, cntperweek$ )
            call "CONVERT" (cntpermonthtot, 0.01, cntpermonth$)
            call "CONVERT" (cntperyeartot , 0.01, cntperyear$ )

            print using L60430
            print using L60190, " ", counttotal$, sesquan1$, " ",         ~
                           sesquan2$," ", sesquan3$, " ", sesquan4$, " ",~
                           cntperday$,cntperweek$,cntpermonth$,cntperyear$

            print
            return

        print_hourly
            gosub column_head_hours

          /* ** START Hourly Report Loop ***/
            for cntperiod% = 1% to maxcntperiod%

            periodtotal = cntquan(cntperiod%)
            if periodtotal = 0 then goto L32580   /* ie. NEXT CNTPERIOD */

            lcntr% = lcntr% + 1%
            if lcntr% < 57 then L32115
                gosub page_head   :  gosub column_head_hours

L32115:     quantot     = quantot + periodtotal
            cntperiod   = cntperiod%
            cntperday   = periodtotal / cntperiod
            hourstot    = hourstot + cnthours(cntperiod%)
            hoursperday = cnthours(cntperiod%) / cntperiod
            hoursperdaytot = hoursperdaytot + hoursperday

            seshour1 = hoursperday * session1
            seshour2 = hoursperday * session2
            seshour3 = hoursperday * session3
            seshour4 = hoursperday * session4

            sesprcnt1 = 100 * session1 / cntperiod
            sesprcnt2 = 100 * session2 / cntperiod
            sesprcnt3 = 100 * session3 / cntperiod
            sesprcnt4 = 100 * session4 / cntperiod

            hourperweek  = hoursperday * 7.0
            hourpermonth = hoursperday * 30.0
            hourperyear  = hoursperday * 365.0

            seshourtot1 = seshourtot1 + seshour1
            seshourtot2 = seshourtot2 + seshour2
            seshourtot3 = seshourtot3 + seshour3
            seshourtot4 = seshourtot4 + seshour4

            call "CONVERT" (cntperiod   , 0.01, cntperiod$  )
            call "CONVERT" (periodtotal , 0.01, periodtotal$)
            call "CONVERT" (seshour1    ,  2.2, seshour1$   )
            call "CONVERT" (seshour2    ,  2.2, seshour2$   )
            call "CONVERT" (seshour3    ,  2.2, seshour3$   )
            call "CONVERT" (seshour4    ,  2.2, seshour4$   )
            call "CONVERT" (hoursperday ,  2.2, hourperday$  )
            call "CONVERT" (hourperweek ,  2.2, hourperweek$ )
            call "CONVERT" (hourpermonth,  2.2, hourpermonth$)
            call "CONVERT" (hourperyear ,  2.2, hourperyear$ )

            if sesprcnt1 <= 100.0 then L32340
                seshour1$ = "     -N/A-"
                seshour1  = 0.0

L32340:     if sesprcnt2 <= 100.0 then L32380
                seshour2$ = "     -N/A-"
                seshour2  = 0.0

L32380:     if sesprcnt3 <= 100.0 then L32420
                seshour3$ = "     -N/A-"
                seshour3  = 0.0

L32420:     if sesprcnt4 <= 100.0 then L32510
                seshour4$ = "     -N/A-"
                seshour4  = 0.0

L32510:     call "RJUSTIFY" (cntperiod$)
            print using L60350, cntperiod$, periodtotal$,                 ~
                            seshour1$, seshour2$, seshour3$, seshour4$,  ~
                            hourperday$, hourperweek$,                   ~
                            hourpermonth$, hourperyear$
L32580:     next cntperiod%

            /* *** End Print Loop *** */

            gosub print_totals_hours
            return

        column_head_hours
            if lcntr% > 50 then gosub page_head

            print skip (3)
            print using L60204
            print
            print using L60230, session1$, session2$, session3$, session4$
            print using L60270
            print using L60310
            lcntr% = lcntr% + 8%
            return

        print_totals_hours
            call "CONVERT" (seshourtot1 ,  2.2, sesquan1$   )
            call "CONVERT" (seshourtot2 ,  2.2, sesquan2$   )
            call "CONVERT" (seshourtot3 ,  2.2, sesquan3$   )
            call "CONVERT" (seshourtot4 ,  2.2, sesquan4$   )
            call "CONVERT" (hoursperdaytot *   1.0, 2.2, cntperday$  )
            call "CONVERT" (hoursperdaytot *   7.0, 2.2, cntperweek$ )
            call "CONVERT" (hoursperdaytot *  30.0, 2.2, cntpermonth$)
            call "CONVERT" (hoursperdaytot * 356.0, 2.2, cntperyear$ )

            print using L60390
            print using L60190, " ", counttotal$, sesquan1$, " ",         ~
                           sesquan2$, " ",sesquan3$, " ", sesquan4$, " ",~
                           cntperday$,cntperweek$,cntpermonth$,cntperyear$

            print
            return

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64990, time$  /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCACR", rptid$
            print using L60110, rpttitle$, pcntr%
            print
            if pcntr% = 0% then gosub print_params
            lcntr% = 3%
            return

        print_params           /* Print Page Zero */
L34510:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34550
                str(i$(), i%, 1%) = hex(20)
                goto L34510
L34550:     print skip(3)
            print tab(26);
            print "----------------------Simulation Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 19% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            gosub page_head :  gosub column_head
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************
            /* ** WORKFIL0 for Period  Report ** */
            FMT CH(3),    PD(14,4)

            /* HNYCCMST File */
L35060:     FMT CH(44), POS(181), CH(3), POS(402), PD(14,4)

            /* HNYQUAN */
L35100:     FMT POS(69) , PD(14,4)

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
              on fieldnr% gosub L40100,         /* Session Number 1  */   ~
                                L40100,         /* Session Number 2  */   ~
                                L40100,         /* Session Number 3  */   ~
                                L40100,         /* Session Number 4  */   ~
                                L40100,         /* Count Rate        */   ~
                                L40100          /* Minimum Manhours  */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40100:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Analyze Counting Resource Requirements",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,07), fac(hex(ac)), sessionhdr$            , ch(42),~
                                                                         ~
               at (07,02), "Proposed Session Period",                    ~
               at (07,30), fac(lfac$( 1)),   session1$          , ch(03),~
               at (07,35), fac(lfac$( 2)),   session2$          , ch(03),~
               at (07,40), fac(lfac$( 3)),   session3$          , ch(03),~
               at (07,45), fac(lfac$( 4)),   session4$          , ch(03),~
                                                                         ~
               at (08,02), "Default Count Rate          ",               ~
               at (08,30), fac(lfac$( 5)),   countrate$         , ch(10),~
                                                                         ~
               at (09,02), "Min Hour/Part",                              ~
               at (09,30), fac(lfac$( 6%)), minhours$           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40315
                  call "MANUAL" ("HNYCCACR") : goto L40110

L40315:        if keyhit% <> 15 then L40330
                  call "PRNTSCRN" : goto L40110

L40330:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40425     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40410
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40415
L40410:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40415:     return

L40425: if fieldnr% > 0% then L40470  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40470:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50100,         /* Session Number 1       */~
                              L50200,         /* Session Number 2       */~
                              L50300,         /* Session Number 3       */~
                              L50400,         /* Session Number 4       */~
                              L50500,         /* Count Rate             */~
                              L50600          /* Minimum Person Hours   */
            return
L50100: REM Test for Session Number 1             SESSION1$
            if session1$ <= " " then session1$ = "0"
            convert session1$ to session1
            if session1 < 0 then errormsg$ = "Session Must be Positive"
            return

L50200: REM Test for Session Number 2             SESSION2$
            if session2$ <= " " then session2$ = "0"
            convert session2$ to session2
            if session2 < 0 then errormsg$ = "Session Must be Positive"
            return

L50300: REM Test for Session Number 3             SESSION3$
            if session3$ <= " " then session3$ = "0"
            convert session3$ to session3
            if session3 < 0 then errormsg$ = "Session Must be Positive"
            return

L50400: REM Test for Session Number 4             SESSION4$
            if session4$ <= " " then session4$ = "0"
            convert session4$ to session4
            if session4 < 0 then errormsg$ = "Session Must be Positive"
            return

L50500: REM Test for Count Rate                   COUNTRATE$
            if countrate$ <> " " then L50520
                countrate$ = "0"
                return
L50520:     countrate = 0
            convert countrate$ to countrate, data goto L50530
L50530:     if countrate <= 0 then                                       ~
                errormsg$ = "Enter a Positive Number        "
            return

L50600: REM Test for Minimum Person hours             MINHOURS$
            if minhours$ <> " " then  L50620
                minhours$ = "0.00"
                return
L50620:     convert minhours$ to minhours, data goto L50635
            call "CONVERT" (minhours, -2.2, minhours$)
            if minhours >= 0.0 then return
L50635:         errormsg$ = "Enter a Positive Number        "
            return


        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Column Header Line 1
L60135:  %CNT      TOTAL  ### DAY SESSION  ### DAY SESSION  ### DAY SESSI~
        ~ON  ### DAY SESSION       COUNTS     COUNTS     COUNTS     COUNTS

*       * Column Header Line 2
L60155: %PRD     COUNTS      COUNTS PRCNT     COUNTS PRCNT     COUNTS PRC~
        ~NT     COUNTS PRCNT     PER DAY   PER WEEK  PER MONTH   PER YEAR

*       * Column Header Line 3
L60175: %--- ----------  ---------- ----- ---------- ----- ---------- ---~
        ~-- ---------- -----  ---------- ---------- ---------- ----------

*       * Report Line
L60190: %### ##########  ########## ##### ########## ##### ########## ###~
        ~## ########## #####  ########## ########## ########## ##########

*       * Header Hours Line
L60204: %                                                     HOURLY ANAL~
        ~YSIS

*       * Column Hours Header Line 1
L60230:  %CNT      TOTAL ### DAY SES      ### DAY SES      ### DAY SES   ~
        ~   ### DAY SES             HOURS      HOURS      HOURS      HOURS

*       * Column Hours Header Line 2
L60270: %PRD     COUNTS       HOURS            HOURS            HOURS    ~
        ~        HOURS           PER DAY   PER WEEK  PER MONTH   PER YEAR

*       * Column Hours Header Line 3
L60310: %--- ----------  ----------       ----------       ----------    ~
        ~   ----------        ---------- ---------- ---------- ----------

*       * Report Hours Line
L60350: %### ##########  ##########       ##########       ##########    ~
        ~   ##########        ########## ########## ########## ##########

*       * Column Hours Subline
L60390: %    ----------  ----------       ----------       ----------    ~
        ~   ----------        ---------- ---------- ---------- ----------

*       * Column Subline
L60430: %    ----------  ---------- ----- ---------- ----- ---------- ---~
        ~-- ---------- -----  ---------- ---------- ---------- ----------

        %** Report Title for page 0
        %############################################################

L64990: %                            * * * * * * * * * *   E N D   O  F  ~
        ~  R E P O R T   @  ########  * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
