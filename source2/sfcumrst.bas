        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   FFFFF   CCC   U   U  M   M  RRRR    SSS   TTTTT   *~
            *  S      F      C   C  U   U  MM MM  R   R  S        T     *~
            *   SSS   FFFF   C      U   U  M M M  RRRR    SSS     T     *~
            *      S  F      C   C  U   U  M   M  R   R      S    T     *~
            *   SSS   F       CCC    UUU   M   M  R   R   SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SFCUMRST - This program resets the cumulative forcast from*~
            *            the designated date, forward, using the BCKLINE*~
            *            and DEMMASTR Files.  If a Part met the Range   *~
            *            Filtering criteria but was not reset in the    *~
            *            SFCUM2 File (No hits in BCK or DEMMASTR) it    *~
            *            will be reset to Zero.                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/19/94 ! Original                                 ! RJH *~
            * 08/23/96 ! Century date conversion                  ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            alloc_flag$1,                /* SO Allocation Flag         */~
            blankdate$8,                 /* blankdate                  */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cumf%(490),                  /* Cumulative Forecast Area   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            day1$6,                      /* Ist Day of Planning Calndr */~
            demand$1,                    /* Demand Type                */~
            demdate$8,                   /* Date of  Demand            */~
            demqty$10,                   /* Demand Quantity            */~
            demstatus$1,                 /* Demand Shipment Date       */~
            edtmessage$79,               /* Edit screen message        */~
            end_day$6,                   /* Planning Calander End Day  */~
            errormsg$79,                 /* Error message              */~
            fmcat$4,                     /* Part Category              */~
            fmmps$8,                     /* MPS Group                  */~
            fmpart$25,                   /* Part Number Range          */~
            hicat$4,                     /* Part Category              */~
            himps$8,                     /* MPS Group                  */~
            hipart$25,                   /* Part Number Range          */~
            hnycat$4,                    /* Part Category frm Hnymastr */~
            hnytype$3,                   /* Part Type     frm Hnymastr */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            locat$4,                     /* Part Category              */~
            lomps$8,                     /* MPS Group                  */~
            lopart$25,                   /* Part Number Range          */~
            mpsgroup$8,                  /* MPS Group                  */~
            over_day$8,                  /* Cum Forcast Over Date      */~
            over_qty$10,                 /* Cum Forcast Over Quantiy   */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Number Description    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            print_over$1,                /* Print Report Flag          */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            reset_date$8,                /* Reset Date                 */~
            reqdship$8,                  /* Required SO Ship Date      */~
            roll_over$1,                 /* Roll Over Performance Flag */~
            roll_under$1,                /* Roll Under Performance Flag*/~
            rpttitle$60,                 /* Report Title               */~
            so$19,                       /* Sales Order With Line #    */~
            sodemtype$1,                 /* Sales Order Demand Type    */~
            test_date$8,                 /* Cut Off Date Frm Screen    */~
            time$10,                     /* Current Time               */~
            tocat$4,                     /* Part Category              */~
            to_from$30,                  /* Type Error Variable        */~
            tomps$6,                     /* MPS Group                  */~
            topart$25,                   /* Part Number Range          */~
            type$(4%)3,                  /* Part Type                  */~
            typedescr$30,                /* Part Type Description      */~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #01 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #02 ! DEMMASTR ! Demand Master File                       *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! CATEGORY ! Inventory category codes file            *~
            * #05 ! GENCODES ! General Codes File                       *~
            * #06 ! MPSGROUP ! MPS Group Codes Master File              *~
            * #07 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #08 ! SYSFILE2 ! Caelus Management System Information     *~
            * #09 ! MPSITEMS ! MPS Items Master File                    *~
            * #50 ! WORKFILE ! Temporary System Workfile (Selected data)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #02, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  3, keypos =   29, keylen =  25, dup,    ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  1, keypos =   10, keylen =  19          ~

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25

            select #04, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #05, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #06, "MPSGROUP",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   8,                     ~
                        alternate key 1, keypos = 39, keylen = 8, dup,   ~
                                  key 2, keypos = 47, keylen = 8, dup

            select #07, "SFCUM2",                                        ~
                        varc, indexed, recsize = 1985,                   ~
                        keypos = 1, keylen = 25

            select #08, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #50, "WORKFILE",           /* Selected HNYQUAN keys */~
                        varc,     indexed,  recsize =  50,               ~
                        keypos =  1,   keylen =  31

            select #09, "MPSITEMS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  25,                     ~
                        alternate key 1, keypos = 26, keylen = 8, dup,   ~
                                  key 2, keypos = 34, keylen = 6, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
                if fs%(1%) <> 1% then L03018
                get rslt$(01%) using L03016, rec%
L03016:              FMT POS(17), BI(4)
L03018:         rec% = max(100%, rec% / 3%) /* for work file creation */
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
                if fs%(2%) <> 1% then L03030
                get rslt$(02%) using L03016, rec2%
                rec% = max(rec%, rec2% / 3%) /* for work file creation */

L03030:     call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%), 0%, rslt$(08%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 0%, rslt$(09%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            demdate$ = blankdate$
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Reset the Cumulative Forecast  " &              ~
                        "                              "

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "SFCUMRST: " & str(cms2v$,,8)

*        See if there are Part Types defined to Check against
            plowkey$ = "PARTTYPE "
            call "PLOWNEXT" (#05, plowkey$, 9%, types_on_file%)

            call "READ100" (#8, "MONTHS OPEN", f1%(8%))
            if f1%(8%) = 0% then exit_program  /* Got to have it */
            get #8, using  L09270, day1$
L09270:      FMT POS(33), CH(6)
            call "DATE" addr("G+", day1$, 490%, end_day$, ret%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  8%
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
                  if keyhit%  = 16% then       reset_cumfcst
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  8% then editpg1
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
            *           R E S E T   C U M   F O R C A S T               *~
            *-----------------------------------------------------------*~
            * Reste the Cumulative Forcast File.                        *~
            *************************************************************
        reset_cumfcst

            call "SHOSTAT" ("Cum. Forecast Reset in Progress...")

            call "FILEBGON" (#50)                /* Bye, bye, WorkFile */
            call "WORKOPEN" (#50, "IO   ", rec%, f1%(50%))/* Hello, WF */

            gosub reset_for_demand
            gosub reset_for_sales
            gosub reset_items_not_found /*Parts not in BVKLINES/DEMMASTR*/
            if print_over$ = "Y" then  gosub print_report

            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Number Range      */~
                              L20200,         /* Part Category          */~
                              L20300,         /* Part Type              */~
                              L20400,         /* MPS Group              */~
                              L20500,         /* Reset Date             */~
                              L20600,         /* Roll Over Performance  */~
                              L20700,         /* Roll Under Performance */~
                              L20800          /* Print Over Performance */
            return
L20100: REM Def/Enable Part Number Range           FMPART$
            if fmpart$             = " " then                            ~
               fmpart$             = "ALL"
            return

L20200: REM Def/Enable Part Category               FMCAT$
            if fmcat$              = " " then                            ~
               fmcat$              = "ALL"
            return

L20300: REM Def/Enable Part Type                   FMTYPE$
            if str(type$())  = " " then  type$(1%) = "ALL"
            return

L20400: REM Def/Enable MPS Group                   FMMPS$
            if fmmps$              = " " then                            ~
               fmmps$              = "ALL"
            return

L20500: REM Def/Enable Reset Date                  FMRESET_DATE$
            if reset_date$ = " " or reset_date$ = blankdate$ then ~
               reset_date$ = date$

            return

L20600: REM Def/Enable Roll Under Performance       ROLL_UNDER$
            if roll_under$ = " " then roll_under$ = "N"
            return

L20700: REM Def/Enable Roll Over Performance       ROLL_OVER$
            if roll_over$ = " " then roll_over$ = "N"
            return

L20800: REM Def/Enable Print Over Performance Flag  PRINT_OVER$
            if print_over$ = " " then print_over$ = "N"
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
         "Enter Part Number Range                                      ",~
         "Enter Part Category                                          ",~
         "Enter Part Type                                              ",~
         "Enter MPS Group                                              ",~
         "Enter Reset Date                                             ",~
         "Roll Under Performance Forward (Positive Cum Forecast) 'Y/N' ",~
         "Roll Over Performance Forward (Negative Cum Forecast) 'Y/N'  ",~
         "Print Over Performance Report 'Y/N'                          "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmcat$, fmmps$, fmpart$, hicat$, himps$, hipart$,  ~
                      locat$, lomps$, lopart$, tocat$, tomps$, topart$,  ~
                      type$(), roll_under$, roll_over$, print_over$,     ~
                      reset_date$

            print_cnt%, pcntr% = 0%


            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
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
            *           R E S E T   S A L E S   F O R C A S T           *~
            *-----------------------------------------------------------*~
            * Decriment Forcast for Sales                               *~
            *************************************************************
        reset_for_sales

            date_test% = 1%
            init (" ") part$, reqdship$, sodemtype$, alloc_flag$
            orderqty =  0
            readkey$ = hex(00)
            call "READ104" (#1, readkey$, soonfile%)
            goto L30140  /* Test Success */
         sales_loop
            call "READNEXT" (#1, soonfile%)
L30140:     if soonfile% = 0% then return

            get #1 using L30190, so$, part$, orderqty,                    ~
                                    reqdship$, sodemtype$, alloc_flag$

L30190:    FMT POS(10), CH(19), POS(32), CH(25), POS( 93), PD(14,4),     ~
                                             POS(212), CH(6), POS(240),  ~
               CH(1), POS(246), CH(1)

            if sodemtype$ = "2" then goto sales_loop
            demdate$ = reqdship$
            gosub range_filter
            if passed% = 0% then goto sales_loop
                demqty = 0
                gosub increment_forcast   /* Initialize */

                call "PIPINDEX" (#8, reqdship$, reqdship%, ret%)
                if alloc_flag$ = "C" then goto L30390 /* Decriment Frcst */

          /* Check for demand */
            call "REDALT0" (#2, so$, 1%, hit%)
            if hit% = 0% then goto sales_loop     /* No Demand */
                get #2 using L30350, demstatus$
L30350:           FMT CH(1)
                if pos("6789" = demstatus$) = 0%                         ~
                                    then goto sales_loop /* Not Planned */

L30390:     /* Decriment the Forcast for Sales */
            demqty = - orderqty
            demdate% = reqdship%
            gosub increment_forcast
            goto sales_loop

        REM *************************************************************~
            *           R E S E T   D E M A N D   F O R C A S T         *~
            *-----------------------------------------------------------*~
            * Increase  Forcast for Demand                              *~
            *************************************************************
        reset_for_demand
            date_test% = 1%
            demand$ = "4"
L31065:     plowkey$ = demand$ & hex(00)
         demand_loop
            call "PLOWNEXT" (#2, plowkey$, 1%, demand%)
            if demand% = 1% then L31105
                if demand$ <> "4" then return
                     demand$ = "5"
                     goto L31065

L31105:     get #2 using L31110, demstatus$, part$, demqty$, demdate$
L31110:   FMT CH(1), POS(29), CH(25), CH(10), POS(83), CH(6)

            if demdate$ = " " then demdate$ = blankdate$
            gosub range_filter
            if passed% = 0% then demand_loop
                demqty = 0
                gosub increment_forcast   /* Initialize */

            if pos("6789" = demstatus$) = 0%                             ~
                               then goto demand_loop /* Not Planned */

            convert demqty$ to demqty, data goto demand_loop

            if demdate$ =  " " or demdate$ = blankdate$ then goto demand_loop
                call "PIPINDEX" (#8, demdate$, demdate%, ret%)
                if ret% <> 0% then  demdate% = 0%

            gosub increment_forcast

            goto demand_loop

        increment_forcast
            over_index% = 0% : over_qty = 0
            mat cumf% = zer
            call "READ101" (#7, part$, f1%(7%))
            if f1%(7%) = 0% then goto L31340
                get #7 using L31235, part$, cumf%()
L31235:              FMT CH(25), 490*BI(4)
            plowkey2$ = str(part$) & hex(00)
            call "PLOWNEXT" (#50, plowkey2$, 25%, f1%(50%))
            if f1%(50%) = 1% then L31340   /* Already done at least one */
                /* Not Done Before */
                call "PIPINDEX" (#8, test_date$, reset%, ret%)
                if reset% < 2%   or reset% > 490% then L31295
                if roll_over$ = "Y" and cumf%(reset% - 1%) < 0%          ~
                     then L31295
                if roll_under$ = "Y" and cumf%(reset% - 1%) > 0%         ~
                     then L31295
L31290:         mat cumf% = zer    /* Initialize */   :  goto L31340
L31295:        /* Seed with previous bucket */
                if reset% < 2%   or reset% > 490% then L31290/* Init */

                for i% = reset% to 490%
                    cumf%(i%)  =  cumf%(reset% - 1%)
                next i%
                for i% = 1% to reset% - 2%
                    cumf%(i%)  =  0%
                next i%
L31340:     /* Incriment the Forcast Demands */
            if demdate% < 1%   or demdate% > 490% then L31380
            for i% = demdate% to 490%
               cumf%(i%) = cumf%(i%) + demqty
               if cumf%(i%) >= 0% or over_index% > 0% then L31370
                     over_index% = i% : over_qty = cumf%(i%)
L31370:     next i%

L31380:     put #7  using L31235, part$, cumf%()
            if f1%(7%) = 0% then write #7   else  rewrite #7

            if over_index% = 0% then over_day$ = blankdate$ else     ~
               call "DATE" addr("G+",day1$,over_index% -1%,over_day$,ret%)

            plowkey2$ = str(part$) & over_day$
            call "READ101" (#50, plowkey2$, f1%(50%))
            put #50 using L31430, part$, over_day$, over_qty, " "
            if f1%(50%) = 0% then write #50  else  rewrite #50
L31430:    FMT CH(25), CH(6), PD(14,4), CH(11)
            return

        REM *************************************************************~
            *          R E S E T   I T E M S   N O T   F O U N D        *~
            *-----------------------------------------------------------*~
            * Zero Record meeting filter criteria not in BCK or DEMMSTR *~
            *************************************************************
        reset_items_not_found /*Parts not in BCKLINES/DEMMASTR*/
            date_test% = 0%
            mat cumf% = zer
            readkey$ = hex(00)
            call "READ105" (#7, readkey$, f1%(7%))
            goto L31510      /* Test */
         loop_sfcum2
            call "READNXT1" (#7,           f1%(7%))
L31510:     if f1%(7%) = 0% then return    /* ** ALL DONE ** */

            get #7 using L31525, part$
L31525:   FMT CH(25)
            gosub range_filter
            if passed% <> 1% then loop_sfcum2
            plowkey$ = str(part$) & hex(00)
            call "PLOWNEXT" (#50, plowkey$, 25%, f1%(50%))
               if f1%(50%) = 1% then loop_sfcum2 /* Has Been Reset */

            put #7  using L31235, part$, cumf%()
            rewrite #7

            goto loop_sfcum2


        REM *************************************************************~
            *               M I S C.  S U B S                           *~
            *************************************************************
        range_filter
            passed%, got_hny% = 0%
            if date_test% = 1%  and  test_date$ >  demdate$ then return

            if fmpart$ = "ALL" then L32150
                if part$ < fmpart$ or part$ > topart$ then return
L32150:     if fmcat$  = "ALL" then L32250
                gosub get_hny_details
                if hnycat$ < fmcat$ or hnycat$ > tocat$ then return
L32250:     if type$(1%) = "ALL" then L32350
            if type$(1%) = "000" and type$(2%) = "999" then L32350
                if got_hny% = 0% then gosub get_hny_details
                if hnytype$ < type$(1%) or hnytype$> type$(2%) then return
L32350:     if fmmps$  = "ALL" then L32420
                call "READ100" (#9, part$, f1%(9%))
                if f1%(9%) = 0% then return
                    get #9 using L32390, mpsgroup$
L32390:        FMT POS(26), CH(8)
                if mpsgroup$ < fmmps$ or mpsgroup$ > tomps$ then return

L32420:     passed% = 1%
            return

        get_hny_details
            call "READ100" (#3, part$, f1%(3%))
            if f1%(3%) = 0% then return  /* Shouldn't Happen */
            get #3 using L32640, hnycat$, hnytype$
L32640:   FMT POS(90), CH(4), POS(180), CH(3)
            got_hny% = 1%
            return

        REM *************************************************************~
            *               P R I N T    S U B S                        *~
            *************************************************************
        print_report
            call "SHOSTAT" ("Report in Progress...")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            pcntr% = -1% : lcntr% = 99% : print_cnt% = 0%
            if lcntr% > 56% then gosub page_head

            plowkey$ = hex(00)
        loop_workfile
            call "PLOWNEXT" (#50, plowkey$, 0%, f1%(50%))
            if f1%(50%) = 0% then goto end_report
            get #50 using L33140, part$, over_day$, over_qty
L33140:        FMT CH(25), CH(6), PD(14,4)
            if over_day$ = blankdate$ then loop_workfile  /*No Over so, next*/

        /* Print Overage Line Detail */
            call "DESCRIBE" (#3, part$, partdescr$, 0%, f1%(3%))
            call "DATEFMT" (over_day$)
            call "CONVERT" (over_qty, 2.2, over_qty$ )

            print using L60200, part$, partdescr$, over_day$, over_qty$
            lcntr% = lcntr% + 1%
            print_cnt% = print_cnt% + 1%

            goto loop_workfile

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            if print_cnt% <> 0% then L33320
                print "No Negative Cumulative Forcasts Encountered."
L33320:     print skip(2)
            print using L60250, time$        /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60060, date$, time$, company$, "SFCUMRST"
            print using L60080, rpttitle$, pcntr%
            print
            lcntr% = 3%
            if pcntr% = 0% then gosub print_params                       ~
                           else gosub column_head
            return

        column_head
            print using L60100
            print using L60120
            lcntr% = lcntr% + 2%
            return

        print_params           /* Print Page Zero */
L33560:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L33600
                str(i$(), i%, 1%) = hex(20)
                goto L33560
L33600:     print skip(3)
            print tab(26);
            print "----------------------Simulation Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 19% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            gosub page_head
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
              on fieldnr% gosub L40095,       /* Part Number Range      */~
                                L40095,       /* Part Category          */~
                                L40095,       /* Part Type              */~
                                L40095,       /* MPS Group              */~
                                L40095,       /* Reset Date             */~
                                L40095,       /* Roll Over Performance  */~
                                L40095,       /* Roll Under Performance */~
                                L40095        /* Print Over Performance */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Restore Cumulative Forecast Selection Criteria",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Part Number Range",                          ~
               at (07,30), fac(lfac$( 1%)), fmpart$             , ch(25),~
               at (07,56), fac(lfac$( 1%)), topart$             , ch(25),~
                                                                         ~
               at (08,02), "Part Category",                              ~
               at (08,30), fac(lfac$( 2%)), fmcat$              , ch(04),~
               at (08,56), fac(lfac$( 2%)), tocat$              , ch(04),~
                                                                         ~
               at (09,02), "Part Type",                                  ~
               at (09,30), fac(lfac$( 3%)), type$(1%)           , ch(03),~
               at (09,56), fac(lfac$( 3%)), type$(2%)           , ch(03),~
                                                                         ~
               at (10,02), "MPS Group",                                  ~
               at (10,30), fac(lfac$( 4%)), fmmps$              , ch(08),~
               at (10,56), fac(lfac$( 4%)), tomps$              , ch(08),~
                                                                         ~
               at (11,02), "Reset Date",                                 ~
               at (11,30), fac(lfac$( 5%)), reset_date$         , ch(08),~
                                                                         ~
               at (12,02), "Roll Under Performance    ",                 ~
               at (12,30), fac(lfac$( 6%)), roll_under$         , ch(01),~
                                                                         ~
               at (13,02), "Roll Over Performance    ",                  ~
               at (13,30), fac(lfac$( 7%)), roll_over$          , ch(01),~
                                                                         ~
               at (14,02), "Print Over Perfomance Rpt ",                 ~
               at (14,30), fac(lfac$( 8%)), print_over$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40315
                  call "MANUAL" ("SFCUMRST") : goto L40110

L40315:        if keyhit% <> 15% then L40330
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
            pf$(3%) = "                                        " &       ~
                     "                       (16)ResetCumFCST"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40470:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
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
            on fieldnr% gosub L50100,         /* Part Number Range      */~
                              L50200,         /* Part Category          */~
                              L50300,         /* Part Type              */~
                              L50400,         /* MPS Group              */~
                              L50500,         /* Reset Date             */~
                              L50600,         /* Roll Over Performance  */~
                              L52610,         /* Roll Under Performance */~
                              L50800          /* Print Over Performance */
            return
L50100: REM Test for Part Number Range            FMPART$
            call "TESTRNGE"                                              ~
                  (fmpart$             , topart$             ,           ~
                   lopart$             , hipart$             ,           ~
                   errormsg$, #3)
            return

L50200: REM Test for Part Category                FMCAT$
            call "TESTRNGE"                                              ~
                  (fmcat$              , tocat$              ,           ~
                   locat$              , hicat$              ,           ~
                   errormsg$, #4)
            return

L50300: REM Test for Part Type                    FMTYPE$
            if type$(1%) <> "ALL" then L50308
                type$(2%) = " "
                goto L50336
L50308:     for i% = 1% to 2%
                if i% = 1% then to_from$ = "From Part Type "             ~
                           else to_from$ = "To Part Type "
                if type$(i%) = "?" and types_on_file% = 1% then L50324
                convert type$(i%)  to type%, data goto L50368
                convert type% to type$(i%), pic(000)
                if type% < 0% or type% > 999% then L50368
                if types_on_file% <> 1% then L50332
L50324:              gosub check_gencode_for_part_type
                     if errormsg$ <> " " then return
                     if i% = 1% and type$(2%) = " " then type$(2%) =     ~
                                                                type$(1%)
L50332:     next i%

L50336:     call "TESTRNGE" (type$(1%), type$(2%), type$(3%), type$(4%), ~
                             errormsg$)
            return

L50368:         errormsg$ = "Please enter " & to_from$ &                 ~
                            " as numeric, 000 to 999"
            return

L50400: REM Test for MPS Group                    FMMPS$
            call "TESTRNGE"                                              ~
                  (fmmps$              , tomps$              ,           ~
                   lomps$              , himps$              ,           ~
                   errormsg$, #6)
            return

L50500: REM Test for Reset Date                     RESET_DATE$
            call "DATEOK" (reset_date$, date%, errormsg$)
                   date% =  date% /* Do Nothing */
            if errormsg$ <> " " then return

            test_date$ = reset_date$
            call "DATUNFMT" (test_date$)

            if test_date$ < day1$ or test_date$ > end_day$  then         ~
               errormsg$ = "Reset Date is Outside of Planning Calander"
            return

L50600: REM Test Roll Under Performance       ROLL_UNDER$
            if pos("YN" = roll_under$) > 0% then return
                errormsg$ = "Please Enter 'Y' or 'N'."
                return

        REM Test Roll Over Performance       ROLL_OVER$
            if pos("YN" = roll_over$) > 0% then return
                errormsg$ = "Please Enter 'Y' or 'N'. "
                return


L50800: REM Test Print Over Performance       PRINT_OVER$
            if pos("YN" = print_over$) > 0% then return
                errormsg$ = "Please Enter 'Y' or 'N'."
                return

        check_gencode_for_part_type
            if type$(i%) = "?" then type$(i%) = all(hex(20))
            readkey$ = "PARTTYPE " &  type$(i%)
            typedescr$   = hex(06) & "Select " & to_from$
            call "PLOWCODE" (#05, readkey$, typedescr$, 9%, .3, f1%(5%))
            if f1%(5%) = 1% then L52420
                errormsg$ = to_from$ & " not on File"
                return
L52420:     type$(i%) = str(readkey$, 10%, 3%)
L52610:     return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60060: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60080: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Column Header Line 1
L60100:  %      PART NUMBER                DESCRIPTION                   ~
        ~     DATE      QUANTITY

*       * Column Header Line 2
L60120:  %      -------------------------  ------------------------------~
        ~---  --------  ----------

L60200:  %      #########################  ##############################~
        ~###  ########  ##########

        %** Report Title for page 0
        %############################################################

L60250:         %                          * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
