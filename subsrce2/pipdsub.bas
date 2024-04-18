        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   DDDD    SSS   U   U  BBBB           *~
            *  P   P    I    P   P  D   D  S      U   U  B   B          *~
            *  PPPP     I    PPPP   D   D   SSS   U   U  BBBB           *~
            *  P        I    P      D   D      S  U   U  B   B          *~
            *  P      IIIII  P      DDDD    SSS    UUU   BBBB           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPDSUB  - Display and/or Print program for the three PIP *~
            *             files; PIPIN, PIPOUT and PIPMASTR             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/27/87 ! Major Rewrite!                           ! MJB *~
            * 03/03/88 ! Added SHELF and PIP to Printed Status Rpt! RJM *~
            * 11/04/88 ! Added HNYALTRS for PLNRSUB               ! KAB *~
            * 03/02/90 ! changed line is GET_PIPOUT subroutine.   ! LAB *~
            *          ! TAG$ > HITAG$ is now PART$ > HIPART$     !     *~
            * 09/25/91 ! PRR 11137 date/display range philosophy. ! JIM *~
            * 11/27/91 ! PRR 12054 Check for valid Flagged condi- ! RJH *~
            *          ! tion before reporting PIPMASTR record.   !     *~
            * 10/25/93 ! Purchased Jobs - Added support for 'RW'  ! JBK *~
            *          ! and 'BW' PIPs.  Added some (%) signs.    !     *~
            * 05/05/94 ! Corrected two bad branches including a   ! JBK *~
            *          ! goto to a FMT statement.                 !     *~
            * 07/18/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "PIPDSUB" (#1,  #2,  #3,  #4,  #6,  #46, #45, #7, #11,   ~
               #12, #15, #23, #24, #35, #40, #41, #8,  #9, #10, #17, #16)

        dim                                                              ~
            actdate$(8)8,                /* Action Date Array          */~
            blankdate$8,                 /* Blank Date for Comparison. */~
            col_header$51,               /* Screen Column Header       */~
            cs$10, moq$10, qty$10,       /* Display Variables          */~
            ss$10, ssi$10,               /*   and more                 */~
            company$60,                  /* Company name for report    */~
            cumf%(490),                  /* Cumulative Forcast array   */~
            cursor%(2),                  /* Cursor location for edit   */~
            d$8,                         /* A Date                     */~
            date$8,                      /* Date for screen display    */~
            datein$8, dateact$8, datestart$8,   /*                     */~
            stdate$(8)8, indate$(8)8,    /* More Date Arrays           */~
            datesw$1, dateswdescr$40,    /* Using What Date?           */~
            ddate$(8)8,                  /* Date array to display      */~
            docsw$3, docswdescr$40,      /* Type of Add or Wdwal?      */~
            dqty$(8)8,                   /* Quantity array to display  */~
            edtmessage$79,               /* Edit screen message        */~
            edtmessage3$79,              /* Edit screen message, Pg 3  */~
            errormsg$79,                 /* Error message              */~
            fromdate$8, todate$8,        /* Date Range                 */~
            frompart$25, topart$25,      /* Part Number Range          */~
            fromstat$3, statdescr$40,    /* Status Flag for PIPMASTR   */~
            fromtag$19, totag$19,        /* PIP Tag Number Range       */~
            headline$(3)79,              /* Status  screen header line */~
            headerline$79,               /* Display screen header line */~
            i$(24)80,                    /* 2nd Screen Image           */~
            j$(24)80,                    /* 1st Screen Image           */~
            infomsg$(7)79,               /* Information messages       */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$(16)79,                 /* Line item display line     */~
            line$79,                     /* Another one                */~
            line2$79,                    /* Screen Line #2             */~
            linekey$(19)25,              /* Line Key for PIPMASTR      */~
            legend$132,                  /* Status Report Header       */~
            lopart$25, hipart$25,        /* Part Range for PLOW        */~
            lotag$19,  hitag$19,         /* Tag Range for PLOW         */~
            moq$(8)10,                   /* MOQ Array for Display      */~
            msg$80,                      /* Message for SHOSTAT        */~
            part$25,                     /* Part Number                */~
            part$(8)25,                  /* Part Array for Display     */~
            partdescr$32,                /* Part description           */~
            partdescr$(8)32,             /* Part description array     */~
            partno$25,                   /* Part Number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pip%(490),                   /* PIP master array           */~
            pipinkey$48,                 /* PIPIN Key                  */~
            pipmastrkey$26,              /* PIPMASTR Key               */~
            pipoutkey$56,                /* PIPOUT Key                 */~
            pipsw$1, pipswdescr$40,      /* Adds, Wdwls, or Status?    */~
            plankey$25,                  /* Key for Subroutine         */~
            plowkey$50,                  /* Key for reads & Plows      */~
            pldate$8,                    /* Todays Date again          */~
            qty$(8)10,                   /* Qty Array for Display      */~
            reporttitle$(3,10)60,        /* Report Title Array         */~
            revumsg$79,                  /* Message for Revue Screen   */~
            savekey$60,                  /* Holding Area               */~
            sfac$(2)1,                   /* Sort Select FAC            */~
            sort$1,                      /* Sort Select Flag           */~
            sortmsg$25,                  /* Sort Select Message        */~
            sortdescr$25,                /* Sort Select Description    */~
            ss$(8)10,                    /* SS  Array for Display      */~
            ssi$(8)8, ssid$(8)8,         /* SSI Array for Display      */~
            stdate$8,                    /* Status Date                */~
            stlit$25,                    /* Status Literal             */~
            stqty$8,                     /* Status Quantity            */~
            stshelf$8,                   /* Shelf Quantity             */~
            stpip$8,                     /* PIP Quantity               */~
            sur$(8)8, surd$(8)8,         /* SUR Array for display      */~
            sur$8, surd$8,               /* Surplus fields             */~
            cs$(8)8, csd$(8)8,           /* CS  Array for display      */~
            stat$1,                      /* Status Code                */~
            tag$19, tagnr$(8)19,         /* PIP tag number & array     */~
            time$8,                      /* Time for report            */~
            ttl$50,                      /* Screen Title Data          */~
            ttl1$5,                      /* Column Titles Revue Screen */~
            ttl2$54,                     /* Column Titles Revue Screen */~
            userid$3                     /* Current User Id            */~

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */~

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
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! PIPIN    ! Planned inventory additions detail       *~
            * # 3 ! PIPOUT   ! Planned inventory use detail rec         *~
            * # 4 ! PIPMASTR ! Planned Inventory Position Master File   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

*       * All files selected and opened in the calling program!

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$, d$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, err%)
            call "TIME" (time$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = " PIPDSUB: " & str(cms2v$,,8)
            sortmsg$ = "Sort Selection"
            call "READ100" (#1, "MONTHS OPEN", f1%(1))
            if f1%(1) = 1% then L09210
                ask% = 2%
                call "ASKUSER" (ask%, "PLANNING DATE ERROR",             ~
                     "Cannot Establish Planning Calendar Start Date",    ~
                     " ", "Press RETURN to Exit Program and Resolve " &  ~
                     "the Problem")
                goto exit_program

L09210:     get #1 using L09240, pldate$
            call "DATE" addr("G-", pldate$, d$, today%, err%)
            today% = today% + 1%
L09240:         FMT XX(32), CH(6)

            col_header$ = "From:                     To:"

            restore line = L09350
            for i% = 1% to 3%
                for j% = 1% to 10%
                    read reporttitle$(i%,j%)
                    call "STRING" addr("CT", reporttitle$(i%,j%), 60%)
                next j%
            next i%

L09350: data                                                             ~
            "Expected Additions From Released Job Orders"              , ~
            "Expected Additions From Work Order Advices"               , ~
            "Unplanned Sales Orders"                                   , ~
            "Expected Additions From Buy (Purchase) Advices"           , ~
            "Expected Additions From Purchase Directives (Req's)"      , ~
            "Expected Additions From Purchase Orders"                  , ~
            "Expected Additions From Goods Pending in QC"              , ~
            "Expected Additions From Buy Work (Purchase Job) Advices"  , ~
            "Expected Additions From Purchase Job Directives (Req's)"  , ~
            "All Expected Inventory Additions"                         , ~
            "Expected Withdrawals to Job Orders"                       , ~
            "Expected Withdrawals to Work Order Advices"               , ~
            "Expected Withdrawals to Sales Orders"                     , ~
            " "                                                        , ~
            " "                                                        , ~
            " "                                                        , ~
            " "                                                        , ~
            "Expected Withdrawals From Buy Work (Purchase Job) Advices", ~
            "Expected Withdrawals From Purchase Job Directives (Req's)", ~
            "All Expected Inventory Withdrawals"                       , ~
            " "                                                        , ~
            "Manufactured Parts In Surplus Stock Condition"            , ~
            "Purchased Parts In Surplus Stock Condition"               , ~
            "Manufactured Parts With a Safety Stock Intrusion"         , ~
            "Purchased Parts With a Safety Stock Intrusion"            , ~
            " "                                                        , ~
            " "                                                        , ~
            "Manufactured Parts With a Critical Shortage"              , ~
            "Purchased Parts With a Critical Shortage"                 , ~
            "All Parts Showing Abnormal Status Codes"

            headline$(1)= "Part Number/Description      S.S. Level   " & ~
                          "  M.O.Q.    On Hand  Sur Qty  on Date"
            headline$(2)= "Part Number/Description      S.S. Level   " & ~
                          "  M.O.Q.    On Hand  Int Qty  on Date"
            headline$(3)= "Part Number/Description      S.S. Level   " & ~
                          "  M.O.Q.    On Hand   Cr Sht  on Date"

            legend$ = "Legend STAT: C = Critical Shortage, I = S/S Intrus~
        ~ion, S = Surplus Condition"
            call "STRING" addr ("CT", legend$, 132%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000

            for fieldnr% = 1% to  8%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            init (" ") infomsg$(), errormsg$
            lastfieldnr% = 0%
L11090:     gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                 if keyhit%  =  1% then gosub startover
                 if keyhit% <> 14% then L11170
                    if pipsw <> 3 then L11160
                    if stat <> 10 then L11160
                        errormsg$ = "Status 'ALL' is available in " &    ~
                                    "printed format ONLY"
                        goto L11090
L11160:             goto display_it
L11170:          if keyhit% <> 30% then L11180
                    gosub print_it : goto inputmode
L11180:          if keyhit%  = 16% then exit_program
                 if keyhit% <>  0% then       editpg1
L11200:     if cursor%(1%) > 4% and cursor%(1%) < 8% then                ~
                                              fieldnr% = cursor%(1%) - 4%
            if cursor%(1%) > 9% and cursor%(1%) < 15% then               ~
                                              fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  8% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11260:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                  lastfieldnr% = fieldnr%
            goto L11200

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            init(" ") infomsg$()
            on fieldnr% gosub L20100,         /* PIP Type           */    ~
                              L20200,         /* Doc Type           */    ~
                              L20300,         /* Using What Date?   */    ~
                              L20400,         /* Part Number Range  */    ~
                              L20500,         /* Date Range         */    ~
                              L20600,         /* Tag Number Range   */    ~
                              L20700,         /* Part Status Code   */    ~
                              L20800          /* Sort Flag          */
            return

L20100: REM Def/Enable Adds, Wdwals, or Status?    PIPSW$
            if pipsw$ = " " then pipsw$ = "A"
            infomsg$(1) = "Enter 'A' to View Expected ADDITIONS " &      ~
                          "to Inventory"
            infomsg$(2) = "Enter 'W' to View Expected WITHDRAWALS " &    ~
                          "from Inventory"
            infomsg$(3) = "Enter 'S' to View PIP Status"
            return

L20200: REM Def/Enable Type of Adds or Wdwals?     DOCSW$
            if docsw$ = " " then docsw$ = "ALL"
            if pipsw <> 3 then L20205
                enabled% = 0%
                return
L20205:     infomsg$(1%) = "'JO' for RELEASED JOBS            'RO' "  &  ~
                           "for PURCHASE DIRECTIVES (Requisitions)"
            infomsg$(2%) = "'WO' for WORK ORDER ADVICES       'PO' "  &  ~
                           "for PURCHASE ORDERS                   "
            infomsg$(3%) = "'SO' for SALES ORDERS             'QC' "  &  ~
                           "for PENDING IN QC                     "
            infomsg$(4%) = "'BO' for BUY ADVICES              'RW' "  &  ~
                           "for PURCHASE JOB DIRECTIVES (Req's)    "
            infomsg$(5%) = "'BW' for BUY WORK (PJ) ADVICES   'ALL' "  &  ~
                           "to see EVERYTHING!                    "
            if pipsw = 1 then return
              str(infomsg$(1%),35%,44%) = str(infomsg$(5%), 1%,33%)
              str(infomsg$(2%),35%,44%) = str(infomsg$(4%),35%,44%)
              str(infomsg$(3%),35%,44%) = str(infomsg$(5%),35%,44%)
              infomsg$(4%) = " "  :  infomsg$(5%) = " "
              return

L20300: REM Def/Enable Using What Date?            DATESW$
            if datesw$ = " " then datesw$ = "D"
            if pipsw = 1 then L20350
                enabled% = 0%
                return
L20350:     infomsg$(1) = "Date selected is used for the Date Range " &  ~
                          "Selection Above"
            call "STRING" addr("CT", infomsg$(1), 79%)
            return

L20400: REM Def/Enable Part Number Range           FROMPART$
            if frompart$ = " " then frompart$ = "ALL"
            return

L20500: REM Def/Enable Date Range                  FROMDATE$
            if fromdate$ <> " " and fromdate$ <> blankdate$ then goto L20520
                fromdate$ = "ALL"
                todate$ = " "
L20520:     if pipsw <> 3 then return
                enabled% = 0%
                return

L20600: REM Def/Enable Document Number Range       FROMTAG$
            if fromtag$ = " " then fromtag$ = "ALL"
            if pipsw <> 3 then return
                enabled% = 0%
                return

L20700: REM Def/Enable Part Status Code            FROMSTAT$
            if fromstat$ = " " then fromstat$ = "ALL"
            if pipsw = 3 then L20725
                enabled% = 0%
                return
L20725:     infomsg$(1) = "Manufactured Part Status           Purc"  &   ~
                          "hased Part Status                     "
            infomsg$(2) = "'2' Surplus Condition              '3' "  &   ~
                          "Surplus Condition                     "
            infomsg$(3) = "'4' Safety Stock Intrusion         '5' "  &   ~
                          "Safety Stock Intrusion                "
            infomsg$(4) = "'8' Critical Shortage Condition    '9' "  &   ~
                          "Critical Shortage Condition           "
            return

L20800: REM Def/Enable Sort Flag                   SORT$
            if sort$ = " " then sort$ = "P"
            if pipsw < 3 then L20840
                enabled% = 0%  :  return
L20840:     sfac$(1) = hex(8c)
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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
         "Enter 'A'- Additions, 'W'- Withdrawals or 'S'- Status        ",~
         "Enter PIP Tag Type Code From The List Shown Above            ",~
         "Enter 'D' for 'Due Date', 'S' for 'Start Date' or 'A' for Actua~
        ~l Date",                                                         ~
         "Enter Beginning and Ending Part Numbers                      ",~
         "Enter Beginning and Ending Dates, 'FIRST', 'LAST', 'PLAN' or 'A~
        ~LL'",                                                            ~
         "Enter Beginning and Ending PIP Tag Numbers                   ",~
         "Enter Part Status Code for Review                            ",~
         "Enter 'P' for Part Number Sequence or 'T' for PIP Tag Number "

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, infomsg$(), sort$,         ~
                      datesw$, docsw$, fromdate$, frompart$, fromstat$,  ~
                      fromtag$, todate$, topart$, totag$, pipsw$,        ~
                      dateswdescr$, docswdescr$, pipswdescr$, statdescr$

            init(hex(9c)) sfac$()
            call "ALLFREE"
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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

L30000: REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

            on file% goto get_pipin, get_pipout, get_pipmastr
                return

L30090: get_pipin
            call "PLOWALTS" (#2, pipinkey$, sort%, 0%, success%)
            if success% = 0% then return

            get #2 using L39060, part$, datein%, tag$, qty, datestart%
            init (hex(00)) plowkey$
            dateact$ = " "


            if str(tag$,,2%) = "JO" and (docsw = 1 or docsw = 10)        ~
                    then L30190 else L30270
L30190:         str(plowkey$,,8%) = str(tag$,12%,8%)
                call "READ100" (#8, plowkey$, f1%(8%))
                    if f1%(8%) <> 1% then L30540
                        get #8 using L30230, dateact$
L30230:                     FMT POS(147), CH(6)
                        call "PIPINDEX" (#1, dateact$, dateact%, err%)
                        if err% <> 0% and err% <> 2% then L30540
                        goto L30560

L30270:     if str(tag$,,2%) = "RO" and (docsw = 5 or docsw = 10)        ~
                    then L30300
            if str(tag$,,2%) = "RW" and (docsw = 9 or docsw = 10)        ~
                    then L30300
            goto L30430
L30300:         str(plowkey$,,19%) = tag$
                call "REDALT0" (#9, plowkey$, 5%, f1%(9%))
                    if f1%(9%) <> 1% then L30540
                        get #9 using L30340, partno$, dateact$
L30340:                     FMT POS(14), CH(25), CH(6)
                        if partno$ = part$ then L30390
L30360:                     call "READNEXT" (#9, f1%(9%))
                                if f1%(9%) <> 1% then L30540
                                if partno$ <> part$ then L30360
L30390:                 call "PIPINDEX" (#1, dateact$, dateact%, err%)
                        if err% <> 0% and err% <> 2% then L30540
                        goto L30560

L30430:     if str(tag$,,2%) = "PO" and (docsw = 6 or docsw = 10)        ~
                    then L30450 else L30540
L30450:         str(plowkey$,,16%) = str(tag$,3%,14%) & "  "
                call "REDALT0" (#10, plowkey$, 1%, f1%(10%))
                    if f1%(10%) <> 1% then L30540
                        get #10 using L30490, dateact$
L30490:                     FMT POS(593), CH(6)
                        call "PIPINDEX" (#1, dateact$, dateact%, err%)
                        if err% <> 0% and err% <> 2% then L30540
                        goto L30560

L30540:     dateact% = 0%  :  dateact$ = " "

L30560:     if sort% = 1% then L30670     /*  Here for Tag Sequence  */
                if fromtag$ = "ALL" then L30620
                    if tag$ < lotag$ then L30090
                    if tag$ <= hitag$ then L30620
                        success% = 0%
                        return
L30620:         if frompart$ = "ALL" then L30760
                    if part$ < lopart$ then L30090
                    if part$ > hipart$ then L30090
                goto L30760

L30670:     if frompart$ = "ALL" then L30720
                if part$ < lopart$ then L30090
                if part$ <= hipart$ then L30720
                    success% = 0%
                    return
L30720:         if fromtag$ = "ALL" then L30760
                    if tag$ < lotag$ then L30090
                    if tag$ > hitag$ then L30090

L30760:     if datesw > 1 then L30800
            if fdate% > datein% or datein% > tdate% then L30090
            goto L30860

L30800:     if datesw > 2 then L30840
            if fdate% > datestart% or datestart% > tdate% then L30090
            goto L30860

L30840:     if fdate% > dateact% or dateact% > tdate% then L30090

L30860:     gosub set_tag
            if err% <> 0% then L30090
            call "DESCRIBE" (#6, part$, partdescr$, 0%, f1%(6))
            call "CONVERT" (qty, 2.4, qty$)

            call "DATE" addr("G+",pldate$,datein%   -1%,datein$   ,err%)
            call "DATE" addr("G+",pldate$,datestart%-1%,datestart$,err%)
            call "DATEFMT" (datein$) : call "DATEFMT" (datestart$)
            call "DATEFMT" (dateact$)

            return

        get_pipout
L30990:     call "PLOWALTS" (#3, pipoutkey$, sort%, 0%, success%)
            if success% = 0% then return

            get #3 using L39130, tag$, part$, dateout%, qty

            if sort% = 1% then L31150     /* Here for Tag Order */
                if fromtag$ = "ALL" then L31100
                    if tag$ < lotag$ then L30990
                    if tag$ <= hitag$ then L31100
                       success% = 0%
                       return
L31100:         if frompart$ = "ALL" then L31240
                    if part$ < lopart$ then L30990
                    if part$ > hipart$ then L30990
                       goto L31240

L31150:     if frompart$ = "ALL" then L31200   /* Here for Part Order */
                if part$ < lopart$ then L30990
                if part$ <= hipart$ then L31200
                    success% = 0%
                    return
L31200:         if fromtag$ = "ALL" then L31240
                    if tag$ < lotag$ then L30990
                    if tag$ > hitag$ then L30990

L31240:     gosub set_tag
            if err% <> 0% then L30990
            if fdate% > dateout% or dateout% > tdate% then L30990
            call "DESCRIBE" (#6, part$, partdescr$, 0%, f1%(6))
            call "CONVERT" (qty, 2.4, qty$)

            call "DATE" addr("G+", pldate$, dateout%-1%, dateout$, err%)
            call "DATEFMT" (dateout$)

            return

        set_tag
            err% = 0%
            if docsw = 10 then return
            if docsw =  3 then L31460
            if docsw =  1 and str(tag$,,2%) = "JO" then return
            if docsw =  2 and str(tag$,,2%) = "WO" then return
            if docsw =  4 and str(tag$,,2%) = "BO" then return
            if docsw =  5 and str(tag$,,2%) = "RO" then return
            if docsw =  6 and str(tag$,,2%) = "PO" then return
            if docsw =  7 and str(tag$,,2%) = "QC" then return
            if docsw =  8 and str(tag$,,2%) = "BW" then return
            if docsw =  9 and str(tag$,,2%) = "RW" then return
               err% = 9%  : return
L31460:     if str(tag$,,2%) <> "JO" and str(tag$,,2%) <> "WO" and       ~
               str(tag$,,2%) <> "BO" and str(tag$,,2%) <> "RO" and       ~
               str(tag$,,2%) <> "BW" and str(tag$,,2%) <> "RW" and       ~
               str(tag$,,2%) <> "PO" and str(tag$,,2%) <> "QC" then return
                    err% = 9%  : return

        get_pipmastr
            init(" ") ssid$, ssi$, csd$, cs$, surd$, sur$
            mat pip% = zer
            mat cumf% = zer
L31530:     call "PLOWALTS" (#4, pipmastrkey$, sort%, 0%, success%)
            if success% = 0% then return

            get #4 using L39200, stat$, part$, pip%(), qty, ss, moq
            if stat$ = " " then L31530

            call "READ100" (#41, part$, f1%(41))
                if f1%(41) = 0% then L31590

            get #41, using L39300, cumf%()

L31590:     if sort% = 1% then L31690     /* Here for Part Order */
                if frompart$ = "ALL" then L31650
                    if part$ < lopart$ then L31530
                    if part$ <= hipart$ then L31650
                       success% = 0%
                       return
L31650:         if stat = 10 then L31770
                    if stat$ <> fromstat$ then L31530
                    goto L31770

L31690:     if stat = 10 then L31730      /* Here for Status/Part Order */
                if stat$ = fromstat$ then L31730
                    success% = 0%
                    return
L31730:         if frompart$ = "ALL" then L31770
                    if part$ < lopart$ then L31530
                    if part$ > hipart$ then L31530

L31770:     call "DESCRIBE" (#6, part$, partdescr$, 0%, f1%(6))
            call "CONVERT" (qty, 2.4, qty$)
            call "CONVERT" (ss , 2.4, ss$ )
            call "CONVERT" (moq, 2.4, moq$)
            ss% = int(round(ss,0))
            moq% = int(round(moq,0))
            max% = moq% + ss%

            sur%, surd%, ssi%, ssid%, csd% = 0%
            ss% = int(round(ss,0))

*       * First is the Surplus Condition
            if stat = 10 and (stat$ = "2" or stat$ = "3") then L31900
            if stat > 3 then L32050
L31900:     for j% = today% to 490%
                if surd% > 0% then L31950
                if pip%(j%) <= (max% + max(0%, cumf%(j%))) then L31940
                    if surd% = 0% then surd% = j%
L31940:     next j%
            if surd% = 0% then L31530  /* Status condition not found */
L31950:     surd$, sur$ = " "
            call "DATE" addr("G+", pldate$, surd%-1%, surd$, err%)
            call "DATEFMT" (surd$)
            sur% = pip%(surd%) - (max% + max(0%, cumf%(surd%)))
            shlf% = pip%(surd%) - max(0%, cumf%(surd%))
            convert sur% to sur$, pic(-#######)
            convert sur% to stqty$, pic(-#######)
            convert shlf% to stshelf$, pic(-#######)
            convert pip%(surd%) to stpip$, pic(-#######)
            stdate$ = surd$
            stlit$ = "S"
*          STLIT$ = "SURPLUS CONDITION"
            return

L32050
*       * Next come the Safety Stock Intrusions
            if stat = 10 and (stat$ = "4" or stat$ = "5") then L32070
            if stat > 5 then L32220
L32070:     for j% = today% to 490%
                if ssid% > 0% then L32120
                if pip%(j%) - max(0%, cumf%(j%)) >= ss% then L32110
                    if ssid% = 0% then ssid% = j%
L32110:     next j%
            if ssid% = 0% then L31530  /* Status condition not found */
L32120:     ssid$, ssi$ = " "
            call "DATE" addr("G+", pldate$, ssid%-1%, ssid$, err%)
            call "DATEFMT" (ssid$)
            ssi% = ss% - (pip%(ssid%) - max(0%, cumf%(ssid%)))
            shlf% = pip%(ssid%) - max(0%, cumf%(ssid%))
            convert ssi% to ssi$, pic(-#######)
            convert ssi% to stqty$, pic(-#######)
            convert shlf% to stshelf$, pic(-#######)
            convert pip%(ssid%) to stpip$, pic(-#######)
            stdate$ = ssid$
            stlit$ = "I"
*          STLIT$ = "SAFETY STOCK INTRUSION"
            return

L32220
*       * The only thing left is Critical Shortages
            if stat = 10 and (stat$ = "8" or stat$ = "9") then L32240
            if stat < 8 then return
L32240:     for j% = today% to 490%
                if csd% > 0% then L32290
                if pip%(j%) - max(0%, cumf%(j%)) >= 0% then L32280
                    if csd% = 0% then csd% = j%
L32280:     next j%
            if csd% = 0% then L31530   /* Status condition not found */
L32290:     csd$, cs$ = " "
            call "DATE" addr("G+", pldate$, csd%-1%, csd$, err%)
            call "DATEFMT" (csd$)
            cs% = pip%(csd%) - max(0%, cumf%(csd%))
            shlf% = pip%(csd%) - max(0%, cumf%(csd%))
            convert cs% to cs$, pic(-#######)
            convert cs% to stqty$, pic(-#######)
            convert shlf% to stshelf$, pic(-#######)
            convert pip%(csd%) to stpip$, pic(-#######)
            stdate$ = csd$
            stlit$ = "C"
*          STLIT$ = "CRITICAL SHORTAGE"
            return

        REM *************************************************************~
            *        M A I N   L O G I C   S E C T I O N                *~
            *-----------------------------------------------------------*~
            * Whatever you requested is done here, in Display Mode      *~
            *************************************************************

        display_it
            if pipsw > 1 then see_pipout
            ttl$ = "Review Expected Inventory ADDITIONS"

        see_pipin
            gosub getpipinkey
L35120:     init(" ") part$(), qty$(),  partdescr$(), actdate$(),        ~
                      stdate$(), indate$(), tagnr$()
            for line% = 1% to 8%
                gosub L30000         /* Get next data record */
                if success% = 0% then L35280

                    part$   (line%) = part$
                    stdate$ (line%) = datestart$
                    actdate$(line%) = dateact$
                    indate$ (line%) = datein$
                    tagnr$  (line%) = tag$
                    qty$    (line%) = qty$
                    call "STRING" addr("LJ", qty$(line%), 10%)
                    partdescr$(line%) = partdescr$
            next line%

L35280:     if success% = 0% then partdescr$(line%) =                    ~
                             hex(85) & "* * * END OF DISPLAY * * *"

L35310:     gosub'072      /* Display Information */
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then see_pipin
                if keyhit% =  5% then L35120
                  savekey$ =  pipinkey$

                if keyhit% = 30% then gosub print_it
                 pipinkey$ = savekey$
                if keyhit% = 16% then inputmode
                go to L35310

        see_pipout
            if pipsw > 2 then see_pipmastr
            ttl$ = "View Expected Inventory WITHDRAWALS"
            headerline$ = "Part Number / Description           Date Out" ~
                        & "     Quantity   Tag Number"
L35480:     gosub getpipoutkey:

L35500:     init (" ") line$()
            for line% = 1% to 15% step 2%
                gosub L30000         /* Get next data record */
                if success% = 0% then L35630
                    REM Build line$(line%)
                    str(line$,1 )  = part$
                    str(line$,37)  = dateout$
                    str(line$,48)  = qty$
                    str(line$,61)  = tag$
                    line$(line%) = line$
                    line$(line%+1%) = "  " & partdescr$
            next line%

L35630:     if success% = 0% then str(line$(line%),26) =                 ~
                             hex(85) & "* * * END OF LISTING * * *"

L35660:     gosub'075      /* Display Information */
                if keyhit% =  1% then gosub startover
                if keyhit% =  2% then L35480
                if keyhit% =  5% then L35500
                  savekey$ =  pipoutkey$

                if keyhit% = 30% then gosub print_pipout
                pipoutkey$ = savekey$

                if keyhit% = 16% then inputmode
                go to L35660

        see_pipmastr
            edtmessage3$= "To see planning information, TAB cursor to des~
        ~ired part and press RETURN."

            ttl$ = "View Planned Inventory Position Part Status"

            gosub getpipmastrkey
            if stat > 7 then do_critical
            if stat > 3 then do_intrusion

        do_surplus
            init(" ") part$(), qty$(),  ss$(),   moq$(),  partdescr$(),  ~
                      sur$(),  surd$(), dqty$(), ddate$()
            for line% = 1% to 8%
                gosub L30000         /* Get next data record */
                if success% = 0% then L36060
                    linekey$(line%) = part$

*       * Build Display
                    part$(line%)     = part$
                    qty$(line%)      = qty$
                    ss$(line%)       = ss$
                    moq$(line%)      = moq$
                    sur$(line%)      = sur$
                    surd$(line%)     = surd$
                    partdescr$(line%)= partdescr$
            next line%

L36060:     mat dqty$ = sur$  :  mat ddate$ = surd$

L36080:     if success% = 0% then partdescr$(line%) =                    ~
                             hex(85) & "* * * End Of Display * * *"

            gosub'071(1%)    /* Display Information */
                if keyhit% =  1% then gosub startover
                if keyhit% <> 2% then       L36150
                gosub getpipmastrkey  : goto do_surplus
L36150:         if keyhit% =  5% then        do_surplus
                                 savekey$ =  pipmastrkey$
                if keyhit% = 30% then gosub print_pipmastr
                                 pipmastrkey$ = savekey$
                if keyhit% = 16% then       inputmode
                if keyhit% =  0% then gosub plnrevue
                goto L36080

        do_intrusion
            init(" ") part$(), qty$(),  ss$(),   moq$(),  partdescr$(),  ~
                      ssi$(),  ssid$(), dqty$(), ddate$()
            for line% = 1% to 8%
                gosub L30000         /* Get next data record */
                if success% = 0% then L36410
                    linekey$(line%) = part$

*       * Build Display
                    part$(line%)     = part$
                    qty$(line%)      = qty$
                    ss$(line%)       = ss$
                    ssi$(line%)      = ssi$
                    ssid$(line%)     = ssid$
                    moq$(line%)      = moq$
                    partdescr$(line%)= partdescr$
            next line%

L36410:     mat dqty$ = ssi$  :  mat ddate$ = ssid$

            if success% = 0% then partdescr$(line%) =                    ~
                             hex(85) & "* * * End Of Display * * *"

L36460:     gosub'071(2%)    /* Display Information */
                if keyhit% =  1% then gosub startover
                if keyhit% <> 2% then       L36500
                gosub getpipmastrkey  : goto do_intrusion
L36500:         if keyhit% =  5% then        do_intrusion
                                 savekey$ =  pipmastrkey$
                if keyhit% = 30% then gosub print_pipmastr
                                 pipmastrkey$ = savekey$
                if keyhit% = 16% then       inputmode
                if keyhit% =  0% then gosub plnrevue
                goto L36460


        do_critical
            init(" ") part$(), qty$(), ss$(),   moq$(),  partdescr$(),   ~
                      cs$(),   csd$(), dqty$(), ddate$()
            for line% = 1% to 8%
                gosub L30000         /* Get next data record */
                if success% = 0% then L36770
                    linekey$(line%) = part$

*       * Build Display
                    part$(line%)     = part$
                    qty$(line%)      = qty$
                    ss$(line%)       = ss$
                    cs$(line%)       = cs$
                    csd$(line%)      = csd$
                    moq$(line%)      = moq$
                    partdescr$(line%)= partdescr$
            next line%

L36770:     mat dqty$ = cs$  :  mat ddate$ = csd$

            if success% = 0% then partdescr$(line%) =                    ~
                             hex(85) & "* * * End Of Display * * *"

L36820:     gosub'071(3)     /* Display Information */
                if keyhit% =  1% then gosub startover
                if keyhit% <> 2% then       L36860
                gosub getpipmastrkey  : goto do_critical
L36860:         if keyhit% =  5% then        do_critical
                                 savekey$ =  pipmastrkey$
                if keyhit% = 30% then gosub print_pipmastr
                                 pipmastrkey$ = savekey$
                if keyhit% = 16% then       inputmode
                if keyhit% =  0% then gosub plnrevue
                goto L36820

        REM *************************************************************~
            *           Set Up PIP File KEYS                            *~
            *************************************************************
        getpipinkey:
            file% = 1%
*       * Decide on sort and create PIPINKEY
            init (hex(00)) pipinkey$
            sort% = 0%                 /*  Primary Key, by Tag Number  */
            if sort$ = "P" then L37120
            str(pipinkey$,1,19) = str(lotag$,1,19)
            return

L37120
*       * Sort on alternate PIPIN key for Part Number
            sort% = 1%
            str(pipinkey$,1,25) = str(lopart$,1,25)
            return

        getpipoutkey:
            file% = 2%
*       * Decide on sort and create PIPOUTKEY and BREAK%
            init (hex(00)) pipoutkey$
            sort% = 0%                 /*  Primary Key, by Tag Number  */
            if sort$ = "P" then L37260
            str(pipoutkey$,1,19) = str(lotag$,1,19)
            return

L37260
*       * Sort on alternate PIPOUT key for Part Number
            sort% = 1%
            str(pipoutkey$,1,25) = str(lopart$,1,25)
            return

        getpipmastrkey:
            file% = 3%
*       * Decide on sort and create PIPMASTRKEY and BREAK%
            init (hex(00)) pipmastrkey$
            break%, sort% = 0%              /*  Primary key, By Part  */
            if stat < 10 then L37400
            str(pipmastrkey$,1,25) = str(lopart$,1,25)
            return

L37400
*       * Sort on alternate PIPMASTR key by Status Code/Part
            break%, sort% = 1%
            str(pipmastrkey$,1,1) = str(fromstat$,,1)
            str(pipmastrkey$,2,25) = str(lopart$,1,25)
            return

        REM *************************************************************~
            *           STOLEN DIRECTLY FROM PLNREVUE                   *~
            *************************************************************

        plnrevue
            loc% = min(8%, int((cursor%(1)-3%)/2))
            plankey$ = linekey$(loc%)
L37530:     gosub first_screen
            mode% = 0%

            if keyhit% = 1% then mode% = 1%
            if keyhit% = 2% then mode% = 2%
            if keyhit% = 3% then mode% = 5%
            if keyhit% = 4% then mode% = 6%
            if keyhit% = 5% then mode% = 8%
            if keyhit% = 6% then mode% = 4%
            if keyhit% = 7% then mode% = 9%
            if keyhit% = 8% then mode% = 3%
            if keyhit% = 9% then mode% = 7%
            if keyhit% =16% then return
            if mode% = 0% then L37530

            call "PLNRSUB" (mode%, plankey$,                             ~
                            #46, #4, #6, #7, #11, #12, #15, #23,         ~
                            #24, #2, #3, #35, #40, #41, #45, #17, #16)

            goto L37530

        REM *************************************************************~
            *             PRINT ROUTINES FOR THE FILES                  *~
            *************************************************************
        print_it
            if pipsw <> 1 then print_pipout
            msg$ = "Printing Expected Inventory Additions Report"
            gosub getpipinkey
            goto L38210

        print_pipout:
            if pipsw <> 2 then print_pipmastr
            msg$ = "Printing Expected Inventory Withdrawals Report"
            gosub getpipoutkey
            goto L38210

        print_pipmastr:
            msg$ = "Printing Planned Inventory Position Status Report"
            gosub getpipmastrkey


L38210: select printer (134)
        call "SETPRNT" ("PIP002", " ", 0%, 0%)
        call "SHOSTAT" (msg$)

            lin%, pag%  = -1%
            gosub print_params
            gosub'61(file%)
L38270:     gosub L30000        /* Get a line from file */
                               /* FILE% and SORT% already defined */
            if success% = 0% then L38350
                if lin% > 58% then gosub'61(file%)     /*new page */
                lin% = lin% + 1%
                gosub'62(file%)                   /* print a line */
                goto L38270

L38350:         print skip(2) : print using L60480
                close printer
                call "SETPRNT" ("PIP002", " ", 0%, 1%)
                return

        deffn'61(i%)
            if pag% > 0% then L38430
            m% = pipsw
            n% = docsw
            if m% = 3% then n% = stat
L38430:     pag% = pag% + 1%
            lin% = 5%

            print page
            print using L60060, date$, time$, company$
            print using L60100, reporttitle$(m%,n%), pag%
            print skip(1)
            if pag% = 0% then return
            on i% goto L38520, L38540, L38560

L38520:         print using L60300 : print using L60330 : return

L38540:         print using L60390 : print using L60420 : return

L38560:         print legend$ : print skip(1)
                print using L60180 : lin% = 8%
                print using L60140 : print using L60220 : return

        deffn'62(i%)
            on i% goto L38610, L38650, L38690

L38610:         print using L60360, part$, partdescr$, datestart$,        ~
                                   dateact$, datein$, qty$, tag$
                return

L38650:         print using L60450, part$, partdescr$, dateout$,          ~
                                   qty$, tag$
                return

L38690:         print using L60260, part$, partdescr$, qty$, ss$,         ~
                                   moq$, stlit$, stshelf$, stpip$,       ~
                                   stdate$, stqty$
                return

        print_params

            gosub'61(file%)

L38764:     ii% = pos(str(j$()) > hex(7f))
            if ii% = 0% then L38780
                str(j$(), ii%, 1%) = hex(20)
                goto L38764

L38780:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 5% to 17%: print tab(37); j$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L39060: FMT                 /* FILE: PIPIN                             */~
            CH(25),         /* Part code                               */~
            BI(4),          /* Date in subscript for PIP               */~
            CH(19),         /* Tag number in level 2 planning          */~
            PD(14,4),       /* Quantity of Something                   */~
            BI(4)           /* Date to start as a date subscript in lev*/~

L39130: FMT                 /* FILE: PIPOUT                            */~
            CH(19),         /* Tag number in level 2 planning          */~
            CH(25),         /* Part code                               */~
            BI(4),          /* Date out of PIP in date subscript form f*/~
            XX(8),          /* Time from the system clock              */~
            PD(14,4)        /* Quantity of Something                   */~

L39200: FMT                 /* FILE: PIPMASTR                          */~
            CH(1),          /* General purpose status indicator        */~
            CH(25),         /* Part code                               */~
            490*BI(4),      /* Planned inventory position              */~
            PD(14,4),       /* Quantity of Something                   */~
            PD(14,4),       /* Safety stock                            */~
            PD(14,4),       /* minimum order qty. / minimum order multi*/~
            XX(14)          /* Remainder of record                     */

L39300: FMT                 /* FILE: SFCUM2                            */~
            XX(25),         /* Part code                               */~
            490*BI(4)       /* Cumulative Sales Forcast array          */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              str(line2$,,60) = "Selection for Review and/or Print"
              if fieldnr% > 0% then init(hex(8c)) lfac$(), sfac$(2)      ~
                               else init(hex(86)) lfac$(), sfac$(2)
              if pipsw = 3 then sfac$(2) = hex(9c)

              on fieldnr% gosub L40115,         /* PIP Type          */   ~
                                L40115,         /* Doc Type          */   ~
                                L40115,         /* Using What Date?  */   ~
                                L40115,         /* Part Number Range */   ~
                                L40115,         /* Date Range        */   ~
                                L40115,         /* Tag Number Range  */   ~
                                L40115,         /* Doc Status        */   ~
                                L40125          /* Sort Select       */
              goto L40135

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40115:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
L40125:           sfac$(2)        = hex(81)  :  return  /* Upper Only */

L40135:     accept                                                       ~
               at (01,02),                                               ~
                  "PLANNED INVENTORY POSITION DISPLAY",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Adds, Wdwals, or Status?",                   ~
               at (05,30), fac(lfac$( 1)), pipsw$               , ch(01),~
               at (05,35), fac(hex(8c)),   pipswdescr$          , ch(40),~
                                                                         ~
               at (06,02), "Type of Adds or Wdwals?",                    ~
               at (06,30), fac(lfac$( 2)), docsw$               , ch(03),~
               at (06,35), fac(hex(8c)),   docswdescr$          , ch(40),~
                                                                         ~
               at (07,02), "Using What Date?",                           ~
               at (07,30), fac(lfac$( 3)), datesw$              , ch(01),~
               at (07,35), fac(hex(8c)),   dateswdescr$         , ch(40),~
                                                                         ~
               at (09,30), fac(hex(ac)), col_header$            , ch(50),~
                                                                         ~
               at (10,02), "Part Number Range",                          ~
               at (10,30), fac(lfac$( 4)), frompart$            , ch(25),~
               at (10,56), fac(lfac$( 4)), topart$              , ch(25),~
                                                                         ~
               at (11,02), "Date Range",                                 ~
               at (11,30), fac(lfac$( 5)), fromdate$            , ch(08),~
               at (11,56), fac(lfac$( 5)), todate$              , ch(08),~
                                                                         ~
               at (12,02), "PIP Tag Number Range",                       ~
               at (12,30), fac(lfac$( 6)), fromtag$             , ch(19),~
               at (12,56), fac(lfac$( 6)), totag$               , ch(19),~
                                                                         ~
               at (13,02), "Part Status",                                ~
               at (13,30), fac(lfac$( 7)), fromstat$            , ch( 3),~
               at (13,35), fac(hex(8c)),   statdescr$           , ch(40),~
                                                                         ~
               at (14,02), fac(sfac$( 1)), sortmsg$             , ch(25),~
               at (14,30), fac(sfac$( 2)), sort$                , ch( 1),~
               at (14,35), fac(sfac$( 1)), sortdescr$           , ch(25),~
                                                                         ~
               at (15,02), fac(hex(8c)),   infomsg$(1%)         , ch(79),~
               at (16,02), fac(hex(8c)),   infomsg$(2%)         , ch(79),~
               at (17,02), fac(hex(8c)),   infomsg$(3%)         , ch(79),~
               at (18,02), fac(hex(8c)),   infomsg$(4%)         , ch(79),~
               at (19,02), fac(hex(8c)),   infomsg$(5%)         , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40420
                  call "MANUAL" ("PIPDSUB ") : goto L40135

L40420:        if keyhit% <> 15% then L40435
                  call "PRNTSCRN" : goto L40135

L40435:        close ws
               call "SCREEN" addr ("C", u3%, "I", j$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40530     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40510
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40510:     if fieldnr% > 2% then L40520
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40520:     return

L40530: if fieldnr% > 0% then L40580  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(14)Display Data       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(30)Print Report       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f10001e)
            return

L40580
*        Edit Mode - Field Enabled
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *    D I S P L A Y   S T A T U S   S C R E E N              *~
            *-----------------------------------------------------------*~
            * Display Screen for All Status Types...                    *~
            *************************************************************
        deffn'071(type%)
            fac$ = hex(8c)
            gosub set_line2

L41045: accept                                                           ~
               at (01,02), fac(hex(8c)), ttl$                   , ch(60),~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(ac)), headline$(type%)       , ch(79),~
                                                                         ~
               at (05,02),  fac(hex(8c)), part$( 1)             , ch(25),~
               at (05,31),  fac(hex(8c)), ss$  ( 1)             , ch(10),~
               at (05,42),  fac(hex(8c)), moq$ ( 1)             , ch(10),~
               at (05,53),  fac(hex(8c)), qty$ ( 1)             , ch(10),~
               at (05,64),  fac(hex(84)), dqty$( 1)             , ch( 8),~
               at (05,73),  fac(hex(84)), ddate$(1)             , ch( 8),~
               at (06,04),  fac(hex(8c)), partdescr$( 1)        , ch(32),~
                                                                         ~
               at (07,02),  fac(hex(8c)), part$( 2)             , ch(25),~
               at (07,31),  fac(hex(8c)), ss$  ( 2)             , ch(10),~
               at (07,42),  fac(hex(8c)), moq$ ( 2)             , ch(10),~
               at (07,53),  fac(hex(8c)), qty$ ( 2)             , ch(10),~
               at (07,64),  fac(hex(84)), dqty$( 2)             , ch( 8),~
               at (07,73),  fac(hex(84)), ddate$(2)             , ch( 8),~
               at (08,04),  fac(hex(8c)), partdescr$( 2)        , ch(32),~
                                                                         ~
               at (09,02),  fac(hex(8c)), part$( 3)             , ch(25),~
               at (09,31),  fac(hex(8c)), ss$  ( 3)             , ch(10),~
               at (09,42),  fac(hex(8c)), moq$ ( 3)             , ch(10),~
               at (09,53),  fac(hex(8c)), qty$ ( 3)             , ch(10),~
               at (09,64),  fac(hex(84)), dqty$( 3)             , ch( 8),~
               at (09,73),  fac(hex(84)), ddate$(3)             , ch( 8),~
               at (10,04),  fac(hex(8c)), partdescr$( 3)        , ch(32),~
                                                                         ~
               at (11,02),  fac(hex(8c)), part$( 4)             , ch(25),~
               at (11,31),  fac(hex(8c)), ss$  ( 4)             , ch(10),~
               at (11,42),  fac(hex(8c)), moq$ ( 4)             , ch(10),~
               at (11,53),  fac(hex(8c)), qty$ ( 4)             , ch(10),~
               at (11,64),  fac(hex(84)), dqty$( 4)             , ch( 8),~
               at (11,73),  fac(hex(84)), ddate$(4)             , ch( 8),~
               at (12,04),  fac(hex(8c)), partdescr$( 4)        , ch(32),~
                                                                         ~
               at (13,02),  fac(hex(8c)), part$( 5)             , ch(25),~
               at (13,31),  fac(hex(8c)), ss$  ( 5)             , ch(10),~
               at (13,42),  fac(hex(8c)), moq$ ( 5)             , ch(10),~
               at (13,53),  fac(hex(8c)), qty$ ( 5)             , ch(10),~
               at (13,64),  fac(hex(84)), dqty$( 5)             , ch( 8),~
               at (13,73),  fac(hex(84)), ddate$(5)             , ch( 8),~
               at (14,04),  fac(hex(8c)), partdescr$( 5)        , ch(32),~
                                                                         ~
               at (15,02),  fac(hex(8c)), part$( 6)             , ch(25),~
               at (15,31),  fac(hex(8c)), ss$  ( 6)             , ch(10),~
               at (15,42),  fac(hex(8c)), moq$ ( 6)             , ch(10),~
               at (15,53),  fac(hex(8c)), qty$ ( 6)             , ch(10),~
               at (15,64),  fac(hex(84)), dqty$( 6)             , ch( 8),~
               at (15,73),  fac(hex(84)), ddate$(6)             , ch( 8),~
               at (16,04),  fac(hex(8c)), partdescr$( 6)        , ch(32),~
                                                                         ~
               at (17,02),  fac(hex(8c)), part$( 7)             , ch(25),~
               at (17,31),  fac(hex(8c)), ss$  ( 7)             , ch(10),~
               at (17,42),  fac(hex(8c)), moq$ ( 7)             , ch(10),~
               at (17,53),  fac(hex(8c)), qty$ ( 7)             , ch(10),~
               at (17,64),  fac(hex(84)), dqty$( 7)             , ch( 8),~
               at (17,73),  fac(hex(84)), ddate$(7)             , ch( 8),~
               at (18,04),  fac(hex(8c)), partdescr$( 7)        , ch(32),~
                                                                         ~
               at (19,02),  fac(hex(8c)), part$( 8)             , ch(25),~
               at (19,31),  fac(hex(8c)), ss$  ( 8)             , ch(10),~
               at (19,42),  fac(hex(8c)), moq$ ( 8)             , ch(10),~
               at (19,53),  fac(hex(8c)), qty$ ( 8)             , ch(10),~
               at (19,64),  fac(hex(84)), dqty$( 8)             , ch( 8),~
               at (19,73),  fac(hex(84)), ddate$(8)             , ch( 8),~
               at (20,04),  fac(hex(8c)), partdescr$( 8)        , ch(32),~
                                                                         ~
               at (22,02),  fac(hex(a4)), edtmessage3$          , ch(79),~
                                                                         ~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), "(2)First",                                   ~
               at (23,45), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), "(5)Next         ",                           ~
               at (24,45), "(30)Print Report",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Select Again",                           ~
                                                                         ~
               keys(hex(000102050d0e0f101e)),                            ~
               key(keyhit%)


               if keyhit% <> 13% then L41495
                  call "MANUAL" ("PIPDSUB ")
                  goto L41045

L41495:        if keyhit% <> 15% then L41515
                  call "PRNTSCRN"
                  goto L41045

L41515:     close ws
            call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *    D I S P L A Y   A D D I T I O N S   S C R E E N        *~
            *-----------------------------------------------------------*~
            * Display Screen for Expected Inventory Additions           *~
            *************************************************************
        deffn'072
            headerline$ = "Part Number / Description        Pln Str" &   ~
                          "t  Act Strt  Plnd In   Qty / Tag Number"
            gosub set_line2

L42100: accept                                                           ~
               at (01,02), fac(hex(8c)), ttl$                   , ch(60),~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(ac)), headerline$            , ch(79),~
                                                                         ~
               at (05,02),  fac(hex(8c)), part$     ( 1)        , ch(25),~
               at (05,35),  fac(hex(8c)), stdate$   ( 1)        , ch( 8),~
               at (05,45),  fac(hex(8c)), actdate$  ( 1)        , ch( 8),~
               at (05,55),  fac(hex(8c)), indate$   ( 1)        , ch( 8),~
               at (05,65),  fac(hex(84)), qty$      ( 1)        , ch(10),~
               at (06,61),  fac(hex(8c)), tagnr$    ( 1)        , ch(19),~
               at (06,04),  fac(hex(8c)), partdescr$( 1)        , ch(32),~
                                                                         ~
               at (07,02),  fac(hex(8c)), part$     ( 2)        , ch(25),~
               at (07,35),  fac(hex(8c)), stdate$   ( 2)        , ch( 8),~
               at (07,45),  fac(hex(8c)), actdate$  ( 2)        , ch( 8),~
               at (07,55),  fac(hex(8c)), indate$   ( 2)        , ch( 8),~
               at (07,65),  fac(hex(84)), qty$      ( 2)        , ch(10),~
               at (08,61),  fac(hex(8c)), tagnr$    ( 2)        , ch(19),~
               at (08,04),  fac(hex(8c)), partdescr$( 2)        , ch(32),~
                                                                         ~
               at (09,02),  fac(hex(8c)), part$     ( 3)        , ch(25),~
               at (09,35),  fac(hex(8c)), stdate$   ( 3)        , ch( 8),~
               at (09,45),  fac(hex(8c)), actdate$  ( 3)        , ch( 8),~
               at (09,55),  fac(hex(8c)), indate$   ( 3)        , ch( 8),~
               at (09,65),  fac(hex(84)), qty$      ( 3)        , ch(10),~
               at (10,61),  fac(hex(8c)), tagnr$    ( 3)        , ch(19),~
               at (10,04),  fac(hex(8c)), partdescr$( 3)        , ch(32),~
                                                                         ~
               at (11,02),  fac(hex(8c)), part$     ( 4)        , ch(25),~
               at (11,35),  fac(hex(8c)), stdate$   ( 4)        , ch( 8),~
               at (11,45),  fac(hex(8c)), actdate$  ( 4)        , ch( 8),~
               at (11,55),  fac(hex(8c)), indate$   ( 4)        , ch( 8),~
               at (11,65),  fac(hex(84)), qty$      ( 4)        , ch(10),~
               at (12,61),  fac(hex(8c)), tagnr$    ( 4)        , ch(19),~
               at (12,04),  fac(hex(8c)), partdescr$( 4)        , ch(32),~
                                                                         ~
               at (13,02),  fac(hex(8c)), part$     ( 5)        , ch(25),~
               at (13,35),  fac(hex(8c)), stdate$   ( 5)        , ch( 8),~
               at (13,45),  fac(hex(8c)), actdate$  ( 5)        , ch( 8),~
               at (13,55),  fac(hex(8c)), indate$   ( 5)        , ch( 8),~
               at (13,65),  fac(hex(84)), qty$      ( 5)        , ch(10),~
               at (14,61),  fac(hex(8c)), tagnr$    ( 5)        , ch(19),~
               at (14,04),  fac(hex(8c)), partdescr$( 5)        , ch(32),~
                                                                         ~
               at (15,02),  fac(hex(8c)), part$     ( 6)        , ch(25),~
               at (15,35),  fac(hex(8c)), stdate$   ( 6)        , ch( 8),~
               at (15,45),  fac(hex(8c)), actdate$  ( 6)        , ch( 8),~
               at (15,55),  fac(hex(8c)), indate$   ( 6)        , ch( 8),~
               at (15,65),  fac(hex(84)), qty$      ( 6)        , ch(10),~
               at (16,61),  fac(hex(8c)), tagnr$    ( 6)        , ch(19),~
               at (16,04),  fac(hex(8c)), partdescr$( 6)        , ch(32),~
                                                                         ~
               at (17,02),  fac(hex(8c)), part$     ( 7)        , ch(25),~
               at (17,35),  fac(hex(8c)), stdate$   ( 7)        , ch( 8),~
               at (17,45),  fac(hex(8c)), actdate$  ( 7)        , ch( 8),~
               at (17,55),  fac(hex(8c)), indate$   ( 7)        , ch( 8),~
               at (17,65),  fac(hex(84)), qty$      ( 7)        , ch(10),~
               at (18,61),  fac(hex(8c)), tagnr$    ( 7)        , ch(19),~
               at (18,04),  fac(hex(8c)), partdescr$( 7)        , ch(32),~
                                                                         ~
               at (19,02),  fac(hex(8c)), part$     ( 8)        , ch(25),~
               at (19,35),  fac(hex(8c)), stdate$   ( 8)        , ch( 8),~
               at (19,45),  fac(hex(8c)), actdate$  ( 8)        , ch( 8),~
               at (19,55),  fac(hex(8c)), indate$   ( 8)        , ch( 8),~
               at (19,65),  fac(hex(84)), qty$      ( 8)        , ch(10),~
               at (20,61),  fac(hex(8c)), tagnr$    ( 8)        , ch(19),~
               at (20,04),  fac(hex(8c)), partdescr$( 8)        , ch(32),~
                                                                         ~
               at (22,02),  fac(hex(a4)), edtmessage$           , ch(79),~
                                                                         ~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), "(2)First",                                   ~
               at (23,45), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), "(5)Next         ",                           ~
               at (24,45), "(30)Print Report",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Select Again",                           ~
                                                                         ~
               keys(hex(000102050d0e0f101e)),                            ~
               key(keyhit%)


               if keyhit% <> 13% then L43000
                  call "MANUAL" ("PIPDSUB ")
                  goto L42100

L43000:        if keyhit% <> 15% then L43040
                  call "PRNTSCRN"
                  goto L42100

L43040:     close ws
            call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *        D I S P L A Y   I N F O R M A T I O N              *~
            *-----------------------------------------------------------*~
            * Multi purpose display screen                              *~
            *************************************************************
        deffn'075
            fac$ = hex(8c)
            gosub set_line2

L45100: accept                                                           ~
               at (01,02), fac(hex(8c)), ttl$                   , ch(60),~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(ac)), headerline$            , ch(79),~
                                                                         ~
               at (05,02),  fac( fac$  ), line$( 1)             , ch(79),~
               at (06,02),  fac(hex(8c)), line$( 2)             , ch(79),~
               at (07,02),  fac( fac$  ), line$( 3)             , ch(79),~
               at (08,02),  fac(hex(8c)), line$( 4)             , ch(79),~
               at (09,02),  fac( fac$  ), line$( 5)             , ch(79),~
               at (10,02),  fac(hex(8c)), line$( 6)             , ch(79),~
               at (11,02),  fac( fac$  ), line$( 7)             , ch(79),~
               at (12,02),  fac(hex(8c)), line$( 8)             , ch(79),~
               at (13,02),  fac( fac$  ), line$( 9)             , ch(79),~
               at (14,02),  fac(hex(8c)), line$(10)             , ch(79),~
               at (15,02),  fac( fac$  ), line$(11)             , ch(79),~
               at (16,02),  fac(hex(8c)), line$(12)             , ch(79),~
               at (17,02),  fac( fac$  ), line$(13)             , ch(79),~
               at (18,02),  fac(hex(8c)), line$(14)             , ch(79),~
               at (19,02),  fac( fac$  ), line$(15)             , ch(79),~
               at (20,02),  fac(hex(8c)), line$(16)             , ch(79),~
                                                                         ~
               at (22,02),  fac(hex(a4)), edtmessage$           , ch(79),~
                                                                         ~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), "(2)First",                                   ~
               at (23,45), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), "(5)Next         ",                           ~
               at (24,45), "(30)Print Report",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Select Again",                           ~
                                                                         ~
               keys(hex(000102050d0e0f101e)),                            ~
               key(keyhit%)


               if keyhit% <> 13% then L45550
                  call "MANUAL" ("PIPDSUB ")
                  goto L45100

L45550:        if keyhit% <> 15% then L45590
                  call "PRNTSCRN"
                  goto L45100

L45590:     close ws
            call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *        S E T   L I N E 2   F O R   D I S P L A Y          *~
            *-----------------------------------------------------------*~
            * Set up STR(LINE2$,,60) for appropriate display            *~
            *************************************************************

        set_line2
            str(line2$,,60) =  " "
            if pipsw = 3 then set_stat_line2
            if docsw = 1 then str(line2$,,60%) =                         ~
                         "Displaying Released Jobs"
            if docsw = 2 then str(line2$,,60%) =                         ~
                         "Displaying Work Order Advices"
            if docsw = 3 then str(line2$,,60%) =                         ~
                         "Displaying Sales Orders"
            if docsw = 4 then str(line2$,,60%) =                         ~
                         "Displaying Buy (Purchase) Advices"
            if docsw = 5 then str(line2$,,60%) =                         ~
                         "Displaying Purchase Directives (Requisitions)"
            if docsw = 6 then str(line2$,,60%) =                         ~
                         "Displaying Purchase Orders"
            if docsw = 7 then str(line2$,,60%) =                         ~
                         "Displaying Parts Pending in QC"
            if docsw = 8 then str(line2$,,60%) =                         ~
                         "Displaying Purchase Job Directives (Req's)  "
            if docsw = 9 then str(line2$,,60%) =                         ~
                         "Displaying Buy (Purchase Job) Advices"
            if docsw <> 10 then return
                if pipsw = 1 then str(line2$,,60) =                      ~
                         "Displaying Everything Expected to Come IN"
                if pipsw = 2 then str(line2$,,60) =                      ~
                          "Displaying Everything Expected to Go OUT"
                return

        set_stat_line2
            if fromstat$ = "ALL" then str(line2$,,60) =                  ~
               "Planned Inventory Position for ALL Status Codes"
            if fromstat$ = "2  " then str(line2$,,60) =                  ~
               "Surplus Condition for Manufactured Parts       "
            if fromstat$ = "3  " then str(line2$,,60) =                  ~
               "Surplus Condition for Purchased Parts          "
            if fromstat$ = "4  " then str(line2$,,60) =                  ~
               "Safety Stock Intrusion for Manufactured Parts  "
            if fromstat$ = "5  " then str(line2$,,60) =                  ~
               "Safety Stock Intrusion for Purchased Parts     "
            if fromstat$ = "8  " then str(line2$,,60) =                  ~
               "Critical Shortage Condition for Manufactured Parts"
            if fromstat$ = "9  " then str(line2$,,60) =                  ~
               "Critical Shortage Condition for Purchased Parts "
            return

        REM *************************************************************~
            *        D I S P L A Y   I N F O R M A T I O N              *~
            *-----------------------------------------------------------*~
            * Display like PLANREVUE                                    *~
            *************************************************************

        first_screen
            str(line2$,,60) = " "
            revumsg$ = "Press Appropriate PFKEY to Perform Desired " &   ~
                       "Function"
            ttl1$ = "PFKEY"
            ttl2$ = "Review Function"

        accept                                                           ~
               at (01,02), "Review Functions Selection",                 ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch( 8),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,09), fac(hex(ac)), ttl1$                  , ch( 5),~
               at (06,16), fac(hex(ac)), ttl2$                  , ch(55),~
                                                                         ~
               at (07,11), "1",                                          ~
               at (07,16), "See Planned Inventory Position and Available ~
        ~to Commit",                                                      ~
                                                                         ~
               at (08,11), "2",                                          ~
               at (08,16), "See Inventory Sources and Applications",     ~
                                                                         ~
               at (09,11), "3",                                          ~
               at (09,16), "See Inventory Sales versus Forecasts",       ~
                                                                         ~
               at (11,11), "4",                                          ~
               at (11,16), "Examine Planned Inventory Position Details", ~
                                                                         ~
               at (12,11), "5",                                          ~
               at (12,16), "Examine Work Center Capacities",             ~
                                                                         ~
               at (14,11), "6",                                          ~
               at (14,16), "Review Effectivity Dates",                   ~
                                                                         ~
               at (15,11), "7",                                          ~
               at (15,16), "Review Bills of Materials",                  ~
                                                                         ~
               at (16,11), "8",                                          ~
               at (16,16), "Review Work Center Routings",                ~
                                                                         ~
               at (18,11), "9",                                          ~
               at (18,16), "Examine Current Demand Status",              ~
                                                                         ~
               at (21,02), fac(hex(ac)), revumsg$               , ch(79),~
                                                                         ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return      ",                           ~
                                                                         ~
                                                                         ~
            keys(hex(010203040506070809ffffff0dff0f10)),                 ~
            key(keyhit%)

            if keyhit%<>15% then L47850
                call "PRNTSCRN"
                goto first_screen

L47850:     if keyhit%<>13% then L47880
                call "MANUAL" ("PIPDSUB ")

L47880:     close ws
            call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* PIP Type          */     ~
                              L50200,         /* Doc Type          */     ~
                              L50300,         /* Using What Date?  */     ~
                              L50400,         /* Part Number Range */     ~
                              L50500,         /* Date Range        */     ~
                              L50700,         /* Tag Number Range  */     ~
                              L50800,         /* Part Status       */     ~
                              L50900          /* Sort Select       */
            return
L50100: REM Test for Adds, Wdwals, or Status?     PIPSW$
            if pipsw$ = "A" then pipsw = 1
            if pipsw$ = "W" then pipsw = 2
            if pipsw$ = "S" then pipsw = 3
            if pipsw < 1 or pipsw > 3 then L50170
            if pipsw = 1 then pipswdescr$ = "Expected Inventory Additions~
        ~ "
            if pipsw = 2 then pipswdescr$ = "Expected Inventory Withdrawa~
        ~ls"
            if pipsw = 3 then pipswdescr$ = "Planned Inventory Position S~
        ~tatus"
            return
L50170:     errormsg$ = "You Must Enter 'A', 'W' or 'S'"
            return

L50200: REM Test for Type of Adds or Wdwals?      DOCSW$
            if pipsw = 3 then return
            docsw = 0
            if docsw$ = "JO " then docsw = 1
            if docsw$ = "WO " then docsw = 2
            if docsw$ = "SO " then docsw = 3
            if docsw$ = "BO " then docsw = 4
            if docsw$ = "RO " then docsw = 5
            if docsw$ = "PO " then docsw = 6
            if docsw$ = "QC " then docsw = 7
            if docsw$ = "BW " then docsw = 8
            if docsw$ = "RW " then docsw = 9
            if docsw$ = "ALL" then docsw = 10
            if pipsw <> 1 then L50260
                if pipsw = 1 and docsw > 0 and docsw < 11 then L50280     ~
                                                          else L50268
L50260:     if pipsw = 2 and (docsw > 0 and docsw <  4) then L50280
            if pipsw = 2 and (docsw > 7 and docsw < 11) then L50280
L50268:         errormsg$ = "PIP Tag type must selected from the list" & ~
                            " below"
                return
L50280:     gosub get_doc_type
            return

L50300: REM Test for Using What Date?             DATESW$
            datesw = 0
            if pipsw = 3 then return
            if datesw$ = "D" then datesw = 1
            if datesw$ = "S" then datesw = 2
            if datesw$ = "A" then datesw = 3
            if datesw < 1 or datesw > 3 then L50372
            if datesw <> 3 then L50360
                if pipsw <> 1 then L50348
                    if docsw = 1 or docsw = 5 or docsw = 6 or docsw = 9  ~
                                    then L50340 else L50348
L50340:             dateswdescr$ = "Using Actual Date"
                    return
L50348:         errormsg$ = "Actual Date Valid ONLY for ADDITIONS type" &~
                            " 'JO', 'RO', 'PO', 'RW'"
                return
L50360:     if datesw = 1 then dateswdescr$ = "Using Due Date"
            if datesw = 2 then dateswdescr$ = "Using Start Date"
            return
L50372:     errormsg$ = "Must be 'D' for Due Date or 'S' for Start " &   ~
                        "Date or 'A' for Actual Date"
            return

L50400: REM Test for Part Number Range            FROMPART$
            call "TESTRNGE" (frompart$, topart$, lopart$, hipart$,       ~
                             errormsg$)
            return

L50500: REM Test for Date Range                   FROMDATE$
            if pipsw = 3 then return
            if fromdate$ <> "PLAN" then L50510
                fdate% = 1% : tdate% = 490% : todate$ = " "
                return
L50510:     if fromdate$ <> "ALL" then L50521
                fdate% = -300001% : tdate% = 300001% : todate$ = " "
                return
L50521:     if fromdate$ <> "FIRST" then L50525
                fdate% = -300001%
                goto L50600
L50525:     call "DATEOK" (fromdate$, u3%, errormsg$)
                if errormsg$ <> " " then return
            j$ = fromdate$ : call "DATUNFMT" (j$)
            call "DATE" addr("G-", pldate$, j$, fdate%, err%)
            if err% = 0% then L50570
               errormsg$ = "Invalid format for the FROM date"
               return

L50570:     fdate% = fdate% + 1%
            if todate$ <> " " and todate$ <> blankdate$ then L50600
                todate$ = fromdate$ : tdate% = fdate%
                return

L50600
*       * Test data for TO date
            if todate$ <> "LAST" then L50605
                tdate% = 300001%
                return
L50605:     call "DATEOK" (todate$, u3%, errormsg$)
                if errormsg$ <> " " then return
            j$ = todate$ : call "DATUNFMT" (j$)
            call "DATE" addr("G-", pldate$, j$, tdate%, err%)
            if err% = 0% then L50655
               errormsg$ = "Invalid format for the TO date"
               return

L50655:     tdate% = tdate% + 1%
            if tdate% < fdate% then errormsg$ = "Invalid date range"
            return

L50700: REM Test for Document Number Range        FROMTAG$
            call "TESTRNGE" (fromtag$, totag$, lotag$, hitag$, errormsg$)
            return

L50800: REM Test for Part Status                  FROMSTAT$
            if fromstat$ = "ALL" then stat = 10
            if fromstat$ = "ALL" then return
            convert fromstat$ to stat, data goto L50845
            if stat < 2 then L50845
            if stat > 3 then L50830
                statdescr$ = "Surplus Condition"       : return
L50830:     if stat > 5 then L50840
                statdescr$ = "Safety Stock Intrusion"  : return
L50840:     if stat > 7 then L50855
L50845:         errormsg$ = "Enter Status Codes '2' - '5' or '8' or '9'"
                return
L50855:     if stat > 9 then L50845
                statdescr$ = "Critical Shortage"
                return

L50900: REM Test for Sort Select                  SORT$
            sortdescr$ = " "
            if sort$ = "P" or sort$ = "T" then L50950
                errormsg$ = "Must enter 'P' or 'T' "
                return
L50950:     if sort$ = "P" then sortdescr$ = "Sorted by Part Number"
            if sort$ = "T" then sortdescr$ = "Sorted by PIP Tag Number"
            return

        get_doc_type
            if docsw =  1 then docswdescr$ = "Released Jobs"
            if docsw =  2 then docswdescr$ = "Work Order Advices"
            if docsw =  3 then docswdescr$ = "Sales Orders"
            if docsw =  4 then docswdescr$ = "Buy (Purchase) Advices"
            if docsw =  5 then docswdescr$ = "Purchase Directives " &    ~
                                             "(Requisitions)"
            if docsw =  6 then docswdescr$ = "Purchase Orders"
            if docsw =  7 then docswdescr$ = "Pending in QC"
            if docsw =  8 then docswdescr$ = "Buy (Purchase Job) Advices"
            if docsw =  9 then docswdescr$ = "Purchase Job Directives " &~
                                             "(Requisitions)"
            if docsw = 10 then docswdescr$ = "See Everything"
            return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * Image statements for printed report                       *~
            *************************************************************

L60060: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                      PIPDSPLY: PIP~
        ~002

L60100: %                                  ##############################~
        ~##############################                          PAGE: ###~
        ~##

L60140: %PART NUMBER               PART DESCRIPTION                 QTY O~
        ~N HND  SSI LEVEL     M.O.Q. STAT  SHELF   P.I.P.   ON DATE  QUANT~
        ~ITY

L60180: %                                                                ~
        ~                                                            PROBL~
        ~EM

L60220: %------------------------- -------------------------------- -----~
        ~----- ---------- ---------- ---- -------- -------- -------- -----~
        ~---

L60260: %######################### ################################ #####~
        ~##### ########## ##########   #  ######## ######## ######## #####~
        ~###

L60300: %     PART NUMBER                 PART DESCRIPTION               ~
        ~   PLN STRT  ACT STRT   PLND IN    QUANTITY  TAG NUMBER

L60330: %     -------------------------   -------------------------------~
        ~-  --------  --------  --------  ----------  -------------------

L60360: %     #########################   ###############################~
        ~#  ########  ########  ########  ##########  ###################

L60390: %            PART NUMBER                 PART DESCRIPTION        ~
        ~           DATE OUT     QUANTITY   TAG NUMBER

L60420: %            -------------------------   ------------------------~
        ~--------   --------   ----------   -------------------

L60450: %            #########################   ########################~
        ~########   ########   ##########   ###################

L60480: %                                        * * * * *  E N D   O F  ~
        ~ R E P O R T  * * * * *

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end
