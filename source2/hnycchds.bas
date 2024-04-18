        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   H   H  DDDD    SSS    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  H   H  D   D  S       *~
            *  HHHHH  N N N   YYY   C      C      HHHHH  D   D   SSS    *~
            *  H   H  N  NN    Y    C   C  C   C  H   H  D   D      S   *~
            *  H   H  N   N    Y     CCC    CCC   H   H  DDDD    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCHDS - This program displays count history information*~
            *            about the p]art/store/lots in HNYCCMST file.   *~
            *            The results of the most recent 6 count sessions*~
            *            are displayed on the lower half of the screen. *~
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
            * 06/08/92 ! Original                                 ! RJ1 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dispfac$1,                   /* Field Attribute Character  */~
            dispfac2$1,                  /* Field Attribute Character  */~
            dispfac3$1,                  /* Field Attribute Character  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$(20)79,                 /* Screen Line Literals       */~
            line2$79,                    /* Screen Line 2              */~
            midline$30,                  /* Screen Line Literals       */~
            msg$79,                      /* Plow screen message        */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
                key$99,                  /* Miscellaneous Read/Plow Key*/~
            sesboh$(200)10,              /* Display Book on Hand Quan  */~
            ses_onfile$6,                /* Count Session on File      */~
            sesdate$(200)8,              /* Display Session Date       */~
            sesrecord$(200)61,           /* Session Record Array       */~
            sesvarq$(200)10,             /* Display Variance Quantity  */~
            sesvarp$(200)7,              /* Display Variance Percent   */~
            sesvarr$(200)6,              /* Display Variance Reason    */~
            sestolhit$(200)10,           /* Display Tolerance Hit Cond.*/~
            sesname$(200)12,             /* Session Name               */~
            userid$3                     /* Current User Id            */


            dim     /* ** Cycle Count Variables ** */                    ~
            actflag$1,                   /* Active Sess'n Flag(P,A,C,b)*/~
            ccgroup$6,                   /* Cycle Count Group Code     */~
            cntnbr$12,                   /* Number of Count Sessions   */~
            cnttlernper$10,              /* Count Tolerance in Percent */~
            cnttlernqty$10,              /* Count Tolerance Quantity   */~
            cumhitprcnt$10,              /* Cumulative Tol Hit Percent */~
            cumtlernhit$10,              /* Cumulative Tolerance Hits  */~
            datecnt$10,                  /* Last Count Date            */~
            lot$6,                       /* Lot Number of Part         */~
            lot_msg$30,                  /* Lot Number of Message      */~
            nextcntdate$10,              /* Next Count Date            */~
            part$25,                     /* Part Number                */~
            partdesc$32,                 /* Part Description (accept)  */~
            partdescr$32,                /* Part Description (plow)    */~
            recdate$10,                  /* Date Record was created    */~
            sesboh$10,                   /* Session Book on Hand Quan  */~
            sescntdate$8,                /* Session Count Date         */~
            sesname$12,                  /* Session Name               */~
            sesstatus$30,                /* Session Status             */~
            sesvarreason$6,              /* Variance Reason            */~
            sesvarquan$10,               /* Session Variance Quantity  */~
            sesvarper$7,                 /* Session Variance Percentage*/~
            store$3,                     /* Store/warehouse            */~
            strdescr$30,                 /* Store Description (plow)   */~
            storedesc$30,                /* Store Description (accept) */~
            tolhit$3                     /* Tolerance Hit Cond.(YES/NO)*/

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
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #07 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #08 ! SYSFILE2 ! Caelus Management System Information     *~
            * #49 ! DUMMY    ! Dummy SELECT File - no actual file assoc *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup     ~

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  3, keypos =   73, keylen =  15, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  1, keypos =   45, keylen =   6, dup     ~

            select #04, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  2, keypos =   14, keylen =  44, dup,    ~
                            key  1, keypos =   13, keylen =  45, dup     ~

            select #05, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #08, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #49, "DUMMY",                                         ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen = 44                       ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "Select PF(1) to Enter Another Part."

            str(line2$,62) = "HNYCCHDS: " & str(cms2v$,,8)

            gosub set_accept_lines
            goto inputmode

        set_accept_lines
            line$(07) = "Cycle Count Group Name"

            line$(08) = "Number of Counts"

            line$(09) = "Cumulative Tolerance Hits"

            line$(10) = "Count Tolerance Quantity"
            midline$  = "Percent"
            line$(11) = "Current Session Activity"

            line$(12) = "Count Session on File"

            line$(13) = "   Session     Session      BOH       Quantity" ~
                     &  "    Percent   Variance    Met    "
            line$(14) = "     Name        Date     Quantity    Variance" ~
                     &  "    Variance   Reason   Tolerance"

            return

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
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
            *  S E L E C T I O N    M O D E   M A I N   P R O G R A M   *~
            *-----------------------------------------------------------*~
            * Handles operation of Selections for data entry screens.   *~
            *************************************************************

        selections
            dispfac$ = hex(8c) : dispfac2$ = hex(ac) : dispfac3$ = hex(84)
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub inputmode
                  if keyhit%  =  5% then gosub more_sessions
                  if keyhit%  =  4% then gosub less_sessions
                  if keyhit%  =  8% then gosub next_part
                  if keyhit%  = 16% then       exit_program

                  goto selections



        next_part   /* Select Next part and Return to input */
            call "READNEXT" (#03, f1%(03))
            gosub initialize_variables
            gosub L30070    /* **Data Load ** */

            return

        more_sessions
            /* ** Scroll Display by advancing array ** */
            if cntr% < 6% then return          /* Only one screen worth */
            ss% = ss% + 6%                    /* Scroll complete screen */
            if ss% > cntr% - 6% then ss% = cntr% - 6%     /* Not to Far */
            return

        less_sessions
            /* ** Reverse Scroll Display by reducing array ** */
            if cntr% < 6% then return          /* Only one screen worth */
            ss% = ss% - 6%               /* Scroll back complete screen */
            if ss% < 0%  then ss% = 0%                    /* Not to Far */
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Number            */~
                              L20150,         /* Store Number           */~
                              L20200          /* Lot Number             */
            return

L20100: REM Def/Enable Part Number                 PART$
            inpmessage$ = "Enter Part Number (enter partial or blank to"&~
                          " see parts on file)."
            return

L20150: REM Def/Enable Store Number                STORE$
            inpmessage$ = "Enter Store Code (enter partial or blank to" &~
                          " see stores on file)."
            return

L20200: REM Def/Enable Lot Number                  LOT$
            call "LOTENABL" (part$, le%, ll%, #08, #01)
            ll% = ll%
            if le% =  0% then lot_msg$ = "Part does not use lot numbers."
            if le% =  1% then lot_msg$ = "Lot number not required."
            if le% =  2% then lot_msg$ = "Part requires lot numbers."
            if le% <> 0% then return
                enabled% = 0%
                plowkey$ = str(part$) & str(store$)
                call "PLOWNEXT" (#2, plowkey$, 28%, enabled%)

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
         "Enter Part Number.                                           ",~
         "Enter Store Number.                                          ",~
         "Enter Lot Number.                                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      actflag$, ccgroup$, cntnbr$, cnttlernper$,         ~
                      cnttlernqty$, cumtlernhit$, lot$, part$,           ~
                      partdesc$, ses_onfile$, store$, storedesc$

            init(" ") sesdate$(), sesrecord$(), sesvarq$(), sesvarp$(),  ~
                      sesvarr$(), sestolhit$(), sesname$(), sesboh$()

            init(" ")                ccgroup$, datecnt$, cnttlernper$,   ~
                cnttlernqty$, cntnbr$, actflag$, nextcntdate$, recdate$, ~
                cumtlernhit$, sesstatus$

            dispfac$, dispfac2$, dispfac3$ = hex(9c)
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            call "READ100" (#03, plowkey$, f1%(03))
L30070:     if f1%(3) = 0% then return
                get #03 using     L35060,                                 ~
                part$, store$, lot$, ccgroup$, datecnt$, cnttlernper,    ~
                cnttlernqty, cntnbr , actflag$, nextcntdate$, recdate$,  ~
                cumtlernhit

            if cntnbr <> 0 then cumhitprcnt = 100 * (cumtlernhit/cntnbr) ~
                           else cumhitprcnt = 0.0

            call "CONVERT" (cnttlernper     ,  2.2, cnttlernper$)
            call "CONVERT" (cnttlernqty     , -2.2, cnttlernqty$)
            call "CONVERT" (cntnbr          ,-0.01, cntnbr$)
            call "CONVERT" (cumtlernhit     ,-0.01, cumtlernhit$)
            call "CONVERT" (cumhitprcnt     , 2.2 , cumhitprcnt$)

            gosub format_dates
            gosub set_session_status
            gosub set_detail_data

            return

        format_dates
            call "DATEFMT" (datecnt$)
            call "DATEFMT" (nextcntdate$)
            call "DATEFMT" (recdate$)
            return

        set_session_status
            if actflag$ <> "P" then L30490
                sesstatus$ = "Current Session is Pre-Active" :  return
L30490:     if actflag$ <> "C" then L30510
                sesstatus$ = "Current Session is Closed"     :  return
L30510:     if actflag$ <> "A" then L30530
                sesstatus$ = "Current Session is Active"     :  return
L30530:     sesstatus$ = "Session Status is Not Set"
            return

        REM *************************************************************~
            *    L O A D   D E T A I L    F I L E   D A T A             *~
            *-----------------------------------------------------------*~
            * Get Cycle Count Detail File Data for Part                 *~
            *************************************************************
        set_detail_data
            cntr% = 0%
            readkey$ = str(part$) & str(store$) & lot$
            call "REDALT0" (#04, readkey$, 2%, f1%(04))
        loop_detail
            if f1%(04) = 0% then L31320
            key$ = key(#4, 2)
            if key$    <> readkey$ then L31320
            cntr%  = cntr% + 1%
            get #04 using L35500 , sesname$,                              ~
                                  sescntdate$, sescount, sesvarquan,     ~
                                  sestolper, sestolquan, sesvarreason$

            boh = sescount + sesvarquan
            if boh = 0.0 then varprcnt = 0.0 /*Calc Tol. Hit Condition */~
                         else varprcnt = 100.0 * (sesvarquan / boh)
            if abs(varprcnt) <= sestolper or                             ~
               abs(sesvarquan)<= sestolquan                              ~
                               then tolhit$ = "YES"  else tolhit$ = "NO"
            call "CONVERT" (sesvarquan ,  2.2, sesvarquan$)
            call "CONVERT" (boh        ,  2.2, sesboh$    )
            call "CONVERT" (varprcnt   ,  2.2, sesvarper$ )
            /* Set Array Values */
            str(sesrecord$(cntr%),  , 8) = sescntdate$
            str(sesrecord$(cntr%),9 ,10) = sesvarquan$
            str(sesrecord$(cntr%),19,10) = sesvarper$
            str(sesrecord$(cntr%),29,6)  = sesvarreason$
            str(sesrecord$(cntr%),35,3)  = tolhit$
            str(sesrecord$(cntr%),38,12) = sesname$
            str(sesrecord$(cntr%),50,10) = sesboh$

            call "READNEXT" (#04, f1%(04))
            goto loop_detail

L31320:     gosub sort_detail_results
            gosub set_display

            return

        sort_detail_results /* Set detail array in order of count dates*/
            call "SORT" addr(sesrecord$(), cntr%, 61%, sesrecord$(),     ~
                                                       1%, 8%, "A", "S" )
*          CALL "SORT" ADDR(SESRECORD$(), CNTR%, 61%)
            return

        set_display
            cntr =  cntr%
            call "CONVERT" (cntr, -0.01, ses_onfile$)

            /* Set Array Values */
            for i% = 1% to cntr%
                sesdate$(i%)   = str(sesrecord$(i%),  , 8)
                call "DATEFMT" (sesdate$(i%))
                sesvarq$(i%)   = str(sesrecord$(i%),9 ,10)
                sesvarp$(i%)   = str(sesrecord$(i%),19,10)
                sesvarr$(i%)   = str(sesrecord$(i%),29,6)
                sestolhit$(i%) = str(sesrecord$(i%),35,3)
                sesname$(i%)   = str(sesrecord$(i%),38,12)
                sesboh$(i%)    = str(sesrecord$(i%),50,10)
            next i%
            ss% = 0%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: HNYCCMST                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(6),          /* Cycle Count Group Code Name             */~
            CH(6),          /* Date Last Counted                       */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            PD(14,4),       /* Number of Counts                        */~
            CH(1),          /* Active Session Flag                     */~
            CH(6),          /* Next Count Date                         */~
            CH(6),          /* Record Start Date                       */~
            PD(14,4)        /* Cumulative Tolerance Hits               */

L35500: FMT                 /* File: HNYCCDTL                          */~
            CH(12),         /* Session Name                            */~
            POS(59),        /*                                         */~
            CH(6),          /* Count Date                              */~
            POS(68),        /*                                         */~
            PD(14,4),       /* Units Counted                           */~
            POS(84),        /*                                         */~
            PD(14,4),       /* Count Variance                          */~
            POS(100),       /*                                         */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            POS(131),       /*                                         */~
            CH(6)           /* Variance Reason Code                    */

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
              on fieldnr% gosub L40120,         /* Part Number       */   ~
                                L40120,         /* Store Number      */   ~
                                L40115          /* Lot Number        */
              goto L40135

L40115:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40120:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40135:     accept                                                       ~
               at (01,02),                                               ~
                  "Cycle Count History Display",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part Number",                                ~
               at (04,30), fac(lfac$( 1)), part$                , ch(17),~
               at (04,49), fac(hex(8c)),   partdesc$            , ch(32),~
                                                                         ~
               at (05,02), "Store Number",                               ~
               at (05,30), fac(lfac$( 2)), store$               , ch(03),~
               at (05,49), fac(hex(8c)),   storedesc$           , ch(32),~
                                                                         ~
               at (06,02), "Lot Number",                                 ~
               at (06,30), fac(lfac$( 3)), lot$                 , ch(06),~
                                                                         ~
               at (07,02), fac(dispfac$) , line$(07)            ,        ~
               at (07,30), fac(dispfac3$), ccgroup$             , ch(06),~
                                                                         ~
               at (08,02), fac(dispfac$) , line$(08)            ,        ~
               at (08,30), fac(dispfac3$), cntnbr$              , ch(10),~
                                                                         ~
               at (09,02), fac(dispfac$) , line$(09)            ,        ~
               at (09,30), fac(dispfac3$), cumtlernhit$         , ch(10),~
               at (09,43), fac(dispfac$) , midline$             ,        ~
               at (09,52), fac(dispfac3$), cumhitprcnt$         , ch(10),~
                                                                         ~
               at (10,02), fac(dispfac$) , line$(10)            ,        ~
               at (10,30), fac(dispfac3$), cnttlernqty$         , ch(10),~
               at (10,43), fac(dispfac$) , midline$             ,        ~
               at (10,52), fac(dispfac3$), cnttlernper$         , ch(10),~
                                                                         ~
               at (11,02), fac(dispfac$) , line$(11)            ,        ~
               at (11,30), fac(dispfac3$), actflag$             , ch(01),~
               at (11,43), fac(dispfac3$), sesstatus$           , ch(30),~
                                                                         ~
               at (12,02), fac(dispfac$) , line$(12)            ,        ~
               at (12,30), fac(dispfac3$), ses_onfile$          , ch(06),~
                                                                         ~
               at (13,02), fac(dispfac$) , line$(13)            ,        ~
               at (14,02), fac(dispfac2$), line$(14)            ,        ~
                                                                         ~
               at (15,04), fac(dispfac3$), sesname$(1% + ss%)   , ch(12),~
               at (15,17), fac(dispfac3$), sesdate$(1% + ss%)   , ch(08),~
               at (15,26), fac(dispfac3$), sesboh$(1% + ss%)    , ch(10),~
               at (15,38), fac(dispfac3$), sesvarq$(1% + ss%)   , ch(10),~
               at (15,53), fac(dispfac3$), sesvarp$(1% + ss%)   , ch(10),~
               at (15,63), fac(dispfac3$), sesvarr$(1% + ss%)   , ch(06),~
               at (15,74), fac(dispfac3$), sestolhit$(1% + ss%) , ch(03),~
                                                                         ~
               at (16,04), fac(dispfac3$), sesname$(2% + ss%)   , ch(12),~
               at (16,17), fac(dispfac3$), sesdate$(2% + ss%)   , ch(08),~
               at (16,26), fac(dispfac3$), sesboh$(2% + ss%)    , ch(10),~
               at (16,38), fac(dispfac3$), sesvarq$(2% + ss%)   , ch(10),~
               at (16,53), fac(dispfac3$), sesvarp$(2% + ss%)   , ch(10),~
               at (16,63), fac(dispfac3$), sesvarr$(2% + ss%)   , ch(06),~
               at (16,74), fac(dispfac3$), sestolhit$(2% + ss%) , ch(03),~
                                                                         ~
               at (17,04), fac(dispfac3$), sesname$(3% + ss%)   , ch(12),~
               at (17,17), fac(dispfac3$), sesdate$(3% + ss%)   , ch(08),~
               at (17,26), fac(dispfac3$), sesboh$(3% + ss%)    , ch(10),~
               at (17,38), fac(dispfac3$), sesvarq$(3% + ss%)   , ch(10),~
               at (17,53), fac(dispfac3$), sesvarp$(3% + ss%)   , ch(10),~
               at (17,63), fac(dispfac3$), sesvarr$(3% + ss%)   , ch(06),~
               at (17,74), fac(dispfac3$), sestolhit$(3% + ss%) , ch(03),~
                                                                         ~
               at (18,04), fac(dispfac3$), sesname$(4% + ss%)   , ch(12),~
               at (18,17), fac(dispfac3$), sesdate$(4% + ss%)   , ch(08),~
               at (18,26), fac(dispfac3$), sesboh$(4% + ss%)    , ch(10),~
               at (18,38), fac(dispfac3$), sesvarq$(4% + ss%)   , ch(10),~
               at (18,53), fac(dispfac3$), sesvarp$(4% + ss%)   , ch(10),~
               at (18,63), fac(dispfac3$), sesvarr$(4% + ss%)   , ch(06),~
               at (18,74), fac(dispfac3$), sestolhit$(4% + ss%) , ch(03),~
                                                                         ~
               at (19,04), fac(dispfac3$), sesname$(5% + ss%)   , ch(12),~
               at (19,17), fac(dispfac3$), sesdate$(5% + ss%)   , ch(08),~
               at (19,26), fac(dispfac3$), sesboh$(5% + ss%)    , ch(10),~
               at (19,38), fac(dispfac3$), sesvarq$(5% + ss%)   , ch(10),~
               at (19,53), fac(dispfac3$), sesvarp$(5% + ss%)   , ch(10),~
               at (19,63), fac(dispfac3$), sesvarr$(5% + ss%)   , ch(06),~
               at (19,74), fac(dispfac3$), sestolhit$(5% + ss%) , ch(03),~
                                                                         ~
               at (20,04), fac(dispfac3$), sesname$(6% + ss%)   , ch(12),~
               at (20,17), fac(dispfac3$), sesdate$(6% + ss%)   , ch(08),~
               at (20,26), fac(dispfac3$), sesboh$(6% + ss%)    , ch(10),~
               at (20,38), fac(dispfac3$), sesvarq$(6% + ss%)   , ch(10),~
               at (20,53), fac(dispfac3$), sesvarp$(6% + ss%)   , ch(10),~
               at (20,63), fac(dispfac3$), sesvarr$(6% + ss%)   , ch(06),~
               at (20,74), fac(dispfac3$), sestolhit$(6% + ss%) , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40920
                  call "MANUAL" ("HNYCCHDS") : goto L40135

L40920:        if keyhit% <> 15 then L40950
                  call "PRNTSCRN" : goto L40135

L40950:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41140     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41100
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41100:     if fieldnr% > 2% then L41120
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41120:     return

L41140:     /*  Non-Edit Mode - Select Fld */
            pf$(1) = "(1)Enter Part    (4)Previous Sessions   " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (5)Next Sessions       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (8)Next Part           " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff0405ffff08ffffffff0dff0f1000)
            if cntr < ss% + 7 then                                       ~
                str(pf$(2),18,20) = " "  :  str(pfkeys$, 5,1) = hex(ff)
            if ss% > 0% then return
                str(pf$(1),18,20) = " "  :  str(pfkeys$, 4,1) = hex(ff)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Part Number            */~
                              L50300,         /* Store Number           */~
                              L50500          /* Lot Number             */
            return
L50100: REM Test for Part Number                  PART$
            abcorig$ = " "
            if part$ = "?" then part$ = " "
            partdescr$ = hex(06) & "Select a Part Number"
            call "PLOWCODE" (#01, part$, partdescr$, 0%, 0.32, f1%(1))
            if f1%(1) = 1% then L50190
                errormsg$ = "Part Number not on file."
                return
L50190:     plowkey$ = part$    :   partdesc$ =  partdescr$
            get #1 using  L50210, abcorig$  /*Get ABC Class frm HNYMASTR */
L50210:     FMT POS(111), CH(1)

            call "PLOWNEXT" (#02, plowkey$, 25%, f1%(2))
            if f1%(2) = 1% then return
                errormsg$ = "No quantity records exist for this part"
                return

L50300: REM Test for Store                        STORE$
            strdescr$ = hex(06) & "Select a Store Number"
            call "PLOWCODE" (#07, store$, strdescr$, 0%, 0.30, f1%(7))
            if f1%(7) = 1% then L50350
                errormsg$ = "Store Number not on file."  : return
L50350:     storedesc$ = strdescr$
            plowkey$ = str(part$) & str(store$) & hex(00)
            call "PLOWNEXT" (#02, plowkey$, 28%, f1%(2))
            if f1%(2) = 1% then L50430
                errormsg$ = "No quantity records exist for this" &       ~
                             " Part & Store."
                return

L50430:     if lot$ = " " then return    /* Lot Not Set Yet */
                fieldnr% = fieldnr% + 1%  :  goto  L50600

L50500: REM Test for Lot Number                   LOT$
            if lot$ = "?" then keyhit% = 8%
            if keyhit% <> 8% then L50600
                msg$ = hex(06) & "Select a Lot Number"
                plowkey$ = str(part$) & str(store$) & hex(00)
                call "PLOWCODE" (#02, plowkey$, msg$, 28%, 0, f1%(2))
                if f1%(2) = 1% then L50590
                     errormsg$ = "No Lots on File."
                     return
L50590:         lot$ = str(plowkey$,29,6)
L50600:     plowkey$ = str(part$) & str(store$) & lot$
            call "READ100" (#02, plowkey$, f1%(02))
            if f1%(2) = 0% then L50680
                gosub dataload      /* Set Default values if in HNYCCMST*/
                if f1%(03) = 1% then L50665
                     errormsg$ = "Not Part of Cycle Counting"
                     fieldnr% = 0%
                     return
L50665:         fieldnr% = 03%   /* Everything OK so Exit ForNext Loop*/
                return
L50680:     errormsg$ = "No quantity record found for this lot."
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
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
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
