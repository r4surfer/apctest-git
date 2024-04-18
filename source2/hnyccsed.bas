        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC    SSS   EEEEE  DDDD    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  S      E      D   D   *~
            *  HHHHH  N N N   YYY   C      C       SSS   EEEE   D   D   *~
            *  H   H  N  NN    Y    C   C  C   C      S  E      D   D   *~
            *  H   H  N   N    Y     CCC    CCC    SSS   EEEEE  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCSED - This program will allow the user to tailor the *~
            *            contents of the Cycle Counting Session.        *~
            *            * Manually Add or Subtract parts from session. *~
            *            * Automatically add parts to be counted based  *~
            *              on a negative/zero on-hand condition.        *~
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
            * 02/14/92 ! Original                                 ! SID *~
            * 05/03/93 ! Updated HNYCCMST select clause.          ! MLJ *~
            * 05/12/93 ! Input & edit mode mods to smooth flow.   ! MLJ *~
            * 07/09/93 ! Zap Workfile on close.                   ! RJH *~
            * 09/01/93 ! Minor mods to input/edit flow.           ! RJH *~
            * 10/29/93 ! PRR 13052 Option to exclude from a sess'n! RJH *~
            *          !   zero qty Parts (Lot/Non Tracked) with  !     *~
            *          !   with no activity for 'X' number of days!     *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            activity_date$6,             /* HNYDETAL Last activity date*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            checkflag$1,                 /* NextCountDate Selection Flg*/~
            cntr$7,                      /* # of rec wrote to CC detail*/~
            cntperiod$3,                 /* Count Period from HNYCCMST */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datelast$6,                  /* Date Last Counted          */~
            descr$42,                    /* Descr For PlowCode         */~
            descrp$60,                   /* HNYCCSIP Description       */~
            descr_m(6),                  /* Descr Map For PlowCode     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmccgroup$6,                 /* FROM CC Group Name         */~
            fmlot$6,                     /* From Lot  Range            */~
            fmpart$25,                   /* From Part Range            */~
            fmstore$3,                   /* From Store Range           */~
            from$25,                     /* Literal for Input Screen   */~
            header$55,                   /* Header Literal for HNYCCRNG*/~
            header$(3)79,                /* For PLOWCODE Call          */~
            hilot$6,                     /* LOT Range for TESTRNGE     */~
            i$(24)80,                    /* Screen Image               */~
            inc(2),                      /* Plowcode Arguments         */~
            inc$(2)16,                   /* Plowcode Arguments         */~
            info$50,                     /* ASKUSER Argument           */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lolot$6,                     /* Lot Range for TESTRNGE     */~
            lot$16,                      /* Lot Code                   */~
            ltp_ex_flg$1,                /* Lot Tracked Exclude Flag   */~
            ltp_ex_date$6,               /* Lot Tracked Inactive Date  */~
            ltp_ex_days$3,               /* Lot Tracked Inactive Days  */~
            nltp_ex_flg$1,               /* NotLot Tracked Exclude Flag*/~
            nltp_ex_date$6,              /* NotLot Tracked Inactiv Date*/~
            nltp_ex_days$3,              /* NotLot Tracked Inactiv Days*/~
            opt$(3)50,                   /* PF keys Options on Scrn 1  */~
            part$25,                     /* Part Numbers               */~
            partstrlot$44,               /* Part Store Lot Combo       */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16$16,                     /* PF Literals HNYCCRNG       */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plancntdate$8,               /* Planned Count Date         */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            qlot$16,                     /* Lot   Number From HNYQUAN  */~
            qpart$25,                    /* Part  Number From HNYQUAN  */~
            qstore$3,                    /* Store Number From HNYQUAN  */~
            r$(24)80,                    /* Screen Image From HNYCCRNG */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            session$12,                  /* Session Name               */~
            sessiondescr$30,             /* Session Description        */~
            store$3,                     /* Store                      */~
            tempdate$8,                  /* Temp Variable              */~
            testdate$8,                  /* Zero Qty Exclusion Date    */~
            to$25,                       /* Literal for Input Screen   */~
            toccgroup$6,                 /* TO CC Group Name           */~
            tolot$6,                     /* To Lot Range               */~
            topart$25,                   /* To Part Range              */~
            tostore$3,                   /* To Store Range             */~
            unfmtdate$6,                 /* Unformated system date     */~
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #06 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #07 ! STORNAME ! Store Information File                   *~
            * #08 ! SYSFILE2 ! Caelus Management System Information     *~
            * #09 ! CATEGORY ! Iventory Category Codes File             *~
            * #10 ! HNYDETAL ! Inventory Details                        *~
            * #50 ! WORKFILE ! Temporary System Workfile                *~
            * #55 ! DUMMY    ! For PLOWCODE Extended Arguments          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup

            select #04, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1,  keylen = 57,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup

            select #05, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #06, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =   1,  keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =  13, dup,    ~
                            key  2, keypos =   56, keylen =  10

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #08, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #09, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos  =   1, keylen = 4

            select #10, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   44,              ~
                        keypos =    1, keylen =  44                      ~

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%,    rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%,    rslt$(02))
                get rslt$(02) using L02666, rec1%
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
                get rslt$(03) using L02666, rec2%
L02666:             FMT POS(17), BI(4)
        /* Create HNYCCDTL file at 1/10 of the HNYQUAN file.  */
            call "OPENCHCK" (#04, fs%(04), f2%(04), rec1%/10, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%,       rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 100%,     rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%,       rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%,       rslt$(08))
            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%,       rslt$(09))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            unfmtdate$, date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYCCSED: " & str(cms2v$,,8)

            opt$(1) = "(7) Manually Subtract Parts from the Session"
            opt$(2) = "(8) Manually Add Parts to the Session"
            opt$(3) = "(9) Auto Add Parts Based on Negative/Zero On-Hand"

            from$ = "From" : to$ = "To"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

                gosub'051(1%)        /* Default / Enables */
L10130:         gosub'101(1%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% = 16% then exit_program
                      if keyhit% <> 0% then       L10130
                gosub'151(1%)
                      if errormsg$ <> " " then L10130
L10142:         gosub'101(1%, 2%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% =  7% then L10261
                      if keyhit% =  8% then L10431
                      if keyhit% =  9% then L10601
                      if keyhit% = 16% then exit_program
                goto L10142

L10261: REM  PF(7) Subtract a Part...
            init(" ") fmpart$, fmstore$, fmlot$, topart$, tostore$, tolot$
            for fieldnr% = 1% to 3%
L10280:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10400
L10300:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10380
L10330:                  fieldnr% = max(1%, fieldnr% - 1%)
                         errormsg$ = " "
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10300
                         if fieldnr% = 1% then L10280
                         goto L10330
L10380:               if keyhit% = 16% and fieldnr% = 1% then            ~
                                                goto back_to_first_screen
                      if keyhit% <> 0% then       L10300
L10400:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10300
            next fieldnr%
            goto editpg2

L10431: REM PF(8) Add Part - Source Flag 'B'...
            init(" ") fmpart$, fmstore$, fmlot$, topart$, tostore$, tolot$
            for fieldnr% = 1% to 7%
L10450:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% <> 0% then L10470
                          if fieldnr% >4% then L10590  else L10570
L10470:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10550
L10500:                  fieldnr% = max(1%, fieldnr% - 1%)
                         errormsg$ = " "
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10470
                         if fieldnr% = 1% then L10450
                         goto L10500
L10550:               if keyhit% = 16% and fieldnr% = 1% then            ~
                                                goto back_to_first_screen
                      if keyhit% <> 0% then       L10470
L10570:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10470
L10590:     next fieldnr%
            goto editpg3

L10601: REM Auto Add based on Negative/Zero On-Hand - Source Flag 'C'...
            init(" ") fmccgroup$, toccgroup$
            for fieldnr% = 1% to  1%
L10620:         gosub'054(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10740
L10640:         gosub'104(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10715
L10670:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'054(fieldnr%)
                         if enabled% = 1% then L10640
                         if fieldnr% = 1% then L10620
                         goto L10670
L10715:               if keyhit% =  8% then select_ranges
                      if keyhit% = 16% and fieldnr% = 1% then            ~
                                               goto back_to_first_screen
                      if keyhit% <> 0% then       L10640
L10740:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10640
            next fieldnr%
            goto editpg4

        back_to_first_screen
            errormsg$ = " "
            goto L10142

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * EDITPG2 -  PF7) Manually Subtract Parts from the Session  *~
            * EDITPG3 -  PF8) Manually Add      Parts from the Session  *~
            * EDITPG4 -  PF9) Auto Add Parts Based on Neg./Zero On-Hand *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       manual_sub_parts
                  if keyhit% <>  0% then       editpg2
L11340:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 3% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11390:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11390
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11390
                  lastfieldnr% = fieldnr%
            goto L11340

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       manual_add_parts
                  if keyhit% <>  0% then       editpg3
L11550:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% =  4% then editpg3      /* Blank line on screen */
            if fieldnr% >  4% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 1% or fieldnr% > 7% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L11600:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11600
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11600
                  lastfieldnr% = fieldnr%
            goto L11550

        editpg4
            lastfieldnr% = 0%
            gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       select_ranges
                  if keyhit%  = 16% then       auto_add_parts
                  if keyhit% <>  0% then       editpg4
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  1% then editpg4
            if fieldnr% = lastfieldnr% then    editpg4
            gosub'054(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg4
L11800:     gosub'104(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       select_ranges
                  if keyhit%  = 16% then       auto_add_parts
                  if keyhit% <>  0% then L11800
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11800
                  lastfieldnr% = fieldnr%
            goto L11800

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        REM *************************************************************~
            *               D E F A U L T / E N A B L E                 *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES for Screen 1 of Input.          *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%

        REM Def/Enable Session /Planned Count Date   SESSION$/PLANCNTDATE$
            inpmessage$ = "Enter Session Name & Date Or Leave Blank To "&~
                          "Search."
            return

        REM *************************************************************~
            *               D E F A U L T / E N A B L E                 *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES for Screen 2 and 3 of Input.    *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21100,         /* Part Numbers           */~
                              L21200,         /* Store                  */~
                              L21300,         /* Lot                    */~
                              L21350,         /* Lot Tracked Excude Flg */~
                              L21430,         /* Non Lot Track Exclude F*/~
                              L21390,         /* Lot Tracked Test Days  */~
                              L21510          /* Non Lot Track Test Days*/
            return

L21100: REM Def/Enable Part Numbers               FMPART$/TOPART$
            if fmpart$ = " " then fmpart$ = "ALL"
            inpmessage$ = "Enter Part Number Range Or Leave Blank To Se"&~
                          "arch."
            return

L21200: REM Def/Enable Store                      FMSTORE$/TOSTORE$
            if fmstore$ = " " then fmstore$ = "ALL"
            inpmessage$ = "Enter Store Number Range Or Leave Blank To S"&~
                          "earch."
            return

L21300: REM Def/Enable Lot                        FMLOT$/TOLOT$
            if fmlot$ = " " then fmlot$ = "ALL"
            inpmessage$ = "Enter Lot Number Range."
            return

L21350: REM Def/Enable Exclude zero Qty Lot Tracked Parts LTP_EX_FLG$
            inpmessage$ = "Exclude Lot Tracked Parts with zero " &       ~
                             "quantity and no activity ('Y' or 'N')"
            if ltp_ex_flg$ = " " then  ltp_ex_flg$  =  "Y"
            return

L21390: REM Def/Enable Exclude zero Qty Nonlot Tracked Parts NLTP_EX_FLG$
            inpmessage$ = "Exclude Non-Lot Tracked Parts with zero " &   ~
                             "qty and no activity ('Y' or 'N')"
            if nltp_ex_flg$ = " " then nltp_ex_flg$  =  "N"
            return

L21430: REM Def/Enable No. days w/o Activity for Lot Tracked LTP_EX_DAYS$
            inpmessage$ =  "Enter Number of Days of No Activity for " &  ~
                              "Lot Tracked exclusion"
            if ltp_ex_flg$  =  "Y" then L21480
                ltp_ex_days$  =  " "
                enabled% = 0%
                return
L21480:     if ltp_ex_days$ = " " then  ltp_ex_days$  =  "60"
            return

L21510: REM Def/Enable No. days w/o Activity fr Nonlot Track LTP_EX_DAYS$
            inpmessage$ = "Enter No. of Days of No Activity for " &      ~
                             "Non-Lot Tracked exclusion"
            if nltp_ex_flg$  =  "Y" then L21560
                nltp_ex_days$  =  " "
                enabled% = 0%
                return
L21560:     if nltp_ex_days$ = " " then  nltp_ex_days$  =  "60"
            return

        REM *************************************************************~
            *               D E F A U L T / E N A B L E                 *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES for Screen 4 of Input.          *~
            *************************************************************

        deffn'054(fieldnr%)
            enabled% = 1%

        REM Def/Enable Cycle Count Group           FMCCGROUP$/TOCCGROUP$
            if fmccgroup$ = " " then fmccgroup$ = "ALL"
            inpmessage$ = "Enter Group Code Range Or Leave Blank To Sea"&~
                          "rch."
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            call "ALLFREE"

            init(" ") errormsg$, inpmessage$, fmstore$, tostore$,        ~
                   fmccgroup$, toccgroup$, part$, session$, store$,      ~
                   fmpart$, topart$, qpart$, qstore$, qlot$, datelast$,  ~
                   sessiondescr$, fmlot$, tolot$, hilot$, lolot$,        ~
                   plancntdate$, pf16$, r$(),                            ~
                   ltp_ex_flg$, ltp_ex_days$, ltp_ex_date$, testdate$,   ~
                   nltp_ex_flg$, nltp_ex_days$, nltp_ex_date$,           ~
                   activity_date$, lot_track_flg$

            cnttlerper, cnttlerqty = 0
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

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        /* Manually Subtracting Parts from Session */
        manual_sub_parts
            call "SHOSTAT" ("Subtracting parts from a Session")
            cntr% = 0% : init(" ") plowkey$ : option$ = "7"

            str(plowkey$,,12) = str(session$) & hex(00)
L32050:     call "PLOWNEXT" (#04, plowkey$, 12%, f1%(04)) /* HNYCCDTL */
                if f1%(04) = 0% then display_result

            if str(plowkey$,,12) > session$ then display_result
            if (fmpart$ = "FIRST" and topart$ = "LAST") or               ~
                    str(fmpart$,,3) = "ALL" then L32102 /* Part Range */
               if str(plowkey$,14,25) < fmpart$  or                      ~
                  str(plowkey$,14,25) > topart$  then L32050
L32102:     if fmstore$ = "ALL" then L32120        /* Store Range */
               if fmstore$ > str(plowkey$,39, 3) or                      ~
                  tostore$ < str(plowkey$,39, 3) then L32050
L32120:     if fmlot$ = "ALL" then L32124          /* Lot Range   */
               if fmlot$ > str(plowkey$,42, 6) or                        ~
                  tolot$ < str(plowkey$,42, 6) then L32050

L32124:     call "READ101" (#03, str(key (#04),14%, 44%), f1%(03%))
                if f1%(03) = 0% then L32130 /* This Shouldn't Happen */
                    put #03 using L32127, "C"
L32127:                     FMT POS(81), CH(1)
                    rewrite #03, eod goto L32130

L32130:     call "DELETE" (#04, str(plowkey$,,57), 57%)

            cntr% = cntr% + 1%

            goto L32050  /* Get next HNYCCDTL record */

        /* Manually Adding Parts to a Session */
        manual_add_parts
            call "SHOSTAT" ("Adding parts to a Session")
            cntr% = 0% : init(" ") plowkey$ : option$ = "8"

            if (fmpart$ = "FIRST" and topart$ = "LAST") or               ~
                                 str(fmpart$,,3) = "ALL" then L33020
            str(plowkey$,,25) = str(fmpart$) addc all(hex(ff))
L33020:     call "PLOWNXT1" (#03, plowkey$, 0%, f1%(03)) /* HNYCCMST */
                if f1%(03) = 0% then display_result

            if pos("AP" = str(key(#03,2),1,1)) <> 0% then L33020
            if (fmpart$ = "FIRST" and topart$ = "LAST") or               ~
                              str(fmpart$,,3) = "ALL" then L33046
               if str(plowkey$,1,25) > topart$ then display_result
L33046:     if fmstore$ = "ALL" then L33054
               if fmstore$ > str(plowkey$,26,3) or                       ~
                  tostore$ < str(plowkey$,26,3) then L33020
L33054:     if fmlot$ = "ALL" then L33080
               if fmlot$ > str(plowkey$,29,6) or                         ~
                  tolot$ < str(plowkey$,29,6) then L33020

L33080:     get #03 using L33084, datelast$, cnttlerper, cnttlerqty,      ~
                                 cntperiod$
L33084:              FMT    POS(51), CH(6), 2*PD(14,4), POS(181), CH(3)

            /* Don't include if Count Period is Less Than or Equal to 0 */
            convert cntperiod$ to cntperiod%, data goto L33020
            if cntperiod% <= 0% then L33020

            /* Test for exclusion of zero quantity items */
            gosub check_hnydetal
            if checkflag$ = "N" then L33020

            put #03 using L33096, "P"     /* Update Active Flag */
L33096:                   FMT POS(81), CH(1)
            rewrite #03, eod goto L33100
        /* #04 HNYCCDTL */
L33100:     put #04 using L33130, session$, "P",                          ~
                str(plowkey$,,44), "B", " ", userid$, 0, 0, 0, 0,        ~
                cnttlerper, cnttlerqty, datelast$, cntperiod$, " "

L33130:         FMT CH(12), CH(1), CH(44), CH(1), CH(6), CH(3),          ~
                    6*PD(14,4), CH(6), CH(3), CH(312)

            write #04, eod goto L33020

            cntr% = cntr% + 1%

            goto L33020  /* Go get next HNYCCMST record */

        check_hnydetal        /* check for exclusion due to no activity */
            checkflag$ = "Y"                        /* start out OK */
            partstrlot$ = plowkey$
            part$ = str(plowkey$,1,25)
            call "READ100" (#2, partstrlot$, f1%(2%))
            if f1%(2%) = 0 then L33300     /* Assume qty zero */
            get #2 using L33240, qty
L33240:     FMT POS( 69), PD(14,4)
            if qty <> 0 then return      /* Don't need to test non zero */

L33300:     call "READ100" (#1, part$, f1%(1%))
            get #1 using L33310, lot_track_flg$
L33310:     FMT POS(130), CH(1)
            if lot_track_flg$ = "Y" then  L33335  /* must be lot tracked */
                if nltp_ex_flg$ = "N" then return
                    testdate$ = nltp_ex_date$
                    goto L33345
L33335:     if ltp_ex_flg$ = "N" then return           /* Dont exclude */
                testdate$ = ltp_ex_date$
L33345:     /* Test activity date */
            plowkey2$ = str(partstrlot$) & hex(00)
            call "PLOWNEXT" (#10, plowkey2$, 34%, f1%(10%))
            if f1%(10%) = 0% then L33380
            get #10 using L33370, activity_date$
L33370:     FMT POS(43), CH(6)
            if testdate$ <= activity_date$ then return
L33380:         checkflag$ = "N"        /*Exclude*/
                return

        /* Auto Add Parts Based on Negative/Zero On-Hand */
        auto_add_parts
            /* Create WorkFile for HNYCCMST */
            call "WORKOPEN" (#50, "IO", rec2%/2%, f2%(51))
            call "SHOSTAT" ("Auto Adding parts to a Session")
            cntr% = 0% : option$ = "9" : init(" ") part$, store$, plowkey$
            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                      fmccgroup$ = "ALL" then L34050
            str(plowkey$,1,6) = str(fmccgroup$) addc all(hex(ff))

L34050:     call "PLOWALTS" (#03, plowkey$, 1%, 0%, f1%(03))/* HNYCCMST */
L34060:         if f1%(03) = 0% then update_hnyccmst

            if pos("AP" = str(key(#03,2),1,1)) <> 0% then L34335
            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                    fmccgroup$ = "ALL" then L34120 /* Test Group Range */
               if key(#03,1) > toccgroup$ then update_hnyccmst

L34120:     get #03 using L34135, part$, store$, lot$, datelast$,         ~
                                 cnttlerper, cnttlerqty, cntperiod$
L34135:                   FMT CH(25), CH(3), CH(16), POS(51), CH(6),     ~
                              2*PD(14,4), POS(181), CH(3)

            /* Don't include if Count Period is Less than or Equal to 0 */
            convert cntperiod$ to cntperiod%, data goto L34335
            if cntperiod% <= 0% then L34335

            /* Plowing thru HNYQUAN looking for PART/STORE Combo   */
            str(readkey$,1,44) = str(part$) & str(store$) & lot$
            call "READ100" (#02, readkey$, f1%(02)) /* HNYQUAN */
              if f1%(02) = 0% then L34335   /* Read Next HNYCCMST Rec. */

            get #02 using L34230, on_hand
L34230:         FMT POS(69), PD(14,4)

            if on_hand > 0 then L34335

            /* Update Session Active Flag in HNYCCMST */
            put #50 using L34252, part$, store$, lot$
L34252:             FMT CH(25), CH(3), CH(16)
            write #50, eod goto L34335

            /* Create Cycle Count Detail File  HNYCCDTL */
            put #04 using L34295, session$, "P", part$, store$, lot$,     ~
                "C", " ", userid$, 0, 0, 0, 0, cnttlerper, cnttlerqty,   ~
                datelast$, cntperiod$, " "
L34295:         FMT CH(12), CH(1), CH(25), CH(3), CH(16), CH(1),         ~
                    CH(6), CH(3), 6*PD(14,4), CH(6), CH(3), CH(312)

            write #04, eod goto L34335

            cntr% = cntr% + 1%

L34335:     call "READNEXT" (#03, f1%(03)) /* Next HNYCCMST */
            goto L34060   /* Go Check End of File */

        /* Using HNYCCRNG to Auto Add Parts Based on Neg./Zero On-Hand */
        select_ranges
            call "WORKOPEN" (#50, "IO", rec2%/2%, f2%(50))
            cntr%, count% = 0% : option$ = "9" : pf16$ = "(16)Process    "
            header$ = "Auto Add Parts Based on Negative/Zero On-Hand"
            call "HNYCCRNG" ("Y", 2%, #01, #02, #09, #07, #03, #50,      ~
                             count%, header$, pf16$, r$())

            /* Starting PLOWing thru WORKFILE        */
            plowkey$ = hex(00)
L34420:     call "PLOWNEXT" (#50, plowkey$, 0%, f1%(50))
                if f1%(50) = 1% then L34450
                   close #50  /* WorkFile */
                   call "FILEBGON" (#50)      /* Zap Workfile */
                   if cntr% = 0% and count% = 0% then L11800
                   goto display_result

L34450:     /* Check to see if Qty On-Hand in HNYQUAN is <= 0  */
            readkey$ = str(plowkey$,1,44)
            call "READ100" (#02, str(readkey$,1,44), f1%(02))
                if f1%(02) = 0% then L34420 /* F1%(02) better be 1% */

            get #02 using L34495, qpart$, qstore$, qlot$, on_hand
L34495:             FMT POS(17), CH(25), CH(3), CH(16), POS(69), PD(14,4)
            if on_hand > 0 then L34420   /* Not the one we want */

            /* Get the rest of the data from HNYCCMST file */
            call "READ101" (#03, str(readkey$,1,44), f1%(03))
                if f1%(03) = 0% then L34420 /* F1%(03) better be 1% */

              if pos("AP" = str(key(#03,2),1,1)) <> 0% then L34420
              get #03 using L34610, datelast$, cnttlerper, cnttlerqty,    ~
                                   cntperiod$
L34610:                  FMT POS(51), CH(6), 2*PD(14,4), POS(181), CH(3)

            /* Don't include if Count Period is <= Zero */
            convert cntperiod$ to cntperiod%, data goto L34420
            if cntperiod% <= 0% then L34420

            put #03 using L34622, "P"     /* Update Active Flag */
L34622:                   FMT POS(81), CH(1)
            rewrite #03, eod goto L34630

L34630:     put #04 using L34295, session$, "P", qpart$, qstore$,         ~
                qlot$, "C", " ", userid$, 0, 0, 0, 0, cnttlerper,        ~
                cnttlerqty, datelast$, cntperiod$, " "

            write #04, eod goto L34420

            cntr% = cntr% + 1%

            goto L34420   /* Get next WORKFILE record */

        update_hnyccmst          /* Update Session Active Flag */
            init(" ") plowkey$
L34810:     call "PLOWNEXT" (#50, plowkey$, 0%, f1%(50))
                if f1%(50) = 1% then L34840
                   close #50 /* Close WorkFile */
                   call "FILEBGON" (#50)      /* Zap Workfile */
                   goto display_result

L34840:     call "READ101" (#03, plowkey$, f1%(03))
               if f1%(03) = 0% then L34810  /* This Shouldn't Happen */
            put #03 using L34860, "P"
L34860:         FMT POS(81), CH(1)
            rewrite #03, eod goto L34810
            goto L34810


        /* Display the # of parts Added, Sub or Auto Added to HNYCCDTL */
        display_result
            convert cntr% to cntr$, pic(#######)
            on pos("789" = option$) goto L35035, L35045, L35055
L35035:     info$ = "Subtracted from Cycle Counting Detail File"
               goto L35080
L35045:     info$ = "Added to Cycle Counting Detail File"
               goto L35080
L35055:     info$ = "Automatically Added to Cycle Counting Detail File"
               goto L35080

L35080:     keyhit% = 2% /* Window in the Bottom */
            call "ASKUSER" (keyhit%, "*** RESULTS ****",                 ~
               "There are a total of " & hex(80) & cntr$ & " record(s)", ~
                info$, "Press Any key to RETURN")

            if tempdate$ = plancntdate$ then inputmode
               call "READ101" (#06, session$, f1%(06))
                 if f1%(06) = 0% then inputmode
                 call "DATUNFMT" (plancntdate$)
               put #06 using L35170, plancntdate$
L35170:            FMT POS(66), CH(6)
               rewrite #06
               call "DATEFMT" (plancntdate$)
             goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
              str(line2$,1,31) = "Current Session: " & session$
              if edit% <> 2% then L40045
                  inpmessage$ = "Select PFkey Option 7, 8 Or 9 To Conti"&~
                                "nue."
L40045:       if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if edit% <> 1% then L40060
                  lfac$(1%) = hex(81)   :   lfac$(2%) = hex(80)
                  lfac$(3%) = hex(9c)
                  goto L40090
L40060:       if edit% <> 2% then L40090
                  init(hex(8c)) lfac$(1%), lfac$(2%)
                  init(hex(84)) lfac$(3%)

L40090:     accept                                                       ~
               at (01,02), "Edit Cycle Count Session"           ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Session Name",                               ~
               at (06,22), fac(lfac$(1%)), session$             , ch(12),~
               at (06,38), fac(hex(8c)),   sessiondescr$        , ch(30),~
                                                                         ~
               at (07,02), "Planned Count Date",                         ~
               at (07,22), fac(lfac$(1%)), plancntdate$         , ch(08),~
                                                                         ~
               at (09,17), fac(lfac$(3%)), opt$(1)              , ch(50),~
               at (10,17), fac(lfac$(3%)), opt$(2)              , ch(50),~
               at (11,17), fac(lfac$(3%)), opt$(3)              , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40200
                  call "MANUAL" ("HNYCCSED") : goto L40090

L40200:        if keyhit% <> 15 then L40215
                  call "PRNTSCRN" : goto L40090

L40215:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1

            pf$(1) = "(1)Start Over    (7)Manually Subtract Pa" &        ~
                     "rts                    (13)Instructions"
            pf$(2) = "                 (8)Manually Add Parts  " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Auto Add Parts      " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffff070809ffffff0dff0f1000)
            if edit% = 2% then return
                str(pf$(1%),18%,43%) = " "
                str(pf$(2%),18%,43%) = " "
                str(pf$(3%),18%,43%) = " "
                str(pfkeys$,7%,3%)   = hex(ffffff)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)

              gosub set_pf2
              str(line2$,1,31) = "Current Session: " & session$
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41080,         /* Part Range        */   ~
                                L41080,         /* Store Range       */   ~
                                L41080          /* Lot Range         */
              goto L41095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41095:     accept                                                       ~
               at (01,02), "Edit Cycle Count Session - Manually Subtracti~
        ~ng Parts"                                               ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,17), fac(hex(ac)), from$                  , ch(25),~
               at (06,44), fac(hex(ac)), to$                    , ch(25),~
               at (07,02), "Part Range",                                 ~
               at (07,17), fac(lfac$( 1)), fmpart$              , ch(25),~
               at (07,44), fac(lfac$( 1)), topart$              , ch(25),~
                                                                         ~
               at (08,02), "Store Range ",                               ~
               at (08,17), fac(lfac$( 2)), fmstore$             , ch(03),~
               at (08,44), fac(lfac$( 2)), tostore$             , ch(03),~
                                                                         ~
               at (09,02), "Lot Range    ",                              ~
               at (09,17), fac(lfac$( 3)), fmlot$               , ch(06),~
               at (09,44), fac(lfac$( 3)), tolot$               , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41220
                  call "MANUAL" ("HNYCCSED") : goto L41095

L41220:        if keyhit% <> 15 then L41235
                  call "PRNTSCRN" : goto L41095

L41235:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L41330     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41310
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41310:     if fieldnr% > 1% then L41320
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41320:     return

L41330: if fieldnr% > 0% then L41375  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41375:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)

              gosub set_pf3
              str(line2$,1,31) = "Current Session: " & session$
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42080,         /* Part Range        */   ~
                                L42080,         /* Store Range       */   ~
                                L42080,         /* Lot Range         */   ~
                                L42080,       /* Lot Tracked Excude Flg */~
                                L42085,       /* Lot Tracked Test Days  */~
                                L42080,       /* Non Lot Track Test FLG */~
                                L42085        /* Non Lot Track Test Days*/
              goto L42095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42085:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42095:     accept                                                       ~
               at (01,02), "Edit Cycle Count Session - Manually Adding Pa~
        ~rts"                                                    ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,17), fac(hex(ac)), from$                  , ch(25),~
               at (06,44), fac(hex(ac)), to$                    , ch(25),~
               at (07,02), "Part Range"                         ,        ~
               at (07,17), fac(lfac$( 1)), fmpart$              , ch(25),~
               at (07,44), fac(lfac$( 1)), topart$              , ch(25),~
                                                                         ~
               at (08,02), "Store Range"                        ,        ~
               at (08,17), fac(lfac$( 2)), fmstore$             , ch(03),~
               at (08,44), fac(lfac$( 2)), tostore$             , ch(03),~
                                                                         ~
               at (09,02), "Lot Range"                          ,        ~
               at (09,17), fac(lfac$( 3)), fmlot$               , ch(06),~
               at (09,44), fac(lfac$( 3)), tolot$               , ch(06),~
                                                                         ~
               at (11,02), "Exclude Zero Qty for Lot Tracked Parts",     ~
               at (11,42), fac(lfac$( 4%)),  ltp_ex_flg$        , ch(01),~
                                                                         ~
               at (12,19), "If no activity within",                      ~
               at (12,42), fac(lfac$( 5%)),  ltp_ex_days$       , ch(03),~
               at (12,46), "days",                                       ~
                                                                         ~
               at (13,02), "Exclude Zero Qty for NonLot Track Parts",    ~
               at (13,42), fac(lfac$( 6%)),  nltp_ex_flg$       , ch(01),~
                                                                         ~
               at (14,19), "If no activity within",                      ~
               at (14,42), fac(lfac$( 7%)),  nltp_ex_days$      , ch(03),~
               at (14,46), "days",                                       ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42350
                  call "MANUAL" ("HNYCCSED") : goto L42095

L42350:        if keyhit% <> 15 then L42365
                  call "PRNTSCRN" : goto L42095

L42365:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L42460     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42440
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42440:     if fieldnr% > 1% then L42450
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42450:     return

L42460: if fieldnr% > 0% then L42505  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L42505:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
              gosub set_pf4
              str(line2$,1,31) = "Current Session: " & session$
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L43075          /* Cycle Count Group */
              goto L43090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L43075:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L43090:     accept                                                       ~
               at (01,02), "Edit Cycle Count Session - Auto Add based on ~
        ~Neg./Zero On-Hand"                                      ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,24), fac(hex(ac)), str(from$,,6)          ,        ~
               at (05,32), fac(hex(ac)), str(to$,,6)            ,        ~
                                                                         ~
               at (06,02), "Cycle Count Group  :",                       ~
               at (06,24), fac(lfac$( 1)), fmccgroup$           , ch(06),~
               at (06,32), fac(lfac$( 1)), toccgroup$           , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L43200
                  call "MANUAL" ("HNYCCSED") : goto L43090

L43200:        if keyhit% <> 15 then L43215
                  call "PRNTSCRN" : goto L43090

L43215:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
        if edit% = 2% then L43310     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "(8)Select Ranges       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffff04ffffff08ffffffff0dff0f1000)
            if fieldnr% = 1% then L43290
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L43290:     if fieldnr% > 2% then L43300
                str(pf$(2),18,17) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L43300:     return

L43310: if fieldnr% > 0% then L43355  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(8)Select Ranges       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0f1000)
            return
L43355:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(8)Select Ranges       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

        REM Test for Session/Planned Count Date   SESSION$/PLANCNTDATE$
            if session$ =  " " or session$ = "?" then L50150
            readkey$ = str(session$) & hex(00)
            call "READ100" (#06, readkey$, f1%(6%))
                if f1%(6%) = 0% then L50120               /* New Session */
            if str(key(#6,1%),,1%) = "P" then L50114
                errormsg$ = "Session is Currently Active or Closed"
                return
L50114:     get #06 using L50116, sessiondescr$, plancntdate$
L50116:         FMT POS(13), CH(30), POS(66), CH(6)
            call "DATEFMT" (plancntdate$)
            return

L50120:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "** CONFIRMATION **",               ~
                  hex(8c) & "Session:" & hex(84) & session$ & hex(8c) &  ~
                  "Does Not Exist " & hex(84),                           ~
                  hex(8c) & "To Create, Press RETURN." & hex(84),        ~
                  "Hit any other PF key To abort")
            if keyhit% <> 0% then L10130

            call "HNYCCSIP" (session$,sessiondescr$,plancntdate$,#6,#4)
                if sessiondescr$ = " " and ~
                   (plancntdate$ = " " or  ~
                    plancntdate$ = blankdate$) then L10130
            return

L50150:     init(hex(00)) str(plowkey$,,99)
            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(3) = hex(80) & "Select a Session Name"
            header$(1) = "  Session Name     Description                "~
                       & "      Planned Count Date"
            descr_m(01) =     1.12  : descr_m(02) = 0001.0
            descr_m(03) =    13.30  : descr_m(04) = 0018.0
            descr_m(05) =    66.061 : descr_m(06) = 0051.0

            inc(1) = 43.01 : inc$(1) = "P"

            if session$ = "?" then session$ = " "
            plowkey$ = str(session$) & hex(00)
            call "PLOWCODE" (#06, plowkey$, descrp$,9000%, 0.42, f1%(06),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #55, descr_m())
            if f1%(6%) = 1% then L50302
                errormsg$ = "You MUST Enter Or Select A Session Name"
                return
L50302:     session$      = str(descrp$,1%,12%)
            sessiondescr$ = str(descrp$,18%,30%)
            plancntdate$  = str(descrp$,51%,8%)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Screen 2. PF7) Subtract a Part from a Session             *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51320,              /* Part Numbers      */~
                              L51570,              /* Store Range       */~
                              L51880               /* Lot Ranges        */
            return


L51320: /* Select Part Range from HNYCCDTL for selected Session */
            if (fmpart$ = "FIRST" and topart$ = "LAST") or               ~
                fmpart$ = "ALL" then return
            if fmpart$ <> "ALL" then L51330 else topart$ = "  " : return
L51330:     if fmpart$ = " " and topart$ = " " then to% = 1% else to% = 0%
            if topart$ <> " " then to% = 1%
            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(1) = "  Part                         Description"
            inc(1) =  1.12 : inc$(1) = session$
            header$(3) = hex(80) & "Select FROM Part Range "
            plowkey$ = fmpart$
            call "PLOWCODE" (#04, plowkey$, descr$,-8025%,-2.32, f1%(04),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #01)
            fmpart$ = plowkey$
            if (f1%(04) = 0% or f1%(04) = 1%) and to% = 1% then L51490
            if (f1%(04) = 0% or f1%(04) = 1%) and to% = 0% then L51552

L51490:     plowkey$ = topart$
            header$(3) = hex(80) & "Select TO Part Range "
            call "PLOWCODE" (#04, plowkey$, descr$,-8025%,-2.32, f1%(04),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #01)
           topart$ = plowkey$
           if fmpart$ <> " " and f1%(04) = 0% then L51528 else L51535
L51528:       if topart$ <> " " then L51535 else topart$ = fmpart$
L51535:    if topart$ < fmpart$ then L51561
           if fmpart$ = " " and topart$ = " " then fmpart$ = "ALL"
           if fmpart$ = " " and topart$ <> " " then fmpart$ = topart$
           if fmpart$ <> " " and topart$ = " " and fmpart$ <> "ALL"      ~
                                            then topart$ = fmpart$
           return
L51552:    if fmpart$ = "?" and topart$ = " " then L51560
           topart$ = fmpart$
           return

L51560:     errormsg$ = "Part Code Not On File."  : return
L51561:     errormsg$ = "FROM Part " & topart$ & " May Not Be Greater "  ~
                      & "Than TO Part " & fmpart$ : return

L51570: /* Select Store Code to Subtract From          FMSTORE$/TOSTORE$*/
            gosub select_store_range
            return

L51880: /* Select Lot Range                       FMLOT$/TOLOT$     */
            if fmlot$ = "ALL" and tolot$ = " " then return
            call "TESTRNGE" (fmlot$, tolot$, hilot$, lolot$, errormsg$)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Screen 3. PF8) Add a Part to a Session                    *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52140,              /* Part Range        */~
                              L52410,              /* Store Range       */~
                              L52450,              /* Lot Range         */~
                              L52500,         /* Lot Tracked Excude Flg */~
                              L52600,         /* Lot Tracked Test Days  */~
                              L52700,         /* Non Lot Track Exclude F*/~
                              L52800          /* Non Lot Track Test Days*/

            return

L52140: /* Select Part Range from HNYCCMST to Add to Selected Session */
            if (fmpart$ = "FIRST" and topart$ = "LAST") or               ~
                fmpart$ = "ALL" then return
            if fmpart$ <> "ALL" then L52162 else topart$ = " " : return
L52162:     if fmpart$ = " " and topart$ = " " then to% = 1% else to% = 0%
            if topart$ <> " " then to% = 1%
            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(1) = "  Part                         Description"
            header$(3) = hex(80) & "Select FROM Part Range"
            plowkey$ = fmpart$
            call "PLOWCODE" (#03, plowkey$, descr$, -8025%,-0.32,f1%(03),~
                         header$(), 0, 0, inc(), inc$(), " ", " ", #01)
            fmpart$ = plowkey$
            if (f1%(03) = 0% or f1%(03) = 1%) and to% = 1% then L52310
            if (f1%(03) = 0% or f1%(03) = 1%) and to% = 0% then L52344

L52310:     plowkey$ = topart$
            header$(3) = hex(80) & "Select TO Part Range "
            call "PLOWCODE" (#03, plowkey$, descr$,-8025%,-0.32, f1%(03),~
                             header$(), 0, 0, inc(), inc$(), " ", " ",   ~
                             #01)
            topart$ = plowkey$
            if fmpart$ <> " " and f1%(03) = 0% then L52331 else L52332
L52331:        if topart$ <> " " then L52332 else topart$ = fmpart$
L52332:     if topart$ < fmpart$ then L52354
            if fmpart$ = " " and topart$ = " " then fmpart$ = "ALL"
            if fmpart$ = " " and topart$ <> " " then fmpart$ = topart$
            if fmpart$ <> " " and topart$ = " " and fmpart$ <> "ALL"     ~
                                             then topart$ = fmpart$
            return
L52344:     if fmpart$ = "?" and topart$ = " " then L52352
            topart$ = fmpart$
            return

L52352:     errormsg$ = "Part Code Not On File."  : return
L52354:     errormsg$ = "FROM Part " & topart$ & " May Not Be Greater "  ~
                      & "Than TO Part " & fmpart$ : return

L52410: /* Select Store Range                 FMSTORE$/TOSTORE$ */
            gosub select_store_range
            return

L52450: /* Select Lot Range                    FMLOT$/TOLOT$    */
            if fmlot$ = "ALL" and tolot$ = " " then return
            call "TESTRNGE" (fmlot$, tolot$, hilot$, lolot$, errormsg$)
            return

L52500: REM Def/Enable Exclude zero Qty Lot Tracked Parts LTP_EX_FLG$
            if ltp_ex_flg$ =  "N" then L52570
            if ltp_ex_flg$ <> "Y" then L52550
                if edit% = 1% or ltp_ex_days$ <> " "  then  return
                    fieldnr% =  5%  :  goto L52600
L52550:     errormsg$ = "Please Enter 'Y' or 'N'."
            return
L52570:     ltp_ex_days$ = " "
            return

L52600: REM Def/Enable No. days w/o Activity for Lot Tracked LTP_EX_DAYS$
            convert ltp_ex_days$ to ltp_ex_days%, data goto L52670
            if ltp_ex_days% < 0% then L52670
                call "DATE" addr("G+", unfmtdate$,  - ltp_ex_days% ,     ~
                                                     ltp_ex_date$, u3%)
                return

L52670:     errormsg$ = "Please Enter a Positive Integer Number."
            return

L52700: REM Def/Enable Exclude zero Qty Nonlot Tracked Parts NLTP_EX_FLG$
            if nltp_ex_flg$ =  "N" then L52770
            if nltp_ex_flg$ <> "Y" then L52750
                if edit% = 1% or nltp_ex_days$ <> " "  then  return
                    fieldnr% =  7%  :  goto L52800
L52750:     errormsg$ = "Please Enter 'Y' or 'N'."
            return
L52770:     nltp_ex_days$ = " "
            return

L52800: REM Def/Enable No. days w/o Activity fr Nonlot Track NLTP_EX_DAYS$
            convert nltp_ex_days$ to nltp_ex_days%, data goto L52870
            if nltp_ex_days% < 0% then L52870
                call "DATE" addr("G+", unfmtdate$,  - nltp_ex_days% ,    ~
                                                    nltp_ex_date$, u3%)
                return

L52870:     errormsg$ = "Please Enter a Positive Integer Number."
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

        deffn'154(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53100          /* Cycle Count Group      */
            return

L53100: REM Test for Cycle Count Group          FMCCGROUP$/TOCCGROUP$
            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                fmccgroup$ = "ALL" then return
            if fmccgroup$ = " " and toccgroup$ = " " then to% = 1%       ~
                                                     else to% = 0%
            if toccgroup$ <> " " then to% = 1%
            descr$ = hex(06) & "Select FROM Group Range"
            call "PLOWCODE" (#05, fmccgroup$, descr$, 0%, 0.30, f1%(05))
             if (f1%(05) = 0% or f1%(05) = 1%) and to% = 1% then L53230
             if (f1%(05) = 0% or f1%(05) = 1%) and to% = 0% then L53320
L53230:     descr$ = hex(06) & "Select TO Group Range"
            call "PLOWCODE" (#05, toccgroup$, descr$, 0%, 0.30, f1%(05))
               if f1%(05) = 0% then L53270
                  if toccgroup$ < fmccgroup$ then L53380 else return
L53270:     if fmccgroup$ = " " and toccgroup$ = " " then                ~
                                           fmccgroup$ = "ALL"
            if fmccgroup$ = " " and toccgroup$ <> " " then               ~
                                           fmccgroup$ = toccgroup$
            if fmccgroup$ <> " " and toccgroup$ = " " then               ~
                                           toccgroup$ = fmccgroup$
            return
L53320:     if fmccgroup$ = "?" and toccgroup$ = " " then L53370
            toccgroup$ = fmccgroup$
            return

L53370:     errormsg$ = "Group Code Does Not Exist." : return
L53380:     errormsg$ = "FROM Group Code" & fmccgroup$ &                 ~
                        " may not be greater than TO Group Code" &       ~
                        toccgroup$                   : return

        select_store_range
            if fmstore$ <> "ALL" then L54050 else tostore$ = "  " : return
L54050:     if fmstore$ = " " and tostore$ = " " then to% = 1%           ~
                                                 else to% = 0%
            if tostore$ <> " " then to% = 1%

            descr$ = hex(06) & "Select FROM Store Range"
            call "PLOWCODE" (#07, fmstore$, descr$, 0%, .3, f1%(07))
               if (f1%(07) = 0% or f1%(07) = 1%) and to% = 1% then L54160
               if (f1%(07) = 0% or f1%(07) = 1%) and to% = 0% then L54250

L54160:     descr$ = hex(06) & "Select TO Store Range"
            call "PLOWCODE" (#07, tostore$, descr$, 0%, .3, f1%(07))
              if tostore$ < fmstore$ then L54320
              if fmstore$ = " " and tostore$ = " " then fmstore$ = "ALL"
              if fmstore$ = " " and tostore$ <> " " then                 ~
                                                    fmstore$ = tostore$
              if fmstore$ <> " " and tostore$ = " " and fmstore$ <> "ALL"~
                                               then tostore$ = fmstore$
            return
L54250:     if fmstore$ = "?" and tostore$ = " " then L54300
            tostore$ = fmstore$
            return
L54300:     errormsg$ = "Store Code Not File." : return
L54320:     errormsg$ = "FROM Store " & tostore$ &                       ~
                        " May Not Be Greater Than TO Store " &           ~
                        fmstore$               : return

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
