        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC    SSS    SSS   N   N   *~
            *  H   H  NN  N  Y   Y  C   C  C   C  S      S      NN  N   *~
            *  HHHHH  N N N   YYY   C      C       SSS    SSS   N N N   *~
            *  H   H  N  NN    Y    C   C  C   C      S      S  N  NN   *~
            *  H   H  N   N    Y     CCC    CCC    SSS    SSS   N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCSSN - This Program will automatically populate a     *~
            *            Cycle Count Session with the highest priority  *~
            *            parts to count.  The population may be limited *~
            *            by the store number or the Cycle Count Group   *~
            *            name.                                          *~
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
            * 05/11/92 ! Original                                 ! RJH *~
            * 05/03/93 ! Allow two methods of loading Parts - by  ! RJH *~
            *          !   number of Parts or number of Person-hrs!     *~
            * 07/21/93 ! Initalize Part Total Counter             ! RJH *~
            * 08/10/93 ! Disallow Edit of 1st two fields.         ! RJH *~
            * 10/29/93 ! PRR 13052 Option to exclude from a sess'n! RJH *~
            *          !   zero qty Parts (Lot/Non Tracked) with  !     *~
            *          !   with no activity for 'X' number of days!     *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            actflag$1,                   /* Cycle Count Activity Flag  */~
            activity_date$6,             /* HNYDETAL Last activity date*/~
            basedate$8,                  /* Next Date Calculation Date */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            category$4,                  /* Part Category Code         */~
            ccgroup$6,                   /* Selection Group Code       */~
            ccquan$10,                   /* Selection Quantity Limit   */~
            ccstore$3,                   /* Selection Store            */~
            checkflag$1,                 /* NextCountDate Selection Flg*/~
            cntperiod$3,                 /* Cycle Count Part Period    */~
            countflag$2,                 /* Exceeded Count Limit Flag  */~
            countrate$10,                /* Count Rate per hour        */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datelastcnt$8,               /* Last Count Date of Part    */~
            descr$42,                    /* Description for PlowCode   */~
            descr_m(6%),                 /* Descript'n map for PlowCode*/~
            askmsg$40,                   /* Askuser Message            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$(3%)79,               /* Header for PlowCode        */~
            group$6,                     /* Record's CC Group Code     */~
            i$(24%)80,                   /* Screen Image               */~
            inc(2%),                     /* PlowCode Arguments         */~
            inc$(2%)16,                  /* PlowCode Arguments         */~
            inpmessage$79,               /* Informational Message      */~
            keyactflag$1,                /* Act Flag Criteria in Loop  */~
            lastlafactor$6,              /* Last  Look Ahead Factor    */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lookaheadfactor$6,           /* Look Ahead Factor          */~
            ltp_ex_flg$1,                /* Lot Tracked Exclude Flag   */~
            ltp_ex_date$6,               /* Lot Tracked Inactive Date  */~
            ltp_ex_days$3,               /* Lot Tracked Inactive Days  */~
            maxhours$10,                 /* Max Person-hours to use    */~
            minhours$10,                 /* Minimum hours per Part     */~
            maxparts$10,                 /* Max Parts to Count         */~
            nextcntdate$8,               /* Prposd Part Next Count Date*/~
            nextsesdate$8,               /* Proposed Next Session Date */~
            nltp_ex_flg$1,               /* NotLot Tracked Exclude Flag*/~
            nltp_ex_date$6,              /* NotLot Tracked Inactiv Date*/~
            nltp_ex_days$3,              /* NotLot Tracked Inactiv Days*/~
            overcount$10,                /* Over Count Amount          */~
            part$25,                     /* Part Number                */~
            partstrlot$44,               /* Part Store Lot Combo       */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plancntdate$8,               /* Planned Count Date         */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            qtymethod$1,                 /* Count Method 'P'art/'M'anHr*/~
            quantotal$10,                /* Total Quantity of Parts    */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            recstartdate$8,              /* Record Start Date          */~
            session$12,                  /* Session Name               */~
            sessiondescr$30,             /* Session Decription         */~
            store$3,                     /* Store/Warehouse            */~
            testdate$8,                  /* Zero Qty Exclusion Date    */~
            testprddate$8,               /* Date to Include Xtra Parts */~
            typetot$10,                  /* Total Part/Store/Lot Combos*/~
            unfmtdate$6,                 /* Unformated system date     */~
            userid$3                     /* Current User Id            */~

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

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
            * #01 ! HNYMASTR ! Inventory Part Master File               *~
            * #02 ! HNYQUAN  ! Inventory Cost and Quantity File         *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #06 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #07 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #09 ! CATEGORY ! Iventory Category Codes File             *~
            * #10 ! HNYDETAL ! Inventory Details                        *~
            * #50 ! WORKFILE ! Temporary System Workfile                *~
            * #55 ! DUMMY    ! Dummy SELECT File - no actual file assoc *~
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
                        alt key  1, keypos =    1, keylen =  44


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

            select #06, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  2, keypos =   56, keylen =  10,         ~
                            key  1, keypos =   43, keylen =  13, dup     ~

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

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
                        keypos = 1,    keylen =  44

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            rec2% = val(str(rslt$(02%),17%,4%),4%) /10%
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            rec3% = val(str(rslt$(03%),17%,4%),4% ) / 2%
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), rec2%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 100%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 0%, rslt$(09%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))

            goto L09050

        open_workfiles
            call "FILEBGON" addr(#50)
            call "WORKOPEN" (#50, "IO", rec3%, f2%(50%))
            return


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

L09050:     call "EXTRACT" addr("ID", userid$)
            unfmtdate$, date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "HNYCCSSN: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  13%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10250
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
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
L10250:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       data_process
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% = 10% then editpg1      /* Blank line on screen */
            if fieldnr% > 10% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 3% or fieldnr% > 13% then editpg1
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
            *     P R O C E S S    C Y C L E   C O U N T   P A R T S    *~
            *-----------------------------------------------------------*~
            * Process Cycle Count Parts after INPUT/EDITING.            *~
            *************************************************************

        data_process
            gosub select_cc_parts /*Qualify the Parts & Populate Session*/
            gosub dataput_2       /* Set Activity Flag to Pre-Active */
            gosub display_results
            goto inputmode

        display_results
            askmsg$ = " "
            call "CONVERT" (typetot    , -0.01, typetot$  )
            if qtymethod$ = "P" then L19530
            if typetot = 0.0 then quantotal = 0.0
            call "CONVERT" (quantotal  , -0.01, quantotal$)
            askmsg$ = "For a Pieces to Count of " & hex(80) & quantotal$

L19530:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "**** RESULTS ****",                ~
                " A Total of " & hex(80) & typetot$ &  " Parts were"  &  ~
                " Selected",                                             ~
                 askmsg$,                                                ~
                "Press Any Key to Continue")

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Session Name           */~
                              L20200,         /* Planned Count Dat      */~
                              L20300,         /* Cycle Count Group      */~
                              L20400,         /* Store/Warehouse        */~
                              L20500,         /* Quantity of Parts      */~
                              L20550,         /* Minimum Person-hours   */~
                              L20575,         /* Count rate per hour    */~
                              L20600,         /* Proposed Next Ses      */~
                              L20700,         /* Look Ahead Factor      */~
                              L20750,         /* Lot Tracked Excude Flg */~
                              L20850,         /* Lot Tracked Test Days  */~
                              L20800,         /* Non Lot Track Exclude F*/~
                              L20900          /* Non Lot Track Test Days*/
            return

L20100: REM Def/Enable Session Name                SESSION$
            return

L20200: REM Def/Enable Planned Count Date        PLANCNTDATE$
            enabled% = 0%
            return

L20300: REM Def/Enable Cycle Count Group Code      CCGROUP$
            ccgroup$ = "ALL"
            return

L20400: REM Def/Enable Store/Warehouse             CCSTORE$
            ccstore$ = "ALL"
            return

L20500: REM Def/Enable Quantity of Parts to Count  CCQUAN
            return

L20550: REM Def/Enable Minimum Person-hours per Part   MINHOURS$
            if qtymethod$ = "P" then enabled% = 0% else L20570
                return
L20570:     minhours$ = "0.25"
            return

L20575: REM Def/Enable Count Rate                    COUNTRATE$
            if qtymethod$ = "P" then enabled% = 0%
            return

L20600: REM Def/Enable Proposed Next Session Date  NEXTSESDATE$
            return

L20700: REM Def/Enable Look ahead factor             LOOKAHEADFACTOR$
            lookaheadfactor$  =  "0.5"

            return

L20750: REM Def/Enable Exclude zero Qty Lot Tracked Parts LTP_EX_FLG$
            if ltp_ex_flg$ = " "  then  ltp_ex_flg$  =  "Y"
            return

L20800: REM Def/Enable Exclude zero Qty Nonlot Tracked Parts NLTP_EX_FLG$
            if nltp_ex_flg$ = " "  then  nltp_ex_flg$  =  "N"
            return

L20850: REM Def/Enable No. days w/o Activity for Lot Tracked LTP_EX_DAYS$
            if ltp_ex_flg$  =  "Y" then L20875
                ltp_ex_days$  =  " "
                enabled% = 0%
                return
L20875:     if ltp_ex_days$ = " " then  ltp_ex_days$  =  "60"
            return

L20900: REM Def/Enable No. days w/o Activity fr Nonlot Track LTP_EX_DAYS$
            if nltp_ex_flg$  =  "Y" then L20950
                nltp_ex_days$  =  " "
                enabled% = 0%
                return
L20950:     if nltp_ex_days$ = " " then  nltp_ex_days$  =  "60"
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
         "Enter Session Name                                           ",~
         "Enter Planned Count Date                                     ",~
         "Enter Cycle Count Group Code                                 ",~
         "Enter Store/Warehouse                                        ",~
         "Enter the Number of Parts to count OR Person-hours to use    ",~
         "Enter Minimum Number of Person-hours per Part                ",~
         "Enter Default Hourly Count rate (Pieces per hour)            ",~
         "Enter Proposed Next Session Date                             ",~
         "Enter Decimal Ratio of a Part's Period for Extra Parts Inclsn",~
         "Exclude Lot Tracked Parts with zero quantity and no activity ",~
         "Enter Number of Days of No Activity for Lot Tracked exclusion",~
         "Exclude Non-Lot Tracked Parts with zero qty and no activity  ",~
         "Enter No. of Days of No Activity for Non-Lot Tracked exclusion"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, quantotal$, typetot$,      ~
                      ccgroup$, session$, plancntdate$, nextsesdate$,    ~
                      ccquan$, ccstore$, sessiondescr$, lookaheadfactor$,~
                      maxhours$, minhours$, maxparts$, countrate$,       ~
                      ltp_ex_flg$, ltp_ex_days$, ltp_ex_date$, testdate$,~
                      nltp_ex_flg$, nltp_ex_days$, nltp_ex_date$,        ~
                      activity_date$, lot_track_flg$
            call "ALLFREE"

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
            *   S E L E C T   C C   P A R T S    F R O M   F I L E      *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        select_cc_parts

            call "SHOSTAT" ("Selecting Cycle Count Parts")
            gosub open_workfiles
            hourstotal, quantotal, typetot  = 0.0   :  countflag$ = "OK"
            parttotal = 0.0
            basedate$ = plancntdate$
            call "DATUNFMT" (basedate$)
            call "DATUNFMT" (nextsesdate$)
            loop% = 0%        :  keyactflag$ = "C"
L30130:     gosub start_selection_loops
            if loop% > 3% then return        /* ** Thats All Folks ** */
            goto L30170

            /* Begin Processing Loop Data */
        loop_cc_master
            if countflag$ = "XX" then return /* ** Thats All Folks ** */
            call "READNEXT" (#03, f1%(03%))
L30170:     if f1%(03%) = 0% then goto L30130

            get #03 using L35250, part$, store$, lot$, group$,            ~
                                 datelastcnt$, cnttlernper, cnttlernqty, ~
                                 actflag$, nextcntdate$,                 ~
                                 recstartdate$, cntperiod$, count_factor
            if keyactflag$ <> actflag$ then L30130 /* Start Another Loop */
            if cntperiod$ <= " " or cntperiod$ = "0"                     ~
                              then loop_cc_master /*Needs Count Period*/
            if qtymethod$ = "P" or count_factor > 0 then L30230
                gosub check_defaults_count_factor
                if count_factor <= 0 then loop_cc_master /*Needs Factor */
L30230:     partstrlot$ = str(part$) & str(store$) & lot$
            call "READ100" (#50, partstrlot$, f1%(50%))
            if f1%(50%) = 1% then loop_cc_master

            /* Test Screen Selection */
            if ccgroup$ = "ALL" then L30300
                if group$ <> ccgroup$ then loop_cc_master /*Try Another*/
L30300:     if ccstore$ = "ALL" then L30315
                if store$ <> ccstore$ then loop_cc_master /*Try Another*/

L30315:     gosub check_date   /* Test NextCountDate for Need */
            if checkflag$ = "N" then loop_cc_master   /*Try Another*/

            /* Check for zero qty */
            call "READ100" (#02, partstrlot$, f1%(02%))
            if f1%(02%) = 0% then loop_cc_master  /* Not in HNYQUAN */
            get #02 using L30576, quan
            if quan <> 0 then L30370
            gosub check_hnydetal   /* Test for Flags and Activity */
            if checkflag$ = "N" then loop_cc_master   /*Try Another*/

L30370:     if qtymethod$ = "P" then L30550  /* Limit by Parts */

            /* Check Quantity */
            if count_factor = 0 or quan <= 0 then L30395  /* Use MinHour */
            quantotal = quantotal + quan
            if quan / count_factor < minhours then L30395
                hourstotal = hourstotal + quan / count_factor
                goto L30400
L30395:     hourstotal = hourstotal + minhours
L30400:     if hourstotal < maxhours then L30510
                if loop% = 1% then L30415 /* If Part is Late lets Ask */
                     /* Check how close to Quantity limit */
                     /* If close we'll Quite else try another part */
                     if (hourstotal - quan/count_factor)/maxhours > 0.95 ~
                            then L30472          else goto loop_cc_master
L30415:         overcount = hourstotal - maxhours
                call "CONVERT" (overcount, -2.2, overcount$)
L30420:         keyhit% = 2%
                call "ASKUSER" (keyhit%, "** POSSIBLE OVERCOUNT **",     ~
                                part$ & " will exceed your"              ~
                                & " Planned Hours By " & overcount$,     ~
                                "To Accept   - Press RETURN",            ~
                                "To Stop Now - Press PF16"             )
                if keyhit% = 0% then L30490
                if keyhit% <> 16% then L30420
L30472:              quantotal = quantotal - quan
                     return                /* ** Thats All Folks ** */

L30490:         countflag$ = "XX"   /* Last Selection */

            /* Everything Is OK So Lets Include It */
L30510:     gosub dataput_1

            goto loop_cc_master         /* Back Around Again */

L30550:     parttotal = parttotal + 1
            if parttotal >= maxparts then L30490   /* Set Flag and Last */~
                                     else L30510   /* Data PUT */

            /* HNYQUAN File */
L30576:     FMT POS(69), PD(14,4)

        check_defaults_count_factor
            count_factor = 0
            call "READ100" (#1, part$, f1%(1%))
            if f1%(1%) = 0% then return
            get #1 using L30660, category$, count_factor
            if count_factor > 0.0 then return
            call "READ100" (#9, category$, f1%(9%))
            if f1%(9%) = 0% then L30642
            get #9 using L30670, count_factor
L30642:     if count_factor = 0.0 then count_factor = countrate
            return
L30660: FMT POS(90), CH(4), POS(119), PD(14,4)    /* Hnymastr */
L30670: FMT POS(53), PD(14,4)                     /* Category */

        start_selection_loops
            if keyactflag$ = " " then   goto L30715
                keyactflag$ = " "  :  loop% = loop% + 1%  :  goto L30720
L30715:         keyactflag$ = "C"
L30720:     str(plowkey$, ,1%) = keyactflag$
            str(plowkey$,2%, ) = all(hex(00))
            call "REDALT4" (#03, plowkey$, 2%, f1%(03%))

            return
           /* *** End START_SELECTION_LOOPS Sub *** */

        check_date
            convert cntperiod$ to cntperiod
            cntperiod% = cntperiod

            if loop% <> 1% then L30850
           /* EARLY Using Current Session Earlier than NextCountDate */
                if nextcntdate$ >  basedate$ then L30930                  ~
                                            else L30920     /* Proccess */

L30850:     if loop% <> 2% then L30880
           /* LATE Using Next Session After NextCountDate */
            if nextcntdate$ > nextsesdate$ then L30930                    ~
                                          else L30920       /* Proccess */

           /* EXTRA Parts Using LookAheadFactor */
L30880:     testprd% = int(cntperiod% * lookaheadfactor)
            call "DATE" addr("G+", nextsesdate$, testprd%,               ~
                                                   testprddate$, err%)
               err% = err%
            if nextcntdate$ > testprddate$ then L30930
            /* Test if Actual Count Period to Short */
                if datelastcnt$ <> " " and datelastcnt$ <> blankdate$ ~
                                     then L30920/*Never Counted So Take*/
                call "DATE" addr("G-", datelastcnt$, basedate$, testprd%,~
                                                                   err%)
                if testprd% < 0.75 * cntperiod then L30930 /* Too Soon */

L30920:     checkflag$ = "Y"  :   return          /* Date Qualifies */
L30930:     checkflag$ = "N"  :   return  /* Date doesn't Qualifies */
          /* *** End CHECK_DATE Sub *** */

        check_hnydetal        /* check for exclusion due to no activity */
            checkflag$ = "Y"                        /* start out OK */
            call "READ100" (#1, part$, f1%(1%))
            get #1 using L30962, lot_track_flg$
L30962:     FMT POS(130), CH(1)
            if lot_track_flg$ = "Y" then  L30976  /* must be lot tracked */
                if nltp_ex_flg$ = "N" then return
                    testdate$ = nltp_ex_date$
                    goto L30980
L30976:     if ltp_ex_flg$ = "N" then return           /* Dont exclude */
                testdate$ = ltp_ex_date$
L30980:     /* Test activity date */
            plowkey2$ = str(partstrlot$) & hex(00)
            call "PLOWNEXT" (#10, plowkey2$, 34%, f1%(10%))
            if f1%(10%) = 0% then L30996
            get #10 using L30990, activity_date$
L30990:     FMT POS(43), CH(6)
            if testdate$ <= activity_date$ then return  /* Dont exclude */
L30996:         checkflag$ = "N"                     /* Exclude      */
                return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput_1    /* Populate Session Detail File */
            readkey$ = str(session$) & "A" & partstrlot$
            call "READ100" (#04, readkey$, f1%(04%))
            if f1%(04%) <> 0% then L31090
            readkey$ = str(session$) & "P" & partstrlot$
            call "READ100" (#04, readkey$, f1%(04%))
            if f1%(04%) =  0% then L31100
L31090:         quantotal = quantotal - quan
                return  /* Shouldn't Happen */
            /* Lets Start Putting */
L31100:     put #50 using L35400, partstrlot$
            write #50

            put #04 using L35030, session$, "P", part$, store$, lot$,     ~
                                 "A", " ", " ", 0, 0, 0, 0,              ~
                                 cnttlernper, cnttlernqty, " ",          ~
                                 cntperiod$, " ", " ", " ", " "
            write #04

            typetot = typetot + 1

            return

        dataput_2   /* Set Activity Flag to Pre-Active in HNYCCMST File */
            readkey$ = all(hex(00))
            call "READ105" (#50, readkey$, f1%(50%))
        loop_workfile
            if f1%(50%) = 0% then return  /* All Done */
            partstrlot$ = key(#50)

            call "READ101" (#03, partstrlot$, f1%(03%))
            if f1%(03%) = 0% then return  /* Shouldn't Happen */
            put #03 using L35500, "P"
            rewrite #03

            call "READNEXT" (#50, f1%(50%))
            goto loop_workfile


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYCCDTL                          */~
            CH(12),         /* Session Name                            */~
            CH(1),          /* Active Session Flag                     */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(1),          /* Flag Show How Session Got Created  C-CC,*/~
            CH(6),          /* Count Date                              */~
            CH(3),          /* Session Entered By (Set up)             */~
            PD(14,4),       /* Number of Units Counted                 */~
            PD(14,4),       /* Unit Value                              */~
            PD(14,4),       /* Unit Variance                           */~
            PD(14,4),       /* Value Variance                          */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            CH(6),          /* Last Count Date                         */~
            CH(3),          /* Last Count Period                       */~
            CH(6),          /* Date of Last Transfer                   */~
            CH(6),          /* Variance Reason Code                    */~
            CH(12),         /* Counted By Name                         */~
            CH(288)         /* Unused Space                            */~

L35250: FMT                 /* FILE: HNYCCMST                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(6),          /* Cycle Count Group Code Name             */~
            CH(6),          /* Date Last Counted                       */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            POS(81),        /*                                         */~
            CH(1),          /* Active Session Flag                     */~
            CH(6),          /* Next Count Date                         */~
            CH(6),          /* Record Start Date                       */~
            POS(181),       /*                                         */~
            CH(3),          /* Count Period in (Days)                  */~
            POS(402),       /* Skip Variable Field                     */~
            PD(14,4)        /* Count Factor                            */

L35400: FMT                 /* Work File                               */~
            CH(44)          /* Part/Store/Lot Key                      */

L35500: FMT                 /* HNYCCMST File                           */~
            POS(81),        /*                                         */~
            CH(1)           /* Activity Flag                           */

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
              on fieldnr% gosub L40115,         /* Session Name      */   ~
                                L40115,         /* Planned Count Dat */   ~
                                L40115,         /* Cycle Count Group */   ~
                                L40115,         /* Store/Warehouse   */   ~
                                L40120,         /* Quantity of Parts */   ~
                                L40120,         /* Minimum Person-hours*/ ~
                                L40120,         /* Minimum Default hrs */ ~
                                L40115,         /* Proposed Next Ses */   ~
                                L40120,         /* Look Ahead Factor */   ~
                                L40115,       /* Lot Tracked Excude Flg */~
                                L40120,       /* Lot Tracked Test Days  */~
                                L40115,       /* Non Lot Track Test FLG */~
                                L40120        /* Non Lot Track Test Days*/
              goto L40130

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40115:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40120:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40130:     accept                                                       ~
               at (01,02),                                               ~
                  "Auto Populate Cycle Count Session",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Session Name",                               ~
               at (06,30), fac(lfac$( 1%)),  session$           , ch(12),~
               at (06,46), fac(hex(8c))  ,   sessiondescr$      , ch(30),~
                                                                         ~
               at (07,02), "Planned Count Date",                         ~
               at (07,30), fac(lfac$( 2%)), plancntdate$        , ch(08),~
                                                                         ~
               at (08,02), "Cycle Count Group Code",                     ~
               at (08,30), fac(lfac$( 3%)), ccgroup$            , ch(06),~
                                                                         ~
               at (09,02), "Store/Warehouse",                            ~
               at (09,30), fac(lfac$( 4%)), ccstore$            , ch(03),~
                                                                         ~
               at (10,02), "Number of Parts to Count   ",                ~
               at (10,30), fac(lfac$( 5%)), maxparts$           , ch(10),~
               at (10,44), " OR  Person-hours",                          ~
               at (10,69), fac(lfac$( 5%)), maxhours$           , ch(10),~
                                                                         ~
               at (11,49), "Min Hour/Part",                              ~
               at (11,69), fac(lfac$( 6%)), minhours$           , ch(10),~
                                                                         ~
               at (12,49), "Default Count Rate",                         ~
               at (12,69), fac(lfac$( 7%)),  countrate$         , ch(10),~
                                                                         ~
               at (13,02), "Proposed Next Session Date",                 ~
               at (13,30), fac(lfac$( 8%)), nextsesdate$        , ch(08),~
                                                                         ~
               at (14,02), "Look Ahead Factor        ",                  ~
               at (14,30), fac(lfac$( 9%)),  lookaheadfactor$   , ch(06),~
                                                                         ~
               at (16,02), "Exclude Zero Qty for Lot Tracked Parts",     ~
               at (16,42), fac(lfac$(10%)),  ltp_ex_flg$        , ch(01),~
                                                                         ~
               at (17,19), "If no activity within",                      ~
               at (17,42), fac(lfac$(11%)),  ltp_ex_days$       , ch(03),~
               at (17,46), "days",                                       ~
                                                                         ~
               at (18,02), "Exclude Zero Qty for NonLot Track Parts",    ~
               at (18,42), fac(lfac$(12%)),  nltp_ex_flg$       , ch(01),~
                                                                         ~
               at (19,19), "If no activity within",                      ~
               at (19,42), fac(lfac$(13%)),  nltp_ex_days$      , ch(03),~
               at (19,46), "days",                                       ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40455
                  call "MANUAL" ("HNYCCSSN") : goto L40130

L40455:        if keyhit% <> 15% then L40470
                  call "PRNTSCRN" : goto L40130

L40470:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40565     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40545
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40545:     if fieldnr% > 3% then L40555
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40555:     return

L40565: if fieldnr% > 0% then L40610  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40610:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Session Name           */~
                              L50570,         /* Planned Count Dat      */~
                              L50700,         /* Cycle Count Group      */~
                              L50800,         /* Store/Warehouse        */~
                              L50900,         /* Quantity of Parts      */~
                              L52010,         /* Minimum Person-hours   */~
                              L52070,         /* Default Count rate     */~
                              L51000,         /* Proposed Next Ses      */~
                              L51100,         /* Look Ahead Factor      */~
                              L51250,         /* Lot Tracked Excude Flg */~
                              L51330,         /* Lot Tracked Test Days  */~
                              L51400,         /* Non Lot Track Exclude F*/~
                              L51450          /* Non Lot Track Test Days*/
            return

L50100: REM Test for Session Name                   SESSION$
            if session$ =  " " or session$ = "?" then L50360
            readkey$ = str(session$)
            call "READ100" (#06, readkey$, f1%(06%))
             if f1%(06%) = 0% then L50230
                if str(key(#06,1),,1%) = "P" then L50170
                   errormsg$ = "Session is Currently Active or Closed"
                   return
L50170:      if plancntdate$ <> " " and plancntdate$ <> blankdate$ then L50570
             get #06 using L50190, plancntdate$
L50190:          FMT POS(66), CH(6)
L50200:        call "DATEFMT" (plancntdate$)
L50210:        call "DESCRIBE" (#06, session$,sessiondescr$, 0%, f1%(06%))
               goto L50340
L50230:           keyhit% = 2%
                  call "ASKUSER" (keyhit%, "** CONFIRMATION **",         ~
                   hex(8c) & "Session: " & hex(84) & session$ & hex(8c) &~
                   " Does Not Exist " & hex(84),                         ~
                   hex(8c) & "To Create, Press RETURN." & hex(84),       ~
                   "Hit any other PF key To abort")
               if keyhit% <> 0% then L10130
                 call "HNYCCSIP" (session$, sessiondescr$, plancntdate$, ~
                                  #06, #04)
                 readkey$ = str(session$)
                 call "READ100" (#06, readkey$, f1%(06%))
                 if f1%(06%) <> 0% then L50330
                     errormsg$ = "Please Enter a Session Name."
                     sessiondescr$, plancntdate$ = " "
                     return

L50330:        goto L50210
L50340:        fieldnr% = 2%
               return

L50360:     init(hex(00)) str(plowkey$,,99%)
            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(3%) = hex(80) & "Select a Session Name"
            header$(1%) = "  Session Name     Description               "~
                       & "       Planned Count Date"
            descr_m(01%) =     1.12  : descr_m(02%) = 0001.0
            descr_m(03%) =    13.30  : descr_m(04%) = 0018.0
            descr_m(05%) =    66.061 : descr_m(06%) = 0051.0

            inc(1%) = 43.01 : inc$(1%) = "P"

            call "PLOWCODE" (#06, plowkey$, descr$, 9000%, 0.42, f1%(06),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #55, descr_m())
            if f1%(06) <> 0% then goto L50520
                errormsg$ = "Session Name Required"  :  return
L50520:     session$ = str(plowkey$, 1%,12%)
            call "DESCRIBE" (#06, session$, sessiondescr$, 0%, f1%(06%))
            get #06 using L50550, plancntdate$
L50550:             FMT POS(66), CH(6)
            if plancntdate$ = " " or plancntdate$ = blankdate$ then return
            goto L50200

L50570:     /* Test for Planned Count Date           PLANCNTDATE$  */
            if plancntdate$ = " " or plancntdate$ = blankdate$ ~
                                then plancntdate$ = date$
            call "DATEOK" (plancntdate$, 0%, errormsg$)
            if errormsg$ <> " " then return
            if date$ > plancntdate$ then errormsg$ =                     ~
                  "Planned Count Date Must Be Equal or Greater Than "    ~
                & "Today's Date."
            if errormsg$ <> " " then fieldnr% = 2%                       ~
                     else fieldnr% = 3%   /* Jump down past Date Field */

            return

L50700: REM Test for Cycle Count Group Code       CCGROUP$
            descr$ = hex(06) & "Select a Cycle Count Group Code."
            if ccgroup$ = "?" then ccgroup$ = hex(00)
            if ccgroup$ = "ALL" then return

            call "GETCODE"  (#05, ccgroup$, descr$, 0%, 0.30, f1%(05%))
            if f1%(05%) = 1% then  return
            if ccgroup$ = " " then return
                errormsg$ = "Group Code Not on Record"
            return

L50800: REM Test for Store/Warehouse              STORE$
            if ccstore$ = "?" then ccstore$ = all(hex(00))
            if ccstore$ = "ALL" then return
            descr$ = hex(0684) & "Select a Store Number"
            call "PLOWCODE" (#07, ccstore$, descr$, 0%, 0.30, f1%(7%))
            if f1%(7%) = 0% then                                         ~
                errormsg$ = "Store Number not on file."
            return

L50900: REM Test for Quantity of Parts to Count   MAXPARTS$ MAXHOURS$
            if maxhours$ <= " " and maxparts$ <= " " then L50910 else L50920
L50910:         errormsg$ = "Please Enter Parts to count OR Person-hours."
                return
L50920:     if maxhours$ > " " and maxparts$ > " " then L50910
            if maxhours$ > " " then L50955
                qtymethod$ = "P"
                convert maxparts$ to maxparts , data goto L50945
                if maxparts > 0.0 then return
L50945:         errormsg$ = "Enter a Value Greater than Zero"
                return
L50955:     qtymethod$ = "M"
            convert maxhours$ to maxhours , data goto L50945
            if maxhours > 0.0 then return  else L50945

L51000: REM Test for Proposed Next Session Date   NEXTSESDATE$
            call "DATEOK" (nextsesdate$, nextsesdate%, errormsg$)
              nextsesdate% = nextsesdate%
                if errormsg$ <> " " then return
            call "DATUNFMT" (plancntdate$)
            call "DATUNFMT" (nextsesdate$)
            if plancntdate$ < nextsesdate$ then L51060
                errormsg$ = "Next Session date must be greater than"  &  ~
                             " Count Date."
L51060:     call "DATEFMT" (plancntdate$)
            call "DATEFMT" (nextsesdate$)
            return

L51100: REM Test for Look Ahead Factor            LOOKAHEADFACTOR$
            /* Multiplier for how far into the fulture for          */
            /* NextCountDate to still be include as an EXTRA.   */
            if lookaheadfactor$  <> " " then L51150
                errormsg$ = "'Blank' Not Allowed"
L51150:     if lastlafactor$ = lookaheadfactor$ then return/*Must WantAh*/
            convert  lookaheadfactor$ to lookaheadfactor
            if lookaheadfactor  <= 1.0  then L51210
            errormsg$ = "Not Recomended to have Factor Greater Than 1.0"
            lastlafactor$ = lookaheadfactor$

L51210:     return

L51250: REM Def/Enable Exclude zero Qty Lot Tracked Parts LTP_EX_FLG$
            if ltp_ex_flg$ =  "N" then L51290
            if ltp_ex_flg$ <> "Y" then L51280
                if edit% = 1% or ltp_ex_days$ <> " "  then  return
                    fieldnr% = 11%  :  goto L51330
L51280:     errormsg$ = "Please Enter 'Y' or 'N'."
            return
L51290:     ltp_ex_days$ = " "
            return

L51330: REM Def/Enable No. days w/o Activity for Lot Tracked LTP_EX_DAYS$
            convert ltp_ex_days$ to ltp_ex_days%, data goto L51375
            if ltp_ex_days% < 0% then L51375
                call "DATE" addr("G+", unfmtdate$,  - ltp_ex_days% ,     ~
                                                     ltp_ex_date$, u3%)
                return

L51375:     errormsg$ = "Please Enter a Positive Integer Number."
            return

L51400: REM Def/Enable Exclude zero Qty Nonlot Tracked Parts NLTP_EX_FLG$
            if nltp_ex_flg$ =  "N" then L51435
            if nltp_ex_flg$ <> "Y" then L51425
                if edit% = 1% or nltp_ex_days$ <> " "  then  return
                    fieldnr% = 13%  :  goto L51450
L51425:     errormsg$ = "Please Enter 'Y' or 'N'."
            return
L51435:     nltp_ex_days$ = " "
            return

L51450: REM Def/Enable No. days w/o Activity fr Nonlot Track NLTP_EX_DAYS$
            convert nltp_ex_days$ to nltp_ex_days%, data goto L51510
            if nltp_ex_days% < 0% then L51510
                call "DATE" addr("G+", unfmtdate$,  - nltp_ex_days% ,    ~
                                                    nltp_ex_date$, u3%)
                return

L51510:     errormsg$ = "Please Enter a Positive Integer Number."
            return


L52010: REM Test for Minimum Person-hours             MINHOURS$
            convert minhours$ to minhours , data goto L52040
            if minhours > 0.0 then return
L52040:         errormsg$ = "Enter a Value Greater than Zero"
            return

L52070: REM Test for Count Rate                   COUNTRATE$
            if countrate$ <> " " then L52100
                countrate$ = "0"
L52100:     countrate = 0
            convert countrate$ to countrate, data goto L52120
L52120:     if countrate <= 0 then                                       ~
                errormsg$ = "Enter a Positive Number        "
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
