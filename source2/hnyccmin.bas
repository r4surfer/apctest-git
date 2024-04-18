        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   M   M  IIIII  N   N   *~
            *  H   H  NN  N  Y   Y  C   C  C   C  MM MM    I    NN  N   *~
            *  HHHHH  N N N   YYY   C      C      M M M    I    N N N   *~
            *  H   H  N  NN    Y    C   C  C   C  M   M    I    N  NN   *~
            *  H   H  N   N    Y     CCC    CCC   M   M  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCMIN - This program populates the Cycle Count Master  *~
            *            File with Part/Store/Lot Combinations selected *~
            *            from the HNYQUAN file.  The P/S/L/ combinations*~
            *            are chosen with a range selection sub-routine  *~
            *            and associated with a Cycle Count Group Name.  *~
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
            * 04/21/92 ! Original                                 ! RJ2 *~
            * 04/07/93 ! Default Count Period is set to '0'.      ! RJH *~
            * 05/04/93 ! Add Count Factor Field (Pieces per Hour) ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            abclockflag$1,               /* ABC Auto Change Lock Flag  */~
            ccgroup$6,                   /* Cycle Count Group Name     */~
            cntperiodflag$1,             /* CC Period Change Lock Flag */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            defaultlockflag$1,           /* Default Lock Flag          */~
            descr$50,                    /* PlowCode Description Variab*/~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$79,                   /* Sort Selection Screen Headr*/~
            i$(24%)80,                   /* Screen Image               */~
            r$(24%)80,                   /* Screen Image (Range Sub)   */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lot$16,                      /* Lot Number of Part         */~
            part$25,                     /* Part Number                */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pf16$,                       /* PF Range Select Literal    */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            putcount$10,                 /* Count of New Records Added */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            recdate$8,                   /* Record Start Date          */~
            store$3,                     /* Store/Warehouse of Part    */~
            translockflag$1,             /* Transactn Change Lock Flag */~
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
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
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
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #07 ! STORNAME ! Store Information File                   *~
            * #09 ! CATEGORY ! Inventory Category Code File             *~
            * #50 ! WORKFILE ! Temporary System Work File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

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

            select #05, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #09, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos  =   1, keylen = 4

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   44,              ~
                        keypos  =   1, keylen = 44

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            rec% = val(str(rslt$(02%),17%,4%),4%) / 2%
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), rec%, rslt$(03%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 0%, rslt$(09%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            recdate$ = date$  :  call "DATUNFMT" (recdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "HNYCCMIN: " & str(cms2v$,,8%)
            pf16$ = "(16)Generate"
            defaultlockflag$ = "N"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
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

            goto range_selection

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
            if fieldnr% < 1% or fieldnr% >  4% then editpg1
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
            /* Lets loop thru the Workfile of Part/Store/Lots and check */
            /* if that P/S/L is already in the Cycle Count Master File  */
            /* If not, lets add it to the HNYCCMST.                     */
            call "SHOSTAT" ("Populating Cycle Count Master File")
            readkey$ = all(hex(00))
            call "READ104" (#50, readkey$, f1%(50%))
            goto L19100
L19090:     call "READNEXT" (#50, f1%(50%))
L19100:     if f1%(50%) = 0% then goto display_askuser

            readkey$ = key(#50)
            call "READ100" (#03, readkey$,f1%(03%))/*Check the CC Master*/
            if f1%(03%) = 1% then goto L19090 /*Already there,get another*/

            part$ = str(readkey$,,25%)
            store$ = str(readkey$,26%, 3%)
            lot$   = str(readkey$,29%,16%)
            if store$ <> "001" then L19190
                call "READ100" (#07, store$, f1%(07%))
                if f1%(07%) = 0% then L19090  /*No Reserved Store Records*/
L19190:     putcount = putcount + 1

            gosub dataput
            goto L19090     /* Get next Workfile Selection */

        display_askuser
            call "CONVERT" (putcount, 0.01, putcount$)
            aukey% = 0%
            call "ASKUSER" (aukey%, " ", putcount$ &                     ~
                            " New Cycle Count Master File Records Added",~
                            "Press AnyKey to Continue" ,  " " )
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Cycle Count Group      */~
                              L20200,         /* ABC Lock Flag          */~
                              L20300,         /* Transaction Factor Lock*/~
                              L20400          /* Cycle Count Period Lock*/
            return

L20100: REM Def/Enable Cycle Count Group Name      CCGROUP$
            return

L20200: REM Def/Enable ABC Lock Flag               ABCLOCKFLAG$
            if abclockflag$ = " " then                                   ~
                 abclockflag$ = defaultlockflag$

            return

L20300: REM Def/Enable Tranasaction Factor Lock Flag  TRANSLOCKFLAG$
            if translockflag$ = " " then                                 ~
                 translockflag$ = defaultlockflag$

            return

L20400: REM Def/Enable Count Period Lock Flag      CNTPERIODFLAG$
            if cntperiodflag$ = " " then                                 ~
                 cntperiodflag$ = defaultlockflag$

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
         "Enter Cycle Count Group Name                                 ",~
         "Enter ABC Class Auto Lock Flag (Y or N)                      ",~
         "Enter Transaction Factor Auto Lock Flag (Y or N)             ",~
         "Enter Count Period Auto Lock Flag (Y or N)                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      ccgroup$, abclockflag$, translockflag$,            ~
                      cntperiodflag$
            putcount = 0.0
            call "FILEBGON" (#50)  /* CLose & Zap Workfile */
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
*       DATALOAD
        range_selection
            /* Call Range Selection Subroutine and make Workfile of   */
            /* desired Part/Store/Lots to add to HNYCCMST File.       */
            call "WORKOPEN" (#50, "IO", 200%, f2%(50))
            header$ = "Generate Cycle Count Master File for " & ccgroup$
            call "HNYCCRNG" ("N", 1%, #01, #02, #09, #07, #03, #50,      ~
                             count%, header$, pf16$, r$() )

            if count% = 0% then L30140
                goto datasave
            aukey% = 0%
L30140:     call "ASKUSER" (aukey%, " ", "No Records Selected that Meet" ~
                            & " the Range Criteria",                     ~
                            "Press Anykey to Continue", " ")
            goto inputmode
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            put #03 using L35035,               /* HNYCCMST             */~
            part$          ,/* Part Number (Product Code)              */~
            store$         ,/* Warehouse or Store                      */~
            lot$           ,/* Which lot in inventory - always used wit*/~
            ccgroup$       ,/* Cycle Count Group Code Name             */~
            " "            ,/* Date Last Counted                       */~
            0              ,/* Count Tolerance Percentage              */~
            0              ,/* Count Tolerance Quantity                */~
            0              ,/* Number of Counts                        */~
            " "            ,/* Active Session Flag                     */~
            " "            ,/* Next Count Date                         */~
            recdate$       ,/* Record Start Date                       */~
            0              ,/* Cumulative Tolerance Hits               */~
            0              ,/* Cumulative Count Delta (+)              */~
            0              ,/* Cumulative Count Delta (+)              */~
            0              ,/* Cumulative BOH Quantity                 */~
            abclockflag$   ,/* Lock Flag for ABC Class                 */~
            " "            ,/* ABC Class                               */~
            " "            ,/* Old ABC Class                           */~
            " "            ,/* User Who Last Changed ABC Class         */~
            " "            ,/* Date ABC Class Last Changed             */~
            translockflag$ ,/* Lock Flag for Transaction Factor        */~
            0              ,/* Transaction Frequency Factor            */~
            0              ,/* Old Transaction Frequency Factor        */~
            0              ,/* Transaction Threshold Factor            */~
            0              ,/* Old Transaction Threshold Factor        */~
            " "            ,/* User Last Changed Transaction Frequency */~
            " "            ,/* Date Transaction Frequency Factor Last C*/~
            cntperiodflag$ ,/* Lock Flag - Count Period                */~
            "0"            ,/* Count Period in (Days)                  */~
            " "            ,/* Old Count Period                        */~
            " "            ,/* User Who Last Changed Count Period      */~
            " "            ,/* Date Count Period Last Changed          */~
            " "            ,/* Date First Count was Made               */~
            " "            ,/* Variable Field                          */~
             0             ,/* Count Factor  (Pieces per Hour)         */~
            " "             /* Filler                                  */

            write #03
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35035: FMT                 /* FILE: HNYCCMST                          */~
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
            PD(14,4),       /* Cumulative Tolerance Hits               */~
            PD(14,4),       /* Cumulative Count Delta (+)              */~
            PD(14,4),       /* Cumulative Count Delta (+)              */~
            PD(14,4),       /* Cumulative BOH Quantity                 */~
            CH(1),          /* Lock Flag for ABC Class                 */~
            CH(1),          /* ABC Class                               */~
            CH(1),          /* Old ABC Class                           */~
            CH(3),          /* User Who Last Changed ABC Class         */~
            CH(6),          /* Date ABC Class Last Changed             */~
            CH(1),          /* Lock Flag for Transaction Factor        */~
            PD(14,4),       /* Transaction Frequency Factor            */~
            PD(14,4),       /* Old Transaction Frequency Factor        */~
            PD(14,4),       /* Transaction Threshold Factor            */~
            PD(14,4),       /* Old Transaction Threshold Factor        */~
            CH(3),          /* User Last Changed Transaction Frequency */~
            CH(6),          /* Date Transaction Frequency Factor Last C*/~
            CH(1),          /* Lock Flag - Count Period                */~
            CH(3),          /* Count Period in (Days)                  */~
            CH(3),          /* Old Count Period                        */~
            CH(3),          /* User Who Last Changed Count Period      */~
            CH(6),          /* Date Count Period Last Changed          */~
            CH(6),          /* Date First Count was Made               */~
            CH(200),        /* Variable Field                          */~
            PD(14,4),       /* Count Rate                              */~
            CH(387)         /* Filler                                  */

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
              on fieldnr% gosub L40130,         /* Cycle Count Group */   ~
                                L40130,         /* ABC Lock Flag     */   ~
                                L40130,         /* Transaction Lock  */   ~
                                L40130          /* Count Period Lock */
              goto L40160

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40130:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40160:     accept                                                       ~
               at (01,02),                                               ~
                  "Populate Cycle Count Master File",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Cycle Count Group Name",                     ~
               at (06,30), fac(lfac$( 1%)), ccgroup$            , ch(06),~
                                                                         ~
               at (07,02), "ABC Class Auto Change Lock",                 ~
               at (07,38), fac(lfac$( 2%)), abclockflag$        , ch(01),~
                                                                         ~
               at (08,02), "Transaction Factor Auto Change Lock",        ~
               at (08,38), fac(lfac$( 3%)), translockflag$      , ch(01),~
                                                                         ~
               at (09,02), "Count Period Auto Change Lock",              ~
               at (09,38), fac(lfac$( 4%)), cntperiodflag$      , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40660
                  call "MANUAL" ("HNYCCMIN") : goto L40160

L40660:        if keyhit% <> 15% then L40690
                  call "PRNTSCRN" : goto L40160

L40690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40840
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40840:     if fieldnr% > 2% then L40860
                str(pf$(2%),18%,26%) = " " :str(pfkeys$, 4%,1%) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                      (16)Populate File"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Cycle Count Group      */~
                              L50200,         /* ABC Lock Flag          */~
                              L50300,         /* Transaction Factor Lock*/~
                              L50400          /* Cycle Count Period Lock*/
            return
L50100: REM Test for Cycle Count Group Name       CCGROUP$
            plowkey$ = ccgroup$
            descr$ = hex(06) & "Select Cycle Count Group"
            call "PLOWCODE" (#05, plowkey$, descr$, 0%, 0.30, f1%(05%))
            if f1%(05%) = 1% then L50160
                errormsg$ = "Group Code Doesn't Exist" : return
L50160:     ccgroup$ = plowkey$
            return

L50200: REM Def/Enable ABC Lock Flag               ABCLOCKFLAG$
            p% = pos("YN" = abclockflag$)
            if p% <> 0% then return
                errormsg$ = "Select 'Y' or 'N' for Lock Flag."
            return

L50300: REM Def/Enable Tranasaction Factor Lock Flag  TRANSLOCKFLAG$
            p% = pos("YN" = translockflag$)
            if p% <> 0% then return
                errormsg$ = "Select 'Y' or 'N' for Lock Flag."
            return

L50400: REM Def/Enable Count Period Lock Flag      CNTPERIODFLAG$
            p% = pos("YN" = cntperiodflag$)
            if p% <> 0% then return
                errormsg$ = "Select 'Y' or 'N' for Lock Flag."
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
