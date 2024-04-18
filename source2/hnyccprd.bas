        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   PPPP   RRRR   DDDD    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  P   P  R   R  D   D   *~
            *  HHHHH  N N N   YYY   C      C      PPPP   RRRR   D   D   *~
            *  H   H  N  NN    Y    C   C  C   C  P      R   R  D   D   *~
            *  H   H  N   N    Y     CCC    CCC   P      R   R  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCPRD - This program allow users to calculate the cycle*~
            *            count period for a range of parts based on     *~
            *            either by ABCD Class, transaction frequency or *~
            *            a combination of both The results are written  *~
            *            to the cycle count master file. (HNYCCMST)     *~
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
            * 07/22/92 ! Original                                 ! SID *~
            * 05/04/93 ! Look to HNYMASTR for ABC Class if needed.! JDH *~
            * 02/28/94 ! PRR 13114- Improve Screen/PF Key Function! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        dim                                                              ~
            abcclass$1,                  /* ABC Class HNYCCMST/HNYMASTR*/~
            abcd$(6%,12%)1,              /* ABCD Class Arrary          */~
            ccgroup$6,                   /* Cycle Count Group Code Name*/~
            changed$1,                   /* Count Period Changed Flag  */~
            cntlastuser$3,               /* Who Last Changed CntPrd    */~
            cntperiod$3,                 /* Count Period (Days)        */~
            cntperiodorig$3,             /* Count Period (Days) Originl*/~
            cntprd$(6%,12%)3,            /* Count Period Arrary        */~
            cntperiodflag$1,             /* Lock Flag - Count Period   */~
            cntperiodold$3,              /* Old Count Period           */~
            cntprdlstdat$10,             /* Date Cnt Prd Last Changed  */~
            cntr$7,                      /* Counter                    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datecnt$10,                  /* Last Count Date            */~
            descr$42,                    /* Descr for PLOWCODE         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            firstcntdate$10,             /* First Count Date           */~
            fmccgroup$6,                 /* Cycle Count Group Range    */~
            fmtrans$(6%,12%)8,           /* FROM Trans Freq Factor     */~
            from$8,                      /* Screen Literals            */~
            header$55,                   /* Screen Header for HNYCCRNG */~
            headers$(3)27,               /* Used for Screen Headers    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lot$16,                      /* Lot Code                   */~
            maxline%(6%),                /*                            */~
            mfac$(12%,4%)1,              /* FACs (Rows, Colums)        */~
            method$1,                    /* Set Count Period Via       */~
            nextcntdate$8,               /* Next Count Date            */~
            nxdate1$8,                   /* Temp Count Date Calculation*/~
            nxdate2$8,                   /* Temp Count Date Calculation*/~
            part$25,                     /* Part Code                  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16$16,                     /* Passed in PF16 Literals    */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            r$(24)80,                    /* Screen Image               */~
            recdate$10,                  /* Record Start Date          */~
            store$3,                     /* Store Code                 */~
            tempdate$8,                  /* Temporary Date             */~
            to$8,                        /* Screen Literals            */~
            toccgroup$6,                 /* Cycle Count Group Range    */~
            totrans$(6%,12%)8,           /* TO Trans Freq Factor       */~
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
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #01 ! HNYCCMST ! Cycle Count Master File                  *~
            * #02 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #03 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #04 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #06 ! HNYDETAL ! Inventroy Details                        *~
            * #07 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #08 ! CATEGORY ! Inventory Category Codes File            *~
            * #09 ! STORNAME ! Store Information File                   *~
            * #50 ! WORKFIL1 ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup


            select #02, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup


            select #03, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #04, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =  13, dup,    ~
                            key  2, keypos =   56, keylen =  10


            select #05, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #06, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup


            select #07, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #08, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select #09, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #50, "WORKFIL1",                                      ~
                        varc,     indexed,  recsize =  44,               ~
                        keypos =    1, keylen =  44                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
                get rslt$(01) using L02802, rec%
L02802:             FMT POS(17), BI(4)
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))
            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            tempdate$ = date$
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYCCPRD: " & str(cms2v$,,8)
            from$ = "From" : to$ = "To"
            headers$(1) = "ABC Class"
            headers$(2) = "Transaction Frequency Range"
            headers$(3) = "Count Period (days)"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  8% then       select_ranges
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        /* *** Set Count Period Selection Method *** */
L10360:     inpmessage$ = "Press PF 10, 11, 12 for Set Period Method."
L10370:     gosub'103(0%   )
               if keyhit%  =  1% then gosub startover
               if keyhit%  = 10% then       set_prd_by_abc
               if keyhit%  = 11% then       set_prd_by_transaction
               if keyhit%  = 12% then       set_prd_by_both
               if keyhit%  = 16% then exit_program
               goto L10370

        input_prd_select
            mat maxline% = zer
            screen2%, delete%, insert% = 0%
            screen2% = 1%  /* The Input Flag */
            for fieldnr% = 1% to 12%
L10460:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10690
L10480:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 20% then goto  previous_screen
                      if keyhit% <>  4% then       L10570
                         if maxline%(s%) > 1% then                       ~
                                       maxline%(s%) = maxline%(s%) -2%
                         fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10480
                         if fieldnr% = 1% then L10460
L10570:               if keyhit% =  6% then L10690
                      if keyhit% <> 16% then L10670
                       errormsg$ = " "
                       if fieldnr% = 1% then exit_program
                       if method% = 3% then L10680
                       for i% = maxline%(s%) + 1% to 12%
                          init(" ") abcd$(s%, i%), cntprd$(s%, i%),      ~
                                    fmtrans$(s%, i%), totrans$(s%, i%)
                       next i%
                       fieldnr% = 13%
L10670:              if keyhit% =  0%  then       L10680
                     if keyhit% <> 16% then       L10480
L10680:              maxline%(s%) = maxline%(s%) + 1%
L10690:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                   if errormsg$ <> " " then L10480
                   if method% = 1% and fieldnr% = 5% then fieldnr% = 13%
            next fieldnr%
            goto editpg2   /* Remain on Screen 2 */

        set_prd_by_abc
            method%, s% = 1%
            goto input_prd_select

        set_prd_by_transaction
            method%, s% = 2%
            goto input_prd_select

        set_prd_by_both
            method%, s%  = 3%
            keyhit% = 0%
            goto input_prd_select

        previous_screen
            if edit% <> 1% then L10990
                ts% = max( 3%, ts% - 1%)
                for i% = 1% to 12%
                  fmtrans$(s%, i%), totrans$(s%, i%), cntprd$(s%, i%),   ~
                  abcd$(s%, i%) = " "
                next i%
L10990:     s% = max( 3%, s% - 1%)
            goto editpg2

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            s% = method%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  =  8% then       select_ranges
                  if keyhit%  = 16% and s% > 0% then data_save
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       select_ranges
                  if keyhit% <>  0% then       L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        editpg2
            inpmessage$ = "To Modify a Line Item, Position Cursor and" & ~
                          " press RETURN."
            lastfieldnr%, delete%, insert% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1%  then gosub startover
                  if keyhit%  =  2%  then  editpg1
                  if keyhit%  = 20%  then  previous_screen
                  if keyhit%  <> 5%  then   L11370
                     if method% <> 3% then editpg1
                        if s% = 6% then  editpg1
                        s% = s% + 1%
                        if ts% >= 6% then  editpg2
                        if s% <= ts% then  editpg2

                        ts% = s%
                        goto input_prd_select
L11370:           if keyhit%  = 11% then       insertlines
                  if keyhit%  = 12% then       deletelines
                  if keyhit% <> 16% then       L11400  else data_save
L11400:           if keyhit% <>  0% then       editpg2
L11410:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% > 5% and method% = 1% then editpg2
            if fieldnr% < 1%                  then editpg2
            if fieldnr% = 1%                  then L11440
            if fieldnr% > maxline%(s%) and method% <> 1% then editpg2
L11440:     if fieldnr% = lastfieldnr%    then editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11470:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11470
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11470
                  lastfieldnr% = fieldnr%
            goto L11410

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        data_save
            cntr% = 0%
            call "SHOSTAT" ("Processing .......")
            if range% =0% then process_via_group else process_via_ranges


        select_ranges
            range% =1%
            call "SHOSTAT" ("Opening Work Files, One Moment Please")
            call "FILEBGON" (#50)
            call "WORKOPEN" (#50, "IO", rec%/2%, f2%(50))
            cntr%, count% = 0% : pf16$ = "(16)Proceed    "
            header$ = "Set Cycle Count Period Via Range Selection   "
            call "HNYCCRNG" ("Y", 2%, #05, #07, #08, #09, #01, #50,      ~
                             count%, header$, pf16$, r$())
            if count% = 0% then inputmode
            s% = 1%
*          INIT(" ") ABCD$(), CNTPRD$(), FMTRANS$(), TOTRANS$()
            goto L10360 /* Get Back to INPUTMODE of Screen 2 */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* Cycle Count Group      */
            return

L20100: REM Def/Enable Cycle Count Group Range   FMCCGROUP$/TOCCGROUP$
            if fmccgroup$ = " " then fmccgroup$ = "ALL"
            inpmessage$ = "Enter Cycle Count Group Range"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            errormsg$ = " "
            enabled% = 1%
            if screen2% = 0% then return

            on method%  gosub L21140,         /* Set By ABC Class       */~
                              L21180,         /* Set By Trans Freq Fctr */~
                              L21230          /* Set By Combo of Both   */
            return


L21140: REM Def/Enable Calculate By ABCD Class
            inpmessage$ = "Enter ABCD Class And Count Period."
            return

L21180: REM Def/Enable Calculate By Trans. Freq.
            inpmessage$ = "Enter Transaction Frequency Range And Count " ~
                        & "Period."
            goto L21300

L21230: REM Def/Enable Calculate By ABCD Class And Trans Freq

            if s% = 3% then abcd$(s%,fieldnr%) = "A"
            if s% = 4% then abcd$(s%,fieldnr%) = "B"
            if s% = 5% then abcd$(s%,fieldnr%) = "C"
            if s% = 6% then abcd$(s%,fieldnr%) = "D"
            inpmessage$ = "Enter Transaction Frequency Range And Count " ~
                        & "Period For '" & abcd$(s%,fieldnr%) & "' Class"

L21300:     if fieldnr% = 1% then                                        ~
              convert 0 to fmtrans$(s%,fieldnr%), pic(########)
            if fieldnr% <= 1% or edit% = 2% then return
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, abcd$(),                   ~
                      cntprd$(), fmtrans$(), totrans$(),                 ~
                      fmccgroup$, toccgroup$, method$

            mat maxline% = zer
            range% = 0% : method$ = "1"  :  totrans = 0.0  : s% = 1%
            /* If RANGE% = 0% -- Calculate via Cycle Count Group Code */
            /*    RANGE% = 1% -- Calculate via Select Ranges  PF8     */
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
            *              L I N E   I T E M  S C R E E N S             *~
            *-----------------------------------------------------------*~
            * Routines for INSERTING Line Items.                        *~
            *************************************************************
        insertlines
            if method% = 1% and maxline%(s%) = 5% then editpg2
            if maxline%(s%) = 12% then editpg2
            fieldnr% = cursor%(1) - 6%
            if fieldnr%  > maxline%(s%) then editpg2
            maxline%(s%) = maxline%(s%) + 1%
            for i% = maxline%(s%) to fieldnr% + 1%   step -1
              if method% = 2% then L29800
              abcd$(s%,i%) = abcd$(s%,i% -1%)
              if method% = 1% then L29845
L29800:       if i% = 1% then L29850
              totrans$(s%,i%)  = totrans$(s%,i% - 1%)
              fmtrans$(s%,i%)  = fmtrans$(s%,i% - 1%)
L29845:       cntprd$( s%,i%)  = cntprd$( s%,i% - 1%)
L29850:     next i%
            if method% = 1% then L29871
            convert totrans$(s%, fieldnr% + 1%) to fmtrans
            convert fmtrans - 1 to fmtrans$(s%, fieldnr% + 1%),          ~
                                                         pic(########)
L29871:     totrans$(s%, fieldnr%), cntprd$(s%, fieldnr%),               ~
                                              abcd$(s%, fieldnr%) = " "

            insert% = 1%
            gosub'052(fieldnr%)
L29910:     gosub'102(fieldnr%,1%)
               if keyhit%  =  1% then gosub startover
               if keyhit% <> 16% then L29950
                  errormsg$ = " "
                  goto L30190  /* GOTO EDITPG2 via Delete to reset Array */
L29950:        if keyhit% <> 0% then L29910
               gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                  if errormsg$ = " " then editpg2
                     goto L29910
               goto editpg2

        deletelines
            fieldnr% = cursor%(1) - 6%
            if fieldnr% <  1% or  fieldnr% > 12% then editpg2
            if fieldnr% >  5% and method%   = 1% then editpg2
            blank1%, blank2%, blank3% = 0%
            if abcd$(s%,fieldnr%)    = " " then blank1% = 1%
            if fmtrans$(s%,fieldnr%) = " " then blank2% = 1%
            if cntprd$(s%,fieldnr%)  = " " then blank3% = 1%
            inpmessage$ = "Press RETURN to Confirm the Deletion or PF16 "~
                        & "to Return to Edit Mode"
            delete% = 1%
            gosub'102(fieldnr%, 1%)
                if keyhit% <> 0% then editpg2

            if method% = 1% and blank1% = 1% and blank3% = 1% then editpg2
            if method% = 2% and blank2% = 1% and blank3% = 1% then editpg2
            if method% = 3% and blank1% = 1% and blank2% = 1% and        ~
                                blank3% = 1% then editpg2

L30190:     for i% = fieldnr% to maxline%(s%) - 1%
              if method% = 2% then L30230
              abcd$(s%,i%) = abcd$(s%,i% + 1%)
                  if method% = 1% then L30270
L30230:       totrans$(s%,i%) = totrans$(s%,i% + 1%)
                  if i% = fieldnr% then L30270
              convert totrans$(s%, i% - 1%) to fmtrans
              convert fmtrans + 1 to fmtrans$(s%, i%), pic(########)
L30270:       cntprd$(s%,i%)  = cntprd$(s%,i% + 1%)
            next i%
            init(" ") abcd$(s%,maxline%(s%)),                            ~
                      cntprd$(s%,maxline%(s%)),                          ~
                      fmtrans$(s%,maxline%(s%)),                         ~
                      totrans$(s%,maxline%(s%))
            maxline%(s%) = maxline%(s%) - 1%
            delete% = 0%
            if method% <> 1% and totrans$(s%,maxline%(s%)) <> "99999999" ~
                             and maxline%(s%) > 0%                       ~
               then totrans$(s%,maxline%(s%)) = "99999999"
            goto editpg2

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        process_via_group
            init(" ") plowkey$ : range% = 0%
            cntr% = 0%
            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                         fmccgroup$ = "ALL" then L31230
            str(plowkey$,1,6) = str(fmccgroup$) addc all(hex(ff))

L31230:     call "PLOWAL1" (#01, plowkey$, 1%, 0%, f1%(01))/* HNYCCMST */
L31250:         if f1%(01) = 0% then display_results

            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                    fmccgroup$ = "ALL" then L31350
            if key(#01,1) > toccgroup$ then display_results
L31350:     gosub set_period
            call "READNXT1" (#01, f1%(1%))
            goto L31250

        /* Process Via Range Selection */
        process_via_ranges
            plowkey$ = hex(00)
L31530:     call "PLOWNEXT" (#50, plowkey$, 0%, f1%(50))
                if f1%(50) = 1% then L31580
                   close #50  /* WorkFiles */
                   goto display_results

L31580:  /* Read HNYCCMST File and Updat it with the new Count Period */
            call "READ101" (#01, str(plowkey$,1,44), f1%(01))
                if f1%(01) = 0% then L31530 /* F1%(01) better be 1% */
            gosub set_period
            goto L31530

        set_period
            gosub get_part_data
            gosub test_set_period
            if changed$ = "N" then return
            gosub check_changed_values
            gosub check_next_date
            gosub write_part_data
            return

        get_part_data
            get #01 using     L37060,                                     ~
            part$,          /* Part Number (Product Code)              */~
            store$,         /* Warehouse or Store                      */~
            lot$,           /* Which lot in inventory - always used wit*/~
            ccgroup$,       /* Cycle Count Group Code Name             */~
            datecnt$,       /* Date Last Counted                       */~
            cntnbr,         /* Number of Counts                        */~
            nextcntdate$,   /* Next Count Date                         */~
            recdate$,       /* Record Start Date                       */~
            abcclass$,      /* ABC Class                               */~
            transfreqfact,  /* Transaction Frequency Factor            */~
            cntperiodflag$, /* Lock Flag - Count Period                */~
            cntperiod$,     /* Count Period in (Days)                  */~
            cntperiodold$,  /* Old Count Period                        */~
            cntlastuser$,   /* User Who Last Changed Count Period      */~
            cntprdlstdat$,  /* Date Count Period Last Changed          */~
            firstcntdate$   /* Date First Count was Made               */

            cntperiodorig$ = cntperiod$

            if abcclass$ <> " " then return
                /* Look to HNYMASTR for the ABC Class */
                call "READ100" (#05, part$, f1%(5%))
                if f1%(5%) = 1% then get #05 using L32315, abcclass$
L32315:              FMT POS(111), CH(1)
                return

        test_set_period
            changed$ = "N"
            if cntperiodflag$ = "Y" and cntperiod$ >  "0" then return
            on method%   goto  abc        ,   /* Set by ABC            */~
                               transaction,   /* Set by Transaction    */~
                               both           /* Set by Both           */

          abc
            for i% = 1% to 5%
                if abcclass$ <> abcd$(1%, i%) then L32480
                cntperiod$ = cntprd$(1%, i%)
                i% = 5%
L32480:     next i%
            goto L32710

          transaction
            for i% = 1% to 12%
                convert totrans$(2%, i%) to totrans  ,data goto L32570
                if transfreqfact > totrans  then L32570
                cntperiod$ = cntprd$(2%, i%)
                i% = 13%
L32570:     next i%
            goto L32710

          both
            for n% = 3% to ts%
              for i% = 1% to 12%
                if abcclass$ <> abcd$(n%, i%) then L32675 /*Next ABC Stuf*/
                    convert totrans$(n%, i%) to totrans , data goto L32680
                    if transfreqfact > totrans  then L32680
                    cntperiod$ = cntprd$(n%, i%)
                    n% = ts% + 1%  /* Lets Escape */
L32675:             i% = 13%
L32680:       next i%
            next n%

L32710:     if cntperiodorig$ <> cntperiod$ then changed$ = "Y"
            return

        write_part_data
            put #01 using    L32920,                                      ~
            cntnbr,         /* Number of Counts                        */~
            nextcntdate$,   /* Next Count Date                         */~
            cntperiod$,     /* Count Period in (Days)                  */~
            cntperiodold$,  /* Old Count Period                        */~
            cntlastuser$,   /* User Who Last Changed Count Period      */~
            cntprdlstdat$,  /* Date Count Period Last Changed          */~
            firstcntdate$   /* Date First Count was Made               */

L32920:     FMT POS(73), PD(14,4), POS(82), CH(6), POS(181), 3*CH(3),    ~
                2*CH(6)

            rewrite #01
            cntr% = cntr% + 1%
            return


        check_changed_values    /* Check for Changed Values */
            if cntperiodorig$ = " "  or  cntperiodorig$ = cntperiod$     ~
                     then return
                cntperiodold$ = cntperiodorig$
                cntprdlstdat$ = tempdate$
                firstcntdate$ = datecnt$
                if  cntnbr <> 0 then  cntnbr = 1
                cntlastuser$  = userid$
                cntnbr% = int(cntnbr)
            return

        check_next_date  /* Calc. the Most Advanced Next Count Date. */
            if cntperiod$ > " "  then L33210
                nextcntdate$ = " "   :   return
L33210:     convert cntperiod$ to cntperiod : cntperiod% = int(cntperiod)
            if recdate$ < firstcntdate$ then L33260
                days% =  int(cntperiod% / 4)
                call "DATE" addr ("G+", tempdate$, days%, nextcntdate$,  ~
                                                                    err%)
                return
L33260:     /* Calc. furthest out NextCountDate */
            days% = (cntnbr%    ) * cntperiod%
            call "DATE" addr ("G+", firstcntdate$ , days% , nxdate1$,err%)
            call "DATE" addr ("G+", datecnt$  , cntperiod%, nxdate2$,err%)
            if nxdate1$ < nxdate2$ then nextcntdate$ = nxdate2$          ~
                                   else nextcntdate$ = nxdate1$
            return


        display_results
            convert cntr% to cntr$, pic(#######)
            keyhit% = 2% /* Window in the Bottom */
            call "ASKUSER" (keyhit%, "*** RESULTS ****",                 ~
               "There are a total of " & hex(80) & cntr$ & " record(s) Mo~
        ~dified", "Press Any key to RETURN", " ")
            goto inputmode


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L37060: FMT                 /* FILE: HNYCCMST                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(3),          /* Warehouse or Store                      */~
            CH(16),         /* Which lot in inventory - always used wit*/~
            CH(6),          /* Cycle Count Group Code Name             */~
            CH(6),          /* Date Last Counted                       */~
            POS(73),        /*                                         */~
            PD(14,4),       /* Number of Counts                        */~
            POS(82),        /*                                         */~
            CH(6),          /* Next Count Date                         */~
            CH(6),          /* Record Start Date                       */~
            POS(127),       /*                                         */~
            CH(1),          /* ABC Class                               */~
            POS(139),       /*                                         */~
            PD(14,4),       /* Transaction Frequency Factor            */~
            POS(180),       /*                                         */~
            CH(1),          /* Lock Flag - Count Period                */~
            CH(3),          /* Count Period in (Days)                  */~
            CH(3),          /* Old Count Period                        */~
            CH(3),          /* User Who Last Changed Count Period      */~
            CH(6),          /* Date Count Period Last Changed          */~
            CH(6)           /* Date First Count was Made               */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40075          /* Cycle Count Group */
              goto L40090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40075:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Set Cycle Count Period",                              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,30), fac(hex(ac)), from$                  , ch(08),~
               at (05,40), fac(hex(ac)), to$                    , ch(08),~
                                                                         ~
               at (06,02), "Cycle Count Group Range",                    ~
               at (06,30), fac(lfac$( 1)), fmccgroup$           , ch(06),~
               at (06,40), fac(lfac$( 1)), toccgroup$           , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40200
                  call "MANUAL" ("HNYCCPRD") : goto L40090

L40200:        if keyhit% <> 15 then L40215
                  call "PRNTSCRN" : goto L40090

L40215:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40310     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(8)Select Ranges       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0f1000)
            return

L40310: if fieldnr% > 0% then L40355  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffff05ffffffffffffff0dff0f1000)
            if fmccgroup$ = "ALL" then return
                str(pf$(3),41,16) = "(8)Select Ranges"
                str(pfkeys$,8,1)  = hex(08)
            return
L40355:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub set_pf2
              if keyhit% <> 12% then L41045 /* Flash, if Deleting */
              init(hex(94)) mfac$(fieldnr%,1%), mfac$(fieldnr%,2%),      ~
                                mfac$(fieldnr%,3%), mfac$(fieldnr%,4%)
              goto L41090
L41045:       if fieldnr% > 0% then init(hex(8c)) mfac$()                ~
                               else init(hex(86)) mfac$()
              on method% gosub L41065,          /* By ABCD Class       */ ~
                               L41079,          /* By Tranaction Freq. */ ~
                               L41079           /* By Both             */
              goto L41090

L41065:       mfac$(fieldnr%, 1%) = hex(81)
              mfac$(fieldnr%, 4%) = hex(82)  : return /* By ABCD       */

L41079:       mfac$(fieldnr%, 1%) = hex(8c)
              mfac$(fieldnr%, 2%) = hex(8c)
              mfac$(fieldnr%, 3%) = hex(82)
              mfac$(fieldnr%, 4%) = hex(82)  : return /* By Both       */

L41090: accept                                                           ~
         at (01,02), "Set Cycle Count Period"                   ,        ~
         at (01,66), "Today:",                                           ~
         at (01,73), fac(hex(8c)), date$                        , ch(08),~
         at (02,02), fac(hex(ac)), line2$                       , ch(79),~
         at (03,02), fac(hex(94)), errormsg$                    , ch(79),~
                                                                         ~
         at (06,06), fac(hex(ac)),      headers$(1%)            , ch(09),~
         at (07,09), fac(mfac$( 1%, 1%)), abcd$(s%, 1%)         , ch(01),~
         at (08,09), fac(mfac$( 2%, 1%)), abcd$(s%, 2%)         , ch(01),~
         at (09,09), fac(mfac$( 3%, 1%)), abcd$(s%, 3%)         , ch(01),~
         at (10,09), fac(mfac$( 4%, 1%)), abcd$(s%, 4%)         , ch(01),~
         at (11,09), fac(mfac$( 5%, 1%)), abcd$(s%, 5%)         , ch(01),~
         at (12,09), fac(mfac$( 6%, 1%)), abcd$(s%, 6%)         , ch(01),~
         at (13,09), fac(mfac$( 7%, 1%)), abcd$(s%, 7%)         , ch(01),~
         at (14,09), fac(mfac$( 8%, 1%)), abcd$(s%, 8%)         , ch(01),~
         at (15,09), fac(mfac$( 9%, 1%)), abcd$(s%, 9%)         , ch(01),~
         at (16,09), fac(mfac$(10%, 1%)), abcd$(s%,10%)         , ch(01),~
         at (17,09), fac(mfac$(11%, 1%)), abcd$(s%,11%)         , ch(01),~
         at (18,09), fac(mfac$(12%, 1%)), abcd$(s%,12%)         , ch(01),~
                                                                         ~
         at (06,19), fac(hex(ac)), headers$(2%)                 , ch(27),~
         at (07,22), fac(mfac$( 1%, 2%)), fmtrans$(s%, 1%)      , ch(08),~
         at (07,31), "-"                                        ,        ~
         at (08,22), fac(mfac$( 2%, 2%)), fmtrans$(s%, 2%)      , ch(08),~
         at (08,31), "-"                                        ,        ~
         at (09,22), fac(mfac$( 3%, 2%)), fmtrans$(s%, 3%)      , ch(08),~
         at (09,31), "-"                                        ,        ~
         at (10,22), fac(mfac$( 4%, 2%)), fmtrans$(s%, 4%)      , ch(08),~
         at (10,31), "-"                                        ,        ~
         at (11,22), fac(mfac$( 5%, 2%)), fmtrans$(s%, 5%)      , ch(08),~
         at (11,31), "-"                                        ,        ~
         at (12,22), fac(mfac$( 6%, 2%)), fmtrans$(s%, 6%)      , ch(08),~
         at (12,31), "-"                                        ,        ~
         at (13,22), fac(mfac$( 7%, 2%)), fmtrans$(s%, 7%)      , ch(08),~
         at (13,31), "-"                                        ,        ~
         at (14,22), fac(mfac$( 8%, 2%)), fmtrans$(s%, 8%)      , ch(08),~
         at (14,31), "-"                                        ,        ~
         at (15,22), fac(mfac$( 9%, 2%)), fmtrans$(s%, 9%)      , ch(08),~
         at (15,31), "-"                                        ,        ~
         at (16,22), fac(mfac$(10%, 2%)), fmtrans$(s%,10%)      , ch(08),~
         at (16,31), "-"                                        ,        ~
         at (17,22), fac(mfac$(11%, 2%)), fmtrans$(s%,11%)      , ch(08),~
         at (17,31), "-"                                        ,        ~
         at (18,22), fac(mfac$(12%, 2%)), fmtrans$(s%,12%)      , ch(08),~
         at (18,31), "-"                                        ,        ~
         at (07,33), fac(mfac$( 1%, 3%)), totrans$(s%, 1%)      , ch(08),~
         at (08,33), fac(mfac$( 2%, 3%)), totrans$(s%, 2%)      , ch(08),~
         at (09,33), fac(mfac$( 3%, 3%)), totrans$(s%, 3%)      , ch(08),~
         at (10,33), fac(mfac$( 4%, 3%)), totrans$(s%, 4%)      , ch(08),~
         at (11,33), fac(mfac$( 5%, 3%)), totrans$(s%, 5%)      , ch(08),~
         at (12,33), fac(mfac$( 6%, 3%)), totrans$(s%, 6%)      , ch(08),~
         at (13,33), fac(mfac$( 7%, 3%)), totrans$(s%, 7%)      , ch(08),~
         at (14,33), fac(mfac$( 8%, 3%)), totrans$(s%, 8%)      , ch(08),~
         at (15,33), fac(mfac$( 9%, 3%)), totrans$(s%, 9%)      , ch(08),~
         at (16,33), fac(mfac$(10%, 3%)), totrans$(s%,10%)      , ch(08),~
         at (17,33), fac(mfac$(11%, 3%)), totrans$(s%,11%)      , ch(08),~
         at (18,33), fac(mfac$(12%, 3%)), totrans$(s%,12%)      , ch(08),~
                                                                         ~
         at (06,51), fac(hex(ac)), headers$(3%)                 , ch(19),~
         at (07,55), fac(mfac$( 1%, 4%)), cntprd$(s%, 1%)       , ch(03),~
         at (08,55), fac(mfac$( 2%, 4%)), cntprd$(s%, 2%)       , ch(03),~
         at (09,55), fac(mfac$( 3%, 4%)), cntprd$(s%, 3%)       , ch(03),~
         at (10,55), fac(mfac$( 4%, 4%)), cntprd$(s%, 4%)       , ch(03),~
         at (11,55), fac(mfac$( 5%, 4%)), cntprd$(s%, 5%)       , ch(03),~
         at (12,55), fac(mfac$( 6%, 4%)), cntprd$(s%, 6%)       , ch(03),~
         at (13,55), fac(mfac$( 7%, 4%)), cntprd$(s%, 7%)       , ch(03),~
         at (14,55), fac(mfac$( 8%, 4%)), cntprd$(s%, 8%)       , ch(03),~
         at (15,55), fac(mfac$( 9%, 4%)), cntprd$(s%, 9%)       , ch(03),~
         at (16,55), fac(mfac$(10%, 4%)), cntprd$(s%,10%)       , ch(03),~
         at (17,55), fac(mfac$(11%, 4%)), cntprd$(s%,11%)       , ch(03),~
         at (18,55), fac(mfac$(12%, 4%)), cntprd$(s%,12%)       , ch(03),~
                                                                         ~
         at (21,02), fac(hex(a4)),   inpmessage$                , ch(79),~
         at (22,02), fac(hex(8c)),   pf$(1)                     , ch(79),~
         at (23,02), fac(hex(8c)),   pf$(2)                     , ch(79),~
         at (24,02), fac(hex(8c)),   pf$(3)                     , ch(79),~
         keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42100
                  call "MANUAL" ("HNYCCPRD") : goto L41090

L42100:        if keyhit% <> 15 then L42130
                  call "PRNTSCRN" : goto L41090

L42130:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42400     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Line       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if s% = 4% or s% = 5% or s% = 6% then L42270 else L42290
L42270:        str(pf$(3%),1%,15%)  = "(20)Prev Screen"
               str(pfkeys$,20%,1%) = hex(14)
L42290:     if insert% = 1% then L42310
            if delete% = 0% then L42340
L42310:        str(pf$(2),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
               str(pf$(3),64%) = "(16)Edit Mode"
               return
L42340:     if fieldnr% > 1% then L42370
               str(pf$(2),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
               goto L42380
L42370:     str(pf$(3),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42380:     return

L42400: if fieldnr% > 0% then L42530  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(11)Insert Line        (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "(12)Delete Line        (16)Process     "
            pfkeys$ = hex(01ffffff05ffffffffff0b0c0dff0f1000)
            if s% = 3% or s% = 4%  or s% = 5%  then L42480
               str(pf$(3),18%,15%) = " "
               str(pfkeys$,05%,1%) = hex(ff)

L42480:        str(pf$(2), 1%,15%) = "(2)First Screen"
               str(pfkeys$,02%,1%) = hex(02)
            if s% = 3% then L42490
               str(pf$(3), 1%,15%)  = "(20)Prev Screen"
               str(pfkeys$,20%,1%) = hex(14)
L42490:     if maxline%(s%) < 12% then L42500
               str(pf$(2),40%,16%) = " "  :  str(pfkeys$,11%,1%) = hex(ff)
L42500:     if range% = 0% then return
               str(pf$(2),18%,18%) = " "  :  str(pfkeys$,4%,1%) = hex(ff)
            return
L42530:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        deffn'103(fieldnr%)
            init(hex(8c))  mfac$()
            gosub set_pf3
            gosub L41090     /*  ACCEPT Screen #2 */
            return

        set_pf3
            pf$(1) = "(1)Start Over     (10)Set Count Period b" &        ~
                     "y ABC                  (13)Instructions"
            pf$(2) = "                  (11)Set Count Period b" &        ~
                     "y Transactions         (15)Print Screen"
            pf$(3) = "                  (12)Set Count Period b" &        ~
                     "y Both                 (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0a0b0c0dff0f1000)
            return
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100          /* Cycle Count Group      */
            return

L50100: REM Test for Cycle Count Group Range      FMCCGROUP$/TOGROUP$
            if (fmccgroup$ = "FIRST" and toccgroup$ = "LAST") or         ~
                fmccgroup$ = "ALL" then return
            if fmccgroup$ = " " and toccgroup$ = " " then to% = 1%       ~
                                                     else to% = 0%
            if toccgroup$ <> " " then to% = 1%
            descr$ = hex(06) & "Select FROM Group Range"
            call "PLOWCODE" (#03, fmccgroup$, descr$, 0%, 0.30, f1%(03))
             if (f1%(03) = 0% or f1%(03) = 1%) and to% = 1% then L50200
             if (f1%(03) = 0% or f1%(03) = 1%) and to% = 0% then L50310
L50200:     descr$ = hex(06) & "Select TO Group Range"
            call "PLOWCODE" (#03, toccgroup$, descr$, 0%, 0.30, f1%(03))
               if f1%(03) = 0% then L50240
                  if toccgroup$ < fmccgroup$ then L50360 else return
L50240:     if fmccgroup$ = " " and toccgroup$ = " " then                ~
                                           fmccgroup$ = "ALL"
            if fmccgroup$ = " " and toccgroup$ <> " " then               ~
                                           fmccgroup$ = toccgroup$
            if fmccgroup$ <> " " and toccgroup$ = " " then               ~
                                           toccgroup$ = fmccgroup$
            return
L50310:     if fmccgroup$ = "?" and toccgroup$ = " " then L50350
            toccgroup$ = fmccgroup$
            return

L50350:     errormsg$ = "Group Code Does Not Exist." : return
L50360:     errormsg$ = "FROM Group Code" & fmccgroup$ &                 ~
                        " may not be greater than TO Group Code" &       ~
                        toccgroup$                   : return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "

            if abcd$(s%,fieldnr%) = " " and fmtrans$(s%,fieldnr%) = " "  ~
                and totrans$(s%,fieldnr%) = " " then L51080

            if cntprd$(s%,fieldnr%) <> " " then L51066
                errormsg$ = "Count Period can not be 'Blank'."
L51063:         maxline%(s%) = maxline%(s%) - 1%
                return
L51066:     call "NUMTEST" (cntprd$(s%,fieldnr%), 1, 999, errormsg$,     ~
                            -0.0, temp) : temp = temp
            if errormsg$ <> " " then L51063  /* Reduce Maxlines & Return */
            call "RJUSTIFY" (cntprd$(s%,fieldnr%))
L51080:     on method% gosub L51140,    /* Set Via ABCD           */      ~
                             L52000,    /* Set Via Trans Freq     */      ~
                             L52000     /* Set Via Both           */

            return

L51140: REM Test for Calculate By ABC Class          ABCD$()
            p% = pos(" ABCD" = abcd$(s%,fieldnr%))
            if p% <> 0% then L51190
               errormsg$ = "Please Enter A, B, C, D, or a Blank."
               return
L51190:     if p% <> 1% then L51230
               cntprd$(s%,fieldnr%) = " "
               if edit% <> 2% then  fieldnr% = 13%
               return
L51230:     gosub check_repeat  /* Can't have same letter on same scrn */
            return

        REM Test for Calculate By Transaction Frequency
            gosub test_trans_freq
            return

L52000: REM Test for Calculate By Both Methods
            gosub test_trans_freq  /* Testing for any Overlapping Range */
            if errormsg$ <> " " then return
            if totrans$(s%,fieldnr%) <> "99999999" then L52082
               if edit% = 2% then return
               fieldnr% = 12%
               return
L52082:       convert totrans$(s%, fieldnr%) to fmtrans, data goto L52090
              fmtrans = fmtrans + 1
              convert fmtrans to fmtrans$(s%,fieldnr% + 1%), pic(########)

L52090:     if edit% <> 2% then return
            if totrans$(s%,maxline%(s%)) = "99999999" then return
            maxline%(s%) = maxline%(s%) + 1%
            abcd$(s%,maxline%(s%)) = abcd$(s%,fieldnr%)
            convert totrans$(s%,fieldnr%) to totrans, data goto L52140
            convert totrans+1 to fmtrans$(s%,maxline%(s%)), pic(########)
            convert 99999999 to totrans$(s%,maxline%(s%)), pic(########)
            if cntprd$(s%,maxline%(s%)) <> " " then return
            if maxline%(s%) > 1% then L52135
                cntprd$(s%,maxline%(s%)) = "  0"
                return
L52135:     cntprd$(s%,maxline%(s%)) = cntprd$(s%,maxline%(s%) - 1%)
L52140:     return

        /* Check to see if there're any repeats when calculate by ABC */
        check_repeat
            mat p = zer : f% = 0%
            if maxline%(s%) <= 1% then return
            search str(abcd$(),1,maxline%(s%)) = abcd$(s%,fieldnr%) to p()
            for i% = 1% to maxline%(s%)
                if p(1%,i%) <> 0% then f% = f% + 1%
                if f% > 1% then L53120
                if i% = maxline%(s%) then f% = 0%
            next i%
            mat p = zer : f% = 0%
            return
L53120:     errormsg$ = "Duplicate ABCD Class OR Blanks are Not Allowed"
            return
            errormsg$ = "ABCD Class Must Be ' ', 'A', 'B', 'C', or 'D'"
            return

        test_trans_freq
            if totrans$(s%,fieldnr%) <> " " then L54012
                  totrans$(s%,fieldnr%) = "99999999"
*                CNTPRD$(S%,FIELDNR%) = "  0"
                  goto L54012
L54012:     call "NUMTEST" (fmtrans$(s%,fieldnr%), 0, 99999999,          ~
                            errormsg$, 0, fmtrans)
            if errormsg$ =  " " then L54020 else L54050
L54020:     call "NUMTEST" (totrans$(s%,fieldnr%), 0, 99999999,          ~
                            errormsg$, 0, totrans)
            if errormsg$ = " " then L54062
L54050:        maxline%(s%) = maxline%(s%) - 1%
               return

L54062:     convert fmtrans to fmtrans$(s%,fieldnr%), pic(########)
            convert totrans to totrans$(s%,fieldnr%), pic(########)

            if maxline%(s%) = 1% then L54320
            for i% = 1% to maxline%(s%)
                if i% = fieldnr% then L54260
                if (fmtrans$(s%,fieldnr%) >=  fmtrans$(s%,i%) and        ~
                    fmtrans$(s%,fieldnr%) <=  totrans$(s%,i%)) or        ~
                   (totrans$(s%,fieldnr%) >=  fmtrans$(s%,i%) and        ~
                    totrans$(s%,fieldnr%) <=  totrans$(s%,i%)) then      ~
                errormsg$ = "Transaction Frequency Range is Over-Lapping"
                if errormsg$ <> " " then i% = maxline%(s%)
L54260:     next i%
L54320:     if fmtrans > totrans then                                    ~
                     errormsg$ = "FROM must be less than or equal to TO"
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
