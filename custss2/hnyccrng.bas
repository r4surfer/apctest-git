        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   RRRR   N   N   GGG    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  R   R  NN  N  G       *~
            *  HHHHH  N N N   YYY   C      C      RRRR   N N N  G GGG   *~
            *  H   H  N  NN    Y    C   C  C   C  R   R  N  NN  G   G   *~
            *  H   H  N   N    Y     CCC    CCC   R   R  N   N   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCRNG - This Sub-Routine will allow the caller to      *~
            *            select the range of ABC/Part Type/Part Cat/    *~
            *            Store/Part Number and return to the caller     *~
            *            the Keys(Pointers) of Part/Store/Lot.          *~
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
            * 02/10/92 ! Original                                 ! SID *~
            * 01/21/93 ! Use HNYMASTER for File search w/ FILE%=3%! RJH *~
            * 03/03/93 ! Modify Category Range Selection to allow ! RJH *~
            *          !  the 1st Set to test FROM 'b' TO 'b'.    !     *~
            * 05/05/93 ! Use HNYMASTR if ABC Class not in HNYCCMST! JDH *~
            * 05/17/93 ! Improved validation of ranges.           ! JDH *~
            * 02/22/94 ! Change READ100 to PLOWNEXT for UNIX fix. ! RJH *~
            * 05/19/00 ! Change to allow input a range of store   ! CMG *~
            *          !    values, requested by Randy S. (EWD001)!     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

         sub "HNYCCRNG" (flag$, file%, #01, #02, #03, #04, #05, #50,     ~
                         count%, header$, pf16$, r$())

        /*  FLAG$ - 'Y' Display 'ABC' class(s) on input screen           ~
                    'N' Do Not display 'ABC'                             ~
            FILE% - 1% (Sort by HNYMASTR)                                ~
                    2% (Sort by HNYCCMST)                                ~
                    3% (Sort by HNYMASTR, But Set 'STORE' Range to 'ALL')~
            #01 - HNYMASTR  Inventory Master File.                       ~
            #02 - HNYQUAN   Inventroy Store Quantity File.               ~
            #03 - CATEGORY  Inventory Category Descriptions.             ~
            #04 - STORNAME  Store Names and Addresses.                   ~
            #05 - HNYCCMST  Cycle Count Master File.                     ~
            #50 - WORKFILE  Work File contains PART/STORE/LOT.           ~
            COUNT% - Number of Records written to the Work File.         ~
            HEADER$  - Header Literal on Line 1 of the Input Screen.     ~
            PF16$ - Passed Literal for PF16 on the Edit Screen           ~
            R$()  - Contains Edit Screen Image for Report Purposes   */


        dim                                                              ~
            abc$5,                       /* ABC Class(s) to Count      */~
            abc_code$5,                  /* ABC Class(s) to Count      */~
            abcdsply$80,                 /* Literal for Input field 1  */~
            cat$4,                       /* Categories to Count        */~
            cat$(9,2)4,                  /* Categories to Count (Range)*/~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$55,                   /* Screen Header Line 1       */~
            include_blanks$1,            /* Inc. Blank ABC Clase Flag  */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lot$16,                      /* Lot Code                   */~
            p(1,5),                      /*                            */~
            part$25,                     /* Part Number                */~
            part$(3,2)25,                /* Part Number (Range)        */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16$16,                     /* Passed in PF16 Literals    */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            quan_part$25,                /* Part Code from HNYQUAN     */~
            r$(24)80,                    /* Screen Image               */~
            readkey$99,                  /* Misc Read Key              */~
            store$3,                     /* Store/Warehouse            */~
            store$(6,2)3,                /* Store/Warehouse (Range)    */~
            type$3,                      /* Type to Count              */~
            type$(9,2)3,                 /* Type to Count (Range)      */~
            userid$3                     /* Current User Id            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

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
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            str(abcdsply$,,24) = "'ABCD' Class(s) to Count"
            str(abcdsply$,25) = "(Enter one, two, or ALL - e.g. AB, ABCD)"
            if flag$ = "N" then str(abcdsply$) = " "

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,1) = "Cycle Count Selection Parameters"
            str(line2$,62) = "HNYCCRNG: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
                if flag$ = "N" and fieldnr% = 1% then L10250
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
L10210:               if keyhit% = 16% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
L10250:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
L11080:     gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <> 16% then       L11110
                    /* Input Range Image for Printing Reports on Page 0 */
                    tran(r$(), hex(208c2084208620ac))replacing
                    goto datasave
L11110:           if keyhit% <>  0% then       editpg1
L11115:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% = 1% and flag$ = "N" then L11080
REM            if fieldnr% = 2% or fieldnr% = 3% or fieldnr% = 4% then L11080   /* (EWD001) */
            if fieldnr% = 2% or fieldnr% = 3% then L11080
            if fieldnr% = 5% or fieldnr% = 6% or fieldnr% = 7%           ~
               then fieldnr% = 2%
            if fieldnr% = 8% or fieldnr% = 9% or fieldnr% = 10%          ~
               then fieldnr% = 3%
            if fieldnr% = 11% or fieldnr% = 12%                          ~
               then fieldnr% = 4%
            if fieldnr% = 13% or fieldnr% = 14% or fieldnr% = 15%        ~
               then fieldnr% = 5%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11115

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            count% = 0%
            call "SHOSTAT" ("Building Workfile")
            gosub selection_process
            if count% > 0% then exit_program
               k% = 1%
               call "ASKUSER" (k%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no eligible records were found",                ~
                 "using the given selection parameters.",                ~
                 "Press RETURN to change the parameters or exit.")
            goto editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* ABC Class(s) to        */~
                              L20200,         /* Part Type              */~
                              L20300,         /* Part Category          */~
                              L20400,         /* Store/Warehouse        */~
                              L20500          /* Part Number            */
            return
L20100: REM Def/Enable 'ABC Class(s) to Count      ABC$
            if keyhit% = 4% and abc$ = " " then abc$ = " "
            if abc$ <> " " then return else abc$ = " ABCD"

            return

L20200: REM Def/Enable Part Type                   TYPE$(9,2)3
            if str(type$(),1) = " " then type$(1,1) = "ALL"
            return

L20300: REM Def/Enable Part Category               CAT$(9,2)4
            if str(cat$(),1) = " " then cat$(1,1) = "ALL"
            return

L20400: REM Def/Enable Store/Warehouse             STORE$(6,2)3
            if str(store$(),1) = " " then store$(1,1) = "ALL"
REM            if file% = 3% then enabled% = 0%       /* (EWD001)  */
            return

L20500: REM Def/Enable Part Number                 PART$(3,2)25
            if str(part$(),1) = " " then part$(1,1) = "ALL"
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
         "Enter ABCD Class(s) to Count.                                ",~
         "Enter Part Type Ranges (000 to 999).                         ",~
         "Enter Part Category Ranges.                                  ",~
         "Enter Store/Warehouse Ranges.                                ",~
         "Enter Part Code Ranges.                                      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      abc$, part$(), cat$(), type$(), store$()
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
        REM *************************************************************~
            *             S E L E C T I O N   P R O C E S S             *~
            *-----------------------------------------------------------*~
            *  Plows HNYMASTR and HNYQUAN files to construction a       *~
            *  Keys(Pointers) and write it to the WORKFILE.  Each record*~
            *  record is examined to see if it meets the user defined   *~
            *  selection criteria.                                      *~
            *************************************************************

        selection_process
            if part$(1,1) = "FIRST" and part$(1,2) = "LAST" then L31150
            if part$(1,1) <> "ALL" then L31156
L31150:        plowkey$ = " "
               if file% <> 2% then gosub hnymastr_loop /* 1% and 3% */   ~
                              else gosub hnyccmst_loop
               return
L31156:     for k% = 1% to 3%
               if part$(k%,1%) = " " then L31162
               plowkey$ = part$(k%,1%)
               str(plowkey$,,25) = addc all(hex(ff))
               if file% <> 2% then gosub hnymastr_loop /* 1% and 3% */   ~
                              else gosub hnyccmst_loop
L31162:     next k%
            return

        hnymastr_loop
            call "PLOWNEXT" (#01, plowkey$, 0%, f1%(01))  /* HNYMASTR */
              if f1%(01) = 0% then return
            get #01 using L31210, part$, cat$, abc_code$, type$
L31210:      FMT CH(25), POS(90), CH(4), POS(111), CH(1), POS(180), CH(3)

            if (part$(1,1) = "FIRST" and part$(1,2) = "LAST") or         ~
               part$(1,1) = "ALL" then L31261
            in_range$ = "N"
            gosub check_part_range
            if in_range$ = "N" then return

L31261:     if cat$(1,1) = "ALL" then L31305
            in_range$ = "N"
            gosub check_category_range
            if in_range$ = "N" then hnymastr_loop

L31305:     if flag$ = "N" then L31345
            in_range$ = "N"
            gosub check_abc_class_range
            if in_range$ = "N" then hnymastr_loop

L31345:     if type$(1,1) = "ALL" then L31385
            in_range$ = "N"
            gosub check_type_range
            if in_range$ = "N" then hnymastr_loop

L31385: /* Check HNYQUAN record for Part/Store/Lot in range */

            readkey$ = part$
            call "PLOWNEXT" (#02, readkey$, 25%, f1%(02%))/* HNYQUAN */
L31400:        if f1%(02%) = 0% then hnymastr_loop

            get #02 using L31430, quan_part$, store$, lot$
L31430:                  FMT    POS(17), CH(25), CH(3), CH(16)

            if quan_part$ > part$ then hnymastr_loop /* See if in range */

            if store$(1,1) = "ALL" then L31485
            in_range$ = "N"
            gosub check_store_range
            if in_range$ = "N" then L31520

L31485: /* Write Part/Store/Lot to the passed in Workfile */
            put #50 using L31500, part$, store$, lot$
L31500:            FMT CH(25), CH(3), CH(16)
            write #50, eod goto L31520

            count% = count% + 1%

L31520:     call "READNEXT" (#02, f1%(02))  /* Get Next HNYQUAN Record */
            goto L31400

        hnyccmst_loop
            call "PLOWNEXT" (#05, plowkey$, 0%, f1%(05))
                if f1%(05) = 0% then return
            get #05 using L31590, part$, store$, lot$, abc_code$
L31590:       FMT CH(25), CH(3), CH(16), POS(127), CH(1)

            if (part$(1,1) = "FIRST" and part$(1,2) = "LAST") or         ~
                part$(1,1) = "ALL" then L31655
            in_range$ = "N"
            gosub check_part_range
            if in_range$ = "N" then return

L31655:     if store$(1,1) = "ALL" then L31770
            in_range$ = "N"
            gosub check_store_range
            if in_range$ = "N" then hnyccmst_loop

L31770: /* Check see if Catagory/Type entered are in range via HNYMASTR */
            readkey$ = part$
            call "PLOWNEXT" (#01, readkey$,25%, f1%(01%))
                if f1%(01%) = 0% then hnyccmst_loop
            get #01 using L31810, cat$, type$
L31810:         FMT POS(90), CH(4), POS(180), CH(3)
        /* Get ABC from HNYMASTR only if HNYCCMST doesn't have one.     */
            if abc_code$ <> " " then L31822
                get #01 using L31818, abc_code$
L31818:              FMT POS(111), CH(1)

L31822:     if flag$ = "N" then L31828
            in_range$ = "N" : gosub check_abc_class_range
            if in_range$ = "N" then hnyccmst_loop

L31828:     if cat$(1,1) = "ALL" then L31860
            in_range$ = "N"
            gosub check_category_range
            if in_range$ = "N" then hnyccmst_loop

L31860:     if type$(1,1) = "ALL" then L31930
            in_range$ = "N"
            gosub check_type_range
            if in_range$ = "N" then hnyccmst_loop

L31930:     put #50 using L31940, part$, store$, lot$
L31940:         FMT CH(25), CH(3), CH(16)

            write #50, eod goto L32000

            count% = count% + 1%

L32000:     goto hnyccmst_loop

        check_part_range
                if part$ >= part$(k%,1%) and part$ <= part$(k%,2%) then  ~
                   in_range$ = "Y"
            return

        check_category_range
            if cat$ < cat$(1%,1%) or cat$ > cat$(1%,2%) then L33120
                in_range$ = "Y"
                return          /* Meets Range Criteria */
L33120:     for s% = 1% to 9%
                if cat$(s%,1%) = " " and cat$(s%,2%) = " " then L33150
                if cat$ >= cat$(s%,1%) and cat$ <= cat$(s%,2%) then      ~
                   in_range$ = "Y"
L33150:     next s%
            return

        check_abc_class_range
            if include_blanks$ = "N" and abc_code$ = " " then return
            if pos(str(abc$,,len(abc$)) = abc_code$) > 0% then           ~
                   in_range$ = "Y"
            return

        check_type_range
            for s% = 1% to 9%
                if type$(s%,1%) = " " and type$(s%,2%) = " " then L33360
                if type$ >= type$(s%,1%) and type$ <= type$(s%,2%) then  ~
                   in_range$ = "Y"
L33360:     next s%
            return

        check_store_range
            for s% = 1% to 6%
              if store$(s%,1%) = " " and store$(s%,2%) = " " then L33460
              if store$ >= store$(s%,1%) and store$ <= store$(s%,2%)     ~
                 then in_range$ = "Y"
L33460:     next s%
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
              on fieldnr% gosub L40095,         /* ABC Class(s)      */   ~
                                L40095,         /* Part Type         */   ~
                                L40095,         /* Part Category     */   ~
                                L40095,         /* Store/Warehouse   */   ~
                                L40095          /* Part Number       */

              if flag$ = "N" and fieldnr% = 1% then lfac$(1) = hex(9d)

              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02), fac(hex(8c)), header$                 ,ch(55),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), line2$                  ,ch(79),~
               at (03,02), fac(hex(94)), errormsg$               ,ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), str(abcdsply$,,24)      ,       ~
               at (04,30), fac(lfac$( 1)), abc$                  ,ch(05),~
               at (04,36), fac(hex(8c)), str(abcdsply$,25)       ,       ~
                                                                         ~
               at (06,22), "Ranges to Include:"                  ,       ~
                                                                         ~
               at (08,02), "Part Type Range(s)"                  ,       ~
               at (08,25), "from:"                               ,       ~
               at (08,33), fac(lfac$(02)),        type$(1,1)     ,ch(03),~
               at (08,38), "to"                                  ,       ~
                                                                         ~
               at (08,41), fac(lfac$(02)),        type$(1,2)     ,ch(03),~
               at (08,48), fac(lfac$(02)),        type$(2,1)     ,ch(03),~
               at (08,53), "to",                                         ~
                                                                         ~
               at (08,56), fac(lfac$(02)),        type$(2,2)     ,ch(03),~
               at (08,63), fac(lfac$(02)),        type$(3,1)     ,ch(03),~
               at (08,68), "to",                                         ~
                                                                         ~
               at (08,71), fac(lfac$(02)),        type$(3,2)     ,ch(03),~
               at (09,33), fac(lfac$(02)),        type$(4,1)     ,ch(03),~
               at (09,38), "to",                                         ~
                                                                         ~
               at (09,41), fac(lfac$(02)),        type$(4,2)     ,ch(03),~
               at (09,48), fac(lfac$(02)),        type$(5,1)     ,ch(03),~
               at (09,53), "to",                                         ~
                                                                         ~
               at (09,56), fac(lfac$(02)),        type$(5,2)     ,ch(03),~
               at (09,63), fac(lfac$(02)),        type$(6,1)     ,ch(03),~
               at (09,68), "to",                                         ~
                                                                         ~
               at (09,71), fac(lfac$(02)),        type$(6,2)     ,ch(03),~
               at (10,33), fac(lfac$(02)),        type$(7,1)     ,ch(03),~
               at (10,38), "to",                                         ~
                                                                         ~
               at (10,41), fac(lfac$(02)),        type$(7,2)     ,ch(03),~
               at (10,48), fac(lfac$(02)),        type$(8,1)     ,ch(03),~
               at (10,53), "to",                                         ~
                                                                         ~
               at (10,56), fac(lfac$(02)),        type$(8,2)     ,ch(03),~
               at (10,63), fac(lfac$(02)),        type$(9,1)     ,ch(03),~
               at (10,68), "to"                                  ,       ~
                                                                         ~
               at (10,71), fac(lfac$(02)),        type$(9,2)     ,ch(03),~
                                                                         ~
               at (11,02), "Part Category Range(s)"              ,       ~
               at (11,25), "from:"                               ,       ~
               at (11,33), fac(lfac$(03)),        cat$(1,1)      ,ch(04),~
               at (11,38), "to"                                  ,       ~
                                                                         ~
               at (11,41), fac(lfac$(03)),        cat$(1,2)      ,ch(04),~
               at (11,48), fac(lfac$(03)),        cat$(2,1)      ,ch(04),~
               at (11,53), "to"                                  ,       ~
                                                                         ~
               at (11,56), fac(lfac$(03)),        cat$(2,2)      ,ch(04),~
               at (11,63), fac(lfac$(03)),        cat$(3,1)      ,ch(04),~
               at (11,68), "to"                                  ,       ~
                                                                         ~
               at (11,71), fac(lfac$(03)),        cat$(3,2)      ,ch(04),~
               at (12,33), fac(lfac$(03)),        cat$(4,1)      ,ch(04),~
               at (12,38), "to"                                  ,       ~
                                                                         ~
               at (12,41), fac(lfac$(03)),        cat$(4,2)      ,ch(04),~
               at (12,48), fac(lfac$(03)),        cat$(5,1)      ,ch(04),~
               at (12,53), "to"                                  ,       ~
                                                                         ~
               at (12,56), fac(lfac$(03)),        cat$(5,2)      ,ch(04),~
               at (12,63), fac(lfac$(03)),        cat$(6,1)      ,ch(04),~
               at (12,68), "to"                                  ,       ~
                                                                         ~
               at (12,71), fac(lfac$(03)),        cat$(6,2)      ,ch(04),~
               at (13,33), fac(lfac$(03)),        cat$(7,1)      ,ch(04),~
               at (13,38), "to"                                  ,       ~
                                                                         ~
               at (13,41), fac(lfac$(03)),        cat$(7,2)      ,ch(04),~
               at (13,48), fac(lfac$(03)),        cat$(8,1)      ,ch(04),~
               at (13,53), "to",                                         ~
                                                                         ~
               at (13,56), fac(lfac$(03)),        cat$(8,2)      ,ch(04),~
               at (13,63), fac(lfac$(03)),        cat$(9,1)      ,ch(04),~
               at (13,68), "to",                                         ~
                                                                         ~
               at (13,71), fac(lfac$(03)),        cat$(9,2)      ,ch(04),~
                                                                         ~
               at (14,02), "Store Range(s)"                      ,       ~
               at (14,25), "from:"                               ,       ~
               at (14,33), fac(lfac$(04)),        store$(1,1)    ,ch(03),~
               at (14,38), "to",                                         ~
                                                                         ~
               at (14,41), fac(lfac$(04)),        store$(1,2)    ,ch(03),~
               at (14,48), fac(lfac$(04)),        store$(2,1)    ,ch(03),~
               at (14,53), "to",                                         ~
                                                                         ~
               at (14,56), fac(lfac$(04)),        store$(2,2)    ,ch(03),~
               at (14,63), fac(lfac$(04)),        store$(3,1)    ,ch(03),~
               at (14,68), "to",                                         ~
                                                                         ~
               at (14,71), fac(lfac$(04)),        store$(3,2)    ,ch(03),~
               at (15,33), fac(lfac$(04)),        store$(4,1)    ,ch(03),~
               at (15,38), "to"                                  ,       ~
                                                                         ~
               at (15,41), fac(lfac$(04)),        store$(4,2)    ,ch(03),~
               at (15,48), fac(lfac$(04)),        store$(5,1)    ,ch(03),~
               at (15,53), "to"                                  ,       ~
                                                                         ~
               at (15,56), fac(lfac$(04)),        store$(5,2)    ,ch(03),~
               at (15,63), fac(lfac$(04)),        store$(6,1)    ,ch(03),~
               at (15,68), "to"                                  ,       ~
               at (15,71), fac(lfac$(04)),        store$(6,2)    ,ch(03),~
                                                                         ~
               at (16,02), "Part Code Range(s)"                  ,       ~
               at (16,21), "from:"                               ,       ~
               at (16,27), fac(lfac$(05)),        part$(1,1)     ,ch(25),~
               at (16,53), "to"                                  ,       ~
               at (16,56), fac(lfac$(05)),        part$(1,2)     ,ch(25),~
                                                                         ~
               at (17,21), "from:"                               ,       ~
               at (17,27), fac(lfac$(05)),        part$(2,1)     ,ch(25),~
               at (17,53), "to"                                  ,       ~
               at (17,56), fac(lfac$(05)),        part$(2,2)     ,ch(25),~
                                                                         ~
               at (18,21), "from:"                               ,       ~
               at (18,27), fac(lfac$(05)),        part$(3,1)     ,ch(25),~
               at (18,53), "to"                                  ,       ~
               at (18,56), fac(lfac$(05)),        part$(3,2)     ,ch(25),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),          inpmessage$    ,ch(79),~
               at (22,02), fac(hex(8c)),          pf$(1)         ,ch(79),~
               at (23,02), fac(hex(8c)),          pf$(2)         ,ch(79),~
               at (24,02), fac(hex(8c)),          pf$(3)         ,ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41390
                  call "MANUAL" ("HNYCCRNG") : goto L40110

L41390:        if keyhit% <> 15% then L41420
                  call "PRNTSCRN" : goto L40110

L41420:        close ws
               call "SCREEN" addr("C", u3%, "I", r$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L42150     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% and flag$ = "Y" then L42110
              if fieldnr% = 2% and flag$ = "N" then L42110
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42110:     if fieldnr% > 1% then L42121
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42121:     if flag$ = "Y" then return
               if fieldnr% <> 2% then return
                 str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
            return

L42150: if fieldnr% > 0% then L42240  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Process     "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            /* Passed in PF16 Texts */
            if pf16$ <> " " then str(pf$(3),63,16) = pf16$
            return
L42240:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* ABC Class(s)           */~
                              L50210,         /* Part Type              */~
                              L50350,         /* Part Category          */~
                              L50660,         /* Store/Warehouse        */~
                              L50970          /* Part Number            */
            return

L50100: REM Test for 'ABC Class(s) to Count       ABC$
            mat p = zer : f% = 0% : include_blanks$ = "N"
            if abc$ = "ALL" then abc$ = " ABCD"
            if abc$ = " "   then abc$ = " ABCD"

            for i% = 1% to 5%
               if pos(" ABCD" = str(abc$, i%, 1%)) = 0% then L50170
            next i%
            for i% = 1% to 5%
              search abc$ = str(abc$, i%, 1%) to p()
               if str(abc$,i%,1%) = " " then L50146
               for k% = 1% to 5%
                 if p(1%, k%) <> 0% then f% = f% + 1%
                 if f% > 1% then L50161
                 if k% = 5% then f% = 0%
               next k%
L50146:        mat p = zer : f% = 0%
            next i%
            include_blanks$ = "N"
            if str(abc$,1,1) = " " then include_blanks$ = "Y"
            return
L50161:     errormsg$ = "Duplicate ABCD Class OR Blanks are Not Allowed"
            return
L50170:     errormsg$ = "ABCD Classe Must Be ' ', 'A', 'B', 'C', 'D', 'A"~
                      & "LL' or a Combination Thereof"
            return

L50210: REM Test for Part Type (Range)            TYPE$(9,2)3
            if str(type$(),1) = " " then type$(1,1) = "ALL"
            if type$(1,1) <> "ALL" then L50250
                str(type$(),4%) = " " : return

L50250:     for s% = 1% to 9%
                if type$(s%,1%) = " " and type$(s%,2%) = " " then L50280
                convert type$(s%,1%) to ptype%, data goto L50330
                if ptype% < 0% or ptype% > 999% then L50330
                convert ptype% to type$(s%,1%), pic(000)
                convert type$(s%,2%) to ptype%, data goto L50330
                if ptype% < 0% or ptype% > 999% then L50330
                convert ptype% to type$(s%,2%), pic(000)
                if type$(s%,1%) > type$(s%,2%) then L50300
L50280:     next s%

            return

L50300:     errormsg$ = "FROM Part Type " & type$(s%,1%) &               ~
                        " may not be greater than TO Part Type " &       ~
                        type$(s%,2%) : return
L50330:     errormsg$ = "Invalid Part Type: " & type$(s%,1%) & " to " &  ~
                        type$(s%,2%) &                                   ~
                        "; Please enter as Numeric, 000 to 999"
            return

L50350: REM Test for Categories to Count (Range)        CAT$(9,2)4
            if  cat$(1%,1%) <> "ALL" then L50390
                str(cat$(),5%) = "  " : return

L50390:     for s% = 1% to 9%
              if s% = 1% and cat$(1%,1%) = " " and cat$(1%,2%) = " "     ~
                  then L50460     /* Allow 'blank' TO 'blank' on 1st Set */
              call "TESTRNGE" (cat$(s%,1%), cat$(s%,2%), temp1$, temp2$, ~
                               errormsg$, #03)
              if errormsg$ <> " " then return
              if cat$(s%,1%) = "ALL" then cat$(s%,1%) = " "
L50460:     next s%
            return

L50660: REM Test for Store/Warehouses to Count (Range)     STORE$(6,2)3
           if store$(1%,1%) <> "ALL" then L50700
             str(store$(),4%) = "  " : return

L50700:    for s% = 1% to 6%
              call "TESTRNGE" (store$(s%,1%), store$(s%,2%), temp1$,     ~
                               temp2$, errormsg$, #04)
              if errormsg$ <>  " " then return
              if store$(s%,1%) = "ALL" then store$(s%,1%) = " "
           next s%
           return

L50970: REM Test for Parts to Count (Range)             PART$(3,2)25
            if part$(1,1) = "FIRST" and part$(1,2) = "LAST" then return
               if part$(1,1) <> "ALL" then L51020
               str(part$(),26%) = "  " : return

L51020:    for s% = 1% to 3%
              call "TESTRNGE" (part$(s%,1%), part$(s%,2%), temp1$,       ~
                               temp2$, errormsg$, #01)
              if errormsg$ <>  " " then return
              if part$(s%,1%) = "ALL" then part$(s%,1%) = " "
           next s%
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
            end