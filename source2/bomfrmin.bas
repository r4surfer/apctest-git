        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M  FFFFF  RRRR   M   M  IIIII  N   N   *~
            *  B   B  O   O  MM MM  F      R   R  MM MM    I    NN  N   *~
            *  BBBB   O   O  M M M  FFFF   RRRR   M M M    I    N N N   *~
            *  B   B  O   O  M   M  F      R   R  M   M    I    N  NN   *~
            *  BBBB    OOO   M   M  F      R   R  M   M  IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMFRMIN - Means of Maintaining (Add, Change, Delete)     *~
            *            BOM Calculation Formulas.                      *~
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
            * 06/13/94 ! Original                                 ! LDJ *~
            * 01/30/95 ! Corrected Valid Variable Field Types incl! LDJ *~
            *          ! Added call to ALLFREE. Fix ERRORMSG Prob.!     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            col1_hdr$13,                 /* Column 1 Header            */~
            col2_hdr$22,                 /* Column 2 Header            */~
            col3_hdr$2,                  /* Column 3 Header            */~
            columnhdr$(2)80,             /* PLOWCODE Argument          */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(6),                /* PLOWCODE Argument          */~
            description$50,              /* Description                */~
            edtmessage$79,               /* Edit screen message        */~
            equal$(50)1,                 /* Expression 'Equals' sign   */~
            errormsg$79,                 /* Error message              */~
            field$22,                    /* Field/Constant Name        */~
            field$(100)22,               /* Field/Constant Name        */~
            field_constant$(100)12,      /* Field/Constant Name in file*/~
            filler$256,                  /* Record filler area         */~
            fld$2,                       /* Work Variable              */~
            form_calc_flag$1,            /* Formula Calculations Used? */~
            formula_name$16,             /* Formula Name               */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(1),                /* PLOWCODE Arg               */~
            incl_excl$(1)1,              /* PLOWCODE Arg               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(53)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            msg$(10)40,                  /* Variable Field Descriptions*/~
            op$1,                        /* Operator                   */~
            op$(100)1,                   /* Operator                   */~
            p%(2),                       /* Search results receiver    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            precision$2,                 /* Formula Precision          */~
            prompt$(10)20,               /* Variable Field Names       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            register$(50)10,             /* Register Name              */~
            switchkey$20,                /* Key for SYSFILE2           */~
            table$1,                     /* Variable Table ID          */~
            type$(10)2,                  /* Variable Field Types       */~
            userid$3                     /* Current User Id            */~

        dim f2%(04),                     /* = 0 if the file is open    */~
            f1%(04),                     /* = 1 if READ was successful */~
            fs%(04),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(04)20                  /* Text from file opening     */

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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! BOMFRMLA ! Contains BOM Calculator Formula Definiti *~
            * #03 ! WORKAREA ! Temporary Work File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #02, "BOMFRMLA",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  16

            select #03, "WORKAREA",                                      ~
                        varc,     indexed,  recsize =   77,              ~
                        keypos =    1, keylen = 37,                      ~
                        alternate key 1, keypos =  35, keylen = 3, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 1%, rslt$(02))

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

            str(line2$,62) = "BOMFRMIN: " & str(cms2v$,,8)

            col1_hdr$ = "Register Name"
            col2_hdr$ = "Field/Constant"
            col3_hdr$ = "OP"

            switchkey$ = "SWITCHS.BOM"
            call "READ100" (#1, switchkey$, f1%(1%))
            form_calc_flag$ = "N"
             if f1%(1%) = 1% then get #1 using L09210, form_calc_flag$
L09210:     FMT POS(24), CH(01)
            if form_calc_flag$ = " " then form_calc_flag$ = "N"
            if form_calc_flag$ = "Y" then L09310

            call "ASKUSER" (u3%, "***FORMULA CALCULATION NOT ALLOWED***",~
            "Formula Calculation Function has been inhibited by the BOM",~
            "Module Administrator.  Entry into this function is Denied!",~
            "Press RETURN/ENTER to Acknowledge this message and EXIT.")
            goto exit_program

L09310: REM *************************************************************~
            *       Data Table for Building Workfile Constants          *~
            *-----------------------------------------------------------*~
            * Field Name   !Source of Data, VF  !                       *~
            * (20 Bytes)   !Table#, & Field #   !Description (40)       *~
            *--------------+--------------------+-----------------------*
        data  "PI",         "System",  "0  ",  "The value of PI",        ~
              "MAT-USAGE%", "System",  "0  ",  "Material Usage % Factor",~
              "BATCH-SIZE", "BOM","0 ","Bill of Material Batch Size Qty",~
              "ZZ ", " ", " "," "

            call "WORKOPEN" (#3, "IO", 30%, f2%(3%))
L09430:     read str(plowkey$,,20%), str(plowkey$,21%,14%),              ~
                 str(plowkey$,35%,3%), str(plowkey$,38%)
            if str(plowkey$,,2%) = "ZZ" then L09480
            write #3, str(plowkey$,,77%)
            goto L09430
L09480: REM *** Now retrieve Variable Field Definitions from SYSFILE2 ***
            switchkey$ = "VF1:FORMULA"
            table$ = "0"
            gosub load_and_dump
            switchkey$ = "VF1:HNYMASTR"
            table$ = "1"
            gosub load_and_dump
            goto inputmode

        load_and_dump
            call "READ100" (#1, switchkey$, f1%(1%))
            if f1%(1%) = 0% then return
            get #1 using L09610, type$(), prompt$()
L09610:     FMT POS(52), 10*CH(2), POS(82), 10*CH(20)
            str(switchkey$,3%,1%) = "2"
            call "READ100" (#1, switchkey$, f1%(1%))
            if f1%(1%) = 1% then get #1 using L09650, msg$()
L09650:     FMT POS(21),10*CH(40)

            for x% = 1% to 10%
                convert x% to fld$, pic(00)
                if (str(type$(x%),,1%)>="0" and str(type$(x%),,1%)<= "9")~
                     or str(type$(x%),,2%) = "N+"                        ~
                     or str(type$(x%),,2%) = "N-" then L09730
                goto L09790
L09730:         tran (str(prompt$(x%),,len(prompt$(x%))),                ~
        "- AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz")replacing
                write #3, using L09810, str(prompt$(x%)), "Parent Part",   ~
                                                 table$, fld$, msg$(x%)
                write #3, using L09810, str(prompt$(x%)), "Component Part",~
                                                 table$, fld$, msg$(x%)
L09790:     next x%
            return
L09810:     FMT CH(20), CH(14), CH(1), CH(2), CH(40)

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
L10210:               if keyhit% = 16% then       exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

            if max% > 0% then editpg1
            max%, o%, i% = 0%
            gosub insert_line
            o% = 0%
            inpmessage$, errormsg$ = " "

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then  o% = 0% /* Offset to 1st  */
                  if keyhit%  =  3% then  o%=max(0%, max%-11%)
                  if keyhit%  =  4% then  o%=max(0%, o%-8%)
                  if keyhit%  =  5% then  o%=min(max(0%,max%-11%), o%+8%)
                  if keyhit%  =  6% then  o%=max(0%, o%-1%)
                  if keyhit%  =  7% then  o%=min(max(0%,max%-11%), o%+1%)
                  if keyhit%  =  8% then  delete_formula
                  if keyhit% <> 11% then       L11170
                     i% = cursor%(1%) - 8% + o%
                     if i% < 1% or i% > max% then editpg1
                     gosub insert_line
                     goto editpg1
L11170:           if keyhit% <> 12% then       L11220
                     i% = cursor%(1%) - 8% + o%
                     if i% < 1% or i% > max% then editpg1
                     gosub delete_line
                     goto editpg1
L11220:           if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg1
L11250:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% > 3% then fieldnr% = cursor%(1%) - 5% + o%
            if cursor%(1%) > 20% then editpg1
            if fieldnr% < 1% or fieldnr% >  3% + max% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            if fieldnr% > 3% then i% = fieldnr% - 3%
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11330:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub field_lookup
                  if keyhit% <>  0% then L11330
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11330
                  lastfieldnr% = fieldnr%
            goto L11250

        delete_formula
            keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** DELETE FORMULA? ***",          ~
                 "Press ENTER/RETURN to DELETE this Formula",            ~
                               " - or -",                                ~
                 "Press any other Function Key to CANCEL the delete.")
            if keyhit% <> 0% then editpg1
            call "DELETE" (#2, formula_name$, 16%)
            goto inputmode

        insert_line
            if max% >= dim(register$(),1) then errormsg$ =               ~
              "Maximum Formula Size reached!  No more lines may be added!"
            if max% >= dim(register$(),1) then return
            if max% = i% then L12140
            for x% = max% to i%+1 step -1%
                 register$(x%+1%) = register$(x%)
                 equal$   (x%+1%) = equal$   (x%)
                 field$(x%*2%+1%) = field$(x%*2%-1%)
                 field$(x%*2%+2%) = field$(x%*2%)
                 op$(x%*2%+1%)    = op$(x%*2%-1%)
                 op$(x%*2%+2%)    = op$(x%*2%)
            next x%
L12140:     i% = i% + 1%
            max% = max% + 1%
            register$(i%)    = " "
            equal$   (i%)    = " "
            field$(i%*2%-1%) = " "
            field$(i%*2%)    = " "
            op$(i%*2%-1%)    = " "
            op$(i%*2%)       = " "
            if i% > o% + 12% then o% = o% + 8%
L12170:     gosub'101(i% + 3%, 1%)     /* Display / Accept  */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub field_lookup
                  if keyhit% <> 16% then L12230
                      gosub delete_line
                      errormsg$ = " "
                      return
L12230:           if keyhit% <> 0% then       L12170
            gosub'151(i% + 3%)     /* Edit Field for Valid Entry */
                  if errormsg$ <> " " then L12170
            goto insert_line

        delete_line
            for x% = i% to max% + 1%
                 register$(x%)    = register$(x%+1%)
                 equal$   (x%)    = equal$   (x%+1%)
                 field$(x%*2%-1%) = field$(x%*2%+1%)
                 field$(x%*2%)    = field$(x%*2%+2%)
                 op$(x%*2%-1%)    = op$(x%*2%+1%)
                 op$(x%*2%)       = op$(x%*2%+2%)
            next x%
            max% = max% - 1%
            if max% > i% then i% = max%
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
*          validate equation
            call "SHOSTAT" ("Validating Formula")
            for x% = 1% to max%
                i% = x%
                gosub'151(4%)
                if errormsg$ = " " then L19150
                   if i% > o% + 12% then o% =min(max(0%,max%-11%), i%-8%)
                   x% = max% : fieldnr% = i% + 3%
L19150:     next x%
            if errormsg$ <> " " then editpg1
            if op$(i%*2%) = " " then L19230
               if i% > o% + 10% then o% =min(max(0%,max%-11%), i%-8%)
               fieldnr% = i% + 3%
               errormsg$="Cannot close an Expression with an Operator: " ~
                        & op$(i%*2%)
               goto editpg1
L19230:     call "SHOSTAT" ("Passed Validation ... Now Saving Formula...")
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Formula Name           */~
                              L20200,         /* Description            */~
                              L20308          /* Precision              */
            return
L20100: REM Def/Enable Formula Name                FORMULA_NAME$
            if f1%(2%) = 1% then enabled% = 0% /* Already on File */
            return

L20200: REM Def/Enable Description                 DESCRIPTION$
            return

L20308: REM Def/Enable Precision                   PRECISION$
            if precision$ = " " then precision$ = "4"
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
            if scrnr% = 1% then restore line = scrn1_msg, min(4%,fieldnr%)
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Formula Name                                           ",~
         "Enter a Description for this formula                         ",~
        "Enter required precision (decimal places) for the result value",~
         "Enter the next step or expression in the equation or PF16 to en~
        ~d"


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, equal$(),                  ~
                      description$, field$(), formula_name$, precision$, ~
                      op$(), register$(), field_constant$()
            o%, max%, f1%(2%) = 0%
            call "ALLFREE"
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #2, using L35030,/* FILE: BOMFRMLA                      */~
            formula_name$,  /* Formula Name or Identifier              */~
            description$,   /* Description                             */~
            precision$,     /* Formula Precision  - Decimal Places     */~
            register$(),    /* Register Name within a BOM Formula Calcu*/~
            field_constant$(),/* Formula Field ref or Constant value   */~
            op$(),          /* Formula Calculation Operator Symbol     */~
            filler$         /* Unused filler area in record (reserved b*/

            for x% = 1% to 100%
                if field_constant$(x%) = " " then L30290
                max% = int(x%/2+.5)
*              *** Test to see if if this is a numeric constant ***
                call "NUMTEST" (field_constant$(x%), -9e11, 9e12,        ~
                     error$, .9,tst)
                if error$ <> " " then L30230 /* not a numeric constant */
                   field$(x%) = field_constant$(x%)
                   goto L30290
*              *** Determine what kind of field it is ***
L30230:         search "BCPRS" = str(field_constant$(x%),,1%) to p%()
                on p%(1%) gosub unpack_sys_fields,                       ~
                                unpack_var_fields,                       ~
                                unpack_var_fields,                       ~
                                unpack_reg_fields,                       ~
                                unpack_sys_fields
L30290:     next x%
            fieldnr% = 4%
            return

        unpack_sys_fields
            field$(x%) = str(field_constant$(x%),3%)
            field$(x%) = field$(x%) & "." & str(field_constant$(x%),,1%)
            return

        unpack_reg_fields
            convert str(field_constant$(x%),3%,2%) to reg%
            field$(x%) = register$(reg%)
            return

        unpack_var_fields
            readkey$ = str(field_constant$(x%),2%,3%)
            call "REDALT0" (#3, readkey$, 1%, f1%(3%))
            if f1%(3%) = 0% then return
            field$(x%) = str(key(#3,0),,20%)
            field$(x%) = field$(x%) & "." & str(field_constant$(x%),,1%)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            init(" ") field_constant$()
            for x% = 1% to max% * 2%
                if field$(x%) = " " then L31160
*              *** Test to see if if this is a numeric constant ***
                call "NUMTEST" (field$(x%), -9e11, 9e12, error$, .9,tst)
                if error$ <> " " then L31140 /* not a numeric constant */
                   field_constant$(x%) = field$(x%)
                   goto L31160
L31140:         search -field$(x%) = "." to p%()
                on sgn(p%(1%))+1% gosub pack_registers, pack_fields
L31160:     next x%

            call "DELETE" (#2, formula_name$, 16%)

            put #2, using L35030,/* FILE: BOMFRMLA                      */~
            formula_name$,  /* Formula Name or Identifier              */~
            description$,   /* Description                             */~
            precision$,     /* Formula Precision  - Decimal Places     */~
            register$(),    /* Register Name within a BOM Formula Calcu*/~
            field_constant$(),/* Formula Field ref or Constant value   */~
            op$(),          /* Formula Calculation Operator Symbol     */~
            filler$         /* Unused filler area in record (reserved b*/

            write #2
            return

        pack_registers
            field_constant$(x%) = "R0"
            search register$() = str(field$(x%),,10%) to p%() step 10%
            if p%(1%) = 0% then call "SHOSTAT" ("ERROR1 in HANDLE_REGS")
            convert p%(1%)/10% + 1% to str(field_constant$(x%),3%,2%),   ~
                pic(00)
            return
        pack_fields
            str(field_constant$(x%),,1%) = str(field$(x%),p%(1%)+1%,1%)
            if field_constant$(x%) <> "S" and field_constant$(x%) <> "B" ~
                then L31420
               str(field_constant$(x%),2%) = "0" &                       ~
                     str(field$(x%),,p%(1%)-1%)
               return
L31420:     plowkey$ = str(field$(x%),,p%(1%)-1%)
            str(plowkey$,21%,1%) = str(field$(x%),p%(1%)+1%,1%)
            call "PLOWNEXT" (#3, plowkey$, 21%, f1%(3%))
            REM *** IF F1%(3) = 0 THEN Big Bug Somewhere! ***
            str(field_constant$(x%),2%) = str(plowkey$,35%,3%)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: BOMFRMLA                          */~
            CH(16),         /* Formula Name or Identifier              */~
            CH(50),         /* Description                             */~
            CH(02),         /* Formula Precision  - Decimal Places     */~
            50*CH(10),      /* Register Name within a BOM Formula Calcu*/~
            100*CH(12),     /* Formula Field Name or Constant value    */~
            100*CH(01),     /* Formula Calculation Operator Symbol     */~
            CH(156)         /* Unused filler area in record (reserved b*/~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              init(" ")equal$()
              for x% = 1% to max%
                  if register$(x%) > " " then equal$(x%) = "="
              next x%
              if fieldnr%>0% or errormsg$>" " then init(hex(8c)) lfac$() ~
                               else init(hex(86)) lfac$()
              init(hex(8c)) str(lfac$(),max%+4%)
              if fieldnr% = 0% and errormsg$ <> " " then                 ~
                 lfac$(i%+3%) = hex(94)
              if fieldnr% > 3% then gosub L40240 else                     ~
              on fieldnr% gosub L40240,         /* Formula Name      */   ~
                                L40230,         /* Description       */   ~
                                L40250          /* Precision         */
              goto L40270

L40230:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40240:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40250:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "BOM Calculation Formula Definition",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Formula Name:",                              ~
               at (04,30), fac(lfac$( 1)), formula_name$        , ch(16),~
                                                                         ~
               at (05,02), "Description:",                               ~
               at (05,30), fac(lfac$( 2)), description$         ,        ~
                                                                         ~
               at (06,02), "Results Precision:",                         ~
               at (06,30), fac(lfac$( 3)), precision$           ,        ~
                                                                         ~
               at (08,02), fac(hex(ac)),   col1_hdr$            ,        ~
               at (08,18), fac(hex(ac)),   col2_hdr$            ,        ~
               at (08,42), fac(hex(ac)),   col3_hdr$            ,        ~
               at (08,46), fac(hex(ac)),   col2_hdr$            ,        ~
               at (08,70), fac(hex(ac)),   col3_hdr$            ,        ~
                                                                         ~
               at (09,02), fac(lfac$(o%+04%)), register$(o%+01%),        ~
               at (10,02), fac(lfac$(o%+05%)), register$(o%+02%),        ~
               at (11,02), fac(lfac$(o%+06%)), register$(o%+03%),        ~
               at (12,02), fac(lfac$(o%+07%)), register$(o%+04%),        ~
               at (13,02), fac(lfac$(o%+08%)), register$(o%+05%),        ~
               at (14,02), fac(lfac$(o%+09%)), register$(o%+06%),        ~
               at (15,02), fac(lfac$(o%+10%)), register$(o%+07%),        ~
               at (16,02), fac(lfac$(o%+11%)), register$(o%+08%),        ~
               at (17,02), fac(lfac$(o%+12%)), register$(o%+09%),        ~
               at (18,02), fac(lfac$(o%+13%)), register$(o%+10%),        ~
               at (19,02), fac(lfac$(o%+14%)), register$(o%+11%),        ~
               at (20,02), fac(lfac$(o%+15%)), register$(o%+12%),        ~
                                                                         ~
               at (09,16), fac(hex(8c)),          equal$(o%+01%),        ~
               at (10,16), fac(hex(8c)),          equal$(o%+02%),        ~
               at (11,16), fac(hex(8c)),          equal$(o%+03%),        ~
               at (12,16), fac(hex(8c)),          equal$(o%+04%),        ~
               at (13,16), fac(hex(8c)),          equal$(o%+05%),        ~
               at (14,16), fac(hex(8c)),          equal$(o%+06%),        ~
               at (15,16), fac(hex(8c)),          equal$(o%+07%),        ~
               at (16,16), fac(hex(8c)),          equal$(o%+08%),        ~
               at (17,16), fac(hex(8c)),          equal$(o%+09%),        ~
               at (18,16), fac(hex(8c)),          equal$(o%+10%),        ~
               at (19,16), fac(hex(8c)),          equal$(o%+11%),        ~
               at (20,16), fac(hex(8c)),          equal$(o%+12%),        ~
                                                                         ~
               at (09,18), fac(lfac$(o%+04%)), field$((o%+01%)*2%-1%),   ~
               at (10,18), fac(lfac$(o%+05%)), field$((o%+02%)*2%-1%),   ~
               at (11,18), fac(lfac$(o%+06%)), field$((o%+03%)*2%-1%),   ~
               at (12,18), fac(lfac$(o%+07%)), field$((o%+04%)*2%-1%),   ~
               at (13,18), fac(lfac$(o%+08%)), field$((o%+05%)*2%-1%),   ~
               at (14,18), fac(lfac$(o%+09%)), field$((o%+06%)*2%-1%),   ~
               at (15,18), fac(lfac$(o%+10%)), field$((o%+07%)*2%-1%),   ~
               at (16,18), fac(lfac$(o%+11%)), field$((o%+08%)*2%-1%),   ~
               at (17,18), fac(lfac$(o%+12%)), field$((o%+09%)*2%-1%),   ~
               at (18,18), fac(lfac$(o%+13%)), field$((o%+10%)*2%-1%),   ~
               at (19,18), fac(lfac$(o%+14%)), field$((o%+11%)*2%-1%),   ~
               at (20,18), fac(lfac$(o%+15%)), field$((o%+12%)*2%-1%),   ~
                                                                         ~
               at (09,43), fac(lfac$(o%+04%)), op$((o%+01%)*2%-1%),      ~
               at (10,43), fac(lfac$(o%+05%)), op$((o%+02%)*2%-1%),      ~
               at (11,43), fac(lfac$(o%+06%)), op$((o%+03%)*2%-1%),      ~
               at (12,43), fac(lfac$(o%+07%)), op$((o%+04%)*2%-1%),      ~
               at (13,43), fac(lfac$(o%+08%)), op$((o%+05%)*2%-1%),      ~
               at (14,43), fac(lfac$(o%+09%)), op$((o%+06%)*2%-1%),      ~
               at (15,43), fac(lfac$(o%+10%)), op$((o%+07%)*2%-1%),      ~
               at (16,43), fac(lfac$(o%+11%)), op$((o%+08%)*2%-1%),      ~
               at (17,43), fac(lfac$(o%+12%)), op$((o%+09%)*2%-1%),      ~
               at (18,43), fac(lfac$(o%+13%)), op$((o%+10%)*2%-1%),      ~
               at (19,43), fac(lfac$(o%+14%)), op$((o%+11%)*2%-1%),      ~
               at (20,43), fac(lfac$(o%+15%)), op$((o%+12%)*2%-1%),      ~
                                                                         ~
               at (09,46), fac(lfac$(o%+04%)), field$((o%+01%)*2%),      ~
               at (10,46), fac(lfac$(o%+05%)), field$((o%+02%)*2%),      ~
               at (11,46), fac(lfac$(o%+06%)), field$((o%+03%)*2%),      ~
               at (12,46), fac(lfac$(o%+07%)), field$((o%+04%)*2%),      ~
               at (13,46), fac(lfac$(o%+08%)), field$((o%+05%)*2%),      ~
               at (14,46), fac(lfac$(o%+09%)), field$((o%+06%)*2%),      ~
               at (15,46), fac(lfac$(o%+10%)), field$((o%+07%)*2%),      ~
               at (16,46), fac(lfac$(o%+11%)), field$((o%+08%)*2%),      ~
               at (17,46), fac(lfac$(o%+12%)), field$((o%+09%)*2%),      ~
               at (18,46), fac(lfac$(o%+13%)), field$((o%+10%)*2%),      ~
               at (19,46), fac(lfac$(o%+14%)), field$((o%+11%)*2%),      ~
               at (20,46), fac(lfac$(o%+15%)), field$((o%+12%)*2%),      ~
                                                                         ~
               at (09,71), fac(lfac$(o%+04%)), op$((o%+01%)*2%),         ~
               at (10,71), fac(lfac$(o%+05%)), op$((o%+02%)*2%),         ~
               at (11,71), fac(lfac$(o%+06%)), op$((o%+03%)*2%),         ~
               at (12,71), fac(lfac$(o%+07%)), op$((o%+04%)*2%),         ~
               at (13,71), fac(lfac$(o%+08%)), op$((o%+05%)*2%),         ~
               at (14,71), fac(lfac$(o%+09%)), op$((o%+06%)*2%),         ~
               at (15,71), fac(lfac$(o%+10%)), op$((o%+07%)*2%),         ~
               at (16,71), fac(lfac$(o%+11%)), op$((o%+08%)*2%),         ~
               at (17,71), fac(lfac$(o%+12%)), op$((o%+09%)*2%),         ~
               at (18,71), fac(lfac$(o%+13%)), op$((o%+10%)*2%),         ~
               at (19,71), fac(lfac$(o%+14%)), op$((o%+11%)*2%),         ~
               at (20,71), fac(lfac$(o%+15%)), op$((o%+12%)*2%),         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41350
                  call "MANUAL" ("BOMFRMIN") : goto L40270

L41350:        if keyhit% <> 15 then L41380
                  call "PRNTSCRN" : goto L40270

L41380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41590     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% < 4% then L41550
                str(pf$(3),64)    = "(16)Edit Mode"
                str(pf$(2),41)    = "(14)Lookup Field Name"
                str(pfkeys$,14%,1%) = hex(0e)
L41550:     if fieldnr% = 2% then L41570
                str(pf$(2),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41570:     return

L41590: if fieldnr% > 0% then L41760  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over  (32)Exit - No Save       " &        ~
                     " (8)Delete Formula     (13)Instructions"
            pf$(2) = "(2)First (4)Prev Scr (6)Prev Line       " &        ~
                     "(11)Insert Line        (15)Print Screen"
            pf$(3) = "(3)Last  (5)Next Scr (7)Next Line       " &        ~
                     "(12)Delete Line        (16)Save Data   "
            pfkeys$ = hex(0102030405060708ffff0b0c0dff0f102000)
            if o% > 0% then L41710
                str(pf$(2%),,40%) = " "
                str(pfkeys$,2%,1%),str(pfkeys$,4%,1%),str(pfkeys$,6%,1%)=~
                     hex(ff)
L41710:     if o% + 11% < max% then return
                str(pf$(3%),,40%) = " "
                str(pfkeys$,3%,1%),str(pfkeys$,5%,1%),str(pfkeys$,7%,1%)=~
                     hex(ff)
            return
L41760:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(14)Lookup Field Name  (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on min(4%,fieldnr%) gosub L50100, /* Formula Name           */~
                                      L50200, /* Description            */~
                                      L50310, /* Precision              */~
                                      L51000  /* Register Name          */
            return
L50100: REM Test for Formula Name                 FORMULA_NAME$
            if formula_name$ = " " or formula_name$ = "?" then L50145
            call "READ100" (#2, formula_name$, f1%(2%))
            if f1%(2%) = 1% then gosub dataload
            return
L50145:     description$ = hex(06) & "Select Formula to EDIT"
            call "PLOWCODE" (#2, formula_name$,description$,0%,.5,f1%(2%))
            if f1%(2%) = 1% then gosub dataload                          ~
            else errormsg$ = "Formula Name cannot be blank or '?' "
            return

L50200: REM Test for Description                  DESCRIPTION$
            if description$ <> " " then return
            errormsg$ = "Formula Description cannot be blank!"
            return

L50310: REM Test for Precision                    PRECISION$
            call "NUMTEST" (precision$, -8, 8, errormsg$, 0, tst)
            return

L51000: REM Test for Register Name                REGISTER$(50)
            if register$(i%) = " " then L51010
               convert register$(i%) to tst, data goto L51010
                 errormsg$="Register Name CANNOT be a number!"
                 return
L51010:     if i% = 1% then L51240
              if register$(i%) = " " then L51080
                search register$() = str(register$(i%)) to p%() step 10%
                if p%(2%) = 0% then L51080
                  errormsg$ ="You Cannot have Duplicate Register Names: "~
                            & register$(i%)
                  return
L51080:       if op$(i%*2%-2%) = " " then L51240
                if register$(i%) = " " then L51130
                  errormsg$ = "Cannot start New Expression Until " &     ~
                              "previous expression is completed!"
                  return
L51130:         if field$(i%*2%-1%) <> " " then L51190
                  if i% > 1% then                                        ~
                    errormsg$ = "Finish the Expression started above!"   ~
                  else                                                   ~
                    errormsg$ = "Finish this Expression!"
                  return
L51190:         if op$(i%*2%-1%) > " " then L51230
                  if field$(i%*2%) = " " and op$(i%*2%) = " " then L51290
L51210:              errormsg$ = "Unfinished Expression!"
                     return
L51230:           if field$(i%*2%) = " " then L51210 else goto L51290
L51240:     if register$(i%) > " " then L51130
              errormsg$ = "A new Expression requires a new Register Name!"
              return
            return

L51290: REM Test for Field/Constant Name 1        FIELD$(100)
            if field$(i%*2%-1%) = " " then return
            gosub'200(field$(i%*2%-1%))
            if errormsg$ > " " then return
            field$(i%*2%-1%) = field$

        REM Test for Operator 1                   OP$(100)
            if op$(i%*2%-1%) = " " then return
            gosub'201(op$(i%*2%-1%))
            if errormsg$ > " " then return

        REM Test for Field/Constant Name 2        FIELD$(100)
            if field$(i%*2%) = " " then return
            gosub'200(field$(i%*2%))
            if errormsg$ > " " then return
            field$(i%*2%) = field$

        REM Test for Operator 2                   OP$(100)
            if op$(i%*2%) = " " then return
            gosub'201(op$(i%*2%))
            if errormsg$ > " " then return
            return

        deffn'200(field$)      /* Validate Field/Component Entry  */

            if field$ = " " then L51660
*          *** Test to see if if this is a numeric constant ***
            call "NUMTEST" (field$, -9e11, 9e12, error$, .9,tst)
            if error$ = " " then return
*          *** Test to see if this a reference to another expression ***
            if len(field$) > 10% then L51660
            search register$() = str(field$,,10%) to p%() step 10%
            if p%(1%) = 0% then L51660
            if p%(1%)/10% + 1% <> i% then return
            errormsg$ = "An Expression cannot reference itself!: " &     ~
                field$
            return
L51660
*          *** Test to see if if this is a field ***
            plowkey$ = field$
            search -field$ = "." to p%()
            if p%(1%) > 0% then brk% = p%(1%) - 1%                       ~
                           else brk% = len(field$)
            break% = 9000%
            if p%(1%) > 0% then str(plowkey$,p%(1%)) = " "
            if p%(1%) > 0% then str(plowkey$,21%,1%) =                   ~
                str(field$, p%(1%)+1%,1%)
            if keyhit% = 14% then L51840
               call "PLOWNEXT" (#3, plowkey$, brk%, f1%(3%))
               if f1%(3%) = 0% then L51840
               if p%(1%)  = 0% then L51940
               if str(plowkey$,21%,1%)=str(field$,p%(1%)+1%,1%) then L51940
               readkey$ = str(plowkey$,,20%) & str(field$,p%(1%)+1%,1%)
               call "PLOWNEXT" (#3, readkey$, brk%, f1%(3%))
               if f1%(3%) = 0% then L51940
               plowkey$ = readkey$
               goto L51940
L51840:     descr_map(01%) = 01.20  : descr_map(02%) = 001
            descr_map(03%) = 21.14  : descr_map(04%) = 022
            descr_map(05%) = 38.40  : descr_map(06%) = 038
            columnhdr$(1%)="  Field Name           Data Source     Descri~
        ~ption"
            mat incl_excl = zer
            call "PLOWCODE" (#3, plowkey$, msg$(1%), break%, .4, f1%(3%),~
                     columnhdr$(), 0,0, incl_excl(), incl_excl$(), "D",  ~
                     " ", #3, descr_map())
            if f1%(3%) = 0% then L51970
L51940:     field$ = str(plowkey$,,20%)
            str(field$,len(field$)+1%) = "." & str(plowkey$,21%,1%)
            return
L51970:     errormsg$ = "Invalid Field Name: " & field$
            return

        deffn'201(op$)      /* Validate Expression Operator Entry  */
            search "+-*/^" = op$ to p%()
            if p%(1%) > 0% then return
            errormsg$ = "Operator Value must be +, -, *, /, or ^ : " &   ~
                        op$
            return

        field_lookup
            if cursor%(1%) < 8% or cursor%(1%) > 20% then return
            if (cursor%(2%) < 18% or cursor%(2%) > 68%) then return
            if (cursor%(2%) > 40% and cursor%(2%) < 46%) then return
            errormsg$ = " "
            if cursor%(2%) > 45% then L52150
               gosub'200(field$(i%*2%-1%))
               if errormsg$ = " " then field$(i%*2%-1%) = field$
               return
L52150:     gosub'200(field$(i%*2%))
            if errormsg$ = " " then field$(i%*2%) = field$
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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "FILEBGON" (#3)
            call "SHOSTAT" ("One Moment Please")

            end
