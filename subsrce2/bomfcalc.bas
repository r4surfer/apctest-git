        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M  FFFFF   CCC    AAA   L       CCC    *~
            *  B   B  O   O  MM MM  F      C   C  A   A  L      C   C   *~
            *  BBBB   O   O  M M M  FFFF   C      AAAAA  L      C       *~
            *  B   B  O   O  M   M  F      C   C  A   A  L      C   C   *~
            *  BBBB    OOO   M   M  F       CCC   A   A  LLLLL   CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMFCALC - BOM Formula Calculator routine.                *~
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
            * 06/15/94 ! Original                                 ! LDJ *~
            * 01/30/95 ! Added test for divide by zero error.     ! LDJ *~
            *          ! Also corrected call to MANUAL.           !     *~
            * 07/11/95 ! Forced formula display for non-stocked   ! JBK *~
            *          !   components in all cases.  Forced load  !     *~
            *          !   of formula field names in all cases.   !     *~
            *          !   In edit mode of UNDEFINED VALUES,      !     *~
            *          !   modified the input message to include  !     *~
            *          !   the name of the variables.             !     *~
            * 06/04/96 ! Init some variables for hung-over formula! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BOMFCALC" (                                                 ~
            parent_part$,                /* BOM Assy Part Number       */~
            component_part$,             /* BOM Comp Part Number       */~
            bom_factors(),               /* N Dimensioned array of     */~
                                         /* which only the 1st element */~
                                         /* is used at present:        */~
                                         /* (1) = BOM BATCH SIZE       */~
            override_formula$,           /* Optional Override Formula  */~
            #1,                          /* SYSFILE2                   */~
            #4,                          /* HNYMASTR                   */~
            results,                     /* Returned Calculation Result*/~
            results1,                    /* Value for BOM Qty Field    */~
            results2,                    /* Value for BOM Times Used   */~
            return_code%)                /*  0% = Okey Dokey           */
                                         /*  1% = User Canceled        */
                                         /*  2% = Formula Error        */
                                         /*  3% = Data Error           */
                                         /*  4% = Form Calculator OFF  */
                                         /*  5% = Divide by Zero error */

        dim                                                              ~
            bom_factors(1),              /* BOM Batch Size in (1)      */~
            col1_hdr$13,                 /* Column 1 Header            */~
            col2_hdr$22,                 /* Column 2 Header            */~
            col3_hdr$2,                  /* Column 3 Header            */~
            component_gvf$(10)20,        /* Parent General Variable Fld*/~
            component_fvf$(10)12,        /* Parent Formula Variable Fld*/~
            component_part$25,           /* BOM Comp Part Number       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            default_formula$16,          /* Default Formula from PARENT*/~
            description$50,              /* Description                */~
            display_edit_flag$1,         /* Display/Edit Allowed?      */~
            save_display_edit_flag$1,    /* Display/Edit Allowed?      */~
            edtmessage$79,               /* Edit screen message        */~
            equal$(50)1,                 /* Expression 'Equals' sign   */~
            error$79,                    /* Work Variable              */~
            errormsg$79,                 /* Error message              */~
            field$22,                    /* Field Value or RegisterName*/~
            field$(100)22,               /* Field Value or RegisterName*/~
            field_save$(100)22,          /* Field/Constant Name        */~
            field_constant$(100)12,      /* Field/Constant Name in file*/~
            filler$256,                  /* Record filler area         */~
            fld$2,                       /* Work Variable              */~
            form_calc_flag$1,            /* Formula Calculations Used? */~
            formula_name$16,             /* Formula Name               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(54)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mat_usage$10,                /* Material Usage Factor %    */~
            msg$(10)40,                  /* Variable Field Descriptions*/~
            ns_comp$1,                   /* Non-stock Part Flags       */~
            op$1,                        /* Operator                   */~
            op$(100)1,                   /* Operator                   */~
            op_save$(100)1,              /* Operator (original)        */~
            override_formula$16,         /* Optional Override Formula  */~
            p%(2),                       /* Search results receiver    */~
            parent_gvf$(10)20,           /* Parent General Variable Fld*/~
            parent_fvf$(10)12,           /* Parent Formula Variable Fld*/~
            parent_part$25,              /* BOM Assy Part Number       */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            precision$2,                 /* Formula Precision          */~
            prompt$(10)20,               /* Variable Field Names       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            reg(50),                     /* Stores Register Values     */~
            register$10,                 /* Register Name              */~
            register$(50)10,             /* Register Name              */~
            register_save$(50)10,        /* Register Names saved       */~
            table$1,                     /* Variable Table ID          */~
            temp(100),                   /* Stores in-process values   */~
            type$(10)2,                  /* Variable Field Types       */~
            switchkey$20,                /* Key for SYSFILE2           */~
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
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
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
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "BOMFRMLA",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  16

            select #03, "WORKAREA",                                      ~
                        varc,     indexed,  recsize =   77,              ~
                        keypos =    1, keylen = 37,                      ~
                        alternate key 1, keypos =  35, keylen = 3, dup

            call "OPENCHCK" (#02, fs%(02), f2%(02), 1%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            if userid$ > " " then been_here_b4% = 1%
            on been_here_b4% goto L09230
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            select degrees
            line2$ = component_part$
            str(line2$,62%) = "BOMFCALC: " & str(cms2v$,,8%)

            col1_hdr$ = "Register Name"
            col2_hdr$ = "Variable Value"
            col3_hdr$ = "OP"

            switchkey$ = "SWITCHS.BOM"
            call "READ100" (#1, switchkey$, f1%(1%))
            form_calc_flag$ = "N"
             if f1%(1%) = 1% then get #1 using L09210, form_calc_flag$,    ~
                display_edit_flag$, mat_usage$
            mat_usage = 0
            convert mat_usage$ to mat_usage, data goto L09205
            if mat_usage <> 0 then mat_usage = mat_usage / 100
L09205:     call "CONVERT" (mat_usage, -0.8, mat_usage$)
L09210:     FMT POS(24), CH(01), CH(01), CH(10)
            if form_calc_flag$ = " " then form_calc_flag$ = "N"
L09230:     if form_calc_flag$ = "Y" then L09300

            call "ASKUSER" (u3%, "***FORMULA CALCULATION NOT ALLOWED***",~
            "Formula Calculation Function has been inhibited by the BOM",~
            "Module Administrator.  Entry into this function is Denied!",~
            "Press RETURN/ENTER to Acknowledge this message and EXIT.")
            goto formula_off_exit

L09300:
            readkey$ = component_part$
            ns_comp$ = "N"  :  save_display_edit_flag$ = " "
            call "READ100" (#4, readkey$, f1%(4%))
            if f1%(4%) <> 0% then L09310
                save_display_edit_flag$ = display_edit_flag$
                display_edit_flag$ = "Y"
                ns_comp$ = "Y"
L09310:     if display_edit_flag$ = "Y" then                             ~
              edtmessage$= "To Modify Displayed Values, Position Cursor"&~
                           " to Line & Press ENTER/RETURN."              ~
            else                                                         ~
              edtmessage$= "These are the values that will be used in " &~
                           "Calculation.  Press PF16 to Calculate."

*          IF DISPLAY_EDIT_FLAG$ = "N" THEN BEGIN_PROCESSING
            on been_here_b4% goto begin_processing

            call "WORKOPEN" (#3, "IO", 30%, f2%(3%))
        REM *** Now retrieve Variable Field Definitions from SYSFILE2 ***
            switchkey$ = "VF1:FORMULA"
            table$ = "0"
            gosub load_and_dump
            switchkey$ = "VF1:HNYMASTR"
            table$ = "1"
            gosub load_and_dump
            goto begin_processing

        load_and_dump
            call "READ100" (#1, switchkey$, f1%(1%))
            if f1%(1%) = 0% then return
            get #1 using L09680, type$(), prompt$()
L09680:     FMT POS(52), 10*CH(2), POS(82), 10*CH(20)
            str(switchkey$,3%,1%) = "2"
            call "READ100" (#1, switchkey$, f1%(1%))
            if f1%(1%) = 1% then get #1 using L09720, msg$()
L09720:     FMT POS(21),10*CH(40)

            for x% = 1% to 10%
                convert x% to fld$, pic(00)
                if (str(type$(x%),,1%)>="0" and str(type$(x%),,1%)<="9") ~
                     or str(type$(x%),,2%) = "N+"                        ~
                     or str(type$(x%),,2%) = "N-" then L09800
                goto L09860
L09800:         tran (str(prompt$(x%),,len(prompt$(x%))),                ~
        "- AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz")replacing
                write #3, using L09880, str(prompt$(x%)), "Parent Part",   ~
                                                 table$, fld$, msg$(x%)
                write #3, using L09880, str(prompt$(x%)), "Component Part",~
                                                 table$, fld$, msg$(x%)
L09860:     next x%
            return
L09880:     FMT CH(20), CH(14), CH(1), CH(2), CH(40)

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *-----------------------------------------------------------*~
            * Main Processing Begins.                                   *~
            *************************************************************
        begin_processing
            gosub initialize_variables
*          retrieve formula variables from COMPONENT
            readkey$ = component_part$
            call "READ100" (#4, readkey$, f1%(4%))
            if f1%(4%) = 0% then L10130
            get #4 using L10120, component_gvf$(), component_fvf$()
L10120:     FMT POS(506), 10*CH(20), POS(738), 10*CH(12)
L10130
*          retrieve formula variables from PARENT
            readkey$ = parent_part$
            call "READ100" (#4, readkey$, f1%(4%))
            if f1%(4%) = 0% then L10200
            get #4 using L10120, parent_gvf$(), parent_fvf$()
            get #4 using L10190, default_formula$
L10190:     FMT POS(858), CH(16)
L10200
*          retrieve formula definition
            formula_name$ = override_formula$
            if formula_name$ = " " then formula_name$ = default_formula$
            description$=hex(06) & "Select Formula to Use for Calculation"
            call "PLOWCODE" (#2, formula_name$,description$,0%,.5,f1%(2%))
            if f1%(2%) = 1% then gosub dataload                          ~
                            else goto user_cancel_exit
*          Display (& maybe edit) values or go straight to calculation
            if display_edit_flag$ = "N" then calculate_formula

            mode% = 2%                   /* Starting Display Mode      */

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, mode%)        /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then  o% = 0% /* Offset to 1st  */
                  if keyhit%  =  3% then  o%=max(0%, max%-11%)
                  if keyhit%  =  4% then  o%=max(0%, o%-8%)
                  if keyhit%  =  5% then  o%=min(max(0%,max%-11%), o%+8%)
                  if keyhit%  =  6% then  o%=max(0%, o%-1%)
                  if keyhit%  =  7% then  o%=min(max(0%,max%-11%), o%+1%)
                  if keyhit%  =  8% then gosub flip_display
                  if keyhit%  = 16% then       calculate_formula
                  if keyhit%  = 32% then       user_cancel_exit
                  if keyhit% <>  0% then       editpg1
            if mode% = 3% then editpg1
            if errormsg$ = " " and display_edit_flag$ = "D" then editpg1
L11250:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 4% then editpg1
            fieldnr% = cursor%(1%) - 5% + o%
            if cursor%(1%) > 20% then editpg1
            if fieldnr% < 1% or fieldnr% >  3% + max% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            if fieldnr% > 3% then i% = fieldnr% - 3%
L11330:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11330
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11330
                  lastfieldnr% = fieldnr%
            goto L11250

        flip_display
            mode% = 3% - sgn(mode%-2%)
            for z% = 1% to max%
                register$ = register$(z%)
                register$(z%) = register_save$(z%)
                register_save$(z%) = register$
            next z%
            for z% = 1% to max%*2%
                field$ = field$(z%)
                field$(z%) = field_save$(z%)
                field_save$(z%) = field$
                op$ = op$(z%)
                op$(z%) = op_save$(z%)
                op_save$(z%) = op$
            next z%
            return

        REM *************************************************************~
            *             C A L C U L A T E   F O R M U L A             *~
            *-----------------------------------------------------------*~
            * Calculate Formula Results using current values.           *~
            *************************************************************

        calculate_formula
*          validate equation
            mode% = 2%
            for x% = 1% to max%
                i% = x%
                gosub'151(4%)
                if errormsg$ = " " then L19140
                   if i% > o% + 12% then o% =min(max(0%,max%-11%), i%-8%)
                   x% = max% : fieldnr% = i% + 3%
L19140:     next x%
            if errormsg$ <> " " then editpg1
            if op$(i%*2%) = " " then L19220
               if i% > o% + 10% then o% =min(max(0%,max%-11%), i%-8%)
               fieldnr% = i% + 3%
               errormsg$="Cannot close an Expression with an Operator: " ~
                        & op$(i%*2%)
               goto editpg1
L19220
*          Passed Validation ... Formula Calculation can now begin
            gosub parse_and_calculate
            results, results1 = round(results,precision)
            results2 = 1
            convert results to error$, pic(-##############0.#########)
            decimals% = pos(-str(error$,pos(error$=".")) > "0") - 1%
            if decimals% <= 4% then L19290
            x% = decimals% - 4%
            results1 = results * 10^x%
            results2 = 1 * 10^(-x%)
L19290:     goto exit_program

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            parent_gvf$(), parent_fvf$(), component_gvf$(),              ~
                component_fvf$(), default_formula$ = " "
            init(" ") errormsg$, inpmessage$, equal$(),                  ~
                      description$, field$(), formula_name$, precision$, ~
                      op$(), register$(), field_constant$(),             ~
                      op_save$(), register_save$(), field_save$()
            o%, max%, f1%(2%) = 0%
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
            goto begin_processing

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

            mat register_save$ = register$
            mat op_save$ = op$
            call "NUMTEST" (precision$, -8, 8, error$, 0, precision)

            for x% = 1% to 100%
                if field_constant$(x%) = " " then L30350
                max% = int(x%/2+.5)
*              *** Test to see if if this is a numeric constant ***
                call "NUMTEST" (field_constant$(x%), -9e11, 9e12,        ~
                     error$, .9,tst)
                if error$ <> " " then L30290 /* not a numeric constant */
                   field$(x%), field_save$(x%) = field_constant$(x%)
                   goto L30350
*              *** Determine what kind of field it is ***
L30290:         search "BCPRS" = str(field_constant$(x%),,1%) to p%()
                on p%(1%) gosub unpack_bom_fields,                       ~
                                unpack_com_fields,                       ~
                                unpack_par_fields,                       ~
                                unpack_reg_fields,                       ~
                                unpack_sys_fields
L30350:     next x%
            return

        unpack_bom_fields
*          only 1 BOM field at present - BATCH SIZE - so I stuff it
            call "CONVERT" (bom_factors(1%), 0.9, field$(x%))
            return

        unpack_sys_fields
            if str(field_constant$(x%),3%) = "PI" then                   ~
                field$(x%) = "3.141592653589"
            if str(field_constant$(x%),3%) = "MAT-USAGE%" then           ~
                field$(x%) = mat_usage$
            field_save$(x%) = str(field_constant$(x%),3%)
            field_save$(x%) = field_save$(x%) & ".S"
            return

        unpack_reg_fields
            convert str(field_constant$(x%),3%,2%) to reg%, data goto    ~
                     formula_error_exit
            field$(x%), field_save$(x%) = register$(reg%)
            return

        unpack_com_fields      /* Component Part Variable Fields */
            convert str(field_constant$(x%),3%,2%) to vf%, data goto     ~
                     formula_error_exit
            if str(field_constant$(x%),2%,1%) = "1" then                 ~
               field$(x%) = component_gvf$(vf%)
            if str(field_constant$(x%),2%,1%) = "0" then                 ~
               field$(x%) = component_fvf$(vf%)
            if field$(x%) = " " then field$(x%) = "UNDEFINED VALUE!"
            plowkey$ = str(field_constant$(x%),2%,3%)
            call "REDALT0" (#3, plowkey$, 1%, f1%(3%))
            if f1%(3%) = 0% then return
            field_save$(x%) = str(key(#3,0),,20%)
            field_save$(x%) = field_save$(x%) & ".C"
            return

        unpack_par_fields      /* Parent Part Variable Fields */
            convert str(field_constant$(x%),3%,2%) to vf%, data goto     ~
                     formula_error_exit
            if str(field_constant$(x%),2%,1%) = "1" then                 ~
               field$(x%) = parent_gvf$(vf%)
            if str(field_constant$(x%),2%,1%) = "0" then                 ~
               field$(x%) = parent_fvf$(vf%)
            if field$(x%) = " " then field$(x%) = "UNDEFINED VALUE!"
            plowkey$ = str(field_constant$(x%),2%,3%)
            call "REDALT0" (#3, plowkey$, 1%, f1%(3%))
            if f1%(3%) = 0% then return
            field_save$(x%) = str(key(#3,0),,20%)
            field_save$(x%) = field_save$(x%) & ".P"
            return

        REM *************************************************************~
            *    P A R S E   F O R M U L A   &   C A L C U L A T E      *~
            *-----------------------------------------------------------*~
            * Parsing algorithm (if we can dignify the below with that  *~
            * name).  Builds results for each expression in the formula *~
            * incrementally.                                            *~
            *************************************************************
        parse_and_calculate
            results, results1, results2 = 0
            mat reg = zer : mat temp = zer : r% = 1% : start% = 1%
            for x% = 1% to max%*2%
                if field$(x%) = " " then L31130
                convert field$(x%) to temp(x%), data gosub get_reg_value
                if op$(x%) = " " then gosub evaluate_expression
L31130:     next x%
            return

        evaluate_expression
            end% = x%
            gosub do_exponentiation    /* Must do this first!          */
            gosub do_multiply_divide   /* Must do this second!         */
            gosub do_add_subtract      /* and we do this last          */
            results, reg(r%) = temp(start%)
            r% = int(x%/2 + .5) + 1%
*          temp code - display values as they change
*          FOR Z% = START% TO X%
*          CALL "CONVERT" (TEMP(Z%), .8, FIELD$(Z%))
*          NEXT Z%
*          GOSUB'101(0%, 2%)           /* Display Screen - No Entry   */
*          end temp
            start% = x% + 1%
            if mod(start%,2) = 0 then start% = start% + 1%
            return

        get_reg_value
            if field$(x%) = " " then return
            convert str(field_constant$(x%),3%,2%) to y%, data goto      ~
                formula_data_exit
            temp(x%) = reg(y%)
            return

*          evaluate exponentiation (powers of^)
        do_exponentiation
            if end% = start% then return
            for y% = end%-1% to start% step -1%
                if op$(y%) <> "^" then L31430
                temp(y%) = temp(y%)^temp(y%+1%)
                if y% = end%-1% then L31420
                for j% = y% to x%-2%
                    op$(j%) = op$(j%+1%) : temp(j%+1%) = temp(j%+2%)
                next j%
L31420:         end% = end% - 1%
L31430:     next y%
            return

*          evaluate multiplication and division operations
        do_multiply_divide
            if end% = start% then return
            for y% = end%-1% to start% step -1%
                if op$(y%) <> "*" and op$(y%) <> "/" then L31580
                if op$(y%) = "*" then temp(y%) = temp(y%)*temp(y%+1%)
                if op$(y%) = "/" and temp(y%+1%) = 0 then divide_by_zero
                if op$(y%) = "/" then temp(y%) = temp(y%)/temp(y%+1%)
                if y% = end%-1% then L31570
                for j% = y% to x%-2%
                    op$(j%) = op$(j%+1%) : temp(j%+1%) = temp(j%+2%)
                next j%
L31570:         end% = end% - 1%
L31580:     next y%
            return

*          evaluate addition and subtraction operations
        do_add_subtract
            if end% = start% then return
            for y% = end%-1% to start% step -1%
                if op$(y%) = "+" then temp(y%) = temp(y%)+temp(y%+1%)
                if op$(y%) = "-" then temp(y%) = temp(y%)-temp(y%+1%)
            next y%
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
              if fieldnr% = 0% then inpmessage$ = edtmessage$
              if edit% = 3% then inpmessage$ =                           ~
              "Shown is the formula definition used for this calculation."
              gosub set_pf1
              init(" ")equal$()
              for z% = 1% to max%
                  if register$(z%) > " " then equal$(z%) = "="
              next z%
              if edit% <> 2% then L40130
              if fieldnr% = 0% then L40130
                  inpmessage$ = "Enter Values for: " &                   ~
                                field_save$((o%+i%)*2%-1%)
                  if field_save$((o%+i%)*2%) <> " " then                 ~
                                inpmessage$ = inpmessage$ & " & " &      ~
                                field_save$((o%+i%)*2%)

L40130:       if fieldnr% > 0% or errormsg$ > " " then                   ~
                   init(hex(8c)) lfac$() else init(hex(86)) lfac$()
              if edit% = 3% then L40270
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
                  "BOM Formula Calculation for:      ",                  ~
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
                  call "MANUAL" ("BOMFCALC") : goto L40270

L41350:        if keyhit% <> 15 then L41380
                  call "PRNTSCRN" : goto L40270

L41380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if fieldnr% > 0% then L41800  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over        (8)View Formula    " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)First (4)Prev Scr (6)Prev Line       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Last  (5)Next Scr (7)Next Line       " &        ~
                     "(32)Quit               (16)Calculate   "
            pfkeys$ = hex(0102030405060708ffffffff0dff0f102000)
            if edit% <> 3% then L41710
                str(pf$(1%),30%,7%) = "Values"
                str(pf$(3%),64%)    = " "
                str(pfkeys$,16%,1%) = hex(ff)
L41710:     if o% > 0% then L41750
                str(pf$(2%),,40%) = " "
                str(pfkeys$,2%,1%),str(pfkeys$,4%,1%),str(pfkeys$,6%,1%)=~
                     hex(ff)
L41750:     if o% + 11% < max% then return
                str(pf$(3%),,40%) = " "
                str(pfkeys$,3%,1%),str(pfkeys$,5%,1%),str(pfkeys$,7%,1%)=~
                     hex(ff)
            return
L41800:                              /*  Edit Mode - Enabled    */
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

        REM Test for Register Name                REGISTER$(50)
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

*          IF FIELD$ = " " THEN 51660
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
*          *** Invalid data in field ***
            errormsg$ = "This is either an invalid Numeric value or " &  ~
                        "an undefined Register Name: " & field$
            return

        deffn'201(op$)      /* Validate Expression Operator Entry  */
            search "+-*/^" = op$ to p%()
            if p%(1%) > 0% then return
            errormsg$ = "Operator Value must be +, -, *, /, or ^ : " &   ~
                        op$
            return

        check_display_edit_flag
            if save_display_edit_flag$ = " " then return
            if ns_comp$ = "N" then return
            display_edit_flag$ = save_display_edit_flag$
            save_display_edit_flag$ = " "
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

        user_cancel_exit
            return_code% = 1%
            gosub check_display_edit_flag
            end

        formula_error_exit
            call "ASKUSER" (u3%, "*** ERROR IN FORMULA DEFINITION ***",  ~
            "Formula Calculation is CANCELLED due to an error in the   ",~
            "definition of this Formula!                               ",~
            "Press RETURN/ENTER to Acknowledge this message and EXIT.")
            return_code% = 2%
            gosub check_display_edit_flag
            end

        formula_data_exit
            call "ASKUSER" (u3%, "*** ERROR IN FORMULA DATA ***",        ~
            "Formula Calculation CANCELLED due to an error in the      ",~
            "data provided to Formula: " & formula_name$,                ~
            "Press RETURN/ENTER to Acknowledge this message and EXIT.")
            return_code% = 3%
            gosub check_display_edit_flag
            end

        formula_off_exit
            return_code% = 4%
            gosub check_display_edit_flag
            end

        divide_by_zero        /* divide by zero errors */
            call "ASKUSER" (u3%, "*** ATTEMPT TO DIVIDE BY ZERO ***",    ~
            "Formula Calculation CANCELLED due to an error in the      ",~
            "Formula or the data provided to Formula: " & formula_name$, ~
            "Press RETURN/ENTER to Acknowledge this message and EXIT.")
            return_code% = 5%
            gosub check_display_edit_flag
            end

        exit_program
            return_code% = 0%
            gosub check_display_edit_flag
            end
