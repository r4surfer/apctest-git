        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCCST00                             *~
            *  Creation Date     - 09/11/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - This Utility Program Defines all the *~
            *                      Exceptions and Miscellaneous Costs   *~
            *                      associated with a product.           *~
            *                                                           *~
            *  Subroutines - APCCS1SB - Utility Screen for Selecting    *~
            *                           Specified Codes for a Field     *~
            *                           Definition.                     *~
            *                APCCS2SB - Utiltiy Screen for Selecting    *~
            *                           Equation Types E and F for      *~
            *                           a Costing Exception Definition. *~
            *                APCCS3SB - Utility Screen for Selection    *~
            *                           of Specific Hardware and        *~
            *                           Packaging Definitions from the  *~
            *                           file (APCCSTHP).                *~
            *                                                           *~
            *  Special Comments  - Copy Screen ('102)                   *~
            *                      Report Screen ('103)                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/11/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/12/98 ! y2k checked                              ! DJD *~
            *************************************************************

        dim                                                              ~
            apc_mod$3, apc_mod_d$30,     /* Product/Model Codes        */~
            apc_color$1, apc_color_d$30, /* Product Color Codes        */~
            apc_proc$2, apc_proc_d$30,   /* Exception Process Codes    */~
            apc_field$2, apc_field_d$30, /* Product Field Code         */~
            apc_codes$1, apc_codes_d$30, /* CODES N = All, Y = Yes     */~
            apc_codes$(50%)4,            /* Assoc. Codes for Field No. */~
            apc_eq$1,                    /* Linealmate A, B, C, D's    */~
            apc_eq_d$30,                 /* Linealmate Description     */~
            apc_eq$(25%)3,               /* Selected A, B, C, D's      */~
            apc_hardware$1,              /* Select Hardware            */~
            apc_hardware_d$30,           /* Select Hardware Description*/~
            apc_package$1,               /* Select Packaging           */~
            apc_package_d$30,            /* Select Packaging Descript. */~
            apc_hardware$(25%)15,        /* Hardware Calcs             */~
            apc_package$(25%)15,         /* Packaging Calcs            */~
            apc_mat_cal1$2,              /* Material Calc Process (1)  */~
            apc_mat_cal2$2,              /* Material Calc Process (2)  */~
            apc_mat_cal1_d$30,           /* Material Process Descript 1*/~
            apc_mat_cal2_d$30,           /* Material Process Descript 2*/~
            apc_lab_calc$2,              /* Labor Calc Process Code    */~
            apc_lab_calc_d$30,           /* Labor Process Descript     */~
            apc_mat_val$8,               /* Material Process Value     */~
            apc_lab_val$8,               /* Labor Process Value        */~
            apc_lab_valu$8,              /* Labor UPMH Value           */~
            apc_lab_dept$2,              /* Labor Calc Department Code */~
            apc_lab_dept_d$30,           /* Labor Department Descript  */~
            apc_assign$1,                /* Costing Assignment Area    */~
            apc_assign_d$30,             /* Costing Assigment Descript */~
            apc_scrap$8,                 /* Definition Scrap Percent   */~
            apc_fil$22,                  /* Filler Area                */~
            fr_mod$3, fr_mod_d$30,       /* Mode For Copy Data         */~
            fr_color$1, fr_color_d$30,   /* Color Code for Copy Data   */~
            fr_assign$1, fr_assign_d$30, /* Color Code for Copy Data   */~
            fr_proc$2, fr_proc_d$30,     /* Process Code for Copy Data */~
            fr_field$2, fr_field_d$30,   /* Field Code for Copy Data   */~
            to_mod$3, to_mod_d$30,       /* To Product Code            */~
            to_eq$1, to_eq_d$30,         /* To Copy Linealmate Y/N     */~
            to_hardware$1, to_hardware_d$30, /* Copy Hardware Y/N      */~
            to_package$1, to_package_d$30,   /* Copy Packaging Y/N     */~
            code_key$24, desc$30,        /* Table Lookups              */~
            table$9,                     /* CODE TABLE NAME            */~
            cst_key$9,                   /* Primary Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        dim title$40,                    /* Rpt Tiltle                 */~
            xtime$8,                     /* Rpt Time                   */~
            hdr$14,                      /* Rpt Code Header            */~
            c$(50%)35, sav_key$9,        /* Rpt Print Code Values      */~
            bg_mod$3, ed_mod$3,          /* Rpt Selection Model Codes  */~
            bg_mod_d$30, ed_mod_d$30,    /* Rpt Product Descriptions   */~
            bg_col$1, bg_col_d$30,       /* Rpt Color Selection        */~
            bg_ass$1, bg_ass_d$30        /* Rpt Costing Assignment Sel */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Costing Exception Utility - Def's "
            pname$ = "APCCST00 - Rev: R6.03"

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
            * #01 ! GENCODES ! Master Code Table File                   *~
            * #02 ! APCCUTEQ ! Saw Optimization Cross-Reference         *~
            * #03 ! APCCSTHP ! Costing Hardware Packaging               *~
            * #04 !          !                                          *~
            * #05 ! APCCSTEX ! APC Costing Exception Definition File    *~
            * #06 !          !                                          *~
            * #07 !          !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #2,   "APCCUTEQ",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =    7,                    ~
                        alt key  1, keypos =  1, keylen = 8

            select #3,   "APCCSTHP",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =   20

            select #5,   "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            call "SHOSTAT" ("Opening File, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),500%, rslt$(5%))

            mat f1% = zer

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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 16%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% =  9% then goto inputmode_a
                      if keyhit% = 14% then gosub generate_report
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 14% then gosub dataput
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 16% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *       I N P U T   M O D E   C O P Y   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_a
            gosub initialize_variables

            for fieldnr% = 1% to  9%
L12100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12220
L12120:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12200
L12150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L12120
                         if fieldnr% = 1% then L12100
                         goto L12150
L12200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12120
L12220:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   C O P Y   S C R E E N          *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub copy_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg2
L13130:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% >  9% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13180:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13180
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13180
                  lastfieldnr% = fieldnr%
            goto L13130

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

            generate_report
L19070:        edit% = 0%
               init(" ") bg_mod$, bg_mod_d$, ed_mod$, ed_mod_d$
               init(" ") bg_col$, bg_col_d$, bg_ass$, bg_ass_d$
L19100:        gosub'103(edit%)
               errormsg$ = " "
               if keyhit% = 1% then gosub startover
               if keyhit% = 16% then goto L19620
               if bg_mod$ <> " " then goto L19200
L19150:           bg_mod$   = "ALL"
                  bg_mod_d$ = "(All) - Defined Product/Models"
                  ed_mod$   = bg_mod$
                  ed_mod_d$ = bg_mod_d$
                  goto L19360
L19200:        if str(bg_mod$,1%,1%) = "A" then goto L19150
               apc_mod$ = bg_mod$
               gosub lookup_model
               if errormsg$ <> " " then goto L19070
               bg_mod_d$ = apc_mod_d$
               if ed_mod$ <> " " then goto L19290
                  ed_mod$ = bg_mod$
                  ed_mod_d$ = bg_mod_d$
                  goto L19550
L19290:        apc_mod$ = ed_mod$
               gosub lookup_model
               if errormsg$ <> " " then goto L19070
               ed_mod_d$ = apc_mod_d$
               if bg_mod$ <= ed_mod$ then goto L19360
                  errormsg$ = "(Error) Invalid Product/Model Code?"
                  goto L19070
L19360:        if bg_col$ <> " " then goto L19400
L19370:           bg_col$   = "A"
                  bg_col_d$ = "(All) - Defined Color Codes"
                  goto L19450
L19400:        if bg_col$ = "A" then goto L19370
               apc_color$ = bg_col$
               gosub lookup_color
               if errormsg$ <> " " then goto L19070
               bg_col_d$ = apc_color_d$
L19450:        if bg_ass$ <> " " then goto L19490
L19460:           bg_ass$   = "A"
                  bg_ass_d$ = "(All) - Defined Costing Areas"
                  goto L19550
L19490:        if bg_ass$ = "A" then goto L19460
               apc_assign$ = bg_ass$
               gosub lookup_area
               if errormsg$ <> " " then goto L19070
               bg_ass_d$ = apc_assign_d$

L19550:        edit% = 1%                          /* Passed all Edits */
               if keyhit% = 14% then goto L19590
               goto L19100

L19590:        call "SHOSTAT" ("Creating Costing Exception Report")

               gosub gen_rpt
L19620:     return clear all
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%

         return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Product/Model Code or (000) = All Models?      ",~
         "Enter a Valid Color Code or (0) = All Colors?                ",~
         "Enter a Valid Cost Assignment Area for Definition?           ",~
         "Enter a Valid Costing Process Code or (00) = Standard?       ",~
         "Enter a Valid Product Field Number or (00) = All Fields?     ",~
         "Select Field Codes? N = No, Y = Yes, Selected Codes?         ",~
         "Select Linealmate Equation Types E and F?  N = No or Y = Yes ",~
         "Select Hardware Materials? N = No or Y = Yes                 ",~
         "Select Packaging Materials? N = No or Y = Yes                ",~
         "Enter a Valid Material Calc Process (1) Definition?          ",~
         "Enter a Valid Material Calc Process (2) Definition?          ",~
         "Enter a Valid Material Calc Value or 0.0 when N/A?           ",~
         "Enter a Valid Labor Calc Process for Definition or (00) N/A? ",~
         "Enter a Valid Labor Calc Value and UPMH Value or 0.0 when N/A",~
         "Enter a Valid Labor Department Code or (00) when N/A?        ",~
         "Enter a Valid Scrap Percent or 0.0 when N/A?                 "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28390
                inpmessage$ = edtmessage$
                return

L28390
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid From Product/Model Code?                       ",~
         "Enter a Valid From Color Code or A = All?                    ",~
         "Enter a Valid From Costing Area Code or A = All?             ",~
         "Enter a Valid From Assembly Code or AA = All?                ",~
         "Enter a Valid From Field Code or AA = All?                   ",~
         "Enter a Valid To Product/Model Code?                         ",~
         "Enter a (Y)es or (N)o to Copy Linealmate Data?               ",~
         "Enter a (Y)es or (N)o to Copy Hardware Data?                 ",~
         "Enter a (Y)es or (N)o to Copy Packaging Data?                "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, apc_mod$, apc_mod_d$,      ~
                      apc_color$, apc_color_d$, apc_field$, apc_field_d$,~
                      apc_codes$, apc_codes_d$, apc_codes$(), table$,    ~
                      apc_eq$, apc_eq$(), apc_eq_d$, apc_hardware_d$,    ~
                      apc_proc$, apc_proc_d$, apc_hardware$,             ~
                      apc_hardware$(), apc_package$, apc_package$(),     ~
                      apc_package_d$, apc_mat_cal1$, apc_mat_cal1_d$,    ~
                      apc_lab_calc$, apc_lab_calc_d$, apc_mat_val$,      ~
                      apc_lab_val$, apc_lab_dept$, apc_lab_dept_d$,      ~
                      apc_assign$, apc_assign_d$, apc_scrap$,            ~
                      apc_lab_valu$, fr_mod$, fr_mod_d$, fr_color$,      ~
                      fr_color_d$, fr_proc$, fr_proc_d$, fr_field$,      ~
                      fr_field_d$, to_mod$, to_mod_d$, apc_mat_cal2$,    ~
                      apc_mat_cal2_d$, bg_mod$, bg_mod_d$, ed_mod$,      ~
                      ed_mod_d$, to_eq$, to_eq_d$, to_hardware$,         ~
                      to_hardware_d$, to_package$, to_package_d$,        ~
                      fr_assign$, fr_assign_d$, bg_col$, bg_col_d$,      ~
                      bg_ass$, bg_ass_d$
             rpt% = 0% : rec% = 0% : copy% = 0%
        return

        REM *************************************************************~
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
            rec% = 0%
            if rpt% = 1% or copy% = 1% then goto L30170
               init(" ") cst_key$
               str(cst_key$,1%,3%) = apc_mod$
               str(cst_key$,4%,1%) = apc_color$
               str(cst_key$,5%,1%) = apc_assign$
               str(cst_key$,6%,2%) = apc_proc$
               str(cst_key$,8%,2%) = apc_field$
               read #5,key = cst_key$, eod goto L30570

L30170:     get #5, using L35040, apc_mod$,      /* Prod/Model Code     */~
                                 apc_color$,    /* Prod Color Code     */~
                                 apc_assign$,   /* Area To Assign Def  */~
                                 apc_proc$,     /* Process Code        */~
                                 apc_field$,    /* Field Number Number */~
                                 apc_codes$,    /* N=N/A, Y = Selected */~
                                 apc_codes$(),  /* Selected Codes      */~
                                 apc_eq$,       /* Linealmate E and F  */~
                                 apc_eq$(),     /* Selected Eq's       */~
                                 apc_hardware$, /* Selecte Hardware    */~
                                 apc_hardware$(),/* Selected Hardware  */~
                                 apc_package$,  /* Select Packaging    */~
                                 apc_package$(),/* Selected Packaging  */~
                                 apc_mat_cal1$, /* Material Calc Code 1*/~
                                 apc_mat_cal2$, /* Material Calc Code 2*/~
                                 apc_mat_val$,  /* Material Calc Value */~
                                 apc_lab_calc$, /* Labor Calc Code     */~
                                 apc_lab_val$,  /* Labor Calc Value    */~
                                 apc_lab_valu$, /* Labor UPMH Value    */~
                                 apc_lab_dept$, /* Labor Department    */~
                                 apc_scrap$,    /* Def Scrap % Opt     */~
                                 apc_fil$       /* Filler Area         */

            if copy% = 1% then goto L30560      /* EXIT ROUTINE       */


            gosub L50130                        /* Product/Model Code */
            gosub L50230                        /* Color Code / All   */
            gosub L51485                        /* Definition Area    */
            gosub L50335                        /* Except Process Code*/
            if rpt% = 1% then gosub L50445
                                               /* Product Field No.  */
            gosub L50595                        /* Field Codes        */
            gosub L50670                        /* Select Lineal      */
            gosub L50745                        /* Select Hardware    */
            gosub L50835                        /* Select Packaging   */
            gosub L50925                        /* Material Calc Proc1*/
            gosub L51020                        /* Material Calc Proc2*/
            gosub L51115                        /* Material Calc Value*/
            gosub L51155                        /* Labor Calc Proc    */
            gosub L51295                        /* Labor Calc Value   */
            gosub L51375                        /* Labor Department   */
            gosub L51590                        /* Scrap Percent      */
L30560: rec% = 1%
L30570: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_it
            call "SHOSTAT" ("Deleting Data for ( "& apc_mod$ & " )")

        dataput
            if rec% = 1% then goto L31160
               str(cst_key$,1%,3%) = apc_mod$
               str(cst_key$,4%,1%) = apc_color$
               str(cst_key$,5%,1%) = apc_assign$
               str(cst_key$,6%,2%) = apc_proc$
               str(cst_key$,8%,2%) = apc_field$
L31160:     read #5,hold,key = cst_key$, eod goto L31190
               delete #5
               if keyhit% = 12% then goto L31440
L31190:     put #5, using L35040, apc_mod$,      /* Prod/Model Code     */~
                                 apc_color$,    /* Prod Color Code     */~
                                 apc_assign$,   /* Area To Assign Def  */~
                                 apc_proc$,     /* Process Code        */~
                                 apc_field$,    /* Field Number Number */~
                                 apc_codes$,    /* N=N/A, Y = Selected */~
                                 apc_codes$(),  /* Selected Codes      */~
                                 apc_eq$,       /* Linealmate E and F  */~
                                 apc_eq$(),     /* Selected Eq's       */~
                                 apc_hardware$, /* Selecte Hardware    */~
                                 apc_hardware$(),/* Selected Hardware  */~
                                 apc_package$,  /* Select Packaging    */~
                                 apc_package$(),/* Selected Packaging  */~
                                 apc_mat_cal1$, /* Material Calc Code 1*/~
                                 apc_mat_cal2$, /* Material Calc Code 2*/~
                                 apc_mat_val$,  /* Material Calc Value */~
                                 apc_lab_calc$, /* Labor Calc Code     */~
                                 apc_lab_val$,  /* Labor Calc Value    */~
                                 apc_lab_valu$, /* Labor UPMH Value    */~
                                 apc_lab_dept$, /* Labor Department    */~
                                 apc_scrap$,    /* Def Scrap % Opt     */~
                                 apc_fil$       /* Filler Area         */
            write #5, eod goto L31470

            if copy% = 1% then return
L31440: return clear all
        goto inputmode

L31470:     call "SHOSTAT" ("(Error) Updating "& cst_key$) : stop
        return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040:     FMT CH(3),         /* APC_MOD$= Prod/Model Code   MODEL    */~
                CH(1),         /* APC_COLOR$= Prod Color Code COLOR    */~
                CH(1),         /* APC_ASSIGN$   = ASSIGN DEF  COST AREA*/~
                CH(2),         /* APC_PROC$ = Process Code    COST PROC*/~
                CH(2),         /* APC_FIELD$ = Field Number   APC_FIELD*/~
                CH(1),         /* APC_CODES$ = N = ALL, Y = APCCS1SB   */~
                50*CH(4),      /* APC_CODES$() = Selected Codes        */~
                CH(1),         /* APC_EQ$ = Linealmate A, B, C, D's    */~
                25*CH(3),      /* APC_EQ$() = Selected Eq's E and F    */~
                CH(1),         /* APC_HARDWARE$ = Misc Hardware        */~
                25*CH(15),     /* APC_HARDWARE$() = Selected Equations */~
                CH(1),         /* APC_PACKAGE$ = Misc Packaging        */~
                25*CH(15),     /* APC_PACKAGE$ = Selected Equations    */~
                CH(2),         /* APC_MAT_CAL1$ = Material Calc Proc(1)*/~
                CH(2),         /* APC_MAT_CAL2$ = Material Calc Proc(2)*/~
                CH(8),         /* APC_MAT_VAL$ = Material Calc Value   */~
                CH(2),         /* APC_LAB_CALC$ = Labor Calc Process   */~
                CH(8),         /* APC_LAB_VAL$ = Labor Calc Value      */~
                CH(8),         /* APC_LAB_VALU$ = Labor UPMH Value     */~
                CH(2),         /* APC_LAB_DEPT$ = Labor Department     */~
                CH(8),         /* APC_SCRAP$ = Definition Scrap % Opt  */~
                CH(22)         /* Filler Area                          */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              on fieldnr% gosub L40320,         /* Product/Model Code */  ~
                                L40320,         /* Color Code / All   */  ~
                                L40320,         /* Definition Area    */  ~
                                L40320,         /* Exception Process  */  ~
                                L40320,         /* Product Field No.  */  ~
                                L40320,         /* Field Codes        */  ~
                                L40320,         /* Linealmate E and F */  ~
                                L40320,         /* Hardware           */  ~
                                L40320,         /* Packaging          */  ~
                                L40320,         /* Material Calc Proc1*/  ~
                                L40320,         /* Material Calc Proc2*/  ~
                                L40320,         /* Mater Calc Value   */  ~
                                L40320,         /* Labor Calc Proc    */  ~
                                L40320,         /* Labor Value/UPMH   */  ~
                                L40320,         /* Labor Department   */  ~
                                L40320          /* Scrap Percent      */

              goto L40350

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40320:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40350:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Product/Model Code:",                        ~
               at (03,25), fac(lfac$(1%)), apc_mod$             , ch(03),~
               at (03,40), fac(hex(84)),   apc_mod_d$           , ch(30),~
                                                                         ~
               at (04,02), "Product Color/All :",                        ~
               at (04,25), fac(lfac$(2%)), apc_color$           , ch(01),~
               at (04,40), fac(hex(84)),   apc_color_d$         , ch(30),~
                                                                         ~
               at (05,02), "Cost Assigment    :",                        ~
               at (05,25), fac(lfac$(3%)), apc_assign$          , ch(01),~
               at (05,40), fac(hex(84)),   apc_assign_d$        , ch(30),~
                                                                         ~
               at (06,02), "Process Code      :",                        ~
               at (06,25), fac(lfac$(4%)), apc_proc$            , ch(02),~
               at (06,40), fac(hex(84)),   apc_proc_d$          , ch(30),~
                                                                         ~
               at (07,02), "Field Number/All  :",                        ~
               at (07,25), fac(lfac$(5%)), apc_field$           , ch(02),~
               at (07,40), fac(hex(84)),   apc_field_d$         , ch(30),~
                                                                         ~
               at (08,02), "Select Field Codes:",                        ~
               at (08,25), fac(lfac$(6%)), apc_codes$           , ch(01),~
               at (08,40), fac(hex(84)),   apc_codes_d$         , ch(30),~
                                                                         ~
               at (09,02), "Select Linealmate :",                        ~
               at (09,25), fac(lfac$(7%)), apc_eq$              , ch(01),~
               at (09,40), fac(hex(84)),   apc_eq_d$            , ch(30),~
                                                                         ~
               at (10,02), "Select Hardware   :",                        ~
               at (10,25), fac(lfac$(8%)), apc_hardware$        , ch(01),~
               at (10,40), fac(hex(84)),   apc_hardware_d$      , ch(30),~
                                                                         ~
               at (11,02), "Select Packaging  :",                        ~
               at (11,25), fac(lfac$(9%)), apc_package$         , ch(01),~
               at (11,40), fac(hex(84)),   apc_package_d$       , ch(30),~
                                                                         ~
               at (12,02), "Mat'l Calc Proc. 1:",                        ~
               at (12,25), fac(lfac$(10%)), apc_mat_cal1$       , ch(02),~
               at (12,40), fac(hex(84)),   apc_mat_cal1_d$      , ch(30),~
                                                                         ~
               at (13,02), "Mat'l Calc Proc. 2:",                        ~
               at (13,25), fac(lfac$(11%)), apc_mat_cal2$       , ch(02),~
               at (13,40), fac(hex(84)),   apc_mat_cal2_d$      , ch(30),~
                                                                         ~
               at (14,02), "Material Value    :",                        ~
               at (14,25), fac(lfac$(12%)), apc_mat_val$        , ch(08),~
                                                                         ~
               at (15,02), "Labor Calc Process:",                        ~
               at (15,25), fac(lfac$(13%)), apc_lab_calc$       , ch(02),~
               at (15,40), fac(hex(84)),   apc_lab_calc_d$      , ch(30),~
                                                                         ~
               at (16,02), "Labor Value       :",                        ~
               at (16,25), fac(lfac$(14%)), apc_lab_val$        , ch(08),~
                                                                         ~
               at (16,40), "UPMH Value :",                               ~
               at (16,54), fac(lfac$(14%)), apc_lab_valu$       , ch(08),~
                                                                         ~
               at (17,02), "Labor Department  :",                        ~
               at (17,25), fac(lfac$(15%)), apc_lab_dept$       , ch(02),~
               at (17,40), fac(hex(84)),   apc_lab_dept_d$      , ch(30),~
                                                                         ~
               at (18,02), "Scrap Percent     :",                        ~
               at (18,25), fac(lfac$(16%)), apc_scrap$          , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 7% then goto L41170
                  gosub select_codes
                  goto L40070

L41170:        if keyhit% <> 8% then goto L41210
                  gosub select_equations
                  goto L40070

L41210:        if keyhit% <> 10% then goto L41260
                  eq_flag% = 1%
                  gosub select_hd_pk
                  goto L40070

L41260:        if keyhit% <> 11% then goto L41310
                  eq_flag% = 2%
                  gosub select_hd_pk
                  goto L40070

L41310:        if keyhit% <> 15 then goto L41350
                  call "PRNTSCRN"
                  goto L40350

L41350:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41580     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (7)Select Codes      " &        ~
                     "(10)Hardware           (14)Print Report"
            pf$(2) = "(4)Previous Field  (8)Linealmate        " &        ~
                     "(11)Packaging          (15)Print Screen"
            pf$(3) = "                   (9)Copy Data         " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffff07ff090a0b0cff0e0f1000)
            if fieldnr% > 1% then goto L41520
               str(pf$(2%), 1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
               goto L41550

L41520:     str(pf$(3%),18%,20%) = " " : str(pfkeys$,9%,1%) = hex(ff)
            str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
            str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
L41550:     gosub check_screen
        return

L41580: if fieldnr% > 0% then L41700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (7)Select Codes      " &        ~
                     "(10)Hardware           (14)Update Data "
            pf$(2) = "                   (8)Linealmate        " &        ~
                     "(11)Packaging          (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(12)Delete             (16)Exit Program"
            pfkeys$ = hex(01ffffffffff0708ff0a0b0cff0e0f1000)
            gosub check_screen
            if rec% = 1% then return
               str(pf$(3%),40%,20%) = " " : str(pfkeys$,12%,1%) = hex(ff)
            return
L41700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(10)Hardware       (7)Select Codes     "
            pf$(2) = "                                        " &        ~
                     "(11)Packaging      (8)Linealmate       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffff0708ff0a0bffffffffff00)
            gosub check_screen
            return

        check_screen
            if apc_codes$ = "Y" then goto L41840
               str(pf$(1%),20%,20%) = " " : str(pfkeys$,7%,1%) = hex(ff)
L41840:     if apc_eq$ = "Y" then goto L41860
               str(pf$(2%),20%,20%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41860:     if apc_hardware$ = "Y" then goto L41880
               str(pf$(1%),40%,20%) = " " : str(pfkeys$,10%,1%) = hex(ff)
L41880:     if apc_package$ = "Y" then goto L41900
               str(pf$(2%),40%,20%) = " " : str(pfkeys$,11%,1%) = hex(ff)
L41900: return

        REM *************************************************************~
            *               C O P Y   S C R E E N                       *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              on fieldnr% gosub L42250,         /* From Model Code    */  ~
                                L42250,         /* From Color Code    */  ~
                                L42250,         /* From Costing Area  */  ~
                                L42250,         /* From Process Code  */  ~
                                L42250,         /* From Field Number  */  ~
                                L42250,         /* To Model Code      */  ~
                                L42250,         /* To Linealmate Y/N  */  ~
                                L42250,         /* To Hardware   Y/N  */  ~
                                L42250          /* To Packaging  Y/N  */

              goto L42280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42280:     accept                                                       ~
               at (01,02),                                               ~
                "APC Costing Utility for Exceptions - Copy Screen",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "From Model Code   :",                        ~
               at (03,25), fac(lfac$(1%)), fr_mod$              , ch(03),~
               at (03,40), fac(hex(84)),   fr_mod_d$            , ch(30),~
                                                                         ~
               at (04,02), "From Color Code   :",                        ~
               at (04,25), fac(lfac$(2%)), fr_color$            , ch(01),~
               at (04,40), fac(hex(84)),   fr_color_d$          , ch(30),~
                                                                         ~
               at (05,02), "From Costing Area :",                        ~
               at (05,25), fac(lfac$(3%)), fr_assign$           , ch(01),~
               at (05,40), fac(hex(84)),   fr_assign_d$         , ch(30),~
                                                                         ~
               at (06,02), "From Process Code :",                        ~
               at (06,25), fac(lfac$(4%)), fr_proc$             , ch(02),~
               at (06,40), fac(hex(84)),   fr_proc_d$           , ch(30),~
                                                                         ~
               at (07,02), "From Field Number :",                        ~
               at (07,25), fac(lfac$(5%)), fr_field$            , ch(02),~
               at (07,40), fac(hex(84)),   fr_field_d$          , ch(30),~
                                                                         ~
               at (08,02), "To Model Code     :",                        ~
               at (08,25), fac(lfac$(6%)), to_mod$              , ch(03),~
               at (08,40), fac(hex(84)),   to_mod_d$            , ch(30),~
                                                                         ~
               at (09,02), "To Linealmate Y/N :",                        ~
               at (09,25), fac(lfac$(7%)), to_eq$               , ch(01),~
               at (09,40), fac(hex(84)),   to_eq_d$             , ch(30),~
                                                                         ~
               at (10,02), "To Hardware   Y/N :",                        ~
               at (10,25), fac(lfac$(8%)), to_hardware$         , ch(01),~
               at (10,40), fac(hex(84)),   to_hardware_d$       , ch(30),~
                                                                         ~
               at (11,02), "To Package    Y/N :",                        ~
               at (11,25), fac(lfac$(9%)), to_package$          , ch(01),~
               at (11,40), fac(hex(84)),   to_package_d$        , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42820
                  call "PRNTSCRN"
                  goto L42280

L42820:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L43020     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "(4)Previous Field                       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% > 1% then goto L42990
               str(pf$(2%), 1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
               goto L43000

L42990:     str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L43000: return

L43020: if fieldnr% > 0% then L43110  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Copy Data   "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffff0708ff0a0bffff0e0f1000)
            return
L43110:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(edit%)
              gosub set_pf3

L44090:     accept                                                       ~
               at (01,02),                                               ~
                "APC Costing Utility for Exceptions - Report ",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Beginning Model-(A)ll:",                     ~
               at (03,25), fac(hex(81)),   bg_mod$              , ch(03),~
               at (03,40), fac(hex(84)),   bg_mod_d$            , ch(30),~
                                                                         ~
               at (04,02), "Ending Model         :",                     ~
               at (04,25), fac(hex(81)),   ed_mod$              , ch(03),~
               at (04,40), fac(hex(84)),   ed_mod_d$            , ch(30),~
                                                                         ~
               at (05,02), "Color Code or (A)ll  :",                     ~
               at (05,25), fac(hex(81)),   bg_col$              , ch(01),~
               at (05,40), fac(hex(84)),   bg_col_d$            , ch(30),~
                                                                         ~
               at (06,02), "Costing Area or (A)ll:",                     ~
               at (06,25), fac(hex(81)),   bg_ass$              , ch(01),~
               at (06,40), fac(hex(84)),   bg_ass_d$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L44430
                  call "PRNTSCRN"
                  goto L44090

L44430:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
            inpmessage$ = "Enter a Valid Beginning/Ending Model Code or (~
        ~ALL)?"
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            if edit% = 1% then return
               str(pf$(1%),64%,16%) = " " : str(pfkeys$,14%,1%) = hex(ff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L50130,         /* Product/Model Code */  ~
                                L50230,         /* Color Code / All   */  ~
                                L51485,         /* Definition Area    */  ~
                                L50335,         /* Except Process Code*/  ~
                                L50445,         /* Product Field No.  */  ~
                                L50595,         /* Field Codes        */  ~
                                L50670,         /* Select Lineal      */  ~
                                L50745,         /* Select Hardware    */  ~
                                L50835,         /* Select Packaging   */  ~
                                L50925,         /* Material Calc Proc1*/  ~
                                L51020,         /* Material Calc Proc2*/  ~
                                L51115,         /* Material Calc Value*/  ~
                                L51155,         /* Labor Calc Proc    */  ~
                                L51295,         /* Labor Calc Value   */  ~
                                L51375,         /* Labor Department   */  ~
                                L51590          /* Scrap Percent      */
            return

L50130: REM Product / Model Code                 APC_MOD$, APC_MOD_D$
           code_key$ = " "
           str(code_key$,1%,9%) = "MODEL    "
           if apc_mod$ <> " " then goto L50190
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Product/Model Code"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L50210
           apc_mod$ = str(code_key$,10%,3%)
        lookup_model
           str(code_key$,1%,9%)   = "MODEL    "
L50190:    str(code_key$,10%,15%) = apc_mod$
           read #1,key = code_key$, using L50200,apc_mod_d$,eod goto L50210
L50200:       FMT POS(25), CH(30)
        return
L50210:     errormsg$ = "(Error) - Invalid Product/Model Code?"
            init(" ") apc_mod$, apc_mod_d$
        return

L50230: REM Product Color Code or All            APC_COLOR$, APC_COLOR_D$
           code_key$ = " "
           str(code_key$,1%,9%) = "COLOR    "
           if apc_color$ <> " " then goto L50285
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Product Color Code"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L50315
           apc_color$ = str(code_key$,10%,1%)
        lookup_color
L50285:    str(code_key$,1%,9%)   = "COLOR    "
           str(code_key$,10%,15%) = apc_color$
           read #1,key = code_key$, using L50305,apc_color_d$,            ~
                                                      eod goto L50315
L50305:       FMT POS(25), CH(30)
        return
L50315:     errormsg$ = "(Error) - Invalid Color Code?"
            init(" ") apc_color$, apc_color$
        return

L50335: REM Exception Process Code        APC_PROC$, APC_PROC_D$
           code_key$ = " "
           str(code_key$,1%,9%) = "COST PROC"
           if apc_proc$ <> " " then goto L50385
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select Exception Process Cd"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L50425
           apc_proc$ = str(code_key$,10%,2%)
L50385:    convert apc_proc$ to apc_proc%, data goto L50425

           convert apc_proc% to apc_proc$,pic(00)

           str(code_key$,10%,15%) = apc_proc$
           read #1,key = code_key$,using L50415,apc_proc_d$,eod goto L50425
L50415:       FMT POS(25), CH(30)
        return
L50425:     errormsg$ = "(Error) - Invalid Exception Process Code?"
            init(" ") apc_proc$, apc_proc_d$
        return

L50445: REM APC Field Code                    APC_FIELD$, APC_FIELD_D$
           cc_len% = 0%
           code_key$ = " "                       /* Field Start 14%,2% */
           str(code_key$,1%,9%) = "APC_FIELD"    /* Field Length 17%,2%*/
           if apc_field$ <> " " then goto L50500
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Valid Field Number"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L50575
           apc_field$ = str(code_key$,10%,2%)
L50500:    convert apc_field$ to apc_field%, data goto L50575

           convert apc_field% to apc_field$, pic(00)
           str(code_key$,10%,15%) = apc_field$
           read #1,key = code_key$, using L50530,apc_field_d$,            ~
                                                      eod goto L50575
L50530:       FMT POS(25), CH(30)
           table$ = str(apc_field_d$,1%,9%)
           convert str(apc_field_d$,17%,2%) to cc_len%, data goto L50545
L50545:
           if edit% <> 1% then return
           if rpt% = 1% or copy% = 1% then return
              gosub dataload
              if rec% = 1% then fieldnr% = 16%
        return
L50575:    errormsg$ = "(Error) - Invalid Field Number Selected?"
           init(" ") apc_field$, apc_field_d$
        return

L50595: REM APC Codes Assoc. with Field          APC_CODES$, APC_CODES_D$
           apc_field% = 0%
           convert apc_field$ to apc_field%, data goto L50610
L50610:
           if apc_field% < 1% or apc_field% > 13% then goto L50625
           if apc_codes$ <> " " then goto L50645
L50625:       apc_codes$ = "N"
              apc_codes_d$ = "(No )-Table Codes Not Applic. "
              init(" ") apc_codes$()
              return
L50645:    if apc_codes$ = "N" then goto L50625
              apc_codes$ = "Y"
              apc_codes_d$ = "(Yes)-Selected Codes Apply    "
        return

L50670: REM APC Linealmate E's and F's           APC_EQ
           if apc_mod$ = "000" or apc_color$ = "0" then goto L50685
           if apc_eq$ <> " " then goto L50690
L50685:       apc_eq$ = "N"
L50690:    if apc_eq$ <> "N" and apc_eq$ <> "Y" then goto L50725
           if apc_eq$ = "N" then                                         ~
              apc_eq_d$ = "(No)-Linealmate Not Applicable"               ~
                            else                                         ~
              apc_eq_d$ = "(Yes)-Selected Equations Apply"
           if apc_eq$ = "N" then init(" ") apc_eq$()
        return
L50725:    errormsg$ = "(Error) - Invalid Linealmate E and F Selection?"
           init(" ") apc_eq$, apc_eq$(), apc_eq_d$
        return

L50745: REM APC Select Hardware Codes            APC_HARDWARE$
        REM  IF APC_ASSIGN$ <> "5" THEN GOTO 50770

           if apc_mod$ = "000" or apc_color$ = "0" then goto L50770
           if apc_hardware$ <> " " then goto L50775
L50770:       apc_hardware$ = "N"
L50775:    if apc_hardware$ <> "N" and apc_hardware$ <> "Y" then         ~
                                                               goto L50815
           if apc_hardware$ = "N" then                                   ~
              apc_hardware_d$ = "(No)-Hardware Does not Apply  "         ~
                            else                                         ~
              apc_hardware_d$ = "(Yes)-Selected Hardware Applys"
           if apc_hardware$ = "N" then init(" ") apc_hardware$()
        return
L50815:    errormsg$ = "(Error) - Invalid Hardware Selection?"
           init(" ") apc_hardware$, apc_hardware$(), apc_hardware_d$
        return

L50835: REM APC Select Packaging                 APC_PACKAGE$
        REM  IF APC_ASSIGN$ <> "6" THEN GOTO 50860

           if apc_mod$ = "000" or apc_color$ = "0" then goto L50860
           if apc_package$ <> " " then goto L50865
L50860:       apc_package$ = "N"
L50865:    if apc_package$ <> "N" and apc_package$ <> "Y" then           ~
                                                               goto L50905
           if apc_package$ = "N" then                                    ~
              apc_package_d$ = "(No)-Packaging Does Not Apply "          ~
                            else                                         ~
              apc_package_d$ = "(Yes)-Selected Packaging Apply"
           if apc_package$ = "N" then init(" ") apc_package$()
        return
L50905:    errormsg$ = "(Error) - Invalid Packaging Selection?"
           init(" ") apc_package$, apc_package$(), apc_package_d$
        return

L50925: REM Material Calc Process                APC_MAT_CAL1$
           code_key$ = " "
           str(code_key$,1%,9%) = "COST CAL1"
           if apc_mat_cal1$ <> " " then goto L50975
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Mat'l Calc Process"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L51000
           apc_mat_cal1$ = str(code_key$,10%,2%)
L50975:    str(code_key$,10%,15%) = apc_mat_cal1$
           read #1,key = code_key$, using L50990,apc_mat_cal1_d$,         ~
                                                        eod goto L51000
L50990:       FMT POS(25), CH(30)
        return
L51000:     errormsg$ = "(Error) - Invalid Material Calc Process?"
            init(" ") apc_mat_cal1$, apc_mat_cal1_d$
        return

L51020: REM Material Calc Process                APC_MAT_CAL2$
           code_key$ = " "
           str(code_key$,1%,9%) = "COST CAL2"
           if apc_mat_cal2$ <> " " then goto L51070
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Mat'l Calc Process"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L51095
           apc_mat_cal2$ = str(code_key$,10%,2%)
L51070:    str(code_key$,10%,15%) = apc_mat_cal2$
           read #1,key = code_key$, using L51085,apc_mat_cal2_d$,         ~
                                                        eod goto L51095
L51085:       FMT POS(25), CH(30)
        return
L51095:     errormsg$ = "(Error) - Invalid Material Calc Process?"
            init(" ") apc_mat_cal2$, apc_mat_cal2_d$
        return

L51115: REM APC Material Calc Value              APC_MAT_VAL$
           if apc_mat_val$ <> " " then goto L51130
L51125:       apc_mat_val$ = "0.0"
L51130:    convert apc_mat_val$ to apc_mat_val, data goto L51125

           convert apc_mat_val to apc_mat_val$, pic(####.##-)
        return

L51155: REM Labor Calc Process                   APC_LAB_CALC$
           if apc_assign$ = "0" then goto L51170
              apc_lab_calc$ = "00"
L51170:    code_key$ = " "
           str(code_key$,1%,9%) = "COST CAL3"
           if apc_lab_calc$ <> " " then goto L51215
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Labor Calc Process"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L51275
           apc_lab_calc$ = str(code_key$,10%,2%)
L51215:    str(code_key$,10%,15%) = apc_lab_calc$
           read #1,key = code_key$, using L51230,apc_lab_calc_d$,         ~
                                                        eod goto L51275
L51230:       FMT POS(25), CH(30)
           if apc_assign$ = "0" then return
              if edit% <> 1% then return
              if rpt% = 1% or copy% = 1% then return
                 gosub L51295
                 gosub L51375
                 gosub L51590
                 fieldnr% = 16%
        return
L51275:     errormsg$ = "(Error) - Invalid Labor Calc Process?"
            init(" ") apc_lab_calc$, apc_lab_calc_d$
        return

L51295: REM APC Labor Calc Value                 APC_LAB_VAL$
           if apc_lab_calc$ = "00" then goto L51310
           if apc_lab_val$ <> " " then goto L51315
L51310:       apc_lab_val$ = "0.0"
L51315:    convert apc_lab_val$ to apc_lab_val, data goto L51310

           convert apc_lab_val to apc_lab_val$, pic(####.##-)
        REM APC Labor UPMH Value                 APC_LAB_VALU$
           if apc_lab_calc$ = "00" then goto L51345
           if apc_lab_valu$ <> " " then goto L51350
L51345:       apc_lab_valu$ = "0.0"
L51350:    convert apc_lab_valu$ to apc_lab_valu, data goto L51345

           convert apc_lab_valu to apc_lab_valu$, pic(####.##-)
        return

L51375: REM Labor Department Code                APC_LAB_DEPT$
           if apc_lab_calc$ <> "00" then goto L51395
              apc_lab_dept$ = "XX"

L51395:    code_key$ = " "
           str(code_key$,1%,9%) = "COST_06ST"
           if apc_lab_dept$ <> " " then goto L51440
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Labor Department  "
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L51465
           apc_lab_dept$ = str(code_key$,10%,2%)
L51440:    str(code_key$,10%,15%) = apc_lab_dept$
           read #1,key = code_key$, using L51455,apc_lab_dept_d$,         ~
                                                           eod goto L51465
L51455:       FMT POS(25), CH(30)
        return
L51465:     errormsg$ = "(Error) - Invalid Labor Department Code?"
            init(" ") apc_lab_dept$, apc_lab_dept_d$
        return

L51485: REM Definition Costing Area              APC_ASSIGN$
           code_key$ = " "
           str(code_key$,1%,9%) = "COST AREA"
           if apc_assign$ <> " " then goto L51540
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Costing Area      "
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L51570
           apc_assign$ = str(code_key$,10%,1%)
        lookup_area
L51540:    str(code_key$,1%,9%)   = "COST AREA"
           str(code_key$,10%,15%) = apc_assign$
           read #1,key = code_key$, using L51560,apc_assign_d$,           ~
                                                           eod goto L51570
L51560:       FMT POS(25), CH(30)
        return
L51570:     errormsg$ = "(Error) - Invalid Costing Assigment Area?"
            init(" ") apc_assign$, apc_assign_d$
        return

L51590: REM APC Scrap Percent                    APC_SCRAP$
           if apc_scrap$ <> " " then goto L51605
L51600:       apc_scrap$ = "0.0"
L51605:    convert apc_scrap$ to apc_scrap, data goto L51600

           convert apc_scrap to apc_scrap$, pic(####.##-)
        return

        REM *************************************************************~
            *              C O P Y   S C R E E N   E D I T S            *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L53190,         /* From Product/Model */  ~
                                L53370,         /* From Color Code    */  ~
                                L53590,         /* From Assignment Cd */  ~
                                L53810,         /* From Process Code  */  ~
                                L54070,         /* From Field Code    */  ~
                                L54370,         /* To Product Code    */  ~
                                L54560,         /* To Copy Linealmate */  ~
                                L54690,         /* To Copy Hardware   */  ~
                                L54820          /* To Copy Packaging  */
            return

L53190: REM Product / Model Code                 FR_MOD$, FR_MOD_D$
           code_key$ = " "
           str(code_key$,1%,9%) = "MODEL    "
           if fr_mod$ <> " " then goto L53290
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Product/Model Code"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L53330
           fr_mod$ = str(code_key$,10%,3%)
L53290:    str(code_key$,10%,15%) = fr_mod$
           read #1,key = code_key$, using L53310,fr_mod_d$,eod goto L53330
L53310:       FMT POS(25), CH(30)
        return
L53330:     errormsg$ = "(Error) - Invalid From Product/Model Code?"
            init(" ") fr_mod$, fr_mod_d$
        return

L53370: REM Product Color Code or All            FR_COLOR$, FR_COLOR_D$
           if fr_color$ <> "A" then goto L53410
              fr_color_d$ = "Copy (All) Colors To Product?"
              return
L53410:    code_key$ = " "
           str(code_key$,1%,9%) = "COLOR    "
           if fr_color$ <> " " then goto L53500
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Product Color Code"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L53550
           fr_color$ = str(code_key$,10%,1%)
L53500:    str(code_key$,10%,15%) = fr_color$
           read #1,key = code_key$, using L53530,fr_color_d$,             ~
                                                      eod goto L53550
L53530:       FMT POS(25), CH(30)
        return
L53550:     errormsg$ = "(Error) - Invalid From Color Code?"
            init(" ") fr_color$, fr_color$
        return

L53590: REM Definition Costing Area              FR_ASSIGN$
           if fr_assign$ <> "A" then goto L53630
              fr_assign_d$ = "Copy (All) Costing Areas?"
              return
L53630:    code_key$ = " "
           str(code_key$,1%,9%) = "COST AREA"
           if fr_assign$ <> " " then goto L53720
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Costing Area      "
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L53770
           fr_assign$ = str(code_key$,10%,1%)
L53720:    str(code_key$,10%,15%) = fr_assign$
           read #1,key = code_key$, using L53750, fr_assign_d$,           ~
                                                           eod goto L53770
L53750:       FMT POS(25), CH(30)
        return
L53770:     errormsg$ = "(Error) - Invalid Costing Assigment Area?"
            init(" ") fr_assign$, fr_assign_d$
        return

L53810: REM Exception Process Code        FR_PROC$, FR_PROC_D$
           if str(fr_proc$,1%,1%) <> "A" then goto L53860
              fr_proc$ = "AA"
              fr_proc_d$ = "Copy (All) Process Codes "
              return
L53860:    code_key$ = " "
           str(code_key$,1%,9%) = "COST PROC"
           if fr_proc$ <> " " then goto L53950
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select Exception Process Cd"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L54030
           fr_proc$ = str(code_key$,10%,2%)
L53950:    convert fr_proc$ to fr_proc%, data goto L54030

           convert fr_proc% to fr_proc$,pic(00)

           str(code_key$,10%,15%) = fr_proc$
           read #1,key = code_key$,using L54010,fr_proc_d$,eod goto L54030
L54010:       FMT POS(25), CH(30)
        return
L54030:     errormsg$ = "(Error) - Invalid Exception Process Code?"
            init(" ") fr_proc$, fr_proc_d$
        return

L54070: REM APC Field Code                    FR_FIELD$, FR_FIELD_D$
           if str(fr_field$,1%,1%) <> "A" then goto L54120
              fr_field$ = "AA"
              fr_field_d$ = "Copy (All) Field Codes"
              return
L54120:    cc_len% = 0%
           code_key$ = " "                       /* Field Start 14%,2% */
           str(code_key$,1%,9%) = "APC_FIELD"    /* Field Length 17%,2%*/
           if fr_field$ <> " " then goto L54220
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Valid Field Number"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L54330
           fr_field$ = str(code_key$,10%,2%)
L54220:    convert fr_field$ to fr_field%, data goto L54330

           convert fr_field% to fr_field$, pic(00)
           str(code_key$,10%,15%) = fr_field$
           read #1,key = code_key$, using L54280,fr_field_d$,             ~
                                                      eod goto L54330
L54280:       FMT POS(25), CH(30)
           table$ = str(fr_field_d$,1%,9%)
           convert str(fr_field_d$,17%,2%) to cc_len%, data goto L54310
L54310:
        return
L54330:    errormsg$ = "(Error) - Invalid Field Number Selected?"
           init(" ") fr_field$, fr_field_d$
        return

L54370: REM APC To Product/Model Codes           TO_MOD$, TO_MOD_D$
           if fr_mod$ = to_mod$ then goto L54520
           code_key$ = " "
           str(code_key$,1%,9%) = "MODEL    "
           if to_mod$ <> " " then goto L54480
        REM - TEST LOOKUP
           f1%(1%) = -10%
           desc$ = hex(06) & "Select a Product/Model Code"
           call "PLOWCODE" (#1, code_key$, desc$, 9%, .30, f1%(1%))
           if f1%(1%) = 0% then goto L54520
           to_mod$ = str(code_key$,10%,3%)
L54480:    str(code_key$,10%,15%) = to_mod$
           read #1,key = code_key$, using L54500,to_mod_d$,eod goto L54520
L54500:       FMT POS(25), CH(30)
        return
L54520:     errormsg$ = "(Error) - Invalid (To) Product/Model Code?"
            init(" ") to_mod$, to_mod_d$
        return

L54560: REM APC Copy Linealmate                  TO_EQ$
           if to_eq$ <> " " then goto L54590
              to_eq$ = "N"
L54590:    if to_eq$ <> "N" and to_eq$ <> "Y" then goto L54650
           if to_eq$ = "N" then                                          ~
              to_eq_d$ = "(No)-Do Not Copy Linealmate."                  ~
                            else                                         ~
              to_eq_d$ = "(Yes)-Copy Linealmate Data."
        return
L54650:    errormsg$ = "(Error) - Invalid Copy Linealmate Selection?"
           init(" ") to_eq$, to_eq_d$
        return

L54690: REM APC Copy Hardware                    TO_HARDWARE$
           if to_hardware$ <> " " then goto L54720
              to_hardware$ = "N"
L54720:    if to_hardware$ <> "N" and to_hardware$ <> "Y" then goto L54780
           if to_hardware$ = "N" then                                    ~
              to_hardware_d$ = "(No)-Do Not Copy Hardware.  "            ~
                            else                                         ~
              to_hardware_d$ = "(Yes)-Copy Hardware Data.  "
        return
L54780:    errormsg$ = "(Error) - Invalid Copy Hardware Selection?"
           init(" ") to_hardware$, to_hardware_d$
        return

L54820: REM APC Copy Packaging                   TO_PACKAGE$
           if to_package$ <> " " then goto L54850
              to_package$ = "N"
L54850:    if to_package$ <> "N" and to_package$ <> "Y" then goto L54910
           if to_package$ = "N" then                                     ~
              to_package_d$ = "(No)-Do Not Copy Packaging. "             ~
                            else                                         ~
              to_package_d$ = "(Yes)-Copy Packaging Data. "
        return
L54910:    errormsg$ = "(Error) - Invalid Copy Packaging Selection?"
           init(" ") to_package$, to_package_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55070: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

        %!                                                               ~
        ~                                                                !

                                                   /* Header Format    */
L55140: %! ######## @ ########                    #######################~
        ~#################                              (###)  Page: ### !
                                                   /* Detail Format    */
L55170: %!(Product/Model):### ##############################  Color Code ~
        ~   :  # ########## Cost Assign: # ##############################!

L55200: %! Process Code  : ## ##############################  Field Code ~
        ~   : ## ##############################                          !

L55230: %! Mat'l Calc (1): ## ##############################  Mat'l Calc ~
        ~(2): ## ##############################    Mat'l Value: ######## !

L55260: %! Lab Calc Proc.: ## ##############################  Lab Departm~
        ~ent: ## ##############################    Labor Value: ######## !

L55290: %!                                                    Scrap Perce~
        ~nt : ########                             UPMH Value : ######## !

L55320: %! ##############: ###################################  #########~
        ~##########################  ################################### !


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            title$ = " APC Costing Exception Definition Report"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            call "DATEFMT" (date$)
            call "SETPRNT" (" ","CEXP",0%,0%)
            select printer(134)
        return

        prt_header
            init(" ") xtime$
            call "TIME" (xtime$)
            if lcnt% <> 99% then print using L55040
            pageno% = pageno% + 1%
            print page
            print using L55040
            print using L55140, date$, xtime$, title$, apc_mod$, pageno%
            lcnt% = 2%
        return

        prt_dtl1
            if lcnt% > 56% then gosub prt_header
               print using L55070
               print using L55170, apc_mod$, apc_mod_d$, apc_color$,      ~
                                apc_color_d$, apc_assign$, apc_assign_d$
               print using L55200, apc_proc$, apc_proc_d$, apc_field$,    ~
                                                          apc_field_d$
               lcnt% = lcnt% + 3%
            if lcnt% > 55% then gosub prt_header
               print using L55070
               print using L55230, apc_mat_cal1$, apc_mat_cal1_d$,        ~
                            apc_mat_cal2$, apc_mat_cal2_d$, apc_mat_val$
               print using L55260, apc_lab_calc$, apc_lab_calc_d$,        ~
                            apc_lab_dept$, apc_lab_dept_d$, apc_lab_val$
               print using L55290, apc_scrap$, apc_lab_valu$
               lcnt% = lcnt% + 4%
        return

        prt_dtl2
            if lcnt% > 58% then gosub prt_header
               print using L55320, hdr$, c$(k%), c$(k% + 1%), c$(k% + 2%)
               lcnt% = lcnt% + 1%
        return

        gen_rpt
           rpt% = 1%
           gosub select_printer
           init(" ") cst_key$
           if bg_mod$ = "ALL" then goto gen_rpt_nxt
              str(cst_key$,1%,3%) = bg_mod$
        gen_rpt_nxt
           read #5,key > cst_key$, using L60580, cst_key$,                ~
                                                    eod goto gen_rpt_done
L60580:      FMT CH(9)
           if bg_mod$ = "ALL" then goto L60610
              if str(cst_key$,1%,3%) > ed_mod$ then goto gen_rpt_done
L60610:    gosub dataload
           gosub prt_dtl1          /* Print Definition Data            */
           gosub fields_load_prt   /* Print the Selected Values Fields */
           gosub lineal_load_prt   /* Print Selected Values Linealmate */
           eq_flag% = 1%           /* Print Selected Values Hardware   */
           gosub hp_load_prt
           eq_flag% = 2%           /* Print Selected Values Packaging  */
           gosub hp_load_prt
           goto gen_rpt_nxt
        gen_rpt_done
           print using L55040
           close printer
        return clear all
        goto inputmode

        fields_load_prt
            if lcnt% > 57% then gosub prt_header
               print using L55070
            hdr$ = "***" & table$ & "**"
            init(" ") c$(), code_key$
            cc_max% = 0%
            if apc_codes$ = "N" then goto L60880
            for k% = 1% to 50%               /* Count Codes Selected */
              if str(apc_codes$(k%),1%,1%) = " " then goto L60870
                 cc_max% = cc_max% + 1%
            next k%
L60870:     if cc_max% <> 0% then goto L60920
L60880:        hdr$ = "N/A FIELDS****"
               k% = 1%
               gosub prt_dtl2
               return
L60920:     str(code_key$,1%,9%) = table$
        for k% = 1% to cc_max%
            str(code_key$,10%,15%) = apc_codes$(k%)
            read #1,key = code_key$, using  L60960, desc$, eod goto L60990
L60960:        FMT POS(25), CH(30)
            str(c$(k%),1%,cc_len%)= str(code_key$,10%,cc_len%)
            str(c$(k%),cc_len%+1%,31%) = "-" & desc$
L60990: next k%
        for k% = 1% to cc_max% step 3%
            gosub prt_dtl2
            hdr$ = " "
        next k%
            print using L55070
        return

        lineal_load_prt
            hdr$ = "**LINEALMATE**"
            init(" ") c$(), code_key$
            cc_max% = 0%
            if apc_eq$ = "N" then goto L61170
            for k% = 1% to 25%               /* Count Codes Selected */
              if str(apc_eq$(k%),1%,1%) = " " then goto L61160
                 cc_max% = cc_max% + 1%
            next k%
L61160:     if cc_max% <> 0% then goto L61210
L61170:        hdr$ = "N/A LINEALMATE"
               k% = 1%
               gosub prt_dtl2
               return
L61210:     str(code_key$,1%,9%)   = "EQUATIONS "
            str(code_key$,10%,2%)  = str(apc_mod$,1%,1%)     & "-"
        for k% = 1% to cc_max%
            str(code_key$,12%,2%)  = str(apc_eq$(k%),1%,1%)  & "-"
            str(code_key$,14%,11%) = str(apc_eq$(k%),2%,2%)
            read #1,key = code_key$, using  L61270, desc$, eod goto L61300
L61270:        FMT POS(25), CH(30)
            str(c$(k%),1%,3%)  = apc_eq$(k%)
            str(c$(k%),4%,31%) = "-" & desc$
L61300: next k%
        for k% = 1% to cc_max% step 3%
            gosub prt_dtl2
            hdr$ = " "
        next k%
            print using L55070
        return

        hp_load_prt                          /* HARDWARE AND PACKAGING */
            hdr$ = "***HARDWARE***"
            if eq_flag% = 2% then hdr$ = "***PACKAGING**"
            init(" ") c$(), code_key$
            cc_max% = 0%
            if eq_flag% = 1% and apc_hardware$ = "N" then goto L61530
            if eq_flag% = 2% and apc_package$  = "N" then goto L61530
            for k% = 1% to 25%               /* Count Codes Selected */
              if eq_flag% = 1% and str(apc_hardware$(k%),1%,1%) = " "    ~
                                                         then goto L61520
              if eq_flag% = 2% and str(apc_package$(k%),1%,1%) = " "     ~
                                                         then goto L61520
                 cc_max% = cc_max% + 1%
            next k%
L61520:     if cc_max% <> 0% then goto L61580
L61530:        hdr$ = "N/A HARDWARE**"
               if eq_flag% = 2% then hdr$ = "N/A PACKAGING*"
               k% = 1%
               gosub prt_dtl2
               return
L61580:     str(code_key$,1%,1%) = "0"                     /* HARDWARE */
            if eq_flag% = 2% then str(code_key$,1%,1%) = "1"
            str(code_key$,2%,3%) = apc_mod$
            str(code_key$,5%,1%) = apc_color$
        for k% = 1% to cc_max%
            str(code_key$,6%,15%)  = apc_hardware$(k%)
            if eq_flag% = 2% then str(code_key$,6%,15%)=apc_package$(k%)

            read #3,key = code_key$, using  L61670, desc$, eod goto L61700
L61670:        FMT POS(21), CH(19)
            str(c$(k%),1%,15%) = str(code_key$,6%,15%)
            str(c$(k%),16%,20%) = "-" & desc$
L61700: next k%
        for k% = 1% to cc_max% step 3%
            gosub prt_dtl2
            hdr$ = " "
        next k%
           if eq_flag% = 1% then print using L55070
        return

        select_codes                    /* Select Data Specified Field */
            call "APCCS1SB" (table$, cc_len%, apc_codes$(), #1)
        return

        select_equations                /* Select Linealmate A,B,C,D'S */
            call "APCCS2SB" (apc_mod$, apc_color$, apc_eq$(), #1, #2)
        return

        select_hd_pk                    /* Select Hardware/Packaging   */
            if eq_flag% <> 1% then goto L61910
            call "APCCS3SB" ( eq_flag%, apc_mod$, apc_color$,            ~
                                                     apc_hardware$(), #3)
        return
L61910:     call "APCCS3SB" ( eq_flag%, apc_mod$, apc_color$,            ~
                                                      apc_package$(), #3)
        return

        copy_data
            call "SHOSTAT" ("Copying Data 'To'( " & to_mod$ & " )")
            copy% = 1%
            init(" ") cst_key$, sav_key$
            str(cst_key$,1%,3%) = fr_mod$
        copy_nxt
            read #5,key > cst_key$, using L62030, cst_key$,               ~
                                                 eod goto copy_done
L62030:        FMT CH(9)
            sav_key$ = cst_key$
            if str(sav_key$,1%,3%) <> fr_mod$ then goto copy_done
               if fr_color$ = "A" then goto L62080
                  if str(sav_key$,4%,1%) <> fr_color$ then goto copy_nxt
L62080:        if fr_assign$ = "A" then goto L62100
                  if str(sav_key$,5%,1%) <> fr_assign$ then goto copy_nxt
L62100:        if fr_proc$ = "AA" then goto L62120
                  if str(sav_key$,6%,2%) <> fr_proc$ then goto copy_nxt
L62120:        if fr_field$ = "AA" then goto L62150
                  if str(sav_key$,8%,2%) <> fr_field$ then goto copy_nxt

L62150:     gosub dataload
            str(cst_key$,1%,3%) = to_mod$
            apc_mod$ = to_mod$
            if to_eq$ = "Y" then goto L62210           /* No Linealmate */
               apc_eq$ = "N" : init(" ") apc_eq$()

L62210:     if to_hardware$ = "Y" then goto L62240     /* No Hardware   */
               apc_hardware$ ="N" : init(" ") apc_hardware$()

L62240:     if to_package$ = "Y" then goto L62270      /* No Packaging  */
               apc_package$ = "N" : init(" ") apc_package$()

L62270:     gosub dataput
            cst_key$ = sav_key$
            goto copy_nxt
        copy_done
        return clear all
        goto inputmode_a

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end
