        REM *************************************************************~
            *              PF(2) - KEY CONVERT DATA                     *~
            *  Program Name      - APCPLN01                             *~
            *  Creation Date     - 07/01/94                             *~
            *  Last Modified Date- 11/13/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program Defines the Master      *~
            *                      Planning Department File. Sets-Up    *~
            *                      Link between Departments, Process,   *~
            *                      Shift and Product(Models).           *~
            *                                                           *~
            *  Code Tables Used  - (MODEL    ) - Model Codes            *~
            *                      (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                      (PLAN UNIT) - Planning Units Codes   *~
            *                      (PLAN DATE) - Production Date Codes  *~
            *                                                           *~
            *  Special Comments  - All Tables Excep (MODEL) can have    *~
            *                      Alphnumeric values.                  *~
            *                                                           *~
            *                      Uses Table Subroutine (APCPLN1B)     *~
            *                      for Displaying Code table Values.    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/26/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/13/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            *10/05/2007! (EWD009) Mod for alpha dept #            ! DES *~
            *09/02/20191 CR2199 Add delete data function F3       ! RDB *~
            *************************************************************

        dim                              /* (APCPLNDP) - FILE          */~
            pl_model$3, pl_model_d$30,   /* Model Code or ALL          */~
            pl_shft$2,  pl_shft_d$30,    /* Planning Shift Cd-PLAN SHFT*/~
            pl_dept$3,  pl_dept_d$30,    /* Planning Dept Cd- PLAN DEPT*/~
            pl_proc$2,  pl_proc_d$30,    /* Planning Proc Cd- PLAN PROC*/~
            pl_units$2, pl_units_d$30,   /* Planning Units Cd-PLAN UNIT*/~
            pl_prod$1,  pl_prod_d$30,    /* Plan Prod Date Cd-PLAN DATE*/~
            pl_units_v$10,               /* Plan Units Value           */~
            pl_seq$3,  cc_rec$32,        /* Plan Sort Sequence No.     */~
            pl_key$14, pl_rec$32,        /*                            */~
            pl_fil$1, cc_key$12          /* Filler Area                */

        dim                              /* Cop Screen Variables       */~
            fr_model$3, fr_model_d$30,   /* From Model                 */~
            to_model$3, to_model_d$30,   /* To Model Code              */~
            fr_dept$3,  fr_dept_d$30,    /* From Department Code       */~
            to_dept$3,  to_dept_d$30,    /* To Department Code         */~
            fr_proc$2,  fr_proc_d$30,    /* From Process Code          */~
            to_proc$2,  to_proc_d$30,    /* To Process Code            */~
            fr_shft$2,  fr_shft_d$30,    /* From Shift Code            */~
            to_shft$2,  to_shft_d$30     /* To Shift Code              */

        dim                              /* Delete Screen Variables    */~
            dl_model$3, dl_model_d$30,   /* Delete Model               */~
            dl_dept$3, dl_dept_d$30,     /* Delete Deparment           */~
            dl_proc$2, dl_proc_d$30,     /* Delete Process Code        */~
            dl_shft$2, dl_shft_d$30      /* Delete Shift Code          */
                                                
        dim                              /* (Program) - Variables      */~
            tab$(10%)10,                 /* Save Code Table Names      */~
            code$15,                     /* Use To Look-Up Table Code  */~
            title$60, date$8,            /* REPORT TITLE               */~
            sel$1, sel_d$30,             /* Report Selection Code      */~
            bg_code$3, bg_code_d$30,     /* Beginning Code Selection   */~
            ed_code$3, ed_code_d$30,     /* Ending Code Selection      */~
            bg_mod$3, bg_mod_d$30,       /* Beginning Model Selection  */~
            ed_mod$3, ed_mod_d$30,       /* Ending Model Selection     */~
            hdr1$30,                     /* Column one (1) Header      */~
            hdr2$10, hdr3$10, hdr4$,     /* Columns (2) thru (4)       */~
            hdr5$27,                     /* Column (5) - Units         */~
            dtl1$3, dtl2$30, dtl3$10,    /* DETAIL PRINT DATA          */~
            dtl4$10, dtl5$10, dtl6$27,   /* ELEMENTS                   */~
            dtl7$10, dtl8$19,            /*                            */~
            runtime$8, sav_code$3,       /* REPORT RUN TIME/BREAK CODE */~
            readkey$50, desc$30,         /* Generic Key                */~
            cursor%(2%), fld%(24%),      /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            hdr$79, msg$(3%)79,          /* Display message on delete  */~
            dl_cnt$6                     /* Count of deleted records   */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = " Planning Master Department Edit/Report "
            pname$ = "APCPLN01 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNDP ! Planning Master Department File          *~
            * #2  ! APCPLNDP ! Planning Master Department File (Copy)   *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #2,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            call "SHOSTAT" ("Initialization")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 100%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),   0%, rslt$(3%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            tab$(1%) = "PLAN DEPT" : tab$(2%) = "PLAN PROC"
            tab$(3%) = "MODEL    " : tab$(4%) = "PLAN SHFT"
            tab$(5%) = "PLAN UNIT" : tab$(6%) = "PLAN DATE"

            fld%(1%) = 1% : fld%(9%)  = 4% : fld%(17%) = 4%
            fld%(2%) = 1% : fld%(10%) = 4% : fld%(18%) = 4%
            fld%(3%) = 2% : fld%(11%) = 4% : fld%(19%) = 4%
            fld%(4%) = 2% : fld%(12%) = 4% : fld%(20%) = 4%
            fld%(5%) = 3% : fld%(13%) = 4% : fld%(21%) = 4%
            fld%(6%) = 3% : fld%(14%) = 4% : fld%(22%) = 4%
            fld%(7%) = 4% : fld%(15%) = 4% : fld%(23%) = 4%
            fld%(8%) = 4% : fld%(16%) = 4% : fld%(24%) = 4%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 8%
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
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% and fieldnr% = 1% then report_input
/* CR2199 */          if keyhit% =  3% and fieldnr% = 1% then delete_input
                      if keyhit% =  5% and fieldnr% = 1% then copy_input
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
                  if keyhit%  = 12% then gosub delete_record
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

                
        REM *************************************************************~
            *      I N P U T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        report_input
            gosub initialize_variables

            for fieldnr% = 1% to 5%
L12080:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12200
L12100:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12180
L12130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L12100
                         if fieldnr% = 1% then L12080
                         goto L12130
L12180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12100
L12200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12100
            next fieldnr%

        REM *************************************************************~
            *       E D I T   M O D E   R E P O R T   S C R E E N       *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L13110:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 5% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13160:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13160
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13160
                  lastfieldnr% = fieldnr%
            goto L13110

        REM *************************************************************~
            *         I N P U T   M O D E   C O P Y   S C R E E N       *~
            *************************************************************

        copy_input
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L14080:         gosub'071(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L14200
L14100:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L14180
L14130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L14100
                         if fieldnr% = 1% then L14080
                         goto L14130
L14180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L14100
L14200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14100
            next fieldnr%

        REM *************************************************************~
            *          E D I T   M O D E   C O P Y   S C R E E N        *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub copy_data
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg3
L15110:     fdn% = cursor%(1%) - 3%
            if fdn% < 1% then fdn% = 1%
            fieldnr% = fld%(fdn%)
            if fieldnr% < 1% or fieldnr% > 4% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'071(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L15160:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15160
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L15160
                  lastfieldnr% = fieldnr%
            goto L15110

        REM *************************************************************~
            *  CR2199                                                   *~
            *     I N P U T   M O D E   D E L E T E   S C R E E N       *~
            *************************************************************
        delete_input
            gosub initialize_variables
                     
            pass% = 0%
            call "APCPASSW" ("APCPLN01", userid$, pass%)
            if pass% <> 0% then goto not_author
            
            for fieldnr% = 1% to 4%
L24080:         gosub'071(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L24200
L24100:         gosub'104(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L24180
L24130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L24100
                         if fieldnr% = 1% then L24080
                         goto L24130
L24180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L24100
L24200:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L24100
            next fieldnr%

        REM *************************************************************~
            *      E D I T   M O D E   D E L E T E   S C R E E N        *~
            *************************************************************
        editpg4
            lastfieldnr% = 0%
            gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub delete_data
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg4
L25110:     fdn% = cursor%(1%) - 3%
            if fdn% < 1% then fdn% = 1%
            fieldnr% = fld%(fdn%)
            if fieldnr% < 1% or fieldnr% > 4% then editpg4
            if fieldnr% = lastfieldnr% then    editpg4
            gosub'071(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg4
L25160:     gosub'104(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L25160
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L25160
                  lastfieldnr% = fieldnr%
            goto L25110
            
                            
        not_author           /* CR2199 */
                errormsg$ = "Not Authorized"
                gosub'101(fieldnr%,1%)
                goto inputmode
        return
        
        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub set_report_format
            gosub select_printer
            gosub generate_report
            close printer
        return clear all
        goto inputmode

        select_printer
            call "SHOSTAT" ("Printing " & str(title$,19%,22%) )
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
        return

        set_report_format
          if sel% <> 1% then goto L19320
             title$ = "Master Department Report-Sort Department"
             hdr1$  = "<- Planning Department Codes->"
             hdr2$  = "Proc. Code"
             hdr3$  = "Shift Code"
             hdr4$  = "Model Code"
             hdr5$  = "<-- Planning Unit Codes -->"
             return
L19320:   if sel% <> 2% then goto L19400
             title$ = "Master Department Report-Sort by Shift  "
             hdr1$  = "<--- Planning Shift Codes --->"
             hdr2$  = "Dept. Code"
             hdr3$  = "Proc. Code"
             hdr4$  = "Model Code"
             hdr5$  = "<-- Planning Unit Codes -->"
             return
L19400:   title$ = "Master Department Report-Sort by Units  "
          hdr1$  = "<Planning Product Units Codes>"
          hdr2$  = "Model Code"
          hdr3$  = "Shift Code"
          hdr4$  = "Dept. Code"
          hdr5$  = "<- Planning Process Codes->"
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
        deffn'061(fieldnr%)
        deffn'071(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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
         "Enter a Valid Planning Department Code?                      ",~
         "Enter a Valid Planning Process Code?                         ",~
         "Enter a Valid Planning Shift Code?                           ",~
         "Enter a Valid Planning Model Code?                           ",~
         "Enter a Valid Planning Units Code?                           ",~
         "Enter a Valid Planning Units Value?                          ",~
         "Enter a Valid Planning Production Date Code?                 ",~
         "Enter a Valid Production Sort Seq. No. for Product (0 - 255)?"

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28310
                inpmessage$ = edtmessage$
                return

L28310
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Report Selection 1 = Dept, 2 = Shift, 3 = Model ??   ",~
         "Enter a Begin Code Associated with Report Selection or ALL ??",~
         "Enter a End Code Associated with Report Selection ??         ",~
         "Enter a Beginning Model Code or ALL?                         ",~
         "Enter a Ending Model Code?                                   "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28480
                inpmessage$ = edtmessage$
                return

L28480
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter a Beginning and Ending Department Code or Same?        ",~
         "Enter a Beginning and Ending Process Code or the Same?       ",~
         "Enter a Beginning and Ending Shift Code or the Same?         ",~
         "Enter a Beginning and Ending Model Code or the Same/ALL      "

        deffn'074(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28481
                inpmessage$ = edtmessage$
                return

L28481
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn4_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn4_msg  :  data                                               ~
         "Enter a Delete Department Code?                              ",~
         "Enter a Delete Process Code or AL?                           ",~
         "Enter a Delete Shift Code or AL?                             ",~
         "Enter a Delete Model Code?                                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, pl_model$,       ~
                      pl_shft$, pl_dept$, pl_proc$, pl_units$, pl_prod$, ~
                      pl_fil$, pl_dept_d$, pl_proc_d$,                   ~
                      pl_model_d$, pl_shft_d$, pl_units_d$, pl_units_v$, ~
                      pl_prod_d$, sel$, sel_d$, bg_code$,                ~
                      bg_code_d$, ed_code$, ed_code_d$, bg_mod$, ed_mod$,~
                      bg_mod_d$, ed_mod_d$, pl_seq$, fr_dept$, fr_model$,~
                      fr_dept_d$, fr_model_d$, fr_proc$, fr_proc_d$,     ~
                      fr_shft$, fr_shft_d$, to_dept$, to_model$,         ~
                      to_dept_d$, to_model_d$, to_proc$, to_proc_d$,     ~
                      to_shft$, to_shft_d$, dl_model$, dl_dept$,         ~
                      dl_proc$, dl_shft$, dl_model_d$, dl_dept_d$,       ~
                      dl_proc_d$, dl_shft_d$

            pl_units = 0.0 : rec% = 0%
 
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
            pl_key$ = " "
            str(pl_key$,1%,3%)  = pl_dept$
            str(pl_key$,4%,2%)  = pl_proc$
            str(pl_key$,6%,2%)  = pl_shft$
            str(pl_key$,8%,3%)  = pl_model$
            str(pl_key$,11%,2%) = pl_units$
            read #1,hold,key = pl_key$, eod goto L30310
        dataload_rpt
            pl_seq% = 0%
            get #1, using L35040, pl_model$, pl_units$, pl_model$,        ~
                                 pl_shft$, pl_dept$, pl_proc$, pl_shft$, ~
                                 pl_model$, pl_units$, pl_units,pl_prod$,~
                                 pl_seq%
            rec% = 1%
            convert pl_seq% to pl_seq$, pic(000)
            gosub L50180                           /* Department Codes  */
            gosub L50340                           /* Process Codes     */
            gosub L50500                           /* Shift Codes       */
            gosub L50660                           /* Model Codes       */
            gosub L50860                           /* Units Codes       */
            gosub L51210                           /* Prod. Date Code   */
            gosub L51370                           /* Prod Sequence No. */
            convert pl_units to pl_units_v$, pic(####.####-)
L30310: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_record
        dataput
            pl_key$ = " "
            str(pl_key$,1%,3%)  = pl_dept$
            str(pl_key$,4%,2%)  = pl_proc$
            str(pl_key$,6%,2%)  = pl_shft$
            str(pl_key$,8%,3%)  = pl_model$
            str(pl_key$,11%,2%) = pl_units$
            read #1,hold,key = pl_key$, eod goto L31180
               delete #1
               if keyhit% = 12% then goto L31230

L31180:     put #1, using L35040, pl_model$, pl_units$, pl_model$,        ~
                                 pl_shft$, pl_dept$, pl_proc$, pl_shft$, ~
                                 pl_model$, pl_units$, pl_units,pl_prod$,~
                                 pl_seq%
            write #1, eod goto L31250
L31230: return clear all
        goto inputmode
L31250:     errormsg$ = "(Error)-Unable to Update Department File? " &   ~
                        pl_key$
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (APCPLNDP)          */
L35040: FMT CH(03),                      /* Model Code        MODEL    */~
            CH(02),                      /* Units Code        PLAN UNIT*/~
            CH(03),                      /* Model Code        MODEL    */~
            CH(02),                      /* Shift Code        PLAN SHFT*/~
            CH(03),                      /* Department Code   PLAN DEPT*/~
            CH(02),                      /* Process Code      PLAN PROC*/~
            CH(02),                      /* Shift Code        PLAN SHFT*/~
            CH(03),                      /* Model Code        MODEL    */~
            CH(02),                      /* Units Code        PLAN UNIT*/~
            PD(14,4),                    /* Planning Units Value       */~
            CH(01),                      /* Prod. Date Code   PLAN DATE*/~
            BI(1)                        /* Sort Sequence Code         */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40230,         /* Department Code      */~
                                L40230,         /* Process Code         */~
                                L40230,         /* Shift Code           */~
                                L40230,         /* Model Code           */~
                                L40230,         /* Units Code           */~
                                L40230,         /* Units Value          */~
                                L40230,         /* Prod Date Code       */~
                                L40240          /* Sort Sequence Number */
              goto L40260

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40240:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Planning Department Code  :",                ~
               at (04,30), fac(lfac$(1%)), pl_dept$             , ch(03),~
               at (04,40), fac(hex(84)), pl_dept_d$             , ch(30),~
                                                                         ~
               at (05,02), "Planning Process Code     :",                ~
               at (05,30), fac(lfac$(2%)), pl_proc$             , ch(02),~
               at (05,40), fac(hex(84)), pl_proc_d$             , ch(30),~
                                                                         ~
               at (06,02), "Planning Shift Code       :",                ~
               at (06,30), fac(lfac$(3%)), pl_shft$             , ch(02),~
               at (06,40), fac(hex(84)), pl_shft_d$             , ch(30),~
                                                                         ~
               at (07,02), "Planning Model Code       :",                ~
               at (07,30), fac(lfac$(4%)), pl_model$            , ch(03),~
               at (07,40), fac(hex(84)), pl_model_d$            , ch(30),~
                                                                         ~
               at (08,02), "Planning Units Code       :",                ~
               at (08,30), fac(lfac$(5%)), pl_units$            , ch(02),~
               at (08,40), fac(hex(84)), pl_units_d$            , ch(30),~
                                                                         ~
               at (09,02), "Planning Units Value      :",                ~
               at (09,30), fac(lfac$(6%)), pl_units_v$          , ch(10),~
                                                                         ~
               at (10,02), "Planning Prod. Date Code  :",                ~
               at (10,30), fac(lfac$(7%)), pl_prod$             , ch(01),~
               at (10,40), fac(hex(84)), pl_prod_d$             , ch(30),~
                                                                         ~
               at (11,02), "Planning Sort Sequence No.:",                ~
               at (11,30), fac(lfac$(8%)), pl_seq$              , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% = 2% then gosub convert_data

               if keyhit% < 6% or keyhit% > 11% then goto L40770
                  tab% = keyhit% - 5%
                  gosub display_codes
                  goto L40070

L40770:        if keyhit% <> 15% then goto L40810
                  call "PRNTSCRN"
                  goto L40260

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41020     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (14)Print Report"
            pf$(2) = "(4)Previous Field  (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "(5)Copy Data       (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Exit Program"
            pfkeys$ = hex(0102030405060708090a0bffffff0e0f1000)
            if fieldnr% = 1% then L40980
                str(pf$(1%),64%) = " "    : str(pfkeys$,14%,1%) = hex(ff)
/* CR2199 */    str(pf$(2%),1%,18%) = " " : str(pfkeys$, 3%,1%) = hex(ff)
                str(pf$(3%),1%,14%) = " " : str(pfkeys$, 5%,1%) = hex(ff)
                str(pf$(3%),64%) = " "    : str(pfkeys$,16%,1%) = hex(ff)
L40980:     if fieldnr% > 1% then L41000
/* CR2199 */
             str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
             pf$(2) = "(3)Delete Data     (7)Process Codes     " &        ~
                      "(10)Units Codes        (15)Print Screen"

L41000: return

L41020: if fieldnr% > 0% then L41140  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (12)Delete Rec. "
            pf$(2) = "                   (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "                   (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Update Data "
            pfkeys$ = hex(01ffffffff060708090a0b0cff0e0f1000)
            if rec% = 1% then return
               str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
        return

L41140:
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

        deffn'102(fieldnr%, edit%)
L42070:       gosub set_pf2

              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42210,         /* Report Selection     */~
                                L42210,         /* Beginning Code       */~
                                L42210,         /* Ending Code          */~
                                L42210,         /* Beginning Model Code */~
                                L42210          /* Ending Model Code    */

              goto L42240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,57), "(Report) Today:",                            ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Report Selection 1,2, or 3:",                ~
               at (04,30), fac(lfac$(1%)), sel$                 , ch(01),~
               at (04,40), fac(hex(84)), sel_d$                 , ch(30),~
                                                                         ~
               at (05,02), "Beginning Code Value, ALL :",                ~
               at (05,30), fac(lfac$(2%)), bg_code$             , ch(03),~
               at (05,40), fac(hex(84)), bg_code_d$             , ch(30),~
                                                                         ~
               at (06,02), "Ending Code Value         :",                ~
               at (06,30), fac(lfac$(3%)), ed_code$             , ch(03),~
               at (06,40), fac(hex(84)), ed_code_d$             , ch(30),~
                                                                         ~
               at (07,02), "Beginning Model Code, ALL :",                ~
               at (07,30), fac(lfac$(4%)), bg_mod$              , ch(03),~
               at (07,40), fac(hex(84)), bg_mod_d$              , ch(30),~
                                                                         ~
               at (08,02), "Ending Model Code         :",                ~
               at (08,30), fac(lfac$(5%)), ed_mod$              , ch(03),~
               at (08,40), fac(hex(84)), ed_mod_d$              , ch(30),~
                                                                         ~
               at (12,27), "     Report Selections      ",               ~
               at (13,27), "----------------------------",               ~
               at (14,27), " (1) Sort by Department Code",               ~
               at (15,27), " (2) Sort by Shift Code     ",               ~
               at (16,27), " (3) Sort by Units Code     ",               ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% < 6% or keyhit% > 11% then goto L42700
                  tab% = keyhit% - 5%
                  gosub display_codes
                  goto L42070

L42700:        if keyhit% <> 15% then goto L42740
                  call "PRNTSCRN"
                  goto L42240

L42740:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42930     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes                         "
            pf$(2) = "(4)Previous Field  (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "                   (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Exit Program"
            pfkeys$ = hex(01ffff04ff060708090a0bffff0e0f1000)
            if fieldnr% = 1% then L42890
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42890:     if fieldnr% > 1% then L42910
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42910: return

L42930: if fieldnr% > 0% then L43020  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (14)Print Report"
            pf$(2) = "                   (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "                   (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Exit Program"
            pfkeys$ = hex(01ffffffff060708090a0b0cff0e0f1000)
        return
L43020:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *                  C o p y    S c r e e n                   *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
L44050:       gosub set_pf3

              gosub'070(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L44170,         /* From/To Department Cd*/~
                                L44170,         /* From/To Process Code */~
                                L44170,         /* From/To Shift Code   */~
                                L44170          /* From/To Model Code   */
              goto L44200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,57), "(*Copy*) Today:",                            ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Copy From Department   :",                   ~
               at (04,30), fac(lfac$(1%)), fr_dept$             , ch(03),~
               at (04,40), fac(hex(84)), fr_dept_d$             , ch(30),~
                                                                         ~
               at (05,02), "     To Department     :",                   ~
               at (05,30), fac(lfac$(1%)), to_dept$             , ch(03),~
               at (05,40), fac(hex(84)), to_dept_d$             , ch(30),~
                                                                         ~
               at (06,02), "Copy From Process Code :",                   ~
               at (06,30), fac(lfac$(2%)), fr_proc$             , ch(02),~
               at (06,40), fac(hex(84)), fr_proc_d$             , ch(30),~
                                                                         ~
               at (07,02), "     To Process Code   :",                   ~
               at (07,30), fac(lfac$(2%)), to_proc$             , ch(02),~
               at (07,40), fac(hex(84)), to_proc_d$             , ch(30),~
                                                                         ~
               at (08,02), "Copy From Shift Code   :",                   ~
               at (08,30), fac(lfac$(3%)), fr_shft$             , ch(02),~
               at (08,40), fac(hex(84)), fr_shft_d$             , ch(30),~
                                                                         ~
               at (09,02), "     To Shift Code     :",                   ~
               at (09,30), fac(lfac$(3%)), to_shft$             , ch(02),~
               at (09,40), fac(hex(84)), to_shft_d$             , ch(30),~
                                                                         ~
               at (10,02), "Copy From Model Code   :",                   ~
               at (10,30), fac(lfac$(4%)), fr_model$            , ch(03),~
               at (10,40), fac(hex(84)), fr_model_d$            , ch(30),~
                                                                         ~
               at (11,02), "     To Model Code     :",                   ~
               at (11,30), fac(lfac$(4%)), to_model$            , ch(03),~
               at (11,40), fac(hex(84)), to_model_d$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% < 6% or keyhit% > 11% then goto L44710
                  tab% = keyhit% - 5%
                  gosub display_codes
                  goto L44050

L44710:        if keyhit% <> 15% then goto L44750
                  call "PRNTSCRN"
                  goto L44200

L44750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L44940     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes                         "
            pf$(2) = "(4)Previous Field  (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "                   (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Exit Program"
            pfkeys$ = hex(01ffff04ff060708090a0bffff0e0f1000)
            if fieldnr% = 1% then L44900
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L44900:     if fieldnr% > 1% then L44920
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L44920: return

L44940: if fieldnr% > 0% then L45030  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (14)Copy Data   "
            pf$(2) = "                   (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "                   (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Exit Program"
            pfkeys$ = hex(01ffffffff060708090a0b0cff0e0f1000)
        return
L45030:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return
        
        REM *************************************************************~
            *              D e l e t e    S c r e e n                   *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
L54050:       gosub set_pf4

              gosub'074(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L54170,         /* Delete Department Cd*/~
                                L54170,         /* Delete Process Code */~
                                L54170,         /* Delete Shift Code   */~
                                L54170          /* Delete Model Code   */
              goto L54200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L54170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L54200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,55), "(*DELETE*) Today:",                          ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Delete Department   :",                   ~
               at (04,30), fac(lfac$(1%)), dl_dept$             , ch(03),~
               at (04,40), fac(hex(84)), dl_dept_d$             , ch(30),~
                                                                         ~
               at (06,02), "Delete Process Code :",                   ~
               at (06,30), fac(lfac$(2%)), dl_proc$             , ch(02),~
               at (06,40), fac(hex(84)), dl_proc_d$             , ch(30),~
                                                                         ~
               at (08,02), "Delete Shift Code   :",                      ~
               at (08,30), fac(lfac$(3%)), dl_shft$             , ch(02),~
               at (08,40), fac(hex(84)), dl_shft_d$             , ch(30),~
                                                                         ~
               at (10,02), "Delete Model Code   :",                      ~
               at (10,30), fac(lfac$(4%)), dl_model$            , ch(03),~
               at (10,40), fac(hex(84)), dl_model_d$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% < 6% or keyhit% > 11% then goto L54710
                  tab% = keyhit% - 5%
                  gosub display_codes
                  goto L54050

L54710:        if keyhit% <> 15% then goto L54750
                  call "PRNTSCRN"
                  goto L54200

L54750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
        if edit% = 2% then L54940     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes                         "
            pf$(2) = "(4)Previous Field  (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "                   (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Exit Program"
            pfkeys$ = hex(01ffff04ff060708090a0bffff0e0f1000)
            if fieldnr% = 1% then L54900
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L54900:     if fieldnr% > 1% then L54920
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L54920: return

L54940: if fieldnr% > 0% then L55030  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (6)Department Codes  " &        ~
                     "(9)Shift Codes         (14)Delete Data "
            pf$(2) = "                   (7)Process Codes     " &        ~
                     "(10)Units Codes        (15)Print Screen"
            pf$(3) = "                   (8)Model Codes       " &        ~
                     "(11)Prod Date Codes    (16)Exit Program"
            pfkeys$ = hex(01ffffffff060708090a0b0cff0e0f1000)
        return
L55030:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50180,         /* Department Code       */ ~
                              L50340,         /* Process Code          */ ~
                              L50500,         /* Shift Code            */ ~
                              L50660,         /* Model Code            */ ~
                              L50860,         /* Product Units Code    */ ~
                              L51080,         /* Product Units Value   */ ~
                              L51210,         /* Production Date Code  */ ~
                              L51370          /* Sort Sequence No.     */
            return

L50180: REM Department Code                       PL_DEPT$, PL_DEPT_D$
            if pl_dept$ <> " " then goto L50220
               pl_dept$ = "000"

L50220:     convert pl_dept$ to x%, data goto L50300

            convert x% to pl_dept$, pic(000)
            code$ = pl_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L50300
            pl_dept_d$ = desc$
        return
L50300:     errormsg$ = "(Error) - Invalid Department Code ??"
            init(" ") pl_dept$, pl_dept_d$
        return

L50340: REM Processing Code                       PL_PROC$, PL_PROC_D$
            if pl_proc$ <> " " then goto L50380
               pl_proc$ = "01"

L50380:     convert pl_proc$ to x%, data goto L50410

            convert x% to pl_proc$, pic(00)
L50410:     code$ = pl_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L50460
            pl_proc_d$ = desc$
        return
L50460:     errormsg$ = "(Error) - Invalid Processing Code ??"
            init(" ") pl_proc$, pl_proc_d$
        return

L50500: REM Planning Shift Code                   PL_SHFT$, PL_SHFT_D$
            if pl_shft$ <> " " then goto L50540
               pl_shft$ = "01"

L50540:     convert pl_shft$ to x%, data goto L50570

            convert x% to pl_shft$, pic(00)
L50570:     code$ = pl_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L50620
            pl_shft_d$ = desc$
        return
L50620:     errormsg$ = "(Error) - Invalid Shift Code ??"
            init(" ") pl_shft$, pl_shft_d$
        return

L50660: REM Product Model Code                    PL_MODEL$, PL_MODEL_D$
            if pl_model$ <> " " then goto L50700
               pl_model$ = "000"

L50700:     if str(pl_model$,1%,3%) <> "000" then goto L50740
               pl_model_d$ = "(ALL) - Models"
               return

L50740:     if str(pl_model$,1%,1%) = "9" then L50745
            goto L50745      /* EWD009 */
            convert pl_model$ to x%, data goto L50820

            convert x% to pl_model$, pic(000)
L50745:     code$ = pl_model$
            tab%  = 3% : gosub check_code
            if code% = 0% then goto L50820
            pl_model_d$ = desc$
        return
L50820:     errormsg$ = "(Error) - Invalid Product Model Code ??"
            init(" ") pl_model$, pl_model_d$
        return

L50860: REM Planning Units Code                   PL_UNITS$,PL_UNITS_D$
            if pl_units$ <> " " then goto L50900
               pl_units$ = "01"

L50900:     convert pl_units$ to x%, data goto L50930

            convert x% to pl_units$, pic(00)
L50930:     code$ = pl_units$
            tab%  = 5% : gosub check_code
            if code% = 0% then goto L51040
            pl_units_d$ = desc$
            if edit% = 2% then return

               if rec% = 1% then return
               gosub dataload
               if rec% = 0% then return
               fieldnr% = 9%
        return
L51040:     errormsg$ = "(Error) - Invalid Planning Units Code ??"
            init(" ") pl_units$, pl_units_d$
        return

L51080: REM Planning Units Value               PL_UNITS_V$, PL_UNITS
            if pl_units_v$ <> " " then goto L51120
               pl_units_v$ = "1.0"

L51120:     convert pl_units_v$ to pl_units, data goto L51170

            convert pl_units to pl_units_v$, pic(####.####-)

        return
L51170:     errormsg$ = "(Error) - Invalid Planning Units Value ??"
            pl_units_v$ = " "
        return

L51210: REM Planning Production Date Code         PL_PROD$,PL_PROD_D$
            if pl_prod$ <> " " then goto L51250
               pl_prod$ = "0"

L51250:     convert pl_prod$ to x%, data goto L51280

            convert x% to pl_prod$, pic(0)
L51280:     code$ = pl_prod$
            tab%  = 6% : gosub check_code
            if code% = 0% then goto L51330
            pl_prod_d$ = desc$
        return
L51330:     errormsg$ = "(Error) - Invalid Production Date Code ??"
            init(" ") pl_prod$, pl_prod_d$
        return

L51370: REM Planning Sort Sequence Number         PL_SEQ$, PL_SEQ%
            if pl_seq$ <> " " then goto L51410
               pl_seq$ = "000"

L51410:     convert pl_seq$ to pl_seq%, data goto L51480

            if pl_seq% < 0% or pl_seq% > 255% then goto L51480

            convert pl_seq% to pl_seq$, pic(000)

        return
L51480:     errormsg$ = "(Error) - Invalid Sort Seq. No.(0 - 255)?"
            pl_seq$ = "000"
        return

        REM *************************************************************~
            *               R e p o r t   E d i t s                     *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52180,         /* Report Selection      */ ~
                              L52340,         /* Begin Code or All     */ ~
                              L52500,         /* End Code              */ ~
                              L52740,         /* Begin Model or All    */ ~
                              L52970          /* Ending Model          */
        return

L52180: REM Report Selection                      SEL$, SEL_D$
            if sel$ <> " " then goto L52220
               sel$ = "1"

L52220:     convert sel$ to sel%, data goto L52300

            if sel% < 1% or sel% > 3% then goto L52300
               if sel% = 1% then sel_d$ = "Sort by Department Code "
               if sel% = 2% then sel_d$ = "Sort by Shift Code  "
               if sel% = 3% then sel_d$ = "Sort by Units Code  "
        return
L52300:     errormsg$ = "(Error) - Invalid Report Selection 1, 2, 3 ??"
            init(" ") sel$, sel_d$
        return

L52340: REM Beginning Code                        BG_CODE$,BG_CODE$_D$
            if bg_code$ <> " " then goto L52371
               bg_code$ = "ALL"

L52371:     if bg_code$ <> "ALL" then goto L52380
               if sel% = 1% then bg_code_d$ = "All (Departments Codes)"
               if sel% = 2% then bg_code_d$ = "All (Shift Codes)"
               if sel% = 3% then bg_code_d$ = "All (Units Codes)"
               return
L52380:     if sel% = 1% then tab% = 1%
            if sel% = 2% then tab% = 4%
            if sel% = 3% then tab% = 5%
            code$ = bg_code$
            gosub check_code
            if code% = 0% then goto L52460
            bg_code_d$ = desc$
        return
L52460:     if sel% = 1% then errormsg$="(Error) - Invalid Dept. Code ??"
            if sel% = 2% then errormsg$="(Error) - Invalid Shift Code ??"
            if sel% = 3% then errormsg$="(Error) - Invalid Units Code ??"
            init(" ") bg_code$, bg_code_d$
        return

L52500: REM Ending Code                           ED_CODE$,ED_CODE_D$
            if ed_code$ <> " " then goto L52550
               ed_code$ = bg_code$

L52550:     if ed_code$ <> "ALL" then goto L52600
               if sel% = 1% then ed_code_d$ = "All (Departments Codes)"
               if sel% = 2% then ed_code_d$ = "All (Shift Codes)"
               if sel% = 3% then ed_code_d$ = "All (Units Codes)"
               return
L52600:     if sel% = 1% then tab% = 1%
            if sel% = 2% then tab% = 4%
            if sel% = 3% then tab% = 3%
            code$ = ed_code$
            gosub check_code
            if code% = 0% then goto L52680
            ed_code_d$ = desc$
        return
L52680:     if sel% = 1% then errormsg$="(Error) - Invalid Dept. Code ??"
            if sel% = 2% then errormsg$="(Error) - Invalid Shift Code ??"
            if sel% = 3% then errormsg$="(Error) - Invalid Units Code ??"
            init(" ") ed_code$, ed_code_d$
        return

L52740: REM Beginning Model Code                  BG_MOD$,BG_MOD_D$
            if bg_mod$ <> " " then goto L52780
               bg_mod$ = "ALL"

L52780:     if bg_mod$ <> "ALL" then goto L52830
               bg_mod_d$ = "All (Model Codes)"
               return
L52830:     tab% = 3%
            code$ = bg_mod$
            gosub check_code
            if code% = 0% then goto L52910
            bg_mod_d$ = desc$
        return
L52910:     errormsg$="(Error) - Invalid Beginning Model Code ??"
            init(" ") bg_mod$, bg_mod_d$
        return

L52970: REM Ending Modle Code                     ED_MOD$,ED_MOD_D$
            if ed_mod$ <> " " then goto L53010
               ed_mod$ = bg_mod$

L53010:     if bg_mod$ <> "ALL" then goto L53040
               ed_mod$   = bg_mod$
               ed_mod_d$ = "All (Model Codes)"
               return
L53040:     tab% = 3%
            code$ = ed_mod$
            gosub check_code
            if code% = 0% then goto L53100
            ed_mod_d$ = desc$
        return
L53100:     errormsg$="(Error) - Invalid Ending Model Code ??"
            init(" ") ed_mod$, ed_mod_d$
        return

        REM *************************************************************~
            *                     C o p y   D a t a                     *~
            *************************************************************
        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54060,         /* From/To Department    */ ~
                              L54195,         /* From/To Process Code  */ ~
                              L54330,         /* From/To Shift Code    */ ~
                              L54465          /* From/To Model Code    */
            return

L54060: REM Copy From/To Department Code          FR_DEPT$, FR_DEPT_D$
            if fr_dept$ <> " " then goto L54080
               fr_dept$ = "000"

L54080:     convert fr_dept$ to x%, data goto L54175

            convert x% to fr_dept$, pic(000)
            code$ = fr_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L54175
            fr_dept_d$ = desc$

            if to_dept$ <> " " then goto L54135
               to_dept$ = fr_dept$

L54135:     convert to_dept$ to x%, data goto L54175

            convert x% to to_dept$, pic(000)
            code$ = to_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L54175
            to_dept_d$ = desc$
        return
L54175:     errormsg$ = "(Error) - Invalid Department Code ??"
            init(" ") fr_dept$, fr_dept_d$, to_dept$, to_dept_d$
        return

L54195: REM Copy From/To Process Code             FR_PROC$, FR_PROC_D$
            if fr_proc$ <> " " then goto L54215
               fr_proc$ = "01"

L54215:     convert fr_proc$ to x%, data goto L54230

            convert x% to fr_proc$, pic(00)
L54230:     code$ = fr_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L54310
            fr_proc_d$ = desc$

            if to_proc$ <> " " then goto L54270
               to_proc$ = fr_proc$

L54270:     convert to_proc$ to x%, data goto L54285

            convert x% to to_proc$, pic(00)
L54285:     code$ = to_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L54310
            to_proc_d$ = desc$
        return
L54310:     errormsg$ = "(Error) - Invalid Processing Code ??"
            init(" ") fr_proc$, fr_proc_d$, to_proc$, to_proc_d$
        return

L54330: REM Copy From/To Shift Code               FR_SHFT$, FR_SHFT_D$
            if fr_shft$ <> " " then goto L54350
               fr_shft$ = "01"

L54350:     convert fr_shft$ to x%, data goto L54365

            convert x% to fr_shft$, pic(00)
L54365:     code$ = fr_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L54445
            fr_shft_d$ = desc$

            if to_shft$ <> " " then goto L54405
               to_shft$ = fr_shft$

L54405:     convert to_shft$ to x%, data goto L54420

            convert x% to to_shft$, pic(00)
L54420:     code$ = to_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L54445
            to_shft_d$ = desc$
        return
L54445:     errormsg$ = "(Error) - Invalid Shift Code ??"
            init(" ") fr_shft$, fr_shft_d$, to_shft$, to_shft_d$
        return

L54465: REM Copy From/To Model                    FR_MODEL$, FR_MODEL_D$
            if fr_model$ <> " " then goto L54485
               fr_model$ = "000"

L54485:     if str(fr_model$,1%,3%) <> "000" then goto L54515
               fr_model_d$ = "(ALL) - Models"
               to_model$ = fr_model$
               to_model_d$ = fr_model_d$
               goto L54660

L54515:     if str(fr_model$,1%,1%) = "9" then goto L54520
            goto L54520      /* EWD009 */
REM               convert fr_model$ to x%, data goto L54645

REM               convert x% to fr_model$, pic(000)
L54520:     code$ = fr_model$
            tab%  = 3% : gosub check_code
            if code% = 0% then goto L54645
            fr_model_d$ = desc$

            if to_model$ <> " " then goto L54570
               to_model$ = fr_model$

L54570:     if str(fr_model$,1%,3%) <> "000" then goto L54600
               fr_model_d$ = "(ALL) - Models"
               to_model$ = fr_model$
               to_model_d$ = fr_model_d$
               goto L54660

L54600:     if str(to_model$,1%,1%) = "9" then goto L54605
REM            goto L54645      /* EWD009 */
REM               convert to_model$ to x%, data goto L54645

REM               convert x% to to_model$, pic(000)
L54605:     code$ = to_model$
            tab%  = 3% : gosub check_code
            if code% = 0% then goto L54645
            to_model_d$ = desc$
            goto L54660
        return
L54645:     errormsg$ = "(Error) - Invalid Product Model Code ??"
            init(" ") fr_model$, fr_model_d$, to_model$, to_model_d$
        return
L54660:     if fr_dept$ = to_dept$ and fr_proc$ = to_proc$ and           ~
               fr_shft$ = to_shft$ and fr_model$ = to_model$ then        ~
                                                   goto L54680
        return
L54680:     errormsg$ = "(Error) - Invalid Copy Selections?"
            gosub initialize_variables
            return clear all
            goto copy_input
  
        REM *************************************************************~
            *                D e l e t e   D a t a                      *~
            *************************************************************

        deffn'154(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L64060,         /* Delete Department    */ ~
                              L64195,         /* Delete Process Code  */ ~
                              L64330,         /* Delete Shift Code    */ ~
                              L64465          /* Delete Model Code    */
            return

L64060: REM Delete Department Code          DL_DEPT$, DL_DEPT_D$
            if dl_dept$ <> " " then goto L64080
               dl_dept$ = "000"

L64080:     convert dl_dept$ to x%, data goto L64175

            convert x% to dl_dept$, pic(000)
            code$ = dl_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L64175
            dl_dept_d$ = desc$

        return
L64175:     errormsg$ = "(Error) - Invalid Department Code ??"
            init(" ") dl_dept$, dl_dept_d$
        return

L64195: REM Delete Process Code             dl_PROC$, dl_PROC_D$
            dl_proc_d$ = "ALL Process Codes"
            if dl_proc$ = "AL" then return
            
            if dl_proc$ <> " " then goto L64215
               dl_proc$ = "01"

L64215:     convert dl_proc$ to x%, data goto L64230

            convert x% to dl_proc$, pic(00)
L64230:     code$ = dl_proc$
            tab%  = 2% : gosub check_code
            if code% = 0% then goto L64310
            dl_proc_d$ = desc$
        return
L64310:     errormsg$ = "(Error) - Invalid Processing Code ??"
            init(" ") dl_proc$, dl_proc_d$
        return

L64330: REM Delete Shift Code               dl_SHFT$, dl_SHFT_D$
            dl_shft_d$ = "ALL Shifts"
            if dl_shft$ = "AL" then return
            
            if dl_shft$ <> " " then goto L64350
               dl_shft$ = "01"

L64350:     convert dl_shft$ to x%, data goto L64365

            convert x% to dl_shft$, pic(00)
L64365:     code$ = dl_shft$
            tab%  = 4% : gosub check_code
            if code% = 0% then goto L64445
            dl_shft_d$ = desc$
        return
L64445:     errormsg$ = "(Error) - Invalid Shift Code ??"
            init(" ") dl_shft$, dl_shft_d$
        return

L64465: REM Delete Model                    dl_MODEL$, dl_MODEL_D$
            if dl_model$ <> " " then goto L64520
               dl_model$ = "000"

L64520:     code$ = dl_model$
            tab%  = 3% : gosub check_code
            if code% = 0% then goto L64645
            dl_model_d$ = desc$

            goto L64660
        return
L64645:     errormsg$ = "(Error) - Invalid Product Model Code ??"
            init(" ") dl_model$, dl_model_d$, to_model$, to_model_d$
        return
L64660:
        return
            
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
L55070: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--!
        %!                                                               ~
        ~                                                                 ~
        ~  !
L55130: %!######## @ ########               #############################~
        ~###############################                      Page: ####  ~
        ~  !
L55160: %!Selection :   # ##############################                 ~
        ~                                                                 ~
        ~  !
L55190: %!Begin Code: ### ##############################                 ~
        ~                    End Code: ### ############################## ~
        ~  !
L55220: %!Code##############################!##########!##########!######~
        ~####!###########################!Plan Units!Production Date Cd !S~
        ~eq!
L55250: %!### ##############################!##########!##########!######~
        ~####!###########################!##########!###################!#~
        ~##!
L55280: %!--- ------------------------------!----------!----------!------~
        ~----!---------------------------!----------!-------------------!-~
        ~--!
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        display_codes
            call "APCPLN1B" (tab%, #3)
        return

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           if sel% = 1% then str(title$,41%,20%) =                       ~
                                       "(" & str(pl_dept_d$,1%,18%) & ")"
           if sel% = 2% then str(title$,41%,20%) =                       ~
                                      "(" & str(pl_shft_d$,1%,18%) & ")"
           if sel% = 3% then str(title$,41%,20%) =                       ~
                                      "(" & str(pl_units_d$,1%,18%) & ")"
           print using L55040
           print using L55130, date$, runtime$, title$, pageno%
           print using L55160, sel$, sel_d$
           print using L55190, bg_code$, bg_code_d$, ed_code$, ed_code_d$
           print using L55070
           print using L55220, hdr1$, hdr2$, hdr3$, hdr4$, hdr5$
           lcntr% = 6%
        return

        print_detail
           if lcntr% > 59% then gosub print_header
              gosub format_detail
              if brk_flg% <> 0% and lcntr% <> 6% then gosub print_header

              print using L55280
              print using L55250, dtl1$, dtl2$, dtl3$, dtl4$, dtl5$,      ~
                                 dtl6$, dtl7$, dtl8$, pl_seq$
              lcntr% = lcntr% + 2%
        return

        format_detail
           brk_flg% = 0%
           if sel% <> 1% then goto L60500
              dtl1$ = pl_dept$
              dtl2$ = pl_dept_d$
              dtl3$ = "    " & pl_proc$  & "    "
              dtl4$ = "    " & pl_shft$  & "    "
              dtl5$ = "   "  & pl_model$ & "    "
              if sav_code$ = pl_dept$ then goto format_done
                 sav_code$ = pl_dept$
                 brk_flg% = 1%
              goto format_done
L60500:    if sel% <> 2% then goto L60600
              dtl1$ = pl_shft$
              dtl2$ = pl_shft_d$
              dtl3$ = "   "   & pl_dept$  & "    "
              dtl4$ = "    "  & pl_proc$  & "    "
              dtl5$ = "   "   & pl_model$ & "    "
              if sav_code$ = pl_shft$ then goto format_done
                 sav_code$ = pl_shft$
                 brk_flg% = 1%
              goto format_done
L60600:    dtl1$ = pl_units$
           dtl2$ = pl_units_d$
           dtl3$ = "   "  & pl_model$ & "    "
           dtl4$ = "    " & pl_shft$  & "    "
           dtl5$ = "   "  & pl_dept$  & "    "
           if sav_code$ = pl_units$ then goto format_done
              sav_code$ = pl_units$
              brk_flg% = 1%
        format_done
           if sel% = 3% then goto L60730
              str(dtl6$,1%,3%)  = pl_units$ & "-"
              str(dtl6$,4%,24%) = pl_units_d$
              goto L60760
L60730:    str(dtl6$,1%,3%)  = pl_proc$  & "-"
           str(dtl6$,4%,24%) = pl_proc_d$

L60760:    dtl7$ = pl_units_v$
           str(dtl8$,1%,2%)  = pl_prod$ & "-"
           str(dtl8$,3%,17%) = pl_prod_d$
        return

        generate_report
           init(" ") pl_key$, sav_code$
           kk% = sel% - 1%
           if bg_code$ = "ALL" then goto L60910
              if sel% = 1% then str(pl_key$,1%,3%) = bg_code$
              if sel% = 2% then str(pl_key$,1%,2%) = str(bg_code$,1%,2%)
              if sel% = 3% then str(pl_key$,1%,2%) = str(bg_code$,1%,2%)
              if sel% = 3% and bg_mod$ <> "ALL" then                     ~
                                str(pl_key$,3%,3%) = bg_mod$

L60910:    read #1,key kk% > pl_key$, eod goto gen_done
           goto L60950
        gen_nxt
           read #1, eod goto gen_done
L60950:    gosub dataload_rpt
           if bg_code$ = "ALL" then goto gen_process
              on sel% goto L60980, L61000, L61020
L60980:          if pl_dept$ > ed_code$ then goto gen_done
                    goto gen_process
L61000:          if pl_shft$ > str(ed_code$,1%,2%) then goto gen_done
                    goto gen_process
L61020:          if pl_units$ > str(ed_code$,1%,2%) then goto gen_done
        gen_process
           if bg_mod$ = "ALL" then goto L61070
              if pl_model$ < bg_mod$ or pl_model$ > ed_mod$ then         ~
                                                             goto gen_nxt
L61070:       gosub print_detail
            goto gen_nxt
        gen_done
            print using L55040
        return

        check_code
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = tab$(tab%)
           str(readkey$,10%,15%)  = code$
           read #3,key = readkey$, using L61190, desc$, eod goto L61220
L61190:        FMT POS(25), CH(30)
           code% = 1%
        return
L61220:    errormsg$ = "(Error) - Invalid Code Value Entered ??"
        return

        convert_data
           if userid$ <> "RHH" then return

           call "SHOSTAT" ("CONVERTING DEPARTMENT DATA")
           pl_seq% = 0%
           init(" ") pl_key$
        convert_next
           read #1,hold,key > pl_key$, using L61340, pl_rec$,             ~
                                                    eod goto convert_done
L61340:       FMT CH(32)
           pl_key$ = str(pl_rec$,11%,12%)
           delete #1
           put #1, using L61380, str(pl_rec$,1%,31%), pl_seq%
L61380:       FMT CH(31), BI(1)
           write #1, eod goto L61450
           goto convert_next

        convert_done
        return clear all
        goto inputmode
L61450:     call "SHOSTAT" ("CONVERT ERROR - "& pl_key$) : stop
            goto convert_next

        copy_data
            call "SHOSTAT" ("Copying Data For Dept ("&fr_dept$&")")

            init(" ") pl_key$, pl_rec$, cc_key$
            str(pl_key$,1%,3%) = fr_dept$
            str(pl_key$,4%,2%) = fr_proc$
            str(pl_key$,6%,2%) = fr_shft$
            if fr_model$ = "000" then goto copy_next
               str(pl_key$,8%,3%) = fr_model$
        copy_next
            read #1,hold,key > pl_key$, using L61600, pl_rec$,            ~
                                                eod goto copy_done
L61600:        FMT CH(32)
            pl_key$ = str(pl_rec$,11%,12%)
            if str(pl_key$,1%,3%) <> fr_dept$ then goto copy_done
            if str(pl_key$,4%,2%) <> fr_proc$ then goto copy_done
            if str(pl_key$,6%,2%) <> fr_shft$ then goto copy_done
            if fr_model$ = "000" then goto L61710
               if str(pl_key$,8%,3%) <> fr_model$ then goto copy_done
                  str(pl_rec$,1%,3%)  = to_model$
                  str(pl_rec$,6%,3%)  = to_model$
                  str(pl_rec$,18%,3%) = to_model$

L61710:     str(pl_rec$, 9%,2%) = to_shft$
            str(pl_rec$,11%,3%) = to_dept$
            str(pl_rec$,14%,2%) = to_proc$
            str(pl_rec$,16%,2%) = to_shft$
            cc_key$ = str(pl_rec$,11%,12%)
            read #1,hold,key = cc_key$, using L61600, cc_rec$,            ~
                                              eod goto L61800
               delete #1

L61800:     write #1, using L61600, pl_rec$, eod goto L61850
            goto copy_next
        copy_done
        return clear all
        goto inputmode
L61850:     call "SHOSTAT" ("Error - Copying Data --> " & cc_key$)
            stop
            goto copy_next

        delete_data

            call "SHOSTAT" ("Delete Data For Dept ("&dl_dept$&")")

            init(" ") pl_key$, pl_rec$, cc_key$
            dl_cnt% = 0%
            
            str(pl_key$,1%,3%) = dl_dept$
            if dl_proc$ = "AL" then goto delete_next
              str(pl_key$,4%,2%) = dl_proc$
            if dl_shft$ = "AL" then goto delete_next
              str(pl_key$,6%,2%) = dl_shft$
            str(pl_key$,8%,3%) = dl_model$
               
        delete_next
            read #1,hold,key > pl_key$, using L71600, pl_rec$,            ~
                                                eod goto delete_done
L71600:        FMT CH(32)
            pl_key$ = str(pl_rec$,11%,12%)
            
            if str(pl_key$,1%,3%) <> dl_dept$ then goto delete_done
            if dl_proc$ = "AL" then goto L71700
            if str(pl_key$,4%,2%) <> dl_proc$ then goto delete_next
L71700:
            if dl_shft$ = "AL" then goto L71725
            if str(pl_key$,6%,2%) <> dl_shft$ then goto delete_next
L71725:
            if str(pl_key$,8%,3%) <> dl_model$ then goto delete_next

            cc_key$ = str(pl_rec$,11%,12%)
            read #1,hold,key = cc_key$, using L71600, cc_rec$,            ~
                                              eod goto L71800
               delete #1
               dl_cnt% = dl_cnt% + 1%
   
L71800:     goto delete_next
        delete_done
            
            convert dl_cnt% to dl_cnt$, PIC(000000)
            
            hdr$     = "**** Number of Records Deleted ****"
            msg$(1%) = "There were " & dl_cnt$ & " deleted records"
            msg$(2%) = " "
            msg$(3%) = "Press any key to return"

            k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
            
        return clear all
        goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
