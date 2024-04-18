        REM *************************************************************~
            *                                                           *~
            *   AAA   M   M  TTTTT  BBBB    OOO   M   M  IIIII  FFFFF   *~
            *  A   A  MM MM    T    B   B  O   O  MM MM    I    F       *~
            *  AAAAA  M M M    T    BBBB   O   O  M M M    I    FFFF    *~
            *  A   A  M   M    T    B   B  O   O  M   M    I    F       *~
            *  A   A  M   M    T    BBBB    OOO   M   M  IIIII  F       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AMTBOMIF - Before using this Program, it is imperative    *~
            *            that the user has previously entered and       *~
            *            defined the necessary Tables with their        *~
            *            respective information to be used for Valid-   *~
            *            ation. The Tables can be defined and entered   *~
            *            using the Caelus Program 'GENCDSIN'.           *~
            *                                                           *~
            *            This Program allows the User to 'Select'       *~
            *            specific values from the 'Table Files' which   *~
            *            are valid for the applicable FIELD ONE value   *~
            *            (ie. Model Number). In order to save the User  *~
            *            time and disk space, the User will be asked    *~
            *            whether the value being entered should be      *~
            *            'INCLUDED' into the Valid values for FIELD ONE,*~
            *            or should be 'EXCLUDED' from the Valid values  *~
            *            for FIELD ONE. The 'EXCLUDED' option should    *~
            *            only be used if 'ALL VALUES' in the associated *~
            *            Table  file are acceptable values for the      *~
            *            specified FIELD ONE 'Except' the Values which  *~
            *            are 'EXCLUDED'. This will eliminate some       *~
            *            repetitive entries.                            *~
            *                                                           *~
            *            Another feature of this Program is that when   *~
            *            entering the Valid Values for FIELD ONE, the   *~
            *            User can use the 'Wild Card' Plus (+). An      *~
            *            example of the use of this feature would be if *~
            *            the Valid Values for this FIELD ONE entry are  *~
            *            all Values in the associated 'TABLE' which     *~
            *            begin with "0". This would be entered in the   *~
            *            Valid Value Field as 0+.                       *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/23/89 ! Original                                 ! WKB *~
            * 11/28/89 ! Mod for non-values of Phantom flds (30+) ! WKB *~
            * 05/02/90 ! FINAL MODS FOR COMPLETION                ! RHH *~
            * 10/18/90 ! Modification for Part No. Change Convert ! WKB *~
            *          ! Convert Field 8 - 08(Old) to 09(New) (H) !     *~
            *          ! Convert Field 7 - 07(Old) to 08(New) (W) !     *~
            *          ! Line No. (61100) Hidden PF(9)            !     *~
            * 10/23/90 ! New Field - Replace Price with PHANTOM$  ! RHH *~
            *          !    Sub Assembly Part Number and BOM Id.  !     *~
            * 02/18/94 ! Re-Write old version and Clean-Up Report.! RHH *~
            * 11/28/97 ! Review for Upgrade to R6.04.03           ! RHH *~
            * 09/10/02 ! (EWD001) Mod to add new copy/delete      ! CMG *~
            *          !             screen.                      !     *~
            * 03/02/04 ! (EWD002) Mod to add security to control  ! CMG *~
            *          !             what users can update data.  !     *~
            * 05/05/16 ! SR74702  Mod to strip "|" from phantom$  ! PWW *~
            *          !             and bom$.                    !     *~
            *************************************************************

        dim                                                              ~
            beg_mod$3, end_mod$3,        /* Beg/End Model Code, or All */~
            beg_fld$3, end_fld$3,        /* Beg/End Field Number       */~
            bom$3,                       /* BOM ID                     */~
            bom_key$31,                  /* BOM Primary Key            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            defkey$36,                   /* Definition Key             */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            field$2, prt_field$2,        /* Field Number               */~
            i$(24%)80,                   /* Screen Image               */~
            field_name$20,               /* Field Name                 */~
            incl$40,                     /* 'Include' or 'Exclude'     */~
            gencode$24,                  /* Gencode Key                */~
            file_name$(12%)8,            /* File Name                  */~
            include$1,                   /* Include/Exclude (I/E)      */~
            filler$05,                   /* Filler Space               */~
            fill$9,                      /* Filler                     */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            model$15,                    /* Model Number               */~
            mod_desc$30,                 /* Model Description          */~
            oldkey$32,                   /* Old Key                    */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            phantom$25,                  /* Phantom Assembly Part Num. */~
            price$8,                     /* Component Price            */~
            prt_model$3,                 /* APC MODEL NUMBER           */~
            printer$30,                  /* Printer Display            */~
            rpt_time$8,                  /* Run Time for Report        */~
            print_title$40,              /* Report Title               */~
            company$40,                  /* Company Name               */~
            sav_model$15, sav_field$2,   /* Model Number for Report    */~
            screen$20,                   /* Screen Display             */~
            type_file$(12%)1,            /* File Type                  */~
            table$9,                     /* Table file                 */~
            userid$3,                    /* Current User Id            */~
            val_desc$30,                 /* Value Description          */~
            value$15,                    /* Values                     */~
            valid_yn$(12%)1,             /* Validate Value             */~
            valid_table$(12%)9,          /* Validation Table Name      */~
            strip_pipe$30                /* SR74702                    */

        dim hdr$40,                      /* ASKUSER HEADER    (EWD002) */~
            msg$(3%)79                   /* ASKUSER TEXT      (EWD002) */


        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Master Validity Maintenance Util. "
            pname$ = "AMTBOMIF - Rev: R6.04"

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
            * #01 ! AMTBOMIF ! Field Definitions for Item Number        *~
            * #02 ! AMTBOMPM ! Bom Generator Param File                 *~
            * #03 ! GENCODES ! Control System Codes File                *~
            * #05 ! BOMMASTR ! BOM Master File                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize = 120,               ~
                        keypos = 1,    keylen = 32                       ~

            select #2,  "AMTBOMPM",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 2                        ~

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #5,  "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 26,   keylen =  31,                     ~
                        alt key  1, keypos  =     1, keylen =  31

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 25%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%),      0%,150%, rslt$(3%))
            call "OPENCHCK" (#5, fs%(5%),      0%,  0%, rslt$(5%))

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

            for fieldnr% = 1% to  6%
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
                      if keyhit% = 14% then goto report_entry
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
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
                  if keyhit%  =  9% then gosub utility_sub
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 16% then gosub datasave
                  if keyhit% <>  0% then goto  editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
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
            *          R E P O R T   E N T R Y  S C R E E N             *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        report_entry
            gosub initialize_variables

            for fieldnr% = 1% to  2%
L12100:         gosub'061(fieldnr%)        /* Default / Enables */
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
            *            R E P O R T   E D I T   M O D E                *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub generate_report
                  if keyhit% <>  0% then       editpg2
L13100:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  2% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13150:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13150
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13150
                  lastfieldnr% = fieldnr%
            goto L13100

        REM *************************************************************~
            *      U t i l i t y  /  R e p o r t   R o u t i n e s      *~
            *************************************************************

        generate_report
            gosub select_printer
            gosub print_report
            gosub close_printer
        return clear all
        goto inputmode

        datasave
            gosub check_security      /*  (EWD002) - BEG */
            if security% <> 0% then gosub data_save
               gosub security_error
               return clear all
               goto inputmode
        data_save                     /*  (EWD002) - END */
            if oldkey$ = defkey$ then goto L19180
            if oldkey$ = " " then goto L19180
            read #1,hold,key = oldkey$, eod goto L19180
                delete #1
L19180:     gosub dataput
        return clear all
        goto inputmode

        delete_it
            gosub check_security      /*  (EWD002) - BEG */
            if security% <> 0% then gosub delete_it_OK
               gosub security_error
               return clear all
               goto inputmode
        delete_it_OK
            read #1,hold,key = defkey$, eod goto L19250
                delete #1
L19250: return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            return

        deffn'061(fieldnr%)
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
         "Enter Field Number to Define                                 ",~
         "Enter Model Number to Verify                                 ",~
         "Enter Acceptable Values associated with this Model/Field     ",~
         "Enter 'I'(Include),'E'(Exclude),'U'(Upper Limit),'L' (Lower) ",~
         "Enter Screen Display                                         ",~
         "Enter Printer Display                                        ",~
         "Enter Phantom Assembly Part Number. ( No Raw Material No.)   ",~
         "Enter BOM ID Associated with Phantom Assembly                "

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
         "Enter a Valid Beginning and Ending Model Code, or (A)LL?     ",~
         "Enter a Valid Beginning and Ending Field Number, or (A)ll?   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, field_name$, mod_desc$,    ~
                      field$, model$, price$, printer$, screen$, value$, ~
                      val_desc$, filler$, oldkey$, phantom$, fill$, bom$,~
                      bom_key$, beg_mod$, end_mod$, beg_fld$, end_fld$

            include$ = "I" : incl$ = "(Include)"
            test = 0 : page_no% = 0% : lcnt% = 0%
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
            *                 D A T A   L O A D                         *~
            *************************************************************

        dataload
            rec% = 0%
            read #1,key = defkey$, eod goto L30160
                get #1 using L35040, model$, field$, value$, include$,    ~
                                 screen$, printer$, phantom$, bom$, fill$
/*SR74702 + */
            strip_pipe$ = screen$
            gosub strip_pipe
            screen$ = strip_pipe$
            strip_pipe$ = printer$
            gosub strip_pipe
            printer$ = strip_pipe$
/*SR74702 - */


            oldkey$ = defkey$
            if include$ = "I" then incl$ = "Include"
            if include$ = "E" then incl$ = "Exclude"
            if include$ = "U" then incl$ = "Upper Limit"
            if include$ = "L" then incl$ = "Lower Limit"
            rec% = 1%
L30160: return

        REM *************************************************************~
            *              P U T   D A T A   I N T O   F I L E          *~
            *************************************************************

        dataput
            read #1,hold,key = defkey$, eod goto L31080
                delete #1

L31080:     put #1 using L35040, model$, field$, value$, include$,        ~
                         screen$, printer$, phantom$, bom$, fill$
            write #1
        return

        REM *************************************************************~
            *              F O R M A T   S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                 /* FILE: AMTBOMIF                          */~
            CH(15),         /* Model Number                            */~
            CH(2),          /* Definition for elements whose Type = FIE*/~
            CH(15),         /* Value of the field of the Model         */~
            CH(1),          /* Include / Exclude the Value             */~
            CH(20),         /* Verbage to appear on the Screen Displays*/~
            CH(30),         /* Verbage to print on the Hardcopy        */~
            CH(25),         /* Phantom Assembly Part Number            */~
            CH(03),         /* Bom Id Associated with Phantom          */~
            CH(09)          /* Filler Area                             */

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
              on fieldnr% gosub L40230,         /* Field Number      */   ~
                                L40220,         /* Model Number      */   ~
                                L40220,         /* Values            */   ~
                                L40220,         /* Inc/Exc           */   ~
                                L40210,         /* Screen Display    */   ~
                                L40210,         /* Printer Display   */   ~
                                L40230,         /* Phantom Assembly  */   ~
                                L40230          /* Phantom BOM Id    */
              goto L40250

L40210:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40230:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Field Number",                               ~
               at (06,30), fac(lfac$(1%)), field$               , ch(02),~
               at (06,46), fac(hex(8c)), field_name$            , ch(08),~
                                                                         ~
               at (07,02), "Model Number",                               ~
               at (07,30), fac(lfac$(2%)), model$               , ch(15),~
               at (07,46), fac(hex(8c)), mod_desc$              , ch(30),~
                                                                         ~
               at (08,02), "Data Validity",                              ~
               at (08,30), fac(lfac$(3%)), value$               , ch(15),~
               at (08,46), fac(hex(8c)), val_desc$              , ch(30),~
                                                                         ~
               at (09,02), "Include/Exclude (I/E/U/L)",                  ~
               at (09,30), fac(lfac$(4%)), include$             , ch(01),~
               at (09,40), fac(hex(8c)), incl$                  , ch(40),~
                                                                         ~
               at (10,02), "Screen Display",                             ~
               at (10,30), fac(lfac$(5%)), screen$              , ch(20),~
                                                                         ~
               at (11,02), "Printer Display",                            ~
               at (11,30), fac(lfac$(6%)), printer$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <>  6 then L40600
                  call "APCBOMCP" (#1, #2, #3, err%)

L40600:        if keyhit% <> 15 then L40640
                  call "PRNTSCRN" : goto L40070

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Report      "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "(6)Copy/Delete Data                     " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff06ffff09ffffffff0e0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40790:     if fieldnr% > 2% then L40810
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40950  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (12)Delete      "
            pf$(2%)= "                 (9)Utilities           " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff09ffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(1%),64%) = " " : str(pfkeys$,12%,1%) = hex(ff)
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,9%,1%) = hex(ff)
            return
L40950:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42140,         /* Model Code        */   ~
                                L42140          /* Field Number      */
              goto L42170

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42170:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beginning Model Code  :",                    ~
               at (06,25), fac(lfac$(1%)), beg_mod$             , ch(03),~
               at (06,40), "Ending Model Code:",                         ~
               at (06,60), fac(lfac$(1%)), end_mod$             , ch(03),~
                                                                         ~
               at (07,02), "Beginning Field Number:",                    ~
               at (07,25), fac(lfac$(2%)), beg_fld$             , ch(03),~
               at (07,40), "Ending Model Code:",                         ~
               at (07,60), fac(lfac$(2%)), end_fld$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then L42440
                  call "PRNTSCRN" : goto L42170

L42440:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42630     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffffff0f1000)
            if fieldnr% = 1% then L42590
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42590:     if fieldnr% > 2% then L42610
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42610:     return

L42630: if fieldnr% > 0% then L42720  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L42720:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50180,         /* Field Number           */~
                              L50490,         /* Model Number           */~
                              L50740,         /* Values                 */~
                              L51130,         /* Inc/Exc                */~
                              L51250,         /* Screen Display         */~
                              L51290,         /* Printer Display        */~
                              L51320,         /* Phantom Assembly       */~
                              L51460          /* BOM Id, Phantom Assem  */
            return

L50180: REM Test for Field Number                 FIELD$
            init(" ") field_name$
            if field$ <> " " then goto L50240
               call "GETCODE" (#2, field$, field_name$, 0%, .08, f1%(2))
               if f1%(2) = 0% then errormsg$ = "Field Not Defined"
               if errormsg$ <> " " then return

L50240:     convert field$ to field%, data goto L50420

            convert field% to field$, pic(00)

            if field$ < "31" then goto L50320
               errormsg$ = "Phantom & Raw Mat'l Number Fields"           ~
                            & " have no values Assigned"
               return
L50320:     read #2,key = field$, eod goto L50420
            get #2 using L50350, field_name$, type_file$(), valid_yn$(),  ~
                                valid_table$(), file_name$()
L50350:        FMT XX(2), CH(08), XX(2), 12*CH(1), 12*CH(1), 12*CH(8),   ~
                    12*CH(8)
            i% = 0%
L50380:     i% = i% + 1%
            if i% > 12% then goto L50450
               if type_file$(i%) <> "T" then goto L50380
        return
L50420:     errormsg$ = "Invalid Field Value? "
            field$, field_name$ = " "
        return
L50450:     errormsg$ = "No Validation is needed for " & field_name$
            i% = 0% : field$, field_name$ = " "
        return

L50490: REM Test for Model/ Field 1 Definition    MODEL$
            if field$ = "01" then goto L50540
               read #2,key = "01", using L50520, table$, eod goto L50530
L50520:           FMT POS(37), CH(08)
L50530:        if field$ <> "01" then str(gencode$,1%,9%) = table$
L50540:     if field$ = "01" then str(gencode$,1%,9%) = valid_table$(1%)
            str(gencode$,10,15) = model$
            if model$ = " " then goto L50620
               read #3,key = gencode$, using L50590, mod_desc$,           ~
                                                           eod goto L50620
L50590:            FMT POS(25), CH(32)
               goto L50650

L50620:     call "PLOWCODE" (#3, gencode$, mod_desc$, 9%, .30, f1%(3))
            if f1%(3) = 0% then goto L50710

L50650:     model$ = str(gencode$,10,15)
            if field$ <> "01" then return
               value$   = model$
               fieldnr% = fieldnr% + 1%
               goto L50740
        return
L50710:     errormsg$ = "Invalid Model Number"
        return

L50740: REM Test for Values                       VALUE$
            str(defkey$,1%,15%)  = model$
            str(defkey$,16%,2%)  = field$
            str(defkey$,18%,15%) = value$
            gosub dataload
            if rec% = 1% and edit% = 1% then fieldnr% = 8%
L50810:
            if valid_yn$(i%) = "N" then goto L51010
               str(gencode$,1%,9%)   = valid_table$(i%)
               str(gencode$,10%,15%) = value$
               if value$ = " " then goto L50920
               read #3,key = gencode$, using L50880, val_desc$,           ~
                                                           eod goto L50920
L50880:            FMT POS(25), CH(32)
               value$ = str(gencode$,10,15)
               if screen$ = " " then screen$ = val_desc$
/*SR74702 + */
            strip_pipe$ = screen$
            gosub strip_pipe
            screen$ = strip_pipe$
/*SR74702 - */
               return
L50920:     str(gencode$,10%,15%) = " "
            call "PLOWCODE" (#3, gencode$, val_desc$, 9%, .30, f1%(3))
            if f1%(3%) <> 0% then goto L50980
               errormsg$ = "Invalid Value chosen for " & field_name$
               return

L50980:     value$ = str(gencode$,10%,15%)
            goto L50740

L51010:     if value$ <> " " then goto L51060
            call "PLOWCODE" (#1, defkey$, val_desc$, 17%, .30, f1%(1))
            val_desc$ = " "
            if f1%(1) <> 0% then goto L50810
            goto L51100
L51060: convert value$ to test, data goto L51100

        screen$ = " " & value$
        return
L51100:     errormsg$ = "Invalid Value chosen for " & field_name$
        return

L51130: REM Test for Include/Exclude (I/E)        INCLUDE$
            if pos("IEUL" = include$) = 0% then                          ~
               errormsg$ = "Acceptable Answers are 'I', 'E', 'U', 'L'"
            if errormsg$ <> " " then return
               if include$ = "I" then incl$ = "Include"
               if include$ = "E" then incl$ = "Exclude"
               if include$ = "U" then incl$ = "Upper Limit"
               if include$ = "L" then incl$ = "Lower Limit"
            incl$ = incl$ & " " & field_name$ & " " & value$ & " (" &    ~
                    val_desc$ & ")"
            return

L51250: REM Test for Screen Display               SCREEN$
            if printer$ = " " then printer$ = screen$
        return

L51290: REM Test for Printer Display              PRINTER$
        return

L51320: REM Test for Phantom Assembly             PHANTOM$
            if phantom$ <> " " then goto L51360
               return

L51360:     bom_key$ = all(hex(00))
            str(bom_key$,1%,25%) = phantom$
            read #5,key > bom_key$, using L51390, bom_key$, eod goto L51420
L51390:       FMT POS(26), CH(31)
            if str(bom_key$,1%,25%) <> phantom$ then goto L51420
        return
L51420:       errormsg$ = "Invalid Phantom Assembly Number?"
              phantom$ = " "
        return

L51460: REM Test for BOM Id                       BOM$

            if phantom$ <> " " then goto L51500
               bom$ = " "
               return
L51500:     if bom$ <> " " then goto L51580
               errormsg$ = "BOM Id is Required When Phantom Assigned."
               return
            bom% = 0%
            convert bom$ to bom%, data goto L51650

            convert bom% to bom$, pic(000)

L51580:     bom_key$ = all(hex(00))
            str(bom_key$,1%,25%) = phantom$
            str(bom_key$,26%,3%) = bom$
            read #5,key > bom_key$, using L51390, bom_key$,eod goto L51650
            if str(bom_key$,1%,25%) <> phantom$ then goto L51650
            if str(bom_key$,26%,3%) <> bom$ then goto L51650
        return
L51650:       errormsg$ = "INVALID Phantom/BOM Assembly Number?"
              bom$ = " "
        return

        REM *************************************************************~
            *                 R e p o r t   D a t a                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52120,         /* Beg/End Model Code     */~
                              L52250          /* Beg/End Field Number   */

            return
L52120: REM Beg/End Model Code                    BEG_MOD$, END_MOD$
            if beg_mod$ <> " " then goto L52170
L52140:        beg_mod$ = "ALL"
               end_mod$ = " "
               return
L52170:     if str(beg_mod$,1%,3%) = "ALL" then goto L52140

            if end_mod$ <> " " then goto L52210
               end_mod$ = beg_mod$
L52210:     if str(end_mod$,1%,3%) = "ALL" then goto L52140

        return

L52250: REM Beg/End Field Number                  BEG_FLD$, END_FLD$
            if beg_fld$ <> " " then goto L52300
L52270:        beg_fld$ = "ALL"
               end_fld$ = " "
               return
L52300:     if str(beg_fld$,1%,3%) = "ALL" then goto L52270

            if end_fld$ <> " " then goto L52340
               end_fld$ = beg_fld$
L52340:     if str(end_fld$,1%,3%) = "ALL" then goto L52270

        return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            rpt_time$ = " "
            print_title$ = "Product Validity Master Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCMST", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            print using L61170
            call "SETPRNT" ("APCMST", " ", 0%, 1%)
        return

        print_report
            call "SHOSTAT" ( "Printing Validity Report")

            defkey$ = hex(00)
            if beg_mod$ = "ALL" then goto L60310
               str(defkey$,1%,3%) = beg_mod$
            if beg_fld$ = "ALL" then goto L60310
               str(defkey$,4%,2%) = beg_fld$

L60310:     read #1,key > defkey$, using L35040, model$, field$, value$,  ~
                          include$, screen$, printer$, phantom$, bom$,   ~
                                             fill$, eod goto print_done
            sav_model$ = model$
            sav_field$ = field$
            goto L60410
        print_next
            read #1, using L35040, model$, field$, value$, include$,      ~
                                  screen$, printer$, phantom$, bom$,     ~
                                             fill$, eod goto print_done
L60410:     if beg_mod$ = "ALL" then goto L60430
               if model$ > end_mod$ then goto print_done
L60430:     if beg_fld$ = "ALL" then goto L60470
               if field$ < str(beg_fld$,1,2) or field$ > str(end_fld$,1,2) ~
                                    then  goto print_next

L60470:     read #2,key = field$, using L60490, field_name$, table$,      ~
                                                           eod goto L60530
L60490:       FMT POS(3), CH(8), POS(37), CH(8)
            get #2 using L60510, type_file$(), valid_yn$(), valid_table$()
L60510:         FMT XX(12), 12*CH(1), 12*CH(1), 12*CH(8)
            goto L60560
L60530:         val_desc$ = "Undefined Value"
                i% = 12%

L60560:     str(gencode$,1%,9%)   = table$
            str(gencode$,10%,15%) = model$
            read #3,key = gencode$, using L60590,mod_desc$,eod goto L60610
L60590:        FMT POS(25), CH(32)
            goto L60640
L60610:        mod_desc$ = "Undefined Model"
               goto L60740

L60640:     i% = 0%
L60650:     i% = i% + 1%
            if i% > 12% then goto print_next
            if type_file$(i%) <> "T" then goto L60650
            val_desc$ = " "
            if valid_yn$(i%) <> "Y" then goto L60740
               str(gencode$,1%,9%)   = valid_table$(i%)
               str(gencode$,10%,15%) = value$
               read #3,key = gencode$, using L60590, val_desc$,           ~
                                                          eod goto L60740
L60740:     gosub print_detail
            goto L60650
        print_done

        return

        report_header
            if lcnt% <> 99% then print using L61170
            print page
            page_no% = page_no% + 1%
            convert page_no% to page$, pic (###)

            print using L61170
            print using L61210, date$, rpt_time$, page$
            print using L61240
            print using L61280
            lcnt% = 4%
            sav_model$, prt_model$ = model$
            sav_field$, prt_field$ = field$
        return

        print_detail
            if sav_field$ = field$ then goto L60990
               prt_field$, sav_field$ = field$

L60990:     if sav_model$ = model$ then goto L61060
               prt_model$, sav_model$ = model$
               prt_field$, sav_field$ = field$
               if lcnt% > 55% then gosub report_header
               print using L61240
               goto L61080

L61060:     if lcnt% > 55% then gosub report_header
            print using L61350
L61080:     print using L61310, prt_model$, prt_field$,field_name$,value$,~
                               str(val_desc$,1%,25%), include$, screen$, ~
                               str(printer$,1%,20%), phantom$
            lcnt% = lcnt% + 2%
            prt_field$, prt_model$ = " "
        return


        REM Header Lines
L61170: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L61210: %!Run: ######## @ ########                       BY (Model) Valid~
        ~ity Field Values                                        Page: ###~
        ~  !
L61240: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--!

L61280: %!Mod!Fl!  Name  !<--- Value --->!< -- Value Description ->! I/E!~
        ~<- Screen Display ->!<- Printer Display >!<-- Phantom Assembly --~
        ~->!
L61310: %!###!##!########!###############!#########################!  # !~
        ~####################!####################!#######################~
        ~##!

L61350: %!---!--!--------!---------------!-------------------------!----!~
        ~--------------------!--------------------!-----------------------~
        ~--!

                                                            /*  (EWD002) - BEG */
        check_security
           security% = 0%
           init(" ") gencode$
           str(gencode$,1%,9%) = "VAL SECUR"
           str(gencode$,10%,3%) = userid$
           read #3,key = gencode$, eod goto not_valid
              security% = 1%
        not_valid
        return

        security_error
            comp% = 2%
            hdr$ = "******** Access Denied ********"
            msg$(1) = "Currently 'You' do not have Access To Selection!"
            msg$(2) = "           A c c e s s   D e n i e d            "
            msg$(3) = "       Press <RETURN> To Continue !!!!          "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                            /*  (EWD002) - END */

        REM *************************************************************~
            *          U T I L I T Y   S U B R O U T I N E              *~
            *************************************************************

        utility_sub
            err% = 0%
            call "APCBOMED" ( field$,         /* Specified Field No.   */~
                              field_name$,    /* Field Name            */~
                              model$,         /* Specified Model Code  */~
                              mod_desc$,      /* Model Description     */~
                              value$,         /* Specified Value       */~
                              val_desc$,      /* Value Description     */~
                              include$,       /* Specified Include Code*/~
                              incl$,          /* Include Description   */~
                              screen$,        /* Screen Description    */~
                              printer$,       /* Printer Description   */~
                              phantom$,       /* Phantom N/A           */~
                              bom$,           /* Bom Id - N/A          */~
                              #1,             /* (AMTBOMIF) - FILE     */~
                              #3,             /* (GENCODES) - FOLE     */~
                              err% )          /* 0% = OK               */
        return clear all
        goto inputmode
        
/*SR74702 + */
    strip_pipe
        sp% =  pos(strip_pipe$ = "|")
        if sp% = 0% then goto strip_pipe_end
        str(strip_pipe$,sp%,1%) = " "
        goto strip_pipe
    strip_pipe_end
        return
/*SR74702 - */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
