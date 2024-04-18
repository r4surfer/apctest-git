        REM *************************************************************~
            *                                                           *~
            *   AAA   M   M  TTTTT  BBBB    OOO   M   M   CCC   DDDD    *~
            *  A   A  MM MM    T    B   B  O   O  MM MM  C   C  D   D   *~
            *  AAAAA  M M M    T    BBBB   O   O  M M M  C      D   D   *~
            *  A   A  M   M    T    B   B  O   O  M   M  C   C  D   D   *~
            *  A   A  M   M    T    BBBB    OOO   M   M   CCC   DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AMTBOMCD - This Program allows the User to Define         *~
            *            Equations for Specific Fields, Which were      *~
            *            Previously Specified in the (Manufactured)     *~
            *            Part Number.                                   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/28/89 ! Original                                 ! WKB *~
            * 01/10/90 ! Modified for Raw Mat'l & Phantom Fields  ! WKB *~
            * 04/24/90 ! CORRECT PROBLEM WITH 'GET FORMULA' ROUTIN! RHH *~
            * 05/02/90 ! FINAL MODS AND CLEANUP                   ! RHH *~
            * 10/18/90 ! Modified to Convert Width and Height     ! RHH *~
            *          ! Fields. Height - 08(Old) to 09(New)      !     *~
            *          !         Width  - 07(Old) to 08(New)      !     *~
            *          ! Line No. (61190) Current Disabled        !     *~
            *          ! Activated with Hidden PF(9) Key ( 41111 )!     *~
            * 11/28/97 ! Mod to review for Upgrade to R6.04.03    ! RHH *~
            *          ! Fields. Height - 08(Old) to 09(New)      !     *~
            *          !         Width  - 07(Old) to 08(New)      !     *~
            * 06/13/01 ! (EWD001) - Mod to ask for selection in   ! RHH *~
            *          !         Report Mode.                     !     *~
            *01/19/2011! (AWD002) - mod to add copy function      ! CMG *~
            *07/12/2013! (AWD003) mod for dim1es and dim2es       ! CMG *~
            *06/06/2019! (CR2067) Mod to add security to control  ! CMN *~
            *          !             what users can update data.  !     *~
            *10/09/2019! (CR2275) correct security to delete      ! MES *~
            *************************************************************

        dim                                                              ~
            rhh$2,                       /* TEST - FIELD NUMBER        */~
            cd_rec$250,                  /* EQUATION RECORD            */~
            calc_seq$(6%),               /* Calculation Seq            */~
            cdkey$42,                    /* AMTBOMCD Key               */~
            sav_cdkey$42,                /* SAVE AMTBOMCD Key (AWD002) */~
            cnt1$5, cnt2$5,              /* Status Counters   (AWD002) */~
            hdr$40, msg$(3%)79,          /* Screen Status Text(AWD002) */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dash$132,                    /* Dashes                     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filler$52,                   /* Filler Spaces              */~
            formula$60,                  /* Formula                    */~
            form$(6%)79,                 /* Formula Phrases            */~
            formula2$60,                 /* Formula Work String        */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            key_fld1$15,                 /* Key Field One              */~
            key_fld2$2,                  /* Key Field Two              */~
            key_name1$8,                 /* Key Field One Name         */~
            key_name2$8,                 /* Key Field Two Name         */~
            lfac$(20%,6%)1,              /* Field Attribute Characters */~
            llfac$(20%)1,              /* Field Attribute Characters */~
            name$(6%,3%)8,               /* Field Name                 */~
            oper$(6%,2%)1,               /* Operaitons                 */~
            oper1$(6%)1,                 /* Operations                 */~
            oper2$(6%)1,                 /* Operations                 */~
            page$3,                      /* Page Number                */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            phan_id$25,                  /* Phantom I.D.               */~
            scrn1$25,                    /* Accept Screen Prompt One   */~
            plowkey$99,                  /* Misc Key                   */~
            runtime$8,                   /* Report Run Time            */~
            test$79,                     /* Testing String             */~
            t_1$79,                      /* FORMULA BUILD STRING (1)   */~
            t_2$79,                      /* FORMULA BUILD STRING (2)   */~
            t_3$79,                      /* FORMULA BUILD STRING (3)   */~
            userid$3,                    /* Current User Id            */~
            value1$(6%)8,                /* Value                      */~
            value2$(6%)8,                /* Value                      */~
            value3$(6%)8,                /* Value                      */~
            value$(6%,3%)8,              /* Value                      */~
/*EWD001*/  beg_mod$3, end_mod$3         /* Beg/End Model Code, or All */

/* (AWD002) */
        dim cpy_beg_mdl$3, cpy_beg_fld$2, cpy_end_mdl$3, cpy_end_fld$2,~
            cpy_beg_mdl_desc$30, cpy_beg_fld_desc$20,                  ~
            cpy_end_mdl_desc$30, cpy_end_fld_desc$20

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */
            init ("-") dash$

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Equation Definition an Edit Util. "
            pname$ = "AMTBOMCD - Rev: R6.04"

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
            * #1  ! AMTBOMCD ! BOM Equation Definitions                 *~
            * #2  ! AMTBOMPM ! Bom Field Definition File                *~
            * #3  ! AMTBOMIF ! BOM Generator Validity File              *~
            * #4  ! GENCODES ! System General Codes file.               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #2,  "AMTBOMPM",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 2

            select #3,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize = 120,               ~
                        keypos = 1,    keylen = 32

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),20%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))

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

        REM Get Field One Field Name
            call "READ100" (#2, "01", f1%(2))
            if f1%(2) = 1% then goto L09365
              stop " CANNOT CONTINUE - FIELD (1) NOT DEFINED  'AMTBOMPM'"
              goto exit_program
L09365:
            get #2 using L09480, key_name1$
L09480:         FMT XX(2), CH(8)
            scrn1$ = "Equation Def For " & key_name1$
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  9%
                for sect% = 1% to 5%
                if fieldnr% < 4% and sect% > 1% then goto L10260
L10120:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10140:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% then goto report_entry
                      if keyhit% = 6% then goto inputmode_copy   /* (AWD002) */
                      if keyhit% <> 0% then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
L10260:         next sect%
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            gosub get_formula
            lastfieldnr% = 0%  : lastsect% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if cursor%(2%) < 81% then sect% = 5%
            if cursor%(2%) < 61% then sect% = 4%
            if cursor%(2%) < 51% then sect% = 3%
            if cursor%(2%) < 41% then sect% = 2%
            if cursor%(2%) < 31% then sect% = 1%
            if f1%(1) = 0% then edit_min% = 1% else edit_min% = 4%
            if fieldnr% < edit_min% or fieldnr% >  9% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            if sect% = lastsect% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11250:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11250
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11250
                  lastfieldnr% = fieldnr%
                  lastsect% = sect%
            goto L11130

        REM *************************************************************~
            *          R E P O R T   E N T R Y  S C R E E N             *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        report_entry                           /* (EWD001) - Begin     */
            gosub initialize_variables

            for fieldnr% = 1% to  2%
L12100:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12220
L12120:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12200
L12150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
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
            goto L13100                        /* (EWD001) - End       */
            
        REM *************************************************************~
            *          R E P O R T   C O P Y    S C R E E N             *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_copy                         /* (AWD002) - Begin     */
            gosub initialize_variables_copy

            for fieldnr% = 1% to  4%
L12240:         gosub'071(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12340
L12260:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12320
L12280:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L12260
                         if fieldnr% = 1% then L12240
                         goto L12280
L12320:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12120
L12340:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12260
            next fieldnr%

        REM *************************************************************~
            *            R E P O R T   E D I T   M O D E                *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  14% then gosub datasave_copy
                  if keyhit% <>  0% then       editpg3
L13300:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  4% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'071(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L13350:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13350
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13350
                  lastfieldnr% = fieldnr%
            goto L13300                        /* (AWD002) - End       */

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub check_security      /*  (CR2067) */
            if security% <> 0% then goto data_save
               gosub security_error
               return clear all
               goto inputmode
        data_save
            gosub dataput
            call "READ101" (#1, cdkey$, f1%(1))
            if f1%(1) = 1% then delete #1
            write #1 using L35030, key_fld1$, key_fld2$, phan_id$,        ~
             value1$(), oper1$(), value2$(), oper2$(), value3$(), filler$
/* (AWD002) */
            if cpy% = 1% then return
            goto inputmode
            
/* (AWD002) - begin */
        datasave_copy
            gosub check_security      /*  (CR2067) */
            if security% <> 0% then goto datasave_copy1
               gosub security_error
               return clear all
               goto inputmode
        datasave_copy1
            cpy% = 1%
            cnt1%, cnt2% = 0%
            init(" ") cnt1$, cnt2$
            call "SHOSTAT" ("Copying Data")
            cdkey$ = all(hex(00))
            cdkey$ = cpy_beg_mdl$
            
L20020:     call "READ102" (#1, cdkey$, f1%(1))
            if f1%(1) = 0% then copy_done
            gosub dataload_2

            str(cdkey$,1%,15%) = key_fld1$
            str(cdkey$,16%,2%) = key_fld2$
            str(cdkey$,18%,25%) = phan_id$
            cnt1% = cnt1% + 1%

REM         if str(cpy_beg_mdl$,1%,3%) = "ALL" then goto L20050
               if str(key_fld1$,1%,3%) <> cpy_beg_mdl$ then goto copy_done
REM               if str(key_fld1$,1%,3%) < cpy_beg_mdl$ then goto L20020

L20050:     if str(cpy_beg_fld$,1%,2%) = "AL" then goto L20100
               if str(key_fld2$,1%,2%) > cpy_end_fld$ then goto L20020
               if str(key_fld2$,1%,2%) < cpy_beg_fld$ then goto L20020
               
L20100:

            sav_cdkey$ = cdkey$
            key_fld1$ = cpy_end_mdl$
            gosub datasave
            cnt2% = cnt2% + 1%
            
            cdkey$ = sav_cdkey$
            key_fld1$ = cpy_beg_mdl$
            
            goto L20020
        
        copy_done
            cpy% = 0%
            convert cnt1% to cnt1$, pic(#####)

            convert cnt2% to cnt2$, pic(#####)

            comp% = 2%
            hdr$    = "**** Copy Statistics ****"
            msg$(1) = " ***** Records Scanned        :"&cnt1$&" ******* "
            msg$(2) = " ***** Records Copied         :"&cnt2$&" ******* "
            msg$(3) = "         Press any Key To Continue......         "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return clear all
        goto inputmode
        
/* (AWD002) - end */

        REM *************************************************************~
            *             G E N E R A T E     R E P O R T               *~
            *-----------------------------------------------------------*~
            * Generates report.                                         *~
            *************************************************************

        generate_report                       /* (EWD001) - Begin     */
            gosub select_printer
            gosub print_report
            gosub close_printer
        return clear all
        goto inputmode                        /* (EWD001) - End       */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            return
            
        deffn'071(fieldnr%)                           /* (AWD002) */
            enabled% = 1%
            return

        deffn'061(fieldnr%)                         /* (EWD001)       */
            enabled% = 1%
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if fieldnr% <> 1% then goto L28150
            inpmessage$ = "Enter the " & key_name1$ : return

L28150:     if fieldnr% <> 2% then goto L28180
            inpmessage$ = "Enter the field # to Calculate"

L28180:     if fieldnr% <> 3% then goto L28210
            inpmessage$ = "Enter Phantom I.D. " : return

L28210:     if fieldnr% < 4% or fieldnr% > 7% then return
            if sect% <> 1% and sect% <> 3% and sect% <> 5% then          ~
                goto L28270
            inpmessage$ = "Enter Value, or 'F' plus the Part's Field #," ~
            & " or 'R' plus the Calc. Result #" : return

L28270:     if sect% <> 2% and sect% <> 4% then return
            inpmessage$ = "Enter '*' for Multiply, '-' for Subtract,"    ~
            & " '+' for Add, or '/' for Divide"   : return

        deffn'060(scrnr%, fieldnr%)                /* (EWD001)          */
            if fieldnr% <> 0% then L28310
                inpmessage$ = edtmessage$
                return

L28310
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter a Valid Beginning Model Code, or (A)LL?                ",~
         "Enter a Valid Ending Model Code, or (A)LL?     "
         
        deffn'070(scrnr%, fieldnr%)                /* (AWD002)          */
            if fieldnr% <> 0% then L28410
                inpmessage$ = edtmessage$
                return

L28410
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Beginning Copy Model Code, or (A)LL?           ",~
         "Enter a Valid Beginning Copy Field Code, or (A)LL?           ",~
         "Enter a Valid Ending Copy Model Code, or (A)LL?              ",~
         "Enter a Vaild Ending Copy Field Code, or (A)LL? "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, phan_id$, value$(),        ~
                calc_seq$(), key_fld1$, key_fld2$, oper$(),              ~
                formula$, form$(), key_name2$, fld1_desc$, filler$,      ~
                beg_mod$, end_mod$                      /* (EWD001)    */

          calc_seq$(1%)= "01" : calc_seq$(2%)= "02" : calc_seq$(3%)= "03"
          calc_seq$(4%)= "04" : calc_seq$(5%)= "05" : calc_seq$(6%)= "06"
            return
            
        initialize_variables_copy
              init(" ") cpy_beg_mdl$, cpy_beg_fld$, cpy_end_mdl$,       ~
                cpy_end_fld$, cpy_beg_mdl_desc$, cpy_beg_fld_desc$,     ~
                cpy_end_mdl_desc$, cpy_end_fld_desc$, cnt1$, cnt2$, msg$(),~
                hdr$

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
            *                     L O A D   D A T A                     *~
            *************************************************************

        dataload
            str(cdkey$,1%,15%) = key_fld1$
            str(cdkey$,16%,2%) = key_fld2$
            str(cdkey$,18%,25%) = phan_id$
            call "READ100" (#1, cdkey$, f1%(1))
            if f1%(1) = 0% then goto L30250
        dataload_2
            get #1 using L35030, key_fld1$, key_fld2$, phan_id$,          ~
             value1$(), oper1$(), value2$(), oper2$(), value3$(), filler$
            for x% = 1% to 6%
                 for i% = 1% to 3%
                 if i% <> 1% then goto L30180
                     value$(x%,i%) = value1$(x%)
                     oper$(x%,i%)  = oper1$(x%)
L30180:          if i% <> 2% then goto L30210
                     value$(x%,i%) = value2$(x%)
                     oper$(x%,i%)  = oper2$(x%)
L30210:          if i% <> 3% then goto L30230
                     value$(x%,i%) = value3$(x%)
L30230:          next i%
            next x%
L30250:     return

        REM *************************************************************~
            *              P U T   D A T A   I N T O   F I L E          *~
            *************************************************************

        dataput
            for x% = 1% to 6%
                 for i% = 1% to 3%
                 if i% <> 1% then goto L31100
                     value1$(x%) = value$(x%,i%)
                     oper1$(x%)  = oper$(x%,i%)
L31100:          if i% <> 2% then goto L31130
                     value2$(x%) = value$(x%,i%)
                     oper2$(x%)  = oper$(x%,i%)
L31130:          if i% <> 3% then goto L31150
                     value3$(x%) = value$(x%,i%)
L31150:          next i%
            next x%
            str(cdkey$,1%,15%) = key_fld1$
            str(cdkey$,16%,2%) = key_fld2$
            str(cdkey$,18%,25%) = phan_id$
            put #1 using L35030, key_fld1$, key_fld2$, phan_id$,          ~
             value1$(), oper1$(), value2$(), oper2$(), value3$(), filler$
            return

        delete_it
            gosub check_security      /*  (CR2067) */
              if security% <> 0% then gosub delete_it_OK          /* (CR2275) */
               gosub security_error
               return clear all
               goto inputmode  
            delete_it_OK   
            str(cdkey$,1%,15%) = key_fld1$
            str(cdkey$,16%,2%) = key_fld2$
            str(cdkey$,18%,25%) = phan_id$ 
            call "READ101" (#1, cdkey$, f1%(1))
            if f1%(1) = 0% then goto L32090
            call "ASKUSER"(keyhit%,"CONFIRMATION",hex(8c) & "Phantom ID:" &   ~
              hex(84) & phan_id$ & hex(8c) & "is about to deleted" & hex(84), ~
              hex(8c) & "To Delete Press PF(16)." & hex(84),                  ~
              "Hit any other PF key to abort Delete")

              
            if keyhit% = 16 then delete #1
L32090:    return clear all 
          goto inputmode
        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: AMTBOMCD                          */~
            CH(15),         /* Key Field One Value                     */~
            CH(2),          /* Key Field Two                           */~
            CH(25),         /* Phantom Identifier                      */~
            6*CH(8),        /* Value or Field # or Result Line #       */~
            6*CH(1),        /* Operand(*+-/)                           */~
            6*CH(8),        /* Value or Field # or Result Line #       */~
            6*CH(1),        /* Operand(*-+/)                           */~
            6*CH(8),        /* Value or Field # or Result Line #       */~
            CH(52)          /* Filler                                  */~

        FMT                 /* FILE: AMTBOMPM                          */~
            CH(2),          /* Field Number                            */~
            CH(8),          /* Field Name                              */~
            CH(2),          /* Field Length                            */~
            12*CH(1),       /* Type of File                            */~
            12*CH(1),       /* Validated Entries                       */~
            12*CH(8),       /* Validation Table                        */~
            12*CH(8),       /* File Name for all Possible Choices      */~
            CH(22)          /* Unused Space                            */~

        FMT                 /* FILE: AMTBOMIF                          */~
            CH(15),         /* Model Number                            */~
            CH(2),          /* Definition for elements whose Type = FIE*/~
            CH(15),         /* Value of the field of the Model         */~
            CH(1),          /* Include / Exclude the Value             */~
            CH(20),         /* Verbage to appear on the Screen Displays*/~
            CH(30),         /* Verbage to print on the Hardcopy        */~
            PD(14,4),       /* Price of the Component                  */~
            CH(29)          /* Unused Space                            */~

        FMT                 /* FILE: GENCODES                          */~
            CH(9),          /* Logical File ID                         */~
            CH(15),         /* Generic for any code in the system      */~
            CH(30),         /* Generic for general code descriptions   */~
            CH(2),          /* Maximum allowable length of a Code      */~
            CH(0072)        /* Unused Space                            */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40280,         /* Key Field One     */   ~
                                L40280,         /* Key Field Two     */   ~
                                L40280,         /* Value             */   ~
                                L40210,         /* Operation         */   ~
                                L40210,         /* Operation         */   ~
                                L40210,         /* Operation         */   ~
                                L40210,         /* Operation         */   ~
                                L40210,         /* Operation         */   ~
                                L40210          /* Operation         */
              goto L40310
L40210:         on sect% gosub L40280,          /* Value             */   ~
                               L40280,          /* Operand           */   ~
                               L40280,          /* Value             */   ~
                               L40280,          /* Operand           */   ~
                               L40280           /* Value             */
                return
           lfac$(fieldnr%,sect%) = hex(80)  :  return  /* Up / Low   */
L40280:    lfac$(fieldnr%,sect%) = hex(81)  :  return  /* Upper Only */
           lfac$(fieldnr%,sect%) = hex(82)  :  return  /* Numeric    */

L40310:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)), scrn1$                 , ch(25),~
               at (06,30), fac(lfac$(01,01)), key_fld1$         , ch(15),~
               at (06,47), fac(hex(8c)), fld1_desc$             , ch(30),~
               at (07,02), "Equation is for Field",                      ~
               at (07,30), fac(lfac$(02,01)), key_fld2$         , ch(02),~
               at (07,40), fac(hex(8c)), key_name2$             , ch(08),~
                                                                         ~
               at (08,02), "Phantom Identifier",                         ~
               at (08,30), fac(lfac$(03,01)), phan_id$          , ch(25),~
                                                                         ~
               at (09,02), "Calculation",                                ~
               at (09,15), "Result #",                                   ~
               at (09,25), fac(hex(8c)),   calc_seq$(1)         , ch(02),~
               at (09,30), fac(lfac$(04,01)), value$(1,1)       , ch(08),~
               at (09,40), fac(lfac$(04,02)), oper$(1,1)        , ch(01),~
               at (09,50), fac(lfac$(04,03)), value$(1,2)       , ch(08),~
               at (09,60), fac(lfac$(04,04)), oper$(1,2)        , ch(01),~
               at (09,70), fac(lfac$(04,05)), value$(1,3)       , ch(08),~
                                                                         ~
               at (10,02), "Calculation",                                ~
               at (10,15), "Result #",                                   ~
               at (10,25), fac(hex(8c)),   calc_seq$(2)         , ch(02),~
               at (10,30), fac(lfac$( 5,01)), value$(2,1)       , ch(08),~
               at (10,40), fac(lfac$( 5,02)), oper$(2,1)        , ch(01),~
               at (10,50), fac(lfac$( 5,03)), value$(2,2)       , ch(08),~
               at (10,60), fac(lfac$( 5,04)), oper$(2,2)        , ch(01),~
               at (10,70), fac(lfac$( 5,05)), value$(2,3)       , ch(08),~
                                                                         ~
               at (11,02), "Calculation" ,                               ~
               at (11,15), "Result #",                                   ~
               at (11,25), fac(hex(8c)),   calc_seq$(3)         , ch(02),~
               at (11,30), fac(lfac$( 6,01)), value$(3,1)       , ch(08),~
               at (11,40), fac(lfac$( 6,02)), oper$(3,1)        , ch(01),~
               at (11,50), fac(lfac$( 6,03)), value$(3,2)       , ch(08),~
               at (11,60), fac(lfac$( 6,04)), oper$(3,2)        , ch(01),~
               at (11,70), fac(lfac$( 6,05)), value$(3,3)       , ch(08),~
                                                                         ~
               at (12,02), "Calculation" ,                               ~
               at (12,15), "Result #",                                   ~
               at (12,25), fac(hex(8c)),   calc_seq$(4)         , ch(02),~
               at (12,30), fac(lfac$( 7,01)), value$(4,1)       , ch(08),~
               at (12,40), fac(lfac$( 7,02)), oper$(4,1)        , ch(01),~
               at (12,50), fac(lfac$( 7,03)), value$(4,2)       , ch(08),~
               at (12,60), fac(lfac$( 7,04)), oper$(4,2)        , ch(01),~
               at (12,70), fac(lfac$( 7,05)), value$(4,3)       , ch(08),~
                                                                         ~
               at (13,02), "Calculation" ,                               ~
               at (13,15), "Result #",                                   ~
               at (13,25), fac(hex(8c)),   calc_seq$(5)         , ch(02),~
               at (13,30), fac(lfac$( 8,01)), value$(5,1)       , ch(08),~
               at (13,40), fac(lfac$( 8,02)), oper$(5,1)        , ch(01),~
               at (13,50), fac(lfac$( 8,03)), value$(5,2)       , ch(08),~
               at (13,60), fac(lfac$( 8,04)), oper$(5,2)        , ch(01),~
               at (13,70), fac(lfac$( 8,05)), value$(5,3)       , ch(08),~
                                                                         ~
               at (14,02), "Calculation" ,                               ~
               at (14,15), "Result #",                                   ~
               at (14,25), fac(hex(8c)),   calc_seq$(6)         , ch(02),~
               at (14,30), fac(lfac$( 9,01)), value$(6,1)       , ch(08),~
               at (14,40), fac(lfac$( 9,02)), oper$(6,1)        , ch(01),~
               at (14,50), fac(lfac$( 9,03)), value$(6,2)       , ch(08),~
               at (14,60), fac(lfac$( 9,04)), oper$(6,2)        , ch(01),~
               at (14,70), fac(lfac$( 9,05)), value$(6,3)       , ch(08),~
                                                                         ~
               at (16,02),"Your Formula is",                             ~
               at (17,02), fac(hex(8c)),   formula$             , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L41120
                  gosub convert_file_8
                  gosub convert_file_7
                  goto L40310


L41120:        if keyhit% =  12% then goto delete_it
               if keyhit% <> 13% then L41170
                  call "MANUAL" ("AMTBOMCD") : goto L40310

L41170:        if keyhit% <> 15% then L41200
                  call "PRNTSCRN" : goto L40310

L41200:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41390     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "(6) Copy Data                           " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff06ffffffffff0cff0e0f1000)
            if fieldnr% = 1% then L41350
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41350:     if fieldnr% > 2% then L41370
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41370:     return

L41390: if fieldnr% > 0% then L41480  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (12)Delete      "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            return
L41480:                              /*  Edit Mode - Enabled    */
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

        deffn'102(fieldnr%, edit%)           /* (EWD001) - Added Screen */
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) llfac$()                ~
                               else init(hex(86)) llfac$()
              on fieldnr% gosub L42140,          /* Model Code        */~
                                L42140           /* Model Code        */  

              goto L42170

                  llfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42140:           llfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  llfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42170:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beginning Model Code:",                      ~
               at (06,25), fac(llfac$(1%)), beg_mod$            , ch(03),~
               at (07,02), "Ending Model Code   :",                      ~
               at (07,25), fac(llfac$(2%)), end_mod$            , ch(03),~
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
            *               C O P Y   D A T A   S C R E E N             *~
            *************************************************************

        deffn'103(fieldnr%, edit%)           /* (AWD002) */
              gosub'070(1%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) llfac$()                ~
                               else init(hex(86)) llfac$()
              on fieldnr% gosub L42150,          /* Beg Model Code    */~
                                L42150,          /* Beg Field Code    */~
                                L42150,          /* End Model Code    */~
                                L42150           /* End Field Code    */

              goto L42180

                  llfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42150:           llfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  llfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "From Copy Model     :",                      ~
               at (06,25), fac(llfac$(1%)), cpy_beg_mdl$        , ch(03),~
               at (06,47), fac(hex(8c)), cpy_beg_mdl_desc$      , ch(30),~
                                                                         ~
               at (07,02), "To Copy Model       :",                      ~
               at (07,25), fac(llfac$(2%)), cpy_end_mdl$        , ch(03),~
               at (07,47), fac(hex(8c)), cpy_end_mdl_desc$      , ch(30),~
                                                                         ~
               at (08,02), "Beginning Copy Field:",                      ~
               at (08,25), fac(llfac$(3%)), cpy_beg_fld$        , ch(02),~
               at (08,47), fac(hex(8c)), cpy_beg_fld_desc$      , ch(20),~
                                                                         ~
               at (09,02), "Ending Copy Field   :",                      ~
               at (09,25), fac(llfac$(4%)), cpy_end_fld$        , ch(02),~
               at (09,47), fac(hex(8c)), cpy_end_fld_desc$      , ch(20),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then L42450
                  call "PRNTSCRN" : goto L42180

L42450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L43630     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffffff0f1000)
            if fieldnr% = 1% then L43590
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L43590:     if fieldnr% > 2% then L43610
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43610:     return

L43630: if fieldnr% > 0% then L43720  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Copy Data   "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L43720:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50180,         /* Key Field One          */~
                              L50370,         /* Key Field Two          */~
                              L50490,         /* Value                  */~
                              L50560,         /* Calculation Seq        */~
                              L50560,         /* Calculation Seq        */~
                              L50560,         /* Calculation Seq        */~
                              L50560,         /* Calculation Seq        */~
                              L50560,         /* Calculation Seq        */~
                              L50700          /* Operation              */
            return
L50180: REM Test for Key Field One                KEY_FLD1$
            str(plowkey$,1,15) = key_fld1$
            str(plowkey$,16,2) = "01"
            str(plowkey$,18,15) = key_fld1$
            call "READ100" (#3, plowkey$, f1%(3))
            if f1%(3) = 0% then goto L50340
            call "READ100" (#2, "01", f1%(2))
            if f1%(2) = 0% then goto L50340
            get #2 using L50270, table$
L50270:         FMT XX(36), CH(8)
            str(plowkey$,1,9) = table$
            str(plowkey$,10,15) = key_fld1$
            str(plowkey$,25) = hex(00)
            call "DESCRIBE" (#4, plowkey$, fld1_desc$, 0%, f1%(4))
            if f1%(4) = 0% then goto L50340
            goto L50350
L50340:     errormsg$ = "Invalid " & key_name1$
L50350:     return

L50370: REM Test for Key Field Two                KEY_FLD2$
            init(" ") key_name2$
            if key_fld2$ > "25" then goto L50430
            read #2,key = key_fld2$, using L50410, key_name2$,            ~
                                                           eod goto L50430
L50410:       FMT XX(2), CH(8)
            return
L50430:     if key_fld2$ > "25" then errormsg$ =                         ~
                 "Phantom & Raw Mat'l Part Number Fields are not Valid"  ~
                     else errormsg$ =                                    ~
                          "Calculations are invalid for this Part Field"
               key_name2$ = "(ERROR) "
            return
                
L50490: REM Test for Phantom ID                   PHAN_ID$
            gosub dataload
            if f1%(1) = 0% then goto L50540
            fieldnr% = 9%
            sect% = 5%
L50540:     return

L50560: REM Test for Calculation Seq
            x% = 0%
            if sect% = 1% then x% = 1%
            if sect% = 2% then x% = 1%
            if sect% = 3% then x% = 2%
            if sect% = 4% then x% = 2%
            if sect% = 5% then x% = 3%
            on sect% gosub L50700,        /* Value$()                   */~
                           L50950,        /* Operand                    */~
                           L50700,        /* Value$()                   */~
                           L50950,        /* Operand                    */~
                           L50700         /* Value$()                   */
            return

L50700: REM Test for Values                       VALUE$()
            if value$(fieldnr% - 3%, x%) <> " " then goto L50770
            if fieldnr% < 4% then goto L50770
            if sect% <> 1% then goto L50770
            fieldnr% = 9%
            sect% = 6%
            return
L50770:     test$ = value$(fieldnr% - 3%, x%)
            if str(test$,1,1) <> "R" then goto L50820
            if str(test$,2,2) < "01" or str(test$,2,2) > "05" then       ~
                errormsg$ = "Invalid Results Number"
            return
L50820:     if str(test$,1,1) <> "F" then goto L50900
            call "READ100" (#2, str(test$,2,2), f1%(2))
            if f1%(2) <> 0% then goto L50870
            errormsg$ = "Invalid Part Field Number"
            return
L50870:     get #2 using L50880, name$(fieldnr% - 3%, x%)
L50880:         FMT XX(2), CH(8)
            return
L50900:     
/* (AWD003) */
/* (AWD003) */      
            if str(test$,1,1) <> "D" then goto L50910
           
            if str(test$,1,3) = "D01" or                ~
                      str(test$,1,3) = "D02" then return
            errormsg$ = "Invalid Dimension Field"
            return
/* (\AWD003) */
L50910:     convert test$ to test, data goto L50930
            test = 0
            return
L50930:     errormsg$ = "Invalid Value"
            return
L50950: REM Test For Operand                      OPER$()
            if oper$(fieldnr% - 3%, x%) <> " " then goto L51000
            if sect% = 2% then goto L51010
            sect% = 6%
            return
L51000:     test$ = oper$(fieldnr% - 3%, x%)
L51010:     if test$ <> "*" and test$ <> "/" and test$ <> "+" and        ~
                test$ <> "-" then errormsg$ = "Invalid Operation"
            return

        REM *************************************************************~
            *                 R e p o r t   D a t a                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)             /* (EWD001) - Added Test '152  */
            errormsg$ = " "
            on fieldnr% gosub L52120,         /* Beg/End Model Code     */~
                              L52180          /* Beg/End Model Code     */

            return
L52120: REM Beg/End Model Code                    BEG_MOD$, END_MOD$
            if beg_mod$ <> " " then goto L52170
L52140:        beg_mod$ = "ALL"
               end_mod$ = " "
               return
L52170:     if str(beg_mod$,1%,3%) = "ALL" then goto L52140
            str(plowkey$,1,9) = "MODEL"
            str(plowkey$,10,15) = beg_mod$
            call "DESCRIBE" (#4, plowkey$, fld1_desc$, 0%, f1%(4))
            if f1%(4) = 0% then goto L52220

L52180:     if end_mod$ <> " " then goto L52210
               end_mod$ = beg_mod$
L52210:     if str(end_mod$,1%,3%) = "ALL" then goto L52140

            str(plowkey$,10,15) = end_mod$
            call "DESCRIBE" (#4, plowkey$, fld1_desc$, 0%, f1%(4))
            if f1%(4) = 0% then goto L52220
            goto L52250
L52220:     errormsg$ = "Invalid " & key_name1$
L52250:     return

        return


        REM *************************************************************~
            *                 C o p y   D a t a                         *~
            *-----------------------------------------------------------*~
            * Test data for the items on Copy Screen                    *~
            *************************************************************

        deffn'153(fieldnr%)             /* (AWD002) */
            errormsg$ = " "
            on fieldnr% gosub L53120,         /* Beg Copy Model Code     */~
                              L53180,         /* End Copy Model Code     */~
                              L53240,         /* Beg Copy Field Code     */~
                              L53290          /* End Copy Field Code     */
            return

L53120:  REM Beginning Copy Model Code                 CPY_BEG_MDL$
            str(plowkey$,1,9) = "MODEL"
            str(plowkey$,10,15) = cpy_beg_mdl$
            call "DESCRIBE" (#4, plowkey$, cpy_beg_mdl_desc$, 0%, f1%(4))
            if f1%(4) = 0% then goto L53150

            return
            
L53150:     errormsg$ = "Invalid " & cpy_beg_mdl$
            init(" ") cpy_beg_mdl$, cpy_beg_mdl_desc$
            return


L53180:  REM Ending Copy Model Code
            str(plowkey$,10,15) = cpy_end_mdl$
            call "DESCRIBE" (#4, plowkey$, cpy_end_mdl_desc$, 0%, f1%(4))
            if f1%(4) = 0% then goto L53220
            
REM         if cpy_end_mdl$ < cpy_beg_mdl$ then goto L53225
            
            return
L53220:     errormsg$ = "Invalid " & cpy_end_mdl$
            init(" ") cpy_end_mdl$, cpy_end_mdl_desc$
            return
L53225:     errormsg$ = "Invalid - End Mdl can't be less than Begin Mdl  " ~
               & cpy_beg_mdl$ & " " & cpy_end_mdl$
               
            init(" ") cpy_beg_mdl$, cpy_beg_mdl_desc$, cpy_end_mdl$, ~
                      cpy_end_mdl_desc$
            return


L53240:  REM Beginning Copy Field Code                    CPY_BEG_FLD$
REM            cpy_beg_fld_desc$
L53245:     if cpy_beg_fld$ <> " " and cpy_beg_fld$ <> "AL" then goto L53250
               cpy_beg_fld$ = "AL"
               cpy_beg_fld_desc$ = "All Fields"
               cpy_end_fld$ = "AL"
               cpy_end_fld_desc$ = "All Fields"
               return
               
L53250:     if cpy_beg_fld$ > "25" then goto L53260
            read #2,key = cpy_beg_fld$, using L50410, cpy_beg_fld_desc$,  ~
                                                           eod goto L53260
            return
L53260:     if cpy_beg_fld$ > "25" then errormsg$ =                       ~
                 "Phantom & Raw Mat'l Part Number Fields are not Valid"  ~
                     else errormsg$ =                                    ~
                          "Calculations are invalid for this Part Field"
            return

            
L53290:  REM Ending Copy Field Code
REM            cpy_end_fld_desc$

            if cpy_end_fld$ <> " " and cpy_end_fld$ <> "AL" then goto L53350
               if cpy_beg_fld$ = "AL" then goto L53245
               cpy_end_fld$ = cpy_beg_fld$
               cpy_end_fld_desc$ = cpy_beg_fld_desc$
               return

L53350:     if cpy_end_fld$ > "25" then goto L53360
            read #2,key = cpy_end_fld$, using L50410, cpy_end_fld_desc$,  ~
                                                           eod goto L53360
                                                           
            if cpy_end_fld$ < cpy_beg_fld$ then goto L53365
            
            return
L53360:     if cpy_beg_fld$ > "25" then errormsg$ =                       ~
                 "Phantom & Raw Mat'l Part Number Fields are not Valid"  ~
                     else errormsg$ =                                    ~
                          "Calculations are invalid for this Part Field"
            return
L53365:     errormsg$ = "Invalid - End Fld can't be less than Begin Fld  " ~
               & cpy_beg_fld$ & " " & cpy_end_fld$

            init(" ") cpy_beg_fld$, cpy_beg_fld_desc$, cpy_end_fld$, ~
                      cpy_end_fld_desc$
            return



        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************

        print_report
            call "SHOSTAT" ("Printing Report")
            cdkey$ = all(hex(00))
L60070:     call "READ102" (#1, cdkey$, f1%(1))
            if f1%(1) = 0% then return
            gosub dataload_2
                                               /* (EWD001) - Begin     */
            str(cdkey$,1%,15%) = key_fld1$
            str(cdkey$,16%,2%) = key_fld2$
            str(cdkey$,18%,25%) = phan_id$
          
            if str(beg_mod$,1%,3%) = "ALL" then goto L60080
               if str(key_fld1$,1%,3%) > end_mod$ then goto L60070
               if str(key_fld1$,1%,3%) < beg_mod$ then goto L60070
                                               /* (EWD001) - End     */
L60080:     gosub get_formula
            gosub L50180                  /* KEY_FLD1$                  */
            gosub L50370                  /* KEY_FLD2$                  */
            if lcntr% > 59% then gosub print_heading
*          IF STR(CDKEY$,1%,15%) = " " THEN GOTO 60043
            if str(cdkey$,1%,15%) <> key_fld1$ then goto L60200
            if lcntr% = 5% then goto L60200
            print using L60530, key_fld2$, key_name2$, phan_id$, formula$
            lcntr% = lcntr% + 1%
            goto L60260
L60200:     if lcntr% = 5% then goto L60230
            print using L60440
            lcntr% = lcntr% + 1%
L60230:     print using L60500, key_fld1$, fld1_desc$, key_fld2$,         ~
                key_name2$, phan_id$, formula$
            lcntr% = lcntr% + 1%
L60260:     goto L60070
        print_heading
            page% = page% + 1%
            if page% > 1% then print using L60440
            print page
            convert page% to page$, pic(###)
            print using L60420, date$, runtime$, page$
            print
            print using L60440
            print using L60470, table$
            print using L60440
            lcntr% = 5%
            return

L60420: %Runtime: ######## @ ########                      Equations for ~
        ~Manufactured Parts                                      Page: ###
L60440: %!-----------------------------------------!---------------------~
        ~---!--------------------!----------------------------------------~
        ~!
L60470: %!######## Equation Definitions            !Associated Field     ~
        ~   !Assoc. Phantom I.D. !Equation                                ~
        ~!
L60500: %!###############-#########################!###-#################~
        ~###!####################!########################################~
        ~!
L60530: %!                                         !###-#################~
        ~###!####################!########################################~
        ~!

        get_formula
            init (" ") formula$, form$()
            for x% = 1% to 6%
              for i% = 1% to 3%
              if value$(x%, i%) = " " then goto L60740
              if str(value$(x%,i%),1,1) <> "R" then goto L60710
              if form$(x%) = " " then form$(x%) = value$(x%,i%)          ~
                        else form$(x%) = form$(x%) & " " & value$(x%,i%)
              goto L60740
L60710:       if name$(x%,i%) <> " " then form$(x%) =  form$(x%) &       ~
                  " " & name$(x%,i%) else                                ~
                       form$(x%) = form$(x%) & " " & value$(x%, i%)
L60740:       if i% <> 3% then form$(x%) = form$(x%) & " " & oper$(x%,i%)
              next i%
            next x%

            gosub decode_equation
            gosub decode_names

        return


        decode_equation

          for x% = 1% to 6%
            init(" ") test$
            if form$(x%) <> " " then goto L60900
               goto L61120
L60900:     test$ = form$(x%)
            if pos(test$ = "R") = 0% then goto L60940
L60920:        r% = pos(test$ = "R")
               if r% <> 0% then goto L60960
L60940:           formula$ = test$
                  goto L61110
L60960:
            init(" ") t_1$, t_2$, t_3$
            convert str(test$,r%+1%,2%) to test%, data goto L61130
            if r% = 1 then goto L61010
               t_1$ = str(test$,1%,r%-1%)          /* LEFT SIDE        */
L61010:        t_3$ = str(test$,r%+4%)             /* RIGHT SIDE       */
               t_2$ = "(" & form$(test%) & ")"     /* RESULT VALUE     */
                                                   /* OR FORMULA VALUE */
               if formula$ <> " " then t_2$ = "(" & formula$ & ")"
               test$ = " "
               if t_1$ <> " " then goto L61090
                  test$ = t_2$ & t_3$
                  goto L60920
L61090:        test$ = t_1$ & t_2$ & t_3$
               goto L60920
L61110:   next x%
L61120: return
L61130:
          stop " COULD NO CALCULATE 'RESULT NO.' - " & test$
        return

        decode_names
        REM   STOP " FORMULA ---> " & FORMULA$
        REM   CLOSE WS

          if formula$ = " " then formula$ = form$(1%)
L61220:   r% = pos(formula$ = "F")
          formula2$ = " "
          if r% = 0% then goto L61340
             rhh$ = str(formula$, r%+1%, 2%)
             name$ = "(ERR-" & rhh$ & ")"
             read #2,key = rhh$, using L50880, name$, eod goto L61290

L61290:         if r% <> 1% then formula2$ = str(formula$,1%, r%-1%)
                formula2$ = formula2$ & " " & name$ & " " &              ~
                                                      str(formula$,r%+4%)
                formula$ = formula2$
                goto L61220
L61340: return

        convert_file_8
                                         /* ROUTINE DISABLED */
          return

          cdkey$ = all(hex(00))
          cd_rec$= " "
          call "SHOSTAT" ("CONVERT EQUATIONS FOR HEIGHT")
L61430:   read #1,hold,key > cdkey$,using L61440, cd_rec$,eod goto L61640
L61440:     FMT CH(250)
            cdkey$ = str(cd_rec$,1%,42%)
            if str(cd_rec$,16%,2%) = "08" then goto L61490
               goto L61430

L61490:     delete #1
            str(cd_rec$,16%,2%) = "09"
            inc% = 0%
            for x% = 0% to  5%
              inc% = x% * 8%
                if str(cd_rec$,43%+inc%,8%) = "F08" then                 ~
                                        str(cd_rec$,43%+inc%,8%) = "F09"
                if str(cd_rec$,97%+inc%,8%) = "F08" then                 ~
                                        str(cd_rec$,97%+inc%,8%) = "F09"
                if str(cd_rec$,151%+inc%,8%) = "F08" then                ~
                                        str(cd_rec$,151%+inc%,8%) = "F09"
            next x%

            write #1, using L61440, cd_rec$
            goto L61430
L61640:
        return

        convert_file_7
                                         /* ROUTINE DISABLED */
          return

          cdkey$ = all(hex(00))
          cd_rec$= " "
          call "SHOSTAT" ("CONVERT EQUATIONS FOR WIDTH")

L61750:   read #1,hold,key > cdkey$,using L61760, cd_rec$,eod goto L61950
L61760:     FMT CH(250)
            cdkey$ = str(cd_rec$,1%,42%)
            if str(cd_rec$,16%,2%) = "07" then goto L61800
               goto L61750
L61800:     delete #1
            str(cd_rec$,16%,2%) = "08"
            inc% = 0%
            for x% = 0% to  5%
              inc% = x% * 8%
                if str(cd_rec$,43%+inc%,8%) = "F07" then                 ~
                                        str(cd_rec$,43%+inc%,8%) = "F08"
                if str(cd_rec$,97%+inc%,8%) = "F07" then                 ~
                                        str(cd_rec$,97%+inc%,8%) = "F08"
                if str(cd_rec$,151%+inc%,8%) = "F07" then                ~
                                        str(cd_rec$,151%+inc%,8%) = "F08"
            next x%

            write #1, using L61760, cd_rec$
            goto L61750
L61950:
        return

        select_printer                              /* (EWD001) - Begin     */
            page% = 0%
            lcntr%    = 99%
            runtime$ = " " 
            print_title$ = "Equation Def Edit Util Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (runtime$)
            call "SETPRNT" ("APCEQU", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            print using L60440
            call "SETPRNT" ("APCEQU", " ", 0%, 1%)
        return                                        /* (EWD001) - End     */
        
                                                            /* + (CR2067) */
        check_security
           security% = 0%
           init(" ") plowkey$
           str(plowkey$,1%,9%) = "VAL SECUR"
           str(plowkey$,10%,3%) = userid$
           read #4,key = plowkey$, eod goto not_valid
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
                                                            /* - (CR2067) */
        
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
