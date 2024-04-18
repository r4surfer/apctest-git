        REM *************************************************************~
            *                  ( As of 03/18/2013 )                     *~
            *   AAA   PPPP    CCC    CCC   U    U  TTTTT  EEEEE   QQQQ  *~
            *  A   A  P   P  C   C  C   C  U    U    T    E      Q    Q *~
            *  AAAAA  PPPP   C      C      U    U    T    EEEE   Q    Q *~
            *  A   A  P      C   C  C   C  U    U    T    E      Q  Q Q *~
            *  A   A  P       CCC    CCC    UUUU     T    EEEEE   QQQQ  *~
            *                                                         Q *~
            *-----------------------------------------------------------*~
            * APCCUTEQ - Saw Optimization Cross Reference File          *~
            *                                                           *~
            *      Group Level    - X     ( 0 thru 9 )             (01) *~
            *      Model Code     - XXX                            (03) *~
            *      Color Code     - X                              (01) *~
            *      Equation Type  - X     (1) = Width Cut          (01) *~
            *                             (2) = Height Cut              *~
            *                             (3) = Cottage Cuts            *~
            *                             (4) = Oriel Cuts              *~
            *                             (5) = 1/3, 1/3, 1/3           *~
            *                             (6) = TSO - Width Cuts        *~
            *                             (7) = TSO - Height Cuts       *~
            *                             (8) = BSO - Width Cuts        *~
            *                             (9) = BSO - Height Cuts       *~
            *                             (A - Z) Width and Height      *~
            *                                   Parts                   *~
            *                                                           *~
            *      Equation No    - XX ( 01 thru 99 )              (02) *~
            *   ------------------------(07)-----                       *~
            *      Raw Material   - XXXXXXXXXX                     (10) *~
            *      Material Length- XXXX ( in Inch's )             (04) *~
            *      Material Units - X    ( Inches/Feet )           (01) *~
            *      Phantom Desig. - XXXXX                          (05) *~
            *      Piecies Cut    - XX                             (02) *~
            *      Cut (Y or N)   - X                              (01) *~
            *      Filler         - X                              (02) *~
            *   ------------------------(32)-----                       *~
            *   Note - Equation Descriptions are Defined in the Code    *~
            *          Table "EQUATIONS" Within the System Tables.      *~
            *          Key = <Prod>-<Eq Type>-<Eq No.>                  *~
            *                ie. 6-1-01                                 *~
            *                                                           *~
            *   Note - For the Screen Department the key is built       *~
            *          using the group level '2'                        *~
            *          Key = <0>-<Eq Type>-<Eq No.>                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/03/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 12/08/98 ! (EWD001) Mod to sa_sash$ allow for other ! RHH *~
            *          !   Codes. (S=Sash and F=Frame)            !     *~
            * 03/12/06 ! (AWD002) mods to 4 new equation types    ! CMG *~
            *12/07/2007! (AWD003) Add copy function               ! DES *~
            *03/24/2012! (AWD004) add color to copy               !     *~  
            *05/21/2012! (AWD005) increase file size of APCCUTEQ  ! CMG *~               
            *02/13/2013! (AWD006) mod to add (s)ash / (f)rame     ! CMG *~
            *          !    and Die Number                        !     *~
            *03/18/2013! (AWD007) mod to add burnoff              ! CMG *~
            *06/21/2017! (CR1002) add topbottom indicator to cuteq! RDB *~
            *06/19/2023! CR3337 allow blank for topbottom indicato! RDB *~
            *************************************************************

        dim                                                              ~
            hny_rec$(4)256,              /* PART NUMBER                */~
            part$25,                     /* PART NUMBER                */~
/*(AWD002)*/desc$(36%)16,                /* TYPE CODE DESC$            */~
            sav_mod$3, sav_col$3,        /* FOR REPORTS                */~
            readkey$24,                  /* GENCODES LOOKUP KEY        */~
            desc$30,                     /* GENCODES Description       */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
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

        dim                                                              ~
            sa_key$7,                    /* (APCCUTEQ) - File Var's    */~
            cpy_key$7,                   /* Copy Check Key             */~          
            sa_key1$8,                   /* (Alternate key             */~
            sa_grp$1, sa_grp_d$30,       /* Grouping Level ( 0 thru 9 )*/~
            sa_mod$3,                    /* Saw Product Model Code     */~
            sa_mod_desc$30,              /* Saw Product Model Descript */~
            sa_tmp$3,                    /* Saw Product Model Code     */~
            sa_new$3,                    /* Saw Product Model Code     */~
            sa_new_desc$30,              /* Saw Product Model Descript */~
            sa_col$1,                    /* Saw Product Color Code     */~
            sa_new_col$1,                /* Saw Copy Color Code (AWD004)*/~
            sa_tmp_col$1,                /* Saw Temp Color      (AWD004)*/~
            sa_col_desc$30,              /* Saw Product Color Descript */~
            sa_new_col_desc$30,          /* Saw Copy Color Desc (AWD004)*/~
            sa_type$1,                   /* Cut Equation Type Code     */~
            sa_type_desc$30,             /* Cut Equation Type Descript */~
            sa_eq$2,                     /* Saw Equation Number        */~
            sa_eq_desc$30,               /* Saw Equation Description   */~
            sa_raw$10,                   /* Extrusion Raw Material Part*/~
            sa_raw_addl$5,               /* (AWD005) additional part data*/~
            sa_raw_desc$30,              /* Part Description           */~
            sa_raw_l$4,                  /* Length of Raw Material Extr*/~
            sa_raw_u$1,                  /* Units Assoc. with Length   */~
            units$6,                     /* 'Inches' or 'Feet  '       */~
            sa_phantom$5,                /* Saw Phantom Desigmator     */~
            sa_cut_p$2,                  /* Number of Pieces to Cut    */~
            sa_cut$1,                    /* Cut Piece (Y or N)         */~
            sa_w_h$1,                   /* Type of Sash(W,H,N,S,F)EWD001*/~
            sa_w_h_desc$10,             /* Type of Sash Description   */~
            sa_s_f$1,                    /* Sash / Frame     (AWD006) */~
            sa_s_f_desc$10,              /* Sash / Frame Desc(AWD006) */~
            sa_die$15,                   /* Die number       (AWD006) */~
            sa_burnoff$10,               /* Burnoff or deduction amt (AWD007)*/~
            sa_topbottom$1,              /* Top Bottom indicator T/B (CR1002)*/~
            rp_grp$1, pl_key$15,         /* Group Level Code           */~
            rp_dept$3,                   /* Report Department Code     */~
            rp_mod$3,                    /* Report Model Code          */~
            rp_col$1,                    /* Report Color Code          */~
            rp_desc1$30,                 /* Product Description        */~
            rp_desc2$30,                 /* Model Code Description     */~
            rp_desc3$30,                 /* Color Code Description     */~
            rp_desc4$30                  /* Group Level Description    */


        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 09/10/97 Saw Optimization Edit Utility  "
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
            * #01 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #02 ! GENCODES ! Master Code Table File                   *~
            * #03 ! HNYMASTR ! Part Master File                         *~
            * #04 ! APCPLNDP ! Planning Master Department File          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,   /*(AWD005)*/~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24

            select #3,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #4,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos = 11,   keylen =  12,                     ~
                        alt key  1, keypos  =     9, keylen = 14,        ~
                            key  2, keypos  =     4, keylen = 12,        ~
                            key  3, keypos  =     1, keylen = 15

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))

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
            
            init(" ") desc$()       /* (AWD006) */                              
            gosub load_desc         /* (AWD006) */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  14%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 11% then gosub display_codes
                      if keyhit% = 14% then goto inputmode_a
                      if keyhit% = 10% then goto inputmode_b
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 10% then goto inputmode_b       
                  if keyhit%  = 11% then gosub display_codes
                  if keyhit%  = 12% then gosub delete_reference
                  if keyhit%  = 14% then goto inputmode_a
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 4%
            if cursor%(2%) = 67%  then fieldnr% = fieldnr% + 1%
            if fieldnr% < 1% or fieldnr% > 14% then editpg1
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
            *       I N P U T   M O D E   F O R   R E P O R T           *~
            *************************************************************

        inputmode_a
            gosub initialize_variables

            for fieldnr% = 1% to  4
L12100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12230
L12120:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12210
L12150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L12120
                         if fieldnr% = 1% then L12100
                         goto L12150
L12210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12120
L12230:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   R E P O R T            *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit% <>  0% then       editpg2
L13140:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 4% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13190:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13190
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13190
                  lastfieldnr% = fieldnr%
            goto L13140

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub select_printer
            sa_key$ = all(hex(00))
            init(" ") sav_mod$, sav_col$
            if rp_grp$ <> "A" then str(sa_key$,1%,1%) = rp_grp$
            if str(rp_mod$,1%,1%) <> "A" then                            ~
                                             str(sa_key$,2%,3%) = rp_mod$
            if str(rp_col$,1%,1%) <> "A" then                            ~
                                             str(sa_key$,5%,1%) = rp_col$
/* CR1002  */
            read #1,key 1% > sa_key$, using L35040, sa_grp$, sa_mod$,     ~
                            sa_col$,sa_type$, sa_eq$, sa_raw$, sa_raw_l$,~
                            sa_raw_u$, sa_phantom$, sa_cut_p$, sa_cut$,  ~
                            sa_w_h$, sa_raw_addl$, sa_s_f$, sa_die$,     ~
                            sa_burnoff, sa_topbottom$,   eod goto print_done
            goto L19260
        next_reference            /* CR1002  */
            read #1,using L35040, sa_grp$, sa_mod$, sa_col$,              ~
                          sa_type$, sa_eq$, sa_raw$, sa_raw_l$,          ~
                          sa_raw_u$, sa_phantom$, sa_cut_p$, sa_cut$,    ~
                          sa_w_h$, sa_raw_addl$, sa_s_f$, sa_die$,       ~
                          sa_burnoff, sa_topbottom$,    eod goto print_done
                          
L19260:     if rp_grp$ = "A" then goto L19290
               if sa_grp$ <> rp_grp$ then goto print_done

L19290:     if str(rp_mod$,1%,1%) = "A" then goto L19320
               if rp_mod$ <> sa_mod$ then goto next_reference

L19320:     if rp_col$ = "A" then goto L19340
               if sa_col$ <> rp_col$ then goto next_reference
L19340:     if rp_dept$ = "ALL" then goto L19380
               gosub check_dept
               if rp_dept% = 0% then goto next_reference

L19380:     sa_type% = 0%              /*(AWD002) K-N */
            sa_type% = pos("123456789ABCDEFGHIJKLMN" = sa_type$)

            if sa_raw_u$ = "F" then units$ = "Feet  "                    ~
                               else units$ = "Inches"
                               
            sa_type_desc$ = "(##)"
            convert sa_type% to str(sa_type_desc$,2,2), pic(##)
            str(sa_type_desc$,5,15) = desc$(sa_type%)
            if sa_type% = 0% then sa_type_desc$ = "(Err)InvalidEq"

            if sa_w_h$ <> " " then goto L19490
               sa_w_h$ = "N"

L19490:     
            convert sa_burnoff to sa_burnoff$, pic(###0.0000-)
            gosub lookup_color
            gosub lookup_equation
            gosub lookup_part
            gosub print_detail
            goto next_reference
        print_done
            print using L55170
            gosub close_printer
        return clear all
        goto inputmode

        check_dept
            rp_dept% = 0%
            init(" ") pl_key$
            str(pl_key$,1%,3%) = sa_mod$
        check_dept_nxt
            read #4,key 3% > pl_key$, using L19670, pl_key$,              ~
                                                 eod goto check_dept_done
L19670:        FMT CH(15)
            if str(pl_key$,1%,3%) <> sa_mod$ then goto check_dept_done
            if str(pl_key$,11%,3%) <> rp_dept$ then goto check_dept_nxt
               rp_dept% = 1%
        check_dept_done
        return

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
         "Enter a Group Level Code ( 0, 1, or 2 ) Default = '0'?       ",~
         "Enter a Product Model Code?                                  ",~
         "Enter a Product Color Code?                                  ",~
/*AWD002*/"Enter a Cut Equation Type Code ( 1 thru N )?                 ",~
         "Enter a Equation Number and the Associated Description?      ",~
         "Enter a Valid Raw Material Part Number. ( Less 11 Characters)",~
         "Enter The Length of the Raw Material Extrusion. (Inches/Feet)",~
         "Enter a Valid Cut Phantom Designator?                        ",~
         "Enter the Number of Pieces to Cut. ( Minimum = 01 )?         ",~
         "Enter (Y)es or (N)o to have Piece Cut by Linealmate?         ",~
         "Enter Sash Type,(W)idth,(H)eight,(N)ot Applic,(S)ash,(F)rame?",~
         "Enter (S)ash or (F)rame?                                     ",~
         "Enter Die Number?                                            "
                                                           /* (EWD001)  */
        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28340
                inpmessage$ = edtmessage$
                return

L28340
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Group Level or (A) = All?                      ",~
         "Enter a Valid Department Code or (ALL)?                      ",~
         "Enter a Valid Model Code or (ALL)?                           ",~
         "Enter a Valid Color Code or (A) = All?                       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sa_mod$, sa_mod_desc$,     ~
                      sa_col$, sa_col_desc$, sa_type$, sa_type_desc$,    ~
                      sa_eq$, sa_eq_desc$, sa_raw$, sa_raw_desc$,        ~
                      sa_raw_l$, sa_raw_u$, units$, sa_phantom$,         ~
                      sa_cut_p$, sa_grp$, sa_cut$, sa_key$,     ~
                      sa_key1$, sa_w_h$, sa_w_h_desc$, sa_grp_d$,      ~
                      rp_grp$, rp_dept$, rp_mod$, rp_col$, rp_desc1$,    ~
                      rp_desc2$, rp_desc3$, rp_desc4$, sa_new$,          ~
                      sa_new_desc$, sa_new_col$, sa_new_col_desc$,       ~
                      sa_raw_addl$, sa_s_f$, sa_s_f_desc$, sa_die$,      ~
                      sa_burnoff$, sa_topbottom$

        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   C O P Y / D E L E T E *~
            *************************************************************

        inputmode_b
            gosub initialize_variables

            for fieldnr% = 1% to  4
L29100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L29230
L29120:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover_b
                      if keyhit%  =  3% then gosub inputmode  
                      if keyhit% <>  4% then       L29210
L29150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L29120
                         if fieldnr% = 1% then L29100
                         goto L29150
L29210:               if keyhit% = 16% then exit_program
                      if keyhit% = 10% and fieldnr% = 2% then gosub copy_model
                      if keyhit% = 11% and fieldnr% = 2% then gosub copy_model_noraw
                      if keyhit% = 13% and fieldnr% = 2% then gosub delete_model
                      if keyhit% <> 0% then       L29120
L29230:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L29120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   C O P Y / D E L E T E  *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover_b
                  if keyhit%  =  3% then gosub inputmode  
                  if keyhit%  = 14% then gosub print_report
                  if keyhit% = 10%  then gosub copy_model
                  if keyhit% = 11%  then gosub copy_model_noraw
                  if keyhit% = 13%  then gosub delete_model
                  if keyhit% = 16%  then exit_program
                  if keyhit% <>  0% then       editpg3
L29140:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 4% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L29190:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L29190
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L29190
                  lastfieldnr% = fieldnr%
            goto L29140

 deffn'053(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L29310
                inpmessage$ = edtmessage$
                return

L29310
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            inpmessage$ = "Enter Model Number    "       
            if fieldnr% = 2 then inpmessage$ = "Enter New Model Number"       
            if fieldnr% = 2 then inpmessage$ = "Enter Color Code, * for All"
            if fieldnr% = 4 then inpmessage$ = "Enter New Color Code, * for All"
            return

        deffn'103(fieldnr%, edit%)
              gosub'053(1%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L29260,         /* Model Code        */   ~
                                L29260,         /* Model Code        */   ~
                                L29260,         /* Color Code        */   ~
                                L29260          /* Color Code        */   

              goto L29290

L29250:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L29260:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L29270:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L29290:     accept                                                       ~
               at (01,02),                                               ~
                  "Model and Color Saw Cross-Reference For Equations",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Model Code              :",                  ~
               at (05,30), fac(lfac$(1%)), sa_mod$              , ch(03),~
               at (05,45), fac(hex(84)), sa_mod_desc$           , ch(30),~
               at (06,02), "New Model Code          :",                  ~
               at (06,30), fac(lfac$(2%)), sa_new$              , ch(03),~
               at (06,45), fac(hex(84)), sa_new_desc$           , ch(30),~
                                                                         ~
               at (07,02), "Color Code              :",                  ~
               at (07,30), fac(lfac$(3%)), sa_col$              , ch(01),~
               at (07,45), fac(hex(84)), sa_col_desc$           , ch(30),~
               at (08,02), "New Color Code          :",                  ~
               at (08,30), fac(lfac$(4%)), sa_new_col$          , ch(01),~
               at (08,45), fac(hex(84)), sa_new_col_desc$       , ch(30),~
                                                                         ~                                                                         
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L30020
                  call "PRNTSCRN"
                  goto L29290

L30020:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L30220     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "(10)Copy Model         (13)Delete Model"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffffffffff0affff0dff0f1000)
L30180:     if fieldnr% > 1% then L30200
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L30200:     return

L30220: if fieldnr% > 0% then L30330  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "(10)Copy Model         (13)Delete Model"
            pf$(2%)= "                                        " &        ~
                     "(11)Copy-NoRawMtrl     (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ff03ffffffffffff0a0bff0dff0f1000)

            return
L30330:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "(10)Copy Model         (13)Delete Model"
            pf$(2%)= "(11)Copy-NoRawMtrl                      " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affff0dffffff00)
            return

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L31320,         /* Model Code            */ ~
                              L31330,         /* Model Code New        */ ~
                              L31340,         /* Color Code            */ ~
                              L31350          /* Color Code New        */

            return

L31320: REM Model Code                            SA_MOD$
           if sa_mod$ <> " " then goto L31322
              goto L31324
L31322:    readkey$ = "MODEL    " & sa_mod$
           call "DESCRIBE" (#2, readkey$, sa_mod_desc$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L31324
        return
L31324:    errormsg$ = "Invalid Model Code"
           init(" ") sa_mod$, sa_mod_desc$
        return

L31330: REM Model Code New                        SA_NEW$
           if sa_new$ <> " " then goto L31332
              goto L31333
L31332:    readkey$ = "MODEL    " & sa_new$
           call "DESCRIBE" (#2, readkey$, sa_new_desc$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L31333
        return
L31333:    errormsg$ = "Invalid New Model Code"
           init(" ") sa_new$, sa_new_desc$
        return

L31340: REM Color Code                            SA_COL$
        if sa_col$ <> " " then goto L31341
L31342:   sa_col$ = "*"
          sa_col_desc$ = "ALL"
         return
         
L31341:  if sa_col$ = "*" then goto L31342
         readkey$ = "COLOR    " & sa_col$
           call "DESCRIBE" (#2, readkey$, sa_col_desc$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L31343
        return
L31343:    errormsg$ = "Invalid Color Code"
           init(" ") sa_col$, sa_col_desc$
        return

L31350: REM Color Code NEW                        SA_NEW_COL$
        if sa_new_col$ <> " " then goto L31351
L31352:   sa_new_col$ = "*"
          sa_new_col_desc$ = "ALL"
         return
         
L31351:  if sa_new_col$ = "*" then goto L31352
         readkey$ = "COLOR    " & sa_new_col$
           call "DESCRIBE" (#2, readkey$, sa_new_col_desc$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L31353
        return
L31353:    errormsg$ = "Invalid New Color Code"
           init(" ") sa_new_col$, sa_new_col_desc$
        return         
         
REM ----------------------------------
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

        startover_b
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode_b

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
          rec% = 0%
          str(sa_key$,1%,3%)  = sa_mod$
          str(sa_key$,4%,1%)  = sa_col$
          str(sa_key$,5%,1%)  = sa_type$
          str(sa_key$,6%,2%)  = sa_eq$
                                               /* (AWD005) (CR1002) */
          read #1,key = sa_key$,using L35040, sa_grp$, sa_mod$, sa_col$,  ~
                        sa_type$, sa_eq$, sa_raw$, sa_raw_l$, sa_raw_u$, ~
                        sa_phantom$, sa_cut_p$, sa_cut$, sa_w_h$,       ~
                        sa_raw_addl$, sa_s_f$, sa_die$, sa_burnoff,     ~
                        sa_topbottom$,                       eod goto L30210

          if sa_raw_addl$ < " " then init(" ") sa_raw_addl$
          if sa_s_f$ < " " then init(" ") sa_s_f$
          if sa_die$ < " " then init(" ") sa_die$
          gosub lookup_part
          if sa_raw_u$ = "F" then units$ = "Feet  "                      ~
                             else units$ = "Inches"
/* (AWD007) */                             
          convert sa_burnoff to sa_burnoff$, pic(###0.0000-)
          rec% = 1%                                         /* ON FILE */
L30210: return

REM      +-------------------------------------------------------------+  ~
         | propt for new model number and save record with new model   |  ~
         | number.   AWD003                                            |  ~
         +-------------------------------------------------------------+
         
copy_model_noraw:
copy_model: 
          init(" ") sa_key$
          str(sa_key$,1%,3%)  = sa_mod$
/* (AWD004) */          
          if sa_col$ = "*" then goto L30680
          str(sa_key$,4%,1%)  = sa_col$
          
L30680:                                      /*(AWD005) */
          read #1,key > sa_key$, using L35040,                   ~
                        sa_grp$, sa_tmp$, sa_tmp_col$, sa_type$,      ~
                        sa_eq$, sa_raw$, sa_raw_l$, sa_raw_u$,        ~
                        sa_phantom$, sa_cut_p$, sa_cut$, sa_w_h$,    ~
                        sa_raw_addl$, sa_s_f$, sa_die$, sa_burnoff,  ~
                        sa_topbottom$,                               ~
                           eod goto L30690
          str(sa_key$,1%,3%)  = sa_tmp$
          str(sa_key$,4%,1%)  = sa_tmp_col$
          str(sa_key$,5%,1%)  = sa_type$
          str(sa_key$,6%,2%)  = sa_eq$

          if sa_tmp$ <> sa_mod$ then goto L30690
/* (AWD004) */    
REM          if sa_col$ <> "*" then sa_col$ = sa_tmp_col$
          if sa_col$ = "*" then goto no_filter_color
             if sa_tmp_col$ <>  sa_col$ then goto L30680
             sa_tmp_col$ = sa_new_col$
             
no_filter_color:
/* (AWD004) */
          init(" ") cpy_key$
          str(cpy_key$,1,3) = sa_new$
          str(cpy_key$,4,1) = sa_tmp_col$
          str(cpy_key$,5,1) = sa_type$
          str(cpy_key$,6,2) = sa_eq$
          read #1, hold, key = cpy_key$, eod goto cpy_create_rec
          
             if keyhit% = 11% then get #1, using NOCPYRAW,           ~
                            sa_grp$, sa_type$, sa_eq$, sa_raw$,      ~
                            sa_raw_l$, sa_raw_u$, sa_phantom$,       ~
                            sa_cut_p$, sa_cut$, sa_w_h$,             ~
                            sa_raw_addl$, sa_burnoff, sa_topbottom$ 
             delete #1
             
cpy_create_rec:             
                                                 /* (AWD005) */
          write #1, using L35040, sa_grp$, sa_new$, sa_tmp_col$, sa_type$,~
                        sa_eq$, sa_raw$, sa_raw_l$, sa_raw_u$,           ~
                        sa_phantom$, sa_cut_p$, sa_cut$, sa_w_h$,       ~
                        sa_raw_addl$, sa_s_f$, sa_die$, sa_burnoff,     ~
                        sa_topbottom$,                                  ~
                             eod goto L30680
         goto L30680

L30690: return clear all
        goto inputmode_b

REM      +-------------------------------------------------------------+  ~
         | propt for new model number and save record with new model   |  ~
         | number.   AWD003                                            |  ~
         +-------------------------------------------------------------+
delete_model: 
          init(" ") sa_key$
          str(sa_key$,1%,3%)  = sa_mod$

L30780:                                            /* (AWD005) (CR1002) */
          read #1, hold, key >= sa_key$, using L35040,               ~
                        sa_grp$, sa_tmp$, sa_col$, sa_type$,         ~
                        sa_eq$, sa_raw$, sa_raw_l$, sa_raw_u$,       ~
                        sa_phantom$, sa_cut_p$, sa_cut$, sa_w_h$,    ~
                        sa_raw_addl$, sa_s_f$, sa_die$, sa_burnoff,  ~   
                        sa_topbottom$,                 eod goto L30790
                        
          str(sa_key$,1%,3%)  = sa_tmp$
          str(sa_key$,4%,1%)  = sa_col$
          str(sa_key$,5%,1%)  = sa_type$
          str(sa_key$,6%,2%)  = sa_eq$

          if sa_tmp$ <> sa_mod$ then goto L30790
          delete #1
         goto L30780

L30790: return clear all
        goto inputmode_b

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************
        dataput
          str(sa_key$,1%,3%)  = sa_mod$
          str(sa_key$,4%,1%)  = sa_col$
          str(sa_key$,5%,1%)  = sa_type$
          str(sa_key$,6%,2%)  = sa_eq$

          read #1,hold,key = sa_key$, eod goto L31130
            delete #1
L31130:                                             /* (AWD005) */
          write #1, using L35040, sa_grp$, sa_mod$, sa_col$, sa_type$,  ~
                        sa_eq$, sa_raw$, sa_raw_l$, sa_raw_u$,          ~
                        sa_phantom$, sa_cut_p$, sa_cut$, sa_w_h$,       ~
                        sa_raw_addl$, sa_s_f$, sa_die$, sa_burnoff,     ~
                        sa_topbottom$,   eod goto L31190

L31190: return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCCUTEQ - Cross Reference */
L35040: FMT                                                              ~
            CH(01),                      /* Saw Group Level Code       */~
            CH(03),                      /* Saw Product Model Code     */~
            CH(01),                      /* Saw Product Color Code     */~
            CH(01),                      /* Cut Equation Type Code     */~
            CH(02),                      /* Saw Equation Number        */~
            CH(10),                      /* Extrusion Raw Material Part*/~
            CH(04),                      /* Length of Raw Material Extr*/~
            CH(01),                      /* Units Assoc. with Length   */~
            CH(05),                      /* Saw Phantom Designator     */~
            CH(02),                      /* Number of Pieces to Cut    */~
            CH(01),                      /* Cut Piece ( Y or N )       */~
            CH(01),                      /* Sash Flag                  */~
            CH(05),                      /* (AWD005) raw additional    */~
            CH(01),                      /* (AWD006) sash / frame      */~
            CH(15),                      /* (AWD006) die               */~
            PD(14,4),                    /* (AWD007) burnoff           */~
            CH(01)                       /* Top/Bottom Indicator (CR1002) */
            
            
NOCPYRAW: FMT                                                            ~
            CH(01),                      /* Group Code                 */~
            POS(06),                     /*                            */~
            CH(01),                      /* Type Code                  */~
            CH(02),                      /* Equation Code              */~
            CH(10),                      /* Extrusion Raw Material Part*/~
            CH(04),                      /* Length Of Material         */~
            CH(01),                      /* Units Assoc with Length    */~
            CH(05),                      /* Phantom                    */~
            CH(02),                      /* Number of Pieces to Cut    */~
            CH(01),                      /* Cut Piece (Y or N)         */~
            CH(01),                      /* Sash Flag                  */~
            CH(05),                      /* (AWD005) raw additional    */~
            POS(54), PD(14,4),           /* (AWD007) Adjustment amt    */~
            CH(01)                       /* Top/Bottom Indicator (CR1002) */            

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
              on fieldnr% gosub L40270,         /* Group Level       */   ~
                                L40260,         /* Model Code        */   ~
                                L40260,         /* Color Code        */   ~
                                L40260,         /* Equation Type 1-K */   ~
                                L40250,         /* Equation No. 00-99*/   ~
                                L40260,         /* Raw Material      */   ~
                                L40260,         /* Material Length   */   ~
                                L40270,         /* Phantom Designator*/   ~
                                L40270,         /* Number Pieces Cut */   ~
                                L40260,         /* Cut Piece (Y or N)*/   ~
                                L40260,         /* Type of Sash (W/H/N) */~
                                L40260,         /* Type Sash / Frame */   ~
                                L40260,         /* Die Number        */   ~
                                L40260          /* Top Bottom Indicator */

              goto L40290

L40250:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40260:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40270:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40290:     accept                                                       ~
               at (01,02),                                               ~
                  "Model and Color Saw Cross-Reference For Equations",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Group Level Code (0 - 2):",                  ~
               at (05,30), fac(lfac$(1%)), sa_grp$              , ch(01),~
               at (05,45), fac(hex(84)), sa_grp_d$              , ch(30),~
               at (06,02), "Model Code              :",                  ~
               at (06,30), fac(lfac$(2%)), sa_mod$              , ch(03),~
               at (06,45), fac(hex(84)), sa_mod_desc$           , ch(30),~
               at (07,02), "Color Code              :",                  ~
               at (07,30), fac(lfac$(3%)), sa_col$              , ch(01),~
               at (07,45), fac(hex(84)), sa_col_desc$           , ch(30),~
/*(AWD002)*/   at (08,02), "Equation Type (1 Thru N):",                  ~
               at (08,30), fac(lfac$(4%)), sa_type$             , ch(01),~
               at (08,45), fac(hex(84)), sa_type_desc$          , ch(30),~
               at (09,02), "Equation Number         :",                  ~
               at (09,30), fac(lfac$(5%)), sa_eq$               , ch(02),~
               at (09,45), fac(hex(84)),   sa_eq_desc$          , ch(30),~
               at (10,02), "Raw Material Part No.   :",                  ~
               at (10,30), fac(lfac$(6%)), sa_raw$              , ch(10),~
               at (10,42), fac(lfac$(6%)), sa_raw_addl$         , ch(05),~
               at (10,49), fac(hex(84)), sa_raw_desc$           , ch(30),~
               at (11,02), "Raw Material Length, I/F:",                  ~
               at (11,30), fac(lfac$(7%)), sa_raw_l$            , ch(04),~
               at (11,45), "Units:",                                     ~
               at (11,52), fac(lfac$(7%)), units$               , ch(06),~
               at (12,02), "Phantom Designator      :",                  ~
               at (12,30), fac(lfac$(8%)), sa_phantom$          , ch(05),~
               at (13,02), "No. Pieces to Cut       :",                  ~
               at (13,30), fac(lfac$(9%)), sa_cut_p$            , ch(02),~
               at (14,02), "Cut Piece (Y)es or (N)o :",                  ~
               at (14,30), fac(lfac$(10%)), sa_cut$             , ch(01),~
               at (15,02), "Type of Sash (W,H,N,S,F):",                  ~
               at (15,30), fac(lfac$(11%)), sa_w_h$            , ch(01), ~
/*(AWD007) */  at (15,35), fac(lfac$(11%)), sa_burnoff$        , ch(10), ~
               at (15,50), fac(hex(84)), sa_w_h_desc$          , ch(10), ~
               at (16,02), "Sash / Frame (S,F)      :",                  ~
               at (16,30), fac(lfac$(12%)), sa_s_f$            , ch(01), ~
               at (16,45), fac(hex(84)), sa_s_f_desc$          , ch(10), ~
               at (17,02), "Die Number              :",                  ~
               at (17,30), fac(lfac$(13%)), sa_die$             , ch(15),~
               at (17,45), "Top or Bottom (T/B) :",                      ~
               at (17,67), fac(lfac$(14%)), sa_topbottom$       , ch(01),~ 
                                                                         ~
               at (18,02), "(1)",                                        ~
               at (18,06), fac(hex(84)), desc$(1%)              , ch(12),~
               at (19,02), "(2)",                                        ~
               at (19,06), fac(hex(84)), desc$(2%)              , ch(12),~
               at (20,02), "(3)",                                        ~
               at (20,06), fac(hex(84)), desc$(3%)              , ch(12),~
                                                                         ~
               at (18,22), "(4)",                                        ~                                                                         
               at (18,26), fac(hex(84)), desc$(4%)              , ch(12),~
               at (19,22), "(5)",                                        ~
               at (19,26), fac(hex(84)), desc$(5%)              , ch(12),~
               at (20,22), "(6)",                                        ~
               at (20,26), fac(hex(84)), desc$(6%)              , ch(12),~
                                                                         ~
               at (18,42), "(7)",                                        ~                                                                         
               at (18,46), fac(hex(84)), desc$(7%)              , ch(12),~
               at (19,42), "(8)",                                        ~
               at (19,46), fac(hex(84)), desc$(8%)              , ch(12),~
               at (20,42), "(9)",                                        ~
               at (20,46), fac(hex(84)), desc$(9%)              , ch(12),~
                                                                         ~
               at (18,62), "(10)",                                       ~                                                                         
               at (18,67), fac(hex(84)), desc$(10%)             , ch(11),~
               at (19,62), "(11)",                                       ~
               at (19,67), fac(hex(84)), desc$(11%)             , ch(11),~
               at (20,62), "(12)",                                       ~
               at (20,67), fac(hex(84)), desc$(12%)             , ch(11),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L41020
                  call "PRNTSCRN"
                  goto L40290

L41020:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41220     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "(10)Copy/Delete Model  (14)Print Report"
            pf$(2%)= "(11)Equat Types  (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0a0bff0d0e0f1000)
L41170:     if fieldnr% = 1% then L41180
                str(pf$(1%),64%)   = " " : str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),64%)   = " " : str(pfkeys$,16%,1%) = hex(ff)
L41180:     if fieldnr% > 1% then L41200
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41200:     return

L41220: if fieldnr% > 0% then L41330  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "(11)Equat Types  (12)Delete Reference   " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffff0a0b0cffff0f1000)
            if rec% = 1% then return
               str(pf$(2%),18%,30%) = " " : str(pfkeys$,12%,1%)=hex(ff)
            return
L41330:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "(11)Equat Types                         " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffff0bffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Report Selection Screen                                   *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42170,         /* Group Level       */   ~
                                L42170,         /* Department Code   */   ~
                                L42170,         /* Model Code        */   ~
                                L42170          /* Color Code        */

              goto L42200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42200:     accept                                                       ~
               at (01,02),                                               ~
                  "Saw Cross-Reference Report Selection Screen",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Group Level or (A)ll       :",               ~
               at (05,35), fac(lfac$(1%)), rp_grp$              , ch(01),~
               at (05,45), fac(hex(84)), rp_desc4$              , ch(30),~
                                                                         ~
               at (06,02), "Department Code or (ALL)   :",               ~
               at (06,35), fac(lfac$(2%)), rp_dept$             , ch(03),~
               at (06,45), fac(hex(84)), rp_desc1$              , ch(30),~
                                                                         ~
               at (07,02), "Product Model Code or (A)LL:",               ~
               at (07,35), fac(lfac$(3%)), rp_mod$              , ch(03),~
               at (07,45), fac(hex(84)), rp_desc2$              , ch(30),~
                                                                         ~
               at (08,02), "Product Color Code or (A)LL:",               ~
               at (08,35), fac(lfac$(4%)), rp_col$              , ch(01),~
               at (08,45), fac(hex(84)), rp_desc3$              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42440
                  call "PRNTSCRN"
                  goto L42200

L42440:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return 

        set_pf2
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50220,         /* Group Level (0 thru 9)*/ ~
                              L50330,         /* Model Code            */ ~
                              L50440,         /* Color Code            */ ~
                              L50540,         /* Equation Type Code    */ ~
                              L50690,         /* Equation No./Desc     */ ~
                              L50920,         /* Raw Material Part     */ ~
                              L51070,         /* Raw Material Length   */ ~
                              L51230,         /* Phantom Designator    */ ~
                              L51350,         /* Number of Pieces to CT*/ ~
                              L51460,         /* Cut Piece (Y or N)    */ ~
                              L51560,         /* Type of Sash (W,H,N)  */ ~
                              L51570,         /* (S)ash or (F)rame (AWD006)*/~
                              L51580,         /* Die Number        (AWD006)*/~
                              L51590          /* Top/Bottom indicator (CR1002)*/

            return

L50220: REM Group Level Code                      SA_GRP$
           sa_grp% = 0%
           if sa_grp$ <> " " then goto L50260
              sa_grp$ = "0"
L50260:    convert sa_grp$ to sa_grp%, data goto L50290
           if sa_grp% = 0% then sa_grp_d$ = "Manufactured Products    "
           if sa_grp% = 1% then sa_grp_d$ = "Manufactured Parts       "
           if sa_grp% = 2% then sa_grp_d$ = "Manufactured Screens     "
           if sa_grp% > 2% then goto L50290
        return
L50290:    errormsg$ = "(Error) - Invalid Level Code, ( 0 thru 2)?"
           init(" ") sa_grp$, sa_grp_d$
        return

L50330: REM Model Code                            SA_MOD$
           if sa_mod$ <> " " then goto L50360
              goto L50400
L50360:    readkey$ = "MODEL    " & sa_mod$
           call "DESCRIBE" (#2, readkey$, sa_mod_desc$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L50400
        return
L50400:    errormsg$ = "Invalid Model Code"
           init(" ") sa_mod$, sa_mod_desc$
        return

L50440: REM Color Code                            SA_COL$
           if sa_col$ <> " " then goto L50470
              goto L50500
L50470:    gosub lookup_color
           if c% = 0% then goto L50500
        return
L50500:    errormsg$ = "Invalid Color Code"
           init(" ") sa_col$, sa_col_desc$
        return

L50540: REM Equation Type Code                    SA_TYPE$
           sa_type% = 0%
           if sa_type$ <> " " then goto L50580
              goto L50650
* (AWD002)
L50580:    sa_type% = pos("123456789ABCDEFGHIJKLMN" = sa_type$)

* (AWD002) 
           if sa_type% < 1% or sa_type% > 23% then goto L50650

           sa_type_desc$ = desc$(sa_type%)

        return
L50650:    errormsg$ = "Invalid Equation Type Code ( 1 thru K )."
           init(" ") sa_type$, sa_type_desc$
        return

L50690: REM Equation Number                       SA_EQ$, SA_EQ_DESC$
           sa_eq% = 0%
           if sa_eq$ <> " " then goto L50730
              sa_eq$ = "01"
L50730:    convert sa_eq$ to sa_eq%, data goto L50850

           convert sa_eq% to sa_eq$, pic(00)

           gosub lookup_equation
           if e% = 0% then goto L50880

           if sa_eq% < 1% or sa_eq% > 99% then goto L50850
              gosub dataload
              if rec% = 0% then return
                 fieldnr% = 15%
        return
L50850:    errormsg$ = "Invalid Equation Number"
           init(" ") sa_eq$, sa_eq_desc$
        return
L50880:    errormsg$ = "(Error) - Not Defined in 'EQUATIONS' Table"
           init(" ") sa_eq$, sa_eq_desc$
        return

L50920: REM Raw Material Part Number              SA_RAW$, SA_RAW_DESC$
            init(" ") part$
            if sa_raw$ <> " " then goto L51000
               sa_raw_desc$ = " "
               sa_raw_desc$ = hex(06) & "Select a Raw Material Part"
               call "GETCODE" (#3,part$, sa_raw_desc$, 0%, 1.32, f1%(3))
               if f1%(3) = 0% then goto L51030
               sa_raw$ = str(part$,1%,10%)
L51000:     gosub lookup_part
            if p% = 0% then L51030
        return
L51030:     errormsg$ = "(Error) - Invalid Part Number."
            init(" ") sa_raw$, sa_raw_desc$
        return

L51070: REM Length of Raw Material                SA_RAW_L$
           sa_raw_l% = 0%
           convert sa_raw_l$ to sa_raw_l%, data goto L51190

           convert sa_raw_l% to sa_raw_l$, pic(0000)

           if sa_raw_l% = 0% then goto L51190
           if str(units$,1%,1%) = "F" then units$ = "Feet  "             ~
                                      else units$ = "Inches"
           sa_raw_u$ = str(units$,1%,1%)

        return
L51190:    errormsg$ = "(Error) - Invalid Length"
           init(" ") sa_raw_l$ , sa_raw_u$, units$
        return

L51230: REM Phantom Designator                     SA_PHANTOM$
           sa_phantom% = -1%
           convert sa_phantom$ to sa_phantom%, data goto L51310

           convert sa_phantom% to sa_phantom$, pic(0000)

           if sa_phantom% < 0% then goto L51310
        return
L51310:    errormsg$ = "Invalid Entry - Phantom Designator Required"
           init(" ") sa_phantom$
        return

L51350: REM Cut Number of Pieces                   SA_CUT_P$
           sa_cut_p% = 1%
           convert sa_cut_p$ to sa_cut_p%, data goto L51380
L51380:
           convert sa_cut_p% to sa_cut_p$, pic(00)
           if sa_cut_p% < 1% or sa_cut_p% > 99% then goto L51420
        return
L51420:    errormsg$ = "Invalid Entry - Must Enter a Valid Number."
           init(" ") sa_cut_p$
        return

L51460: REM Cut Piece ( Y or N )                  SA_CUT$
           if sa_cut$ <> " " then goto L51490
              sa_cut$ = "Y"
L51490:    if sa_cut$ <> "Y" and sa_cut$ <> "N" then goto L51520

        return
L51520:    errormsg$ = "(Error) - Invalid Level Code"
           sa_cut$ = "Y"
        return

L51560: REM Type of Sash ( W, H, N, S, F )          SA_W_H$
            if sa_w_h$ <> " " then goto L51600
               sa_w_h$ = "N"

L51600:     sa_w_h_desc$ = " "
            if sa_w_h$ = "W" then sa_w_h_desc$ = "Width     "
            if sa_w_h$ = "H" then sa_w_h_desc$ = "Height    "
            if sa_w_h$ = "N" then sa_w_h_desc$ = "Not/Applic"
                                                     /* (EWD001)    */
            if sa_w_h$ = "S" then sa_w_h_desc$ = "Sash Cute "
            if sa_w_h$ = "F" then sa_w_h_desc$ = "Frame Cut "
                                                     /* (EWD001)    */
            
            if sa_w_h_desc$ <> " " then goto check_burnoff
               init(" ") sa_w_h$, sa_w_h_desc$
               errormsg$ = "(Error) - Invalid Sash Type, (W, H, N, S, F)?"
        return                                       /* (EWD001)    */
/*(AWD007)*/
check_burnoff:
           if sa_w_h$ <> "W" and sa_w_h$ <> "H" then return
           sa_burnoff = 0.00
           convert sa_burnoff$ to sa_burnoff, data goto bad_burnoff
           
           convert sa_burnoff to sa_burnoff$, pic(###0.0000-)
           return
           
bad_burnoff:    
             init(" ") sa_burnoff$
             errormsg$ = "(Error) - Invalid Burnoff? Burnoff must be~
                         ~ entered for (W & H)?"   
           return
L51570: REM (S)ash or (F)rame                       SA_S_F$
            if sa_s_f$ = " " then return
            if sa_s_f$ = "S" then sa_s_f_desc$ = "SASH    "
            if sa_s_f$ = "F" then sa_s_f_desc$ = "FRAME   "
            if sa_s_f$ = "S" or sa_s_f$ = "F" then return
               init(" ") sa_s_f$, sa_s_f_desc$
               errormsg$ = "(Error) - Invalid (S)ash / (F)rame?"
               
          return
L51580:   REM Die Number                            SA_DIE$

          return
            
L51590:   if sa_topbottom$ = " " then return            /* CR3337 */
          if sa_topbottom$ = "T" or sa_topbottom$ = "B" then  goto L51592
          init(" ") sa_topbottom$
          errormsg$ = "(Error) - Invalid Top/Bottom Indicator, (T, B)?"

L51592:   return

        REM *************************************************************~
            *              R e p o r t   D a t a   E d i t s            *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52150,         /* Group Level (0 thru 2)*/ ~
                              L52310,         /* Department Code       */ ~
                              L52460,         /* Model Code            */ ~
                              L52600          /* Color Code            */ ~

            return

L52150: REM Group Level Code                      RP_GRP$
           rp_grp% = 0%
           if rp_grp$ <> "A" and rp_grp$ <> " " then goto L52210
              rp_grp$ = "A"
              rp_desc4$ = "(A)ll Group Levels"
              return
L52210:    convert rp_grp$ to rp_grp%, data goto L52270
           if rp_grp% = 0% then rp_desc4$ = "Manufactured Products    "
           if rp_grp% = 1% then rp_desc4$ = "Manufactured Parts       "
           if rp_grp% = 2% then rp_desc4$ = "Manufactured Screens     "
           if sa_grp% > 2% then goto L52270
        return
L52270:    errormsg$ = "(Error) - Invalid Level Code, ( 0 thru 2)?"
           init(" ") rp_grp$, rp_desc4$
        return

L52310: REM Department Code                       RP_DEPT$
           if str(rp_dept$,1%,1%) <> "A" and rp_dept$ <> " " then        ~
                                                             goto L52380
              rp_dept$ = "ALL"
              rp_desc1$ = "(ALL) Departments"
              return

L52380:    readkey$ = "PLAN DEPT" & rp_dept$
           call "DESCRIBE" (#2, readkey$, rp_desc1$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L52420
        return
L52420:    errormsg$ = "Invalid Department Code?"
           init(" ") rp_dept$, rp_desc1$
        return

L52460: REM Model Code                            RP_MOD$
           if str(rp_mod$,1%,1%) <> "A" and rp_mod$ <> " " then          ~
                                                            goto L52520
              rp_mod$ = "ALL"
              rp_desc2$ = "(All) Models"
              return
L52520:    readkey$ = "MODEL    " & rp_mod$
           call "DESCRIBE" (#2, readkey$, rp_desc2$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L52560
        return
L52560:    errormsg$ = "Invalid Model Code"
           init(" ") rp_mod$, rp_desc2$
        return

L52600: REM Color Code                            RP_COL$
           if rp_col$ <> " " and rp_col$ <> "A"  then goto L52650
              rp_col$ = "A"
              rp_desc3$ = "(ALL) Colors"
              return
L52650:    sa_col$ = rp_col$
           gosub lookup_color
           if c% = 0% then goto L52700
           rp_desc3$ = sa_col_desc$
        return
L52700:    errormsg$ = "Invalid Color Code"
           init(" ") rp_col$, rp_desc3$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!######## ########                  ############################~
        ~################################                        APCCUTEQ:~
        ~  !

L55090: %!User Id: ###                       ############################~
        ~################################                      Page: #####~
        ~  !

L55130: %!Group Level: #  Dept: ###                                      ~
        ~                                            Model: ###  Color: # ~
        ~  !

L55170: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
REM              10        20        30        40        50        60        70        80        90       100       110       120       130
REM      12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
L55210: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--!

                                                   /* COLUMN HEADER   */
L55260: %!G!Mod!Cl!TypDescription!Eq!EquationDescription!   Material    !~
        ~ Description        !Lgth!U!Phant!Pc!Ct!WH!SF!  Die     !   Adj    !

L55300: %!-!---!--!--------------!--!-------------------!---------------!~
        ~--------------------!----!-!-----!--!--!--!--!----------!----------!
                                                   /* DETAIL          */
REM              10        20        30        40        50        60        70        80        90       100       110       120       130
REM      12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
L55340: %!#!###!##!##############!##!###################!###############!#~
        ~###################!####!#!#####!##! #! #! #!##########!##########!

        print_header
          if lcnt% <> 99% then print using L55170
          page_no% = page_no% + 1%
          print page
          print using L55170
          print using L55050, date$, rpt_time$, company$
          print using L55090, userid$, print_title$, page_no%
          print using L55130, rp_grp$, rp_dept$, rp_mod$, rp_col$
          print using L55210
          print using L55260
          lcnt% = 6%
        return

        print_detail
          if lcnt% > 57% then gosub print_header
          print using L55300 
          print using L55340, sa_grp$, sa_mod$, str(sa_col_desc$,1%,2%),     ~
                             str(sa_type_desc$,1%,15%), sa_eq$,              ~
                             str(sa_eq_desc$,1%,20%), sa_raw$ & sa_raw_addl$,~
                             str(sa_raw_desc$,1%,20%), sa_raw_l$, sa_raw_u$, ~
                             sa_phantom$, sa_cut_p$, sa_cut$, sa_w_h$,       ~
                             sa_s_f$, sa_die$, sa_burnoff$
          lcnt% = lcnt% + 2%
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SHOSTAT" ("Creating Equation Ref. Report")
            page_no% = 0%
            lcnt%    = 99%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCEQ", " ", 0%, 0%)
            print_title$ = "Saw Optimization Cross-Reference"
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCCT", " ", 0%, 1%)
        return

        delete_reference
          call "SHOSTAT" ( "Deleting Specified Reference.")
          sa_key$ = all(hex(00))
          str(sa_key$,1%,3%)  = sa_mod$
          str(sa_key$,4%,1%)  = sa_col$
          str(sa_key$,5%,1%)  = sa_type$
          str(sa_key$,6%,2%)  = sa_eq$
          read #1,hold,key = sa_key$, eod goto L60300
             delete #1
L60300:   rec% = 0%
          return clear all
          goto inputmode

        lookup_equation
          e% = 0%                             /* GET CUT DESCRIPTION   */
          sa_eq_desc$ = " "
          readkey$ = " "
          str(readkey$,1%,9%)  = "EQUATIONS"
          str(readkey$,10%,2%) = str(sa_mod$,1%,1%) & "-" /* PROD LINE */
          if sa_grp$ = "2" then                                          ~
             str(readkey$,10%,2%) = "0" & "-"             /* SCREEN DEP*/
          str(readkey$,12%,2%) = sa_type$ & "-"           /* EQ TYPE   */
          str(readkey$,14%,2%) = sa_eq$                   /* EQ NUMBER */
          call "DESCRIBE" (#2, readkey$, sa_eq_desc$, 0%, f1%(2%))
          if f1%(2%) = 0% then return
          e% = 1%
        return

        lookup_part
          p% = 0%
          sa_raw_desc$ = " "
          part$ = " "
          str(part$,1%,10%) = sa_raw$
          read #3,key = part$, using L60530, sa_raw_desc$, eod goto L60570
L60530:     FMT POS(26), CH(30)

          if len(part$) > 10 then return
          p% = 1%
L60570: return

        lookup_color
            c% = 0%
            readkey$ = all(hex(00))
            readkey$ = "COLOR    " & sa_col$
            call "DESCRIBE" (#2, readkey$, sa_col_desc$, 0%, f1%(2%))
            if f1%(2%) = 0% then return
            c% = 1%
        return

        load_desc
          init(" ") readkey$
          readkey$ = "EQUATYPE"
          eqtype% = 0%
        load_desc_next  
          read #2, key > readkey$, using genFmt, readkey$, desc$,        ~
                                            eod goto load_desc_done
genFmt:        FMT CH(24), CH(30)

               if str(readkey$,1,9) <> "EQUATYPE" then goto load_desc_done
               eqtype% = eqtype% + 1%
               desc$(eqtype%) = desc$
                 goto load_desc_next
        load_desc_done                                                   
        return
        
        display_codes
            tab% = 29%
            call "APCPLN1B" (tab%, #2)
        return        
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
            
