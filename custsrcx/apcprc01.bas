        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPRC01                             *~
            *  Creation Date     - 11/24/93                             *~
            *  Last Modified Date- 11/01/99                             *~
            *  Description       - This Program Defines Key Parameters, *~
            *                      Value Parameters and Calculation     *~
            *                      Method.                              *~
            *  Code Tables Used  - (PRICE 002) - Std/Spc Reference Codes*~
            *                      (PRICE 003) - Std/Spc Calc Codes     *~
            *                      (PRICE 004) - Field Descriptions     *~
            *                      (PRICE 005) - Field Definitions      *~
            *                      (PRICE 006) - Price/Value Formats    *~
            *                      (PRICE 007) - Price/Value Calc Method*~
            *                      (PRICE 008) - Key Calculation Method *~
            *                      (PRICE 009) - Key Unit Conversion    *~
            *                                                           *~
            *  Special Comments  - Uses the Subroutine (APCPR1SB) for   *~
            *                      Code Table Lookup. TAB_VAL$5         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/24/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 04/18/97 ! Mod to allow for Alpha Code Tabel Values ! RHH *~
            *          !   Lines ( 51720 thru 52100 )             !     *~
            *          !                                          !     *~
            * 10/31/97 ! Check for R6.04.03 Upgrade               ! RHH *~
            * 05/18/98 ! Allowed alpha-numeric calc codes         ! ERN *~
            * 11/01/99 ! (EWD001) - New Wood Jamb Pricing to      ! RHH *~
            *          !    support new wood Jamb tables which    !     *~
            *          !    were added to (APCPR1SB) Lookup Sub   !     *~ 
            * 09/24/14 ! (EWD002) - Allow "00" code lookup for    ! PWW *~
            *          !            price value field #2.         !     *~
            *************************************************************

        dim                              /* (APCPCMST) Price Calc Def. */~
            pc_r$2, pc_r_d$32,           /* Std/Spc Reference Code     */~
            pc_rc$3,pc_rc_d$32,          /* Price Ref. Calc Method     */~
            pc_kdesc$(3)3,               /* Field Description Codes    */~
            pc_kdesc_d$(3)32,            /* Field Description Codes    */~
            pc_kfld%(3),                 /* Field Definition Codes     */~
            pc_kfld$(3)2,pc_kfld_d$(3)32,/* Field Definition Codes     */~
            pc_kbeg%(3),                 /* Start Position in Part No. */~
            pc_klen%(3),                 /* Field Length in Part No.   */~
            pc_vtbl$2,  pc_vtbl_d$32,    /* Field Definition Code-Table*/~
          pc_vtblv$(6)3,pc_vtblv_d$(6)32,/* Field Table Values         */~
            pc_vdesc$3, pc_vdesc_d$32,   /* Value Description Codes    */~
            pc_vfmt$2,  pc_vfmt_d$32,    /* Value Field Format Code    */~
            pc_vcalc$2, pc_vcalc_d$32,   /* Value Calculation Code     */~
            pc_kcalc$2, pc_kcalc_d$32,   /* Key   Calculation Code     */~
            pc_kunt$2, pc_kunt_d$32,     /* Key Unit Conversion Code   */~
            pc_key$5,                    /* Primary Key                */~
            pc_rec$64,                   /* (APCPCMSK) - Record        */~
            pc_kfil$16, pc_link$3,       /* Definition Filler Area     */~
            pc_k$25,                     /* Price Lookup Key - Section */~
            pc_c$4,                      /* Price Catalog              */~
            pc_cm$2,                     /* Price Catalog Method       */~
            pc_m$3,                      /* Pricing Model Code         */~
            pc_vt$(6)2,                  /* Price Value Table Codes    */~
            pc_vl(6), pc_vl$(6)10,       /* Price Dollar Values        */~
            p_key$39,                    /* Primary Price Key          */~
            k_val$3, k_desc$32,          /* Key Lookup Table Values    */~
            ik$(3)25,                    /* Key Screen Fields          */~
            tab_hdr$30,                  /* Screen Header For Tables   */~
            tab_val$5, tab_desc$32,      /* Table Key and Description  */~
            sav_key$9,                   /* Used for Loading Screen    */~
            readkey$50, descr$32,        /* GENCODES PRIMARY KEY       */~
            company$60,                  /* For Report Company Name    */~
            print_title$45,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(21)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

                                         /* Report Variables           */
        dim beg_r$2, beg_r_d$30,         /* Beginning Ref. Type Code   */~
            end_r$2, end_r_d$30,         /* Ending Ref. Type Code      */~
            sav_r$2, rr$2,               /* Save Ref. Type Code        */~
            rpt_key$5                    /* Report Primary Key         */

        dim f2%(5),                      /* = 0 if the file is open    */~
            f1%(5),                      /* = 1 if READ was successful */~
            fs%(5),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Pricing Reference Definition Utility    "
            pname$ = "APCPRC01 - Rev: R6.04"

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
            * #01 ! APCPCMSK ! Pricing (Key) Definition File            *~
            * #02 ! GENCODES ! Master System Tables File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCPCMSK",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =    5

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1), f2%(1),1000%, rslt$(1))
            call "OPENCHCK" (#2,  fs%(2), f2%(2),   0%, rslt$(2))

            f1%(1), f1%(2) = 0%

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

            for fieldnr% = 1% to  20%
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
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% then goto inputmode_report
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
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% <> 17 then goto L11180
               fieldnr% = 18%
               if cursor%(2%) < 40% then fieldnr% = 17%
               goto L11210
L11180:     if fieldnr% <> 18 then goto L11210
               fieldnr% = 20%
               if cursor%(2%) < 40% then fieldnr% = 19%
L11210:     if fieldnr% < 1% or fieldnr% > 20% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
        REM EDITPG1_CONT
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11260:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T   S C R E E N     *~
            *************************************************************

        inputmode_report
            gosub initialize_variables

            for fieldnr% = 1% to  2%
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
            *        E D I T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit% <>  0% then       editpg2
L13100:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub generate_report
        return clear all
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
         "Enter a Valid Pricing Type Reference Code?                   ",~
         "Enter a Valid Standard/Special Reference Calc Code?          ",~
         "Enter a Valid Key (1) Description Text Code, Required?       ",~
         "Enter a Valid Key (1) Field Definition Code, Required?       ",~
         "Enter a Valid Key (2) Description Text Code, or '000'=N/A?   ",~
         "Enter a Valid Key (2) Field Definition Code?                 ",~
         "Enter a Valid Key (3) Description Text Code, or '000'=N/A?   ",~
         "Enter a Valid Key (3) Field Definition Code?                 ",~
         "Enter a Valid Price Value Field Table Code, or '00'=N/A?     ",~
         "Enter a Valid Price Value Description Code, Required?        ",~
         "Enter a Valid Price Value Field (1) Code Value, or '00'=N/A? ",~
         "Enter a Valid Price Value Field (2) Code Value, or '00'=N/A? ",~
         "Enter a Valid Price Value Field (3) Code Value, or '00'=N/A? ",~
         "Enter a Valid Price Value Field (4) Code Value, or '00'=N/A? ",~
         "Enter a Valid Price Value Field (5) Code Value, or '00'=N/A? ",~
         "Enter a Valid Price Value Field (6) Code Value, or '00'=N/A? ",~
         "Enter a Valid Price Value Field Format Code, Required?       ",~
         "Enter a Valid Price Value Calculation Code, Required?        ",~
         "Enter a Valid (Key) Calculation Method Code, Required?       ",~
         "Rnter a Valid (Key) Unit Conversion Method Code, Required?   "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28430
                inpmessage$ = edtmessage$
                return

L28430
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Beginning Reference Type Code?                 ",~
         "Enter a Valid Ending Reference Type Code?                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, pc_r$, pc_rc$, pc_kdesc$(),~
                      pc_r_d$, pc_rc_d$, pc_key$, pc_rec$, pc_vtbl$,     ~
                      pc_vtblv$(), pc_vdesc$, pc_vfmt$, pc_vcalc$  ,     ~
                      pc_kfil$, pc_kdesc_d$(), pc_kfld$(), pc_kfld_d$(), ~
                      pc_vtbl_d$, pc_vtblv_d$(), pc_vdesc_d$, pc_vfmt_d$,~
                      pc_vcalc_d$, pc_kcalc$, pc_kcalc_d$, pc_kunt$,     ~
                      pc_kunt_d$, pc_k$, pc_c$, pc_cm$, pc_m$, p_key$,   ~
                      pc_vt$(), pc_vl$(), ik$(), k_val$, k_desc$
           rpt% = 0%
           mat pc_vl    = zer
           mat pc_kfld% = zer
           mat pc_kbeg% = zer
           mat pc_klen% = zer
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
            pc_rec% = 0%
            pc_key$ = all(hex(00))
            str(pc_key$,1%,2%) = pc_r$
            str(pc_key$,3%,3%) = pc_rc$
            read #1,key = pc_key$, eod goto L30240
               get #1, using L35040, pc_r$, pc_rc$, pc_kdesc$(1%),        ~
                                    pc_kdesc$(2%), pc_kdesc$(3%),        ~
                                    pc_kfld%(1%), pc_kfld%(2%),          ~
                                    pc_kfld%(3%), pc_kbeg%(1%),          ~
                                    pc_kbeg%(2%), pc_kbeg%(3%),          ~
                                    pc_klen%(1%), pc_klen%(2%),          ~
                                    pc_klen%(3%), pc_vtbl$, pc_vtln%,    ~
                                    pc_vdesc$,                           ~
                                    pc_vtblv$(1%), pc_vtblv$(2%),        ~
                                    pc_vtblv$(3%), pc_vtblv$(4%),        ~
                                    pc_vtblv$(5%), pc_vtblv$(6%),        ~
                                    pc_vfmt$, pc_vcalc$, pc_kcalc$,      ~
                                    pc_kunt$, pc_kfil$
            pc_rec% = 1%
L30240: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_it
        dataput
            pc_rec% = 0%
            pc_key$ = all(hex(00))
            str(pc_key$,1%,2%) = pc_r$
            str(pc_key$,3%,3%) = pc_rc$
            read #1,hold,key = pc_key$, eod goto L31150
               delete #1
               if keyhit% = 12% then return
L31150:     put #1, using L35040,    pc_r$, pc_rc$, pc_kdesc$(1%),        ~
                                    pc_kdesc$(2%), pc_kdesc$(3%),        ~
                                    pc_kfld%(1%), pc_kfld%(2%),          ~
                                    pc_kfld%(3%), pc_kbeg%(1%),          ~
                                    pc_kbeg%(2%), pc_kbeg%(3%),          ~
                                    pc_klen%(1%), pc_klen%(2%),          ~
                                    pc_klen%(3%), pc_vtbl$, pc_vtln%,    ~
                                    pc_vdesc$,                           ~
                                    pc_vtblv$(1%), pc_vtblv$(2%),        ~
                                    pc_vtblv$(3%), pc_vtblv$(4%),        ~
                                    pc_vtblv$(5%), pc_vtblv$(6%),        ~
                                    pc_vfmt$, pc_vcalc$, pc_kcalc$,      ~
                                    pc_kunt$, pc_kfil$
            write #1, eod goto L31320
            pc_rec% = 1%
        return clear all
        goto inputmode

L31320:     errormsg$ = "(Error) - Updating Key Def. File???"
        return

        REM *************************************************************~
            *       C O N V E R T   D A T A   F O R   S C R E E N       *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        data_convert
            for i% = 1% to 3%
                convert pc_kfld%(i%) to pc_kfld$(i%), pic(00)

            next i%
            if rpt% = 0% then goto L32120
               gosub L50300                     /* Price Ref Code    */
               gosub L50480                     /* Price Ref Calc Cde*/
L32120:     gosub L50750                        /* Key (1) Descript  */
            gosub L50980                        /* Key (1) Field Def */
            gosub L50760                        /* Key (2) Descript  */
            gosub L50990                        /* Key (2) Field Def */
            gosub L50770                        /* Key (3) Descript  */
            gosub L51000                        /* Key (3) Field Def */
            gosub L51330                        /* Value Field Table */
            gosub L51540                        /* Value Field Descr */
            gosub L51730                        /* Value Code (1)    */
            gosub L51740                        /* Value Code (2)    */
            gosub L51750                        /* Value Code (3)    */
            gosub L51760                        /* Value Code (4)    */
            gosub L51770                        /* Value Code (5)    */
            gosub L51780                        /* Value Code (6)    */
            gosub L52110                        /* Value Format Code */
            gosub L52310                        /* Value Calc. Code  */
            gosub L52500                        /* Key Calc Method   */
            gosub L52820                        /* Key Unit Convert  */

        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                     /* (APCPCMSK) (Key) Definition File    */~
            CH(02),             /* Std/Spc Reference Code (PRICE 002)  */~
            CH(03),             /* Std/Spc Ref. Calc Cde- (PRICE 003)  */~
            3*CH(03),           /* Key Field Descript   - (PRICE 004)  */~
            3*BI(1),            /* Key Field Def. Code  - (PRICE 005)  */~
            3*BI(1),            /* Key Field Start Pos  - Pos(11,2)    */~
            3*BI(1),            /* Key Field Length     - Pos(14,2)    */~
            CH(02),             /* Value Field Table      (PRICE 005)  */~
            BI(1),              /* Length of Table Code                */~
            CH(03),             /* Value Field Descripti  (PRICE 004)  */~
            6*CH(03),           /* Table Code Values - From Table      */~
            CH(02),             /* Value Format Code      (PRICE 006)  */~
            CH(02),             /* Value Calc Method      (PRICE 007)  */~
            CH(02),             /* Key Calc Method        (PRICE 008)  */~
            CH(02),             /* Key Unit Conversion    (PRICE 009)  */~
            CH(09)              /* Filler Area                         */

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
              on fieldnr% gosub L40350,         /* Price Ref Code    */   ~
                                L40350,         /* Price Ref Calc Cde*/   ~
                                L40350,         /* Key (1) Descript  */   ~
                                L40350,         /* Key (1) Field Def */   ~
                                L40350,         /* Key (2) Descript  */   ~
                                L40350,         /* Key (2) Field Def */   ~
                                L40350,         /* Key (3) Descript  */   ~
                                L40350,         /* Key (3) Field Def */   ~
                                L40350,         /* Value Field Table */   ~
                                L40350,         /* Value Field Descr */   ~
                                L40350,         /* Value Code (1)    */   ~
                                L40350,         /* Value Code (2)    */   ~
                                L40350,         /* Value Code (3)    */   ~
                                L40350,         /* Value Code (4)    */   ~
                                L40350,         /* Value Code (5)    */   ~
                                L40350,         /* Value Code (6)    */   ~
                                L40350,         /* Value Format Code */   ~
                                L40350,         /* Value Calc. Code  */   ~
                                L40350,         /* Key Calc Method   */   ~
                                L40350          /* Key Unit Convert  */

              goto L40380

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40350:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40380:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Price Ref. Code    -PF(2):",                 ~
               at (03,30), fac(lfac$( 1)), pc_r$                , ch(02),~
               at (03,40), fac(hex(84)), pc_r_d$                , ch(30),~
                                                                         ~
               at (04,02), "Price Ref Calc Code-PF(3):",                 ~
               at (04,30), fac(lfac$( 2)), pc_rc$               , ch(03),~
               at (04,40), fac(hex(84)), pc_rc_d$               , ch(30),~
                                                                         ~
               at (05,02), "   Key (1) Descript-PF(5):",                 ~
               at (05,30), fac(lfac$( 3)), pc_kdesc$(1%)        , ch(03),~
               at (05,40), fac(hex(84)), pc_kdesc_d$(1%)        , ch(30),~
                                                                         ~
               at (06,02), "   Key (1) Field No-PF(6):",                 ~
               at (06,30), fac(lfac$( 4)), pc_kfld$(1%)         , ch(02),~
               at (06,40), fac(hex(84)), pc_kfld_d$(1%)         , ch(30),~
                                                                         ~
               at (07,02), "   Key (2) Descript-PF(5):",                 ~
               at (07,30), fac(lfac$( 5)), pc_kdesc$(2%)        , ch(03),~
               at (07,40), fac(hex(84)), pc_kdesc_d$(2%)        , ch(30),~
                                                                         ~
               at (08,02), "   Key (2) Field No-PF(6):",                 ~
               at (08,30), fac(lfac$( 6)), pc_kfld$(2%)         , ch(02),~
               at (08,40), fac(hex(84)), pc_kfld_d$(2%)         , ch(32),~
                                                                         ~
               at (09,02), "   Key (3) Descript-PF(5):",                 ~
               at (09,30), fac(lfac$( 7)), pc_kdesc$(3%)        , ch(03),~
               at (09,40), fac(hex(84)), pc_kdesc_d$(3%)        , ch(32),~
                                                                         ~
               at (10,02), "   Key (3) Field No-PF(6):",                 ~
               at (10,30), fac(lfac$( 8)), pc_kfld$(3%)         , ch(02),~
               at (10,40), fac(hex(84)), pc_kfld_d$(3%)         , ch(32),~
                                                                         ~
               at (11,02), "Value Field Number -PF(6):",                 ~
               at (11,30), fac(lfac$( 9)), pc_vtbl$             , ch(02),~
               at (11,40), fac(hex(84)), pc_vtbl_d$             , ch(32),~
                                                                         ~
               at (12,02), "Value Field Descr  -PF(5):",                 ~
               at (12,30), fac(lfac$(10)), pc_vdesc$            , ch(03),~
               at (12,40), fac(hex(84)), pc_vdesc_d$            , ch(32),~
                                                                         ~
               at (13,02), "   Value Field (1) Code  :",                 ~
               at (13,30), fac(lfac$(11)), pc_vtblv$(1%)        , ch(03),~
               at (13,40), fac(hex(84)), pc_vtblv_d$(1%)        , ch(32),~
                                                                         ~
               at (14,02), "   Value Field (2) Code  :",                 ~
               at (14,30), fac(lfac$(12)), pc_vtblv$(2%)        , ch(03),~
               at (14,40), fac(hex(84)), pc_vtblv_d$(2%)        , ch(32),~
                                                                         ~
               at (15,02), "   Value Field (3) Code  :",                 ~
               at (15,30), fac(lfac$(13)), pc_vtblv$(3%)        , ch(03),~
               at (15,40), fac(hex(84)), pc_vtblv_d$(3%)        , ch(32),~
                                                                         ~
               at (16,02), "   Value Field (4) Code  :",                 ~
               at (16,30), fac(lfac$(14)), pc_vtblv$(4%)        , ch(03),~
               at (16,40), fac(hex(84)), pc_vtblv_d$(4%)        , ch(32),~
                                                                         ~
               at (17,02), "   Value Field (5) Code  :",                 ~
               at (17,30), fac(lfac$(15)), pc_vtblv$(5%)        , ch(03),~
               at (17,40), fac(hex(84)), pc_vtblv_d$(5%)        , ch(32),~
                                                                         ~
               at (18,02), "   Value Field (6) Code  :",                 ~
               at (18,30), fac(lfac$(16)), pc_vtblv$(6%)        , ch(03),~
               at (18,40), fac(hex(84)), pc_vtblv_d$(6%)        , ch(32),~
                                                                         ~
               at (19,02), "Val Format:",                                ~
               at (19,15), fac(lfac$(17)), pc_vfmt$             , ch(02),~
               at (19,20), fac(hex(84)), pc_vfmt_d$             , ch(20),~
                                                                         ~
               at (19,40), "Val Calc. :",                                ~
               at (19,52), fac(lfac$(18)), pc_vcalc$            , ch(02),~
               at (19,57), fac(hex(84)), pc_vcalc_d$            , ch(20),~
                                                                         ~
               at (20,02), "Key Calc. :",                                ~
               at (20,15), fac(lfac$(19)), pc_kcalc$            , ch(02),~
               at (20,20), fac(hex(84)), pc_kcalc_d$            , ch(20),~
                                                                         ~
               at (20,40), "Key Unt Cv:",                                ~
               at (20,52), fac(lfac$(20)), pc_kunt$             , ch(02),~
               at (20,57), fac(hex(84)), pc_kunt_d$             , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41360
                  call "PRNTSCRN"
                  goto L40380

L41360:        if keyhit% =  4 then goto L41430
                  if keyhit% < 2 or keyhit% > 10 then goto L41430
                     tab_1% = keyhit%
                     if keyhit% > 3 then tab_1% = tab_1% - 1%
                     gosub lookup_tab_1
                     goto L40070

L41430:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41620     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "(7)Field Formats       (10)Key Unit Cv "
            pf$(2) = "(2)Ref. Codes    (5)Field Descriptions  " &        ~
                     "(8)Val Calc Method     (14)Print Report"
            pf$(3) = "(3)Ref. Calc's   (6)Field Definitions   " &        ~
                     "(9)Key Calc Method     (16)Exit Program"
            pfkeys$ = hex(0102030405060708090affffff0e0f1000)
            if fieldnr% = 1% then L41580
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41580:     if fieldnr% > 1% then L41600
                str(pf$(1),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41600:     return

L41620: if fieldnr% > 0% then L41710  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(7)Field Formats       (10)Key Unit Cv "
            pf$(2) = "(2)Ref. Codes    (5)Field Descriptions  " &        ~
                     "(8)Val Calc Method     (12)Delete Rec  "
            pf$(3) = "(3)Ref. Calc's   (6)Field Definitions   " &        ~
                     "(9)Key Calc Method     (16)Save Data   "
            pfkeys$ = hex(010203ff05060708090aff0cff0e0f1000)
            return
L41710:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(7)Field Formats       (10)Key Unit Cv "
            pf$(2) = "(2)Ref. Codes    (5)Field Descriptions  " &        ~
                     "(8)Val Calc Method                     "
            pf$(3) = "(3)Ref. Calc's   (6)Field Definitions   " &        ~
                     "(9)Key Calc Method                     "
            pfkeys$ = hex(010203ff05060708090affffffffffff00)
            return

        REM *************************************************************~
            *               R e p o r t   S c r e e n                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
L44070:       gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L44170,         /* Beg Ref Code      */   ~
                                L44170          /* End Ref Code      */

              goto L44200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44200:     accept                                                       ~
               at (01,02),                                               ~
                  "Report Input Screen for Pricing Reference Type Codes",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Beginning Reference Code :",                 ~
               at (03,30), fac(lfac$( 1)), beg_r$               , ch(02),~
               at (03,40), fac(hex(84)), beg_r_d$               , ch(30),~
                                                                         ~
               at (04,02), "Ending Reference Code    :",                 ~
               at (04,30), fac(lfac$( 2)), end_r$               , ch(02),~
               at (04,40), fac(hex(84)), end_r_d$               , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L44470
                  call "PRNTSCRN"
                  goto L44070

L44470:        if keyhit% <> 2 then goto L44530
                  tab_1% = 2%
                  gosub lookup_tab_1
                  goto L44070

L44530:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L44720     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "(2)Ref. Codes                           " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(0102ff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L44680
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L44680:     if fieldnr% > 1% then L44700
                str(pf$(1),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L44700:     return

L44720: if fieldnr% > 0% then L44810  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "(2)Ref. Codes                           " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(0102ffffffffffffffffffffff0e0f1000)
            return
L44810:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "(2)Ref. Codes                           " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *       D a t a   I n p u t   S c r e e n   E d i t s       *~
            *-----------------------------------------------------------*~
            * Edits for Items on Screen 1                               *~
            *************************************************************

        deffn'151(fieldnr%)
            tab_val$, errormsg$ = " "
              on fieldnr% gosub L50300,         /* Price Reference Code */~
                                L50480,         /* Price Ref Calc Code  */~
                                L50750,         /* Key (1) Description  */~
                                L50980,         /* Key (1) Field Def.   */~
                                L50760,         /* Key (2) Description  */~
                                L50990,         /* Key (2) Field Def.   */~
                                L50770,         /* Key (3) Description  */~
                                L51000,         /* Key (3) Field Def.   */~
                                L51330,         /* Value Table Def Code */~
                                L51540,         /* Value Field Descript */~
                                L51730,         /* Value (1) Field Code */~
                                L51740,         /* Value (2) Field Code */~
                                L51750,         /* Value (3) Field Code */~
                                L51760,         /* Value (4) Field Code */~
                                L51770,         /* Value (5) Field Code */~
                                L51780,         /* Value (6) Field Code */~
                                L52110,         /* Value Format Code    */~
                                L52310,         /* Value Calc Codes     */~
                                L52500,         /* Key Calc Method      */~
                                L52820          /* Key Unit Conversion  */
            return

L50300: REM Price Reference Code                  PC_R$
           if pc_r$ <> " " then goto L50330
              goto L50440
L50330:    convert pc_r$ to zz%, data goto L50340
L50340:
           convert zz% to pc_r$, pic(00)

           tab_2% = 0%
           tab_1% = 2%                        /* (PRICE 002) Ref Codes */
           tab_val$ = pc_r$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L50440
               pc_r_d$ = tab_desc$
        return
L50440:    errormsg$ = "(Error) - Invalid Reference Code?    "
           pc_r$, pc_r_d$ = " "
        return

L50480: REM Price Reference Calc Codes            PC_RC$
           if pc_rc$ <> " " then goto L50510
              pc_rc$ = "000"
L50510:    convert pc_rc$ to zz%, data goto L50520

           convert zz% to pc_rc$, pic(000)
L50520:
           tab_2% = 0%
           tab_1% = 3%                   /* (PRICE 003) Ref Calc Codes */
           tab_val$ = pc_r$ & pc_rc$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L50700
               pc_rc_d$ = tab_desc$
               pc_link$ = str(tab_desc$,22%,3%)
           if edit% <> 1% then return
           if rpt% <> 0% then return
           gosub dataload
           if pc_rec% = 0% then return
              gosub data_convert
              fieldnr% = 20%

        return
L50700:    errormsg$ = "(Error) - Invalid Reference Code?    "
           pc_rc$, pc_rc_d$ = " "
        return

        REM Key Descriptions ( 1 thru 3 )         PC_KDESC$()
L50750:    x% = 1% : goto L50780
L50760:    x% = 2% : goto L50780
L50770:    x% = 3%
L50780:    if pc_kdesc$(x%) <> " " then goto L50800
              pc_kdesc$(x%) = "000"
L50800:    if x% < 2% then goto L50830
              if pc_kdesc$(x%-1%) = "000" then pc_kdesc$(x%) = "000"

L50830:    convert pc_kdesc$(x%) to zz%, data goto L50840
L50840:
           convert zz% to pc_kdesc$(x%), pic(000)
           tab_2% = 0%
           tab_1% = 4%             /* (PRICE 004) - Field Descriptions */
           tab_val$ = pc_kdesc$(x%)
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L50930
               pc_kdesc_d$(x%) = str(tab_desc$,1%,20%) & " "
        return
L50930:    errormsg$ = "(Error) - Invalid Filed Description? "
           init(" ") pc_kdesc$(x%), pc_kdesc_d$(x%)
        return

        REM Key Field Definitions ( 1 thru 3 )    PC_KFLD$()
L50980:    x% = 1% : goto L51010
L50990:    x% = 2% : goto L51010
L51000:    x% = 3%
L51010:    if pc_kfld$(x%) <> " " then goto L51030
              pc_kfld$(x%) = "00"
L51030:    if pc_kdesc$(x%) = "000" then pc_kfld$(x%) = "00"
           convert pc_kfld$(x%) to zz%, data goto L51050
L51050:
           convert zz% to pc_kfld$(x%), pic(00)

           tab_2% = 0%
           tab_1% = 5%         /* (PRICE 005) - Field Definition Codes */
           tab_val$ = pc_kfld$(x%)
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L51280
               pc_kfld_d$(x%) = str(tab_desc$,1%,9%) & " "
               convert tab_val$ to pc_kfld%(x%), data goto L51280

               if pc_kfld%(x%) <> 0% then goto L51190
                  pc_kbeg%(x%), pc_klen%(x%) = 0%
                  return
L51190:        convert str(tab_desc$,11%,2%) to pc_kbeg%(x%),            ~
                                                          data goto L51280

               convert str(tab_desc$,14%,2%) to pc_klen%(x%),            ~
                                                          data goto L51280

               zz% = pc_klen%(1%) + pc_klen%(2%) + pc_klen%(3%)
               if zz% > 25% then goto L51280     /* Exceeds Part Number */
        return
L51280:    errormsg$ = "(Error) - Invalid Field Number or Selection?"
           init(" ") pc_kfld$(x%), pc_kfld_d$(x%)
           pc_kbeg%(x%), pc_klen%(x%) = 0%
        return

L51330: REM Value Field Table Code                PC_VTBL$
           if pc_vtbl$ <> " " then goto L51360
              pc_vtbl$ = "00"
L51360:    convert pc_vtbl$ to zz%, data goto L51370
L51370:
           convert zz% to pc_vtbl$, pic(00)

           tab_2% = 0%
           tab_1% = 5%         /* (PRICE 005) - Field Definition Codes */
           tab_val$ = pc_vtbl$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L51500
               pc_vtbl_d$ = str(tab_desc$,1%,9%) & " "
               pc_vtln% = 0%
               convert str(tab_desc$,14%,2%) to pc_vtln%,data goto L51480
L51480:
        return
L51500:    errormsg$ = "(Error) - Invalid Value Field Code?    "
           pc_vtbl$, pc_vtbl_d$ = " "
        return

L51540: REM Value of Field Description Code       PC_VDESC$
           if pc_vdesc$ <> " " then goto L51570
              pc_vdesc$ = "000"
L51570:    convert pc_vdesc$ to zz%, data goto L51580
L51580:
           convert zz% to pc_vdesc$, pic(000)

           tab_2% = 0%
           tab_1% = 4%             /* (PRICE 004) - Field Descriptions */
           tab_val$ = pc_vdesc$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L51680
               pc_vdesc_d$ = str(tab_desc$,1%,20%) & " "
        return
L51680:    errormsg$ = "(Error) - Invalid Value Description?   "
           pc_vdesc$, pc_vdesc_d$ = " "
        return

        REM Value Field Table Value               PC_VTBLV$()
L51730:    x% = 1% : goto L51790               /* Note Tables May Have */
L51740:    x% = 2% : goto L51790               /* Alpha Values are     */
L51750:    x% = 3% : goto L51790               /*  COLOR, GLASS, LITING*/
L51760:    x% = 4% : goto L51790               /*  HINGE, LOCKS, SCREEN*/
L51770:    x% = 5% : goto L51790               /* (EWD001) - PRICE 029 */
L51780:    x% = 6%                             /*    thru PRICE 032    */
L51790:    convert pc_vtbl$ to tab_2%, data goto L52070
                                              /* Check for Valid Table */

           if tab_2% > 0% and tab_2% < 8% then goto L51870
                                                /* (EWD001) Wood Jamb  */
           if tab_2% > 20% and tab_2% < 29% then goto L51870
                                                /* (EWD001) Wood Jamb  */           
L51820:       pc_vtblv$(x%)   = "000"
              pc_vtblv_d$(x%) = " "
              if x% = 1% and pc_vdesc$ <> "000" then                     ~
                                            pc_vtblv_d$(x%) = pc_vdesc_d$
              return
L51870:    zz% = 0%                                /* MOD - 04/17/97   */
        REM  CONVERT PC_VTBLV$(X%) TO ZZ%, DATA GOTO 51890

             convert pc_vtblv$(x%) to zz%, data goto L51990
                                                   /* Alpha Value Found*/
               if x% <> 2% then goto N51990               /*EWD002*/      
                  if pc_vtblv$(x%) = "000" then goto L51820  /*EWD002*/
                  if pc_vtblv$(x%) <> "00" then goto N51990   /*EWD002*/
                  convert zz% to pc_vtblv$(x%), pic(00)      /*EWD002*/
                  goto L51990                                /*EWD002*/
                  
N51990:        if zz% = 000% then goto L51820      /* Not Applicable   */
               if pc_vtln% = 3% then                                     ~
                  convert zz% to pc_vtblv$(x%), pic(000)
               if pc_vtln% = 2% then                                     ~
                  convert zz% to pc_vtblv$(x%), pic(00)
               if pc_vtln% = 1% then                                     ~
                  convert zz% to pc_vtblv$(x%), pic(0)

L51990:    tab_val$ = pc_vtblv$(x%)
           gosub lookup_tab_2                         /* (EWD001)      */
           if tab_rec% = 0% then goto L52070
               str(pc_vtblv_d$(x%),1%,20%)  = pc_vdesc_d$
               str(pc_vtblv_d$(x%),20%,1%) = " "
               str(pc_vtblv_d$(x%),21%,10%) = str(tab_desc$,p%+1%,10%)
               str(pc_vtblv_d$(x%),30%,1%) = ":"
        return
L52070:    errormsg$ = "(Error) - Invalid Code Value Specified?"
           init(" ") pc_vtblv$(x%), pc_vtblv_d$(x%)
        return

L52110: REM Value Field Format Codes              PC_VFMT$
           if pc_vfmt$ <> " " then goto L52150
              pc_vfmt$ = "00"
              goto L52270
L52150:    convert pc_vfmt$ to zz%, data goto L52160
L52160:
           convert zz% to pc_vfmt$, pic(00)

           tab_2% = 0%
           tab_1% = 6%
           tab_val$ = pc_vfmt$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52270
               pc_vfmt_d$ = tab_desc$
               pc_vfmt_d$ = str(tab_desc$,1%, p%)
        return
L52270:    errormsg$ = "(Error) - Invalid Format Code Specified?"
           pc_vfmt$, pc_vfmt_d$ = " "
        return

L52310: REM Value Calculation Method              PC_VCALC$
           if pc_vcalc$ <> " " then goto L52340
              pc_vcalc$ = "00"
L52340:    convert pc_vcalc$ to zz%, data goto L52350
L52350:
           convert zz% to pc_vcalc$, pic(00)

           if pc_vcalc$ = "00" then goto L52460
           tab_2% = 0%
           tab_1% = 7%
           tab_val$ = pc_vcalc$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52460
               pc_vcalc_d$ = str(tab_desc$, 1%, p%-1%)
        return
L52460:    errormsg$ = "(Error) - Invalid Calculation Method?  "
           pc_vcalc$, pc_vcalc_d$ = " "
        return

L52500: REM Key Lookup Methods                    PC_KCALC$
           if pc_kcalc$ <> " " then goto L52530
              pc_kcalc$ = "00"
L52530:    convert pc_kcalc$ to zz%, data goto L52540
L52540:
           convert zz% to pc_kcalc$, pic(00)

           tab_2% = 0%
           tab_1% = 8%              /* (PRICE 008) - Key Lookup Method */
           tab_val$ = pc_kcalc$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52780
               pc_kcalc_d$ = tab_desc$
               pc_kcalc_d$ = str(tab_desc$,1%, p%-1%)
                                                /* VERIFY WIDTH/HEIGHT */
               if pc_kcalc$ = "4" and pc_kfld%(1%) <> 8% then goto L52780
                                                /* VERIFY UNITED INCH  */
              if pc_kcalc$ = "2" and pc_kfld%(1%) <> 15% then goto L52780
              if pc_kcalc$ = "8" and pc_kfld%(1%) <>  9% then goto L52780
              if pc_kcalc$ = "9" and pc_kfld%(1%) <>  8% then goto L52780
                                                /* VERIFY TABLE LOOKUP */
               if pc_kcalc$ <> "1" and pc_kcalc$ <> "6" then return
                  for i% = 1% to 3%
                      if pc_kfld%(i%) > 0% and pc_kfld%(i%) < 8% then    ~
                                                                   return
                  next i%
                  goto L52780                  /* Not a Valid Selection */
        return
L52780:    errormsg$ = "(Error) - Invalid Lookup Method? "
           pc_kcalc$, pc_kcalc_d$ = " "
        return

L52820: REM Key Unit Conversion Method            PC_KUNT$
           if pc_kunt$ <> " " then goto L52860
              pc_kunt$ = "00"
              goto L53000
L52860:    convert pc_kunt$ to zz%, data goto L52870
L52870:
           convert zz% to pc_kunt$, pic(00)

           tab_2% = 0%
           tab_1% = 9%          /* (PRICE 009) - Unit Conversion Table */
           tab_val$ = pc_kunt$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L53000
               pc_kunt_d$ = str(tab_desc$,1%, p%-1%)
               if pc_kfld%(1%) <> 15% then return
                  if pc_kunt$ <> "01" and pc_kunt$ <> "02" and           ~
                                         pc_kunt$ <> "09" then goto L53000
        return
L53000:    errormsg$ = "(Error) - Invalid Unit Conversion Code?"
           pc_kunt$, pc_kunt_d$ = " "
        return

        REM *************************************************************~
            *      R e p o r t   I n p u t   S c r e e n   E d i t s    *~
            *-----------------------------------------------------------*~
            * Edits for Items on Report Screen                          *~
            *************************************************************

        deffn'152(fieldnr%)
            tab_val$, errormsg$ = " "
              on fieldnr% gosub L54120,         /* Beg Ref Type Code    */~
                                L54360          /* End Ref Type Code    */
              return

L54120: REM Price Reference Code                  BEG_R$
           if beg_r$ <> " " then goto L54200
L54140:       beg_r$ = "AA"
              beg_r_d$ = "(A)ll Reference Type Codes"
              end_r$ = " "
              end_r_d$ = " "
              fieldnr% = 2%
              return
L54200:    if beg_r$ = "AA" then goto L54140
           convert beg_r$ to zz%, data goto L54220
L54220:
           convert zz% to beg_r$, pic(00)

           tab_2% = 0%
           tab_1% = 2%                        /* (PRICE 002) Ref Codes */
           tab_val$ = beg_r$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L54320
               beg_r_d$ = tab_desc$
        return
L54320:    errormsg$ = "(Error) - Invalid Beginning Reference Type Code?"
           beg_r$, beg_r_d$ = " "
        return

L54360: REM Price Reference Code                  END_R$
           if beg_r$ = "AA" then return

           if end_r$ <> " " then goto L54410
              end_r$ = beg_r$
L54410:    convert end_r$ to zz%, data goto L54420
L54420:
           convert zz% to end_r$, pic(00)

           tab_2% = 0%
           tab_1% = 2%                        /* (PRICE 002) Ref Codes */
           tab_val$ = end_r$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L54520
               end_r_d$ = tab_desc$
        return
L54520:    errormsg$ = "(Error) - Invalid Ending Reference Type Code?"
           end_r$, end_r_d$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                            /* HEADER  */
L55050: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55080: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55110: %!                                           ####################~
        ~#########################                             PAGE: ### !

L55140: %! Reference Code: ##  ##############################            ~
        ~                                      Date: ######## @ ######## !
L55160: %!Ref Calc Code: (###) ####################           Link: ###  ~
        ~                                                                !
L55180: %!  Key   (1) Descrpt No.: ###  ####################  Key (1) Fie~
        ~ld No.: ##  #########  Key Calc. Method: ## ####################!

L55210: %!  Key   (2) Descrpt No.: ###  ####################  Key (2) Fie~
        ~ld No.: ##  #########  Key Unit Convert: ## ####################!

L55240: %!  Key   (3) Descrpt No.: ###  ####################  Key (3) Fie~
        ~ld No.: ##  #########                                           !

L55270: %!  Value Field Number   :  ##  #########                        ~
        ~     Value (1) Field Descrpt: ### ##############################!

L55300: %!  Value Field Descript.: ###  ####################             ~
        ~     Value (2) Field Descrpt: ### ##############################!

L55330: %!  Value Field Format   :  ##  ####################             ~
        ~     Value (3) Field Descrpt: ### ##############################!

L55360: %!  Value Field Calc.    :  ##  ####################             ~
        ~     Value (4) Field Descrpt: ### ##############################!

L55390: %!                                                               ~
        ~     Value (5) Field Descrpt: ### ##############################!

L55420: %!                                                               ~
        ~     Value (6) Field Descrpt: ### ##############################!


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
          if lcnt% <> 99% then print using L55050
          page_no% = page_no% + 1%
          rpt_time$ = " "
          call "TIME" (rpt_time$)
          print page
          print using L55050
          print using L55110, print_title$, page_no%
          print using L55140, pc_r$, pc_r_d$, date$, rpt_time$
          lcnt% = 3%
          sav_r$ = pc_r$
        return

        print_detail
          if pc_r$ <> sav_r$ then gosub print_header
          if lcnt% > 54% then gosub print_header
          print using L55080
          print using L55160, pc_rc$, str(pc_rc_d$,1%,20%), pc_link$
          print using L55180, pc_kdesc$(1%),pc_kdesc_d$(1%),pc_kfld$(1%), ~
                             pc_kfld_d$(1%), pc_kcalc$, pc_kcalc_d$
          print using L55210, pc_kdesc$(2%),pc_kdesc_d$(2%),pc_kfld$(2%), ~
                             pc_kfld_d$(2%), pc_kunt$, pc_kunt_d$
          lcnt% = lcnt% + 4%
          if pc_kdesc$(3%) = "000" then goto L60320             /* SKIP */
          print using L55240, pc_kdesc$(3%),pc_kdesc_d$(3%),pc_kfld$(3%), ~
                             pc_kfld_d$(3%)
          lcnt% = lcnt% + 1%

L60320:   print using L55270, pc_vtbl$,  pc_vtbl_d$,  pc_vtblv$(1%),      ~
                                                     pc_vtblv_d$(1%)
          print using L55300, pc_vdesc$, pc_vdesc_d$, pc_vtblv$(2%),      ~
                                                     pc_vtblv_d$(2%)
          print using L55330, pc_vfmt$,  pc_vfmt_d$,  pc_vtblv$(3%),      ~
                                                     pc_vtblv_d$(3%)
          print using L55360, pc_vcalc$, pc_vcalc_d$, pc_vtblv$(4%),      ~
                                                     pc_vtblv_d$(4%)
          lcnt% = lcnt% + 4%
          if pc_vtblv$(5%) = "000" then goto L60460             /* SKIP */
          print using L55390,                         pc_vtblv$(5%),      ~
                                                     pc_vtblv_d$(5%)
          lcnt% = lcnt% + 1%

L60460:   if pc_vtblv$(6%) = "000" then goto L60500             /* SKIP */
          print using L55420,                         pc_vtblv$(6%),      ~
                                                     pc_vtblv_d$(6%)
          lcnt% = lcnt% + 1%
L60500: return

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$="Reference (Key) and (Value) Definition Report"
            date$ = date  :  call "DATEFMT" (date$)
            rpt_time$ = " "
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCPRC", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            print using L55050
            call "SETPRNT" ("APCPRC", " ", 0%, 1%)
        return

        generate_report
            init(" ") sav_r$, rpt_key$, rr$
            rpt% = 1%
          call "SHOSTAT" ("Printing Ref Calc. Definition Report")
            gosub select_printer
            if beg_r$ = "AA" then goto generate_next
               str(rpt_key$,1%,2%) = beg_r$
        generate_next
          read #1,key > rpt_key$, using L60790, rpt_key$,                 ~
                                                   eod goto generate_done
L60790:         FMT CH(05)
                                            /* TEST KEY FOR SELECTIONS */
             rr$ = str(rpt_key$,1%,2%)
          if beg_r$ = "AA" then goto L60860
             if rr$ < beg_r$ then goto generate_next
             if rr$ > end_r$ then goto generate_done

L60860:   pc_r$  = str(rpt_key$,1%,2%)
          pc_rc$ = str(rpt_key$,3%,3%)
          gosub dataload
          gosub data_convert
          gosub print_detail
             goto generate_next
        generate_done
            gosub close_printer
        return

        REM *************************************************************~
            *        S P E C I A L   T A B L E   R O U T I N E S        *~
            *************************************************************

        lookup_tab_1                   /* Load Data for Display Screen */
            on tab_1% gosub p_2, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9
               return
        p_2                                /* Lookup Reference Code Def*/
            tab_hdr$ = " Pricing Type Reference Codes "
            sav_key$ = "PRICE 002"
            goto L61360
        p_3                                /* Lookup Std/Spc Ref Calc  */
            tab_hdr$ = "Standard/Special Ref Calc Code"
            if len(pc_r_d$) > 1 then tab_hdr$ = pc_r_d$
            sav_key$ = "PRICE 003"
            goto L61360
        p_4                                /* Lookup Field Descriptions*/
            tab_hdr$ = "Pricing Field Description Code"
            sav_key$ = "PRICE 004"
            goto L61360
        p_5                                /* Lookup Field Definitions */
            tab_hdr$ = " Field Definitions - Part No. "
            sav_key$ = "PRICE 005"
            goto L61360
        p_6                                /* Lookup Price/Value Format*/
            tab_hdr$ = " Price/Value Format Type Code "
            sav_key$ = "PRICE 006"
            goto L61360
        p_7                                /* Lookup Price/Value Calc  */
            tab_hdr$ = "Price/Value Calculation Method"
            sav_key$ = "PRICE 007"
            goto L61360
        p_8                                /* Lookup (Key) Calc Method */
            tab_hdr$ = "  (Key) - Calculation Method  "
            sav_key$ = "PRICE 008"
            goto L61360
        p_9                                /* Lookup (Key) Unit Convert*/
            tab_hdr$ = "(Key) - Unit Conversion Method"
            sav_key$ = "PRICE 009"

L61360:     init(" ") readkey$
            str(readkey$,1%,9%) = sav_key$
            if tab_1% <> 3% then goto L61400
               str(readkey$,10%,2%) = pc_r$
L61400: REM - TEST LOOKUP
           descr$ =hex(06)& tab_hdr$
           call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2))
        return

                                           /* (EWD001) - Sub Modified  */
        lookup_tab_2                       /* Get Value from Code Desc */
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                            #2, tab_rec%)
        return                             /* (EWD001) - End           */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
