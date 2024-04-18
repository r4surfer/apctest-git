        REM *************************************************************~
            *                                                           *~
            *   AAA   W   W  DDDD   RRRR   PPPP   TTTTT   000   4   4   *~
            *  A   A  W   W  D   D  R   R  P   P    T    0   0  4   4   *~
            *  AAAAA  W w W  D   D  RRRRR  PPPP     T    0   0  44444   *~
            *  A   A  Ww wW  D   D  R  R   P        T    0   0      4   *~
            *  A   A  W   W  DDDD   R   R  P        T     000       4   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDRPT04 - New Loading Report for Shipping                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/07/05 ! New Program for Loading - Last Mod Date  ! RHH *~
            * 04/26/05 ! (AWD001)Mod to Status Code Print         !     *~
            * 01/01/06 ! (PAR000) CR347 New Sub Part No.          ! RHH *~
            *10/02/2008! (AWD002) mod to drop number              ! DES *~
            *10/22/2014! (AWD003) mod to add screen code "7"      ! PWW *~
            *02/22/2017! (SR79716) mod to show trans-it & rga stat! CMN *~
            *************************************************************

        dim                                                              ~
            readkey$24, rhh$6,           /* Gencode Key                */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            sc_type$1, sc_type_d$30,     /* Analysis Type Selection    */~
            sc_load$5, sc_load_d$30,     /* Appian Load Number         */~
            sc_drop$2, sc_drop_d$30,     /* Load Drop Number           */~
            ld_app_key0$5, ld_app_rec$128, /* (APCPLNLD) File          */~
            sc_key1$27,                  /* (APCPLNSC) - File          */~
            ls_key$20,                   /* (AWDAPPLS) - File          */~
            dt_key3$23, dt_rec$256,      /* (APCPLNDT) - File          */~
            dt_dept$3, dt_st$2, dt_drop$,/* Dept, Status, Drop         */~
            or_key4$8, or_po$16,         /* (APCPLNOR) - File          */~
            wrk_key$44, wrk_rec$222,     /* Work File Key              */~
            drop_key$50,                 /* Model Code                 */~
            model$3,                     /* Model Code                 */~
            scr$1,                       /* Screen Code                */~
            r_col$3, r_dropx$8,          /* Counter                    */~
            r_drop$2, sav_drop$2,        /* Drop Number                */~
            r_bar$18,                    /* Barcode Number             */~
            r_po$16,                     /* Po Number                  */~
            r_part$25,                   /* Part Number                */~
            r_sub_part$20,               /* New Sub Part No.   (PAR000)*/~
            r_stat$2, stat$(50%)6,       /* MFG Status Code            */~
            r_cust$9,                    /* Customer Code              */~
            r_seq$4,                     /* Production Sequence No.    */~
            r_flag$(10%)1,               /* Analysis Flags             */~
            r_flag%(10%), r_flagd%(10%), /* Analysis and Drop Totals   */~
            r_flag_tot$(10%)4, r_flagd$(10%)4,/* Analysis and Drop Totals*/~
            total$4, total1$4,           /* Total Pieces               */~
            warranty$4,                  /* Total of Warranty          */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */
                                         /* (PAR000)                   */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

                                         /* (PAR000)                   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$35, pname$21
            apc$   = "Analysis Report of Loading/Staging "
            pname$ = "AWDRPT04 - 01/01/2006"

        REM *************************************************************

            mat f2% = con

            mat f1% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APCPLNDT ! Production Master Detail                 *~
            * #03 ! GENCODES ! System Master Code Table Files           *~
            * #04 ! APCPLNLD ! Planning Load Master - Old APCMAST       *~
            * #05 ! APCPLNSC ! Planning Master Schedule-Old APCLINES    *~
            * #06 ! APCPLNOR ! Planning S.O. Header History-Old APCORDER*~
            * #07 ! AWDAPPLS ! Appian Shipping label file       (AWD002)*~
            * #10 ! APCRPTWK ! Report Work File                         *~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01,   "APCPLNDT",                                    ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup


            select #03,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #04, "AWDAPPLD",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =    5,                    ~
                        alt key 1,  keypos =  1,   keylen =  16,         ~
                            key 2,  keypos =  2,   keylen =  15,         ~
                            key 3,  keypos = 17,   keylen =  15

            select #05, "APCPLNSC",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #06,  "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #07, "AWDAPPLS",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1,  keypos = 21,   keylen =  34, dup ,   ~
                            key 2,  keypos = 23,   keylen =  32, dup ,   ~
                            key 3,  keypos = 56,   keylen =  10, dup

            select #10, "AWDRPTWK",                                      ~
                        varc,     indexed,  recsize =   266,             ~
                        keypos =    1, keylen =  50

                                                        /* (PAR000)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR000)     */
            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#04, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#05, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#06, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#07, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

                                               /* (AWD001)             */
            init(" ") stat$()

            stat$( 3%) = "Planed"              /* Planned for Analysis */
            stat$( 4%) = "SchedM"              /* Scheduled Make       */
            stat$( 6%) = "PStock"              /* Pull from Stack Inv. */
            stat$( 7%) = "PShape"              /* Pull Shape from Stock*/
            stat$( 8%) = "SStock"              /* Scheduled Stock Make */
            stat$(10%) = "I-Prod"              /* In Production        */
            stat$(11%) = "W-Remk"              /* Wood Surround Remake */
            stat$(12%) = "Complt"              /* Manufature Complete  */
            stat$(13%) = "W-Recv"              /* Wood Surround Recevied*/
            stat$(14%) = "Staged"              /* Product Staged       */
            stat$(15%) = "In-RGA"              /* In RGA               */
            stat$(16%) = "Loaded"              /* Product Loaded       */
            stat$(18%) = "B.O.L."              /* B.O.L Printed        */
            stat$(19%) = "Remove"              /* Removed From Load    */
            stat$(20%) = "Invoic"              /* Product Invoiced     */
            stat$(26%) = "TranIt"              /* TransIt (SR79716)    */
            stat$(32%) = "RGA   "              /* RGA     (SR79716)    */

            stat$(25%) = "******"              /* Other                */
                                               /* (AWD001)             */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub open_work_file

            gosub generate_report
        return clear all
        REM GOTO INPUTMODE
        goto exit_program

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
         "Enter Analysis Type 1=Not Loaded,2=Not Staged,3=Loaded,4=Staged,A=All Product?",~
         "Enter a Valid Appian Load Number?                            ",~
         "Enter a Valid Drop Number, or 'AL' = ALL?                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_type$, sc_type_d$,      ~
                      sc_load$, sc_load_d$, sc_drop$, sc_drop_d$


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
        REM DATALOAD
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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
              on fieldnr% gosub L40180,         /* Analysis Type    */   ~
                                L40180,         /* Load Number      */   ~
                                L40180          /* Drop Number      */

              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,63), "Today:",                                     ~
               at (01,70), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Analysis Type (1234A) :",                    ~
               at (06,26), fac(lfac$(1%)), sc_type$             , ch(01),~
               at (06,40), fac(hex(84)), sc_type_d$             , ch(30),~
                                                                         ~
               at (07,02), "Production Load No.   :",                    ~
               at (07,26), fac(lfac$(2%)), sc_load$             , ch(05),~
               at (07,40), fac(hex(84)), sc_load_d$             , ch(30),~
                                                                         ~
               at (08,02), "Drop Seq No. or 'AL'  :",                    ~
               at (08,26), fac(lfac$(3%)), sc_drop$             , ch(02),~
               at (08,40), fac(hex(84)), sc_drop_d$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40560
                  call "PRNTSCRN"
                  goto L40210

L40560:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40750     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40710
                str(pf$(3%),64%)    = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40710:     if fieldnr% > 1% then L40730
                str(pf$(2%),18%,26%) = " "  :  str(pfkeys$,4%,1%) = hex(ff)
L40730:     return

L40750: if fieldnr% > 0% then L40840  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40840:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                                        " &       ~
                     "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50150,         /* Analysis Type 1 or 2  */ ~
                              L50200,         /* Load Number           */ ~
                              L50300          /* Drop Number           */
            return


L50150: REM Valid Analysis Type               sc_type$, sc_type_d$
            p% = pos("1234A" = sc_type$)
            if p% = 0% then goto L50160

            if sc_type$ = "1" then                                ~
                              sc_type_d$ = "1-Analysis-Product Not Loaded."
            if sc_type$ = "2" then                                ~
                              sc_type_d$ = "2-Analysis-Product Not Staged."
            if sc_type$ = "3" then                                ~
                              sc_type_d$ = "3-Analysis-Product Loaded    ."
            if sc_type$ = "4" then                                ~
                              sc_type_d$ = "4-Analysis-Product Staged    ."
            if sc_type$ = "A" then                                ~
                              sc_type_d$ = "A-Analysis-All Product on Load"

        return

L50160:        errormsg$ = "(Error) Invalid Analysis Type Selection (1234 or A)?"
               init(" ") sc_type$, sc_type_d$
               gosub error_prompt
        return

L50200: REM Valid Load Number                  sc_load$, sc_load_d$
            sc_load% = 0%
            convert sc_load$ to sc_load%, data goto L50210
L50210:
            convert sc_load% to sc_load$, pic(00000)
                                                     /* (APCPLNLD) File     */

            sc_load% = 0%
            init(" ") ld_app_key0$, ld_app_rec$
            ld_app_key0$ = sc_load$

            read #4,key 0% = ld_app_key0$, using L50220, ld_app_rec$, eod goto L50230
L50220:        FMT CH(128)

                                                     /* Check for Appian    */
            if str(ld_app_rec$,83%,1%) = "Y" then sc_load% = 1%
                                                     /* Check Non-Appian    */
            if str(ld_app_rec$,83%,1%) = "N" then sc_load% = 2%

               sc_load_d$  = str(ld_app_rec$,53%,30%)
                                               /* Check for Valid         */
                                               /* Appian/Non-Appian Load  */
               if sc_load% = 0% then goto L50230

        return
L50230:
               errormsg$ = "(Error) Invalid Appian Load? "
               gosub error_prompt
               init(" ") sc_load$, sc_load_d$
        return

L50300: REM Valid Drop No.                     sc_drop$, sc_drop_d$
            if sc_drop$ <> "AL" then goto L50310
L50305:        sc_drop$   = "AL"
               sc_drop_d$ = "(ALL) - Drops"
               return
L50310:
            sc_drop% = 0%
            if sc_drop$ <> " " then goto L50320
               goto L50305
                                               /* (APCPLNSC) File       */
L50320:     init(" ") sc_key1$
            str(sc_key1$,1%,5%) = sc_load$
L50330:     read #5,key 1% > sc_key1$, using L50340, sc_key1$,            ~
                                                           eod goto L50350
L50340:        FMT XX(6), CH(27)
            if str(sc_key1$,1%,5%)   <> sc_load$ then goto L50350
            if str(sc_key1$,11%,2%)  <> sc_drop$ then goto L50330
            convert sc_drop$ to sc_drop%, data goto L50350

            sc_drop_d$ = "Specified Drop Number"
        return
L50350:     errormsg$ = "(Error) - Invalid Drop Number For Load?"
            gosub error_prompt
            init(" ") sc_drop$
        return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########                           #################~
        ~#######################                                 AWDRPT04:~
        ~!

L55090: %!Analysis Type     :     # ##############################       ~
        ~                                                      Page: #####~
        ~!

L55130: %!Appian Load Number: ##### ##############################       ~
        ~                                                      Drop: ###  ~
        ~!

                                                   /* Customer Header */
L55210: %!Col!Drop!Production Barcode!  P.O.  Number  !<- MFG Part Number~
        ~ ----->!Status!Customer !Seq.!Sash!Samp!Part!Scr!B/B!Mul!Pat!Wind~
        ~!

L55250: %!---!----!------------------!----------------!------------------~
        ~-------!------!---------!----!----!----!----!---!---!---!---!----~
        ~!

L55290: %!###!####!##################!################!##################~
        ~#######!######!#########!####!  # ! #  ! #  ! # ! # ! # ! # ! #  ~
        ~!

L55320: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55360: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!
                                                    /* Total Line      */
L55400: %!Report Totals: ####  Warranty: ####          Analysis Totals   ~
        ~                           !####!####!####!####!###!####!###!####~
        ~!

L55410: %!Drop Totals: Pieces: ####                    Analysis Totals   ~
        ~                           !####!####!####!####!###!####!###!####~
        ~!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("AWDRPT", " ",25000%, 0%)
            select printer (134)

            page_no% = 0%
            lcnt%    = 99%

            print_title$ = "AWD Loading/Staging Analysis Report  "
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            company$ = "ATRIUM Windows and Doors"
            call "FMTTITLE" (company$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("AWDRPT", " ",0%, 1%)
        return

        generate_report
                                         /* Check label file for data */
            gosub selecting_data
                                         /* Print sorted report       */

            call "SHOSTAT" ("PRINTING REPORT")
            CALL "PAUSE" ADDR(50%)

            gosub select_printer

            init(" ") wrk_key$, wrk_rec$, sav_drop$

            count%    = 0%                /* Total for Report           */
            count_d%  = 0%                /* Total for Drop             */
            warranty% = 0%
            sav_drop$ = "XX"
            sav_drop% = 0%
            mat r_flag%  = zer

            mat r_flagd% = zer

            wrk_key$ = all(hex(00))
            drop_key$ = all(hex(00))

        generate_report_next
            init(" ") r_col$, r_drop$, r_bar$, r_po$, r_part$, r_stat$, ~
                      r_cust$, r_seq$, r_flag$(), model$, scr$, r_sub_part$
                                                /* (PAR000)           */
            init(" ") flag$, pgm$, so_inv$, item_no$, bcksubpt_rec$,    ~
                      flds$(), info_flds$()
                                                /* (PAR000)           */

            read #10,key > drop_key$, using GEN_1, drop_key$, wrk_rec$,  ~
                                                  eod goto generate_done
GEN_1:         FMT CH(50), CH(222)
            wrk_key$ = str(drop_key$,7,44)
            if sav_drop$ = "XX" then sav_drop$ = str(wrk_rec$,4%,2%)
                                               /* Check Drop         */
            if sav_drop$ <> str(wrk_rec$,4%,2%) then gosub print_drop_totals

            count%   = count% + 1%             /* Report total       */
            count_d% = count_d% +1%            /* Drop Total         */

                                               /* Column             */
            convert count% to r_col$, pic(000)

            r_drop$  = str(wrk_rec$,4%,2%)     /* Drop Number        */
                                               /* (AWD002)           */
            r_dropx$ = str(wrk_rec$,115%,8%)     /* Drop Number        */

            r_bar$   = str(wrk_rec$,6%,18%)    /* Barcode            */

            r_po$    = str(wrk_rec$,24%,16%)   /* P.O. Number        */

            r_part$  = str(wrk_rec$,40%,25%)   /* Part Number        */

            if str(r_part$,5%,4%) = "WARR" then warranty% = warranty% + 1%

                                               /* (AWD001)           */
            r_stat$  = str(wrk_rec$,65%,2%)    /* Status Code        */
            r_stat%, s_stat% = 0%
            convert r_stat$ to r_stat%, data goto GEN_2
GEN_2:
            s_stat% = r_stat%
            if r_stat% < 3% or r_stat% > 20% then r_stat% = 25%
/* (SR79716) */
            if s_stat% = 26% or s_stat% = 32% then r_stat% = s_stat%
                                               /* (AWD001)           */

            r_cust$  = str(wrk_rec$,67%,9%)    /* Customer Code      */

            r_seq$   = str(wrk_rec$,77%,4%)    /* Production Seq. No.*/

            r_flag$(1%) = str(wrk_rec$,100%,1%)   /* Sashes          */
            if r_flag$(1%) = "X" then r_flag%(1%) = r_flag%(1%) + 1%
                                                  /* Sashes Drop     */
            if r_flag$(1%) = "X" then r_flagd%(1%) = r_flagd%(1%) + 1%

            r_flag$(2%) = str(wrk_rec$,101%,1%)   /* Samples         */
            if r_flag$(2%) = "X" then r_flag%(2%) = r_flag%(2%) + 1%
                                                  /* Samples Drop    */
            if r_flag$(2%) = "X" then r_flagd%(2%) = r_flagd%(2%) + 1%

            r_flag$(3%) = str(wrk_rec$,102%,1%)   /* Parts           */
            if r_flag$(3%) = "X" then r_flag%(3%) = r_flag%(3%) + 1%
                                                  /* Parts Drop      */
            if r_flag$(3%) = "X" then r_flagd%(3%) = r_flagd%(3%) + 1%

            r_flag$(4%) = str(wrk_rec$,103%,1%)   /* Screens         */
            if r_flag$(4%) = "X" then r_flag%(4%) = r_flag%(4%) + 1%
                                                  /* Screens Drop    */
            if r_flag$(4%) = "X" then r_flagd%(4%) = r_flagd%(4%) + 1%

            r_flag$(5%) = str(wrk_rec$,104%,1%)   /* Bay/Bow         */
            if r_flag$(5%) = "X" then r_flag%(5%) = r_flag%(5%) + 1%
            if r_flag$(5%) = "G" then r_flag%(5%) = r_flag%(5%) + 1%
                                                  /* Bay/Bow Drop    */
            if r_flag$(5%) = "X" then r_flagd%(5%) = r_flagd%(5%) + 1%
            if r_flag$(5%) = "G" then r_flagd%(5%) = r_flagd%(5%) + 1%

            r_flag$(6%) = str(wrk_rec$,105%,1%)   /* Mulled          */
            if r_flag$(6%) = "X" then r_flag%(6%) = r_flag%(6%) + 1%
                                                  /* Mulled Drop     */
            if r_flag$(6%) = "X" then r_flagd%(6%) = r_flagd%(6%) + 1%

            r_flag$(7%) = str(wrk_rec$,106%,1%)   /* Patio doors     */
            if r_flag$(7%) = "X" then r_flag%(7%) = r_flag%(7%) + 1%
                                                  /* Patio Drop      */
            if r_flag$(7%) = "X" then r_flagd%(7%) = r_flagd%(7%) + 1%

            r_flag$(8%) = str(wrk_rec$,107%,1%)   /* Windows         */
            if r_flag$(8%) = "X" then r_flag%(8%) = r_flag%(8%) + 1%
                                                  /* Windows Drop    */
            if r_flag$(8%) = "X" then r_flagd%(8%) = r_flagd%(8%) + 1%

            gosub print_detail

            goto generate_report_next
        generate_done

            gosub print_total

            gosub close_printer
            call "FILEBGON" addr(#10)
        return

        print_header                        /* Page Header       */
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55320
          lcnt% = 0%
          print page
          print using L55320
          print using L55050, date$, rpt_time$, company$
          print using L55090, sc_type$, sc_type_d$, page_no%
          print using L55130, sc_load$, sc_load_d$, sc_drop$
          print using L55360
          print using L55210
          lcnt% = lcnt% + 6%
        return

        print_detail                        /* Line Item Detail  */

          if lcnt% > 57% then gosub print_header
                                            /* Print Columns     */
          if sav_drop% = 0% then print using L55250               ~
                            else print using L55360

                                            /* print Detail      */
          sav_drop% = 0%
          goto print_a

        return

        print_a                             /* (AWD001)          */
REM       print using L55290, r_col$, r_drop$, r_bar$, r_po$, r_part$
          temp$ = str(r_dropx$,4,4)
      if temp$ = "    " then temp$ = r_drop$
          print using L55290, r_col$, temp$, r_bar$, r_po$, r_part$,   ~
                stat$(r_stat%), r_cust$, r_seq$, r_flag$(1%), r_flag$(2%),~
                r_flag$(3%), r_flag$(4%), r_flag$(5%), r_flag$(6%),       ~
                r_flag$(7%), r_flag$(8%)

          lcnt% = lcnt% + 2%
        return

        print_total                              /* Column Totals */
            init(" ") r_flag_tot$(), total$
            convert count% to total$, pic(####)

            for i% = 1% to 10%
                convert r_flag%(i%) to r_flag_tot$(i%), pic(####)

            next i%

            convert warranty% to warranty$, pic(####)

                                            /* (AWD001)             */
            print using L55360
            print using L55400, total$, warranty$,                   ~
                                r_flag_tot$(1%), r_flag_tot$(2%),    ~
                                r_flag_tot$(3%), r_flag_tot$(4%),    ~
                                r_flag_tot$(5%), r_flag_tot$(6%),    ~
                                str(r_flag_tot$(7%),2%,3%), r_flag_tot$(8%)
            print using L55320
        return

        print_drop_totals                   /* Department Totals */

            if lcnt% > 57% then gosub print_header
                                            /* Print Columns     */

            init(" ") r_flagd$(), total1$
            convert count_d% to total1$, pic(####)

            for i% = 1% to 10%
                convert r_flagd%(i%) to r_flagd$(i%), pic(####)

            next i%

                                            /* (AWD001)             */
            print using L55360
            print using L55410, total1$,                             ~
                                      r_flagd$(1%), r_flagd$(2%),    ~
                                      r_flagd$(3%), r_flagd$(4%),    ~
                                      r_flagd$(5%), r_flagd$(6%),    ~
                                      str(r_flagd$(7%),2%,3%), r_flagd$(8%)

            lcnt% = lcnt% + 2%

            count_d% = 0%

            mat r_flagd% = zer

            sav_drop$ = str(wrk_rec$,4%,2%)    /* Set Next Drop    */

            sav_drop% = 1%

        return

        selecting_data
             call "SHOSTAT" ("Selecting Analysis Data for Load")

             count% = 0% : cnt% = 0%

             dt_key3$ = all(hex(00))
             str(dt_key3$,1%,5%)  = sc_load$
             read #1,key 3% > dt_key3$, using SEL_1, dt_rec$,                  ~
                                                   eod goto selecting_data_done
SEL_1:          FMT CH(256)

             goto SEL_3
        selecting_data_next
             if mod(count%,25%) <> 0 then goto SEL_2
                convert count% to count$, pic(######)
                call "SHOSTAT" ("Production Records Checked ("&count$&")")

SEL_2:       read #1, using SEL_1, dt_rec$, eod goto selecting_data_done

SEL_3:       count% = count% + 1%
             convert count% to rhh$, pic(###)

             if sc_load$ <> str(dt_rec$,1%,5%) then goto selecting_data_done
                dt_dept$ = str(dt_rec$,42%,3%)     /* Department Code */
                gosub check_support
                if supp% = 1% then goto selecting_data_next
                dt_st$   = str(dt_rec$,64%,2%)     /* Current Status  */
                dt_drop$ = str(dt_rec$,11%,2%)     /* Drop No.        */
                if sc_drop$ = "AL" then goto SEL_4 /* Check All Drops */
                                                   /* Specified Drop  */
                   if dt_drop$ <> sc_drop$ then goto selecting_data_next
SEL_4:
                                                   /* Check Status    */
                if sc_type$ <> "1" then goto SEL_5
                                                   /* Check Not Loaded*/
                if dt_st$ < "16" then goto SEL_6          /* (SR79716) */
                if dt_st$ = "26" or dt_st$ <> "32" then goto SEL_6
                      goto selecting_data_next
SEL_5:                                             /* Check Not Staged*/
                if sc_type$ <> "2" then goto SEL_5A

                if dt_st$ < "14" then goto SEL_6
                if dt_st$ = "26" or dt_st$ <> "32" then goto SEL_6
                      goto selecting_data_next
SEL_5A:                                            /* Loaded Product  */
                if sc_type$ <> "3" then goto SEL_5C

                   if dt_st$ = "16" then goto SEL_6
                      goto selecting_data_next
SEL_5C:                                            /* Staged Product  */
                if sc_type$ <> "4" then goto SEL_5D

                   if dt_st$ = "14" then goto SEL_6
                      goto selecting_data_next

SEL_5D:                                            /* All Product     */
                                                   /* All Status's    */
SEL_6:
                                            /***** Selected Data ******/
        REM     call "SHOSTAT" ("In Report ----> " & rhh$ )
        REM     stop

               cnt% = cnt% + 1%
               init(" ") wrk_key$, wrk_rec$
               dt_drop% = 0%               /* Reverse Drop Seq       */
               convert dt_drop$ to dt_drop%, data goto SEL_6A
SEL_6A:
                                           /* Build Sort Key         */
               dt_drop% = 99% - dt_drop%
               convert dt_drop% to str(wrk_key$,1%,2%), pic(##)

                                           /* Set Prod Barcode       */
               str(wrk_key$,3%,18%) = str(dt_rec$,24%,18%)
                                           /* Set Department Code    */
               str(wrk_key$,21%,3%) = str(dt_rec$,42%,3%)
                                           /* Set Process Code       */
               str(wrk_key$,24%,2%) = str(dt_rec$,45%,2%)

                                           /* Build Sort Record      */
                                           /* Store Counter          */
               str(wrk_rec$,1%,3%) = "000"

                                           /* Store Drop Number      */
               str(wrk_rec$,4%,2%) = dt_drop$
                                           /* Store Barcode          */
               str(wrk_rec$,6%,18%) = str(dt_rec$,24%,18%)
                                           /* Store P.O. Number      */
               gosub lookup_po

               str(wrk_rec$,24%,16%) = or_po$
                                        /* Store Part Number          */
               str(wrk_rec$,40%,25%) = str(dt_rec$,189%,25%)

               r_part$  = str(dt_rec$,189%,25%)   /* Part Number      */

               model$   = str(r_part$,1%,3%)      /* Model Code       */

               scr$     = str(r_part$,11%,1%)     /* Screen Flag      */

                                        /* Store Status Code          */
               str(wrk_rec$,65%,2%)  = dt_st$
                                        /* Store Customer Code        */
               str(wrk_rec$,67%,9%%) = str(dt_rec$,124%,9%)
                                        /* Production Sequence No.    */
               str(wrk_rec$,76%,5%)  = str(dt_rec$,111%,5%)
                                        /* Available                  */
               str(wrk_rec$,81%,19%) = " "

               ls_key$ = str(dt_rec$,24,18) & str(dt_rec$,11,2)
               str(wrk_rec$,115%,8%) = "        "
               read #7, key = ls_key$, using AWDAPPLS, ls_drop$, eod goto notfnd
               str(wrk_rec$,115%,8%) = ls_drop$
                                        /* Build Flags                */
                                        /* Sashes                     */
        REM       if str(dt_rec$,214%,1%) <> "0" then                   ~
        REM                                   str(wrk_rec$,100%,1%) = "X"

               if scr$ = "4" or scr$ = "5" then                         ~
                                              str(wrk_rec$,100%,1%) = "X"
               if scr$ = "4" or scr$ = "5" then                         ~
                                              goto selecting_data_update

                                           /* Samples                    */
               if str(dt_rec$,216%,1%) <> "0" then                      ~
                                              str(wrk_rec$,101%,1%) = "X"
               if str(dt_rec$,216%,1%) <> "0" then                      ~
                                              goto selecting_data_update

                                           /* Screen                    */
               gosub lookup_screens

               if screens% = 1%          then str(wrk_rec$,103%,1%) =  "X"
               if screens% = 1%          then goto selecting_data_update


                                        /* Parts                       */
               gosub lookup_parts

               if parts% = 1%            then str(wrk_rec$,102%,1%) = "X"
               if parts% = 1%            then goto selecting_data_update

                                        /* Bay/Bow Product              */
               if str(dt_rec$,189%,1%) = "9" then                        ~
                                              str(wrk_rec$,104%,1%) = "X"
                                        /* Check for Garden             */
               if str(dt_rec$,189%,3%) = "998" then                      ~
                                              str(wrk_rec$,104%,1%) = "G"
               if str(dt_rec$,189%,3%) = "997" then                      ~
                                              str(wrk_rec$,104%,1%) = "G"
               if str(dt_rec$,189%,1%) = "9" then                        ~
                                              goto selecting_data_update

                                        /* Mulled Product               */
               if str(dt_rec$,217%,3%) <> "000" then                     ~
                                              str(wrk_rec$,105%,1%) = "X"
               if str(dt_rec$,217%,3%) <> "000" then                     ~
                                              goto selecting_data_update

                                        /* Patio Door                   */
               if dt_dept$ = "023" then        str(wrk_rec$,106%,1%) = "X"
               if dt_dept$ = "023" then        goto selecting_data_update

                                        /* Just a Window                */
               str(wrk_rec$,107%,1%) = "X"
                                        /* new drop code    AWD002      */
               ls_key$ = str(dt_rec$,24,18) & str(dt_rec$,11,2)
               str(wrk_rec$,115%,8%) = "        "
readit:        read #7, key = ls_key$, using AWDAPPLS, ls_drop$, eod goto notfnd
AWDAPPLS:      FMT POS(1017), CH(8)
               str(wrk_rec$,115%,8%) = ls_drop$
notfnd:
        selecting_data_update
           init(" ") drop_key$
        tmp% = 0%
           convert str(wrk_rec$,118,2) to tmp%, data goto dataerr
           goto dataok
dataerr:       convert str(wrk_rec$,4,2) to tmp%, data goto dataok
dataok:        tmp% = 100% - tmp%
           convert tmp% to tmp$, pic (00)
               drop_key$ = tmp$
REM            drop_key$ = str(wrk_rec$,118%,4%)     /* Drop Number        */
               str(drop_key$,3,4) = str(wrk_rec$,118%,4%)
               str(drop_key$,7,44) = wrk_rec$
               gosub update_work
               goto selecting_data_next
        selecting_data_done
              if count% = 0% then goto selecting_data_err
        REM         convert count% to count$, pic(####)

        REM         call "SHOSTAT" ("Selected records found----> " & count$)
        REM         stop

        return
selecting_data_err:
            errormsg$ = "No Data Found"
            gosub error_prompt
        return

        update_work
            read #10, hold, key = drop_key$, eod goto write_work


             delete #10
write_work:

            write #10, using UPD_1, drop_key$, wrk_rec$, eod goto UPD_2

UPD_1:         FMT CH(50), CH(222)
        return
UPD_2:
            call "SHOSTAT" ("(Error) Barcode Number - " & str(wrk_key$,24%,18%) )
            stop
        return

        open_work_file
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 500%, rslt$(10%))
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_support
           supp% = 0%
           if dt_dept$ = "102" or dt_dept$ = "104" then                  ~
                                                  goto check_support_done
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #3,key = readkey$, eod goto check_support_done
/*replace*/ supp% = 1%
        check_support_done
        return

        lookup_po
            init(" ") or_key4$, or_po$
            or_key4$ = str(dt_rec$,24%,8%)
            read #6,key 4% = or_key4$, using lookup_po_1, or_po$,        ~
                                       eod goto lookup_po_done
lookup_po_1:   FMT POS(36), CH(16)
lookup_po_done
        return

        lookup_screens
           screens% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "SCREENONL"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, eod goto lookup_screens_done
           screens% = 1%
        lookup_screens_done
           if scr$ = "8" then screens% = 1%   /* Screen Only           */
        return

        lookup_parts                          /* Check Parts Table     */
           parts% = 0%


           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLANPARTS"
           str(readkey$,10%,15%) = model$
           read #3,key = readkey$, eod goto lookup_parts_done
           parts% = 1%
        lookup_parts_done
                                              /* Check Part No. Length */
           if len( str(dt_rec$,189%,25%) ) < 19 then parts% = 1%
                                              /* Check Part Flag       */
           if str(dt_rec$,215%,1%) = "Y" then parts% = 1%
                                              /* FGO's                 */
/*         if scr$ = "6" then parts% = 1%                           */
           if scr$ = "6" or scr$ = "7" then parts% = 1%     /*AWD003*/

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end
