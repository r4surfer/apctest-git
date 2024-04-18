        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   EEEEE   QQQQ   U   U   AAA   TTTTT  *~
            *  A   A  P   P  C   C  E      Q    Q  U   U  A   A    T    *~
            *  AAAAA  PPPP   C      EEEE   Q    Q  U   U  AAAAA    T    *~
            *  A   A  P      C   C  E      Q  Q Q  U   U  A   A    T    *~
            *  A   A  P       CCC   EEEEE   QQQQQ   UUU   A   A    T    *~
            *                                    Q                      *~
            *-----------------------------------------------------------*~
            * APCEQUAT - Equation and Part Cross-Reference File         *~
            *    - Record Type (1-Glass  - All Models (Top),<> Bot '1'  *~
            *                  (2-Screen - Standard/Special/Full        *~
            *                  (3-Glass  - Cottage/Oriel/(1??)Top       *~
            *                  (4-Screen - Cottage/Oriel                *~
            *                  (5-Glass  - Bottom (1??)                 *~
            *                  (6-Glass  - Cottage/Oriel Bottom (1??)   *~
            *                  (7-Glass  - Bay/Bow                      *~
            *                  (8-Screen - Bay/Bow                      *~
            *    - Model Number                                         *~
            *    - Primary Code  (Glass,Screen)                         *~
            *    - Hinge Code                                           *~
            *    - CLMR ( Y or N )                                      *~
            *    - WALLWIDT ( Y or N )                                  *~
            *    - Phantom Designator                                   *~
            *    - Filler                                               *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/07/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/05/97 ! Change revision number to 60403          ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            eq_key$8,                    /* PRIMARY KEY                */~
            sav_model$1,                 /* SAVE PRODUCT LINE CODE     */~
            desc$(10)40,                 /* Type Descriptions          */~
            eq_type$1,                   /* Type ( 1 thru 2)           */~
            eq_type_rpt$1,               /* Type ( 1 thru 2)           */~
            eq_model$3,                  /* Model Code                 */~
            eq_model_rpt$3,              /* Model Code                 */~
            eq_cd1$2,                    /* Primary Code - for Type    */~
            eq_cd2$2,                    /* Hinge Code                 */~
            eq_clmr$1,                   /* CLMR ( Y or N )            */~
            eq_wallwidt$1,               /* WALLWIDT ( Y or N )        */~
            eq_phantom$4,                /* Applicable Phantom Desig.  */~
            eq_filler$2,                 /* Filler Area                */~
            readkey$24,                  /* GENCODES LOOKUP KEY        */~
            type_desc$30,                /* TXN Type Description       */~
            type_desc_rpt$30,            /* TXN Type Description       */~
            model_desc$30,               /* Model Description          */~
            model_desc_rpt$30,           /* Model Description          */~
            cd1_desc$30,                 /* Primary Code Desc$         */~
            cd2_desc$30,                 /* Hinge Description          */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10),                     /* = 0 if the file is open    */~
            f1%(10),                     /* = 1 if READ was successful */~
            fs%(10),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/05/97 APC New Program                "
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
            * #01 ! APCEQUAT ! Equation and Part Cross-Reference        *~
            * #02 ! GENCODES ! Master Code Table File                   *~
            * #03 !          !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),500%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))

            f1%(1), f1%(2), f1%(3) = 0%

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

            for fieldnr% = 1% to  7%
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
L10200:               if keyhit% = 14% then gosub generate_report
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
                  if keyhit%  = 12% then gosub delete_reference
                  if keyhit%  = 14% then gosub generate_report
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        generate_report
            eq_type_rpt$    = "A"
            eq_model_rpt$   = "ALL"
            type_desc_rpt$  = "(A)ll Equation Types"
            model_desc_rpt$ = "(A)ll Models"
L19055:     gosub'102(1%,0%)
            errormsg$ = " "
            if keyhit% =  1 then gosub startover
            if keyhit% = 14 then gosub print_report
            if keyhit% = 16 then goto exit_program
            if eq_type_rpt$ <> "A" then goto L19100
               type_desc_rpt$  = "(A)ll Equation Types"
               goto L19125

L19100:        eq_type$ = eq_type_rpt$
               gosub L50180
               if errormsg$ <> " " then goto generate_report
                  eq_type_rpt$   = eq_type$
                  type_desc_rpt$ = type_desc$
L19125:     if eq_model_rpt$ <> "ALL" then goto L19145
               model_desc_rpt$ = "(A)ll Models"
               goto L19055

L19145:        eq_model$ = eq_model_rpt$
               gosub L50440
               if errormsg$ <> " " then goto generate_report
                  eq_model_rpt$   = eq_model$
                  model_desc_rpt$ = model_desc$
                  goto L19055

        return clear all
        goto inputmode

        print_report
            gosub select_printer
            eq_key$ = all(hex(00))
            if eq_type_rpt$ = "A" then goto L19235
               str(eq_key$,1%,1%) = eq_type_rpt$
               if eq_model_rpt$ = "ALL" then goto L19235
                  str(eq_key$,2%,3%) = eq_model$

L19235:     sav_model$ = " "
            read #1,key > eq_key$, using L35040, eq_type$, eq_model$,     ~
                          eq_cd1$, eq_cd2$, eq_clmr$, eq_wallwidt$,      ~
                          eq_phantom$, eq_filler$, eod goto L19530
            goto L19285
        next_reference
            read #1, using L35040, eq_type$, eq_model$,                   ~
                          eq_cd1$, eq_cd2$, eq_clmr$, eq_wallwidt$,      ~
                          eq_phantom$, eq_filler$, eod goto L19530

L19285:     if eq_type_rpt$ = "A" then goto L19295
               if eq_type_rpt$ <> eq_type$ then goto L19530
L19295:     if eq_model_rpt$ = "ALL" then goto L19310
               if eq_model_rpt$ <> eq_model$ then goto next_reference

L19310:     str(eq_key$,1%,1%) = eq_type$
            str(eq_key$,2%,3%) = eq_model$
            str(eq_key$,5%,2%) = eq_cd1$
            str(eq_key$,7%,2%) = eq_cd2$
            eq_type% = 1%
            if sav_model$ = str(eq_model$,1%,1%) then goto L19350
               sav_model$ = str(eq_model$,1%,1%)
               lcnt% = 99%                           /* FORCE NEW PAGE */
L19350:     convert eq_type$ to eq_type%, data goto L19355
L19355:
              if eq_type% = 1% then                                      ~
                           type_desc$ = "Glass - Top/Bottom            "
              if eq_type% = 2% then                                      ~
                           type_desc$ = "Screen - Standard/Special/Full"
              if eq_type% = 3% then                                      ~
                           type_desc$ = "Glass - Cottage/Oriel/(1??)Top"
              if eq_type% = 4% then                                      ~
                           type_desc$ = "Screen - Cottage/Oriel        "
              if eq_type% = 5% then                                      ~
                           type_desc$ = "Glass - (1??) Series Bottom   "
              if eq_type% = 6% then                                      ~
                           type_desc$ = "Glass - Cottage/Oriel/(1??)Bot"
              if eq_type% = 7% then                                      ~
                           type_desc$ = "Glass - Bay/Bow               "
              if eq_type% = 8% then                                      ~
                           type_desc$ = "Screen - Bay/Bow              "
            readkey$ = "MODEL    " & eq_model$
            call "DESCRIBE" (#2, readkey$, model_desc$, 0%, f1%(2%))

            if eq_type% <> 2% and eq_type% <> 4% and eq_type% <> 8% then ~
                              readkey$ = "GLASS    " & eq_cd1$      else ~
                              readkey$ = "SCREEN   " & str(eq_cd1$,2%,1%)
            call "DESCRIBE" (#2, readkey$, cd1_desc$, 0%, f1%(2%))
                                    /* HINGE NOT APPLICABLE FOR SCREEN */
            if eq_type% <> 2% and eq_type% <> 4% and eq_type% <> 8% then ~
                                                               goto L19500
               cd2_desc$ = " N/A "
               goto L19515
L19500:     readkey$ = "HINGE    " & eq_cd2$
            call "DESCRIBE" (#2, readkey$, cd2_desc$, 0%, f1%(2%))

L19515:     gosub print_detail
            goto next_reference

L19530:     gosub close_printer
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20170,         /* Record Type           */ ~
                              L20210,         /* Model Code            */ ~
                              L20250,         /* Primary Code          */ ~
                              L20290,         /* Hinge Code            */ ~
                              L20330,         /* CLMR ( Y or N )       */ ~
                              L20370,         /* WALLWIDT ( Y or N )   */ ~
                              L20410          /* Phantom Designator    */
         return

L20170: REM Record Type                            EQ_TYPE$
        REM EQ_TYPE$ = " "
         return

L20210: REM Model Code                             EQ_MODEL$
        REM EQ_MODEL$ = " "
         return

L20250: REM Primary Code For Type                  EQ_CD1$
        REM EQ_CD1$ = "01"
         return

L20290: REM Hinge Code                             EQ_CD2$
        REM EQ_CD2$ = "00"
         return

L20330: REM CLMR Applicable                        EQ_CLMR$
        REM EQ_CLMR$ = "N"
         return

L20370: REM Wallwidth Applicable                   EQ_WALLWIDT$
        REM EQ_WALLWIDT$ = "N"
         return

L20410: REM Phantom Designator                     EQ_PHANTOM$
        REM EQ_PHANTOM$ = " "
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
         "Enter (1-GL,2-SC,3-GL C/O/T,4-SC C/O,5-GL Bot,6-Gl C/O/B,7/8B",~
         "Enter a Valid Model Code.                                    ",~
         "Enter a Valid Code Associated with Type.                     ",~
         "Enter a Valid Hinge Code ( When Applicable ).                ",~
         "CLMR Applicable ( (Y)es or (N)o ).                           ",~
         "WALLWIDTH Applicable ( (Y)es or (N)o ).                      ",~
         "Enter a Valid Phantom Designator ( Required ).               "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28300
                inpmessage$ = edtmessage$
                return

L28300
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter Equation Type or (A) for All, and Model or (A) for All."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, eq_type$, eq_model$,       ~
                      eq_cd1$, eq_cd2$, eq_clmr$, eq_wallwidt$,          ~
                      eq_phantom$, eq_filler$, readkey$, eq_key$,        ~
                      type_desc$, model_desc$, cd1_desc$, cd2_desc$,     ~
                      desc$()

              desc$(1%) = "1 = Glass  - Top/Bottom                 "
              desc$(2%) = "2 = Screen - Standard/Special/Full      "
              desc$(3%) = "3 = Glass  - Cottage/Oriel/(1??)Top     "
              desc$(4%) = "4 = Screen - Cottage/Oriel              "
              desc$(5%) = "5 = Glass  - (1??) Series Bottom        "
              desc$(6%) = "6 = Glass  - Cottage/Oriel/(1??)Bot     "
              desc$(7%) = "7 = Glass  - Bay/Bow                    "
              desc$(8%) = "8 = Screen - Bay/Bow                    "

        return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *************************************************************~

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
          str(eq_key$,1%,1%)  = eq_type$
          str(eq_key$,2%,3%)  = eq_model$
          str(eq_key$,5%,2%)  = eq_cd1$
          str(eq_key$,7%,2%)  = eq_cd2$

          read #1,key = eq_key$,using L35040, eq_type$, eq_model$,        ~
                        eq_cd1$, eq_cd2$, eq_clmr$, eq_wallwidt$,        ~
                        eq_phantom$, eq_filler$, eod goto L30170

          rec% = 1%                                         /* ON FILE */
L30170: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        dataput
          str(eq_key$,1%,1%)  = eq_type$
          str(eq_key$,2%,3%)  = eq_model$
          str(eq_key$,5%,2%)  = eq_cd1$
          str(eq_key$,7%,2%)  = eq_cd2$

          read #1,hold,key = eq_key$, eod goto L31115
            delete #1
L31115:   write #1, using L35040, eq_type$, eq_model$, eq_cd1$, eq_cd2$,  ~
                                 eq_clmr$, eq_wallwidt$, eq_phantom$,    ~
                                 eq_filler$, eod goto L31150

L31150: return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCEQUAT - Cross Reference */
L35040:     FMT CH(01),                  /* TXN Type ( 1 thru 8 )      */~
                CH(03),                  /* Model Code                 */~
                CH(02),                  /* Primary Code               */~
                CH(02),                  /* Hinge Code                 */~
                CH(01),                  /* CLMR Applicable            */~
                CH(01),                  /* WALLWIDT Applicable        */~
                CH(04),                  /* Phantom Designator         */~
                CH(02)                   /* Filler Area                */

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
              on fieldnr% gosub L40230,         /* TXN Type Code     */   ~
                                L40230,         /* Model Code        */   ~
                                L40230,         /* Primary Code      */   ~
                                L40230,         /* Hinge Code        */   ~
                                L40220,         /* CLMR ( Y or N )   */   ~
                                L40220,         /* WALLWIDT ( Y or N)*/   ~
                                L40230          /* Phantom Designator*/

              goto L40250

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40230:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "Glass/Screen Equation and Part Cross-Reference Data", ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "TXN Type    :",                              ~
               at (06,20), fac(lfac$( 1)), eq_type$             , ch(01),~
               at (06,30), fac(hex(84)), type_desc$             , ch(30),~
               at (07,02), "Model Code  :",                              ~
               at (07,20), fac(lfac$( 2)), eq_model$            , ch(03),~
               at (07,30), fac(hex(84)), model_desc$            , ch(30),~
               at (08,02), "Primary Code:",                              ~
               at (08,20), fac(lfac$( 3)), eq_cd1$              , ch(02),~
               at (08,30), fac(hex(84)), cd1_desc$              , ch(30),~
               at (09,02), "Hinge Code  :",                              ~
               at (09,20), fac(lfac$( 4)), eq_cd2$              , ch(02),~
               at (09,30), fac(hex(84)), cd2_desc$              , ch(30),~
               at (10,02), "CLMR   (Y/N):",                              ~
               at (10,20), fac(lfac$( 5)), eq_clmr$             , ch(01),~
               at (11,02), "WALLWIDT Y/N:",                              ~
               at (11,20), fac(lfac$( 6)), eq_wallwidt$         , ch(01),~
               at (12,02), "Phantom Des.:",                              ~
               at (12,20), fac(lfac$( 7)), eq_phantom$          , ch(04),~
                                                                         ~
               at (13,20), fac(hex(84)), desc$(1%)              , ch(40),~
               at (14,20), fac(hex(84)), desc$(2%)              , ch(40),~
               at (15,20), fac(hex(84)), desc$(3%)              , ch(40),~
               at (16,20), fac(hex(84)), desc$(4%)              , ch(40),~
               at (17,20), fac(hex(84)), desc$(5%)              , ch(40),~
               at (18,20), fac(hex(84)), desc$(6%)              , ch(40),~
               at (19,20), fac(hex(84)), desc$(7%)              , ch(40),~
               at (20,20), fac(hex(84)), desc$(8%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40710
                  call "PRNTSCRN"
                  goto L40250

L40710:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40910     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40870
                str(pf$(1),64%)   = " "  :  str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3),64%)   = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40870:     if fieldnr% > 1% then L40890
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40890:     return

L40910: if fieldnr% > 0% then L41020  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (12)Delete Reference   " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(2),18%,30%) = " " : str(pfkeys$,12%,1%)=hex(ff)
            return
L41020:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
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
              on fieldnr% gosub L42170            /* Equation Type Code*/
                                                 /* and Model Code    */
              goto L42200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42200:     accept                                                       ~
               at (01,02),                                               ~
                  "Glass/Screen Equation Cross-Reference Report(s)",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (06,02), "TXN Type    :",                              ~
               at (06,20), fac(lfac$( 1)), eq_type_rpt$         , ch(01),~
               at (06,30), fac(hex(84)), type_desc_rpt$         , ch(30),~
               at (07,02), "Model Code  :",                              ~
               at (07,20), fac(lfac$( 1)), eq_model_rpt$        , ch(03),~
               at (07,30), fac(hex(84)), model_desc_rpt$        , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42480
                  call "PRNTSCRN"
                  goto L42200

L42480:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50180,         /* TXN Type Code         */ ~
                              L50440,         /* Model Code            */ ~
                              L50540,         /* Primary Code          */ ~
                              L50760,         /* Hinge Code            */ ~
                              L50930,         /* CLMR ( Y or N )       */ ~
                              L51010,         /* WALLWIDT ( Y or N )   */ ~
                              L51090          /* Phantom Designator    */

            return

L50180: REM TXN Type Code                         EQ_TYPE$
           if eq_type$ <> " " then goto L50210
              eq_type$ = "1"
L50210:    convert eq_type$ to eq_type%, data goto L50220
L50220:
           if eq_type% < 1% or eq_type% > 8% then goto L50410
              if eq_type% = 1% then                                      ~
                           type_desc$ = "Glass - Top/Bottom            "
              if eq_type% = 2% then                                      ~
                           type_desc$ = "Screen - Standard/Special/Full"
              if eq_type% = 3% then                                      ~
                           type_desc$ = "Glass - Cottage/Oriel/(1??)Top"
              if eq_type% = 4% then                                      ~
                           type_desc$ = "Screen - Cottage/Oriel        "
              if eq_type% = 5% then                                      ~
                           type_desc$ = "Glass - (1??) Series Bottom   "
              if eq_type% = 6% then                                      ~
                           type_desc$ = "Glass - Cottage/Oriel/(1??)Bot"
              if eq_type% = 7% then                                      ~
                           type_desc$ = "Glass - Bay/Bow               "
              if eq_type% = 8% then                                      ~
                           type_desc$ = "Screen - Bay/Bow              "
        return
L50410:    errormsg$ = " Invalid TXN Type ( 1 Thru 8 ) "
        return

L50440: REM Model Code                            EQ_MODEL$
           if eq_model$ <> " " then goto L50470
              goto L50510
L50470:    readkey$ = "MODEL    " & eq_model$
           read #2,key = readkey$,using L50490, model_desc$,eod goto L50510
L50490:      FMT POS(25), CH(32)
        return
L50510:    errormsg$ = "Invalid Model Code"
        return

L50540: REM Primary Code                          EQ_CD1$
           if eq_cd1$ <> " " then goto L50570
              goto L50710
L50570:    if eq_type% = 2% or eq_type% = 4% or eq_type% = 8%  then      ~
                                 str(eq_cd1$,1%,1%) = "0"   /* SCREEN */

           convert eq_cd1$ to eq_cd1%, data goto L50610
L50610:
           convert eq_cd1% to eq_cd1$, pic(00)


           if eq_type% <> 2% and eq_type% <> 4% and eq_type% <> 8% then  ~
                            readkey$ = "GLASS    " & eq_cd1$      else   ~
                            readkey$ = "SCREEN   " & str(eq_cd1$,2%,1%)
           call "DESCRIBE" (#2, readkey$, cd1_desc$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L50710
        return
L50710:    if eq_type% <> 2% and eq_type% <> 4% and eq_type% <> 8% then  ~
                             errormsg$ = "Invalid Code For Glass " else  ~
                             errormsg$ = "Invalid Code For Screen "
        return

L50760: REM Hinge Code                            EQ_CD2$
           if eq_cd2$ <> " " then goto L50790
              eq_cd2$ = "00"
L50790:    convert eq_cd2$ to eq_cd2%, data goto L50800
L50800:
           convert eq_cd2% to eq_cd2$, pic(00)

           readkey$ = "HINGE    " & eq_cd2$
           call "DESCRIBE" (#2, readkey$, cd2_desc$, 0%, f1%(2%) )
           if f1%(2%) = 0% then goto L50900
           gosub dataload
           if rec% = 0% then goto L50890
              fieldnr% = 8%
L50890: return
L50900:    errormsg$ = "Invalid HINGE Code"
        return

L50930: REM CLMR ( Y or N )                       EQ_CLMR$
           if eq_clmr$ <> " " then goto L50960
              eq_clmr$ = "N"
L50960:    if eq_clmr$ <> "Y" and eq_clmr$ <> "N" then goto L50980
        return
L50980:    errormsg$ = "Invalid Entry - ( Y or N )"
        return

L51010: REM WALLWIDT ( Y or N )                   EQ_WALLWIDT$
           if eq_wallwidt$ <> " " then goto L51040
              eq_wallwidt$ = "N"
L51040:    if eq_wallwidt$ <> "Y" and eq_wallwidt$ <> "N" then goto L51060
        return
L51060:    errormsg$ = "Invalid Entry - ( Y or N )"
        return

L51090: REM Phantom Designator                     EQ_PHANTOM$
           if eq_phantom$ <> " " then goto L51120
              errormsg$ = "Invalid Entry - Phantom Designator Required"
L51120: return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %######## ########                   ############################~
        ~################################                        APCEQUAT:

L55080: %User Id: ###                        ############################~
        ~################################                      PAGE: #####
                                                   /* COLUMN HEADER   */
L55110: % Type   Model  Code  <------- Description -------->  Hinge  <---~
        ~-- Description ----------> C B Phan <----- Type Description -----~
        ~>
L55140: %------  -----  ----  ------------------------------  -----  ----~
        ~-------------------------- - - ---- -----------------------------~
        ~-
                                                   /* DETAIL          */
L55180: %######   ###    ##   ##############################    ##   ####~
        ~########################## # # #### #############################~
        ~#

        print_header

          page_no% = page_no% + 1%
          print page
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print
          print using L55110
          print using L55140
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 60% then gosub print_header

           print using L55180, eq_type$, eq_model$, eq_cd1$,              ~
                              str(cd1_desc$,1%,20%), eq_cd2$,            ~
                              str(cd2_desc$,1%,20%), eq_clmr$,           ~
                              eq_wallwidt$, eq_phantom$,                 ~
                              type_desc$
          lcnt% = lcnt% + 1%
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
            print_title$ = "Equation and Part Cross-Reference"
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCEQ", " ", 0%, 1%)
        return

        delete_reference
          eq_key$ = all(hex(00))
          str(eq_key$,1%,1%)  = eq_type$
          str(eq_key$,2%,3%)  = eq_model$
          str(eq_key$,5%,2%)  = eq_cd1$
          str(eq_key$,7%,2%)  = eq_cd2$
          read #1,hold,key = eq_key$, eod goto L60270
             delete #1
L60270:   rec% = 0%
          return clear all
          goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
