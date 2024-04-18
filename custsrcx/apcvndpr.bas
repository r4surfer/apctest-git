        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   AAA   PPPP    CCC   V   V  N   N  DDDD   PPPP   RRRR    *~
            *  A   A  P   P  C   C  V   V  NN  N  D   D  P   P  R   R   *~
            *  AAAAA  PPPP   C      V   V  N N N  D   D  PPPP   RRRR    *~
            *  A   A  P      C   C   V V   N  NN  D   D  P      R   R   *~
            *  A   A  P       CCC     V    N   N  DDDD   P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCVNDPR - Adjust Vendor Prices for a Specified Vendor    *~
            *            and Specified Part Classes.                    *~
            *                                                           *~
            * Notes - 1. Specified Vendor                               *~
            *         2. Part Class (Aluminum Extrusion,Vinly,Glass     *~
            *         3. Adjustment Type ( (D)ollars or (P)ercent       *~
            *         4. Adjustment Amount or Percent                   *~
            *         5. Effective Date of Price Change                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/22/90 ! Completed Program                        ! RHH *~
            *          !                                          !     *~
            * 11/13/97 ! Changed Program Version ID To 60403      ! DJD *~
            *          !                                          !     *~
            * 12/20/99 ! Mod to change vendor's price to allow    ! CMG *~
            *          !     five decimal places.  (EWD0001)      !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            vendor_key$59,               /* Vendor Price Key           */~
            vendor_cnt$6,                /* Number of Parts_updated    */~
            vendor$9,                    /* Vendor Code                */~
            class$1,                     /* Part Class (A),(V),(G)     */~
            adj_type$1,                  /* Adjustment Type Code D or P*/~
            adjustment$12,               /* Adjustment Amount          */~
            eff_dte$8,                   /* Effective Date Formatted   */~
            eff_dte1$8,                  /* Effective Date Unformatted */~
            part_class$1,                /* Selection Within Part Class*/~
            ven_desc$30,                 /* Vendor Name                */~
            class_desc$30,               /* Part Class Description     */~
            type_desc$30,                /* Adjustment Type Description*/~
            rpt_time$8,                  /* REPORT TIME                */~
            company$60,                  /* COMPANY NAME               */~
            print_title$60,              /* TITLE                      */~
            part_no$25,                  /* PART NUMBER                */~
            part_desc$32,                /* PART DESCRIPTION           */~
            old_amt$12,                  /* OLD PRICE                  */~
            new_amt$12,                  /* NEW PRINT                  */~
            price_rec$256,               /* VENDOR PRICE RECORD        */~
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

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/13/97 Pre-Release Version            "
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
            * #01 ! VENDOR   ! Vendor Master File                       *~
            * #02 ! VENPRICE ! Vendor Price File                        *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "VENDOR",                                        ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key  1,    keypos = 10, keylen = 30

            select #02, "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 10,   keylen =  59,                     ~
                        alt key   1,   keypos =  1, keylen = 34, dup,    ~
                            key   2,   keypos = 35, keylen = 34

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key   1,   keypos = 102, keylen =  9, dup,   ~
                            key   2,   keypos =  90, keylen =  4, dup,   ~
                            key   3,   keypos =  26, keylen = 32, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),  0%, rslt$(03))

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

            for fieldnr% = 1% to  5%
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

                  if keyhit%  = 16% then goto process_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
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
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        process_data
          call "SHOSTAT" ("Updating Vendor Prices")

          call "SETPRNT" ("APC001", " ", 0%, 0%)
          select printer (134)
          rpt_time$ = " "
          call "TIME" (rpt_time$)
          lcnt% = 99% : page_no% = 0%
          vendor_cnt% = 0%
          vendor_key$ = all(hex(00))

        process_next
          read #2,key > vendor_key$, using L19220, vendor_key$,           ~
                                                 eod goto process_done
L19220:     FMT POS(10), CH(59)
          if str(vendor_key$,26%,9%) <> vendor$ then goto process_next
             part_no$ = str(vendor_key$,1%,25%)
             if class$ = "A" then gosub update_aluminum
             if class$ = "V" then gosub update_vinyl
             if class$ = "G" then gosub update_glass
          goto process_next

        process_done
          gosub update_complete
          close printer
          call "SETPRNT" (" ", " ", 0%, 1%)
          goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20110,         /* Vendor Code           */ ~
                              L20150,         /* Part Class (A),(V),(G)*/ ~
                              L20190,         /* Adjustment Type D,P   */ ~
                              L20230,         /* Adjustment Amount     */ ~
                              L20270          /* Effective Date        */

         return

L20110: REM Vendor Code                            VENDOR$
        REM VENDOR$ = " "
         return

L20150: REM Part Class                             CLASS$
        REM CLASS$ = " "
         return

L20190: REM Adjustment Type                        ADJ_TYPE$
        REM ADJ_TYPE$ = "D"
         return

L20230: REM Adjustment Amount                      ADJUSTMENT$
        REM ADJUSTMENT$ = "0.0"
         return

L20270: REM Effective Date                         EFF_DTE$
        REM EFF_DTE$ = DATE$
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
         "Enter Code for Vendor you wish to Adjust Prices For.         ",~
         "Enter Part Class Code - Entries (A)luminum, (V)inyl, (G)lass.",~
         "Enter Adjustment Type Code - (D)ollars/cents, (P)ercent.     ",~
         "Enter Adjustment Amount (Plus or Minus)                      ",~
         "Enter Effective Date For Price Change.                       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, vendor$, class$, adj_type$,~
                      adjustment$, eff_dte$, eff_dte1$, ven_desc$,       ~
                      part_class$, class_desc$, type_desc$


            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
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
          read #2,hold,key = vendor_key$, using L30080, price_rec$,       ~
                                                      eod goto L30170
L30080:     FMT CH(256)
          get price_rec$, using L30100, old_amt
L30100:     FMT POS(69), PD(15,5)                 /* (EWD0001) */
          if adj_type$ <> "P" then goto L30150
             new_amt = round(adjustment * old_amt, 5)   /* (EWD0001) */
             new_amt = round(new_amt + old_amt, 5)      /* (EWD0001) */
             goto L30160
L30150:   new_amt = round(adjustment + old_amt, 5)      /* (EWD0001) */
L30160:   gosub dataput
L30170:  return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
          put price_rec$, using L30100, new_amt

          str(price_rec$,109%,6%) = str(eff_dte1$,1%,6%)
          str(price_rec$,121%,6%) = str(eff_dte1$,1%,6%)
          str(price_rec$,127%,3%) = userid$

          rewrite #2, using L30080, price_rec$
          part_no$, part_desc$, old_amt$, new_amt$ = " "
          part_no$ = str(price_rec$,10%,25%)
          convert old_amt to old_amt$, pic(#######.####)

          convert new_amt to new_amt$, pic(#######.####)

          read #3,key = part_no$, using L31200, part_desc$,eod goto L31220
L31200:     FMT POS(26), CH(32)

L31220:   gosub print_detail
          vendor_cnt% = vendor_cnt% + 1%
         return

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
              on fieldnr% gosub L40200,         /* Vendor Code       */   ~
                                L40200,         /* Part Class        */   ~
                                L40200,         /* Adjustment Type   */   ~
                                L40210,         /* Adjustment Amount */   ~
                                L40190          /* Effective Date    */

              goto L40230

L40190:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "Vendor Price - Adjustment Utility",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code     ",                           ~
               at (06,20), fac(lfac$( 1)), vendor$              , ch(09),~
               at (06,35), fac(hex(84)), ven_desc$              , ch(30),~
                                                                         ~
               at (07,02), "Part Class      ",                           ~
               at (07,20), fac(lfac$( 2)), class$               , ch(01),~
               at (07,35), fac(hex(84)), class_desc$            , ch(30),~
                                                                         ~
               at (08,02), "Adjustment Type ",                           ~
               at (08,20), fac(lfac$( 3)), adj_type$            , ch(01),~
               at (08,35), fac(hex(84)), type_desc$             , ch(30),~
                                                                         ~
               at (09,02), "Adjustment Amt  ",                           ~
               at (09,20), fac(lfac$( 4)), adjustment$          , ch(12),~
                                                                         ~
               at (10,02), "Effective Date  ",                           ~
               at (10,20), fac(lfac$( 5)), eff_dte$             , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40710     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffffff1000)
            if fieldnr% = 1% then L40670
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40670:     if fieldnr% > 1% then L40690
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40690:     return

L40710: if fieldnr% > 0% then L40800  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                       (16)Process Data"
            pfkeys$ = hex(01ffffffffffffffffffffffffffff1000)
            return
L40800:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50160,         /* VENDOR CODE           */ ~
                              L50390,         /* Part Class            */ ~
                              L50520,         /* Adjustment Type       */ ~
                              L50640,         /* Adjustment Amount     */ ~
                              L50770          /* Effective Date        */

            return

L50160: REM Vendor Code                           VENDOR$
            if vendor$ <> " " then goto L50230
               vendor$  = " "
               ven_desc$ = hex(06) & "Select Vendor to Adjust"
               call "GETCODE" (#1, vendor$, ven_desc$, 0%, 0.30, f1%(1))
               ven_desc$ = " "
               if f1%(1) = 0% then goto L50350
L50230:     vendor_key$ = all(hex(00))
            str(vendor_key$,1%,9%) = vendor$
            read #2,key 1% > vendor_key$, using L50270,vendor_key$,       ~
                                          eod goto L50290
L50270:       FMT CH(34)
            if str(vendor_key$,1%,9%) = vendor$ then goto L50320
L50290:        errormsg$ = " NO PRICES ON FILE FOR SELECTED VENDOR "
               return

L50320:     read #1,key = vendor$, using L50330,ven_desc$,eod goto L50350
L50330:       FMT POS(40), CH(30)
         return
L50350:     errormsg$ = "Invalid Vendor Selection ?"
            vendor$ = " "
         return

L50390: REM Part Class                            CLASS$
            if class$ <> " " then goto L50420
               goto L50480
L50420:     if class$ <> "A" and class$ <> "V" and class$ <> "G" then    ~
                                               goto L50480
            if class$ = "A" then gosub sel_aluminum
            if class$ = "V" then gosub sel_vinyl
            if class$ = "G" then gosub sel_glass
         return
L50480:     errormsg$ = "Invalid Part Class (A)luminum,(V)inyl,(G)lass ?"
            class$ = "A"
         return

L50520: REM Adjustment Type                       ADJ_TYPE$
            if adj_type$ <> " " then goto L50550
               goto L50600
L50550:     if adj_type$ <> "D" and adj_type$ <> "P" then goto L50600
            type_desc$ = "Dollars & Cents"
            if adj_type$ = "P" then type_desc$ = "Percent"

         return
L50600:     errormsg$ = "Enter Adjustment Type (D)ollars/cents, (P)ercent"
            adj_type$ = "D"
         return

L50640: REM Adjustment Amount                     ADJUSTMENT$
            if adjustment$ <> " " then goto L50670
               goto L50720
L50670:     convert adjustment$ to adjustment, data goto L50720
                                                /* (EWD0001) */
            convert adjustment to adjustment$, pic(#####.#####-)
            if adjustment = 0 then goto L50720
         return
L50720:     errormsg$ = "Invalid Adjustment Amount?"
            adjustment = 0.0                    /* (EWD0001) */ 
            convert adjustment to adjustment$, pic(#####.#####-)
         return

L50770: REM Effective Date for Price              EFF_DTE$
            if eff_dte$ <> " " then goto L50800
               eff_dte$ = date$
L50800:     date% = 0%
            call "DATEOK" (eff_dte$, date%, errormsg$)
            if errormsg$ <> " " then goto L50870
            eff_dte1$ = eff_dte$
            call "DATUNFMT" (eff_dte1$)
         return
            errormsg$ = "Invalid Effective Price Date ?"
L50870:     eff_dte$ = " "
         return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %######## ########                   ############################~
        ~################################                        APCVNDPR:

L55080: %USER ID: ###                        ############################~
        ~################################                      PAGE: #####

L55110: %VENDOR: #########   NAME: ##############################        ~
        ~EFFECTIVE DATE: ########

                                                   /* COLUMN HEADER */
L55130: % < ---- PART NUMBER ----->    <-------- DESCRIPTION -------->   ~
        ~  OLD AMOUNT     NEW AMOUNT
L55141: % -------------------------    -------------------------------   ~
        ~------------   ------------

L55160: % #########################    ################################  ~
        ~############   ############

        print_header
          page_no% = page_no% + 1%
          print page
          print using L55050, date$, rpt_time$, company$
          print_title$ = "VENDOR PRICE ADJUSTMENT UPDATE"
          call "FMTTITLE" (print_title$, " ", 12%)
          print using L55080, userid$, print_title$, page_no%
          print
          print using L55110, vendor$, ven_desc$, eff_dte$
          print
          print using L55130
          print using L55141
          lcnt% = 8%
        return

        print_detail
          if lcnt% > 60% then gosub print_header

          print using L55160, part_no$, part_desc$, old_amt$, new_amt$

          lcnt% = lcnt% + 1%
        return

        sel_aluminum

        REM *************************************************************~
            *     A L U M I N U M   E X T R U S I O N   S C R E E N     *~
            *************************************************************
            part_class$ = " "
            accept                                                       ~
               at (01,22),                                               ~
                  "Aluminum Extrusion Selection Screen",                 ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,23), "******************************************", ~
               at (07,23), "*  (1) SOLID  (Not Painted)              *", ~
               at (08,23), "*  (2) SOLID  (Painted Only)             *", ~
               at (09,23), "*  (3) SOLID  (All- Solid)               *", ~
               at (10,23), "*  (4) HOLLOW (Not Painted)              *", ~
               at (11,23), "*  (5) HOLLOW (Painted Only)             *", ~
               at (12,23), "*  (6) HOLLOW (All - Hollow)             *", ~
               at (13,23), "*  (7) ALL                               *", ~
               at (14,23), "*                                        *", ~
               at (15,23), "*  Enter Selection   ( )                 *", ~
               at (16,23), "*                                        *", ~
               at (17,23), "******************************************", ~
                                                                         ~
               at (15,45), fac(hex(81)), part_class$            , ch(01)

               a% = pos( "1234567" = part_class$)
               if a% <> 0% then goto L60290
                  errormsg$ = "Invalid Selection ( 1 thru 7) ?"
                  goto sel_aluminum
L60290:        errormsg$ = " "
         if a% = 1% then class_desc$ = "Aluminum Solid (Not Painted)"
         if a% = 2% then class_desc$ = "Aluminum Solid (Painted Only)"
         if a% = 3% then class_desc$ = "Aluminum Solid (All - Solid)"
         if a% = 4% then class_desc$ = "Aluminum Hollow (Not Painted)"
         if a% = 5% then class_desc$ = "Aluminum Hollow (Painted Only)"
         if a% = 6% then class_desc$ = "Aluminum Hollow (All - Hollow)"
         if a% = 7% then class_desc$ = "All Aluminum Extrusions"

        return

        sel_vinyl

        REM *************************************************************~
            *         V I N Y L   E X T R U S I O N   S C R E E N       *~
            *************************************************************
            part_class$ = " "
            accept                                                       ~
               at (01,25),                                               ~
                  "Vinyl Extrusion Selection Screen",                    ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,23), "******************************************", ~
               at (07,23), "*  MODELS (6)            MODELS (8)      *", ~
               at (08,23), "*                                        *", ~
               at (09,23), "*  (0) WHITE             (5) WHITE       *", ~
               at (10,23), "*  (1) BRONZE            (6) BRONZE      *", ~
               at (11,23), "*  (2) COCO              (7) COCO        *", ~
               at (12,23), "*  (3) BEIGE             (8) BEIGE       *", ~
               at (13,23), "*  (4) ALL               (9) ALL         *", ~
               at (14,23), "*                                        *", ~
               at (15,23), "*  Enter Selection   ( )                 *", ~
               at (16,23), "*                                        *", ~
               at (17,23), "******************************************", ~
                                                                         ~
               at (15,45), fac(hex(81)), part_class$            , ch(01)

               a% = pos( "0123456789" = part_class$)
               if a% <> 0% then goto L60690
                  errormsg$ = "Invalid Selection ( 0 thru 9) ?"
                  goto sel_vinyl
L60690:        errormsg$ = " "
               if a% = 0% then class_desc$ = "Vinyl  - White (6) "
               if a% = 1% then class_desc$ = "Vinyl  - Bronze (6) "
               if a% = 2% then class_desc$ = "Vinyl  - Coco (6) "
               if a% = 3% then class_desc$ = "Vinyl  - Beige (6) "
               if a% = 4% then class_desc$ = "All - Vinyl (6) "
               if a% = 5% then class_desc$ = "Vinyl  - White (8) "
               if a% = 6% then class_desc$ = "Vinyl  - Bronze (8) "
               if a% = 7% then class_desc$ = "Vinyl  - Coco (8) "
               if a% = 8% then class_desc$ = "Vinyl  - Beige (8) "
               if a% = 9% then class_desc$ = "All - Vinyl (8) "
         return

        sel_glass

        REM *************************************************************~
            *                  G L A S S   S C R E E N                  *~
            *************************************************************
            part_class$ = " "
            accept                                                       ~
               at (01,30),                                               ~
                  "Glass Selection Screen",                              ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,23), "******************************************", ~
               at (07,23), "*  (1) 201 - Category                    *", ~
               at (08,23), "*  (2) 210 - Category                    *", ~
               at (09,23), "*  (3) All                               *", ~
               at (10,23), "*                                        *", ~
               at (11,23), "*                                        *", ~
               at (12,23), "*                                        *", ~
               at (13,23), "*                                        *", ~
               at (14,23), "*                                        *", ~
               at (15,23), "*  Enter Selection   ( )                 *", ~
               at (16,23), "*                                        *", ~
               at (17,23), "******************************************", ~
                                                                         ~
               at (15,45), fac(hex(81)), part_class$            , ch(01)

               a% = pos( "123" = part_class$)
               if a% <> 0% then goto L61110
                  errormsg$ = "Invalid Selection ( 1 thru 3) ?"
                  goto sel_glass
L61110:        errormsg$ = " "
               if a% = 1% then class_desc$ = "Glass (201) Category"
               if a% = 2% then class_desc$ = "Glass (210) Category"
               if a% = 3% then class_desc$ = "All - Glass "
         return

        update_aluminum
          if str(part_no$,2%,1%) <> "4" and str(part_no$,2%,1%) <> "5"   ~
                               then return    /* NOT SOLID OR HOLLOW */

          if a% > 3% then goto L61330
             if str(part_no$,2%,1%) <> "4" then return  /* NOT SOLID */
             if a% <> 1% then goto L61280
               if str(part_no$,5%,1%) = "0" or str(part_no$,5%,1%) = "1" ~
                                   then goto L61450  /* NOT PAINTED */
               return

L61280:      if a% <> 2% then goto L61450            /* ALL SOLID    */
               if str(part_no$,5%,1%) <> "0" and    /* PAINTED ONLY */   ~
                  str(part_no$,5%,1%) <> "1" then goto L61450
             return

L61330:   if a% > 6% then goto L61450                /* ALL ALUMINUM */
             if str(part_no$,2%,1%) <> "5" then return  /* NOT HOLLOW */
             if a% <> 4% then goto L61400
               if str(part_no$,5%,1%) = "0" or str(part_no$,5%,1%) = "1" ~
                                   then goto L61450  /* NOT PAINTED  */
               return

L61400:      if a% <> 5% then goto L61450            /* ALL HOLLOW   */
               if str(part_no$,5%,1%) <> "0" and    /* PAINTED ONLY */   ~
                  str(part_no$,5%,1%) <> "1" then goto L61450
             return

L61450:   gosub dataload
        return

        update_vinyl
          if str(part_no$,2%,3%) <> "105" then return  /* NOT VINYL */
             if str(part_no$,1%,1%) <> "6" then goto L61580

             if a% = 4% then goto L61640            /* (6) ALL VINYL */
             if a% = 0% and str(part_no$,5%,1%) = "2" then goto L61640
             if a% = 1% and str(part_no$,5%,1%) = "3" then goto L61640
             if a% = 2% and str(part_no$,5%,1%) = "5" then goto L61640
             if a% = 3% and str(part_no$,5%,1%) = "6" then goto L61640
             return
L61580:      if a% = 9% then goto L61640            /* (8) ALL VINYL */
             if a% = 5% and str(part_no$,5%,1%) = "2" then goto L61640
             if a% = 6% and str(part_no$,5%,1%) = "3" then goto L61640
             if a% = 7% and str(part_no$,5%,1%) = "5" then goto L61640
             if a% = 8% and str(part_no$,5%,1%) = "6" then goto L61640
             return
L61640:   gosub dataload
        return

        update_glass
          if str(part_no$,2%,3%) <> "201" and                            ~
             str(part_no$,2%,3%) <> "210" then return

          if a% = 3% then goto L61760
          if a% = 1% and str(part_no$,2%,3%) = "201" then goto L61760
          if a% = 2% and str(part_no$,2%,3%) = "210" then goto L61760
          return

L61760:   gosub dataload
        return

        update_complete

        REM *************************************************************~
            *               U P D A T E   C O M P L E T E               *~
            *************************************************************
            convert vendor_cnt% to vendor_cnt$, pic(######)
            accept                                                       ~
               at (01,30),                                               ~
                  "Vendor Price Update Complete",                        ~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (10,23), "******************************************", ~
               at (11,23), "*                                        *", ~
               at (12,23), "*  (      ) Vendor Parts Updated         *", ~
               at (13,23), "*                                        *", ~
               at (14,23), "******************************************", ~
                                                                         ~
               at (12,27), fac(hex(94)), vendor_cnt$            , ch(06)

         return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
