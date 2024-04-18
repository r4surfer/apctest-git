        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    SSS   K   K  U    U                *~
            *  A   A  P   P  C   C  S      K  K   U    U                *~
            *  AAAAA  PPPP   C       SSS   KKKK   U    U                *~
            *  A   A  P      C   C      S  K  K   U    U                *~
            *  A   A  P       CCC    SSS   K   K   UUUU                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCSKU  - Add and Change Sku Numbers                      *~
            *    - SKU Customer Code                                    *~
            *      - Sku Number                                         *~
            *        - APC Part Number                                  *~
            *          - Sku Stock Price                                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/08/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          ! RHH *~
            *          !                                          !     *~
            * 11/06/97 ! Revision Update For 60403                ! DJD *~
            * 05/14/98 ! (EWD001) Mod for new field for new Lowes ! RHH *~
            *          ! Color Labels. (PLANLABEL)                !     *~
            *************************************************************

        dim                                                              ~
            scr$(5),                     /* SCREEN TEXT                */~
            readkey$50,                  /* GENERIC KEY                */~
            beg_sku$3,                   /* Beg Customer Sku Code      */~
            beg_desc$30,                 /* Beg Description            */~
            end_sku$3,                   /* End Customer Sku Code      */~
            end_desc$30,                 /* End Description            */~
            sav_sku$3,                   /* Save Customer Sku Code     */~
            sort$1,                      /* Sort Seq. (1)Sku,(2)Part   */~
            sort_desc$30,                /* Sort Seq. Description      */~
            sku_code$3,                  /* Customer Sku Code          */~
            sku_desc$32,                 /* Customer Sku Code Descript */~
            sku_price$10,                /* Sku Stock Price            */~
            hdr$40,                      /* ASKUSER                    */~
            msg$(3)79,                   /* ASKUSER                    */~
            partno$25,                   /* Part Number                */~
            desc$32,                     /* Part Number Description    */~
            upccode$20,                  /* Part Number UPC CODE       */~
            skuno$25,                    /* Sku Number                 */~
            sku_key$28,                  /* Sku Key                    */~
            sku_rec$73,                  /* Sku RECORD                 */~
            sku_label$2,                 /* (EWD001) New Color Label   */~
            sku_label_d$30,              /* (EWD001) Tabel Desc        */~
            sku_fil$7,                   /* FILLER                     */~
            cnt$4,                       /* Counter                    */~
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

        dim cus_key$9,                   /* Primary Customer Key       */~
            cus_code$9,                  /* Customer Code              */~
            cus_desc$30,                 /* Customer Address           */~
            cus_city$18,                 /* Customer City              */~
            cus_st$2,                    /* Customer State             */~
            cus_zp$9,                    /* Customer Zip               */~
            cus_zip$10,                  /* Customer Zip  Report       */~
            cus_ph$10,                   /* Phone Contact              */~
            cus_phone$14                 /* Phone Contact Report       */

        dim f2%( 5),                     /* = 0 if the file is open    */~
            f1%( 5),                     /* = 1 if READ was successful */~
            fs%( 5),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$( 5)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/06/97 APC New Program                "
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
            * #01 ! CUSTOMER ! Master Customer File                     *~
            * #02 ! HNYMASTR ! Part Master File                         *~
            * #03 ! APCSKUNO ! Sku Number Master File                   *~
            * #04 ! GENCODES ! Master Table File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #3,  "APCSKUNO",                                      ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1), f2%(1),  0%, rslt$(1))
            call "OPENCHCK" (#2, fs%(2), f2%(2),  0%, rslt$(2))
            call "OPENCHCK" (#3, fs%(3), f2%(3),500%, rslt$(3))
            call "OPENCHCK" (#4, fs%(4), f2%(4),  0%, rslt$(4))

            f1%(1), f1%(2), f1%(3), f1%(4) = 0%

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
L10210:               if keyhit% = 14% then gosub utility_screen
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 12% then gosub delete_skewno
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
            *             G e n e r i c   S c r e e n                   *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        utility_screen
           init(" ") beg_sku$,end_sku$, beg_desc$,end_desc$
           sort$ = "1"
           sort_desc$ = "(1) Report Sequence By Sku No."

L19110:    gosub'102(1%,1%)
           if keyhit% = 1% then gosub startover
           if keyhit% = 16% then goto exit_program
           if keyhit% <> 0% then goto L19110
              if beg_sku$ <> "ALL" then goto L19190
                 beg_sku$ = "ALL" : beg_desc$ ="(ALL) Customer Sku No.s."
                 end_sku$ = " "   : end_desc$ = " "
                 goto utility_next
L19190:       convert beg_sku$ to beg_sku%, data goto L19470

              convert beg_sku% to beg_sku$, pic(000)
              sku_code$ = beg_sku$
              gosub sku_lookup
              if sku% = 0% then goto L19470
                 beg_desc$ = sku_desc$

              if end_sku$ <> " " then goto L19300
                 end_sku$ = beg_sku$

L19300:       convert end_sku$ to end_sku%, data goto L19470

              convert end_sku% to end_sku$, pic(000)
              sku_code$ = end_sku$
              gosub sku_lookup
              if sku% = 0% then goto L19470
                 end_desc$ = sku_desc$
        utility_next
           if sort$="1" then sort_desc$="(1) Report Sequence By Sku No."
           if sort$="2" then sort_desc$="(2) Report Sequence By Part.  "
           if sort$ <> "1" and sort$ <> "2" then goto utility_screen
           gosub'102(0%,2%)
           if keyhit% =  1% then gosub startover
           if keyhit% = 14% then gosub print_report
           if keyhit% = 16% then gosub utility_check
           goto utility_next

L19470:    errormsg$ = "(Error) - Invalid Beg/End Customer Sku Codes."
           goto utility_screen

        utility_check
           if str(beg_sku$,1%,1%) <> "A" and beg_sku$ <> end_sku$ then   ~
              goto L19560
           errormsg$ = "(Error) - Invalid From/To Customer Sku Codes."
           goto utility_screen

L19560: gosub copy_skuno

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
         "Enter Valid Customer Sku Code.                               ",~
         "Enter Valid Sku Number.  ( Max. (25) Characters )            ",~
         "Enter Valid Part Number. ( Max. (25) Characters )            ",~
         "Enter Applicable Sku Stock Price.                            ",~
         "Enter the Applicable Color Label Code 00=N/A, or 01 thru 05? "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28260
                inpmessage$ = edtmessage$
                return

L28260
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Beg/End or Copy From/To Customer Sku Code or All"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, partno$, desc$, sku_code$, ~
                      skuno$, sku_price$, sku_desc$, sku_fil$, sku_key$, ~
                      sav_sku$, upccode$, sku_rec$, beg_sku$, end_sku$,  ~
                      beg_desc$, end_desc$, scr$(), sort$, sort_desc$,   ~
                      sku_label$, sku_label_d$
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
          str(sku_key$,1%,3%)  = sku_code$
          str(sku_key$,4%,25%) = skuno$
          read #3,key = sku_key$, using L35040, sku_code$, skuno$,        ~
                                 sku_code$, partno$, sku_price, sku_label$,~
                                 sku_fil$, eod goto L30170
          gosub sku_lookup                             /* (EWD001) -    */
          gosub part_lookup
          convert sku_price to sku_price$, pic($#####.##-)

          if sku_label$ <> " " then goto L30165
             sku_label$ = "00"    
L30165:      gosub lookup_label
          rec% = 1%                                         /* ON FILE */
L30170: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        dataput
          str(sku_key$,1%,3%)  = sku_code$
          str(sku_key$,4%,25%) = skuno$
          read #3,hold,key = sku_key$, eod goto L31110
            delete #3
L31110:   write #3, using L35040, sku_code$, skuno$, sku_code$, partno$,  ~
                                      sku_price, sku_label$,              ~
                                      sku_fil$, eod goto L31150
        return clear all                           /* (EWD001) -     */
        goto inputmode
L31150:   errormsg$ = "(Error) - Unable to Update Sku Number "
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCSKUNO - Sku Number File */
L35040:     FMT CH(03),                  /* Customer Sku Code          */~
                CH(25),                  /* Sku Number                 */~
                CH(03),                  /* Customer Sku Code          */~
                CH(25),                  /* APC Part Number            */~
                PD(14,4),                /* Sku Stock Price            */~
                CH(2),                   /* Color Label (EWD001)       */~
                CH(7)                    /* Filler (EWD001)            */

                                         /* APCSKEW - Old Format       */
            FMT CH(09),                  /* Customer Number - Bill-To  */~
                CH(25),                  /* Part Number                */~
                CH(25),                  /* Skew Number                */~
                CH(05)                   /* Filler Area                */

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
              on fieldnr% gosub L40090,         /* Customer Sku Code */   ~
                                L40090,         /* Sku Number        */   ~
                                L40090,         /* Part Number       */   ~
                                L40095,         /* Sku Stock Price   */   ~
                                L40095          /* Color Label Code  */
              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40095:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Create/Edit Sku Number Data Utility",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sku Code    :",                              ~
               at (06,20), fac(lfac$( 1)), sku_code$            , ch(03),~
               at (06,46), fac(hex(84)),   sku_desc$            , ch(30),~
                                                                         ~
               at (07,02), "Sku Number  :",                              ~
               at (07,20), fac(lfac$( 2)), skuno$               , ch(25),~
                                                                         ~
               at (08,02), "Part Number :",                              ~
               at (08,20), fac(lfac$( 3)), partno$              , ch(25),~
               at (08,46), fac(hex(84)),   desc$                , ch(32),~
                                                                         ~
               at (09,02), "Stock Price :",                              ~
               at (09,20), fac(lfac$( 4)), sku_price$           , ch(10),~
               at (09,40), "UPC:",                                       ~
               at (09,46), fac(hex(84)),   upccode$             , ch(20),~
                                                                         ~
               at (10,02), "Color Label :",                              ~
               at (10,20), fac(lfac$( 5)), sku_label$           , ch(02),~
               at (10,46), fac(hex(84)),   sku_label_d$         , ch(30),~ 
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9 then goto L40275
                  gosub print_rpt_a
                  goto L40105

L40275:        if keyhit% <> 15 then goto L40295
                  call "PRNTSCRN"
                  goto L40105

L40295:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40400     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Utility Scr "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Customer Cross Ref. " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffff0e0f1000)
            if fieldnr% = 1% then L40375
                str(pf$(3),64%)   = " "  :  str(pfkeys$,16%,1%) = hex(ff)
                str(pf$(1),64%)   = " "  :  str(pfkeys$,14%,1%) = hex(ff)
L40375:     if fieldnr% > 1% then L40390
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1)   = hex(ff)
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 9,1)   = hex(ff)
L40390:     return

L40400: if fieldnr% > 0% then L40455  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 ( 9)Customer Cross Ref." &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (12)Delete Sku No.     " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ff03ffffffffff09ffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(3),18%,30%) = " " : str(pfkeys$,12%,1%)=hex(ff)
            return
L40455:                              /*  Edit Mode - Enabled    */
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
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41180          /* Beginning/ From   */
                                               /* Ending/To Sku No. */


              goto L41210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41210:     accept                                                       ~
               at (01,02),                                               ~
                  "Sku Number Report / Copy Utility Program",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beginning/From Sku Number:",                 ~
               at (06,30), fac(lfac$( 1)), beg_sku$             , ch(03),~
               at (06,50), fac(hex(84)),   beg_desc$            , ch(30),~
               at (07,02), "Ending/To Sku Number     :",                 ~
               at (07,30), fac(lfac$( 1)), end_sku$             , ch(03),~
               at (07,50), fac(hex(84)),   end_desc$            , ch(30),~
               at (08,02), "Report (1)Sku, (2)Part   :",                 ~
               at (08,30), fac(lfac$( 1)), sort$                , ch(01),~
               at (08,50), fac(hex(84)),   sort_desc$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41460
                  call "PRNTSCRN"
                  goto L41210

L41460:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L41610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L41610: if fieldnr% > 0% then L41700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Copy Data   "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L41700:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50150,         /* Customer Sku Code     */ ~
                              L50270,         /* Sku Number            */ ~
                              L50360,         /* Part Number           */ ~
                              L50520,         /* Sku Stock Price       */ ~
                              L50700          /* Color Label Code      */ 

            return

L50150: REM Customer Sku Code                     SKU_CODE$
           convert sku_code$ to sku_code%, data goto L50230

           convert sku_code% to sku_code$, pic(000)

           gosub sku_lookup
           if sku% = 0% then goto L50230
        return
L50230:    errormsg$ = "(Error) - Customer Sku Code is Required."
           init(" ") sku_code$, sku_desc$
        return

L50270: REM Sku Number                            SKUNO$
           if skuno$ <> " " then goto L50310
              errormsg$ = "(Error) Invalid Sku Number?"

L50310:    gosub dataload
           if rec% = 0% then return
              if edit% = 1% then fieldnr% = 5%
        return

L50360: REM Part Number                           PARTNO$
           if partno$ <> " " then goto L50420
              desc$ = hex(06) & "Select Manufactured Part"
              call "PLOWCODE" (#2, partno$, desc$, 0%, .30, f1%(2))
              desc$ = " "
              if f1%(2) = 0 then goto L50480
L50420:    if len(partno$) < 19% then goto L50480
        part_lookup
           part% = 0%
           read #2,key = partno$, eod goto L50480
           get #2, using L50460, desc$, upccode$
L50460:       FMT POS(26), CH(32), POS(566), CH(20)
           part% = 1%
        return
L50480:    errormsg$ = "(Error) Invalid Manufactured Part No. Selection?"
           partno$, desc$ = " "
        return

L50520: REM Sku Stock Price                       SKU_PRICE$
           if sku_price$ <> " " then goto L50560
              sku_price$ = "0.0"

L50560:    convert sku_price$ to sku_price, data goto L50600

           convert sku_price to sku_price$, pic($#####.##-)
        return
L50600:    errormsg$ = "(Error) - Invalid Price Entered."
           sku_price$ = " " : sku_price = 0.0
        return

L50700: REM Sku Color label code                  SKU_LABEL$
            init(" ") readkey$, sku_label_d$
            if sku_label$ <> " " then goto L50710
               sku_label$ = "00"
L50710:     gosub lookup_label
        return

        lookup_label
            str(readkey$,1%,9%)   = "PLANLABEL"
            str(readkey$,10%,15%) = sku_label$
L50715:     read #4,key = readkey$, using L50720, sku_label_d$,          ~
                                                          eod goto L50730
L50720:         FMT POS(25), CH(30)
        return
L50730:     sku_label$ = "00"
            goto L50715
        return  

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!######## ########                  ############################~
        ~################################                      APCSKU:!

L55080: %!USER ID: ###                       ############################~
        ~################################                  PAGE: #####!

L55110: %!Customer Sku Code: ###  ##############################         ~
        ~                                                             !

L55140: %+---------------------------------------------------------------~
        ~-------------------------------------------------------------+

L55170: %!---------------------------------------------------------------~
        ~-------------------------------------------------------------!

                                                   /* Column Header   */
L55210: %!<------- Sku Number ---->!<----- Part  Number ---->!<------ Par~
        ~t Description ------->!<- Upc Code ->!<- Stock Price ->!Label!

L55240: %!-------------------------!-------------------------!-----------~
        ~----------------------!--------------!-----------------!-----!

                                                   /* Detail Data   */
L55280: %!#########################!#########################!###########~
        ~######################! ############ !    ##########   !  ## !


L55320: %!Seq.!Customer Code!<--------- Description -------->!<----- City~
        ~ ----->!<-State->!<-- Zip  -->!<--- Phone -->!   <Remarks>   !

L55350: %!----!-------------!--------------------------------!-----------~
        ~-------!---------!------------!--------------!---------------!

L55380: %!####!  #########  ! ############################## !###########~
        ~#######!   ##    ! ########## !##############!               !

        print_header
          if lcnt% <> 99% then print using L55140
          page_no% = page_no% + 1%
          print page
          print using L55140
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print using L55110, sku_code$, sku_desc$
          print using L55170
          print using L55210
          lcnt% = 6%
        return

        print_header_a
          if lcnt% <> 99% then print using L55140
          page_no% = page_no% + 1%
          print page
          print using L55140
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print using L55110, sku_code$, sku_desc$
          print using L55170
          print using L55320
          lcnt% = 6%
        return

        print_detail
          if lcnt% > 58% then gosub print_header
          if sav_sku$ = sku_code$ then goto L55740
             gosub sku_lookup
             sav_sku$ = sku_code$
             gosub print_header

L55740:   print using L55240
          print using L55280, skuno$, partno$, desc$,str(upccode$,1%,12%),~
                             sku_price$, sku_label$
          lcnt% = lcnt% + 2%
        return

        print_detail_a
          if lcnt% > 58% then gosub print_header_a
          print using L55350
          print using L55380, cnt$, cus_code$, cus_desc$, cus_city$,      ~
                             cus_st$, cus_zip$, cus_phone$
          lcnt% = lcnt% + 2%
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SHOSTAT" ("Creating Sku Number Report")
            page_no% = 0%
            lcnt%    = 99%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCSKU", " ", 0%, 0%)
            print_title$ = "Customer Sku Number Parts"
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCSKU", " ", 0%, 1%)
        return

        copy_skuno
          gosub prompt_user
          if comp% = 0% then goto L60490                /* Exit Process */
             sku_key$ = all(hex(00))
             str(sku_key$,1%,3%) = beg_sku$
             read #3,key > sku_key$,using L60370,sku_key$,eod goto L60280
             if beg_sku$ = str(sku_key$,1%,3%) then goto L60310
L60280:         errormsg$ = "No Data on File for 'Beginning Sku Code'."
                goto copy_skuno

L60310:   call "SHOSTAT" ("Copying Customer Sku Code Data")

          sku_key$ = all(hex(00))
          str(sku_key$,1%,3%) = beg_sku$
L60350:   read #3,key > sku_key$,using L60370, sku_rec$, eod goto L60490

L60370:     FMT CH(73)
          sku_key$ = str(sku_rec$,1%,28%)
          if str(sku_key$,1%,3%) <> beg_sku$ then goto L60490
          str(sku_rec$,1%,3%)  = end_sku$
          str(sku_rec$,29%,3%) = end_sku$
          read #3,hold,key = str(sku_rec$,1%,28%), eod goto L60450
          delete #3

L60450:      put #3,using L60370, sku_rec$
             write #3, eod goto L60350
          goto L60350

L60490: return clear all
        goto inputmode

        delete_skewno
          gosub prompt_user
          if comp% = 0% then goto L60610                /* Exit Process */
             sku_key$ = all(hex(00))
             str(sku_key$,1%,3%)  = sku_code$
             str(sku_key$,4%,25%) = skuno$
             read #3,hold,key = sku_key$, eod goto L60600
               delete #3
L60600:      rec% = 0%
L60610: return clear all
        goto inputmode

        prompt_user
            comp% = 2%
            hdr$ = "** Sku Number Maintenance **"
            msg$(1) = " **********  Do You Wish to Continue  ********** "
            msg$(2) = "Press <RETURN> To 'Exit Process', or Press Any   "
            msg$(3) = "(PF) Key To Continue...........                  "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        sku_lookup
           init(" ") readkey$, sku_desc$
           sku% = 0%
           str(readkey$,1%,9%)   = "SKU CODE "
           str(readkey$,10%,15%) = sku_code$
           read #4,key = readkey$, using L60790, sku_desc$,eod goto L60810
L60790:       FMT POS(25), CH(32)
           sku% = 1%
L60810: return

        print_report
            gosub prompt_user
            if comp% = 0% then goto L61180             /* Exit Process */
            sav_sku$ = " "
            gosub select_printer
            sku_key$ = all(hex(00))
            if beg_sku$ <> "ALL" then str(sku_key$,1%,3%) = beg_sku$
            if sort$ = "2" then goto L60970
                                                  /* SKU NO.  SEQUENCE */
            read #3,key > sku_key$, using L35040, sku_code$, skuno$,      ~
                                          sku_code$, partno$, sku_price, ~
                                sku_label$, sku_fil$, eod goto print_done
            goto L61000
                                                  /* PART NO. SEQUENCE */
L60970:        read #3,key 1% > sku_key$, using L35040, sku_code$,        ~
                                  skuno$, sku_code$, partno$, sku_price, ~
                                 sku_label$, sku_fil$, eod goto print_done
L61000:     sav_sku$ = sku_code$
            gosub sku_lookup
            goto L61070
L61030: next_part
            read #3, using L35040, sku_code$, skuno$, sku_code$, partno$, ~
                                          sku_price, sku_label$, sku_fil$,~
                                                     eod goto print_done
L61070:     if beg_sku$ = "ALL" then goto L61030
              if sort$ = "2" and sku_code$ > end_sku$ then goto next_part
               if sku_code$ > end_sku$ then goto print_done

            gosub part_lookup
            convert sku_price to sku_price$, pic($#####.##-)
            gosub print_detail
            goto next_part
        print_done
            print using L55140
            gosub close_printer
L61180: return clear all
        goto inputmode

        lookup_customer
            get #1, using L61240, cus_desc$, cus_city$, cus_st$,          ~
                                 cus_zp$, cus_ph$
L61240:        FMT POS(10), CH(30), POS(403), CH(18), CH(2), XX(1),      ~
                   CH(9), XX(20), CH(10)
            cus_zip$ = str(cus_zp$,1%,5%) & "-" & str(cus_zp$,6%,4%)
            cus_phone$ = "("&str(cus_ph$,1%,3%)&") "&str(cus_ph$,4%,3%)  ~
                         & "-" & str(cus_ph$,7%,4%)
        return

        print_rpt_a
            gosub select_printer
            call "SHOSTAT" ("Customer Sku Reference Report")
            print_title$ = "Customer Sku Reference Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            cnt% = 0%
            cus_key$ = all(hex(00))
            read #1,key > cus_key$, using L61430, cus_code$, sav_sku$,    ~
                                                 eod goto rpt_done
            goto L61440
        rpt_next
            read #1, using L61430, cus_code$, sav_sku$, eod goto rpt_done
L61430:        FMT CH(9), POS(1000), CH(3)
L61440:     if sav_sku$ <> sku_code$ then goto rpt_next
            gosub lookup_customer
            cnt% = cnt% + 1%
            convert cnt% to cnt$, pic(0000)

            gosub print_detail_a
            goto rpt_next
        rpt_done
            print using L55140
            gosub close_printer
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
