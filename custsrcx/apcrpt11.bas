        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   RRRR   PPPP   TTTTT    1      1     *~
            *  A   A  P   P  C   C  R   R  P   P    T     11     11     *~
            *  AAAAA  PPPP   C      RRRRR  PPPP     T      1      1     *~
            *  A   A  P      C   C  R  R   P        T      1      1     *~
            *  A   A  P       CCC   R   R  P        T    11111  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCRPT11 - Total Customer Sales By Date, By Customer,     *~
            *            and By Product, by Route                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/06/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 12/03/97 ! Mods for Release Upgrade to R6.04.03     ! RHH *~
            * 03/30/98 ! Y2K modifications                        ! ERN *~
            *************************************************************

        dim                                                              ~
            grp_route$5,                 /* Customer Route Code        */~
            route1$5,                    /* Beginning Route Code       */~
            route2$5,                    /* Ending Route Code          */~
            sav_route$5,                 /* Customer Route Code        */~
            apc_scr$120,                 /* SCREEN DESCRIPTION         */~
            apc_prt$60,                  /* PRINT DESCRIPTION          */~
            apc_sze$20,                  /* SIZE DESCRIPTION           */~
            type$1,                      /* Report Type (C)ust, (M)odel*/~
            type_desc$35,                /* Report Type Description    */~
            cus1$9,                      /* Beginning Customer Code    */~
            cus2$9,                      /* Ending Customer Code       */~
            dte1$10, beg_dte$10,         /* Beg     Sales Order Date   */~
            dte2$10, end_dte$10,         /* End     Sales Order Date   */~
            mod1$3,                      /* Beg Model Code             */~
            mod2$3,                      /* End Model Code             */~
            descr$32,                    /* Generic Description        */~
            bck_key$25,                  /* BCKMASTR Primary Key       */~
            ord_dte$6,                   /* SALES ORDER DATE           */~
            ln_key$19,                   /* BCKLINES Primary Key       */~
            ln_rec$256,                  /* BCKLINES Record            */~
            cuscode$9,                   /* Customer Code of S.O.      */~
            part$25,                     /* Part No. of S.O.           */~
            wrk_key$40,                  /* Work Key Primary           */~
            pcnt$6,                      /*                            */~
            so$16,                       /*                            */~
            stock_key$32,                /*                            */~
            wrk_stock$,                  /*                            */~
            wrk_qty$6,                   /*                            */~
            wrk_sales$14,                /*                            */~
            tot_mod$6,                   /*                            */~
            tot_mod1$6,                  /*                            */~
            tot_mod2$6,                  /*                            */~
            tot_stk$6,                   /*                            */~
            tot_stk1$6,                  /*                            */~
            tot_stk2$6,                  /*                            */~
            tot_sales$14,tot_sales1$14,  /*                            */~
            tot_sales2$14,               /*                            */~
            model$3,                     /* MODEL CODE                 */~
            mod_desc$32,                 /* MODEL DESCRIPTION          */~
            sav_model$3,                 /* MODEL CODE                 */~
            sav_cuscode$9,               /* CUSTOMER CODE              */~
            sav_part$25,                 /* PART NUMBER                */~
            cust_name$30,                /* CUSTOMER NAME              */~
            part_desc$32,                /* PART DESCRIPTION           */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
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
            dim apc$40, pname$21
            apc$   = "(EWD) Customer Sales, Product,Mod,Route "
            pname$ = "APCRPT11 - Rev: R6.04"

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
            * #01 ! CUSTOMER ! Customer Master File                     *~
            * #02 ! BCKMASTR ! Sales Order Header File                  *~
            * #03 ! BCKLINES ! Sales Order Detail File                  *~
            * #04 ! GENCODES ! System Master Code Table Files           *~
            * #05 ! HNYMASTR ! Part Master File                         *~
            * #06 ! APCSTOCK ! Standard Stock Products                  *~
            * #08 ! AMTBOMIF ! PART NUMBER VALIDITY FILE                *~
            * #10 ! APCRPTWK ! Report Work File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #05, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #6,  "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =   70,              ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =    8, keylen =  32

            select #8,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            select #10, "APCRPTW1",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  40,                     ~
                        alt key  1, keypos =   53, keylen =  12, dup,    ~
                            key  2, keypos =   15, keylen =  25, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#04, fs%(04), f2%(04),  0%, rslt$(04))

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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   5%
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
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
            gosub open_files

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
         "Enter a Valid Rpt Type (C)ustomer,(M)odel,(S)ummary,(R)oute  ",~
         "Enter a Valid Beginning and Ending Customer Code, or (ALL).  ",~
         "Enter a Valid Beginning and Ending Order Date, or (ALL).     ",~
         "Enter a Valid Beginning and Ending Model Code, or (ALL).     ",~
         "Enter a Valid Beginning and Ending Route Code, or (ALL).     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, cus1$, cus2$, dte1$, dte2$,~
                      beg_dte$, end_dte$, mod1$, mod2$, descr$,          ~
                      readkey$, bck_key$, ln_key$, ln_rec$, wrk_key$,    ~
                      model$, cuscode$, tot_mod$, tot_mod1$, tot_mod2$,  ~
                      tot_sales$, tot_sales1$, tot_sales2$, tot_stk$,    ~
                      tot_stk1$, tot_stk2$,                              ~
                      cust_name$, part_desc$, wrk_qty$, wrk_sales$,      ~
                      type$, type_desc$, grp_route$, route1$, route2$
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
              on fieldnr% gosub L40180,         /* Report Type       */   ~
                                L40180,         /* Beg/End Customer  */   ~
                                L40180,         /* Beg/End Order Date*/   ~
                                L40180,         /* Beg/End Model Code*/   ~
                                L40180          /* Beg/End Route Code*/
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (06,02), "Report Type (C,M,S,R):",                     ~
               at (06,25), fac(lfac$( 1)), type$                , ch(01),~
               at (06,40), fac(hex(84)), type_desc$             , ch(35),~
                                                                         ~
               at (07,02), "Beg/End Customer Code:",                     ~
               at (07,25), fac(lfac$( 2)), cus1$                , ch(09),~
               at (07,40), fac(lfac$( 2)), cus2$                , ch(09),~
                                                                         ~
               at (08,02), "Beg/End Order Date   :",                     ~
               at (08,25), fac(lfac$( 3)), dte1$                , ch(10),~
               at (08,40), fac(lfac$( 3)), dte2$                , ch(10),~
                                                                         ~
               at (09,02), "Beg/End Model Code   :",                     ~
               at (09,25), fac(lfac$( 4)), mod1$                , ch(03),~
               at (09,40), fac(lfac$( 4)), mod2$                , ch(03),~
                                                                         ~
               at (10,02), "Beg/End Route Code   :",                     ~
               at (10,25), fac(lfac$( 5)), route1$              , ch(05),~
               at (10,40), fac(lfac$( 5)), route2$              , ch(05),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40560
                  call "PRNTSCRN"
                  goto L40210

L40560:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40750     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40710
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40710:     if fieldnr% > 1% then L40730
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40730:     return

L40750: if fieldnr% > 0% then L40840  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40840:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50150,         /* Report Type           */ ~
                              L50330,         /* Beg/End Customer Code */ ~
                              L50510,         /* Beg/End Order Date    */ ~
                              L50760,         /* Beg/End Model Code    */ ~
                              L50990          /* Beg/End Route Code    */
            return

L50150: REM Report Type
            if type$ <> " " then goto L50180
               type$ = "M"
L50180:     p% = pos("CMSR" = type$)
            if p% = 0% then goto L50290
            if type$ = "C" then                                          ~
                       type_desc$ = "Customer Product Sales with Summary"
            if type$ = "M" then                                          ~
                       type_desc$ = "Model Product Sales with Summary"
            if type$ = "S" then                                          ~
                       type_desc$ = "Part Number Summary Only"
            if type$ = "R" then                                          ~
                       type_desc$ = "Route, Customer Prod Sales With Sum"
        return
L50290:     errormsg$ = "(Error) Invalid Report Type. (C,M,S or R)?"
            init(" ") type$, type_desc$
        return

L50330: REM Beginning and Ending Customer         CUS1$,CUS2$
            if cus1$ = "ALL" then goto L50480
            if cus1$ <> " "  then goto L50370
               goto L50480
L50370:     if cus2$ <> " " then goto L50400
               cus2$ = cus1$

L50400:     call "DESCRIBE" (#1, cus1$, descr$, 0%, f1%(1))
            if f1%(1) = 0 then goto L50450
            call "DESCRIBE" (#1, cus2$, descr$, 0%, f1%(1))
            if f1%(1) = 0 then goto L50450
        return
L50450:     errormsg$ = "(Error) Invalid Customer Code. "
            init(" ") cus1$, cus2$
        return
L50480:     cus1$ = "ALL" : cus2$ = "ALL"
        return

L50510: REM Beginning and Ending Order Date        DTE1$,DTE2$
            if str(dte1$,1%,3%) = "ALL" then goto L50720
            if dte1$ <> " " then goto L50560
               goto L50720

L50560:        date% = 0%
               call "DATEOKC" (dte1$, date%, errormsg$)
               if errormsg$ <> " " then return
               if dte2$ <> " " then goto L50620
                  dte2$ = dte1$

L50620:           call "DATEOKC" (dte2$, date%, errormsg$)
                  if errormsg$ <> " " then return
               beg_dte$ = dte1$
               end_dte$ = dte2$
               call "DATUFMTC" (beg_dte$)
               call "DATUFMTC" (end_dte$)
        return
               errormsg$ = "(Error) Invalid Order Date."
               init(" ") dte1$, dte2$
        return
L50720:     dte1$, dte2$ = "ALL"
            beg_dte$ = "ALL" : end_dte$ = "ALL"
        return

L50760: REM Beginning and Ending Model Code        MOD1$,MOD2$
            if mod1$ = "ALL" then goto L50960
            if mod1$ <> " "  then goto L50800
               goto L50960
L50800:     if mod2$ <> " " then goto L50830
               mod2$ = mod1$

L50830:     readkey$ = all(hex(00))
            readkey$ = "MODEL    " & mod1$
            call "DESCRIBE" (#4, readkey$, descr$, 0%, f1%(4))
            if f1%(4) = 0 then goto L50930
            readkey$ = all(hex(00))
            readkey$ = "MODEL    " & mod2$
            call "DESCRIBE" (#4, readkey$, descr$, 0%, f1%(4))
            if f1%(4) = 0 then goto L50930
            if mod1$ > mod2$ then goto L50930
        return
L50930:     errormsg$ = "(Error) Invalid Model Code.."
            init(" ") mod1$, mod2$
        return
L50960:     mod1$ = "ALL" : mod2$ = "ALL"
        return

L50990: REM Beginning and Ending Route Code        ROUTE1$,ROUTE2$
            if route1$ = "ALL" then goto L51190
            if route1$ <> " "  then goto L51030
               goto L51190
L51030:     if route2$ <> " " then goto L51060
               route2$ = route1$

L51060:     readkey$ = all(hex(00))
            readkey$ = "ROUTECODE" & route1$
            call "DESCRIBE" (#4, readkey$, descr$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51160
            readkey$ = all(hex(00))
            readkey$ = "ROUTECODE" & route2$
            call "DESCRIBE" (#4, readkey$, descr$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51160
            if route1$ > route2$ then goto L51160
        return
L51160:     errormsg$ = "(Error) Invalid Model Code.."
            init(" ") route1$, route2$
        return
L51190:     route1$ = "ALL" : route2$ = "ALL"
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########                        ####################~
        ~####################                                    APCRPT11:~
        ~!

L55090: %!Customer Beg:#########    End:#########    ####################~
        ~####################                                  Page: #####~
        ~!

L55130: %!Order Dt Beg: ##########  End: ##########                      ~
        ~                                        Model Beg: ###   End: ###~
        ~!

L55170: %!Route    Beg:#########    End:#########                        ~
        ~                                                                 ~
        ~!
                                                   /* Customer Header */
L55210: %!Customer !<----------- Name ----------->!<----- Part Number ---~
        ~-->!<------ Part Description ------>! Stock!Ord QT!Sales Dollars ~
        ~!

L55250: %!---------!------------------------------!----------------------~
        ~---!--------------------------------!------!------!--------------~
        ~!

L55290: %!#########!##############################!######################~
        ~###!################################!######!######!##############~
        ~!
L55320: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55360: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!

L55400: %!                                   Stock Percentage ( ###.##-) ~
        ~             Totals For Model (###) !######!######!##############~
        ~!

L55440: %!                                   Stock Percentage ( ###.##-) ~
        ~    Totals For Customer (#########) !######!######!##############~
        ~!

L55480: %!                                   Stock Percentage ( ###.##-) ~
        ~    Totals For Route    (  #####  ) !######!######!##############~
        ~!

                                                   /* Model Header    */
L55530: %!Model!<-------- Description --------->!Customer !<------- Custo~
        ~mer Name ------->!- Standard Stock -!Quantity!<-Sales Dollars->  ~
        ~!

L55570: %!-----!--------------------------------!---------!--------------~
        ~-----------------!------------------!--------!-------------------~
        ~!

L55610: %! ### !################################!#########!##############~
        ~################ !      ######      ! ###### ! ##############    ~
        ~!

L55650: %!                          Stock Percentage ( ###.##-)    Totals~
        ~ For Model (###) !      ######      ! ###### ! ##############    ~
        ~!

                                                     /* Summary Report */
L55700: %!<----- Part Number ----->!<------ Part Description ------>!<---~
        ~----------------------------------->!Stock !Qty Or!Sales Dollars ~
        ~!
L55730: %!#########################!################################!####~
        ~####################################!######!######!##############~
        ~!
L55760: %!-------------------------!--------------------------------!----~
        ~------------------------------------!------!------!--------------~
        ~!

L55800: %!                                                               ~
        ~        Stock Percentage ( ###.##-) !######!######!##############~
        ~!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("APCRPT", " ",25000%, 0%)
            select printer (134)
        select_printer_again
            page_no% = 0%
            lcnt%    = 99%

            print_title$ = type_desc$
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "FMTTITLE" (company$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("APCRPT", " ",0%, 1%)
        return

        generate_report
            gosub select_data
            call "SHOSTAT" ("PRINTING REPORT")
            gosub select_printer
L60270: REM ( SUMMARY ENTRY )
            tot_mod% = 0% : tot_mod1% = 0% : tot_mod2% = 0%
            tot_stk% = 0% : tot_stk1% = 0% : tot_stk2% = 0%
            tot_sales = 0.0 : tot_sales1 = 0.0 : tot_sales2 = 0.0
            finish% = 0%
            init(" ") sav_model$, sav_cuscode$, sav_part$, sav_route$
            wrk_key$ = all(hex(00))
                                            /* CUSTOMER AND ROUTE SCAN */
            if type$ <> "C" and type$ <> "R" then goto L60400
               read #10,key > wrk_key$, using L60640, wrk_key$, wrk_qty%, ~
                                        wrk_stock%, wrk_sales, model$,   ~
                                        cuscode$, eod goto generate_done
               goto L60530
L60400:     if type$ <> "M" then goto L60460          /* MODEL SCAN     */
            read #10,key 1% > wrk_key$, using L60640, wrk_key$, wrk_qty%, ~
                                        wrk_stock%, wrk_sales, model$,   ~
                                        cuscode$, eod goto generate_done
            goto L60530
                                                     /* SUMMARY SCAN   */
L60460:     read #10,key 2% > wrk_key$, using L60640, wrk_key$, wrk_qty%, ~
                                        wrk_stock%, wrk_sales, model$,   ~
                                        cuscode$, eod goto generate_done
               sav_part$ = str(wrk_key$,15%,25%)
               part$ = sav_part$
               gosub lookup_part_long

L60530:       sav_model$   = model$
              sav_cuscode$ = cuscode$
              sav_route$   = str(wrk_key$,1%,5%)
              gosub lookup_customer
              if type$ = "M" then gosub lookup_model
            goto L60660
        generate_next
            read #10, using L60640, wrk_key$, wrk_qty%, wrk_stock%,       ~
                                   wrk_sales, model$, cuscode$,          ~
                                   eod goto generate_done

L60640:        FMT CH(40), 2*BI(2), PD(14,4), CH(3), CH(9)

L60660:     convert wrk_qty%   to wrk_qty$,   pic(######)
            convert wrk_stock% to wrk_stock$, pic(######)
            convert wrk_sales  to wrk_sales$, pic(##,###,###.##-)
            grp_route$ = str(wrk_key$,1%,5%)
            if type$ <> "C" and type$ <> "R" then goto L60750
               part$ = str(wrk_key$,15%,25%)              /* CUSTOMER  */
               if sav_part$ <> part$ then gosub lookup_part
               gosub print_detail
               goto generate_next
L60750:     if type$ <> "M" then goto L60810
               str(sav_key$,1%,3%) = model$               /* MODEL     */
               str(sav_key$,4%,9%) = cuscode$
               gosub print_detail_m
               goto generate_next

L60810:        part$ = str(wrk_key$,15%,25%)              /* SUMMARY   */
               gosub print_detail_s
               goto generate_next
        generate_done
            finish% = 1%
            if type$ <> "C" then goto L60940
               gosub print_model_total
               gosub print_sales_total
               gosub print_route_total
               print using L55320
               type$ = "S"
               gosub select_printer_again
               goto L60270
L60940:     if type$ <> "M" then goto L61020
               model$ = "XXX"
               gosub print_detail_m
               print using L55320
               type$ = "S"
               gosub select_printer_again
               goto L60270

L61020:     finish% = 1%                           /* Close out Report */
            gosub print_detail_sum
            gosub print_sum_total
            print using L55320

            gosub close_printer
            call "FILEBGON" addr(#10)
        return

        lookup_part
            call "DESCRIBE" (#5, part$, part_desc$, 0%, f1%(5))
            sav_part$ = part$
            if f1%(5) <> 0 then return
        lookup_part_long
               err% = 0%
               call "APCDESCR" (part$, apc_scr$, apc_prt$, apc_sze$,     ~
                                 #8, err%)
               str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
               str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
        return

        lookup_customer
            read #1, key = cuscode$, using L61260, cust_name$,            ~
                                                          eod goto L61270
L61260:        FMT POS(10), CH(30)
L61270: return

        lookup_model
            readkey$ = all(hex(00))
            readkey$ = "MODEL    " & model$
            call "DESCRIBE" (#4, readkey$, mod_desc$, 0%, f1%(4))
        return

        print_header
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55320
          lcnt% = 0%
          print page
          print using L55320
          print using L55050, date$, rpt_time$, company$
          print using L55090, cus1$, cus2$, print_title$, page_no%
          print using L55130, dte1$, dte2$, mod1$, mod2$
          if type$ <> "R" then goto L61470
             print using L55170, route1$, route2$
             lcnt% = lcnt% + 1%
L61470:   print using L55360
          if type$ = "C" then print using L55210
          if type$ = "M" then print using L55530
          if type$ = "S" then print using L55700
          if type$ = "R" then print using L55210
          lcnt% = lcnt% + 6%
        return

        print_detail_s
          if sav_part$ = part$ then goto L61740

        print_detail_sum
             if lcnt% > 57% then gosub print_header
             convert tot_mod%  to tot_mod$,   pic(######)
             convert tot_stk%  to tot_stk$,   pic(######)
             convert tot_sales to tot_sales$, pic(##,###,###.##-)
             print using L55760
             print using L55730, sav_part$, part_desc$,                   ~
                    str(apc_scr$,1%,40%), tot_stk$, tot_mod$, tot_sales$
             lcnt% = lcnt% + 2%
             tot_mod%  = 0%
             tot_stk%  = 0%
             tot_sales = 0.0
             sav_part$ = part$
             gosub lookup_part_long
             if finish% = 1% then return

L61740:   tot_mod%   = tot_mod% + wrk_qty%
          tot_stk%   = tot_stk% + wrk_stock%
          tot_sales  = round(tot_sales + wrk_sales, 2)
          tot_mod1%  = tot_mod1% + wrk_qty%
          tot_stk1%  = tot_stk1% + wrk_stock%
          tot_sales1 = round(tot_sales1 + wrk_sales, 2)
        return

        print_sum_total
            pcnt = 0.0
            x = tot_stk1% : y = tot_mod1%
            if y = 0 then goto L61870
            pcnt = round ( x / y * 100, 2 )
L61870:     convert pcnt to pcnt$, pic(###.##-)

            convert tot_mod1%  to tot_mod1$,   pic(######)
            convert tot_stk1%  to tot_stk1$,   pic(######)
            convert tot_sales1 to tot_sales1$, pic(##,###,###.##-)
            print using L55760
            print using L55800, pcnt$, tot_stk1$, tot_mod1$, tot_sales1$
        return

        print_detail
          if sav_model$ <> model$ then gosub print_model_total
          if sav_cuscode$ <> cuscode$ then gosub print_sales_total
          if sav_route$ <> grp_route$ then gosub print_route_total

          if lcnt% > 57% then gosub print_header
          print using L55250
          print using L55290, cuscode$, cust_name$, part$, part_desc$,    ~
                             wrk_stock$, wrk_qty$, wrk_sales$
          lcnt% = lcnt% + 2%
          tot_mod%   = tot_mod% + wrk_qty%
          tot_stk%   = tot_stk% + wrk_stock%
          tot_sales  = round(tot_sales + wrk_sales, 2)
          tot_mod1% = tot_mod1% + wrk_qty%
          tot_stk1% = tot_stk1% + wrk_stock%
          tot_sales1 = round(tot_sales1 + wrk_sales, 2)
          tot_mod2% = tot_mod2% + wrk_qty%
          tot_stk2% = tot_stk2% + wrk_stock%
          tot_sales2 = round(tot_sales2 + wrk_sales, 2)
        return

        print_detail_m
          if sav_model$ = model$ then goto L62210
             goto print_detail_end

L62210:   if str(sav_key$,1%,3%) = sav_model$ and                        ~
             str(sav_key$,4%,9%) = sav_cuscode$ then goto L62430

        print_detail_end
             if lcnt% > 57% then gosub print_header
             convert tot_mod%  to tot_mod$,   pic(######)
             convert tot_stk%  to tot_stk$,   pic(######)
             convert tot_sales to tot_sales$, pic(##,###,###.##-)

             print using L55570
             print using L55610, sav_model$, mod_desc$, sav_cuscode$,     ~
                                cust_name$, tot_stk$, tot_mod$,          ~
                                tot_sales$
             lcnt% = lcnt% + 2%
             if sav_model$ <> model$ then gosub print_mod_total
             gosub lookup_customer
             tot_mod%  = 0%
             tot_stk%  = 0%
             tot_sales = 0.0
             sav_cuscode$ = cuscode$
             if finish% = 1% then return

L62430:   tot_mod%   = tot_mod% + wrk_qty%
          tot_stk%   = tot_stk% + wrk_stock%
          tot_sales  = round(tot_sales + wrk_sales, 2)
          tot_mod1%  = tot_mod1% + wrk_qty%
          tot_stk1%  = tot_stk1% + wrk_stock%
          tot_sales1 = round(tot_sales1 + wrk_sales, 2)
        return

        print_mod_total
            pcnt = 0.0
            x = tot_stk1% : y = tot_mod1%
            if y = 0 then goto L62560
            pcnt = round ( x / y * 100, 2 )
L62560:     convert pcnt to pcnt$, pic(###.##-)

            convert tot_mod1%  to tot_mod1$,   pic(######)
            convert tot_stk1%  to tot_stk1$,   pic(######)
            convert tot_sales1 to tot_sales1$, pic(##,###,###.##-)
            print using L55570
            print using L55650, pcnt$, sav_model$, tot_stk1$,             ~
                                                   tot_mod1$, tot_sales1$
            lcnt% = lcnt% + 2%
            tot_mod1% = 0%
            tot_stk1% = 0%
            tot_sales1 = 0.0
            gosub lookup_model
            sav_model$ = model$
        return

        print_model_total
            pcnt = 0.0
            x = tot_stk% : y = tot_mod%
            if y = 0 then goto L62770
            pcnt = round ( x / y * 100, 2 )
L62770:     convert pcnt to pcnt$, pic(###.##-)

            convert tot_mod%  to tot_mod$,   pic(######)
            convert tot_stk%  to tot_stk$,   pic(######)
            convert tot_sales to tot_sales$, pic(##,###,###.##-)
            print using L55250
            print using L55400, pcnt$, sav_model$, tot_stk$, tot_mod$,    ~
                                                               tot_sales$
            lcnt% = lcnt% + 2%
            tot_sales = 0.0
            tot_mod%, tot_stk% = 0%
            sav_model$ = model$
        return

        print_sales_total
            pcnt = 0.0
            x = tot_stk1% : y = tot_mod1%
            if y = 0 then goto L62960
            pcnt = round ( x / y * 100, 2 )
L62960:     convert pcnt to pcnt$, pic(###.##-)

            convert tot_mod1%  to tot_mod1$,   pic(######)
            convert tot_stk1%  to tot_stk1$,   pic(######)
            convert tot_sales1 to tot_sales1$, pic(##,###,###.##-)
            print using L55250
            print using L55440, pcnt$, sav_cuscode$, tot_stk1$,           ~
                                                   tot_mod1$, tot_sales1$
            lcnt% = lcnt% + 2%
            tot_sales1 = 0.0
            tot_mod1%  = 0%
            tot_stk1%  = 0%
            sav_model$   = model$
            sav_cuscode$ = cuscode$
            gosub lookup_customer
        return

        print_route_total
            pcnt = 0.0
            x = tot_stk2% : y = tot_mod2%
            if y = 0 then goto L63180
            pcnt = round ( x / y * 100, 2 )
L63180:     convert pcnt to pcnt$, pic(###.##-)

            convert tot_mod2%  to tot_mod2$,   pic(######)
            convert tot_stk2%  to tot_stk2$,   pic(######)
            convert tot_sales2 to tot_sales2$, pic(##,###,###.##-)
            print using L55250
            print using L55480, pcnt$, sav_route$  , tot_stk2$,           ~
                                                   tot_mod2$, tot_sales2$
            lcnt% = lcnt% + 2%
            tot_sales2 = 0.0
            tot_mod2%  = 0%
            tot_stk2%  = 0%
            sav_route$ = grp_route$
        return

        select_data
            call "SHOSTAT" ("Scanning Sales Orders")

             call "OPENCHCK" (#10, fs%(10), f2%(10),5000%, rslt$(10))
             count% = 0%
             bck_key$ = " " : ord_disc = 0.0
             if cus1$ <> "ALL" then str(bck_key$,1%,9%) = cus1$
             read #2,key > bck_key$, using L63500, cuscode$, so$,         ~
                           ord_dte$, ord_disc,  eod goto scan_orders_done
             goto L63510
        scan_orders_next
             if mod(count%,500) <> 0 then goto L63480
                convert count% to count$, pic(######)
                call "SHOSTAT" ("Sales Orders Scanned ("&count$&")")

L63480:      read #2, using L63500, cuscode$, so$, ord_dte$,              ~
                                      ord_disc, eod goto scan_orders_done
L63500:         FMT CH(9), CH(16), POS(806), CH(6), POS(859), PD(14,4)
L63510:      count% = count% + 1%

L63520:      grp_route$ = "00000"
             if cus1$ = "ALL" then goto L63560
                if cuscode$ > str(cus2$,1%,9%) then                      ~
                                               goto scan_orders_done
L63560:      if dte1$ = "ALL" then goto L63600
                if ord_dte$ < str(beg_dte$,1%,6%) or                     ~
                   ord_dte$ > str(end_dte$,1%,6%) then                   ~
                                              goto scan_orders_next
L63600:      if type$ <> "R" then goto L63660
                gosub lookup_route
                if route1$ = "ALL" then goto L63660
                   if grp_route$ < route1$ or grp_route$ > route2$ then  ~
                                              goto scan_orders_next

L63660:      ln_key$ = all(hex(00))
             str(ln_key$,1%,16%) = so$
             read #3,key > ln_key$, using L63750, ln_rec$, ord_qty,       ~
                           ord_price, ln_disc, eod goto scan_orders_next
             goto L63760
        next_line_item
             read #3, using L63750, ln_rec$, ord_qty, ord_price, ln_disc, ~
                                                eod goto scan_orders_next

L63750:        FMT CH(56), POS(93), PD(14,4), POS(165), 2*PD(14,4)
L63760:      if so$ <> str(ln_rec$,10%,16%) or                           ~
                cuscode$ <> str(ln_rec$,1%,9%) then                      ~
                                                goto scan_orders_next
             part$  = str(ln_rec$,32%,25%)
             model$ = str(part$,1%,3%)
             if mod1$ = "ALL" then goto L63840
                if model$ < mod1$ or model$ > mod2$ then                 ~
                                                      goto next_line_item
L63840:        ord_sales = round( ord_price * ord_qty, 2)
                                                    /* LINE DISCOUNT */
               discamt = round(ord_sales * ln_disc * .01, 2)
               ord_sales = round(ord_sales - discamt, 2)
                                                    /* ORDER DISCOUNT*/
               discamt = round(ord_sales * ord_disc * .01, 2)
               ord_sales = round(ord_sales - discamt, 2)
               ord_qty% = int(ord_qty)
               gosub lookup_stock
               gosub update_work
               goto next_line_item
        scan_orders_done
        return

        update_work
            wrk_key$ = all(hex(00))
            str(wrk_key$,1%,5%)   = grp_route$         /* ROUTE CODE   */
            str(wrk_key$,6%,9%)   = cuscode$           /* CUSTOMER     */
            str(wrk_key$,15%,25%) = part$              /* PART NUMBER  */
        REM    UPDATE_REC
            read #10,hold,key = wrk_key$, using L64060, wrk_qty%,         ~
                              wrk_stock%, wrk_sales, eod goto create_rec
L64060:       FMT POS(41), 2*BI(2), PD(14,4)
                wrk_qty%   = wrk_qty% + ord_qty%
                wrk_stock% = wrk_stock% + ord_stock%
                wrk_sales  = round(wrk_sales + ord_sales, 2)
                put #10, using L64060, wrk_qty% , wrk_stock%, wrk_sales
            rewrite #10
            return
        create_rec
            write #10, using L64160, wrk_key$, ord_qty%, ord_stock%,      ~
                                              ord_sales, model$, cuscode$
L64160:       FMT CH(40), 2*BI(2), PD(14,4), CH(3), CH(9)
        return

        lookup_stock
            ord_stock% = 0%
            stock_key$ = all(hex(00))
            str(stock_key$,1%,25%) = part$
            read #6,key 1% > stock_key$, using L64250, stock_key$,        ~
                                                           eod goto L64280
L64250:        FMT POS(8), CH(32)
            if part$ <> str(stock_key$,1%,25%) then goto L64280
               ord_stock% = ord_qty%
L64280: return

        lookup_route
            read #1,key = cuscode$,using L64320, grp_route$,eod goto L64340
L64320:        FMT POS(980), CH(5)
            if len(grp_route$) = 5 and grp_route$ <> " " then goto L64350
L64340:        grp_route$ = "99999"
L64350: return

        open_files
            close #1
            close #4

            call "APCPAUSE" (apc%, "APCRPT11")
            if apc% <> 0% then goto exit_program

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),  0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04),  0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05),  0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06),  0%, rslt$(06))
            call "OPENCHCK" (#08, fs%(08), f2%(08),  0%, rslt$(08))
            call "OPENCHCK" (#10, fs%(10), f2%(10),  0%, rslt$(10))

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5) = 0%

            if fs%(10) <> 1 then goto L64570
               call "FILEBGON" addr(#10)
L64570: return
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
