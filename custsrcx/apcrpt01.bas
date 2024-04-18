        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   RRRR   PPPP   TTTTT   000     1     *~
            *  A   A  P   P  C   C  R   R  P   P    T    0   0   11     *~
            *  AAAAA  PPPP   C      RRRRR  PPPP     T    0   0    1     *~
            *  A   A  P      C   C  R  R   P        T    0   0    1     *~
            *  A   A  P       CCC   R   R  P        T     000   11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCRPT01 - Special Reports from Group Header Information  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/14/91 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/24/97 ! Mods to run with New Planning System.    ! RHH *~
            *          !                                          !     *~
            * 11/12/97 ! Revision Update For 60403                ! DJD *~
            * 03/15/97 ! Clean-Up                                 ! RHH *~
            *************************************************************

        dim                                                              ~
            or_key$51,                   /* Primary Key                */~
            or_cuscode$9,                /* Customer Number            */~
            or_due$8,                    /* Calculated Due Date        */~
            or_route$5,                  /* Group Route Code           */~
            or_so$8,                     /* Group Sales Order Number   */~
            or_po$16,                    /* Group P.O. Number          */~
            or_rec$170,                  /* Group Record               */~
            or_status$2,                 /* STATUS CODE                */~
            bck_rec$256,                 /* PARTIAL S.O. DETAIL REC    */~
            model$3,                     /* Model Code                 */~
            model_desc$30,               /* Model Code Description     */~
            prt_model$3,                 /* Print Model Code           */~
            prt_part$25,                 /* Print Part Number          */~
            prt_desc$30,                 /* Print Part Description     */~
            sav_model$3,                 /* Save Model Code            */~
            sav_part$25,                 /* Save Part Number           */~
            sav_desc$30,                 /* Save Part Description      */~
            b_dte$10,                    /* Formatted Due Date         */~
            b_dte1$10,                   /* Beginning Date             */~
            e_dte$10,                    /* Formatted Due Date         */~
            e_dte1$10,                   /* ENDING    Date             */~
            chk_dte$6,                   /* RECORD TEST GROUP DATE     */~
            chk_status$1, chk_status_d$30,/* For Planned or Not Planned*/~
            readkey$24,                  /* Gencodes Key               */~
            mod$3,                       /* MODEL CODE                 */~
            wrk$5,                       /* UNIQUE IDENTIFIER          */~
            wrk_key$28,                  /* PRIMARY KEY                */~
            total_qty$5,                 /* TOTAL QTY FOR MODEL        */~
            total_price$10,              /* TOTAL PRICE FOR MODEL      */~
            total_q$5,                   /* TOTAL QTY FOR REPORT       */~
            total_p$10,                  /* TOTAL PRICE FOR REPORT     */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
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

        dim                              /* File = (BCKLINES)          */~
            bck_key$25,                  /* Primary Key                */~
            bck_ord$16,                  /* Sales Order                */~
            bck_seq$3,                   /* Line Item Seq              */~
            bck_part$25,                 /* Part Number                */~
            bck_desc$32,                 /* Part Number Description    */~
            bck_qty$4,                   /* S.O. Quantity              */~
            bck_price$8,                 /* S.O. Price                 */~
            bck_textid$4,                /* S.O. Text Id               */~
            hdr_price$8                  /* Total Line Price           */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 03/15/99 EWD SPECIAL REPORT             "
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
            * #1  ! APCPLNOR ! Group S.O. Header File                   *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! BCKMASTR ! S.O. Master File                         *~
            * #4  ! GENCODES ! SYSTEM MASTER CODE TABLE FILES           *~
            * #5  ! BCKLINES ! S.O. Line Item File                      *~
            * #10 ! APCWORK1 ! APC Work File                            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup


            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #3,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #5,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #10, "APCWORK1",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =   5,                     ~
                        alt key  1, keypos =    6, keylen =  28, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))

            mat f1% = zer

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

            for fieldnr% = 1% to   3%
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
         "Enter a Valid Group Due Date Associated with Sales Orders.   ",~
         "Enter a Valid Model or (All).                                ",~
         "Enter a (1) - For Not Planned or (2) - For Planned Sales Order's?"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, or_rec$, or_key$,          ~
                      or_due$, or_route$, or_so$, or_po$, model$,        ~
                      model_desc$, or_cuscode$, bck_key$, bck_ord$,      ~
                      bck_seq$, bck_part$, bck_desc$, bck_qty$,          ~
                      bck_price$, bck_textid$, hdr_price$, chk_status$,  ~
                      chk_status_d$

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
        REM DATALOAD

        REM RETURN

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
                                         /* APCWORK1 - Work File       */
L35040:     FMT CH(05),                  /* Unique Itemtifier          */~
                CH(03),                  /* Model Code                 */~
                CH(25),                  /* Part number                */~
                CH(32),                  /* Part Description           */~
                CH(05),                  /* Route Code                 */~
                CH(09),                  /* Customer Code              */~
                CH(08),                  /* Sales Order Number         */~
                CH(16),                  /* P.O. Number                */~
                CH(04),                  /* Order Quantity             */~
                PD(14,4),                /* Order Price-Total For Line */~
                CH(04),                  /* Text Id                    */~
                CH(09)                   /* Filler                     */

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
              on fieldnr% gosub L40160,         /* Order Due Date    */   ~
                                L40170,         /* MODEL             */   ~
                                L40170          /* Planning Status   */ 

              goto L40200

L40160:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "(APCRPT01) - (New) Forecast Analysis of S.O. Data ",  ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Begin S.O. Due Date :",                      ~
               at (06,25), fac(lfac$(1%)), b_dte$               , ch(10),~
               at (06,40), "End S.O. Due Date   :",                      ~
               at (06,65), fac(lfac$(1%)), e_dte$               , ch(10),~
                                                                         ~
               at (07,02), "Model or (All) :",                           ~
               at (07,25), fac(lfac$(2%)), model$               , ch(03),~
               at (07,40), fac(hex(84)),   model_desc$          , ch(30),~
                                                                         ~
               at (08,02), "Plan Status    :",                           ~
               at (08,25), fac(lfac$(3%)), chk_status$          , ch(01),~
               at (08,40), fac(hex(84)),   chk_status_d$        , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40470
                  call "PRNTSCRN"
                  goto L40200

L40470:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40660     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40620
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40620:     if fieldnr% > 1% then L40640
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40640:     return

L40660: if fieldnr% > 0% then L40750  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40750:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50130,         /* Group Due Date        */ ~
                              L50330,         /* Model or (All)        */ ~
                              L50500    /* 1 = Not Planned,2 = Planned */
            return

L50130: REM Group Due Date                        B_DTE$ E_DTE$
            b_dte1$, e_dte1$ = " " : date% = 0%
            if b_dte$ <> " " then goto L50170
               goto L50230
L50170:     call "DATEOKC" (b_dte$, date%, errormsg$ )
            if errormsg$ <> " " then return
               b_dte1$ = b_dte$
               call "DATUFMTC" (b_dte1$)
               or_due$ = str(b_dte1$,1%,6%)
               goto L50250
L50230:     errormsg$ = "Must Enter a Valid Beginning S.O. Due Date"
         return
L50250:     if e_dte$ <> " " then goto L50270
               e_dte$ = b_dte$
L50270:     call "DATEOKC" (e_dte$, date%, errormsg$ )
            if errormsg$ <> " " then return
               e_dte1$ = e_dte$
               call "DATUFMTC" (e_dte1$)
        return

L50330: REM Model Number                          MODEL$
            model_desc$ = "(All)"
            if model$ <> " " then goto L50380
               goto L50470

L50380:     if model$ = "ALL" then return
            convert model$ to model%, data goto L50470

            convert model% to model$, pic(000)

            readkey$ = all(hex(00))
            readkey$ = "FORECAST " & model$
            call "DESCRIBE" (#4, readkey$, model_desc$, 0%, f1%(4%))
            if f1%(4%) <> 0% then return
L50470:        errormsg$ = "Must Enter a Valid Model Code or (All)"
               model_desc$ = "(Error)"
        return

L50500: REM Check Status                      chk_status$
            init(" ") chk_status_d$
 
            if chk_status$ <> " " then goto L50510
               chk_status$ = "1"

L50510:      if chk_status$ = "1" then                                   ~
                chk_status_d$ = " Only Not Planned Sales Orders"     

             if chk_status$ = "2" then                                   ~
                chk_status_d$ = " All Planned Sales Orders     "

             if len(chk_status_d$) > 5 then return
                chk_status$ = "1"
                goto L50510  
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* Report Header */
L50560: %########## ########                 ############################~
        ~################################                        APCRPT01:

L50590: %User Id: ###                        ############################~
        ~################################                      Page: #####

L50620: %Beg Date: ##########   End Date: ##########
                                                   /* Column 1 Header */
L50640: %Model  <----- Part Number ----->  <------- Description -------->~
        ~  Route  Customer   Sales Ord  P.O. Number         Qty   Price

L50670: %-----  -------------------------  ------------------------------~
        ~  -----  ---------  ---------  ----------------   ----   --------

                                                   /* DETAIL 1      */
L50710: % ###   #########################  ##############################~
        ~  #####  #########   ########  ################   ####   #####.##

L50740: % Totals for Model (###)                                         ~
        ~                                                 #####  ######.##

L50770: % Total for All Models Reported                                  ~
        ~                                                 ##### #######.##

        print_header
          sav_model$, sav_part$, sav_desc$ = " "
          page_no% = page_no% + 1%
          print page
          print using L50560, date$, rpt_time$, company$
          print using L50590, userid$, print_title$, page_no%
          print using L50620, b_dte$, e_dte$
          print
          print using L50640
          print using L50670
          lcnt% = 6%
        return

        print_detail
          if lcnt% > 60% then gosub print_header
          if sav_model$ = model$ then goto L51000
             prt_model$, sav_model$ = model$
             prt_part$, sav_part$   = bck_part$
             prt_desc$ = bck_desc$
             goto L51080
L51000:   if sav_part$ = bck_part$ then goto L51060
             prt_part$ , sav_part$ = bck_part$
             prt_desc$ = bck_desc$
             prt_model$ = " "
             goto L51080

L51060:   prt_model$ = " "
          prt_part$, prt_desc$ = " "
L51080:   total_qty% = total_qty% + bck_qty%
          total_price = round(total_price + hdr_price, 2)
          convert hdr_price to hdr_price$, pic(#####.##)

          print using L50710, prt_model$, prt_part$, prt_desc$,           ~
                             or_route$, or_cuscode$, or_so$, or_po$,     ~
                             bck_qty$, hdr_price$
          lcnt% = lcnt% + 1%
          return

        print_totals
          if page_no% = 0% then return
          if (lcnt% + 3%) > 60% then gosub print_header
          convert total_qty% to total_qty$, pic(#####)

          convert total_price to total_price$, pic(######.##)

          print
          print using L50740, mod$, total_qty$, total_price$
          print
          lcnt% = lcnt% + 3%
          total_q% = total_q% + total_qty%
          total_p  = round(total_p + total_price, 2)
          total_qty% = 0%
          total_price = 0.0
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%

            print_title$ = "Forecast Analysis For Group Date"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCLDS", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCLDS", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Scanning Data")
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 500%, rslt$(10%))
            wrk% = 0%
            or_key$ = all(hex(00))
            str(or_key$,1%,6%) = str(or_due$,1%,6%)
            read #1,key > or_key$, using L60320, or_rec$,                 ~
                                                 eod goto generate_done
            goto L60330
        generate_next
            read #1, using L60320, or_rec$, eod goto generate_done
L60320:        FMT CH(170)
L60330:     chk_dte$    = str(or_rec$,1%,6%)
            or_status$  = str(or_rec$,60%,2%)
            if chk_dte$ > str(e_dte1$,1%,6%) then goto generate_done

            if chk_status$ <> "1" then L60335
                                               /* Only Not Planned */
               if or_status$ > "01" then goto generate_next
                  goto L60340
                                               /* Planned Only     */ 
L60335:        if or_status$ < "02" then goto generate_next
               if or_status$ > "87" then goto generate_next

L60340:     or_route$   = str(or_rec$,11%,5%)
            or_cuscode$ = str(or_rec$,27%,9%)
            or_so$      = str(or_rec$,52%,8%)
            or_po$      = str(or_rec$,36%,16%)

             bck_key$ = " " : ord_disc = 0.0
             str(bck_key$,1%,9%)  = or_cuscode$
             str(bck_key$,10%,8%) = or_so$
             read #3,key = bck_key$, using L60470,ord_disc, eod goto L60480
L60470:         FMT POS(859), PD(14,4)
L60480:
             bck_key$ = all(hex(00))
             str(bck_key$,1%,16%) = or_so$
             order% = 0%
             read #5,key > bck_key$, using L60570, bck_rec$,eod goto L60780
             goto L60590
        next_line_item
             order% = 0%
             read #5, using L60570, bck_rec$, eod goto L60780
L60570:        FMT CH(256)

L60590:      bck_ord$  = str(bck_rec$,10%,16%)
             bck_part$ = str(bck_rec$,32%,25%)
             bck_desc$ = str(bck_rec$,57%,32%)
             get str(bck_rec$,93%,8%) using L60630, bck_qty
L60630:        FMT PD(14,4)
             get str(bck_rec$,165%,16%) using L60650,bck_price,bck_ln_disc
L60650:        FMT 2*PD(14,4)
             bck_textid$ = str(bck_rec$,242%,4%)
             if bck_ord$ <> or_so$ then goto L60780
                hdr_price = round( bck_price * bck_qty, 2)
                                                    /* LINE DISCOUNT */
                discamt = round(hdr_price * bck_ln_disc * .01, 2)
                hdr_price = round(hdr_price - discamt, 2)
                                                    /* ORDER DISCOUNT*/
                discamt = round(hdr_price * ord_disc * .01, 2)
                hdr_price = round(hdr_price - discamt, 2)
                convert bck_qty to bck_qty$, pic(0000)
                order% = 1%

L60780:     if order% = 0% then goto generate_next
               mod$ = str(bck_part$,1%,3%)
            if model$ = "ALL" then goto L60830
               if model$ <> mod$ then goto next_line_item

L60830:     readkey$ = all(hex(00))
            readkey$ = "FORECAST " & mod$
            call "DESCRIBE" (#4, readkey$, model_desc$, 0%, f1%(4%))
            if f1%(4%) = 0% then goto next_line_item
               wrk% = wrk% + 1%
               convert wrk% to wrk$, pic(00000)
               write #10,using L35040, wrk$, mod$, bck_part$, bck_desc$,  ~
                                      or_route$, or_cuscode$, or_so$,    ~
                                      or_po$, bck_qty$, hdr_price,       ~
                                      bck_textid$
               goto next_line_item

        generate_done

            call "SHOSTAT" ("("& wrk$ & ") Line Items Selected")
            total_q% = 0%                   /* TOTAL QTY FOR REPORT   */
            total_p  = 0.0                  /* TOTAL PRICE FOR REPORT */
            gosub select_printer
            mod$ = " "
            wrk_key$ = all(hex(00))
            read #10,key 1% > wrk_key$, using L35040, wrk$, model$,       ~
                                       bck_part$, bck_desc$,             ~
                                       or_route$, or_cuscode$, or_so$,   ~
                                       or_po$, bck_qty$, hdr_price,      ~
                                       bck_textid$, eod goto work_done
            goto L61140
        work_next
            read #10, using L35040,    wrk$, model$, bck_part$, bck_desc$,~
                                      or_route$, or_cuscode$, or_so$,    ~
                                      or_po$, bck_qty$, hdr_price,       ~
                                      bck_textid$, eod goto work_done
L61140:     convert bck_qty$ to bck_qty%, data goto L61150
L61150:
            if mod$ = model$ then goto L61200
               gosub print_totals
               mod$ = model$

L61200:     gosub print_detail
            goto work_next
        work_done
            gosub print_totals
            convert total_q% to total_q$, pic(00000)
            convert total_p to total_p$, pic(###,###.##)
            print
            print using L50770, total_q$, total_p$
            gosub close_printer
            call "FILEBGON" addr(#10)
        return

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
