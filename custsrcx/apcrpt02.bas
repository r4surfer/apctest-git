        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   RRRR   PPPP   TTTTT   000    222    *~
            *  A   A  P   P  C   C  R   R  P   P    T    0   0  2   2   *~
            *  AAAAA  PPPP   C      RRRRR  PPPP     T    0   0     2    *~
            *  A   A  P      C   C  R  R   P        T    0   0    2     *~
            *  A   A  P       CCC   R   R  P        T     000   22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCRPT02 - Special Report of Product analysis by Route    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/14/91 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 06/10/92 ! MOD ADD GRAND TOTALS TO END OF REPORT    ! RHH *~
            * 04/14/93 ! MOD ADD (312) AND (870) MODELS A AND B   ! RHH *~
            * 01/19/97 ! MODS FOR NEW PLANNING SYSTEM             !     *~
            *          !                                          !     *~
            * 11/12/97 ! Revision Update For 60403                ! DJD *~
            *************************************************************

        dim                                                              ~
            grp_key$51,                  /* Primary Key                */~
            grp_cuscode$9,               /* Customer Number            */~
            grp_due$6,                   /* Calculated Due Date        */~
            grp_route$5,                 /* Group Route Code           */~
            grp_so$8,                    /* Group Sales Order Number   */~
            grp_po$16,                   /* Group P.O. Number          */~
            grp_rec$170,                 /* Group Record               */~
            grp_status$2,                /* CURRENT STATUS             */~
            model$3,                     /* Model Code                 */~
            model_desc$30,               /* Model Code Description     */~
            b_dte$10,                    /* Formatted Due Date         */~
            b_dte1$10,                   /* Beginning Date             */~
            e_dte$10,                    /* Formatted Due Date         */~
            e_dte1$10,                   /* Ending Date                */~
            chk_dte$10,                  /* RECORD DUE DATE            */~
            readkey$24,                  /* Gencodes Key               */~
            total_qty$10,                /* TOTAL QTY FOR ROUTE        */~
            total_units$10,              /* TOTAL UNITS FOR ROUTE      */~
            total_price$10,              /* TOTAL PRICE FOR ROUTE      */~
            qty(15),                     /* Total Qty For Product Line */~
            units(15),                   /* Total Units for Product Ln */~
            price(15),                   /* Totl Price for Product Ln  */~
            t_qty(15),                   /* Tot Qty for Product Ln Run */~
            t_units(15),                 /* Tot Unt for Product Ln Run */~
            t_price(15),                 /* Tot Prc for Product Ln Run */~
            qty$(15)10,                  /*                            */~
            units$(15)10,                /*                            */~
            price$(15)10,                /*                            */~
            dept$30,                     /* Department Code Description*/~
            p$(10)1,                     /* Product Line Designator    */~
            p_line$(15)25,               /* Product Line Description   */~
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

        dim                              /* File = (BCKLINES)          */~
            bck_key$25,                  /* Primary Key                */~
            bck_ord$16,                  /* Sales Order                */~
            bck_seq$3,                   /* Line Item Seq              */~
            bck_part$25,                 /* Part Number                */~
            bck_desc$32,                 /* Part Number Description    */~
            bck_qty$4,                   /* S.O. Quantity              */~
            bck_price$8,                 /* S.O. Price                 */~
            hdr_price$8                  /* Total Line Price           */

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
            cms2v$ = "06.04.03 11/12/97 Product Group Date Report      "
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
            * #01 ! APCPLNOR ! NEW S.O. HEADER MASTER - APCORDER - OLD  *~
            * #02 ! CUSTOMER ! Customer Master File                     *~
            * #03 ! BCKMASTR ! S.O. Master File                         *~
            * #04 ! GENCODES ! SYSTEM MASTER CODE TABLE FILES           *~
            * #05 ! BCKLINES ! S.O. Line Item File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01,  "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup


            select #02, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #03, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #04, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #5,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),100%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),  0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04),  0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05),  0%, rslt$(05))

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5) = 0%

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

            p$(1) = "0"  : p$(6) = "5"
            p$(2) = "1"  : p$(7) = "6"
            p$(3) = "2"  : p$(8) = "7"
            p$(4) = "3"  : p$(9) = "8"
            p$(5) = "4"  : p$(10)= "9"

            for i% = 1% to 10%
                p_line$(i%) = " "
                gosub lookup_dept
            next i%

            p_line$(11%) = "Vinyl Patio Door    (312)"
            p_line$(12%) = "Vinyl Hopper Window (870)"

            mat t_qty   = zer
            mat t_units = zer
            mat t_price = zer

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   1%
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
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
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
            on fieldnr% gosub L20130          /* Due Date              */

         return

L20130: REM Due Date                               GRP_DUE$
        REM GRP_DUE$ = DATE
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
         "Enter a Valid Due Date Associated with Sales Orders?         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, grp_rec$, grp_key$,        ~
                      grp_due$, grp_route$, grp_so$, grp_po$, model$,    ~
                      model_desc$, grp_cuscode$, bck_key$, bck_ord$,     ~
                      bck_seq$, bck_part$, bck_desc$, bck_qty$,          ~
                      bck_price$, hdr_price$, b_dte$, b_dte1$, e_dte$,   ~
                      e_dte1$, chk_dte$
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

        REM

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
              on fieldnr% gosub L40150          /* Order Due Date    */

              goto L40190

L40150:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02),                                               ~
                  "(APCRPT02) - Forecast Analysis of Route's   ",        ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beginning Due Date  :",                      ~
               at (06,25), fac(lfac$( 1)), b_dte$               , ch(10),~
               at (07,02), "Ending Due Date     :",                      ~
               at (07,25), fac(lfac$( 1)), e_dte$               , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50120          /* Group Due Date        */

            return

L50120: REM Group Due Date                        B_DTE$ E_DTE$
            b_dte1$, e_dte1$ = " " : date% = 0%
            if b_dte$ <> " " then goto L50170
               goto L50230
L50170:     call "DATEOKC" (b_dte$, date%, errormsg$ )
            if errormsg$ <> " " then return
               b_dte1$ = b_dte$
               call "DATUFMTC" (b_dte1$)
               grp_due$ = str(b_dte1$,1%,6%)
               goto L50241
L50230:     errormsg$ = "Must Enter a Valid Beginning Group Due Date"
         return
L50241:     if e_dte$ <> " " then goto L50243
               e_dte$ = b_dte$
L50243:     call "DATEOKC" (e_dte$, date%, errormsg$ )
            if errormsg$ <> " " then return
               e_dte1$ = e_dte$
               call "DATUFMTC" (e_dte1$)
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L50310: %########## ########   ########################################  ~
        ~      APCRPT02:

L50340: %USER ID: ###        ########################################    ~
        ~    PAGE: #####
                                                   /* COLUMN 1 HEADER */
L50370: % ROUTE: #####       BEG DATE: ##########  END DATE: ##########

L50390: %  <---- PRODUCT LINES ---->  QUANTITY        UNITS         PRICE
L50400: %  -------------------------  --------     --------     ---------
L50410: %0 #########################     #####     #####.##     ######.##
L50420: %1 #########################     #####     #####.##     ######.##
L50430: %2 #########################     #####     #####.##     ######.##
L50440: %3 #########################     #####     #####.##     ######.##
L50450: %4 #########################     #####     #####.##     ######.##
L50460: %5 #########################     #####     #####.##     ######.##
L50470: %6 #########################     #####     #####.##     ######.##
L50480: %7 #########################     #####     #####.##     ######.##
L50490: %8 #########################     #####     #####.##     ######.##
L50500: %9 #########################     #####     #####.##     ######.##
L50502: %A #########################     #####     #####.##     ######.##
L50505: %B #########################     #####     #####.##     ######.##
L50510: %                             --------     --------     ---------
L50520: % TOTALS                         #####     #####.##     ######.##

        print_header
          page_no% = page_no% + 1%
          print page
          print using L50310, date$, rpt_time$, company$
          print using L50340, userid$, print_title$, page_no%
          print
          lcnt% = 3%
        return

        print_detail
          if sav_route$ <> " " then goto L50670
             goto L51120

L50670:   total_qty, total_units, total_price = 0.0
          for i% = 1% to 12%
              convert qty(i%) to qty$(i%), pic(#####)

              convert units(i%) to units$(i%), pic(#####.##)

              convert price(i%) to price$(i%), pic(######.##)

              total_qty   = round(total_qty   + qty(i%),   2)
              total_units = round(total_units + units(i%), 2)
              total_price = round(total_price + price(i%), 2)
          next i%
              convert total_qty to total_qty$, pic(#####)

              convert total_units to total_units$, pic(#####.##)

              convert total_price to total_price$, pic(######.##)

          gosub print_header
          print using L50370, sav_route$, b_dte$, e_dte$
          print
          print using L50390
          print using L50400
          print using L50410,p_line$(1%), qty$(1%), units$(1%), price$(1%)
          print
          print using L50420,p_line$(2%), qty$(2%), units$(2%), price$(2%)
          print
          print using L50430,p_line$(3%), qty$(3%), units$(3%), price$(3%)
          print
          print using L50440,p_line$(4%), qty$(4%), units$(4%), price$(4%)
          print
          print using L50450,p_line$(5%), qty$(5%), units$(5%), price$(5%)
          print
          print using L50460,p_line$(6%), qty$(6%), units$(6%), price$(6%)
          print
          print using L50470,p_line$(7%), qty$(7%), units$(7%), price$(7%)
          print
          print using L50480,p_line$(8%), qty$(8%), units$(8%), price$(8%)
          print
          print using L50490,p_line$(9%), qty$(9%), units$(9%), price$(9%)
          print
         print using L50500,p_line$(10%),qty$(10%),units$(10%),price$(10%)
          print
         print using L50502,p_line$(11%),qty$(11%),units$(11%),price$(11%)
          print
         print using L50505,p_line$(12%),qty$(12%),units$(12%),price$(12%)
          print using L50510
          print using L50520, total_qty$, total_units$, total_price$

L51120:   mat qty   = zer
          mat units = zer
          mat price = zer
          total_qty, total_units, total_price = 0.0
          sav_route$ = grp_route$
          init(" ") qty$(), units$(), price$(), total_qty$, total_units$,~
                            total_price$
          return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%

            print_title$ = "Forecast Analysis For Route's"
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
            gosub select_printer
            sav_route$ = " "
            grp_key$ = all(hex(00))
            str(grp_key$,1%,6%) = grp_due$
            read #1,key > grp_key$, using L60330, chk_dte$, grp_status$,  ~
                                                 eod goto generate_done
            goto L60340
        generate_next
            read #1, using L60330, chk_dte$, grp_status$,                 ~
                                                 eod goto generate_done
L60330:        FMT CH(6), POS(60), CH(2)
L60340:     if chk_dte$ < str(b_dte1$,1%,6%) or                          ~
               chk_dte$ > str(e_dte1$,1%,6%) then goto generate_done
            if grp_status$ > "01" then goto generate_next
               get #1, using L60380, grp_route$, grp_cuscode$, grp_so$
L60380:          FMT POS(11), CH(5), POS(27), CH(9), POS(52), CH(8)

            if str(sav_route$,1%,2%) <> str(grp_route$,1%,2%) then       ~
                                                       gosub print_detail
             bck_key$ = " " : ord_disc = 0.0
             str(bck_key$,1%,9%)  = grp_cuscode$
             str(bck_key$,10%,8%) = grp_so$
             read #3,key = bck_key$, using L60460,ord_disc, eod goto L60470
L60460:         FMT POS(859), PD(14,4)
L60470:
             bck_key$ = all(hex(00))
             str(bck_key$,1%,16%) = grp_so$
             order% = 0%
             read #5,key > bck_key$, using L60600, bck_ord$, bck_part$,   ~
                                     bck_desc$, bck_qty, bck_price,      ~
                                              bck_ln_disc, eod goto L60730
             goto L60620
        next_line_item
             order% = 0%
             read #5, using L60600, bck_ord$, bck_part$, bck_desc$,       ~
                                   bck_qty, bck_price, bck_ln_disc,      ~
                                                           eod goto L60730
L60600:        FMT POS(10), CH(16), POS(32), CH(25), CH(32), POS(93),    ~
                   PD(14,4), POS(165), 2*PD(14,4)
L60620:      if str(bck_ord$,1%,8%) <> grp_so$ then goto L60730
                hdr_price = round( bck_price * bck_qty, 2)
                                                    /* LINE DISCOUNT */
                discamt = round(hdr_price * bck_ln_disc * .01, 2)
                hdr_price = round(hdr_price - discamt, 2)
                                                    /* ORDER DISCOUNT*/
                discamt = round(hdr_price * ord_disc * .01, 2)
                hdr_price = round(hdr_price - discamt, 2)
                convert bck_qty to bck_qty$, pic(0000)
                order% = 1%

L60730:     if order% = 0% then goto generate_next
               gosub update_products
               goto next_line_item
        generate_done
            gosub print_detail
            gosub print_grand_tot
            gosub close_printer
        return

        print_grand_tot
            mat qty   = zer
            mat units = zer
            mat price = zer
            for i% = 1% to 12%
                qty(i%)   = t_qty(i%)
                units(i%) = t_units(i%)
                price(i%) = t_price(i%)
            next i%
            sav_route$ = "TOTAL"
            print_title$ = "Forcast Analysis - Grand Totals"
            gosub print_detail
        return

        update_products
            convert str(bck_part$,1%,1%) to p%, data goto L60980
L60980:
            if str(bck_part$,1%,3%) = "312" then p% = 10%
            if str(bck_part$,1%,3%) = "870" then p% = 11%

            qty(p%+1%)   = round(qty(p%+1%) + bck_qty, 2)
            price(p%+1%) = round(price(p%+1%) + hdr_price, 2)
            gosub calculate_units
            units(p%+1%) = round(units(p%+1%) + unit, 2)
            t_qty(p%+1%)   = round(t_qty(p%+1%) + bck_qty, 2)
            t_price(p%+1%) = round(t_price(p%+1%) + hdr_price, 2)
            t_units(p%+1%) = round(t_units(p%+1%) + unit, 2)
        return

        calculate_units                  /* ONLY MANUFACTURED PARTS */
            unit = 0.0
            if len(bck_part$) < 19% then return
            model$ = str(bck_part$,1%,3%)
            readkey$ = all(hex(00))
            readkey$ = "SYS CODES" & model$
            call "DESCRIBE" (#4, readkey$, model_desc$, 0%, f1%(4%))
            if f1%(4%) = 0% then return
            if str(model_desc$,21%,1%) <> " " then goto L61190
               str(model_desc$,21%,5%) = "00.00"
                                         /* Convert Width  */
L61190:     convert str(bck_part$,13%,4%) to a1%,data goto L61200
L61200:                                  /* Convert Height */
            convert str(bck_part$,17%,3%) to a2%,data goto L61220
L61220:                                  /* Always Round Up*/
            if str(bck_part$,16%,1%) <> "0" then a1% = a1% + 10%
            if str(bck_part$,19%,1%) <> "0" then a2% = a2% + 10%
                                    /* UNIT_I% = UNITED INCHES       */
            unit_i% = int(a1%/10) + int(a2%/10)
            convert str(model_desc$,21%,5%) to fact, data goto L61280
L61280:
            fact = round(fact,2)
            x    = round( ((fact * unit_i%)/116.0),2)
            unit = round( x * bck_qty, 2)
        return

        lookup_dept                           /* Look Up Department    */
            dept$ = " "
            readkey$ = all(hex(00))
            readkey$ = "DEPTCODE " & p$(i%)
            call "DESCRIBE" (#4, readkey$, dept$, 0%, f1%(4%) )
            if f1%(4%) = 0% then return
            p_line$(i%) = str(dept$,1%,25%)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
