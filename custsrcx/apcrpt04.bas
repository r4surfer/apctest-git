        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRPT04                             *~
            *  Creation Date     - 02/03/97                             *~
            *  Last Modified Date- 11/12/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - New Utility Program to Create the    *~
            *                      Daily Sales Analysis Report By       *~
            *                      Salesman.                            *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *  Subroutine Used   -                                      *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/03/97 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/12/97 ! Revision Update For 60403                ! DJD *~
            * 03/30/98 ! Y2k modifications                        ! ERN *~
            *************************************************************

        dim                                                              ~
            sa_key1$19,                  /* Alternate Key 2            */~
            or_cuscode$9,                /* Customer Number            */~
            or_st$2,                     /* Order Status (N)ew or (C)  */~
            or_so$8,                     /* Group Sales Order Number   */~
            or_sls$4,                    /* Group Salesman Code        */~
            sa_rec$32,                   /* Sales Analysis Record      */~
            or_sls_name$32,              /* Salesman Name              */~
            sa_dte1$10,                  /* Formatted S.O. Orig Date   */~
            ord_disc$8,                  /* Discount Amount            */~
            ord_tot$10,                  /* Customer Total Dollars     */~
            sls_tot$10,                  /* Salesman Total Dollars     */~
            sls_tot1$10,                 /* Salesman Total Dollars     */~
            rpt_tot$10,                  /* Report Total Dollars       */~
            rpt_tot1$10,                 /* Report Total Dollars       */~
            ord_qty$5,                   /* Customer Total Units       */~
            sls_qty$5,                   /* Salesman Total Units       */~
            sls_qty1$5,                  /* Salesman Total Units       */~
            sls_disc$8,                  /* Discount Amount            */~
            sls_disc1$8,                 /* Discount Amount            */~
            rpt_qty$5,                   /* Report Total Units         */~
            rpt_qty1$5,                  /* Report Total Units         */~
            rpt_disc$8,                  /* Discount Amount            */~
            rpt_disc1$8,                 /* Discount Amount            */~
            readkey$24,                  /* Gencodes Key               */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(25%),                /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            msg$3,                       /* New or Changed             */~
            chk_date$10, chk_dte$6,      /* Today Unformatted          */~
            userid$3                     /* Current User Id            */

        dim                              /* File = (BCKLINES)          */~
            bck_key$19,                  /* Primary Key                */~
            bck_ord$16,                  /* Sales Order                */~
            bck_seq$3,                   /* Line Item Seq              */~
            apc_scr$120,                 /* SCREEN DESCRIPTION         */~
            apc_prt$60,                  /* PRINT DESCRIPTION          */~
            apc_sze$20,                  /* SIZE DESCRIPTION           */~
            bck_part$25                  /* Part Number                */

        dim                              /* File = (BCKLINES)          */~
            ord_key$25,                  /* Primary Key                */~
            ord_dte$6                    /* Orig Date Entered          */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        dim                              /* REPORT VARIABLES           */~
            descript$32,                 /* GENCODES DESCRIPTION       */~
            sav_sls$4,                   /* Save Salesman Code         */~
            sav_cust$9,                  /* Save Customer Code         */~
            sav_so$8,                    /* Save Sales Order Number    */~
            prt_shipto$9,                /* Ship to Code               */~
            prt_shipto_name$30,          /* Ship to Name               */~
            prt_so$8,                    /* Sales Order Number         */~
            prt_prod$3,                  /* Product Code               */~
            prt_color$5,                 /* Product Color              */~
            prt_desc$32,                 /* Product Description        */~
            prt_qty$5,                   /* Product Quantity           */~
            prt_pc$10,                   /* Product Unit Price         */~
            prt_pc_tot$10                /* Product Total Price        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/12/97 Daily Sales Register           "
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
            * #1  ! APCPLNOR ! New S.O. Header Master (APCORDER) Old    *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! BCKMASTR ! S.O. Master File                         *~
            * #4  ! GENCODES ! System Master Code Tables                *~
            * #5  ! BCKLINES ! S.O. Line Item File                      *~
            * #6  ! SLMMASTR ! Salesman Master File                     *~
            * #7  ! APCPLNSA ! Daily Sales Analysis Primary File        *~
            * #10 ! AMTBOMIF ! Part Number Validity File                *~
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

            select #6,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #7,  "APCPLNSA",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =   11, keylen =  17,                     ~
                        alt key  1, keypos =    1, keylen =  19, dup

            select #10, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))

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
            chk_date$ = date
            
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
         "Enter a Valid DATE For the Sales Analysis Report.             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sa_date$, sa_dte1$,        ~
                      sa_rec$
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
        dataload
            get #7, using L30070, sa_rec$
L30070:       FMT CH(32)
            chk_dte$     = str(sa_rec$,1%,6%)    /* Date of Last Change*/
            or_sls$      = str(sa_rec$,7%,4%)    /* Salesman Number    */
            or_cuscode$  = str(sa_rec$,11%,9%)
            or_so$       = str(sa_rec$,20%,8%)
            or_st$       = str(sa_rec$,28%,2%)   /* 00=New, 01=Changed */
            bck_key$ = all(hex(00))
            str(bck_key$,1%,16%) = or_so$
            gosub load_header
            msg$ = "NEW"
            if chk_dte$ <> ord_dte$ then msg$ = "CHG"
            gosub get_salesman
        return

        load_header
            ord_dte$ = chk_date$         /* Load Original Order Date */
            ord_disc = 0.0               /* Load Order Level Disc %  */
            ord_key$ = " "
            str(ord_key$,1%,9%)  = or_cuscode$
            str(ord_key$,10%,8%) = or_so$
            read #3,key = ord_key$, using L30280, ord_dte$, ord_disc,     ~
                                                           eod goto L30290
L30280:        FMT POS(830), CH(6), POS(859), PD(14,4)
L30290: return

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
              on fieldnr% gosub L40160          /* S.O. DATE         */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02),                                               ~
                "New Version (APCRPT04) - Daily Sales Analysis Report ", ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sales Analysis Report Date:",                ~
               at (06,30), fac(lfac$(1%)), sa_dte1$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
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
            on fieldnr% gosub L50120          /* S.O. Date             */

            return

L50120: REM Sales Analysis Date                 SA_DATE$, SA_DTE1$
            sa_date$ = " " : date% = 0%
            if sa_dte1$ <> " " then goto L50160
               goto L50220
L50160:     call "DATEOKC" (sa_dte1$, date%, errormsg$ )
            if errormsg$ <> " " then return
               x$ = sa_dte1$
               call "DATUFMTC" (x$)
               sa_date$ = str(x$,1%,6%)
               goto L50230
L50220:     errormsg$ = "Must Enter a Valid Sales Analysis Date."
L50230:  return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L50300: %+---------------------------------------------------------------~
        ~---------------------------------------------------------------+
                                                   /* Column 1 Header */
L50330: %!Report Date:  ##########                   ####################~
        ~####################                               Page: ##### !
L50350: %!Salesman   :      ####  ##############################         ~
        ~                                                               !
L50370: %!---------------------------------------------------------------~
        ~---------------------------------------------------------------!

L50400: %! Ship To !<------- Ship to  Name ------>!Sls  Ord!Prod!Color!<-~
        ~-------  Description -------->! Qty   !Unit Price! Total Price !

L50430: %!---------!------------------------------!--------!----!-----!--~
        ~------------------------------! ----- !----------! ----------- !

L50460: %!#########!##############################!########! ###!#####!##~
        ~##############################! ##### !##########!  ########## !

L50490: %!                                                            ! #~
        ~## Total for S.O.  (########) ! ##### !D ########!  ########## !
L50510: %!                                                            ! N~
        ~ew-Total for Salesman  (####) ! ##### !D ########!  ########## !

L50523: %!                                                            ! C~
        ~hg-Total for Salesman  (####) ! ##### !D ########!  ########## !

L50530: %!                                                            ! N~
        ~ew-Total Sales for (########) ! ##### !D ########!  ########## !

L50552: %!                                                            ! C~
        ~hg-Total Sales for (########) ! ##### !D ########!  ########## !

        print_header
          page_no% = page_no% + 1%
          print page
          print using L50300
          print using L50330, sa_dte1$, print_title$, page_no%
          print using L50350, or_sls$, or_sls_name$
          print using L50370
          print using L50400
          print using L50430
          lcnt% = 6%
        return

        print_detail
          if lcnt% > 60% then gosub print_header
          if lcnt% = 6% then sel% = 1%

          on sel% goto L50750, L50800, L50850

L50750:   print using L50460, prt_shipto$, prt_shipto_name$, prt_so$,     ~
                             prt_prod$, prt_color$, prt_desc$, prt_qty$, ~
                             prt_pc$, prt_pc_tot$
             goto L50880

L50800:   print using L50460, " "        , " "             , prt_so$,     ~
                             prt_prod$, prt_color$, prt_desc$, prt_qty$, ~
                             prt_pc$, prt_pc_tot$
             goto L50880

L50850:   print using L50460, " "        , " "             , " "    ,     ~
                             prt_prod$, prt_color$, prt_desc$, prt_qty$, ~
                             prt_pc$, prt_pc_tot$
L50880:   lcnt% = lcnt% + 1%
        return

        print_order_total
                                               /* Calc Order Discount */
          discamt = round(ord_tot * ord_disc * .01, 2)
          ord_tot = round(ord_tot - discamt, 2)
          convert discamt to ord_disc$, pic(#####.##)

          convert ord_qty to ord_qty$, pic(#####)

          convert ord_tot to ord_tot$, pic(###,###.##)

          print using L50430
          print using L50490, msg$, or_so$, ord_qty$, ord_disc$, ord_tot$
          print using L50430
          lcnt% = lcnt% + 3%
                                         /* Update Totals */
          if msg$ <> "NEW" then goto L51140
             sls_disc= round(sls_disc + discamt, 2)
             rpt_disc= round(rpt_disc + discamt, 2)
             sls_tot = round(sls_tot + ord_tot, 2) /* New Order Totals */
             rpt_tot = round(rpt_tot + ord_tot, 2)
             sls_qty = round(sls_qty + ord_qty, 2) /* New Order Qty    */
             rpt_qty = round(rpt_qty + ord_qty, 2)
             goto L51210
L51140:   sls_disc1 = round(sls_disc1 + discamt, 2)
          rpt_disc1 = round(rpt_disc1 + discamt, 2)
          sls_tot1 = round(sls_tot1 + ord_tot, 2)  /* Chg Order Totals */
          rpt_tot1 = round(rpt_tot1 + ord_tot, 2)
          sls_qty1 = round(sls_qty1 + ord_qty, 2)  /* Chg Order Qty    */
          rpt_qty1 = round(rpt_qty1 + ord_qty, 2)

L51210:   discamt, ord_disc, ord_qty, ord_tot = 0.0
        return

        print_sls_total

          convert sls_disc to sls_disc$, pic(#####.##)   /* New Orders */

          convert sls_qty to sls_qty$, pic(#####)

          convert sls_tot to sls_tot$, pic(###,###.##)

          convert sls_disc1 to sls_disc1$, pic(#####.##) /* Chg Orders */

          convert sls_qty1 to sls_qty1$, pic(#####)

          convert sls_tot1 to sls_tot1$, pic(###,###.##)

          print using L50510, sav_sls$, sls_qty$, sls_disc$, sls_tot$
          print using L50430
          print using L50523, sav_sls$, sls_qty1$, sls_disc1$, sls_tot1$
          print using L50300
          sls_disc, sls_qty, sls_tot = 0.0
          sls_disc1, sls_qty1, sls_tot1 = 0.0
        return

        print_rpt_total

          convert rpt_disc to rpt_disc$, pic(#####.##)

          convert rpt_qty to rpt_qty$, pic(#####)

          convert rpt_tot to rpt_tot$, pic(###,###.##)

          convert rpt_disc1 to rpt_disc1$, pic(#####.##)

          convert rpt_qty1 to rpt_qty1$, pic(#####)

          convert rpt_tot1 to rpt_tot1$, pic(###,###.##)

          print using L50530, sa_dte1$, rpt_qty$, rpt_disc$, rpt_tot$
          print using L50370
          print using L50552, sa_dte1$, rpt_qty1$, rpt_disc1$, rpt_tot1$
          print using L50300
          rpt_disc,  rpt_qty, rpt_tot = 0.0
          rpt_disc1, rpt_qty1, rpt_tot1 = 0.0
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$ = "Daily Sales Analysis Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCSLS", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCSLS", " ", 0%, 1%)
        return

        generate_report
            ord_qty, ord_disc, ord_tot = 0.0
            sls_qty, sls_disc, sls_tot = 0.0
            sls_qty1, sls_disc1, sls_tot1 = 0.0
            rpt_qty, rpt_disc, rpt_tot = 0.0
            rpt_qty1, rpt_disc1, rpt_tot1 = 0.0
            init(" ") ord_qty$, ord_disc$, ord_tot$, sls_qty$, sls_disc$,~
                      sls_tot$, sls_qty1$, sls_disc1$, sls_tot1$,        ~
                      rpt_qty$, rpt_disc$, rpt_tot$, rpt_qty1$,          ~
                      rpt_disc1$, rpt_tot1$, sav_cust$, sav_so$

            call "SHOSTAT" ("Creating Sales Analysis Report")
            gosub select_printer
            sa_key1$  = all(hex(00))
            str(sa_key1$,1%,6%) = sa_date$
            read #7,key 1% > sa_key1$, eod goto generate_done
            goto L60370
        generate_next
            read #7, eod goto generate_done
L60370:     gosub dataload
            if sa_date$ <> chk_dte$ then goto generate_done
               if sav_sls$ = or_sls$ then goto next_sales_order
                  if sav_sls$ <> " " then gosub print_sls_total
                     sav_sls$ = or_sls$
                     gosub print_header
        next_sales_order
            gosub sales_order
            if order% = 1% then goto L60500
               gosub print_order_total
               goto generate_next

L60500:     sel% = 3%                    /* Print No Cust and No S.O. */
            if sav_cust$ = or_cuscode$ then goto L60570
               gosub get_shipto
               sav_cust$ = or_cuscode$
               sav_so$   = or_so$
               sel% = 1%                 /* Print Full Line Format    */
               goto L60610
L60570:     if sav_so$ = or_so$ then goto L60610
               sav_so$ = or_so$          /* Print No Cust just S.O.   */
               sel% = 2%

L60610:     gosub print_detail
            goto next_sales_order
        generate_done
            gosub print_sls_total
            gosub print_rpt_total
            gosub close_printer
        return

        get_salesman
            init(" ") or_sls_name$
            call "DESCRIBE" (#6, or_sls$, or_sls_name$, 0%, f1%(6))

        return

        get_shipto
            init(" ") prt_shipto_name$
            prt_shipto$ = or_cuscode$
            read #2, key = prt_shipto$, using L60800, prt_shipto_name$,   ~
                                                          eod goto L60810
L60800:        FMT POS(10), CH(30)
L60810: return

        sales_order
             order%, err% = 0%
             read #5,key > bck_key$, using L60880, bck_ord$, bck_seq$,    ~
                           bck_part$, prt_desc$, unit_qty, unit_price,   ~
                                                 ln_disc, eod goto L61080
L60880:        FMT POS(10), CH(16), CH(03), XX(03), CH(25), CH(32),      ~
                            XX(04), PD(14,4), POS(165), 2*PD(14,4)
             if bck_ord$ <> or_so$ then goto L61080
                call "APCDESCR" (bck_part$, apc_scr$, apc_prt$, apc_sze$,~
                                 #10, err%)
                str(prt_desc$,1%,16%) = str(apc_prt$,1%,16%)
                str(prt_desc$,17%,16%) = str(apc_sze$,1%,16%)

                str(bck_key$,1%,16%) = bck_ord$     /* SET PRIMARY KEY */
                str(bck_key$,17%,3%) = bck_seq$
                total_price =  round(  unit_price * unit_qty, 2)
                                         /* CALCULATE LINE DISCOUNT */
                discamt     =  round( total_price * ln_disc * .01, 2)
                total_price =  round( total_price - discamt, 2)
                convert unit_qty to prt_qty$, pic(#####)
                convert unit_price to prt_pc$, pic(###,###.##)
                convert total_price to prt_pc_tot$, pic(###,###.##)
                prt_so$   = str(bck_ord$,1%,8%)
                prt_prod$ = str(bck_part$,1%,3%)
                readkey$  = "COLOR    " & str(bck_part$,4%,1%)
                call "DESCRIBE" (#4, readkey$, descript$, 0%, f1%(4%))
                prt_color$ = str(descript$,6%,5%)
                ord_tot = round(ord_tot + total_price, 2)
                ord_qty = round(ord_qty + unit_qty, 2)
          order% = 1%
L61080: return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
