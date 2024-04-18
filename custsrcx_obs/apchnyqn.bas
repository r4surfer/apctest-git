        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   H   H  N   N  Y   Y   QQQ   N   N   *~
            *  A   A  P   P  C   C  H   H  NN  N  Y   Y  Q   Q  NN  N   *~
            *  AAAAA  PPPP   C      HHHHH  N N N   YYY   Q   Q  N N N   *~
            *  A   A  P      C   C  H   H  N  NN    Y    Q   Q  N  NN   *~
            *  A   A  P       CCC   H   H  N   N    Y     QQQ   N   N   *~
            *                                                Q          *~
            *-----------------------------------------------------------*~
            * APCHNYQN - Lookup Up On-Hnad and Open Quantity for a      *~
            *            Part.                                          *~
            *                                                           *~
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
            * 04/05/91 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/26/97 ! Update to 60403 Revision                 ! DJD *~
            *************************************************************

        dim                                                              ~
            apc_scr$120,                 /*                            */~
            apc_prt$60,                  /*                            */~
            apc_sze$20,                  /*                            */~
            dte$6,save_date$6,           /* TEST DATES                 */~
            prt_date$8,                  /*                            */~
            part_desc$32,                /*                            */~
            so$(12)8,                    /* 12 S.O. BUCKETS            */~
            dt$(12)6,                    /* 12 DATES                   */~
            beg_mon$3,end_mon$3,         /* BEG AND END MONTH          */~
            beg_ord$8,end_ord$8,         /* BEG AND END ORDER          */~
            beg_prod$3,end_prod$3,       /* BEG AND END PRODUCT LINE   */~
            ord_ord$8,                   /*                            */~
            ord_key$19,                  /*                            */~
            wrk_key$31,                  /*                            */~
            on_hand$12,                  /* ON HAND QUANTITY           */~
            bck_ord$12,                  /* BACK ORDER QTY             */~
            on_ord$12,                   /* ON ORDER QUANTITY          */~
            on_comit$12,                 /* QTY COMMITTED              */~
            in_proc$12,                  /* QUANTITY IN PROCESS        */~
            qty_pend$12,                 /*                            */~
            readkey$50,                  /*                            */~
            net_qty$12,                  /* Net On Hand Quantity       */~
            part_no$25,                  /* Part Number                */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(25),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            chk_date$6,                  /* TODAY UNFORMATTED          */~
            userid$3                     /* Current User Id            */

        dim f2%(10),                     /* = 0 if the file is open    */~
            f1%(10),                     /* = 1 if READ was successful */~
            fs%(10),                     /* = 1 if file open, -1 if it */~
            rslt$(10)4                   /*   doesn't exist, or 0 if   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/26/97 Pre-Release Version            "
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
            * #01 ! HNYQUAN  ! HNYQUAN FILE                             *~
            * #02 ! HNYMASTR ! HNYMASTR FILE                            *~
            * #03 ! BCKLINES ! S.O. DETAIL LINES FILE                   *~
            * #04 ! BCKMASTR ! S.O. MASTER HEADER FILE                  *~
            * #05 ! APCHNYWK ! REPORT WORK FILE                         *~
            * #06 ! AMTBOMIF ! BOM VALIDITY FILE                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select  #2, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            select #03,  "BCKLINES",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =   10, keylen =   19


            select #04,  "BCKMASTR",                                     ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #05,  "APCHNYWK",                                     ~
                        varc,     indexed,  recsize = 64,                ~
                        keypos =    1, keylen =  31

            select #06, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),  0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04),  0%, rslt$(04))
        REM CALL "OPENCHCK" (#05, FS%(05), F2%(05),  0%, RSLT$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06),  0%, rslt$(06))

            f1%(1) = 0%

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
            chk_date$ = date

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
                      if keyhit% = 14% then gosub process_data
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
                  if keyhit%  = 14% then gosub process_data
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

        process_data
            call "APCHNYSB" (so$(), dt$(), #3, #4)
            gosub orders
            gosub generate_report
            call "SHOSTAT" ("Deleting Work File")
        REM CALL "DELETE" (#5, " ", 0%)
            call "FILEBGON" addr(#5)
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Part Number           */,~
                              L20170,         /* Beg/End Month         */,~
                              L20210          /* Beg/End Product       */

         return

L20130: REM Load Number                            PART_NO$
        REM PART_NO$ = " "
         return

L20170: REM BEGINNING AND ENDING MONTH             BEG_MON$, END_MON$
        REM BEG_MON$, END_MON$ = " "
         return

L20210: REM BEGINNING AND ENDING PRODUCT           BEG_PROD$, END_PROD$
        REM BEG_PROD$, END_PROD$ = " "
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
         "Enter a Valid Part Number (Blank for Report), '?' Lookup.    ",~
         "Enter Begining/Ending Month or 'ALL'.                        ",~
         "Enter a Valid Beginning/Ending Product or 'ALL'.             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, part_no$,        ~
                      rpt_time$, company$, print_title$, on_hand$,       ~
                      bck_ord$, on_ord$, on_comit$, in_proc$, qty_pend$, ~
                      net_qty$, so$(), dt$(), ord_ord$, beg_ord$,        ~
                      end_ord$, beg_prod$, end_prod$, wrk_key$,          ~
                      beg_mon$, end_mon$
        return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
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
              on fieldnr% gosub L40170,         /* Load Number         */ ~
                                L40170,         /* Month/Product Range */ ~
                                L40170
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Inventory/Current Quantities Report ",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Valid Part Number :",                        ~
               at (06,25), fac(lfac$( 1)), part_no$             , ch(25),~
                                                                         ~
               at (07,02), "Beginning Month   :",                        ~
               at (07,25), fac(lfac$( 2)), beg_mon$             , ch(03),~
                                                                         ~
               at (07,40), "Ending Month      :",                        ~
               at (07,65), fac(lfac$( 2)), end_mon$             , ch(03),~
                                                                         ~
               at (08,02), "Beginning Product :",                        ~
               at (08,25), fac(lfac$( 3)), beg_prod$            , ch(03),~
                                                                         ~
               at (08,40), "Ending Product    :",                        ~
               at (08,65), fac(lfac$( 3)), end_prod$            , ch(03),~
                                                                         ~
               at (12,02), "Current On-Hand Quantity:",                  ~
               at (12,40), fac(hex(84)),   on_hand$             , ch(12),~
               at (13,02), "Current Open S.O. Quantity :",               ~
               at (13,40), fac(hex(84)),   bck_ord$             , ch(12),~
               at (14,02), "Current On-Order Quantity  :",               ~
               at (14,40), fac(hex(84)),   on_ord$              , ch(12),~
               at (15,02), "Current Committed Quantity :",               ~
               at (15,40), fac(hex(84)),   on_comit$            , ch(12),~
               at (16,02), "Current Quantity In_Process:",               ~
               at (16,40), fac(hex(84)),   in_proc$             , ch(12),~
               at (17,02), "Current Quantity Pending   :",               ~
               at (17,40), fac(hex(84)),   qty_pend$            , ch(12),~
                                                                         ~
               at (19,02), "Net Quantity On - Hand     :",               ~
               at (19,40), fac(hex(94)),   net_qty$             , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40690
                  call "PRNTSCRN"
                  goto L40200

L40690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40840
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40840:     if fieldnr% > 1% then L40860
                str(pf$(1),64%)   = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50140,         /* S.O. Date             */ ~
                              L50290,         /* BEG/END MONTH         */ ~
                              L50570          /* BEG/END PRODUCT       */

            return

L50140: REM PART NUMBER                         PART_NO$
            if part_no$ <> "?" then goto L50190
                part_no$ = " "
                call "GETCODE" (#2, part_no$, " ", 0%, 3, f1%(2))

L50190:     if part_no$ <> " " then goto L50210
        return
L50210:     gosub get_quantity
            if ord% = 0% then goto L50240
        return
L50240:     errormsg$ = "Must Enter a Valid Part Number."
            part_no$ = " "
        return


L50290: REM BEG/END MONTH                       BEG_MON$, END_MON$
            beg_mon% = 1%
            if beg_mon$ <> " " and beg_mon$ <> "ALL" then goto L50350
               beg_mon$ = "ALL"
               beg_mon% = 1%
               goto L50400
L50350:     convert beg_mon$ to beg_mon%, data goto L50360
L50360:
            if beg_mon% < 1% or beg_mon% > 12% then goto L50520

L50400:     end_mon% = 12%
            if end_mon$ <> " "  and end_mon$ <> "END" then goto L50450
               end_mon$ = "END"
               end_mon% = 12%
               goto L50500
L50450:     convert end_mon$ to end_mon%, data goto L50460
L50460:
            if end_mon% < 1% or end_mon% > 12% then goto L50520
L50500: return

L50520:     errormsg$ = "INVALID BEG OR END PARAMETER "
            beg_mon$, end_mon$ = " "
            beg_ord$, end_ord$ = " "
            beg_mon%, end_mon% = 0%
        return

L50570: REM BEG/END PRODUCT                     BEG_PROD$, END_PROD$
            if beg_prod$ <> " " and beg_prod$ <> "ALL" then goto L50610
               beg_prod$ = "000"
               goto L50650
L50610:     convert beg_prod$ to beg_prod%, data goto L50730

            convert beg_prod% to beg_prod$, pic(000)

L50650:     if end_prod$ <> " "  and end_prod$ <> "END" then goto L50680
               end_prod$ = "999"
               goto L50720
L50680:     convert end_prod$ to end_prod%, data goto L50730

            convert end_prod% to end_prod$, pic(000)

L50720: return
L50730:     errormsg$ = "INVALID BEG OR END PARAMETER "
            beg_prod$, end_prod$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
                                                   /* COLUMN 1 HEADER */
L55070: %!FOR:  ########                   ##############################~
        ~##########                                          PAGE : ###  !

L55100: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55130: %!<------ PART NUMBER ------>!<---------- DESCRIPTION --------->!~
        ~ON ORDER QTY ! QUANTITY SHIPPED ! OPEN ORDER QUANTITY!  ON HAND !

L55160: %!---------------------------!----------------------------------!~
        ~-------------!------------------!--------------------!----------!

L55190: %! ######################### ! ################################ !~
        ~ ####,###.##-!   ####,###.##-   !    ####,###.##-    !##,###.##-!

L55220: %! TOTAL FOR (###)           !                                  !~
        ~ ####,###.##-!   ####,###.##-   !    ####,###.##-    !##,###.##-!

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************


        get_quantity
          ord% = 0%
          on_hand, bck_ord, on_ord, on_comit, in_proc, qty_pend = 0.0
          init(" ") on_hand$, bck_ord$, on_ord$, on_comit$, in_proc$,    ~
                                                 qty_pend$, net_qty$
          net_qty = 0.0
          readkey$ = all(hex(00))
          str(readkey$,1%,25%) = part_no$
          str(readkey$,26%,3%) = "300"
          read #1,key > readkey$, eod goto L60220
            get #1, using L60180,readkey$, on_hand, bck_ord, on_ord,      ~
                                          on_comit, in_proc, qty_pend

L60180:        FMT POS(17), CH(44), POS(69), 6* PD(14,4)
          if str(readkey$,1%,25%) <> part_no$ then goto L60380
             if str(readkey$,26%,3%) <> "300" then goto L60380
             ord% = 1%
L60220:      convert on_hand to on_hand$, pic(####,###.##-)

             convert bck_ord to bck_ord$, pic(####,###.##-)

             convert on_ord to on_ord$, pic(####,###.##-)

             convert on_comit to on_comit$, pic(####,###.##-)

             convert in_proc to in_proc$, pic(####,###.##-)

             convert qty_pend to qty_pend$, pic(####,###.##-)

             net_qty = round(on_hand + bck_ord + on_ord + on_comit +     ~
                             in_proc + qty_pend, 2)
             convert net_qty to net_qty$, pic(####,###.##-)

L60380: return

        orders
             count% = 0%
             call "SHOSTAT" ("Building Sorted Work File")
             call "OPENCHCK" (#05, fs%(05), f2%(05),3000%, rslt$(05))
             beg_ord$ = so$(beg_mon%)

             ord_ord$ = beg_ord$
             start% = 1%
             for i% = 1% to 12%
               if so$(i%) <> " " then goto L60510
                  so$(i%) = "99999999"
L60510:      next i%
             if end_mon% <> 12% then goto L60550
                end_ord$ = "99999999"
                goto L60570
L60550:      end_ord$ = so$(end_mon% + 1%)

L60570:      ord_key$ = all(hex(00))
             str(ord_key$,1%,8%) = ord_ord$
             read #3,key > ord_key$, eod goto orders_done
             goto L60630
        next_order
             read #3, eod goto orders_done
L60630:        get #3, using L60650, ord_ord$, part_no$, ord_qty,ord_ship,~
                                                                ord_open
L60650:          FMT XX(9), CH(16), XX(6), CH(25), XX(36), 3*PD(14,4)
             if ord_ord$ >= end_ord$ then goto orders_done
             count% = count% + 1%
             if ord_ord$ < so$(1%) then goto next_order
             if len(part_no$) < 19 then goto next_order
             if beg_prod$ = "ALL" then goto L60740
                mod$ = str(part_no$,1%,3%)
                if mod$ < beg_prod$ or mod$ > end_prod$ then             ~
                                                          goto next_order
L60740:         for i% = start% to 11%
                  if ord_ord$ < so$(i%+1%) then goto L60770
                next i%
L60770:         start% = i%
                wrk_key$ = all(hex(00))
                w1, w2, w3 = 0.0
                f1%(5%) = 0%
                str(wrk_key$,1%,6%)  = dt$(i%)
                str(wrk_key$,7%,25%) = part_no$
                read #5,hold,key = wrk_key$, using L60850, w1, w2, w3,    ~
                                                          eod goto L60880
L60850:            FMT POS(32), 3*PD(14,4)
                f1%(5%) = 1%

L60880:         w1 = round(w1 + ord_qty, 2)
                w2 = round(w2 + ord_ship, 2)
                w3 = round(w3 + ord_open, 2)
                put #5, using L60920, wrk_key$, w1, w2, w3, " "
L60920:           FMT CH(31), 3*PD(14,4), CH(9)
                if f1%(5%) = 0% then write #5 else rewrite #5
                if mod(count%,100) <> 0% then goto L60980
                   convert count% to count$, pic(########)
                   call "SHOSTAT" ("ORDERS PROCESSED ("&count$&")")

L60980:         goto next_order
        orders_done

        return

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$ = "Inventory Product Sold by S.O. "
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCHNY", " ",4000%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCHNY", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Creating Inventory Product Sold Report")
            gosub select_printer
            mod$, prt_date$, dte$ = " "
            tot_w1, tot_w2, tot_w3, tot_w4 = 0.0
            wrk_key$ = all(hex(00))
            read #5,key > wrk_key$, eod goto generate_done
            goto L61290
        generate_next
            read #5, eod goto generate_done
L61290:       get #5, using L61300, save_date$, part_no$, w1, w2, w3
L61300:         FMT CH(6), CH(25), 3*PD(14,4)
              if save_date$ = dte$ then goto L61370
                 if dte$ <> " " then lcnt% = 66% else lcnt% = 99%
                 dte$ = save_date$
                 str(prt_date$,1%,6%)  = dte$
                 call "DATEFMT" (prt_date$)

L61370:       gosub print_detail
            goto generate_next
        generate_done
          print using L55160
          print using L55220, mod$, tot_w1, tot_w2, tot_w3, tot_w4
          print using L55040
            gosub close_printer
        return

        print_header
          page_no% = page_no% + 1%
          print page
          print using L55040
          print using L55070, prt_date$, print_title$, page_no%
          print using L55100
          print using L55130
          lcnt% = 4%
        return

        print_detail
          if mod$ = str(part_no$,1%,3%) then goto L61610
             gosub print_totals
             mod$ = str(part_no$,1%,3%)

L61610:   if lcnt% < 60% then goto L61640
             if lcnt% <> 99% then print using L55040
             gosub print_header
L61640:   gosub lookup_part
          print using L55160
          print using L55190, part_no$, part_desc$, w1, w2, w3, on_hand
          lcnt% = lcnt% + 2%
        return

        lookup_part
          call "DESCRIBE" (#2, part_no$, part_desc$, 0%, f1%(2%))
          if f1%(2%) <> 0% then goto L61800

          err% = 0%
          call "APCDESCR" (part_no$, apc_scr$, apc_prt$, apc_sze$, #6,   ~
                                                                   err% )
          str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
          str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)

L61800:   on_hand = 0.0
          readkey$ = all(hex(00))
          str(readkey$,1%,25%) = part_no$
          str(readkey$,26%,3%) = "300"
          read #1,key > readkey$, using L61860, readkey$, on_hand,        ~
                                                         eod goto L61900
L61860:        FMT XX(16), CH(44), XX(8), PD(14,4)
          if str(readkey$,1%,25%) <> part_no$ then on_hand = 0.0
          if str(readkey$,26%,3%) <> "300" then on_hand = 0.0

L61900:   tot_w1 = round(tot_w1 + w1, 2)
          tot_w2 = round(tot_w2 + w2, 2)
          tot_w3 = round(tot_w3 + w3, 2)
          tot_w4 = round(tot_w4 + on_hand, 2)
        return

        print_totals
          if lcnt% = 99% then return
          print using L55160
          print using L55220, mod$, tot_w1, tot_w2, tot_w3, tot_w4
          print using L55040
          gosub print_header
          tot_w1, tot_w2, tot_w3, tot_w4 = 0.0
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
