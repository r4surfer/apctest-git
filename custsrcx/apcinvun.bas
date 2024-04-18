        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   IIIII  N   N  V   V  U   U  N   N   *~
            *  A   A  P   P  C   C    I    NN  N  V   V  U   U  NN  N   *~
            *  AAAAA  PPPP   C        I    N N N  V   V  U   U  N N N   *~
            *  A   A  P      C   C    I    N  NN   V V   U   U  N  NN   *~
            *  A   A  P       CCC   IIIII  N   N    V     UUU   N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCINVUN - Sorts by Type or Category or Class             *~
            *            Show Unit Conversion Factor and Cost Per Unit  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/21/93 ! Original                                 ! RHH *~
            *          !                                          !     *~
            * 11/11/97 ! Revision Update For 60403                ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                              /* Program Variables          */~
            readkey$50,                  /* Generic Key                */~
            col_head$51,                 /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            beg_cat$4, end_cat$4,        /* CATEGORY                   */~
            beg_cls$4, end_cls$4,        /* CLASS CODE                 */~
            beg_typ$3, end_typ$3,        /* TYPE CODE                  */~
            beg_str$3, end_str$3,        /* Store Code                 */~
            part_no$25,                  /* PART NUMBER                */~
            descr$32,                    /* PART DESCRIPTION           */~
            class$4,                     /* Part Class                 */~
            store$3,                     /* Store Code                 */~
            type$3,                      /* Part Type                  */~
            category$4,                  /* CATEGORY CODE              */~
            sort_key$8,                  /* SORT KEY                   */~
            srt_rec$120,                 /* SORT RECORD                */~
            unit$1,                      /* CONVERSION UNIT CODE       */~
            raw_size$10,                 /* RAW SIZE CONVERSION        */~
            unit_cost$10,                /* UNIT COST                  */~
            unit_desc$15,                /* UNIT DESCRIPTION           */~
            units$(10)15,                /* UNIT DESCRIPTION           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            rpttitle$60,                 /* Report Title               */~
            sort_opt$1,                  /* How Sorted                 */~
            runtime$8,                   /* System Time                */~
            hand_qty$10, order_qty$10,   /*                            */~
            on_hand$10, on_order$10,     /*                            */~
            sel$1, sel_d$30,             /*                            */~
            userid$3                     /* Current User Id            */

        dim f2%(5),                      /* = 0 if the file is open    */~
            f1%(5),                      /* = 1 if READ was successful */~
            fs%(5),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/15/93 Pre-Release Version            "
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
            * #01 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! APCINVUN ! Sortfile                                 *~
            * #04 ! GENCODES ! MASTER CODE FILE                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #3,  "APCINVUN",                                      ~
                        varc,     indexed, recsize = 128,                ~
                        keypos = 1, keylen = 8

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1), f2%(1), 0%, rslt$(1))
            call "OPENCHCK" (#2, fs%(2), f2%(2), 0%, rslt$(2))
            call "OPENCHCK" (#3, fs%(3), f2%(3), 0%, rslt$(3))
            if fs%(3) <> 0 then call "FILEBGON" addr(#3)
            mat f1% = zer

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


            str(col_head$, 1) = "Beginning Code"
            str(col_head$,27) = "Ending Code"

            units$( 1) = "Cost Per Piece "
            units$( 2) = "Per Square Foot"
            units$( 3) = "Per Square Inch"
            units$( 4) = "Cost Per Foot  "
            units$( 5) = "Cost Per Inch  "
            units$( 6) = "Cost Per U. I. "
            units$( 7) = "Cost per Pound "
            units$( 8) = "Cost Per Ounce "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
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
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub process_data
                  if keyhit% <>  0% then gosub editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
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
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************

        process_data
            gosub sort
            gosub rpt_select
            call "SHOSTAT" (" Printing Report by ( "&sort_head$&" )" )
            total1, total2, total3 = 0.0
            sort_key$ = " "
            read #3,key > sort_key$, using L19170, srt_rec$,              ~
                                                    eod goto process_done
               goto L19180
        process_next
            read #3, using L19170, srt_rec$, eod goto process_done
L19170:          FMT POS(9), CH(120)
L19180:       part_no$   = str(srt_rec$,1%,25%)
              descr$     = str(srt_rec$,26%,32%)
              category$  = str(srt_rec$,58%,4%)
              class$     = str(srt_rec$,62%,4%)
              type$      = str(srt_rec$,66%,3%)
              store$     = str(srt_rec$,69%,3%)
              unit$      = str(srt_rec$,80%,1%)
              unit_desc$ = "***************"
              raw_size, unit_cost = 0.0
              if unit$ = "*" then goto L19370
                 convert unit$ to unit%, data goto L19290
L19290:
                 unit_desc$ = units$(unit%)
                 raw_size$  = str(srt_rec$,81%,10%)
                 unit_cost$ = str(srt_rec$,91%,10%)
                 convert raw_size$ to raw_size, data goto L19340
L19340:
                 convert unit_cost$ to unit_cost, data goto L19360
L19360:
L19370:          convert unit_cost to unit_cost$, pic(###.####)

              hand_qty, order_qty = 0.0
              on_hand, on_order = 0.0
              hand_qty$  = str(srt_rec$,101%,10%)
              order_qty$ = str(srt_rec$,111%,10%)
              convert hand_qty$ to hand_qty, data goto L19440
L19440:
              convert order_qty$ to order_qty, data goto L19460
L19460:
              on_order = round( (raw_size * order_qty) * unit_cost, 2)
              on_hand  = round( (raw_size * hand_qty ) * unit_cost, 2)
              convert on_order to on_order$, pic(######.##-)

              convert on_hand to on_hand$, pic(######.##-)

              tot_val = round(on_hand + on_order, 2)
              total1  = round( total1 + on_order, 2)
              total2  = round( total2 + on_hand,  2)
              total3  = round( total3 + tot_val,  2)
              convert tot_val to tot_val$, pic(####,###.##-)
              if sel$ = "1" then gosub print_detail                      ~
                            else gosub print_detail_v
              goto process_next
        process_done
           gosub close_printer
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Part Category                                          ",~
         "Enter Part Type                                              ",~
         "Enter Store Code                                             ",~
         "Sort Options are 1 - Type, 2 - Category, 3 - Store           ",~
         "Report Selection - (1)-Unit Description, (2)-Inventory Value?"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_cat$, end_cat$,        ~
                      beg_cls$, end_cls$, beg_typ$, end_typ$, beg_str$,  ~
                      end_str$, part_no$, descr$, category$, class$,     ~
                      type$, store$, readkey$, raw_unit$, sel$, sel_d$,  ~
                      sort_head$, on_hand$, on_order$, total1$, total2$, ~
                      total3$, tot_val$, sort_opt$
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
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* File: HNYQUAN (#1)                      */~
            POS(17),        /* Lot Number                              */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Bin Location                            */~
            PD(14,4),       /* quantity on-hand                        */~
            XX(8),                                                       ~
            PD(14,4),       /* quantity on order                       */~
            XX(24),                                                      ~
            PD(14,4)        /* Total Cost                              */~

L35365: FMT                 /* File: HNYMASTR (#2)                     */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number Description                 */~
            POS(90),                                                     ~
            CH(4),          /* Part Category Code                      */~
            POS(133),                                                    ~
            CH(4),          /* User definable (free text) Class code fo*/~
            POS(180),                                                    ~
            CH(03),         /* Type of Part in HNYMASTR file           */~
            POS(686),                                                    ~
            CH(20)          /* Raw Unit Definition from Var Field */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40190,         /* Part Category     */   ~
                                L40190,         /* Part Type         */   ~
                                L40190,         /* Store Code        */   ~
                                L40200,         /* Sort Option       */   ~
                                L40190          /* Unit or Inv Value */
              goto L40220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40200:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,24),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02),                                               ~
             "NOTE: All MFG Parts are Skipped. (Greater than 18 Digits)",~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   col_head$            , ch(51),~
                                                                         ~
               at (07,02), "Part Category",                              ~
               at (07,30), fac(lfac$( 1)), beg_cat$             , ch(04),~
               at (07,56), fac(lfac$( 1)), end_cat$             , ch(04),~
                                                                         ~
               at (08,02), "Part Type",                                  ~
               at (08,30), fac(lfac$( 2)), beg_typ$             , ch(03),~
               at (08,56), fac(lfac$( 2)), end_typ$             , ch(03),~
                                                                         ~
               at (09,02), "Store Code",                                 ~
               at (09,30), fac(lfac$( 3)), beg_str$             , ch(03),~
               at (09,56), fac(lfac$( 3)), end_str$             , ch(03),~
                                                                         ~
               at (10,02), "Sort Option",                                ~
               at (10,30), fac(lfac$( 4)), sort_opt$            , ch(1), ~
               at (10,40), fac(hex(84)),   sort_head$           , ch(30),~
                                                                         ~
               at (11,02), "Selection",                                  ~
               at (11,30), fac(lfac$( 5)), sel$                 , ch(01),~
               at (11,40), fac(hex(84)),   sel_d$               , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L40630
                  call "PRNTSCRN" : goto L40220

L40630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
        if edit% = 2% then L40820     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40800
L40790:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40800:     return

L40820: if fieldnr% > 0% then L40910  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40910:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Part Category          */~
                              L50300,         /* Part Type              */~
                              L50450,         /* Store Code             */~
                              L50600,         /* Sort Option            */~
                              L50680          /* Report Selection       */
            return

L50150: REM Test for Part Category                BEG_CAT$, END_CAT$
            if str(beg_cat$,1%,1%) = "A" then goto L50180
            if beg_cat$ <> " " then goto L50210
L50180:        beg_cat$ = "ALL "
               end_cat$ = "    "
               return
L50210:     if end_cat$ <> " " then goto L50240
               end_cat$ = beg_cat$
               return
L50240:      if beg_cat$ > end_cat$ then goto L50260
        return
L50260:     errormsg$ = "(Error) - Invalid Part Category Code."
            beg_cat$, end_cat$ = " "
        return

L50300: REM Test for Part Type                    BEG_TYP$, END_TYP$
            if str(beg_typ$,1%,1%) = "A" then goto L50330
            if beg_typ$ <> " " then goto L50360
L50330:        beg_typ$ = "ALL"
               end_typ$ = "   "
               return
L50360:     if end_typ$ <> " " then goto L50390
               end_typ$ = beg_typ$
               return
L50390:      if beg_typ$ > end_typ$ then goto L50410
        return
L50410:     errormsg$ = "(Error) - Invalid Part Class Code."
            beg_typ$, end_typ$ = " "
        return

L50450: REM Test for Store Code                    BEG_STR$, END_STR$
            if str(beg_str$,1%,1%) = "A" then goto L50480
            if beg_str$ <> " " then goto L50510
L50480:        beg_str$ = "ALL"
               end_str$ = "   "
               return
L50510:     if end_str$ <> " " then goto L50540
               end_str$ = beg_str$
               return
L50540:      if beg_str$ > end_str$ then goto L50560
        return
L50560:     errormsg$ = "(Error) - Invalid Store Code. "
            beg_str$, end_str$ = " "
        return

L50600: REM Test for Report Type
            if sort_opt$ < "1" or sort_opt$ > "3" then                   ~
                  errormsg$ = "Invalid Report Sort Option, Try Again"
            if sort_opt$ = "1" then sort_head$ = "Type    "
            if sort_opt$ = "2" then sort_head$ = "Category"
            if sort_opt$ = "3" then sort_head$ = "Store   "
        return

L50680: REM Report Selection
            if sel$ <> " " then goto L50710
               sel$ = "1"
L50710:     if sel$ = "1" then sel_d$ = "Unit Description Report"
            if sel$ = "2" then sel_d$ = "Inventory Value Report"
            if sel$ <> "1" and sel$ <> "2" then goto L50750
        return
L50750:     errormsg$ = "(Error) - Invalid Report Selection 1 or 2"
            init(" ") sel$, sel_d$
        return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L55070: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55100: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55130: %!RUN ######## @ ########             ###########################~
        ~#################################                      APCINVUN:!

*       * Header Line 2
L55170: %!                                    ###########################~
        ~#################################                    PAGE: #### !

*       * Deteail line 1                              /* Unit Report  */
L55210: %!<----- Part Number ----->!<-------- Description --------->!Type~
        ~!Class!Category!Store! Unit Description  ! Raw Size ! Unit Cost !

L55240: %!#########################!################################! ###~
        ~! ####!  ####  ! ### !# - ###############!##########!########## !

L55270: %!-------------------------!--------------------------------!----~
        ~!-----!--------!-----!-------------------!----------!-----------!

*       * Detail Line 1                               /* Value Report */
L55310: %!< Part Number >!<-------- Description ------->!Typ!Cat.!Str!U!U~
        ~nit Cst!Order  Qty!Hand   Qty!On Order$$!On Hand $$! Total Value!

L55340: %!###############!##############################!###!####!###!#!#~
        ~#######!##########!##########!##########!##########!############!

L55370: %!---------------!------------------------------!---!----!---!-!-~
        ~-------!----------!----------!----------!----------!------------!

*       * Total Line 1                                /* Value Report */
L55410: %! Report Totals for Inventory Value                             ~
        ~                             !##########!##########!############!

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

        rpt_select
            select printer(134)
            runtime$ = " "  :  call "TIME" (runtime$)
            call "SETPRNT" ("INVUN", " ", 0%, 0%)
            pageno% = 0% : lcntr% = 99% : ret% = 0%
            call "COMPNAME" (12%, company$, ret%)
            rpttitle$ = "    APC Inventory Listing  - Sort " &           ~
                        "Option by " & sort_head$
           call "FMTTITLE" (company$, " ", 12%)
           call "FMTTITLE" (rpttitle$, " ", 12%)
        return

        close_printer
            if sel$ = "1" then print using L55070                         ~
                          else gosub print_totals

            call "SETPRNT" ("INVUN", " ", 0%, 1%)
            call "FILEBGON" addr(#3)
        return

        page_head              /* Page Heading Print Routine */
            if lcntr% <> 99% then print using L55070
            pageno% = pageno% + 1%
            print page        /* Top of Form */
            print using L55070
            print using L55130, date$, runtime$, company$
            print using L55170, rpttitle$, pageno%
            print using L55100
            print using L55210
            lcntr% = 5%
        return

        page_head_v            /* Page Heading Print Routine */
            if lcntr% <> 99% then print using L55070
            pageno% = pageno% + 1%
            print page        /* Top of Form */
            print using L55070
            print using L55130, date$, runtime$, company$
            print using L55170, rpttitle$, pageno%
            print using L55100
            print using L55310
            lcntr% = 5%
        return

        print_detail
            if lcntr% > 58% then gosub page_head
            print using L55270
            print using L55240, part_no$, descr$, type$, class$,category$,~
                        store$, unit$, unit_desc$, raw_size$, unit_cost$
            lcntr% = lcntr% + 2%
        return

        print_detail_v
            if lcntr% > 58% then gosub page_head_v
            print using L55370
            print using L55340, str(part_no$,1%,15%), str(descr$,1%,30%), ~
                               type$, category$, store$, unit$,          ~
                               unit_cost$, order_qty$, hand_qty$,        ~
                               on_order$, on_hand$, tot_val$
            lcntr% = lcntr% + 2%
        return

        print_totals
            convert total1 to total1$, pic(##,###,###)
            convert total2 to total2$, pic(##,###,###)
            convert total3 to total3$, pic(####,###,###)
            print using L55100
            print using L55410, total1$, total2$, total3$
            print using L55070
        return

        sort
            call "OPENCHCK" (#3, fs%(3), f2%(3), 500%, rslt$(3))
            call "SHOSTAT" ("Beginning Sort by ( "&sort_head$&" )" )
            cnt%, sort% = 0%
            readkey$ = all(hex(00))
            read #2,key > readkey$, using L35365, part_no$, descr$,       ~
                                    category$, class$, type$, raw_unit$, ~
                                                       eod goto sort_done
               goto L60890
        sort_next
            read #2, using L35365, part_no$, descr$, category$, class$,   ~
                                  type$, raw_unit$, eod goto sort_done
L60890:     cnt% = cnt% + 1%
            if mod(cnt%,100) = 0 then print at(04,38);"[";cnt%;"]"
            if len(part_no$) > 18% then goto sort_next

            if beg_cat$ = "ALL " then goto L60970
               if category$ < beg_cat$ or category$ > end_cat$ then      ~
                                             goto sort_next
                                                       /* CHECK TYPE   */
L60970:     if beg_typ$ = "ALL" then goto L61010
               if type$ < beg_typ$ or type$ > end_typ$ then              ~
                                             goto sort_next
                                                       /* CHECK STORE  */
L61010:     readkey$ = all(hex(00))
            str(readkey$,1%,25%) = part_no$
        check_store
            read #1,key > readkey$, using L61060, readkey$, hand_qty,     ~
                                    order_qty, cost, eod goto sort_next
L61060:        FMT POS(17), CH(44), POS(69), PD(14,4), POS(85), PD(14,4),~
                                    POS(117), PD(14,4)
            if str(readkey$,1%,25%) <> part_no$ then goto sort_next
               store$ = str(readkey$,26%,3%)
            if beg_str$ = "ALL" then goto L61140
               if store$ < beg_str$ or store$ > end_str$ then            ~
                                                         goto check_store
                                                   /* CHECK SORT TYPES */
L61140:     init(" ") sort_key$, srt_rec$
            if sort_opt$ = "1" then str(sort_key$,1%,4%) = type$
            if sort_opt$ = "2" then str(sort_key$,1%,4%) = category$
            if sort_opt$ = "3" then str(sort_key$,1%,4%) = store$
            sort% = sort% + 1%
            convert sort% to str(sort_key$,5%,4%), pic(0000)
            str(srt_rec$,1%,25%)  = part_no$
            str(srt_rec$,26%,32%) = descr$
            str(srt_rec$,58%,4%)  = category$
            str(srt_rec$,62%,4%)  = class$
            str(srt_rec$,66%,3%)  = type$
            str(srt_rec$,69%,3%)  = store$
            str(srt_rec$,72%,8%)  = " "
            str(srt_rec$,80%,1%)  = "*"   /* COST UNIT CONVERSION CODE */
            str(srt_rec$,81%,10%) = "**********"       /* RAW MAT SIZE */
            str(srt_rec$,91%,10%) = "**********"       /* UNIT COST    */
            str(srt_rec$,101%,20%) = " "

            unit_cost = 0.0
            p% = pos(raw_unit$ = "/")
            if p% = 0% then goto L61470
               raw_size = 1.0 : unit% = 1%
               convert str(raw_unit$,1%,p%-1%) to raw_size,              ~
                                                          data goto L61380
L61380:
               convert str(raw_unit$, p%+1%,1%) to unit%,                ~
                                                          data goto L61410
L61410:
               unit_cost = round( (cost / raw_size), 4)
               convert unit% to str(srt_rec$,80%,1%), pic(#)
               convert raw_size to str(srt_rec$,81,10%), pic(#######.##)
               convert unit_cost to str(srt_rec$,91%,10%),pic(#####.####)

L61470:     convert hand_qty  to str(srt_rec$,101%,10%), pic(######.##-)
            convert order_qty to str(srt_rec$,111%,10%), pic(######.##-)

            write #3, using L61510, sort_key$, srt_rec$, eod goto L61520
L61510:          FMT CH(8), CH(120)
L61520:     goto check_store
        sort_done
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
