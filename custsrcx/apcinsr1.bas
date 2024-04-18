        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   IIIII  N   N   SSS   RRRR     1     *~
            *  A   A  P   P  C   C    I    NN  N  S      R   R   11     *~
            *  AAAAA  PPPP   C        I    N N N   SSS   RRRR     1     *~
            *  A   A  P      C   C    I    N  NN      S  R   R    1     *~
            *  A   A  P       CCC   IIIII  N   N   SSS   R   R  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCINSR1 - Sorts by Type or Category or Class             *~
            *            Same as (APCINSR1) but it also has a Quantity  *~
            *            Option.                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/01/91 ! Original                                 ! RHH *~
            * 04/07/92 ! Modification Add Quantity Options        ! RHH *~
            * 11/02/94 ! Mod for ROP Changes                      !     *~
            *          !                                          !     *~
            * 11/06/97 ! Change revision number to 60403          ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                              /* Program Variables          */~
            qty_opt$1,                   /* Quantity Options (1,2,3)   */~
            qty_desc$(3%)25,             /* Quantity Descriptions      */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmpart_category$4,           /* Part Category              */~
            fmpart_class$4,              /* Part Class                 */~
            fmpart_store$3,              /* Store Code                 */~
            fmpart_type$3,               /* Part Type                  */~
            hicategory$4,                /* Part Category              */~
            hiclass$4,                   /* Part Class                 */~
            histore$3,                   /* Store Code                 */~
            hitype$3,                    /* Part Type                  */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            locategory$4,                /* Part Category              */~
            loclass$4,                   /* Part Class                 */~
            lostore$3,                   /* Store Code                 */~
            lotype$3,                    /* Part Type                  */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            rpttitle$60,                 /* Report Title               */~
            sort_opt$1,                  /* How Sorted                 */~
            sort$7,                      /* For Print Column (1)       */~
            time$8,                      /* System Time                */~
            topart_category$4,           /* Part Category              */~
            topart_class$4,              /* Part Class                 */~
            topart_store$3,              /* Store Code                 */~
            topart_type$3,               /* Part Type                  */~
            userid$3                     /* Current User Id            */

        dim                              /* User Defined               */~
            descr$32,                    /* Part Description           */~
            part_no$25,                  /* Part Number                */~
            lot$16,                      /* Lot Number                 */~
            category$4,                  /* Category Code              */~
            class$4,                     /* Class Code                 */~
            type$3,                      /* Type Code                  */~
            sort_head$8,                 /* Column Heading for Sort Fld*/~
            on_hand$10,                  /* On-Hand Qty in String      */~
            on_order$10,                 /* On-Order Qty in String     */~
            rop_qty$10,                  /*                            */~
            roq_qty$10,                  /*                            */~
            cost$12,                     /* Cost in String             */~
            cost_ext$12,                 /*                            */~
            store$3,                     /* Store Number               */~
            bin$8                        /* Bin Location               */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim readkey$44,                  /* Scan Key                   */~
            sort_key$35,                 /* Sort Key                   */~
            sort_rec$137,                /* Sort Record                */~
            cnt$5, sel$, scr$36          /* COUNT AND SELECTED         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/06/97 Special Inv/ROP Report         "
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
            * #1  ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! APCINSRT ! Sort Work File                           *~
            * #4  ! ROPHNY   ! Re-Order Point Master File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #3,  "APCINSRT",                                      ~
                        varc,     indexed, recsize = 172,                ~
                        keypos =  1, keylen = 35

            select #4,  "ROPHNY",                                        ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen =  4

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),200%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            ret% = 0%
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "APCINSR1: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
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
                  if keyhit%  = 16% then gosub scan_data
                  if keyhit% <>  0% then gosub editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
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
            *           S O R T   W O R K   A R E A                     *~
            *-----------------------------------------------------------*~
            * Data Sort Area for Report                                 *~
            *************************************************************

        scan_data
            call "SHOSTAT" ("Sorting Inventory ... Please be patient")
            cnt%, sel% = 0%
            cnt$, sel$ = " "

            part_no$ = " "
            read #2,key > part_no$, using L35365, part_no$, descr$,       ~
                          category$, class$, type$, eod goto scan_done
            goto L19090
        scan_next
            read #2, using L35365, part_no$, descr$, category$, class$,   ~
                                  type$, eod goto scan_done
L19090:     cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0% then goto L19140
               convert cnt% to cnt$, pic(#####)

               convert sel% to sel$, pic(#####)

               scr$ = "Rec's Scanned(xxxxx) Selected(xxxxx)"
               str(scr$,15%,5%) = cnt$
               str(scr$,31%,5%) = sel$
               call "SHOSTAT" ( scr$ )

L19140:     if category$ < locategory$ or category$ > hicategory$        ~
                                              then goto scan_next
            if class$ < loclass$ or class$ > hiclass$ then goto scan_next
            if type$ < lotype$ or type$ > hitype$ then goto scan_next
            sort_key$ = " "
            if sort_opt$ = "1" then str(sort_key$,1%,7%) = type$
            if sort_opt$ = "2" then str(sort_key$,1%,7%) = category$
            if sort_opt$ = "3" then str(sort_key$,1%,7%) = class$

            readkey$ = " "
            str(readkey$,1%,25%) = part_no$
L19195:     gosub check_quantity
                if f1%(1%) = 0 then goto scan_next
            gosub update_sort
            goto L19195                    /* CHECK ALL STORES FOR PART */
        goto scan_next
        scan_done
            gosub generate_report
        return clear all
        goto inputmode

        check_quantity
L19250:     f1%(1%) = 0%
            read #1,key > readkey$, using L19260, readkey$, eod goto L19330
L19260:        FMT XX(16), CH(44)
            if str(readkey$,1%,25%) <> part_no$ then goto L19330
            if str(readkey$,26%,3%) < lostore$ or                        ~
               str(readkey$,26%,3%) > histore$ then goto L19250
               get #1,using L35030, part_no$, store$, lot$, bin$, on_hand,~
                                   on_order, cost
                                              /* TEST QUANTITY OPTIONS */
            if qty_opt% = 1% then goto L19320      /* (ALL) QUANTITIES  */
               if qty_opt% <> 2% then goto L19315
                  if on_hand > 0 then goto L19250  /* (ZERO) QUANTITIES */
                  goto L19320
L19315:        if on_hand < 1.0 then goto L19250   /* (NON-ZERO) QTY    */
L19320:     if sort_opt$ = "4" then str(sort_key$,1%,7%) = store$
            f1%(1%) = 1%
L19330: return

        update_sort
            rop_qty, roq_qty = 0.0
            read #4,key = part_no$, using L19360, rop_qty, roq_qty,       ~
                                                 eod goto L19365
L19360:        FMT XX(25), 2*PD(14,4)
L19365:     cost_ext = round( cost * on_hand, 2)

            convert on_hand to on_hand$,   pic(#,###,###-)

            convert rop_qty to rop_qty$,   pic(#,###,###-)

            convert on_order to on_order$, pic(#,###,###-)

            convert roq_qty to roq_qty$,   pic(#,###,###-)

            convert cost to cost$,         pic(####,###.##-)

            convert cost_ext to cost_ext$, pic(####,###.##-)

            sel% = sel% + 1%
            str(sort_key$,8%,3%)   = store$
            str(sort_key$,11%,25%) = part_no$
            sort_rec$ = " "
            str(sort_rec$,1%,3%)   = type$         /* TYPE CODE        */
            str(sort_rec$,4%,25%)  = part_no$      /* PART NUMBER      */
            str(sort_rec$,29%,32%) = descr$        /* PART DESCRIPTION */
            str(sort_rec$,61%,10%) = on_hand$      /* ON HAND QTY      */
            str(sort_rec$,71%,10%) = rop_qty$      /* ROP QTY          */
            str(sort_rec$,81%,10%) = on_order$     /* ON ORDER QTY     */
            str(sort_rec$,91%,10%) = roq_qty$      /* EOQ QTY          */
            str(sort_rec$,101%,12%)= cost$         /* UNIT COST        */
            str(sort_rec$,113%,12%)= cost_ext$     /* EXTENDED COST    */
            str(sort_rec$,125%,13%)= " "

            write #3,using L19515, sort_key$, sort_rec$, eod goto L19525
L19515:        FMT CH(35), CH(137)
        return
L19525:    stop "(Error) - Writing Sort Record --> " & sort_key$
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20150,         /* Part Category          */~
                              L20200,         /* Part Class             */~
                              L20250,         /* Part Type              */~
                              L20300,         /* Store Code             */~
                              L20350,         /* Report Type (Sort)     */~
                              L20380          /* Quantity Options       */
            return
L20150: REM Def/Enable Part Category               FMPART_CATEGORY$
            if fmpart_category$    = " " then                            ~
               fmpart_category$    = "ALL"
            return

L20200: REM Def/Enable Part Class                  FMPART_CLASS$
            if fmpart_class$       = " " then                            ~
               fmpart_class$       = "ALL"
            return

L20250: REM Def/Enable Part Type                   FMPART_TYPE$
            if fmpart_type$        = " " then                            ~
               fmpart_type$        = "ALL"
            return

L20300: REM Def/Enable Store Code                  FMPART_STORE$
            if fmpart_store$       = " " then                            ~
               fmpart_store$       = "ALL"
            return

L20350: REM Def/Enable Report Type
            return

L20380: REM Def/Quantity Options                   QTY_OPT$
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
         "Enter Beginning and Ending Part Category Code, Blank = 'ALL'?",~
         "Enter Beginning and Ending Part Class Code, Blank = 'ALL'?   ",~
         "Enter Beginning and Ending Part Type Code, Blank = 'ALL'?    ",~
         "Enter Beginning and Ending Store Code, Blank = 'ALL'?        ",~
         "Sort Options are 1 - Type, 2 - Category, 3 - Class, 4 - Store",~
         "Enter Quantity Options (1)=All, (2)=Zero, (3)=Non-Zero ?     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, fmpart_store$, lot$,       ~
                      fmpart_category$, fmpart_class$, fmpart_type$,     ~
                      hicategory$, hiclass$, hitype$, histore$,          ~
                      locategory$, loclass$, lotype$, lostore$, class$,  ~
                      topart_category$, topart_class$, topart_type$,     ~
                      sort_opt$, topart_store$, type$, on_order$,        ~
                      part_no$, cost$, category$, bin$, on_hand$,        ~
                      rop_qty$, roq_qty$, cost_ext$, sort_head$


            qty_opt% = 1%
            qty_opt$ = "1"
            qty_desc$(1%) = "(All) Quantities     "
            qty_desc$(2%) = "(Zero) Quantities    "
            qty_desc$(3%) = "(Non-Zero) Quantities"
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

L35030: FMT                 /* FILE: HNYQUAN                           */~
            XX(16),         /* Lot Number                              */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* Bin Location                            */~
            PD(14,4),       /* quantity on-hand                        */~
            XX(8),                                                       ~
            PD(14,4),       /* quantity on order                       */~
            XX(24),                                                      ~
            PD(14,4)        /* Total Cost                              */~

L35365: FMT                 /* FILE: HNYMASTR                          */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number Description                 */~
            XX(32),                                                      ~
            CH(4),          /* Part Category Code                      */~
            XX(39),                                                      ~
            CH(4),          /* User definable (free text) Class code fo*/~
            XX(43),                                                      ~
            CH(03)          /* Type of Part in HNYMASTR file           */~

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
              on fieldnr% gosub L40200,         /* Part Category     */   ~
                                L40200,         /* Part Class        */   ~
                                L40200,         /* Part Type         */   ~
                                L40200,         /* Store Code        */   ~
                                L40210,         /* Sort Option       */   ~
                                L40210          /* Quantity Option   */
              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Part Category",                              ~
               at (07,30), fac(lfac$( 1)), fmpart_category$     , ch(04),~
               at (07,56), fac(lfac$( 1)), topart_category$     , ch(04),~
                                                                         ~
               at (08,02), "Part Class",                                 ~
               at (08,30), fac(lfac$( 2)), fmpart_class$        , ch(04),~
               at (08,56), fac(lfac$( 2)), topart_class$        , ch(04),~
                                                                         ~
               at (09,02), "Part Type",                                  ~
               at (09,30), fac(lfac$( 3)), fmpart_type$         , ch(03),~
               at (09,56), fac(lfac$( 3)), topart_type$         , ch(03),~
                                                                         ~
               at (10,02), "Store Code",                                 ~
               at (10,30), fac(lfac$( 4)), fmpart_store$        , ch(03),~
               at (10,56), fac(lfac$( 4)), topart_store$        , ch(03),~
                                                                         ~
               at (11,02), "Sort Option",                                ~
               at (11,30), fac(lfac$( 5)), sort_opt$            , ch(1), ~
                                                                         ~
               at (12,02), "Qty  Option",                                ~
               at (12,30), fac(lfac$( 6)), qty_opt$             , ch(1), ~
               at (12,56), fac(hex(84)),   qty_desc$(qty_opt%)  , ch(21),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L40690
                  call "PRNTSCRN" : goto L40230

L40690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40850
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40860
L40850:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
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
                              L50240,         /* Part Class             */~
                              L50330,         /* Part Type              */~
                              L50410,         /* Store Code             */~
                              L50500,         /* Report Option          */~
                              L50610          /* Quantity Option        */
            return
L50150: REM Test for Part Category                FMPART_CATEGORY$
            call "TESTRNGE"                                              ~
                  (fmpart_category$    , topart_category$    ,           ~
                   locategory$    , hicategory$    ,                     ~
                   errormsg$)
            if fmpart_category$ = topart_category$ then                  ~
               locategory$, hicategory$ = fmpart_category$
            return

L50240: REM Test for Part Class                   FMPART_CLASS$
            call "TESTRNGE"                                              ~
                  (fmpart_class$       , topart_class$       ,           ~
                   loclass$       , hiclass$       ,                     ~
                   errormsg$)
            if fmpart_class$ = topart_class$ then                        ~
               loclass$, hiclass$ = fmpart_class$
            return

L50330: REM Test for Part Type                    FMPART_TYPE$
            call "TESTRNGE"                                              ~
                  (fmpart_type$        , topart_type$        ,           ~
                   lotype$        , hitype$        ,                     ~
                   errormsg$)
            if fmpart_type$ = topart_type$ then                          ~
               lotype$, hitype$ = fmpart_type$
            return
L50410: REM Test for Store Code                    FMPART_STORE$
            call "TESTRNGE"                                              ~
                 (fmpart_store$        , topart_store$        ,          ~
                  lostore$        , histore$       ,                     ~
                  errormsg$)
            if fmpart_store$ = topart_store$ then                        ~
               lostore$, histore$ = fmpart_store$
            return

L50500: REM Test for Report Type
            if sort_opt$ < "1" or sort_opt$ > "4" then                   ~
                  errormsg$ = "Invalid Report Sort Option, Try Again"
            if sort_opt$ = "1" then sort_head$ = "Type    "
            if sort_opt$ = "2" then sort_head$ = "Category"
            if sort_opt$ = "3" then sort_head$ = "Class   "
            if sort_opt$ = "4" then sort_head$ = "Store   "
            rpttitle$ = "    APC Inventory Listing  - Sort " &           ~
                        "Option by " & sort_head$
            return

L50610: REM Quantity Options
            if qty_opt$ <> " " then goto L50650
               qty_opt$ = "1"

L50650:     qty_opt% = 1%
            convert qty_opt$ to qty_opt%, data goto L50700

            if qty_opt% < 1% or qty_opt% > 3% then goto L50700
        return
L50700:     errormsg$ = "Invalid Quantity Option, Valid Entries 1,2,3?"
            qty_opt$ = " "
        return


        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L55070: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
L55100: %!RUN ######## @ ########              ##########################~
        ~##################################                       ########~
        ~  !
*       * Header Line 2
L55140: %!                                    ###########################~
        ~#################################                      PAGE: ####~
        ~  !
L55170: %!                                                               ~
        ~                                                                 ~
        ~  !
L55200: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--!

L55240: %!########!<-- Part Number -->!<------ Part Description ----->!  ~
        ~On-Hand !   R-O-P  ! On-Order !   R-O-Q  !    Cost    ! Ext Cost ~
        ~  !
L55270: %!####### !###################!###############################!##~
        ~########!##########!##########!##########!############!##########~
        ~##!
L55300: %!--------!-------------------!-------------------------------!--~
        ~--------!----------!----------!----------!------------!----------~
        ~--!

        %** Report Title for page 0
L55350: %############################################################

        %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *-----------------------------------------------------------*

        page_head              /* Page Heading Print Routine */
            pageno% = pageno% + 1%
            if pageno% = 0% then gosub print_params
            if lcntr% <> 99% then print using L55070
            print page        /* Top of Form */
            print using L55070
            print using L55100, date$, time$, company$, "APCINSR1"
            print using L55140, rpttitle$, pageno%
            print using L55170
            print using L55200
            print using L55240, sort_head$
            lcntr% = 6%
            return

        print_params                                /* Print Page Zero */
            print page
            tran(i$(), hex(208c2084208620ac))replacing
            print using L55350, rpttitle$
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pageno% = pageno% + 1%
        return

        print_detail
            if lcntr% > 57% then gosub page_head
            print using L55300
            print using L55270, sort$, str(part_no$,1%,19%),              ~
                        str(descr$,1%,31%),on_hand$, rop_qty$, on_order$,~
                        roq_qty$, cost$, cost_ext$
            lcntr% = lcntr% + 2%
        return

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        generate_report
            call "SHOSTAT" ("Now Processing Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" (" ", " ", 0%, 0%)
            pageno% = -1% : lcntr% = 99%       /* Page & Line Counters */
            sort_key$ = " "
            read #3,key > sort_key$, using L60660, sort_key$,             ~
                                  type$, part_no$, descr$, on_hand$,     ~
                                  rop_qty$, on_order$, roq_qty$, cost$,  ~
                                  cost_ext$, eod goto generate_done
            goto L60670
        generate_next
            read #3, using L60660, sort_key$,                             ~
                                  type$, part_no$, descr$, on_hand$,     ~
                                  rop_qty$, on_order$, roq_qty$, cost$,  ~
                                  cost_ext$, eod goto generate_done
L60660:        FMT CH(35), CH(3), CH(25), CH(32), 4*CH(10), 2*CH(12)
L60670:     sort$ = str(sort_key$,1%,7%)
            gosub print_detail
            goto generate_next
        generate_done
            print using L55070
            close printer
            call "FILEBGON" (#3)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
