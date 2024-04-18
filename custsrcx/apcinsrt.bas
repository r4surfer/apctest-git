        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   IIIII  N   N   SSS   RRRR   TTTTT   *~
            *  A   A  P   P  C   C    I    NN  N  S      R   R    T     *~
            *  AAAAA  PPPP   C        I    N N N   SSS   RRRR     T     *~
            *  A   A  P      C   C    I    N  NN      S  R   R    T     *~
            *  A   A  P       CCC   IIIII  N   N   SSS   R   R    T     *~
            *            ( Keith Hunter Utility Report )                *~
            *-----------------------------------------------------------*~
            * APCINSRT - Sorts by Type or Category or Class(No Quantity)*~
            *            Same as (APCINSR1) Which has a Quantity Option *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/01/91 ! Original                                 ! RHH *~
            * 04/07/92 ! Modification Add Quantity Options        ! RHH *~
            *          !                                          !     *~
            * 11/06/97 ! Reflect 60403 Revision Code              ! DJD *~
            *          !                                          !     *~
            *          !                                          !     *~
            *************************************************************

        dim                              /* Program Variables          */~
            qty_opt$1,                   /* Quantity Options (1,2,3)   */~
            qty_desc$(3)25,              /* Quantity Descriptions      */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* File Name                  */~
            fmpart_category$4,           /* Part Category              */~
            fmpart_class$4,              /* Part Class                 */~
            fmpart_store$3,              /* Store Code                 */~
            fmpart_type$3,               /* Part Type                  */~
            hicategory$4,                /* Part Category              */~
            hiclass$4,                   /* Part Class                 */~
            histore$3,                   /* Store Code                 */~
            hitype$3,                    /* Part Type                  */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Library Name               */~
            line2$79,                    /* Screen Line #2             */~
            locategory$4,                /* Part Category              */~
            loclass$4,                   /* Part Class                 */~
            lostore$3,                   /* Store Code                 */~
            lotype$3,                    /* Part Type                  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            record$64,                   /* Sort Record                */~
            rpttitle$60,                 /* Report Title               */~
            sort_opt$1,                  /* How Sorted                 */~
            sort$116,                    /* For Sortcall               */~
            time$8,                      /* System Time                */~
            topart_category$4,           /* Part Category              */~
            topart_class$4,              /* Part Class                 */~
            topart_store$3,              /* Store Code                 */~
            topart_type$3,               /* Part Type                  */~
            userid$3,                    /* Current User Id            */~
            vol$6                        /* Volume Name                */~

        dim                              /* User Defined               */~
            mkey$25,                     /* HNYMASTR Master Key        */~
            descr$32,                    /* Part Description           */~
            quankey$44,                  /* HNYQUAN Key                */~
            part_no$25,                  /* Part Number                */~
            lot$16,                      /* Lot Number                 */~
            category$4,                  /* Category Code              */~
            class$4,                     /* Class Code                 */~
            type$3,                      /* Type Code                  */~
            rpt_detl$4,                  /* Selected Sort field        */~
            chk_change$4,                /* Check Sortfield Breaks     */~
            sort_head$8,                 /* Column Heading for Sort Fld*/~
            on_hand$11,                  /* On-Hand Qty in String      */~
            on_order$10,                 /* On-Order Qty in String     */~
            cost$11,                     /* Cost in String             */~
            store$3,                     /* Store Number               */~
            bin$8,                       /* Bin Location               */~
            extension$12                 /* Unit Price * On_Hand       */

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
            cms2v$ = "06.04.03 11/06/97 Pre-Release Version            "
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
            * #03 ! SORTFILE ! Sortfile                                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup
            select #03, "SORTFILE",                                      ~
                        varc,                                            ~
                        consec,                                          ~
                        recsize = 64                                     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "APCINSRT: " & str(cms2v$,,8)

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
                  if keyhit%  = 16% then goto extract_data
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
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data
            call "SHOSTAT" ("Sorting Inventory ... Please be patient")
            scount% = 0
            records% = 1000
            call "WORKOPN2" (#3, "OUTPT", records%, f1%(3))
L19100:     gosub check_master
                if f1%(2) = 0 then goto L19170
            plowkey$ = part_no$
L19130:     gosub check_quantity
                if f1%(1) = 0 then goto L19100
            gosub sort_records
            goto L19130
L19170:     close #3
            call "GETNAMES" addr(#3, file$, lib$, vol$)
            str(sort$,1%,8%) = file$
            str(sort$,9%,8%) = lib$
            str(sort$,17%,6%) = vol$
            str(sort$,23%,22%) = str(sort$,,22%)
            call "SORTCALL" addr(sort$, ret%)
            if ret% = 0% then goto L19320
                k% = 2%
                call "ASKUSER" (k%, "*** Sort Failure ***",              ~
               "Either unable to link to Sort (not found or protected)", ~
               "or SORT failed for some other reason (lack of space).",  ~
               "Press <RETURN> to acknowledge & exit.")
                goto exit_program
L19320:     call "WORKOPN2" (#3, "INPUT", records%, f1%(3))
            goto generate_report

        sort_records
            str(record$, 1%,5%) = rpt_detl$
            str(record$, 6%,25%) = part_no$
            str(record$,31%,3%) = store$
            str(record$,34%,16%) = lot$
            if scount% > 0% then L19450
                str(sort$,45%,9%) = "0001005CA"    /* Sort Option      */
                str(sort$,54%,9%) = "0006025CA"    /* Part Number      */
                str(sort$,63%,9%) = "0031003CA"    /* Store Number     */
                str(sort$,72%,9%) = "0034016CA"    /* Lot Number       */
L19450:     write #3, record$
            scount% = scount% + 1%
            return
        check_master
L19490:     call "PLOWNEXT" (#2, mkey$, 0%, f1%(2))
                if f1%(2) = 0 then return
            gosub load_master
            if category$ < locategory$ or category$ > hicategory$        ~
                then goto L19490
            if class$ < loclass$ or class$ > hiclass$ then L19490
            if type$ < lotype$ or type$ > hitype$ then L19490
            if sort_opt$ = "1" then rpt_detl$ = type$
            if sort_opt$ = "2" then rpt_detl$ = category$
            if sort_opt$ = "3" then rpt_detl$ = class$
            return

        check_quantity
L19620:     call "PLOWNEXT" (#1, plowkey$, 25%, f1%(1))
            if f1%(1) = 0 then goto L19750
            gosub load_quantity
                                              /* TEST QUANTITY OPTIONS */
            if qty_opt% = 1% then goto L19730      /* (ALL) QUANTITIES  */
               if qty_opt% <> 2% then goto L19710
                  if on_hand > 0 then goto L19620  /* (ZERO) QUANTITIES */
                  goto L19730

L19710:        if on_hand < 1.0 then goto L19620   /* (NON-ZERO) QTY    */

L19730:     if store$ < lostore$ or store$ > histore$ then L19620
            if sort_opt$ = "4" then rpt_detl$ = store$
L19750:     return

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
         "Enter Part Category                                          ",~
         "Enter Part Class                                             ",~
         "Enter Part Type                                              ",~
         "Enter Store Code                                             ",~
         "Sort Options are 1 - Type, 2 - Category, 3 - Class, 4 - Store",~
         "Enter Quantity Options (1)=All, (2)=Zero, (3)=Non-Zero       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, fmpart_store$, mkey$, lot$,~
                      fmpart_category$, fmpart_class$, fmpart_type$,     ~
                      hicategory$, hiclass$, hitype$, histore$, quankey$,~
                      locategory$, loclass$, lotype$, lostore$, class$,  ~
                      topart_category$, topart_class$, topart_type$,     ~
                      sort_opt$, record$, sort$, topart_store$, type$,   ~
                      on_order$, part_no$, cost$, category$, bin$,       ~
                      rpt_detl$, chk_change$, sort_head$, on_hand$,      ~
                      extension$, qty_opt$

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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            call "SHOSTAT" ("Now Processing Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head
            let rpt_detl$ = " "
        next_record
L30120:     call "READNEXT" (#3, f1%(3))
                if f1%(3) <> 0% then L30160
                     f1%(3) = 0
                     goto end_report
L30160:     get #3, using L30170, rpt_detl$, part_no$, store$, lot$
L30170:         FMT CH(5), CH(25), CH(3), CH(16)
            let mkey$ = part_no$
            call "READ100" (#2, mkey$, f1%(2))
                if f1%(2) = 0 then L30120           /* No Masterrecord  */
            gosub load_master
            if chk_change$ = " " then chk_change$ = rpt_detl$
            if chk_change$ = rpt_detl$ then goto L30282
            print : print : lcntr% = lcntr% + 2%
            chk_change$ = rpt_detl$
L30282:     let str(quankey$,1,25) = part_no$
            let str(quankey$,26,3) = store$
            let str(quankey$,29,16) = lot$
            call "READ100" (#1%, quankey$, f1%(1))
            if f1%(1) = 0 then goto sortread_err
            gosub load_quantity
            convert on_hand to on_hand$, pic (-######.###)
            convert on_order to on_order$, pic (-#####.###)
            convert cost to cost$, pic (-######.###)
            extension = cost * on_hand
            convert extension to extension$, pic (-###,###.###)
            if lcntr% > 56% then gosub page_head
            print using L60140, rpt_detl$, part_no$, descr$, on_hand$,    ~
                               on_order$, cost$, extension$
            lcntr% = lcntr% + 1%
            goto next_record
        sortread_err
            call "ASKUSER" (2%, "Please Call Customer Support..Sortfile",~
            " cannot find HNYQUAN record. Press <RETURN> to continue.")
        end_report                /* Report Ending Routine */
            print skip(2)
            print using L64990     /* End of report line */
            close printer
            call "FILEBGON" (#3)
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "APCINSRT"
            print using L60110, rpttitle$, pcntr%   : print
            print using L60130, sort_head$
            print
            lcntr% = 6%
            return

        print_params           /* Print Page Zero */
            print page
            tran(i$(), hex(208c2084208620ac))replacing
            print using L64980, rpttitle$
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

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

               if keyhit% <> 13 then L40660
                  call "MANUAL" ("APCINSRT") : goto L40230

L40660:        if keyhit% <> 15 then L40690
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

        load_master
            get #2, using L35365, part_no$, descr$, category$, class$,    ~
                                 type$
            return
        load_quantity
            get #1, using L35030, part_no$, store$, lot$, bin$, on_hand,  ~
                                 on_order, cost
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:RPTID

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####
L60130: %######## Part Number               Description                  ~
        ~    Qty-On-Hand  Qty-on-Order    Unit-Cost   Extension
L60140: %######## ######################### #############################~
        ~### ############  ###########  ########### ############
        %** Report Title for page 0
L64980: %############################################################

L64990: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
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
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end