        REM *************************************************************~          *                                                           *~
            *  V   V  EEEEE  N   N  PPPP   U   U  RRRR   RRRR   PPPP    *~
            *  V   V  E      NN  N  P   P  U   U  R   R  R   R  P   P   *~
            *  V   V  EEEE   N N N  PPPP   U   U  RRRR   RRRR   PPPP    *~
            *   V V   E      N  NN  P      U   U  R   R  R   R  P       *~
            *    V    EEEEE  N   N  P       UUU   R   R  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENPURRP - Report of purchase history by vendor and part. *~
            *            Various selection criteria and option to print *~
            *            part summary or receipt details.               *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/23/96 ! Original                                 ! JLR *~
            * 11/07/96 ! Add option to compare against standard   ! JLR *~
            * 02/19/97 ! option to convert LBS-CC to TONS-GAL     ! JLR *~
            * 03/25/97 ! allow quantities to have 4 decimals      ! JLR *~
            * 04/24/97 ! multiple invoices on a single receiver   ! JLR *~
            * 07/03/97 ! don't multiply standard in conv-uom = no ! JLR *~
            * 01/11/99 ! Modifications for century dates          ! JLR *~
	    * 12/15/99 ! retrieve weight from new Caelus field    ! JLR *~
	    * 05/12/00 ! option for inventoriable items only      ! JLR *~
            *************************************************************

        dim                                                              ~
            catcode$4,                   /* Part category              */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            conv_uom$3,                  /* Convert unit of measure    */~
            cost$(3)10,                  /* Formatted costs to print   */~
            cost_set$8,                  /* Cost set ID                */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            equiv$7,                     /* Equivalents                */~
            eqv$9,                       /* formatted equivalents      */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Work file name             */~
            fmcatcode$4,                 /* Part Category              */~
            fmdate$10,                   /* From date                  */~
            fmpart$25,                   /* Part Number                */~
            fmparttype$3,                /* Part Type                  */~
            fmvencode$9,                 /* Vendor Code                */~
            hicatcode$4,                 /* Part Category              */~
            hipart$25,                   /* Part Number                */~
            hiparttype$3,                /* Part Type                  */~
            hivencode$9,                 /* Vendor Code                */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invdate$8,                   /* Invoice date               */~
            invoice$16,                  /* Invoice number             */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Work file library          */~
            line2$79,                    /* Screen Line #2             */~
            locatcode$4,                 /* Part Category              */~
            lopart$25,                   /* Part Number                */~
            loparttype$3,                /* Part Type                  */~
            lovencode$9,                 /* Vendor Code                */~
            old_part$25, old_po_no$16,   /* Break comparisons          */~
            old_receiver$16, old_vbkseq$3,                               ~
            old_vencode$9,                                               ~
            orderdat$8,                  /* Order date                 */~
            part$25,                     /* Part number                */~
            part_desc$32,                /* Part description           */~
            parttype$3,                  /* Part type                  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po_no$16,                    /* Purchase order number      */~
            qty$(3)8,                    /* formatted print quantities */~
            rcvdate$8,                   /* Receiving date             */~
            receiver$16,                 /* Receiver number            */~
            rpttitle$60,                 /* Report Title               */~
            save_part$25,                /* Part number to save reread */~
            set_desc$32,                 /* Cost set description       */~
            sort$116,                    /* Parameters for SORTCALL    */~
            status$1,                    /* Purchase order line status */~
            std$9,                       /* Formatted standard cost    */~
	    stock$3,			 /* Inventoriable item         */~
            stk_uom$4,                   /* Stocking unit of measure   */~
            store$3,                     /* Store code                 */~
            sub_cost(3),                 /* Subtotals for cost         */~
            sub_qty(3),                  /* Subtotals for quantities   */~
            sub_std(3),                  /* Subtotals for standard cost*/~
            sum_dtl$1,                   /* Summary or Details         */~
            time$8,                      /* System Time                */~
            tocatcode$4,                 /* Part Category              */~
            todate$10,                   /* Date Range                 */~
            topart$25,                   /* Part Number                */~
            toparttype$3,                /* Part Type                  */~
            tot_cost(3),                 /* Totals for cost            */~
            tot_std(3),                  /* Totals for standard cost   */~
            tovencode$9,                 /* Vendor Code                */~
            unit_cost$1,                 /* Extended or unit cost      */~
            userid$3,                    /* Current User Id            */~
            vbkseq$3,                    /* Purchase order line seq#   */~
            vencode$9,                   /* Vendor code                */~
            vendate$8,                   /* payables date              */~
            vendname$30,                 /* Vendor Name                */~
            vol$6,                       /* Work file volume           */~
            wgt$11                       /* Formatted weight to print  */

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
            cms2v$ = "07.00.01 01/11/99 Century dates                  "
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! CATEGORY ! INVENTORY CATEGORY CODES FILE            *~
            * #05 ! GENCODES ! System General Codes file.               *~
            * #06 ! RCVMASTR ! Receiver Headers File                    *~
            * #07 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #08 ! PAYMASTR ! PAYABLES MAIN FILE  (INVOICE DATES)      *~
            * #09 ! PAYLINES ! Payables Invoice Line Item File          *~
            * #10 ! VBKMASTR ! PURCHASE ORDER HEADER FILE               *~
            * #11 ! VBKLINES ! Purchase Order Line Items File           *~
            * #20 ! WORKFILE ! Temporary System Workfile                *~
            * #21 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 01, keypos =   10, keylen =  30, dup

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key 01, keypos =  102, keylen =   9, dup,    ~
                            key 02, keypos =   90, keylen =   4, dup,    ~
                            key 03, keypos =   26, keylen =  32, dup

            select #04, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select #05, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #06, "RCVMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  16                      ~

            select #07, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  3, keypos =  128, keylen =  24,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  1, keypos =    1, keylen =  69          ~

            select #08, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  25                      ~

            select #09, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  2, keypos =   17, keylen =  47,         ~
                            key  1, keypos =    1, keylen =  63          ~

            select #10, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key 01, keypos =   10, keylen =  16

            select #11, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 01, keypos =  333, keylen =  20, dup

            select #20, "WORKFILE",                                      ~
                        varc,      consec,  recsize =  149

            select #21, "WORKFILE",                                      ~
                        varc,      consec,  recsize =  100

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))
            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))

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

            rpttitle$ = "Vendor Purchase History by Par" &               ~
                        "t                             "

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "VENPURRP: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  10%
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
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  10% then editpg1
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

            call "WORKOPN2" (#20, "OUTPT", 2500%, f2%(20))
            if f2%(20) = 0% then L13120
            call "SHOSTAT" ("Error opening workfile, exiting program")
            goto exit_program

L13120:     call "SHOSTAT" ("Extracting receipts, building workfile")
            test_part% = 0%           /* no need to test part info */
            if fmcatcode$ <> "ALL" or fmparttype$ <> "ALL" then          ~
                test_part% = 1%
	    if stock$ = "YES" then test_part% = 1%
            if fmdate$ = "ALL" then L13220
            call "DATUFMTC" (fmdate$)
            call "DATUFMTC" (todate$)

L13220:     plowkey$ = all(hex(00))
            call "PLOWNEXT" (#6, plowkey$, 0%, f1%(6))
            goto L13280

        next_receiver
            call "READNEXT" (#6, f1%(6))
L13280:     if f1%(6) = 0 then payables
            get #6 using L13300, receiver$, rcvdate$
L13300:     FMT CH(16), POS(86), CH(6)
            if fmdate$ = "ALL" then L13380
            if rcvdate$ < fmdate$ then next_receiver
            if rcvdate$ > todate$ then next_receiver

*        receiver ok, now get and test lines
L13380:     plowkey$ = str(receiver$) & lovencode$
            str(plowkey$, 26) = all(hex(00))
        next_receiver_line
            call "PLOWNEXT" (#7, plowkey$, 16%, f1%(7))
            if f1%(7) = 0 then next_receiver
            get #7 using L13450, part$, vencode$, po_no$, vbkseq$,        ~
                part_desc$, orderdat$, qty_oh, purch_pr, store$
L13450:     FMT CH(25), POS(42), CH(16), POS(51), CH(16), CH(3), POS(78),~
                CH(32), CH(6), POS(204), PD(14,4), PD(14,7), POS(332),   ~
                CH(3)
            if vencode$ > hivencode$ then next_receiver
            gosub test_part_ok
            if part_ok% = 0% then next_receiver_line
            if conv_uom$ = "NO" or f1%(3) = 0 then L13490
            if stk_uom$ <> "LBS" then L13486
                qty_oh = qty_oh / 2000
                purch_pr = purch_pr * 2000
                goto L13490
L13486:     if stk_uom$ <> "CC" then L13490
                qty_oh = qty_oh / 3785
                purch_pr = purch_pr * 3785
L13490:     write #20 using L13520, vencode$, part$, po_no$, vbkseq$,     ~
                "1", receiver$, rcvdate$, " ", part_desc$, qty_oh,       ~
                purch_pr, orderdat$, store$
L13520:     FMT CH(9), CH(25), CH(16), CH(3), CH(1), CH(16), CH(6),      ~
                CH(16), CH(32), PD(14,4), PD(14,7), CH(6), CH(3)
            goto next_receiver_line

        payables
            call "SHOSTAT" ("Extracting invoices, building workfile")
            plowkey$ = lovencode$
            str(plowkey$, 10) = all(hex(00))
            call "PLOWNEXT" (#8, plowkey$, 0%, f1%(8))
            goto L13650

        next_payable
            call "READNEXT" (#8, f1%(8))
L13650:     if f1%(8) = 0 then purchase_orders
            get #8 using L13680, vencode$, invoice$, invdate$, vendate$
L13680:     FMT CH(9), CH(16), XX(16), CH(6), POS(87), CH(6)
            if vencode$ > hivencode$ then purchase_orders
            if fmdate$ = "ALL" then L13740
            if vendate$ < fmdate$ then next_payable
            if vendate$ > todate$ then next_payable

L13740:     plowkey$ = str(vencode$) & invoice$
            str(plowkey$, 26) = all(hex(00))

        next_payable_line
            call "PLOWNEXT" (#9, plowkey$, 25%, f1%(9))
            if f1%(9) = 0 then next_payable
            get #9 using L13820, receiver$, po_no$, vbkseq$, part$, v_qty,~
                unitpric
L13820:     FMT 2*CH(16), CH(3), POS(73), CH(25), PD(14,4), POS(131),    ~
                PD(14,7)
            if v_qty = 0 then next_payable_line    /* no quantity */
            gosub test_part_ok
            if part_ok% = 0% then next_payable_line
            if conv_uom$ = "NO" or f1%(3) = 0 then L13890
            if stk_uom$ <> "LBS" then L13886
                v_qty = v_qty / 2000
                unitpric = unitpric * 2000
                goto L13890
L13886:     if stk_uom$ <> "CC" then L13890
                v_qty = v_qty / 3785
                unitpric = unitpric * 3785
L13890:     write #20 using L13520, vencode$, part$, po_no$, vbkseq$,     ~
                "2", receiver$, vendate$, invoice$, " ", v_qty,          ~
                unitpric, " ", " "
            goto next_payable_line

        purchase_orders
            call "SHOSTAT" ("Extracting open purchase orders, building " ~
                & "workfile")
            plowkey$ = lovencode$
            str(plowkey$, 10) = all(hex(00))
            call "PLOWNEXT" (#10, plowkey$, 0%, f1%(10))
            goto L14040

        next_purchase_order
            call "READNEXT" (#10, f1%(10))
L14040:     if f1%(10) = 0 then sort_file
            get #10 using L14060, vencode$, po_no$, orderdat$, store$
L14060:     FMT CH(9), CH(16), POS(451), CH(6), POS(477), CH(3)
            if vencode$ > hivencode$ then sort_file
            if fmdate$ = "ALL" then L14110
            if orderdat$ < fmdate$ then next_purchase_order
            if orderdat$ > todate$ then next_purchase_order

L14110:     plowkey$ = str(vencode$) & po_no$
            str(plowkey$, 26) = all(hex(00))
        next_purchase_order_line
            call "PLOWNEXT" (#11, plowkey$, 25%, f1%(11))
            if f1%(11) = 0 then next_purchase_order
            get #11 using L14180, vbkseq$, part$, part_desc$, vq_out,     ~
                unitpric, status$
L14180:     FMT POS(26), CH(3), POS(32), CH(25), CH(32), POS(109),       ~
                PD(14,4), PD(14,7), POS(310), CH(1)
            if vq_out = 0 then next_purchase_order_line
            if status$ = "C" then next_purchase_order_line
            gosub test_part_ok
            if part_ok% = 0% then next_purchase_order_line
            if conv_uom$ = "NO" or f1%(3) = 0 then L14220
            if stk_uom$ <> "LBS" then L14216
                vq_out = vq_out / 2000
                unitpric = unitpric * 2000
                goto L14220
L14216:     if stk_uom$ <> "CC" then L14220
                vq_out = vq_out / 3785
                unitpric = unitpric * 3785
L14220:     write #20 using L13520, vencode$, part$, po_no$, vbkseq$,     ~
                "3", " ", " ", " ", part_desc$, vq_out, unitpric,        ~
                orderdat$, store$
            goto next_purchase_order_line

        test_part_ok
            part_ok% = 0%               /* exclude */
            if part$ < lopart$ then L17640
            if part$ > hipart$ then L17640
            if test_part% = 0% and conv_uom$ = "NO" then L17630
            call "READ100" (#3, part$, f1%(3))
	    if f1%(3) = 1 then L17575
	    if stock$ = "YES" then L17640
	    if fmcatcode$ = "ALL" and fmparttype$ = "ALL" then L17630    ~
	       else L17640
L17575:     get #3 using L17580, stk_uom$, catcode$, parttype$
L17580:     FMT POS(74), CH(4), POS(90), CH(4), POS(180), CH(3)
            if test_part% = 0% then L17630
            if catcode$ < locatcode$ then L17640
            if catcode$ > hicatcode$ then L17640
            if parttype$ < loparttype$ then L17640
            if parttype$ > hiparttype$ then L17640
L17630:     part_ok% = 1%               /* include */
L17640:     return

        sort_file
            call "SHOSTAT" ("Sorting workfile")
            call "GETNAMES" addr (#20, file$, lib$, vol$)
            close #20
            sort$ = str(file$) & str(lib$) & str(vol$)
            str(sort$, 23, 22) = str(sort$, 1, 22)
            str(sort$, 45, 9) = "0001053CA"
            str(sort$, 54, 9) = "0055016CA"
            str(sort$, 63, 9) = "0054001CA"
            call "SORTCALL" addr (sort$, ret%)
            if ret% = 0 then L18990
               call "SHOSTAT" ("Error sorting workfile, exiting program")
               goto exit_program
L18990:     goto generate_report

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Date Range             */~
                              L20200,         /* Vendor Code            */~
                              L20300,         /* Part Type              */~
                              L20400,         /* Part Category          */~
                              L20500,         /* Part Number            */~
                              L20600,         /* Summary or Detail      */~
                              L20700,         /* Extended or unit cost  */~
                              L20800,         /* Cost set to use        */~
                              L20900,         /* Convert unit of measure*/~
			      L20950
            return
L20100: REM Def/Enable Date Range                  FMDATE$
            if fmdate$             = " " then                            ~
               fmdate$             = "ALL"
            return

L20200: REM Def/Enable Vendor Code                 FMVENCODE$
            if fmvencode$          = " " then                            ~
               fmvencode$          = "ALL"
            return

L20300: REM Def/Enable Part Type                   FMPARTTYPE$
            if fmparttype$         = " " then                            ~
               fmparttype$         = "ALL"
            return

L20400: REM Def/Enable Part Category               FMCATCODE$
            if fmcatcode$          = " " then                            ~
               fmcatcode$          = "ALL"
            return

L20500: REM Def/Enable Part Number                 FMPART$
            if fmpart$             = " " then                            ~
               fmpart$             = "ALL"
            return

L20600: REM Def/Enable Summary or Details          SUM_DTL$
            if sum_dtl$            = " " then                            ~
               sum_dtl$            = "D"
            return

L20700: REM Def/Enable Extended or Unit Cost       UNIT_COST$
            if unit_cost$          = " " then                            ~
               unit_cost$          = "E"
            if sum_dtl$ = "S" then unit_cost$ = "E"
            return

L20800: REM Def/Enable Cost Set to Use             COST_SET$
            if cost_set$           = " " then                            ~
               cost_set$           = "NONE"
            return

L20900: REM Def/Enable Convert unit of measures    CONV_UOM$
            if conv_uom$           = " " then                            ~
               conv_uom$           = "NO"
            return

L20950: REM Def/Enable Stock Items only            STOCK$
            if stock$              = " " then                            ~
               stock$              = "YES"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Date Range                                             ",~
         "Enter Vendor Code       ? - for valid codes                  ",~
         "Enter Part Type         ? - for valid codes                  ",~
         "Enter Part Category     ? - for valid codes                  ",~
         "Enter Part Number       ? - for valid codes                  ",~
         "Enter Summary or Details   'S' or 'D'                        ",~
         "Enter 'E' for Extended Cost or 'U' for Unit Cost             ",~
         "Enter Cost Set to Use for Comparison, or 'NONE'              ",~
         "Enter Convert LBS/CC to TONS/GAL    'YES' or 'NO'            ",~
	 "Enter Include Inventory Items Only  'YES' or 'NO'            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, unit_cost$, cost_set$,     ~
                      fmcatcode$, fmdate$, fmpart$, fmparttype$,         ~
                      fmvencode$, hicatcode$, set_desc$, conv_uom$,      ~
                      hipart$, hiparttype$, hivencode$,                  ~
                      locatcode$, lopart$, loparttype$, stock$,           ~
                      lovencode$, tocatcode$, todate$,                   ~
                      topart$, toparttype$, sum_dtl$, tovencode$
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
            select printer(160)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("ZVBK03", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

            call "WORKOPN2" (#20, "INPUT", 0%, f2%(20))
            if f2%(20) = 0 then L30175
            call "SHOSTAT" ("Error re-opening workfile, exiting program")
            goto exit_program

L30175:     call "SHOSTAT" ("Printing Vendor Purchase History")
            part_desc$ = " "
            save_part$ = all(hex(ff))
            old_vencode$ = " "
            old_part$ = " "
            old_po_no$ = " "
            old_vbkseq$ = " "
            old_receiver$ = " "

            mat sub_cost = zer
            mat sub_std  = zer
            mat sub_qty  = zer
            mat tot_cost = zer
            mat tot_std  = zer
            sub_eqv = 0
            sub_wgt = 0
            tot_eqv = 0
            tot_wgt = 0
            multi_inv% = 0%      /* multiple invoice flag */

        next_work_record
            call "READNEXT" (#20, f1%(20))
            if f1%(20) = 0 then new_vendor
            get #20 using L13520, vencode$, part$, po_no$, vbkseq$,       ~
                type$, receiver$

*        check for break - subtotal
            if vencode$ <> old_vencode$ then gosub new_vendor
            if part$ <> old_part$ then gosub new_part
            if po_no$ <> old_po_no$ or vbkseq$ <> old_vbkseq$ or         ~
                receiver$ <> old_receiver$ then gosub new_line

            on pos("123" = type$) goto receiver, payable, purchase_order
            goto next_work_record

        receiver
            get #20 using L30540, rcvdate$, qty_oh, unitpric_rcv,         ~
                orderdat$, store$
L30540:     FMT POS(71), CH(6), XX(16), XX(32), PD(14,4), PD(14,7),      ~
                CH(6), CH(3)
            if part_desc$ = " " then get #20 using L30570, part_desc$
L30570:     FMT POS(93), CH(32)
            rcv_cost = round(qty_oh * unitpric_rcv, 2)
            goto next_work_record

        payable
            if invoice$ = " " then L30620
                multi_inv% = 1%         /* set multiple flag */
                gosub new_line
                multi_inv% = 0%         /* clear flag        */
L30620:     get #20 using L30630, vendate$, invoice$, v_qty, unitpric_ap
L30630:     FMT POS(71), CH(6), CH(16), XX(32), PD(14,4), PD(14,7)
            if part_desc$ = " " then get #20 using L30570, part_desc$
            ven_cost = round(v_qty * unitpric_ap, 2)
            goto next_work_record

        purchase_order
            get #20 using L30700, vq_out, unitpric_po, orderdat$, store$
L30700:     FMT POS(125), PD(14,4), PD(14,7), CH(6), CH(3)
            if part_desc$ = " " then get #20 using L30570, part_desc$
            po_cost = round(vq_out * unitpric_po, 2)
            goto next_work_record

        new_line
            ext_wgt, ext_eqv = 0
            if old_part$ = save_part$ then L30910
            wgt, eqv, std = 0
            call "READ100" (#3, old_part$, f1%(3))
            if f1%(3) = 0 then L30960
            get #3 using L30820, stk_uom$, wgt, equiv$
L30820:     FMT POS(74), CH(4), POS(287), PD(14,4), POS(570), CH(7)
            if part_desc$ = " " then get #3 using L30840, part_desc$
L30840:     FMT POS(26), CH(32)
            if cost_set$ = "NONE" then L30870
                call "STCCOSTS" (old_part$, cost_set$, #1, 1%, std)
                if std = 0 then L30870
                if conv_uom$ = "NO" then L30870
                if stk_uom$ = "LBS" then std = std * 2000
                if stk_uom$ = "CC"  then std = std * 3785
L30870:     save_part$ = old_part$    /* Save last part number read */

            convert equiv$ to eqv, data goto L30910
L30910:     ext_wgt = wgt * qty_oh
            ext_eqv = eqv * qty_oh

            if ln% <> 0% or sum_dtl$ = "S" then L30980
                if lcntr% > 55% then gosub page_head
L30960:         print using L60250, old_part$, part_desc$
                lcntr% = lcntr% + 1%
L30980:     ln% = ln% + 1%   /* number of part lines processed */
            if sum_dtl$ = "S" then L31320
            qty$() = " "
            cost$() = " "
            wgt$, eqv$ = " "
            if qty_oh <> 0 then call "CONVERT" (qty_oh, 0.4, qty$(1))
            if v_qty  <> 0 then call "CONVERT" (v_qty,  0.4, qty$(2))
            if vq_out <> 0 then call "CONVERT" (vq_out, 0.4, qty$(3))
            if unit_cost$ = "U" then L31140 /* Unit costs */
            if rcv_cost <> 0 then                                        ~
                call "CONVERT" (rcv_cost, 2.2, cost$(1))
            if ven_cost <> 0 then                                        ~
                call "CONVERT" (ven_cost, 2.2, cost$(2))
            if po_cost <> 0 then                                         ~
                call "CONVERT" (po_cost, 2.2, cost$(3))
            goto L31200
L31140:     if unitpric_rcv <> 0 then                                    ~
                call "CONVERT" (unitpric_rcv, 2.7, cost$(1))
            if unitpric_ap <> 0 then                                     ~
                call "CONVERT" (unitpric_ap, 2.7, cost$(2))
            if unitpric_po <> 0 then                                     ~
                call "CONVERT" (unitpric_po, 2.7, cost$(3))
L31200:     if ext_wgt <> 0 then call "CONVERT" (ext_wgt, 0.1, wgt$)
            if ext_eqv <> 0 then call "CONVERT" (ext_eqv, 0.1, eqv$)
            if orderdat$ <> " " then call "DATEFMT" (orderdat$)
            if rcvdate$  <> " " then call "DATEFMT" (rcvdate$)

            if lcntr% > 56% then gosub page_head
            print using L60200, orderdat$, old_po_no$, old_vbkseq$,       ~
                old_receiver$, rcvdate$, store$, invoice$, qty$(1),      ~
                cost$(1), qty$(2), cost$(2), qty$(3), cost$(3), wgt$,    ~
                eqv$
            lcntr% = lcntr% + 1%

L31320:     sub_qty(1)  = sub_qty(1)  + qty_oh
            sub_qty(2)  = sub_qty(2)  + v_qty
            sub_qty(3)  = sub_qty(3)  + vq_out
            sub_cost(1) = sub_cost(1) + rcv_cost
            sub_cost(2) = sub_cost(2) + ven_cost
            sub_cost(3) = sub_cost(3) + po_cost
            sub_wgt     = sub_wgt     + ext_wgt
            sub_eqv     = sub_eqv     + ext_eqv

L31410:     qty_oh, v_qty, vq_out = 0
            rcv_cost, ven_cost, po_cost = 0
            unitpric_rcv, unitpric_ap, unitpric_po = 0
            invoice$ = " "
            if multi_inv% = 0% then L31470
                if orderdat$ <> " " then call "DATUNFMT" (orderdat$)
                if rcvdate$  <> " " then call "DATUNFMT" (rcvdate$)
                goto L31480
L31470:     orderdat$ = " "
            rcvdate$ = " "
L31480:     old_receiver$ = receiver$
            old_po_no$    = po_no$
            old_vbkseq$   = vbkseq$
            return

        new_part
            gosub new_line
           if ln% = 1% and sum_dtl$ = "D" and unit_cost$ = "E" then L31840
            qty$() = " "
            cost$() = " "
            wgt$, eqv$ = " "
            if sub_qty(1) <> 0 then                                      ~
                call "CONVERT" (sub_qty(1), 0.4, qty$(1))
            if sub_qty(2) <> 0 then                                      ~
                call "CONVERT" (sub_qty(2), 0.4, qty$(2))
            if sub_qty(3) <> 0 then                                      ~
                call "CONVERT" (sub_qty(3), 0.4, qty$(3))
            if sub_cost(1) <> 0 then                                     ~
                call "CONVERT" (sub_cost(1), 2.2, cost$(1))
            if sub_cost(2) <> 0 then                                     ~
                call "CONVERT" (sub_cost(2), 2.2, cost$(2))
            if sub_cost(3) <> 0 then                                     ~
                call "CONVERT" (sub_cost(3), 2.2, cost$(3))
            if sub_wgt <> 0 then call "CONVERT" (sub_wgt, 0.1, wgt$)
            if sub_eqv <> 0 then call "CONVERT" (sub_eqv, 0.1, eqv$)

            if lcntr% > 55% and sum_dtl$ = "S" then gosub page_head
            if sum_dtl$ = "D" then                                       ~
            print using L60200, " ", " ", " ", " ", " ", " ",             ~
                "Part Subtotals", qty$(1), cost$(1), qty$(2),            ~
                cost$(2), qty$(3), cost$(3), wgt$, eqv$                  ~
            else                                                         ~
            print using L60280, old_part$, part_desc$, qty$(1), cost$(1), ~
                qty$(2), cost$(2), qty$(3), cost$(3), wgt$, eqv$
            lcntr% = lcntr% + 1%

L31840:     if cost_set$ = "NONE" then L32040
                call "CONVERT" (std, 4.4, std$)
                cost$() = " "
                for i% = 1% to 3%
                sub_std(i%) = sub_qty(i%) * std
                if sub_std(i%) <> 0 then                                 ~
                    call "CONVERT" (sub_std(i%) , 2.2, cost$(i%))
                next i%
                print using L60330, "Cost at Standard of", std$, cost$(1),~
                     cost$(2), cost$(3)

                mat sub_qty = sub_std - sub_cost   /* QTY temp work */
                cost$() = " "
                for i% = 1% to 3%       /* variance */
                if sub_qty(i%) <> 0 then                                 ~
                    call "CONVERT" (sub_qty(i%), 2.2, cost$(i%))
                next i%
                print using L60330, "Variance from Standard", " ",        ~
                     cost$(1), cost$(2), cost$(3)
                lcntr% = lcntr% + 2%

                mat tot_std  = tot_std  + sub_std
                mat sub_std = zer

L32040:     mat tot_cost = tot_cost + sub_cost
            tot_wgt = tot_wgt + sub_wgt
            tot_eqv = tot_eqv + sub_eqv

            mat sub_qty = zer
            mat sub_cost = zer
            sub_wgt, sub_eqv = 0

            print              /* Space between parts  */
            lcntr% = lcntr% + 1%

L32150:     old_part$ = part$
            ln% = 0%           /* print lines for part */
            part_desc$ = " "
            return

        new_vendor
            if old_vencode$ <> " " then L32250
                gosub L31410
                gosub L32150
                goto L32670
L32250:     gosub new_part
            cost$() = " "
            wgt$, eqv$ = " "
            if tot_cost(1) <> 0 then                                     ~
                call "CONVERT" (tot_cost(1), 2.2, cost$(1))
            if tot_cost(2) <> 0 then                                     ~
                call "CONVERT" (tot_cost(2), 2.2, cost$(2))
            if tot_cost(3) <> 0 then                                     ~
                call "CONVERT" (tot_cost(3), 2.2, cost$(3))
            if tot_wgt <> 0 then call "CONVERT" (tot_wgt, 0.1, wgt$)
            if tot_eqv <> 0 then call "CONVERT" (tot_eqv, 0.1, eqv$)

            print
            print using L60200, " ", " ", " ", " ", " ", " ",             ~
                "Vendor Totals", " ", cost$(1), " ", cost$(2), " ",      ~
                cost$(3), wgt$, eqv$
            lcntr% = lcntr% + 2%

            if cost_set$ = "NONE" then L32610
                std$ = " "
                cost$() = " "
                for i% = 1% to 3%
                if tot_std(i%) <> 0 then                                 ~
                    call "CONVERT" (tot_std(i%) , 2.2, cost$(i%))
                next i%
                print using L60330, "Cost at Standard", std$, cost$(1),   ~
                     cost$(2), cost$(3)

                mat tot_std = tot_std - tot_cost
                cost$() = " "
                for i% = 1% to 3%       /* variance */
                if tot_std(i%) <> 0 then                                 ~
                    call "CONVERT" (tot_std(i%), 2.2, cost$(i%))
                next i%
                print using L60330, "Variance from Standard", " ",        ~
                     cost$(1), cost$(2), cost$(3)
                lcntr% = lcntr% + 2%

                mat tot_std = zer

L32610:     mat tot_cost = zer
            tot_wgt, tot_eqv = 0

            print              /* Space between vendors */
            lcntr% = lcntr% + 1%

L32670:     if f1%(20) = 0 then end_report
            old_vencode$ = vencode$
            call "DESCRIBE" (#2, vencode$, vendname$, 0%, f1%(2))
            if lcntr% > 55% then gosub page_head
            print using L60250, "         Vendor: " & vencode$, vendname$
            lcntr% = lcntr% + 1%
            return

        end_report                /* Report Ending Routine */
            print skip(2)
            print using L64990     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
        REM Get rid of workfiles
            call "FILEBGON" (#20)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "VENPURRP"
            print using L60110, rpttitle$, pcntr%
            print
            print using L60150
            print
            lcntr% = 5%
            return

        print_params           /* Print Page Zero */
            print page
L34520:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L34540
                str(i$(), i%, 1%) = hex(20)
                goto L34520
L34540:     print using L64980, rpttitle$
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 19% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

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
              on fieldnr% gosub L40115,         /* Date Range        */   ~
                                L40115,         /* Vendor Code       */   ~
                                L40115,         /* Part Type         */   ~
                                L40115,         /* Part Category     */   ~
                                L40115,         /* Part Number       */   ~
                                L40115,         /* Summary or Detail */   ~
                                L40115,         /* Extended-unit cost*/   ~
                                L40115,         /* Cost Set to Use   */   ~
                                L40115,         /* Convert unit meas.*/   ~
			        L40115		/* Inventory only    */
              goto L40130

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40115:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40130:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Date Range",                                 ~
               at (07,30), fac(lfac$( 1)), fmdate$              , ch(10),~
               at (07,56), fac(lfac$( 1)), todate$              , ch(10),~
                                                                         ~
               at (08,02), "Vendor Code",                                ~
               at (08,30), fac(lfac$( 2)), fmvencode$           , ch(09),~
               at (08,56), fac(lfac$( 2)), tovencode$           , ch(09),~
                                                                         ~
               at (09,02), "Part Type",                                  ~
               at (09,30), fac(lfac$( 3)), fmparttype$          , ch(03),~
               at (09,56), fac(lfac$( 3)), toparttype$          , ch(03),~
                                                                         ~
               at (10,02), "Part Category",                              ~
               at (10,30), fac(lfac$( 4)), fmcatcode$           , ch(04),~
               at (10,56), fac(lfac$( 4)), tocatcode$           , ch(04),~
                                                                         ~
               at (11,02), "Part Number",                                ~
               at (11,30), fac(lfac$( 5)), fmpart$              , ch(25),~
               at (11,56), fac(lfac$( 5)), topart$              , ch(25),~
                                                                         ~
               at (12,02), "Summary or Details",                         ~
               at (12,30), fac(lfac$( 6)), sum_dtl$             , ch(01),~
                                                                         ~
               at (13,02), "Extended or Unit Cost",                      ~
               at (13,30), fac(lfac$( 7)), unit_cost$           , ch(01),~
                                                                         ~
               at (14,02), "Cost Set to Use",                            ~
               at (14,30), fac(lfac$( 8)), cost_set$            , ch(08),~
               at (14,40), fac(hex(8c)), set_desc$              , ch(32),~
                                                                         ~
               at (15,02), "Convert LBS-CC to TONS-GAL",                 ~
               at (15,30), fac(lfac$( 9)), conv_uom$            , ch(03),~
                                                                         ~
               at (16,02), "Inc. Inventory Parts Only",                  ~
               at (16,30), fac(lfac$(10)), stock$               , ch(03),~
                                                                         ~
               at (18,20), "Note - caution on date range selection.",    ~
               at (19,20), "If a cost set is selected, it will apply to",~
               at (20,20), "all transactions selected.",                 ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40400
                  call "MANUAL" ("VENPURRP") : goto L40130

L40400:        if keyhit% <> 15 then L40415
                  call "PRNTSCRN" : goto L40130

L40415:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40510     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40495
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40500
L40495:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40500:     return

L40510: if fieldnr% > 0% then L40555  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40555:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Date Range             */~
                              L50200,         /* Vendor Code            */~
                              L50300,         /* Part Type              */~
                              L50400,         /* Part Category          */~
                              L50500,         /* Part Number            */~
                              L50600,         /* Summary or Detail      */~
                              L50700,         /* Extended or Unit Cost  */~
                              L50800,         /* Cost Set to Use        */~
                              L50900,         /* Convert unit of measure*/~
			      L50950          /* Inventory Parts Only   */
            return
L50100: REM Test for Date Range                   FMDATE$
            if fmdate$ <> "ALL" then L50110
                todate$ = " "
                fmdate% = 00000101%    :  todate% = 99991231%
                goto L50190
L50110:     call "DATEOKC" (fmdate$, fmdate%, errormsg$)
            if errormsg$ <> " " then L50190
            if todate$ = " " then todate$ = fmdate$
            call "DATEOKC" (todate$, todate%, errormsg$)
            if errormsg$ <> " " then L50190
            if fmdate% <= todate% then L50190
            errormsg$ = "Beginning date can not be greater than the end" ~
                & "ing date"
L50190:     return

L50200: REM Test for Vendor Code                  FMVENCODE$
            call "TESTRNGE"                                              ~
                  (fmvencode$          , tovencode$          ,           ~
                   lovencode$          , hivencode$          ,           ~
                   errormsg$, #2)
            return

L50300: REM Test for Part Type                    FMPARTTYPE$
            call "TESTRNGE"                                              ~
                  (fmparttype$         , toparttype$         ,           ~
                   loparttype$         , hiparttype$         ,           ~
                   errormsg$, #5, "PARTTYPE ")
            return

L50400: REM Test for Part Category                FMCATCODE$
            call "TESTRNGE"                                              ~
                  (fmcatcode$          , tocatcode$          ,           ~
                   locatcode$          , hicatcode$          ,           ~
                   errormsg$, #4)
            return

L50500: REM Test for Part Number                  FMPART$
            call "TESTRNGE"                                              ~
                  (fmpart$             , topart$             ,           ~
                   lopart$             , hipart$             ,           ~
                   errormsg$, #3)
            return

L50600: REM Test for Summary or Details           SUM_DTL$
            if sum_dtl$ <> "S" and sum_dtl$ <> "D" then L50610
            if sum_dtl$ = "S" then unit_cost$ = "E"
            goto L50690
L50610:     errormsg$ = "Please enter 'S' or 'S'"
L50690:     return

L50700: REM Test for Extended or Unit Cost        UNIT_COST$
            if unit_cost$ <> "E" and unit_cost$ <> "U" then L50724
            if sum_dtl$ = "S" then unit_cost$ = "E"
            goto L50790
L50724:     errormsg$ = "Please enter 'E' or 'U'"
L50790:     return

L50800: REM Test for Cost Set to Use              COST_SET$
            if cost_set$ <> "NONE" then L50840
                set_desc$ = "No Standard Costs"
                goto L50890
L50840:     plowkey$ = "STC.HDR." & cost_set$
            set_desc$ = "Select Cost Set"
            call "PLOWCODE" (#1, plowkey$, set_desc$, 8%, 0.3, f1%(1))
            if f1%(1) = 1 then L50870
                errormsg$ = "Cost set " & cost_set$ & " not found"
                return
L50870:     cost_set$ = str(plowkey$, 9, 8)
            call "PUTPAREN" (set_desc$)
L50890:     return

L50900: REM Test for Convert unit of measure      CONV_UOM$
            if str(conv_uom$, 1, 1) = "Y" then conv_uom$ = "YES"
            if str(conv_uom$, 1, 1) = "N" then conv_uom$ = "NO"
            if conv_uom$ = "YES" or conv_uom$ = "NO" then L50940
            errormsg$ = "Please answer 'YES' or 'NO'"
L50940:     return

L50950: REM Test for Inventory Items Only         STOCK$
            if str(stock$, 1, 1) = "Y" then stock$ = "YES"
            if str(stock$, 1, 1) = "N" then stock$ = "NO"
            if stock$ = "YES" or stock$ = "NO" then L50990
            errormsg$ = "Please answer 'YES' or 'NO'"
L50990:     return

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

*       * Column Headings
L60150: % Ordered Purchase Order   Seq Receiver         Rcv-Date STR Invo~
        ~ice           Qty-Rcv   Cost-Rcv   Qty-AP    Cost-AP   Qty-PO    ~
        ~Cost-PO      Weight  9" Equiv

*       * Detail Lines
L60200: %######## ################ ### ################ ######## ### ####~
        ~############ ######## ########## ######## ########## ######## ###~
        ~####### ########### #########

*       * Vendor - Part headings
L60250: %#########################  ################################

*       * Detail lines - summary report
L60280: %#########################  ################################     ~
        ~             ######## ########## ######## ########## ######## ###~
        ~####### ########### #########

*       * Standard cost and variance
L60330: %                                           #####################~
        ~# #########           ##########          ##########          ###~
        ~#######

        %** Report Title for page 0
L64980: %FORM (003)   ###################################################~
        ~#############

L64990: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
