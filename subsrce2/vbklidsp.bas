        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  BBBB   K   K  L      IIIII  DDDD    SSS   PPPP    *~
            *  V   V  B   B  K  K   L        I    D   D  S      P   P   *~
            *  V   V  BBBB   KKK    L        I    D   D   SSS   PPPP    *~
            *   V V   B   B  K  K   L        I    D   D      S  P       *~
            *    V    BBBB   K   K  LLLLL  IIIII  DDDD    SSS   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKLIDSP - This subroutine displays PO line item info.    *~
            *            Prices are NOT displayed.                      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01-24-91 ! Original                                 ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VBKLIDSP" (vendor$, po$, seq$, #01, #03, #04)

        dim                                                              ~
            conv_factor$10,              /* Qty per Vendor UOM         */~
            cur_due$8,                   /* Current Due Date           */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            inpmessage$79,               /* Informational Message      */~
            last_recd$8,                 /* Last Received Date         */~
            line2$79,                    /* Screen Line #2             */~
            lot$6,                       /* Lot                        */~
            next_ship$8,                 /* Next Shipment Date         */~
            not_before$8,                /* Don't receive before date  */~
            not_after$8,                 /* Don't receive after date   */~
            orig_due$8,                  /* Original Due Date          */~
            orig_qty$10,                 /* Original Order Qty         */~
            part$25,                     /* Part Number                */~
            part_cat$4,                  /* Part Category              */~
            part_descr$34,               /* Part Description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16,                       /* Purchase Order Number      */~
            recd_qty$10,                 /* Total Rec to Date          */~
            remain_qty$10,               /* Remaining On Order         */~
            seq$3,                       /* Purchase Order Line Item # */~
            status$1,                    /* Line Item Status           */~
            status_descr$16,             /* Line Item Status Descriptn */~
            store$3,                     /* Store                      */~
            store_descr$32,              /* Store Name Description     */~
            uom$4,                       /* Stocking UOM               */~
            userid$3,                    /* Current User Id            */~
            ven_org_qty$10,              /* Original Qty in Vendor UOM */~
            ven_part$25,                 /* Vendor Part #              */~
            ven_rec_qty$10,              /* Received Qty in Vendor UOM */~
            ven_rem_qty$10,              /* Remaining Qty in Vendor UOM*/~
            ven_uom$4,                   /* Vendor Shipping UOM        */~
            vendor$9                     /* Vendor                     */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            * #01 ! VBKLINES ! Purchase Order Line Items File           *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! STORNAME ! STORE INFORMATION FILE                   *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "Press (RETURN) or PF16 to Return to " &      ~
                           "Previous Screen."

            str(line2$,62) = "VBKLIDSP: " & str(cms2v$,,8)
            str(line2$,01,07) = "Vendor:"
            str(line2$,09,09) = vendor$
            str(line2$,19,11) = "P.O.Number:"
            str(line2$,31,16) = po$
            str(line2$,48,08) = "PO Line:"
            str(line2$,57,03) = seq$

        REM *************************************************************~
            *       M A I N   L O G I C - M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles calls to collect & display data.                  *~
            *************************************************************

            gosub initialize_variables
            gosub dataload
L10100:     gosub'101

            if keyhit% = 16% then end
            if keyhit% <> 0% then L10100
            end

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      conv_factor$, cur_due$, last_recd$, lot$,          ~
                      next_ship$, orig_due$, orig_qty$, part$,           ~
                      part_cat$, recd_qty$, remain_qty$, status$,        ~
                      store$, uom$, ven_part$, not_before$, not_after$,  ~
                      part_descr$, store_descr$, ven_uom$, status_descr$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            plowkey$ = str(vendor$) & str(po$) & seq$
            call "READ100" (#01, plowkey$, f1%(1))
            if f1%(1) <> 1% then no_data
            get #01 using L35030, part$, part_descr$, part_cat$, orig_qty,~
                                 recd_qty, remain_qty, cur_due$,         ~
                                 last_recd$, next_ship$, lot$, job$,     ~
                                 store$, ven_part$, ven_uom$,            ~
                                 conv_factor, orig_due$, status$,        ~
                                 not_before$, not_after$

            call "PUTPAREN" (part_descr$)

            call "DATEFMT" (cur_due$)
            call "DATEFMT" (last_recd$)
            call "DATEFMT" (next_ship$)
            call "DATEFMT" (orig_due$)
            call "DATEFMT" (not_before$)
            call "DATEFMT" (not_after$)

            call "CONVERT" (orig_qty,    -0.4, orig_qty$)
            call "CONVERT" (recd_qty,    -0.4, recd_qty$)
            call "CONVERT" (remain_qty,  -0.4, remain_qty$)
            call "CONVERT" (conv_factor, -0.4, conv_factor$)

            ven_org_qty = round(orig_qty  /  conv_factor, 4)
            ven_rec_qty = round(recd_qty  /  conv_factor, 4)
            ven_rem_qty = round(remain_qty / conv_factor, 4)

            call "CONVERT" (ven_org_qty, -0.4, ven_org_qty$)
            call "CONVERT" (ven_rec_qty, -0.4, ven_rec_qty$)
            call "CONVERT" (ven_rem_qty, -0.4, ven_rem_qty$)

            if status$ = "A" then status_descr$ = "(Active)        "
            if status$ = "C" then status_descr$ = "(Closed)        "
            if status$ = "H" then status_descr$ = "(On Hold)       "
            if status$ = "W" then status_descr$ = "(Awaiting Price)"

            call "DESCRIBE" (#04, store$, store_descr$, 1%, f1%(4))
            if f1%(4) = 0% then store_descr$ = "(Store Not on File!)"

            uom$ = "????"
            call "READ100" (#03, part$, f1%(3))
            if f1%(3) = 1% then get #03 using L30490, uom$
L30490:         FMT POS(74), CH(4)

            return

        no_data
L30610:     u3% = 2%
            call "ASKUSER" (u3%, "*** NO DATA ***",  " ",                ~
                            "No Line Item Info on File; Press RETURN " & ~
                            "to acknowledge.", " ")
            if u3% <> 0% then L30610
            end

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: VBKLINES                          */~
            XX(9),          /* Vendor Code                             */~
            XX(16),         /* Purchase Order Number                   */~
            XX(3),          /* Purchase Line Sequence Number (not ITEM */~
            XX(3),          /* vbklines item number                    */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* part number description                 */~
            CH(4),          /* category code                           */~
            PD(14,4),       /* quantity originally ordered             */~
            PD(14,4),       /* Total Quantity Received To Date - Purcha*/~
            PD(14,4),       /* quantity outstanding on order           */~
            XX(8),          /* Unit Price                              */~
            XX(8),          /* Extension Amount (Quantity * Price)     */~
            XX(9),          /* Purchases Account Number                */~
            CH(6),          /* P.O. Line Item Due Date                 */~
            CH(6),          /* P.O. Line Item Date of Last Receipt -Las*/~
            CH(6),          /* date of receipt of next shipment        */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Job Number                              */~
            CH(3),          /* Store or Warehouse Code                 */~
            XX(1),          /* on-hand posting option                  */~
            XX(3),          /* Type - used generically for special desc*/~
            XX(16),         /* Packing Slip # for last quantity receive*/~
            CH(25),         /* Vendors Part Number for purchase order. */~
            XX(66),         /* filler for rest of record or internal sp*/~
            CH(04),         /* Unit of Measure                         */~
            PD(14,4),       /* Units of Something                      */~
            CH(6),          /* Original Due Date                       */~
            XX(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* General purpose status indicator for Mgt*/~
            XX(2),          /* Number of Times a Field or Record has be*/~
            XX(20),         /* Who is to receive merchandise.          */~
            XX(20),         /* Person requesting merchandise.          */~
            XX(5),          /* User assigned reference number          */~
            CH(6),          /* Do not receive before date              */~
            CH(6)           /* Do not receive after date.              */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101
              gosub set_pf1

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Purchase Order Line Item Display",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Part Number",                                ~
               at (04,16), fac(hex(84)),   part$                , ch(25),~
               at (04,42), fac(hex(84)),   part_descr$          , ch(34),~
                                                                         ~
               at (05,02), "Part Category",                              ~
               at (05,16), fac(hex(84)),   part_cat$            , ch(04),~
                                                                         ~
               at (06,02), "Store",                                      ~
               at (06,16), fac(hex(84)),   store$               , ch(03),~
               at (06,42), fac(hex(84)),   store_descr$         , ch(32),~
                                                                         ~
               at (07,02), "Lot",                                        ~
               at (07,16), fac(hex(84)),   lot$                 , ch(06),~
                                                                         ~
               at (07,27), "Job",                                        ~
               at (07,31), fac(hex(84)),   job$                 , ch(08),~
                                                                         ~
               at (08,02), "Vendor Part #",                              ~
               at (08,16), fac(hex(84)),   ven_part$            , ch(25),~
                                                                         ~
               at (09,02), "Quantities:             INTERNAL",           ~
               at (09,52), "VENDOR",                                     ~
                                                                         ~
               at (10,14), "Stocking UOM",                               ~
               at (10,28), fac(hex(84)),   uom$                 , ch(04),~
                                                                         ~
               at (10,39), "Shipping UOM",                               ~
               at (10,53), fac(hex(84)),   ven_uom$             , ch(04),~
                                                                         ~
               at (11,08), "Qty per Vendor UOM",                         ~
               at (11,28), fac(hex(84)),   conv_factor$         , ch(10),~
                                                                         ~
               at (12,08), "Original Order Qty",                         ~
               at (12,28), fac(hex(84)),   orig_qty$            , ch(10),~
               at (12,53), fac(hex(84)),   ven_org_qty$         , ch(10),~
                                                                         ~
               at (13,08), "Remaining On Order",                         ~
               at (13,28), fac(hex(84)),   remain_qty$          , ch(10),~
               at (13,53), fac(hex(84)),   ven_rem_qty$         , ch(10),~
                                                                         ~
               at (14,08), "Total Rec to Date",                          ~
               at (14,28), fac(hex(84)),   recd_qty$            , ch(10),~
               at (14,53), fac(hex(84)),   ven_rec_qty$         , ch(10),~
                                                                         ~
               at (15,02), "Line Item Status",                           ~
               at (15,22), fac(hex(84)),   status$              , ch(01),~
               at (15,25), fac(hex(84)),   status_descr$        , ch(16),~
                                                                         ~
               at (16,02), "Current Due Date",                           ~
               at (16,22), fac(hex(84)),   cur_due$             , ch(08),~
                                                                         ~
               at (16,33), "Not Before",                                 ~
               at (16,44), fac(hex(84)),   not_before$          , ch(08),~
                                                                         ~
               at (16,55), "Not After",                                  ~
               at (16,65), fac(hex(84)),   not_after$           , ch(08),~
                                                                         ~
               at (17,02), "Original Due Date",                          ~
               at (17,22), fac(hex(84)),   orig_due$            , ch(08),~
                                                                         ~
               at (18,02), "Next Shipment Date",                         ~
               at (18,22), fac(hex(84)),   next_ship$           , ch(08),~
                                                                         ~
               at (19,02), "Last Received Date",                         ~
               at (19,22), fac(hex(84)),   last_recd$           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40920
                  call "MANUAL" ("VBKLIDSP") : goto L40090

L40920:        if keyhit% <> 15 then L40950
                  call "PRNTSCRN" : goto L40090

L40950:         return

        set_pf1
            pf$(1) = "(RETURN)Return                          " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)RETURN      "
            pfkeys$ = hex(ffffffffffffffffffffffff0dff0f1000)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

            end
