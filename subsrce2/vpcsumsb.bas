        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP    CCCC   SSS   U   U  M   M   SSS   BBBB    *~
            *  V   V  P   P  C      S      U   U  MM MM  S      B   B   *~
            *  V   V  PPPP   C       SSS   U   U  M M M   SSS   BBBB    *~
            *   V V   P      C          S  U   U  M   M      S  B   B   *~
            *    V    P       CCCC   SSS    UUU   M   M   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPCSUMSB - Sums Purchasing Contract and line (if          *~
            *            specified) PO and PAY info.                    *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/27/94 ! Original (w/help from LDJ)               ! ERN *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VPCSUMSB"                   /* SUMMARIZE VPC VBK/PAY INFO */~
                   ( #1,                 /* VPCMASTR channel           */~
                     #3,                 /* VBKLINES channel           */~
                     #4,                 /* PAYLINES channel           */~
                     arg_contract_id$,   /* Contract to summarized     */~
                     arg_contract_line$, /* Line to be  summarized     */~
                                         /*   (" " => hdr level)       */~
                                         /*                            */~
                                         /* *** HEADER LEVEL INFO ***  */~
                     hdr%,               /*  HDR Exists? 0=No, 1=Yes   */~
                     vpc_min_dlr,        /*  Minimum Dollars           */~
                     vpc_max_dlr,        /*  Maximum Dollars           */~
                     vpc_start$,         /*  Start Date                */~
                     vpc_end$,           /*  End   Date                */~
                     vpc_vbk_dlr,        /*  PO  Dollars (Uninvoiced)  */~
                     vpc_pay_dlr,        /*  A/P Dollars (Invoiced)    */~
                                         /*                            */~
                                         /* **** LINE LEVEL INFO ****  */~
                     lin%,               /*  Line Exists? 0=No, 1=Yes  */~
                     lin_min_dlr,        /*  Minimum Dollars           */~
                     lin_max_dlr,        /*  Maximum Dollars           */~
                     lin_min_qty,        /*  Minimum Qty               */~
                     lin_max_qty,        /*  Maximum Qty               */~
                     lin_start$,         /*  Start Date                */~
                     lin_end$,           /*  End   Date                */~
                     lin_vbk_qty,        /*  PO  Quantity (UnInvoiced) */~
                     lin_vbk_dlr,        /*  PO  Dollars  (UnInvoiced) */~
                     lin_pay_qty,        /*  A/P Quantity (Invoiced)   */~
                     lin_pay_dlr )       /*  A/P Dollars  (Invoiced)   */


        dim                                                              ~
            arg_contract_id$16,          /* Contract to summ for       */~
            arg_contract_line$4,         /* Contract line to sum fer   */~
            contract$16,                 /* Test value from data       */~
            contract_line$4,             /* Test value from data       */~
            lin_start$6, lin_end$6,      /* Line Start-End YYMMDDs     */~
            plowkey$50,                  /* Plow key variable          */~
            readkey$50,                  /* Read key variable          */~
            source$1,                    /* Source File                */~
            srce_doc$16, srce_line$3,    /* Source Document            */~
            vendor$9,                    /* Vendor                     */~
            vpc_start$6, vpc_end$6,      /* Hdr  Start-End YYMMDDs     */~
            vpcxref_key$50               /* What it says               */

        dim f2%(04),                     /* = 0 if the file is open    */~
            f1%(04),                     /* = 1 if READ was successful */~
            fs%(04),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(04)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

            mat f2% = con


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! VPCMASTR ! Vendor Purchases Contract Master File    *~
            * #02 ! VPCXREF  ! Purchasing Contract, VBK/PAY Lines X-Ref *~
            * #03 ! VBKLINES ! Purchase Order Line Items File           *~
            * #04 ! PAYLINES ! Payables Invoice Line Item File          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "VPCXREF",                                       ~
                        varc,     indexed,  recsize =  133,              ~
                        keypos = 1,    keylen = 49,                      ~
                        alt key  1, keypos =   21, keylen =  49

            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            hdr%, lin% = 0%

            vpc_min_dlr, vpc_max_dlr, vpc_vbk_dlr, vpc_pay_dlr,          ~
            lin_min_dlr, lin_max_dlr, lin_vbk_qty, lin_vbk_dlr,          ~
            lin_pay_qty, lin_pay_dlr, lin_min_qty, lin_max_qty        = 0

            vpc_start$, vpc_end$, lin_start$, lin_end$ = " "

            if fs%(2%) = 0% then exit_program

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          M A I N                           ~
            *-----------------------------------------------------------*~
            * Loop thru VPCXREF for the purchasing contract requested -- ~
            * a-summing as we go.                                        ~
            *************************************************************

*        FIRST, get the VPC header and line item info...
            gosub get_header_fields

            gosub get_line_fields

*        Now, get the VBK and A/P data...
            vpcxref_key$ = arg_contract_id$

        vpcxref_loop
            call "PLOWNEXT" (#2, vpcxref_key$, 16%, f1%(2%))
            if f1%(2%) = 0% then exit_program

            get #2 using L10200, source$, vendor$, srce_doc$, srce_line$
L10200:         FMT XX(20), CH(1), CH(9), CH(16), CH(3)

            readkey$ = str(vendor$) & str(srce_doc$) & srce_line$
            vbk_dlr, vbk_qty, pay_dlr, pay_qty = 0

            if source$ = "P" then gosub get_vbk_line                     ~
                             else gosub get_pay_line

            vpc_vbk_dlr  = vpc_vbk_dlr + vbk_dlr
            vpc_pay_dlr  = vpc_pay_dlr + pay_dlr

            if str(vpcxref_key$,17%,4%) <> arg_contract_line$ then L10400

                lin_vbk_qty  = lin_vbk_qty + vbk_qty
                lin_vbk_dlr  = lin_vbk_dlr + vbk_dlr
                lin_pay_qty  = lin_pay_qty + pay_qty
                lin_pay_dlr  = lin_pay_dlr + pay_dlr

L10400:     goto vpcxref_loop



        get_header_fields
            readkey$ = arg_contract_id$
            call "READ100" (#1, readkey$, hdr%)
            if hdr% = 0% then return

            get #1 using L10510, vpc_start$, vpc_end$,                    ~
                                vpc_min_dlr, vpc_max_dlr
L10510:         FMT POS(86), 2*CH(6), POS(126), 2*PD(14,4)
            return


        get_line_fields
            readkey$ = str(arg_contract_id$,,16) & arg_contract_line$
            call "READ100" (#1, readkey$, f1%(1%))
            if f1%(1%) = 0% then return

            get #1 using L10630, lin_start$, lin_end$,                    ~
                                lin_min_qty, lin_max_qty,                ~
                                lin_min_dlr, lin_max_dlr
L10630:         FMT POS(86), 2*CH(6), XX(4), 2*PD(14,4), POS(126),       ~
                    2*PD(14,4)
            return


        get_vbk_line
            call "READ100" (#3, readkey$, srce%)
            if srce% = 0% then return

            get #3 using L10775, vbk_order, vbk_rcvd, vbk_open, unit_price
            vbk_order = vbk_order
            vbk_qty   = vbk_rcvd + vbk_open
            gosub net_pay_lines_out
            vbk_qty = max(0, vbk_qty - pay_qty)
            pay_qty = 0
            vbk_dlr   = vbk_qty  * unit_price
            return
L10775:         FMT POS(93), 3*PD(14,4), PD(14,7)


        get_pay_line
            call "READ100" (#4, readkey$, srce%)
            if srce% = 0% then return

            get #4 using L10870, pay_qty, pay_dlr
            return
L10870:         FMT POS(98), 2*PD(14,4)
L11000:         FMT POS(296), CH(16), CH(4)

        net_pay_lines_out
            plowkey$ = str(srce_doc$) & str(srce_line$) & vendor$
L11040:     call "PLOWALTS" (#04, plowkey$, 2%, 28%, f1%(4%))
            if f1%(4%) = 0% then return

            get #4 using L11000, contract$, contract_line$
            if contract$ <> arg_contract_id$ then L11040
            get #4 using L10870, p_qty
            pay_qty = pay_qty + p_qty
            goto L11040

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution.                                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
