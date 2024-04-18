        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP    CCC    SSS   U   U  M   M  RRRR   Y   Y   *~
            *  V   V  P   P  C   C  S      U   U  MM MM  R   R  Y   Y   *~
            *  V   V  PPPP   C       SSS   U   U  M M M  RRRR    YYY    *~
            *   V V   P      C   C      S  U   U  M   M  R   R    Y     *~
            *    V    P       CCC    SSS    UUU   M   M  R   R    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPCSUMRY - Prints Summary Information on Vendor           *~
            *            Purchase Contracts, Contract Lines, and        *~
            *            Purchases (Invoice & P.O. Lines).              *~
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
            * 07/14/94 ! Original                                 ! LDJ *~
            * 09/23/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            clinekey$20,                 /* Contract Lines PLOWKEY     */~
            columnttl$51,                /* Column titles line         */~
            contract$16,                 /* Contract ID                */~
            contractkey$29,              /* Contract Master Plow Key   */~
            contract_test$16,            /* Contract ID                */~
            contract_line$4,             /* Contract Line              */~
            contract_line_test$4,        /* Contract Line              */~
            contract_txtid$4,            /* Text ID for Contract       */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            due_date$8,                  /* P.O. Line Due Date         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmcontract$16,               /* Contract ID:               */~
            fmvendor$9,                  /* Vendor:                    */~
            hdr_comments$(2)50,          /* Contract Header Comments   */~
            hdr_descr$30,                /* Contract Header Name / Desc*/~
            hdr_end_date$8,              /* Contract Header End Date   */~
            hdr_start_date$8,            /* Contract Header Start Date */~
            hicontract$16,               /* Contract ID:               */~
            hivendor$9,                  /* Vendor:                    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invoice$16,                  /* Invoice Number             */~
            inv_line$3,                  /* Invoice Line               */~
            job$8,                       /* Job Code                   */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lin_comments$(2)50,          /* Contract Line Comments     */~
            lin_descr$30,                /* Contract Line Description  */~
            lin_end_date$8,              /* Contract Header End Date   */~
            lin_item$25,                 /* Contract Line Item         */~
            lin_start_date$8,            /* Contract Header Start Date */~
            lin_type$9,                  /* Contract Line Type Descr   */~
            lin_unit_price$15,           /* Contract Line Unit Price   */~
            lin_uom$4,                   /* Contract Line UOM          */~
            locontract$16,               /* Contract ID:               */~
            lovendor$9,                  /* Vendor:                    */~
            paylkey$99,                  /* PAYLINES Net Read/Plow Key */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16,                       /* Purchase Order Number      */~
            po_line$3,                   /* Purchase Order Line        */~
            rpttitle$60,                 /* Report Title               */~
            section_break_line$132,      /* Dashed Line 4 Section Break*/~
            time$8,                      /* System Time                */~
            tocontract$16,               /* Contract ID:               */~
            tovendor$9,                  /* Vendor:                    */~
            udate$8,                     /* Temporary Std Date for Calc*/~
            unit_price$10,               /* Unit Price from PAY/VBK lin*/~
            userid$3,                    /* Current User Id            */~
            vencontact$20,               /* Vendor Contact Name        */~
            vencurr$4,                   /* Vendor's Std Currency      */~
            vendkey$9,                   /* VENDOR Plow Key            */~
            vendor$9,                    /* Vendor Code                */~
            venfax$7,                    /* Vendor Fax #               */~
            veninfo$(7)30,               /* Vendor Names & Addresses   */~
            venparent$9,                 /* Parent Vendor Code         */~
            venparentdescr$32,           /* Parent Vendor Name         */~
            venphone$10,                 /* Vendor Phone               */~
            ven_totals(12),              /* Vendor Purchase Totals     */~
            xrefkey$49,                  /* VPCXREF Plow Key           */~
            yr$4                         /* Year Digits in VENHSTRY    */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #02 ! VENDOR   ! Vendor Master File                       *~
            * #03 ! VPCMASTR ! Vendor Purchases Contract Master File    *~
            * #04 ! VPCXREF  ! Purchasing Contract, VBK/PAY Lines X-Ref *~
            * #05 ! VBKLINES ! Purchase Order Line Items File           *~
            * #06 ! PAYLINES ! Payables Invoice Line Item File          *~
            * #07 ! TXTFILE  ! System Text File                         *~
            * #08 ! VENHSTRY ! Vendor History File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup     ~

            select #03, "VPCMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  2, keypos =   60, keylen =  26, dup,    ~
                            key  1, keypos =    1, keylen =  29          ~

            select #04, "VPCXREF",                                       ~
                        varc,     indexed,  recsize =  133,              ~
                        keypos =    1, keylen =  49,                     ~
                        alt key  1, keypos =   21, keylen =  49, dup     ~

            select #05, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos =  333, keylen =  20, dup     ~

            select #06, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  2, keypos =   17, keylen =  47,         ~
                            key  1, keypos =    1, keylen =  63          ~

            select #07, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select #08, "VENHSTRY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  11                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, u3%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "P U R C H A S E   C O N T R A C T S   " &       ~
                        "S U M M A R Y   R P T"

            str(columnttl$, 1%) = "Beginning Code"
            str(columnttl$,27%) = "Ending Code"

            line2$ = "Input Report Selection Criteria"
            str(line2$,62%) = "VPCSUMRY: " & str(cms2v$,,8)

            section_break_line$ = all("-")

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
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
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
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
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Vendor:                */~
                              L20200          /* Contract ID:           */
            return
L20100: REM Def/Enable Vendor                      FMVENDOR$
            if fmvendor$           = " " then                            ~
               fmvendor$           = "ALL"
            return

L20200: REM Def/Enable Contract ID                 FMCONTRACT$
            if fmcontract$         = " " then                            ~
               fmcontract$         = "ALL"
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
         "Enter Vendor                                                 ",~
         "Enter Contract ID                                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmcontract$, fmvendor$, hicontract$, hivendor$,    ~
                      locontract$, lovendor$, tocontract$, tovendor$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            call "SHOSTAT" ("Report In Progress ...")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("VPC001", "VPC1    ", 0%, 0%)
            pcntr% = -1% : l% = 99% /* Page & Line Counters */

            vendkey$ = lovendor$
            gosub vendor_loop
            goto end_report                /* Report Ending Routine */

        vendor_loop
            call "PLOWALTS" (#02, vendkey$, 0%, 0%, f1%(2%))
            if f1%(2%) = 0% then return
            if vendkey$ > hivendor$ then return
            contractkey$ = str(vendkey$,,9%) & locontract$
            gosub contract_hdr_loop
            goto vendor_loop

        contract_hdr_loop
            call "PLOWALTS" (#03, contractkey$, 1%, 9%, f1%(3%))
            if f1%(3%) = 0% then return
            l% = 99%       /* Set for Page Break on new Contract   */
            if str(contractkey$,10%,16%) > hicontract$ then return

            if str(contractkey$,,9%) <> vendor$ then                     ~
                gosub load_and_print_vendor_data
            gosub load_and_print_contract_hdr_data
            xrefkey$ = contract$
            str(xrefkey$,21%) = "I"
            first_payline% = 1%
            gosub paylines_loop
            xrefkey$ = contract$
            str(xrefkey$,21%) = "P"
            first_vbkline% = 1%
            gosub vbklines_loop
            clinekey$ = str(contract$) & hex(20202021)
            first_line% = 1%
            gosub contract_line_loop
            str(contractkey$,26%) = hex(ffffffff)
            goto contract_hdr_loop

        contract_line_loop
            call "PLOWALTS" (#03, clinekey$, 0%, 16%, f1%(3%))
            if f1%(3%) = 0% then return
            gosub load_and_print_contract_line_data
            xrefkey$ = str(contract$) & contract_line$
            str(xrefkey$,21%) = "I"
            first_payline% = 1%
            gosub paylines_loop
            xrefkey$ = str(contract$) & contract_line$
            str(xrefkey$,21%) = "P"
            first_vbkline% = 1%
            gosub vbklines_loop
            goto contract_line_loop

        paylines_loop
            call "PLOWALTS" (#04, xrefkey$, 0%, 21%, f1%(4%))
            if f1%(4%) = 0% then return
            plowkey$ = str(xrefkey$,22%,28%)
            call "READ100" (#06, plowkey$, f1%(6%))
            if f1%(6%) = 0% then paylines_loop
            gosub load_and_print_paylines
            goto paylines_loop

        vbklines_loop
            call "PLOWALTS" (#04, xrefkey$, 0%, 21%, f1%(4%))
            if f1%(4%) = 0% then return
            plowkey$ = str(xrefkey$,22%,28%)
            call "READ100" (#05, plowkey$, f1%(5%))
            if f1%(5%) = 0% then vbklines_loop
            gosub load_and_print_vbklines
            goto vbklines_loop

        load_and_print_vendor_data
            get #2 using L31880,  vendor$, veninfo$(), vencontact$,       ~
                     venphone$, venparent$, vencurr$, venfax$

            call "DATEFMT" (date, 0%, udate$)
            convert str(udate$,1%,4%) to date%
            convert date% - 1% to yr$, pic(0000)
            gosub'88(yr$)
            lastyr_total = yr_total
            gosub'88(str(udate$,1%,4%))
            ytd_total = yr_total
            if venparent$ > " " then                                     ~
              call "DESCRIBE" (#02,venparent$,venparentdescr$,1%,f1%(2%))~
            else venparentdescr$ = " "

            if l% >  48% then gosub page_head
            print using L60150, vendor$, veninfo$(1%), str(venphone$,,3%),~
                               str(venphone$,4%,4%), str(venphone$,8%,3%)
            print using L60180, veninfo$(2%), str(venfax$,,4%),           ~
                               str(venfax$,5%,3%), lastyr_total
            print using L60210, veninfo$(3%), vencontact$, ytd_total
            l% = l% + 3%
            if venparent$ = " " and veninfo$(4%) = " " then L31770
              print using L60240, veninfo$(4%), venparent$, venparentdescr$
              l% = l% + 1%
L31770:     if veninfo$(5%) = " " and vencurr$ = " " then L31800
              print using L60270, veninfo$(5%), vencurr$
              l% = l% + 1%
L31800:     if veninfo$(6%) = " " then L31830
              print using L60300, veninfo$(6%)
              l% = l% + 1%
L31830:     if veninfo$(7%) = " " then return
              print using L60300, veninfo$(7%)
              l% = l% + 1%
            return

L31880:     FMT CH(9), 7*CH(30), CH(20), CH(10), POS(494), CH(9),        ~
                POS(528), CH(4), CH(7)

        load_and_print_contract_hdr_data
            contract$ = str(contractkey$,10%)
            contract_txtid$ = hex(ffffffff)
            if str(contractkey$,26%) = " " then get #03 using L31980,     ~
              contract_line$,hdr_descr$, hdr_comments$(), contract_txtid$~
            else contract_line$, hdr_descr$, hdr_comments$() = " "

L31980:     FMT POS(26), CH(4),CH(30), POS(142), 2*CH(50), POS(242), CH(4)
            call "VPCSUMSB" (#03, #05, #06, contract$, contract_line$,   ~
                     hdr%, hdr_min_dlr, hdr_max_dlr, hdr_start_date$,    ~
                     hdr_end_date$, hdr_vbk_dlr, hdr_pay_dlr, lin%,      ~
                     lin_min_dlr, lin_max_dlr, lin_min_qty, lin_max_qty, ~
                     lin_start_date$, lin_end_date$, lin_vbk_qty,        ~
                     lin_vbk_dlr, lin_pay_qty, lin_pay_dlr)
            call "DATEFMT" (hdr_start_date$)
            call "DATEFMT" (hdr_end_date$)

            REM *** Start Print Contract Header ***
            if l% > 48% then gosub page_head
            print section_break_line$
            print using L60330
            print using L60360
            print using L60390, contract$, hdr_descr$
            print using L60410, hdr_start_date$, hdr_max_dlr,             ~
                               hdr_comments$(1%)
            print using L60440, hdr_end_date$, hdr_min_dlr,               ~
                               hdr_comments$(2%)
            print using L60470, hdr_pay_dlr, hdr_vbk_dlr,                 ~
                               hdr_pay_dlr + hdr_vbk_dlr
            l% = l% + 7%
L32210:     call "TXTPRINT" (#07, fs%(7%), 134%, contract_txtid$,        ~
                "VPC001", 20%, l%, 56%, "Y", " ", prt_status%)
            if prt_status% = 0% then return
                gosub page_head
                goto L32210

        load_and_print_contract_line_data
            get #03 using L32330, contract_line$, lin_descr$, lin_type$,  ~
              lin_item$, lin_uom$, lin_unit_price,                       ~
              lin_comments$(), contract_txtid$
            call "CONVERT" (lin_unit_price, 2.7, lin_unit_price$)

L32330:     FMT POS(26), CH(4),CH(30), CH(1), CH(25), POS(98), CH(4),    ~
                POS(118), PD(14,7), POS(142), 2*CH(50), POS(242), CH(4)

            if lin_type$ = "A" then lin_type$ = "Activity:"
            if lin_type$ = "P" then lin_type$ = "Part:"
            if lin_type$ = "M" then lin_type$ = "Miscell "
            call "VPCSUMSB" (#03, #05, #06, contract$, contract_line$,   ~
                     hdr%, hdr_min_dlr, hdr_max_dlr, hdr_start_date$,    ~
                     hdr_end_date$, hdr_vbk_dlr, hdr_pay_dlr, lin%,      ~
                     lin_min_dlr, lin_max_dlr, lin_min_qty, lin_max_qty, ~
                     lin_start_date$, lin_end_date$, lin_vbk_qty,        ~
                     lin_vbk_dlr, lin_pay_qty, lin_pay_dlr)
            call "DATEFMT" (lin_start_date$)
            call "DATEFMT" (lin_end_date$)

            REM *** Start Print Contract Line ***
            if l% > 48% then gosub page_head
            print
            if first_line% = 0% then L32560
                print using L60520
                print using L60550
                line% = line% + 2%
                first_line% = 0%
L32560:     print using L60580, contract_line$, lin_descr$, lin_type$,    ~
                               lin_item$, lin_unit_price$, lin_uom$
            print using L60600, lin_start_date$, lin_max_dlr, lin_max_qty

            print using L60630, lin_end_date$, lin_min_dlr, lin_min_qty

            print using L60700, lin_pay_qty, lin_vbk_qty,                 ~
                               lin_pay_qty + lin_vbk_qty
            print using L60660, lin_pay_dlr, lin_vbk_dlr,                 ~
                               lin_pay_dlr + lin_vbk_dlr
            if lin_comments$() = " " then L32680
                print using L60740, lin_comments$(1%), lin_comments$(2%)
                l% = l% + 1%

L32680:     l% = l% + 6%
L32690:     call "TXTPRINT" (#07, fs%(7%), 134%, contract_txtid$,        ~
                "VPC001", 20%, l%, 56%, "Y", " ", prt_status%)
            if prt_status% = 0% then return
                gosub page_head
                goto L32690

        load_and_print_paylines
            get #06 using L32780, po$, po_line$, invoice$, inv_line$,     ~
                lin_item$, qty, extension, job$, unit_price
L32780:     FMT POS(17), CH(16), CH(3), POS(45), CH(16), CH(3), POS(73), ~
                CH(25), PD(14,4), PD(14,4), CH(08), POS(131), PD(14,7)
            call "CONVERT" (unit_price, 2.7, unit_price$)

            REM *** Start Printing
            if l% > 56% then gosub page_head
            if first_payline% = 0% then L32900
                if l% > 52% then gosub page_head
                print
                print using L60780
                print using L60810
                print using L60830
                l% = l% + 4%
                first_payline% = 0%
L32900:     print using L60860, invoice$, inv_line$, job$, qty,           ~
                     unit_price$, extension, po$, po_line$, lin_item$
            l% = l% + 1%
            return

        load_and_print_vbklines
            get #05 using L32916, po$, po_line$, lin_item$, qty, qtyoo,   ~
                unit_price, extension, due_date$, job$
L32916:     FMT POS(10), CH(16), CH(3), POS(32), CH(25), POS(101),       ~
                2*PD(14,4), PD(14,7), PD(14,4), POS(142), CH(6),         ~
                POS(166), CH(8)
            call "CONVERT" (unit_price, 2.7, unit_price$)
            call "DATEFMT" (due_date$)
            gosub net_pay_lines_out
            qty = qty - invoiced_qty : if qty + qtyoo = 0 then return
            extension = round((qty + qtyoo) * unit_price, 2)
            REM *** Start Printing
            if l% > 56% then gosub page_head
            if first_vbkline% = 0% then L32942
                if l% > 52% then gosub page_head
                print
                print using L60900
                print using L60930
                print using L60960
                l% = l% + 4%
                first_vbkline% = 0%
L32942:     print using L60990, po$, po_line$, job$,qty,qtyoo,unit_price$,~
                     extension, due_date$, lin_item$
            l% = l% + 1%
            return

        end_report                /* Report Ending Routine */
            print skip(2)
            print using L64990     /* End of report line */
            close printer
            call "SETPRNT" ("VPC001", "VPC1    ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "VPCSUMRY"
            print using L60110, rpttitle$, pcntr%
            print : print
            l% = 5%
            first_payline% = 1%
            first_vbkline% = 1%
            first_line% = 1%
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
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        deffn'88(yr$)
            REM *** Get Purchase History Totals for Vendor (Not Contract)
            convert yr$ to yr%
            plowkey$ = str(vendor$) & bin(yr%,2)
            call "READ100" (#08, plowkey$, f1%(8%))
            if f1%(8%) = 1% then get #08 using L34760, ven_totals()       ~
                            else mat ven_totals = zer
            yr_total = 0
            for x% = 1% to 12%
                yr_total = yr_total + ven_totals(x%)
            next x%
            return

L34760:     FMT POS(12), 12*PD(14,4)

        net_pay_lines_out
            invoiced_qty = 0
            paylkey$ = str(po$) & str(po_line$) & vendor$
L34810:     call "PLOWALTS" (#06, paylkey$, 2%, 28%, f1%(6%))
            if f1%(6%) = 0% then return

            get #6 using L34900, contract_test$, contract_line_test$
            if contract_test$ <> contract$ then L34810
            get #6 using L34910, p_qty
            invoiced_qty = invoiced_qty + p_qty
            goto L34810

L34900:     FMT POS(296), CH(16), CH(4)
L34910:     FMT POS(98), 2*PD(14,4)

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
              on fieldnr% gosub L40080,         /* Vendor:           */   ~
                                L40080          /* Contract ID:      */
              goto L40095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Purchase Contracts Summary Report",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Vendor:",                                    ~
               at (07,30), fac(lfac$( 1)), fmvendor$            , ch(09),~
               at (07,56), fac(lfac$( 1)), tovendor$            , ch(09),~
                                                                         ~
               at (08,02), "Contract ID:",                               ~
               at (08,30), fac(lfac$( 2)), fmcontract$          , ch(16),~
               at (08,56), fac(lfac$( 2)), tocontract$          , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40240
                  call "MANUAL" ("VPCSUMRY") : goto L40095

L40240:        if keyhit% <> 15 then L40255
                  call "PRNTSCRN" : goto L40095

L40255:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40350     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40335
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40340
L40335:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40340:     return

L40350: if fieldnr% > 0% then L40395  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40395:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50110,         /* Vendor:                */~
                              L50280          /* Contract ID:           */
            return
L50110: REM Test for Vendor                       FMVENDOR$
            call "TESTRNGE"                                              ~
                  (fmvendor$           , tovendor$           ,           ~
                   lovendor$           , hivendor$           ,           ~
                   errormsg$, #02)
            return

L50280: REM Test for Contract ID                  FMCONTRACT$
            if fmcontract$ = "ALL" or fmcontract$ = "FIRST" or           ~
               fmcontract$ = " " then L50315
            call "VPCPIKME" (" ", fmcontract$, " ", " ", " ", " ",       ~
                #03, #02, f1%(3%))
            if f1%(3%) = 1% then L50315
                errormsg$ = "Contract Not Found: " & fmcontract$
                return
L50315:     if tocontract$ = " " or tocontract$ = "LAST" then L50350
            call "VPCPIKME" (" ", tocontract$, " ", " ", " ", " ",       ~
                #03, #02, f1%(3%))
            if f1%(3%) = 1% then L50350
                errormsg$ = "Contract Not Found: " & tocontract$
                return
L50350:     call "TESTRNGE"                                              ~
                  (fmcontract$         , tocontract$         ,           ~
                   locontract$         , hicontract$         ,           ~
                   errormsg$)
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:VPC001

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: #####

*       * Vendor Lines
L60150: %Vendor: #########  ##############################      Phone  : ~
        ~(###) #### ###               ....Total Purchases This Vendor.....

L60180: %                   ##############################      Fax    : ~
        ~#### ###                              Last Year: $###,###,###.##-

L60210: %                   ##############################      Contact: ~
        ~####################               Year-To-Date: $###,###,###.##-

L60240: %                   ##############################      Parent : ~
        ~#########  ################################

L60270: %                   ##############################      Std Cur: ~
        ~####

L60300: %                   ##############################

*       * Contract Header Lines
L60330: %                                                     Contract He~
        ~ader Information

L60360: %                                                     -----------~
        ~----------------

L60390: %Contract: ################  ##############################

L60410: %           Start Date: ########   Maximum Value: $###,###,###.##~
        ~      ##################################################

L60440: %           End Date  : ########   Minimum Value: $###,###,###.##~
        ~      ##################################################

L60470: %           Total Expenditures against Contract = Invoiced: $###,~
        ~###,###.##-   Uninvoiced: $###,###,###.##-   Total: $###,###,###.~
        ~##-

*       * Contract Lines
L60520: %                                                     Contract Li~
        ~nes Information

L60550: %                                                     -----------~
        ~---------------

L60580: %    Line: ####  ##############################     For #########~
        ~ #########################   Unit Price: $###############  ####

L60600: %           Start Date: ########   Maximum Value: $###,###,###.##~
        ~   Maximum Quantity: ###,###,###.##

L60630: %           End Date  : ########   Minimum Value: $###,###,###.##~
        ~   Minimum Quantity: ###,###,###.##

L60660: %           Total Expenditures against Line  =    Invoiced: $###,~
        ~###,###.##-   Uninvoiced: $###,###,###.##-   Total: $###,###,###.~
        ~##-

L60700: %           Total Quantity against this Line =    Invoiced:  ###,~
        ~###,###.##-   Uninvoiced:  ###,###,###.##-   Total:  ###,###,###.~
        ~##-

L60740: %             ################################################## ~
        ~##################################################

*       * Invoiced Purchases Detail Lines
L60780: %           ..........................................Invoiced Pu~
        ~rchases Detail....................................................

L60810: %           Invoice Number   Line  For Job  Invoice Qty Unit Pric~
        ~e      Extension  Purchase Order   Line  Part Code

L60830: %           ---------------- ----  -------- ----------- ---------~
        ~- --------------  ---------------- ----  -------------------------

L60860: %           ################ ####  ######## #######.##- #########~
        ~# $##,###,###.##- ################ ####  #########################

*       * Non-Invoiced Purchases Detail Lines
L60900: %           ........................................Non-Invoiced ~
        ~Purchases Detail..................................................

L60930: %           Purchase Order   Line  For Job  Qty Receivd Qty on Or~
        ~der Unit Price       Extension  Date Due Part Code

L60960: %           ---------------- ----  -------- ----------- ---------~
        ~--- ---------- ---------------  -------- -------------------------

L60990: %           ################ ####  ######## #######.##- ########.~
        ~##- ######.##- $##,###,###.##- -########.##  ######## ############

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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
