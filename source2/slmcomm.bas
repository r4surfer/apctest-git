        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   L      M   M   CCC    OOO   M   M  M   M          *~
            *  S      L      MM MM  C   C  O   O  MM MM  MM MM          *~
            *   SSS   L      M M M  C      O   O  M M M  M M M          *~
            *      S  L      M   M  C   C  O   O  M   M  M   M          *~
            *   SSS   LLLLL  M   M   CCC    OOO   M   M  M   M          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SLMCOMM  - Selects, sorts, computes and prints a report   *~
            *            of salesmen's commissions based on invoices.   *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/28/86 ! Original                                 ! JIM *~
            * 05/14/87 ! Std Cost Changes                         ! ERN *~
            * 06/08/89 ! Changed to allow same salesman with same ! MJB *~
            *          !  invoice on same date for different customer   *~
            * 06/11/91 ! Malco- Added option to omit Unit Cost.   ! JIM *~
            * 03/16/92 ! PRR 12137- Std Cost Corrections.         ! JDH *~
            *          ! PRR 11865- Added Date Type option.       !     *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            appcd$1,                     /* Apply code from SLMMAST2   */~
            apply$(6)7,                  /* Apply descriptions         */~
            applyamt$12,                 /* Edited amount applied      */~
            arilines_key$20,             /* Key to ARILINES file       */~
            brk_cst$9,                   /* Control Customer Break     */~
            brk_dat$8,                   /* Controls date breaks       */~
            brk_inv$8,                   /* Controls invoice breaks    */~
            brk_slm$4,                   /* Controls salesman breaks   */~
            catgy$4,                     /* Category code from ARILINES*/~
            commamt$12,                  /* Edited commission amount   */~
            commrate$5,                  /* Edited commission rate     */~
            company_name$60,             /* Company name from COMPNAME */~
            cost_yorn$1,                 /* Omit Cost? Y/N             */~
            cursor%(2),                  /* Cursor location for edit   */~
            cust_code$9,                 /* Customer code from ARIMASTR*/~
            cust_name$30,                /* Customer name from ARIMASTR*/~
            date$8,                      /* Date for screen display    */~
            date_to_use$1,               /* Date Type to use in ranges */~
            detl_yorn$1,                 /* Print/don't print detail   */~
            detlstring$105,              /* Edited line item data      */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            extension$11,                /* Edited line item extension */~
            hdr$40,                      /* ASKUSER constant           */~
            high_date$10,                /* High date for capture      */~
            high_salm$4,                 /* High salesman for capture  */~
            hnystdcost$10,               /* Edited standard cost       */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inv_date$8,                  /* Invoice date from ARIMASTR */~
            inv_hdr$40,                  /* Invoice header description */~
            invoice$8,                   /* Invoice # from ARIMASTR    */~
            lastpur$24,                  /* Last purged description    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lindiscpct$5,                /* Edited discount percent    */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linqtyship$8,                /* Edited quantity shipped    */~
            linunitprc$9,                /* Edited unit price          */~
            low_date$10,                 /* INVOICE DATE RANGE         */~
            low_salm$4,                  /* RANGE OF SALESMEN          */~
            msg$(3)80,                   /* ASKUSER constant           */~
            part_nmbr$25,                /* Part number from ARILINES  */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            purge_date$8,                /* Date last purged (SYSFILE2)*/~
            rptid$6,                     /* Report ID                  */~
            slmmast2_key$33,             /* Key to SLMMAST2            */~
            slman$4,                     /* Salesman code frm WORKFILE */~
            slman$(3)4,                  /* Salesman codes frm ARIMASTR*/~
            slman_name$32,               /* Salesman names frm SLMMASTR*/~
            so_seqnr$3,                  /* SO sequence # from ARILINES*/~
            split(3),                    /* Comm split % from ARIMASTR */~
            sub_hdr$70,                  /* Report sub-header          */~
            sumslsman$(200)4,            /* Summary totals salesman cd */~
            sumslname$(200)32,           /* Summary totals salesman nm */~
            sumslbase(200),              /* Summary totals base        */~
            sumslcomm(200),              /* Summary totals commission  */~
            tstpurgdt$8,                 /* test purge date            */~
            time$8,                      /* Time of day stamp          */~
            tot_desc$66,                 /* Edited total descriptions  */~
            totinvamt(4),                /* Invoice total array        */~
            totcommsn(4),                /* Commission total array     */~
            userid$3,                    /* Current User Id            */~
            work_fil$8,                  /* Workfile name              */~
            work_lib$8,                  /* Workfile library           */~
            work_vol$6                   /* Workfile volume            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * #3  ! ARIMASTR ! Invoice Master File                      *~
            * #4  ! ARILINES ! Invoice Line Items File                  *~
            * #5  ! SLMMAST2 ! Salesmen Commission Structure            *~
            * #6  ! SLMMASTR ! Salesman master file                     *~
            * #8  ! SYSFILE2 ! Caelus Management System Information     *~
            * #9  ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17

            select #4,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #5,  "SLMMAST2",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  33

            select #6,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #8,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #9,  "WORKFILE", varc, consec, recsize =  109

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            get rslt$(3) using L02500, nbr_inv%
L02500:         FMT  POS(17), BI(4)
                rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            get rslt$(4) using L02500, nbr_lin%
                rslt$(5 ) = "REQUIRED"
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
                rslt$(6 ) = "REQUIRED"
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
                rslt$(8 ) = "REQUIRED"
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            if nbr_inv% > 0% then goto initial_continue
L09060:         comp% = 2%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no Invoice/Commission data to print"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L09060
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */
        initial_continue
        REM *************************************************************~
            * DATE RANGE ENTERED BY THE OPERATOR (IF ANY), MAY NOT IM-  *~
            * PINGE UPON THE DATE LAST PURGED AS RECORDED IN 'SYSFILE2'.*~
            *************************************************************
            purge_date% = 0% : init (" ") purge_date$, errormsg$
            call "READ100" (#8, "LAST PURGE DATES", f1%(8))
            if f1%(8) = 0% then goto initial_continue_1
            get #8 using L09220, purge_date$
L09220:         FMT  POS(21), CH(6)
            tstpurgdt$ = "19010101"
            call "DATECONV" (tstpurgdt$)
            if not (purge_date$ > tstpurgdt$) then goto initial_continue_1
            call "DATEOK" (purge_date$, purge_date%, errormsg$)
            if errormsg$ <> " " then goto exit_program
            lastpur$ = "Last Purged: " & purge_date$

        initial_continue_1
            call "EXTRACT" addr("ID", userid$)
            call "COMPNAME" (12%, company_name$, comp%)
            rptid$ = "SLM002"
            max_lines% = 56%
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            apply$(1) = "NO APPL" : apply$(2) = "GRS EXT"
            apply$(3) = "NET EXT" : apply$(4) = "ST COST"
            apply$(5) = "MRG GRS" : apply$(6) = "MRG NET"
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, high_salm$, high_date$
            low_salm$, low_date$ = "ALL" : detl_yorn$ = "Y"
            cost_yorn$ = "Y" : date_to_use$ = "I"

            for fieldnr% = 1% to 5%
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Print Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11150:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1% or fieldnr% > 5% then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                if cursor%(1) - 5% <> fieldnr% then goto L11150
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT"                                               ~
                ("Records are being selected ... Please stand by")
            init (" ") sumslsman$(), sumslname$()
            mat sumslbase = zer
            mat sumslcomm = zer
            mat totinvamt = zer
            mat totcommsn = zer
            inv_hdr$ = "ALL INVOICES"
            if low_date$ <> "ALL" then                                   ~
                inv_hdr$="INVOICES DATED "&low_date$&" TO "&high_date$
            call "WORKOPN2" (#9, "OUTPT", 3%*(nbr_lin%+nbr_inv%), f2%(9))
            call "GETNAMES" addr(#9, work_fil$, work_lib$, work_vol$)
            nbr_recs% = 0%   /* Initialize count of records selected */
            plowkey$ = xor plowkey$

        plow_thru_arimastr
            call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
            if f1%(3) = 0% then goto sort_work_file
            get #3 using L12300, cust_code$, invoice$, cust_name$,        ~
                slman$(), split(), inv_date$, temp$, invgrsamt, invdiscpct

        REM PARTIAL RECORD LAYOUT FOR FILE 'ARIMASTR' *******************
L12300:         FMT  CH(9),              /* Customer code              */~
                     CH(8),              /* Invoice number             */~
                     POS(53), CH(30),    /* Ship-to name               */~
                     POS(501), 3*CH(4),  /* Up to 3 salesman codes     */~
                     POS(513), 3*BI(1),  /* Up to 3 commission splits  */~
                     POS(521), CH(6),    /* Invoice Date               */~
                     POS(533), CH(6),    /* Invoice Posting Date       */~
                     POS(793), PD(14,4), /* Invoice gross amount       */~
                     POS(801), PD(14,4)  /* Invoice discount percent   */

            if str(invoice$,,3) = "RCR" then plow_thru_arimastr
            if date_to_use$ = "P" then inv_date$ = temp$
            errormsg$ = " "
            call "DATEOK" (inv_date$, inv_date%, errormsg$)
            if errormsg$ <> " " then goto plow_thru_arimastr
            if inv_date% < fr_date% then goto plow_thru_arimastr
            if inv_date% > to_date% then goto plow_thru_arimastr
            call "DATUNFMT" (inv_date$)
            nbr_inv% = 0% /* INDICATE NO SALESMAN DATA */
            if slman$(1%) <> slman$(2%) then L12453
                split(1%) = split(1%) + split(2%)
                slman$(2%) = " " : split(2%) = 0
L12453:     if slman$(1%) <> slman$(3%) then L12457
                split(1%) = split(1%) + split(3%)
                slman$(3%) = " " : split(3%) = 0
L12457:     if slman$(2%) <> slman$(3%) then L12465
                split(2%) = split(2%) + split(3%)
                slman$(3%) = " " : split(3%) = 0
L12465:     for slm% = 1% to 3%
                if slman$(slm%) = " " and split(slm%) = 0                ~
                     then goto bump_sman_master
                if slman$(slm%) < from_salm$ then goto bump_sman_master
                if slman$(slm%) > to_salm$ then goto bump_sman_master
                write #9 using L12550, slman$(slm%), inv_date$,           ~
                         cust_code$, invoice$, hex(000000), cust_name$,  ~
                         invgrsamt, invdiscpct, split(slm%)

L12550:              FMT  CH(4),         /* SALESMAN CODE              */~
                          CH(6),         /* DATE INVOICE POSTED        */~
                          CH(9),         /* CUSTOMER CODE              */~
                          CH(8),         /* INVOICE NUMBER             */~
                          CH(3),         /* SO SEQUENCE NUMBER         */~
                          CH(30),        /* CUSTOMER NAME              */~
                          PD(14,4),      /* GROSS INVOICE AMOUNT       */~
                          PD(14,4),      /* INVOICE DISCOUNT PERCENT   */~
                          BI(1),         /* COMMISSION SPLIT %         */~
                          XX(32)         /* FILLER                     */

                nbr_inv%, nbr_recs% = 1% /* INDICATE 'RECORDS SELECTED'*/
        bump_sman_master
            next slm%
            if nbr_inv% = 0% then goto plow_thru_arimastr

        REM READ ASSOCIATED LINE ITEMS ********************************
            arilines_key$ = str(cust_code$,,9) & invoice$
            str(arilines_key$,18) = all(hex(00))

        plow_thru_arilines
            call "PLOWNEXT" (#4, arilines_key$, 17%, f1%(4))
            if f1%(4) = 0% then goto plow_thru_arimastr
            get #4 using L12820, so_seqnr$, part_nmbr$, catgy$,           ~
                linqtyship, linunitprc, lindiscpct, stdcost

        REM PARTIAL RECORD LAYOUT OF FILE 'ARILINES' ********************
L12820:         FMT  POS(18), CH(3),     /* SO sequence number         */~
                     POS(24), CH(25),    /* Part number                */~
                     POS(81), CH(4),     /* Part category              */~
                     POS(93), PD(14,4),  /* Quantity shipped           */~
                     POS(109), PD(14,4), /* Unit price (stocking)      */~
                     POS(141), PD(14,4), /* Line item discount percent */~
                     POS(638), PD(14,4)  /* Total Std Cost of shipped  */

            if linqtyship = 0 then goto plow_thru_arilines
            for slm% = 1% to 3%
                if slman$(slm%) = " " and split(slm%) = 0                ~
                                             then goto bump_sman_detail
                if slman$(slm%) < from_salm$ then goto bump_sman_detail
                if slman$(slm%) > to_salm$   then goto bump_sman_detail
                slmmast2_key$ = str(slman$(slm%),,4) & "    "& part_nmbr$
                gosub read_comm_structure
                if f1%(5) = 1% then write_work_file
                     str(slmmast2_key$,5,4) = catgy$
                     str(slmmast2_key$,9)   = all(" ")
                     gosub read_comm_structure
                     if f1%(5) = 1% then write_work_file
                          str(slmmast2_key$,5,4) = "****"
                          gosub read_comm_structure

            write_work_file
                write #9 using L13190, slman$(slm%), inv_date$,           ~
                         cust_code$, invoice$, so_seqnr$, catgy$,        ~
                         part_nmbr$, linqtyship, linunitprc, lindiscpct, ~
                         appcd$, commrate, stdcost

L13190:              FMT  CH(4),         /* SALESMAN CODE              */~
                          CH(6),         /* DATE INVOICE POSTED        */~
                          CH(9),         /* Customer Code              */~
                          CH(8),         /* INVOICE NUMBER             */~
                          CH(3),         /* SO SEQUENCE NUMBER         */~
                          CH(4),         /* PART CATEGORY CODE         */~
                          CH(25),        /* PART NUMBER                */~
                          PD(14,4),      /* QUANTITY SHIPPED           */~
                          PD(14,4),      /* UNIT PRICE (STOCKING)      */~
                          PD(14,4),      /* LINE ITEM DISCOUNT PERCENT */~
                          CH(1),         /* APPLIES CODE               */~
                          PD(14,4),      /* COMMISSION RATE            */~
                          PD(14,4),      /* STANDARD COST              */~
                          XX(9)          /* FILLER                     */

        bump_sman_detail
            next slm%
            goto plow_thru_arilines

        sort_work_file
            if nbr_recs% > 0% then sort_continue
L13390:         comp% = 2%
                hdr$ = "*** NULL SET SELECTED ***"
                msg$(1) = "There are no records that satisfy your" &     ~
                     " criteria"
                msg$(3) = "Press RETURN to continue"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L13390
                goto inputmode
        sort_continue
            call "SLCTSORT" (#9, 30%)
            call "SHOSTAT" ("Sales Commission Report in process")
            page_nbr% = 0% : nbr_lines% = 99%
            time$ = time
            call "TIME" (time$)
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            sub_hdr$ = "SALES COMMISSION REPORT FOR "
            call "FMTTITLE" (sub_hdr$, inv_hdr$, 2%)
            colsw%, one_time%, eodsw% = 0% : ptr% = 1%

        REM PRINT 'PAGE ZERO' -- THE SELECTION SCREEN *******************
            pagesw% = 1%
            gosub page_0_heading
            print skip (4)
L14022:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L14030
                str(i$(), i%, 1%) = " "
                goto L14022
L14030:     print using L60100, "    ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60100, i$(n%)
            next n%
            pagesw% = 0%

        read_sorted_work_file
            read #9 using L16040, slman$, inv_date$, cust_code$, invoice$,~
                                 so_seqnr$, eod goto end_of_report

L16040:              FMT  CH(4),         /* SALESMAN CODE              */~
                          CH(6),         /* DATE INVOICE POSTED        */~
                          CH(9),         /* Customer Code              */~
                          CH(8),         /* INVOICE NUMBER             */~
                          CH(3)          /* SO SEQUENCE NUMBER         */

            brksw% = 0%
            if one_time% <> 0% then goto test_for_key_break
                gosub salesman_reset
                gosub date_reset
                gosub invoice_reset
                one_time% = 1%
        test_for_key_break
            if slman$ <> brk_slm$ then gosub salesman_total
            if inv_date$ <> brk_dat$ then gosub date_total
            if cust_code$ = brk_cst$ and invoice$ = brk_inv$ then L16190
                gosub invoice_total
L16190:     if brksw% = 0% then goto read_detail_fields
                if detl_yorn$ <> "Y" then goto read_sorted_work_file
                print : nbr_lines% = nbr_lines% + 1%
                goto read_sorted_work_file

        read_detail_fields
        REM CONTINUE WITH DETAIL RECORD FROM WORKFILE *******************
            get #9 using L16300, catgy$, part_nmbr$, linqtyship,          ~
                linunitprc, lindiscpct, appcd$, commrate, hnystdcost


L16300:              FMT  POS(31), CH(4),/* PART CATEGORY CODE         */~
                          CH(25),        /* PART NUMBER                */~
                          PD(14,4),      /* QUANTITY SHIPPED           */~
                          PD(14,4),      /* UNIT PRICE (STOCKING)      */~
                          PD(14,4),      /* LINE ITEM DISCOUNT PERCENT */~
                          CH(1),         /* APPLIES CODE               */~
                          PD(14,4),      /* COMMISSION RATE            */~
                          PD(14,4)       /* STANDARD COST              */

            gosub apply_net_ext /* GET DEFAULT 'EXTENSION' */
            on pos("12345"=appcd$)+1% gosub apply_invalid,               ~
                apply_gross_ext, apply_net_ext, apply_std_cost,          ~
                apply_grs_margin, apply_net_margin
            gosub calculate_commission
            init (" ") lindiscpct$, hnystdcost$
            call "CONVERT" (linqtyship, 2.2, linqtyship$)
            call "CONVERT" (linunitprc, 2.4, linunitprc$)
            if lindiscpct <> 0 then                                      ~
                call "CONVERT" (lindiscpct, 1.2, lindiscpct$)
            call "CONVERT" (extension, 2.2, extension$)
            if pos("12345"=appcd$) > 2% or hnystdcost <> 0 then          ~
                call "CONVERT" (hnystdcost, 2.2, hnystdcost$)
            if cost_yorn$ <> "Y" then hnystdcost$ = " "
            convert applyamt to applyamt$, pic (-####,###.##)
            convert commamt to commamt$, pic (-####,###.##)
            call "CONVERT" (commrate, 1.2, commrate$)
            call "DATEFMT" (inv_date$)
            if detl_yorn$ <> "Y" then goto add_to_totals
            if nbr_lines% > max_lines% then gosub page_heading
            detlstring$ = str(part_nmbr$,,len(str(part_nmbr$)))        & ~
                str(linqtyship$,,len(str(linqtyship$)))                & ~
                str(linunitprc$,,len(str(linunitprc$)))                & ~
                str(lindiscpct$,,len(str(lindiscpct$)))                & ~
                str(extension$,, len(str(extension$)))                 & ~
                str(hnystdcost$,,len(str(hnystdcost$)))         & " "  & ~
                str(apply$(pos("12345"=appcd$)+1%),,                     ~
                     len(str(apply$(pos("12345"=appcd$)+1%))))         & ~
                str(applyamt$,,  len(str(applyamt$)))                  & ~
                str(commrate$,,  len(str(commrate$)))                  & ~
                str(commamt$,,   len(str(commamt$)))
            if omitsw% <> 2% then goto L16730
                print using L60190, " ", " ", so_seqnr$, catgy$,          ~
                     detlstring$
                goto L16790
L16730:     if omitsw% <> 1% then goto L16770
                print using L60190, " ", invoice$, so_seqnr$, catgy$,     ~
                     detlstring$
                goto L16790
L16770:     print using L60190, inv_date$, invoice$, so_seqnr$, catgy$,   ~
                     detlstring$
L16790:     nbr_lines% = nbr_lines% + 1%
        add_to_totals
            omitsw% = 2%
            totinvamt(4%) = totinvamt(4%) + applyamt
            totcommsn(4%) = totcommsn(4%) + commamt
            goto read_sorted_work_file

        read_comm_structure
            commrate = 0 : appcd$ = " "
            call "READ100" (#5, slmmast2_key$, f1%(5))
            if f1%(5) = 0% then return
            get #5 using L16910, commrate, appcd$
L16910:         FMT  POS(34), PD(14,4),  /* COMMISSION RATE            */~
                     POS(42), CH(1)      /* APPLIES CODE               */
            return

        apply_invalid
            applyamt = 0
            return
        apply_gross_ext
            extension, applyamt = linqtyship * linunitprc
            return
        apply_net_ext
            gosub apply_gross_ext
            applyamt = applyamt - ((applyamt * lindiscpct) / 100)
            applyamt = applyamt - ((applyamt * invdiscpct) / 100)
            extension = applyamt
            return
        apply_std_cost
            applyamt = hnystdcost
            return
        apply_grs_margin
            gosub apply_std_cost
            savstdcost = hnystdcost
            gosub apply_gross_ext
            applyamt = applyamt - savstdcost
            return
        apply_net_margin
            gosub apply_std_cost
            savstdcost = hnystdcost
            gosub apply_net_ext
            applyamt = applyamt - savstdcost
            return

        calculate_commission
            commamt = round((((applyamt*commrate)/100)*split)/100, 2)
            applyamt = round(applyamt, 2)
            return

        salesman_total
            gosub date_total
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60310 /* UNDERSCORE */
            tot_desc$ = "** SALESPERSON " & brk_slm$ & " " &             ~
                        slman_name$ & " TOTALS:"
            call "FMTTITLE" (tot_desc$, " ", 1%)
            print using L60340, tot_desc$, totinvamt(2%), totcommsn(2%)
                print
                print using L60500 /* END OF SALESMAN */
                nbr_lines% = 99% /* NEW PAGE FOR NEW SALESMAN */
            totinvamt(1%) = totinvamt(1%) + totinvamt(2%)
            totcommsn(1%) = totcommsn(1%) + totcommsn(2%)
            if ptr% > 200% then goto L18210
                sumslsman$(ptr%) = brk_slm$
                sumslname$(ptr%) = slman_name$
                sumslbase(ptr%) = totinvamt(2%)
                sumslcomm(ptr%) = totcommsn(2%)
                ptr% = ptr% + 1%
L18210:     totinvamt(2%), totcommsn(2%) = 0
        salesman_reset
            if eodsw% = 1% then return
            brk_slm$ = slman$
            call "DESCRIBE" (#6, slman$, slman_name$, 1%, f1%(6))
            if slman_name$ = " " then                                    ~
                                    slman_name$ = "(Unknown Salesperson)"
            omitsw% = 0%
            return

        date_total
            gosub invoice_total
            if nbr_lines% > max_lines% then gosub page_heading
            call "DATEFMT" (brk_dat$)
            tot_desc$ = "* DATE " & brk_dat$ & " TOTALS:"
            call "FMTTITLE" (tot_desc$, " ", 1%)
            print using L60310 /* UNDERSCORE */
            if detl_yorn$ <> "Y" then goto L18410
                print using L60370, " ", tot_desc$, totinvamt(3%),        ~
                    totcommsn(3%)
                goto L18440
L18410:     print using L60370, brk_dat$, tot_desc$, totinvamt(3%),       ~
                totcommsn(3%)
            print : nbr_lines% = nbr_lines% + 1%
L18440:     nbr_lines% = nbr_lines% + 2%
            totinvamt(2%) = totinvamt(2%) + totinvamt(3%)
            totcommsn(2%) = totcommsn(2%) + totcommsn(3%)
            totinvamt(3%), totcommsn(3%) = 0
        date_reset
            if eodsw% = 1% then return
            brk_dat$ = inv_date$
            omitsw% = 0%
            return

        invoice_total
            if nbr_lines% > max_lines% then gosub page_heading
            if detl_yorn$ = "Y" then goto L18610
                print using L60220, brk_inv$, brk_cst$, cust_name$, " ",  ~
                      invdiscpct, " ", split, " ", totinvamt(4%),        ~
                      totcommsn(4%)
                goto L18660
L18610:     print using L60310      /* Underscore */
            nbr_lines% = nbr_lines% + 1%
            print using L60220, " ", brk_cst$, cust_name$, "Disc:",       ~
                  invdiscpct, "Split:", split, "Invoice Totals:",        ~
                  totinvamt(4%), totcommsn(4%)
L18660:     nbr_lines% = nbr_lines% + 1%
            totinvamt(3%) = totinvamt(3%) + totinvamt(4%)
            totcommsn(3%) = totcommsn(3%) + totcommsn(4%)
            totinvamt(4%), totcommsn(4%) = 0

        invoice_reset
            if eodsw% = 1% then return
            brk_inv$ = invoice$
            brk_cst$ = cust_code$
            get #9 using L18750, cust_name$, invgrsamt, invdiscpct, split
L18750:         FMT  POS(31), CH(30), 2*PD(14,4), BI(1)
            omitsw%, brksw% = 1%
            return

        end_of_report
            eodsw% = 1%
            gosub salesman_total
            sub_hdr$ = "SALES COMMISSION SUMMARY FOR "
            call "FMTTITLE" (sub_hdr$, inv_hdr$, 2%)
            nbr_lines% = 99% : colsw% = 1%
            for nx% = 1% to min(ptr%-1%, 200%)
                if nbr_lines% > max_lines% then gosub page_heading
                print using L60440, sumslsman$(nx%), sumslname$(nx%),     ~
                     sumslbase(nx%), sumslcomm(nx%)
                nbr_lines% = nbr_lines% + 1%
            next nx%
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60480
            tot_desc$ = "*** RUN TOTALS:"
            call "FMTTITLE" (tot_desc$, " ", 1%)
            print using L60460, tot_desc$, totinvamt(1), totcommsn(1)
            print
            print using L60520   /* END OF REPORT */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)

        REM SCRATCH THE WORK FILE HOLDING CURRENT LINE ITEMS ************
            close #9
            call "SCRATCH" addr("F", work_fil$, work_lib$, work_vol$,    ~
                "B", " ", comp%)
            goto inputmode

        page_heading
            page_nbr% = page_nbr% + 1%
        page_0_heading
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, sub_hdr$, page_nbr%
            omitsw% = 0%
            if pagesw% <> 0% then return
            print
            if colsw% <> 0% then goto summary_columns
            nbr_lines% = 8%
            print using L60120, brk_slm$, slman_name$
            print
            if date_to_use$ = "I" then temp$ = "INV" else temp$ = "PST"
            if detl_yorn$ <> "Y" then goto L19240
                if cost_yorn$ = "Y"                                      ~
                     then print using L60130, temp$, "#", "STD COST"      ~
                     else print using L60130, temp$, "#", " "
                if cost_yorn$ = "Y"                                      ~
                     then print using L60160, "---------"                 ~
                     else print using L60160, " "
                goto L19260
L19240:     print using L60250, temp$, "#", "#" /* INVOICE COLUMN HDRS */
            print using L60280                  /* UNDERSCORES    */
L19260:     print
            return
        summary_columns
            nbr_lines% = 6%
            print using L60400
            print using L60420
            print
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20130,         /* RANGE OF SALESMEN*/~
                                    L20180,         /* INVOICE DATE RANG*/~
                                    L20330,         /* Date Type to Use */~
                                    L20230,         /* PRINT DETAIL Y/N */~
                                    L20280          /* Print Costs? Y/N */
                     return

L20130:     REM RANGE OF SALESMEN           LOW_SALM$, HIGH_SALM$
            inpmessage$ = "Enter range of SALESPERSON Codes, 'LAST' or " ~
                          & "'ALL'"
                return

L20180:     REM INVOICE DATE RANGE          LOW_DATE$, HIGH_DATE$
            inpmessage$ = "Enter range of Invoice Posting dates, " &     ~
                "'FIRST', 'LAST' or 'ALL'"
                return

L20230:     REM PRINT DETAIL Y OR N         DETL_YORN$
            inpmessage$ = "Enter 'Y' to print detail invoice lines, " &  ~
                "'N' to omit detail lines"
                return

L20280:     REM PRINT Costs? Y OR N         COST_YORN$
            if detl_yorn$ = "Y" then goto L20290
                cost_yorn$ = "N"
                enabled% = 0%
                return
L20290:     inpmessage$ = "Enter 'Y' to print Unit Costs, 'N' to omit U"&~
                "nit Costs."
                return

L20330:     REM Date Type to Use (I/P)      DATE_TO_USE$
            inpmessage$ = "Enter 'I' to Use the Invoice Date or 'P' to "&~
                "Use the Posting Date."
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,63%) = "SLMCOMM: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40200,         /* RANGE OF SALESMEN*/~
                                    L40200,         /* INVOICE DATE RANG*/~
                                    L40200,         /* Date Type to Use */~
                                    L40200,         /* PRINT DETAIL Y/N */~
                                    L40200          /* Print Costs? Y/N */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40270

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Commission Report",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Range of Salespeople:",                      ~
               at (06,30), fac(lfac$( 1)), low_salm$            , ch(04),~
               at (06,42), fac(lfac$( 1)), high_salm$           , ch(04),~
                                                                         ~
               at (07,02), "Invoice Date Range:",                        ~
               at (07,30), fac(lfac$( 2)), low_date$            , ch(10),~
               at (07,42), fac(lfac$( 2)), high_date$           , ch(10),~
               at (07,54), fac(hex(8c)),   lastpur$             , ch(24),~
                                                                         ~
               at (08,02), "Date Type to Use (I or P):",                 ~
               at (08,30), fac(lfac$( 3)), date_to_use$         , ch(01),~
                                                                         ~
               at (09,02), "Print Detail? (Y or N):",                    ~
               at (09,30), fac(lfac$( 4)), detl_yorn$           , ch(01),~
                                                                         ~
               at (10,02), "Print Unit Cost? (Y or N):",                 ~
               at (10,30), fac(lfac$( 5)), cost_yorn$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40640
                  call "MANUAL" ("SLMCOMM ")
                  goto L40270

L40640:        if keyhit% <> 15 then L40680
                  call "PRNTSCRN"
                  goto L40270

L40680:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* RANGE OF SALESMEN*/~
                                    L50190,         /* INVOICE DATE RANG*/~
                                    L50540,         /* Date Type to Use */~
                                    L50420,         /* PRINT DETAIL Y/N */~
                                    L50480          /* Print Costs? Y/N */
                  return

L50130:     REM RANGE OF SALESMEN              LOW_SALM$, HIGH_SALM$
            call "TESTRNGE" (low_salm$, high_salm$, from_salm$,          ~
                to_salm$, errormsg$)
            from_salm$ = addc (hex(01))
                return

L50190:     REM INVOICE DATE RANGE             LOW_DATE$, HIGH_DATE$
            fr_date% = 0% :  to_date% = 99999999%
            if low_date$ = " " then low_date$ = "FIRST"
            if low_date$ = "ALL" then L50380
            if low_date$ = "FIRST" then L50340
                call "DATEOKC" (low_date$, fr_date%, errormsg$)
                if errormsg$ <> " " then return
                if not (fr_date% < purge_date%) then goto L50300
                     errormsg$ = "The FROM date conflicts with the Date"&~
                          " Last Purged"
                     return
L50300:         if high_date$ = "LAST" then L50380
                if high_date$ <> " " then L50360
                     high_date$ = low_date$ : to_date% = fr_date%
                     goto L50380
L50340:     if high_date$ = " " then high_date$ = "LAST"
            if high_date$ = "LAST" then L50380
L50360:     call "DATEOKC" (high_date$, to_date%, errormsg$)
            if errormsg$ <> " " then return
L50380:     if fr_date% > to_date% then errormsg$ =                      ~
                "The FROM Date must be EARLIER than the TO Date"
            return

L50420: REM PRINT DETAIL Y OR N                    DETL_YORN$
            if detl_yorn$ = "Y" then return
            if detl_yorn$ = "N" then return
            errormsg$ = "You must enter either a 'Y' or an 'N'"
            return

L50480: REM Print Costs? Y OR N                    COST_YORN$
            if cost_yorn$ = "Y" then return
            if cost_yorn$ = "N" then return
            errormsg$ = "You must enter either a 'Y' or an 'N'"
            return

L50540: REM Date Type to Use ? (I/P)               DATE_TO_USE$
            if date_to_use$ = "I" then return
            if date_to_use$ = "P" then return
            errormsg$ = "You must enter either an 'I' or a 'P'."
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                       SLMCOMM####~
        ~###
L60070: %                              ##################################~
        ~####################################                      PAGE: #~
        ~###
L60100: %                          ######################################~
        ~##########################################
L60120: %     SALESPERSON: #### ################################
L60130: %### DATE INVOICE# SEQ CTGY PART NUMBER              QTY SHIP    ~
        ~PRICE DISC  EXTENSION  ######## APPLIED BASE AMOUNT RATE  COMMISS~
        ~ION
L60160: %-------- -------- --- ---- ------------------------ -------- ---~
        ~----- ---- ---------- ######### ------- ----------- ---- --------~
        ~---
L60190: %######## ######## ### #### #####################################~
        ~#################################################################~
        ~###
L60220: %         ######## ######### ############################## #####~
        ~ ###.##% ###### ###%  ############### -#,###,###.##    -#,###,###~
        ~.##
L60250: %### DATE INVOICE# CUSTOMER# CUSTOMER NAME                       ~
        ~    DISC        SPLIT                   BASE AMOUNT       COMMISS~
        ~ION
L60280: %-------- -------- --------- ------------------------------      ~
        ~   -----        -----                   -----------      --------~
        ~---
L60310: %                                                                ~
        ~                                        -----------      --------~
        ~---
L60340: %                                   #############################~
        ~##################################### -#,###,###.##    -#,###,###~
        ~.##
L60370: %########                           #############################~
        ~##################################### -#,###,###.##    -#,###,###~
        ~.##
L60400: %                  SALESPERSON   SALESPERSON'S NAME              ~
        ~                 BASE AMOUNT     COMMISSION
L60420: %                  -----------   --------------------------------~
        ~            ---------------- --------------
L60440: %                      ####      ################################~
        ~            -####,###,###.## -##,###,###.##
L60460: %         #######################################################~
        ~########### -####,###,###.## -##,###,###.##
L60480: %                                                                ~
        ~            ---------------- --------------
L60500: %                                                       ** END OF~
        ~ SALESPERSON **
L60520: %                                                        ** END O~
        ~F REPORT **

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
