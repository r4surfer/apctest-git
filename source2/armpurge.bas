        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  PPPP   U   U  RRRR    GGG   EEEEE   *~
            *  A   A  R   R  MM MM  P   P  U   U  R   R  G      E       *~
            *  AAAAA  RRRR   M M M  PPPP   U   U  RRRR   G GGG  EEEE    *~
            *  A   A  R   R  M   M  P      U   U  R   R  G   G  E       *~
            *  A   A  R   R  M   M  P       UUU   R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMPURGE - Allows controlled purging of A/R Invoices and  *~
            *            checks.                                        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/13/88 ! Original                                 ! ERN *~
            * 10/17/88 ! Added Multi-currency files to purge.     ! JDH *~
            * 04/27/89 ! Added ARIMSCUR and CRCL2CUR to purge.    ! MLJ *~
            * 11/21/89 ! Added CRCMSCUR too! Let's get them all.  ! JDH *~
            * 11/30/89 ! Fixed Customer name on purged checks list! JDH *~
            *          !  & honoring "Check ref'd in TB" flag.    !     *~
            * 07/08/91 ! Fixed FS95 problem on delete of CRCLINES.! JDH *~
            * 04/10/95 ! PRr 13187 - Xref modification, Delete    ! RJH *~
            *          !  Invoice Xref parts from Shadow File.    !     *~
            * 07/26/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            amt$10,                      /* For printing               */~
            blankdate$8,                 /* Blank Date for Comparisons */~
            billto$9,                    /* Bill-to Customer Number    */~
            bol$3,                       /* Bill of Lading Number      */~
            chk_ctl$17,                  /* Check break control        */~
            chk_date$8,                  /*       Date                 */~
            chk_nr$8,                    /*       Number               */~
            chk_posted$8,                /*       Post Date            */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            cusname$30,                  /* Customer Name              */~
            custype$2,                   /*          Type Code         */~
            date$8,                      /* Date for screen display    */~
            descr$30,                    /* Misc description field     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hstry$(14)79,                /* History- Screen lines      */~
            hstry_hdrs$(2)79,            /*          Screen headers    */~
            h_key$20, h_status$1,        /*          Key & Status      */~
            hc_date$8, hc_types$(2)5,    /*          Check Params      */~
            hc_yns$(3)1,                 /*                            */~
            hi_date$8, hi_types$(2)5,    /*          Invoice Params    */~
            hi_yns$(4)1,                 /*                            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inpmessage2$79,              /* Informational Message      */~
            inv_date$8,                  /* Invoice Date               */~
            invoice$8,                   /*         Number             */~
            last_by$3,                   /* User who ran last purge    */~
            last_hdr$15, last_hdr_fac$1, /* Screen header display      */~
            last_purged$8,               /* Date last purge ran        */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mode$5,                      /* File Open Mode             */~
            open_mode$5,                 /* Open mode instructions     */~
            opt_chk_date$(2)8,           /* Chk Purge through date     */~
            opt_chk_list$(2)1,           /*     Print list of purges?  */~
            opt_chk_orphans$(2)1,        /*     Purge Orphans?         */~
            opt_chk_tb$(2)1,             /*     Purge if in TB?        */~
            opt_chk_type$(6)5,           /*     Cust Type Range        */~
            opt_inv_date$(2)8,           /* Inv Purge through date     */~
            opt_inv_list$(2)1,           /*     Print list of purges?  */~
            opt_inv_orphans$(2)1,        /*     Purge Orphans?         */~
            opt_inv_so$(2)1,             /*     Purge if in SO file?   */~
            opt_inv_tb$(2)1,             /*     Purge if in TB?        */~
            opt_inv_type$(6)5,           /*     Cust Type Range        */~
            pf$(3)79, pf2$(3)79,         /* PF Screen Literals         */~
            pfkeys$32, pfkeys2$32,       /*    Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16,                       /* Purchase Order Number      */~
            printer_open$1,              /* Printer opened? (Y/ )      */~
            purge_key$20,                /* SYSFILE2 key for control   */~
            readkey$99,                  /* Misc Read Key Variable     */~
            restart$10,                  /* Processing restart flag    */~
            runtime$8,                   /* Report start time          */~
            ship_date$8,                 /* Invoice Ship date          */~
            shipto$9,                    /* Ship-to Customer Number    */~
            so$16,                       /* Sales Order Number         */~
            status$1,                    /* Purge Status (D/1/2/3/C)   */~
            stlmnt$12,                   /* Settlement Number          */~
            textid$4,                    /* Text ID                    */~
            thru_date$8,                 /* Fmtd purge through date    */~
            tos$(2)2,                    /* "to" prompts               */~
            types$(3)14,                 /* Type Range for report      */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20, axd$4           /* Text from file opening     */

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
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! CUSTOMER ! Customer Master File                     *~
            * # 4 ! TXTFILE  ! System Text File                         *~
            * # 5 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * # 6 ! GENCODES ! General Codes File                       *~
            * # 7 ! BCKMASTR ! Sales Order Header File                  *~
            * #10 ! ARIMASTR ! Invoice Master File                      *~
            * #11 ! ARILINES ! Invoice Line Items File                  *~
            * #12 ! CRCMASTR ! Cash Receipts Check Header File          *~
            * #13 ! CRCLINES ! Cash Receipts Check Detail File          *~
            * #42 ! ARILNCUR ! Currency specific counterpart of ARILINES*~
            * #43 ! CRCLNCUR ! Currency-specific counterpart of CRCLINES*~
            * #44 ! CRCMSCUR ! Currency-specific counterpart of CRCMASTR*~
            * #45 ! ARIMSCUR ! Currency-specific counterpart of ARIMASTR*~
            * #46 ! CRCL2CUR ! Currency-specific counterpart of CRCLINES*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 3, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup     ~

            select # 4, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select # 5, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21                      ~

            select # 6, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select # 7, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #10, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17                      ~

            select #11, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20                      ~

            select #12, "CRCMASTR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  17                      ~

            select #13, "CRCLINES",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  21                      ~

            select #42, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  24

            select #43, "CRCLNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  21,                     ~
                        alt key  1, keypos =    1, keylen =  25

            select #44, "CRCMSCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 17,                      ~
                        alt key  1, keypos =  1, keylen =  21

            select #45, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  17,                     ~
                        alt key  1, keypos =    1, keylen =  21

            select #46, "CRCL2CUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  21,                     ~
                        alt key  1, keypos =    1, keylen =  21

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            gosub startup_control  /* Checks for restart & load deflts */

            str(line2$,62) = "ARMPURGE: " & str(cms2v$,,8)
            last_hdr_fac$  = hex(8c)
            if last_purged$ = " " or last_purged$ = blankdate$ then L10000
                call "DATEFMT" (last_purged$)
                last_hdr$     = last_purged$ & " by " & last_by$
                last_hdr_fac$ = hex(ac)


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            proceed% = 0%  :  edit% = 1%

            for fieldnr% = 1% to 11%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
                if proceed% <> 2% then L10120
                     if fieldnr% = 7% then proceed% = 0% else L10240
L10120:         if proceed% = 1% then L10240
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10100
                         goto L10160
L10210:               if keyhit% =  6% then proceed% = 1%
                      if keyhit% = 16% and  fieldnr% = 1% then abort_pgm
                      if keyhit% <> 0% and  keyhit% <> 6% then L10130
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 16% then       begin_purge
                  if keyhit% <>  0% then       editpg1
L11111:     if cursor%(1) >= 14% then cursor%(1) = cursor%(1) - 2%
            fieldnr% = max(1%, min(cursor%(1%) - 5%, 11%))
            if fieldnr% < 1% or fieldnr% > 11% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% and keyhit% <> 3% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
                  goto L11111

        REM *************************************************************~
            *               B E G I N   P U R G E                       *~
            *-----------------------------------------------------------*~
            * Open files per mode specified and delete, delete, delete. *~
            *************************************************************
        begin_purge

*        First make sure that there is something to do...
            if (opt_inv_date$(1%) <> " " and opt_inv_date$(1%) <> blankdate$) ~
            or (opt_chk_date$(1%) <> " " and opt_chk_date$(1%) <> blankdate$) ~
            or opt_inv_orphans$(1) = "Y" or opt_chk_orphans$(1) = "Y"    ~
                                                              then L12190
L12110:         u3% = 2%
                call "ASKUSER" (u3%, "NOTHING TO DO",                    ~
                                "There is nothing specified to do.",     ~
                                "Press RETURN to EXIT program, -OR-",    ~
                                "Press PF-1 to RETURN to data entry.")
                if u3% = 0% then abort_pgm
                if u3% = 1% then editpg1   else L12110

L12190
*        Now let's open files per mode specified...
L12200:     gosub open_mode_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <> 16% then       L12200
            mode$ = "SHARE"
            if str(open_mode$,1,1) <> " " then mode$ = "IO"
                f9% =  3%  :  gosub open_file  :  mode$ = "SHARE"
            if str(open_mode$,2,1) <> " " then mode$ = "IO"
                f9% =  7%  :  gosub open_file  :  mode$ = "SHARE"
            if str(open_mode$,3,1) <> " " then mode$ = "IO"
                f9% =  5%  :  gosub open_file  :  mode$ = "SHARE"
            if str(open_mode$,4,1) <> " " then mode$ = "IO"
                f9% = 10%  :  gosub open_file
                f9% = 11%  :  gosub open_file  :  mode$ = "SHARE"
                f9% = 42%  :  gosub open_file  :  mode$ = "SHARE"
                f9% = 45%  :  gosub open_file  :  mode$ = "SHARE"
            if str(open_mode$,5,1) <> " " then mode$ = "IO"
                f9% = 12%  :  gosub open_file
                f9% = 13%  :  gosub open_file  :  mode$ = "SHARE"
                f9% = 43%  :  gosub open_file  :  mode$ = "SHARE"
                f9% = 44%  :  gosub open_file  :  mode$ = "SHARE"
                f9% = 46%  :  gosub open_file  :  mode$ = "SHARE"
            goto L12420

            open_file
                call "OPENFILE" (#f9%, mode$, f2%(f9%), rslt$(f9%), axd$)
                return

L12420
*       ** Verify that required files are indeed open...
            if opt_inv_date$(1%) = " " or opt_inv_date$(1%) = blankdate$ ~
                    then L12510
                if opt_inv_type$(1) = "ALL" or f2%(3) = 0% then L12460
                    descr$ = "CUSTOMER MASTER"     : goto open_error
L12460:         if opt_inv_tb$(1) = "Y" or f2%(5) = 0% then L12480
                    descr$ = "A/R TRIAL BALANCE"   : goto open_error
L12480:         if opt_inv_so$(1) = "Y" or f2%(7) = 0% then L12510
                    descr$ = "SALES ORDER MASTER"  : goto open_error

L12510:         if opt_inv_orphans$(1) = "N" or f2%(10) = 0% then L12540
                    descr$ = "INVOICE HEADERS"     : goto open_error

L12540:     if opt_chk_date$(1%) = " " or opt_chk_date$(1%) = blankdate$ ~
                    then L12600
                if opt_chk_type$(1) = "ALL" or f2%(3) = 0% then L12570
                    descr$ = "CUSTOMER MASTER"     : goto open_error
L12570:         if opt_chk_tb$(1) = "Y" or f2%(5) = 0% then L12600
                    descr$ = "A/R TRIAL BALANCE"   : goto open_error

L12600:         if opt_chk_orphans$(1) = "N" or f2%(12) = 0% then purge
                    descr$ = "CHECK HEADERS"       : goto open_error

          open_error:
            if restart$ = " " then L12710
            call "ASKUSER" (2%,  "FILE OPEN ERROR",                      ~
                            "Unable to open required file: " & descr$,   ~
                            "Please resolve problem and rerun program.", ~
                            "Press RETURN to ABORT running of program.")
            goto exit_program

L12710:     u3% = 2%
            call "ASKUSER" (u3%, "FILE OPEN ERROR",                      ~
                            "Unable to open required file: " & descr$,   ~
                            "Press PF- 1 to RETURN to entry screens;",   ~
                            "Press PF-16 to ABORT running of program.")
            if u3% = 16% then abort_pgm
            if u3% <> 1% then L12710
                gosub close_files
                goto  editpg1


        REM *************************************************************~
            *                 P U R G A T O R Y                         *~
            *-----------------------------------------------------------*~
            * Where senile records are exonerated and released.         *~
            *************************************************************
        purge

*        First take care of record keeping responsibilities...
            call "DATUNFMT" (opt_inv_date$(1))
            call "DATUNFMT" (opt_chk_date$(1))
            if status$ = "D" then status$ = "1"
            call "READ101" (#2, purge_key$, f1%(2))
            put #2 using L30175, purge_key$, date, userid$, status$,      ~
                                opt_inv_date$(1)   ,                     ~
                                opt_inv_type$(1)   , opt_inv_type$(2)   ,~
                                opt_inv_tb$(1)     , opt_inv_so$(1)     ,~
                                opt_inv_orphans$(1), opt_inv_list$(1)   ,~
                                opt_chk_date$(1)   ,                     ~
                                opt_chk_type$(1)   , opt_chk_type$(2)   ,~
                                opt_chk_tb$(1)     , opt_chk_orphans$(1),~
                                opt_chk_list$(1)   , " ", " "
            rewrite #2
            call "SHOSTAT" ("A/R Purge In Process...")
            if status$ = "2" then purge_inv_orphans
            if status$ = "3" then purge_checks

        REM *************************************************************~
            *           P U R G E   I N V O I C E  F I L E S            *~
            *-----------------------------------------------------------*~
            * Purge the invoice header and line files.  Purge is one    *~
            * pass, via the headers, and orphans is a separate pass.    *~
            * Text has to be deleted with both the headers and lines.   *~
            *************************************************************

            if opt_inv_date$(1%) = " " or opt_inv_date$(1%) = blankdate$ ~
                then purge_inv_orphans

            if opt_inv_list$(1) = "N" then invoice_loop
                thru_date$ = opt_inv_date$(1)
                call "DATEFMT" (thru_date$)
                types$(3)  = opt_inv_type$(1)
                if opt_inv_type$(2) > " " then types$(3) = types$(3) &   ~
                                                " to " & opt_inv_type$(2)
                gosub open_printer
                gosub inv_page_heading

        invoice_loop:
            call "READNEXT" (#10, f1%(10))
            if f1%(10) = 0% then end_invoices

            get #10 using L15260, shipto$, invoice$, po$, so$, bol$,      ~
                                 cusname$, ship_date$, inv_date$,        ~
                                 amt, billto$, stlmnt$
L15260:         FMT CH(9), CH(8), 2*CH(16), CH(3), CH(30), POS(413),     ~
                    CH(6), POS(521), CH(6), POS(833), PD(14,4), POS(849),~
                    CH(9), CH(12)
            inv_hdrs_read% = inv_hdrs_read% + 1%
            if str(invoice$,,4) = "RCR-"    then invoice_loop
            if inv_date$ > opt_inv_date$(1) then invoice_loop
            if str(opt_inv_type$(3),,1) = hex(00) and                    ~
               str(opt_inv_type$(4),,1) = hex(ff) then L15400
                if shipto$ = last_cus$ then L15380
                     call "READ100" (#3, shipto$, f1%(3))
                     if f1%(3) = 0%  then L15400
                     get #3 using L15370, last_cus$, custype$
L15370:                   FMT CH(9), POS(1023), CH(2)
L15380:         if custype$ < opt_inv_type$(3) or                        ~
                   custype$ > opt_inv_type$(4) then invoice_loop
L15400:     if opt_inv_tb$(1) <> "N" then L15440
                readkey$ = str(billto$,,9) & stlmnt$
                call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 1% then invoice_loop
L15440:     if opt_inv_so$(1) <> "N" then L15480
                readkey$ = str(shipto$,,9) & so$
                call "READ100" (#7, readkey$, f1%(7))
                if f1%(7) = 1% then invoice_loop
L15480:     gosub purge_invoice
            goto  invoice_loop

        end_invoices:
            if opt_inv_list$(1) = "Y" then gosub end_inv_print


        purge_inv_orphans:
            if opt_inv_orphans$(1) = "N" then purge_checks

            call "READ101" (#2, purge_key$, f1%(2))   /* Update Status */
            put #2 using L15610, "2"
L15610:         FMT POS(30), CH(1)
            rewrite #2

            plowkey$ = all(hex(00))      /* Get to front of file       */

        inv_orphan_loop:
            call "PLOWNEXT" (#11, plowkey$, 0%, f1%(11))
            if f1%(11) = 0% then purge_checks
                readkey$ = str(plowkey$,,17)
                call "READ100" (#10, readkey$, f1%(10))
                if f1%(10) = 0% then L15740
                     str(plowkey$,18,3) = hex(ffffff)
                     goto inv_orphan_loop
L15740:         str(plowkey$,18,3) = hex(000000)
                gosub purge_xref_parts
                gosub purge_inv_lines
                inv_orphans% = inv_orphans% + lines_purged%
                goto inv_orphan_loop


        REM *************************************************************~
            *            P U R G E   C H E C K   F I L E S              *~
            *-----------------------------------------------------------*~
            * Purge the check header and lines files.  Input is via     *~
            * the line items file to allow purging and orphans to be    *~
            * done in one pass.                                         *~
            *************************************************************

        purge_checks:
            if (opt_chk_date$(1%) = " " or opt_chk_date$(1%) = blankdate$) ~
                 and opt_chk_orphans$(1%) = "N" then end_purge

*        Update the control record to make restart effecient.
            call "READ101" (#2, purge_key$, f1%(2))   /* Update Status */
            put #2 using L15610, "3"
            rewrite #2

            last_cus$ = hex(00)

            if opt_chk_list$(1) = "N" then L16280
                thru_date$ = opt_chk_date$(1)
                call "DATEFMT" (thru_date$)
                types$(3)  = opt_chk_type$(1)
                if opt_chk_type$(2) > " " then types$(3) = types$(3) &   ~
                                               " to " & opt_chk_type$(2)
                gosub open_printer
                gosub chk_page_heading

L16280
*        Make an exception regarding the first check line...
            goto next_check

        check_loop:
            call "READNEXT" (#13, f1%(13))         /* CRCLINES */
            if f1%(13) = 0% then L16430

            if str(key(#13),,17%) <> chk_ctl$ then L16430
L16360:         if opt_chk_tb$(1) <> "N" then check_loop
                     get #13 using L16380, billto$, stlmnt$
L16380:                   FMT CH(9), POS(23), CH(12)
                     readkey$ = str(billto$,,9) & stlmnt$
                     call "READ100" (#5, readkey$, f1%(5))
                     if f1%(5) = 0% then check_loop else next_check

L16430:     gosub purge_check

          next_check
            plowkey$ = str(chk_ctl$,,17) & hex(ffffff)
            call "PLOWNEXT" (#13, plowkey$, 0%, f1%(13))
            if f1%(13) = 0% then end_check_purge

            chk_ctl$ = str(key(#13),,17%)
            call "READ100" (#12, chk_ctl$, f1%(12))
            if f1%(12) = 1% then L16560
                if opt_chk_orphans$(1) = "Y"                             ~
                                           then gosub purge_check_orphans
                goto next_check
L16560:     if opt_chk_date$(1%) = " " or opt_chk_date$(1%) = blankdate$ ~
                    then next_check
            get #12 using L16590, billto$, chk_nr$, chk_date$, amt,       ~
                                 chk_posted$
L16590:         FMT CH(9), CH(8), CH(6), POS(40), PD(14,4), POS(108),    ~
                    CH(6)
            if chk_date$ > opt_chk_date$(1) then next_check
            if str(opt_chk_type$(3),,1) = hex(00) and                    ~
               str(opt_chk_type$(4),,1) = hex(ff) then L16360
                if billto$ = last_cus$ then L16690
                     call "READ100" (#3, billto$, f1%(3))
                     if f1%(3) = 0%  then L16710
                     get #3 using L16680, last_cus$, custype$
L16680:                   FMT CH(9), POS(1023), CH(2)
L16690:         if custype$ < opt_chk_type$(3) or                        ~
                   custype$ > opt_chk_type$(4) then next_check
L16710:         goto L16360


        end_check_purge
            if opt_chk_list$(1) = "Y" then gosub end_chk_print


        REM *************************************************************~
            *                   E N D   P U R G E                       *~
            *-----------------------------------------------------------*~
            * Update statuses, display results, and quit.               *~
            *************************************************************

        end_purge:
            call "SETPRNT" ("ARM014", " ", 0%, 1%)
            close printer
            gosub close_files

            call "READ101" (#2, purge_key$, f1%(2))   /* Update Status */
            put #2 using L15610, "C"
            rewrite #2

            call "READ101" (#2, "LAST PURGE DATES", f1%(2))
            if f1%(2) = 1% then L18190
                put #2 using L18180, "LAST PURGE DATES", " ", " "
L18180:              FMT CH(20), 2*CH(240)
L18190:     get #2 using L18200, last_purged$
L18200:              FMT POS(21), CH(6)
            if last_purged$ > opt_inv_date$(1)  or                       ~
               last_purged$ > opt_chk_date$(1) then L18260
            if opt_inv_date$(1) < opt_chk_date$(1)                then   ~
                                  last_purged$ = opt_inv_date$(1) else   ~
                                  last_purged$ = opt_chk_date$(1)
L18260:     put #2 using L18200, last_purged$
            if f1%(2) = 0% then write #2 else rewrite #2


*        Display results screen and then get out of Dodge...
L18310:     gosub results_screen
            if keyhit% <> 16% then L18310
            goto exit_program


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Inv Purge to Date      */~
                              L20150,         /* Inv Cus Type Rnge      */~
                              L20200,         /* Purge Inv In TB?       */~
                              L20250,         /* Purge Inv if SO?       */~
                              L20300,         /* Purge Inv Orphans      */~
                              L20350,         /* Print Inv List         */~
                              L20400,         /* Chk Purge to Date      */~
                              L20450,         /* Chk Cus Type Rnge      */~
                              L20500,         /* Purge Chk in TB?       */~
                              L20550,         /* Purge Chk Orphans      */~
                              L20600          /* Print Chk List?        */
            return

L20100: REM Def/Enable  Purge Through Invoice Date OPT_INV_DATE$(2)
            return

L20150: REM Def/Enable  Customer Type Range        OPT_INV_TYPE$(6)
            tos$(1) = "to"
            return

L20200: REM Def/Enable  Purge if in Trial Balance? OPT_INV_TB$(2)
            return

L20250: REM Def/Enable  Purge if SO Order on file? OPT_INV_SO$(2)
            return

L20300: REM Def/Enable  Purge Orphaned Line Items? OPT_INV_ORPHANS$(2)
            return

L20350: REM Def/Enable  Print List of Purged Inv?  OPT_INV_LIST$(2)
            return

L20400: REM Def/Enable  Purge Through Check Date   OPT_CHK_DATE$(2)
            return

L20450: REM Def/Enable  Customer Type Range        OPT_CHK_TYPE$(6)
            tos$(2) = "to"
            return

L20500: REM Def/Enable  Purge if in Trial Balance? OPT_CHK_TB$(2)
            return

L20550: REM Def/Enable  Purge Orphaned Line Items? OPT_CHK_ORPHANS$(2)
            return

L20600: REM Def/Enable  Print List of Purged Chks? OPT_CHK_LIST$(2)
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Date or '-##' to calculate.  Set blank for no purge.   ",~
         "Enter Customer Type Code range (may use ALL, FIRST, & LAST). ",~
         "Purge Invoice if it is still found in the Trial Balance?(Y/N)",~
         "Purge Invoice if Sales Order is still on file? (Y/N)         ",~
         "Purge Orphaned Invoice Line Items? (Y/N)                     ",~
         "Print Listing of Purged Invoices? (Y/N)                      ",~
         "Enter Date or '-##' to calculate.  Set blank for no purge.   ",~
         "Enter Customer Type Code range (may use ALL, FIRST, & LAST). ",~
         "Purge Check if it is still found in the Trial Balance? (Y/N) ",~
         "Purge Orphaned Check Line Items? (Y/N)                       ",~
         "Print Listing of Purged Checks? (Y/N)                        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables:
            init(" ") errormsg$, inpmessage$,                            ~
                      opt_chk_date$(), opt_chk_list$(),                  ~
                      opt_chk_orphans$(), opt_chk_tb$(),                 ~
                      opt_chk_type$(), opt_inv_date$(),                  ~
                      opt_inv_list$(), opt_inv_orphans$(),               ~
                      opt_inv_so$(), opt_inv_tb$(), opt_inv_type$(),     ~
                      purge_key$, last_purged$, last_by$, last_hdr$,     ~
                      restart$, tos$(), types$(), hstry$()
            i% = 0%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
                if restart$ <> " " then exit_program
                call "READ101" (#2, purge_key$, f1%(2))
                if f1%(2) = 1% then delete #2
                goto L09000

        REM *************************************************************~
            *             S T A R T   U P   C O N T R O L               *~
            *-----------------------------------------------------------*~
            * Determines startup status and prepares control record.    *~
            *************************************************************
        startup_control:
            gosub initialize_variables
            plowkey$ = "ARM.PURGE." & hex(00)
            call "PLOWNXT1" (#2, plowkey$, 10%, f1%(2))
            if f1%(2) = 1% then L30120

*        First purge ever.  Set default values.
            this_purge% = 65535%
            call "DATE" addr("G+", date, -90%, opt_inv_date$(1), err%)
            opt_inv_type$(1)   , opt_chk_type$(1) = "ALL"
            opt_inv_type$(3)   , opt_chk_type$(3) = all(hex(00))
            opt_inv_type$(4)   , opt_chk_type$(4) = all(hex(ff))
            opt_inv_tb$(1)     , opt_inv_so$(1)   = "N"
            opt_chk_date$(1)                      = opt_inv_date$(1)
            opt_chk_tb$(1)                        = "N"
            opt_inv_orphans$(1), opt_inv_list$(1) = "N"
            opt_chk_orphans$(1), opt_chk_list$(1) = "N"
            goto common_with_first_time

L30120
*        Read control record and determine status.
            get #2 using L30175, purge_key$, last_purged$, last_by$,      ~
                                status$,                                 ~
                                opt_inv_date$(2)   ,                     ~
                                opt_inv_type$(5)   , opt_inv_type$(6)   ,~
                                opt_inv_tb$(2)     , opt_inv_so$(2)     ,~
                                opt_inv_orphans$(2), opt_inv_list$(2)   ,~
                                opt_chk_date$(2)   ,                     ~
                                opt_chk_type$(5)   , opt_chk_type$(6)   ,~
                                opt_chk_tb$(2)     , opt_chk_orphans$(2),~
                                opt_chk_list$(2)
L30175:         FMT CH(20), CH(6), CH(3), CH(1), CH(6), 2*CH(5), 4*CH(1),~
                    CH(6), 2*CH(5), 3*CH(1), CH(231), CH(200)
            get purge_key$ using L30190, this_purge%
L30190:         FMT POS(11), BI(2)
            on pos("D123C" = status$) goto data_entry, inprocess,        ~
                                      inprocess, inprocess, completed

        data_entry:
            if last_by$ <> userid$ then notify_in_process
                call "SHOSTAT" ("Restarting... Status = Data Entry.")
                delete #2
                goto startup_control

        inprocess:
            if last_by$ <> userid$ then notify_in_process
                call "SHOSTAT" ("Restarting... Status = In-Process(" &   ~
                                               status$ & ").")
                get #2 using L30175, purge_key$, last_purged$, last_by$,  ~
                                   status$, opt_inv_date$(1),            ~
                                   opt_inv_type$(1), opt_inv_type$(2),   ~
                                   opt_inv_tb$(1), opt_inv_so$(1),       ~
                                   opt_inv_orphans$(1), opt_inv_list$(1),~
                                   opt_chk_date$(1),                     ~
                                   opt_chk_type$(1), opt_chk_type$(2),   ~
                                   opt_chk_tb$(1), opt_chk_orphans$(1),  ~
                                   opt_chk_list$(1)
                restart$ = "RESTART"
                call "STRTRLSE" (#2)
                gosub format_data
                return clear
                goto begin_purge

        completed:  /* Set up purge using the last options as defaults */
            this_purge% = this_purge% - 1%
            if opt_inv_date$(2%) = " " or opt_inv_date$(2%) = blankdate$ ~
                   then L30370
                call "DATE" addr("G-", last_purged$, opt_inv_date$(2),   ~
                                                               u3%, err%)
                if err% <> 0% then u3% = -90%
                call "DATE" addr("G+", date, u3%, opt_inv_date$(1), err%)
L30370:     opt_inv_type$   (1%) = opt_inv_type$   (5%)
            opt_inv_type$   (2%) = opt_inv_type$   (6%)
            opt_inv_tb$     (1%) = opt_inv_tb$     (2%)
            opt_inv_so$     (1%) = opt_inv_so$     (2%)
            opt_inv_orphans$(1%) = opt_inv_orphans$(2%)
            opt_inv_list$   (1%) = opt_inv_list$   (2%)
            if opt_chk_date$(2%) = " " or opt_chk_date$(2%) = blankdate$ ~
                   then L30425
                call "DATE" addr("G-", last_purged$, opt_chk_date$(2%),   ~
                                                               u3%, err%)
                if err% <> 0% then u3% = -90%
                call "DATE" addr("G+", date, u3%, opt_chk_date$(1%), err%)
L30425:     opt_chk_type$   (1%) = opt_chk_type$   (5%)
            opt_chk_type$   (2%) = opt_chk_type$   (6%)
            opt_chk_tb$     (1%) = opt_chk_tb$     (2%)
            opt_chk_orphans$(1%) = opt_chk_orphans$(2%)
            opt_chk_list$   (1%) = opt_chk_list$   (2%)
          common_with_first_time:
            status$ = "D"
            put purge_key$ using L30465, "ARM.PURGE.", this_purge%
L30465:         FMT CH(10), BI(2)
            put #2 using L30175, purge_key$, date, userid$, status$,      ~
                                opt_inv_date$(1)   ,                     ~
                                opt_inv_type$(1)   , opt_inv_type$(2)   ,~
                                opt_inv_tb$(1)     , opt_inv_so$(1)     ,~
                                opt_inv_orphans$(1), opt_inv_list$(1)   ,~
                                opt_chk_date$(1)   ,                     ~
                                opt_chk_type$(1)   , opt_chk_type$(2)   ,~
                                opt_chk_tb$(1)     , opt_chk_orphans$(1),~
                                opt_chk_list$(1)   , " ", " "
            write #2
            gosub format_data
            gosub load_history
            return

        notify_in_process:
            call "STRTRLSE" (#2)
            call "ASKUSER"  (0%, "PURGE IN-PROCESS",                     ~
                             "An A/R Purge has already been",            ~
                             "started by user " & last_by$ & ".",        ~
                             "Press RETURN to EXIT...")
            goto exit_program

        format_data:
            call "DATEFMT"  (opt_inv_date$(1))
            call "DATEFMT"  (opt_inv_date$(2))
            call "TESTRNGE" (opt_inv_type$(1), opt_inv_type$(2),         ~
                             opt_inv_type$(3), opt_inv_type$(4),         ~
                             errormsg$)
            call "DATEFMT"  (opt_chk_date$(1))
            call "DATEFMT"  (opt_chk_date$(2))
            call "TESTRNGE" (opt_chk_type$(1), opt_chk_type$(2),         ~
                             opt_chk_type$(3), opt_chk_type$(4),         ~
                             errormsg$)
            if opt_inv_type$(2) <> " " then tos$(1) = "to"
            if opt_chk_type$(2) <> " " then tos$(2) = "to"
            types$(1) = opt_inv_type$(5)
            if opt_inv_type$(6) <> " " then types$(1) = types$(1) &      ~
                                                " to " & opt_inv_type$(6)
            call "STRING" addr("CT", types$(1), 14%)
            types$(2) = opt_chk_type$(5)
            if opt_chk_type$(6) <> " " then types$(2) = types$(2) &      ~
                                                " to " & opt_chk_type$(6)
            call "STRING" addr("CT", types$(2), 14%)
        return


        load_history:    /* Load history into arrays.  Purge 15th      */
                         /* occurance on.                              */
            str(hstry_hdrs$(1),15) = "-------  I N V O I C E S  -------"
            str(hstry_hdrs$(1),49) = "-------   C H E C K S  -------"
            hstry_hdrs$(2) = hex(ac) & "Run Date" & hex(ac) & "By "    & ~
                             hex(ac) & "ThruDate" & hex(ac)            & ~
                             "CustomerTypes" & hex(ac) & "TB" & hex(ac)& ~
                             "SO" & hex(ac) & "Or" & hex(ac) & "P"     & ~
                             hex(ac) & "ThruDate" & hex(ac)            & ~
                             "CustomerTypes" & hex(ac) & "TB" & hex(ac)& ~
                             "Or" & hex(ac) & "P" & hex(8c)
            plowkey$ = purge_key$                  /* Skips current    */

          history_loop
            call "PLOWNXT1" (#2, plowkey$, 10%, f1%(2))
            if f1%(2) = 0% then return

            if i% < 14% then L30805
                delete #2
                goto history_loop

L30805:     get #2 using L30175, h_key$, h_date$, h_by$, h_status$,       ~
                                hi_date$, hi_types$(), hi_yns$(),        ~
                                hc_date$, hc_types$(), hc_yns$()
            i% = i% + 1%
            call "DATEFMT" (h_date$)
            str(hstry$(i%), 2) = h_date$
            str(hstry$(i%),11) = h_by$
            call "DATEFMT" (hi_date$)
            str(hstry$(i%),15) = hi_date$
            str(hstry$(i%),24) = hi_types$(1)
            if hi_types$(2) > " " then str(hstry$(i%),30) = "to " &      ~
                                                     str(hi_types$(2),,4)
            str(hstry$(i%),39) = hi_yns$(1)
            str(hstry$(i%),42) = hi_yns$(2)
            str(hstry$(i%),45) = hi_yns$(3)
            str(hstry$(i%),47) = hi_yns$(4)

            call "DATEFMT" (hc_date$)
            str(hstry$(i%),49) = hc_date$
            str(hstry$(i%),58) = hc_types$(1)
            if hc_types$(2) > " " then str(hstry$(i%),64) = "to " &      ~
                                                             hc_types$(2)
            str(hstry$(i%),73) = hc_yns$(1)
            str(hstry$(i%),76) = hc_yns$(2)
            str(hstry$(i%),78) = hc_yns$(3)
            goto history_loop

        REM *************************************************************~
            *                P U R G E   R O U T I N E S                *~
            *-----------------------------------------------------------*~
            * Remove transactions from files.                           *~
            *************************************************************

        purge_invoice:
            plowkey$ = str(shipto$,,9) & str(invoice$,,8) & hex(000000)
            gosub purge_inv_lines
            inv_lines_purged% = inv_lines_purged% + lines_purged%

            if opt_inv_list$(1) = "Y" then gosub print_invoice
            get #10 using L31005, textid$
L31005:         FMT POS(901), CH(4)
            call "DELETE"   (#10, plowkey$, 17%)
            call "DELETE"   (#45, plowkey$, 17%)
            call "TXTFUTIL" (#4 , f2%(4), "DELE", textid$)
            gosub purge_xref_parts
            inv_hdrs_purged% = inv_hdrs_purged% + 1%
            return

        purge_inv_lines:       /* Set PLOWKEY$ before calling!         */
            lines_purged% = 0%
          inv_line_loop
            call "PLOWNEXT" (#11, plowkey$, 17%, f1%(11))
            if f1%(11) = 0% then return
                get #11 using L31065, textid$
L31065:              FMT POS(190), CH(4)
                call "READ101" (#42, key(#11), f1%(42))
                if f1%(42) <>0% then delete #42
                call "TXTFUTIL" (#4, f2%(4), "DELE", textid$)
                call "DELETE" (#11, key(#11), 17%)
                lines_purged% = lines_purged% + 1%
                goto inv_line_loop

        purge_check:
            gosub purge_check_lines
            chk_lines_purged% = chk_lines_purged% + lines_purged%
            call "DELETE" (#12, str(chk_ctl$,,17), 17%)
            call "DELETE" (#44, str(chk_ctl$,,17), 17%) /* Shadow Away */
            chk_hdrs_purged% = chk_hdrs_purged% + 1%
            if opt_chk_list$(1) = "Y" then gosub print_check
            return

        purge_check_orphans:
            gosub purge_check_lines
            chk_orphans% = chk_orphans% + lines_purged%
            return

        purge_check_lines:
            lines_purged% = 0%
            plowkey$ = str(chk_ctl$,,17) & hex(000000)
L31170:     call "PLOWNEXT" (#13, plowkey$, 17%, f1%(13))
            if f1%(13) = 0% then return
                call "READ101" (#43, key(#13), f1%(43))
                if f1%(43) <> 0% then delete #43
                call "READ101" (#46, key(#13), f1%(46))
                if f1%(46) <> 0% then delete #46
                call "DELETE" (#13, key(#13), 17%)
                lines_purged% = lines_purged% + 1%
                goto L31170


        purge_xref_parts
                call "PTUSEDSB" ("D", "ARI ", str(plowkey$,,17%), "ALL", ~
                                 " ", " ", " ", ret%)/* Del Xref Shadow */
                ret% = ret%  /* do nothing line */
            return

        REM *************************************************************~
            *        I N V O I C E   R E P O R T   R O U T I N E S      *~
            *-----------------------------------------------------------*~
            * Routines and Image statements for printing of reports.    *~
            *************************************************************

        open_printer:
            if printer_open$ = "Y" then return
                call "TIME" (runtime$)
                call "COMPNAME" (12%, company$, u3%)
                line% = 857%
                select printer(134)
                call "SETPRNT" ("ARM014", " ", 0%, 0%)
                printer_open$ = "Y"
                return

        inv_page_heading:
            page% = page% + 1%  :  line% = 8%
            print page
            print using L32490, date$, runtime$, company$
            print using L32510, page%
            print
            print using L32530, thru_date$, opt_inv_tb$(1)
            print using L32550, types$(3) , opt_inv_so$(1)
            print
            print using L32570
            print using L32590
            print using L32610
            return

        print_invoice:
            if line% > 55% then gosub inv_page_heading
            call "DATEFMT" (ship_date$)
            call "DATEFMT" (inv_date$ )
            call "CONVERT" (amt, 2.2, amt$)
            print using L32630, shipto$, cusname$, invoice$, po$, so$,    ~
                               bol$, ship_date$, inv_date$, stlmnt$, amt$
            line% = line% + 1%
            return

        end_inv_print:
            if line% > 55% then gosub inv_page_heading
            print
            print using L32650, inv_hdrs_read%, inv_hdrs_purged%,         ~
                               inv_lines_purged%
            return



L32490: %RUN DATE: ######## ########            #########################~
        ~###################################               ARMPURGE:ARM014
L32510: %                                             ACCOUNTS RECEIVABLE~
        ~ PURGE: PURGE INVOICE FILES                           PAGE: ###
L32530: %       Purge through: ########         Purge Invoice If Referenc~
        ~ed in Trial Balance? : #
L32550: %      Customer Types: ##############   Purge Invoice If Sales Or~
        ~der is still on file?: #
L32570: % Ship-to                                  Invoice               ~
        ~                          Date     Date    Settlement     Net
L32590: %  Number  Ship-to Customer Name           Number  Customer PO   ~
        ~   Sales Order      BOL  Shipped Invoiced    Number      Amount
L32610: %--------- ------------------------------ -------- --------------~
        ~-- ---------------- --- -------- -------- ------------ ----------
L32630: %######### ############################## ######## ##############~
        ~## ################ ### ######## ######## ############ ##########
L32650: %*** END INVOICES ***   Headers read: ###,###    Headers Purged: ~
        ~###,###   Lines Purged: #####,###


        REM *************************************************************~
            *           C H E C K  R E P O R T   R O U T I N E S        *~
            *-----------------------------------------------------------*~
            * Routines and Image statements for printing of reports.    *~
            *************************************************************

        chk_page_heading:
            page% = page% + 1%  :  line% = 8%
            print page
            print using L33370, date$, runtime$, company$
            print using L33390, page%
            print
            print using L33410, thru_date$, types$(3), opt_chk_tb$(1)
            print
            print using L33430
            print using L33450
            print using L33470
            return

        print_check:
            if line% > 55% then gosub chk_page_heading
            call "DATEFMT" (chk_date$)
            call "DATEFMT" (chk_posted$)
            call "CONVERT" (amt, 2.2, amt$)
            call "DESCRIBE" (#3, billto$, cusname$, 0%, f1%(3))
            if f1%(3) = 0% then cusname$ = "No Name on File"
            print using L33490, billto$, cusname$, chk_nr$, chk_date$,    ~
                               chk_posted$, amt$
            line% = line% + 1%
            return

        end_chk_print:
            if line% > 55% then gosub inv_page_heading
            print
            print using L33510, chk_hdrs_purged%, chk_lines_purged%
            return



L33370: %RUN DATE: ######## ########            #########################~
        ~###################################               ARMPURGE:ARM014
L33390: %                                              ACCOUNTS RECEIVABL~
        ~E PURGE: PURGE CHECK FILES                            PAGE: ###
L33410: %     Purge through: ########     Customer Types: ############## ~
        ~      Purge Check If Referenced in  Trial Balance?: #
L33430: %      Bill-to                                              Check~
        ~      Post
L33450: %       Number   Bill-to Customer Name           Check Nr    Date~
        ~      Date    Chk Amount
L33470: %     ---------  ------------------------------  --------  ------~
        ~--  --------  ----------
L33490: %     #########  ##############################  ########  ######~
        ~##  ########  ##########
L33510: %*** END CHECKS ***     Headers Purged: ###,###   Lines Purged: #~
        ~####,###



        REM *************************************************************~
            *           M I S C   I / O   R O U T I N E S               *~
            *-----------------------------------------------------------*~
            * A place to put common I/O routines.                       *~
            *************************************************************

        close_files:
            if f2%( 3) <> 0% then L34080  :  close #3   :  f2%( 3) = 1%
L34080:     if f2%( 5) <> 0% then L34090  :  close #5   :  f2%( 5) = 1%
L34090:     if f2%( 7) <> 0% then L34100  :  close #7   :  f2%( 7) = 1%
L34100:     if f2%(10) <> 0% then L34110  :  close #10  :  f2%(10) = 1%
L34110:     if f2%(11) <> 0% then L34120  :  close #11  :  f2%(11) = 1%
L34120:     if f2%(12) <> 0% then L34130  :  close #12  :  f2%(12) = 1%
L34130:     if f2%(13) <> 0% then L34132  :  close #13  :  f2%(13) = 1%
L34132:     if f2%(42) <> 0% then L34133  :  close #42  :  f2%(42) = 1%
L34133:     if f2%(43) <> 0% then L34134  :  close #43  :  f2%(43) = 1%
L34134:     if f2%(45) <> 0% then L34135  :  close #45  :  f2%(45) = 1%
L34135:     if f2%(46) <> 0% then L34140  :  close #46  :  f2%(46) = 1%
L34140:     return

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
              if fieldnr% = 0% and (opt_inv_date$(1) = " " or ~
                                    opt_inv_date$(1) = blankdate$) then ~
                                   lfac$(2),lfac$(3),lfac$(4),lfac$(6) = hex(8c)
              if fieldnr% = 0% and (opt_chk_date$(1) = " " or ~
                                    opt_chk_date$(1) = blankdate$) then ~
                                    lfac$(8), lfac$(9), lfac$(11) = hex(8c)
              on fieldnr% gosub L40145,         /* Inv Purge to Date */   ~
                                L40145,         /* Inv Cus Type Rnge */   ~
                                L40145,         /* Purge Inv In TB?  */   ~
                                L40145,         /* Purge Inv if SO?  */   ~
                                L40145,         /* Purge Inv Orphans */   ~
                                L40145,         /* Print Inv List    */   ~
                                L40145,         /* Chk Purge to Date */   ~
                                L40145,         /* Chk Cus Type Rnge */   ~
                                L40145,         /* Purge Chk in TB?  */   ~
                                L40145,         /* Purge Chk Orphans */   ~
                                L40145          /* Print Chk List?   */
              goto L40160

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40145:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40160:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge A/R Invoices and Checks",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "I N V O I C E S",                            ~
               at (05,65), fac(last_hdr_fac$), last_hdr$,         ch(15),~
               at (06,02), " Purge Through Invoice Date",                ~
               at (06,40), fac(lfac$( 1)), opt_inv_date$(1)     , ch(08),~
               at (06,68), fac(hex(8c))  , opt_inv_date$(2)     , ch(08),~
                                                                         ~
               at (07,02), " Customer Type Range",                       ~
               at (07,46), fac(hex(8c))  , tos$(1)              , ch(02),~
               at (07,40), fac(lfac$( 2)), opt_inv_type$(1)     , ch(05),~
               at (07,49), fac(lfac$( 2)), opt_inv_type$(2)     , ch(04),~
               at (07,65), fac(hex(8c))  , types$(1)            , ch(14),~
                                                                         ~
               at (08,02), " Purge if in Trial Balance?",                ~
               at (08,40), fac(lfac$( 3)), opt_inv_tb$(1)       , ch(01),~
               at (08,72), fac(hex(8c))  , opt_inv_tb$(2)       , ch(01),~
                                                                         ~
               at (09,02), " Purge if Sales Order on file?",             ~
               at (09,40), fac(lfac$( 4)), opt_inv_so$(1)       , ch(01),~
               at (09,72), fac(hex(8c))  , opt_inv_so$(2)       , ch(01),~
                                                                         ~
               at (10,02), " Purge Orphaned Line Items?",                ~
               at (10,40), fac(lfac$( 5)), opt_inv_orphans$(1)  , ch(01),~
               at (10,72), fac(hex(8c))  , opt_inv_orphans$(2)  , ch(01),~
                                                                         ~
               at (11,02), " Print List of Purged Invoices?",            ~
               at (11,40), fac(lfac$( 6)), opt_inv_list$(1)     , ch(01),~
               at (11,72), fac(hex(8c))  , opt_inv_list$(2)     , ch(01),~
                                                                         ~
               at (13,02), "C H E C K S",                                ~
               at (14,02), " Purge Through Check Date",                  ~
               at (14,40), fac(lfac$( 7)), opt_chk_date$(1)     , ch(08),~
               at (14,68), fac(hex(8c))  , opt_chk_date$(2)     , ch(08),~
                                                                         ~
               at (15,02), " Customer Type Range",                       ~
               at (15,46), fac(hex(8c))  , tos$(2)              , ch(02),~
               at (15,40), fac(lfac$( 8)), opt_chk_type$(1)     , ch(05),~
               at (15,49), fac(lfac$( 8)), opt_chk_type$(2)     , ch(04),~
               at (15,65), fac(hex(8c))  , types$(2)            , ch(14),~
                                                                         ~
               at (16,02), " Purge if in Trial Balance?",                ~
               at (16,40), fac(lfac$( 9)), opt_chk_tb$(1)       , ch(01),~
               at (16,72), fac(hex(8c))  , opt_chk_tb$(2)       , ch(01),~
                                                                         ~
               at (17,02), " Purge Orphaned Line Items?",                ~
               at (17,40), fac(lfac$(10)), opt_chk_orphans$(1)  , ch(01),~
               at (17,72), fac(hex(8c))  , opt_chk_orphans$(2)  , ch(01),~
                                                                         ~
               at (18,02), " Print List of Purged Checks?",              ~
               at (18,40), fac(lfac$(11)), opt_chk_list$(1)     , ch(01),~
               at (18,72), fac(hex(8c))  , opt_chk_list$(2)     , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <>  9% then L40500
                  gosub display_purge_history  : goto L40160
L40500:        if keyhit% <> 13% then L40510
                  call "MANUAL" ("ARMPURGE")   : goto L40160
L40510:        if keyhit% <> 15% then L40525
                  call "PRNTSCRN"              : goto L40160

L40525:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40630     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     " (9)See Purge History  (15)Print Screen"
            pf$(3) = "                 (6)Proceed to Edit Mode" &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff06ffff09ffffff0dff0f1000)
            if i% > 0% then L40600
                str(pf$(2),42,21) = " "  :  str(pfkeys$, 9,1) = hex(ff)
L40600:     if fieldnr% = 1% then L40610
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40610:     if fieldnr% > 1% then L40620
                str(pf$(2),18,22) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40620:     return

L40630: if fieldnr% > 0% then L40685  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     " (9)See Purge History  (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Begin Purge "
            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f1000)
            if i% > 0% then L40680
                str(pf$(2),42,21) = " "  :  str(pfkeys$, 9,1) = hex(ff)
L40680:     return
L40685:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over    (3)Set Corresponding Fi" &        ~
                     "eld                    (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ff03ffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *       F I L E   O P E N   M O D E   S C R E E N           *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        open_mode_screen
            gosub purge_orphans_requirements
            inpmessage$ = "Enter a non-blank character next to files" &  ~
                          " that are to be open for exclusive use."
            gosub set_pf_open_mode_screen

L41120:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge A/R Invoices and Checks",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Place a non-blank character next to the files~
        ~ that are to be opened for",                                     ~
               at (06,02), "exclusive use by this program.  Having files ~
        ~opened exclusively will enhance",                                ~
               at (07,02), "the execution speed of this program BUT will ~
        ~disallow other programs from",                                   ~
               at (08,02), "accessing those files.",                     ~
                                                                         ~
               at (10,25), fac(lfac$(1)), str(open_mode$,1,1),           ~
               at (10,29), "Customer Master File",                       ~
                                                                         ~
               at (11,25), fac(lfac$(2)), str(open_mode$,2,1),           ~
               at (11,29), "Sales Order Files",                          ~
                                                                         ~
               at (12,25), fac(lfac$(3)), str(open_mode$,3,1),           ~
               at (12,29), "A/R Trial Balance",                          ~
                                                                         ~
               at (13,25), fac(lfac$(4)), str(open_mode$,4,1),           ~
               at (13,29), "A/R Invoice Master",                         ~
                                                                         ~
               at (14,25), fac(lfac$(5)), str(open_mode$,5,1),           ~
               at (14,29), "A/R Check Master",                           ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41520
                  call "MANUAL" ("ARMPURGE") : goto L41120

L41520:        if keyhit% <> 15% then L41550
                  call "PRNTSCRN" : goto L41120

L41550:        if keyhit% <>  2% then L41600
                     open_mode$ = all ("X")
                     gosub purge_orphans_requirements
                     goto L41120

L41600:        if keyhit% <>  3% then return
                     open_mode$ = " "
                     gosub purge_orphans_requirements
                     goto L41120

        set_pf_open_mode_screen
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Set All Exclusive                    " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Set All Shared                       " &        ~
                     "                       (16)Begin Purge "
            pfkeys$ = hex(010203ffffffffffffffffff0dff0f1000)
            return

        purge_orphans_requirements:
            init (hex(81)) lfac$()
            if opt_inv_orphans$(1) = "N" then L41790
                str(open_mode$,4,1) = "X"  :  lfac$(4) = hex(8c)
L41790:     if opt_chk_orphans$(1) = "N" then L41810
                str(open_mode$,5,1) = "X"  :  lfac$(5) = hex(8c)
L41810:     return


        REM *************************************************************~
            *           P U R G E   R E S U L T S   R E C A P           *~
            *-----------------------------------------------------------*~
            * Summary screen of what occured during the purge.          *~
            *************************************************************

        results_screen
              inpmessage$ = "Press PF-16 to Return to Menu."
              gosub set_pf_results

L42100:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge A/R Invoices and Checks",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "P U R G E   R E S U L T S   S U M M A R Y",  ~
                                                                         ~
               at (08,05), "INVOICES",                                   ~
               at (09,08), "Header Records Read",                        ~
               at (09,43), fac(hex(84)), inv_hdrs_read%, pic(##,###,###),~
               at (10,08), "Header Records Purged",                      ~
               at (10,43), fac(hex(84)), inv_hdrs_purged%,               ~
                                                         pic(##,###,###),~
               at (11,08), "Line Item Records Purged",                   ~
               at (11,43), fac(hex(84)), inv_lines_purged%,              ~
                                                         pic(##,###,###),~
               at (12,08), "Orphan Line Item Records Purged",            ~
               at (12,43), fac(hex(84)), inv_orphans%,   pic(##,###,###),~
                                                                         ~
               at (14,05), "CHECKS",                                     ~
               at (15,08), "Header Records Purged",                      ~
               at (15,43), fac(hex(84)), chk_hdrs_purged%,               ~
                                                         pic(##,###,###),~
               at (16,08), "Line Item Records Purged",                   ~
               at (16,43), fac(hex(84)), chk_lines_purged%,              ~
                                                         pic(##,###,###),~
               at (17,08), "Orphan Line Item Records Purged",            ~
               at (17,43), fac(hex(84)), chk_orphans%,   pic(##,###,###),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L42510
                  call "MANUAL" ("ARMPURGE") : goto L42100

L42510:        if keyhit% <> 15% then return
                  call "PRNTSCRN" : goto L42100

        set_pf_results
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(ffffffffffffffffffffffff0dff0f1000)
            return


        REM *************************************************************~
            *       D I S P L A Y   P U R G E   H I S T O R Y           *~
            *-----------------------------------------------------------*~
            * Displays the parameters for the last 14 purges.           *~
            *************************************************************

        display_purge_history

              inpmessage2$ = "Press PF-16 to Return to Data Entry."
              gosub set_pf_history

L43110:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge A/R Invoices and Checks: DISPLAY HISTORY",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), hstry_hdrs$(1)         , ch(79),~
               at (05,02), fac(hex(8c)), hstry_hdrs$(2)         , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)), hstry$( 1)             , ch(79),~
               at (07,02), fac(hex(8c)), hstry$( 2)             , ch(79),~
               at (08,02), fac(hex(8c)), hstry$( 3)             , ch(79),~
               at (09,02), fac(hex(8c)), hstry$( 4)             , ch(79),~
               at (10,02), fac(hex(8c)), hstry$( 5)             , ch(79),~
               at (11,02), fac(hex(8c)), hstry$( 6)             , ch(79),~
               at (12,02), fac(hex(8c)), hstry$( 7)             , ch(79),~
               at (13,02), fac(hex(8c)), hstry$( 8)             , ch(79),~
               at (14,02), fac(hex(8c)), hstry$( 9)             , ch(79),~
               at (15,02), fac(hex(8c)), hstry$(10)             , ch(79),~
               at (16,02), fac(hex(8c)), hstry$(11)             , ch(79),~
               at (17,02), fac(hex(8c)), hstry$(12)             , ch(79),~
               at (18,02), fac(hex(8c)), hstry$(13)             , ch(79),~
               at (19,02), fac(hex(8c)), hstry$(14)             , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage2$         , ch(79),~
               at (22,02), fac(hex(8c)),   pf2$(1)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf2$(2)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf2$(3)              , ch(79),~
                     keys(pfkeys2$), key(keyhit%)

               if keyhit% <> 13% then L43520
                  call "MANUAL" ("ARMPURGE") : goto L43110

L43520:        if keyhit% <> 15% then return
                  call "PRNTSCRN" : goto L43110

        set_pf_history
            pf2$(1) = "                                        " &       ~
                      "                       (13)Instructions"
            pf2$(2) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf2$(3) = "                                        " &       ~
                      "                       (16)Return...   "
            pfkeys2$ = hex(ffffffffffffffffffffffff0dff0f1000)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50210,         /* Inv Purge to Date      */~
                              L50420,         /* Inv Cus Type Rnge      */~
                              L50700,         /* Purge Inv In TB?       */~
                              L50780,         /* Purge Inv if SO?       */~
                              L50830,         /* Purge Inv Orphans      */~
                              L50910,         /* Print Inv List         */~
                              L50990,         /* Chk Purge to Date      */~
                              L51180,         /* Chk Cus Type Rnge      */~
                              L51460,         /* Purge Chk in TB?       */~
                              L51540,         /* Purge Chk Orphans      */~
                              L51620          /* Print Chk List?        */
            return

L50210
*        Test PURGE THROUGH INVOICE DATE         OPT_INV_DATE$(1)
            if opt_inv_date$(1%) <> " " and opt_inv_date$(1%) <> blankdate$ ~
                     then L50250
                if edit% = 2% then L50380 else proceed% = 2%
                goto L50380
L50250:     call "STRING" addr("LJ", opt_inv_date$(1), 8%)
            if str(opt_inv_date$(1),,1) <> "-" then L50320
                convert opt_inv_date$(1) to u3%, data goto L50290
                goto L50310
L50290:              errormsg$ = "Invalid numeric entry for offset."
                     return
L50310:         call "DATE" addr("G+", date, u3%, opt_inv_date$(1), err%)
L50320:     call "DATEOK" (opt_inv_date$(1), u3%, errormsg$)
            if errormsg$ <> " " then return
            call "DATUNFMT" (opt_inv_date$(1))
            if opt_inv_date$(1) > date then                              ~
                      errormsg$ = "Purge date must be on or before today"
            call "DATEFMT" (opt_inv_date$(1))
L50380:     if edit% = 2% and keyhit% = 3% then                          ~
                                      opt_chk_date$(1) = opt_inv_date$(1)
            return

L50420
*        Test for CUSTOMER TYPE RANGE            OPT_INV_TYPE$(1)
            if opt_inv_type$(1) <> "?" then L50500
                plowkey$ = "CUS TYPES"  : opt_inv_type$(1) = " "
                descr$   = hex(06) & "Select FROM CUSTOMER TYPE"
                call "PLOWCODE" (#6, plowkey$, descr$, 9%, .3, f1%(6))
                if f1%(6) = 1% then L50490
                     errormsg$ = hex(00) : return
L50490:         opt_inv_type$(1) = str(plowkey$, 10)
L50500:     if opt_inv_type$(2) <> "?" then L50570
                plowkey$ = "CUS TYPES"  : opt_inv_type$(2) = " "
                descr$   = hex(06) & "Select TO CUSTOMER TYPE"
                call "PLOWCODE" (#6, plowkey$, descr$, 9%, .3, f1%(6))
                if f1%(6) = 1% then L50560
                     errormsg$ = hex(00) : return
L50560:         opt_inv_type$(2) = str(plowkey$, 10)
L50570:     call "TESTRNGE" (opt_inv_type$(1), opt_inv_type$(2),         ~
                             opt_inv_type$(3), opt_inv_type$(4),         ~
                             errormsg$)
            if edit% <> 2% or keyhit% <> 3% then L50650
                opt_chk_type$(1) = opt_inv_type$(1)
                opt_chk_type$(2) = opt_inv_type$(2)
                opt_chk_type$(3) = opt_inv_type$(3)
                opt_chk_type$(4) = opt_inv_type$(4)
L50650:     tos$(1), tos$(2) = "to"
            if opt_inv_type$(2) = " " then tos$(1) = " "
            if opt_chk_type$(2) = " " then tos$(2) = " "
            return

L50700
*        Test for PURGE IF IN TRIAL BALANCE?     OPT_INV_TB$(1)
            if pos("YN" = opt_inv_tb$(1)) > 0% then L50740
                errormsg$ = "Enter 'Y' or 'N'."
                return
L50740:     if edit% = 2% and keyhit% = 3% then                          ~
                                          opt_chk_tb$(1) = opt_inv_tb$(1)
            return

L50780
*        Test for PURGE IF SO ORDER ON FILE?     OPT_INV_SO$(1)
            if pos("YN" = opt_inv_so$(1)) > 0% then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50830
*        Test for PURGE ORPHANED LINE ITEMS?     OPT_INV_ORPHANS$(1)
            if pos("YN" = opt_inv_orphans$(1)) > 0% then L50870
                errormsg$ = "Enter 'Y' or 'N'."
                return
L50870:     if edit% = 2% and keyhit% = 3% then                          ~
                                opt_chk_orphans$(1) = opt_inv_orphans$(1)
            return

L50910
*        Test for PRINT LIST OF PURGED INV?      OPT_INV_LIST$(1)
            if pos("YN" = opt_inv_list$(1)) > 0% then L50950
                errormsg$ = "Enter 'Y' or 'N'."
                return
L50950:     if edit% = 2% and keyhit% = 3% then                          ~
                                      opt_chk_list$(1) = opt_inv_list$(1)
            return

L50990
*        Test for PURGE THROUGH CHECK DATE       OPT_CHK_DATE$(2)
            if opt_chk_date$(1%) <> " " and opt_chk_date$(1%) <> blankdate$ ~
                   then L51010
                proceed% = 1%
                return
L51010:     call "STRING" addr("LJ", opt_chk_date$(1), 8%)
            if str(opt_chk_date$(1),,1) <> "-" then L51080
                convert opt_chk_date$(1) to u3%, data goto L51050
                goto L51070
L51050:              errormsg$ = "Invalid numeric entry for offset."
                     return
L51070:         call "DATE" addr("G+", date, u3%, opt_chk_date$(1), err%)
L51080:     call "DATEOK" (opt_chk_date$(1), u3%, errormsg$)
            if errormsg$ <> " " then return
            call "DATUNFMT" (opt_chk_date$(1))
            if opt_chk_date$(1) > date then                              ~
                      errormsg$ = "Purge date must be on or before today"
            call "DATEFMT" (opt_chk_date$(1))
            if edit% = 2% and keyhit% = 3% then                          ~
                                      opt_inv_date$(1) = opt_chk_date$(1)
            return

L51180
*        Test for CUSTOMER TYPE RANGE            OPT_CHK_TYPE$()
            if opt_chk_type$(1) <> "?" then L51260
                plowkey$ = "CUS TYPES"  : opt_chk_type$(1) = " "
                descr$   = hex(06) & "Select FROM CUSTOMER TYPE"
                call "PLOWCODE" (#6, plowkey$, descr$, 9%, .3, f1%(6))
                if f1%(6) = 1% then L51250
                     errormsg$ = hex(00) : return
L51250:         opt_chk_type$(1) = str(plowkey$, 10)
L51260:     if opt_chk_type$(2) <> "?" then L51330
                plowkey$ = "CUS TYPES"  : opt_chk_type$(2) = " "
                descr$   = hex(06) & "Select TO CUSTOMER TYPE"
                call "PLOWCODE" (#6, plowkey$, descr$, 9%, .3, f1%(6))
                if f1%(6) = 1% then L51320
                     errormsg$ = hex(00) : return
L51320:         opt_chk_type$(2) = str(plowkey$, 10)
L51330:     call "TESTRNGE" (opt_chk_type$(1), opt_chk_type$(2),         ~
                             opt_chk_type$(3), opt_chk_type$(4),         ~
                             errormsg$)
            if edit% <> 2% or keyhit% <> 3% then L51410
                opt_inv_type$(1) = opt_chk_type$(1)
                opt_inv_type$(2) = opt_chk_type$(2)
                opt_inv_type$(3) = opt_chk_type$(3)
                opt_inv_type$(4) = opt_chk_type$(4)
L51410:     tos$(1), tos$(2) = "to"
            if opt_inv_type$(2) = " " then tos$(1) = " "
            if opt_chk_type$(2) = " " then tos$(2) = " "
            return

L51460
*        Test for PURGE IF IN TRIAL BALANCE?     OPT_CHK_TB$(2)
            if pos("YN" = opt_chk_tb$(1)) > 0% then L51500
                errormsg$ = "Enter 'Y' or 'N'."
                return
L51500:     if edit% = 2% and keyhit% = 3% then                          ~
                                          opt_inv_tb$(1) = opt_chk_tb$(1)
            return

L51540
*        Test for PURGE ORPHANED LINE ITEMS?     OPT_CHK_ORPHANS$(1)
            if pos("YN" = opt_chk_orphans$(1)) > 0% then L51580
                errormsg$ = "Enter 'Y' or 'N'."
                return
L51580:     if edit% = 2% and keyhit% = 3% then                          ~
                                opt_inv_orphans$(1) = opt_chk_orphans$(1)
            return

L51620
*        Test for PRINT LIST OF PURGED CHKS?     OPT_CHK_LIST$(1)
            if pos("YN" = opt_chk_list$(1)) > 0% then L51660
                errormsg$ = "Enter 'Y' or 'N'."
                return
L51660:     if edit% = 2% and keyhit% = 3% then                          ~
                                      opt_inv_list$(1) = opt_chk_list$(1)
            return

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
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        abort_pgm
            call "READ101" (#2, purge_key$, f1%(2))
            if f1%(2) = 1% then delete #2

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
