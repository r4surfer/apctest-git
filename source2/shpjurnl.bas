        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   JJJJJ  U   U  RRRR   N   N  L       *~
            *  S      H   H  P   P    J    U   U  R   R  NN  N  L       *~
            *   SSS   HHHHH  PPPP     J    U   U  RRRR   N N N  L       *~
            *      S  H   H  P      J J    U   U  R   R  N  NN  L       *~
            *   SSS   H   H  P       J      UUU   R   R  N   N  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPJURNL - Prints Shipment's Journal and Posts G/L w/     *~
            *            Shipping Transactions.                         *~
            *----------------------------------------------------------Q*~
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
            * 08/12/86 ! Original                                 ! ERN *~
            * 05/14/87 ! Std Cost Changes                         ! ERN *~
            * 08/09/90 ! G/L Export file modifications            ! ERN *~
            * 11/26/90 ! Added Management Values to GLEXPORT.     ! JDH *~
            * 05/22/91 ! Execution of G/L Export code dependent on! JBK *~
            *          !  Export flag being on.                   !     *~
            *          ! Fixed PRR's                              !     *~
            *          !  11602, 11638, 11971- Report not paging  !     *~
            *          !    correctly                             !     *~
            *          !  11962- Enlarged detail report totals    !     *~
            * 12/23/91 ! Branch around setting GL Export info if  ! JDH *~
            *          !  journal is summarized.                  !     *~
            * 03/18/92 ! PRR 12298 - Moved test for Required Files! JDH *~
            * 06/05/95 ! COQS project to expand COGS Accnts for   ! RJH *~
            *          !  Cost Buckets.                           !     *~
            * 07/24/96 ! Fix COQS Part Debit Total not summing.   ! RJH *~
            * 09/19/96 ! Changes for the year 2000.               ! DXL *~
            * 11/05/96 ! COGS Credit now not reversing sign twice.! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$9,                      /* An Account Code            */~
            accta$9,                     /* Intra-divisional CoGS Acct */~
            acctb$9,                     /* Alt Owners Cust CoGS Acct  */~
            acct1$12, acct2$12,          /* Account Codes for Printing */~
            acctdescr$30,                /* Account Description        */~
            amt1$10, amt2$10,            /* Amounts for Printing       */~
            askuser$(3)79,               /* Ask User Messages          */~
            bdate$8,                     /* Batch date for SHPJURNL    */~
            company$60,                  /* Company/Division Name      */~
            consolidate_cogs$(12)9,      /* Conslidate COGS Accounts   */~
            consolidate_cost(12),        /* Consolidated Costs         */~
            consol_post_amt(12),         /* Consolidated Posting Amts  */~
            cost(12),                    /* Cost buckets               */~
            craccts$(8)9,                /* Credit Accounts            */~
            crs(8),                      /* Credit Amounts             */~
            crtype$(8)1,                 /* Type of G/L account        */~
            ctlkey$100,                  /* Subtotalling Control       */~
            cuscode$9,                   /* Customer Code              */~
            date$10,                     /* Report Run Date            */~
            datefrom$10,dateto$10,       /* Low/High Trans Time Dates  */~
            doc$20,                      /* Document ID                */~
            draccts$(8)9,                /* Debit Accounts             */~
            drs(8),                      /* Debit Amounts              */~
            drtype$(8)1,                 /* TYpe of G/L account        */~
            expanded_cogs$(12)9,         /* Expanded COGS Accounts     */~
            export_on$1,                 /* G/L Export File processing?*/~
            file$1,                      /* Work File ID               */~
            icc$6,                       /* Intercompany Corporate Code*/~
            last$100,                    /* Print Control Variable     */~
            lot$6,                       /* Lot Number                 */~
            mgtrpt_on$1,                 /* Is Management Reporting on?*/~
            mode$2,                      /* Posting Mode (CR or DR)    */~
            newfile$1,                   /* Next Work file for Trans   */~
            origuser$3,                  /* User who generated Trans   */~
            part$25,                     /* Part Number                */~
            plowkey$100,                 /* Misc use Plow Key          */~
            post$1,                      /* Posting Level              */~
            postdate$6, postdatef$8,     /* Post Date (Unfmtd, Fmtd)   */~
            printcuscode$9,              /* Customer Number            */~
            printdoc$20,                 /* Document ID                */~
            printlot$6,                  /* Lot Number                 */~
            printpart$25,                /* Part Code                  */~
            printpostdate$8,             /* Post Date                  */~
            printseqnr$3,                /* Sequence Number            */~
            printstore$3,                /* Store                      */~
            qty$10,                      /* Quantity                   */~
            readkey$100,                 /* Multi-use Read Variable    */~
            rpt_title$60,                /* Report Title               */~
            seqnr$3,                     /* Line Sequence Number       */~
            status$1,                    /* In-process Status          */~
            store$3,                     /* Store                      */~
            summary$1,                   /* Posting Summary Option Flag*/~
            text$100,                    /* G/L Text                   */~
            time$8,                      /* Report Run Time            */~
            timefrom$8, timeto$8,        /* Low/High Trans Time Times  */~
            transfrom$7, transto$7,      /* Low/High Trans Time Stamps */~
            userid$3                     /* User running program       */

        dim                              /* G/L Export Posting info    */~
            bol$3,                       /* Bill of Lading ID          */~
            country$3,                   /* Customer country code      */~
            custype$2,                   /* Customer type code         */~
            gl_post_info$(2)255,         /* G/L export posting info    */~
            partcat$4,                   /* Part category code         */~
            partclass$4,                 /* Part class code            */~
            partgen$16,                  /* Part generic code          */~
            parttype$4,                  /* Part type code             */~
            project$8,                   /* Project code               */~
            salesm$4,                    /* Salesman code              */~
            salesr$4,                    /* Sales Region code          */~
            salest$10,                   /* Sales Tax code             */~
            shp_type$1,                  /* Indicates SO or Invoice    */~
            so$16,                       /* Sale Order number          */~
            state$2,                     /* Customer State code        */~
            tran_type$5,                 /* G/L transaction type       */~
            uom$4,                       /* Part Stocking Unit of Meas */~
            zip$9                        /* Customer Zip code          */

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
            * #1  ! SHPHNYTF ! Shipping-Inventory Transaction File      *~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! GLMAIN   ! GENERAL LEDGER.  SALES ACCT VERIFICATION *~
            * #4  ! GLDETAIL ! GENERAL LEDGER DETIAL FILE               *~
            * #5  ! CUSTOMER ! Customer Master File                     *~
            * #6  ! HNYMASTR ! Inventory Part Master File               *~
            * #7  ! BCKMASTR ! Sales Order Header File                  *~
            * #8  ! ARIMASTR ! Invoice Header File                      *~
            * #9  ! BCKLINES ! Sales Order Line Item File               *~
            * #10 ! ARILINES ! Invoice Line Item File                   *~
            * #11 ! GLCOGSXR ! GL COGS Accounts via Cost buckets        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SHPHNYTF",                                      ~
                        varc,     indexed,  recsize = 572,               ~
                        keypos =  1,   keylen = 46,                      ~
                        alt key 1, keypos = 47, keylen = 80

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #3,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select #4,  "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26                      ~

            select #5,  "CUSTOMER",                                      ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos = 1, keylen =   9,                        ~
                        alt key  1, keypos =  10, keylen =  30, dup,     ~
                            key  2, keypos = 424, keylen =   9, dup,     ~
                            key  3, keypos = 771, keylen =   9, dup,     ~
                            key  4, keypos = 780, keylen =   9, dup

            select #6,  "HNYMASTR",                                      ~
                        varc, indexed, recsize =  900,                   ~
                        keypos = 1, keylen =  25,                        ~
                        alt key  1, keypos = 102, keylen = 9, dup,       ~
                            key  2, keypos =  90, keylen = 4, dup

            select #7,  "BCKMASTR",                                      ~
                        varc, indexed, recsize = 1000,                   ~
                        keypos = 1, keylen =  25,                        ~
                        alt key  1, keypos =  26, keylen =  16, dup

            select #8,  "ARIMASTR",                                      ~
                        varc, indexed, recsize = 2000,                   ~
                        keypos = 1, keylen =  17,                        ~
                        alt key 1, keypos = 10, keylen =  8, dup,        ~
                            key 2, keypos = 18, keylen = 16, dup,        ~
                            key 3, keypos = 34, keylen = 16, dup

            select #9,  "BCKLINES",                                      ~
                        varc, indexed, recsize =  300,                   ~
                        keypos = 10, keylen = 19

            select #10, "ARILINES",                                      ~
                        varc, indexed, recsize = 750,                    ~
                        keypos = 1, keylen = 20

            select #11, "GLCOGSXR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  9

        call "SHOSTAT" ("Opening Files, One Moment Please")
            rslt$(1 ) = "REQUIRED"
                call "OPENCHCK" (#1, fs%(1), f2%(1),   0%, rslt$(1))
            rslt$(2 ) = "REQUIRED"
                call "OPENCHCK" (#2, fs%(2), f2%(2),   0%, rslt$(2))
            rslt$(3 ) = "REQUIRED"
                call "OPENCHCK" (#3, fs%(3), f2%(3),   0%, rslt$(3))
            if min(fs%()) < 0% then exit_program

                call "OPENCHCK" (#4, fs%(4), f2%(4),   0%, rslt$(4))
                call "OPENCHCK" (#5, fs%(5), f2%(5),   0%, rslt$(5))
                call "OPENCHCK" (#6, fs%(6), f2%(6),   0%, rslt$(6))
                call "OPENCHCK" (#7, fs%(7), f2%(7),   0%, rslt$(7))
                call "OPENCHCK" (#8, fs%(8), f2%(8),   0%, rslt$(8))
                call "OPENCHCK" (#9, fs%(9), f2%(9),   0%, rslt$(9))
                call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
                call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
L09025:     ret% = 2%
            askuser$(1) = "Press RETURN to continue with the Shipments" &~
                " Journal"
            askuser$(2) = "-- OR --"
            askuser$(3) = "Press PF(16) to cancel program and return " & ~
                "to menu"
            call "ASKUSER" (ret%, "*** SHIPMENTS JOURNAL ***",           ~
                askuser$(1), askuser$(2), askuser$(3))
            if ret% = 16% then goto exit_program
            if ret% <> 0% then goto L09025
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (time$)
            call "COMPNAME" (12%, company$, ret%)

*        See if Management Reporting is on
            mgtrpt_on$, export_on$ = "N"
            call "READ100" (#2, "SWITCHS.GL", f1%(2))
            if f1%(2) = 1% then get #2 using L09086, export_on$, mgtrpt_on$
L09086:         FMT POS(22), CH(1), POS(59), CH(1)
            if mgtrpt_on$ = "Y" then gosub init_mgt

*        Load the Control Record to determine (1) If we're restarting;
*        and (2) which logical file to update.
            plowkey$ = "0"
            call "READ101" (#1, plowkey$, f1%(1))
            if f1%(1) = 1% then L09160
                askuser$(1) = "The Transaction File Control Record is" & ~
                              "Missing."
                askuser$(2) = "Press any PF-KEY to end."
                askuser$(3) = "(Program probably run out of sequence.)"
                call "ASKUSER" (ret%, "DATA BASE ERROR",                 ~
                                askuser$(1), askuser$(2), askuser$(3))
                goto exit_program

L09160:     get #1 using L09165, file$, status$
L09165:         FMT XX(126), CH(1), CH(1)
            if status$ <> " " then L09365

*        NOT RESTARTING- See if there are any transactions to report on.
*          If we are going to continue then update which file that
*          transactions are to go into and flag update in-process.
            plowkey$ = file$ & all(hex(00))
            call "PLOWNEXT" (#1, plowkey$, 1%, f1%(1))
            if f1%(1) = 1% then L09250
                call "STRTRLSE" addr (#1)
                ret% = 0%
                askuser$(1) = "The Report Transaction file is empty."
                askuser$(2) = "Press any PF-KEY to end."
                askuser$(3) = " "
                call "ASKUSER" (ret%, "NO TRANSACTIONS",                 ~
                                askuser$(1), askuser$(2), askuser$(3))
                goto exit_program
L09250:     if str(plowkey$,2,1) <> hex(00) then L09295
                ret% = 0%
                call "STRTRLSE" addr (#1)
                askuser$(1) = "There is a Transaction in-process."
                askuser$(2) = "Press PF-16 to try to start again."
                askuser$(3) = "Press any other PF-Key to EXIT."
                call "ASKUSER" (ret%, "TRANSACTION IN-PROCESS",          ~
                                askuser$(1), askuser$(2), askuser$(3))
                if ret% = 16% then L09000 else exit_program
L09295
*           Get report information and save it (flags update in-process)
                bdate$ = date
                call "JNLINFO" ("01", "RSJ", postseq%, summary$,         ~
                                 rpt_title$, bdate$, #2, f2%(2), ret%)
                call "READ101" (#1, "0", f1%(1))
                post$ = "D" : if summary$ = "Y" then post$ = "S"
                call "FMTTITLE" (rpt_title$, "JOURNAL", 12%)
                newfile$ = "1" : if file$ = "1" then newfile$ = "2"
                rewrite #1 using L09345, "0", "0", newfile$, "R", userid$, ~
                                       post$, postseq%, rpt_title$,      ~
                                       " ", " "
L09345:              FMT CH(46), CH(80), CH(1), CH(1), CH(3), CH(1),     ~
                         BI(4), CH(60), CH(200), CH(176)
                goto L10000

L09365
*        RESTARTING- retrieve update info and continue
            call "STRTRLSE" addr (#1)
            if file$ = "1" then file$ = "2" else file$ = "1"
            get #1 using L09390, status$, origuser$, post$, postseq%,      ~
                               rpt_title$
L09390:         FMT XX(127), CH(1), CH(3), CH(1), BI(4), CH(60)
            if origuser$ = userid$ then L09445
                ret% = 0%
                call "STRTRLSE" addr (#1)
                askuser$(1) = "Report/Update has already been started" & ~
                              " by User " & origuser$
                askuser$(2) = "Press any PF-Key to exit Program."
                askuser$(3) = " "
                call "ASKUSER" (ret%, "IN-PROCESS CONFLICT",             ~
                                askuser$(1), askuser$(2), askuser$(3))
                goto exit_program
L09445:     ret% = 0%
            askuser$(1) = "Press PF-16 to Acknowledge Restart,"
            askuser$(2) = "-OR-"
            askuser$(3) = "Any Other PF-Key to Abort."
            call "ASKUSER" (ret%, "R E S T A R T I N G",                 ~
                            askuser$(1), askuser$(2), askuser$(3))
            if ret% <> 16% then exit_program
                if status$ = "U" then post_gl
                     plowkey$ = file$ & hex(ff)  /* Remove summaries   */
                     call "DELETE" (#1, plowkey$, 2%)


L10000: REM *************************************************************~
            *            P R I N T   R E P O R T                        *~
            * --------------------------------------------------------- *~
            * (1) Print Detail Report and generate summary              *~
            * (2) Print summary report                                  *~
            * (3) Update in-process status to 'updating'.               *~
            *************************************************************

*        Initialization for Report Printing
            call "SHOSTAT" ("Printing Shipping Journal")
            line% = 857%
            page% = 0%
            select printer(134)
            call "SETPRNT" ("SHP005", " ", 0%, 0%)
            plowkey$   = file$ & all(hex(00))
            summary%   = 0%
            first%     = 1%
            transfrom$ = all(hex(ff))
            transto$   = all(hex(00))

        detail_report_loop
            call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%(1))
            if f1%(1) = 0%                  or                           ~
               str(plowkey$,1,1) <> file$   or                           ~
               str(plowkey$,2,1)  = hex(ff) then end_detail_report
*        See if this record is to be processed (0s held for restarts).
            get #1 using L10260, qty, drs(), crs()
L10260:         FMT XX(129), PD(14,4), POS(210), 8*PD(14,4), POS(346),   ~
                                       8*PD(14,4)
            for t% = 1% to 8%
                if drs(t%) <> 0 or crs(t%) <> 0 then L10330
            next t%
            if qty = 0 then detail_report_loop

L10330
*        Check if we need to do any sub-totalling
            if first% = 1% then L10410
                if str(plowkey$,2,6) = str(ctlkey$,2,6) then L10370
                     gosub post_date_totals  :  goto L10410
L10370:         if str(plowkey$,8,3) = str(ctlkey$,8,3) then L10390
                     gosub store_totals      :  goto L10410
L10390:         if str(plowkey$,11,25) = str(ctlkey$,11,25) then L10410
                     gosub part_totals
L10410:     ctlkey$ = plowkey$  :  first% = 0%
            if str(plowkey$,74,7) < transfrom$ then                      ~
                                          transfrom$ = str(plowkey$,74,7)
            if str(plowkey$,74,7) > transto$   then                      ~
                                          transto$   = str(plowkey$,74,7)

*        Detail Line Print
            postdate$ = str(ctlkey$,2,6)
            get #1 using L10510, cuscode$, doc$, seqnr$, lot$, qty,       ~
                                draccts$(), drs(), craccts$(),crs(),     ~
                                cost()
L10510:         FMT XX(81), CH(9), CH(20), CH(3),CH(6), XX(10), PD(14,4),~
                    8*CH(9), 8*PD(14,4), 8*CH(9), 8*PD(14,4),            ~
                    POS(410), 12*PD(14,4)
            totalqty = totalqty + qty
            call "CONVERT" (qty, 2.2, qty$)
            for a% = 1% to 8%
              if a% > 1% and draccts$(a%) = " " and craccts$(a%) = " "   ~
                                                   then L10860 /* Next A */
                if line% > 55% then gosub page_heading
                tempacct$ = draccts$(a%)
                gosub check_for_expanding_cogs
                   if expand_cogs% = 1% then mode$ = "DR" else mode$ = " "
                acct1$ = draccts$(a%)  :  call "GLFMT" (acct1$)
                if expand_cogs% = 1% then L10600 /* Only one side can be */
                tempacct$ = craccts$(a%)         /*  a COGS Expansion */
                gosub check_for_expanding_cogs
                   if expand_cogs% = 1% then mode$ = "CR" else mode$ = " "
L10600:         acct2$ = craccts$(a%)  :  call "GLFMT" (acct2$)
                call "CONVERT" (drs(a%), 2.2, amt1$)
                call "CONVERT" (crs(a%), 2.2, amt2$)
                if draccts$(a%) = " " then amt1$ = " "
                if craccts$(a%) = " " then amt2$ = " "
                if str(last$, 8, 3) <> str(ctlkey$, 8, 3) then           ~
                                      printstore$   = str(ctlkey$, 8, 3)
                if str(last$,11,25) <> str(ctlkey$,11,25) then           ~
                                      printpart$    = str(ctlkey$,11,25)
                if str(last$,36, 9) <> str(ctlkey$,36, 9) then           ~
                                      printcuscode$ = str(ctlkey$,36, 9)
                if str(last$,45,20) <> str(ctlkey$,45,20) then           ~
                                      printdoc$     = str(ctlkey$,45,20)
                if str(last$,65, 3) <> str(ctlkey$,65, 3) then           ~
                                      printseqnr$   = str(ctlkey$,65, 3)
                if str(last$,68, 6) <> str(ctlkey$,68, 6) then           ~
                                      printlot$     = str(ctlkey$,68, 6)
                if expand_cogs% = 0% or a% <> 1% then L10770
                     gosub print_expanded_cogs
                     if start% = 0% then L10770  /* some kind of failure */
                     goto L10810
L10770:         print using L12750, printstore$, printpart$,              ~
                                   printcuscode$, printdoc$, printseqnr$,~
                                   printlot$, qty$,                      ~
                                   acct1$, amt1$, acct2$, amt2$
L10810:         init (" ") printstore$, printpart$, printcuscode$,       ~
                           printdoc$, printseqnr$, printlot$, qty$
                last$ = ctlkey$
                line% = line% + 1%
                qty$  = " "
L10860:     next a%
            for a% = 1% to 8%
                if draccts$(a%) = " " then L10920
                   if a% = 1% and mode$ = "DR"                           ~
                                            then L10910/*Already Expanded*/
                       acct1$ = draccts$(a%) : dramt = drs(a%) : cramt = 0
                       gosub update_summary
L10910:            partdr = partdr + drs(a%)
L10920:         if craccts$(a%) = " " then L10960
                if a% = 1% and mode$ = "CR" then L10950/*Already Expanded*/
                     acct1$ = craccts$(a%) : cramt = crs(a%) : dramt = 0
                     gosub update_summary
L10950:         partcr = partcr + crs(a%)
L10960:     next a%
            goto detail_report_loop


        update_summary
            readkey$ = file$ & hex(ff) & str(postdate$) & acct1$
            call "READ101" (#1, readkey$, f1%(1))
            olddr, oldcr = 0
            if f1%(1) = 1% then get #1 using L11050, olddr, oldcr
L11050:         FMT XX(126), 2*PD(14,4)
            dramt = dramt + olddr
            cramt = cramt + oldcr
            put #1 using L11100, readkey$, readkey$, dramt, cramt,        ~
                                " ", " "
L11100:         FMT CH(46), CH(80), 2*PD(14,4), CH(200), CH(230)
            if f1%(1) = 0% then write #1 else rewrite #1
            return


        end_detail_report
            if first% = 1% then summary_report
                gosub post_date_totals
                print using L12900, jrnldr, jrnlcr
                goto summary_report

        post_date_totals
            gosub store_totals
            if line% > 56% then gosub page_heading
            print using L12870, postdatef$, datedr, datecr
            print
            line%  = 857%
            jrnldr = jrnldr + datedr
            jrnlcr = jrnlcr + datecr
            datedr, datecr = 0
            str(last$, 2) = all(hex(ff))
            return

        store_totals
            gosub part_totals
            if line% > 56% then gosub page_heading
            print using L12840, str(ctlkey$,8,3), storedr, storecr
            print
            line% = line% + 2%
            datedr = datedr + storedr
            datecr = datecr + storecr
            storedr, storecr = 0
            str(last$, 8) = all(hex(ff))
            return

        part_totals
            if line% > 56% then gosub page_heading
            call "CONVERT" (totalqty, 2.2, qty$)
            print using L12780
            print using L12810, str(ctlkey$,11,25), qty$, partdr, partcr
            print
            line% = line% + 3%
            storedr = storedr + partdr
            storecr = storecr + partcr
            partdr, partcr, totalqty = 0
            str(last$,11) = all(hex(ff))
            return




        summary_report
            summary%, first% = 1%
            plowkey$ = file$ & hex(ff) & all(hex(00))
            last$ = all(hex(ff))
            datedr, datecr = 0
            gosub page_heading

        summary_report_loop
            call "PLOWNEXT" (#1, plowkey$, 2%, f1%(1))
            if f1%(1) = 0% then end_summary_report

            if first% = 0%                           and                 ~
               str(plowkey$,3,6) <> str(ctlkey$,3,6) then gosub subtotals
            ctlkey$ = plowkey$  :  first% = 0%

            get #1 using L11750, postdate$, acct1$, dramt, cramt
L11750:         FMT XX(2), CH(6), CH(9), POS(127), 2*PD(14,4)
            if postdate$ = last$ then L11790
                printpostdate$ = postdate$
                call "DATEFMT" (printpostdate$)
L11790:     call "DESCRIBE" (#3, acct1$, acctdescr$, 0%, f1%(3))
            if f1%(3) = 0% then acctdescr$ = "** ACCOUNT NOT ON FILE **"
            call "GLFMT" (acct1$)
            if line% > 55% then gosub page_heading
            print using L13030, printpostdate$, acct1$, acctdescr$,       ~
                                                            dramt, cramt
            printpostdate$ = " "
            line% = line% + 1%
            datedr = datedr + dramt
            datecr = datecr + cramt
            last$ = str(ctlkey$,3,6)
            goto summary_report_loop


        subtotals    /* Sub-totals for Summary Report        */
            if first% = 1% then return   /* No summary records */
                printpostdate$ = str(ctlkey$,3,6)
                call "DATEFMT" (printpostdate$)
                print using L13060
                print using L13090, printpostdate$, datedr, datecr
                print
                datedr, datecr = 0
                line% = line% + 3%
                last$ = all(hex(ff))
                return


        end_summary_report
            gosub subtotals
            print using L13120, jrnldr, jrnlcr
            get transfrom$ using L12100, date%, time%
L12100:         FMT BI(3), BI(4)
            gosub format_time_stamp
                datefrom$ = date$ : timefrom$ = time$
            get transto$   using L12100, date%, time%
            gosub format_time_stamp
                dateto$   = date$ : timeto$   = time$
            print
            print using L13150, datefrom$, timefrom$, dateto$, timeto$
            print
            print using L13180            /* End of Report! */
            goto post_gl

        format_time_stamp
            date% = date% + 19000000%
            convert date% to date$, pic(00000000)
            call "DATFMTC" (date$)

            convert time% to time$, pic(00000000)
            str(time$,7) = "  "
            call "TIMEOK" (time$, temp, " ")  :  temp = temp
            return


        page_heading
            page% = page% + 1%  :  line% = 8%
            print page
            print using L12570, date$, time$, company$
            print using L12600, postseq%, rpt_title$, page%
            print
            if summary% <> 0% then L12480
                postdatef$ = str(ctlkey$,2,6)      /* Detail Header    */
                call "DATEFMT" (postdatef$)
                print using L12630, postdatef$
                print
                print using L12660
                print using L12690
                print using L12720
                goto L12530

L12480:         print using L12940                  /* Summary Header   */
                print
                print using L12970
                print using L13000

L12530:     last$ = all(hex(ff))
            return


L12570: %RUN DATE: ########  ########       #############################~
        ~###############################                    SHPHNYRP-SHP00~
        ~5
L12600: % JOURNAL: RSJ  SEQ: ########       #############################~
        ~###############################                         PAGE: ###~
        ~#
L12630: %POST DATE: ########


L12660: %                               SHIP-TO                          ~
        ~          QUANTITY ----- D E B I T S ----- ---- C R E D I T S ---~
        ~-
L12690: %STR PART NUMBER                CUSTOMER DOCUMENT NUMBER      SEQ~
        ~ LOT       SHIPPED  G/L ACCOUNT    DOLLARS  G/L ACCOUNT    DOLLAR~
        ~S
L12720: %--- ------------------------- --------- -------------------- ---~
        ~ ------ ---------- ------------ ---------- ------------ ---------~
        ~-
L12750: %### ######################### ######### #################### ###~
        ~ ###### ########## ############ ########## ############ #########~
        ~#
L12780: %                                                                ~
        ~        ----------           -------------           ------------~
        ~-
L12810: %                          * TOTALS FOR PART ####################~
        ~#####   ##########           -#########.##           -#########.#~
        ~#
L12840: %                         ** TOTALS FOR STORE ###                ~
        ~                             -#########.##           -#########.#~
        ~#
L12870: %                        *** TOTALS FOR POST DATE ########       ~
        ~                             -#########.##           -#########.#~
        ~#
L12900: %                       **** JOURNAL TOTALS                      ~
        ~                             -#########.##           -#########.#~
        ~#

L12940: %                  * * * * * * * * * *   S U M M A R Y   B Y   D ~
        ~A T E   A N D   A C C O U N T   * * * * * * * * * *

L12970: %                  POST DATE   G/L ACCOUNT   ACCOUNT DESCRIPTION ~
        ~                          DEBITS            CREDITS

L13000: %                  ---------   ------------  --------------------~
        ~----------        --------------     --------------

L13030: %                   ########   ############  ####################~
        ~##########       -###,###,###.##    -###,###,###.##

L13060: %                                                                ~
        ~                  --------------     --------------

L13090: %                                                        * ######~
        ~## TOTALS:       -###,###,###.##    -###,###,###.##

L13120: %                                                       **  JOURN~
        ~AL TOTALS:       -###,###,###.##    -###,###,###.##

L13150: %ENTRIES FROM ########## ######## TO ########## ########


L13180: %                                                        ** END O~
        ~F REPORT **

        print_expanded_cogs
            if mode$ = "CR"  then postamt, postamt_tot = crs(a%)         ~
                             else postamt, postamt_tot = drs(a%)
            gosub breakout_cogs
               if start% = 0% then return
            first_print% = 0%

            for i% = 1% to 12%
                if consolidate_cogs$(i%) = " " then L13600/* Next I% */
                if mode$ = "CR" then L13390
                    acct1$ = consolidate_cogs$(i%):call "GLFMT" (acct1$)
                    call "CONVERT" (consol_post_amt(i%), 2.2, amt1$)
                    if consol_post_amt(i%) = 0 then L13600  /* NEXT I% */
                    if first_print% = 0% then L13490
                    print using L12750, " ", " ", " ", " ", " ", " ", " ",~
                                       acct1$, amt1$, " ", " "
                    line% = line% + 1%
                    goto L13540      /* Finish up */
L13390:         /* Credit Side */
                acct2$ = consolidate_cogs$(i%):call "GLFMT" (acct2$)
                call "CONVERT" (consol_post_amt(i%), 2.2, amt2$)
                if consol_post_amt(i%) = 0 then L13600  /* NEXT I% */
                if first_print% = 0% then L13490
                   print using L12750, " ", " ", " ", " ", " ", " ", " ", ~
                                      " ", " ", acct2$, amt2$
                   line% = line% + 1%
                   goto L13540       /* Finish up */
                /* Printing first line */
L13490:         print using L12750, printstore$, printpart$,              ~
                                   printcuscode$, printdoc$, printseqnr$,~
                                   printlot$, qty$,                      ~
                                   acct1$, amt1$, acct2$, amt2$
                first_print% = 1%

L13540:         /* Add to the summary file */
                if mode$ = "CR" then L13570
                     acct1$ = acct1$
                     call "GLUNFMT" (acct1$)
                     cramt = 0 : dramt = consol_post_amt(i%)
                     gosub update_summary
                     goto L13600
L13570:          acct1$ = acct2$
                 call "GLUNFMT" (acct1$)
                 dramt = 0 : cramt = consol_post_amt(i%)
                 gosub update_summary

L13600:     next i%

            return

        REM *************************************************************~
            *                P O S T    G / L                           *~
            * --------------------------------------------------------- *~
            * Post General Ledger in the manner indicated.  As we go    *~
            * kill the transaction records.  When done, clear the       *~
            * records not posted from and reset the control record.     *~
            *************************************************************
        post_gl
            close printer  :  call "SETPRNT" ("SHP005", " ", 0%, 1%)
            call "SHOSTAT" ("Posting Shipping Transactions")
*        First update the control record to show that we are updating
            readkey$ = "0"
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L14140, "U"
L14140:         FMT POS(128), CH(1)
            rewrite #1


*        Now branch to the posting loop per the summary flag
            if post$ = "S" then summary_post else detail_post


        summary_post      /* Post G/L from the Summary created         */
          summary_post_loop
            plowkey$ = file$ & hex(ff) & all(hex(00))
            call "PLOWNEXT" (#1, plowkey$, 2%, f1%(1))
            if f1%(1) = 0% then postings_done
            if str(plowkey$,,1) <> file$ then postings_done
                get #1 using L14290, postdate$, acct$, dramt, cramt
L14290:              FMT XX(2), CH(6), CH(9), POS(127), 2*PD(14,4)
                text$ = " "
                str(text$,69) = "SHIPPING: INVENTORY SUMMARY POST"
                tran_type$ = "RSJ01"
                postamt = dramt - cramt
                tempacct$ = acct$
                if export_on$ = "Y" then gosub load_gl_info
                call "GLPOST2" (acct$, dramt, cramt, postdate$, 0%,      ~
                                "01", text$, "RSJ", postseq%, userid$,   ~
                                #3, #4, #2, ret%, str(doc$,,16),         ~
                                gl_post_info$())
                call "READ101" (#1, plowkey$, f1%(1))
                delete #1
            goto summary_post_loop


        detail_post       /* Post G/L Transaction by Transaction       */
          detail_post_loop
            plowkey$ = file$ & all(hex(00))
            call "PLOWNXT1" (#1, plowkey$, 1%, f1%(1))
            if f1%(1) = 0% then postings_done
            if str(plowkey$,,1) <> file$   then postings_done
            if str(plowkey$,2,1) = hex(ff) then postings_done
                get #1 using L14500, postdate$, store$, part$, cuscode$,  ~
                                    doc$, seqnr$, lot$, userid$, qty,    ~
                                    draccts$(), drs(), craccts$(), crs(),~
                                    cost(),                              ~
                                    drtype$(), crtype$(), dmc
L14500:              FMT POS(48), CH(6), CH(3), CH(25), CH(9), CH(20),   ~
                         CH(3), CH(6), XX(7), CH(3), PD(14,4),           ~
                         8*CH(9), 8*PD(14,4), 8*CH(9), 8*PD(14,4),       ~
                         POS(410), 12*PD(14,4),                          ~
                         8*CH(1), 8*CH(1), PD(14,4)
                     text$ = " "
                     str(text$, 1,30) = str(cuscode$) & doc$
                     shp_type$ = "1"
                     if str(doc$,1,8) <> "INVOICE:" then L14555
                     doc$ = str(doc$,10,11)
                     shp_type$ = "2"
L14555:         for a% = 1% to 8%
                     str(text$,31,34) = str(part$,,25) & str(store$,,3) &~
                                                             lot$
                     str(text$,69,32) = "SHIPPING: INVENTORY POSTING"
                     if draccts$(a%) = " " then L14627
                          postamt, postamt_tot = drs(a%)
                          tran_type$ = "RSJ01"
                          if drtype$(a%) = "H" then L14589
                          str(tran_type$,5,1) = "2"
                          if drtype$(a%) = "B" then L14589
                          str(tran_type$,5,1) = "3"
                          if drtype$(a%) = "C" then L14589
                          if drtype$(a%) = "S" then tran_type$ = "RSJ05" ~
                             else tran_type$ = "RSJ04"
L14589:                   tempacct$ = draccts$(a%)
                          if export_on$ = "Y" then gosub load_gl_info
                          gosub check_for_expanding_cogs
                          if expand_cogs% = 0% then L14600
                              mode$ = "DR"
                              gosub post_expanded_cogs
                              if start% <> 0% then L14627 /*Success*/
L14600:                   call "GLPOST2" (draccts$(a%), drs(a%), 0,      ~
                                          postdate$, 0%, "01", text$,    ~
                                          "RSJ", postseq%, userid$,      ~
                                          #3, #4, #2, ret%,str(doc$,,16),~
                                          gl_post_info$())
L14627:              if craccts$(a%) = " " then L14678
                          postamt_tot = crs(a%)
                          postamt = -crs(a%)
                          tran_type$ = "RSJ01"
                          if crtype$(a%) = "H" then L14639
                          str(tran_type$,5,1) = "2"
                          if crtype$(a%) = "B" then L14639
                          str(tran_type$,5,1) = "3"
                          if crtype$(a%) = "C" then L14639
                          if crtype$(a%) = "S" then tran_type$ = "RSJ05" ~
                             else tran_type$ = "RSJ04"
L14639:                   tempacct$ = craccts$(a%)
                          if export_on$ = "Y" then gosub load_gl_info
                          gosub check_for_expanding_cogs
                          if expand_cogs% = 0% then L14650
                              mode$ = "CR"
                              gosub post_expanded_cogs
                              if start% <> 0% then L14678 /*Success*/
L14650:                   call "GLPOST2" (craccts$(a%), 0, crs(a%),      ~
                                          postdate$, 0%, "01", text$,    ~
                                          "RSJ", postseq%, userid$,      ~
                                          #3, #4, #2, ret%,str(doc$,,16),~
                                          gl_post_info$())
L14678:         if mgtrpt_on$ = "Y" then gosub init_mgt
                next a%
                call "READ101" (#1, plowkey$, f1%(1))
                delete #1
                goto detail_post_loop


        load_gl_info
            if post$ <> "S" then gosub set_posting_info
L15020:     put str(gl_post_info$(),,) using L15490,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Transaction Currency amount*/~
                postamt,                 /* Functional Currency amount */~
                qty,                     /* Unit amount                */~
                cuscode$,                /* Customer code CH(9)        */~
                so$,                     /* Sales Order number CH(16)  */~
                bol$,                    /* BOL number CH(3)           */~
                custype$,                /* Customer Type CH(2)        */~
                state$,                  /* State CH(2)                */~
                country$,                /* Country CH(3)              */~
                zip$,                    /* ZIP CH(9)                  */~
                salesr$,                 /* Sales Region CH(4)         */~
                salest$,                 /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                salesm$,                 /* Salesman code CH(4)        */~
                invoice$,                /* Invoice Number CH(8)       */~
                part$,                   /* Part Number CH(25)         */~
                partcat$,                /* Part Category CH(4)        */~
                partclass$,              /* Part Class CH(4)           */~
                partgen$,                /* Part Generic code CH(16)   */~
                parttype$,               /* Part Type CH(3)            */~
                uom$,                    /* Part UOM CH(4)             */~
                store$,                  /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
                project$,                /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                mdmc,                    /* Mgt DMC PD(14,4)           */~
                msval,                   /* Mgt Sales Value  PD(15,4)  */~
                msvol,                   /* Mgt Sales Volume PD(15,4)  */~
                " ",                     /* Filler                     */~
                icc$,                    /* Intercompany Corp CH(6)    */~
                " ",                     /* Intradiv Sales Acct CH(9)  */~
                " ",                     /* Alt Owner Sales Acct CH(9) */~
                accta$,                  /* Intradiv CoGS Acct CH(9)   */~
                acctb$,                  /* Alt Owner CoGS Acct CH(9)  */~
                tempacct$,               /* CMS Posting Account CH(9)  */~
                " "                      /* Filler                     */

            return

L15490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Transaction Currency amount*/~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice  CH(16)     */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                PD(14,4),                /* Mgt DMC PD(14,4)           */~
                PD(15,4),                /* Mgt Sales Value  PD(15,4)  */~
                PD(15,4),                /* Mgt Sales Volume PD(15,4)  */~
                CH(8),                   /* Filler                     */~
                CH(6),                   /* Intercompany Corp CH(6)    */~
                CH(9),                   /* Intradiv Sales Acct CH(9)  */~
                CH(9),                   /* Alt Owner Sales Acct CH(9) */~
                CH(9),                   /* Intradiv CoGS Acct CH(9)   */~
                CH(9),                   /* Alt Owner CoGS Acct CH(9)  */~
                CH(9),                   /* CMS Posting Account CH(9)  */~
                CH(108)                  /* Filler                     */

        breakout_cogs      /*Break Out the COGS for Cost buckets & Post */
          /* Broken out COGS were captured previously via CHECK_FOR_EX..*/
          /* Consolidate COGS Breakout, any Duplicate Accts will combine*/
            mat consolidate_cost = zer
            start% = 0%
            init(" ") consolidate_cogs$()
            /* What is the first non blank acct? */
            for i% = 1% to 12%
                if expanded_cogs$(i%) = " " then L16110
                    start% = i% + 1%
                    consolidate_cost(1%)  = cost(i%)
                    consolidate_cogs$(1%) = expanded_cogs$(i%)
                    i% = 13%
L16110:     next i%
            if start% = 0% then return  /* Big trouble, No Accts */
            if start% > 12% then  L16280     /* Jump to Posting */

            /* Consolidate Buckets Accts and Amounts */
            for i% = start% to 12%
              if expanded_cogs$(i%) = " " then L16260   /* Next I% */
              for j% = 1% to 12%
                if expanded_cogs$(i%)<>consolidate_cogs$(j%) then L16210
                    consolidate_cost(j%) = consolidate_cost(j%) + cost(i%)
                    j% = 13%
                    goto L16250                      /* Exit J Loop */
L16210:         if consolidate_cogs$(j%) <> " " then L16250  /*Next J*/
                    consolidate_cost(j%)  = cost(i%)
                    consolidate_cogs$(j%) = expanded_cogs$(i%)
                    j% = 13%                        /* Exit J Loop */
L16250:       next j%
L16260:     next i%

L16280:   /* Lets Set the Posting Amounts w/o Rounding errors */
            total_cost, rnd_total = 0
            for i% = 1% to 12%:total_cost = total_cost + cost(i%):next i%
            if total_cost <> 0 then L16320   /* Zero Shouldn't happen */
                start% = 0%   /* Will post to reqular acct */
                return
L16320:     mat consol_post_amt = zer
            for i% = 1% to 12%
              consol_post_amt(i%) =  round(                              ~
                      consolidate_cost(i%) * postamt_tot / total_cost, 2)
              rnd_total = rnd_total + consol_post_amt(i%)
            next i%
            delta = postamt_tot - rnd_total
            if delta = 0 then return
            /* Distribute round error */
            max_bckt% = 1% : max_amt  = 0
            for i% = 1% to 12%
                if consol_post_amt(i%) <= 0 then L16480   /* Next I% */
                    if consol_post_amt(i%) <= max_amt then L16480
                        max_amt = consol_post_amt(i%)
                        max_bckt% = i%
L16480:     next i%
           /* Force the Round correction */
            consol_post_amt(max_bckt%) = consol_post_amt(max_bckt%)      ~
                                                        + delta
            return

        post_expanded_cogs /*Break Out the COGS for Cost buckets & Post */

            gosub breakout_cogs
            if start% = 0% then return

          /* Accounts are broken out and then consolidated & Ammounts   */
          /*  Distributed so We're All ready to post the new COGS accts */

            for i% = 1% to 12%
                postamt =  consol_post_amt(i%)
                tempacct$ = consolidate_cogs$(i%)
                if tempacct$ = " " or postamt = 0 then L16760 /* Next I% */

                if export_on$ = "Y" then gosub load_gl_info
                if mode$ = "CR" then                                     ~
                          call "GLPOST2" (tempacct$, 0,  postamt,        ~
                                          postdate$, 0%, "01", text$,    ~
                                          "RSJ", postseq%, userid$,      ~
                                          #3, #4, #2, ret%,str(doc$,,16),~
                                          gl_post_info$())               ~
                                else                                     ~
                          call "GLPOST2" (tempacct$, postamt,    0,      ~
                                          postdate$, 0%, "01", text$,    ~
                                          "RSJ", postseq%, userid$,      ~
                                          #3, #4, #2, ret%,str(doc$,,16),~
                                          gl_post_info$())
L16760:     next i%
            return


        postings_done     /* Clear the rest of the file and sign-off   */
            plowkey$ = file$
            call "DELETE" (#1, plowkey$, 1%)

            readkey$ = "0"
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L17070, " "
L17070:         FMT POS(128), CH(100)
            rewrite #1
            goto L65000

        check_for_expanding_cogs
            expand_cogs% = 0%
            if fs%(11%) <> 1% then return  /* No Breakout COGS File */
            if a% <> 1% then return /* Only 1st acct considered for this*/
            readkey$ = tempacct$
            call "READ100" (#11, readkey$, f1%(11%))
            if f1%(11%) = 0% then return

            get #11 using L17180, expanded_cogs$()
L17180:        FMT POS(10), 12*CH(9)
            expand_cogs% = 1%
            return

        set_posting_info
            if cuscode$ = lastcust$ then L18100

            /* start with customer master */
            gosub init_customer /* clear the customer related variables */
            call "READ100" (#5, cuscode$, f1%(5))
               if f1%(5) = 0% then L18100 /* where did he go? */
            get #5 using L18080, custype$, country$
L18080:     FMT POS(1023), CH(2), POS(1073), CH(3)

L18100:     /* now check if a sales order or invoice */
            if shp_type$ = "2" then L18310

            /* Load up the sales order information */
            readkey$ = str(cuscode$) & str(doc$,1,pos(doc$="-")-1%)
            bol$ = str(doc$,pos(doc$="-")+1%,3%)
            call "READ100" (#7, readkey$, f1%(7))
               if f1%(7) = 0% then L18470
            lastcust$ = cuscode$
            get #7 using L18210, so$, state$, zip$, salesm$, salesr$

L18210:     FMT POS(10), CH(16), POS(210), CH(2), XX(1), CH(9), POS(580),~
                CH(4), POS(595), CH(4)

            readkey$ = str(readkey$,10,16) & seqnr$
            call "READ100" (#9, readkey$, f1%(9))
               if f1%(9) = 0% then L18470
            get #9 using L18280, project$
L18280:     FMT POS(224), CH(8)
            goto L18470 /* check on part number */

L18310:     /* Load up the invoice information */
            readkey$ = str(cuscode$) & str(doc$,1,8)
            call "READ100" (#8, readkey$, f1%(8))
               if f1%(8) = 0% then L18470
            get #8 using L18370, invoice$, so$, bol$, state$, zip$,       ~
                                salesm$, salesr$, salest$
L18370:     FMT POS(10), CH(8), POS(34), CH(16), CH(3), POS(221), CH(2), ~
                XX(1), CH(9), POS(501), CH(4), POS(516), CH(4), POS(873),~
                CH(10)

            str(readkey$,18,3) = seqnr$
            call "READ100" (#10, readkey$, f1%(10))
               if f1%(10) = 0% then L18470
            get #10 using L18450, project$
L18450:     FMT POS(618), CH(8)

L18470:     /* Load up the part information */
            gosub init_part_info
            call "READ100" (#6, part$, f1%(6))
                if f1%(6) = 0% then L18545
            get #6 using L18530, partgen$, uom$, partcat$, partclass$,    ~
            parttype$
L18530:     FMT POS(58), CH(16), CH(4), POS(90), CH(4), POS(133), CH(4), ~
                POS(180), CH(3)
L18545:     gosub mgt_report_info
            return

        init_customer
            init (" ") country$, custype$, state$, zip$, salesm$,        ~
                       salesr$, salest$, project$
            return

        init_part_info
            init (" ") partgen$, partcat$, uom$, partclass$, parttype$
            return

        init_mgt
            init (" ") icc$, accta$, acctb$
            mdmc, msval, msvol = 0
            factor = 1
            return

        mgt_report_info
            if tran_type$ <> "RSJ03" then init_mgt /* RETURN from there */
            if mgtrpt_on$ <> "Y" then return /* Skip Management Rpting */
            if dmc = -1 then return  /* No Mgt DMC on file for part    */
                call "MGTVALUS" ("S", cuscode$, part$, postdate$, factor,~
                                 accta$, acctb$, icc$, #5, #6, #2)
                msval = postamt : msvol = qty : mdmc = dmc
                if icc$ = " " then return /* Regular Sale */
                if factor = -999 then factor = 1  /* Factor not found */
                if accta$ <> " " and acctb$ <> " " then gosub xtra_mgt_rec
            return

        xtra_mgt_rec  /* Type 3 transaction creates extra mgt records. */
            save_postamt    = postamt
            save_qty        = qty
            save_tran_type$ = tran_type$
            postamt, qty = 0

*        Set up for Intra-Divisional CoGS Record
            tran_type$ = "RSJ06"
            msval = mdmc * save_qty
            tempacct$ = accta$
            gosub L15020  /* LOAD_GL_INFO without SET_POSTING_INFO */
            call "GLPOST2" (accta$, 0, 0, postdate$, 0%, "01", text$,    ~
                            "RSJ", postseq%, userid$, #3, #4, #2, ret%,  ~
                            str(doc$,,16), gl_post_info$())

*        Set up for Alternate Owner CoGS Record
            tran_type$ = "RSJ07"
            msval = mdmc * save_qty * factor
            tempacct$ = acctb$
            gosub L15020  /* LOAD_GL_INFO without SET_POSTING_INFO */
            call "GLPOST2" (acctb$, 0, 0, postdate$, 0%, "01", text$,    ~
                            "RSJ", postseq%, userid$, #3, #4, #2, ret%,  ~
                            str(doc$,,16), gl_post_info$())

            postamt    = save_postamt
            qty        = save_qty
            tran_type$ = save_tran_type$
            gosub init_mgt
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
            call "JNLCLOSE" ("01", "RSJ", postseq%, ret%)
            call "SHOSTAT" ("One Moment Please")

            end
