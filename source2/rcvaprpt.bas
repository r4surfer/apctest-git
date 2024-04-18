        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    CCC   V   V    A    PPPP   RRRR   PPPP   TTTTT   *~
            *  R   R  C   C  V   V   A A   P   P  R   R  P   P    T     *~
            *  RRRR   C      V   V  AAAAA  PPPP   RRRR   PPPP     T     *~
            *  R   R  C   C   V V   A   A  P      R   R  P        T     *~
            *  R   R   CCC     V    A   A  P      R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVAPRPT - Print Uninvoiced (open) Receivers in fore or   *~
            *            background.                                    *~
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
            * 06/18/86 ! Original                                 ! HES *~
            * 05/19/87 ! Standard Costing Modifications           ! ERN *~
            * 04/17/91 ! PRR 10947/10922  Set Select defaults to  ! SID *~
            *          !   'ALL'.                                 !     *~
            * 02/19/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 02/07/94 ! PRR 12997 - Changed requirements for     ! JBK *~
            *          !   inclusion in report.  Now very small   !     *~
            *          !   uninvoiced qtys will not be included.  !     *~
            * 12/05/94 ! Add Sort option 'by Expense Acct'.       ! RJH *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            back$1,                      /* Background flag            */~
            beg_date$10,                 /* Beginning Received Date    */~
            beg_part$(2)25,              /* Beginning Part Number      */~
            beg_po$(2)16,                /* Beginning Purchase Order   */~
            beg_receiver$(2)16,          /* Beginning Receiver Number  */~
            beg_vendor$(2)9,             /* Beginning Vendor Number    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            end$1,                       /* End Program Flag           */~
            end_date$10,                 /* Ending Received Date       */~
            end_part$(2)25,              /* Ending Part Number         */~
            end_po$(2)16,                /* Ending Purchase Order      */~
            end_receiver$(2)16,          /* Ending Receiver Number     */~
            end_vendor$(2)9,             /* Ending Vendor Number       */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Work File Name             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_po$16,                  /* Last P.O.     Processed    */~
            last_expense_acct$12,        /* Last GL Expense Account    */~
            last_vendor$9,               /* Last Vendor   Processed    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Work File Library          */~
            line2$79,                    /* Second Line of Screen Headr*/~
            message$78,                  /* Informational Message      */~
            ok$1,                        /* OK to Print? Flag          */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_title$60,              /* Report Sorted By Title     */~
            print$(4)10,                 /* Work Variable              */~
            print1$12,                   /* Work Variable              */~
            rpt_time$8,                  /* Report Time                */~
            sort$116,                    /* Sort parameters to SORTCALL*/~
            sortby$1,                    /* Sorting Sequence,D,G, or V */~
            temp1$10, temp2$10,          /* Date Dummy Variables       */~
            title$(2)30,                 /* Screen Column Headings     */~
            userid$3,                    /* Current User Id            */~
            vol$6                        /* Work File Volume           */~

        dim                 /* FILE: RCVLINES Variables                */~
            part$25        ,/* Part code                               */~
            receiver$16    ,/* Receiver Control Number                 */~
            vendor$9       ,/* Vendor code                             */~
            po$16          ,/* Purchase Order Number                   */~
            line$3         ,/* Purchase Line Sequence Number (not ITEM */~
            partdescr$32   ,/* part number description                 */~
            podate$8       ,/* Date Ordered (Purchase Order Date)      */~
            duedate$8      ,/* P.O. Line Item Due Date                 */~
            lastrecdate$8  ,/* P.O. Line Item Date of Last Receipt -Las*/~
            packslip$16    ,/* Packing Slip # for last quantity receive*/~
            textid1$4      ,/* Internal ID to Purchase Order Level Text*/~
            textid2$4      ,/* Internal ID to P.O. Line Item Level Text*/~
            textid3$4      ,/* Internal ID to Q.C. Line Item Level Text*/~
            vend_uom$4     ,/* Unit of Measure                         */~
            payables_acct$9,/* Payables Account                        */~
            rec_hold_acct$9,/* General Ledger Account Number           */~
            qc_hold_acct$9 ,/* General Ledger Account Number           */~
            expense_acct$12,/* GENERAL LEDGER ACCOUNT NUMBER           */~
            store$3        ,/* Store or Warehouse Code                 */~
            lot$6          ,/* Which lot in inventory - always used wit*/~
            scrap_store$3  ,/* Warehouse or Store                      */~
            scrap_lot$6    ,/* Lot Number                              */~
            reject_code$6   /* rejection code  ccccss fmt              */~

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
            * #1  ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #2  ! SYSFILE2 ! SYSTEM INFO (DEFAULT PAY DATES)          *~
            * #4  ! WORKFILE ! Temporary Sort / Work File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  800,                                  ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24

            select #2,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #4, "WORKFILE",                                       ~
                        varc,     consec,  recsize =  400

            call "SHOSTAT" ("Opening Files, One Moment Please")

            rslt$(1), rslt$(2) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$, "TT", back$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "GETNAMES" addr(#1, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",records%,ret%)
            call "COMPNAME" (12%, company$, ret%)

            title$(1) = "From"  :  title$(2) = "To  "

            if back$ = "B" then printing_in_background

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)EXIT PROGRAM"
            errormsg$, inpmessage$, beg_receiver$(), beg_date$, sortby$, ~
            end_receiver$(), end_date$, last_vendor$, end$, beg_part$(), ~
            end_part$(), beg_po$(), end_po$(), beg_vendor$(),            ~
            end_vendor$(), back$, last_expense_acct$, last_po$    = " "
            ventotal, rpt_total, exp_subtotal = 0.0
            lines_printed%, end_report% = 0%

            for fieldnr% = 1 to 7
L10150:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10270
L10170:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10250
L10200:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10170
                         if fieldnr% = 1% then L10150
                         goto L10200
L10250:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10170
L10270:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10170
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
            pf16$ = "(16)PRINT REPORT"
            if back$ = "Y" then pf16$ = "(16)SUBMIT TASK"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
            oldfield% = 0%
L11170:     fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% > 7 then edtpg1
            if fieldnr% = oldfield% then edtpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11240:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11240
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11240
                  oldfield% = fieldnr%
                  goto L11170

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if back$ <> "Y" then L19135
                call "READ101" (#2, "zRCVAPRPT." & userid$, f1%(2))
                put #2, using L19070, "zRCVAPRPT." & userid$,             ~
                  beg_vendor$(), end_vendor$(), beg_po$(), end_po$(),    ~
                  beg_part$(), end_part$(), beg_receiver$(),             ~
                  end_receiver$(), beg_date$, end_date$, sortby$
                if f1%(2) = 0 then write #2  else rewrite #2
L19070:         FMT CH(20), 19*CH(25)
                call "TASKUP" ("ME", 0%)
                goto L65000

                printing_in_background
                call "READ101" (#2, "zRCVAPRPT." & userid$, f1%(2))
                     if f1%(2) = 0 then error_exit
                get #2, using L19070, errormsg$,                          ~
                  beg_vendor$(), end_vendor$(), beg_po$(), end_po$(),    ~
                  beg_part$(), end_part$(), beg_receiver$(),             ~
                  end_receiver$(), beg_date$, end_date$, sortby$
                delete #2

L19135:     plowkey$ = all(hex(ff))
            str(plowkey$,,9) = beg_vendor$(2)
            if beg_date$ <> "ALL" then call "DATUFMTC" (beg_date$)
            call "DATUFMTC" (end_date$)
            if sortby$ = "V" then L19265   /* Don't need to do workfile */
            call "SHOSTAT" ("Now Selecting Items to Report")
            counter% = 0%
            records% = max(records%,100%)
            call "WORKOPN2" (#4, "OUTPT", records%, f2%(4))
            gosub selection_process
            close #4
            if counter% > 0% then L19245
L19195:        if back$ = "B" then L19340
               k% = 2%
               call "ASKUSER" (k%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no Receiver Lines eligible for Printing",       ~
                 "were found using the given selection parameters.",     ~
                 "Press RETURN to change the parameters or exit.")
               call "FILEBGON" (#4)
               if beg_date$ <> "ALL" then call "DATFMTC" (beg_date$)
               call "DATFMTC" (end_date$)
               goto edtpg1
L19245:     call "SHOSTAT" ("Now Sorting Items Selected")
            gosub sort_file
            call "WORKOPN2" (#4, "INPUT", records%, f2%(4))

L19265:     call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("RCV002","        ",records%*4%, 0%)
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            select printer (134)
            l% = 0%                     /* Number of Lines Left on Page*/
            page% = -1%                 /* Page Counter                */
            gosub print_params
            if sortby$ = "V" then gosub print_by_vendor                  ~
                             else gosub print_by_sortfile
            if page% = 0% then L19195
            gosub vendor_totals
            end_report% = 1%
            gosub print_exp_acct_break
            gosub report_totals
            close printer
            call "SETPRNT" ("RCV002","        ",records%, 1%)
L19340:     call "FILEBGON" (#4)
            if back$ <> "B" then inputmode
            message$ = "rptReport RCVAPRPT in background: No data met sel~
        ~ection criteria."
                if page% = 0% then L19390
            message$ = "rptReport RCVAPRPT in background: Completed"
            call "TIME" (str(message$,45,8))
            goto L19390
            error_exit : message$ = "rptReport RCVAPRPT in background: Ca~
        ~nceled."
L19390:     call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
            goto L65000

        selection_process
            call "PLOWALTS" (#1, plowkey$, 2%, 0%, f1%(1))
                if f1%(1) = 0% then return
            gosub load_rcvlines
            ok$ = "Y"
            gosub include_test
            if ok$ = "X" then return
            if ok$ <> "Y" then selection_process
            gosub write_sort_record
            goto selection_process

        include_test
            if vendor$ <= beg_vendor$(2) then ok$ = "N"
            if vendor$ > end_vendor$(2) then ok$ = "X"  /* Done */
            if po$ <= beg_po$(2) or po$ > end_po$(2) then ok$ = "N"
            if part$ <= beg_part$(2) or part$ > end_part$(2) then ok$="N"
            if receiver$<=beg_receiver$(2) or receiver$>end_receiver$(2) ~
                  then ok$ = "N"
            if lastrecdate$ < beg_date$ or lastrecdate$ > end_date$      ~
                  then ok$ = "N"
            if open_qty = 0 then ok$ = "N"
            if abs(open_qty) < .005 and abs(amt_not_inv) < .005          ~
                  then ok$ = "N"
            return

        sort_file
            call "GETNAMES" addr(#4, file$, lib$, vol$)
            sort$ = file$
            str(sort$,9%,8%) = lib$
            str(sort$,17%,6%) = vol$
            str(sort$,23%,22%) = str(sort$,,22%)
            if sortby$ = "D" then  L19630  /* else must be 'G' */
                str(sort$,45%,9%) = "0323009CA"  /* GL Expense Acct. */
                str(sort$,54%,9%) = "0122006CA"  /* Date Received    */
                str(sort$,63%,9%) = "0042009CA"  /* Vendor    #      */
                str(sort$,72%,9%) = "0051016CA"  /* P.O.      #      */
                str(sort$,81%,9%) = "0067003CA"  /* Line Item #      */
                str(sort$,90%,9%) = "0026016CA"  /* Receiver  #      */
                goto  L19680
L19630:     str(sort$,45%,9%) = "0122006CA"        /* Date Received    */
            str(sort$,54%,9%) = "0042009CA"        /* Vendor    #      */
            str(sort$,63%,9%) = "0051016CA"        /* P.O.      #      */
            str(sort$,72%,9%) = "0067003CA"        /* Line Item #      */
            str(sort$,81%,9%) = "0026016CA"        /* Receiver  #      */

L19680:     call "SORTCALL" addr(sort$, ret%)
            if ret% = 0% then return
               if back$ = "B" then error_exit
               k% = 2%
               call "ASKUSER" (k%, "*** SORT FAILURE ***",               ~
                "Either unable to link to SORT (not found or protected)",~
                "or SORT failed for some other reason (not enuff space)."~
                ,"Press RETURN to acknowlege & exit.")
               goto exit_program
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20160,         /* Vendor Range     */~
                                    L20200,         /* PO Range         */~
                                    L20240,         /* Part Range       */~
                                    L20280,         /* Receiver Range   */~
                                    L20320,         /* Receive Date Rng */~
                                    L20420,         /* Sorting Sequence */~
                                    L20480          /* Print In Bckgrnd?*/
                     return
L20160:     REM Default/Enable for Vendor Range
                if beg_vendor$(1) = " " then beg_vendor$(1) = "ALL"
                inpmessage$ = "Enter the Vendor Code Range To be " &     ~
                              "Included or 'ALL' for all Vendors."
                return
L20200:     REM Default/Enable for PO Range
                if beg_po$(1) = " " then beg_po$(1) = "ALL"
                inpmessage$ = "Enter the P.O. Number Range To be " &     ~
                              "Included or 'ALL' for all P.O.'s."
                return
L20240:     REM Default/Enable for Part Range
                if beg_part$(1) = " " then beg_part$(1) = "ALL"
                inpmessage$ = "Enter the Part Number Range To be " &     ~
                              "Included or 'ALL' for all Parts."
                return
L20280:     REM Default/Enable for Receiver Number Range
                if beg_receiver$(1) = " " then beg_receiver$(1) = "ALL"
                inpmessage$ = "Enter the Receiver # Range To be " &      ~
                              "Included or 'ALL' for all Receivers."
                return
L20320:     REM Default/Enable for Beginning Received Date
                inpmessage$ = "Enter the Receive Date Range to Print"
                if beg_date$ = " " or beg_date$ = blankdate$ ~
                                 then beg_date$ = "19000101"
                if end_date$ = " " or end_date$ = blankdate$ ~
                                 then end_date$ = "20991231"
                call "DATEOKC" (beg_date$, 0%, " " )
                call "DATEOKC" (end_date$, 0%, " " )
                return

L20420:     REM Default/Enable for Sorting Sequence
                if sortby$ = " " then sortby$ = "V"
                inpmessage$ = "Print by: 'D' - Recieved Date, 'G' - G/L" ~
                              &  " Expense Acct, 'V' - Vendor."
                return

L20480:     REM Default/Enable for Print In Background?
                if back$ = " " then back$ = "N"
                inpmessage$ = "Enter 'Y' to print this report in"  &     ~
                              " background."
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_rcvlines
            get #1 using L35030, /* FILE: RCVLINES                      */~
            part$          ,/* Part code                               */~
            receiver$      ,/* Receiver Control Number                 */~
            vendor$        ,/* Vendor code                             */~
            po$            ,/* Purchase Order Number                   */~
            line$          ,/* Purchase Line Sequence Number (not ITEM */~
            sdate%         ,/* System Date                             */~
            stime%         ,/* The System Time when a transaction was e*/~
            partdescr$     ,/* part number description                 */~
            podate$        ,/* Date Ordered (Purchase Order Date)      */~
            duedate$       ,/* P.O. Line Item Due Date                 */~
            lastrecdate$   ,/* P.O. Line Item Date of Last Receipt -Las*/~
            packslip$      ,/* Packing Slip # for last quantity receive*/~
            sdate%         ,/* System Date                             */~
            stime%         ,/* System Time                             */~
            textid3$       ,/* PO Level Receiver Text ID               */~
            qty_recvd      ,/* Total Quantity Received To Date - Purcha*/~
            qty_rec_hold   ,/* Total Quantity currently in Receiving Ho*/~
            qty_qc         ,/* Quantity currently in QC (Pending)-Purch*/~
            qty_qc_hold    ,/* Total Quantity currently in QC Hold (P.O*/~
            qty_scrapped   ,/* Total quantity rejected /  scrapped.    */~
            qty_returned   ,/* Total Quantity Returned to Vendor.      */~
            qty_to_hny     ,/* Total quantity moved to on-hand inventor*/~
            po_price       ,/* PO unit Price                           */~
            filler         ,/* Filler                                  */~
            filler         ,/* Filler                                  */~
            conv_factor    ,/* # of Stocking (base) Units which go into*/~
            vend_uom$      ,/* Unit of Measure                         */~
            extension      ,/* Extension Amount (Quantity * Price)     */~
            amt_not_inv    ,/* Value of Receiver Line that HASN'T been */~
            amt_invoiced   ,/* Value of Receiver Line that HAS been inv*/~
            ap_adj_amt     ,/* Accounts Payable Adjustment Amount      */~
            scrap_adj_amt  ,/* Scrap Adjustment Amount                 */~
            rework_adj_amt ,/* Rework Adjustment Amount                */~
            payables_acct$ ,/* Payables Account                        */~
            rec_hold_acct$ ,/* General Ledger Account Number           */~
            qc_hold_acct$  ,/* General Ledger Account Number           */~
            expense_acct$  ,/* GENERAL LEDGER ACCOUNT NUMBER           */~
            store$         ,/* Store or Warehouse Code                 */~
            lot$           ,/* Which lot in inventory - always used wit*/~
            scrap_store$   ,/* Warehouse or Store                      */~
            scrap_lot$     ,/* Lot Number                              */~
            reject_code$   ,/* rejection code  ccccss fmt              */~
            open_qty       ,/* Open/Remaining Qty to Invoice           */~
            ap_price       ,/* A/P unit Price                          */~
            filler         ,/* Filler                                  */~
            filler         ,/* Filler                                  */~
            textid1$       ,/* Line Level Receiver Text ID             */~
            textid2$        /* Q.C.Line Item Text ID                   */

            return

        load_sortfile
            get #4 using L35030, /* FILE: SORTFILE                      */~
            part$          ,/* Part code                               */~
            receiver$      ,/* Receiver Control Number                 */~
            vendor$        ,/* Vendor code                             */~
            po$            ,/* Purchase Order Number                   */~
            line$          ,/* Purchase Line Sequence Number (not ITEM */~
            sdate%         ,/* System Date                             */~
            stime%         ,/* The System Time when a transaction was e*/~
            partdescr$     ,/* part number description                 */~
            podate$        ,/* Date Ordered (Purchase Order Date)      */~
            duedate$       ,/* P.O. Line Item Due Date                 */~
            lastrecdate$   ,/* P.O. Line Item Date of Last Receipt -Las*/~
            packslip$      ,/* Packing Slip # for last quantity receive*/~
            sdate%         ,/* System Date                             */~
            stime%         ,/* System Time                             */~
            textid3$       ,/* PO Level Receiver Text ID               */~
            qty_recvd      ,/* Total Quantity Received To Date - Purcha*/~
            qty_rec_hold   ,/* Total Quantity currently in Receiving Ho*/~
            qty_qc         ,/* Quantity currently in QC (Pending)-Purch*/~
            qty_qc_hold    ,/* Total Quantity currently in QC Hold (P.O*/~
            qty_scrapped   ,/* Total quantity rejected /  scrapped.    */~
            qty_returned   ,/* Total Quantity Returned to Vendor.      */~
            qty_to_hny     ,/* Total quantity moved to on-hand inventor*/~
            po_price       ,/* PO unit price                           */~
            filler         ,/* Filler                                  */~
            filler         ,/* Filler                                  */~
            conv_factor    ,/* # of Stocking (base) Units which go into*/~
            vend_uom$      ,/* Unit of Measure                         */~
            extension      ,/* Extension Amount (Quantity * Price)     */~
            amt_not_inv    ,/* Value of Receiver Line that HASN'T been */~
            amt_invoiced   ,/* Value of Receiver Line that HAS been inv*/~
            ap_adj_amt     ,/* Accounts Payable Adjustment Amount      */~
            scrap_adj_amt  ,/* Scrap Adjustment Amount                 */~
            rework_adj_amt ,/* Rework Adjustment Amount                */~
            payables_acct$ ,/* Payables Account                        */~
            rec_hold_acct$ ,/* General Ledger Account Number           */~
            qc_hold_acct$  ,/* General Ledger Account Number           */~
            expense_acct$  ,/* GENERAL LEDGER ACCOUNT NUMBER           */~
            store$         ,/* Store or Warehouse Code                 */~
            lot$           ,/* Which lot in inventory - always used wit*/~
            scrap_store$   ,/* Warehouse or Store                      */~
            scrap_lot$     ,/* Lot Number                              */~
            reject_code$   ,/* rejection code  ccccss fmt              */~
            open_qty       ,/* Open/Remaining Qty to Invoice           */~
            ap_price       ,/* A/P unit price                          */~
            filler         ,/* Filler                                  */~
            filler         ,/* Filler                                  */~
            textid1$       ,/* Line Level Receiver Text ID             */~
            textid2$        /* Q.C.Line Item Text ID                   */

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        write_sort_record
            put #4 using L35030, /* FILE: SORTFILE                      */~
            part$          ,/* Part code                               */~
            receiver$      ,/* Receiver Control Number                 */~
            vendor$        ,/* Vendor code                             */~
            po$            ,/* Purchase Order Number                   */~
            line$          ,/* Purchase Line Sequence Number (not ITEM */~
            sdate%         ,/* System Date                             */~
            stime%         ,/* The System Time when a transaction was e*/~
            partdescr$     ,/* part number description                 */~
            podate$        ,/* Date Ordered (Purchase Order Date)      */~
            duedate$       ,/* P.O. Line Item Due Date                 */~
            lastrecdate$   ,/* P.O. Line Item Date of Last Receipt -Las*/~
            packslip$      ,/* Packing Slip # for last quantity receive*/~
            sdate%         ,/* System Date                             */~
            stime%         ,/* System Time                             */~
            textid3$       ,/* PO Level Receiver Text ID               */~
            qty_recvd      ,/* Total Quantity Received To Date - Purcha*/~
            qty_rec_hold   ,/* Total Quantity currently in Receiving Ho*/~
            qty_qc         ,/* Quantity currently in QC (Pending)-Purch*/~
            qty_qc_hold    ,/* Total Quantity currently in QC Hold (P.O*/~
            qty_scrapped   ,/* Total quantity rejected /  scrapped.    */~
            qty_returned   ,/* Total Quantity Returned to Vendor.      */~
            qty_to_hny     ,/* Total quantity moved to on-hand inventor*/~
            po_price       ,/* PO unit price                           */~
            filler         ,/* Filler                                  */~
            filler         ,/* Filler                                  */~
            conv_factor    ,/* # of Stocking (base) Units which go into*/~
            vend_uom$      ,/* Unit of Measure                         */~
            extension      ,/* Extension Amount (Quantity * Price)     */~
            amt_not_inv    ,/* Value of Receiver Line that HASN'T been */~
            amt_invoiced   ,/* Value of Receiver Line that HAS been inv*/~
            ap_adj_amt     ,/* Accounts Payable Adjustment Amount      */~
            scrap_adj_amt  ,/* Scrap Adjustment Amount                 */~
            rework_adj_amt ,/* Rework Adjustment Amount                */~
            payables_acct$ ,/* Payables Account                        */~
            rec_hold_acct$ ,/* General Ledger Account Number           */~
            qc_hold_acct$  ,/* General Ledger Account Number           */~
            expense_acct$  ,/* GENERAL LEDGER ACCOUNT NUMBER           */~
            store$         ,/* Store or Warehouse Code                 */~
            lot$           ,/* Which lot in inventory - always used wit*/~
            scrap_store$   ,/* Warehouse or Store                      */~
            scrap_lot$     ,/* Lot Number                              */~
            reject_code$   ,/* rejection code  ccccss fmt              */~
            open_qty       ,/* Open/Remaining Qty to Invoice           */~
            ap_price       ,/* A/P unit price                          */~
            filler         ,/* Filler                                  */~
            filler         ,/* Filler                                  */~
            textid1$       ,/* Line Level Receiver Text ID             */~
            textid2$       ,/* Q.C.Line Item Text ID                   */~
            " "             /* Filler                                  */

            write #4
            counter% = counter% + 1%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: RCVLINES                          */~
            CH(25),         /* Part code                               */~
            CH(16),         /* Receiver Control Number                 */~
            CH(9),          /* Vendor code                             */~
            CH(16),         /* Purchase Order Number                   */~
            CH(3),          /* Purchase Line Sequence Number (not ITEM */~
            BI(4),          /* System Date                             */~
            BI(4),          /* The System Time when a transaction was e*/~
            CH(32),         /* part number description                 */~
            CH(6),          /* Date Ordered (Purchase Order Date)      */~
            CH(6),          /* P.O. Line Item Due Date                 */~
            CH(6),          /* P.O. Line Item Date of Last Receipt -Las*/~
            CH(16),         /* Packing Slip # for last quantity receive*/~
            BI(4),          /* System Date                             */~
            BI(4),          /* System Time                             */~
            CH(4),          /* P.O. level Receiver Text                */~
            PD(14,4),       /* Total Quantity Received To Date - Purcha*/~
            PD(14,4),       /* Total Quantity now in Receiving Hold    */~
            PD(14,4),       /* Quantity currently in QC (Pending)-Purch*/~
            PD(14,4),       /* Total Quantity currently in QC Hold (P.O*/~
            PD(14,4),       /* Total quantity rejected /  scrapped.    */~
            PD(14,4),       /* Total Quantity Returned to Vendor.      */~
            PD(14,4),       /* Total quantity moved to on-hand inventor*/~
            PD(14,7),       /* PO unit price                           */~
            PD(14,7),       /* Filler                                  */~
            PD(14,7),       /* Filler                                  */~
            PD(14,4),       /* # of Stocking (base) Units which go into*/~
            CH(4),          /* Unit of Measure                         */~
            PD(14,4),       /* Extension Amount (Quantity * Price)     */~
            PD(14,4),       /* Value of Receiver, Open Amount          */~
            PD(14,4),       /* Value of Receiver, Invoiced Amount      */~
            PD(14,4),       /* Accounts Payable Adjustment Amount      */~
            PD(14,4),       /* Scrap Adjustment Amount                 */~
            PD(14,4),       /* Rework Adjustment Amount                */~
            CH(9),          /* Payables Account                        */~
            CH(9),          /* Receiver Holding Account Number         */~
            CH(9),          /* Q. C. Holding Account Number            */~
            CH(9),          /* Expenses Account Number.                */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(6),          /* Which lot in inventory - Default value  */~
            CH(3),          /* Scrap Warehouse/Store Number            */~
            CH(6),          /* Scrap Lot Number                        */~
            CH(6),          /* rejection code  ccccss fmt              */~
            PD(14,4),       /* Open (Uninvoiced) Quantity              */~
            PD(14,7),       /* A/P unit price                          */~
            PD(14,7),       /* Filler                                  */~
            PD(14,7),       /* Filler                                  */~
            CH(4),          /* Line level Receiver Text                */~
            CH(4),          /* Q.C. Line Item Text                     */~
            CH(5)           /* Filler                                  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "RCVAPRPT: " & str(cms2v$,,8%)
                  str(line2$,,50) = "Report Selection Criteria"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40220,         /* Vendor Range     */~
                                    L40220,         /* PO Range         */~
                                    L40220,         /* Part Range       */~
                                    L40220,         /* Receiver Range   */~
                                    L40220,         /* Receive Date Rng */~
                                    L40220,         /* Sorting Sequence */~
                                    L40220          /* Print In Bckgrnd?*/
                  goto L40280

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40280:
L40290:     accept                                                       ~
               at (01,02), "Print Uninvoiced Receivers Report",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,22), fac(hex(ac)), title$(1)              , ch(25),~
               at (06,48), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (07,02), "Vendor Number",                              ~
               at (07,22), fac(lfac$( 1)), beg_vendor$(1)       , ch(09),~
               at (07,48), fac(lfac$( 1)), end_vendor$(1)       , ch(09),~
                                                                         ~
               at (08,02), "P. O. Number",                               ~
               at (08,22), fac(lfac$( 2)), beg_po$(1)           , ch(16),~
               at (08,48), fac(lfac$( 2)), end_po$(1)           , ch(16),~
                                                                         ~
               at (09,02), "Part Number",                                ~
               at (09,22), fac(lfac$( 3)), beg_part$(1)         , ch(25),~
               at (09,48), fac(lfac$( 3)), end_part$(1)         , ch(25),~
                                                                         ~
               at (10,02), "Receiver Number",                            ~
               at (10,22), fac(lfac$( 4)), beg_receiver$(1)     , ch(16),~
               at (10,48), fac(lfac$( 4)), end_receiver$(1)     , ch(16),~
                                                                         ~
               at (11,02), "Received Date",                              ~
               at (11,22), fac(lfac$( 5)), beg_date$            , ch(10),~
               at (11,48), fac(lfac$( 5)), end_date$            , ch(10),~
                                                                         ~
               at (12,02), "Sorting Sequence?",                          ~
               at (12,23), fac(lfac$( 6)), sortby$              , ch(01),~
                                                                         ~
               at (13,02), "Print In Background?",                       ~
               at (13,23), fac(lfac$( 7)), back$                , ch(01),~
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

               if keyhit% <> 13 then L40830
                  call "MANUAL" ("RCVAPRPT")
                  goto L40290

L40830:        if keyhit% <> 15 then L40870
                  call "PRNTSCRN"
                  goto L40290

L40870: REM    IF FIELDNR% > 0% THEN RETURN
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
                  on fieldnr% gosub L50160,         /* Vendor Range     */~
                                    L50210,         /* PO Range         */~
                                    L50250,         /* Part Range       */~
                                    L50300,         /* Receiver Range   */~
                                    L50350,         /* Receive Date Rng */~
                                    L50450,         /* Sorting Sequence */~
                                    L50490          /* Print In Bckgrnd?*/
                  return
L50160:     REM Test Data for Vendor Range
                call "TESTRNGE"   (beg_vendor$(1), end_vendor$(1),       ~
                                   beg_vendor$(2), end_vendor$(2),       ~
                                   errormsg$)
                return
L50210:     REM Test Data for PO Range
                call "TESTRNGE"   (beg_po$(1), end_po$(1), beg_po$(2),   ~
                                   end_po$(2), errormsg$)
                return
L50250:     REM Test Data for Part Range
                call "TESTRNGE"   (beg_part$(1), end_part$(1),           ~
                                   beg_part$(2), end_part$(2),           ~
                                   errormsg$)
                return
L50300:     REM Test Data for Receiver Number Range
                call "TESTRNGE"   (beg_receiver$(1), end_receiver$(1),   ~
                                   beg_receiver$(2), end_receiver$(2),   ~
                                   errormsg$)
                return
L50350:     REM Test Data for Received Date Range
                call "DATEOKC" (beg_date$, bdate%, errormsg$)
                if errormsg$ <> " " then return
                if end_date$ = " " or end_date$ = blankdate$ ~
                                 then end_date$ = beg_date$
                call "DATEOKC" (end_date$, edate%, errormsg$)
                if errormsg$ > " " then return
                if edate% >= bdate% then return
                errormsg$ = "Ending Date Cannot be less than "     &     ~
                            "Beginning Date"
                return
L50450:     REM Test Data for Sorting Sequence
                if sortby$ = "V" or sortby$ = "D"                        ~
                                 or sortby$ = "G" then return
                errormsg$ = "Must be 'D', 'G', or 'V'"
                return
L50490:     REM Test Data for Background?
                if back$ <> "Y" then back$ = "N"
                return

        REM *************************************************************~
            *               R E P O R T   R O U T I N E S               *~
            *-----------------------------------------------------------*~
            * Report printing logic.                                    *~
            *************************************************************

        print_by_vendor
            call "PLOWALTS" (#1, plowkey$, 2%, 0%, f1%(1))
                if f1%(1) = 0% then return
            gosub load_rcvlines
            ok$ = "Y"
            gosub include_test
            if ok$ = "X" then return
            if ok$ <> "Y" then print_by_vendor
            gosub print_report
            goto print_by_vendor

        print_by_sortfile
            call "READNEXT" (#4, f1%(4))
            if f1%(4) = 0% then return
            gosub load_sortfile
            gosub print_report
            goto print_by_sortfile
            return   /* ?! */

        print_report
            if l% < 1% then gosub print_heading
            call "DATE" addr("G-", lastrecdate$, date, days%, ret%)
                if ret% <> 0 then days% = 0
            call "DATEFMT" (duedate$)
            call "DATEFMT" (lastrecdate$)
            call "GLFMT"   (expense_acct$)
            call "CONVERT" (open_qty, 0.2, print$(1))
            call "CONVERT" (ap_price, 2.7, print$(2))
            call "CONVERT" (amt_not_inv, 2.2, print$(3))
            call "STRING" addr ("LJ", line$, 3%)

            if sortby$ <> "G" then L57115
                if last_expense_acct$ <> expense_acct$ then              ~
                                              gosub print_exp_acct_break
L57115:     if last_vendor$ = vendor$ and last_po$ = po$ then            ~
               print using L64290, " ", " ", line$, duedate$, receiver$,  ~
                                  lastrecdate$, part$, print$(1),        ~
                                  print$(2), print$(3), days%
            if last_vendor$ = vendor$ and last_po$ <> po$ then           ~
               print using L64290, " ", po$, line$, duedate$, receiver$,  ~
                                  lastrecdate$, part$, print$(1),        ~
                                  print$(2), print$(3), days%
            if last_vendor$ <> vendor$ then gosub vendor_totals
            if last_vendor$ <> vendor$ then                              ~
               print using L64290, vendor$, po$, line$,duedate$,receiver$,~
                                  lastrecdate$, part$, print$(1),        ~
                                  print$(2), print$(3), days%
            l% = l% - 1%
            last_vendor$ = vendor$
            last_po$ = po$
            last_expense_acct$ = expense_acct$
            ventotal = ventotal + amt_not_inv
            rpt_total = rpt_total + amt_not_inv
            exp_subtotal = exp_subtotal + amt_not_inv
            lines_printed% = lines_printed% + 1
            return

        vendor_totals
            if sortby$ <> "V" then return
            if last_vendor$ = " " then return
            if lines_printed% < 2 then L60080
            call "CONVERT" (ventotal, 2.2, print$(4))
            print using L64330
            print using L64370, last_vendor$, print$(4)
            l% = l% - 2%
L60080:     ventotal, lines_printed% = 0
            if l% < 1% then return
                print
                l% = l% - 1%
                return

        report_totals
            if l% < 7% then gosub print_heading
            call "CONVERT" (rpt_total, 2.2, print1$)
            print
            print using L64250
            print using L64400, print1$
            print skip(2)
            print using L64440
            return

        print_exp_acct_break
            if sortby$ <> "G" then return
            if last_expense_acct$ = " " then L61240

            if lines_printed% < 2 then L61190
            call "CONVERT" (exp_subtotal, 2.2, print$(4))
            print using L64330
            print using L64500, last_expense_acct$, print$(4)
            l% = l% - 2%
L61190:     exp_subtotal, lines_printed% = 0
            if l% < 1% then L61240
                print
                l% = l% - 1%

L61240:     /* Print Expense Account header */
            if end_report% = 1% then return
            print
            print using L64480, expense_acct$
            print using L64530

            lines_printed% = lines_printed% + 2%
            l% = l% - 3%
            return

        print_heading
            print page
            page% = page% + 1%
            print using L64060, date$, rpt_time$, company$
            print_title$ = "UNINVOICED RECEIVERS REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64100, print_title$, page%
            if sortby$ <> "V" then L62075
                 print_title$ = "BY VENDOR"
                 goto L62090
L62075:     if sortby$ = "G" then print_title$ = "BY EXPENSE ACCOUNT"    ~
                             else print_title$ = "BY DATE RECEIVED"
L62090:     call "FMTTITLE" (print_title$, " ", 12%)
            print using L64130, print_title$
            if end$ = "Y" then return
            print
            print using L64180
            print using L64210
            print using L64250
            l% = 49%
            return

        print_params
            end$ = "Y"
            gosub print_heading
            tran(i$(), hex(208c2084208620ac))replacing
            print skip(3)
            print using L63220 : print

            print using L63240
            print using L63260, "Vendor Number", beg_vendor$(1),          ~
                                                           end_vendor$(1)
            print using L63260, "P. O. Number", beg_po$(1), end_po$(1)
            print using L63260, "Part Number", beg_part$(1), end_part$(1)
            print using L63260, "Receiver Number", beg_receiver$(1),      ~
                                                         end_receiver$(1)
            temp1$ = beg_date$ : call "DATFMTC" (temp1$)
            temp2$ = end_date$ : call "DATFMTC" (temp2$)
            print using L63260, "Received Date", temp1$, temp2$
            print using L63260, "Sorting Sequence", sortby$, " "
                temp$ = "N"
                if back$ = "B" or back$ = "Y" then temp$ = "Y"
            print using L63260, "Print In Background", temp$, " "

            print : print using L63280
            end$ = " "
            return

L63220: %                                     ----------------- Report Se~
        ~lection Parameters -----------------
L63240: %                                                           FROM ~
        ~                     TO
L63260: %                                      #################### #####~
        ~#################### #########################
L63280: %                                     ---------------------------~
        ~------------------------------------

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L64060: % ######## ########                  ############################~
        ~################################                    RCVAPRPT:RCV0~
        ~02

L64100: %                                    ############################~
        ~################################                        PAGE:####

L64130: %                                    ############################~
        ~################################

L64180: %                                                             DAT~
        ~E                                                              DA~
        ~YS
L64210: %VENDOR    PURCHASE ORDER & LINE  DATE DUE RECEIVER NUMBER  RECEI~
        ~VED PART NUMBER                 QUANTITY PRICE EACH  EXTENSION OP~
        ~EN

L64250: %========= ================ ====  ======== ================ =====~
        ~=== ========================= ========== ========== ========== ==~
        ~==

L64290: %######### ################ ###   ######## ################ #####~
        ~### ######################### ########## ########## ########## -#~
        ~##

L64330: %--------- ---------------- ----  -------- ---------------- -----~
        ~--- ------------------------- ---------- ---------- ---------- --~
        ~--

L64370: %                                                                ~
        ~    Total For Vendor #########                      ##########

L64400: %                                                                ~
        ~    Report Grand Total                            ############ **~
        ~**

L64440: %                                        * * * * *   E N D   O F ~
        ~  R E P O R T   * * * * *

L64480: %EXPENSE ACCOUNT :  ############

L64500: %                                                                ~
        ~    Total For Expense Account ############          ##########

L64530: %-------------------------------
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
            call "SHOSTAT" ("One Moment Please")
            end
