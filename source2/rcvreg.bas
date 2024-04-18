        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    CCC   V   V  RRRR   EEEEE   GGG                  *~
            *  R   R  C   C  V   V  R   R  E      G                     *~
            *  RRRR   C      V   V  RRRR   EEEE   G GGG                 *~
            *  R   R  C   C   V V   R   R  E      G   G                 *~
            *  R   R   CCC     V    R   R  EEEEE   GGG                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVREG   - Print Receiver register reports.               *~
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
            * 05/22/86 ! Original                                 ! LCJ *~
            * 06/18/86 ! Minor Tweeks                             ! HES *~
            * 05/19/87 ! Minor Tweek (Std Costing Changes)        ! ERN *~
            * 11/01/88 ! End date default repair.                 ! JDH *~
            * 09/20/89 ! Added PLOWCODE to receiver range.        ! JDH *~
            * 01/13/93 ! Page 0 Facs fix, & End of Report Time.   ! RJH *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            beg_date$10,                 /* Beginning Received Date    */~
            beg_receiver$16,             /* Beginning Receiver Number  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$52,                    /* Description var for PLOWCOD*/~
            edtmessage$79,               /* Edit screen message        */~
            end_date$10,                 /* Ending Received Date       */~
            end_receiver$16,             /* Ending Receiver Number     */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Work File Name             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_po$16,                  /* Last P.O.     Processed    */~
            last_receiver$16,            /* Last Receiver Processed    */~
            last_vendor$9,               /* Last Vendor   Processed    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Work File Library          */~
            line2$79,                    /* Second Line of Screen Headr*/~
            ok$1,                        /* OK to Print? Flag          */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_title$60,              /* Report Sorted By Title     */~
            print$(7)10,                 /* Work Variable              */~
            qc$3,                        /* Print QC In Progress Only? */~
            qchold$3,                    /* Print QC Hold Only?        */~
            rec_hold$3,                  /* Print Receiver Hold Only?  */~
            rejects$3,                   /* Print Rejects Only?        */~
            rpt_time$8,                  /* Report Time                */~
            scraps$3,                    /* Print Scrap/Rework Only?   */~
            sort$116,                    /* Sort parameters to SORTCALL*/~
            sortby$1,                    /* Sorting Sequence, R or D   */~
            title$(2)25,                 /* Screen column title        */~
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
            expense_acct$9 ,/* GENERAL LEDGER ACCOUNT NUMBER           */~
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
            * #4  ! SORTFILE ! Temporary Sort / Work File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24

            select #4, "SORTFILE",                                       ~
                        varc,     consec,  recsize =  400

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "GETNAMES" addr(#1, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",records%,ret%)
            call "COMPNAME" (12%, company$, ret%)

            title$(1) = "From"  :  title$(2) = "To  "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)EXIT PROGRAM"
            init(" ") errormsg$, inpmessage$,                            ~
                      beg_receiver$               ,/* Beg Receiver Num */~
                      end_receiver$               ,/* End Receiver Num */~
                      beg_date$                   ,/* Beg Receive Date */~
                      end_date$                   ,/* End Receive Date */~
                      rec_hold$                   ,/* Receiver Hold?   */~
                      rejects$                    ,/* Rejects Only ?   */~
                      scraps$                     ,/* Scrap/ReworK Only*/~
                      qc$                         ,/* QC In Progress ? */~
                      qchold$                     ,/* QC Hold Only?    */~
                      sortby$                      /* Sorting Sequence */

            for fieldnr% = 1 to 8
L10310:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10430
L10330:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10410
L10360:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10330
                         if fieldnr% = 1% then L10310
                         goto L10360
L10410:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10330
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10330
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
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
            oldfield% = 0%
L11140:     fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% >  8 then edtpg1
            if fieldnr% = oldfield% then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11190:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  oldfield% = fieldnr%
                  goto L11140

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if beg_receiver$ = "ALL" then plowkey$ = " "                 ~
                                     else plowkey$ = beg_receiver$
            if beg_date$ <> "ALL" then call "DATUFMTC" (beg_date$)
            call "DATUFMTC" (end_date$)
            if sortby$ = "R" then L19290
            call "SHOSTAT" ("Now Selecting Items to Report")
            counter% = 0%
            records% = max(records%,100%)
            call "WORKOPN2" (#4, "OUTPT", records%, f2%(4))
            gosub selection_process
            close #4
            if counter% > 0% then L19260
L19190:        k% = 2%
               call "ASKUSER" (k%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no Receiver Lines eligible for Printing",       ~
                 "were found using the given selection parameters.",     ~
                 "Press RETURN to change the parameters or exit.")
               call "FILEBGON" (#4)
               if beg_date$ <> "ALL" then call "DATFMTC" (beg_date$)
               call "DATFMTC" (end_date$)
               goto edtpg1
L19260:     call "SHOSTAT" ("Now Sorting Items Selected")
            gosub sort_file
            call "WORKOPN2" (#4, "INPUT", records%, f2%(4))
L19290:     call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("RCV001","        ",records%*4%, 0%)
            rpt_time$ = " "             /* Grab System Time            */
            call "TIME" (rpt_time$)
            select printer (134)
            l% = 0%                     /* Number of Lines Left on Page*/
            page% = -1%                 /* Page Counter (of course)    */
            gosub print_params
            if sortby$ = "R" then gosub print_by_receiver                ~
                             else gosub print_by_date
            if page% = 0% then L19190
            if l% < 1% then gosub print_heading
            print skip(2)
            rpt_time$ = " "  :  call "TIME" (rpt_time$)
            print "                                   ********** E N D   ~
        ~O F   R E P O R T  @ " & rpt_time$ & " ********** "
            close printer
            call "SETPRNT" ("RCV001","        ",records%, 1%)
            call "FILEBGON" (#4)
            goto inputmode

        selection_process
            gosub plow_lines
            return

        plow_lines
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then return
            gosub load_rcvlines
            ok$ = "Y"
            gosub include_test
            if ok$ <> "Y" then plow_lines
            gosub write_sort_record
            goto plow_lines

        include_test
            if beg_receiver$ = "ALL" then L19680
               if receiver$ < beg_receiver$ or receiver$ > end_receiver$ ~
                  then ok$ = "N"
L19680:     if lastrecdate$ < beg_date$ or lastrecdate$ > end_date$      ~
                  then ok$ = "N"
            if rec_hold$ = "YES" and qty_rec_hold = 0 then ok$ = "N"
            if qchold$ = "YES"   and qty_qc_hold  = 0 then ok$ = "N"
            if qc$ = "YES"       and qty_qc       = 0 then ok$ = "N"
            if rejects$ = "YES"  and qty_returned = 0 then ok$ = "N"
            if scraps$ = "YES"   and qty_scrapped = 0 then ok$ = "N"
            return

        sort_file
            call "GETNAMES" addr(#4, file$, lib$, vol$)
            sort$ = file$
            str(sort$,9%,8%) = lib$
            str(sort$,17%,6%) = vol$
            str(sort$,23%,22%) = str(sort$,,22%)
            str(sort$,45%,9%) = "0122006CA"        /* Date Received    */
            str(sort$,54%,9%) = "0026016CA"        /* Receiver  #      */
            str(sort$,63%,9%) = "0042009CA"        /* Vendor    #      */
            str(sort$,72%,9%) = "0051016CA"        /* P.O.      #      */
            str(sort$,81%,9%) = "0067003CA"        /* Line Item #      */
            call "SORTCALL" addr(sort$, ret%)
            if ret% = 0% then return
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
                  on fieldnr% gosub L20100,         /* Recvr Number Rng */~
                                    L20300,         /* Rcvd Date Range  */~
                                    L20500,         /* Receiver Hold?   */~
                                    L20600,         /* Rejects Only ?   */~
                                    L20700,         /* Scrap/ReworK Only*/~
                                    L20800,         /* QC In Progress ? */~
                                    L20900,         /* QC Hold Only?    */~
                                    L20991          /* Sorting Sequence */
                     return
L20100:     REM Default/Enable for Receiver Number Range
                inpmessage$ = "Enter the Range for Receiver # To Print " ~
                           &  "or 'ALL' to Print all Receivers"
                if beg_receiver$ = " " then beg_receiver$ = "ALL"
                if beg_receiver$ = "ALL" then end_receiver$ = "   "
                return
L20300:     REM Default/Enable for Received Date Range
                inpmessage$ = "Enter the Receive Date Range to Print"
                if beg_date$ = " " or beg_date$ = blankdate$ then ~
                   beg_date$ = "19000101"
                   call "DATFMTC" (beg_date$)
                if end_date$ <> " " and end_date$ <> blankdate$ then return
                   end_date$="20991231"
                   call "DATFMTC" (end_date$)
                return
L20500:     REM Default/Enable for Print Receiver Hold Only?
                if rec_hold$ = " " then enabled% = 0%
                if rec_hold$ = " " then rec_hold$ = "NO "
                inpmessage$ = "Enter YES to print only those Receiver " &~
                              "Lines with qty's in Receiver Hold"
                return
L20600:     REM Default/Enable for Print Rejects Only?
                if rejects$ = " " then enabled% = 0%
                if rejects$ = " " then rejects$ = "NO "
                inpmessage$ = "Enter YES to print only those Receiver " &~
                              "Lines with Reject qty's"
                return
L20700:     REM Default/Enable for Print Scrap/Rework Only?
                if scraps$ = " " then enabled% = 0%
                if scraps$ = " " then scraps$ = "NO "
                inpmessage$ = "Enter YES to print only those Receiver " &~
                              "Lines with Scrap/Rework qty's"
                return
L20800:     REM Default/Enable for Print QC In Progress Only?
                if qc$ = " " then enabled% = 0%
                if qc$ = " " then qc$ = "NO "
                inpmessage$ = "Enter YES to print only those Receiver " &~
                              "Lines with qty's in Q.C."
                return
L20900:     REM Default/Enable for Print QC Hold Only?
                if qchold$ = " " then enabled% = 0%
                if qchold$ = " " then qchold$ = "NO "
                inpmessage$ = "Enter YES to print only those Receiver " &~
                              "Lines with qty's in Q.C. Hold"
                return
L20991:     REM Default/Enable for Sorting Sequence
                if sortby$ = " " then sortby$ = "R"
                inpmessage$ = "Enter 'R' to print by Receiver Number  " &~
                              "or 'D' to Print by Date Received."
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
            u3% = 0%
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
            material       ,/* Material price per unit                 */~
            labor          ,/* Labor price per unit                    */~
            overhead       ,/* Overhead price  per unit                */~
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
            mtl            ,/* material cost from A/P                  */~
            lbr            ,/* labor cost    from A/P                  */~
            ovh            ,/* overhead cost from A/P                  */~
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
            material       ,/* Material price per unit                 */~
            labor          ,/* Labor price per unit                    */~
            overhead       ,/* Overhead price  per unit                */~
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
            mtl            ,/* material cost from A/P                  */~
            lbr            ,/* labor cost    from A/P                  */~
            ovh            ,/* overhead cost from A/P                  */~
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
            material       ,/* Material price per unit                 */~
            labor          ,/* Labor price per unit                    */~
            overhead       ,/* Overhead price  per unit                */~
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
            mtl            ,/* material cost from A/P                  */~
            lbr            ,/* labor cost    from A/P                  */~
            ovh            ,/* overhead cost from A/P                  */~
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
            PD(14,7),       /* Material cost per unit                  */~
            PD(14,7),       /* Labor cost per unit                     */~
            PD(14,7),       /* Overhead per unit                       */~
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
            PD(14,7),       /* material price A/P                      */~
            PD(14,7),       /* labor price A/P                         */~
            PD(14,7),       /* overhead price A/P                      */~
            CH(4),          /* Line level Receiver Text                */~
            CH(4),          /* Q.C. Line Item Text                     */~
            CH(5)           /* Filler                                  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,64%) = "RCVREG: " & str(cms2v$,,8%)
                  str(line2$,,50) = "Report Selection Criteria"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40260,         /* Recvr Number Rng */~
                                    L40260,         /* Recvd Date Range */~
                                    L40260,         /* Receiver Hold?   */~
                                    L40260,         /* Rejects Only ?   */~
                                    L40260,         /* Scrap/ReworK Only*/~
                                    L40260,         /* QC In Progress ? */~
                                    L40260,         /* QC Hold Only?    */~
                                    L40260          /* Sorting Sequence */

                  goto L40320

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40260:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40320:
L40330:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Receiver Register Report(s)",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,30), fac(hex(ac)), title$(1)              , ch(17),~
               at (06,48), fac(hex(ac)), title$(2)              , ch(17),~
               at (07,02),                                               ~
                  "Receiver Number",                                     ~
               at (07,30), fac(lfac$( 1)), beg_receiver$        , ch(16),~
               at (07,48), fac(lfac$( 1)), end_receiver$        , ch(16),~
               at (08,02),                                               ~
                  "Received Date",                                       ~
               at (08,30), fac(lfac$( 2)), beg_date$            , ch(10),~
               at (08,48), fac(lfac$( 2)), end_date$            , ch(10),~
               at (09,02),                                               ~
                  "Print Receiver Hold Only?",                           ~
               at (09,30), fac(lfac$( 3)), rec_hold$            , ch(03),~
               at (10,02),                                               ~
                  "Print Rejects Only?",                                 ~
               at (10,30), fac(lfac$( 4)), rejects$             , ch(03),~
               at (11,02),                                               ~
                  "Print Scrap/Rework Only?",                            ~
               at (11,30), fac(lfac$( 5)), scraps$              , ch(03),~
               at (12,02),                                               ~
                  "Print QC In Progress Only?",                          ~
               at (12,30), fac(lfac$( 6)), qc$                  , ch(03),~
               at (13,02),                                               ~
                  "Print QC Hold Only?",                                 ~
               at (13,30), fac(lfac$( 7)), qchold$              , ch(03),~
               at (14,02),                                               ~
                  "Sorting Sequence?",                                   ~
               at (14,30), fac(lfac$( 8)), sortby$              , ch(01),~
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

               if keyhit% <> 13 then L40850
                  call "MANUAL" ("RCVREG  ")
                  goto L40330

L40850:        if keyhit% <> 15 then L40890
                  call "PRNTSCRN"
                  goto L40330

L40890: REM    IF FIELDNR% > 0% THEN RETURN
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
                  on fieldnr% gosub L50100,         /* Recvr Number Rng */~
                                    L50300,         /* Recvd Date Range */~
                                    L50500,         /* Receiver Hold?   */~
                                    L50600,         /* Rejects Only ?   */~
                                    L50700,         /* Scrap/ReworK Only*/~
                                    L50800,         /* QC In Progress ? */~
                                    L50900,         /* QC Hold Only?    */~
                                    L51000          /* Sorting Sequence */
                  return
L50100:     REM Test Data for Receiver Number Range
                if beg_receiver$ = "ALL" then end_receiver$ = " "
                if beg_receiver$ = "ALL" then return
                plowkey$ = xor plowkey$ : plowkey$ = beg_receiver$
                descr$ = hex(06) & "Choose Beginning Receiver Number or"&~
                                   " Press PF16"
                call "PLOWCODE" (#1, plowkey$, descr$, -16%,-.001, f1%(1))
                if f1%(1) = 1% then beg_receiver$ = str(plowkey$,,16)
                if beg_receiver$ > "   " then L50200
                errormsg$ = "Beginning Receiver Number Cannot be blank!"
                return
L50200:         if end_receiver$ = " " then end_receiver$ = beg_receiver$
                plowkey$ = xor plowkey$ : plowkey$ = end_receiver$
                descr$ = hex(06) & "Choose Ending Receiver Number or" &  ~
                                   " Press PF16"
                call "PLOWCODE" (#1, plowkey$, descr$, -16%,-.001, f1%(1))
                if f1%(1) = 1% then end_receiver$ = str(plowkey$,,16)
                if end_receiver$ >= beg_receiver$ then return
                errormsg$ = "Ending Receiver # Cannot be less than " &   ~
                            "Beginning Receiver #"
                return
L50300:     REM Test Data for Received Date Range
                call "DATEOKC" (beg_date$, bdate%, errormsg$)
                if errormsg$ <> " " then return
                if end_date$ = " " or end_date$ = blankdate$ then ~
                   end_date$ = beg_date$
                call "DATEOKC" (end_date$, edate%, errormsg$)
                if errormsg$ > " " then return
                if edate% >= bdate% then return
                errormsg$ = "Ending Date Cannot be less than "     &     ~
                            "Beginning Date"
                return
L50500:     REM Test Data for Print Receiver Hold Only?
                if rec_hold$ = "YES" or rec_hold$ = "NO" then return
                errormsg$ = "Must be YES or NO"
                return
L50600:     REM Test Data for Print Rejects Only?
                if rejects$ = "YES" or rejects$ = "NO" then return
                errormsg$ = "Must be YES or NO"
                return
L50700:     REM Test Data for Print Scrap/Rework Only?
                if scraps$ = "YES" or scraps$ = "NO" then return
                errormsg$ = "Must be YES or NO"
                return
L50800:     REM Test Data for Print QC In Progress Only?
                if qc$ = "YES" or qc$ = "NO" then return
                errormsg$ = "Must be YES or NO"
                return
L50900:     REM Test Data for Print QC Hold Only?
                if qchold$ = "YES" or qchold$ = "NO" then return
                errormsg$ = "Must be YES or NO"
                return
L51000:     REM Test Data for Sorting Sequence
                if sortby$ = "R" or sortby$ = "D" then return
                errormsg$ = "Must be 'R' or 'D'"
                return

        print_by_receiver
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then return
            gosub load_rcvlines
            ok$ = "Y"
            gosub include_test
            if ok$ <> "Y" then print_by_receiver
            gosub print_report
            goto print_by_receiver

        print_by_date
            call "READNEXT" (#4, f1%(4))
            if f1%(4) = 0% then return
            gosub load_sortfile
            gosub print_report
            goto print_by_date
            return   /* ?! */

        print_report
            if l% < 1% then gosub print_heading
            if receiver$ <> last_receiver$ then gosub print_receiver_hdng
            call "CONVERT" (qty_recvd   , 0.2, print$(1))
            call "CONVERT" (qty_rec_hold, 0.2, print$(2))
            call "CONVERT" (qty_to_hny  , 0.2, print$(3))
            call "CONVERT" (qty_returned, 0.2, str(print$(4),,9))
            call "CONVERT" (qty_scrapped, 0.2, str(print$(5),,9))
            call "CONVERT" (qty_qc      , 0.2, str(print$(6),,9))
            call "CONVERT" (qty_qc_hold , 0.2, str(print$(7),,9))
            call "STRING" addr ("LJ", line$, 3%)
            if last_vendor$ = vendor$ and last_po$ = po$ then            ~
               print using L64290, " ", " ", line$, part$, print$(1),     ~
                                  print$(2), print$(3), print$(4),       ~
                                  print$(5), print$(6), print$(7)
            if last_vendor$ = vendor$ and last_po$ <> po$ then           ~
               print using L64290, " ", po$, line$, part$, print$(1),     ~
                                  print$(2), print$(3), print$(4),       ~
                                  print$(5), print$(6), print$(7)
            if last_vendor$ <> vendor$ then                              ~
               print using L64290, vendor$, po$, line$, part$, print$(1), ~
                                  print$(2), print$(3), print$(4),       ~
                                  print$(5), print$(6), print$(7)
            l% = l% - 1%
            last_vendor$ = vendor$
            last_po$ = po$
            return

        print_receiver_hdng
            if l% < 4% then gosub print_heading
            last_receiver$ = receiver$
            last_vendor$ = " "
            last_po$ = " "
            if len(lastrecdate$) < 8% then call "DATEFMT" (lastrecdate$)
            print
            if sortby$ = "R" then                                        ~
                       print using L64160, receiver$, lastrecdate$
            if sortby$ = "D" then                                        ~
                       print using L64170, lastrecdate$, receiver$
            l% = l% - 2%
            return

        print_heading
            print page
            page% = page% + 1%
            print using L64060, date$, rpt_time$, company$
            print_title$ = "RECEIVER REGISTER REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64100, print_title$, page%
            if sortby$ = "R" then print_title$ = "BY RECEIVER NUMBER"    ~
                             else print_title$ = "BY DATE RECEIVED"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64130, print_title$
            if end$ = "Y" then return
            l% = 57%
            print skip(1)
            print using L64180
            print using L64210
            print using L64250
            l% = l% - 4%
            return

        print_params
            end$ = "Y"
            gosub print_heading
            end$ = " "
L63013:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L63021
                str(i$(), i%, 1%) = hex(20)
                goto L63013
L63021:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 6% to 17%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L64060: % ######## ########                  ############################~
        ~################################                      RCVREG:RCV0~
        ~01

L64100: %                                    ############################~
        ~################################                        PAGE:####

L64130: %                                    ############################~
        ~################################

L64160: %***** RECEIVER: ################ DATE RECEIVED: ######## *****
L64170: %***** DATE RECEIVED: ########    RECEIVER: ################ *****
L64180: %                                                            TOTA~
        ~L     .........................DISTRIBUTION......................~
        ~..
L64210: %VENDOR    PURCHASE ORDER & LINE  PART NUMBER                RECE~
        ~IVED  RCVER-HOLD      STOCK    REJECT  SCRP/RWK     IN QC   QC-HO~
        ~LD

L64250: %--------- ---------------- ----  ------------------------- -----~
        ~----- ---------- ---------- --------- --------- --------- -------~
        ~--

L64290: %######### ################ ###   ######################### #####~
        ~##### ########## ########## ######### ######### ######### #######~
        ~##

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
