        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  BBBB   K   K  H   H   SSS   TTTTT  RRRR   Y   Y   *~
            *  V   V  B   B  K  K   H   H  S        T    R   R  Y   Y   *~
            *  V   V  BBBB   KKK    HHHHH   SSS     T    RRRR    YYY    *~
            *   V V   B   B  K  K   H   H      S    T    R   R    Y     *~
            *    V    BBBB   K   K  H   H   SSS     T    R   R    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKHSTRY - Prints P.O. Receipts History Report.           *~
            *            Report may be sorted three ways;               *~
            *              by Purchase Order;                           *~
            *              by Vendor, Purchase Order;                   *~
            *              by Order Date, Purchase Order.               *~
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
            * 05/29/86 ! Original                                 ! LDJ *~
            * 03/29/88 ! Correct Leadtime extraction logic        ! HES *~
            * 02/02/89 ! Fixed PLOWKEY$ to pick up first record   ! MJB *~
            *          !  in range selections.                    !     *~
            * 05/08/91 ! Added option for history files.          ! JDH *~
            * 03/30/92 ! Minor mods for DEC Compatability.        ! JDH *~
            * 01/13/93 ! Page 0 Facs fix                          ! RJH *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            actlt$7,                     /* Actual (Computed) Lead Time*/~
            beg_date$10,                 /* Beginning Order Date       */~
            beg_po$16,                   /* Beginning P.O. Number      */~
            beg_vendor$9,                /* Beginning Vendor Code      */~
            blankdate$8,                 /* blank unfmt date           */~
            column$(2)30,                /* Range Selection Headers    */~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            due_date$8,                  /* P.O. Line Item Due Date    */~
            edtmessage$79,               /* Edit screen message        */~
            end_date$10,                 /* Ending Order Date          */~
            end_po$16,                   /* Ending P.O. Number         */~
            end_vendor$9,                /* Ending Vendor Code         */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Work File Name             */~
            filekey$99,                  /* Miscellaneous Read/Plow Key*/~
            from$1,                      /* Where STD LeadTime From    */~
            hist_msg$7,                  /* History or Current Message */~
            history$1,                   /* Use history files?         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            item$3,                      /* P.O. Line Item Number      */~
            last_po$16,                  /* Last P.O.     Processed    */~
            last_vendor$9,               /* Last Vendor   Processed    */~
            ldtime$(490)3,               /* Part Lead Times - ENGMASTR */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Work File Library          */~
            line2$79,                    /* Second Line of Screen Headr*/~
            ok$1,                        /* OK to Print? Flag          */~
            order_date$8,                /* P.O. Order Date            */~
            order_date2$8,               /* P.O. Order Date            */~
            p%(2),                       /* Search Receiver Array      */~
            pastdue$2,                   /* Past Due Flag              */~
            past_due$3,                  /* Print Past Due Only ?      */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_title$60,              /* Report Sorted By Title     */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpt_time$8,                  /* Report Time                */~
            sort$116,                    /* Sort parameters to SORTCALL*/~
            sortby$,                     /* Sort Sequence              */~
            stdlt$10,                    /* Standard Lead-Time         */~
            testlt$3,                    /* Test Lead Time (VENPRICE)  */~
            userid$3,                    /* Current User Id            */~
            venpart$25,                  /* Vendor Part Number         */~
            vol$6,                       /* Work File Volume           */~
            yymmdd$(490)6                /* Production calendar        */

        dim                 /* FILE: RCVLINES Variables                */~
            part$25        ,/* Part code                               */~
            receiver$16    ,/* Receiver Control Number                 */~
            vendor$9       ,/* Vendor code                             */~
            po$16          ,/* Purchase Order Number                   */~
            seq$3          ,/* Purchase Line Sequence Number (not ITEM */~
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
            * #1  ! VBKMASTR ! Purchase Order Headers Master File       *~
            * #2  ! VBKLINES ! Purchase Orders Line Item Master file    *~
            * #3  ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #4  ! SORTFILE ! Temporary Sort / Work File               *~
            * #5  ! HNYMASTR ! Inventory Master File                    *~
            * #6  ! ENGMASTR ! Engineering Master File                  *~
            * #7  ! CALMASTR ! Planning Production Calendar File        *~
            * #8  ! VENPRICE ! Vendor Prices / Leadtimes File           *~
            * #11 ! VBKHMSTR ! Purchase Order Headers History File      *~
            * #12 ! VBKHLNES ! Purchase Orders Line Item History file   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #2,  "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #3,  "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24

            select #4, "SORTFILE",                                       ~
                        varc,     consec,  recsize =   22

            select #5,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #6,  "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29

            select #7 , "CALMASTR",                                      ~
                        varc,     indexed,  recsize = 1962,              ~
                        keypos =    1, keylen =   2

            select #8 , "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34

            select #11, "VBKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #12, "VBKHLNES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~


            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
                rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))

            if min(fs%()) < 0% then exit_program

            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUFMTC" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            column$(1)="From " : column$(2)="To "
            call "GETNAMES" addr(#2%, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",records_cur%,ret%)
            call "GETNAMES" addr(#12%, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",records_hst%,ret%)
            call "COMPNAME" (12%, company$, ret%)
            gosub load_calendar

            mc% = 1% : lc% = 2%     /* Prime channels as current */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)EXIT PROGRAM"
            init(" ") errormsg$, inpmessage$,                            ~
                      beg_po$                     ,/* Beg P.O. Number  */~
                      end_po$                     ,/* End P.O. Number  */~
                      beg_vendor$                 ,/* Beginning Vendor */~
                      end_vendor$                 ,/* Ending Vendor    */~
                      beg_date$                   ,/* Beg Order Date   */~
                      end_date$                   ,/* Ending Order Date*/~
                      past_due$                   ,/* Past Due Only?   */~
                      sortby$                     ,/* Sort Sequence    */~
                      history$                     /* Use History Files*/

            for fieldnr% = 1 to 6
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
L11160:     fieldnr% = cursor%(1) - 6%
            if fieldnr% < 1 or fieldnr% >  6 then edtpg1
            if fieldnr% = oldfield% then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11260:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                  oldfield% = fieldnr%
            goto L11160

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if sortby$ = "1" then plowkey$=beg_po$
            if sortby$ = "1" then key% = 1% else key% = 0%
            if sortby$ = "2" then plowkey$=beg_vendor$
            if sortby$ = "3" then plowkey$ = " "
            if beg_vendor$ = "ALL" and sortby$ = "2" then plowkey$ = " "
            if beg_po$ = "ALL" and sortby$ = "1" then plowkey$ = " "
            if sortby$ = "3" and beg_vendor$ <> "ALL" then               ~
                          plowkey$=beg_vendor$
            if beg_date$ <> "ALL" then call "DATUFMTC" (beg_date$)
            call "DATUFMTC" (end_date$)
            mc% = 1% : lc% = 2% : hist_msg$ = "Current"
            records% = records_cur%
            if history$ = "N" then L19058
                mc% = 11% : lc% = 12% : hist_msg$ = "History"
                records% = records_hst%
L19058:     if sortby$ <> "3" then L19155         /* No Sort Needed   */
            call "SHOSTAT" ("Now Selecting Items to Report")
            counter% = 0%
            records% = max(records%,500%)
            call "WORKOPN2" (#4, "OUTPT", records%, f2%(4))
            gosub selection_process
            close #4
            if counter% > 0% then L19140
L19095:        k% = 2%
               call "ASKUSER" (k%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no Receiver Lines eligible for Printing",       ~
                 "were found using the given selection parameters.",     ~
                 "Press RETURN to change the parameters or exit.")
               call "FILEBGON" (#4)
               if beg_date$ <> "ALL" then call "DATFMTC" (beg_date$)
               call "DATFMTC" (end_date$)
               goto edtpg1
L19140:     call "SHOSTAT" ("Now Sorting Items Selected")
            gosub sort_file
            call "WORKOPN2" (#4, "INPUT", records%, f2%(4))
L19155:     call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("VBK006","        ",records%*4%, 0%)
            rpt_time$ = " "             /* Grab System Time            */
            call "TIME" (rpt_time$)
            select printer (134)
            page% = -1% : l% = 0%
            gosub print_params
            l% = 0%                     /* Number of Lines Left on Page*/
            page% = 0%                  /* Page Counter (of course)    */
            if sortby$ <> "3" then gosub print_from_vbkmastr             ~
                                else gosub print_from_sortfile
            if page% = 0% then L19095
            if l% < 1% then gosub print_heading
            print
            print tab(50); "***** E N D   O F   R E P O R T ****"
            close printer
            call "SETPRNT" ("VBK006","        ",records%, 1%)
            call "FILEBGON" (#4)
            goto inputmode

        selection_process
            str(plowkey$,,25) = addc all(hex(ff))
L19280:     call "PLOWNEXT" (#mc%, plowkey$, 0%, f1%(mc%))
            if f1%(mc%) = 0% then return
            gosub load_vbkmastr
            ok$ = "Y"
            gosub include_test
            if ok$ <> "Y" then L19280
            gosub write_sort_record
            goto L19280

        include_test
            if beg_po$ = "ALL" then L19345
               if po$ < beg_po$ or po$ > end_po$ then ok$ = "N"
L19345:     /* need to unfmt begin and end dates for order_date check */
            call "DATUFMTC" (beg_date$)
            call "DATUFMTC" (end_date$)
            if order_date$ < beg_date$ or order_date$ > end_date$        ~
                  then ok$ = "N"
            call "DATFMTC" (beg_date$)
            call "DATFMTC" (end_date$)
            if beg_vendor$ = "ALL" then return
            if vendor$ < beg_vendor$ or vendor$ > end_vendor$            ~
                  then ok$ = "N"
            return

        sort_file
            call "GETNAMES" addr(#4, file$, lib$, vol$)
            sort$ = file$
            str(sort$,9%,8%) = lib$
            str(sort$,17%,6%) = vol$
            str(sort$,23%,22%) = str(sort$,,22%)
            str(sort$,45%,9%) = "0001006CA"        /* Date Ordered     */
            str(sort$,54%,9%) = "0007016CA"        /* P.O.      #      */
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
            GOTO INPUTMODE

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  inpmessage$ = " "
                  on fieldnr% gosub L20100,         /* P.O. Range       */~
                                    L20300,         /* Vendor Range     */~
                                    L20500,         /* Date Range       */~
                                    L20700,         /* Past Due Only?   */~
                                    L20750,         /* Sort Sequence    */~
                                    L20820          /* History Files?   */
                     return
L20100:     REM Default/Enable for P.O. Number Range
                inpmessage$ = "Enter the P.O. Range To Print or "        ~
                            & "'ALL' to Print all Purchase Orders"
                if beg_po$ = " " then beg_po$ = "ALL"
                if beg_po$ = "ALL" then end_po$ = " "
                return
L20300:     REM Default/Enable for Vendor Code Range
                inpmessage$ = "Enter Vendor Range To Print or "          ~
                            & "'ALL' to Include All Vendors"
                if beg_vendor$ = " " then beg_vendor$ = "ALL"
                if beg_vendor$ = "ALL" then end_vendor$ = " "
                return
L20500:     REM Default/Enable for Order Date Range
                inpmessage$ = "Enter the Order Date Range to Print "
                if beg_date$ = " " or beg_date$ = blankdate$ then ~
                   beg_date$ = "19010101"
                if beg_date$ = "19010101" then call "DATEOKC" (beg_date$, bdate%, errormsg$)
                end_date$ = date
                call "DATFMTC" (end_date$)
                return
L20700:     REM Default/Enable for Past Due Only?
                inpmessage$ = "Enter YES to Print Only Past Due P.O. " & ~
                              "Line Items or NO for Full Report"
                if past_due$ = " " then past_due$ = "NO"
                return
L20750:     REM Default/Enable for Sort Sequence
                inpmessage$ = "Enter a '1' to Sort by P.O. Number, a '2' ~
        ~by Vendor or a '3' by Order Date"
                if sortby$ > " " then return
                sortby$ = "1"
                return

L20820:     REM Default/Enable for Use History Files?
                inpmessage$ = "Report against History Files? (Y/N)"
                if history$ = " " then history$ = "N"
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
        load_vbkmastr
            get #mc% using L35030,  /* FILE: VBKMASTR                   */~
               vendor$     ,/* Vendor Code                             */~
               po$         ,/* Purchase Order Number                   */~
               order_date$  /* Date Ordered (Purchase Order Date)      */

            today% = 0%
            search yymmdd$() = str(order_date$,,6%) to p%() step 6
            if p%(1) = 0% then return /* Not in Calendar or No Calendar*/
            today% = p%(1)/6% + 1%
            return

        load_sortfile
            get #4 using L31991, /* FILE: SORTFILE                      */~
               order_date$,     /* P.O. Order Date                     */~
               po$              /* Purchase Order Number               */
            return

        load_rcvlines
            get #3 using L35860, /* FILE: RCVLINES                      */~
            part$          ,/* Part code                               */~
            receiver$      ,/* Receiver Control Number                 */~
            vendor$        ,/* Vendor code                             */~
            po$            ,/* Purchase Order Number                   */~
            seq$           ,/* Purchase Line Sequence Number (not ITEM */~
            sdate%         ,/* System Date                             */~
            stime%         ,/* The System Time when a transaction was e*/~
            partdescr$     ,/* part number description                 */~
            podate$        ,/* Date Ordered (Purchase Order Date)      */~
            duedate$       ,/* P.O. Line Item Due Date                 */~
            lastrecdate$   ,/* P.O. Line Item Date of Last Receipt -Las*/~
            packslip$      ,/* Packing Slip # for last quantity receive*/~
            sdate%         ,/* System Date                             */~
            stime%         ,/* System Time                             */~
            textid3$       ,/* INTERNAL ID TO TEXT IN TXTFILE.         */~
            qty_recvd      ,/* Total Quantity Received To Date - Purcha*/~
            qty_rec_hold   ,/* Total Quantity currently in Receiving Ho*/~
            qty_qc         ,/* Quantity currently in QC (Pending)-Purch*/~
            qty_qc_hold    ,/* Total Quantity currently in QC Hold (P.O*/~
            qty_scrapped   ,/* Total quantity rejected /  scrapped.    */~
            qty_returned   ,/* Total Quantity Returned to Vendor.      */~
            qty_to_hny     ,/* Total quantity moved to on-hand inventor*/~
            material       ,/* Material cost per unit                  */~
            labor          ,/* Labor cost per unit                     */~
            overhead       ,/* Overhead per unit                       */~
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
            apr            ,/* material cost from A/P                  */~
            textid1$       ,/* Line Level Receiver Text ID             */~
            textid2$        /* Q.C.Line Item Text ID                   */

            REM *** Calculate Actual Lead Time for Report ***
            call "DATE" addr("G-", str(order_date$,,6%),                 ~
                                   str(lastrecdate$,,6%), actlt%, ret%)
            convert actlt% to actlt$, pic(-######)
            call "DATEFMT" (lastrecdate$)
            return
            sdate%, stime%, qty_rec_hold, qty_qc, qty_qc_hold,           ~
              qty_scrapped, qty_returned, qty_to_hny, material, labor,   ~
              overhead, conv_factor, extension, amt_not_inv,amt_invoiced,~
              ap_adj_amt, scrap_adj_amt, rework_adj_amt, open_qty,       ~
              apr = 0

        load_vbklines
            get #lc% using L35415,  /* FILE: VBKLINES                   */~
               item$       ,/* vbklines item number                    */~
               part$       ,/* Part Number                             */~
            order_qty      ,/* quantity originally ordered             */~
            qty_received   ,/* Total Quantity Received To Date - Purcha*/~
            qty_remaining  ,/* quantity outstanding on order           */~
            due_date$      ,/* P.O. Line Item Due Date                 */~
            venpart$        /* Vendors Part Number for purchase order. */

            REM *** Try For Standard Part Lead Time ***
            stdlt$, from$, pastdue$ = " "
            call "READ100" (#5, part$, f1%(5))
            if f1%(5) = 0% then L30515    /* Try For Effective LeadTime */
            get #5 using L30505, stdlt$
L30505:     FMT POS(170), CH(10)
            from$ = "S"

L30515:     REM *** Try For Effective Date Lead Time ***
            if f1%(7) = 0% then L30570    /* No Calendar                */
            if today% = 0% then L30570    /* Order Date Not In Calendar */
            call "READ100" (#6, str(part$,,25%) & "2001", f1%(6))
            if f1%(6) = 0% then L30570    /* No Planned LeadTimes       */
            get #6, using L30545, ldtime$()
L30545:     FMT XX(29), 490*CH(3)

            stdlt$ = ldtime$(mod(today%-pltbase%,365%)+1%)
            from$ = "E"

L30570:     REM *** Try For Vendor's LeadTime (Most Preferred) ***
            readkey$ = str(part$) & str(vendor$) & venpart$
            call "READ100" (#8, readkey$, f1%(8))
            if f1%(8) = 0% then L30600
               get #8 using L30590, testlt$
               if testlt$ = " " then L30600
                  stdlt$ = testlt$
L30590:           FMT POS(83), CH(3)
                  from$ = "V"

L30600:     REM *** Now Convert Standard Lead Time to Standard Format ***
            stdlt = 0
            call "NUMTEST" (stdlt$, 0, 9999, errormsg$, 0, stdlt)
            if errormsg$=" " then call "CONVERT" (stdlt,0,str(stdlt$,,7))
            str(stdlt$,,3%) = "(" & from$ & ")"

            REM *** Now Set Past Due if Appropriate ***
            if qty_remaining > 0 and due_date$ < date then pastdue$ ="**"
            call "DATEFMT" (due_date$)      /* P.O. Line item due date */
            return

        rem**************************************************************~
            *          l o a d   c a l m a s t r    d a t a             *~
            *-----------------------------------------------------------*~
            * loads and formats the production calendar.                *~
            *************************************************************
        load_calendar
            today% = 0
            call "READ100" (#7 ,  "10", f1%(7))
                if f1%(7) = 0 then return
            get #7 , using L30695, str(yymmdd$(),1,1470)
L30695:         FMT XX(2), CH(1470)

            call "READ100" (#7 ,  "11", f1%(7))
                if f1%(7) = 0 then bad_calendar
            get #7 , using L30720, str(yymmdd$(),1471,1470)
L30720:         FMT XX(2), CH(1470)

           search str(yymmdd$(),4) = hex(0301) to cursor%() step 6
           if cursor%(1%) = 0% then bad_calendar
           pltbase%=(cursor%(1%)+5%)/6%
           return

        bad_calendar
            k% = 0%
            call "ASKUSER" (k%, "***INVALID PLANNING CALENDAR***",       ~
            "Your Planning/Production Calendar is Missing or Invalid!",  ~
            "Please Press RETURN to Acknowlege this message & EXIT,",    ~
            "Then run the Procedure PROCCAL to correct the situation!")
            goto exit_program

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        write_sort_record
            put #4 using L31991, /* FILE: SORTFILE                      */~
               order_date$,     /* P.O. Order Date                     */~
               po$              /* Purchase Order Number               */
            write #4
            counter% = counter% + 1%
            return
L31991:     FMT CH(6), CH(16)

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: VBKMASTR                          */~
            CH(9),          /* Vendor Code                             */~
            CH(16),         /* Purchase Order Number                   */~
            XX(30),         /* vendor name                             */~
            XX(150) ,       /* Company Address                         */~
            XX(20),         /* contact's name                          */~
            XX(9),          /* Payables Account                        */~
            XX(16),         /* last invoice number                     */~
            XX(200),        /* variable fields                         */~
            CH(6),          /* Date Ordered (Purchase Order Date)      */~
            CH(6),          /* cancellation date                       */~
            CH(8),          /* Job Number                              */~
            CH(6),          /* P.O. Line Item Due Date                 */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(6),          /* receiving date                          */~
            CH(3),          /* Puchase Agent ID                        */~
            CH(1),          /* P.O. Conformation Flag (Y/N)            */~
            CH(1),          /* Frieght Terms                           */~
            CH(30),         /* F.O.B.                                  */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(30),         /* ship via free text field                */~
            CH(4),          /* Vendor Type                             */~
            CH(6),          /* Vendor Buy From Code                    */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(2),          /* Number of Times a Field or Record has be*/~
            CH(6),          /* Date something changed                  */~
            CH(10),         /* Maximum dollar amount allowed           */~
            CH(6),          /* Date 'something' expires                */~
            CH(3),          /* Purchase Line Sequence Number (not ITEM */~
            CH(6),          /* Date a transaction was entered          */~
            CH(3),          /* user-id of specific user                */~
            CH(6),          /* Date something changed                  */~
            CH(3),          /* user-id of specific user                */~
            PD(14,4),       /* total order amount (orig.)              */~
            PD(14,4),       /* current order amount                    */~
            CH(10),         /* User Name for a Text Copy Element       */~
            6*CH(30),       /* Ship To Name and Address                */~
            CH(214)         /* Filler For Rest of Record or Internal Sp*/~

L35415: FMT                 /* FILE: VBKLINES                          */~
            XX(9),          /* Vendor Code                             */~
            XX(16),         /* Purchase Order Number                   */~
            XX(3),          /* Purchase Line Sequence Number (not ITEM */~
            CH(3),          /* vbklines item number                    */~
            CH(25),         /* Part Number                             */~
            XX(32),         /* part number description                 */~
            XX(4),          /* category code                           */~
            PD(14,4),       /* quantity originally ordered             */~
            PD(14,4),       /* Total Quantity Received To Date - Purcha*/~
            PD(14,4),       /* quantity outstanding on order           */~
            XX(8),          /* Unit Price                              */~
            XX(8),          /* Extension Amount (Quantity * Price)     */~
            XX(9),          /* Purchases Account Number                */~
            CH(6),          /* P.O. Line Item Due Date                 */~
            XX(6),          /* P.O. Line Item Date of Last Receipt -Las*/~
            XX(6),          /* date of receipt of next shipment        */~
            XX(6),          /* Lot Number                              */~
            XX(8),          /* Job Number                              */~
            XX(3),          /* Store or Warehouse Code                 */~
            XX(1),          /* on-hand posting option                  */~
            XX(3),          /* Type - used generically for special desc*/~
            XX(16),         /* Packing Slip # for last quantity receive*/~
            CH(25),         /* Vendors Part Number for purchase order. */~
            CH(66),         /* filler for rest of record or internal sp*/~
            CH(04),         /* Unit of Measure                         */~
            PD(14,4),       /* amount of units                         */~
            CH(6),          /* Original Due Date                       */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* General purpose status indicator        */~
            CH(2),          /* Number of Times a Field or Record has be*/~
            CH(20),         /* Who is to receive merchandise.          */~
            CH(20),         /* Person requesting merchandise.          */~
            CH(5),          /* User assigned reference number          */~
            CH(6),          /* Do not receive before date              */~
            CH(6),          /* Do not receive after date.              */~
            CH(3),          /* buyer/planner code                      */~
            PD(14,4),       /* Total Quantity currently in Receiving Ho*/~
            PD(14,4),       /* Quantity currently in QC (Pending)-Purch*/~
            PD(14,4),       /* Total Quantity currently in QC Hold (P.O*/~
            PD(14,4),       /* Total quantity moved to on-hand inventor*/~
            PD(14,4),       /* Total quantity rejected /  scrapped.    */~
            PD(14,4),       /* Total quantity moved to rework.         */~
            CH(80)          /* Filler For Rest of Record or Internal Sp*/~

L35860: FMT                 /* FILE: RCVLINES                          */~
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
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            PD(14,4),       /* Total Quantity Received To Date - Purcha*/~
            PD(14,4),       /* Total Quantity currently in Receiving Ho*/~
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
            PD(14,4),       /* Value of Receiver Line that HASN'T been */~
            PD(14,4),       /* Value of Receiver Line that HAS been inv*/~
            PD(14,4),       /* Accounts Payable Adjustment Amount      */~
            PD(14,4),       /* Scrap Adjustment Amount                 */~
            PD(14,4),       /* Rework Adjustment Amount                */~
            CH(9),          /* Payables Account                        */~
            CH(9),          /* General Ledger Account Number           */~
            CH(9),          /* General Ledger Account Number           */~
            CH(9),          /* Expenses Account Number.                */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Lot Number                              */~
            CH(6),          /* rejection code  ccccss fmt              */~
            PD(14,4),       /* Open (Uninvoiced) Quantity              */~
            PD(14,7),       /* material price A/P                      */~
            XX(16),         /* filler                                  */~
            CH(4),          /* Line level Receiver Text                */~
            CH(4),          /* Q.C. Line Item Text                     */~
            CH(5)           /* Filler                                  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "VBKHSTRY: " & str(cms2v$,,8%)
                  str(line2$,1,50) = "Report Selection Criteria"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40240,         /* P.O. Number Range*/~
                                    L40240,         /* Vendor Range     */~
                                    L40240,         /* Order Date Range */~
                                    L40240,         /* Sort Sequence    */~
                                    L40240,         /* Past Due Only?   */~
                                    L40240          /* History Files?   */

                  goto L40300

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40240:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40300:
L40310:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Purchase Order Receipt History",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,20), fac(hex(ac)), column$(1)             , ch(25),~
               at (06,47), fac(hex(ac)), column$(2)             , ch(25),~
               at (07,02),                                               ~
                  "P.O. Number",                                         ~
               at (07,20), fac(lfac$( 1)), beg_po$              , ch(16),~
               at (07,47), fac(lfac$( 1)), end_po$              , ch(16),~
               at (08,02),                                               ~
                  "Vendor Code",                                         ~
               at (08,20), fac(lfac$( 2)), beg_vendor$          , ch(09),~
               at (08,47), fac(lfac$( 2)), end_vendor$          , ch(09),~
               at (09,02),                                               ~
                  "Order Date",                                          ~
               at (09,20), fac(lfac$( 3)), beg_date$            , ch(10),~
               at (09,47), fac(lfac$( 3)), end_date$            , ch(10),~
               at (10,02),                                               ~
                  "Past Due Only ?",                                     ~
               at (10,20), fac(lfac$( 4)), past_due$            , ch(03),~
               at (11,02), "Sort Order",                                 ~
               at (11,20), fac(lfac$( 5)), sortby$              , ch(01),~
               at (12,02), "History Files?",                             ~
               at (12,20), fac(lfac$( 6)), history$             , ch(01),~
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

               if keyhit% <> 13 then L40770
                  call "MANUAL" ("VBKHSTRY")
                  goto L40310

L40770:        if keyhit% <> 15 then L40820
                  call "PRNTSCRN"
                  goto L40310

L40820:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* P.O. Number Range*/~
                                    L50300,         /* Vendor Range     */~
                                    L50500,         /* Order Date Range */~
                                    L50800,         /* Past Due Only?   */~
                                    L50840,         /* Sort Sequence    */~
                                    L50890          /* History Files?   */
                  return
L50100:     REM Test Data for P.O. Number Range
                if beg_po$ = "ALL" then end_po$ = " "
                if beg_po$ = "ALL" then return
                if beg_po$ > "   " then L50210
                errormsg$ = "Cannot be blank!"
                return
L50210:         if end_po$ = " " then end_po$ = beg_po$
                if end_po$ >= beg_po$ then return
                errormsg$ = "Ending P.O. # Cannot be less than " &       ~
                            "Beginning P.O. #"
                return
L50300:     REM Test Data for Vendor Code Range
                if beg_vendor$ = "ALL" then end_vendor$ = " "
                if beg_vendor$ = "ALL" then return
                if beg_vendor$ > "   " then L50410
                errormsg$ = "Cannot be blank!"
                return
L50410:         if end_vendor$ = " " then end_vendor$ = beg_vendor$
                if end_vendor$ >= beg_vendor$ then return
                errormsg$ = "Ending Vendor Cannot be less than " &       ~
                            "Beginning Vendor"
                return
L50500:     REM Test Data for Order Date Range
                call "DATEOKC" (beg_date$, bdate%, errormsg$)
                if errormsg$ <> " " then return
                if end_date$ = " " then end_date$ = beg_date$
                if end_date$ = beg_date$ then call "DATUFMTC" (end_date$)
                call "DATEOKC" (end_date$, edate%, errormsg$)
                if errormsg$ <> " " then return
                if edate% >= bdate% then return
                errormsg$ = "Ending Date Cannot be less than "     &     ~
                            "Beginning Date"
                return
L50800:     REM Test Data for Past Due Only?
                if past_due$ = "YES" or past_due$ = "NO" then return
                errormsg$ = "Must be YES or NO"
                return
L50840:     REM Test Data for Sort Sequence
                if sortby$ = "1" or sortby$ = "2" or sortby$ = "3" then  ~
                           return
                errormsg$ = "Sort Sequence Must be '1', '2', or '3'"
                return

L50890:     REM Test Data for Use History Files?
                if history$ = "Y" or history$ = "N" then return
                errormsg$ = "Use History Files must be 'Y'es or 'N'o."
                return

        print_from_vbkmastr
            if key% = 0% then str(plowkey$,,25) = addc all(hex(ff))      ~
                         else str(plowkey$,,16) = addc all(hex(ff))
L55015:     call "PLOWALTS" (#mc%, plowkey$, key%, 0%, f1%(mc%))
            if f1%(mc%) = 0% then return
            gosub load_vbkmastr
            ok$ = "Y"
            gosub include_test
            if ok$ <> "Y" then L55015
            order_date2$ = order_date$
            call "DATEFMT" (order_date2$)
            filekey$ = str(vendor$) & po$
            gosub plow_lines
            goto L55015

        print_from_sortfile
            call "READNEXT" (#4, f1%(4))
            if f1%(4) = 0% then return
            gosub load_sortfile
            call "REDALT0" (#mc%, po$, 1%, f1%(mc%))
            if f1%(mc%) = 0% then print_from_sortfile
            gosub load_vbkmastr
            order_date2$ = order_date$
            call "DATEFMT" (order_date2$)
            filekey$ = str(vendor$) & po$
            gosub plow_lines
            goto print_from_sortfile
            return

        plow_lines
            call "PLOWNEXT" (#lc%, filekey$, 25%, f1%(lc%))
            if f1%(lc%) = 0% then return
            gosub load_vbklines
            if pastdue$ = " " and past_due$ = "YES" then plow_lines
            gosub print_po_line
            str(filekey$,29%) = all(hex(00))
            call "PLOWALTS" (#3, filekey$, 2%, 28%, f1%(3))
            if f1%(3) = 0% then plow_lines
            if l% < 4% then gosub print_heading
            print : l% = l% - 1%
            print                                                        ~
        "                                            ________________    ~
        ~       ________     ________________   ____________ _______";
            print  skip(0);
            print using L64300
            l% = l% - 1%
            gosub plow_receiver_history
            str(filekey$,29%) = " "
            print
            l% = l% - 1%
            goto plow_lines

        plow_receiver_history
            gosub load_rcvlines
            gosub print_rcv_line
            call "PLOWALTS" (#3, filekey$, 2%, 28%, f1%(3))
            if f1%(3) = 0% then return
            goto plow_receiver_history

        print_po_line
            if l% < 1% then gosub print_heading
            if po$ <> last_po$ and l% < 50% then print
            if po$ <> last_po$ and l% < 50% then l% = l% - 1%
            if po$ <> last_po$ then                                      ~
            print using L64270, po$, vendor$, order_date2$, item$, part$, ~
                               due_date$, order_qty, qty_remaining,      ~
                               qty_received, str(stdlt$,,7%), pastdue$   ~
                               else                                      ~
            print using L64270, " ", " ",     " ",          item$, part$, ~
                               due_date$, order_qty, qty_remaining,      ~
                               qty_received, str(stdlt$,,7%), pastdue$
            l% = l% - 1%
            last_vendor$ = vendor$
            last_po$ = po$
            return

        print_rcv_line
            if l% > 0% then L58050
               gosub print_heading
               print                                                     ~
        "                                            ________________    ~
        ~       ________     ________________   ____________ _______";
               print  skip(0);
               print using L64300
               l% = l% - 1%
L58050:     print using L64330, receiver$, lastrecdate$, packslip$,       ~
                               qty_recvd, actlt$
            l% = l% - 1%
            return

        print_heading
            print page
            page% = page% + 1%
            print using L64060, date$, rpt_time$, company$
            print_title$ = "PURCHASE ORDER RECEIPT HISTORY REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64100, hist_msg$, print_title$, page%
            if sortby$ = "1" then print_title$ = "BY PURCHASE ORDER"
            if sortby$ = "2" then print_title$ = "BY VENDOR"
            if sortby$ = "3" then print_title$ = "BY ORDER DATE"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64130, print_title$
            print
            l% = 50%
            if end$ = "Y" then return
            print using L64210
            print using L64240
            last_po$ = " "
            return

        print_params
            end$ = "Y"
            gosub print_heading
            end$ = " "
L61035:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L61055
                str(i$(), i%, 1%) = hex(20)
                goto L61035
L61055:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 6% to 17%: print tab(37); i$(x%) : next x%
            print tab(37); "Legend: Standard Leadtime Flags are:"
            print tab(37); "           S = Standard LT"
            print tab(37); "           E = Planning Effective LT"
            print tab(37); "           V = Vendor's Stated LT"
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
        ~################################                    VBKHSTRY:VBK0~
        ~06

L64100: %  (####### Files Only)              ############################~
        ~################################                        PAGE:####

L64130: %                                    ############################~
        ~################################

L64210: %PURCHASE ORDER    VENDOR     ORD-DATE   LN  PART NUMBER         ~
        ~       DUE DATE  ORDERED     REMAINING   RECEIVED    STD-LT PAST-~
        ~DUE

L64240: %----------------  ---------  --------  ---  --------------------~
        ~-----  --------  ----------  ----------  ---------- ------- -----~
        ~---

L64270: %################  #########  ########  ###  ####################~
        ~#####  ######## -#######.## -#######.## -#######.## #######    ##

L64300: %                                            RECEIVER NUMBER     ~
        ~       REC-DATE     PACKING LIST         QTY-REC     ACT-LT

L64330: %                                            ################    ~
        ~       ########     ################   -########.## #######

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
