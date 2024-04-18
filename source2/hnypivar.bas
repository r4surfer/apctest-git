        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  V   V    A    RRRR    *~
            *  H   H  NN  N  Y   Y  P   P    I    V   V   A A   R   R   *~
            *  HHHHH  N N N   YYY   PPPP     I    V   V  AAAAA  RRRR    *~
            *  H   H  N  NN    Y    P        I     V V   A   A  R   R   *~
            *  H   H  N   N    Y    P      IIIII    V    A   A  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIVAR - This program prints the Physical Inventory     *~
            *            Variance Report(s) for the specified Count     *~
            *            Session.                                       *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/30/86 ! Original                                 ! LDJ *~
            * 03/19/87 ! File layout changes, no significant      ! LDJ *~
            *          !  functionality changes to this program.  !     *~
            *          !  (just more complex processing code...)  !     *~
            *          !  Fixed a sort bug that occurred if a     !     *~
            *          !  recount ticket(s) was generated and then!     *~
            *          !  voided.                                 !     *~
            * 03/10/88 ! Fixed truncating book Qty & truncating   ! DAW *~
            *          ! totals.  Cleaned headings and also fixed !     *~
            *          ! variance percent to be properly checked. !     *~
            * 06/01/88 ! Minor Cleanup                            ! MJB *~
            * 10/04/88 ! Minor Cleanup                            ! JDH *~
            * 03/30/89 ! Fixed NEXT w/o FOR when editing;PRR 10738! RJM *~
            * 11/30/90 ! Added MAT TOT_COST1 = 0 @ ln 30890       ! MJB *~
            * 06/26/91 !(NO PRR) Corrected Scratching of Printfile! RJB *~
            *          !    upon program exit. Added 'ALLFREE'    !     *~
            * 07/10/91 !QC-FIXES Added literal 'Date' to screen,  !     *~
            *          !    Aligned columns on Lot Level Sub-total!     *~
            * 10/03/91 ! PRR 12121 Add Part as Ascend Sort field. ! JIM *~
            * 11/05/91 ! CMS/DEC 'MASK' Project.                  ! SID *~
            *          !  Added 'SHOSTAT' for file open.          !     *~
            * 01/08/92 ! Added a print option literal texts to    ! SID *~
            *          !  the report heading for part number      !     *~
            * 09/21/92 ! Changed ABC Class from CH(4) to CH(5)    ! SID *~
            * 10/02/92 ! Include Location Variance on Report, use ! RJH *~
            *          !  'L' sufix on Location values printed    !     *~
            *          !  PRR 12051 Use 'Snapshot' Location File  !     *~
            *          !  PRR 12232 Line spacing between subtotals!     *~
            *          !  Report Parametes page moved to PAGE = 0 !     *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 06/07/93 ! Report Total Image change to 1) Not      ! RJH *~
            *          !  Display '$' 2) Display the value's Sign !     *~
            * 02/08/94 ! PRR 13089 - Check for Std Cost when Part ! RJH *~
            *          !  is not in Captured Costs                !     *~
            * 08/09/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            abc$5,                       /* ABC Classes to Count       */~
            abc_cat$1,                   /* ABC Category from Part Mstr*/~
            accounted_for$1,             /* All Tickets Accounted For? */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cat$(9,2)4,                  /* Categories to Count (Range)*/~
            cba$4,                       /* ABC Classes to Report      */~
            check_digit$1,               /* Calc & Append Check Digit? */~
            company$60,                  /* Company / Division Name    */~
            control$2,                   /* Printer Control Characters */~
            cost$9,                      /* Unit Cost printed          */~
            cost1(12),                   /* Cost Buckets from HNYPICST */~
            cost2(12),                   /* Cost Buckets from HNYPITKT */~
            cost3(12),                   /* Cost Buckets from HNYPICST */~
            cost4(12),                   /* Cost Buckets from HNYPITKT */~
            cost_s(12),                  /* Cost Buckets from Stnd Cost*/~
            count_date$8,                /* Date Counted               */~
            count_qty$10,                /* Ticket Count Quantity      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date1$8,                     /* Date Session Generated     */~
            date2$8,                     /* Date Quantities Captured   */~
            dateent$6,                   /* Date Count Entered         */~
            description$32,              /* Count Session Number       */~
            detail_level$1,              /* Detail Level to Print      */~
            dif2$10,                     /* Quantities     Difference  */~
            diff$10,                     /* Extended Value Difference  */~
            edtmessage$79,               /* Edit screen message        */~
            enter_date$8,                /* Date Count Entered         */~
            enter_time$8,                /* Time Count Entered         */~
            errormsg$79,                 /* Error message              */~
            exclude_qty_pct$3,           /* Exclude Items w/QTY % <    */~
            exclude_ext_pct$3,           /* Exclude Items w/EXT % <    */~
            ext1$10,                     /* Extended Value - Count     */~
            ext2$10,                     /* Extended Value - File      */~
            extra$6,                     /* Number of Extra Tickets    */~
            ext(4,2),                    /* Extensions Accumulators    */~
            file$8,                      /* HNYPITKT File Name         */~
            filekey$100,                 /* Misc Read / Plow Key       */~
            group$3,                     /* Group Tickets By Part #    */~
            gvar$1,                      /* Update G/L variances flag  */~
            glvar$1,                     /* Update G/L variances flag  */~
            hdr$30,                      /* Report Sequence Literal    */~
            hyvar$,                      /* Update HNY variances Flag  */~
            hnyvar$,                     /* Update HNY variances Flag  */~
            i$(24)80,                    /* Screen Image               */~
            include_missing$3,           /* Include Missing Tickets ?  */~
            inpmessage$79,               /* Informational Message      */~
            lastlot$16,                  /* Last Lot  read from file   */~
            lastpart$25,                 /* Last Part read from file   */~
            last_part$25,                /* Last Part read from file   */~
            laststore$3,                 /* Last Store read from file  */~
            lastticket$6,                /* Last Ticket Nbr Processed  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* HNYPITKT Library name      */~
            line2$79,                    /* Second Line of Screen Headr*/~
            loc$8,                       /* Part Location Code         */~
            lockey$100,                  /* Misc Read / Plow Key       */~
            lot$16,                      /* Lot Code                   */~
            lotcode$16,                  /* Lot Code                   */~
            lot_or_loc$1,                /* Tickets by Lot or Loc Flag */~
            method$1,                    /* Cost Method                */~
            name$20,                     /* Count by name              */~
            neg_only$1,                  /* Count Neg On-Hand Only ?   */~
            override$1,                  /* Cost Override flag         */~
            ovrd$1,                      /* Cost Override flag         */~
            p$134,                       /* Print Line                 */~
            parameters$3,                /* Print Session Parameters?  */~
            part$25,                     /* Part Code                  */~
            part$(3,2)25,                /* Parts to Count (Ranges)    */~
            partcode$25,                 /* Part Code                  */~
            partdescr$32,                /* Part Description           */~
            part_req$1,                  /* Part Numbers required ?    */~
            pf16$16,                     /* PF 16 literal              */~
            pf4$17,                      /* PF  4 literal              */~
            plowkey$100,                 /* Misc Read / Plow Key       */~
            prefix$3,                    /* Ticket Number Prefix (Opt) */~
            print$1,                     /* Print tickets or sheets    */~
            qoh$10,                      /* File Quantity On Hand      */~
            qty_var$6,                   /* Quantity Variance %        */~
            readkey$100,                 /* Misc Read / Plow Key       */~
            recounte$3,                  /* Ext Recount % Level        */~
            reportdate$45,               /* Formatted Date & Time      */~
            seq$1,                       /* Ticket Number Sequence     */~
            sequence$18,                 /* Ticket Number Sequence     */~
            session_date$8,              /* Planned Count Date         */~
            session_nbr$2,               /* Count Session Number       */~
            sort$116,                    /* Sort parameters to SORTCALL*/~
            sortby$1,                    /* Sort in Descending Seq By..*/~
            start_ticket$6,              /* Starting Ticket Number     */~
            store$3,                     /* Store / Warehouse Code     */~
            store2$3,                    /* Store / Warehouse Code     */~
            subtotal$1,                  /* Print Subtotals flag       */~
            supplement$1,                /* Supplemental Ticket Flag   */~
            suppl$1,                     /* Supplemental Ticket Flag   */~
            ticket$12,                   /* Formatted Ticket Number    */~
            time$8,                      /* Time Quantities Captured   */~
            tot_qty(4,2),                /* Quantities Accumulators    */~
            tot_qty1$10,                 /* Print total count Qty      */~
            tot_qty2$10,                 /* Print total book  Qty      */~
            tot_qty3$10,                 /* Print total Diff  Qty      */~
            uom$4,                       /* Sales Unit of Measure      */~
            userid$3,                    /* Current User Id            */~
            user_id$3,                   /* Entered by user id         */~
            vol$6,                       /* HNYPITKT Volume name       */~
            whse$3,                      /* Warehouse to Print or ALL  */~
            whsedescr$32,                /* Warehouse to Print Descript*/~
            whse$(9,2)3,                 /* Warehouses to Count (Range)*/~
            workkey$100                  /* Misc Read / Plow Key       */~

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

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYPISYS ! Physical Inventory System Session Contro *~
            * #2  ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! STORNAME ! Store Info File - Name/Address           *~
            * #5  ! HNYPICST ! Physical Inventory Costs Snapshot File   *~
            * #6  ! PRINTER  ! Printer Spooled Output File              *~
            * #7  ! TESTFILE ! Work file for Multiple Locations per Lot *~
            * #8  ! WORKFILE ! Work file for Sorting Tickets            *~
            * #9  ! HNYPILOC ! Physical Inventory Location Snapshot File*~
            * #13 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~

            select #1,  "HNYPISYS", varc, indexed, recsize =  512,       ~
                        keypos =    7, keylen =   2,                     ~
                        alt key  1, keypos =    1, keylen =   8

            select #2,  "HNYPITKT", varc, indexed, recsize =  512,       ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  52, dup,    ~
                            key  2, keypos =  313, keylen =  16

            select #3,  "HNYMASTR", varc, indexed, recsize =  900,       ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #4,  "STORNAME", varc, indexed, recsize =  300,       ~
                        keypos = 1, keylen = 3

            select #5,  "HNYPICST", varc, indexed, recsize =  256,       ~
                        keypos =    1, keylen =  46

            select #6,  "PRINTER", printer, recsize = 134

            select #7,  "TESTFILE", varc, indexed, recsize =   44,       ~
                        keypos =    1, keylen =  44

            select #8,  "WORKFILE", varc, consec,  recsize =   81

            select #9 , "HNYPILOC",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  44                      ~

            select #13, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~


            call "SHOSTAT" ("Opening Files, One Moment Please...")

            rslt$(1%), rslt$(2%), rslt$(3%), rslt$(5%) = "REQUIRED"

            call "OPENCHCK" (#1,  fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#9,  fs%(9%), f2%(9%), 0%, rslt$(9%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%), 0%, rslt$(13%))

            if f2%(1%) + f2%(2%) + f2%(3%) + f2%(5%) + f2%(13%) <> 0%    ~
                                                       then exit_program
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
                           " to Desired Value and Press (RETURN)."

            call "GETNAMES" addr(#2, file$, lib$, vol$)
            call "READFDR"  addr(file$,lib$,vol$,0%,"RC",records%,f2%(2))
            records% = max(1000%, records%)

            call "COMPNAME" (2%, company$, ret%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, session_nbr$, whsedescr$,  ~
                      description$, whse$, cba$, group$, detail_level$,  ~
                      exclude_qty_pct$, exclude_ext_pct$, subtotal$,     ~
                      sortby$,            recounte$, parameters$,        ~
                      include_missing$
            pf16$ = "(16)Exit Program"
            pf4$  = "(4)Previous Field"

            for fieldnr% = 1% to 11%
L10150:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10290
L10170:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10250
L10200:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10170
                         if fieldnr% = 1% then L10150
                         goto L10200
L10250:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10290:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editmode
            pf4$  = " "
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$

            gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 12% then editmode
            if fieldnr% > 9% then fieldnr% = fieldnr% - 1%
            gosub'051(fieldnr%)
                  if enabled% = 0% then editmode
                  pf16$ = " "
L11200:     gosub'101(fieldnr%)
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)
                if errormsg$ <> " " then L11200
            goto editmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Now Selecting Entries for the Report")
            if lot_or_loc$ <> "L" or sortby$ = "B" then L19100
               call "WORKOPN2" (#7, "OUTPT", records%, f2%(7))
               close #7
               call "WORKOPN2" (#7, "IO   ", records%, f2%(7))
L19100:     call "WORKOPN2" (#8, "OUTPT", records%, f2%(8))
            call "DATE" addr("HL", reportdate$)
            call "SPCESMSH" (reportdate$, 2%)
            call "STRING" addr("LJ",reportdate$, 45%)
            plowkey$ = session_nbr$
            l% = 0%                     /* Number of Lines Left on Page*/
            page% = 0%                  /* Page Counter (of course)    */
            mat tot_qty = zer
            mat ext = zer
            lastpart$, last_part$ = " "
            counter% = 0%               /* Nbr of records to sort file */
            gosub generate_sortfile
            if counter% > 0% then L19310
               call "ASKUSER" (k%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no Ticket records meeting the given parameters",~
                 "were found for this Count Session.              ",     ~
                 "Press RETURN to change the parameters or exit.")
                 gosub scratch_workfiles
                 goto editmode
L19310:     call "SHOSTAT" ("Now Sorting the Selected Entries")
            call "SETPRNT" ("HNY009","HNYPI   ",records%, 0%)
            call "OPENPRT" (#6, records%, file$, lib$, vol$, f2%(6))
            gosub sort_the_file
            call "SHOSTAT" ("Report Generation In Progress")
L19335:     if parameters$ = "YES" then gosub print_params
            if group$ = "NO" then L19410
               call "READNEXT" (#8, f1%(8))
               if f1%(8) = 0% then L19335
               get #8 using L19520, lastpart$, laststore$, lastlot$
               first_time% = 1%
               gosub read_and_print_by_part
               goto L19415
L19410:     gosub read_and_print_by_ticket
L19415:     gosub report_total
            if page% = 0% then L19480
*             GOSUB'181(1%)
               time$ = " " : call "TIME" (time$)
               put p$ using L34730, " " , time$
               gosub'181(1%)
L19480:     close #6 : gosub scratch_workfiles
            call "SETPRNT" ("HNY009","HNYPI   ",records%, 1%)
            goto inputmode

L19520:     FMT POS(26), CH(25), CH(3), CH(16), CH(12)

        sort_the_file
            call "GETNAMES" addr(#8, file$, lib$, vol$)
            close #8
            sort$ = file$
            str(sort$,9%,8%)   = lib$
            str(sort$,17%,6%)  = vol$
            str(sort$,23%,22%) = str(sort$,,22%)
            str(sort$,45%,04%) = "0001"
            if sortby$ <> "P"                                            ~
                then str(sort$,49%,03%) = "008"                          ~
                else str(sort$,49%,03%) = "025"
            if sortby$ <> "P"                                            ~
                then str(sort$,52%,01%) = "F"                            ~
                else str(sort$,52%,01%) = "C"
            if sortby$ <> "P"                                            ~
                then str(sort$,53%,01%) = "D"                            ~
                else str(sort$,53%,01%) = "A"
            str(sort$,54%,04%) = "0070"
            str(sort$,58%,03%) = "012"
            str(sort$,61%,01%) = "C"
            str(sort$,62%,01%) = "A"
            if group$ = "NO" then L19720
               str(sort$,54%,04%) = "0026"
               str(sort$,58%,03%) = "056"
L19720:     call "SORTCALL" addr(sort$, ret%)
            if ret% = 0% then L19790
               call "ASKUSER" (k%, "*** SORT FAILURE ***",               ~
               "Either unable to link to SORT (not found or protected)", ~
               "or SORT failed for some other reason (not enough space)."~
              ,"Press RETURN to acknowledge & exit (still restartable).")
               goto exit_program
L19790:     call "WORKOPN2" (#8, "INPUT", 0%, f2%(8))
            return

        scratch_workfiles
            call "FILEBGON" (#7)
            call "FILEBGON" (#8)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20200,         /* Count Session Num*/~
                                    L20240,         /* Store to Print or*/~
                                    L20290,         /* ABC Classes to Pr*/~
                                    L20340,         /* Group Tickets ?  */~
                                    L20400,         /* Print Detail Leve*/~
                                    L20460,         /* Exclude QTY Var %*/~
                                    L20510,         /* Print Subtotals ?*/~
                                    L20620,         /* Sort Sequence    */~
                                    L20670,         /* Generate Recount%*/~
                                    L20730,         /* Include Missing ?*/~
                                    L20780          /* Print Parameters?*/
                  return
L20200: REM Default/Enable for Count Session Number
                inpmessage$ = "Enter the Count Session Number to Print or~
        ~ RETURN to Find"
                return
L20240: REM Default/Enable for Warehouse
                inpmessage$ = "Enter ALL to include all Warehouses or a s~
        ~pecific Warehouse"
                if whse$ = " " then whse$ = "ALL"
                return
L20290: REM Default/Enable for ABC Classes
                inpmessage$ = "Enter ALL or the specific ABC class(es) to~
        ~ include in this report"
                if cba$ = " " then cba$ = "ALL"
                return
L20340: REM Default/Enable for Group Tickets by Part
                inpmessage$ = "Enter YES to group tickets by Part# or NO"
                if group$ = " " then group$ = "YES"
                if lot_or_loc$ = "L" then group$ = "YES"
                if lot_or_loc$ = "L" then enabled% = 0%
                return
L20400: REM Default/Enable for Detail Level to Print
                inpmessage$ = "P=Part Level Only, W=Part/Whse Level, L=Pa~
        ~rt/Whse/Lot Level or T=Ticket Level"
                if group$ <> "YES" then detail_level$ = "T"
                if group$ <> "YES" then enabled% = 0%
                if detail_level$ = " " then detail_level$ = "T"
                return
L20460: REM Default/Enable for Exclude Qty Var %
                inpmessage$ = "Include only items with a Quantity Varianc~
        ~e above this percentage"
        REM 0% = List all, 90% = List above 90% Only, etc
                if exclude_qty_pct$ = " " then exclude_qty_pct$ = "0"
                return
L20510: REM Default/Enable for Print Subtotals ?
                inpmessage$ = "Print Sub-Totals? (P=Parts Only, W=Part/Wh~
        ~se, L=Part/Whse/Lot, N=None)"
                if group$ <> "YES" then subtotal$ = "N"
                if group$ <> "YES" then enabled% = 0%
                if detail_level$ = "P" then subtotal$ = "P"
                if detail_level$ = "W" then subtotal$ = "W"
                if detail_level$ = "L" then subtotal$ = "L"
                if lot_or_loc$ = "L" and subtotal$=" " then subtotal$="L"
                if detail_level$ <>"T" then enabled% = 0%
                if subtotal$ = " " and group$= "YES" then subtotal$ = "P"
                if subtotal$ = " " then subtotal$ = "N"
                return
L20620: REM Default/Enable for Sort Sequence
                inpmessage$ = "V=Var %; E=Ext Cost Diff; C=Count x Cost; ~
        ~B=Book Qty x Cost; P=Part"
                if sortby$ = " " then sortby$ = "V"
                return
L20670: REM Default/Enable for Recount Percentages
                inpmessage$ = "Change to less than 100 to auto-generate R~
        ~ecount Tickets"
                if recounte$ = " " then recounte$ = "999"
                return
L20730: REM Default/Enable for Include Missing Tickets ?
                inpmessage$ = "YES to include unaccounted for Tickets on ~
        ~Report or NO"
                if include_missing$ = " " then include_missing$ = "NO"
                return
L20780: REM Default/Enable for Print Session Parameters?
                inpmessage$ = "YES to Print Session Parameters Informatio~
        ~n or NO"
                if parameters$ = " " then parameters$ = "NO"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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

        dataload

            get #1 using L35060 ,                                         ~
            session_date$,  /* Date count session planned              */~
            session_nbr$,   /* Number corresponding to a Inventory Coun*/~
            description$,   /* Generic for general code descriptions   */~
            whse$(),        /* Warehouse or Stores  (Ranges)           */~
            cat$(),         /* Category codes (Ranges)                 */~
            part$(),        /* Part codes (ranges)                     */~
            neg_only$,      /* Count Negative items only ?             */~
            lot_or_loc$,    /* 1 Ticket per Lot or 1 per Location flag */~
            prefix$,        /* 1 to 3 character prefix to the P.I. Tick*/~
            start_ticket$,  /* Ticket Number to a Physical Inventory Co*/~
            lastticket$,    /* Last Ticket Number used                 */~
            check_digit$,   /* Calculate & append check digit ?        */~
            extra$,         /* Number of extra Physical Inventory Ticke*/~
            seq$,           /* Flag controlling what sequence tickets w*/~
            part_req$,      /* Part Numbers required in Count Entry ?  */~
            print$,         /* Print tickets or Count sheets  Indicator*/~
            hnyvar$,        /* Update inventory variances     Indicator*/~
            glvar$,         /* Update G/L       variances     Indicator*/~
            date1$,         /* Date a session generated                */~
            date2$,         /* Date costs/quantities captured          */~
            time$,          /* Time costs/quantities captured          */~
            accounted_for$, /* All Tickets Accounted For ? (Y or blank)*/~
            abc$            /* ABC category                            */

            call "DATEFMT" (session_date$)
            call "DATEFMT" (date1$)
            call "DATEFMT" (date2$)
            call "TIME" (time$)
            return

        load_ticket
            get #2 using L35310 ,                                         ~
               session_nbr$,/* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
               recount%,    /* A number decremented for each recount   */~
                            /* (99=original                            */~
               part$,       /* Part Number                             */~
               store$,      /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
               lot$,        /* Lot Number                              */~
               loc$,        /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
               supplement$, /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
               gvar$,       /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
               hyvar$,      /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
               override$,   /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
               name$,       /* Name of person who counted something    */~
               user_id$,    /* user-id of specific user                */~
                            /* User Id of Data Entry Operator          */~
               count_date$, /* Date something was counted              */~
               enter_date$, /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
               enter_time$, /* The System Time when a transaction was  */~
                            /* entered                                 */~
               count_qty,   /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
               locqty,      /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
               cost2(),     /* 12 cost buckets & total cost            */~
               tot_cost2    /*              - user entered/supplied.   */~

            filekey$ = str(session_nbr$) & str(part$) & str(store$) & lot$
            if filekey$ = key(#5) and fs(#5) = "00" then L30840
            call "READ100" (#5, filekey$, f1%(5))
            if f1%(5) = 1% then L30840

            mat cost_s = zer : tot_cost, tot_cost_s, qoh = 0
            gosub try_for_standard
            mat cost1 = cost_s : tot_cost1 = tot_cost_s
            goto L30930

L30840:     get #5 using L36130,                                          ~
                qoh,        /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
                cost1(),    /* 12 cost buckets                         */~
                tot_cost1   /* Total unit cost                         */~
                            /*         These are the frozen or         */~
                            /*               'snapshot' cost fields    */

L30930:     if override$ = "Y" then tot_cost = tot_cost2                 ~
                               else tot_cost = tot_cost1
            return

        try_for_standard
            call "READ100" (#3, part$, f1%(3%))
            if f1%(3%) = 0% then return
            get #3 using L30991, method$
            if method$ = "S" or method$ = "T" or method$ = "F" then      ~
               call "STCCOSTS" (part$, " ", #13, 2%, tot_cost_s, cost_s())
            return

L30991:     FMT POS(307), CH(1)

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        generate_sortfile
            str(plowkey$,15%) =  hex(ff)
L31080:     call "PLOWNEXT" (#2, plowkey$, 2%, f1%(2))
            if f1%(2) = 0% then return
            gosub load_ticket
            if supplement$ = "X" and part$ = " " then generate_sortfile
            if include_missing$ = "NO" and (enter_date$ = " " ~
               or enter_date$ = blankdate$) then generate_sortfile
            if count_qty < 0 then L31080              /* Voided Ticket  */
            include% = 1%
            gosub include_check
            if include% = 1% then generate_sortfile

               if sortby$ = "V" then sortkey1 = abs(qty_var)
               if sortby$ = "E" then sortkey1 = abs(ext1 - ext2)
               if sortby$ = "C" then sortkey1 = abs(ext1)
               if sortby$ = "B" then sortkey1 = abs(ext2)
               if sortby$ = "P"                                          ~
                     then write #8, using L31271, part$, part$, store$,   ~
                          lot$, ticket$                                  ~
                     else write #8, using L31270, sortkey1, " ", part$,   ~
                          store$, lot$, ticket$
               counter% = counter% + 1%
               goto generate_sortfile
L31270:     FMT FL(8), CH(17), CH(25), CH(3), CH(16), CH(12)
L31271:     FMT CH(25), CH(25), CH(3), CH(16), CH(12)

        include_check
            if whse$ <> "ALL" and store$ <> whse$ then return
            if cba$ = "ALL" then L31380
               if part$ = lastpart$ then L31370
                  call "READ100" (#3, part$, f1%(3))
                  if f1%(3) = 0% then return
                  lastpart$ = part$
                  get #3 using L31510 , abc_cat$
L31370:        if pos(str(cba$,,len(cba$)) = abc_cat$) = 0% then return
L31380:     if group$ = "YES" then gosub sum_part_tickets
            if group$ = "YES" then L31460
               if qoh <> 0 then L31450
                  qty_var = 0
                  if count_qty = qoh then L31460
                     qty_var = 9999
                     goto L31460
L31450:        qty_var = min(999, abs((qoh - count_qty) / qoh))
L31460:     if eqp > 0 and round(qty_var,1) < eqp then return
            if group$ = "YES" then L31490
               ext1 = tot_cost * count_qty : ext2 = tot_cost * qoh
L31490:     include% = 0%
            return
L31510:     FMT POS(111), CH(1)

        sum_part_tickets
            if part$ = last_part$ then L31840
            tcqty, tfqty, text1, text2 = 0
            if lot_or_loc$ <> "L" or sortby$ = "B" then L31590
               call "DELETE" (#7, part$, 25%)
L31590:     readkey$ = str(part$) & hex(000000)
L31600:     call "PLOWALTS" (#2, readkey$, 1%, 25%, f1%(2))
            goto L31630
L31610:     call "READNEXT" (#2, f1%(2))
            if str(key(#2,1),,25%) <> str(readkey$,,25%) then f1%(2) = 0%
L31630:     if f1%(2) = 0% then L31840
            if str(key(#2),,2%) <> session_nbr$ then L31600
            get #2 using L31930 , store2$, lotcode$, suppl$, ovrd$,       ~
                                 dateent$, cqty, cost4(), tot_cost4
            if whse$ <> "ALL" and whse$ <> store2$ then L31600
            if include_missing$ = "NO" and           ~
                        (dateent$ = " " or           ~
                         dateent$ = blankdate$) then L31600
            if cqty < 0 then L31610       /* Voided ticket              */
            filekey$ = str(session_nbr$) & str(part$) & str(store2$) &   ~
                       lotcode$
            if filekey$ = key(#5) and fs(#5) = "00" then L31674
            call "READ100" (#5, filekey$, f1%(5))
            if f1%(5) = 1% then L31674
            mat cost_s = zer : tot_cst, tot_cost_s, qoh = 0
            gosub try_for_standard
            mat cost3 = cost_s : tot_cost3 = tot_cost_s : goto L31725
L31674:     get #5 using L36130,                                          ~
                fqty,       /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
                cost3(),    /* 12 cost buckets                         */~
                tot_cost3   /*   Total cost                            */~
                            /*         These are the frozen or         */~
                            /*                'snapshot' cost fields   */

L31725:     if ovrd$ = "Y" then tot_cst = tot_cost4                      ~
                           else tot_cst = tot_cost3
            tcqty = tcqty + cqty
            text1 = text1 + cqty * tot_cst
            if lot_or_loc$ <> "L" or sortby$ = "B" then L31800
               workkey$ = str(part$) & str(store2$) & str(lotcode$)
               call "READ100" (#7, workkey$, f1%(7))
               if f1%(7) = 1% then L31600
               write #7, str(part$), str(store2$), str(lotcode$)
L31800:     tfqty = tfqty + fqty
            text2 = text2 + fqty * tot_cst
            goto L31600

L31840:     if tfqty <> 0 then L31890
               qty_var = 0
               if tcqty = tfqty then L31900
                  qty_var = 9999
                  goto L31900
L31890:     qty_var = min(999,abs((tfqty - tcqty) / tfqty))
L31900:     ext1 = text1 : ext2 = text2
            last_part$ = part$
            return
L31930:     FMT POS(41), CH(3), CH(16), POS(68), CH(1),                  ~
                POS(71), CH(1), POS(101), CH(6), POS(113), PD(14,4),     ~
                POS(209), 13*PD(14,4)

        read_and_print_by_ticket
            call "READNEXT" (#8, f1%(8))
            if f1%(8) = 0% then return
            get #8 using L32020, ticket$
L32020:     FMT POS(70), CH(12)
            plowkey$ = str(session_nbr$) & str(ticket$) & hex(00)
L32030:     call "PLOWNEXT" (#2, plowkey$, 14%, f1%(2))
            if f1%(2) = 0% then read_and_print_by_ticket  /*Shouldn't */
            gosub load_ticket                             /* happen!  */
            if count_qty < 0 then L32030            /* Voided Ticket    */
            if part$ = key(#3) and fs(#3) < "10" then L32080
               partdescr$, uom$  = " "
               call "READ100" (#3, part$, f1%(3))
               if f1%(3) = 0% then L32080
               get #3, using L32075, partdescr$, uom$
L32075:        FMT POS(26),CH(32), POS(74), CH(4)
L32080:     if qoh <> 0 then L32105
               qty_var = 0
               if count_qty = qoh then L32130
                  qty_var = 9999
                  goto L32130
L32105:     if (qoh - count_qty) / qoh >= 0 then                         ~
                 qty_var = min(999, 100 * (qoh - count_qty) / qoh)       ~
            else qty_var = max(-999, 100 * (qoh - count_qty) / qoh)
            qty_var = -qty_var
            qty_var=round(qty_var,1)
            ext1 = round(tot_cost * count_qty,2)
L32130:     ext2 = round(tot_cost * qoh,2)
            tot_qty(4,1) = tot_qty(4,1) + count_qty
            ext(4,1) = ext(4,1) + ext1
            tot_qty(4,2) = tot_qty(4,2) + qoh
            ext(4,2) = ext(4,2) + ext2
            dif2 = count_qty - qoh
            call "CONVERT" (dif2, 0.2, dif2$)
            call "CONVERT" (tot_cost, 2.4, cost$)
            call "CONVERT" (ext1, 2.2, ext1$)
            call "CONVERT" (ext2, 2.2, ext2$)
            call "CONVERT" (count_qty, 0.2, count_qty$)
            call "CONVERT" (qoh, 0.2, qoh$ )
            diff = ext1 - ext2
            call "CONVERT" (diff, 2.2, diff$)
            if qty_var<9999 then convert qty_var to qty_var$, pic(###.#+)~
            else qty_var$ = "*****+"
            recount% = 99% - recount%
            if recount% = 0% then L32225
               ticket$ = ticket$ & "-"
               if recount% < 10% then convert recount% to                ~
                  str(ticket$,len(ticket$)+1%,1%), pic(0)                ~
               else convert recount% to                                  ~
                  str(ticket$,len(ticket$)+1%,2%), pic(00)
L32225:     if l% < 5% then gosub print_heading
            put p$ using L34700, part$, uom$, store$, lot$, count_qty$,   ~
                        qoh$, dif2$, cost$, ext1$, ext2$, diff$, qty_var$
            gosub'181(1%)
            put p$ using L34720 , partdescr$, " ", loc$
            gosub'181(1%)
            l% = l% - 2%
            if abs(qty_var) > rcte and abs(qty_var) < 9999               ~
               then gosub generate_recount_ticket
            if rcte < 999 and abs(qty_var) = 9999                        ~
               then gosub generate_recount_ticket
            goto read_and_print_by_ticket

        read_and_print_by_part
            if f1%(8) = 0% then return
            get #8 using L19520, part$, store$, lot$, ticket$
            new_lot% = 0%
            if first_time% = 0% then L32350
               first_time% = 0%
L32325:        partcode$   = part$
L32330:        store2$     = store$
L32335:        lotcode$    = lot$
               new_lot%    = 1%
               goto L32370
L32350:     if part$  <> lastpart$  then L32325 else partcode$ = " "
            if store$ <> laststore$ then L32330 else store2$ = " "
            if lot$   <> lastlot$   then L32335 else lotcode$ = " "

L32370:     if part$  <> lastpart$  then gosub part_break
            if store$ <> laststore$ then gosub store_break
            if lot$   <> lastlot$   then gosub lot_break
            if partcode$ <> " " then L32400
               partdescr$, uom$ = " "

L32400:     plowkey$ = str(session_nbr$) & str(ticket$) & hex(00)
L32405:     call "PLOWNEXT" (#2, plowkey$, 14%, f1%(2))
            if f1%(2) = 0% then read_and_print_by_part  /*Shouldn't */
            gosub load_ticket
            if count_qty < 0 then L32405            /* Voided Ticket    */
            if part$ = key(#3) and fs(#3) < "10" then L32455
               partdescr$, uom$  = " "
               call "READ100" (#3, part$, f1%(3))
               if f1%(3) = 0% then L32455
               get #3, using L32450, partdescr$, uom$
L32450:        FMT POS(26),CH(32), POS(74), CH(4)
L32455:     if qoh <> 0 then L32480
               qty_var = 0
               if count_qty = qoh then L32500
                  qty_var = 9999
                  goto L32500
L32480:     if (qoh - count_qty) / qoh >= 0 then                         ~
                 qty_var = min(999, 100 * (qoh - count_qty) / qoh)       ~
            else qty_var = max(-999, 100 * (qoh - count_qty) / qoh)
            qty_var = -qty_var
L32500:     ext1 = round(tot_cost * count_qty,2)
            ext2 = round(tot_cost * qoh,2)
            qty_var = round(qty_var,1)
            diff = ext1 - ext2
            tot_qty(1,1) = tot_qty(1,1) + count_qty
            ext(1,1) = ext(1,1) + ext1
            if lot_or_loc$ = "L" and new_lot% = 0% then L32540
               tot_qty(1,2) = tot_qty(1,2) + qoh
               ext(1,2) = ext(1,2) + ext2
L32540:     if detail_level$ <> "T" then L32685
            diff = ext1 - ext2
            dif2 = count_qty - qoh
            call "CONVERT" (dif2, 0.2, dif2$)
            call "CONVERT" (qoh, 0.2, qoh$)
            call "CONVERT" (tot_cost, 2.4, cost$)
            call "CONVERT" (ext1, 2.2, ext1$)
            call "CONVERT" (ext2, 2.2, ext2$)
            call "CONVERT" (diff, 2.2, diff$)
            call "CONVERT" (count_qty, 0.2, count_qty$)
            if qty_var<9999 then convert qty_var to qty_var$, pic(###.#+)~
            else qty_var$ = "*****+"
            if lot_or_loc$="L" then   gosub set_location_vals
            recount% = 99% - recount%
            if recount% = 0% then L32640
               ticket$ = ticket$ & "-"
               if recount% < 10% then convert recount% to                ~
                  str(ticket$,len(ticket$)+1%,1%), pic(0)                ~
               else convert recount% to                                  ~
                  str(ticket$,len(ticket$)+1%,2%), pic(00)
L32640:     if l% < 5% then gosub print_heading

            put p$ using L34700, partcode$, uom$, store2$, lotcode$,      ~
                   count_qty$, qoh$, dif2$, cost$, ext1$,                ~
                   ext2$, diff$, qty_var$
            gosub'181(1%)
            put p$ using L34720 , partdescr$, ticket$, loc$
            gosub'181(1%)
            l% = l% - 2%
L32685:     if abs(qty_var) > rcte and abs(qty_var) < 9999               ~
               then gosub generate_recount_ticket
            if rcte < 999 and abs(qty_var) = 9999                        ~
               then gosub generate_recount_ticket
            call "READNEXT" (#8, f1%(8))
            goto read_and_print_by_part

        lot_break
            tot_qty(2,1) = tot_qty(2,1) + tot_qty(1,1)
            tot_qty(2,2) = tot_qty(2,2) + tot_qty(1,2)
            ext(2,1) = ext(2,1) + ext(1,1)
            ext(2,2) = ext(2,2) + ext(1,2)
            if  subtotal$ <> "L" then L32930
               call "CONVERT" (ext(1,1),2.2,ext1$)
               call "CONVERT" (ext(1,2),2.2,ext2$)
               diff = ext(1,1) - ext(1,2)
               call "CONVERT" (diff,2.2,diff$)
            if tot_qty(1,2) <> 0 then L32795
               qty_var = 0
               if tot_qty(1,1)=tot_qty(1,2) then L32825
                  qty_var = 9999
                  goto L32825
L32795:     if (tot_qty(1,2)-tot_qty(1,1))/tot_qty(1,2) >= 0 then        ~
          qty_var = min(999,100*(tot_qty(1,2)-tot_qty(1,1))/tot_qty(1,2))~
            else                                                         ~
          qty_var = max(-999,100*(tot_qty(1,2)-tot_qty(1,1))/tot_qty(1,2))
            qty_var = -qty_var
            qty_var=round(qty_var,1)
L32825:     if l% < 1% then gosub print_heading
            if qty_var<9999 then convert qty_var to qty_var$, pic(###.#+)~
            else qty_var$ = "*****+"
            if detail_level$ <> "T" then                                 ~
               str(p$,47%) = " _________ __________  __________          ~
        ~  __________ __________  __________  ______"                     ~
            else                                                         ~
               str(p$,47%) = " --------- ----------  ----------          ~
        ~  ---------- ----------  ----------  ------"
            if detail_level$ <> "L" then gosub'181(0%)
            call "CONVERT" (tot_qty(1,1), 0.2, tot_qty1$)
            call "CONVERT" (tot_qty(1,2), 0.2, tot_qty2$)
            call "CONVERT" ((tot_qty(1,1) - tot_qty(1,2)), 0.2, tot_qty3$)
            if detail_level$ = "T" then put p$ using L34707, " ", " ",    ~
                  " ", "Lot  Tot", tot_qty1$, tot_qty2$, tot_qty3$, " ", ~
                  ext1$, ext2$, diff$, qty_var$                          ~
            else                        put p$ using L34707, lastpart$,   ~
                  laststore$, lastlot$,"Lot  Tot", tot_qty1$, tot_qty2$, ~
                  tot_qty3$, " ", ext1$, ext2$, diff$, qty_var$
            gosub'181(1%)
            gosub'181(1%)              /*  Skip one line  */
            l% = l% - 2%
L32930:     tot_qty(1,1), tot_qty(1,2), ext(1,1), ext(1,2) = 0
            lastlot$ = lot$
            return

        store_break
            gosub lot_break
            tot_qty(3,1) = tot_qty(3,1) + tot_qty(2,1)
            tot_qty(3,2) = tot_qty(3,2) + tot_qty(2,2)
            ext(3,1) = ext(3,1) + ext(2,1)
            ext(3,2) = ext(3,2) + ext(2,2)
            if  subtotal$ <> "W" and  subtotal$ <> "L" then L33185
               call "CONVERT" (ext(2,1), 2.2, ext1$)
               call "CONVERT" (ext(2,2), 2.2, ext2$)
               diff = ext(2,1) - ext(2,2)
               call "CONVERT" (diff, 2.2, diff$)
            if tot_qty(2,2) <> 0 then L33030
               qty_var = 0
               if tot_qty(2,1)=tot_qty(2,2) then L33060
                  qty_var = 9999
                  goto L33060
L33030:     if (tot_qty(2,2)-tot_qty(2,1))/tot_qty(2,2) >= 0 then        ~
          qty_var = min(999,100*(tot_qty(2,2)-tot_qty(2,1))/tot_qty(2,2))~
            else                                                         ~
          qty_var = max(-999,100*(tot_qty(2,2)-tot_qty(2,1))/tot_qty(2,2))
            qty_var = -qty_var
            qty_var=round(qty_var,1)
L33060:     if qty_var<9999 then convert qty_var to qty_var$, pic(###.#+)~
            else qty_var$ = "*****+"
            if l% < 1% then gosub print_heading
            if detail_level$ <> "T" or subtotal$ <> "W" then             ~
               str(p$,47%) = " _________ __________  __________          ~
        ~  __________ __________  __________  ______"                     ~
            else                                                         ~
               str(p$,47%) = " --------- ----------  ----------          ~
        ~  ---------- ----------  ----------  ------"
            if detail_level$ <> "W" then gosub'181(0%)
            call "CONVERT" (tot_qty(2,1), 0.2, tot_qty1$)
            call "CONVERT" (tot_qty(2,2), 0.2, tot_qty2$)
            call "CONVERT" ((tot_qty(2,1) - tot_qty(2,2)), 0.2, tot_qty3$)
            if detail_level$ = "T" then j% = 2% else j% = 1%
            for i% = 1% to j%
               if detail_level$ = "T" then put p$ using L34707, " ", " ", ~
                  " ","Whse Tot", tot_qty1$, tot_qty2$, tot_qty3$, " ",  ~
                  ext1$, ext2$, diff$, qty_var$                          ~
               else                        put p$ using L34707, lastpart$,~
                  laststore$, " ","Whse Tot", tot_qty1$, tot_qty2$,      ~
                  tot_qty3$, " ", ext1$, ext2$, diff$, qty_var$
                  if i% = 1% then gosub'181(1%) else gosub'181(0%)
            next i%

               gosub'181(1%)
               l% = l% - 2%
L33185:     tot_qty(2,1), tot_qty(2,2), ext(2,1), ext(2,2) = 0
            laststore$ = store$
            return

        part_break
            gosub store_break
            tot_qty(4,1) = tot_qty(4,1) + tot_qty(3,1)
            tot_qty(4,2) = tot_qty(4,2) + tot_qty(3,2)
            ext(4,1) = ext(4,1) + ext(3,1)
            ext(4,2) = ext(4,2) + ext(3,2)
            if subtotal$ = "N" then L33440
               call "CONVERT" (ext(3,1), 2.2, ext1$)
               call "CONVERT" (ext(3,2), 2.2, ext2$)
               diff = ext(3,1) - ext(3,2)
               call "CONVERT" (diff, 2.2, diff$)
            if tot_qty(3,2) <> 0 then L33285
               qty_var = 0
               if tot_qty(3,1)=tot_qty(3,2) then L33310
                  qty_var = 9999
                  goto L33310
L33285:     if (tot_qty(3,2)-tot_qty(3,1))/tot_qty(3,2) >= 0 then        ~
          qty_var = min(999,100*(tot_qty(3,2)-tot_qty(3,1))/tot_qty(3,2))~
            else                                                         ~
          qty_var = max(-999,100*(tot_qty(3,2)-tot_qty(3,1))/tot_qty(3,2))
            qty_var = -qty_var
L33310:     qty_var = round(qty_var,1)
            if qty_var<9999 then convert qty_var to qty_var$, pic(###.#+)~
            else qty_var$ = "*****+"
            if l% < 1% then gosub print_heading
            if detail_level$ <> "T" or subtotal$ <> "P" then             ~
               str(p$,47%) = " _________ __________  __________          ~
        ~  __________ __________  __________  ______"                     ~
            else                                                         ~
               str(p$,47%) = " --------- ----------  ----------          ~
        ~  ---------- ----------  ----------  ------"
            if detail_level$ <> "P" then gosub'181(0%)
            call "CONVERT" (tot_qty(3,1), 0.2, tot_qty1$)
            call "CONVERT" (tot_qty(3,2), 0.2, tot_qty2$)
            call "CONVERT" ((tot_qty(3,1) - tot_qty(3,2)), 0.2, tot_qty3$)
            if detail_level$ = "T" then j% = 3% else j% = 1%
            for i% = 1% to j%
               if detail_level$ = "T" then put p$ using L34707, " ", " ", ~
                  " ","Part Tot", tot_qty1$, tot_qty2$, tot_qty3$, " ",  ~
                  ext1$, ext2$, diff$, qty_var$                          ~
               else                        put p$ using L34707, lastpart$,~
                  " ", " ","Part Tot", tot_qty1$, tot_qty2$, tot_qty3$,  ~
                  " ", ext1$, ext2$, diff$, qty_var$
                  if i% = 1% then gosub'181(1%) else gosub'181(0%)
               next i%
               gosub'181(1%)
               l% = l% - 2%
L33440:     tot_qty(3,1), tot_qty(3,2), ext(3,1), ext(3,2) = 0
            lastpart$ = part$
            return

        report_total
            if group$ = "YES" then gosub part_break
            diff = ext(4,1) - ext(4,2)
            if ext(4,2) <> 0 then L33500
               qty_var = 0
               if ext(4,1)=ext(4,2) then L33525
                  qty_var = 9999
                  goto L33525
L33500:     if (ext(4,2)-ext(4,1))/ext(4,2) >= 0 then                    ~
               qty_var = min(999,100*(ext(4,2)-ext(4,1))/ext(4,2))       ~
            else                                                         ~
               qty_var = max(-999,100*(ext(4,2)-ext(4,1))/ext(4,2))
            qty_var = -qty_var
L33525:     qty_var=round(qty_var,1)
            if qty_var<9999 then convert qty_var to qty_var$, pic(###.#+)~
            else qty_var$ = "*****+"
            if l% < 5% then gosub print_heading
            p$ = all("=")
            gosub'181(2%)
            for i% = 1% to 3%
                put p$ using L34712, ext(4,1), ext(4,2), diff, qty_var$
                  if i% = 1% then gosub'181(1%) else gosub'181(0%)
            next i%
            gosub'181(1%)
            tot_qty(4,1), tot_qty(4,2), ext(4,1), ext(4,2) = 0
            return

        generate_recount_ticket
            gosub load_ticket
            recount% = recount% - 1%
            put #2 using L36490 ,                                         ~
               session_nbr$,/* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
               recount%,    /* A number decremented for each recount   */~
                            /* (99=original                            */~
               part$,       /* Part Number                             */~
               store$,      /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
               lot$,        /* Lot Number                              */~
               loc$,        /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
               supplement$, /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
               gvar$,       /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
               hyvar$,      /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
               override$,   /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
               " ",         /* Name of person who counted something    */~
               " ",         /* user-id of specific user                */~
                            /* User Id of Data Entry Operator          */~
               " ",         /* Date something was counted              */~
               " ",         /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
               " ",         /* The System Time when a transaction was  */~
                            /* entered                                 */~
               0,           /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
               0,0,0,0,0,   /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
               0,0,0,0,0,   /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
               locqty,      /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
               cost2(),     /* 12 cost buckets & total cost            */~
               tot_cost2,   /*              - user entered/supplied.   */~
               session_nbr$,/* Number corresponding to a Inventory     */~
               "N",         /* Tickets/Count Sheets Print Flag         */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
               recount%,    /* A number decremented for each recount   */~
                            /* (99% = Original)                        */~
               " "          /* Filler                                  */

            write #2
            if accounted_for$ = " " then return
            accounted_for$ = " "
            call "READ101" (#1, session_nbr$, f1%(1))
        REM *** no test - if gone we're screwed anyway ***
            put #1 using L33860, accounted_for$
L33860:     FMT POS(365), CH(1)
            rewrite #1
            return

        set_location_vals
            ext2$, diff$, qoh$, qty_var$, dif2$ = " "
            lockey$ = str(session_nbr$) & str(part$) & str(store$) &     ~
                      str(lot$,,6%) & str(loc$)
            call "READ100" (#9, lockey$, f1%(9%))
                if f1%(9%) = 0% then return
            get #9 using L33990, tqoh
            tdif2 = count_qty - tqoh
            call "CONVERT" (tqoh , 0.2, qoh$ )
            call "CONVERT" (tdif2, 0.2, dif2$)
            qoh$  = str(qoh$ ,2,9)  &  "L"
            dif2$ = str(dif2$,2,9)  &  "L"

            return

L33990: FMT POS(45), PD(14,4)

        print_heading
            gosub'180
            page% = page% + 1%
            put p$ using L34560, reportdate$, company$, page%
            gosub'181(1%)
            put p$ using L34575 , userid$
            gosub'181(1%)
            if sortby$ = "E" then hdr$ = "BY EXTENDED COST DIFFERENCE"
            if sortby$ = "V" then hdr$ = "BY VARIANCE PERCENTAGE"
            if sortby$ = "C" then hdr$ = "BY EXTENDED VALUE - COUNT QTY "
            if sortby$ = "B" then hdr$ = "BY EXTENDED VALUE - BOOK QTY  "
            if sortby$ = "P" then hdr$ = "BY PART NUMBER "
            call "STRING" addr("CT", hdr$, 30%)
            put p$ using L34595, hdr$, session_nbr$
            gosub'181(1%)
            if f1%(8) = 0% then return
            gosub'181(1%)
            put p$ using L34640, " "
            gosub'181(1%)
            put p$ using L34660, " "
            gosub'181(1%)
            put p$ using L34680, " "
            gosub'181(1%)
            l% = 53%
            return

        print_params
            page% = -1%
            gosub print_heading
            on val(seq$,1) - 48% gosub L34390, L34400, L34410, L34420, L34430,~
                                       L34440
            put p$ using L34745, " "
            gosub'181(1%)
            gosub'181(1%)
            put p$ using L34760, abc$, whse$(1,1), whse$(1,2), cat$(1,1), ~
                               cat$(1,2), part$(1,1)
            gosub'181(1%)
            put p$ using L34780,neg_only$,whse$(2,1),whse$(2,2),cat$(2,1),~
                               cat$(2,2)
            gosub'181(1%)
            put p$ using L34795,              whse$(3,1), whse$(3,2),     ~
                               cat$(3,1), cat$(3,2), part$(1,2)
            gosub'181(1%)
            put p$ using L34815, lot_or_loc$, whse$(4,1), whse$(4,2),     ~
                               cat$(4,1), cat$(4,2)
            gosub'181(1%)
            put p$ using L34830,              whse$(5,1), whse$(5,2),     ~
                               cat$(5,1), cat$(5,2), part$(2,1)
            gosub'181(1%)
            put p$ using L34850, prefix$,     whse$(6,1), whse$(6,2),     ~
                               cat$(6,1), cat$(6,2)
            gosub'181(1%)
            put p$ using L34865, start_ticket$, whse$(7,1), whse$(7,2),   ~
                               cat$(7,1), cat$(7,2), part$(2,2)
            gosub'181(1%)
            put p$ using L34885, lastticket$, whse$(8,1), whse$(8,2),     ~
                               cat$(8,1), cat$(8,2)
            gosub'181(1%)
            put p$ using L34900, check_digit$, whse$(9,1), whse$(9,2),    ~
                               cat$(9,1), cat$(9,2), part$(3,1)
            gosub'181(1%)
            put p$ using L34920, extra$
            gosub'181(1%)
            put p$ using L34930, sequence$, part$(3,2)
            gosub'181(1%)
            put p$ using L34945, hnyvar$ : gosub'181(1%)
            put p$ using L34950, glvar$  : gosub'181(1%)
            put p$ using L34960, date1$
            gosub'181(1%)
            put p$ using L34965, date2$, time$
            gosub'181(1%)
            gosub'181(2%)
            p$ = "  REPORT PRINT PARAMETERS"
            gosub'181(1%)
L34359:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34364
                str(i$(), i%, 1%) = hex(20)
                goto L34359

L34364:     for x% = 7% to 17%
                p$ = i$(x%)
                gosub'181(1%)
            next x%
            return

L34390:     sequence$ = "Store/Loc/Part/Lot"
            return
L34400:     sequence$ = "Store/Part/Lot/Loc"
            return
L34410:     sequence$ = "Store/Part/Loc/Lot"
            return
L34420:     sequence$ = "Part/Store/Loc/Lot"
            return
L34430:     sequence$ = "Part/Loc/Lot/Store"
            return
L34440:     sequence$ = "Part/Lot/Loc/Store"
            return

        deffn'180
            control$=hex(8001)
            write #6 using L34480 , control$, p$
            init(" ")p$
            return
L34480:     FMT CH(2), CH(132)

        deffn'181(skp%)       /* If Negative then Skip After Printing */
            if skp% < 0% then control$      = hex(40)                    ~
            else               control$      = hex(00)
            str(control$,2,1) = bin(abs(skp%),1)
            write #6 using L34480 , control$, p$
            init(" ")p$
            return

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L34560: %################################### ############################~
        ~################################                        PAGE:####

L34575: %HNYPIVAR   User ID: ###                          PHYSICAL INVENT~
        ~ORY VARIANCE REPORT                                     RPT : HNY~
        ~009

L34595: %                                        ########################~
        ~######   FOR COUNT SESSION ##

        %                                                      TO BE COUN~
        ~TED BY: ########

L34640: %                          UOM    Whs   Lot/    Count     Book   ~
        ~     Quantity              Count      Book        Ext Value   Var~
        ~nce #
L34660: %Part                      Ticket No.   Loc'n   Quantity  Quantit~
        ~y    Difference  Unit Cost Ext Value  Ext Value   Difference    %~
        ~#
L34680: %------------------------- ------------ ------  --------- -------~
        ~---  ----------  --------- ---------- ----------  ----------  ---~
        ~----#
L34700: %######################### ####   ###   ###### ########## -######~
        ~#### -########## ######### ########## -########## -########## ###~
        ~.##-
L34707: %######################### ### ###### ######## ########## -######~
        ~#### -########## ######### ########## -########## -########## ###~
        ~.##-
L34712: % **** REPORT TOTALS ***  Extended Count Value=-###,###,###.##  E~
        ~xtended Book Value=-###,###,###.##  Difference=-##,###,###.## ###~
        ~.#+

L34720: %######################### ############ ########

L34730: %                                            *************** END ~
        ~OF REPORT ***************#    @   ########

L34745: %                                                      SESSION PA~
        ~RAMETERS SECTION #

L34760:  % ABC Classes Selected: #####           Warehouses to Count: ###~
        ~ to ###   Categories: #### to ####   Parts: #####################~
        ~####

L34780:  % Count Negative QOH Only?: ###                              ###~
        ~ to ###               #### to ####              to

L34795:  % 1 Ticket per Part/Store/Lot (P),                           ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34815:  %  or 1 per Part/Store/Lot/Loc (L): #                        ###~
        ~ to ###               #### to ####

L34830:  %                                                            ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34850:  % Ticket Number Prefix   : ###                               ###~
        ~ to ###               #### to ####              to

L34865:  % Starting Ticket Number : ############                      ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34885:  % Ending Ticket Number   : ############                      ###~
        ~ to ###               #### to ####

L34900:  % Check Digits ?         : ###                               ###~
        ~ to ###               #### to ####          #####################~
        ~####

L34920:  % Number of Extra Tickets: ######                               ~
        ~                                                to
L34930:  % Ticket Number Sequence : ##################                   ~
        ~                                            #####################~
        ~####
L34945:  % Post Var to Inventory? : #
L34950:  % Post Variance to G/L ? : #

L34960:  % Session Created On     : ########
L34965:  % Costs/Qtys Captured On : ########  at ########

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: HNYPISYS                          */~
            CH(6),          /* Date something was counted              */~
            CH(2),          /* Number corresponding to a Inventory Coun*/~
            CH(30),         /* Generic for general code descriptions   */~
            XX(3),          /* Filler                                  */~
            18*CH(3),       /* Warehouse or Stores                     */~
            18*CH(4),       /* category code                           */~
            6*CH(25),       /* Part code                               */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(3),          /* 1 to 3 character prefix to the P.I. Tick*/~
            CH(06),         /* Ticket Number to a Physical Inventory Co*/~
            CH(06),         /* Last ticket number generated for a Count*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Number of extra Physical Inventory Ticke*/~
            CH(1),          /* Flag controlling what sequence tickets w*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Date a transaction was entered          */~
            CH(6),          /* System Date                             */~
            CH(6),          /* The System Time when a transaction was e*/~
            CH(1),          /* All Tickets Accounted for ? (Y or blank)*/~
            CH(5)           /* ABC Category                            */~
        /*  CH(1)           /* Source Flag                             */~
        /*  CH(141)         /* Filler                                  */~

L35310: FMT                 /* FILE: HNYPITKT                          */~
            CH(2),          /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
            BI(1),          /* A number decremented for each recount   */~
                            /* (99=original                            */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
            CH(1),          /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
            CH(1),          /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
            CH(20),         /* Name of person who counted something    */~
            CH(3),          /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
            CH(6),          /* Date something was counted              */~
            CH(6),          /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
            CH(6),          /* The System Time when a transaction was  */~
                            /* entered                                 */~
            POS(113),       /* Position for Field QTY-COUNTED          */~
            PD(14,4),       /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
            POS(201),       /* Quantity From HNYLOCNS file (if         */~
            PD(14,4),       /* applicable)                             */~
                            /*                                         */~
            12*PD(14,4),    /* 12 cost buckets         User            */~
            PD(14,4)        /*  total Cost              Overrides      */

L36130: FMT                 /* FILE: HNYPICST                          */~
            POS(47),        /* Position for Field ON-HAND              */~
            PD(14,4),       /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
            12*PD(14,4),    /* 12 cost buckets & total cost            */~
            PD(14,4)        /*         These are the frozen or         */~
                            /* 'snapshot' cost fields                  */

L36490: FMT                 /* FILE: HNYPITKT                          */~
            CH(2),          /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
            BI(1),          /* A number decremented for each recount   */~
                            /* (99=original                            */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
            CH(1),          /* General Purpose Flag or Switch          */~
                            /* Indicator                               */~
                            /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
            CH(1),          /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
            CH(20),         /* Name of person who counted something    */~
            CH(3),          /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
            CH(6),          /* Date something was counted              */~
            CH(6),          /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
            CH(6),          /* The System Time when a transaction was  */~
                            /* entered                                 */~
            PD(14,4),       /* Quantity counted of something           */~
                            /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
            5*PD(14,4),     /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
            5*PD(14,4),     /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
            PD(14,4),       /* Quantity of Something                   */~
                            /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
            12*PD(14,4),    /* 12 cost buckets & total cost            */~
            PD(14,4),       /*       Override entries - user supplied  */~
            CH(2),          /* Session Number                          */~
            CH(1),          /* Print Flag                              */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
            BI(1),          /* A number decremented for each recount   */~
            CH(164)         /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$ = "Report Selection Criteria Screen"
                  str(line2$,62%) = "HNYPIVAR: " & str(cms2v$,,8%)
                  if errormsg$ > " " then print at(1,1);bell
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40360 ,        /* Count Session Num*/~
                                    L40360 ,        /* Warehouse        */~
                                    L40360 ,        /* ABC CLasses      */~
                                    L40360 ,        /* Group By Part ?  */~
                                    L40360 ,        /* Detail Level     */~
                                    L40390 ,        /* Exclude by Qty % */~
                                    L40360 ,        /* Print Subtotals? */~
                                    L40360 ,        /* Sort Sequence    */~
                                    L40390 ,        /* Recount Variances*/~
                                    L40360 ,        /* Include Missing ?*/~
                                    L40360          /* Print Parameters?*/

                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40430

L40360:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40390:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40430:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Physical Inventory Variance Report",            ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
        "Count Session Number       :",                                  ~
               at (06,31), fac(lfac$( 1)), session_nbr$         , ch(02),~
               at (06,35), fac(hex(8c)),   description$         , ch(32),~
               at (07,02),                                               ~
        "Print ALL Whses or Only... :",                                  ~
               at (07,31), fac(lfac$( 2)), whse$                , ch(03),~
               at (07,35), fac(hex(8c)),   whsedescr$           , ch(32),~
               at (08,02),                                               ~
        "Print ALL ABC Class or Only:",                                  ~
               at (08,31), fac(lfac$( 3)), cba$                 , ch(04),~
               at (09,02),                                               ~
        "Group Tickets W/Same Part #:",                                  ~
               at (09,31), fac(lfac$( 4)), group$               , ch(03),~
               at (10,02),                                               ~
        "Detail Level to Print To...:",                                  ~
               at (10,31), fac(lfac$( 5)), detail_level$        , ch(01),~
               at (11,02),                                               ~
        "Exclude Items w/Variance% <:",                                  ~
               at (11,31), fac(lfac$( 6)), exclude_qty_pct$     , ch(03),~
               at (12,02),                                               ~
        "Print Subtotals ?          :",                                  ~
               at (12,31), fac(lfac$( 7)), subtotal$            , ch(01),~
               at (13,02),                                               ~
        "Sort into Sequence By...   :",                                  ~
               at (13,31), fac(lfac$( 8)), sortby$              , ch(01),~
               at (14,02),                                               ~
        "Generate Recount Tickets",                                      ~
               at (15,02),                                               ~
        "for Tickets over this Var% :",                                  ~
               at (15,31), fac(lfac$(09)), recounte$            , ch(03),~
               at (16,02),                                               ~
        "Include Uncounted Tickets ?:",                                  ~
               at (16,31), fac(lfac$(10)), include_missing$     , ch(03),~
               at (17,02),                                               ~
        "Print Session Parameters  ?:",                                  ~
               at (17,31), fac(lfac$(11)), parameters$          , ch(03),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), fac(hex(8c)), pf4$                   , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L41270
                  call "MANUAL" ("HNYPIVAR")
                  goto L40430

L41270:        if keyhit% <> 15 then L41310
                  call "PRNTSCRN"
                  goto L40430

L41310:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Page 1.                        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50280,         /* Count Session Num*/~
                                    L50350,         /* Store to Print or*/~
                                    L50410,         /* ABC Classes to Pr*/~
                                    L50530,         /* Group Tickets ?  */~
                                    L50570,         /* Print Detail Leve*/~
                                    L50630,         /* Exclude QTY Var %*/~
                                    L50700,         /* Print Subtotals ?*/~
                                    L50760,         /* Sort Sequence    */~
                                    L50810,         /* Generate Recount%*/~
                                    L50860,         /* Include Missing ?*/~
                                    L50900          /* Print Parameters?*/
                     return
L50280: REM Test Data for Count Session Number
                call "GETCODE" (#1, session_nbr$,description$     , 1%,  ~
                                0, f1%(1))
                if f1%(1) = 0% then errormsg$ =                          ~
                   "Session Undefined or None on File"                   ~
                else gosub dataload
                return
L50350: REM Test Data for Warehouse
                if whse$ = "ALL" then return
                call "GETCODE" (#4, whse$, whsedescr$, 1%, 0, f1%(4))
                if f1%(4) = 0% then errormsg$ =                          ~
                   "Warehouse / Store not on file"
                return
L50410: REM Test Data for ABC Classes
                if cba$ = "ALL" then return
                if abc$ = "ALL" then abc$ = " ABCX"
                for x% = 1% to len(cba$)
                    if pos(abc$ = str(cba$,x%,1%)) = 0% then errormsg$=  ~
                      "ABC Category Not Selected for this Session"
                next x%
                if abc$ = " ABCX" then abc$ = "ALL"
                return
                errormsg$="Must be ALL or a combination of the following"~
                          & ": " & abc$
                return
L50530: REM Test Data for Group Tickets by Part
                if group$ = "Y" then group$ = "YES"
                if group$ = "N" then group$ = "NO"
                if group$ = "YES" or group$ = "NO " then return
                errormsg$="Must be YES or NO"
                return
L50570: REM Test Data for Detail Level to Print
                if group$ <> "YES" then detail_level$ = "T"
                if detail_level$ = "L" and lot_or_loc$ = "P" then        ~
                   detail_level$ = "T"
                if detail_level$ = "T" or detail_level$ = "W" or         ~
                   detail_level$ = "P" or detail_level$ = "L" then return
                errormsg$ = "Must be P, W, L, or T"
                return
L50630: REM Test Data for Exclude Qty Var %
                call "NUMTEST" (exclude_qty_pct$,0,100,errormsg$,0,eqp)
                eqp = eqp / 100
                return
L50700: REM Test Data for Print Subtotals ?
                if group$ <> "YES" then subtotal$ = "N"
                if lot_or_loc$ = "P" and subtotal$ = "L" then L50751
                if lot_or_loc$ = "L" and subtotal$ = "N" then L50755
                if subtotal$ = "N" or subtotal$ = "W" or subtotal$ = "P" ~
                   or subtotal$ = "L" then return
                errormsg$ = "Must be L, W, P, or N"
                return
L50751:         subtotal$ = "W"
                errormsg$ = "Lot is the lowest level for this session, Su~
        ~btotaling by Lot is unnecessary."
                return
L50755:         subtotal$ = "L"
                errormsg$ = "Lot is the lowest level at which variance an~
        ~alysis is meaningful for this session"
                return
L50760: REM Test Data for Sort Sequence
                if sortby$ = "V" or sortby$ = "E" or sortby$ = "C" or    ~
                   sortby$ = "B" or sortby$ = "P" then return
                errormsg$ = "Must be V, E, C, B or P"
                return
L50810: REM Test Data for Recount Percentages
                call "NUMTEST" (recounte$,0,999,errormsg$,0,rcte)
                return
L50860: REM Test Data for Include Missing Tickets ?
                if include_missing$ = "Y" then include_missing$ = "YES"
                if include_missing$ = "N" then include_missing$ = "NO "
                if include_missing$ = "YES" or include_missing$ = "NO "  ~
                   then return
                errormsg$="Must be YES or NO"
                return
L50900: REM Test Data for Print Session Paramters ?
                if parameters$ = "Y" then parameters$ = "YES"
                if parameters$ = "N" then parameters$ = "NO "
                if parameters$ = "YES" or parameters$ = "NO " then return
                errormsg$="Must be YES or NO"
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end
