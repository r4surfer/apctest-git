        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y V   V   L      RRRR   PPPP   TTTTT   *~
            *  H   H  NN  N   Y Y  V   V   L      R   R  P    P   T     *~
            *  HHHHH  N N N    Y   V   V   L      RRRR   PPPP     T     *~
            *  H   H  N  NN    Y    V  V   L      R   R  P        T     *~
            *  H   H  N   N    Y     V     LLLLL  R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYVLRPT - PRINTS INVENTORY VALUATION REPORT, SORTED BY   *~
            *            PART NUMBER FOR A PARTICULAR INVENTORY TYPE.   *~
            *                                                           *~
            * EWDVLRPT - (EWD) Version of Report Uses the Old Format    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 11/22/85 ! ORIGINAL.                                ! SGA *~
            * 08/29/86 ! PRR# 4404 - FIXED TO INCLUDE LAST STORE/ ! LKM *~
            *          ! ACCOUNT IN TOTALING                            *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 05/13/87 ! Std Cost Changes (HNYMASTR/QUAN files).  ! ERN *~
            * 07/15/87 ! Brought subroutines CALLs up to date, etc! JIM *~
            * 05/25/88 ! Now totals last store printed            ! HES *~
            * 06/09/89 ! Corrected page break logic               ! MJB *~
            * 04/06/98 ! (EWD) Version of Inventory Evaluation Rpt! RHH *~
            *************************************************************~

        dim                                                              ~
            account$12,                  /* INVENTORY ASSET ACCOUNT    */~
            accountdesc$30,              /* ACCOUNT DESCRIPTION        */~
            blankline$79,                /* USED IN SCREEN FORMAT      */~
            date$45,                     /* NICELY FORMATTED DATE/TIME */~
            date1$8,                     /* MM/DD/YY FORMATED DATE     */~
            descr$32,                    /* DESCRIPTION                */~
            errormsg$79,                 /* ERROR MESSAGE FOR INPUT    */~
            firstpart$25,                /* FIRST PART NUMBER IN RANGE */~
            firststore$3,                /* FIRST STORE CODE IN RANGE  */~
            hdr$40,                      /* MESSAGE HEADER             */~
            hnycstcd$1,                  /* INVENTORY COST METHOD      */~
            last_account_printed$12,     /* LAST G/L ACCOUNT PRINTED   */~
            lastlot$6,                   /* LAST LOT PROCESSED         */~
            lastpart$25,                 /* LAST PART NUMBER IN RANGE  */~
            last_part_printed$25,        /* LAST PART NUMBER PRINTED   */~
            laststore$3,                 /* LAST STORE CODE IN RANGE   */~
            last_store_printed$3,        /* LAST STORE PRINTED         */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            linenumber%(5),              /* WHICH ITEM FOR THE 5 COLUMN*/~
            lot$6,                       /* LOT NUMBER                 */~
            mid$80,                      /* MIDDLE LINE (USUALLY -OR-) */~
            newreadkey$43,               /* READ KEY FOR WORK FILE     */~
            oldquankey$50,               /* USED IN PLOW ON HNYQUAN    */~
            partnr$25,                   /* THIS PART NUMBER           */~
            pf1$79,                      /* PF 1   KEY PROMPT          */~
            pf2$79,                      /* PF 2   KEY PROMPT          */~
            prtvar$(10)50,               /* HOLDING VARIABLES FOR PRINT*/~
            sortflag$1,                  /* ORDER TO SORT REPORT       */~
            storedesc$30,                /* STORE DESCRIPTION          */~
            storenr$3,                   /* STORE NUMBER               */~
            qty(5),                      /* QUANTITIES FROM DISK.      */~
            qtytype$1,                   /* TYPE OF INVENTORY QUANTITY */~
            reportdate$30,               /* REPORT DATE                */~
            time$08,                     /* TIME IN HOUR AND MINUTES   */~
            username$24                  /* USER NAME                  */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.  THEY ARE AN INTRINSIC PART OF THE   */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 2 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 3 ! HNYQUAN  ! INVENTORY STORE QUANTITY DETAIL FILE     *~
            * # 4 ! STORNAME ! STORE NAMES AND ADDRESS                  *~
            * # 5 ! WORK1    ! WORK FILE FOR INVENTORY SORT BY PART     *~
            * # 6 ! WORK2    ! WORK FILE FOR INVENTORY SORT BY G/L ACCT *~
            * # 7 ! GLMAIN   ! GENERAL LEDGER MASTER FILE               *~
            *************************************************************

            select  #2, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select #3,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #4, "STORNAME",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 3

            select #5, "WORK1",          /* WORK FILE FOR SORT BY PART */~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 34

            select #6, "WORK6",          /* WORK FILE FOR SORT BY ACCT */~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 43

            select #7, "GLMAIN",         /* GENERAL LEDGER MASTER FILE */~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#2, "SHARE", f2%(2), rslt$(2), axd$(2))
            call "OPENFILE" (#3, "SHARE", f2%(3), rslt$(3), axd$(3))
            call "OPENFILE" (#4, "SHARE", f2%(4), rslt$(4), axd$(4))
            call "OPENFILE" (#7, "SHARE", f2%(7), rslt$(7), axd$(7))

            if f2%(2) > 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * SETS DATES FOR SCREEN DISPLAY.                            *~
            *************************************************************
            date1$ = date
            call "DATEFMT" (date1$)
            wseq% = 1
            partsprinted%, pagenumber% = 0
            pageline% = 99%
            call "DATE" addr ("HD", date$)
            call "EXTRACT"  addr("NA", username$)
            reportdate$ = str(date$,13,20)

        REM *************************************************************~
            *         I N P U T   S E L E C T I O N S                   *~
            *                                                           *~
            * Input Selection & Sort Criteria.                          *~
            *************************************************************

        inputmode1
            init(" ") errormsg$, laststore$, lastpart$, blankline$,      ~
                      qtytype$, sortflag$
            firststore$ = "ALL"
            firstpart$ = "ALL"

            for fieldnr% = 1 to 6
                gosub'051(fieldnr%)
                  if enabled% = 0 then L09730
L09650:         gosub'101(fieldnr%)
                  if keyhit%  =  1 then       inputmode1
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L09650
                gosub'151(fieldnr%)
                  if errormsg$ <> " " then L09650
L09730:     next fieldnr%

            call "SHOSTAT" ("Selecting Inventory Quantity Records.")

        REM *************************************************************~
            *            E X T R A C T   S E C T I O N                  *~
            *-----------------------------------------------------------*~
            * Extract data for report based on input criteria.          *~
            *************************************************************
        REM OPEN WORKFILE
            got_some% = 0% /* Got none */
            if sortflag$ = "1" then file% = 5% else file% = 6%
            call "WORKOPEN" (#file%, "IO", 1000%, f2%(file%))
        REM Set Range selections for read
            if firststore$ <> "ALL" then goto L15065
               init(hex(00)) firststore$
               init(hex(ff)) laststore$
L15065:     if firstpart$ <> "ALL" then goto L15100
               init(hex(00)) firstpart$
               init(hex(ff)) lastpart$

L15100: REM Initialize Lot Variables
            init(" ") lot$
            mat qty = zer  :  cost = 0
            oldquankey$ = firstpart$
            call "READ104" (#3, oldquankey$, f1%(3))
            goto L15180

        readloop
            call "READNEXT" (#3, f1%(3))
L15180:         if f1%(3) = 0 then print_report
            get #3, using L15210, partnr$, storenr$, lot$, qty(),         ~
                                 cost, account$, hnycstcd$
L15210:     FMT POS(17), CH(25), CH(3), CH(6), POS(69), 5*PD(14,4),      ~
                         POS(117), PD(14,4), POS(259), CH(9),            ~
                         POS(403), CH(1)
            if partnr$ > lastpart$ then print_report
            if storenr$ < firststore$ or                                 ~
                          storenr$ > laststore$ then readloop
            gosub write_to_workfile
            goto readloop

        REM *************************************************************~
            *                  PRINT REPORT SECTION                     *~
            *                                                           *~
            *************************************************************

        print_report
            if got_some% <> 0% then goto L15357
                call "ASKUSER" (got_some%, "*** NULL SET SELECTED ***",  ~
                     "There are no records in the criteria you selected",~
                     " ", "Press (RETURN) to acknowledge and continue")
                call "FILEBGON" (#file%)
                goto inputmode1
L15357:     select printer(134)
            call "SHOSTAT" ("Printing Inventory Valuation Report.")
            partsprinted%, pagenumber% = 0
            call "TIME" (time$)
            str(reportdate$,21) = time$
            init (hex(00)) newreadkey$
            if file% = 6 then L15518

        REM *************************************************************~
            *  ROUTINE FOR THE SELECTION OF INFORMATION FROM THE WORK   *~
            *  FILE FOR INFORMATION IN ORDER OF STORE                   *~
            *************************************************************

            call "READ102" (#5, newreadkey$, f1%(5))
               if f1%(5) = 0 then L19000
               goto L15435
L15425:     call "READNEXT" (#5, f1%(5))
               if f1%(5) = 0 then L19000
L15435:     get #5, using L15480,  storenr$, partnr$, lot$, wseq%, descr$,~
                                  qty(1), qty(2), qty(3), qty(4), qty(5),~
                                  cost, hnycstcd$
            call "DESCRIBE" (#2, partnr$, descr$, 0%, f1%(3))
            call "DESCRIBE" (#4, storenr$, storedesc$, 0%, f1%(4))
            if qtytype$ = "1" and qty(1) = 0 then goto L15425
            if qtytype$ = "2" and qty(2) = 0 then goto L15425
            if qtytype$ = "3" and qty(3) = 0 then goto L15425
            if storenr$ = last_store_printed$ then L15479
                gosub print_part_totals
                gosub print_store_totals
                pageline% = 99%
                init (" ")  prtvar$(8), last_part_printed$
L15479:     goto L15651
L15480:           FMT   CH( 3),          /* STORE NUMBER               */~
                        CH(25),          /* PART NUMBER                */~
                        CH(6),           /* LOT NUMBER                 */~
                        BI(4),           /* WRITE SEQUENCE NUMBER      */~
                        CH(32),          /* PART DESCRIPTION           */~
                        5*PD(14,4),      /* 5 QUANTITIES               */~
                        PD(14,4),        /* COST                       */~
                        CH(1)            /* INVENTORY COSTING METHOD   */

        REM *************************************************************~
            *  ROUTINE FOR THE SELECTION OF INFORMATION FROM THE WORK   *~
            *  FILE FOR INFORMATION IN ORDER OF G/L ACCOUNT NUMBER      *~
            *************************************************************

L15518:     call "READ102" (#6, newreadkey$, f1%(6))
                goto L15540
               if f1%(6) = 0 then L19000
L15530:     call "READNEXT" (#6, f1%(6))
               if f1%(6) = 0 then L19000
L15540:     get #6, using L15565, account$, storenr$, partnr$,lot$, wseq%,~
                                  descr$, qty(1), qty(2), qty(3), qty(4),~
                                  qty(5), cost, hnycstcd$
            call "DESCRIBE" (#2, partnr$, descr$, 0%, f1%(3))
            call "DESCRIBE" (#4, storenr$, storedesc$, 0%, f1%(4))
            call "DESCRIBE" (#7, account$, accountdesc$, 0%, f1%(7))
            call "GLFMT" (account$)
            goto L15610

L15565:           FMT   CH( 9),          /* Inventory Account Number   */~
                        CH( 3),          /* Store Number               */~
                        CH(25),          /* Part Number                */~
                        CH(6),           /* Lot Number                 */~
                        BI(4),           /* Write Sequence Number      */~
                        CH(32),          /* Part Description           */~
                        5*PD(14,4),      /* 5 Quantities               */~
                        PD(14,4),        /* Cost                       */~
                        CH(1)            /* Inventory Costing Method   */

L15610:     if qtytype$ = "1" and qty(1) = 0 then goto L15530
            if qtytype$ = "2" and qty(2) = 0 then goto L15530
            if qtytype$ = "3" and qty(3) = 0 then goto L15530
            if account$ = last_account_printed$ then L15651
               gosub print_part_totals
               gosub print_account_totals
               pageline% = 99%
               partsprinted% = 0
               init (" ")  prtvar$(8), last_part_printed$
            gosub print_page_heading

L15651: REM Print Part And Lot Information
            mat linenumber% = con
            if partnr$ = prtvar$(8) then L15715
            if partnr$ <> last_part_printed$ then gosub print_part_totals
               prtvar$(8) = partnr$
               prtvar$(9) = descr$
            if pageline% > 54% then gosub print_page_heading
            print using L50350, prtvar$(8), prtvar$(9)
            pageline% = pageline% + 1
L15715:     lotsprinted% = lotsprinted% + 1
            lastlot$ = lot$
            last_store_printed$ = storenr$
            last_account_printed$ = account$

        REM Loop Through Computing And Printing Lines Until Done.
            for column% = 1 to 5
                on column% gosub L16000, L16500, L17000, L18000, L18500
            next column%
            print using L50300, prtvar$(1), prtvar$(2), prtvar$(3),       ~
                               prtvar$(4), prtvar$(5), prtvar$(6),       ~
                               prtvar$(7)
            pageline% = pageline% + 1%
            total_part_qty = total_part_qty + totalqty
            total_part_cost = total_part_cost + totalcost
            if file% = 6 then L15530
            goto L15425              /* Next Record      */

        print_part_totals
            if lotsprinted% = 0 then return
            if pageline% > 56% then gosub print_page_heading
            if lotsprinted% = 1 then L15890
            if total_part_qty = 0 then L15890
                call "CONVERT" (total_part_qty, 2.2, total_part_qty$)
                call "CONVERT" (total_part_cost, 2.4, total_part_cost$)
                print using L50600, total_part_qty$, total_part_cost$
                pageline% = pageline% + 1%
L15890:     partsprinted% = partsprinted% + 1
            print using L50200
            total_cat_cost = total_cat_cost + total_part_cost
            total_part_qty = 0
            total_part_cost = 0
            lotsprinted% = 0
            last_part_printed$ = partnr$
            pageline% = pageline% + 1%
            return

L16000: REM +---------------------------------------------------+        ~
            !         Handles Column 1 And Column 2             !        ~
            !       Prints Part#, Description And Lot#.         !        ~
            +---------------------------------------------------+

            on linenumber%(1) gosub L16080
            return

L16080: REM Print Lot Number
            prtvar$(1) = "  LOT NUMBER:" & lot$
            prtvar$(2) = " "
            linenumber%(1) = 2
            return

L16500: REM +---------------------------------------------------+        ~
            ! Handles The Third Column.  Prints Inventory Cost  !        ~
            ! Methods.                                          !        ~
            +---------------------------------------------------+

            on linenumber%(2) gosub L16580
            return

L16580: REM Print The Inventory Cost Method
            if hnycstcd$="R" then prtvar$(3) = "ACTUAL LIFO"
            if hnycstcd$="X" then prtvar$(3) = "ACTUAL LIFO/ADJ ACT"
            if hnycstcd$="A" then prtvar$(3) = "AVERAGE COST"
            if hnycstcd$="S" then prtvar$(3) = "STANDARD LIFO"
            if hnycstcd$="F" then prtvar$(3) = "FIXED STANDARD"
            if hnycstcd$="L" then prtvar$(3) = "LAST COST"
            if hnycstcd$="M" then prtvar$(3) = "MANUAL COST"
            if hnycstcd$="T" then prtvar$(3) = "STANDARD FIFO"
            if hnycstcd$="P" then prtvar$(3) = "ACTUAL FIFO"
            if hnycstcd$="Y" then prtvar$(3) = "ACTUAL FIFO/ADJ ACT"
            if hnycstcd$=" " then prtvar$(3) = "NO SELECTION"
            linenumber%(2) = 2
            return

L17000: REM +---------------------------------------------------+        ~
            ! Handles The Fourth Column.  Prints Inventory      !        ~
            ! Quantities.                                       !        ~
            +---------------------------------------------------+

            on linenumber%(3) gosub L17080
            return

L17080: REM Print Quantity Based On Quantity Type Selected
            totalqty = 0
            if qtytype$ = "1" then goto L17160
                if qtytype$ = "2" then goto L17230
                    if qtytype$ = "3" then goto L17300

L17160: REM Print On Hand Quantities
            prtvar$(4) = "O.H.:"
            totalqty = totalqty + qty(1)
            call "CONVERT" (qty(1), 2.2, str(prtvar$(5),,10))
            linenumber%(3) = 2
            return

L17230: REM Print Backorderd Quantities
            prtvar$(4) = "BO'D:"
            totalqty = totalqty + qty(2)
            call "CONVERT" (qty(2), 2.2, str(prtvar$(5),,10))
            linenumber%(3) = 2
            return

L17300: REM Print On Order Quantities
            prtvar$(4) = "ONORD:"
            totalqty = totalqty + qty(3)
            call "CONVERT" (qty(3), 2.2, str(prtvar$(5),,10))
            linenumber%(3) = 2
            return

L18000: REM +---------------------------------------------------+        ~
            ! Prints The Fifth Column Showing The Average Unit  !        ~
            ! Cost As Well As The Total Avg Cost For The Part.  !        ~
            +---------------------------------------------------+

            on linenumber%(4) gosub  L18090
            return

L18090: REM Print Total Cost, Even If Zero.
            totalcost = 0
            totalcost = totalcost + cost
            call "CONVERT" (totalcost, 2.4, str(prtvar$(6),,14))
            linenumber%(4) = 2
            return

L18500: REM +--------------------------------------------------+         ~
            ! Print The Sixth Column Which Reflects The Total  !         ~
            ! Cost For Each Part/Lot Quantity.                 !         ~
            +--------------------------------------------------+
            on linenumber%(5) gosub L18570
            return

L18570: REM Now For Total Cost
            if qtytype$ = "1" then totalcost = round(totalcost*qty(1),2)
            if qtytype$ = "2" then totalcost = round(totalcost*qty(2),2)
            if qtytype$ = "3" then totalcost = round(totalcost*qty(3),2)

            call "CONVERT" (totalcost, 2.4, str(prtvar$(7),,16))
            linenumber%(5) = 2
            return

L19000: REM *************************************************************~
            *    Prints Number Of Parts Printed, Total Cost For All     *~
            *    Parts Printed And Exits Program If End Of Print.       *~
            *************************************************************
        REM PRINT_GRAND_TOTALS
            if gpartsprinted% = 0 and partsprinted% = 0 then L19030
                gosub print_part_totals
                total_cat_cost = total_cat_cost + total_part_cost
                if file% = 6 then L19025
                    print using L50200
                    pageline% = pageline% + 1%
                    gosub print_store_totals
                    goto L19030
L19025:         print using L50200
                pageline% = pageline% + 1%
                gosub print_account_totals
L19030:     if gpartsprinted% = 0 then L19045
                if pageline% > 52% then gosub print_page_heading
                print
                print using L50410, gpartsprinted%, grand_total
                gpartsprinted%, grand_total = 0
L19045:     close #file%
            call "FILEBGON" (#file%)
            close printer
            pageline% = 99%
            goto inputmode1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0%
                  on fieldnr% gosub L20100,      /* FIRST STORE NUMBER  */~
                                    L20120,      /* LAST STORE NUMBER   */~
                                    L20150,      /* FIRST PART NUMBER   */~
                                    L20180,      /* LAST PART NUMBER    */~
                                    L20210,      /* QUANTITY TYPE       */~
                                    L20240       /* PRINT ORDER         */
                     return
L20100:     REM DEFAULT/ENABLE FOR FIRST STORE NUMBER
                enabled% = 1%
                return
L20120:     REM DEFAULT/ENABLE FOR LAST PART NUMBER
                enabled% = 1%
                return
L20150:     REM DEFAULT/ENABLE FOR FIRST PART NUMBER
                enabled% = 1%
                return
L20180:     REM DEFAULT/ENABLE FOR LAST PART NUMBER
                enabled% = 1%
                return
L20210:     REM DEFAULT/ENABLE FOR QUANTITY TYPE
                enabled% = 1%
                return
L20240:     REM DEFAULT/ENABLE FOR PRINT ORDER
                enabled% = 1%
                return

        REM *************************************************************~
            * ROUTINE TO PRINT TOTALS FOR INVENTORY VALUATION REPORT BY *~
            * STORE OR INVENTORY ACCOUNT NUMBER                         *~
            **************************************************************
        print_store_totals
            if partsprinted% = 0 then return
            if pageline% > 54% then gosub print_page_heading
            print using L50400, last_store_printed$, partsprinted%,       ~
                               total_cat_cost
            pageline% = pageline% + 1%
            gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            total_cat_cost = 0
            partsprinted% = 0
            return

        print_account_totals
            if partsprinted% = 0 then return
            if pageline% > 54% then gosub print_page_heading
            print using L50405, last_account_printed$, partsprinted%,     ~
                               total_cat_cost
            gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            pageline% = pageline% + 1%
            total_cat_cost = 0
            partsprinted% = 0
            return

        REM *************************************************************~
            *    ROUTINE FOR WRITING THE INFORMATION TO THE APPROPRIATE *~
            *    WORK FILE.                                             *~
            *************************************************************

        write_to_workfile
            got_some% = 1%  /* Got some */
            if file% = 6 then goto L33500

            write #5, using L33120,storenr$, partnr$, lot$, wseq%, descr$,~
                                  qty(1), qty(2), qty(3), qty(4), qty(5),~
                                  cost, hnycstcd$
               wseq% = wseq% + 1
               return

L33120:           FMT   CH( 3),          /* STORE NUMBER               */~
                        CH(25),          /* PART NUMBER                */~
                        CH(6),           /* LOT NUMBER                 */~
                        BI(4),           /* WRITE SEQUENCE NUMBER      */~
                        CH(32),          /* PART DESCRIPTION           */~
                        5*PD(14,4),      /* 5 QUANTITIES               */~
                        PD(14,4),        /* UNIT COST                  */~
                        CH(1)            /* INVENTORY COSTING METHOD   */


L33500:     write #6, using L33800,account$,storenr$,partnr$,lot$, wseq%, ~
                                  descr$, qty(1), qty(2), qty(3), qty(4),~
                                  qty(5), cost, hnycstcd$

               wseq% = wseq% + 1
               return

L33800:           FMT   CH( 9),          /* INVENTORY ACCOUNT NUMBER   */~
                        CH( 3),          /* STORE NUMBER               */~
                        CH(25),          /* PART NUMBER                */~
                        CH(6),           /* LOT NUMBER                 */~
                        BI(4),           /* WRITE SEQUENCE NUMBER      */~
                        CH(32),          /* PART DESCRIPTION           */~
                        5*PD(14,4),      /* 5 QUANTITIES               */~
                        PD(14,4),        /* COSTS                      */~
                        CH(1)            /* INVENTORY COSTING METHOD   */

        REM *************************************************************~
            *         D E F I N E   S T O R E                           *~
            *                                                           *~
            * PERMITS THE OPERATOR TO ENTER A STORE TO PRINT LOT INFO   *~
            * FOR.  IF BLANK IS ENTERED, AND THIS STORE (IE THE BLANK)  *~
            * IS NOT ON FILE, THEN ONLY THE MASTER INFO IS PRINTED      *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40130,       /* FIRST STORE NUMBER */~
                                    L40130,       /* LAST STORE NUMBER  */~
                                    L40130,       /* FIRST PART NUMBER  */~
                                    L40130,       /* LAST PART NUMBER   */~
                                    L40140,       /* QUANTITY TYPE      */~
                                    L40140        /* PRINT ORDER        */
                     goto L40170

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40140:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40170:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Inventory Valuation Report",                    ~
               at (02,02),                                               ~
                  "Date:",                                               ~
               at (02,09), fac(hex(8c)), date1$                 , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Input Store Number Range:",                           ~
               at (05,38),                                               ~
                  "Input Desired Range of Parts:",                       ~
               at (06,02),                                               ~
                  "-------------------------",                           ~
               at (06,38),                                               ~
                  "-----------------------------",                       ~
               at (07,04),                                               ~
                  "First Store",                                         ~
               at (07,19), fac(lfac$( 1)), firststore$          , ch(03),~
               at (07,40),                                               ~
                  "First Part",                                          ~
               at (07,54), fac(lfac$( 3)), firstpart$           , ch(25),~
               at (08,04),                                               ~
                  "Last Store",                                          ~
               at (08,19), fac(lfac$( 2)), laststore$            ,ch(03),~
               at (08,40),                                               ~
                  "Last Part",                                           ~
               at (08,54), fac(lfac$( 4)), lastpart$             ,ch(25),~
               at (11,02),                                               ~
                  "Select Quantity Type:",                               ~
               at (11,38),                                               ~
                  "Select Print Order:",                                 ~
               at (12,02),                                               ~
                  "---------------------",                               ~
               at (12,38),                                               ~
                  "-------------------",                                 ~
               at (13,04),                                               ~
                  "Quantity Type",                                       ~
               at (13,19), fac(lfac$( 5)), qtytype$             , ch(1), ~
               at (13,42),                                               ~
                  "Print Order",                                         ~
               at (13,60), fac(lfac$( 6)), sortflag$            , ch(1), ~
               at (15,05),                                               ~
                  "1. On Hand",                                          ~
               at (15,43),                                               ~
                  "1.  By Store Number",                                 ~
               at (16,05),                                               ~
                  "2. Back Ordered",                                     ~
               at (16,43),                                               ~
                  "2.  By Inventory Account Number",                     ~
               at (17,05),                                               ~
                  "3. On Order",                                         ~
               at (22,02), fac(hex(ac)), blankline$             , ch(79),~
               at (23,02),                                               ~
                  "(1) Start Over",                                      ~
               at (23,20),                                               ~
                  "(13) Instructions",                                   ~
               at (23,60),                                               ~
                  "(15) Print Screen",                                   ~
               at (24,60),                                               ~
                  "(16) Exit Program",                                   ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L40460
                call "MANUAL" ("HNYVLRPT")
                goto L40170

L40460:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * TESTS FOR STORE ON FILE.                                  *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L45100,       /* FIRST STORE NUMBER */~
                                    L45200,       /* LAST STORE NUMBER  */~
                                    L45300,       /* FIRST PART NUMBER  */~
                                    L45400,       /* LAST PART NUMBER   */~
                                    L45500,       /* QUANTITY TYPE      */~
                                    L45600        /* PRINT ORDER        */
                   return

L45100: REM TEST DATA FOR STORE RANGE SELECTION
            errormsg$ = " "
            REM HANDLES CASE FOR "ALL" STORE SELECTION
                if firststore$ <> "ALL" then L45130
                   fieldnr% = 2
                   return
L45130:     REM HANDLES CASE FOR SINGLE STORE SELECTION
                call "GETCODE"(#4, firststore$, " ", 0%, 0, f1%(4))
                  if f1%(4) <> 0 then return
                  errormsg$ = "Invalid Store Number: " & firststore$
                  return
                  laststore$ = firststore$

L45200:     REM TESTS DATA FOR LAST STORE SELECTED.
                if laststore$ = firststore$ then return
                call "GETCODE"(#4, laststore$, " ", 0%, 0, f1%(4))
                  if f1%(4) <> 0 then L45245
                  errormsg$ = "Invalid Store Number: " & firststore$
                  return
                if laststore$ = " " then laststore$ = firststore$
                if laststore$ < firststore$ then L45400
                   firststore$ = firststore$ addc  all(hex(ff))
L45245:            return
            REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                errormsg$ = "Invalid Range!  Please Respecify."
                   return

L45300:     REM TEST DATA FOR PART NUMBER RANGE
                errormsg$ = " "
            REM HANDLES CASE FOR "ALL" PARTS
                if firstpart$ <> "ALL" then L45330
                   fieldnr% = 4
                   return
L45330:     REM HANDLES CASE FOR SINGLE PART
                call "GETCODE"(#2, firstpart$, " ", 0%, 0, f1%(2))
                  if f1%(2) <> 0 then return
                  errormsg$ = "Invalid Part Number: " & firstpart$
                  return
                  lastpart$ = firstpart$

L45400:     REM TESTS DATA FOR LAST PART SELECTED.
                if lastpart$ = firstpart$ then return
                call "GETCODE"(#2, lastpart$, " ", 0%, 0, f1%(2))
                  if f1%(2) <> 0 then L45435
                  errormsg$ = "Invalid Part Number: " & firstpart$
                  return
                if lastpart$ = " " then lastpart$ = firstpart$
L45435:         if lastpart$ < firstpart$ then L45450
                   return
L45450:     REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                errormsg$ = "Invalid Range!  Please Respecify."
                return

L45500: REM TEST DATA FOR QUANTITY TYPE SELECTED
            if qtytype$ = "1" or qtytype$ = "2" or                       ~
                                 qtytype$ = "3" then return
            errormsg$ = "Quantity Type Must Be '1', '2', '3'."
            return

L45600: REM TEST DATA FOR PRINT ORDER SELECTION
            if sortflag$ = "1" or sortflag$ = "2" then return
            errormsg$ = " Print Order Must Be '1' or '2'."
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Handles All The Print Formatting.                         *~
            *************************************************************

L50070: % Report Date: ############################                      ~
        ~                                                       Page #####

L50075: %                                                  INVENTORY VALU~
        ~ATION REPORT

L50080: %                                               IN ORDER OF PART ~
        ~NUMBER BY STORE                    Requested By: ################~
        ~####
L50085: %                                               =================~
        ~===============

L50090: %                                     IN ORDER OF PART NUMBER BY ~
        ~GENERAL LEDGER ACCOUNT NUMBER      Requested By: ################~
        ~####
L50095: %                                     ===========================~
        ~=============================

        %Page ######    I N V E N T O R Y   V A L U A T I O N   R E P O R~
        ~ T   B Y   W A R E H O U S E   ##################################~
        ~###

L50130: %                                            Store (###) - ######~
        ~#########################

L50140: %                                     Account Number (###########~
        ~#) - #############################

L50200: % +----------------------------!---------------------------------~
        ~+---------------+-----------------+--------------+---------------~
        ~-+

L50300: % !############################!#################################~
        ~!###############!###### ##########!##############!###############~
        ~#!

L50350: % !############################!#################################~
        ~!               !                 !              !               ~
        ~ !
L50400: % ********** Total Number Of Parts Printed For Store Number ###  ~
        ~is ########  For A Total Value of-#,###,###,###.##

L50405: % **********  Total Number Of Parts Printed For Account Number ##~
        ~########## is ########  For A Total Value of-#,###,###,###.##

L50410: % *****Grand Total Number Of Parts Printed For This Report is ###~
        ~#####  For A Total Value of-#,###,###,###.##

        %  Grand Total Of Cost Of Parts Printed is-##,###,###,###.##

L50500: %     P A R T    N U M B E R          D E S C R I P T I O N      ~
        ~   Cost Method    Unit Quantities   Avg Unit Cost   Total Cost   ~


L50600: % !                            !                                 ~
        ~!Total Quantity !#################!  Part Total  !###############~
        ~#!


        REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************
        print_page_heading
            print page
            call "TIME" (time$)
            pagenumber% = pagenumber% + 1
            print using L50070, reportdate$, pagenumber%
            print using L50075
            if file% = 6 then L60380
                call "READ100" (#4, storenr$, f1%(4))
                if f1%(4) = 1 then L60300
                    return% = 2
                    hdr$ = "INFORMATION ERROR"
                    pf1$ = "Store Number " & storenr$
                    str(pf1$,17,41) = "For Part Number " & partnr$
                    str(pf1$,59,15) = "Is Not Valid..."
                    pf2$ = "Press (Return) To Acknowledge"
                    mid$ = "No file exists for this Store."
                    call "ASKUSER" (return%, hdr$, pf1$, mid$, pf2$)
                    goto inputmode1
L60300:         get #4 using L60310, storenr$, storedesc$
L60310:             FMT CH(3), CH(30)
                print using L50080, username$
                print using L50085
                print using L50130, storenr$, storedesc$
                print
                last_store_printed$ = storenr$
                goto L60580
L60380:     call "GLUNFMT" (account$)
            call "READ100" (#7, account$, f1%(7))
            if f1%(7) = 1 then L60500
                call "GLFMT" (account$)
                return% = 2
                hdr$ = "INFORMATION ERROR"
                pf1$ = "Acct. Number " & account$
                str(pf1$,25,41) = "For Part Number " & partnr$
                str(pf1$,66,12) = "Is Not Valid."
                pf2$ = "Press (Return) To Acknowledge"
                mid$ = "No file exists for this General Ledger Account."
                call "ASKUSER" (return%, hdr$, pf1$, mid$, pf2$)
                goto inputmode1
L60500:     get #7 using L60510, account$, accountdesc$
L60510:         FMT CH(9), CH(30)
            call "GLFMT" (account$)
            print using L50090, username$
            print using L50095
            print using L50140, account$, accountdesc$
            print
            last_account_printed$ = account$
L60580:     print using L50500
            print using L50200

            pageline% = 8%
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS MESSAGE *~
            * (ONLY IN FOREGROUND) WHILE WE LINK BACK TO THE MENU.      *~
            *************************************************************

             call "SHOSTAT" ("One Moment Please")
             end
