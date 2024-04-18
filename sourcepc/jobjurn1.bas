        REM *************************************************************~
            *                                                           *~
            *  JJJJJ   OOO   BBBB   JJJJJ  U   U  RRRR   N   N    1     *~
            *    J    O   O  B   B    J    U   U  R   R  NN  N   11     *~
            *    J    O   O  BBBB     J    U   U  RRRR   N N N    1     *~
            *  J J    O   O  B   B  J J    U   U  R   R  N  NN    1     *~
            *   J      OOO   BBBB    J      UUU   R   R  N   N  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JOBJURN1 - THIS PROGRAM PRINTS THE JOB MASTER REPORT.     *~
            *            ALL THE DATA ON THE JOB MASTER FILE IS PRINTED *~
            *            ON THIS REPORT.                                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/04/81 ! ORIGINAL                                 ! TOM *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 11/05/86 ! Modified for Revenue/Mega Project        ! MJB *~
            * 03/28/88 ! PRR-6090 JOB range was not working       ! TLJ *~
            * 06/07/88 ! ADDL FIXES FOR MISC PROBS, HEADINGS, ETC ! BPN *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************
        dim                                                              ~
            acct$(4)9,                   /* GENERAL LEDGER DIST ACCTS  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* USED IN SCREEN FORMAT      */~
            column1$20,                  /* FIRST COLUMN TO PRINT      */~
            column2$40,                  /* SECOND COLUMN TO PRINT     */~
            column3$20,                  /* THIRD COLUMN TO PRINT      */~
            column4$20,                  /* FOURTH COLUMN TO PRINT     */~
            column5$30,                  /* FIFTH COLUMN TO PRINT      */~
            cusdescr$30,                 /* CUSTOMER DESCRIPTION       */~
            date$45,                     /* NICELY FORMATTED DATE/TIME */~
            date1$8,                     /* MM/DD/YY FORMATED DATE     */~
            descr$32,                    /* DESCRIPTION                */~
            errormsg$79,                 /* ERROR MESSAGE FOR INPUT    */~
            firstjobnr$8,                /* FIRST JOB  NUMBER IN RANGE */~
            first$8,                     /* FIRST JOB  NUMBER IN RANGE */~
            hours$8,                     /* WORK HOURS                 */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* Input message              */~
            jobsprint$8,                 /* WORK JOBS PRINT            */~
            lastjobnr$8,                 /* LAST JOB  NUMBER IN RANGE  */~
            last$8,                      /* LAST JOB  NUMBER IN RANGE  */~
            line2$79,                    /* Screen Line #2             */~
            linenumber%(6),              /* WHICH ITEM FOR THE 5 COLUMN*/~
            overdolls$8,                 /* OVERTIME DOLLARS           */~
            overhours$8,                 /* OVERTIME HOURS             */~
            partnr$25,                   /* THIS PART NUMBER           */~
            pf$16                        /* Print PF key with FAC      */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

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
            * # 2 ! JOBMASTR ! JOB MASTER FILE                          *~
            * # 3 ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            *************************************************************

            select  #2, "JOBMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 8

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENFILE" (#2, "SHARE", f2%(2), rslt$(2), axd$(2))
            call "OPENFILE" (#3, "SHARE", f2%(3), rslt$(3), axd$(3))

            if f2%(2) > 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SETS DATES FOR SCRREN DISPLAY, AND INVESTIGATES WHETHER   *~
            * PROGRAM RUNS IN FOREGROUND OR BACKGROUND.                 *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date1$ = date
            call "DATEFMT" (date1$)

            call "EXTRACT" addr("TT", tasktype$)

            str(line2$,62) = "JOBJURN1: " & str(cms2v$,1,8)
            inpmessage$ = "Enter JOB RANGE. Use '?' to view list. " &    ~
                                    "May specify 'ALL' 'FIRST' or 'LAST'"

            firstjobnr$ = "ALL"

        REM *************************************************************~
            *         P R I N T    Q U A N T I T I E S ?                *~
            *                                                           *~
            * THIS REPORT CAN BE RUN WITHOUT PRINTING THE COMBINED      *~
            * TOTALS FOR THE VARIOUS QUANTITIES ON FILE.  ITS FASTER... *~
            *************************************************************

            if tasktype$ = "B" then L65000

L10000: REM *************************************************************~
            *         I N P U T   R A N G E   T O   P R I N T           *~
            *                                                           *~
            * Inputs the range of jobs to print.                        *~
            *************************************************************

L10060:     init(" ") errormsg$, firstjobnr$, lastjobnr$, blankline$,    ~
                      first$, last$

L10090:     gosub L40000
L10100:           if keyhit%  =  0 then       L10200
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <> 14 then L10170
                     obtained% = 0%
                     gosub L45000
                     if errormsg$ <> " " then L10090
                     if obtained% = 0% then gosub generate_report        ~
                     else goto L10090
                     if errormsg$ = " " then goto L10060 else goto L10100
L10170:           if keyhit%  = 16 then       L65000
                  goto L10090

L10200:     gosub L45000
                  goto L10090

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * DOES ALL THE STUFF NECESSARY.  THIS IS HANDLED AS 5-COLUMN*~
            * PRINTOUT--                                                *~
            *                                                           *~
            *************************************************************
        generate_report:
            call "SHOSTAT" ("Printing Work in Process / Job Cost Report")
            jobsprinted%, pagenumber% = 0
            pageline% = 1000
            select printer(134)
            call "SETPRNT" ("JOB000", " ", 1%, 0%)

            REM PRIME THE GUN BY READING THE FIRST JOB NUMBER
                call "READ102" (#2, first$, f1%(2))
                     if f1%(2) = 0 then L19000
                        gosub L30000
                       go to L10480

L10430:     REM NOW GO PLODDING THROUGH FILE WITH READNEXTS
                 call "READNEXT" (#2, f1%(2))
                       if f1%(2) = 0 then L19000
                           gosub L30000

L10480:     REM CHECK TO SEE IF WE ARE OUT OF RANGE
                if jobnr$ > last$ then L10430   /**********/

            REM PRINT PART
                jobsprinted% = jobsprinted% + 1
                colsdone% = 0
                mat linenumber% = con
                if pageline% + 6 > 64 then pageline% = 1000
                               /* PAGE FEED IF NOT ENOUGH THIS PAGE.   */
                               /* (FOOLS LINE COUNT BY SAYING WE'RE AT */
                               /* WAY BEYOND END OF PAGE.              */

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L10610:         for column% = 1 to 5
                    on column% gosub L11000, L12000, L13000, L14000, L15000
                    next column%
                if colsdone% < 5 then L10680        /* IF NOT YET DONE  */
                           pageline% = pageline% + 1 /* FOR EXTRA LINE */
                           print using L50140       /* SEPARATOR LINE   */
                           goto L10430              /* NEXT PART        */
L10680:         gosub L60000              /* PAGE HEADING, IF NECCESSARY*/
                print using L50180, column1$, column2$, column3$,         ~
                            column4$, column5$



                goto L10610

L11000:     REM +---------------------------------------------------+    ~
                ! HANDLES COLUMN 1--JOB NUMBER, CUSTOMER CODE       !    ~
                !                                                   !    ~
                +---------------------------------------------------+

                on linenumber%(1) gosub L11070, L11130, L11200
                   return
L11070:            REM PRINT JOB NUMBER
                       column1$ = " "
                       column1$ = "JOB: "
                       str(column1$, 6) = jobnr$
                       linenumber%(1) = 2          /* SET NEXT CASE.   */
                       return
L11130:            REM PRINT COUSTOMER CODE
                       column1$ = " "
                       if cuscode$ = " " then L11200
                       column1$ = "CUS: "
                       str(column1$, 6) = cuscode$
                       linenumber%(1) = 3
                       return
L11200:            REM ZAP VARIABLES. DONE WITH THIS COLUMN
                       column1$ = " "
                       linenumber%(1) = 4
                       colsdone% = colsdone% + 1
                       return

L12000:     REM +---------------------------------------------------+    ~
                ! HANDLES THE SECOND COLUMN.  PRINTS JOB DESCRIPTION!    ~
                ! DATE OPENED, DATE CLOSED, CUSTOMER DESCRIPTION    !    ~
                !                                                   !    ~
                +---------------------------------------------------+

                on linenumber%(2) gosub L12090, L12150, L12210, L12280,      ~
                   L12360, L12490, L12570, L12440
                   return
L12090:            REM PRINT JOB DESCRIPTION FIRST.
                       column2$ = " "
                       if descr$ = " " then L12150
                       column2$ = descr$
                       linenumber%(2) = 2
                       return
L12150:            REM PRINT CUSTOMER DESCRIPTION NOW AND FOREVER
                       column2$ = " "
                       if cusdescr$ = " " then L12210
                       column2$ = cusdescr$
                       linenumber%(2) = 3
                       return
L12210:            REM NOW LETS PRINT THE PART NUMBER THIS JOB IS MAKING
                       column2$ = " "
                       if partnr$ = " " then L12280
                       column2$ = "PART NUMBER "
                       str(column2$, 14) = partnr$
                       linenumber%(2) = 4
                       return
L12280:            REM HANDLES DATE THE JOB WAS OPENED
                       column2$ = " "
                       if dateopened$ = " " or dateopened$ = blankdate$ ~
                                                             then L12360
                       call "DATEFMT" (dateopened$)
                       column2$ = "DATE OPENED "
                       str(column2$, 13) = dateopened$
                       linenumber%(2) = 5
                       return
L12360:            REM HANDLES THE DATE JOB WAS CLOSED
                       column2$ = " "
                       if dateclosed$ = " " or dateopened$ = blankdate$ ~
                                                             then  L12440
                       call "DATEFMT" (dateclosed$)
                       column2$ = "DATE CLOSED "
                       str(column2$, 13) = dateclosed$
                       linenumber%(2) = 6
                       return
L12440:         REM LETS GO HOME
                      column2$ = " "
                      colsdone% = colsdone% + 1
                      linenumber%(2) = 9
                      return
L12490:         REM LABOR AND PURCHASE EXPENSE ACCOUNTS
                    column2$ = " "
                    if acct$(1) = " " then L12530
                     column2$ = "LABOR EXP: " & acct$(1)
L12530:             if acct$(2) = " " then L12570
                     column2$ = column2$ & "  PURCHASE EXP: " & acct$(2)
                    linenumber%(2) = 7
                     return
L12570:         REM HNY (WIP) AND TRANSFER ACCT
                     column2$ = " "
                     if acct$(3) = " " then L12610
                     column2$ = "WIP ACCT: " & acct$(3)
L12610:              if acct$(4) = " " then L12440
                     column2$ = column2$ & " TRANSFER ACCT: " & acct$(4)
                     linenumber%(2) = 8
                     return
L13000:     REM +---------------------------------------------------+    ~
                ! HANDLES THE THIRD COLUMN OF REPORT WHICH INCLUDES !    ~
                ! THE QUANTITY TO MAKE FOR THE JOB AND THE QUANTITY !    ~
                ! COMPLETED TO DATE FOR THIS JOB                    !    ~
                +---------------------------------------------------+

                on linenumber%(3) gosub L13090, L13160, L13280, L13360, L13230
                   return

L13090:         REM HANDLE THE QUANTITY TO MAKE FOR THIS JOB
                    if quantity = 0 then L13160
                    column3$ = " "
                    column3$ = "TO MAKE "
                    call "CONVERT" (quantity, 2.2, str(column3$,10,10))
                    linenumber%(3) = 2
                    return
L13160:         REM HANDLE QUANTITY COMPLETED TO DATE FOR THIS JOB
                    column3$ = " "
                    if quancomplete = 0 then L13280
                    column3$ = "COMPLTD  "
                    call "CONVERT"(quancomplete, 2.2, str(column3$,10,10))
                    linenumber%(3) = 3
                    return
L13230:         REM WE ARE DONE WITH THIS COLUMN
                    column3$ = " "
                    colsdone% = colsdone% + 1
                    linenumber%(3) = 6
                    return
L13280:         REM NORMAL HOURS
                    column3$ = " "
                    call "CONVERT" (hours, 0.0, hours$)
                    if hours$ = "########" then hours = 0
                    if hours = 0 then L13360
                    totalhours = totalhours + hours
                    column3$ = "HOURS"
                    call "CONVERT" (hours, 2.2, str(column3$,10,10))
                    linenumber%(3) = 4
                    return
L13360:         REM OVERTIME HOURS
                    column3$ = " "
                    call "CONVERT" (overhours, 0.0, overhours$)
                    if overhours$ = "########" then overhours = 0
                    if overhours = 0 then L13230
                    totaloverhours = totaloverhours + overhours
                    column3$ = "OVERTIME"
                    call "CONVERT" (overhours, 2.2, str(column3$,10,10))
                    linenumber%(3) = 5
                    return

L14000:     REM +---------------------------------------------------+    ~
                ! HANDLES THE LABOR, PURCHASE, MATERIAL COSTS. TOTAL!    ~
                ! OF THE FOUR  COSTS AND THEN TOTAL COMMITTED COST. !    ~
                ! THIS IS SO COMMITTED COST IS NOT ADDED TO TOTAL   !    ~
                +---------------------------------------------------+

                on linenumber%(4) gosub                                  ~
                     L14110, L14200, L14290, L14380,                         ~
                               L14670, L14470, L14540, L14620
                   return

L14110:         REM LABOR COST (AS ALWAYS SKIPS IF ZERO)
                    column4$ = " "  : totalcost = 0
                    if labor  = 0 then L14200
                    column4$ = "LABOR: "
                    call "CONVERT" (labor, 2.2, str(column4$,11,10))
                    linenumber%(4) = 2
                    totalcost = totalcost + labor
                    totallabor = totallabor + labor
                    return
L14200:         REM HANDLES PURCHASES POSTED TO THIS JOB
                    column4$ = " "
                    if purchase = 0 then L14290
                    column4$ = "PURCHASE: "
                    call "CONVERT" (purchase, 2.2, str(column4$,11,10))
                    linenumber%(4) = 3
                    totalcost = totalcost + purchase
                    totalpur = totalpur + purchase
                    return
L14290:         REM HANDLES MATERIAL COST POSTED TO THIS JOB
                    column4$ = " "
                    if material = 0 then L14380
                    column4$ = "MATERIAL: "
                    call "CONVERT" (material, 2.2, str(column4$,11,10))
                    linenumber%(4) = 4
                    totalcost = totalcost + material
                    totalmat = totalmat + material
                    return
L14380:     REM HANDLES THE TOTAL OF LABOR, PURCHASE AND MATERIAL COSTS
                    column4$ = " "
                    if overhead = 0 then L14670
                    column4$ = "OVERHEAD: "
                    call "CONVERT" (overhead, 2.2, str(column4$,11,10))
                    linenumber%(4) = 5
                    totalcost = totalcost + overhead
                    totalhead = totalhead + overhead
                    return
L14470:     REM HANDLES THE TOTAL OF LABOR PURCHASES AND MATERIAL COSTS
                    column4$ = " "
                    if totalcost = 0 then L14540
                    column4$ = "TOTAL: "
                    call "CONVERT" (totalcost, 2.2, str(column4$,11,10))
                    linenumber%(4) = 7
                    return
L14540:         REM HANDLES THE TOTAL INVENTORY COMMITTED TO THIS JOB
                    column4$ = " "
                    if committed = 0 then L14620
                    column4$ = "COMMITTED:"
                    call "CONVERT" (committed, 2.2, str(column4$,11,10))
                    linenumber%(4) = 8
                    totalcom = totalcom + committed
                    return
L14620:         REM BLANKS OUT THE LINE, FINISHES COLUMN.
                    column4$ = " "
                    linenumber%(4) = 9
                    colsdone% = colsdone% + 1
                    return
L14670:         REM HANDLE OVERTIME DOLLARS
                    column4$ = " "
                    call "CONVERT" (overdolls, 0.0, overdolls$)
                    if overdolls$ = "########" then overdolls = 0
                    if overdolls = 0 then L14470
                    column4$ = "OVERTIME:"
                    call "CONVERT" (overdolls, 2.2, str(column4$,11,10))
                    linenumber%(4) = 6
                    totalcost = totalcost + overdolls
                    totaloverdolls = totaloverdolls + overdolls
                    return

L15000:     REM +--------------------------------------------------+     ~
                ! HANDLES THE DOLLARS TRANSFERRED TO INVENTORY,    !     ~
                ! TO COST OF SALES, AND TO MISCELLANEOUS           !     ~
                +--------------------------------------------------+

                on linenumber%(5) gosub L15090, L15180, L15270, L15360

                return

L15090:         REM HANDLES AMOUNT TRANFERRED TO INVENTORY
                 column5$ = " "
                 if trantohny = 0 then L15180
                    column5$ = "INV "
                    call "CONVERT" (trantohny, 2.2, str(column5$,6,10))
                    linenumber%(5) = 2
                    totaltohny = totaltohny + trantohny
                    return

L15180:         REM HANDLES AMOUNT TRANSFERRRED TO COST OF SALES
                 column5$ = " "
                 if trantosls = 0 then L15270
                    column5$ = "SALE "
                    call "CONVERT" (trantosls, 2.2, str(column5$,6,10))
                    linenumber%(5) = 3
                    totaltosls = totaltosls + trantosls
                    return

L15270:         REM HANDLES AMOUNT TO MISCELLANEOUS
                 column5$ = " "
                    if trantomisc = 0 then L15360
                    column5$ = "MISC "
                    call "CONVERT" (trantomisc, 2.2, str(column5$,6,10))
                    linenumber%(5) = 4
                    totaltomisc = totaltomisc + trantomisc
                    return

L15360:         REM HANDLES LETS GO HOME
                 column5$ = " "
                 colsdone% = colsdone% + 1
                 linenumber%(5) = 5
                 return

L19000: REM PRINTS NUMBER OF PARTS PRINTED AND EXITS PROGRAM.
            if jobsprinted% <> 0 then L19131
L19020:         u3% = 2%
                call "ASKUSER" (u3%, " ", "No report printed.",          ~
                                "No JOBs exist within the range.",       ~
                                "Press PF(1) to STARTOVER or RETURN "   &~
                                         "to EDIT criteria.")
                if u3% <> 1% and u3% <> 0% then L19020
                if u3% = 0% then L19100
                   firstjobnr$ = " " : lastjobnr$ = " "
L19100:         keyhit% = 0%
                errormsg$ = "No JOBs exist within the range."
                goto L19230

L19131:     jobsprinted = jobsprinted%
            call "CONVERT" (jobsprinted, -0.01, jobsprint$)
            print using L50220, jobsprint$
            print using L50290, totalhours + .005, totallabor + .005,     ~
                               totaloverhours + .005, totaloverdolls +   ~
                                                       .005
            print using L50320, totalpur + .005, totalmat + .005,         ~
                               totalhead + .005
            print using L50350, totalcom + .005, totaltohny + .005,       ~
                               totaltosls + .005
            print using L50390, totaltomisc + .005
L19230:     close printer
            call "SETPRNT" ("JOB000", " ", 0%, 1%)
            return

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
            goto L10000

L30000: REM *************************************************************~
            *             D I S K   R E A D   R O U T I N E             *~
            *                                                           *~
            * READS THE INFO OF THE PART IN THE DMS BUFFER AND RETURN.  *~
            *************************************************************

        REM GET THE JOB  INFORMATION
            get #2,  using L30170,                                        ~
                         jobnr$, descr$, dateopened$, dateclosed$,       ~
                         cuscode$, partnr$, quantity, quancomplete,      ~
                         labor, purchase, material, committed,           ~
                         trantohny, trantosls, trantomisc, overhead,     ~
                         str(acct$(), 1), hours, overhours, overdolls

             call "DESCRIBE" (#3, cuscode$, cusdescr$, 0%, f1%(3))
             return

L30170:              FMT CH(8),          /* JOB NMUBER                 */~
                         CH(30),         /* DESCRIPTION                */~
                         CH(6),          /* DATE OPENED                */~
                         CH(6),          /* DATE CLOSED                */~
                         CH(9),          /* CUSTOMER CODE              */~
                         CH(25),         /* PART TO BE MADE            */~
                         PD(14,4),       /* QUANTITY TO BE MADE        */~
                         PD(14,4),       /* QUANTITY COMPLETED TO DATE */~
                         PD(14,4),       /* TOTAL DOLLARS DIRECT LABOR */~
                         PD(14,4),       /* TOTAL DOLLARS DRCT PURCHASE*/~
                         PD(14,4),       /* TOTAL $ MATERIAL FROM HNY  */~
                         PD(14,4),       /* TOTAL $ COMMITTED HNY      */~
                         PD(14,4),       /* TOTAL $ TRANSFERRED TO HNY */~
                         PD(14,4),       /* TOTAL $ TRANSFERRED TO SLS */~
                         PD(14,4),       /* TOTAL $ TRANSFERRED TO MIS */~
                         XX(32),         /* SKIP SEQUENCE NUMBERS      */~
                         PD(14,4),       /* TOTAL $ OVERHEAD COSTS     */~
                         CH(36),         /* ACCOUNTS                   */~
                         3*PD(14,4)      /* HOURS, DOLLARS OVERTIME    */~

L40000: REM *************************************************************~
            *         D E F I N E   R A N G E   T O   P R I N T         *~
            *                                                           *~
            * Defines a range of jobs to report upon.                   *~
            *************************************************************

            str(line2$,1,50) = "Enter range of JOB numbers."
            pf$ = "(14)Print Report"

L40080:     accept                                                       ~
               at (01,02),                                               ~
                  "PRINT JOB MASTER REPORT",                             ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date1$                 , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
               at (07,02),                                               ~
                  "JOB Range",                                           ~
               at (07,30), fac(hex(81)), firstjobnr$            , ch(08),~
               at (07,40), "TO",                                         ~
               at (07,44), fac(hex(81)), lastjobnr$             , ch(08),~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over                      (13)Instructions", ~
               at (22,65), fac(hex(84)), pf$                    , ch(16),~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0e0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L40390
                call "MANUAL" ("JOBJURN1")
                goto L40080

L40390:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

L45000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * Test data for job numbers selected!                       *~
            *************************************************************
        REM Test for JOB                     FIRSTJOBNR$
            if str(firstjobnr$,1,1) <> "?" then L45110
                 errormsg$ = hex(06)&"Enter the 'FROM' JOB for the range."
                 readkey$ = " "
                 call "GETCODE" (#2, readkey$, errormsg$, 0%, 0, f1%(1))
                 if f1%(1) = 1% then firstjobnr$ = readkey$

L45110:     if str(lastjobnr$,1,1) <> "?" then L45170
                 errormsg$ = hex(06) & "Enter the 'TO' JOB for the range."
                 readkey$ = " "
                 call "GETCODE" (#2, readkey$, errormsg$, 0%, 0, f1%(1))
                 if f1%(1) = 1% then lastjobnr$ = readkey$

L45170:     if str(firstjobnr$,1,1)<> "?" and                            ~
                              str(lastjobnr$,1,1) <> "?" then L45220
                 errormsg$ = hex(00)
                 goto L45390

L45220:     errormsg$ = " "
            if firstjobnr$ = " " and lastjobnr$ <> " " then              ~
                   firstjobnr$ = "FIRST"
            if firstjobnr$ <> " " or lastjobnr$ <> " " then L45280
                 errormsg$ = "Enter a RANGE or an existing BOM."
                 goto L45390
L45280:     if lastjobnr$ <> " "  or errormsg$ <> " " or                 ~
                 firstjobnr$ = "ALL" or firstjobnr$ = "FIRST" then L45360
                 lastjobnr$ = firstjobnr$
                 call "GETCODE" (#2, firstjobnr$,errormsg$,0%,0,f1%(1))
                 if lastjobnr$ <> firstjobnr$ then obtained% = 1%
                 lastjobnr$ = " "
                 if f1%(1) = 1% then L45350
                    errormsg$ = "JOB does not exist.  Enter a RANGE or "&~
                                                      "an EXISTING JOB."
                    goto L45390
L45350:          errormsg$ = " "
L45360:     call "TESTRNGE" (firstjobnr$, lastjobnr$,                    ~
                             first$, last$, errormsg$)
            if firstjobnr$ = lastjobnr$ then lastjobnr$ = " "
L45390:     return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * Handles all the print formatting.                         *~
            *************************************************************

L50060: %PAGE ######         W O R K  I N  P R O C E S S  /  J O B  C O S~
        ~ T   R E P O R T      ###########################################~
        ~##

L50100: %  TO MAKE = QUANTITY TO BE PRODUCED ON THIS JOB     COMPLTD = QU~
        ~ANTITY COMPLETED TO DATE ON THIS JOB    SALE = COST OF SALES
L50120: %                  MISC = MISCELLANEOUS

L50140: % +--------------------+-----------------------------------------~
        ~+---------------------+---------------------+--------------------~
        ~--+

L50180: % !####################!#########################################~
        ~!#####################!#####################!####################~
        ~##!

L50220: % ********** TOTAL NUMBER OF JOBS PRINTED WAS  ########          ~

L50250: % !  JOB NUMBER        !        D E S C R I P T I O N            ~
        ~!     QUANTITIES      !       COSTS         ! AMOUNT TRANSFERRED ~
        ~  !

L50290: % TOTAL HOURS ###,###.##    TOTAL LABOR COSTS $###,###,###.##    ~
        ~TOTAL OVERTIME ###,###.##   TOTAL OVERTIME COSTS $###,###,###.##

L50320: % TOTAL PURCHASES $###,###,###.##     TOTAL MATERIAL $###,###,###~
        ~.##     TOTAL OVERHEAD COSTS $###,###,###.##

L50350: % TTL INVENTORY COMMITTED $###,###,###.##     TTL RETURNED TO INV~
        ~ENTORY $###,###,###.##     TTL TRANSFERED TO SALES $###,###,###.#~
        ~#

L50390: % TOTAL TRANSFERED TO OTHER JOBS $###,###,###.##

L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************

            pageline% = pageline% + 1
            if pageline% < 64 then return
               print page
               call "DATE" addr ("HD", date$)
               pagenumber% = pagenumber% + 1
               print using L50060, pagenumber%, date$
               print skip (1)
               print using L50100
               print using L50120
               print using L50140
               print using L50250
               print using L50140
               pageline% = 8
               return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS MESSAGE *~
            * (ONLY IN FOREGROUND) WHILE WE LINK BACK TO THE MENU.      *~
            *************************************************************

             call "SHOSTAT" ("Closing Files, One Moment Please")
        end
