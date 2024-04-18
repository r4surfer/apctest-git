        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD   EEEEE  DDDD   U   U   CCC    *~
            *  P   P  R   R  L      D   D  E      D   D  U   U  C   C   *~
            *  PPPP   RRRR   L      D   D  EEEE   D   D  U   U  C       *~
            *  P      R   R  L      D   D  E      D   D  U   U  C   C   *~
            *  P      R   R  LLLLL  DDDD   EEEEE  DDDD    UUU    CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDEDUC - PRINTS THE DEDUCTION REPORT SORTED BY DEDUCTION*~
            *            DESCRIPTION, BY EMPLOYEE DEPARTMENT.           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/19/81 ! ORIGINAL                                 ! TEM *~
            * 08/04/81 ! MONTH END PROCESSING                     ! TEM *~
            * 06/25/86 ! CHANGED SELECTION TO DEDUCTION METHOD    ! SGA *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        com                                                              ~
            extlen%(15),                 /* EXTERNAL FIELD LENGTHS     */~
            field%(15),                  /* SELECTABLE FIELD LIST      */~
            format$(15)1,                /* DATA TYPE FORMAT CODES     */~
            from$(15)25,                 /* LOW RANGE DATA TEST ITEMS  */~
            fromnr(15),                  /* LOW RANGE NUMERIC TEST ITEM*/~
            length%(15),                 /* INTERNAL FIELD LENGTHS     */~
            position%(15),               /* POSITION IN REC (FROM 1)   */~
            prompt$(15)25,               /* FIELD NAME PROMPT          */~
            record$(12)250,              /* 3 RECORDS * 1000 CHARS EA. */~
            record%(15),                 /* WHICH OF 3 RECORDS IT'S IN */~
            to$(15)25,                   /* HI VALUE RANGE DATA TEST   */~
            tonr(15)                     /* HI RANGE NUMERIC RANGE TEST*/~

        dim                                                              ~
            accruals(4),                 /* CURR,MTD,QTD,YTD DEDUCTIONS*/~
            totaldept(4),                /* TOTAL PER DEPARTMENT       */~
            date$8,                      /* SCREEN FORMATTED DATE      */~
            ddr$12,                      /* DEDUCTION DESCRIPTION      */~
            ddr2$12,                     /* DEDUCTION DESCRIPTION 2    */~
            ddkey$15,                    /* DEDUCTION KEY              */~
            dept$4,                      /* DEPARTMENT CODE            */~
            dept2$4,                     /* DEPARTMENT CODE 2          */~
            total(4),                    /* DEDUCT TOTALS PER DEDUC    */~
            edtmessage$79,               /* EDIT MESSAGE TEXT          */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            linenumber%(3),              /* PRINT CONTROL VARIABLE     */~
            pgmid$18,                    /* Pgrm & Rev # for pass to   */~
                                         /*   SLCTSCRN.  Program in pos*/~
                                         /*   1 - 8 w/ : & space and   */~
                                         /*   release level in position*/~
                                         /*   11- 18 for display screen*/~
            prtdate$45,                  /* DATE FOR PRINTING          */~
            prtvar$(11)20,               /* PRINTING VARIABLE          */~
            readkey$50,                  /* FILE READ KEY              */~
            readkey1$50                  /* Plow key for Deduction File*/~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
            mat f2% = con

                     /* THE VARIABLE  F2%()            SHOULD NOT BE   */
                     /* MODIFIED.     IT  IS AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTION LINE ITEM FILE        *~
            * # 9 ! SORTWORK ! WORK FILE FOR SORT ROUTINE               *~
            *************************************************************

            select #4,  "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #5, "EMPDEDXN",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 400,                                    ~
                       keypos = 1, keylen = 15,                          ~
                       alt key 1, keypos = 16, keylen = 18, dup

            select #9, "SORTWORK",                                       ~
                       consec,                                           ~
                       recsize = 50


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files to Print Deduction Summary Rep~
        ~ort")

            call "OPENCHCK" (#4, 0%, f2%( 4),   0%, " ")
            call "OPENCHCK" (#5, 0%, f2%( 5),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *                                                           *~
            * HERE IS HOW THE PARAMETERS FOR THE SELECT (ALSO SORT) KEYS*~
            * WORK.  THE FIRST OF EACH DATA ITEM IS THE NAME OF THE KEY.*~
            * IT IS THE PROMPT THAT APPEARS ON THE SCREEN.              *~
            *      THE FIRST ITEM TO THE RIGHT OF THE PROMPT IS THE     *~
            * DATA TYPE.  THE DATA TYPE IS ONE OF THE FOLLOWING.        *~
            *                                                           *~
            *      U = UPPER CASE ALPHANUMERIC.                         *~
            *      L = UPPER/LOWER CASE ALPHANUMERIC.                   *~
            *      D = DATE-FORMATTED UPPER CASE FIELD.  MUST BE YYMMDD *~
            *      N = NUMERIC 8-BYTE FLOATING PT. ALL OTHER NUMERICS   *~
            *          SHOULD BE COMPARED ASCII, OR MODIFY THE ROUTINE. *~
            *                                                           *~
            * THE REMAINING NUMBERS IDENTIFY RESPECTIVELY THE LENGTH OF *~
            * THE FIELD ON THE DISK, THE NUMBER OF POSITIONS IT FILLS   *~
            * ON THE PAPER, WHICH OF THE UP-TO-3 RECORDS IN CORE THIS   *~
            * FIELD LIES IN, AND THE POSITION OF THE FIELD WITHIN THE   *~
            * RECORD.                                                   *~
            *                                                           *~
            * THE FIRST BLANK PROMPT NAME SIGNIFIES THE END OF THE      *~
            * LIST OF PROMPTS ON THE SYSTEM.                            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            pgmid$ = "PRLDEDUC: " & str(cms2v$,1,8)

            pageline% = 1000

            REM SET UP DATA FOR SELECT INTERPRETATION
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

                data "Deduction Method         ", "L", 06, 06, 2, 034,   ~
                     "Employee Department      ", "U", 04, 04, 1,  92,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001

L10000: REM *************************************************************~
            *    S U P E R   S E L E C T O R   R O U T I N E            *~
            *                                                           *~
            * USES ***SUPER SELECTOR*** ROUTINES TO SELECT AND SORT THE *~
            *         DESIRED RECORDS.                                  *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, from$(), to$()
            mat fromnr = zer: mat tonr = zer: mat field% = zer
            inpmessage$ = "'ALL', 'FIRST', and 'LAST' are valid Parameter~
        ~s"

            for temp% = 1 to 15
                if prompt$(temp%) <> " " then from$(temp%) = "ALL"
            next temp%

L10160:     call "SLCTSCRN" ("Print Deduction Summary Report", errormsg$,~
                           inpmessage$, keyhit%, pgmid$)
                  if keyhit%  =  1 then       L10000
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L10160
            call "SLCTTEST" (errormsg$, maxfields%)
                  if errormsg$ <> " " then L10160

            REM Now select the desired deduction records
                readkey$ = " "
                flag% = 0
                call "SHOSTAT" ("Selecting Deductions...")
                call "WORKOPEN" (#9, "OUTPT", 4000%, f2%(9))

            REM Plow through employee file.  disregard terminated ones
L10310:         call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                     if f1%(4) = 0 then L10530
                get #4, using L10340, str(record$(), 1, 136)
L10340:                 FMT CH(136)

                 REM Plow down deduction records
                     readkey1$ = str(record$(), 1, 12)
L10390:              call "PLOWNEXT" (#5, readkey1$, 12%, f1%(5))
                           if f1%(5) = 0 then L10310
                     get #5, using L10420, str(record$(), 1001, 200)
L10420:                      FMT CH(200)
                call "SLCTPASS" (maxfields%, select%)
                     if select% = 0 then L10390
                     write #9, using L10490, str(record$(), 1034, 06),    ~
                                            str(record$(),   92, 04),    ~
                                            str(record$(), 1001, 15),    ~
                                            " "
L10490:              FMT CH(06), CH(04), CH(15), CH(19)
                     flag% = 1
                     go to L10390

L10530:     REM Now sort the work file with the desired records
                if flag% = 0 then L65000
                call "SHOSTAT" ("Sorting Selected Deductions...")
                call "SLCTSORT" (#9, 16%)

                call "SHOSTAT" ("Printing Deduction Report...")
                select printer (134)

            REM Prime by reading first record
                read #9, using L10640, ddr$, dept$, ddkey$,               ~
                                      eod go to L65000
L10640:                  FMT CH(06), CH(04), CH(15)
L10650:         gosub L20000    /* PRINT A DESCRIPTION        */
                if finis% = 99 then L65000
                ddr$ = ddr2$
                dept$ = dept2$
                mat total = zer
                go to L10650

L20000: REM *************************************************************~
            * P R I N T   D E D U C T I O N S                           *~
            *                                                           *~
            * PRINTS THE DEDUCTION REPORT FOR ONE DEDUCTION DESCRIPTION.*~
            * CALL A PLOW ON THE CONSEC FILE.                           *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con
            init(" ") prtvar$()

            REM Loop through printing lines until done
L20150:         for column% = 1 to 2
                    on column% gosub L21000, L22000
                    next column%
                if colsdone% < 2 then L20220
                             pageline% = pageline% + 1
                             print using L50110
                             return
L20220:         gosub L60000
                print using L50150, prtvar$(1), prtvar$(2), prtvar$(3),   ~
                                   prtvar$(4), prtvar$(5), prtvar$(6)
                go to L20150

           REM **********************************************************
L21000:     REM *** Handles first  column--deduction description
                on linenumber%(1) gosub L21100, L21200
                   return

L21100:         REM Handles first  case--deduction description
                    prtvar$(1) = ddr$
                    linenumber%(1) = 2
                    return
L21200:         REM Handles second case--zap variables
                    prtvar$(1) = " "
                    linenumber%(1) = 3
                    colsdone% = colsdone% + 1
                    return

           REM **********************************************************
L22000:     REM *** Handles second column--deduction categories
                on linenumber%(2) gosub L22100, L22200, L22300, L22400
                   return

L22100:         REM Handles first  case--get and print figures
L22110:             gosub L40000
                    prtvar$(2) = dept$
                    for t% = 1 to 4
                       call "CONVERT"(totaldept(t%), 2.2,                ~
                                        str(prtvar$(t%+2), 1, 10))
                       total(t%) = round(total(t%) + totaldept(t%), 2)
                    next t%
                    linenumber%(2) = 2
                    return
L22200:         REM Handles second case--check if totals needed
                    if finis% = 99 then L22300
                    if ddr2$ <> ddr$ then L22300
                    dept$ = dept2$
                    go to L22110
L22300:         REM Handles third case---totals
                    prtvar$(2) = "TOTALS"
                    for t% = 1 to 4
                      call "CONVERT" (total(t%), 2.2,                    ~
                                         str(prtvar$(t%+2), 1, 10))
                    next t%
                    linenumber%(2) = 4
                    return
L22400:         REM Handles fourth case--zap variables
                    for t% = 2 to 6
                        prtvar$(t%) = " "
                    next t%
                    linenumber%(2) = 5
                    colsdone% = colsdone% + 1
                    return

L40000: REM *************************************************************~
            *       P L O W   N E X T   F O R   W O R K   F I L E       *~
            *                                                           *~
            * PLOWS DOWN WORK FILE RETRIEVING DEDUCTION TOTALS FOR A    *~
            * SINGLE DEPARTMENT WITHIN A DEDUCTION DESCRIPTION.         *~
            * ASSUMES THAT WE HAVE THE DEDUCTION DESCRIPTION, AND THE   *~
            * DEPARTMENT CODE, AND THAT WE HAVE ALREADY READ THE FIRST  *~
            * DEDUCTION KEY.                                            *~
            *************************************************************

            REM Initialize variables
                mat totaldept = zer

L40130:     REM Read deduction record and total
                call "READ100" (#5, ddkey$, f1%(5))
                     if f1%(5) = 0 then L40200
                get #5, using L40170, accruals()
L40170:                 FMT XX(116), 4*PD(14,4)
                for i% = 1 to 4
                    totaldept(i%) = round(totaldept(i%) + accruals(i%), 2)
                next i%

L40200:     REM Read next record
                read #9, using L40230, ddr2$, dept2$, ddkey$,             ~
                                      eod go to L40270
L40230:                  FMT CH(06), CH(4), CH(15)
                if ddr2$ = ddr$ and dept2$ = dept$ then L40130
                return

L40270:     REM No more records in work file
                finis% = 99
                return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING.                         *~
            *************************************************************

L50070: %PAGE ######         D E D U C T I O N   S U M M A R Y   R E P O ~
        ~R T                    ##########################################~
        ~###

L50110: %                       +-----------------------+------------+---~
        ~---------+-------------+-------------+-------------+


L50150: %                       !      ############     !   ######   ! ##~
        ~######## ! ##########  ! ##########  ! ##########  !


L50190: %                       ! DEDUCTION METHOD      ! DEPARTMENT !  C~
        ~URRENT   !     MTD     !     QTD     !     YTD     !

L50220: %                       !-----------------------!------------!---~
        ~---------!-------------!-------------!-------------!

L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************

            pageline% = pageline% + 1
            if pageline% < 60 then return
            if pagenumber% = 0 then L60110
               print using L50110
L60110:        print page
               call "DATE" addr ("HD", prtdate$)
               pagenumber% = pagenumber% + 1
               print using L50070, pagenumber%, prtdate$
               print
               print using L50110
               print using L50190
               print using L50220
               pageline% = 7
               return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            call "FILEBGON" (#9)
            end
