        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   RRRR   JJJJJ  W   W  RRRR   K   K  PPPP   RRRR    *~
            *  P   P  R   R    J    W   W  R   R  K  K   P   P  R   R   *~
            *  PPPP   RRRR     J    W   W  RRRR   KKK    PPPP   RRRR    *~
            *  P      R   R  J J    W W W  R   R  K  K   P      R   R   *~
            *  P      R   R   J      W W   R   R  K   K  P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRJWRKPR - PRINTS THE LABOR DETAILS FOR THE JOB NUMBERS   *~
            *            SPECIFIED.  SORT IS BY Work Center BY JOB      *~
            *            NUMBER.  USES *SUPER SELECTOR* ROUTINES.       *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/07/81 ! ORIGINAL                                 ! TEM *~
            * 08/04/81 ! MONTH END PROCESSING                     ! TEM *~
            * 10/15/81 ! ADD OVERTIME TOTALS                      ! TEM *~
            * 06/13/91 ! Eliminated unused function FNX.          ! JDH *~
            * 07/15/91 !(NO PRR) Renamed JWRKPRNT to new name     ! RJB *~
            * 02/01/93 !PRR 12532 'Labor Class' now 'Work Center'.! JIM *~
            * 02/04/93 !QC Re-work to better standardize.         ! JIM *~
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
            dcost$10,                    /* DIRECT LABOR COST          */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            errormsg$79, inpmessage$79,                                  ~
            etype$12,                    /* EARNINGS TYPE              */~
            hdrdate$50,                  /* HEADER DATE FOR REPORT     */~
            jobnr$8,                     /* JOB NUMBER FROM DISK TO YOU*/~
            ocost$10,                    /* OVERHEAD COST              */~
            pgmid$18,                    /* PROGRAM & VERSION          */~
            prehrs$10,                   /* Formatted number           */~
            precst$10,                   /* Formatted number           */~
            rate$10,                     /* UNIT RATE                  */~
            readkey$50,                  /* READ RECORD WITH THIS      */~
            rptid$6,                     /* Report ID                  */~
            sortkey$50,                  /* KEY TO GO TO SORT FILE     */~
            totj1$10,                    /* Formatted number           */~
            totj2$10,                    /* Formatted number           */~
            totj3$10,                    /* Formatted number           */~
            totaljobdc$10,               /* Formatted number           */~
            totaljoboc$10,               /* Formatted number           */~
            udescr$6,                    /* UNITS DESCRIPTION          */~
            units$10,                    /* UNITS WORKED               */~
            wdate$8,                     /* DATE WORK PERFORMED        */~
            work$5,                      /* FOR COMPARISON TO FORM FEE */~
            wrkdescr$32                  /* Work Center  DESCRIPTION   */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *------+----------+-----------------------------------------*~
            *FILE #!  PRNAME  ! DESCRIPTION                             *~
            *------+----------+-----------------------------------------*~
            * #  3 ! WCMASTR  ! WORK CENTER DESCRIPTION                 *~
            * #  4 ! JOBLABOR ! JOB LABOR DETAIL FILE                   *~
            * #  9 ! WORKFILE ! WORK FILE FOR SELECTED RECORDS          *~
            *************************************************************

            select  # 3, "WCMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 5,                         ~
                         alt key 1, keypos = 1, keylen = 6

            select  # 4, "JOBLABOR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 16

            select # 9, "WORKFILE",                                      ~
                        consec,                                          ~
                        recsize = 100

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES MISCELLANEOUS INFORMATION NEEDED TO RUN THE   *~
            * PROGRAM.  ALSO INITIALIZES THE VARIABLES NEEDED TO USE THE*~
            * SELECTOR ROUTINE.                                         *~
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

            REM SET UP DATA FOR SELECT INTERPRETATION.
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

                data "Project Number           ", "U",  8,  8, 1, 001,   ~
                     "Work Center Code         ", "U",  4,  4, 1, 035,   ~
                     "Date Work Performed      ", "D",  6,  8, 1, 029,   ~
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

            pgmid$ = "PRJWRKPR: " & str(cms2v$,,8)
            rptid$ = "W/C004"

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * GETS THE ***SUPER SELECTOR*** INFORMATION FOR THE PROGRAM *~
            * WHICH WE WILL USE IN THE SELECT PASS, WHICH FOLLOWS.      *~
            *************************************************************

            totalwrkdc, totalwrkoc, totaljobdc, totaljoboc = 0
            totw1, totw2, totw3, totj1, totj2, totj3 = 0
            pageline% = 1000
            init(" ") errormsg$, from$(), to$()
            inpmessage$ = "Enter desired ranges of values, then press ("&~
                "RETURN) to process."
            mat fromnr = zer: mat tonr = zer: mat field% = zer

            for temp% = 1 to 15
                if prompt$(temp%) = " " then L10130
                   from$(temp%) = "ALL"
L10130:         next temp%

L10150:     call "SLCTSCRN" ("Work Center Labor Detail Report",errormsg$,~
                            inpmessage$, keyhit%, pgmid$)
                  if keyhit%  =  1 then       L10000
                  if keyhit%  = 16 then       L65080
                  if keyhit% <>  0 then       L10150
            call "SLCTTEST" (errormsg$, maxfields%)
                  if errormsg$ <> " " then L10150

        REM *************************************************************~
            *       P L O W   T H R O U G H   A N D   S E L E C T       *~
            *                                                           *~
            * PLOWS THROUGH THE LABOR DETAIL          RECORDS AND       *~
            * SELECTS ON THE FIELDS WITH THE VALUES WE HAVE SPECIFIED.  *~
            *************************************************************

            readkey$ = " "
            call "SHOSTAT"  ("Selecting Records... One Moment Please")
            call "FILEBGON" (#9) : f2%(9%) = 1%   /* Bye-bye, WORKFILE */
            call "WORKOPEN" (# 9, "OUTPT", 5000%, f2%(9))

L11110:     call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                 if f1%(4) = 0 then L11250          /* GO SORT WORKFILE */
            call "MOVERWA" addr(#4, str(record$(), 1))

            call "SLCTPASS" (maxfields%, select%)
            if select% = 0 then L11110          /* GET NEXT RECORD  */
               sortkey$ = " "
               str(sortkey$,  1,  4) = str(record$(), 35, 4)
               str(sortkey$,  5,  8) = str(record$(),  1, 8)
               str(sortkey$, 13,  6) = str(record$(), 29, 6)
               str(sortkey$, 19, 12) = str(record$(), 17,12)
               write #9, using L11220, sortkey$, str(readkey$, 1, 16)
L11220:                  FMT CH(64), CH(36)
               goto L11110

L11250:     REM NOW THAT SELECT ROUTINE HAS FINISHED, SORT THE RESULT.
                call "SLCTSORT" (#9, 30%)

            REM READ SORTED FILE AND PRINT THE DATA
                call "SHOSTAT" ("Printing Workstation Labor Details")
                somethingwasprinted% = 0%

            REM GET THE VERY FIRST RECORD
L11320:         read #9, using L11330, sortkey$, readkey$, eod goto L19190
L11330:                  FMT CH(64), CH(36)

                call "READ100" (#4, str(readkey$, 1, 16), f1%(4))
                      if f1%(4) = 0 then L11320

                select printer (134)
                call "SETPRNT" (rptid$, " ", 0%, 0%)
                pagenumber% = 0%

                gosub L30000                        /*LOAD DETAIL RECORD*/
                somethingwasprinted% = 1%
                call "DESCRIBE" (#3, wrkstn$, wrkdescr$, 1%, f1%(3))
                thisjob$ = jobnr$
                prtjob$ = jobnr$
                work$ = wrkstn$
                goto L12000

L11500:     REM GET THE NEXT RECORDS USING THE READ COMMAND
L11510:         read #9, using L11330, sortkey$, readkey$, eod goto L19000

            REM READ DETAIL FILE TO SEE IF RECORD EXISTS (IT SHOULD)
                call "READ100" (#4, str(readkey$, 1, 16), f1%(4))
                      if f1%(4) = 0 then L11510

                gosub L30000              /* GET DETAIL RECORD */

L12000: REM *************************************************************~
            * SEE IF WE HIT A NEW Work Center.  IF NOT THEN TOTAL $$$   *~
            * AND DROP THROUGH TO CHECK JOB.  IF NEW THEN SUBTOTAL,     *~
            * TOTAL, INITIALIZE AND MOVE ON.                            *~
            *************************************************************

            REM CHECK FOR NEW Work Center
                if wrkstn$ = work$ then L12360
                call "DESCRIBE" (#3, wrkstn$, wrkdescr$, 1%, f1%(3))

            REM FORMAT SUBTOTALS
                call "CONVERT" (totj1, 2.2, totj1$)
                call "CONVERT" (totj2, 2.2, totj2$)
                call "CONVERT" (totj3, 2.2, totj3$)
                call "CONVERT" (totaljobdc, 2.2, totaljobdc$)
                call "CONVERT" (totaljoboc, 2.2, totaljoboc$)
                print using L50160, totj1$, totaljobdc$, totaljoboc$,     ~
                                   totj2$, totj3$
                print using L50120

            REM RESET SUBTOTALING VARIABLES
                totaljobdc, totaljoboc, totj1, totj2, totj3 = 0
                thisjob$ = jobnr$
                prtjob$ = jobnr$

            REM PRINT Work Center TOTALS, RESET VARIABLES
                totw1 = round(totw1, 2%)
                totw2 = round(totw2, 2%)
                totw3 = round(totw3, 2%)
                totalwrkdc = round(totalwrkdc, 2%)
                totalwrkoc = round(totalwrkoc, 2%)
                print using L50180, work$, totw1, totalwrkdc, totalwrkoc, ~
                                   totw2, totw3
                work$ = wrkstn$
                pageline% = 1000
                totalwrkdc, totalwrkoc, totw1, totw2, totw3 = 0

L12360:     REM KEEP TOTALS FOR Work Center
                totalwrkdc = totalwrkdc + dcost
                totalwrkoc = totalwrkoc + ocost
                totw1 = totw1 + units
                totw2 = totw2 + prehrs
                totw3 = totw3 + precst

        REM *************************************************************~
            * SEE IF WE HIT A NEW JOB    CODE, IF YES THEN PRINT THE    *~
            * TOTAL FOR THIS JOB.    DROP THRU AND PRINT LINE ITEM      *~
            *************************************************************

            REM CHECK FOR NEW JOB NUMBER. PRINT SUBTOTAL IF NEEDED
                if jobnr$ = thisjob$ then L13170
                call "CONVERT" (totj1, 2.2, totj1$)
                call "CONVERT" (totj2, 2.2, totj2$)
                call "CONVERT" (totj3, 2.2, totj3$)
                call "CONVERT" (totaljobdc, 2.2, totaljobdc$)
                call "CONVERT" (totaljoboc, 2.2, totaljoboc$)
                print using L50160, totj1$, totaljobdc$, totaljoboc$,     ~
                                   totj2$, totj3$
                print using L50120
                pageline% = pageline% + 2

            REM RESET SUBTOTALING VARIABLES
                thisjob$ = jobnr$
                prtjob$ = jobnr$
                totaljobdc, totaljoboc, totj1, totj2, totj3 = 0

L13170:     REM ACCUMULATE TOTALS
                totj1 = totj1 + units
                totj2 = totj2 + prehrs
                totj3 = totj3 + precst
                totaljobdc = totaljobdc + dcost
                totaljoboc = totaljoboc + ocost

        rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

                gosub L60000              /* PAGE HEADING, IF NECCESSARY*/

              print using L50200, prtjob$, wdate$, empcode$, etype$,      ~
                                udescr$, units$, dcost$, ocost$, prehrs$,~
                                precst$

                prtjob$ = " "
                go to L11500

L19000: REM *************************************************************~
            *  PRINT THE TOTALS FOR THE LAST Work Center AND EXIT.      *~
            *                                                           *~
            *************************************************************

            REM PRINT TOTALS FOR JOB
                call "CONVERT" (totj1, 2.2, totj1$)
                call "CONVERT" (totj2, 2.2, totj2$)
                call "CONVERT" (totj3, 2.2, totj3$)
                call "CONVERT" (totaljobdc, 2.2, totaljobdc$)
                call "CONVERT" (totaljoboc, 2.2, totaljoboc$)
                print using L50160, totj1$, totaljobdc$, totaljoboc$,     ~
                                   totj2$, totj3$

            REM PRINT TOTALS FOR Work Center
                totw1 = round(totw1, 2%)
                totw2 = round(totw2, 2%)
                totw3 = round(totw3, 2%)
                totalwrkdc = round(totalwrkdc, 2%)
                totalwrkoc = round(totalwrkoc, 2%)
                print using L50120
               print using L50180, wrkstn$, totw1, totalwrkdc, totalwrkoc,~
                                  totw2, totw3
                print using L50120
                time$ = " " : call "TIME" (time$)
                print using L50230, time$
                close printer
                call "SETPRNT" (rptid$, " ", 0%, 1%)
L19190:     if somethingwasprinted% = 1% then L10000
            call "ASKUSER" (2%, "NOTHING SELECTED",                      ~
                            "There were no records selected to print.",  ~
                            " ", "Press RETURN to Continue....")
                go to L10000

L30000: REM *************************************************************~
            *    L O A D   L A B O R           D E T A I L   R E C D    *~
            *                                                           *~
            * LOADS JOB LABOR     DETAIL RECORD FROM THE FILE           *~
            *                                                           *~
            *************************************************************

            get   #4, using L30200,                                       ~
                      jobnr$, empcode$, wdate$, wrkstn$, etype$,         ~
                      udescr$, rate, units, dcost, ocost, prehrs, precst

            call "DATEFMT" (wdate$)
            call "CONVERT" (rate,  2.2, rate$)
            call "CONVERT" (units, 2.2, units$)
            call "CONVERT" (dcost, 2.2, dcost$)
            call "CONVERT" (ocost, 2.2, ocost$)
            call "CONVERT" (prehrs, 2.2, prehrs$)
            call "CONVERT" (precst, 2.2, precst$)
            return


L30200:     FMT CH(8),                   /* JOB NUMBER                 */~
                XX(8),                   /* SKIP SEQUENCE NUMBER       */~
                CH(12),                  /* EMPLOYEE CODE              */~
                CH(06),                  /* DATE WORK PERFORMED        */~
                CH(04),                  /* WORK STATION CODE          */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(06),                  /* UNITS DESCRIPTION          */~
                PD(14,4),                /* UNIT RATE                  */~
                PD(14,4),                /* AMOUNT OF UNITS            */~
                PD(14,4),                /* DIRECT LABOR COST          */~
                PD(14,4),                /* OVERHEAD LABOR COST        */~
                XX(9),                   /* SKIP AUDIT INFO            */~
                PD(14,4),                /* OVERTIME HOURS             */~
                PD(14,4)                 /* OVERTIME COST              */~

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING. DEVELOPED BY ME, NO     *~
            * THANKS TO "EZPRINT" AND ITS NASTY BUGS (V 1.4)            *~
            *************************************************************

L50070: %PAGE ######         W O R K S T A T I O N   L A B O R   D E T A ~
        ~I L                ##############################################~
        ~###

L50100: %     FOR WORK CENTER  ####  ################################### ~
        ~                                                  PRJWRKPR#######

L50120: %+-----------+----------+--------------+--------------+--------+-~
        ~-----------+------------+------------+------------+------------+

L50140: %!JOB  NUMBER!WORK  DATE! EMPLOYEE CODE! EARNINGS TYPE! UNITS  ! ~
        ~DIRECT AMT !DIRECT  COST!OVERHEAD CST!OVERTIME HRS!OVERTIME CST!

L50160: %!           !          !              ! TOTAL FOR JOB         ! ~
        ~########## ! ########## ! ########## ! ########## ! ########## !

L50180: %! ****TOTALS****   FOR WORK CENTER ####                        #~
        ~#######.## ! #######.## ! #######.## ! #######.## ! #######.## !

L50200: %! ########  ! ######## ! ############ ! ############ ! ###### ! ~
        ~########## ! ########## ! ########## ! ########## ! ########## !

L50230: %                                                   ** END OF REP~
        ~ORT @ ######## **

L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************

            select printer (134)
            pageline% = pageline% + 1
            if pageline% < 55 then return
               pagenumber% = pagenumber% + 1
               if pagenumber% > 1 then print using L50120
               print page
               call "DATE" addr ("HD", hdrdate$)
               print using L50070, pagenumber%, hdrdate$
               print using L50100, wrkstn$, wrkdescr$, "-" & rptid$
               print skip (1)
               print using L50120
               print using L50140
               print using L50120
               pageline% = 8
               return

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

L65080:     call "SHOSTAT" ("Closing Files, One Moment Please")
            end
