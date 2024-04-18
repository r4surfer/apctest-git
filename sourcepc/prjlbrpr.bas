        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   RRRR   JJJJJ  L      BBBB   RRRR   PPPP   RRRR    *~
            *  P   P  R   R    J    L      B   B  R   R  P   P  R   R   *~
            *  PPPP   RRRR     J    L      BBBB   RRRR   PPPP   RRRR    *~
            *  P      R   R  J J    L      B   B  R   R  P      R   R   *~
            *  P      R   R   J     LLLLL  BBBB   R   R  P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRJLBRPR - PRINTS THE LABOR DETAILS FOR THE JOB NUMBERS   *~
            *            SPECIFIED.  SORT IS BY JOB NUMB BY WORKSTATION.*~
            *            USES *SUPER SELECTOR* ROUTINES.                *~
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
            * 08/10/88 ! Fixed Negative Totals Bug.  Replaced     ! TLJ *~
            *          ! obsolete NUMPRINT with CONVERT.          !     *~
            * 09/26/88 ! Standardised Report, deleted CONVERTS.   ! TLJ *~
            * 06/13/91 ! Eliminated unused function FNX. PRR 12035! JDH *~
            * 07/15/91 !(NO PRR) Renamed JLBRPRNT to New Name     ! RJB *~
            * 08/23/96 ! Changes for the year 2000.               ! DXL *~
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
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                  /* Company or Division Name   */~
            date$8,                      /* Date for screen display    */~
            dateopen$8,                  /* DATE JOB WAS OPENED        */~
            dateclosed$8,                /* DATE JOB WAS CLOSED        */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            etype$12,                    /* EARNINGS TYPE              */~
            final(5),                    /* FINAL TOTALS               */~
            hdrdate$50,                  /* HEADER DATE FOR REPORT     */~
            job$8,                       /* FOR COMPARISION TO FORM FEE*/~
            jobnr$8,                     /* JOB NUMBER FROM DISK TO YOU*/~
            jobdescr$34,                 /* SALEMAN'S DESCRIPTION      */~
            line2$79,                    /* Screen Display Line 2      */~
            prtjobclosed$40,             /* PRINT DATE CLOSED          */~
            readkey$50,                  /* READ RECORD WITH THIS      */~
            rpttitle$60,                 /* Report Title               */~
            sortkey$50,                  /* KEY TO GO TO SORT FILE     */~
            time$8,                      /* System Time                */~
            udescr$6,                    /* UNITS DESCRIPTION          */~
            wdate$8                      /* DATE WORK PERFORMED        */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
            str(line2$) = "PRJLBRPR: " & str(cms2v$,,8)
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
            * #  3 ! JOBMASTR ! WIP/ JOB MASTER FILE  (WIP IT GOOD)     *~
            * #  4 ! JOBLABOR ! JOB LABOR DETAIL FILE                   *~
            * #  9 ! WORKFILE ! WORK FILE FOR SELECTED RECORDS          *~
            *************************************************************

            select  # 3, "JOBMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 8

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
            *      S = SINGLE PARAMTER FIELD (NO RANGE PROCESSING)      *~
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

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, u3%) : u3% = u3%
            rpttitle$ = "WIP / JC Detail of Labor      " &               ~
                        "                              "
            call "STRING" addr("CT", rpttitle$, 60%)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("JLB001", " ", 0%, 0%)
            page% = 0%

            REM SET UP DATA FOR SELECT INTERPRETATION.
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

                data "JOB NUMBER               ", "U",  8,  8, 1, 001,   ~
                     "WORKSTATION CODE         ", "U",  4,  4, 1, 035,   ~
                     "DATE WORK PERFORMED      ", "D",  6,  8, 1, 029,   ~
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
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * GETS THE ***SUPER SELECTOR*** INFORMATION FOR THE PROGRAM *~
            * WHICH WE WILL USE IN THE SELECT PASS, WHICH FOLLOWS.      *~
            *************************************************************

            mat final = zer
            totalforwrk, totalforjob, totalforovh, totaljovh = 0
            a, a1, b, b1, c, c1 = 0
            pageline% = 1000
            init(" ") errormsg$, inpmessage$, from$(), to$()
            mat fromnr = zer: mat tonr = zer: mat field% = zer

            for temp% = 1 to 15
                if prompt$(temp%) = " " then L10130
                   from$(temp%) = "ALL"
L10130:         next temp%

L10150:     call "SLCTSCRN" ("WIP/JC Labor Detail Report",     errormsg$,~
                               inpmessage$, keyhit%, line2$)
                  if keyhit%  =  1 then gosub L10000
                  if keyhit%  = 16 then       L65000
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
            call "SHOSTAT"  ("Selecting Records...One Moment Please")
            call "WORKOPEN" (# 9, "OUTPT", 5000%, f2%(9))

L11110:     call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                 if f1%(4) = 0 then L11250          /* GO SORT WORKFILE */
            call "MOVERWA" addr(#4, str(record$(), 1))

            call "SLCTPASS" (maxfields%, select%)
            if select% = 0 then L11110          /* GET NEXT RECORD  */
               sortkey$ = " "
               str(sortkey$,  1,  8) = str(record$(), 1,  8)
               str(sortkey$,  9,  4) = str(record$(), 35, 4)
               str(sortkey$, 13,  6) = str(record$(), 29, 6)
               str(sortkey$, 19, 12) = str(record$(), 17,12)
               write #9, using L11220, sortkey$, str(readkey$, 1, 16)
L11220:                  FMT CH(64), CH(36)
               goto L11110

L11250:     REM NOW THAT SELECT ROUTINE HAS FINISHED, SORT THE RESULT.
                call "SLCTSORT" (#9, 30%)

            REM READ SORTED FILE AND PRINT THE DATA
                call "SHOSTAT" ("Printing Job Cost Labor Details")

            REM GET THE VERY FIRST RECORD
L11320:         read #9, using L11330, sortkey$, readkey$, eod goto L65000
L11330:                  FMT CH(64), CH(36)

                call "READ100" (#4, str(readkey$, 1, 16), f1%(4))
                      if f1%(4) = 0 then L11320

                gosub L30000                        /*LOAD DETAIL RECORD*/

                firstjob$ = jobnr$
                call "DESCRIBE" (#3, jobnr$, jobdescr$, 0%, f1%(3))
                get #3, using L11410, dateopen$, dateclosed$
L11410:           FMT XX(38), 2*CH(6)
                call "DATEFMT" (dateopen$)
                call "DATEFMT" (dateclosed$)
                thiswrkstn$ = wrkstn$
                prtwrkstn$ = wrkstn$
                job$ = jobnr$
                go to L12000

L11500:     REM GET THE NEXT RECORDS USING THE READ COMMAND
L11510:         read #9, using L11330, sortkey$, readkey$, eod goto L19000

            REM READ DETAIL FILE TO SEE IF RECORD EXISTS (IT SHOULD)
                call "READ100" (#4, str(readkey$, 1, 16), f1%(4))
                      if f1%(4) = 0 then L11510

                gosub L30000              /* GET DETAIL RECORD */

L12000: REM *************************************************************~
            * SEE IF WE HIT A NEW JOB, IF NOT THEN ADD THE TOTAL FIELD  *~
            * TO THE TOTALFORJOB FIELD AND DROP THROUGH TO CHECK FOR THE*~
            * WRKSTN.  IF THERE IS A NEW JOB NUMBER, THEN TOTAL THE     *~
            * WRKSTN FOR THIS JOB AND INITIALIZE THE WRKSTN AND JOB     *~
            * TOTAL FIELDS AND GO ON TO PRINT A NEW PAGE.               *~
            *************************************************************

            REM CHECK FOR NEW JOB NUMBER
                if jobnr$ = job$ then L12360
                call "DESCRIBE" (#3, jobnr$, jobdescr$, 0%, f1%(3))
                     if f1%(3) = 0 then L12150
                        get #3, using L12130, dateopen$, dateclosed$
L12130:                        FMT XX(38), 2*CH(6)

L12150:     REM FORMAT NEW JOB DATA AND SUBTOTALS
                call "DATEFMT" (dateopen$)
                call "DATEFMT" (dateclosed$)
                print using L50156
                print using L50160, a, totalforwrk, totalforovh, b, c
                print

            REM RESET SUBTOTALING VARIABLES
                totalforwrk, totalforovh, a, b, c = 0
                thiswrkstn$ = wrkstn$
                prtwrkstn$ = wrkstn$

            REM PRINT JOB TOTALS, RESET VARIABLES
                print using L50180,job$,a1,totalforjob,totaljovh,b1,c1
                gosub L60000
                job$ = jobnr$
                pageline% = 1000
                final(1) = final(1) + a1
                final(2) = final(2) + totalforjob
                final(3) = final(3) + totaljovh
                final(4) = final(4) + b1
                final(5) = final(5) + c1
                totalforjob, totaljovh, a1, b1, c1 = 0

L12360:     REM KEEP TOTALS FOR JOB
                totalforjob = totalforjob + dcost
                totaljovh = totaljovh + ocost
                a1 = a1 + units
                b1 = b1 + over1
                c1 = c1 + over2

        REM *************************************************************~
            * SEE IF WE HIT A NEW WRKSTN CODE, IF YES THEN PRINT THE    *~
            * TOTAL FOR THIS WRKSTN. DROP THRU AND PRINT LINE ITEM      *~
            *************************************************************

            REM CHECK FOR NEW WORKSTATION. PRINT SUBTOTAL IF NEEDED
                if wrkstn$ = thiswrkstn$ then L13170
                print using L50156
                print using L50160, a, totalforwrk, totalforovh, b, c
                print

            REM RESET SUBTOTALING VARIABLES
                thiswrkstn$ = wrkstn$
                prtwrkstn$ = wrkstn$
                totalforwrk, totalforovh, a, b, c = 0

L13170:     REM ACCUMULATE TOTALS
                totalforwrk = totalforwrk + dcost
                totalforovh = totalforovh + ocost
                a = a + units
                b = b + over1
                c = c + over2

        rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

                if pageline% + 8 > 56 then pageline% = 1000
                               /* PAGE FEED IF NOT ENOUGH THIS PAGE.   */
                               /* (FOOLS LINE COUNT BY SAYING WE'RE AT */
                               /* WAY BEYOND END OF PAGE.              */


                gosub L60000              /* PAGE HEADING, IF NECCESSARY*/

              print using L50200, prtwrkstn$, wdate$, empcode$, etype$,   ~
                                udescr$, units, dcost, ocost, over1,     ~
                                over2

                prtwrkstn$ = " "
                go to L11500

L19000: REM *************************************************************~
            *  PRINT THE TOTALS FOR THE LAST JOB AND EXIT THE PROGRAM   *~
            *                                                           *~
            *************************************************************

            REM PRINT TOTALS FOR WORKSTATION
                print using L50156
                print using L50160, a, totalforwrk, totalforovh, b, c

            REM PRINT TOTALS FOR JOB
                print
                print using L50180,job$,a1,totalforjob,totaljovh,b1,c1
                final(1) = final(1) + a1
                final(2) = final(2) + totalforjob
                final(3) = final(3) + totaljovh
                final(4) = final(4) + b1
                final(5) = final(5) + c1
                print page  /* Total Page Header */
                print using L50070, date$, time$, company$, "PRJLBRPR"
                print using L50091, rpttitle$, page% + 1%
                print skip(2)
                print "OVERALL TOTALS FOR JOBS IN THE RANGE FROM ";      ~
                               firstjob$; " TO "; job$
                print skip(2)
                print using L19470, final(1)
                print using L19480, final(2)
                print using L19490, final(3)
                print
                print using L19510, final(4)
                print using L19520, final(5)
                call "SETPRNT" (" ", " ", 0%, 1%)
                go to L65000

L19470: %TOTAL DIRECT HOURS      -##,###,###.##
L19480: %TOTAL LABOR COST       -$##,###,###.##
L19490: %TOTAL OVERHEAD COST    -$##,###,###.##

L19510: %TOTAL OVERTIME HOURS    -##,###,###.##
L19520: %TOTAL OVERTIME COST    -$##,###,###.##


L30000: REM *************************************************************~
            *    L O A D   L A B O R           D E T A I L   R E C D    *~
            *                                                           *~
            * LOADS JOB LABOR     DETAIL RECORD FROM THE FILE           *~
            *                                                           *~
            *************************************************************

            get   #4, using L30200,                                       ~
                      jobnr$, empcode$, wdate$, wrkstn$, etype$,         ~
                      udescr$, rate, units, dcost, ocost, over1, over2
            rate = rate

            call "DATEFMT" (wdate$)
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
                XX(9),                                                   ~
                2*PD(14,4)

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING. DEVELOPED BY ME, NO     *~
            * THANKS TO "EZPRINT" AND ITS NASTY BUGS (V 1.4)            *~
            *************************************************************

L50070: %RUN ######## @ ########              ###########################~
        ~#################################                ########:JLB001

L50091: %                                     ###########################~
        ~#################################                    PAGE:  ####

L50100: %FOR JOB NUMBER ########  ##################################
L50110: %DATE OPENED ########     ###################################

L50120: %___________ __________ ______________ ______________ ________ __~
        ~___________ ____________ ____________ ____________ ____________

L50140: %WORKSTATION WORK  DATE  EMPLOYEE CODE  EARNINGS TYPE  UNITS     ~
        ~  AMOUNT    DIRECT  COST OVERHEAD CST OVERTIME HRS OVERTIME CST

L50156: %                                                              __~
        ~___________ ____________ ____________ ____________ ____________

L50160: %                                       TOTAL FOR WORKSTATION  -#~
        ~########.## -########.## -########.## -########.## -########.##

L50180: % TOTALS FOR JOB ########                                      -#~
        ~########.## -$#######.## -$#######.## -########.## -$#######.##

L50200: %   ####      ########   ############   ############   ######  -#~
        ~########.## -########.## -########.## -########.## -########.##


L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************

            select printer (134)
            pageline% = pageline% + 1
            if pageline% < 56 then return
               page% = page% + 1%
               print page
               call "DATE" addr ("HD", hdrdate$)
               print using L50070, date$, time$, company$, "PRJLBRPR"
               print using L50091, rpttitle$, page%
               prtjobclosed$ = " "
               if dateclosed$ = " " or dateclosed$ = blankdate$ then L60161
               prtjobclosed$ = "DATE CLOSED: " & dateclosed$
L60161:        print
               print using L50100, jobnr$, jobdescr$
               print using L50110, dateopen$, prtjobclosed$
               print skip (2)
               print using L50140
               print using L50120
               print
               pageline% = 8
               return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
