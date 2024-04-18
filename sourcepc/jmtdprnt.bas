        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  M   M  TTTTT  DDDD   PPPP   RRRR   N   N  TTTTT   *~
            *    J    MM MM    T    D   D  P   P  R   R  NN  N    T     *~
            *    J    M M M    T    D   D  PPPP   RRRR   N N N    T     *~
            *  J J    M   M    T    D   D  P      R   R  N  NN    T     *~
            *   J     M   M    T    DDDD   P      R   R  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JMTDPRNT - THIS PROGRAM PRINTS DETAILS FOR THE MATERIALS  *~
            *            FROM INVENTORY THAT HAVE BEEN *DIRECTLY* POSTED*~
            *            TO A GIVEN JOB. THE DIRECT MATERIALS DETAIL    *~
            *            FILE IS SORTED IN JOB NO, PART NO, STORE NO,   *~
            *            DATE MOVED ORDER.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/07/81 ! ORIGINAL                                 ! TOM *~
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
            datemoved$8,                 /* DATE PART MOVED TO JOB     */~
            dateopen$8,                  /* DATE JOB WAS OPENED        */~
            dateclosed$8,                /* DATE JOB WAS CLOSED        */~
            hdrdate$50,                  /* HEADER DATE FOR REPORT     */~
            job$8,                       /* FOR COMPARISION TO FORM FEE*/~
            jobnr$8,                     /* JOB NUMBER FROM DISK TO YOU*/~
            jobdescr$34,                 /* SALEMAN'S DESCRIPTION      */~
            part$25,                     /* OUR PURCHASE ORDER NUMBER  */~
            partcode$25,                 /* PART NUMBER TO COMPARE     */~
            prtjobclosed$40,             /* PRINT DATE CLOSED          */~
            prtpartcode$25,              /* PRINT THE PART NUMBER      */~
            quantity$10,                 /* PRINT THE QUANTITY MOVED   */~
            sortkey$50,                  /* KEY TO GO TO SORT FILE     */~
            total$10,                    /* PRINT THE TOTAL DLLARS     */~
            totalforjob$10,              /* PRINT THE TOTAL FOR JOB    */~
            totalforpart$10,             /* PRINTS THE TOTAL FOR PART  */~
            readkey$50,                  /* READ RECORD WITH THIS      */~
            unitcost$10                  /* PRINT UNIT COST            */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.04 12/15/87 Patch Release                  "
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
            * #  4 ! JOBMTLDR ! JOB MATERIAL POSTED DIRECTLY DETAIL FILE*~
            * #  9 ! WORKFILE ! WORK FILE FOR SELECTED RECORDS          *~
            *************************************************************

            select  # 3, "JOBMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 8

            select  # 4, "JOBMTLDR",                                     ~
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
            * PROGRAM.  ALSO INITIALIZES STUFF NEEDED FOR THE SUPER     *~
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

                data "JOB NUMBER               ", "U",  8,  8, 1, 001,   ~
                     "PART NUMBER              ", "U", 25, 25, 1, 017,   ~
                     "STORE NUMBER             ", "U", 03, 03, 1, 042,   ~
                     "DATE MOVED TO JOB        ", "D",  6,  8, 1, 051,   ~
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
            totalforpart, totalforjob = 0
            pageline% = 1000
            init(" ") errormsg$, inpmessage$, from$(), to$()
            mat fromnr = zer: mat tonr = zer: mat field% = zer

            for temp% = 1 to 15
                if prompt$(temp%) = " " then L10130
                   from$(temp%) = "ALL"
L10130:         next temp%

L10150:     call "SLCTSCRN" ("WIP/JC MATERIAL USED REPORT",              ~
                            errormsg$, inpmessage$, keyhit%, "JMTDPRNT")
                  if keyhit%  =  1 then gosub L10000
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L10150
            call "SLCTTEST" (errormsg$, maxfields%)
                  if errormsg$ <> " " then L10150

        REM *************************************************************~
            *       P L O W   T H R O U G H   A N D   S E L E C T       *~
            *                                                           *~
            * PLOWS THROUGH THE SALES ANALYSIS MASTER RECORDS AND       *~
            * SELECTS ON THE FIELDS WITH THE VALUES WE HAVE SPECIFIED.  *~
            *************************************************************

            readkey$ = " "
            call "SHOSTAT"  ("Selecting Records...One Moment Please")
            call "WORKOPEN" (# 9, "OUTPT", 5000%, f2%(9))

L11110:     call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                 if f1%(4) = 0 then L11250          /* GO SORT WORKFILE */
            get #4, str(record$())

            call "SLCTPASS" (maxfields%, select%)
            if select% = 0 then L11110          /* GET NEXT RECORD  */
               sortkey$ = " "
               str(sortkey$,  1,  8) = str(record$(), 1,  8)
               str(sortkey$,  9, 25) = str(record$(), 17, 25)
               str(sortkey$, 33,  3) = str(record$(), 42, 6)
               str(sortkey$, 36,  6) = str(record$(), 51, 6)
               write #9, using L11220, sortkey$, str(readkey$, 1, 16)
L11220:                  FMT CH(64), CH(36)
               goto L11110

L11250:     REM NOW THAT SELECT ROUTINE HAS FINISHED, SORT THE RESULT.
                call "SLCTSORT" (#9, 23%)
        REM READ THE SORTED FILE AND PRINT THE DATA

            call "SHOSTAT" ("Printing Material Posted Directly to Jobs")

        REM GET THE FIRST RECORD
L11320:         read #9, using L11330, sortkey$, readkey$, eod goto L65000
L11330:                  FMT CH(64), CH(36)

                call "READ100" (#4, str(readkey$, 1, 16), f1%(4))
                      if f1%(4) = 0 then L11320

                gosub L30000                        /*LOAD DETAIL RECORD*/
                call "DESCRIBE" (#3, jobnr$, jobdescr$, 0%, f1%(3))
                get #3, using L11410, dateopen$, dateclosed$
L11410:           FMT XX(38), 2*CH(6)
                call "DATEFMT" (dateopen$)
                call "DATEFMT" (dateclosed$)
                part$ = partcode$
                prtpartcode$ = partcode$
                job$ = jobnr$
                go to L12000

        REM GET THE NEXT RECORDS USING THE READ COMMAND
L11510:         read #9, using L11330, sortkey$, readkey$, eod goto L19000

        REM READ DETAIL FILE TO SEE IF RECORD EXISTS (IT SHOULD)
                call "READ100" (#4, str(readkey$, 1, 16), f1%(4))
                      if f1%(4) = 0 then L11510

                gosub L30000              /* GET DETAIL RECORD */

L12000: REM *************************************************************~
            * SEE IF WE HIT A NEW JOB, IF NOT THEN ADD THE TOTAL FIELD  *~
            * TO THE TOTALFORJOB FIELD AND DROP THROUGH TO CHECK FOR THE*~
            * PART.    IF THERE IS A NEW JOB NUMBER, THEN TOTAL THE     *~
            * PART   FOR THIS JOB AND INITIALIZE THE PART   AND JOB     *~
            * TOTAL FIELDS AND GO ON TO PRINT A NEW PAGE.               *~
            *************************************************************

                if jobnr$ = job$ then L12290
                call "DESCRIBE" (#3, jobnr$, jobdescr$, 0%, f1%(3))
                if f1%(3) = 0 then L12220
                get #3, using L12120, dateopen$, dateclosed$
L12120:           FMT XX(38), 2*CH(6)
                call "DATEFMT" (dateopen$)
                call "DATEFMT" (dateclosed$)
                print using L50100
                call "CONVERT" (totalforpart, 2.2, totalforpart$)
                print using L50500, totalforpart$
                print using L50100
                totalforpart = 0
                part$ = partcode$
                prtpartcode$ = partcode$
L12220:         call "CONVERT" (totalforjob, 2.2, totalforjob$)
                print using L50600, job$, totalforjob$
                gosub L60000
                gosub L60000
                job$ = jobnr$
                pageline% = 1000
                totalforjob = 0
L12290:         totalforjob = totalforjob + total

        REM *************************************************************~
            * SEE IF WE HIT A NEW PART   CODE, IF YES THEN PRINT THE    *~
            * TOTAL FOR THIS PART.   DROP THRU AND PRINT LINE ITEM      *~
            *************************************************************

                if part$ = partcode$ then L13130
                print using L50100
                call "CONVERT" (totalforpart, 2.2, totalforpart$)
                print using L50500, totalforpart$
                print using L50100
                part$ = partcode$
                prtpartcode$ = partcode$
                totalforpart = 0
L13130:         totalforpart = totalforpart + total

        rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

                if pageline% + 6 > 64 then pageline% = 1000
                               /* PAGE FEED IF NOT ENOUGH THIS PAGE.   */
                               /* (FOOLS LINE COUNT BY SAYING WE'RE AT */
                               /* WAY BEYOND END OF PAGE.              */

                           pageline% = pageline% + 1 /* FOR EXTRA LINE */

                gosub L60000              /* PAGE HEADING, IF NECCESSARY*/

             print using L50900, prtpartcode$, store$, lot$, datemoved$,  ~
                                quantity$, unitcost$, total$

                prtpartcode$ = " "
                go to L11510

L19000: REM *************************************************************~
            *  PRINT THE TOTALS FOR THE LAST JOB AND EXIT THE PROGRAM   *~
            *                                                           *~
            *************************************************************

             print using L50100
             call "CONVERT" (totalforpart, 2.2, totalforpart$)
             print using L50500, totalforpart$

             print using L50100
             call "CONVERT" (totalforjob, 2.2, totalforjob$)
             print skip (1)
             print using L50600, job$, totalforjob$
             go to L65000

L30000: REM *************************************************************~
            *    L O A D   P U R C H A S E S   D E T A I L   R E C D    *~
            *                                                           *~
            * LOADS JOB PURCHASES DETAIL RECORD FROM THE FILE           *~
            *                                                           *~
            *************************************************************

            get   #4, using L30200,                                       ~
                      jobnr$, partcode$, store$, lot$, datemoved$,       ~
                      quantity, ucost, total

            call "DATEFMT" (datemoved$)
            if quantity < 0 then total = -total
            call "CONVERT" (quantity, 0.2, quantity$)
            call "CONVERT" (total, 2.2, total$)
            call "CONVERT" (ucost, 2.4, unitcost$)
            return

L30200:     FMT CH(8),                   /* JOB NUMBER                 */~
                XX(8),                   /* SKIP SEQUENCE NUMBER       */~
                CH(25),                  /* PART NUMBER MOVED TO JOB   */~
                CH(03),                  /* STORE NUMBER OF PART MOVED */~
                CH(6),                   /* LOT NUMBER OF PART MOVED   */~
                CH(06),                  /* DATE PART MOVED TO JOB     */~
                PD(14,4),                /* QUANTITY MOVED TO JOB      */~
                PD(14,4),                /* UNIT COST OF PART MOVED    */~
                PD(14,4)                 /* TOTAL COST OF MOVEMENT     */~

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING. DEVELOPED BY ME, NO     *~
            * THANKS TO "EZPRINT" AND ITS NASTY BUGS (V 1.4)            *~
            *************************************************************

L49980: %PAGE ######         W I P  /  J C    D E T A I L   O F   M A T E~
        ~ R I A L S   ####################################################~
        ~###
L50010: %  FOR JOB NUMBER ########  ##################################   ~
        ~   DATE OPENED ########     ###################################
L50100: %+-------------------------+---+------+-----------------+--------~
        ~------------------+------------+---------------------+
L50300: %!     PART NUMBER         !STR!LOT.NO!DATE MOVED TO JOB! QUANTIT~
        ~Y MOVED TO JOB    ! UNIT COST  ! TOTAL COST EXTENDED !
L50500: %!                                                               ~
        ~                      TOTAL FOR PART  ##########     !
L50600: %                                                                ~
        ~   TOTAL FOR JOB NUMBER  ########  IS ##########
L50900: %!#########################!###!######!    ########     !      ##~
        ~########          ! ###########!      ##########     !

L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************
            select printer (134)
            pageline% = pageline% + 1
            if pageline% < 64 then return
               print page
               call "DATE" addr ("HD", hdrdate$)
               pagenumber% = pagenumber% + 1
               print using L49980, pagenumber%, hdrdate$
               prtjobclosed$ = " "
               if str(dateclosed$, 1, 2) = " " then L60130
               prtjobclosed$ = "JOB CLOSING DATE: " & dateclosed$
L60130:        print using L50010, jobnr$, jobdescr$, dateopen$,          ~
                                  prtjobclosed$
               print skip (1)
               print using L50100
               print using L50300
               print using L50100
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
