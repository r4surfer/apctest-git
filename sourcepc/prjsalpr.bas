        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   RRRR   JJJJJ   SSS    AAA   L      PPPP   RRRR    *~
            *  P   P  R   R    J    S      A   A  L      P   P  R   R   *~
            *  PPPP   RRRR     J     SSS   AAAAA  L      PPPP   RRRR    *~
            *  P      R   R  J J        S  A   A  L      P      R   R   *~
            *  P      R   R   J      SSS   A   A  LLLLL  P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRJSALPR - THIS PROGRAM PRINTS THE INVOICES THAT HAVE BEEN*~
            *            POSTED TO A GIVEN JOB.  (RIPPED OFF FROM THE   *~
            *            OTHER DETAIL PRINTING PROGRAMS).               *~
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
            * 07/15/91 ! ORIGINAL                                 ! RJB *~
            * 07/01/81 ! ORIGINAL                                 ! TOM *~
            * 06/13/91 ! Eliminated FNX.  Chngd SHOWMSG to SHOSTAT! JDH *~
            * 07/15/91 !(NO PRR) Renamed JSALPRNT to New Name     ! RJB *~
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
            cuscode$,                    /* CUSTOMER CODE FROM FILE    */~
            customer$,                   /* CODE TO COMPARE TO FORM FED*/~
            dateopen$8,                  /* DATE JOB WAS OPENED        */~
            dateclosed$8,                /* DATE JOB WAS CLOSED        */~
            glacct$9,                    /* ACCT NO FROM PURCH FILE    */~
            hdrdate$50,                  /* HEADER DATE FOR REPORT     */~
            invoicedate$8,               /* DATE OF THE INVOICE        */~
            invoicenr$8,                 /* INVOICE NUMBER             */~
            item$25,                     /* ITEM ORDERED ON PURCH FILE */~
            job$8,                       /* FOR COMPARISION TO FORM FEE*/~
            jobnr$8,                     /* JOB NUMBER FROM DISK TO YOU*/~
            jobdescr$34,                 /* SALEMAN'S DESCRIPTION      */~
            ponumber$16,                 /* OUR PURCHASE ORDER NUMBER  */~
            prtjobclosed$40,             /* PRINT DATE CLOSED          */~
            prtcuscode$9,                /* PRINT THE CUSTOMER CODE    */~
            quantity$10,                 /* PRINT THE QUANTITY PURCH.  */~
            sortkey$50,                  /* KEY TO GO TO SORT FILE     */~
            total$10,                    /* PRINT THE TOTAL DLLARS     */~
            totalforjob$10,              /* PRINT THE TOTAL FOR JOB    */~
            totalforcus$10,              /* PRINTS THE TOTAL FOR CUSTOM*/~
            readkey$50                   /* READ RECORD WITH THIS      */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #  4 ! JOBSALES ! JOB INVOICING DETAIL FILE               *~
            * #  9 ! WORKFILE ! WORK FILE FOR SELECTED RECORDS          *~
            *************************************************************

            select  # 3, "JOBMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 8

            select  # 4, "JOBSALES",                                     ~
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
                     "CUSTOMER CODE            ", "U",  9,  9, 1, 017,   ~
                     "INVOICE NUMBER           ", "U",  8,  8, 1, 026,   ~
                     "DATE INVOICED            ", "D",  6, 10, 1, 034,   ~
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
            totalforcus, totalforjob = 0
            pageline% = 1000
            init(" ") errormsg$, inpmessage$, from$(), to$()
            mat fromnr = zer: mat tonr = zer: mat field% = zer

            for temp% = 1 to 15
                if prompt$(temp%) = " " then L10130
                   from$(temp%) = "ALL"
L10130:         next temp%

L10150:     call "SLCTSCRN" ("WIP/JC INVOICE DETAIL REPORT",   errormsg$,~
                            inpmessage$, keyhit%, "PRJSALPR")
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
            call "MOVERWA" addr(#4, str(record$(), 1))

            call "SLCTPASS" (maxfields%, select%)
            if select% = 0 then L11110          /* GET NEXT RECORD  */
               sortkey$ = " "
               str(sortkey$,  1,  8) = str(record$(), 1,  8)
               str(sortkey$,  9,  9) = str(record$(), 17, 9)
               str(sortkey$, 18,  8) = str(record$(), 26, 8)
               write #9, using L11220, sortkey$, str(readkey$, 1, 16)
L11220:                  FMT CH(64), CH(36)
               goto L11110

L11250:     REM NOW THAT SELECT ROUTINE HAS FINISHED, SORT THE RESULT.
                call "SLCTSORT" (#9, 23%)
        REM READ THE SORTED FILE AND PRINT THE DATA

            call "SHOSTAT" ("Printing Job Cost Invoicing Details")

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
                customer$ = cuscode$
                prtcuscode$ = cuscode$
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
            * CUSTOMER.IF THERE IS A NEW JOB NUMBER, THEN TOTAL THE     *~
            * CUSTMR FOR THIS JOB AND INITIALIZE THE CUSTMR AND JOB     *~
            * TOTAL FIELDS AND GO ON TO PRINT A NEW PAGE.               *~
            *************************************************************

          REM CHECK FOR A NEW JOB NUMBER
                if jobnr$ = job$ then L12380
                call "DESCRIBE" (#3, jobnr$, jobdescr$, 0%, f1%(3))
                if f1%(3) = 0 then L12180
                get #3, using L12130, dateopen$, dateclosed$
L12130:           FMT XX(38), 2*CH(6)

          REM FORMAT NEW JOB DATA AND SUBTOTALS
                call "DATEFMT" (dateopen$)
                call "DATEFMT" (dateclosed$)
L12180:         print using L50100
                call "NUMPRINT" (totalforcus, 2, totalforcus$)
                print using L50500, totalforcus$
                print using L50100

          REM RESET SUBTOTALLING VARIABLES
                totalforcus = 0
                customer$ = cuscode$
                prtcuscode$ = cuscode$

          REM PRINT JOB TOTALS AND RESET VARIABLES
                call "NUMPRINT" (totalforjob, 2, totalforjob$)
                print using L50600, job$, totalforjob$
                gosub L60000
                gosub L60000
                job$ = jobnr$
                pageline% = 1000
                totalforjob = 0

          REM KEEP TOTALS FOR THE JOB
L12380:         totalforjob = totalforjob + total

        REM *************************************************************~
            * SEE IF WE HIT A NEW CUSTMR CODE, IF YES THEN PRINT THE    *~
            * TOTAL FOR THIS CUSTMR. DROP THRU AND PRINT LINE ITEM      *~
            *************************************************************

          REM CHECK FOR NEW CUSTOMER CODE
                if cuscode$ = customer$ then L13200

          REM IF NEW CUSTOMER CODE THEN PRINT TOTAL
                print using L50100
                call "NUMPRINT" (totalforcus, 2, totalforcus$)
                print using L50500, totalforcus$
                print using L50100

          REM NOW RESET SUBTOTALLING VARIABLES
                customer$ = cuscode$
                prtcuscode$ = cuscode$
                totalforcus = 0

          REM KEEP THE TOTAL FOR CUSTOMER
L13200:         totalforcus = totalforcus + total

        rem**************************************************************~
           * print the line item of the detail                          *~
           **************************************************************

                if pageline% + 6 > 64 then pageline% = 1000
                               /* PAGE FEED IF NOT ENOUGH THIS PAGE.   */
                               /* (FOOLS LINE COUNT BY SAYING WE'RE AT */
                               /* WAY BEYOND END OF PAGE.              */

                           pageline% = pageline% + 1 /* FOR EXTRA LINE */

                gosub L60000              /* PAGE HEADING, IF NECCESSARY*/

             print using L50900, prtcuscode$, invoicenr$, invoicedate$,   ~
                      ponumber$, item$, quantity$, total$, glacct$

                prtcuscode$ = " "
                go to L11510

L19000: REM *************************************************************~
            *  PRINT THE TOTALS FOR THE LAST JOB AND EXIT THE PROGRAM   *~
            *                                                           *~
            *************************************************************

             print using L50100
             call "NUMPRINT" (totalforcus, 2, totalforcus$)
             print using L50500, totalforcus$

             print using L50100
             call "NUMPRINT" (totalforjob, 2, totalforjob$)
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
                      jobnr$,  cuscode$, invoicenr$, invoicedate$,       ~
                      ponumber$, item$, quantity, total, glacct$

            call "DATEFMT" (invoicedate$)
            call "NUMPRINT" (quantity, 2, quantity$)
            call "NUMPRINT" (total, 2, total$)
            return


L30200:     FMT CH(8),                   /* JOB NUMBER                 */~
                XX(8),                   /* SKIP SEQUENCE NUMBER       */~
                CH(9),                   /* CUSTOMER CODE              */~
                CH(8),                   /* CUSTMR INVOICE NUMBER      */~
                CH(6),                   /* CUSTMR INVOICE DATE        */~
                CH(16),                  /* OUR PO NUMBER              */~
                CH(25),                  /* ITEM SOLD TO CUSTOMER      */~
                PD(14,4),                /* QUANTITY ORDERED           */~
                PD(14,4),                /* ITEM TOTAL EXTENDED COST   */~
                CH(9)                    /* GL ACCOUNT NUMBER          */~



        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING. DEVELOPED BY ME, NO     *~
            * THANKS TO "EZPRINT" AND ITS NASTY BUGS (V 1.4)            *~
            *************************************************************

L49980: %PAGE ######         W I P  /  J C    D E T A I L   O F   S A L E~
        ~ S           ####################################################~
        ~###
L50010: %  FOR JOB NUMBER ########  ##################################   ~
        ~   DATE OPENED ########     ###################################
L50100: %+---------+-----------------+--------+----------------+---------~
        ~------------------+----------+----------+---------+
L50300: %!CUSTOMER !  INVOICE NUMBER ! I.DATE !CUS P.O. NUMBER ! ITEM    ~
        ~SOLD              ! QUANTITY !  TOTAL   !G/L ACCT.!
L50500: %!                                                               ~
        ~          TOTAL FOR CUSTOMER  ##########          !
L50600: %                                                           TOTAL~
        ~ FOR JOB NUMBER  ########  IS ##########
L50900: %!#########!#################!########!################!#########~
        ~##################!##########!##########!#########!

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

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
