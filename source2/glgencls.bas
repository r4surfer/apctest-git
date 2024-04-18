        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L       GGG   EEEEE  N   N   CCC   L       SSS    *~
            *  G      L      G      E      NN  N  C   C  L      S       *~
            *  G GGG  L      G GGG  EEEE   N N N  C      L       SSS    *~
            *  G   G  L      G   G  E      N  NN  C   C  L          S   *~
            *   GGG   LLLLL   GGG   EEEEE  N   N   CCC   LLLLL   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLGENCLS - GENERATES ZEROING TRANSACTIONS FOR YEAR END    *~
            *            CLEARING OF ACCOUNTS.  PROCESSES ONE 'STEP' AT *~
            *            A TIME.                                        *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/22/83 ! ORIGINAL                                 ! HES *~
            * 11/11/86 ! Insure Hit On Pack Account Numbers       ! HES *~
            * 04/03/91 ! PRR 11857 Corrected Format Statement for ! SID *~
            *          !  GNJBUF2 File.                           !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            batch$10,                    /* KEY FOR GNJBUFFR           */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            hdr$60,                      /* Header for ASKUSER         */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            postacct$9,                  /* FOR LINE ITEMS             */~
            prevbal(15),                 /* PREVIOUS YEARS BALANCE     */~
            seqnr$3,                     /* SEQUENCE NUMBER FOR LINES  */~
            step$2,                      /* STEP                       */~
            userid$3                     /* USER RUNNING THIS PROGRAM  */

        dim f2%(32),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(32),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(32)20                  /* TEXT FROM FILE OPENING     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! GLMAIN   ! General Ledger Main File                 *~
            * # 3 ! GNJBUFFR ! Gen journal header bfr area              *~
            * # 4 ! GNJBUF2  ! Gen journal detail bfr area              *~
            * # 5 ! GLCLSETO ! Stores G/L close to accounts and step nu *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #02, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #03, "GNJBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   4, keylen =  10,                      ~
                        alt key  1, keypos =    1, keylen =  13

            select #04, "GNJBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  13                      ~

            select #05, "GLCLSETO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   22,                                  ~
                        keypos =   14, keylen =   9,                     ~
                        alt key  1, keypos =   12, keylen =  11,         ~
                            key  2, keypos =    3, keylen =   9, dup,    ~
                            key  3, keypos =    1, keylen =  22          ~

            call "SHOSTAT" ("Preparing to generate closing transactions")

            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, 0%, f2%( 3), 100%, rslt$( 3))
            call "OPENCHCK" (# 4, 0%, f2%( 4), 100%, rslt$( 4))
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, rslt$( 5))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)
            str(line2$,62) = "GLGENCLS: " & str(cms2v$,1,8)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."

                call "READ100" (#1, "FISCAL DATES", f1%(1))
                     if f1%(1) <> 0 then L09320
L09140:         ask% = 2%
                hdr$ = "***** FISCAL DATE ERROR *****"
                call "ASKUSER" (ask%, hdr$, "An error has occurred in Fin~
        ~ding the Fiscal Dates", "Press 'RETURN' to EXIT this program",   ~
                "And Correct the Problem")
                if ask% <> 0% then L09140
                goto L65000

L09320:         get #1, using L09330, monthopen%
L09330:                 FMT XX(20), XX(2), XX(136), BI(2)

            if monthopen% < 12 then inputmode

            hdr$ = "***** CLOSING ERROR *****"
L09370:     ask% = 2%
            call "ASKUSER" (ask%, hdr$, "You CANNOT generate closing " & ~
                "entries until AFTER the year is closed", "Press RETURN"&~
                " to EXIT this Program", "And Close the Fiscal Year")
            if ask% <> 0% then L09370
            goto L65000

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, step$
            return% = 99

            gosub find_a_name
            for fieldnr% = 1 to  1
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then  gosub startover
                  if keyhit%  = 16 then  datasave
                  if keyhit% <>  0 then  L11060
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% >  1 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Closing entry generation in process ( batch ~
        ~name is"& hex(84) & batch$ & hex(8c) & ")")

            hits% = 0%
            plowkey$ = all(hex(00))
            str(plowkey$,,2) = step$

L19140:     call "PLOWALTS" (#5, plowkey$, 1%, 2%, f1%(5)) /*  M A I N  */
                if f1%(5) = 0 then L19470                   /*  P L O W  */
            get #5, using L19170, clsacct$                  /*  L O O P  */
L19170:     FMT XX(2), CH(9)

            call "READ100" (#2, str(plowkey$,3,9), f1%(2))
            get #2, using L19210, prevbal()    /* get prior balances */
L19210:     FMT XX(44), 15*PD(14,4)
            postamt = 0
            for i% = 1 to 15               /* Calc year end balance,   */
                postamt = postamt + prevbal(i%) /*including closing tra*/
            next i%                       /*that may have already postd*/

L19270:     REM Each journal entry can't exceed ABS(9,000,000.00)
            bal = min(abs(postamt),9000000)*sgn(postamt)
            if bal = 0 then L19140        /* Nothing Left To Do */
            postamt = postamt - bal

            hits% = hits% + 1
            if hits% < 801 then L19360
            gosub find_a_name

L19360:     gosub'99 (str(plowkey$,3), -bal)    /* write lines */

            hits% = hits% + 1
            if hits% < 801 then L19420
            gosub find_a_name

L19420:     gosub'99 (clsacct$, bal)            /*  TO BUFFER  */
            return% = 0
            goto L19270

L19470:     if hits% = 0 then gosub clear_buffer  else goto L65160
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20100          /* STEP             */
                     return
L20100:     REM DEFAULT/ENABLE FOR STEP
                step$ = " "
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            gosub clear_buffer
            return clear all
            goto inputmode

        REM *************************************************************~
            *          L O O K S   F O R   A   F R E E   K E Y          *~
            *                                                           *~
            * IN THE GNJBUFFR, THEN ALLOCATES IT.                       *~
            *************************************************************

        find_a_name

            batch$ = "CLOSING-A"
L33090:         call "READ100" (#3, batch$, f1%(3))
                  if f1%(3) = 0 then L33160
L33110:         str(batch$,,9) = add hex(01)

                if batch$ > "CLOSING-Z" then L34070
                goto L33090

L33160:     REM WRITE HEADER TO BUFFER FILE.
                write #3, using L33260, userid$, batch$,                  ~
                "AUTO GENERATED CLOSING ENTRIES", "99", date, "GYE"," "

                call "READ100" (#3, batch$, f1%(3))
                  if f1%(3) = 0 then L33110
                saved_header% = 1

                hits% = 1%

        return

L33260:     FMT CH(3),                   /* USERID                     */~
                CH(10),                  /* NAME OF ENTRY              */~
                CH(36),                  /* DESCRIPTION                */~
                CH(02),                  /* WHICH MODULE               */~
                CH(06),                  /* POSTING DATE               */~
                CH(03),                  /* JOURNAL NAME               */~
                CH(40)                   /* FILLER                     */

        deffn'99 (postacct$, amount)  /* WRITE A LINE ITEM */
                if postacct$ = " " then return
                convert hits% to seqnr$, pic(###)
                debit, credit = amount
                if amount < 0 then debit = 0 else credit = 0
                write #4,  using L33430, batch$,seqnr$,postacct$,batch$,  ~
                                        " "," ","Year End Closing Entry",~
                                        abs(debit), abs(credit), " "

        return

L33430:     FMT CH(10),                  /* ENTRY NAME                 */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* ACCOUNT NUMBER             */~
                CH(30),                  /* REFERENCE 1                */~
                CH(34),                  /* REFERENCE 2                */~
                CH(4),                   /* ADJ-FLAG, G/L Adj Posting  */~
                CH(32),                  /* DESCRIPTION THIS LINE      */~
                PD(14,4),                /* DEBIT AMOUNT               */~
                PD(14,4),                /* CREDIT AMOUNT              */~
                CH(22)                   /* FILLER                     */


        clear_buffer
             call "DELETE" (#3, batch$, 8%)
             call "DELETE" (#4, batch$, 8%)
             saved_header% = 0
             return

L34070:     hdr$ = "***** MAXIMUM STEP ERROR *****"
L34080:     ask% = 2%
            call "ASKUSER" (ask%, hdr$, "CANNOT handle more than 26 " &  ~
                "Steps at this time.", " ", "Press RETURN to exit " &    ~
                "Program.")
            if ask% <> 0% then L34080
            goto L65000

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

        FMT                      /* FILE: GNJBUF2                      */~
            XX(10),              /* general journal entry name         */~
            CH(3),               /* sequence number for gnjbuf2        */~
            CH(9),               /* general ledger account number      */~
            CH(32),              /* REFERENCE 1                        */~
            CH(32),              /* REFERENCE 2                        */~
            CH(36),              /* general ledger account description */~
            PD(14,4),            /* debit amount                       */~
            PD(14,4),            /* credit amount                      */~
            XX(32)               /* filler for rest of record or inter */~

        FMT                      /* FILE: GLCLSETO                     */~
            CH(2),               /* closing step number.               */~
            CH(9),               /* account to close to                */~
            CH(2),               /* same as step                       */~
            CH(9)                /* G/L account number.                */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40170          /* STEP             */
                     goto L40210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40170:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40210: accept                                                           ~
               at (01,02), "GENERATE YEAR END CLOSING TRANSACTIONS",     ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$,                   ch(08),~
               at (02,02), fac(hex(ac)), line2$,                  ch(79),~
               at (06,22),                                               ~
        "Please indicate the desired step number",                       ~
               at (07,41), fac(lfac$( 1)), step$                , ch(02),~
               at (09,12),                                               ~
        "Only accounts with the above step number will be processed.",   ~
               at (10,08),                                               ~
        "Two transactions will be generated for *each* account in this st~
        ~ep.",                                                            ~
               at (11,09),                                               ~
        "The first will make the account's beginning of year balance zero~
        ~,",                                                              ~
               at (12,09),                                               ~
        "the other will post the corrosponding 'close to' account with th~
        ~e",                                                              ~
               at (13,12),                                               ~
        "same (reversed) amount.  After this program has completed,",    ~
               at (14,08),                                               ~
        "you must run GLDIRECT (Direct Journal Entry) to post the generat~
        ~ed",                                                             ~
               at (15,08),                                               ~
        "tranactions. At that time you may also modify them.  The batch n~
        ~ame",                                                            ~
               at (16,33), "will be",                                    ~
               at (16,41), fac(hex(84)), batch$                 , ch(10),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40620
                  call "MANUAL" ("GLGENCLS")
                  goto L40210

L40620:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41170          /* STEP             */
                     goto L41210

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41170:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41210:     accept                                                       ~
               at (01,02), "GENERATE YEAR END CLOSING TRANSACTIONS",     ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$,                   ch(08),~
               at (02,02), fac(hex(ac)), line2$,                  ch(79),~
               at (06,22),                                               ~
        "Please indicate the desired step number",                       ~
               at (07,41), fac(lfac$( 1)), step$                , ch(02),~
               at (09,12),                                               ~
        "Only accounts with the above step number will be processed.",   ~
               at (10,08),                                               ~
        "Two transactions will be generated for *each* account in this st~
        ~ep.",                                                            ~
               at (11,09),                                               ~
        "The first will make the account's beginning of year balance zero~
        ~,",                                                              ~
               at (12,09),                                               ~
        "the other will post the corrosponding 'close to' account with th~
        ~e",                                                              ~
               at (13,12),                                               ~
        "same (reversed) amount.  After this program has completed,",    ~
               at (14,08),                                               ~
        "you must run GLDIRECT (Direct Journal Entry) to post the generat~
        ~ed",                                                             ~
               at (15,08),                                               ~
        "tranactions. At that time you may also modify them.  The batch n~
        ~ame",                                                            ~
               at (16,33), "will be",                                    ~
               at (16,41), fac(hex(84)), batch$                 , ch(10),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)PROCESS     ",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41620
                  call "MANUAL" ("GLGENCLS")
                  goto L41210

L41620:        if keyhit% <> 15 then L41660
                  call "PRNTSCRN"
                  goto L41210

L41660:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* STEP             */
                     return
L50100:     REM TEST DATA FOR STEP
                convert step$ to step%, data goto L50200
                if step% < 1 then L50200
                convert step% to step$, pic(00)
                return
L50200:         errormsg$ = "Invalid entry for step number"
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            gosub clear_buffer

L65160:     call "SHOSTAT" ("Closing Files, One Moment Please")

            end return%
