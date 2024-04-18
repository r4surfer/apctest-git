        REM *************************************************************~
            *                                                           *~
            *   CCC   H   H  K   K  U   U  DDDD    OOO   IIIII  TTTTT   *~
            *  C   C  H   H  K  K   U   U  D   D  O   O    I      T     *~
            *  C      HHHHH  KKK    U   U  D   D  O   O    I      T     *~
            *  C   C  H   H  K  K   U   U  D   D  O   O    I      T     *~
            *   CCC   H   H  K   K   UUU   DDDD    OOO   IIIII    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHKUDOIT - PAY SPECIFIC INVOICES ONLY.  ALSO SERVES AS THE*~
            *            BUFFER EDIT PROGRAM FOR CHECK GENERATION.      *~
            *----------------------------------------------------------Q*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/30/80 ! ORIGINAL (FROM CSHINPUT)                 ! BCW *~
            * 06/02/81 ! ADDED 16 CHAR INVOICE NUMBER             ! TOM *~
            * 07/16/81 ! DELETE FROM BUFFER AT WILL               ! TEM *~
            * 07/23/81 ! FIX RECALL OF OLD INVOICE ON FILE INFO   ! TEM *~
            * 10/01/81 ! NO RECALL FROM MAIN FILE                 ! TEM *~
            * 03/17/82 ! ADD FEATURES AND CLEAN UP                ! GLW *~
            * 03/29/82 ! TOOK INTO CONSIDERATION THE NON-DISC.AMT.! BEV *~
            * 03/11/83 ! CONTOL NUMBER ASSIGNMENT                 ! KEN *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 10/01/84 ! CALLS TO 'DESCRIBE' IN EDIT TO 'GETCODE' ! JWG *~
            *          !  ELIM CHANCE OF TWO CHECKS SAME INVOICE  !     *~
            * 12/16/85 ! Vendor file format changes               ! MJB *~
            * 01/09/86 ! 1099 CATEGORY                            ! KAB *~
            * 04/10/86 ! CORRECTED PF 16 PROMPT AND CLEAR INFOMSG ! WPH *~
            *          !   ELIM DEFAULT DISCOUNT UNLESS FIRST PAY !     *~
            *          !   AND BROUGHT TO STANDARD                !     *~
            * 05/19/86 ! Invoice File Format Changes              ! HES *~
            * 02/05/88 ! Test for non-statutory currency          ! KAB *~
            * 08/01/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            aa$10, nn$10, dd$10,                                         ~
            askhdr$40,                                                   ~
            askpf1$80,                                                   ~
            askmid$80,                                                   ~
            askpf2$80,                                                   ~
            acct$(100)16,                /* DEBIT ACCOUNT NUMBERS      */~
            amount$(100)10,              /* DEBIT AMOUNTS              */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* BLANK LINE ON SCREENS      */~
            cashacct$16,                 /* CASH IN BANK ACCOUNT NUMBER*/~
            cashacctdescr$32,            /* CASH IN BANK ACCT DESCRIPTN*/~
            checkdate$8,                 /* DATE OF THIS CHECK         */~
            checknr$8,                   /* CHECK NUMBER THIS CHECK    */~
            chkchk$8,                    /* FILE#10 LAST CHECK NUMBER  */~
            cshchk$8,                    /* FILE#11 LAST CHECK NUMBER  */~
            chkdte$8,                    /* CHECK DATE FOR DATUNFMT    */~
            currkey$50,                  /* CURRENCY PLOW KEY          */~
            cursor%(2),                  /* CURSOR LOCATIONS FOR EDITS */~
            d19020101$8,                 /* Another date for comparison*/~
            date$8,                      /* SCREEN DISPLAY DATE INFO   */~
            descr$(100)32,               /* SCREEN ACCOUNT DESCRIPTIONS*/~
            defcashacct$9,               /* CASH IN BANK ACCOUNT NUMBER*/~
            defdiscacct$9,               /* DISCOUNTS TAKEN ACCOUNT    */~
            defpuracct$9,                /* DEFAULT PURCHASES          */~
            disc$(100)10,                /* DISCOUNTS EACH LINE ITEM   */~
            discacct$16,                 /* DISCOUNTS TAKEN ACCOUNT    */~
            discacctdescr$32,            /* DISCOUNT ACCT DESCRIPTION  */~
            discdate$8,                  /* DATE TO PAY WITH DISCOUNT  */~
            discount$10,                 /* DISCOUNT AMOUNT THIS CHECK */~
            diskkey$50,                  /* KEY TO READ DATA WITH      */~
            edtmessage$79,               /* "TO MODIFY VALUES" TEXT    */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(5,20)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            header$79,                   /* Screen Title               */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            invdate$(100)6,              /* INVOICE DATES (UNFORMATTED)*/~
            invoice$(100)16,             /* INVOICE NUMBERS FOR DETAILS*/~
            key$11,                      /* KEYS TO HEADER FILE        */~
            lastcheck$8,                 /* LAST CHECK NUMBER INPUT    */~
            lastven$9,                   /* LAST VENDOR INPUT          */~
            linefac$(20)1,               /* FAC'S FOR LINEAR INPUT     */~
            numchar$3,                   /* 3 CHARACTER PRINT NUMBER   */~
            oldreadkey$50,               /* OLD KEY FOR PLOW ROUTINES  */~
            pfkeys$(4)17,                /* PF KEYS FOR TABULAR SCREEN */~
            postdate$8,                  /* PAYABLES DATE (SCREEN FMT) */~
            readkey$50,                                                  ~
            work$32,                                                     ~
            reversekey$11,               /* REVERSE OF BUFFER KEY      */~
            seq$3,                       /* SEQUENCE NUMBERS FOR FILE  */~
            seq$(100)3,                  /* LINE NUMBERS ON SCREEN     */~
            separator$(5)79,             /* SEPERATOR LINES FOR TABULR */~
            temp$50,                     /* FILLER AND SUCH            */~
            ttl$(4,2)64,                 /* TITLES FOR TABULAR SCREEN  */~
            tran$(24)80,                 /* FOR EDIT COMPUTATIONS      */~
            type$(100)1,                 /* ACCOUNT TYPES DEBIT ACCTS  */~
            ten99$(100)4,                /* 1099 CATEGORIES            */~
            userid$3,                    /* USERID OF CURRENT USER     */~
            vencode$9,                   /* VENDOR CODE THIS VENDOR    */~
            venname$32                   /* VENDOR NAME                */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE (ACCT DESCR'S)  *~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN FILE                       *~
            * # 9 ! CHKBUFFR ! CHECK GENERATION CHECK HEADERS           *~
            * #10 ! CHKBUF2  ! CHECK GENERATION CHECK DETAILS           *~
            * #11 ! CSHBUF2  ! DISBURSEMENTS DETAIL BUFFER              *~
            * #12 ! SYSFILE2 ! SYSTEMS SECRETS FILE                     *~
            * #13 ! GENCODES ! General System Codes File                *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #9, "CHKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen =  7,                         ~
                        alternate key 1, keypos= 8, keylen= 7,           ~
                                  key 2, keypos=24, keylen= 8

            select #10, "CHKBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select #11, "CSHBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select #12, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #13, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            select #41,  "PAYLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 28,                      ~
                        alt key  1, keypos =   1, keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1 , 0%, 0%,   0%, " ")
            call "OPENCHCK" (#2 , 0%, 0%,   0%, " ")
            call "OPENCHCK" (#3 , 0%, 0%,   0%, " ")
            call "OPENCHCK" (#5 , 0%, 0%,   0%, " ")
            call "OPENCHCK" (#9 , 0%, 0%, 100%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#11, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#12, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#13, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#41, 0%, 0%,   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES VARIABLES THAT WE WILL LATER USE.  SETS       *~
            * PAYABLES DATE, EDITING INFORMATION, AND MORE.             *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            d19020101$ = "19020101"
            call "DATECONV" (d19020101$)

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)

            call "READ100" (#1, userid$, f1%(1))
                 if f1%(1) <> 0 then L09180
                 call "ASKUSER" (2%, "Sorry", " ",                       ~
                      "You Are Not A Valid User In This Database",       ~
                                               "Press (RETURN) To Exit.")
                 goto L65000

L09180:     get #1, using L09190, postdate$
L09190:         FMT XX(9), CH(6)
            call "WHICHPER" (#12, postdate$, thismonth%)
            if thismonth% > 0% then L09280
                call "ASKUSER" (0%, "INVALID POSTING DATE",              ~
                     "Your Posting Date is not within the posting wi" &  ~
                     "ndow.",                                            ~
                     "Please change your posting date & try again.",     ~
                     "Press RETURN to acknowledge and Exit.")
                goto exit_program
L09280:     call "DATEFMT" (postdate$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            REM SET SCREEN TITLES
              ttl$(1,1) = "(1)Start Over (2)Col 1 (4)Line Above (13)Instr~
        ~   (16)EDITMODE"
              ttl$(1,2) = "Enter Invoices Paid By This Check"
              ttl$(2,1) = "(1)Start Over(2)First(3)Last(4)Prev(5)Next(6)D~
        ~own(7)Up"
              ttl$(2,2) = "(9)Header(11)Inst(12)Del (13)Instr (15)Prt Scr~
        ~n   (16)SUMMARY"
              ttl$(3,1) = "Supply Requested Items and RETURN  or (1) To E~
        ~xit Insert Mode"
              ttl$(4,1) = "Press RETURN To DELETE Flashing Line or (1) To~
        ~ EXIT Delete."

            pfkeys$(1) = hex(000102040d0f10ffffffffffffffffffff)
            pfkeys$(2) = hex(0001020304050607090b0c0d0f10ffffff)
            pfkeys$(3) = hex(00010fffffffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010fffffffffffffffffffffffffffff)

            REM SET TRAN$ FOR EDIT MODE FIELD NUMBER COMPUTATION.
                init(hex(00)) str(tran$(),     1)
                init(hex(01)) str(tran$(6),  6, 23)
                init(hex(02)) str(tran$(6), 27, 13)
                init(hex(03)) str(tran$(7),  1, 22)
                init(hex(04)) str(tran$(7), 30, 20)
                init(hex(05)) str(tran$(7), 70,  4)

                copy str(tran$(), 321, 1280) to str(tran$(), 641)

            call "READ100" (#12, "MODULE.DEFAULTS.AP", f1%(12))
                if f1%(12) = 0% then L10000
            get #12, using L09630, defpuracct$, defcashacct$, defdiscacct$
L09630:         FMT XX(20), XX(4), XX(4), XX(8), CH(9), XX(18), 2*CH(9)

L10000: REM *************************************************************~
            *                  I N P U T   H E A D E R                  *~
            *                                                           *~
            * INPUTS HEADER, LOADS OLD CHECKS IF ON FILE, ETC...        *~
            *************************************************************

        inputmode
            init(" ") vencode$, venname$, checknr$, errormsg$,           ~
                      checkdate$, discount$, discacct$,key$,temp$,       ~
                      discacctdescr$, cashacct$, cashacctdescr$,         ~
                      invoice$(), acct$(), descr$(), amount$(), seq$(),  ~
                      disc$(), invdate$(),blankline$,type$(),invdate$(), ~
                      ten99$()

            editmode%, delete_it% = 0

            for fieldnr% = 1 to 6
                gosub'160(fieldnr%)
                      if enabled% = 0 then L10240
L10190:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover2
                      if keyhit%  =  2 then editlines
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10190
L10240:         gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10190
                next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

            init(" ") seq$(), invoice$(), acct$(), descr$(),             ~
                      amount$(), disc$(), type$(), ten99$(),             ~
                      invdate$(), errormsg$, infomsg$
            maxlines%, line%, screenline%, currentline% = 0

L11110:     screenline% = screenline% + 1
            infomsg$ = " "
            if screenline% < 6 then L11160
               line% = line% + 5
               screenline% = 1
L11160:     currentline% = line% + screenline%
            if currentline% > 100 then editmode
            call "SETSEP" (separator$(), line%, screenline%)

          gosub input_lines
            if keyhit% <> 16 then goto L11110
                gosub columnone
                goto editlines


        input_lines
            for fieldnr% = 1 to 5
L11290:         gosub'163(fieldnr%)      /* GET SPECIAL DEFAULTS.      */
                      if enabled% =  0 then       L11390
L11310:         gosub'203(screenline%, fieldnr%)
                      if keyhit%  =  0 then       L11390
                      if keyhit%  =  1 then gosub startover1
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  4 then gosub lineabove
                      if keyhit%  =  9 then inputmode
                      if keyhit%  = 16 and fieldnr% = 1 then return
                         goto L11290
L11390:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L11310
                next fieldnr%
                maxlines% = maxlines% + 1
                convert currentline% to seq$(currentline%), pic(###)
                return

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            *                                                           *~
            * EDITS HEADER INFORMATION, COMPUTING DISCOUNT FROM LINE ITM*~
            *************************************************************

        editmode
            gosub total_up_check
            editmode% = 1      /* FLAG THAT WE'RE EDITING FOR DATA TEST*/

L12100:     gosub'202(0%)                /* SHOW WITH NON-MODIFIABLE   */
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover2
                  if keyhit%  =  2 then       editlines
                  if keyhit%  = 12 then gosub delete_it
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L12100
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 6 then L12100
            if fieldnr% = 1 then L12100 /* USER CANT CHG VEN CODE     */
            if fieldnr% = 2 then L12100 /* USER CANT CHG CONTR NUMBER */
            if fieldnr% = 4 then L12100 /* USER CANT CHG HDR DISC AMT */

            REM FIELD IS GOOD
L12240:         gosub'202(fieldnr%)
                      if keyhit% =  1 then gosub startover2
                      if keyhit% <> 0 then       L12240
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L12240
                goto L12100

        REM *************************************************************~
            *             E D I T   L I N E    I T E M S                *~
            *                                                           *~
            * THIS SECTION EDITS THE LINE ITEMS                         *~
            *************************************************************

        editlines
            line%, currentline%, screenline% = 0
            errormsg$, infomsg$ = " "
            call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))

L12430:     gosub'213(0%, 0%)
                  if keyhit%  =  0 then       L12620
                  if keyhit%  = 16 then       editmode
                  if keyhit%  =  1 then gosub startover1
                  if keyhit%  =  2 then line% = 0
                  if keyhit%  =  3 then line% = max(0,maxlines%-5)
                  if keyhit%  =  4 then line% = max(0,line%-4)
                  if keyhit%  =  5 then line% = min(line%+4,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  6 then line% = max(0,line%-1)
                  if keyhit%  =  7 then line% = min(line%+1,max(0,       ~
                                                maxlines%-5))
                  if keyhit%  =  9 then       editmode
                  if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode

                  call"SETSEP"(separator$(),line%,min(maxlines%-line%,5%))
                  goto L12430

L12620:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                fieldnr% = val(str(tran$(cursor%(1)),cursor%(2)))
                if fieldnr% = 0 or fieldnr% = 1 then L12430
                screenline% = (cursor%(1)-5)/4+1
                currentline% = line% + screenline%
                if currentline% > maxlines% then L12430

L12690:         gosub'213(screenline%, fieldnr%)
                      if keyhit%  = 1 then gosub startover1
                      if keyhit% <> 0 then L12690
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L12690
                goto L12430

        REM *************************************************************~
            *  C O L U M N   O N E ,   L I N E   A B O V E   L O G I C  *~
            *                                                           *~
            * CONTAINS THE CODE FOR COLUMN ONE AND LINE ABOVE FUNCTIONS.*~
            *************************************************************

        lineabove
            if currentline% = 1 then return
            c% = currentline%
            on fieldnr% gosub L13160,               /* INVOICE NUMBER   */~
                              L13170,               /* ACCOUNT NUMBER   */~
                              L13200,               /* AMOUNT           */~
                              L13210,               /* DISCOUNT         */~
                              L13220                /* 1099 CODE        */
            return

L13160:     invoice$ (c%) = invoice$ (c%-1): return
L13170:     acct$    (c%) = acct$    (c%-1)
            descr$   (c%) = descr$   (c%-1)
            type$    (c%) = type$    (c%-1): return
L13200:     amount$  (c%) = amount$  (c%-1): return
L13210:     disc$    (c%) = disc$    (c%-1): return
L13220:     ten99$   (c%) = ten99$   (c%-1): return

        columnone
            c% = currentline%
            if c% <= 0 then return
            init(" ") seq$(c%), invoice$(c%), acct$(c%),  type$(c%),     ~
                      amount$(c%), disc$(c%), descr$(c%), invdate$(c%),  ~
                      ten99$(c%), errormsg$, infomsg$
            fieldnr% = 1
            return

        delete_it
L13350:     askhdr$ = "* * * W A R N I N G * * *"
            askpf1$ = "This Check Is About To Be Deleted From The File"
            askmid$ = "Press PF1 To Cancel Delete And Return To Edit"
            askpf2$ = "Press (RETURN) To DELETE CHECK"
            keyhit% = 2%
            call "ASKUSER" (keyhit%, askhdr$, askpf1$, askmid$, askpf2$)
                if keyhit% = 0% then L13450
                if keyhit% = 1% then return
                   goto L13350

L13450:     return clear
            delete_it% = 1
            gosub L31000
            goto inputmode

        REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *                                                           *~
            * HANDLES INSERTION OF A LINE ITEM INTO THE INVOICE.        *~
            *************************************************************

        insertmode
            if maxlines% = 100 then return         /* ARRAY FULL, CAN'T*/
            REM OTHERWISE, SET CURRENTLINE%, SCREENLINE%, AND COPY RIGHT
                screenline% = int((cursor%(1)-1)/4)
                if line% + screenline% < maxlines% then L14120
                   screenline% = maxlines% - line% /* TO INS AT END    */
L14120:         if screenline% <> 5 then L14170     /* BOTTOM OF PAGE   */
                   line% = line% + 1
                   screenline% = screenline% - 1
                   goto L14170

L14170:         call "SETSEP" (separator$(),line%,                       ~
                                      min(5%, maxlines%-line%+1%))
                currentline%, c% = screenline% + line%

            REM COPY ALL THE ELEMENTS UP ONE
                if c% >= maxlines% then L14340
                for temp% = maxlines% to c% step -1
                    invoice$ (temp%+1) = invoice$ (temp%)
                    acct$    (temp%+1) = acct$    (temp%)
                    descr$   (temp%+1) = descr$   (temp%)
                    amount$  (temp%+1) = amount$  (temp%)
                    disc$    (temp%+1) = disc$    (temp%)
                    type$    (temp%+1) = type$    (temp%)
                    invdate$ (temp%+1) = invdate$ (temp%)
                    ten99$   (temp%+1) = ten99$   (temp%)
                    next temp%

L14340:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1

                init(" ") invoice$(c%), acct$(c%), amount$(c%),          ~
                ten99$(c%), disc$(c%), type$(c%), invdate$(c%), descr$(c%)

            gosub  input_lines
               if keyhit% <> 16% then L14460
                 gosub columnone
                 goto L14550

L14460:         REM RENUMBER ITEM NUMBERS, THEN SET CURSOR%(1)
                    if invoice$(currentline%) = " " then L14520
                       for temp% = 1 to max(maxlines%, currentline%+1)
                           if invoice$(temp%) = " " then L14510
                              convert temp% to seq$(temp%), pic(###)
L14510:                    next temp%
L14520:         cursor%(1) = min(cursor%(1)+4, 24)
                goto insertmode

L14550:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                c% = currentline%
                gosub L14690              /* ACTUALLY DELETE @C%        */

                temp% = maxlines% + 1
                init(" ") seq$(temp%), invoice$(temp%), type$(temp%),    ~
                          acct$(temp%), amount$(temp%), invdate$(temp%), ~
                          disc$(temp%), descr$(temp%), ten99$(temp%),    ~
                          errormsg$, infomsg$

            if currentline% >= maxlines% and screenline% = 5             ~
               then line% = max(0%, line% - 1%)
            return

L14690:     for temp% = currentline% to maxlines%
                invoice$  (temp%) = invoice$  (temp%+1)
                acct$     (temp%) = acct$     (temp%+1)
                descr$    (temp%) = descr$    (temp%+1)
                amount$   (temp%) = amount$   (temp%+1)
                disc$     (temp%) = disc$     (temp%+1)
                type$     (temp%) = type$     (temp%+1)
                invdate$  (temp%) = invdate$  (temp%+1)
                ten99$    (temp%) = ten99$    (temp%+1)
                seq$      (temp%) = seq$      (temp%+1)
                next temp%

            if maxlines% = 0 then return
               for temp% = 1 to max(maxlines%, currentline%)
                   if invoice$(temp%) = " " then L14850
                      convert temp% to seq$(temp%), pic(###)
L14850:            next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *                                                           *~
            * DELETES A LINE ITEM FROM THE INVOICE.  NOTE THAT WE HAVE  *~
            * TO COPY  THE ROUTINE FROM INSERT MODE THAT COPIES ALL THE *~
            * DATA BACK DOWN ONE LINE, WHICH WE WILL USE HERE TO DELETE *~
            * THE ACTUAL LINE WE WANT TO.  THIS MAKES IT EASIER TO ADD  *~
            * FIELDS TO THE LINE ITEMS SINCE THERE'S ONLY ONE DELETE    *~
            * ROUTINE, ALTHOUGH IT BREAKS UP THE STRUCTURE OF THE CODE. *~
            *************************************************************

        deletemode
                if maxlines% = 0 then return
                screenline% = int((cursor%(1)-1)/4)
                if screenline% < 1 then return
                currentline% = screenline% + line%
                if currentline% > maxlines% then return

L15180:         gosub'233(screenline%)
                      if keyhit%  =  1 then       return
                      if keyhit% <>  0 then       L15180

                c% = currentline%
                if currentline% < maxlines% then gosub L14690
                                         /* ACTUALLY DELETE LINE @C%   */
                temp% = maxlines%
                init(" ") seq$(temp%), invoice$(temp%), invdate$(temp%), ~
                          acct$(temp%), amount$(temp%), disc$(temp%),    ~
                          errormsg$, infomsg$, descr$(temp%),            ~
                          type$(temp%), ten99$(temp%)

                maxlines% = maxlines% - 1
                if currentline% >= maxlines% and screenline% = 5         ~
                   then line% = max(0%, line% - 1%)
                return

        REM *************************************************************~
            *                 T O T A L   C H E C K                     *~
            *                                                           *~
            *************************************************************

        total_up_check
            checktotal,disctotal,amttot = 0 : if maxlines% = 0 then L17140
            for temp% = 1 to maxlines%
                convert amount$(temp%) to amount
                convert disc$  (temp%) to discount
                amttot =  round(amttot + amount, 2)
                checktotal = round(checktotal + amount - discount, 2)
                disctotal  = round(disctotal  + discount, 2)
            next temp%
L17140:     call "CONVERT" (amttot, 2.2, aa$)
            call "CONVERT" (checktotal, 2.2, nn$)
            call "CONVERT" (disctotal, 2.2, dd$)
        return

        REM *************************************************************~
            *                   W R I T E  D A T A                      *~
            *                                                           *~
            *************************************************************

        datasave
            REM Check Check Total.
            gosub total_up_check
                if checktotal > 0 then L19110
                errormsg$= "Negative Or Zero Checks Cannot Be Written"
                goto editmode
L19110:     REM NOW SAVE THE INVOICE ON THE BUFFER.
                gosub L31000    /* WRITE DATA, DELETING OLD INV ON BUFFR*/

            REM SET LAST VENDOR/CHECK NUMBERS
                lastven$ = vencode$
                lastcheck$ = checknr$
                goto inputmode

        REM *************************************************************~
            *      I N P U T   E N A B L E   H E A D E R   I N F O      *~
            *                                                           *~
            * SETS DEFAULTS FOR THE HEADER INFORMATION.                 *~
            *************************************************************

            deffn'160(fieldnr%)
                  enabled% = 0: infomsg$ = " "
                  on fieldnr% gosub L20150,         /* VENDOR CODE      */~
                                    L20180,         /* CHECK NUMBER     */~
                                    L20230,         /* CHECK DATE       */~
                                    L20270,         /* DISCOUNT AMOUNT  */~
                                    L20300,         /* DISCOUNT ACCOUNT */~
                                    L20370          /* CASH IN BANK ACCT*/
                  return
L20150:     REM SET DEFAULTS FOR VENDOR CODE INFORMATION.
                enabled% = 1
                return
L20180:     REM SET DEFAULTS FOR NEXT CHECK NUMBER.
                enabled% = 1
                infomsg$ = "Leave Control # Blank, Unless You Wish To Rec~
        ~all A Previously Entered Check."
                return
L20230:     REM SET DEFAULTS FOR CHECK DATE
                enabled% = 1
                checkdate$ = postdate$
                return
L20270:     REM SET DEFAULTS FOR DISCOUNT AMOUNT
                enabled% = 0
                return
L20300:     REM SET DEFAULTS FOR DISCOUNT ACCOUNT THIS GUY.
                get #3, using L20320, discacct$
L20320:                 FMT XX(276), CH(9)
                if discacct$ = " " then discacct$ = defdiscacct$
                call "DESCRIBE"(#2, discacct$, discacctdescr$, 1%, f1%(2))
                call "GLFMT" (discacct$)
                return
L20370:     REM SET DEFAULTS FOR CASH IN BANK ACCOUNT THIS GUY.
                get #3, using L20390, cashacct$
L20390:                 FMT XX(267), CH(9)
                if cashacct$ = " " then cashacct$ = defcashacct$
                call "DESCRIBE"(#2, cashacct$, cashacctdescr$, 1%, f1%(2))
                call "GLFMT" (cashacct$)
                return

        REM *************************************************************~
            *   I N P U T   E N A B L E   T A B U L A R   F I E L D S   *~
            *                                                           *~
            * SETS UP AND ENABLES TABULAR INPUT FIELDS.  GIVES DEFAULTS *~
            * FOR ITEMS SUCH AS THE DEBIT ACCOUNT, AND DISCOUNT AMOUNT. *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  c% = currentline%
                  on fieldnr% gosub L21160,         /* INVOICE NUMBER   */~
                                    L21210,         /* DEBIT ACCOUNT #  */~
                                    L21390,         /* DEBIT AMOUNT     */~
                                    L21510,         /* DISCOUNT AMOUNT  */~
                                    L21610          /* 1099 CATEGORY    */
                  return
L21160:     REM INPUT ENABLE FOR INVOICE NUMBER.
                if currentline% = 100 then infomsg$ = "This Is The Last L~
        ~ine Available"
                enabled% = 1
                return
L21210:     REM INPUT ENABLE FOR DEBIT ACCOUNT NUMBER.
                if currentline% = 100 then infomsg$ = "This Is The Last L~
        ~ine Available"
                if f1%(5) <> 0 then L21300
                     enabled% = 1
                     get #3, using L21270, acct$(c%)
L21270:              FMT XX(249), CH(9)
                     if acct$(c%) = " " then acct$(c%) = defpuracct$
                     goto L21330
L21300:         get #5, using L21310, acct$(c%)
L21310:         FMT XX(56), CH(9)
                enabled% = 0
L21330:         REM Describe & get type...
                call "DESCRIBE" (#2, acct$(c%), descr$(c%), 0%, f1%(2))
                if f1%(2) <> 0 then get #2, using L21360, type$(c%)
L21360:         FMT XX(39), CH(1)
                call "GLFMT" (acct$(c%))
                return
L21390:     REM INPUT ENABLE FOR AMOUNT OF THING.
                if currentline% = 100 then infomsg$ = "This Is The Last L~
        ~ine Available"
                enabled% = 1
                if f1%(5) = 0 then return
                get #5, using L21450, temp
L21450:         FMT XX(118), PD(14,4)
                REM See If There's Any Checks In CHKBUF2 &/or CSHBUF2
                gosub L53000
                temp = temp - bufchecktotal
                call "CONVERT" (temp, -2.2, amount$(c%))
                return
L21510:     REM INPUT ENABLE FOR DISCOUNT AMOUNT THIS LINE.
                if currentline% = 100 then infomsg$ = "This Is The Last L~
        ~ine Available"
                if amount$(c%) <> "0.00" then enabled% = 1
                if f1%(5) = 0 then L21580
                   gosub L60000           /* COMPUTE DISCOUNT AMT PROPER*/
                   return
L21580:         disc$(c%) = "0.00"
                return

L21610: REM DEFAULT ENABLE FOR 1099 CATEGORY
            if f1%(5) = 0% then L21670
               get #5, using L21640, ten99$(c%)
L21640:            FMT XX(162), CH(4)
               return

L21670:        get #3, using L21680, ten99$(c%)
L21680:            FMT XX(514), CH(4)
               if ten99$(c%) <> " " then enabled% = 1
               return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover1: REM ALLOW USER OPPORTUNITY TO START OVER.
            keyhit1% = 1%
            goto L29955

        startover2: REM ALLOW USER OPPORTUNITY TO START OVER.
            keyhit1% = 2%

L29955:     call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *      L O A D   O L D   C H E C K   F R O M   F I L E      *~
            *                                                           *~
            * CALLED FROM DATA TEST ROUTINE, THIS ROUTINE WILL LOAD UP  *~
            * THE OLD CHECK PROVIDED IT HAS BEEN LOADED FROM THE DISK   *~
            * INTO THE BUFFER.  NOTE THAT THIS DOES A RETURN SO THAT WE *~
            * CAN USE IT ELSEWHERE FROM THE DATA TEST ROUTINE, WHICH    *~
            * HANDLES THE RETURN CLEAR AND THE BRANCH TO EDIT MODE.     *~
            *************************************************************

            oldcheckonfile% = 0

            call "REDALT0" (#9, checknr$, 2%, f1%(9))
                  if f1%(9) = 0 then return
            REM GET CHECK OFF BUFFER.
                get #9, using L30142, tempven$
L30142:               FMT XX(14), CH(9)
                if tempven$=vencode$ then L30150
                    errormsg$="Check Assigned To Vendor: " & tempven$
                    return
L30150:         oldcheckonfile% = 1
                get #9, using L30500,key$,reversekey$,vencode$,checkdate$,~
                        discount, discacct$, cashacct$
                call "DATEFMT" (checkdate$)
                call "DESCRIBE"(#3, vencode$,  venname$,       1%, f1%(3))
                call "DESCRIBE"(#2, discacct$, discacctdescr$, 1%, f1%(2))
                   call "GLFMT" (discacct$)
                call "DESCRIBE"(#2, cashacct$, cashacctdescr$, 1%, f1%(2))
                   call "GLFMT" (cashacct$)
                call "CONVERT"(discount, -2.4, discount$)

            REM PLOW ROUTINE TO GET CHECK LINE ITEMS.
                diskkey$ = vencode$
                str(diskkey$, 10) = checknr$
                maxlines% = 0

L30290:         call "PLOWNEXT" (#10, diskkey$, 17%, f1%(10))
                     if f1%(10) = 0 then return
                maxlines%, temp% = maxlines% + 1
                get #10, using L30420, invoice$(temp%), acct$(temp%),     ~
                         type$(temp%), amount, discount, invdate$(temp%),~
                         ten99$(temp%)

                REM NOW FORMAT INFORMATION.
                    call "CONVERT" (amount, -2.2, amount$(temp%))
                    call "CONVERT" (discount, -2.2, disc$(temp%))
                    call "DESCRIBE" (#2, acct$(temp%), descr$(temp%),    ~
                                                   0%, f1%(2))
                    call "GLFMT" (acct$(temp%))
                    convert temp% to seq$(temp%), pic(###)
                goto L30290

L30420:                 FMT XX(20),      /* SKIP VEN#, CHK#, SEQ#      */~
                            CH(16),      /* INVOICE NUMBER             */~
                            CH(9),       /* DEBIT  ACCOUNT             */~
                            CH(1),       /* TYPE CODE (DIRECT OR RECVBL*/~
                            PD(14,4),    /* AMOUNT                     */~
                            PD(14,4),    /* DISCOUNT AMOUNT            */~
                            CH(6),       /* INVOICE DATE INFORMATION   */~
                            CH(4)        /* 1099 CATEGORY              */~

L30500:                 FMT 2*CH(07),    /* FWD & REVERSE KEYS         */~
                            CH(9),       /* VENDOR CODE                */~
                            XX(8),       /* SKIP CHECK NUMBER          */~
                            CH(6),       /* DATE OF CHECK (YYMMDD)     */~
                            PD(14,4),    /* DISCOUNT AMOUNT            */~
                            CH(9),       /* DISCOUNT ACCOUNT           */~
                            CH(9)        /* CASH IN BANK ACCOUNT       */

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *                                                           *~
            * WRITE DATA TO FILE, DELETING OLD FROM BUFFER SHOULD THAT  *~
            * BE NECESSARY.                                             *~
            *************************************************************

            REM DELETE OLD CHECK FROM BUFFER JUST BEFORE SO NO PROBLEM
                REM DELETE HEADER AND LINE ITEMS USING RT-JUSTIFIED CK #
                    oldreadkey$ = vencode$
                    str(oldreadkey$,10) = checknr$
                    call "DELETE" (#10, oldreadkey$, 17%)
                REM LOOK IF OLD CHECK IN BUFFER. IF NOT INCREMENT NUMBER
            call "REDALT1" (#9, checknr$, 2%, f1%(9))
                  if f1%(9) <> 0 then delete #9
                    if delete_it% = 1 then return

            REM SET UP KEYS FOR WRITING TO BUFFER--FORWARD AND REVERSE KEY
                call "DATUNFMT" (checkdate$)
                call "GLUNFMT" (discacct$)
                call "GLUNFMT" (cashacct$)
                if key$<>" " then L31220
L31190:         call "FMTKEYCK" (#9,key$,reversekey$)
                str(checknr$,1,4)="X000"
                str(checknr$,5,4)=str(key$,4,4)

L31220:     REM UNFORMAT AND WRITE HEADER INFORMATION TO THE FILE.
                write  #9, using L31580,                                  ~
                           key$, reversekey$, vencode$, checknr$,        ~
                           checkdate$, disctotal, discacct$,             ~
                           cashacct$, checktotal,temp$, eod goto L31190

            REM WRITE NEW CHECK LINE ITEMS TO BUFFER IF ANY.
                if maxlines% = 0 then return
                for temp% = 1 to maxlines%
                    convert temp% to seq$, pic(###)
                    convert amount$(temp%) to amount
                    convert disc$  (temp%) to discount
                    call "GLUNFMT" (acct$(temp%))
                    write #10, using L31460,                              ~
                               vencode$, checknr$, seq$,                 ~
                               invoice$(temp%), acct$(temp%),            ~
                               type$(temp%), amount, discount,           ~
                               invdate$(temp%), ten99$(temp%), temp$
                    next temp%
                 return

L31460:                         FMT CH(9),         /* VENDOR CODE      */~
                                    CH(8),         /* CHECK NUMBER     */~
                                    CH(3),         /* SEQUENCE NUMBER  */~
                                    CH(16),        /* INVOICE NUMBER   */~
                                    CH(09),        /* ACCT NUMBER      */~
                                    CH(1),         /* TYPE CODE        */~
                                    PD(14,4),      /* DEBIT  AMOUNT    */~
                                    PD(14,4),      /* DISCOUNT AMOUNT  */~
                                    CH(6),         /* INVOICE DATE     */~
                                    CH(4),         /* 1099 CATEGORY    */~
                                    CH(28)         /* FILL IT OUT      */

L31580:                         FMT /* TO USE IN WRITING HEADER        */~
                                    2*CH(07),      /* KEY + REVERSE KEY*/~
                                    CH(9),         /* VENDOR CODE      */~
                                    CH(8),         /* CHECK NUMBER     */~
                                    CH(6),         /* CHECK DATE       */~
                                    PD(14,4),      /* DISCOUNT AMOUNT  */~
                                    2*CH(9),       /* DISCOUNT,CASH ACC*/~
                                    PD(14,4),      /* TOTAL CHECK AMT  */~
                                    CH(29)         /* FILL IT OUT      */

        REM *************************************************************~
            *     I N P U T   C H E C K   H E A D E R   S C R E E N     *~
            *                                                           *~
            * GETS CHECK HEADERS FROM THE SCREEN.  STANDARD INPUT STYLE *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(8c)) linefac$()
                  if lastven$ <> " " then header$ = "Last Vendor: " &    ~
                       lastven$ & "  " & "Last Control No: " & lastcheck$
                  if fieldnr% > 2% then header$ = " "
                  str(header$,62) = "CHKUDOIT: " & cms2v$
                  on fieldnr% gosub  L40230,        /* VENDOR CODE      */~
                                     L40230,        /* CHECK NUMBER     */~
                                     L40230,        /* CHECK DATE       */~
                                     L40260,        /* DISCOUNT TAKEN   */~
                                     L40230,        /* DISCOUNT ACCOUNT */~
                                     L40230         /* CASH IN BANK ACCT*/
                  goto L40300

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linefac$(fieldnr%) = hex(80)
                      return
L40230:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linefac$(fieldnr%) = hex(81)
                      return
L40260:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linefac$(fieldnr%) = hex(82)
                      return

L40300:     accept                                                       ~
               at (01,02), "Manage A/P Check Holding File",              ~
               at (01,39), "Post Date: XXXXXXXX  Todays Date:",          ~
               at (01,50), fac(hex(8c)),   postdate$            , ch(08),~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   header$              , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "Vendor code",                                         ~
               at (06,30), fac(linefac$(1)), vencode$           , ch(09),~
               at (06,48), fac(hex(8c)), venname$               , ch(32),~
               at (07,02),                                               ~
                  "Control number",                                      ~
               at (07,30), fac(linefac$(2)), checknr$           , ch(08),~
               at (08,02),                                               ~
                  "Date you wish on check",                              ~
               at (08,30), fac(linefac$(3)), checkdate$         , ch(08),~
               at (09,02),                                               ~
               at (09,30), fac(linefac$(4)), discount$          , ch(10),~
               at (10,02),                                               ~
                  "Discount G/L account",                                ~
               at (10,30), fac(linefac$(5)), discacct$          , ch(12),~
               at (10,48), fac(hex(8c)), discacctdescr$         , ch(32),~
               at (11,02),                                               ~
                  "Cash in bank G/L account",                            ~
               at (11,30), fac(linefac$(6)), cashacct$          , ch(12),~
               at (11,48), fac(hex(8c)), cashacctdescr$         , ch(32),~
               at (21,02), fac(hex(a4)), infomsg$               , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L40750
                call "MANUAL" ("CHKUDOIT")
                goto L40300

L40750:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *            E D I T   H E A D E R   S C R E E N            *~
            *                                                           *~
            * EDITS HEADER INFORMATION.  JUST LIKE LINE ITEMS, EXCEPT   *~
            * FOR THE INSTRUCTIONS AND PFKEY DEFINITIONS.               *~
            *************************************************************

            deffn'202(fieldnr%)
                  init(hex(8c)) linefac$()
                  init(hex(86)) str(linefac$(),3)
                  header$ = " "
                  str(header$,62) = "CHKUDOIT: " & cms2v$
                  on fieldnr% gosub  L41230,        /* VENDOR CODE      */~
                                     L41230,        /* CHECK NUMBER     */~
                                     L41230,        /* CHECK DATE       */~
                                     L41260,        /* DISCOUNT TAKEN   */~
                                     L41230,        /* DISCOUNT ACCOUNT */~
                                     L41230         /* CASH IN BANK ACCT*/
                  goto L41300

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      init (hex(8c)) linefac$()
                      linefac$(fieldnr%) = hex(80)
                      return
L41230:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      init (hex(8c)) linefac$()
                      linefac$(fieldnr%) = hex(81)
                      return
L41260:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      init (hex(8c)) linefac$()
                      linefac$(fieldnr%) = hex(82)
                      return

L41300:     accept                                                       ~
               at (01,02), "Manage A/P Check Holding File",              ~
               at (01,39), "Post Date: XXXXXXXX  Todays Date:",          ~
               at (01,50), fac(hex(8c)),   postdate$            , ch(08),~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   header$              , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "Vendor code",                                         ~
               at (06,30), fac(linefac$(1)), vencode$           , ch(09),~
               at (06,48), fac(hex(8c)), venname$               , ch(32),~
               at (07,02),                                               ~
                  "Control number",                                      ~
               at (07,30), fac(linefac$(2)), checknr$           , ch(08),~
               at (08,02),                                               ~
                  "Date you wish on check",                              ~
               at (08,30), fac(linefac$(3)), checkdate$         , ch(08),~
               at (10,02),                                               ~
                  "Discount G/L account",                                ~
               at (10,30), fac(linefac$(5)), discacct$          , ch(12),~
               at (10,48), fac(hex(8c)),     discacctdescr$     , ch(32),~
               at (11,02),                                               ~
                  "Cash in bank G/L account",                            ~
               at (11,30), fac(linefac$(6)), cashacct$          , ch(12),~
               at (11,48), fac(hex(8c)),     cashacctdescr$     , ch(32),~
                                                                         ~
               at (13,15), "Gross Check:",                               ~
               at (13,28), fac(hex(8c)),     aa$                , ch(10),~
               at (14,15), "Discount   :",                               ~
               at (14,28), fac(hex(8c)),     dd$                , ch(10),~
               at (15,15), "Net Check  :",                               ~
               at (15,28), fac(hex(84)),     nn$                , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),     edtmessage$        , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,25),                                               ~
                  "(12)Delete Check",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(2)Line Items",                                       ~
               at (24,65),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(0001020c0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41850
                  call "MANUAL" ("CHKUDOIT")
                  goto L41300

L41850:        if keyhit% <> 15 then L41890
                  call "PRNTSCRN"
                  return

L41890:        REM GET CURSOR POSITION ON SCREEN FOR EDIT AND OTHER FUN.
                   close ws
                   call "SCREEN" addr("C", 0%,"I",i$(),cursor%())
                   return

        REM *************************************************************~
            *       T A B L E   O P E R A T I O N S   S C R E E N       *~
            *                                                           *~
            * THIS IS THE SCREEN ROUTINE THAT HANDLES INPUT, EDIT,      *~
            * INSERT, AND DELETE MODES.                                 *~
            *************************************************************

            deffn'203(screenline%, fieldnr%)
                  screen% = 1
                  goto L42290

            deffn'213(screenline%, fieldnr%)
                  screen% = 2
                  init(hex(86)) fac$()
                  if fieldnr% = 0 then L42300
                  goto L42290

            deffn'223(screenline%, fieldnr%)
                  screen% = 3
                  goto L42290

            deffn'233(screenline%)
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 6
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L42470

L42290:           init(hex(84)) fac$()
L42300:           on fieldnr% gosub L42400,         /* INVOICE NUMBER   */~
                                    L42400,         /* ACCOUNT NUMBER   */~
                                    L42430,         /* AMOUNT           */~
                                    L42430,         /* DISCOUNT         */~
                                    L42400          /* 1099 CATEGORY    */
                  goto L42470

            REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                fac$(screenline%, fieldnr%) = hex(80)
                return
L42400:     REM SET FAC'S FOR UPPER CASE ONLY INPUT
                fac$(screenline%, fieldnr%) = hex(81)
                return
L42430:     REM SET FAC'S FOR NUMERIC INPUT.
                fac$(screenline%, fieldnr%) = hex(82)
                return

L42470:     accept                                                       ~
               at (01,02), fac(hex(8c)),      ttl$(screen%, 1)  , ch(62),~
               at (02,02), fac(hex(8c)),      ttl$(screen%, 2)  , ch(62),~
               at (01,65), "! VEN:"                             ,        ~
               at (01,72), fac(hex(8c)),    vencode$            , ch(09),~
               at (02,65), "! CTL:"                             ,        ~
               at (02,72), fac(hex(8c)),    checknr$            , ch(08),~
               at (03,65), "+-------------"                     ,        ~
               at (03,02), fac(hex(94)),    errormsg$           , ch(62),~
               at (04,02), fac(hex(84)),    infomsg$            , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)),    separator$(1)       , ch(79),~
               at (09,02), fac(hex(84)),    separator$(2)       , ch(79),~
               at (13,02), fac(hex(84)),    separator$(3)       , ch(79),~
               at (17,02), fac(hex(84)),    separator$(4)       , ch(79),~
               at (21,02), fac(hex(84)),    separator$(5)       , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84)),    seq$     (line%+ 1) , ch(03),~
               at (10,02), fac(hex(84)),    seq$     (line%+ 2) , ch(03),~
               at (14,02), fac(hex(84)),    seq$     (line%+ 3) , ch(03),~
               at (18,02), fac(hex(84)),    seq$     (line%+ 4) , ch(03),~
               at (22,02), fac(hex(84)),    seq$     (line%+ 5) , ch(03),~
                                                                         ~
               at (06,02), "Invoice"                                    ,~
               at (10,02), "Invoice"                                    ,~
               at (14,02), "Invoice"                                    ,~
               at (18,02), "Invoice"                                    ,~
               at (22,02), "Invoice"                                    ,~
                                                                         ~
               at (06,10), fac(fac$(1, 1)), invoice$ (line%+ 1) , ch(16),~
               at (10,10), fac(fac$(2, 1)), invoice$ (line%+ 2) , ch(16),~
               at (14,10), fac(fac$(3, 1)), invoice$ (line%+ 3) , ch(16),~
               at (18,10), fac(fac$(4, 1)), invoice$ (line%+ 4) , ch(16),~
               at (22,10), fac(fac$(5, 1)), invoice$ (line%+ 5) , ch(16),~
                                                                         ~
               at (06,27), "Acc"                                        ,~
               at (10,27), "Acc"                                        ,~
               at (14,27), "Acc"                                        ,~
               at (18,27), "Acc"                                        ,~
               at (22,27), "Acc"                                        ,~
                                                                         ~
               at (06,31), fac(fac$(1, 2)), acct$    (line%+ 1) , ch(12),~
               at (10,31), fac(fac$(2, 2)), acct$    (line%+ 2) , ch(12),~
               at (14,31), fac(fac$(3, 2)), acct$    (line%+ 3) , ch(12),~
               at (18,31), fac(fac$(4, 2)), acct$    (line%+ 4) , ch(12),~
               at (22,31), fac(fac$(5, 2)), acct$    (line%+ 5) , ch(12),~
                                                                         ~
               at (06,45), fac(hex(84)),    descr$   (line%+ 1) , ch(30),~
               at (10,45), fac(hex(84)),    descr$   (line%+ 2) , ch(30),~
               at (14,45), fac(hex(84)),    descr$   (line%+ 3) , ch(30),~
               at (18,45), fac(hex(84)),    descr$   (line%+ 4) , ch(30),~
               at (22,45), fac(hex(84)),    descr$   (line%+ 5) , ch(30),~
                                                                         ~
               at (07,02), "Amount"                                     ,~
               at (11,02), "Amount"                                     ,~
               at (15,02), "Amount"                                     ,~
               at (19,02), "Amount"                                     ,~
               at (23,02), "Amount"                                     ,~
                                                                         ~
               at (07,12), fac(fac$(1, 3)), amount$  (line%+ 1) , ch(10),~
               at (11,12), fac(fac$(2, 3)), amount$  (line%+ 2) , ch(10),~
               at (15,12), fac(fac$(3, 3)), amount$  (line%+ 3) , ch(10),~
               at (19,12), fac(fac$(4, 3)), amount$  (line%+ 4) , ch(10),~
               at (23,12), fac(fac$(5, 3)), amount$  (line%+ 5) , ch(10),~
                                                                         ~
               at (07,30), "Discount"                                   ,~
               at (11,30), "Discount"                                   ,~
               at (15,30), "Discount"                                   ,~
               at (19,30), "Discount"                                   ,~
               at (23,30), "Discount"                                   ,~
                                                                         ~
               at (07,40), fac(fac$(1, 4)), disc$    (line%+ 1) , ch(10),~
               at (11,40), fac(fac$(2, 4)), disc$    (line%+ 2) , ch(10),~
               at (15,40), fac(fac$(3, 4)), disc$    (line%+ 3) , ch(10),~
               at (19,40), fac(fac$(4, 4)), disc$    (line%+ 4) , ch(10),~
               at (23,40), fac(fac$(5, 4)), disc$    (line%+ 5) , ch(10),~
                                                                         ~
               at (07,55), "1099 Category"                              ,~
               at (11,55), "1099 Category"                              ,~
               at (15,55), "1099 Category"                              ,~
               at (19,55), "1099 Category"                              ,~
               at (23,55), "1099 Category"                              ,~
                                                                         ~
               at (07,70), fac(fac$(1, 5)), ten99$   (line%+ 1) , ch( 4),~
               at (11,70), fac(fac$(2, 5)), ten99$   (line%+ 2) , ch( 4),~
               at (15,70), fac(fac$(3, 5)), ten99$   (line%+ 3) , ch( 4),~
               at (19,70), fac(fac$(4, 5)), ten99$   (line%+ 4) , ch( 4),~
               at (23,70), fac(fac$(5, 5)), ten99$   (line%+ 5) , ch( 4),~
                                                                         ~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13 then L43440
                  call "MANUAL" ("CHKUDOIT")
                  goto L42470

L43440:        if keyhit% <> 15 then L43480
                  call "PRNTSCRN"
                  goto L42470

L43480:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *              T E S T   H E A D E R   D A T A              *~
            * MAKES SURE THE VENDOR IS ON FILE, THAT THE CHECK IS NOT   *~
            * (LOADS AND DROPS INTO EDIT MODE IF IT IS)                 *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* VENDOR           */~
                                    L50210,         /* CHECK NUMBER     */~
                                    L50330,         /* CHECK DATE       */~
                                    L50380,         /* DISCOUNT AMOUNT  */~
                                    L50420,         /* DISCOUNT ACCOUNT */~
                                    L50500          /* CASH IN BANK ACCT*/
                  return

L50160:     REM TEST TO SEE THAT VENDOR IS ON FILE
                call "GETCODE" (#3, vencode$, venname$, 0%, 1.3, f1%(3))
                if f1%(3) = 1 then return
                   errormsg$ = "Vendor Not On File:" & vencode$
                   return
L50210:     REM TEST FOR CHECK ON FILE, LOAD UP IF YES. GENERATES, TOO.
                if editmode% = 1 then return       /* NO TEST IF EDIT  */
                if checknr$ = " " then return      /* AUTO GENERATE    */
                gosub L30000              /* SEARCH FOR CHECK ON FILE.  */
                if oldcheckonfile%<>0 then L50290
                if errormsg$<>" " then return
                errormsg$="Check Not On File: " & checknr$
                return
L50290:            return clear
                   return clear
                   goto editmode

L50330:     REM TEST FOR DATE OF CHECK OK.
                if checkdate$ = " " or checkdate$ = blankdate$ ~
                                  then checkdate$ = postdate$
                call "DATEOK" (checkdate$, temp%, errormsg$)
                return

L50380:     REM TEST FOR DISCOUNT TAKEN OK, SET ZERO IF BLANK.
                call "NUMTEST"(discount$,-9e7,9e7,errormsg$,2.4,discount)
                     return

L50420:     REM TEST FOR DISCOUNT ACCOUNT, GET DESCRIPTION IF YES
                discacctdescr$ = hex(0684) & "Select Discount Account"
                call"GETCODE"(#2, discacct$, discacctdescr$,1%,0,f1%(2))
                     if f1%(2) = 1 then return
                        errormsg$ = "Discount Account Not On File:"      ~
                                             & discacct$
                        return

L50500:     REM TEST FOR CASH IN BANK ACCOUNT, DESCRIBE IF HERE.
              cashacctdescr$ = hex(0684) & "Select 'Cash In Bank' Account"
                call"GETCODE" (#2, cashacct$, cashacctdescr$,1%,0,f1%(2))
                     if f1%(2) = 1 then return
                        errormsg$ = "Cash In Bank Account Not On File:"  ~
                                             & cashacct$
                        return

        REM *************************************************************~
            *   T E S T   D A T A   F O R   C H E C K   D E T A I L S   *~
            *                                                           *~
            * MAKES SURE ALL THE NECESSARY INFORMATION IS ON FILE       *~
            * FOR THE INVOICE HEADERS.  NOTE THAT THE DEBIT  ACCOUNT    *~
            * WILL BE SKIPPED IF THE INVOICE IS ON FILE.  SHOULD IT BE  *~
            * NECESSARY TO MODIFY IT FROM THE INVOICE'S PAYABLES ACCT   *~
            * YOU CAN PICK IT UP IN EDIT MODE.                          *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub L51200,         /* INVOICE #        */~
                                    L51690,         /* DEBIT  ACCOUNT   */~
                                    L51790,         /* AMOUNT OF THING  */~
                                    L52130,         /* DISCOUNT AMOUNT  */~
                                    L52170          /* 1099 CATEGORY    */
                  return
            REM TEST FOR INVOICE ON FILE.
L51200:         infomsg$ = " "
                if amount$(c%) <> " " then return  /* DONT TEST EDIT   */
                if invoice$(c%) = " " then L51640  /* DONT TEST NULL INV*/
          /* VERIFY NO PAYMENT TO INVOICE THIS EXECUTION ALREADY   */
                if maxlines% = 0 then goto L51310
                     for temp% = 1 to maxlines% + 1
                     if temp% = c% then goto L51300
                     if invoice$(temp%) <> invoice$(c%) then L51300
                     convert temp% to numchar$, pic(###)  : errormsg$ =  ~
                "Invoice Payment This Run With Entry" & numchar$ : return
L51300:                 next temp%
L51310:         gosub L53000
                if payok% = 0 then L51430   /*NO CHECKS OR BUFCHECKTOT=0*/
                   REM INVOICE BEING PAID BY CHKBUF2 (AND/OR) CSHBUF2
                   infomsg$="WARNING:Inv Now"
                   if numchk% = 0 then L51370
                   str(infomsg$,len(infomsg$)+2)="CHKBUF=" & chkchk$
L51370:            on payok% goto L51430,L51390,L51380   /*CHK,CSH, BOTH */
L51380:              str(infomsg$,len(infomsg$)+2)="&"
L51390:            convert numcsh% to numchar$, pic(###)
                   str(infomsg$,len(infomsg$)+2) = numchar$ &            ~
                            " CSHBUF,LAST=" & cshchk$

L51430:         outstandingamount = 0
                diskkey$ = vencode$
                str(diskkey$,10) = invoice$(c%)
                call "READ100" (#5, diskkey$, f1%(5))
                     if f1%(5) = 0 then L51640
                     get #5, using L51500, invdate$(currentline%),        ~
                                          outstandingamount, hold$
L51500:              FMT XX(41), CH(6), XX(71), PD(14,4), XX(40), CH(1)
                     if hold$ <> "Y" then L51540
                     errormsg$ = "Invoice On Hold, Can't Be Paid."
                     return
L51540:         currkey$ = diskkey$
                call "PLOWNEXT" (#41, currkey$, 25%, f1%(41))
                   if f1%(41) = 0%  then L51550
                errormsg$ = "Invoice is not for Statutory Currency."
                return

L51550:         REM TO SUBTRACT ALL PAYMENTS FROM CHKBUF2(AND/OR)CSHBUF2
                outstandingamount = outstandingamount - bufchecktotal
                if payok% = 0 then L51590
                str(infomsg$,len(infomsg$)+2) = "DUE" : goto L51600
L51590:         infomsg$="Invoice Has A Current Balance Due Of"
L51600:         call "CONVERT" (outstandingamount, -2.2,                 ~
                                        str(infomsg$,len(infomsg$)+2,10))
                return

L51640:         REM IF INVOICE NOT ON FILE, SUPPLY PURCH ACCT THIS VENDOR
                    infomsg$="Invoice Not On File (Direct Sale Assumed)"
                    invdate$(currentline%) = date
                    return

L51690:     REM CHECK THE DEBIT ACCOUNT
                descr$(c%) = hex(0684) & "SELECT DISTRIBUTION ACCOUNT"
                call "GETCODE"(#2, acct$(c%), descr$(c%),0%,0, f1%(2))
                if f1%(2) = 0 then L51760
                   get #2, using L51740, type$(c%)
L51740:                        FMT XX(39), CH(1)
                   return
L51760:         errormsg$="Account not on file: " & acct$(c%)
                return

L51790:     REM CHECK OUT THE AMOUNT.  NOTE NEGATIVES ALLOWED.
                if outstandingamount = 0 then L51840
                REM ASSUME OUTSTANDING AMOUNT IF BLANK AMOUNT ENTERED.
                   if amount$(c%) <> " " then L51840
                    call "CONVERT" (outstandingamount, -2.2, amount$(c%))
L51840:      REM CHECK THE AMOUNT PAYING IS LESS THAN OUTSTANDINGAMOUNT
                if amount$(c%) = " " then amount$(c%) = "0"
                convert amount$(c%) to amount, data goto L51990
                if f1%(5) = 0% then L51960
                   temp = outstandingamount - amount  /* NEW BALANCE */
                if outstandingamount >= 0 then L51930
                   if temp > 0 then L52020
                   if amount >= outstandingamount then L51960

L51930:            if temp < 0 then L52020
                   if amount > outstandingamount then L52020

L51960:         call "CONVERT" (amount, -2.2, amount$(c%))
                return

L51990:         errormsg$ = "Illegal entry for amount: " & amount$(c%)
                return

L52020:      REM WE KNOW THE AMOUNT ON THIS CHECK IS TOO MUCH            ~
                    FOR THIS EXISTING INVOICE NUMBER
                errormsg$="THIS CHECK"
                call "CONVERT" (amount, -2.2,                            ~
                               str(errormsg$,len(errormsg$)+2,10))
                str(errormsg$,len(errormsg$)+2) =                        ~
                               "CURRENT INVOICE BALANCE"
                call "CONVERT" (outstandingamount, -2.2,                 ~
                               str(errormsg$,len(errormsg$)+2,10))
                        return

L52130:     REM DISCOUNT AMOUNT TEST.
                call "NUMTEST" (disc$(c%), 0, 9e7, errormsg$, 2.4, temp)
                      return

L52170:     REM TEST DATA FOR 1099 CATEGORY VALIDATION
                if ten99$(c%) = " " then return
                readkey$ = "1099 CATS" & ten99$(c%)
                work$ = hex(06) & "Select 1099 Category"
                call "PLOWCODE" (#13, readkey$, work$, 9%, 0.30, f1%(13))
                     if f1%(13) = 1% then L52250
                     errormsg$ = "Invalid 1099 Category Code"
                     return
L52250:         ten99$(c%) = str(readkey$,10)
                return

L53000: REM *************************************************************~
            * TEST OK TO PAY THIS INVOICE. IT IS NOT OK IF THERE IS ANY *~
            * CHECK PENDING (IE IN CHKBUF2 OR CSHBUF2) ADDRESSING THIS  *~
            * INVOICE                                                   *~
            *************************************************************

            payok%, numchk%, numcsh%, bufchecktotal = 0
            chkchk$, cshchk$ = " "
            findkey$=invoice$(c%)
            call "REDALT0" (#10,findkey$,1%,f1%(10))
            goto L53120
L53110:          call "READNEXT" (#10,f1%(10))
L53120:     if f1%(10)=0 then L53230
            get #10, using L53140, tven$,tcheck$,tinv$,tdebit,tdiscount
L53140:              FMT CH(09),CH(08),XX(03),CH(16),XX(10),2*PD(14,4)
            if tinv$<>invoice$(c%) then L53230
            if tven$<>vencode$ then L53110
               REM FOUND SAME INVOICE,VENDOR ADD AMOUNT TO BUFFER SUM
               bufchecktotal = bufchecktotal + tdebit + tdiscount
                  REM UPDATE FLAGS AND CHECK NUMBER
                  numchk% = numchk% + 1  :  chkchk$ = tcheck$
            goto L53110

L53230:     call "REDALT0" (#11,findkey$,1%,f1%(11))
            goto L53260
L53250:          call "READNEXT" (#11,f1%(11))
L53260:     if f1%(11)=0 then L53370
            get #11, using L53280, tven$,tcheck$,tinv$,tdebit,tdiscount
L53280:              FMT CH(09),CH(08),XX(03),CH(16),XX(10),2*PD(14,4)
            if tinv$<>invoice$(c%) then L53370
            if tven$<>vencode$ then L53250
               REM FOUND SAME INVOICE,VENDOR ADD AMOUNT TO BUFFER SUM
               bufchecktotal = bufchecktotal + tdebit + tdiscount
                  REM UPDATE FLAGS AND CHECK NUMBER
                  numcsh% = numcsh% + 1  :  cshchk$ = tcheck$
            goto L53250

L53370:     if numchk% > 0 then payok% = 1
            if numcsh% > 0 then payok% = payok% + 2

            return

L60000: REM *************************************************************~
            *   C O M P U T E   P R O P E R   D I S C O U N T   A M T   *~
            *                                                           *~
            * COMPUTES PROPER DISCOUNT AMOUNT FOR AN INVOICE ON FILE SO *~
            * WE CAN OK IT.                                             *~
            *************************************************************

            if f1%(5) = 0 then return
               get #5, using L60100, discdate$, totalamnt, balance,       ~
                                                                 discamnt
L60100:        FMT XX(72), CH(6), XX(32), 2*PD(14,4), XX(28), PD(14,4)
             chkdte$ = checkdate$
             call "DATUNFMT"(chkdte$)

             convert amount$(currentline%) to amount

              if discdate$ < d19020101$ then L60230
              if discdate$ < chkdte$            then L60230
              if balance <= 0                   then L60230
              if abs(balance - totalamnt) > .01 then L60230
              if abs(amount  - totalamnt) > .01 then L60230
              if payok% <> 0                    then L60230
              goto L60250
L60230:           disc$(c%) = "0.00"
                  return
L60250:       discount = discamnt
              call "DATEFMT"(discdate$)
              call "CONVERT" (discount, -2.2, disc$(currentline%))
           infomsg$ = "A Discount Of " & disc$(currentline%) & " Is Avail~
        ~able If Paid By " & discdate$
                  return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
