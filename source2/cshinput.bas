        REM *************************************************************~
            *                                                           *~
            *   CCC    SSS   H   H  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  C   C  S      H   H    I    NN  N  P   P  U   U    T     *~
            *  C       SSS   HHHHH    I    N N N  PPPP   U   U    T     *~
            *  C   C      S  H   H    I    N  NN  P      U   U    T     *~
            *   CCC    SSS   H   H  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CSHINPUT - INPUT CASH DISBURSEMENTS AND POST TO BUFFER.   *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+---------------WHAT-----------------------+-WHO-*~
            * 05/11/80 ! ORIGINAL (FROM CRCINPUT)                 ! BCW *~
            * 09/20/80 ! CLEAN UP; NEW TABULAR SCREEN LAYOUTS     ! BCW *~
            * 05/27/81 ! EXPANDED TO 16 CHAR INVOICE NUMBER       ! TOM *~
            * 07/23/81 ! ADD DISCOUNT TAKEN TO LINE ITEM          ! TEM *~
            * 08/28/82 ! ADDED TEST FOR RECONCILED CHECK          ! ECR *~
            * 10/01/84 ! ELIM 2 CHECKS TO SAME INVOICE THIS RUN   ! JWG *~
            * 12/02/85 ! NO DUPLICATE CHECK NUMBERS ALLOWED       ! KAB *~
            * 12/16/85 ! Vendor file format changes               ! MJB *~
            * 05/15/86 ! PAYMASTR FILE FORMAT CHANGE              ! HES *~
            * 04/26/89 ! Changes to honor "M-C on" flag           ! MLJ *~
            * 10/12/89 ! Minor clean up & fix branch in processing! JDH *~
            *          !  Statutory line items; was getting only  !     *~
            *          !  the first one. Also, mods for MC 'Off'. !     *~
            * 10/16/89 ! Corrected GET of Currency Master's check ! JDH *~
            *          !  amount and discount, was backwards.     !     *~
            *          !  Also, totaled MC paylines to get default!     *~
            *          !  check amount, was converting statutory  !     *~
            *          !  which led to rounding errors.           !     *~
            * 12/07/92 ! PRR 12392 - Correcetd access problems on ! MLJ *~
            *          !  multiple check lines. Added channels 40 !     *~
            *          !  thru 48 to prog select files Rem. Also, !     *~
            *          !  line tran currrency code now displayed  !     *~
            *          !  when tran same as stat currency.  Fixed !     *~
            *          !  numerous implied integers.              !     *~
            * 01/14/94 ! PRR 12990 - Changed rounding for multiple! JBK *~
            *          !  currency checks to (hopefully) avoid    !     *~
            *          !  problems downstream. M-C amounts now    !     *~
            *          !  rounded before being written to buffers.!     *~
            * 02/28/94 ! PRR 13098 - When determining the default ! JBK *~
            *          !  payment amount for M-C checks, the      !     *~
            *          !  amount of previous partial payments now !     *~
            *          !  considered.                             !     *~
            * 08/02/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            acct$(100)16,                /* ACCOUNT NUMBERS FOR LINES  */~
            amount$(100)10,              /* AMOUNT EACH LINE ITEM      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* BLANK LINE FOR INPUT SCREEN*/~
            cashacct$16,                 /* CASH IN BANK ACCOUNT       */~
            cashacctdescr$32,            /* CASH ACCOUNT DESCRIPTION   */~
            checkdate$8,                 /* CHECK DATE INFORMATION     */~
            checknr$8,                   /* CHECK NUMBER               */~
            cshchk$8,                    /* PRINT LAST CSHBUF2 CHECK   */~
            chkchk$8,                    /*      "     CHKBUF2 CHECK   */~
            currflag$(100)1,             /* Non-check Currency Warning */~
            currtype$1,                  /* CURRENCY TABLE             */~
            curr$1,                      /* CURRENCY Y/N               */~
            currdescr$32,                /* CURRENCY DESCR             */~
            currkey$50,                  /* CURRENCY PLOWKEY           */~
            currency$4,                  /* Check Currency             */~
            currency$(100)4,             /* Invoice Currency           */~
            convdate$6,                  /* Conversion Eff. Date       */~
            convdate$(100)6,             /* Conversion Eff. Date       */~
            conveqv(100),                /* Conversion Factor          */~
            convunt(100),                /* Conversion Factor          */~
            gain_loss$9,                 /* Currency Gain/Loss Account */~
            statutory$4,                 /* STATUTORY CURRENCY         */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            defcashacct$9,               /* CASH IN BANK ACCOUNT       */~
            defdiscacct$9,               /* DISCOUNT ACCOUNT NUMBER    */~
            defpuracct$9,                /* PURCHASE ACCOUNT DEFAULT   */~
            disco$(100)10,               /* DISCOUNT TAKEN PER LINE    */~
            vencode$9,                   /* VENDOR CODE                */~
            venname$32,                  /* VENDOR NAME                */~
            date$8,                      /* DATE FOR SCREEN HEADER     */~
            descr$(100)32,               /* DESCRIPTIONS OF ACCOUNTS   */~
            discacct$16,                 /* DISCOUNT ACCOUNT NUMBER    */~
            discacctdescr$32,            /* DISCOUNT ACCT DESCRIPTION  */~
            discount$10,                 /* DISCOUNT TAKEN AMOUNT      */~
            diskkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            edttran$80,                  /* MAP TO FIELD # FROM CURSOR */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20,5)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            header$100,                  /* CHECK HEADER INFO AS 1     */~
            header1$79,                  /* Screen Title               */~
            colhdr$79,                   /* COLUMN HEADERS FOR SCREEN  */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            invoice$(100)16,             /* INVOICE NUMBERS FOR LINES  */~
            invdate$(100)6,              /* DATE OF INV WHEN PAID      */~
            key$7,                       /* FORWARD BUFFER KEY         */~
            lastcheck$8,                 /* LAST CHECK NUMBER WRITTEN  */~
            lastven$9,                   /* LAST VENDOR INPUT INFO.    */~
            linefac$(20)1,               /* FIELD ATTRIBUTE CHARACTERS */~
            numchar$4,                   /* PRINT NUMBER OF CSH CHECKS */~
            pf$(3)79,                    /* PF KEY PROMPTS             */~
            pfkey$17,                    /* PFKEYS ENABLED             */~
            pfkeys$(4)17,                /* PFKEYS ENABLED IN TABULAR  */~
            postdate$8,                  /* POSTING DATE               */~
            prooftotal$10,               /* PROOF TOTAL FOR TESTING    */~
            purgedmsg$79,                /* PURGED MESSAGE             */~
            purgedfac$1,                 /* AND FAC FOR IT             */~
            readkey$50,                                                  ~
            work$50,                                                     ~
            reconciled$1,                /* CHECK RECONCILIATION FLAG  */~
            reversekey$7,                /* REVERSE BUFFER KEY         */~
            seqnr$3,                     /* SEQUENCE NUMBER FOR WRITES */~
            status$(100)1,               /* LINE STATUS                */~
            tcheck$8,                    /* Temp Check Number          */~
            ten99$(100)4,                /* 1099 CATEGORIES            */~
            tinv$16,                     /* Temporary Invoice Number   */~
            tseq$3,                      /* Temporary Check Seq Number */~
            tttle$(4,2)64,               /* TITLES FOR TABULAR SCREENS */~
            tven$9,                      /* Temporary Vendor Number    */~
            type$(100)1,                 /* ACCOUNT TYPES FOR LINES    */~
            userid$3                     /* USERID OF CURRENT USER     */~

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
            * # 7 ! CSHMASTR ! DISBURSEMENTS CHECK HEADER FILE          *~
            * # 8 ! CSHLINES ! DISBURSEMENTS CHECK DETAIL FILE          *~
            * # 9 ! CSHBUFFR ! DISBURSEMENTS BUFFER AREA                *~
            * #10 ! CSHBUF2  ! DISBURSEMENTS CHECK DETAIL BUFFER AREA   *~
            * #11 ! CHKBUF2  ! CHECK LINE ITEMS                         *~
            * #12 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * #13 ! GENCODES ! 1099 VALIDATION                          *~
            * #40 ! CURMASTR ! CURRENCY MASTER FILE                     *~
            * #41 ! CURCONVR ! CURRENCY CONVERSION TABLES               *~
            * #42 ! CSHMSCBF ! CURRENCY INFORMATION FOR CSHBUFFR        *~
            * #43 ! CSHLNCBF ! CURRENCY INFORMATION FOR CSHBUF2         *~
            * #45 ! CSHMSCUR ! CURRENCY INFORMATION FOR CSHMASTR        *~
            * #46 ! CSHLNCUR ! CURRENCY INFORMATION FOR CSHLINES        *~
            * #48 ! PAYLNCUR ! CURRENCY INFORMATION FOR PAYLINES        *~
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
                        keypos = 1, keylen = 9,                          ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #7, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
                        alt key 1, keypos = 41, keylen = 9, dup,         ~
                            key 2, keypos = 50, keylen = 6, dup,         ~
                            key 3, keypos = 10, keylen = 8, dup

            select  #8, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select  #9, "CSHBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 7,                          ~
                        alt key 1, keypos =  8, keylen =   7,            ~
                            key 2, keypos = 15, keylen =  17,            ~
                            key 3, keypos = 24, keylen =   8, dup

            select #10, "CSHBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select #11, "CHKBUF2",                                       ~
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

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #42, "CSHMSCBF",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 17,                      ~
                        alt key  1, keypos =  1, keylen =  21

            select #43, "CSHLNCBF",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 20,                      ~
                        alt key  1, keypos =  1, keylen =  24

            select #45, "CSHMSCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 17,                      ~
                        alt key  1, keypos =  1, keylen =  21

            select #46, "CSHLNCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 20,                      ~
                        alt key  1, keypos =  1, keylen =  24

            select #48, "PAYLNCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 28,                      ~
                        alt key  1, keypos =  1, keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, 0%,   0%, " ")
            call "OPENCHCK" (# 2, 0%, 0%,   0%, " ")
            call "OPENCHCK" (# 3, 0%, 0%,   0%, " ")
            call "OPENCHCK" (# 5, 0%, 0%,   0%, " ")
            call "OPENCHCK" (# 7, 0%, 0%,   0%, " ")
            call "OPENCHCK" (# 8, 0%, 0%,   0%, " ")
            call "OPENCHCK" (# 9, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 200%, " ")
            call "OPENCHCK" (#11, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#12, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#13, 0%, 0%,   0%, " ")

*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#12, "SWITCHS.CUR", f1%(12%))
            if f1%(12%) <> 0% then get #12 using L05190, curr$, statutory$,~
                currtype$
L05190:         FMT POS(21), CH(1), CH(4), POS(34), CH(1)
            if curr$ <> "Y" then statutory$ = " "
            if curr$ <> "Y" then goto L09000
               f1%(42%) = 100% : f1%(43%), f1%(44%) = 200%
                call "OPENCHCK" (#40, 0%, 0%,   0%, " ")
                call "OPENCHCK" (#41, 0%, 0%,   0%, " ")
                call "OPENCHCK" (#45, f1%(45%),  0%,   0%, " ")
                call "OPENCHCK" (#46, f1%(46%),  0%,   0%, " ")
                call "OPENCHCK" (#48, f1%(48%),  0%,   0%, " ")
            if f1%(45%) < 0% and f1%(46%) < 0% and f1%(47%) < 0% then L05260
               f1%(42%) = 100% : f1%(43%), f1%(44%) = 200%
L05260:         call "OPENCHCK" (#42, 0%, 0%, f1%(42%), " ")
                call "OPENCHCK" (#43, 0%, 0%, f1%(43%), " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SET DATES, TRANSLATION TABLES, ETC.                       *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            init(hex(00)) edttran$
            init(hex(01)) str(edttran$,  3%, 17%)
            init(hex(02)) str(edttran$, 30%, 13%)
            init(hex(03)) str(edttran$, 53%, 11%)
            init(hex(04)) str(edttran$, 64%, 11%)
            init(hex(05)) str(edttran$, 75%,  5%)

            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#1, userid$, f1%(1%))
                 if f1%(1%) <> 0% then L09230
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                 "You Are Not Listed As A Valid User In This Data Base", ~
                                         " ",  "Press (RETURN) To Exit.")
                goto L65000
L09230:     get #1, using L09240, postdate$
L09240:     FMT XX(9), CH(6)
            REM VALIDATE USERS POSTING DATE
                call "WHICHMON" (#12, postdate$, this%)
                  if this% <> 0% then L09330
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                      "Your Posting Date Is Outside The Posting Window", ~
                                          " ", "Press (RETURN) To Exit.")
                goto L65000

L09330:     call "DATEFMT" (postdate$)

            colhdr$     = "*  Invoice Number   T/Curr   G/L Account  Type~
        ~          Amount   Discount 1099"

            tttle$(1%,1%) = "(1)Start Over(2)Col 1(4)Line Above(13)Instr ~
        ~(16)EDIT MODE"
            tttle$(2%,1%) = "(1)Start Over(2)First(3)Last(4)Prev(5)Next(6~
        ~)Down(7)Up"
            tttle$(2%,2%) = "(9)Header(11)Ins(12)Del(13)Instr (15)Prt Scr~
        ~n (16)WRITE DATA"
            tttle$(3%,1%) = "(1)Start Over(2)Col 1(4)Line Above(13)Instr ~
        ~(16)EXIT INS."
            tttle$(4%,1%) = "Press RETURN to DELETE Flashing Line or (1) ~
        ~To EXIT Delete."

            pfkeys$(1%) = hex(000102040f10ffffffffffffffffffffff)
            pfkeys$(2%) = hex(0001020304050607090b0c0d0f10ffffff)
            pfkeys$(3%) = hex(000102040f10ffffffffffffffffffffff)
            pfkeys$(4%) = hex(00010fffffffffffffffffffffffffffff)

            call "READ100" (#12, "MODULE.DEFAULTS.AP", f1%(12%))
                if f1%(12%) = 0% then L10000
            get #12, using L09570, defpuracct$, defcashacct$, defdiscacct$
L09570:         FMT XX(20), XX(4), XX(4), XX(8), CH(9), XX(18), 2*CH(9)

L10000: REM *************************************************************~
            *                  I N P U T   H E A D E R                  *~
            *                                                           *~
            * INPUTS HEADER, LOADS OLD CHECKS IF ON FILE, ETC...        *~
            *************************************************************

        inputmode
            init(" ") vencode$, venname$, checknr$, errormsg$,           ~
                      checkdate$, discount$, discacct$, discacctdescr$,  ~
                      cashacct$, cashacctdescr$, invoice$(), acct$(),    ~
                      type$(), descr$(), amount$(), errormsg$, infomsg$, ~
                      disco$(),ten99$(),invdate$(),status$(),purgedmsg$, ~
                      blankline$, currency$, currency$(), convdate$,     ~
                      convdate$(), currflag$(), currdescr$

            mat conveqv = con : mat convunt = con : conveqv, convunt = 1
            purgedfac$ = hex(9c)
            netpurged, netdiscpurged = 0
            editmode% = 0%

            for fieldnr% = 1% to 7%
                gosub'160(fieldnr%)
                      if enabled% =  0% then L10200
L10160:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover2
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10160
L10200:         gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10160
                next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

            maxlines%, screenline%, currentline% , line% = 0%

L11080:     screenline%  = screenline%  + 1%
            currentline% = currentline% + 1%
            if currentline% > 100% then editmode
               if screenline% <= 19% then L11150
                  screenline% = 1%
                  line% = line% + 19%

L11150:     infomsg$ = " "
            for fieldnr% = 1% to 5%
                gosub'161(fieldnr%)
                      if enabled% =  0% then L11250
L11190:         gosub'203(fieldnr%)
                      if keyhit%  =  1% then gosub startover1
                      if keyhit% <>  2% then L11220
                         gosub columnone:goto L11150
L11220:               if keyhit%  =  4% then gosub lineabove
                      if keyhit%  = 16% and fieldnr% = 1% then L11320
                      if keyhit% <>  0% then       L11190
L11250:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L11190
                next fieldnr%

            maxlines% = maxlines% + 1%
            goto L11080

L11320:     gosub columnone

        REM *************************************************************~
            *       E D I T   H E A D E R   I N F O R M A T I O N       *~
            *                                                           *~
            * EDITS HEADER INFORMATION.                                 *~
            *************************************************************

        editmode
            gosub L19230                  /* RECALCULATE CORRECT DISC   */
            init(" ") infomsg$, purgedmsg$, blankline$
            blankline$ = "To Modify Displayed Values, Position Cursor to ~
        ~Field & Press (RETURN)."
            if netpurged=0 and netdiscpurged=0 then L12087
            purgedfac$=hex(84)
            put purgedmsg$, using L12075, netpurged, netdiscpurged
L12075: %A NET AMOUNT OF -#######.## WITH DISCOUNTS OF -#######.## HAS BE~
        ~EN PURGED

L12087:     editmode% = 1%     /* FLAG THAT WE'RE EDITING FOR DATA TEST*/
L12088:     errormsg$ = " "
L12090:     gosub'202(0%)                /* SHOW WITH NON-MODIFIABLE   */
                  if keyhit%  =  1% then gosub startover2
                  if keyhit%  =  2% then       editlines
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 12% then       datasave
                  if keyhit% <>  0% then       L12088
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 3% or fieldnr% > 7% then L12088

               errormsg$ = " "

            gosub'160(fieldnr%)
                  if enabled% =  0% then L12088
L12170:     gosub'202(fieldnr%)
                  if keyhit%  =  1% then gosub startover2
                  if keyhit% <>  0% then       L12170
            gosub'150(fieldnr%)
                  if errormsg$ <> " " then L12170
            goto editmode

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        editlines
            line%, currentline%, screenline% = 0%

L13075:     errormsg$ = " "
            infomsg$ = "Line Status - 'F' = Frozen, 'A' = Available, ' ' ~
        ~= Added This Session"

L13090:     gosub'213(0%)
                  if keyhit%  =  0% then       L13260
                  if keyhit%  =  1% then gosub startover1
                  if keyhit%  = 16% then       datasave
                  if keyhit%  =  2% then line% = 0%
                  if keyhit%  =  3% then line% = max(0%, maxlines% - 15%)
                  if keyhit%  =  4% then line% = max(0%, line% - 15%)
                  if keyhit%  =  5% then line% = min(line% + 15%,        ~
                                                 max(0%, maxlines% - 15%))
                  if keyhit%  =  6% then line% = max(0%, line% - 1%)
                  if keyhit%  =  7% then line% = min(line% + 1%,         ~
                                                 max(0%, maxlines% - 15%))
                  if keyhit%  =  9% then goto  editmode
                  if keyhit%  = 11% then gosub insertmode
                  if keyhit%  = 12% then gosub deletemode
                  goto L13075

L13260:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                screenline% = max(0%, cursor%(1%) - 5%)
                if screenline%  =  0% then L13075
                c%, currentline% = screenline% + line%
                if currentline% > maxlines% then L13075
                if status$(currentline%) <> "F" then L13310
                   errormsg$ = "Line Frozen, Edit Not Allowed"
                   goto L13090
L13310:         fieldnr% = val(str(edttran$,cursor%(2%)))
                if fieldnr% = 0% or fieldnr% = 1% then L13075
                infomsg$, errormsg$ = " "

                if fieldnr% <> 3% then L13365
                   convert amount$(c%) to temp, data goto L13410
                   call "CONVERT" (temp, -2.2, amount$(c%))

L13365:         if fieldnr% <> 4% then L13410
                   convert disco$(c%) to temp, data goto L13410
                   call "CONVERT" (temp, -2.2, disco$(c%))

L13410:         gosub'213(fieldnr%)      /* NOW GET FIELD TO MODIFY    */
                      if keyhit%  =  1% then gosub startover1
                      if keyhit% <>  0% then L13410
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L13410
                goto L13075

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *                                                           *~
            * COLUMN ONE KEY AND LINE ABOVE KEY FUNCTIONS HANDLED HERE. *~
            *************************************************************

        columnone
            c% = currentline%
            init(" ") invoice$(c%), acct$(c%), descr$(c%), amount$(c%),  ~
                      type$(c%), disco$(c%), ten99$(c%), invdate$(c%),   ~
                      status$(c%), infomsg$, errormsg$, currency$(c%),   ~
                      convdate$(c%), currflag$(c%)
            conveqv(c%), convunt(c%) = 1

            return


        lineabove
            if currentline% = 1% then return
            c% = currentline%
            on fieldnr% goto  L14200,     /* INVOICE NUMBER             */~
                              L14210,     /* DEBIT  ACCOUNT NUMBER      */~
                              L14220,     /* AMOUNT OF CHECK.           */~
                              L14230,     /* DISCOUNT TAKEN             */~
                              L14240      /* 1099 CATEGORY              */
            return
L14200:                       invoice$ (c%) = invoice$ (c%-1%)
                              currency$(c%) = currency$(c%-1%)
                              currflag$(c%) = currflag$(c%-1%): return
L14210:                       acct$    (c%) = acct$    (c%-1%)
                              descr$   (c%) = descr$   (c%-1%)
                              type$    (c%) = type$    (c%-1%): return
L14220:                       amount$  (c%) = amount$  (c%-1%): return
L14230:                       disco$   (c%) = disco$   (c%-1%): return
L14240:                       ten99$   (c%) = ten99$   (c%-1%): return

L15000: REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *                                                           *~
            * HANDLES INSERTION OF A LINE ITEM INTO THE INVOICE.        *~
            *************************************************************

        insertmode
            if maxlines% < 100% then L15080         /* ARRAY FULL, CAN'T*/
               errormsg$ = "Maximum Number of Lines Entered"
               return clear
               goto L13090

L15080:     REM OTHERWISE, SET CURRENTLINE%, SCREENLINE%, AND COPY RIGHT
                screenline% = max(0%, cursor%(1%) - 5%)
                if line% + screenline% < maxlines% then L15120
                   screenline% = maxlines% - line% /* TO INS AT END    */
L15120:         if screenline% <> 19% then L15150   /* BOTTOM OF PAGE   */
                   line% = line% + 1%
                   screenline% = screenline% - 1%
L15150:         currentline%, c% = screenline% + line%

            REM COPY ALL THE ELEMENTS UP ONE
                if c% >= maxlines% then L15270
                for temp% = maxlines% to c% step -1%
                    invoice$ (temp%+1%) = invoice$ (temp%)
                    invdate$ (temp%+1%) = invdate$ (temp%)
                    currency$(temp%+1%) = currency$(temp%)
                    currflag$(temp%+1%) = currflag$(temp%)
                    convdate$(temp%+1%) = convdate$(temp%)
                    conveqv  (temp%+1%) = conveqv  (temp%)
                    convunt  (temp%+1%) = convunt  (temp%)
                    acct$    (temp%+1%) = acct$    (temp%)
                    descr$   (temp%+1%) = descr$   (temp%)
                    type$    (temp%+1%) = type$    (temp%)
                    amount$  (temp%+1%) = amount$  (temp%)
                    disco$   (temp%+1%) = disco$   (temp%)
                    ten99$   (temp%+1%) = ten99$   (temp%)
                    status$  (temp%+1%) = status$  (temp%)
                    next temp%

L15270:         screenline% = screenline% + 1%
                c%, currentline% = currentline% + 1%

                gosub columnone

            REM NOW INPUT THE LINE, ENABLE CANCEL OUT OPTION
L15340:         errormsg$, infomsg$ = " "
                for fieldnr% = 1% to 5%
                    gosub'161(fieldnr%)
                          if enabled% =  0% then L15430
L15380:             gosub'223(fieldnr%)
                          if keyhit%  =  1% then gosub startover1
                          if keyhit% <>  2% then L15388
                             gosub columnone:goto L15340
L15388:                   if keyhit%  =  4% then gosub lineabove
                          if keyhit%  = 16% and fieldnr% = 1% then L15490
                          if keyhit% <>  0% then L15380
                    gosub'151(fieldnr%)
                          if errormsg$ <> " " then L15380
L15430:             next fieldnr%

                maxlines%  = maxlines% + 1%
                cursor%(1%) = min(cursor%(1%) + 1%, 24%)
                goto L15000
L15480:
L15490:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                c% = currentline%
                if currentline% <= maxlines% then gosub L15620

            if currentline% >= maxlines% and screenline% = 19%           ~
               then line% = max(0%, line%- 1%)

            currentline% = maxlines% + 1%
            gosub columnone
            return

L15620:     for temp% = currentline% to maxlines%
                invoice$ (temp%) = invoice$ (temp%+1%)
                invdate$ (temp%) = invdate$ (temp%+1%)
                currency$(temp%) = currency$(temp%+1%)
                currflag$(temp%) = currflag$(temp%+1%)
                convdate$(temp%) = convdate$(temp%+1%)
                conveqv  (temp%) = conveqv  (temp%+1%)
                convunt  (temp%) = convunt  (temp%+1%)
                acct$    (temp%) = acct$    (temp%+1%)
                descr$   (temp%) = descr$   (temp%+1%)
                type$    (temp%) = type$    (temp%+1%)
                amount$  (temp%) = amount$  (temp%+1%)
                disco$   (temp%) = disco$   (temp%+1%)
                ten99$   (temp%) = ten99$   (temp%+1%)
                status$  (temp%) = status$  (temp%+1%)
                next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *                                                           *~
            * DELETES A LINE ITEM FROM THE CHECK.                       *~
            *************************************************************

        deletemode
            if maxlines% = 0% then return
            screenline% = cursor%(1%) - 5%
            if screenline% < 1% then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return
               if status$(currentline%) <> "F" then L16120
                  return clear
                  errormsg$ = "Line Frozen, Delete Not Allowed"
                  goto L13090
L16120:     infomsg$, errormsg$ = " "

L16130:     gosub'233(screenline%)
                  if keyhit%  =  1% then       return
                  if keyhit% <>  0% then       L16130

            c% = currentline%
            if currentline% < maxlines% then gosub L15480
                                         /* ACTUALLY DELETE LINE @C%   */
            temp% = maxlines%
            init(" ") invoice$(temp%), acct$(temp%), type$(temp%),       ~
                      descr$(temp%), amount$(temp%), errormsg$, infomsg$,~
                      disco$(temp%), ten99$(temp%), invdate$(temp%),     ~
                      status$(temp%), currency$(temp%), currflag$(temp%),~
                      convdate$(temp%)
            conveqv(temp%), convunt(temp%) = 1
            maxlines% = maxlines% - 1%
            return

        REM *************************************************************~
            * G E T  P R O O F   T O T A L   A N D   W R I T E  D A T A *~
            *                                                           *~
            * MAKES SURE NET AMOUNT OF CHECK BALANCES WITH WHAT THE USER*~
            * ENTERED.  IF IT BALANCES OK, THEN TOSS THE CHECK INTO THE *~
            * CHECK BUFFER. OTHERWISE, PRINT ERROR MESSAGE, AND GIVE    *~
            * OPERATOR A CHOICE TO REENTER TOTAL OR GO BACK TO EDIT.    *~
            *************************************************************

        datasave
            if keyhit% <> 12% then L19100
               if pos(str(status$()) = "F") = 0% then L19100
                  errormsg$ = "Check Has Frozen Lines, Delete ALL Not All~
        ~owed."
                  goto L12090

L19100:     REM FIRST GET WHAT USER THINKS TOTAL AMOUNT OF CHECK IS.
                errormsg$, prooftotal$ = " "
                oldmaxlines% = maxlines%
                if keyhit% = 12% then maxlines% = 0%

L19130:         gosub'209
                      if keyhit% <> 1% then L19150
                         maxlines% = oldmaxlines%
                         goto editmode
L19150:               if keyhit% <> 0% then L19130
                errormsg$ = " "
                convert prooftotal$ to prooftotal, data goto L19190
                   goto L19310
L19190:                 errormsg$ = "Invalid Entry For Proof Total :"    ~
                               & prooftotal$
                        goto L19130

L19230:     REM THEN COMPUTE WHAT WE THINK IT IS.
                checktotal, discount = 0
                if maxlines% = 0% then L19282
                for temp% = 1% to maxlines%
                    convert amount$(temp%) to amount
                    convert disco$(temp%) to disc
                    checktotal = checktotal + amount - disc
                    checktotal = round(checktotal, 2%)
                    discount = discount + disc
                    discount = round(discount, 2%)
                    next temp%
                REM THIS WOULD BE A GOOD PLACE TO ACT ON PURGED AMOUNTS
L19282:         discount=discount+netdiscpurged
                checktotal=checktotal+netpurged
                checktotal = round(checktotal, 2%)
                discount = round(discount, 2%)
                call "CONVERT" (discount, -2.2, discount$)
                return

L19310:     REM NOW COMPARE THE TWO.  GENERATE ERROR IF NOT EQUAL
            REM COULD ACT ON PURGED AMOUNTS HERE IF CUSTOMER DESIRES
                gosub L19230
        REM     CONVERT PROOFTOTAL$ TO PROOFTOTAL
                prooftotal=prooftotal + netpurged
                prooftotal = round(prooftotal,2%)
                if prooftotal = checktotal then L19370
                   errormsg$="PROOF TOTAL DOES NOT MATCH AMOUNT ENTERED!"
                   goto L19130

L19370:     REM NOW DELETE OLD INVOICE FROM BUFFER AND WRITE NEW ONE.
                str(diskkey$,  1%) = vencode$
                str(diskkey$, 10%) = checknr$
                call "REDALT1" (#9, diskkey$, 2%, f1%(9%))
                if f1%(9%) = 1% then delete #9
                call "DELETE" (#10, diskkey$, 17%)
                call "DELETE" (#42, diskkey$, 17%)
                call "DELETE" (#43, diskkey$, 17%)

                gosub L31000
                lastven$  = vencode$
                lastcheck$ = checknr$
                goto inputmode

        REM *************************************************************~
            * S E T   D E F A U L T S   F O R   L I N E A R   I N P U T *~
            *                                                           *~
            * SETS DEFAULTS FOR LINEAR INPUT INFORMATION.               *~
            *************************************************************

            deffn'160(fieldnr%)
                  enabled% = 0%
                  on fieldnr% gosub L20150,         /* VENDOR CODE      */~
                                    L20180,         /* CHECK NUMBER     */~
                                    L20210,         /* CHECK DATE       */~
                                    L20250,         /* DISCOUNT AMOUNT  */~
                                    L20270,         /* DISCOUNT ACCOUNT */~
                                    L20360,         /* CASH IN BANK ACCT*/~
                                    L20450          /* CURRENCY         */
                  return
L20150:     REM SET DEFAULTS FOR VENDOR CODE
                enabled% = 1%
                return
L20180:     REM SET DEFAULTS FOR CHECK NUMBER
                blankline$ = "Check Number Must Begin with 'M' if Non-num~
        ~eric"
                enabled% = 1%
                return
L20210:     REM SET DEFAULTS FOR CHECK DATE
                blankline$ = " "
                if currency$ = statutory$ then L20220
                if currency$ = " "        then L20220
                if maxlines% = 0%         then L20220
                   return
L20220:         enabled% = 1%
                if checkdate$ = " " or checkdate$ = blankdate$ ~
                                  then checkdate$ = postdate$
                return
L20250:     REM SET DEFAULTS FOR DISCOUNT AMOUNT
                return
L20270:     REM SET DEFAULTS FOR DISCOUNT ACCOUNT
                enabled% = 1%
                if discacct$ <> " " then return
                get #3, using L20310, discacct$
L20310:                 FMT XX(276), CH(9)
                if discacct$ = " " then discacct$ = defdiscacct$
                call "DESCRIBE"(#2, discacct$, discacctdescr$,0%, f1%(2%))
                call "GLFMT" (discacct$)
                return
L20360:     REM SET DEFAULTS FOR CASH IN BANK ACCOUNT
                enabled% = 1%
                if cashacct$ <> " " then return
                get #3, using L20400, cashacct$
L20400:                 FMT XX(267), CH(9)
                if cashacct$ = " " then cashacct$ = defcashacct$
                call "DESCRIBE"(#2, cashacct$, cashacctdescr$,0%, f1%(2%))
                call "GLFMT" (cashacct$)
                return
L20450
*        Currency code                         CURRENCY$
            if curr$ <> "Y" then return
            if maxlines% > 0% then return
            blankline$  = "Enter Currency Code or '?' to see list. Blan"&~
                "k = Statutory."
            enabled% = 1%
            return

        REM *************************************************************~
            * E N A B L E / D E F A U L T   F O R   L I N E   I T E M S *~
            *                                                           *~
            * SET DEFAULTS FOR ACCOUNT NUMBER AND AMOUNT, DEPENDING ON  *~
            * WHETHER OR NOT INVOICE IS ON FILE.                        *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0%
                  c% = currentline%
                  on fieldnr% gosub L21100,         /* INVOICE NUMBER   */~
                                    L21200,         /* ACCOUNT NUMBER   */~
                                    L21300,         /* DEBIT  AMOUNT    */~
                                    L21600,         /* DISCOUNT TAKEN   */~
                                    L21700          /* 1099 CATEGORY    */
                  return
L21100:     REM SET DEFAULTS FOR INVOICE NUMBER
                enabled% = 1%
                return
L21200:     REM SET DEFAULTS FOR ACCOUNT NUMBER
                enabled% = 1% - sgn(f1%(5%))
                if f1%(5%) = 0% then L21245
                   get #5, using L21220, acct$(c%)
L21220:                    FMT XX(56), CH(9)
L21225:            call "DESCRIBE" (#2, acct$(c%), descr$(c%),0%, f1%(2%))
                   call "GLFMT" (acct$(c%))
                   get #2, using L21235, type$(c%)
L21235:                          FMT XX(39), CH(1)
                   if type$(c%)  = "L" then descr$(c%) = "PAYABLES"
                   if type$(c%) <> "L" then descr$(c%) = "DIRECT  "
                   return
L21245:         REM IF INVOICE NOT ON FILE, GET PURCHASES ACCOUNT DEFAULT.
                    get #3, using L21255, acct$(c%)
L21255:                     FMT XX(249), CH(9)
                    if acct$(c%) = " " then acct$(c%) = defpuracct$
                    goto L21225
L21300:     REM SET DEFAULTS FOR DEBIT  AMOUNT
                enabled% = 1%
                if f1%(5) = 0% then return
                   get #5, using L21320, amount
L21320:                    FMT XX(118), PD(14,4)
                REM Add up PAYLNCUR to get transaction amount
                str(currkey$,,9%) = vencode$
                str(currkey$,10%) = invoice$(c%)
                tranamt = 0 : currthere% = 0%
L21345:         call "PLOWNEXT" (#48, currkey$, 25%, f1%(48%))
                     if f1%(48%) = 0% then L21380
                          get #48, using L21360, tamt
L21360:                        FMT POS(33), PD(14,4)
                          tranamt = tranamt + tamt
                          currthere% = 1%
                          goto L21345
L21380:         REM Add up CSHLNCUR to get payments and gain-loss amounts
                call "REDALT0" (#8, invoice$(c%), 1%, f1%(8%))
L21390:              if f1%(8%) <> 1% then L21445
                get #8 using L21400, tven$, tcheck$, tseq$, tinv$
L21400:              FMT CH(9), CH(8), CH(3), CH(16)
                if tinv$ <> invoice$(c%) then L21445
                if tven$ <> vencode$ then L21441
                call "READ100" (#46, str(tven$) & str(tcheck$) &         ~
                                str(tseq$), f1%(46%))
                     if f1%(46%) <> 1% then L21441
                get #46 using L21435, tamt, gain_loss_amt
L21435:              FMT POS(69), PD(14,4), POS(85), PD(14,4)
                tranamt = round(tranamt - tamt + gain_loss_amt, 2%)
L21441:         call "READNEXT" (#8, f1%(8%))
                     goto L21390
L21445:              if currthere% = 1% then amount = tranamt
                REM TO SEE IF THERE'S ANY CHECKS IN CHKBUF2 &/OR CSHBUF2
                gosub L52000
                amount = amount - bufchecktotal
*              AMOUNT = AMOUNT * CONVUNT(C%)
                   call "CONVERT" (amount, -2.2, amount$(c%))
                return

L21600:     REM SET DEFAULTS FOR DISCOUNT TAKEN
                enabled% = 1%
                return

L21700: REM DEFAULT ENABLE FOR 1099 CATEGORY
            if f1%(5%) = 0% then L21760
               get #5, using L21730, ten99$(c%)
L21730:            FMT XX(162), CH(4)
               return

L21760:        get #3, using L21770, ten99$(c%)
L21770:            FMT XX(514), CH(4)
               if ten99$(c%) <> " " then enabled% = 1%
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
            goto L29960

        startover2: REM ALLOW USER OPPORTUNITY TO START OVER.
            keyhit1% = 2%

L29960:     call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *      L O A D   O L D   C H E C K   F R O M   F I L E      *~
            *                                                           *~
            * FIND CHECK NUMBER IN DISBURSEMENTS BUFFER. IF NOT THERE,  *~
            * CHECK THE DISBURSEMENTS MAIN FILE. IF NOT THERE, RETURN SO*~
            * THAT WE CAN INPUT THE NEW INVOICE.                        *~
            *************************************************************

            REM INITIALIZE FOR LOADING UP A CHECK.
                oldcheckonfile%, temp%, maxlines% = 0%
                netcheck,totamt,totdisc=0
                reconciled$ = " "
                str(diskkey$,  1%) = vencode$
                str(diskkey$, 10%) = checknr$

            REM FIRST, TRY BUFFER TO FIND CHECK.
                call "REDALT0" (#9, diskkey$, 2%, f1%(9%))
                      if f1%(9%) = 0% then L30165
                get #9, using L30095, header$,netcheck
L30095:                 FMT XX(31), CH(32),PD(14,4)
                file% = 10%
                currency$ = statutory$
                call "READ100" (#42, key(#9,2%), f1%(42%))
                   if f1%(42%) = 0% then L30275
                get #42 using L30135, currency$, convdate$, conveqv,      ~
                                     convunt, netcheck, discount,        ~
                                     gain_loss$
L30135:             FMT CH(4), XX(17), CH(6), 2*PD(14,7), 2*PD(14,4),    ~
                        XX(8), CH(9)
                put str(header$,7%,8%) using L30150, discount
L30150:             FMT PD(14,4)
                goto L30275

L30165:     REM TRY FOR CHECK IN MAIN DISBURSEMENTS FILE IF NOT IN BUFFER.
                call "READ100" (#7, diskkey$, f1%(7%))
                     if f1%(7) = 0% then return     /* IF NO THEN INPUT */
                get #7, using L30185, header$,netcheck
L30185:                 FMT XX(17), CH(32),XX(24),PD(14,4)
                file% = 8%
                currency$ = statutory$
                call "READ100" (#45, key(#7), f1%(45%))
                   if f1%(45%) = 0% then L30275
                get #45 using L30225, currency$, convdate$, conveqv,      ~
                                     convunt, netcheck, discount,        ~
                                     gain_loss$
L30225:             FMT CH(4), XX(17), CH(6), 2*PD(14,7), 2*PD(14,4),    ~
                        XX(8), CH(9)
                put str(header$,7%,8%) using L30240, discount
L30240:             FMT PD(14,4)
                oldcheckonfile% = 1%
            REM CHECK TO SEE IF MAIN DISBURSEMENTS CHECK HAS BEEN PAID.
                get #7, using L30260, reconciled$
L30260:                 FMT XX(81), CH(1)
                if reconciled$ = "Y" then return

L30275:     REM GET AND FORMAT HEADER INFORMATION.
                oldcheckonfile% = 1%
                get header$, using L30415,                                ~
                             checkdate$, discount, discacct$, cashacct$
                call "DATEFMT" (checkdate$)
                call "DESCRIBE"(#2, discacct$, discacctdescr$,0%, f1%(2%))
                   call "GLFMT" (discacct$)
                call "DESCRIBE"(#2, cashacct$, cashacctdescr$,0%, f1%(2%))
                   call "GLFMT" (cashacct$)
                call "CONVERT" (discount, -2.2, discount$)

            REM READ LINE ITEMS FROM BUFFER FILE.
                if file% = 8% then L30490
L30340:         call "PLOWNEXT" (#10, diskkey$, 17%, f1%(10%))
                     if f1%(10%) = 0% then L30680
                temp%, maxlines% = temp% + 1%
                get #10,    using L30440, invoice$(temp%), acct$(temp%),  ~
                            type$(temp%), amount, disc, invdate$(temp%), ~
                            ten99$(temp%), status$(temp%)
                call "READ100" (#43, key(#10), f1%(43%))
                    if f1%(43%) <> 0% then L30380
                        if curr$ = "Y" and currency$ = statutory$ then   ~
                            currency$(temp%) = currency$
                        goto L30400
L30380:         get #43 using L30395, currency$(temp%), convdate$(temp%), ~
                                     conveqv(temp%), convunt(temp%),     ~
                                     amount, disc
L30395:             FMT CH(4), POS(47), CH(6), 2*PD(14,7), 2*PD(14,4)
L30400:         gosub L30555
                goto L30340

L30415:     FMT CH(6),                   /* DATE OF CHECK              */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(9),                   /* DISCOUNT ACCOUNT           */~
                CH(9)                    /* CASH IN BANK ACCOUNT       */~

L30440:     FMT XX(20),                  /* SKIP KEY(VEN, CHK#, SEQ#)  */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(9),                   /* DEBIT  ACCOUNT             */~
                CH(1),                   /* TYPE CODE (L = LIABILITY)  */~
                PD(14,4),                /* DEBIT  AMOUNT              */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(6),                   /* DATE OF INV WHEN PAID      */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1)                    /* LINE STATUS                */~

L30490:     REM READ LINE ITEMS FROM MAIN FILE.
                call "PLOWNEXT" (#8,  diskkey$, 17%, f1%(8%))
                     if f1%(8%) = 0% then L30680
                temp%, maxlines% = temp% + 1%
                get #8,     using L30625, invoice$(temp%), acct$(temp%),  ~
                            type$(temp%), amount, disc, invdate$(temp%), ~
                            ten99$(temp%), status$(temp%)
                call "READ100" (#46, key(#8), f1%(46%))
                   if f1%(46%) = 0% then L30540
                get #46 using L30395, currency$(temp%), convdate$(temp%), ~
                                     conveqv(temp%), convunt(temp%),     ~
                                     amount, disc
                    FMT CH(4), POS(47), CH(6), 2*PD(14,7), 2*PD(14,4)
L30540:         gosub L30555
                goto L30490

L30555: REM COMMON FORMATTER FOR LINE FIELDS

                call "CONVERT" (amount, 2.2, amount$(temp%))
                call "CONVERT" (disc  , 2.2, disco$(temp%))
                call "DESCRIBE" (#2, acct$(temp%), descr$(temp%),        ~
                                                            0%, f1%(2%))
                call "GLFMT" (acct$(temp%))
                if type$(temp%)  = "L" then descr$(temp%) = "PAYABLES"
                if type$(temp%) <> "L" then descr$(temp%) = "DIRECT  "
                totamt=totamt+amount
                totdisc=totdisc+disc
                if status$(temp%) <> "F" then status$(temp%) = "A"
                return

L30625:     FMT XX(20),                  /* SKIP KEY(VEN, CHK#, SEQ#)  */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(9),                   /* DEBIT  ACCOUNT             */~
                CH(1),                   /* TYPE CODE (L = LIABILITY)  */~
                PD(14,4),                /* DEBIT  AMOUNT              */~
                XX(6),                   /* SKIP DATE POSTED           */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(6),                   /* INV DATE WHEN PAID         */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1)                    /* LINE STATUS                */~

L30680:     netpurged=netcheck-(totamt-totdisc)
            netdiscpurged=discount-totdisc
            netpurged = round(netpurged,2%)
            netdiscpurged = round(netdiscpurged,2%)
            return

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *                                                           *~
            * WRITE DATA TO FILE, DELETING OLD FROM BUFFER SHOULD THAT  *~
            * BE NECESSARY.                                             *~
            *************************************************************

            REM WRITE THIS CHECK'S LINE ITEMS TO BUFFER.
                disctotal, checktotal, check_gain_loss = 0
                if maxlines% = 0% then L31480
                for temp% = 1% to maxlines%
                    convert temp% to seqnr$, pic(###)
                    convert amount$(temp%) to amount
                    convert disco$(temp%) to disc
                    call "GLUNFMT" (acct$(temp%))
                    if status$(temp%) <> "F" then status$(temp%) = " "

                    if currency$ = statutory$ then L31360
                    gain_loss = amount * (conveqv - conveqv(temp%))
                    gain_loss = round(gain_loss, 2%)
                    check_gain_loss = check_gain_loss + gain_loss
                    if convdate$ = " " then convdate$ = blankdate$
                    if convdate$(temp%) = " " then convdate$(temp%) = blankdate$
                    write #43, using L31250, currency$,                   ~
                               vencode$, checknr$, seqnr$,               ~
                               convdate$, conveqv, convunt,              ~
                               convdate$(temp%), conveqv(temp%),         ~
                               convunt(temp%),                           ~
                               amount, disc, gain_loss, " "
L31250:                   FMT  CH(4), CH(9), CH(8), CH(3), CH(6),        ~
                               2*PD(14,7), CH(6), 2*PD(14,7), 3*PD(14,4),~
                               CH(8)

                    checktotal = checktotal + amount - disc
                    disctotal = disctotal + disc

                    amount = round(amount * conveqv, 2%)
                    disc   = round(disc   * conveqv, 2%)
                    goto L31390

L31360:             checktotal = checktotal + amount - disc
                    disctotal = disctotal + disc

L31390:             if invdate$ = " " then invdate$ = blankdate$
                    write #10, using L31730,                              ~
                               vencode$, checknr$, seqnr$,               ~
                               invoice$(temp%), acct$(temp%),            ~
                               type$(temp%), amount, disc,               ~
                               invdate$(temp%), ten99$(temp%),           ~
                               status$(temp%), " "

                    next temp%

L31480:     REM WRITE HEADER NOW THAT CHECK TOTAL COMPUTED OK.
                checktotal = checktotal + netpurged
                discount = disctotal + netdiscpurged
                checktotal = round(checktotal,2%)
                discount   = round(discount,2%)
                call "DATUNFMT" (checkdate$)
                call "FMTKEY" (#9, key$, reversekey$)
                call "GLUNFMT" (discacct$)
                call "GLUNFMT" (cashacct$)

                if currency$ = statutory$ then L31670
                if convdate$ = " " then convdate$ = blankdate$
                write #42, using L31630, currency$, vencode$, checknr$,   ~
                           convdate$, conveqv, convunt,                  ~
                           checktotal, discount, check_gain_loss,        ~
                           gain_loss$, " "
L31630:               FMT CH(4), CH(9), CH(8), CH(6), 2*PD(14,7),        ~
                          3*PD(14,4), CH(9), CH(24)

                checktotal = round(checktotal * conveqv, 2%)
                discount   = round(discount   * conveqv, 2%)

L31670:         if checkdate$ = " " then checkdate$ = blankdate$
                write  #9, using L31860,                                  ~
                           key$, reversekey$, vencode$, checknr$,        ~
                           checkdate$, discount, discacct$,              ~
                           cashacct$, checktotal," "
                return

L31730:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(9),                   /* DEBIT  ACCOUNT             */~
                CH(1),                   /* DEBIT  ACCOUNT TYPE        */~
                PD(14,4),                /* DEBIT  AMOUNT              */~
                PD(14,4),                /* DISCOUNT TAKEN             */~
                CH(6),                   /* INV DATE ON PAYMENT        */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1),                   /* LINE STATUS                */~
                CH(27)                   /* FILLER                     */~

L31860:     FMT CH(7),                   /* BUFFER KEY                 */~
                CH(7),                   /* REVERSE BUFFER KEY         */~
                CH(9),                   /* VENDOR CODE                */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(6),                   /* CHECK DATE                 */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(9),                   /* DISCOUNT ACCOUNT           */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                PD(14,4),                /* NET CHECK AMOUNT           */~
                CH(29)                   /* FILLER                     */

        REM *************************************************************~
            *     I N P U T   C H E C K   H E A D E R   S C R E E N     *~
            *                                                           *~
            * GETS CHECK HEADERS FROM THE SCREEN.  STANDARD INPUT STYLE *~
            *************************************************************

            deffn'201(fieldnr%)
               init (" ") pf$()
               pf$(1%) = "(1)Start Over"
               str(pf$(1%),64%) = "(13)Instructions"
               str(pf$(2%),64%) = "(15)Print Screen"
               if fieldnr% = 1% then str(pf$(3%),64%)= "(16)EXIT PROGRAM"
               pfkey$ = hex(00010d0f10)
               goto L40250

            deffn'202(fieldnr%)
               init (" ") pf$()
               pf$(1%) = "(1)Start Over"
               str(pf$(1%),64%) = "(13)Instructions"
               pf$(2%) = "(2)Line Items"
               str(pf$(2%),19%)="(12)Delete All Lines (Void Issued Check)"
               str(pf$(2%),64%) = "(15)Print Screen"
               if fieldnr% = 0% then str(pf$(3%),64%)= "(16)SAVE DATA   "
               pfkey$ = hex(0001020c0d0f10)

L40250:           header1$ = " "
                  if fieldnr% > 1% then L40290
                  if lastven$ = " " then L40290
                  if checknr$ = " " then header1$ = "Last Vendor: " &    ~
                            lastven$ & "  " & "Last Check: " & lastcheck$
L40290:           str(header1$,63%) = "CSHINPUT:" & cms2v$
                  init(hex(84)) linefac$()
                  if fieldnr% = 0% then init(hex(86)) linefac$()
                  on fieldnr% gosub  L40430,        /* VENDOR CODE      */~
                                     L40430,        /* CHECK NUMBER     */~
                                     L40430,        /* CHECK DATE       */~
                                     L40460,        /* DISCOUNT TAKEN   */~
                                     L40430,        /* DISCOUNT ACCOUNT */~
                                     L40430,        /* CASH IN BANK ACCT*/~
                                     L40430         /* CURRENCY         */
                  goto L40500

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linefac$(fieldnr%) = hex(80)
                      return
L40430:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linefac$(fieldnr%) = hex(81)
                      return
L40460:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linefac$(fieldnr%) = hex(82)
                      return

L40500:     accept                                                       ~
               at (01,02), "Manage A/P Checks",                          ~
               at (01,38),"Post Date: XXXXXXXX  Today's Date:",          ~
               at (01,50), fac(hex(8c)), postdate$              , ch(08),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header1$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Vendor Code",                                         ~
               at (06,30), fac(linefac$(1%)), vencode$          , ch(09),~
               at (06,49), fac(hex(8c)),     venname$           , ch(32),~
               at (07,02),                                               ~
                  "Check Number",                                        ~
               at (07,30), fac(linefac$(2%)), checknr$          , ch(08),~
               at (08,02),                                               ~
                  "Date of Check",                                       ~
               at (08,30), fac(linefac$(3%)), checkdate$        , ch(08),~
               at (09,02),                                               ~
                  "Discount Taken",                                      ~
               at (09,30), fac(linefac$(4%)), discount$         , ch(10),~
               at (10,02),                                               ~
                  "Discount Account",                                    ~
               at (10,30), fac(linefac$(5%)), discacct$         , ch(12),~
               at (10,49), fac(hex(8c)),     discacctdescr$     , ch(32),~
               at (11,02),                                               ~
                  "Cash In Bank Account",                                ~
               at (11,30), fac(linefac$(6%)), cashacct$         , ch(12),~
               at (11,49), fac(hex(8c)),     cashacctdescr$     , ch(32),~
               at (12,02),                                               ~
                  "Check Currency",                                      ~
               at (12,30), fac(linefac$(7%)), currency$         , ch(04),~
               at (12,49), fac(hex(8c)),     currdescr$         , ch(32),~
                                                                         ~
               at (13,02), fac(purgedfac$),purgedmsg$           , ch(79),~
               at (21,02), fac(hex(a4)),     blankline$         , ch(79),~
                                                                         ~
               at (22,02), fac(hex(8c)), pf$(1%)                , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2%)                , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3%)                , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13% then L40930
                  call "MANUAL" ("CSHINPUT")
                  goto L40500

L40930:        if keyhit% <> 15% then L40970
                  call "PRNTSCRN"
                  return

L40970:        REM GET CURSOR POSITION ON SCREEN FOR EDIT AND OTHER FUN.
                   close ws
                   call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
                   return

        REM *************************************************************~
            *         T A B U L A R   I N P U T   S C R E E N S         *~
            *                                                           *~
            * SCREEN CODE FOR INPUT, EDIT, INSERT, AND DELETE MODES.    *~
            *************************************************************

            deffn'203(fieldnr%)
                  screen% = 1%
                  goto L42290

            deffn'213(fieldnr%)
                  screen% = 2%
                  init(hex(86)) fac$()
                  if fieldnr% = 0% then L42300
                  goto L42290

            deffn'223(fieldnr%)
                  screen% = 3%
                  goto L42290

            deffn'233(screenline%)
                  screen% = 4%
                  init(hex(84)) fac$()
                  for temp% = 1% to 4%
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L42470

            REM SET FAC'S FOR INPUT SCREENS.
L42290:         init(hex(84)) fac$()
L42300:         on fieldnr% gosub  L42400,          /* INVOICE NUMBER   */~
                                   L42400,          /* DEBIT  ACCOUNT   */~
                                   L42430,          /* AMOUNT           */~
                                   L42430,          /* DISCOUNT TAKEN   */~
                                   L42400           /* 1099 CATEGORY    */
                goto L42470

                REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                    fac$(screenline%, fieldnr%) = hex(80)
                    return
L42400:         REM SET FAC'S FOR UPPER CASE ONLY INPUT
                    fac$(screenline%, fieldnr%) = hex(81)
                    return
L42430:         REM SET FAC'S FOR UPPER CASE ONLY INPUT
                    fac$(screenline%, fieldnr%) = hex(82)
                    return

L42470:     accept                                                       ~
               at (01,02), fac(hex(8c)), tttle$(screen%, 1%)    , ch(63),~
               at (02,02), fac(hex(8c)), tttle$(screen%, 2%)    , ch(63),~
               at (01,65),                                               ~
                  "! VEN:",                                              ~
               at (01,72), fac(hex(84)), vencode$               , ch(09),~
               at (02,65),                                               ~
                  "! CHK:",                                              ~
               at (02,72), fac(hex(84)), checknr$               , ch(09),~
               at (03,02), fac(hex(94)), errormsg$              , ch(63),~
               at (03,65),                                               ~
                  "+---------------",                                    ~
               at (04,02), fac(hex(84)), infomsg$               , ch(79),~
               at (05,02), fac(hex(ac)), colhdr$                , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)),    status$ (line%+ 1%) , ch( 1),~
               at (07,02), fac(hex(8c)),    status$ (line%+ 2%) , ch( 1),~
               at (08,02), fac(hex(8c)),    status$ (line%+ 3%) , ch( 1),~
               at (09,02), fac(hex(8c)),    status$ (line%+ 4%) , ch( 1),~
               at (10,02), fac(hex(8c)),    status$ (line%+ 5%) , ch( 1),~
               at (11,02), fac(hex(8c)),    status$ (line%+ 6%) , ch( 1),~
               at (12,02), fac(hex(8c)),    status$ (line%+ 7%) , ch( 1),~
               at (13,02), fac(hex(8c)),    status$ (line%+ 8%) , ch( 1),~
               at (14,02), fac(hex(8c)),    status$ (line%+ 9%) , ch( 1),~
               at (15,02), fac(hex(8c)),    status$ (line%+10%) , ch( 1),~
               at (16,02), fac(hex(8c)),    status$ (line%+11%) , ch( 1),~
               at (17,02), fac(hex(8c)),    status$ (line%+12%) , ch( 1),~
               at (18,02), fac(hex(8c)),    status$ (line%+13%) , ch( 1),~
               at (19,02), fac(hex(8c)),    status$ (line%+14%) , ch( 1),~
               at (20,02), fac(hex(8c)),    status$ (line%+15%) , ch( 1),~
               at (21,02), fac(hex(8c)),    status$ (line%+16%) , ch( 1),~
               at (22,02), fac(hex(8c)),    status$ (line%+17%) , ch( 1),~
               at (23,02), fac(hex(8c)),    status$ (line%+18%) , ch( 1),~
               at (24,02), fac(hex(8c)),    status$ (line%+19%) , ch( 1),~
                                                                         ~
               at (06,05), fac(fac$( 1%,1%)),invoice$(line%+ 1%), ch(16),~
               at (07,05), fac(fac$( 2%,1%)),invoice$(line%+ 2%), ch(16),~
               at (08,05), fac(fac$( 3%,1%)),invoice$(line%+ 3%), ch(16),~
               at (09,05), fac(fac$( 4%,1%)),invoice$(line%+ 4%), ch(16),~
               at (10,05), fac(fac$( 5%,1%)),invoice$(line%+ 5%), ch(16),~
               at (11,05), fac(fac$( 6%,1%)),invoice$(line%+ 6%), ch(16),~
               at (12,05), fac(fac$( 7%,1%)),invoice$(line%+ 7%), ch(16),~
               at (13,05), fac(fac$( 8%,1%)),invoice$(line%+ 8%), ch(16),~
               at (14,05), fac(fac$( 9%,1%)),invoice$(line%+ 9%), ch(16),~
               at (15,05), fac(fac$(10%,1%)),invoice$(line%+10%), ch(16),~
               at (16,05), fac(fac$(11%,1%)),invoice$(line%+11%), ch(16),~
               at (17,05), fac(fac$(12%,1%)),invoice$(line%+12%), ch(16),~
               at (18,05), fac(fac$(13%,1%)),invoice$(line%+13%), ch(16),~
               at (19,05), fac(fac$(14%,1%)),invoice$(line%+14%), ch(16),~
               at (20,05), fac(fac$(15%,1%)),invoice$(line%+15%), ch(16),~
               at (21,05), fac(fac$(16%,1%)),invoice$(line%+16%), ch(16),~
               at (22,05), fac(fac$(17%,1%)),invoice$(line%+17%), ch(16),~
               at (23,05), fac(fac$(18%,1%)),invoice$(line%+18%), ch(16),~
               at (24,05), fac(fac$(19%,1%)),invoice$(line%+19%), ch(16),~
                                                                         ~
               at (06,23), fac(hex(8c)), currency$(line%+ 1%)   , ch(04),~
               at (07,23), fac(hex(8c)), currency$(line%+ 2%)   , ch(04),~
               at (08,23), fac(hex(8c)), currency$(line%+ 3%)   , ch(04),~
               at (09,23), fac(hex(8c)), currency$(line%+ 4%)   , ch(04),~
               at (10,23), fac(hex(8c)), currency$(line%+ 5%)   , ch(04),~
               at (11,23), fac(hex(8c)), currency$(line%+ 6%)   , ch(04),~
               at (12,23), fac(hex(8c)), currency$(line%+ 7%)   , ch(04),~
               at (13,23), fac(hex(8c)), currency$(line%+ 8%)   , ch(04),~
               at (14,23), fac(hex(8c)), currency$(line%+ 9%)   , ch(04),~
               at (15,23), fac(hex(8c)), currency$(line%+10%)   , ch(04),~
               at (16,23), fac(hex(8c)), currency$(line%+11%)   , ch(04),~
               at (17,23), fac(hex(8c)), currency$(line%+12%)   , ch(04),~
               at (18,23), fac(hex(8c)), currency$(line%+13%)   , ch(04),~
               at (19,23), fac(hex(8c)), currency$(line%+14%)   , ch(04),~
               at (20,23), fac(hex(8c)), currency$(line%+15%)   , ch(04),~
               at (21,23), fac(hex(8c)), currency$(line%+16%)   , ch(04),~
               at (22,23), fac(hex(8c)), currency$(line%+17%)   , ch(04),~
               at (23,23), fac(hex(8c)), currency$(line%+18%)   , ch(04),~
               at (24,23), fac(hex(8c)), currency$(line%+19%)   , ch(04),~
                                                                         ~
               at (06,28), fac(hex(84)), currflag$(line%+ 1%)   , ch(01),~
               at (07,28), fac(hex(84)), currflag$(line%+ 2%)   , ch(01),~
               at (08,28), fac(hex(84)), currflag$(line%+ 3%)   , ch(01),~
               at (09,28), fac(hex(84)), currflag$(line%+ 4%)   , ch(01),~
               at (10,28), fac(hex(84)), currflag$(line%+ 5%)   , ch(01),~
               at (11,28), fac(hex(84)), currflag$(line%+ 6%)   , ch(01),~
               at (12,28), fac(hex(84)), currflag$(line%+ 7%)   , ch(01),~
               at (13,28), fac(hex(84)), currflag$(line%+ 8%)   , ch(01),~
               at (14,28), fac(hex(84)), currflag$(line%+ 9%)   , ch(01),~
               at (15,28), fac(hex(84)), currflag$(line%+10%)   , ch(01),~
               at (16,28), fac(hex(84)), currflag$(line%+11%)   , ch(01),~
               at (17,28), fac(hex(84)), currflag$(line%+12%)   , ch(01),~
               at (18,28), fac(hex(84)), currflag$(line%+13%)   , ch(01),~
               at (19,28), fac(hex(84)), currflag$(line%+14%)   , ch(01),~
               at (20,28), fac(hex(84)), currflag$(line%+15%)   , ch(01),~
               at (21,28), fac(hex(84)), currflag$(line%+16%)   , ch(01),~
               at (22,28), fac(hex(84)), currflag$(line%+17%)   , ch(01),~
               at (23,28), fac(hex(84)), currflag$(line%+18%)   , ch(01),~
               at (24,28), fac(hex(84)), currflag$(line%+19%)   , ch(01),~
                                                                         ~
               at (06,31), fac(fac$( 1%,2%)), acct$(line%+ 1%)  , ch(12),~
               at (07,31), fac(fac$( 2%,2%)), acct$(line%+ 2%)  , ch(12),~
               at (08,31), fac(fac$( 3%,2%)), acct$(line%+ 3%)  , ch(12),~
               at (09,31), fac(fac$( 4%,2%)), acct$(line%+ 4%)  , ch(12),~
               at (10,31), fac(fac$( 5%,2%)), acct$(line%+ 5%)  , ch(12),~
               at (11,31), fac(fac$( 6%,2%)), acct$(line%+ 6%)  , ch(12),~
               at (12,31), fac(fac$( 7%,2%)), acct$(line%+ 7%)  , ch(12),~
               at (13,31), fac(fac$( 8%,2%)), acct$(line%+ 8%)  , ch(12),~
               at (14,31), fac(fac$( 9%,2%)), acct$(line%+ 9%)  , ch(12),~
               at (15,31), fac(fac$(10%,2%)), acct$(line%+10%)  , ch(12),~
               at (16,31), fac(fac$(11%,2%)), acct$(line%+11%)  , ch(12),~
               at (17,31), fac(fac$(12%,2%)), acct$(line%+12%)  , ch(12),~
               at (18,31), fac(fac$(13%,2%)), acct$(line%+13%)  , ch(12),~
               at (19,31), fac(fac$(14%,2%)), acct$(line%+14%)  , ch(12),~
               at (20,31), fac(fac$(15%,2%)), acct$(line%+15%)  , ch(12),~
               at (21,31), fac(fac$(16%,2%)), acct$(line%+16%)  , ch(12),~
               at (22,31), fac(fac$(17%,2%)), acct$(line%+17%)  , ch(12),~
               at (23,31), fac(fac$(18%,2%)), acct$(line%+18%)  , ch(12),~
               at (24,31), fac(fac$(19%,2%)), acct$(line%+19%)  , ch(12),~
                                                                         ~
               at (06,44), fac(hex(8c)),    descr$(line%+ 1%)   , ch(08),~
               at (07,44), fac(hex(8c)),    descr$(line%+ 2%)   , ch(08),~
               at (08,44), fac(hex(8c)),    descr$(line%+ 3%)   , ch(08),~
               at (09,44), fac(hex(8c)),    descr$(line%+ 4%)   , ch(08),~
               at (10,44), fac(hex(8c)),    descr$(line%+ 5%)   , ch(08),~
               at (11,44), fac(hex(8c)),    descr$(line%+ 6%)   , ch(08),~
               at (12,44), fac(hex(8c)),    descr$(line%+ 7%)   , ch(08),~
               at (13,44), fac(hex(8c)),    descr$(line%+ 8%)   , ch(08),~
               at (14,44), fac(hex(8c)),    descr$(line%+ 9%)   , ch(08),~
               at (15,44), fac(hex(8c)),    descr$(line%+10%)   , ch(08),~
               at (16,44), fac(hex(8c)),    descr$(line%+11%)   , ch(08),~
               at (17,44), fac(hex(8c)),    descr$(line%+12%)   , ch(08),~
               at (18,44), fac(hex(8c)),    descr$(line%+13%)   , ch(08),~
               at (19,44), fac(hex(8c)),    descr$(line%+14%)   , ch(08),~
               at (20,44), fac(hex(8c)),    descr$(line%+15%)   , ch(08),~
               at (21,44), fac(hex(8c)),    descr$(line%+16%)   , ch(08),~
               at (22,44), fac(hex(8c)),    descr$(line%+17%)   , ch(08),~
               at (23,44), fac(hex(8c)),    descr$(line%+18%)   , ch(08),~
               at (24,44), fac(hex(8c)),    descr$(line%+19%)   , ch(08),~
                                                                         ~
               at (06,54), fac(fac$( 1%,3%)),amount$(line%+ 1%) , ch(10),~
               at (07,54), fac(fac$( 2%,3%)),amount$(line%+ 2%) , ch(10),~
               at (08,54), fac(fac$( 3%,3%)),amount$(line%+ 3%) , ch(10),~
               at (09,54), fac(fac$( 4%,3%)),amount$(line%+ 4%) , ch(10),~
               at (10,54), fac(fac$( 5%,3%)),amount$(line%+ 5%) , ch(10),~
               at (11,54), fac(fac$( 6%,3%)),amount$(line%+ 6%) , ch(10),~
               at (12,54), fac(fac$( 7%,3%)),amount$(line%+ 7%) , ch(10),~
               at (13,54), fac(fac$( 8%,3%)),amount$(line%+ 8%) , ch(10),~
               at (14,54), fac(fac$( 9%,3%)),amount$(line%+ 9%) , ch(10),~
               at (15,54), fac(fac$(10%,3%)),amount$(line%+10%) , ch(10),~
               at (16,54), fac(fac$(11%,3%)),amount$(line%+11%) , ch(10),~
               at (17,54), fac(fac$(12%,3%)),amount$(line%+12%) , ch(10),~
               at (18,54), fac(fac$(13%,3%)),amount$(line%+13%) , ch(10),~
               at (19,54), fac(fac$(14%,3%)),amount$(line%+14%) , ch(10),~
               at (20,54), fac(fac$(15%,3%)),amount$(line%+15%) , ch(10),~
               at (21,54), fac(fac$(16%,3%)),amount$(line%+16%) , ch(10),~
               at (22,54), fac(fac$(17%,3%)),amount$(line%+17%) , ch(10),~
               at (23,54), fac(fac$(18%,3%)),amount$(line%+18%) , ch(10),~
               at (24,54), fac(fac$(19%,3%)),amount$(line%+19%) , ch(10),~
                                                                         ~
               at (06,65), fac(fac$( 1%,4%)),disco$ (line%+ 1%) , ch(10),~
               at (07,65), fac(fac$( 2%,4%)),disco$ (line%+ 2%) , ch(10),~
               at (08,65), fac(fac$( 3%,4%)),disco$ (line%+ 3%) , ch(10),~
               at (09,65), fac(fac$( 4%,4%)),disco$ (line%+ 4%) , ch(10),~
               at (10,65), fac(fac$( 5%,4%)),disco$ (line%+ 5%) , ch(10),~
               at (11,65), fac(fac$( 6%,4%)),disco$ (line%+ 6%) , ch(10),~
               at (12,65), fac(fac$( 7%,4%)),disco$ (line%+ 7%) , ch(10),~
               at (13,65), fac(fac$( 8%,4%)),disco$ (line%+ 8%) , ch(10),~
               at (14,65), fac(fac$( 9%,4%)),disco$ (line%+ 9%) , ch(10),~
               at (15,65), fac(fac$(10%,4%)),disco$ (line%+10%) , ch(10),~
               at (16,65), fac(fac$(11%,4%)),disco$ (line%+11%) , ch(10),~
               at (17,65), fac(fac$(12%,4%)),disco$ (line%+12%) , ch(10),~
               at (18,65), fac(fac$(13%,4%)),disco$ (line%+13%) , ch(10),~
               at (19,65), fac(fac$(14%,4%)),disco$ (line%+14%) , ch(10),~
               at (20,65), fac(fac$(15%,4%)),disco$ (line%+15%) , ch(10),~
               at (21,65), fac(fac$(16%,4%)),disco$ (line%+16%) , ch(10),~
               at (22,65), fac(fac$(17%,4%)),disco$ (line%+17%) , ch(10),~
               at (23,65), fac(fac$(18%,4%)),disco$ (line%+18%) , ch(10),~
               at (24,65), fac(fac$(19%,4%)),disco$ (line%+19%) , ch(10),~
                                                                         ~
               at (06,76), fac(fac$( 1%,5%)),ten99$ (line%+ 1%) , ch( 4),~
               at (07,76), fac(fac$( 2%,5%)),ten99$ (line%+ 2%) , ch( 4),~
               at (08,76), fac(fac$( 3%,5%)),ten99$ (line%+ 3%) , ch( 4),~
               at (09,76), fac(fac$( 4%,5%)),ten99$ (line%+ 4%) , ch( 4),~
               at (10,76), fac(fac$( 5%,5%)),ten99$ (line%+ 5%) , ch( 4),~
               at (11,76), fac(fac$( 6%,5%)),ten99$ (line%+ 6%) , ch( 4),~
               at (12,76), fac(fac$( 7%,5%)),ten99$ (line%+ 7%) , ch( 4),~
               at (13,76), fac(fac$( 8%,5%)),ten99$ (line%+ 8%) , ch( 4),~
               at (14,76), fac(fac$( 9%,5%)),ten99$ (line%+ 9%) , ch( 4),~
               at (15,76), fac(fac$(10%,5%)),ten99$ (line%+10%) , ch( 4),~
               at (16,76), fac(fac$(11%,5%)),ten99$ (line%+11%) , ch( 4),~
               at (17,76), fac(fac$(12%,5%)),ten99$ (line%+12%) , ch( 4),~
               at (18,76), fac(fac$(13%,5%)),ten99$ (line%+13%) , ch( 4),~
               at (19,76), fac(fac$(14%,5%)),ten99$ (line%+14%) , ch( 4),~
               at (20,76), fac(fac$(15%,5%)),ten99$ (line%+15%) , ch( 4),~
               at (21,76), fac(fac$(16%,5%)),ten99$ (line%+16%) , ch( 4),~
               at (22,76), fac(fac$(17%,5%)),ten99$ (line%+17%) , ch( 4),~
               at (23,76), fac(fac$(18%,5%)),ten99$ (line%+18%) , ch( 4),~
               at (24,76), fac(fac$(19%,5%)),ten99$ (line%+19%) , ch( 4),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 15% then L44340
                  call "PRNTSCRN"
                  goto L42470

L44340:        if screen% <> 2% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *             P R O O F   T O T A L   S C R E E N           *~
            *                                                           *~
            * PROOF TOTAL SCREEN  GETS PROOF TOTAL AND COMPARES IT WITH *~
            * THE TOTAL WE HAVE ACCUMULATED.                            *~
            *************************************************************

            deffn'209
                  header1$ = "For Vendor: " & vencode$
                  header1$ = header1$ & "  Check Number: " & checknr$
                  str(header1$,63%) = "CSHINPUT:" & cms2v$

L49120:     accept                                                       ~
               at (01,02), "A/P Check Proof Total",                      ~
               at (01,38), "Post Date: XXXXXXXX  Today's Date:",         ~
               at (01,50), fac(hex(8c)), postdate$              , ch(08),~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header1$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (09,16),                                               ~
                  "****************  ENTER PROOF TOTAL  ***************",~
               at (10,16),                                               ~
                  "*                                                  *",~
               at (11,16),                                               ~
                  "*                                                  *",~
               at (12,16),                                               ~
                  "*         NET AMOUNT OF CHECK =",                     ~
               at (12,48), fac(hex(82)), prooftotal$            , ch(10),~
               at (12,59),                                               ~
                  "        *",                                           ~
               at (13,16),                                               ~
                  "*                                                  *",~
               at (14,16),                                               ~
                  "*                                                  *",~
               at (15,16),                                               ~
                  "****************************************************",~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (23,02),                                               ~
                  "(1)Return To Edit Mode",                              ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(ENTER)Try Proof Total Again",                        ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13% then L49580
                  call "MANUAL" ("CSHINPUT")
                  goto L49120

L49580:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L49120

        REM *************************************************************~
            *              T E S T   H E A D E R   D A T A              *~
            *                                                           *~
            * MAKES SURE THE VENDOR IS ON FILE, THAT THE CHECK ISN'T,   *~
            * (LOADS AND DROPS INTO EDIT MODE IF IT IS), AND CHECKS     *~
            * OUT THE OTHER NUMBERS TO MAKE SURE THEY'RE OK.            *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50100,         /* VENDOR           */~
                                    L50200,         /* CHECK NUMBER     */~
                                    L50300,         /* CHECK DATE       */~
                                    L50400,         /* DISCOUNT AMOUNT  */~
                                    L50500,         /* DISCOUNT ACCOUNT */~
                                    L50600,         /* CASH IN BANK ACCT*/~
                                    L50710          /* CURRENCY         */
                  return

L50100:     REM TEST TO SEE THAT VENDOR IS ON FILE
                call "GETCODE" (#3, vencode$, venname$, 0%, 1.3, f1%(3%))
                    if f1%(3%) = 1% then return
                       errormsg$ = "Vendor Not on File: " & vencode$
                       return
L50200:     REM TEST FOR CHECK ON FILE, LOAD UP IF YES.
                if editmode% = 1% then return      /* NO TEST IF EDIT  */
                if checknr$ <> " " then L50205
                   errormsg$ = "Blank Check Number Not Allowed."
                   return
L50205:         gosub check_on_file
                   if str(checknr$,1%,1%)="M" then return

                convert checknr$ to checknr%, data goto L50218
                convert checknr% to str(checknr$,1%,8%), pic (00000000)

                gosub check_on_file

                str (checknr$,1%,1%)="M"
                gosub check_on_file

                return

L50218:            errormsg$="Invalid Check Number, Must Begin with 'M': ~
        ~" & checknr$
                   return
        check_on_file

            gosub L30000
            if oldcheckonfile% = 0% then L50235
            if reconciled$ = "Y" then L50231        /* CHECK RECONCILED?*/
               return clear
               return clear
               return clear
                 goto editmode

L50231:            errormsg$ = "This Check Has Been Reconciled, No Edit A~
        ~llowed!"
                   goto L50248

L50235:     call "REDALT0" (#7, checknr$, 3%, f1%(7%))
                if f1%(7%) = 0% then L50241
            get #7, using L50238, othervendor$
L50238:         FMT CH(9)
            goto L50246

L50241:     call "REDALT0" (#9, checknr$, 3%, f1%(9%))
                if f1%(9%) = 0% then return
            get #9, using L50244, othervendor$
L50244:         FMT XX(14), CH(9)

L50246:            errormsg$ = "Check Number " & checknr$ &              ~
                                   " Exists for Vendor: " & othervendor$
L50248:            return clear
                   return

L50300:     REM TEST FOR DATE OF CHECK OK.
                call "DATEOK" ( checkdate$, 0%, errormsg$ )
                if errormsg$ = " " then call "DATREVRS" ( checkdate$, rev_date$, errormsg$ )
                return

L50400:     REM TEST FOR DISCOUNT TAKEN OK, SET ZERO IF BLANK.
                call "NUMTEST"(discount$,-9e7,9e7,errormsg$,2.2,discount)
                return

L50500:     REM TEST FOR DISCOUNT ACCOUNT, GET DESCRIPTION IF YES
                call "GETCODE"(#2,discacct$,discacctdescr$,0%,0,f1%(2%))
                     if f1%(2%) = 1% then return
                        errormsg$ = "Discount Account Not on File: "     ~
                                             & discacct$
                        return
L50600:     REM TEST FOR CASH IN BANK ACCOUNT, DESCRIBE IF HERE.
                call "GETCODE"(#2,cashacct$,cashacctdescr$,0%,0,f1%(2%))
                     if f1%(2%) = 1% then return
                        errormsg$ = "Cash in Bank Account Not on File: " ~
                                             & cashacct$
                        return

L50710
*        Currency code                       CURRENCY$
            convdate$ = " " : conveqv, convunt = 1:gain_loss$ = " "
            if curr$ <> "Y" then return
            if currency$ = " " then currency$ = statutory$
            if currency$ = "?" then currency$ = " "
            call "GETCODE" (#40, currency$, currdescr$, 0%, 0, f1%(40%))
            if f1%(40%) <> 0% then goto L50780
L50760:         errormsg$ = "Invalid Currency code.  Try again." : return
L50780:     if currency$ = statutory$ then L50840
            get #40 using L50782, gain_loss$
L50782:         FMT POS(41), CH(9)
            currkey$ = str(currtype$) & str(currency$) & rev_date$
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41%))
               if f1%(41%) = 0% then L50760
            get #41 using L50830, convdate$, conveqv, convunt
L50830:         FMT POS(12), CH(6), 2*PD(14,7)
L50840:     blankline$ = " "
            return

        REM *************************************************************~
            *   T E S T   D A T A   F O R   C H E C K   D E T A I L S   *~
            *                                                           *~
            * MAKES SURE ALL THE NECESSARY DATA IS ON FILE; LOADS       *~
            * CHECK HEADERS FROM FILE FOR EDITING.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, infomsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub L51170,         /* INVOICE #        */~
                                    L51650,         /* DEBIT  ACCOUNT   */~
                                    L51760,         /* AMOUNT PAID OFF  */~
                                    L51790,         /* DISCOUNT TAKEN   */~
                                    L51880          /* 1099 CATEGORY    */
                  return

L51170:     REM TEST FOR INVOICE ON FILE.
                if amount$(c%) <> " " then return  /* EDIT MODE        */
                f1%(5%) = 0%
                if invoice$(c%) = " " then L51570   /* DIRECT PAYMENT   */
                REM VERIFY NO PAYMENT TO INVOICE THIS EXECUTION ALREADY
                     if maxlines% = 0% then goto L51290
                     for temp% = 1% to maxlines% + 1%
                     if temp% = c% then goto L51280
                     if invoice$(temp%) <> invoice$(c%) then L51280
                     convert temp% to numchar$, pic(####) : errormsg$ =  ~
                "Invoice Payment This Run with Entry" & numchar$ : return
L51280:                 next temp%
L51290:         gosub L52000
                if payok% = 0% then L51410          /*NO BUFFER CHECKS */
                   REM INVOICE BEING PAID BY CHKBUF2 (AND/OR) CSHBUF2
                   infomsg$="WARNING:Inv.Paid by"
                   if numchk% = 0% then L51350
                   str(infomsg$,len(infomsg$)+2%)="CHKBUF=" & chkchk$
L51350:            on payok% goto L51410,L51370,L51360   /*CHK,CSH, BOTH */
L51360:              str(infomsg$,len(infomsg$)+2%)="&"
L51370:            convert numcsh% to numchar$, pic(####)
                   str(infomsg$,len(infomsg$)+2%) = numchar$ &           ~
                            " CSHBUF CHECKS,LAST=" & cshchk$

L51410:         str(diskkey$, 1%) = vencode$
                str(diskkey$,10%) = invoice$(c%)
                call "READ100" (#5, diskkey$, f1%(5%))
                     if f1%(5%) = 0% then L51570
                if infomsg$=" "then infomsg$ = "Invoice On File."
                   get #5, using L51443, invdate$(c%)
L51443:                     FMT XX(41), CH(6)
                     if curr$ <> "Y" then L51590
                        currkey$ = diskkey$
                        call "PLOWNEXT" (#48, currkey$, 25%, f1%(48%))
                           if f1%(48%) = 0% then L51520
                        if str(key (#48,1%),1%,4%) = currency$ then L51525
L51490:                    errormsg$ = "Check Currency Does Not Match" & ~
                                       " Invoice Currency"
                           return
L51520:                 if currency$ = statutory$ then L51590 else L51490
L51525:            get #48 using L51535, convdate$(c%), conveqv(c%),      ~
                                        convunt(c%)
L51535:                FMT POS(49), CH(6), 2*PD(14,7)
                   currency$(c%) = currency$
                   return
L51570:         REM IF INVOICE NOT ON FILE, SAY SO.
                   if infomsg$=" "then infomsg$="Invoice >>Not<< On File."
                   invdate$(c%) = date
L51590:            currency$(c%) = currency$
                   convdate$(c%) = convdate$
                   conveqv  (c%) = conveqv
                   convunt  (c%) = convunt
                      return
L51650:     REM EXAMINE THE DEBIT ACCOUNT.
                descr$(c%) = " "
                call "GETCODE"(#2, acct$(c%), descr$(c%), 0%, 0, f1%(2%))
                if f1%(2%) = 0% then L51740
                   get #2, using L51700 , type$(c%)
L51700:                        FMT XX(39), CH(1)
                if type$(c%)  = "L" then descr$(c%) = "PAYABLES"
                if type$(c%) <> "L" then descr$(c%) = "DIRECT  "
                   return
L51740:         errormsg$="ACCOUNT NOT ON FILE: " & acct$(c%)
                return
L51760:     REM CHECK OUT THE AMOUNT.  NOTE NEGATIVES ALLOWED.
                call "NUMTEST"(amount$(c%),-9e7,9e7,errormsg$,-2.2,temp)
                return
L51790:     REM CHECK OUT THE DISCOUNT.
                low = -9e7
                top =  9e7
                convert amount$(c%) to temp, data goto L51850
                if temp >= 0 then top = temp else low = temp
                if temp >= 0 then low = 0    else top = 0
L51850:         call "NUMTEST"(disco$(c%),low,top,errormsg$,-2.2,temp)
                return

L51880: REM 1099 CATEGORY VALIDATION

            if ten99$(c%) = " " then return
            readkey$ = "1099 CATS" & ten99$(c%)
            work$ = hex(06) & "Select 1099 Category"
            call "PLOWCODE" (#13, readkey$, work$, 9%, 0.30, f1%(13%))
            if f1%(13%) = 1% then L51970
                errormsg$ = "Invalid 1099 Category Code"
                return
L51970:     ten99$(c%) = str(readkey$,10%)
            return

L52000: REM *************************************************************~
            * TEST OK TO PAY THIS INVOICE. IT IS*NOW OK*IF THERE IS ANY *~
            * CHECK PENDING (IE IN CSHBUF2 OR CHKBUF2) ADDRESSING THIS  *~
            * INVOICE                                                   *~
            *************************************************************
            payok%,numchk%,numcsh%=0% : bufchecktotal=0
            chkchk$,cshchk$ = " "
            findkey$=invoice$(c%)
            REM Won't find anything here for a MC transaction since only ~
                manual checks are allowed at this time (R6)
            call "REDALT0" (#11,findkey$,1%,f1%(11%))
            goto L52075
L52070:          call "READNEXT" (#11,f1%(11%))
L52075:     if f1%(11%) = 0% then L52130
            get #11, using L52085, tven$,tcheck$,tinv$,tdebit,tdiscount
L52085:              FMT CH(09),CH(08),XX(03),CH(16),XX(10),2*PD(14,4)
            if tinv$<>invoice$(c%) then L52130
            if tven$<>vencode$ then L52070
               REM FOUND SAME INVOICE,VENDOR ADD AMOUNT TO BUFFER SUM
               bufchecktotal = bufchecktotal + tdebit + tdiscount
                  REM UPDATE FLAGS AND CHECK NUMBER
                  numchk% = numchk% + 1% :  chkchk$ = tcheck$
            goto L52070

L52130:     call "REDALT0" (#10,findkey$,1%,f1%(10%))
            goto L52145
L52140:          call "READNEXT" (#10,f1%(10%))
L52145:     if f1%(10%) = 0% then L52200
            get #10, using L52155, tven$,tcheck$,tseq$, tinv$,tdebit,     ~
                                  tdiscount
L52155:              FMT CH(09),CH(08),CH(03),CH(16),XX(10),2*PD(14,4)
            if tinv$<>invoice$(c%) then L52200
            if tven$<>vencode$ then L52140
               REM FOUND SAME INVOICE,VENDOR ADD AMOUNT TO BUFFER SUM
               REM See if there's any MC stuff first
               str(currkey$,,9%)    = tven$
               str(currkey$,10%,8%) = tcheck$
               str(currkey$,18%,3%) = tseq$
               call "READ100" (#43, currkey$, f1%(43%))
               if f1%(43%)<> 0% then get #43, using L52177,tdebit,tdiscount
L52177:            FMT POS(69), 2*PD(14,4)
               bufchecktotal = bufchecktotal + tdebit + tdiscount
                  REM UPDATE FLAGS AND CHECK NUMBER
                  numcsh% = numcsh% + 1% :  cshchk$ = tcheck$
            goto L52140

L52200:     if numchk% > 0% then payok% = 1%
            if numcsh% > 0% then payok% = payok% + 2%

            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            REM SET RETURN CODE FROM BUFFER.
                readkey$ = userid$
                call "PLOWNEXT" (#9, readkey$, 3%, f1%(9%))
            end f1%(9%)
