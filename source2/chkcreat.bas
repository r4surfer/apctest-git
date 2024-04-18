        REM *************************************************************~
            *                                                           *~
            *   CCC   H   H  K   K   CCC   RRRR   EEEEE   AAA   TTTTT   *~
            *  C   C  H   H  K  K   C   C  R   R  E      A   A    T     *~
            *  C      HHHHH  KKK    C      RRRR   EEEE   AAAAA    T     *~
            *  C   C  H   H  K  K   C   C  R   R  E      A   A    T     *~
            *   CCC   H   H  K   K   CCC   R   R  EEEEE  A   A    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHKCREAT - CREATES CHECKS FOR PROCESSING AUTOMATICALLY    *~
            *            LATER.  PERMITS EITHER A RANGE OF VENDORS, OR A*~
            *            SINGLE VENDOR, PAY TAKING OR NOT TAKING        *~
            *            DISCOUNTS.                                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/03/80 ! ORIGINAL                                 ! BCW *~
            * 07/31/80 ! MODIFIED BUFFER STRUCTURE                ! BCW *~
            * 06/07/81 ! MODS TO 16 CHAR INVOICE NUMBER           ! TOM *~
            * 06/20/81 ! LESS LIBERAL WITH DISCOUNT TAKING        ! TEM *~
            * 03/11/83 ! CORRECTED ERRORS IN CONTROL NUMBERS      ! KEN *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! CALLS TO 'FILEOPEN' CHANGED TO 'OPENFILE'! HES *~
            * 01/25/84 ! Track credit invoices - don't skip@ 21750! ECR *~
            *          ! Read PAYMASTR for dicsount rate. U/L Case!     *~
            * 07/09/85 ! FINAL CLEANUP & GL CODE FMT CONTROL      ! KEN *~
            *          ! PROGRAM STANDARD IS INTERNAL GL CODE     ! KEN *~
            * 12/13/85 ! Vendor file format changes               ! MJB *~
            * 01/08/86 ! 1099 Modifications                       ! KAB *~
            * 05/15/86 ! INVOICE FILE FORMAT CHANGE               ! HES *~
            * 02/05/88 ! Test for non-statutory currency          ! KAB *~
            * 12/30/93 ! Added test of Non-A/P liability accounts.! JBK *~
            *          !  Any Invoice coded to a Non-A/P liab.    !     *~
            *          !  will be skipped.                        !     *~
            * 01/16/95 ! PRR 13343. Max # of Non-AP Accts now 50. ! JDH *~
            * 08/01/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            accttyp$1,                   /* Payables Acct Type         */~
            apkey$20,                    /* SYSFILE2 Key Field         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* LINE UNDERLINED IN SCREEN  */~
            cashacct$9,                  /* CASH IN BANK ACCT THIS VEN */~
            checkdate$10,                /* CHECK DATE FOR BUFFER      */~
            checknr$8,                   /* CHECK NUMBER INFORMATION   */~
            currkey$50,                  /* CURRENCY FILE KEY          */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* DATE SHOWN ON SCREEN       */~
            date$(3)8,                   /* DATES FOR VALIDATION       */~
            defcashacct$9,               /* CASH IN BANK ACCT DEFAULT  */~
            defdiscacct$9,               /* DISCOUNT ACCOUNT DEFAULT   */~
            delqdate$10,                 /* DELINQUENT CUT-OFF DATE    */~
            discacct$9,                  /* DISCOUNT ACCOUNT NUMBER    */~
            discdate$6,                  /* DISCOUNT DUE DATE FROM INV */~
            diskkey$50,                  /* KEY FOR LOADING DATA       */~
            errormsg$79,                 /* ERROR MESSAGE FOR DATA TEST*/~
            fac$(10,10)1,                /* FIELD ATTRIBUTE CHARS      */~
            fastpay$1,                   /* FAST PAY FLAG              */~
            firstven$9,                  /* FIRST VENDOR CODE          */~
            firsttype$4,                 /* FIRST VENDOR TYPE          */~
            findkey$20,                  /* SEARCH KEY FOR INVOICES    */~
            header$79,                   /* Screen Title               */~
            hold$1,                      /* INVOICE HOLD FLAG          */~
            i$(24)80,                    /* JUNK SCREEN IMAGE          */~
            index%(1),                   /* Search Receiver            */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            invoicedate$8,               /* DATE OF INVOICE WE PAY     */~
            invoicenr$16,                /* INVOICE NUMBER             */~
            instr$17,                    /* PF KEY MESSAGE             */~
            key$11,                      /* KEY TO BUFFER FILE         */~
            lastven$9,                   /* LAST VENDOR CODE IN RANGE  */~
            lasttype$4,                  /* LAST VENDOR TYPE IN RANGE  */~
            linfac$(20)1,                /* LINEAR INPUT FAC'S         */~
            nextdate$10,                 /* DATE OF NEXT CHKS          */~
            nextinvoicekey$50,           /* NEXT INVOICE THIS VENDOR   */~
            nonapacct$(50)9,             /* Valid Non-AP Liability Acct*/~
            payacct$9,                   /* PAYABLES ACCOUNT THIS INV  */~
            payaccttype$1,               /* PAYABLES ACCOUNT TYPE      */~
            paydate$6,                   /* CURRENT PAYABLES DATE      */~
            plowkey$50,                  /* PLOW VARIABLE              */~
            regulardate$6,               /* PAY W/O DISCOUNT DATE      */~
            reversekey$11,               /* KEY$ X-OR HEX(FF)'S FOR FUN*/~
            seqnr$3,                     /* SEQUENCE NUMBER FOR LINES  */~
            temp$50,                     /* JUNK STRING, USED ALL OVER */~
            ten99$4,                     /* 1099 CATEGORY              */~
            userid$3,                    /* USER ID THIS USER          */~
            vencode$9,                   /* VENDOR CODE THIS VENDOR    */~
            ventype$4,                   /* VENDOR TYPE                */~
            vendors$(6,6)9               /* VENDOR CODES TABLE         */~

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
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 4 ! SYSFILE2 ! SYSTEM INFORMATION (MONTHS OPEN LIST)    *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN FILE                       *~
            * # 9 ! CHKBUFFR ! CASH DISBURSEMENTS CHECK GEN BUFFER      *~
            * #10 ! CHKBUF2  ! CASH DISBURSEMENTS CHECK GEN DETAIL BUFFR*~
            * #11 ! CSHBUF2  ! DISBURSEMENTS DETAIL BUFFER              *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #4, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos =   1, keylen = 20

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #9, "CHKBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 07,                         ~
                        alternate key 1, keypos =  08, keylen = 07,      ~
                                  key 2, keypos =  24, keylen =  8

            select #10, "CHKBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alt key 1, keypos = 21, keylen = 16, dup

            select #11, "CSHBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alt key 1, keypos = 21, keylen = 16, dup

            select #41,  "PAYLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 28,                      ~
                        alt key  1, keypos =   1, keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 200%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#41, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SETS PAYABLES DATE AND OTHER NECESSARY INFORMATION.       *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            REM GET PAYABLES DATE INFORMATION.
                call "EXTRACT" addr("ID", userid$)
                call "READ100" (#1, userid$, f1%(1))
                     if f1%(1) = 0 then L65000
                get #1, using L09140, checkdate$
L09140:                 FMT XX(9), CH(6)
                call "DATFMTC" (checkdate$)

                date$ = date
                call "DATEFMT" (date$)

            REM SET UP BLANKLINE$ TO HAVE "!"'S ON BOTTOM LINE OF SCREEN.
                blankline$ = "   !"
                copy str(blankline$, 4) to str(blankline$, 16)

            REM GET DEFAULT CASH/DISCOUNT ACCOUNTS FOR JUST IN CASE

            call "READ100" (#4, "MODULE.DEFAULTS.AP", f1%(4))
                if f1%(4) = 0% then L09350
            get #4, using L09330, defcashacct$, defdiscacct$
L09330:         FMT XX(20), XX(4), XX(4), XX(8), XX(27), 2*CH(9)

L09350:     init (" ")  nonapacct$()
            apkey$ = "APACCOUNTS-NON" & " "
            call "READ100" (#4, apkey$ , f1%(4%))
                if f1%(4%) = 0% then L10000
            get #4 using L09400, nonacctnum%, nonapacct$()
L09400:         FMT XX(20), BI(2), 50*CH(09)

L10000: REM *************************************************************~
            *      I N P U T   L I N E A R   I N F O R M A T I O N      *~
            *                                                           *~
            * INPUTS LINEAR INFORMATION.                                *~
            *************************************************************

        inputmode
            init(" ") errormsg$, infomsg$, vendors$(), checknr$,         ~
                      fastpay$, paydate$, nextdate$, delqdate$, date$(), ~
                      firstven$, lastven$, firsttype$, lasttype$
            vend% = 0%
            instr$ = "(16)Exit Program"

            for fieldnr% = 1 to 6
                gosub'160(fieldnr%)
                      if enabled% = 0 then L10250
L10190:         gosub'200(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% < 3 then L65000
                      if keyhit% <>  0 then       L10190
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10190
L10250:         next fieldnr%

        REM *************************************************************~
            *     I N P U T   T A B U L A R   I N F O R M A T I O N     *~
            *                                                           *~
            * INPUT TABULAR ITEMS HERE.                                 *~
            *     IF THE RANGE SPECIFIED IS "ALL", THEN WE DON'T NEED   *~
            * TO INPUT INDIVIDUAL VENDOR CODES.                         *~
            *************************************************************

            if firstven$ = "ALL" and firsttype$ = "ALL" then L12000

            for screenline% = 1 to 6
L11140:         for fieldnr% = 1 to 6
L11150:             gosub'201(screenline%, fieldnr%)
                          if keyhit%  =  1 then gosub startover
                          if keyhit%  =  2 then       columnone
                          if keyhit%  = 16 then       editprep
                          if keyhit% <>  0 then       L11150
                    gosub'151(screenline%, fieldnr%)
                          if errormsg$ <> " " then L11150
                    next fieldnr%
                next screenline%

        editprep: REM PREPARE TO ENTER EDIT MODE.  ONLY USED ONCE.
            errormsg$, infomsg$ = " "
            vendors$(screenline%, fieldnr%) = " "
            goto L12000

        columnone: REM COLUMN ONE FUNCTION CODE
            init(" ") str(vendors$(), (screenline%-1)*54+1),             ~
                      errormsg$, infomsg$
            goto L11140

L12000: REM *************************************************************~
            *                    E D I T   M O D E                      *~
            *                                                           *~
            * EDITS SCREEN INFORMATION.  CHECKS FOR LINEAR OR           *~
            *   TABULAR EDIT REQUIREMENTS                               *~
            *************************************************************

L12120:     REM DISPLAY SCREEN AND IDENTIFY FIELD TO EDIT
                gosub'210(0%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L20000
                      if keyhit% <>  0 then       L12120
                if cursor%(1) >= 15 then L12300

            REM LINEAR EDIT SECTION
                infomsg$ = " "
                fieldnr% = cursor%(1) - 5
                if fieldnr% < 1 or fieldnr% > 6 then L12120
L12230:         gosub'210(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L12230
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L12230
                goto L12120

L12300:     REM TABULAR EDIT SECTION
                if firstven$ = "ALL" and firsttype$ = "ALL" then L12120
                   screenline% = cursor%(1) - 14
                if screenline% < 1 or screenline% > 6 then L12120
                   fieldnr% = (cursor%(2)-4)/12 + 1
                if fieldnr% < 1 or fieldnr% > 6 then L12120

L12370:         gosub'211(screenline%, fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then L12370
                gosub'151(screenline%, fieldnr%)
                      if errormsg$ <> " " then L12370
                goto L12120

        REM *************************************************************~
            *    E N A B L E   L I N E A R   I N P U T   F I E L D S    *~
            *                                                           *~
            * ENABLES LINEAR INPUT FIELDS.  WE NEED TO CHECK HERE FOR   *~
            * APPROPRIATE DEFAULT INFORMATION, AND GET IT FROM DISK WHEN*~
            * NEEDED.                                                   *~
            *************************************************************

            deffn'160(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L13100,         /* CHECK DATE       */~
                                    L13200,         /* DELINQUENT DATE  */~
                                    L13300,         /* NEXT CHECK CUTOFF*/~
                                    L13400,         /* FAST PAY QUESTION*/~
                                    L13500,         /* VENDOR RANGE     */~
                                    L13600          /* TYPE RANGE       */
                  return

L13100:     REM ENABLE CHECK DATE USED FOR THIS BATCH ONLY.
                enabled% = 1
                return
L13200:     REM ENABLE DELINQUENT CUT-OFF DATE
                delqdate$ = checkdate$
                enabled% = 1
                return
L13300:     REM ENABLE DATE OF NEXT CHECKS...
                enabled% = 1
                return
L13400:     REM ENABLE FAST PAY QUESTION
                fastpay$ = "Y"
                enabled% = 1
                return
L13500:     REM ENABLE VENDOR RANGE
                enabled% = 1
                firstven$ = "ALL"
                return
L13600:     REM ENABLE TYPE RANGE
                enabled% = 1
                firsttype$ = "ALL"
                return

L20000: REM *************************************************************~
            *               G E N E R A T E   C H E C K S               *~
            *************************************************************

            call "SHOSTAT" ("Automatic Check Generation In Progress")

            call "DATUFMTC" (checkdate$)
            call "DATUFMTC" (delqdate$)
            call "DATUFMTC" (nextdate$)

            REM FIRST, NORMALIZE KEYS FOR RANGE PRINTING.
                if firstven$ <> "ALL" then L20180
                   init(hex(00)) firstven$
                   init(hex(ff)) lastven$
                   goto L20240
L20180:      REM HANDLES CASE FOR SINGLE VENDORS
                 if lastven$ <> " " then L20210
                    lastven$ = firstven$
L20210:      REM HANDLES CASE FOR A RANGE OF VENDORS
                    firstven$ = firstven$ addc all(hex(ff))

L20240:     REM PLOW ROUTINE FOR RANGE
                plowkey$ = firstven$
L20260:         call "PLOWNEXT" (#3, plowkey$, 0%, f1%(9))
                     if f1%(9) = 0 then L20330
                thiscode$ = plowkey$
                if thiscode$ > lastven$ then L20330
                if firsttype$ = "ALL" then L20322
                   get #3 using L20305, ventype$
L20305:                FMT XX(476), CH(4)
                   if ventype$ < firsttype$ then L20260
                   if ventype$ > lasttype$  then L20260

L20322:            gosub'250(thiscode$)
                   goto L20260

L20330:     REM PLOW ROUTINE FOR SINGLE VENDORS.
L20340:             vend% = vend% + 1
                    if vend% > 36 then L65000
                    screenline% =  int((vend%-1)/6) + 1
                    fieldnr% = mod(vend%-1,6) + 1
                    thiscode$ = vendors$(screenline%, fieldnr%)
                    if thiscode$ = " " then L20340

                    gosub'250(thiscode$)
                    go to L20340

        REM *************************************************************~
            *   G E N E R A T E   C H E C K   F O R   A   V E N D O R   *~
            *                                                           *~
            * GENERATES CHECK FOR A SINGLE VENDOR, THEN STORES IT IN A  *~
            *  BUFFER.                                                  *~
            *                                                           *~
            * LOGIC FLOW...                                             *~
            * 1.)  LOAD NEXT INVOICE ON FILE THIS VENDOR...             *~
            *      IF NO FURTHER INVOICES THIS VDR, GO HANDLE CHECKS.   *~
            *      IF NO CHECK TO BE DONE, EXIT ROUTINE.                *~
            *                                                           *~
            * 2.)  IF PAYABLES ACCOUNT TYPE IS NOT "LIABILITY", THEN    *~
            *         GOTO (1)                                          *~
            *      IF REGULAR PAYMENT DATE > DATE OF NEXT CHECKS THEN   *~
            *         GOTO (3), AS WE MAY WANT TO FAST PAY IT STILL.    *~
            *      OTHERWISE...                                         *~
            *      GOTO (4) (PAY THE INVOICE)                           *~
            *                                                           *~
            * 3.)  IF FAST PAYMENT IS NOT AUTHORIZED, GOTO (1)          *~
            *      IF DISCOUNT DUE DATE = BLANKDATE THEN GO TO (1)      *~
            *      IF DISCOUNTS DUE DATE > NEXT DUE DATE THEN GOTO (1)  *~
            *      IF DISCOUNTS DUE DATE < PAYABLES DATE THEN GOTO (1)  *~
            *      GOTO (4) (PAY INVOICE)                               *~
            *                                                           *~
            * 4.)  IF DISCOUNTS DUE DATE < TODAY'S PAYABLES DATE        *~
            *          THEN GOTO (5).                                   *~
            *      IF OUTSTANDING AMOUNT OF INVOICE <0 THEN GOTO (5)    *~
            *      IF FAST PAYMENT NOT AUTHORIZED THEN DISCOUNT = 0,ELSE*~
            *      DISCOUNT = DISCOUNT % *(AMOUNT - NONDISCOUNTABLE AMT)*~
            *                                                           *~
            * 5.)  IF DISCOUNT TEST (ABOVE, 4) FAILS, THEN WE JUST PAY  *~
            *      THE INVOICE OFF (DISCOUNT = 0).                      *~
            *                                                           *~
            * 6.)  SAVE THE LINE ITEM DIRECT ONTO THE DISK.             *~
            *************************************************************

            deffn'250(vencode$)

                REM SET KEYS FOR PLOW ROUTINE
                    call "READ100" (#3, vencode$, f1%(3))
                         if f1%(3) = 0 then return
                    nextinvoicekey$ = vencode$

L21460:         REM INITIALIZATION PART OF ROUTINE--SET NEXT CHECK NUMBER
                    init (" ") temp$,checknr$,key$
                    disctotal, checktotal = 0
                    seqnr% = 0

L21510:         REM PLOW INVOICES.
                    call "PLOWNEXT" (#5, nextinvoicekey$, 9%, f1%(5))
                         if f1%(5) = 0 then L22130

                    get #5, using L21640, invoicenr$, invoicedate$,       ~
                         payacct$, payaccttype$, regulardate$, discdate$,~
                         nondisc, totalamnt, balance, discamnt, ten99$,  ~
                         hold$

                    if hold$ = "Y" then L21510
                    currkey$ = nextinvoicekey$
                    call "PLOWNEXT" (#41, currkey$, 25%, f1%(41))
                       if f1%(41) <> 0% then L21510
                    nondisc = round (nondisc,2%)
                    balance = round (balance,2%)
                    accttyp$ = payaccttype$
                    gosub ap_account_check

L21640:                     FMT XX(9),   /* VENDOR CODE                */~
                                CH(16),  /* INVOICE NUMBER             */~
                                XX(16),  /* PO NUMBER                  */~
                                CH(6),   /* DATE OF INVOICE            */~
                                XX(9),   /* PURCHASES ACCOUNT NUMBER   */~
                                CH(9),   /* PAYABLES ACCOUNT NUMBER    */~
                                CH(1),   /* PAYABLES ACCOUNT TYPE      */~
                                2*CH(6), /* REGULAR, DISCOUNT DATES    */~
                                PD(14,4),/* NON-DISCOUNTABLE AMOUNT    */~
                                XX(24),  /* DATES                      */~
                                PD(14,4),/* TOTAL INVOICE AMOUNT       */~
                                PD(14,4),/* OUTSTANDING AMOUNT THIS INV*/~
                                XX(28),  /* FREE TEXT AND DISCOUNT %   */~
                                PD(14,4),/* DISCOUNT                   */~
                                CH(4),   /* 1099 CATEGORY              */~
                                CH(1)    /* HOLD FLAG                  */

*                  IF PAYACCTTYPE$ <> "L" THEN 21510
                    if accttyp$ <> "L" then L21510
                    if abs(balance)  < .01 then L21510
                    gosub L52000
                    if payok%<>0 then L21510
                    if regulardate$ > delqdate$ then L21890
                       goto L21960

L21890:         REM CHECK FOR DISCOUNT APPLICABLE
                    if fastpay$ <> "Y" then L21510
                    if discdate$ = blankdate$ then L21510
                    if discdate$ < checkdate$ then L21510
                    if discdate$ < nextdate$  then L21960
                       goto L21510

L21960:     REM GENERATE LINE ITEM FOR THIS INVOICE & TAKE DISCOUNT
                    discount = 0
                    if discdate$ = blankdate$         then L22050
                    if discdate$ < checkdate$         then L22050
                    if balance <= 0                   then L22050
                    if abs(balance - totalamnt) > .01 then L22050
                    discount = discamnt

L22050:         REM PAY OFF INVOICE, WRITE THIS DETAIL TO THE BUFFER FILE.
                    gosub L30000
                    checktotal = round(checktotal + balance - discount, 2)
                    disctotal  = round(disctotal + discount, 2)
                    if seqnr% < 100% then L21510  /* CONTINUE           */
                       gosub L22130               /* CLOSE THIS CHECK   */
                       goto  L21460               /* RESUME THIS VENDOR */

L22130:     REM THIS ROUTINE FORMATS ALL THE INFORMATION IN CHECK HEADER.
                if seqnr% = 0 then L22190
                if checktotal <= 0 then L22190
                gosub L31250
                return

L22190:     REM THIS ROUTINE DELETES THE CHECK IF TOTAL IS < 0
                diskkey$ = vencode$
                str(diskkey$, 10) = checknr$
                call "DELETE" (#10, diskkey$, 17%)
                if key$=" " then return
                call "READ101" (#9, key$, f1%(9))
                if f1%(9)<>0 then delete #9
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *  W R I T E   C H E C K   D E T A I L   T O   B U F F E R  *~
            *                                                           *~
            * WRITES CHECK DETAIL TO THE BUFFER.  INCREMENTS SEQNR% SO  *~
            * CALLING ROUTINE DOESN'T HAVE TO CHECK.                    *~
            *************************************************************

            if seqnr%=0 then gosub L31000
            seqnr% = seqnr% + 1
            convert seqnr% to seqnr$, pic(###)

            write #10, using L30900,                                      ~
                       vencode$, checknr$, seqnr$, invoicenr$,           ~
                       payacct$, payaccttype$, balance,                  ~
                       discount, invoicedate$, ten99$, temp$
            return

L30900:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(16),                  /* INVOICE PAYING OFF NUMBER  */~
                CH(9),                   /* ACCOUNT NUMBER TO DEBIT    */~
                CH(1),                   /* DEBIT ACCOUNT TYPE         */~
                PD(14,4),                /* DEBIT AMOUNT               */~
                PD(14,4),                /* DISCOUNT THIS DETAIL #     */~
                CH(6),                   /* INVOICE DATE               */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(28)                   /* FILLER                     */

L31000: REM *************************************************************~
            *         W R I T E   C H E C K   T O   B U F F E R         *~
            *                                                           *~
            * NOW THAT WE HAVE THE CHECK, WRITE IT TO BUFFER IN STD.    *~
            * FASHION.  NOTE THAT THE KEY IS NOT THE STANDARD ARBITRARY *~
            * SEQUENCE NUMBER, RATHER, IT'S THE CHECK NUMBER, SO THAT WE*~
            * CAN RECALL THE CHECKS IN ASCENDING ORDER.                 *~
            *************************************************************

            REM DECODE DATE, RIGHT-JUSTIFY CHECK NUMBER

                get #3, using L31104, cashacct$, discacct$
L31104:                 FMT XX(267), 2*CH(9)

                if cashacct$ = " " then cashacct$ = defcashacct$
                if discacct$ = " " then discacct$ = defdiscacct$

            REM SET UP KEY INFORMATION
                call "FMTKEYCK" (#9,key$,reversekey$)

            str(checknr$,1,4)="X000"
            str(checknr$,5,4)=str(key$,4,4)

            write  #9, using L31310,                                      ~
                       key$, reversekey$, vencode$, checknr$,            ~
                       checkdate$, disctotal, discacct$, cashacct$,      ~
                       checktotal, temp$
            return

            REM REREAD AND INSERT $ AMOUNTS
L31250:     call "READ101" (#9, key$, f1%(9))
            if f1%(9)=0 then return
            temp1,temp2=0
            get  #9, using L31310,                                        ~
                       key$, reversekey$, vencode$, checknr$,            ~
                       checkdate$, temp1, discacct$, cashacct$,          ~
                       temp2, temp$
            rewrite  #9, using L31310,                                    ~
                       key$, reversekey$, vencode$, checknr$,            ~
                       checkdate$, disctotal, discacct$, cashacct$,      ~
                       checktotal, temp$
            return
L31310:                     FMT 2*CH(7),           /* KEY + REVERSE KEY*/~
                                CH(9),             /* VENDOR CODE      */~
                                CH(8),             /* CHECK NUMBER     */~
                                CH(6),             /* CHECK DATE       */~
                                PD(14,4),          /* DISCOUNT AMOUNT  */~
                                2*CH(9),           /* DISCOUNT,CASH ACC*/~
                                PD(14,4),          /* TOTAL CHECK AMT  */~
                                CH(29)             /* FILLER           */

        ap_account_check
            if accttyp$       <> "L" then return
            if payacct$        = " " then return
            if nonapacct$(1%)  = " " then return
            search str(nonapacct$(), 1%, 9% * nonacctnum%)               ~
                                   = str(payacct$) to index%() step 9%
                if index%(1%)  =  0% then return
            accttyp$ = "N"
            return

        REM *************************************************************~
            *             I N P U T   I N F O R M A T I O N             *~
            *                                                           *~
            * GETS INFORMATION IN BOTH TABULAR AND LINEAR INPUT MODES.  *~
            * NOTE THAT THE SCREEN ITSELF IS CALLED BY BOTH PARTS, BUT  *~
            * THE FAC-SETTING ROUTINES ARE (UNDERSTANDABLY) DIFFERENT.  *~
            *************************************************************

            deffn'201(screenline%, fieldnr%)
                  init(hex(84)) fac$(), linfac$()
                  fac$(screenline%, fieldnr%) = hex(81)
                  instr$ = "(16)Edit Mode"
                  goto L40130

            deffn'200(fieldnr%)                    /* LINEAR INPUT     */
                  init(hex(84)) linfac$(), fac$()
                  if fieldnr% > 1 then instr$ = " "

                  on fieldnr% gosub L40165,         /* CHECK DATE       */~
                                    L40165,         /* DELQ CUTOFF DATE */~
                                    L40165,         /* NEXT CHECKS DATE */~
                                    L40165,         /* FAST PAY? THING  */~
                                    L40165,         /* VENDOR RANGE     */~
                                    L40165          /* TYPE RANGE       */
L40130:           header$ = " "
                  str(header$,63) = "CHKCREAT:" & cms2v$
                  goto L40200

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linfac$(fieldnr%) = hex(80)
                      return
L40165:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linfac$(fieldnr%) = hex(82)
                      return

L40200:     accept                                                       ~
               at (01,02), "Automatic Vendor Check Generation",          ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
               at (06,02),                                               ~
                  "CHECK DATE FOR THIS RUN",                             ~
               at (06,50), fac(linfac$( 1)), checkdate$         , ch(10),~
               at (07,02),                                               ~
                  "INVOICE PAY CUT-OFF DATE",                            ~
               at (07,50), fac(linfac$( 2)), delqdate$          , ch(10),~
               at (08,02),                                               ~
                  "WHEN WILL YOU NEXT CREATE AND PRINT CHECKS?",         ~
               at (08,50), fac(linfac$( 3)), nextdate$          , ch(10),~
               at (09,02),                                               ~
                  "DO YOU WANT TO FAST PAY TO GET DISCOUNTS (Y/N)",      ~
               at (09,50), fac(linfac$( 4)), fastpay$           , ch(01),~
               at (10,02),                                               ~
                  "VENDOR CODE RANGE",                                   ~
               at (10,50), fac(linfac$( 5)), firstven$          , ch(09),~
               at (10,60), "TO",                                         ~
               at (10,63), fac(linfac$( 5)), lastven$           , ch(09),~
               at (11,02),                                               ~
                  "VENDOR TYPE RANGE",                                   ~
               at (11,50), fac(linfac$( 6)), firsttype$         , ch(04),~
               at (11,60), "TO",                                         ~
               at (11,63), fac(linfac$( 6)), lasttype$          , ch(04),~
               at (13,26),                                               ~
                  "SPECIFIC VENDOR CODES TO BE PAID",                    ~
               at (14,05),                                               ~
        "+-----------+-----------+-----------+-----------+-----------+---~
        ~--------+",                                                      ~
                                                                         ~
               at (15,05), "!",                                          ~
               at (15,07), fac(fac$(1,1)), vendors$(1,1)        , ch(09),~
               at (15,17), "!",                                          ~
               at (15,19), fac(fac$(1,2)), vendors$(1,2)        , ch(09),~
               at (15,29), "!",                                          ~
               at (15,31), fac(fac$(1,3)), vendors$(1,3)        , ch(09),~
               at (15,41), "!",                                          ~
               at (15,43), fac(fac$(1,4)), vendors$(1,4)        , ch(09),~
               at (15,53), "!",                                          ~
               at (15,55), fac(fac$(1,5)), vendors$(1,5)        , ch(09),~
               at (15,65), "!",                                          ~
               at (15,67), fac(fac$(1,6)), vendors$(1,6)        , ch(09),~
               at (15,77), "!",                                          ~
                                                                         ~
               at (16,05), "!",                                          ~
               at (16,07), fac(fac$(2,1)), vendors$(2,1)        , ch(09),~
               at (16,17), "!",                                          ~
               at (16,19), fac(fac$(2,2)), vendors$(2,2)        , ch(09),~
               at (16,29), "!",                                          ~
               at (16,31), fac(fac$(2,3)), vendors$(2,3)        , ch(09),~
               at (16,41), "!",                                          ~
               at (16,43), fac(fac$(2,4)), vendors$(2,4)        , ch(09),~
               at (16,53), "!",                                          ~
               at (16,55), fac(fac$(2,5)), vendors$(2,5)        , ch(09),~
               at (16,65), "!",                                          ~
               at (16,67), fac(fac$(2,6)), vendors$(2,6)        , ch(09),~
               at (16,77), "!",                                          ~
                                                                         ~
               at (17,05), "!",                                          ~
               at (17,07), fac(fac$(3,1)), vendors$(3,1)        , ch(09),~
               at (17,17), "!",                                          ~
               at (17,19), fac(fac$(3,2)), vendors$(3,2)        , ch(09),~
               at (17,29), "!",                                          ~
               at (17,31), fac(fac$(3,3)), vendors$(3,3)        , ch(09),~
               at (17,41), "!",                                          ~
               at (17,43), fac(fac$(3,4)), vendors$(3,4)        , ch(09),~
               at (17,53), "!",                                          ~
               at (17,55), fac(fac$(3,5)), vendors$(3,5)        , ch(09),~
               at (17,65), "!",                                          ~
               at (17,67), fac(fac$(3,6)), vendors$(3,6)        , ch(09),~
               at (17,77), "!",                                          ~
                                                                         ~
               at (18,05), "!",                                          ~
               at (18,07), fac(fac$(4,1)), vendors$(4,1)        , ch(09),~
               at (18,17), "!",                                          ~
               at (18,19), fac(fac$(4,2)), vendors$(4,2)        , ch(09),~
               at (18,29), "!",                                          ~
               at (18,31), fac(fac$(4,3)), vendors$(4,3)        , ch(09),~
               at (18,41), "!",                                          ~
               at (18,43), fac(fac$(4,4)), vendors$(4,4)        , ch(09),~
               at (18,53), "!",                                          ~
               at (18,55), fac(fac$(4,5)), vendors$(4,5)        , ch(09),~
               at (18,65), "!",                                          ~
               at (18,67), fac(fac$(4,6)), vendors$(4,6)        , ch(09),~
               at (18,77), "!",                                          ~
                                                                         ~
               at (19,05), "!",                                          ~
               at (19,07), fac(fac$(5,1)), vendors$(5,1)        , ch(09),~
               at (19,17), "!",                                          ~
               at (19,19), fac(fac$(5,2)), vendors$(5,2)        , ch(09),~
               at (19,29), "!",                                          ~
               at (19,31), fac(fac$(5,3)), vendors$(5,3)        , ch(09),~
               at (19,41), "!",                                          ~
               at (19,43), fac(fac$(5,4)), vendors$(5,4)        , ch(09),~
               at (19,53), "!",                                          ~
               at (19,55), fac(fac$(5,5)), vendors$(5,5)        , ch(09),~
               at (19,65), "!",                                          ~
               at (19,67), fac(fac$(5,6)), vendors$(5,6)        , ch(09),~
               at (19,77), "!",                                          ~
                                                                         ~
               at (20,05), "!",                                          ~
               at (20,07), fac(fac$(6,1)), vendors$(6,1)        , ch(09),~
               at (20,17), "!",                                          ~
               at (20,19), fac(fac$(6,2)), vendors$(6,2)        , ch(09),~
               at (20,29), "!",                                          ~
               at (20,31), fac(fac$(6,3)), vendors$(6,3)        , ch(09),~
               at (20,41), "!",                                          ~
               at (20,43), fac(fac$(6,4)), vendors$(6,4)        , ch(09),~
               at (20,53), "!",                                          ~
               at (20,55), fac(fac$(6,5)), vendors$(6,5)        , ch(09),~
               at (20,65), "!",                                          ~
               at (20,67), fac(fac$(6,6)), vendors$(6,6)        , ch(09),~
               at (20,77), "!",                                          ~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,64),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,64),                                               ~
                  "(15)Print Screen",                                    ~
               at (23,02),                                               ~
                  "(2)Column One",                                       ~
               at (24,64), fac(hex(84)), instr$                 , ch(16),~
                                                                         ~
               keys(hex(0001020d0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L40885
                call "MANUAL" ("CHKCREAT")
                goto L40200

L40885:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40200

        REM *************************************************************~
            *              E D I T   M O D E   S C R E E N              *~
            *************************************************************

            deffn'211(screenline%, fieldnr%)
                  init(hex(84)) fac$(), linfac$()
                  fac$(screenline%, fieldnr%) = hex(81)
                  goto L41100

            deffn'210(fieldnr%)                    /* LINEAR INPUT     */
                  init(hex(84)) linfac$(), fac$()
                  if fieldnr% = 0 then init(hex(86)) fac$()
                  on fieldnr% gosub L41135,         /* CHECK DATE       */~
                                    L41135,         /* DELQ CUTOFF DATE */~
                                    L41135,         /* NEXT CHECKS DATE */~
                                    L41135,         /* FAST PAY? THING  */~
                                    L41135,         /* FIRST VENDOR CODE*/~
                                    L41135          /* LAST VENDOR CODE */
L41100:           header$ = "Edit Selection Data"
                  str(header$,63) = "CHKCREAT:" & cms2v$
                  instr$ = "(16)Create Checks"
                  goto L41170

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      linfac$(fieldnr%) = hex(80)
                      return
L41135:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      linfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linfac$(fieldnr%) = hex(82)
                      return

L41170:     accept                                                       ~
               at (01,02), "Automatic Vendor Check Generation",          ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
               at (06,02),                                               ~
                  "CHECK DATE FOR THIS RUN",                             ~
               at (06,50), fac(linfac$( 1)), checkdate$         , ch(10),~
               at (07,02),                                               ~
                  "INVOICE PAY CUT-OFF DATE",                            ~
               at (07,50), fac(linfac$( 2)), delqdate$          , ch(10),~
               at (08,02),                                               ~
                  "WHEN WILL YOU NEXT CREATE AND PRINT CHECKS?",         ~
               at (08,50), fac(linfac$( 3)), nextdate$          , ch(10),~
               at (09,02),                                               ~
                  "DO YOU WANT TO FAST PAY TO GET DISCOUNTS (Y/N)",      ~
               at (09,50), fac(linfac$( 4)), fastpay$           , ch(01),~
               at (10,02),                                               ~
                  "VENDOR CODE RANGE",                                   ~
               at (10,50), fac(linfac$( 5)), firstven$          , ch(09),~
               at (10,60), "TO",                                         ~
               at (10,63), fac(linfac$( 5)), lastven$           , ch(09),~
               at (11,02),                                               ~
                  "VENDOR TYPE RANGE",                                   ~
               at (11,50), fac(linfac$( 6)), firsttype$         , ch(04),~
               at (11,60), "TO",                                         ~
               at (11,63), fac(linfac$( 6)), lasttype$          , ch(04),~
               at (13,26),                                               ~
                  "SPECIFIC VENDOR CODES TO BE PAID",                    ~
               at (14,05),                                               ~
        "+-----------+-----------+-----------+-----------+-----------+---~
        ~--------+",                                                      ~
                                                                         ~
               at (15,05), "!",                                          ~
               at (15,07), fac(fac$(1,1)), vendors$(1,1)        , ch(09),~
               at (15,17), "!",                                          ~
               at (15,19), fac(fac$(1,2)), vendors$(1,2)        , ch(09),~
               at (15,29), "!",                                          ~
               at (15,31), fac(fac$(1,3)), vendors$(1,3)        , ch(09),~
               at (15,41), "!",                                          ~
               at (15,43), fac(fac$(1,4)), vendors$(1,4)        , ch(09),~
               at (15,53), "!",                                          ~
               at (15,55), fac(fac$(1,5)), vendors$(1,5)        , ch(09),~
               at (15,65), "!",                                          ~
               at (15,67), fac(fac$(1,6)), vendors$(1,6)        , ch(09),~
               at (15,77), "!",                                          ~
                                                                         ~
               at (16,05), "!",                                          ~
               at (16,07), fac(fac$(2,1)), vendors$(2,1)        , ch(09),~
               at (16,17), "!",                                          ~
               at (16,19), fac(fac$(2,2)), vendors$(2,2)        , ch(09),~
               at (16,29), "!",                                          ~
               at (16,31), fac(fac$(2,3)), vendors$(2,3)        , ch(09),~
               at (16,41), "!",                                          ~
               at (16,43), fac(fac$(2,4)), vendors$(2,4)        , ch(09),~
               at (16,53), "!",                                          ~
               at (16,55), fac(fac$(2,5)), vendors$(2,5)        , ch(09),~
               at (16,65), "!",                                          ~
               at (16,67), fac(fac$(2,6)), vendors$(2,6)        , ch(09),~
               at (16,77), "!",                                          ~
                                                                         ~
               at (17,05), "!",                                          ~
               at (17,07), fac(fac$(3,1)), vendors$(3,1)        , ch(09),~
               at (17,17), "!",                                          ~
               at (17,19), fac(fac$(3,2)), vendors$(3,2)        , ch(09),~
               at (17,29), "!",                                          ~
               at (17,31), fac(fac$(3,3)), vendors$(3,3)        , ch(09),~
               at (17,41), "!",                                          ~
               at (17,43), fac(fac$(3,4)), vendors$(3,4)        , ch(09),~
               at (17,53), "!",                                          ~
               at (17,55), fac(fac$(3,5)), vendors$(3,5)        , ch(09),~
               at (17,65), "!",                                          ~
               at (17,67), fac(fac$(3,6)), vendors$(3,6)        , ch(09),~
               at (17,77), "!",                                          ~
                                                                         ~
               at (18,05), "!",                                          ~
               at (18,07), fac(fac$(4,1)), vendors$(4,1)        , ch(09),~
               at (18,17), "!",                                          ~
               at (18,19), fac(fac$(4,2)), vendors$(4,2)        , ch(09),~
               at (18,29), "!",                                          ~
               at (18,31), fac(fac$(4,3)), vendors$(4,3)        , ch(09),~
               at (18,41), "!",                                          ~
               at (18,43), fac(fac$(4,4)), vendors$(4,4)        , ch(09),~
               at (18,53), "!",                                          ~
               at (18,55), fac(fac$(4,5)), vendors$(4,5)        , ch(09),~
               at (18,65), "!",                                          ~
               at (18,67), fac(fac$(4,6)), vendors$(4,6)        , ch(09),~
               at (18,77), "!",                                          ~
                                                                         ~
               at (19,05), "!",                                          ~
               at (19,07), fac(fac$(5,1)), vendors$(5,1)        , ch(09),~
               at (19,17), "!",                                          ~
               at (19,19), fac(fac$(5,2)), vendors$(5,2)        , ch(09),~
               at (19,29), "!",                                          ~
               at (19,31), fac(fac$(5,3)), vendors$(5,3)        , ch(09),~
               at (19,41), "!",                                          ~
               at (19,43), fac(fac$(5,4)), vendors$(5,4)        , ch(09),~
               at (19,53), "!",                                          ~
               at (19,55), fac(fac$(5,5)), vendors$(5,5)        , ch(09),~
               at (19,65), "!",                                          ~
               at (19,67), fac(fac$(5,6)), vendors$(5,6)        , ch(09),~
               at (19,77), "!",                                          ~
                                                                         ~
               at (20,05), "!",                                          ~
               at (20,07), fac(fac$(6,1)), vendors$(6,1)        , ch(09),~
               at (20,17), "!",                                          ~
               at (20,19), fac(fac$(6,2)), vendors$(6,2)        , ch(09),~
               at (20,29), "!",                                          ~
               at (20,31), fac(fac$(6,3)), vendors$(6,3)        , ch(09),~
               at (20,41), "!",                                          ~
               at (20,43), fac(fac$(6,4)), vendors$(6,4)        , ch(09),~
               at (20,53), "!",                                          ~
               at (20,55), fac(fac$(6,5)), vendors$(6,5)        , ch(09),~
               at (20,65), "!",                                          ~
               at (20,67), fac(fac$(6,6)), vendors$(6,6)        , ch(09),~
               at (20,77), "!",                                          ~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,64),                                               ~
                  "(13)Instructions",                                    ~
               at (24,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,64),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,64), fac(hex(84)), instr$                 , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41850
                  call "MANUAL" ("CHKCREAT")
                  goto L41170

L41850:        if keyhit% <> 15 then L41870
                  call "PRNTSCRN"
                  goto L41170

L41870:        REM GET CURSOR LOCATION
                   close ws
                   call "SCREEN" addr("C", 0%, "I", i$(), cursor%(1))
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS ALL THE NUMBERS FOR VALIDITY, AND THE DATES FOR OK, *~
            * AND THAT THE KEYS IN THE RANGE OF VENDORS TO PRINT ARE OK.*~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% gosub L50100,         /* CHECK DATE       */~
                                    L50200,         /* DELQ CUTOFF DATE */~
                                    L50300,         /* NEXT CREATION DAT*/~
                                    L50400,         /* FAST PAY?        */~
                                    L50500,         /* VENDOR RANGE     */~
                                    L50600          /* TYPE RANGE       */
                  return

L50100:     REM TEST CHECK DATE
                call "DATEOKC" (checkdate$, date%, errormsg$)
                     if errormsg$ <> " " then return
                call "DATUFMTC" (checkdate$)
                date$(1) = checkdate$
                call "DATFMTC" (checkdate$)
                call "WHICHMON" (#4, date$(1), thismonth%)
                if thismonth% <> 0 then return
                errormsg$ = "Check Date Not In Currently Open Month: "   ~
                                         & checkdate$
                if date$(3) <> " " and date$(3) <> blankdate$ then L50330
                return

L50200:     REM TEST DELINQUENT CUT-OFF DATE
                call "DATEOKC" (delqdate$, date%, errormsg$)
                     if errormsg$ <> " " then return
                call "DATUFMTC" (delqdate$)
                date$(2) = delqdate$
                call "DATFMTC" (delqdate$)
                if date$(3) <> " " and date$(3) <> blankdate$ then L50355
                return

L50300:     REM TEST DATE WE'RE NEXT GOING TO CREATE CHECKS.
                call "DATEOKC" (nextdate$, date%, errormsg$)
                if errormsg$ <> " " then return
                   call "DATUFMTC" (nextdate$)
                   date$(3) = nextdate$
                   call "DATFMTC" (nextdate$)
L50330:         if date$(3) > date$(1) then L50355
                   errormsg$ = "Next Check Run Must Be After CHECK Date"
                   return
L50355:         if date$(3) > date$(2) then return
                   errormsg$ = "Next Check Run Must Be After CUT-OFF Date"
                   return

L50400:     REM TEST FAST PAY QUESTION ANSWER.
                if fastpay$ = "Y" then return
                if fastpay$ = "N" then return
                   errormsg$ = "MUST BE 'Y' OR 'N'"
                   return

L50500:     REM TEST VENDOR RANGE
                if firstven$ <> "ALL" then L50540
                   lastven$ = " "
                   return
L50540:         if lastven$ = " " then lastven$ = firstven$
                if firstven$ <= lastven$ then return
                errormsg$="Last Vendor Code Must Be Greater Than First."
                return

L50600:     REM TEST LAST VENDOR CODE.
                if firsttype$ <> "ALL" then L50640
                   lasttype$ = " "
                   return
L50640:         if lasttype$ = " " then lasttype$ = firsttype$
                if firsttype$ <= lasttype$ then return
                errormsg$="Last Vendor Type Must Be Greater Than First."
                return

        REM *************************************************************~
            *             T E S T   T A B U L A R   D A T A             *~
            *                                                           *~
            *************************************************************

            deffn'151(line%, field%)
                  errormsg$, infomsg$ = " "

            REM TEST VENDOR CODE TO MAKE SURE IT'S O.K. (BLANK = OK)
                if vendors$(line%, field%) = " " then return
                pos% = 6 * (line%-1) + field%

                REM FIRST,  SEE IF THE VENDOR IS ON FILE.
                    call "GETCODE" (#3, vendors$(line%, field%),         ~
                                         infomsg$, 1%, 1.3, f1%(3))
                    if f1%(3) = 1 then L51190
                       errormsg$ = "Vendor Code Not On File: "           ~
                                         & vendors$(line%, field%)
                       return

L51190:         REM SECOND, IF ON FILE, SEE IF ALREADY IN TABLE.
                    search str(vendors$(),1) = vendors$(line%, field%)   ~
                          to cursor%() step 9
                     if int((cursor%(1)+8%)/9%) <> pos% then L51310
                     if cursor%(2) = 0% then return
L51310:                   errormsg$ = "Vendor Already Entered: " &       ~
                                         vendors$(line%, field%)
                          return

L52000: REM *************************************************************~
            * TEST OK TO PAY THIS INVOICE. IT IS NOT OK IF THERE IS ANY *~
            * CHECK PENDING (IE IN CHKBUF2 OR CSHBUF2) ADDRESSING THIS  *~
            * INVOICE.                                                  *~
            *************************************************************

            payok%=1

            findkey$=invoicenr$
            call "REDALT0" (#10,findkey$,1%,f1%(10))
            goto L52105
L52100:          call "READNEXT" (#10,f1%(10))
L52105:     if f1%(10)=0 then L52200
            get #10, using L52120, tven$,tinv$
L52120:              FMT CH(09),XX(11),CH(16)
            if tinv$<>invoicenr$ then L52200
            if tven$<>vencode$ then L52100
            return

L52200:     call "REDALT0" (#11,findkey$,1%,f1%(11))
            goto L52235
L52230:          call "READNEXT" (#11,f1%(11))
L52235:     if f1%(11)=0 then L52300
            get #11, using L52250, tven$,tinv$
L52250:              FMT CH(09),XX(11),CH(16)
            if tinv$<>invoicenr$ then L52300
            if tven$<>vencode$ then L52230
            return

L52300:     payok%=0
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            * CLOSES ALL THE FILES CURRENTLY OPEN.                      *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
