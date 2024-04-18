        REM *************************************************************~
            *                                                           *~
            *   CCC    SSS   H   H  FFFFF  IIIII  L      EEEEE    1     *~
            *  C   C  S      H   H  F        I    L      E       11     *~
            *  C       SSS   HHHHH  FFFF     I    L      EEEE     1     *~
            *  C   C      S  H   H  F        I    L      E        1     *~
            *   CCC    SSS   H   H  F      IIIII  LLLLL  EEEEE  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CSHFILE1 - POSTS CHECKS TO CASH DISBURSEMENTS, DEDUCTING  *~
            *            THE AMOUNT OF THE CHECK FROM THE OUTSTANDING   *~
            *            BALANCE FOR BOTH THE VENDOR AND ANY OUTSTANDING*~
            *            INVOICES AND STUFF.                            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 04/20/80 ! ORIGINAL                                 ! BCW *~
            * 05/27/81 ! EXPANDED TO 16 CHAR INVOICE NUMBER       ! TOM *~
            * 06/07/81 ! REWRITE ENTIRE VENDOR MASTER RECORD      ! TEM *~
            * 06/20/81 ! UPDATE TO NEW STANDARDS-OPEN MASTER FILES! TEM *~
            * 07/23/81 ! ADD DISCOUNT TAKEN TO LINE ITEM          ! TEM *~
            * 10/06/81 ! ROUND ON RESAVE TO MAIN FILE             ! TEM *~
            * 08/30/82 ! ADDED RECONCILIATION FILEDS TO CSHMASTR  ! ECR *~
            * 12/16/85 ! Vendor file format changes               ! MJB *~
            * 01/12/86 ! 1099 POSTING                             ! KAB *~
            * 03/11/86 ! 1099 POSTING/LINE FREEZES                ! KAB *~
            * 05/15/86 ! PAYMASTR FILE FORMAT CHANGE              ! HES *~
            * 04/25/89 ! Changed to write year of check date      ! LAB *~
            *          ! instead of post date as first 2 bytes of !     *~
            *          ! CSH1099 file.                            !     *~
            * 08/22/89 ! Put STR() around SEARCH so as to find the! JDH *~
            *          !   correct invoice when first n characters!     *~
            *          !   are the same.    Thanks Kenny.         !     *~
            * 12/17/93 ! Added EOD branch on write to CSH1099.    ! JDH *~
            * 12/29/93 ! Added Code Check for Non-A/P G/L         ! JBK *~
            *          !   Liability Accounts and treat as Direct !     *~
            * 01/16/95 ! PRR 13343. Non-A/P Lia. Accts now to 50. ! JDH *~
            * 08/02/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            apkey$20,                    /* SYSFILE2 File Field        */~
            askhdr$40,                                                   ~
            askpf1$80,                                                   ~
            askmid$80,                                                   ~
            askpf2$80,                                                   ~
            acct$(100)9,                 /* CREDIT ACCOUNT NUMBERS     */~
            amount(100),                 /* AMOUNT OF THIS DETAIL.     */~
            cashacct$9,                  /* CASH IN BANK ACCOUNT       */~
            checknr$8,                   /* AND CURRENT CHECK NUMBER   */~
            cshlncbf$(100)100,           /* CURRENCY SHADOW - LINES    */~
            cshmscbf$100,                /* CURRENCY SHADOW - MASTER   */~
            datetime$7,                  /* Date/Time Stamp            */~
            gl_old(100),                 /* OLD GAIN-LOSS              */~
            gl_new(100),                 /* OLD GAIN-LOSS              */~
            dateposted$6,                /* MODULE DATE POSTED         */~
            datereconciled$6,            /* RECONCILIATION DATE        */~
            delta$(200)16,               /* STACK FOR CHANGES IN AMTS  */~
            delta(200),                  /* AND AMOUNTS EACH ITEM IN ST*/~
            deltaten99$(200)4,           /* AND 1099 CATEGORIES        */~
            deltaoldten99$(200)4,        /* AND 1099 CATEGORIES        */~
            disc(100),                   /* DISCOUNT THIS DETAIL       */~
            discountacct$9,              /* DISCOUNT ACCOUNT           */~
            diskjunkkey$50,              /* TEMP KEY FOR READING IN    */~
            index%(1),                   /* Search Receiver            */~
            invoice$(100)16,             /* INVOICE NUMBERS            */~
            invdate$(100)6,              /* DATE OF INV AS OF PAY      */~
            lastdate$6,                  /* DATE LAST MODIFYING        */~
            lastuserid$3,                /* USERID LAST MODIFYING      */~
            nextcheckkey$50,             /* PLOW VARIABLE              */~
            nonapacct$(50)9,             /* Valid Non-A/P Liab. Acct   */~
            oldacct$(100)9,              /* OLD CREDIT ACCOUNT LIST    */~
            oldamount(100),              /* OLD CHECK AMOUNTS          */~
            oldcashacct$9,               /* OLD CASH IN BANK ACCOUNT   */~
            olddateposted$6,             /* OLD DATE POSTED            */~
            olddiscountacct$9,           /* DISCOUNT ACCT OLD CHECK    */~
            oldinvoice$(100)16,          /* OLD INVOICE LIST           */~
            oldreadkey$50,               /* ANOTHER PLOW ROUTINE KEY.  */~
            oldtype$(100)1,              /* OLD ACCOUNT TYPE LIST      */~
            oldten99$(100)4,             /* OLD 1099 CATEGORY          */~
            origdate$6,                  /* DATE ORIGINALLY INPUT      */~
            origuserid$3,                /* ORIGINALLY INPUT BY (USERID*/~
            payacct$9,                   /* Payables Account           */~
            paymastr$(7)50,              /* ONE INVOICE HEADER REC.    */~
            paytype$1,                   /* Payables Account Type      */~
            reconciled$1,                /* CHECK RECONCILIATION FLAG  */~
            statusfill$(100)22,          /* LINE STATUS & FILLER       */~
            stkinvoice$16,               /* STACK INVOICE              */~
            stkten99$4,                  /* 1099 CATEGORY              */~
            stkoldten99$4,               /* 1099 CATEGORY              */~
            tdate$8,                     /* Temporary Date Variable    */~
            ten99$(100)4,                /* OLD 1099 CATEGORY          */~
            type$(100)1,                 /* ACCOUNT TYPE (A OR R)      */~
            vencode$9,                   /* VENDOR CODE                */~
            vendor$(10)60                /* VENDOR MASTER RECORD       */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            f2% = 1%

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN FILE                       *~
            * # 7 ! CSHMASTR ! CASH DISBURSEMENTS CHECK HEADER FILE     *~
            * # 8 ! CSHLINES ! CASH DISBURSEMENTS CHECK DETAIL FILE     *~
            * # 9 ! CSHBUFFR ! CASH DISBURSEMENTS BUFFER AREA           *~
            * #10 ! CSHBUF2  ! CASH DISBURSEMENTS CHECK DETAIL BUFFER   *~
            * #11 ! SYSFILE2 ! System Catch All File                    *~
            * #30 ! CSH1099  ! CASH DISBURSEMENTS 1099 DETAIL FILE      *~
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
                        alternate key 1, keypos = 41, keylen = 9, dup,   ~
                                         /* CASH IN BANK ACCOUNT       */~
                                  key 2, keypos = 50, keylen = 6, dup,   ~
                                         /* DATE POSTED                */~
                                  key 3, keypos = 10, keylen = 8, dup    ~
                                         /* CHECK NO. SHOULDN'T BE DUPS*/

            select  #8, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup
                                         /* INVOICE NUMBER             */

            select  #9, "CSHBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 7,                          ~
                        alt key 1, keypos = 8, keylen = 7,               ~
                            key 2, keypos = 15, keylen = 17,             ~
                            key 3, keypos = 24, keylen =  8, dup

            select #10, "CSHBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alt key 1, keypos = 21, keylen = 16, dup

            select #30, "CSH1099",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos = 7, keylen = 20,                         ~
                        alt key 1, keypos =  1, keylen = 26,             ~
                            key 2, keypos = 27, keylen = 32

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

            select #11, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1, keylen = 20

            call "SHOSTAT"  ("Opening Files, One Moment Please.")

            call "OPENCHCK" (# 1, f%, f2%, 0%, " ")
            call "OPENCHCK" (# 3, f%, f2%, 0%, " ")
            call "OPENCHCK" (# 5, f%, f2%, 0%, " ")
            call "OPENCHCK" (# 7, f%, f2%, 1%, " ")
                if f% < 1% then error_exit
            call "OPENCHCK" (# 8, f%, f2%, 1%, " ")
                if f% < 1% then error_exit
            call "OPENCHCK" (# 9, f%, f2%, 0%, " ")
            call "OPENCHCK" (#10, f%, f2%, 0%, " ")
            call "OPENCHCK" (#11, f%, f2%, 0%, " ")
            call "OPENCHCK" (#30, f%, f2%, 1%, " ")
                if f% < 1% then error_exit
            call "OPENCHCK" (#42, f%, f2%, 0%, " ")
               if f% > 0% then f1% = 100%
            call "OPENCHCK" (#43, f%, f2%, 0%, " ")
               if f% > 0% then f1% = 100%
            call "OPENCHCK" (#45, f%, f2%, f1%, " ")
            call "OPENCHCK" (#46, f%, f2%, f1%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES VARIABLES REQUIRED IN THE PROGRAM             *~
            *************************************************************

            call "SHOSTAT" ("Posting Checks to Cash Disbursements File")

            call "EXTRACT" addr ("ID", userid$)

            call "READ100" (#1, userid$, f1%(1))
                 if f1%(1) = 0% then error_exit
             get #1, using L09160, paydate$
L09160:          FMT XX(9), CH(6)        /* SKIP USERID, GET DATE      */

            reconciled$ = "N"
            datereconciled$ = " "

            nextcheckkey$ = userid$

             init (" ")  nonapacct$()
             apkey$ = "APACCOUNTS-NON" & " "
             call "READ100" (#11, apkey$ , f1%(11%))
                 if f1%(11%) = 0% then L10000
             get #11 using L09290, nonacctnum%, nonapacct$()
L09290:          FMT XX(20), BI(2), 50*CH(09)

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            *************************************************************

            REM GET VENDOR CODE AND CHECK NUMBER FOR INVOICE IN BUFFER
                call "PLOWNEXT" (#9, nextcheckkey$, 3%, f1%(9))
                     if f1%(9) = 0 then L65000      /* EOD...           */
                get #9, using L10150, vencode$, checknr$
L10150:                 FMT XX(14),      /* SKIP FWD & REVERSE KEYS    */~
                            CH(9),       /* VENDOR   CODE              */~
                            CH(8)        /* CHECK NUMBER               */

                gosub L30000              /* GET NEW CHECK INFORMATION  */
                gosub L31000              /* AND GET OLD CHECK TOO      */

        REM *************************************************************~
            *      A C T U A L L Y   P R O C E S S   T H E   D A T A    *~
            *                                                           *~
            * THE ROUTINE ABOVE RETRIEVED DATA FROM THE FILE.  NOW WE   *~
            * PROCESS IT.                                               *~
            *************************************************************

            REM  SETS ORIGINAL, CURRENT ID & DATES
                lastuserid$ = userid$     /* ASSUME THAT OLD ON FILE.  */
                lastdate$ =  date
                if oldcheckonfile% = 1 then L11160  /* IF IT IS, OUT.*/
                   lastuserid$, lastdate$ = " "
                   origuserid$ = userid$
                   origdate$ = date

L11160:     REM DELETE OLD CHECK FROM MASTER FILE AND WRITE NEW ONE.
                diskjunkkey$ = vencode$
                str(diskjunkkey$, 10) = checknr$
                call "DELETE" (#7, diskjunkkey$, 17%)
                call "DELETE" (#8, diskjunkkey$, 17%)
                call "DELETE" (#30, diskjunkkey$, 17%)
                call "DELETE" (#45, diskjunkkey$, 17%)
                call "DELETE" (#46, diskjunkkey$, 17%)
                gosub L14000

                REM INITIALIZE STACK TO UPDATE BALANCES ON INVOICE HEADER
                    init(hex(ff)) delta$()
                    deltaptr% = 0
                    mat delta = zer

                REM PUSH OLD AND NEW CHECKS ON STACK.
                    if oldmaxlines% = 0 then L12115
                    for temp% = 1 to oldmaxlines%
                        payacct$ = oldacct$(temp%)
                        paytype$ = oldtype$(temp%)
                        gosub ap_account_check
                        if paytype$ = "L"                                ~
                           then gosub'160 (oldinvoice$(temp%),           ~
                             -(oldamount(temp%) - gl_old(temp%)),        ~
                             oldten99$(temp%), "    ")
                           next temp%
L12115:             if maxlines% = 0 then L12180
                    for temp% = 1 to maxlines%
                        payacct$ = acct$(temp%)
                        paytype$ = type$(temp%)
                        gosub ap_account_check
                        if paytype$ = "L"                                ~
                           then gosub'160(invoice$(temp%),               ~
                                   amount(temp%) - gl_new(temp%),        ~
                                   "    ", ten99$(temp%))
                        next temp%

L12180:         REM NOW THAT WE HAVE STACK, UPDATE INVOICE HEADERS.
                    if deltaptr% = 0 then L12370    /* NO ON FILE INV'S */
                       for temp% = 1 to deltaptr%
                           if deltaoldten99$(temp%) <> deltaten99$(temp%)~
                              then L12220
                           if delta(temp%) = 0 then L12330
L12220:                    diskjunkkey$ = vencode$
                           str(diskjunkkey$,10) = delta$(temp%)
                           call "READ101" (#5, diskjunkkey$, f1%(5))
                                if f1%(5) = 0 then L12330
                           get #5, str(paymastr$(),,350)
                           payacct$ = str(paymastr$(),57%,9%)
                           paytype$ = str(paymastr$(),66%,1%)
                           gosub ap_account_check
                           if paytype$ <> "L" then L12330
                           get str(paymastr$(),119,8), using L12350,      ~
                                                balance
                           newone = round(balance - delta(temp%), 2)
                           put str(paymastr$(),119,8), using L12350,      ~
                                                newone
                           str(paymastr$(),163,4) = deltaten99$(temp%)
                           rewrite #5, str(paymastr$(),,350)

L12330:                next temp%
L12350:                FMT PD(14,4)

L12370:         REM UPDATE VENDOR   OUTSTANDING BALANCE FIELD.
                    REM TOTAL UP APPLIED CREDITS (NON-DIRECT SALE ITEMS)
                        appliedtotal = 0
                        if deltaptr% = 0 then L13000
                        for temp% = 1 to deltaptr%
                            appliedtotal = appliedtotal + delta(temp%)
                            next temp%
                        call "READ101"(#3, vencode$, f1%(3))
                        if f1%(3) = 0 then L13000
                           get #3, using L12510, str(vendor$(),1)
                           get str(vendor$(),310,8) using L12520,balance
                           balance = balance - appliedtotal
                           balance = round(balance, 2)
                           put str(vendor$(),310,8) using L12520,balance
                           rewrite #3, using L12510,str(vendor$(),1)

L12510:                        FMT CH(600)
L12520:                        FMT PD(14,4)

L13000:     REM AND FINALLY, DELETE THE CHECK FROM THE CHECK BUFFER.
                diskjunkkey$ = vencode$
                str(diskjunkkey$, 10) = checknr$
                call "DELETE" (#9,  nextcheckkey$, 7%)
                call "DELETE" (#10, diskjunkkey$, 17%)
                call "DELETE" (#42, diskjunkkey$, 17%)
                call "DELETE" (#43, diskjunkkey$, 17%)
                goto L10000

L14000:     REM NOW WRITE THE NEW CHECK (W/UPDATED INFORMATION) TO FILE.
                dateposted$ = paydate$
                checkamount, tempdisc = 0

                    if maxlines% = 0% then L14500
                    for temp% = 1 to maxlines%
                        convert temp% to seqnr$, pic(###)
                        write #8, using L14750,                           ~
                                  vencode$, checknr$, seqnr$,            ~
                                  invoice$(temp%), acct$(temp%),         ~
                                  type$(temp%),amount(temp%),dateposted$,~
                                  disc(temp%), invdate$(temp%),          ~
                                  ten99$(temp%), statusfill$(temp%)

                        if cshlncbf$(temp%) = " " then L14180
                           write #46 using L14173, cshlncbf$(temp%)
L14173:                          FMT CH(100)

L14180:                 if ten99$(temp%) = " " then L14300
L14190:                 call "GETDTTM" addr(datetime$)
                        tdate$ = checkdate$
                        call "DATEFMT" ( tdate$, cdyy% )
                        cdyy% = cdyy% / 10000% : REM Leave only CCYY
                        write #30, using L14880,                         ~
                                   cdyy%, ten99$(temp%),                 ~
                                   vencode$, checknr$, seqnr$,           ~
                                   vencode$, invoice$(temp%),            ~
                                   datetime$,                            ~
                                   checkdate$, dateposted$,              ~
                                   amount(temp%), disc(temp%),           ~
                                   cashacct$, discountacct$,             ~
                                   acct$(temp%), type$(temp%), " ",      ~
                                   eod goto L14190

L14300:             checkamount=checkamount+amount(temp%)-disc(temp%)
                    tempdisc = tempdisc + disc(temp%)

                    next temp%

L14500:         REM NOW WRITE HEADER INFORMATION TO FILE.
                    write #7, using L14590,                               ~
                              vencode$, checknr$, checkdate$,            ~
                              netdiscount, discountacct$, cashacct$,     ~
                              dateposted$, origdate$, origuserid$,       ~
                              lastdate$, lastuserid$, netamount,         ~
                              reconciled$, datereconciled$," "
                    if cshmscbf$ = " " then return
                       write #45 using L14563, cshmscbf$
L14563:                      FMT CH(100)
                    return

L14590:                           FMT CH(9),       /* VENDOR   CODE    */~
                                      CH(8),       /* CHECK NUMBER     */~
                                      CH(6),       /* CHECK DATE       */~
                                      PD(14,4),    /* DISCOUNT AMOUNT  */~
                                      CH(9),       /* DISCOUNT ACCOUNT */~
                                      CH(9),       /* CASH IN BANK ACCT*/~
                                      CH(6),       /* DATE POSTED      */~
                                      CH(6),       /* ORIGINAL DATE    */~
                                      CH(3),       /* USERID-ORIGINAL  */~
                                      CH(6),       /* CURRENT DATE     */~
                                      CH(3),       /* USERID-CURRENT   */~
                                      PD(14,4),    /* NET CHECK AMOUNT */~
                                      CH(1),       /* RECONCILED FLAG  */~
                                      CH(6),       /* DATE RECONCILED  */~
                                      CH(12)       /* FILLER           */

L14750:                           FMT CH(9),       /* VENDOR   CODE    */~
                                      CH(8),       /* CHECK NUMBER     */~
                                      CH(3),       /* SEQUENCE NUMBER  */~
                                      CH(16),      /* INVOICE NUMBER   */~
                                      CH(9),       /* CREDIT ACCOUNT # */~
                                      CH(1),       /* ACCOUNT TYPE     */~
                                      PD(14,4),    /* AMOUNT THIS LINE */~
                                      CH(6),       /* DATE POSTED      */~
                                      PD(14,4),    /* DISCOUNT TAKEN   */~
                                      CH(6),       /* DATE OF INV (PAY)*/~
                                      CH(4),       /* 1099 CATEGORY    */~
                                      CH(22)       /* STATUS & FILLER  */

L14880:                           FMT BI(2),       /* YY CHECK DATE    */~
                                      CH(4),       /* 1099 CATEGORY    */~
                                      CH(9),       /* VENDOR CODE      */~
                                      CH(8),       /* CHECK NUMBER     */~
                                      CH(3),       /* SEQ. NUMBER      */~
                                      CH(9),       /* VENDOR CODE      */~
                                      CH(16),      /* INVOICE #        */~
                                      CH(7),       /* SYSTEM DATE      */~
                                                   /* SYSTEM TIME      */~
                                      CH(6),       /* CHECK DATE       */~
                                      CH(6),       /* POSTED DATE      */~
                                      PD(14,4),    /* GROSS            */~
                                      PD(14,4),    /* DISCOUNT         */~
                                      CH(9),       /* CASH ACCOUNT     */~
                                      CH(9),       /* DISC ACCOUNT     */~
                                      CH(9),       /* EXP. ACCOUNT     */~
                                      CH(1),       /* EXP. ACCT TYPE   */~
                                      CH(6)        /* FILLER           */

        REM *************************************************************~
            *                CHECK A/P LIABILITY ACCOUNTS               *~
            * --------------------------------------------------------- *~
            * Check for Non-A/P General Ledger Liability Accounts       *~
            *                                                           *~
            *************************************************************
        ap_account_check
            if paytype$       <> "L" then return
            if payacct$        = " " then return
            if nonapacct$(1%)  = " " then return
            search str(nonapacct$(), 1%, 9% * nonacctnum%)               ~
                                   = str(payacct$) to index%() step 9%
                if index%(1%)  =  0% then return
            paytype$ = "N"
            return

L30000: REM *************************************************************~
            *        L O A D   C H E C K   F R O M   B U F F E R        *~
            *                                                           *~
            * LOADS A CHECK FROM THE BUFFER. GETS ALL THE INFORMATION   *~
            * WE WILL NEED FOR THAT.                                    *~
            *************************************************************

            init(" ") invoice$(), acct$(), type$(), statusfill$()
            mat amount = zer : mat disc = zer
            mat gl_new = zer : init (" ") cshlncbf$(), cshmscbf$

            get #9, using L30350, checkdate$, netdiscount, discountacct$, ~
                    cashacct$, netamount
            call "READ100" (#42, key(#9,2), f1%(42))
               if f1%(42) = 0% then L30130
            get #42 using L30114, cshmscbf$
L30114:         FMT CH(100)

L30130:     REM SET KEY FOR READING IN CHECK OFF BUFFER.
                temp%, maxlines% = 0
                oldreadkey$ = vencode$
                str(oldreadkey$, 10) = checknr$

L30180:     call "PLOWNEXT" (#10, oldreadkey$, 17%, f1%(10))
                 if f1%(10) = 0 then return
                  temp%, maxlines% = temp% + 1
                  get #10, using L30290, invoice$(temp%), acct$(temp%),   ~
                                        type$(temp%), amount(temp%),     ~
                                        disc(temp%), invdate$(temp%),    ~
                                        ten99$(temp%), statusfill$(temp%)
            call "READ100" (#43, key(#10), f1%(43))
               if f1%(43) = 0% then L30180
            get #43 using L30274, cshlncbf$(temp%)
L30274:         FMT CH(100)
            get #43 using L30276, gl_new(temp%)
L30276:         FMT POS(85), PD(14,4)
            gl_new(temp%) = round(gl_new(temp%), 2%)
            goto L30180

L30290:             FMT XX(20),          /* SKIP CUS#, CHK#, SEQ#      */~
                        CH(16),          /* INVOICE NUMBER             */~
                        CH(9),           /* CREDIT ACCOUNT             */~
                        CH(1),           /* TYPE CODE ("L" OR OTHER)   */~
                        PD(14,4),        /* AMOUNT                     */~
                        PD(14,4),        /* DISCOUNT AMOUNT            */~
                        CH(6),           /* INV DATE AS OF PAY         */~
                        CH(4),           /* 1099 CATEGORY              */~
                        CH(22)           /* STATUS & FILLER            */

L30350:             FMT XX(14),          /* SKIP FWD & REVERSE KEYS    */~
                        XX(17),          /* CUSCODE & CHECK#(WE KNOW IT*/~
                        CH(6),           /* DATE OF CHECK (YYMMDD)     */~
                        PD(14,4),        /* DISCOUNT AMOUNT            */~
                        CH(9),           /* DISCOUNT ACCOUNT           */~
                        CH(9),           /* CASH IN BANK ACCOUNT       */~
                        PD(14,4)         /* NET CHECK AMOUNT           */~

L31000: REM *************************************************************~
            *        G E T   O L D   C H E C K   O F F   F I L E        *~
            *                                                           *~
            * WE NEED TO LOAD THE OLD CHECK OFF THE FILE BECAUSE WE WILL*~
            * NEED TO RECOMPUTE THE VENDOR'S OUTSTANDING BALANCE        *~
            * BASED ON THE NEW CHECK  , BUT ALSO WE HAVE TO REVERSE THE *~
            * OLD.                                                      *~
            *************************************************************

                mat oldamount = zer
                init(" ") oldinvoice$(), oldacct$(), oldtype$()
                oldmaxlines% = 0
                mat gl_old = zer

            REM FIND THE OLD CHECK ON FILE, RETURN IF NOT.
                oldcheckonfile% = 0
                oldreadkey$ = vencode$
                str(oldreadkey$, 10) = checknr$
                call "READ100" (#7, oldreadkey$, f1%(7))
                     if f1%(7) = 0 then return     /* IF NO OLD, FINE. */
                oldcheckonfile% = 1

            get #7, using L31400,                                         ~
                    olddiscount, olddiscountacct$, oldcashacct$,         ~
                    olddateposted$, origdate$, origuserid$
            olddiscount = olddiscount  /* OUT DAMN COMPILER MESSAGE */

            REM GET LINE ITEMS IN PLOW ROUTINE.
                temp%, oldmaxlines% = 0

L31280:         call "PLOWNEXT" (#8, oldreadkey$, 17%, f1%(8))
                     if f1%(8) = 0 then return
                temp%, oldmaxlines% = temp% + 1
                get #8, using L31510, oldinvoice$(temp%), oldacct$(temp%),~
                                     oldtype$(temp%), oldamount(temp%),  ~
                                     oldten99$(temp%)
                call "READ100" (#46, key(#10), f1%(46))
                   if f1%(46) = 0% then L31280
                get #46 using L31377, gl_old(temp%)
L31377:             FMT POS(85), PD(14,4)
                gl_old(temp%) = round(gl_old(temp%), 2%)
                goto L31280

L31400:             FMT XX(17),          /* VENDOR   CODE + CHECK #    */~
                        XX(6),           /* DATE OF CHECK              */~
                        PD(14,4),        /* OLD DISCOUNT AMOUNT        */~
                        CH(9),           /* OLD DISCOUNT ACCOUNT       */~
                        CH(9),           /* OLD CASH IN BANK ACCOUNT   */~
                        CH(6),           /* DATE POSTED (YYMMDD)       */~
                        CH(6),           /* ORIGINALLY INPUT ON (DATE) */~
                        CH(3),           /* ORIGINALLY INPUT BY (USERID*/~
                        XX(9),           /* SKIP LAST USER DATE        */~
                        PD(14,4)         /* OLD CHECK AMOUNT           */~

L31510:             FMT XX(20),          /* SKIP KEY                   */~
                        CH(16),          /* INVOICE NUMBER             */~
                        CH(9),           /* ACCOUNT NUMBER             */~
                        CH(1),           /* ACCOUNT TYPE               */~
                        PD(14,4)         /* AMOUNT THIS LINE           */

        REM *************************************************************~
            *      P U S H   I N V O I C E S   O N T O   S T A C K      *~
            *                                                           *~
            * THIS STACK GETS AMOUNTS PAID ON INVOICES FOR THE OLD AND  *~
            * NEW CHECKS SO WE CAN POST CHANGE IN OUTSTANDING AMOUNTS   *~
            * TO THE INVOICE HEADER RECORDS.  THIS IS SO THAT WE HAVE TO*~
            * GET    AS FEW INVOICES AS POSSIBLE, AND HOPEFULLY ONLY    *~
            * THOSE WHICH HAVE THEIR BALANCES CHANGED.                  *~
            *************************************************************

            deffn'160(stkinvoice$, stkamount, stkoldten99$, stkten99$)
                  search str(delta$(),1) = str(stkinvoice$)              ~
                               to location$ step 16
                  if str(location$, 1, 2) = hex(0000) then L50180
                     location% = int(val(location$,2)/16) + 1
                     delta(location%) = delta(location%) + stkamount
                     deltaten99$(location%) = stkten99$
                     deltaoldten99$(location%) = stkoldten99$
                     return

L50180:           REM THIS PUSHES NEW ENTRY ONTO STACK.
                      deltaptr% = deltaptr% + 1
                      delta$(deltaptr%) = stkinvoice$
                      delta(deltaptr%)  = stkamount
                      deltaten99$(deltaptr%) = stkten99$
                      deltaoldten99$(deltaptr%) = stkoldten99$
                      return

        REM *************************************************************~
            *          ERROR EXIT ROUTINE                               *~
            *************************************************************

        error_exit

            askhdr$ = "* * * E R R O R * * *"
            askpf1$ = "The Program 'CSHFILE1' is Unable To Transfer Your ~
        ~Transactions"
            askmid$ = "Please Contact a SYSTEM ADMINISTRATOR for Assistan~
        ~ce"
            askpf2$ = "Press RETURN to ACKNOWLEDGE and CONTINUE"

            keyhit% = 0%

            call "ASKUSER" (keyhit%, askhdr$, askpf1$, askmid$, askpf2$)

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND EXITS PROGRAM    *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
