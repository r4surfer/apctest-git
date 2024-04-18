        REM *************************************************************~
            *                                                           *~
            *  PPPP    AAA   Y   Y  DDDD    SSS   PPPP   L      Y   Y   *~
            *  P   P  A   A  Y   Y  D   D  S      P   P  L      Y   Y   *~
            *  PPPP   AAAAA   YYY   D   D   SSS   PPPP   L       YYY    *~
            *  P      A   A    Y    D   D      S  P      L        Y     *~
            *  P      A   A    Y    DDDD    SSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYDSPLY - DISPLAYS LEDGER CARDS, SUMMARY AND DETAILED    *~
            *            AGING SCHEDULE, COMPLETE INVOICES AND CHECKS,  *~
            *            AND PROVIDES FOR HARDCOPY OUTPUT.              *~
            *            PAYABLES VERSION OF RECDSPLY, WHICH EXPLAINS   *~
            *            THE RECEIVABLES VARIABLE NAMES                 *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 10/31/80 ! ORIGINAL COMPLETED                       ! SLS *~
            * 03/18/82 ! PRINT FULL VENDOR STATUS REPORT          ! GLW *~
            * 03/26/82 ! CORRECTED REPORT WORDING, WAS NOT PRINTG ! BEV *~
            *          !  "NO INVOICES", AND SHORTENED PAGE LENGTH!     *~
            * 03/31/82 ! CORRECTED WRONG PHONE NUMBERS ON REPORT  ! BEV *~
            * 02/18/83 ! DEFINE VALID AP ACCOUNTS                 ! KEN *~
            * 05/17/85 ! SET VALID LIABILITIES INTO 2 BUCKETS     ! JRW *~
            * 04/05/84 ! FIXED A FEW MINOR BUGS                   ! JUDD*~
            * 09/24/84 ! FIX PRRS 1756, 35, & 8401304             ! BLT *~
            * 07/23/85 ! Added extendable G/L Account number      ! HES *~
            * 12/13/85 ! Vendor file format change                ! MJB *~
            * 04/14/86 ! ADDED PRINT OF DISCOUNT ON INVOICE PRINT ! WPH *~
            * 05/13/86 ! INVOICE FILES FORMAT CHANGES (RCV STUFF) ! HES *~
            * 08/14/86 ! Screen Corrections                       ! KAB *~
            * 05/18/87 ! PAYLINES record length mod- Standard cost! JIM *~
            * 09/30/87 ! Added logic to exclude reccuring invoices!     *~
            *          ! from appearing to this program           ! DAW *~
            * 01/06/89 ! Corrected PF Literal on Invc Detail Scrn ! MJB *~
            * 03/21/89 ! Fixed Print Vendor Status, now 1 PRT File! RJM *~
            * 04/04/89 ! Added askuser to end to Aging Display,   ! RJM *~
            *          !  (Summary & Detail) to warn user of      !     *~
            *          !  returning to menu after hitting PF5.    !     *~
            * 06/01/89 ! Fixed to process correct vendor following! MLJ *~
            *          !   call to GETCODE.                       !     *~
            * 07/12/89 ! Added ability to toggle between STATUTORY! MLJ *~
            *          !  and TRANSACTION values on displays      !     *~
            * 01/02/90 ! Minor tweek to check screen with MC.     ! JDH *~
            * 01/05/90 ! More "minor tweeks" to multi-currency.   ! MLJ *~
            * 04/05/90 ! Bandaid on Ledger Card screen handling.  ! JDH *~
            * 08/14/90 ! Added vendor name & address load for     ! MJB *~
            *          !   Invoice display from ledger display.   !     *~
            * 04/12/91 ! PRR 11786  Added a multi-currency flag   ! SID *~
            *          !   to prevent PFkeys from getting blank   !     *~
            *          !   out.                                   !     *~
            * 01/02/92 ! PRR 11600  Added GETCODE on Vendor code  ! WPH *~
            * 04/03/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 10/06/92 ! PRR 12633 - Unapplied checks are now     ! MLJ *~
            *          !   correctly reflected in aging.          !     *~
            * 10/15/92 ! Increased printed and displayed check    ! MLJ *~
            *          !   amt by one position.  Page number now  !     *~
            *          !   gets reset appropriately.              !     *~
            * 10/28/92 ! PRR 12647 - Amts applied to purged inv's ! MLJ *~
            *          !  now under separate heading and no longer!     *~
            *          !  appear as unapplied.                    !     *~
            *          ! PRR 12399 - Strung 2nd arg of SEARCH at  !     *~
            *          !  line #52055.                            !     *~
            * 12/04/92 ! The ON FILE? column of the report now has!     *~
            *          !  unapplied checks flagged as "NO/UNAP".  ! MLJ *~
            * 12/09/92 ! PRR 12375 - No longer displays garbage   ! MLJ *~
            *          !  for curr desc on invoice header display.!     *~
            *          ! PRR 12391 - Invoices on hold are now     !     *~
            *          !  flagged on A/P001 with "[Hold]" in the  !     *~
            *          !  receiver number column.                 !     *~
            *          ! PRR 12619 - Added AKSUSER to include zero!     *~
            *          !  balance invoices on PF(6) Print Detailed!     *~
            *          !  Aging Schedule.                         !     *~
            * 09/08/93 ! PRR 12867 - Fixed ONFILE$ test to include! JDH *~
            *          !  unapplied for not-on-file asterisk.     !     *~
            *          ! PRR 13017 - Fixed branches to a FMT stmnt!     *~
            *          ! PRR 12988 - Total Payables for a vendor  !     *~
            *          !  on summary aging now considers unapplied!     *~
            * 01/18/94 ! Misc fixes for 6.03.00.  Including new   ! JBK *~
            *          !  Non-A/P Liability accounts to exclude   !     *~
            *          !  checks written to them from aging and   !     *~
            *          !  totals.  Also multiple changes for M-C  !     *~
            *          !  in calculation and display where there  !     *~
            *          !  are multiple payments (checks) against  !     *~
            *          !  an invoice.                             !     *~
            * 12/15/94 ! Added PF10 Find Invoice.                 ! JDH *~
            * 01/16/95 ! PRR 13343. Non-A/P Lia. Accts now to 50. ! JDH *~
            * 08/15/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            accountdescr$32,             /* DESCRIPTION OF CHECK CREDIT*/~
            acct$(6)16,                  /* ACCOUNT NUMBER CODES       */~
            accttype$1,                  /* ACCOUNT TYPE (CHECK FOR "A"*/~
            address$(3)30,               /* VENDOR   ADDRESS.          */~
            aging(5),                    /* AGING (NUMERIC)            */~
            agingcheck$50,               /* BUFFERS NEXTCHECKKEY$      */~
            agingcheckdetail$20,         /* BUFFERS CHECKDETAILKEY$    */~
            agingvencode$9,              /* BUFFERS VENCODE$           */~
            agingdate$6,                 /* USED FOR AGING INVOICES    */~
            agingfirstvend$9,            /* BUFFERS FIRSTVEND$         */~
            aginginvoice$50,             /* BUFFERS NEXTINVOICEKEY$    */~
            aginglastvend$9,             /* BUFFERS LASTVEND$          */~
            agingline$(20)79,            /* BUFFERS LINE$()            */~
            aginglinekey$(24)25,         /* KEYS FOR BRANCHING FROM PRT*/~
            apacct$(28)16,               /* VALID AP ACCOUNTS          */~
            apkey$20,                    /* SYSFILE2 KEY FIELD         */~
            availcredit$10,              /* PRINTS AVAILABLE CREDIT    */~
            bal_line$15,                 /* BAL UNDELINE - DISPLAY     */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cat$(6)4,                    /* LINE ITEM CATEGORY CODES   */~
            ck_label1$13,                /* STATUTORY LABEL - DISPLAY  */~
            ck_label2$15,                /* TRANSACTION LABEL -DISPLAY */~
            date$8,                      /* Todays date for display    */~
            checkdate$8,                 /* DATE OF CHECK              */~
            checkdetailkey$28,           /* KEY FOR CHECK DETAILS      */~
            checkmessage$50,             /* MSG. ABOUT UNAPPLIED CHECKS*/~
            checknumber$8,               /* CHECK NUMBER               */~
            checkstk$(100)8,             /* CHECK NUMBERS FOR STACK.   */~
            checkstk(100),               /* AND AMOUNTS PAID EA CHECK  */~
            creditlimit$10,              /* PRINTS CREDIT LIMIT        */~
            cshacct$16,                  /* CASH IN BANK ACCOUNT       */~
            cshacctdescr$32,             /* DESCRIPTION OF CASH IN BANK*/~
            curr$1,                      /* MULTI-CURRENCY FLAG        */~
            curr_label$13,               /* CURRENCY DISPLAY LABEL     */~
            curr_desc$32,                /* CURRENCY DISPLAY DESCR.    */~
            curr_code$4,                 /* CURRENCY DISPLAY CODE      */~
            cursor%(2),                  /* CURSOR POSITION            */~
            vencode$9,                   /* VENDOR   CODE FOR DISPLAY  */~
            veninfo$(4)30,               /* VEN. INFO FOR DETAIL AGING */~
            venname$30,                  /* VENDOR   NAME              */~
            vendorcode$79,               /* DISPLAY VENDOR   CODE      */~
            cutoffdates$(5)6,            /* CUTOFF DATES FOR AGING     */~
            dateposted$10,               /* DATE CHECK WAS POSTED      */~
            descr$(6)32,                 /* DESCRIPTIONS               */~
            disacct$16,                  /* DISCOUNT ACCOUNT NUMBER    */~
            disacctdescr$32,             /* DESCRIPTION OF DISCOUNT ACC*/~
            discdate$10,                 /* DUE DATE TAKING DISCOUNT   */~
            duedate$10,                  /* DUE DATE IGNORING DISCOUNT */~
            errormsg$79,                 /* ERROR MESSAGE FOR DATA TEST*/~
            extensiont$(6)10,            /* EXTENSION - INV AMT (TRAN) */~
            extension$(6)10,             /* EXTENSION - INV AMT (STAT) */~
            exchg_ratei$10,              /* INVOIVE EXCHANGE RATE      */~
            exchg_ratec$10,              /* CHECK EXCHANGE RATE        */~
            findinvoice$16,              /* ASKSTRNG VARIABLE          */~
            firstvend$9,                 /* FIRST VENDOR   IN RANGE    */~
            foriegn_curr$1,              /* FORIEGN CURRENCY IN USE ?  */~
            freetest$20,                 /* FREE TEST FIELD ON HEADER  */~
            gain_loss_label1$18,         /* G/L DISPLAY LABEL - ACCT   */~
            g_l_stk$(100)16,             /* Gain-Loss Accts for Stack  */~
            g_l_stk(100),                /* Gain_Loss Amts for Stack   */~
            gain_loss_amt$10,            /* G/L DISPLAY AMOUNT         */~
            gain_loss_label2$21,         /* G/L DISPLAY LABEL - AMT    */~
            gain_loss_acct$9,            /* G/L DISPLAY ACCOUNT        */~
            hdrdate$45,                  /* HEADER FOR PRINTED OUTPUT  */~
            header_balance$10,           /* MC - 2nd Header, Inv. Bal  */~
            header_curr$4,               /* MC - 2nd Header, Currency  */~
            header_invamt$10,            /* MC - 2nd Header, Inv. Amt  */~
            header_nodisc$10,            /* MC - 2nd Header, Non-Disc  */~
            header_label1$31,            /* MC - 2nd Header, Col. Label*/~
            hit$2,                       /* SUBROUTINE SEARCH TARGET   */~
            holdflag$1,                  /* INVOICE ON HOLD FLAG       */~
            i$(24)80,                    /* USED FOR CURSOR POSITION   */~
            invoice$16,                  /* INVOICES PAID BY A CHECK   */~
            inv_type$1,                  /* Invoice type field         */~
            invoicedate$8,               /* DATE OF INVOICE            */~
            invoicelinekey$50,           /* KEY FOR INVOICE LINE ITEMS */~
            invoicenr$16,                /* INVOICE NUMBER             */~
            invoicepaidkey$25,           /* KEY TO TEST FOR UNAPP. CHK.*/~
            item$(6)3,                   /* ITEM NUMBERS               */~
            job$(6)8,                    /* JOB NUMBER INFORMATION     */~
            keys$(11)50,                 /* ACTIVE KEYS IN MAIN SCREEN */~
            lastvend$9,                  /* LAST VENDOR   DISPLAYED    */~
            lastdate$8,                  /* LAST OF LAST MODIFICATION  */~
            lastuserid$3,                /* USER LAST MODIFYING DOCUMNT*/~
            ledgercheck$50,              /* BUFFERS NEXTCHECKKEY$      */~
            ledgercheckdetail$28,        /* BUFFERS CHECKDETAILKEY$    */~
            ledgervencode$9,             /* BUFFERS VENCODE$           */~
            ledgerfirstvend$9,           /* BUFFERS FIRSTVEND$         */~
            ledgerinvoice$50,            /* BUFFERS NEXTINVOICEKEY$    */~
            ledgerlastvend$9,            /* BUFFERS LASTVEND$          */~
            ledgerline$(20)79,           /* BUFFERS LINE$()            */~
            ledgerlinekey$(24)25,        /* KEYS FOR BRANCHING FROM PRT*/~
            line$(20)79,                 /* SCREEN TO DISPLAY TO       */~
            line$79,                     /* LINE TO DISPLAY TO         */~
            line2$79,                    /* Screen line #2             */~
            linea$(20)79,                /* Multi-Curr 1st line array  */~
            linea$79,                    /* 1st Line: Multi-Curr TRANS */~
            lineb$(20)79,                /* Multi-Curr 2st line array  */~
            lineb$79,                    /* 2nd Line: Multi-Curr TRANS */~
            linec$(20)79,                /* Mulit-Curr CK Tran Array   */~
            linenumber%(2),              /* PRINTING ROUTINE LINE CNTRS*/~
            linestatus$1,                /* LINE FORZEN OR MODIFIABLE  */~
            lfac$(20)1,                  /* SCREEN DISPLAY FAC         */~
            lnseqno$3,                   /* INVOICE ON THIS LINE       */~
            location$10,                 /* USED WITH "SEARCH" COMMAND */~
            lot$(6)6,                    /* LOT NUMBER INFORMATION     */~
            mc_flag1$1,                  /* 2-LINE VENDOR CARD DISPLAY */~
            message$79,                  /* USED FOR SCREEN MESSAGES   */~
            nextcheckkey$50,             /* PLOW KEY FOR NEXT CHECK    */~
            nextvend$9,                  /* NEXT VENDOR   IN RANGEPRINT*/~
            nextinvoicekey$50,           /* PLOW KEY FOR NEXT INVOICE  */~
            no_more$1,                   /* NO MORE INVOICES - DISPLAY */~
            nonapacct$(50)16,            /* Valid Non-AP Liability Acct*/~
            okkeys$16,                   /* OK KEYS FOR MAIN SCREEN    */~
            onfile$7,                    /* IS INVOICE PAID ON FILE?   */~
            origdate$8,                  /* DATE OF ORIGINAL ENTRY     */~
            origuserid$3,                /* USER FIRST ENTERED DOCUMNT */~
            part$(6)26,                  /* PART NUMBERS               */~
            pay_flag$1,                  /* USE PAYMASTR CURRENCY CODE */~
            pay_curr_code$4,             /* CURRENCY CODE - PAYMASTR   */~
            pay_curr_desc$30,            /* CURRENCY DESC - PAYMASTR   */~
            period$(5)10,                /* NAMES OF AGING PERIODS     */~
            pfkey$79,                    /* FORMATTED LIST OF PF KEYS  */~
            pfkeys$20,                   /* KEYS POSSIBLY FUNCTIONAL   */~
            pfkeycheck$(4)79,            /* PFKEYS FOR DISPLAY CHECK   */~
            pfkeyinvoice$(4)79,          /* PFKEYS FOR DISPLAY INVOICE */~
            pfkeysummary$(3)79,          /* PFKEYS FOR DISPLAY SUMMARY */~
            phone$30,                    /* FORMATTED TELEPHONE NUMBER */~
            po$(6)16,                    /* ACCEPT ARRAY OF PO NUMBERS */~
            poline$(6)3,                 /* ACCEPT ARRAY OF PO NUMBERS */~
            price$(6)10,                 /* UNIT PRICE - STATUTORY     */~
            pricet$(6)10,                /* UNIT PRICE - TRANSACTION   */~
            prtaccttype$10,              /* FOR "CREDIT?" FLAG ON CHECK*/~
            prt$(5)12,                   /* PRINT AGED INVOICE AMOUNTS */~
            prtvencode$9,                /* VEN. CODE FOR DETAIL AGING */~
            prtdate$10,                  /* HOLDS INVOICE DATE         */~
            qtysold$(6)10,               /* QUANTITY SOLD              */~
            payacct$16,                  /* PAYEIVABLES ACCOUNT INFO   */~
            payacctdescr$32,             /* PAYEIVABLES ACCT DESCRIPTN */~
            payaccttype$1,               /* PAYEIVABLES ACCOUNT TYPE   */~
            paydate$8,                   /* PAYEIVABLES DATE-IMPORTANT */~
            payfac$1,                    /* PAYEIVABLES DATE FAC       */~
            receiver$18,                 /* PURCHASE ORDER NUMBER      */~
            readcm$4,                    /* CURMASTR - READ KEY        */~
            readcurr$20,                 /* CSHLNCUR - READ KEY        */~
            readcsh$17,                  /* CSHMSCUR - READ KEY        */~
            readpay$28,                  /* PAYLNCUR - READ KEY        */~
            screenvencode$9,             /* VENCODE$ FOR MAIN SCREEN   */~
            screendate$8,                /* FORMATTED PAYEIVABLES DATE */~
            separator$(5)79,             /* INTER-LINE SEPARATOR TEXT  */~
            slsacct$16,                  /* SALES ACCOUNT NUMBER       */~
            slsacctdescr$32,             /* SALES ACCOUNT DESCRIPTION  */~
            stat_curr_code$4,            /* CURRENCY CODE - STATUTORY  */~
            stat_curr_desc$30,           /* CURRENCY DESC - STATUTORY  */~
            store$(6)3,                  /* ACCEPT ARRAY OF STORE NUMS */~
            sumapplied(2),               /*                            */~
            suminvoices(2),              /*                            */~
            sumunapplied(2),             /*                            */~
            sumbalance(2),               /*                            */~
            sum_label$34,                /* MULTI-CURRENCY SUM LABEL   */~
            tempaccttype$1,              /* TEMPORARY ACCOUNT TYPE     */~
            tempaging(5),                /* BUFFER FOR AGING()         */~
            tempvencode$9,               /* TEMPORARY VENDOR   CODE    */~
            tempdate$6,                  /* TEMPORARY DATE             */~
            tempinvoice$16,              /* TEMPORARY INVOICE NUMBER   */~
            tempphone$16,                /* HOLDS PHONE NUMBER         */~
            temptotal(5),                /* BUFFER FOR TOTAL()         */~
            tran_curr_code$4,            /* CURR CODE  - TRANSACTION   */~
            tran_curr_desc$30,           /* CURR DESCR - TRANSACTION   */~
            tran_pmt_amt(100),           /* TRAN amt paid each check   */~
            ttacct$16,                   /* SUBROUTINE ACCT ARG        */~
            ttype$1,                     /* SUBROUTINE TYPE ARG        */~
            text$4,                      /* Document Text Id. Number   */~
            text$(113,1)70,              /* Free Text Array            */~
            text1$79,                    /* TEXT FOR MESSAGE ABOUT     */~
            text2$79,                    /*      THE IMPORTANCE OF THE */~
            text3$79,                    /*      PAYEIVABLES DATE      */~
            title$79,                    /* TITLES FOR SCREEN DISPLAY  */~
            total(5),                    /* AGING REPORT FINAL TOTAL   */~
            txbl$(6)1,                   /* TAXABLE (Y/N) THIS LINE    */~
            type$16,                     /* DESC. OF PAYIEVABLES ACCT. */~
            userid$3,                    /* HOLDS USERID               */~
            wherefrom$50,                /* HOLDS PROGRAM IN PROGRESS  */~
            xixacct$16,                  /* CHECK CREDIT ACCOUNT       */~
            zflag$1                      /* INCLUDE 0 BAL INV FLAG     */~

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
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN FILE                       *~
            * # 6 ! PAYLINES ! PAYABLES LINE ITEMS                      *~
            * # 7 ! CSHMASTR ! CASH RECEIPTS CHECK HEADER FILE          *~
            * # 8 ! CSHLINES ! CASH RECEIPTS CHECK DETAIL FILE          *~
            * #10 ! STORNAME ! STORE INFORMATION                        *~
            * #11 ! SYSFILE2 ! SYSTEM CATCH ALL FILE                    *~
            * #12 ! TXTFILE  ! System Text File                         *~
            * #15 ! CSHLNCUR ! Currency Specific - CSHLINES             *~
            * #16 ! CURMASTR ! Multi-Currency Master File               *~
            * #17 ! CSHMSCUR ! Currency Specific - CSHMASTR             *~
            * #18 ! PAYLNCUR ! Currency Specific - PAYLINES             *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select # 2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen = 9,                          ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #6, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select  #7, "CSHMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 17,                         ~
                        alternate key 1, keypos = 41, keylen = 9, dup,   ~
                                  key 2, keypos = 50, keylen = 6, dup,   ~
                                  key 3, keypos = 10, keylen = 8, dup    ~

            select  #8, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select  #10, "STORNAME",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 3

            select  #11, "SYSFILE2",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #12, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #15, "CSHLNCUR",                                      ~
                        varc,  indexed,  recsize = 100,                  ~
                        keypos =  5,  keylen = 20,                       ~
                   alternate key 1, keypos =  1,  keylen = 44

            select #16, "CURMASTR",                                      ~
                        varc,  indexed,  recsize = 256,                  ~
                        keypos =  1,  keylen =  4

            select #17, "CSHMSCUR",                                      ~
                        varc,  indexed,  recsize =  100,                 ~
                        keypos =  5,  keylen =  17,                      ~
                   alternate key 1, keypos =  1,  keylen = 21

            select #18, "PAYLNCUR",                                      ~
                        varc,  indexed,  recsize = 100,                  ~
                        keypos =  5,  keylen =  28,                      ~
                   alternate key 1, keypos =  1,  keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 6, "SHARE", f2%( 6), rslt$( 6), axd$( 6))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))

        REM *************************************************************~
            *         L O C A T I O N    O F    R O U T I N E S         *~
            *                                                           *~
            * DESPITE THE SIZE OF THIS PROGRAM, IT IS MERELY A COLLECT- *~
            * ION OF SMALL ROUTINES.  UNDERSTANDING THESE ROUTINES IS   *~
            * THE KEY TO UNDERSTANDING THIS LARGE BUT SIMPLE PROGRAM.   *~
            *                                                           *~
            * MAIN_SCREEN                                    10000      *~
            *                                                           *~
            *      DISPLAY_LEDGER                            11000      *~
            *           EXIT_DISPLAY_LEDGER                  11000      *~
            *                                                           *~
            *           DISPLAY_INVOICE                      12000      *~
            *                EXIT_DISPLAY_INVOICE            12000      *~
            *                FORMAT_DISPLAY_INVOICE          13000      *~
            *                                                           *~
            *           DISPLAY_CHECK                        14000      *~
            *                EXIT_DISPLAY_CHECK              14000      *~
            *                FORMAT_DISPLAY_CHECK            15000      *~
            *                                                           *~
            *           DISPLAY_SUMMARY_LEDGER               16000      *~
            *                                                           *~
            *      PRINT_LEDGER                              17000      *~
            *      PRINT_INDIVIDUAL_LEDGER                   18000      *~
            *                                                           *~
            *           PRINT_INVOICE                        19000      *~
            *                EXIT_PRINT_INVOICE              19000      *~
            *                                                           *~
            *           PRINT_CHECK                          20000      *~
            *                 EXIT_PRINT_CHECK               20000      *~
            *                                                           *~
            *           PRINT_SUMMARY_LEDGER                 21000      *~
            *                                                           *~
            *      DISPLAY_DETAIL_AGING                      22000      *~
            *           EXIT_DISPLAY_DETAIL_AGING            22000      *~
            *                                                           *~
            *      PRINT_DETAIL_AGING                        23000      *~
            *           EXIT_PRINT_DETAIL_AGING              23000      *~
            *                                                           *~
            *      DISPLAY_SUMMARY_AGING                     24000      *~
            *           EXIT_DISPLAY_SUMMARY_AGING           24000      *~
            *                                                           *~
            *      PRINT_SUMMARY_AGING                       25000      *~
            *           EXIT_PRINT_SUMMARY_AGING             25000      *~
            *                                                           *~
            * CONTROL_DISPLAY_INVOICE                        26000      *~
            * CONTROL_DISPLAY_CHECK                          26000      *~
            * CONTROL_DISPLAY_DETAIL_AGING                   26000      *~
            * CONTROL_DISPLAY_SUMMARY_AGING                  26000      *~
            *                                                           *~
            * CONTROL_PRINT_INVOICE                          28000      *~
            * CONTROL_PRINT_CHECK                            28000      *~
            *      NEW_PAGE                                  28000      *~
            *      CHECK_HEADING                             28000      *~
            * CONTROL_PRINT_DETAIL_AGING                     28000      *~
            * CONTROL_PRINT_SUMMARY_AGING                    28000      *~
            * CONTROL_PRINT_ENTIRE_INVOICE                   28000      *~
            * CONTROL_PRINT_ENTIRE_CHECK                     28000      *~
            *                                                           *~
            * READ_NEXT_INVOICE                              30000      *~
            * READ_NEXT_CHECK                                31000      *~
            * READ_NEXT_INVOICE_PAID                         32000      *~
            * FIND_APPLICABLE_CHECKS                         33000      *~
            *                                                           *~
            * COMPUTE_SUMMARY_TOTAL                          34000      *~
            * COMPUTE_SUMMARY_CHECKS                         34000      *~
            * AGING                                          35000      *~
            * SUMMARY_TOTAL_MANIPULATION                     35000      *~
            *                                                           *~
            * READ_NEXT_VENDOR                               36000      *~
            *      NO_MORE_VENDORS                           36000      *~
            *                                                           *~
            * HOLD_LEDGER_POINTERS                           37000      *~
            * RESTORE_LEDGER_POINTERS                        37000      *~
            * HOLD_AGING_POINTERS                            37000      *~
            * RESTORE_AGING_POINTERS                         37000      *~
            *                                                           *~
            * DISPLAY_ENTIRE_INVOICE                         38000      *~
            * PRINT_ENTIRE_INVOICE                           38000      *~
            *                                                           *~
            * DISPLAY_ENTIRE_CHECK                           39000      *~
            * PRINT_ENTIRE_CHECK                             39000      *~
            *                                                           *~
            * CONTROL_SCREEN                                 40000      *~
            *      GET_CURSOR_POSITION                       40000      *~
            * SUMMARY_SCREEN                                 41000      *~
            * INVOICE_SCREEN                                 42000      *~
            * CHECK_SCREEN                                   43000      *~
            * DETAIL_AGING_SCREEN                            44000      *~
            * SUMMARY_AGING_SCREEN                           45000      *~
            *                                                           *~
            * INVOICE_HEADER_FIRST_SCREEN                    46000      *~
            * INVOICE_HEADER_SECOND_SCREEN                   46000      *~
            * INVOICE_LINE_ITEM_SCREEN                       47000      *~
            *                                                           *~
            * CHECK_HEADER_SCREEN                            48000      *~
            * CHECK_LINE_ITEM_SCREEN                         49000      *~
            *                                                           *~
            * TEST_VENDOR_CODE                               50000      *~
            * TEST_RANGE                                     51000      *~
            *                                                           *~
            * END_PROGRAM                                    65000      *~
            *************************************************************

        REM *************************************************************~
            *    I N I T I A L I Z A T I O N   F O R   P R O G R A M    *~
            *                                                           *~
            * INITIALIZES USER ID AND A FEW OTHER THINGS THAT WE NEED.  *~
            * *** DON'T FORGET *** ALSO INITIALIZES CUTOFF DATES FOR    *~
            * AGING SCHEDULES.                                          *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC"(blankdate$)

            call "EXTRACT" addr ("ID", userid$) /* GET CURRENT USER    */
            call "READ100" (#1, userid$, f1%(1))
                 if f1%(1) = 0 then exit_program
                 get #1, using L09120 , paydate$
L09120:                  FMT XX(9), CH(6)

             pagenumber% = 0%
             init (" ") apacct$(), nonapacct$()
             apkey$ = "APACCOUNTS" & " "
             call "READ100" (#11, apkey$ , f1%(11%))
                 if f1%(11%) <> 0% then L09180
                     gosub L55000
                     goto L09280
L09180:      get #11 using L09190, acctnum%, apacct$()
L09190:          FMT XX(20), BI(2), 28*CH(16)

             for i% = 1 to 28
                call "GLFMT" (apacct$(i%))
             next i%

L09280:      apkey$ = "APACCOUNTS-NON" & " "
             call "READ100" (#11, apkey$ , f1%(11%))
                 if f1%(11%) <> 0% then L09310
                     gosub L55200
                     goto L09340
L09310:      get #11 using L09312, nonacctnum%, nonapacct$()
L09312:          FMT XX(20), BI(2), 50*CH(09)

             for i% = 1% to 50%
                call "GLFMT" (nonapacct$(i%))
             next i%

L09340:          screendate$ = paydate$
                 call "DATEFMT" (screendate$)

        text1$ = "NOTE:"
        text2$ = "Checks and Invoices Posted After the Payables Date Show~
        ~n Above Will NOT Be"
        text3$ = "Included in the Ledger Detail, Ledger Summary Page, or ~
        ~The Aging Schedules."

        REM              SET HEADINGS FOR DISPLAY LEDGER                 ~
               THE VARIABLE MODE% INDICATES CALL FROM MENU OR AGING      ~
                   #1 IS WHEN LEDGER IS CALLED FROM MENU                 ~
                   #3 SAME AS #1 BUT FOR MULTI-CURRENCY                  ~
                   #2 IS WHEN LEDGER IS CALLED FROM DISPLAYED AGING      ~
                   #4 SAME AS #2 BUT FOR MULTI-CURRENCY...

        pfkeysummary$(1)= "(2)Invoices (3)Checks (5)Next Vendor   (14)Pri~
        ~nt Vendor (15)Print Scrn (16)Menu"
        pfkeysummary$(2)= "(2)Invoices (3)Checks (5)Aging Display (14)Pri~
        ~nt Vendor (15)Print Scrn (16)EXIT"
        pfkeysummary$(3)= "(2)Invoices (3)Checks (5)Next Vendor   (14)Pri~
        ~nt Vendor (15)Print Scrn (16)Menu"

        pfkeyinvoice$(1)= "(2)First (5)Next (8)Sum (9)Checks (10)Find Inv~
        ~ (12)Detail (14)Prt Vend (16)Menu"
        pfkeyinvoice$(2)= "(2)First (5)Next (8)Sum (9)Checks (10)Find Inv~
        ~ (12)Detail (14)Prt Vend (16)EXIT"

        pfkeycheck$(1)=   "(2)First (5)Next (8)Sum (9)Invoice (12)Detail ~
        ~14)Prt Vend (15)Prt Scrn (16)Menu"
        pfkeycheck$(2)=   "(2)First (5)Next (8)Sum (9)Invoice (12)Detail ~
        ~14)Prt Vend (15)Prt Scrn (16)EXIT"

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62) = "PAYDSPLY: " & str(cms2v$,,8)

        REM Check multi-currency usage, get statutory code and descr...
            curr$, mc_flag1$ = "N"
            stat_curr_code$, stat_curr_desc$ = " "
            call "READ100" (#11, "SWITCHS.CUR", f1%(11))
            if f1%(11) = 0% then L10000
                get #11 using L09740, curr$, stat_curr_code$
L09740:              FMT POS(21), CH(1), CH(4)
            if curr$ = "N" then L10000   /*  Multi-currency not used  */

            curr_label$ = "Currency Code" :  no_more$ = "N"
            gain_loss_label1$ = "Gain/Loss Account"
            gain_loss_label2$ = "Realized Gain/Loss(+)"
            sum_label$ = "------- Statutory  Amounts -------"
            bal_line$  = "==============="
            ck_label1$ = "--Statutory--"
            ck_label2$ = "--Transaction--"
            header_label1$ = "--Statutory--   --Transaction--"
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))
            call "OPENFILE" (#17, "SHARE", f2%(17), rslt$(17), axd$(17))
            call "OPENFILE" (#18, "SHARE", f2%(18), rslt$(18), axd$(18))
            readcm$ = stat_curr_code$
            call "READ100" (#16, readcm$, f1%(16))
            if f1%(16) = 0% then L10000
                get #16 using L09830, stat_curr_desc$
L09830:              FMT POS(5), CH(30)

L10000: REM *************************************************************~
            *                  M A I N    S C R E E N                   *~
            *                                                           *~
            * GETS THE VENDORS  CODE OR RANGE OF CODES, AND THEN PRINTS *~
            * OR DISPLAYS THE DESIRED REPORT.                           *~
            *************************************************************

        main_screen
            errormsg$ = " "
            init (hex(a4)) payfac$
            if firstvend$ <> " " then L10150
               firstvend$ = "ALL"
               lastvend$  = "   "
               goto L10190

L10150: REM Adjust first and last vendor from "ALL"...
            if firstvend$ = "ALL" or firstvend$ = lastvend$ then         ~
               lastvend$ = " "

L10190:     if wherefrom$ <> " " then L10370
        REM We are at the Beginning Control Menu...
                init (hex(ff)) okkeys$
                str( okkeys$, 1,12) = hex(020304050607ff0d0e0f1000)
                message$   = "                                         "
                keys$(1)  = "(2)        Display Vendor Ledger Cards    "
                keys$(2)  = "(3)        Display Detailed Aging Schedule"
                keys$(3)  = "(4)        Display Summary Aging Schedule "
*              KEYS$(4)  = "                                          "
                keys$(4)  = "(5)        Print Full Vendor Status Report"
                keys$(5)  = "(6)        Print Detailed Aging Schedule  "
                keys$(6)  = "(7)        Print Summary Aging Schedule   "
                keys$(7)  = "                                          "
                keys$(8)  = "(13)       Instructions                   "
                keys$(9)  = "(14)       Adjust Payables Date           "
                keys$(10) = "(15)       Print Screen                   "
                keys$(11) = "(16)       Exit Payables Display          "
                goto L10550

L10370: REM We are in the middle of a Display...
                init (hex(ff)) okkeys$
                str( okkeys$, 1, 7) = hex(010506070d0f10)
                message$  = "Program Was Displaying VENDOR:"
                str(message$, 32) = screenvencode$
                keys$(1)  = "(1)        Return To                      "
                keys$(2)  = "                                          "
                keys$(3)  = "(5)        Print Vendor Ledger Cards      "
                keys$(4)  = "(6)        Print Detailed Aging Schedule  "
                keys$(5)  = "(7)        Print Summary Aging Schedule   "
                keys$(6)  = "                                          "
                keys$(7)  = "(13)       Instructions                   "
                keys$(8)  = "(15)       Print This Screen              "
                keys$(9)  = "(16)       CANCEL                         "
                keys$(10) = "                                          "
                keys$(11) = "                                          "
                str(keys$(1), 22), str(keys$(9), 19) = wherefrom$

L10550:         gosub control_screen
                      if mainscreen%  =  1 then return
                      if mainscreen%  < 16 then L10630
                         if wherefrom$ = " " then exit_program
                            wherefrom$ = " "
                            return clear all
                            goto main_screen

L10630:         gosub test_range
                     if errormsg$ <> " " then L10550
                     if mainscreen%  =  2 then gosub display_ledger
                     if mainscreen%  =  3 then gosub display_detail_aging
                     if mainscreen%  =  4 then                           ~
                                          gosub display_summary_aging
                     if mainscreen%  =  5 then gosub print_ledger
                     if mainscreen%  =  6 then gosub print_detail_aging
                     if mainscreen%  =  7 then gosub print_summary_aging

                     goto main_screen

        REM *************************************************************~
            *            D I S P L A Y    A    L E D G E R              *~
            *                                                           *~
            * THIS CONTROLS THE PLOWING FOR DISPLAY OF LEDGER CARDS.    *~
            *************************************************************

        display_ledger
            wherefrom$ = "'DISPLAY VENDOR LEDGERS'"
        REM Set PFKEY labels for STANDARD or TRANSACTION...
            if curr$ = "N" then mode% = 1% else mode% = 3%
L11090:     gosub read_next_vendor
                  if f1%(3) = 0 then exit_display_ledger
                     screenvencode$ = vencode$     /* FOR MAIN SCREEN  */
                     gosub display_invoice
                     goto L11090          /* PROCESS THE NEXT VENDOR    */

        exit_display_ledger
            wherefrom$ = " "             /* REPORT FINISHED            */
            return

        REM *************************************************************~
            * D I S P L A Y   R O U T I N E   F O R   I N V O I C E S   *~
            *                                                           *~
            * DISPLAYS THE ENTRIES FOR THE VENDOR'S INVOICES.           *~
            * DOES SO IN A FORMAT SIMILAR TO A 2-COLUMN REPORT, BUT THE *~
            * ANALOG TO THE PAGE ROUTINE HERE IS THAT WHICH DOES THE    *~
            * DISPLAY AND THEN RETURNS WHEN BUTTON PRESSED.             *~
            *************************************************************

        display_invoice
        REM Set first key for invoices...
                line% = 0
                first_screen% = 1%
                if came_from_find_inv% = 1% then L12135
                    nextinvoicekey$ = vencode$
L12135:         came_from_find_inv% = 0%
                init(" ") line$(), linea$(), lineb$()
                init(" ") ledgerlinekey$()

L12170: REM Get next invoice and any checks paying it...
                gosub read_next_invoice  /* GET INVOICE                */
                if f1%(5) = 0 then exit_display_invoice
                   gosub format_display_invoice      /* INVOICE FORMAT */
                   goto  L12170           /* GET NEXT INVOICE           */

        exit_display_invoice
        REM If more invoice to be displayed, do so...
                if line% = 0 then gosub L12300  /* SHOW NO INV'S */
                   line% = 1000
                   gosub control_display_invoice     /* DISPLAY OUTPUT */
                   goto display_check

L12300: REM If LINE% = 0, tell user "NO INVOICES"...
                    init(" ") line$(), linea$(), lineb$()
                    message$ = "NO INVOICES"
                    str(line$(8), 40-len(message$)/2) = message$
                    str(lineb$(4), 40-len(message$)/2) = message$
                    return

        REM *************************************************************~
            * S C R E E N   F O R M A T   I N V O I C E   E N T R I E S *~
            *                                                           *~
            * PRINTS SCREEN ENTRIES FOR THE VENDOR.    NOTE THAT SINCE  *~
            * WE'RE DOING A PLOW ROUTINE WITHIN A PLOW ROUTINE (CHECKS) *~
            * WE WILL USE THE MULTI COLUMN PRINT APPROACH TO HANDLE THIS*~
            *************************************************************

        format_display_invoice
            counter% = 0                 /* COUNTER FOR PAYING CHECKS  */
            mat linenumber% = con        /* LINE COUNTERS FOR NEXT LINE*/
            colsdone% = 0

L13065:     line$ = "                       !          !           !     ~
        ~   !           ! "
            linea$, lineb$ = line$
            for column% = 1 to 2         /* FOR EACH COLUMN            */
                on column% gosub L13135, L13365
                next column%
            if colsdone% >= 2 then return
               gosub control_display_invoice     /* DISPLAY OUTPUT */
               line$(line%) = line$
                   linea$(line%) = linea$
                   lineb$(line%) = lineb$
               ledgerlinekey$(line%+4) = nextinvoicekey$
               goto L13065

L13135: REM Set up Invoice #, Date, Amount, Balance, Currency Codes,     ~
            Currency Descriptions, Gain/Loss Account and Description...

                    on linenumber%(1) gosub L13155, L13260, L13340
                    return

L13155:             REM Line 1...
                        put line$, using L13575,                          ~
                            invoicenr$, invoicedate$, invamt
                            if holdflag$="Y"then str(line$,18,6)="[Hold]"
                        put str(line$,69,11), using L13590,               ~
                            balance

                    if foriegn_curr$ = "Y" then L13205
                    linea$ = line$
                    str(lineb$,2,4) = stat_curr_code$
                    str(lineb$,7,28) = stat_curr_desc$
                    goto L13245

L13205:                 put linea$, using L13575,                         ~
                            invoicenr$, invoicedate$, tran_inv_amt
                        if holdflag$="Y"then str(linea$,18,6)="[Hold]"
                        if pay_flag$ = "N" then                          ~
                            put str(linea$,69,11), using L13590,          ~
                                    tran_balance                         ~
                        else put str(linea$,69,11), using L13590,         ~
                                    tran_inv_amt
                        str(lineb$,2,4) = tran_curr_code$
                        str(lineb$,7,28) = tran_curr_desc$
*                      IF G_L_STK(COUNTER%) <> 0 THEN 13236 ELSE 13245
*                      IF GAIN_LOSS_AMT <> 0 THEN 13236 ELSE 13245
*                          STR(LINEB$,36,14) = "Gain/Loss Acct"
*                          STR(LINEB$,51,12) = GAIN_LOSS_ACCT$
*                          STR(LINEB$,51,12) = G_L_STK$(COUNTER%)
*                          STR(LINEB$,64,5) = "G/L ="
*                          CALL "CONVERT" (G_L_STK(COUNTER%), -2.2,     ~
*                                          STR(LINEB$,70%,10%))
*                          STR(LINEB$,70,10) = GAIN_LOSS_AMT$

L13245:             linenumber%(1) = 2
                    return

L13260:             REM Line 2...
                        if invoicedate$ = dateposted$ and receiver$ = " "~
                           then L13340
                        if invoicedate$ = dateposted$                    ~
                           then dateposted$ = " "                        ~
                           else dateposted$ = "(" & dateposted$ & ")"
                        if receiver$ > " "                               ~
                           then receiver$ = "(" & receiver$ & ")"
                        put line$, using L13580,                          ~
                            receiver$, dateposted$
                        put linea$, using L13580,                         ~
                            receiver$, dateposted$
                        linenumber%(1) = 3
                        return

L13340:             REM Line 3...
                        colsdone% = colsdone% + 1
                        linenumber%(1) = 4
                        return

L13365: REM Set up Invoice payment amount & form (CHECK, CASH, NONE)...

                    on linenumber%(2) gosub L13385, L13550
                    return

L13385:         REM Set up Next Check Detail for Invoice...
                        if payaccttype$ <> "L" and                       ~
                           payaccttype$ <> "M" then L13500
                        if max_check_counter% = 0 then L13455
                        counter% = counter% + 1
                        put str(line$,48,21), using L13585,               ~
                            checkstk$(counter%), checkstk(counter%)
                    if foriegn_curr$ = "Y" then                          ~
                        put str(linea$,48,21), using L13585,              ~
                            checkstk$(counter%), tran_pmt_amt(counter%)  ~
                    else put str(linea$,48,21), using L13585,             ~
                            checkstk$(counter%), checkstk(counter%)
                    if foriegn_curr$ <> "Y" then L13435
                    if g_l_stk(counter%) = 0 then L13435
                          str(lineb$,36,14) = "Gain/Loss Acct"
                          str(lineb$,51,12) = g_l_stk$(counter%)
                          str(lineb$,64,5) = "G/L ="
                          call "CONVERT" (g_l_stk(counter%), -2.2,       ~
                                          str(lineb$,70%,10%))

L13435:             if counter% < max_check_counter% then return
                        linenumber%(2) = 2
                        return

L13455:         REM Set "NONE" as the check paying Invoice...
                        put str(line$,48,21), using L13585,               ~
                            "NONE", " "
                        put str(linea$,48,21), using L13585,              ~
                            "NONE", " "
                        linenumber%(2) = 2
                        return

L13500:         REM If Invoice's Rec Acct Type <> "A" then it's a "CASH" ~
                    Invoice without checks...
                        put str(line$,48,21), using L13585,               ~
                            "CASH", " "
                        put str(linea$,48,21), using L13585,              ~
                            "CASH", " "
                        linenumber%(2) = 2
                        return

L13550:         REM Zap out everything and pass...
                        linenumber%(2) = 3
                        colsdone% = colsdone% + 1
                        return

L13575: %#######################! ######## !-#######.##!
L13580: %     ##################!##########!           !
L13585: %########!-#######.##!
L13590: %-#######.##

        REM *************************************************************~
            *                 C H E C K   D I S P L A Y                 *~
            *                                                           *~
            * DISPLAY CHECKS FOR THIS VENDOR   SHOWING WHICH INVOICES   *~
            * THEY PAY OFF.  THIS FUNCTION IS REALLY SIMPLE IN THAT ALL *~
            * WE HAVE TO DO IS SHOW THE CHECK AND ITS DETAILS.          *~
            *************************************************************

        display_check
        REM Set key for displaying this Vendor's checks...
                line% = 0
                nextcheckkey$ = vencode$
                init(" ") line$(), linea$(), lineb$()
                checkmessage$ = " "
                if curr$ = "N" then mode% = 1% else mode% = 3%

L14160: REM Get the next check and display it...
                gosub read_next_check    /* LOAD UP THE CHECK HEADER   */
                if f1%(7) = 0 then exit_display_check
                gosub format_display_check      /* FORMAT CHECK        */
                goto  L14160

        exit_display_check
        REM Display any checks remaining in LINEA$(), LINEB$()...
                if line% = 0 then gosub L14290
                   line% = 1000
                   gosub control_display_check       /* DISPLAY OUTPUT */
                   goto display_summary_ledger

L14290: REM If LINE% = 0 Tell user "NO CHECKS"...
                    init(" ") line$(), linea$(), lineb$()
                    message$ = "NO CHECKS"
                    str(line$(8),40-len(message$)/2) = message$
                    str(linea$(8),40-len(message$)/2) = message$
                    return

        REM *************************************************************~
            *   S C R E E N   F O R M A T   C H E C K   E N T R I E S   *~
            *                                                           *~
            * SCREEN FORMATS THE CHECK ENTRIES THAT WE HAVE PLOWED DOWN *~
            * THE CHECK FILE TO GET.  THIS IS VERY SIMPLE FORMAT, AS    *~
            * AS THERE IS NO CROSS-CHECKING.                            *~
            *************************************************************

        format_display_check
            mat linenumber% = con
            colsdone% = 0
            flag1% = 0                   /* NUMBER OF INVOICES PAID    */


L15130:     line$ = "        !          !           !          !        !~
        ~            !  !           "
            linea$, lineb$ = line$
            flag% = 0                    /* ANYTHING PRINTED           */

            for column% = 1 to 2
                on column% gosub L15260, L15520
            next column%

                if colsdone% >= 2 then return
                if flag% = 0 then return           /* NOTHING TO PRINT */
                   gosub control_display_check     /* DISPLAY OUTPUT   */
                   line$(line%) = line$
                   linea$(line%) = linea$
                   ledgerlinekey$(line% + 4) = nextcheckkey$
                   goto L15130

L15260: REM Set up Check #, Date Posted, Net and Discount Amount...
                on linenumber%(1) gosub L15300, L15380, L15470
                return

L15300:         REM Check number, date of check, net & discount...
                    str(prtdate$,2) = checkdate$
                    put line$, using L15880,                              ~
                        checknumber$, prtdate$, netcheck, discount, " "
                if foriegn_curr$ = "Y" then                              ~
                    put linea$, using L15880,                             ~
                        checknumber$, prtdate$, tran_net, tran_disc, " " ~
                else linea$ = line$
                    flag% = 1
                    linenumber%(1) = 2
                    return

L15380:         REM Set up Date Posted if different than Check Date...
                    if dateposted$ = checkdate$ then L15470
                       dateposted$ = "(" & dateposted$ & ")"
                       put line$, using L15880,                           ~
                           " ", dateposted$, " ", " ", str(invoice$,9,8)
                       put linea$, using L15880,                          ~
                           " ", dateposted$, " ", " ", str(invoice$,9,8)
                       linenumber%(1) = 3
                       flag% = 1
                       return

L15470:         REM Blank when done...
                    linenumber%(1) = 4
                    colsdone% = colsdone% + 1
                    return

L15520: REM Set up Invoice #, G/L Acct, Direct?, Currency Code and detail~
            disbursement amount...
                on linenumber%(2) gosub L15560, L15730
                return

L15560:         REM Set up details one invoice at a time...
                    gosub read_next_invoice_paid   /* GET INVOICE PAID */
                    if f1%(8) = 0 then L15730
                       flag% = 1               /* COUNTS LINES PRINTED */
                       flag1% = flag1% + 1
                       if str(onfile$,,2%) = "NO" then L15660
                       put str(line$,43), using L15890,                   ~
                       str(invoice$,1,8), xixacct$, prtaccttype$, amount
                    if foriegn_curr$ = "Y" then                          ~
                       put str(linea$,43), using L15890,                  ~
                       str(invoice$,1,8), tran_curr_code$, prtaccttype$, ~
                          tran_pmt else                                  ~
                       put str(linea$,43), using L15890,                  ~
                       str(invoice$,1,8), stat_curr_code$, prtaccttype$, ~
                          amount
                       return

L15660:         REM Invoice not on file...
                        put str(line$,43), using L15900,                  ~
                        str(invoice$,1,8), xixacct$, prtaccttype$, amount
                    if foriegn_curr$ = "Y" then                          ~
                        put str(linea$,43), using L15900,                 ~
                        str(invoice$,1,8),tran_curr_code$, prtaccttype$, ~
                            tran_pmt else                                ~
                        put str(linea$,43), using L15900,                 ~
                        str(invoice$,1,8),stat_curr_code$,prtaccttype$,  ~
                            amount
                        checkmessage$ =                                  ~
                        "Invoices marked with a star (*) are not on file"
                        return

L15730:         REM Zap when done...
                    if flag1% = 0 then L15790   /* NO DETAILS PRINTED   */
                    linenumber%(2) = 3
                    colsdone% = colsdone% + 1
                    return

L15790:         REM When there are no details...
                    colsdone% = 2        /* AS THE CRITERIA FOR PURGING*/
                                         /* A CHECK IS THAT IT HAS NO  */
                                         /* LINE ITEMS, THIS IS A CHECK*/
                                         /* WHICH SHOULD NOT BE ON THE */
                                         /* FILE AT ALL. THEREFORE, WE */
                                         /* SUPPRESS ITS PRINTING HERE.*/
                    return

L15880: %########!##########!-#######.##!-######.##!########
L15890: %!########!############! #!-#######.##
L15900: %*########!############! #!-#######.##

        REM *************************************************************~
            *   D I S P L A Y   S U M M A R Y   I N F O R M A T I O N   *~
            *                                                           *~
            *************************************************************

        display_summary_ledger
        REM Compute and Display summary information...
L16120:     gosub compute_summary_total

L16140:     gosub summary_screen         /* DISPLAY TOTALS.            */
                  if keyhit%  =  0 then return
                  if keyhit%  =  2 then display_invoice
                  if keyhit%  =  3 then display_check
                  if keyhit%  =  5 then return
                  if keyhit%  = 14 then L16230
                  if keyhit%  = 16 then L16290
                     goto L16140

L16230: REM Branch to print individual ledger...
                gosub hold_ledger_pointers
                gosub print_individual_ledger
                close printer
                pagenumber% = 0%
                gosub restore_ledger_pointers
                goto L16140

L16290: REM Branch to control screen or aging screen...
                if mode% = 2% or mode% = 4% then return
                gosub hold_ledger_pointers
                gosub main_screen
                gosub restore_ledger_pointers
                goto L16120

        REM *************************************************************~
            *      P R I N T   R E C E I V A B L E S    R E P O R T     *~
            *                                                           *~
            * PRINTS LEDGER CARDS, DETAILED AGING SCHEDULE, OR SUMMARY  *~
            * AGING SCHEDULE, DEPENDING ON THE VALUE OF REPORTTYPE$,    *~
            * DETERMINED BELOW.                                         *~
            *************************************************************

        print_ledger
                call "SETPRNT" ("VEN001", " ", 0%, 0%)

L17100:         gosub read_next_vendor
                if f1%(3) = 0 then L17140
                   gosub print_individual_ledger
                   goto L17100
L17140:         close printer
                pagenumber% = 0%
                return

        REM *************************************************************~
            *      P R I N T    I N D I V I D U A L    L E D G E R      *~
            *                                                           *~
            * THIS ROUTINE REALLY IS JUST A LABEL, TO MAKE BRANCHES     *~
            * FROM VARIOUS DISPLAYS (AGINGS, ETC) EASIER TO READ.       *~
            *************************************************************

        print_individual_ledger
        REM  Print Invoice, Print Check, and Print Summary Ledger...

             message$ = "Printing Full Payables Report for:"
                       str(message$, 36) = venname$
                       call "SHOSTAT" (message$)
                       goto print_invoice

        REM *************************************************************~
            *    P R I N T   R O U T I N E   F O R   I N V O I C E S    *~
            *                                                           *~
            * PRINTS THE INVOICES FOR CURRENT VENDOR.                   *~
            *************************************************************

        print_invoice
        REM Set first key for invoices and checks...
                invoice% = 0
                line% = 1000
                nextinvoicekey$ = vencode$
                mat aging = zer
                mat suminvoices = zer
                sumapplied, sumgainloss = 0
                mat sumapplied = zer
                mat sumunapplied = zer
                cashinvoices, sumdirect, sumunapplied, sumapplpurged = 0

        REM Get next invoice and any checks paying it...
L19160:         gosub read_next_invoice  /* GET INVOICE                */
                if f1%(5) = 0 then exit_print_invoice
                   gosub aging                     /* ADD TO TOTALS    */
                   gosub control_print_invoice
                   if payaccttype$ <> "L"                                ~
                   and payaccttype$ <> "M" then L19390
                   if max_check_counter% = 0% then L19330
                   counter% = 1%
                if curr$ = "Y" then                                      ~
                   print using L28365, invoicenr$, receiver$,             ~
                         invoicedate$, dateposted$, xixacct$, invamt,    ~
                         checkstk$(1%), checkstk(1%), g_l_stk(1%),       ~
                         balance                                         ~
                else                                                     ~
                   print using L28395, invoicenr$, receiver$,             ~
                         invoicedate$, dateposted$, xixacct$, invamt,    ~
                         checkstk$(1%), checkstk(1%), balance

L19260:            if counter% = max_check_counter% then L19160
                      gosub control_print_invoice
                      counter% = counter% + 1%

                   if curr$ = "Y" then                                   ~
                      print using L28365, " ", " ", " ", " ", " ", " ",   ~
                            checkstk$(counter%), checkstk(counter%),     ~
                            g_l_stk(counter%), " "                       ~
                   else                                                  ~
                      print using L28395, " ", " ", " ", " ", " ", " ",   ~
                            checkstk$(counter%), checkstk(counter%), " "
                      goto L19260

L19330: REM No checks for this invoice...
               if holdflag$ = "Y" then receiver$ = "[Hold]"
                   if curr$ = "Y" then                                   ~
                       print using L28365, invoicenr$, receiver$,         ~
                             invoicedate$, dateposted$, xixacct$, invamt,~
                             "NONE", " ", " ", balance                   ~
                   else                                                  ~
                       print using L28395, invoicenr$, receiver$,         ~
                             invoicedate$, dateposted$, xixacct$, invamt,~
                             "NONE", " ", balance
                       goto L19160

L19390: REM Cash invoice...
                   if curr$ = "Y" then                                   ~
                       print using L28365, invoicenr$, receiver$,         ~
                             invoicedate$, dateposted$, xixacct$, invamt,~
                             "CASH", " ", " ", balance                   ~
                   else                                                  ~
                       print using L28395, invoicenr$, receiver$,         ~
                             invoicedate$, dateposted$, xixacct$, invamt,~
                             "CASH", " ", balance
                       goto L19160

        exit_print_invoice
        REM Print separator line if necessary...
             if invoice% = 0    then L19520  /* ANY INVOICES PRINTED?   */
                   line% = line% + 1
                   print using L28330     /* CAP LINE                   */
                   goto print_check

L19520:         REM IF INV % = 0, THEN TELL USER "NO INVOICES"
             gosub new_page

                    init(" ") line$(), linea$(), lineb$()
                    line% = line% + 6
                    print skip (3)
                    print tab (58);"* * * * * * * *"
                    print tab (58);"* NO INVOICES *"
                    print tab (58);"* * * * * * * *"
                    goto print_check

        REM *************************************************************~
            *     P R I N T    R O U T I N E    F O R    C H E C K S    *~
            *                                                           *~
            * PRINTS CHECKS FOR THIS VENDOR   SHOWING WHICH INVOICES    *~
            * THEY PAY OFF.                                             *~
            *************************************************************

        print_check
        REM Set keys for printing checks...
                check% = 0
                nextcheckkey$ = vencode$

        REM Get next check and print it...
L20130:         gosub read_next_check    /* LOAD UP THE CHECK HEADER   */
                if f1%(7) = 0 then exit_print_check

                REM First line...
                   gosub read_next_invoice_paid    /* GET FIRST DETAIL */
                         if f1%(8) = 0 then L20130  /* PAYS NO INVOICES!*/

                         n%=1
                         if payaccttype$="M" then n%=2%

                   sumapplied = round(sumapplied + applied,2%)
                   sumunapplied = round(sumunapplied+unapplied,2%)
                   sumapplpurged = round(sumapplpurged + pgapplied, 2%)
                   sumapplied(n%) = round(sumapplied(n%)+applied,2%)
                   sumunapplied(n%) = round(sumunapplied(n%)+unapplied,2)
                   sumdirect = round(sumdirect + direct, 2%)
                         if check% = 0 then gosub check_heading
                         gosub control_print_check
                         print using L28428, checknumber$, checkdate$,    ~
                               dateposted$, netcheck, discount, invoice$,~
                               xixacct$, prtaccttype$, amount, onfile$

                REM Subsequent lines...
L20290:                  gosub read_next_invoice_paid  /* NEXT INVOICES*/
                               if f1%(8) = 0 then L20130  /*NO MORE INV.*/
                                  gosub control_print_check
                                  n%=1
                              if payaccttype$="M" then n%=2%
                    sumapplied = round(sumapplied + applied,2%)
                    sumunapplied = round(sumunapplied+unapplied,2%)
                    sumapplpurged = round(sumapplpurged + pgapplied, 2%)
                    sumapplied(n%)=round(sumapplied(n%)+applied, 2%)
                    sumunapplied(n%)=round(sumunapplied(n%)+unapplied,2)
                    sumdirect = round(sumdirect + direct, 2%)
                                  print using L28428, " ", " ", " ", " ", ~
                                        " ", invoice$, xixacct$,         ~
                                        prtaccttype$, amount, onfile$
                                  goto L20290       /* GET NEXT INVOICE */

        exit_print_check
        REM Print separator line if necessary...
                if check% = 0 then L20470 /* ANY CHECKS PRINTED?        */
                   line% = line% + 1
                   print using L28330     /* CAP LINE                   */
                   goto print_summary_ledger

L20470:         REM If CHECK% = 0, tell user "NO CHECKS"...
                    init(" ") line$(), linea$(), lineb$()
                    if line% > 57 then gosub new_page    /* MAKE ROOM  */
                    line% = line% + 6
                    print skip (3)
                    print tab (58);"* * * * * * * *"
                    print tab (58);"*  NO CHECKS  *"
                    print tab (58);"* * * * * * * *"
                    goto print_summary_ledger

        REM *************************************************************~
            *     P R I N T   S U M M A R Y   I N F O R M A T I O N     *~
            *                                                           *~
            * JUST DOES THE NECESSARY OUTPUT, AS THE TOTALS WERE        *~
            * COMPUTED WHILE PRINTING INVOICES AND CHECKS.              *~
            *************************************************************

        print_summary_ledger
           REM Get enough room to print summary on one page...
               if line% < 49 then L21130            /* PLENTY OF ROOM   */
                  line% = 1000
                  gosub new_page

L21130:        print skip (2)
               gosub summary_total_manipulation
               if curr$ = "N" then L21245
           REM Transaction Currency...
               sumbalance(1) = round(sumbalance(1) + sumgainloss, 2%)
               print using L21368
               print using L21376, "INVOICES", suminvoices(1),            ~
                                  "INVOICES", suminvoices(2),            ~
                                  "CASH INVOICES", cashinvoices
               print using L21376, "APPLIED CHECKS    (-)", sumapplied(1),~
                                  "APPLIED CHECKS", sumapplied(2),       ~
                                  "DIRECT CHECKS", sumdirect
               print using L21376, "UNAPPLIED CHECKS  (-)",               ~
                                   sumunapplied(1),                      ~
                                  "UNAPPLIED CHECKS", sumunapplied(2)
               print using L21379, "REALIZED GAIN/LOSS(+)", sumgainloss,  ~
                                  "----CHECK APPLIED TO PURGED INV----"
               print using L21376, " ", "================",               ~
                                  " ", "================",               ~
                                  "APPLIED TO PURGED INV", sumapplpurged
               print using L21376, "RECONCILLIATION", sumbalance(1),      ~
                                  "BALANCE", sumbalance(2)
               print using L21376, " ", "==============",                 ~
                                  " ", "=============="
               goto L21261
           REM Statutory Currency...
L21245:        print using L21388
               print using L21396, "INVOICES", suminvoices(1),            ~
                                  "INVOICES", suminvoices(2),            ~
                                  "CASH INVOICES", cashinvoices
               print using L21396, "APPLIED CHECKS", sumapplied(1),       ~
                                  "APPLIED CHECKS", sumapplied(2),       ~
                                  "DIRECT CHECKS", sumdirect
               print using L21396, "UNAPPLIED CHECKS", sumunapplied(1),   ~
                                  "UNAPPLIED CHECKS", sumunapplied(2)
               print using L21396, " ", "================",               ~
                                  " ", "================"
               print using L21396, "BALANCE", sumbalance(1),              ~
                                  "BALANCE", sumbalance(2)
               print using L21396, " ", "==============",                 ~
                                  " ", "=============="

L21261:        print
               print tab (24); "AGING SCHEDULE AS OF ";screendate$
               print using L21410
               print using L21450, period$(1), period$(2), period$(3),    ~
                     period$(4), period$(5)
               print using L21410
               print using L21430, aging(1), aging(2), aging(3), aging(4),~
                     aging(5)
               print using L21410
               return          /* END OF VENDOR PRINT ROUTINE        */

        REM Transaction Currency...
L21368: %       ---------- PAYABLES SUMMARY ---------     ------- MISC LI~
        ~ABILITIES --------       --CASH INVOICES AND DIRECT CHECKS--

L21376: %       #####################  -##########.##     ###############~
        ~### -##########.##       #####################-##########.##

L21379: %       #####################  -##########.##                    ~
        ~                         ###################################

        REM Statutory Currency...
L21388: %         -------- PAYABLES SUMMARY -------       ------- MISC LI~
        ~ABILITIES --------       --CASH INVOICES AND DIRECT CHECKS--

L21396: %         #################  -##########.##       ###############~
        ~##  -##########.##       ###################  -##########.##

L21410: %                       +----------------+----------------+------~
        ~----------+----------------+----------------+
L21430: %                       ! -##########.## ! -##########.## ! -####~
        ~######.## ! -##########.## ! -##########.## !
L21450: %                       !   ##########   !   ##########   !   ###~
        ~#######   !   ##########   !   ##########   !

        REM *************************************************************~
            *         D E T A I L    A G I N G    D I S P L A Y         *~
            *                                                           *~
            * THIS ROUTINE DISPLAYS ALL OF THE OUTSTANDING INVOICES FOR *~
            * EACH VENDOR,    AND ALSO TOTALS UP ANY UNAPPLIED CHECKS,  *~
            * THUS SHOWING THE TOTAL OUTSTANDING BALANCE.               *~
            *************************************************************

        display_detail_aging
            line% = 0                    /* COUNTER FOR LINE           */
            init(" ") line$(), linea$(), lineb$(), aginglinekey$()

        REM Set Mode for branch to Ledger Display...
            if curr$ = "N" then mode% = 2% else mode% = 4%

            venline% = 1                 /* LINE WITH VENDOR NAME      */
            mat total = zer              /* FINAL REPORT TOTALS        */
            wherefrom$ = "'DETAILED AGING SCHEDULE'"

L22080:     gosub read_next_vendor
                  if f1%(3) = 0 then exit_display_detail_aging

            REM Initialize flags, counters, etc...
                screenvencode$ = vencode$/* FOR MAIN SCREEN            */
                mat aging = zer          /* TOTAL FOR THIS VENDOR      */
                nextinvoicekey$ = vencode$
                flag% = 0                /* COUNTS LINES PRINTED       */
                if line% = 0 then L22140  /* SUPPRESS ON VERY 1ST SCREEN*/
                gosub control_display_detail_aging           /* OUTPUT */
                         line$( line% ) = "--------+----------+----------~
        ~-+-----------+-----------+-----------+-----------"
L22140:         gosub control_display_detail_aging           /* OUTPUT */
                put line$(line%), using L22510, vencode$, venname$, phone$
                aginglinekey$(line% + 4) = vencode$
                venline% = line%

            REM Read Invoices...
L22170:         gosub read_next_invoice
                      if f1%(5) = 0 then L22240
                      if balance = 0 then L22170 /* DON'T PRINT IF PAID */
                      gosub aging        /* ADD TO TOTAL AND GET LOC.% */
                      gosub control_display_detail_aging     /* OUTPUT */
                      put line$(line%), using L22465,                     ~
                          invoicenr$, invoicedate$
                      call "CONVERT" (balance, 2.2,                      ~
                               str(line$(line%), location% * 12 + 9, 11))
                      aginglinekey$(line% + 4) = nextinvoicekey$
                      flag% = flag% + 1  /* COUNT LINES PRINTED        */
                      goto L22170         /* GET NEXT INVOICE           */

            REM Read Checks...
L22240:         gosub compute_summary_checks
                      if sumunapplied = 0 then L22275 /*UNAPPLIED CHECKS*/
                      gosub control_display_detail_aging     /* OUTPUT */
                      put line$(line%), using L22480, sumunapplied
                      flag% = flag% + 1  /* COUNT LINES PRINTED        */
                      aginglinekey$(line%+4) = vencode$

L22275:      REM Display total only if necessary (more than 1 line)...

             for v% = 1% to 5%
                total(v%) = round(total(v%) + aging(v%), 2%)
             next v%

                if flag% < 2 then L22080
                sumbalance = round(aging(1) + aging(2) + aging(3)        ~
                           + aging(4) + aging(5), 2%)
                prt$() = " "             /* TO SUPPRESS "0.00" DISPLAY */
                for temp% = 1 to 5       /* WE PRINT TO PRT$()         */
                    if aging(temp%) <> 0 then call "CONVERT"             ~
                                (aging(temp%), 2.2, str(prt$(temp%),,10))
                next temp%
                gosub control_display_detail_aging    /* OUTPUT */
                put line$(line%), using L22490, sumbalance, prt$(1),      ~
                    prt$(2), prt$(3), prt$(4), prt$(5)
                    aginglinekey$(line%+4) = vencode$
                    goto L22080

        exit_display_detail_aging
                if line% = 0 then L22430            /* NOTHING PRINTED  */
                gosub control_display_detail_aging
                         line$( line% ) = "--------+----------+----------~
        ~-+-----------+-----------+-----------+-----------"
                gosub control_display_detail_aging
           total = round(total(1)+total(2)+total(3)+total(4)+total(5),2)
                put line$(line%), using L22500, total, total(1), total(2),~
                                  total(3), total(4), total(5)
L22405:         line% = 1000
                gosub control_display_detail_aging
                kh% = 2%
                call "ASKUSER" (kh%, " ***  END OF REPORT  *** ",        ~
                     "Press PF 1 to go back to Detail Aging Display",    ~
                     "Press ENTER to return to the Menu",                ~
                     " ")
                if kh% = 1% then L22405
                wherefrom$ = " "                   /* REPORT COMPLETED */
                return

L22430:         REM Tell user "NO DATA"...
                    str(line$(8), 32, 16) = "NOTHING ON FILE"
                    line% = 1000
                    gosub control_display_detail_aging
                    wherefrom$ = " "               /* REPORT COMPLETED */
                    return

L22465: %########! ######## !           !           !           !        ~
        ~   !
L22480: %UNAPPLIED CHECKS   !-#######.##!           !           !        ~
        ~   !
L22490: %TOTAL  -########.##!-#######.##!-#######.##!-#######.##!-#######~
        ~.##!-#######.##
L22500: %*FINAL*-########.##!-#######.##!-#######.##!-#######.##!-#######~
        ~.##!-#######.##
L22510: %CODE....! #########!NAME ##############################!  PHONE ~
        ~##############

        REM *************************************************************~
            *           D E T A I L    A G I N G    P R I N T           *~
            *                                                           *~
            * THIS ROUTINE PRINTS ALL OF THE OUTSTANDING INVOICES FOR   *~
            * EACH VENDOR,    AND ALSO TOTALS UP ANY UNAPPLIED CHECKS,  *~
            * THUS SHOWING THE TOTAL OUTSTANDING BALANCE.               *~
            *************************************************************

        print_detail_aging
            line% = 1000                 /* COUNTER FOR LINE           */
            mat total = zer              /* FINAL REPORT TOTALS        */
            tagline% = 1                 /* SIGNALS DELIMITER LINE     */
            call "SETPRNT" ("A/P002", " ", 0%, 0%)
            pagenumber% = 0%

L23060:     kh% = 2%  :  zflag$ = "N"
            call "ASKUSER" (kh%, "*** INCLUDE ZERO BALANCES ? ***",      ~
                 "Press RETURN to Include Zero Balance Invoices -OR-",   ~
                 "PF16 to Omit.", "PF1 to abort Print.")
            if kh% = 0% then L23067
                if kh% = 16% then L23068
                    if kh% = 1% then return else goto L23060
L23067:     zflag$ = "Y"
L23068:     call "SHOSTAT" ("Printing Detailed Aging Schedule")

L23070:     gosub read_next_vendor
                  if f1%(3) = 0 then exit_print_detail_aging

            REM Initialize flags, counters, etc...
                mat aging = zer          /* TOTAL FOR THIS VENDOR      */
                nextinvoicekey$ = vencode$
                flag% = 0                /* COUNTS LINES PRINTED       */
                infoline% = 0     /* COUNTS INFO LINES TO BE PRINTED   */
                prtvencode$ = vencode$   /* SET TO " " AFTER 1ST PRINT */
                veninfo$(1) = venname$
                veninfo$(2) = phone$
                if creditlimit = 0 then veninfo$(3) = " "                ~
                else put veninfo$(3), using L23140, ":", creditlimit
L23140:                  %CREDIT LIMIT#      -#######.##

            REM Read Invoices...
L23155:         gosub read_next_invoice
                      if f1%(5) = 0 then L23240
                      if zflag$ = "N" and balance = 0 then L23155
                      gosub aging        /* ADD TO TOTAL AND GET LOC.% */
                      prt$() = " "
                      call"CONVERT"(balance,2.2,str(prt$(location%),,10))
                      infoline% = min(infoline% + 1, 4)
                      gosub control_print_detail_aging       /* OUTPUT */
                      print using L28710, prtvencode$,veninfo$(infoline%),~
                          invoicenr$,      invoicedate$, receiver$,      ~
                          prt$(1), prt$(2), prt$(3), prt$(4), prt$(5)
                      prtvencode$ = " "
                      flag% = flag% + 1  /* COUNT LINES PRINTED        */
                      goto L23155         /* GET NEXT INVOICE           */

            REM Read Checks...
L23240:         gosub compute_summary_checks
                      if sumunapplied = 0 then L23285 /*UNAPPLIED CHECKS*/
                      infoline% = min(infoline% + 1, 4)
                      gosub control_print_detail_aging     /* OUTPUT */
                      print using L28710, prtvencode$,veninfo$(infoline%),~
                      " ", " ", "UNAPPLIED CHECKS", sumunapplied, " ",   ~
                      " ", " ", " "
                      flag% = flag% + 1  /* COUNT LINES PRINTED        */

L23285:     REM Print total only if necessary (more than one line)...
                if flag% = 0 then L23070            /* NOTHING PRINTED  */

             for v% = 1% to 5%
                total(v%) = round(total(v%) + aging(v%), 2%)
             next v%

                REM Print more Vendor information if necessary...
L23305:             infoline% = min(infoline% + 1, 4)
                    if veninfo$(infoline%) = " " then L23330
                       gosub control_print_detail_aging
                       print using L28710, " ", veninfo$(infoline%),      ~
                             " ", " ", " ", " ", " ", " ", " ", " "
                       goto L23305        /* ANY MORE INFO?             */
L23330:         if flag% < 1 then L23395        /* > 0 LINE PRINTED?*/
                total = round(aging(1) + aging(2) + aging(3)             ~
                      + aging(4) + aging(5), 2%)
                prt$() = " "

                for temp% = 1 to 5       /* CONVERT AGING() TO AN ALPHA*/
                    if aging(temp%) <> 0 then call "CONVERT"             ~
                                (aging(temp%), 2.2, str(prt$(temp%),,10))
                next temp%

                gosub control_print_detail_aging
                print using L28730, prtvencode$, total, prt$(1), prt$(2), ~
                      prt$(3), prt$(4), prt$(5)
L23395:         print using L28690        /* DELIMITER LINE             */
                tagline% = 1
                line% = line% + 1
                goto L23070     /* LOAD UP NEXT VENDOR                  */

        exit_print_detail_aging
                   total = round(total(1) + total(2) + total(3)          ~
                               + total(4) + total(5), 2%)
            prt$() = " "

            for temp% = 1 to 5
                if total(temp%) <> 0 then                                ~
                   call "CONVERT" (total(temp%),2.2,str(prt$(temp%),,10))
            next temp%

            gosub control_print_detail_aging
            print using L28750, total, prt$(1), prt$(2), prt$(3),         ~
                                      prt$(4), prt$(5)
            if total = 0 then L23500      /* AVOID DIVIDE BY 0          */

            for temp% = 1 to 5
                total(temp%) = total(temp%) / total * 100  + .005
            next temp%

            print using L28770, total(1), total(2), total(3), total(4),   ~
                               total(5)   /* PERCENT OF TOTAL          */
L23500:     print using L28330             /* DELIMITER LINE            */
            close printer
            pagenumber% = 0%
            return

        REM *************************************************************~
            *        S U M M A R Y    A G I N G    D I S P L A Y        *~
            *                                                           *~
            * THIS ROUTINE DISPLAYS AN AGED TOTAL OUTSTANDING BALANCE   *~
            * FOR EACH VENDOR.   THE BALANCE CONSISTS OF OUTSTANDING    *~
            * INVOICES AND UNAPPLIED CREDITS.                           *~
            *************************************************************

        display_summary_aging
            line% = -2                   /* COUNTER FOR LINE           */
            init(" ") line$(), linea$(), lineb$(), aginglinekey$()

        REM Set mode for branch to Ledger Display...
            if curr$ = "N" then mode% = 2% else mode% = 4%

            mat total = zer              /* FINAL REPORT TOTALS        */
            wherefrom$ = "'SUMMARY AGING SCHEDULE'"

L24150:     gosub read_next_vendor
                  if f1%(3) = 0 then exit_display_summary_aging
            gosub compute_summary_total
                  if sumbalance = 0 then L24150
            gosub control_display_summary_aging             /* OUTPUT */
            put line$(line%), using L24550, vencode$, venname$, phone$
            put line$(line% + 1), using L24530, sumbalance, aging(1),     ~
                                  aging(2), aging(3), aging(4), aging(5)
            line$(line%+2) ="---------------+------------+------------+--~
        ~----------+------------+-----------"

          for v% = 1% to 5%
            total(v%) = round(total(v%) + aging(v%), 2%)
          next v%

            aginglinekey$(line% + 4) = vencode$
            aginglinekey$(line% + 5) = vencode$
            screenvencode$ = vencode$    /* FOR MAIN SCREEN  */
            goto L24150

        exit_display_summary_aging
            if line% = 0 then L24460                /* NOTHING PRINTED  */
            gosub control_display_summary_aging
            line$ (line%)  =" *FINAL TOTAL* !            !            !  ~
        ~          !            !          "
            total = round(total(1)+total(2)+total(3)+total(4)+total(5),2)
            put line$(line% + 1), using L24530, total, total(1), total(2),~
                                  total(3), total(4), total(5)
            line$(line%+2) ="---------------+------------+------------+--~
        ~----------+------------+-----------"
L24410:     line% = 1000
            gosub control_display_summary_aging
            kh% = 2%
            call "ASKUSER" (kh%, " ***  END OF REPORT  *** ",            ~
                 "Press PF 1 to go back to Summary Aging Display",       ~
                 "Press ENTER to return to the Menu",                    ~
                 " ")
            if kh% = 1% then L24410
            wherefrom$ = " "                       /* REPORT COMPLETED */
            return

L24460:     REM Tell user "NO DATA"...
                str(line$(8), 32, 16) = "NOTHING ON FILE"
                line% = 1000
                gosub control_display_summary_aging
                wherefrom$ = " "                   /* REPORT COMPLETED */
                return

L24530: %  -########.## !-#######.## !-#######.## !-#######.## !-#######.~
        ~## !-#######.##
L24550: %CODE ######### !NAME   ############################## !  PHONE  ~
        ~##############

        REM *************************************************************~
            *          S U M M A R Y    A G I N G    P R I N T          *~
            *                                                           *~
            * THIS ROUTINE PRINTS AN AGED TOTAL OUTSTANDING BALANCE     *~
            * FOR EACH VENDOR.   THE BALANCE CONSISTS OF OUTSTANDING    *~
            * INVOICES AND UNAPPLIED CREDITS.                           *~
            *************************************************************

        print_summary_aging
            line% = 1000                 /* COUNTER FOR LINE           */
            mat total = zer              /* FINAL REPORT TOTALS        */
            call "SETPRNT" ("A/P003", " ", 0%, 0%)
            pagenumber% = 0%
            call "SHOSTAT" ("Printing Summary Aging Schedule")

L25130:     gosub read_next_vendor
                  if f1%(3) = 0 then exit_print_summary_aging
            gosub compute_summary_total
            if sumbalance = 0 then L25130
            for v% = 1% to 5%
               total(v%) = round(total(v%) + aging(v%), 2%)
            next v%
               prt$() = " "
               for temp% = 1 to 5       /* CONVERT AGING() TO AN ALPHA*/
                   if aging(temp%) <> 0 then call "CONVERT"              ~
                                (aging(temp%), 2.2, str(prt$(temp%),,10))
                      next temp%
               gosub control_print_summary_aging
               call "CONVERT" (creditlimit, 2.2, creditlimit$)
               call "CONVERT" (availcredit, 2.2, availcredit$)
               sumbalance=round(sumbalance,2%)
               print using L29000, vencode$, venname$, sumbalance,        ~
                     prt$(1), prt$(2), prt$(3), prt$(4), prt$(5)
               goto L25130      /* LOAD UP NEXT VENDOR                  */

        exit_print_summary_aging
            total = round(total(1)+total(2)+total(3)+total(4)+total(5),2)
            prt$() = " "

            for temp% = 1 to 5
                if total(temp%) <> 0 then                                ~
                   call "CONVERT" (total(temp%),2.2,str(prt$(temp%),,10))
            next temp%

            gosub control_print_summary_aging
            print using L28985            /* DELIMITER LINE             */
            print using L29000, " ", "   ***** FINAL  TOTAL *****",       ~
                  total, prt$(1), prt$(2), prt$(3), prt$(4), prt$(5)
            if total = 0 then L25510      /* AVOID DIVIDE BY 0          */

            for temp% = 1 to 5
                total(temp%) = total(temp%) / total * 100  + .005
            next temp%

            print using L29015, total(1), total(2), total(3), total(4),   ~
                               total(5)   /* PERCENT OF TOTAL          */
L25510:     print using L28330             /* DELIMITER LINE            */
            close printer
            pagenumber% = 0%
            return

        REM *************************************************************~
            *              D I S P L A Y    C O N T R O L               *~
            *                                                           *~
            * THESE ROUTINES, TWO FOR LEDGER CARDS (INVOICE DISPLAY     *~
            * CONTROL AND CHECK DISPLAY CONTROL), AND ONE EACH FOR      *~
            * DETAILED AND SUMMARY AGING SCHEDULES, HANDLE THE PAGING   *~
            * FUNCTION FOR THE DISPLAYS AND ALSO, IF REQUESTED, HANDLE  *~
            * BRANCHING TO OTHER FUNCTIONS.                             *~
            *************************************************************

        control_display_invoice
            no_more$ = "N"

            line% = line% + 1
            if line% < 21 then return
L26070:        gosub invoice_screen      /* AND JUST SHOW SCREEN AS IS */
                     if keyhit%  =  0 then       L26185
                     if keyhit%  =  2 then       L26125
                     if keyhit%  =  5 then       L26185
                     if keyhit%  =  8 then       L26215
                     if keyhit%  =  9 then       L26155
                     if keyhit%  = 10 then       find_inv
                     if keyhit%  = 12 then       L26245
                     if keyhit%  = 14 then       L26290
                     if keyhit%  = 16 then       L26320
                        goto L26070

L26125:        REM Branch to first invoice...
                   return clear          /* FOR CONTROL_INVOICE_DISPLAY*/
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_INVOICE_DISPLAY */
                   goto display_invoice  /* BRANCH TO FIRST INVOICE    */

L26155:        REM Branch to first check...
                   return clear          /* FOR CONTROL_INVOICE_DISPLAY*/
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_INVOICE_DISPLAY */
                   goto display_check    /* BRANCH TO FIRST CHECK      */

               REM Next page of invoices...
L26185:            first_screen% = 0%  /* Jeez, another flag!! */
                   if mc_flag1$ = "N" then L26190
                   if no_more$ = "Y" then L26206
L26190:            line% = 1%     /* SET LINE COUNTER           */
                   str(line$(), 1) = " " /*STR(LINE$(),1502)*/
                   str(linea$(), 1) = str(linea$(),791)
                   str(lineb$(), 1) = str(lineb$(),791)
                   str(linea$(), 791) = " "
                   str(lineb$(), 791) = " "
                   str(ledgerlinekey$(),101) = str(ledgerlinekey$(), 576)
                   if mc_flag1$ = "N" then return
                   no_more$ = "Y"
                   goto L26070
L26206:                no_more$ = "N"
                       str(linea$(), 1) = " "
                       str(lineb$(), 1) = " "
                       return

L26215:        REM Branch to summary page...
                   return clear          /* FOR CONTROL_DISPLAY_INVOICE*/
                   if line% < 1000 then                                  ~
                   return clear          /* FOR CONTROL_DISPLAY_INVOICE*/
                   goto display_summary_ledger

L26245:        REM Display entire invoice...
                   gosub get_cursor_position
                   if mc_flag1$ = "Y" then L26266
                   if ledgerlinekey$(cursor%(1)) = " " then L26070
                   gosub hold_ledger_pointers
                   nextinvoicekey$ = ledgerlinekey$(cursor%(1))
                   idx% = (cursor%(1) - 4)
                   gosub display_entire_invoice
                   gosub restore_ledger_pointers
                   goto L26070

              REM Prepare display from 2-line Vendor Ledger Card...
L26266:            idx% = (cursor%(1) + 5) / 2
                   if ledgerlinekey$(idx%) = " " then L26070
                   gosub hold_ledger_pointers
                   nextinvoicekey$ = ledgerlinekey$(idx%)
                   idx% = (idx% - 4)
                   gosub display_entire_invoice
                   gosub restore_ledger_pointers
                   goto L26070

L26290:        REM Branch to individual Ledger card...
                   gosub hold_ledger_pointers
                   gosub print_individual_ledger
                   close printer
                   pagenumber% = 0%
                   gosub restore_ledger_pointers
                   goto L26070

L26320:        REM Branch to main screen or aging display...
L26322:            if mode% = 2% or mode% = 4% then L26322
                   gosub hold_ledger_pointers
                   gosub main_screen
                   gosub restore_ledger_pointers
                   goto L26070

                   return clear          /* FOR CONTROL_DISPLAY_INVOICE*/
                   if line% < 1000 then                                  ~
                   return clear          /* FOR CONTROL_INVOICE_FORMAT */
                   return                /* TO AGING SCHEDULE          */

        find_inv /* Find invoice based on user input */
                   line% = 1% : first_screen% = 0% : findinvoice$ = " "
                   k% = 0%
                   call "ASKSTRNG" (k%," FIND INVOICE NUMBER ",          ~
                        "Enter the Invoice Number you want to search" &  ~
                        " for", "and press RETURN.", "Invoice Number",   ~
                        findinvoice$, 16%)
                   if k% = 1% then L26070
                   if findinvoice$ = " " then L26070
                   findinvoice$ = findinvoice$ addc all(hex(ff))
                   str(nextinvoicekey$,10,16) = str(findinvoice$)
                   gosub read_next_invoice  /* Just checking... */
                       if f1%(5) = 0 then L26070
                   return clear          /* FOR CONTROL_DISPLAY_INVOICE*/
*                 IF LINE% < 1000 THEN                                  ~
*                 RETURN CLEAR          /* FOR CONTROL_INVOICE_FORMAT */
                   came_from_find_inv% = 1%
                   str(nextinvoicekey$,10,16) = str(findinvoice$)
                   goto display_invoice

        control_display_check
            line% = line% + 1
            if line% < 21 then return
L26401:        if curr$ = "N" then mode% = 1% else mode% = 3%
L26405:        gosub check_screen        /* AND JUST SHOW SCREEN AS IS */
                     if keyhit%  =  0 then       L26520
                     if keyhit%  =  2 then       L26490
                     if keyhit%  =  5 then       L26520
                     if keyhit%  =  8 then       L26555
                     if keyhit%  =  9 then       L26460
                     if keyhit%  = 12 then       L26585
                     if keyhit%  = 14 then       L26630
                     if keyhit%  = 16 then       L26660
                        goto L26401

L26460:        REM Branch to first invoice...
                   return clear          /* FOR CONTROL_DISPLAY_CHECK  */
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_DISPLAY_CHECK   */
                   goto display_invoice  /* BRANCH TO FIRST INVOICE    */

L26490:        REM Branch to first check...
                   return clear          /* FOR CONTROL_DISPLAY_CHECK  */
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_DISPLAY_CHECK   */
                   goto display_check    /* BRANCH TO FIRST CHECK      */

L26520:        REM Next page of checks...
                   line% = 6             /* SET LINE COUNTER           */
                   str(line$(), 1) = str(line$(),1186)
                   str(linea$(), 1) = str(linea$(),1186)
                   str(lineb$(), 1) = str(lineb$(),1186)
                   str(ledgerlinekey$(), 69) = str(ledgerlinekey$(), 324)
                   checkmessage$ = " "
                   return

L26555:        REM branch to summary page...
                   return clear          /* FOR CONTROL_DISPLAY_CHECK  */
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_DISPLAY_CHECK   */
                   goto display_summary_ledger

L26585:        REM Branch to display individual check...
                   gosub get_cursor_position
                   if ledgerlinekey$(cursor%(1)) = " " then L26405
                   gosub hold_ledger_pointers
                   nextcheckkey$ = ledgerlinekey$(cursor%(1))
                   gosub display_entire_check
                   gosub restore_ledger_pointers
                   goto L26401

L26630:        REM Branch to individual ledger card...
                   gosub hold_ledger_pointers
                   gosub print_individual_ledger
                   close printer
                   pagenumber% = 0%
                   gosub restore_ledger_pointers
                   goto L26401

L26660:        REM Branch to main screen or aging display...
                   if mode% = 2% or mode% = 4% then L26695
                   gosub hold_ledger_pointers
                   gosub main_screen
                   gosub restore_ledger_pointers
                   goto L26401

L26695:            return clear          /* FOR CONTROL_DISPLAY_CHECK  */
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_DISPLAY_CHECK   */
                   return                /* TO AGING SCREEN            */

        control_display_detail_aging
            line% = line% + 1
            if line% < 21 then return
L26740:        gosub detail_aging_screen /* AND JUST SHOW SCREEN AS IS */
                     if keyhit%  =  0 then       L26815
                     if keyhit%  =  2 then       L26785
                     if keyhit%  =  5 then       L26815
                     if keyhit%  = 12 then       L26935
                     if keyhit%  = 14 then       L27030
                     if keyhit%  = 16 then       L27085
                        goto L26740

L26785:        REM Branch to first page of report...
                   return clear          /* FOR CONTROL_DISPLAY_DETAIL */
                   if firstvend$ = "ALL" then nextvend$ = hex(00)  else  ~
                      nextvend$ = firstvend$ addc all(hex(ff))
                   goto display_detail_aging

L26815:        REM Next page of report...
                   if line% > 999% then return
                   if venline% > 18 then L26870
                   line$(1) = line$(venline%)      /* VENDOR   NAME +  */
                   line$(2) = line$(19)            /* THE LAST 2 LINES */
                   line$(3) = line$(20)            /* ON THE SCREEN    */
                   aginglinekey$(5) = aginglinekey$(venline%+4)
                   aginglinekey$(6) = aginglinekey$(23)
                   aginglinekey$(7) = aginglinekey$(24)
                   line%    = 4
                   goto L26910

L26870:            line% = 1
                   for temp% = venline% to 20      /* THE LINES FROM   */
                       line$(line%) = line$(temp%) /* THE VENDOR   TO  */
                                                   /* THE END OF SCREEN*/
                       aginglinekey$(line%+4) = aginglinekey$(temp%+4)
                       line% = line% + 1
                       next temp%

L26910:            venline% = 1          /* SET LINE FLAG, CLEAR SCREEN*/
                   init(" ") str(line$(), line% * 79 - 78)
                   init(" ") str(aginglinekey$(), line% * 17 + 52)
                   return

L26935:        REM Branch to more detail...
                   gosub get_cursor_position
                   if aginglinekey$(cursor%(1)) = " " then L26740
                   gosub hold_aging_pointers
                   if len( aginglinekey$(cursor%(1))) < 10 then L26980
                      nextinvoicekey$ = aginglinekey$(cursor%(1))
                      gosub display_entire_invoice      /*BRANCH TO SEE*/
                      goto L27015                        /*THE INVOICE. */

L26980:            vencode$ = aginglinekey$(cursor%(1)) /*THIS BRANCHES*/
                   gosub test_vendor_code              /*TO THE LEDGER*/
                         if errormsg$ <> " " then L27015 /*CARD DISPLAY.*/

                   if str(line$(cursor%(1)-4), 1, 9) = "UNAPPLIED"       ~
                      then gosub display_check     /* UNAPPLIED CHECKS */~
                      else gosub display_invoice
L27015:            gosub restore_aging_pointers
                   goto L26740

L27030:        REM Branch to individual ledger card...
                   gosub get_cursor_position
                   if aginglinekey$(cursor%(1)) = " " then L26740
                   gosub hold_aging_pointers
                   vencode$ = aginglinekey$(cursor%(1))
                   gosub test_vendor_code
                         if errormsg$ <> " " then L27070      /*UNLIKELY*/
                   gosub print_individual_ledger
                   close printer
                   pagenumber% = 0%
L27070:            gosub restore_aging_pointers
                   goto L26740

L27085:        REM Branch to main screen or aging display...
                   gosub hold_aging_pointers
                   gosub main_screen
                   gosub restore_aging_pointers
                   goto L26740

        control_display_summary_aging
            line% = line% + 3
            if line% < 17 then return
L27135:        gosub summary_aging_screen/* AND JUST SHOW SCREEN AS IS */
                     if keyhit%  =  0 then       L27210
                     if keyhit%  =  2 then       L27180
                     if keyhit%  =  5 then       L27210
                     if keyhit%  = 12 then       L27240
                     if keyhit%  = 14 then       L27295
                     if keyhit%  = 16 then       L27350
                        goto L27135

L27180:        REM Branch to first page of report...
                   return clear          /* FOR CONTROL_DISPLAY_SUMMARY*/
                   if firstvend$ = "ALL" then nextvend$ = hex(00)  else  ~
                      nextvend$ = firstvend$ addc all(hex(ff))
                   goto display_summary_aging

L27210:        REM Next page of report...
                   if line% > 999% then return
                   str(line$(), 1) = str(line$(),1186)
                   str(aginglinekey$(), 69) = str(aginglinekey$(), 324)
                   line%    = 4
                   return

L27240:        REM Branch to more detail...
                   gosub get_cursor_position
                   if aginglinekey$(cursor%(1)) = " " then L27135
                   gosub hold_aging_pointers
                   vencode$ = aginglinekey$(cursor%(1))
                   gosub test_vendor_code
                         if errormsg$ <> " " then L27280
                   gosub display_invoice
L27280:            gosub restore_aging_pointers
                   goto L27135

L27295:        REM Branch to individual ledger card...
                   gosub get_cursor_position
                   if aginglinekey$(cursor%(1)) = " " then L27135
                   gosub hold_aging_pointers
                   vencode$ = aginglinekey$(cursor%(1))
                   gosub test_vendor_code
                         if errormsg$ <> " " then L27335      /*UNLIKELY*/
                   gosub print_individual_ledger
                   close printer
                   pagenumber% = 0%
L27335:            gosub restore_aging_pointers
                   goto L27135

L27350:        REM Branch to main screen or aging display...
                   gosub hold_aging_pointers
                   gosub main_screen
                   gosub restore_aging_pointers
                   goto L27135

        REM *************************************************************~
            *     H A R D C O P Y    P A G E    C O N T R O L L E R     *~
            *                                                           *~
            * HANDLES PAGING AND HEADINGS.                              *~
            *************************************************************

        control_print_invoice
                line% = line% + 1
                if line% < 56 then return
                   gosub new_page
                   print using L28330
                if curr$ = "N" then L28071
                   print using L28347           /* Multi-Currency       */
                   print using L28356
                   return
L28071:         print using L28377              /* Statutory            */
                print using L28386
                return

        control_print_check
                line% = line% + 1
                if line% < 56 then return
                   gosub new_page
                   print using L28330
                   print using L28404
                   print using L28416
                   return

        new_page
                   select printer (134)
                   if line% < 100 then print using L28330
                   print page
                   line% = 14
                   pagenumber% = pagenumber% + 1
                   call "DATE" addr ("HD", hdrdate$)
                   print using L28290, pagenumber%, hdrdate$
                   print using L28305, screendate$
                   print
                   print using L28315, "VENDOR  CODE:",  vencode$,        ~
                         "ADDRESS:", address$(1)
                   print using L28315, "VENDOR  NAME:",  venname$,        ~
                         " ", address$(2)
                   print using L28315, "PHONE NUMBER:", phone$,           ~
                         " ", address$(3)
                   print using L28315, "   ", " "
                   if curr$ = "N" then                                   ~
                      print                                              ~
                   else print using L28327
                   return

        check_heading
                   check% = 1
                   if line% < 56 then L28255        /* ROOM FOR CHECKS  */
                      line% = 67                   /* FORCE PAGE EJECT */
                      return

L28255:            line% = line% + 6
                   print skip (3)
                   print using L28330
                   print using L28404
                   print using L28416
                   return

L28290: %PAGE #####    A/P001                V E N D O R      L E D G E R~
        ~                       ##########################################~
        ~###
L28305: %                                      (PAYABLES DATE: ########)

L28315: %################ ########################################       ~
        ~                           ######### ############################~
        ~###

L28327: %                                              -- ALL AMOUNTS IN ~
        ~STATUTORY CURRENCY --

L28330: %================================================================~
        ~=================================================================~
        ~===

        REM Transaction Currency...

L28347: %! INVOICE NUMBER ! RECEIVER NUMBER ! INV DATE!  POSTED !  G/L AC~
        ~CT  !  INV AMOUNT! CHECK NO ! AMOUNT PAID!  GAIN/LOSS ! INV BALAN~
        ~CE!

L28356: %!----------------+-----------------+---------+---------+--------~
        ~----+------------+----------+------------+------------+----------~
        ~--!

L28365: %!################! ################! ########! ########! #######~
        ~##  !-########.##! ######## !-########.##!-########.##!-########.~
        ~##!

        REM Statutory Currency...

L28377: %! INVOICE NUMBER ! RECEIVER NUMBER  !INVOICE DATE! DATE POSTED! ~
        ~  G/L ACCT    !INVOICE AMT. ! CHECK NO. ! AMOUNT PAID ! INV. BAL.~
        ~  !

L28386: %!----------------+------------------+------------+------------+-~
        ~--------------+-------------+-----------+-------------+----------~
        ~--!

L28395: %!################! ################ !  ########  !   ######## ! ~
        ~############# !-#######.##  !  ######## !-########.## !-########.~
        ~##!

L28404: %! CHECK NO.! CHECK DATE ! DATE POSTED ! NET  CHECK !  DISCOUNT  ~
        ~!   INVOICE PAID  ! G/L  ACCOUNT !DRT. PAYMNT?! AMT. PAID !ON FIL~
        ~E?!

L28416: %!----------+------------+-------------+------------+------------~
        ~+-----------------+--------------+------------+-----------+------~
        ~--!

L28428: %! ######## !  ########  !   ########  !-########.##!-########.##~
        ~!#################! ############ !  ########  !-#######.##! #####~
        ~##!

        control_print_detail_aging
                select printer(134)
                line% = line% + 1
                tagline% = tagline% - 1  /* GETS SET TO 1 EVERY TIME A */
                                         /* DELIMITER LINE IS PRINTED  */
                                         /* THEN STEADLY DROPS BELOW   */
                                         /* 0 UNTIL THE NEXT LINE      */
                if line% < 60 then return
                if tagline% < 0 then print using L28330
                   print page
                   call "DATE" addr("HD", hdrdate$)
                   pagenumber% = pagenumber% + 1
                   print using L28610, pagenumber%, hdrdate$
                   print using L28305, screendate$
                   print
                   print "PRINT RANGE: ";
                   if firstvend$ = "ALL" then print "ALL VENDORS"        ~
                      else print "FROM VENDOR "; firstvend$;             ~
                                 " TO VENDOR "; lastvend$
                   print using L28330
                   print using L28630
                   print using L28650
                   print using L28670, period$(1), period$(2), period$(3),~
                                      period$(4), period$(5)
                   print using L28690
                   line% = 13
                   prtvencode$ = vencode$    /* RESTORE FOR 1ST LINE   */
                   return

L28610: %PAGE #####   A/P002    D E T A I L E D     P A Y A B L E S   A G~
        ~ I N G                 ##########################################~
        ~###

L28630: %!  VENDOR !                      !    INVOICE     ! INVOICE!    ~
        ~            !            A G I N G    B R E A K D O W N          ~
        ~  !

L28650: %!         !TRUNCATED VENDOR INFO.!                !        !  P.~
        ~O.  NUMBER  +----------+----------+----------+----------+--------~
        ~--+

L28670: %!   CODE  !                      !     NUMBER     !  DATE  !    ~
        ~            !##########!##########!##########!##########!########~
        ~##!

L28690: %+---------+----------------------+----------------+--------+----~
        ~------------+----------+----------+----------+----------+--------~
        ~--+

L28710: %!#########!######################!################!########!-###~
        ~#########.##!-######.##!-######.##!-######.##!-######.##!-######.~
        ~##!

L28730: %!#########!TOTAL...........     -###,###,###.##   !        !    ~
        ~            !##########!##########!##########!##########!########~
        ~##!

L28750: %!         !  ***** FINAL  TOTAL ***** -########.## *****        ~
        ~            !##########!##########!##########!##########!########~
        ~##!

L28770: %!         !  *** PERCENT OF TOTAL ***                           ~
        ~            !   -###.##%   -###.##%   -###.##%   -###.##%   -###.~
        ~##%

        control_print_summary_aging
                select printer(134)
                line% = line% + 1
                if line% < 55 then return
                   print page
                   call "DATE" addr ("HD", hdrdate$)
                   pagenumber% = pagenumber% + 1
                   print using L28905, pagenumber%, hdrdate$
                   print using L28305, screendate$
                   print
                   print "PRINT RANGE: ";
                   if firstvend$ = "ALL" then print "ALL VENDORS"        ~
                      else print "FROM VENDOR "; firstvend$;             ~
                                 " TO VENDOR ";  lastvend$
                   print using L28925
                   print using L28940
                   print using L28955
                   print using L28970, period$(1), period$(2), period$(3),~
                                      period$(4), period$(5)
                   print using L28985
                   line% = 11
                   return

L28905: %PAGE #####   A/P003      S U M M A R Y     P A Y A B L E S    A ~
        ~G I N G                ##########################################~
        ~###

L28925: %+---------+------------------------------+----------+----------+~
        ~------------+----------------------------------------------------~
        ~--+
L28940: %!  VENDOR !                              !          !          !~
        ~  TOTAL     !            A G I N G     B R E A K D O W N         ~
        ~  !
L28955: %!         !     V E N D O R    N A M E   !          !          !~
        ~            +----------+----------+----------+----------+--------~
        ~--+
L28970: %!  CODE   !                              !          !          !~
        ~ PAYABLES   !##########!##########!##########!##########!########~
        ~##!
L28985: %+---------+------------------------------+----------+----------+~
        ~------------+----------+----------+----------+----------+--------~
        ~--+
L29000:  %!#########!##############################!          !          ~
        ~!-########.##!##########!##########!##########!##########!#######~
        ~###!
L29015: %!         !   *** PERCENT OF TOTAL ***   !          !          !~
        ~            !    ###.##%    ###.##%    ###.##%    ###.##%    ###.~
        ~##%

        control_print_entire_invoice
                line% = line% + 1
                if line% < 66 then return
                print page
                call "DATE" addr ("HD", hdrdate$)
                print "CONTINUATION OF LINE ITEMS:"; tab(87); hdrdate$
                print skip (2)
                print using L38723, "VENDOR CODE             ", vencode$, ~
                                                               venname$
                print using L38717, "INVOICE NUMBER          ", invoicenr$
                print using L38717, "RECEIVER NUMBER         ", receiver$
                print skip (2)
                print using L38741
                print using L38747
                line% = 16
                return

        control_print_entire_check
                line% = line% + 1
                if line% < 66 then return
                print page
                call "DATE" addr ("HD", hdrdate$)
                print "CONTINUATION OF INVOICES PAID:"; tab(87); hdrdate$
                print skip (2)
                print using L38723, "VENDOR CODE           ", vencode$,   ~
                                                             venname$
                print using L38717, "CHECK NUMBER          ", checknumber$
                print skip (2)
                print using L39844
                print using L39852
                line% = 15
                return

        REM *************************************************************~
            *  L O A D    &    F O R M A T    N E X T    I N V O I C E  *~
            *                                                           *~
            * LOADS UP THE NEXT INVOICE FOR PRINTING THE "INVOICES" PART*~
            * OF THE LEDGER CARD.                                       *~
            *************************************************************

        read_next_invoice
                tran_inv_amt, sub_inv_amt = 0
L30080:         call"PLOWNEXT" (#5, nextinvoicekey$, 9%, f1%(5))
                     if f1%(5) = 0 then return

             invoice% = 1

                    get #5, using L30210, invoicenr$, receiver$,          ~
                            invoicedate$, xixacct$, payaccttype$,        ~
                        dateposted$, invamt, balance, holdflag$,         ~
                        inv_type$, pay_curr_code$
                    if inv_type$ = "R" then L30080  /* Plow again */
                        call "GLFMT" (xixacct$)
                            gosub'100 (xixacct$,payaccttype$)
                            payaccttype$=ttype$
                            invamt = round(invamt, 2%)
                            balance = round(balance, 2%)
                    if curr$ = "N" then L30172
                    if pay_curr_code$ = stat_curr_code$ then L30173
                    if pay_curr_code$ = " " then L30172 else L30175
L30172:                    pay_curr_code$ = stat_curr_code$
L30173:                    pay_curr_desc$ = stat_curr_desc$
                           goto L30370
L30175:             readcm$ = pay_curr_code$
                    call "READ100" (#16, readcm$, f1%(16))
                    if f1%(16) = 0 then L30370
                        get #16 using L30179, pay_curr_desc$
L30179:                     FMT POS(5), CH(30)
                    readpay$ = nextinvoicekey$
L30186:             call "PLOWNEXT" (#18, readpay$, 25%, f1%(18))
                    if f1%(18) = 0 then L30370
                        get #18 using L30191, sub_inv_amt, exchg_ratei
L30191:                     FMT POS(33), PD(14,4), POS(63), PD(14,7)
                    sub_inv_amt = round(sub_inv_amt, 2%)
                    tran_inv_amt = (tran_inv_amt + sub_inv_amt)
                    call "CONVERT" (exchg_ratei, 2.7, exchg_ratei$)
                    goto L30186

L30210:               FMT                                                ~
                          XX(9),         /* VENDOR CODE                */~
                          CH(16),        /* INVOICE NUMBER             */~
                          CH(16),        /* PURCHASE ORDER NUMBER      */~
                          CH(6),         /* INVOICE DATE               */~
                          XX(9),         /* PURCHASES ACCOUNT DEFAULT  */~
                          CH(9),         /* PAYABLES ACCOUNT           */~
                          CH(1),         /* PAYABLES ACCOUNT TYPE      */~
                          XX(20),        /* SKIP PAY DATES/NON-DISCOUNT*/~
                          CH(6),         /* (MODULE) DATE POSTED       */~
                          XX(18),        /* SKIP AUDIT TRAIL INFO      */~
                          PD(14,4),      /* ORIGINAL INVOICE AMOUNT    */~
                          PD(14,4),      /* OUTSTANDING INVOICE AMOUNT */~
                          XX(40),        /* SKIP AUDIT TRAIL INFO      */~
                          CH(1),         /* INVOICE HOLD FLAG          */~
                          CH(1),         /* Invoice type flag          */~
                          POS(173),      /*                            */~
                          CH(4)          /* Currency Code              */

L30370:             REM Check if this invoice date is > payables date...
                        if dateposted$ > paydate$ then L30080

                    REM Format invoice data...
                        agingdate$ = invoicedate$ /*USED TO AGE INVOICE*/
                        call "DATEFMT" (invoicedate$)
                        call "DATEFMT" (dateposted$)
                        if payaccttype$ = "L"  or                        ~
                           payaccttype$ = "M"  then L30460
                               balance, tran_balance = 0

L30460:             REM Load checks into core if this is a payeivable...
                        if payaccttype$ = "L" or payaccttype$ = "M"      ~
                        then gosub find_applicable_checks

                    return

        REM *************************************************************~
            *    L O A D    &    F O R M A T    N E X T    C H E C K    *~
            *                                                           *~
            * LOADS UP THE NEXT CHECK FOR PRINTING THE "CHECKS" PART    *~
            * OF THE LEDGER CARD.                                       *~
            *************************************************************

        read_next_check
L31080:         call "PLOWNEXT" (#7, nextcheckkey$, 9%, f1%(7))
                     if f1%(7) = 0 then return

                REM Load up check header information...
                    get #7, using L31170, checknumber$, checkdate$,       ~
                            discount, dateposted$, netcheck
                    discount = round(discount, 2)
                    netcheck = round(netcheck, 2)

L31170:             FMT XX(9),           /* SKIP VENDOR   CODE         */~
                        CH(8),           /* GET CHECK NUMBER           */~
                        CH(6),           /* CHECK DATE                 */~
                        PD(14,4),        /* DISCOUNT AMOUNT            */~
                        XX(9),           /* DISCOUNT ACCOUNT           */~
                        XX(9),           /* CASH IN BANK ACCOUNT       */~
                        CH(6),           /* DATE POSTED                */~
                        XX(18),          /* SKIP AUDIT TRAIL DATES     */~
                        PD(14,4)         /* NET CHECK AMOUNT           */

                    REM See if check date > Payeivables date...
                        if dateposted$ > paydate$ then L31080

                    call "DATEFMT" (checkdate$)
                    call "DATEFMT" (dateposted$)
                    checkdetailkey$ = nextcheckkey$

               if curr$ = "N" then L31510  /* Multi-Currency not used */

               foriegn_curr$ = "N"
               tran_curr_code$ = " "
               tran_pmt, gain_loss_amt, units_per_stat, tran_disc,       ~
                         tran_net = 0
               readcsh$ = str(vencode$) & str(checknumber$)
               call "READ100" (#17, readcsh$, f1%(17))
               if f1%(17) = 0% then L31510
                   foriegn_curr$ = "Y"
               get #17 using L31470, tran_curr_code$, units_per_stat,     ~
                       tran_net, tran_disc, gain_loss_amt
L31470:            FMT CH(4), POS(36), PD(14,7), PD(14,4), PD(14,4),     ~
                       PD(14,4)
               tran_pmt = round(tran_pmt, 2%)
               tran_disc = round(tran_disc, 2%)
               tran_net = round(tran_net, 2%)

L31510:        return

        REM *************************************************************~
            *  L O A D    &    F O R M A T    C H E C K    D E T A I L  *~
            *                                                           *~
            * LOADS UP THE NEXT INVOICE PAID ON THE CHECK CURRENTLY     *~
            * BEING PRINTED UNDER "CHECKS"                              *~
            *************************************************************

        read_next_invoice_paid
               applied, unapplied, direct, pgapplied = 0
L32090:        call "PLOWNEXT" (#8, checkdetailkey$, 17%, f1%(8))
               if f1%(8) = 0 then return       /* NO MORE DETAILS */
                  get #8, using L32140, invoice$, xixacct$ ,              ~
                          accttype$, amount, tempdate$, linestatus$
                  amount = round(amount, 2)
                  call "GLFMT" (xixacct$)

L32140:           FMT XX(20),            /* SKIP KEY OF FILE           */~
                      CH(16),            /* INVOICE NUMBER             */~
                      CH(9),             /* DEBIT ACCOUNT              */~
                      CH(1),             /* ACCOUNT TYPE               */~
                      PD(14,4),          /* AMOUNT                     */~
                      CH(6),             /* DATE POSTED                */~
                      XX(18),            /* SKIP DISC, INV DATE, CAT   */~
                      CH(1)              /* LINE STATUS                */

                  if curr$ <> "Y" then L32205
                     tran_pmt = amount
                     call "READ100" (#15, key(#8), f1%(15))
                          if f1%(15) = 0% then L32205
                     get #15 using L32200, tran_pmt
L32200:                   FMT POS(69), PD(14,4)
L32205:           gosub'100 (xixacct$,accttype$)
                  accttype$=ttype$
                  if tempdate$  > paydate$ then L32090
                  if accttype$ <> "L" and accttype$ <> "M" then L32500

                  REM See if invoice paid is on file...
                      prtaccttype$ = " "
                      invoicepaidkey$ = vencode$
                      str(invoicepaidkey$, 10) = invoice$

                     payaccttype$ = accttype$

                      call "READ100" (#5, invoicepaidkey$, f1%(5))
                           if f1%(5%) <> 0% then L32290
                               if linestatus$ = "F" then L32484 else L32460
L32290:                       get #5, using L32320, ttacct$, payaccttype$,~
                                      invoicedate$
                              call "GLFMT" (ttacct$)

L32320:                           FMT XX(56),                /* SKIP   */~
                                      CH(9),                 /* ACCT   */~
                                      CH(1),                 /* ACCTYPE*/~
                                      XX(20),                /* SKIP   */~
                                      CH(6)                  /* DATE   */

                              gosub'100 (ttacct$,payaccttype$)
                              payaccttype$=ttype$
                              if payaccttype$ <> "L"                     ~
                             and payaccttype$ <> "M" then L32460
                                 REM If invoice date > pay date then the ~
                                     check is unapplied as of this       ~
                                     summary's date...
                                     if invoicedate$ > paydate$ then L32460
                                        applied = round(amount, 2%)
                                        onfile$ = " "  /* INV FOUND   */
                                        return

L32460:                                 unapplied = round(amount, 2%)
                                        onfile$ = "NO/UNAP" /*NOT FOUND */
                                        return

L32484:                                 pgapplied = round(amount, 2%)
                                        onfile$ = "NO" /* INV PURGED  */
                                        return

L32500:               prtaccttype$ = "Y"
                      direct = round(amount, 2%)
                      onfile$ = "N/A"   /* INVOICE NOT EXPECTED ON FILE*/
                      return

        REM *************************************************************~
            *  S P E C I A L   P L O W   R O U T I N E   F O R   C H X  *~
            *                                                           *~
            * THIS PLOWS DOWN THE ALTERNATE KEY LIST FOR THE CHECK FILE *~
            * LOOKING FOR THE NEXT OCCURRENCE OF AN INVOICE NUMBER.     *~
            *************************************************************

        find_applicable_checks
            max_check_counter% = 0
            init(hex(ff))  checkstk$(), g_l_stk$()
            mat checkstk = zer  :  mat g_l_stk = zer
            gain_loss_amt = 0
            balance = round(invamt, 2%)
            foriegn_curr$, pay_flag$ = "N"
            tran_balance = tran_inv_amt

            REM Note how ew get first invoice by specifying key and then ~
                read without the key to get subsequent references...

            REM Get first reference...
                call "REDALT0" (#8, invoicenr$, 1%, f1%(8))

                REM Plow routine for invoices...
L33202:             if f1%(8) = 1 then L33230   /* Check Exists */
                        if curr$ = "N" then return
                    if max_check_counter% <> 0% then return
                         if pay_curr_code$ = " " then return
                         if pay_curr_code$ = stat_curr_code$ then return
                    tran_curr_code$ = pay_curr_code$
                    tran_curr_desc$ = pay_curr_desc$
                    foriegn_curr$, pay_flag$ = "Y"
                    readpay$ = str(tempvencode$) & str(tempinvoice$) &   ~
                               str(lnseqno$)
                    call "READ100" (#18, readpay$, f1%(18))
                    if f1%(18) = 0 then L33224
                        get #18 using L33222, tran_inv_amt
L33222:                     FMT POS(33), PD(14,4)
                    tran_balance = tran_inv_amt
L33224:             return
L33230:             get #8, using L33290, tempvencode$, checknumber$,     ~
                            lnseqno$, tempinvoice$, ttacct$,             ~
                            tempaccttype$, tempamount, tempdate$
                    tempamount = round(tempamount, 2)
                    call "GLFMT" (ttacct$)

L33290:                 FMT CH(9),       /* VENDOR   CODE              */~
                            CH(8),       /* CHECK NUMBER               */~
                            CH(3),       /* SEQUENCE NUMBER            */~
                            CH(16),      /* INVOICE NUMBER             */~
                            CH(9),       /* ACCOUNT NUMBER             */~
                            CH(1),       /* ACCOUNT TYPE               */~
                            PD(14,4),    /* AMOUNT OF TRANSACTION      */~
                            CH(6)        /* DATE OF CHECK              */


            REM SSee if invoice number has changed...
                if tempinvoice$ <> invoicenr$ then return
                if tempvencode$ <> vencode$ then L33860

            REM See if check date O.K. & if account type is "A"...
                if tempdate$ > paydate$ then L33860     /* TOO RECENT */
                gosub'100 (ttacct$,tempaccttype$)
                tempaccttype$=ttype$
                if tempaccttype$ <> "L" and tempaccttype$<>"M" then L33860

                if curr$ = "N" then L33800  /* Multi-Currency not used */
                if pay_curr_code$ = stat_curr_code$ then L33800
                    tran_curr_code$ = pay_curr_code$
                    tran_curr_desc$ = pay_curr_desc$

        REM Get check information in transaction currency...
                foriegn_curr$ = "Y"
                readcurr$ = str(tempvencode$) & str(checknumber$) &      ~
                            str(lnseqno$)
                call "READ100" (#15, readcurr$, f1%(15))
                if f1%(15) = 0% then L33800  /* No Check - TRANSACTION */
                    pay_flag$ = "N"
                gain_loss_acct$ = " "
                tran_pmt_amt, gain_loss_amt, tran_disc, tran_net = 0
*              TRAN_BALANCE = 0
                get #15 using L33580, tran_pmt_amt, gain_loss_amt
L33580:             FMT POS(69), PD(14,4), XX(8), PD(14,4)

            REM Get Gain/Loss Account Number...
                readcsh$ = str(tempvencode$) & str(checknumber$)
                call "READ100" (#17, readcsh$, f1%(17))
                if f1%(17) = 0% then L33790
                    get #17 using L33680, gain_loss_acct$
L33680:                 FMT POS(68), CH(9)
                call "GLFMT" (gain_loss_acct$)

L33790:     REM Now push detail onto the detail stack...
L33800:         max_check_counter% = max_check_counter% + 1
                checkstk$(max_check_counter%) = checknumber$
                tran_pmt_amt(max_check_counter%) =                       ~
                              round(tran_pmt_amt, 2%)
                checkstk(max_check_counter%)  = round(tempamount, 2%)
                gain_loss_amt = round(gain_loss_amt, 2%)
                call "CONVERT" (gain_loss_amt, -2.2, gain_loss_amt$)
                g_l_stk$(max_check_counter%)  = gain_loss_acct$
                g_l_stk(max_check_counter%)   = gain_loss_amt
                balance = round(balance - tempamount + gain_loss_amt, 2%)
            if foriegn_curr$ = "Y" then                                  ~
                tran_balance = round(tran_balance - tran_pmt_amt, 2%)

            REM Load up next invoice...
L33860:         call "READNEXT" (#8, f1%(8))
                goto L33202

        REM *************************************************************~
            *      C O M P U T E    S U M M A R Y    T O T A L S        *~
            *                                                           *~
            * NECESSARY TO PLOW THROUGH THE INVOICE AND CHECKS FILE     *~
            * AND PERFORM SOME TOTALLING.                               *~
            *************************************************************

        compute_summary_total
            REM Initialize invoice pointers and accumulators...
                nextinvoicekey$ = vencode$
                mat aging = zer
                mat suminvoices = zer
                sumbalance, sumgainloss, suminvoices, cashinvoices = 0

                REM Total up invoices...
L34170:             gosub read_next_invoice
                         if f1%(5) = 0 then L34220
                            gosub aging            /* ADD UP TOTALS    */
                            goto L34170

L34220: compute_summary_checks
            REM Initialize check pointers and accumulators...
                nextcheckkey$ = vencode$
                mat sumapplied = zer
                mat sumunapplied = zer
                sumapplied, sumunapplied, sumdirect, sumapplpurged = 0

                REM Total up checks...
L34280:             gosub read_next_check          /* GET CHECK NUMBER */
                          if f1%(7) = 0 then L34380
L34300:                      gosub read_next_invoice_paid
                                   if f1%(8) = 0 then L34280
                                n%=1
                                if payaccttype$="M" then n%=2%
                      sumapplied = round(sumapplied + applied,2%)
                      sumunapplied = round(sumunapplied + unapplied,2%)
                      sumapplpurged = round(sumapplpurged+pgapplied, 2%)
                      sumapplied(n%) = round(sumapplied(n%)+applied,2%)
                                sumunapplied(n%) = round(sumunapplied(n%)~
                                                 + unapplied, 2%)
                                sumdirect = round(sumdirect+direct, 2%)
                                      goto L34300

L34380:     gosub summary_total_manipulation       /* FINAL STEP       */
            sumbalance(1) = round(sumbalance(1) + sumgainloss, 2%)
            return

        REM *************************************************************~
            *             S U M M A R Y    R O U T I N E S              *~
            *                                                           *~
            * THESE ARE ROUTINES USED IN ARRIVING AT THE SUMMARY INFO.  *~
            * "AGING" AGES INVOICES, IF A PAYABLE,    AND ADDS THE TOTAL*~
            * INTO THE INVOICE TOTALS.                                  *~
            * "SUMMARY_TOTAL" HANDLES FINAL MANIPULATIONS ON THE TOTALS *~
            *************************************************************

        aging
                REM IF DIRECT SALE, DON'T AGE THIS INVOICE
                    if payaccttype$ = "L"                                ~
                    or payaccttype$ = "M" then L35140

                       cashinvoices = round(cashinvoices + invamt, 2%)
                       location% = 1%
                       search cutoffdates$() <= agingdate$ to location$  ~
                                                                  step 6%
                       location% = val(location$,2)/6+1
                       return
L35140:
                n%=1
                if payaccttype$="M" then n%=2

                suminvoices(n%) = round(suminvoices(n%) + invamt, 2%)
                sumbalance(n%) =  round(sumbalance(n%) + balance, 2%)
*              SUMBALANCE  = ROUND(SUMBALANCE + BALANCE, 2%)
                suminvoices = round(suminvoices + invamt, 2%)
*              SUMGAINLOSS = ROUND(SUMGAINLOSS + GAIN_LOSS_AMT, 2%)
                if max_check_counter% = 0% then L35185
                     countercheck% = 1%
L35169:              sumgainloss = round(sumgainloss +                   ~
                                              g_l_stk(countercheck%), 2%)
                     countercheck% = countercheck% + 1%
                     if countercheck% <= max_check_counter% then L35169
L35185:         search cutoffdates$() <= agingdate$ to location$ step 6
                location% = val(location$,2)/6+1
                aging(location%) = round(aging(location%)+balance, 2%)
                return

        summary_total_manipulation
           for n%= 1 to 2
        sumbalance(n%) = suminvoices(n%)-sumapplied(n%)-sumunapplied(n%)
           sumbalance(n%)=round(sumbalance(n%),2%)
           next n%

           REM  Sumbalance = Suminvoices - Sumapplied - Sumunapplied ????
                availcredit = max(creditlimit - suminvoices + sumapplied ~
                               + sumunapplied, 0)
                availcredit = round(availcredit, 2%)
                aging(1) = round(aging(1) - sumunapplied, 2%)
                sumbalance = round(aging(1) + aging(2) + aging(3)        ~
                                 + aging(4) + aging(5), 2%)
                sumapplied = - sumapplied
                sumunapplied = - sumunapplied
                return

        REM *************************************************************~
            *           R E A D    N E X T    C U S T O M E R           *~
            *                                                           *~
            * PLOWS THROUGH THE VENDOR   MASTER FILE, FINDING VENDORS   *~
            * WITH EITHER CHECKS OR INVOICES TO PRINT.                  *~
            *************************************************************

        read_next_vendor
            REM Plow through vendor master file...
                call "PLOWNEXT" (#3, nextvend$, 0%, f1%(3))
                     if f1%(3) = 0 then no_more_vendors
                     if nextvend$ > lastvend$ then no_more_vendors
                     vencode$ = nextvend$
                     if lastvend$ = firstvend$ then return /* PRINT ONE*/

                     REM Anything to print for this vendor?...
                         nextinvoicekey$, nextcheckkey$ = vencode$
                         gosub read_next_invoice   /* ANY INVOICES?    */
                               if f1%(5) = 1 then L36240
                         gosub read_next_check     /* ANY CHECKS?      */
                               if f1%(7) = 1 then L36240
                                  REM NOTHING TO PRINT HERE
                                  goto read_next_vendor

L36240:              gosub test_vendor_code
                           if errormsg$ <> " " then read_next_vendor
                           return

        no_more_vendors
                 f1%(3) = 0
                 return

        REM *************************************************************~
            *   P O I N T E R    B U F F E R I N G    R O U T I N E S   *~
            *                                                           *~
            * THESE ROUTINES HOLD THE MAJOR POINTERS FOR THE CURRENT    *~
            * DISPLAY WHILE THE USER BRANCHES EITHER TO HARDCOPY, OR TO *~
            * THE LEDGER CARD FROM THE CURRENT DISPLAY.  THEN, THEY     *~
            * RESTORE THE VALUE OF THE POINTERS SO THE PROGRAM CAN      *~
            * FINISH PROCESSING.                                        *~
            *************************************************************

        hold_ledger_pointers
            ledgerline%         = line%
            mat ledgerline$     = line$
            ledgerinvoice$      = nextinvoicekey$
            ledgercheck$        = nextcheckkey$
            ledgercheckdetail$  = checkdetailkey$
            ledgervencode$      = vencode$
            ledgerfirstvend$    = firstvend$
            ledgerlastvend$     = lastvend$
            return

        restore_ledger_pointers
            line%               = ledgerline%
            mat line$           = ledgerline$
            nextinvoicekey$     = ledgerinvoice$
            nextcheckkey$       = ledgercheck$
            checkdetailkey$     = ledgercheckdetail$
            vencode$, nextvend$ = ledgervencode$
            firstvend$          = ledgerfirstvend$
            lastvend$           = ledgerlastvend$
            gosub test_vendor_code    /* RESTORE NAME, SALESMAN, ETC */
            return

        hold_aging_pointers
            agingline%          = line%
            mat agingline$      = line$

            for v% = 1% to 5%
              tempaging(v%)   = round(aging(v%), 2%)
              temptotal(v%)   = round(total(v%), 2%)
            next v%

            aginginvoice$       = nextinvoicekey$
            agingcheck$         = nextcheckkey$
            agingcheckdetail$   = checkdetailkey$
            agingvencode$       = vencode$
            agingfirstvend$     = firstvend$
            aginglastvend$      = lastvend$
            return

        restore_aging_pointers
            line%               = agingline%
            mat line$           = agingline$

            for v% = 1% to 5%
              aging(v%)           = round(tempaging(v%), 2%)
              total(v%)           = round(temptotal(v%), 2%)
            next v%

            nextinvoicekey$     = aginginvoice$
            nextcheckkey$       = agingcheck$
            checkdetailkey$     = agingcheckdetail$
            vencode$, nextvend$ = agingvencode$
            firstvend$          = agingfirstvend$
            lastvend$           = aginglastvend$
            gosub test_vendor_code     /* RESTORE NAME, SALESMAN, ETC */
            return

        REM *************************************************************~
            *       D I S P L A Y    E N T I R E    I N V O I C E       *~
            *                                                           *~
            * READS THE ENTIRE INVOICE FROM THE PAYABLES    FILE SO WE  *~
            * CAN DISPLAY THE DETAIL.                                   *~
            *************************************************************

        display_entire_invoice
            call "READ100" (#5, nextinvoicekey$, f1%(5))
                 if f1%(5) = 0 then return         /* NOT FOUND        */
                 get #5, using L38138,                                    ~
                      vencode$, invoicenr$, receiver$,                   ~
                      invoicedate$, slsacct$, payacct$, payaccttype$,    ~
                      duedate$, discdate$, nondiscamount, dateposted$,   ~
                      origdate$, origuserid$, lastdate$, lastuserid$,    ~
                      invamt, balance, freetest$, discpcnt, discamt,     ~
                      ten99$, holdflag$, text$
                gosub test_vendor_code
                if holdflag$ <> "Y" then holdflag$ = " "
                invamt = round(invamt, 2%)
                balance = round(balance, 2%)
                nondiscamount = round(nondiscamount, 2%)
            REM Format all data in the header...
                call "DESCRIBE" (#2, slsacct$, slsacctdescr$, 1%, f1%(2))
                  call "GLFMT" (slsacct$)
                call "DESCRIBE" (#2, payacct$, payacctdescr$, 1%, f1%(2))
                  call "GLFMT" (payacct$)
                  gosub'100 (payacct$,payaccttype$)
                  payaccttype$=ttype$
                call "DATEFMT"  (invoicedate$)
                if discdate$ = blankdate$ then discdate$ = "00/00/00" else ~
                call "DATEFMT"  (discdate$)
                call "DATEFMT"  (dateposted$)
                call "DATEFMT"  (duedate$)
                call "DATEFMT"  (origdate$)
                if lastdate$ = " " or lastdate$ = blankdate$ ~
                                 then lastdate$ = "NONE"               ~
                  else call "DATEFMT"(lastdate$)
                type$ = "(UNDEFINED)"    /* SET DEFAULT FOR BELOW      */
                if payaccttype$ = "$" then type$ = "(CASH IN BANK)"
                if payaccttype$ = "A" then type$ = "(ASSET)"
                if payaccttype$ = "L" then type$ = "(LIABILITY)"
                if payaccttype$ = "N" then type$ = "(NON-AP LIAB.)"
                if payaccttype$ = "M" then type$ = "(MISC LIABILITY)"
                if payaccttype$ = "C" then type$ = "(CAPITAL)"
                if payaccttype$ = "R" then type$ = "(REVENUE)"
                if payaccttype$ = "E" then type$ = "(EXPENSE)"

L38138:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(06),                  /* INVOICE DATE               */~
                CH(09),                  /* PURCHASES ACCOUNT DEFAULT  */~
                CH(09),                  /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE      */~
                CH(6),                   /* DUE DATE WITHOUT DISCOUNT  */~
                CH(6),                   /* DUE DATE TAKING DISCOUNT   */~
                PD(14,4),                /* NON-DISCOUNTABLE AMOUNT    */~
                CH(6),                   /* DATE POSTED                */~
                CH(6),                   /* ORIGINALLY INPUT ON (DATE) */~
                CH(3),                   /* ORIGINALLY INPUT BY (USER) */~
                CH(6),                   /* LAST MODIFIED ON (DATE)    */~
                CH(3),                   /* LAST MODIFIED BY (USER)    */~
                PD(14,4),                /* TOTAL INVOICE AMOUNT       */~
                PD(14,4),                /* CURRENT OUTSTANDING BALANCE*/~
                CH(20),                  /* FREE TEST FIELD            */~
                2*PD(14,4),              /* DISC PERCENT & DISC AMOUNT */~
                CH(4),                   /* 1099 Category              */~
                CH(1),                   /* Hold Flag                  */~
                XX(1),                   /* Invoice Type               */~
                CH(4)                    /* Text ID                    */

L38210: REM Display first page...
L38213:     gosub invoice_header_first_screen          /* 1ST PAGE    */
                  if keyhit%  =  0 then       L38237
                  if keyhit%  =  2 then       L38264
                  if keyhit%  =  5 then       L38237
                  if keyhit%  = 14 then gosub print_entire_invoice
                  if keyhit%  = 16 then       return
                     goto L38213

L38237: REM Display second page...
L38240:     gosub invoice_header_second_screen
                  if keyhit%  =  0 then       L38264
                  if keyhit%  =  2 then       L38264
                  if keyhit%  =  4 then       L38210
                  if keyhit%  = 14 then gosub print_entire_invoice
                  if keyhit%  = 16 then       return
                     goto L38240

L38264: REM Line items...
            REM First, set key information...
                str(invoicelinekey$,  1) = vencode$
                str(invoicelinekey$, 10) = invoicenr$
                str(invoicelinekey$, 26) = " "
                readpay$ = invoicelinekey$  /* GET READY FOR MC */
                line%, t%, flag% = 0
                REM Initialize line item fields...
                    init(" ")                                            ~
                        item$(), part$(), descr$(), cat$(), qtysold$(),  ~
                        price$(), extension$(), txbl$(), acct$(), lot$(),~
                        job$(), separator$(), price$(), po$(), poline$(),~
                        store$(), pricet$(), extensiont$()

L38303:     REM Basic loop which gets screenload of lines starts here...
                call "PLOWNEXT" (#6, invoicelinekey$, 25%, f1%(6))
                     if f1%(6) = 0 then L38357
                flag% = 1      /* ANYTHING WRITTEN FOR THIS SCREEN?    */
                t% = t% + 1
                line% = line% + 1

                get #6, using L38465, po$(t%), poline$(t%), acct$(t%),    ~
                              part$(t%), qtysold, extension, job$(t%),   ~
                              store$(t%), lot$(t%), price
                price = round(price, 7%)
                call "GLFMT" (acct$(t%))
                call "CONVERT" (qtysold, -0.2, qtysold$(t%))
                call "CONVERT" (price, -2.7, price$(t%))
                call "CONVERT" (extension, -2.2, extension$(t%))

                if curr$ = "N" then L38354  /* MULTI-CURRENCY NOT USED */
                   call "PLOWNEXT" (#18, readpay$, 25%, f1%(18))
                   if f1%(18) = 0% then L38346
                       get #18 using L38341, extensiont, pricet
L38341:                    FMT POS(33), PD(14,4), PD(14,7)
                       pricet = round(pricet, 7%)
                       call "CONVERT" (pricet, -2.7, pricet$(t%))
                       call "CONVERT" (extensiont, -2.2, extensiont$(t%))
                       goto L38354
L38346:            pricet$(t%)     = price$(t%)    /* TRAN IS STAT     */
                   extensiont$(t%) = extension$(t%)

L38354:         if t% < 5 then L38303               /* GET NEXT LINE    */
L38357:         if flag% = 0 then return           /* NO MORE LINES    */

            REM Display this page of line items...
                call "SETSEP" (separator$(), line% - t%, t%)
L38369:         gosub invoice_line_item_screen
                      if keyhit%  =  0 then       L38393
                      if keyhit%  =  2 then       L38210
                      if keyhit%  =  5 then       L38393
                      if keyhit%  = 14 then gosub print_entire_invoice
                      if keyhit%  = 16 then       return
                         goto L38369

L38393:         REM Branch to next page of line items...
                    item$      (1) = item$      (5)
                    part$      (1) = part$      (5)
                    descr$     (1) = descr$     (5)
                    cat$       (1) = cat$       (5)
                    acct$      (1) = acct$      (5)
                    qtysold$   (1) = qtysold$   (5)
                    price$     (1) = price$     (5)
                    txbl$      (1) = txbl$      (5)
                    lot$       (1) = lot$       (5)
                    job$       (1) = job$       (5)
                    extension$ (1) = extension$ (5)
                    separator$ (1) = separator$ (5)
                    pricet$    (1) = pricet$    (5)  /* Tran Currency */
                    extensiont$(1) = extensiont$(5)  /* Tran Currency */

                    for t% = 2 to 5
                         item$(t%), part$(t%), descr$(t%), cat$(t%),     ~
                         qtysold$(t%), price$(t%), extension$(t%),       ~
                         txbl$(t%), acct$(t%), lot$(t%), job$(t%),       ~
                         store$(t%), separator$(t%), pricet$(t%),        ~
                         extensiont$(t%) = " "
                    next t%
                    t% = 1
                    flag% = 0
                    goto L38303

L38465:     FMT XX(16),                  /* RECEIVER NUMBER            */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(03),                  /* PURCHASE ORDER LINE NUMBER */~
                XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* PURCHASES ACCOUNT NUMBER   */~
                CH(25),                  /* PART NUMBER                */~
                PD(14,4),                /* QUANTITY BOUGHT            */~
                PD(14,4),                /* EXTENSION                  */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(03),CH(06),PD(14,7)   /* STORE, LOT, PRICE          */

        print_entire_invoice
            if curr$ = "N" then L38530
               readpay$ = str(vencode$) & str(invoicenr$)
               call "PLOWNEXT" (#18, readpay$, 25%, f1%(18))
               if f1%(18) = 0% then L38525
                  get #18 using L38511, tran_curr_code$, exchg_ratei
L38511:               FMT CH(4), POS(63), PD(14,7)
                  call "CONVERT" (exchg_ratei, 2.7, exchg_ratei$)
               readcurr$ = tran_curr_code$
               call "READ100" (#16, readcurr$, f1%(16))
               if f1%(16) = 0% then L38528
                  get #16 using L38523, tran_curr_desc$
L38523:               FMT POS(5), CH(30)
                  goto L38530
L38525:        exchg_ratei$ = "1.00" : tran_curr_code$ = stat_curr_code$
               tran_curr_desc$ = stat_curr_desc$
                  goto L38530
L38528:        tran_curr_desc$ = " "

L38530:     call "SHOSTAT" ("Printing Invoice Information")
            call "DATE" addr ("HD", hdrdate$)
            select printer (134)         /* SET UP THE PAGE  */
            print page
            print "D E T A I L E D    I N V O I C E    P R I N T O U T";
            print tab(87), hdrdate$
            print skip(3)
            print using L38717, "VENDOR CODE              ", vencode$;
            print "---------- AUDIT TRAIL DATES ---------"
            print using L38717, "INVOICE NUMBER          ", invoicenr$,   ~
                               "INVOICE DATE            ", invoicedate$
            print using L38717, "RECEIVER NUMBER         ", receiver$,    ~
                               "PAYABLES POSTING DATE   ", dateposted$
            print using L38717, "                        ", " ",          ~
                               "DATE ORIGINALLY INPUT   ", origdate$
            print using L38717, "VENDOR NAME             ", venname$,     ~
                               "ORIGINALLY INPUT BY     ", origuserid$
            print using L38717, "VENDOR  ADDRESS (1)     ", address$(1),  ~
                               "DATE LAST MODIFIED      ", lastdate$
            print using L38717, "VENDOR  ADDRESS (2)     ", address$(2),  ~
                               "LAST MODIFIED BY        ", lastuserid$
            print using L38717, "VENDOR  ADDRESS (3)     ", address$(3)
            print using L38717, "VENDOR PHONE NUMBER     ", phone$
            print using L38723, "PURCHASE ACCOUNT DEFAULT", slsacct$,     ~
                                                           slsacctdescr$
            print using L38723, "PAYABLES ACCOUNT        ", payacct$,     ~
                                                           payacctdescr$
            print using L38723, "PAYABLES ACCOUNT TYPE   ", payaccttype$, ~
                                                           type$

            if curr$ = "N" then L38607
            print using L38729, "REGULAR DUE DATE        ", duedate$, " ",~
                "TRANSACTION CURRENCY", tran_curr_desc$
            print using L38731, "DISCOUNT DUE DATE       ",discdate$, " ",~
                "EXCHANGE RATE", exchg_ratei$, tran_curr_code$, "/",     ~
                stat_curr_code$
            print using L38762, round(discpcnt,2), round(discamt, 2)
            print using L38735, "NON-DISCOUNTABLE AMOUNT ", nondiscamount
            print using L38739, "NET INVOICE AMOUNT      ", invamt, " ",  ~
                "-- ALL AMOUNTS IN STATUTORY CURRENCY --"
                goto L38618
L38607:     print using L38723, "REGULAR DUE DATE        ", duedate$, " "
            print using L38723, "DISCOUNT DUE DATE       ",discdate$, " "
            print using L38762, round(discpcnt,2), round(discamt, 2)
            print using L38735, "NON-DISCOUNTABLE AMOUNT ", nondiscamount
            print using L38735, "NET INVOICE AMOUNT      ", invamt
L38618:     print using L38735, "CURRENT OUTSTANDING BALANCE", balance,   ~
                               "(TRUE INVOICE BALANCE)"
            REM Line items...
                REM First, set key information...
                    str(invoicelinekey$,  1) = vencode$
                    str(invoicelinekey$, 10) = invoicenr$
                    str(invoicelinekey$, 26) = " "
                    t% = 1
            print skip (2)
            print using L38741
            print using L38747
            line% = 30

L38657:     REM Basic lopp which gets each line item...
                call "PLOWNEXT" (#6, invoicelinekey$, 25%, f1%(6))
                     if f1%(6) = 1 then L38672
                        close printer: pagenumber% = 0% : return

L38672:         get #6, using L38465, po$(6), poline$(6), acct$(6),       ~
                              part$(6), qtysold, extension, job$(6),     ~
                              store$(6), lot$(6), price

                call "GLFMT" (acct$(6))
                call "CONVERT" (qtysold, 0.2, qtysold$(6))
                call "CONVERT" (price,   2.7, price$(6))
                call "CONVERT" (extension, 2.2, extension$(6))

                gosub control_print_entire_invoice
                print using L38753, acct$(6), part$(6), qtysold$(6),      ~
                      price$(6), extension$(6), job$(6), po$(6),         ~
                      poline$(6), store$(6), lot$(6)
                goto L38657

L38717: %############################  ################################  ~
        ~                ############################  ########
L38723: %############################  ##############  ##################~
        ~##############  ############################  ########
L38729: %############################  ##############  ##################~
        ~##############  #################### ############################
L38731: %############################  ##############  ##################~
        ~##############  #############  ########## #### # ####
L38735: %############################  -##########.##  ##################~
        ~##############  ############################  ########
L38737: %############################  -##########.##             #######~
        ~############# ##############################
L38739: %############################  -##########.##  ##################~
        ~##############  #######################################
L38741: %ACCOUNT      PART NUMBER                    QUANTITY        PRIC~
        ~E    EXTENSION   JOB        P.O. NUMBER & LINE     STORE  LOT
L38747: %------------ --------------------------   ----------   ---------~
        ~-   ----------   --------   --------------------   -----  ------
L38753: %############ ##########################   ##########   #########~
        ~#   ##########   ########   ################ ###    ###   ######
        %     ################################
L38762: %DISCOUNT PERCENT / AMOUNT  -###.##  / -#######.##
L38763: %############################################             #######~
        ~###### ########## #### # ####
L38775: %##############                            ######################~
        ~#################
        REM *************************************************************~
            *         D I S P L A Y   E N T I R E   C H E C K           *~
            *                                                           *~
            * READS THE ENTIRE CHECK FROM THE PAYEIVABLES FILE, SO WE   *~
            * CAN DISPLAY ALL OF THE DETAIL.                            *~
            *************************************************************

        display_entire_check
            call "READ100" (#7, nextcheckkey$, f1%(7))
                 if f1%(7) = 0 then return   /* UNLIKELY */
            get #7, using L39120, checknumber$, checkdate$, discount,     ~
                    disacct$, cshacct$, dateposted$, origdate$,          ~
                    origuserid$, lastdate$, lastuserid$, netcheck
            netcheck = round(netcheck, 2)
            discount = round(discount, 2)

            REM Format data in the header...
                call "DESCRIBE" (#2, disacct$, disacctdescr$, 1%, f1%(2))
                     call "GLFMT" (disacct$)
                call "DESCRIBE" (#2, cshacct$, cshacctdescr$, 1%, f1%(2))
                     call "GLFMT" (cshacct$)
                call "DATEFMT" (checkdate$)
                call "DATEFMT" (dateposted$)
                call "DATEFMT" (origdate$)
                if lastdate$ = " " or lastdate$ = blankdate$ ~
                                 then lastdate$ = "NONE" ~
                  else call "DATEFMT"(lastdate$)
                grosscheck = round(netcheck + discount, 2%)
                discount = -discount     /* SET UP SIGN FOR DISPLAY    */

L39120:     FMT XX(9),                   /* SKIP VENDOR CODE           */~
                CH(8),                   /* GET CHECK NUMBER           */~
                CH(6),                   /* CHECK DATE                 */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(9),                   /* DISCOUNT ACCOUNT           */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                CH(6),                   /* DATE POSTED                */~
                CH(6),                   /* ORIGINALLY INPUT ON (DATE) */~
                CH(3),                   /* ORIGINALLY INPUT BY (USER) */~
                CH(6),                   /* LAST MODIFIED ON (DATE)    */~
                CH(3),                   /* LAST MODIFIED BY (USER)    */~
                PD(14,4)                 /* NET CHECK AMOUNT           */

           REM Check for Multi-currency & get values if approprate...
               foriegn_curr$ = "N"
               if curr$ = "N" then L39244
                   foriegn_curr$ = "Y"
               readcsh$ = str(vencode$) & str(checknumber$)
               call "READ100" (#17, readcsh$, f1%(17))
               if f1%(17) = 0% then  L39224
               get #17 using L39204, curr_code$, tran_check, tran_disc
L39204:            FMT CH(4), POS(44), PD(14,4), PD(14,4)
               tran_check = round(tran_check, 2%)
               tran_disc  = round(tran_disc, 2%)
               tran_net   = round(tran_check - tran_disc, 2%)
                   goto L39244
L39224:        tran_check = round(grosscheck, 2%)
               tran_disc  = round(discount, 2%)
               tran_net   = round(netcheck, 2%)
               curr_code$ = stat_curr_code$

L39244:     REM Display header...
                gosub check_header_screen
                      if keyhit%  =  0 then       L39276
                      if keyhit%  =  2 then       L39276
                      if keyhit%  = 14 then gosub print_entire_check
                      if keyhit%  = 16 then       return
                         goto L39244

L39276:     REM Line items, Detail of invoices paid...
                checkdetailkey$ = nextcheckkey$
                init(" ") line$(), linec$()
                flag%,  line% = 0

L39296:     REM Plow each invoice paid by this check...
                call "PLOWNEXT" (#8, checkdetailkey$, 17%, f1%(8))
                     if f1%(8) = 0 then L39404
                     flag% = 1
                     line% = line% + 1
                     get #8, using L32140, invoice$, xixacct$, accttype$, ~
                             amount, tempdate$
                         amount = round(amount, 2%)
                     call "DESCRIBE" (#2, xixacct$,accountdescr$, 0%,    ~
                                                                  f1%(2))
                         call "GLFMT" (xixacct$)
                     gosub'100 (xixacct$,accttype$)
                     if curr$ <> "Y" then L39340
                         tran_pmt = amount
                         tran_curr_desc$ = stat_curr_desc$
                         call "READ100" (#15, key(#8), f1%(15))
                         if f1%(15) = 0% then L39340
                             get #15 using L39332,tran_curr_code$,tran_pmt
L39332:                          FMT CH(4), POS(69), PD(14,4)
                             tran_pmt = round(tran_pmt,2%)
                         call "READ100" (#16, tran_curr_code$, f1%(16))
                         if f1%(16) = 0% then L39340
                             get #16 using L39337, tran_curr_desc$
L39337:                          FMT POS(5), CH(32)
L39340:              accttype$=ttype$
                     type$ = "UNDEFINED"
                     if accttype$ = "$" then type$ = "CASH"
                     if accttype$ = "A" then type$ = "ASSET"
                     if accttype$ = "L" then type$ = "LIABILITY"
                     if accttype$ = "M" then type$ = "MISC LIABILITY"
                     if accttype$ = "N" then type$ = "NON-AP LIAB."
                     if accttype$ = "C" then type$ = "CAPITAL"
                     if accttype$ = "R" then type$ = "REVENUE"
                     if accttype$ = "E" then type$ = "EXPENSE"
                     put line$(line%), using L39464, invoice$, xixacct$,  ~
                         type$, accountdescr$, amount
                     if curr$ <> "N" then                                ~
                     put linec$(line%), using L39464, invoice$, xixacct$, ~
                         type$, tran_curr_desc$, tran_pmt
                     if line% < 20 then L39296

L39404:     REM Display this line...
                if flag% = 0 then return
                gosub check_line_item_screen
                      if keyhit%  =  0 then       L39440
                      if keyhit%  =  2 then       L39244
                      if keyhit%  = 14 then gosub print_entire_check
                      if keyhit%  = 16 then       return
                         goto L39404

L39440:      REM Branch to next line...
                 flag% = 0
                 line% = 5   /* 5+1 = 6 COMPARE W/CONTROL CHECK DISLAY */
                 str(line$(), 1) = str(line$(), 1186)
                 goto L39296

L39464: % ################!############!#########!#######################~
        ~###!-#######.##

        print_entire_check
            if curr$ = "N" then L39548
               readcsh$ = str(vencode$) & str(checknumber$)
               call "READ100" (#17, readcsh$, f1%(17))
               if f1%(17) = 0 then L39536
                  get #17 using L39504, tran_curr_code$, exchg_ratec
L39504:               FMT CH(4), POS(36), PD(14,7)
                  call "CONVERT" (exchg_ratec, 2.7, exchg_ratec$)
               readcurr$ = tran_curr_code$
               call "READ100" (#16, readcurr$, f1%(16))
                  if f1%(16) = 0 then L39544
                     get #16 using L39528, tran_curr_desc$
L39528:                  FMT POS(5), CH(30)
                     goto L39548
L39536:        exchg_ratec$ = "1.00": tran_curr_code$ = stat_curr_code$
               tran_curr_desc$ = stat_curr_desc$ : goto L39548
L39544:        tran_curr_desc$ = " "
L39548:     call "SHOSTAT" ("Printing Check Information")
            call "DATE" addr ("HD", hdrdate$)
            select printer (134)
            print page
            print "D E T A I L E D    C H E C K    P R I N T O U T";
            print tab (85); hdrdate$
            print skip (3)
            print using L38717, "VENDOR CODE             ", vencode$;
            print "---------- AUDIT TRAIL DATES ---------"
            print using L38717, "VENDOR NAME             ", venname$,     ~
                               "CHECK DATE              ", checkdate$
            print using L38717, "CHECK NUMBER            ", checknumber$, ~
                               "PAYABLES POSTING DATE   ", dateposted$
            print using L38723, "DISCOUNT ACCOUNT        ", disacct$,     ~
                                                           disacctdescr$,~
                               "DATE ORIGINALLY INPUT   ", origdate$
            print using L38723, "CASH IN BANK ACCOUNT    ", cshacct$,     ~
                                                           cshacctdescr$,~
                                "ORIGINALLY INPUT BY    ", origuserid$
            print using L38717, "                        ", " ",          ~
                               "DATE LAST MODIFIED      ", lastdate$
            print using L38735, "GROSS CHECK AMOUNT      ", grosscheck,   ~
                        "  " , "LAST MODIFIED BY        ", lastuserid$
            print using L38735, "LESS DISCOUNT           ", discount
            print using L38735, "                        ","=============="

            if curr$ = "N" then L39680
            print using L38737, "NET CHECK               ", netcheck,     ~
                "TRANSACTION CURRENCY", tran_curr_desc$
            print using L38763, " ", "EXCHANGE RATE ", exchg_ratec$,      ~
                tran_curr_code$, "/", stat_curr_code$
                goto L39688

L39680:     print using L38735, "NET CHECK               ", netcheck

L39688:     REM Line items...
                checkdetailkey$ = nextcheckkey$    /* SET LINE ITEM KEY*/

                print skip (2)           /* PRINT HEADER LINES         */
            if curr$ = "N" then                                          ~
               print    "INVOICES PAID:";                                ~
            else print using L38775, "INVOICES PAID:", "-- ALL AMOUNTS IN ~
        ~STATUTORY CURRENCY --"
                print skip (0), "______________"
                print
                print using L39844
                print using L39852
                line% = 27

            REM Plow each invoice paid by this check...
L39752:         call "PLOWNEXT" (#8, checkdetailkey$, 17%, f1%(8))
                     if f1%(8) = 1 then L39764
                        close printer: pagenumber% = 0% : return
L39764:              get #8, using L32140, invoice$, xixacct$, accttype$, ~
                             amount, tempdate$
                         amount = round(amount,2%)
                     call "DESCRIBE" (#2, xixacct$,accountdescr$, 0%,    ~
                                                                  f1%(2))
                        call "GLFMT" (xixacct$)
                        gosub'100 (xixacct$,accttype$)
                        accttype$=ttype$
                     type$ = "(UNDEFINED)"
                     if accttype$ = "$" then type$ = "(CASH)"
                     if accttype$ = "A" then type$ = "(ASSET)"
                     if accttype$ = "L" then type$ = "(LIABILITY)"
                     if accttype$ = "M" then type$ = "(MISC LIABILITY)"
                     if accttype$ = "N" then type$ = "(NON-AP LIAB.)"
                     if accttype$ = "C" then type$ = "(CAPITAL)"
                     if accttype$ = "R" then type$ = "(REVENUE)"
                     if accttype$ = "E" then type$ = "(EXPENSE)"
                     gosub control_print_entire_check
                     print using L39860, invoice$, xixacct$, accttype$,   ~
                         type$, accountdescr$, amount
                     goto L39752          /* GET NEXT CHECK DETAIL      */

L39844: % INVOICE NO.       ACCOUNT       ACCOUNT  TYPE        ACCOUNT  D~
        ~ESCRIPTION           AMOUNT PAID
L39852: % ---------------  ------------   - -----------  ----------------~
        ~----------------     -----------
L39860: % ################ ############   # ###########  ################~
        ~################    -########.##

        REM *************************************************************~
            *          D I S P L A Y   M A I N    S C R E E N           *~
            *                                                           *~
            * GETS A RANGE OF CODE NUMBERS TO BE PRINTED, AND SELECTS   *~
            * THE TYPE OF REPORT DESIRED.                               *~
            *************************************************************

        control_screen
            accept                                                       ~
               at (01,02),                                               ~
                  "SELECT PAYABLES REPORT",                              ~
               at (01,43),                                               ~
                  "Payables Date:",                                      ~
               at (01,58), fac(payfac$), screendate$            , ch(08),~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), message$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Vendor Code",                                   ~
               at (06,30), fac(hex(81)), firstvend$             , ch(09),~
               at (07,02),                                               ~
                  "Last Vendor Code",                                    ~
               at (07,30), fac(hex(81)), lastvend$              , ch(09),~
               at (09,07),                                               ~
                  "Select Report Type:",                                 ~
               at (10,12),                                               ~
                  "P.F. Key      ----------- Function ----------",       ~
               at (11,15), fac(hex(8c)), keys$(1)               , ch(50),~
               at (12,15), fac(hex(8c)), keys$(2)               , ch(50),~
               at (13,15), fac(hex(8c)), keys$(3)               , ch(50),~
               at (14,15), fac(hex(8c)), keys$(4)               , ch(50),~
               at (15,15), fac(hex(8c)), keys$(5)               , ch(50),~
               at (16,15), fac(hex(8c)), keys$(6)               , ch(50),~
               at (17,15), fac(hex(8c)), keys$(7)               , ch(50),~
               at (18,15), fac(hex(8c)), keys$(8)               , ch(50),~
               at (19,15), fac(hex(8c)), keys$(9)               , ch(50),~
               at (20,15), fac(hex(8c)), keys$(10)              , ch(50),~
               at (21,15), fac(hex(8c)), keys$(11)              , ch(50),~
               at (22,02), fac(hex(84)), text1$                 , ch(79),~
               at (23,02), fac(hex(84)), text2$                 , ch(79),~
               at (24,02), fac(hex(84)), text3$                 , ch(79),~
                                                                         ~
               keys(okkeys$),                                            ~
               key (mainscreen%)

               if mainscreen% <> 13 then L40440
                call "MANUAL" ("PAYDSPLY")
                goto control_screen

L40440:        if mainscreen% <> 14 then L40480
                  payfac$ = hex(81)
                  goto control_screen

L40480:        if mainscreen% <> 15 then L40520
                  call "PRNTSCRN"
                  goto control_screen

L40520:        if mainscreen% <> 0  then return
                  if payfac$ <> hex(81) then getcode_on_vendor
                  errormsg$ = " "
                  call "DATEOK" (screendate$, u3%, errormsg$)
                  if errormsg$ = " " then payfac$ = hex(a4)
                  goto control_screen


        getcode_on_vendor
                  if firstvend$ <> "?" then L40620
                  call "GETCODE" ( #3, firstvend$, " ", 0%, 0, f1%(3%))
                      goto control_screen

L40620:           if lastvend$ <> "?" then goto control_screen
                  call "GETCODE" ( #3, lastvend$, " ", 0%, 0, f1%(3%))
                      goto control_screen

        get_cursor_position
                  close ws
                  call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
                          u3% = u3%
                  return

        REM *************************************************************~
            *   D I S P L A Y   R E C E I V A B L E S   S U M M A R Y   *~
            *                                                           *~
            * DISPLAY VENDOR'S   SUMMARY PAGE AND TOSS ONTO SCREEN.     *~
            *************************************************************

        summary_screen
            if curr$ = "N" then L41055
               lfac$(1) = hex(8c)  :  lfac$(2) = hex(84)
               goto L41065
L41055:     lfac$(1), lfac$(2) = hex(9c)

L41065:     accept                                                       ~
               at (01,02), fac(hex(ac)), pfkeysummary$(mode%)   , ch(79),~
               at (03,02), fac(hex(84)), venname$               , ch(30),~
               at (03,37), "Vendor Code:",                               ~
               at (03,50), fac(hex(84)), vencode$               , ch(09),~
               at (04,02), fac(hex(84)), address$(1)            , ch(30),~
               at (05,02), fac(hex(84)), address$(2)            , ch(30),~
               at (05,37), "PHONE #: ",                                  ~
               at (05,52), fac(hex(84)), phone$                 , ch(20),~
               at (06,02), fac(hex(84)), address$(3)            , ch(30),~
                                                                         ~
                  /* NOW DISPLAY SUMMARY FOR VENDOR                    */~
               at (08,24), fac(lfac$(1)), sum_label$            , ch(34),~
               at (09,05), "-------- Payables Summary --------",         ~
               at (09,44), "------- Misc Liabilities -------",           ~
               at (10,05), "Invoices",                                   ~
               at (10,27), fac(hex(84)),suminvoices(1),pic(-########.##),~
               at (10,44), "Invoices",                                   ~
               at (10,64), fac(hex(84)),suminvoices(2),pic(-########.##),~
               at (11,05), "Applied Checks    (-)",                      ~
               at (11,27), fac(hex(84)), sumapplied(1),pic(-########.##),~
               at (11,44), "Applied Checks",                             ~
               at (11,64), fac(hex(84)), sumapplied(2),pic(-########.##),~
               at (12,05), "Unapplied Checks  (-)",                      ~
               at (12,27),fac(hex(84)),sumunapplied(1),pic(-########.##),~
               at (12,44), "Unapplied Checks",                           ~
               at (12,64),fac(hex(84)),sumunapplied(2),pic(-########.##),~
               at (13,05),fac(lfac$(1)),   gain_loss_label2$    , ch(21),~
               at (13,27),fac(lfac$(2)), sumgainloss  ,pic(-########.##),~
               at (14,27), "============",                               ~
               at (14,64), "============",                               ~
               at (15,05), "Reconcilliation",                            ~
               at (15,27), fac(hex(84)), sumbalance(1),pic(-########.##),~
               at (15,44), "Reconcilliation",                            ~
               at (15,64), fac(hex(84)), sumbalance(2),pic(-########.##),~
               at (16,05), "==================================",         ~
               at (16,44), "================================",           ~
               at (17,24), "Cash Invoices",                              ~
               at (17,46), fac(hex(84)), cashinvoices, pic(-########.##),~
               at (18,24), "Direct Checks",                              ~
               at (18,46), fac(hex(84)), sumdirect   , pic(-########.##),~
               at (19,24), "Applied To Purged Inv",                      ~
               at (19,46), fac(hex(84)), sumapplpurged,pic(-########.##),~
                                                                         ~
                  /* NOW DISPLAY AGING BREAKDOWN                       */~
               at (20,03), "Aging Breakdown as of:",                     ~
               at (20,27), fac(hex(a4)), screendate$            , ch(08),~
               at (21,03),                                               ~
             "+--------------+--------------+--------------+-------------~
        ~-+--------------+",                                              ~
               at (22,05), fac(hex(8c)), period$(1)             , ch(10),~
               at (22,20), fac(hex(8c)), period$(2)             , ch(10),~
               at (22,35), fac(hex(8c)), period$(3)             , ch(10),~
               at (22,50), fac(hex(8c)), period$(4)             , ch(10),~
               at (22,65), fac(hex(8c)), period$(5)             , ch(10),~
               at (23,03),                                               ~
             "+--------------+--------------+--------------+-------------~
        ~-+--------------+",                                              ~
               at (24,05), fac(hex(84)), aging(1)    , pic(-########.##),~
               at (24,20), fac(hex(84)), aging(2)    , pic(-########.##),~
               at (24,35), fac(hex(84)), aging(3)    , pic(-########.##),~
               at (24,50), fac(hex(84)), aging(4)    , pic(-########.##),~
               at (24,65), fac(hex(84)), aging(5)    , pic(-########.##),~
               at (24,78), "!",                                          ~
               at (22,03), "!",                                          ~
               at (22,18), "!",                                          ~
               at (22,33), "!",                                          ~
               at (22,48), "!",                                          ~
               at (22,63), "!",                                          ~
               at (22,78), "!",                                          ~
               at (24,03), "!",                                          ~
               at (24,18), "!",                                          ~
               at (24,33), "!",                                          ~
               at (24,48), "!",                                          ~
               at (24,63), "!",                                          ~
                                                                         ~
               keys(hex(00010203050d0e0f10)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41550
                  call "MANUAL" ("PAYDSPLY")
                  goto summary_screen

L41550:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto summary_screen

        REM *************************************************************~
            *       D I S P L A Y   I N V O I C E   D E T A I L S       *~
            *                                                           *~
            * DISPLAYS INVOICE DETAILS FOR THE VENDOR LEDGER CARD.      *~
            *************************************************************

        invoice_screen
            if mc_flag1$ = "Y" then L42500

                if curr$ = "N" then                                      ~
            title$   =" Invoice#      (Rcv.#) !Date(Post)!  Amount   !Che~
        ~ck  #!   Amount  !  Balance  "                                   ~
                else                                                     ~
            title$   =" Invoice#      (Rcv.#) !Date(Post)!Stat Amount!Che~
        ~ck  #!Stat Amount!  Balance  "

            if mode% = 3% then pfkeyinvoice$(mode%) ="(2)1st (5)Nxt (7)Cu~
        ~r (8)Sum (9)Chks (10)Find Inv (12)Dtl (14)Prt Vend  (16)Menu"
            if mode% = 4% then pfkeyinvoice$(mode%) ="(2)1st (5)Nxt (7)Cu~
        ~r (8)Sum (9)Chks (10)Find Inv (12)Dtl (14)Prt Vend  (16)Exit"

            if mode% = 1% or mode% = 2% then L42066
            if first_screen% <> 1% and curr$ = "N" then                  ~
                                     str(pfkeyinvoice$(mode%),15,6) = " "

L42066:     pfkeys$ = hex(00ff02ffff05ff0708090aff0c0d0e0f10ffffffff)
               if curr$ = "N" then str(pfkeys$, 8,1) = hex(ff)
               if first_screen% <> 1% and curr$ = "N" then               ~
                                      str(pfkeys$, 8,1) = hex(ff)

             accept                                                      ~
               at (01,02), fac(hex(ac)), pfkeyinvoice$(mode%)   , ch(79),~
               at (02,02),                                               ~
                  "Vendor",                                              ~
               at (02,12), fac(hex(84)), vendorcode$            , ch(45),~
               at (02,57), "Phone #:",                                   ~
               at (02,66), fac(hex(8c)), phone$                 , ch(14),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8c)), line$(20)              , ch(79),~
                                                                         ~
                          keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 7 then L42250
                  mc_flag1$ = "Y"
                  goto L42500               /* Toggle to TRANS display */

L42250:        if keyhit% <> 13 then L42270
                  call "MANUAL" ("PAYDSPLY")
                  goto invoice_screen

L42270:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto invoice_screen

L42500:     title$   =" Invoice#      (Rcv.#) !Date(Post)!Tran Amount!Che~
        ~ck  #!Tran Amount!  Balance  "

            if mode% = 3% then pfkeyinvoice$(mode%) ="(2)1st (5)Nxt (7)St~
        ~at (8)Sum (9)Chks (12)Dtl (14)Prt Vend (15)Prt Scrn (16)Menu"
            if mode% = 4% then pfkeyinvoice$(mode%) ="(2)1st (5)Nxt (7)St~
        ~at (8)Sum (9)Chks (12)Dtl (14)Prt Vend (15)Prt Scrn (16)Exit"

            if mode% = 1% or mode% = 2% then L42540
            if first_screen% <> 1%  and curr$ = "N" then                 ~
                                     str(pfkeyinvoice$(mode%),15,7) = " "

L42540:     pfkeys$ = hex(0002050708090c0d0e0f10)
               if first_screen% <> 1% and curr$ = "N" then               ~
                              str(pfkeys$, 4,1) = hex(ff)

L42550:      accept                                                      ~
               at (01,02), fac(hex(ac)), pfkeyinvoice$(mode%)   , ch(79),~
               at (02,02),                                               ~
                  "Vendor",                                              ~
               at (02,12), fac(hex(84)), vendorcode$            , ch(45),~
               at (02,57), "Phone #:",                                   ~
               at (02,66), fac(hex(8c)), phone$                 , ch(14),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), linea$(1)              , ch(79),~
               at (06,02), fac(hex(8c)), lineb$(1)              , ch(79),~
               at (07,02), fac(hex(8c)), linea$(2)              , ch(79),~
               at (08,02), fac(hex(8c)), lineb$(2)              , ch(79),~
               at (09,02), fac(hex(8c)), linea$(3)              , ch(79),~
               at (10,02), fac(hex(8c)), lineb$(3)              , ch(79),~
               at (11,02), fac(hex(8c)), linea$(4)              , ch(79),~
               at (12,02), fac(hex(8c)), lineb$(4)              , ch(79),~
               at (13,02), fac(hex(8c)), linea$(5)              , ch(79),~
               at (14,02), fac(hex(8c)), lineb$(5)              , ch(79),~
               at (15,02), fac(hex(8c)), linea$(6)              , ch(79),~
               at (16,02), fac(hex(8c)), lineb$(6)              , ch(79),~
               at (17,02), fac(hex(8c)), linea$(7)              , ch(79),~
               at (18,02), fac(hex(8c)), lineb$(7)              , ch(79),~
               at (19,02), fac(hex(8c)), linea$(8)              , ch(79),~
               at (20,02), fac(hex(8c)), lineb$(8)              , ch(79),~
               at (21,02), fac(hex(8c)), linea$(9)              , ch(79),~
               at (22,02), fac(hex(8c)), lineb$(9)              , ch(79),~
               at (23,02), fac(hex(8c)), linea$(10)             , ch(79),~
               at (24,02), fac(hex(8c)), lineb$(10)             , ch(79),~
                                                                         ~
               keys(str(pfkeys$)),                                       ~
               key (keyhit%)

               if keyhit% <> 7 then L42725
                  mc_flag1$ = "N"
                  goto invoice_screen   /* Toggle to STAT display */

L42725:        if keyhit% <> 13 then L42745
                  call "MANUAL" ("PAYDSPLY")
                  goto L42550

L42745:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42550

        REM *************************************************************~
            *         D I S P L A Y   C H E C K   D E T A I L S         *~
            *                                                           *~
            * DISPLAYS THE LINE ITEM ARRAY LINE$() FOR CHECKS ON THE    *~
            * VENDOR LEDGER                                             *~
            *************************************************************

        check_screen
            pfkeys$ = hex(00ff02ffff05ff070809ffff0c0d0e0f10ffffffff)

            if mc_flag1$ = "Y" then L43540

            title$   ="Check  #!Date(Post)! Net Check ! Discount !Invoice~
        ~#! Account    !Dt?     Amount"

            if mode% = 3% then pfkeycheck$(mode%) ="(2)1st (5)Nxt (7)Tran~
        ~ (8)Sum (9)Inv (12)Dtl (14)Prt Vend (15)Prt Scrn (16)Menu"

            if mode% = 4% then pfkeycheck$(mode%) ="(2)1st (5)Nxt (7)Tran~
        ~ (8)Sum (9)Inv (12)Dtl (14)Prt Vend (15)Prt Scrn (16)Exit"

               if curr$ = "N" then                                       ~
                   str(pfkeys$, 8,1) = hex(ff)

             accept                                                      ~
               at (01,02), fac(hex(ac)), pfkeycheck$(mode%)     , ch(79),~
               at (02,02),                                               ~
                  "Vendor",                                              ~
               at (02,12), fac(hex(84)), vendorcode$            , ch(45),~
               at (02,57), "Phone #:",                                   ~
               at (02,66), fac(hex(8c)), phone$                 , ch(14),~
               at (03,15), fac(hex(84)), checkmessage$          , ch(48),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8c)), line$(20)              , ch(79),~
                                                                         ~
                         keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 7 then L43460
                  mc_flag1$ = "Y"
                  goto L43540         /* Toggle to Transaction Display */

L43460:        if keyhit% <> 13 then L43500
                  call "MANUAL" ("PAYDSPLY")
                  goto check_screen

L43500:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto check_screen

L43540:     title$   ="Check  #!Date(Post)! Net Check ! Discount !Invoice~
        ~#! Curr. Code !Dt?     Amount"

            if mode% = 3% then pfkeycheck$(mode%) ="(2)1st (5)Nxt (7)Stat~
        ~ (8)Sum (9)Inv (12)Dtl (14)Prt Vend (15)Prt Scrn (16)Menu"

            if mode% = 4% then pfkeycheck$(mode%) ="(2)1st (5)Nxt (7)Stat~
        ~ (8)Sum (9)Inv (12)Dtl (14)Prt Vend (15)Prt Scrn (16)Exit"

             accept                                                      ~
               at (01,02), fac(hex(ac)), pfkeycheck$(mode%)     , ch(79),~
               at (02,02),                                               ~
                  "Vendor",                                              ~
               at (02,12), fac(hex(84)), vendorcode$            , ch(45),~
               at (02,57), "Phone #:",                                   ~
               at (02,66), fac(hex(8c)), phone$                 , ch(14),~
               at (03,15), fac(hex(84)), checkmessage$          , ch(48),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), linea$( 1)             , ch(79),~
               at (06,02), fac(hex(8c)), linea$( 2)             , ch(79),~
               at (07,02), fac(hex(8c)), linea$( 3)             , ch(79),~
               at (08,02), fac(hex(8c)), linea$( 4)             , ch(79),~
               at (09,02), fac(hex(8c)), linea$( 5)             , ch(79),~
               at (10,02), fac(hex(8c)), linea$( 6)             , ch(79),~
               at (11,02), fac(hex(8c)), linea$( 7)             , ch(79),~
               at (12,02), fac(hex(8c)), linea$( 8)             , ch(79),~
               at (13,02), fac(hex(8c)), linea$( 9)             , ch(79),~
               at (14,02), fac(hex(8c)), linea$(10)             , ch(79),~
               at (15,02), fac(hex(8c)), linea$(11)             , ch(79),~
               at (16,02), fac(hex(8c)), linea$(12)             , ch(79),~
               at (17,02), fac(hex(8c)), linea$(13)             , ch(79),~
               at (18,02), fac(hex(8c)), linea$(14)             , ch(79),~
               at (19,02), fac(hex(8c)), linea$(15)             , ch(79),~
               at (20,02), fac(hex(8c)), linea$(16)             , ch(79),~
               at (21,02), fac(hex(8c)), linea$(17)             , ch(79),~
               at (22,02), fac(hex(8c)), linea$(18)             , ch(79),~
               at (23,02), fac(hex(8c)), linea$(19)             , ch(79),~
               at (24,02), fac(hex(8c)), linea$(20)             , ch(79),~
                                                                         ~
                         keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 7 then L43750
                  mc_flag1$ = "N"
                  goto check_screen  /* Toggle to Statutory Display   */

L43750:        if keyhit% <> 13 then L43770
                  call "MANUAL" ("PAYDSPLY")
                  goto L43540

L43770:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43540

        REM *************************************************************~
            *        D I S P L A Y   D E T A I L E D    A G I N G       *~
            *                                                           *~
            * DISPLAYS THE LINE ITEM ARRAY FOR THE DETAILED AGING       *~
            * SCHEDULE                                                  *~
            *************************************************************

        detail_aging_screen
            pfkey$="(2)First (5)Next (12)Display Detail (14)Print Vendor ~
        ~  (15)Prnt Screen (16)Menu"
            put title$, using L44130, period$(1), period$(2), period$(3), ~
                                     period$(4), period$(5)
L44130: %Invoice !   Date   ! ##########! ##########! ##########! #######~
        ~###! ##########

        message$= "To See Ledger or Invoice, Position Cursor at VEND. COD~
        ~E or INVOICE, Then PF(12)"

             accept                                                      ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (02,02), fac(hex(84)), message$               , ch(79),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8c)), line$(20)              , ch(79),~
                                                                         ~
               keys(hex(0002050c0d0e0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L44510
                  call "MANUAL" ("PAYDSPLY")
                  goto detail_aging_screen

L44510:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto detail_aging_screen

        REM *************************************************************~
            *        D I S P L A Y   S U M M A R Y     A G I N G        *~
            *                                                           *~
            * DISPLAYS THE LINE ITEM ARRAY FOR THE SUMMARY AGING        *~
            * SCHEDULE                                                  *~
            *************************************************************

        summary_aging_screen
            pfkey$="(2)First (5)Next (12)Display Detail (14)Print Vendor ~
        ~  (15)Prnt Screen (16)Menu"

            if curr$ = "N" then                                          ~
                put title$, using L45130, period$(1), period$(2),         ~
                                     period$(3), period$(4), period$(5)  ~
            else                                                         ~
                put title$, using L45142, period$(1), period$(2),         ~
                                     period$(3), period$(4), period$(5)

L45130: %     Total     ! ########## ! ########## ! ########## ! ########~
        ~## ! ##########
L45142: %Statutory Total! ########## ! ########## ! ########## ! ########~
        ~## ! ##########

        message$= "To See Vendor Ledger, Position Cursor on VENDOR LINE, ~
        ~Then Press PF(12)"

             accept                                                      ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (02,02), fac(hex(84)), message$               , ch(79),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8c)), line$(20)              , ch(79),~
                                                                         ~
               keys(hex(0002050c0d0e0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L45500
                  call "MANUAL" ("PAYDSPLY")
                  goto summary_aging_screen

L45500:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto summary_aging_screen

        REM *************************************************************~
            * D I S P L A Y   E N T I R E   I N V O I C E   H E A D E R *~
            *                                                           *~
            * DISPLAYS THE ENTIRE HEADER OF AN INVOICE                  *~
            *************************************************************

        invoice_header_first_screen
            if curr$ = "N" then  L46085
                if str(lineb$(idx%),2%,4%) = " " then L46085
                curr_code$ = str(lineb$(idx%),2,4)
                if curr_code$ = stat_curr_code$ then L46085

                lfac$(1) = hex(8c) : lfac$(2) = hex(84)
                curr_desc$ = str(lineb$(idx%),7,28)
                    tran_curr_desc$ = curr_desc$
                call "PUTPAREN" (curr_desc$)
                goto L46090
L46085:     lfac$(1), lfac$(2) = hex(9c)
                tran_curr_desc$ = stat_curr_desc$
L46090:     pfkey$="(2)Lines  (5)Next Page  (8)Text    (14)Print Invoice ~
        ~ (15)Prnt Scrn  (16)Return"

L46105: accept                                                           ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (04,02),                                               ~
                  "Vendor Code",                                         ~
               at (04,30), fac(hex(84)),     vencode$           , ch(09),~
               at (05,02),                                               ~
                  "Invoice Number",                                      ~
               at (05,30), fac(hex(84)),     invoicenr$         , ch(16),~
               at (06,02),                                               ~
                  "Receiver Number",                                     ~
               at (06,30), fac(hex(84)),     receiver$          , ch(16),~
               at (07,02), fac(lfac$(1)),    curr_label$        , ch(13),~
               at (07,30), fac(lfac$(2)),    curr_code$         , ch(04),~
               at (07,49), fac(lfac$(1)),    curr_desc$         , ch(32),~
               at (09,02),                                               ~
                  "Vendor Name",                                         ~
               at (09,30), fac(hex(8c)),     venname$           , ch(30),~
               at (10,02),                                               ~
                  "Vendor Address  (1)",                                 ~
               at (10,30), fac(hex(8c)),     address$( 1)       , ch(30),~
               at (11,02),                                               ~
                  "Vendor Address  (2)",                                 ~
               at (11,30), fac(hex(8c)),     address$( 2)       , ch(30),~
               at (12,02),                                               ~
                  "Vendor Address  (3)",                                 ~
               at (12,30), fac(hex(8c)),     address$( 3)       , ch(30),~
               at (13,02),                                               ~
                  "Purchases Account Default"                           ,~
               at (13,30), fac(hex(8c)),     slsacct$           , ch(12),~
               at (13,49), fac(hex(8c)),     slsacctdescr$      , ch(32),~
               at (14,02),                                               ~
                  "Payables Account",                                    ~
               at (14,30), fac(hex(8c)),     payacct$           , ch(12),~
               at (14,49), fac(hex(8c)),     payacctdescr$      , ch(32),~
               at (15,02),                                               ~
                  "Payables Account Type",                               ~
               at (15,30), fac(hex(8c)),      payaccttype$      , ch(01),~
               at (15,49), fac(hex(8c)),      type$             , ch(16),~
               at (16,02), fac(lfac$(1)),     gain_loss_label1$ , ch(18),~
               at (16,30), fac(lfac$(1)),     gain_loss_acct$   , ch(12),~
               at (17,02),                                               ~
                  "Due Date W/O Discount",                               ~
               at (17,30), fac(hex(8c)),      duedate$          , ch(08),~
               at (18,02),                                               ~
                  "Due Date With Discount",                              ~
               at (18,30), fac(hex(8c)),      discdate$         , ch(08),~
               at (19,02),                                               ~
                  "Free Text Field",                                     ~
               at (19,30), fac(hex(8c)),      freetest$         , ch(20),~
               at (20,02),                                               ~
                  "Hold Payment Flag",                                   ~
               at (20,30), fac(hex(94)),      holdflag$         , ch(01),~
                                                                         ~
               keys (hex(000205080d0f0e10)),                             ~
               key  (keyhit%)

               if keyhit% <> 8 then L46410
                  message$ = "Free Text For Invoice: " & invoicenr$
                  call "TXTDSPLY" (#12, 0%, "011",message$,text$,text$())
                  goto L46105

L46410:        if keyhit% <> 13 then L46430
                  call "MANUAL" ("PAYDSPLY")
                  goto L46105

L46430:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L46105


        invoice_header_second_screen
            if curr$ = "N" then L46481          /* MC not used - Bypass */
               if nondiscamount = 0% then L46467
                  readpay$ = str(vencode$) & str(invoicenr$)
                  call "PLOWNEXT" (#18, readpay$, 25%, f1%(18))
                  if f1%(18) = 0% then L46476
                     get #18 using L46463, header_curr$, conv_fac
L46463:                  FMT CH(4), POS(63), PD(14,7)
                     header_nodisc = round(nondiscamount * conv_fac, 2%)
                     call "CONVERT" (header_nodisc, 2.2, header_nodisc$)
                     goto L46468
L46467:        header_nodisc$ = "      0.00"
L46468:        x% = 1%
L46469:        if x% > 20% then L46473
                  if invoicenr$ = str(linea$(x%),1,16) then L46473
                     x% = x% + 1%
                     goto L46469
L46473:        header_invamt$  = str(linea$(x%),37,10)
               header_balance$ = str(linea$(x%),70,10)
               header_curr$    = str(lineb$(x%),2,4)    :   goto L46479
L46476:        call "CONVERT" (nondiscamount, 2.2, header_nodisc$)
               call "CONVERT" (invamt, 2.2, header_invamt$)
               call "CONVERT" (balance, 2.2, header_balance$)
L46479:           lfac$(1) = hex(8c)  :  goto L46481
                  lfac$(1) = hex(9c)
L46481:     pfkey$="(2)Lines  (4)Prev Page  (8)Text    (14)Print Invoice ~
        ~ (15)Prnt Scrn  (16)Return"
L46483:     accept                                                       ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (04,02), "Vendor Code",                                ~
               at (04,30), fac(hex(84)), vencode$               , ch(09),~
               at (05,02), "Invoice Number",                             ~
               at (05,30), fac(hex(84)), invoicenr$             , ch(16),~
               at (06,02), "Receiver Number",                            ~
               at (07,30), fac(lfac$(1)), header_label1$        , ch(31),~
               at (06,30), fac(hex(84)), receiver$              , ch(16),~
               at (08,02), "Non-Discountable Amount",                    ~
               at (08,31),fac(hex(8c)), nondiscamount, pic(-########.##),~
               at (08,51), fac(lfac$(1)), header_nodisc$        , ch(10),~
               at (09,02), "Net Invoice Amount",                         ~
               at (09,31), fac(hex(8c)), invamt      , pic(-########.##),~
               at (09,51), fac(lfac$(1)), header_invamt$        , ch(10),~
               at (10,02), "Current Outstanding Balance",                ~
               at (10,31), fac(hex(8c)), balance     , pic(-########.##),~
               at (10,51), fac(lfac$(1)), header_balance$       , ch(10),~
               at (10,62), fac(lfac$(1)), header_curr$          , ch(04),~
               at (11,02), "Hold Payment Flag",                          ~
               at (11,41), fac(hex(94)),      holdflag$         , ch(01),~
               at (12,02), "1099 Category",                              ~
               at (12,30), fac(hex(84)),      ten99$            , ch(04),~
               at (17,02), "----------AUDIT TRAIL DATES ----------"     ,~
               at (18,02), "Invoice Date",                               ~
               at (18,30), fac(hex(8c)), invoicedate$           , ch(08),~
               at (19,02), "Payable Posting Date",                       ~
               at (19,30), fac(hex(8c)), dateposted$            , ch(08),~
               at (20,02), "Date Originally Input",                      ~
               at (20,30), fac(hex(8c)), origdate$              , ch(08),~
               at (21,02), "Originally Input By",                        ~
               at (21,30), fac(hex(8c)), origuserid$            , ch(03),~
               at (22,02), "Date Last Modified",                         ~
               at (22,30), fac(hex(8c)), lastdate$              , ch(08),~
               at (23,02), "Last Modified By",                           ~
               at (23,30), fac(hex(8c)), lastuserid$            , ch(03),~
                                                                         ~
               keys (hex(000204080d0e0f10)),                             ~
               key  (keyhit%)

               if keyhit% <> 8 then L46760
                  message$ = "Free Text For Invoice: " & invoicenr$
                  call "TXTDSPLY" (#12, 0%, "011",message$,text$,text$())
                  goto L46483

L46760:        if keyhit% <> 13 then L46780
                  call "MANUAL" ("PAYDSPLY")
                  goto L46483

L46780:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L46483

        REM *************************************************************~
            *           D I S P L A Y    L I N E    I T E M S           *~
            *                                                           *~
            * DISPLAYS LINE ITEMS FOR AN INVOICE                        *~
            *************************************************************

        invoice_line_item_screen
            curr_desc$ = " "
            if mc_flag1$ = "Y" then L47501

            if curr$ <> "N" then L47045

            pfkey$="(2)Header Page  (5)Next Page  (14)Print Invoice  (15)~
        ~Print Screen  (16)EXIT    "

            goto L47054

L47045:     pfkey$="(2)Header Page (5)Next Page (7)Tran (14)Print Invoice~
        ~ (15)Print Screen (16)EXIT"

                curr_desc$ = stat_curr_desc$

L47054:     pfkeys$=hex(00ff02ffff05ff07ffffffffff0d0e0f10ffffffff)
            if curr$ = "N" then str(pfkeys$,8,1) = hex(ff)

            accept                                                       ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (02,02), "Invoice Number: ",                           ~
               at (02,19), fac(hex(84)), invoicenr$             , ch(16),~
               at (02,44), "Vendor:",                                    ~
               at (02,52), fac(hex(84)), vencode$               , ch(09),~
               at (03,02), fac(hex(84)), curr_desc$             , ch(32),~
               at (05,02), fac(hex(84)),    separator$(1)       , ch(79),~
               at (09,02), fac(hex(84)),    separator$(2)       , ch(79),~
               at (13,02), fac(hex(84)),    separator$(3)       , ch(79),~
               at (17,02), fac(hex(84)),    separator$(4)       , ch(79),~
               at (21,02), fac(hex(84)),    separator$(5)       , ch(79),~
               at (06,06), "Acct"                                       ,~
               at (10,06), "Acct"                                       ,~
               at (14,06), "Acct"                                       ,~
               at (18,06), "Acct"                                       ,~
               at (22,06), "Acct"                                       ,~
               at (06,11), fac(hex(84)), acct$(1)               , ch(12),~
               at (10,11), fac(hex(84)), acct$(2)               , ch(12),~
               at (14,11), fac(hex(84)), acct$(3)               , ch(12),~
               at (18,11), fac(hex(84)), acct$(4)               , ch(12),~
               at (22,11), fac(hex(84)), acct$(5)               , ch(12),~
               at (06,27), "Part"                                       ,~
               at (10,27), "Part"                                       ,~
               at (14,27), "Part"                                       ,~
               at (18,27), "Part"                                       ,~
               at (22,27), "Part"                                       ,~
               at (06,33), fac(hex(84)), part$(1)               , ch(25),~
               at (10,33), fac(hex(84)), part$(2)               , ch(25),~
               at (14,33), fac(hex(84)), part$(3)               , ch(25),~
               at (18,33), fac(hex(84)), part$(4)               , ch(25),~
               at (22,33), fac(hex(84)), part$(5)               , ch(25),~
               at (06,59), "Store",                                      ~
               at (10,59), "Store",                                      ~
               at (14,59), "Store",                                      ~
               at (18,59), "Store",                                      ~
               at (22,59), "Store",                                      ~
               at (06,65), fac(hex(84)),store$(1%)        ,ch(03),       ~
               at (10,65), fac(hex(84)),store$(2%)        ,ch(03),       ~
               at (14,65), fac(hex(84)),store$(3%)        ,ch(03),       ~
               at (18,65), fac(hex(84)),store$(4%)        ,ch(03),       ~
               at (22,65), fac(hex(84)),store$(5%)        ,ch(03),       ~
               at (06,70), "Lot", at(10,70), "Lot", at(14,70), "Lot",    ~
               at (18,70), "Lot", at(22,70), "Lot",                      ~
               at (06,74), fac(hex(84)), lot$(1%)        , ch(06),       ~
               at (10,74), fac(hex(84)), lot$(2%)        , ch(06),       ~
               at (14,74), fac(hex(84)), lot$(3%)        , ch(06),       ~
               at (18,74), fac(hex(84)), lot$(4%)        , ch(06),       ~
               at (22,74), fac(hex(84)), lot$(5%)        , ch(06),       ~
               at (07,06), "Quan"                                       ,~
               at (11,06), "Quan"                                       ,~
               at (15,06), "Quan"                                       ,~
               at (19,06), "Quan"                                       ,~
               at (23,06), "Quan"                                       ,~
                     at (08,06), "PO",                                   ~
               at (07,14), fac(hex(84)), qtysold$ (1)           , ch(10),~
               at (11,14), fac(hex(84)), qtysold$ (2)           , ch(10),~
               at (15,14), fac(hex(84)), qtysold$ (3)           , ch(10),~
               at (19,14), fac(hex(84)), qtysold$ (4)           , ch(10),~
               at (23,14), fac(hex(84)), qtysold$ (5)           , ch(10),~
                     at (07,27), "Price",                                ~
                     at (11,27), "Price",                                ~
                     at (15,27), "Price",                                ~
                     at (19,27), "Price",                                ~
                     at (23,27), "Price",                                ~
              at (07,33), fac(hex(84)), price$(1%)        , ch(10),      ~
              at (11,33), fac(hex(84)), price$(2%)        , ch(10),      ~
              at (15,33), fac(hex(84)), price$(3%)        , ch(10),      ~
              at (19,33), fac(hex(84)), price$(4%)        , ch(10),      ~
              at (23,33), fac(hex(84)), price$(5%)        , ch(10),      ~
               at (07,59), "Extension"                                  ,~
               at (11,59), "Extension"                                  ,~
               at (15,59), "Extension"                                  ,~
               at (19,59), "Extension"                                  ,~
               at (23,59), "Extension"                                  ,~
               at (07,70), fac(hex(84)), extension$ (1)         , ch(10),~
               at (11,70), fac(hex(84)), extension$ (2)         , ch(10),~
               at (15,70), fac(hex(84)), extension$ (3)         , ch(10),~
               at (19,70), fac(hex(84)), extension$ (4)         , ch(10),~
               at (23,70), fac(hex(84)), extension$ (5)         , ch(10),~
               at (08,06), "PO#                  PO Line",               ~
               at (12,06), "PO#                  PO Line",               ~
               at (16,06), "PO#                  PO Line",               ~
               at (20,06), "PO#                  PO Line",               ~
               at (24,06), "PO#                  PO Line",               ~
               at (08,10), fac(hex(84)), po$(1)                 , ch(16),~
               at (12,10), fac(hex(84)), po$(2)                 , ch(16),~
               at (16,10), fac(hex(84)), po$(3)                 , ch(16),~
               at (20,10), fac(hex(84)), po$(4)                 , ch(16),~
               at (24,10), fac(hex(84)), po$(5)                 , ch(16),~
               at (08,35), fac(hex(84)), poline$(1)             , ch(03),~
               at (12,35), fac(hex(84)), poline$(2)             , ch(03),~
               at (16,35), fac(hex(84)), poline$(3)             , ch(03),~
               at (20,35), fac(hex(84)), poline$(4)             , ch(03),~
               at (24,35), fac(hex(84)), poline$(5)             , ch(03),~
               at (08,59), "Job"                                        ,~
               at (12,59), "Job"                                        ,~
               at (16,59), "Job"                                        ,~
               at (20,59), "Job"                                        ,~
               at (24,59), "Job"                                        ,~
               at (08,64), fac(hex(84)), job$ (1)               , ch(08),~
               at (12,64), fac(hex(84)), job$ (2)               , ch(08),~
               at (16,64), fac(hex(84)), job$ (3)               , ch(08),~
               at (20,64), fac(hex(84)), job$ (4)               , ch(08),~
               at (24,64), fac(hex(84)), job$ (5)               , ch(08),~
                                                                         ~
                   keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <>  7 then L47396
                  mc_flag1$ = "Y"
                  goto L47501        /* Toggle to Transaction Display */

L47396:        if keyhit% <> 13 then L47408
                  call "MANUAL" ("PAYDSPLY")
                  goto invoice_line_item_screen

L47408:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto invoice_line_item_screen

L47501:     pfkey$="(2)Header Page (5)Next Page (7)Stat (14)Print Invoice~
        ~ (15)Print Screen (16)EXIT"
            curr_desc$ = tran_curr_desc$

L47513:     accept                                                       ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (02,02), "Invoice Number: ",                           ~
               at (02,19), fac(hex(84)), invoicenr$             , ch(16),~
               at (02,44), "Vendor:",                                    ~
               at (02,52), fac(hex(84)), vencode$               , ch(09),~
               at (03,02), fac(hex(84)), curr_desc$             , ch(32),~
               at (05,02), fac(hex(84)),    separator$(1)       , ch(79),~
               at (09,02), fac(hex(84)),    separator$(2)       , ch(79),~
               at (13,02), fac(hex(84)),    separator$(3)       , ch(79),~
               at (17,02), fac(hex(84)),    separator$(4)       , ch(79),~
               at (21,02), fac(hex(84)),    separator$(5)       , ch(79),~
               at (06,06), "Acct"                                       ,~
               at (10,06), "Acct"                                       ,~
               at (14,06), "Acct"                                       ,~
               at (18,06), "Acct"                                       ,~
               at (22,06), "Acct"                                       ,~
               at (06,11), fac(hex(84)), acct$(1)               , ch(12),~
               at (10,11), fac(hex(84)), acct$(2)               , ch(12),~
               at (14,11), fac(hex(84)), acct$(3)               , ch(12),~
               at (18,11), fac(hex(84)), acct$(4)               , ch(12),~
               at (22,11), fac(hex(84)), acct$(5)               , ch(12),~
               at (06,27), "Part"                                       ,~
               at (10,27), "Part"                                       ,~
               at (14,27), "Part"                                       ,~
               at (18,27), "Part"                                       ,~
               at (22,27), "Part"                                       ,~
               at (06,33), fac(hex(84)), part$(1)               , ch(25),~
               at (10,33), fac(hex(84)), part$(2)               , ch(25),~
               at (14,33), fac(hex(84)), part$(3)               , ch(25),~
               at (18,33), fac(hex(84)), part$(4)               , ch(25),~
               at (22,33), fac(hex(84)), part$(5)               , ch(25),~
               at (06,59), "Store",                                      ~
               at (10,59), "Store",                                      ~
               at (14,59), "Store",                                      ~
               at (18,59), "Store",                                      ~
               at (22,59), "Store",                                      ~
               at (06,65), fac(hex(84)),store$(1%)        ,ch(03),       ~
               at (10,65), fac(hex(84)),store$(2%)        ,ch(03),       ~
               at (14,65), fac(hex(84)),store$(3%)        ,ch(03),       ~
               at (18,65), fac(hex(84)),store$(4%)        ,ch(03),       ~
               at (22,65), fac(hex(84)),store$(5%)        ,ch(03),       ~
               at (06,70), "Lot", at(10,70), "Lot", at(14,70), "Lot",    ~
               at (18,70), "Lot", at(22,70), "Lot",                      ~
               at (06,74), fac(hex(84)), lot$(1%)        , ch(06),       ~
               at (10,74), fac(hex(84)), lot$(2%)        , ch(06),       ~
               at (14,74), fac(hex(84)), lot$(3%)        , ch(06),       ~
               at (18,74), fac(hex(84)), lot$(4%)        , ch(06),       ~
               at (22,74), fac(hex(84)), lot$(5%)        , ch(06),       ~
               at (07,06), "Quan"                                       ,~
               at (11,06), "Quan"                                       ,~
               at (15,06), "Quan"                                       ,~
               at (19,06), "Quan"                                       ,~
               at (23,06), "Quan"                                       ,~
                     at (08,06), "PO",                                   ~
               at (07,14), fac(hex(84)), qtysold$ (1)           , ch(10),~
               at (11,14), fac(hex(84)), qtysold$ (2)           , ch(10),~
               at (15,14), fac(hex(84)), qtysold$ (3)           , ch(10),~
               at (19,14), fac(hex(84)), qtysold$ (4)           , ch(10),~
               at (23,14), fac(hex(84)), qtysold$ (5)           , ch(10),~
                     at (07,27), "Price",                                ~
                     at (11,27), "Price",                                ~
                     at (15,27), "Price",                                ~
                     at (19,27), "Price",                                ~
                     at (23,27), "Price",                                ~
              at (07,33), fac(hex(84)), pricet$(1%)       , ch(10),      ~
              at (11,33), fac(hex(84)), pricet$(2%)       , ch(10),      ~
              at (15,33), fac(hex(84)), pricet$(3%)       , ch(10),      ~
              at (19,33), fac(hex(84)), pricet$(4%)       , ch(10),      ~
              at (23,33), fac(hex(84)), pricet$(5%)       , ch(10),      ~
               at (07,59), "Extension"                                  ,~
               at (11,59), "Extension"                                  ,~
               at (15,59), "Extension"                                  ,~
               at (19,59), "Extension"                                  ,~
               at (23,59), "Extension"                                  ,~
               at (07,70), fac(hex(84)), extensiont$ (1)        , ch(10),~
               at (11,70), fac(hex(84)), extensiont$ (2)        , ch(10),~
               at (15,70), fac(hex(84)), extensiont$ (3)        , ch(10),~
               at (19,70), fac(hex(84)), extensiont$ (4)        , ch(10),~
               at (23,70), fac(hex(84)), extensiont$ (5)        , ch(10),~
               at (08,06), "PO#                  PO Line",               ~
               at (12,06), "PO#                  PO Line",               ~
               at (16,06), "PO#                  PO Line",               ~
               at (20,06), "PO#                  PO Line",               ~
               at (24,06), "PO#                  PO Line",               ~
               at (08,10), fac(hex(84)), po$(1)                 , ch(16),~
               at (12,10), fac(hex(84)), po$(2)                 , ch(16),~
               at (16,10), fac(hex(84)), po$(3)                 , ch(16),~
               at (20,10), fac(hex(84)), po$(4)                 , ch(16),~
               at (24,10), fac(hex(84)), po$(5)                 , ch(16),~
               at (08,35), fac(hex(84)), poline$(1)             , ch(03),~
               at (12,35), fac(hex(84)), poline$(2)             , ch(03),~
               at (16,35), fac(hex(84)), poline$(3)             , ch(03),~
               at (20,35), fac(hex(84)), poline$(4)             , ch(03),~
               at (24,35), fac(hex(84)), poline$(5)             , ch(03),~
               at (08,59), "Job"                                        ,~
               at (12,59), "Job"                                        ,~
               at (16,59), "Job"                                        ,~
               at (20,59), "Job"                                        ,~
               at (24,59), "Job"                                        ,~
               at (08,64), fac(hex(84)), job$ (1)               , ch(08),~
               at (12,64), fac(hex(84)), job$ (2)               , ch(08),~
               at (16,64), fac(hex(84)), job$ (3)               , ch(08),~
               at (20,64), fac(hex(84)), job$ (4)               , ch(08),~
               at (24,64), fac(hex(84)), job$ (5)               , ch(08),~
                                                                         ~
                   keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <>  7 then L47735
                  mc_flag1$ = "N"
                  goto invoice_line_item_screen  /* Statutory Display */

L47735:        if keyhit% <> 13 then L47743
                  call "MANUAL" ("PAYDSPLY")
                  goto L47513

L47743:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L47513


        REM *************************************************************~
            *  D I S P L A Y    E N T I R E    C H E C K    H E A D E R *~
            *                                                           *~
            * DISPLAYS THE ENTIRE HEADER OF A CHECK                     *~
            *************************************************************

        check_header_screen
            if foriegn_curr$ = "N" then lfac$(1) = hex(9c)               ~
               else lfac$(1) = hex(8c)

            pfkey$="(2)Line Items       (14)Print Check        (15)Print ~
        ~Screen       (16)EXIT"

            accept                                                       ~
               at (01,02), fac(hex(ac)),  pfkey$                , ch(79),~
               at (04,02), "Vendor Code",                                ~
               at (04,30), fac(hex(84)),  vencode$              , ch(09),~
               at (05,02), "Check Number",                               ~
               at (05,30), fac(hex(84)),  checknumber$          , ch(08),~
               at (07,02), "Discount Amount",                            ~
               at (07,30), fac(hex(8c)),  disacct$              , ch(12),~
               at (07,49), fac(hex(8c)),  disacctdescr$         , ch(32),~
               at (08,02), "Cash in Bank Account",                       ~
               at (08,30), fac(hex(8c)),  cshacct$              , ch(12),~
               at (08,49), fac(hex(8c)),  cshacctdescr$         , ch(32),~
               at (10,29), fac(lfac$(1)), ck_label1$            , ch(13),~
               at (10,45), fac(lfac$(1)), ck_label2$            , ch(15),~
               at (11,02), "Gross Check",                                ~
               at (11,30), fac(hex(8c)),  grosscheck,  pic(-########.##),~
               at (11,48), fac(lfac$(1)), tran_check,  pic(-########.##),~
               at (11,61), fac(lfac$(1)), curr_code$            , ch(04),~
               at (12,02), "Less Discount",                              ~
               at (12,30), fac(hex(8c)),  discount,    pic(-########.##),~
               at (12,48), fac(lfac$(1)), tran_disc,   pic(-########.##),~
               at (13,29), "=============",                              ~
               at (13,45), fac(lfac$(1)), bal_line$             , ch(15),~
               at (14,02), "Net Check",                                  ~
               at (14,30), fac(hex(8c)),  netcheck,    pic(-########.##),~
               at (14,48), fac(lfac$(1)), tran_net,    pic(-########.##),~
               at (14,61), fac(lfac$(1)), curr_code$            , ch(04),~
               at (18,02), "---------- AUDIT TRAIL DATES ----------"    ,~
               at (19,02), "Check Date",                                 ~
               at (19,30), fac(hex(8c)),  checkdate$            , ch(08),~
               at (20,02), "Payables Posting Date",                      ~
               at (20,30), fac(hex(8c)),  dateposted$           , ch(08),~
               at (21,02), "Date Originally Input",                      ~
               at (21,30), fac(hex(8c)),  origdate$             , ch(08),~
               at (22,02), "Originally Input By",                        ~
               at (22,30), fac(hex(8c)),  origuserid$           , ch(03),~
               at (23,02), "Date Last Modified",                         ~
               at (23,30), fac(hex(8c)),  lastdate$             , ch(08),~
               at (24,02), "Last Modified By",                           ~
               at (24,30), fac(hex(8c)),  lastuserid$           , ch(03),~
                                                                         ~
               keys(hex(00020d0e0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L48640
                  call "MANUAL" ("PAYDSPLY")
                  goto check_header_screen

L48640:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto check_header_screen

        REM *************************************************************~
            *       D I S P L A Y   C H E C K   L I N E   I T E M S     *~
            *                                                           *~
            * DISPLAYS THE LINE ITEMS ON A CHECK.                       *~
            *************************************************************

        check_line_item_screen
            if mc_flag1$ = "Y" then L49550

            if curr$ <> "N" then L49084
            pfkey$="(2)Header Page      (14)Print Check        (15)Print ~
        ~Screen       (16)EXIT"
            title$   ="   Invoice No.   !  Account   !  Type   !  D e s c~
        ~ r i p t i o n   !Amount Paid"
                goto L49102

L49084:     pfkey$="(2)Header Page    (7)Tran    (14)Print Check    (15)P~
        ~rint Screen    (16)Exit"

            title$   ="   Invoice No.   !  Account   !  Type   !  D e s c~
        ~ r i p t i o n   !Stat Amount"

L49102:     pfkeys$ = hex(00ff02ffffffff07ffffffffff0d0e0f10ffffffff)
            if curr$ = "N" then str(pfkeys$, 8,1) = hex(ff)

             accept                                                      ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (02,02),                                               ~
                  "Vendor",                                              ~
               at (02,12), fac(hex(84)), vendorcode$            , ch(45),~
               at (02,52), "Check Number",                               ~
               at (02,66), fac(hex(84)), checknumber$           , ch(14),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8c)), line$(20)              , ch(79),~
                                                                         ~
                         keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <>  7 then L49460
                  mc_flag1$ = "Y"
                  goto L49550         /* Toggle to Transaction Display */

L49460:        if keyhit% <> 13 then L49500
                  call "MANUAL" ("PAYDSPLY")
                  goto check_line_item_screen

L49500:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto check_line_item_screen

L49550:     pfkey$="(2)Header Page    (7)Stat    (14)Print Check    (15)P~
        ~rint Screen    (16)Exit"

            title$   ="   Invoice No.   !  Account   !  Type   !    C u r~
        ~ r e n c y       !Tran Amount"

L49590:      accept                                                      ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (02,02),                                               ~
                  "Vendor",                                              ~
               at (02,12), fac(hex(84)), vendorcode$            , ch(45),~
               at (02,52), "Check Number",                               ~
               at (02,66), fac(hex(84)), checknumber$           , ch(14),~
               at (04,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)), linec$( 1)             , ch(79),~
               at (06,02), fac(hex(8c)), linec$( 2)             , ch(79),~
               at (07,02), fac(hex(8c)), linec$( 3)             , ch(79),~
               at (08,02), fac(hex(8c)), linec$( 4)             , ch(79),~
               at (09,02), fac(hex(8c)), linec$( 5)             , ch(79),~
               at (10,02), fac(hex(8c)), linec$( 6)             , ch(79),~
               at (11,02), fac(hex(8c)), linec$( 7)             , ch(79),~
               at (12,02), fac(hex(8c)), linec$( 8)             , ch(79),~
               at (13,02), fac(hex(8c)), linec$( 9)             , ch(79),~
               at (14,02), fac(hex(8c)), linec$(10)             , ch(79),~
               at (15,02), fac(hex(8c)), linec$(11)             , ch(79),~
               at (16,02), fac(hex(8c)), linec$(12)             , ch(79),~
               at (17,02), fac(hex(8c)), linec$(13)             , ch(79),~
               at (18,02), fac(hex(8c)), linec$(14)             , ch(79),~
               at (19,02), fac(hex(8c)), linec$(15)             , ch(79),~
               at (20,02), fac(hex(8c)), linec$(16)             , ch(79),~
               at (21,02), fac(hex(8c)), linec$(17)             , ch(79),~
               at (22,02), fac(hex(8c)), linec$(18)             , ch(79),~
               at (23,02), fac(hex(8c)), linec$(19)             , ch(79),~
               at (24,02), fac(hex(8c)), linec$(20)             , ch(79),~
                                                                         ~
                         keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <>  7 then L49879
                  mc_flag1$ = "N"
                  goto check_line_item_screen  /* Statutory Display */

L49879:        if keyhit% <> 13 then L49885
                  call "MANUAL" ("PAYDSPLY")
                  goto L49590

L49885:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L49590

        REM *************************************************************~
            *                      T E S T   D A T A                    *~
            *                                                           *~
            * BE SURE THAT THE VENDOR CODE IS ON FILE                   *~
            *************************************************************

        test_vendor_code
                  errormsg$ = " "

                  REM Make sure that vendor is on file...
                      call "GETCODE" (#3, vencode$, " ", 0%, 0, f1%(3))
                      if f1%(3) = 1 then L50140
                         errormsg$ = "Vendor Not on File: " & vencode$
                         return
L50140:       REM Get vendor demographics...
                  phone$ = " "
                  tempphone$ = " "
                          get #3, using L50210, venname$,                 ~
                                  address$(1), address$(2), address$(3), ~
                                  tempphone$
                          creditlimit = 0
L50210:                   FMT XX(39),    /* SKIP VENDOR CODE & DESC    */~
                              3*CH(30),  /* NAME, ADDR 1 & 2           */~
                              XX(60),    /*                            */~
                              CH(30),    /* City, State, Zip           */~
                              XX(20),    /* SKIP CONTACT               */~
                              CH(10)     /* GRAB PHONE NUMBER          */~

                      REM Format all information...
                          vendorcode$=vencode$
                          if venname$ <> " "                             ~
                             then vendorcode$ = vendorcode$ & " ("       ~
                                         & venname$ & ")"
                          if tempphone$ = " " then L50400
                             REM FORMAT PHONE NUMBER (REVERSE $TRAN)
                             str(tempphone$,11,2) = "--"
                             phone$ = hex(0001020a0304050b06070809)
                             tran(phone$,tempphone$)
L50400:                   return

        REM *************************************************************~
            *     T E S T   D A T A   F O R   R A N G E   P R I N T     *~
            *                                                           *~
            * MAKES SURE THAT THE LAST IS NOT LESS THAN THE FIRST,      *~
            * NORMALIZES THE KEYS FOR PLOWING (IN THE CASE OF 'ALL'     *~
            * OR SINGLE VENDOR), ETC.                                   *~
            *************************************************************

        test_range
                  errormsg$ = " "
                  REM Test Payeivables date...
                  call "DATEOK" (screendate$, temp%, errormsg$)
                  if errormsg$ <> " " then return
                     paydate$ = screendate$
                     call "DATUNFMT" (paydate$)
                     call "RPERIODS" (paydate$, cutoffdates$(), period$())
                     payfac$ = hex(a4)   /* DISABLE DATE     */

                  REM "ALL" vendors...
                      if firstvend$ <> "ALL" then L51230
                         init(hex(00)) nextvend$
                         init(hex(ff)) lastvend$
                         return
L51230:           REM Single vendor...
                      if lastvend$ <> " " then L51320
                         vencode$ = firstvend$
                         gosub test_vendor_code
                         if errormsg$ <> " " then return
                         if vencode$ = firstvend$ then L51280
                            firstvend$ = vencode$
L51280:                  lastvend$ = firstvend$
                         nextvend$ = firstvend$ addc all(hex(ff))
                         return
L51320:           REM Range of vendors...
                      if lastvend$ < firstvend$ then L51370
                         nextvend$ = firstvend$ addc all(hex(ff))
                         return
L51370:           REM Error message - Last > First...
                      errormsg$ = "Invalid Range!  Please Respecify."
                      return

        REM Routine to handle valid A/P accounts...

            deffn'100 (ttacct$,ttype$)
                  if ttype$<>"L" then return
                  if apacct$(1%) = " " then L52080
                  search str(apacct$(), 1%, 16%*acctnum%)                ~
                           = str(ttacct$) to hit$ step 16%
                  if hit$ <> hex(0000) then return
                     ttype$ = "M"
L52080:           if nonapacct$(1%) = " " then return
                  search str(nonapacct$(), 1%, 16% * nonacctnum%)        ~
                           = str(ttacct$) to hit$ step 16%
                  if hit$ =  hex(0000) then return
                     ttype$ = "N"
                  return

L55000: REM *************************************************************~
            *        S Y S F I L E 2  E R R O R  S C R E E N            *~
            *                                                           *~
            * DISPLAY SCREEN FOR NO SYSFILE2 "APACCOUNTS" RECORD        *~
            *************************************************************
        accept                                                           ~
            at (02,02), "PAYDSPLY ERROR SCREEN",                         ~
            at (10,06), "SYSFILE2 DOES NOT CONTAIN THE RECORD WITH THE ",~
            at (11,06), "AP LIABILITY ACCOUNTS FOR THIS PROGRAM.  YOU  ",~
            at (12,06), "MAY FIRST EXECUTE 'PAYACCTS' TO ESTABLISH THE ",~
            at (13,06), "AP LIABILITY ACCOUNTS IN SYSFILE2 FOR THIS    ",~
            at (14,06), "PROGRAM.  PRESS RETURN TO CONTINUE.           ",~
                                                                         ~
            keys(hex(00))
            return


L55200: REM *************************************************************~
            *        S Y S F I L E 2  E R R O R  S C R E E N            *~
            *                                                           *~
            * DISPLAY SCREEN FOR NO SYSFILE2 "APACCOUNTS-NON" RECORD    *~
            *************************************************************
        accept                                                           ~
            at (02,02), "PAYDSPLY ERROR SCREEN",                         ~
            at (10,06), "SYSFILE2 DOES NOT CONTAIN THE RECORD WITH THE ",~
            at (11,06), "NON-AP LIABILITY ACCOUNTS FOR THIS PROGRAM.   ",~
            at (12,06), "YOU MAY FIRST EXECUTE 'PAYACCTS' TO ESTABLISH ",~
            at (13,06), "THE NON-AP LIABILITY ACCOUNTS IN SYSFILE2 FOR ",~
            at (14,06), "THIS PROGRAM.  PRESS RETURN TO CONTINUE.      ",~
                                                                         ~
            keys(hex(00))
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            call "SETPRNT" ("A/P002", " ", 0%, 1%)

            end
