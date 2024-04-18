        REM *************************************************************~
            *                                                           *~
            *   CCC    SSS   H   H      J  U   U  RRRR   N   N    1     *~
            *  C   C  S      H   H      J  U   U  R   R  NN  N   11     *~
            *  C       SSS   HHHHH      J  U   U  RRRR   N N N    1     *~
            *  C   C      S  H   H  J   J  U   U  R   R  N  NN    1     *~
            *   CCC    SSS   H   H   JJJ    UUU   R   R  N   N  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CSHJURN1 - TAKES CHECKS, PRINTS THE CASH DISBURSEMENTS   ,*~
            *            JOURNAL, AND POSTS THE CHECKS TO THE G/L.      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/14/80 ! ORIGINAL (FROM NEW CRCJURN1)             ! BCW *~
            * 06/02/81 ! ADDED 16 CHAR INVOICE NUMBER             ! TOM *~
            * 06/23/81 ! ACCOUNT BREAKDOWN PRINTING ERROR         ! TEM *~
            * 07/21/81 ! POSTING IMBALANCE TO GL FIXED            ! TEM *~
            * 08/13/81 ! ROUNDING; BATCH TOTAL                    ! TEM *~
            * 10/06/81 ! SHOW NO POST TO GL ON AUDIT TRAIL        ! TEM *~
            * 03/18/82 ! CORRECT POSTING  AT 12160, GOTO 12215    ! GLW *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 12/16/85 ! Vendor file fmt changes, calls to GLPOST ! MJB *~
            * 04/05/89 ! Added post date & seq # to both headers  ! LAB *~
            * 08/22/89 ! Rounded Gain/Loss amount.                ! JDH *~
            * 08/06/90 ! G/L export file modifications.           ! RAC *~
            * 05/23/91 ! Conditioned Execution of G/L Export code.! JBK *~
            * 08/09/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            *************************************************************

        dim                                                              ~
            account$9,                   /* DUMMY ARG IN PUSH ROUTINES */~
            acct$9,                      /* ACCOUNT NUMBER FROM LINER  */~
            accttype$1,                  /* ACCOUNT TYPE THIS LINE     */~
            aid$1,                       /* AID CHARACTER FROM GETPARM */~
            cashacct$9,                  /* CASH IN BANK ACCT THIS CHK */~
            checkdate$8,                 /* CHECK DATE INFORMATION     */~
            checknr$8,                   /* CHECK NUMBER               */~
            creditsstk$(250)9,           /* CREDIT ACCOUNTS FOR RCP#1  */~
            creditsstk (250),            /* CREDIT AMOUNTS FOR RCP#1   */~
            descr$32,                    /* VENDOR NAME                */~
            lastvendor$9,                /* LAST VENDOR CODE POSTED    */~
            vencode$9,                   /* VENDOR CODE                */~
            debitsstk$(250)9,            /* DEBIT ACCOUNTS FOR RCP#1   */~
            debitsstk (250),             /* DEBIT ACCOUNTS FOR RCP#1   */~
            discacct$9,                  /* DISCOUNT ACCOUNT NUMBER    */~
            diskkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            gain_loss$9,                 /* GAIN/LOSS ACCOUNT          */~
            hdrdate$45,                  /* FORMATTED DATE/TIME        */~
            invoice$16,                  /* INVOICE NUMBER             */~
            jnlid$3,                     /* JOURNAL ID                 */~
            linenumber%(2),              /* LINE POINTERS FOR JURN1    */~
            location$2,                  /* LOCATOR ARRAY FOR PUSH RTNS*/~
            moduleno$20,                 /* MODULE ID                  */~
            netcheck$10,                 /* NET CHECK AMOUNT INFO      */~
            postme$3,                    /* TO POST OR NOT TO POST ... */~
            prtacct$16,                  /* ACCOUNT NUMBER TO PRINT    */~
            prtacctdescr$30,             /* ACCOUNT DESCRIPTION        */~
            prtchecknr$8,                /* CHECK NUMBER TO PRINT      */~
            prtcredit$10,                /* CREDIT AMOUNT TO PRINT     */~
            prtvencode$9,                /* VENDOR CODE TO PRINT       */~
            prtvenname$30,               /* VENDOR NAME TO PRINT       */~
            prtdebit$10,                 /* DEBIT AMOUNT TO PRINT      */~
            rcpacct$(2)16,               /* RCP#1 ACCOUNT NUMBERS      */~
            rcpacctdescr$(2)30,          /* RCP#1 ACCOUNT DESCRIPTIONS */~
            rcpamt(2),                   /* RCP#1 CREDIT/DEBIT AMOUNTS */~
            rcpline%(2),                 /* RCP#1 LINE COUNT POINTERS  */~
            rcpptr%(2),                  /* RCP#1 STACK POINTERS       */~
            paydate$8,                   /* RECEIVABLES DATE THIS USER */~
            stack$(100)9,                /* CREDIT ACCOUNTS THIS CHECK */~
            stack(100),                  /* CREDIT AMOUNTS THIS CHECK  */~
            stackinv$(100)16,            /* INVOICE FOR WHATS IN STACK */~
            summary$1,                   /* SUMMARY (Y OR N)           */~
            text$100,                    /* G/L DETAIL FREE TEXT FIELD */~
            title$50,                    /* JOURNAL TITLE              */~
            title1$50,                   /* JOURNAL TITLE              */~
            userid$3                     /* USERID OF CURRENT USER     */~

        dim                              /* G/L Export Posting Info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            gl_invoice$16,               /* G/L Vendor Invoice         */~
            tran_type$5,                 /* G/L Tranaction type        */~
            ventype$4                    /* Vendor Type code           */

        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */

        dim f1%(64)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************
            mat f1% = zer

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE (ACCT DESCR'S)  *~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 4 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            * # 7 ! CSHMASTR ! CASH DISBURSEMENTS CHECK HEADER FILE     *~
            * # 8 ! CSHLINES ! CASH DISBURSEMENTS CHECK DETAIL FILE     *~
            * # 9 ! CSHBUFFR ! CASH DISBURSEMENTS BUFFER AREA           *~
            * #10 ! CSHBUF2  ! CASH DISBURSEMENTS CHECK DETAIL BUFFER   *~
            * #11 ! SYSFILE2 ! SYSTEM DEFAULT INFORMATION FILE          *~
            * #13 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #14 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
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

            select  #4, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

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
                        alt key 1, keypos = 8, keylen = 7,               ~
                            key 2, keypos =15, keylen = 17,              ~
                            key 3, keypos =24, keylen =  8, dup

            select #10, "CSHBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alt key 1, keypos = 21, keylen= 16, dup

            select #11, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20
/*(AWD001)*/
            select #13, "GLORTRAN",                                       ~
                        varc,     indexed, recsize = 512,                 ~
                        keypos = 1,    keylen = 33,                       ~
                        alt key 1, keypos = 31, keylen = 47,              ~
                            key 2, keypos = 81, keylen = 26

            select #14, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

/*(AWD001)*/

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

            call "SHOSTAT" ("Opening Files, One Moment.")

            call "OPENCHCK" (# 1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 8, 0%, 0%, 0%, " ")
            call "OPENCHCK" (# 9, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, f2%, 0%, " ")
/*(AWD001)*/
            call "OPENCHCK" (#13, 0%, f2%, 100%, " ")
            call "OPENCHCK" (#14, 0%, f2%, 0%, " ")
/*(AWD001)*/
            call "OPENCHCK" (#42, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#43, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#45, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#46, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES KEY FOR THIS USER'S INVOICES IN THE BUFFER,   *~
            * RECEIVABLES DATE, AND OTHER THINGS. ALSO DOES A GETPARM   *~
            * TO FIND OUT IF WE ACTUALLY POST TO THE G/L OR IF WE JUST  *~
            * PRINT THE JOURNAL (VENDOR FILE ONLY PROGRAMS)             *~
            *************************************************************


                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #14, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/


            line% = 1000
            lastvendor$ = " "

            call "EXTRACT" addr ("ID", userid$)
            call "READ100" (#1, userid$, f1%(1))
                 if f1%(1) = 0 then L65000
                 get #1, using L09160 , paydate$
L09160:                  FMT XX(9), CH(6)
            call "WHICHMON" (#11, paydate$, whichmonth%)
                 if whichmonth% = 0 then L65000
            date$ = date
            call "DATEFMT" (date$)
            postdate$ = paydate$
            call "DATEFMT" (postdate$)

            nextcheckkey$ = userid$
            batchtotal,disctotal,grcredittotal,grdebittotal=0

            REM DO A GETPARM TO FIND IF THIS BATCH GETS POSTED TO G/L.
                call "GETPARM" addr ("I ", "R", "POSTME  ", aid$, "0001",~
                                     "CSHJN1",                           ~
                                   "Post This Buffer To General Ledger?",~
                                     35%, "K", "POSTME  ", postme$, 3%,  ~
                                     5%, 32%, "A")

L09300:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ", aid$, "0001",~
                                     "CSHJN1",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09300

                if postme$ = "YES" then returncode% = 0                  ~
                   else returncode% = 1
                moduleno$ = "02"
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                text$, paydate$, #11, f2%, returncode%)
                title$, title1$ = text$
                call "FMTTITLE" (title$, "JOURNAL", 12%)
                call "FMTTITLE" (title1$, "RECAP", 12%)
                call "SETPRNT" ("A/P007", " ", 0%, 0%)
            if postme$ = "YES" then                                      ~
            call "SHOSTAT" ("Posting Cash Disbursements")                ~
                               else                                      ~
            call "SHOSTAT" ("Printing Supplementary Cash Disbursements Jo~
        ~urnal")

*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#11, "SWITCHS.GL", f1%(11))
            if f1%(11) = 1% then get #11 using L09640, export_on$
L09640:         FMT POS(22), CH(1)

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * GET NEXT CHECK IN USER'S BUFFER, PRINT RECAP WHEN WE HIT  *~
            * THE END OF THE BUFFER.                                    *~
            *************************************************************

            call "PLOWNEXT" (#9, nextcheckkey$, 3%, f1%(9))
                  if f1%(9) = 0 then L40000

            get #9, using L10330, vencode$, checknr$
L10330:             FMT XX(14), CH(9), CH(8)
            currency$, descr$ = " "
            if vencode$ <> lastvendor$ then call "READ100"               ~
                  (#3, vencode$, f1%(3))
               if f1%(3) = 0% then L11000
            get #3 using L10480, descr$, ventype$
L10480:     FMT POS(10), CH(30), POS(477), CH(4)

L11000: REM *************************************************************~
            *      U N P O S T   O L D   C H E C K ,   I F   A N Y      *~
            *                                                           *~
            * POST NEGATIVE OF AMOUNTS ON OLD CHECK TO G/L, ETC.        *~
            *************************************************************

            REM FIRST THING TO DO IS TO SEE IF OLD CHECK EVEN EXISTS.
                str(diskkey$,  1) = vencode$
                str(diskkey$, 10) = checknr$
                call "READ100" (#7, diskkey$, f1%(7))
                     if f1%(7) = 0 then L12000      /* NO OLD CHECK     */
                gosub L32000              /* GET OLD CHECK HEADER       */

            REM THEN PUSH THE OLD CHECK ONTO THE STACK, POSTING TO RECAP
                gosub L50000              /* INITIALIZE STACK           */
                netamount,netdiscount=0
                str(diskkey$,  1) = vencode$
                str(diskkey$, 10) = checknr$

L11180:         call "PLOWNEXT" (#8, diskkey$, 17%, f1%(8))
                     if f1%(8) = 0 then L11250
                        gosub L33000      /* LOAD CHECK LINE ITEM       */
                        gosub'160(acct$, -(amount-line_gl))
                        gosub'161(acct$, -(amount-line_gl))
                        netamount=netamount+amount-thisdisc
                        netdiscount=netdiscount+thisdisc
                        netamount = round(netamount,2%)
                        netdiscount = round(netdiscount, 2%)
                        goto L11180

L11250:     REM NOW PROCESS CASH, DISCOUNT TO G/L AND ALSO TO STACKS.
                gosub'162(cashacct$, -netamount)
                if postme$ <> "YES" then L11360
                   text$ = " "
                   str(text$,  1) = str(vencode$) & str(checknr$)
                   str(text$, 65) = "REV-"
                   str(text$, 69) = descr$
                   if netamount<>netcheck then str(text$, 65, 4) =       ~
                                                      "ADJ"
                   tran_type$ = "VXD01"
                   postamt = netamount
                   if export_on$ = "Y" then gosub load_gl_info
                   call "GLPOST2" (cashacct$, 0, -netamount,             ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())

L11360:     REM PROCESS DISCOUNT AMOUNT
                if netdiscount = 0 then L11475
                gosub'162(discacct$, -netdiscount)
                if postme$ <> "YES" then L11475
                   text$ = " "
                   str(text$,  1) = str(vencode$) & str(checknr$)
                   str(text$, 65) = "REV-"
                   str(text$, 69) = descr$
                   if netamount<>netcheck then str(text$, 65, 4) =       ~
                                                      "ADJ"
                   tran_type$ = "VXD02"
                   postamt = netdiscount
                   if export_on$ = "Y" then gosub load_gl_info
                   call "GLPOST2" (discacct$, 0, -netdiscount,           ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())

L11475:     REM PROCESS GAIN_LOSS AMOUNT
                if gain_loss = 0 then L11600
                gosub'161(gain_loss$, -gain_loss)
                if postme$ <> "YES" then L11600
                   text$ = " "
                   str(text$,  1) = str(vencode$) & str(checknr$)
                   str(text$, 65) = "REV-"
                   str(text$, 69) = descr$
                   if netamount<>netcheck then str(text$, 65, 4) =       ~
                                                      "ADJ"
                   tran_type$ = "VXD03"
                   postamt = gain_loss
                   if export_on$ = "Y" then gosub load_gl_info
                   call "GLPOST2" (gain_loss$, -gain_loss, 0,            ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())

L11600:     REM AND NOW PROCESS THE STACK TO THE G/L
                if postme$ <> "YES" then L11780
                if pointer% = 0 then L11780
                   text$ = " "
                   str(text$,  1) = str(vencode$) & str(checknr$)
                   str(text$, 65) = "REV-"
                   str(text$, 69) = descr$
                   if netamount<>netcheck then str(text$, 65, 4) =       ~
                                                      "ADJ"
                   for temp% = 1 to pointer%
                       if summary$ <> "Y" then str(text$,31,34) =        ~
                           str(vencode$) & str(stackinv$(temp%))
                   tran_type$ = "VXD04"
                   postamt = stack(temp%)
                   if export_on$ = "Y" then gosub load_gl_info
                       call "GLPOST2" (stack$(temp%),  stack(temp%), 0,  ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())
                       next temp%

L11780:     REM PRINT CHECK JOURNAL ENTRY FOR OLD CHECK
                netcheck      = -netcheck
                discount      = -discount
                netamount     = -netamount
                netdiscount   = -netdiscount
                gosub L20000

L12000: REM *************************************************************~
            *              P R O C E S S   N E W   C H E C K            *~
            *                                                           *~
            * PROCESS THE NEW CHECK IN MUCH THE SAME WAY AS THE OLD ONE.*~
            * LOOK FOR THE CASE IN WHICH NEW CHECK HAS NO LINE ITEMS.   *~
            *************************************************************

            REM MUST REMEMBER TO GET NEW CHECK INFORMATION
                gosub L30000

            REM PUSH THE ENTRIES FOR THE CHECK TO RECAP & CHECKS
                gosub L50000
                netamount,netdiscount=0
                str(diskkey$,  1) = vencode$
                str(diskkey$, 10) = checknr$

L12150:         call "PLOWNEXT" (#10, diskkey$, 17%, f1%(10))
                     if f1%(10) = 0 then L12220
                        gosub L31000
                        gosub'160(acct$, amount - line_gl)
                        gosub'161(acct$, amount - line_gl)
                        netamount = netamount + amount - thisdisc
                        netdiscount = netdiscount + thisdisc
                        netamount = round(netamount,2%)
                        netdiscount = round(netdiscount,2%)
                        goto L12150

L12220:     REM NOW HANDLE THE CREDIT SIDE--FOOL AROUND W/DISCOUNT, CASH
                gosub'162(cashacct$, netamount)
                gl_invoice$ = " "
                if postme$ <> "YES" then L12330
                if netamount = 0 then L12330
                   text$ = " "
                   str(text$,  1) = str(vencode$) & str(checknr$)
                   str(text$,69) = descr$
                   if netamount<>netcheck then str(text$,65, 4) =        ~
                                                      "ADJ"
                   tran_type$ = "VXD01"
                   postamt = -netamount
                   if export_on$ = "Y" then gosub load_gl_info
                   call "GLPOST2" (cashacct$, 0, netamount,              ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())

L12330:     REM POST NEW DISCOUNT TO G/L
                if netdiscount = 0   then L12460
                   gosub'162(discacct$, netdiscount)
                   if postme$ <> "YES" then L12460
                      text$ = " "
                      str(text$,  1) = str(vencode$) & str(checknr$)
                      str(text$, 69) = descr$
                   if netamount<>netcheck then str(text$,65, 4) =        ~
                                                      "ADJ"
                   tran_type$ = "VXD02"
                   postamt = -netdiscount
                   if export_on$ = "Y" then gosub load_gl_info
                      call "GLPOST2" (discacct$, 0, netdiscount,         ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())

L12460:     REM POST NEW GAIN/LOSS TO G/L
                if gain_loss = 0   then L12600
                   gosub'161(gain_loss$, gain_loss)
                   if postme$ <> "YES" then L12600
                      text$ = " "
                      str(text$,  1) = str(vencode$) & str(checknr$)
                      str(text$, 69) = descr$
                   if netamount<>netcheck then str(text$,65, 4) =        ~
                                                      "ADJ"
                   tran_type$ = "VXD03"
                   postamt = gain_loss
                   if export_on$ = "Y" then gosub load_gl_info
                      call "GLPOST2" (gain_loss$, gain_loss, 0,          ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())

L12600:     REM AND FINALLY, POST THE DETAILS TO THE G/L
                if pointer% =  0     then L12810
                if postme$  <> "YES" then L12810
                   text$ = " "
                   str(text$,  1) = str(vencode$) & str(checknr$)
                   str(text$, 69) = descr$
                   if netamount<>netcheck then str(text$,65,4) =         ~
                                                      "ADJ"
                   for temp% = 1 to pointer%
                       if summary$ <> "Y" then str(text$,31,34) =        ~
                           str(vencode$) & str(stackinv$(temp%))
                   gl_invoice$ = stackinv$(temp%)
                   tran_type$ = "VXD04"
                   postamt = stack(temp%)
                   if export_on$ = "Y" then gosub load_gl_info
                       call "GLPOST2" (stack$(temp%), stack(temp%), 0,   ~
                                   paydate$, 0%, "02", text$, jnlid$,    ~
                                   pstseq%, userid$, division$, #2, #4,  ~
                                   #11, #13,  /* (AWD001) */             ~
                                   returncode%, checknr$, gl_post_info$())
                       next temp%

L12810:     REM PRINT CHECK JOURNAL ENTRY FOR NEW CHECK; GET NEW INV.
                gosub L20000              /* AND DO THE DIRTY WORK.     */
                goto  L10000

        load_gl_info

            put str(gl_post_info$(),,) using L13470,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                currency$,               /* Currency code CH(4)        */~
                convunt,                 /* Currency Unit per Book     */~
                postamt,                 /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                " ",                     /* Part Number CH(25)         */~
                " ",                     /* Part Category CH(4)        */~
                " ",                     /* Part Class CH(4)           */~
                " ",                     /* Part Generic code CH(16)   */~
                " ",                     /* Part Type CH(3)            */~
                " ",                     /* Part UOM CH(4)             */~
                " ",                     /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                vencode$,                /* Vendor code CH(9)          */~
                ventype$,                /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                gl_invoice$,             /* Vendor Invoice CH(16)      */~
                checknr$,                /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                " ",                     /* Employee number CH(12)     */~
                " ",                     /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L13470: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Transaction Currency amount*/~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice CH(16)      */~
                CH(8),                   /* Check Payment number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(191)                  /* Filler                     */

L20000: REM *************************************************************~
            *   P R I N T   C A S H   R E C E I P T S   J O U R N A L   *~
            *                                                           *~
            * PRINTS A LINE OF THE DISBURSEMENTS JOURNAL.  NO BIG DEAL. *~
            *************************************************************
            lastvendor$ = vencode$
            mat linenumber% = con
            debitstotal, creditstotal = 0
            colsdone% = 0
            prtpointer% = 0

L20110:     for column% = 1 to 2
                on column% gosub L21000, L22000
                next column%
                if colsdone% < 2 then L20170
                   print using L29160
                   line% = line% + 1
                   return
L20170:         gosub L28000    /* LINE COUNTER/PAGE HEADING            */
                print using L29190, prtchecknr$, prtvencode$, prtvenname$,~
                                   netcheck$, prtacct$, prtacctdescr$,   ~
                                   prtdebit$, prtcredit$
                goto L20110

L21000:     REM HANDLES FIRST COLUMN OF REPORT.
                on linenumber%(1) gosub L21100, L21200
                return

L21100:         REM PRINT VENDOR, CHECK INFORMATION
                    call "CONVERT" (netcheck, 2.2, netcheck$)
                    prtchecknr$ = checknr$
                    prtvencode$ = vencode$
                    call "DESCRIBE" (#3,prtvencode$,prtvenname$,0%,f1%(3))
                    linenumber%(1) = 2
                    batchtotal = batchtotal + netcheck
                    disctotal = disctotal + discount
                    return
L21200:         REM SET ALL THAT STUFF TO BLANKS
                    netcheck$, prtchecknr$, prtvencode$, prtvenname$=" "
                    colsdone% = colsdone% + 1
                    linenumber%(1) = 3
                    return

L22000:     REM HANDLES SECOND COLUMN OF REPORT.
                on linenumber%(2) gosub L22100, L22200, L22250,             ~
                                        L22300, L22400, L22500
                   return
L22100:         REM PRINT CASH IN BANK DEBIT ACCOUNT INFORMATION
                    prtacct$ = cashacct$
                    call "DESCRIBE"(#2, prtacct$, prtacctdescr$,0%,f1%(2))
                    call "GLFMT" (prtacct$)
                    call "CONVERT" (netamount, 2.2, prtcredit$)
                    creditstotal = creditstotal + netamount
                    grcredittotal=grcredittotal+netamount
                    prtdebit$ = " "
                    linenumber%(2) = 2
                    return
L22200:         REM PRINT DISCOUNT TAKEN CREDIT INFORMATION
                    if netdiscount = 0 then L22250
                    prtacct$ = discacct$
                    call "DESCRIBE" (#2,discacct$,prtacctdescr$,0%,f1%(2))
                    call "GLFMT" (prtacct$)
                    call "CONVERT" (netdiscount, 2.2, prtcredit$)
                    creditstotal = creditstotal + netdiscount
                    grcredittotal=grcredittotal+netdiscount
                    prtdebit$ = "  "
                    linenumber%(2) = 3
                    return
L22250:         REM PRINT GAIN LOSS DEBIT INFORMATION
                    if gain_loss = 0 then L22300
                    prtacct$ = gain_loss$
                    call "DESCRIBE"(#2,gain_loss$,prtacctdescr$,0%,f1%(2))
                    call "GLFMT" (prtacct$)
                    call "CONVERT" (gain_loss, 2.2, prtdebit$)
                    debitstotal = debitstotal + gain_loss
                    grdebittotal=grdebittotal + gain_loss
                    prtcredit$ = "  "
                    linenumber%(2) = 4
                    return
L22300:         REM PRINT STACK OF DEBIT INFORMATION
                    if pointer% = 0 then L22400
                       prtpointer% = prtpointer% + 1
                       prtacct$ = stack$(prtpointer%)
                       call "DESCRIBE" (#2, prtacct$,                    ~
                                         prtacctdescr$, 0%, f1%(2))
                       call "GLFMT" (prtacct$)
                       if summary$ <> "Y" then prtacctdescr$ =           ~
                           stackinv$(prtpointer%)
                       call"CONVERT"(stack(prtpointer%), 2.2, prtdebit$)
                       debitstotal = debitstotal + stack(prtpointer%)
                       grdebittotal=grdebittotal+stack(prtpointer%)
                       prtcredit$ = " "
                       if prtpointer% < pointer% then return
                          linenumber%(2) = 5
                          return
L22400:         REM PRINT TOTAL LINE FOR THIS CHECK
                    prtacct$ = "**TOTAL**"
                    prtacctdescr$=" "
                    call "CONVERT" (debitstotal,  2.2, prtdebit$)
                    call "CONVERT" (creditstotal, 2.2, prtcredit$)
                    linenumber%(2) = 6
                    return
L22500:         REM ZAP ALL THE VARIABLES
                    prtacct$, prtacctdescr$, prtdebit$, prtcredit$=" "
                    colsdone% = colsdone% + 1
                    linenumber%(2) = 7
                    return

L28000:     REM PAGE CONTROL ROUTINE FOR CASH DISBURSEMENTS RECAP.
                line% = line% + 1
                if line% < 60 then return
                   line% = 6
                   select printer(134)
                   print page
                   call "DATE" addr("HD", hdrdate$)
                   crcjurnpage% = crcjurnpage% + 1
                   if postme$ = "YES" then                               ~
                   print using L29000, crcjurnpage%, title$, pstseq%,     ~
                       postdate$, date$                                  ~
                            else                                         ~
                   print using L29032, crcjurnpage%, title$, pstseq%,     ~
                       postdate$, date$
                   print "A/P007"
                   print using L29040
                   print using L29070
                   print using L29100
                   print using L29130, "#"
                   print using L29160
                   return

L29000: %PAGE #####   ###################################################~
        ~####               SEQ: ###  POST DATE: ########  RUN DATE: #####~
        ~###

L29032: %PAGE #####   ###################################################~
        ~#### (NOT POSTED)  SEQ: ###  POST DATE: ########  RUN DATE: #####~
        ~###

L29040: %   +--------+---------+------------------------------+----------~
        ~-----------------------------------------------------------------~
        ~-+
L29070: %   !  CHECK ! VENDOR  !                              !   NET    ~
        ~!               A C C O U N T  B R E A K D O W N                 ~
        ~ !

L29100: %   !        !         !     V E N D O R   N A M E    !  CHECK   ~
        ~+------------+------------------------------+----------+---------~
        ~-+
L29130: %   ! NUMBER !  NUMBER !                              !  AMOUNT  ~
        ~!ACCOUNT #   !     ACCOUNT  DESCRIPTION     !DEBIT AMT.!CREDIT AM~
        ~T!
L29160: %   +--------+---------+------------------------------+----------~
        ~+------------+------------------------------+----------+---------~
        ~-+
L29190: %   !########!#########!##############################!##########~
        ~!############!##############################!##########!#########~
        ~#!
L29220: %   !******TOTALS*******                           -##,###,###.##~
        ~(DISCOUNTS) -#######.##                     -#######.##-#######.#~
        ~#!
L29250: %   !------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-!
L30000: REM *************************************************************~
            *     G E T   N E W   C H E C K   H E A D E R   I N F O     *~
            *                                                           *~
            * CHECK HEADER INFORMATION FROM CHECK BUFFER                *~
            *************************************************************

            get #9, using L30100,                                         ~
                    checkdate$, discount, discacct$, cashacct$, netcheck
            discount = round(discount,2%)
            netcheck = round(netcheck,2%)
            gain_loss = 0:gain_loss$ = " "
            call "READ100" (#42, key(#9,2), f1%(42))
               if f1%(42) = 0% then return
            get #42 using L30077, currency$, convunt, gain_loss, gain_loss$
L30077:         FMT CH(4), POS(36), PD(14,7), POS(60), PD(14,4), CH(9)
            gain_loss = round(gain_loss, 2)
            return

L30100:     FMT XX(14),                  /* SKIP FORWARD & REVERSE KEYS*/~
                XX(9),                   /* VENDOR CODE                */~
                XX(8),                   /* CHECK NUMBER               */~
                CH(6),                   /* CHECK DATE (YYMMDD)        */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(9),                   /* DISCOUNT ACCOUNT           */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                PD(14,4)                 /* NET CHECK AMOUNT           */~

L31000: REM *************************************************************~
            * L O A D   C H E C K   D E T A I L   F R O M   B U F F E R *~
            *                                                           *~
            * LOADS A SINGLE CHECK DETAIL FROM THE BUFFER.              *~
            *************************************************************

            get #10, using L31090, invoice$, acct$, accttype$, amount,    ~
                                  thisdisc
            amount = round(amount,2%)
            thisdisc = round(thisdisc,2%)
            line_gl = 0
            call "READ100" (#43, key(#10), f1%(43))
               if f1%(43) = 0% then return
            get #43 using L31072, line_gl
L31072:         FMT POS(85), PD(14,4)
            return

L31090:     FMT XX(20),                  /* SKIP KEY                   */~
                CH(16),                  /* INVOICE NUMBER PAYING OFF  */~
                CH(9),                   /* ACCOUNT NUMBER TO CREDIT   */~
                CH(1),                   /* ACCOUNT TYPE OF CREDIT ACCT*/~
                PD(14,4),                /* AMOUNT PAID                */~
                PD(14,4)                 /* DISCOUNT TAKEN             */~

L32000: REM *************************************************************~
            *         L O A D   O L D   C H E C K   H E A D E R         *~
            *                                                           *~
            * LOADS THE OLD CHECK HEADER FROM CHECK MASTER FILE.  NOTE  *~
            * THAT SINCE THE PROCESSING FUNCTIONS ARE SEGREGATED, WE    *~
            * CAN LOAD THE CHECK INTO THE SAME VARIABLES AS THE NEW ONE *~
            *************************************************************

            get #7, using L32120,                                         ~
                    checkdate$, discount, discacct$, cashacct$, netcheck
            netcheck = round(netcheck,2%)
            discount = round(discount,2%)
            gain_loss = 0:gain_loss$ = " "
            call "READ100" (#45, key(#7), f1%(45))
               if f1%(45) = 0% then return
            get #45 using L32097, currency$, convunt, gain_loss, gain_loss$
L32097:         FMT CH(4), POS(36), PD(14,7), POS(60), PD(14,4), CH(9)
            return

L32120:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(8),                   /* CHECK NUMBER               */~
                CH(6),                   /* CHECK DATE (YYMMDD)        */~
                PD(14,4),                /* DISCOUNT TAKEN AMOUNT      */~
                CH(9),                   /* DISCOUNT ACCOUNT           */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                XX(6),                   /* DATE POSTED                */~
                XX(6),                   /* DATE ORIGINALLY INPUT      */~
                XX(3),                   /* ORIGINALLY INPUT BY (USER) */~
                XX(6),                   /* DATE LAST MODIFIED         */~
                XX(3),                   /* LAST MODIFIED BY           */~
                PD(14,4)                 /* CHECK AMOUNT               */~

L33000: REM *************************************************************~
            *    L O A D   D E T A I L   F R O M   M A I N   F I L E    *~
            *                                                           *~
            * LOADS A SINGLE CHECK DETAIL INTO CORE FROM THE CASH       *~
            * DISBURSEMENTS CHECK DETAIL FILE.                          *~
            *************************************************************

            get #8,using L33100,invoice$,acct$,accttype$,amount,thisdisc
            amount = round(amount,2%)
            thisdisc = round(thisdisc,2%)
            line_gl = 0
            call "READ100" (#46, key(#8), f1%(46))
               if f1%(46) = 0% then return
            get #46 using L33077, line_gl
L33077:         FMT POS(85), PD(14,4)
            return

L33100:     FMT XX(20),                  /* SKIP KEY                   */~
                CH(16),                  /* INVOICE NUMBER PAYING OFF  */~
                CH(9),                   /* ACCOUNT NUMBER TO CREDIT   */~
                CH(1),                   /* ACCOUNT TYPE OF CREDIT ACCT*/~
                PD(14,4),                /* AMOUNT PAID                */~
                XX(6),                   /* POSTED DATE                */~
                PD(14,4)                 /* DICOUNT                    */

L40000: REM *************************************************************~
            *        P R I N T   D A I L Y   R E C A P   I N F O        *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  SHOWS OFF JUST FOR FUN.  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

            REM PRINT BATCH TOTAL
                batchtotal = round(batchtotal, 2%)
                disctotal  = round(disctotal  ,2%)
                grcredittotal = round(grcredittotal,2%)
                grdebittotal  = round(grdebittotal ,2%)
                print using L29220, batchtotal,disctotal,grcredittotal,   ~
                                   grcredittotal
                print using L29250

            if debitsptr% = 0 and creditsptr% = 0 then L65000

            totaldebits, totalcredits, colsdone% = 0
            mat rcpline% = con
            mat rcpptr% = zer
            gosub L48000        /* SKIP TO TOP OF PAGE.                 */

L40200:     for column% = 1 to 2
                on column% gosub L40270, L40610
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then L65000          /* DONE W/ REPORT   */
                goto L40200

L40270:     REM HANDLES LEFT MOST COLUMN FOR THE REPORT
                on rcpline%(1) gosub L40310, L40440, L40480, L40520, L40560
                return

L40310:         REM PRINT DEBITS STACK, IF ANY...
                    if debitsptr% = 0 then L40480
                    rcpptr%(1)  = rcpptr%(1) + 1
                    rcpamt(1)   = debitsstk(rcpptr%(1))
                    rcpacct$(1) = debitsstk$(rcpptr%(1))
                    call "DESCRIBE" (#2, rcpacct$(1), rcpacctdescr$(1),  ~
                                         0%, f1%(2))
                    call "GLFMT" (rcpacct$(1))
                    print using L49020,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits = totaldebits + debitsstk(rcpptr%(1))
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
L40440:         REM PRINTS SEPARATOR LINE.
                    print using L49010, "*--";
                    rcpline%(1) = 3
                    return
L40480:         REM PRINTS TOTAL LINE
                    totaldebits = round(totaldebits, 2%)
                    print using L49030, totaldebits;
                    rcpline%(1) = 4
                    return
L40520:         REM PRINTS STARS
                    print using L49000, "*";
                    rcpline%(1) = 5
                    return
L40560:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L40610:     REM HANDLES RIGHT HAND COLUMN--SALES,FRT, TAX, TOTAL CREDITS
                print tab(70);
                on rcpline%(2) gosub L40650, L40780, L40820, L40860, L40900
                   return
L40650:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L40820
                    rcpptr%(2)  = rcpptr%(2) + 1
                    rcpamt(2)   = creditsstk(rcpptr%(2))
                    rcpacct$(2) = creditsstk$(rcpptr%(2))
                    call "DESCRIBE" (#2, rcpacct$(2), rcpacctdescr$(2),  ~
                                         0%, f1%(2))
                    call "GLFMT" (rcpacct$(2))
                    print using L49020, rcpacct$(2), rcpacctdescr$(2),    ~
                               rcpamt(2);
                    totalcredits = totalcredits + creditsstk(rcpptr%(2))
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L40780:         REM PRINT SEPARATOR LINE
                    print using L49010, "*--";
                    rcpline%(2) = 3
                    return
L40820:         REM PRINT TOTAL CREDITS LINE
                    totalcredits = round(totalcredits, 2%)
                    print using L49031, totalcredits;
                    rcpline%(2) = 4
                    return
L40860:         REM PRINT STARS
                    print using L49000,"*";
                    rcpline%(2) = 5
                    return
L40900:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L48000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                select printer (134)
                   call "DATE" addr ("HD", hdrdate$)
                   print page
                   if postme$ = "YES" then                               ~
                   print using L49050, 1%, title1$, pstseq%, postdate$,   ~
                          date$                                          ~
                                      else                               ~
                   print using L49082, 1%, title1$, pstseq%, postdate$,   ~
                          date$
                   print
                   print using L49090
                   print
                   print using L49130
                   print using L49170,"#","#"
                   print using L49210
                   return

L49000: %**************************#***********************************
L49010: %#--------------+--------------------------------+------------*
L49020: %* ############ ! ############################## !-#######.## *
L49030: %*              ! TOTAL DEBITS                   !-#######.## *
L49031: %*              ! TOTAL CREDITS                  !-#######.## *

L49050: %PAGE #####   ###################################################~
        ~####               SEQ: ###  POST DATE: ########  RUN DATE: #####~
        ~###

L49082: %PAGE #####   ###################################################~
        ~#### (NOT POSTED)  SEQ: ###  POST DATE: ########  RUN DATE: #####~
        ~###

L49090: %=========================D E B I T S==========================  ~
        ~     ===========================C R E D I T S====================~
        ~==

L49130: %**************************************************************  ~
        ~     ************************************************************~
        ~**

L49170: %* ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT   *  ~
        ~     * ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT  ~
        ~ *

L49210: %*--------------+--------------------------------+------------*  ~
        ~     *--------------+--------------------------------+-----------~
        ~-*

L50000: REM *************************************************************~
            *     S T A C K   P R O C E S S I N G   R O U T I N E S     *~
            *                                                           *~
            * STACK PROCESSING ROUTINES PUSH STUFF ONTO THE STACK FOR   *~
            * CREDITS THIS INVOICE, FOR BOTH DEBITS AND CREDITS THIS    *~
            * BATCH.                                                    *~
            *************************************************************

            REM FIRST, ZAP OLD STACK CONTENTS.
                pointer% = 0
                init(hex(ff)) stack$(), stackinv$()
                mat stack = zer
                return

            deffn'160(account$, stack)
                  if summary$ <> "Y" then L50210
                  if stack = 0 then return
                  search stack$() = str(account$) to location$ step 9
                  if str(location$,1,2) = hex(0000) then L50210
                     location% = val(location$,2)/9+1
                     stack(location%) = stack(location%) + stack
                     stack(location%) = round(stack(location%), 2%)
                     return
L50210:           REM ELSE PUSH NEW ENTRY IF OLD NOT ON STACK...
                      pointer% = pointer% + 1
                      stack$(pointer%) = account$
                      stack (pointer%) = stack
                      stackinv$(pointer%) = invoice$
                      return

            deffn'161(account$, dbitst)
                  if dbitst = 0 then return
                  search debitsstk$() = str(account$) to location$ step 9
                  if str(location$,1,2) = hex(0000) then L50340
                     location%=val(location$,2)/9+1
                     debitsstk(location%) = debitsstk(location%)+dbitst
                     debitsstk(location%) = round(debitsstk(location%),2%)
                     return
L50340:           REM ELSE PUSH NEW ENTRY IF OLD NOT ON STACK...
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitsstk (debitsptr%) = dbitst
                      return

            deffn'162(account$, crdtst)
                  if crdtst = 0 then return
                  search creditsstk$() = str(account$) to location$ step 9
                  if str(location$,1,2) = hex(0000) then L50470
                     location% = val(location$,2)/9+1
                     creditsstk(location%)=creditsstk(location%) + crdtst
                     creditsstk(location%)=round(creditsstk(location%),2%)
                     return
L50470:           REM ELSE PUSH NEW ENTRY IF OLD NOT ON STACK...
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditsstk (creditsptr%) = crdtst
                      return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            call "JNLCLOSE" ("02", jnlid$, pstseq%, returncode%)
            call "SETPRNT" ("A/P007", " ", 0%, 1%)
            end
