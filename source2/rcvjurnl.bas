        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  RRRR    CCC   V   V  JJJJJ  U   U  RRRR   N   N  L       *~
            *  R   R  C   C  V   V    J    U   U  R   R  NN  N  L       *~
            *  RRRR   C      V   V    J    U   U  RRRR   N N N  L       *~
            *  R   R  C   C   V V   J J    U   U  R   R  N  NN  L       *~
            *  R   R   CCC     V     J      UUU   R   R  N   N  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVJURNL - PRINTS LOG OF GL TRANSACTIONS FOR RCVINPUT.    *~
            *            POSTING IS OPTIONAL, CONTROLLED BY GETPARM.    *~
            *            POSTING CAN BE 'YES' OR 'NO'.  SUPPORTED       *~
            *            POSTING OPTIONS WILL BE 'F' FOR FULL DETAIL,   *~
            *            'D' FOR DOCUMENT, OR 'Y' FOR SUMMARY.          *~
            *-----------------------------------------------------------*~
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
            * 07/01/86 ! ORIGINAL                                 ! KAB *~
            * 08/29/90 ! G/L Export file modifications.           ! RAC *~
            * 04/19/91 ! PRR 11119  Added "***Suspense Account***"! SID *~
            *          !  to the report when there's an offset    !     *~
            *          !  between Actual and Standard cost.       !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            aid$1,                                                       ~
            col1$32,                     /* COLUMN ONE DISPLAY VAR     */~
            company$70,                  /* COMPANY FOR HEADERS        */~
            creditsstk$(900)12,          /* CREDIT ACCOUNTS FOR RCP#1  */~
            creditsstk(900),             /* CREDIT AMOUNTS FOR RCP#1   */~
            date$8,                      /* PRINT FORMATTED DATE/TIME  */~
            time$8,                      /* PRINT FORMATTED DATE/TIME  */~
            debitsstk$(900)12,           /* DEBIT ACCOUNTS FOR RCP#1   */~
            debitsstk(900),              /* DEBIT AMOUNTS FOR RCP#1    */~
            jnlid$3,                     /* JOURNAL ID                 */~
            moduleno$20,                 /* MODULE NUMBER              */~
            po_credit$(9000)9,           /* CREDIT ACCOUNTS            */~
            po_credit(9000),             /* CREDIT AMOUNTS             */~
            po_debits$(9000)9,           /* DEBIT ACCOUNTS             */~
            po_debits(9000),             /* DEBIT AMOUNTS              */~
            location$2,                  /* LOCATOR ARRAY (STACK SUBS) */~
            next_pogl_key$32,            /* NEXT POGLBUFF KEY          */~
            old_ven_po$41,               /* OLD VENDOR & PO CONCAT     */~
            payablesdate$6,              /* PAYABLES DATE THIS USER.   */~
            payjurnline%(3),             /* LINE POINTERS--JURN1 PHASE */~
            postdate$6,                  /* Posting Date of Transaction*/~
            postme$3,                    /* FLAG FOR FILE ONLY STUFF   */~
            prtacct_d$16,                /* ACCOUNT NUMBERS TO PRINT   */~
            prtacct_c$16,                /* CREDIT PRINT ACCOUNT       */~
            prtacctdesc_c$32,            /* CREDIT ACCT DESCRIPTION    */~
            prtacctdesc_d$32,            /* DEBIT    "      "          */~
            prtcredit_amt$,              /* CREDIT PRINT AMOUNT        */~
            prtdebit_amt$,               /* DEBIT    "     "           */~
            rcpacct$(2)16,               /* ACCOUNTS FOR RCP#1 PHASE   */~
            rcpacctdescr$(2)30,          /* ACCOUNT DESCRIPTION--RCP#1 */~
            rcpamt(2),                   /* AMOUNTS FOR RCP#1 PHASE    */~
            rcpline%(2),                 /* LINE COUNTERS FOR RCP#1    */~
            rcpptr%(2),                  /* POINTER FOR RCP#1          */~
            rcvr$16,                     /* RECEIVER                   */~
            summary$1,                   /* SUMMARY INDICATOR          */~
            text$100,                    /* TEXT PASSED TO "G/LPOST"   */~
            savetext$100,                /*  TO HAVE AND TO HOLD       */~
            tttle$40,                    /* JOURNAL TITLE              */~
            jrntttle$70,                 /* JOURNAL TITLE              */~
            type$2,                      /* TRANSACTION TYPE           */~
            userid$3,                    /* USERID THIS USER           */~
            vencode$9,                   /* VENDOR CODE                */~
            ven_po$41                    /* VENDOR & PO CONCAT         */

        dim gl_post_info$(2)255          /* G/L Export Posting Info    */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "OPENCHCK"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************
            mat f2% = con

                     /* REMEMBER, ONLY YOU CAN HELP PREVENT APOPLEXY.  */
                     /* THE BEST WAY TO PREVENT IT IS TO *NOT* MODIFY  */
                     /* THE VARIABLE  F2%()  IN THE PROGRAM.           */
                     /* I PROMISE I WILL GO INTO A SCREAMING FIT IF YOU*/
                     /* EVEN HINT AT IT.                               */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 4 ! SYSFILE2 ! SYSTEM DEFAULT INFO FILE (OPEN MONTHS)   *~
            * # 5 ! POGLBUFF ! PO TO GL BUFFER FILE                     *~
            * #12 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select # 2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1,                                      ~
                        keylen = 9


            select #3,  "VENDOR"                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                       alt key 1, keypos = 10, keylen = 30, dup

            select  #4, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #5, "RCVJRNTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 670,                                   ~
                        keypos = 10, keylen = 10

            select #12, "GLDETAIL",      /* GENERAL LEDGER DETAILS     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            call "SHOSTAT" ("Opening Files; One Moment Please.")

            call "OPENCHCK" (# 1, 0%, f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, 0%, f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, 0%, f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, 0%, f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, 0%, f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (#12, 0%, f2%(12), 0%, rslt$(12))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES RECAP STACKS, SETS USER ID, OTHER STUFF.      *~
            *************************************************************

            init(hex(ff)) debitsstk$(), creditsstk$(), po_debits$(),     ~
                          po_credit$()
            mat debitsstk = zer
            mat creditsstk = zer
            debitsptr%, creditsptr% = 0

            call "EXTRACT" addr ("ID", userid$) /* GET CURRENT USER    */
            init (hex(00)) next_pogl_key$
            str(next_pogl_key$,1,3) = str(userid$,1,3)

            REM GET PAYABLES DATE AND FIND OUT WHICH MONTH IT'S IN.
                call "READ100" (#1, userid$, f1%(1))
                     if f1%(1) = 0 then end
                     get #1, using L09220 , payablesdate$
L09220:                            FMT XX(9), CH(6)
                call "WHICHMON" (#4, payablesdate$, whichmonth%)
                     if whichmonth% = 0 then L65000           /* ERR    */

            payjurnline%, rcpline% = 1000

            REM DO A GETPARM TO FIND IF THIS BATCH GETS POSTED TO G/L.
                call "GETPARM" addr ("I ", "R", "POSTME  ", aid$, "0001",~
                                     "PYJRN1",                           ~
                                     "Post This Buffer To G/L?        ", ~
                                     32%, "K", "POSTME  ", postme$, 3%,  ~
                                     5%, 32%, "A")

                if postme$ <> "NO" then postme$ = "YES"

L09350:      REM DO A GETPARM TO FIND JNLID$

                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "RCVJN1",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09350

                returncode% = 0%
                moduleno$ = "05"
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                      tttle$, payablesdate$, #4, f2%(4), returncode%)

            if postme$ <> "YES" then summary$ = "F"
            if pos("FDY" = summary$) = 0% then summary$ = "F"
            if jnlid$ = "PRC" then L09470
               if pos("FY" = summary$) = 0% then summary$ = "F"

L09470:     jrntttle$ = tttle$ & " JOURNAL"
            temp$ = " ":if postme$ <> "YES" then temp$ = "(NOT POSTED)"
            call "FMTTITLE" (jrntttle$, temp$, 12%)
            old_ven_po$=" ":rcvdebit, rcvcredit = 0

            if postme$ <> "YES" then                                     ~
            call "SHOSTAT" ("Printing Supplementary Receipts Journal")   ~
                               else                                      ~
            call"SHOSTAT"("Printing Receipts Journal and Posting to G/L")

            at_least_one%=0%

            date$ = date:call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, f1%(5))
            time$ = " ":call "TIME" (time$)

            close ws
                select printer (134)
                call "SETPRNT" ("RCV004", " ", 0%, 0%)

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *************************************************************

L10040:    call "PLOWNEXT" (#5, next_pogl_key$,3%,f1%(5))
                     if f1%(5)<>0 then L10110
                              REM ELSE
                                  if at_least_one% = 0% then L10080
                                     gosub L20000
                                     gosub L21000
                                     gosub L22000
L10080:                              call "DELETE" (#5,userid$,3%)
                                        go to L40000

L10110:    at_least_one%=1

                get #5, using L10270, type$, postdate$, acct$, text$,     ~
                        debit, credit, gl_post_info$()
                if str(type$,1,1) <> "W" then L10240
                   vencode$ = str(text$,31,9)
                   ponumber$ = str(text$,40,16)
                     goto L10261
L10240:            vencode$ = str(text$,1,9)
                   ponumber$ = str(text$,10,16)

L10261:         if type$ <> "HD" then temp$ = str(text$,80,16)
                if type$ =  "HD" then temp$ = str(text$,80,4) &          ~
                                                        str(text$,90,11)
                rcvr$ = str(text$,26,5) & str(text$,69,11)
                init (" ") str(text$,26,5), str(text$,69,32)
                str(text$,69,32) = str(rcvr$,,16) & temp$

L10270:         FMT CH(2),               /* Posting Type               */~
                                         /*   CP = P/PAY CREDIT        */~
                                         /*   DR = RECVR HOLD          */~
                                         /*   DQ = QC HOLD             */~
                                         /*   DP = P/PAY DEBIT (RTV)   */~
                                         /*   DS = SCRAP STORE/LOT     */~
                                         /*   DH = ON-HAND INVENTORY   */~
                                         /*   HD = HNYPST2 DIRECT ADJ  */~
                                         /*   HA = APOST ASSET ACCOUNT */~
                                         /*   HV = APOST VARIANCE ACCT */~
                                         /*   WC = WIP CREDIT          */~
                                         /*   WD = WIP DEBIT           */~
                    XX(3),               /* Journal Id (blank)         */~
                    XX(4),               /* Posting sequence (0%)      */~
                    XX(3),               /* User Id.                   */~
                    XX(7),               /* Date/Time Stamp            */~
                    CH(6),               /* Posting Date               */~
                    CH(9),               /* GL Account                 */~
                    CH(100),             /* GL Text  ****              */~
                                         /*     1/ 9 = Vendor Code     */~
                                         /*    10/16 = PO Number       */~
                                         /*    26/ 5 = Receiver,1,5    */~
                                         /*    31/25 = Part Code       */~
                                         /*    56/ 3 = Store           */~
                                         /*    59/ 6 = Lot             */~
                                         /*    65/ 4 = Adjustment Code */~
                                         /*    69/11 = Receiver,6,11   */~
                                         /*    80/21 = Text            */~
                    PD(14,4),            /* Debit                      */~
                    PD(14,4),            /* Credit                     */~
                    XX(6),               /* System Date                */~
                    XX(4),               /* Filler                     */~
                    2*CH(255)            /* G/L Export Posting Info    */~

                     ven_po$ = str(text$,69,16) & str(vencode$,1,9) &    ~
                                                               ponumber$
                     debit  = round(debit,  2)
                     credit = round(credit, 2)

                     if old_ven_po$ = " " then L10660
                     if old_ven_po$ <> ven_po$ then gosub L20000
                     if str(old_ven_po$,,16) <> str(ven_po$,,16)         ~
                                                     then gosub L21000

L10660:              old_ven_po$=ven_po$
                     rcvdebit  = rcvdebit  + debit
                     rcvcredit = rcvcredit + credit

                     gosub'160(acct$, credit)
                     gosub'161(acct$, debit)
                     gosub'162(acct$, debit)
                     gosub'163(acct$, credit)

                     if postme$ <> "YES" then L10040
                     if summary$ <> "F"  then L10040
                        gosub post_to_gl
                        goto L10040

        REM *************************************************************~
            * POST SUMMARY ONLY TO GL                                   *~
            *************************************************************

        post_s_to_gl

            if summary$ <> "Y" then return
            if debitsptr% <> 0% then L17150
            if creditsptr% = 0% then return

L17150:     text$ = " "
            str(text$,69,32) = "RECEIVING SUMMARY RECAP"
            if jnlid$ = "PRC" then L17190
            str(text$,69,32) = "DISTRIBUTION SUMMARY RECAP"

L17190:     for p% = 1% to max(creditsptr%, debitsptr%)

                if p% > creditsptr% then L17270
                   acct$  = str(creditsstk$(p%),1,9)
                   debit  = 0
                   credit = creditsstk(p%)
                      gosub post_to_gl

L17270:         if p% > debitsptr% then L17330
                   acct$  = str(debitsstk$(p%),1,9)
                   credit = 0
                   debit  = debitsstk(p%)
                      gosub post_to_gl

L17330:     next p%

            return

        REM *************************************************************~
            * POST DOCUMENT LEVEL SUMMARY TO GL                         *~
            *************************************************************

        post_d_to_gl

            if summary$ <> "D" then return
            if po_debitsptr% <> 0% then L18080
            if po_creditptr%  = 0% then return

L18080:     saveacct$  = acct$
            savedebit  = debit
            savecredit = credit
            savetext$  = text$

            text$ = " "
            str(text$,1,25) = str(old_ven_po$,17,25)
            str(text$,69,25) = str(old_ven_po$,1,16) & " P.O. SUMMARY"

            for p% = 1% to max(po_creditptr%, po_debitsptr%)

                if p% > po_creditptr% then L18210
                   acct$  = po_credit$(p%)
                   debit  = 0
                   credit = po_credit(p%)
                      gosub post_to_gl

L18210:         if p% > po_debitsptr% then L18270
                   acct$  = po_debits$(p%)
                   credit = 0
                   debit  = po_debits(p%)
                      gosub post_to_gl

L18270:     next p%

            acct$  = saveacct$
            debit  = savedebit
            credit = savecredit
            text$  = savetext$

            return

        REM *************************************************************~
            * COMMON ROUTINE FOR POSTING TO G/L                         *~
            *************************************************************

        post_to_gl

                     call "GLPOST2" (acct$, debit, credit, postdate$,    ~
                                    0%, "05", text$, jnlid$, pstseq%,    ~
                                    userid$, #2, #12, #4, returncode%,   ~
                                    rcvr$, gl_post_info$())
             return

L20000: REM *************************************************************~
            * PRINT THE P.O. AND ZERO THE ARRAYS                        *~
            *************************************************************

             gosub post_d_to_gl
             if jnlid$ = "PRC" then gosub L27000

             mat po_debits = zer
             mat po_credit = zer
             init (hex(ff)) po_debits$(),po_credit$()
             prtdebits_total,prtcredit_total = 0
             po_debitsptr%,po_creditptr% = 0

            return

L21000: REM *************************************************************~
            * PRINT RECEIVER TOTALS, JUST BECAUSE                       *~
            *************************************************************

            if jnlid$ <> "PRC" then L21080

            print using L29900, str(old_ven_po$,1,16), rcvdebit, rcvcredit
            print using L29580
            payjurnline% = payjurnline% + 2

L21080:     rcvdebit, rcvcredit = 0
            return

L22000: REM *************************************************************~
            * PRINT 'GRAND' (OR AT LEAST BETTER THAN SO-SO) TOTALS      *~
            *************************************************************

            REM PRINT BATCH TOTALS

            if jnlid$ <> "PRC" then return

                payjurnline% = payjurnline% - 4%
                gosub L29000
                print using L29820, batchtotal1, batchtotal2
                print using L29860

                return

L27000: REM *************************************************************~
            *     P R I N T   PAYABLES    J O U R N A L   E N T R Y     *~
            *                                                           *~
            * PRINTS THE PAYABLES JOURNAL USING AMOUNTS IN THE DEBITS   *~
            * STACK.  ALSO PRINTS VENDOR INFO, PAYABLES ACCOUNT.        *~
            *************************************************************

            gosub L27370:gosub L27645:gosub L28000 /* SNEAKY INIT */

            colsdone% = 0
            prtdebits_ptr% = 0           /* TEMPORARY POINTER INTO STAK*/
            prtcreditsptr% = 0           /* TEMPORARY POINTER INTO STAK*/
            mat payjurnline% = con

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L27130:         for column% = 1 to 3
                    on column% gosub L27270, L27490, L27720
                    next column%

                gosub L29000              /* PAGE HEADING, IF NECCESSARY*/
                print using L29780, col1$,prtacct_d$,prtacctdesc_d$,      ~
                                   prtdebit_amt$,prtacct_c$,             ~
                                   prtacctdesc_c$,prtcredit_amt$

                if colsdone% < 3 then L27130        /* IF NOT YET DONE  */
                gosub L29000
                gosub L27585 : gosub L27880          /* GET TOTALS       */
                print using L29780, col1$,prtacct_d$,prtacctdesc_d$,      ~
                                   prtdebit_amt$,prtacct_c$,             ~
                                   prtacctdesc_c$,prtcredit_amt$

                   payjurnline% = payjurnline% + 1
                   print using L29740               /* SEPARATOR LINE   */
                   return                          /* NEXT PART        */

L27270: REM HANDLES FIRST COLUMN (COL1$)
            on payjurnline%(1) gosub L27300,L27320,L27339,L27350,L27370
                           REM       P.O. ,VENCD,VNAME,DATE ,BLANK
            return
L27300:             col1$=str(old_ven_po$,26,16)
                    payjurnline%(1)=2
                       return
L27320:             col1$=str(old_ven_po$,17,9)
                    payjurnline%(1)=3
                       return
L27339:             init (" ") col1$
                    call "DESCRIBE" (#3,str(old_ven_po$,17,9),col1$,0%,  ~
                                                f1%(3))
                    payjurnline%(1)=4
                       return
L27350:             col1$ = str(old_ven_po$,,16)
                    payjurnline%(1)=5
                    colsdone%=colsdone%+1
                       return
L27370:             init (" ") col1$
                       return
L27490: REM HANDLES SECOND COLUMN (DEBITS)
            on payjurnline%(2) gosub L27520, L27645, L27585
                           REM      A LINE, BLANK, TOTAL
                               REM TOTAL LINE NOT EVER CALLED FROM HERE
                return


L27520:     if po_debitsptr% = 0 then L27571             /* SKIP IF NONE*/
               prtdebits_ptr% = prtdebits_ptr% + 1      /* NEXT ELEMENT*/
               call"NUMPRINT"(po_debits(prtdebits_ptr%),.5, prtdebit_amt$)
               prtacct_d$ = po_debits$(prtdebits_ptr%)
               call"DESCRIBE"(#2,prtacct_d$,prtacctdesc_d$,0%,f1%(2))
               if prtacctdesc_d$ = " " then                              ~
                             prtacctdesc_d$ = "*** Suspense Account ***"
               call "GLFMT" (prtacct_d$)

                    prtdebits_total=round(prtdebits_total +              ~
                                         po_debits(prtdebits_ptr%), 2)
               if prtdebits_ptr% < po_debitsptr% then return
L27571:           colsdone%=colsdone%+1
                  payjurnline%(2) = 2            /* BLANKS FROM NOW ON*/
                    return

L27585:      REM THIRD CASE--PRINT TOTAL LINE

                call "NUMPRINT"(prtdebits_total,.5,prtdebit_amt$)
                prtacct_d$="**TOTAL**"
                prtacctdesc_d$="TOTAL DEBITS"
                batchtotal1 = round(batchtotal1 + prtdebits_total, 2)
                   return

L27645:      REM SECOND CASE--SET THE LINE TO BLANKS, INCREMENT #COLS
                prtacctdesc_d$, prtdebit_amt$,prtacct_d$ = " "
                   return

L27720:     REM **** HANDLES STUFF FOR THIRD COLUMN (CREDITS)
               on payjurnline%(3) gosub L27760, L28000, L27880
                              REM      A LINE, BLANK, TOTAL
                               REM TOTALS LINE NOT EVER CALLED FROM HERE
                  return

L27760:     REM FIRST CASE--PRINT SALES STACK, ACCUMULATE TOTALS
            if po_creditptr% = 0 then L27860    /* SKIP IF NONE*/
               prtcreditsptr% = prtcreditsptr% + 1       /* NEXT ELEMENT*/
               call"NUMPRINT"(po_credit(prtcreditsptr%),.5,prtcredit_amt$)
               prtacct_c$=po_credit$(prtcreditsptr%)
               call"DESCRIBE"(#2,prtacct_c$,prtacctdesc_c$,0%,f1%(2))
               if prtacctdesc_c$ = " " then                              ~
                             prtacctdesc_c$ = "*** Suspense Account ***"
               call "GLFMT" (prtacct_c$)
                  prtcredit_total=round(prtcredit_total +                ~
                                         po_credit(prtcreditsptr%), 2)
                if prtcreditsptr% < po_creditptr% then return
L27860:            payjurnline%(3) = 2
                   colsdone%=colsdone%+1
                      return

L27880:         REM THIRD CASE--PRINT TOTAL LINE FOR THIS INVOICE.
                    col1$ = " "
                    call "NUMPRINT"(prtcredit_total,.5,prtcredit_amt$)
                    prtacct_c$="**TOTAL**"
                    prtacctdesc_c$="TOTAL CREDITS"
                    batchtotal2 = round(batchtotal2 + prtcredit_total,2)
                    return

L28000:         REM SECOND CASE--SET THE LINE TO BLANKS, INCREMENT #COLS
                    prtacctdesc_c$,prtcredit_amt$,prtacct_c$ = " "
                    return

L29000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING SALES JOURNAL.

                payjurnline% = payjurnline% + 1
                if payjurnline% < 57 then return
                   if payjurnpage% <> 0% then print using L29740
                   print page
                   payjurnpage% = payjurnpage% + 1
                   print using L29500,date$, time$, company$
                   print using L29540,jnlid$,pstseq%,jrntttle$,payjurnpage%
                   print
                   print using L29580
                   print using L29620
                   print using L29740
                   print using L29700, "#" , "#"
                   print using L29740
                   payjurnline% = 9      /* SET STARTING LINE ON PAGE  */
                   return

L29500: %######## ########              #################################~
        ~#####################################                RCVJURNL:RCV~
        ~004

L29540: %###:-#######                   #################################~
        ~#####################################                      PAGE #~
        ~###

L29580: %+--------------------+------------------------------------------~
        ~------------+----------------------------------------------------~
        ~--+

L29620: %!  P.O. INFORMATION  !                    D E B I T S           ~
        ~            !                   C R E D I T S                    ~
        ~  !


L29700: %!                    ! ACCOUNT #      !   ACCOUNT DESCRIPTION   ~
        ~ !    AMOUNT! ACCOUNT #      !   ACCOUNT DESCRIPTION    !    AMOU~
        ~NT!

L29740: %+--------------------+----------------+-------------------------~
        ~-+----------+----------------+--------------------------+--------~
        ~--+

L29780: %!####################!################!#########################~
        ~#!##########!################!##########################!########~
        ~##!

L29820: %! ***** TOTALS ***** !                                       -##~
        ~#,###,###.##        <=  TOTALS SHOULD AGREE  =>     -###,###,###.~
        ~##!

L29860: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L29900: %! ################   ! RECEIVER TOTALS                       -##~
        ~#,###,###.##!                                       -###,###,###.~
        ~##!

L40000: REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.                           WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

           gosub post_s_to_gl

            REM SORT ARRAYS
                call "SORT" addr(str(debitsstk$(), 1), 250%, 12%)
                call "SORT" addr(str(creditsstk$(), 1), 250%, 12%)

            jrntttle$ = tttle$ & " RECAP"
            temp$ = " ":if postme$ <> "YES" then temp$ = "(NOT POSTED)"
            call "FMTTITLE" (jrntttle$, temp$, 12%)
            totaldebits, totalcredits, colsdone% = 0
            mat rcpline% = con
            mat rcpptr% = zer
            gosub L48000        /* SKIP TO TOP OF PAGE.                 */

L40190:     for column% = 1 to 2
                on column% gosub L41000, L42000
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then L65000          /* DONE W/ REPORT   */
                goto L40190

L41000:     REM HANDLES LEFT MOST COLUMN FOR THE REPORT
                on rcpline%(1) gosub L41100, L41200, L41300, L41400, L41500
                return

L41100:         REM PRINT PAYABLES STACK, IF ANY...
                    if debitsptr% = 0 then L41300
                    rcpptr%(1) = rcpptr%(1) + 1
                    convert str(debitsstk$(rcpptr%(1)),10,3) to t%
                    rcpamt(1)=debitsstk(t%)
                    rcpacct$(1)=str(debitsstk$(rcpptr%(1)),1,9)
                    call "DESCRIBE" (#2, rcpacct$(1), rcpacctdescr$(1),  ~
                                              0%, f1%(2))
                    call "GLFMT" (rcpacct$(1))
                    if rcpacctdescr$(1) = " " then                       ~
                           rcpacctdescr$(1) = "*** Suspense Account ***"
                    print using L49050,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits=round(totaldebits+debitsstk(t%),2)
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
                           FMT XX(9), CH(30)
L41200:         REM PRINTS SEPARATOR LINE.
                    print using L49025, "*--";
                    rcpline%(1) = 3
                    return
L41300:         REM PRINTS TOTAL LINE
                    print using L49060, totaldebits;
                    rcpline%(1) = 4
                    return
L41400:         REM PRINTS STARS
                    print using L49020, "*";
                    rcpline%(1) = 5
                    return
L41500:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L42000:     REM HANDLES RIGHT HAND COLUMN--SALES,FRT, TAX, TOTAL CREDITS
                print tab(68);
                on rcpline%(2) gosub L42100, L42200, L42300, L42400, L42500
                   return
L42100:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L42300
                    rcpptr%(2) = rcpptr%(2) + 1
                    convert str(creditsstk$(rcpptr%(2)),10,3) to t%
                    rcpamt(2)=creditsstk(t%)
                    rcpacct$(2)=str(creditsstk$(rcpptr%(2)),1,9)
                    call "DESCRIBE" (#2, rcpacct$(2), rcpacctdescr$(2),  ~
                                              0%, f1%(2))
                    call "GLFMT" (rcpacct$(2))
                    if rcpacctdescr$(2) = " " then                       ~
                           rcpacctdescr$(2) = "*** Suspense Account ***"
                    print using L49050,rcpacct$(2),rcpacctdescr$(2),      ~
                               rcpamt(2);
                    totalcredits=round(totalcredits+creditsstk(t%),2)
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L42200:         REM PRINT SEPARATOR LINE
                    print using L49025, "*--";
                    rcpline%(2) = 3
                    return
L42300:         REM PRINT TOTAL CREDITS LINE
                    print using L49061, totalcredits;
                    rcpline%(2) = 4
                    return
L42400:         REM PRINT STARS
                    print using L49020,"*";
                    rcpline%(2) = 5
                    return
L42500:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L48000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP

                   print page
                   rcppage% = 1
                   print using L49080, date$, time$, company$
                   print using L49112, jnlid$,pstseq%,jrntttle$,rcppage%
                   print
                   print using L49120
                   print
                   print using L49160
                   print using L49200,"#","#"
                   print using L49240
                   rcpline% = 9      /* SET STARTING LINE ON PAGE  */
                   return

L49020: %#****************************************************************
L49025: %#-----------------+--------------------------------+------------*
L49050: %* ################! ############################## !-#######.## *
L49060: %*                 ! TOTAL DEBITS                   !-#######.## *
L49061: %*                 ! TOTAL CREDITS                  !-#######.## *

L49080: %######## ########              #################################~
        ~#####################################                RCVJURNL:RCV~
        ~004

L49112: %###:-#######                   #################################~
        ~#####################################                      PAGE #~
        ~###

L49120: %===========================D E B I T S==========================~
        ~=  ==========================C R E D I T S=======================~
        ~===

L49160: %****************************************************************~
        ~*  **************************************************************~
        ~***

L49200: %* ACCOUNT #       !     D E S C R I P T I O N      !     AMOUNT ~
        ~*  * ACCOUNT #       !     D E S C R I P T I O N      !     AMOUN~
        ~T *

L49240: %*-----------------+--------------------------------+------------~
        ~*  *-----------------+--------------------------------+----------~
        ~--*

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************
            deffn'160(account$, amount)    /* CREDITS THIS PO */
                  if amount = 0 then return
                  if summary$ = "F" then L50068
                  search str(po_credit$(),1) = str(account$,,9)          ~
                               to location$ step 9  /* FIND THIS ACCT  */
                  if location$ = hex(0000) then L50068
                     junk% = int(val(location$,2)/9)+1
                     po_credit(junk%) = round(po_credit(junk%) + amount,2)
                     return
L50068:           REM PUSH NEW ENTRY ONTO TOP OF STACK.
                      po_creditptr% = po_creditptr% + 1  /* INCR TOP */
                      po_credit$(po_creditptr%)=account$
                      po_credit(po_creditptr%)=round(amount,2)
                      return

            deffn'161(account$, amount)  /* DEBITS THIS PO */
                  if amount = 0 then return
                  if summary$ = "F" then L50140
                  search str(po_debits$(),1) = str(account$,,9)          ~
                               to location$ step 9  /* FIND THIS ACCT  */
                  if location$ = hex(0000) then L50140
                     junk% = int(val(location$,2)/9)+1
                     po_debits(junk%) = round(po_debits(junk%) + amount,2)
                     return
L50140:           REM PUSH NEW ENTRY ONTO TOP OF STACK.
                      po_debitsptr% = po_debitsptr% + 1  /* INCR TOP */
                      po_debits$(po_debitsptr%)=account$
                      po_debits(po_debitsptr%)=round(amount,2)
                      return

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  if amount = 0 then return
                  search str(debitsstk$(),1) = str(account$,,9)          ~
                              to location$ step 12 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L50280  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/12)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = round(debitsstk(junk%) + amount,2)
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L50280:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      convert debitsptr%                                 ~
                         to str(debitsstk$(debitsptr%), 10, 3), pic(###)
                      debitsstk(debitsptr%) = round(amount,2)
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  if amount = 0 then return
                  search str(creditsstk$(),1) = str(account$,,9)         ~
                               to location$ step 12
                  if location$ = hex(0000) then L50430  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/12)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = round(creditsstk(junk%)+amount,2)
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L50430:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      convert creditsptr% to                             ~
                         str(creditsstk$(creditsptr%), 10, 3), pic(###)
                      creditsstk(creditsptr%) = round(amount,2)
                      return

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC
        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

            call "JNLCLOSE" (moduleno$, jnlid$, pstseq%, returncode%)
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" (" ", " ", 0%, 1%)
            end
