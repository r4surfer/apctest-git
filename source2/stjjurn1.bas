        REM *************************************************************~
            *                                                           *~
            *   SSS   TTTTT  JJJJJ  JJJJJ  U   U  RRRR   N   N    1     *~
            *  S        T      J      J    U   U  R   R  NN  N   11     *~
            *   SSS     T      J      J    U   U  RRRR   N N N    1     *~
            *      S    T    J J    J J    U   U  R   R  N  NN    1     *~
            *   SSS     T     J      J      UUU   R   R  N   N  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STJJURN1 - Prints Standard Journal Daily Recap and Posts  *~
            *            Amounts to General Ledger.                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 06/04/80 ! ORIGINAL (FROM STJJURN1)                 ! BCW *~
            * 09/11/80 ! CLEANUP AND POST TO G/L                  ! TEM *~
            * 11/18/80 ! CORRECT EXIT IF GLDATE IN CLOSED MONTH   ! TEM *~
            * 08/05/81 ! MONTH END CLOSING                        ! TEM *~
            * 12/13/82 ! ROUND TOTALS ON RECAP                    ! ECR *~
            * 05/15/84 ! CORRECTED REVERSAL G/L DATE FROM HISTORY ! LDJ *~
            *          !  'MONTHS OPEN' TO 'FISCAL DATES' OPEN MON!     *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 06/19/85 ! Corrected STJBUF2 file format            ! LDJ *~
            * 09/24/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 04/21/89 ! Rem's out delete of buffer records at    ! RJM *~
            *          !  month end (55000). That job is done by  !     *~
            *          !  STJFILE2.  This fixes Currency Reversals!     *~
            *          !  that were being left in STJMASTR.       !     *~
            * 10/12/89 ! Fixed program name & unwanted combining  ! JDH *~
            *          !  of similar accounts on recap.           !     *~
            * 11/07/89 ! Removed Statutory from heading when Dual ! MJB *~
            *          !  Books is not active.                    !     *~
            * 04/20/90 ! Increased size of debit & credit fields. ! JDH *~
            * 05/06/90 ! Now flags STJBUFFR as posted.            ! JDH *~
            * 06/28/91 ! Added coding for G/L Export file.        ! JBK *~
            *          ! Added CALL to ALLFREE.                   !     *~
            * 03/31/93 ! PRR 12763 Exit program without printing  ! JIM *~
            *          !  header & end of report OR closing print-! JIM *~
            *          !  er when printer was not SELECTed.       ! JIM *~
            *************************************************************

        dim abcacct$9,                   /* ACCOUNT NUMBER FOR STACK   */~
            acct$9,                      /* ACCOUNT NUMBERS FOR ENTRY  */~
            adj$4,                       /* Adj Flag - N/A here        */~
            aid$1,                       /* AID CHAR FOR GET PARM      */~
            begdate$(17)8,               /* BEGINNING FISCAL DATES     */~
            coname$60,                                                   ~
            credit$12,                   /* CREDIT AMOUNTS THIS ENTRY  */~
            creditsstk$(250)9,           /* CREDIT ACCOUNT RECAP STACK */~
            creditsstk(250),             /* PARALLEL STACK FOR AMOUNTS */~
            date$8,                                                      ~
            debit$12,                    /* DEBIT AMOUNTS THIS ENTRY   */~
            debitsstk$(250)9,            /* DEBIT RECAP STACK          */~
            debitsstk(250),              /* PARALLEL STACK FOR AMOUNTS */~
            descr$32,                    /* ACCOUNT DESCRIPTIONS       */~
            description$36,              /* DESCRIPTION OF ENTRY       */~
            dual_books$1,                /* Dual Books Flag            */~
            gldate$6,                    /* POST DATE                  */~
            jnlid$3,                     /* JOURNAL ID                 */~
            location$2,                  /* LOCATION OF ACCT ON STACK  */~
            moduleno$20,                 /* MODULE NUMBER              */~
            monthe$3,                    /* MONTH END ?                */~
            name$10,                     /* JOURNAL ENTRY NAME         */~
            postedflag$1,                /* Has journal been posted?   */~
            postmsg$35,                  /* MESSAGE FOR SCREEN         */~
            prtacct$16,                  /* POST DATE                  */~
            prtdate$8,                   /* POST DATE                  */~
            pstseq$10,                   /* POSTING SEQ PRINT VARIABLE */~
            rcpacct$(2)16,               /* ACCOUNT NUMBERS FOR RECAP  */~
            rcpacctdescr$(2)30,          /* DESCRIPTIONS FOR RECAP     */~
            rcpamt(2),                   /* RECAP AMOUNTS FOR STUFF    */~
            rcpline%(2),                 /* LINE COUNTER FOR RECAP     */~
            rcpptr%(2),                  /* POINTERS INTO RECAP STACK  */~
            ref1$30,                     /* REFERENCE 1                */~
            ref2$34,                     /* REFERENCE 2                */~
            rptid$6,                     /* Report ID                  */~
            text$100,                    /* GL TEXT                    */~
            time$8,                                                      ~
            title$60                     /* JOURNAL TITLE              */~

        dim                              /* G/L Export Posting info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            reversing_entry$1,           /* Reversing entry flag       */~
            tran_type$5                  /* G/L transaction type       */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #01 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            * #02 ! GLMAIN   ! GENERAL LEDGER MAIN FILE (ACCT DESCR'S)  *~
            * #03 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            * #04 ! SYSFILE2 ! SYSTEM INFORMATION (JOURNAL PAGE NUMBERS)*~
            * #09 ! STJBUFFR ! STANDARD JOURNAL HEADER BUFFER AREA.     *~
            * #10 ! STJBUF2  ! STANDARD JOURNAL DETAIL BUFFER AREA.     *~
            *************************************************************

            select #01, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select  #02, "GLMAIN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #03, "GLDETAIL",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select  #04, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #09, "STJBUFFR",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 7,                          ~
                        alt key 1, keypos= 8, keylen= 7,                 ~
                            key 2, keypos=15, keylen=10

            select #10, "STJBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 13

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (#02, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (#03, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (#04, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (#09, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))

        REM OPEN BUFFER AND GLDETAIL FILES IF NOT ALREADY OPEN
            if f2%( 9) = 0 then L02660
               call "OPENFILE"(#09, "OUTPT", f2%( 9), rslt$( 9), axd$( 9))
               close #09
               call "OPENFILE"(#09, "SHARE", f2%( 9), rslt$( 9), axd$( 9))

L02660:     if f2%(10) = 0 then L02710
               call "OPENFILE"(#10, "OUTPT", f2%(10), rslt$(10), axd$(10))
               close #10
               call "OPENFILE"(#10, "SHARE", f2%(10), rslt$(10), axd$(10))

L02710:     if f2%( 4) = 0 then L09000
               call "OPENFILE"(#04, "OUTPT", f2%( 4), rslt$( 4), axd$( 4))
               close #04
               call "OPENFILE"(#04, "SHARE", f2%( 4), rslt$( 4), axd$( 4))

L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * INITIALIZES THE USUAL CONTROL VARIBLES                    *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            time$ = " " : call "TIME" (time$)
            call "COMPNAME" (12%, coname$, 0%)
            rptid$ = "G/L016"
            postedflag$ = "Y"
            gosub L51000
            printline% = 60

        REM INITIALIZE STACKS
            init(hex(ff)) debitsstk$(), creditsstk$()
            mat debitsstk = zer
            mat creditsstk= zer
            debitsptr%, creditsptr%, prtrec% = 0

        REM RETRIEVE GENERAL LEDGER DATE
            call "READ100" (#01, userid$, f1%(1))
                  if f1%(1) = 0 then exit_prog_no_print
                  get #01, using L09240, gldate$
L09240:                   FMT XX(21),    /* SKIP USERID + OTHER DATES  */~
                              CH( 6)     /* GENERAL LEDGER DATE        */

                call "GETPARM" addr ("I ", "R", "MONTHEND", aid$, "0001",~
                                     "GLTRAL",                           ~
                                     "Are We at Month End?",             ~
                                     20%, "K", "MONTHEND", monthe$, 3%,  ~
                                     5%, 32%, "A")

L09330:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "JURN1 ",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09330
                moduleno$="06"
                returncode% = 1
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                title$, gldate$, #04, f2%(4), returncode%)
            call "FMTTITLE" (title$, "JOURNAL", 12%)

            if monthe$ <> "YES" then L09550
               thismonth% = 2
               call "READ100" (#04, "FISCAL DATES", f1%(4))
                    if f1%(4) = 0 then exit_prog_no_print
                    get #04, using L09510, begdate$(), monthopen%
L09510:                     FMT XX(22), 17*CH(8), BI(2)
                    gldate$ = begdate$(monthopen%)
               go to L09710

L09550: REM RETRIEVE GENERAL LEDGER DATE
            call "READ100" (#01, userid$, f1%(1))
                  if f1%(1) = 0 then exit_prog_no_print
                  get #01, using L09590, gldate$
L09590:                   FMT XX(21),    /* SKIP USERID + OTHER DATES  */~
                              CH( 6)     /* GENERAL LEDGER DATE        */

            call "WHICHPER" (#04, gldate$, thismonth%)
                  if thismonth% > 0 then L09710
                u3% = 0%
                call "ASKUSER" (u3%, "*** GENERAL DATE ERROR ***",       ~
                    "This users general date is not in an open month.",  ~
                    "No journal entries just input will be posted",      ~
                    "Press (RETURN) to acknowledge and exit program")
                goto exit_prog_no_print

L09710:     dual_books$ = "N"        /* Default to 'no' */
            call "READ100" (#04, "SWITCHS.GL", f1%(4))
                if f1%(4) = 0% then goto L09800
            get #04 using L09750, dual_books$
L09750:         FMT POS(21), CH(1)


L09800
*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#4, "SWITCHS.GL", f1%(4))
            if f1%(4) = 1% then get #4 using L09840, export_on$
L09840:         FMT POS(22), CH(1)

        REM *************************************************************~
            *                   M A I N   P R O G R A M                 *~
            *                                                           *~
            * PLOWS THROUGH BUFFER AND PLAYS WITH STUFF.  THEN PROCESSES*~
            * IT, EXITING TO THE DAILY RECAP IF NOT.  NOTE THAT WE ALSO *~
            * WILL GET OUT OF THE PROGRAM IF NO ENTRIES THIS GUY.       *~
            *************************************************************

            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            postmsg$ = "," & hex(84) & "and posting General Ledger."
            if monthe$ <> "YES" then postmsg$ = " "
            call "SHOSTAT" ("Printing Transactions And Recap" & postmsg$)

            oldentrykey$ = userid$

L10140:     call "READ103" (#09, oldentrykey$, f1%(9))
                 if f1%(9) = 0 then L40000
            get #09, using L10170, newentrykey$, name$, description$,     ~
                                  reversing_entry$
L10170:     FMT CH(7), XX(7), CH(10), CH(36), POS(70), CH(1)
                    if str(newentrykey$, 1, 3) <> userid$                ~
                       then L40000
            put #09, using L10194, postedflag$   /* Flag as Posted */
L10194:         FMT POS(69), CH(1)
            rewrite #09
            oldentrykey$ = newentrykey$
            oldreadkey$  = name$
                prtrec% = 1
                returncode% = 0
            moduleno$ = "06"
                returncode% = 0
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                " ", gldate$, #04, f2%(4), returncode%)
            convert pstseq% to pstseq$, pic(##########)
            call "STRING" addr("LJ",pstseq$,10%,pstseq$)
            postid$ = jnlid$ & pstseq$
            printline% = printline% + 3
            prtctl% = 0
            if printline% > 59 then gosub heading
            print using L12530
            print using L12570, name$, description$, "06", postid$
            print using L12530
            printline% = printline% + 4
            prtctl% = 3
            if printline% > 59 then gosub heading
            gosub sub_heading

            totaldebits, totalcredits = 0

        REM *************************************************************~
            *           P R O C E S S   I N F O R M A T I O N           *~
            *                                                           *~
            *************************************************************

L11050:         acct$, descr$ = " "
                debit, credit = 0
                call "READ102" (#10, oldreadkey$, f1%(10))
                     if f1%(10) = 0 then L11610
                get #10, using L11100, newreadkey$
L11100:                        FMT CH(13)
                if str(newreadkey$,1,10)<>str(oldreadkey$,1,10) then L11610
                   oldreadkey$ = newreadkey$
                   get #10, using L11150,                                 ~
                            acct$, ref1$,ref2$,adj$,descr$, debit, credit
L11150:                           FMT XX(13),      /* KEY              */~
                                      CH(9),       /* ACCOUNT NUMBER   */~
                                      CH(30),      /* REF1             */~
                                      CH(34),      /* REF2             */~
                                      CH(04),      /* Adj Flag         */~
                                      CH(32),      /* DESCRIPTION      */~
                                      2*PD(14,4)   /* DEBITS, CREDITS  */

                if acct$ = " " then L11050
                   prtdate$ = gldate$
                   call "DATEFMT" (prtdate$)
                   text$ = " "
                   str(text$, 1) = ref1$
                   str(text$,31) = ref2$
                   str(text$,65) = adj$
                   str(text$,69) = descr$
                call "CONVERT" (debit, 2.2, debit$)
                call "CONVERT" (credit, 2.2, credit$)
                if debit = 0  then debit$ = " "
                if credit = 0 then credit$ = " "
                totaldebits = totaldebits + debit
                totalcredits = totalcredits + credit
                printline% = printline% + 1
                prtctl% = 1
                if printline% > 59% then gosub heading
                prtacct$ = acct$
                call "GLFMT" (prtacct$)
                if export_on$ <> "Y" then L11360
                     tran_type$ = str(jnlid$) & "01"
                     if reversing_entry$ = "Y" then                      ~
                          tran_type$ = str(jnlid$) & "02"
                     gl_postamt = debit - credit
                     gosub load_gl_info
L11360:         print using L12770, prtacct$,prtdate$,ref1$,ref2$,descr$, ~
                                   debit$, credit$
                   if debit <> 0                                         ~
                      then gosub'162(acct$, debit)
                   if credit <> 0                                        ~
                      then gosub'163(acct$, credit)
                   call "GLPOST2"(acct$,                                 ~
                                  debit,                                 ~
                                  credit,                                ~
                                  gldate$,                               ~
                                  1%,                                    ~
                                  "06",                                  ~
                                  text$,                                 ~
                                  jnlid$,                                ~
                                  pstseq%,                               ~
                                  userid$,                               ~
                                  #02,                                   ~
                                  #03,                                   ~
                                  #04,                                   ~
                                  u3%, " ", gl_post_info$())
                goto L11050

L11610:     printline% = printline% + 5
            prtctl% = 2
            if printline% > 59 then gosub heading
            print using L13060
            if totaldebits < 1e8 and totalcredits < 1e8 then L11650
                print using L12810, totaldebits, totalcredits
                goto L11660
L11650:     print using L12798, totaldebits, totalcredits
L11660:     print using L12480
            call "JNLCLOSE" (str(moduleno$,1,2), jnlid$, pstseq%,        ~
                             returncode%)
            call "ALLFREE"
            goto L10140


        REM *************************************************************~
            *           P R I N T  L I S T  R O U T I N E               *~
            *                                                           *~
            * PRINTS JOURNAL ENTRY LIST FROM EITHER THE BUFFER OR MASTER*~
            *************************************************************

L12480: %================================================================~
        ~=================================================================~
        ~===
L12530: %!                                                               ~
        ~                                                                 ~
        ~  !
L12570: %!  JOURNAL NAME: ##########  DESCRIPTION: ######################~
        ~##############  MODULE: ##  POSTING ID: #############            ~
        ~  !
L12770: %!############# ######## ######################### ##############~
        ~########### #############################!############!##########~
        ~##!
L12798: %!                                                               ~
        ~                                         !-########.##!-########.~
        ~##!
L12810: %!                       Total Debits ###########.##             ~
        ~Total Credits ###########.##             !    <---    !    <---  ~
        ~  !
        sub_heading

        print using L12910
L12910: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+
        print using L12950
L12950: %!ACCOUNT NMBR !  DATE  !     REFERENCE 1         !        REFERE~
        ~NCE 2      !       DESCRIPTION           !    DEBIT   !   CREDIT ~
        ~  !
        print using L12990
L12990: %+---------------------------------------------------------------~
        ~-----------------------------------------+------------+----------~
        ~--+
        print using L13030
L13030: %!                                                               ~
        ~                                         !            !          ~
        ~  !
L13060: %!                                                               ~
        ~                                         !------------!----------~
        ~--!
        %!                                                               ~
        ~                                         !============!==========~
        ~==!
        return

        heading
            print page
            pageno% = pageno% + 1
            print using L13371, date$, time$, coname$, "-" & rptid$
            print using L13380, title$, pageno%
            if dual_books$ = "Y" then print using L13410
            print
            print using L12480
            if prtctl% = 0 then printline% = 9%
            if prtctl% = 1 then printline% = 6%
            if prtctl% = 2 then printline% = 8%
            if prtctl% = 3 then printline% = 10%
            prtctl% = 0
            return

L13371: %RUN: ######## @ ########            ############################~
        ~################################                     STJJURN1####~
        ~###
L13380: %                                    ############################~
        ~################################                          PAGE: #~
        ~###
L13410: %                                              S T A T U T O R Y ~
        ~  G / L   S Y S T E M
L13430: %                                                       *** END O~
        ~F REPORT ***

        load_gl_info

            put str(gl_post_info$(),,) using L14490,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Currency Units per Book    */~
                gl_postamt,              /* Functional Currency amount */~
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
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                " ",                     /* Check Payment Number CH(8) */~
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

L14490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
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
                CH(8),                   /* Check Payment Number CH(8) */~
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

L40000: REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  *******CENSORED********  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

            if newentrykey$ = userid$ then L65000   /* NOTHING DONE.    */
            if prtrec% = 0 then L65000              /* NOTHING DONE.    */

            totaldebits, totalcredits, colsdone% = 0
            mat rcpline% = con
            mat rcpptr% = zer
            gosub L48000        /* SKIP TO TOP OF PAGE.                 */

L40160:     for column% = 1 to 2
                on column% gosub L40230, L40640
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then L55000          /* DONE W/ REPORT   */
                goto L40160

L40230:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L40270, L40460, L40500, L40540, L40580
                return

L40270:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L40500
                    rcpptr%(1) = rcpptr%(1) + 1
                    rcpamt(1)=debitsstk(rcpptr%(1))
                    rcpacct$(1)=debitsstk$(rcpptr%(1))
                    REM GET ACCOUNT DESCRIPTION
                        call "READ100" (#02, rcpacct$(1), f1%(2))
                        if f1%(2) = 0                                    ~
                           then rcpacctdescr$(1)="ACCOUNT NOT ON FILE"   ~
                           else get #02, using L40370, rcpacctdescr$(1)
L40370:                                 FMT XX(9), CH(30)
                    call "GLFMT" (rcpacct$(1))
                    print using L48190,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits=totaldebits+debitsstk(rcpptr%(1))
                    totaldebits=round(totaldebits, 2)
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
                           FMT XX(9), CH(30)
L40460:         REM PRINTS SEPARATOR LINE.
                    print using L48180, "*--";
                    rcpline%(1) = 3
                    return
L40500:         REM PRINTS TOTAL LINE
                    print using L48200, totaldebits;
                    rcpline%(1) = 4
                    return
L40540:         REM PRINTS STARS
                    print using L48170, "*";
                    rcpline%(1) = 5
                    return
L40580:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L40640:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(70);
                on rcpline%(2) gosub L40680, L40860, L40900, L40940, L40980
                   return
L40680:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L40900
                    rcpptr%(2) = rcpptr%(2) + 1
                    rcpamt(2)=creditsstk(rcpptr%(2))
                    rcpacct$(2)=creditsstk$(rcpptr%(2))
                    REM PRINT ACCOUNT DESCIPTION.
                        call "READ100" (#02, rcpacct$(2), f1%(2))
                        if f1%(2)=0                                      ~
                           then rcpacctdescr$(2)="ACCOUNT NOT ON FILE"   ~
                           else get #02, using L40850, rcpacctdescr$(2)
                    call "GLFMT" (rcpacct$(2))
                    print using L48190,rcpacct$(2),rcpacctdescr$(2),      ~
                               rcpamt(2);
                    totalcredits=totalcredits+creditsstk(rcpptr%(2))
                    totalcredits=round(totalcredits, 2)
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L40850:             FMT XX(9), CH(30)
L40860:         REM PRINT SEPARATOR LINE
                    print using L48180, "*--";
                    rcpline%(2) = 3
                    return
L40900:         REM PRINT TOTAL CREDITS LINE
                    print using L48210, totalcredits;
                    rcpline%(2) = 4
                    return
L40940:         REM PRINT STARS
                    print using L48170,"*";
                    rcpline%(2) = 5
                    return
L40980:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L48000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                   print page
                   rcppage% = 1
                   print using L13371, date$, time$, coname$, "-"&rptid$
                   print using L13380, title$, rcppage%
                   print using L13410
                   print using L48310
                   print
                   print using L48350
                   print using L48390
                   print using L48430
                   return

L48170: %**************************#************************************
L48180: %#--------------+--------------------------------+-------------*
L48190: %* ############ ! ############################## !-#########.##*
L48200: %*              ! TOTAL DEBITS                  -###########.##*
L48210: %*              ! TOTAL CREDITS                 -###########.##*

L48310: %=========================D E B I T S=========================== ~
        ~     ===========================C R E D I T S====================~
        ~===

L48350: %*************************************************************** ~
        ~     ************************************************************~
        ~***

L48390: %* ACCOUNT NMBR !     D E S C R I P T I O N      !    AMOUNT   * ~
        ~     * ACCOUNT NMBR !     D E S C R I P T I O N      !    AMOUNT ~
        ~  *

L48430: %*--------------+--------------------------------+-------------* ~
        ~     *--------------+--------------------------------+-----------~
        ~--*

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(abcacct$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  search str(debitsstk$(),1) = str(abcacct$)             ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L50150  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L50150:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = abcacct$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(abcacct$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  search str(creditsstk$(),1) = str(abcacct$)            ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L50300  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L50300:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = abcacct$
                      creditsstk(creditsptr%) = amount
                      return

L51000: REM *************************************************************~
            *    B L O C K / U N B L O C K   P A G E   N U M B E R S    *~
            *                                                           *~
            * THIS SUBROUTINE JUST BLOCKS AND UNBLOCKS THE PAGE NUMBERS *~
            * FOR THE GENERAL JOURNAL.  NOTE THAT PAGE NUMBERS          *~
            * CONSECUTIVELY ARE A NECESSITY AS THERE ARE NO TRACEABLE   *~
            * SOURCE DOCUMENTS FOR THE GENERAL JOURNAL.  BLOCKING ALG   *~
            * GRABS JOURNAL PAGE NUMBERS AND FILLS IN HIS USERID FOR    *~
            * "HELD BY" FIELD.  IF NON-BLANK, SOMEONE ELSE HAS THEM, SO *~
            * WAIT 5 SEC AND CONTINUE TO WAIT TILL THEY'RE FREE.        *~
            * TO UNBLOCK THEM, WRITE BLANKS TO "HELD BY" FIELD.         *~
            *************************************************************

            REM BLOCK ROUTINE
                return
            REM UNBLOCK ROUTINE.
                return

L55000: REM *************************************************************~
            *   T O S S    R O U T I N E   (C L E A R  B U F F E R)     *~
            *                                                           *~
            *************************************************************

*          IF MONTHE$ <> "YES" THEN 65000
*
*          CALL "SHOSTAT" ("Deleting Journal Entries From Buffer")
*
*          OLDREADKEY$ = USERID$
*
*          CALL "READ102" (#09, OLDREADKEY$, F1%(9))
*               IF F1%(9) = 0 THEN 65000
*          GET #09, USING 55820, NEWREADKEY$
*                  FMT CH(7)
*                  IF STR(OLDREADKEY$, 1, 3) <> STR(NEWREADKEY$, 1, 3)  ~
*                     THEN 65000
*          OLDREADKEY$ = NEWREADKEY$
*
*          GET #09, USING 55880, NAME$
*                  FMT XX(14), CH(10)
*          CALL "DELETE" (#10, NAME$, 10%)
*          CALL "DELETE" (#09, NEWREADKEY$, 7%)
*          GOTO 55770

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            *************************************************************

            if printline% > 59 then gosub heading
            print : print using L13430      /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
        exit_prog_no_print
            call "JNLCLOSE" (moduleno$, jnlid$, pstseq%, returncode%)
            call "SHOSTAT" ("One Moment Please")
            end
