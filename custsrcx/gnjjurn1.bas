        REM *************************************************************~
            *                                                           *~
            *   GGG   N   N  JJJJJ  JJJJJ  U   U  RRRR   N   N    1     *~
            *  G      NN  N    J      J    U   U  R   R  NN  N   11     *~
            *  G GGG  N N N    J      J    U   U  RRRR   N N N    1     *~
            *  G   G  N  NN  J J    J J    U   U  R   R  N  NN    1     *~
            *   GGG   N   N   J      J      UUU   R   R  N   N  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GNJJURN1 - PRINTS GENERAL JOURNAL DAILY RECAP AND TOSSES  *~
            *            IT OUT TO THE G/L FILE.                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 06/02/80 ! ORIGINAL                                 ! BCW *~
            * 09/15/80 ! CLEANUP AND ADD POSTING TO GENERAL LEDGER! TEM *~
            * 02/11/82 ! CORRECT REPORT HEADING @ LINE 49080      ! BEV *~
            * 12/13/82 ! DIM LOCATION$, ROUND TOTALS ON RECAP     ! ECR *~
            * 11/11/83 ! NOW POSTS TO ANY JOURNAL                 ! ECR *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 07/30/85 ! CLEANED UP, ADD EXTENDABLE ACCT NUMBER   ! HES *~
            * 09/22/87 ! Minor mods; update print; omit USERINFO  ! JIM *~
            * 11/21/88 ! Support STC Journals in PROC33MN         ! JDH *~
            * 04/07/89 ! Fixed GLDETAIL Description               ! GGO *~
            *          ! Fixed DIM for REF2$ (s/be 34)            !     *~
            * 12/14/90 ! Changed Call to JNLINFO for GL Batches,  ! RAC *~
            *          !    added Call to JNLCLOSE for GL Batches !     *~
            * 06/28/91 ! Added coding for G/L Export file.        ! JBK *~
            *          ! Added CALL to ALLFREE.                   !     *~
            * 08/09/06 ! (AWD001) Mod to add GLORTRAN             ! CMG *~
            *************************************************************

        dim account$9,                   /* ACCOUNT NUMBER FOR STACK   */~
            acct$9,                      /* ACCOUNT NUMBERS FOR ENTRY  */~
            adj$4,                       /* Adj flag for G/L adjs      */~
            aid$1,                       /* AID CHAR FOR GET PARM      */~
            coname$60,                                                   ~
            credit$13,                   /* CREDIT AMOUNTS THIS ENTRY  */~
            creditsstk$(1500)9,          /* CREDIT ACCOUNT RECAP STACK */~
            creditsstk(1500),            /* PARALLEL STACK FOR AMOUNTS */~
            debit$13,                    /* DEBIT AMOUNTS THIS ENTRY   */~
            debitsstk$(1500)9,           /* DEBIT RECAP STACK          */~
            debitsstk(1500),             /* PARALLEL STACK FOR AMOUNTS */~
            descr$36,                    /* ACCOUNT DESCRIPTIONS       */~
            description$36,              /* DESCRIPTION OF ENTRY       */~
            gldate$6,                    /* POST DATE                  */~
            jnlid$3,                     /* JOURNAL ID                 */~
            journal$3,                   /* WHICH JOURNAL IS BEING POST*/~
            location$2,                  /* LOCATION OF ACCT ON STACK  */~
            modno$2,                     /* INPUT RECORD MODULE NUMBER */~
            moduleno$2,                  /* MODULE NUMBER              */~
            moddescr$30,                 /* MODULE DESCRIPTION         */~
            name$10,                     /* JOURNAL ENTRY NAME         */~
            post$3,                      /* YES/NO POST                */~
            postmsg$35,                  /* MESSAGE FOR SCREEN         */~
            postmsg1$12,                 /* POST MESSAGE FOR PRINT     */~
            prtdate$8,                   /* POST DATE                  */~
            pstseq$10,                   /* POSTING SEQ PRINT VARIABLE */~
            rcpacct$(2)16,               /* ACCOUNT NUMBERS FOR RECAP  */~
            rcpacctdescr$(2)30,          /* DESCRIPTIONS FOR RECAP     */~
            rcpamt(2),                   /* RECAP AMOUNTS FOR STUFF    */~
            rcpline%(2),                 /* LINE COUNTER FOR RECAP     */~
            rcpptr%(2),                  /* POINTERS INTO RECAP STACK  */~
            ref1$32,                     /* REFERENCE 1                */~
            ref2$34,                     /* REFERENCE 2                */~
            rptid$6,                     /* Report ID                  */~
            subhdr$80,                                                   ~
            text$100,                    /* GL TEXT                    */~
            time$8,                      /* Time of day                */~
            title$70                     /* JOURNAL TITLE              */

        dim                              /* G/L Export Posting info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            tran_type$5                  /* G/L transaction type       */

        dim division$3,                  /* division number (AWD001)   */~
            schema$8                     /* schema          (AWD001)   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            * #02 ! GLMAIN   ! GENERAL LEDGER MAIN FILE (ACCT DESCR'S)  *~
            * #03 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            * #04 ! SYSFILE2 ! SYSTEM INFORMATION (JOURNAL PAGE NUMBERS)*~
            * #09 ! GNJBUFFR ! GENERAL JOURNAL HEADER BUFFER AREA.      *~
            * #10 ! GNJBUF2  ! GENERAL JOURNAL DETAIL BUFFER AREA.      *~
            * #13 ! GLORTRAN ! GL Oracle transmit file     (AWD001)     *~
            * #14 ! GENCODES ! GENERAL CODES MASTER FILE   (AWD001)     *~
            *************************************************************

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

            select  #09, "GNJBUFFR",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos =  4, keylen = 10,                        ~
                        alt key 1, keypos= 1, keylen= 13

            select #10, "GNJBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 13

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

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#02, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (#03, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (#04, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (#09, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
/*(AWD001)*/
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#14, "SHARE", f2%(14), rslt$(14), axd$(14))
/*(AWD001)*/

            if f2%(13) = 0 then L09000
            call "OPENFILE" (#13, "OUTPT", f2%(13), rslt$(13), axd$(13))
            close #13
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))


L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * INITIALIZES THE USUAL CONTROL VARIBLES                    *~
            *************************************************************

                                                          /* (AWD001)  */

            schema_err%, schema% = 0%
            init(" ") schema$, division$
            call "SCHEMA" (schema$, schema%, #14, schema_err%)


            if schema% = 1% then division$ = "036"   /* NC */
            if schema% = 2% then division$ = "080"   /* NE */
            if schema% = 3% then division$ = "035"   /* AES*/


            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$) : call "TIME" (time$)
            call "COMPNAME" (12%, coname$, 0%)
            rptid$ = "G/L015"

        REM INITIALIZE STACKS
            init(hex(ff)) debitsstk$(), creditsstk$()

            REM DO A GETPARM TO SEE IF WE POST
                call "GETPARM" addr ("I ", "R", "POSTME  ", aid$, "0001",~
                                     "JURN1P",                           ~
         "Enter 'YES' to post to GL, anything else for a trial run only",~
                                     61%, "K", "POSTME  ", post$, 3%,    ~
                                     5%, 32%, "A")

L09310:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "JURN1J",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09310

*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#4, "SWITCHS.GL", f1%(4))
            if f1%(4) = 1% then get #4 using L09540, export_on$
L09540:         FMT POS(22), CH(1)

        REM *************************************************************~
            *                   M A I N   P R O G R A M                 *~
            *                                                           *~
            * PLOWS THROUGH BUFFER AND PLAYS WITH STUFF.  THEN PROCESSES*~
            * IT, EXITING TO THE DAILY RECAP IF NOT.  NOTE THAT WE ALSO *~
            * WILL GET OUT OF THE PROGRAM IF NO ENTRIES THIS GUY.       *~
            *************************************************************

            postmsg$ = "," & hex(84) & "and posting General Ledger."
                if post$ <> "YES" then postmsg$ = " "
            call "SHOSTAT" ("Printing Transactions And Recap" & postmsg$)
            postmsg1$ = " "
                if post$ <> "YES" then postmsg1$ = "(NOT POSTED)"
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)

            oldentrykey$ = userid$
L10160:     call "PLOWALTS" (#09, oldentrykey$, 1%, 3%, f1%(9))
                 if f1%(9) = 0 then L40000
            get #09,using L10190,name$,description$,modno$,gldate$,journal$
L10190:     FMT XX(3), CH(10), CH(36), CH(2), CH(6), CH(3)
*          IF JOURNAL$ = "GYE" AND JNLID$ = "GAJ" THEN 10230
            if journal$ <> jnlid$ then L10160

            REM Ok, all clear to process...
            moduleno$ = modno$
            if post$ <> "YES" then  returncode% = 1%                     ~
                else returncode% = 0%
L10270:         call "JNLINFO" (moduleno$, journal$, pstseq%, " ",       ~
                                title$, gldate$, #04, f2%(4), returncode%)
            if returncode% <> 0 then L10270
            moddescr$ = " "
            call "JNLINFO" ("00", moduleno$, 0%, " ", moddescr$, " ",    ~
                            #04, f1%(4), 2%)
            if moddescr$ = " " then moddescr$ = "Unknown"
            title$ = title$ & " JOURNAL"
            subhdr$ = title$ & "   " & postmsg1$
            call "FMTTITLE" (subhdr$, " ", 2%)
            prtdate$ = gldate$
            call "DATEFMT" (prtdate$)
            convert pstseq% to pstseq$, pic(##########)
            call "STRING" addr("LJ",pstseq$,10%,pstseq$)
            if post$ <> "YES" then pstseq$ = "N/A"
            printline% = 99999999%
            pageno% = 0
            gosub heading

        REM *************************************************************~
            *           P R O C E S S   I N F O R M A T I O N           *~
            *                                                           *~
            *************************************************************

            totaldebits, totalcredits = 0
            oldreadkey$  = name$

L11080:         call "PLOWNEXT" (#10, oldreadkey$, 10%, f1%(10))
                     if f1%(10) = 0 then L11430
                get #10, using L11110, acct$, ref1$, ref2$, adj$, descr$, ~
                    debit, credit
L11110:                           FMT XX(13),      /* KEY              */~
                                      CH(9),       /* ACCOUNT NUMBER   */~
                                      CH(30),      /* REF1             */~
                                      CH(34),      /* REF2             */~
                                      CH(4),       /* Adj flag for Adjs*/~
                                      CH(32),      /* DESCRIPTION      */~
                                      2*PD(14,4)   /* DEBITS, CREDITS  */

                rcpacct$(1) = acct$
                call "GLFMT" (rcpacct$(1))
                text$ = ref1$
                str(text$,31) = ref2$
                str(text$,65) = adj$
                str(text$,69) = descr$
                call "CONVERT" (debit, 2.2, debit$)
                call "CONVERT" (credit, 2.2, credit$)
                if abs(debit ) < .001 then debit$ = " "
                if abs(credit) < .001 then credit$ = " "
                totaldebits = totaldebits + debit
                totalcredits = totalcredits + credit
                gosub heading
                print using L12370, rcpacct$(1), ref1$, ref2$, descr$,    ~
                                                          debit$, credit$
                gosub'162(acct$, debit)
                gosub'163(acct$, credit)
                if post$ <> "YES" then L11080

                if export_on$ <> "Y" then L11350
                     tran_type$  = "GNJ13"           /* Unknown Source */
                     if journal$ = "GNJ" then tran_type$ = "GNJ01"
                     if journal$ = "GAJ" then tran_type$ = "GNJ02"
                     if journal$ = "GYE" then tran_type$ = "GNJ03"
                     if journal$ = "STC" then tran_type$ = "GNJ04"
                     gl_postamt  = debit - credit
                     gosub load_gl_info
L11350:            call "GLPOST2" (acct$, debit, credit, gldate$, 1%,    ~
                                  modno$, text$, journal$, pstseq%,      ~
                                  userid$, division$, #02, #03, #04, #13,~
                                  0%, " ",  /* (AWD001) */               ~
                                  gl_post_info$())
                if post$ <> "YES" then L11080
                call "READ101" (#10, oldreadkey$, f1%(10))
                     if f1%(10) <> 0 then delete #10
                goto L11080

L11430:     print using L12310
            print using L12400, totaldebits, totalcredits
            print using L12430
            if post$ <> "YES" then L11490
            call "REDALT1" (#09, oldentrykey$, 1%, f1%(9))
                if f1%(9) <> 0 then delete #09
            call "JNLCLOSE" (moduleno$, journal$, pstseq%, 0%)
            call "ALLFREE"
L11490:     goto L10160

        REM *************************************************************~
            *           P R I N T  L I S T  R O U T I N E               *~
            *                                                           *~
            * PRINTS JOURNAL ENTRY LIST FROM EITHER THE BUFFER OR MASTER*~
            *************************************************************

        heading
            if printline% > 59 then L12100
               printline% = printline% + 1
               return
L12100:     if pageno% > 0 then print using L12310
            print page
            pageno% = pageno% + 1
            print using L12221, date$, time$, coname$, "-" & rptid$
            print using L12230, subhdr$, pageno%
            print using L12260, modno$,moddescr$,journal$,pstseq$,prtdate$
            print
            print using L12290, name$, description$
            print using L12310
            print using L12340
            print using L12310
            printline% = 8%
            return

L12221: %RUN: ######## @ ########            ############################~
        ~################################                     GNJJURN1####~
        ~###
L12230: %                          ######################################~
        ~##########################################                PAGE: #~
        ~###
L12260: %MODULE: ## ##################################  JOURNAL ID:###   ~
        ~    POSTING SEQ: #############                    POST DATE:#####~
        ~###
L12290: %NAME:##########  DESCRIPTION: ##################################~
        ~########################
L12310: %+----------------+---------------------------+------------------~
        ~-------+------------------------------+-------------+------------~
        ~--+
L12340: %! ACCOUNT        !     REFERENCE 1           !        REFERENCE ~
        ~2      !       DESCRIPTION            !    DEBIT    !    CREDIT  ~
        ~  !
L12370: %!################!###########################!##################~
        ~#######!##############################!#############!############~
        ~# !
L12400: %!                                                               ~
        ~        TOTALS FOR ENTRY =======>     !-#########.##!-#########.#~
        ~# !
L12430: %!                                                               ~
        ~                                      +=============+============~
        ~==+

L12470: %                                                       *** END O~
        ~F REPORT ***

        load_gl_info

            put str(gl_post_info$(),,) using L13490,                      ~
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

L13490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
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

            if debitsptr% = 0 and creditsptr% = 0 then L65000

            totaldebits, totalcredits, colsdone% = 0
            mat rcpline% = con
            mat rcpptr% = zer
            rcpacct$() = " "
            gosub L48000        /* SKIP TO TOP OF PAGE.                 */

L40160:     for column% = 1 to 2
                on column% gosub L40230, L40680
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then L65000          /* DONE W/ REPORT   */
                goto L40160

L40230:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L40270, L40500, L40540, L40580, L40620
                return

L40270:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L40540
                    rcpptr%(1) = rcpptr%(1) + 1
                    rcpamt(1)=debitsstk(rcpptr%(1))
                    rcpacct$(1)=debitsstk$(rcpptr%(1))
                    if rcpacct$(1) <> "OVER FLOW" then L40350
                       rcpacctdescr$(1) = "Rest Of Debits (can't print)"
                       goto L40420
L40350:             REM GET ACCOUNT DESCRIPTION
                        call "READ100" (#02, rcpacct$(1), f1%(2))
                        if f1%(2) = 0                                    ~
                           then rcpacctdescr$(1)="ACCOUNT NOT ON FILE"   ~
                           else get #02, using L40400, rcpacctdescr$(1)
L40400:                                 FMT XX(9), CH(30)
                    call "GLFMT" (rcpacct$(1))
L40420:             print using L48190,rcpacct$(1),rcpacctdescr$(1),      ~
                                         rcpamt(1);
                    totaldebits=totaldebits+debitsstk(rcpptr%(1))
                    totaldebits=round(totaldebits, 2)
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
                           FMT XX(9), CH(30)
L40500:         REM PRINTS SEPARATOR LINE.
                    print using L48180, "*--";
                    rcpline%(1) = 3
                    return
L40540:         REM PRINTS TOTAL LINE
                    print using L48200, totaldebits;
                    rcpline%(1) = 4
                    return
L40580:         REM PRINTS STARS
                    print using L48170, "*";
                    rcpline%(1) = 5
                    return
L40620:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L40680:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(68);
                on rcpline%(2) gosub L40720, L40940, L40980, L41020, L41060
                   return
L40720:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L40980
                    rcpptr%(2) = rcpptr%(2) + 1
                    rcpamt(2)=creditsstk(rcpptr%(2))
                    rcpacct$(2)=creditsstk$(rcpptr%(2))
                    if rcpacct$(2) <> "OVER FLOW" then L40800
                       rcpacctdescr$(2) = "Rest Of Credits (can't print)"
                       goto L40860
L40800:             REM PRINT ACCOUNT DESCIPTION.
                        call "READ100" (#02, rcpacct$(2), f1%(2))
                        if f1%(2)=0                                      ~
                           then rcpacctdescr$(2)="ACCOUNT NOT ON FILE"   ~
                           else get #02, using L40930, rcpacctdescr$(2)
                    call "GLFMT" (rcpacct$(2))
L40860:             print using L48190,rcpacct$(2),rcpacctdescr$(2),      ~
                               rcpamt(2);
                    totalcredits=totalcredits+creditsstk(rcpptr%(2))
                    totalcredits=round(totalcredits, 2)
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L40930:             FMT XX(9), CH(30)
L40940:         REM PRINT SEPARATOR LINE
                    print using L48180, "*--";
                    rcpline%(2) = 3
                    return
L40980:         REM PRINT TOTAL CREDITS LINE
                    print using L48210, totalcredits;
                    rcpline%(2) = 4
                    return
L41020:         REM PRINT STARS
                    print using L48170,"*";
                    rcpline%(2) = 5
                    return
L41060:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L48000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                   print page
                   rcppage% = 1
                   print using L12221, date$, time$, coname$, "-" & rptid$
                   if journal$ = "01" then                               ~
                      print using L48230, title$, rcppage%                ~
                         else                                            ~
                      print using L48270, title$, rcppage%
                   print using L48292, postmsg1$
                   print using L48310
                   print
                   print using L48350
                   print using L48390,"#","#"
                   print using L48430
                   return

L48170: %**************************#*************************************
L48180: %#--------------+--------------------------------+--------------*
L48190: %* ############ ! ############################## !-#########.## *
L48200: %*              ! TOTAL DEBITS                   !-#########.## *
L48210: %*              ! TOTAL CREDITS                  !-#########.## *

L48230: %########################################             J O U R N A~
        ~ L  R E C A P                                             PAGE: #~
        ~###

L48270: %########################################             J O U R N A~
        ~ L  R E C A P                                             PAGE: #~
        ~###
L48292: %                                                           #####~
        ~#######
L48310: %=========================D E B I T S============================~
        ~   =============================C R E D I T S====================~
        ~==

L48350: %****************************************************************~
        ~   **************************************************************~
        ~**

L48390: %* ACCOUNT #    !     D E S C R I P T I O N      !    AMOUNT    *~
        ~   * ACCOUNT #    !     D E S C R I P T I O N      !    AMOUNT   ~
        ~ *

L48430: %*--------------+--------------------------------+--------------*~
        ~   *--------------+--------------------------------+-------------~
        ~-*

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  if abs(amount) < .001 then return
                  search str(debitsstk$(),1) = account$                  ~
                               to location$ step 9 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L50160  /* PUSH NEW ITEM*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH CELL?  */
                     debitsstk(junk%) = debitsstk(junk%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L50160:           REM PUSH NEW ITEM ONTO STACK.
                      if debitsptr% < dim(debitsstk$(),1) then L50220
                         debitsstk$(debitsptr%) = "OVER FLOW"
                         debitsstk(debitsptr%) =                         ~
                         debitsstk(debitsptr%) + amount
                         return
L50220:               debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  if abs(amount) < .001 then return
                  search str(creditsstk$(),1) = account$                 ~
                               to location$ step 9
                               /* SCAN ONLY WHAT IS USED OF STACK      */
                  if location$ = hex(0000) then L50370  /* IF NO ==> NEW*/
                     junk% = int(val(location$,2)/9)+1 /* WHICH ELEMENT*/
                     creditsstk(junk%) = creditsstk(junk%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L50370:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      if creditsptr% < dim(creditsstk$(),1) then L50430
                         creditsstk$(creditsptr%) = "OVER FLOW"
                         creditsstk(creditsptr%) =                       ~
                         creditsstk(creditsptr%) + amount
                         return
L50430:               creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      creditsstk(creditsptr%) = amount
                      return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS A       *~
            * MESSAGE (ONLY IN FOREGROUND) WHILE LINKING TO THE NEXT    *~
            * PROGRAM.                                                  *~
            *************************************************************

        REM Finish the print-out
            print : print using L12470          /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            call "SHOSTAT" ("One Moment Please")
            end
