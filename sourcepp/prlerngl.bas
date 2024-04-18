        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      EEEEE  RRRR   N   N   GGG   L       *~
            *  P   P  R   R  L      E      R   R  NN  N  G      L       *~
            *  PPPP   RRRR   L      EEEE   RRRR   N N N  G GGG  L       *~
            *  P      R   R  L      E      R   R  N  NN  G   G  L       *~
            *  P      R   R  LLLLL  EEEEE  R   R  N   N   GGG   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLERNGL - POST CONTENTS OF EARNINGS BUFFER TO G/L AND    *~
            *            PRINT REPORT.  NOTE THAT THE EARNINGS ACCOUNT  *~
            *            ARE DEBITS AND WE CREDIT THE GROSS PAYROLL     *~
            *            ACCOUNT (LIABILITY).                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/12/80 ! ORIGINAL                                 ! BCW *~
            * 08/03/81 ! ROUNDING ON GROSS TOTALS.                ! TEM *~
            *          ! ADDITION OF MTD HOLD AREA FOR NEXT MONTH ! TEM *~
            * 09/28/81 ! SORTED DAILY RECAP                       ! TEM *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 07/23/85 ! EXPANDABLE ACCOUNT NUMBER, ADD CAT TO STK! HES *~
            * 12/09/85 ! Added Input Audit Report                 ! HES *~
            * 07/25/89 ! Fixed Possible Contention with In/Process! KAB *~
            *          ! @ 50K Area (Potential FS 22)             !     *~
            * 12/26/90 ! Changed Call to JNLINFO for GL Batches,  ! RAC *~
            *          !    added Call to JNLCLOSE for GL Batches !     *~
            * 06/20/91 ! Added code for G/L Export file.          ! JBK *~
            *          ! Added CALL to ALLFREE                    !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 12/29/93 ! PRR 13077. Elliminated superfluous read. ! JDH *~
            * 07/26/94 ! PRR 13634  Eliminated erroneous printing ! MLJ *~
            *          !   of '**Not in Personnel File**' line    !     *~
            *          !   preceeding Grand Totals line on Audit. !     *~
            * 07/26/95 ! PRR 13471 - Added remark to Audit Trail  ! JBK *~
            *          !   report inicating that the Dollars      !     *~
            *          !   Earned figure is the result of an      !     *~
            *          !   operator override since it does not    !     *~
            *          !   equal the rate times units.            !     *~
            *************************************************************

        dim                                                              ~
            account$9,                   /* STACK PUSHING ARGUMENT     */~
            acct$9,                      /* EARNINGS RECORD EXPENSE ACC*/~
            addamount$10,                /* FOR REPORT                 */~
            addunits$10,                 /* FOR REPORT                 */~
            cash$1,                      /* PAID IN CASH? FLAG         */~
            creditsstk$(300)12,          /* CREDIT ACCOUNT RECAP STACK */~
            creditsstk(300),             /* PARALLEL STACK FOR AMOUNTS */~
            date$8,                      /* TODAY'S DATE SCREEN DISPLAY*/~
            debitsstk$(300)12,           /* DEBIT RECAP STACK          */~
            debitsstk(300),              /* PARALLEL STACK FOR AMOUNTS */~
            deductflag$1,                /* PROCESS ON THIS RUN?       */~
            catg$4,                      /* CATEGORY   ON EARNINGS REC */~
            earnrec$36,                  /* FILLER FOR EARNINGS RECORD */~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            estack$(300)20,              /* DEPARTMENT RECAP STACK     */~
            estak1(300),                 /* UNITS TOTAL                */~
            estak2(300),                 /* DOLLARS TOTAL              */~
            freqs%(7),                   /* Frequencies paid this run  */~
            grossacct$9,                 /* GROSS PAY ACCOUNT          */~
            hdrdate$45,                  /* FORMATTED DATE/TIME INFO   */~
            jnlid$3,                     /* JOURNAL ID                 */~
            location$2,                  /* LOCATION FOR STACK ROUTINE */~
            moduleno$20,                 /* MODULE ID                  */~
            name$30,                     /* NAME OF EMPLOYEE           */~
            name$(3)20,                  /* EMPLOYEE'S NAME            */~
            oldfreqs%(7),                /* Current Frequencies in pros*/~
            oldempcode$12,               /* Last Employee Code         */~
            print$(4)20,                 /* PRINT VARIABLES            */~
            prtname$40,                  /* NAME OF EMPLOYEE           */~
            prttype$15,                  /* EARNINGS TYPE FOR PRINT    */~
            rate$10,                     /* FOR PRINT                  */~
            rcpacct$(2)16,               /* DEBIT, CREDIT ACCS--RCP#1  */~
            rcpacctdescr$(2)30,          /* DESCRIPTIONS OF ACCOUNTS   */~
            rcpamt(2),                   /* AMOUNTS OF RECAP STUFF.    */~
            rcpline%(5),                 /* RECAP LINE NUMBERS         */~
            rcpptr%(2),                  /* POINTER INTO STACKS        */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            seqnr$3,                     /* SEQUENCE NUMBERS ALPHA     */~
            sickcode$2,                  /* SICK ACCRUAL METHOD (CODE) */~
            string$20,                   /* WORK VARIABLE              */~
            summary$1,                   /* JOURNAL SUMMARY INDICATOR  */~
            tempo$7,tempn$7,             /* IN PROCESS FLAGS           */~
            temp2$19,                    /* Temporary Variable         */~
            text$100,                    /* TEXT TO G/LPOST ROUTINE    */~
            title$60,                    /* JOURNAL TITLE              */~
            txbl$1,                      /* TAXABLE? THIS EARNINGS REC */~
            type$12,                     /* EARNINGS TYPE CODE         */~
            units$6,                     /* UNITS OF PAY THIS EARNING  */~
            vacacode$2                   /* VACATION ACCRUAL METHOD    */~

        dim                              /* G/L Export Posting info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            gl_catg$4,                   /* G/L P/R Earnings Catagory  */~
            gl_depart$4,                 /* G/L P/R Employee Depart #  */~
            gl_earnings$12,              /* G/L P/R Earnings Descr.    */~
            gl_employee$12,              /* G/L P/R Employee Number    */~
            tran_type$5                  /* G/L transaction type       */

        dim f2%(20),                     /* FILE STATUS FLAGS FOR      */~
            f1%(20)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! PAYROLL DATE (TO POST TO G/L)            *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN ACCOUNT FILE.        *~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! GLDETAIL ! GENERAL LEDGER TRANSACTION DETAIL FILE   *~
            * # 6 ! USERINFO ! SYSTEM USER INFO FILE (PAYROLL DATES)    *~
            * # 7 ! PERMASTR ! PERSONNEL MASTER FILE                    *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select # 2, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select # 3, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select # 4, "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alt key  1, keypos =  16, keylen = 28

            select # 5, "GLDETAIL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 26

            select # 6, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select # 7, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1,  keypos = 28,  keylen = 23,          ~
                            key  2,  keypos =  2,  keylen = 49,          ~
                            key  3,  keypos =  1,  keylen = 50


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call"SHOSTAT"("Preparing To Post Earnings To General Ledger")

            call "OPENCHCK" (#1, 0%, f2%(1), 0%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2), 0%, " ")
            call "OPENCHCK" (#3, 0%, f2%(3), 0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4), 0%, " ")
            call "OPENCHCK" (#5, 0%, f2%(5), 0%, " ")
            call "OPENCHCK" (#6, 0%, f2%(6), 0%, " ")
            call "OPENCHCK" (#7, 0%, f2%(7), 0%, " ")

        REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            pageline% = 123456789

            REM GET PAYROLL DATE AND SET CORRESPONDING WHICHPER%
                call "EXTRACT" addr("ID", userid$)
                call "READ100" (#6, userid$, f1%(6))
                     if f1%(6) = 0 then L65000
                get #6, using L09190, prldate$
L09190:                 FMT XX(3), XX(12), CH(6)
                call "WHICHPER" (#1, prldate$, whichper%)
                     if whichper% = 0 then L65000

            call "REDALT0" (#3, "Y", 1%, f1%(3))
                 if f1%(3) = 0 then L65000

L09260:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "PRERNG",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09260

                returncode% = 0
                moduleno$ = "09"
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                title$, prldate$, #1, f2%(1), returncode%)
                call "FMTTITLE" (title$, "RECAP", 12%)

*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#1, "SWITCHS.GL", f1%(1))
            if f1%(1) = 1% then get #1 using L09395, export_on$
L09395:         FMT POS(22), CH(1)

            REM INITIALIZE STACKS
                gosub L60000

L10000: REM *************************************************************~
            *           L O A D   E M P L O Y E E   R E C O R D         *~
            *                                                           *~
            * LOADS THE NEXT EMPLOYEE WHO HAS EARNINGS TO BE PROCESSED, *~
            * AND POSTS THOSE WHO DO.                                   *~
            *************************************************************

            call "ALLFREE"

            gosub L30000
            if deductflag$ = "Y" then gosub L11000
            call "READNEXT" (#3, f1%(3))
                 if f1%(3) = 0 then L40000
            goto L10000

L11000: REM *************************************************************~
            *       P O S T   E M P L O Y E E ' S   E A R N I N G S     *~
            *                                                           *~
            * ONCE ROUTINE ABOVE HAS FOUND ELIGIBLE EMPLOYEES, THIS     *~
            * ROUTINE TAKES OVER AND POSTS THE NONZERO EARNINGS RECORDS *~
            * TO THE G/L STACK, THEN POSTS THE STACK AND THE TOTAL      *~
            * EARNINGS TO THE G/L IN THE APPROPRIATE PLACES.            *~
            * ALSO ADD "ADD AMOUNT" AND "ADD UNITS" TO "CURRENT AMOUNT" *~
            * AND "CURRENT UNITS", RESPECTIVELY.                        *~
            * FURTHER, ADD TO RESPECTIVE ACCUMULATORS AS SPECIFIED      *~
            *************************************************************

            REM INITIALIZE THE DATA FOR THIS EMPLOYEE
                anything%,addunits,addamount,gross = 0

            REM Accumulate Previous Entries...
            readkey$ = empcode$
L11170:     call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                if f1%(4) = 0 then L11260
            get #4, using L11200, temp, temp1, currunits, curramount
L11200:     FMT XX(84), 4*PD(14,4)
            if temp <> 0 or temp1 <> 0 then anything% = 1
            addunits = addunits + currunits
            addamount = addamount + curramount
            goto L11170

L11260:     if anything% = 0 then L11740 /* Release file, on to next */
            readkey$ = empcode$
            gosub L12000   /* Print 'Opening' amounts */
            str(text$,31) = empcode$
            str(text$,69) = "EMP: " & name$
            if freq% <> 0% then freqs%(freq%) = 1%

            if hereonce% <> 0 then L11370
            call "SHOSTAT" ("Posting Earnings to General Ledger And Print~
        ~ing Recap")
            hereonce% = 1

L11370:     REM NOW PLOW THROUGH EARNINGS RECORDS
L11380:         call "PLOWNXT1" (#4, readkey$, 12%, f1%(4))
                     if f1%(4) = 0 then L11670
                gosub L31000              /* Get earnings record.       */
                if addamount = 0 and addunits = 0 then L11380 /*NOTHING */
                addamount = round(addamount, 2)
                curramount = curramount + addamount
                currunits  = currunits  + addunits
                gross = gross + addamount
                gosub L32000               /* Update Earnings Record */

                gosub'162(acct$, addamount)     /* Recap debits amt */

            REM NOW POST EARNINGS EXPENSE AMOUNT TO G/L.
                str(text$,,30) = type$
                if summary$ = "Y" then L11640

                     if export_on$   <> "Y" then L11570
                        tran_type$    = "EEX02"
                        gl_catg$      = catg$
                        gl_earnings$  = type$
                        gl_depart$    = depart$
                        gl_employee$  = empcode$
                        gl_postamt    = addamount
                        gosub load_gl_info
L11570:         call "GLPOST2" (acct$, addamount, 0, prldate$,           ~
                               0%, "09", text$, jnlid$,                  ~
                               pstseq%, userid$, #2, #5, #1,             ~
                               err%, " ", gl_post_info$())

            REM MOVE ON...
L11640:         gosub L12000                     /* print it.    */
                goto L11380                      /* get next.    */

L11670:     REM NOW POST GROSS PAYROLL ACCOUNT...
                gosub'163(grossacct$, gross)
                str(text$,,30) = " "
                if summary$ = "Y" then L11750

                     if export_on$   <> "Y" then L11700
                        tran_type$    = "EEX01"
                        gl_catg$      = " "
                        gl_earnings$  = " "
                        gl_depart$    = depart$
                        gl_employee$  = empcode$
                        gl_postamt    = -gross
                        gosub load_gl_info
L11700:         call "GLPOST2" (grossacct$, 0, gross, prldate$,          ~
                               0%, "09", text$, jnlid$,                  ~
                               pstseq%, userid$, #2, #5, #1,             ~
                               err%, " ", gl_post_info$())

L11740:     REM GO BACK AND GET NEXT EMPLOYEE.
L11750:         return

L12000: REM *************************************************************~
            *      P R I N T   A N   E A R N I N G S   E N T R Y        *~
            *                                                           *~
            * HANDLES THE PRINTING OF THE EARNINGS ENTRY JUST POSTED    *~
            *************************************************************

            REM IF SAME EMPLOYEE, BLANK OUT NAME VARIABLE
                if empcode$ <> oldempcode$ then L12110
                   prtname$ = " "
                   go to L12430

L12110:     REM NEW EMPLOYEE.  PRINT TOTAL AND TAG LINE UNLESS FIRST ONE
                if oldempcode$ = " " then L12240
                gosub L13000
                call "CONVERT" (empearned, 2.4, addamount$)
                call "CONVERT" (empunits, 2.4, addunits$)
                print using L13480, " ", " EMPLOYEE TOTAL", addunits$,    ~
                                                     " ", addamount$, " "
                totalunits = totalunits + empunits
                totaldollars = totaldollars + empearned
                gosub L13000
                if pageline% = 7 then L12240
                print using L13460

L12240:     REM SET EMPLOYEE CODE, NAME AND ZERO EARNINGS
                prtname$ = "**Not in Personnel File**"
                call "READ100" (#7, empcode$, f1%(7))
                     if f1%(7) = 0 then L12370
                get #7, using L12330, name$(3), name$(1), name$(2)
L12330:         FMT XX(1), CH(15), CH(10), CH(1)
                name$ = name$(3) & ", " & name$(1) & " " & name$(2)
                if name$(2) <> " " then name$ = name$ & "."
                prtname$ = "(" & empcode$ & ") " & name$
L12370:         empearned, empunits = 0
                oldempcode$ = empcode$
                if oldempcode$ = hex(ffffffffffffffffffffffff) then L12590
                prttype$ = "   *PRIOR INPUT"
                rate$, temp$ = " "
                goto L12520

L12430:     REM PRINT ENTRY, TOTAL AND RETURN
                temp$ = acct$
                call "GLFMT" (temp$)
                prttype$ = type$
                call "CONVERT" (rate, 2.4, rate$)
                gosub'172(depart$, type$, addunits, addamount)
                newearned = newearned + addamount
                newunits = newunits + addunits

L12520:         call "CONVERT" (addunits, 2.4, addunits$)
                call "CONVERT" (addamount, 2.4, addamount$)
                gosub L13000
                temp2$ = " "
                if addunits = 0 and addamount = 0 then L12550
                if rate = 0 or addunits = 0 then L12550
                if addunits * rate = addamount then L12550
                temp2$ = "Dollars Override?"
L12550:         print using L13510, prtname$, prttype$, addunits$, rate$, ~
                                             addamount$, temp$, temp2$
                temp2$ = " "  :  rate = 0
                empearned = empearned + addamount
                empunits = empunits + addunits
L12590:         return

        load_gl_info

            put str(gl_post_info$(),,) using L12700,                      ~
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
                gl_employee$,            /* Employee number CH(12)     */~
                gl_depart$,              /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                gl_earnings$,            /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                gl_catg$,                /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L12700: FMT     CH(5),                   /* Transaction Type CH(5)     */~
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

L13000: REM *************************************************************~
            *    P A G E    C O N T R O L   R O U T I N E               *~
            *                                                           *~
            * CONTROLS WHETHER OR NOT TO PRINT PAGE HEADER              *~
            *************************************************************

            select printer (134)
            REM CHECK PAGE BREAK
                pageline% = pageline% + 1
                if pageline% < 60 then return

            REM IS THIS PAGE ONE?
                if pagenumber% = 0 then L13150
                print using L13390

L13150:     REM NEW PAGE
                call "DATE" addr("HD", hdrdate$)
                print page
                pagenumber% = pagenumber% + 1
                print using L13360, pagenumber%, hdrdate$
                print
                print using L13390
                print using L13410
                print using L13430
                print using L13460

            REM SET PAGE LINE AND RETURN
                pageline% = 7
                return

        REM *************************************************************~
            *        I M A G E     S T A T E M E N T S                  *~
            *                                                           *~
            * IMAGE STATEMENTS FOR EARNINGS AUDIT TRAIL                 *~
            *************************************************************

L13360: % PAGE###  E A R N I N G S   I N P U T   A U D I T   T R A I L   ~
        ~   #############################################

L13390: % +----------------------------------------+-----------------+---~
        ~--------+-----------+------------+-------------+
L13410: % !E M P L O Y E E   I N F O R M A T I O N !    EARNINGS     !   ~
        ~UNITS   !           !  DOLLARS   !   EXPENSE   !
L13430: % !                                        !      TYPE       !   ~
        ~WORKED  !  PAY RATE !   EARNED   !   ACCOUNT   !

L13460: % !----------------------------------------!-----------------!---~
        ~--------!-----------!------------!-------------!
L13480: % !########################################! ################!###~
        ~####### !########## ! ########## ! ############!

L13510: % !########################################! ################!###~
        ~####### !########## ! ########## ! ############! ################~
        ~####

L30000: REM *************************************************************~
            *           L O A D   E M P L O Y E E   R E C O R D         *~
            *                                                           *~
            * LOADS THAT WHICH WE NEED TO SEE FROM THE EMPLOYEE MASTER  *~
            * RECORD.  THE FIELDS THAT WE ARE IN PARTICULAR CONCERNED   *~
            * WITH ARE THE EMPLOYEE CODE, NAME, GROSS PAYROLL ACCOUNT,  *~
            * AND WHETHER OR NOT THIS EMPLOYEE SHOULD BE INCLUDED.      *~
            *************************************************************

            get   # 3, using L30230, empcode$, temp$, grossacct$,         ~
                                    deductflag$, vacacode$, sickcode$,   ~
                                    depart$

            freq% = 0%
            convert temp$ to freq%, data goto L30190
L30190:     return

L30230:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                XX(4),                   /* WORKSTATION CODE           */~
                XX(1),                   /* FED FILING STATUS          */~
                XX(1),                   /* STATE FILING STATUS        */~
                CH(1),                   /* PAY FREQUENCY              */~
                XX(1),                   /* MODE OF PAYMENT            */~
                XX(4),                   /* DIRECT DEPOSIT BANK CODE   */~
                XX(12),                  /* D.D. ACCOUNT NUMBER        */~
                XX(9),                   /* CASH IN BANK ACCOUNT       */~
                CH(9),                   /* GROSS PAYROLL ACCOUNT      */~
                XX(2),                   /* NORMAL HRS/DAY ASCII(##)   */~
                XX(1),                   /* AUTOPAYROLL FLAG           */~
                XX(6),                   /* FIRST DATE PAY PERIOD      */~
                XX(6),                   /* LAST DATE PAY PERIOD       */~
                CH(1),                   /* DEDUCT ON THIS PRLDDUCT RUN*/~
                XX(02),                  /* NORMAL HRS/WEEK ASCII(##)  */~
                XX(10),                  /* WORKMANS' COMP. CODE DEF   */~
                CH(2),                   /* VACATION ACCRUAL METHOD    */~
                CH(2),                   /* SICK ACCRUAL METHOD (CODE) */~
                XX(3),                   /* O/H NUMBER                 */~
                XX(2),                   /* PRIMARY STATE CODE         */~
                CH(4),                   /* DEPARTMENT                 */~
                XX(41)                   /* FILLER FOR REST OF RECORD  */

L31000: REM *************************************************************~
            *           L O A D   E A R N I N G S   R E C O R D         *~
            *                                                           *~
            * LOADS THE EARNINGS RECORDS FOR THE EMPLOYEE.  WE MIGHT AS *~
            * WELL GET THE WHOLE THING SINCE WE MIGHT WANT TO MODIFY THE*~
            * REPORT LATER.                                             *~
            *************************************************************

            get   #4, using L31160, seqnr$,                               ~
                       type$, catg$, cash$, txbl$, units$, rate, acct$,  ~
                       usunits, usbucks, addunits, addamount, currunits, ~
                       curramount, mtdunits, mtdamount, qtdunits,        ~
                       qtdamount, ytdunits, ytdamount, earnrec$
            return

L31160:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE CODE (AGAIN)      */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(4),                   /* CATEGORY   CODE            */~
                CH(1),                   /* PAID IN CASH? FLAG         */~
                CH(1),                   /* IS THIS TAXABLE?           */~
                CH(6),                   /* UNIT DESCRIPTION           */~
                PD(14,4),                /* UNIT RATE                  */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                                                                         ~
                PD(14,4),                /* USUAL PAYROLL   UNITS      */~
                PD(14,4),                /* USUAL PAYROLL   AMOUNT     */~
                PD(14,4),                /* BUFFER ADD      UNITS      */~
                PD(14,4),                /* BUFFER ADD      AMOUNT     */~
                PD(14,4),                /* CURRENT CHECK   UNITS      */~
                PD(14,4),                /* CURRENT CHECK   AMOUNT     */~
                PD(14,4),                /* MONTH   TO DATE UNITS      */~
                PD(14,4),                /* MONTH   TO DATE AMOUNT     */~
                PD(14,4),                /* QUARTER TO DATE UNITS      */~
                PD(14,4),                /* QUARTER TO DATE AMOUNT     */~
                PD(14,4),                /* YEAR    TO DATE UNITS      */~
                PD(14,4),                /* YEAR    TO DATE AMOUNT     */~
                CH(36)                   /* FILLER FOR REST OF RECORD  */~

            FMT PD(14,4),                /* HOLD FOR MTD UNITS         */~
                PD(14,4)                 /* HOLD FOR MTD AMOUNT        */~

L32000: REM *************************************************************~
            *           S A V E   E A R N I N G S   R E C O R D         *~
            *                                                           *~
            * SAVES THE EARNINGS RECORDS FOR THE EMPLOYEE.              *~
            *************************************************************

            rewrite #4, using L32170, empcode$, seqnr$, empcode$,         ~
                        type$, catg$, cash$, txbl$, units$, rate, acct$, ~
                        usunits, usbucks, 0, 0, currunits, curramount,   ~
                        mtdunits, mtdamount, qtdunits, qtdamount,        ~
                        ytdunits, ytdamount, earnrec$
            return

L32170:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(12),                  /* EMPLOYEE CODE (AGAIN)      */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(4),                   /* CATEGORY   CODE            */~
                CH(1),                   /* PAID IN CASH? FLAG         */~
                CH(1),                   /* IS THIS TAXABLE?           */~
                CH(6),                   /* UNIT DESCRIPTION           */~
                PD(14,4),                /* UNIT RATE                  */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                                                                         ~
                PD(14,4),                /* USUAL PAYROLL   UNITS      */~
                PD(14,4),                /* USUAL PAYROLL   AMOUNT     */~
                PD(14,4),                /* BUFFER ADD      UNITS      */~
                PD(14,4),                /* BUFFER ADD      AMOUNT     */~
                PD(14,4),                /* CURRENT CHECK   UNITS      */~
                PD(14,4),                /* CURRENT CHECK   AMOUNT     */~
                PD(14,4),                /* MONTH   TO DATE UNITS      */~
                PD(14,4),                /* MONTH   TO DATE AMOUNT     */~
                PD(14,4),                /* QUARTER TO DATE UNITS      */~
                PD(14,4),                /* QUARTER TO DATE AMOUNT     */~
                PD(14,4),                /* YEAR    TO DATE UNITS      */~
                PD(14,4),                /* YEAR    TO DATE AMOUNT     */~
                CH(36)                   /* FILLER FOR REST            */

L40000: REM *************************************************************~
            *         F I N I S H   U P   A N D   E X I T               *~
            *                                                           *~
            * PRINTS FINAL JOURNALS AND ENDS.                           *~
            *************************************************************

            REM DONE WITH THIS USERID.  PRINT LAST TOTALS
                if pageline% = 123456789 then L41000
                init(hex(ff)) empcode$   /* FAKE OUT PRINT ROUTINE */
                gosub L12000
                call "CONVERT" (totaldollars-newearned, 2.4, addamount$)
                call "CONVERT" (totalunits-newunits, 2.4, addunits$)
                print using L13480, "      ****** GRAND TOTALS ******",   ~
                       "   *PRIOR INPUT", addunits$, " ", addamount$, " "
                call "CONVERT" (newearned, 2.4, addamount$)
                call "CONVERT" (newunits, 2.4, addunits$)
                print using L13480, " ", "THIS POSTING", addunits$, " ",  ~
                                                          addamount$, " "
                call "CONVERT" (totaldollars, 2.4, addamount$)
                call "CONVERT" (totalunits, 2.4, addunits$)
                print using L13480, " ", "   REPORT TOTAL", addunits$,    ~
                                                     " ", addamount$, " "
                print using L13390

L41000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE EARNINGS RECAP BY DEPARTMENT.                  *~
            *************************************************************

            if eptr% = 0  then L45000
            pageline% = 100
            startpage% = pagenumber%
            call "SORT" addr(str(estack$(), 1), 300%, 20%,               ~
                             str(estack$(), 1), 1%, 16%, "A", "S")
            gosub L41220
            print$(1) = "****"
            print$(2) = "***TOTALS***"
            print$(3), print$(4) = " "
            call "CONVERT" (tot1, 2.4, str(print$(3), 4, 10))
            call "CONVERT" (tot2, 2.2, str(print$(4), 4, 10))
            print using L42120, print$(1), print$(2), print$(3),          ~
                               print$(4)
            print using L42110
            goto L45000

L41220:     colsdone% = 0
            mat rcpline% = con
            go1%, e1, e2, tot1, tot2 = 0

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L41270:         for column% = 1 to 1
                    on column% gosub L41390
                    next column%
                if colsdone% < 1 then L41340
                   REM EXIT ROUTINE FOR FINISHED.
                       return

L41340:         gosub L41840              /* Page heading, if neccessary*/
                print using L42120, print$(1), print$(2), print$(3),      ~
                                   print$(4)
                goto L41270

L41390:     REM FIRST  COLUMN--EARNINGS STACK
                on rcpline%(1) gosub L41420, L41570, L41690, L41780
                   return
L41420:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    go1% = go1% + 1
                    if go1% > eptr% then L41570
                    if go1% = 1 then prevdept1$ = str(estack$(go1%), 1, 4)
                    if prevdept1$ <> str(estack$(go1%), 1, 4) then L41570
                    print$(1) = str(estack$(go1%), 1, 4)
                    print$(2) = str(estack$(go1%), 5, 12)
                    convert str(estack$(go1%), 17, 4) to t%
                    print$(3), print$(4) = " "
                    call "CONVERT" (estak1(t%),2.2, str(print$(3),4,10))
                    call "CONVERT" (estak2(t%),2.2, str(print$(4),4,10))
                    e1 = e1 + estak1(t%)
                    e2 = e2 + estak2(t%)
                    rcpline%(1) = 1
                    return
L41570:         REM HANDLES SECOND CASE--DEPARTMENT TOTAL
                    print$(1) = prevdept1$
                    print$(2) = "***TOTAL***"
                    print$(3), print$(4) = " "
                    call "CONVERT" (e1, 2.4, str(print$(3),4,10))
                    call "CONVERT" (e2, 2.2, str(print$(4),4,10))
                    prevdept1$ = str(estack$(go1%), 1, 4)
                    tot1 = tot1 + e1
                    tot2 = tot2 + e2
                    e1, e2 = 0
                    rcpline%(1) = 3
                    return
L41690:         REM HANDLES THIRD CASE--TAG LINE
                    print$(1) = "----"
                    print$(2) = "------------"
                    print$(3) = "-----------------"
                    print$(4) = "----------------"
                    if go1% > eptr% then rcpline%(1) = 4                 ~
                                    else rcpline%(1) = 1
                    go1% = go1% - 1
                    return
L41780:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(1), print$(2), print$(3), print$(4) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 5
                    return

L41840:     REM PAGE CONTROL SUBROUTINE
                select printer(134)
                pageline% = pageline% + 1
                if pageline% < 60 then return
                   call "DATE" addr ("HD", hdrdate$)
                   if pagenumber% = startpage% then L41910
                         print using L42110
L41910:            print page
                   pagenumber% = pagenumber% + 1
                   print using L42030, pagenumber%, hdrdate$
                   print
                   print using L42070
                   print using L42080
                   print using L42090
                   print using L42100
                   print using L42110
                   pageline% = 6
                   return

L42030: %PAGE #####             E A R N I N G S   B Y   D E P A R T M E N~
        ~ T                     ##########################################~
        ~###

L42070: %   +----------------------------------------------------+
L42080: %   !                  E A R N I N G S                   !
L42090: %   !----+------------+-----------------+----------------!
L42100: %   !DEPT! TYPE       !      UNITS      !     DOLLARS    !
L42110: %   +----+------------+-----------------+----------------!
L42120: %   !####!############!#################!################!

L45000: REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.             WILL NOT PRINT ANYTHING      *~
            *   WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY        *~
            *************************************************************

            if debitsptr% = 0 and creditsptr% = 0 then L50000
               text$ = "SUMMARY"
               str(text$,31) = "SUMMARY"
               str(text$,69) = "PAYROLL EARNINGS"
               call "SORT" addr(str(debitsstk$(), 1), 300%, 12%)
               call "SORT" addr(str(creditsstk$(), 1), 300%, 12%)
               totaldebits, totalcredits, colsdone% = 0
               mat rcpline% = con
               mat rcpptr%  = zer
               gosub L48000               /* Skip to top of page.       */

L45160:     for column% = 1 to 2
                on column% gosub L45230, L45600
                next column%
                print                    /* Free up line.              */
            if  colsdone% >= 2 then L50000          /* Report Done      */
                goto L45160

L45230:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L45260, L45410, L45450, L45500, L45540
                   return
L45260:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L45450
                    rcpptr%(1)  = rcpptr%(1) + 1
                    convert str(debitsstk$(rcpptr%(1)),10,3) to t%
                    rcpamt(1)   = debitsstk (t%)
                    rcpacct$(1) = str(debitsstk$(rcpptr%(1)),1,9)
                if summary$ <> "Y" then L45320
                    if export_on$ <> "Y" then L45312
                       init (" ")  gl_catg$, gl_depart$, gl_earnings$,   ~
                                   gl_employee$
                       tran_type$ = "EEX02"
                       gl_postamt = rcpamt(1)
                       gosub load_gl_info

L45312:         call "GLPOST2" (rcpacct$(1), rcpamt(1), 0, prldate$,     ~
                               0%, "09", text$, jnlid$,                  ~
                               pstseq%, userid$, #2, #5, #1,             ~
                               err%, " ", gl_post_info$())
L45320:             call "DESCRIBE" (#2, rcpacct$(1), rcpacctdescr$(1),  ~
                                                   0%, f1%(2))
                    call "GLFMT" (rcpacct$(1))
                    print using L49020, rcpacct$(1), rcpacctdescr$(1),    ~
                                         rcpamt(1);
                    totaldebits = totaldebits + debitsstk(t%)
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
L45410:         REM PRINTS SEPARATOR LINE.
                    print using L49010, "*--";
                    rcpline%(1) = 3
                    return
L45450:         REM PRINTS TOTAL LINE
                    totaldebits = round(totaldebits, 2%)
                    print using L49030, totaldebits;
                    rcpline%(1) = 4
                    return
L45500:         REM PRINTS STARS
                    print using L49000, "*";
                    rcpline%(1) = 5
                    return
L45540:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L45600:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(70);
                on rcpline%(2) gosub L45640, L45790, L45830, L45880, L45920
                   return
L45640:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L45830
                    rcpptr%(2)  = rcpptr%(2) + 1
                    convert str(creditsstk$(rcpptr%(2)),10,3) to t%
                    rcpamt(2)   = creditsstk (t%)
                    rcpacct$(2) = str(creditsstk$(rcpptr%(2)),1,9)
                if summary$ <> "Y" then L45700
                    if export_on$ <> "Y" then L45694
                       init (" ")  gl_catg$, gl_depart$, gl_earnings$,   ~
                                   gl_employee$
                       tran_type$ = "EEX01"
                       gl_postamt = -rcpamt(2)
                       gosub load_gl_info

L45694:         call "GLPOST2" (rcpacct$(2), 0, rcpamt(2), prldate$,     ~
                               0%, "09", text$, jnlid$,                  ~
                               pstseq%, userid$, #2, #5, #1,             ~
                               err%, " ", gl_post_info$())
L45700:             call "DESCRIBE" (#2, rcpacct$(2), rcpacctdescr$(2),  ~
                                             0%, f1%(2))
                    call "GLFMT" (rcpacct$(2))
                    print using L49020, rcpacct$(2),rcpacctdescr$(2),     ~
                               rcpamt(2);
                    totalcredits = totalcredits + creditsstk(t%)
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L45790:         REM PRINT SEPARATOR LINE
                    print using L49010, "*--";
                    rcpline%(2) = 3
                    return
L45830:         REM PRINT TOTAL CREDITS LINE
                    totalcredits = round(totalcredits, 2%)
                    print using L49040, totalcredits;
                    rcpline%(2) = 4
                    return
L45880:         REM PRINT STARS
                    print using L49000,"*";
                    rcpline%(2) = 5
                    return
L45920:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L48000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                select printer (134)
                   call "DATE" addr ("HD", hdrdate$)
                   print page
                   print using L49060, pagenumber%+1, title$, hdrdate$
                   print
                   print using L49100
                   print
                   print using L49140
                   print using L49180,"#","#"
                   print using L49220
                   return

L49000: %**************************#***********************************
L49010: %#--------------+--------------------------------+------------*
L49020: %* ############ ! ############################## !-#######.## *
L49030: %*              ! TOTAL DEBITS                   !-#######.## *
L49040: %*              ! TOTAL CREDITS                  !-#######.## *

L49060: %PAGE #####             #########################################~
        ~###################    ##########################################~
        ~###

L49100: %=========================D E B I T S==========================  ~
        ~     ===========================C R E D I T S====================~
        ~==

L49140: %**************************************************************  ~
        ~     ************************************************************~
        ~**

L49180: %* ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT   *  ~
        ~     * ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT  ~
        ~ *

L49220: %*--------------+--------------------------------+------------*  ~
        ~     *--------------+--------------------------------+-----------~
        ~-*

L50000: REM *************************************************************~
            *                S A V E   R E S U L T S                    *~
            *                                                           *~
            * LOGS THE FACT THAT INVOLVED PAY FREQUENCIES NOW NEED THE  *~
            * DEDUCTIONS RECALCULATED BEFORE CHECKS CAN BE PRINTED.     *~
            *************************************************************

            mat oldfreqs% = con
            mat oldfreqs% = (48%)*oldfreqs%

            REM Set up new...
            for i% = 1% to 7%
                if freqs%(i%) <> 0% then oldfreqs%(i%) = 48%+i%
            next i%
            put str(tempn$,,7) using L50087, oldfreqs%()
L50087:         FMT 7*BI(1)

L50090:     call "READ101" (#1, "PAYROLLS_IN_PROCESS:", f1%(1))
                if f1%(1) <> 0 then L50110
            write #1, using L50106, "PAYROLLS_IN_PROCESS:", tempn$, " ",  ~
                                   " ", eod goto L50090
L50106:     FMT CH(20), CH(7), CH(250), CH(223)
            goto L65000

L50110:     get #1, using L50120, tempo$
L50120:     FMT POS(21), CH(7)

            REM Net old with new...
            str(tempo$,,7) = str(tempo$,,7) or str(tempn$,,7)

            put #1, using L50120, tempo$
            rewrite #1

            goto L65000

L60000: REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            REM INITIALIZE STACKS
                init(hex(ff)) debitsstk$(), creditsstk$(), estack$()
                mat debitsstk = zer
                mat creditsstk = zer
                mat estak1     = zer
                mat estak2     = zer
                debitsptr%, creditsptr%, eptr% = 0
                return

            deffn'162(account$, amount)  /* Recap debits accumulator   */
                temp% = debitsptr%
                if debitsptr% = 300 then L60230
                  search str(debitsstk$(),1) = account$                  ~
                               to location$ step 12 /* Find account #   */
                  if location$ = hex(0000) then L60270
                     temp% = int(val(location$,2)/12)+1
L60230:              debitsstk(temp%) = debitsstk(temp%) + amount
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     debitsstk(temp%) = round(debitsstk(temp%), 2%)
                     return
L60270:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = account$
                      convert debitsptr% to                              ~
                               str(debitsstk$(debitsptr%),10,3), pic(###)
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* Recap credits accumulator  */
                temp% = creditsptr%
                if creditsptr% = 300 then L60420
                  search str(creditsstk$(),1) = account$                 ~
                               to location$ step 12
                  if location$ = hex(0000) then L60460
                     temp% = int(val(location$,2)/12)+1
L60420:              creditsstk(temp%) = creditsstk(temp%) + amount
                                         /* Update amt for that element*/
                     creditsstk(temp%) = round(creditsstk(temp%), 2%)
                     return
L60460:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = account$
                      convert creditsptr% to                             ~
                         str(creditsstk$(creditsptr%),10,3), pic(###)
                      creditsstk(creditsptr%) = amount
                      return

            deffn'172(dpt$, etype$, hrs, dolls)
                temp% = eptr%
                if eptr% = 300 then L60630
                string$ = dpt$
                str(string$,5) = etype$
                search str(estack$(),1) = str(string$,,16)               ~
                                         to location$ step 20
                if location$ = hex(0000) then L60660
                     temp% = int(val(location$,2)/20)+1
L60630:              estak1(temp%) = estak1(temp%) + hrs
                     estak2(temp%) = estak2(temp%) + dolls
                     return
L60660:         REM PUSH NEW ITEM ONTO STACK.
                      eptr% = eptr% + 1
                      estack$(eptr%) = string$
                      convert eptr% to                                   ~
                               str(estack$(eptr%), 17, 4), pic(####)
                      estak1(eptr%) = hrs
                      estak2(eptr%) = dolls
                      return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "JNLCLOSE" (moduleno$, jnlid$, pstseq%, returncode%)
            call "SHOSTAT" ("One Moment Please")
            end
