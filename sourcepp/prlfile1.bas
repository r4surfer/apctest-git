        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      FFFFF  IIIII  L      EEEEE    1     *~
            *  P   P  R   R  L      F        I    L      E       11     *~
            *  PPPP   RRRR   L      FFFF     I    L      EEEE     1     *~
            *  P      R   R  L      F        I    L      E        1     *~
            *  P      R   R  LLLLL  F      IIIII  LLLLL  EEEEE  11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLFILE1 - POSTS JOB EARNINGS BUFFER FOR THIS EMPLOYEE TO *~
            *            THE EARNINGS RECORD FILE, THE LABOR CLASS FILE,*~
            *            AND THE JOB COST FILE.                         *~
            *----------------------------------------------------------Q*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/02/81 ! ORIGINAL                                 ! TEM *~
            * 08/03/81 ! ROUNDING ON EARNINGS DOLLARS             ! TEM *~
            * 08/19/81 ! POSTING TO GENERAL LEDGER. EARNINGS RECAP! TEM *~
            * 10/26/81 ! ADJUSTMENT FOR OVERTIME LABOR POSTING    ! TEM *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION   ! RAC *~
            * 12/09/85 ! CHANGE CALL FROM WHICHMON TO WHICHPER    ! HES *~
            * 12/16/85 ! CHANGE CALL TO GLPOST & JBVLPOST         ! HES *~
            * 04/22/87 ! Get Labor Class Overhead % from STCLABOR ! JIM *~
            * 06/17/87 ! Final part of Std Costing Changes        ! ERN *~
            * 12/26/90 ! Changed Call to JNLINFO for GL Batches,  ! RAC *~
            *          !    added Call to JNLCLOSE for GL Batches !     *~
            * 06/21/91 ! Added code for G/L Export file.          ! JBK *~
            *          ! Added CALL to ALLFREE                    !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 04/13/93 ! Add Channel #17 for JBMASTRC file.       ! JBK *~
            *          !  Modified call to JBVLPOST for #17, all  !     *~
            *          !  part of Core Value Project.             !     *~
            *************************************************************

        dim                                                              ~
            activity$4,                  /* Activity Code              */~
            costs(12),                   /* Cost Arrays for JBVLPOST   */~
            expacct$9,                   /* GL ACCOUNT FOR LABOR       */~
            crdacct$9,                   /* CREDIT ACCOUNT IN STACK    */~
            creditsstk$(200)9,           /* CREDIT ACCOUNT STACK       */~
            creditsstk(200),             /* CREDIT ACCOUNT STACK       */~
            date$8,                      /* SCREEN FORMATTED DATE      */~
            dbtacct$9,                   /* DEBIT ACCOUNT IN STACK     */~
            debitsstk$(200)9,            /* DEBIT ACCOUNT STACK        */~
            debitsstk(200),              /* DEBIT AMOUNT STACK         */~
            depart$4,                    /* Employee Department Code   */~
            catg$4,                      /* DEPARTMENT CODE            */~
            earntype$12,                 /* EARNINGS TYPE DESCRIPTION  */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            errormsg$79,                 /* STFOPEN error message      */~
            estack$(500)20,              /* DEPARTMENT RECAP STACK     */~
            estak1(500),                 /* UNITS TOTAL                */~
            estak2(500),                 /* DOLLARS TOTAL              */~
            gltext$100,                  /* GENERAL LEDGER TEXT        */~
            hdrdate$45,                  /* FORMATTED DATE FOR HEADER  */~
            jnlid$3,                     /* JOUNRAL ID                 */~
            job$8,                       /* JOB NUMBER                 */~
            linenumber%(2),              /* PRINT ROUTINE CONTROLLER   */~
            location$2,                  /* LOCATOR IN STACK SEARCHES  */~
            moduleno$20,                 /* MODULE NUMBER              */~
            name$(3)20,                  /* EMPLOYEE'S NAME            */~
            name1$25,                    /* EMPLOYEE NAME FOR GLDETAIL */~
            oldempcode$12,               /* OLD EMPLOYEE'S CODE        */~
            plowkey1$50,                 /* INITIAL PLOW KEY           */~
            plowkey2$50,                 /* SECONDARY PLOW KEY         */~
            print$(10)20,                /* PRINT VARIABLES            */~
            prtvar$60,                   /* NAME PRINT VARIABLE        */~
            prldate$8,                   /* USER'S PAYROLL DATE        */~
            readkey$50,  readkey2$100,   /* FILE READ KEY              */~
            record$(50)20,               /* ENTIRE RECORD HOLDER       */~
            record2$(50)10,              /* ANOTHER ENTRIE RECORD HOLD */~
            rcpacct$(2)16,               /* RECAP ACCOUNTS             */~
            rcpacctdescr$(2)30,          /* RECAP ACCOUNT DESCR'S      */~
            rcpamt(2),                   /* RECAP AMOUNTS              */~
            rcpline%(2),                 /* RECAP LINE NUMBERS         */~
            rcpptr%(2),                  /* POINTER INTO STACKS        */~
            set$8, setid$4,              /* Current Cost Set IDs       */~
            summary$1,                   /* SUMMARY POST INDICATOR     */~
            title$60,                    /* JOURNAL TITLE              */~
            unitdescr$6,                 /* EARNING UNIT DESCRIPTION   */~
            userid$3,                    /* USER ID CODE               */~
            wdate$8,                     /* DATE WORK DONE -UNFORMATTED*/~
            wfdate$8,                    /* DATE WORK DONE - FORMATTED */~
            lclass$4,                    /* LABOR CLASS CODE           */~
            wunit$7,                     /* UNITS WORKED               */~
            wcenter$4                    /* WORK CENTER CODE           */~

        dim                              /* G/L Export Posting info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            gl_activity$4,               /* G/L Export Activity Code   */~
            gl_catg$4,                   /* G/L P/R Earnings Catagory  */~
            gl_depart$4,                 /* G/L P/R Employee Depart #  */~
            gl_earnings$12,              /* G/L P/R Earnings Descr.    */~
            gl_employee$12,              /* G/L P/R Employee Number    */~
            gl_lclass$4,                 /* G/L P/R Labor Class        */~
            gl_project$8,                /* G/L P/R Project Number     */~
            gl_wcenter$4,                /* G/L P/R Work Center        */~
            tran_type$5                  /* G/L transaction type       */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "OPENCHCK"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE  F2%()            SHOULD NOT BE   */
                     /* MODIFIED.     IT  IS AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * # 2 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 3 ! EMPEARN1 ! EMPLOYEE EARNINGS FILE                   *~
            * # 5 ! JOBMASTR ! JOB MASTER FILE                          *~
            * # 6 ! USERINFO ! USER INFORMATION FILE                    *~
            * # 7 ! JOBLABOR ! DIRECT JOB LABOR DETAIL FILE             *~
            * # 9 ! PRLBUFFR ! EMPLOYEE JOB EARNINGS BUFFER             *~
            * #10 ! JBMASTR2 ! VERSION 2 JOB MASTER                     *~
            * #11 ! JBVALUE2 ! VERSION 2 JOB VALUE FILE                 *~
            * #14 ! PERMASTR ! PERSONNEL EMPLOYEE MASTER FILE           *~
            * #15 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #16 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            * #17 ! JBMASTRC ! Production job Core Value Added File     *~
            * #30 ! STCnnnnL ! Std Costing Labor Standards (STCLABOR)   *~
            *-----+----------+------------------------------------------*~
            * where 'nnnn' is a unique cost set identifier generated by *~
            * the call to 'STCFOPEN'.                                   *~
            *************************************************************

            select #1, "SYSFILE2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1, keylen = 20

             select #2, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #3, "EMPEARN1",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 200,                                    ~
                       keypos = 1, keylen = 15,                          ~
                       alt key 1, keypos = 16, keylen = 28

            select #5, "JOBMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 700,                                    ~
                       keypos = 1, keylen = 8

            select #6, "USERINFO",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 150,                                    ~
                       keypos = 1, keylen = 3

            select #7, "JOBLABOR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 200,                                    ~
                       keypos = 1, keylen = 16

            select #9, "PRLBUFFR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 150,                                    ~
                       keypos = 1, keylen = 18

            select #10, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos = 1, keylen = 8

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #11, "JBVALUE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 23

            select #15, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #16, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select #17, "JBMASTRC",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  600,                                 ~
                         keypos = 1, keylen = 8

            select #30, "STCLABOR", varc, indexed, recsize = 323,        ~
                        keypos = 1, keylen = 4


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

        call "SHOSTAT" ("Preparing To Post Job Earnings")
            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (# 6, 0%, f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (# 7, 0%, f2%( 7), 300%, rslt$( 7))
            call "OPENCHCK" (# 9, 0%, f2%( 9),   0%, rslt$( 9))
            call "OPENCHCK" (#10, 0%, f2%(10),   0%, rslt$(10))
            call "OPENCHCK" (#11, 0%, f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#14, 0%, f2%(14),   0%, rslt$(14))
            call "OPENCHCK" (#15, 0%, f2%(15),   0%, rslt$(15))
            call "OPENCHCK" (#16, 0%, f2%(16),   0%, rslt$(16))

            call "STCFOPEN" /* Open STCLABOR in 'Shared' mode          */~
                (" ", "xxxSxx", #1, errormsg$, #30,#30,#30,#30,#30,#30)


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * INITIALIZE VARIABLES NEEDED FOR SCREEN I/O, EDIT MODE,    *~
            * AND SYSTEM DATES.                                         *~
            *************************************************************

        REM CHECK PAYROLL DATE
            call "EXTRACT" addr ("ID", userid$)
            call "READ100" (#6, userid$, f1%(6))
                 if f1%(6) = 0 then L65000
                 get #6, using L09140, prldate$
L09140:                  FMT XX(15), CH(6)
                 call "WHICHPER" (#1, prldate$, thisper%)
                 if thisper% = 0 then L65000

            date$ = prldate$
            call "DATEFMT" (date$)
            call "DATE" addr("HD", hdrdate$)

            pageline% = 1000

            REM INITIALIZE STACKS
                     gosub L50000

L09300:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "PRFIL1",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09300

                returncode% = 0
                moduleno$ = "09"
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                title$, prldate$, #1, f2%(1), returncode%)
                call "FMTTITLE" (title$, "RECAP", 12%)

*        Get defaults for Job distributions
            dflt_dlabr% = 2%  :  dflt_ohead% = 3%
            buckets% = 1%
            call "STCSETID" (buckets%, #1, set$, setid$)
            if buckets% = 0% then L09600
                readkey2$ = "STC.HDR." & set$
                call "READ100" (#1, readkey2$, f1%(1))
                if f1%(1) = 0% then L09600
                     get #1 using L09540, dflt_dlabr%, dflt_ohead%
L09540:                   FMT POS(443), 2*BI(1)

L09600
*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#1, "SWITCHS.GL", f1%(1))
            if f1%(1) = 1% then get #1 using L09640, export_on$
L09640:         FMT POS(22), CH(1)

        REM *************************************************************~
            *    M A I N   P R O G R A M                                *~
            * --------------------------------------------------------- *~
            * Controlling section.  Plows through buffer calling        *~
            * appropiate subroutines.                                   *~
            *************************************************************

        call "SHOSTAT" ("Posting Job Earnings...")

L10090
*       ** Plow on USERID, Picking up employee code

            call "ALLFREE"

            plowkey1$ = userid$
            call "PLOWNEXT" (#9, plowkey1$, 3%, f1%(9))
            if f1%(9) = 0% then L11030

            get #9 using L10150, empcode$
L10150:         FMT XX(3), CH(12)

*       * Read and get MASTER RECORD
            call "READ101" (#2, empcode$, f1%(2))
            if f1%(2) = 0% then L10090      /* UNLIKELY, BUT... */

            gosub L34000   /* Get overhead accrual acct */

            plowkey2$ = userid$  /* Key for this Employee's Earnings   */
            str(plowkey2$, 4) = empcode$

*        Plow through picking up Employee's Earnings
L10270:     call "PLOWNEXT" (#9, plowkey2$, 15%, f1%(9))
            if f1%(9) = 0% then L10990

            gosub L30000    /* Get earning's data from buffer  */

            gosub L31000    /* Update earnings record          */

            gosub L33000    /* Retrieve Labor Class Overhead % */

            overdollars = round(dollars * (overhead/100), 2)

          /* Post to Project -or- Job                                  */
            call "READ100" (#14, empcode$, f1%(14))
            get #14 using L10410, name1$
L10410:         FMT XX(1), CH(25)
            if job$ = " " or str(job$,,1) = hex(00) then L10580

                call "JLBRPOST" (job$, empcode$, wdate$, wcenter$,       ~
                 /* Project */   earntype$, unitdescr$, rate, worked,    ~
                                 dollars, overdollars, #5, #7, f2%(5),   ~
                                 f2%(7), u3%)

                mat costs = zer
                costs(dlabr_bkt%) = dollars
                costs(ohead_bkt%) = overdollars
                call "JBVLPOST" (#10, #11, #17, u3%, job$, 1%, wdate$,   ~
                                 prldate$, userid$, name1$, costs(),     ~
                                 rate, worked, wcenter$, activity$,      ~
                                 empcode$, earntype$, lclass$,           ~
                                 unitdescr$)

L10580:   /* Post Labor Distribution to G/L Expense Account  */
            gltext$ =  job$
            if job$ = " " or str(job$,,1) = hex(00) then gltext$ = catg$
            str(gltext$,10) = earntype$
            str(gltext$,31) =  empcode$
            str(gltext$,69) =  "EMPLOYEE : "& name1$
            if summary$ = "Y" then L10680

                if export_on$ <> "Y" then L10650
                     tran_type$ = "EEJ01"
                     if job$ = " " or str(job$,,1) = hex(00) then        ~
                          tran_type$ = "EEJ02"
                     gl_activity$ = activity$
                     gl_catg$     = catg$
                     gl_depart$   = depart$
                     gl_earnings$ = earntype$
                     gl_employee$ = empcode$
                     gl_lclass$   = lclass$
                     gl_project$  = job$
                     gl_wcenter$  = wcenter$
                     gl_postamt   = dollars
                     if str(job$,,1) = hex(00) then gl_project$ = " "
                     gosub load_gl_info

L10650:            call "GLPOST2" (expacct$, dollars, 0, prldate$,       ~
                                  0%, "09", gltext$, jnlid$,             ~
                                  pstseq%, userid$, #15, #16, #1, u3%,   ~
                                  " ", gl_post_info$())
L10680:     gosub'162 (expacct$, dollars)

          /* Then credit the Gross Payroll Accrual (holding) Acct      */
            if summary$ = "Y" then L10750
                if export_on$ <> "Y" then L10720
                     tran_type$ = "EEJ03"
                     gl_postamt   = -dollars
                     gosub load_gl_info
L10720:            call "GLPOST2" (grossacct$, 0, dollars, prldate$,     ~
                                  0%, "09", gltext$, jnlid$,             ~
                                  pstseq%, userid$, #15, #16, #1, u3%,   ~
                                  " ", gl_post_info$())
L10750:     gosub'163 (grossacct$, dollars)

          /* Post Overhead to Expense Account...                       */
            str(gltext$,69) =  "OVERHEAD : "& name1$
            if summary$ = "Y" then L10830
                if export_on$ <> "Y" then L10800
                     tran_type$ = "EEJ04"
                     gl_postamt   = overdollars
                     gosub load_gl_info
L10800:            call "GLPOST2" (expacct$, overdollars, 0, prldate$,   ~
                                  0%, "09", gltext$, jnlid$,             ~
                                  pstseq%, userid$, #15, #16, #1, u3%,   ~
                                  " ", gl_post_info$())
L10830:     gosub'162 (expacct$, overdollars)

          /* Then post the other side of Overhead...         */
            if summary$ = "Y" then L10900
                if export_on$ <> "Y" then L10870
                     tran_type$ = "EEJ05"
                     gl_postamt   = -overdollars
                     gosub load_gl_info
L10870:            call "GLPOST2" (overacct$, 0, overdollars, prldate$,  ~
                                  0%, "09", gltext$, jnlid$,             ~
                                  pstseq%, userid$, #15, #16, #1, u3%,   ~
                                  " ", gl_post_info$())
L10900:     gosub'163 (overacct$, overdollars)

          /* PRINT entry, DELETE entry, and RETURN for next  */
            gosub L20000
            plowkey1$ = key(#9)
            call "READ101" (#9, plowkey1$, f1%(9))
            if f1%(9) <> 0% then delete #9
            goto L10270

L10990
*        Done with this employee. Update master record
            gosub L32000
            goto L10090

L11030
*       * Done with this User ID.
            if pageline% = 1000 then L65000
            init(hex(ff)) empcode$
            gosub L20000
            totaldollars = round(totaldollars, 2%)
            call "CONVERT" (totalunits, 2.2, wunit$)
            print using L61700, "*****TOTALS*****", " ", " ", " ",        ~
                               " ", wunit$, totaldollars, " ", totalover
            print using L61300

        REM *************************************************************~
            *    P R I N T   D E P A R T M E N T   R E C A P            *~
            *                                                           *~
            *************************************************************

            if eptr% = 0  then L11320
            line% = 1000
            page% = 0
            call "SORT" addr(str(estack$(), 1), 500% , 20%,              ~
                             str(estack$(), 1), 1%, 16%, "A", "S")
            gosub L44000
            print$(1) = "****"
            print$(2) = "***TOTALS***"
            print$(3), print$(4) = " "
            call "CONVERT" (tot1, 2.4, str(print$(3), 4,10))
            call "CONVERT" (tot2, 2.2, str(print$(4), 4,10))
            print using L45530, print$(1), print$(2), print$(3),          ~
                               print$(4)
            print using L45380
L11320:     goto L40000

        load_gl_info

            put str(gl_post_info$(),,) using L12490,                      ~
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
                gl_project$,             /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                gl_wcenter$,             /* Work Center CH(4)          */~
                gl_activity$,            /* Activity code CH(4)        */~
                gl_employee$,            /* Employee number CH(12)     */~
                gl_depart$,              /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                gl_earnings$,            /* Earnings Type CH(12)       */~
                " ",                     /* Deduction Type CH(12)      */~
                gl_catg$,                /* P/R Category CH(4)         */~
                gl_lclass$,              /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L12490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
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

L20000: REM *************************************************************~
            *   P R I N T   A   J O B   E A R N I N G   E N T R Y       *~
            *                                                           *~
            * HANDLES THE PRINTING OF A JOB EARNINGS ENTRY              *~
            *************************************************************

            REM IF SAME EMPLOYEE, BLANK OUT NAME VARIABLE
                if empcode$ <> oldempcode$ then L20110
                   prtvar$ = " "
                   go to L20310

L20110:     REM NEW EMPLOYEE.  PRINT TOTAL AND TAG LINE UNLESS FIRST ONE
                if oldempcode$ = " " then L20200
                gosub L60000
                earned = round(earned, 2%)
                call "CONVERT" (units, 2.2, str(temp$,,7))
                print using L61700, " ", "TOTAL EARNED", " ", " ", " ",   ~
                                   temp$, earned, " ", over
                totalunits = totalunits + units
                totaldollars = totaldollars + earned
                totalover    = totalover    + over
                gosub L60000
                if pageline% = 7 then L20200
                print using L61600

L20200:     REM SET EMPLOYEE CODE, NAME AND ZERO EARNINGS
                call "READ100" (#2, empcode$, f1%(2))
                     if f1%(2) = 0 then return     /* FAKE OUT         */
                     get #2, using L20240, depart$
L20240:              FMT XX(91), CH(4)
                call "READ100" (#14, empcode$, f1%(14))
                     if f1%(14) = 0 then return     /* FAKE OUT         */
                     get #14, using L20248, name$(3), name$(1), name$(2)
L20248:              FMT XX(1), CH(15), CH(10), CH(1)
                prtvar$ = "(" & empcode$ & ") " & name$(3) & ", "        ~
                                                & name$(1) & " "         ~
                                                & name$(2)
                earned, units, over = 0
                oldempcode$ = empcode$

L20310:     REM PRINT ENTRY, TOTAL AND RETURN
                gosub L60000
                temp$ = expacct$
                call "GLFMT" (temp$)
                print using L61700, prtvar$, earntype$, job$, lclass$,    ~
                          wfdate$, wunit$, dollars, temp$, overdollars
                gosub'172(depart$, earntype$, worked, dollars)
                earned = earned + dollars
                units = units + worked
                over  = over  + overdollars
                earned = round(earned, 2%)
                return

L30000: REM *************************************************************~
            *    L O A D   I N F O   F R O M   P R L B U F F R          *~
            *                                                           *~
            *************************************************************

            get  #9, using L30150, empcode$, seqnr$, earntype$, job$,     ~
                            lclass$, wdate$, worked, expacct$, wcenter$, ~
                            activity$
            wfdate$ = wdate$
            call "DATEFMT" (wfdate$)
            call "CONVERT" (worked, 2.2, wunit$)
            convert seqnr$ to temp%
            convert temp% to seqnr$, pic(###)
            return
L30150:              FMT       XX(3),              /* USERID           */~
                               CH(12),             /* EMPLOYEE CODE    */~
                               XX(3),              /* SKIP SEQUENCE    */~
                               CH(3),              /* EARNING TYPE NO. */~
                               CH(12),             /* EARNINGS DESC    */~
                               CH(8),              /* JOB NUMBER       */~
                               CH(4),              /* LABOR CLASS CODE */~
                               CH(6),              /* DATE WORK DONE   */~
                               PD(14,4),           /* UNITS WORKED     */~
                               CH(9),              /* ACCOUNT          */~
                               CH(4),              /* WORKCENTER       */~
                               CH(4)               /* Activity Code    */

L31000: REM *************************************************************~
            *   U P D A T E    E A R N I N G S   R E C O R D            *~
            *                                                           *~
            * READS AND UPDATES AN EARNINGS RECORD.                     *~
            *************************************************************

            REM INITIALIZE VARIABLES
                rate, dollars = 0
                unitdescr$ = " "

            REM CREATE READ KEY FOR EARNINGS RECORD FILE
                readkey$ = empcode$
                str(readkey$, 13) = seqnr$

            REM READ ON ALTERNATE KEY IN HOLD MODE
                call "READ101" (#3, readkey$, f1%(3))
                     if f1%(3) = 0 then return
                gosub L35000  /* Get from buffer */
                dollars = round(rate * worked, 2)

                      currunits  = currunits   + worked
                      curramount = curramount  + dollars

            REM RESAVE BUFFER UNITS
                gosub L36000
                return

L32000: REM *************************************************************~
            *   U P D A T E   E M P L O Y E E   M A S T E R             *~
            *                                                           *~
            * SETS DEDUCTION FLAG ON MASTER RECORD                      *~
            *************************************************************

            REM READ AND GET MASTER RECORD
                call "READ101" (#2, empcode$, f1%(2))
                     if f1%(2) = 0 then return
                get #2, using L32100, str(record$(), 1)
L32100:                 FMT CH(136)

            REM RESET FLAG
                str(record$(), 70, 1) = "Y"

            REM RESAVE MASTER RECORD
                put #2, using L32170, str(record$(), 1)
L32170:                 FMT CH(136)
                rewrite #2

            REM AND RETURN
                return

L33000: REM *************************************************************~
            *   R E T R I E V E   W O R K S T A T I O N   OVERHEAD  %   *~
            *                                                           *~
            * RETRIEVE LABOR CLASS OVERHEAD PERCENTAGE.                 *~
            *************************************************************

            overhead   = 0
            dlabr_bkt% = dflt_dlabr%
            ohead_bkt% = dflt_ohead%

            call "READ100" (#30, str(lclass$), f1%(30))
            if f1%(30) = 0% then return
                get #30 using L33130, dlabr_bkt%, ohead_bkt%, overhead
L33130:              FMT POS(59), 2*BI(1), PD(14,4)
                return


L34000: REM *************************************************************~
            *           L O A D   E M P L O Y E E   R E C O R D         *~
            *                                                           *~
            * LOADS THAT WHICH WE NEED TO SEE FROM THE EMPLOYEE MASTER  *~
            * RECORD.  THE FIELDS THAT WE ARE IN PARTICULAR CONCERNED   *~
            * WITH ARE THE EMPLOYEE CODE,       GROSS PAYROLL ACCOUNT,  *~
            * AND WHETHER OR NOT EMPLOYEE SHOULD BE INCLUDED THIS RUN.  *~
            *************************************************************

            get   # 2, using L34100, str(record$(), 1)
L34100:                FMT CH(136)
            get str(record$(), 1), using L34130, empcode$, grossacct$,    ~
                                   depart$, overacct$
L34130:        FMT CH(12), XX(33), CH(9), XX(15), XX(22), CH(4), CH(9)
            return

L35000: REM *************************************************************~
            *           L O A D   E A R N I N G S   R E C O R D         *~
            *                                                           *~
            * LOADS THE EARNINGS RECORDS FOR THE EMPLOYEE.              *~
            *************************************************************

            get   # 3, using L35200, str(record$(), 1), currunits,        ~
                       curramount, mtdunits, mtdamount, qtdunits,        ~
                       qtdamount, ytdunits, ytdamount,                   ~
                       str(record2$(), 1)

            get str(record$(),40), using L35106, catg$, rate
L35106:     FMT CH(4), XX(8), PD(14,4)
            return

L35200:     FMT CH(84),                  /*                            */~
                XX(8)   ,                /* BUFFER ADD      UNITS      */~
                XX(8)   ,                /* BUFFER ADD      AMOUNT     */~
                PD(14,4),                /* CURRENT CHECK   UNITS      */~
                PD(14,4),                /* CURRENT CHECK   AMOUNT     */~
                PD(14,4),                /* MONTH   TO DATE UNITS      */~
                PD(14,4),                /* MONTH   TO DATE AMOUNT     */~
                PD(14,4),                /* QUARTER TO DATE UNITS      */~
                PD(14,4),                /* QUARTER TO DATE AMOUNT     */~
                PD(14,4),                /* YEAR    TO DATE UNITS      */~
                PD(14,4),                /* YEAR    TO DATE AMOUNT     */~
                CH(36)                   /* FILLER FOR REST OF RECORD  */~

L36000: REM *************************************************************~
            *     R E W R I T E   E A R N I N G S   R E C O R D         *~
            *                                                           *~
            *************************************************************

            rewrite #3, using L36140, str(record$(), 1), 0, 0, currunits, ~
                        curramount, mtdunits, mtdamount, qtdunits,       ~
                        qtdamount, ytdunits, ytdamount,                  ~
                        str(record2$(),,36)
            return

L36140:     FMT CH(84),                  /* BEGINNING OF RECORD TO...  */~
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
                CH(36)                   /* FILLER FOR REST            */~

L40000: REM *************************************************************~
            *         P R I N T   D A I L Y   R E C A P   I N F O       *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.                           WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

            if debitsptr% = 0 and creditsptr% = 0 then L65000
               gltext$ = "SUMMARY"
               str(gltext$,31) = "SUMMARY"
               str(gltext$,69) = "PAYROLL EARNINGS JOB POST"
               totaldebits, totalcredits, colsdone% = 0
               mat rcpline% = con
               mat rcpptr%  = zer
               gosub L41500               /* SKIP TO TOP OF PAGE.       */

L40220:     for column% = 1 to 2
                on column% gosub L40290, L40640
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then L65000          /* DONE W/ REPORT   */
                goto L40220

L40290:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L40320, L40450, L40490, L40540, L40580
                   return
L40320:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L40490
                    rcpptr%(1)  = rcpptr%(1) + 1
                    rcpamt(1)   = debitsstk (rcpptr%(1))
                    rcpacct$(1) = debitsstk$(rcpptr%(1))
                   if summary$ <> "Y" then L40370
                      if export_on$ <> "Y" then L40362
                          tran_type$ = "EEJ01"
                          init (" ")  gl_activity$, gl_catg$,            ~
                                      gl_depart$, gl_earnings$,          ~
                                      gl_employee$, gl_lclass$,          ~
                                      gl_project$, gl_wcenter$
                          postamt = rcpamt(1)
                          gosub load_gl_info
L40362:            call "GLPOST2" (rcpacct$(1), rcpamt(1), 0, prldate$,  ~
                                  0%, "09", gltext$, jnlid$,             ~
                                  pstseq%, userid$, #15, #16, #1, u3%,   ~
                                  " ", gl_post_info$())
L40370:             call "DESCRIBE" (#15, rcpacct$(1), rcpacctdescr$(1), ~
                                                   0%, f1%(15))
                    call "GLFMT" (rcpacct$(1))
                    print using L42020, rcpacct$(1), rcpacctdescr$(1),    ~
                                         rcpamt(1);
                    totaldebits = totaldebits + debitsstk(rcpptr%(1))
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
L40450:         REM PRINTS SEPARATOR LINE.
                    print using L42010, "*--";
                    rcpline%(1) = 3
                    return
L40490:         REM PRINTS TOTAL LINE
                    totaldebits = round(totaldebits, 2%)
                    print using L42030, totaldebits;
                    rcpline%(1) = 4
                    return
L40540:         REM PRINTS STARS
                    print using L42000, "*";
                    rcpline%(1) = 5
                    return
L40580:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L40640:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(70);
                on rcpline%(2) gosub L40680, L40810, L40850, L40900, L40940
                   return
L40680:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L40850
                    rcpptr%(2)  = rcpptr%(2) + 1
                    rcpamt(2)   = creditsstk (rcpptr%(2))
                    rcpacct$(2) = creditsstk$(rcpptr%(2))
                   if summary$ <> "Y" then L40730
                      if export_on$ <> "Y" then L40724
                          tran_type$ = "EEJ03"
                          init (" ")  gl_activity$, gl_catg$,            ~
                                      gl_depart$, gl_earnings$,          ~
                                      gl_employee$, gl_lclass$,          ~
                                      gl_project$, gl_wcenter$
                          postamt = rcpamt(2)
                          gosub load_gl_info
L40724:            call "GLPOST2" (rcpacct$(2), 0, rcpamt(2), prldate$,  ~
                                  0%, "09", gltext$, jnlid$,             ~
                                  pstseq%, userid$, #15, #16, #1, u3%,   ~
                                  " ", gl_post_info$())
L40730:             call "DESCRIBE" (#15, rcpacct$(2), rcpacctdescr$(2), ~
                                             0%, f1%(15))
                    call "GLFMT" (rcpacct$(2))
                    print using L42020, rcpacct$(2),rcpacctdescr$(2),     ~
                               rcpamt(2);
                    totalcredits = totalcredits + creditsstk(rcpptr%(2))
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L40810:         REM PRINT SEPARATOR LINE
                    print using L42010, "*--";
                    rcpline%(2) = 3
                    return
L40850:         REM PRINT TOTAL CREDITS LINE
                    totalcredits = round(totalcredits, 2%)
                    print using L42040, totalcredits;
                    rcpline%(2) = 4
                    return
L40900:         REM PRINT STARS
                    print using L42000,"*";
                    rcpline%(2) = 5
                    return
L40940:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L41500:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                select printer (134)
                   call "DATE" addr ("HD", hdrdate$)
                   print page
                   rcppage% = 1
                   print using L42060, rcppage%, title$, hdrdate$
                   print
                   print using L42100
                   print
                   print using L42140
                   print using L42180,"#","#"
                   print using L42220
                   return

L42000: %**************************#***********************************
L42010: %#--------------+--------------------------------+------------*
L42020: %* ############ ! ############################## !-#######.## *
L42030: %*              ! TOTAL DEBITS                   !-#######.## *
L42040: %*              ! TOTAL CREDITS                  !-#######.## *

L42060: %PAGE #####              ########################################~
        ~#####################  ##########################################~
        ~###

L42100: %=========================D E B I T S==========================  ~
        ~     ===========================C R E D I T S====================~
        ~==

L42140: %**************************************************************  ~
        ~     ************************************************************~
        ~**

L42180: %* ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT   *  ~
        ~     * ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT  ~
        ~ *

L42220: %*--------------+--------------------------------+------------*  ~
        ~     *--------------+--------------------------------+-----------~
        ~-*

L44000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE EARNINGS RECAP BY DEPARTMENT.                  *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con
            go1%, e1, e2, tot1, tot2 = 0

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L44110:         for column% = 1 to 1
                    on column% gosub L44240
                    next column%
                if colsdone% < 1 then L44180
                   REM EXIT ROUTINE FOR FINISHED.
                       return

L44180:         gosub L45090              /* PAGE HEADING, IF NECCESSARY*/
                print using L45530, print$(1), print$(2), print$(3),      ~
                                   print$(4)
                goto L44110

L44240:     REM FIRST  COLUMN--EARNINGS STACK
                on linenumber%(1) gosub L44270, L44420, L44540, L44630
                   return
L44270:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    go1% = go1% + 1
                    if go1% > eptr% then L44420
                    if go1% = 1 then prevdept1$ = str(estack$(go1%), 1, 4)
                    if prevdept1$ <> str(estack$(go1%), 1, 4) then L44420
                    print$(1) = str(estack$(go1%), 1, 4)
                    print$(2) = str(estack$(go1%), 5, 12)
                    convert str(estack$(go1%), 17, 4) to t%
                    print$(3), print$(4) = " "
                    call "CONVERT" (estak1(t%),2.2, str(print$(3),4,10))
                    call "CONVERT" (estak2(t%),2.2, str(print$(4),4,10))
                    e1 = e1 + estak1(t%)
                    e2 = e2 + estak2(t%)
                    linenumber%(1) = 1
                    return
L44420:         REM HANDLES SECOND CASE--DEPARTMENT TOTAL
                    print$(1) = prevdept1$
                    print$(2) = "***TOTAL***"
                    print$(3), print$(4) = " "
                    call "CONVERT" (e1, 2.4, str(print$(3),4,10))
                    call "CONVERT" (e2, 2.2, str(print$(4),4,10))
                    prevdept1$ = str(estack$(go1%), 1, 4)
                    tot1 = tot1 + e1
                    tot2 = tot2 + e2
                    e1, e2 = 0
                    linenumber%(1) = 3
                    return
L44540:         REM HANDLES THIRD CASE--TAG LINE
                    print$(1) = "----"
                    print$(2) = "------------"
                    print$(3) = "-----------------"
                    print$(4) = "----------------"
                    if go1% > eptr% then linenumber%(1) = 4              ~
                                    else linenumber%(1) = 1
                    go1% = go1% - 1
                    return
L44630:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(1), print$(2), print$(3), print$(4) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(1) = 5
                    return

L45090:     REM PAGE CONTROL SUBROUTINE
                select printer(134)
                line% = line% + 1
                if line% < 60 then return
                   call "DATE" addr ("HD", hdrdate$)
                   if page% = 0 then L45160
                         print using L45500
L45160:            print page
                   page% = page% + 1
                   print using L45300, page%, hdrdate$
                   print
                   print using L45380
                   print using L45410
                   print using L45440
                   print using L45470
                   print using L45500
                   line% = 6              /* SET STARTING LINE ON PAGE  */
                   return

L45300: %PAGE #####             E A R N I N G S   B Y   D E P A R T M E N~
        ~ T                     ##########################################~
        ~###

L45380: %   +----------------------------------------------------+

L45410: %   !                  E A R N I N G S                   !

L45440: %   !----+------------+-----------------+----------------!

L45470: %   !DEPT! TYPE       !      UNITS      !     DOLLARS    !

L45500: %   +----+------------+-----------------+----------------!

L45530: %   !####!############!#################!################!


L50000: REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            REM INITIALIZE STACKS
                init(hex(ff)) debitsstk$(), creditsstk$(), estack$()
                mat debitsstk  = zer
                mat creditsstk = zer
                mat estak1     = zer
                mat estak2     = zer
                debitsptr%, creditsptr%, eptr% = 0
                return

            deffn'162(dbtacct$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  search str(debitsstk$(),1) = dbtacct$                  ~
                               to location$ step 9
                  if location$ = hex(0000) then L50310
                     temp% = int(val(location$,2)/9)+1
                     debitsstk(temp%) = debitsstk(temp%) + amount
                     debitsstk(temp%) = round(debitsstk(temp%), 2%)
                     return
L50310:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      debitsstk$(debitsptr%) = dbtacct$
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(crdacct$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  search str(creditsstk$(),1) = crdacct$                 ~
                               to location$ step 9
                  if location$ = hex(0000) then L50460
                     temp% = int(val(location$,2)/9)+1
                     creditsstk(temp%) = creditsstk(temp%) + amount
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     creditsstk(temp%) = round(creditsstk(temp%), 2%)
                     return
L50460:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      creditsptr% = creditsptr% + 1
                      creditsstk$(creditsptr%) = crdacct$
                      creditsstk(creditsptr%) = amount
                      return

            deffn'172(dpt$, etype$, hrs, dolls)
                  string$ = dpt$
                  str(string$, 5) = etype$
                  search str(estack$(),1) = str(string$, 1, 16)          ~
                                         to location$ step 20
                  if location$ = hex(0000) then L50770
                     temp% = int(val(location$,2)/20)+1
L50740:              estak1(temp%) = estak1(temp%) + hrs
                     estak2(temp%) = estak2(temp%) + dolls
                     return
L50770:           REM PUSH NEW ITEM ONTO STACK.
                      if eptr% < 500% then L50780
                         temp% = 500%
                         goto L50740
L50780:               eptr% = eptr% + 1
                      estack$(eptr%) = string$
                      convert eptr% to                                   ~
                               str(estack$(eptr%), 17, 4), pic(####)
                      estak1(eptr%) = hrs
                      estak2(eptr%) = dolls
                      return

L60000: REM *************************************************************~
            *    P A G E    C O N T R O L   R O U T I N E               *~
            *                                                           *~
            * CONTROLS WHETHER OR NOT TO PRINT PAGE HEADER              *~
            *************************************************************

            select printer (134)
            REM CHECK PAGE BREAK
                pageline% = pageline% + 1
                if pageline% < 60 then return

            REM IS THIS PAGE ONE?
                if pagenumber% = 0 then L60150
                print using L61300

L60150:     REM NEW PAGE
                print page
                pagenumber% = pagenumber% + 1
                print using L61200, pagenumber%, hdrdate$
                print
                print using L61300
                print using L61400
                print using L61500
                print using L61600

            REM SET PAGE LINE AND RETURN
                pageline% = 7
                return

        REM *************************************************************~
            *        I M A G E     S T A T E M E N T S                  *~
            *                                                           *~
            * IMAGE STATEMENTS FOR EARNINGS AUDIT TRAIL                 *~
            *************************************************************

L61200: % PAGE ########    J O B   E A R N I N G S   I N P U T   A U D I ~
        ~T   T R A I L         ###########################################~
        ~##

L61300: % +----------------------------------------+--------------+------~
        ~--+------+---------+---------+------------+------------+---------~
        ~--+
L61400: % !E M P L O Y E E   I N F O R M A T I O N !   EARNINGS   !  JOB ~
        ~  !LABOR !DATE WORK! UNITS   !  DOLLARS   !   LABOR    !  ACCRUED~
        ~  !
L61500: % !                                        !     TYPE     ! NUMBE~
        ~R !CLASS !PERFORMED! WORKED  !   EARNED   !  ACCOUNT   ! OVERHEAD~
        ~  !
L61600: % !----------------------------------------!--------------!------~
        ~--!------!---------!---------!------------!------------!---------~
        ~--!
L61700: % !########################################! #############!######~
        ~##! #### !######## ! ####### !-###,###.## !############!-###,###.~
        ~##!

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "JNLCLOSE" (moduleno$, jnlid$, pstseq%, returncode%)
            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
