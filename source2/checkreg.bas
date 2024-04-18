        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   CCC   H   H  EEEEE   CCC   K   K  RRRR   EEEEE   GGG    *~
            *  C   C  H   H  E      C   C  K  K   R   R  E      G       *~
            *  C      HHHHH  EEEE   C      KKK    RRRR   EEEE   G GGG   *~
            *  C   C  H   H  E      C   C  K  K   R   R  E      G   G   *~
            *   CCC   H   H  EEEEE   CCC   K   K  R   R  EEEEE   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CHECKREG - DISPLAYS CHECK REGISTER FOR ACCOUNTS PAYABLE   *~
            *            AND PAYROLL.  VARIOUS SELCTION AND SORTING     *~
            *            OPTIONS ARE AVAILABLE AS WELL AS INDIVIDUAL    *~
            *            CHECK DISPLAY AND OUTSTANDING CHECK RECONCIL-  *~
            *            IATION.                                        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/28/82 ! ORIGINAL                                 ! ECR *~
            * 03/29/84 ! Correct paging after detail view,        ! ECR *~
            *          ! Enable PF16 on det.lines; Fix prnt paging!     *~
            *          ! Correct "TOTAL CHECKS RECONCILED TODAY"  !     *~
            * 12/11/85 ! Changed For New Payroll Check File Layout! HES *~
            * 12/16/85 ! Vendor file format changes               ! MJB *~
            * 08/11/86 ! Corrected format/unformat of G/L account ! RAC *~
            * 03/31/87 ! Don't open EMPMASTR if mode=PAY,visaversa! HES *~
            * 08/23/88 ! Fixed truncation of 12 digit acct #'s    ! RJM *~
            *          !   Fixed Fatal call to STRING @ 26731     ! RJM *~
            * 08/29/89 ! Corrected Range Selection on check #     ! MJB *~
            * 08/30/89 ! Corrected PF5 scrolling & Removed PF4    ! MJB *~
            * 04/02/92 ! Minor mods for DEC Compatability.        ! MJB *~
            * 03/30/94 ! PRR 13155. Fixed high end vendor rng test! JDH *~
            * 06/01/94 ! PRR 13175 -Fixed Manual Reconcile to     ! RJH *~
            *          !   Process all 15 lines.                  !     *~
            *          ! PRR 12964 - Correct MODULE is defaulted  !     *~
            *          !   to screen & errormsg if changed.       !     *~
            * 08/01/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        com                                                              ~
            extlen%(15),                 /* EXTERNAL FIELD LENGTHS     */~
            field%(15),                  /* SELECTABLE FIELD LIST      */~
            format$(15)1,                /* DATA TYPE FORMAT CODES     */~
            from$(15)25,                 /* LOW RANGE DATA TEST ITEMS  */~
            fromnr(15),                  /* LOW RANGE NUMERIC TEST ITEM*/~
            length%(15),                 /* INTERNAL FIELD LENGTHS     */~
            position%(15),               /* POSITION IN REC (FROM 1)   */~
            prompt$(15)25,               /* FIELD NAME PROMPT          */~
            record$(12)250,              /* 3 RECORDS * 1000 CHARS EA. */~
            record%(15),                 /* WHICH OF 3 RECORDS IT'S IN */~
            to$(15)25,                   /* HI VALUE RANGE DATA TEST   */~
            tonr(15)                     /* HI RANGE NUMERIC RANGE TEST*/~

        dim                                                              ~
            xixacct$16,                  /* CHECK CREDIT ACCOUNT       */~
            accountdescr$32,             /* DESCRIPTION OF CHECK CREDIT*/~
            accountkey$(24)9,            /* KEY FOR ACCOUNT INFO       */~
            address$(3)30,               /* CUSTOMER ADDRESS.          */~
            amount$10,                   /* LINE ITEM AMOUNT           */~
            amount(15),                  /* USED WHEN MANUALY RECONCLNG*/~
            balance$12,                  /* OUTSTANDING BALANCE ON ACCT*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            check$(15)8,                 /* CHECKS WHEN MANUALY RECONCL*/~
            checkcode$12,                /* PAYEE ON CHECK             */~
            checkdate$8,                 /* DATE OF CHECK              */~
            checkdetailkey$20,           /* KEY FOR CHECK DETAILS      */~
            checkkeys$(2)9,              /* PF KEYS ON FOR LINE ITEMS  */~
            checknumber$8,               /* CHECK NUMBER               */~
            code$(2)8,                   /* VENDOR OR EMPLOYEE TEXT    */~
            cshacct$16,                  /* CASH IN BANK ACCOUNT       */~
            cshacctdescr$32,             /* DESCRIPTION OF CASH IN BANK*/~
            cshtemp$16,                  /* CASH IN BANK ACCOUNT TEMP  */~
            cshtempdescr$32,             /* DESCRIPTION OF CASH IN BANK*/~
            cursor%(2),                  /* CURSOR POSITION            */~
            date$8,                      /* TODAY'S DATE               */~
            dateposted$10,               /* DATE CHECK WAS POSTED      */~
            datereconciled$8,            /* DATE CHECK WAS RECONCILED  */~
            deductions$10,               /* HEADER DEDUCTIONS          */~
            dedxn$12,                    /* DEDUCTION DESCRIPTION      */~
            deposits$(6)37,              /* DIRECT DEPOSIT BREAKDOWN   */~
            disacct$16,                  /* DISCOUNT ACCOUNT NUMBER    */~
            disacctdescr$32,             /* DESCRIPTION OF DISCOUNT ACC*/~
            disc$10,                     /* LINE ITEM DISCOUNT         */~
            discount$10,                 /* HEADER DISCOUNT            */~
            earn$18,                     /* EARNINGS DESCRIPTION       */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            errormsg$79,                 /* ERROR MESSAGE FOR DATA TEST*/~
            fac$(20)1,                   /* FACS FOR LINE ITEM SCREEN  */~
            fnct$50,                     /* Screen line for function   */~
            firstpaydate$8,              /* FIRST DATE THIS PAY PERIOD */~
            grossacct$16,                /* GROSS PAYROLL ACCOUNT      */~
            grossacctdescr$32,           /* DESCRIPTION OF GROSS ACCT  */~
            grosschk$10,                 /* HEADER GROSS AMOUNT        */~
            hdrdate$45,                  /* HEADER FOR PRINTED OUTPUT  */~
            i$(24)80,                    /* USED FOR CURSOR POSITION   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            invdate$8,                   /* DATE OF INVOICE            */~
            invoice$16,                  /* INVOICES PAID BY A CHECK   */~
            keys$(11)50,                 /* ACTIVE KEYS IN MAIN SCREEN */~
            lastdate$8,                  /* LAST OF LAST MODIFICATION  */~
            lastpaydate$8,               /* LAST DATE THIS PAY PERIOD  */~
            lastuserid$3,                /* USER LAST MODIFYING DOCUMNT*/~
            line$(20)79,                 /* SCREEN TO DISPLAY TO       */~
            line2$79,                    /* Screen line #2             */~
            manual$8,                    /* For call to MANUAL         */~
            manualkey$50,                /* PLOW KEY FOR MANUAL RECONCL*/~
            message$79,                  /* MESSAGE TEXT STRING        */~
            mode$7,                      /* FLAG FOR DISPLAY OR PRINT  */~
            mod$3,                       /* ACCOUNTING MODULE FOR MANUL*/~
            module$3,                    /* ACCOUNTING MODULE TO SORT  */~
            name$62,                     /* FORMATTED EMPLOYEE NAME    */~
            name$(3)20,                  /* EMPLOYEE NAME              */~
            net$10,                      /* LINE ITEM NET AMT          */~
            netchk$10,                   /* NET CHECK AMOUNT FOR PRINT */~
            olddaterec$6,                /* OLD RECONCILED FIELD       */~
            oldreconciled$1,             /* OLD RECONCILED FLAG        */~
            origdate$8,                  /* DATE OF ORIGINAL ENTRY     */~
            origuserid$3,                /* USER FIRST ENTERED DOCUMNT */~
            osadj(10),                   /* O/S BALANCE ADJUST ARRAY   */~
            osmessage$79,                /* OUTSTANDING CHECK MESSAGE  */~
            othermsg$35,                 /* OUTSTANDING CHECK MESSAGE  */~
            paidarrow$2,                 /* ARROW FOR CHECKS PAID TODAY*/~
            passpgm$18,                  /* Pgm & Rel # for SLCTSCRN   */~
            pbalance$12,                 /* PROPOSED CHECKING ACCT BAL */~
            pfkey$79,                    /* FORMATTED LIST OF PF KEYS  */~
            pfkeycheck$(2)79,            /* PFKEYS FOR DISPLAY CHECK   */~
            pfkeyregister$(2)79,         /* PFKEYS FOR DISPLAY SUMMARY */~
            plowkey$(2)30,               /* FOR PLOWS ON PAYROLL CHECK */~
            print$(6)12,                 /* FORMATTED PAYROLL STUB INFO*/~
            readkey$50,                  /* KEY TO FIND DATA WITH      */~
            readkey2$50,                 /* KEY TO FIND DATA WITH      */~
            receiver$16,                 /* PURCHASE ORDER NUMBER      */~
            reconciled$1,                /* CHECK RECONCILIATION FLAG  */~
            registerline$(20)79,         /* BUFFER LINE$()             */~
            reglinekey$(24)23,           /* KEY FOR BRANCHING          */~
            sort$1,                      /* SORT OPTION FLAG           */~
            sortkey$100,                 /* KEY FOR SORTING WORK FILE  */~
            seqnr$3,                     /* REVERSE PAYCHECK SEQ. NO.  */~
            runmode$3,                   /* MODULE OK FROM GET PARM -    ~
                                              A/P, PRL, ALL            */~
            tempacct$16,                 /* TEMPORARY ACCT STRING      */~
            tempdate$8,                  /* TEMPORARY DATE STRING      */~
            tempreconciled$1,            /* TEMPORARY RECONCILED FLAG  */~
            thischeckkey$50,             /* PLOW KEY FOR THIS CHECK    */~
            thismodule$3,                /* MODULE KEY FOR THIS CHECK  */~
            tttle$79,                    /* TTTLES FOR SCREEN DISPLAY  */~
            tttlechk$(2)79,              /* TTTLES FOR LINE ITEM SCREEN*/~
            totalrec$12,                 /* TOTAL OF RECONCILED  CHECKS*/~
            total$12,                    /* TOTAL OF RECONCILED  CHECKS*/~
            type$16,                     /* TYPE OF A/P DEBIT ACCOUNT  */~
            userid$3,                    /* HOLDS USERID               */~
            vencode$9,                   /* VENDOR CODE                */~
            venname$30                   /* VENDOR NAME                */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

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
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE  - FISCAL DATES  *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 4 ! VENDOR   ! VENDOR MASTER RECORD FILE                *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN FILE  (INVOICE DATES)      *~
            * # 7 ! CSHMASTR ! CASH DISBURSEMENTS CHECK HEADER FILE     *~
            * # 8 ! CSHLINES ! CASH DISBUSREMENTS CHECK DETAIL FILE     *~
            * # 9 ! PRLCHK   ! PAYROLL CHECK REGISTER FILE              *~
            * #10 ! SORTWORK ! WORK FILE TO SORT WITH                   *~
            * #11 ! WORKFILE ! WORK FILE FOR MANUAL RECONCILE CHECKRECAL*~
            * #12 ! PERMASTR ! PERSONNEL MASTER FILE                    *~
            * #13 ! PRLCHK2  ! PAYROLL CHECK REGISTER LINES FILE        *~
            * #20 ! GLDETAIL ! GENERAL LEDGER DETIAL FILE               *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #2,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #4,  "VENDOR",                                        ~
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
                        alternate key 1, keypos = 41, keylen = 9, dup,   ~
                                  key 2, keypos = 50, keylen = 6, dup,   ~
                                  key 3, keypos = 10, keylen = 8, dup

            select  #8, "CSHLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 20,                         ~
                        alternate key 1, keypos = 21, keylen = 16, dup

            select # 9, "PRLCHK",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 23,                       ~
                        alt key  1, keypos =  13, keylen =  11,          ~
                            key  2, keypos =  42, keylen =   9, dup

            select #10, "SORTWORK",                                      ~
                        consec,                                          ~
                        recsize = 100

            select #11, "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 24,                                    ~
                        keypos = 1, keylen = 24

            select #12, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #13, "PRLCHK2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 25

            select #20, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26                      ~

            REM DO A GETPARM TO FIND WHICH MODULES ARE ACTIVE
                call "GETPARM" addr ("ID", "S", "RUNMODE ", " ", "0001", ~
                                     "CHKREG", 0%, "K", "RUNMODE ",      ~
                                     runmode$, 3%, 1%, 30%, "C")

            if runmode$ = "PAY" then L03200
            if runmode$ = "PRL" then L03200
            runmode$ = "ALL"

L03200:     call "SHOSTAT" ("Linking To The Data Base For Check Register ~
        ~Management")

            call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (# 2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))
            if runmode$ = "PRL" then L03300
            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (# 7, "SHARE", f2%( 7), rslt$( 7), axd$( 7))
            call "OPENFILE" (# 8, "SHARE", f2%( 8), rslt$( 8), axd$( 8))
L03300:     if runmode$="PAY" then L03350
            call "OPENFILE" (# 3, "SHARE", f2%( 3), rslt$( 3), axd$( 3))
            call "OPENFILE" (# 9, "SHARE", f2%( 9), rslt$( 9), axd$( 9))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
L03350:     call "OPENFILE" (#20, "SHARE", f2%(20), rslt$(20), axd$(20))

        REM *************************************************************~
            *         L O C A T I O N    O F    R O U T I N E S         *~
            *                                                           *~
            * DESPITE THE SIZE OF THIS PROGRAM, IT IS MERELY A COLLECT- *~
            * ION OF SMALL ROUTINES.  UNDERSTANDING THESE ROUTINES IS   *~
            * THE KEY TO UNDERSTANDING THIS LARGE BUT SIMPLE PROGRAM.   *~
            *                                                           *~
            * MAIN_SCREEN                                    10000      *~
            * SELECT_RECORDS                                 11000      *~
            * SORT_REGISTER                                  12000      *~
            *                                                           *~
            *      DISPLAY_REGISTER                          13000      *~
            *           EXIT_DISPLAY_REGISTER                13000      *~
            *           FORMAT_DISPLAY_REGISTER              14000      *~
            *                                                           *~
            *           DISPLAY_ENTIRE_AP_CHECK              15000      *~
            *                EXIT_DISPLAY_ENTIRE_AP_CHECK    15000      *~
            *                PRINT_ENTIRE_AP_CHECK           16000      *~
            *                                                           *~
            *           DISPLAY_ENTIRE_PR_CHECK              17000      *~
            *                EXIT_DISPLAY_ENTIRE_PR_CHECK    17000      *~
            *                PRINT_ENTIRE_PR_CHECK           18000      *~
            *                                                           *~
            *           RECONCILE_CHECK                      19000      *~
            *           UNRECONCILE_CHECK                    19000      *~
            *                                                           *~
            *           OUTSTANDING_BALANCE_CALCULATOR       20000      *~
            *                                                           *~
            *      PRINT_REGISTER                            24000      *~
            *                                                           *~
            * CONTROL_DISPLAY_REGISTER                       26000      *~
            *                                                           *~
            * CONTROL_PRINT_REGISTER                         28000      *~
            * CONTORL_PRINT_ENTIRE_AP_CHECK                  28000      *~
            *                                                           *~
            * READ_NEXT_CHECK                                30000      *~
            * READ_PO_NUMBER                                 31000      *~
            * REWRITE_AP_CHECK_HEADER                        32000      *~
            * REWRITE_PR_CHECK_HEADER                        33000      *~
            *                                                           *~
            * SETUP_SUPER_SELECTOR                           34000      *~
            * RESET_SUPER_SELECTOR                           34000      *~
            * COMPUTE_OUTSTANDING_CHECKS_ON_ACCOUNT          35000      *~
            *                                                           *~
            * HOLD_REGISTER_POINTERS                         38000      *~
            * RESTORE_REGISTER_POINTERS                      38000      *~
            *                                                           *~
            * SUPER_SELECTOR_SCREEN                          40000      *~
            *        GET_CURSOR_POSITION                     40000      *~
            * DISPLAY_OR_PRINT_SCREEN                        41000      *~
            * CHECK_REGISTER_SCREEN                          42000      *~
            * AP_CHECK_HEADER_SCREEN                         43000      *~
            * PR_CHECK_HEADER_SCREEN                         44000      *~
            * CHECK_LINE_ITEM_SCREEN                         45000      *~
            * OUTSTANDING_BALANCE_SCREEN                     46000      *~
            *                                                           *~
            * TEST_SUPER_SELECTOR_RANGE                      50000      *~
            *                                                           *~
            * EXIT_PROGRAM                                   65000      *~
            *************************************************************

        REM *************************************************************~
            *    I N I T I A L I Z A T I O N   F O R   P R O G R A M    *~
            *                                                           *~
            * INITIALIZES REQUIRED PROGRAM VARIABLES                    *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr ("ID", userid$) /* GET CURRENT USER    */
            date$ = date
            call "DATEFMT" (date$)

                keys$(1)  = "(1)        Start Over                     "
                keys$(2)  = "                                          "
                keys$(3)  = "(5)        Display Check Register         "
                keys$(4)  = "                                          "
                keys$(5)  = "(9)        Print Check Register           "
                keys$(6)  = "(13)       Instructions                   "
                keys$(7)  = "(15)       Print Screen                   "
                keys$(8)  = "(16)       EXIT PROGRAM                   "
                fnct$     = "PF Key     Function                       "

        pfkeyregister$(1) = "(2)First (8)Detail  (9)Outstanding Balance (~
        ~10/26)Reconcile (12/28)Unreconcile"

        pfkeyregister$(2) = "        (5)Next  (14)Prnt Register (15)Prnt ~
        ~Scrn (17)Manual Reconcile (16)EXIT"

        pfkeycheck$(1)  = "(2)First(5)Next(8)Header(10)Acct Info(13)Instr~
        ~.(14)Prt Chk(15)Prt Scrn(16)RETRN"

        pfkeycheck$(2)  = "(2)First   (8)Header   (13)Instru.   (14)Prt C~
        ~hk   (15)Prt Scrn    (16)RETURN  "

           checkkeys$(1) = hex(000205080a0d0e0f10)
           checkkeys$(2) = hex(0002080d0e0f10ffff)

        tttlechk$(1) =   "RECEIVER NUMBER  !   INVOICE NO.   !INV. DATE !~
        ~  AMOUNT  ! DISCOUNT !NET AMOUNT"

        tttlechk$(2) =   "   EARNINGS  !       UNITS       !   AMOUNT   !~
        ~  DEDUCTIONS  !     AMOUNT "

           code$(1) = "VENDOR"
           code$(2) = "EMPLOYEE"

           manual$ = "CHECKREG"
           if runmode$ = "PAY" then manual$ = "PROC2REG"
           if runmode$ = "PRL" then manual$ = "PROC6REG"

           str(line2$,62) = "CHECKREG: " & str(cms2v$,1,8)

        REM *************************************************************~
            *                  M A I N    S C R E E N                   *~
            *                                                           *~
            * GETS THE RANGE OF CHECK NUMBERS, DATES, CASH IN BANK      *~
            * ACCTS, MODULES, VENDOR/EMPLOYEE CODES OF CHECKS REGISTER  *~
            * LINES TO DISPLAY.                                         *~
            *************************************************************

        main_screen:
            errormsg$, check$() = " "
            mat amount = zer

               gosub setup_super_selector
L10130:        prompt$(8) = "   Sort by (1 - 5)"   /* RESET SORT PROMPT */

               gosub super_selector_screen
               prompt$(8) = " "
                     if keyhit%  =  1 then gosub startover
                     if keyhit%  = 16 then exit_program
                     if keyhit% <>  0 then L10130

               gosub test_super_selector_range
                     if errormsg$ <> " " then L10130

               gosub display_or_print_screen
                     if keyhit%  =  1 then gosub startover
                     if keyhit%  =  5 then mode$ = "DISPLAY"
                     if keyhit%  =  9 then mode$ = "PRINT"
                     if keyhit%  = 16 then exit_program

               gosub select_records
               gosub sort_register

               if mode$ = "DISPLAY" then gosub display_register
               if mode$ = "PRINT"   then gosub print_register

               goto main_screen

        REM *************************************************************~
            *            S E L E C T   R E C O R D S                    *~
            *                                                           *~
            * SELECTS THE CHECKS THE USER WISHES TO SEE.  FIRST THE A/P *~
            * RECORDS ARE DONE, THEN THE *** SUPER SELECTOR IS RESET TO *~
            * CORRECTLY SELECT PAYROLL RECORDS.  THE SORT FILE IS SET   *~
            * UP ACCORDING TO THE USER'S SELECTED SORT OPTION.          *~
            *************************************************************

        select_records:

            REM OPEN THE WORK FILE FOR SORTING
                if f2%(10) = 0  then close #10
                call "WORKOPEN" (#10, "OUTPT", 5000%, f2%(10))
                if f2%(11) = 0  then close #11
                call "WORKOPEN" (#11, "SHARE", 5000%, f2%(11))
                call "SHOSTAT" ("Selecting Checks for Check Register... O~
        ~ne Moment Please")
            REM NOW DO THE ACCOUNTS PAYABLE REGISTER (CSHMASTR)
                if module$ = "P/R" then payroll

                str(record$(), 1001) = "A/P"
                readkey$ = " "

L11096:         call "PLOWNEXT" (#7, readkey$, 0%, f1%(7))
                     if f1%(7) = 0 then payroll
                call "MOVERWA" addr(#7, str(record$(), 1))

                call "SLCTPASS" (maxfields%, select%)
                if select% = 0 then L11096

            REM NOW PROCESS SORT OPTION ON A SELECTED RECORD
                sortkey$ = all(" ")
                on sort% gosub L11200, L11225, L11250, L11275, L11300
                write #10, using L11144, sortkey$, "A/P",                 ~
                                        str(readkey$, 1, 17)
L11144:                    FMT CH(64), CH(3), CH(33)
                write #11, using L11156, "A", str(record$(),10,8), " ",   ~
                                             str(record$(),,9), " ",     ~
                                             eod goto L11096

L11156:         FMT CH(1), CH(8), CH(3), CH(9), CH(3)

                goto L11096


L11200:              REM  SORT CHECK # BY CHECK DATE
                          str(sortkey$,  2,  7) = str(record$(), 11,  7)
                          str(sortkey$,  9,  6) = str(record$(), 18,  6)
                          return

L11225:              REM  SORT CHECK DATE BY CHECK #
                          str(sortkey$,  1,  6) = str(record$(), 18,  6)
                          str(sortkey$,  8,  7) = str(record$(), 11,  7)
                          return

L11250:              REM  SORT DATE POSTED BY CHECK #
                          str(sortkey$,  1,  6) = str(record$(), 50,  6)
                          str(sortkey$,  8,  7) = str(record$(), 11,  7)
                          return

L11275:              REM  SORT CASH IN BANK ACCOUNT BY CHECK #
                          str(sortkey$,  1,  9) = str(record$(), 41,  9)
                          str(sortkey$, 11,  7) = str(record$(), 11,  7)
                          return

L11300:              REM  SORT VENDOR CODE BY CHECK #
                          str(sortkey$,  1,  9) = str(record$(),  1,  9)
                          str(sortkey$, 10,  3) = "   "
                          str(sortkey$, 14,  7) = str(record$(), 11,  7)
                          return

        payroll:
            REM NOW DO THE PAYROLL REGISTER (PRLEARNR)
                if module$ = "A/P" then return

                gosub reset_super_selector

                str(record$(), 1001) = "P/R"
                readkey$ = " "

L11390:         call "PLOWNEXT" (#9, readkey$, 0%, f1%(9))
                     if f1%(9) = 0 then return
                call "MOVERWA" addr(#9, str(record$(), 1))

                call "SLCTPASS" (maxfields%, select%)
                if select% = 0 then L11390

            REM NOW PROCESS SORT OPTION ON A SELECTED RECORD
                sortkey$ = all(" ")
                on sort% gosub L11475, L11500, L11525, L11550, L11575
                write #10, using L11450, sortkey$, "P/R",                 ~
                                        str(readkey$, 1, 23)
L11450:                    FMT CH(64), CH(3), CH(33)
                write #11, using L11457, "P", str(record$(),13,11),       ~
                                             str(record$(),,12),         ~
                                             eod goto L11390

L11457:               FMT CH(1), CH(11), CH(12)

                goto L11390


L11475:              REM  SORT CHECK # BY CHECK DATE
                          str(sortkey$,  1,  8) = str(record$(), 13,  8)
                          str(sortkey$,  9,  6) = str(record$(), 24,  6)
                          return

L11500:              REM  SORT CHECK DATE BY CHECK #
                          str(sortkey$,  1,  6) = str(record$(), 24,  6)
                          str(sortkey$,  7,  8) = str(record$(), 13,  8)
                          return

L11525:              REM  SORT DATE POSTED BY CHECK #
                          str(sortkey$,  1,  6) = str(record$(), 24,  6)
                          str(sortkey$,  7,  8) = str(record$(), 13,  8)
                          return

L11550:              REM  SORT CASH IN BANK ACCOUNT BY CHECK #
                          str(sortkey$,  1,  9) = str(record$(), 42,  9)
                          str(sortkey$, 10,  8) = str(record$(), 13,  8)
                          return

L11575:              REM  SORT EMPLOYEE CODE BY CHECK #
                          str(sortkey$,  1, 12) = str(record$(),  1, 12)
                          str(sortkey$, 13,  8) = str(record$(), 13,  8)
                          return

        REM *************************************************************~
            *                S O R T    R E G I S T E R                 *~
            *                                                           *~
            * THIS CONTROLS THE SORTING OF THE WORK FILE CONTAINING THE *~
            * SELECTED CHECK REGISTER ENTRIES.                          *~
            *************************************************************

        sort_register:
            call "SHOSTAT" ("Sorting Register, One Moment Please")
            call "SLCTSORT"(#10, 20%)
            return

        REM *************************************************************~
            *            D I S P L A Y    R E G I S T E R               *~
            *                                                           *~
            * THIS CONTROLS THE PLOWING FOR DISPLAY OF CHECK REGISTERS. *~
            *************************************************************

        display_register:
            line%, record%, saverecord% = 0
            init (" ") line$()
            init (" ") reglinekey$()

L13110:     gosub read_next_check
                  if f1%(10) = 0 then exit_display_register
                     gosub format_display_register
                     goto L13110

        exit_display_register:
            REM  IF REGISTER LINES LEFT TO DISPLAY, DO SO...
            if line% = 0 then gosub L13230
               line% = 1000
               gosub control_display_register
               goto display_register             /* RETURN TO 1ST PAGE */

L13230:     REM IF LINE% = 0 THEN TELL USER "NO CHECKS"
                init(" ") line$()
                message$ = "NO CHECKS"
                str(line$(8), 40-len(message$)/2) = message$
                return

        REM *************************************************************~
            *  S C R E E N   F O R M A T   C H E C K   R E G I S T E R  *~
            *                                                           *~
            * SCREEN FORMATS THE CHECK REGISTER THAT WE HAVE PLOWED DOWN*~
            * THE SORT  FILE TO GET.                                    *~
            *************************************************************

        format_display_register:

            line% = line% + 1%

            put line$(line%), using L14210, checknumber$, checkdate$,     ~
                              dateposted$, checkcode$, module$, cshacct$,~
                              netchk$, reconciled$, paidarrow$

            reglinekey$(line%+4) = str(checkcode$, 1, 12) &              ~
                                   str(checknumber$, 1, 8) & module$

            if line% < 20% then return
               gosub control_display_register
               return

L14210: %######## ! ######## ! ######## !############!###!############!##~
        ~########!  # ##

        REM *************************************************************~
            *    D I S P L A Y   E N T I R E    A / P    C H E C K      *~
            *                                                           *~
            * READS THE ENTIRE CHECK FROM THE CSHMASTR AND CSHLINES, SO *~
            * WE CAN DISPLAY ALL OF THE DETAIL.                         *~
            *************************************************************

        display_entire_ap_check:

            readkey$ = str(thischeckkey$,  1, 9) &                       ~
                       str(thischeckkey$, 13, 8)

            call "READ100" (#7, readkey$, f1%(7))
                 if f1%(7) = 0 then return     /* UNLIKELY */
            get #7, using L15220, vencode$, checknumber$, checkdate$,     ~
                    discount, disacct$, cshacct$, dateposted$, origdate$,~
                    origuserid$, lastdate$, lastuserid$, netchk,         ~
                    reconciled$, datereconciled$

            REM GET VENDOR NAME AND ADDRESS
                call "READ100" (#4, vencode$, f1%(4))
                     if f1%(4) <> 0 then get #4, using L15115,            ~
                                    venname$, str(address$(), 1, 60),    ~
                                              str(address$(), 61, 30)
L15115:              FMT XX(39), CH(30), CH(60), XX(60), CH(30)

            REM FORMAT DATA IN HEADER
                checkcode$ = vencode$
                call "DESCRIBE" (#2, disacct$, disacctdescr$, 1%, f1%(2))
                     call "GLFMT" (disacct$)
                call "DESCRIBE" (#2, cshacct$, cshacctdescr$, 1%, f1%(2))
                     call "GLFMT" (cshacct$)
                call "DATEFMT" (checkdate$)
                call "DATEFMT" (dateposted$)
                call "DATEFMT" (origdate$)
                if lastdate$ = " " or lastdate$ = blankdate$ ~
                                 then lastdate$ = "NONE"     ~
                   else call "DATEFMT" (lastdate$)
                if reconciled$ = "Y"  then                               ~
                   call "DATEFMT" (datereconciled$)                      ~
                else                                                     ~
                   datereconciled$ = "* O/S * "
                grosschk = netchk + discount
                discount = -discount     /* SET UP SIGN FOR DISPLAY    */
                call "CONVERT" (netchk, 2.2, netchk$)
                call "CONVERT" (grosschk, 2.2, grosschk$)
                call "CONVERT" (discount, 2.2, discount$)

L15220:     FMT CH(9),                   /* SKIP CUSTOMER CODE         */~
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
                PD(14,4),                /* NET CHECK AMOUNT           */~
                CH(1),                   /* RECONCILIATION FLAG        */~
                CH(6)                    /* DATE RECONCILED            */

L15295:     REM DISPLAY HEADER
                gosub ap_check_header_screen
                      if keyhit%  =  0 then       L15335
                      if keyhit%  =  2 then       L15335
                      if keyhit%  = 14 then gosub print_entire_ap_check
                      if keyhit%  = 16 then       return
                         goto L15295

L15335:     REM LINE ITEMS, DETAIL OF INVOICES PAID
                checkdetailkey$ = readkey$
                init(" ") line$()
                init(" ") accountkey$()
                init(hex(8c)) fac$()
                message$ = "Position cursor on line item and press PF-10 ~
        ~to see account information"
                flag% = 0
                line% = 0
                pfflag% = 1

L15390:     REM PLOW EACH INVOICE PAID BY THIS CHECK
                call "PLOWNEXT" (#8, checkdetailkey$, 17%, f1%(8))
                     if f1%(8) = 0 then L15500
                     flag% = 1
                     line% = line% + 1
                     get #8, using L15425, invoice$, xixacct$, accttype$, ~
                             amount, invdate$, disc
L15425:                      FMT XX(20), CH(16), CH(9), CH(1), PD(14,4), ~
                                 CH(6), PD(14,4)
                     accountkey$(line%+4) = xixacct$
                     call "GLFMT" (xixacct$)
                     net = amount - disc
                     call "CONVERT" (amount, 2.2, amount$)
                     call "CONVERT" (disc  , 2.2, disc$)
                     call "CONVERT" (net   , 2.2, net$)
                     call "DATEFMT" (invdate$)
                     gosub read_po_number
                     put line$(line%), using L15490, receiver$, invoice$, ~
                         invdate$, amount$, disc$, net$
                     if line% < 20 then L15390

L15490: %################ ! ################! ######## !##########!######~
        ~####!##########
L15500:     REM DISPLAY OUT LINE
                if flag% = 0 then L15295
                gosub check_line_item_screen
                      if keyhit%  =  0 then       L15560
                      if keyhit%  =  2 then       L15335
                      if keyhit%  =  5 then       L15560
                      if keyhit%  =  8 then       L15295
                      if keyhit%  = 10 then       gosub L15615
                      if keyhit%  = 14 then gosub print_entire_ap_check
                      if keyhit%  = 16 then       return
                         goto L15500

L15560:      REM BRANCH TO NEXT LINE
                 flag% = 0
                 line% = 5   /* COMPARE W/CONTROL CHECK DISLAY */
                 init(hex(8c)) fac$()
                 message$ = "Postion cursor on line item and press PF-10 ~
        ~to see account information"
                 str(line$(), 1) = str(line$(), 1186)
                 str(accountkey$(), 37) = str(accountkey$(), 136)
                 goto L15390


L15615:     REM  GET ACCOUNT INFO FOR A LINE ITEM
                     gosub get_cursor_position
                     init(hex(8c)) fac$()
                     fac$(cursor%(1) - 4%) = hex(84)
                     xixacct$ = accountkey$(cursor%(1))
                     call "DESCRIBE" (#2, xixacct$, accountdescr$, 1%,   ~
                                      f1%(2))
                     call "GLFMT" (xixacct$)
                     if accttype$ = "$" then type$ = "Cash"
                     if accttype$ = "A" then type$ = "Asset"
                     if accttype$ = "L" then type$ = "Liability"
                     if accttype$ = "C" then type$ = "Capital"
                     if accttype$ = "R" then type$ = "Revenue"
                     if accttype$ = "E" then type$ = "Expense"
                     put message$ using L15690, xixacct$, type$,          ~
                                               accountdescr$
L15690: %DEBIT ACCT: ############## ######### ###########################~
        ~#######
                     return

        REM *************************************************************~
            *       P R I N T   E N T I R E    A / P    C H E C K       *~
            *                                                           *~
            * READS THE ENTIRE CHECK FROM THE CSHMASTR AND CSHLINES, SO *~
            * WE CAN PRINT ALL OF THE DETAIL.                           *~
            *************************************************************

        print_entire_ap_check:

            call "SHOSTAT" ("Printing Check Information")
            call "DATE" addr ("HD", hdrdate$)
            select printer (134)
            print page
            print "D E T A I L E D    C H E C K    P R I N T O U T";
            print tab (85); hdrdate$
            print tab (85); "A/P008"
            print skip (2)
            print using L16780, "VENDOR CODE             ", vencode$;
            print "---------- AUDIT TRAIL DATES ---------"
            print using L16780, "VENDOR NAME             ", venname$,     ~
                               "CHECK DATE              ", checkdate$
            print using L16780, "VENDOR ADDRESS          ", address$(1),  ~
                               "G/L POSTING DATE        ", dateposted$
            print using L16780, "                        ", address$(2),  ~
                               "DATE ORIGINALLY INPUT   ", origdate$
            print using L16780, "                        ", address$(3),  ~
                                "ORIGINALLY INPUT BY    ", origuserid$
            print using L16780, "CHECK NUMBER            ", checknumber$, ~
                               "DATE LAST MODIFIED      ", lastdate$
            print using L16800, "DISCOUNT ACCOUNT        ", disacct$,     ~
                                                           disacctdescr$,~
                               "LAST MODIFIED BY        ", lastuserid$
            print using L16800, "CASH IN BANK ACCOUNT    ", cshacct$,     ~
                                                           cshacctdescr$,~
                               "DATE RECONCILED         ",datereconciled$
            print using L16780, "                        ", " "
            print using L16820, "GROSS CHECK AMOUNT      ", grosschk
            print using L16820, "LESS DISCOUNT           ", discount
            print using L16820, "                        ","=============="
            print using L16820, "NET CHECK               ", netchk

            REM LINE ITEMS...............................................

                checkdetailkey$ = readkey$         /* SET LINE ITEM KEY*/

                print skip (2)           /* PRINT HEADER LINES         */
                print           "INVOICES PAID:";
                print skip (0), "______________"
                print
                print using L16850
                print using L16870
                line% = 25

            REM PLOW EACH INVOICE PAID BY THIS CHECK
L16530:         call "PLOWNEXT" (#8, checkdetailkey$, 17%, f1%(8))
                     if f1%(8) = 1 then L16560
                        close printer : return
L16560:              get #8, using L15425, invoice$, xixacct$, accttype$, ~
                             amount, invdate$, disc
                     net = amount - disc
                     call "CONVERT" (amount, 2.2, amount$)
                     call "CONVERT" (disc  , 2.2, disc$)
                     call "CONVERT" (net   , 2.2, net$)
                     call "DATEFMT" (invdate$)
                     gosub read_po_number
                     call "DESCRIBE" (#2, xixacct$, accountdescr$, 0%,   ~
                                      f1%(2))
                     call "GLFMT" (xixacct$)
                     if accttype$ = "$" then type$ = "(CASH)"
                     if accttype$ = "A" then type$ = "(ASSET)"
                     if accttype$ = "L" then type$ = "(LIABILITY)"
                     if accttype$ = "C" then type$ = "(CAPITAL)"
                     if accttype$ = "R" then type$ = "(REVENUE)"
                     if accttype$ = "E" then type$ = "(EXPENSE)"
                     gosub control_print_entire_ap_check
                     print using L16900, receiver$, invoice$, invdate$,   ~
                         xixacct$, type$, accountdescr$, amount$, disc$, ~
                         net$
                     goto L16530          /* GET NEXT CHECK DETAIL      */

L16780: %############################  ################################  ~
        ~                ############################  ########
L16800: %############################  ##############  ##################~
        ~##############  ############################  ########
L16820: %############################  -##########.##  ##################~
        ~##############  ############################  ########

L16850: %RECEIVER NUMBER    INVOICE NO.   INV. DATE   ACCOUNT    ACCT TYP~
        ~E      ACCOUNT DESCRIPTION          DEBIT AMT  DISCOUNT      NET
L16870: %---------------- --------------- --------- ------------ --------~
        ~-- -----------------------------   ---------- ---------- --------~
        ~--
L16900: %################ ################ ######## ############ ########~
        ~## #############################   ########## ########## ########~
        ~##

        REM *************************************************************~
            *    D I S P L A Y   E N T I R E    P / R    C H E C K      *~
            *                                                           *~
            * READS THE ENTIRE CHECK FROM THE PRLEARNR FILE, SO WE      *~
            * CAN DISPLAY ALL OF THE DETAIL.                            *~
            *************************************************************

        display_entire_pr_check:
            readkey$ = str(thischeckkey$,  1, 20)

            call "READ102" (#9, readkey$, f1%(9))
                 if f1%(9) = 0 then return     /* UNLIKELY */
            get #9, using L17415, empcode$, checknumber$, seqnr$,         ~
                    checkdate$, firstpaydate$, lastpaydate$, cshacct$,   ~
                    grosschk, netchk, reconciled$, datereconciled$,      ~
                    postdate$, other$
            othermsg$ = " "
            if other$ = " " then L17105
            othermsg$ = "(Also Received Check:" &hex(84)&other$&hex(8c29)
            if str(checknumber$,,1) = "D" then L17105
            othermsg$ = "(Also Direct Deposit:" &hex(84)&other$&hex(8c29)

L17105:     plowkey$(2) = key(#9) : dp% = 0 : deposits$() = " "
            str(plowkey$(2),24) = "W" & hex(00)
L17115:     call "PLOWNEXT" (#13, plowkey$(2), 24%, f1%(13))
                if f1%(13) = 0 then L17185
            get #13, using L17130, temp$, dedxn
L17130:     FMT XX(44), CH(6), XX(16), PD(14,4)
            if temp$ <> "DIRECT" then L17115
            if dp%=0 then deposits$(1) = hex(a4) &                       ~
                                   "Bank   Account Number         Amount"
            dp% = max(dp% + 1, 2)
            get #13, using L17165, str(deposits$(dp%),9),                 ~
                                                  str(deposits$(dp%),2,4)
L17165:     FMT POS(93), CH(17), CH(4)
            call "CONVERT" (dedxn, 2.2, str(deposits$(dp%),28,10))
            goto L17115

L17185:     REM GET EMPLOYEES GROSS PAYROLL ACCOUNT
                call "READ100" (#3, empcode$, f1%(3))
                     if f1%(3) <> 0 then get #3, using L17205,            ~
                                    grossacct$
L17205:              FMT XX(45), CH(9)

            REM GET EMPLOYEE NAME AND ADDRESS
                call "READ100" (#12, empcode$, f1%(12))
                    if f1%(12) <> 0 then get #12, using L17245,           ~
                                    name$(3), name$(1), name$(2),        ~
                                    str(address$()), str(address$(3),18)

L17245:         FMT XX(1),CH(15),CH(10),CH(1),XX(33),CH(75),XX(25),CH(2)
                if name$(2) <> " " then str(name$(2),2) = "."
                str(address$(3),16,2) = hex(a1a1)
                call "SPCSMASH" (address$(3))
                str(address$(3),pos(address$(3)=hex(a1)),2) = ", "
                address$(3) = address$(3) & "."
                call "LINSMASH" (address$())

            REM FORMAT DATA IN HEADER
                checkcode$ = empcode$
                name$ = name$(1)
                if name$ = " " then name$ = name$(2)                     ~
                               else name$ = name$ & " " & name$(2)
                if name$ = " " then name$ = name$(3)                     ~
                               else name$ = name$ & " " & name$(3)
                call "DESCRIBE" (#2, cshacct$, cshacctdescr$, 1%, f1%(2))
                     call "GLFMT" (cshacct$)
                call "DESCRIBE" (#2, grossacct$, grossacctdescr$, 1%,    ~
                                 f1%(2))
                     call "GLFMT" (grossacct$)
                call "DATEFMT" (checkdate$)
                call "DATEFMT" (postdate$)
                call "DATEFMT" (firstpaydate$)
                call "DATEFMT" (lastpaydate$)
                if reconciled$ = "Y"  then                               ~
                   call "DATEFMT" (datereconciled$)                      ~
                else                                                     ~
                   datereconciled$ = "* O/S * "
                deductions = grosschk - netchk
                deductions = -deductions /* SET UP SIGN FOR DISPLAY    */
                call "CONVERT" (netchk, 2.2, netchk$)
                call "CONVERT" (grosschk, 2.2, grosschk$)
                call "CONVERT" (deductions, 2.2, deductions$)

L17415:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* REVERSE SEQUENCE NUMBER    */~
                CH(6),                   /* DATE OF CHECK              */~
                CH(6),                   /* FIRST DATE THIS PAY PERIOD */~
                CH(6),                   /* LAST DATE THIS PAY PERIOD  */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                PD(14,4),                /* GROSS PAY AMOUNT           */~
                PD(14,4),                /* NET PAY AMOUNT             */~
                CH(1),                   /* RECONCILIATION FLAG        */~
                CH(6),                   /* DATE RECONCILED            */~
                CH(6),                   /* POST DATE                  */~
                XX(4),                   /* DEPARTMENT                 */~
                CH(8)                    /* Slip/Check Number          */

L17490:     REM DISPLAY HEADER
                gosub pr_check_header_screen
                      if keyhit%  =  0 then       L17530
                      if keyhit%  =  2 then       L17530
                      if keyhit%  = 14 then gosub print_entire_pr_check
                      if keyhit%  = 16 then       return
                         goto L17490

L17530:     REM LINE ITEMS, DETAIL OF INVOICES PAID
                init(" ") line$()
                init(hex(8c)) fac$()
                message$ =  " "
                pfflag% = 2
                plowkey$(1), plowkey$(2) = key(#9)
                str(plowkey$(1),24) = "P" & hex(00)
                str(plowkey$(2),24) = "W" & hex(00)

            REM PACK THE ENTRIES IN THE EARNINGS STACK.
                for line% = 1 to 15
                    earn$ = " "
                    call "PLOWNEXT" (#13, plowkey$(1), 24%, f1%(13))
                          if f1%(13) = 0 then L17615
                    get #13, using L17605, earn$, earn1, earn2
L17605:             FMT XX(32), CH(18), PD(14,4), XX(8), PD(14,4)

L17615:             dedxn$ = " " : dp% = 0
                    call "PLOWNEXT" (#13, plowkey$(2), 24%, f1%(13))
                          if f1%(13) = 0 then L17660
                    get #13, using L17635, temp$, dedxn$, temp1$, dedxn
L17635:             FMT XX(25),CH(1),XX(6),CH(12),CH(6),XX(16),PD(14,4)
                    if str(plowkey$(2),13,1)="D" and                     ~
                                             temp1$ = "DIRECT" then L17615
                    if temp$ = "N" then L17615   /* Employee Paid Only */

L17660:             init(" ") print$()   /* INITIALIZE PRINT COLUMNS   */
                    flag% = 0
                    if earn$ = " " then L17700
                       print$(1) = str(earn$, 1, 12)
                       convert earn1 to print$(2), pic(-######.##)
                       convert earn2 to print$(4), pic(-######.##)
                       print$(3) = str(earn$, 13)
                       flag% = 1
L17700:             if dedxn$ = " "  then L17720
                       convert dedxn to print$(6), pic(-######.##)
                       print$(5) = dedxn$
                       flag% = 1
L17720:             if flag% = 0 then L17760
                    put line$(line%), using L17745, print$(1), print$(2), ~
                               print$(3), print$(4), print$(5), print$(6)
                next line%

L17745: %############ ! ########## ###### ! ########## ! ############ ! #~
        ~#########
L17755:     REM DISPLAY OUT LINE
L17760:         gosub check_line_item_screen
                      if keyhit%  =  0 then       L17755
                      if keyhit%  =  2 then       L17530
                      if keyhit%  =  8 then       L17490
                      if keyhit%  = 14 then gosub print_entire_pr_check
                      if keyhit%  = 16 then       return
                         goto L17755

        REM *************************************************************~
            *       P R I N T   E N T I R E    P/R    C H E C K         *~
            *                                                           *~
            * READS THE ENTIRE CHECK FROM PRLEARNR, SO WE CAN PRINT ALL *~
            * OF THE DETAIL.                                            *~
            *************************************************************

        print_entire_pr_check:
            call "SHOSTAT" ("Printing Check Information")
            call "DATE" addr ("HD", hdrdate$)
            select printer (134)
            print page
            print "D E T A I L E D    C H E C K    P R I N T O U T";
            print tab (85); hdrdate$
            print tab (85); "A/P008"
            print skip (2)
            print using L16780, "EMPLOYEE CODE           ", empcode$;
            print "---------- AUDIT TRAIL DATES ---------"
            print using L16780,"EMPLOYEE NAME           ",str(name$,1,32),~
                               "CHECK DATE              ", checkdate$
            print using L16780, "                        ",str(name$,33), ~
                               "G/L POSTING DATE        ", checkdate$
            print using L16780, "EMPLOYEE ADDRESS        ", address$(1),  ~
                               "DATE RECONCILED         ",datereconciled$
            print using L16780, "                        ", address$(2),  ~
                               "FIRST PAY DATE          ", firstpaydate$
            print using L16780, "                        ", address$(3),  ~
                               "LAST  PAY DATE          ", lastpaydate$
            print using L16780, "CHECK NUMBER            ", checknumber$
            print using L16800, "GROSS PAYROLL ACCOUNT   ", grossacct$,   ~
                                                         grossacctdescr$
            print using L16800, "CASH IN BANK ACCOUNT    ", cshacct$,     ~
                                                           cshacctdescr$
            print using L16820, "GROSS CHECK AMOUNT      ", grosschk
            print using L16820, "LESS DEDUCTIONS         ", deductions
            print using L16820, "                        ","=============="
            print using L16820, "NET CHECK               ", netchk

            REM LINE ITEMS...............................................

                checkdetailkey$ = readkey$         /* SET LINE ITEM KEY*/

                print skip (2)           /* PRINT HEADER LINES         */
                print           "EARINGS AND DEDUCTIONS:";
                print skip (0), "_______________________"
                print
                print using L18920
                print using L18950
                line% = 25
                plowkey$(1), plowkey$(2) = key(#9)
                str(plowkey$(1),24) = "P" & hex(00)
                str(plowkey$(2),24) = "W" & hex(00)

            REM PACK THE ENTRIES IN THE EARNINGS STACK.
                for temp% = 1 to 15
                    earn$ = " "
                    call "PLOWNEXT" (#13, plowkey$(1), 24%, f1%(13))
                          if f1%(13) = 0 then L18600
                    get #13, using L18580, earn$, earn1, earn2
L18580:             FMT XX(32), CH(18), PD(14,4), XX(8), PD(14,4)

L18600:             dedxn$ = " "
                    call "PLOWNEXT" (#13, plowkey$(2), 24%, f1%(13))
                          if f1%(13) = 0 then L18680
                    get #13, using L18640, temp$, dedxn$, dedxn
L18640:             FMT XX(25), CH(1), XX(6), CH(18), XX(16), PD(14,4)
                    if temp$ = "N" then L18600   /* Employee Paid Only */


L18680:             init(" ") print$()   /* INITIALIZE PRINT COLUMNS   */
                    flag% = 0
                    if earn$ = " " then L18760
                       print$(1) = str(earn$, 1, 12)
                       convert earn1 to print$(2), pic(-######.##)
                       convert earn2 to print$(4), pic(-######.##)
                       print$(3) = str(earn$, 13)
                       flag% = 1
L18760:             if dedxn$ = " "  then L18800
                       convert dedxn to print$(6), pic(-######.##)
                       print$(5) = dedxn$
                       flag% = 1
L18800:             if flag% = 0 then L18850
                    print using L18880, print$(1), print$(2), print$(3),  ~
                                       print$(4), print$(5), print$(6)
                next temp%

L18850:         close printer
                return

L18880: %############   ########## ######   ##########   ############   #~
        ~#########

L18920: %   EARNINGS          UNITS           AMOUNT      DEDUCTIONS     ~
        ~ AMOUNT

L18950: %-------------  -----------------   ----------   ------------   -~
        ~---------

        REM *************************************************************~
            *               R E C O N C I L E    C H E C K              *~
            *                                                           *~
            * ALLOWS USER TO RECONCILE A CHECK.  UPDATES DISPLAY ONLY IF*~
            * FILE REWRITE IS SUCCESSFUL.                               *~
            *************************************************************

        reconcile_check:

            tempdate$ = date
            tempreconciled$ = "Y"
            if thismodule$ = "A/P" then gosub rewrite_ap_check_header    ~
                                   else gosub rewrite_pr_check_header

            if error% = 1 then return

        REM UPDATE FLAGS AND DATES NOW THAT WE ARE SURE OF REWRITE
            reconciled$ = "Y"
            datereconciled$ = date$
            if regline% <> 0 then                                        ~
                             str(registerline$(regline%), 76, 4) = "Y <-"

            return

        REM *************************************************************~
            *           U N R E C O N C I L E    C H E C K              *~
            *                                                           *~
            * ALLOW USER TO UNRECONCILE CHECK. UPDATES DISPLAY ONLY IF  *~
            * FILE REWRITE IS SUCCESSFUL.                               *~
            *************************************************************

        unreconcile_check:

            tempdate$ = " "
            tempreconciled$ = "N"
            if thismodule$ = "A/P" then gosub rewrite_ap_check_header    ~
                                   else gosub rewrite_pr_check_header

            if error% = 1 then return

        REM UPDATE FLAGS AND DATES NOW THAT WE ARE SURE OF REWRITE
            reconciled$ = "N"
            datereconciled$ = "* O/S * "
            str(registerline$(regline%), 76, 4) = "N   "

            return

        REM *************************************************************~
            *O U T S T A N D I N G   B A L A N C E   C A L C U L A T O R*~
            *                                                           *~
            * THIS SUBROUTINE COMPUTES THE OUTSTANDING BALANCE OF       *~
            * UNRECONCILED CHECKS FOR A GIVEN CASH IN BANK ACCOUNT.  THE*~
            * CHECKS THAT HAVE BEEN RECONCILED TODAY ARE TOTALED USING  *~
            * THE FILE REGTEMP.  THE ROUTINE ALLOWS UP TO 10            *~
            * ADJUSTMENTS TO THE OUTSTANDING BAL.                       *~
            *************************************************************

        outstanding_balance_calculator:
L20120:    mat osadj = zer
           fac$(1) = hex(81)
           fac$(2) = hex(8c)
           init(" ") cshtemp$, cshtempdescr$, total$, totalrec$,         ~
                     balance$, pbalance$
           totalrec, total, balance, pbalance = 0
           flag% = 0%
           osmessage$ = "Enter Account Number and Press RETURN"

L20210:    gosub outstanding_balance_screen
                 if oskeyhit% =  0% then L20290
                 if oskeyhit% =  1% then L20120
                 if oskeyhit% =  5% then L20120
                 if oskeyhit% =  9% then L20440
                 if oskeyhit% = 16% then return
                    goto L20210

L20290:    if flag% = 1 then L20410
              errormsg$ = " "
              call "GETCODE" (#2, cshtemp$, cshtempdescr$, 1%, 0, f1%(2))
              if f1%(2) = 1 then L20350
                 errormsg$ = "Nonexistent G/L ACCOUNT...Please Re-enter"
                 goto L20210
L20350:       gosub compute_outstanding_checks_on_account
              osmessage$ = "Enter Adjustments and Press RETURN to Recompu~
        ~te Outstanding Balance"
              fac$(1) = hex(84)
              fac$(2) = hex(82)
              flag% = 1
L20410:    gosub L20490
           goto L20210

L20440: REM  CLEAR ADJUSTMENTS AND RESET BALANCE
             mat osadj = zer
             gosub L20490
             goto L20210

L20490: REM  COMPUTE, ROUND AND FORMAT BALANCES
             call "CONVERT" (balance, 2.2, balance$)
             call "CONVERT" (total  , 2.2, total$)
             call "CONVERT" (totalrec, 2.2, totalrec$)

             pbalance = balance + total
             for temp% = 1 to 10
                  osadj(temp%) = round(osadj(temp%), 2)
                  pbalance = pbalance + osadj(temp%)
             next temp%

             call "CONVERT" (pbalance, 2.2, pbalance$)
             return

        REM *************************************************************~
            *              P R I N T    R E G I S T E R                 *~
            *                                                           *~
            * THIS CONTROLS THE PLOWING FOR PRINT OF CHECK REGISTERS.   *~
            *************************************************************

        print_register:
            REM SETUP FOR PRINTING REGISTER
                call "SETPRNT" ("A/P008", " ", 0%, 0%)
                line%   = 1000
                reconed, unrecon = 0
                record% = 0
                pagenumber% = 0
                call "SHOSTAT" ("Printing check register")

            REM GET NEXT CHECK AND PRINT IT
L24140:         gosub read_next_check    /* LOAD UP THE CHECK HEADER   */
                if f1%(10) = 0 then exit_print_register

                   REM PRINT LINE ......................................
                         gosub control_print_register
                         print using L28370, checknumber$, checkdate$,    ~
                               dateposted$, checkcode$, module$,         ~
                               cshacct$, netchk$, datereconciled$,       ~
                               paidarrow$

                if reconciled$="Y" then reconed=round(netchk+reconed,2)  ~
                                   else unrecon=round(netchk+unrecon,2)
                goto L24140

        exit_print_register:
                REM PRINT CAP LINE IF NECESSARY
                if record% = 1 then L24340 /* ANY CHECKS PRINTED?       */
                   line% = line% + 1
                   print using L28362     /* CAP LINE                   */
                   print using L28340, reconed
                   print using L28344, unrecon
                   print using L28350, round(unrecon + reconed,2)
                   print using L28310
L24310:            close printer
                   call "SETPRNT" ("A/P008", " ", 0%, 1%)
                   return

L24340:         REM IF RECORD% = 0, THEN TELL USER "NO CHECKS"
                    if line% > 57 then gosub new_page
                    line% = line% + 6
                    print skip (3)
                    print tab (58);"* * * * * * * *"
                    print tab (58);"*  NO CHECKS  *"
                    print tab (58);"* * * * * * * *"
                    goto L24310

        REM *************************************************************~
            *              D I S P L A Y    C O N T R O L               *~
            *                                                           *~
            * THESE ROUTINES HANDLE THE PAGING FUNCTION FOR THE         *~
            * DISPLAYS AND ALSO, IF REQUESTED, HANDLE BRANCHING TO      *~
            * OTHER FUNCTIONS.                                          *~
            *************************************************************

        control_display_register:

L26050:        gosub check_register_screen        /* SHOW SCREEN AS IS */
                     if keyhit%  =  2 then       L26130       /* FIRST  */
                     if keyhit%  =  5 then       L26190       /* NEXT   */
                     if keyhit%  =  8 then       L26225       /* DETAIL */
                     if keyhit%  =  9 then       L26295       /* O/S BAL*/
                     if keyhit%  = 10 then       L26345       /* REC    */
                     if keyhit%  = 26 then       L26315       /* REC    */
                     if keyhit%  = 12 then       L26445       /* UNREC  */
                     if keyhit%  = 28 then       L26415       /* UNREC  */
                     if keyhit%  = 14 then       L26515       /* PRT REG*/
                     if keyhit%  = 16 then       L26545       /* EXIT   */
                     if keyhit%  = 17 then       L26575       /* MANUAL */
                        goto L26050

L26130:        REM BRANCH TO FIRST ENTRY IN REGISTER
                   return clear          /* FOR CONTROL_DISPLAY_REGIS  */
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_DISPLAY_REGISTR */
                   goto display_register        /* BRANCH TO 1ST ENTRY */

L26190:        REM NEXT PAGE OF CHECKS
                   if line% = 1000% then L26050
                   saverecord% = max(record%    , 0)
                   line% = 0      /* SET LINE COUNTER           */
                   reglinekey$(), line$() = " "
                   return

L26225:        REM BRANCH TO DISPLAY INDIVIDUAL CHECK
                   gosub get_cursor_position
                   if reglinekey$(cursor%(1)) = " " then L26050
                   gosub hold_register_pointers
                   thischeckkey$ = reglinekey$(cursor%(1))
                   thismodule$   = str(thischeckkey$, 21, 3)
                   regline%      = cursor%(1) - 4%
                   if thismodule$ = "A/P" then                           ~
                      gosub display_entire_ap_check                      ~
                   else                                                  ~
                      gosub display_entire_pr_check
                   gosub restore_register_pointers
                   goto L26050

L26295:        REM BRANCH TO OUTSTANDING BALANCE CALCULATOR
                   gosub outstanding_balance_calculator
                   goto L26050

L26315:        REM GLOBAL RECONCILE (WHOLE SCREEN)
                   for i%=5% to 24%
                       cursor%(1)=i%:gosub L26370
                   next i%
                   goto L26050

L26345:        REM BRANCH TO RECONCILE CHECK
                   gosub L26365
                   goto L26050

L26365:            gosub get_cursor_position
L26370:            if reglinekey$(cursor%(1)) = " " then L26405
                   gosub hold_register_pointers
                   thischeckkey$ = reglinekey$(cursor%(1))
                   thismodule$   = str(thischeckkey$, 21, 3)
                   regline%      = cursor%(1) - 4%
                   gosub reconcile_check
                   gosub restore_register_pointers
L26405:            return

L26415:        REM GLOBAL UNRECONCILE (WHOLE SCREEN)
                   for i% = 5% to 24%
                       cursor%(1)=i%:gosub L26470
                   next i%
                   goto L26050

L26445:        REM BRANCH TO UNRECONCILE CHECK
                   gosub L26465
                   goto L26050

L26465:            gosub get_cursor_position
L26470:            if reglinekey$(cursor%(1)) = " " then L26505
                   gosub hold_register_pointers
                   thischeckkey$ = reglinekey$(cursor%(1))
                   thismodule$   = str(thischeckkey$, 21, 3)
                   regline%      = cursor%(1) - 4%
                   gosub unreconcile_check
                   gosub restore_register_pointers
L26505:            return

L26515:        REM BRANCH TO PRINT REGISTER
                   gosub hold_register_pointers
                   gosub print_register
                   gosub restore_register_pointers
                   goto L26050

L26545:        REM BRANCH TO MAIN SCREEN
                   return clear          /* FOR CONTROL_DISPLAY_CHECK  */
                   if line% < 1000 then                                  ~
                   return clear          /* FOR FORMAT_DISPLAY_CHECK   */
                   return                /* TO MAIN SCREEN             */

L26575
*       ***** REM MANUAL CHECK RECONCILE ******************************
                   record% = saverecord% - 1
L26585:            done_any% = 0
                   errormsg$, check$() = " "
                   mat amount = zer
                   fac$(1) = hex(8c)
                   if runmode$ = "ALL" then fac$(1) = hex(81)
                   if runmode$ = "PAY" then mod$ = "A/P"
                   if runmode$ = "PRL" then mod$ = "P/R"
                   osmessage$ = " "
L26620:            gosub enter_checks
                   if oskeyhit% <> 16 then L26635
                       record% = saverecord%
                       line% = 0      /* SET LINE COUNTER           */
                       reglinekey$(), line$() = " "
                       return
L26635:            if oskeyhit% <> 0 then L26585
                   if mod$ = "P/R" or mod$ = "A/P" then L26655
                     errormsg$ = "Please Select The Module"
                     goto L26620
L26655:            fac$(1) = hex(8c)
                   key$ = "A" : if mod$ = "P/R" then key$ = "P"

                   for i% = 1% to 15%
                     if check$(i%) = " " then L26880
                        if str(key$,,1) = "A" then L26800

        REM PAYROLL CHECK MANUAL RECONCILE
                     call "STRING" addr("RJ", check$(i%), 8%)
                     str(key$,2) = check$(i%) & hex(00)
                     call "PLOWNEXT" (#11, key$, 9%, f1%(11))
                            if f1%(11) = 0 then L26940
                     get #11, using L26738, str(thischeckkey$,13,8),      ~
                                                  str(thischeckkey$,,12)
L26738:                   FMT XX(1), CH(8), XX(3), CH(12)
           REM NOTE THAT WE ONLY DEAL WITH THE CURRENT VERSION
                     manualkey$ = str(thischeckkey$,,20)
                     call "PLOWNEXT" (#9, manualkey$, 20%, f1%(9))
                            if f1%(9) = 0 then L26940
                     get #9, using L26770, chkamount
L26770:              FMT XX(58), PD(14,4)  /* NET PAY AMOUNT */
                     if round(chkamount,2)<>round(amount(i%),2) then L26915
                     thischeckkey$ = manualkey$
                     thismodule$   = "P/R"
                     gosub reconcile_check
                     if error% = 1 then L26890
                     goto L26875

L26800: REM  AP CHECK MANUAL RECONCILE
                     str(key$,2) = str(check$(i%),1,8) & hex(00)
                     call "PLOWNEXT" (#11, key$, 9%, f1%(11))
                            if f1%(11) <> 0 then L26838
                     if str(check$(i%),1,1) = "M" then L26826
                        convert check$(i%) to temp%, data goto L26940
                        convert temp% to str(key$,2), pic(00000000)
                     call "PLOWNEXT" (#11, key$, 9%, f1%(11))
                            if f1%(11) <> 0 then L26838
                            goto L26830
L26826:              convert str(check$(i%),2,7) to temp%, data goto L26940
                     convert temp% to str(key$,3,7), pic(0000000)
L26830:              str(key$,2,1) = "M"
                     call "PLOWNEXT" (#11, key$, 9%, f1%(11))
                            if f1%(11) = 0 then L26940

L26838:              get #11, using L26842, str(manualkey$,10,8),         ~
                                               str(manualkey$,1,9)
L26842:                  FMT XX(1), CH(8), XX(3), CH(9)
                     call "READ100" (#7, manualkey$, f1%(7))
                            if f1%(7) = 0 then L26940
                     get #7, using L26850, chkamount
L26850:              FMT XX(73), PD(14,4)   /* NET PAY AMOUNT */
                     if round(chkamount,2)<>round(amount(i%),2) then L26915
                     thischeckkey$ = str(manualkey$,,9) & "   " &        ~
                                                    str(manualkey$,10,8)
                     thismodule$   = "A/P"
                     regline%      = 0
                     gosub reconcile_check
                     if error% = 1 then L26890

L26875:              done_any% = 1
L26880:              errormsg$, check$(i%) = " " : amount(i%) = 0
                     goto L26970
L26890:                   errormsg$ = "Check Number [" & check$(i%) &    ~
                                         "] Has Already Been Reconciled"
                          goto L26960
L26915:                   errormsg$ = "Amount Is Incorrect On Check ["   ~
                               & check$(i%) & "]. Please Re-enter Amount"
                          goto L26960
L26940:                   errormsg$ = "Check Number [" & check$(i%) &    ~
                                           "] Not Found. Please Re-enter"
L26960:                   i% = 15%
                          fac$(1) = hex(8c)
L26970:            next i%
                   if errormsg$ <> " " then L26620
                   errormsg$ = hex(84) &"Enter Next Set Of Check Numbers"
                   if done_any% = 0 then errormsg$ = hex(84) & "Please" &~
            " Enter List Of Checks To Reconcile, Press (ENTER) When Done"
                   goto L26620

        REM *************************************************************~
            *     H A R D C O P Y    P A G E    C O N T R O L L E R     *~
            *                                                           *~
            * HANDLES PAGING AND HEADINGS WHEN WE ARE PRINTING THE      *~
            * REGISTER AND INDIVIDUAL CHECK INFORMATION.                *~
            *************************************************************

        control_print_register:
                line% = line% + 1%
                if line% < 54% then return
                   gosub new_page
                   print using L28310
                   print using L28354
                   print using L28362
                   return

        new_page:
                   select printer (134)
                   if line% < 100% then print using L28310
                   print page
                   line% = 6%
                   pagenumber% = pagenumber% + 1%
                   call "DATE" addr ("HD", hdrdate$)
                   print using L28270, pagenumber%, hdrdate$
                   print
                   return

L28270: %PAGE #####     A/P008              C H E C K    R E G I S T E R ~
        ~                       ##########################################~
        ~###

L28310: %================================================================~
        ~=================================================================~
        ~===
L28340: %!                                                               ~
        ~                     TOTAL RECONCILED    -########.## !          ~
        ~  !
L28344: %!                                                               ~
        ~                     TOTAL UNRECONCILED  -########.## !          ~
        ~  !
L28350: %!                                                               ~
        ~                     TOTAL CHECKS        -########.## !          ~
        ~  !
L28354: %! CHECK NO.! CHECK DATE ! DATE POSTED !    PAYEE     !       C O~
        ~ M M E N T S        !MODULE!  ACCOUNT     !  AMOUNT   ! DATE PAID~
        ~  !

L28362: %+----------+------------+-------------+--------------+----------~
        ~--------------------+------+--------------+-----------+----------~
        ~--+

L28370:    %! ######## !  ########  !   ########  ! ############ !       ~
        ~                       !  ### ! ############ !########## ! ######~
        ~## ##!

        control_print_entire_ap_check:
                line% = line% + 1
                if line% < 66 then return
                print page
                call "DATE" addr ("HD", hdrdate$)
                print "CONTINUATION OF INVOICES PAID:"; tab(87); hdrdate$
                print skip (2)
                print using L16800, "CUSTOMER CODE         ", vencode$,   ~
                                                             venname$
                print using L16780, "CHECK NUMBER          ", checknumber$
                print skip (2)
                print using L16850
                print using L16870
                line% = 15
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************


        REM *************************************************************~
            *    L O A D    &    F O R M A T    N E X T    C H E C K    *~
            *                                                           *~
            * LOADS UP THE NEXT CHECK FOR DISPLAYING THE CHECK REGISTER *~
            *************************************************************

        read_next_check:
                f1%(10) = 0
                record% = record% + 1%
                read #10, record = record%, using L28454, sortkey$,       ~
                          module$, readkey$, eod goto L28462
L28454:                   FMT CH(64), CH(3), CH(33)

                if module$ = "A/P" then  ap_check                        ~
                                   else  pr_check
L28462:         return

           ap_check:
                call "READ100" (#7, str(readkey$, 1, 17), f1%(7))
                     if f1%(7) = 0 then return

                    get #7, using L28482, vencode$, checknumber$,         ~
                            checkdate$, cshacct$, dateposted$, netchk,   ~
                            reconciled$, datereconciled$
                    call "GLFMT" (cshacct$)

L28482:               FMT                                                ~
                          CH(9),         /* VENDOR CODE                */~
                          CH(8),         /* CHECK NUMBER               */~
                          CH(6),         /* CHECK DATE                 */~
                          XX(8),         /* DISCOUNT AMOUNT            */~
                          XX(9),         /* DISCOUNT ACCOUNT           */~
                          CH(9),         /* CASH IN BANK ACCOUNT       */~
                          CH(6),         /* DATE POSTED                */~
                          XX(18),        /* SKIP AUDIT TRAIL DATES     */~
                          PD(14,4),      /* NET CHECK AMOUNT           */~
                          CH(1),         /* RECONCILIATION FLAG        */~
                          CH(6)          /* DATE RECONCILED            */

                    REM FORMAT CHECK DATA...
                        checkcode$ = vencode$
                        len% = 9%
                        gosub format_check_data

                    f1%(10) = 1
                    return

           pr_check:
                call "READ100" (#9, str(readkey$, 1, 23), f1%(9))
                     if f1%(9) = 0 then return

                    get #9, using L28540, empcode$, checknumber$,         ~
                            checkdate$, cshacct$,  netchk,               ~
                            reconciled$, datereconciled$, postdate$
                    call "GLFMT" (cshacct$)

L28540:               FMT                                                ~
                          CH(12),        /* EMPLOYEE CODE              */~
                          CH(8),         /* CHECK NUMBER               */~
                          XX(3),         /* REVERSE SEQUENCE NUMBER    */~
                          CH(6),         /* CHECK DATE                 */~
                          XX(6),         /* FIRST DATE THIS PAY PERIOD */~
                          XX(6),         /* LAST  DATE THIS PAY PERIOD */~
                          CH(9),         /* CASH IN BANK ACCOUNT       */~
                          XX(8),         /* GROSS AMOUNT               */~
                          PD(14,4),      /* NET CHECK AMOUNT           */~
                          CH(1),         /* RECONCILIATION FLAG        */~
                          CH(6),         /* DATE RECONCILED            */~
                          CH(6)          /* DATE POSTED                */

                    REM FORMAT CHECK DATA...
                        checkcode$ = empcode$
                        dateposted$ = postdate$
                        len% = 12%
                        gosub format_check_data

                    f1%(10) = 1
                    return

          format_check_data:
                        call "CONVERT" (netchk, 2.2, netchk$)
                        call "DATEFMT" (checkdate$)
                        call "DATEFMT" (dateposted$)
                        if reconciled$ = "Y" then                        ~
                           call "DATEFMT" (datereconciled$)

                REM  FLAG DISPLAY FOR CHECK RECONCILED TODAY AND RECORD
                        paidarrow$ = " "
                        if datereconciled$ <> date$ then return
                           paidarrow$ = "<-"

                           return

        REM *************************************************************~
            *              R E A D   P O   N U M B E R                  *~
            *                                                           *~
            * READS THE PURCHASE ORDER NUMBER FOR A VENDOR INVOICE      *~
            *************************************************************

        read_po_number:
            receiver$ = " "
            readkey2$ = vencode$
            str(readkey2$, 10) = invoice$
            call "READ100" (#5, readkey2$, f1%(5))
                 if f1%(5) = 0 then return
            get #5, using L28650, receiver$
L28650:             FMT XX(25), CH(16)
            return


        REM *************************************************************~
            *   R E W R I T E   A / P   C H E C K   H E A D E R         *~
            *                                                           *~
            * REWRITES THE A/P CHECK HEADER (CSHMASTR) IN THE EVENT OF  *~
            * A NEWLY RECONCILED CHECK.                                 *~
            *************************************************************

        rewrite_ap_check_header:
           error% = 1                              /* SET ERROR SWITCH */
           readkey$ = str(thischeckkey$,  1, 9)  &                       ~
                      str(thischeckkey$, 13, 8)
           call "READ101" (#7, readkey$, f1%(7))
                if f1%(7) = 0 then return

           get #7, using L28690, str(record$(), 1, 73), netchk,           ~
                                oldreconciled$, olddaterec$ ,junk$
L28690:            FMT CH(73), PD(14,4), CH(1), CH(6), CH(12)

           if oldreconciled$ = tempreconciled$ then return

           rewrite #7, using L28690, str(record$(), 1, 73), netchk,       ~
                                    tempreconciled$, tempdate$,junk$

           error% = 0
           return

        REM *************************************************************~
            *   R E W R I T E   P / R   C H E C K   H E A D E R         *~
            *                                                           *~
            * REWRITES THE PAYROLL EARNIINGS REGISTER  IN THE EVENT OF  *~
            * A NEWLY RECONCILED CHECK.                                 *~
            *************************************************************

        rewrite_pr_check_header:
           error% = 1                              /* SET ERROR SWITCH */
           readkey$ = str(thischeckkey$,  1, 20)
           call "READ103" (#9, readkey$, f1%(9))
                if f1%(9) = 0 then return

           get #9, using L28742, str(record$(), 1, 58), netchk,           ~
                                oldreconciled$, olddaterec$,             ~
                                str(record$(), 1000, 47)
L28742:            FMT CH(58), PD(14,4), CH(1), CH(6), CH(47)

           if oldreconciled$ = tempreconciled$ then return

           rewrite #9, using L28742, str(record$(), 1, 58), netchk,       ~
                                    tempreconciled$, tempdate$,          ~
                                    str(record$(), 1000, 47)
           error% = 0
           return

        REM *************************************************************~
            *      S E T   U P   S U P E R   S E L E C T O R            *~
            *                                                           *~
            * INITIALIZES ALL THE VARIABLES NEEDED TO DO THE REPORT.    *~
            *                                                           *~
            * HERE IS HOW THE PARAMETERS FOR THE SELECT (ALSO SORT) KEYS*~
            * WORK.  THE FIRST OF EACH DATA ITEM IS THE NAME OF THE KEY.*~
            * IT IS THE PROMPT THAT APPEARS ON THE SCREEN.              *~
            *      THE FIRST ITEM TO THE RIGHT OF THE PROMPT IS THE     *~
            * DATA TYPE.  THE DATA TYPE IS ONE OF THE FOLLOWING.        *~
            *                                                           *~
            *      U = UPPER CASE ALPHANUMERIC.                         *~
            *      L = UPPER/LOWER CASE ALPHANUMERIC.                   *~
            *      D = DATE-FORMATTED UPPER CASE FIELD.  MUST BE YYMMDD *~
            *      S = SINGLE PARAMTER FIELD (NO RANGE PROCESSING)      *~
            *      N = NUMERIC 8-BYTE FLOATING PT. ALL OTHER NUMERICS   *~
            *          SHOULD BE COMPARED ASCII, OR MODIFY THE ROUTINE. *~
            *                                                           *~
            * THE REMAINING NUMBERS IDENTIFY RESPECTIVELY THE LENGTH OF *~
            * THE FIELD ON THE DISK, THE NUMBER OF POSITIONS IT FILLS   *~
            * ON THE PAPER, WHICH OF THE UP-TO-3 RECORDS IN CORE THIS   *~
            * FIELD LIES IN, AND THE POSITION OF THE FIELD WITHIN THE   *~
            * RECORD.                                                   *~
            *                                                           *~
            * THE FIRST BLANK PROMPT NAME SIGNIFIES THE END OF THE      *~
            * LIST OF PROMPTS ON THE SYSTEM.                            *~
            *************************************************************

        setup_super_selector:

            REM SET UP DATA FOR SELECT INTERPRETATION.
                restore
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

                data "1. Check Number          ", "U",  7,  7, 1, 011,   ~
                     "2. Check Date            ", "D",  6, 10, 1, 018,   ~
                     "3. Date Posted           ", "D",  6, 10, 1, 050,   ~
                     "4. Cash in Bank Account  ", "U",  9, 12, 1, 041,   ~
                     "5. Vendor/Employee Code  ", "U",  9, 12, 1, 001,   ~
                     "   Module (A/P,P/R,ALL)  ", "S",  3,  3, 2, 001,   ~
                     "   Reconciled (Y,N,ALL)  ", "S",  1,  3, 1, 082,   ~
                     "   Sort by (1 - 5)       ", "S",  1,  1, 3, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001

            init(" ")  from$(), to$()
            mat fromnr = zer: mat tonr = zer: mat field% = zer

            for temp% = 1 to 15
                if prompt$(temp%) = " " then L28882
                   from$(temp%) = "ALL"
                next temp%

L28882:     if runmode$ = "PRL" then from$(6%) = "P/R"
            if runmode$ = "PAY" then from$(6%) = "A/P"
            from$(8) = "1"

            inpmessage$ = "Select Range of Checks and Sorting Option, The~
        ~n Press (RETURN)"

            return

        reset_super_selector:

                REM SET UP DATA FOR SELECT INTERPRETATION.
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

                data "1. Check Number          ", "U",  7,  7, 1, 014,   ~
                     "2. Check Date            ", "D",  6, 10, 1, 024,   ~
                     "3. Date Posted           ", "D",  6, 10, 1, 074,   ~
                     "4. Cash in Bank Account  ", "U",  9, 12, 1, 042,   ~
                     "5. Vendor/Employee Code  ", "U", 12, 12, 1, 001,   ~
                     "   Module (A/P,P/R,ALL)  ", "S",  3,  3, 2, 001,   ~
                     "   Reconciled (Y,N,ALL)  ", "S",  1,  3, 1, 067,   ~
                     "   Sort by (1 - 5)       ", "S",  1,  1, 3, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001

            return

        REM *************************************************************~
            *    C O M P U T E   O U T S T A N D I N G  C H E C K S     *~
            *                                                           *~
            * SUMS UP THE BALANCE OF OUTSTANDING CHECKS ON THAT ACCOUNT.*~
            *************************************************************

            compute_outstanding_checks_on_account:
               tempacct$ = cshtemp$
               call "GLUNFMT" (tempacct$)
               call "SHOSTAT" ("Computing Outstanding Checks for Account"~
                                & hex(84) & cshtemp$)
               total, totalrec = 0

                 REM  PLOW THROUGH ALL A/P CHKS WITH SAME TEMPACCT
                     call "REDALT0" (#7, tempacct$, 1%, f1%(7))
                          if f1%(7) <> 1 then L29006

L28986:              get #7, using L28988, chkamt, reconciled$, temp$
L28988:                      FMT XX(73), PD(14,4), CH(1), CH(6)
                     if reconciled$ = "Y" then L28994
                        total = round(chkamt + total, 2%)
                        goto L28996
L28994:              if temp$<>date then L28996
                        totalrec=round(totalrec+chkamt,2)
L28996:              call "READNEXT" (#7, f1%(7))
                          if f1%(7) = 0 then L29006
                          if key(#7, 1) <> tempacct$ then L29006
                     goto L28986

L29006:          REM  PLOW THROUGH ALL P/R CHECKS WITH SAME TEMPACCT
                     call "REDALT0" (#9, tempacct$, 2%, f1%(9))
                          if f1%(9) <> 1 then L29062

L29014:              get #9, using L29016, chkamt, reconciled$, temp$
L29016:                      FMT XX(58), PD(14,4), CH(1), CH(6)
                     if reconciled$ = "Y"  then  L29022
                        total = round(chkamt + total, 2%)
                        goto L29024
L29022:              if temp$<>date then L29024
                        totalrec=round(totalrec+chkamt,2)
L29024:              call "READNEXT" (#9, f1%(9))
                          if f1%(9) = 0 then L29062
                          if key(#9, 2) <> tempacct$ then L29062
                     goto L29014

L29062:          REM GET THE CURRENT BALANCE FROM G/L
                     call "GLBALANC" (#1, #2, #20, tempacct$, date$,     ~
                                                      balance, errormsg$)
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

        hold_register_pointers:
            registerline%         = line%
            registerrecord%       = record%
            mat registerline$     = line$
            return

        restore_register_pointers:
            line%               = registerline%
            record%             = registerrecord%
            mat line$           = registerline$
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
            goto main_screen

        REM *************************************************************~
            *          D I S P L A Y   M A I N    S C R E E N           *~
            *                                                           *~
            * GETS A RANGE OF CHECKS TO BE DISPLAYES/PRINTED USING      *~
            * THE *** SUPER SELECTOR *** ROUTINES.                      *~
            *************************************************************

        super_selector_screen:
            passpgm$ = "CHECKREG: " & str(cms2v$,1,8)
            call "SLCTSCRN" ("CHECK REGISTER", errormsg$, inpmessage$,   ~
                              keyhit%, passpgm$)
            return


        get_cursor_position:
                  close ws
                  call "SCREEN" addr("C",0%,"I",i$(),cursor%())
                  return

        REM *************************************************************~
            *   D I S P L A Y   O R   P R I N T   S C R E E N           *~
            *                                                           *~
            * SELECT TO DISPLAY OR PRINT THE CHECK REGESTER             *~
            *************************************************************

        display_or_print_screen:
L41065:     str(line2$,1,50) = "Select Function"

            accept                                                       ~
               at (01,02), "CHECK REGISTER",                             ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (10,15), fac(hex(ac)), fnct$                  , ch(50),~
               at (11,15), fac(hex(8c)), keys$(1)               , ch(50),~
               at (12,15), fac(hex(8c)), keys$(2)               , ch(50),~
               at (13,15), fac(hex(8c)), keys$(3)               , ch(50),~
               at (14,15), fac(hex(8c)), keys$(4)               , ch(50),~
               at (15,15), fac(hex(8c)), keys$(5)               , ch(50),~
               at (16,15), fac(hex(8c)), keys$(6)               , ch(50),~
               at (17,15), fac(hex(8c)), keys$(7)               , ch(50),~
               at (18,15), fac(hex(8c)), keys$(8)               , ch(50),~
                                                                         ~
               keys(hex(0105090d0f10)),                                  ~
               key (keyhit%)

            str(line2$,1,50) = " "

            if keyhit% <> 13 then L41870
                call "MANUAL" (manual$)
                goto L41065

L41870:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto display_or_print_screen

        REM *************************************************************~
            *         D I S P L A Y   C H E C K   R E G I S T E R       *~
            *                                                           *~
            * DISPLAYS THE LINE ITEM ARRAY LINE$() FOR REGISTER         *~
            * SELECTIONS.                                               *~
            *************************************************************

        check_register_screen:

            tttle$   ="Check  # !   Date   !Date(Post)!   Payee    !Mod! ~
        ~Account    !  Amount  ! Paid?"

            message$ ="Position cursor on register line and press PF-8 to~
        ~ see check,PF-10 to reconcile"

L42150:      accept                                                      ~
               at (01,02), fac(hex(8c)), pfkeyregister$(1)      , ch(79),~
               at (02,02), fac(hex(ac)), pfkeyregister$(2)      , ch(79),~
               at (03,02), fac(hex(84)), message$               , ch(79),~
               at (04,02), fac(hex(ac)), tttle$                 , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8e)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8e)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8e)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8e)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8e)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8e)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8e)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8e)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8e)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8e)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8e)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8e)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8e)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8e)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8e)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8e)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8e)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8e)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8e)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8e)), line$(20)              , ch(79),~
                                                                         ~
               keys(hex(020508090a0c0d0e0f10111a1c)),                    ~
               key (keyhit%)

            if keyhit% <> 13 then L42450
                call "MANUAL" (manual$)
                goto L42150

L42450:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto check_register_screen

        REM *************************************************************~
            *  D I S P L A Y    E N T I R E    C H E C K    H E A D E R *~
            *                                                           *~
            * DISPLAYS THE ENTIRE HEADER OF AN ACCOUNTS PAYABLE CHECK   *~
            *************************************************************

        ap_check_header_screen:
            pfkey$="(2)Line Items     (14)Print Check     (15)Print Scree~
        ~n     (16)EXIT        "

            accept                                                       ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (03,02),                                               ~
                  "Vendor code",                                         ~
               at (03,30), fac(hex(84)),     vencode$           , ch(09),~
               at (03,49), fac(hex(84)),     venname$           , ch(30),~
               at (04,02),                                               ~
                  "Vendor address",                                      ~
               at (04,30), fac(hex(84)),     address$(1)        , ch(30),~
               at (05,30), fac(hex(84)),     address$(2)        , ch(30),~
               at (06,30), fac(hex(84)),     address$(3)        , ch(30),~
               at (07,02),                                               ~
                  "Check number",                                        ~
               at (07,30), fac(hex(84)),     checknumber$       , ch(08),~
               at (09,02),                                               ~
                  "Discount account",                                    ~
               at (09,30), fac(hex(8c)),     disacct$           , ch(12),~
               at (09,49), fac(hex(8c)),     disacctdescr$      , ch(32),~
               at (10,02),                                               ~
                  "Cash in bank account",                                ~
               at (10,30), fac(hex(8c)),     cshacct$           , ch(12),~
               at (10,49), fac(hex(8c)),     cshacctdescr$      , ch(32),~
               at (11,02),                                               ~
                  "Gross check",                                         ~
               at (11,30), fac(hex(8c)),     grosschk$          , ch(10),~
               at (12,02),                                               ~
                  "Less discount",                                       ~
               at (12,30), fac(hex(8c)),     discount$          , ch(10),~
               at (13,30),                                " ===========",~
               at (14,02),                                               ~
                  "Net check",                                           ~
               at (14,30), fac(hex(8c)),     netchk$            , ch(10),~
               at (17,02),                                               ~
                  "---------- AUDIT TRAIL DATES ----------",             ~
               at (18,02),                                               ~
                  "Check date",                                          ~
               at (18,30), fac(hex(8c)), checkdate$             , ch(08),~
               at (19,02),                                               ~
                  "General ledger posting date",                         ~
               at (19,30), fac(hex(8c)), dateposted$            , ch(08),~
               at (20,02),                                               ~
                  "Date originally input",                               ~
               at (20,30), fac(hex(8c)), origdate$              , ch(08),~
               at (21,02),                                               ~
                  "Originally input by",                                 ~
               at (21,30), fac(hex(8c)), origuserid$            , ch(03),~
               at (22,02),                                               ~
                  "Date last modified",                                  ~
               at (22,30), fac(hex(8c)), lastdate$              , ch(08),~
               at (23,02),                                               ~
                  "Last modified by",                                    ~
               at (23,30), fac(hex(8c)), lastuserid$            , ch(03),~
               at (24,02),                                               ~
                  "Date reconciled",                                     ~
               at (24,30), fac(hex(8c)), datereconciled$        , ch(08),~
                                                                         ~
               keys(hex(00020d0e0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L43690
                call "MANUAL" (manual$)
                goto ap_check_header_screen

L43690:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto ap_check_header_screen

        REM *************************************************************~
            *  D I S P L A Y    E N T I R E    C H E C K    H E A D E R *~
            *                                                           *~
            * DISPLAYS THE ENTIRE HEADER OF A PAYROLL CHECK             *~
            *************************************************************

        pr_check_header_screen:
            dtype$ = "Check"
            if str(checknumber$,,1) = "D" then dtype$ = "Dslip"
            pfkey$="(2)Line Items     (14)Print sssss     (15)Print Scree~
        ~n     (16)EXIT        "
            str(pfkey$,29,5) = dtype$

            accept                                                       ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (03,02),                                               ~
                  "Employee code",                                       ~
               at (03,30), fac(hex(84)),     empcode$           , ch(12),~
               at (04,02), fac(hex(84)),     name$              , ch(62),~
               at (05,02),                                               ~
                  "Employee address",                                    ~
               at (05,30), fac(hex(84)),     address$(1)        , ch(30),~
               at (06,30), fac(hex(84)),     address$(2)        , ch(30),~
               at (07,30), fac(hex(84)),     address$(3)        , ch(30),~
               at (08,02), fac(hex(8c)),     dtype$             , ch(05),~
               at (08,08), "number",                                     ~
               at (08,25), fac(hex(84)),     checknumber$       , ch(08),~
               at (08,35), fac(hex(8c)),     othermsg$          , ch(35),~
               at (10,02),                                               ~
                  "Gross payroll account",                               ~
               at (10,30), fac(hex(8c)),     grossacct$         , ch(12),~
               at (10,49), fac(hex(8c)),     grossacctdescr$    , ch(32),~
               at (11,02),                                               ~
                  "Cash in bank account",                                ~
               at (11,30), fac(hex(8c)),     cshacct$           , ch(12),~
               at (11,49), fac(hex(8c)),     cshacctdescr$      , ch(32),~
               at (13,02),                                               ~
                  "Gross pay",                                           ~
               at (13,30), fac(hex(8c)),     grosschk$          , ch(10),~
               at (14,02),                                               ~
                  "Less deductions",                                     ~
               at (14,30), fac(hex(8c)),     deductions$        , ch(10),~
               at (15,30),                                 " ==========",~
               at (16,02),                                               ~
                  "Amount",                                              ~
               at (16,30), fac(hex(8c)),     netchk$            , ch(10),~
               at (17,02),                                               ~
                  "For period",                                          ~
               at (17,13), fac(hex(84)),     firstpaydate$      , ch(08),~
               at (17,22),                                               ~
                  "To ",                                                 ~
               at (17,25), fac(hex(84)),     lastpaydate$       , ch(08),~
                                                                         ~
               at (19,44), fac(hex(8c)),     deposits$(1)       , ch(37),~
               at (20,44), fac(hex(8c)),     deposits$(2)       , ch(37),~
               at (21,44), fac(hex(8c)),     deposits$(3)       , ch(37),~
               at (22,44), fac(hex(8c)),     deposits$(4)       , ch(37),~
               at (23,44), fac(hex(8c)),     deposits$(5)       , ch(37),~
               at (24,44), fac(hex(8c)),     deposits$(6)       , ch(37),~
                                                                         ~
               at (19,02),                                               ~
                  "---------- AUDIT TRAIL DATES ----------",             ~
               at (20,02), fac(hex(8c)),     dtype$             , ch(05),~
               at (20,08), "date",                                       ~
               at (20,30), fac(hex(8c)), checkdate$             , ch(08),~
               at (21,02),                                               ~
                  "General ledger posting date",                         ~
               at (21,30), fac(hex(8c)), postdate$              , ch(08),~
               at (22,02),                                               ~
                  "Date reconciled",                                     ~
               at (22,30), fac(hex(8c)), datereconciled$        , ch(08),~
                                                                         ~
               keys(hex(00020d0e0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L44790
                call "MANUAL" (manual$)
                goto pr_check_header_screen

L44790:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto pr_check_header_screen

        REM *************************************************************~
            *       D I S P L A Y   C H E C K   L I N E   I T E M S     *~
            *                                                           *~
            * DISPLAYS THE LINE ITEMS ON A CHECK.                       *~
            *************************************************************

        check_line_item_screen:
             dtype$ = "Check Number"
             if str(checknumber$,,1) = "D" then dtype$ = "Deposit Slip"

             accept                                                      ~
               at (01,02), fac(hex(ac)), pfkeycheck$(pfflag%)   , ch(79),~
               at (02,02), fac(hex(8c)), code$(pfflag%)         , ch(09),~
               at (02,12), fac(hex(84)), checkcode$             , ch(12),~
               at (02,52), fac(hex(8c)), dtype$,                         ~
               at (02,66), fac(hex(8c)), checknumber$           , ch(14),~
               at (03,02), fac(hex(84)), message$               , ch(79),~
               at (04,02), fac(hex(ac)), tttlechk$(pfflag%)     , ch(79),~
                                                                         ~
               at (05,02), fac(fac$( 1)), line$( 1)             , ch(79),~
               at (06,02), fac(fac$( 2)), line$( 2)             , ch(79),~
               at (07,02), fac(fac$( 3)), line$( 3)             , ch(79),~
               at (08,02), fac(fac$( 4)), line$( 4)             , ch(79),~
               at (09,02), fac(fac$( 5)), line$( 5)             , ch(79),~
               at (10,02), fac(fac$( 6)), line$( 6)             , ch(79),~
               at (11,02), fac(fac$( 7)), line$( 7)             , ch(79),~
               at (12,02), fac(fac$( 8)), line$( 8)             , ch(79),~
               at (13,02), fac(fac$( 9)), line$( 9)             , ch(79),~
               at (14,02), fac(fac$(10)), line$(10)             , ch(79),~
               at (15,02), fac(fac$(11)), line$(11)             , ch(79),~
               at (16,02), fac(fac$(12)), line$(12)             , ch(79),~
               at (17,02), fac(fac$(13)), line$(13)             , ch(79),~
               at (18,02), fac(fac$(14)), line$(14)             , ch(79),~
               at (19,02), fac(fac$(15)), line$(15)             , ch(79),~
               at (20,02), fac(fac$(16)), line$(16)             , ch(79),~
               at (21,02), fac(fac$(17)), line$(17)             , ch(79),~
               at (22,02), fac(fac$(18)), line$(18)             , ch(79),~
               at (23,02), fac(fac$(19)), line$(19)             , ch(79),~
               at (24,02), fac(fac$(20)), line$(20)             , ch(79),~
                                                                         ~
               keys(checkkeys$(pfflag%)),                                ~
               key (keyhit%)

            if keyhit% <> 13 then L45470
                call "MANUAL" (manual$)
                goto check_line_item_screen

L45470:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto check_line_item_screen

        REM *************************************************************~
            *    O U T S T A N D I N G   B A L A N C E   S C R E E N    *~
            *                                                           *~
            * DISPLAYS THE OUTSTANDING BALANCE FOR CHECK'S CASH ACCOUNT *~
            *************************************************************

        outstanding_balance_screen:

           accept                                                        ~
              at (01,02), "OUTSTANDING BALANCE CALCULATOR",              ~
              at (02,02), "Date:",                                       ~
              at (02,09), fac(hex(8c)), date$                   , ch(08),~
              at (03,02), fac(hex(94)), errormsg$               , ch(46),~
                                                                         ~
              at (01,48), "! Total checks reconciled today  ",           ~
              at (02,48), "! For this account:",                         ~
              at (02,68), fac(hex(84)), totalrec$               , ch(12),~
              at (03,48), "+--------------------------------",           ~
                                                                         ~
              at (04,02), "Cash in bank account",                        ~
              at (04,23), fac(fac$(1)), cshtemp$                , ch(12),~
              at (04,36), fac(hex(84)), cshtempdescr$           , ch(32),~
              at (06,02),                                                ~
                 "Current balance for account ",                         ~
              at (06,30), fac(hex(84)), cshtemp$                , ch(12),~
              at (06,43), fac(hex(8c)), balance$                , ch(12),~
              at (07,02),                                                ~
                 "Total outstanding checks",                             ~
              at (07,38), "+",                                           ~
              at (07,43), fac(hex(8c)), total$                  , ch(12),~
              at (08,02),                                                ~
                 "Enter adjustments:",                                   ~
              at (08,43), fac(fac$(2)), osadj( 1), pic(-########.##),    ~
              at (09,43), fac(fac$(2)), osadj( 2), pic(-########.##),    ~
              at (10,43), fac(fac$(2)), osadj( 3), pic(-########.##),    ~
              at (11,43), fac(fac$(2)), osadj( 4), pic(-########.##),    ~
              at (12,43), fac(fac$(2)), osadj( 5), pic(-########.##),    ~
              at (13,43), fac(fac$(2)), osadj( 6), pic(-########.##),    ~
              at (14,43), fac(fac$(2)), osadj( 7), pic(-########.##),    ~
              at (15,43), fac(fac$(2)), osadj( 8), pic(-########.##),    ~
              at (16,43), fac(fac$(2)), osadj( 9), pic(-########.##),    ~
              at (17,43), fac(fac$(2)), osadj(10), pic(-########.##),    ~
              at (18,43),                             "============",    ~
              at (19,02),                                                ~
                 "Proposed balance",                                     ~
              at (19,43), fac(hex(8c)), pbalance$               , ch(12),~
              at (21,02), fac(hex(a4)), osmessage$              , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20),                                               ~
                  "(5)New Account",                                      ~
               at (23,40),                                               ~
                  "(9)Clear Adjustments",                                ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), "(16)RETURN      ",                           ~
              keys(hex(000105090d0f10)),                                 ~
              key (oskeyhit%)

            if oskeyhit% <> 13 then L46620
                call "MANUAL" (manual$)
                goto outstanding_balance_screen

L46620:        if oskeyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto outstanding_balance_screen

        REM *************************************************************~
            *   M A N U A L   C H E C K   R E C O N C I L E A T I O N   *~
            *                                                           *~
            * ENTER CHECKS AND AMOUNTS TO RECONCILE.                    *~
            *************************************************************

        enter_checks:
           fac$(2) = hex(81)
           str(line2$,1,50) = "Enter check numbers and amounts"

           accept                                                        ~
              at (01,02), "MANUAL CHECK RECONCILE",                      ~
              at (01,67), "Date:",                                       ~
              at (01,73), fac(hex(8c)), date$                   , ch(08),~
              at (02,02), fac(hex(ac)), line2$                  , ch(79),~
                                                                         ~
              at (04,02), fac(hex(94)), errormsg$               , ch(79),~
              at (05,02), "For Module (A/P OR P/R)",                     ~
              at (05,26), fac(fac$(1)), mod$                    , ch(03),~
              at (06,02), "Check Numbers :",                             ~
              at (06,30), "Amounts :",                                   ~
              at (06,20), fac(fac$(2)), check$( 1),               ch(08),~
              at (06,40), fac(fac$(2)), amount( 1), pic(-########.##),   ~
              at (07,20), fac(fac$(2)), check$( 2),               ch(08),~
              at (07,40), fac(fac$(2)), amount( 2), pic(-########.##),   ~
              at (08,20), fac(fac$(2)), check$( 3),               ch(08),~
              at (08,40), fac(fac$(2)), amount( 3), pic(-########.##),   ~
              at (09,20), fac(fac$(2)), check$( 4),               ch(08),~
              at (09,40), fac(fac$(2)), amount( 4), pic(-########.##),   ~
              at (10,20), fac(fac$(2)), check$( 5),               ch(08),~
              at (10,40), fac(fac$(2)), amount( 5), pic(-########.##),   ~
              at (11,20), fac(fac$(2)), check$( 6),               ch(08),~
              at (11,40), fac(fac$(2)), amount( 6), pic(-########.##),   ~
              at (12,20), fac(fac$(2)), check$( 7),               ch(08),~
              at (12,40), fac(fac$(2)), amount( 7), pic(-########.##),   ~
              at (13,20), fac(fac$(2)), check$( 8),               ch(08),~
              at (13,40), fac(fac$(2)), amount( 8), pic(-########.##),   ~
              at (14,20), fac(fac$(2)), check$( 9),               ch(08),~
              at (14,40), fac(fac$(2)), amount( 9), pic(-########.##),   ~
              at (15,20), fac(fac$(2)), check$(10),               ch(08),~
              at (15,40), fac(fac$(2)), amount(10), pic(-########.##),   ~
              at (16,20), fac(fac$(2)), check$(11),               ch(08),~
              at (16,40), fac(fac$(2)), amount(11), pic(-########.##),   ~
              at (17,20), fac(fac$(2)), check$(12),               ch(08),~
              at (17,40), fac(fac$(2)), amount(12), pic(-########.##),   ~
              at (18,20), fac(fac$(2)), check$(13),               ch(08),~
              at (18,40), fac(fac$(2)), amount(13), pic(-########.##),   ~
              at (19,20), fac(fac$(2)), check$(14),               ch(08),~
              at (19,40), fac(fac$(2)), amount(14), pic(-########.##),   ~
              at (20,20), fac(fac$(2)), check$(15),               ch(08),~
              at (20,40), fac(fac$(2)), amount(15), pic(-########.##),   ~
              at (21,02), fac(hex(a4)), osmessage$              , ch(79),~
               at (23,02),                                               ~
                  "(RETURN) RECONCILE",                                  ~
               at (24,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)CANCEL/EXIT",                                     ~
              keys(hex(000105090d0f10)),                                 ~
              key (oskeyhit%)

               if oskeyhit% <> 13 then L47660
                  call "MANUAL" (manual$)
                  goto enter_checks

L47660:        if oskeyhit% <> 15 then L47700
                  call "PRNTSCRN"
                  goto enter_checks

L47700:        errormsg$ = " "
               str(line2$,1,50) = " "
               return

        REM *************************************************************~
            *                      T E S T   D A T A                    *~
            *                                                           *~
            * BE SURE THAT THE *** SUPER SELECTOR *** RANGES ARE OK     *~
            *************************************************************

        test_super_selector_range
            errormsg$ = " "

            if from$(4) = "ALL" then L50100
            if from$(4) = "FIRST" then L50090
            tempacct$ = from$(4)
            call "GLVALID" (tempacct$, from$(4), errormsg$)
L50090:     if to$(4) = "LAST" then L50096
            if to$(4) = " " then L50096
            tempacct$ = to$(4)
            call "GLVALID" (tempacct$, to$(4), errormsg$)
L50096:           if errormsg$ <> " " then return
L50100:     call "SLCTTEST" (errormsg$,  maxfields%)
                  if errormsg$ <> " " then return

            REM TEST FOR VALID MODULE ENTRY
                module$ = from$(6)
                if module$  = "A/P" or  module$  = "P/R" or              ~
                   module$  = "ALL"     then L50265
                   errormsg$ ="Module Entry Must Be A/P (Accounts Payable~
        ~), P/R (Payroll), or ALL !!"
                   return
L50265:         if runmode$ = "PAY" and from$(6%) <> "A/P" then L50280
                if runmode$ = "PRL" and from$(6%) <> "P/R" then L50290
                goto L50300
L50280:            errormsg$ ="Module Entry Must Be A/P (Accounts Payable~
        ~)"   :   return
L50290:            errormsg$ ="Module Entry Must Be P/R (Payroll)"
                 return

L50300:     REM TEST FOR VAILD RECONCILIATION ENTRY
                if from$(7) = "Y"   or  from$(7) = "N"   or              ~
                   from$(7) = "ALL"     then L50400
                   errormsg$= "Reconciled Entry Must be Y, N, or A (ALL)"
                   return

L50400:     REM TEST SORT OPTION
                sort$ = from$(8)
                convert sort$ to sort%, data goto L50440
                if sort% >= 1% and sort% <= 5% then return
L50440:            errormsg$ = "Sort Option Must Be 1, 2, 3, 4 OR 5 !!"
                   return

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program:
            call "SHOSTAT" ("Closing Files, One Moment Please")
            call "FILEBGON" (#10)
            call "FILEBGON" (#11)
            end
