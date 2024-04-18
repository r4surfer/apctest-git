        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      EEEEE  M   M  PPPP   IIIII  N   N   *~
            *  P   P  R   R  L      E      MM MM  P   P    I    NN  N   *~
            *  PPPP   RRRR   L      EEEE   M M M  PPPP     I    N N N   *~
            *  P      R   R  L      E      M   M  P        I    N  NN   *~
            *  P      R   R  LLLLL  EEEEE  M   M  P      IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLEMPIN - Employee Master File Maintenance Program.      *~
            *            This combines linear input and two tables of   *~
            *            information.                                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/13/80 ! ORIGINAL                                 ! BCW *~
            * 04/17/81 ! FIX SUPER INSERT                         ! TEM *~
            * 05/15/81 ! ADD WORKMANS' COMP TO EARNINGS RECORD    ! TEM *~
            * 06/23/81 ! NORMAL HOURS PER WEEK FROM DEFAULTS      ! TEM *~
            * 12/08/83 ! ALL KINDS OF STUFF (PREPARING FOR RELEAS)! HES *~
            * 03/12/86 ! Added Subroutine CDANPOST in Data Save.  ! LDJ *~
            * 03/26/86 ! Extended PRLDEPTF Record Length          ! HES *~
            * 08/18/86 ! Added Direct Deposit Fields.             ! LDJ *~
            * 09/01/86 ! More related to above, plus standards    ! HES *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 07/08/87 ! Fixed internal build of DEDXN recs (DctD)! HES *~
            * 01/08/88 ! Fixed bug in earnings insert logic       ! HES *~
            * 08/24/88 ! Now calculates Usual Amount when the Rate! RJM *~
            *          !   or Usual Units is changed.             !     *~
            *          !   Also Back-Calculates Usual Units when  !     *~
            *          !   Usual Amount is changed.               !     *~
            * 12/21/88 ! Add dates to Direct Deposit info and     ! ERN *~
            *          !   updating of PRLDDTIF w/ prenotificatns !     *~
            *          !   Also, condensed program so it would    !     *~
            *          !   compile again.                         !     *~
            * 04/10/89 ! Allow input of GOAL on deductions screen ! RJM *~
            * 05/31/89 ! Correct accrual checking before deleting !     *~
            *          !    earning or deduction type             ! GGO *~
            * 09/06/89 ! Fixed Default of cr & db Accts on super  ! MJB *~
            *          !  insert of deductions.                   !     *~
            * 06/19/90 ! Fixed PF2 (restart line) for deductions, ! JDH *~
            *          !  fixed defaults for deductions.          !     *~
            * 12/13/91 ! Relaxed test on State Status (1991 updts)! KAB *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 03/08/93 ! PRR 10474 - SuperInsert no longer allows ! MLJ *~
            *          !   you to insert an earnings code which   !     *~
            *          !   already exists in the master file.     !     *~
            *          ! PRR 12070 - Deleted call to "Employee    !     *~
            *          !   Defaults" at line # 9090.  Mods to     !     *~
            *          !   Default/Enables in conjunction with    !     *~
            *          !   this change.                           !     *~
            *          ! PRR 12541 - Can now specify "$" or "L"   !     *~
            *          !   type accounts for Cash In Bank.        !     *~
            * 08/26/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            arate$10,                    /* RATE ACCRUALS USE FOR ESTMT*/~
            autopay$1,                   /* AUTOMATIC PAYROLL FLAG     */~
            bankacct$(5)17,              /* CHECKING ACCOUNT NUMBER    */~
            bankcode$(5)4,               /* BANK I.D. CODE FOR DIRECTS */~
            bankdescr$(5)32,             /* BANK NAME FIELD            */~
            bankeff$(5,2)10,             /* DD Effective Dates         */~
            banknote$(5,2)8,             /* DD Last Prenotification    */~
            bankold$(5)23,               /* Old Bank Info              */~
            banktest$23,                 /* Searching argument         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cashacct$16,                 /* CASH IN BANK ACCOUNT       */~
            cashacctdescr$32,            /* CASH IN BANK ACCT DESCRIPTN*/~
            category$(18)6,              /* CATEGORIES FOR BUILD TABLES*/~
            checkkey$50,                 /* CHECK EARNINGS TYPES       */~
            columnhdr$(2)1,              /* UNUSED PLOWCODE ARGUMENT   */~
            cursor%(2),                  /* CURSOR COLUMN LOCATION     */~
            date$8,                      /* TODAY'S DATE SCREEN DISPLAY*/~
            ddamount(5),                 /* DIRECT DEPOSIT AMOUNTS     */~
            ddamount$(5)09,              /* DIRECT DEPOSIT AMOUNTS     */~
            ddheader$(2)79,              /* DIRECT DEPOSIT SCREEN HDR  */~
            dedamt(4),                   /* DEDUCTION AMOUNTS FOR READS*/~
            dedcat$(100)6,               /* DEDUCTION CATEGORIES       */~
            dedmethod$(100)6,            /* DEDUCTION METHODS          */~
            deductflag$1,                /* FLAG TO DO HIM IN PRLDDUCT */~
            deduction$(100)12,           /* DEDUCTION DESCRIPTIONS     */~
            dedempflag$(100)1,           /* DEDUCTION EMPLOYEE PAYS?   */~
            dedcracct$(100)16,           /* DEDUCTION CREDIT ACCOUNT   */~
            deddbacct$(100)16,           /* DEDUCTION DEBIT ACCOUNT    */~
            dedapplies$(100)6,           /* DEDUCTION APPLIES FIELD    */~
            deddescr$(4,100)15,          /* DEDUCTION AMOUNT DESCRIPTNS*/~
            dedamt$(4,100)12,            /* DEDUCTION AMOUNTS          */~
            dedgoal$(100)10,             /* DEDUCTION GOAL AMOUNTS     */~
            dedrecord$(100)184,          /* DEDUCTION MTD, QTD, YTD AMT*/~
            dfltype$12,                  /* DEDUCTION TYPE   (ERN)     */~
            dfldedmthod$6,               /* DEDUCTION METHOD (DED)     */~
            dept$4,                      /* DEPARTMENT CODE THIS GUY   */~
            deptdescr$32,                /* DEPARTMENT DESCRIPTION     */~
            dseq%(5),                    /* CONTROLS SEQ# ON DIRECT DEP*/~
            dtstamp$7,                   /* Date/Time Stamp            */~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            empname$55,                  /* FELLA'S NAME               */~
            erncata$(100)4,              /* EARNINGS CATEGORY   CODE   */~
            erntype$(100)12,             /* EARNINGS TYPE (DESCRIPTION)*/~
            erncash$(100)1,              /* EARNINGS PAYABLE IN CASH?  */~
            ernunits$(100)6,             /* EARNINGS UNITS DESCRIPTION */~
            ernrate$(100)10,             /* EARNINGS RATE              */~
            ernacct$(100)16,             /* EARNINGS ACCOUNT NUMBER    */~
            ernjunk1$(100)1,             /* 1 BYTE FREE                */~
            ernrecord$(100)80,           /* EARNINGS UNIT/AMT RECORDS  */~
            ernusunits$(100)10,          /* EARNINGS USUAL UNITS       */~
            ernusbucks$(100)10,          /* EARNINGS USUAL AMOUNT      */~
            ernsicka$(100)2,             /* EARNINGS SICK ACCRUAL FLAG */~
            ernvacat$(100)2,             /* EARNINGS VACATION ACRL FLAG*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(5,12)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            filler$7,                    /* FILLER                     */~
            fstatus$1,                   /* FEDERAL FILING STATUS      */~
            fstatusdescr$20,             /* STATUS DESCRIPTION         */~
            firstpaydate$8,              /* FIRST DATE PAY PERIOD      */~
            grossacct$16,                /* GROSS PAY ACCOUNT          */~
            grossacctdescr$32,           /* GROSS PAY ACCT DESCRIPTION */~
            header$79,                   /* Screen Title               */~
            i$(24)80,                    /* SCREEN IMAGE WORK AREA     */~
            incl_excl(2),                /* PLOWCODE ARGUMENT          */~
            incl_excl$(2)1,              /* PLOWCODE ARGUMENT          */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inscat$6,                    /* CATEGORY FOR SUPER-INSERT  */~
            jrate$10,                    /* Rate To Post Job At Extrnly*/~
            shift$1,                     /* Normal Shift               */~
            lastemp$12,                  /* LAST EMPLOYEE INPUT        */~
            lastpaydate$8,               /* LAST DATE THIS PAY PERIOD  */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            maxlines%(2),                /* MAX LINES FOR 2 TABLES     */~
            message$79,                  /* MESSAGE FOR INPUT          */~
            new$32,                      /* METHOD TO CHANGE TO DESCR  */~
            newcode$2,                   /* METHOD TO CHANGE TO        */~
            newdate$8,                   /* METHOD CHANGE DATE         */~
            normalhrs$2,                 /* NORMAL HOURS PER DAY       */~
            normweekhrs$2,               /* NORMAL HOURS PER WEEK      */~
            overacct$16,                 /* OVERHEAD ACCRUAL ACCOUNT   */~
            overacctdescr$32,            /* OVERHEAD ACCOUNT DESCRIPTIN*/~
            p%(1),                       /* SEARCH receiver            */~
            payfreq$1,                   /* PAY FREQUENCY (1-7)        */~
            payfreqdescr$32,             /* PAY FREQUENCY DESCRIPTION  */~
            paymode$1,                   /* HOW THIS GUY IS PAID       */~
            paymodedescr$32,             /* PAY MODE DESCRIPTION       */~
            pfkeys$(5)17,                /* FUNCTION KEYS ENABLED LISTS*/~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            primstate$2,                 /* PRIMARY STATE WORKING IN   */~
            readkey$50,                  /* READ KEY FOR FILE INFO     */~
            rest1$(100)32,               /* REST OF EARNINGS RECORD    */~
            search%(1),                  /* SEARCH RESULT              */~
            sick$32,                     /* SICK ACCRUAL METHOD DESCRIP*/~
            sickcode$2,                  /* SICK ACCRUAL METHOD        */~
            separator$(5)79,             /* SEPARATORS FOR TABLE INPUT */~
            sstatus$1,                   /* STATE FILING STATUS        */~
            sstatusdescr$32,             /* STATUS DESCRIPTION         */~
            statusdescr$32,              /* STATUS DESCRIPTION         */~
            surepay$(5)2,                /* SAVINGS OR CHECKING ACCOUNT*/~
            tempded(13),                 /* TEMP FOR CHECKING DED ACRLS*/~
            tempern(10),                 /* TEMP FOR CHECKING ERN ACRLS*/~
            tempamt$10, tempdfi$9,       /* Temp stuff                 */~
            tempname$22, temptr$2,       /*                            */~
            title$(4,2)64,               /* P.F. KEY DESCRIPTIONS      */~
            tran$(6)80,                  /* CURSOR=>FIELD TRANS TABLE  */~
            userid$3,                    /* USERID OF CURRENT USER     */~
            vacation$32,                 /* VACA ACCRUAL METHOD DESCRIP*/~
            vaccode$2,                   /* VACA ACCRUAL METHOD        */~
            wkcomdef$10,                 /* WORKMANS' COMP. DEFAULT    */~
            laborclas$4,                 /* LABOR CLASS CODE           */~
            labordescr$32                /* LABOR CLASS CODE DESCRIPTN */

        dim                      /* PERSONNEL STUFF                    */~
            lname$15,            /* Last name of person - part of pers */~
            fname$10,            /* First name of person               */~
            mname$1              /* Middle name of person              */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! System information file (payroll default)*~
            * # 2 ! GLMAIN   ! General ledger main account file.        *~
            * # 3 ! EMPMASTR ! Employee file master records.            *~
            * # 4 ! EMPEARN1 ! Employee earnings details file           *~
            * # 5 ! EMPDEDXN ! Employee deductions file.                *~
            * # 6 ! PRLDEPTF ! Payroll department code file             *~
            * # 7 ! EMPBANKS ! Employee direct deposit information      *~
            * # 8 ! GENCODES ! General Codes File                       *~
            * # 9 ! PRLBANKF ! Payroll bank code number file            *~
            * #10 ! PRLDDT   ! Payroll deduction definition table       *~
            * #11 ! PRLDDEFL ! Payroll deduction default file           *~
            * #12 ! PRLEDFLT ! Payroll earnings default file            *~
            * #13 ! PRLACRLS ! Vacation and sick accrual methods file   *~
            * #14 ! PERMASTR ! Personnel master file                    *~
            * #15 ! PRLDDTIF ! Direct Deposit Transaction File          *~
            * #20 ! USERINFO ! User information file                    *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #2,  "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #3,  "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #4,  "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alt key  1, keypos =  16, keylen = 28

            select #5,  "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alt key  1, keypos =  16, keylen = 18, dup

            select #6,  "PRLDEPTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 4

            select #7,  "EMPBANKS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 1, keylen = 13

            select #8,  "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  128,                                  ~
                        keypos =    1, keylen =   24

            select #9,  "PRLBANKF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 1, keylen = 4,                         ~
                         alt key 1, keypos = 5, keylen = 30, dup

            select #10, "PRLDDT",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 6

            select #11, "PRLDDEFL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 9

            select #12, "PRLEDFLT",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 1, keylen = 7

            select #13, "PRLACRLS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 2

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #15, "PRLDDTIF",                                      ~
                        varc, indexed, recsize = 108,                    ~
                        keypos = 102, keylen = 7,                        ~
                        alt key 1, keypos = 95, keylen = 14

            select #20, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

        call "SHOSTAT" ("Opening Files, One Moment Please")
            f1%(3%), f1%(4%), f1%(5%), f1%(7%) = 100%
            for x% = 1% to 14%
                call "OPENCHCK" (#x%, 0%, f2%(x%), f1%(x%), " ")
            next x%
            call "OPENCHCK" (#20, 0%, f2%(20), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~                  * SETS VALUES OF VARIABLES CRITICAL TO PROGRAM OPERATION.   *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            call "EXTRACT" addr ("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            ddheader$(1%)= "    Bank  Tran                      Deposit" ~
                      & "     Effective Period      Last"
            ddheader$(2%)=                                               ~
                        "   " & hex(ac) & "Code" & hex(8cac) & "Code"    ~
                      & hex(8cac) & "Bank Account Nmbr" & hex(8cac)      ~
                      & "  Amount  " & hex(ac) & "   From   " & hex(ac)  ~
                      & "    To    " & hex(8cac) & "Notice  " & hex(8c)
            title$(1%,1%)="(1)Start Over(2)Col 1(4)Line Above(13)Instrs(1~
        ~6)EDIT MODE"
            title$(2%,1%)="(1)Start Over(2)First(3)Last(4)Prev(5)Next(6)D~
        ~own(7)Up(9)Header"
            title$(2%,2%)="(10)Super-Ins(11)Ins(12)Del(13)Instrs(15)Prt S~
        ~crn (16)SAVE DATA"
            title$(3%,1%)="Supply Requested Items and RETURN or (1) to Ex~
        ~it Insert Mode"
            title$(4%,1%)="Press RETURN to DELETE Flashing Line or (1) To~
        ~ EXIT DELETE."

            pfkeys$(1%) = hex(000102040f10ffffffffffffffffffffff)
            pfkeys$(2%) = hex(0001020304050607090a0b0c0d0f10ffff)
            pfkeys$(3%) = hex(00010fffffffffffffffffffffffffffff)
            pfkeys$(4%) = hex(00010fffffffffffffffffffffffffffff)

        REM Set up first half of TRAN$() for edit earnings table...
                init(hex(00)) tran$()
                init(hex(01)) str(tran$( 1%),  1%)
                init(hex(02)) str(tran$( 1%), 11%)
                init(hex(03)) str(tran$( 1%), 29%)
                init(hex(04)) str(tran$( 1%), 37%)
                init(hex(05)) str(tran$( 1%), 58%)
                init(hex(06)) str(tran$( 2%),  1%)
                init(hex(07)) str(tran$( 2%), 24%)
                init(hex(08)) str(tran$( 2%), 47%)
                init(hex(09)) str(tran$( 3%),  1%)
                init(hex(0a)) str(tran$( 3%), 22%)

        REM Set up second half of TRAN$() for edit deductions...
                init(hex(01)) tran$(4%)
                init(hex(02)) str(tran$(4%), 13%)
                init(hex(03)) str(tran$(4%), 25%)
                init(hex(04)) str(tran$(4%), 46%)
                init(hex(05)) tran$(5%)
                init(hex(06)) str(tran$(5%), 31%)
                init(hex(07)) str(tran$(5%), 59%)
                init(hex(08)) tran$(6%)
                init(hex(09)) str(tran$(6%), 17%)
                init(hex(0a)) str(tran$(6%), 33%)
                init(hex(0b)) str(tran$(6%), 49%)
                init(hex(0c)) str(tran$(6%), 65%)

        REM See if ACH Tape sub-module is active...
            call "READ100" (#1, "PRLDD.INFORMATION", tape%)
            if tape% = 1% then                                           ~
                           call "OPENCHCK" (#15, 0%, f2%(15%), 200%, " ")

        REM *************************************************************~
            *            I N P U T   M A I N   P R O G R A M            *~
            *-----------------------------------------------------------*~
            * Inputs header information.  Note the STRTRLSE call at top.*~
            * note also at top ERNRECORD$() and DEDRECORD$().  These    *~
            * are the accumulators for MTD, QTD, YTD, and CURRENT       *~
            * earnings and deduction amounts.  Be CAREFUL of them!!     *~
            *************************************************************

        inputmode
              errormsg$, infomsg$, primstate$, ernvacat$(), ddamount$(), ~
              dept$, deptdescr$, vaccode$, empcode$, laborclas$,         ~
              labordescr$, bankeff$(),          dedmethod$(), sstatus$,  ~
              statusdescr$, sstatusdescr$, payfreq$, ernjunk1$(),        ~
              payfreqdescr$, paymode$, paymodedescr$, bankcode$(), sick$,~
              bankdescr$(), bankacct$(), cashacct$, autopay$, vacation$, ~
              cashacctdescr$, grossacct$, grossacctdescr$, dedapplies$(),~
              normalhrs$, firstpaydate$, lastpaydate$, overacctdescr$,   ~
              normweekhrs$, wkcomdef$, filler$, dedgoal$(), fstatus$,    ~
              erncata$(), erntype$(), ernacct$(), ernunits$(), dedamt$(),~
              ernrate$(), ernusunits$(), ernusbucks$(), arate$, empname$,~
              erncash$(), ernsicka$(), sickcode$, dedcat$(), overacct$,  ~
              deduction$(), deddescr$(),  deddbacct$(), fstatusdescr$,   ~
              dedempflag$(), dedcracct$(), new$, newcode$, newdate$,     ~
              jrate$, shift$, surepay$(), banknote$() = " "

            init(hex(00)) ernrecord$(), dedrecord$(), rest1$()
            deductflag$ = "N"
            change%, editmode% = 0%

            call "ALLFREE"

L10340:     for fieldnr% = 1% to 12%
                gosub enable_161
                      if enabled% =  0% then L10500
L10370:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10450
L10400:                  fieldnr% = fieldnr% - 1%
                         if fieldnr% < 1% then L10340
                         gosub enable_161
                         if enabled% <> 0% then L10370
                         goto L10400
L10450:               if keyhit%  =  8% then gosub show_personnel_stuff
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10370
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10370
L10500:         next fieldnr%

L10520:     for fieldnr% = 1% to 10%
                gosub enable_162
                      if enabled% =  0% then L10670
L10550:         gosub'202(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10630
L10580:                  fieldnr% = fieldnr% - 1%
                         if fieldnr% < 1% then L10520
                         gosub enable_162
                         if enabled% <> 0% then L10550
                         goto L10580
L10630:               if keyhit%  =  8% then gosub show_personnel_stuff
                      if keyhit% <>  0% then       L10550
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10550
L10670:         next fieldnr%

L10690:     for fieldnr% = 1% to 5%
                gosub'163(fieldnr%)
                      if enabled% =  0% then L10840
L10720:         gosub'203(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10800
L10750:                  fieldnr% = fieldnr% - 1%
                         if fieldnr% < 1% then L10690
                         gosub'163(fieldnr%)
                         if enabled% <> 0% then L10720
                         goto L10750
L10800:               if keyhit%  =  8% then gosub show_personnel_stuff
                      if keyhit% <>  0% then       L10720
                gosub L59000
                      if errormsg$ <> " " then L10720
L10840:     next fieldnr%

        REM *************************************************************~
            *          I N P U T   E A R N I N G S   T A B L E          *~
            *-----------------------------------------------------------*~
            * Input earnings table.  First, show screen and get the     *~
            * names of the catagories  he gets his earnings from.  Then *~
            * load them into the table and go from there.               *~
            *************************************************************

        REM INPUT EARNINGS TYPES TO BUILD TABLE FROM...
                errormsg$, infomsg$, category$() = " "
L11100:         gosub L45000
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L11100
                gosub L55000              /* TEST DATA FOR ALL OK       */
                      if errormsg$ <> " " then L11100

        REM Now build earnings table from above...
                c% = 0%
                for subscript% = 1% to 18%
                    if category$(subscript%) = " " then L11260
                       readkey$ = category$(subscript%)
L11210:                call "PLOWNEXT" (#12, readkey$, 4%, f1%(12%))
                            if f1%(12%) = 0% then L11260
                       c% = c% + 1%
                       gosub L32000       /* load & format earnings ent */
                       goto L11210
L11260:         next subscript%
                maxlines%(1) = c%

        REM Now input mode for the line items...
                line%, screenline% = 0%
                infomsg$, errormsg$ = " "
                if maxlines%(1%) = 0% then L11590   /* If none in table */

L11340:         screenline% = screenline% + 1%
                if screenline% < 6% then L11380
                   line% = line% + 5%
                   screenline% = 1%
L11380:         currentline%, c% = line% + screenline%
                if currentline% > maxlines%(1%) then L11550
                call "SETSEP"(separator$(), line%, screenline%)

                for fieldnr% = 1% to 10%
                    gosub'164(fieldnr%)
                          if enabled% =  0% then       L11520
L11450:             gosub'204(screenline%, fieldnr%)
                          if keyhit%  =  0% then       L11500
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  =  4% then gosub elineabove
                          goto L11450
L11500:             gosub'154(fieldnr%)
                          if errormsg$ <> " " then L11450
L11520:         next fieldnr%
                goto L11340

L11550: REM Now do regular input mode for more line items...
                screenline% = screenline% - 1%
                infomsg$ = "Now You Can Input Additional Earnings Info."

L11590:         screenline% = screenline% + 1%
                if screenline% < 6% then L11630
                   line% = line% + 5%
                   screenline% = 1%
L11630:         currentline%, c% = line% + screenline%
                if currentline% > 100% then L12000
                call "SETSEP"(separator$(), line%, screenline%)

L11670:         for fieldnr% = 1% to 10%
                    gosub'164(fieldnr%)
                          if enabled% =  0% then       L11790
L11700:             gosub'204(screenline%, fieldnr%)
                          if keyhit%  =  0% then       L11770
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  =  2% then gosub ecolumnone
                          if keyhit%  =  4% then gosub elineabove
                          if keyhit%  = 16% and fieldnr% = 1% then L11830
                          goto L11700
L11770:             gosub'154(fieldnr%)
                          if errormsg$ <> " " then L11700
L11790:         next fieldnr%
                maxlines%(1%) = maxlines%(1%) + 1%
                goto L11590

L11830: REM Zap the current field for this guy...
                erncata$(c%) = " "

L12000: REM *************************************************************~
            *        I N P U T   D E D U C T I O N S   T A B L E        *~
            *-----------------------------------------------------------*~
            * Inputs deductions table, using a scheme similar to the    *~
            * input earnings table setup above.                         *~
            *************************************************************

        REM Input deductions categories to build table from...
                errormsg$, infomsg$, category$() = " "
L12090:         gosub L45315
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L12090
                gosub L56000              /* Test data for all ok       */
                      if errormsg$ <> " " then L12090

        REM Now build earnings table from above...
                c% = 0%
                for subscript% = 1% to 18%
                    if category$(subscript%) = " " then L12250
                       readkey$ = category$(subscript%)
L12200:                call "PLOWNEXT" (#11, readkey$, 6%, f1%(11%))
                            if f1%(11%) = 0% then L12250
                       c% = c% + 1%
                       gosub L33000       /* Load & format earnings ent */
                       goto L12200
L12250:         next subscript%
                maxlines%(2%) = c%

        REM Now input mode for the line items...
                line%, screenline% = 0%
                infomsg$, errormsg$ = " "
                if maxlines%(2%) = 0% then L12580   /* If none in table */

L12330:         screenline% = screenline% + 1%
                if screenline% < 5% then L12370
                   line% = line% + 4%
                   screenline% = 1%
L12370:         currentline%, c% = line% + screenline%
                if currentline% > maxlines%(2%) then L12540
                call "SETSEP"(separator$(), line%, screenline%)

                ins% = 1%
                for fieldnr% = 1% to 12%
                    gosub L24000
                          if enabled% =  0% then       L12510
L12440:             gosub'205(screenline%, fieldnr%)
                          if keyhit%  =  0% then       L12490
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  =  4% then gosub dlineabove
                          goto L12440
L12490:             gosub L54000
                          if errormsg$ <> " " then L12440
L12510:         next fieldnr%
                ins% = 0%
                goto L12330

L12540: REM Now input mode for additional line items...
                screenline% = screenline% - 1%
                infomsg$ = "Now You Can Input Additional Deduction Info."

L12580:         screenline% = screenline% + 1%
                if screenline% < 5% then L12620
                   line% = line% + 4%
                   screenline% = 1%
L12620:         currentline%, c% = line% + screenline%
                if currentline% > 100% then L13000             /* EDIT   */
                call "SETSEP"(separator$(), line%, screenline%)

                ins% = 1%
L12660:         for fieldnr% = 1% to 12%
                    gosub L24000
                          if enabled% =  0% then       L12780
L12690:             gosub'205(screenline%, fieldnr%)
                          if keyhit%  =  0% then       L12760
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  =  2% then gosub dcolumnone
                          if keyhit%  =  4% then gosub dlineabove
                          if keyhit%  = 16% and fieldnr% = 1% then L12820
                          goto L12690
L12760:             gosub L54000
                          if errormsg$ <> " " then L12690
L12780:         next fieldnr%
                ins% = 0%
                maxlines%(2%) = maxlines%(2%) + 1%
                goto L12580

L12820: REM Zap current line's field one before edit...
                    dedcat$(c%) = " "

L13000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Edit mode main program edits linear information.          *~
            *************************************************************

        editmode
            init(" ") errormsg$, infomsg$
            editmode% = 1%

L13100: REM EDTPG1...
            gosub edit_message
            gosub'211(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       edtearnings
                  if keyhit%  =  3% then       edtdedxns
                  if keyhit%  =  5% then       edtpg2
                  if keyhit%  =  8% then gosub show_personnel_stuff
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L13100
            oldfieldnr% = 0%
L13220:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 2% or fieldnr% > 12% then L13100
            if fieldnr% = oldfieldnr% then L13100
            oldfieldnr% = fieldnr%

            gosub enable_161
                  if enabled% =  0% then L13100
L13290:     gosub'211(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then       L13290
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L13290
            goto L13220

        edit_message
            message$ = "To Modify Displayed Values, Position Cursor To "&~
                       "Desired Value And Press RETURN."
            return

L13360: edtpg2
            gosub edit_message
            gosub'212(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       edtearnings
                  if keyhit%  =  3% then       edtdedxns
                  if keyhit%  =  4% then       L13100
                  if keyhit%  =  5% then       edtpg3
                  if keyhit%  =  8% then gosub show_personnel_stuff
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L13360
            oldfieldnr% = 0%
L13490:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 10% then L13360
            if fieldnr% = oldfieldnr% then L13360
            oldfieldnr% = fieldnr%

            gosub enable_162
                  if enabled% =  0% then L13360
L13560:     gosub'212(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then       L13560
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L13560
            goto L13490

L13630: edtpg3
            gosub edit_message
            gosub'213(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       edtearnings
                  if keyhit%  =  3% then       edtdedxns
                  if keyhit%  =  4% then       L13360
                  if keyhit%  =  8% then gosub show_personnel_stuff
                  if keyhit%  = 16% then       datasave
            oldfieldnr% = 0%
L13740:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 5% then L13630
            if fieldnr% = oldfieldnr% then L13630
            oldfieldnr% = fieldnr%

            gosub'163(fieldnr%)
                  if enabled% =  0% then L13630
L13810:     gosub'213(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then       L13810
            gosub L59000
                  if errormsg$ <> " " then L13810
            goto L13740

        REM *************************************************************~
            *           E D I T   E A R N I N G S   T A B L E           *~
            *-----------------------------------------------------------*~
            * Edits earnings table. Also has super-insert code for      *~
            * earnings.  This is like insert except it gets a lot of    *~
            * lines off the disk.                                       *~
            *************************************************************

        edtearnings
            line%, currentline%, screenline% = 0%
            call"SETSEP"(separator$(),line%,min(maxlines%(1%)-line%,5%))

L14120:     errormsg$ = " "
L14130:     gosub'214(0%, 0%)
                  if keyhit%  =  0% then       L14330
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then line% = 0%
                  if keyhit%  =  3% then line% = max(0%,maxlines%(1%)-5%)
                  if keyhit%  =  4% then line% = max(0%,line%-4%)
                  if keyhit%  =  5% then line% = min(line%+4%,max(0%,    ~
                                                maxlines%(1%)-5%))
                  if keyhit%  =  6% then line% = max(0%,line%-1%)
                  if keyhit%  =  7% then line% = min(line%+1%,max(0%,    ~
                                                maxlines%(1%)-5%))
                  if keyhit%  =  9% then       editmode
                  if keyhit%  = 10% then gosub esuperinsert
                  if keyhit%  = 11% then gosub einsertmode
                  if keyhit%  = 12% then gosub edeletemode
                  if keyhit%  = 16% then       datasave
                  call "SETSEP" (separator$(), line%, min(maxlines%(1%)  ~
                                                        - line%, 5%))
                  goto L14120

L14330: REM Now figure out which field he hit....
                temp% = cursor%(1%) - 5%
                if temp% < 1% then L14120
                temp% = mod(temp%+4%,4%)
                if temp% = 0% then L14120
                fieldnr% = val(str(tran$(temp%),cursor%(2%)))
                if fieldnr% = 0% then L14120
                screenline% = (cursor%(1%)-5%)/4%+1%
                c%, currentline% = line% + screenline%
                if currentline% > maxlines%(1%) then L14120

                if fieldnr% <> 8% then L14510
                   if ernusunits$(c%) = " " then L14510
                   if ernrate$(c%) = " " then L14510
                      convert ernrate$(c%) to ernrate
                      convert ernusunits$(c%) to ernusunits
                      call "CONVERT" (round(ernusunits * ernrate, 2%),   ~
                                                   -2.2, ernusbucks$(c%))
L14510:         gosub'214(screenline%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then L14510
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L14510
                goto L14130

        REM *************************************************************~
            *         E D I T   D E D U C T I O N S   T A B L E         *~
            *-----------------------------------------------------------*~
            * Edits deductions table.  Also deductions super-insert code*~
            * The only tricky worth noting here is that we use the same *~
            * TRAN$() as the earnings table, except that we add 24 to   *~
            * CURSOR%(1) so that we use the upper half of the array.    *~
            * I didn't want to use a 2-d array here since I thought it  *~
            * would make the subscripting of TRAN$() too difficult.     *~
            * Oh, yes, we have an edit enable (see l13000, PRLDEDXN)    *~
            *************************************************************

        edtdedxns
            line%, currentline%, screenline% = 0%
            call"SETSEP"(separator$(),line%,min(maxlines%(2%)-line%,5%))

L14750:     errormsg$ = " "
L14760:     gosub'215(0%, 0%)
                  if keyhit%  =  0% then       L14960
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then line% = 0%
                  if keyhit%  =  3% then line% = max(0%,maxlines%(2%)-4%)
                  if keyhit%  =  4% then line% = max(0%,line%-3%)
                  if keyhit%  =  5% then line% = min(line%+4%,max(0%,    ~
                                                maxlines%(2%)-4%))
                  if keyhit%  =  6% then line% = max(0%,line%-1%)
                  if keyhit%  =  7% then line% = min(line%+1%,max(0%,    ~
                                                maxlines%(2%)-4%))
                  if keyhit%  =  9% then       editmode
                  if keyhit%  = 10% then gosub dsuperinsert
                  if keyhit%  = 11% then gosub dinsertmode
                  if keyhit%  = 12% then gosub ddeletemode
                  if keyhit%  = 16% then       datasave
                  call "SETSEP" (separator$(), line%, min(maxlines%(2%)  ~
                                                            - line%, 4%))
                  goto L14750

L14960: REM Now figure out which field he hit...
                temp% = cursor%(1%)-5%
                if temp% < 1% then L14750
                temp% = min(mod(temp%+5%,5%), 3%)
                if temp% = 0% then L14750
                fieldnr% = val(str(tran$(temp%+3%),cursor%(2%)))
                if fieldnr% = 0% then L14750
                screenline% = (cursor%(1%)-5%)/5%+1%
                c%, currentline% = line% + screenline%
                if currentline% > maxlines%(2%) then L14750
                if dedcat$(c%) = "SYSTEM" then L14750
                   if fieldnr% < 8% or fieldnr% > 11% then L15100
                      if deddescr$(fieldnr%-7%,c%) = " " then L14750

L15100:         gosub'215(screenline%, fieldnr%)
                      if keyhit%  = 1% then gosub startover
                      if keyhit% <> 0% then L15100
                gosub L54000
                      if errormsg$ <> " " then L15100
                goto L14760

        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *-----------------------------------------------------------*~
            * Handles column one and line above logic for both tables.  *~
            *************************************************************

        ecolumnone
            gosub clear_eline
            return clear all
            goto L11670

        elineabove
            if c% = 1% then return
               on fieldnr% gosub L15410,            /* CATEGORY   CODE  */~
                                 L15420,            /* TYPE CODE        */~
                                 L15430,            /* PAID IN CASH?    */~
                                 L15450,            /* UNITS DESCRIPTION*/~
                                 L15460,            /* RATE PER UNIT    */~
                                 L15470,            /* DEBIT ACCOUNT #  */~
                                 L15480,            /* USUAL UNITS      */~
                                 L15490,            /* USUAL AMOUNT     */~
                                 L15500,            /* SICK ACCRUAL FLAG*/~
                                 L15510             /* SICK ACCRUAL FLAG*/
                  return
L15410:     erncata$   (c%) = erncata$   (c%-1%) : return
L15420:     erntype$   (c%) = erntype$   (c%-1%) : return
L15430:     erncash$   (c%) = erncash$   (c%-1%) : return
            ernjunk1$  (c%) = ernjunk1$  (c%-1%) : return
L15450:     ernunits$  (c%) = ernunits$  (c%-1%) : return
L15460:     ernrate$   (c%) = ernrate$   (c%-1%) : return
L15470:     ernacct$   (c%) = ernacct$   (c%-1%) : return
L15480:     ernusunits$(c%) = ernusunits$(c%-1%) : return
L15490:     ernusbucks$(c%) = ernusbucks$(c%-1%) : return
L15500:     ernsicka$  (c%) = ernsicka$  (c%-1%) : return
L15510:     ernvacat$  (c%) = ernvacat$  (c%-1%) : return

        dcolumnone
            gosub clear_dline
            return clear all
            goto L12660

        dlineabove
            if c% = 1% then return
               on fieldnr% gosub L15730,            /* DEDXN CATEGORY   */~
                                 L15740,            /* DEDUCTION METHOD */~
                                 L15750,            /* DEDUCTION DESCR  */~
                                 L15760,            /* EMPLOYEE PAYS IT?*/~
                                 L15770,            /* CREDIT ACCOUNT   */~
                                 L15780,            /* DEBIT ACCOUNT    */~
                                 L15790,            /* APPLIES FIELD    */~
                                 L15800,            /* AMOUNT 1 FIELD   */~
                                 L15810,            /* AMOUNT 2 FIELD   */~
                                 L15820,            /* AMOUNT 3 FIELD   */~
                                 L15830,            /* AMOUNT 4 FIELD   */~
                                 L15840             /* GOAL FIELD       */
                  return
L15730:     dedcat$    (c%)  = dedcat$     (c%-1%) : return
L15740:     dedmethod$ (c%)  = dedmethod$  (c%-1%) : return
L15750:     deduction$ (c%)  = deduction$  (c%-1%) : return
L15760:     dedempflag$(c%)  = dedempflag$ (c%-1%) : return
L15770:     dedcracct$ (c%)  = dedcracct$  (c%-1%) : return
L15780:     deddbacct$ (c%)  = deddbacct$  (c%-1%) : return
L15790:     dedapplies$(c%)  = dedapplies$ (c%-1%) : return
L15800:     dedamt$  (1%,c%) = dedamt$  (1%,c%-1%) : return
L15810:     dedamt$  (2%,c%) = dedamt$  (2%,c%-1%) : return
L15820:     dedamt$  (3%,c%) = dedamt$  (3%,c%-1%) : return
L15830:     dedamt$  (4%,c%) = dedamt$  (4%,c%-1%) : return
L15840:     dedgoal$    (c%) = dedgoal$    (c%-1%) : return

        REM *************************************************************~
            *    I N S E R T / D E L E T E   F O R   E A R N I N G S    *~
            *-----------------------------------------------------------*~
            * Insert and delete mode code for the earnings table is in  *~
            * this section.  It's fairly standard code.                 *~
            *************************************************************

L15930: einsertmode
            if maxlines%(1%) >= 100% then return
               screenline% = int((cursor%(1%)-1%)/4%)
               if line% + screenline% < maxlines%(1%) then L15980
                  screenline% = maxlines%(1%) - line%        /* AT END */
L15980:        if screenline% <> 5% then L16030     /* BOTTOM OF PAGE   */
                  line% = line% + 1%
                  screenline% = screenline% - 1%
                  goto L16030

L16030:     call "SETSEP" (separator$(), line%, min(5%,                  ~
                                         maxlines%(1%) - line% + 1%))
            currentline%, c% = screenline% + line%
            gosub L16740                  /* ROLL ARRAYS UP ONE LINE.   */

        REM Now input the line, make so we can cancel out if necessary...
                infomsg$ = " "
                for fieldnr% = 1% to 10%
                    gosub'164(fieldnr%)
                          if enabled% =  0%  then L16180
L16130:             gosub'224(screenline%, fieldnr%)
                          if keyhit%  =  1% then L16240
                          if keyhit% <>  0% then L16130
                    gosub'154(fieldnr%)
                          if errormsg$ <> " " then L16130
L16180:         next fieldnr%

                maxlines%(1%) = maxlines%(1%) + 1%
                goto L15930

L16240: REM This routine aborts insert mode and destroys SCREENLINE%...
                gosub L16980              /* Actually delete @ C%      */

                temp% = c%
                c% = maxlines%(1%) + 1%
                gosub clear_eline
                c% = temp%

            if currentline% >= maxlines%(1%) and screenline% = 5%        ~
               then line% = max(0%, maxlines%(1%) - 5%)
            return

        edeletemode
        REM Figure out where we are on screen and return if invalid...
                if maxlines%(1%) = 0% then return
                screenline% = int((cursor%(1%)-1%)/4%)
                if screenline% < 1% then return
                c%, currentline% = screenline% + line%
                if currentline% > maxlines%(1%) then return

                get ernrecord$(c%), using L16450, tempern() /*  ACCRUALS */
L16450:         FMT 10 * PD(14,4)                          /*   MUST BE */
                u3% = 0%                                   /*   ZERO TO */
                if max(tempern()) > .009 then u3% = 1%   /* DELETE LINE */
                if min(tempern()) < -.009 then u3% = 1%
                if u3% = 0% then L16550
                errormsg$ = "EARNTYPE'S Accruals MUST Be Zeroed Before "&~
                            "It Can Be Deleted"
                return clear
                goto L14130

L16550: REM Now show delete screen with line flashing...
L16560:         gosub'234(screenline%)
                      if keyhit%  =  1% then       return
                      if keyhit% <>  0% then       L16560

        REM Now that he's approved it, delete it...
                c% = currentline%
                if currentline% < maxlines%(1%) then gosub L16980
                                         /* Actually delete line @ C%  */
                temp% = c%
                c% = maxlines%(1%)
                gosub clear_eline
                c% = temp%

                maxlines%(1%) = maxlines%(1%) - 1%
                if screenline% > maxlines%(1) - 5%                       ~
                   then line% = max(0%, maxlines%(1%)-5%)
                return

L16740: REM ***** SUBSIDIARY ROUTINE -- Copy all the elements up one...
                if c% >= maxlines%(1%) then L16920
                for temp% = maxlines%(1%) to c% step -1%
                    erncata$   (temp%+1%) = erncata$   (temp%)
                    erntype$   (temp%+1%) = erntype$   (temp%)
                    erncash$   (temp%+1%) = erncash$   (temp%)
                    ernjunk1$  (temp%+1%) = ernjunk1$  (temp%)
                    ernunits$  (temp%+1%) = ernunits$  (temp%)
                    ernrate$   (temp%+1%) = ernrate$   (temp%)
                    ernacct$   (temp%+1%) = ernacct$   (temp%)
                    ernusunits$(temp%+1%) = ernusunits$(temp%)
                    ernusbucks$(temp%+1%) = ernusbucks$(temp%)
                    ernrecord$ (temp%+1%) = ernrecord$ (temp%)
                    ernsicka$  (temp%+1%) = ernsicka$  (temp%)
                    ernvacat$  (temp%+1%) = ernvacat$  (temp%)
                    rest1$     (temp%+1%) = rest1$     (temp%)
                next temp%

L16920:         screenline% = screenline% + 1%
                c%, currentline% = currentline% + 1%
                gosub clear_eline
                errormsg$, infomsg$ = " "
                return

L16980: REM ***** SUBSIDIARY ROUTINE -- Roll down from C% to end...
                for temp% = currentline% to maxlines%(1%)
                    erncata$   (temp%) = erncata$   (temp%+1%)
                    erntype$   (temp%) = erntype$   (temp%+1%)
                    erncash$   (temp%) = erncash$   (temp%+1%)
                    ernjunk1$  (temp%) = ernjunk1$  (temp%+1%)
                    ernunits$  (temp%) = ernunits$  (temp%+1%)
                    ernrate$   (temp%) = ernrate$   (temp%+1%)
                    ernacct$   (temp%) = ernacct$   (temp%+1%)
                    ernusunits$(temp%) = ernusunits$(temp%+1%)
                    ernusbucks$(temp%) = ernusbucks$(temp%+1%)
                    ernrecord$ (temp%) = ernrecord$ (temp%+1%)
                    ernsicka$  (temp%) = ernsicka$  (temp%+1%)
                    ernvacat$  (temp%) = ernvacat$  (temp%+1%)
                    rest1$     (temp%) = rest1$     (temp%+1%)
                next temp%
                return

        REM *************************************************************~
            *  I N S E R T / D E L E T E   F O R   D E D U C T I O N S  *~
            *-----------------------------------------------------------*~
            * Insert and delete code for the deductions table.          *~
            *************************************************************

L17220: dinsertmode
            if maxlines%(2%) >= 100% then return   /* at end of docmnt */
               screenline% = int((cursor%(1%)-1%)/5%)
               if line% + screenline% < maxlines%(2%) then L17270
                  screenline% = maxlines%(2%) - line%
L17270:        if screenline% <> 4% then L17320      /* bottom of page   */
                  line% = line% + 1%
                  screenline% = screenline% - 1%
                  goto L17320

L17320:     call "SETSEP" (separator$(), line%, min(4%,                  ~
                                             maxlines%(2%) - line% + 1%))
            currentline%, c% = screenline% + line%
            gosub L18220                  /* roll arrays up one line.   */

        REM Now input the line, make so we can cancel out if required...
                infomsg$ = " "
                ins% = 1%
                for fieldnr% = 1% to 12%
                    gosub L24000
                          if enabled% = 0% then L17470
L17420:             gosub'225(screenline%, fieldnr%)
                          if keyhit%  =  1% then L17520
                          if keyhit% <>  0% then L17420
                    gosub L54000
                          if errormsg$ <> " " then L17420
L17470:         next fieldnr%
                maxlines%(2%) = maxlines%(2%) + 1%
                ins% = 0%
                goto L17220

L17520: REM This routine aborts insert mode and destroys SCREENLINE%...
                gosub L18570              /* actually delete @ C%      */

                temp% = c%
                c% = maxlines%(2%) + 1%
                gosub clear_dline
                c% = temp%

            if currentline% >= maxlines%(2%) and screenline% = 4%        ~
               then line% = max(0%, maxlines%(2%) - 4%)
            return

        ddeletemode
        REM Figure out where we are on screen and return if invalid...
                if maxlines%(2%) = 0% then return
                screenline% = int((cursor%(1%)-1%)/5%)
                if screenline% < 1% then return
                c%, currentline% = screenline% + line%
                if currentline% > maxlines%(2%) then return

                get dedrecord$(c%), using L17730, tempded() /*  Accruals */
L17730:         FMT 13 * PD(14,4)                          /*   must be */
                u3% = 0%                                   /*   zero to */
                tempded(1%), tempded(2%) = 0             /* DELETE line */
                if max(tempded()) > .009 then u3% = 1%
                if min(tempded()) < -.009 then u3% = 1%

                if u3% = 0% then L17840
                errormsg$ = "DEDUCTION'S Accruals Must Be Zeroed Before"&~
                            " It Can Be Deleted"
                return clear
                goto L14760

L17840: REM Now show delete screen with line flashing...
L17850:         gosub'235(screenline%)
                      if keyhit%  =  1% then       return
                      if keyhit% <>  0% then       L17850

        REM Now that he's approved it, delete it...
                c% = currentline%
                if currentline% < maxlines%(2%) then gosub L18570
                                         /* Actually delete line @ C%  */
                temp% = c%
                c% = maxlines%(2%)
                gosub clear_dline
                c% = temp%

                maxlines%(2%) = maxlines%(2%) - 1%
                if screenline% > maxlines%(2%) - 4%                      ~
                   then line% = max(0%, maxlines%(2%)-4%)
                return

L18220: REM ***** SUBSIDIARY ROUTINE -- Copy all the elements up one...
                if c% >= maxlines%(2%) then L18440
                for temp% = maxlines%(2%) to c% step -1%
                    dedcat$     (temp%+1%) = dedcat$     (temp%)
                    dedmethod$  (temp%+1%) = dedmethod$  (temp%)
                    deduction$  (temp%+1%) = deduction$  (temp%)
                    dedempflag$ (temp%+1%) = dedempflag$ (temp%)
                    dedcracct$  (temp%+1%) = dedcracct$  (temp%)
                    deddbacct$  (temp%+1%) = deddbacct$  (temp%)
                    dedapplies$ (temp%+1%) = dedapplies$ (temp%)
                    deddescr$(1%,temp%+1%) = deddescr$(1%,temp%)
                    deddescr$(2%,temp%+1%) = deddescr$(2%,temp%)
                    deddescr$(3%,temp%+1%) = deddescr$(3%,temp%)
                    deddescr$(4%,temp%+1%) = deddescr$(4%,temp%)
                    dedamt$  (1%,temp%+1%) = dedamt$  (1%,temp%)
                    dedamt$  (2%,temp%+1%) = dedamt$  (2%,temp%)
                    dedamt$  (3%,temp%+1%) = dedamt$  (3%,temp%)
                    dedamt$  (4%,temp%+1%) = dedamt$  (4%,temp%)
                    dedgoal$    (temp%+1%) = dedgoal$    (temp%)
                    dedrecord$  (temp%+1%) = dedrecord$  (temp%)
                    next temp%

L18440:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1
                gosub clear_dline
                return

L18570: REM ***** SUBSIDIARY ROUTINE -- Roll down from C% to end...
                for temp% = currentline% to maxlines%(2)
                    dedcat$    (temp%) = dedcat$    (temp%+1)
                    dedmethod$ (temp%) = dedmethod$ (temp%+1)
                    deduction$ (temp%) = deduction$ (temp%+1)
                    dedempflag$(temp%) = dedempflag$(temp%+1)
                    dedcracct$ (temp%) = dedcracct$ (temp%+1)
                    deddbacct$ (temp%) = deddbacct$ (temp%+1)
                    dedapplies$(temp%) = dedapplies$(temp%+1)
                    deddescr$(1,temp%) = deddescr$(1,temp%+1)
                    deddescr$(2,temp%) = deddescr$(2,temp%+1)
                    deddescr$(3,temp%) = deddescr$(3,temp%+1)
                    deddescr$(4,temp%) = deddescr$(4,temp%+1)
                    dedamt$  (1,temp%) = dedamt$  (1,temp%+1)
                    dedamt$  (2,temp%) = dedamt$  (2,temp%+1)
                    dedamt$  (3,temp%) = dedamt$  (3,temp%+1)
                    dedamt$  (4,temp%) = dedamt$  (4,temp%+1)
                    dedgoal$   (temp%) = dedgoal$   (temp%+1)
                    dedrecord$ (temp%) = dedrecord$ (temp%+1)
                next temp%
                return

        REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *-----------------------------------------------------------*~
            * Writes data to file.                                      *~
            * Note that we need to have a slight departure from standard*~
            * here in that we cannot delete the old employee record here*~
            * Since we lose hold on the old employee master once we go  *~
            * to delete the line items and we might have someone else   *~
            * out to get the record, in which case we get nuked.  The   *~
            * delete routine is buried in the middle of the write sub.  *~
            *************************************************************

        datasave
            gosub L31000
            lastemp$ = empcode$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for page 1 of the screen *~
            *************************************************************

            enable_161
                  message$ = " "
                  enabled% = 1%
                  on fieldnr% goto  L20220,         /* EMPLOYEE CODE    */~
                                    L20260,         /* FED FILE STAT    */~
                                    L20290,         /* STATE FILE STAT  */~
                                    L20340,         /* PAY FREQ         */~
                                    L20430,         /* PAY MODE         */~
                                    L20520,         /* CASH ACCOUNT#    */~
                                    L20620,         /* GROSS PAYRLL ACCT*/~
                                    L20720,         /* PRIMARY DEPARTMEN*/~
                                    L20750,         /* PRIMARY STATE    */~
                                    L20780,         /* NRML HRS PER DAY */~
                                    L20830,         /* NRML HRS PER WEEK*/~
                                    L20880          /* AUTO PAY FLAG    */

L20220: REM DEFAULT/ENABLE FOR EMPLOYEE CODE...
                message$ = "Enter Employee Code Or Leave Blank To Search."
                return

L20260: REM DEFAULT/ENABLE FOR FEDERAL FILING STAT...
                message$ = "Enter Federal Filing Status 'S'ingle or 'M'"&~
                           "arried."
                return

L20290: REM DEFAULT/ENABLE FOR STATE FILING STAT...
                if sstatus$ = " " then sstatus$ = fstatus$
                message$ = "Enter State Filing Status (S, M, H, J or A)."
                return

L20340: REM DEFAULT/ENABLE FOR PAY FREQUENCY...
                message$ = "Enter Pay Frequency (1, 2, 3, 4, 5, 6 or 7)."
                return

L20430: REM DEFAULT/ENABLE FOR MODE OF PAYMENT...
                message$ = "Enter Mode Of Payment 'C'heck, '$'Cash Or '"&~
                           "D'irect Deposit."
                return

L20520: REM DEFAULT/ENABLE FOR CASH IN BANK ACCOUNT...
                message$ = "Enter The Cash In Bank Account."
                return

L20620: REM DEFAULT/ENABLE FOR GROSS PAYROLL ACCOUNT...
                message$ = "Enter The Payroll Accrual Account."
                return

L20720: REM DEFAULT/ENABLE FOR PRIMARY DEPARTMENT...
                message$ = "Enter Department Or Leave Blank."
                return

L20750: REM DEFAULT/ENABLE FOR PRIMARY STATE WORKING IN...
                message$ = "Enter The Standard Two Letter State Abbrevi"&~
                           "ation."
                return

L20780: REM DEFAULT/ENABLE FOR NORMAL HOURS PER DAY...
                message$ = "Enter Employee's Normal Work Hours Per Day."
                return

L20830: REM DEFAULT/ENABLE FOR NORMAL HOURS PER WEEK...
                message$ = "Enter Employee's Normal Work Hours Per Week."
                return

L20880: REM DEFAULT/ENABLE FOR AUTOMATIC PAYROLL (Y/N)...
                message$ = "Automatic Payroll Enables Employee's Earnin"&~
                           "gs To Be Calculated Automatically."
                return

        REM *************************************************************~
            *         D E F A U L T / E N A B L E   P A G E   2         *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for page 2 of document   *~
            *************************************************************

            enable_162
                  message$ = " "
                  enabled% = 1%
                  on fieldnr% goto  L21200,         /* LABOR CLASS      */~
                                    L21240,         /* OVERHEAD ACCT    */~
                                    L21280,         /* WORKMANS' COMP.  */~
                                    L21320,         /* VACATION ACCRUAL */~
                                    L21340,         /* SICK ACCRUAL CODE*/~
                                    L21360,         /* NORMAL RATE      */~
                                    L21380,         /* VACA TO CHANGE TO*/~
                                    L21400,         /* VACA DATE TO CHNG*/~
                                    L21440,         /* RATE TO POST JOB */~
                                    L21500          /* SHIFT            */

L21200: REM DEFAULT/ENABLE FOR LABOR CLASS...
                message$ = "Enter Labor Class To Use During Job Posting"&~
                           " To Determine Overhead Percentage."
                return

L21240: REM DEFAULT/ENABLE FOR LABOR CLASS CODE...
                message$ = "If Labor Is To Be Posted To A Job, Enter De"&~
                           "fault Overhead Accrual Account."
                return

L21280: REM DEFAULT/ENABLE FOR WORKMANS' COMP. CODE DEFAULT...
                message$ = "Enter Workmans' Compensation Code (Memo Onl"&~
                           "y)."
                return

L21320: REM DEFAULT/ENABLE FOR VACATION ACCRUAL CODE...
                message$ = "Enter Vacation Accrual Method."
                return

L21340: REM DEFAULT/ENABLE FOR SICK ACCRUAL CODE...
                message$ = "Enter Sick Leave Accrual Method."
                return

L21360: REM DEFAULT/ENABLE FOR NORMAL RATE...
                message$ = "Enter The Base Rate For Estimating Sick And"&~
                           " Vacation Accrued Liablity."
                return

L21380: REM DEFAULT/ENABLE FOR VACATION METHOD TO CHANGE TO...
                message$ = "Enter The NEW Vacation Accrual Method."
                return

L21400: REM DEFAULT/ENABLE FOR VACATION METHOD CHANGE DATE...
                message$ = "Enter The Effective Date Of The NEW Vacatio"&~
                           "n Accrual Method."
                if newcode$ > " " then return
                enabled% = 0%
                return

L21440: REM DEFAULT/ENABLE FOR TIME CARD PAY RATE (JOB POSTING)...
                message$ = "Enter Rate To Charge Job If Posting Job Cos"&~
                           "ts Outside Payroll."
                if dept$ > " " then return
                enabled% = 0%
                return

L21500: REM DEFAULT/ENABLE FOR SHIFT...
                message$ = "Enter Normal Work Shift."
                if dept$ > " " then return
                enabled% = 0%
                return

        REM *************************************************************~
            *         D E F A U L T / E N A B L E   P A G E   3         *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for page 3 of document   *~
            *************************************************************

            deffn'163(fieldnr%)
                  message$ = " "
                  enabled% = 1%

        REM Default/Enable For DEPOSIT INFORMATION...
           if paymode$ <> "D" then enabled% = 0%
           if fieldnr% > 1% and bankcode$(fieldnr%-1%) = " " then        ~
                                                            enabled% = 0%
           message$ = "Enter Requested Info.  If Any Field Is Entered" & ~
                      ", All Must Be Entered."
           if editmode% <> 0% then enabled% = 1%
           return

        REM *************************************************************~
            *   D E F A U L T / E N A B L E   F O R   E A R N I N G S   *~
            *-----------------------------------------------------------*~
            * Default/enable for earnings table- Input Mode only...     *~
            *************************************************************

            deffn'164(fieldnr%)
                  enabled% = 1%
                  on fieldnr% goto  L22380,         /* CATEGORY   CODE  */~
                                    L22420,         /* EARNINGS TYPE    */~
                                    L22460,         /* PAID IN CASH?    */~
                                    L22510,         /* UNITS DESCRIPTION*/~
                                    L22550,         /* RATE PER UNIT    */~
                                    L22610,         /* ACCOUNT NUMBER   */~
                                    L22650,         /* USUAL UNITS      */~
                                    L22690,         /* USUAL AMOUNT     */~
                                    L22800,         /* SICK ACCRUAL FLAG*/~
                                    L22880          /* VACA ACCRUAL FLAG*/

L22380: REM DEFAULT/ENABLE FOR CATEGORY CODE...
            if erncata$(c%) <> " " then enabled% = 0%
            return

L22420: REM DEFAULT/ENABLE FOR EARNINGS TYPE...
            if erntype$(c%) <> " " then enabled% = 0%
            return

L22460: REM DEFAULT/ENABLE FOR PAID IN CASH? FLAG...
            if erncash$(c%) <> " " then enabled%     = 0%
            if erncash$(c%)  = " " then erncash$(c%) = "Y"
            return

L22510: REM DEFAULT/ENABLE FOR UNITS DESCRIPTION...
            if ernunits$(c%) <> " " then enabled% = 0%
            return

L22550: REM DEFAULT/ENABLE FOR UNIT RATE...
            if ernrate$(c%) <> " " then enabled% = 0%
            if ernrate$(c%) = " " and c% > 1%                            ~
                                      then ernrate$(c%) = ernrate$(c%-1%)
            return

L22610: REM DEFAULT/ENABLE FOR ACCOUNT NUMBER...
            if ernacct$(c%) <> " " then enabled% = 0%
            return

L22650: REM DEFAULT/ENABLE FOR USUAL UNITS...
            if ernusunits$(c%) <> " " then enabled% = 0%
            return

L22690: REM DEFAULT/ENABLE FOR USUAL AMOUNT...
           enabled% = 0%
           if ernusbucks$(c%) <> " " then return
               if ernusunits$(c%) = " " then return
                    enabled% = 1%
                    convert ernrate$(c%) to ernrate
                    convert ernusunits$(c%) to ernusunits
                    call "CONVERT" (round(ernusunits * ernrate, 2%),     ~
                                                 -2.2, ernusbucks$(c%))
                    return

L22800: REM DEFAULT/ENABLE FOR SICK ACCRUAL FLAG...
            if ernsicka$(c%) <> " " then enabled%      = 0%
            if enabled% = 0% then return
            if ernsicka$(c%)  = " " then ernsicka$(c%) = "1"
            if str(erntype$(c%),,4%)= "SICK" then ernsicka$(c%) = "-1"
            infomsg$ = "Enter 1 If This Accrues Sick Pay, -1 If It" &    ~
                       " Takes Sick Pay, Or 0 If Neither."
            return

L22880: REM DEFAULT/ENABLE FOR VACATION ACCRUAL FLAG...
            if ernvacat$(c%) <> " " then enabled% = 0%
            if enabled% = 0% then return
            if ernvacat$(c%) = " " then ernvacat$(c%) = "1"
            if str(erntype$(c%),,4%)= "VACA" then ernvacat$(c%) = "-1"
            infomsg$ = "Enter 1 If This Accrues Vacation Pay, -1 If" &   ~
                       " It Takes Vctn Pay, Or 0 If Neither."
            return

L24000: REM *************************************************************~
            *     D E F A U L T / E N A B L E   D E D U C T I O N S     *~
            *-----------------------------------------------------------*~
            * Default/enable for deductions table.                      *~
            *************************************************************

*       ***DEFFN'165(FIELDNR%)
                  enabled% = 1%
                  on fieldnr% goto  L24210,         /* DEDXN CATEGORY   */~
                                    L24270,         /* DEDXN METHOD     */~
                                    L24310,         /* DEDXN DESCRIPTION*/~
                                    L24350,         /* EMPLOYEE PAYS?   */~
                                    L24390,         /* CREDIT ACCOUNT   */~
                                    L24430,         /* DEBIT ACCOUNT    */~
                                    L24500,         /* APPLIES FIELD    */~
                                    L24540,         /* AMOUNT 1         */~
                                    L24540,         /* AMOUNT 2         */~
                                    L24540,         /* AMOUNT 3         */~
                                    L24540,         /* AMOUNT 4         */~
                                    L24580          /* GOAL             */

L24210: REM DEFAULT/ENABLE FOR DEDUCTION CATEGORY...
            if dedcat$(c%) <> " " then enabled% = 0%
            infomsg$ = "Category Need Not Mean Anything, But It Must" &  ~
                       " Not Be Blank."
            return

L24270: REM DEFAULT/ENABLE FOR DEDUCTION METHOD...
            if dedmethod$(c%) <> " " then enabled% = 0%
            return

L24310: REM DEFAULT/ENABLE FOR DEDUCTION DESCRIPTION...
            if deduction$(c%) <> " " then enabled% = 0%
            return

L24350: REM DEFAULT/ENABLE FOR EMPLOYEE PAYS THIS? FLAG...
            if dedempflag$(c%) <> " " then enabled% = 0%
            return

L24390: REM DEFAULT/ENABLE FOR CREDIT ACCOUNT NUMBER...
            if dedcracct$(c%) <> " " then enabled% = 0%
            return

L24430: REM DEFAULT/ENABLE FOR DEBIT ACCOUNT NUMBER...
            enabled% = 0%
            if deddbacct$(c%) <> " " then return
            if dedempflag$(c%) = "Y" then deddbacct$(c%) = grossacct$    ~
                                     else enabled% = 1%
            return

L24500: REM DEFAULT/ENABLE FOR APPLIES FIELD...
            if dedapplies$(c%) <> " " then enabled% = 0%
                      return

L24540: REM DEFAULT/ENABLE FOR AMOUNT 1 - 4...
            if deddescr$(fieldnr% - 7%,c%) = " " then enabled% = 0%
            return

L24580: REM DEFAULT/ENABLE FOR GOAL...
*          IF DEDGOAL$(C%) <> " " THEN ENABLED% = 0%
            /* Let User edit this, If it's not used by DED, who cares */
            return

        REM *************************************************************~
            *     S U P E R   I N S E R T   F O R   E A R N I N G S     *~
            *-----------------------------------------------------------*~
            * Handles super insert (insert an entire earnings category) *~
            * for earnings table.  This is similar to insert mode for   *~
            * the earnings table except no screen input.                *~
            *************************************************************

        esuperinsert

            errormsg$, inscat$ = " "
L25110:     gosub L46000                  /* get category to insert     */
                  if keyhit%  =  1% then startover
                  if keyhit%  = 16% then edtearnings
                  if keyhit% <>  0% then L25110
            gosub L57000                  /* test for validity          */
                  if errormsg$ <> " " then L25110

        REM Plow routine for loading all entries in the category...
            readkey$ = str(inscat$)
            oldline% = line%
            currentline%, c% = line% + int((cursor%(1%)-1%)/4%)
            if c% > maxlines%(1%) then currentline%, c% = maxlines%(1%)
                if cursor%(1%)< 5% then currentline%,c% = max(0, line%-1%)
            firstline% = currentline%

L25260:     call "PLOWNEXT" (#12, readkey$, 4%, f1%(12%))
                if f1%(12%) = 0% then L25340
            get #12 using L25266, dfltype$
L25266:         FMT POS(8), CH(12)
            search str(erntype$()) = str(dfltype$) to search%() step 12%
                if search%(1%) <> 0% then L25282
                        currentline%, c% = currentline% + 1%
                        gosub L25690      /* ROLL ARRAYS UP ONE LINE.   */
                        gosub L32000      /* LOAD LINE FROM DISK.       */
                        maxlines%(1%) = maxlines%(1%) + 1%
                        goto L25260
L25282:     u3% = 0%
            call "ASKUSER" (u3%, "***** DUPLICATE EARNINGS TYPE *****",  ~
                 "Category " & inscat$ & " Earnings Type " & dfltype$   &~
                 " Already Exists.", "Press RETURN To Insert Remaining "&~
                 "Types In This Category", "-OR- Press PF16 To End This"&~
                 " Category.")
            if u3% = 0% then L25260
                if u3% <> 16% then L25282

L25340: REM Now end loading up entries routine...
                lastline% = currentline%

        REM Now begin input mode for the super inserted line items...
                line% = firstline% - mod(firstline%, 5%)
                screenline% = firstline% - line%
                infomsg$, errormsg$ = " "
                if maxlines%(1%) = 0% then return

L25430:         screenline% = screenline% + 1%
                if screenline% < 6% then L25470
                   line% = line% + 5%
                   screenline% = 1%
L25470:         currentline%, c% = line% + screenline%
                if currentline% > lastline% then L25650
                call "SETSEP"(separator$(), line%, min(maxlines%(1%)     ~
                                                     - line%, 5%))

                for fieldnr% = 1% to 10%
                    gosub'164(fieldnr%)
                          if enabled% =  0% then       L25620
L25550:             gosub'204(screenline%, fieldnr%)
                          if keyhit%  =  0% then       L25600
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  =  4% then gosub elineabove
                          goto L25550
L25600:             gosub'154(fieldnr%)
                          if errormsg$ <> " " then L25550
L25620:         next fieldnr%
                goto L25430

L25650: REM Terminate super insert mode...
                line% =min(max(lastline%-5%,0%), max(maxlines%(1%)-5%,0%))
                return

L25690: REM This routine rolls the current line down.  subset of 17800...
                if currentline% > maxlines%(1%) then return
                for temp% = maxlines%(1%) to c% step -1%
                    erncata$   (temp%+1%) = erncata$   (temp%)
                    erntype$   (temp%+1%) = erntype$   (temp%)
                    erncash$   (temp%+1%) = erncash$   (temp%)
                    ernjunk1$  (temp%+1%) = ernjunk1$  (temp%)
                    ernunits$  (temp%+1%) = ernunits$  (temp%)
                    ernrate$   (temp%+1%) = ernrate$   (temp%)
                    ernacct$   (temp%+1%) = ernacct$   (temp%)
                    ernusunits$(temp%+1%) = ernusunits$(temp%)
                    ernusbucks$(temp%+1%) = ernusbucks$(temp%)
                    ernsicka$  (temp%+1%) = ernsicka$  (temp%)
                    ernvacat$  (temp%+1%) = ernvacat$  (temp%)
                    ernrecord$ (temp%+1%) = ernrecord$ (temp%)
                    rest1$     (temp%+1%) = rest1$     (temp%)
                next temp%

        clear_eline
            erncata$(c%), erntype$(c%), erncash$(c%), errormsg$,         ~
            ernunits$(c%), ernsicka$(c%), ernvacat$(c%), ernjunk1$(c%),  ~
            ernusunits$(c%), ernusbucks$(c%), ernrate$(c%), ernacct$(c%),~
            infomsg$ = " "
            init(hex(00)) ernrecord$(c%), rest1$(c%)
            return

        REM *************************************************************~
            *   S U P E R   I N S E R T   F O R   D E D U C T I O N S   *~
            *-----------------------------------------------------------*~
            * Super insert for deductions mode.  Similar to earnings.   *~
            *************************************************************

        dsuperinsert
            errormsg$, inscat$ = " "

L26090:     gosub L46350                  /* Get category to insert     */
                  if keyhit%  =  1% then startover
                  if keyhit%  = 16% then edtdedxns
                  if keyhit% <>  0% then L26090
            gosub L58000                  /* Test for validity          */
                  if errormsg$ <> " " then L26090

        REM Plow routine for loading all entries in the category...
            readkey$ = str(inscat$)
            oldline% = line%
            currentline%, c% = line% + int((cursor%(1%))/5%)
            if c% > maxlines%(2%) then currentline%,c% = maxlines%(2%)
                if cursor%(1) < 5 then currentline%, c% = max(0, line%-1%)
            firstline% = currentline%

L26240:     call "PLOWNEXT" (#11, readkey$, 6%, f1%(11%))
                if f1%(11%) = 0% then L26320
            get #11 using L26246, dfldedmthod$
L26246:         FMT POS(10), CH(6)
            search str(dedmethod$()) = str(dfldedmthod$) to search%()    ~
                                                                 step 6%
                if search%(1%) <> 0% then L26264
                    currentline%, c% = currentline% + 1%
                    gosub L26670              /* ROLL ARRAYS UP ONE LINE */
                    gosub L33000              /* LOAD LINE FROM DISK  */
                    maxlines%(2%) = maxlines%(2%) + 1%
                    goto L26240
L26264:     u3% = 0%
            call "ASKUSER" (u3%, "***** DUPLICATE DEDUCTION *****",      ~
                 "Category " & inscat$ & " Deduction Method " &          ~
                  dfldedmthod$ & " Already Exists.", "Press RETURN To I"&~
                 "nsert Remaining Methods In This Category", "-OR- Pres"&~
                 "s PF16 To End This Category.")
            if u3% = 0% then L26240
                if u3% <> 16% then L26264

L26320: REM Now end loading up entries routine...
                lastline% = currentline%

        REM Now begin input mode for the super inserted line items...
                line% = firstline% - mod(firstline%, 5%)
                screenline% = firstline% - line%
                infomsg$, errormsg$ = " "
                if maxlines%(2%) = 0% then return

L26410:         screenline% = screenline% + 1%
                if screenline% < 5% then L26450
                   line% = line% + 4%
                   screenline% = 1%
L26450:         currentline%, c% = line% + screenline%
                if currentline% > lastline% then L26630
                call "SETSEP"(separator$(), line%, min(maxlines%(2%)     ~
                                                     - line%, 5%))

                for fieldnr% = 1% to 12%
                    gosub L24000
                          if enabled% =  0% then       L26580
L26530:             gosub'205(screenline%, fieldnr%)
                          if keyhit%  =  0% then       L26580
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  =  4% then gosub dlineabove
                          goto L26530
L26580:             gosub L54000
                          if errormsg$ <> " " then L26530
                next fieldnr%
                goto L26410

L26630: REM Terminate super insert mode...
                line% =min(max(lastline%-5%,0%),max(maxlines%(2%)-5%,0%))
                return

L26670: REM This routine rolls the current line down. Subset of 17800...
                if currentline% > maxlines%(2%) then return
                for temp% = maxlines%(2%) to c% step -1%
                    dedcat$     (temp%+1%) = dedcat$     (temp%)
                    dedmethod$  (temp%+1%) = dedmethod$  (temp%)
                    deduction$  (temp%+1%) = deduction$  (temp%)
                    dedempflag$ (temp%+1%) = dedempflag$ (temp%)
                    dedcracct$  (temp%+1%) = dedcracct$  (temp%)
                    deddbacct$  (temp%+1%) = deddbacct$  (temp%)
                    dedapplies$ (temp%+1%) = dedapplies$ (temp%)
                    deddescr$(1%,temp%+1%) = deddescr$(1%,temp%)
                    deddescr$(2%,temp%+1%) = deddescr$(2%,temp%)
                    deddescr$(3%,temp%+1%) = deddescr$(3%,temp%)
                    deddescr$(4%,temp%+1%) = deddescr$(4%,temp%)
                    dedamt$  (1%,temp%+1%) = dedamt$  (1%,temp%)
                    dedamt$  (2%,temp%+1%) = dedamt$  (2%,temp%)
                    dedamt$  (3%,temp%+1%) = dedamt$  (3%,temp%)
                    dedamt$  (4%,temp%+1%) = dedamt$  (4%,temp%)
                    dedgoal$    (temp%+1%) = dedgoal$    (temp%)
                    dedrecord$  (temp%+1%) = dedrecord$  (temp%)
                next temp%

        clear_dline
            dedcat$(c%), deduction$(c%), dedmethod$(c%), dedempflag$(c%),~
            dedcracct$(c%), deddbacct$(c%), dedapplies$(c%), errormsg$,  ~
            deddescr$(1%,c%),deddescr$(2%,c%),deddescr$(3%,c%),infomsg$, ~
            deddescr$(4%,c%),dedamt$(1%,c%),dedamt$(2%,c%),dedgoal$(c%), ~
            dedamt$(3%,c%), dedamt$(4%,c%) = " "
            init(hex(00)) dedrecord$(c%)
            return

        REM *************************************************************~
            *       D E S C R I B E   V A R I O U S   F I E L D S       *~
            *-----------------------------------------------------------*~
            * Provides descriptions of several fields on the screen for *~
            * data test, recall, and default/enable sections of code.   *~
            *************************************************************

L29100: REM Set description for pay frequency...
                payfreqdescr$ = " "
                on val(payfreq$) - 48% gosub L29120, L29130, L29140, L29150, ~
                                             L29160, L29170, L29180
                return
L29120:         payfreqdescr$ = "(Weekly)"        : return
L29130:         payfreqdescr$ = "(Bi-Weekly)"     : return
L29140:         payfreqdescr$ = "(Semi-Monthly)"  : return
L29150:         payfreqdescr$ = "(Monthly)"       : return
L29160:         payfreqdescr$ = "(Quarterly)"     : return
L29170:         payfreqdescr$ = "(Semi-annually)" : return
L29180:         payfreqdescr$ = "(Annually)"      : return
                return
L29200: REM Set description for mode of pay...
                paymodedescr$ = " "
                if paymode$ = "$" then paymodedescr$ = "(Cash)"
                if paymode$ = "C" then paymodedescr$ = "(Check)"
                if paymode$ = "D" then paymodedescr$ = "(Direct Deposit)"
                return
L29300: REM Set description for federal filing status...
                fstatusdescr$ = " "
                if fstatus$ = "S" then fstatusdescr$="(Single)"
                if fstatus$ = "M" then fstatusdescr$="(Married)"
                return
L29355: REM Set description for state filing status...
                sstatusdescr$ = " "
                if sstatus$ = "S" then sstatusdescr$="(Single)"
                if sstatus$ = "M" then sstatusdescr$="(Married)"
                if sstatus$ = "H" then sstatusdescr$="(Head Of Household)"
                if sstatus$ = "J" then sstatusdescr$="(Married Filing Jnt~
        ~,One Working)"
                if sstatus$ = "A" then sstatusdescr$="(All)"
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
            k% = 2%
            call "STARTOVR" (k%)
            if k% = 1% then return
            if k% <> 0% then startover
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            * L O A D   E M P L O Y E E   M A S T E R   F O R   E D I T *~
            *-----------------------------------------------------------*~
            * Loads the employee master, his earnings and deductions    *~
            * records for input.                                        *~
            *************************************************************

            call "READ101" (#14, empcode$, f1%(14%))
                 if f1%(14%) = 0% then return
            gosub load_per_stuff

            call "READ101" (#3, empcode$, f1%(3%))
                 if f1%(3%) = 0% then return

          get  #3, using L30470, empcode$, laborclas$, fstatus$, sstatus$,~
                       payfreq$, paymode$, temp$,                        ~
                       cashacct$, grossacct$, normalhrs$, autopay$,      ~
                       firstpaydate$, lastpaydate$, deductflag$,         ~
                       normweekhrs$, wkcomdef$, vaccode$, sickcode$,     ~
                       primstate$, dept$, overacct$, arate, newcode$,    ~
                       newdate$, jrate, shift, filler$

        REM *** Read the direct deposit breakdown ***...
            readkey$ = empcode$ : u3% = 0%
L30120:     call "PLOWNEXT" (#7, readkey$, 12%, f1%(7%))
                if f1%(7%) = 0% then L30230
            u3% = u3% + 1%
            get #7 using L30785, temp$, temp$,                            ~
                 bankcode$(u3%),    /* direct deposit bank code        */~
                 bankacct$(u3%),    /* Direct Deposit checking account */~
                 surepay$(u3%),     /* Direct Deposit transaction code */~
                 ddamount(u3%),     /* Amount of something.            */~
                 bankeff$(u3%,1%),  /* Effective Date- From            */~
                 bankeff$(u3%,2%),  /* Effective Date- To              */~
                 banknote$(u3%,1%)  /* Last Prenotification            */
            if bankeff$(u3%,2%) = blankdate$ then bankeff$(u3%,2%) = " "
            if bankeff$(u3%,2%) = " "   or ~
               bankeff$(u3%,2%) >= date or ~
               bankeff$(u3%,2%) = blankdate$ then  ~
                bankold$(u3%) = str(bankcode$(u3%),, 4%)                 ~
                              & str(surepay$ (u3%),, 2%)                 ~
                              & str(bankacct$(u3%),,17%)
            call "DATFMTC" (bankeff$ (u3%,1%))
            call "DATFMTC" (bankeff$ (u3%,2%))
            call "DATEFMT" (banknote$(u3%,1%))
            banknote$(u3%,2%) = banknote$(u3%,1%)
            call "DESCRIBE"(#9,bankcode$(u3%),bankdescr$(u3%),0%,f1%(9%))
            call "CONVERT" (ddamount(u3%), 2.2, ddamount$(u3%))
            goto L30120

L30230:     if arate > 10000 then arate = 0
            if jrate > 10000 then jrate = 0
            if shift > 4 then shift = 0
            call "CONVERT" (arate, -2.4, arate$)
            call "CONVERT" (jrate, -2.4, jrate$)
            convert shift to shift$, pic(#)
            if shift = 0 then shift$ = " "
        REM Now format INFOMSG$ to say what we're up to...
                infomsg$ = "Loading Employee" & hex(84) & empcode$ & " ("~
                             & empname$ & ")"
                call "SHOSTAT" (infomsg$)

                gosub L39070                  /* Format employee master*/

        REM Load the earnings records...
                maxlines%(1%), c% = 0%
                readkey$ = empcode$

L30320:         call "PLOWNEXT" (#4, readkey$, 12%, f1%(4%))
                     if f1%(4%) = 0% then L30380
                maxlines%(1%), c% = c% + 1%
                get   # 4, using L30615,                                  ~
                           erntype$(c%), erncata$(c%), erncash$(c%),     ~
                           ernjunk1$(c%), ernunits$(c%), ernrate,        ~
                           ernacct$(c%), ernusunits, ernusbucks,         ~
                           ernrecord$(c%), ernsicka$(c%), ernvacat$(c%), ~
                           rest1$(c%)
                gosub L39300              /* FORMAT EARNINGS RECORDS    */
                goto L30320

L30380: REM Load the deductions records...
                maxlines%(2%), c% = 0%
                readkey$ = empcode$

L30400:         call "PLOWNEXT" (#5, readkey$, 12%, f1%(5%))
                     if f1%(5%) = 0% then L30460
                maxlines%(2), c% = c% + 1%
                get   # 5, using L30700,                                  ~
                           dedcat$(c%), dedmethod$(c%), deduction$(c%),  ~
                           dedempflag$(c%), dedcracct$(c%),              ~
                           deddbacct$(c%), dedapplies$(c%),              ~
                           dedamt(1%),dedamt(2%),dedamt(3%),dedamt(4%),  ~
                           dedgoal, dedrecord$(c%)
                gosub L39520              /* Format deductions records  */
                goto L30400

L30460:         return

L30470:     FMT CH(12),                  /* Employee code              */~
                CH(4),                   /* Labor class code           */~
                CH(1),                   /* Fed filing status          */~
                CH(1),                   /* State filing status        */~
                CH(1),                   /* Pay frequency              */~
                CH(1),                   /* Mode of payment            */~
                CH(16),                  /* Filler                     */~
                CH(9),                   /* Cash in bank account       */~
                CH(9),                   /* Gross payroll account      */~
                CH(2),                   /* Normal hrs/day ASCII(##)   */~
                CH(1),                   /* Autopayroll flag           */~
                CH(6),                   /* First date pay period      */~
                CH(6),                   /* Last date pay period       */~
                CH(1),                   /* Deduct on this prldduct run*/~
                CH(02),                  /* Normal hrs/week ASCII(##)  */~
                CH(10),                  /* Workmans' comp. code def   */~
                CH(2),                   /* Vacation accrual method    */~
                CH(2),                   /* Sick accrual method        */~
                XX(3),                   /* Free space                 */~
                CH(2),                   /* Primary state code         */~
                CH(4),                   /* Department                 */~
                CH(9),                   /* Overhead account           */~
                PD(14,4),                /* Average rate for s&v acrls */~
                CH(2),                   /* Next vacation accrual metho*/~
                CH(6),                   /* Vacation accrual changedate*/~
                PD(14,4),                /* Rate for extrbal jb posting*/~
                BI(1),                   /* Shift                      */~
                CH(7)                    /* Filler                     */

L30615:     FMT XX(12),                  /* Employee code              */~
                XX(3),                   /* Sequence number            */~
                XX(12),                  /* Employee code (again)      */~
                CH(12),                  /* Earnings type              */~
                CH(4),                   /* Department code            */~
                CH(1),                   /* Paid in cash? flag         */~
                CH(1),                   /* Free byte                  */~
                CH(6),                   /* Unit description           */~
                PD(14,4),                /* Unit rate                  */~
                CH(9),                   /* Expense account number     */~
                PD(14,4),                /* Usual payroll # of units   */~
                PD(14,4),                /* Usual payroll amount       */~
                CH(80),                  /* Record accumulators        */~
                CH(02),                  /* Sick accrual flag          */~
                CH(02),                  /* Vacation accrual flag      */~
                CH(32)                   /* Spare change               */~

L30700:     FMT XX(12),                  /* Employee code              */~
                XX(3),                   /* Sequence number            */~
                XX(12),                  /* Employee number            */~
                CH(6),                   /* Deduction category (loc?!) */~
                CH(6),                   /* Method of deduction (ddt)  */~
                CH(12),                  /* Deduction description      */~
                CH(1),                   /* Employee pays this? flag   */~
                CH(9),                   /* Credit account             */~
                CH(9),                   /* Debit account              */~
                CH(6),                   /* Applies field (123456)     */~
                PD(14,4),                /* Amount 1 or 0 if not used  */~
                PD(14,4),                /* Amount 2 or 0 if not used  */~
                PD(14,4),                /* Amount 3 or 0 if not used  */~
                PD(14,4),                /* Amount 4 or 0 if not used  */~
                PD(14,4),                /* Goal                       */~
                CH(184)                  /* End of record accumulator  */~

L30785: FMT                 /* FILE: EMPBANKS                          */~
            CH(12),         /* employee code                           */~
            CH(1),          /* Sequence Number                         */~
            CH(4),          /* direct deposit bank code                */~
            CH(20),         /* Direct Deposit checking account number  */~
            CH(2),          /* Direct Deposit Code for Checking or Savi*/~
            PD(14,4),       /* Amount of something.                    */~
            3*CH(6),        /* Effective Dates, Prenotification Date   */~
            CH(191)         /* Filler (Internal, unused space)         */

L31000: REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves payroll data on file.  Note that we have the records*~
            * for this guy in core always, so we have to have him in    *~
            * hold mode so we can make him balance.                     *~
            *************************************************************

            gosub L39250                  /* Unformat master record     */

        REM Now format INFOMSG$ to say what we're up to...
                infomsg$ = "Saving Employee" & hex(84) & empcode$ & " (" ~
                             & empname$ & ")"
                call "SHOSTAT" (infomsg$)
            arate, jrate, shift = 0
            convert arate$ to arate, data goto L31080
L31080:     convert jrate$ to jrate, data goto L31085
L31085:     convert shift$ to shift, data goto L31095

L31095:     put #3,using L30470, empcode$, laborclas$, fstatus$, sstatus$,~
                       payfreq$, paymode$, " ",                          ~
                       cashacct$, grossacct$, normalhrs$, autopay$,      ~
                       firstpaydate$, lastpaydate$, deductflag$,         ~
                       normweekhrs$, wkcomdef$, vaccode$, sickcode$,     ~
                       primstate$, dept$, overacct$, arate, newcode$,    ~
                       newdate$, jrate, shift, filler$

            if f1%(3%) = 0% then write   # 3
            if f1%(3%) = 1% then rewrite # 3

            if f1%(3%) = 0% then call "CDANPOST" (#3, "A")               ~
                            else call "CDANPOST" (#3, "C")

        REM *** Write the direct deposit breakdown ***...
            call "GLFMT" (cashacct$)
            call "GLFMT" (grossacct$)
            call "DELETE" (#7, empcode$, 12%)
            f1s% = f1%(1%)
            mat f1% = zer : u3% = 0% : mat dseq% = zer
            search dedmethod$() = "DIRECT" to f1%() step 6%
            for x% = 1% to 64%  /* Now all DIRECT deductns are indexed */
                if f1%(x%) = 0% then L31240
                   f1%(x%) = (f1%(x%)+5%)/6%
                   if dedcat$(f1%(x%)) <> "SYSTEM" then f1%(x%)=0%  /*?*/
                   if f1%(x%) <> 0% then u3% = x%
                   if f1%(x%) <> 0% then dedapplies$(f1%(x%)) = " "
            next x%  /* All DIRECT Deduction Applies Fields Are Blank */

L31240:     for c% = 1% to 5%
                if bankcode$(c%) = " " then L31510
                convert c% to temp$, pic(#)
                call "DATUNFMT" (bankeff$(c%,1%))
                call "DATUNFMT" (bankeff$(c%,2%))
                if banknote$(c%,1) <> "PENDING" then                     ~
                                       call "DATUNFMT" (banknote$(c%,1%))
                if banknote$(c%,1%) <> "PENDING" then L31340
                     if surepay$(c%) = "22" then temptr$ = "23"          ~
                                            else temptr$ = "33"
                     call "READ100" (#9, bankcode$(c%), f1%(9%))
                     get #9 using L31300, tempdfi$
L31300:                   FMT POS(125), CH(9)
                     convert ddamount(c%) * 100% to tempamt$,            ~
                                                          pic(0000000000)
                     tempname$ = lname$ & ", " & fname$
L31310:              call "GETDTTM" addr(str(dtstamp$,,7%))
                     write #15 using L31331, "6", temptr$, tempdfi$,      ~
                               bankacct$(c%), tempamt$, empcode$,        ~
                               tempname$, " ", "0", " ", " ", " ",       ~
                               dtstamp$, eod goto L31310
L31331:                   FMT CH(1), CH(2), CH(9), CH(17), CH(10),       ~
                              CH(15), CH(22), CH(2), CH(1), CH(8), CH(7),~
                              CH(7), CH(7)
                     banknote$(c%,1%) = date
L31340:         if bankeff$(c%,1%) = " " then bankeff$(c%,1%) = blankdate$
                if bankeff$(c%,2%) = " " then bankeff$(c%,2%) = blankdate$
                write #7 using L30785, empcode$, temp$, bankcode$(c%),    ~
                         bankacct$(c%), surepay$(c%), ddamount(c%),      ~
                         bankeff$(c%,1%),bankeff$(c%,2%),banknote$(c%,1),~
                         " "
        REM Insure corresponding Deductions exists...
                if u3% = 0% then L31405
                for x% = 1% to u3%
                  y% = f1%(x%)
                  if str(dedrecord$(y%),125%,4%)<>bankcode$(c%)  then L31400
                  if str(dedrecord$(y%),105%,20%)<>bankacct$(c%) then L31400
                  if str(dedrecord$(y%),129%,2%)<> surepay$(c%)  then L31400
                  goto L31455  /* Already In deduction List */
L31400:         next x%
L31405: REM Need To Create One...
                  y%, maxlines%(2%) = maxlines%(2%) + 1%
                  dedcat$(y%)     = "SYSTEM"
                  dedmethod$(y%)  = "DIRECT"
                  deduction$(y%)  = "DIRECT DEPOS"
                  dedempflag$(y%) = "Y"
                  str(dedrecord$(y%),105%,20%) = bankacct$(c%)
                  deddescr$(1%,y%) = bankacct$(c%)
                  str(dedrecord$(y%),129%,2%) = surepay$(c%)
                  deddescr$(2%,y%) = "Sure Pay: " & surepay$(c%)
L31455:         str(dedrecord$(y%),125%,4%) = bankcode$(c%)
                str(dedrecord$(y%),131%,6%) = bankeff$ (c%, 1%)
                str(dedrecord$(y%),137%,6%) = bankeff$ (c%, 2%)
                dedapplies$(y%) = "123456"
                dedamt$(1%,y%) = ddamount$(c%)
                dedamt$(2%,y%), dedamt$(3%,y%), dedamt$(4%,y%) = "0"
                deddescr$(3%,y%) = "Amount: " & ddamount$(c%)
                dedgoal$(y%) = "0"
                dedcracct$(y%)  = cashacct$
                deddbacct$(y%)  = grossacct$
                dseq%(c%) = y%  /* Store element for entry */
L31510:     next c%

        REM Now delete the old earnings and deductions records...
                call "DELETE" (#4, empcode$, 12%)
                call "DELETE" (#5, empcode$, 12%)

        REM Plow routine to write earnings records...
                if maxlines%(1%) = 0% then L31605
                for c% = 1% to maxlines%(1)
                    gosub L39410          /* Unformat earnings records  */
                    gosub L39780          /* Check for duplication      */
                    write # 4, using L31760,                              ~
                               empcode$, c%, empcode$, erntype$(c%),     ~
                               erncata$(c%), erncash$(c%), ernjunk1$(c%),~
                               ernunits$(c%), ernrate, ernacct$(c%),     ~
                               ernusunits, ernusbucks, ernrecord$(c%),   ~
                               ernsicka$(c%), ernvacat$(c%), rest1$(c%)
                next c%

L31605: REM Plow routine to write deductions records...
                if maxlines%(2%) = 0% then L31740
                for c% = 1% to maxlines%(2%)
                    gosub L39680
                    x% = c%
                    if dedmethod$(c%) <> "DIRECT" then L31695
        REM This logis insures items are in proper order...
                       x% = c% + 400% /*Old ones go to end,or disappear*/
                       if dedapplies$(c%) <> " " then L31665
                          if pos(str(dedrecord$(c%),,104%) > hex(00)) = 0~
                                                               then L31730
                          goto L31695 /* Retain, has history */
L31665: REM Now insure live ones are in proper order...
                       for i% = 1% to 5%
                          if dseq%(i%) = c% then x% = i% + 300%
                          if dseq%(i%) = c% then L31695
                       next i%
                       /* Should NEVER fall through */
L31695:             write # 5, using L31845,                              ~
                               empcode$, x%, empcode$, dedcat$(c%),      ~
                               dedmethod$(c%), deduction$(c%),           ~
                               dedempflag$(c%), dedcracct$(c%),          ~
                               deddbacct$(c%), dedapplies$(c%),          ~
                               dedamt(1),dedamt(2),dedamt(3),dedamt(4),  ~
                               dedgoal, dedrecord$(c%)
L31730:         next c%

L31740: REM NOW RETURN...
                f1%(1) = f1s%
                return

L31760:     FMT CH(12),                  /* Employee code              */~
                PIC(###),                /* Sequence number            */~
                CH(12),                  /* Employee code (again)      */~
                CH(12),                  /* Earnings type              */~
                CH(4),                   /* Category   code            */~
                CH(1),                   /* Paid in cash? flag         */~
                CH(1),                   /* Free byte                  */~
                CH(6),                   /* Unit description           */~
                PD(14,4),                /* Unit rate                  */~
                CH(9),                   /* Expense account number     */~
                PD(14,4),                /* Usual payroll # of units   */~
                PD(14,4),                /* Usual payroll amount       */~
                CH(80),                  /* Record accumulators        */~
                CH(02),                  /* Sick accrual flag          */~
                CH(02),                  /* Vacation accrual flag      */~
                CH(32)                   /* Free space                 */~

L31845:     FMT CH(12),                  /* Employee code              */~
                PIC(###),                /* Sequence number            */~
                CH(12),                  /* Employee number            */~
                CH(6),                   /* Deduction category (loc?!) */~
                CH(6),                   /* Method of deduction (ddt)  */~
                CH(12),                  /* Deduction description      */~
                CH(1),                   /* Employee pays this? flag   */~
                CH(9),                   /* Credit account             */~
                CH(9),                   /* Debit account              */~
                CH(6),                   /* Applies field (123456)     */~
                PD(14,4),                /* Amount 1 or 0 if not used  */~
                PD(14,4),                /* Amount 2 or 0 if not used  */~
                PD(14,4),                /* Amount 3 or 0 if not used  */~
                PD(14,4),                /* Amount 4 or 0 if not used  */~
                PD(14,4),                /* Goal                       */~
                CH(184)                  /* Record accumulators        */~

L32000: REM *************************************************************~
            *      L O A D   A N   E A R N I N G S   D E F A U L T      *~
            *-----------------------------------------------------------*~
            * Loads an earnings default record from the earnings file.  *~
            *************************************************************

            get  #12, using L32220,                                       ~
                      erncata$(c%), erntype$(c%), erncash$(c%),          ~
                      ernjunk1$(c%),ernunits$(c%), ernrate, ernacct$(c%),~
                      ernusunits, ernusbucks, ernsicka$(c%), ernvacat$(c%)
            gosub L39300
            return

L32220:     FMT CH(4),                   /* Category   code            */~
                XX(3),                   /* Sequence number            */~
                CH(12),                  /* Earnings type              */~
                CH(1),                   /* Paid in cash?              */~
                CH(1),                   /* Free byte                  */~
                CH(6),                   /* Units                      */~
                PD(14,4),                /* Rate per unit              */~
                CH(9),                   /* Account number             */~
                PD(14,4),                /* Usual units                */~
                PD(14,4),                /* Usual dollars              */~
                CH(2),                   /* Sick accrual flag          */~
                CH(2)                    /* Vacation accrual flag      */~

L33000: REM *************************************************************~
            * L O A D   O N E   F R O M   D E D X N S   D E F A U L T S *~
            *-----------------------------------------------------------*~
            * Load a record from the deduction default file.            *~
            * Also gets the descriptions of the record from the D.D.T.  *~
            *************************************************************

            get  #11, using L33330, dedcat$(c%),                          ~
                      dedmethod$(c%), deduction$(c%), dedapplies$(c%),   ~
                      dedcracct$(c%), deddbacct$(c%), dedempflag$(c%),   ~
                      dedamt(1%),dedamt(2%),dedamt(3%),dedamt(4%),dedgoal
            gosub L39520
            if f1%(10%) = 0% then L33270
            return

L33270: REM Error case for deduction not found in DDT...
                for temp% = 1% to 4%
                    deddescr$(temp%, c%) = hex(b4) & "*NOT IN DDT*"
                next temp%
                return

L33330:     FMT CH(6),                   /* Deduction category         */~
                XX(3),                   /* Sequence number (pic(###)) */~
                CH(6),                   /* Method of deduction        */~
                CH(12),                  /* Deduction description      */~
                CH(6),                   /* Applies (123456)           */~
                CH(9),                   /* Credit account             */~
                CH(9),                   /* Debit account              */~
                CH(1),                   /* Employer/employee flag     */~
                PD(14,4),                /* Amount 1                   */~
                PD(14,4),                /* Amount 2                   */~
                PD(14,4),                /* Amount 3                   */~
                PD(14,4),                /* Amount 4                   */~
                PD(14,4)                 /* Goal of deduction          */~

L34000: REM *************************************************************~
            *   L O A D   F R O M   D E D X N   D E F I N I T I O N S   *~
            *-----------------------------------------------------------*~
            * Loads a record from the deduction definition file.        *~
            *************************************************************

            call "READ100" (#10, dedmethod$(c%), f1%(10%))
                 if f1%(10%) = 0% then return

            get #10, using L34300,                                        ~
                     dedmethod$(c%), deduction$(c%), dedempflag$(c%),    ~
                     dedcracct$(c%), deddbacct$(c%), dedapplies$(c%),    ~
                     deddescr$(1%,c%), deddescr$(2%,c%),deddescr$(3%,c%),~
                     deddescr$(4%,c%),dedamt(1%),dedamt(2%),dedamt(3%),  ~
                     dedamt(4%), dedgoal

            gosub L39520
            if dedempflag$(c%) <> "Y" then dedempflag$(c%) = "NO"
            return

L34300:     FMT CH(6),                   /* Method of deduction        */~
                CH(12),                  /* Deduction description      */~
                CH(1),                   /* Employee pays flag         */~
                CH(9),                   /* Credit account             */~
                CH(9),                   /* Debit account              */~
                CH(6),                   /* Applies                    */~
                CH(15),                  /* Description 1              */~
                CH(15),                  /* Description 2              */~
                CH(15),                  /* Description 3              */~
                CH(15),                  /* Description 4              */~
                PD(14,4),                /* Amount 1                   */~
                PD(14,4),                /* Amount 2                   */~
                PD(14,4),                /* Amount 3                   */~
                PD(14,4),                /* Amount 4                   */~
                PD(14,4),                /* Goal                       */~
                XX(3)                    /* Routine number             */~

        REM *************************************************************~
            *        G E T   F R O M         P E R M A S T R            *~
            *                                                           *~
            *************************************************************

        load_per_stuff

            get #14 using L35610, lname$, fname$, mname$

            if mname$ <> " " then                                        ~
                        empname$ = fname$ & " " & mname$ & ". " & lname$ ~
                  else  empname$ = fname$ & " " & lname$
            return

L35610: FMT                      /* FILE: PERMASTR                     */~
            XX(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1)                /* Middle name of person              */


        REM *************************************************************~
            *     F O R M A T   I T E M S   T O   D I S P L A Y         *~
            *-----------------------------------------------------------*~
            * Formats line item and header information for display.     *~
            *************************************************************

L39070: REM Format information for employee master record...
                if firstpaydate$ <>" " then call "DATEFMT" (firstpaydate$)
                if  lastpaydate$ <>" " then call "DATEFMT" ( lastpaydate$)
                if  newdate$ <>" " then call "DATEFMT" (newdate$)
                call "DESCRIBE"(#2,cashacct$,  cashacctdescr$, 1%,f1%(2%))
                     call "GLFMT" (cashacct$)
                call "DESCRIBE"(#2,grossacct$, grossacctdescr$,1%,f1%(2%))
                     call "GLFMT" (grossacct$)
                call "DESCRIBE"(#2,overacct$,  overacctdescr$, 1%,f1%(2%))
                    call "GLFMT" (overacct$)
                call "DESCRIBE" (#6, dept$, deptdescr$, 1%, f1%(6%))
                call "DESCRIBE" (#13, vaccode$, vacation$, 1%, f1%(13%))
                call "DESCRIBE" (#13, sickcode$, sick$, 1%, f1%(13%))
                call "DESCRIBE" (#13, newcode$, new$, 1%, f1%(13%))
                if newcode$ = " " then new$ = "No anticipated change"
                readkey$ = "LBR CLASS" & laborclas$
                call "DESCRIBE"(#8, readkey$, labordescr$, 1%, f1%(8%))
                gosub L29100              /* Describe pay frequency     */
                gosub L29200              /* Describe mode of pay       */
                gosub L29300              /* Describe fed filing status */
                gosub L29355              /* Describe state filing stat */
                return

L39250: REM Unformat information for employee master record...
                call "GLUNFMT" (cashacct$)
                call "GLUNFMT" (grossacct$)
                call "GLUNFMT" (overacct$)
                call "DATUNFMT" (firstpaydate$)
                call "DATUNFMT" (lastpaydate$)
                call "DATUNFMT" (newdate$)
                return

L39300: REM Format information for earnings records...
                ernrate$(c%), ernusunits$(c%), ernusbucks$(c%) = " "
                if ernrate = 0 then L39330
                call "CONVERT" (ernrate, 2.4, ernrate$(c%))
L39330:         if ernusunits = 0 then L39360
                   call "CONVERT" (ernusunits, 2.4, ernusunits$(c%))
L39360:         if ernusbucks = 0 then L39372
                   call "CONVERT" (ernusbucks, 2.2, ernusbucks$(c%))
L39372:         call "GLFMT" (ernacct$(c%))
                return

L39410: REM Unformat information for earnings records...
                if ernrate$(c%)<>" " then convert ernrate$(c%) to ernrate~
                                     else ernrate = 0
                if ernusunits$(c%) <> " "                                ~
                   then convert ernusunits$(c%) to ernusunits            ~
                   else ernusunits = 0
                if ernusbucks$(c%) <> " "                                ~
                   then convert ernusbucks$(c%) to ernusbucks            ~
                   else ernusbucks = 0
                call "GLUNFMT" (ernacct$(c%))
                return

L39520: REM Format information for deductions records...
                call "GLFMT" (deddbacct$(c%))
                call "GLFMT" (dedcracct$(c%))
                deddescr$(1%,c%), deddescr$(2%,c%), deddescr$(3%,c%),    ~
                deddescr$(4%,c%), dedamt$(1%,c%), dedamt$(2%,c%),        ~
                dedamt$(3%,c%), dedamt$(4%,c%) = " "
                if dedmethod$(c%) <> "DIRECT" then L39530
                     deddescr$(1%,c%) = str(dedrecord$(c%),105%,20%)
                     return
L39530:         call "READ100" (#10, dedmethod$(c%), f1%(10%))
                     if f1%(10%) = 0% then return
                get #10, using L39570, deddescr$(1%,c%), deddescr$(2%,c%),~
                                      deddescr$(3%,c%), deddescr$(4%,c%)
L39570:                  FMT XX(43), 4*CH(15)

                for u3% = 1% to 4%
                    if deddescr$(u3%, c%) = " " then L39630
                       call "STRING" addr("RJ", deddescr$(u3%,c%),15%)
                       call "CONVERT" (dedamt(u3%),2.4,dedamt$(u3%,c%))
L39630:         next u3%
                call "CONVERT" (dedgoal, 2.2, dedgoal$(c%))
                return

L39680: REM Unformat information for deductions records...
                call "GLUNFMT" (deddbacct$(c%))
                call "GLUNFMT" (dedcracct$(c%))
                mat dedamt = zer
                for temp% = 1% to 4%
                    if deddescr$(temp%, c%) <> " " then convert          ~
                     dedamt$(temp%, c%) to dedamt(temp%), data goto L39730
L39730:         next temp%
                if dedgoal$(c%)<>" " then convert dedgoal$(c%) to dedgoal~
                                     else dedgoal = 0
                return

L39780: REM Check for earnings type dupilcation...
                str(checkkey$,  1%) = empcode$
                str(checkkey$, 13%) = erntype$(c%)
                str(checkkey$, 25%) = erncata$(c%)
                call "REDALT0" (#4, checkkey$, 1%, f1%(4%))
                      if f1%(4%) = 0% then return
                change% = change% + 1%
                convert change% to str(erntype$(c%),8%,5%), pic(#####)
                go to L39780

        REM *************************************************************~
            *     I N P U T / E D I T   P A G E   1   S C R E E N S     *~
            *-----------------------------------------------------------*~
            * Inputs and edits employee master page 1 data.             *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                header$ = " "
                if lastemp$<>" " then header$="Last Employee: "&lastemp$
                pfktext$(1%)= "(1)Start Over          (4)Previous Field  ~
        ~                     (13)Instructions"
                pfktext$(2%)= "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3%)= "                       (8)Review Personnel~
        ~ Data                (16)Exit Program"
                pfkeys$(5%) = hex(000104080d0f10)
                if fieldnr% > 1% then L40105
                   str(pfktext$(1%),,44%),str(pfktext$(3%),24%,30%) = " "
                   str(pfkeys$(5%),3%,2%) = hex(ffff)
                   goto L40115
L40105:         header$, str(pfktext$(3%),60%) = " "
                str(pfkeys$(5%),7%,1%) = hex(ff)
L40115:         goto L40200

            deffn'211(fieldnr%)    /* Editmode logic... */
                init(hex(86)) lfac$() : lfac$(1%) = hex(8c)
                header$ = " "
                pfktext$(1%)= "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2%)= "(2)Earnings            (5)Next Page       ~
        ~                     (15)Print Screen"
                pfktext$(3%)= "(3)Deductions          (8)Review Personnel~
        ~ Data                (16)Save Data   "
                pfkeys$(5%) = hex(0001020305080d0f10)
                if fieldnr% = 0% then L40200
                   str(pfktext$(2%),,60%), str(pfktext$(3%),,60%) = " "
                   str(pfkeys$(5%),3%,4%) = hex(ffffffff)
                   init(hex(8c)) lfac$()

L40200:           str(header$,62%) = "PRLEMPIN: " & cms2v$
                  str(pfktext$(3%),63%,1%) = hex(84)
                  on fieldnr% goto  L40290,         /* Employee code    */~
                                    L40290,         /* Fed file stat    */~
                                    L40290,         /* State file stat  */~
                                    L40305,         /* Pay freq         */~
                                    L40290,         /* Pay mode         */~
                                    L40290,         /* Cash account#    */~
                                    L40290,         /* Gross payrll acct*/~
                                    L40290,         /* Primary departmen*/~
                                    L40290,         /* Primary state    */~
                                    L40305,         /* Nrml hrs per day */~
                                    L40305,         /* Nrml hrs per week*/~
                                    L40290          /* Auto pay flag    */
                     goto L40345
                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      goto L40345
L40290:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      goto L40345
L40305:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      goto L40345
                  REM SET FAC'S FOR CITY/STATE/ZIP
                      lfac$(7%) = hex(80)
                      lfac$(20%) = hex(81)


L40345:     accept                                                       ~
               at (01,02), "Manage Employee Payroll Master File",        ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Employee Code",                              ~
               at (06,20), fac(lfac$( 1%)), empcode$            , ch(12),~
               at (06,34), fac(hex(84)), empname$               , ch(46),~
                                                                         ~
               at (08,02), "Federal Filing Status",                      ~
               at (08,30), fac(lfac$( 2%)), fstatus$            , ch(01),~
               at (08,33), fac(hex(8c)),   fstatusdescr$        , ch(20),~
                                                                         ~
               at (09,02), "State Filing Status",                        ~
               at (09,30), fac(lfac$( 3%)), sstatus$            , ch(01),~
               at (09,33), fac(hex(8c)),   sstatusdescr$        , ch(32),~
                                                                         ~
               at (10,02), "Pay Frequency",                              ~
               at (10,30), fac(lfac$( 4%)), payfreq$            , ch(01),~
               at (10,33), fac(hex(8c)),   payfreqdescr$        , ch(32),~
                                                                         ~
               at (11,02), "Mode Of Payment",                            ~
               at (11,30), fac(lfac$( 5%)), paymode$            , ch(01),~
               at (11,33), fac(hex(8c)),   paymodedescr$        , ch(32),~
                                                                         ~
               at (12,02), "Cash In Bank Account",                       ~
               at (12,30), fac(lfac$( 6%)), cashacct$           , ch(12),~
               at (12,44), fac(hex(8c)),   cashacctdescr$       , ch(32),~
                                                                         ~
               at (13,02), "Payroll Accrual Account",                    ~
               at (13,30), fac(lfac$( 7%)), grossacct$          , ch(12),~
               at (13,44), fac(hex(8c)),   grossacctdescr$      , ch(32),~
                                                                         ~
               at (14,02), "Department",                                 ~
               at (14,30), fac(lfac$( 8%)), dept$               , ch(04),~
               at (14,44), fac(hex(8c)),   deptdescr$           , ch(32),~
                                                                         ~
               at (15,02), "Primary State Abbrev.",                      ~
               at (15,30), fac(lfac$( 9%)), primstate$          , ch(02),~
                                                                         ~
               at (16,02), "Normal Hours Per Day",                       ~
               at (16,30), fac(lfac$(10%)), normalhrs$          , ch(02),~
                                                                         ~
               at (17,02), "Normal Hours Per Week",                      ~
               at (17,30), fac(lfac$(11%)), normweekhrs$        , ch(02),~
                                                                         ~
               at (18,02), "Automatic Payroll? (Y/N)",                   ~
               at (18,30), fac(lfac$(12%)), autopay$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$(5%)),                                        ~
               key (keyhit%)

               if keyhit% <> 13% then L40660
                  call "MANUAL" ("PRLEMPIN")
                  goto L40345

L40660:        if keyhit% <> 15% then L40680
                  call "PRNTSCRN"
                  goto L40345

L40680:        if editmode% = 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *     I N P U T / E D I T   P A G E   2   S C R E E N S     *~
            *-----------------------------------------------------------*~
            * Inputs and edits the information on employee master page 2*~
            *************************************************************

            deffn'202(fieldnr%)
                init(hex(8c)) lfac$()
                header$ = "This Employee: " & empcode$
                pfktext$(1%)= "(1)Start Over          (4)Previous Field  ~
        ~                     (13)Instructions"
                pfktext$(2%)= "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3%)= "                       (8)Review Personnel~
        ~ Data                (16)Exit Program"
                pfkeys$(5%) = hex(000104080d0f10)
                if fieldnr% > 1% then L41100
                   str(pfktext$(1%),20%,25%) = " "
                   str(pfkeys$(5%),3%) = hex(ff)
                   goto L41110
L41100:         str(pfktext$(3%),60%) = " "
                str(pfkeys$(5%),7%,1%) = hex(ff)
L41110:         goto L41200

            deffn'212(fieldnr%)    /* Editmode logic... */
                init(hex(86)) lfac$()
                header$ = "This Employee: " & empcode$
                pfktext$(1%)= "(1)Start Over          (4)Prev Page       ~
        ~                     (13)Instructions"
                pfktext$(2%)= "(2)Earnings            (5)Next Page       ~
        ~                     (15)Print Screen"
                pfktext$(3%)= "(3)Deductions          (8)Review Personnel~
        ~ Data                (16)Save Data   "
                pfkeys$(5%) = hex(000102030405080d0f10)
                if fieldnr% = 0% then L41200
                   str(pfktext$(1%),20%,30%) = " "
                   str(pfktext$(2%),,60%), str(pfktext$(3%),,60%) = " "
                   str(pfkeys$(5%),3%,5%) = hex(ffffffffff)
                   init(hex(8c)) lfac$()

L41200:           str(header$,62%) = "PRLEMPIN: " & cms2v$
                  str(pfktext$(3%),63%,1%) = hex(84)
                  on fieldnr% goto  L41285,         /* Labor class      */~
                                    L41285,         /* Overhead account */~
                                    L41285,         /* Workmans' comp.  */~
                                    L41285,         /* Vacation accrual */~
                                    L41285,         /* Sick accrual code*/~
                                    L41300,         /* Sick & vaca value*/~
                                    L41285,         /* Vaca to change to*/~
                                    L41285,         /* Vaca date to chng*/~
                                    L41285,         /* Rate for jb post */~
                                    L41285          /* Shift            */
                     goto L41320

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      goto L41320
L41285:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      goto L41320
L41300:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)


L41320:     accept                                                       ~
               at (01,02), "Manage Employee Payroll Master File",        ~
               at (01,50), "Page 2    Todays Date:",                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Labor Class (Overhead Rate)",                ~
               at (06,30), fac(lfac$( 1%)), laborclas$          , ch(04),~
               at (06,49), fac(hex(8c)),   labordescr$          , ch(32),~
                                                                         ~
               at (07,02), "Applied Overhead Account",                   ~
               at (07,30), fac(lfac$( 2%)), overacct$           , ch(12),~
               at (07,49), fac(hex(8c)),   overacctdescr$       , ch(32),~
                                                                         ~
               at (08,02), "Workmans' Comp. Code",                       ~
               at (08,30), fac(lfac$( 3%)), wkcomdef$           , ch(10),~
                                                                         ~
               at (09,02), "Vacation Accrual Method",                    ~
               at (09,30), fac(lfac$( 4%)), vaccode$            , ch(02),~
               at (09,49), fac(hex(8c)),   vacation$            , ch(32),~
                                                                         ~
               at (10,02), "Sick Accrual Method",                        ~
               at (10,30), fac(lfac$( 5%)), sickcode$           , ch(02),~
               at (10,49), fac(hex(8c)),   sick$                , ch(32),~
                                                                         ~
               at (11,02), "Base Rate (For S&V Accrual)",                ~
               at (11,30), fac(lfac$( 6%)), arate$              , ch(10),~
                                                                         ~
               at (12,02), "Change Vacation To Method:",                 ~
               at (12,30), fac(lfac$( 7%)), newcode$            , ch(02),~
               at (12,49), fac(hex(8c)),   new$                 , ch(32),~
                                                                         ~
               at (13,02), "   On First Payroll After:",                 ~
               at (13,30), fac(lfac$( 8%)), newdate$            , ch(08),~
                                                                         ~
               at (14,02), "$/HR For External Job Posting",              ~
               at (14,32), fac(lfac$( 9%)), jrate$              , ch(10),~
                                                                         ~
               at (15,02), "Normal Shift (1-4)",                         ~
               at (15,32), fac(lfac$(10)), shift$               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$(5%)),                                        ~
               key (keyhit%)

               if keyhit% <> 13% then L41590
                  call "MANUAL" ("PRLEMPIN")
                  goto L41320

L41590:        if keyhit% <> 15% then L41610
                  call "PRNTSCRN"
                  goto L41320

L41610:        if editmode% = 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *         E A R N I N G S   T A B L E   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Inputs/edits/inserts/deletes earnings table.              *~
            *************************************************************

            deffn'204(screenline%, fieldnr%)
                  screen% = 1%
                  goto L42140

            deffn'214(screenline%, fieldnr%)
                  screen% = 2%
                  init(hex(86)) fac$()
                  if fieldnr% = 0% then L42145
                  goto L42140

            deffn'224(screenline%, fieldnr%)
                  screen% = 3%
                  goto L42140

            deffn'234(screenline%)
                  screen% = 4%
                  init(hex(84)) fac$()
                  for temp% = 1% to 10%
                      fac$(screenline%, temp%) = hex(94)
                  next temp%
                  goto L42255

L42140:           init(hex(84)) fac$()
L42145:           on fieldnr% goto  L42220,         /* Category   code  */~
                                    L42220,         /* Earnings type    */~
                                    L42220,         /* Paid in cash?    */~
                                    L42220,         /* Units description*/~
                                    L42235,         /* Rate per unit    */~
                                    L42220,         /* Account number   */~
                                    L42235,         /* Usual units      */~
                                    L42235,         /* Usual amount     */~
                                    L42220,         /* Sick accrual flag*/~
                                    L42220          /* Vaca accrual flag*/
                  goto L42255

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      goto L42255
L42220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      goto L42255
L42235:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)


L42255:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$(screen%,1%)     , ch(64),~
               at (02,02), fac(hex(8c)), title$(screen%,2%)     , ch(64),~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (01,67), "! EARNINGS FOR"                             ,~
               at (02,67), "+"                                          ,~
               at (02,69), fac(hex(84)), empcode$               , ch(12),~
                                                                         ~
               at (05,02), fac(hex(84)),    separator$( 1%)     , ch(79),~
               at (09,02), fac(hex(84)),    separator$( 2%)     , ch(79),~
               at (13,02), fac(hex(84)),    separator$( 3%)     , ch(79),~
               at (17,02), fac(hex(84)),    separator$( 4%)     , ch(79),~
               at (21,02), fac(hex(84)),    separator$( 5%)     , ch(79),~
                                                                         ~
               at (06,02), "Cat."                                       ,~
               at (10,02), "Cat."                                       ,~
               at (14,02), "Cat."                                       ,~
               at (18,02), "Cat."                                       ,~
               at (22,02), "Cat."                                       ,~
                                                                         ~
               at (06,07), fac(fac$(1%,1%)), erncata$ (line%+1%), ch(04),~
               at (10,07), fac(fac$(2%,1%)), erncata$ (line%+2%), ch(04),~
               at (14,07), fac(fac$(3%,1%)), erncata$ (line%+3%), ch(04),~
               at (18,07), fac(fac$(4%,1%)), erncata$ (line%+4%), ch(04),~
               at (22,07), fac(fac$(5%,1%)), erncata$ (line%+5%), ch(04),~
                                                                         ~
               at (06,12), "Type"                                       ,~
               at (10,12), "Type"                                       ,~
               at (14,12), "Type"                                       ,~
               at (18,12), "Type"                                       ,~
               at (22,12), "Type"                                       ,~
                                                                         ~
               at (06,17), fac(fac$(1%,2%)), erntype$ (line%+1%), ch(12),~
               at (10,17), fac(fac$(2%,2%)), erntype$ (line%+2%), ch(12),~
               at (14,17), fac(fac$(3%,2%)), erntype$ (line%+3%), ch(12),~
               at (18,17), fac(fac$(4%,2%)), erntype$ (line%+4%), ch(12),~
               at (22,17), fac(fac$(5%,2%)), erntype$ (line%+5%), ch(12),~
                                                                         ~
               at (06,30), "Cash?"                                      ,~
               at (10,30), "Cash?"                                      ,~
               at (14,30), "Cash?"                                      ,~
               at (18,30), "Cash?"                                      ,~
               at (22,30), "Cash?"                                      ,~
                                                                         ~
               at (06,36), fac(fac$(1%,3%)), erncash$ (line%+1%), ch(01),~
               at (10,36), fac(fac$(2%,3%)), erncash$ (line%+2%), ch(01),~
               at (14,36), fac(fac$(3%,3%)), erncash$ (line%+3%), ch(01),~
               at (18,36), fac(fac$(4%,3%)), erncash$ (line%+4%), ch(01),~
               at (22,36), fac(fac$(5%,3%)), erncash$ (line%+5%), ch(01),~
                                                                         ~
               at (06,38), "Units Descrip"                              ,~
               at (10,38), "Units Descrip"                              ,~
               at (14,38), "Units Descrip"                              ,~
               at (18,38), "Units Descrip"                              ,~
               at (22,38), "Units Descrip"                              ,~
                                                                         ~
               at (06,52), fac(fac$(1%,4%)), ernunits$(line%+1%), ch(06),~
               at (10,52), fac(fac$(2%,4%)), ernunits$(line%+2%), ch(06),~
               at (14,52), fac(fac$(3%,4%)), ernunits$(line%+3%), ch(06),~
               at (18,52), fac(fac$(4%,4%)), ernunits$(line%+4%), ch(06),~
               at (22,52), fac(fac$(5%,4%)), ernunits$(line%+5%), ch(06),~
                                                                         ~
               at (06,59), "Rate"                                       ,~
               at (10,59), "Rate"                                       ,~
               at (14,59), "Rate"                                       ,~
               at (18,59), "Rate"                                       ,~
               at (22,59), "Rate"                                       ,~
                                                                         ~
               at (06,64), fac(fac$(1%,5%)), ernrate$ (line%+1%), ch(10),~
               at (10,64), fac(fac$(2%,5%)), ernrate$ (line%+2%), ch(10),~
               at (14,64), fac(fac$(3%,5%)), ernrate$ (line%+3%), ch(10),~
               at (18,64), fac(fac$(4%,5%)), ernrate$ (line%+4%), ch(10),~
               at (22,64), fac(fac$(5%,5%)), ernrate$ (line%+5%), ch(10),~
                                                                         ~
               at (07,02), "Exp. Acct"                                  ,~
               at (11,02), "Exp. Acct"                                  ,~
               at (15,02), "Exp. Acct"                                  ,~
               at (19,02), "Exp. Acct"                                  ,~
               at (23,02), "Exp. Acct"                                  ,~
                                                                         ~
               at (07,12), fac(fac$(1%,6%)), ernacct$ (line%+1%), ch(12),~
               at (11,12), fac(fac$(2%,6%)), ernacct$ (line%+2%), ch(12),~
               at (15,12), fac(fac$(3%,6%)), ernacct$ (line%+3%), ch(12),~
               at (19,12), fac(fac$(4%,6%)), ernacct$ (line%+4%), ch(12),~
               at (23,12), fac(fac$(5%,6%)), ernacct$ (line%+5%), ch(12),~
                                                                         ~
               at (07,25), "Usual Units"                                ,~
               at (11,25), "Usual Units"                                ,~
               at (15,25), "Usual Units"                                ,~
               at (19,25), "Usual Units"                                ,~
               at (23,25), "Usual Units"                                ,~
                                                                         ~
               at (07,37), fac(fac$(1%,7%)),ernusunits$(line%+1%),ch(10),~
               at (11,37), fac(fac$(2%,7%)),ernusunits$(line%+2%),ch(10),~
               at (15,37), fac(fac$(3%,7%)),ernusunits$(line%+3%),ch(10),~
               at (19,37), fac(fac$(4%,7%)),ernusunits$(line%+4%),ch(10),~
               at (23,37), fac(fac$(5%,7%)),ernusunits$(line%+5%),ch(10),~
                                                                         ~
               at (07,48), "Usual Amount"                               ,~
               at (11,48), "Usual Amount"                               ,~
               at (15,48), "Usual Amount"                               ,~
               at (19,48), "Usual Amount"                               ,~
               at (23,48), "Usual Amount"                               ,~
                                                                         ~
               at (07,61), fac(fac$(1%,8%)),ernusbucks$(line%+1%),ch(10),~
               at (11,61), fac(fac$(2%,8%)),ernusbucks$(line%+2%),ch(10),~
               at (15,61), fac(fac$(3%,8%)),ernusbucks$(line%+3%),ch(10),~
               at (19,61), fac(fac$(4%,8%)),ernusbucks$(line%+4%),ch(10),~
               at (23,61), fac(fac$(5%,8%)),ernusbucks$(line%+5%),ch(10),~
                                                                         ~
               at (08,02), "Sick Accrual Flag",                          ~
               at (12,02), "Sick Accrual Flag",                          ~
               at (16,02), "Sick Accrual Flag",                          ~
               at (20,02), "Sick Accrual Flag",                          ~
               at (24,02), "Sick Accrual Flag",                          ~
                                                                         ~
               at (08,20), fac(fac$(1%,9%)), ernsicka$(line%+1%), ch(02),~
               at (12,20), fac(fac$(2%,9%)), ernsicka$(line%+2%), ch(02),~
               at (16,20), fac(fac$(3%,9%)), ernsicka$(line%+3%), ch(02),~
               at (20,20), fac(fac$(4%,9%)), ernsicka$(line%+4%), ch(02),~
               at (24,20), fac(fac$(5%,9%)), ernsicka$(line%+5%), ch(02),~
                                                                         ~
               at (08,23), "Vacation Accrual Flag",                      ~
               at (12,23), "Vacation Accrual Flag",                      ~
               at (16,23), "Vacation Accrual Flag",                      ~
               at (20,23), "Vacation Accrual Flag",                      ~
               at (24,23), "Vacation Accrual Flag",                      ~
                                                                         ~
               at (08,45), fac(fac$(1%,10%)), ernvacat$(line%+1%),ch(02),~
               at (12,45), fac(fac$(2%,10%)), ernvacat$(line%+2%),ch(02),~
               at (16,45), fac(fac$(3%,10%)), ernvacat$(line%+3%),ch(02),~
               at (20,45), fac(fac$(4%,10%)), ernvacat$(line%+4%),ch(02),~
               at (24,45), fac(fac$(5%,10%)), ernvacat$(line%+5%),ch(02),~
               keys (pfkeys$(screen%)),                                  ~
               key  (keyhit%)

               if keyhit% <> 13% then L42955
                  call "MANUAL" ("PRLEMPIN")
                  goto L42255
L42955:        if keyhit% <> 15% then L42970
                  call "PRNTSCRN"
                  goto L42255
L42970:        if screen% <> 2% and screen% <> 3% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *        D E D U C T I O N   T A B L E   S C R E E N        *~
            *-----------------------------------------------------------*~
            * Input/edit/insert/delete deductions table.                *~
            *************************************************************

            deffn'205(screenline%, fieldnr%)
                  screen% = 1%
                  goto L43140

            deffn'215(screenline%, fieldnr%)
                  screen% = 2%
                  init(hex(86)) fac$()
                  if fieldnr% = 0% then L43145
                  goto L43140

            deffn'225(screenline%, fieldnr%)
                  screen% = 3%
                  goto L43140

            deffn'235(screenline%)
                  screen% = 4%
                  init(hex(84)) fac$()
                  for temp% = 1% to 12%
                      fac$(screenline%, temp%) = hex(94)
                  next temp%
                  goto L43265

L43140:           init(hex(84)) fac$()
L43145:           on fieldnr% goto  L43230,         /* Dedxn category   */~
                                    L43230,         /* Deduction method */~
                                    L43230,         /* Dedxn description*/~
                                    L43230,         /* Employee pays?   */~
                                    L43230,         /* Credit account   */~
                                    L43230,         /* Debit account    */~
                                    L43230,         /* Applies          */~
                                    L43245,         /* Amount 1         */~
                                    L43245,         /* Amount 2         */~
                                    L43245,         /* Amount 3         */~
                                    L43245,         /* Amount 4         */~
                                    L43245          /* Goal             */
                  goto L43265

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT.
                      fac$(screenline%, fieldnr%) = hex(80)
                      goto L43265
L43230:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(81)
                      goto L43265
L43245:           REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      fac$(screenline%, fieldnr%) = hex(82)


L43265:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$(screen%,1%)     , ch(64),~
               at (02,02), fac(hex(8c)), title$(screen%,2%)     , ch(64),~
                                                                         ~
               at (01,67), "! DEDUCTIONS: "                             ,~
               at (02,67), "!             "                             ,~
               at (03,67), "+-------------"                             ,~
               at (02,69), fac(hex(84)), empcode$               , ch(12),~
                                                                         ~
               at (03,02), fac(hex(84)), infomsg$               , ch(63),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)),    separator$( 1%)     , ch(79),~
               at (10,02), fac(hex(84)),    separator$( 2%)     , ch(79),~
               at (15,02), fac(hex(84)),    separator$( 3%)     , ch(79),~
               at (20,02), fac(hex(84)),    separator$( 4%)     , ch(79),~
                                                                         ~
               at (06,02), "Cat"                                        ,~
               at (11,02), "Cat"                                        ,~
               at (16,02), "Cat"                                        ,~
               at (21,02), "Cat"                                        ,~
                                                                         ~
               at (06,06), fac(fac$(1%,1%)), dedcat$  (line%+1%), ch(06),~
               at (11,06), fac(fac$(2%,1%)), dedcat$  (line%+2%), ch(06),~
               at (16,06), fac(fac$(3%,1%)), dedcat$  (line%+3%), ch(06),~
               at (21,06), fac(fac$(4%,1%)), dedcat$  (line%+4%), ch(06),~
                                                                         ~
               at (06,13), "Mthd"                                       ,~
               at (11,13), "Mthd"                                       ,~
               at (16,13), "Mthd"                                       ,~
               at (21,13), "Mthd"                                       ,~
                                                                         ~
               at (06,18), fac(fac$(1%,2%)), dedmethod$(line%+1%),ch(06),~
               at (11,18), fac(fac$(2%,2%)), dedmethod$(line%+2%),ch(06),~
               at (16,18), fac(fac$(3%,2%)), dedmethod$(line%+3%),ch(06),~
               at (21,18), fac(fac$(4%,2%)), dedmethod$(line%+4%),ch(06),~
                                                                         ~
               at (06,25), "Descr"                                      ,~
               at (11,25), "Descr"                                      ,~
               at (16,25), "Descr"                                      ,~
               at (21,25), "Descr"                                      ,~
                                                                         ~
               at (06,31), fac(fac$(1%,3%)), deduction$(line%+1%),ch(12),~
               at (11,31), fac(fac$(2%,3%)), deduction$(line%+2%),ch(12),~
               at (16,31), fac(fac$(3%,3%)), deduction$(line%+3%),ch(12),~
               at (21,31), fac(fac$(4%,3%)), deduction$(line%+4%),ch(12),~
                                                                         ~
               at (06,46), "Employee pays?"                             ,~
               at (11,46), "Employee pays?"                             ,~
               at (16,46), "Employee pays?"                             ,~
               at (21,46), "Employee pays?"                             ,~
                                                                         ~
               at (06,61), fac(fac$(1%,4%)),dedempflag$(line%+1%),ch(01),~
               at (11,61), fac(fac$(2%,4%)),dedempflag$(line%+2%),ch(01),~
               at (16,61), fac(fac$(3%,4%)),dedempflag$(line%+3%),ch(01),~
               at (21,61), fac(fac$(4%,4%)),dedempflag$(line%+4%),ch(01),~
                                                                         ~
               at (07,02), "Credit acct"                                ,~
               at (12,02), "Credit acct"                                ,~
               at (17,02), "Credit acct"                                ,~
               at (22,02), "Credit acct"                                ,~
                                                                         ~
               at (07,14), fac(fac$(1%,5%)), dedcracct$(line%+1%),ch(12),~
               at (12,14), fac(fac$(2%,5%)), dedcracct$(line%+2%),ch(12),~
               at (17,14), fac(fac$(3%,5%)), dedcracct$(line%+3%),ch(12),~
               at (22,14), fac(fac$(4%,5%)), dedcracct$(line%+4%),ch(12),~
                                                                         ~
               at (07,31), "Debit acct"                                 ,~
               at (12,31), "Debit acct"                                 ,~
               at (17,31), "Debit acct"                                 ,~
               at (22,31), "Debit acct"                                 ,~
                                                                         ~
               at (07,42), fac(fac$(1%,6%)), deddbacct$(line%+1%),ch(12),~
               at (12,42), fac(fac$(2%,6%)), deddbacct$(line%+2%),ch(12),~
               at (17,42), fac(fac$(3%,6%)), deddbacct$(line%+3%),ch(12),~
               at (22,42), fac(fac$(4%,6%)), deddbacct$(line%+4%),ch(12),~
                                                                         ~
               at (07,59), "Applies"                                    ,~
               at (12,59), "Applies"                                    ,~
               at (17,59), "Applies"                                    ,~
               at (22,59), "Applies"                                    ,~
                                                                         ~
               at (07,67), fac(fac$(1%,7%)),dedapplies$(line%+1%),ch(06),~
               at (12,67), fac(fac$(2%,7%)),dedapplies$(line%+2%),ch(06),~
               at (17,67), fac(fac$(3%,7%)),dedapplies$(line%+3%),ch(06),~
               at (22,67), fac(fac$(4%,7%)),dedapplies$(line%+4%),ch(06),~
                                                                         ~
               at (08,02), fac(hex(8c)),  deddescr$(1%,line%+1%), ch(15),~
               at (13,02), fac(hex(8c)),  deddescr$(1%,line%+2%), ch(15),~
               at (18,02), fac(hex(8c)),  deddescr$(1%,line%+3%), ch(15),~
               at (23,02), fac(hex(8c)),  deddescr$(1%,line%+4%), ch(15),~
                                                                         ~
               at (08,18), fac(hex(8c)),  deddescr$(2%,line%+1%), ch(15),~
               at (13,18), fac(hex(8c)),  deddescr$(2%,line%+2%), ch(15),~
               at (18,18), fac(hex(8c)),  deddescr$(2%,line%+3%), ch(15),~
               at (23,18), fac(hex(8c)),  deddescr$(2%,line%+4%), ch(15),~
                                                                         ~
               at (08,34), fac(hex(8c)),  deddescr$(3%,line%+1%), ch(15),~
               at (13,34), fac(hex(8c)),  deddescr$(3%,line%+2%), ch(15),~
               at (18,34), fac(hex(8c)),  deddescr$(3%,line%+3%), ch(15),~
               at (23,34), fac(hex(8c)),  deddescr$(3%,line%+4%), ch(15),~
                                                                         ~
               at (08,50), fac(hex(8c)),  deddescr$(4%,line%+1%), ch(15),~
               at (13,50), fac(hex(8c)),  deddescr$(4%,line%+2%), ch(15),~
               at (18,50), fac(hex(8c)),  deddescr$(4%,line%+3%), ch(15),~
               at (23,50), fac(hex(8c)),  deddescr$(4%,line%+4%), ch(15),~
                                                                         ~
               at (08,66), "Goal"                                       ,~
               at (13,66), "Goal"                                       ,~
               at (18,66), "Goal"                                       ,~
               at (23,66), "Goal"                                       ,~
                                                                         ~
               at (09,04), fac(fac$(1%,8%)), dedamt$(1%,line%+1%),ch(12),~
               at (14,04), fac(fac$(2%,8%)), dedamt$(1%,line%+2%),ch(12),~
               at (19,04), fac(fac$(3%,8%)), dedamt$(1%,line%+3%),ch(12),~
               at (24,04), fac(fac$(4%,8%)), dedamt$(1%,line%+4%),ch(12),~
                                                                         ~
               at (09,20), fac(fac$(1%,9%)), dedamt$(2%,line%+1%),ch(12),~
               at (14,20), fac(fac$(2%,9%)), dedamt$(2%,line%+2%),ch(12),~
               at (19,20), fac(fac$(3%,9%)), dedamt$(2%,line%+3%),ch(12),~
               at (24,20), fac(fac$(4%,9%)), dedamt$(2%,line%+4%),ch(12),~
                                                                         ~
               at (09,36), fac(fac$(1%,10%)),dedamt$(3%,line%+1%),ch(12),~
               at (14,36), fac(fac$(2%,10%)),dedamt$(3%,line%+2%),ch(12),~
               at (19,36), fac(fac$(3%,10%)),dedamt$(3%,line%+3%),ch(12),~
               at (24,36), fac(fac$(4%,10%)),dedamt$(3%,line%+4%),ch(12),~
                                                                         ~
               at (09,52), fac(fac$(1%,11%)),dedamt$(4%,line%+1%),ch(12),~
               at (14,52), fac(fac$(2%,11%)),dedamt$(4%,line%+2%),ch(12),~
               at (19,52), fac(fac$(3%,11%)),dedamt$(4%,line%+3%),ch(12),~
               at (24,52), fac(fac$(4%,11%)),dedamt$(4%,line%+4%),ch(12),~
                                                                         ~
               at (09,68), fac(fac$(1%,12%)),dedgoal$ (line%+1%), ch(10),~
               at (14,68), fac(fac$(2%,12%)),dedgoal$ (line%+2%), ch(10),~
               at (19,68), fac(fac$(3%,12%)),dedgoal$ (line%+3%), ch(10),~
               at (24,68), fac(fac$(4%,12%)),dedgoal$ (line%+4%), ch(10),~
                                                                         ~
               keys (pfkeys$(screen%)),                                  ~
               key  (keyhit%)

               if keyhit% <> 13% then L44105
                  call "MANUAL" ("PRLEMPIN")
                  goto L43265
L44105:        if keyhit% <> 15% then L44125
                  call "PRNTSCRN"
                  goto L43265

L44125:        if screen% <> 2% and screen% <> 3% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L45000: REM *************************************************************~
            *   B U I L D   E A R N I N G S   T A B L E   S C R E E N   *~
            *-----------------------------------------------------------*~
            * Screen to input categories for use in building the        *~
            * earnings table.                                           *~
            *************************************************************

            header$ = "This Employee: " & empcode$
            str(header$,62%) = "PRLEMPIN: " & cms2v$
            message$ = "Leave All Fields Blank To Enter Earnings Indivi"&~
                       "dually."

L45050:     accept                                                       ~
               at (01,02), "Build Employee Earnings Table",              ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,27), "******************************",             ~
               at (09,27), "*    EARNINGS CATEGORIES:    *",             ~
               at (10,27), "*                            *",             ~
               at (11,27), "*                            *",             ~
               at (12,27), "*                            *",             ~
               at (13,27), "*                            *",             ~
               at (14,27), "*                            *",             ~
               at (15,27), "*                            *",             ~
               at (16,27), "*                            *",             ~
               at (17,27), "******************************",             ~
                                                                         ~
               at (11,30), fac(hex(81)), category$( 1%)         , ch(04),~
               at (11,40), fac(hex(81)), category$( 2%)         , ch(04),~
               at (11,50), fac(hex(81)), category$( 3%)         , ch(04),~
               at (12,30), fac(hex(81)), category$( 4%)         , ch(04),~
               at (12,40), fac(hex(81)), category$( 5%)         , ch(04),~
               at (12,50), fac(hex(81)), category$( 6%)         , ch(04),~
               at (13,30), fac(hex(81)), category$( 7%)         , ch(04),~
               at (13,40), fac(hex(81)), category$( 8%)         , ch(04),~
               at (13,50), fac(hex(81)), category$( 9%)         , ch(04),~
               at (14,30), fac(hex(81)), category$(10%)         , ch(04),~
               at (14,40), fac(hex(81)), category$(11%)         , ch(04),~
               at (14,50), fac(hex(81)), category$(12%)         , ch(04),~
               at (15,30), fac(hex(81)), category$(13%)         , ch(04),~
               at (15,40), fac(hex(81)), category$(14%)         , ch(04),~
               at (15,50), fac(hex(81)), category$(15%)         , ch(04),~
               at (16,30), fac(hex(81)), category$(16%)         , ch(04),~
               at (16,40), fac(hex(81)), category$(17%)         , ch(04),~
               at (16,50), fac(hex(81)), category$(18%)         , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13% then L45295
                  call "MANUAL" ("PRLEMPIN")
                  goto L45050

L45295:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L45050

L45315: REM *************************************************************~
            * B U I L D   D E D U C T I O N S   T A B L E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Build deductions table screen (similar to earnings table) *~
            *************************************************************

            header$ = "This Employee: " & empcode$
            str(header$,62%) = "PRLEMPIN: " & cms2v$
            message$ = "Leave All Fields Blank To Enter Deductions Indi"&~
                       "vidually."

L45360:     accept                                                       ~
               at (01,02), "Build Employee Deductions Table",            ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (08,27), "******************************",             ~
               at (09,27), "*   DEDUCTIONS CATEGORIES:   *",             ~
               at (10,27), "*                            *",             ~
               at (11,27), "*                            *",             ~
               at (12,27), "*                            *",             ~
               at (13,27), "*                            *",             ~
               at (14,27), "*                            *",             ~
               at (15,27), "*                            *",             ~
               at (16,27), "*                            *",             ~
               at (17,27), "******************************",             ~
                                                                         ~
               at (11,30), fac(hex(81)), category$( 1%)         , ch(06),~
               at (11,39), fac(hex(81)), category$( 2%)         , ch(06),~
               at (11,48), fac(hex(81)), category$( 3%)         , ch(06),~
               at (12,30), fac(hex(81)), category$( 4%)         , ch(06),~
               at (12,39), fac(hex(81)), category$( 5%)         , ch(06),~
               at (12,48), fac(hex(81)), category$( 6%)         , ch(06),~
               at (13,30), fac(hex(81)), category$( 7%)         , ch(06),~
               at (13,39), fac(hex(81)), category$( 8%)         , ch(06),~
               at (13,48), fac(hex(81)), category$( 9%)         , ch(06),~
               at (14,30), fac(hex(81)), category$(10%)         , ch(06),~
               at (14,39), fac(hex(81)), category$(11%)         , ch(06),~
               at (14,48), fac(hex(81)), category$(12%)         , ch(06),~
               at (15,30), fac(hex(81)), category$(13%)         , ch(06),~
               at (15,39), fac(hex(81)), category$(14%)         , ch(06),~
               at (15,48), fac(hex(81)), category$(15%)         , ch(06),~
               at (16,30), fac(hex(81)), category$(16%)         , ch(06),~
               at (16,39), fac(hex(81)), category$(17%)         , ch(06),~
               at (16,48), fac(hex(81)), category$(18%)         , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13% then L45600
                  call "MANUAL" ("PRLEMPIN")
                  goto L45360

L45600:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L45360

L46000: REM *************************************************************~
            *      S U P E R - I N S E R T   E D I T   S C R E E N      *~
            *-----------------------------------------------------------*~
            * Inputs codes of items to be super-inserted.               *~
            *************************************************************

            header$ = "This Employee: " & empcode$
            str(header$,62%) = "PRLEMPIN: " & cms2v$
L46080:     accept                                                       ~
               at (01,02), "Super-Insert Earnings Categories" ,          ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Category To Insert",                         ~
               at (06,30), fac(hex(81)), inscat$                , ch(04),~
                                                                         ~
               at (21,02), fac(hex(bc)), message$               , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,02), "(1)Start Over   ",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return      ",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L46310
                  call "MANUAL" ("PRLEMPIN")
                  goto L46080

L46310:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L46080

L46350:     header$ = "This Employee: " & empcode$
            str(header$,62%) = "PRLEMPIN: " & cms2v$
            accept                                                       ~
               at (01,02), "Super-Insert Deduction Categories" ,         ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Category To Insert",                         ~
               at (06,30), fac(hex(81)), inscat$                , ch(06),~
                                                                         ~
               at (21,02), fac(hex(bc)), message$               , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,02), "(1)Start Over   ",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return      ",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L46600
                  call "MANUAL" ("PRLEMPIN")
                  goto L46350

L46600:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L46350

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *-----------------------------------------------------------*~
            * Screen for editing page 1 of document.                    *~
            *************************************************************

        show_personnel_stuff
            call "PRLEMPSB" (#14)
            return

        REM *************************************************************~
            *     I N P U T / E D I T   P A G E   3   S C R E E N S     *~
            *-----------------------------------------------------------*~
            * Inputs and edits the information on employee master page 3*~
            *************************************************************

            deffn'203(fieldnr%)
                init(hex(8c)) lfac$()
                header$ = "This Employee: " & empcode$
                pfktext$(1%)= "(1)Start Over          (4)Previous Field  ~
        ~                     (13)Instructions"
                pfktext$(2%)= "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3%)= "                       (8)Review Personnel~
        ~ Data                (16)Exit Program"
                pfkeys$(5%) = hex(000104080d0f10)
                if fieldnr% > 1% then L48370
                   str(pfktext$(1%),20%,25%) = " "
                   str(pfkeys$(5%),3%,1%) = hex(ff)
                   goto L48370

            deffn'213(fieldnr%)    /* Editmode logic... */
                init(hex(86)) lfac$()
                header$ = "This Employee: " & empcode$
                pfktext$(1%)= "(1)Start Over          (4)Prev Page       ~
        ~                     (13)Instructions"
                pfktext$(2%)= "(2)Earnings                               ~
        ~                     (15)Print Screen"
                pfktext$(3%)= "(3)Deductions          (8)Review Personnel~
        ~ Data                (16)Save Data   "
                pfkeys$(5%) = hex(0001020304080d0f10)
                if fieldnr% = 0% then L48370
                   str(pfktext$(2%),,20%), str(pfktext$(3%),,60%) = " "
                   str(pfktext$(1%),20%,25%) = " "
                   str(pfkeys$(5%),3%,4%) = hex(ffffffff)
                   init(hex(8c)) lfac$()

L48370:           str(header$,62%) = "PRLEMPIN: " & cms2v$
                  str(pfktext$(3%),63%,1%) = hex(84)
                  lfac$(fieldnr%) = hex(81)

            accept                                                       ~
               at (01,02), "Employee Direct Deposits Data",              ~
               at (01,50), "Page 3    Todays Date:",                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(8c)), ddheader$(1%)          , ch(79),~
               at (06,02), fac(hex(8c)), ddheader$(2%)          , ch(79),~
                                                                         ~
               at (07,02), "1)", at (08,02), "2)", at (09,02), "3)",     ~
               at (10,02), "4)", at (11,02), "5)",                       ~
                                                                         ~
               at (07,06), fac(lfac$(1%)), bankcode$(1%)        , ch(04),~
               at (08,06), fac(lfac$(2%)), bankcode$(2%)        , ch(04),~
               at (09,06), fac(lfac$(3%)), bankcode$(3%)        , ch(04),~
               at (10,06), fac(lfac$(4%)), bankcode$(4%)        , ch(04),~
               at (11,06), fac(lfac$(5%)), bankcode$(5%)        , ch(04),~
                                                                         ~
               at (07,13), fac(lfac$(1%)), surepay$(1%)         , ch(02),~
               at (08,13), fac(lfac$(2%)), surepay$(2%)         , ch(02),~
               at (09,13), fac(lfac$(3%)), surepay$(3%)         , ch(02),~
               at (10,13), fac(lfac$(4%)), surepay$(4%)         , ch(02),~
               at (11,13), fac(lfac$(5%)), surepay$(5%)         , ch(02),~
                                                                         ~
               at (07,18), fac(lfac$(1%)), bankacct$(1%)        , ch(17),~
               at (08,18), fac(lfac$(2%)), bankacct$(2%)        , ch(17),~
               at (09,18), fac(lfac$(3%)), bankacct$(3%)        , ch(17),~
               at (10,18), fac(lfac$(4%)), bankacct$(4%)        , ch(17),~
               at (11,18), fac(lfac$(5%)), bankacct$(5%)        , ch(17),~
                                                                         ~
               at (07,38), fac(lfac$(1%)), ddamount$(1%)        , ch(09),~
               at (08,38), fac(lfac$(2%)), ddamount$(2%)        , ch(09),~
               at (09,38), fac(lfac$(3%)), ddamount$(3%)        , ch(09),~
               at (10,38), fac(lfac$(4%)), ddamount$(4%)        , ch(09),~
               at (11,38), fac(lfac$(5%)), ddamount$(5%)        , ch(09),~
                                                                         ~
               at (07,48), fac(lfac$(1%)), bankeff$(1%,1%)      , ch(10),~
               at (08,48), fac(lfac$(2%)), bankeff$(2%,1%)      , ch(10),~
               at (09,48), fac(lfac$(3%)), bankeff$(3%,1%)      , ch(10),~
               at (10,48), fac(lfac$(4%)), bankeff$(4%,1%)      , ch(10),~
               at (11,48), fac(lfac$(5%)), bankeff$(5%,1%)      , ch(10),~
                                                                         ~
               at (07,59), fac(lfac$(1%)), bankeff$(1%,2%)      , ch(10),~
               at (08,59), fac(lfac$(2%)), bankeff$(2%,2%)      , ch(10),~
               at (09,59), fac(lfac$(3%)), bankeff$(3%,2%)      , ch(10),~
               at (10,59), fac(lfac$(4%)), bankeff$(4%,2%)      , ch(10),~
               at (11,59), fac(lfac$(5%)), bankeff$(5%,2%)      , ch(10),~
                                                                         ~
               at (07,70), fac(hex(8c))  , banknote$(1%,1%)     , ch(08),~
               at (08,70), fac(hex(8c))  , banknote$(2%,1%)     , ch(08),~
               at (09,70), fac(hex(8c))  , banknote$(3%,1%)     , ch(08),~
               at (10,70), fac(hex(8c))  , banknote$(4%,1%)     , ch(08),~
               at (11,70), fac(hex(8c))  , banknote$(5%,1%)     , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$(5)),                                         ~
               key (keyhit%)

               if keyhit% <> 13% then L49040
                  call "MANUAL" ("PRLEMPIN")
                  goto L41320

L49040:        if keyhit% <> 15% then L49080
                  call "PRNTSCRN"
                  goto L41320

L49080:        if editmode% = 0 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *          T E S T   D A T A   F O R   P A G E   1          *~
            *-----------------------------------------------------------*~
            * TEsts data for page 1 of linear input                     *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% goto  L50210,         /* Employee code    */~
                                    L50310,         /* Fed file stat    */~
                                    L50380,         /* State file stat  */~
                                    L50450,         /* Pay freq         */~
                                    L50520,         /* Pay mode         */~
                                    L50580,         /* Cash account#    */~
                                    L50690,         /* Gross payrll acct*/~
                                    L50800,         /* Department code  */~
                                    L50850,         /* Primary state    */~
                                    L50870,         /* Nrml hrs per day */~
                                    L50910,         /* Nrml hrs per week*/~
                                    L50950          /* Auto pay flag    */

L50210: REM TEST DATA FOR EMPLOYEE CODE...
                if empcode$ <> " " then L50240
                      call "GETEMPL" (#14, empcode$, " ", 0%, f1%(14%))
L50240:          gosub L30000
                 if f1%(14%) = 1% then L50280
                 errormsg$ ="Employee Is Not Setup In The Personnel File"
                 return
L50280:         if f1%(3%) = 0% then return
                return clear all
                goto editmode

L50310: REM TEST DATA FOR FED FILE STAT...
                if pos("SM" = fstatus$) = 0% then L50350
                gosub L29300        /* DESCRIBE FED FILING STAT   */
                return
L50350:            errormsg$ = "Federal Filing Status MUST Be 'M' Or 'S'"
                   return

L50380: REM TEST DATA FOR STAT FILE STAT...
*              IF POS("SMHJA" = SSTATUS$) = 0 THEN 50420
                if sstatus$ <> " " then L50400
                   errormsg$ = "State Filing Status CANNOT Be Blank"
                   return
L50400:         gosub L29355        /* DESCRIBE STATE FILING STAT */
                return
                   errormsg$ = "State Filing Status MUST be S, M, H, J "&~
                               "Or A"
                   return

L50450: REM TEST DATA FOR PAY FREQUENCY...
                if payfreq$ < "1" or payfreq$ > "7" then L50490
                gosub L29100        /* DESCRIBE PAY FREQUENCY     */
                return
L50490:            errormsg$ = "Pay Frequency MUST Be Betwee 1 And 7"
                   return

L50520: REM TEST DATA FOR PAY MODE (CASH, DIRECT DEPOSIT, OR CHECK)...
                if pos("$CD" = paymode$) = 0% then L50560
                gosub L29200        /* DESCRIBE PAY MODE          */
                return
L50560:            errormsg$ = "Mode Of Payment MUST Be C, $ Or D"
                   return

L50580: REM TEST DATA FOR CASH IN BANK ACCOUNT...
                incl_excl(1%) = 40.01 : incl_excl$(1%) = "$"
                incl_excl(2%) = 40.01 : incl_excl$(2%) = "L"
                cashacctdescr$ = hex(06) & "Select G/L Cash Account"
                call "PLOWCODE" (#2, cashacct$,cashacctdescr$, 5000%,    ~
                                 .3, f1%(2%), columnhdr$(), 0,0,         ~
                                 incl_excl(), incl_excl$())
                if f1%(2%) = 0% then L50670
                call "PUTPAREN" (cashacctdescr$)
                return
L50670:             errormsg$ = "Cash In Bank Account CANNOT Be Blank"
                    return

L50690: REM TEST DATA FOR GROSS PAYROLL ACCOUNT...
                incl_excl(1%) = 40.01 : incl_excl$(1%) = "L"
                incl_excl(2%) = 00.00 : incl_excl$(2%) = " "
                grossacctdescr$ = hex(06) & "Select Payroll Accrual Acct"
                call "PLOWCODE" (#2, grossacct$,grossacctdescr$, 5000%,  ~
                                 .3, f1%(2%), columnhdr$(), 0,0,         ~
                                 incl_excl(), incl_excl$())
                if f1%(2%) = 0% then L50780
                call "PUTPAREN" (grossacctdescr$)
                return
L50780:             errormsg$ = "Gross Payroll Account CANNOT Be Blank"
                    return

L50800: REM TEST DATA FOR DEPARTMENT...
                if dept$ = " " then return
                call "GETCODE" (#6, dept$, deptdescr$, 1%, 0, f1%(6%))
                if f1%(6%) = 0% then errormsg$ = hex(00)
                return

L50850: REM TEST DATA FOR PRIMARY STATE EMPLOYED IN...
                return

L50870: REM TEST NORMAL HOURS PER DAY...
                if normalhrs$ = " " then return
                call "NUMTEST" (normalhrs$, 0, 99, errormsg$, 0.0, 0)
                return

L50910: REM TEST NORMAL HOURS PER WEEK...
                if normweekhrs$ = " " then return
                call "NUMTEST" (normweekhrs$, 0, 99, errormsg$, 0.0, 0)
                return

L50950: REM TEST DATA FOR AUTOPAY?...
                if autopay$ <> "Y" then autopay$ = "N"
                return

        REM *************************************************************~
            *          T E S T   D A T A   F O R   P A G E   2          *~
            *-----------------------------------------------------------*~
            * Tests data for page 2 of linear input                     *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% goto  L51190,         /* Labor class      */~
                                    L51270,         /* Overhead acct .  */~
                                    L51330,         /* Workmans' comp.  */~
                                    L51350,         /* Vacation accrual */~
                                    L51420,         /* Sick accrual code*/~
                                    L51540,         /* Sick & vaca value*/~
                                    L51570,         /* Vaca to change to*/~
                                    L51640,         /* Vaca date to chng*/~
                                    L51680,         /* Rate to post job */~
                                    L51710          /* Shift            */

L51190: REM TEST DATA FOR LABOR CLASS...
                labordescr$ = " "
                if laborclas$ = " " then return
                readkey$ = "LBR CLASS" & laborclas$
                call "PLOWCODE"(#8, readkey$,labordescr$, 9%,.30, f1%(8%))
                if f1%(8%) = 1% then laborclas$ = str(readkey$,10%) else ~
                      errormsg$ = "Labor Class Code NOT On File"
                      return

L51270: REM TEST DATA FOR OVERHEAD ACCOUNT...
                if overacct$ = " " then return
                   call "GETCODE" (#2, overacct$, overacctdescr$,        ~
                                                          1%, 0, f1%(2%))
                if f1%(2%) = 0% then errormsg$ = hex(00)
                   return

L51330: REM TEST DATA FOR WORKMANS' COMP CODE...
                   return

L51350: REM TEST DATA FOR VACATION ACCRUAL METHOD (CODE)...
                vacation$ = "Vacation accrual NOT used"
                if vaccode$ = " " then return
                call "GETCODE" (#13, vaccode$, vacation$, 1%, 0, f1%(13%))
                    if f1%(13%) <> 0% then return
                errormsg$ = "Vacation Accrual Method NOT On File"
                return

L51420: REM TEST DATA FOR SICK ACCRUAL METHOD (CODE)...
                sick$ = "Sick accrual NOT used"
                if sickcode$ = " " then return
                call "GETCODE" (#13, sickcode$, sick$, 1%, 0, f1%(13%))
                    if f1%(13%) <> 0% then L51490
                errormsg$ = "Sick Accrual Method NOT On File"
                return
L51490:     get #13, using L51500, vestflag$
L51500:     FMT XX(57), CH(1)
            if vestflag$ = "Y" then errormsg$ = "This Method Invalid Fo"&~
                               "r Sick Because Of It's Vesting Method"
                return

L51540: REM TEST DATA FOR SICK AND VACATION ACCRUAL EVALUATION RATE...
                call "NUMTEST" (arate$, 0, 9e7, errormsg$, 2.4, arate)
                return

L51570: REM TEST DATA FOR VACATION ACCRUAL METHOD TO CHANGE TO...
                new$ = "No anticipated change"
                if newcode$ = " " then return
                call "GETCODE" (#13, newcode$, new$, 1%, 0, f1%(13%))
                    if f1%(13%) <> 0% then return
                errormsg$ = "Vacation Accrual Method NOT On File"
                    return

L51640: REM TEST DATA FOR VACATION ACCRUAL CHANGE DATE...
                if newcode$ = " " and (newdate$ = " " or ~
                                       newdate$ = blankdate$) then return
                call "DATEOK" (newdate$, u3%, errormsg$)
                    return

L51680: REM TEST DATA FOR RATE FOR POSTING JOB EXTERNALLY...
                call "NUMTEST" (jrate$, 0, 999, errormsg$, 2.4, 0)
                    return

L51710: REM TEST DATA FOR SHIFT...
                if pos(" 1234" = shift$) > 0% then return
                     errormsg$ = "Shift MUST Be 1, 2, 3 Or 4"
                     return

        REM *************************************************************~
            *  T E S T   D A T A   F O R   E A R N I N G S   T A B L E  *~
            *-----------------------------------------------------------*~
            * Tests data for the entries in the earnings table.         *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% goto  L52190,         /* Category   code  */~
                                    L52290,         /* Earnings type    */~
                                    L52330,         /* Paid in cash?    */~
                                    L52360,         /* Units description*/~
                                    L52400,         /* Rate per unit    */~
                                    L52600,         /* Account number   */~
                                    L52690,         /* Usual units      */~
                                    L52780,         /* Usual amount     */~
                                    L52870,         /* Sick accrual flag*/~
                                    L52940          /* Vaca accrual flag*/

L52190: REM TEST DATA FOR CATEGORY   CODE...
                      return

L52290: REM TEST DATA FOR EARNINGS TYPE...
                if erntype$(c%) = " " then L52322
                   temp$ = erntype$(c%)       /* CHECK */
                   erntype$(c%) = " "     /* FOR DUPLICATE */
                   search str(erntype$()) = str(temp$,,12%) to cursor%() ~
                                                                step 12%
                   erntype$(c%) = temp$
                   if cursor%(1%) = 0% then return
                errormsg$ = "CANNOT Have The Same Earnings Type Set Up "&~
                            "Twice"
                return
L52322:             errormsg$ = "Earnings Type CANNOT Be Blank"
                    return

L52330: REM TEST DATA FOR PAID IN CASH?...
                if erncash$(c%) <> "Y" then erncash$(c%) = "N"
                return

L52360: REM TEST DATA FOR UNITS DESCRIPTION...
                if ernunits$(c%) <> " " then return
                   errormsg$ = "Units Description CANNOT Be Blank"
                   return

L52400: REM TEST DATA FOR RATE PER UNIT...
                call "NUMTEST" (ernrate$(c%), 0, 9e7, errormsg$, 2.4, n)
                if errormsg$ <> " " then return
                if ernusunits$(c%) = " " then ernusbucks$(c%) = " "
                convert ernusunits$(c%) to uernunits, data goto L52475
                uernbucks = round(uernunits * n, 2%)
                call "CONVERT" (uernbucks, -2.2, ernusbucks$(c%))
L52475:     return

*          IF C% <> 1% THEN RETURN
*          IF MAXLINES%(1) < 2 THEN RETURN
*          FOR LOOP% = 2% TO MAXLINES%(1)
*              IF ERNTYPE$(LOOP%) = "PREMIUM" THEN 52550
*              IF ERNTYPE$(LOOP%) = "OVERTIME" THEN 52550
*                 ERNRATE$(LOOP%) = ERNRATE$(C%)
*                 GO TO 52590
*              CONVERT ERNRATE$(C%) TO TEMP
*              CALL "CONVERT" (TEMP*1.5, 2.4, ERNRATE$(LOOP%))
*          NEXT LOOP%
*          RETURN

L52600: REM TEST DATA FOR ACCOUNT NUMBER...
                   call "GETCODE" (#2, ernacct$(c%), infomsg$, 1%, 0,    ~
                                                   f1%(2%))
                   if f1%(2%) <> 0% then return
                errormsg$ = "Earnings Account Number CANNOT Be Blank"
                return

L52690: REM TEST DATA FOR USUAL UNITS...
*              IF ERNUSUNITS$(C%) = " " THEN 52750
                call "NUMTEST" (ernusunits$(c%),0,9e7,errormsg$, 0.4, n)
                     if errormsg$ <> " " then return
                convert ernrate$(c%) to uernrate, data goto L52750
                uernbucks = round(uernrate * n, 2%)
                call "CONVERT" (uernbucks, -2.2, ernusbucks$(c%))
L52750:         if ernusunits$(c%) <= "0" then ernusbucks$(c%),          ~
                                               ernusunits$(c%) = " "
                return

L52780: REM TEST DATA FOR USUAL AMOUNT...
*              IF ERNUSBUCKS$(C%) = " " THEN 52850
                call "NUMTEST" (ernusbucks$(c%),0,9e7,errormsg$, 2.2, n)
                     if errormsg$ <> " " then return
                convert ernrate$(c%) to uernrate, data goto L52850
                if uernrate > 0 then uernunits = round( n / uernrate, 2%)~
                                else uernunits = 0
                call "CONVERT" (uernunits, -0.4, ernusunits$(c%))
L52850:         if ernusbucks$(c%) <= "0" then ernusbucks$(c%),          ~
                                               ernusunits$(c%) = " "
                return

L52870: REM TEST DATA FOR SICK ACCRUAL FLAG...
                call "NUMTEST" (ernsicka$(c%),-1,1,errormsg$,0,temp)
                     return

L52940: REM TEST DATA FOR VACATION ACCRUAL FLAG...
                call "NUMTEST" (ernvacat$(c%),-1,1,errormsg$,0,temp)
                return

L54000: REM *************************************************************~
            *      T E S T   D A T A   F O R   D E D U C T I O N S      *~
            *-----------------------------------------------------------*~
            * Tests data for the deductions table.                      *~
            *************************************************************

*       ***DEFFN'155(FIELDNR%)
                  errormsg$, infomsg$ = " "
                  on fieldnr% goto  L54210,         /* Dedxn category   */~
                                    L54250,         /* Dedxn method     */~
                                    L54430,         /* Dedxn description*/~
                                    L54470,         /* Employee pays?   */~
                                    L54500,         /* Credit account   */~
                                    L54560,         /* Debit account    */~
                                    L54620,         /* Applies field    */~
                                    L54790,         /* Amount 1         */~
                                    L54790,         /* Amount 2         */~
                                    L54790,         /* Amount 3         */~
                                    L54790,         /* Amount 4         */~
                                    L54880          /* Goal             */

L54210: REM TEST DATA FOR DEDUCTION CATEGORY...
                if dedcat$(c%) <> " " then L54231
                   errormsg$ = "Deduction Category CANNOT Be Blank"
L54231:         if dedcat$(c%) <> "SYSTEM" then return
                   errormsg$ = "'SYSTEM' Is A Reserved Word"
                   return

L54250: REM TEST DATA FOR DEDUCTION METHOD...
                   call "GETCODE" (#10, dedmethod$(c%)," ",0%, 0,f1%(10%))
                        if f1%(10%) = 0% then L54360
                   if dedmethod$(c%) <> "DIRECT" then L54280
                      errormsg$ = "DIRECT Can Be Assigned By System Only"
                      return
L54280:            temp$ = dedmethod$(c%)       /* CHECK */
                   dedmethod$(c%) = " "     /* FOR DUPLICATE */
                   search str(dedmethod$()) = str(temp$,,6%) to cursor%()~
                                                               step 6%
                   dedmethod$(c%) = temp$
                   if cursor%(1) <> 0% then L54410
                   if ins% = 1% then gosub L34000
                   return
L54360:            errormsg$ = "Unknown Method Of Deduction"
                   return
                errormsg$ = "Deduction Method CANNOT Be Blank"
                return
L54410:             errormsg$ = "CANNOT Have The Same Deduction Method "&~
                                "Set Up Twice"
                    return

L54430: REM TEST DATA FOR DEDUCTION DESCRIPTION...
                if deduction$(c%) <> " " then return
                   errormsg$ = "Deduction Description CANNOT Be Blank"
                   return

L54470: REM TEST DATA FOR EMPLOYEE PAYS THIS? FLAG...
                if dedempflag$(c%) <> "Y" then dedempflag$(c%) = "N"
                return

L54500: REM TEST DATA FOR CREDIT ACCOUNT NUMBER...
                if dedcracct$(c%) <> " " then L54510
                    get #10 using L54920, dedcracct$(c%)
                    call "GLFMT" (dedcracct$(c%))
L54510:         call "GETCODE" (#2, dedcracct$(c%), infomsg$,            ~
                                1%, 0, f1%(2%))
                if f1%(2%) <> 0% then return
                    errormsg$ = "Credit Account CANNOT Be Blank"
                    return

L54560: REM TEST DATA FOR DEBIT ACCOUNT NUMBER...
                   call "GETCODE" (#2, deddbacct$(c%), infomsg$,         ~
                                         1%, 0, f1%(2%))
                   if f1%(2%) <> 0% then return
                errormsg$ = "Debit Account CANNOT Be Blank"
                return

L54620: REM TEST DATA FOR APPLIES FIELD...
                if dedapplies$(c%) = "ONCE" then return
                if dedapplies$(c%) = " " then L54760
                      for temp% = 1% to 6%
                          if str(dedapplies$(c%), temp%,1%)=" " then L54710
                          if str(dedapplies$(c%), temp%,1%)<"1" then L54730
                          if str(dedapplies$(c%), temp%,1%)>"6" then L54730
L54710:               next temp%
                     return
L54730:         errormsg$ = "Applies MUST Be A Combination Of 1-6"
                return
L54760:         infomsg$ = hex(94) & "Warning : Deduction Will Never Be"&~
                                     "Applied."
                return

L54790: REM TEST DATA FOR DEDUCTION AMOUNT FIELDS...
                t% = fieldnr% - 7%
                if dedamt$(t%,c%) = " " and deddescr$(t%,c%) = " "       ~
                                                              then return
                if deddescr$(t%,c%)<>" " or dedamt$(t%,c%)=" " then L54860
                   errormsg$ = "Deduction Amount CANNOT Be Blank"
                   return
L54860:         call "NUMTEST" (dedamt$(t%,c%),0,9999999,errormsg$,2.4,0%)
                return

L54880: REM TEST DATA FOR GOAL...
                call "NUMTEST" (dedgoal$(c%), 0, 9e7,errormsg$, 2.2, 0%)
                return

L54920:     FMT POS(20), CH(9)     /* Credit Account */
            FMT POS(29), CH(9)     /* Debit  Account */
            FMT POS(44), CH(15), POS(104), PD(14,4)  /* #1 Desc & Amt */
            FMT POS(59), CH(15), POS(112), PD(14,4)  /* #1 Desc & Amt */
            FMT POS(74), CH(15), POS(120), PD(14,4)  /* #1 Desc & Amt */
            FMT POS(89), CH(15), POS(128), PD(14,4)  /* #1 Desc & Amt */
            FMT POS(136), PD(14,4)   /* Goal */


L55000: REM *************************************************************~
            *   T E S T   D A T A   O N   B U I L D   E A R N I N G S   *~
            *-----------------------------------------------------------*~
            * Makes sure all data in the build earnings screen is O.K.  *~
            *************************************************************

            errormsg$ = " "
            maxlines%(1) = 0%

            for temp% = 1% to 18%
                if category$(temp%) = " " then L55200
                   count% = 0%
                   readkey$ = category$(temp%)
L55140:            call "PLOWNEXT" (#12, readkey$, 4%, f1%(12%))
                        if f1%(12%) = 0% then L55180
                           count% = count% + 1%
                   goto L55140
L55180:         if count% = 0% then L55250 /* CATEGORY NOT ON FILE       */
                   maxlines%(1%) = maxlines%(1%) + count%
L55200:     next temp%
            if maxlines%(1%) > 100% then L55290
            return

        REM ERROR MESSAGES FOR VARIOUS CONDITIONS OF BAD INPUT...
L55250:         errormsg$ = "Earnings Category Not On File"
                return

L55290:         errormsg$ = "Maximum Number Of Earnings Will Be Exceede"&~
                            "d, You Must First Delete"
                return

L56000: REM *************************************************************~
            * T E S T   D A T A   O N   B U I L D   D E D U C T I O N S *~
            *-----------------------------------------------------------*~
            * Tests data on the build deductions cycle.                 *~
            *************************************************************

            errormsg$ = " "
            maxlines%(2%) = 0%

            for temp% = 1% to 18%
                if category$(temp%) = " " then L56190
                   count% = 0% : inscat$ = category$(temp%)
                   readkey$ = category$(temp%)
L56130:            call "PLOWNEXT" (#11, readkey$, 6%, f1%(11%))
                        if f1%(11%) = 0% then L56170
                           count% = count% + 1%
                   goto L56130
L56170:         if count% = 0% then L56230    /* Category not on file   */
                   maxlines%(2%) = maxlines%(2%) + count%
L56190:     next temp%
            if maxlines%(2%) > 100% then L56260
            return

L56230:     errormsg$ = "Deduction Category NOT On File"
            return

L56260:     errormsg$ = "Maximum Number Of Deductions Will Be Exceeded,"&~
                        " You Must First Delete"


L57000: REM *************************************************************~
            * T E S T   D A T A   F O R   E A R N I N G S   S U P I N S *~
            *-----------------------------------------------------------*~
            * Tests data for earnings super insert and makes sure that  *~
            * the number of items in the category will not overflow the *~
            * array.                                                    *~
            *************************************************************

            errormsg$ = " "
            count% = maxlines%(1%)              /* Current # of Entries */

            readkey$ = str(inscat$)
            call "PLOWCODE" (#12, readkey$, " ", -4%, -0.001, f1%(12%))
                if f1%(12%) <> 0% then L57170
                    errormsg$ = "Earnings Category NOT On File"
                    return
L57170: REM Check to see if additions will cause table overflow...
            inscat$ = str(readkey$,,4)
            str(readkey$,5%,3%) = hex(000000)
L57200:     if count% < 100% then L57240
                 errormsg$ = "Maximum Number Of Earnings Will Be Exceed"&~
                             "ed, You Must First Delete"
                 return
L57240:     call "PLOWNEXT" (#12, readkey$, 4%, f1%(12%))
                if f1%(12%) = 0% then return
            count% = count% + 1%
            goto L57200

L58000: REM *************************************************************~
            *   T E S T   D A T A   F O R   D E D X N   S U P R I N S   *~
            *-----------------------------------------------------------*~
            * Tests data for deduction super-insert.  Makes sure that   *~
            * the category is both on file and will expand the number of*~
            * earnings to less than 100.                                *~
            *************************************************************

            errormsg$ = " "
            count% = maxlines%(2%)              /* Current # of Entries */

            readkey$ = str(inscat$)
            call "PLOWCODE" (#11, readkey$, " ", -6%, -0.001, f1%(11%))
                if f1%(11%) <> 0% then L58170
                    errormsg$ = "Deduction Category NOT On File"
                    return
L58170: REM Check to see if additions will cause table overflow...
            inscat$ = str(readkey$,,6%)
            str(readkey$,7%,3%) = hex(000000)
L58200:     if count% < 100% then L58240
                errormsg$ = "Maximum Number Of Deductions Will Be Excee"&~
                            "ded, You Must First Delete"
                return
L58240:     call "PLOWNEXT" (#11, readkey$, 6%, f1%(11%))
                if f1%(11%) = 0% then return
            count% = count% + 1%
            goto L58200

L59000: REM *************************************************************~
            *          T E S T   D A T A   F O R   P A G E   3          *~
            *-----------------------------------------------------------*~
            * Tests data for page 3 of linear input                     *~
            *************************************************************

*       ***DEFFN'153(FIELDNR%)
                errormsg$, infomsg$ = " "
                x% = fieldnr%

*        Test data for DEPOSIT INFORMATION
            if bankcode$(x%) <> " " or bankacct$(x%) <> " " or           ~
               surepay$ (x%) <> " " or ddamount$(x%) <> " " then L59230
                     bankeff$ (x%,1%), bankeff$(x%,2%) = " "
                     banknote$(x%,1%) = " "
                     ddamount(x%) = 0
                     return

L59230:     if bankcode$(x%) <> " " then  L59260
                errormsg$ = "Bank Code CANNOT Be Blank"
                return
L59260:     bankdescr$(x%) = hex(06) & "Select Bank # " & bin(x%+48%)
            call "GETCODE" (#9, bankcode$(x%), bankdescr$(x%), 0%, 1,    ~
                                                                 f1%(9%))
            if f1%(9%) <> 0% then L59320
                errormsg$ = "Invalid Bank Account: " & bankcode$(x%)
                return

L59320
*        Test Data for SURE PAY CODE (Transaction Code)
            if surepay$(x%) = "22" or surepay$(x%) = "32" then L59380
                errormsg$ = "Transaction Code MUST Be 22 Or 32"
                return

L59380
*        Test data for BANK ACCOUNT NUMBER
            if bankacct$(x%) <> " " then L59420
                errormsg$ = "Account Number CANNOT Be Blank"
                return

L59420
*        Test data for DEPOSIT AMOUNTS
            call "NUMTEST" (ddamount$(x%), .01, 999999.99, errormsg$,    ~
                                                      -2.2, ddamount(x%))
            if errormsg$ <> " " then return

*        Test data for EFFECTIVE DATE RANGE
            u3% = 0%  :  u4% = 99999999%  :  tempeff$ = "99999999"
            if bankeff$(x%,1%) = blankdate$ then bankeff$(x%,1%) = " "
            if bankeff$(x%,1%) = " " then L59510
                call "DATEOKC" (bankeff$(x%,1%), u3%, errormsg$)
                if errormsg$ <> " " then return
L59510:     if bankeff$(x%,2%) = blankdate$ then bankeff$(x%,2%) = " "
            if bankeff$(x%,2%) = " " then L59540
                call "DATEOKC" (bankeff$(x%,2%), u4%, errormsg$)
                if errormsg$ <> " " then return
                tempeff$ = bankeff$(x%,2%)
                call "DATUFMTC" (tempeff$)
L59540:     if u3% <= u4% then L59550
                errormsg$ = "FROM Date Must Be On Or Before TO Date"
                return

L59550
*        Indicate to user (and to data save) if we will prenotify...
            banknote$(x%,1%) = banknote$(x%,2%)
            if tape% = 0% or tempeff$ < date then return

            banktest$ = str(bankcode$(x%),, 4%) &                        ~
                        str(surepay$ (x%),, 2%) &                        ~
                        str(bankacct$(x%),,17%)
            search str(bankold$()) = str(banktest$,,23%) to p%() step 23%
            if p%(1%) = 0% then banknote$(x%,1%) = "PENDING "
            return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Closes all the files currently open, and also displays    *~
            * a message (only if in foreground) while linking to the    *~
            * next program.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
