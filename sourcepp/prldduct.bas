        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD   DDDD   U   U   CCC   TTTTT   *~
            *  P   P  R   R  L      D   D  D   D  U   U  C   C    T     *~
            *  PPPP   RRRR   L      D   D  D   D  U   U  C        T     *~
            *  P      R   R  L      D   D  D   D  U   U  C   C    T     *~
            *  P      R   R  LLLLL  DDDD   DDDD    UUU    CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDDUCT - PLOWS THROUGH EMPLOYEE MASTER FILE FOR THOSE   *~
            *            EMPLOYEES WHO HAVE THE DEDUCT FLAG SET.        *~
            *            COMPUTES DEDUCTIONS AND POSTS THEM TO THE      *~
            *            CURRENT AREA.  DOES NOT RESET DEDUCTIONS FLAG  *~
            *            SINCE HE STILL NEEDS CHECK PRINTED.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/29/80 ! ORIGINAL                                 ! BCW *~
            * 04/16/81 ! PAY PERIOD CHECK; NEW DEDUCTION TYPES    ! TEM *~
            * 05/12/81 ! RETRIEVE FICA VARIABLES FROM SYSFILE2    ! TEM *~
            * 08/13/81 ! NO FUTA OR SDI ON SICK PAY               ! TEM *~
            * 10/13/81 ! USE MTD HOLD TO FIND YTD                 ! TEM *~
            * 10/19/81 ! USE RANGE TO PROCESS                     ! JAM *~
            * 03/08/84 ! CORRECT ERROR HANDLING FOR INVAL. ROUTINE! ECR *~
            * 10/23/85 ! ADD ID, IL, MA, ME, AND OH TAX ROUTINES  ! SGA *~
            * 09/01/86 ! Added direct deposit ability             ! HES *~
            * 12/04/86 ! Changed MA-IT Routine                    ! SGA *~
            * 10/06/87 ! Added support for federal 125 rules      ! HES *~
            * 11/24/87 ! Handle '87 utah tax change               ! HES *~
            * 01/05/88 ! Support for SYSFLAT & %_OF_NET routines  ! HES *~
            * 02/08/88 ! Added Kansas State Income Tax            ! HES *~
            * 03/28/88 ! Added Oregon State Income Tax            ! HES *~
            * 07/05/88 ! ADDED NORTH CAROLINA INCOME TAX ROUTINE  ! MDE *~
            * 12/01/88 ! Updated California Tax Calculation       ! KAB *~
            * 01/04/89 ! Added checking of direct deposit         ! ERN *~
            *          !   effectivity dates                      !     *~
            * 01/15/89 ! Fixed North Corolina Routine             ! KAB *~
            * 01/20/89 ! Fixed Limit on IRA Routine               ! KAB *~
            * 12/15/89 ! 1990 Repairs                             ! KAB *~
            * 10/02/90 ! Added NEW Employer 401K Routine          ! CBH *~
            * 06/13/91 ! Eliminated unused function FNX.  ALLFREE.! JDH *~
            * 12/02/91 ! 1991 Repairs                             ! KAB *~
            * 12/13/91 ! 1992 Repairs                             ! KAB *~
            * 12/20/91 ! Possible additional NC Update            ! KAB *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 11/20/92 ! PRR 12627, zero ded rec. on recalc.      ! KAB *~
            *          !  Make sure Ded is calculated if employer !     *~
            *          !  pays, even though net is zero.          !     *~
            *          !  Be more careful about net falling below !     *~
            *          !  zero, (negative check amount!).         !     *~
            * 09/26/97 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            acct$9,                      /* EARNINGS ACCOUNT NUMBER    */~
            amount(5),                   /* 4 DEDUCTION PAR. + GOAL    */~
            blankdate$8,                 /* Blank Date for Comparison. */~
            cash$1,                      /* PAID IN CASH? EARNING FLAG */~
            constants(6),                /* CONSTANTS FOR A MTHD       */~
            date$8,                      /* DATE, SCREEN FORMATTED     */~
            dedamt9(4),                  /* SAVED VARIABLE             */~
            dedamt(4),                   /* CURR, MTD, QTD, YTD DEDXNS */~
            dedapplies$6,                /* DEDUCTION APPLIES INFO     */~
            dedcat$6,                    /* DEDUCTION CATEGORY INFO    */~
            dedcracct$9,                 /* DEDUCTION CREDIT ACCOUNT   */~
            deddbacct$9,                 /* DEDUCTION DEBIT ACCOUNT    */~
            dedhold$8,                   /* DEDAMOUNT HOLD             */~
            dedmethod$6,                 /* DEDUCTION METHOD           */~
            dedkey$6, dedsave$6,         /* DEDUCTION METHOD           */~
            dedrecord$100,               /* DEDUCTION RECORD NUMBER    */~
            deductflag$1,                /* DO WE PROCESS THIS GUY?    */~
            deduction$12,                /* DEDUCTION DESCRIPTION      */~
            dept$4,                      /* EARNINGS DEPARTMENT CODE   */~
            dept$(2)4,                   /* DEPARTMENT RANGE           */~
            eff$6, effr$(2)6,            /* Effectivity Dates          */~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            empcode$(2)12,               /* EMPLOYEE CODE RANGE        */~
            empmthds$(90)6,              /* LISTS OF EMPLOYEES DEDUCTNS*/~
            errormsg1$79,                /* ABORT MESSAGE              */~
            errormsg2$79,                /* ABORT MESSAGE              */~
            errormsg3$79,                /* ABORT MESSAGE              */~
            fac1$1, fac2$1, fac3$1,      /* SOME FACS                  */~
            notsub(90,2),                /* FOR EACH DED, $ EXEMPT     */~
            location$2,                  /* LOCATOR ARRAY FOR MATSEARCH*/~
            fstatus$1,                   /* FEDERAL FILING STATUS      */~
            sstatus$1,                   /* STATE FILING STATUS        */~
            tstatus$1,                   /* STATE FILING STATUS        */~
            payfreq$1,                   /* PAY FREQUENCY (1-7)        */~
            payfreq$(2)1,                /* PAY FREQUENCY RANGE        */~
            period$1,                    /* THIS PAY PERIOD THIS MONTH */~
            readkey$50,                  /* READ KEY FOR FILE INFO     */~
            routine$8,                   /* ROUTINE NAME FROM THE DDT  */~
            methodsname$30,              /* METHODS DESCRIPTION        */~
            seqnr$3,                     /* EARNINGS SEQUENCE NUMBER   */~
            s%(2),                       /* FOR SEARCHES               */~
            sbjct(4,2),                  /* GROSS PAY SUBJECT TO DEDUC */~
            sbj(4,2),                    /* GROSS PAY SUBJECT TO DEDUC */~
            txbl$1,                      /* IS THIS EARNING TAXABLE?   */~
            type$12,                     /* EARNINGS TYPE DESCRIPTION  */~
            units$6,                     /* EARNINGS UNIT OF MEASURE   */~
            work$(80)6,                  /*                            */~
                                         /*                            */~
            xtable$7,                    /*                            */~
            xtdescr$30,                  /*                            */~
            xthead$79,                   /*                            */~
            xtmin (12),                  /*                            */~
            xtmax (12),                  /*                            */~
            xtbase(12),                  /*                            */~
            xtpct (12)                   /*                            */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE (PAYROLL PERIOD)     *~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTIONS FILE.                *~
            * # 7 ! EMPBANKS ! EMPLOYEE DIRECT DEPOSIT INFORMATION      *~
            * #10 ! PRLDDT   ! PAYROLL DEDUCTION DEFINITION TABLE       *~
            * #11 ! PRTABLS  ! PAYROLL INCOME TAX WITHHOLDING TABLES    *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

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
                         alternate key  1, keypos =  16, keylen = 28

            select # 5, "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 18, dup

            select # 7, "EMPBANKS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 1, keylen = 13

            select #10, "PRLDDT",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 6

            select #11, "PRLTABLS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 7


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Preparing To Calculate Employee Deductions")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, prldtest%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES THINGS NECESSARY TO THE PLOW ROUTINE--DOES A  *~
            * "START" ON THE ALTERNATE KEY FOR PEOPLE TO DEDUCT.        *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            call "PLOWNEXT" (#7, readkey$, 0%, f1%(7))
                if f1%(7) = 0 then L09360
            call "READ100" (#10, "DIRECT", f1%(10))
                if f1%(10) <> 0 then L09360
            REM This needs to exist only to allow reporting on this mthd,~
                that is why it is written here.  It is not needed in     ~
                (or used by) this program to process the direct deposits
            write #10, using L09230, "DIRECT", "DIRECT DEPOS", "Y", 0, 0,  ~
                             0, 0, 0, "DIRECT", "N", " ", 0, 0, 0, 0, 0, ~
                             0, "Direct Deposit Control Record", "N", " "

L09230:     FMT CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(85),                  /* EMPLOYEE PAYS FLAG, ACCTS, */~
                                         /* APPLIES, DESCRIPTIONS      */~
                5*PD(14,4),              /* AMOUNT 1 - 5               */~
                CH(8),                   /* ROUTINE NAME               */~
                CH(1),                   /* EXEMPT FLAG                */~
                CH(150),                 /* CONSTANT DESCRIPTIONS      */~
                6*PD(14,4),              /* CONSTANT AMOUNTS           */~
                CH(30),                  /* extended description       */~
                CH(1),                   /* TAX TABLE REQUIRED?        */~
                CH(19)                   /* FREE SPACE IN DDT RECORD   */~

L09360:     if prldtest% < 0% then L14000
            call "REDALT0" (#3, "Y", 1%, f1%(3))
                 if f1%(3) = 0 then L65000

            call "SHOSTAT" ("Calculating Deductions")

            REM GET PROCESSING RANGE
                gosub L35000

            REM GET PAYROLL PERIOD FROM SYSFILE2
                call "READ100" (#1, "PAYROLL PERIOD", f1%(1))
                period$ = "1"
                if f1%(1) = 0 then L10000
                   get #1, using L09490, period$
L09490:                    FMT XX(20), CH(1)

L10000: REM *************************************************************~
            *    P L O W   T H R O U G H   E M P L O Y E E   F I L E    *~
            *                                                           *~
            * PLOW THROUGH EMPLOYEE MASTER FILE, LOOKING FOR THOSE WHO  *~
            * HAVE THE TAKE DEDUCTIONS FLAG SET.  FOR THOSE, LOAD UP    *~
            * THE MASTERS AND TOTAL THEIR EARNINGS.                     *~
            *************************************************************

            gosub L30000
            if deductflag$ <> "Y" then L10170
            if empcode$ <= empcode$(1) then L10170
            if empcode$ >  empcode$(2) then L10170
            if dept$ <= dept$(1) then L10170
            if dept$ >  dept$(2) then L10170
            if payfreq$ <= payfreq$(1) then L10170
            if payfreq$ >  payfreq$(2) then L10170
                gosub L11000
                call "ALLFREE"  /* Kenny says here, so here she be! */
L10170:     call "READNEXT" (#3, f1%(3))
                if f1%(3) = 0 then L50000
            goto L10000

L11000: REM *************************************************************~
            *   P R O C E S S   D E D U C T I O N S   T H I S   G U Y   *~
            *                                                           *~
            * JUST CALLS THE DEDUCTION TAKING SUBROUTINE.  REACH INTO   *~
            * THE DDT AND FIND THIS DEDUCTION METHOD AND GET ITS ROUTINE*~
            * NAME.                                                     *~
            *************************************************************

            REM FIRST WE HAVE TO LOAD UP ALL HIS METHS (METHODS OF DED)
                mat notsub = zer
                empmthds$() = " "
                t%, k401, exempt125 = 0
                readkey$ = empcode$
L11130:         call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                          if f1%(5) = 0 then L11250  /* DOES A PRE_READ  */
                     get #5, using L11160, mthd$  /* ON THE EMPS DEDUCTS */
L11160:              FMT XX(33), CH(6)           /* TO DEDUCTION ARRAY. */
                     if mthd$ = "DIRECT" then L11130 /* These stay out */
                     search empmthds$() = str(mthd$,,6) to s%() step 6
                       if s%(1) <> 0 then notsub((s%(1)+5)/6, 1)= -1e9
                     if t% >= 90% then L11250     /* ARRAY IS LATER USED */
                     t% = t% + 1                 /*   TO STORE EXEMPT   */
                     empmthds$(t%) = mthd$       /*  EARNINGS FOR EACH  */
                goto L11130                            /* DEDUCTION */

L11250:     REM NOW WE HAVE TO TOTAL UP THE EARNINGS.
                dollars, units = 0
                readkey$ = empcode$
                numdeds% = max(1, t%)

L11300:         call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                     if f1%(4) = 0 then L11560
                gosub L32000              /* LOAD AN EARNINGS RECORD.   */
                   units = units + pieceunits
                   dollars = dollars + piecedollars /* ADD UP GROSS */

                REM SEE IF EARNTYPE IS NOT SUBJECT TO ANY OF THE EMPS DEDS
                call "READ100" (#1, "PRLTXABL" & str(type$), f1%(1))
                     if f1%(1) = 0 then L11300
                get #1, using L11400, work$()
L11400:         FMT XX(20), 80*CH(6)

                for t% = 1 to numdeds%
                  if empmthds$(t%) = " " then L11500
                     search work$() = str(empmthds$(t%),,6) to s%() step 6
                       if s%(1) = 0 then L11500
                       if notsub(t%,1)= -1e9 then L11500  /* SOMETHING*/
                                                          /* SCREWIE */
                          notsub(t%,1) = notsub(t%,1) + pieceunits
                          notsub(t%,2) = notsub(t%,2) + piecedollars
L11500:         next t%
                goto L11300

*          NOW,  ACROSS FROM EACH OF THE EMPLS DEDS ( EMPMTHDS$(N) ) WILL
*          BE THE EARNINGS AND/OR UNITS THAT ARE EXEMPT ( NOTSUB(N) )

L11560:     REM NEXT, WE HAVE TO PLOW THROUGH THE DEDUCTIONS
                readkey$ = empcode$
                t%, did_fed%, did_fica% = 0
                netpay = round(dollars,2)

L11610:         call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                     if f1%(5) = 0 then return     /* NEXT EMPLOYEE    */
                gosub L31000              /* LOAD DEDUCTION RECORD      */
                amount = 0

                REM Direct Deposits...
                if dedmethod$ <> "DIRECT" then L11770
                    REM Special Code For These...
                    REM Note that because of high seq #s, all 'normal'
                    REM deductions are calced by the time we get here.
                    if eff$ < effr$(1) or eff$ > effr$(2) then L11920
                    if mode$ <> "D" then L11920
                    if abs(dollars) < .001 then L11920   /* No Gross! */
                    if netpay <= 0 then L11730        /* No Pay Left! */
                 /* GOSUB DIRECT */
                    amount = min(amount(1), netpay)
L11730:             sbjct(1,1) = units
                    sbjct(1,2) = dollars
                    goto L11890

L11770:         REM See If This Deduction Applies This Period...
                t% = t% + 1
                if dedmethod$<>empmthds$(t%) then return  /*Shouldnt*/
                                                          /* Happen */
                if abs(dollars) < .001   then L11920   /* No Gross! */
                if dedapplies$ = "ONCE" then L11850
                search dedapplies$ = period$ to location$
                   if location$ = hex(0000) then L11920   /* Nope! */
L11850:         REM It Does...
                gosub L33000         /*   Load the DDT Entry Now   */
                gosub L20000         /*   Calc Amount to Deduct    */

L11890:         amount = max(0, round(amount, 2))  /* Got it */
                if dedempflag$ = "Y" then L11902
                   goto L11920
L11902:         amount = min(netpay, amount)
                netpay = round(netpay - amount, 2)

L11920:            call "READ101" (#5, readkey$, f1%(1))
                        if f1%(1) = 0 then L11610
                   gosub L34000             /* and Save it */
                   goto  L11610

L14000: REM *************************************************************~
            * HIDDEN TEST ROUTINE                                       *~
            *************************************************************
            count% = 0%
            fac1$ = hex(81):fac2$= hex(81):fac3$= hex(9c)
            gosub L14500
              if keyhit% =  1% then L14000
              if keyhit% = 16% then end 99%
              if keyhit% = 32% then end 99%
            fac1$ = hex(8c):fac2$= hex(84):fac3$= hex(84)

            for count% = 1% to inc%
            netpay = dollars
            gosub L15500
            gosub L14500
              if keyhit% =  1% then L14000
              if keyhit% = 16% then end 99%
              if keyhit% = 32% then end 99%
            dollars = dollars + increment
            next count%
            goto L14000

L14500: REM *************************************************************~
            * ACCEPT FOR TEST ROUTINE                                   *~
            *************************************************************

L14540:     accept                                                       ~
               at (01,02),                                               ~
                  "Payroll Deduction Tester (no EMPMASTR file)",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Deduction Amount",                           ~
               at (05,30), fac(fac3$),     amount       , pic(######.##),~
               at (05,45), "YTD",                                        ~
               at (05,65), fac(fac1$),     dedamt(4)    , pic(######.##),~
                                                                         ~
               at (06,02), "Wages",                                      ~
               at (06,30), fac(fac2$),     dollars      , pic(######.##),~
               at (06,45), "YTD  ",                                      ~
               at (06,65), fac(fac1$),     sbjct(4,2)   , pic(######.##),~
                                                                         ~
               at (07,02), "   Increment",                               ~
               at (07,30), fac(fac1$),     increment    , pic(######.##),~
               at (07,45), "Iterations",                                 ~
               at (07,65), fac(fac1$),     inc%         ,        pic(##),~
               at (07,70), fac(fac3$),   count%         ,        pic(##),~
                                                                         ~
               at (08,02), "Units",                                      ~
               at (08,30), fac(fac1$),     units        , pic(######.##),~
                                                                         ~
               at (09,02), "Pay Frequency (1-8)",                        ~
               at (09,25), fac(fac1$),     payfreq$             , ch(01),~
               at (09,30), "Fed Status",                                 ~
               at (09,45), fac(fac1$),     fstatus$             , ch(01),~
               at (09,50), "State Status",                               ~
               at (09,65), fac(fac1$),     sstatus$             , ch(01),~
                                                                         ~
               at (10,02), "Deduction Method",                           ~
               at (10,20), fac(fac1$),     dedkey$              , ch(06),~
               at (10,30), "-or- Deduction Routine/Table Set",           ~
               at (10,65), fac(fac1$),     routine$             , ch(08),~
               at (10,75), fac(fac1$),     dedmethod$           , ch(06),~
                                                                         ~
               at (12,02), "Employee Variable 1",                        ~
               at (12,30), fac(fac1$),     amount(1)    , pic(######.##),~
               at (13,02), "                  2",                        ~
               at (13,30), fac(fac1$),     amount(2)    , pic(######.##),~
               at (14,02), "                  3",                        ~
               at (14,30), fac(fac1$),     amount(3)    , pic(######.##),~
               at (15,02), "                  4",                        ~
               at (15,30), fac(fac1$),     amount(4)    , pic(######.##),~
               at (16,02), "(Goal)            5",                        ~
               at (16,30), fac(fac1$),     amount(5)    , pic(######.##),~
                                                                         ~
               at (12,45), "System Constant 1",                          ~
               at (12,65), fac(fac1$),     constants(1) , pic(######.##),~
               at (13,45), "                2",                          ~
               at (13,65), fac(fac1$),     constants(2) , pic(######.##),~
               at (14,45), "                3",                          ~
               at (14,65), fac(fac1$),     constants(3) , pic(######.##),~
               at (15,45), "                4",                          ~
               at (15,65), fac(fac1$),     constants(4) , pic(######.##),~
               at (16,45), "                5",                          ~
               at (16,65), fac(fac1$),     constants(5) , pic(######.##),~
               at (17,45), "                6",                          ~
               at (17,65), fac(fac1$),     constants(6) , pic(######.##),~
                                                                         ~
               at (19,02), "Fed. Flag",                                  ~
               at (19,13), fac(fac1$),     did_fed%             , pic(#),~
               at (19,16), "Fed. Amount",                                ~
               at (19,30), fac(fac1$),     fed_it       , pic(######.##),~
                                                                         ~
               at (20,02), "FICA Flag",                                  ~
               at (20,13), fac(fac1$),     did_fica%            , pic(#),~
               at (20,16), "FICA Amount",                                ~
               at (20,30), fac(fac1$),     fica1        , pic(######.##),~
               at (20,45), "FICA YTD ",                                  ~
               at (20,65), fac(fac1$),     fica2        , pic(######.##),~
                                                                         ~
               at (22,02), "401K Amount",                                ~
               at (22,30), fac(fac1$),     k401         , pic(######.##),~
               at (22,45), "QBENS Amount",                               ~
               at (22,65), fac(fac1$),     exempt125    , pic(######.##),~
                                                                         ~
               at (23,02), fac(hex(a4)), errormsg$              , ch(79),~
               at (24,02),                                               ~
        "Press PF16 or PF32 to exit, PF1 to Re-enter. PF3 to see TABLE.",~
               keys(hex(20100f000103)), key(keyhit%)

               if keyhit% <> 15 then L15390
                  call "PRNTSCRN" : goto L14540

L15390:        if keyhit% <>  3 then return
                  gosub L16000
                  goto L14540

L15500: REM *************************************************************~
            * ACTUAL WORK LOOP FOR TEST, JUST CALLS ROUTINES            *~
            *************************************************************

                amount = 0 : t% = 1%

                REM Direct Deposits...
                if dedkey$    <> "DIRECT" then L15680
                    REM Special Code For These...
                    REM Note that because of high seq #s, all 'normal'
                    REM deductions are calced by the time we get here.
                 /* IF EFF$ < EFFR$(1) OR EFF$ > EFFR$(2) THEN 15770   */
                 /* IF NETPAY <= 0 THEN 15640     /* NO NET LEFT       */
                 /* IF MODE$ = "D" THEN GOSUB DIRECT                   */
                    amount = min(amount(1), netpay)
                    sbjct(1,1) = units
                    sbjct(1,2) = dollars
                    goto L15770

L15680:         if abs(dollars) < .001 then L15790 /* NO GROSS! */
                if dedkey$ = " " then L15750
                if dedmethod$ = " " then dedmethod$ = dedkey$
                   dedsave$   = dedmethod$
                   dedmethod$ = dedkey$
                gosub L33000         /*   LOAD THE DDT ENTRY NOW   */
                   dedkey$    = dedmethod$
                   dedmethod$ = dedsave$
L15750:         gosub L20000         /*   CALC AMOUNT TO DEDUCT    */

L15770:         amount = max(0, round(amount, 2))  /* GOT IT */

L15790:            return

L16000: REM *************************************************************~
            *                                                           *~
            * TABLE DISPLAY FOR TEST                                    *~
            *                                                           *~
            *************************************************************

            xtdescr$ = "Table Not on File"
            mat xtmin = zer : mat xtbase = zer
            mat xtmin = zer : mat xtpct  = zer
            xtded = 0 : xtbrkt% = 0%
            tstatus$ = sstatus$
            if routine$ = "FIT" then tstatus$ = fstatus$
            if dedmethod$ = " " then xtable$ = str(tstatus$,,1) & dedkey$~
                             else xtable$ = str(tstatus$,,1) & dedmethod$

               xthead$=                                                  ~
          "                     At Least   But Less Than          Amount ~
        ~   Plus Percent"

            call "READ100" (#11, xtable$, f1%(11))
                if f1%(11) = 0% then L16810

            get #11, using L16380, xtdescr$, xtded, xtbrkt%,              ~
                                 xtmin(), xtbase(), xtpct(), xtmax()

L16380:     FMT      XX(07),             /* CODE                       */~
                     CH(30),             /* DESCRIPTION                */~
                     PD(14,4),           /* DEPENDENT ALLOWANCE        */~
                     BI(4),              /* BRACKETS                   */~
                     12*PD(14,4),        /* MINIMUMS                   */~
                     12*PD(14,4),        /* BASE AMOUNTS               */~
                     12*PD(14,4),        /* PERCENTS                   */~
                     12*PD(14,4),        /* MAXIMUMS                   */~
                     XX(67)              /* FILLER                     */

L16810: accept                                                           ~
               at (01,02),                                               ~
                  "Payroll Tax Table",                                   ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), errormsg$              , ch(79),~
               at (04,02),                                               ~
                  "Method",                                              ~
               at (04,10), fac(hex(84)),  str(xtable$,2)        , ch(06),~
               at (04,18),                                               ~
                  "Status (Table ID)",                                   ~
               at (04,37), fac(hex(84)),  str(xtable$,,1)       , ch(01),~
               at (04,40), fac(hex(84)),  xtdescr$              , ch(30),~
               at (06,40),                                               ~
                  "Number of Brackets",                                  ~
               at (06,65), fac(hex(84)),  xtbrkt%              , pic(##),~
               at (06,02), "Dependent Allowance",                        ~
               at (06,25), fac(hex(84)),  xtded        ,pic(-#######.##),~
               at (07,02), fac(hex(ac)) , xthead$               , ch(79),~
                                                                         ~
               at (08,02), "Bracket  1",                                 ~
               at (09,02), "Bracket  2",                                 ~
               at (10,02), "Bracket  3",                                 ~
               at (11,02), "Bracket  4",                                 ~
               at (12,02), "Bracket  5",                                 ~
               at (13,02), "Bracket  6",                                 ~
               at (14,02), "Bracket  7",                                 ~
               at (15,02), "Bracket  8",                                 ~
               at (16,02), "Bracket  9",                                 ~
               at (17,02), "Bracket 10",                                 ~
               at (18,02), "Bracket 11",                                 ~
               at (19,02), "Bracket 12",                                 ~
                                                                         ~
               at (08,20), fac(hex(84)),    xtmin  ( 1),pic(-#######.##),~
               at (09,20), fac(hex(84)),    xtmin  ( 2),pic(-#######.##),~
               at (10,20), fac(hex(84)),    xtmin  ( 3),pic(-#######.##),~
               at (11,20), fac(hex(84)),    xtmin  ( 4),pic(-#######.##),~
               at (12,20), fac(hex(84)),    xtmin  ( 5),pic(-#######.##),~
               at (13,20), fac(hex(84)),    xtmin  ( 6),pic(-#######.##),~
               at (14,20), fac(hex(84)),    xtmin  ( 7),pic(-#######.##),~
               at (15,20), fac(hex(84)),    xtmin  ( 8),pic(-#######.##),~
               at (16,20), fac(hex(84)),    xtmin  ( 9),pic(-#######.##),~
               at (17,20), fac(hex(84)),    xtmin  (10),pic(-#######.##),~
               at (18,20), fac(hex(84)),    xtmin  (11),pic(-#######.##),~
               at (19,20), fac(hex(84)),    xtmin  (12),pic(-#######.##),~
                                                                         ~
               at (08,52), fac(hex(84)),    xtbase ( 1),pic(-#######.##),~
               at (09,52), fac(hex(84)),    xtbase ( 2),pic(-#######.##),~
               at (10,52), fac(hex(84)),    xtbase ( 3),pic(-#######.##),~
               at (11,52), fac(hex(84)),    xtbase ( 4),pic(-#######.##),~
               at (12,52), fac(hex(84)),    xtbase ( 5),pic(-#######.##),~
               at (13,52), fac(hex(84)),    xtbase ( 6),pic(-#######.##),~
               at (14,52), fac(hex(84)),    xtbase ( 7),pic(-#######.##),~
               at (15,52), fac(hex(84)),    xtbase ( 8),pic(-#######.##),~
               at (16,52), fac(hex(84)),    xtbase ( 9),pic(-#######.##),~
               at (17,52), fac(hex(84)),    xtbase (10),pic(-#######.##),~
               at (18,52), fac(hex(84)),    xtbase (11),pic(-#######.##),~
               at (19,52), fac(hex(84)),    xtbase (12),pic(-#######.##),~
                                                                         ~
               at (08,68), fac(hex(84)),    xtpct  ( 1),pic(-#####.####),~
               at (09,68), fac(hex(84)),    xtpct  ( 2),pic(-#####.####),~
               at (10,68), fac(hex(84)),    xtpct  ( 3),pic(-#####.####),~
               at (11,68), fac(hex(84)),    xtpct  ( 4),pic(-#####.####),~
               at (12,68), fac(hex(84)),    xtpct  ( 5),pic(-#####.####),~
               at (13,68), fac(hex(84)),    xtpct  ( 6),pic(-#####.####),~
               at (14,68), fac(hex(84)),    xtpct  ( 7),pic(-#####.####),~
               at (15,68), fac(hex(84)),    xtpct  ( 8),pic(-#####.####),~
               at (16,68), fac(hex(84)),    xtpct  ( 9),pic(-#####.####),~
               at (17,68), fac(hex(84)),    xtpct  (10),pic(-#####.####),~
               at (18,68), fac(hex(84)),    xtpct  (11),pic(-#####.####),~
               at (19,68), fac(hex(84)),    xtpct  (12),pic(-#####.####),~
                                                                         ~
               at (08,36), fac(hex(84)),    xtmax  ( 1),pic(-#######.##),~
               at (09,36), fac(hex(84)),    xtmax  ( 2),pic(-#######.##),~
               at (10,36), fac(hex(84)),    xtmax  ( 3),pic(-#######.##),~
               at (11,36), fac(hex(84)),    xtmax  ( 4),pic(-#######.##),~
               at (12,36), fac(hex(84)),    xtmax  ( 5),pic(-#######.##),~
               at (13,36), fac(hex(84)),    xtmax  ( 6),pic(-#######.##),~
               at (14,36), fac(hex(84)),    xtmax  ( 7),pic(-#######.##),~
               at (15,36), fac(hex(84)),    xtmax  ( 8),pic(-#######.##),~
               at (16,36), fac(hex(84)),    xtmax  ( 9),pic(-#######.##),~
               at (17,36), fac(hex(84)),    xtmax  (10),pic(-#######.##),~
               at (18,36), fac(hex(84)),    xtmax  (11),pic(-#######.##),~
               at (19,36), fac(hex(84)),    xtmax  (12),pic(-#######.##),~
                                                                         ~
               at (21,02), fac(hex(a4)) ,  errormsg$            , ch(79),~
               at (22,02), "Press any PF Key to RETURN",                 ~
                                                                         ~
               key(keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L16810

L20000: REM *************************************************************~
            *           S U B   C A L L I N G   S E C T I O N           *~
            *                                                           *~
            * THIS SECTION SETS/FIGURES THE AMOUNT TO DEDUCT.  FOR EACH *~
            * EMP DED LINE, THE MTHD IS USED TO READ THE DDT RECORD,    *~
            * WHICH IN TURN CONTAINS THE CALC ROUTINE NAME.  THIS IS    *~
            * THEN USED TO DETERMINE HOW THE DEDUCT AMOUNT IS FIGURED.  *~
            * THE DDT RECORD ALSO CONTAINS THE DEDUCTION CONSTANTS,     *~
            * SUCH AS FICA RATE(S), FUTA RATE, ECT.  NOTE THAT EMPLOYEE *~
            * SPECIFIC VARIABLES ARE IN AMOUNT() - THESE MAY BE USED IN *~
            * CONJUNTION WITH THE CONSTANTS TO ARRIVE AT THE DED AMOUNT.*~
            * ALSO NOTE THAT WHETHER THE EMPLOYEE PAYS IT OR NOT IS NOT *~
            * IMPORTANT HERE. YOU SHOULD SEE THAT THERE IS A *DIRECT*   *~
            * RELATIONSHIP BETWEEN WHAT IS HERE AND WHAT IS SET UP IN   *~
            * PRLDDT FILE.  INADVERTANTLY CHANGING THE VARIABLE FIELDS  *~
            * IN DDT CAN THROW THE DEDUCTION INTO NEVER-NEVER LAND..... *~
            * NOTICE THAT THE MAJORITY OF DEDUCTIONS CAN BE DONE BY     *~
            * THE 'FICA' SECTION. IE, A % OF GROSS, 0 LIMIT MAINS NO LMT*~
            *************************************************************

        REM Tweek earnings for calculation of deduction we are on.       ~
            First reduce base(s) by any exempt earnings...
            sbjct(1,1) = units   - max(notsub(t%,1), 0) /* ONLY $ */
            sbjct(1,2) = dollars - max(notsub(t%,2), 0) /* SUBJECT*/

        REM Now reduce 'base' by any exempt deductions we have passed...

*        Following are the deductions that are based on
*        GROSS - K401 - EXEMPT125 (QBENS),  or just GROSS - EXEMPT125
*        Note that these are by 'ROUTINE$', not 'DEDMETHOD$'. If you have
*        two deductions using same routine and one is tax exempt and
*        the other not, then the check here will have to be based on
*        DEDMETHOD, ie. the 'method' will have to be hardcoded.
*        These below are based on ROUTINE because only one method uses
*        each routine (there is nothing generic about CA-IT!).  We're
*        trying to avoid hard coding METHODS if at all possible (only
*        FIT uses DEDMETHOD$ presently).
*        Note the default for a deduction is to include 401K & EXEMPT125

*        Most if not all state income taxes are based on gross less 401k
            if routine$= "FIT     " then L20640
            if routine$= "AZ-IT   " then L20640
            if routine$= "AL-IT   " then L20640
            if routine$= "AR-IT   " then L20640
            if routine$= "CA-IT   " then L20640
            if routine$= "CT-IT   " then L20640
            if routine$= "GA-IT   " then L20640
            if routine$= "ID-IT   " then L20640
            if routine$= "OR-IT   " then L20640
            if routine$= "KS-IT   " then L20640
            if routine$= "KT-IT   " then L20640
            if routine$= "VA-IT   " then L20640
            if routine$= "VT-IT   " then L20640
            if routine$= "IA-IT   " then L20640
            if routine$= "IL-IT   " then L20640
            if routine$= "MA-IT   " then L20640
            if routine$= "MD-IT   " then L20640
            if routine$= "ME-IT   " then L20640
            if routine$= "MO-IT   " then L20640
            if routine$= "OH-IT   " then L20640
            if routine$= "OK-IT   " then L20640
            if routine$= "NC-IT   " then L20640
            if routine$= "NC-IT1  " then L20640
            if routine$= "SC-IT   " then L20640
            if routine$= "UT-IT   " then L20640
            if routine$= "FICAEXMP" then L20640
            if routine$= "FICA    " then L20680  /* EXEMPT125 only */
            goto L20710 /* Deductions is based on gross, period */

L20640: REM Deductions based on gross, less 401K, less EXEMPT125...
            sbjct(1,2)=max(sbjct(1,2)-k401-exempt125, 0)
            goto L20710

L20680: REM Deductions based on gross less EXEMPT125 (only)...
            sbjct(1,2)=max(sbjct(1,2)-exempt125, 0)

L20710:     if dedempflag$ = "Y" then L20730
               goto L21000  /* Employer Pays, Who cares if I have any?*/
L20730:     if netpay  < .001 then return        /* Nothing Left! */
                                                 /* Sucked Dry!   */

L21000: REM *************************************************************~
            * How Is The Deduction Amount Calculated? This Is The Main  *~
            * Branch Section - Sends Us Where We Need To Be...          *~
            *************************************************************

            if routine$ = "401K    " then ira:  /* 401k CONTRIBUTIONS  */
            if routine$ = "401K+   " then employer_ira:
            if routine$ = "401K+SYS" then employer_401k:
            if routine$ = "QBENS   " then qbens:
            if routine$ = "FIT     " then fit:  /* FEDERAL INCOME TAX  */
            if routine$ = "AZ-IT   " then az_it:/* ARIZONA INCOME TAX  */
            if routine$ = "AL-IT   " then al_it:/* ALABAMA INCOME TAX  */
            if routine$ = "AR-IT   " then ar_it:/* Arkansas Income Tax */
            if routine$ = "CA-IT   " then ca_it:/* CALIFORNIA INCM TAX */
            if routine$ = "CT-IT   " then ct_it:/* CONNECTICUT INC TAX */
            if routine$ = "GA-IT   " then ga_it:/* GEORGIA INCOME TAX  */
            if routine$ = "ID-IT   " then id_it:/* IDAHO INCOME TAX    */
            if routine$ = "OR-IT   " then or_it:/* Oregon INCOME TAX   */
            if routine$ = "NC-IT   " then nc_it:/* N. C.  INCOME TAX   */
            if routine$ = "NC-IT1  " then nc_it1:/* N. C.  INCOME TAX  */
            if routine$ = "KS-IT   " then oh_it:/* Kansas INCOME TAX   */
            if routine$ = "KS-IT   " then ks_it:/* Kansas INCOME TAX   */
            if routine$ = "KT-IT   " then kt_it:/* Kentucky Income Tax */
            if routine$ = "VA-IT   " then va_it:/* VIRGINIA INCOME TAX */
            if routine$ = "VT-IT   " then vt_it:/* VERMONT  INCOME TAX */
            if routine$ = "IA-IT   " then ia_it:/* Iowa Inc Tax        */
            if routine$ = "IL-IT   " then il_it:/* ILLINOIS INC. TAX   */
            if routine$ = "MA-IT   " then ma_it:/* MASS. INCOME TAX    */
            if routine$ = "MD-IT   " then md_it:/* MARYLAND INCOME TAX */
            if routine$ = "ME-IT   " then me_it:/* MAIN INCOME TAX     */
            if routine$ = "MO-IT   " then mo_it:/* MISSOURI INCOME TAX */
            if routine$ = "OH-IT   " then oh_it:/* OHIO INCOME TAX     */
            if routine$ = "OK-IT   " then ok_it:/* OKLAHOMA INCOME TAX */
            if routine$ = "SC-IT   " then sc_it:/* S. C.  INCOME TAX   */
            if routine$ = "UT-IT   " then ut_it:/* Utah INCOME TAX     */
            if routine$ = "FICA    " then fica: /* less any EXEMPT125  */
            if routine$ = "FICAEXMP" then fica: /*less 401K & EXEMPT125*/
            if routine$ = "LIKEFICA" then fica:
            if routine$ = "FICATYPE" then ficatype:
            if routine$ = "$PERHOUR" then cents_per_hour:    /* WA L&I */
            if routine$ = "DOL/HOUR" then dollars_per_hour:
            if routine$ = "WITHHOLD" then withhold:
            if routine$ = "FLAT    " then flat:
            if routine$ = "SYSFLAT " then sysflat:
            if routine$ = "%_OF_NET" then pct_of_net:
            if routine$ = "TABLENET" then tablenet:
            if routine$ = "SYSTMPCT" then systmpct:

            errormsg1$ = "REQUEST FOR UNDEFINED CALCULATION METHOD "     ~
                          & "(" & hex(84) & routine$
            errormsg1$ = errormsg1$ & ")."
            gosub'99 (errormsg1$, "Record In PRLDDT File is Invalid.",   ~
                                  "(Specifically, The ROUTINE Field)")

        REM *************************************************************~
            *      F E D E R A L    T A X E S   D O N E   H E R E       *~
            *                                                           *~
            * ALSO THE ROUTINE 'FICA' RESIDES HERE... IT IS USED FOR    *~
            * ANY DEDUCTION THAT IS A PERCENTAGE OF GROSS PAY (SUBJECT) *~
            * UP TO A LIMIT. (LIMIT IS COMPARED TO YTD SBJCT GROSS PAY) *~
            * MANY STATE AND LOCAL TAXES ARE LIKE THIS, AS IS FUTA.     *~
            *************************************************************

        ira:      REM Employee 401(k) Contributions. All deductions that ~
                  are based on gross less IRA *MUST* be calced *AFTER*   ~
                  IRA.  Deductions are calcd in the order they are found.~
                  This means that the user has the resposibility to make ~
                  the IRA deduction the first in the employee's list.    ~
                  If the deduction is based on gross only, it doesn't    ~
                  matter if it is before or after IRA.  The variable K401~
                  is referenced in some deductions.  This variable will  ~
                  contain the calced IRA amount, and is used to reduce   ~
                  the gross subject to the deduction.  Modify the        ~
                  reference to this variable to change a deduction from  ~
                  being based on gross to being based on gross less IRA, ~
                  or visa-versa.  Note that AZ-IT, and possibly          ~
                  others are already exempt by nature of their           ~
                  withholding calculation.

                  amount = 0
                  limit = amount(5)   /* 'GOAL' field */
                  if abs(limit) < .01 then limit = 9e9
                  if limit <= dedamt(4) then return /* GOAL REACHED?*/

                  gosub ficatype      /* Stomps LIMIT */
                  limit = amount(5)   /* 'GOAL' field */
                  if abs(limit) < .01 then limit = 9e9
                  amount = max(0, amount + amount(3))
                  amount = max(min(amount, limit - dedamt(4)), 0)
                  amount = round(amount, 2)
                  k401 = k401 + amount
                  return

        qbens:    REM Employee cafeteria Contributions. All deductions   ~
                  that are based on gross less QBENS *MUST* be calced    ~
                  *AFTER* no tax.  Deductions are calcd in the order they~
                  found. This means that the user has the resposibility  ~
                  to insure the QBENS deduction is first on empl list.   ~
                  If the deduction is based on gross only, it doesn't    ~
                  matter if it is before or after QBENS.  The variables  ~
                  K401 is referenced by most tax deductns.  The variable ~
                  EXEMPT125 is referenced by the remaining tax deductns. ~
                  These variables will contain the exempt amounts, and   ~
                  are used to reduce the gross subject to the deduction. ~
                  Modify the table at 20000 to change a deduction from   ~
                  being based on gross to being based on gross less      ~
                  QBENS, or visa-versa. Note that AZ-IT, and             ~
                  possibly others are already exempt by nature of their  ~
                  withholding calculation.

                  limit = amount(5)
                  if abs(limit) < .01 then limit = 9e9
                  if limit <= dedamt(4) then return /* GOAL REACHED?*/

                  amount = (amount(1)/100) * sbjct(1,2)
                  amount = max(0, amount + amount(2))
                  amount = max(min(amount, limit - dedamt(4)), 0)
                  amount = round(amount, 2)
                  exempt125 = exempt125 + amount
                  return

        fit:      REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                  REM AMOUNT(2) IS ANY FLAT AMOUNT TO ADD
                  REM AMOUNT(3) IS A STRAIGHT %OF GROSS TO ADD

                  call "PRLWHTX"  (sbjct(1,2), amount(1), amount,        ~
                                   payfreq$,  fstatus$ & dedmethod$, #11)

                  tempy = (amount(3)/100) * sbjct(1,2)
                  amount = max(0, round(amount + amount(2) + tempy,2))
                  did_fed% = 1     /* SAVE BECAUSE SOME OTHER TAXES */
                  fed_it = amount    /* ARE BASED ON THE FIT AMOUNT */
                  return

        fica:     REM CONSTANTS(1) IS RATE, (2) IS LIMIT
                  if constants(1) = 0 then gosub'99 ("The Rate Is Zero Fo~
        ~r a " & routine$ & " Deduction", methodsname$, " ")
                  limit = constants(2)
                  if abs(limit) < .01 then limit = 9e9
                  sbjct(1,2) = min(sbjct(1,2), limit - sbjct(4,2))
                  if sbjct(1,2) > .01 then amount =                      ~
                          max(0, round((constants(1)*.01)*sbjct(1,2), 2))
                  if dedmethod$ <> "FICAEE" then L23320
                  did_fica% = 1     /* SAVE BECAUSE SOME OTHER TAXES */
                  fica1 = amount    /* ARE BASED ON EMPLEE FICA AMOUNT */
                  fica2 = dedamt(4) /* USED IN MA-IT ROUTINE */
L23320:           return

        employer_ira:    REM EMPLOYER'S 401(k) CONTRIBUTION.
*                AMOUNT(1) is % to match (ie Employer says I'll match
*                X% of what employee pays).  AMOUNT(2) is flat dollars
*                to add to the percentage, so if percentage is zero then
*                the flat amount IS the amount the employer will pay.
*                AMOUNT(3) is the annual limit the employer will pay.

                  amount = (k401/100) * amount(1)
                  amount = round(amount + amount(2), 2)
                  limit = amount(3)
                  if abs(limit) < .01 then limit = 9e9
                  amount = max(min(amount, limit - dedamt(4)), 0)
                  return

        employer_401k:   REM  NEW  EMPLOYER'S 401(k) CONTRIBUTION.
*                CONSTANT(1) is % to match (ie. Employer says I'll match
*                X% of what employee pays).  CONSTANT(2) is maximum %
*                Employer will match (ie. I'll match X% upto Y% of Emply
*                Gross and NO MORE.  CONSTANT(3) is a flat dollar amount
*                to add to the percentage, so if percentage is zero then
*                the flat amount IS the amount the employer will pay.
*                CONSTANT(4) is the annual limit the employer will pay.

                  amount = min(((constants(1)/100) * k401), (sbjct(1,2) *~
                                (constants(2)/100) * (constants(1)/100)))
                  amount = round(amount + constants(3), 2)
                  limit = constants(4)
                  if abs(limit) < .01 then limit = 9e9
                  amount = max(min(amount, limit - dedamt(4)), 0)
                  return

        al_it:    /********ALABAMA STATE INCOME TAX CALCULATION**********/
                 REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                 REM AMOUNT(2) IS THE # FO DEPENDENTS FOR PRLWHTAX
                 REM AMOUNT(3) IS THE ADDITIONAL TAX WITHOLDING AMOUNT
                 REM CONSTANT (1) IS ZERO BRACKET AMOUNT FOR SINGLE
                 REM CONSTANT (2) IS ZERO BRACKET AMOUNT FOR MARRIED
                 REM CONSTANT (3) IS ZERO BRACKET PERCENTAGE LIMIT
                 REM CONSTANT (4) IS SINGLE PERSONAL EXEMPTION AMOUNT
                 REM CONSTANT (5) IS MARRIED PERSONAL EXEMPTION AMOUNT
                 REM CONSTANT (6) IS FIT ALLOWANCE PERCENTAGE
                 if did_fed% = 0 then gosub'99 ("Can't calculate Alabama ~
        ~State Income Tax, Because The Federal Income Tax", "Has Not Been ~
        ~Calculated. The FED Deduction Should Come Before The State In",  ~
        "The Employee Payroll Master File.")
                 gosub get_factor
                 taxable = sbjct(1,2) * factor
                 maxmum = 0 : perexempt = 0

                 if sstatus$ = "M" then maxmum = constants(2)
                 if sstatus$ = "S" then maxmum = constants(1)
                 if amount(1) = 0  then maxmum = constants(1)
                 if sstatus$ = "M" then perexempt = constants(5)
                 if sstatus$ = "S" then perexempt = constants(4)
                 if amount(1) = 0  then perexempt = 0

                 taxable = taxable - (                                   ~
                                min(maxmum, (constants(3)/100) * taxable)~
                                + (fed_it * factor * (constants(6)/100)) ~
                                + perexempt )

                 taxable = taxable / factor

                 if amount(1) = 0  then  tstatus$ = "S"                  ~
                                   else  tstatus$ = sstatus$

                 call "PRLWHTX" (taxable, amount(2), amount, payfreq$,   ~
                                    tstatus$ & dedmethod$, #11)

                 amount = max(0, round(amount,2)) + amount(3)
                 return

        az_it:    /******************************************************/
                  REM ARIZONA IS EASY, BUT FEDERAL *HAS* TO COME BEFORE IT
                  REM NOTE THAT AZ-IT IS 401K EXEMPT BY NATURE OF CALCLTN
                  REM NOTE THAT AZ-IT IS 'QBENS' EXEMPT BY NATURE OF CALC
                  if did_fed% = 0 then gosub'99 ("Cant calculate Arizona ~
        ~State Income Tax, Because The Federal Income Tax", "Has Not Been ~
        ~Calculated... The FED Deduction Should Come Before The State In",~
        "The Employee Payroll Master File.")

                  amount = (amount(1)/100) * fed_it
                  amount = max(0, round(amount,2)) + amount(2)
                  return

        ca_it:    /****** CALIFORNIA STATE INCOME TAX CALCULATION *******/
                  gosub get_factor        /* ANNUALIZE GROSS PAY, SET   */
                  taxable = sbjct(1,2)*factor   /* VARIABLES TO LOW STUF*/

                          /* AMOUNT(1) = REGULAR EXEMPTIONS             */
                          /* AMOUNT(2) = ESTIMATED DEDUCTIONS           */
                          /* AMOUNT(3) = FLAT ADD ON                    */
                          /*                                            */
                          /* CONSTANTS(1) = LOW STANDARD DEDUCTION      */
                          /* CONSTANTS(2) = HIGH STANDARD DEDUCTION     */
                          /* CONSTANTS(3) = TAX CREDIT PER REG. EXEMPT. */
                          /* CONSTANTS(4) = EST. DED CREDIT             */
                          /* CONSTANTS(5) = LOW INCOME EXEMPT - LOW     */
                          /* CONSTANTS(6) = LOW INCOME EXEMPT - HIGH    */

                  amount = 0
                  if sstatus$ = "H" then L24405
                  if sstatus$ = "M" and amount(1) > 1 then L24405
                     if taxable <= constants(5) then L24460
                     taxable = max(0, taxable - constants(1))
                     goto L24425
L24405:           REM HIGH STUFF
                     if taxable <= constants(6) then L24460
                     taxable = max(0, taxable - constants(2))
                  REM AND ON WE GO...
L24425:              taxable = max(0, taxable - (constants(4)*amount(2)))
                     if taxable <= 0 then L24460

                  taxable = taxable/factor
                  call "PRLWHTX"  (taxable, amount(1), amount, payfreq$, ~
                                              sstatus$ & dedmethod$, #11)

L24460:           amount = max(amount-(amount(1)*constants(3)/factor), 0)~
                                        + amount(3) /* ADDTNL $ TO ADD */
                  return


        ga_it:    /******** GEORGIA STATE INCOME TAX CALCULATION ********/
                  gosub get_factor               /* ANNUALIZE GROSS PAY */
                  taxable = sbjct(1,2)*factor

                  REM FIGURE STANDARD DEDUCTION
                  standard = 1500        /* JUST IN CASE */
                  if sstatus$="S" or sstatus$="H" then standard = 2300

                  REM 'J' MEAN MARRIED FILING JOINT, ONE SPOUSE WORKS
                  if sstatus$="J" then standard = 3000

                  REM 'M' MEANS MARRIED FILING JOINT, BOTH WOKING OR
                  REM           MARRIED FILING SEPERATE
*                IF SSTATUS$="M" THEN STANDARD =  1500                  ~

                  personal = 1500
                  if sstatus$ = "J" then personal = 3000

         REM          THE DEPENDENT ALLOWANCE IS ENTERED AS PART OF THE  ~
                      TAX TABLE, EXCEPT FOR THE ADDITIONAL FLAT AMOUNT   ~
                      FOR HEAD OF HOUSE HOLD WITH AT LEAST ONE DEPENDENT

*                IF SSTATUS$ = "H" AND AMOUNT(1) > 0 THEN PERSONAL =    ~
*                   PERSONAL + 800  /*HEADOFHOUSE WITH ONE OR MORE DEP*/

         REM          DE-ANNUALIZE THE SUM OF ALL DEDUCTIONS, THEN SUB-  ~
                      TRACT IT FROM CURRENT GROSS (SBJCT), SEND TO TABLES
                  taxable=sbjct(1,2)- round((standard+personal)/factor,2)
                  if sstatus$ = "J" then tstatus$ = "H"                  ~
                                    else tstatus$ = sstatus$
                  call "PRLWHTX"  (taxable, amount(1), amount, payfreq$, ~
                                              tstatus$ & dedmethod$, #11)

                  amount = max(0, amount) + amount(2)
                        REM JUST ADDED ANY ADDITIONAL REQUESTED AMOUNT
                  return

        id_it:   REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                 REM AMOUNT(2) IS THE ADDITIONAL TAX WITHOLDING AMOUNT
                 REM CONSTANT (1) IS ZERO BRACKET AMOUNT FOR SINGLE
                 REM CONSTANT (2) IS ZERO BRACKET AMOUNT FOR MARRIED

                 gosub get_factor
                 zerobracket = 0
                 if sstatus$ = "S" then zerobracket = constants(1)/factor
                 if sstatus$ = "M" then zerobracket = constants(2)/factor
                 call "PRLWHTX" (max(sbjct(1,2) - zerobracket, 0),       ~
                 amount(1), amount, payfreq$, sstatus$ & dedmethod$, #11)

                 amount = max(0, round(amount, 2)) + amount(2)
                 return

        or_it:   REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                 REM AMOUNT(2) IS THE ADDITIONAL TAX WITHOLDING AMOUNT
                 REM CONSTANT (1) IS MAXIMUM AMOUNT OF FIT ALLOWED
                 REM CONSTANT (2) IS THEN PERSONAL ALLOWANCE, BUT WE'LL  ~
                                  CONTINUE TO LOOK IN TABE IF ZERO

                 if did_fed% = 0 then gosub'99 ("Can't calculate Oregon S~
        ~tate Income Tax, Because The Federal Income Tax", "Has Not Been C~
        ~alculated... The FED Deduction Should Come Before The State In", ~
        "The Employee Payroll Master File.")
                 gosub get_factor : tstatus$ = sstatus$
                 zerobracket = max(min(constants(1)/factor, fed_it), 0)
                 if amount(1) > 2 then tstatus$ = "M"
                 call "PRLWHTX" (max(sbjct(1,2) - zerobracket, 0),       ~
                 0, amount, payfreq$, tstatus$ & dedmethod$, #11)

                 REM Everyone just HAS to be different, you know...
                 allowamt = constants(2)
                 if allowamt > 0 then L24920
                 call "READ100"  (#11, tstatus$ & dedmethod$, f1%(11))
                     if f1%(11) <> 0% then L24910
                   call "ASKUSER" (2%, "****** NOTE ******",             ~
                   "The Oregon Tax Table was not found, it needs",       ~
                   "to be created before processing can continue.",      ~
                   "Press ENTER to EXIT.")
                       goto L65000
L24910:          get #11, using L24915, allowamt
L24915:          FMT XX(37), PD(14,4)
L24920:          allowance = (amount(1)*allowamt)/factor
                 amount = max(0, amount-allowance)
                 amount = round(amount + amount(2), 2)
                 return

        ks_it:   REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                 REM AMOUNT(2) IS THE ADDITIONAL TAX WITHOLDING AMOUNT
                 REM CONSTANT (1) IS % OF 'FIT' ALLOWANCE MARRIED
                 REM CONSTANT (2) IS % OF 'FIT' ALLOWANCE SINGLE
                 REM TABLE 'A' IS TO CALC THE STANDARD DEDUCTION, MARRIED
                 REM TABLE 'J' IS TO CALC THE STANDARD DEDUCTION, SINGLE
                 REM TABLE 'M' & 'S' ARE TO CALC TAX AMOUNT, AS USUAL
                 REM FEDERAL INC TAX *HAS* TO COME BEFORE THIS...
                 if did_fed% = 0 then gosub'99 ("Can't calculate Kansas S~
        ~tate Income Tax, Because The Federal Income Tax", "Has Not Been C~
        ~alculated... The FED Deduction Should Come Before The State In", ~
        "The Employee Payroll Master Files.")

                 temp$ = "J" : if sstatus$ = "M" then temp$ = "A"
                 fit_pct = constants(2)
                    if sstatus$ = "M" then fit_pct = constants(1)
                 fed_allow = (fit_pct/100) * fed_it
                 call "PRLWHTX" (sbjct(1,2), 0, std_dedctn, payfreq$,    ~
                                                 temp$ & dedmethod$, #11)
                 std_dedctn = std_dedctn + fed_allow

                 REM Actually Calc Tax Now...
                 call "PRLWHTX" (max(sbjct(1,2) - std_dedctn, 0),        ~
                 amount(1), amount, payfreq$, sstatus$ & dedmethod$, #11)
                 amount = max(0, amount) + amount(2)
                 return

        kt_it:  /******* KENTUCKY STATE INCOME TAX CALCULATION **********/
                 REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                 REM AMOUNT(2) IS THE ADDITIONAL TAX WITHOLDING AMOUNT
                 REM CONSTANTS(1) IS STANDARD DEDUCTION AMOUNT
                 REM CONSTANTS(2) IS PERSONAL EXEMPTION CREDIT
                 REM CONSTANTS(3) IS FEDERAL ADD ON PERCENT
                 if did_fed% = 0 then gosub'99 ("Can't calculate Kentucky~
        ~ State Income Tax, Because The Federal Income Tax", "Has Not Been~
        ~ Calculated. The FED Deduction Should Come Before The State In", ~
        "The Employee Payroll Master File.")
                 gosub get_factor

                 taxable = max((sbjct(1,2) + (fed_it*(constants(3)/100)) ~
                              - (constants(1)/factor)), 0)

                 call "PRLWHTX" (taxable, 0, amount, payfreq$,           ~
                                              sstatus$ & dedmethod$, #11)

                 amount = amount - (amount(1) * (constants(2)/factor))
                 amount = max(0, round(amount,2)) + amount(2)
                 return

        va_it:   REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                 REM AMOUNT(2) IS THE ADDITIONAL TAX WITHOLDING AMOUNT
                 REM CONSTANT (1) IS ZERO BRACKET AMOUNT

                 gosub get_factor
                 zerobracket = constants(1)/factor
                 call "PRLWHTX" (max(sbjct(1,2) - zerobracket, 0),       ~
                                 amount(1), amount, payfreq$,            ~
                                 sstatus$ & dedmethod$, #11)
                 amount = max(0, round(amount, 2)) + amount(2)
                 return

        md_it:    /******************************************************/
                  gosub get_factor        /* AMOUNT(1) IS NUMBER OF EXMP*/
                  maxmum = constants(2)/factor  /* MAX STNDRD DEDUCTION */
                  minmum = constants(3)/factor  /* MIN STNDRD DEDUCTION */
                  standard = max(minmum, min((constants(1)/100) *        ~
                                                   sbjct(1,2), maxmum))
                  taxable = sbjct(1,2) - standard
                  call "PRLWHTX"  (taxable, amount(1), amount, payfreq$, ~
                                              sstatus$ & dedmethod$, #11)

                  if amount <= 0 then amount = 0
                  amount = amount + amount * (amount(2) / 100) /* LOCAL */

                  amount = max(amount,0)+amount(3) /* ADDITION $ TO ADD */
                  return

        vt_it:    REM AMOUNT(1) IS THE ADDITIONAL TAX W/H AMOUNT
                  REM WIERD ONE, GRADUATED PERCENT OF FED_IT
                  taxable = fed_it
                  call "PRLWHTX" (taxable, 0, amount,                    ~
                                   payfreq$, sstatus$ & dedmethod$, #11)

                  amount = max(0, round(amount,2)) + amount(1)
                  return

        ma_it:    REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                  REM AMOUNT(2) IS THE ADDITIONAL W/H TAX AMOUNT
                  REM AMOUNT(3) IS FLAG FOR WHEHTER EMPLOYEE IS BLIND
                  REM CONSTANTS(1) IS MAXIMUM ANNUAL FICA DEDUCTION AMNT
                  REM CONSTANTS(2) IS SINGLE  ANNUAL EXEMPTION AMOUNT
                  REM CONSTANTS(3) IS MULTILPLE DEPENDENT EXEMPT. AMOUNT
                  REM CONSTANTS(4) IS FLAT DEPENDENT EXEMPTION AMOUNT
                  REM CONSTANTS(5) IS INCOME TAX WITHHOLDING PERCENTAGE

                  if did_fica% = 0 then gosub'99("CAN'T CALCULATE MASSACH~
        ~USETTE INCOME TAX, BECAUSE THE EMPLOYEE F.I.C.A.", "HAS NOT BEEN ~
        ~CALCULATED... THE FED DEDUCTION SHOULD COME BEFORE THE STATE IN",~
        "THE EMPLOYEE PAYROLL MASTER FILE.")

                  gosub get_factor
                  taxable = sbjct(1,2)
                  if fica2 >= constants(1) then L25795
                     ficamt  = min((constants(1)-fica2),fica1)
                     taxable = max((taxable-ficamt), 0)
L25795:           if amount(1) = 1 then L25810
                  if amount(1) > 1 then L25830
                  goto L25855
L25810: REM Calculate Exemption Deduction with 1 Exemption
                  flatamt = constants(2)/factor
                  taxable = max(taxable-flatamt, 0)
                  goto L25855
L25830: REM Calculate Exemption Deduction with Greater than 1 Exemption
                  dependamnt = constants(3)/factor
                  flatamt = constants(4)/factor
                  depded = dependamnt * amount(1)
                  taxable = max((taxable-depded)-flatamt, 0)
L25855: REM Calculate Tax Withholding
                  taxrate = constants(5)/100
                  amount = taxable * taxrate
        REM Calculate Blind Exemption Credit
                  if amount(3) = 0 then L25890
                  blindamt = (constants(2)/factor) * taxrate
                  amount   = amount - blindamt
L25890:           amount = max(0, round(amount,2)) + amount(2)
                  return

        oh_it:    REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
        il_it:    REM AMOUNT(2) IS THE ADDITIONAL TAX W/H AMOUNT

                  call "PRLWHTX" (sbjct(1,2), amount(1), amount,         ~
                                   payfreq$, sstatus$ & dedmethod$, #11)

                  amount = max(0, round(amount,2)) + amount(2)
                  return

        me_it:    /******************************************************/
                  /* AMOUNT(1) IS NUMBER OF EXEMPTIONS                  */
                  /* AMOUNT(2) IS THE ADDITIONAL AMOUNT TO BE WITHHELD  */
                  /* AMOUNT(3) IS THE % TO BE WITHHELD FOR LOCAL TAX    */
                  /* CONSTANT(1) IS THE STD DEDUCTION %                 */
                  /* CONSTANT(2) IS THE STD DEDUCTION AMNT FOR MARRIED  */
                  /* CONSTANT(3) IS THE STD DEDUCTION AMNT FOR SINGLE   */
                  gosub get_factor
                  if sstatus$ = "M" then maxmum1 = constants(2)/factor
                  if sstatus$ = "S" then maxmum1= constants(3)/factor
                  maxmum = round(maxmum1,0)
                  taxable = sbjct(1,2) - min((constants(1)/100) *        ~
                                                      sbjct(1,2), maxmum)
                  call "PRLWHTX"  (taxable, amount(1), amount, payfreq$, ~
                                              sstatus$ & dedmethod$, #11)
                  if amount <= 0 then amount = 0
                  amount = amount + amount * (amount(3) / 100)+amount(2)
                  return

        nc_it:    /******************************************************/
             gosub get_factor
             tempy = (sbjct(1,2) * factor)
             stddedamt = min(((constants(1)/100) * tempy), constants(2))
             stddedamt = stddedamt + amount(1)
             call "PRLWHTX"  (sbjct(1,2), stddedamt, amount, payfreq$,   ~
                                   sstatus$ & dedmethod$, #11)
             if amount <= 0 then amount = 0
             return

        nc_it1:   /******************************************************/
             gosub get_factor : tstatus$ = sstatus$
             if sstatus$ <> "M" then L26320                   /* MARRIED */
                stddedamt = constants(1) : goto L26355
L26320:      if sstatus$ <> "S" then L26330                   /* SINGLE  */
                stddedamt = constants(2) : goto L26355
L26330:      if sstatus$ <> "H" then L26340                   /* HEAD HSE*/
                stddedamt = constants(3) : goto L26355
L26340:      if sstatus$ <> "Q" then L26346                   /* WIDOWER */
                stddedamt = constants(4) : goto L26355
L26346:      if sstatus$ <> "J" then L26348                   /* JOINT   */
                stddedamt = constants(5) : goto L26355
L26348: /*   IF SSTATUS$ <> "?" THEN 24???                   /* ALL OTHR*/
                stddedamt = constants(6)

L26355:      taxable=sbjct(1,2)- round(stddedamt/factor,2)
             call "PRLWHTX"  (taxable, amount(1), amount, payfreq$,      ~
                                   tstatus$ & dedmethod$, #11)
             amount = max(0, round(amount, 2)) + amount(2)
             return

        ut_it:    /******************************************************/
                  REM AMOUNT(1) IS THE NUMBER OF EXEMPTIONS
                  REM AMOUNT(2) IS ANY FLAT AMOUNT TO ADD
                  REM AMOUNT(3) IS A STRAIGHT %OF GROSS TO ADD
                  if did_fed% = 0 then gosub'99    ("Cant calculate Utah ~
        ~State Income Tax, Because The Federal Income Tax", "Has Not Been ~
        ~Calculated... The FED Deduction Should Come Before The State In",~
        "The Employee Payroll Master File.")

                  deduct = (constants(1)/100) * fed_it
                  call "PRLWHTX"  (sbjct(1,2)-deduct, amount(1), amount, ~
                                   payfreq$,  sstatus$ & dedmethod$, #11)
                  if amount <= 0 then amount = 0

                  tempy = (amount(3)/100) * sbjct(1,2)
                  amount = max(0, round(amount + amount(2) + tempy,2))
                  return

        sc_it:    /******************************************************/
             gosub get_factor
             tempy = (sbjct(1,2) * factor)
             if amount(1) > 0  then  goto  L26530
             stddedamt = 0.0
             goto L26545
L26530:      max_percent = constants(2)
             if amount(1) >= 2.0 then max_percent = constants(3)
             stddedamt = min(((constants(1)/100) * tempy), max_percent )
L26545:      taxable = sbjct(1,2) - round( (stddedamt / factor), 2)

             call "PRLWHTX"  (taxable, amount(1), amount, payfreq$,      ~
                                   sstatus$ & dedmethod$, #11)
             if amount <= 0 then amount = 0
             amount = amount + amount(2)
             return

        ct_it:    /******************************************************/
                  /* C(1) = ANNUAL PERCENT                              */
                  /* C(2) = SINGLE (A) MAX EXEMPTION                    */
                  /* C(3) = H/HHLD (B) MAX EXEMPTION                    */
                  /* C(4) = MARRIED(C) MAX EXEMPTION                    */

             gosub get_factor
             taxable = (sbjct(1,2) * factor)
             tstatus$ = "D" : exempt = 0     /* START WITH NO SLACK  */
             if sstatus$ = "S" then tstatus$ = "A"
             if sstatus$ = "A" then tstatus$ = "A"
             if sstatus$ = "H" then tstatus$ = "B"
             if sstatus$ = "B" then tstatus$ = "B"
             if sstatus$ = "M" then tstatus$ = "C"
             if sstatus$ = "C" then tstatus$ = "C"
             if tstatus$ = "D" then L26740    /* NO SLACK */
                if tstatus$ = "A" then exempt = constants(2)
                if tstatus$ = "B" then exempt = constants(3)
                if tstatus$ = "C" then exempt = constants(4)

             taxable = max(taxable - exempt, 0)
             if taxable <= exempt then L26740  /* NO ADD BACK */

                addback = taxable + 999.50    /* GETS TRICKY NOW */
                addback = (int(addback / 1000)) * 1000
                addback = max(0, min(addback - exempt, exempt))
                taxable = taxable + addback

L26740:      amount = max(((constants(1)/100) * taxable),0)
             amount = amount / factor
             if tstatus$ = "D" then L26770    /* NO SLACK */

             taxable = (sbjct(1,2) * factor)
                            /* NOTE HARD CODED 7 = ANNUAL PAY FREQUENCY */
             call "PRLWHTX"  (taxable, 0, amount1, "7",                  ~
                                         sstatus$ & dedmethod$, #11)
             amount = amount * ((100 - amount1) / 100)

L26770:      amount = round(amount + amount(1), 2)  /* ADD ON */
             return

        ar_it:   REM C(1) = MAX MARRIED DED
                 REM C(2) = MAX JOINT   DED
                 REM C(3) = MAX SINGLE  DED
                 REM C(4) = STD DED PERCENT
                 REM C(5) = EXEMPTION CREDIT (ANNUAL)
                 REM C(5) = DEDUCTION CREDIT (ANNUAL)
                 REM A(1) = EXEMPTIONS
                 REM A(2) = DEPENDENTS
                 REM A(3) = THE EVER POPULAR ADD-ON
            gosub get_factor
            taxable = (sbjct(1,2) * factor)
            stdded = 0
            if amount(1) = 0 then L26885
            stdded = (constants(4)/100) * taxable
               if sstatus$ = "S" then stdded = min(constants(3), stdded)
               if sstatus$ = "J" then stdded = min(constants(2), stdded)
               if sstatus$ = "M" then stdded = min(constants(1), stdded)
L26885:     taxable = (taxable - stdded) / factor
            call "PRLWHTX"  (taxable, 0, amount, payfreq$,               ~
                                         sstatus$ & dedmethod$, #11)

            dedcr = min(amount(1), 2) * constants(5)
            dedcr = dedcr + amount(2) * constants(6)
            dedcr = dedcr / factor

            amount = max(0, amount - dedcr)
            amount = round(amount + amount(3), 2)
            return

        ia_it:   REM C(1) = STD DEDUCTION 0/1
                 REM C(2) = STD DEDUCTION 2+
                 REM C(3) = FIT ALLOWANCE PERCENT
                 REM C(4) = EXEMPTION CREDIT (HIGH)
                 REM C(5) = EXEMPTION CREDIT (LOW)
                 REM C(6) = MAX HIGH EXEMPTIONS
                 REM A(1) = EXEMPTIONS
                 REM A(2) = THE EVER POPULAR ADD-ON

            gosub get_factor
            taxable = (sbjct(1,2) * factor)
            stdded = constants(1)
            if amount(1) > 1 then stdded = constants(2)
            stdded = stdded + (fed_it * (constants(3)/100) * factor)
            taxable = (taxable - stdded) / factor
            call "PRLWHTX"  (taxable, 0, amount, payfreq$,               ~
                                         sstatus$ & dedmethod$, #11)
            dedno = min(amount(1), constants(6))
            dedcr = dedno * constants(4)
            dedno = max(0, amount(1) - dedno)
            dedcr = dedcr + (dedno * constants(5))
            dedcr = dedcr / factor

            amount = max(0, amount - dedcr)
            amount = round(amount + amount(2), 2)
            return

        mo_it:   REM C(1) = STD DEDUCTION MARRIED
                 REM C(2) = STD DEDUCTION SINGLE
                 REM C(3) = FIT ALLOWANCE PERCENT
                 REM C(4) = ADDITIONAL DEPENDENT ALLOWANCE
                 REM C(5) = MAX ADDED ALLOWANCE, MARRIED, SINGLE = M - 1
                 REM C(6) = MAX FEDERAL ALLOWANCE
                 REM A(1) = EXEMPTIONS
                 REM A(2) = THE EVER POPULAR ADD-ON

            gosub get_factor
            taxable = (sbjct(1,2) * factor)
            stdded = 0 : dedno = 0
            if sstatus$ = "M" then stdded = constants(1)
            if sstatus$ = "S" then stdded = constants(2)
            if sstatus$ = "M" then dedno = min(amount(1), constants(5))
            if sstatus$ = "S" then dedno =                               ~
                                max(0, min(amount(1), constants(5) - 1))
            fedmax = constants(6)
            if fedmax = 0 then fedmax = 9e7
            fedded = max(constants(6),                                   ~
                         fed_it * (constants(3)/100) * factor)

            stdded = stdded + (constants(4) * dedno) + fedded
            taxable = (taxable - stdded) / factor

            call "PRLWHTX"  (taxable, amount(1), amount, payfreq$,       ~
                                         sstatus$ & dedmethod$, #11)

            amount = max(0, amount)
            amount = round(amount + amount(2), 2)
            return

        ok_it:    /******************************************************/
                  /* C1 = PERCENT                                       */
                  /* C2 = MAXIMUM STANDARD                              */
                  /* C3 = MINIMUM STANDARD                              */
                  /* C4 = EXEMPTION CREDIT                              */
                  /* C5 = FIXED STANDARD                                */
                  /* C6 = MAXIMUM ANNUAL WAGE                           */
                  /* A1 = EXEMPTIONS                                    */
                  /* A2 = THE EVER POPULAR FIXED ADD-ON                 */
                  /* ***** CAUTION ***** ROUTIME RELIES ON SEPARATE     */
                  /*                     METHODS FOR MARRIED & SINGLE   */
                  /******************************************************/

                  gosub get_factor
                  taxable1 = sbjct(1,2) * factor
                  taxable  = min(taxable1, constants(6))

                  maxmum = constants(2)
                  minmum = constants(3)
                  standard = max(minmum, min(maxmum,                     ~
                                           taxable * (constants(1)/100)))

                  standard1 = (constants(1)/100) * (max(0, taxable -     ~
                             ((amount(1) * constants(4)) + constants(5))))

                  taxable = (taxable - standard - standard1) / factor
                  taxable1 = sbjct(1,2)

                  call "PRLWHTX"  (taxable, amount(1), amount, payfreq$, ~
                                              sstatus$ & dedmethod$, #11)

                  call "PRLWHTX"  (taxable1, 0, amount1, payfreq$,       ~
                                                   "A" & dedmethod$, #11)

                  amount = max(0, amount + amount1)
                  amount = round(amount + amount(2), 2)
                  return

        ficatype: /******************************************************/
*                AMOUNT(1) IS RATE, (2) IS LIMIT (AGAINST YTD GROSS)
*                THIS IS JUST LIKE FICA EXCEPT THAT THE RATE & LIMIT
*                ARE ENTERED AND STORED ON AN EMPLOYEE BY EMPLOYEE BASIS

                  limit = amount(2)
                  if abs(limit) < .01 then limit = 9e9
                  sbjct(1,2) = min(sbjct(1,2), limit - sbjct(4,2))
                  if sbjct(1,2) > .01 then amount =                      ~
                             max(0, round((amount(1)*.01)*sbjct(1,2), 2))
                  return

        pct_of_net: /****************************************************/
                  REM AMOUNT(1) IS % TO TAKE, AS LONG AS NET IS GREATER  ~
                      THAN OR EQUAL TO AMOUNT(2), AND GOAL (5) NOT HIT
                  limit = amount(5): if abs(limit) < .01 then limit = 9e9
                  if limit <= dedamt(4) then return /* GOAL REACHED?*/
                  if netpay < amount(2) then return /* NOT ENOUGH NET?*/
                  amount = max(0, (amount(1)*.01) * netpay)
                  amount = max(min(amount, limit - dedamt(4)), 0)
                  return

        tablenet:   /****************************************************/
                  limit = amount(5): if abs(limit) < .01 then limit = 9e9
                  if limit <= dedamt(4) then return /* GOAL REACHED?*/
                  taxable = netpay
                  call "PRLWHTX"  (taxable, amount(1), amount, payfreq$, ~
                                                   "A" & dedmethod$, #11)
                  if amount <= 0 then amount = 0
                  amount = round(amount + amount(2), 2)
                  amount = max(min(amount, limit - dedamt(4)), 0)
                  return

        flat:     /******************************************************/
                  REM AMOUNT(1) IS $ TO TAKE, AS LONG AS GOAL (5) NOT HIT
                  limit = amount(5)
                  if abs(limit) < .01 then limit = 9e9
                  if limit <= dedamt(4) then return /* GOAL REACHED?*/
                  amount = max(min(amount(1), limit - dedamt(4)), 0)
                  return

        sysflat:  /******************************************************/
                  REM CONSTANTS(1) IS AMT, AS LONG AS CONSTANTS(2) NOT HIT
                  limit = constants(2)
                  if abs(limit) < .01 then limit = 9e9
                  if limit <= dedamt(4) then return /* GOAL REACHED?*/
                  amount = max(min(constants(1), limit - dedamt(4)), 0)
                  return

        withhold: /******************************************************/
                  call "WITHHOLD" (sbjct(), amount(), dedamt(), amount)
                  return

        cents_per_hour : /***********************************************/
                         REM CONSTANTS(1) IS RATE,SBJCT(1,1) IS HOURS SBJ~
                             NOTE THAT RATE IS DEVIDED BY 100...         ~
                             ORGINALY SET UP FOR WASH WORK COMP, BUT CAN ~
                             BE USED FOR ANYTHING FIGURED SIMILARY

                           amount = (constants(1)/100) * sbjct(1,1)
                           return

        dollars_per_hour : /*********************************************/
                         REM CONSTANTS(1) IS RATE,SBJCT(1,1) IS HOURS SBJ

                           amount = constants(1) * sbjct(1,1)
                           return

        systmpct:          /*********************************************/

                           amount = (constants(1)/100) *                 ~
                                    (constants(2)/100) *                 ~
                                    (amount(1)/100) *                    ~
                                    sbjct(1,2)
                           amount = max(0, round(amount,2))
                           return

*       DIRECT:  /* Net pay up to AMOUNT(1) is $ to Deposit (HES)      */
            amount = min(amount(1), netpay)
            return

        REM *************************************************************~
            *  A N N U A L I Z E   C U R R E N T   P A Y                *~
            *                                                           *~
            * SOME OF THE ROUTINES ABOVE NEED THIS FACTOR               *~
            *************************************************************~

        get_factor

            REM FIGURE ANNUALIZED PAY FACTOR
                on pos("12345678" = payfreq$) goto L29630, L29640, L29650,  ~
                                     L29660, L29670, L29680, L29690, L29700
                gosub'99("Employee Pay Frequency Is Invalid",payfreq$," ")

L29630:         rem        WEEKLY (1) :   factor = 52    : return
L29640:         rem      BIWEEKLY (2) :   factor = 26    : return
L29650:         rem   SEMIMONTHLY (3) :   factor = 24    : return
L29660:         rem       MONTHLY (4) :   factor = 12    : return
L29670:         rem     QUARTERLY (5) :   factor = 4     : return
L29680:         rem    SEMIANNUAL (6) :   factor = 2     : return
L29690:         rem        ANNUAL (7) :   factor = 1     : return
L29700:         rem         DAILY (8) :   factor = 260   : return

L30000: REM *************************************************************~
            *   L O A D   E M P L O Y E E   M A S T E R   R E C O R D   *~
            *                                                           *~
            * LOADS THE EMPLOYEE MASTER RECORD FROM THE FILE SO THAT WE *~
            * CAN USE ALL THE INFO THERE, INCLUDING BUT NOT LIMITED TO  *~
            * THE MARITAL STATUS AND THE LIKE.                          *~
            *************************************************************

            get   # 3, using L30190,                                      ~
                       empcode$, fstatus$, sstatus$, payfreq$, mode$,    ~
                       deductflag$, dept$
            return

L30190:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                XX(4),                   /* WORKSTATION CODE           */~
                CH(1),                   /* FED FILING STATUS          */~
                CH(1),                   /* STATE FILING STATUS        */~
                CH(1),                   /* PAY FREQUENCY              */~
                CH(1),                   /* MODE OF PAYMENT            */~
                XX(4),                   /* DIRECT DEPOSIT BANK CODE   */~
                XX(12),                  /* D.D. ACCOUNT NUMBER        */~
                XX(9),                   /* CASH IN BANK ACCOUNT       */~
                XX(9),                   /* GROSS PAYROLL ACCOUNT      */~
                XX(2),                   /* NORMAL HRS/DAY ASCII(##)   */~
                XX(1),                   /* AUTOPAYROLL FLAG           */~
                XX(6),                   /* FIRST DATE PAY PERIOD      */~
                XX(6),                   /* LAST DATE PAY PERIOD       */~
                CH(1),                   /* DEDUCT ON THIS PRLDDUCT RUN*/~
                XX(02),                  /* NORMAL HRS/WEEK ASCII(##)  */~
                XX(10),                  /* WORKMANS' COMP. CODE DEF   */~
                XX(1),                   /* JOBCODE                    */~
                XX(1),                   /* SHIFT CODE                 */~
                XX(2),                   /* AREA CODE                  */~
                XX(3),                   /* O/H NUMBER                 */~
                XX(2),                   /* PRIMARY STATE CODE         */~
                CH(4),                   /* DEPARTMENT                 */~
                XX(41)                   /* FILLER FOR REST OF RECORD  */

L31000: REM *************************************************************~
            *      L O A D   A   D E D U C T I O N S   R E C O R D      *~
            *                                                           *~
            * LOADS A DEDUCTIONS RECORD FROM THE EMPLOYEE'S FILE.  THEN *~
            * RETURNS SO THAT WE CAN PROCESS THE DEDUCTION.             *~
            *************************************************************

            get   # 5, using L31300,                                      ~
                       dedcat$, dedmethod$, deduction$, dedempflag$,     ~
                       dedcracct$, deddbacct$, dedapplies$, amount(1),   ~
                       amount(2), amount(3), amount(4), amount(5),       ~
                       dedamt9(1), dedamt9(2), dedamt9(3), dedamt9(4),   ~
                       dedhold$, sbj(1,1), sbj(1,2), sbj(2,1), sbj(2,2), ~
                       sbj(3,1), sbj(3,2), sbj(4,1), sbj(4,2),           ~
                       dedrecord$
            get #5 using L31151, effr$()
L31151:         FMT POS(247), 2*CH(6)
            if effr$(1%) = " " or effr$(1%) = blankdate$ then ~
               effr$(1%) = all(hex(00))
            if effr$(2%) = " " or effr$(2%) = blankdate$ then ~
               effr$(2%) = all(hex(ff))

            mat dedamt = dedamt9
            mat sbjct = sbj
            dedamt(1), sbjct(1,1), sbjct(1,2) = 0
            return

L31300: FMT XX(12),              /* EMPLOYEE CODE                      */~
            XX(3),               /* SEQUENCE NUMBER                    */~
            XX(12),              /* EMPLOYEE NUMBER                    */~
            CH(6),               /* DEDUCTION CATEGORY (LOC?!)         */~
            CH(6),               /* METHOD OF DEDUCTION (DDT)          */~
            CH(12),              /* DEDUCTION DESCRIPTION              */~
            CH(1),               /* EMPLOYEE PAYS THIS? FLAG           */~
            CH(9),               /* CREDIT ACCOUNT                     */~
            CH(9),               /* DEBIT ACCOUNT                      */~
            CH(6),               /* APPLIES FIELD (123456)             */~
            PD(14,4),            /* AMOUNT 1 OR 0 IF NOT USED          */~
            PD(14,4),            /* AMOUNT 2 OR 0 IF NOT USED          */~
            PD(14,4),            /* AMOUNT 3 OR 0 IF NOT USED          */~
            PD(14,4),            /* AMOUNT 4 OR 0 IF NOT USED          */~
            PD(14,4),            /* GOAL                               */~
            PD(14,4),            /* CURRENT DEDUCTION AMOUNT           */~
            PD(14,4),            /* MTD         "        "             */~
            PD(14,4),            /* QTD         "        "             */~
            PD(14,4),            /* YTD         "        "             */~
            CH(08),              /* HOLD AMOUNT                        */~
            PD(14,4),            /* Current Units Subject To Deduction */~
            PD(14,4),            /* Current Dollars Subject To Deducti */~
            PD(14,4),            /* Month To Date Units Subject        */~
            PD(14,4),            /* Month To Date Dollars Subject      */~
            PD(14,4),            /* Quarter To Date Units Subject      */~
            PD(14,4),            /* Quarter To Date Dollars Subject    */~
            PD(14,4),            /* Year To Date Units subject         */~
            PD(14,4),            /* Year To Date Dollars Subject       */~
            CH(80)               /* filler for rest of record or inter */~

L32000: REM *************************************************************~
            *          L O A D   E A R N I N G S   R E C O R D          *~
            *                                                           *~
            * LOADS THE EARNINGS RECORDS AND TO PERMIT TOTALING FOR     *~
            * GROSS PAY AND THE LIKE.  UNTIL I DECIDE WHAT TO DO, THIS  *~
            * IS IT, RATHER THAN A FORMAL "PRLTOTAL" SUBROUTINE.        *~
            *************************************************************

            get   # 4, using L32130, seqnr$,                              ~
                       type$, dept$, cash$, txbl$, units$, acct$,        ~
                       pieceunits, piecedollars
            return

L32130:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE CODE (AGAIN)      */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(4),                   /* DEPARTMENT CODE            */~
                CH(1),                   /* PAID IN CASH? FLAG         */~
                CH(1),                   /* IS THIS TAXABLE?           */~
                CH(6),                   /* UNIT DESCRIPTION           */~
                XX(8),                   /* UNIT RATE                  */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                XX(32),                                                  ~
                PD(14,4),                /* CURRENT CHECK   UNITS      */~
                PD(14,4)                 /* CURRENT CHECK   AMOUNT     */

L33000: REM *************************************************************~
            *                L O A D   D D T   E N T R Y                *~
            *                                                           *~
            * LOADS AN ENTRY FROM THE DDT SO THAT WE MIGHT USE IT IN    *~
            * COMPUTING DEDUCTIONS.                                     *~
            *************************************************************

            call "READ100" (#10, dedmethod$, f1%(10))
                 if f1%(10) = 0 then return

            get #10, using L33590, routine$, constants(), methodsname$
            return         /* THIS TELLS US WHAT CODE SECTION TO GO TO */
                           /* TO CALC THE AMOUNT TO DEDUCT, AND ALSO   */
                           /* US ANY CONSTANTS NEEDED. IE FUTA RATE, ECT*/

L33590:     FMT XX(143),                 /* METHOD OF DEDUCTION        */~
                CH(8),                   /* ROUTINE NAME               */~
                XX(151),                 /*                            */~
                6*PD(14,4),              /* CONSTANT AMOUNTS           */~
                CH(30)                   /* ROUTINE DESCRIPTION        */~

L34000: REM *************************************************************~
            *        R E S A V E   D E D U C T I O N   R E C O R D      *~
            *                                                           *~
            * REWRITES DEDUCTION RECORD.  NEEDS THE AMOUNT COMPUTED BY  *~
            * THE DEDUCTION COMPUTATION ROUTINE.                        *~
            *************************************************************

            rewrite # 5, using L34150,                                    ~
                         readkey$, empcode$, dedcat$, dedmethod$,        ~
                         deduction$, dedempflag$, dedcracct$, deddbacct$,~
                         dedapplies$, amount(1), amount(2), amount(3),   ~
                         amount(4), amount(5), amount, dedamt9(2),       ~
                         dedamt9(3), dedamt9(4), dedhold$,               ~
                         sbjct(1,1), sbjct(1,2), sbj(2,1), sbj(2,2),     ~
                         sbj(3,1), sbj(3,2), sbj(4,1), sbj(4,2),         ~
                         str(dedrecord$,,80)
            return

L34150: FMT CH(15),              /* EMPLOYEE CODE + SEQUENCE #         */~
            CH(12),              /* EMPLOYEE NUMBER                    */~
            CH(6),               /* DEDUCTION CATEGORY (LOC?!)         */~
            CH(6),               /* METHOD OF DEDUCTION (DDT)          */~
            CH(12),              /* DEDUCTION DESCRIPTION              */~
            CH(1),               /* EMPLOYEE PAYS THIS? FLAG           */~
            CH(9),               /* CREDIT ACCOUNT                     */~
            CH(9),               /* DEBIT ACCOUNT                      */~
            CH(6),               /* APPLIES FIELD (123456)             */~
            PD(14,4),            /* AMOUNT 1 OR 0 IF NOT USED          */~
            PD(14,4),            /* AMOUNT 2 OR 0 IF NOT USED          */~
            PD(14,4),            /* AMOUNT 3 OR 0 IF NOT USED          */~
            PD(14,4),            /* AMOUNT 4 OR 0 IF NOT USED          */~
            PD(14,4),            /* GOAL                               */~
            PD(14,4),            /* CURRENT DEDUCTION AMOUNT           */~
            PD(14,4),            /* MTD         "       "              */~
            PD(14,4),            /* QTD         "       "              */~
            PD(14,4),            /* YTD         "       "              */~
            CH(08),              /* HOLD AMOUNT                        */~
            PD(14,4),            /* Current Units Subject To Deduction */~
            PD(14,4),            /* Current Dollars Subject To Deducti */~
            PD(14,4),            /* Month To Date Units Subject        */~
            PD(14,4),            /* Month To Date Dollars Subject      */~
            PD(14,4),            /* Quarter To Date Units Subject      */~
            PD(14,4),            /* Quarter To Date Dollars Subject    */~
            PD(14,4),            /* Year To Date Units subject         */~
            PD(14,4),            /* Year To Date Dollars Subject       */~
            CH(80)               /* filler for rest of record or inter */~

L35000: REM *************************************************************~
            *  SET PROCESSING RANGES                                    *~
            *************************************************************

            init(hex(00)) dept$(1), empcode$(1), payfreq$(1)
            init(hex(ff)) dept$(2), empcode$(2), payfreq$(2)

            call "READ100" (#1, "PRL PROCESSING RANGE", f1%(1))
                     if f1%(1) = 0 then return
                     get #1, using L35400, str(dept$(), 1),               ~
                                          str(empcode$(), 1),            ~
                                          str(payfreq$(), 1), eff$
            if eff$ = " " or eff$ = blankdate$ then eff$ = date
            if dept$(1) <> "ALL" then L35170
               init(hex(00)) dept$(1)
               init(hex(ff)) dept$(2)
               go to L35210
L35170:     if dept$(2) <> " " then L35190
               dept$(2) = dept$(1)
L35190:        dept$(1) = dept$(1) addc all(hex(ff))

L35210:     if empcode$(1) <> "ALL" then L35250
               init(hex(00)) empcode$(1)
               init(hex(ff)) empcode$(2)
               go to L35330
L35250:     if empcode$(2) <> " " then L35270
               empcode$(2) = empcode$(1)
L35270:        empcode$(1) = empcode$(1) addc all(hex(ff))

L35330:     if payfreq$(2) <> " " then L35350
               payfreq$(2) = payfreq$(1)
L35350:        payfreq$(1) = payfreq$(1) addc all(hex(ff))
               return

L35400:              FMT       XX(20), CH(8), CH(24), CH(2), CH(6)

L50000: REM *************************************************************~
            *                S A V E   R E S U L T S                    *~
            *                                                           *~
            * LOGS THE FACT THAT INVOLVED PAY FREQUENCIES NOW CAN HAVE  *~
            * CHECKS PRINTED.                                           *~
            *************************************************************

            temp$ = "0000000"
            call "READ101" (#1, "PAYROLLS_IN_PROCESS:", f1%(1))
                if f1%(1) = 0 then L50140
            get #1, using L50110, temp$
L50110:     FMT POS(21), CH(7)
            delete #1

L50140:     REM Net old with new...
            for i% = 1% to 7%
                convert i% to str(temp1$,,1), pic(#)
                if str(temp1$,,1) <= payfreq$(1) then L50200
                if str(temp1$,,1) > payfreq$(2) then L50200
                str(temp$,i%,1) = "0"
L50200:     next i%

            write #1, using L50230, "PAYROLLS_IN_PROCESS:", temp$," ", " "
L50230:     FMT CH(20), CH(7), CH(250), CH(223)
            goto L65000

        REM *************************************************************~
            *                       A B O R  T                          *~
            *                                                           *~
            * WE WILL WIND UP HERE IF A TRAPABLE ERROR IS ENCOUNTERED   *~
            * WHILE FIGURING DEDUCTIONS. THE USER IS INFORMED AS MUCH   *~
            * AS IS POSSIBLE AS TO WHERE AND WHY THE ERROR OCCURED.     *~
            *************************************************************

        deffn'99 (errormsg1$, errormsg2$, errormsg3$)

            for i% = 1 to 10 : print bell; : next i%

            accept                                                       ~
               at (01,02), "WARNING: PLEASE READ THE FOLLOWING...",      ~
               at (04,02), "DEDUCTION CALCULATION CAN'T BE COMPLETED FOR ~
        ~EMPLOYEE: ", fac(hex(84)), empcode$,                             ~
               at (09,02), fac(hex(8c)), errormsg1$,              ch(79),~
               at (10,02), fac(hex(8c)), errormsg2$,              ch(79),~
               at (11,02), fac(hex(8c)), errormsg3$,              ch(79),~
               at (16,02),"PLEASE RECTIFY THE PROBLEM, THEN RETURN AND TR~
        ~Y AGAIN.",                                                       ~
               at (24,39),                                               ~
                  "(16)ACKNOWLEDGE, PRINT SCREEN, AND EXIT",             ~
                                                                         ~
               keys(hex(10))

               call "PRNTSCRN"
               return clear
            if prldtest% < 0% then L14000
            call "SHOSTAT" ("Processing ABORTED")
            end 99%

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
