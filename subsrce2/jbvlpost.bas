        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   V   V  L      PPPP    OOO    SSS   TTTTT   *~
            *    J    B   B  V   V  L      P   P  O   O  S        T     *~
            *    J    BBBB   V   V  L      PPPP   O   O   SSS     T     *~
            *  J J    B   B   V V   L      P      O   O      S    T     *~
            *   J     BBBB     V    LLLLL  P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBVLPOST - POSTS TO JOB VALUE RECORD AND TO JOB MASTER REC*~
            *            ORD FOR VERSION TWO. TYPE CODE FLAGS WHAT WILL *~
            *            BE OUTPUT AND HOW THAT OUTPUT WILL AFFECT JOB  *~
            *            MASTER.                                        *~
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
            * 03/22/83 ! ORIGINAL                                 ! KEN *~
            * 11/01/85 ! ADDED USERID TO CALL, REMOVED F2%s       ! HES *~
            * 06/09/87 ! 12 COST BUCKETS, NEW FORMAT              ! KAB *~
            * 05/15/93 ! Core/Closing Adj Ledger Posting          ! KAB *~
            * 03/09/95 ! PRR 13268 - Write to JBVALUE2 now uses   ! RJH *~
            *          !  Postdate & Sysdate.                     !     *~
            * 07/18/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        REM *************************************************************~
            * THIS ROUTINE WRITES A RECORD OF VALUE ADDED TO A JOB. IT   ~
            * WILL RECIEVE EITHER + OR - AMOUNTS AFFECTING THE CORRESPOND~
            * ING ACCUMULATORS IN THE JOB MASTER RECORD.  THE THREE TYPES~
            * CAN BE DISTINGUISHED BY TRANSACTION TYPE.                  ~
            *************************************************************

        sub "JBVLPOST"(#1,               /* UFB ADDRESS OF JBMASTR2    */~
                       #2,               /* UFB ADDRESS OF JBVALUE2    */~
                       #4,               /* UFB ADDRESS OF JBMASTRC    */~
                       returncode%,      /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */~
                                         /* 99 = RECORD *NOT* POSTED   */~
                                         /* 98 = JOB CLOSED            */~
                       jobnr$,           /* JOB  TO BE UPDATED         */~
                       type%,            /* TYPE OF INPUT              */~
                                         /* 1=EMPLOYEE LABOR           */~
                                         /* 2=WORK CENTER COSTS        */~
                                         /* 3=DIRECT COSTS             */~
                       workdate$,        /* DATE WORK WAS PERFORMED    */~
                                         /* OR DIRECT TRANSACTION DATE */~
                       postdate$,        /* POSTING DATE OF USER       */~
                       userid$,          /* WHO                        */~
                       posttext$,        /* POSTING TEXT               */~
                       cost(),           /* COSTS PASSED IN            */~
        /* The following arguments are optional (unused) for type 3.   */~
                       unitrate,         /* UNIT RATE  ($/UNIT)        */~
                       units,            /* AMOUNT OF UNITS            */~
                       workstat$,        /* WORKSTATION                */~
                       activity$,        /* ACTIVITY                   */~
        /* The following arguments are optional (unused) for type 2.   */~
                       empcode$,         /* EMPLOYEE CODE              */~
                       earnstype$,       /* EARNINGS TYPE              */~
                       lclass$,          /* LABOR CLASS                */~
                       unitdescr$)       /* UNIT DESCRIPTION           */~


           dim  alpha$42,                /* ALPHA FIELDS               */~
                activity$4,              /* ACTIVITY                   */~
                blankdate$8,             /* Blank Date for Comparison. */~
                cost(12), cost$96,       /* COSTS                      */~
                earnstype$12,            /* EARNINGS TYPE              */~
                empcode$12,              /* EMPLOYEE CODE              */~
                jobnr$8,                 /* JOB NUMBER                 */~
                jbclosedate$6,           /* JOB CLOSE DATE             */~
                jobcost(12), jobcost$96, /* JOB COSTS                  */~
                lclass$4,                /* LABOR CLASS                */~
                numeric$32,              /* NUMERIC FIELDS             */~
                postdate$6,              /* POSTING DATE               */~
                posttext$40,             /* POSTING TEXT               */~
                systime$8,               /* DATE/TIME STAMP            */~
                total(1),                /* TOTAL RECEIVER             */~
                type$1,                  /* TRANSACTION TYPE           */~
                unitdescr$6,             /* EARNINGS UNIT DESCRIPTION  */~
                user$3,                  /* USER ID                    */~
                userid$3,                /* USER ID                    */~
                work(1,12),              /* WORK ARRAY                 */~
                workdate$6,              /* DATE WORK PERFORMED        */~
                workstat$4,              /* WORKSTATION                */~
                zeros$(3)200             /* HEX (00) STRING            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            init (hex(00)) zeros$()

            returncode% = 99%            /* SET SO GOOD RESETS IT.     */

            if type% < 1% or type% > 9% then end     /* CANT HANDLE IT */
            if open1% = 0 then call "OPENCHCK" (#1, open1%, 0%, 0%, " ")
                if open1% < 1 then end   /* NO JBMASTR2 FILE           */

            user$ = userid$
            if user$ = " " then call "EXTRACT" addr("ID", user$)

            call "READ101" (#1, jobnr$, f1%)
              if f1% = 0 then end        /* NO JBMASTR2 RECORD         */

            returncode% = 98%    /* JB EXISTS BUT MAY BE CLOSED       */
            get #1 using L10150, jbclosedate$
L10150:         FMT POS(153), CH(6)
            if jbclosedate$ <> " " and jbclosedate$ <> blankdate$ ~
                  then end     /* JOB CLOSED!!!!!     */

            call "PACKZERO" (cost(), cost$)
            mat work = con:mat total = work * cost

            on type% goto L10300, L10300, L10500, L10700,                    ~
                          L11000, L11000, L11000, L11000, L11000
               end

L10300: REM -- UPDATE JOB FOR TYPE 1 AND 2 (LABOR AND WORK CENTER) --

            get #1 using L10330, jobtotal, jobcost()
L10330:         FMT POS(232), PD(14,4), POS(336), 12*PD(14,4)

            jobtotal = round(jobtotal + total(1), 4)
            mat jobcost = jobcost + cost
            call "PACKZERO" (jobcost(), jobcost$)

            put #1 using L10430, jobtotal, jobcost$
L10430:         FMT POS(232), PD(14,4), POS(336), CH(96)
            goto rewrite_jbmastr

L10500: REM       -- UPDATE JOB FOR TYPE 3 (DIRECT MISC. COSTS) --

            get #1 using L10530, jobtotal, jobcost()
L10530:         FMT POS(232), PD(14,4), POS(432), 12*PD(14,4)

            jobtotal = round(jobtotal + total(1), 4)
            mat jobcost = jobcost + cost
            call "PACKZERO" (jobcost(), jobcost$)

            put #1 using L10630, jobtotal, jobcost$
L10630:         FMT POS(232), PD(14,4), POS(432), CH(96)

            goto rewrite_jbmastr

L10700: REM       -- UPDATE JOB FOR TYPE 4 (CLOSING ADJUSTMENTS) --

            get #1 using L10730, jobtotal, jobcost()
L10730:         FMT POS(1147), PD(14,4), POS(1155), 12*PD(14,4)

            jobtotal = round(jobtotal + total(1), 4)
            mat jobcost = jobcost + cost
            call "PACKZERO" (jobcost(), jobcost$)

            put #1 using L10830, jobtotal, jobcost$
L10830:         FMT POS(1147), PD(14,4), POS(1155), CH(96)

        rewrite_jbmastr
            rewrite #1
            goto L13000

L11000
*       *** CORE SHADOW FILE UPDATE SECTION - 5,6,7  ****

            if fs4% <> 0% then L11040
               call "OPENCHCK" (#4, fs4%, f1%, 100%, " ")
L11040:     call "READ101" (#4, jobnr$, f1%)
               if f1% <> 0% then L11080
                  put #4 using L11070, jobnr$, str(zeros$())
L11070:               FMT CH(8), CH(592)
L11080:     on (type% - 4%) goto L11100, L11200, L11300, L11400,             ~
                                 upd_jbmastrc
               end
L11100
*        TYPE 5, CORE SHADOW VALUE
            get #4 using L11120, jobtotal, jobcost()
L11120:         FMT POS(9), PD(14,4), POS(113), 12*PD(14,4)
            jobtotal = round(jobtotal + total(1), 4)
            mat jobcost = jobcost + cost
            call "PACKZERO" (jobcost(), jobcost$)
            put #4 using L11170, jobtotal, jobcost$
L11170:         FMT POS(9), PD(14,4), POS(113), CH(96)
            goto upd_jbmastrc

L11200
*        TYPE 6, CORE CREDITS
            get #4 using L11220, jobtotal, jobcost()
L11220:         FMT POS(209), PD(14,4), POS(217), 12*PD(14,4)
            jobtotal = round(jobtotal + total(1), 4)
            mat jobcost = jobcost + cost
            call "PACKZERO" (jobcost(), jobcost$)
            put #4 using L11270, jobtotal, jobcost$
L11270:         FMT POS(209), PD(14,4), POS(217), CH(96)
            goto upd_jbmastrc

L11300
*        TYPE 7, CORE CLOSING ADJUSTMENTS
            get #4 using L11320, jobtotal, jobcost()
L11320:         FMT POS(313), PD(14,4), POS(321), 12*PD(14,4)
            jobtotal = round(jobtotal + total(1), 4)
            mat jobcost = jobcost + cost
            call "PACKZERO" (jobcost(), jobcost$)
            put #4 using L11370, jobtotal, jobcost$
L11370:         FMT POS(313), PD(14,4), POS(321), CH(96)
            goto upd_jbmastrc

L11400
*        TYPE 8, EFFECT ON CFG
            get #4 using L11410, jobtotal, jobcost()
L11410:         FMT POS(417), PD(14,4), POS(425), 12*PD(14,4)
            jobtotal = round(jobtotal + total(1), 4)
            mat jobcost = jobcost + cost
            call "PACKZERO" (jobcost(), jobcost$)
            put #4 using L11435, jobtotal, jobcost$
L11435:         FMT POS(417), PD(14,4), POS(425), CH(96)
            goto upd_jbmastrc

        upd_jbmastrc /* Type 9, Memo only, but create JBMASTRC record */
            if f1% = 0% then write #4 else rewrite #4
            goto L13000

L13000: REM NOW MESS A ROUND WITH THE DETAIL RECORD

            if open2% = 0 then call "OPENCHCK" (#2, open2%, 0%, 100%," ")
               if open2% < 0% then end          /* NOT EXPECTED        */

            init (" ") alpha$ : init (hex(00)) numeric$

            on type% goto L13100, L13200, L13300, L13400,                    ~
                          L13500, L13600, L13700, L13800, L13850

L13100: REM --- LABOR DETAIL ---

            type$ = "L"
            put alpha$ using L13150, workstat$, empcode$, earnstype$,     ~
                                    lclass$, unitdescr$, activity$
L13150:         FMT CH(4), CH(12), CH(12), CH(4), CH(6), CH(4)
            goto L13260

L13200: REM --- WORK CENTER DETAIL ---

            type$ = "W"
            put alpha$ using L13240, workstat$, " ", activity$
L13240:         FMT CH(4), CH(34), CH(4)

L13260:     put str(numeric$,,16) using L13270, unitrate, units
L13270:         FMT 2*PD(14,4)
            goto write_jbvalue2

L13300: REM --- DIRECT MISC. DETAIL ---

            type$ = "M"
            goto write_jbvalue2

L13400: REM --- CLOSING ADJUSTMENT DETAIL ---
            type$ = "C"
            goto write_jbvalue2

L13500: REM --- CORE DEBIT (SHADOW) DETAIL ---
            type$ = "X"
            goto write_jbvalue2

L13600: REM --- CORE CREDIT DETAIL ---
            type$ = "Y"
            goto write_jbvalue2

L13700: REM --- CORE CLOSING DETAIL ---
            type$ = "Z"
            goto write_jbvalue2

L13800: REM --- CORE EFFECT ON CFG  ---
            type$ = "F"
            goto write_jbvalue2

L13850: REM --- CORE MEMOES         ---
            type$ = "G"
            goto write_jbvalue2

        REM --- WRITE THE RECORD ---

        write_jbvalue2
            str(systime$,,1) = "C"

L14050:     call "GETDTTM" addr(str(systime$,2,7))

            write #2 using L15000, jobnr$, type$, postdate$, systime$,    ~
                                  date     , user$, total(1), cost$,     ~
                                  posttext$, alpha$, numeric$, " ",      ~
                                  eod goto L14050

            returncode% = 0%             /* A.O.K. FINE                */
            end

L15000:    FMT       CH(8),    /* JOB NUMBER                           */~
                     CH(1),    /* TRANSACTION TYPE - L, W, M           */~
                     CH(6),    /* WORK DATE                            */~
                     CH(8),    /* 'C' & SYSTEM DATE, TIME              */~
                     CH(6),    /* GL POST DATE                         */~
                     CH(3),    /* USER ID                              */~
                     PD(14,4), /* TRANSACTION TOTAL                    */~
                     CH(96),   /* TRANSACTION COSTS                    */~
                     CH(40),   /* POSTING TEXT                         */~
                     CH(42),   /* ALPHA BLOCK                          */~
                     CH(32),   /* NUMERIC BLOCK                        */~
                     CH(50)    /* FILLER                               */~

           end
