        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  L      BBBB   RRRR   PPPP    OOO    SSS   TTTTT   *~
            *    J    L      B   B  R   R  P   P  O   O  S        T     *~
            *    J    L      BBBB   RRRR   PPPP   O   O   SSS     T     *~
            *  J J    L      B   B  R   R  P      O   O      S    T     *~
            *   J     LLLLL  BBBB   R   R  P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JLBRPOST - THIS SUBROUTINE POSTS THE DIRECT LABOR TO A    *~
            *            GIVEN JOB. IT POSTS THE TOTAL DOLLARS OF DIRECT*~
            *            LABOR TO THE JOBMASTR FILE AND THEN WRITES A   *~
            *            DETAIL OF THE TRANSACTION TO THE JOBLABOR FILE *~
            *            THE REASON WHY WORKSPACE$ IS USED HERE AND IN  *~
            *            OTHER SUBROUTINES ISSSS IF YOU USE AN XX(##)   *~
            *            SKIP COMMAND, THE VS TRASHES WHERE YOU SKIPPED *~
            *            SO USE A VARIABLE WHEN SKIPPING WITH REWRITE   *~
            *            THIS ALSO UPDATES THE TOTAL $ OVERHEAD FIELD   *~
            *            IN THE JOBMASTR RECORD.                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/15/81 ! ORIGINAL                                 ! TOM *~
            * 10/25/81 ! UPDATE OVERTIME SLOTS AS WELL            ! TEM *~
            *************************************************************

        sub "JLBRPOST"(jobnr$,           /* JOB  TO BE UPDATED         */~
                      empcode$,          /* EMPLOYEE CODE              */~
                      workdate$,         /* DATE WORK WAS PERFORMED    */~
                      workstat$,         /* WORKSTATION                */~
                      earnstype$,        /* EARNINGS TYPE              */~
                      unitdescr$,        /* UNIT DESCRIPTION           */~
                      unitrate,          /* UNIT RATE  ($/UNIT)        */~
                      units,             /* AMOUNT OF UNITS            */~
                      tcost,             /* TOTAL LABOR COST POSTED    */~
                      ohcost,            /* TOTAL OVERHEAD COST POSTED */~
                      #1,                /* UFB ADDRESS OF JOBMASTR    */~
                      #2,                /* UFB ADDRESS OF JOBLABOR    */~
                      f21%,              /* STATUS FLAG FOR JOBMASTR   */~
                      f22%,              /* STATUS FLAG FOR JOBLABOR   */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */~
                                         /* 99 = RECORD *NOT* POSTED   */~

            dim                                                          ~
                earnstype$12,            /* EARNINS TYPE               */~
                empcode$12,              /* EMPLOYEEE CODE             */~
                freespace$(11)10,        /* SPARE CHANGE IN JOBPURCH   */~
                jobnr$8,                 /* JOB NUMBER TO POST         */~
                jobrecord$(10)70,        /* JOB MASTER FILE RECORD     */~
                seqnr$8,                 /* DETAIL FILE SEQUENCE NUMBER*/~
                sysdate$6,               /* SYSTEM CLOCK DATE          */~
                unitdescr$6,             /* UNIT DESCRIPTION           */~
                userid$3,                /* USER ID                    */~
                workdate$6,              /* DATE WORK PERFORMED        */~
                workstat$4               /* WORK STATION CODE          */~

           dim  f2%(3),                  /* FILE STATUS FLAGS          */~
                f1%(3),                  /* RECORD STATUS FOR INVENTORY*/~
                rslt$(3)20,              /* ERROR MESSAGE FROM FILEOPEN*/~
                axd$(3)4                 /* ALTERNATE KEY POINTERS     */

            returncode% = 99             /* SET SO GOOD RESETS IT.     */
            f2%(1) = f21%                /* KLUGE FOR COMPILER CAN'T   */
            f2%(2) = f22%                /* HANDLE ARRAYS IN PROLG.    */

            if f2%(1) <> 0 then end      /* NO JOB MASTER FILE         */
            if f2%(2) = 0 then L10510

               call "FILEOPEN" (#2,"SHARE",f2%(2),rslt$(2),axd$(2))
               REM NOW SEE IF DETAIL FILE EXISTS.  IF NOT, CREATE IT.
                   if f2%(2) = 0 then L10510        /* IF YES, OUT.     */
                      call "FILEOPEN"(#2,"OUTPT",f2%(2),rslt$(2),axd$(2))
                      close #2
                      call "FILEOPEN"(#2,"SHARE",f2%(2),rslt$(2),axd$(2))

L10510:     f21% = f2%(1)      /* RESET FLAG IN CALLING ROUTINE        */
            f22% = f2%(2)

            call "READ101" (#1, jobnr$, f1%(1))
              if f1%(1) = 0 then end               /* NOT POSTED       */

            REM INVESTIGATE OVERTIME
                overhours, overdolls, t1, t2 = 0
                if earnstype$ = "PREMIUM" then L10620
                if earnstype$ = "DOUBLE"  then L10620
                if earnstype$ = "OVERTIME" then L10620
                t1 = units
                t2 = tcost
                go to L10630
L10620:         overhours = units
                overdolls = tcost

L10630:     REM NOW RETRIEVE THIS RECORD
                get #1, using L10650, str(jobrecord$(), 1, 700)
L10650:                 FMT CH(700)

            REM EXTRACT PERTINENT INFORMATION
               get str(jobrecord$(), 101),     using L10720,              ~
                                               directlabor,              ~
                                               seqnr, hours,             ~
                                               ovhours, ovdolls
L10720:               FMT PD(14,4), XX(56), PD(14,4), XX(60), 3*PD(14,4)

            REM UPDATE TOTAL DIRECT LABOR AND SEQUENCE NUMBER
                   directlabor = max(0, directlabor + t2)
                   seqnr = seqnr + 1
                   hours = max(0, hours + t1)
                   ovhours = max(0, ovhours + overhours)
                   ovdolls = max(0, ovdolls + overdolls)

            REM NOW REWRITE RECORD
                put str(jobrecord$(), 101, 8), using L10830, directlabor
L10830:              FMT PD(14,4)
                put str(jobrecord$(), 165, 8), using L10830, seqnr
                put str(jobrecord$(), 233, 24), using L10870, hours,      ~
                                         ovhours, ovdolls
L10870:              FMT 3*PD(14,4)

            REM NOW GET THE TOTAL OVERHEAD FROM JOBMASTR AND UPDATE
                get str(jobrecord$(), 189, 8), using L10920,              ~
                                               totaloverhead
L10920:                    FMT PD(14,4)
                totaloverhead = max(0, totaloverhead + ohcost)

                put str(jobrecord$(), 189, 8), using L10920,              ~
                                               totaloverhead

                rewrite #1, using L10990, str(jobrecord$(), 1, 700)
L10990:                     FMT CH(700)
                returncode% = 0

            REM NOW WRITE PURCHASES DETAIL RECORD
                call "EXTRACT" addr("ID", userid$) /* GET USER CODE    */
                sysdate$ = date
                convert seqnr to seqnr$, pic(########)

                init(hex(00)) freespace$()
                write #2, using L11150,                                   ~
                          jobnr$, seqnr$, empcode$, workdate$,           ~
                          workstat$, earnstype$, unitdescr$, unitrate,   ~
                          t1,    t2,    ohcost, userid$, sysdate$,       ~
                          overhours, overdolls,                          ~
                          str(freespace$(), 1)
                returncode% = 0
                end

L11150:         FMT CH(8),               /* JOB NUMBER                 */~
                    CH(8),               /* SEQUENCE NUMBER            */~
                    CH(12),              /* EMPLOYEE CODE              */~
                    CH(6),               /* DATE WORK WAS PERFORMED    */~
                    CH(4),               /* WORKSTATION CODE           */~
                    CH(12),              /* EARNINGS TYPE              */~
                    CH(6),               /* UNIT DESCRIPTION           */~
                    PD(14,4),            /* UNIT RATE  ($/UNIT)        */~
                    PD(14,4),            /* AMOUNT OF UNITS            */~
                    PD(14,4),            /* TOTAL DIRECT LABOR COSTS   */~
                    PD(14,4),            /* TOTAL OVERHEAD COSTS       */~
                    CH(3),               /* USERID                     */~
                    CH(6),               /* SYSTEM CLOCK DATE CREATED  */~
                    2*PD(14,4),          /* OVERTIME HOURS & DOLLARS   */~
                    CH(87)               /* SPARE CHANGE               */
