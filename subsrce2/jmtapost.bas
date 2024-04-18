        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  M   M  TTTTT   AAA   PPPP    OOO    SSS   TTTTT   *~
            *    J    MM MM    T    A   A  P   P  O   O  S        T     *~
            *    J    M M M    T    AAAAA  PPPP   O   O   SSS     T     *~
            *  J J    M   M    T    A   A  P      O   O      S    T     *~
            *   J     M   M    T    A   A  P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JMTAPOST - THIS ROUTINE MAKES ADJUSTMENTS WHEN A PART     *~
            *            IS TAKEN OUT OF A JOB A PUT BACK INTO THE      *~
            *            INVENTORY DURING HNYADDNS. IT DECREASES THE $  *~
            *            MATERIAL IN JOBMASTR                           *~
            *                  AND CREATES A DETAIL IN JOBMTLDR TO BOOT.*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/20/81 ! ORIGINAL                                 ! TOM *~
            *************************************************************
        sub "JMTAPOST"(jobnr$,           /* JOB  TO BE UPDATED         */~
                      partnr$,           /* PART NUMBER TO POST        */~
                      store$,            /* STORE NUMBER               */~
                      lot$,              /* LOT NUMBER                 */~
                      datemoved$,        /* DATE PART MOVED TO JOB     */~
                      quantity,          /* QUANTITY MOVED TO JOB      */~
                      ucost,             /* UNIT COST OF ITEM          */~
                      tcost,             /* TOTAL COST OF ITEM         */~
                      #1,                /* UFB ADDRESS OF JOBMASTR    */~
                      #2,                /* UFB ADDRESS OF JOBMTLDR    */~
                      f21%,              /* STATUS FLAG FOR JOBMASTR   */~
                      f22%,              /* STATUS FLAG FOR JOBMTLDR   */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */~
                                         /* 99 = RECORD *NOT* POSTED   */~

            dim                                                          ~
                datemoved$6,             /* DATE MOVED TO THE JOB      */~
                freespace$(12)10,        /* FREE SPACE IN FILE         */~
                jobnr$8,                 /* JOB NUMBER TO POST         */~
                jobrecord$(10)70,        /* JOB MASTER FILE RECORD     */~
                lot$6,                   /* LOT NUMBER                 */~
                partnr$25,               /* PART NUMNBER               */~
                seqnr$8,                 /* DETAIL FILE SEQUENCE NUMBER*/~
                store$3,                 /* STORE NUMBER               */~
                sysdate$6,               /* SYSTEM CLOCK DATE          */~
                userid$3,                /* USER ID                    */~
                workspace$(10)10         /* GET FILE INFO              */

           dim  f2%(2),                  /* FILE STATUS FLAGS          */~
                f1%(2),                  /* RECORD STATUS FOR INVENTORY*/~
                rslt$(2)20,              /* ERROR MESSAGE FROM FILEOPEN*/~
                axd$(2)4                 /* ALTERNATE KEY POINTERS     */

            returncode% = 99             /* SET SO GOOD RESETS IT.     */
            f2%(1) = f21%                /* KLUGE FOR COMPILER CAN'T   */
            f2%(2) = f22%                /* HANDLE ARRAYS IN PROLG.    */

            if f2%(1) <> 0 then end      /* NO JOB MASTER FILE         */
            if f2%(2) = 0 then L10690

               call "FILEOPEN" (#2,"SHARE",f2%(2),rslt$(2),axd$(2))
               REM NOW SEE IF HNYDIRECT TO JOB DETAIL FILE EXISTS
                   if f2%(2) = 0 then L10690        /* IF YES, OUT.     */
                      call "FILEOPEN"(#2,"OUTPT",f2%(2),rslt$(2),axd$(2))
                      close #2
                      call "FILEOPEN"(#2,"SHARE",f2%(2),rslt$(2),axd$(2))

L10690:     f21% = f2%(1)      /* RESET FLAG IN CALLING ROUTINE        */
            f22% = f2%(2)

                call "EXTRACT" addr("ID", userid$) /* GET USER CODE    */
                sysdate$ = date

            call "READ101" (#1, jobnr$, f1%(1))
              if f1%(1) = 0 then end               /* NOT POSTED       */

            REM NOW RETRIEVE THIS RECORD
                get #1, using L10840, str(jobrecord$(), 1, 700)
L10840:                 FMT CH(700)

            REM EXTRACT PERTINENT INFORMATION
               get str(jobrecord$(), 117, 64)  using L10900,              ~
                                               totalhnymaterial,         ~
                                               str(workspace$(), 1, 48), ~
                                               seqnr
L10900:             FMT PD(14,4), CH(48), PD(14,4)

            REM UPDATE TOTAL MATERIAL DOLLARS AND SEQUENCE NUMBER
               totalhnymaterial =  totalhnymaterial - tcost
               seqnr = seqnr + 1

            REM NOW REWRITE RECORD
                put str(jobrecord$(), 117, 64), using L10900,             ~
                                                totalhnymaterial,        ~
                                                str(workspace$(), 1, 48),~
                                                seqnr

                rewrite #1, using L11020, str(jobrecord$(), 1, 700)
L11020:                     FMT CH(700)
                returncode% = 0

            REM NOW WRITE MATERIALS WITHDRAWN FROM THE JOB DETAIL REC.
                convert seqnr to seqnr$, pic(########)

                write #2, using L15130,                                   ~
                          jobnr$, seqnr$, partnr$, store$, lot$,         ~
                          datemoved$, -quantity, ucost, tcost, userid$,  ~
                          sysdate$,                                      ~
                          str(freespace$(), 1, 111)
                returncode% = 0
                end

L15130:         FMT CH(8),               /* JOB NUMBER                 */~
                    CH(8),               /* SEQUENCE NUMBER            */~
                    CH(25),              /* PART NUMBER                */~
                    CH(3),               /* STORE NUMBER               */~
                    CH(6),               /* LOT NUMBER                 */~
                    CH(6),               /* DATE MOVED TO JOB          */~
                    PD(14,4),            /* QUANTITY MOVED TO JOB      */~
                    PD(14,4),            /* UNIT COST OF ITEM          */~
                    PD(14,4),            /* ITEM EXTENDED TOTAL COST   */~
                    CH(3),               /* USER ID                    */~
                    CH(6),               /* SYSTEM DATE RECORD SAVED   */~
                    CH(111)              /* SPARE CHANGE               */~

