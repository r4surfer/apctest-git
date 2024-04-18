        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  M   M  TTTTT  DDDD   PPPP    OOO    SSS   TTTTT   *~
            *    J    MM MM    T    D   D  P   P  O   O  S        T     *~
            *    J    M M M    T    D   D  PPPP   O   O   SSS     T     *~
            *  J J    M   M    T    D   D  P      O   O      S    T     *~
            *   J     M   M    T    DDDD   P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JMTDPOST - This program posts material entered direct     *~
            *            to a given job.  (Inventory Material).         *~
            *            It posts to the total dollars material field on*~
            *            the JOBMASTR file and writes a detail of the   *~
            *            transaction to the JOBMTLDR file. It also      *~
            *            checks to see if the part has been committed   *~
            *            to the job. If the part has been committed,    *~
            *            the routine relieves the amount committed from *~
            *            the HNYQUAN file as well as posting amount     *~
            *            moved to the On Hand field of HNYQUAN.         *~
            *            Then it adjusts the quantity used to date field*~
            *            in the committed material detail record.       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/15/81 ! ORIGINAL                                 ! TOM *~
            * 01/29/87 ! Changes to the HNYQUAN file layout.      ! LDJ *~
            *************************************************************
        sub "JMTDPOST"(jobnr$,           /* Job  to be updated         */~
                      partnr$,           /* Part number to post        */~
                      store$,            /* Store number               */~
                      lot$,              /* Lot number                 */~
                      datemoved$,        /* Date part moved to job     */~
                      quantity,          /* Quantity moved to job      */~
                      ucost,             /* Inventory Cost per unit    */~
                      tcost,             /* Total cost of item         */~
                      #1,                /* UFB Address of JOBMASTR    */~
                      #2,                /* UFB Address of JOBMTLDR    */~
                      #3,                /* UFB Address of HNYQUAN     */~
                      #4,                /* UFB Address of HNYMASTR    */~
                      #5,                /* UFB Address of SYSFILE2    */~
                      userid$,           /* Current User ID            */~
                      returncode%)       /* Error return from subroutin*/~
                                         /* 0  = Record posted         */~
                                         /* 99 = Record *not* posted   */~

            dim                                                          ~
                datemoved$6,             /* Date moved to the job      */~
                freespace$(12)10,        /* Free space in file         */~
                jobnr$8,                 /* Job number to post         */~
                jobrecord$(10)70,        /* Job master file record     */~
                lot$16,                  /* Lot number                 */~
                partnr$25,               /* Part number                */~
                seqnr$8,                 /* Detail file sequence number*/~
                store$3,                 /* Store number               */~
                sysdate$6,               /* System clock date          */~
                userid$3,                /* User id                    */~
                workspace$(10)10         /* Get file info              */

           dim  f2%(5),                  /* File status flags          */~
                fs%(5),                  /* File status flags          */~
                f1%(5),                  /* Record status flags        */~
                rslt$(5)20               /* Error message from OPENFILE*/~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10422
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
L10422: REM *************************************************************
            returncode% = 99%            /* Set so good resets it.     */

            if f2%(1) <> 0 then end      /* No job master file         */
            if fs%(2) = 0% then                                          ~
               call "OPENCHCK" (#2, fs%(2), f2%(2), 1%, rslt$(2))

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
                   totalhnymaterial = totalhnymaterial + tcost
                   seqnr = seqnr + 1

            REM NOW REWRITE RECORD
                put str(jobrecord$(), 117, 64), using L10900,             ~
                                                totalhnymaterial,        ~
                                                str(workspace$(), 1, 48),~
                                                seqnr

                rewrite #1, using L11020, str(jobrecord$(), 1, 700)
L11020:                     FMT CH(700)
                returncode% = 0

        REM NOW GO AND UPDATE COMMITTED THROUGH HNYPOST
                call "HNYPST1" (partnr$, store$, lot$,0,0,0,quantity,0,  ~
                               #3, #4, #5, f2%(3),f2%(4), f2%(5), 0%, u3%)

            REM NOW WRITE MATERIALS POSTED DIRECTLY TO JOB DETAIL REC.
                convert seqnr to seqnr$, pic(########)

                write #2, using L15130,                                   ~
                          jobnr$, seqnr$, partnr$, store$, lot$,         ~
                          datemoved$, quantity, ucost, tcost, userid$,   ~
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

            u3% = 1%                     /* Ducking compiler returncode*/
