        REM *************************************************************~
            *                                                           *~
            *  JJJJJ   SSS    AAA   L      PPPP    OOO    SSS   TTTTT   *~
            *    J    S      A   A  L      P   P  O   O  S        T     *~
            *    J     SSS   AAAAA  L      PPPP   O   O   SSS     T     *~
            *  J J        S  A   A  L      P      O   O      S    T     *~
            *   J      SSS   A   A  LLLLL  P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JSALPOST - THIS SUBROUTINE POSTS THE MOVEMENT FROM THE    *~
            *            INVOICING MODULE TO JOBCOST. IT UPDATES THE    *~
            *            AMOUNT OF DOLLARS TRANSFERRED TO SALES IN THE  *~
            *            JOB MASTER RECORD AND THEN CREATES A DETAIL    *~
            *            RECORD IN JOBSALES FILE. THE SEQUENCE NUMBER   *~
            *            USED TO WRITE TO THE JOBSALES DETAIL FILE IS   *~
            *            THE SAME NUMBER USED TO POST TO THE MATERIAL   *~
            *            COMMITTMENTS FILE (THAT SHOULD NOT MATTER) AS  *~
            *            FAR AS ANY WRITE ERRORS. ALSO YOU'LL NOTICE    *~
            *            THAT I DID NOT PUT ALL DIMS IN ALPHA ORDER...  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/01/81 ! ORIGINAL                                 ! TOM *~
            *************************************************************
        sub "JSALPOST"(jobnr$,           /* JOB  TO BE UPDATED         */~
                      cuscode$,          /* CUSTOMER CODE              */~
                      cusinvoice$,       /* CUSTOMER INVOICE NUMBER    */~
                      invdate$,          /* CUSTOMER INVOICE DATE      */~
                      ponumber$,         /* CUS PURCHASE ORDER NUMBER  */~
                      item$,             /* ITEM SOLD                  */~
                      qty,               /* QUANTITY OF LINE ITEM      */~
                      tcost,             /* TOTAL COST OF ITEM         */~
                      glacct$,           /* GENERAL LEDGER ACCOUNT     */~
                      #1,                /* UFB ADDRESS OF JOBMASTR    */~
                      #2,                /* UFB ADDRESS OF JOBSALES    */~
                      f21%,              /* STATUS FLAG FOR JOBMASTR   */~
                      f22%,              /* STATUS FLAG FOR JOBSALES   */~
                      returncode%)       /* ERROR RETURN FROM SUBROUTIN*/~
                                         /* 0  = RECORD POSTED         */~
                                         /* 99 = RECORD *NOT* POSTED   */~

            dim                                                          ~
                freespace$(9)10,         /* SPARE CHANGE IN JOBSALES   */~
                glacct$9,                /* GENERAL LEDGER ACCOUNT     */~
                invdate$6,               /* DATE OF VENDOR INVOICE     */~
                item$25,                 /* ITEM TO BE POSTED          */~
                jobnr$8,                 /* JOB NUMBER TO POST         */~
                jobrecord$(10)70,        /* JOB MASTER FILE RECORD     */~
                ponumber$16,             /* CUS PURCHASE ORDER NUMBER  */~
                seqnr$8,                 /* DETAIL FILE SEQUENCE NUMBER*/~
                sysdate$6,               /* SYSTEM CLOCK DATE          */~
                cuscode$9,               /* CUSTOMER CODE              */~
                cusinvoice$8,            /* CUS.   INVOICE NUMBER      */~
                userid$3,                /* USER ID                    */~
                workspace$(10)10         /* FILE INFORMATION SKIPPED   */

           dim  f2%(3),                  /* FILE STATUS FLAGS          */~
                f1%(3),                  /* RECORD STATUS FOR INVENTORY*/~
                rslt$(3)20,              /* ERROR MESSAGE FROM FILEOPEN*/~
                axd$(3)4                 /* ALTERNATE KEY POINTERS     */

            returncode% = 99             /* SET SO GOOD RESETS IT.     */
            f2%(1) = f21%                /* KLUGE FOR COMPILER CAN'T   */
            f2%(2) = f22%                /* HANDLE ARRAYS IN PROLG.    */

            if f2%(1) <> 0 then end      /* NO JOB MASTER FILE         */
            if f2%(2) = 0 then L10560

               call "FILEOPEN" (#2,"SHARE",f2%(2),rslt$(2),axd$(2))
               REM NOW SEE IF DETAIL FILE EXISTS.  IF NOT, CREATE IT.
                   if f2%(2) = 0 then L10560        /* IF YES, OUT.     */
                      call "FILEOPEN"(#2,"OUTPT",f2%(2),rslt$(2),axd$(2))
                      close #2
                      call "FILEOPEN"(#2,"SHARE",f2%(2),rslt$(2),axd$(2))

L10560:     f21% = f2%(1)      /* RESET FLAG IN CALLING ROUTINE        */
            f22% = f2%(2)

            call "READ101" (#1, jobnr$, f1%(1))
              if f1%(1) = 0 then end               /* NOT POSTED       */

            REM NOW RETRIEVE THIS RECORD
                get #1, using L10750, str(jobrecord$(), 1, 700)
L10750:                 FMT CH(700)

            REM EXTRACT PERTINENT INFORMATION
               get str(jobrecord$(), 141, 80), using L10870,              ~
                                               trantosls,                ~
                                               str(workspace$(), 1, 24), ~
                                               seqnr
L10870:               FMT PD(14,4), CH(24), PD(14,4)

            REM UPDATE TRANSFERRAL TO SALES TOTAL AND SEQUENCE NUMBER
                   trantosls = max(0, trantosls + tcost)
                   seqnr = seqnr + 1

            REM NOW REWRITE RECORD
                put str(jobrecord$(), 141, 80), using L10870,             ~
                                                trantosls,               ~
                                                str(workspace$(), 1, 24),~
                                                seqnr

                rewrite #1, using L11100, str(jobrecord$(), 1, 700)
L11100:                     FMT CH(700)
                returncode% = 0

            REM NOW WRITE TRANSFERRAL TO SALES DETAIL RECORD
                call "EXTRACT" addr("ID", userid$) /* GET USER CODE    */
                sysdate$ = date
                convert seqnr to seqnr$, pic(########)

                write #2, using L11260,                                   ~
                          jobnr$, seqnr$, cuscode$, cusinvoice$,         ~
                          invdate$, ponumber$, item$, qty, tcost,        ~
                          glacct$, userid$, sysdate$,                    ~
                          str(freespace$(), 1, 86)
                returncode% = 0
                end

L11260:         FMT CH(8),               /* JOB NUMBER                 */~
                    CH(8),               /* SEQUENCE NUMBER            */~
                    CH(9),               /* CUSTOMER CODE              */~
                    CH(08),              /* CUSTOMER INVOICE NUMBER    */~
                    CH(6),               /* CUSTOMER INVOICE DATE      */~
                    CH(16),              /* CUS PURCHASE ORDER NUMBER  */~
                    CH(25),              /* ITEM SOLD TO CUSTOMER      */~
                    PD(14,4),            /* QUANTITY OF THE LINE ITEM  */~
                    PD(14,4),            /* ITEM EXTENDED TOTAL COST   */~
                    CH(9),               /* GENERAL LEDGER ACCOUNT     */~
                    CH(3),               /* USER ID                    */~
                    CH(6),               /* SYSTEM DATE RECORD SAVED   */~
                    CH(86)               /* SPARE CHANGE               */~

