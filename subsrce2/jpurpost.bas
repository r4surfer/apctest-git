        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  PPPP   U   U  RRRR   PPPP    OOO    SSS   TTTTT   *~
            *    J    P   P  U   U  R   R  P   P  O   O  S        T     *~
            *    J    PPPP   U   U  RRRR   PPPP   O   O   SSS     T     *~
            *  J J    P      U   U  R   R  P      O   O      S    T     *~
            *   J     P       UUU   R   R  P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JPURPOST - This subroutine posts the purchases made       *~
            *            against a job.                                 *~
            *            It posts the total purchases to the jobmastr   *~
            *            file and writes the detail of the purchase to  *~
            *            the JOBPURCH detail file. Does not allow the   *~
            *            total dollar amount of purchases on the job    *~
            *            master file to go below zero (in case one      *~
            *            starts backing out vendor invoices)            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/15/81 ! ORIGINAL                                 ! TOM *~
            * 10/02/86 ! Changed OPENFILE to OPENCHCK             ! MJB *~
            *************************************************************

        sub "JPURPOST"(jobnr$,           /* Job  to be updated         */~
                      vencode$,          /* Vendor code                */~
                      veninvoice$,       /* Vendor invoice number      */~
                      invdate$,          /* Vendor invoice date        */~
                      ponumber$,         /* Our purchase order number  */~
                      item$,             /* Item ordered               */~
                      qty,               /* Quantity of line item      */~
                      tcost,             /* Total cost of item         */~
                      glacct$,           /* General ledger account     */~
                      #1,                /* UFB address of JOBMASTR    */~
                      #2,                /* UFB address of JOBPURCH    */~
                      f21%,              /* Status flag for JOBMASTR   */~
                      f22%,              /* Status flag for JOBPURCH   */~
                      returncode%)       /* Error return from subroutin*/~
                                         /* 0  = Record posted         */~
                                         /* 99 = Record *NOT* Posted   */

            dim                                                          ~
                freespace$(9)10,         /* Free Space in   JOBPURCH   */~
                glacct$9,                /* General ledger account     */~
                invdate$6,               /* Date of vendor invoice     */~
                item$25,                 /* Item to be posted          */~
                jobnr$8,                 /* Job number to post         */~
                jobrecord$(10)70,        /* Job master file record     */~
                ponumber$16,             /* Our purchase order number  */~
                seqnr$8,                 /* Detail file sequence number*/~
                sysdate$6,               /* System clock date          */~
                vencode$9,               /* Vendor code                */~
                veninvoice$16,           /* Vendor invoice number      */~
                userid$3,                /* User ID                    */~
                workspace$(10)10         /* File information skipped   */

           dim  f2%(3),                  /* File status flags          */~
                f1%(3),                  /* Record status for inventory*/~
                rslt$(3)20               /* Error message from OPENCHCK*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10412
            cms2v$ = "04.16.05 10/27/86 MPS & Miscellaneous            "
L10412: REM *************************************************************
            returncode% = 99             /* Set so good resets it. */
            f2%(1) = f21%
            f2%(2) = f22%

            call "OPENCHCK" (#1, 0%, f2%(1),   0%, rslt$(1))
            call "OPENCHCK" (#2, 0%, f2%(2), 100%, rslt$(2))

            if f2%(1) <> 0 then end      /* No job master file     */

            f21% = f2%(1)      /* Reset flag in calling routine        */
            f22% = f2%(2)

            call "READ101" (#1, jobnr$, f1%(1))
              if f1%(1) = 0 then end               /* Not posted       */

            REM Now retrieve this record
                get #1, using L10750, str(jobrecord$(), 1, 700)
L10750:                 FMT CH(700)

            REM Extract pertinent information
               get str(jobrecord$(), 109, 80), using L10870,              ~
                                               purchases,                ~
                                               str(workspace$(), 1, 64), ~
                                               seqnr
L10870:               FMT PD(14,4), CH(64), PD(14,4)

            REM Update purchases total and sequence number
                   purchases = max(0, purchases + tcost)
                   seqnr = seqnr + 1

            REM Now rewrite record
                put str(jobrecord$(), 109, 80), using L10870,             ~
                                                purchases,               ~
                                                str(workspace$(), 1, 64),~
                                                seqnr

                rewrite #1, using L11100, str(jobrecord$(), 1, 700)
L11100:                     FMT CH(700)
                returncode% = 0

            REM Now write purchases detail record
                call "EXTRACT" addr("ID", userid$) /* Get user code    */
                sysdate$ = date
                convert seqnr to seqnr$, pic(########)

                write #2, using L11260,                                   ~
                          jobnr$, seqnr$, vencode$, veninvoice$,         ~
                          invdate$, ponumber$, item$, qty, tcost,        ~
                          glacct$, userid$, sysdate$,                    ~
                          str(freespace$(), 1, 78)
                returncode% = 0
                end

L11260:         FMT CH(8),               /* Job number                 */~
                    CH(8),               /* Sequence number            */~
                    CH(9),               /* Vendor code                */~
                    CH(16),              /* Vendor invoice number      */~
                    CH(6),               /* Vendor invoice date        */~
                    CH(16),              /* Our purchase order number  */~
                    CH(25),              /* Item ordered               */~
                    PD(14,4),            /* Quantity of the line item  */~
                    PD(14,4),            /* Item extended total cost   */~
                    CH(9),               /* General ledger account     */~
                    CH(3),               /* User ID                    */~
                    CH(6),               /* System date record saved   */~
                    CH(78)               /* Free Space                 */~

