        REM THISPROGRAMWASGENERATEDUSINGTHEGENMENUPROGRAMWHICHISAPROPRIET~
            *                                                           *~
            *  JJJJJ  BBBB   IIIII  N   N  U   U   SSS   EEEEE          *~
            *    J    B   B    I    NN  N  U   U  S      E              *~
            *    J    BBBB     I    N N N  U   U   SSS   EEE            *~
            *  J J    B   B    I    N  NN  U   U      S  E              *~
            *   JJ    BBBB   IIIII  N   N   UUU    SSS   EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBINUSE - Tells you wether there are any unprocessed      *~
            *           Transactions in the TIF for this job.           *~
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
            * 09/27/85 ! ORIGINAL                                 ! HES *~
            * 06/10/87 ! Standard Costing Changes                 ! ERN *~
            * 03/01/88 ! Added Call To MANUAL, More Detailed Msg  ! HES *~
            * 04/14/92 ! PRR 12108. Added options to Clear In-use ! JDH *~
            *          !   and Write JBTIF In-use flag.           !     *~
            * 10/01/92 ! In Use Clearing can now be job specific. ! JDH *~
            * 05/25/93 ! Warn or Block if Bad (or even slightly   ! KAB *~
            *          !    Naughty) transactions on file.        !     *~
            * 06/11/93 ! Format Numeric Zones of JBTIF so         ! KAB *~
            *          !   STCIMPFL doesn't get convert errors    !     *~
            * 07/25/97 ! Changes for Year 2000                    ! LDJ *~
            ARYPRODUCTOFCAELUSASSOCIATESSPOKANEWAALLRIGHTSRESERVEDGENMENU

            sub "JBINUSE" (job$,                                         ~
                           return%) /* IN 1% = Clear User In-use Flag. */
                                    /*    2% = Check In-use & Write    */
                                    /*         In-use Flag if free.    */
                                    /*    3% = Only Check In-use       */
                                   /* OUT 0% = Not In-use.             */
                                   /*     1% = In-use.                 */

        dim                                                              ~
            adm1$1, adm2$1, adm3$1,      /* DB Admin Check             */~
            blankdate$8,                 /* Blank Date value           */~
            job$8,                       /* JOB NUMBER                 */~
            readkey$20,                  /* G/P READKEY$               */~
            oldjob$8,                    /* CONTROL VARIABLE           */~
            plowkey$32,                  /* Standard plow variable     */~
            showerr$1,                   /* WARN IF BAD TRANS          */~
            tjob$8,                      /* JOB NUMBER Temporary       */~
            tuser$3,                     /* UserID     Temporary       */~
            userid$3,                    /* UserID(Used by or for Flag)*/~
            zero$100                     /* Hex (00)'s                 */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *   1 ! JBTIF    ! Shop Floor Transaction Image File        *~
            *   2 ! SYSFILE2 ! Temporarily                              *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1, "JBTIF",                                          ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos =    9, keylen =  18,                     ~
                        alt key  1, keypos =  1, keylen =  26

            select #2, "SYSFILE2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos =    1, keylen =  20

            do_what% = return%   /* Save incoming behavior option */

            return% = 0%
            if do_what% = 1% then L03060
                if job$ = " " then Exit_Routine

L03060:     call "OPENCHCK" (#1, fo%, fs%, 0%, " ")
            if fs% <> 0% then end        /* No JBTIF file */

            if showerr$ <> " " then L03210
            call "OPENCHCK" (#2, fo%, fs%, 0%, " ")
            if fs% <> 0% then L03210
            readkey$ = "SWITCHS.SFC"
            call "READ100" (#2, readkey$, fo%)
               if fo% = 0% then L03210
            get #2 using L03170, showerr$
L03170:         FMT POS(71), CH(1)
            close #2

L03210:     if pos("BWN" = showerr$) = 0% then showerr$  = "N"

            if showerr$ <> "B" then L09000   /* Might Care, but not much */
            if adm3$    <> " " then L09000   /* Why do twice             */

            adm1$, adm2$ = " " : mod_admin% = 0% /* Should be, anyway  */
            call "CMSMACHK" ("SFC", adm1$, adm2$)
            if adm1$ = "Y" or adm2$ = "Y" then mod_admin% = 1%
            adm3$ = "X"

L09000: REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *************************************************************

            if userid$ = " " then call "EXTRACT" addr ("ID", userid$)
            init (hex(00)) zero$

            if blankdate$ = " " then call "DATUFMTC" (blankdate$)

        REM *************************************************************~
            *                    M A I N   P R O G R A M                *~
            * --------------------------------------------------------- *~
            * PROCESSING BEGINS (and ends) HERE.                        *~
            *************************************************************

            if do_what% = 1% then clear_in_use

            oldjob$ = job$  /* Guarantees DIM of 8 */

            if showerr$ = "N" then L11000
            plowkey$ = str(oldjob$) & "X" & hex(000000)
            call "PLOWALTS" (#1, plowkey$, 1%, 9%, return%)
            if return% = 0% then L11000

*        Tell user whats going on...
L10170:    keyhit% = 2%
           if showerr$ = "W" then L10200
           if mod_admin% <> 0% then L10200

                call "ASKUSER" (keyhit%, "Bad Transaction Error",        ~
         hex(8c) & "Job currently has rejected Transactions on file." &  ~
         hex(84),                                                        ~
         hex(8c) & "Press (RETURN) to bypass job until the problem is res~
        ~olved." & hex(84),                                               ~
         hex(8c) & "Press PF(13) For Additional Information." & hex(84))

            goto L10320

L10200:         call "ASKUSER" (keyhit%, "Bad Transaction Warning",      ~
         hex(8c) & "Job currently has rejected Transactions on file." &  ~
         hex(84),                                                        ~
         hex(8c) & "Press PF16 to disregard and check for availability." ~
         & hex(84),                                                      ~
         hex(8c) & "Press (RETURN) to bypass job until the problem is res~
        ~olved." & hex(84))

                if keyhit%  = 16% then L11000
L10320:         if keyhit%  =  0% then Exit_Routine
                if keyhit% <> 13% then L10170
                   call "MANUAL" ("JBINUSE")
                   goto L10170

L11000:     plowkey$ = str(oldjob$) & hex(20000000)
            call "PLOWALTS" (#1, plowkey$, 1%, 9%, return%)
            if return% = 0% then write_tif
            get #1, using L11040, tuser$
L11040:     FMT POS(28), CH(3)

*        Tell user whats going on...
L11070:    keyhit% = 2%
           if str(key(#1,0),,3) =" J0" then L11190 /* Someone using Job */

                call "ASKUSER" (keyhit%, "Job In Use",                   ~
         hex(8c) & "Job currently has a transaction pending in the backgr~
        ~ound posting queue." & hex(84),                                  ~
         hex(8c) & "Try Job again immediately, or wait a while, depending~
        ~ on your system load." & hex(84),                                ~
         hex(8c) & "Press (RETURN) to Continue, or PF(13) For Additional ~
        ~Information." & hex(84))
            goto L11270

L11190:         call "ASKUSER" (keyhit%, "Job In Use",                   ~
         hex(8c) & "Job is currently being managed by user " & tuser$  & ~
        "." & hex(84),                                                   ~
         hex(8c) & "You will need to wait until those changes are complet~
        ~ed." & hex(84),                                                  ~
         hex(8c) & "Press (RETURN) to Continue, or PF(13) For Additional ~
        ~Information." & hex(84))

L11270:         if keyhit% <> 13% then Exit_Routine
                   call "MANUAL" ("JBINUSE")
                   goto L11070


        write_tif
            if do_what% <> 2% then Exit_Routine
            write #1 using L12030, job$," ", "J0 ", date, time, " ", userid$," ", 0%,   ~
                                  blankdate$," ", 0, " ", zero$, " "
L12030:         FMT CH(8), CH(1), CH(3), CH(6), CH(8),CH(1), CH(3),CH(5), BI(4), CH(6),CH(34),    ~
                    PD(14,4), CH(96), CH(96), CH(71)

            goto Exit_Routine

        clear_in_use
            REM Clear In-use markers...
            readkey$ = " J0" & all(hex(00))
L13030:     call "PLOWNXT1" (#1, readkey$, 3%, f1%)
                if f1% = 0% then Exit_Routine
            get #1 using L13060, tjob$, tuser$
L13060:         FMT CH(8), POS(28), CH(3)
            if job$ = " " then L13110
                if job$ <> tjob$ or tuser$ <> userid$ then L13030
                     delete #1
                     goto L13030
L13110:     if tuser$ = userid$ then delete #1
            goto L13030

Exit_Routine:
        REM THISPROGRAMWASGENERATEDBYGENMENUAPROPRIETARYPRODUCTOFCCAELUS*~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENMENUGENMENUG

            end
