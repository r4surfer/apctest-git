        REM THISPROGRAMWASGENERATEDUSINGTHEGENMENUPROGRAMWHICHISAPROPRIET~
            *                                                           *~
            *  JJJJJ  BBBB    222   TTTTT  IIIII  FFFFF                 *~
            *    J    B   B  2   2    T      I    F                     *~
            *    J    BBBB      2     T      I    FFF                   *~
            *  J J    B   B   2       T      I    F                     *~
            *   JJ    BBBB   22222    T    IIIII  F                     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JB2TIF  - Writes transactions to image file corrosponding *~
            *           to formats that JBTIF handles.                  *~
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
            * 03/19/86 ! EOD INSURANCE ON WRITE STATEMENT         ! KAB *~
            * 06/10/87 ! 12 COST BUCKETS                          ! KAB *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            ARYPRODUCTOFCAELUSASSOCIATESSPOKANEWAALLRIGHTSRESERVEDGENMENU

            sub "JB2TIF"  (id$, wake%, return%, function%, priority$,    ~
                           job$, modno$, jnlid$, pstseq%, userid$, date$,~
                           part$, store$, lot$, quan, anything$, more$,  ~
                           costpassed$)

        dim                                                              ~
            anything$56,                 /* PIPOUT, TO JOB, OR ?       */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cost$96,                     /* COSTS WRITTEN              */~
            costpassed$96,               /* COSTS PASSED               */~
            date$6,                      /* Posting date               */~
            id$2,                        /* Task to toss to            */~
            jnlid$3,                     /* JOURNAL ID                 */~
            job$8,                       /* JOB NUMBER                 */~
            lot$6,                       /* INVENTORY LOT NUMBER       */~
            modno$2,                     /* MODULE ID                  */~
            more$40,                     /* Free text or ?             */~
            part$25,                     /* INVENTORY PART NUMBER      */~
            priority$1,                  /* TRANSACTIO PRIORITY        */~
            store$3,                     /* WAREHOUSE                  */~
            userid$3                     /* STAMP OG THE INFAMOUS USER */

        dim                                                              ~
            f2%(1),                      /* FILE STATUS FLAGS          */~
            rslt$(1)20,                  /* TEXT FROM FILE OPENING     */~
            axd$(1)4                     /* ALT KEY POINTER FROM OPEN'G*/

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
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1, "JBTIF",                                          ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos =    9, keylen =  18,                     ~
                        alt key  1, keypos =  1, keylen = 26

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            return% = 0
            if wake% > 1% then wake_up   /* Allows not passing all args*/
            if here% <> 0 then write_it
            mat f2% = con
            call "OPENFILE" (#1 , "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))

            REM IF JBTIF NOT OPEN, THEN CREATE IT
                if f2%(1) = 0% then L10000
                call "OPENFILE" (#1 ,"OUTPT",f2%(1 ),rslt$(1 ),axd$(1 ))
                close #1
                call "OPENFILE" (#1 ,"SHARE",f2%(1 ),rslt$(1 ),axd$(1 ))

L10000: REM *************************************************************~
            *                    M A I N   P R O G R A M                *~
            *                                                           *~
            * PROCESSING BEGINS HERE.                                   *~
            *************************************************************

            here% = 123456789%

        write_it
            if userid$ = " " then call "EXTRACT" addr ("ID", userid$)
            if date$ = " " or date$ = blankdate$ then date$ = date
            cost$ = costpassed$
            if cost$ = " " then init (hex(00)) cost$

L10110:     write #1, using L10210, job$, " ", id$, priority$, date, time,~
                      function%, userid$, modno$, jnlid$, pstseq%, date$,~
                      part$, store$, lot$, quan, anything$, more$, cost$,~
                      " ", eod goto L10110

            if wake% = 0 then L65000
            if function% = 99% then wake% = 9998%

        wake_up
            return% = wake%    /* Allows passing '999n%' to end task */
            call "TASKUP" (id$, return%)

L10210:     FMT CH(8),                   /* Job Number                 */~
                CH(1),                   /* Record Status              */~
                CH(2),                   /* Port Id  (trans type)      */~
                CH(1),                   /* Priority                   */~
                CH(6),                   /* Date                       */~
                CH(8),                   /* Time (hhmmss100th)         */~
                BI(1),                   /* Function                   */~
                CH(3),                   /* User Id                    */~
                CH(2),                   /* Module                     */~
                CH(3),                   /* Journal Id                 */~
                BI(4),                   /* Posting Sequence           */~
                CH(6),                   /* Posting Date               */~
                CH(25),                  /* Component Part             */~
                CH(3),                   /* Store                      */~
                CH(6),                   /* Lot                        */~
                PD(14,4),                /* Quantity                   */~
                CH(56),                  /* Varies                     */~
                CH(40),                  /* Varies                     */~
                CH(96),                  /* Costs                      */~
                CH(71)                   /* Error Text (20) and Filler */~

L65000: REM THISPROGRAMWASGENERATEDBYGENMENUAPROPRIETARYPRODUCTOFCCAELUS*~
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
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENMENUGENMENUG

            end /* Later on Doogan */
