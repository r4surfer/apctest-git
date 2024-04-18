        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  JJJJJ  BBBB   JJJJJ  N   N  L       CCC   L       SSS    *~
            *    J    B   B    J    NN  N  L      C   C  L      S       *~
            *    J    BBBB     J    N N N  L      C      L       SSS    *~
            *  J J    B   B  J J    N  NN  L      C   C  L          S   *~
            *   J     BBBB    J     N   N  LLLLL   CCC   LLLLL   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBJNLCLS - THIS SUBROUTINE PASSES A RECORD TO THE JBPOST  *~
            *            PROGRAMS VIA THE JBTIF TO INDICATE A GENERAL   *~
            *            LEDGER BATCH TO CLOSE.                         *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07-30-90 ! ORIGINAL                                 ! RAC *~
            * 03/31/93 ! PRR 12747 Removed SHOSTAT at end.        ! JIM *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "JBJNLCLS"   (jtype$,        /* J1 or J2 transaction type  */~
                          userid$,       /* Userid who wants to close  */~
                          modno$,        /* Module for batch to close  */~
                          jnlid$,        /* Journal for batch to close */~
                          pstseq%,       /* Posting Sequence of batch  */~
                          return%)       /* Return code 0 = OK         */~
                                         /*            99 = Not OK     */~

        dim date$6,                      /* System Date                */~
            jnlid$3,                     /* Journal ID of Batch        */~
            jtype$2,                     /* J1 or J2 transaction Type  */~
            userid$3                     /* User who requested close   */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GLBATCH  ! General Ledger Batch Control File        *~
            *************************************************************

            select #1, "GLBATCH",                                        ~
                       varc, indexed, recsize = 150,                     ~
                       keypos = 5, keylen = 9,                           ~
                       alt key  1, keypos =    1, keylen = 13,           ~
                           key  2, keypos =   22, keylen =  8, dup       ~

            call "OPENCHCK" (#1,  0%, 0%, 0%, " ")

        REM *************************************************************~
            *               I N I T I A L I Z A T I O N                 *~
            *                                                           *~
            *************************************************************

            date$ = date

        REM *************************************************************~
            *           P R O C E S S  C L O S E  R E Q U E S T         *~
            *                                                           *~
            *Send request to close batch to JBTIF                       *~
            *************************************************************

            return% = 99%
            put readkey$ using L10080, modno$, jnlid$, pstseq%
L10080:     FMT CH(2), CH(3), BI(4)

            call "READ101" (#1, readkey$, f1%)
                if f1% = 0% then L65000
            get #1 using L10150, status$
L10150:     FMT CH(1)
            if status$ > "0" then L10200
            put #1 using L10150, "1"
            rewrite #1

L10200: /* Now send the Request to the JBTIF file */
            if status$ > "1" then L65000

            call "JB2TIF"               /* Writes to Transaction Image */~
                 (jtype$,               /* JBPOST1 or JBPOST2 trans    */~
                  1%,                   /* Wake up task flag 0,1,2,9999*/~
                  0%,                   /* Not used if Wake flag = 0   */~
                  11%,                  /* Transaction type            */~
                  hex(99),              /* Priority                    */~
                  " ",                  /* Not used                    */~
                  modno$,               /* G/L module to post          */~
                  jnlid$,               /* G/L journal to post         */~
                  pstseq%,              /* G/L posting sequence number */~
                  userid$,              /* Who                         */~
                  date$,                /* Date requested              */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                    0,                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ",                  /* Not used                    */~
                  " ")                  /* Not used                    */~

            return% = 0%

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
