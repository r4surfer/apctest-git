        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  JJJJJ  N   N  L       CCC   L       OOO    SSS   EEEEE   *~
            *    J    NN  N  L      C   C  L      O   O  S      E       *~
            *    J    N N N  L      C      L      O   O   SSS   EEEE    *~
            *  J J    N  NN  L      C   C  L      O   O      S  E       *~
            *   J     N   N  LLLLL   CCC   LLLLL   OOO    SSS   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JNLCLOSE - THIS SUBROUTINE CLOSES A GENERAL LEDGER BATCH  *~
            *            IDENTIFIED BY THE INCOMING PARAMETERS.         *~
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
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            sub "JNLCLOSE"    (module$,  /* Module ID of Batch to Close*/~
                               jnlid$,   /* Jounal ID of Batch to Close*/~
                               pstseq%,  /* Posting Sequence of Batch  */~
                               return%)  /* Return Code for Close      */

        dim jnlid$3,                     /* Journal ID of Batch        */~
            module$2,                    /* Module ID of Batch         */~
            readkey$9                    /* General Purpose Key        */~


                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            * FILES                                                     *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L02042
            cms2v$ = "R6.01.00 10/07/91 CMS General Release            "
L02042: REM *************************************************************
            select #1, "GLBATCH",                                        ~
                       varc, indexed, recsize = 150,                     ~
                       keypos = 5, keylen = 9,                           ~
                       alt key  1, keypos =    1, keylen = 13,           ~
                           key  2, keypos =   22, keylen =  8, dup       ~

            call "OPENCHCK" (#1,  0%, 0%, 0%, " ")

        REM *************************************************************~
            *             C L O S E  B A T C H                          *~
            *                                                           *~
            *Closes General Ledger Batch Requested.                     *~
            *************************************************************

            return% = 99%
            put readkey$ using L10070, module$, jnlid$, pstseq%
L10070:     FMT CH(2), CH(3), BI(4)

            call "READ101" (#1, readkey$, f1%)
                if f1% = 0% then L10170
            get #1 using L10120, status$
L10120:     FMT CH(1)
            if status$ > "1" then L10170
            put #1 using L10155, "2", date, time
            rewrite #1
L10155:     FMT POS(1), CH(1), POS(46), 2*CH(8)
            return% = 0%
L10170:     end

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
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

