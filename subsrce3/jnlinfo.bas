        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  N   N  L      IIIII  N   N  FFFFF   OOO           *~
            *    J    NN  N  L        I    NN  N  F      O   O          *~
            *    J    N N N  L        I    N N N  FFFF   O   O          *~
            *  J J    N  NN  L        I    N  NN  F      O   O          *~
            *   J     N   N  LLLLL  IIIII  N   N  F       OOO           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JNLINFO  - GETS JOURNAL PSTSEQ AND TITLE                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/07/85 ! ORIGINAL                                 ! RAC *~
            * 07/14/85 ! HIGHLY MODIFIED                          ! KAB *~
            * 07/15/87 ! Changed OPENFILE to OPENCHCK.            ! ERN *~
            * 01/31/91 ! Changed ACCEPT to GETPARM.               ! KAB *~
            * 05/14/91 ! Merged and Modified RAC code for GL      ! JBK *~
            *          !   Batch Control                          !     *~
            * 06/28/91 ! Semi-smart century rather than Rich's 19.! JDH *~
            * 04/02/92 ! PRR 12346.  Chg'd DIM of READKEY$ from 9 ! MLJ *~
            *          !  to 99 to ensure PLOWALTS of #2 gets the !     *~
            *          !  lowest rec first.                       !     *~
            * 10/26/92 ! PRR 12652 - Now get Journal Title and    ! MLJ *~
            *          !  Summary Option on restart.              !     *~
	    * 05/29/96 ! Changes for the year 2000                ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "JNLINFO" (module$,      /* MODULE ID                  */~
                           jnlid$,       /* JOURNAL ID                 */~
                           pstseq%,      /* POSTING SEQUENCE NUMBER    */~
                           summary$,     /* SUMMARY POSTING (Y OR N)   */~
                           jtitle$,      /* TITLE OF JOURNAL           */~
                           postdate$,    /* Posting Date of Batch      */~
                           #1,           /* UFB OF SYSFILE2            */~
                           f21%,         /* STATUS FLAG FOR SYSFILE2   */~
                           returncode%)  /* INPUT - 2% INFO ONLY       */~
                                         /* INPUT - 1% INFO, HARD ERROR*/~
                                         /* INPUT - ?% INCR, HARD ERROR*/~
                                         /* OUTPUT- 0% ALL O.K.        */~
                                         /* OUTPUT- <> 0% ERROR        */~

        dim ~
            err$(2)79,                   /* ERRMSG                     */~
	    filler$2,                    /* Filler                     */~
            jnlid$3,                     /* JOURNAL ID                 */~
            module$2,                    /* MODULE ID PASSED           */~
            moduleno$20,                 /* MODULE ID                  */~
            postdate$8,                  /* POSTING DATE               */~
            pstseq$8,                    /* Posting Sequence Number    */~
            readkey$99,                  /* General Purpose Key        */~
            summary$1,                   /* SUMMARY (Y OR N)           */~
            userid$3,                    /* User ID                    */~
            jtitle$30                    /* TITLE OF JOURNAL           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            * FILES                                                     *~
            *************************************************************

            select #2, "GLBATCH",                                        ~
                       varc, indexed, recsize = 150,                     ~
                       keypos =    5, keylen =    9,                     ~
                       alt key  1, keypos =    1, keylen = 13,           ~
                           key  2, keypos =   22, keylen =  8, dup       ~

                       call "OPENCHCK" (#2,  0%, 0%, 100%, " ")

            infonly% = returncode%
            returncode% = 1%
            pstseq% = 0% : restart% = -1%
            init (" ") jtitle$, summary$

            call "EXTRACT" addr("ID", userid$)

            if infonly% <> 0% then L02140
            readkey$ = "0" & str(userid$) & str(module$,1%,2%) &         ~
                           str(jnlid$,1%,3%) & hex(00000000)
            call "PLOWALTS" (#2, readkey$, 1%, 9%, f1%)
                if f1% = 0% then L02140
                get #2 using L02080, restart%
L02080:         FMT XX(9), BI(4)
                infonly% = 1%

L02140:     call "OPENCHCK" (#1, fs%, f21%, 0%, " ")
            fs% = f21%

L11000:     moduleno$ = "MODULENO:" & str(module$, 1, 2) & str(jnlid$,1,3)

            call "READ101" (#1, moduleno$, f1%)
                if f1% = 0 and infonly% = 2% then end
                if f1% <> 0 then L11030
                        gosub'99(1%)
                        goto L11000

L11030:     get #1 using L11040, jtitle$, pstseq%, summary$
L11040:         FMT XX(20), CH(30), BI(4), CH(1)
            if restart% > 0% then pstseq% = restart%
            if infonly% <> 0% then L11230
               newpost% = pstseq% + 1%
                 if newpost% > 99999999% then newpost% = 1%
               put #1, using L11100, newpost%
L11100:            FMT POS(51), BI(4)
               rewrite #1

            put readkey$ using L11155, module$, jnlid$, pstseq%
L11155:     FMT CH(2), CH(3), BI(4)
L11160:     call "READ100" (#2, readkey$, f1%)
                if f1% = 1% then L11250  /* Batch already exists must */
                                        /* be restart                */
L11175:     write #2 using L11185, "0", userid$, module$, jnlid$, pstseq%,~
                       filler$,  postdate$, " ", date, time, " "
L11185:     FMT CH(1), CH(3), CH(2), CH(3), BI(4), ch(2), CH(6), CH(8), ~
                CH(8), CH(8), CH(105)

L11230:     returncode% = 0
            call "ALLFREE"
            end
L11250:     get #2 using L11260, status$
L11260:     FMT CH(1)
            if status$ = "0" then L11230
            gosub'99(2%)
            goto L11160

        FMT                              /* SYSFILE2                   */~
            CH(9),                       /* "MODULENO:"                */~
            CH(2),                       /* MODULE$                    */~
            CH(3),                       /* JNLID$                     */~
            CH(6),                       /* BLANK FILLER               */~
            CH(30),                      /* JOURNAL (MODULE) TITLE     */~
            BI(4),                       /* NEXT SEQUENCE NO.          */~
            CH(1)                        /* SUMMARY OPTION             */~

        deffn'99(err%)

            call "ALLFREE"

*       ** PRINT AT(1,1), BELL
            if err% = 2% then L40056

            err$(1) ="Either Module:" & str(module$,1,2) & " or Journal:"~
                    & str(jnlid$,,3) & " Does Not Exist."
            err$(2) ="This Condition Must Be Rectified Via GLJNLINP."
            goto L40066

L40056:     convert pstseq% to pstseq$, pic(00000000)
            err$(1) ="Batch for Module:" & str(module$,1,2) & " Journal "~
              & str(jnlid$,,3) & " Sequence " & str(pstseq$,,8) &        ~
              " Not Open"
            err$(2) ="This Condition Must Be Rectified Via GLBATMGT."
L40066:
        /*
           ACCEPT                                                        ~
              AT (01,02), "WARNING: PLEASE READ THE FOLLOWING",          ~
              AT (04,02), "G/L POSTING FOR THIS PROCEDURE IS NOT POSSIBLE~
         .  YOU SHOULD EITHER PRINT THIS SCREEN",                        ~
              AT (05,02), "OR WRITE DOWN THE MODULE AND JOURNAL ID.  YOU ~
        MUST THEN DO THE FOLLOWING BEFORE",                              ~
              AT (06,02), "CONTINUING:",                                 ~
              AT (08,05), FAC(HEX(8C)), ERR$(1),                         ~
              AT (09,05), FAC(HEX(8C)), ERR$(2),                         ~
              AT (11,02), "FAILURE TO PERFORM THE ABOVE BY CANCELLING OUT~
         OF THIS PROCEDURE WILL RESULT",                                 ~
              AT (12,02), "IN THE LOSS OF AUTOMATED POSTING.",           ~
              AT (24,02),                                                ~
                 "(15)PRINT SCREEN",                                     ~
              AT (24,50),                                                ~
                 "(16)RETRY AFTER CORRECTION",                           ~
              KEYS(HEX(0F10)),                                           ~
              KEY (KEYHIT%)

              IF KEYHIT% <> 15 THEN 40350
               CALL "PRNTSCRN"
               GOTO 40120

              IF KEYHIT% <> 16 THEN 40120
              RETURN
        */
            call "GETPARM" addr("I ", "A",                               ~
                           "JNLINFO ", " ", "0001", "JNLINF", 6%,        ~
            "G/L Posting for this Procedure is not Possible:   ",    50%,~
            "                                                  ",    50%,~
            err$(1)                                             ,    50%,~
            "                                                  ",    50%,~
            err$(2)                                             ,    50%,~
            "(Cancellation will result in loss of Posting.)    ",    50%)

           return
