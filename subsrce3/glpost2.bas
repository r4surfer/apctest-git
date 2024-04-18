        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      PPPP    OOO    SSS   TTTTT   222           *~
            *  G      L      P   P  O   O  S        T        2          *~
            *  G GGG  L      PPPP   O   O   SSS     T      2            *~
            *  G   G  L      P      O   O      S    T     2             *~
            *   GGG   LLLLL  P       OOO    SSS     T    22222          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPOST2  - Receives the parameters from the caller and    *~
            *            passes them to GLPOST3. In this way, we can    *~
            *            handle those sites that have 'local authority' *~
            *            (i.e., two sets of books) requirements.        *~
            *                                                           *~
            *            First passes original parameters to GLPOST3;   *~
            *            then looks up the local authority account in   *~
            *            GLCROSS2; then passes the local authority      *~
            *            set of parameters to GLPOST3.                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 08/20/87 ! Rewrite. Former GLPOST2 has been renamed ! JIM *~
            *          !  GLPOST3 (which see for original mod     !     *~
            *          !  block and subroutine logic.             !     *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 05/30/91 ! Added RAC's G/L Export File Logic.       ! JBK *~
            * 09/28/94 ! PRR 13294. Restamp time for fast machines! JDH *~
            * 02/28/95 ! REMed out call to TASKUP.  Had been that ! JDH *~
            *          !  way before.  No idea why it was unREMed !     *~
            *          !  since the TASKUP stuff was for custom   !     *~
            *          !  background processing and required      !     *~
            *          !  GLPOST2 to come out of the SSL.         !     *~
	    * 05/28/96 ! Changes for the year 2000                ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "GLPOST2" (saveaccount$,     /* Account to be Updated      */~
                       debit,            /* Debit Amount (0 if credit) */~
                       credit,           /* Credit Amount (0 if debit) */~
                       date$,            /* Date of Module Posting Date*/~
                       anyperiod%,       /* 0 means restricted to 3open*/~
                       modl$,            /* Source Module ID           */~
                       savetext$,        /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                       jnlid$,           /* Journal ID                 */~
                       pstseq%,          /* Posting Sequence Number    */~
                       userid$,          /* Guess                      */~
                       #1,               /* UFB Address of GLMAIN file */~
                       #2,               /* UFB Address of GLDETAIL    */~
                       #3,               /* UFB Address of SYSFILE2    */~
                       returncode%,      /* Error Code Returned        */~
                       doc$,             /* Multi-currency Document ID */~
                       postinfo$())      /* CMS general posting info   */

        dim date$6,                      /* Date of Module when posting*/~
            dual_books$1,                /* Dual books in effect?      */~
            adjacct$9,                   /* SUSPENCE ACCOUNT           */~
            doc$16,                      /* Multi Currency Document ID */~
            f1%(12),                     /* Record Status from READs   */~
            filler$2,                    /* Blank spaces for filler    */~
            loclaccount$9,               /* Local authority account #  */~
            jnlid$3,                     /* Journal ID                 */~
            postinfo$(2)255,             /* CMS general posting info   */~
            saveaccount$9,               /* Saves Passed Account Number*/~
            savetext$100,                /* Saves Passed Text          */~
            modl$2,                      /* Module ID                  */~
            userid$3                     /* User generating transaction*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #10 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #11 ! GLDETAL2 ! G. L. detail records for local authority *~
            * #12 ! GLCROSS2 ! G. L. chart-to-local-authority cross ref.*~
            * #13 ! GLEXPORT ! G. L. Export File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #10, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #11, "GLDETAL2",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            select #12, "GLCROSS2",                                      ~
                        varc,     indexed,  recsize =  27,               ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  18

            select #13, "GLEXPORT",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen = 32

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if beenherebefore% <> 0% then goto L09100
                adjacct$ = " "
                dual_books$ = "N"                    /* Default to 'no' */
                export$ = "N"                        /* Default to 'no' */
                post$ = "Y"                          /* Default to 'yes'*/
                call "READ100" (#03, "SWITCHS.GL", f1%(3))
                     if f1%(3) = 0% then goto L10000
                get #03 using L09095, dual_books$, export$, backgrnd$, post$
L09095:              FMT POS(21), 2*CH(1), CH(8), XX(5), CH(1)
L09100:         if dual_books$ <> "Y" then goto L09500
                if beenherebefore% <> 0% then goto L09210
                     call "OPENCHCK" (#10, 0%, 0%, 100%, " ")
                     call "OPENCHCK" (#11, 0%, 0%, 100%, " ")
                     call "OPENCHCK" (#12, 0%, 0%, 100%, " ")

                     call "READ100" (#03, "FISCAL DATES", f1%(3))
                          if f1%(3) = 0% then L09500
                     get #03 using L09190, adjacct$
L09190:              FMT POS(417), CH(9)

L09210:              call "READ100" (#12, adjacct$, f1%(12))
                     if f1%(12) = 0% then dual_book_err% = -999%

L09500:         if export$ <> "Y" then L10000
                if beenherebefore% <> 0% then goto L10000
                     call "OPENCHCK" (#13, 0%, 0%,1000%, " ")

L10000: REM *************************************************************~
            *       Pass original parameters- GLMAIN & GLDETAIL         *~
            *************************************************************
            suspence_taken% = 0%
            if post$ = "N" then L12000 /* if 'Y' or 'D' then post CMS   */

            if dual_book_err% <> 0% then returncode% = dual_book_err%

            call "GLPOST3" (saveaccount$,/* Account to be Updated      */~
                            debit,       /* Debit Amount (0 if credit) */~
                            credit,      /* Credit Amount (0 if debit) */~
                            date$,       /* Date of Module Posting Date*/~
                            anyperiod%,  /* 0 means restricted to 3open*/~
                            modl$,       /* Source Module ID           */~
                            savetext$,   /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                            jnlid$,      /* Journal ID                 */~
                            pstseq%,     /* Posting Sequence Number    */~
                            userid$,     /* Guess                      */~
                            #1,          /* UFB Address of GLMAIN file */~
                            #2,          /* UFB Address of GLDETAIL    */~
                            #3,          /* UFB Address of SYSFILE2    */~
                            returncode%) /* Error Code Returned        */

                if dual_book_err% <> 0% then returncode% = dual_book_err%

                if returncode% <> 1000% then L11000
                     returncode% = 0%
                     suspence_taken% = 1%

L11000: REM *************************************************************~
            *  Look up & pass the local authority account variables,    *~
            *  i.e., change to files GLMAIN2 & GLDETAL2.                *~
            *************************************************************

            if dual_books$ <> "Y" then L12000 /*No Local Authority books*/
                loclaccount$ = saveaccount$
L11043:         if suspence_taken% = 1% then loclaccount$ = adjacct$
            call "READ100" (#12, loclaccount$, f1%(12))
            if f1%(12) = 1% then L11070
                 if loclaccount$ <> adjacct$ then L11064
                     loclaccount$ = "?Loc Auth"
                     goto L11090
L11064:          suspence_taken% = 1%
                 goto L11043
L11070:     get #12 using L11080, loclaccount$
L11080:         FMT POS(10), CH(9)
L11090:     call "GLPOST3" (loclaccount$,/* Account to be Updated      */~
                            debit,       /* Debit Amount (0 if credit) */~
                            credit,      /* Credit Amount (0 if debit) */~
                            date$,       /* Date of Module Posting Date*/~
                            anyperiod%,  /* 0 means restricted to 3open*/~
                            modl$,       /* Source Module ID           */~
                            savetext$,   /* Reference text (100 Chars) */~
                                         /*   1st 30 chars = Reference1*/~
                                         /*   2nd 34 chars = Reference2*/~
                                         /*   3rd  4 chars = Adjust Flg*/~
                                         /*   Lst 32 chars = Descriptn */~
                            jnlid$,      /* Journal ID                 */~
                            pstseq%,     /* Posting Sequence Number    */~
                            userid$,     /* Guess                      */~
                            #10,         /* UFB Address of GLMAIN2 file*/~
                            #11,         /* UFB Address of GLDETAIL2 fl*/~
                            #3,          /* UFB Address of SYSFILE2    */~
                            returncode%) /* Error Code Returned        */

                if returncode% = 1000% then returncode% = 0%

L12000: REM POST EXPORT FILE
            if export$ <> "Y" then L65000

            filler$ = "  "

L12080:     call "GETDTTM" addr(datetime$)
            put #13 using L12170, modl$, jnlid$, pstseq%, doc$, datetime$,~
                                 postinfo$(), filler$, date$, debit,     ~
                                 credit, saveaccount$, savetext$,        ~
                                 anyperiod%, userid$, " ", " ", " ", " ",~
                                 " ", " "

            write #13, eod goto L12080

L12170: FMT CH(2), CH(3), BI(4), CH(16), CH(7), 2*CH(255), CH(2), CH(6), ~
            2*PD(15,4), CH(9), CH(100), BI(4), CH(3), 5*CH(256), CH(62)

            if beenherebefore% <> 0% then L65000
               if backgrnd$ = " " then L65000
*             CALL "TASKUP" ("ZZ",  0%, BACKGRND$, " ", " ", "Y", "Y",  ~
*                            "Y", "N")

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            beenherebefore% = 1%
            end
