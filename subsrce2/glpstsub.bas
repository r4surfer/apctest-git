        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      PPPP    SSS   TTTTT   SSS   U   U  BBBB    *~
            *  G      L      P   P  S        T    S      U   U  B   B   *~
            *  G GGG  L      PPPP    SSS     T     SSS   U   U  BBBB    *~
            *  G   G  L      P          S    T        S  U   U  B   B   *~
            *   GGG   LLLLL  P       SSS     T     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPSTSUB - WRITES A WORKFILE RECORD TO BE POSTED BY       *~
            *            GLPOST2/3.  ORIGINAL WAS COPIED FROM GLPRTSUB. *~
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
            * 08/24/90 ! ORIGINAL                                 ! RAC *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GLPSTSUB" (modno$,          /*MODULE NUMBER               */~
                        jnlid$,          /*JOURNAL ID                  */~
                        pstseq%,         /* POSTING SEQUENCE NUMBER    */~
                        userid$,         /* USER ID                    */~
                        date$,           /* POSTING DATE               */~
                        account$,        /* GL ACCOUNT                 */~
                        text$,           /* GL TEXT PASSED             */~
                        debit,           /* DEBIT AMOUNT PASSED        */~
                        credit,          /* CREDIT AMOUNT PASSED       */~
                        #1,              /* POST FILE                  */~
                        f21%,            /* POST FILE OPEN INDICATOR   */~
                        gl_post_info$()) /* G/L Export Posting Info    */~

        dim                                                              ~
            account$9,                   /* GL ACCOUNT                 */~
            date$6,                      /* POSTING DATE               */~
            gl_post_info$(2)255,         /* G/L Export Posting info    */~
            jnlid$3,                     /* JOURNAL ID                 */~
            modno$2,                     /* MODULE NUMBER              */~
            text$100,                    /* GL TEXT                    */~
            userid$3,                    /* USER ID                    */~
            seq$7                        /* UNIQUE KEY INSURANCE       */~

        dim rslt$20                      /* RETURN CODE FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************

            if fo% <> 0% then L02030
               call "OPENCHCK" (#1, fo%, f21%, 1%, rslt$)
L02030:     if fo% < 1% then end

            glamt = debit - credit
            if glamt < 0 then L02110
               debit1 = glamt
               credit1 = 0
                  goto L02140

L02110:        debit1 = 0
               credit1 = -(glamt)

L02140:     call "GETDTTM" addr(seq$)

            write #1, using L10000, modno$, jnlid$,pstseq%,userid$,seq$,  ~
                      date$, account$,text$,debit1,credit1,date, " ",    ~
                      gl_post_info$(), eod goto L02140

            end

L10000:     FMT                                                          ~
            CH(2),                       /* MODULE NUMBER              */~
            CH(3),                       /* JOURNAL ID                 */~
            BI(4),                       /* POSTING SEQUENCE NUMBER    */~
            CH(3),                       /* USER ID                    */~
            CH(7),                       /* SEQUENCE NUMBER            */~
            CH(6),                       /* GL POSTING DATE            */~
            CH(9),                       /* GL ACCOUNT NUMBER          */~
            CH(100),                     /* GL TEXT                    */~
            2*PD(14,4),                  /* DEBIT, CREDIT              */~
            CH(6),                       /* SYSTEM DATE                */~
            CH(4),                       /* filler                     */~
            2*CH(255)                    /* G/L Export Posting Info    */

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
