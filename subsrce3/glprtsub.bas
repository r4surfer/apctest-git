        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      PPPP   RRRR   TTTTT   SSS   U   U  BBBB    *~
            *  G      L      P   P  R   R    T    S      U   U  B   B   *~
            *  G GGG  L      PPPP   RRRR     T     SSS   U   U  BBBB    *~
            *  G   G  L      P      R   R    T        S  U   U  B   B   *~
            *   GGG   LLLLL  P      R   R    T     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPRTSUB - WRITES GLDETAIL RECORD INFORMATION TO FILE #   *~
            *            PASSED FROM CALLING PROGRAM. A JOURNAL CAN BE  *~
            *            GENERATED FROM THESE RECORDS BY RUNNING        *~
            *            GLPRTJNL.                                      *~
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
            * 03/25/85 ! ORIGINAL                                 ! RAC *~
            * 08/16/85 ! Fixed Format Statement of Journal Print  ! LDJ *~
            *          !   file to add up to 160 bytes.           !     *~
            * 01/30/86 ! CLEAN UP & SPEED UP THIS THING           ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GLPRTSUB" (modno$,          /*MODULE NUMBER               */~
                        jnlid$,          /*JOURNAL ID                  */~
                        pstseq%,         /* POSTING SEQUENCE NUMBER    */~
                        userid$,         /* GLKEY ALT #1 PASSED        */~
                        date$,           /* POSTING DATE               */~
                        account$,        /* GL ACCOUNT                 */~
                        text$,           /* GL TEXT PASSED             */~
                        debit,           /* DEBIT AMOUNT PASSED        */~
                        credit,          /* CREDIT AMOUNT PASSED       */~
                        #1,              /* PRINT FILE                 */~
                        f21%)            /* PRINT FILE OPEN INDICATOR  */~

        dim                                                              ~
            account$9,                   /* GL ACCOUNT                 */~
            date$6,                      /* POSTING DATE               */~
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
            cms2v$ = "04.16.01 04/04/86 Physical inventory and miscel   "
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
                      eod goto L02140

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
            CH(4)                        /* filler                     */

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
