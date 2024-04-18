        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC    AAA    CCC   TTTTT   SSS   U   U  BBBB    *~
            *  W   W  C   C  A   A  C   C    T    S      U   U  B   B   *~
            *  W   W  C      AAAAA  C        T     SSS   U   U  BBBB    *~
            *  W W W  C   C  A   A  C   C    T        S  U   U  B   B   *~
            *   W W    CCC   A   A   CCC     T     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCACTSUB - SUBROUTINE TO RETURN BOM/RTE ACTIVITY          *~
            *            ASSOCIATED WITH A WCOUT RECORD                 *~
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
            * 05/09/85 ! ORIGINAL                                 ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "WCACTSUB" (wctag$   ,       /* WCOUT TAG                  */~
                        bom$     ,       /* BOM FROM JBCROSS2          */~
                        rte$     ,       /* RTE FROM JBCROSS2          */~
                        step$    ,       /* DERVIVED FROM TAG          */~
                        actcode$ ,       /* ACTIVITY CODE              */~
                        actdescr$,       /* ACTIVITY DESCRIPTION       */~
                        #1       ,       /* WCOUT                      */~
                        #2       ,       /* JBCROSS2                   */~
                        #3       )       /* RTEMASTR                   */~

        dim                                                              ~
            wctag$35,                    /* WCOUT TAG                  */~
            bom$3,                       /* BOM FROM JBCROSS2          */~
            rte$3,                       /* RTE FROM JBCROSS2          */~
            step$4,                      /* DERVIVED FROM TAG          */~
            actcode$4,                   /* ACTIVITY CODE              */~
            actdescr$30,                 /* ACTIVITY DESCRIPTION       */~
            readkey$100                  /* GENERAL PURPOSE READ KEY   */~

        REM **************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
        REM *************************************************************
        init (" ")                                                       ~
            bom$,                        /* BOM FROM JBCROSS2          */~
            rte$,                        /* RTE FROM JBCROSS2          */~
            step$,                       /* DERVIVED FROM TAG          */~
            actcode$,                    /* ACTIVITY CODE              */~
            actdescr$                    /* ACTIVITY DESCRIPTION       */~

            call "READ100" (#1, wctag$, f1%)
                if f1% = 0 then L65000

            init (hex(00)) readkey$
            str(readkey$,1,19)=str(wctag$,1,19)

            call "READ100" (#2, readkey$, f1%)
                if f1% = 0 then L65000

            get #2, using L10220, rte$, bom$
L10220:         FMT XX(25), CH(3), XX(19), XX(25), CH(3), XX(19)

            init (hex(00)) readkey$
            get #2, using L10260, str(readkey$,1,28)
L10260:         FMT CH(28)
            get str(wctag$,22,2) using L10280, s%
L10280:         FMT BI(2)
            s% = s%/100%
            convert s% to str(readkey$,29,3), pic(###)

            call "READ100" (#3, readkey$, f1%)
                if f1% = 0 then L65000

            get #3, using L10350, step$, actcode$, actdescr$
L10350:         FMT POS(88), CH(4), POS(93), CH(4), CH(30)

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end

