        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   U   U  N   N   222   H   H  RRRR    SSS    *~
            *  W   W  C   C  U   U  NN  N  2  2   H   H  R   R  S       *~
            *  W W W  C      U   U  N N N    2    HHHHH  RRRR    SSS    *~
            *  WW WW  C   C  U   U  N  NN   2     H   H  R  R       S   *~
            *  W   W   CCC    UUU   N   N  22222  H   H  R   R   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCUN2HRS - Converts quantity passed in to hours based     *~
            *            on conversion factor stored in WCMASTER file.  *~
            *            also returns a literal atempting to describe   *~
            *            the (time) unit of measure the center is       *~
            *            normally expressed.                            *~
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
            * 12/18/85 ! ORIGINAL                                 ! HES *~
            * 05/23/91 ! PRR 11420 Factor = 24 if 'VEND' not found! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

            sub "WCUN2HRS" (#1,               /* WCMASTR  File UFB     */~
                            wc$,              /* Work Center           */~
                            factor,           /* Conversion Factor     */~
                            units,            /* WC in, Hours out      */~
                            unitsdescr$)      /* Description of WC     */
                                              /* Time unit             */
        dim                                                              ~
            oldwc$4,                     /* Last Wc Processed          */~
            unitsdescr$30,               /*                            */~
            descr$30,                    /*                            */~
            wc$4                         /* WORK CENTER                */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************

        REM *************************************************************~
            *       P R O C E S S I N G   H A P P E N S   H E R E       *~
            *                                                           *~
            * DOES CONVERSION...                                        *~
            *************************************************************

            descr$ = "(**Work Center NOT On File**)"
            if wc$ = hex(00000000) then L10170  /* factor is passed in */
            if wc$ = oldwc$ then L10150
            call "READ100" (#1, wc$, f1%)
            if f1% <> 0 then goto L10110
                if wc$ = "VEND" then factor = 24
                goto L65000
L10110:     get #1, using L10130, wcfactor
            oldwc$ = wc$
L10130:     FMT XX(2019), BI(2)

L10150:     REM Convert it
            factor = wcfactor
L10170:     descr$ = "(** Factor Can't Be < 1 **)"
            if factor < 1 then L65000

            REM Desribe it
            temp = 24/factor
            units = units * temp
            if temp < 1 then temp = temp * 60
            descr$ = "One Unit ="
            call "CONVERT" (temp, -0.4, str(descr$,12,11))
            if 24/factor < 1 then descr$ = descr$ & " Minutes"           ~
                             else descr$ = descr$ & " Hours"
            if str(descr$,12,2)="1" then str(descr$,pos(-descr$="s"))=" "
            call "PUTPAREN" (descr$)

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

            unitsdescr$ = descr$
            end
