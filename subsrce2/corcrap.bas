        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR    CCC   RRRR    AAA   PPPP           *~
            *  C   C  O   O  R   R  C   C  R   R  A   A  P   P          *~
            *  C      O   O  RRRR   C      RRRR   AAAAA  PPPP           *~
            *  C   C  O   O  R   R  C   C  R   R  A   A  P              *~
            *   CCC    OOO   R   R   CCC   R   R  A   A  P              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORCRAP  - Find appropriate records in core credit file   *~
            *            for application of open debits                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/17/92 ! Original                                 ! KAB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "CORCRAP" (#1,               /* CORCRMAS                   */~
                       cuscode$,         /* Customer Code              */~
                       pcuscode$,        /* Parent Customer Code       */~
                       cpart$,           /* Core Part Code             */~
                       openqty,          /* Open Quantity              */~
                       apkey$(),         /* Keys to records for applic.*/~
                       aplines%,         /* Number of entries in above */~
                       rslvqty)          /* Quantity Resolved          */

        dim                                                              ~
            apkey$(100)40,               /* Keys to records for applic.*/~
            cuscode$9,                   /* Customer Code              */~
            cpart$25,                    /* Core Part Code             */~
            hold$1,                      /* Hold Flag                  */~
            pcuscode$9,                  /* Parent Customer Code       */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            userid$3                     /* Current User Id            */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CORCRMAS ! Core Credit Master File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)

            apmax% = dim(apkey$(),1)

            qtyopen = openqty
            rslvqty = 0

            aplines% = 0%
            init (" ") apkey$()

        REM *************************************************************~
            * PROCESS SECTION                                           *~
            *************************************************************

            plowkey$ = str(cuscode$,,9) & str(cpart$,,25)

L10060:     init (hex(00)) str(plowkey$,35)

            call "PLOWALTS" (#1, plowkey$, 5%, 34%, f1%)
               if f1% = 0% then try_parent
L10100:     get #1, using L10110, readkey$, qty1, qty2, hold$, drop%
L10110:         FMT POS(10), CH(26), POS(176), 2*PD(14,4),               ~
                    POS(304), CH(1), BI(2)
            if hold$ <> " " then L10260

        /* Honor to the Nth degree, drop off date */
            goto L10230   /* Not Yet */
            td1$ = str(readkey$,21,6) : td2$ = date
            call "DATE" addr("G-", td1$, td2$, d1%, d2%)
               if d2% <> 0% then L10230
                  if d1% > drop% then L10260
        /* Honor to the Nth degree, drop off date */

L10230:     qty = max(0, qty1 - qty2)
            qty = min(qty, qtyopen)
            if qty > 0 then gosub load_array
L10260:     call "READNEXT" (#1, f1%)
               if f1% = 0% then try_parent
               if str(key(#1,5%),,34%) <> str(plowkey$,,34%)             ~
                  then try_parent
               goto L10100

        try_parent
            if str(plowkey$,,9) = pcuscode$ then gosub exit_application
            plowkey$ = str(pcuscode$,,9) & str(cpart$,,25)
            goto L10060


        load_array
            aplines% = aplines% + 1%
            put apkey$(aplines%) using L10410, readkey$, qty
L10410:         FMT CH(26), PD(14,4)
            qtyopen = qtyopen - qty
            if qtyopen = 0 then exit_application
            if aplines% >= apmax% then exit_application
            return

        exit_application
            return clear all
            rslvqty = openqty - qtyopen

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

            end
