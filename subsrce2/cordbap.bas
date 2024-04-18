        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR   DDDD   BBBB    AAA   PPPP           *~
            *  C   C  O   O  R   R  D   D  B   B  A   A  P   P          *~
            *  C      O   O  RRRR   D   D  BBBB   AAAAA  PPPP           *~
            *  C   C  O   O  R   R  D   D  B   B  A   A  P              *~
            *   CCC    OOO   R   R  DDDD   BBBB   A   A  P              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORDBAP  - Find appropriate records in core debit file    *~
            *            for application of open credits                *~
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

        sub "CORDBAP" (#1,               /* CORDRMAS                   */~
                       cuscode$,         /* Customer Code              */~
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
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            sortdate$6,                  /* Reverse Date               */~
            type$2,                      /* Type - Parent / Child      */~
            work$20,                     /* Miscellaneous Work Area    */~
            work$(2000)50,               /* Miscellaneous Work Area    */~
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
            * #01 ! CORDRMAS ! Core Debit Master File                   *~
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
            wrkmax% = dim(work$(),1)

            qtyopen = openqty : qtyopen2 = openqty
            rslvqty = 0

            wrk%, aplines%, pline% = 0%
            init (" ") apkey$()
            init (hex(ff)) work$()
            readkey$ = cuscode$

        REM *************************************************************~
            * PROCESS SECTION                                           *~
            *************************************************************

            type$ = "01"     /* DENOTE PARENT, HE'S FIRST ON THE LIST */
            plowkey$ = str(cuscode$,,9) & str(cpart$,,25)

L10070:     init (hex(00)) str(plowkey$,35)
            tempopen = qtyopen2

            call "PLOWALTS" (#1, plowkey$, 5%, 34%, f1%)
               if f1% = 0% then try_child
L10120:     get #1, using L10140, temp$, work$, sortdate$,                ~
                                 qty1, qty2, hold$, drop%
L10140:         FMT CH(9), CH(20), CH(6), POS(176), 2*PD(14,4),          ~
                    POS(304), CH(1), BI(2)
            if hold$ <> " "      then L10410

        /* Honor to the Nth degree, drop off date */
            goto L10260   /* Not Yet */
            td1$ = sortdate$ : td2$ = date
            call "DATE" addr("G-", td1$, td2$, d1%, d2%)
               if d2% <> 0% then L10260
                  if d1% > drop% then L10360
        /* Honor to the Nth degree, drop off date */

L10260:     if type$ = "01"      then L10290
            if temp$ <> cuscode$ then L10410

L10290:     qty = max(0, qty1 - qty2)
            if qty <= 0 then L10410
            wrk% = wrk% + 1%
            put str(work$(wrk%)) using L10340, type$, sortdate$, work$,   ~
                                              qty
L10340:         FMT CH(2), CH(6), CH(20), PD(14,4)
            tempopen = tempopen - qty
L10360:     if type$ <> "01" then L10380
               if wrk% >= apmax% then L10580
L10380:     if wrk% >= wrkmax% then L10580
            if tempopen > 0 then L10410
               if type$ = "01" then load_ap_array else try_child
L10410:     call "READNEXT" (#1, f1%)
               if f1% = 0% then try_child
               if str(key(#1,5%),,34%) <> str(plowkey$,,34%)             ~
                  then try_child
               goto L10120

        try_child
L10480:     call "PLOWALTS" (#1, readkey$, 1%, 9%, f1%)
               if f1% = 0% then L10580
            init (hex(ff)) str(readkey$,19%)
            if str(readkey$,10%,9%) = cuscode$ then L10480
            str(plowkey$,1%,9%) = str(readkey$,10%,9%)
               if type$ = "01" then pline% = wrk%
               if type$ = "01" then qtyopen2= tempopen
               type$ = "02"    /* DENOTE CHILD */
               goto L10070

L10580:     if wrk% = 0% then exit_program
            if type$ = "01" then load_ap_array
            if pline% = wrk% then load_ap_array
            call "SORT" addr(work$(), wrk%, 50%, work$())

        load_ap_array
            if wrk% = 0% then exit_program
            wrki% = 1%

L10670:     aplines% = aplines% + 1%
            get str(work$(wrki%)) using L10690, qty
L10690:         FMT POS(29), PD(14,4)
            qty = min(qty, qtyopen)

            apkey$(aplines%)              = str(work$(wrki%), 9%,20%)
            str(apkey$(aplines%),21%, 6%) = str(work$(wrki%), 3%, 6%)
            put str(apkey$(aplines%),27%, 8%) using L10750, qty
L10750:         FMT PD(14,4)

            qtyopen = qtyopen - qty
            if qtyopen = 0 then exit_load
            if aplines% >= apmax% then exit_load
               wrki% = wrki% + 1%
                 if wrki% > wrk% then exit_load
            goto L10670

        exit_load
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

        exit_program

            end
