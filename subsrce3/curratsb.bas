        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   CCC   U   U  RRRR   RRRR    AAA   TTTTT   SSS   BBBB    *~
            *  C   C  U   U  R   R  R   R  A   A    T    S      B   B   *~
            *  C      U   U  RRRR   RRRR   AAAAA    T     SSS   BBBB    *~
            *  C   C  U   U  R   R  R   R  A   A    T        S  B   B   *~
            *   CCC    UUU   R   R  R   R  A   A    T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CURRATSB - Returns the exchange rates for the date, table *~
            *            and currency passed in.                        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1993, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/19/93 ! ORIGINAL                                 ! JDH *~
	    * 05/28/96 ! Changes for the year 2000                ! DXL *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "CURRATSB" (currency$, table$, date$, equiv, per_stat, err$)

        dim                                                              ~
            currency$4,                  /* Currency Code              */~
            date$8,                      /* Date passed in - formatted */~
            err$79,                      /* Error Message              */~
            plowkey$99,                  /* Plow Variable              */~
            rev_date$6,                  /* Reverse date for Exchange  */~
            table$1                      /* Exchange Rate Table        */~

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #40 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            *************************************************************~
            *                                                           *

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L02122
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L02122: REM *************************************************************
            select #40, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            if fs% <> 1% then call "OPENCHCK" (#40, fs%, f2%, 0%, " ")
            if fs% = 1% then L03000
                err$ = "Currency Exchange Rate file is not available."
                goto L65000

L03000: REM *************************************************************~
            * Real work begins here.                                    *~
            *************************************************************

*        First we calulate the reverse date
         call "DATREVRS" ( date$, rev_date$, err$ )
            if err$ <> " " then L65000

*        Now get the rates
            equiv, per_stat = 1.0
            plowkey$ = str(table$) & str(currency$) & rev_date$
            call "PLOWNEXT" (#40, plowkey$, 5%, f2%)
            if f2% = 1% then L03200
                err$ = "Exchange Rates not on file."
                goto L65000

L03200:     get #40 using L03210, equiv, per_stat
L03210:         FMT POS(18), 2*PD(14,7)

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
