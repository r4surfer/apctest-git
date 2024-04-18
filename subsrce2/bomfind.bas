        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  BBBB    OOO   M   M  FFFFF  IIIII  N   N  DDDD           *~
            *  B   B  O   O  MM MM  F        I    NN  N  D   D          *~
            *  BBBB   O   O  M M M  FFFF     I    N N N  D   D          *~
            *  B   B  O   O  M   M  F        I    N  NN  D   D          *~
            *  BBBB    OOO   M   M  F      IIIII  N   N  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMFIND  - RETURNS THE EFFECTIVE BOM ID FOR PART/DATE.    *~
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
            * 06/23/92 ! ORIGINAL                                 ! JDH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "BOMFIND" (part$,            /* Part to find BOM ID for    */~
                       date$,            /* Effective date - unformatd */~
                       #1,               /* ENGMASTR File              */~
                       #2,               /* SYSFILE2 File              */~
                       bom_id$)          /* BOM ID Returned            */

        dim bom_array$(490)3,            /* Effective BOM Array        */~
            bom_id$3,                    /* Effective BOM for Part     */~
            date$8,                      /* Effective Date Passed in   */~
            part$25,                     /* Part Passed in             */~
            readkey$99                   /* General Purpose Read Key   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "

        REM *************************************************************~
            *                  F I N D   T H E   B O M                  *~
            *************************************************************

            bom_id$ = " "

*        First we find out what the planning calendar date index is
            call "PIPINDEX" (#2, date$, dateindex%, err%)
            if err% <> 0% then L65000

*        Now we get the effective BOM Array
            readkey$ = str(part$) & "1" & hex(00)
            call "PLOWNEXT" (#1, readkey$, 26%, found%)
            if found% = 0% then L65000
            if str(readkey$,,25) <> part$ then L65000
                get #1 using L09150, str(bom_array$())
L09150:              FMT POS(30), CH(1470)

*        And finally we get the effective BOM ID - simple enough
            bom_id$ = bom_array$(dateindex%)

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
