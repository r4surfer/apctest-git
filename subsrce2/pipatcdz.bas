        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   IIIII  PPPP    AAA   TTTTT   CCC   DDDD   ZZZZZ   *~
            *  P   P    I    P   P  A   A    T    C   C  D   D     Z    *~
            *  PPPP     I    PPPP   AAAAA    T    C      D   D    Z     *~
            *  P        I    P      A   A    T    C   C  D   D   Z      *~
            *  P      IIIII  P      A   A    T     CCC   DDDD   ZZZZZ   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPATCDZ - THIS RETURNS THE PIP, SHELF, ATC1, AND ATC2    *~
            *            FOR A GIVEN PART ON A GIVEN DATE.              *~
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
            * 06/11/92 ! ORIGINAL                                 ! JDH *~
            * 10/23/92 ! Initialized returned error code to zero. ! JDH *~
            * 02/17/94 ! Option for honoring ATC Horizon numbers. ! JDH *~
            * 07/07/97 ! Changes for the year 2000.               !     *~
            *          ! Expect an unformated date argument.      ! DXL *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "PIPATCDZ" (part$,           /* Part                       */~
                        datein$,         /* Date (passed in unfrmatted)*/~
                        #1,              /* PIPMASTR file              */~
                        #2,              /* SYSFILE2 file              */~
                        #3,              /* SFCUM2   file              */~
                        pip%,            /* PIP qty for part/date      */~
                        shelf%,          /* Shelf qty for part/date    */~
                        atc1%,           /* ATC 1 qty for part/date    */~
                        atc2%,           /* ATC 2 qty for part/date    */~
                        err%,            /* Returned error message     */~
                                         /* 0 - All ok, fine           */~
                                         /* 1 - Date problem           */~
                                         /* 2 - No PIPMASTR record     */~
                        atch1%,          /* ATC 1 qty - honors horizon */~
                       atch2%,          /* ATC 2 qty - honors horizon */~
                        horz%)           /* HORIZON in days            */

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            call "NARGS" addr(args%) /* # of arguments passed in.  If */
                                     /* 13 then horizon numbers too.  */

        dim atc1%(490),                  /* ATC 1 Array                */~
            atc2%(490),                  /* ATC 2 Array                */~
            cumf%(490),                  /* Cumulative Forecast Array  */~
            date$8,                      /* Date for Calculations      */~
            datein$8,                    /* Date Passed In             */~
            part$25,                     /* Part Passed In             */~
            pip1%(490),                  /* PIP Array                  */~
            pip2%(490),                  /* Shelf Array                */~
            today$8                      /* Today's Date               */


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            today$ = date
            pip%, shelf%, atc1%, atc2% = 0%
            once%, err% = 0%
            if args% <> 13% then L09080
                atch1%, atch2% = 0%

L09080
*        Get Planning Calendar Date Indexes
            date$ = datein$  /* So Date doesn't change for caller */
            call "PIPINDEX" (#2, date$, date%, ret%)
            if ret% = 0% then L09160
                err% = 1% : end
L09160:     call "PIPINDEX" (#2, today$, today%, ret%)
            if ret% = 0% then L09200
                err% = 1% : end

L09200
*        Build Arrays
            call "READ100" (#1, part$, planned_part%)
            if planned_part% = 1% then L09260
                err% = 2% : end
L09260:     get #1 using L09270, atch% /* Part's ATC Horizon */
L09270:         FMT POS(2023), BI(2)
            atch% = mod(atch%, 1000%)/* Converted ATC Horizon */
            start% = 489%

                call "MXFL4GT" addr(#1, 26%, pip1%(1%), 490%)

L09290:         mat atc1% = pip1%
                call "MXATCSB" addr(atc1%(1%), pip1%(1%), atc1%(1%),     ~
                                    start%, 1%, today%)

            if once% = 1% then L09340 /* Done it once already */
                call "READ100" (#3, part$, fcst_there%)
L09340:            if fcst_there% <> 0% then L09370
                mat pip2% = pip1% : mat atc2% = atc1% : goto L09500

L09370:     if once% = 1% then L09420 /* Done it once already */
                call "MXFL4GT" addr(#3, 25%, cumf%(1%), 490%)
                call "MXSFASB" addr(pip2%(1%), pip1%(1), cumf%(1%),      ~
                                    1%, 490%, 0%)

L09420:         mat atc2% = pip2%
                call "MXATCSB" addr(atc2%(1%), pip2%(1), atc2%(1%),      ~
                                    start%, 1%, today%)

L09500
*        Set Values
            if once% = 1% then L09560 /* Done it already */
                pip%   = pip1%(date%)
                shelf% = pip2%(date%)
                atc1%  = atc1%(date%)
                atc2%  = atc2%(date%)
                goto L09600
L09560:     atch1%  = atc1%(date%)
            atch2%  = atc2%(date%)
            horz% = atch%
            goto L65000

L09600:     if args% <> 13% then L65000
                once% = 1% /* Done it once already flag */
                start% = min(489%, today% + atch%)
                goto L09290

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
