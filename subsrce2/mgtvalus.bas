        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  M   M   GGG   TTTTT  V   V   AAA   L      U   U   SSS    *~
            *  MM MM  G        T    V   V  A   A  L      U   U  S       *~
            *  M M M  G GGG    T    V   V  AAAAA  L      U   U   SSS    *~
            *  M   M  G   G    T     V V   A   A  L      U   U      S   *~
            *  M   M   GGG     T      V    A   A  LLLLL   UUU    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MGTVALUS - RETURNS THE MANAGEMENT TRANSFER FACTOR AND,    *~
            *            POSSIBLY, THE MANAGEMENT GL ACCOUNTS.          *~
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
            * 11/19/90 ! ORIGINAL                                 ! JDH *~
            * 05/15/91 ! Changed File Position for ICC & ICOC     ! JBK *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "MGTVALUS" (mode$,           /* "I'nvoice or 'S'hipping    */~
                        cuscode$,        /* Customer Code              */~
                        part$,           /* Part Number                */~
                        datein$,         /* Posting Date               */~
                        factor,          /* Management Transfer Factor */~
                        acct1$,          /* Intra-divisional account   */~
                        acct2$,          /* Alternate Owners account   */~
                        icc$,            /* Intercompany Corporate Code*/~
                        #03,             /* CUSTOMER File              */~
                        #04,             /* HNYMASTR File              */~
                        #02)             /* SYSFILE2 File              */

        dim acct1$9,                     /* Intra-divisional account   */~
            acct2$9,                     /* Alternate Owners account   */~
            accts$(4)9,                  /* Potential GL accounts      */~
            category$4,                  /* Part Category              */~
            cuscode$9,                   /* Customer Code              */~
            custype$2,                   /* Customer Type              */~
            date$8,                      /* Posting Date               */~
            datein$6,                    /* Posting Date               */~
            from$6,                      /* From effective date        */~
            icc$6,                       /* Intercompany Corporate Code*/~
            icoc$9,                      /* Alternate Owner Customer Cd*/~
            mgtrpt_on$1,                 /* Is Management Reporting on?*/~
            mode$1,                      /* "I'nvoice or 'S'hipping    */~
            part$25,                     /* Part Number                */~
            plowkey$99, readkey$99,      /* Misc PLOW & READ keys      */~
            to$6                         /* To expires date            */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01932
            cms2v$ = "R6.01.00 10/07/91 CMS General Release            "
L01932: REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! MGTFCTR2 ! Management Transfer Factors File         *~
            * #02 ! SYSFILE2 ! General Purpose System File              *~
            * #03 ! CUSTOMER ! Customer File                            *~
            * #04 ! HNYMASTR ! Part Master File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "MGTFCTR2",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =  1,   keylen = 46

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Open the files for the set requested
            if alreadybeenherebefore% = 1% then L09200
                alreadybeenherebefore% = 1%
                call "OPENCHCK" (#01, 0%, f2%(3),   0%, " ")

*        See if Management Reporting is on, plus the accounts
            call "READ100" (#02, "SWITCHS.GL", f1%(02))
            if f1%(2) = 1% then get #02 using L09130, mgtrpt_on$, accts$()
L09130:         FMT POS(59), CH(1), 4*CH(9)

L09200
*        Initialization
            init (" ") acct1$, acct2$, icc$, icoc$, custype$, category$
            factor = -999
            date$ = datein$
            call "DATEOK" (date$, d%, err$) /* Make sure its formatted */
            if d% = 0% then date$ = date else call "DATUNFMT" (date$)
            err$ = " "

        REM *************************************************************~
            *       H E R E ' S   T H E   M E A T                       *~
            *************************************************************

            if mgtrpt_on$ <> "Y" then end /* Done too much already! */

*        First get info on customer
            call "READ100" (#03, cuscode$, f1%(3))
                if f1%(3) <> 1% then end   /* Ouch!!! */
            get #03 using L10080, custype$, icc$, icoc$
L10080:         FMT POS(1023), CH(2), POS(1076), CH(6), CH(9)

            if icc$ = " " and icoc$ = " " then end  /* Nothing to do, */
                                                    /* normal sales.  */

*        Next get info for the part
            call "READ100" (#04, part$, f1%(4))
                if f1%(4) <> 1% then end   /* Non-stocked parts don't */
                                           /* count.                  */
            get #04 using L10180, category$
L10180:         FMT POS(90), CH(4)

*        Now determine the Management Tranfer Factor
            type% = 3%     /* Prime with type 3 transaction             */
            if icoc$ <> " " then L10260
                type% = 2% /* Since no ICOC, must be type 2 transaction */
                icoc$ = cuscode$             /* Set up for common reads */

L10260
*        Search hierarchy is as follows
*          Search (1) Customer Code / Part Number
*                 (2) Customer Code / Part Category
*                 (3) Customer Type / Part Number
*                 (4) Customer Type / Part Category

*        SEARCH #1-  Customer Code / Part Number
            init (" ") plowkey$,  readkey$
            str(plowkey$, 1, 9) = icoc$
            str(plowkey$,12,25) = part$
            gosub common_search

*        SEARCH #2-  Customer Code / Part Category
            init (" ") plowkey$,  readkey$
            str(plowkey$, 1, 9) = icoc$
            str(plowkey$,37, 4) = category$
            gosub common_search

            if type% <> 3% then L10470
*        Get the ICOCs Customer Type for type 3 transactions
            call "READ100" (#03, icoc$, f1%(3))
                if f1%(3) <> 1% then end_search  /* Shouldn't happen */
            get #03 using L10458, custype$
L10458:         FMT POS(1023), CH(2)

L10470
*        SEARCH #3-  Customer Type / Part Number
            init (" ") plowkey$,  readkey$
            str(plowkey$,10, 2) = custype$
            str(plowkey$,12,25) = part$
            gosub common_search

*        SEARCH #4-  Customer Type / Part Category
            init (" ") plowkey$,  readkey$
            str(plowkey$,10, 2) = custype$
            str(plowkey$,37, 4) = category$
            gosub common_search

            goto end_search  /* Must not have found anything */

        common_search
            readkey$ = " "
L10630:     call "PLOWNEXT" (#01, plowkey$, 40%, f1%(1))
            if f1%(1) = 0% then L10750
                get #01 using L10660, from$, to$
L10660:             FMT XX(40), 2*CH(6)
                if from$  > date$ then goto L10750  /* Not yet Effectve */
                if to$    < date$ then goto L10630  /* Already Expired  */
                    readkey$ = plowkey$            /* Might be the one */
                    get #01 using L10710, factor
L10710:                 FMT POS(62), PD(14,4)
                goto L10630

L10750:     if readkey$ = " " then return          /* No Record found  */
                return clear   /* End the Search here because we've    */
                               /* found what we were looking for.      */

        end_search
            if factor = -999 then end  /* Couldn't find a factor */
            if type% <> 3% then end    /* No use going on if not type 3 */

*        Now we set the GL accounts
            acct1$ = accts$(1)  /* Intra-divisional Sales Account      */
            acct2$ = accts$(2)  /* Alternate Owners Customer Sales Acct*/

            if mode$ = "I" then L10930

            acct1$ = accts$(3)  /* Intra-divisional CoGS Account       */
            acct2$ = accts$(4)  /* Alternate Owners Customer CoGS Acct */

L10930
*        Last we determine the ICC for the Atlernate Owner
            call "READ100" (#03, icoc$, f1%(3))
                if f1%(3) <> 1% then end   /* Oops!!! */
            get #03 using L10970, icc$
L10970:         FMT POS(1076), CH(6)

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
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
