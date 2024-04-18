        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR   PPPP   RRRR    CCC    SSS   BBBB    *~
            *  C   C  O   O  R   R  P   P  R   R  C   C  S      B   B   *~
            *  C      O   O  RRRR   PPPP   RRRR   C       SSS   BBBB    *~
            *  C   C  O   O  R   R  P      R   R  C   C      S  B   B   *~
            *   CCC    OOO   R   R  P      R   R   CCC    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORPRCSB - Derive 'Sell Price' via criteria established   *~
            *            in CORxxBLD.  Not used there because more      *~
            *            extensive use of defaults would make for       *~
            *            redundancy, but should be used wherever else   *~
            *            pricing is required.                           *~
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
            * 09/30/92 ! Original                                 ! KB2 *~
            * 08/19/93 ! Premature branch preventing use of system! KB2 *~
            *          !   level price code.                      !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "CORPRCSB" (pcuscode$,       /* Customer Code              */~
                        prpart$,         /* Reman Part                 */~
                        pacpart$,        /* Actual Core Part           */~
                        pacflag$,        /* Actual Core Part Flag      */~
                        pprdate$,        /* Pricing Date               */~
                        prqty,           /* Pricing Quantity           */~
                        price,           /* Price Returned             */~
                        #01,             /* SYSFILE2                   */~
                        #02,             /* CORPARNT                   */~
                        #03,             /* COREXREF                   */~
                        #05)             /* HNYMASTR                   */

        dim /* Argument List                                           */~
            pcuscode$9,      /* Customer Code                          */~
            prpart$25,       /* Reman Part, if Available. May be blnk  */~
            pacpart$25,      /* Actual Core Part  - Use this if        */~
                             /*   1) Reman Part not found in XREF or   */~
                             /*      blank                             */~
                             /*   2) Reman Part Currently linked to    */~
                             /*      something else & Flag is not blnk */~
            pacflag$1,       /* Actual Core Part Override Flag         */~
            pprdate$8        /* Pricing Date, formatted or not         */

        dim /* General Purpose / Misc. Variables                       */~
            acpart$25,                   /* Alternate Core Part        */~
            acflag$1,                    /* Alternate Core Flag        */~
            rcpc$1, pcpc$1,              /* Price Codes   (Pricing)    */~
            rpart$25,                    /* Reman Part                 */~
            cpart$25,                    /* Core  Part                 */~
            prdate$8,                    /* Pricing Date               */~
            cat$4,                       /* Category Code (Pricing)    */~
            custype$2,                   /* Customer Type (Pricing)    */~
            plowkey$99, readkey$99,      /* Miscellaneous Read/Plow Key*/~
            swtchs$200,                  /* Core Switches              */~
            userid$3                     /* Current User Id            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! CORPARNT ! Core Parent Cross Reference File         *~
            * #03 ! COREXREF ! Core Part Cross Reference File           *~
            * #05 ! HNYMASTR ! Parts Master File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        /*  SELECT #01, "SYSFILE2",                                      ~
                        VARC,     INDEXED,  RECSIZE =  500,              ~
                        KEYPOS =    1, KEYLEN =  20                      ~

            SELECT #02, "CORPARNT",                                      ~
                        VARC,     INDEXED,  RECSIZE =  100,              ~
                        KEYPOS =   10, KEYLEN =   9,                     ~
                        ALT KEY  1, KEYPOS =    1, KEYLEN =  18          ~

            SELECT #03, "COREXREF",                                      ~
                        VARC,     INDEXED,  RECSIZE =  500,              ~
                        KEYPOS =   26, KEYLEN =  50,                     ~
                        ALT KEY  2, KEYPOS =   76, KEYLEN =  25, DUP,    ~
                            KEY  1, KEYPOS =    1, KEYLEN =  50          ~

            SELECT #05, "HNYMASTR",                                      ~
                        VARC,     INDEXED,  RECSIZE = 1000,              ~
                        KEYPOS =    1, KEYLEN =  25                      ~
        */
            if userid$ <> " " then L09380

            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#02, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#03, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#05, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)

            readkey$ = "SWITCHS.COR"
            call "READ100" (#1, readkey$, swtchs%)
               if swtchs% <> 0% then get #1 using L09120 , swtchs$
               if swtchs%  = 0% then swtchs% = 99%
L09120:           FMT CH(200)

            REM  /* File #01- SYSFILE2 record layout for 'SWITCHS.COR' */~
                CH(20), /*    1/20       /* Key- 'SWITCHS.COR'         */~
                CH(09), /*   21/9        /* Unapplied Cores G/L Acct   */~
                CH(09), /*   30/9        /* Core Variance G/L Acct     */~
                CH(09), /*   39/9        /* Interim Core Liability G/L */~
                CH(09), /*   48/9        /* Core Bank Interim A/R G/L  */~
                CH(09), /*   57/9        /* Interim COGS G/L Acct      */~
                CH(09), /*   66/9        /* Interim Sales G/L Acct     */~
                CH(09), /*   75/9        /* Core Deposit Liability G/L */~
                CH(09), /*   84/9        /* F/G Inventory G/L Acct #   */~
                CH(09), /*   93/9        /* Core Receipt Hold          */~
                CH(01), /*  102/1        /* Post Interim COGS/Sales?   */~
                CH(01), /*  103/1        /* Write Manual Adj. Audit Fl?*/~
                BI(02), /*  104/2        /* Default Drop-Off Days      */~
                CH(01), /*  106/1        /* Auto-assign Document ID?   */~
                CH(03), /*  107/3        /* Next Document ID Prefix    */~
                BI(04), /*  110/4        /* Next Document ID #         */~
                CH(1),  /*  114/1        /* Cost Flag                  */~
                CH(1),  /*  115/1        /* Price Flag                 */~
                CH(1),  /*  116/1        /* Price Code                 */~
                CH(1),  /*  117/1        /* Post as Unapplied?         */~
                CH(1),  /*  118/1        /* Credit Memo on Application */~
                CH(9)   /*  119/9        /* Core WIP Account           */

L09380:     price      = 0                  /* Price Returned          */
            if swtchs% = 99% then exit_program        /* NOT INSTALLED */

                cuscode$ = pcuscode$        /* Customer Code           */
                rpart$   = prpart$          /* Reman Part              */
                acpart$  = pacpart$         /* Actual Core Part        */
                acflag$  = pacflag$         /* Actual Core Part Flag   */
                prdate$  = pprdate$         /* Pricing Date            */
                qtyorig  = prqty            /* Pricing Quantity        */
                rcpc$    = " "              /* Price Code              */

                call "DATEOK" (prdate$, f1%, err$)
                   if err$ <> " " then L09520
                call "DATUNFMT" (prdate$)
                goto L09540
L09520:            prdate$ = date

L09540:         cdflt% = -1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            if rpart$ = " " then L10100
            plowkey$ = rpart$
            call "PLOWALTS" (#3, plowkey$, 0%, 25%, f1%)
               if f1% <> 0% then L10120         /* Got One              */
L10100:           if acpart$ = " " then c_override     /* Can't Help   */
                  goto L10150
L10120:     if acpart$ = " " then L10190        /* Nothing Requested    */
            if str(plowkey$,26,25) = acpart$ then L10190 /* Same Thing  */
            if acflag$ = " " then L10190        /* Nothing Requested    */
L10150:        plowkey$ = acpart$
               call "REDALT0" (#3, plowkey$, 1%, cdflt%)
                  if cdflt% = 0% then c_override      /* Missed!       */

L10190:     get #3 using L10200, cpart$, price, rcpc$
L10200:         FMT POS(51), CH(25), POS(155), PD(14,4), CH(1)

            /* Got first level of defaults                      */

            if price  >  0  then L10660         /* Got a 'Fixed' Price */
            if rcpc$ <> " " then c_override    /* Try with this       */
            if cdflt% >= 0% then c_override    /* All Done Here       */
            plowkey$ = cpart$
            call "REDALT0" (#3, plowkey$, 1%, cdflt%)
               if cdflt% <> 0% then c_override   /* All Done Here     */

            get #3 using L10350, price, rcpc$
L10350:         FMT POS(155), PD(14,4), CH(1)
            if price  >  0  then L10660         /* Got a 'Fixed' Price */

        c_override
            if rcpc$ <> " " then rcpc$ = str(swtchs$,116%,1%)
            pcpc$, custype$ = " "
            call "READ100" (#2, cuscode$, f1%)
               if f1% = 0% then final_pricing
            get #2 using L10460, pcpc$, custype$
L10460:         FMT XX(31), CH(1), CH(2)

        final_pricing
            if pcpc$ <> " " then rcpc$ = pcpc$
            if rcpc$ = " " then L10660    /* All we can do !!    */
            cat$ = " ": conv = 1
            call "READ100" (#5, cpart$, f1%)
               if f1% <> 0% then get #5 using L10540, conv, cat$
L10540:           FMT POS(82), PD(14,7), CH(4)
            call "CPRASSGN" (cuscode$, custype$, cpart$, cat$, rcpc$,    ~
                             prdate$, " ", "    ", -1, qtyorig, #1, #5,  ~
                             price, disc, err$)
            if price <= 0  then L10600
            if err$  = " " then L10630
L10600:        price = 0
               goto L10660

L10630:     if disc >  0 then price = (price * (100 - disc)) * .01
            if conv <> 0 then price =  price / conv

L10660:        price = round(price, 4)

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
