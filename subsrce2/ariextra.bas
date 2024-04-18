        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   AAA   RRRR   IIIII  EEEEE  X   X  TTTTT  RRRR    AAA    *~
            *  A   A  R   R    I    E       X X     T    R   R  A   A   *~
            *  AAAAA  RRRR     I    EEEE     X      T    RRRR   AAAAA   *~
            *  A   A  R   R    I    E       X X     T    R   R  A   A   *~
            *  A   A  R   R  IIIII  EEEEE  X   X    T    R   R  A   A   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIEXTRA - Calculate how many extra lines a SO or Invoice *~
            *            line may generate.                             *~
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
            * 07/22/93 ! ORIGINAL                                 ! KAB *~
            * 12/19/94 ! Add Precious Metal Surcharge Count       ! RJH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "ARIEXTRA" (customer$,       /* Who?                       */~
                        part$,           /* What for?                  */~
                        type$,           /* Core, VAT, Pmetals, etc.   */~
                                         /*                 " " = All  */~
                        extra%,          /* How Many?                  */~
                        #1)              /* SYSFILE2                   */

        dim cor_def_prebill$1,           /* Default Prebill            */~
            cor_def_extras$1,            /* Default Extra Lines        */~
            cor_prebill$1,               /* Customer Prebill           */~
            cor_extras$1,                /* Customer Extra Lines       */~
            customer$9,                  /* Who?                       */~
            extra_allowed$1,             /* Flag                       */~
            lastcust$9,                  /* Same as last Who?          */~
            lastcustpm$9,                /* Same as last Who?          */~
            part$25,                     /* What?                      */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_cus_inv$1,                /* Precious Metal INV Flag    */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            pm_cus_so$1,                 /* Precious Metal SO Flag     */~
            lastpart$25,                 /* Same as last What?         */~
            plowkey$100,                 /* G/P Junk                   */~
            so_flag$1,                   /* Precious Metal SO Flag     */~
            inv_flag$1,                  /* Precious Metal INV Flag    */~
            type$1                       /* Core, VAT, etc. " " = All  */

        dim fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not checked (OPENCHCK)   */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************

            f1% = f1%   /* Need an executable statement here */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! System Control File                      *~
            * #02 ! CORPARNT ! Core Deposit Tracking Core Parent file.  *~
            * #03 ! COREXREF ! Core Deposit Cross Reference file.       *~
            * #04 ! HNYPMTBL ! Precious Metal Item  table               *~
            * #05 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "CORPARNT",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   10, keylen =   9,                     ~
                        alt key  1, keypos = 1, keylen =  18

            select #03, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =  1, keylen =  50,           ~
                            key  2, keypos = 76, keylen =  25, dup

            select #04, "HNYPMTBL",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  35,                     ~
                        alt key  1, keypos =   26, keylen =  10, dup

            select #05, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            if fs%(1%) <> 0% then L09000
               call "OPENCHCK" (#1, fs%(1%), 0%, 0%, " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            extra% = 0%
            if fs%(1%) < 0% then exit_program /* No SYSFILE2 - bye, bye */
            on pos("CP" = type$) goto L10000, L11000
                                        /* Fall Through if TYPE$ = " " */

L10000: REM *************************************************************~
            * Check for Core Bank Generated Extra Lines                 *~
            *************************************************************

            if core% = -1% then end_core /* Move on to next possibility */
            if core% <> 0% then L10200    /* All Ready Got Defaults      */
               plowkey$ = "SWITCHS.COR"
               call "READ100" (#1, plowkey$, core%)
                  if core% <> 0% then L10110
L10090:              core% = -1%
                     goto end_core       /* Move on to next possibility */
L10110:        get #1 using L10120, cor_def_prebill$, extra_allowed$,     ~
                                   cor_def_extras$

L10120:            FMT POS(117), CH(1), POS(136), CH(1), POS(137), CH(1)
               if extra_allowed$ <> "Y" then L10090
               call "OPENCHCK" (#3, fs%(3%), 0%, 0%, " ")
                  if fs%(3%) < 0% then L10090  /* No XREF, why bother */
               call "OPENCHCK" (#2, fs%(2%), 0%, 0%, " ")

L10200:     if lastcust$ = customer$ then L10300
               cor_prebill$, cor_extras$ = " " : lastcust$ = customer$
               call "READ100" (#2, customer$, f1%)
                  if f1% = 0% then L10260
               get #2 using L10250, cor_prebill$, cor_extras$
L10250:            FMT POS(30), CH(1), POS(36), CH(1)
L10260:     if cor_prebill$ = " " then cor_prebill$ = cor_def_prebill$
            if cor_extras$  = " " then cor_extras$  = cor_def_extras$

L10300:     if cor_prebill$ <> "Y" then end_core    /* Moving on . . . */
            if cor_extras$  <> "Y" then end_core    /* Moving on . . . */

            if lastpart$ = part$ then L10510
               lastpart$ = part$
               plowkey$  = part$
               call "PLOWALTS" (#3, plowkey$, 0%, 25%, add_core%)
                  if add_core% <> 0% then add_core% = 1%  /* jic */

*       ** Last, but not least tell how many extas core may generate
L10510:     extra% = extra% + add_core%
        /* It's just that simple */

        end_core
            if type$ = "C" then exit_program

L11000: REM *************************************************************~
            * Check for Precious Metals Generated Extra Lines            ~
            *************************************************************
            add_pm% = 0%
            if pm%   = -1% then end_pmetal /*Move on to next possibility*/
            if pm%   <> 0% then L11200    /* All Ready Got Defaults      */
               plowkey$ = "SWITCHS.BCK"
               call "READ100" (#1, plowkey$, pm%)
                  if pm% <> 0% then L11110
L11090:              pm% = -1%
                     goto end_pmetal     /* Move on to next possibility */
L11110:        get #1 using L11140, pm_on$, pm_so$, pm_inv$
L11140:            FMT POS( 60), 3*CH(1)
               if pm_on$ <> "Y" then goto end_pmetal
               call "OPENCHCK" (#4, fs%(4%), 0%, 0%, " ")
                  if fs%(4%) < 0% then L11090  /* No PMTBL so why bother */
               call "OPENCHCK" (#5, fs%(5%), 0%, 0%, " ")

L11200:     if lastcustpm$ = customer$ then L11300
            /* Get Customer Specific Defaults */
                pm_cus_so$, pm_cus_inv$ = " " : lastcustpm$ = customer$
                plowkey$ = customer$
                call "READ100" (#5, plowkey$, f1%(5%))
                   if f1%(5%) = 0% then L11300   /*Odd, but use defaults */
                get #5 using L11255, pm_cus_so$, pm_cus_inv$
L11255:           FMT POS(226), 2*CH(2)

            if pm_cus_so$ <> " " then so_flag$ = pm_cus_so$              ~
                                 else so_flag$ = pm_so$
            if pm_cus_inv$ <> " " then inv_flag$ = pm_cus_inv$           ~
                                 else inv_flag$ = pm_inv$

            if inv_flag$ = "N" and so_flag$ = "N" then goto end_pmetal

L11300:     /* Lets Count the PM's for this Part */
            add_pm% = 0%
            plowkey$ = str(part$) & hex(00)
L11320:     call "PLOWNEXT" (#04, plowkey$, 25%, f1%(4%))
            if f1%(4%) = 0% then L11400
            add_pm% = add_pm% + 1%
            goto L11320

L11400:     extra% = extra% + add_pm%

        end_pmetal
            if type$ = "P" then exit_program

*       *** Reserved for the next thing that comes along
*       *** But Until Then, just fall on out. . .

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
