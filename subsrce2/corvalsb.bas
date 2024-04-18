        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   CCC    OOO   RRRR   V   V   AAA   L       SSS   BBBB    *~
            *  C   C  O   O  R   R  V   V  A   A  L      S      B   B   *~
            *  C      O   O  RRRR   V   V  AAAAA  L       SSS   BBBB    *~
            *  C   C  O   O  R   R   V V   A   A  L          S  B   B   *~
            *   CCC    OOO   R   R    V    A   A  LLLLL   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORVALSB - LOCATES CORE PART FOR REMAN PARTS AND ACCOUNTS *~
            *            FOR CORE VALUE $ WHEN THE REMAN PART IS MOVED  *~
            *            AROUND.  CORE PART VALUE IS TO BE KEPT SEPARATE*~
            *            FROM REMAN PART VALUE                          *~
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
            * 02/04/93 ! ORIGINAL                                 ! JBK *~
            * 01/31/97 ! PRR - 13687.  Under certain conditions   ! JBK *~
            *          !  a blank part number can be passed in    !     *~
            *          !  and the sub was returning false infor   !     *~
            *          !  on it as though it were a reman part.   !     *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "CORVALSB" (core_trans$,     /* Transaction Type           */~
                                         /*  IA = Inventory Addition   */~
                                         /*  IC = Cycle count adjust.  */~
                                         /*  IP = Physical adjustment  */~
                                         /*  IW = Inventory Withdrawal */~
                                         /*  RT = A/R transaction      */~
                                         /*  CK = Check for Reman      */~
                        remanpart$,      /* Reman Part Number (Maybe)  */~
                        remanstore$,     /* Reman Store Code (Maybe)   */~
                        remanlot$,       /* Reman Lot Number (Maybe)   */~
                        reman_qty,       /* Quantity of Reman Part     */~
                                         /* Core Cost Returned (Maybe) */~
                        date$,           /* Module Date (YYMMDD)       */~
                        remanacct1in$,   /* Reman HNY Asset Acct(Maybe)*/~
                                         /* Core FG Acct Return (Maybe)*/~
                        remanacct2in$,   /* Reman Part Offset Acct     */~
                        glmod$,          /* G/L Module Posting Inventor*/~
                        jnlid$,          /* Journal ID                 */~
                        pstseq%,         /* Posting Sequence No        */~
                        gltextpassed$,   /* G/L Text Passed from Caller*/~
                        userid$,         /* User                       */~
                            #1,          /* UFB Address of HNYQUAN     */~
                            #2,          /* UFB Address of SYSFILE2    */~
                            #3,          /* UFB Address of HNYMASTR    */~
                            #4,          /* UFB Address of COREWORK    */~
                        ret%)            /* Error Code Returned;       */~
                                         /* 99 = Core Not Installed    */~
                                         /* 98 = No COREXREF File      */~
                                         /* 97 = Insufficient Quantity */~
                                         /* 96 = Not Reman Part        */~
                                         /* 95 = Core Part Not On File */~
                                         /* 94 = No Cost for Core      */~
                                         /* 93 = Invalid Trans Code    */~
                                         /* 92 = Miscellaneous Error   */~
                                         /*  0 = Valid Reman Part      */~

        dim                                                              ~
            acct$9,                      /* G/L Account                */~
            accts$(2)9,                  /* G/L Account Array          */~
            acct_dbcr(2,2),              /* G/L Posting Values         */~
            acct_type$1,                 /* Account Type For GLText    */~
            acct_type$(2)1,              /* Account Type For GLText    */~
            corekey$99,                  /* Key to the Core Master     */~
            corelot$16,                  /* Core Lot Number            */~
            corepart$25,                 /* Core Part Code             */~
            coreparttype$3,              /* Core Part Type Code        */~
            corestore$3,                 /* Core Part Store            */~
            core_flag_accts$(10)9,       /* Core Flags G/L Accounts    */~
            core_mstr_accts$(7)9,        /* Core Master G/L Accounts   */~
            core_trans$2,                /* Core Transaction Code      */~
            glmod$2,                     /* G/L Module                 */~
            gltext$100,                  /* G/L Posting Text           */~
            gltextpassed$100,            /* G/L Text Passed from Caller*/~
            jnlid$3,                     /* G/L Journal ID             */~
            location%(1),                /* Search Results             */~
            plowkey$99,                  /* Miscellaneous Plowkey      */~
            readkey$99,                  /* Miscellaneous Readkey      */~
            remanacct1in$9,              /* Reman Part HNY Asset Acct  */~
            remanacct2in$9,              /* Reman Part Offset Acct     */~
            remanlot$16,                 /* Reman Lot Code             */~
            remanpart$25,                /* Reman Part Code            */~
            remanstore$3,                /* Reman Store Code           */~
            reman_accts$(7)9,            /* Reman Part G/L Accounts    */~
            swtchs$200,                  /* Core Flags Variables       */~
            suspense_acct$16,            /* System Suspense Account    */~
            userid$3,                    /* User ID                    */~
            valid_trans$20               /* Valid Core Trans Codes     */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #05 ! COREXREF ! Core Part Cross Reference File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #05, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup     ~


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if been_here_before% <> 1% then L09075
                ret% = 99%                            /* Not Installed */
                if swtchs% = 99%  then end            /* Not Installed */

                ret% = 98%                            /* No COREXREF   */
                if swtchs% = 98%  then end            /* No COREXREF   */
                goto L10000

L09075
*        See if Core is Installed
               plowkey$ = "SWITCHS.COR"
               call "READ100" (#2, plowkey$, swtchs%)
               if swtchs% <> 0% then get #2 using L09120, swtchs$
               if swtchs%  = 0% then swtchs% = 99%
L09120:           FMT CH(200)

            REM  /* File #02- SYSFILE2 record layout for 'SWITCHS.COR' */~
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

            ret% = 99%
            if swtchs% = 99% then end                 /* Not Installed */
            ret% = 98%                  /* Set Up For No COREXREF File */

            call "OPENCHCK" (#5, corexref%, 0%, 0%, " ")
            if corexref% <> 1% then swtchs% = 98%     /* No COREXREF   */
            if swtchs% = 98% then end                 /* No COREXREF   */

*        Lets put the Core Flags Accts into format similar to COREXREF
            get swtchs$ using L09560,                                      ~
                        core_flag_accts$( 8%),  /* Unapplied Cores     */~
                        core_flag_accts$( 7%),  /* Core Variance Acct  */~
                        core_flag_accts$( 9%),  /* Interm Core Liab    */~
                        core_flag_accts$( 1%),  /* Core Bank Interm A/R*/~
                        core_flag_accts$( 4%),  /* Interim COGS Acct   */~
                        core_flag_accts$( 3%),  /* Interim Sales Acct  */~
                        core_flag_accts$( 2%),  /* Core Deposit Liab   */~
                        core_flag_accts$( 5%),  /* F/G Inventory Acct  */~
                        core_flag_accts$(10%),  /* Core Receipt Hold   */~
                        core_flag_accts$( 6%)   /* Core WIP Account    */
L09560:         FMT POS(21), 9*CH(9), POS(119), CH(9)

*        Get System Suspense Account
            readkey$ = "FISCAL DATES"
            call "READ100" (#2, readkey$, fiscal%)
                if fiscal% = 0% then L09650
            get #2 using L09630, suspense_acct$
L09630:         FMT POS(417), CH(16)

L09650
*        Set Valid Transaction codes
            valid_trans$ = "IAIWICIPRTCK"
            valid_tran_count% = len(valid_trans$)
            valid_tran_count% = int(valid_tran_count% / 2%)

            been_here_before% = 1%

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            ret% = 97%                       /* Set up for No Quantity */
            if core_trans$ = "CK" then L10080
            if abs(reman_qty) > .0001 then L10080
            end

L10080
*        Set up to see if this is a reman item with an associated core
            ret% = 96%                      /* Set up of No Reman Part */
            init (hex(00))  plowkey$
            plowkey$ = str(remanpart$)
                if remanpart$ <> " " then L10110
                     reman% = 0%
                     end

L10110:     call "PLOWALTS" (#5, plowkey$, 0%, 25%, reman%)
                if reman% = 0% then end               /* No Reman Part */

*        We have a reman part now get some information about it
            ret% = 95%                       /* Set up of No Core Part */
            get #5 using L10170, corepart$, reman_accts$()
L10170:         FMT POS(51), CH(25), POS(101), 6*CH(9), POS(170), CH(9)

            if corepart$ = " " then end

*        Have Core Part, see if can find standard cost for it
            ret% = 94%                        /* No cost for Core Part */
            call "STCCOSTS" (corepart$, " ", #2, 1%, corecost)
                if corecost = 0 then end

            core_extension = round(corecost * reman_qty, 2%)

*        Now test the CORE_TRANS and branch as necessary
            ret% = 93%                     /* Set up for invalid trans */
            search valid_trans$ = core_trans$ to location%() step 2%
                if location%(1%) = 0% then end
            core_trans% = int(location%(1%) / 2%) + 1%

            if core_trans% < 1% or                                       ~
               core_trans% > valid_tran_count% then end

*        Now set up GLTEXT$ for entries
            ret% = 92%               /* Set up for miscellaneous Error */
            gltext$ = gltextpassed$
            str(gltext$,31%, 34%) = str(corepart$)
            str(gltext$,65%,4%) = " "
            init (" ")  accts$(), acct_type$(), acct_type$
            mat acct_dbcr = zer

*        Branch to selected routine
            on core_trans% goto code_ia, code_iw, code_ip, code_ip,      ~
                                code_rt, code_ck
            end


        REM *************************************************************~
            *  Posting routines for the various core value transactions.*~
            *  Routines can be added as needed.  Usually only have to   *~
            *  find the source and destination G/L account numbers.     *~
            *************************************************************

*        Inventory additions, source account CR either core WIP or Core
*        Purchases, destination account DR Core Finished Goods

        code_ia                                  /* Inventory Addition */
*        Get Asset Account
            gosub'150 (5%, 1%, "H")                /* Set for Core F/G */

*        Get Offset Account
            call "READ100" (#3, corepart$, hnymastr%)
                if hnymastr% = 0% then L11155
            get #3 using L11150, coreparttype$
L11150:         FMT POS(180), CH(3)
L11155:     coreparttype% = 100%
            convert coreparttype$ to coreparttype%, data goto L11165
L11165:     source% = 1%                          /* Set for Purchases */
            if coreparttype% =   0% then source% = 2%   /* Set for WIP */
            if coreparttype% > 499% then source% = 2%   /* Set for WIP */
            on source% goto L11195, L11220

*        Get Purchases thru HNYGLGET
L11195:     call "HNYGLGET" (corepart$, remanstore$, remanlot$,          ~
                             accts$(2%), 2%, #3, #1)
            if accts$(2%) = " " then accts$(2%) = suspense_acct$
            acct_dbcr(2%,2%) = core_extension
            goto L11400

L11220
*        Get WIP account thru core accounts
            gosub'150 (6%, 2%, " ")                /* Set for Core WIP */

*        Prepare to Write G/L values to work file
L11400:     gosub write_work_file_loop
            ret% = 0%
            end

        code_iw                                 /* Inventory Withdrawl */
*        Get Asset Account - Core Finished Goods
            gosub'150 (5%, 1%, "H")

*        Get the Offset account - Core WIP
            gosub'150 (6%, 2%, " ")                /* Set for Core WIP */

*        Prepare to Write G/L values to work file
            gosub write_work_file_loop
            ret% = 0%
            end

        code_ip                 /* Physical Inv. or Cycle Count Adjust */
*        Get Asset Account - Core Finished Goods
            gosub'150 (5%, 1%, "H")

*        Get the Offset account - Core Inventory Variance Account
            gosub'150 (7%, 2%, " ")           /* Set for Core Variance */

*        Prepare to Write G/L values to work file
            gosub write_work_file_loop
            ret% = 0%
            end

        code_rt                                    /* Customer Returns */
*        Get Asset Account - Core Finished Goods
            gosub'150 (5%, 1%, "H")

*        Get the Offset account - Core Inventory Variance Account
            gosub'150 (7%, 2%, " ")           /* Set for Core Variance */

*        Prepare to Write G/L values to work file
            gosub write_work_file_loop
            ret% = 0%
            end

        code_ck          /* General, check for reman and return values */
*        Get Asset Account - Core Finished Goods
            gosub'150 (5%, 1%, "H")
            remanacct1in$ = accts$(1%)
            remanpart$    = corepart$
            reman_qty     = corecost
            ret% = 0%
            end

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

        REM *************************************************************~
            *             Core Value Account Search Routine             *~
            *                                                           *~
            * First check the Reman Part for the account.  Second check *~
            * the Core Master Part for the account.  Lastly, check the  *~
            * Core Flags account structure for the account.             *~
            *************************************************************

*        Search out the correct account from the Core Account Structure
        deffn'150(search%, index%, acct_type$)
            if search% < 1% or search% > 7% then L40300
            accts$(index%) = " "

*        First check the accounts for the Reman part for the Core Acct
            accts$(index%) = reman_accts$(search%)
                if accts$(index%) <> " " then found_account

*        Second Check the accounts for the Core Master for the Core Acct
*        Try to save extra reads of COREXREF if possible
            init (" ")  corekey$
            corekey$ = corepart$
            if fs(#5) < "10" and corepart$ <> " " and                    ~
                str(corekey$, 1%,50%) = key(#5,1%) then L40210
            call "REDALT0" (#5, corekey$, 1%, corepart%)
                if corepart% <> 1% then L40250
            get #5, using L40200, core_mstr_accts$()
L40200:         FMT POS(101), 6*CH(9), POS(170), CH(9)
L40210:     accts$(index%) = core_mstr_accts$(search%)
                if accts$(index%) <> " " then found_account

*        Not in Reman part or Core Master so try Core Flags Accounts
L40250:     accts$(index%) = core_flag_accts$(search%)
                if accts$(index%) <> " " then found_account

*        If by some chance still don't have account, default it to
*        the system suspense account
L40300:     accts$(index%) = suspense_acct$

*        Set-up Values for the Account
        found_account
            acct_dbcr(index%, index%) = core_extension
            acct_type$(index%)        = acct_type$
            return

*        Prepare to write the entries to the work file
        write_work_file_loop
            for i% = 1% to 2%
                acct$ = accts$(i%)
                debitamt  = acct_dbcr(i%, 1%)
                creditamt = acct_dbcr(i%, 2%)
                str(gltext$, 67%, 1%) = acct_type$(i%)
                gosub write_work_file
            next i%
            return


        write_work_file

            glamt = debitamt - creditamt
            if glamt < 0 then L41080
               debit1 = glamt
               credit1 = 0
                  goto L41110

L41080:        debit1 = 0
               credit1 = -(glamt)

L41110:     call "GETDTTM" addr(seq$)

            write #4,  using L41210, glmod$, jnlid$, pstseq%, userid$,    ~
                       seq$, date$, acct$, gltext$, debit1, credit1,     ~
                       date, corepart$, corestore$, corelot$, reman_qty, ~
                       corecost, core_extension, " ",                    ~
                       eod goto L41110

            return

L41210:     FMT                                                          ~
            CH(2),                       /* MODULE NUMBER              */~
            CH(3),                       /* JOURNAL ID                 */~
            BI(4),                       /* POSTING SEQUENCE NUMBER    */~
            CH(3),                       /* USER ID                    */~
            CH(7),                       /* SEQUENCE NUMBER            */~
            CH(6),                       /* GL POSTING DATE            */~
            CH(9),                       /* GL ACCOUNT NUMBER          */~
            CH(100),                     /* GL TEXT                    */~
            2*PD(14,4),                  /* DEBIT, CREDIT              */~
            CH(6),                       /* SYSTEM DATE                */~
            CH(25),                      /* Core Part                  */~
            CH(3),                       /* Core Store                 */~
            CH(16),                      /* Core Lot Number            */~
            PD(14,4),                    /* Core Quantity              */~
            PD(14,4),                    /* Core Cost                  */~
            PD(14,4),                    /* Core Extension             */~
            CH(32)                       /* filler                     */

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
