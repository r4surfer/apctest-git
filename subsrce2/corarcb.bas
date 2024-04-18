        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR    AAA   RRRR    CCC   BBBB           *~
            *  C   C  O   O  R   R  A   A  R   R  C   C  B   B          *~
            *  C      O   O  RRRR   AAAAA  RRRR   C      BBBB           *~
            *  C   C  O   O  R   R  A   A  R   R  C   C  B   B          *~
            *   CCC    OOO   R   R  A   A  R   R   CCC   BBBB           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORARCB  - Create Core Master Records from 'invoice like' *~
            *            input, perform applications if possible and    *~
            *            save data.                                     *~
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
            * 08/13/92 ! Original                                 ! KEN *~
            * 11/04/92 ! Better Handling of FS22.                 ! KEN *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "CORARCB" (f$,           /* Function -                 */~
                                         /*   "I " - non specific inv. */~
                                         /*          checks qty sign.  */~
                                         /*          makes Positive    */~
                                         /*          and becomes PD/PC */~
                                         /*   "PD" - Pre-debit         */~
                                         /*   "PC" - Pre-credit        */~
                                         /*   "D " - Write ready debit */~
                                         /*   "C " - Write ready credt */~
                                         /*   "DF" - [RE]write debit   */~
                                         /*   "CF" - [RE]write credit  */~
                           apply$,       /* Apply - 'Y' or other       */~
                           record$(),    /* Passed in - either write   */~
                                         /*           ready dbt or crdt*/~
                                         /*           or ala CORxxBLD  */~
                                         /*       out - Master Written */~
                           ret%)         /* 99 - Core not installed    */
                                         /* 98 - Build or Funct Error  */
                                         /* 97 - CORJNLSB Error        */
                                         /* 22 - Can't Write, Dup      */
                                         /*  0 - All Ok                */

        dim                                                              ~
            acct$9,                      /* Variance Account           */~
            apkey$(300)40,               /* Application Keys           */~
            apply$1,                     /* Apply - 'Y' or other       */~
            cpart$25,                    /* Core Part                  */~
            cuscode$9,                   /* Customer Code              */~
            f$2, fn$2,                   /* Function                   */~
            line1$200, line2$200,        /* Line Image                 */~
            readkey$99, plowkey$99,      /* G.P. Read/Plow Keys        */~
            record$(4)256,               /* Master Image               */~
            pcuscode$9,                  /* Parent Customer            */~
            swtchs$200,                  /* Switches Contents          */~
            userid$3                     /* Who Else                   */

        dim                                                              ~
            gltrn0$90,         /* G/L Transaction Data Block - Hdr Old */~
            gltrn1$90,         /* G/L Transaction Data Block - Hdr New */~
            gltran0$90,        /* G/L Transaction Data Block - Old     */~
            gltran1$90,        /* G/L Transaction Data Block - New     */~
            gltranx$90,        /* G/L Transaction Data Block - Inactive*/~
            glkey$25,          /* G/L Transaction Master/Det. Key      */~
            glinfo$135,        /* G/L Transaction Information          */~
            gltext$30,         /* G/L Transaction Text                 */~
            gltype$2,          /* G/L Transaction Type                 */~
            gldacct$9,         /* G/L Transaction Detail Account       */~
            gldtext$30         /* G/L Transaction Detail Text          */

        REM /* Associated Numeric Variables                            */~
            GLQTY,             /* G/L Transaction Master Quantity      */~
            GLPRICE,           /* G/L Transaction Master Price         */~
            GLCOST,            /* G/L Transaction Master Cost          */~
            GLDQTY,            /* G/L Transaction Detail Quantity      */~
            GLSEQ%,            /* G/L Data Save Counter                */~
            GLSEQ1%            /* G/L Data Save Trans Count (MSTR = 0) */

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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! CORDRMAS ! Core Debit Master File                   *~
            * #03 ! CORDRLIN ! Core Debit Lines File                    *~
            * #09 ! COREXREF ! Core Cross Reference File                *~
            * #12 ! CORCRMAS ! Core Credit Master File                  *~
            * #13 ! CORCRLIN ! Core Credit Lines File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "CORDRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   19, keylen =  11, dup,    ~
                            key  3, keypos =   36, keylen =  34, dup,    ~
                            key  4, keypos =   61, keylen =  40, dup,    ~
                            key  5, keypos =  601, keylen =  40, dup     ~

            select #03, "CORDRLIN",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  23                      ~

            select #09, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #12, "CORCRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   19, keylen =  11, dup,    ~
                            key  3, keypos =   36, keylen =  34, dup,    ~
                            key  4, keypos =   61, keylen =  40, dup,    ~
                            key  5, keypos =  601, keylen =  40, dup     ~

            select #13, "CORCRLIN",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  23                      ~

            if swtchs% <> 0% then L09000
            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")

            readkey$ = "SWITCHS.COR"
            call "READ100" (#1, readkey$, swtchs%)
               if swtchs% <> 0% then L05070
                  swtchs%  = -1% : goto L09000
L05070:     get #1 using L05080, swtchs$
L05080:         FMT POS(21), CH(200)
            swtchs% = 1%
            call "OPENCHCK" (#02, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#03, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#09, 0%, 0%,   0%, " ")
            call "OPENCHCK" (#12, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#13, 0%, 0%, 100%, " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            ret% = 99%
            if swtchs% < 1% then exit_program
            fn$ = f$
            gltranx$ = "X": init (hex(00)) str(gltranx$,2%,24%)

        REM *************************************************************~
            * M A I N   P R O G R A M - Phase 1                         *~
            *-----------------------------------------------------------*~
            * Handles Pre-Testing and Building                          *~
            *************************************************************

            if fn$ <> "I " then test_pd
               get str(record$()) using L10080, qty
L10080:            FMT POS(93), PD(14,4)
               if qty = 0 then build_error
               if qty < 0 then L10120
                  fn$ = "PD"
                  goto test_pd
L10120:        put str(record$()) using L10080, -qty
               fn$ = "PC"

        test_pd
            if fn$ <> "PD" then test_pc
               call "CORDBBLD" (record$(), ret%)
                  if ret% > 10% then build_error
               fn$ = "D "
               goto apply_d

        test_pc
            if fn$ <> "PC" then test_fn
               call "CORCRBLD" (record$(), ret%)
                  if ret% > 10% then build_error
               fn$ = "C "
               goto apply_c

        build_error
            ret% = 98%
            goto exit_program

        test_fn
            if fn$ = "DF" then apply_d
            if fn$ = "D " then apply_d
            if fn$ = "CF" then apply_c
            if fn$ = "C " then apply_c
               goto build_error

        REM *************************************************************~
            * M A I N   P R O G R A M - Debits                          *~
            *-----------------------------------------------------------*~
            * Checks for Duplicates and Application                     *~
            *************************************************************

        apply_d
            if str(fn$,2%,1%) = "F" then L11140
               readkey$ = str(record$(),10,20)
               call "READ100" (#2, readkey$, f1%)
                  if f1% = 0% then L11140
                     ret% = 22%
                     goto exit_program

L11140
*        Set Up GLTRN1$
            glseq% = -1% : glseqinc% = 1%
            gosub get_db_jnl

            if apply$ <> "Y" then write_dbm

            get str(record$()) using L11440, pcuscode$, cuscode$, cpart$, ~
                                            openqty, qtyrslv, dblines%
L11440:         FMT POS(1), 2*CH(9), POS(70), CH(25),                    ~
                    POS(176), 2*PD(14,4), POS(317), BI(2)
            openqty = openqty - qtyrslv : rslvqty = 0

            call "CORCRAP" (#12,         /* CORCRMAS                   */~
                        cuscode$,        /* Customer Code              */~
                        pcuscode$,       /* Parent Customer Code       */~
                        cpart$,          /* Core Part Code             */~
                        openqty,         /* Open Quantity              */~
                        apkey$(),        /* Keys to records for applic.*/~
                        aplines%,        /* Number of entries in above */~
                        rslvqty)         /* Quantity Resolved          */

            if aplines% = 0% then write_dbm
                rslvqty = 0
                gosub get_cpart_var_account
                line1$, line2$ = " "
                str(line1$,  1%, 20%) = str(record$(), 10%, 20%)
                str(line1$, 32%,  8%) = str(record$(),200%,  8%)
                str(line1$, 40%,  8%) = str(record$(),192%,  8%)
                str(line1$, 48%,  6%) = str(record$(), 30%,  6%)
                str(line1$, 54%,  6%) = date
                str(line1$, 60%,  6%) = date
                str(line1$, 60%,  6%) = xor hex(ffffffffffff)
                str(line1$, 66%,  9%) = acct$
                str(line1$, 75%, 30%) = "Auto. Application"
                str(line1$,105%, 19%) = str(record$(),126%, 19%)
                str(line1$,124%, 19%) = str(record$(),151%, 19%)
                str(line1$,143%,  1%) = "A"
                str(line1$,144%,  4%) = hex(ffffffff)
                str(line1$,155%,  3%) = userid$
                str(line1$,158%,  6%) = date

                str(line2$, 45%, 25%) = cpart$
                init (hex(00)) str(line2$,114%)

            for i% = 1% to aplines%
                str(line1$, 21%,  3%) = " "
                str(line1$, 24%,  8%) = str(apkey$(i%),27%,8%)

                str(line2$, 22%, 20%) = str(apkey$(i%),,20%)
                str(line2$, 42%,  3%) = " "
                str(line2$, 70%,  6%) = str(record$(), 30%,  6%)
                str(line2$, 90%,  8%) = str(apkey$(i%),27%,8%)
                str(line2$, 98%,  8%) = str(record$(),200%,  8%)
                str(line2$,106%,  8%) = str(record$(),192%,  8%)

            get apkey$(i%) using L12120, qty
L12120:         FMT POS(27), PD(14,4)

            dblines% = dblines% + 1%
            convert dblines% to str(line1$,21,3), pic(###)

            plowkey$ = str(line2$,22,20)
            call "READ101" (#12, plowkey$, f1%)
               if f1% <> 0% then L12230
                  str(line2$,1,1) = "X"
                  goto next_db_app

L12230:     get #12 using L12260, str(line2$, 70%,  6%), crrslv,          ~
                                  str(line2$,106%,  8%),                 ~
                                  str(line2$, 98%,  8%), crseq%
L12260:         FMT POS(30), CH(6), POS(184), PD(14,4), 2*CH(8),         ~
                    POS(317), BI(2)

            crrslv = crrslv + qty
            put #12 using L12292, crrslv
L12292:         FMT POS(184), PD(14,4)
        db_cr_eod
            crseq% = crseq% + 1%
            convert crseq% to str(line2$,42,3), pic(###)
            put #12 using L12320, crseq%
L12320:         FMT POS(317), BI(2)
            rewrite #12

            write #13 using L12460, str(line2$,22,23),                    ~
                                   str(line2$,90,24),                    ~
                                   str(line2$,70, 6),                    ~
                                   str(line1$,54,110),                   ~
                                   " ",                                  ~
                                   str(line1$, 1,23),                    ~
                                   cpart$,                               ~
                                   str(line1$,48, 6),                    ~
                                   " ",                                  ~
                                   str(line1$,24,24),                    ~
                                   str(line2$,114,24),                   ~
                                   eod goto L12472
L12460:     FMT CH(23), CH(24), CH(6), CH(110), CH(21), CH(23), CH(25),  ~
                CH(6), CH(14), CH(24), CH(24)
            goto L12490
L12472
*        EOD Disaster
            call "READ101" (#12, plowkey$, f1%)
               if f1% = 0% then next_db_app
            get #12 using L12320, crseq%
            goto db_cr_eod

L12490:     gltran1$ = " "
            get #12 using L12620,                                         ~
                   glpart$,                                              ~
                   str(gltran1$,10%,16%),/* Price, Cost                */~
                   str(gltran1$,26%,45%),/* 'Asset' Account            */~
                                         /*  A/R    Account            */~
                                         /*  Sales  Account            */~
                                         /*  COGS   Account            */~
                                         /*  CDL    Account            */~
                   str(gltran1$,80%, 2%),/*  CDL    Flag               */~
                                         /*  Unap.  Flag               */~
                   str(gltran1$,71%, 9%) /*  Unap.  Account            */

L12620:         FMT POS(36), CH(25), POS(192), CH(16),                   ~
                    POS(544), CH(45), CH(2), XX(1), CH(9)

                gltran0$ = gltranx$
                get #13 using L12670, glkey$, gldqty, gldacct$, gldtext$
L12670:             FMT CH(23), PD(14,4), POS(66), CH(9), CH(30)
*              GLDQTY = -GLDQTY
                put gltran1$ using L12700, gldqty, gldacct$
L12700:             FMT POS(2), PD(14,4), POS(26), CH(9)
                str(gltype$,1,2) = "DB"
                glseq1% = 10*i% + 1%
                gltext$ = "Debit Line Added, Appl."
                if gldtext$ <> " " then gltext$ = gldtext$
                gosub write_line_jnl

L12770:     write #3 using L12780, str(line1$,,163), str(line2$,,137),    ~
                     eod goto L12782
L12780:               FMT CH(163), CH(137)
            goto L12795
L12782
*        Another kind of EOD Disaster
                dblines% = dblines% + 1%
                convert dblines% to str(line1$,21,3), pic(###)
                call "READ101" (#13, str(line2$,22,23), f1%)
                   if f1% = 0% then L12770
                put #13 using L12788, str(line1$, 1,23)
L12788:             FMT POS(185), CH(23)
                rewrite #13
                goto L12770

L12795:         gltran0$ = gltranx$ : gltran1$ = gltrn1$
                get #3 using L12815, glkey$, gldqty, gldacct$, gldtext$,  ~
                                    str(gltype$,2)
L12815:             FMT CH(23), PD(14,4), POS(66), CH(9), CH(30),        ~
                        POS(143), CH(1)
                gldqty = -gldqty
                put gltran1$ using L12835, gldqty, gldacct$
L12835:             FMT POS(2), PD(14,4), POS(26), CH(9)
                str(gltype$,1,1) = "D"
                glseq1% = 10*i%
                glpart$ = str(record$(),70%,25%)
                gltext$ = "Debit Line Added"
                if gldtext$ <> " " then gltext$ = gldtext$
                gosub write_line_jnl

            rslvqty = rslvqty + qty

        next_db_app
            next i%

            qtyrslv = qtyrslv + rslvqty
            put str(record$()) using L12950, qtyrslv, dblines%
L12950:         FMT POS(184), PD(14,4), POS(317), BI(2)


        write_dbm

            readkey$ = str(record$(),10,20)
            call "READ101" (#2, readkey$, f1%)

            if f1% <> 0% then L13260
               gltext$  = "New Core Debit Record"
               goto L13300

L13260:     gltext$ = "Update Core Debit Record"

*        OK to WRITE or REWRITE

L13300:     put #2 using L13310, str(record$(),,650%)
L13310:         FMT CH(650)
            if f1% = 0% then write #2 else rewrite #2

            glinfo$ = str(record$(),10%,20%)
            str(glinfo$,26%,25%) = str(record$(),70%,25%)
            str(glinfo$,51%,30%) = gltext$

            call "CORJNLSB" ("JW", glseq%, 0%, glseqinc%, 1%,            ~
                             "D ", gltrn1$, gltrn0$, glinfo$, ret%)

            goto exit_program

        get_db_jnl
*        Set Up GLTRN1$, GLTRN0$
            gltrn1$ = " "
            get str(record$()) using L13670, glqty, glprice, glcost,      ~
                   str(gltrn1$,26%,45%),/* 'Asset' Account             */~
                                        /*  A/R    Account             */~
                                        /*  Sales  Account             */~
                                        /*  COGS   Account             */~
                                        /*  CDL    Account             */~
                   str(gltrn1$,80%, 2%),/*  CDL    Flag                */~
                                        /*  Unap.  Flag                */~
                   str(gltrn1$,71%, 9%) /*  Unap.  Account             */


L13670:         FMT POS(176), PD(14,4), POS(192), 2*PD(14,4),            ~
                    POS(544), CH(45), CH(2), XX(1), CH(9)

*          GLQTY = -GLQTY
            put str(gltrn1$, 2%,24%) using L13695, glqty, glprice, glcost
L13695:         FMT 3*PD(14,4)

            readkey$ = str(record$(),10,20)
            call "READ100" (#2, readkey$, f1%)

            init (" ") gltrn0$
            if f1% <> 0% then L13745
               gltrn0$ = gltranx$
               return

L13745:     get #2 using L13805,                                          ~
                   str(gltrn0$, 2%, 8%), /* Original Qty               */~
                   str(gltrn0$,10%,16%), /* Price, Cost                */~
                   str(gltrn0$,26%,45%), /* 'Asset' Account            */~
                                         /*  A/R    Account            */~
                                         /*  Sales  Account            */~
                                         /*  COGS   Account            */~
                                         /*  CDL    Account            */~
                   str(gltrn0$,80%, 2%), /*  CDL    Flag               */~
                                         /*  Unap.  Flag               */~
                   str(gltrn0$,71%, 9%)  /*  Unap.  Account            */

L13805:         FMT POS(176), CH(8), POS(192), CH(16),                   ~
                    POS(544), CH(45), CH(2), XX(1), CH(9)

            return

        REM *************************************************************~
            * M A I N   P R O G R A M - Credits                         *~
            *-----------------------------------------------------------*~
            * Checks for Duplicates and Application                     *~
            *************************************************************

        apply_c
            if str(fn$,2%,1%) = "F" then L14140
               readkey$ = str(record$(),10,20)
               call "READ100" (#12, readkey$, f1%)
                  if f1% = 0% then L14140
                     ret% = 22%
                     goto exit_program

L14140
*        Set Up GLTRAN$
            glseq% = -1% : glseqinc% = 1%
            gosub get_cr_jnl

            if apply$ <> "Y" then write_crm

            get str(record$()) using L14440, cuscode$, cpart$,            ~
                                            openqty, qtyrslv, crlines%
L14440:         FMT POS(10), CH(9), POS(70), CH(25),                     ~
                    POS(176), 2*PD(14,4), POS(317), BI(2)
            openqty = openqty - qtyrslv : rslvqty = 0

            call "CORDBAP" (#2,          /* CORDBMAS                   */~
                        cuscode$,        /* Customer Code              */~
                        cpart$,          /* Core Part Code             */~
                        openqty,         /* Open Quantity              */~
                        apkey$(),        /* Keys to records for applic.*/~
                        aplines%,        /* Number of entries in above */~
                        rslvqty)         /* Quantity Resolved          */

            if aplines% = 0% then write_crm
                rslvqty = 0
                gosub get_cpart_var_account
                line1$, line2$ = " "
                str(line1$,  1%, 20%) = str(record$(), 10%, 20%)
                str(line1$, 32%,  8%) = str(record$(),200%,  8%)
                str(line1$, 40%,  8%) = str(record$(),192%,  8%)
                str(line1$, 48%,  6%) = str(record$(), 30%,  6%)
                str(line1$, 54%,  6%) = date
                str(line1$, 60%,  6%) = date
                str(line1$, 60%,  6%) = xor hex(ffffffffffff)
                str(line1$, 66%,  9%) = acct$
                str(line1$, 75%, 30%) = "Auto. Application"
                str(line1$,143%,  1%) = "A"
                str(line1$,144%,  4%) = hex(ffffffff)
                str(line1$,155%,  3%) = userid$
                str(line1$,158%,  6%) = date

                str(line2$, 22%, 20%) = str(apkey$(i%),,20%)
                str(line2$, 45%, 25%) = cpart$
                init (hex(00)) str(line2$,114%)

            for i% = 1% to aplines%
                str(line1$, 21%,  3%) = " "
                str(line1$, 24%,  8%) = str(apkey$(i%),27%,8%)
                str(line1$,105%, 19%) = " "
                str(line1$,124%, 19%) = " "

                str(line2$, 22%, 20%) = str(apkey$(i%),,20%)
                str(line2$, 42%,  3%) = " "
                str(line2$, 70%,  6%) = str(record$(), 30%,  6%)
                str(line2$, 90%,  8%) = str(apkey$(i%),27%,8%)
                str(line2$, 98%,  8%) = str(record$(),200%,  8%)
                str(line2$,106%,  8%) = str(record$(),192%,  8%)

            get apkey$(i%) using L15140, qty
L15140:         FMT POS(27), PD(14,4)

            crlines% = crlines% + 1%
            convert crlines% to str(line1$,21,3), pic(###)

            plowkey$ = str(line2$,22,20)
            call "READ101" (#2, plowkey$, f1%)
               if f1% <> 0% then L15250
                  str(line2$,1,1) = "X"
                  goto next_cr_app

L15250:     get #2 using L15300, str(line2$, 70%,  6%),                   ~
                                str(line1$,105%, 19%),                   ~
                                str(line1$,124%, 19%), dbrslv,           ~
                                str(line2$,106%,  8%),                   ~
                                str(line2$, 98%,  8%), dbseq%
L15300:         FMT POS(30), CH(6), POS(126), CH(19), POS(151), CH(19),  ~
                    POS(184), PD(14,4), 2*CH(8), POS(317), BI(2)

            dbrslv = dbrslv + qty    : dbseq% = dbseq% + 1%
            put #2 using L15332, dbrslv
L15332:         FMT POS(184), PD(14,4)
        cr_db_eod
            dbseq% = dbseq% + 1%
            convert dbseq% to str(line2$,42,3), pic(###)
            put #2 using L15360, dbseq%
L15360:         FMT POS(317), BI(2)
            rewrite #2

            write #3 using L15500,  str(line2$,22,23),                    ~
                                   str(line2$,90,24),                    ~
                                   str(line2$,70, 6),                    ~
                                   str(line1$,54,110),                   ~
                                   " ",                                  ~
                                   str(line1$, 1,23),                    ~
                                   cpart$,                               ~
                                   str(line1$,48, 6),                    ~
                                   " ",                                  ~
                                   str(line1$,24,24),                    ~
                                   str(line2$,114,24),                   ~
                                   eod goto L15512
L15500:     FMT CH(23), CH(24), CH(6), CH(110), CH(21), CH(23), CH(25),  ~
                CH(6), CH(14), CH(24), CH(24)
            goto L15530
L15512
*        EOD Disaster
            call "READ101" (#2, plowkey$, f1%)
               if f1% = 0% then next_cr_app
            get #2 using L15360, dbseq%
            goto cr_db_eod

L15530:     gltran1$ = " "
            get #02 using L15660,                                         ~
                   glpart$,                                              ~
                   str(gltran1$,10%,16%),/* Price, Cost                */~
                   str(gltran1$,26%,45%),/* 'Asset' Account            */~
                                         /*  A/R    Account            */~
                                         /*  Sales  Account            */~
                                         /*  COGS   Account            */~
                                         /*  CDL    Account            */~
                   str(gltran1$,80%, 2%),/*  CDL    Flag               */~
                                         /*  Unap.  Flag               */~
                   str(gltran1$,71%, 9%) /*  Unap.  Account            */

L15660:         FMT POS(36), CH(25), POS(192), CH(16),                   ~
                    POS(544), CH(45), CH(2), XX(1), CH(9)

                gltran0$ = gltranx$
                get #03 using L15710, glkey$, gldqty, gldacct$, gldtext$
L15710:             FMT CH(23), PD(14,4), POS(66), CH(9), CH(30)
                gldqty = -gldqty
                put gltran1$ using L15740, gldqty, gldacct$
L15740:             FMT POS(2), PD(14,4), POS(26), CH(9)
                str(gltype$,1,2) = "CB"
                glseq1% = 10*i% + 1%
                gltext$ = "Credit Line Added, Appl."
                if gldtext$ <> " " then gltext$ = gldtext$
                gosub write_line_jnl

L15805:     write #13 using L15810, str(line1$,,163), str(line2$,,137),   ~
                      eod goto L15812
L15810:               FMT CH(163), CH(137)
            goto L15824
L15812
*        Another kind of EOD Disaster
                crlines% = crlines% + 1%
                convert crlines% to str(line1$,21,3), pic(###)
                call "READ101" (#3, str(line2$,22,23), f1%)
                   if f1% = 0% then L15805
                put #3 using L15818, str(line1$, 1,23)
L15818:             FMT POS(185), CH(23)
                rewrite #3
                goto L15805

L15824:         gltran0$ = gltranx$ : gltran1$ = gltrn1$
                get #13 using L15835, glkey$, gldqty, gldacct$, gldtext$, ~
                                     str(gltype$,2)
L15835:             FMT CH(23), PD(14,4), POS(66), CH(9), CH(30),        ~
                        POS(143), CH(1)
*              GLDQTY = -GLDQTY
                put gltran1$ using L15855, gldqty, gldacct$
L15855:             FMT POS(2), PD(14,4), POS(26), CH(9)
                str(gltype$,1,1) = "C"
                glseq1% = 10*i%
                glpart$ = str(record$(),70%,25%)
                gltext$ = "Credit Line Added"
                if gldtext$ <> " " then gltext$ = gldtext$
                gosub write_line_jnl

            rslvqty = rslvqty + qty

        next_cr_app
            next i%

            qtyrslv = qtyrslv + rslvqty
            put str(record$()) using L15915, qtyrslv, crlines%
L15915:         FMT POS(184), PD(14,4), POS(317), BI(2)

        write_crm

               readkey$ = str(record$(),10,20)
               call "READ101" (#12, readkey$, f1%)

            if f1% <> 0% then L16260
               gltext$  = "New Core Credit Record"
               goto L16300

L16260:     gltext$ = "Update Core Credit Record"

*        OK to WRITE or REWRITE

L16300:     put #12 using L16310, str(record$(),,650%)
L16310:            FMT CH(650)
            if f1% = 0% then write #12 else rewrite #12

            glinfo$ = str(record$(),10%,20%)
            str(glinfo$,26%,25%) = str(record$(),70%,25%)
            str(glinfo$,51%,30%) = gltext$

            call "CORJNLSB" ("JW", glseq%, 0%, glseqinc%, 1%,            ~
                             "C ", gltrn1$, gltrn0$, glinfo$, ret%)


               goto exit_program

        get_cr_jnl

*        Set Up GLTRN1$
            gltrn1$ = " "
            get str(record$()) using L16950, glqty, glprice, glcost,      ~
                   str(gltrn1$,26%,45%),/* 'Asset' Account             */~
                                        /*  A/R    Account             */~
                                        /*  Sales  Account             */~
                                        /*  COGS   Account             */~
                                        /*  CDL    Account             */~
                   str(gltrn1$,80%, 2%),/*  CDL    Flag                */~
                                        /*  Unap.  Flag                */~
                   str(gltrn1$,71%, 9%) /*  Unap.  Account             */


L16950:         FMT POS(176), PD(14,4), POS(192), 2*PD(14,4),            ~
                    POS(544), CH(45), CH(2), XX(1), CH(9)

            glqty = -glqty
            put str(gltrn1$, 2%,24%) using L17000, glqty, glprice, glcost
L17000:         FMT 3*PD(14,4)

            readkey$ = str(record$(),10,20)
            call "READ100" (#12, readkey$, f1%)

            init (" ") gltrn0$
            if f1% <> 0% then L17100
               gltrn0$ = gltranx$
               return

L17100:     get #12 using L17220,                                         ~
                   glqty,                /* Original Qty               */~
                   str(gltrn0$,10%,16%), /* Price, Cost                */~
                   str(gltrn0$,26%,45%), /* 'Asset' Account            */~
                                         /*  A/R    Account            */~
                                         /*  Sales  Account            */~
                                         /*  COGS   Account            */~
                                         /*  CDL    Account            */~
                   str(gltrn0$,80%, 2%), /*  CDL    Flag               */~
                                         /*  Unap.  Flag               */~
                   str(gltrn0$,71%, 9%)  /*  Unap.  Account            */

L17220:         FMT POS(176), PD(14,4), POS(192), CH(16),                ~
                    POS(544), CH(45), CH(2), XX(1), CH(9)

            glqty = - glqty
            put str(gltrn0$, 2%, 8%) using L17270, glqty
L17270:         FMT PD(14,4)

            return

        write_line_jnl

            glinfo$ = glkey$
            str(glinfo$,26%,25%) = glpart$
            str(glinfo$,51%,30%) = gltext$

            call "CORJNLSB" ("JW", glseq%, glseq1%, glseqinc%, 1%,       ~
                             gltype$, gltran1$, gltran0$, glinfo$, ret%)

            return

        get_cpart_var_account
            readkey$ = cpart$
            call "REDALT0" (#9, readkey$, 1%, f1%)
               if f1% <> 0% then L19070
            get #9 using L19050, acct$
L19050:         FMT POS(170), CH(9)
            if acct$ <> " " then L19080
L19070:        acct$ = str(swtchs$,10,9)
L19080:     return

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
