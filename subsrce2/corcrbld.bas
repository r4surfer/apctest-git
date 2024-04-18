        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR    CCC   RRRR   BBBB   L      DDDD    *~
            *  C   C  O   O  R   R  C   C  R   R  B   B  L      D   D   *~
            *  C      O   O  RRRR   C      RRRR   BBBB   L      D   D   *~
            *  C   C  O   O  R   R  C   C  R   R  B   B  L      D   D   *~
            *   CCC    OOO   R   R   CCC   R   R  BBBB   LLLLL  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORCRBLD - Builds Core Credit Record, after passing       *~
            *            ARILINES, Receipt Date                         *~
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
            * 07/16/92 ! Original                                 ! KB2 *~
            * 07/15/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "CORCRBLD" (record$(), ret%)

        REM Passed in, RECORD$() contains (essentially) an ARILINES      ~
            record in bytes 1 - 750 (simplicity).  Critcal fields mapped ~
            below.  Appended to that will be (optionally) certain data   ~
            normally available from ???MASTR.  The only critcal field,   ~
            receipt date, will be defaulted to today if passed blank.    ~
            In addition, if a specific core part # is passed,            ~
            and dependent on some flags, a record will be constructed    ~
            even though the remanufactured part has no usual core part,  ~
            or is normally linked to something else.  One more addition. ~
            If requested, system defaults for G/L accounts, cutoff days, ~
            etc. can be returned even if no core part is indicated.

        REM Record String Layout                                         ~
            Field         Pos/Len                                        ~
            CUSCODE         1/9          /* Customer Code              */~
            RCPTDOC        10/8          /* Receipt Number             */~
            RECPT   LINE   18/3          /* Receipt Line               */~
            REMAN. PART    24/25         /* Remanufactured Part Code   */~
            QUANTITY       93/8 PD(14,4) /* Quantity ('nuf said)       */~
                                         /* Now comes ARIMASTR Stuff   */~
            RECEIPT DATE  751/32         /* Filler                     */~
            RECEIPT DATE  783/6          /* Receipt Date               */~
                                         /* Additional Mystery Stuff   */~
            ALT CORE PART 901/25         /* Alternate Core Part        */~
            ALT CORE FLAG 926/1          /* Non Blank - Use alternate  */~
                                         /*             regardless of  */~
                                         /*             Reman indicates*/~
                                         /*     Blank - Use only if    */~
                                         /*             Reman has no   */~
                                         /*             Cross Reference*/~
            DEFAULTS FLAG 927/1          /* Non Blank - Return Default */~
                                         /*             G/L's Etc even */~
                                         /*             if all of the  */~
                                         /*             above fail.    */~
           ALT NS PART    928/25         /* Alternate NS Part          */~
           INVENTORY PART 953/25         /* Actual Received Part       */~
           INVENTORY QTY  978/8          /* Disregarded entirely if    */~
                                         /* inventory part is blank or */~
                                         /* the same as core part.     */~
                                         /* Otherwise may be passed as */~
                                         /* blanks and disregarded or  */~
                                         /* could contain quantity     */~
                                         /* posted to inventory (may be*/~
                                         /* diffent from core qty) to  */~
                                         /* place in record (memo) and */~
                                         /* use in pricing (also memo) */

        REM Return (with a little luck) RECORD$() position 1 - 650 is a  ~
            CORCRMAS record, or at least contains default values. With   ~
            any form of success positions 651 - 800 will contain         ~
            SWITCHS.COR positions 21 - 170. [Possibly Saving a Read].    ~
            Most immediately useful, pos 661/9 will be core Variance     ~
            Account, which is needed for auto-application.               ~
            Repeated at 801/9 is the core variance account derived via   ~
            the default hierarchy, for consistency.                      ~
            Otherwise it should be unchanged.                            ~
                                                                         ~
            Return Code  99 - Core Switches not found, Core bank not     ~
                              installed.                                 ~
                         98 - Reman part not found in core bank, no      ~
                              alternate specified. No Defaults Requested.~
                         97 - Alternate Specified, but not found.  No    ~
                              Defaults Requested.                        ~
                          1 - Only Defaults Returned.                    ~
                          0 - 'Complete' success.  May have been forced  ~
                              or requested to use alternate.

        dim /* General Purpose / Misc. Variables                       */~
            acpart$25,                   /* Alternate Core Part        */~
            acflag$1,                    /* Alternate Core Flag        */~
            anpart$25,                   /* Alternate N/S  Part        */~
            ahpart$25, ahqty$8,          /* Actual Recieved Part       */~
            blankdate$8,                 /* Blank date for comparison  */~
            dflag$1,                     /* Defaults Flag              */~
            pcmonapp$1,                  /* Credit Memo on Application */~
            rcpc$1, pcpc$1,              /* Price Codes   (Pricing)    */~
            cat$4,                       /* Category Code (Pricing)    */~
            custype$2,                   /* Customer Type (Pricing)    */~
            plowkey$99, readkey$99,      /* Miscellaneous Read/Plow Key*/~
            punapacct$, ppostunap$1,     /* Override Uanapplied Cores  */~
            swtchs$200,                  /* Core Switches              */~
            record$(4)256,               /* ARILINES in/CORDRMAS OUT   */~
            tdate$8,                     /* Temp.  formatted date      */~
            temp$8,                      /* Temp date                  */~
            udate$8,                     /* Temp unformatted date      */~
            userid$3                     /* Current User Id            */~

        dim /* CORCRMAS variables                                      */~
            pcuscode$9,                  /* Core Parent Code           */~
            cuscode$9,                   /* Ship-To Customer Code      */~
            rcptdoc$8,                   /* Receipt Number             */~
            rcpline$3,                   /* Receipt Line Item Number   */~
            rcpdate$6,                   /* Receipt Date               */~
            cpart$25,                    /* Core Part #                */~
            rcprdate$6,                  /* Rev Receipt Date for Aging */~
            npart$25,                    /* Core Credit NSP Part #     */~
            spart$25,                    /* Part Actually Received     */~
            dcost$96, cost(12),          /* 12*PD(14,4)- Cost per Unit */~
            cdate$6,                     /* Expiration Date            */~
            rpart$25,                    /* Reman Part Code            */~
            aracct$9,                    /* Interim A/R    Acct        */~
            varacct$9,                   /* Core Variance  Acct        */~
            slacct$9,                    /* Interim Sales  Acct        */~
            icgacct$9,                   /* Interim COGS   Acct        */~
            cdlacct$9,                   /* Core Dep Liab. Acct        */~
            cdlflag$1,                   /* Core Dep Liab. Flag        */~
            unapacct$9,                  /* Unapplied Cores Account    */~
            postunap$1,                  /* Post Unapplied Cores Acct  */~
            cmonapp$1                    /* Credit Memo on Application */

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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! CORPARNT ! Core Parent Cross Reference File         *~
            * #03 ! COREXREF ! Core Part Cross Reference File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "CORPARNT",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   10, keylen =   9,                     ~
                        alt key  1, keypos =    1, keylen =  18          ~

            select #03, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  2, keypos =   76, keylen =  25, dup,    ~
                            key  1, keypos =    1, keylen =  50          ~

            select #05, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25                      ~

            if userid$ <> " " then L09500

            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#02, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#03, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#05, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)

            readkey$ = "SWITCHS.COR"
            call "READ100" (#1, readkey$, swtchs%)
               if swtchs% <> 0% then get #1 using L09130, swtchs$
               if swtchs%  = 0% then swtchs% = 99%
L09130:           FMT CH(200)

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
                CH(9)   /*  119/9        /* Core WIP Acount            */

L09500:     ret% = 99%
            if swtchs% = 99% then exit_program        /* NOT INSTALLED */
            ret% = 98%       /* SET UP FOR NON-CORE ITEM */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            get str(record$()) using L10100, cuscode$, rcptdoc$, rcpline$,~
                                            rpart$, qtyorig, rcpdate$,   ~
                                            acpart$, acflag$, dflag$,    ~
                                            anpart$, ahpart$, ahqty$
L10100:     FMT CH(9), CH(8), CH(3), XX(3), CH(25), POS(93), PD(14,4),   ~
                POS(783), CH(6),                                         ~
                POS(901), CH(25), CH(1), CH(1), CH(25), CH(25), CH(8)

            if rcpdate$ = " " or rcpdate$ = blankdate$ then rcpdate$ = date

            if rpart$ = " " then L10182
            plowkey$ = rpart$
            call "PLOWALTS" (#3, plowkey$, 0%, 25%, f1%)
               if f1% <> 0% then L10185         /* Got One              */
L10182:           if acpart$ = " " then load_defaults  /* Can't Help   */
                  goto L10220
L10185:     ret% = 0%                          /* Success Guaranteed   */
            if acpart$ = " " then L10450        /* Nothing Requested    */
            if str(plowkey$,26,25) = acpart$ then L10450 /* Same Thing  */
            if acflag$ = " " then L10450        /* Nothing Requested    */
L10220:        plowkey$ = acpart$
               call "REDALT0" (#3, plowkey$, 1%, f1%)
                  if f1% = 0% then L10246       /* Missed!              */
                     ret% = 0
                     goto L10450
L10246:     ret% = 97%

        load_defaults
            if dflag$ = " " then exit_program
            ret% = 1%
            cpart$, npart$, rcpc$ = " "
            if acpart$ <> " " then cpart$ = acpart$
            if anpart$ <> " " then npart$ = anpart$
            cost, price = 0 : init (hex(00)) dcost$
            get str(swtchs$) using L10410, varacct$, unapacct$, aracct$,  ~
                                          icgacct$, slacct$, cdlacct$,   ~
                                          cdlflag$, cdays%, rcpc$,       ~
                                          postunap$, cmonapp$
L10410:         FMT POS(21), 2*CH(9), POS(48), 4*CH(9), POS(102), CH(1), ~
                    POS(104), BI(2), POS(116), 3*CH(1)
            goto default_resume

L10450:     get #3 using L10470, cpart$, npart$, aracct$, cdlacct$,       ~
                   slacct$, icgacct$, price, rcpc$, cdays%, varacct$
L10470:         FMT POS(51), 2*CH(25), POS(101), 4*CH(9), XX(18),        ~
                    PD(14,4), CH(1), BI(2), POS(170), CH(9)

            /* Got first level of defaults, will build it now!! */

            plowkey$ = cpart$
            call "REDALT0" (#3, plowkey$, 1%, cdflt%)

            /* A/R Account */
            if aracct$ <> " " then L10630
               if cdflt% <> 0% then get #3 using L10580, aracct$
L10580:           FMT POS(101), CH(9)
            if aracct$ <> " " then L10630
               get str(swtchs$) using L10610, aracct$
L10610:           FMT POS(48), CH(9)

L10630:     /* Core Deposit Liability Account */
            if cdlacct$ <> " " then L10710
               if cdflt% <> 0% then get #3 using L10660, cdlacct$
L10660:           FMT POS(110), CH(9)
            if cdlacct$ <> " " then L10710
               get str(swtchs$) using L10690, cdlacct$
L10690:           FMT POS(75), CH(9)

L10710:     /* Sales Account */
            if slacct$ <> " " then L10790
               if cdflt% <> 0% then get #3 using L10740, slacct$
L10740:           FMT POS(119), CH(9)
            if slacct$ <> " " then L10790
               get str(swtchs$) using L10770, slacct$
L10770:           FMT POS(66), CH(9)

L10790:     /* Cost of Goods Sold Account */
            if icgacct$ <> " " then L10870
               if cdflt% <> 0% then get #3 using L10820, icgacct$
L10820:           FMT POS(128), CH(9)
            if icgacct$ <> " " then L10870
               get str(swtchs$) using L10850, icgacct$
L10850:           FMT POS(57), CH(9)

L10870:     /* Core Variance Account */
            if varacct$ <> " " then L10942
               if cdflt% <> 0% then get #3 using L10900, varacct$
L10900:           FMT POS(170), CH(9)
            if varacct$ <> " " then L10942
               get str(swtchs$) using L10930, varacct$
L10930:           FMT POS(30), CH(9)

L10942:     /* Unapplied Cores Account     */
            get str(swtchs$) using L10946, unapacct$
L10946:           FMT POS(21), CH(9)

            /* End of Account Chain        */

            if cdays% <> 0% then pricing_stuff
               if cdflt% <> 0% then get #3 using L10970, cdays%
L10970:           FMT POS(164), BI(2)
            if cdays% <> 0% then pricing_stuff
               get str(swtchs$) using L10985, cdays%
L10985:           FMT POS(104), BI(2)
            /* Best we can do here, but wait for Parent */

        pricing_stuff
            if price  >  0  then other_flags     /* Got 'Fixed' Price */
            if rcpc$ <> " " then other_flags     /* Try with this     */
               if cdflt% <> 0% then get #3 using L11040, price, rcpc$
L11040:           FMT POS(155), PD(14,4), CH(1)
            if price  >  0  then other_flags     /* Got 'Fixed' Price */
            if rcpc$ <> " " then other_flags     /* Try with this     */
               get str(swtchs$) using L11090, rcpc$
L11090:           FMT POS(116), CH(1)

        other_flags
            get str(swtchs$) using L11130, cdlflag$, postunap$, cmonapp$
L11130:           FMT POS(102), CH(1), POS(117), 2*CH(1)

        default_resume

            if cpart$ <> " " then L11520
               cost = 0 : init (hex(00)) dcost$ : goto L11600
L11520:     call "STCCOSTS" (cpart$, "        ", #1, 2%, cost, cost())
            call "PACKZERO" (cost(), dcost$)

L11600:     pcuscode$ = cuscode$
            punapacct$, ppostunap$, pcmonapp$, pcpc$, custype$ = " "
            call "READ100" (#2, cuscode$, f1%)
               if f1% = 0% then L11670
            get #2 using L11640, pcuscode$, cdays1%, punapacct$,          ~
                                ppostunap$, pcmonapp$, pcpc$, custype$
L11640:         FMT CH(9), XX(9), BI(2), CH(9), 3*CH(1), CH(2)
            if cdays1% <> 0% then cdays% = cdays1%
            if punapacct$ <> " " then unapacct$ = punapacct$
            if ppostunap$ <> " " then postunap$ = ppostunap$
            if pcmonapp$  <> " " then cmonapp$  = pcmonapp$

L11670:     temp$ = rcpdate$ : cdate$ = " "
            cdays% = 999%    /* Credits Dont Drop Off */
            if cdays% = 999% then L11805

            call "DATE" addr("G+", temp$, cdays%, cdate$, r%)
              if r% = 8% then L11805
            tdate$ = cdate$
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$,5%,2%) to mm%
            mm% = mm% + 1%
            if mm% < 13% then L11760
            convert str(udate$,1%,4%) to yy%
            yy% = yy% + 1% : mm% = 1%
            convert yy% to str(udate$,1%,4%), pic(0000)
L11760:     convert mm% to str(udate$,5%,2%), pic(00)
            str(udate$,7%,2%) = "01"
            call "DATECONV" (udate$)
            cdate$ = udate$
            temp$ = cdate$
            call "DATE" addr("G+", temp$, -1%, cdate$, r%)

L11805:     rcprdate$ = rcpdate$ xor hex(ffffffffffff)

*        FINAL_PRICING
            if price > 0 then sub_part_stuff    /* Got a 'Fixed' Price */
            if pcpc$ <> " " then rcpc$ = pcpc$
            if rcpc$ = " " then sub_part_stuff  /* All we can do !!    */
            cat$ = " ": conv = 1
            call "READ100" (#5, cpart$, f1%)
               if f1% <> 0% then get #5 using L11850, conv, cat$
L11850:           FMT POS(82), PD(14,7), CH(4)
            call "CPRASSGN" (cuscode$, custype$, cpart$, cat$, rcpc$,    ~
                             rcpdate$, " ", "    ", -1, qtyorig, #1, #5, ~
                             price, disc, err$)
            if price <= 0  then L11880
            if err$  = " " then L11895
L11880:        price = 0
               goto sub_part_stuff

L11895:     if disc >  0 then price = (price * (100 - disc)) * .01
            if conv <> 0 then price =  price / conv
               price = round(price, 4)

        sub_part_stuff
            spart$   = cpart$   /* Load up for no substitution . . .    */
            sqtyorig = qtyorig  /* Assumes one for one, but then . . .  */
            sprice   = price    /* Probably not the best, but memo only */
            scost    = cost     /* Recalced if actual substitution      */

            if ahpart$ <> " " then spart$ = ahpart$
            if spart$ = cpart$ then L12000
               call "STCCOSTS" (spart$, "        ", #1, 1%, scost)

               if ahqty$ <> " " then get ahqty$ using L11930, sqtyorig
L11930:           FMT PD(14,4)
*        Reprice for sub part sold as core
            sprice = 0 : rcpc$ = " "
            if rpart$ = " " then L11952
               readkey$ = str(rpart$,,25%) & spart$
               call "REDALT0" (#3, readkey$, 0%, f1%)
                  if f1% = 0% then L11952
               get #3 using L11944, sprice, rcpc$
L11944:            FMT POS(155), PD(14,4), CH(1)
               if sprice > 0 then L12000
               if rcpc$ <> " " then L11966

L11952:        readkey$ = spart$
               call "REDALT0" (#3, readkey$, 1%, f1%)
                  if f1% = 0% then L11966
               get #3 using L11960, sprice, rcpc$
L11960:            FMT POS(155), PD(14,4), CH(1)
               if sprice > 0 then L12000

L11966:     if pcpc$ <> " " then rcpc$ = pcpc$
            if rcpc$ = " " then L12000           /* All we can do !! */
            cat$ = " ": conv = 1
            call "READ100" (#5, spart$, f1%)
               if f1% <> 0% then get #5 using L11976, conv, cat$
L11976:           FMT POS(82), PD(14,7), CH(4)
            call "CPRASSGN" (cuscode$, custype$, spart$, cat$, rcpc$,    ~
                             rcpdate$, " ", "    ", -1, sqtyorig, #1, #5,~
                             sprice, disc, err$)
            if sprice <= 0  then L11988
            if err$  = " " then L11994
L11988:        sprice = 0
               goto L12000

L11994:     if disc >  0 then sprice = (sprice * (100 - disc)) * .01
            if conv <> 0 then sprice =  sprice / conv
               sprice = round(sprice, 4)

L12000:     init (" ") record$()

            put str(record$()) using L12500, /* CORDRMAS                */~
                pcuscode$,               /* Core Parent Code           */~
                cuscode$,                /* Ship-To Customer Code      */~
                rcptdoc$,                /* Receipt Number             */~
                rcpline$,                /* Receipt Line Item Number   */~
                rcpdate$,                /* Receipt Date               */~
                cpart$,                  /* Core Part #                */~
                cuscode$,                /* Ship-To Customer Code      */~
                cpart$,                  /* Core Part #                */~
                rcprdate$,               /* Rev Receipt Date for Aging */~
                npart$,                  /* Core Credit NSP Part #     */~
                spart$,                  /* Core Credit NSP Part #     */~
                sqtyorig,                /* Original Quantity          */~
                sprice,                  /* Price per Unit             */~
                scost,                   /* Total Cost per unit        */~
                " ",                     /* Filler                     */~
                qtyorig,                 /* Original Quantity          */~
                0,                       /* Received to Date           */~
                price,                   /* Price per Unit             */~
                cost,                    /* Total Cost per unit        */~
                dcost$,                  /* 12*PD(14,4)- Cost per Unit */~
                " ",                     /* Active or Hold Flag        */~
                cdays%,                  /* Expiration Number of Days  */~
                cdate$,                  /* Expiration Date            */~
                hex(ffffffff),           /* Text file pointer          */~
                0%,                      /* # Line Items/this Master   */~
                " ",                     /* Filler/Reserved for VarFlds*/~
                rpart$,    /*  1/25      /* Reman Part Code            */~
                varacct$,  /* 26/9       /* Core Variance  Acct        */~
                aracct$,   /* 35/9       /* Interim A/R    Acct        */~
                slacct$,   /* 44/9       /* Interim Sales  Acct        */~
                icgacct$,  /* 53/9       /* Interim COGS   Acct        */~
                cdlacct$,  /* 62/9       /* Core Dep Liab. Acct        */~
                cdlflag$,  /* 71/1       /* Core Dep Liab. Flag        */~
                postunap$, /* 72/1       /* Post Unapplied Cores Acct  */~
                cmonapp$,  /* 73/1       /* Credit Memo On Application */~
                unapacct$, /* 74/9       /* Unapplied Cores Account    */~
                cuscode$,  /* 83/9       /* Ship-To Customer Code      */~
                cpart$,    /* 92/25      /* Core Part #                */~
                rcpdate$,  /* 117/6      /* Receipt Date for Applicatn */~
                " ",       /* 123/1      /* Hold Flag, Cost / Price    */~
                userid$,                 /* Last Modified User         */~
                date,                    /* Last Modified Date         */~
                str(swtchs$,21%,150%),   /* Core Switches              */~
                varacct$                 /* Core Variance Account      */

L12500:     FMT /* File #01- CORCRMAS (Core Bank Credit Master) layout */~
                CH(09),    /*   1/9      /* Core Parent Code           */~
                CH(09),    /*  10/9      /* Ship-To Customer Code      */~
                CH(08),    /*  19/8      /* Return Doc Id/Cr Memo #    */~
                CH(03),    /*  27/3      /* blank or CR Memo Line #    */~
                CH(06),    /*  30/6      /* Receipt Date               */~
                CH(25),    /*  36/25     /* Core Part #                */~
                CH(09),    /*  61/9      /* Ship-To Customer Code      */~
                CH(25),    /*  70/25     /* Core Part #                */~
                CH(06),    /*  95/6      /* Rev Receipt Date for Aging */~
                CH(25),    /* 101/25     /* Core Credit NSP Part #     */~
                CH(25),    /* 126/25     /* Part Actually Recieved     */~
                PD(14,4),  /* 151/8      /* Original Quantity (actual) */~
                PD(14,4),  /* 159/8      /* Price per Unit (actual)    */~
                PD(14,4),  /* 167/8      /* Total Cost per unit (act)  */~
                CH(1),     /* 175/1      /* Filler                     */~
                PD(14,4),  /* 176/8      /* Original Quantity          */~
                PD(14,4),  /* 184/8      /* Applied to Date            */~
                PD(14,4),  /* 192/8      /* Price per Unit             */~
                PD(14,4),  /* 200/8      /* Total Cost per unit        */~
                CH(96),    /* 208/96     /* 12*PD(14,4)- Cost per Unit */~
                CH(01),    /* 304/1      /* Active or Hold Flag        */~
                BI(02),    /* 305/2      /* Expiration Number of Days  */~
                CH(06),    /* 307/6      /* Expiration Date            */~
                CH(04),    /* 313/4      /* Text file pointer          */~
                BI(02),    /* 317/2      /* # Line Items/this Master   */~
                CH(200),   /* 319/200    /* Filler/Reserved for VarFlds*/~
                CH(25),    /* 519/25     /* Reman Part Code            */~
                CH(9),     /* 544/9      /* Core HNY Asset Acct        */~
                CH(9),     /* 553/9      /* Interim A/R    Acct        */~
                CH(9),     /* 562/9      /* Interim Sales  Acct        */~
                CH(9),     /* 571/9      /* Interim COGS   Acct        */~
                CH(9),     /* 580/9      /* Core Dep Liab. Acct        */~
                CH(1),     /* 589/1      /* Core Dep Liab. Flag        */~
                CH(1),     /* 590/1      /* Post Unapplied Cores Acct  */~
                CH(1),     /* 591/1      /* Credit Memo On Application */~
                CH(9),     /* 592/9      /* Unapplied Cores Account    */~
                CH(9),     /* 601/9      /* Ship-To Customer Code      */~
                CH(25),    /* 610/25     /* Core Part #                */~
                CH(6),     /* 635/6      /* Receipt Date for Applicatn */~
                CH(1),     /* 641/1      /* Hold Flag, Cost / Price    */~
                CH(03),    /* 642/3      /* Last Modified User         */~
                CH(06),    /* 645/6      /* Last Modified Date         */~
                CH(150),   /* 651/150    /* Switches Info              */~
                CH(9)      /* 801/9      /* Core variance Account      */~

        REM FMT                    /* File #01- COREXREF record layout */~
                CH(25),   /*   1/25      /* Core Part #   AK1          */~
                CH(25),   /*  26/25      /* Reman Part #   "   PK      */~
                CH(25),   /*  51/25      /* Core Part # again   "      */~
                CH(25),   /*  76/25      /* Core Cr NSP # AK2          */~
                CH(09),   /* 101/9       /* Core Bank - Interim A/R    */~
                CH(09),   /* 110/9       /* Core Deposit Liability     */~
                CH(09),   /* 119/9       /* Interim Sales              */~
                CH(09),   /* 128/9       /* Interim COGS               */~
                CH(09),   /* 137/9       /* Core F/G Inventory         */~
                CH(09),   /* 146/9       /* Core WIP                   */~
                PD(14,4), /* 155/8       /* Core Charge/Price          */~
                CH(01),   /* 163/1       /* Price code                 */~
                BI(02),   /* 164/2       /* Default Drop-Off Days      */~
                CH(04),   /* 166/4       /* Text file pointer          */~
                CH(09),   /* 170/9       /* Core Variance Account      */~
                CH(113),  /* 179/113     /* Filler                     */~
                CH(200),  /* 292/200     /* Filler/Reserved for VarFlds*/~
                CH(03),   /* 492/3       /* Last Modified User         */~
                CH(06)    /* 495/6       /* Last Modified Date         */

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
