        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   AAA   RRRR   IIIII  EEEEE  X   X   AAA   SSSS   BBBB    *~
            *  A   A  R   R    I    E       X X   A   A  S      B   B   *~
            *  AAAAA  RRRR     I    EEEE     X    AAAAA   S     BBBB    *~
            *  A   A  R   R    I    E       X X   A   A     S   B   B   *~
            *  A   A  R   R  IIIII  EEEEE  X   X  A   A  SSSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIEXASB - ADD EXTRA LINES TO INVOICE (CAUTION USES COM)  *~
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
            * 07/26/93 ! ORIGINAL                                 ! KB2 *~
            * 01/05/95 ! Add Precious Metal Surcharges            ! RJH *~
            * 07/10/96 ! Changes for the year 2000.               ! DXL *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            sub "ARIEXASB" (             /* Relies on COM. . . . . .   */~
                 currency$,              /* Currency code& description */~
                 cuscode$,               /* Customer Code              */~
                 invdate$,               /* Invoice Date               */~
                 invtype$,               /* Type of Invoice            */~
                 statutory$,             /* Statutory currency code    */~
                 store$,                 /* Store Code                 */~
                 pm_flags$,              /* PM Surcharge Flags         */~
                 so$,                    /* SO Number                  */~
                                                                         ~
                 convunt,                /* Currency conversion fields */~
                 maxlines%,              /* Total number of lines      */~
                                                                         ~
                            #1,          /* SYSFILE2                   */~
                            #4,          /* HNYMASTR                   */~
                            #5,          /* HNYQUAN                    */~
                            ret%)        /* Lines Added                */

        com                              /* From ARIINPUT              */~
            bolqty(100),                 /* Qty shipped on BOL         */~
            buflot$(100,30)6,            /* Buffer Lots                */~
            bufmark$(100,30)1,           /* Buffer Lot Marker          */~
            bufqty(100,30),              /* Buffer Lot Quantity        */~
            cat$(100)4,                  /* Part Category              */~
            completemsg$(100)30,         /* Flagged Complete in Shippng*/~
            conv(100),                   /* Conversion Pricing->Stockng*/~
            cus_part$(100)25,            /* Customer Part Number       */~
            cus_part_descr$(100)32,      /* Customer Part Number Descr */~
            cus_part_type$(100)1,        /* Customer Part Number Source*/~
            def_percent(100),            /* Default % overship allowed */~
            def_unit(100),               /* Def. units overship allowed*/~
            demtype$(100)1,              /* Demand Type for SA         */~
            descr$(100)32,               /* Part Description           */~
            discs$(100)12,               /* Sales Disc Account- Lines  */~
            e_lines%(100),               /* Extra Lines                */~
            item$(100)3,                 /* P.O. Item Number           */~
            linediscamt(100),            /* Line Item Discount Amt     */~
            linediscpct(100),            /* Line Item Discount %       */~
            lineext(100),                /* Line Item Extensions       */~
            lots$(100,30)6,              /* Lot Numbers                */~
            lotqtys(100,30),             /* Lot Quantities             */~
            lotqtys_orig(100,30),        /* Lot Quantities (Original)  */~
            lsts$(100)1,                 /* Line Status                */~
            mark$(100,30)1,              /* Line Marker                */~
            nonstockmsg$(100)16,         /* Non-Stock Part Flag & Msg  */~
            openqty(100),                /* Open Order Quantity        */~
            order(100),                  /* Original Order Quantity    */~
            part$(100)25,                /* Part Code                  */~
            postlot$(100,30)6,           /* All Ready Posted           */~
            postmark$(100,30)1,          /* All Ready Posted Marker    */~
            postqty(100,30),             /* All Ready Posted Qty.      */~
            price(100),                  /* Unit Price (Pricing UOM)   */~
            pricestk(100),               /* Unit Price (Stockng UOM)   */~
            priceuom$(100)4,             /* Pricing Unit of Measure    */~
            project$(100)8,              /* Project Number             */~
            qtypreinv(100),              /* Total Qty Pre-invoiced     */~
            qtyschld(100),               /* Total Qty Scheduled        */~
            qtyshipped(100),             /* Total Qty Shipped & Invd   */~
            sales$(100)12,               /* Sales Acct- Lines          */~
            seq$(100)3,                  /* Line Item Number           */~
            ship(100),                   /* Quantity Shipped           */~
            sn_index2%(100,30),          /*             LotX Indices   */~
            sn_lineid$(100)4,            /*             Line Seq Nr    */~
            sn_lot1was$(100)6,           /*             Last Values    */~
            soopen(100),                 /* Original Open Qty on SO    */~
            soorder(100),                /* Order Quantity on SO       */~
            soseq$(100)3,                /* SO Seq # X-Ref             */~
            stkuom$(100)4,               /* Stocking Unit of Measure   */~
            taxable$(100)1,              /* Line Taxable? (Y/N)        */~
            textidl$(100)4               /* Text ID- Hdr, Lines        */

        dim                              /* Passed arguments           */~
            currency$4,                  /* Currency code& description */~
            cuscode$9,                   /* Customer Code              */~
            invdate$8,                   /* Invoice Date               */~
            invtype$1,                   /* Type of Invoice            */~
            statutory$4,                 /* Statutory currency code    */~
            store$3,                     /* Store Code                 */~
            pm_flags$3,                  /* Passsed PM Surcharge Flags */~
            so$16                        /* Sales Order Number         */~

        dim                              /* General Purpose Stuff      */~
            askmsg$(3)79,                /* Ask User Messages          */~
            plowkey$100,                 /* Misc Read/Plow Key         */~
            plowkey2$100,                /* Misc Read/Plow Key         */~
            readkey$100                  /* Misc Read/Plow Key         */

        dim                              /* Core Section               */~
            cor_def_prebill$1,           /* Default Prebill            */~
            cor_def_extras$1,            /* Default Extra Lines        */~
            cor_prebill$1,               /* Customer Prebill           */~
            cor_extras$1,                /* Customer Extra Lines       */~
            corepart$25,                 /* Core Part (Pricing)        */~
            corensp$25,                  /* Core Non Stocked Part      */~
            lastcust$9,                  /* Same as last Who?          */~
            lastpart$25                  /* Same as last What?         */

        dim                              /* More for PM Code           */~
            pm_code$10,                  /* Precious Metal             */~
            pm_descr$30,                 /* PM Description             */~
            pm_on$1,                     /* PM Surcharge ON Flag       */~
            pm_so$1,                     /* PM Surcharge SO Flag       */~
            pm_inv$1,                    /* PM Surcharge INV Flag      */~
            rev_date$8                   /* Reverse Date               */

        dim fs%(64), f1%(64)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            ret% = 0%
            ml1% = dim(seq$(),1)
            ml2% = dim(lots$(),2)

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! System Control File                      *~
            * #02 ! CORPARNT ! Core Deposit Tracking Core Parent file.  *~
            * #03 ! COREXREF ! Core Deposit Cross Reference file.       *~
            * #06 ! HNYPMTBL ! Precious Metal Code Table                *~
            * #07 ! HNYPMPRC ! Precious Metal Price Table               *~
            * #08 ! BCKPMSLD ! Precious Metal SO Shadow File            *~
            * #09 ! PMCODES  ! Precious Metal Codes Table               *~
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

            select #06, "HNYPMTBL",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  35,                     ~
                        alt key  1, keypos =   26, keylen =  10, dup

            select #07, "HNYPMPRC",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  25

            select #08, "BCKPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  29,                     ~
                         alternate key 1, keypos = 30, keylen = 25, dup, ~
                                   key 2, keypos = 73, keylen =  9, dup, ~
                                   key 3, keypos = 20, keylen = 10, dup

            select #09, "PMCODES",                                       ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =  10

            if fs1% <> 0% then L09000
               call "OPENCHCK" (#1, fs1%, 0%, 0%, " ")

L09000: REM *************************************************************~
            * Initialization?                                           *~
            *************************************************************

            if fs1% < 0% then exit_program   /* No SYSFILE2 - bye, bye */

            if maxlines% <=   0% then exit_program
*          IF MAXLINES% >= ML1% THEN EXIT_PROGRAM

            total_e_lines% = 0%
            for i% = 1% to maxlines%
                if e_lines%(i%) <= 0% then L09130
                if lsts$(i%)    = "D" then L09130
*       ** Type specific tests here to avoid telling someone needlessly
                if ship(i%)     = 0   then L09130  /* Not for zeroes     */
                   if invtype$ <> "C" then L09117  /* Let Core Bank Have */
                      if ship(i%) > 0 then L09130  /* Credit Lines       */
                          goto  L09120             /* C/M may have all   */
L09117:               if ship(i%) < 0 then L09130  /* ready been issued  */
*       ** Made it, this line is a candidate
L09120:         total_e_lines% = total_e_lines% + e_lines%(i%)
L09130:     next i%
            if total_e_lines% <= 0% then exit_program
*       ** Got a Candidate

            askmsg$(1%) = "Insertion of Implied Lines is about to occur."
            askmsg$(2%) = "Press PF16 to continue insertion."
            askmsg$(3%) = "Press PF1 to bypass at this time."
L09200:     ask% = 2%
            call "ASKUSER" (ask%, "* * * INSERTION ACKNOWLEDGE * * *",   ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if ask% =  1% then exit_program
            if ask% = 16% then L09270
               goto L09200

L09270:     if maxlines% >= ml1% then exit_program

            if maxlines%  > 1%   then L10000
               /* No Need to Push, pull, yank, etc for one line */
               st%  = 1% : sr%  = ml1%
               gosub add_line  /* EXTRA% */
               maxlines% = st% + extra%
               goto exit_program

L10000: REM *************************************************************~
            * First, Make Room to work                                  *~
            *************************************************************

            for i% = maxlines% to 1% step -1%
                st% = ml1% + i% - maxlines% : sr% = i%
                gosub move_line
            next i%

        REM *************************************************************~
            * Now move them back, one at a time, inserting as we go     *~
            *************************************************************

            top% = ml1% + 1% - maxlines%
            st%  = 1%

            for i% = top% to ml1%
                if st% < ml1% then L11075
L11073:            st% = ml1% + 1%
                   goto L11130
L11075:         if st% = i% then L11073
                sr% = i%
                gosub move_line
                gosub add_line  /* EXTRA% */
                st% = st% + extra% + 1%
                if st% > ml1% then L11073
            next i%

L11130:     maxlines% = st% - 1%
            if maxlines% >= ml1% then exit_program

        REM *************************************************************~
            * A Little Clean-up is Required (Darn)                      *~
            *************************************************************

            st% = ml1%

                bolqty         (st%) = 0
                cat$           (st%) = " "
                completemsg$   (st%) = " "
                conv           (st%) = 0
                cus_part$      (st%) = " "
                cus_part_descr$(st%) = " "
                cus_part_type$ (st%) = " "
                def_percent    (st%) = 0
                def_unit       (st%) = 0
                demtype$       (st%) = " "
                descr$         (st%) = " "
                discs$         (st%) = " "
                e_lines%       (st%) = 0%
                item$          (st%) = " "
                linediscamt    (st%) = 0
                linediscpct    (st%) = 0
                lineext        (st%) = 0
                lsts$          (st%) = " "
                nonstockmsg$   (st%) = " "
                openqty        (st%) = 0
                order          (st%) = 0
                part$          (st%) = " "
                price          (st%) = 0
                pricestk       (st%) = 0
                priceuom$      (st%) = " "
                project$       (st%) = " "
                qtypreinv      (st%) = 0
                qtyschld       (st%) = 0
                qtyshipped     (st%) = 0
                sales$         (st%) = " "
                seq$           (st%) = " "
                ship           (st%) = 0
                sn_lineid$     (st%) = " "
                sn_lot1was$    (st%) = hex(000000000000)
                soopen         (st%) = 0
                soorder        (st%) = 0
                soseq$         (st%) = " "
                stkuom$        (st%) = " "
                taxable$       (st%) = " "
                textidl$       (st%) = hex(ffffffffffff)

                for l% = 1% to ml2%

                buflot$     (st%, l%) = " "
                bufmark$    (st%, l%) = " "
                bufqty      (st%, l%) = 0
                lots$       (st%, l%) = " "
                lotqtys     (st%, l%) = 0
                lotqtys_orig(st%, l%) = 0
                mark$       (st%, l%) = " "
                postlot$    (st%, l%) = " "
                postmark$   (st%, l%) = " "
                postqty     (st%, l%) = 0
                sn_index2%  (st%, l%) = 0%

                next l%

            top% = max(top%, maxlines% + 1%)
            if top% >= ml1% then exit_program
            sr% = ml1%
            for i% = top% to ml1% - 1%
                st% = i%
                gosub move_line
            next i%
            goto exit_program

        REM *************************************************************~
            * Actual Line Move                                          *~
            *************************************************************

        move_line
                bolqty         (st%) = bolqty         (sr%)
                cat$           (st%) = cat$           (sr%)
                completemsg$   (st%) = completemsg$   (sr%)
                conv           (st%) = conv           (sr%)
                cus_part$      (st%) = cus_part$      (sr%)
                cus_part_descr$(st%) = cus_part_descr$(sr%)
                cus_part_type$ (st%) = cus_part_type$ (sr%)
                def_percent    (st%) = def_percent    (sr%)
                def_unit       (st%) = def_unit       (sr%)
                demtype$       (st%) = demtype$       (sr%)
                descr$         (st%) = descr$         (sr%)
                discs$         (st%) = discs$         (sr%)
                e_lines%       (st%) = e_lines%       (sr%)
                item$          (st%) = item$          (sr%)
                linediscamt    (st%) = linediscamt    (sr%)
                linediscpct    (st%) = linediscpct    (sr%)
                lineext        (st%) = lineext        (sr%)
                lsts$          (st%) = lsts$          (sr%)
                nonstockmsg$   (st%) = nonstockmsg$   (sr%)
                openqty        (st%) = openqty        (sr%)
                order          (st%) = order          (sr%)
                part$          (st%) = part$          (sr%)
                price          (st%) = price          (sr%)
                pricestk       (st%) = pricestk       (sr%)
                priceuom$      (st%) = priceuom$      (sr%)
                project$       (st%) = project$       (sr%)
                qtypreinv      (st%) = qtypreinv      (sr%)
                qtyschld       (st%) = qtyschld       (sr%)
                qtyshipped     (st%) = qtyshipped     (sr%)
                sales$         (st%) = sales$         (sr%)
                ship           (st%) = ship           (sr%)
                sn_lineid$     (st%) = sn_lineid$     (sr%)
                sn_lot1was$    (st%) = sn_lot1was$    (sr%)
                soopen         (st%) = soopen         (sr%)
                soorder        (st%) = soorder        (sr%)
                soseq$         (st%) = soseq$         (sr%)
                stkuom$        (st%) = stkuom$        (sr%)
                taxable$       (st%) = taxable$       (sr%)
                textidl$       (st%) = textidl$       (sr%)

                for l% = 1% to ml2%

                buflot$     (st%, l%) = buflot$     (sr%, l%)
                bufmark$    (st%, l%) = bufmark$    (sr%, l%)
                bufqty      (st%, l%) = bufqty      (sr%, l%)
                lots$       (st%, l%) = lots$       (sr%, l%)
                lotqtys     (st%, l%) = lotqtys     (sr%, l%)
                lotqtys_orig(st%, l%) = lotqtys_orig(sr%, l%)
                mark$       (st%, l%) = mark$       (sr%, l%)
                postlot$    (st%, l%) = postlot$    (sr%, l%)
                postmark$   (st%, l%) = postmark$   (sr%, l%)
                postqty     (st%, l%) = postqty     (sr%, l%)
                sn_index2%  (st%, l%) = sn_index2%  (sr%, l%)

                next l%

                seq$(st%) = " "
                if seq$(sr%) <> " " then                                 ~
                   convert st% to seq$(st%), pic(###)

            return

        REM *************************************************************~
            * Set Up as much as possible from parent                    *~
            *   defaults, wild guesses . . .                            *~
            * [Customs Need to be in an approprite section, NOT HERE]   *~
            *************************************************************
        set_default_extra

            add% = st% + extra%

                bolqty         (add%) = 0
                cat$           (add%) = " "
                completemsg$   (add%) = " "
                conv           (add%) = 1
                cus_part$      (add%) = " "
                cus_part_descr$(add%) = " "
                cus_part_type$ (add%) = " "
                def_percent    (add%) = 0
                def_unit       (add%) = 0
                demtype$       (add%) = demtype$    (st%)
                descr$         (add%) = " "            /* Concoct below */
                discs$         (add%) = discs$      (st%)
                e_lines%       (add%) = 0%
                item$          (add%) = item$       (st%)
                linediscamt    (add%) = 0
                linediscpct    (add%) = 0
                lineext        (add%) = 0              /* Concoct below */
                lsts$          (add%) = "A"
                nonstockmsg$   (add%) = "(Non-Stock Part)"
                openqty        (add%) = 0
                order          (add%) = 0
                part$          (add%) = " "            /* Concoct below */
                price          (add%) = 0              /* Concoct below */
                pricestk       (add%) = 0              /* Concoct below */
                priceuom$      (add%) = "EACH"
                project$       (add%) = project$    (st%)
                qtypreinv      (add%) = 0
                qtyschld       (add%) = 0
                qtyshipped     (add%) = 0
                sales$         (add%) = " "            /* Concoct below */
                ship           (add%) = ship        (st%)
                sn_lineid$     (add%) = " "
                sn_lot1was$    (add%) = hex(000000000000)
                soopen         (add%) = 0
                soorder        (add%) = 0
                soseq$         (add%) = " "
                stkuom$        (add%) = "EACH"
                taxable$       (add%) = taxable$    (st%)
                textidl$       (add%) = hex(ffffffff)

                for l% = 1% to 30%

                buflot$     (add%, l%) = " "
                bufmark$    (add%, l%) = " "
                bufqty      (add%, l%) = 0
                lots$       (add%, l%) = " "
                lotqtys     (add%, l%) = 0
                lotqtys_orig(add%, l%) = 0
                mark$       (add%, l%) = " "
                postlot$    (add%, l%) = " "
                postmark$   (add%, l%) = " "
                postqty     (add%, l%) = 0
                sn_index2%  (add%, l%) = 0%

                next l%

                lotqtys   (add%, 1%) = ship        (st%)
                convert add% to seq$(add%), pic(###)
            return

        REM *************************************************************~
            * Actual Work Gets Done Here                                *~
            *************************************************************

        add_line
            extra% = 0%
            if e_lines%(st%) <= 0% then return
            if lsts$   (st%) = "D" then return

        REM *************************************************************~
            * Some Generally Good tests, but maybe not generic          *~
            *************************************************************
            if ship(st%) = 0 then return         /* Not for zeroes      */
               if invtype$ <> "C" then L21080     /* Let Others have con-*/
                  if ship(st%) > 0 then return   /* trol of Credit Line */
                     goto  L22000                 /* C/M may have all    */
                  if ship(st%)< 0 then return    /* ready been issued   */
L21080:

L22000: REM *************************************************************~
            * Check for Core Bank Generated Extra Lines                 *~
            *************************************************************

            if core% = -1% then end_core /* Move on to next possibility */
            if core% <> 0% then L22200    /* All Ready Got Defaults      */
               plowkey$ = "SWITCHS.COR"
               call "READ100" (#1, plowkey$, core%)
                  if core% <> 0% then L22110
L22090:              core% = -1%
                     goto end_core       /* Move on to next possibility */
L22110:        get #1 using L22140, cor_def_prebill$, extra_allowed$,     ~
                                   cor_def_extras$

L22140:            FMT POS(117), CH(1), POS(136), CH(1), POS(137), CH(1)
               if extra_allowed$ <> "Y" then L22090
               call "OPENCHCK" (#3, fs%, 0%, 0%, " ")
                  if fs% < 0% then L22090       /* No XREF, why bother */
               call "OPENCHCK" (#2, fs%, 0%, 0%, " ")

L22200:     if lastcust$ = cuscode$ then L22290
               cor_prebill$, cor_extras$ = " " : lastcust$ = cuscode$
               call "READ100" (#2, cuscode$, f1%)
                  if f1% = 0% then L22260
               get #2 using L22250, cor_prebill$, cor_extras$
L22250:            FMT POS(30), CH(1), POS(36), CH(1)
L22260:     if cor_prebill$ = " " then cor_prebill$ = cor_def_prebill$
            if cor_extras$  = " " then cor_extras$  = cor_def_extras$

L22290:     if cor_prebill$ <> "Y" then end_core    /* Moving on . . . */
            if cor_extras$  <> "Y" then end_core    /* Moving on . . . */

            if lastpart$ = part$(st%) then L22410
               lastpart$ = part$(st%)
               plowkey$  = part$(st%)
               call "PLOWALTS" (#3, plowkey$, 0%, 25%, add_core%)
                  if add_core% = 0% then end_core
                     get #3 using L22380, corepart$, corensp$
L22380:                  FMT CH(25), POS(76), CH(25)
                     add_core% = 1%

L22410:     if add_core% = 0% then end_core
            if st% + extra% + add_core% > sr% then end_core
*       *** OK Lets Do It

            extra% = extra% + add_core%
            gosub set_default_extra

*       *** Now for the remaining loose ends

            part$(add%) = corensp$

*        Get some necessary values - Sales Acct, Price, etc.

*        Derive the charge/price for the Core Part Number.
            price(add%) = 0 : temp$ = invdate$
            call "CORPRCSB"  /* Core Pricing Subroutine                */~
                (cuscode$,               /* Customer Code              */~
                part$(st%),              /* Reman Part                 */~
                corepart$,               /* Actual Core Part           */~
                " ",                     /* Actual Core Part Flag      */~
                temp$,                   /* Pricing Date               */~
                ship(add%),              /* Pricing Quantity           */~
                price(add%),             /* Price Returned             */~
                #01,                     /* SYSFILE2                   */~
                #02,                     /* CORPARNT                   */~
                #03,                     /* COREXREF                   */~
                #04)                     /* HNYMASTR                   */

            price(add%)    = max(0, price(add%))
            pricestk(add%) = price(add%)
            lineext(add%) = round(ship(add%) * price(add%), 2)
            gosub set_for_currency

            call "HNYGLGET" (corepart$, store$, " ", sales$(add%),       ~
                5%, #04, #05)
            call "GLFMT" (sales$(add%))

            descr$(add%) = "CORE:" & str(part$(st%))

*                 1         2         3
*        12345678901234567890123456789012
*        CORE:ppppppppppppppppppppppppp
*             1234567890123456789012345

        end_core

*       *****************************************************************

        REM *************************************************************~
            * Check for Precious Metals Generated Extra Lines            ~
            *************************************************************

            if pm%   = -1% then end_pmetal /*Move on to next possibility*/
            if pm%   <> 0% then L24190      /* All Ready Got Defaults    */

            pm_on$  = str(pm_flags$,1%,1%)

               if pm_on$ = "Y" then L24120
L24100:              pm% = -1%
                     goto end_pmetal     /* Move on to next possibility */
L24120:        pm% = 1%
               call "OPENCHCK" (#06, fs%(6%), 0%, 0%, " ")
                  if fs%(4%) < 0% then L24100  /* No PMTBL so why bother */
               call "OPENCHCK" (#07, fs%(7%), 0%, 0%, " ")
               call "OPENCHCK" (#08, fs%(8%), 0%, 0%, " ")
               call "OPENCHCK" (#09, fs%(9%), 0%, 0%, " ")

L24190:     pm_so$  = str(pm_flags$,2%,1%)
            pm_inv$ = str(pm_flags$,3%,1%)
            if pm_so$ <> "Y" and pm_inv$ <> "Y" then end_pmetal

            plowkey$ = str(part$(st%)) & hex(00)
            call "PLOWNEXT" (#06, plowkey$, 25%, f1%(6%))
               if f1%(6%) = 0% then end_pmetal /* No PM Association */

            base_price%, c% = 0%
            if pm_inv$ <> "Y" then goto pm_expand_for_so_only

        REM *************************************************************~
            * Going to Try ..... INV                                    *~
            *************************************************************

            if pm_so$ <> "Y" then L24470   /* Expand Lines */

            /* Get Original SO Base Price */
            readkey$ = all(hex(20))
            str(readkey$,1%,19%) = str(so$) & str(soseq$(st%))
            call "READ100" (#08, readkey$, f1%(8%))
                if f1%(8%) = 0%  then L24470  /* Expand Lines */
            get #08 using L24430, part_price, temp_price
                                             temp_price =  temp_price
L24430:        FMT POS(55), PD(14,7), PD(14,7)
            base_price = part_price : base_price% = 1%
            /* C% = 1% */

L24470:     /* OK, Now We Expand the Lines for Each PMetal Associated */

            plowkey$ = str(part$(st%)) & hex(00)

L24510:     if st% + extra% + 1% > sr% then L24540    /* No Room = miss */
L24520:     call "PLOWNEXT" (#06, plowkey$, 25%, f1%(6%))
               if f1%(6%) <> 0% then L24570
L24540:           if c% = 0% then end_pmetal
                  goto pmetal_done

L24570:     get #06 using L24580, pm_code$, pm_qty, pm_factor
L24580:         FMT POS(26), CH(10), PD(14,7), PD(14,7)

            rev_date$ = invdate$ xor hex(ffffffff)
            readkey$ = str(cuscode$) & str(pm_code$) & rev_date$
            call "READ104" ( #07, readkey$, f1%(7%))
            if f1%(7%) = 0% then default
            if str(key(#07),1%,19%) > str(readkey$,1%,19%) then default
            goto L24750                         /* found it */


default:    /* Try for default */
                str(readkey$,1%,9%) = " "
                call "READ104" ( #07, readkey$, f1%(7%))
                if f1%(7%) = 0% then L24520     /* To much wrong */
                if str(key(#07),1%,19%) > str(readkey$,1%,19%) then L24520


L24750:     get #7,using L24760, pm_price
L24760:         FMT POS(32), PD(14,7)

            if pm_factor = 0 then L24520           /* Shouldn't happen */
            temp_price =  round(ship(st%) * pm_qty                       ~
                                          * pm_price / pm_factor, 2)
            if temp_price <= 0 then L24520

*       *** OK,  Really going to do it
            extra% = extra% + 1%
            c% = c% + 1%
            gosub set_default_extra

            pm_descr$ = " "
            call "DESCRIBE" (#09, pm_code$, pm_descr$, 0%, f1%(9%))
            lineext (add%) = temp_price
            price   (add%) = pm_qty * pm_price / pm_factor
            descr$  (add%) = "Surcharge: " & pm_descr$
            part$   (add%) = pm_code$
            sales$  (add%) = sales$(st%)
            pricestk(add%) = price(add%)                  /* ** TEMP * */
            gosub set_for_currency
            goto L24510   /* Back for more */

            /* End of this Looping */


        REM *************************************************************~
            * Going to Try ..... SO                                     *~
            *************************************************************

        pm_expand_for_so_only

            plowkey2$ = all(hex(00))
            str(plowkey2$,1%,19%) = str(so$) & str(soseq$(st%))

L25150:     if st% + extra% + 1% > sr% then L25180
L25160:     call "PLOWNEXT" (#08, plowkey2$, 19%, f1%(8%))
                if f1%(8%) <> 0%  then L25210
L25180:            if c% > 0% then pmetal_done
                      goto end_pmetal

L25210:     get #08 using L25220, pm_code$, pm_price, pm_factor
L25220:          FMT POS(20), CH(10), POS(55), PD(14,7), PD(14,7)
            if pm_code$ <> " " then L25300
                /* 1st record has different layout*/
                base_price = pm_price : base_price% = 1%
                /* C% =  1% */
                goto L25160

            /* IF ST% + EXTRA% + 1% > SR% THEN 25170 here to get B_P */
L25300:     if pm_factor = 0 then L25160          /* Shouldn't happen */

            readkey$ = str(part$(st%)) & pm_code$
            call "READ100" (#06, readkey$, f1%(6%))
                if f1%(6%) = 0% then  L25160       /* Shouldn't happen */
            get #6 using L25360, pm_qty
L25360:         FMT POS(36), PD(14,7)

            temp_price = round(ship(st%) * pm_qty                        ~
                                         * pm_price / pm_factor, 2)
            if temp_price <= 0 then L25160

            c% = c% + 1%
            extra% = extra% + 1%
            gosub set_default_extra

            pm_descr$ = " "
            call "DESCRIBE" (#09, pm_code$, pm_descr$, 0%,f1%(9%))
            lineext (add%) = temp_price
            price   (add%) = pm_qty * pm_price / pm_factor
            descr$  (add%) = "Surcharge: " & pm_descr$
            part$   (add%) = pm_code$
            sales$  (add%) = sales$(st%)
            pricestk(add%) = price(add%)                  /* ** TEMP * */
            gosub set_for_currency
            goto L25150       /* Back for more */

            /* End of this Looping */


        REM *************************************************************~
            * Common Ends for PM Routines                               *~
            *************************************************************

        pmetal_done
            if base_price% = 0% then end_pmetal
            if abs(price(st%) - base_price) < 0.0001  then end_pmetal

               price(st%) = base_price
               pricestk(st%)   = round(price(st%) / conv(st%)         , 4)
               tempext         = round(price(st%)*ship(st%) / conv(st%),2)
               linediscamt(st%) = round(tempext* linediscpct(st%) * .01,2)
               linediscamt(st%) = -linediscamt(st%)
               lineext(st%)     = round(tempext + linediscamt(st%)    , 2)

        end_pmetal

*       *****************************************************************

*       *** Reserved for the next thing that comes along
*       *** But Until Then, Bye. . .

        REM *************************************************************~
            * Common Ending for All Routines                            *~
            * Fix Up E_LINES%, RET% to reflect the fact that something  *~
            * Really was added & Don't try it again, ever. . .          *~
            *************************************************************

            ret% = ret% + extra%
            e_lines%(st%) = -extra%
            return   /* Back to Move and Insert Loop in the 11000's */


        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        set_for_currency
            if currency$ = statutory$ then return
               pricestk(add%)  = round(pricestk(add%)  * convunt, 4)
               price   (add%)  = round(price   (add%)  * convunt, 4)
               lineext (add%)  = round(lineext (add%)  * convunt, 4)
            return

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
