        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   M   M   CCC    AAA   L       SSS    U   U  BBBB   *~
            *  P   P  MM MM  C   C  A   A  L      S    S  U   U  B   B  *~
            *  PPPP   M M M  C      AAAAA  L        S     U   U  BBBB   *~
            *  P      M   M  C   C  A   A  L           S  U   U  B   B  *~
            *  P      M   M   CCC   A   A  LLLLL   SSSS    UUU   BBBB   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PMCALSUB - This program calculates the precious metal     *~
            *            surcharge for a CMS Part                       *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/13/94 ! Original                                 ! RJH *~
            * 05/26/97 ! PRR 13721-add alt key #3 to both BCKPMSLD! RJH *~
            *          !   and MLQPMSLD files.                    !     *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            * 02/05/98 ! Bug Fix to Y2K Reverse Date Calc.        ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "PMCALSUB"(cuscode_in$,   /* Customer Code                 */~
                       part_in$,      /* Part Number                   */~
                       qty,           /* Part Quantity                 */~
                       price,         /* Part Price                    */~
                       conv,          /* Stocking to Pricing Conversion*/~
                       mode$,         /* Mode - 'C'alculate, 'D'isplay,*/~
                                      /*     'P'rint, 'S'ave, 'R'emove */~
                       calc_date$,    /* What date to calculate effectv*/~
                                      /*   PM Prices for?              */~
                       s_charge,      /* Total PM Surcharge            */~
                       o_type$,       /* Order Type - 'M'LQ, 'S'O      */~
                       o_no$,         /* Order Number                  */~
                       pm_so$,        /* PM SO Flag                    */~
                       pm_inv$,       /* PM INV Flag                   */~
                       result%)       /*  1% = All is well             */~
                                      /*  0% = No PM associated w/Part */~
                                      /* -1% = At least one PM had no  */~
                                      /*         Price                 */~

        dim                                                              ~
            calc_date$8,                 /* Date to base calcs on      */~
            calc_date2$8,                /* Date to base calcs on      */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            cus_temp$9, cus_temp2$9,     /* Customer Code              */~
            date$8,                      /* Date for screen display    */~
            disp_line_hdr$79,            /* Display Lines Header       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Screen Line #2             */~
            mode$1,                      /* Mode (C, D, P)             */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Number Description    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            pm_code$(100)10,             /* Precious Metal             */~
            pm_date$(100)8,              /* Precious Metal Effecive Dte*/~
            pm_descr$(100)30,            /* PM Description             */~
            pm_ext$(100)10, pm_ext(100), /* Line Amount                */~
            pm_factor$(100)10,pm_factor(100), /* Factor                */~
            pm_price$(100)10,pm_price(100),   /* Price                 */~
            pm_qty$(100)10,pm_qty(100),  /* PM Quantity                */~
            pm_temp$10,                  /* PM Temp Item code          */~
            pm_uom$(100)4,               /* Unit of Measure            */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpttitle$60,                 /* Report Title               */~
            qty$10,                      /* Quantity                   */~
            rev_date$6,                  /* Reverse date (999999 - d)  */~
            s_charge$10,                 /* Surcharge                  */~
            time$10,                     /* TIME                       */~
            userid$3,                    /* Current User Id            */~
            warnmsg$79                   /* Display Warning (info) Msg */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYPMTBL ! Precious metal code table                *~
            * #02 ! HNYPMPRC ! Precious Metal price table               *~
            * #03 ! HNYMASTR ! Part Master File                         *~
            * #04 ! PMCODES  ! Precious Metal Codes table               *~
            * #05 ! BCKPMSLD ! Precious Metal SO Shadow File            *~
            * #06 ! MLQPMSLD ! Precious Metal MLQ Shadow File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYPMTBL",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  35,                     ~
                        alt key  1, keypos =   26, keylen =  10, dup

            select #02, "HNYPMPRC",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  25

            select  #3, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            select #04, "PMCODES",                                       ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =  10

            select #05, "BCKPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  29,                     ~
                         alternate key 1, keypos = 30, keylen = 25, dup, ~
                                   key 2, keypos = 73, keylen =  9, dup, ~
                                   key 3, keypos = 20, keylen = 10, dup

            select #06, "MLQPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  21,                     ~
                         alternate key 1, keypos = 22, keylen = 25, dup, ~
                                   key 2, keypos = 65, keylen =  9, dup, ~
                                   key 3, keypos = 12, keylen = 10, dup

*          CALL "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))

            if mode$ = "S" and o_type$ = "S" then                        ~
                call "OPENCHCK" (#05, fs%(05%), f2%(05%), 100%,rslt$(05%))
            if mode$ = "S" and o_type$ = "M" then                        ~
                call "OPENCHCK" (#06, fs%(06%), f2%(06%), 100%,rslt$(06%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            calc_date2$ = calc_date$
            call "DATEOK" ( calc_date2$, 0%, " " )

            cuscode$ = cuscode_in$
            part$    = part_in$

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "PMCALSUB: " & str(cms2v$,,8)

            disp_line_hdr$ = "PM Item    Description         UOM      " &~
                             " Price   Qty/Item  per Parts  Extension"

            sum% = 10%  /* Number of lines on Display Screen */

        REM *************************************************************~
            *                   M A I N   P R O G R A M                 *~
            *-----------------------------------------------------------*~
            *************************************************************

            if mode$ = "R" then goto remove_pm_shadow_file

            gosub load_lines
            if mode$ = "D" then goto display_pm_lines
            if mode$ = "P" then goto print_pm_lines
            if mode$ = "C" then goto exit_program
            if mode$ = "S" then goto save_to_shadow_file

            goto display_pm_lines

        display_pm_lines
            d% = 1%

L11280:
            gosub display_screen        /* Display Screen - No Entry   */
                  if keyhit%  =  1% then goto exit_program
            if keyhit% =  2% then d% = 1%                /* First page */
            if keyhit% =  3% then d% = max(1%, c% - sum% + 1)/*Last pg */
            if keyhit% =  4% then d% = max(1%, d% - sum%)   /* Prev pg */
            if keyhit% =  5% then                                        ~
                         d% = min(c% - sum% +1%, d% + sum%) /* Next pg */
            if keyhit% =  6% then d% = max(1%, d% - 1%)        /* Down */
            if keyhit% =  7% then                                        ~
                           d% = min(c% - sum% +1%, d% + 1%) /* Next pg */
            if keyhit%  = 16% then       exit_program
            if keyhit% <>  0% then       L11280   /* Redisplay */         ~
                              else       L11280   /* Redisplay */

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_lines
            init(" ") pm_code$(), pm_descr$(), pm_factor$(), pm_qty$(),  ~
                      pm_uom$(), pm_ext$(), pm_date$(), pm_price$(),     ~
                      s_charge$, warnmsg$

            mat pm_factor     =  con   /* Set to one */
            mat pm_qty        =  zer
            mat pm_price      =  zer
            mat pm_ext        =  zer
            d% = 1%
            c%, s_charge  = 0%
            call "DESCRIBE" (#3, part$, partdescr$, 0%, f1%(3%))

            plowkey$ = str(part$) & hex(00)
L30180:     call "PLOWNEXT" (#01, plowkey$, 25%, f1%(1%))
            if f1%(1%) <> 0% then L30250
               if c% > 0% then L30620 /* End this Looping */
                   warnmsg$ =                                            ~
                      "No Precious Metal Items associated with this Part."
                       result% = 0%
                   return

L30250:     c% = c% + 1%
            get #01 using L30280, pm_code$(c%), pm_qty(c%), pm_factor(c%),~
                                 pm_uom$(c%)
L30280:     FMT POS(26), CH(10), PD(14,7), PD(14,7), CH(4)
            if pm_code$(c%) <> " " then  L30295
                c% = c% - 1%
                goto L30180   /* Back for more */

L30295:     /* Get effective price */
            rev_date$ = calc_date$ xor hex(ffffffffffff)
            cus_temp$ = cuscode$
            readkey$ = str(cuscode$) & str(pm_code$(c%)) & rev_date$
            if cuscode$ = " " then L30370
            call "READ104" ( #02, readkey$, f1%(2%))
            if f1%(2%) = 0% then L30360
            if str(key(#02),1%,19%) > str(readkey$,1%,19%) then L30360
               goto L30400    /* Good Record */

              /* Try for default */
L30360:         str(readkey$,1%,9%), cus_temp$ = " "
L30370:         call "READ104" (#02, readkey$, f1%(2%))
                if str(key(#02),1%,19%) = str(readkey$,1%,19%) and ~
                   f1%(2%) <> 0% then L30400

                     result% = -1%    /* No Price Available */
                     pm_price(c%) =  0
                     pm_date$(c%) = " "
                     c% = c% - 1%
                     goto L30180
L30400:     get #2 using L30420, cus_temp2$, pm_temp$,                    ~
                                       pm_date$(c%), pm_price(c%)
L30420:       FMT CH(9), CH(10), POS(26), CH(6), PD(14,7)

              if pm_temp$ = pm_code$(c%) and cus_temp$ = cus_temp2$      ~
                                                                then L30490
                if cus_temp$ <> " " then L30360 /* Try for default price */
                    pm_price(c%) =  0
                    pm_date$(c%) = " "
                    c% = c% - 1%
                    goto L30180   /* No hit Try for next */

L30490:     call "DESCRIBE" (#04, pm_code$(c%), pm_descr$(c%), 0%,f1%(2%))

            if pm_factor(c%) = 0% then L30520  /* Shouldn't happen */
              pm_ext(c%) = qty * pm_qty(c%) * pm_price(c%) / pm_factor(c%)
L30520:     call "CONVERT" (pm_qty(c%)   , 4.7, pm_qty$(c%))
            call "CONVERT" (pm_factor(c%), 4.7, pm_factor$(c%))
            call "CONVERT" (pm_price(c%) , 4.7, pm_price$(c%))
            call "CONVERT" (pm_ext(c%)   , 2.2, pm_ext$(c%))
            call "DATEFMT" (pm_date$(c%))
            call "CONVERT" (qty, 2.2, qty$)

            s_charge  = s_charge  + pm_ext(c%)

            goto L30180   /* Back for more */

L30620:     /* Close out this Looping */
            pm_code$(c%+1%), pm_descr$(c%+1%), pm_factor$(c%+1%),        ~
            pm_qty$(c%+1%), pm_uom$(c%+1%), pm_ext$(c%+1%),              ~
            pm_date$(c%+1%), pm_price$(c%+1%) = " "
            call "CONVERT" (s_charge , 2.2, s_charge$)
            result% = 1%
            return

        save_to_shadow_file
            if c% < 1% then L30880
            if qty <> 0 then baseprice = price - s_charge * conv / qty   ~
                        else baseprice = price
            if o_type$ <> "S" then L30715
                 str(readkey$,1%,19%) = o_no$
                 str(readkey$,20%,10%) = " "
                 call "READ101" (#5, readkey$, f1%(5%))
                    /* NOTE: 1st record stores Base Price & New Price */
                 put #5 using L30800, o_no$, " ", part$, baseprice, price,~
                            pm_so$, pm_inv$, cuscode$, userid$, date, " "
                 if f1%(5%) = 0% then write #5 else rewrite #5
                 goto L30760
L30715:     if o_type$ <> "M" then L30880  /* Shouldn't happen */
                 str(readkey$,1%,11%) = o_no$
                 str(readkey$,12%,10%) = " "
                 call "READ101" (#6, readkey$, f1%(6%))
                 put #6 using L30855, o_no$, " ", part$, price, 1, pm_so$,~
                                     pm_inv$, cuscode$, userid$, date, " "
                 if f1%(6%) = 0% then write #6 else rewrite #6

L30760:     for i% = 1% to c%
                if o_type$ <> "S" then L30820
                     str(readkey$,1%,19%) = o_no$
                     str(readkey$,20%,10%) = pm_code$(i%)
                     call "READ101" (#5, readkey$, f1%(5%))
                     if pm_factor(i%) = 0 then pm_temp = 1 else          ~
                                    pm_temp = pm_qty(i%) / pm_factor(i%)
                     put #5 using L30800, o_no$, pm_code$(i%), part$,     ~
                                     pm_price(i%), pm_temp, pm_so$,      ~
                                     pm_inv$, cuscode$, userid$, date,   ~
                                     pm_descr$(i%), " "
L30800:              FMT CH(19), CH(10), CH(25), PD(14,7), PD(14,7),     ~
                         2*CH(1), CH(9), CH(3), CH(6), CH(30), CH(30)
                     if f1%(5%) = 0% then write #5 else rewrite #5
                     goto L30870
L30820:         if o_type$ <> "M" then L30880  /* Shouldn't happen */
                     if pm_factor(i%) = 0 then pm_temp = 1 else          ~
                                    pm_temp = pm_qty(i%) / pm_factor(i%)
                     str(readkey$,1%,11%) = o_no$
                     str(readkey$,12%,10%) = pm_code$(i%)
                     call "READ101" (#6, readkey$, f1%(6%))
                     put #6 using L30855, o_no$, pm_code$(i%), part$,     ~
                                     pm_price(i%), pm_temp,pm_so$,       ~
                                     pm_inv$, cuscode$, userid$, date,   ~
                                     pm_descr$(i%), " "
L30855:              FMT CH(11), CH(10), CH(25), PD(14,7), PD(14,7),     ~
                         2*CH(1), CH(9), CH(3), CH(6), CH(30), CH(38)
                     if f1%(6%) = 0% then write #6 else rewrite #6
L30870:          next i%

L30880:     goto exit_program


        remove_pm_shadow_file
            if o_type$ <> "S" then L30940
                if str(o_no$,17%,3%) = " " then len% = 16% else len% = 19%
                call "DELETE" (#5, o_no$, len%)
                goto L30990

L30940:     if o_type$ <> "M" then L30990
                if str(o_no$, 8%,3%) = " " then len% = 8% else len% = 11%
                call "DELETE" (#6, o_no$, len%)

L30990:     goto exit_program

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

*       set_rev_date
            call "DATREVRS" ( calc_date$, rev_date$, " " )
            return


        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        print_pm_lines

            call "SHOSTAT" ("Printing Surcharge Data")

            gosub generate_report
            if c% < 1% then L32200

            for i% = 1% to c%
                gosub print_lines
            next i%

L32170:     gosub end_report
            goto exit_program

L32200:     /* Nothing to do */
            print :  print " No Precious Metals Associated with this Part"
            goto L32170


        generate_report
            call "COMPNAME" (12%, company$, 0%)
            rpttitle$ = "Precious Metal Surcharges"
            call "FMTTITLE" (rpttitle$, " ", 12%)
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("HNY061", " ", 0%, 0%)
            pcntr% =  0% : lcntr% = 99% /* Page & Line Counters */
            return

        print_lines

            if lcntr% > 54% then gosub page_head

            print using L60300, pm_code$(i%), pm_descr$(i%), pm_uom$(i%), ~
                              pm_price$(i%), pm_qty$(i%), pm_factor$(i%),~
                              pm_ext$(i%), pm_date$(i%)

            lcntr% = lcntr% + 1%
            return

        end_report                /* Report Ending Routine */
            print
            print using L60390, s_charge$
            print skip(2)
            print using L60180     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "BCKPMCAL"
            print using L60110, rpttitle$, pcntr%
            print
            print
            print using L60330, part$ , partdescr$
            print using L60370, qty$, calc_date2$
            print using L60250                  /* Sub-header line 1 */
            print using L60270                  /* Sub-header line 2 */
            lcntr% = 8%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: HNYPMTBL                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(10),         /* Precious Metal Item Code                */~
            PD(14,7),       /* Precious metal quantity                 */~
            PD(14,7),       /* Multiplier to set PM Price per xxx Parts*/~
            CH(4),          /* Unit of Measure                         */~
            CH(3),          /* Definition of Type USER (user ids)      */~
            CH(6),          /* Date record last changed                */~
            CH(36)          /* Unused Space                            */~

        FMT                 /* FILE: HNYPMPRC                          */~
            CH(9),          /* Customer Code                           */~
            CH(10),         /* Precious Metal Item Code                */~
            CH(6),          /* Reversed date (999999 - yymmdd)         */~
            CH(6),          /* effective date                          */~
            PD(14,7),       /* Precious Metal Price at SO Time         */~
            CH(4),          /* Unit of Measure                         */~
            CH(3),          /* Definition of Type USER (user ids)      */~
            CH(6),          /* Date record last changed                */~
            CH(48)          /* Unused Space                            */~

        REM *************************************************************~
            *               S C R E E N   D I S P L A Y                 *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

*       DEFFN'102(FIELDNR%, EDIT%)
        display_screen
              gosub set_pf2

L41115:     accept                                                       ~
               at (01,02),                                               ~
                  "Precious Metal Surcharge Calculation",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part No:",                                   ~
               at (06,11), fac(hex(8c)), part$                  , ch(25),~
               at (06,37), fac(hex(8c)), partdescr$             , ch(32),~
               at (07,02), "Quantity:",                                  ~
               at (07,12), fac(hex(8c)), qty$                   , ch(10),~
               at (07,24), "Date:"   ,                                   ~
               at (07,30), fac(hex(8c)), calc_date2$            , ch(08),~
               at (07,41), "Total Surcharge:"   ,                        ~
               at (07,58), fac(hex(8c)), s_charge$              , ch(10),~
                                                                         ~
               at (07,58), fac(hex(84)), warnmsg$               , ch(79),~
                                                                         ~
               at (09,02), fac(hex(ac)), disp_line_hdr$         , ch(79),~
                                                                         ~
               at (10,02), fac(hex(8c)), pm_code$   (d% + 0%)   , ch(10),~
               at (10,13), fac(hex(8c)), pm_descr$  (d% + 0%)   , ch(30),~
               at (10,33), fac(hex(8c)), pm_uom$    (d% + 0%)   , ch(04),~
               at (10,38), fac(hex(8c)), pm_price$  (d% + 0%)   , ch(10),~
               at (10,49), fac(hex(8c)), pm_qty$    (d% + 0%)   , ch(10),~
               at (10,60), fac(hex(8c)), pm_factor$ (d% + 0%)   , ch(10),~
               at (10,71), fac(hex(8c)), pm_ext$    (d% + 0%)   , ch(10),~
                                                                         ~
               at (11,02), fac(hex(8c)), pm_code$   (d% + 1%)   , ch(10),~
               at (11,13), fac(hex(8c)), pm_descr$  (d% + 1%)   , ch(30),~
               at (11,33), fac(hex(8c)), pm_uom$    (d% + 1%)   , ch(04),~
               at (11,38), fac(hex(8c)), pm_price$  (d% + 1%)   , ch(10),~
               at (11,49), fac(hex(8c)), pm_qty$    (d% + 1%)   , ch(10),~
               at (11,60), fac(hex(8c)), pm_factor$ (d% + 1%)   , ch(10),~
               at (11,71), fac(hex(8c)), pm_ext$    (d% + 1%)   , ch(10),~
                                                                         ~
               at (12,02), fac(hex(8c)), pm_code$   (d% + 2%)   , ch(10),~
               at (12,13), fac(hex(8c)), pm_descr$  (d% + 2%)   , ch(30),~
               at (12,33), fac(hex(8c)), pm_uom$    (d% + 2%)   , ch(04),~
               at (12,38), fac(hex(8c)), pm_price$  (d% + 2%)   , ch(10),~
               at (12,49), fac(hex(8c)), pm_qty$    (d% + 2%)   , ch(10),~
               at (12,60), fac(hex(8c)), pm_factor$ (d% + 2%)   , ch(10),~
               at (12,71), fac(hex(8c)), pm_ext$    (d% + 2%)   , ch(10),~
                                                                         ~
               at (13,02), fac(hex(8c)), pm_code$   (d% + 3%)   , ch(10),~
               at (13,13), fac(hex(8c)), pm_descr$  (d% + 3%)   , ch(30),~
               at (13,33), fac(hex(8c)), pm_uom$    (d% + 3%)   , ch(04),~
               at (13,38), fac(hex(8c)), pm_price$  (d% + 3%)   , ch(10),~
               at (13,49), fac(hex(8c)), pm_qty$    (d% + 3%)   , ch(10),~
               at (13,60), fac(hex(8c)), pm_factor$ (d% + 3%)   , ch(10),~
               at (13,71), fac(hex(8c)), pm_ext$    (d% + 3%)   , ch(10),~
                                                                         ~
               at (14,02), fac(hex(8c)), pm_code$   (d% + 4%)   , ch(10),~
               at (14,13), fac(hex(8c)), pm_descr$  (d% + 4%)   , ch(30),~
               at (14,33), fac(hex(8c)), pm_uom$    (d% + 4%)   , ch(04),~
               at (14,38), fac(hex(8c)), pm_price$  (d% + 4%)   , ch(10),~
               at (14,49), fac(hex(8c)), pm_qty$    (d% + 4%)   , ch(10),~
               at (14,60), fac(hex(8c)), pm_factor$ (d% + 4%)   , ch(10),~
               at (14,71), fac(hex(8c)), pm_ext$    (d% + 4%)   , ch(10),~
                                                                         ~
               at (15,02), fac(hex(8c)), pm_code$   (d% + 5%)   , ch(10),~
               at (15,13), fac(hex(8c)), pm_descr$  (d% + 5%)   , ch(30),~
               at (15,33), fac(hex(8c)), pm_uom$    (d% + 5%)   , ch(04),~
               at (15,38), fac(hex(8c)), pm_price$  (d% + 5%)   , ch(10),~
               at (15,49), fac(hex(8c)), pm_qty$    (d% + 5%)   , ch(10),~
               at (15,60), fac(hex(8c)), pm_factor$ (d% + 5%)   , ch(10),~
               at (15,71), fac(hex(8c)), pm_ext$    (d% + 5%)   , ch(10),~
                                                                         ~
               at (16,02), fac(hex(8c)), pm_code$   (d% + 6%)   , ch(10),~
               at (16,13), fac(hex(8c)), pm_descr$  (d% + 6%)   , ch(30),~
               at (16,33), fac(hex(8c)), pm_uom$    (d% + 6%)   , ch(04),~
               at (16,38), fac(hex(8c)), pm_price$  (d% + 6%)   , ch(10),~
               at (16,49), fac(hex(8c)), pm_qty$    (d% + 6%)   , ch(10),~
               at (16,60), fac(hex(8c)), pm_factor$ (d% + 6%)   , ch(10),~
               at (16,71), fac(hex(8c)), pm_ext$    (d% + 6%)   , ch(10),~
                                                                         ~
               at (17,02), fac(hex(8c)), pm_code$   (d% + 7%)   , ch(10),~
               at (17,13), fac(hex(8c)), pm_descr$  (d% + 7%)   , ch(30),~
               at (17,33), fac(hex(8c)), pm_uom$    (d% + 7%)   , ch(04),~
               at (17,38), fac(hex(8c)), pm_price$  (d% + 7%)   , ch(10),~
               at (17,49), fac(hex(8c)), pm_qty$    (d% + 7%)   , ch(10),~
               at (17,60), fac(hex(8c)), pm_factor$ (d% + 7%)   , ch(10),~
               at (17,71), fac(hex(8c)), pm_ext$    (d% + 7%)   , ch(10),~
                                                                         ~
               at (18,02), fac(hex(8c)), pm_code$   (d% + 8%)   , ch(10),~
               at (18,13), fac(hex(8c)), pm_descr$  (d% + 8%)   , ch(30),~
               at (18,33), fac(hex(8c)), pm_uom$    (d% + 8%)   , ch(04),~
               at (18,38), fac(hex(8c)), pm_price$  (d% + 8%)   , ch(10),~
               at (18,49), fac(hex(8c)), pm_qty$    (d% + 8%)   , ch(10),~
               at (18,60), fac(hex(8c)), pm_factor$ (d% + 8%)   , ch(10),~
               at (18,71), fac(hex(8c)), pm_ext$    (d% + 8%)   , ch(10),~
                                                                         ~
               at (19,02), fac(hex(8c)), pm_code$   (d% + 9%)   , ch(10),~
               at (19,13), fac(hex(8c)), pm_descr$  (d% + 9%)   , ch(30),~
               at (19,33), fac(hex(8c)), pm_uom$    (d% + 9%)   , ch(04),~
               at (19,38), fac(hex(8c)), pm_price$  (d% + 9%)   , ch(10),~
               at (19,49), fac(hex(8c)), pm_qty$    (d% + 9%)   , ch(10),~
               at (19,60), fac(hex(8c)), pm_factor$ (d% + 9%)   , ch(10),~
               at (19,71), fac(hex(8c)), pm_ext$    (d% + 9%)   , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41675
                  call "MANUAL" ("PMCALSUB") : goto L41115

L41675:        if keyhit% <> 15% then L41690
                  call "PRNTSCRN" : goto L41115

L41690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                          u3% = u3%
               return

        set_pf2

            pf$(1) = "              (4)Prev Page              " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)First Page (5)Next Page              " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Last Page  (6)Down (7)Up             " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(ff020304050607ffffffffff0dff0f1000)
*        If we're at the 'top' of the line items, DISable PF(2), 4 & 6.
            if d% <> 1% then goto L41855
                str(pf$(2%),,13%), str(pf$(1%),15%,12%),                 ~
                     str(pf$(3%),15%,7%) = " "
                str(pfkeys$,2%,1%), str(pfkeys$,4%,1%),                  ~
                     str(pfkeys$,6%,1%) = hex(ff)
L41855
*        If we're at the 'end' of the line items, DISable PF(3), 5 & 7.

            if d% + sum% - 1% < c% then L41890
                str(pf$(3%),,12%), str(pf$(2%),15%,12%),                 ~
                     str(pf$(3%),23%,5%) = " "
                str(pfkeys$,3%,1%), str(pfkeys$,5%,1%),                  ~
                     str(pfkeys$,7%,1%) = hex(ff)
L41890:     return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************~

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

        %----------------------------------------------------------------~
        ~--------------
        %                         ---------------------------------------~
        ~-----------------------------------------------
L60180: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

*       * Precious Metal Codes Lines
L60250: %    PM CODE     PM Description              UOM     PM PRICE    ~
        ~  PM QTY      FACTOR   EXTENSION  EFF DATE
L60270: %    ----------  --------------------------  ----  ----------  --~
        ~--------  ----------  ----------  --------

L60300: %    ##########  ##########################  ####  ##########  ##~
        ~########  ##########  ##########  ########

L60330: % Part Number: #########################  Descr: ################~
        ~##############

L60370: % Quantity: ##########    Date: ########

L60390: %                                             Surcharge Total: ##~
        ~##########

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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
*          CALL "SHOSTAT" ("One Moment Please")

            end
