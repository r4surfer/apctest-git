        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP   RRRR    CCC   H   H   GGG    SSS   BBBB    *~
            *  V   V  P   P  R   R  C   C  H   H  G      S      B   B   *~
            *  V   V  PPPP   RRRR   C      HHHHH  G GGG   SSS   BBBB    *~
            *   V V   P      R   R  C   C  H   H  G   G      S  B   B   *~
            *    V    P      R   R   CCC   H   H   GGG    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPRCHGSB - Modifies list prices based on percent or fixed *~
            *            amount.  Discounts and specials can be kept or *~
            *            zeroed out.  Current price can be calculated   *~
            *            from list and discount or have the price change*~
            *            applied to it.                                 *~
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
            * 04/27/94 ! Original                                 ! JD2 *~
            * 06/28/96 ! Add blank date for assignment            ! DER *~
            * 12/20/99 ! Mod to change vendor's price to allow    ! CMG *~
            *          !     five decimal places.  (EWD0001)      !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "VPRCHGSB" (#01, #02, #03)

        dim                                                              ~
            blank_date$8,                /* Blank date for assignment  */~
            calc_apply$,                 /* Calc or Apply Change       */~
            change$(2)10,                /* % & Fixed Change Values    */~
            change(2),                   /* % & Fixed Change Values    */~
            columnttl$(2)25,             /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmpart$25,                   /* Our Part Number            */~
            fmvencode$9,                 /* Vendor Code                */~
            fmvenpart$25,                /* Vendor Part Number         */~
            hipart$25,                   /* Our Part Number            */~
            hivencode$9,                 /* Vendor Code                */~
            hivenpart$25,                /* Vendor Part Number         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lopart$25,                   /* Our Part Number            */~
            lovencode$9,                 /* Vendor Code                */~
            lovenpart$25,                /* Vendor Part Number         */~
            new_cur_price$10,            /* New Current Price          */~
            new_price_date$6,            /* New Price Changed Date     */~
            new_spc_price$10,            /* New Special Price          */~
            new_effec_date$8,            /* New Effective Date         */~
            new_expir_date$8,            /* New Expires Date           */~
            new_lst_price$10,            /* New List Price             */~
            new_disc_pct$6,              /* New Discount Percent       */~
            new_disc_amt$10,             /* New Discount Amount        */~
            old_cur_price$10,            /* Old Current Price          */~
            old_price_date$6,            /* Old Price Changed Date     */~
            old_spc_price$10,            /* Old Special Price          */~
            old_effec_date$8,            /* Old Effective Date         */~
            old_expir_date$8,            /* Old Expires Date           */~
            old_lst_price$10,            /* Old List Price             */~
            old_disc_pct$6,              /* Old Discount Percent       */~
            old_disc_amt$10,             /* Old Discount Amount        */~
            part$25,                     /* Part Number                */~
            part_descr$32,               /* Part Description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_rpt$1,                 /* Print List of Changes?     */~
            rpttitle$60,                 /* Report Title               */~
            time$8,                      /* System Time                */~
            topart$25,                   /* Our Part Number            */~
            tovencode$9,                 /* Vendor Code                */~
            tovenpart$25,                /* Vendor Part Number         */~
            userid$3,                    /* Current User Id            */~
            vencode$9,                   /* Vendor Code                */~
            venpart$25,                  /* Vendor Part Number         */~
            zero_disc$1,                 /* Zero Discounts (Y/N)       */~
            zero_spec$1                  /* Zero Specials (Y/N)        */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

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
            * #01 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #02 ! VENDOR   ! Vendor Master File                       *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blank_date$ = " "
            call "DATUNFMT" (blank_date$)
            call "COMPNAME" (12%, company$, ret%) : ret% = 0%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            columnttl$(1%) = "Beginning Code"
            columnttl$(2%) = "Ending Code"

            str(line2$,62) = "VPRCHGSB: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  8%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then       data_save
                  if keyhit%  = 16% then       data_save
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% = 4% or fieldnr% = 9% then editpg1 /*Blank Lines*/
            if fieldnr% > 4% then fieldnr% = fieldnr% - 1% /* 2nd Block */
            if fieldnr% > 7% then fieldnr% = fieldnr% - 1% /* 3rd Block */
            if fieldnr% < 1% or fieldnr% >  8% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *                 D A T A   S A V E                         *~
            *-----------------------------------------------------------*~
            * Convert prices based on entered parameters.               *~
            *************************************************************
        data_save
            found_something% = 0%
            if keyhit% = 14% then rpt_only% = 1% else rpt_only% = 0%
            if rpt_only% = 1% then                                       ~
                call "SHOSTAT" ("Printing Report...")  else              ~
                call "SHOSTAT" ("Changing Prices...")
*        With three different delimiters, I'm afraid we'll just have to
*        read every record in the vendor range.
            plowkey$ = all(hex(ff))
            str(plowkey$,,9%) = lovencode$
            call "REDALT3" (#01, plowkey$, 2%, f1%(1%))
            goto L13150
        loop
            call "READNXT1" (#01, f1%(1%))
L13150:         if f1%(1%) = 0% then end_report
            get #01 using L13170, vencode$, part$, venpart$
L13170:         FMT CH(9), CH(25), XX(9), CH(25)
            if vencode$ > hivencode$ then end_report
            if venpart$ <= lovenpart$ or venpart$ > hivenpart$ then loop
            if part$ <= lopart$ or part$ > hipart$ then loop

*        Now we've got a record that meets the criteria, so change it
            found_something% = 1%
            get #01 using L13300, old_cur_price, old_price_date$,         ~
                                 old_spc_price, old_effec_date$,         ~
                                 old_expir_date$, old_price_date$,       ~
                                 old_price_date$, old_lst_price,         ~
                                 old_disc_pct, old_disc_amt
                                 /* OLD_PRICE_DATE$ is not really used */
                                 /* it's only a placeholder for the FMT.*/
L13300:         FMT POS(69), PD(14,5), CH(6), POS(101), PD(14,4),        ~
                    3*CH(6), CH(3), POS(134), 3*PD(14,4)         /* (EWD0001) */ 
            call "CONVERT" (old_cur_price, 4.5, old_cur_price$)  /* (EWD0001) */
            call "CONVERT" (old_spc_price, 4.4, old_spc_price$)
            call "CONVERT" (old_lst_price, 4.4, old_lst_price$)
            call "CONVERT" (old_disc_amt,  4.4, old_disc_amt$)
            call "CONVERT" (old_disc_pct,  2.2, old_disc_pct$)

            new_lst_price = (old_lst_price * (1 + (change(1%)/100)))     ~
                            + change(2%)
            if new_lst_price < 0 then new_lst_price = 0
            new_lst_price = round(new_lst_price, decimals%)
            call "CONVERT" (new_lst_price, 4.4, new_lst_price$)

            new_disc_pct$ = old_disc_pct$ : new_disc_pct = old_disc_pct
            new_disc_amt$ = old_disc_amt$ : new_disc_amt = old_disc_amt
            if zero_disc$ = "N" then L13470
                new_disc_pct, new_disc_amt = 0
                    call "CONVERT" (new_disc_amt,  4.4, new_disc_amt$)
                    call "CONVERT" (new_disc_pct,  2.2, new_disc_pct$)

L13470:     if calc_apply$ = "A" then L13530
                new_cur_price = (new_lst_price *                         ~
                                 (1 - (new_disc_pct)/100))               ~
                                  - new_disc_amt
                if new_cur_price < 0 then new_cur_price = 0
                new_cur_price = round(new_cur_price, decimals%)
                goto L13560
L13530:     new_cur_price = (old_cur_price * (1 + (change(1%)/100)))     ~
                            + change(2%)
            if new_cur_price < 0 then new_cur_price = 0
            new_cur_price = round(new_cur_price, decimals%)
L13560:     call "CONVERT" (new_cur_price, 4.4, new_cur_price$)

            new_spc_price$ = old_spc_price$
            new_spc_price  = old_spc_price
            new_effec_date$ = old_effec_date$
            new_expir_date$ = old_expir_date$
            if zero_spec$ = "N" then L13680
                new_spc_price$ = "      0.00"
                new_spc_price  = 0
                new_effec_date$ = blank_date$
                new_expir_date$ = blank_date$

L13680:     new_price_date$ = date
            if rpt_only% = 1% then L13770
            put #01 using L13300, new_cur_price, new_price_date$,         ~
                                 new_spc_price, new_effec_date$,         ~
                                 new_expir_date$, new_price_date$,       ~
                                 userid$,         new_lst_price,         ~
                                 new_disc_pct, new_disc_amt
            rewrite #01

L13770:     if print_rpt$ = "Y" then gosub generate_report
            goto loop

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Vendor Code            */~
                              L20200,         /* Vendor Part            */~
                              L20300,         /* Our Part Number        */~
                              L20400,         /* Price Change           */~
                              L20500,         /* Zero Discounts         */~
                              L20600,         /* Calc or Apply Dsc      */~
                              L20700,         /* Zero Specials          */~
                              L20800          /* Print Report           */
            return
L20100: REM Def/Enable Vendor Code                 FMVENCODE$
            if fmvencode$          = " " then                            ~
               fmvencode$          = "ALL"
            return

L20200: REM Def/Enable Vendor Part Number          FMVENPART$
            if fmvenpart$          = " " then                            ~
               fmvenpart$          = "ALL"
            return

L20300: REM Def/Enable Our Part Number             FMPART$
            if fmpart$             = " " then                            ~
               fmpart$             = "ALL"
            return

L20400: REM Def/Enable Current List Price Change   CHANGE$
            return

L20500: REM Def/Enable Zero Discounts (Y/N)        ZERO_DISC$
            if zero_disc$ = " " then zero_disc$ = "N"
            return

L20600: REM Def/Enable Current Price- Calc or Aply CALC_APPLY$
            if calc_apply$ = " " then calc_apply$ = "C"
            return

L20700: REM Def/Enable Zero Specials (Y/N)         ZERO_SPEC$
            if zero_spec$ = " " then zero_spec$ = "N"
            return

L20800: REM Def/Enable Print Report  (Y/N)         PRINT_RPT$
            if print_rpt$ = " " then print_rpt$ = "Y"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Vendor Code Range or '?' for Selection Screen.         ",~
         "Enter Vendor Part Number Range.                              ",~
         "Enter Our Part Number Range or '?' for Selection Screen.     ",~
         "Enter a % or Fixed Amount to Change Current List Price.      ",~
         "Enter 'Y'es to Zero Out Discounts % and Fixed Amount Fields. ",~
         "'C'= Calculate Current Price Based on List & Discount; 'A'= App~
        ~ly Change.",                                                     ~
         "Enter 'Y'es to Zero Out Special Prices and Dates.            ",~
         "Enter 'Y'es to Print a Listing of Changes to Part Prices.    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      calc_apply$, change$(), fmpart$, fmvencode$,       ~
                      fmvenpart$, zero_disc$, zero_spec$, print_rpt$,    ~
                      hipart$, hivencode$, hivenpart$,                   ~
                      lopart$, lovencode$, lovenpart$,                   ~
                      topart$, tovencode$, tovenpart$
            change(1%), change(2%) = 0
            pcntr% = -1%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
          if pcntr% > -1% then write_detail
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            if rpt_only% = 1% then                                       ~
                rpttitle$ = "     What If Analysis Based on Percent" &   ~
                            " or Fixed Amount"                      else ~
                rpttitle$ = "     Modified Prices Based on Percent" &    ~
                            " or Fixed Amount"
            call "SETPRNT" ("VPR002", " ", 0%, 0%)
            lcntr% = 99% /* Line Counter */
            if lcntr% > 56% then gosub page_head

        write_detail
            part_descr$ = "** Not On File **"
            call "DESCRIBE" (#03, part$, part_descr$, 0%, f1%(3%))
            call "DATEFMT" (old_effec_date$)
            call "DATEFMT" (new_effec_date$)
            call "DATEFMT" (old_expir_date$)
            call "DATEFMT" (new_expir_date$)

            if lcntr% > 45% then gosub page_head
            print using L60200, vencode$, venpart$, part$, part_descr$,   ~
                               "List Price     ",                        ~
                               old_lst_price$, new_lst_price$
            print using L60260, "Percent Disc.  ",                        ~
                               old_disc_pct$, new_disc_pct$
            print using L60230, "Fixed Discount ",                        ~
                               old_disc_amt$, new_disc_amt$
            print using L60230, "Current Price  ",                        ~
                               old_cur_price$, new_cur_price$
            print using L60230, "Special Price  ",                        ~
                               old_spc_price$, new_spc_price$
            print using L60230, "Effective Date ",                        ~
                               old_effec_date$, new_effec_date$
            print using L60230, "Expire Date    ",                        ~
                               old_expir_date$, new_expir_date$
            print
            lcntr% = lcntr% + 8%
            return

        end_report                /* Report Ending Routine */
          call "ALLFREE" /* Release any hold that might be there */
          if found_something% = 0% then nothing_selected
          if print_rpt$ <> "Y" then L33950
            print skip(2)
            time$ = " "  :  call "TIME" (time$)
            print using L64990, time$     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
L33950:     goto inputmode

        nothing_selected
            u3% = 2%
            call "ASKUSER" (u3%, " *** NOTHING SELECTED *** ",           ~
                            "No Vendor Prices were selected based",      ~
                            "on the parameters entered.",                ~
                            "Press RETURN to acknowledge...")
            goto editpg1

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "VPRCHGSB"
            print using L60110, userid$, rpttitle$, pcntr%
            print
            print using L60140
            print using L60170
            lcntr% = 5%
            return

        print_params           /* Print Page Zero */
            print page
L34520:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L34530
                str(i$(), i%, 1%) = hex(20)
                goto L34520
L34530:     print using L60070, date$, time$, company$, "VPRCHGSB"
            print using L60110, userid$, rpttitle$, pcntr%
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40105,         /* Vendor Code       */   ~
                                L40105,         /* Vendor Part       */   ~
                                L40105,         /* Our Part Number   */   ~
                                L40105,         /* Price Change      */   ~
                                L40105,         /* Zero Discounts    */   ~
                                L40105,         /* Calc or Apply Dsc */   ~
                                L40105,         /* Zero Specials     */   ~
                                L40105          /* Print Rpt         */
              goto L40120

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40105:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40120:     accept                                                       ~
               at (01,02), "Modify Prices Based on Percent or Fixed Amoun~
        ~t",                                                              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,25), fac(hex(ac)),   columnttl$(1%)       , ch(25),~
               at (06,52), fac(hex(ac)),   columnttl$(2%)       , ch(25),~
                                                                         ~
               at (07,02), "Vendor Code",                                ~
               at (07,25), fac(lfac$(1%)), fmvencode$           , ch(09),~
               at (07,52), fac(lfac$(1%)), tovencode$           , ch(09),~
                                                                         ~
               at (08,02), "Vendor Part Number",                         ~
               at (08,25), fac(lfac$(2%)), fmvenpart$           , ch(25),~
               at (08,52), fac(lfac$(2%)), tovenpart$           , ch(25),~
                                                                         ~
               at (09,02), "Our Part Number",                            ~
               at (09,25), fac(lfac$(3%)), fmpart$              , ch(25),~
               at (09,52), fac(lfac$(3%)), topart$              , ch(25),~
                                                                         ~
               at (11,02), "Current List Price Change                  % ~
        ~-or-            Fixed Amount",                                   ~
               at (11,34), fac(lfac$(4%)), change$(1%)          , ch(10),~
               at (11,52), fac(lfac$(4%)), change$(2%)          , ch(10),~
                                                                         ~
               at (12,02), "Zero Discounts (Y/N)",                       ~
               at (12,43), fac(lfac$(5%)), zero_disc$           , ch(01),~
                                                                         ~
               at (13,02), "Calc Current Price or Apply Change (C/A)",   ~
               at (13,43), fac(lfac$(6%)), calc_apply$          , ch(01),~
                                                                         ~
               at (17,50), "Rounding to # decimals.",                    ~
               at (17,62), fac(hex(84)), decimals%,               pic(#),~
                                                                         ~
               at (14,02), "Zero Specials (Y/N)",                        ~
               at (14,43), fac(lfac$(7%)), zero_spec$           , ch(01),~
                                                                         ~
               at (16,02), "Print Listing of Changes (Y/N)",             ~
               at (16,43), fac(lfac$(8%)), print_rpt$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40365
                  call "MANUAL" ("VPRCHGSB") : goto L40120

L40365:        if keyhit% <> 15 then L40380
                  call "PRNTSCRN" : goto L40120

L40380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40475     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40460
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40465
L40460:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40465:     return

L40475: if fieldnr% > 0% then L40520  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                      (1" &        ~
                     "4)What If Report Only  (16)Process     "
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
            return
L40520:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Vendor Code            */~
                              L50200,         /* Vendor Part            */~
                              L50300,         /* Our Part Number        */~
                              L50400,         /* Price Change           */~
                              L50500,         /* Zero Discounts         */~
                              L50600,         /* Calc or Apply Dsc      */~
                              L50700,         /* Zero Specials          */~
                              L50800          /* Print Report           */
            return
L50100: REM Test for Vendor Code                  FMVENCODE$
            call "TESTRNGE"                                              ~
                  (fmvencode$          , tovencode$          ,           ~
                   lovencode$          , hivencode$          ,           ~
                   errormsg$, #02)
            return

L50200: REM Test for Vendor Part Number           FMVENPART$
            call "TESTRNGE"                                              ~
                  (fmvenpart$          , tovenpart$          ,           ~
                   lovenpart$          , hivenpart$          ,           ~
                   errormsg$)
            return

L50300: REM Test for Our Part Number              FMPART$
            call "TESTRNGE"                                              ~
                  (fmpart$             , topart$             ,           ~
                   lopart$             , hipart$             ,           ~
                   errormsg$, #03)
            return

L50400: REM Test for Current List Price Change    CHANGE$
            change(1%), change(2%) = 0 : decimals% = 4%
            if change$(1%) = " " then L50425
             /* See if we should round to 2 or 4 decimals */
             slash% = pos(change$(1%) = "/")
             if slash% = 0% then L50416
                temp$ = str(change$(1%), slash% + 1%,)
                convert temp$ to decimals%, data goto L50409
                if decimals% = 2% or decimals% = 4% then L50411
L50409:              errormsg$ = "Rounding parameter after '/' must be "&~
                                 "2 or 4." : return
L50411:         if decimals% <> 4% then decimals% = 2%
                change$(1%) = str(change$(1%),,slash% - 1%)
L50416:         call "NUMTEST" (change$(1%), -100, 1000, errormsg$,      ~
                                -2.2, change(1%))
                if errormsg$ <> " " then return
L50425:     if change$(2%) = " " then L50450
                call "NUMTEST" (change$(2%), -99999, 99999, errormsg$,   ~
                                 2.4, change(2%))
                if errormsg$ <> " " then return
L50450:     if change$(1%) = " " or change$(2%) = " " then return
                errormsg$ = "At least one of the Change Fields Must be "&~
                            "Blank."
                return

L50500: REM Test for Zero Discounts (Y/N)         ZERO_DISC$
            if zero_disc$ = "N" or zero_disc$ = "Y" then return
                errormsg$ = "Invalid entry; please enter 'Y'es or 'N'o."
            return

L50600: REM Test for Current Price- Calc or Aply  CALC_APPLY$
            if calc_apply$ = "C" or calc_apply$ = "A" then return
                errormsg$ = "Invalid entry; please enter 'C'alc or 'A'" &~
                            "pply."
            return

L50700: REM Test for Zero Specials (Y/N)          ZERO_SPEC$
            if zero_spec$ = "N" or zero_spec$ = "Y" then return
                errormsg$ = "Invalid entry; please enter 'Y'es or 'N'o."
            return

L50800: REM Test for Printing Change Report       PRINT_RPT$
            if print_rpt$ = "N" or print_rpt$ = "Y" then return
                errormsg$ = "Invalid entry; please enter 'Y'es or 'N'o."
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %Run ######## @ ########              ###########################~
        ~#################################                 ########:VPR002

*       * Header Line 2
L60110: %User ###                             ###########################~
        ~#################################                     Page: ####

L60140: %Vendor    Vendor's Part Number      Our Part Number           Pa~
        ~rt Description                 What Changed        Before      Af~
        ~ter
L60170: %--------- ------------------------- ------------------------- --~
        ~------------------------------ --------------- ---------- -------~
        ~---
L60200: %######### ######################### ######################### ##~
        ~############################## ############### ########## #######~
        ~###
L60230: %                                                                ~
        ~                               ############### ########## #######~
        ~###
L60260: %                                                                ~
        ~                               ###############   ###### %   #####~
        ~# %
        %** Report Title for page 0
        %############################################################

L64990: %                    * * * * * * * * * *   E N D   O F   R E P O ~
        ~R T  (@ ########)  * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
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
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
