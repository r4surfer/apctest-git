        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  FFFFF   AAA   DDDD   IIIII   SSS   PPPP   RRRR   TTTTT   *~
            *  F      A   A  D   D    I    S      P   P  R   R    T     *~
            *  FFFF   AAAAA  D   D    I     SSS   PPPP   RRRR     T     *~
            *  F      A   A  D   D    I        S  P      R   R    T     *~
            *  F      A   A  DDDD   IIIII   SSS   P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FADISPRT - Fixed Assets Disposition Report                *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/03/88 ! Original (Re-write using Standards)      ! TLJ *~
            * 01/11/93 ! Page 0 Facs, Header, & End Report Time.  ! RJH *~
            * 08/06/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            accum_depr(3),               /* ACCUMULATED DEPRECIATION   */~
            asset_code$10,               /* Asset Code read in         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bonus_depr(3),               /* BONUS DEPRECIATION         */~
            cat_descr$30,                /* CATEGORY DESCRIPTION       */~
            cat$(4)1,                    /* Category Flag              */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            current_depr(3),             /* CURRENT DEPRECIATION       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_1$30,                  /* DESCRIPTION LINE 1         */~
            descr_2$30,                  /* DESCRIPTION LINE 2         */~
            depr_method$(3)2,            /* DEPRECIATION METHOD        */~
            dispdate$10,                 /* DISPOSAL DATE              */~
            disposal_date$10,            /* DISPOSAL DATE              */~
            disposal_descr$30,           /* DISPOSAL DESCRIPTION       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            exp_deduct(3),               /* EXPENSE DEDUCTION          */~
            fiscbeg$10,                  /* Fiscal Begin Date          */~
            fiscend$10,                  /* Fiscal End Date            */~
            fmasset$10,                  /* Assets                     */~
            fmdisp$10,                   /* Disposal Date              */~
            hiasset$10,                  /* Assets                     */~
            hidisp$10,                   /* Disposal Date              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            itc_reduct(3),               /* ITC BASIS REDUCTION        */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            life$(3)5,                   /* ASSET LIFE                 */~
            line2$79,                    /* Screen Line #2             */~
            loasset$10,                  /* Assets                     */~
            lodisp$10,                   /* Disposal Date              */~
            orig_basis(3),               /* ORIGINAL BASIS             */~
            other_descr$(3)50,           /* OTHER BASIS REDUCTION DESCR*/~
            other_reduct(3),             /* OTHER BASIS REDUCTION      */~
            percentage(3),               /* PERCENTAGE                 */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            property_type$1,             /* PROPERTY TYPE              */~
            rpttitle$60,                 /* Report Title               */~
            salvage_value(3),            /* SALVAGE VALUE              */~
            servdate$(3)8,               /* DATE FIRST PLACED IN SERV  */~
            service_date$10,             /* DATE FIRST PLACED IN SERV  */~
            time$8,                      /* System Time                */~
            toasset$10,                  /* Assets                     */~
            todisp$10,                   /* Disposal Date              */~
            type_code$1,                 /* Asset Type Code            */~
            userid$3                     /* Current User Id            */~

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
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "FAMASTER",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  120, keylen =  10,                     ~
                        alt key  1, keypos =   58, keylen =   1, dup     ~

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, u3%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Fixed Assets Disposition Report"
            call "STRING" addr("CT", rpttitle$, 60%)

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "FADISPRT: " & str(cms2v$,,8)

            readkey$ = "SWITCHS.FA "
            call "READ101" (#02, readkey$, f1%(2))
            if f1%(2) = 0% then L20290
              get #2 using L09240 , fiscbeg$, fiscend$
L09240:           FMT    POS(21), CH(8), CH(8)
              call "DATFMTC" (fiscbeg$)
              call "DATFMTC" (fiscend$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10201
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10201:               if keyhit% =  6% then cat$() = "XXXX"
                      if keyhit% =  7% then cat$() = "    "
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% then fieldnr% = 1%
            if fieldnr% >  3% then fieldnr% = 3%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 6% then cat$() = "XXXX"
                  if keyhit%  = 7% then cat$() = "    "
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Assets                 */~
                              L20200,         /* Disposal Date          */~
                              L20300          /* Depr. Category         */
            return
L20100: REM Def/Enable Assets                      FMASSET$
            if fmasset$            = " " then                            ~
               fmasset$            = "ALL"
            return

L20200: REM Def/Enable Disposal Date               FMDISP$
            if fmdisp$ <> " " and fmdisp$ <> blankdate$ then L20290
              fmdisp$ = fiscbeg$
              todisp$ = fiscend$
L20290:     return

L20300: REM Def/Enable Depreciation Category         CAT$()
            if str(cat$(),1,4) = " " then str(cat$(),1,4) = "XXXX"
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
         "Enter Asset Range                                            ",~
         "Enter Disposal Date Range                                    ",~
         "Depreciation Categories, B)Book, F)Federal Tax, S)State/Local, ~
        ~A)AMT"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmasset$, fmdisp$, hiasset$,                       ~
                      hidisp$, loasset$,lodisp$, toasset$,               ~
                      todisp$, cat$()
        return

        init_report_var:
            init(" ") group$, subgroup$
            no_of_assets%, end%, grp% = 0

            itc_ttl1, itc_ttl2, itc_ttl3 = 0
            disposal_ttl1, disposal_ttl2, disposal_ttl3 = 0
            total_ttl1, total_ttl2, total_ttl3 = 0
            adjust_ttl1, adjust_ttl2, adjust_ttl3 = 0
            gain_ttl1, gain_ttl2, gain_ttl3 = 0

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
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
            call "SHOSTAT" ("Printing Fixed Assets Disposition Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("F/A002", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            for cat% = 1% to 4%
              gosub init_report_var
              if cat$(cat%) <> " " then gosub print_report
            next cat%
            if lcntr% + 3% > 57% then gosub page_head
            print skip(2)
            time$ = " "  :  call "TIME" (time$)
            print using L64990, time$      /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        print_report:
            gosub get_cat_descr
            gosub page_head
            plowkey$ = loasset$
            next_rec:
              /* Read and Test the Record */
              call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
              if f1%(1) = 0 then end_report
              get #01, using L32250,                                      ~
                     depr_method$(),                                     ~
                     servdate$(),                                        ~
                     property_type$,                                     ~
                     dispdate$,                                          ~
                     asset_code$,                                        ~
                     descr_1$,                                           ~
                     descr_2$,                                           ~
                     type_code$,                                         ~
                     itc_taken,                                          ~
                     disposal_price,                                     ~
                     disposal_descr$,                                    ~
                     life$(),                                            ~
                     percentage(),                                       ~
                     orig_basis(),                                       ~
                     salvage_value(),                                    ~
                     itc_reduct(),                                       ~
                     bonus_depr(),                                       ~
                     exp_deduct(),                                       ~
                     other_reduct(),                                     ~
                     other_descr$(),                                     ~
                     accum_depr(),                                       ~
                     current_depr(),                                     ~
                     amt_flag$,                                          ~
                     amt_adj,                                            ~
                     amt_cur,                                            ~
                     amt_accum

            if asset_code$ >= hiasset$ then end_report
            if asset_code$ < loasset$ then next_rec
            if type_code$ = "1" then gosub new_group
            if type_code$ = "2" then gosub new_subgroup
            if dispdate$ < lodisp$ or dispdate$ >= hidisp$ or dispdate$ = " "  ~
                                   or dispdate$ = blankdate$ then next_rec
            if type_code$ = "3" then gosub print_asset
            goto next_rec

        end_report                /* Report Ending Routine */
            if no_of_assets% = 0% then L30711
            end% = 1%
            gosub new_group
            print using L60560, itc_ttl1, disposal_ttl1, total_ttl1,      ~
                               adjust_ttl1, gain_ttl1
            print
L30711:     print using L60430, no_of_assets%
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "FADISPRT"
            print using L60110, userid$, rpttitle$, pcntr%
            print using L60124, cat_descr$
            print
            if fiscend$ <> " " and fiscend$ <> blankdate$ then ~
                                                      to$ = "TO" else to$ = " "
            print using L60140, fiscbeg$, to$, fiscend$
            print
            print using L60180
            print using L60210
            print using L60250
            lcntr% =  9%
            return

        new_group:
            if no_of_assets%=0% then L31080  /* No assets in prev. group */
            if group$ = " " then L31080      /* No Previous Group        */
              grp% = 1%
              gosub new_subgroup
              subgroup$ = " "
              grp% = 0
              /* Print Previous Groups Total */
              if lcntr% + 4% > 57% then gosub page_head
              print using L60350
              print using L60520, group$,                                 ~
                                itc_ttl2, disposal_ttl2, total_ttl2,     ~
                                adjust_ttl2, gain_ttl2

              print using L60350
              print
              lcntr% = lcntr% + 4%
L31080:     if end% = 1% then return
            if lcntr% + 6%>57% then gosub page_head /* at least 1 asset */
            print using L60490, asset_code$, descr_1$, descr_2$
            print
            lcntr% = lcntr% + 2%
            group$ = asset_code$
            /* Reset Group Totals */
            itc_ttl2, disposal_ttl2, total_ttl2 = 0
            adjust_ttl2, gain_ttl2 = 0
            return

        new_subgroup:
            if no_of_assets% = 0 then L31300
            if subgroup$ = " " then L31300 /* First Subgroup, No totals */
              if lcntr% + 3% > 57% then gosub page_head
              print using L60350
              print using L60450, subgroup$,                              ~
                                 itc_ttl3, disposal_ttl3, total_ttl3,    ~
                                 adjust_ttl3, gain_ttl3

              print
              lcntr% = lcntr% + 3%
L31300:     if end% = 1% then return     /* End of Report Flag */
            if grp% = 1% then L31380
              if lcntr% + 2% > 57% then gosub page_head
              print using L60441, asset_code$, descr_1$, descr_2$
              print
              lcntr% = lcntr% + 2%
              subgroup$ = asset_code$
            /* Reset Subgroup Totals */
L31380:     itc_ttl3, disposal_ttl3, total_ttl3 = 0
            adjust_ttl3, gain_ttl3 = 0
            return

        print_asset:           /* Print the Asset */
            disposal_date$ = str(dispdate$,1,8)
            call "DATFMTC" (disposal_date$)
            if cat% <> 4% then L31550
              /* 4-Alternate Minimum Tax */
              if amt_flag$ = " " then L31750 /* AMT does not exist */
              service_date$ = str(servdate$(2%),1,8) /* Same as Federal */
              call "DATFMTC" (service_date$)
              total_depr   = amt_cur + amt_accum
              adjust_basis = amt_adj
              goto L31620

            /* 1-BOOK, 2-Federal, 3-State/Local */
L31550:     service_date$ = str(servdate$(cat%),1,8)
            call "DATFMTC" (service_date$)
            total_depr   = current_depr(cat%) + accum_depr(cat%) +       ~
                           bonus_depr(cat%)
            adjust_basis = orig_basis(cat%)   - itc_reduct(cat%) -       ~
                           exp_deduct(cat%)   - other_reduct(cat%)

L31620:     gain         = disposal_price  +  total_depr - adjust_basis
            if lcntr% + 4% > 57% then gosub page_head
            print using L60290, asset_code$,   service_date$,             ~
                               disposal_date$, disposal_descr$,          ~
                               itc_taken, disposal_price, total_depr,    ~
                               adjust_basis, gain

            print using L60330, descr_1$
            print using L60330, descr_2$

            print
            lcntr% = lcntr% + 4%
            gosub sum_it
L31750:     return

        sum_it:
            itc_ttl1      = itc_ttl1      + itc_taken
            disposal_ttl1 = disposal_ttl1 + disposal_price
            total_ttl1    = total_ttl1    + total_depr
            adjust_ttl1   = adjust_ttl1   + adjust_basis
            gain_ttl1     = gain_ttl1     + gain

            itc_ttl2      = itc_ttl2      + itc_taken
            disposal_ttl2 = disposal_ttl2 + disposal_price
            total_ttl2    = total_ttl2    + total_depr
            adjust_ttl2   = adjust_ttl2   + adjust_basis
            gain_ttl2     = gain_ttl2     + gain

            itc_ttl3      = itc_ttl3      + itc_taken
            disposal_ttl3 = disposal_ttl3 + disposal_price
            total_ttl3    = total_ttl3    + total_depr
            adjust_ttl3   = adjust_ttl3   + adjust_basis
            gain_ttl3     = gain_ttl3     + gain

            no_of_assets% = no_of_assets% + 1%
            return

        get_cat_descr:
            if cat% = 1% then cat_descr$ ="Book Depreciation"
            if cat% = 2% then cat_descr$ ="Federal Depreciation"
            if cat% = 3% then cat_descr$ ="State or Local Depreciation"
            if cat% = 4% then cat_descr$ ="Alternate Minimum Tax"
            call "STRING" addr("CT", cat_descr$, 30%)
            return

        print_params           /* Print Page Zero */
            print page
L32045:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L32065
                str(i$(), i%, 1%) = hex(20)
                goto L32045
L32065:     print using L60070, date$, time$, company$, "FADISPRT"
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
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L32250: FMT                 /* FILE: FAMASTER                          */~
            3*CH(2),        /* Depreciation Method                     */~
            3*CH(8),        /* Date Placed In Service                  */~
            XX(9),          /* Depreciation Expense G/L Account Code   */~
            XX(9),          /* Accumulated Depreciation G/L Account Cod*/~
            XX(9),          /* Asset G/L Account Code                  */~
            CH(1),          /* Property Type                           */~
            CH(8),          /* Disposal Date                           */~
            XX(8),          /* Purchase Date                           */~
            XX(30),         /* Location of fixed asset                 */~
            XX(15),         /* Identification Code                     */~
            CH(10),         /* Asset Code                              */~
            CH(30),         /* Fixed Asset Description Line #1         */~
            CH(30),         /* Fixed Asset Description Line #2         */~
            CH(1),          /* Fixed Asset Record Type Code            */~
            XX(8),          /* Purchase Price  PD(14,4)                */~
            PD(14,4),       /* Investment Tax Credit Taken             */~
            PD(14,4),       /* Disposal Price                          */~
            CH(30),         /* Disposal Description                    */~
            XX(9),          /* Vendor Code                             */~
            XX(16),         /* Invoice Number                          */~
            XX(30),         /* General Group Name for categorizing tabl*/~
            3*CH(5),        /* Asset Life (Years)                      */~
            XX(3),          /* PRORATION CONVENTION                    */~
            3*PD(14,4),     /* Percent for Declining Balance or Percent*/~
            XX(12),         /* Year depr method was switch to straight */~
            3*PD(14,4),     /* Original Basis                          */~
            3*PD(14,4),     /* Salvage Value                           */~
            3*PD(14,4),     /* ITC Basis Reduction                     */~
            3*PD(14,4),     /* Bonus Depreciation Taken                */~
            3*PD(14,4),     /* Expense Deduction Taken                 */~
            3*PD(14,4),     /* Other Basis Reduction                   */~
            3*CH(50),       /* Other Basis Reduction Description       */~
            3*PD(14,4),     /* Accumulated Depreciation                */~
            3*PD(14,4),     /* Current Depreciation                    */~
            XX(6),          /* In service period.                      */~
            CH(1),          /* Alternate Minimum Tax Flag 1-Adjust, 2-P*/~
            XX(10),         /* Alternate Minimum Table Group Code      */~
            XX(5),          /* Alternate Minimum Tax Class Life        */~
            PD(14,4),       /* Alternate Minimum Tax Adjusted Basis    */~
            PD(14,4),       /* Alternate Minimum Tax Current Year's Dep*/~
            PD(14,4)        /* Alternate Minimum Tax Accumulative Depre*/

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
              on fieldnr% gosub L40170,         /* Assets            */   ~
                                L40170,         /* Disposal Date     */   ~
                                L40170          /* Depr. Category    */
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Assets",                                     ~
               at (07,30), fac(lfac$( 1)), fmasset$             , ch(10),~
               at (07,56), fac(lfac$( 1)), toasset$             , ch(10),~
                                                                         ~
               at (08,02), "Disposal Date",                              ~
               at (08,30), fac(lfac$( 2)), fmdisp$              , ch(10),~
               at (08,56), fac(lfac$( 2)), todisp$              , ch(10),~
                                                                         ~
               at (09,02), "Depreciation Categories",                    ~
               at (09,30), "B)",                                         ~
               at (09,33), fac(lfac$( 3)), cat$(1)              , ch(01),~
               at (09,35), "F)",                                         ~
               at (09,38), fac(lfac$( 3)), cat$(2)              , ch(01),~
               at (09,40), "S)",                                         ~
               at (09,43), fac(lfac$( 3)), cat$(3)              , ch(01),~
               at (09,45), "A)",                                         ~
               at (09,48), fac(lfac$( 3)), cat$(4)              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40580
                  call "MANUAL" ("FADISPRT") : goto L40200

L40580:        if keyhit% <> 15 then L40610
                  call "PRNTSCRN" : goto L40200

L40610:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (6)Mark All            " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (7)Clear All           " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ffffffffff0dff0f1000)
            if fieldnr% = 1% then L40770
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40780
L40770:         str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40780:     if fieldnr% = 3% then L40820
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
                goto L40860
L40820:     if str(cat$(),1,4) <> "XXXX" then L40840
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
L40840:     if str(cat$(),1,4) <> "    " then L40860
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (6)Mark All            " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (7)Clear All           " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffff0607ffffffffff0dff0fff00)
            if fieldnr% = 3% then L41090
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
                goto L41130
L41090:     if str(cat$(),1,4) <> "XXXX" then L41110
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 6,1) = hex(ff)
L41110:     if str(cat$(),1,4) <> "    " then L41130
                str(pf$(3),18,26) = " "  :  str(pfkeys$, 7,1) = hex(ff)
L41130:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Assets                 */~
                              L50200,         /* Disposal Date          */~
                              L50400          /* Depr. Category         */
            return

L50130: REM Test for Assets                       FMASSET$
            call "TESTRNGE"                                              ~
                  (fmasset$            , toasset$            ,           ~
                   loasset$            , hiasset$            ,           ~
                   errormsg$)
            return

L50200: REM Test for Disposal Date                FMDISP$
            if fmdisp$ = "ALL" then L50300
            if fmdisp$ = "FIRST" then L50260
              call "DATEOKC" (fmdisp$, u3%, errormsg$)
              if errormsg$ <> " " then return
              call "DATUFMTC" (fmdisp$)
L50260:     if todisp$ = "LAST" then L50300
              call "DATEOKC" (todisp$, u3%, errormsg$)
              if errormsg$ <> " " then return
              call "DATUFMTC" (todisp$)
L50300:     call "TESTRNGE"                                              ~
                  (fmdisp$             , todisp$             ,           ~
                   lodisp$             , hidisp$             ,           ~
                   errormsg$)
            if fmdisp$ = "ALL" or fmdisp$ = "FIRST" then L50360
              call "DATFMTC" (fmdisp$)
L50360:     if todisp$ = "LAST" then L50380
              call "DATFMTC" (todisp$)
L50380:     return

L50400: REM Test for Depreciation Category          CAT$()
            if cat$(1) <> " " then cat$(1) = "X"
            if cat$(2) <> " " then cat$(2) = "X"
            if cat$(3) <> " " then cat$(3) = "X"
            if cat$(4) <> " " then cat$(4) = "X"
            if str(cat$(),1,4) = " " then                                ~
                               errormsg$ = "A Category Must be selected."
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########                 ########################~
        ~####################################                ########: F/A~
        ~002

*       * Header Line 2
L60110: %REPORT BY: ###                          ########################~
        ~####################################                    PAGE:  ##~
        ~###

*       * Header Line 3
L60124: %                                                       #########~
        ~#####################

L60140: %Fiscal Year: ##########  ##  ##########

L60180: %ASSET CODE/ DATE PLACED  DISPOSAL                               ~
        ~INVESTMENT      DISPOSAL      TOTAL          ADJUSTED

L60210: %  DESCR     IN SERVICE     DATE     DISPOSAL DESCRIPTION        ~
        ~TAX CREDIT         VALUE   DEPRECIATION       BASIS     GAIN (LOS~
        ~S)

L60250: %__________  __________   __________ _________________________ __~
        ~__________  ____________  _____________  ____________  __________~
        ~___

L60290: %##########  ##########   ########## ######################### ##~
        ~#######.##- #########.##- ##########.##- #########.##- #########.~
        ~##-

L60330: %     ##############################

L60350: %                                                              __~
        ~__________  ____________  _____________  ____________  __________~
        ~___

L60430: %NUMBER OF ASSETS PRINTED:  ######

L60441: %SUBGROUP:  ########## ############################## ###########~
        ~###################

L60450: %**   TOTAL FOR SUBGROUP: ##########                           ##~
        ~#######.##- #########.##- ##########.##- #########.##- #########.~
        ~##-

L60490: %GROUP:     ########## ############################## ###########~
        ~###################

L60520: %***  TOTAL FOR GROUP: ##########                              ##~
        ~#######.##- #########.##- ##########.##- #########.##- #########.~
        ~##-

L60560: %**** GRAND TOTAL:                                             ##~
        ~#######.##- #########.##- ##########.##- #########.##- #########.~
        ~##-

*       * Report Title for page 0
        %############################################################

L64990:       %                            * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @  ########  * * * * * * * * * *

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
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
