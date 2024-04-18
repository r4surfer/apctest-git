        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA    SSS   U   U  BBBB                         *~
            *  F      A   A  S      U   U  B   B                        *~
            *  FFFF   AAAAA   SSS   U   U  BBBB                         *~
            *  F      A   A      S  U   U  B   B                        *~
            *  F      A   A   SSS    UUU   BBBB                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FASUB    - THIS SUBROUTINE IS CALLED BY "FAINPUT" TO ENTER*~
            *            THE DATA ON BOOK, FEDERAL TAX, AND STATE/LOCAL *~
            *            TAX DEPRECIATION (SCREENS 3, 4, AND 5).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/04/84 ! ORIGINAL                                 ! NHC *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 09/08/88 ! Updated to new Tax Laws & File Changes.  ! RJM *~
            * 09/20/88 ! Some Rework for Q.C. & Modify some Terms ! RJM *~
            * 09/22/88 ! Mods to allow Blank Convention & Period. ! RJM *~
            * 09/29/88 ! Added Alternate Minimum Tax Screen.      ! RJM *~
            * 10/07/88 ! Disabled edit of Fed depr table fields   ! RJM *~
            *          !  if AMT type is 1.  Fixed Inputmode cycle!     *~
            *          !  if AMT type is selected.                !     *~
            * 12/01/88 ! Disabled entry of SEC 179 Expense Field  ! RJM *~
            *          !  on the BOOK & STATE Depr Screens.       ! RJM *~
            * 01/11/89 ! Fixed Bug - Not copying in inservice     ! TLJ *~
            *          ! for following books after pressing PF16  !     *~
            *          ! to enter edit mode.                      !     *~
            * 07/15/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "FASUB" (asset_code$,        /* ASSET CODE                 */~
                     property_type$,     /* PERSONAL, REAL, LOW INCOME */~
                     disposal_date$,     /* DISPOSAL DATE              */~
                     purch_date$,        /* PURCHASE DATE              */~
                     purch_price,        /* PURCHASE PRICE             */~
                     amti_flag$,         /* ALTERNATIVE MIN TAX FLAG   */~
                     service_date$(),    /* DATE PLACED IN SERVICE     */~
                     group$(),           /* GROUP TABLE NAME           */~
                     life$(),            /* LIFE IN YEARS              */~
                     convention$(),      /* PRORATION CONVENTION 1->3  */~
                     cnv_dflt$,          /* CONVENTION DEFAULT         */~
                     in_service$(),      /* PERIOD PUT IN SERVICE      */~
                     depr_method$(),     /* DEPRECIATION METHOD        */~
                     percentage(),       /* PERCENTAGE                 */~
                     switch_year$(),     /* YEAR OF METHOD SWITCH      */~
                     orig_basis(),       /* ORIGINAL BASIS OF ASSET    */~
                     salvage_value(),    /* SALVAGE VALUE              */~
                     itc_reduct(),       /* INVEST TAX CREDIT TAKEN    */~
                     bonus_depr(),       /* BONUS DEPRECIATION TAKEN   */~
                     exp_deduct(),       /* EXPENSE DEDUCTION TAKEN    */~
                     other_reduct(),     /* USER DEFINED REDUCTION     */~
                     other_descr$(),     /* OTHER REDUCT DESCRIPTION   */~
                     accum_depr(),       /* ACCUMULATED DEPRECIATION   */~
                     current_depr(),     /* CURRENT DEPRECIATION       */~
                     old_depr(),         /* ORIGINAL DEPRECIATION      */~
                     itc_taken,          /* Investment Tax Credit      */~
                     fiscal_begin$,      /* FISCAL YR BEGINNING DATE   */~
                     fiscal_end$,        /* FISCAL YR ENDING DATE      */~
                     keyhit%,            /* PF KEY                     */~
                     i%,                 /* SCREEN TO MODIFY           */~
                     #02)                /* "FATABLE"-PERCENTAGE TABLE */

        dim                                                              ~
            accum_depr(4),               /* ACCUMULATED DEPRECIATION   */~
            accum_depr$(4)11,            /* ACCUMULATED DEPRECIATION   */~
            amti_flag$1,                 /* ALTERNATIVE MIN TAX FLAG   */~
            amt_adjustment$11,           /* ALT MIN TAX ADJUSTMENT     */~
            amt_lif_prmpt$(2)21,         /* AMT PROMPT FOR CLASS LIFE  */~
            amt_methoddescr$(2)32,       /* AMT DESCRIPTION            */~
            asset_code$10,               /* ASSET CODE                 */~
            bas$11,                      /* REMAINING BASIS            */~
            begdate$8,                   /* BEGINING DATE (FATABLE)    */~
            blankdate$8,                 /* Blank date for comparison  */~
            bonus_depr(3),               /* BONUS DEPRECIATION TAKEN   */~
            bonus_depr$(3)11,            /* BONUS DEPRECIATION TAKEN   */~
            cnv_descr$13,                /* PRORATE CONVENTION DESCRIP.*/~
            cnv_dflt$1,                  /* PRORATE CONVENTION DEFAULT */~
            cnv_prompt$(2)20,            /* PRORATE CONVENTION PROMPT  */~
            cnv_text$(4)14,              /* CONVENTION TYPES TEXT      */~
            convention$(3)1,             /* PRORATION CONVENTION       */~
            current_depr(4),             /* CURRENT DEPRECIATION       */~
            current_depr$(4)11,          /* CURRENT DEPRECIATION       */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            depr_method$(3)2,            /* DEPRECIATION METHOD        */~
            deprdescr$32,                /* DEPRECIATION DESCRIPTION   */~
            deprmeth$(9)32,              /* DEPR METHOD DESCRIPTION    */~
            descr_map(12),               /* PLOWCODE DISPLAY MAP       */~
            disposal_date$10,            /* DISPOSAL DATE              */~
            dispdate$10,                 /* DISPOSAL DATE UNFORMATTED  */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            enddate$8,                   /* ENDING DATE (FATABLE)      */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            exp_deduct(3),               /* EXPENSE DEDUCTION TAKEN    */~
            exp_deduct$(3)11,            /* EXPENSE DEDUCTION TAKEN    */~
            fdate$10,                    /* FMT'D FISCAL YR END DATE   */~
            fiscal_begin$8,              /* FISCAL YR BEG              */~
            fiscal_end$8,                /* FISCAL YR END              */~
            group$(4)10,                 /* GROUP TABLE NAME           */~
            grp_prompt$(2)20,            /* GROUP TABLE PROMPT         */~
            hdr$(3)79,                   /* PLOWCODE HEADER TEXT       */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            in_service$(3)2,             /* PERIOD PUT IN SERVICE      */~
            incl(2),                     /* PLOWCODE INCLUDE/EXCLUDE   */~
            incl$(2)12,                  /* PLOWCODE INCLUDE/EXCLUDE   */~
            inpmessage2$79,              /* INPUT MESSAGE LINE 20      */~
            inpmessage3$79,              /* INPUT MESSAGE LINE 19      */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            itc_reduct(3),               /* ITC BASIS REDUCTION        */~
            itc_reduct$(3)11,            /* ITC BASIS REDUCTION        */~
            lfac$(22)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lif_prompt$(2)20,            /* LIFE IN YEARS PROMPT       */~
            life$(4)5,                   /* LIFE (YEARS)               */~
            line2$79,                    /* Screen Header              */~
            method$1,                    /* TEMP DEPR METHOD           */~
            old_depr(4),                 /* ORIGINAL DEPRECIATION      */~
            old_depr$(4)11,              /* ORIGINAL DEPRECIATION      */~
            old_deprdescr$(4)32,         /* ORIGINAL DEPR DESCRIPTION  */~
            orig_basis(4),               /* ORIGINAL BASIS             */~
            orig_basis$(4)11,            /* ORIGINAL BASIS             */~
            other_descr$(3)50,           /* OTHER REDUCTION DESCRIPTION*/~
            other_reduct(3),             /* OTHER REDUCTION            */~
            other_reduct$(3)11,          /* OTHER BASIS REDUCTION      */~
            percentage(3),               /* PERCENTAGE                 */~
            percentage$(3)6,             /* PERCENTAGE                 */~
            pf$(3)79,                    /* PF KEY TEXT                */~
            pfkey$32,                    /* VALID PF KEYS              */~
            plowkey$20,                  /*                            */~
            plowkey2$20,                 /*                            */~
            property_type$1,             /* PROPERTY TYPE              */~
            proptypes$10,                /* PROPERTY TYPES  (FATABLE)  */~
            purch_date$10,               /* PURCHASE DATE              */~
            purdate$10,                  /* PURCHASE DATE              */~
            salvage_value(3),            /* SALVAGE VALUE              */~
            salvage_value$(3)11,         /* SALVAGE VALUE              */~
            servdate$10,                 /* SERVICE DATE               */~
            service_date$(3)10,          /* DATE PLACED IN SERVICE     */~
            srv_prompt$(2)20,            /* IN SERVICE PERIOD PROMPT   */~
            switch$4,                    /* TEMP SWITCH YR  (YYYY)     */~
            switch_year$(3)4,            /* YEAR OF SWITCH  (YYYY)     */~
            tdate$10,                    /* Temporary Date Variable    */~
            temp$10,                     /* Temporary Date Variable    */~
            testdate$10,                 /* test date                  */~
            title_banner$(4)60           /* TITLE AT TOP OF SCREEN     */

        dim f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            f2%(64)                      /* = 0 IF THE FILE IS OPEN    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! FATABLE  ! Fixed Assets ACRS Depreciation Tables    *~
            *************************************************************~

            select #64, "DUMMY", varc, consec, recsize = 64


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            title_banner$(1%) = "FIXED ASSETS INPUT:  BOOK DEPRECIATION"
            title_banner$(2%) = "FIXED ASSETS INPUT:  FEDERAL TAX DEPRECI~
        ~ATION"
            title_banner$(3%) = "FIXED ASSETS INPUT:  STATE/LOCAL TAX DEP~
        ~RECIATION"
            title_banner$(4%) = "FIXED ASSETS INPUT:  ALTERNATE MINIMUM T~
        ~AX DEPRECIATION"

            fdate$ = fiscal_end$
            call "DATFMTC" (fdate$)
            dispdate$ = disposal_date$

            put str(line2$,,79) using L09230, asset_code$, fdate$,         ~
                                            str(cms2v$,,8)
L09230: %Asset Code: ##########     Fiscal Yr End: ##########          FA~
        ~INPUT: ########


           grp_prompt$(1) = "Group Table Name"     : grp_prompt$(2) = " "
           lif_prompt$(1) = "Recovery (Years)"
           lif_prompt$(2) = "Life  (Years)"
           cnv_prompt$(1) = "Proration Convention" : cnv_prompt$(2) = " "
           srv_prompt$(1) = "Period In Service"    : srv_prompt$(2) = " "
           z% = 1%

            deprmeth$(1) = "Straight Line"
            deprmeth$(2) = "Straight Line After Auto Switch"
            deprmeth$(3) = "Declining Balance"
            deprmeth$(4) = "Declining Balance w/Auto Switch"
            deprmeth$(5) = "Sum Of The Years Digits"
            deprmeth$(6) = "Table Method #1"
            deprmeth$(7) = "Table Method #2"
            deprmeth$(8) = "Percent"
            deprmeth$(9) = "Manual"

            amt_lif_prmpt$(1) = "Class Life (Recovery)"
            amt_lif_prmpt$(2) = "Recovery in Years"

            amt_methoddescr$(1) = "Alternative Minimum Tax Table"
            amt_methoddescr$(2) = "Tax Preference Item using S/L"

            amt%, edit_amt% = 0%
            convert amti_flag$ to amt%, data goto L09530

L09530:     cnv_text$(1) = "Half-Year"
            cnv_text$(2) = "Mid-Quarter"
            cnv_text$(3) = "Mid-Month"
            cnv_text$(4) = "No Convention"

            init(" ") old_depr$(), old_deprdescr$()
            for temp% = 1% to 4%
                convert old_depr(temp%) to old_depr$(temp%),             ~
                pic(########.##)
*              CALL "STRING" ADDR("LJ", DEPR_METHOD$(I%), 2%)
            next temp%
            itc_reduct(1), itc_reduct(3) = 0
            amt_inputmode_flag% = 0%
            if keyhit% = 10% then edit_amt% = 1%
            if keyhit% <> 0 then L11000
            i%, inpflg% = 1%
            edit_amt% = 0%
            if amt% > 0% then amt_inputmode_flag% = 1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
          init(" ") errormsg$, inpmessage$, group$(i%), convention$(i%), ~
          orig_basis$(i%), salvage_value$(i%), itc_reduct$(i%),          ~
          bonus_depr$(i%), exp_deduct$(i%), other_reduct$(i%),           ~
          current_depr$(i%), accum_depr$(i%), life$(i%), in_service$(i%),~
          percentage$(i%), deprdescr$, method$, switch$, cnv_descr$

L10140:     for fieldnr% = 1% to 17%
                if i% = 1% then gosub'051(fieldnr%)                      ~
                           else gosub'052(fieldnr%)
                      if enabled% = 0% then L10340
L10240:         gosub'101(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10320
L10270:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10140
                         gosub'051(fieldnr%)
                         if enabled% = 0% then L10270
                         goto L10240
L10320:               if keyhit%  = 16% then L10500
                      if keyhit% <>  0% then       L10240
L10340:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10240
                next fieldnr%
L10370:     gosub L60000
            inpflg% = 0%
            i% = i% + 1%
            if i% > 3% and keyhit% = 16% then L10460
            if i% > 3% then L65000
            if keyhit% = 16% then L10370
            if i% = 3% and amt_inputmode_flag% = 1% then gosub L12000
            goto inputmode

L10460:     keyhit% = 9%
            if amt_inputmode_flag% = 1% then keyhit% = 99%
            gosub L61000   /* CALL FADEPR, CALCULATE ALT MIN TAX ADJUST */
            goto L65000

L10500:     x% = fieldnr%
            for fieldnr% = x% to 17%
                if i%  = 1% then gosub'051(fieldnr%)                     ~
                            else gosub'052(fieldnr%)
            next fieldnr%
            goto L10370


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            if edit_amt% = 1% then i% = 2%

            init(" ") errormsg$, inpmessage$,                            ~
            orig_basis$(i%), salvage_value$(i%), itc_reduct$(i%),        ~
            bonus_depr$(i%), exp_deduct$(i%), other_reduct$(i%),         ~
            current_depr$(i%), accum_depr$(i%), cnv_descr$,              ~
            percentage$(i%), deprdescr$, method$, switch$

            z% = 2%
            call "STRING" addr("LJ", depr_method$(i%), 2%)
            method$ = depr_method$(i%)
            switch$ = switch_year$(i%)
            servdate$ = service_date$(i%)
            c% = pos("123 " = convention$(i%))
            if c% > 0% then cnv_descr$ = cnv_text$(c%)
            convert method$ to d%, data goto L11176
            if d% >= 1% and d% <= 9% then deprdescr$ = deprmeth$(d%)
            if d% = 6% or d% = 7% then z% = 1% else z% = 2%
            if exp_deduct(2%) <> 0 then                                  ~
               call "CONVERT" (exp_deduct(2%), 2.2, exp_deduct$(2%))
            if exp_deduct(3%) <> 0 then                                  ~
               call "CONVERT" (exp_deduct(3%), 2.2, exp_deduct$(3%))
            exp_deduct(1%) = 0

L11176: for x% = 1% to 3%
            if percentage(x%) <> 0 then                                  ~
               call "CONVERT" (percentage(x%), 2.2, percentage$(x%))
            if orig_basis(x%) <> 0 then                                  ~
               call "CONVERT" (orig_basis(x%), 2.2, orig_basis$(x%))
            if salvage_value(x%) <> 0 then                               ~
              call "CONVERT" (salvage_value(x%), 2.2, salvage_value$(x%))
            if itc_reduct(x%) <> 0 then                                  ~
               call "CONVERT" (itc_reduct(x%), 2.2, itc_reduct$(x%))
            if bonus_depr(x%) <> 0 then                                  ~
               call "CONVERT" (bonus_depr(x%), 2.2, bonus_depr$(x%))
            if other_reduct(x%) <> 0 then                                ~
               call "CONVERT" (other_reduct(x%), 2.2, other_reduct$(x%))
               call "CONVERT" (accum_depr(x%), 2.2, accum_depr$(x%))
               call "CONVERT" (current_depr(x%), 2.2, current_depr$(x%))
        next x%
            if z% = 2% then                                              ~
                       group$(i%), convention$(i%), in_service$(i%) = " "

            if edit_amt% = 1% then L13000

L11370:     pct_flg%, cont_flag% = 0%

            gosub'101(0%, 2%)
                  errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       L65000
                  if keyhit%  =  9% then       L65000
                  if keyhit%  =  6% then       i% = 1%  /* BOOK DEPR   */
                  if keyhit%  =  7% then       i% = 2%  /* FEDERAL DPR */
                  if keyhit%  =  8% then       i% = 3%  /* STATE/LOCAL */
                  if keyhit%  = 10% and amt% > 0% then L13000
                  if keyhit% >=  6% and keyhit% <= 8% then L11000
                  if keyhit% <>  0% then       L11370
            fieldnr% = cursor%(1%) - 4%
            if fieldnr% <  1% or fieldnr% > 15% then L11370
            if fieldnr% >  9% then fieldnr% = fieldnr% + 1%
            if fieldnr% > 12% then fieldnr% = fieldnr% + 1%
            if (fieldnr% = 9% or fieldnr% = 12%) and cursor%(2%) > 42%   ~
                  then fieldnr% = fieldnr% + 1%

L11600:     gosub'051(fieldnr%)
               if enabled% = 0% then L11810
            gosub'101(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  4% then L11690
                        errormsg$ = " "
                        fieldnr% = fieldnr% - 1%
                        goto L11600
L11690:           if keyhit% <>  0% then L11600
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11600
                  if cont_flag% <> 0% then fieldnr% = fieldnr% + 1%
                  if cont_flag% <> 0% then L11600
            if pct_flg% <> 1% then L11800
                if percentage(i%) <> 0% then L11800
                    fieldnr% = 7%
                    goto L11600
L11800:     gosub L60000
L11810:     goto L11370


L12000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, group$(4%), life$(4%),     ~
                    orig_basis$(4%), current_depr$(4%), accum_depr$(4%), ~
                    amt_adjustment$, deprdescr$, cnv_descr$

L12140:     for fieldnr% = 1% to 4%
                gosub'053(fieldnr%)
                      if enabled% = 0% then L12300
L12180:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L12260
L12210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L12140
                         gosub'053(fieldnr%)
                         if enabled% = 0% then L12210
                         goto L12180
L12260:               if keyhit%  = 16% then L10370
                      if keyhit% <>  0% then       L12180
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L12180
L12300:         next fieldnr%
            gosub L61000
            i% = 3%
            return

L13000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            edit_amt% = 1%
            if life$(4) = " " then gosub L12000
            init(" ") errormsg$, inpmessage$,                            ~
                    orig_basis$(4%), current_depr$(4%), accum_depr$(4%), ~
                    amt_adjustment$

            if amt% = 1% and group$(4%) = " " then gosub L12140
            if orig_basis(4%) <> 0 then                                  ~
               call "CONVERT" (orig_basis(4%), 2.2, orig_basis$(4%))
            call "CONVERT" (current_depr(4%), 2.2, current_depr$(4%))
            call "CONVERT" (accum_depr(4%), 2.2, accum_depr$(4%))

            gosub L61000
            gosub L60000

L13450:     cont_flag% = 0%

            gosub'103(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       L65000
                  if keyhit%  =  9% then       L65000
                  if keyhit%  =  6% then       i% = 1%  /* BOOK DEPR   */
                  if keyhit%  =  7% then       i% = 2%  /* FEDERAL DPR */
                  if keyhit%  =  8% then       i% = 3%  /* STATE/LOCAL */
                  if keyhit% >=  6% and keyhit% <= 8% then edit_amt% = 0%
                  if keyhit% >=  6% and keyhit% <= 8% then L11000
                  if keyhit% <>  0% then       L13450
            fieldnr% = cursor%(1%) - 6%
            if fieldnr% <  1% or  fieldnr% > 7% then L13450
            if fieldnr% >  2% and fieldnr% < 6% then L13450
            if fieldnr% =  6% or  fieldnr% = 7%                          ~
                  then fieldnr% = fieldnr% - 3%


L13630:     gosub'053(fieldnr%)
               if enabled% = 0% then L13450
            gosub'103(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  4% then L13710
                        errormsg$ = " "
                        fieldnr% = fieldnr% - 1%
                        goto L13630
L13710:           if keyhit% <>  0% then L13630
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L13630
                  if cont_flag% <> 0% then fieldnr% = fieldnr% + 1%
                  if cont_flag% <> 0% then L13630
            gosub L61000
            gosub L60000
            goto L13450

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *  Defaults for Page 3 and Format/Enable for All Pages.     *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  inpmessage$, inpmessage2$, inpmessage3$ = " "
                  on fieldnr% gosub L20100,         /* SERVICE DATE     */~
                                    L20150,         /* DEPR METHOD      */~
                                    L20750,         /* GROUP TABLE NAME */~
                                    L20200,         /* LIFE (YEARS)     */~
                                    L20800,         /* CONVENTION       */~
                                    L20850,         /* IN SERVICE PERIOD*/~
                                    L20250,         /* PERCENTAGE       */~
                                    L20300,         /* YEAR OF SWITCH   */~
                                    L20350,         /* ORIGINAL BASIS   */~
                                    L20400,         /* SALVAGE VALUE    */~
                                    L20450,         /* ITC REDUCTION    */~
                                    L20500,         /* BONUS DEPR       */~
                                    L20550,         /* EXPENSE DEPR     */~
                                    L20600,         /* OTHER REDUCTION  */~
                                    L20640,         /* OTHER REDUCT DESC*/~
                                    L20650,         /* ACCUM DEPR       */~
                                    L20700          /* CURRENT DEPR     */
                     return
L20100:     REM DEFAULT/ENABLE FOR DATE PLACED IN SERVICE
                if service_date$(i%) = " " or  ~
                    service_date$(i%) = blankdate$ then                  ~
                     service_date$(i%) = purch_date$
                inpmessage$ = "Enter the Date this Asset was placed into ~
        ~Service."
                return
L20150:     REM DEFAULT/ENABLE FOR DEPRECIATION METHOD
                cont_flag% = 0%
                inpmessage3$ ="DEPR METHODS:  1=S/L,  2=S/L After Auto Sw~
        ~itch,  3=Declining Balance,"
                inpmessage2$ ="4=Declining Balance With Auto Switch To S/~
        ~L,  5=Sum Of The Years Digits,"
                inpmessage$ = "6=Table Method # 1,  7=Table Method # 2,  ~
        ~8=Percent,  9=Manual"
                return
L20200:     REM DEFAULT/ENABLE FOR LIFE (YEARS)
                if z% = 2% then                                          ~
                        inpmessage$ = "Enter the Estimated Life of "    &~
                                      "this Asset."                      ~
                   else inpmessage$ = "Enter the Recovery Period of an" &~
                       " Exiting Depreciation Table, or Blank for a List"
                if amt% = 1% and e% = 2% and i% = 2% then enabled% = 0%
                if amt% = 1% and e% = 2% and i% = 2% then gosub L20950
                return
L20250:     REM DEFAULT/ENABLE FOR PERCENTAGE
                cont_flag% = 0%
                if d% <> 3% and d% <> 4% and d% <> 8% then enabled% = 0%
                if enabled% = 1% then                                    ~
                  call "CONVERT" (percentage(i%), -2.2, percentage$(i%)) ~
                  else percentage(i%) = 0
                if percentage(i%) = 0 then percentage$(i%) = " "
                if enabled% = 0% then return
                if d% = 3% or d% = 4% then                               ~
                        inpmessage$ = "Enter the Declining Balance "    &~
                                      "Percentage (ie, 150% or 200%)."   ~
                   else inpmessage$ = "Enter the Straight Percentage "  &~
                           "to use to Calculate the Yearly Depreciation."
                return
L20300:     REM DEFAULT/ENABLE FOR YEAR OF SWITCH
                if d% <> 2% then enabled% = 0%
                if enabled% = 0% then switch$, switch_year$(i%) = " "
                return
L20350:     REM DEFAULT/ENABLE FOR ORIGINAL BASIS
                inpmessage$ = "Enter the Original Purchase Price for this~
        ~ Asset in this Book."
                if orig_basis(i%) = 0 then orig_basis$(i%) = " "
                if purch_price = 0 or orig_basis$(i%) <> " " then L20375
                call "CONVERT" (purch_price, -2.2, orig_basis$(i%))
                return
L20375:         call "CONVERT" (orig_basis(i%), -2.2, orig_basis$(i%))
                return
L20400:     REM DEFAULT/ENABLE FOR SALVAGE VALUE
                if d% = 6% or d% = 7% then enabled% = 0%
                if enabled% = 1% then call "CONVERT" (salvage_value(i%), ~
                                                -2.2, salvage_value$(i%))~
                                 else salvage_value(i%) = 0
                if salvage_value(i%) = 0 then salvage_value$(i%) = " "
                inpmessage$ = "Enter the Estimated Salvage Value of this ~
        ~Asset."
                return
L20450:     REM DEFAULT/ENABLE FOR ITC BASIS REDUCTION
                if itc_reduct(i%) = 0 then itc_reduct$(i%) = " "
                enabled% = 0%
                if i% = 2% then L20480
L20470:            itc_reduct$(i%) = " "
                   return
L20480:
                tdate$ = servdate$
                call "DATEFMT" ( tdate$, 0%, servdate$)
                servdatelt% = 0%
                testdate$ = "19860101"
                call "DATECONV" (testdate$)
                if servdate$ < testdate$ then servdatelt% = 1%
                if (   itc_taken = round(orig_basis(i%) * .1, 2)         ~
                   or  itc_taken = round(orig_basis(i%) * .06, 2) )      ~
                   and servdatelt% = 1% then L20496
                goto L20470
L20496:         itc_reduct(2%) = itc_taken / 2
                call "CONVERT" (itc_reduct(2%), 2.2, itc_reduct$(2%))
                return
L20500:     REM DEFAULT/ENABLE FOR BONUS DEPRECIATION TAKEN
                call "CONVERT" (bonus_depr(i%), -2.2, bonus_depr$(i%))
                if bonus_depr(i%) = 0 then bonus_depr$(i%) = " "
                inpmessage$ = "Enter the Bonus Depreciation Taken."
                return
L20550:     REM DEFAULT/ENABLE FOR EXPENSE DEDUCTION TAKEN
                if i% = 2% or i% = 3% then L20560
                     enabled% = 0%
                     exp_deduct$(i%) = " "
                     return
L20560:         call "CONVERT" (exp_deduct(i%), -2.2, exp_deduct$(i%))
                if exp_deduct(i%) = 0 then exp_deduct$(i%) = " "
                inpmessage$ = "Enter the Section 179 Expense Taken."
                return
L20600:     REM DEFAULT/ENABLE FOR OTHER BASIS REDUCTION
               call "CONVERT" (other_reduct(i%), -2.2, other_reduct$(i%))
                if other_reduct(i%) = 0 then other_reduct$(i%) = " "
                inpmessage$ = "Enter any Other Basis Reductions."
                return
L20640:     REM DEFAULT/ENABLE FOR OTHER BASIS REDUCTION DESCRIPTION
                if other_reduct(i%) = 0 then enabled% = 0%
                inpmessage$ = "Enter a Description of the Other Basis Red~
        ~uction reported."
                return
L20650:     REM DEFAULT/ENABLE FOR ACCUMULATED DEPRECIATION
                call "CONVERT" (accum_depr(i%), -2.2, accum_depr$(i%))
                inpmessage$ = "Enter the Total Accumulated Depreciation o~
        ~f this Asset."
                return
L20700:     REM DEFAULT/ENABLE FOR CURRENT DEPRECIATION
                if d% <> 9% then enabled% = 0%
                if enabled% = 1% then  call "CONVERT" (current_depr(i%), ~
                                                -2.2, current_depr$(i%))
                return
L20750:     REM DEFAULT/ENABLE FOR GROUP TABLE NAME
                if d% <> 6% and d% <> 7% then enabled% = 0%
                if amt% = 1% and e% = 2% and i% = 2% then enabled% = 0%
                if amt% = 1% and e% = 2% and i% = 2% then gosub L20950
                if enabled% = 1% then cont_flag% = 1%
                inpmessage$ = "Enter the Group Name of an Existing Deprec~
        ~iation Table, or Blank to see List."
                return
L20800:     REM DEFAULT/ENABLE FOR PRORATION CONVENTION
                if d% <> 6% and d% <> 7% then enabled% = 0%
                if amt% = 1% and e% = 2% and i% = 2% then enabled% = 0%
                if amt% = 1% and e% = 2% and i% = 2% then L20950
                if convention$(i%) = " " and enabled% = 1% then          ~
                                             convention$(i%) = cnv_dflt$
                if enabled% = 0% then cnv_descr$ = " "
                if enabled% = 1% then cont_flag% = 1%
                inpmessage$ = "Enter the Convention of an Existing Deprec~
        ~iation Table, or '?' to see List."
                return
L20850:     REM DEFAULT/ENABLE FOR IN PERIOD SERVICE
                if d% <> 6% and d% <> 7% then enabled% = 0%
                if amt% = 1% and e% = 2% and i% = 2% then enabled% = 0%
                if amt% = 1% and e% = 2% and i% = 2% then gosub L20950
                if pos(" 1" = convention$(i%)) <> 0% then enabled% = 0%
                if convention$(i%) = "1" then L20895
                     if enabled% = 0% then return
                cont_flag% = 1%
                inpmessage$ = "Enter the Period Put in Service Relative t~
        ~o the Proration Convention, or '?'."
                if in_service$(i%) <> " " then return
L20895:         c% = pos("123" = convention$(i%))
                on c% goto L20910, L20920, L20920
                return
L20910:         in_service$(i%) = " 1"
L20915:         return
L20920:
                tdate$ = service_date$(i%)
                call "DATEFMT" (tdate$, 0%, service_date$(i%))
                tdate$ = fiscal_begin$
                call "DATEFMT" (tdate$, 0%, fiscal_begin$)
                convert str(service_date$(i%),,2) to x%, data goto L20915
                convert str(fiscal_begin$,5,2) to y%, data goto L20915
                call "DATECONV" (service_date$(i%)):  /* to internal fmt */
                call "DATECONV" (fiscal_begin$):      /* to internal fmt */
                if x% < y% then x% = x% + 12%
                w% = x% + 1% - y%
                if c% = 2% then w% = int( w% / 3  + .7 )
                convert w% to in_service$(i%), pic(##)
                return

L20950:         errormsg$ = "You must turn off Alternate Minimum Tax Type~
        ~ 1 on Header Screen to Edit this."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4 & 5 *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  inpmessage$, inpmessage2$, inpmessage3$ = " "
                  on fieldnr% gosub L21250,         /* SERVICE DATE     */~
                                    L21280,         /* DEPR METHOD      */~
                                    L21760,         /* GROUP TABLE NAME */~
                                    L21320,         /* LIFE (YEARS)     */~
                                    L21800,         /* CONVENTION       */~
                                    L21850,         /* PERIOD IN SERVICE*/~
                                    L21350,         /* PERCENTAGE       */~
                                    L21390,         /* YEAR OF SWITCH   */~
                                    L21420,         /* ORIGINAL BASIS   */~
                                    L21460,         /* SALVAGE VALUE    */~
                                    L21500,         /* ITC REDUCTION    */~
                                    L21530,         /* BONUS DEPR       */~
                                    L21570,         /* EXPENSE DEPR     */~
                                    L21610,         /* OTHER REDUCTION  */~
                                    L21650,         /* OTHER REDUCT DESC*/~
                                    L21680,         /* ACCUM DEPR       */~
                                    L21720          /* CURRENT DEPR     */
                     return
L21250:     REM DEFAULT/ENABLE FOR DATE PLACED IN SERVICE
                service_date$(i%) = service_date$(1%)
                gosub'051(fieldnr%)
                return
L21280:     REM DEFAULT/ENABLE FOR DEPRECIATION METHOD
                method$ = depr_method$(1%)
                gosub'051(fieldnr%)
                return
L21320:     REM DEFAULT/ENABLE FOR LIFE (YEARS)
                life$(i%) = life$(1%)
                gosub'051(fieldnr%)
                return
L21350:     REM DEFAULT/ENABLE FOR PERCENTAGE
                gosub'051(fieldnr%)
                if enabled% = 1% then percentage$(i%) = percentage$(1%)
                return
L21390:     REM DEFAULT/ENABLE FOR YEAR OF SWITCH
                gosub'051(fieldnr%)
                return
L21420:     REM DEFAULT/ENABLE FOR ORIGINAL BASIS
                orig_basis$(i%) = orig_basis$(1%)
                gosub'051(fieldnr%)
                return
L21460:     REM DEFAULT/ENABLE FOR SALVAGE VALUE
                salvage_value$(i%) = salvage_value$(1%)
                gosub'051(fieldnr%)
                return
L21500:     REM DEFAULT/ENABLE FOR ITC BASIS REDUCTION
                gosub'051(fieldnr%)
                return
L21530:     REM DEFAULT/ENABLE FOR BONUS DEPRECIATION TAKEN
                bonus_depr$(i%) = bonus_depr$(1%)
                gosub'051(fieldnr%)
                return
L21570:     REM DEFAULT/ENABLE FOR EXPENSE DEDUCTION TAKEN
                exp_deduct$(i%) = exp_deduct$(1%)
                gosub'051(fieldnr%)
                return
L21610:     REM DEFAULT/ENABLE FOR OTHER BASIS REDUCTION
                other_reduct$(i%) = other_reduct$(1%)
                gosub'051(fieldnr%)
                return
L21650:     REM DEFAULT/ENABLE FOR OTHER BASIS REDUCTION
                other_descr$(i%) = other_descr$(1%)
                gosub'051(fieldnr%)
                if enabled% = 0% then other_descr$(i%) = " "
                return
L21680:     REM DEFAULT/ENABLE FOR ACCUMULATED DEPRECIATION
                accum_depr$(i%) = accum_depr$(1%)
                gosub'051(fieldnr%)
                return
L21720:     REM DEFAULT/ENABLE FOR CURRENT DEPRECIATION
                current_depr$(i%) = current_depr$(1%)
                gosub'051(fieldnr%)
                return
L21760:     REM DEFAULT/ENABLE FOR GROUP TABLE NAME
                if d% <> 6% and d% <> 7% then enabled% = 0%
                if enabled% = 1% then group$(i%) = group$(1%)
                if enabled% = 1% then gosub'051(fieldnr%)
                return
L21800:     REM DEFAULT/ENABLE FOR PRORATION CONVENTION
                if d% <> 6% and d% <> 7% then enabled% = 0%
                if enabled% = 1% then convention$(i%) = convention$(1%)
                if enabled% = 1% then gosub'051(fieldnr%)
                if enabled% = 0% then cnv_descr$ = " "
                return
L21850:     REM DEFAULT/ENABLE FOR IN SERVICE PERIOD
                if d% <> 6% and d% <> 7% then enabled% = 0%
                if enabled% = 1% then in_service$(i%) = in_service$(1%)
                if enabled% = 1% then gosub'051(fieldnr%)
                return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4 & 5 *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1%
                  inpmessage$, inpmessage2$, inpmessage3$ = " "
                  on fieldnr% gosub L22200,         /* GROUP TABLE      */~
                                    L22300,         /* CLASS LIFE       */~
                                    L22400,         /* ADJUSTED BASIS   */~
                                    L22500          /* AMT ACCUM DEPR   */~

                     return

L22200:     REM DEFAULT/ENABLE FOR AMT GROUP TABLE NAME
                if amt% <> 1% then enabled% = 0%
                if life$(4%) = " " then life$(4%) = life$(2%)
                inpmessage$ = "Enter the Group Name of an Existing Deprec~
        ~iation Table, or Blank to see List."
                return
L22300:     REM DEFAULT/ENABLE FOR AMT CLASS LIFE
                if life$(4%) = " " then life$(4%) = life$(2%)
                if amt% = 1% then cont_flag% = 1%
                if amt% = 1% then inpmessage$ = "Enter the Class Life Rec~
        ~overy an Exiting Depr. Table or Blank to see List"
                if amt% = 2% then inpmessage$ = "Enter the Recovery Life ~
        ~to use in the Straight Line Depr Calculation."
                return
L22400:     REM DEFAULT/ENABLE FOR AMT ADJUSTED BASIS
                if amt% = 1% then                                        ~
                inpmessage$ = "Enter the Adjusted Basis to use for Cal" &~
                              "culation of the A.M.T. Adjustment."       ~
                else                                                     ~
                inpmessage$ = "Enter the Adj. Basis to use for Cal" &    ~
                           "culation of the A.M.T. Tax Preference Item."

                if orig_basis(4%) > 0 then L22470
                orig_basis(4%) = orig_basis(2%) - itc_reduct(2%)         ~
                               - bonus_depr(2%) - exp_deduct(2%)         ~
                               - other_reduct(2%)
L22470:         call "CONVERT" (orig_basis(4%), -2.2, orig_basis$(4%))
                return

L22500:     REM DEFAULT/ENABLE FOR AMT ACCUMULATED DEPRECIATION
                if amt% = 1% then                                        ~
                inpmessage$ = "Enter the Accum. Depr to use for Calculati~
        ~on of the A.M.T. Adjustment."                                    ~
                else inpmessage$ = "Enter the Accum. Depr to use for Calc~
        ~ultion of the A.M.T. Tax Preference Item."
                if accum_depr(4%) > 0 then                               ~
                   call "CONVERT" (accum_depr(4%), -2.2, accum_depr$(4%))
                return


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            return clear all
            goto L65000

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            *************************************************************

            deffn'101(fieldnr%, e%)

               if fieldnr% > 0% then init(hex(8c)) lfac$()               ~
                                else init(hex(86)) lfac$()
               d% = 0%
               if i% = 2% then itc_prompt$ = "ITC Basis Reduction"       ~
                          else itc_prompt$ = " "
               convert depr_method$(i%) to d%, data goto L40060
L40060:        if d% = 2% then sw_prompt$ = "Year Of Switch"             ~
                          else sw_prompt$ = " "
               if z% = 2% then cnv_descr$ = " "
               gosub setpf1

               on fieldnr% gosub    L40190,         /* SERVICE DATE     */~
                                    L40190,         /* DEPR METHOD      */~
                                    L40190,         /* GROUP TABLE      */~
                                    L40205,         /* LIFE / RECOVERY  */~
                                    L40190,         /* CONVENTION       */~
                                    L40190,         /* IN SERVICE PERIOD*/~
                                    L40205,         /* PERCENTAGE       */~
                                    L40205,         /* YEAR OF SWITCH   */~
                                    L40205,         /* ORIGINAL BASIS   */~
                                    L40205,         /* SALVAGE VALUE    */~
                                    L40205,         /* ITC REDUCTION    */~
                                    L40205,         /* BONUS DEPR       */~
                                    L40205,         /* EXPENSE DEPR     */~
                                    L40205,         /* OTHER REDUCTION  */~
                                    L40190,         /* OTHER REDUCT DESC*/~
                                    L40205,         /* ACCUM DEPR       */~
                                    L40205          /* CURRENT DEPR     */
                     goto L40225

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40205:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40225:     if fieldnr% = 2% then L41000
L40230:
            tdate$ = service_date$(i%)
            if tdate$ < " " then call "DATFMTC"(tdate$)

            accept                                                       ~
               at (01,02), fac(hex(8c)), title_banner$(i%)      , ch(60),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02),                                               ~
                  "Date Placed In Service",                              ~
               at (05,30), fac(lfac$( 1)), tdate$               , ch(10),~
                                                                         ~
               at (06,02),                                               ~
                  "Depreciation Method",                                 ~
               at (06,30), fac(lfac$( 2)), method$              , ch(01),~
               at (06,49), fac(hex(8c)),   deprdescr$           , ch(32),~
                                                                         ~
               at (07,02), fac(hex(8c)),   grp_prompt$(z%)      , ch(20),~
               at (07,30), fac(lfac$( 3)), group$(i%)           , ch(10),~
                                                                         ~
               at (08,02), fac(hex(8c)),   lif_prompt$(z%)      , ch(20),~
               at (08,30), fac(lfac$( 4)), life$(i%)            , ch(05),~
                                                                         ~
               at (09,02), fac(hex(8c)),   cnv_prompt$(z%)      , ch(20),~
               at (09,30), fac(lfac$( 5)), convention$(i%)      , ch(01),~
               at (09,49), fac(hex(8c)),   cnv_descr$           , ch(13),~
                                                                         ~
               at (10,02), fac(hex(8c)),   srv_prompt$(z%)      , ch(20),~
               at (10,30), fac(lfac$( 6)), in_service$(i%)      , ch(02),~
                                                                         ~
               at (11,02),                                               ~
                  "Percentage",                                          ~
               at (11,30), fac(lfac$( 7)), percentage$(i%)      , ch(06),~
                                                                         ~
               at (12,02), fac(hex(8c)), sw_prompt$             , ch(20),~
               at (12,30), fac(lfac$( 8)), switch$              , ch(04),~
                                                                         ~
               at (13,02),                                               ~
                  "Original Cost",                                       ~
               at (13,30), fac(lfac$( 9)), orig_basis$(i%)      , ch(11),~
               at (13,49),                                               ~
                  "Salvage Value",                                       ~
               at (13,67), fac(lfac$(10)), salvage_value$(i%)   , ch(11),~
                                                                         ~
               at (14,02), fac(hex(8c)), itc_prompt$            , ch(20),~
               at (14,30), fac(lfac$(11)), itc_reduct$(i%)      , ch(11),~
                                                                         ~
               at (15,02),                                               ~
                  "Bonus Depreciation Taken",                            ~
               at (15,30), fac(lfac$(12)), bonus_depr$(i%)      , ch(11),~
               at (15,49),                                               ~
                  "Sec 179 Expense",                                     ~
               at (15,65), fac(lfac$(13)), exp_deduct$(i%)      , ch(11),~
                                                                         ~
               at (16,02),                                               ~
                  "Other Basis Reduction",                               ~
               at (16,30), fac(lfac$(14)), other_reduct$(i%)    , ch(11),~
                                                                         ~
               at (17,02),                                               ~
                  "Other Basis Reduct Descr",                            ~
               at (17,30), fac(lfac$(15)), other_descr$(i%)     , ch(50),~
                                                                         ~
               at (18,02),                                               ~
                  "Accumulated Depreciation",                            ~
               at (18,30), fac(lfac$(16)), accum_depr$(i%)      , ch(11),~
                                                                         ~
               at (19,02),                                               ~
                  "Current Year Depreciation",                           ~
               at (19,30), fac(lfac$(17)), current_depr$(i%)    , ch(11),~
               at (19,49), fac(hex(8c)),   old_deprdescr$(i%)   , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L40640
                  call "MANUAL" ("FAINPUT ")
                  goto L40230

L40640:        if keyhit% <> 15 then L40660
                  call "PRNTSCRN"
                  goto L40230

L40660:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               service_date$(i%) = tdate$
               call "DATUFMTC" (service_date$(i%))
               return



        setpf1
        if e% = 2% then L40780            /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     " ~
                  & "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                " ~
                  & "             (15)Print Screen"
           pf$(3) = "                                                  " ~
                  & "             (16)Edit Mode   "
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0f1000)
           if amt% = 1% and i% < 3% and (fieldnr% < 9% or i% = 1%)       ~
                then L40745
           if i% > 1% or fieldnr% > 9% then L40755
L40745:         str(pf$(3),64)   = " "
                str(pfkey$,16,1) = hex(ff)
L40755:    if fieldnr% > 1% then L40770
                str(pf$(2),18,18)   = " "
                str(pfkey$,4,1) = hex(ff)
L40770:    return

L40780:  if fieldnr% > 0% then L40865     /* Edit Mode- Select Field    */
            pf$(1) = "(1)Start Over     (6)Edit Book Depr          (9)Hea~
        ~der Screen  (13)Instructions"
            pf$(2) = "                  (7)Edit Federal Tax Depr   (10)Ed~
        ~it A.M.T.   (15)Print Screen"
            pf$(3) = "                  (8)Edit State/Local Tax Depr     ~
        ~           " & hex(84) & "(16)SAVE DATA"
           pfkey$ = hex(01ffffffff060708090affff0dff0f1000)
           if amt% > 0% then L40835
              str(pf$(i%), 19, 25%+i%) = " "
              str(pfkey$, i%+5%, 1) = hex(ff)
L40835:    if amt% > 0% and edit_amt% = 0% then L40850
              str(pf$(2), 44, 20) = " "
              str(pfkey$, 10, 1) = hex(ff)
L40850:    inpmessage$ = edtmessage$
                return
                                         /* Edit Mode- Field Enabled   */
L40865:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0fffffffff00)
           if cont_flag% = 1% and fieldnr% > 1% then return
           str(pf$(2),18,18) = " "
           str(pfkey$,4,1) = hex(ff)
           return


L41000: REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUT MODE FOR DEPRECIATION METHOD FIELD                  *~
            *************************************************************

L41060:
            tdate$ = service_date$(i%)
            if tdate$ < " " then call "DATFMTC"(tdate$)

            accept                                                       ~
               at (01,02), fac(hex(8c)), title_banner$(i%)      , ch(60),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02),                                               ~
                  "Date Placed In Service",                              ~
               at (05,30), fac(lfac$( 1)), tdate$               , ch(10),~
                                                                         ~
               at (06,02),                                               ~
                  "Depreciation Method",                                 ~
               at (06,30), fac(lfac$( 2)), method$              , ch(01),~
               at (06,49), fac(hex(8c)),   deprdescr$           , ch(32),~
                                                                         ~
               at (07,02), fac(hex(8c)),   grp_prompt$(z%)      , ch(20),~
               at (07,30), fac(lfac$( 3)), group$(i%)           , ch(10),~
                                                                         ~
               at (08,02), fac(hex(8c)),   lif_prompt$(z%)      , ch(20),~
               at (08,30), fac(lfac$( 4)), life$(i%)            , ch(05),~
                                                                         ~
               at (09,02), fac(hex(8c)),   cnv_prompt$(z%)      , ch(20),~
               at (09,30), fac(lfac$( 5)), convention$(i%)      , ch(01),~
               at (09,49), fac(hex(8c)),   cnv_descr$           , ch(14),~
                                                                         ~
               at (10,02), fac(hex(8c)),   srv_prompt$(z%)      , ch(20),~
               at (10,30), fac(lfac$( 6)), in_service$(i%)      , ch(02),~
                                                                         ~
               at (11,02),                                               ~
                  "Percentage",                                          ~
               at (11,30), fac(lfac$( 7)), percentage$(i%)      , ch(06),~
                                                                         ~
               at (12,02), fac(hex(8c)), sw_prompt$             , ch(20),~
               at (12,30), fac(lfac$( 8)), switch$              , ch(04),~
                                                                         ~
               at (13,02),                                               ~
                  "Original Cost",                                       ~
               at (13,30), fac(lfac$( 9)), orig_basis$(i%)      , ch(11),~
               at (13,49),                                               ~
                  "Salvage Value",                                       ~
               at (13,67), fac(lfac$(10)), salvage_value$(i%)   , ch(11),~
                                                                         ~
               at (14,02), fac(hex(8c)), itc_prompt$            , ch(20),~
               at (14,30), fac(lfac$(11)), itc_reduct$(i%)      , ch(11),~
                                                                         ~
               at (15,02),                                               ~
                  "Bonus Depreciation Taken",                            ~
               at (15,30), fac(lfac$(12)), bonus_depr$(i%)      , ch(11),~
               at (15,49),                                               ~
                  "Sec 179 Expense",                                     ~
               at (15,67), fac(lfac$(13)), exp_deduct$(i%)      , ch(11),~
                                                                         ~
               at (16,02),                                               ~
                  "Other Basis Reduction",                               ~
               at (16,30), fac(lfac$(14)), other_reduct$(i%)    , ch(11),~
                                                                         ~
               at (17,02),                                               ~
                  "Other Basis Reduct Descr",                            ~
               at (17,30), fac(lfac$(15)), other_descr$(i%)     , ch(50),~
                                                                         ~
               at (18,02),                                               ~
                  "Accumulated Depreciation",                            ~
               at (18,30), fac(lfac$(16)), accum_depr$(i%)      , ch(11),~
                                                                         ~
               at (19,02), fac(hex(84)),   inpmessage3$         , ch(79),~
               at (20,02), fac(hex(84)),   inpmessage2$         , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L41890
                  call "MANUAL" ("FAINPUT ")
                  goto L41060

L41890:        if keyhit% <> 15 then L41930
                  call "PRNTSCRN"
                  goto L41060

L41930:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               service_date$(i%) = tdate$
               call "DATUFMTC" (service_date$(i%))
               return


        REM *************************************************************~
            *      I N P U T   &   E D I T   A T M   S C R E E N        *~
            *                                                           *~
            *************************************************************

            deffn'103(fieldnr%, e%)
               deprdescr$ = " "
               if fieldnr% > 0% then init(hex(8c)) lfac$()               ~
                                else init(hex(86)) lfac$()

               convert depr_method$(2%) to d%, data goto L42101
               deprdescr$ = deprmeth$(d%)
L42101:        cnv_descr$ = " "
               c% = pos("123" = convention$(2%))
               if amt% = 2% then lfac$(1) = hex(8c)
               if amt% = 1% and c% > 0% then cnv_descr$ = cnv_text$(c%)
               amt_convention$, amt_period$ = " "
               if amt% = 2% then L42120
                  amt_convention$ = convention$(2%)
                  amt_period$ = in_service$(2%)
L42120:        gosub setpf1

               on fieldnr% gosub    L42360,         /* GROUP TABLE      */~
                                    L42390,         /* LIFE / RECOVERY  */~
                                    L42390,         /* ADJUSTED BASIS   */~
                                    L42390          /* AMT ACCUM DEPR   */~

                     goto L42440

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42360:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42390:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42440:     tdate$ = service_date$(2%)
            if tdate$ < " " then call "DATFMTC"(tdate$)

            accept                                                       ~
               at (01,02), fac(hex(8c)), title_banner$(4%)      , ch(60),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02),                                               ~
                  "Date Placed In Service",                              ~
               at (05,30), fac(hex(8c)),   tdate$               , ch(10),~
                                                                         ~
               at (06,49), fac(hex(8c)),  amt_methoddescr$(amt%), ch(32),~
                                                                         ~
               at (07,02), fac(hex(8c)),   grp_prompt$(amt%)    , ch(20),~
               at (07,30), fac(lfac$( 1)), group$(4%)           , ch(10),~
                                                                         ~
               at (08,02), fac(hex(8c)),   amt_lif_prmpt$(amt%) , ch(21),~
               at (08,30), fac(lfac$( 2)), life$(4%)            , ch(05),~
                                                                         ~
               at (09,02), fac(hex(8c)),   cnv_prompt$(amt%)    , ch(20),~
               at (09,30), fac(hex(8c)),   amt_convention$      , ch(01),~
               at (09,49), fac(hex(8c)),   cnv_descr$           , ch(14),~
                                                                         ~
               at (10,02), fac(hex(8c)),   srv_prompt$(amt%)    , ch(20),~
               at (10,30), fac(hex(8c)),   amt_period$          , ch(02),~
                                                                         ~
               at (12,02),                                               ~
                  "Adjusted Basis",                                      ~
               at (12,30), fac(lfac$( 3)), orig_basis$(4%)      , ch(11),~
                                                                         ~
               at (13,02),                                               ~
                  "AMT ACCUM Depreciation",                              ~
               at (13,30), fac(lfac$( 4)), accum_depr$(4%)      , ch(11),~
                                                                         ~
               at (14,02),                                               ~
                  "AMT CURRENT Depreciation",                            ~
               at (14,30), fac(hex(8c)),   current_depr$(4%)    , ch(11),~
               at (14,49), fac(hex(8c)),   old_deprdescr$(4%)   , ch(32),~
                                                                         ~
               at (16,02),                                               ~
                  "AMT DEPR ADJUSTMENT",                                 ~
               at (16,30), fac(hex(8c)),   amt_adjustment$      , ch(11),~
                                                                         ~
               at (18,02),                                               ~
                  "Federal Current Depr",                                ~
               at (18,30), fac(hex(8c)),   current_depr$(2%)    , ch(11),~
                                                                         ~
               at (19,02),                                               ~
                  "Federal Depreciation Method",                         ~
               at (19,40), fac(hex(8c)),   depr_method$(2%)     , ch(02),~
               at (19,49), fac(hex(8c)),   deprdescr$           , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L43270
                  call "MANUAL" ("FAINPUT ")
                  goto L42440

L43270:        if keyhit% <> 15 then L43310
                  call "PRNTSCRN"
                  goto L42440

L43310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* SERVICE DATE     */~
                                    L50300,         /* DEPR METHOD      */~
                                    L50500,         /* Group Table Name */~
                                    L50700,         /* LIFE (YEARS)     */~
                                    L50900,         /* Convention       */~
                                    L51000,         /* In Service Period*/~
                                    L51100,         /* PERCENTAGE       */~
                                    L51300,         /* YEAR OF SWITCH   */~
                                    L51400,         /* ORIGINAL BASIS   */~
                                    L51600,         /* SALVAGE VALUE    */~
                                    L51800,         /* ITC REDUCTION    */~
                                    L52000,         /* BONUS DEPR       */~
                                    L52200,         /* EXPENSE DEPR     */~
                                    L52400,         /* OTHER REDUCTION  */~
                                    L52600,         /* OTHER REDUCT DESC*/~
                                    L52700,         /* ACCUM DEPR       */~
                                    L53000          /* CURRENT DEPR     */
                     return

L50100:     REM TEST DATA FOR DATE PLACED IN SERVICE    SERVICE_DATE$(I%)
                service_date$(i%) = tdate$
                if service_date$(i%) = " " or ~
                   service_date$(i%) = blankdate$ then L50200
                call "DATEOKC" (service_date$(i%),temp%,errormsg$)
                     if errormsg$ <> " " then return
                servdate$ = str(service_date$(i%),1,10)
                call "DATUFMTC" (servdate$)
                purdate$ = str(purch_date$,1,10)
                call "DATUFMTC" (purdate$)
                if servdate$ < purdate$ then L50215
                tdate$ = servdate$
                call "DATEFMT" (tdate$, 0%, servdate$)
                convert str(servdate$,1,4) to yy%, data goto L50200
                call "DATECONV" (servdate$)    /* to internal fmt */
                if depr_method$(i%) = " " then L50165
                convert depr_method$(i%) to d%, data goto L50165
                if (d% = 6% or d% = 7%) and yy% < 1981% then L50235
L50165:         if yy% >= 1986% and i% = 2% then itc_taken = 0
                if e% = 1% then L50185
                     if d% <> 6% and d% <> 7% then return
                     gosub check_depr_table
L50185:         if inpflg% <> 0% then L50186
                    call "DATUFMTC" (service_date$(i%))
                       return
L50186:             call "DATUFMTC" (service_date$(1%))
                service_date$(2%), service_date$(3%) = service_date$(1%)
                return
L50200:         errormsg$ = "PLEASE ENTER THE DATE FIRST PLACED IN SERVIC~
        ~E"
                return
L50215:         errormsg$ = "THIS DATE IS BEFORE THE PURCHASE DATE: " &  ~
                                                              purch_date$
                return

L50235:         errormsg$ = "ASSETS USINGS DEPRECIATION TABLES MUST HAVE ~
        ~BEEN PLACED IN SERVICE AFTER 1980"
                return


L50300:     REM TEST DATA FOR DEPRECIATION METHOD       DEPR_METHOD$(I%)
                convert method$ to d%, data goto L50435
                if d% < 1% or d% > 9% then L50435
                tdate$ = servdate$
                call "DATEFMT" (tdate$, 0%, servdate$)
                convert str(servdate$,1,4) to yy%, data goto L50325
                call "DATECONV" (servdate$)   /* to internal fmt */
                if (d% = 6% or d% = 7%) and yy% < 1981% then L50235
                if i% = 2% and amt% = 1% and d% <> 6% and d% <> 7% then  ~
                                                              goto L50465
L50325:         if d% = 6% or d% = 7% then z% = 1% else z% = 2%
                convert d% to str(depr_method$(i%),,1), pic(#)
                deprdescr$ = deprmeth$(d%)
                if d% = 3% or d% = 4% or d% = 8% then pct_flg% = 1%      ~
                                                 else pct_flg% = 0%
                   if pct_flg% = 1% then L50360
                      percentage$(i%) = " "
                      percentage(i%) = 0
                      pct_flg% = 0%
L50360:         if d% <> 6% and d% <> 7% then                            ~
                       group$(i%), convention$(i%), in_service$(i%) = " "
                if d% <> 2% then switch$, switch_year$(i%) = " "
                if d% <> 6% and d% <> 7%  then L50420
                     plowkey$ = all(hex(00))
                     call "PLOWNEXT" (#02, plowkey$, 0%, f1%(2))
                          if f1%(2) = 0% then L50450
                     gosub L55000
                if e% = 2% and (group$(i%) = " " or f1%(2) =  0%)        ~
                     then cont_flag% = 1%
                if errormsg$ <> " " then fieldnr% = fieldnr% + 1%
                salvage_value$(i%) = " " : salvage_value(i%) = 0
L50420:         if inpflg% = 0% then return
                depr_method$(2%), depr_method$(3%) = depr_method$(1%)
                return
L50435:         errormsg$ = "Valid Depreciation Methods: 1,2,3,4,5,6,7,8,~
        ~9"
                return
L50450:         errormsg$ = "There Are No Depreciation Tables On File,  C~
        ~HOOSE ANOTHER METHOD."
                return
L50465:         errormsg$ = "Alternative Minimum Tax was Selected; You MU~
        ~ST use Depreciation Method 6 or 7."
                return

L50500:     REM TEST DATA FOR GROUP TABLE NAME          GROUP$(I%)
                if enabled% = 0% then return
                cont_flag% = 1%
                if group$(i%) = " " then L50540
                call "STRING" addr("LJ", group$(i%), 10%)
L50540:         gosub L55000
                if errormsg$ <> " " then cont_flag% = 1%
                if inpflg% = 0% then return
                group$(2%), group$(3%) = group$(1%)
                return

L50700:     REM TEST DATA FOR LIFE (YEARS)              LIFE$(I%)
                if life$(i%) = " " and z% = 1% then L50763
                convert life$(i%) to life, data goto L50820
                if z% = 2% and life < 1% or life > 99% then L50800
                if z% = 1% and life < 1% or life > 60% then L50810
                convert life to life$(i%), pic(##.00)
                if z% > 1% then L50770
L50763:         cont_flag% = 1%
                gosub L55000
                     if errormsg$ <> " " then cont_flag% = 1%
L50770:         if inpflg% = 0% then return
                life$(2%), life$(3%) = life$(1%)
                return
L50800:         errormsg$ = "VALID LIFE: 1 - 99 YEARS"
                return
L50810:         errormsg$ = "VALID RECOVERY: 1 - 60 YEARS"
                return
L50820:         errormsg$ = "INVALID NUMBER"
                return

L50900:     REM TEST DATA FOR PRORATION CONVENTION      CONVENTION$(I%)
                if enabled% = 0% then return
                cont_flag% = 1%
                if convention$(i%) = " " then in_service$(i%) = " "
                if convention$(i%) = "1" then in_service$(i%) = " 1"
                gosub L55000
                if i% = 2% and amt% = 1% and convention$(i%) = " "       ~
                     then L50975
                if convention$(i%) = " " then in_service$(i%) = " "
                if convention$(i%) = "1" then in_service$(i%) = " 1"
                if f1%(2) = 1% and pos(" 1" = convention$(i%)) <> 0%     ~
                     then cont_flag% = 0%
                if f1%(2) = 0% and convention$(i%) = " " then L50990
                cnv_descr$ = " "
                if cont_flag% = 1% then in_service$(i%) = " "
                c% = pos( "123 " = convention$(i%))
                if c% > 0% then cnv_descr$ = cnv_text$(c%)
                if inpflg% = 0% then return
                convention$(2%), convention$(3%) = convention$(1%)
                return
L50975:              errormsg$ = "Alternative Minimum Tax was Selected; T~
        ~he Table MUST have a Convention."
                cont_flag% = 1%
                return
L50990:              errormsg$ = "Table Not On File!  Press PF(4) and Sel~
        ~ect a different Group and Recovery."
                cont_flag% = 1%
                return

L51000:     REM TEST DATA FOR IN SERVICE PERIOD         IN_SERVICE$(I%)
                if enabled% = 0% then return
                cont_flag% = 1%
                if in_service$(i%) = " " then L51015
                     convert in_service$(i%) to s%, data goto L51015
                     convert s% to in_service$(i%), pic(##)
L51015:         gosub L55000
                if errormsg$ = " " then cont_flag% = 0%                  ~
                                   else cont_flag% = 1%
*       ****** IN_SERVICE$(2%), IN_SERVICE$(3%) = IN_SERVICE$(1%)
                if inpflg% = 0% then return
                return

L51100:     REM TEST DATA FOR PERCENTAGE                PERCENTAGE$(I%)
                convert depr_method$(i%) to d%, data goto L51107
L51107:         if percentage$(i%) <> " " then L51140
                if d% = 3% or d% = 4% then                               ~
                    errormsg$ = "A Percentage Is Required For Declining B~
        ~alance Depreciation"
                if d% = 8% then errormsg$ = "A Percentage Is Required For~
        ~ Depreciation Method 8."
                return
L51140:         if d% = 8% then limit = 100 else limit = 999.99
                call "NUMTEST" (percentage$(i%), 0, limit, errormsg$,    ~
                                -2.2, percentage(i%))
                if errormsg$ <> " " then return
                if inpflg% = 0% then return
                percentage(2%), percentage(3%) = percentage(1%)
                return

L51300:     REM TEST DATA FOR YEAR OF SWITCH            SWITCH_YEAR$(I%)
                switch_year$(i%) = switch$
                if inpflg% = 0% then return
                switch_year$(2%), switch_year$(3%) = switch_year$(1%)
                return

L51400:     REM TEST DATA FOR ORIGINAL COST
                if orig_basis$(i%) <> " " then L51420
                orig_basis(i%) = 0
                return
L51420:         call "NUMTEST" (orig_basis$(i%), 0, 99999999999,         ~
                                errormsg$, -2.2, orig_basis(i%))
                if errormsg$ <> " " then return
                basis = salvage_value(i%) + itc_reduct(i%)               ~
                      + bonus_depr(i%)    + exp_deduct(i%)               ~
                      + other_reduct(i%)  + accum_depr(i%)

                if orig_basis(i%) < basis then L51485
                if inpflg% = 0% then return
                orig_basis(2%), orig_basis(3%) = orig_basis(1%)
                return
L51485:         call "CONVERT" (basis, -2.2, bas$)
                errormsg$ = "ORIGINAL BASIS IS LESS THAN THE TOTAL BASIS ~
        ~REDUCTIONS (" & bas$ & ")"
                return

L51600:     REM TEST DATA FOR SALVAGE VALUE
                if salvage_value$(i%) <> " " then L51620
                salvage_value(i%) = 0
                return
L51620:         call "NUMTEST" (salvage_value$(i%), 0, 99999999999,      ~
                                errormsg$, -2.2, salvage_value(i%))
                if errormsg$ <> " " then return
                basis = orig_basis(i%)    - itc_reduct(i%)               ~
                      - bonus_depr(i%)    - exp_deduct(i%)               ~
                      - other_reduct(i%)  - accum_depr(i%)

                if salvage_value(i%) > basis then L51685
                if inpflg% = 0% then return
                salvage_value(2%), salvage_value(3%) = salvage_value(1%)
                return
L51685:         call "CONVERT" (basis, -2.2, bas$)
                errormsg$ = "SALVAGE VALUE EXCEEDS THE REMAINING BASIS ("~
        & bas$ & ")"
                return

L51800:     REM TEST DATA FOR ITC BASIS REDUCTION
*              IF ITC_REDUCT$(I%) <> " " THEN 51820
*              ITC_REDUCT(I%) = 0
*              RETURN
*              CALL "NUMTEST" (ITC_REDUCT$(I%), 0, 99999999999,         ~
*                              ERRORMSG$, -2.2, ITC_REDUCT(I%))
*              IF ERRORMSG$ <> " " THEN RETURN
*              BASIS = ORIG_BASIS(I%)    - SALVAGE_VALUE(I%)            ~
*                    - BONUS_DEPR(I%)    - EXP_DEDUCT(I%)               ~
*                    - OTHER_REDUCT(I%)  - ACCUM_DEPR(I%)
*
*              IF ITC_REDUCT(I%) > BASIS THEN 51885
*              RETURN
*
*              CALL "CONVERT" (BASIS, -2.2, BAS$)
*              ERRORMSG$ = "ITC REDUCTION EXCEEDS THE REMAINING BASIS ("~
*                          & BAS$ & ")"
                return

L52000:     REM TEST DATA FOR BONUS DEPRECIATION TAKEN
                if bonus_depr$(i%) <> " " then L52020
                bonus_depr(i%) = 0
                return
L52020:         call "NUMTEST" (bonus_depr$(i%), 0, 99999999999,         ~
                                errormsg$, -2.2, bonus_depr(i%))
                if errormsg$ <> " " then return
                basis = orig_basis(i%)    - salvage_value(i%)            ~
                      - itc_reduct(i%)    - exp_deduct(i%)               ~
                      - other_reduct(i%)  - accum_depr(i%)

                if bonus_depr(i%) > basis then L52085
                if inpflg% = 0% then return
                bonus_depr(2%), bonus_depr(3%) = bonus_depr(1%)
                return
L52085:         call "CONVERT" (basis, -2.2, bas$)
                errormsg$ ="BONUS DEPRECIATION EXCEEDS THE REMAINING BASI~
        ~S (" & bas$ & ")"
                return

L52200:     REM TEST DATA FOR EXPENSE DEDUCTION TAKEN
                if exp_deduct$(i%) <> " " then L52220
                exp_deduct(i%) = 0
                return
L52220:         call "NUMTEST" (exp_deduct$(i%), 0, 99999999999,         ~
                                errormsg$, -2.2, exp_deduct(i%))
                if errormsg$ <> " " then return
                basis = orig_basis(i%)    - salvage_value(i%)            ~
                      - itc_reduct(i%)    - bonus_depr(i%)               ~
                      - other_reduct(i%)  - accum_depr(i%)

                if exp_deduct(i%) > basis then L52285
                if exp_deduct(i%) > 10000% then errormsg$ = "The Expense ~
        ~Deduction cannot exceed $10,000 !"
                if inpflg% = 0% then return
                exp_deduct(2%), exp_deduct(3%) = exp_deduct(1%)
                return
L52285:         call "CONVERT" (basis, -2.2, bas$)
                errormsg$ = "EXPENSE DEDUCTION EXCEEDS THE REMAINING BASI~
        ~S (" & bas$ & ")"
                return

L52400:     REM TEST DATA FOR OTHER BASIS REDUCTION
                if other_reduct$(i%) <> " " then L52420
                other_reduct(i%) = 0
                other_descr$(i%) = " "
                return
L52420:         call "NUMTEST" (other_reduct$(i%), 0, 99999999999,       ~
                                errormsg$, -2.2, other_reduct(i%))
                if errormsg$ <> " " then return
                basis = orig_basis(i%)    - salvage_value(i%)            ~
                      - itc_reduct(i%)    - bonus_depr(i%)               ~
                      - exp_deduct(i%)    - accum_depr(i%)

                if other_reduct(i%) > basis then L52485
                if other_reduct(i%) <= 0 then other_descr$(i%) = " "
                if inpflg% = 0% then return
                other_reduct(2%), other_reduct(3%) = other_reduct(1%)
                return
L52485:         call "CONVERT" (basis, -2.2, bas$)
                errormsg$ = "OTHER BASIS REDUCTION EXCEEDS THE REMAINING ~
        ~BASIS (" & bas$ & ")"
                return

L52600:     REM TEST DATA FOR OTHER BASIS REDUCTION DESCRIPTION
                if inpflg% = 0 then return
                other_descr$(2%), other_descr$(3%) = other_descr$(1%)
                return

L52700:     REM TEST DATA FOR ACCUMULATED DEPRECIATION
                if accum_depr$(i%) <> " " then L52740
                accum_depr(i%) = 0
                return
L52740:         call "NUMTEST" (accum_depr$(i%), 0, 99999999999,         ~
                                errormsg$, -2.2, accum_depr(i%))
                if errormsg$ <> " " then return
                basis = orig_basis(i%)    - salvage_value(i%)            ~
                      - itc_reduct(i%)    - bonus_depr(i%)               ~
                      - exp_deduct(i%)    - other_reduct(i%)

                if accum_depr(i%) > basis then L52890
                if inpflg% = 0% then return
                accum_depr(2%), accum_depr(3%) = accum_depr(1%)
                return
L52890:         call "CONVERT" (basis, -2.2, bas$)
                errormsg$ = "ACCUMULATED DEPRECIATION EXCEEDS THE REMAINI~
        ~NG BASIS (" & bas$ & ")"
                return

L53000:     REM TEST DATA FOR CURRENT DEPRECIATION
                if current_depr$(i%) <> " " then L53040
                current_depr(i%) = 0
                return
L53040:         call "NUMTEST" (current_depr$(i%), 0, 99999999999,       ~
                                errormsg$, -2.2, current_depr(i%))
                if errormsg$ <> " " then return
                basis = orig_basis(i%)    - salvage_value(i%)            ~
                      - itc_reduct(i%)    - bonus_depr(i%)               ~
                      - exp_deduct(i%)    - other_reduct(i%)             ~
                      - accum_depr(i%)

               if current_depr(i%) > basis then L53190
               if inpflg% = 0% then return
               current_depr(2%), current_depr(3%) = current_depr(1%)
               return
L53190:        call "CONVERT" (basis, -2.2, bas$)
               errormsg$ = "CURRENT DEPRECIATION EXCEEDS REMAINING BASIS ~
        ~(" & bas$ & ")"
                return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON AMT SCREEN                    *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L54100,         /* GROUP TABLE      */~
                                    L54200,         /* CLASS LIFE       */~
                                    L54300,         /* ADJUSTED BASIS   */~
                                    L54400          /* AMT ACCUM DEPR   */
                  return

L54100:     REM TEST DATA FOR AMT GROUP TABLE NAME
                cont_flag% = 1%
                if group$(4%) = " " then L54140
                call "STRING" addr("LJ", group$(4%), 10%)
L54140:         gosub L57000
                if group$(4%) = " " then                                 ~
                                errormsg$ = "A SELECTION MUST BE MADE!"
                return

L54200:     REM TEST DATA FOR AMT CLASS LIFE
                if amt% = 1% and life$(4%) = " " then L54230
                convert life$(4%) to life, data goto L54265
                if amt% = 2% and life < 1% or life > 99% then L54245
                if amt% = 1% and life < 1% or life > 60% then L54255
                convert life to life$(4%), pic(##.00)
                if amt% > 1% then L54235
L54230:         gosub L57000
L54235:            if errormsg$ = " " then cont_flag% = 0%
                return
L54245:         errormsg$ = "VALID LIFE: 1 - 99 YEARS"
                return
L54255:         errormsg$ = "VALID CLASS LIFE: 1 - 60 YEARS"
                return
L54265:         errormsg$ = "INVALID NUMBER"
                return

L54300:     REM TEST DATA FOR AMT ADJUSTED BASIS
                call "NUMTEST" (orig_basis$(4%), 0, 9e9, errormsg$,      ~
                                -2.2, orig_basis(4%))
                amt_inputmode_flag% = 0%
                return

L54400:     REM TEST DATA FOR AMT ACCUMULATED DEPRECIATION
                call "NUMTEST" (accum_depr$(4%), 0, 9e9, errormsg$,      ~
                                -2.2, accum_depr(4%))
                return


L55000: REM *************************************************************~
            *  P L O W C O D E   C O N T R O L   F O R   F A T A B L E  *~
            *************************************************************

            temp$ = service_date$(i%)
            call "DATUFMTC" (temp$)
            if fieldnr% = 2% and e% = 2% then L55080
            if fieldnr% < 3% then return
            if e% = 1% and fieldnr% = 5% and convention$(i%) = " " then  ~
                                                              goto L55080
            if e% = 1% then L55130
L55080:        plowkey$ = str(group$(i%))      & str(life$(i%)) &        ~
                          str(convention$(i%)) & str(in_service$(i%))
               call "READ100" (#02, plowkey$, f1%(2))
                     if f1%(2) = 1% then L55850
                     if fieldnr% = 2% then return
L55130:     incl$(), hdr$() = " "
            mat incl = zer
            key_descr = 0.18
            mat descr_map = zer
*          F1%(2%) = -(FIELDNR% + 4%)
*                     12345678901234567890123456789012345678901234567890
            hdr$(1) = "  Group     Recovery  Convention  Period  Property~
        ~-Types  Start-Date"

            on fieldnr% - 2% goto L55240, L55310, L55380, L55460
               return
L55240:     i$(1) = hex(06) & "Please Select the Group Table Name"
            plowkey$ = group$(i%) & hex(00)
            str(hdr$(1),13,29) = " "
            lngth = 0
            break% = -9010%
            key_descr = -0.001
                goto L55530
L55310:     i$(1) = hex(06) & "Please Select the Recovery (Years) Version~
        ~ of the Table."
            plowkey$ = str(group$(i%)) & life$(i%) & hex(00)
            str(hdr$(1),23,19) = " "
            lngth = 5
            break% = 9010%
                goto L55530
L55380:     i$(1) = hex(06) & "Please Select the Proration Convention Tab~
        ~le."
            plowkey$ = str(group$(i%)) & str(life$(i%)) &                ~
                       str(convention$(i%)) & hex(00)
            str(hdr$(1),35, 6) = " "
            lngth = 1
            break% = 9015%
                goto L55530
L55460:     i$(1) = hex(06) & "Please Select the In Service Period from T~
        ~able."
            plowkey$ = str(group$(i%)) & str(life$(i%)) &                ~
                       str(convention$(i%)) & in_service$(i%)
            lngth = 2
            break% = 9016%

L55530
*          IF E% <> 1% THEN 55630
               b% = abs(break%) - 9000% + lngth
               plowkey2$ = plowkey$
               if break% < 0% then plowkey2$ = str(group$(i%)) & hex(00)
            if fieldnr% = 6% then call "READ100" (#02, plowkey2$, f1%(2))~
               else call "PLOWNEXT" (#02, plowkey2$, b%, f1%(2))
                     if f1%(2) = 0% then L55630
               if fieldnr% = 6% then gosub L55850
               if errormsg$ <> " " then L55630
               return                              /* FOUND ONE */

L55630:     descr_map(1) =  1.10  :  descr_map(2) =  1.0
         if fieldnr% < 4% then L55730
            descr_map(3) = 11.05 : descr_map(4) = 13.0

         if fieldnr% < 5% then L55730
            descr_map(5) = 16.01 : descr_map(6) = 25.0

         if fieldnr% < 6% then L55730
            descr_map(7) = 17.02 : descr_map(8) = 35.0

L55730:     descr_map(9) =  275.07  :  descr_map(10) = 44.0
            descr_map(11) =  19.081 :  descr_map(12) = 58.0
            errormsg$ = " "
            call "PLOWCODE" (#02, plowkey$, i$(1%), break%, key_descr,   ~
                             f1%(2%), hdr$(), lngth, 1, incl(), incl$(), ~
                             "d", "Y", #64, descr_map())

            if f1%(2%) = 1% then L55830
               errormsg$ = "A SELECTION MUST BE MADE!"
               return
L55830:     on fieldnr% - 2% gosub L55970, L56000, L56030, L56060
            if fieldnr% = 5% and convention$(i%) = " " then L55850
            if fieldnr% <> 6% then return
L55850:     get #02 using L55860, begdate$, enddate$, proptypes$
L55860:         FMT POS(19), 2*CH(8), POS(275), CH(7)
            s1%, s2% = 0%
            if enddate$ = " " or enddate$ = blankdate$ then enddate$ = hex(ff)
            if temp$ < begdate$ or temp$ > enddate$ then s1% = 1%
            s2% = pos(proptypes$ = property_type$)
            if s1% = 1% and s2% = 0% then L56100
            if s1% = 1% then L56140
            if s2% = 0% then L56180
            cont_flag% = 0%
            return

L55970:     get #02 using L55980, group$(i%)
L55980:         FMT CH(10)
            return
L56000:     get #02 using L56010, life$(i%)
L56010:         FMT POS(11), CH(5)
            return
L56030:     get #02 using L56040, convention$(i%)
L56040:         FMT POS(16), CH(1)
            return
L56060:     get #02 using L56070, in_service$(i%)
L56070:         FMT POS(17), CH(2)
            return

L56100:     errormsg$ = "The SERVICE DATE and The PROPERTY TYPE are NOT S~
        ~upported by This Table !"
            return

L56140:     errormsg$ = "The SERVICE DATE is NOT Within the Effective Ran~
        ~ge of This Table !"
            return

L56180:     errormsg$ = "The PROPERTY TYPE of This Asset is NOT Supported~
        ~ by This Table !"
            return


L57000: REM *************************************************************~
            *  P L O W C O D E   C O N T R O L -   A M T   S C R E E N  *~
            *************************************************************

            if fieldnr% > 2% then return
            temp$ = service_date$(2%)
            call "DATUFMTC" (temp$)

            if e% = 1% and fieldnr% = 1% then L57150
               plowkey$ = str(group$(4%))      & str(life$(4%)) &        ~
                          str(convention$(2%)) & str(in_service$(2%))
               call "READ100" (#02, plowkey$, f1%(2))
                     if f1%(2) = 1% then L57880

L57150:     incl$(), hdr$() = " "
            mat incl = zer
            key_descr = 0.18
            mat descr_map = zer
            hdr$(1) = "  Group     Recovery  Convention  Period  Property~
        ~-Types  Start-Date"

            on fieldnr% goto L57260, L57330
               return
L57260:     i$(1) = hex(06) & "Please Select the Group Table Name"
            plowkey$ = group$(4%) & hex(00)
            str(hdr$(1),13,29) = " "
            lngth = 0
            break% = -9010%
            key_descr = -0.001
                goto L57400

L57330:     i$(1) = hex(06) & "Please Select the Recovery (Years) Version~
        ~ of the Table."
            plowkey$ = str(group$(4%)) & life$(4%) & hex(00)
*          STR(HDR$(1),23,19) = " "
            lngth = 5
            break% = 9010%

L57400:     incl(1) = 16.01  :  incl$(1) = convention$(2%)
            incl(2) = 17.02  :  incl$(2) = in_service$(2%)

            if fieldnr% = 1% then L57660
               b% = abs(break%) - 9000% + lngth
               plowkey2$ = plowkey$
*             IF BREAK% < 0% THEN PLOWKEY2$ = STR(GROUP$(4%)) & HEX(00)
*          IF FIELDNR% = 2% THEN CALL "READ100" (#02, PLOWKEY2$, F1%(2))~
*             ELSE CALL "PLOWNEXT" (#02, PLOWKEY2$, B%, F1%(2))
               call "READ100" (#02, plowkey2$, f1%(2))
                     if f1%(2) = 0% then L57660
*             IF FIELDNR% = 2% THEN GOSUB 57880
               gosub L57880
               if errormsg$ <> " " then L57660
               return                              /* FOUND ONE */

L57660:     descr_map(1) =  1.10  :  descr_map(2) =  1.0
            if fieldnr% < 2% then L57760
               descr_map(3) = 11.05 :  descr_map(4)  = 13.0
               descr_map(5) = 16.01 :  descr_map(6)  = 25.0
               descr_map(7) = 17.02 :  descr_map(8)  = 35.0
L57760:     descr_map(9)   = 275.07 :  descr_map(10) = 44.0
            descr_map(11)  =  19.08 :  descr_map(12) = 58.0
            errormsg$ = " "
            call "PLOWCODE" (#02, plowkey$, i$(1%), break%, key_descr,   ~
                             f1%(2%), hdr$(), lngth, 1, incl(), incl$(), ~
                             "d", "Y", #64, descr_map())

            if f1%(2%) = 1% then L57860
               errormsg$ = "Select an AMT table that matches the Federal ~
        ~Convention & In Service Period."
               return
L57860:     on fieldnr% gosub L58000, L58030
            if fieldnr% <> 2% then return
L57880:     get #02 using L57890, begdate$, enddate$, proptypes$
L57890:         FMT POS(19), 2*CH(8), POS(275), CH(7)
            s1%, s2% = 0%
            if enddate$ = " " or enddate$ = blankdate$ then enddate$ = hex(ff)
            if temp$ < begdate$ or temp$ > enddate$ then s1% = 1%
            s2% = pos(proptypes$ = property_type$)
            if s1% = 1% and s2% = 0% then L58130
            if s1% = 1% then L58170
            if s2% = 0% then L58210
            cont_flag% = 0%
            return

L58000:     get #02 using L58010, group$(4%)
L58010:         FMT CH(10)
            return
L58030:     get #02 using L58040, life$(4%)
L58040:         FMT POS(11), CH(5)
            return

L58130:     errormsg$ = "The SERVICE DATE and The PROPERTY TYPE are NOT S~
        ~upported by This Table !"
            return

L58170:     errormsg$ = "The SERVICE DATE is NOT Within the Effective Ran~
        ~ge of This Table !"
            return

L58210:     errormsg$ = "The PROPERTY TYPE of This Asset is NOT Supported~
        ~ by This Table !"
            return


        REM *************************************************************~
            *        VERIFY EFFECTIVE DATE RANGE & PROPERTY TYPES       *~
            *************************************************************

        check_depr_table

            plowkey$ = str(group$(i%)) & str(life$(i%))                  ~
                     & str(convention$(i%)) & str(in_service$(i%))
            t% = 0%
            call "READ100" (#02, plowkey$, f1%(2))
                if f1%(2) = 0% then L60460          /* ERROR */
            temp$ = service_date$(i%)
            call "DATUFMTC" (temp$)
L59130:     get #02 using L59140, begdate$, enddate$, proptypes$
L59140:         FMT POS(19), 2*CH(8), POS(275), CH(7)
            s1%, s2% = 0%
            if enddate$ = " " or enddate$ = blankdate$ then enddate$ = hex(ff)
            if temp$ < begdate$ or temp$ > enddate$ then s1% = 1%
            s2% = pos(proptypes$ = property_type$)
            if s1% = 1% and s2% = 0% then L58130    /* ERROR OUT */
            if s1% = 1% then L58170                 /* ERROR OUT */
            if s2% = 0% then L58210                 /* ERROR OUT */
            if i% = 2% and amt% = 1% and t% = 0% then L59250
            return

L59250:     plowkey$ = str(group$(4%)) & str(life$(4%))                  ~
                     & str(convention$(2%)) & str(in_service$(2%))

            call "READ100" (#02, plowkey$, f1%(2))
                if f1%(2) = 0% then L61500
            t% = 1%
            goto L59130


L60000: REM SUBROUTINE TO CALCULATE THE CURRENT YEAR'S DEPRECIATION

            servdate$ = service_date$(i%)
            call "DATUFMTC" (servdate$)
            call "FADEPR"     (asset_code$,                              ~
                               property_type$,                           ~
                               dispdate$,                                ~
                               servdate$,                                ~
                               group$(i%),                               ~
                               convention$(i%),                          ~
                               in_service$(i%),                          ~
                               life$(i%),                                ~
                               depr_method$(i%),                         ~
                               percentage(i%),                           ~
                               switch_year$(i%),                         ~
                               orig_basis(i%),                           ~
                               salvage_value(i%),                        ~
                               itc_reduct(i%),                           ~
                               bonus_depr(i%),                           ~
                               exp_deduct(i%),                           ~
                               other_reduct(i%),                         ~
                               accum_depr(i%),                           ~
                               current_depr(i%),                         ~
                               fiscal_begin$,                            ~
                               fiscal_end$,                              ~
                               ret%,                                     ~
                               #02)

            current_depr$(i%) = " "
*          IF CURRENT_DEPR(I%) = 0 THEN 60231
            call "CONVERT" (current_depr(i%), 2.2, current_depr$(i%))

            convert depr_method$(i%) to d%, data goto L60330
            deprdescr$ = deprmeth$(d%)
L60330:     method$ = depr_method$(i%)
            switch$ = switch_year$(i%)
         if old_depr(i%) <> current_depr(i%) then                        ~
            old_deprdescr$(i%) = "(Original Value: " & old_depr$(i%)     ~
                                                                   & ")" ~
                else old_deprdescr$(i%) = " "

            if ret% = 0% or e% = 1% then return
            if ret% <> 1% then L60450
                errormsg$ = "LIFE IS INVALID PLEASE REENTER."
                fieldnr% = 4%
                cont_flag% = 1%
                return clear all
                goto L11600
L60450:     if ret% <> 2% then L60510
L60460:         errormsg$ = "DEPRECIATION TABLE NO LONGER ON FILE, PLEASE~
        ~ ENTER A VALID TABLE."
                fieldnr% = 3%
                cont_flag% = 1%
                return clear all
                goto L11600
L60510:     return


L61000: REM SUBROUTINE TO CALCULATE ALTERNATIVE MINIMUM TAX ADJUSTMENT

            if keyhit% = 99% then life$(4%) = " "
            if life$(4%) = " " then return
            method$ = " "
            if amti_flag$ = "1" then method$ = "7"
            if amti_flag$ = "2" then method$ = "S"
            if method$ = " " then return
            servdate$ = service_date$(2%)
            call "DATUFMTC" (servdate$)

            call "FADEPR"     (asset_code$,                              ~
                               property_type$,                           ~
                               dispdate$,                                ~
                               servdate$,                                ~
                               group$(4%),                               ~
                               convention$(2%),                          ~
                               in_service$(2%),                          ~
                               life$(4%),                                ~
                               method$,                                  ~
                               0,                                        ~
                               " ",                                      ~
                               orig_basis(4%),                           ~
                               0,                                        ~
                               0,                                        ~
                               0,                                        ~
                               0,                                        ~
                               0,                                        ~
                               accum_depr(4%),                           ~
                               current_depr(4%),                         ~
                               fiscal_begin$,                            ~
                               fiscal_end$,                              ~
                               ret%,                                     ~
                               #02)

            current_depr$(4%) = " "
            call "CONVERT" (current_depr(4%), 2.2, current_depr$(4%))

            amt_adjustment = current_depr(2%) - current_depr(4%)
            call "CONVERT" (amt_adjustment, 2.2, amt_adjustment$)

            if old_depr(4%) <> current_depr(4%) then                     ~
              old_deprdescr$(4%) = "(Original Value: " & old_depr$(4%)   ~
                                                                   & ")" ~
              else   old_deprdescr$(4%) = " "

            if ret% = 0% then return
            if ret% <> 1% then L61490
                errormsg$ = "LIFE IS INVALID PLEASE REENTER."
                fieldnr% = 2%
                return clear all
                goto L13630
L61490:     if ret% <> 2% then L61540
L61500:         errormsg$ = "DEPRECIATION TABLE NO LONGER ON FILE, PLEASE~
        ~ ENTER A VALID TABLE."
                fieldnr% = 1%
                return clear all
                goto L13630
L61540:     return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN


            end
