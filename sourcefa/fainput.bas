        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   IIIII  N   N  PPPP   U   U  TTTTT          *~
            *  F      A   A    I    NN  N  P   P  U   U    T            *~
            *  FFFF   AAAAA    I    N N N  PPPP   U   U    T            *~
            *  F      A   A    I    N  NN  P      U   U    T            *~
            *  F      A   A  IIIII  N   N  P       UUU     T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAINPUT  - THIS PROGRAM ALLOWS THE USER TO ENTER AND EDIT *~
            *            FIXED ASSETS.  AFTER CHANGING FIELDS FOR AN    *~
            *            ASSET, THE PROGRAM WILL RECALCULATE THE CURRENT*~
            *            YEAR'S DEPRECIATION.                           *~
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
            * 05/01/84 ! ORIGINAL                                 ! NHC *~
            * 10/04/85 ! Changed VENDOR File Format               ! MJB *~
            * 09/06/88 ! Standardized, Added Property Types,      ! RJM *~
            *          !  File changes to FAMASTER & FATABLE.     ! RJM *~
            * 09/20/88 ! Now will NOT allow Blank Asset Code      ! RJM *~
            * 09/26/88 ! Added Alternative Minimum Tax Mods.      ! RJM *~
            * 12/06/89 ! Allow Modification of Property Type.     ! TLJ *~
            * 08/28/89 ! Corrected target of GOTO @ ln 11261      ! MJB *~
            * 08/06/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            accum_depr(4),               /* ACCUMULATED DEPRECIATION   */~
            amti_flag$1,                 /* ALT. MINIMUM TAX TYPE FLAG */~
            asset_acct$16,               /* ASSET G/L ACCOUNT #        */~
            asset_acctdescr$32,          /* ASSET G/L ACCOUNT #        */~
            asset_code$10,               /* ASSET CODE                 */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            book$(4)12,                  /* Description Of Books       */~
            bonus_depr(3),               /* BONUS DEPRECIATION TAKEN   */~
            cnv_dflt$1,                  /* CONVENTION DEFAULT         */~
            convention$(3)1,             /* PRORATION CONVENTION       */~
            convention$1,                /* PRORATION CONVENTION       */~
            current_depr(4),             /* CURRENT YEAR'S DEPR        */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            depr_acct$16,                /* ACCUM DEPR G/L ACCOUNT #   */~
            depr_acctdescr$32,           /* ACCUM DEPR G/L ACCOUNT #   */~
            depr_method$(3)2,            /* DEPRECIATION METHOD        */~
            descr_1$30,                  /* ASSET DESCRIPTION #1       */~
            descr_2$30,                  /* ASSET DESCRIPTION #2       */~
            dispdate$10,                 /* DISPOSAL DATE int fmt      */~
            disposal_date$10,            /* DISPOSAL DATE FORMATTED    */~
            disposal_descr$30,           /* DISPOSAL DESCRIPTION       */~
            disposal_price$11,           /* DISPOSAL PRICE             */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            exp_acct$16,                 /* EXPENSE G/L ACCOUNT #      */~
            exp_acctdescr$32,            /* EXPENSE G/L ACCOUNT #      */~
            exp_deduct(3),               /* EXPENSE DEDUCTION TAKEN    */~
            fdate$10,                    /* FISCAL YR END MM/DD/YYYY   */~
            fiscal_begin$8,              /* FISCAL YR BEGINNING DATE   */~
            fiscal_end$8,                /* FISCAL YR ENDING DATE      */~
            group$(4)10,                 /* GROUP TABLE NAME           */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            id_code$15,                  /* IDENTIFICATION CODE        */~
            in_service$(3)2,             /* PERIOD IN PUT IN SERVICE   */~
            inservice$2,                 /* PERIOD IN PUT IN SERVICE   */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            inpmessage1$79,              /* INPUT MESSAGE              */~
            invoice_number$16,           /* INVOICE NUMBER             */~
            itc_reduct(3),               /* ITC REDUCTION              */~
            itc_taken$11,                /* INVESTMENT TAX CREDIT TAKEN*/~
            last_asset$10,               /* LAST ASSET READ            */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            life$(4)5,                   /* LIFE IN YEARS              */~
            line2$79,                    /* Screen Header              */~
            location$30,                 /* LOCATION                   */~
            method$2,                    /* METHOD FOR AMT             */~
            old_depr(4),                 /* ORIGINAL DEPRECIATION      */~
            orig_basis(4),               /* ORIGINAL BASIS             */~
            other_descr$(3)50,           /* OTHER BASIS REDUCT DESCR   */~
            other_reduct(3),             /* OTHER REDUCTION            */~
            percentage(3),               /* PERCENT FOR MANUAL OR DB   */~
            pf$(3)79,                    /* PF KEY TEXT                */~
            pfkey$32,                    /* VALID PF KEYS              */~
            plowkey$99,                  /* MISC. READ/PLOW KEY        */~
            property_type$1,             /* PROPERTY TYPE              */~
            propertydescr$(7)20,         /* PROPERTY TYPE DESCRIPTION  */~
            propdescr$20,                /* PROPERTY TYPE DESCRIPTION  */~
            purdate$10,                  /* PURCHASE DATE (YYYYMMDD)   */~
            purch_date$10,               /* PURCHASE DATE FORMATTED    */~
            purch_price$11,              /* PURCHASE PRICE             */~
            salvage_value(3),            /* SALVAGE VALUE              */~
            servdate$(3)10,              /* SERVICE DATE (YYYYMMDD)    */~
            switch_year$(3)4,            /* YEAR OF SWITCH             */~
            testdate$8,                  /* test date                  */~
            types$7,                     /* Types read in from FATABLE */~
            typedescr$32,                /* GROUP, SUBGROUP, ASSET     */~
            typecodedescr$(3)32,         /* GROUP, SUBGROUP, ASSET     */~
            type_code$1,                 /* TYPE CODE                  */~
            vendor_code$9,               /* VENDOR CODE                */~
            vendor_codedescr$32          /* VENDOR CODE                */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

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
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #02 ! FATABLE  ! Fixed Assets Depreciation Percentages    *~
            * #04 ! SYSFILE2 ! Caelus Management System Information     *~
            * #05 ! VENDOR   ! Vendor Master File                       *~
            * #06 ! GLMAIN   ! General Ledger Main File                 *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "FAMASTER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 120,  keylen = 10,                      ~
                        alt key  1, keypos = 58, keylen =   1, dup

            select #02, "FATABLE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1,    keylen = 18


            select #04, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1,    keylen = 20                       ~

            select #05, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1,    keylen = 9,                       ~
                        alt key  1,    keypos = 10, keylen = 30, dup

            select #06, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1,    keylen = 9                        ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, 0%, f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#02, 0%, f2%(2 ), 0%, rslt$(2 ))
            if f2%(2) = 0% then L02570
                call "ASKUSER" (0%, "FILE NOT FOUND!",                   ~
                            "THE ACRS DEPRECIATION TABLES",              ~
                            "HAVE NOT BEEN ESTABLISHED.",                ~
                            "PLEASE PRESS (ENTER) TO END.")
                goto L65000
L02570:     call "OPENCHCK" (#04, 0%, f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#05, 0%, f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#06, 0%, f2%(6 ), 0%, rslt$(6 ))
            if f2%(1) = 0 then L09000
            call "OPENFILE" (#01,  "OUTPT", f2%(1 ), rslt$(1 ), axd$(1 ))
            close #01
            call "OPENCHCK" (#01, 0%, f2%(1 ), 0%, rslt$(1 ))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            typecodedescr$(1%) = "(GROUP)"
            typecodedescr$(2%) = "(SUBGROUP)"
            typecodedescr$(3%) = "(ASSET)"

            propertydescr$(1%) = "(PERSONAL PROPERTY)"
            propertydescr$(2%) = "(REAL ESTATE)"
            propertydescr$(3%) = "(LOW INCOME HOUSING)"
            propertydescr$(4%) = "(AMORTIZED PROPERTY)"
            propertydescr$(5%) = "(LEASED PROPERTY)"
            propertydescr$(6%) = "(RESIDENTIAL RENTAL)"
            propertydescr$(7%) = "(OTHER)"

            book$(1) = "BOOK DEPR"
            book$(2) = "FED TAX DEPR"
            book$(3) = "STATE/LOCAL DEPR"
            book$(4) = "AMTI"

        REM GET FISCAL BEGINNING AND ENDING DATES FROM "SYSFILE2"
            call "READ100" (#04, "SWITCHS.FA", f1%(4))
            if f1%(4) <> 0 then L09610

                call "ASKUSER" (0%, "RECORD NOT FOUND",                  ~
                                    "Fixed Assets Behavior Switches and",~
                                    "Fiscal Dates Have Not Been Set",    ~
                                    "Press Any Key To Abort Program.")
                goto L65000

L09610:     get #04, using L09630, fiscal_begin$, fiscal_end$, cnv_dflt$
L09630:             FMT XX(20), 2*CH(8), CH(1)
            fdate$ = fiscal_end$
            call "DATFMTC" (fdate$)

            asset_code$ = " "
            put str(line2$,,79) using L09910, asset_code$, fdate$,         ~
                                            str(cms2v$,,8)
L09910: %Asset Code: ##########     Fiscal Yr End: ##########          FA~
        ~INPUT: ########


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************
        inputmode
            last_asset$ = asset_code$ & " "
            purch_price, itc_taken, disposal_price = 0
            mat current_depr = zer
            mat percentage = zer
            mat orig_basis = zer
            mat salvage_value = zer
            mat itc_reduct = zer
            mat bonus_depr = zer
            mat exp_deduct = zer
            mat other_reduct = zer
            mat accum_depr = zer
            init(" ") errormsg$, inpmessage$, propdescr$, typedescr$,    ~
                      asset_code$, descr_1$, descr_2$, type_code$,       ~
                      id_code$, purch_price$, amti_flag$,                ~
                      property_type$, itc_taken$, location$, group$(),   ~
                      disposal_price$, disposal_descr$,                  ~
                      vendor_code$,   vendor_codedescr$, convention$(),  ~
                      invoice_number$,                                   ~
                      asset_acct$, asset_acctdescr$, depr_acct$,         ~
                      depr_acctdescr$, exp_acct$, exp_acctdescr$,        ~
                      life$(), depr_method$(),                           ~
                      switch_year$(), other_descr$(), in_service$(),     ~
                      inpmessage1$
            init( blankdate$ ) purch_date$, disposal_date$, purdate$,    ~
                               servdate$(), dispdate$
            t% = 0
            p% = 0
            str(line2$, 13, 10) = " "
L10090:     for fieldnr% = 1% to 14%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10180
L10120:         gosub'101(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10140
L10133:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10090
                         gosub'051(fieldnr%)
                         if enabled% = 0% then L10133
                         goto L10120
L10140:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10120
                gosub'151(fieldnr%,0%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

L10200:     for fieldnr% = 1% to 5%
                gosub'052(fieldnr%)
                      if enabled% = 0% then L10280
L10230:         gosub'102(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10250
L10242:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10200
                         gosub'052(fieldnr%)
                         if enabled% = 0% then L10242
                         goto L10230
L10250:               if keyhit% <>  0% then       L10230
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10230
L10280:         next fieldnr%
                i% = 1%
                keyhit% = 0%
            goto L12000

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        edtpg1
            init(" ")inpmessage$, inpmessage1$
            gosub'101(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if type_code$ = "1" or type_code$ = "2" then L11110
                  if keyhit%  =  5% then       edtpg2
                  if keyhit% < 6% or keyhit% > 10% then L11110
                      if keyhit% = 9% then edtpg1
                      if keyhit%  = 10% and amti_flag$ = " " then edtpg1
                      i% = keyhit% - 5%
                      goto L12000
L11110:           if keyhit% <>  0% then       edtpg1
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 2% or fieldnr% > 14% then edtpg1
            if (type_code$ = "1" or type_code$ = "2") and                ~
               (fieldnr% < 1% or fieldnr% > 4%) then edtpg1

L11141:     gosub'051(fieldnr%)
                  if enabled% = 0% then edtpg1
L11150:     gosub'101(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11150
            gosub'151(fieldnr%,1%)
                  if errormsg$ <> " " then L11150
            goto edtpg1

        edtpg2
L11230:     gosub'102(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       edtpg1
                  if keyhit%  = 16% then       datasave
                  if type_code$ = "1" or type_code$ = "2" then L11230
                  if keyhit% < 6% or keyhit% > 10% then L11270
                      if keyhit%  = 9% then edtpg2
                      if keyhit%  = 10% and amti_flag$ = " " then edtpg2
                      i% = keyhit% - 5%
                      goto L12000
L11270:           if keyhit% <>  0% then       edtpg2
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% > 5% then edtpg2
            gosub'052(fieldnr%)
L11310:     gosub'102(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11310
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11310
            goto L11230

L12000: REM GO TO SUBROUTINE TO EDIT DEPRECIATION BOOKS
            gosub L36000
            if keyhit% <> 99% then L12030  /* Return Code from FASUB=99 */
                fieldnr% = 14%            /* if AMT screen skip'd.     */
                errormsg$ = "Enter Blank, or Press Return To Continue Wit~
        ~h Input of Alt Min Tax Depr Data"
                goto L11141
L12030:     if keyhit% =  1% then inputmode
            if keyhit% = 16% then datasave
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L32000
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  inpmessage$, inpmessage1$ = " "
                  on fieldnr% gosub L20100,         /* ASSET CODE       */~
                                    L20150,         /* DESCRIPTION #1   */~
                                    L20200,         /* DESCRIPTION #2   */~
                                    L20250,         /* TYPE CODE        */~
                                    L20300,         /* ID CODE          */~
                                    L20350,         /* PURCHASE DATE    */~
                                    L20400,         /* PURCHASE PRICE   */~
                                    L20450,         /* PROPERTY TYPE    */~
                                    L20500,         /* ITC TAKEN        */~
                                    L20550,         /* LOCATION         */~
                                    L20600,         /* DISPOSAL DATE    */~
                                    L20650,         /* DISPOSAL PRICE   */~
                                    L20700,         /* DISPOSAL DESCR   */~
                                    L20750          /* AMT FLAG         */
                     return
L20100:     REM DEFAULT/ENABLE FOR ASSET CODE
                inpmessage$ = "PRESS (ENTER) TO FIND ASSET CODES"
                return
L20150:     REM DEFAULT/ENABLE FOR ASSET DESCRIPTION #1
                return
L20200:     REM DEFAULT/ENABLE FOR ASSET DESCRIPTION #2
                return
L20250:     REM DEFAULT/ENABLE FOR TYPE CODE
                inpmessage$ =  "VALID TYPE CODES:  1=GROUP, 2=SUBGROUP, 3~
        ~=ASSET"
                type_code$ = "3"
                return
L20300:     REM DEFAULT/ENABLE FOR IDENTIFICATION CODE
                inpmessage$ = "Enter Your Identifiction Code for this Ass~
        ~et.  This is just a Memo field."
                return
L20350:     REM DEFAULT/ENABLE FOR PURCHASE DATE
                inpmessage$ = "Enter the Date of Purchase."
                if purch_date$ <> " " and purch_date$ <> blankdate$ then return
                purch_date$ = date
                call "DATFMTC" (purch_date$)
                call "DATEOKC" (purch_date$, u3%, errormsg$)
                return
L20400:     REM DEFAULT/ENABLE FOR PURCHASE PRICE
                inpmessage$ = "Enter the Original Purchase Price for this~
        ~ Asset."
                call "CONVERT" (purch_price, -2.2, purch_price$)
                if purch_price = 0 then purch_price$ = " "
                return
L20450:     REM DEFAULT/ENABLE FOR PROPERTY TYPE
                inpmessage1$ = "VALID TYPES:  P=Personal Property, R=Real~
        ~ Estate, L=Low Income Housing"
                inpmessage$  = "A=Amortized Property, E=Leased Property, ~
        ~X=Residential Rental, O=Other"
                property_type$ = "P"
                return
L20500:     REM DEFAULT/ENABLE FOR INVESTMENT TAX CREDIT TAKEN
                call "CONVERT" (itc_taken, -2.2, itc_taken$)
                inpmessage$ = "Enter Investment Tax Credit OR a number fo~
        ~llowed by a % to have it Calculated."
                testdate$ = "19860101"
                call "DATECONV" (testdate$)
                if purdate$ >= testdate$ then enabled% = 0%
                if enabled% = 0% or itc_taken = 0 then itc_taken$ = " "
                return
L20550:     REM DEFAULT/ENABLE FOR LOCATION
                inpmessage$ = "Enter the Location of the Asset, this is j~
        ~ust a Memo field."
                return
L20600:     REM DEFAULT/ENABLE FOR DISPOSAL DATE
                inpmessage$ = "Enter the Disposal Date, if any. Any date ~
        ~here will effect Depr on Final Year."
                return
L20650:     REM DEFAULT/ENABLE FOR DISPOSAL PRICE
                if disposal_date$ = " " or disposal_date$ = blankdate$   ~
                                            then enabled% = 0%
                disposal_price$ = " "
                if enabled% = 1% then                                    ~
                   call "CONVERT" (disposal_price, -2.2, disposal_price$)
                inpmessage$ = "Enter the Amount expected to receive upon ~
        ~Disposal of this Asset."
                return
L20700:     REM DEFAULT/ENABLE FOR DISPOSAL DESCRIPTION
                if disposal_date$ = " " or disposal_date$ = blankdate$   ~
                                        then enabled% = 0%
                if enabled% = 0% then disposal_descr$ = " "
                return
L20750:     REM DEFAULT/ENABLE FOR ALTERNATE MINIMUM TAX TYPE FLAG
                inpmessage$ = "'1' = Alternative Minimum Tax Table, '2' =~
        ~ Tax Preference Item, or Blank."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  inpmessage$ = " "
                  on fieldnr% gosub L21100,         /* VENDOR CODE      */~
                                    L21200,         /* INVOICE NUMBER   */~
                                    L21300,         /* ASSET ACCOUNT    */~
                                    L21400,         /* DEPR ACCOUNT     */~
                                    L21500          /* EXPENSE ACCOUNT  */
                     return
L21100:     REM DEFAULT/ENABLE FOR VENDOR CODE
                return
L21200:     REM DEFAULT/ENABLE FOR INVOICE NUMBER
                return
L21300:     REM DEFAULT/ENABLE FOR ASSET G/L ACCOUNT #
                return
L21400:     REM DEFAULT/ENABLE FOR ACCUM DEPR G/L ACCOUNT #
                return
L21500:     REM DEFAULT/ENABLE FOR EXPENSE G/L ACCOUNT #
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
            goto inputmode

L30000: REM *************************************************************~
            *          LOAD DATA FROM THE FIXED ASSETS FILE             *~
            *                                                           *~
            *************************************************************

            mat old_depr = zer

            get #01, using L35300,                                        ~
                     depr_method$(),                                     ~
                     servdate$(),                                        ~
                     exp_acct$,                                          ~
                     depr_acct$,                                         ~
                     asset_acct$,                                        ~
                     property_type$,                                     ~
                     dispdate$,                                          ~
                     purdate$,                                           ~
                     location$,                                          ~
                     id_code$,                                           ~
                     asset_code$,                                        ~
                     descr_1$,                                           ~
                     descr_2$,                                           ~
                     type_code$,                                         ~
                     purch_price,                                        ~
                     itc_taken,                                          ~
                     disposal_price,                                     ~
                     disposal_descr$,                                    ~
                     vendor_code$,                                       ~
                     invoice_number$,                                    ~
                     group$(1),                                          ~
                     group$(2),                                          ~
                     group$(3),                                          ~
                     life$(1),                                           ~
                     life$(2),                                           ~
                     life$(3),                                           ~
                     convention$(),                                      ~
                     percentage(),                                       ~
                     switch_year$(),                                     ~
                     orig_basis(1),                                      ~
                     orig_basis(2),                                      ~
                     orig_basis(3),                                      ~
                     salvage_value(),                                    ~
                     itc_reduct(),                                       ~
                     bonus_depr(),                                       ~
                     exp_deduct(),                                       ~
                     other_reduct(),                                     ~
                     other_descr$(),                                     ~
                     accum_depr(1),                                      ~
                     accum_depr(2),                                      ~
                     accum_depr(3),                                      ~
                     current_depr(1),                                    ~
                     current_depr(2),                                    ~
                     current_depr(3),                                    ~
                     in_service$(),                                      ~
                     amti_flag$,                                         ~
                     group$(4),                                          ~
                     life$(4),                                           ~
                     orig_basis(4),                                      ~
                     current_depr(4),                                    ~
                     accum_depr(4)


        REM FORMAT DATA IN FILE TO DISPLAY ON SCREEN

         if vendor_code$ = " " then L30510
            call "DESCRIBE" (#05, vendor_code$, vendor_codedescr$, 1%,   ~
                             f1%(5))
            if f1%(5) = 0 then vendor_codedescr$="(VENDOR NOT ON FILE)"

L30510:  if asset_acct$ = " " then L30550
            call "DESCRIBE" (#06, asset_acct$,asset_acctdescr$,1%,f1%(6))
            if f1%(6) = 0 then asset_acctdescr$="(ACCOUNT # NOT ON FILE)"
            call "GLFMT" (asset_acct$)
L30550:  if depr_acct$ = " " then L30590
            call "DESCRIBE" (#06, depr_acct$,depr_acctdescr$,1%,f1%(6))
            if f1%(6) = 0 then depr_acctdescr$="(ACCOUNT # NOT ON FILE)"
            call "GLFMT" (depr_acct$)
L30590:  if exp_acct$ = " " then L30630
            call "DESCRIBE" (#06, exp_acct$,exp_acctdescr$,1%,f1%(6))
            if f1%(6) = 0 then exp_acctdescr$="(ACCOUNT # NOT ON FILE)"
            call "GLFMT" (exp_acct$)
L30630:  if purdate$ = " " or purdate$ = blankdate$ then L30660
            purch_date$ = purdate$
            call "DATFMTC" (purch_date$)
L30660:  if dispdate$ = " " or dispdate$ = blankdate$ then L30700
            disposal_date$ = dispdate$
            call "DATFMTC" (disposal_date$)

L30700:  if purch_price > 0 then                                         ~
            call "CONVERT" (purch_price, -2.2, purch_price$)
         if itc_taken > 0 then                                           ~
            call "CONVERT" (itc_taken, -2.2, itc_taken$)
         if disposal_price > 0 then                                      ~
            call "CONVERT" (disposal_price, -2.2, disposal_price$)

            t% = pos("123" = type_code$)
            if t% > 0% then typedescr$ = typecodedescr$(t%)
            p% = pos("PRLAEXO" = property_type$)
            if p% > 0% then propdescr$ = propertydescr$(p%)

            for i% = 1% to 3%
L30860:        old_depr(i%) = current_depr(i%)
            next i%

            for i% = 1% to 3%
                gosub L37000
            next i%
            if pos("12" = amti_flag$) = 0% then L30980
               old_depr(4%) = current_depr(4%)
               gosub L38000
            return
L30980:     group$(4), life$(4), amti_flag$ = " "
            orig_basis(4), current_depr(4), accum_depr(4) = 0
            return

L32000: REM *************************************************************~
            *          WRITE DATA TO THE FIXED ASSETS FILE              *~
            *                                                           *~
            *************************************************************

            if amti_flag$ <> "1" then L32220
                readkey$ = str(group$(4)) & str(life$(4)) &              ~
                           str(convention$(2)) & str(in_service$(2))
                call "READ100" (#02, readkey$, f1%(2))
                     if f1%(2) = 1% then L32150
                        errormsg$ = "ALT MIN TAX Depreciation Table Is No~
        ~t On File, Please Correct"
                        return clear all
                        goto edtpg1
L32150:         if convention$(2) <> " " and in_service$(2) <> " " then  ~
                                                                    L32220
                     errormsg$ = "A CONVENTION And PERIOD Is Required On ~
        ~the Fed Tax Book W/ A.M.T. Selected"
                     return clear all
                     goto edtpg1

L32220:     call "SHOSTAT" ("Saving Data . . .")
            call "READ101" (#01, asset_code$, f1%(1))
            call "GLUNFMT" (asset_acct$)
            call "GLUNFMT" (depr_acct$)
            call "GLUNFMT" (exp_acct$)

            put #01, using L35300,                                        ~
                     depr_method$(),                                     ~
                     servdate$(),                                        ~
                     exp_acct$,                                          ~
                     depr_acct$,                                         ~
                     asset_acct$,                                        ~
                     property_type$,                                     ~
                     dispdate$,                                          ~
                     purdate$,                                           ~
                     location$,                                          ~
                     id_code$,                                           ~
                     asset_code$,                                        ~
                     descr_1$,                                           ~
                     descr_2$,                                           ~
                     type_code$,                                         ~
                     purch_price,                                        ~
                     itc_taken,                                          ~
                     disposal_price,                                     ~
                     disposal_descr$,                                    ~
                     vendor_code$,                                       ~
                     invoice_number$,                                    ~
                     group$(1),                                          ~
                     group$(2),                                          ~
                     group$(3),                                          ~
                     life$(1),                                           ~
                     life$(2),                                           ~
                     life$(3),                                           ~
                     convention$(),                                      ~
                     percentage(),                                       ~
                     switch_year$(),                                     ~
                     orig_basis(1),                                      ~
                     orig_basis(2),                                      ~
                     orig_basis(3),                                      ~
                     salvage_value(),                                    ~
                     itc_reduct(),                                       ~
                     bonus_depr(),                                       ~
                     exp_deduct(),                                       ~
                     other_reduct(),                                     ~
                     other_descr$(),                                     ~
                     accum_depr(1),                                      ~
                     accum_depr(2),                                      ~
                     accum_depr(3),                                      ~
                     current_depr(1),                                    ~
                     current_depr(2),                                    ~
                     current_depr(3),                                    ~
                     in_service$(),                                      ~
                     amti_flag$,                                         ~
                     group$(4),                                          ~
                     life$(4),                                           ~
                     orig_basis(4),                                      ~
                     current_depr(4),                                    ~
                     accum_depr(4)

            if f1%(1) <> 0% then rewrite #01                             ~
                            else write #01
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35300: FMT                      /* FILE: FAMASTER                     */~
            3*CH(2),             /* DEPRECIATION METHOD                */~
            3*CH(8),             /* DATE FIRST PLACED IN SERVICE       */~
            CH(9),               /* EXPENSE G/L ACCT CODE              */~
            CH(9),               /* ACCUM DEPRECIATION ACCOUNT         */~
            CH(9),               /* ASSET G/L ACCOUNT CODE             */~
            CH(1),               /* PROPERTY TYPE                      */~
            CH(8),               /* DISPOSAL DATE                      */~
            CH(8),               /* PURCHASE DATE                      */~
            CH(30),              /* LOCATION                           */~
            CH(15),              /* IDENTIFICATION CODE                */~
            CH(10),              /* ASSET CODE                         */~
            CH(30),              /* ASSET DESCRIPTION LINE 1           */~
            CH(30),              /* ASSET DESCRIPTION LINE 2           */~
            CH(1),               /* TYPE CODE                          */~
            PD(14,4),            /* PURCHASE PRICE                     */~
            PD(14,4),            /* INVESTMENT TAX CR TAKEN            */~
            PD(14,4),            /* DISPOSAL PRICE                     */~
            CH(30),              /* DISPOSAL DESCRIPTION               */~
            CH(9),               /* VENDOR CODE                        */~
            CH(16),              /* INVOICE NUMBER                     */~
            3*CH(10),            /* GROUP TABLE NAME                   */~
            3*CH(5),             /* LIFE                               */~
            3*CH(1),             /* PRORATION CONVENTION  1, 2 OR 3    */~
            3*PD(14,4),          /* PERCENTAGE                         */~
            3*CH(4),             /* YEAR OF SWITCH                     */~
            3*PD(14,4),          /* ORIGINAL BASIS                     */~
            3*PD(14,4),          /* SALVAGE VALUE                      */~
            3*PD(14,4),          /* ITC BASIS REDUCTION                */~
            3*PD(14,4),          /* BONUS DEPRECIATION TAKEN           */~
            3*PD(14,4),          /* EXPENSE DEDUCTION TAKEN            */~
            3*PD(14,4),          /* OTHER BASIS REDUCTION              */~
            3*CH(50),            /* OTHER BASIS REDUCT DESCRIPTION     */~
            3*PD(14,4),          /* ACCUMULATED DEPRECIATION           */~
            3*PD(14,4),          /* CURRENT DEPRECIATION               */~
            3*CH(2),             /* PERIOD IN SERVICE                  */~
            CH(1),               /* ALTERNATE MINIMUM TAX TYPE FLAG    */~
            CH(10),              /* AMTI GROUP TABLE NAME              */~
            CH(5),               /* AMTI CLASS LIFE                    */~
            PD(14,4),            /* AMTI ADJUSTED BASIS                */~
            PD(14,4),            /* AMTI CURRENT DEPRECIATION          */~
            PD(14,4),            /* AMTI ACCUMULATED DEPRECIATION      */~
            XX(159)              /* FILLER                             */

L36000: REM CALL THE SUBROUTINE TO ENTER DEPRECIATION VALUES
            call "FASUB"                                                 ~
                    (asset_code$,                                        ~
                     property_type$,                                     ~
                     dispdate$,                                          ~
                     purdate$,                                           ~
                     purch_price,                                        ~
                     amti_flag$,                                         ~
                     servdate$(),                                        ~
                     group$(),                                           ~
                     life$(),                                            ~
                     convention$(),                                      ~
                     cnv_dflt$,                                          ~
                     in_service$(),                                      ~
                     depr_method$(),                                     ~
                     percentage(),                                       ~
                     switch_year$(),                                     ~
                     orig_basis(),                                       ~
                     salvage_value(),                                    ~
                     itc_reduct(),                                       ~
                     bonus_depr(),                                       ~
                     exp_deduct(),                                       ~
                     other_reduct(),                                     ~
                     other_descr$(),                                     ~
                     accum_depr(),                                       ~
                     current_depr(),                                     ~
                     old_depr(),                                         ~
                     itc_taken,                                          ~
                     fiscal_begin$,                                      ~
                     fiscal_end$,                                        ~
                     keyhit%,                                            ~
                     i%,                                                 ~
                     #02)

            return


L37000: REM SUBROUTINE TO CALCULATE THE CURRENT YEAR'S DEPRECIATION
            call "FADEPR"     (asset_code$,                              ~
                               property_type$,                           ~
                               dispdate$,                                ~
                               servdate$(i%),                            ~
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
            return


L38000: REM SUBROUTINE TO CALCULATE THE CURRENT YEAR'S AMT DEPRECIATION

            method$ = " "
            if amti_flag$ = "1" then method$ = "7"
            if amti_flag$ = "2" then method$ = "S"
            if method$ = " " then return

            call "FADEPR"     (asset_code$,                              ~
                               property_type$,                           ~
                               dispdate$,                                ~
                               servdate$(2%),                            ~
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
            return


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%, e%)
                  p% = 1%
                  gosub setpf1
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  on fieldnr% gosub L40130,         /* ASSET CODE       */~
                                    L40130,         /* DESCRIPTION #1   */~
                                    L40130,         /* DESCRIPTION #2   */~
                                    L40145,         /* TYPE CODE        */~
                                    L40130,         /* ID CODE          */~
                                    L40130,         /* PURCHASE DATE    */~
                                    L40145,         /* PURCHASE PRICE   */~
                                    L40130,         /* PROPERTY TYPE    */~
                                    L40130,         /* ITC TAKEN        */~
                                    L40130,         /* LOCATION         */~
                                    L40130,         /* DISPOSAL DATE    */~
                                    L40145,         /* DISPOSAL PRICE   */~
                                    L40130,         /* DISPOSAL DESCR   */~
                                    L40135          /* AMTI TYPE FLAG   */
                     goto L40165

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
L40135:               lfac$(fieldnr%) = hex(81)
                      return
L40145:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40165:     accept                                                       ~
               at (01,02),                                               ~
                  "FIXED ASSETS INPUT",                                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Asset Code  ",                                        ~
               at (06,30), fac(lfac$( 1)), asset_code$          , ch(10),~
               at (07,02),                                               ~
                  "Asset Description #1",                                ~
               at (07,30), fac(lfac$( 2)), descr_1$             , ch(30),~
               at (08,02),                                               ~
                  "Asset Description #2",                                ~
               at (08,30), fac(lfac$( 3)), descr_2$             , ch(30),~
               at (09,02),                                               ~
                  "Type Code",                                           ~
               at (09,30), fac(lfac$( 4)), type_code$           , ch(01),~
               at (09,49), fac(hex(8c)),   typedescr$           , ch(32),~
               at (10,02),                                               ~
                  "Identification Code  ",                               ~
               at (10,30), fac(lfac$( 5)), id_code$             , ch(15),~
               at (11,02),                                               ~
                  "Purchase Date",                                       ~
               at (11,30), fac(lfac$( 6)), purch_date$          , ch(10),~
               at (12,02),                                               ~
                  "Purchase Price",                                      ~
               at (12,30), fac(lfac$( 7)), purch_price$         , ch(11),~
               at (13,02),                                               ~
                  "Property Type",                                       ~
               at (13,30), fac(lfac$( 8)), property_type$       , ch(01),~
               at (13,49), fac(hex(8c)), propdescr$             , ch(20),~
               at (14,02),                                               ~
                  "Investment Tax Credit Taken",                         ~
               at (14,30), fac(lfac$( 9)), itc_taken$           , ch(11),~
               at (15,02),                                               ~
                  "Location",                                            ~
               at (15,30), fac(lfac$(10)), location$            , ch(30),~
               at (16,02),                                               ~
                  "Disposal Date",                                       ~
               at (16,30), fac(lfac$(11)), disposal_date$       , ch(10),~
               at (17,02),                                               ~
                  "Disposal Price",                                      ~
               at (17,30), fac(lfac$(12)), disposal_price$      , ch(11),~
               at (18,02),                                               ~
                  "Disposal Description",                                ~
               at (18,30), fac(lfac$(13)), disposal_descr$      , ch(30),~
               at (19,02),                                               ~
                  "Alternate Minimum Tax Type",                          ~
               at (19,30), fac(lfac$(14)), amti_flag$           , ch(01),~
                                                                         ~
               at (20,02), fac(hex(84)),   inpmessage1$         , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L40505
                  call "MANUAL" ("FAINPUT ")
                  goto L40165

L40505:        if keyhit% <> 15 then L40530
                  call "PRNTSCRN"
                  goto L40165

L40530:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%, e%)
                  p% = 2%
                  gosub setpf1
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()

                  inpmessage1$ = "*********  The above fields are for mem~
        ~o and reporting purposes only. *********"
                  inpmessage$  = "*********   The Fixed Assets module doe~
        ~s not post to General Ledger.  *********"
                  on fieldnr% gosub L41180,         /* VENDOR CODE      */~
                                    L41180,         /* INVOICE NUMBER   */~
                                    L41180,         /* ASSET ACCOUNT    */~
                                    L41180,         /* DEPR ACCOUNT     */~
                                    L41180          /* EXPENSE ACCOUNT  */
                     goto L41250

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41180:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41250:     accept                                                       ~
               at (01,02),                                               ~
                  "FIXED ASSETS INPUT",                                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Vendor Code  ",                                       ~
               at (06,30), fac(lfac$( 1)), vendor_code$         , ch(09),~
               at (06,49), fac(hex(8c)),   vendor_codedescr$    , ch(32),~
               at (07,02),                                               ~
                  "Invoice Number",                                      ~
               at (07,30), fac(lfac$( 2)), invoice_number$      , ch(16),~
               at (08,02),                                               ~
                  "Asset G/L Account #",                                 ~
               at (08,30), fac(lfac$( 3)), asset_acct$          , ch(12),~
               at (08,49), fac(hex(8c)),   asset_acctdescr$     , ch(32),~
               at (09,02),                                               ~
                  "Accum Depr G/L Account #",                            ~
               at (09,30), fac(lfac$( 4)), depr_acct$           , ch(12),~
               at (09,49), fac(hex(8c)),   depr_acctdescr$      , ch(32),~
               at (10,02),                                               ~
                  "Expense G/L Account #",                               ~
               at (10,30), fac(lfac$( 5)), exp_acct$            , ch(12),~
               at (10,49), fac(hex(8c)),   exp_acctdescr$       , ch(32),~
                                                                         ~
               at (20,02), fac(hex(84)),   inpmessage1$         , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkey$),                                             ~
               key (keyhit%)

               if keyhit% <> 13 then L41730
                  call "MANUAL" ("FAINPUT ")
                  goto L41250

L41730:        if keyhit% <> 15 then L41762
                  call "PRNTSCRN"
                  goto L41250

L41762:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        setpf1:
        if e% = 2% then L42140            /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     " ~
                  & "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                " ~
                  & "             (15)Print Screen"
           pf$(3) = "                                                  " ~
                  & "             (16)Exit Program"
           pfkey$ = hex(01ffff04ffffffffffffffff0dff0f1000)
           if fieldnr% = 1% and p% = 1% then L42112
                str(pf$(3),64)   = " "
                str(pfkey$,16,1) = hex(ff)
L42112:    if fieldnr% > 1% then L42120
                str(pf$(2),18,18) = " "
                str(pfkey$,4,1) = hex(ff)
L42120:    return

L42140:  if fieldnr% > 0% then L42280     /* Edit Mode- Select Field    */
            pf$(1) = "(1)Start Over     (6)Edit Book Depr                ~
        ~            (13)Instructions"
            pf$(2) = "(4)Previous Page  (7)Edit Federal Tax Depr   (10)Ed~
        ~it A.M.T.   (15)Print Screen"
            pf$(3) = "(5)Next Page      (8)Edit State/Local Tax Depr     ~
        ~           " & hex(84) & "(16)SAVE DATA"
           pfkey$ = hex(01ffff0405060708ff0affff0dff0f1000)
           str(pf$(p%+1%), 1, 16) = " "
           str(pfkey$, p%+3%, 1) = hex(ff)
           inpmessage$ = edtmessage$
           if type_code$ = "3" then L42250
              str(pf$(1),19,30) = " "  :  str(pf$(2),1,60) = " "
              str(pf$(3), 1,60) = " "  :  str(pfkey$,4,7) = all(hex(ff))
              return
L42250:    if amti_flag$ <> " " then L42265
              str(pf$(2), 44, 20) = " "
              str(pfkey$, 10, 1) = hex(ff)
L42265:    return
                                         /* Edit Mode- Field Enabled   */
L42280:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%,edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L50115,         /* ASSET CODE       */~
                                    L50205,         /* DESCRIPTION #1   */~
                                    L50220,         /* DESCRIPTION #2   */~
                                    L50235,         /* TYPE CODE        */~
                                    L50290,         /* ID CODE          */~
                                    L50305,         /* PURCHASE DATE    */~
                                    L50360,         /* PURCHASE PRICE   */~
                                    L50410,         /* PROPERTY TYPE    */~
                                    L50605,         /* ITC TAKEN        */~
                                    L50765,         /* LOCATION         */~
                                    L50780,         /* DISPOSAL DATE    */~
                                    L50885,         /* DISPOSAL PRICE   */~
                                    L50925,         /* DISPOSAL DESCR   */~
                                    L50935          /* AMTI Type flag   */
                     return
L50115:     REM TEST DATA FOR ASSET CODE
               if asset_code$ <> " " and asset_code$ <> "?" then L50190
               asset_code$ = " "
               call "PLOWCODE" (#01, asset_code$, i$(1), 0%, 0.6, f1%(1))
L50135:              if f1%(1) = 1% then L50170
                     if str(asset_code$,,1) = " " then                   ~
                     errormsg$ = "Asset Code May Not Be Blank, or Begin w~
        ~/ a Blank."
                     if errormsg$ = " " then                             ~
                               str(line2$, 13, 10) = asset_code$
                     return
L50170:         gosub L30000
                return clear all
                str(line2$, 13, 10) = asset_code$
                goto edtpg1
L50190:              call "READ100" (#01, asset_code$, f1%(1))
                     goto L50135

L50205:     REM TEST DATA FOR ASSET DESCRIPTION #1
                return

L50220:     REM TEST DATA FOR ASSET DESCRIPTION #2
                return

L50235:     REM TEST DATA FOR TYPE CODE
                t% = pos("123"=type_code$)
                if t% = 0% then L50270
                typedescr$ = typecodedescr$(t%)
                if type_code$ = "3" then return
                return clear all
                goto edtpg1
L50270:         errormsg$ = "VALID TYPE CODES: 1=GROUP, 2=SUBGROUP, 3=ASS~
        ~ET"
                return

L50290:     REM TEST DATA FOR IDENTIFICATION CODE
                return

L50305:     REM TEST DATA FOR PURCHASE DATE
                if purch_date$ = " " or purch_date$ = blankdate$ then L50345
                call "DATEOKC" (purch_date$,pdate%,errormsg$)
                if errormsg$ <> " " then return
                purdate$ = purch_date$
                call "DATUFMTC" (purdate$)
                testdate$ = "19860101"
                call "DATECONV" (testdate$)
                if purdate$ >= testdate$ then itc_taken = 0
                return
L50345:         errormsg$ = "INVALID DATE"
                return

L50360:     REM TEST DATA FOR PURCHASE PRICE
                if purch_price$ <> " " then L50385
                purch_price = 0
                return

L50385:         call "NUMTEST" (purch_price$, 0, 99999999999, errormsg$, ~
                                2.2, purch_price)
                if errormsg$ <> " " then fieldnr% = fieldnr% - 1%
                return

L50410:     REM TEST DATA FOR PROPERTY TYPE
                p% = pos("PRLAEXO" = property_type$)
                if p% = 0% then                                          ~
                  errormsg$ = "VALID TYPES:  P, R, L, A, E, X or O."     ~
                else propdescr$ = propertydescr$(p%)
                if errormsg$ <> " " or edit% = 0% then return
                /* If Edit Mode make sure new property type is OK. */
                i% = 0%
                check_next_book:
                  i% = i% + 1%
                  if i% > 4% then return /* All O.K. */
                  if i% <> 4% then L50505
                    if amti_flag$ = " " then check_next_book
                    convention$ = convention$(2)
                    inservice$ = in_service$(2)
                    goto L50525
L50505:           if depr_method$(i%)<>"6" and depr_method$(i%)<>"7"     ~
                                                   then check_next_book
                  convention$ = convention$(i%)
                  inservice$ = in_service$(i%)
L50525:           plowkey$ = str(group$(i%),1,10) & str(life$(i%),1,5) & ~
                      str(convention$,1,1) & str(inservice$,1,2)
                  call "READ100" (#02, plowkey$, f1%(2))
                  if f1%(2) <> 0% then L50560
                    errormsg$ = "*ERROR* Table Not Found for " &         ~
                                book$(i%) & "."
                    return
L50560:           get #02 using L50565, types$
L50565:           FMT        POS(275), CH(7)
                  f% = pos(types$ = property_type$)
                  if f% <> 0% then check_next_book
                    errormsg$ = "*ERROR* Property Type not supported "  &~
                                "by the specified table for " & book$(i%)~
                                &  "."
                    return

L50605:     REM TEST DATA FOR INVESTMENT TAX CREDIT TAKEN
                if itc_taken$ <> " " then L50635
L50615:         itc_taken, itc_reduct(2) = 0
                itc_taken$ = " "
                goto L50740

L50635:         if pos(itc_taken$ = "%") > 0% then L50685
L50640:         call "NUMTEST" (itc_taken$, 0, 99999999999, errormsg$,   ~
                                2.2, itc_taken)
                pr = round(itc_taken / purch_price, 4)
L50655:         if pr = 0 then L50615
                if pr <> .10 and pr <> .08 and pr <> .06 and pr <> .04   ~
                    then errormsg$ = "The ITC MUST be 10%, 8%, 6% or 4% o~
        ~f the Purchase Price."
                if errormsg$ <> " " then return
                goto L50715
L50685:            x% = pos(itc_taken$ = "%")
                   convert str(itc_taken$,,x%-1%) to xx%, data goto L50640
                   pr = xx% / 100
                   itc_taken = purch_price * pr
                   goto L50655

L50715:            call "CONVERT" (itc_taken, -2.2, itc_taken$)
                   if pr = 0.1 or pr = 0.06                              ~
                      then itc_reduct(2) = round(itc_taken / 2, 2)       ~
                      else itc_reduct(2) = 0
                   if itc_taken = 0 then itc_taken$ = " "
L50740:            if e% <> 2% then return
                      i% = 2%
                      gosub L37000     /* Re-Calc Federal Depreciation */
                      return

L50765:     REM TEST DATA FOR LOCATION
                return

L50780:     REM TEST DATA FOR DISPOSAL DATE
                if disposal_date$ <> " " and disposal_date$ <> blankdate$ ~
                                                               then L50805
                dispdate$ = blankdate$
                disposal_price$, disposal_descr$ = " "
                disposal_price = 0
                goto L50855
L50805:         call "DATEOKC" (disposal_date$,ddate%,errormsg$)
                if errormsg$ <> " " then return
                if purch_date$ = " " or purch_date$ = blankdate$ then L50845
                call "DATEOKC" (purch_date$,pdate%,errormsg$)
                if ddate% >= pdate% then L50845
                errormsg$ = "DISPOSAL DATE CANNOT BE BEFORE THE PURCHASE ~
        ~DATE"
                return
L50845:         dispdate$ = disposal_date$
                call "DATUFMTC" (dispdate$)
L50855:         if e% = 1% then return
                for i% = 1% to 3%    /* CALL SUBROUTINE TO RECALCULATE */
                    gosub L37000      /* CURRENT YEAR'S DEPRECIATION    */
                next i%
                gosub L38000
                return
L50885:     REM TEST DATA FOR DISPOSAL PRICE
                if disposal_price$ <> " " then L50905
                disposal_price = 0
                return
L50905:         call "NUMTEST" (disposal_price$, 0, 99999999999,         ~
                                errormsg$, 2.2, disposal_price)
                return

L50925:     REM TEST DATA FOR DISPOSAL DESCRIPTION
                return
L50935:     REM TEST DATA FOR ALTERNANTE MINIMUM TAX TYPE FLAG
                if pos("12 " = amti_flag$) > 0% then L50955
                   errormsg$ = "INVALID TYPE: Must be  1, 2 or Blank."
                   return
L50955:         if amti_flag$ <> " " then L50975
                   group$(4), life$(4) = " "
                   orig_basis(4), current_depr(4), accum_depr(4) = 0
                   return
L50975:         if e% = 1% then return
                if amti_flag$ = "2" then L51000
                   convert depr_method$(2) to d%, data goto L51020
                   if d% <> 6% and d% <> 7% then L51020
                   if convention$(2) = " " then L51035
L51000:         return clear  :  return clear
                keyhit% = 10%
                if amti_flag$ = "2" then group$(4%) = " "
                goto L12000     /* FORCE INPUT ON A.M.T. SCREEN */
L51020:            errormsg$ = "The Federal Tax Depr Method must be 6 or ~
        ~7 to use AMT Type 1."
                   return
L51035:            errormsg$ = "The Federal Tax Depr Convention can NOT b~
        ~e blank to use AMT Type 1."
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52150,         /* VENDOR CODE      */~
                                    L52250,         /* INVOICE NUMBER   */~
                                    L52280,         /* ASSET ACCOUNT    */~
                                    L52380,         /* DEPR ACCOUNT     */~
                                    L52470          /* EXPENSE ACCOUNT  */
                     return
L52150:     REM TEST DATA FOR VENDOR CODE
                if vendor_code$ <> " " then L52190
                vendor_codedescr$ = " "
                return
L52190:   call "GETCODE" (#05, vendor_code$, vendor_codedescr$, 1%, 1.3, ~
                          f1%(5))
                if f1%(5) <> 0 then return
                vendor_codedescr$ = "(VENDOR NOT ON FILE)"
                return

L52250:     REM TEST DATA FOR INVOICE NUMBER
                return

L52280:     REM TEST DATA FOR ASSET G/L ACCOUNT #
                if asset_acct$ <> " " then L52320
                asset_acctdescr$ = " "
                return
L52320:   call "GETCODE" (#06, asset_acct$, asset_acctdescr$, 1%, 0,     ~
                          f1%(6))
                if f1%(6) <> 0 then return
                asset_acctdescr$ = "(ACCOUNT # NOT ON FILE)"
                return

L52380:     REM TEST DATA FOR ACCUM DEPR G/L ACCOUNT #
                if depr_acct$ <> " " then L52420
                depr_acctdescr$ = " "
                return
L52420:   call "GETCODE"(#06, depr_acct$, depr_acctdescr$, 1%, 0, f1%(6))
                if f1%(6) <> 0 then return
                depr_acctdescr$ = "(ACCOUNT # NOT ON FILE)"
                return

L52470:     REM TEST DATA FOR EXPENSE G/L ACCOUNT #
                if exp_acct$ <> " " then L52510
                exp_acctdescr$ = " "
                return
L52510:   call "GETCODE"(#06, exp_acct$, exp_acctdescr$, 1%, 0, f1%(6))
                if f1%(6) <> 0 then return
                exp_acctdescr$ = "(ACCOUNT # NOT ON FILE)"
                return

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

            call "SHOSTAT" ("One Moment Please")

            end
