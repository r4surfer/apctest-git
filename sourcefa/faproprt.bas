        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   PPPP   RRRR    OOO   PPPP   RRRR   TTTTT   *~
            *  F      A   A  P   P  R   R  O   O  P   P  R   R    T     *~
            *  FFFF   AAAAA  PPPP   RRRR   O   O  PPPP   RRRR     T     *~
            *  F      A   A  P      R   R  O   O  P      R   R    T     *~
            *  F      A   A  P      R   R   OOO   P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAPROPRT - THIS PROGRAM PRINTS THE DEPR PROJECTION REPORT *~
            *            FOR A RANGE OF ASSET NUMBERS AND DEPRECIATION  *~
            *            CATEGORY (BOOK, FEDERAL TAX, OR STATE TAX).    *~
            *            SUBTOTALS AND TOTALS ARE PRINTED FOR EACH SUB- *~
            *            GROUP AND GROUP.                               *~
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
            * 05/18/84 ! ORIGINAL                                 ! NLH *~
            * 09/21/88 ! F/A File Mods, Screen & Report Standards ! RJM *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            accum_depr(3),               /* ACCUMULATED DEPRECIATION   */~
            asset_code$10,               /* ASSET CODE                 */~
            begdate$8,                   /* FISCAL YEAR BEGIN DATE     */~
            bonus_depr(3),               /* BONUS DEPRECIATION         */~
            byy$4,                       /* FISCAL YR BEGIN YEAR       */~
            category_descr$28,           /* CATEGORY DESCRIPTION       */~
            current(5),                  /* ARRAY FOR 5 YRS CURR DEPR  */~
            current_depr(3),             /* CURRENT DEPRECIATION       */~
            convention$(3)1,             /* PRORATION CONVENTION       */~
            company$60,                                                  ~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            depr_category$1,             /* DEPRECIATION CATEGORY      */~
            depr_method$(3)2,            /* DEPRECIATION METHOD        */~
            descr_1$30,                  /* DESCRIPTION LINE 1         */~
            descr_2$30,                  /* DESCRIPTION LINE 2         */~
            dispdate$8,                  /* DISPOSAL DATE              */~
            enddate$8,                   /* FISCAL ENDING DATES        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            exp_deduct(3),               /* EXPENSE DEDUCTION          */~
            eyy$4,                       /* FISCAL YR END YEAR         */~
            fasset$19,                   /* FIRST ASSET IN RANGE       */~
            bdate$10,                    /* FISCAL YEAR BEGIN DATE     */~
            fdate$10,                    /* FISCAL YEAR END DATE MMDDYY*/~
            fiscal_begin$10,             /* FISCAL YEAR BEGIN DATE     */~
            fiscal_end$10,               /* FISCAL YEAR END DATE       */~
            first_asset$10,              /* FIRST ASSET #              */~
            fyb$(5)8,                    /* ARRAY OF 5 BEGIN DATES     */~
            fye$(5)8,                    /* ARRAY OF 5 END DATES       */~
            hi_asset$10,                 /* End of Asset Range         */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            in_service$(3)2,             /* PERIOD PUT IN SERVICE      */~
            itc_reduct(3),               /* ITC BASIS REDUCTION        */~
            last_asset$10,               /* LAST ASSET #               */~
            line2$79,                                                    ~
            life$(3)5,                   /* ASSET LIFE                 */~
            lo_asset$10,                 /* Start of Asset Range       */~
            method$32,                   /* METHOD                     */~
            method_descr$(9)23,          /* METHOD DESCRIPTION         */~
            orig_basis(3),               /* ORIGINAL BASIS             */~
            other_reduct(3),             /* OTHER BASIS REDUCTION      */~
            percentage(3),               /* PERCENTAGE                 */~
            property_type$1,             /* PROPERTY TYPE              */~
            report_date$(5)10,           /* 5 DATES TO PRINT ON REPORT */~
            salvage_value(3),            /* SALVAGE VALUE              */~
            savemethod$2,                /* CURRENT DEPR METHOD        */~
            servdate$(3)8,               /* DATE FIRST PLACED IN SERV  */~
            switch_year$(3)4,            /* YEAR OF SWITCH             */~
            tdate$10,                    /* Temporary Date Variable    */~
            tgroup$(3)10,                /* GROUP TABLE NAME           */~
            time$8,                                                      ~
            userid$3,                                                    ~
            type_code$1                  /* RECORD TYPE CODE           */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20                  /* TEXT FROM FILE OPENING     */


        REM *************************************************************~
            *                  RELEASE VERSION ID SECTION               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE  F2%()            SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #02 ! FATABLE  ! FIXED ASSETS DEPRECIATION PERCENT TABLES *~
            * #03 ! SYSFILE2 ! CAELUS MANAGEMENT SYSTEM INFORMATION     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "FAMASTER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 120,  keylen = 10,                      ~
                        alt key  1, keypos = 58, keylen =  1

            select #02, "FATABLE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1,    keylen = 18


            select #03, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1,    keylen = 20                       ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, 0%, f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#02, 0%, f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#03, 0%, f2%(3 ), 0%, rslt$(3 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            time$ = " "
            call "TIME" (time$)

            call "EXTRACT" addr("TT", tasktype$, "ID", userid$)

            call "COMPNAME" (12%, company$, f1%(3))

            init(" ")first_asset$, last_asset$, category_descr$

            str(line2$,62) = "FAPROPRT: " & str(cms2v$,,8)

        REM READ "SYSFILE2" FOR FISCAL YEAR END DATE
            call "READ100" (#3, "SWITCHS.FA", f1%(3))
                 if f1%(3) = 0% then L09794

            get #3, using L09710, begdate$, enddate$

L09710:         FMT XX(20), CH(8), CH(8)
            fiscal_begin$ = begdate$
            fiscal_end$ = enddate$
            bdate$ = fiscal_begin$
            fdate$ = fiscal_end$
            call "DATFMTC" (bdate$)
            call "DATFMTC" (fdate$)

            call "DATECONV" (fiscal_begin$, beg_yy%)
            beg_yy% = beg_yy% / 10000%         /* Remove Days & Month */
            call "DATECONV" (fiscal_end$, end_yy%)
            end_yy% = end_yy% / 10000%         /* Remove Days & Month */
            call "DATEOKC" (bdate$, u3%, errormsg$)
                if errormsg$ <> " " then L09794
            call "DATEOKC" (fdate$, u3%, errormsg$)
                if errormsg$ <> " " then L09794
            goto L09820

L09794:     call "ASKUSER" (0%, "ERROR IN FIXED-ASSETS FISCAL DATES",    ~
                          errormsg$,                                     ~
                          "CORRECT THIS BEFORE CONTINUING WITH YEAR END",~
                          "PRESS RETURN TO EXIT PROGRAM")
            goto L65000


L09820:     for yy% = 1% to 5%
                convert beg_yy% to byy$, pic(####)
                convert end_yy% to eyy$, pic(####)

                tdate$ = fiscal_begin$
                call "DATEFMT" (tdate$, 0%, fiscal_begin$)
                str(fiscal_begin$,1%,4%) = byy$
                call "DATECONV" (fiscal_begin$)

                tdate$ = fiscal_end$
                call "DATEFMT" ( tdate$, 0%, fiscal_end$ )
                str(fiscal_end$,1%,4%) = eyy$
                call "DATECONV" (fiscal_end$)

                fye$(yy%) = fiscal_end$
                fyb$(yy%) = fiscal_begin$
                end_yy% = end_yy% + 1%
                beg_yy% = beg_yy% + 1%
                report_date$(yy%) = fye$(yy%)
                call "DATFMTC" (report_date$(yy%))
            next yy%

            method_descr$(1%) = "STRAIGHT LINE"
            method_descr$(2%) = "S/L AFTER SWITCH"

            method_descr$(3%) = "DECLINING BALANCE"
            method_descr$(4%) = "DEC.BAL.W/SWITCH TO S/L"

            method_descr$(5%) = "SUM OF THE YEARS DIGITS"
            method_descr$(6%) = "TABLE METHOD # 1"
            method_descr$(7%) = "TABLE METHOD # 2"

            method_descr$(8%) = "PERCENT"
            method_descr$(9%) = "MANUAL"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            if tasktype$ = "B" then L10340
        inputmode
            init(" ") errormsg$, inpmessage$, group$, subgroup$,         ~
                      first_asset$, last_asset$, lo_asset$, hi_asset$
            inpmessage$ = "Valid Depreciation Categories are 'B' Book, 'F~
        ~' Federal Tax and 'S' State/Local"
            first_asset$ = "ALL"
            last_asset$ = " "

            no_of_assets%, end%, grp%, pagenr% = 0
            net_basisttl1, net_basisttl2, net_basisttl3 = 0
            remainttl1, remainttl2, remainttl3 = 0
            depr1ttl1, depr1ttl2, depr1ttl3 = 0
            depr2ttl1, depr2ttl2, depr2ttl3 = 0
            depr3ttl1, depr3ttl2, depr3ttl3 = 0
            depr4ttl1, depr4ttl2, depr4ttl3 = 0
            depr5ttl1, depr5ttl2, depr5ttl3 = 0

            line% = 100%

L10120:     gosub L40000
                      if keyhit%  =  1% then inputmode
                      if keyhit%  = 16% then L65000
                      if keyhit% <>  0% then L10120
            gosub L50000
                      if errormsg$ <> " " then L10120

            call "SHOSTAT" ("Printing Fixed Assets Future Depreciation Re~
        ~port")
            select printer(134)
            call "SETPRNT" ("F/A005", " ", 0%, 0%)
            gosub L11000
            if tasktype$ = "B" then L65000
            goto  inputmode

L10340:     REM SETS RANGE FOR BACKGROUND MODE.
                init(hex(00)) lo_asset$
                init(hex(ff)) hi_asset$
                select printer(134)
                call "SETPRNT" ("F/A005", " ", 0%, 0%)
                gosub L11000
                goto  L65000

L11000: REM *************************************************************~
            *               READ FIXED ASSETS MASTER FILE & PRINT       *~
            *                                                           *~
            *************************************************************


            call "PLOWNEXT" (#1, lo_asset$, 0%, f1%(1))
            if f1%(1) = 0 then L19000
            if lo_asset$ > hi_asset$ then L19000

            get #1, using L11340,                                         ~
                     depr_method$(),                                     ~
                     servdate$(),                                        ~
                     property_type$,                                     ~
                     dispdate$,                                          ~
                     asset_code$,                                        ~
                     descr_1$,                                           ~
                     descr_2$,                                           ~
                     type_code$,                                         ~
                     tgroup$(),                                          ~
                     life$(),                                            ~
                     convention$(),                                      ~
                     percentage(),                                       ~
                     switch_year$(),                                     ~
                     orig_basis(),                                       ~
                     salvage_value(),                                    ~
                     itc_reduct(),                                       ~
                     bonus_depr(),                                       ~
                     exp_deduct(),                                       ~
                     other_reduct(),                                     ~
                     accum_depr(),                                       ~
                     current_depr(),                                     ~
                     in_service$()

L11340: FMT                      /* FILE: FAMASTER                     */~
            3*CH(2),             /* DEPRECIATION METHOD                */~
            3*CH(8),             /* DATE FIRST PLACED IN SERVICE       */~
            XX(9),               /* EXPENSE G/L ACCT CODE              */~
            XX(9),               /* ACCUM DEPRECIATION ACCOUNT         */~
            XX(9),               /* ASSET G/L ACCOUNT CODE             */~
            CH(1),               /* PROPERTY TYPE                      */~
            CH(8),               /* DISPOSAL DATE                      */~
            XX(8),               /* PURCHASE DATE                      */~
            XX(30),              /* LOCATION                           */~
            XX(15),              /* IDENTIFICATION CODE                */~
            CH(10),              /* ASSET CODE                         */~
            CH(30),              /* ASSET DESCRIPTION LINE 1           */~
            CH(30),              /* ASSET DESCRIPTION LINE 2           */~
            CH(1),               /* TYPE CODE                          */~
            XX(8),               /* PURCHASE PRICE                     */~
            XX(8),               /* INVESTMENT TAX CR TAKEN            */~
            XX(8),               /* DISPOSAL PRICE                     */~
            XX(30),              /* DISPOSAL DESCRIPTION               */~
            XX(9),               /* VENDOR CODE                        */~
            XX(16),              /* INVOICE NUMBER                     */~
            3*CH(10),            /* GROUP TABLE NAMES                  */~
            3*CH(5),             /* LIFE                               */~
            3*CH(1),             /* CONVENTION                         */~
            3*PD(14,4),          /* PERCENTAGE                         */~
            3*CH(4),             /* YEAR OF SWITCH                     */~
            3*PD(14,4),          /* ORIGINAL BASIS                     */~
            3*PD(14,4),          /* SALVAGE VALUE                      */~
            3*PD(14,4),          /* ITC BASIS REDUCTION                */~
            3*PD(14,4),          /* BONUS DEPRECIATION TAKEN           */~
            3*PD(14,4),          /* EXPENSE DEDUCTION TAKEN            */~
            3*PD(14,4),          /* OTHER BASIS REDUCTION              */~
            XX(150),             /* OTHER BASIS REDUCT DESCRIPTION     */~
            3*PD(14,4),          /* ACCUMULATED DEPRECIATION           */~
            3*PD(14,4),          /* CURRENT DEPRECIATION               */~
            3*CH(2)              /* IN SERVICE PERIOD                  */


            if type_code$ <> "3" then L12290
            i% = cat%
            savemethod$ = depr_method$(i%)

            remain_basis = orig_basis(i%) - salvage_value(i%) -          ~
                           itc_reduct(i%) - bonus_depr(i%) -             ~
                           exp_deduct(i%) - other_reduct(i%) -           ~
                           accum_depr(i%)
            if depr_method$(i%) = "3" or depr_method$(i%) = "4" or       ~
               depr_method$(i%) = "6" or depr_method$(i%) = "7" then     ~
                remain_basis = remain_basis + salvage_value(i%)


            net_remain_basis = remain_basis
            for yy% = 1% to 5%
                fiscal_begin$ = fyb$(yy%)
                fiscal_end$ = fye$(yy%)
            call "FADEPR"     (asset_code$,                              ~
                               property_type$,                           ~
                               dispdate$,                                ~
                               servdate$(i%),                            ~
                               tgroup$(i%),                              ~
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
                               u3%,                                      ~
                               #02)

                current(yy%) = round(current_depr(i%), 2%)
                if net_remain_basis - current(yy%) < 0 then              ~
                    current(yy%) = round(net_remain_basis,2%)
                accum_depr(i%) = accum_depr(i%) + current(yy%)
                net_remain_basis = net_remain_basis - current(yy%)

            next yy%

            net_remain_basis = remain_basis /*TO HANDLE ROUNDING ERRORS*/
            for yy% = 1% to 5%
                net_remain_basis =                                       ~
                round(net_remain_basis,2%) - round(current(yy%),2%)
            next yy%
L12290:     gosub L20000
            goto L11000

L19000: REM PRINT THE GRAND TOTALS AND THE NUMBER OF ASSETS
            if no_of_assets% = 0% then L19040


            end% = 1%
            gosub L21000
            print using L31220,                                           ~
                               net_basisttl1, depr1ttl1, depr2ttl1,      ~
                               depr3ttl1, depr4ttl1, depr5ttl1,          ~
                               remainttl1
            gosub L28300
            print using L31030
            print
L19040:     print using L31060, no_of_assets%
            close printer
            call "SETPRNT" ("F/A005", " ", 0%, 1%)
            return

L20000: REM *************************************************************~
            *        PRINT THE ASSET AND ACCUMULATED TOTALS             *~
            *                                                           *~
            *************************************************************

            gosub L28000
            convert type_code$ to record_type%, data goto L23430
            on record_type% goto L21000, L22000, L23000

L21000: REM CHANGE IN ASSET GROUP - TYPE 1
            if no_of_assets% = 0 then L21140
            if group$ = " " then L21140
            grp% = 1%
            gosub L22000
            subgroup$ = " "
            grp% = 0
            print using L31000
            gosub L28300
            print using L31180, group$,                                   ~
                               net_basisttl2, depr1ttl2, depr2ttl2,      ~
                               depr3ttl2, depr4ttl2, depr5ttl2,          ~
                               remainttl2
            gosub L28300
            print using L31000
            gosub L28300
            print
            gosub L28300
L21140:     if end% = 1% then return
            print using L31150, asset_code$, descr_1$, descr_2$
            gosub L28300
            print
            gosub L28300
            group$ = asset_code$
            net_basisttl2, depr1ttl2, depr2ttl2, depr3ttl2 = 0
            depr4ttl2, depr5ttl2, remainttl2 = 0
            return

L22000: REM CHANGE IN ASSET SUBGROUP - TYPE 2
            if no_of_assets% = 0 then L22240
            if subgroup$ = " " then L22240
            print using L31000
            gosub L28300
            print using L31110, subgroup$,                                ~
                               net_basisttl3, depr1ttl3, depr2ttl3,      ~
                               depr3ttl3, depr4ttl3, depr5ttl3,          ~
                               remainttl3
            gosub L28300
            print
            gosub L28300
L22240:     if end% = 1% then return
            if grp% = 1% then L22275
            print using L31080, asset_code$, descr_1$, descr_2$
            gosub L28300
            print
            gosub L28300
            subgroup$ = asset_code$
L22275:     net_basisttl3, depr1ttl3, depr2ttl3, depr3ttl3 = 0
            depr4ttl3, depr5ttl3, remainttl3 = 0
            return
L23000: REM PRINT AN ASSET - TYPE 3
            method$ = " "
            convert savemethod$ to method%, data goto L23040
            if method% > 0% and method% < 10% then                       ~
                method$ = method_descr$(method%)
L23040:     if line% + 4% <= 57% then L23070
                line% = line% + 4%
                gosub L28000
L23070:     print using L30260, asset_code$, savemethod$, method$,        ~
                           property_type$, switch_year$(i%),remain_basis,~
                               current(1%), current(2%),                 ~
                               current(3%), current(4%),                 ~
                               current(5%), net_remain_basis
            print using L30290, descr_1$
            print using L30290, descr_2$
            print
            line% = line% + 4%

            net_basisttl1 = net_basisttl1 + remain_basis
            depr1ttl1 = depr1ttl1 + current(1%)
            depr2ttl1 = depr2ttl1 + current(2%)
            depr3ttl1 = depr3ttl1 + current(3%)
            depr4ttl1 = depr4ttl1 + current(4%)
            depr5ttl1 = depr5ttl1 + current(5%)
            remainttl1 = remainttl1 + net_remain_basis

            net_basisttl2 = net_basisttl2 + remain_basis
            depr1ttl2 = depr1ttl2 + current(1%)
            depr2ttl2 = depr2ttl2 + current(2%)
            depr3ttl2 = depr3ttl2 + current(3%)
            depr4ttl2 = depr4ttl2 + current(4%)
            depr5ttl2 = depr5ttl2 + current(5%)
            remainttl2 = remainttl2 + net_remain_basis

            net_basisttl3 = net_basisttl3 + remain_basis
            depr1ttl3 = depr1ttl3 + current(1%)
            depr2ttl3 = depr2ttl3 + current(2%)
            depr3ttl3 = depr3ttl3 + current(3%)
            depr4ttl3 = depr4ttl3 + current(4%)
            depr5ttl3 = depr5ttl3 + current(5%)
            remainttl3 = remainttl3 + net_remain_basis


            no_of_assets% = no_of_assets% + 1%
L23430:     return

L28000: REM *************************************************************~
            *            PRINT THE PAGE HEADING                         *~
            *                                                           *~
            *************************************************************


            if line% <= 55 then return
            pagenr% = pagenr% + 1%
            print page
            print using L30040, date$, time$, company$
            print using L30080, userid$, category_descr$, pagenr%
            print
            print using L30120, fdate$
            print using L30130, fasset$, lasset$
            print
            print using L30150
            print using L30180, report_date$(1%), report_date$(2%),       ~
                               report_date$(3%), report_date$(4%),       ~
                               report_date$(5%)
            print using L30220
            line% = 9%
            return

L28300:     line% = line% + 1%
            if line% > 55% then gosub L28000
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
            *            REPORT FORMAT STATEMENTS                       *~
            *************************************************************

L30040: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                      FAPROPRT: F/A~
        ~005

L30080: %BY: ###                       F U T U R E   D E P R E C I A T I ~
        ~O N   S C H E D U L E  - ############################   PAGE:  ##~
        ~##

L30120: %FISCAL YEAR ENDING ##########
L30130: %ASSET CODE RANGE: ################### TO #############

L30150: %ASSET CODE/       CURRENT            PROP SWTCH     NET     +---~
        ~---------------- FISCAL YEAR ENDING ------------------+  REMAININ~
        ~G
L30180: %  DESCR     DEPRECIATION METHOD      TYPE YEAR     BASIS     ###~
        ~#######  ##########  ##########  ##########  ##########    BASIS ~


L30220: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

L30260: %##########  ## #######################  # #### #########.## ####~
        ~####.## ########.## ########.## ########.## ########.## ########.~
        ~##
L30290: %     ##############################

L31000: %                                               ------------ ----~
        ~------- ----------- ----------- ----------- ----------- ---------~
        ~--
L31030: %                                               ============ ====~
        ~======= =========== =========== =========== =========== =========~
        ~==
L31060: %NUMBER OF ASSETS PRINTED:  ######

L31080: %SUBGROUP:  ########## ############################## ###########~
        ~###################

L31110: %TOTAL FOR SUBGROUP: ##########                 #########.## ####~
        ~####.## ########.## ########.## ########.## ########.## ########.~
        ~##

L31150: %GROUP:     ########## ############################## ###########~
        ~###################

L31180: %TOTAL FOR GROUP:    ##########                 #########.## ####~
        ~####.## ########.## ########.## ########.## ########.## ########.~
        ~##

L31220: %GRAND TOTAL:                                   #########.## ####~
        ~####.## ########.## ########.## ########.## ########.## ########.~
        ~##


L40000: REM *************************************************************~
            *        I N P U T   M O D E   S C R E E N   P A G E  1     *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME                            *~
            *************************************************************

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "FIXED ASSETS FUTURE DEPRECIATION SCHEDULE",           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST ASSET CODE",                                    ~
               at (06,30), fac(hex(81)), first_asset$           , ch(10),~
               at (07,02),                                               ~
                  "LAST ASSET CODE",                                     ~
               at (07,30), fac(hex(81)), last_asset$            , ch(10),~
               at (08,02),                                               ~
                  "DEPRECIATION CATEGORY",                               ~
               at (08,30), fac(hex(81)), depr_category$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40610
                  call "MANUAL" ("FAPROPRT")
                  goto L40230

L40610:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40230

L50000: REM *************************************************************~
            *                    T E S T  D A T A                       *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            init(" ") errormsg$

            REM TEST DATA
                if pos("BFS"=depr_category$) = 0% then L50400
                if pos("BFS"=depr_category$) = 0% then L50400

            call "TESTRNGE" (first_asset$, last_asset$, lo_asset$,       ~
                             hi_asset$, errormsg$)
            if errormsg$ <> " " then return

            if first_asset$ <> "ALL" then L50220
                fasset$ = "(BEGINNING OF FILE)"
                lasset$ = "(END OF FILE)"
                goto L50320

L50220:     if lo_asset$ = hex(00) then fasset$ = "(BEGINNING OF FILE)"  ~
                                   else fasset$ = first_asset$
            if hi_asset$ = hex(ff) then lasset$ = "(END OF FILE)"        ~
                                   else lasset$ = last_asset$

L50320:         category_descr$ = "BOOK DEPRECIATION"
                if depr_category$ = "F" then category_descr$ = "FEDERAL T~
        ~AX DEPRECIATION"
                if depr_category$ = "S" then category_descr$ = "STATE/LOC~
        ~AL TAX DEPRECIATION"
                cat% = pos("BFS"=depr_category$)
                return

L50400:         errormsg$ = "Invalid Depreciation Category"
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
