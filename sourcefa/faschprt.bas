        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA    SSS    CCC   H   H  PPPP   RRRR   TTTTT   *~
            *  F      A   A  S      C   C  H   H  P   P  R   R    T     *~
            *  FFFF   AAAAA   SSS   C      HHHHH  PPPP   RRRR     T     *~
            *  F      A   A      S  C   C  H   H  P      R   R    T     *~
            *  F      A   A   SSS    CCC   H   H  P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FASCHPRT - THIS PROGRAM PRINTS THE DEPRECIATION SCHEDULE  *~
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
            * 09/12/88 ! File changes to FAMASTER, Standardized   ! RJM *~
            *          !   Added Report ID "F/A004" & Print new   !     *~
            *          !   data fields.  Fixed report Alignment.  !     *~
            * 09/21/88 ! Fixed Method Descriptions Array          ! RJM *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            accum_depr(3),               /* ACCUMULATED DEPRECIATION   */~
            accum_depr$12,               /* ACCUMULATED DEPRECIATION   */~
            asset_code$10,               /* ASSET NUMBER               */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bonus_depr(3),               /* BONUS DEPRECIATION         */~
            bonus_depr$11,               /* BONUS DEPRECIATION         */~
            category_descr$28,           /* CATEGORY DESCRIPTION       */~
            cnv_descr$(4)14,             /* PRORATION CONVENTION DESCR */~
            convention$(3)1,             /* PRORATION CONVENTION       */~
            company$60,                                                  ~
            current_depr(3),             /* CURRENT DEPRECIATION       */~
            current_depr$11,             /* CURRENT DEPRECIATION       */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            depr_category$1,             /* DEPRECIATION CATEGORY      */~
            depr_method$(3)2,            /* DEPRECIATION METHOD        */~
            descr_1$30,                  /* DESCRIPTION LINE 1         */~
            descr_2$30,                  /* DESCRIPTION LINE 2         */~
            disposal_desc$17,            /* DISPOSAL DESCRIPTION       */~
            dispdate$8,                  /* DISPOSAL DATE              */~
            disposal_date$10,            /* DISPOSAL DATE              */~
            enddate$8,                                                   ~
            errormsg$79,                 /* ERROR MESSAGE              */~
            exp_deduct(3),               /* EXPENSE DEDUCTION          */~
            exp_deduct$11,               /* EXPENSE DEDUCTION          */~
            fasset$19,                   /* FIRST ASSET IN RANGE       */~
            fiscal_end$10,               /* FISCAL YEAR END DATE       */~
            first_asset$10,              /* FIRST ASSET #              */~
            hi_asset$10,                 /* End of Asset Range         */~
            in_service$(3)2,             /* IN SERVICE PERIOD          */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            itc_reduct(3),               /* ITC BASIS REDUCTION        */~
            itc_reduct$11,               /* ITC BASIS REDUCTION        */~
            lasset$13,                   /* LAST ASSET IN RANGE        */~
            last_asset$10,               /* LAST ASSET #               */~
            life$(3)5,                   /* ASSET LIFE                 */~
            line2$79,                    /* Screen Header              */~
            lo_asset$10,                 /* Start of Asset Range       */~
            method$56,                   /* METHOD                     */~
            method_descr$(9)56,          /* METHOD DESCRIPTION         */~
            orig_basis(3),               /* ORIGINAL BASIS             */~
            orig_basis$12,               /* ORIGINAL BASIS             */~
            other_descr$(3)50,           /* OTHER BASIS REDUCTION DESC */~
            other_reduct(3),             /* OTHER BASIS REDUCTION      */~
            other_reduct$11,             /* OTHER BASIS REDUCTION      */~
            percentage(3),               /* PERCENTAGE                 */~
            percentage$6,                /* PERCENTAGE                 */~
            property_descr$(7)18,        /* PROPERTY TYPE DESCRIPTION  */~
            property_type$1,             /* PROPERTY TYPE              */~
            salvage_value(3),            /* SALVAGE VALUE              */~
            salvage_value$11,            /* SALVAGE VALUE              */~
            servdate$(3)8,               /* DATE FIRST PLACED IN SERV  */~
            service_date$10,             /* DATE FIRST PLACED IN SERV  */~
            time$8,                                                      ~
            today$10,                    /* TODAY'S DATE YYYYMMDD      */~
            tgroup$(3)10,                /* GROUP TABLE NAME           */~
            userid$3

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
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "FAMASTER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 120,  keylen = 10,                      ~
                        alt key  1, keypos = 58, keylen =  1

            select #02, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1,    keylen = 20                       ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, 0%, f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#02, 0%, f2%(2 ), 0%, rslt$(2 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            today$ = date$
            call "DATEOKC" (today$, u3%, errormsg$)
            call "DATUFMTC" (today$)
            time$ = " "
            call "TIME" (time$)

            call "EXTRACT" addr("TT", tasktype$, "ID", userid$)

            init(" ")first_asset$, last_asset$, category_descr$,         ~
                     subgroup$, group$, line2$, lo_asset$, hi_asset$

            str(line2$,62) = "FASCHPRT: " & str(cms2v$,,8)

            property_descr$(1) = "PERSONAL PROPERTY"
            property_descr$(2) = "REAL ESTATE"
            property_descr$(3) = "LOW INCOME HOUSING"
            property_descr$(4) = "AMORTIZED PROPERTY"
            property_descr$(5) = "LEASED PROPERTY"
            property_descr$(6) = "RESIDENTIAL RENTAL"
            property_descr$(7) = "OTHER"

        REM READ "SYSFILE2" FOR FISCAL YEAR END DATE
            call "READ100" (#02, "SWITCHS.FA        ", f1%(2))
            get #02, using L09290, enddate$
L09290:          FMT XX(28), CH(8)
            fiscal_end$ = enddate$
            call "DATFMTC" (fiscal_end$)         /* MM/DD/YYYY */

            method_descr$(1%) = "STRAIGHT LINE"
            method_descr$(2%) = "STRAIGHT LINE AFTER SWITCH FROM DECLININ~
        ~G BALANCE"
            method_descr$(3%) = "DECLINING BALANCE"
            method_descr$(4%) = "DECLINING BALANCE WITH AUTOMATIC SWITCH ~
        ~TO STRAIGHT LINE"
            method_descr$(5%) = "SUM OF THE YEARS DIGITS"
            method_descr$(6%) = "TABLE METHOD # 1"
            method_descr$(7%) = "TABLE METHOD # 1"
            method_descr$(8%) = "PERCENTAGE"
            method_descr$(9%) = "MANUAL"

            cnv_descr$(1) = "HALF - YEAR"
            cnv_descr$(2) = "MID - QUARTER"
            cnv_descr$(3) = "MID - MONTH"
            cnv_descr$(4) = " "

            call "COMPNAME" (12%, company$, u3%)
            u3% = u3%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, subgroup$, group$
            inpmessage$ = "Valid Depreciation Categories: 'B'=Book, 'F'=F~
        ~Ederal Tax, 'S'=State/Local"
            first_asset$ = "ALL"
            last_asset$ = " "

            no_of_assets%, end%, grp% = 0
            basis_ttl1, basis_ttl2, basis_ttl3 = 0
            accum_ttl1, accum_ttl2, accum_ttl3 = 0
            current_ttl1, current_ttl2, current_ttl3 = 0
            salvage_ttl1, salvage_ttl2, salvage_ttl3 = 0
            itc_ttl1, itc_ttl2, itc_ttl3 = 0
            bonus_ttl1, bonus_ttl2, bonus_ttl3 = 0
            exp_ttl1, exp_ttl2, exp_ttl3 = 0
            other_ttl1, other_ttl2, other_ttl3 = 0

            line% = 56%
            if tasktype$ = "B" then L10390

L10280:     gosub L40000
                      if keyhit%  =  1% then inputmode
                      if keyhit%  = 16% then L65000
                      if keyhit% <>  0% then L10280
            gosub L50000
                      if errormsg$ <> " " then L10280

           call "SHOSTAT" ("Printing Fixed Assets Depreciation Schedule")
            select printer(134)
            call "SETPRNT" ("F/A004", " ", 0%, 0%)
            gosub L11000
            goto  inputmode

L10390:     REM SETS RANGE FOR BACKGROUND MODE.
                init(hex(00)) lo_asset$
                init(hex(ff)) hi_asset$
            select printer(134)
            call "SETPRNT" ("F/A004", " ", 0%, 0%)
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
                     orig_basis(),                                       ~
                     salvage_value(),                                    ~
                     itc_reduct(),                                       ~
                     bonus_depr(),                                       ~
                     exp_deduct(),                                       ~
                     other_reduct(),                                     ~
                     other_descr$(),                                     ~
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
            XX(12),              /* YEAR OF SWITCH                     */~
            3*PD(14,4),          /* ORIGINAL BASIS                     */~
            3*PD(14,4),          /* SALVAGE VALUE                      */~
            3*PD(14,4),          /* ITC BASIS REDUCTION                */~
            3*PD(14,4),          /* BONUS DEPRECIATION TAKEN           */~
            3*PD(14,4),          /* EXPENSE DEDUCTION TAKEN            */~
            3*PD(14,4),          /* OTHER BASIS REDUCTION              */~
            3*CH(50),            /* OTHER BASIS REDUCT DESCRIPTION     */~
            3*PD(14,4),          /* ACCUMULATED DEPRECIATION           */~
            3*PD(14,4),          /* CURRENT DEPRECIATION               */~
            3*CH(2)              /* IN SERVICE PERIOD                  */


            gosub L20000
            goto L11000

L19000: REM PRINT THE GRAND TOTALS AND THE NUMBER OF ASSETS
            if no_of_assets% = 0% then L19150

            end% = 1%
            gosub L21000
            if line% + 4% <= 55% then L19090
            line% = line% + 10%
            gosub L28000
L19090:     print using L31220,                                           ~
                               basis_ttl1, salvage_ttl1, itc_ttl1,       ~
                               bonus_ttl1, exp_ttl1, other_ttl1,         ~
                               accum_ttl1, current_ttl1
            print using L31030
            print
L19150:     print using L31060, no_of_assets%
            close printer
            call "SETPRNT" ("F/A001", " ", 0%, 1%)
            return

L20000: REM *************************************************************~
            *        PRINT THE ASSET AND ACCUMULATED TOTALS             *~
            *                                                           *~
            *************************************************************

            gosub L28000
            convert type_code$ to record_type%, data goto L20070
            on record_type% goto L21000, L22000, L23000
L20070:     return

L21000: REM CHANGE IN ASSET GROUP - TYPE 1
            if no_of_assets% = 0% then L21180
            if group$ = " " then L21180
            grp% = 1%
            gosub L22000
            subgroup$ = " "
            grp% = 0
            print using L31000
            gosub L28300
            print using L31180, group$,                                   ~
                               basis_ttl2, salvage_ttl2, itc_ttl2,       ~
                               bonus_ttl2, exp_ttl2, other_ttl2,         ~
                               accum_ttl2, current_ttl2
            gosub L28300
            print using L31000
            gosub L28300
            print
            gosub L28300
L21180:     if end% = 1% then return
            print using L31150, asset_code$, descr_1$, descr_2$
            gosub L28300
            print
            gosub L28300
            group$ = asset_code$
            basis_ttl2, salvage_ttl2, itc_ttl2, bonus_ttl2, exp_ttl2 = 0
            other_ttl2, accum_ttl2, current_ttl2 = 0
            return

L22000: REM CHANGE IN ASSET SUBGROUP - TYPE 2
            if no_of_assets% = 0 then L22120
            if subgroup$ = " " then L22120
            print using L31000
            gosub L28300
            print using L31110, subgroup$,                                ~
                               basis_ttl3, salvage_ttl3, itc_ttl3,       ~
                               bonus_ttl3, exp_ttl3, other_ttl3,         ~
                               accum_ttl3, current_ttl3
            gosub L28300
            print
            gosub L28300
L22120:     if end% = 1% then return
            if grp% = 1% then L22190
            print using L31080, asset_code$, descr_1$, descr_2$
            gosub L28300
            print
            gosub L28300
            subgroup$ = asset_code$
L22190:     basis_ttl3, salvage_ttl3, itc_ttl3, bonus_ttl3, exp_ttl3 = 0
            other_ttl3, accum_ttl3, current_ttl3 = 0
            return


L23000: REM PRINT AN ASSET - TYPE 3
            service_date$ = str(servdate$(cat%),1,8)
            disposal_date$ = str(dispdate$,1,8)
            call "DATFMTC" (service_date$)
            call "DATFMTC" (disposal_date$)

            call "CONVERT" (percentage(cat%),   -2.2, percentage$)
            call "CONVERT" (orig_basis(cat%),    2.2, orig_basis$)
            call "CONVERT" (salvage_value(cat%), 2.2, salvage_value$)
            call "CONVERT" (itc_reduct(cat%),    2.2, itc_reduct$)
            call "CONVERT" (bonus_depr(cat%),    2.2, bonus_depr$)
            call "CONVERT" (exp_deduct(cat%),    2.2, exp_deduct$)
            call "CONVERT" (other_reduct(cat%),  2.2, other_reduct$)
            call "CONVERT" (accum_depr(cat%),    2.2, accum_depr$)
            call "CONVERT" (current_depr(cat%),  2.2, current_depr$)

            c% = 4%
            if convention$(cat%) = " " then L23200
            convert convention$(cat%) to c%, data goto L23200

L23200:     method$, life$ = " "
            convert depr_method$(cat%) to meth%, data goto L23270
         if meth% > 0% and meth% < 9% then method$ = method_descr$(meth%)
            disposal_desc$ = " "
            if dispdate$ < today$ and dispdate$ <> " "                   ~
                                  and dispdate$ <> blankdate$ then       ~
                                      disposal_desc$ = "*** RETIRED ***"

            if meth% <> 6% and meth% <> 7% then life$ = life$(cat%)
L23270:     pr% = pos("PRLAEXO"=property_type$)
            if line% + 8% <= 57% then L23310
                line% = line% + 8%
                gosub L28000
L23310:     print using L30270, asset_code$,   service_date$, life$,     ~
                               orig_basis$, salvage_value$,              ~
                               itc_reduct$, bonus_depr$, exp_deduct$,    ~
                               other_reduct$, accum_depr$, current_depr$
            print using L30300, descr_1$
            print using L30300, descr_2$
            print using L30320, property_type$, property_descr$(pr%)
            print using L30330, depr_method$(cat%), method$
            print using L30350, percentage$
            print using L30360, disposal_date$, disposal_desc$
            if meth% <> 6% and meth% <> 7% then L23460
               print using L30370, tgroup$(cat%), life$(cat%),            ~
                                  convention$(cat%), cnv_descr$(c%),     ~
                                  in_service$(cat%)
               line% = line% + 1%
L23460:     print
            line% = line% + 8%

            basis_ttl1   = basis_ttl1   + orig_basis(cat%)
            salvage_ttl1 = salvage_ttl1 + salvage_value(cat%)
            itc_ttl1     = itc_ttl1     + itc_reduct(cat%)
            bonus_ttl1   = bonus_ttl1   + bonus_depr(cat%)
            exp_ttl1     = exp_ttl1     + exp_deduct(cat%)
            other_ttl1   = other_ttl1   + other_reduct(cat%)
            accum_ttl1   = accum_ttl1   + accum_depr(cat%)
            current_ttl1 = current_ttl1 + current_depr(cat%)

            basis_ttl2   = basis_ttl2   + orig_basis(cat%)
            salvage_ttl2 = salvage_ttl2 + salvage_value(cat%)
            itc_ttl2     = itc_ttl2     + itc_reduct(cat%)
            bonus_ttl2   = bonus_ttl2   + bonus_depr(cat%)
            exp_ttl2     = exp_ttl2     + exp_deduct(cat%)
            other_ttl2   = other_ttl2   + other_reduct(cat%)
            accum_ttl2   = accum_ttl2   + accum_depr(cat%)
            current_ttl2 = current_ttl2 + current_depr(cat%)

            basis_ttl3   = basis_ttl3   + orig_basis(cat%)
            salvage_ttl3 = salvage_ttl3 + salvage_value(cat%)
            itc_ttl3     = itc_ttl3     + itc_reduct(cat%)
            bonus_ttl3   = bonus_ttl3   + bonus_depr(cat%)
            exp_ttl3     = exp_ttl3     + exp_deduct(cat%)
            other_ttl3   = other_ttl3   + other_reduct(cat%)
            accum_ttl3   = accum_ttl3   + accum_depr(cat%)
            current_ttl3 = current_ttl3 + current_depr(cat%)

            no_of_assets% = no_of_assets% + 1%
            return

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
            print using L30120, fiscal_end$
            print using L30130, fasset$, lasset$
            print
            print using L30160
            print using L30190
            print using L30230
            line% = 8%
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
        ~##############################                      FASCHPRT: F/A~
        ~004

L30080: %BY: ###             F I X E D   A S S E T S   D E P R E C I A T ~
        ~I O N   S C H E D U L E  - ############################ PAGE:  ##~
        ~##

L30120: %FISCAL YEAR ENDING ##########
L30130: %ASSET CODE RANGE: ################### TO #############

L30160: %ASSET CODE/ DATE PLACED             ORIGINAL      SALVAGE   ITC ~
        ~BASIS  BONUS DEPR   EXPENSE   OTHER BASIS    ACCUMULATED   CURREN~
        ~T
L30190: %DESCRIPTION IN SERVICE  LIFE         BASIS         VALUE    REDU~
        ~CTION    TAKEN      DEDUCTION   REDUCTION       DEPR     YEAR'S D~
        ~EPR

L30230: %----------  ---------- -------- ------------  ----------- ------~
        ~----- ----------- ----------- -----------   ------------ --------~
        ~---

L30270: %##########  ########## #####Yrs ############  ########### ######~
        ~##### ########### ########### ###########   ############ ########~
        ~###
L30300: %     ##############################

L30320: %     PROPERTY TYPE: #   ##################
L30330: %     DEPR METHOD  : ##  ########################################~
        ~################
L30350: %     PERCENTAGE   : ###.##
L30360: %     DISPOSAL DATE: ##########   #################
L30370: %DEPRECIATION TABLE: GROUP: ##########  RECOVERY: #####  CONVENTI~
        ~ON: # (##############)  PERIOD: ##

L31000: %                                ------------ ------------ ------~
        ~----- ----------- ----------- ----------- -------------- --------~
        ~---
L31030: %                               ============= ============ ======~
        ~===== =========== =========== =========== ============== ========~
        ~===
L31060: %NUMBER OF ASSETS PRINTED:  ######

L31080: %SUBGROUP:  ########## ############################## ###########~
        ~###################

L31110: %TOTAL FOR SUBGROUP: ########## ##########.## #########.## ######~
        ~##.## ########.## ########.## ########.## ###########.## ########~
        ~.##

L31150: %GROUP:     ########## ############################## ###########~
        ~###################

L31180: %TOTAL FOR GROUP:    ########## ##########.## #########.## ######~
        ~##.## ########.## ########.## ########.## ###########.## ########~
        ~.##

L31220: %     GRAND TOTAL:              ##########.## #########.## ######~
        ~##.## ########.## ########.## ########.## ###########.## ########~
        ~.##

L40000: REM *************************************************************~
            *        I N P U T   M O D E   S C R E E N   P A G E  1     *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME                            *~
            *************************************************************

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "FIXED ASSETS DEPRECIATION SCHEDULE",                  ~
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
                  call "MANUAL" ("FASCHPRT")
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

            if pos("BFS"=depr_category$) = 0% then L50400

            call "TESTRNGE" (first_asset$, last_asset$, lo_asset$,       ~
                             hi_asset$, errormsg$)
            if errormsg$ <> " " then return

            if first_asset$ <> "ALL" then L50150
                fasset$ = "(BEGINNING OF FILE)"
                lasset$ = "(END OF FILE)"
                goto L50320

L50150:     if lo_asset$ = hex(00) then fasset$ = "(BEGINNING OF FILE)"  ~
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

L50400:     errormsg$="Valid Depreciation Categories are 'B', 'F' and 'S'"
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
