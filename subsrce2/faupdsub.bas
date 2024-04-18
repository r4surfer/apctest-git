        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   U   U  PPPP   DDDD    SSS   U   U  BBBB    *~
            *  F      A   A  U   U  P   P  D   D  S      U   U  B   B   *~
            *  FFFF   AAAAA  U   U  PPPP   D   D   SSS   U   U  BBBB    *~
            *  F      A   A  U   U  P      D   D      S  U   U  B   B   *~
            *  F      A   A   UUU   P      DDDD    SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAUPDSUB - THIS PROGRAM ADDS THE CURRENT DEPRECIATION TO  *~
            *            THE ACCUMULATED DEPR, CALCULATES NEW DEPR,     *~
            *            OPTIONALLY DELETES RETIRED ASSETS AND UPDATES  *~
            *            THE FISCAL YEAR BEGINNING AND END DATES.       *~
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
            * 05/21/84 ! ORIGINAL                                 ! NLH *~
            * 09/09/88 ! FILE CHANGES & STANDARDS UPDATE          ! RJM *~
            *          !   Added Report ID "F/A003"               !     *~
            * 09/21/88 ! Swapped Report Columns, Mod RPT Headings ! RJM *~
            *          !   Forced Recalculation of Current Year's !     *~
            *          !   Depreciation.                          !     *~
            * 09/22/88 ! Error line on Report if Depr Table Not   ! RJM *~
            *          !   On File.                               !     *~
            * 09/30/88 ! Changed parameters to FADEPR & added     ! RJM *~
            *          !   Error messages to report & AMT Update. !     *~
            * 10/07/88 ! Added AMT line to the report.            ! TLJ *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "FAUPDSUB" (#01,             /* FAMASTER                   */~
                        #02,             /* FATABLE                    */~
                        #03,             /* SYSFILE2                   */~
                        #07,             /* FATEXT                     */~
                        update%,         /* 1%=UPDATE, 0% = REPORT ONLY*/~
                        del$,            /* YES=DELETE DISPOSED ASSETS */~
                        fiscal_begin$,   /* fiscal begin date          */~
                        fiscal_end$,     /* fiscal end date            */~
                        next_begin$,     /* next fiscal begin date     */~
                        next_end$)       /* next fiscal end date       */

        dim                                                              ~
            begin_date$8,                /* Fixed Asset Fiscal Yr Begin*/~
            blankdate$8,                 /* Blank Date for Comparison  */~
            category$(4)11,              /* Category Description       */~
            company$60,                                                  ~
            date$8,                      /* Date for Screen Display    */~
            del$3,                       /* DO YOU WISH TO DELETE ASSET*/~
            end_date$8,                  /* Fixed Asset Fiscal Yr End  */~
            fiscal_begin$10,             /* Current Fiscal Begin Date  */~
            fiscal_end$10,               /* Current Fiscal End Date    */~
            last_fiscal$10,              /* Last Fiscal Year           */~
            new_fiscal$10,               /* Next Fiscal Year End Date  */~
            next_begin$10,               /* Next Fiscal Begin Date     */~
            next_end$10,                 /* Next Fiscal End Date       */~
            old_accum(4),                /* OLD ACCUMULATED DEPR       */~
            old_current(4),              /* OLD CURRENT DEPRECIATION   */~
            old_fiscal$10,               /* Old Fiscal Year End        */~
            old_method$(4)1,             /* OLD DEPR METHOD            */~
            old_switch$(3)4,             /* OLD YEAR OF SWITCH         */~
            old_ttl_accum(4),            /* TOTAL OLD ACCUM DEPR       */~
            old_ttl_current(4),          /* TOTAL OLD CURRENT DEPR     */~
            other_descr$(3)50,           /* OTHER BASIS REDUCTION DESCR*/~
            readkey$10,                  /* KEY TO FIXED ASSETS MASTER */~
            tdate$8,                     /* Temporary Date             */~
            ttl_accum(4),                /* TOTAL ACCUM DEPR           */~
            ttl_current(4),              /* TOTAL CURRENT DEPR         */~
            time$8,                                                      ~
            ret%(4),                     /* ERROR CODES FROM FADEPR    */~
            userid$3

        dim                                                              ~
            accum_depr(4),               /* ACCUMULATED DEPRECIATION   */~
            amt_flag$1,                  /* ALT MIN TAX, TPI OR NONE ? */~
            amt_group$10,                /* ALT MIN TAX GROUP NAME     */~
            amt_life$5,                  /* ALT MIN TAX CLASS LIFE     */~
            asset_acct$9,                /* ASSET G/L ACCOUNT #        */~
            asset_code$10,               /* ASSET NUMBER               */~
            blank$4,                     /* Blank for SW_YR, FADEPR    */~
            bonus_depr(3),               /* BONUS DEPRECIATION TAKEN   */~
            convention$(3)1,             /* PRORATION CONVENTION       */~
            current_depr(4),             /* CURRENT YEAR'S DEPR        */~
            depr_acct$9,                 /* ACCUM DEPR G/L ACCOUNT #   */~
            depr_method$(4)2,            /* DEPRECIATION METHOD        */~
            descr_1$30,                  /* ASSET DESCRIPTION #1       */~
            descr_2$30,                  /* ASSET DESCRIPTION #2       */~
            dispdate$8,                  /* DISPOSAL DATE (YYYYMMDD)   */~
            disposal_date$10,            /* DISPOSAL DATE              */~
            disposal_descr$30,           /* DISPOSAL DESCRIPTION       */~
            exp_acct$9,                  /* EXPENSE G/L ACCOUNT #      */~
            exp_deduct(3),               /* EXPENSE DEDUCTION TAKEN    */~
            group$(3)10,                 /* GROUP TABLE NAME           */~
            id_code$15,                  /* IDENTIFICATION NUMBER      */~
            in_service$(3)2,             /* PERIOD PUT IN SERVICE      */~
            invoice_number$16,           /* INVOICE NUMBER             */~
            itc_reduct(3),               /* ITC REDUCTION              */~
            life$(3)5,                   /* LIFE IN YEARS              */~
            location$30,                 /* LOCATION                   */~
            orig_basis(4),               /* ORIGINAL BASIS             */~
            other_reduct(3),             /* OTHER REDUCTION            */~
            percentage(3),               /* PERCENT FOR PERCENT OR DB  */~
            property_type$1,             /* PROPERTY TYPE              */~
            purdate$8,                   /* PURCHASE DATE (YYMMDD)     */~
            salvage_value(3),            /* SALVAGE VALUE              */~
            service_date$(3)10,          /* DATE 1ST PLACED IN SERVICE */~
            servdate$(3)8,               /* SERVICE DATE (YYMMDD)      */~
            switch_year$(3)4,            /* YEAR OF SWITCH             */~
            type_code$1,                 /* TYPE CODE                  */~
            vendor_code$9,               /* VENDOR NUMBER              */~
            yy$4

        dim f1%(64)                      /* = 0 IF THE FILE IS OPEN    */
                                         /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  RELEASE VERSION ID SECTION               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************~
            MAT F2% = CON

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            time$ = " "
            call "TIME" (time$)

            call "COMPNAME" (12%, company$, u3%)
            u3% = u3%

            call "EXTRACT" addr("ID", userid$)
            last_fiscal$ = " "
            call "DATEOKC" (fiscal_end$, u3%, "       ")
            call "DATUFMTC" (fiscal_end$)
            tdate$ = fiscal_end$
            call "DATEFMT" (tdate$, 0%, fiscal_end$)
            convert str(fiscal_end$,1%,4%) to yy%, data goto L09670
            yy% = yy% - 1%
            convert yy% to yy$, pic(0000)
            last_fiscal$ = yy$ & str(fiscal_end$,5%,4%)
            call "DATECONV" (fiscal_end$)    /* to internal fmt */
            call "DATFMTC" (fiscal_end$)
            call "DATFMTC" (last_fiscal$)

L09670:     new_fiscal$ = next_end$
            old_fiscal$ = fiscal_end$

            mat old_ttl_accum = zer
            mat ttl_accum = zer
            mat old_ttl_current = zer
            mat ttl_current = zer

        REM *************************************************************~
            *          SEQUENTIAL READ THROUGH ASSET MASTER FILE        *~
            *                                                           *~
            *************************************************************

            select printer(134)
            call "SETPRNT" ("F/A003", " ", 0%, 0%)

            call "DATUFMTC" (fiscal_begin$)
            call "DATUFMTC" (fiscal_end$)
            call "DATUFMTC" (next_begin$)
            call "DATUFMTC" (next_end$)

            line% = 57%
            category$(1%) = "BOOK"
            category$(2%) = "FEDERAL"
            category$(3%) = "STATE/LOCAL"
            category$(4%) = "AMT"
            readkey$ = hex(00)
L11180:     call "PLOWNEXT" (#01, readkey$, 0%, f1%(1))
                 if f1%(1) = 0 then L18000     /* CHANGE DATE AND END*/
            del_flg% = 0%
            gosub L30000                  /* GET AND FORMAT STATEMENTS */
            if type_code$ <>"3" then L11180 /* UPDATE ASSETS ONLY */
            if dispdate$ = " " or dispdate$ = blankdate$ then L11330
            if del$ <> "YES" then L11330  /* DON'T DELETE RETIRED ASSETS*/
            if dispdate$ > str(fiscal_end$,1,8) then L11330
                del_flg% = 1%
                gosub L20000
                if update% <> 1% then L11180
                call "READ101" (#01, readkey$, f1%(1))
                delete #01
                call "DELETE" (#07, asset_code$,10%)
                goto L11180
L11330:     for i% = 1% to 3%

                disposal_date$ = str(dispdate$,1,8)
                service_date$(i%) = str(servdate$(i%),1,8)
                call "DATFMTC" (disposal_date$)
                call "DATFMTC" (service_date$(i%))

*        Recalculate THIS YEAR's Depreciation just to be safe.

            if pos(depr_method$(i%) = "9") > 0% then L11620
                call "FADEPR" (asset_code$,                              ~
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
                               ret%(i%),                                 ~
                               #02)

L11620:         old_method$(i%) = depr_method$(i%)
                old_switch$(i%) = switch_year$(i%)
                old_accum(i%)   = accum_depr(i%)
                old_current(i%) = current_depr(i%)

                accum_depr(i%) = accum_depr(i%) + current_depr(i%)

                call "FADEPR" (asset_code$,                              ~
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
                               next_begin$,                              ~
                               next_end$,                                ~
                               u3%,                                      ~
                               #02)

            next i%

*        Get Depreciation For Alt. Minimum Tax Book
            if pos("12" = amt_flag$) = 0% then L12590
                depr_method$(4) = " "
                if amt_flag$ = "1" then depr_method$(4) = "7"            ~
                                   else depr_method$(4) = "S"

                zro = 0
                blank$ = " "
                call "FADEPR" (asset_code$,                              ~
                               property_type$,                           ~
                               dispdate$,                                ~
                               servdate$(2%),                            ~
                               amt_group$,                               ~
                               convention$(2%),                          ~
                               in_service$(2%),                          ~
                               amt_life$,                                ~
                               depr_method$(4),                          ~
                               zro,                                      ~
                               blank$,                                   ~
                               orig_basis(4%),                           ~
                               zro,                                      ~
                               zro,                                      ~
                               zro,                                      ~
                               zro,                                      ~
                               zro,                                      ~
                               accum_depr(4%),                           ~
                               current_depr(4%),                         ~
                               fiscal_begin$,                            ~
                               fiscal_end$,                              ~
                               ret%(4),                                  ~
                               #02)

                old_method$(4%) = depr_method$(4%)
                old_accum(4%)   = accum_depr(4%)
                old_current(4%) = current_depr(4%)

                accum_depr(4%)  = accum_depr(4%) + current_depr(4%)

                zro = 0
                blank$ = " "
                call "FADEPR" (asset_code$,                              ~
                               property_type$,                           ~
                               dispdate$,                                ~
                               servdate$(2%),                            ~
                               amt_group$,                               ~
                               convention$(2%),                          ~
                               in_service$(2%),                          ~
                               amt_life$,                                ~
                               depr_method$(4),                          ~
                               zro,                                      ~
                               blank$,                                   ~
                               orig_basis(4%),                           ~
                               zro,                                      ~
                               zro,                                      ~
                               zro,                                      ~
                               zro,                                      ~
                               zro,                                      ~
                               accum_depr(4%),                           ~
                               current_depr(4%),                         ~
                               next_begin$,                              ~
                               next_end$,                                ~
                               u3%,                                      ~
                               #02)

L12590:     gosub L20000
            if update% <> 1% then L11180
            gosub L32000
            goto L11180


L18000: REM UPDATE THE SYSFILE2 DATE RECORD HERE
            if update% <> 1% then L19000
            begin_date$ = str(next_begin$,1,8)
            end_date$ = str(next_end$,1,8)
            cnv_dflt$ = "1"
            call "READ101" (#03, "SWITCHS.FA", f1%(3))
            if f1%(3) = 1% then get #03, using L18037, cnv_dflt$
L18037:         FMT POS(37), CH(1)
            put #03, using L18060, "SWITCHS.FA", begin_date$, end_date$,  ~
                                  cnv_dflt$, " "
L18060:     FMT CH(20), CH(8), CH(8), CH(1), CH(463)
            if f1%(3) = 0% then write #03                                ~
                           else rewrite #03


L19000: REM PRINT THE GRAND TOTALS AND THE NUMBER OF ASSETS
            if no_of_assets% = 0% then L19130
            if line% + 7% <= 57% then L19050
                line% = line% + 7%
                gosub L28090
L19050:     print
            print using L29350
            for i% = 1% to 4%
                print using L29380, category$(i%),                        ~
                               old_ttl_accum(i%), old_ttl_current(i%),   ~
                               ttl_accum(i%), ttl_current(i%)
            next i%
            print
L19130:     print using L29440, no_of_assets%
            close printer
            call "SETPRNT" ("F/A003", " ", 0%, 1%)
            goto L65000

L20000: REM *************************************************************~
            *        PRINT THE ASSET AND ACCUMULATE TOTALS              *~
            *                                                           *~
            *************************************************************

            gosub L28000
            if line% + 5% <= 57% then L23030
                line% = line% + 5%
                gosub L28090
L23030:     print using L29260, asset_code$, descr_1$, descr_2$
            if del_flg% = 1% then L23300
            for i% = 1% to 4%
                if i% <> 4% then L23140
                /* Print AMT Line */
                  if pos("12" = amt_flag$) = 0% then L23240
                  print using L29290, category$(i%),                      ~
                                     old_method$(i%), depr_method$(i%),  ~
                                     " ", " ",                           ~
                                     old_accum(i%), old_current(i%),     ~
                                     accum_depr(i%), current_depr(i%)
                  goto L23190
L23140:         print using L29290, category$(i%),                        ~
                                   old_method$(i%), depr_method$(i%),    ~
                                   old_switch$(i%), switch_year$(i%),    ~
                                   old_accum(i%), old_current(i%),       ~
                                   accum_depr(i%), current_depr(i%)
L23190:         old_ttl_accum(i%) = old_ttl_accum(i%) + old_accum(i%)
                ttl_accum(i%) = ttl_accum(i%) + accum_depr(i%)
                old_ttl_current(i%)=old_ttl_current(i%) + old_current(i%)
                ttl_current(i%) = ttl_current(i%) + current_depr(i%)
                on ret%(i%) gosub L23370, L23400, L23430, L23460,, L23490
L23240:     next i%

            print
            line% = line% + 6%
            goto L23340

L23300:     print using L29320, disposal_date$, disposal_descr$
            print
            line% = line% + 4%

L23340:     no_of_assets% = no_of_assets% + 1%
            return

L23370:     print using L24040
            return

L23400:     print using L24000
            return

L23430:     print using L24080
            return

L23460:     print using L24120
            return

L23490:     print using L24150
            return

L24000: % *****************************  DEPRECIATION TABLE NOT ON FILE! ~
        ~& DEPRECIATION AMOUNT IS SET TO 0:  MANUALLY CALCULATE DEPRECIATI~
        ~ON

L24040: % *****************************  ASSET LIFE/RECOVERY IS INVALID! ~
        ~& DEPRECIATION AMOUNT IS SET TO 0:  MANUALLY CALCULATE DEPRECIATI~
        ~ON

L24080: % *****************************  ASSET'S IN SERVICE DATE IS OUTSI~
        ~DE THE EFFECTIVE DATE RANGE OF THE DEPRECIATION TABLE SELECTED.

L24120: % *****************************  ASSET'S PROPERY TYPE IS NOT SUPP~
        ~ORTED BY THE DEPRECIATION TABLE SELECTED.  FIX THE ASSET OR TABLE

L24150: % *****************************  ASSET'S DEPRECIATION METHOD IS N~
        ~OT VALID.

L28000: REM *************************************************************~
            *            PRINT THE PAGE HEADING                         *~
            *                                                           *~
            *************************************************************


            if line% <= 55 then return

L28090:     pagenr% = pagenr% + 1%
            print page
            print using L29040, date$, time$, company$
            if update% = 1% then L28150
                 print using L29110, userid$, pagenr%
            goto L28170
L28150:          print using L29080, userid$, pagenr%
L28170:     print
            print using L29150, old_fiscal$, new_fiscal$
            print using L29152
            print using L29160, last_fiscal$, old_fiscal$, old_fiscal$,   ~
                               new_fiscal$
            print using L29190
            print using L29220
            line% = 8%
            return

        REM *************************************************************~
            *            REPORT FORMAT STATEMENTS                       *~
            *************************************************************

L29040: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                      FAUPDSUB: F/A~
        ~003

L29080: %BY: ###                             F I X E D   A S S E T S   Y ~
        ~E A R   E N D   R E P O R T                             PAGE:  ##~
        ~##

L29110: %BY: ###                   F I X E D   A S S E T S   P R O   F O ~
        ~R M A   Y E A R   E N D   R E P O R T                   PAGE:  ##~
        ~##

L29150: %OLD FISCAL YEAR END ##########   NEW FISCAL YEAR END ##########

L29152: %                                                             BEG~
        ~ BALANCE @      CURRENT       ACCUMULATED       NEXT YEAR'S

L29160: %ASSET CODE/     OLD DEPR  NEW DEPR  OLD YEAR   NEW YEAR        #~
        ~#########     ##########       ##########        ##########

L29190: %DESCRIPTION      METHOD    METHOD   OF SWITCH  OF SWITCH       A~
        ~CCUM DEPR    DEPRECIATION     DEPRECIATION      DEPRECIATION

L29220: %________________________________________________________________~
        ~_________________________________________________________________~
        ~___

L29260: %##########  ############################## #####################~
        ~#########

L29290: %    ###########    #          #      ####       ####      ######~
        ~####.##   ##########.##    ##########.##     ##########.##

L29320: %    DISPOSAL DATE: ##########  ##############################   ~
        ~****  DELETED  ****

L29350: %****  GRAND TOTALS

L29380: %    ###########                                           ######~
        ~####.##   ##########.##    ##########.##     ##########.##

L29440: %NUMBER OF ASSETS PRINTED:  ######


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

L30000: REM *************************************************************~
            *               DATA I/O - FIXED ASSETS MASTER FILE         *~
            *                                                           *~
            *************************************************************

            get #01, using L35300,                                        ~
                     depr_method$(1),                                    ~
                     depr_method$(2),                                    ~
                     depr_method$(3),                                    ~
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
                     disposal_descr$,                                    ~
                     vendor_code$,                                       ~
                     invoice_number$,                                    ~
                     group$(),                                           ~
                     life$(),                                            ~
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
                     amt_flag$,                                          ~
                     amt_group$,                                         ~
                     amt_life$,                                          ~
                     orig_basis(4),                                      ~
                     current_depr(4),                                    ~
                     accum_depr(4)

            if pos("12" = amt_flag$) <> 0% then L31860
               amt_group$, amt_life$, amt_flag$ = " "
               orig_basis(4), current_depr(4), accum_depr(4) = 0
L31860:     return

L32000: REM WRITE UPDATED RECORD
            call "READ101" (#01, readkey$, f1%(1))
            put #01, using L32140,                                        ~
                     depr_method$(1),                                    ~
                     depr_method$(2),                                    ~
                     depr_method$(3),                                    ~
                     switch_year$(),                                     ~
                     accum_depr(1),                                      ~
                     accum_depr(2),                                      ~
                     accum_depr(3),                                      ~
                     current_depr(1),                                    ~
                     current_depr(2),                                    ~
                     current_depr(3),                                    ~
                     amt_flag$,                                          ~
                     amt_group$,                                         ~
                     amt_life$,                                          ~
                     orig_basis(4),                                      ~
                     current_depr(4),                                    ~
                     accum_depr(4)

L32140:     FMT CH(2),CH(2),CH(2),POS(342),3*CH(4),POS(648), 6*PD(14,4), ~
                POS(702), CH(1), CH(10), CH(5), 3*PD(14,4)

            rewrite #01
            return

        REM *************************************************************~
            *                 FORMAT STATEMENTS                         *~
            *                                                           *~
            *************************************************************

L35300: FMT                      /* FILE: FAMASTER                     */~
            CH(2),               /* DEPRECIATION METHOD                */~
            CH(2),               /* DEPRECIATION METHOD                */~
            CH(2),               /* DEPRECIATION METHOD                */~
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
            XX(8),               /* PURCHASE PRICE                     */~
            XX(8),               /* INVESTMENT TAX CR TAKEN            */~
            XX(8),               /* DISPOSAL PRICE                     */~
            CH(30),              /* DISPOSAL DESCRIPTION               */~
            CH(9),               /* VENDOR CODE                        */~
            CH(16),              /* INVOICE NUMBER                     */~
            3*CH(10),            /* GROUP TABLE NAME                   */~
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
            3*CH(50),            /* OTHER BASIS REDUCT DESCRIPTION     */~
            3*PD(14,4),          /* ACCUMULATED DEPRECIATION           */~
            3*PD(14,4),          /* CURRENT DEPRECIATION               */~
            3*CH(2),             /* IN SERVICE PERIOD                  */~
            CH(1),               /* ALTERNATE MINIMUM TAX TYPE FLAG    */~
            CH(10),              /* AMTI GROUP TABLE NAME              */~
            CH(5),               /* AMTI CLASS LIFE                    */~
            PD(14,4),            /* AMTI ADJUSTED BASIS                */~
            PD(14,4),            /* AMTI CURRENT DEPRECIATION          */~
            PD(14,4),            /* AMTI ACCUMULATED DEPRECIATION      */~
            XX(159)              /* FILLER                             */

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
