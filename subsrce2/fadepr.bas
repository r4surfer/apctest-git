        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF   AAA   DDDD   EEEEE  PPPP   RRRR                  *~
            *  F      A   A  D   D  E      P   P  R   R                 *~
            *  FFFF   AAAAA  D   D  EEEE   PPPP   RRRR                  *~
            *  F      A   A  D   D  E      P      R   R                 *~
            *  F      A   A  DDDD   EEEEE  P      R   R                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FADEPR   - THIS EXTERNAL SUBROUTINE CALCULATES THE        *~
            *            DEPRECIATION FOR THE FIXED ASSETS SYSTEM.      *~
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
            * 05/08/84 ! ORIGINAL                                 ! NHC *~
            * 09/06/88 ! Modified Depreciation Table Lookup, usage! RJM *~
            *          !   Fixed "Sum of the Years Digits" Method !     *~
            *          !   Removed ACRS Alternate, now use Table. !     *~
            * 09/21/88 ! Modified ACRS_TABLE; Proration (OS_YR)   ! RJM *~
            *          !   of the % from the table only occurs for!     *~
            *          !   Depreciation Method = '6' in the year  !     *~
            *          !   of Disposal for Property Types R,L & X !     *~
            *          !   only.  Never Prorates in Year 1.       !     *~
            * 09/22/88 ! FIXED DECLINING BALANCE METHOD           ! RJM *~
            * 09/26/88 ! Added Method AMT_STRAIGHTLINE & Expanded ! RJM *~
            *          !   Arrays passed in changed to Scalars    !     *~
            * 09/29/88 ! Changed to expect ALL dates passed in to ! RJM *~
            *          !   be Unformatted; Added error return code!     *~
            *          !   Added test to end if beyond disp. year !     *~
            * 10/07/88 ! Traps for Method '9' and exits if true   ! RJM *~
            * 12/10/91 ! PRRs 11717, 12172- Removed leap year days! JBK *~
            *          !   from calculation of Straight-line depr.!     *~
            * 01/25/93 ! PRR 12737 - Corrected calc on type 7's   ! MLJ *~
            *          !   when fiscal year does not fall within  !     *~
            *          !   the same calendar year.                !     *~
            * 08/31/93 ! More mods for table calculation of depr. ! MLJ *~
            * 07/15/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "FADEPR"(asset_code$,                                        ~
                     property_type$,                                     ~
                     disposal_date$,                                     ~
                     service_date$,                                      ~
                     group$,                                             ~
                     convention$,                                        ~
                     in_service$,                                        ~
                     life$,                                              ~
                     depr_method$,                                       ~
                     percentage,                                         ~
                     switch_year$,                                       ~
                     orig_basis,                                         ~
                     salvage_value,                                      ~
                     itc_reduct,                                         ~
                     bonus_depr,                                         ~
                     exp_deduct,                                         ~
                     other_reduct,                                       ~
                     accum_depr,                                         ~
                     current_depr,                                       ~
                     fiscal_begin$,                                      ~
                     fiscal_end$,                                        ~
                     ret%,            /*     0 = SUCCESSFUL            */~
                                      /*     1 = LIFE IS BAD OR 0      */~
                                      /*     2 = DEPR TABLE NOT FOUND  */~
                                      /*     3 = TBL DATES <> SERV DT  */~
                                      /*     4 = TBL TYPES <> PROPERTY */~
                                      /*     5 = BAD FISCAL DATES      */~
                                      /*     6 = BAD DEPR METHOD       */~
                     #02)                /* "FATABLE"-DEPR % TABLE     */

        dim                                                              ~
            acrs(60),                    /* ACRS PERCENTAGE ARRAY      */~
            anniv_date$10,               /* ANNIVERSARY DATE           */~
            asset_code$10,               /* ASSET CODE                 */~
            begdate$6,                   /* Beginning Date             */~
            blankdate$8,                 /* Blank date for comparison  */~
            convention$1,                /* PRORATION CONVENTION       */~
            depr_method$2,               /* DEPRECIATION METHOD        */~
            disdate$10,                  /* DISPOSAL DATE (YYMMDD)     */~
            disposal_date$10,            /* DISPOSAL DATE              */~
            enddate$8,                   /* Ending Date                */~
            ed$8,                        /* ENDING DATE IN THE YEAR    */~
            fiscal_begin$10,             /* BEGINNING OF FISCAL YEAR   */~
            fiscal_end$10,               /* END OF FISCAL YEAR         */~
            group$10,                    /* GROUP TABLE NAME           */~
            in_service$2,                /* IN SERVICE PERIOD          */~
            julian_date$7,               /* For Julian calculations    */~
            life$5,                      /* LIFE IN YEARS              */~
            md$8,                        /* BEGINNING DATE IN THE YEAR */~
            proptypes$7,                 /* PROPERTY TYPES (FATABLE)   */~
            property_type$1,             /* PROPERTY TYPE              */~
            readkey$50,                  /* KEY TO READ FATABLE        */~
            sdate$10,                    /* SERVICE DATE - YYMMDD      */~
            service_date$10,             /* DATE PLACED IN SERVICE     */~
            switch_year$4,               /* YEAR SWITCHED TO S/L       */~
            tdate$8,                     /* Temporary date             */~
            temp1$8,                                                     ~
            temp2$8,                                                     ~
            temp3$8,                                                     ~
            yy$4                         /* ANNIVERSARY DATE YEAR      */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01932
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L01932: REM *************************************************************
            mat f2% = con

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

            ret% = 0%
            call "STRING" addr("LJ", depr_method$, 2%)

            if depr_method$ = "9" then L65000   /* MANUAL */

            life = 0
            current_depr, remain_basis = 0
            rate, acrs_basis = 0

            convert life$ to life, data goto L09250
            if life = 0 then L09250
            goto L09650
L09250:         ret% = 1%
                goto L65000

L09650:     sdate$ = service_date$
            if sdate$ <= fiscal_end$ then L09702
            goto L65000

L09702:     disdate$ = disposal_date$

            if sdate$ < fiscal_begin$ then md$ = fiscal_begin$           ~
                                      else md$ = sdate$
            if disdate$ < fiscal_end$ then ed$ = disdate$                ~
                                      else ed$ = fiscal_end$
            if disdate$ = " " or disdate$ = blankdate$ then ed$ = fiscal_end$
            if disdate$ <> " " and disdate$ <> blankdate$ and ~
               disdate$ < fiscal_begin$ then L65000
            gosub'099(fiscal_begin$, fiscal_end$)

            if pos("12" = depr_method$) = 0% then L09890
            gosub'088(fiscal_begin$, fiscal_end$)
            dys% = dys% - leap_day%

L09890:     days = dys%
            days = days + 1
            if days <= 1 then days = 0
            if days = 0 or err% <> 0% then ret% = 5%   /* ERROR */
            if ret% > 0% then L65000                    /* ERROR */

        REM *************************************************************~
            *                                                           *~
            *          CALL THE APPROPRIATE SUBROUTINE DEPENDING        *~
            *               ON THE TYPE OF DEPRECIATION.                *~
            *                                                           *~
            *************************************************************~

            if pos("123456789S" = depr_method$) = 0% then ret% = 6%

            if depr_method$ = "1" or                                     ~
               depr_method$ = "2" then gosub straightline
            if depr_method$ = "3" then gosub declinbalance
            if depr_method$ = "4" then gosub dbwithswitch
            if depr_method$ = "5" then gosub sum_of_years_digits
            if depr_method$ = "6" then gosub acrs_table
            if depr_method$ = "7" then gosub acrs_table
            if depr_method$ = "8" then gosub percent
            if depr_method$ = "S" then gosub amt_straightline
            goto L65000

        REM *************************************************************~
            *    D I F F E R E N C E   B E T W E E N   2   D A T E S    *~
            *************************************************************

        def fn'099(temp1$, temp2$)
*        Special handling for year 2000, it should be a leap year
            tdate$ = temp1$
            call "DATEFMT" (tdate$, 0%, temp1$)
            tdate$ = temp2$
            call "DATEFMT" (tdate$, 0%, temp2$)
            c1_yr%, c2_yr% = 0
            convert str(temp1$,1%,4%) to c1_yr%
            convert str(temp2$,1%,4%) to c2_yr%
L12045:     if c1_yr% <> 2000% and c2_yr% <> 2000% then L12055
                c1_yr% = c1_yr% - 4%         /* Reduce YYYY to 1900's */
                c2_yr% = c2_yr% - 4%         /* Reduce YYYY to 1900's */
                convert c1_yr% to str(temp1$,1%,4%), pic(0000)
                convert c2_yr% to str(temp2$,1%,4%), pic(0000)
                goto L12045
L12055:     call "DATECONV" (temp1$)
            call "DATECONV" (temp2$)
            call "DATE" addr("G-", temp1$, temp2$, dys%, err%)
            return

        REM Subroutine to try and elimanate Leap year days for SL
        def fn'088(temp1$, temp2$)

            tdate$ = temp1$
            call "DATEFMT" (tdate$, 0%, temp1$)
            tdate$ = temp2$
            call "DATEFMT" (tdate$, 0%, temp2$)

            leap_day%, c1_yr%, c2_yr%, c3_yr%, y1_yr%, y2_yr% = 0
            temp3$ = " "

*        Year 2000 is special case, reduce both the years by 4 until
*        both years are in the period 1901 to 1999

L12280:     convert str(temp1$,1,4) to c1_yr%
            convert str(temp2$,1,4) to c2_yr%
            if c1_yr% <> 2000% and c2_yr% <> 2000% then L12370
                c1_yr% = c1_yr% - 4%
                c2_yr% = c2_yr% - 4%
                convert c1_yr% to str(temp1$,1,4), pic(0000)
                convert c2_yr% to str(temp2$,1,4), pic(0000)
                goto L12280

L12370:     convert str(temp1$,3,2) to y1_yr%
            convert str(temp2$,3,2) to y2_yr%

            if y2_yr% >= y1_yr% then L12470

            c1_yr% = c1_yr% - (y2_yr% + 1%)
            c2_yr% = 1999
            convert c1_yr% to str(temp1$,1,4), pic(0000)
            convert c2_yr% to str(temp2$,1,4), pic(0000)

L12470:     c3_yr% = c1_yr%
            convert c3_yr% to str(temp3$,1,4), pic(0000)
            temp3$ = temp3$ & "0301"

            call "DATECONV" (temp1$)
            call "DATECONV" (temp3$)
            call "DATE" addr("G-", temp1$, temp3$, t_days%, day_ret%)
            tdate$ = temp1$
            call "DATEFMT" (tdate$, 0%, temp1$)
            tdate$ = temp3$
            call "DATEFMT" (tdate$, 0%, temp3$)

            if t_days% > 0% then L12590

L12560:     c3_yr% = c3_yr% + 1%
            if c3_yr% > c2_yr% then L12710
            convert c3_yr% to str(temp3$,1,4), pic(0000)

L12590:
            call "DATECONV" (temp2$)
            call "DATECONV" (temp3$)
            call "DATE" addr("G-", temp3$, temp2$, t_days%, day_ret%)
            tdate$ = temp2$
            call "DATEFMT" (tdate$, 0%, temp1$)
            tdate$ = temp3$
            call "DATEFMT" (tdate$, 0%, temp3$)

            if t_days% <= 0%  then L12710

            call "DATECONV" (temp3$)
            call "DATE" addr("GJ", temp3$, julian_date$, day_ret%)
            tdate$ = temp3$
            call "DATEFMT" (tdate$, 0%, temp3$)
            call "DATJULCV" (julian_date$)

            convert str(julian_date$,5%,3%) to julian_day%
            if julian_day% = 61% then leap_day% = leap_day% + 1%
            goto L12560

L12710:     call "DATECONV" (temp1$)
            call "DATECONV" (temp2$)
            call "DATECONV" (temp3$)
            return

        REM *************************************************************~
            *                                                           *~
            *            DEPRECIATION CALCULATION SUBROUTINES           *~
            *                                                           *~
            *************************************************************~

        straightline
            gosub'099(sdate$, fiscal_begin$)
            gosub'088(sdate$, fiscal_begin$)
            dys% = dys% - leap_day%
            days_used% = dys%
            if days_used% < 0% or err% <> 0% then days_used% = 0%

            yrs_used     = days_used% / days
*           IF YRS_USED - INT(YRS_USED) = 0 THEN 30074
*           IF YRS_USED - INT(YRS_USED) <= .001 THEN YRS_USED = 0
            remain_yrs   = life - yrs_used
            if remain_yrs <= 0 then L30280  /* END IF NO REMAINING LIFE */

            gosub'099(md$, ed$)
            gosub'088(md$, ed$)
            dys% = dys% - leap_day%
            os_days% = dys%
            os_days% = os_days% + 1%
            if os_days% <= 0% or err% <> 0% then L30270 /* END */

            basis = orig_basis - salvage_value - itc_reduct -            ~
                    bonus_depr - exp_deduct - other_reduct
            remain_basis = basis - accum_depr

            os_yr = os_days% / days
            if os_yr > remain_yrs then os_yr = remain_yrs

          if remain_yrs <> 0 then                                        ~
             current_depr = round(remain_basis * (os_yr / remain_yrs),2%)

*        CURRENT_DEPR = MIN(CURRENT_DEPR, ROUND(BASIS / LIFE,2%))

          if current_depr > remain_basis then current_depr = remain_basis
L30270:     return
L30280:

        declinbalance
            gosub'099(sdate$, fiscal_begin$)
            days_used% = dys%
            if days_used% < 0% or err% <> 0% then days_used% = 0%
            yrs_used     = days_used% / days
            remain_yrs   = life - yrs_used
            if remain_yrs <= 0 then L30500

            gosub'099(md$, ed$)
            os_days% = dys%
            os_days% = os_days% + 1%
            if os_days% <= 0% or err% <> 0% then L30500
            os_yr = os_days% / days
            if os_yr > remain_yrs then os_yr = remain_yrs
            if yrs_used >= 1 then os_yr = 1

            remain_basis = orig_basis - itc_reduct - bonus_depr -        ~
                           exp_deduct - other_reduct - accum_depr
            if life <> 0 then rate = (1 / life)
            if remain_basis <= salvage_value then L30500
            if percentage <= 0 then percentage = 100

            current_depr = round((remain_basis * os_yr * rate            ~
                                  * percentage * .01), 2%)

            if current_depr > remain_basis - salvage_value               ~
                  then current_depr = remain_basis - salvage_value
L30500:     return


        sum_of_years_digits

            tdate$ = fiscal_begin$
            call "DATEFMT" (tdate$, 0%, fiscal_begin$)
            tdate$ = sdate$
            call "DATEFMT" (tdate$, 0%, sdate$)
            anniv_date$ = str(fiscal_begin$,1,4) & str(sdate$,5,4)
            if anniv_date$ >= fiscal_begin$ then L30630

            convert str(fiscal_begin$,1,4) to yy
            yy = yy + 1
            convert yy to yy$, pic(0000)
            anniv_date$ = yy$ & str(sdate$,5,4)

            call "DATECONV" (anniv_date$)        /* to internal fmt */
            call "DATECONV" (fiscal_begin$)      /* to internal fmt */
            call "DATECONV" (sdate$)             /* to internal fmt */

L30630:     gosub'099(sdate$, fiscal_begin$)
            days_used% = dys%
            if days_used% < 0% or err% <> 0% then days_used% = 0%

            basis = orig_basis - salvage_value - itc_reduct -            ~
                    bonus_depr - exp_deduct    - other_reduct

            remain_basis = basis - accum_depr

            if ed$ < anniv_date$ then temp2$ = ed$                       ~
                                 else temp2$ = anniv_date$

            gosub'099(fiscal_begin$, temp2$)
            os2_yr = 0
            os2_days% = dys%
            if os2_days% < 0% or err% <> 0% then os2_days% = 0%
            os2_yr = os2_days% / days

            gosub'099(anniv_date$, ed$)
            os_yr = 0
            os_days% = dys%
            if os_days% < 0% or err% <> 0% then os_days% = 0%            ~
                                           else os_days% = os_days% + 1%

            os_yr        = os_days% / days
            yrs_used     = days_used% / days
            remain_yrs%  = int(life - yrs_used)
            if remain_yrs% < 0 then remain_yrs% = 0
            if os_yr > remain_yrs% then os_yr = remain_yrs%

                /* SUM OF THE YEARS DIGITS CALCULATION */
            temp% = int(life + .99)
            syd% = (temp% * (temp% + 1%)) / 2%

            if syd% <> 0% then                                           ~
            current_depr = round((basis * os_yr * remain_yrs% / syd%), 2)

            if syd% <> 0% and os2_yr > 0 and remain_yrs% < life then     ~
            current_depr = current_depr + round(((basis * os2_yr) *      ~
                                          (remain_yrs% + 1%) / syd%), 2)
            if current_depr > remain_basis then                          ~
                                           current_depr = remain_basis
            return


        percent
            gosub'099(sdate$, fiscal_begin$)
            days_used% = dys%
            if days_used% < 0% or err% <> 0% then days_used% = 0%
            yrs_used     = days_used% / days
            remain_yrs   = life - yrs_used
            if remain_yrs <= 0 then L31370

            gosub'099(md$, ed$)
            os_days% = dys%
            os_days% = os_days% + 1%
            if os_days% <= 0% or err% <> 0% then L31370

            remain_basis = orig_basis - salvage_value - itc_reduct -     ~
                           bonus_depr - exp_deduct - other_reduct

            if days <> 0 then os_yr = os_days% / days
            if os_yr > remain_yrs then os_yr = remain_yrs
            current_depr =                                               ~
                round(remain_basis * os_yr * percentage * .01, 2%)
            if current_depr > remain_basis - accum_depr                  ~
                then current_depr = remain_basis - accum_depr
            if current_depr < 0 then current_depr = 0
L31370:     return


        dbwithswitch
            gosub declinbalance
            db_depr = current_depr
            gosub straightline
            sl_depr = current_depr
            if sl_depr > db_depr then L31500
            current_depr = db_depr
            return

L31500:     switch_year$ = str(fiscal_end$,1,4)
            depr_method$ = "2"
            return

        acrs_table
            os_yr = 1
            if depr_method$ = "7" then L32220   /* NOT ACRS !!  */
               if pos("RXLP" = property_type$) = 0% then L32220
                  if property_type$ <> "P" then L32050
                     if ed$ < fiscal_end$ then L32470  /* DISPOSAL? */
                        goto L32220
L32050:           if ed$ >= fiscal_end$ then L32220 /* Year Of Disposal?*/
                     gosub'099(fiscal_begin$, ed$)
                     os_days% = dys%
                     os_days% = os_days% + 1%
                     if os_days% < 0% or err% <> 0% then L32470
                     if days <> 0 then os_yr = os_days% / days


L32220:     acrs_basis = orig_basis - itc_reduct - exp_deduct            ~
                                    - bonus_depr - other_reduct
            recovery_yr% = 1%
            if sdate$ >= fiscal_begin$ and sdate$ <= fiscal_end$         ~
                                                               then L32330
            if sdate$ > fiscal_end$ then return  /* Premature */
            tdate$ = sdate$
            call "DATEFMT" (tdate$, 0%, sdate$)
            tdate$ = fiscal_end$
            call "DATEFMT" (tdate$, 0%, fiscal_end$)
            convert str(sdate$,1%,4%) to y1%, data goto L32470
            convert str(fiscal_end$,1%,4%) to y2%, data goto L32470
            call "DATECONV" (fiscal_end$):       /* to internal fmt */

            recovery_yr% = (y2% - y1%) + 1%
            tdate$ = fiscal_begin$
            call "DATEFMT" (tdate$, 0%, fiscal_begin$)
            convert str(fiscal_begin$,1%,4%) to y3%, data goto L32470
            call "DATECONV" (fiscal_begin$):       /* to internal fmt */
                if y3% = y2% then L32330
                    if y3% = y1% then L32330
        REM  Fiscal year not within same calendar year.  Redeuce recovery~
             year if service date is in the 1st half of the fical year.

            tdate$ = fiscal_begin$
            call "DATEFMT" (tdate$, 0%, fiscal_begin$)
            convert str(fiscal_begin$,5%,2%) to m1%, data goto L32470
            call "DATECONV" (fiscal_begin$):       /* to internal fmt */
                m2% = m1% + 6%
            convert str(sdate$,5%,2%) to sd%, data goto L32470
            call "DATECONV" (sdate$):            /* to internal fmt */
            if sd% >= m1% and sd% <= m2% then                            ~
                recovery_yr% = recovery_yr% - 1%

L32330:     readkey$ = str(group$) & str(life$) & str(convention$) &     ~
                       str(in_service$)
            call "READ100" (#02, readkey$, f1%(2%))
                if f1%(2%) = 0% then ret% = 2%
                if f1%(2%) = 0% then L32470

            get #02, using L32390, begdate$, enddate$, acrs(), proptypes$
L32390:         FMT POS(19), 2*CH(8), 60*PD(7,4), CH(7)
            tdate$ = enddate$
            call "DATEFMT" (tdate$, 0%, enddate$)
            if enddate$ < "19800101" then enddate$ = "99999999"
            call "DATECONV" (enddate$):          /* to internal fmt */
            if sdate$ < begdate$ or sdate$ > enddate$ then ret% = 3%
            if pos(proptypes$ = property_type$) = 0%  then ret% = 4%
            if ret% > 0% then return
            life = round(life + 1.4, 0)

            if recovery_yr% > 60% or recovery_yr% > life then L32470
            current_depr = round( (acrs_basis * acrs(recovery_yr%) * .01 ~
                                   * os_yr), 2%)
            if current_depr > acrs_basis - accum_depr                    ~
                        then  current_depr = acrs_basis - accum_depr
L32470:     return



        amt_straightline

            if sdate$ = md$ then mult = .5 else mult = 1

            current_depr = round(orig_basis * mult * ( 1/life ), 2)

            if current_depr > orig_basis - accum_depr then               ~
               current_depr = orig_basis - accum_depr
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

            end
