*       ****************************************************************~
*                           ( As of 11/11/97 - RHH )                   *~
*             Last Mod --   ( As of 10/20/2009 - CMG )                 *~
*        (Same as (APCCSTSB) with Exception that RAW_UNIT  is returned)*~
*                                                                      *~
*        APCCST9B - Subroutine to Calculate the Costs Associated with  *~
*                   a Raw Material Part Number.                        *~
*                                                                      *~
*                   (A) RAW_COST - Is the Calculated Material Cost     *~
*                                  using the Unit Cost (RAW_UNIT).     *~
*                   (B) RAW_FRT  - Is the Calculated Freight Cost of   *~
*                                  Material using the Freight Cost per *~
*                                  Unit Percent (FRT_PCNT) '+'         *~
*                   (C) RAW_VINLY- Is the Calculated Amount of Vinyl   *~
*                                  Discount using the Vinyl Discount   *~
*                                  Percent (VINYL_PCNT).               *~
*                                                                      *~
*                   RAW$      = Raw Material Part Number               *~
*                   RAW_QTY   = Raw Material Quantity                  *~
*                   RAW_CODE% = Raw Material Units Code                *~
*                               (1) = Raw Material Piece               *~
*                               (2) = Cost Per Sq. Foot                *~
*                               (3) = Glass Square Inch                *~
*                               (4) = Cost Per Foot                    *~
*                               (5) = Cost Per Inch                    *~
*                         (N/A) (6) = Cost Per U.I.                    *~
*                               (7) = Cost Per Pound                   *~
*                               (8) = Cost Per Ounce                   *~
*                   RAW_COST  = Cost of Material                       *~
*                   RAW_DESC$ = Description of Raw Material            *~
*                   RAW_UNIT  = Unit Cost of Raw Material              *~
*                   RAW_FRT   = Raw Material Freight Cost.             *~
*                   RAW_VINYL = Raw Material Vinyl Discount Amount     *~
*                   RAW_ERR%  = Error Code (0% = Ok, 1% = Error)       *~
*                                                                      *~
*----------!----------------------------------------------------!------*~
*   DATE   !                MODIFICATION                        ! WHO  *~
*----------!----------------------------------------------------!------*~
* 03/27/98 ! y2k compliant                                      !  DJD *~
* 06/05/00 ! Mod to add error message to show raw material num  !  CMG *~
*          !     that is not set up properly. (EWD001)          !      *~
*10/20/2009! (AWD001) mod to take out SHOSTAT                   !  CMG *~
*----------!----------------------------------------------------!------*~
*       ****************************************************************

        sub "APCCST9B" (raw$,            /* Raw Mat. Part No.         */ ~
                        raw_qty,         /* Raw Mat. Quantity         */ ~
                        raw_code%,       /* Raw Mat. Unit Code        */ ~
                        raw_cost,        /* Calculated Material Cost  */ ~
                        raw_desc$,       /* Description of Raw Mat.   */ ~
                        raw_unit,        /* Raw Mat'l Unit Cost       */ ~
                        raw_frt,         /* Calculate Freight Cost    */ ~
                        raw_vinyl,       /* Calcualte Discount Amount */ ~
                        #1,              /* HNYQUAN File              */ ~
                        #2,              /* HNYMASTR                  */ ~
                        raw_err% )       /* Error Code 0% = Ok        */

        dim raw$25,                      /* Raw Mat. Part Number      */ ~
            raw_unit$20,                 /* Var Field  (HNYMASTR)     */ ~
            raw_desc$32,                 /* Raw Material Description  */ ~
            readkey$44                   /* Generic Key               */

            raw_err% = 0%
            raw_vinyl, raw_frt = 0.0
            readkey$ = all(hex(00))
            str(readkey$,1%,25%) = raw$   /* 1st get Raw Material Cost */
L00580:     raw_cost, cost = 0.0  : err% = 1%     /* Set Err (HNYQUAN) */
            read #1,key > readkey$, using L00610  , readkey$, cost,         ~
                                                 eod goto L01400
L00610:        FMT POS(17), CH(44), POS(117), PD(14,4)
            if str(readkey$,1%,25%) <> raw$ then goto L00660
            if str(readkey$,26%,3%) = "100" then goto L00660
               goto L00580                           /* Get Next Store Cod*/

L00660:     err% = 2%                             /* Set Err (HNYMASTR)*/
            init(" ") raw_desc$, raw_unit$        /* 2nd Get Unit Info */
            read #2,key = raw$, using L00700  , raw_desc$, raw_unit$,       ~
                                                            eod goto L01400
L00700:        FMT POS(26), CH(32), POS(686), CH(20)
            p% = pos(raw_unit$ = "/")
            raw_size = 1.0 : unit% = 1% : err% = 3%    /* Err No Units */
            if str(raw_unit$,1%,p% - 1%) = " " then goto L01410 
            convert str(raw_unit$,1%,p% - 1%) to raw_size, data goto L01410
                                                  /* (EWD001) */
            err% = 4%                             /* Err No Unit Code */
            if str(raw_unit$,p% + 1%, 1%) = " " then goto L01410
            convert str(raw_unit$,p% + 1%, 1%) to unit%, data goto L01410
                                                  /* (EWD001) */
            raw_unit = round( ( cost / raw_size ), 5)     /* Raw Mat'l */
            raw_conv = raw_qty                            /* Unit Cost */
            if raw_code% = unit% then goto L00840
               on unit% gosub L01090 , L01110 , L01150 , L01190 , L01230 , L01270 ,  ~
                              L01300 , L01340 , L01270

L00840:   if raw_err% <> 0% then goto exit_sub
             raw_cost = round( raw_conv * raw_unit, 5)  /*Material Cost*/
             frt_pcnt, vinyl_pcnt = 0.0
             x1% , x2%, x3% = 0%
             p1% = pos(str(raw_unit$,p%+1%) = "/")
             if p1% = 0% then goto exit_sub             /*No Frt/Vinyl */
             x1% = p% + p1%
             p2% = pos(str(raw_unit$,x1%+1%) = "/")
             if p2% = 0% then goto exit_sub             /*No Frt/Vinyl */
             x2% = x1% + p2%
             p3% = pos(str(raw_unit$,x2%+1%) = "/")
             x3% = x2% + p3%
                                            /* Calculate Freight Cost  */
          l1% = (x2% - x1%) - 1%                        /* Freight Pcnt*/
          if l1% < 1% then goto L01010
          convert str(raw_unit$,x1%+1%,l1%) to frt_pcnt,data goto L01000
L01000:
L01010:   raw_frt   = round(raw_cost * frt_pcnt, 5)
          l2% = (x3% - x2%) - 1%            /* Calculate Vinyl Discount*/
          if l2% < 1% then goto exit_sub               /* No Vinyl Pcnt*/
          convert str(raw_unit$,x2%+1%,l2%) to vinyl_pcnt,data goto L01050
L01050:
          raw_vinyl = round(raw_cost * vinyl_pcnt, 5)
          goto exit_sub

L01090: REM - COST PER PIECE OF RAW MAT.
        return
L01110: REM - COST PER SQ FOOT
             if raw_code% <> 3% then raw_err% = 5%      /* ERROR =  5% */
             raw_conv = round( raw_qty / 144.0, 5) /* Sq Inch-Sq Feet  */
        return
L01150: REM - COST PER SQ INCH
             if raw_code% <> 2% then raw_err% = 6%      /* ERROR =  6% */
             raw_conv = round( raw_qty * 144.0, 5) /* Sq Feet-Sq Inch  */
        return
L01190: REM - COST PER FOOT
             if raw_code% <> 5% then raw_err% = 7%      /* ERROR =  7% */
             raw_conv = round((raw_qty / 12.0), 5) /* Inches - Feet    */
        return
L01230: REM - COST PER INCH
             if raw_code% <> 4% then raw_err% = 8%      /* ERR0R =  8% */
             raw_conv = round( raw_qty * 12.0, 5)  /* Feet - Inches    */
        return
L01270: REM - CODES (6,7,8,9) NOT USED
             raw_err% = 9%                              /* ERROR =  9% */
        return
L01300: REM - COST PER POUND
             if raw_code% <> 8% then raw_err% = 10%     /* ERROR = 10% */
             raw_conv = round((raw_qty / 16.0), 5) /* Ounce - Pound    */
        return
L01340: REM - COST PER OUNCE
             if raw_code% <> 7% then raw_err% = 11%     /* ERROR = 11% */
             raw_conv = round( raw_qty * 16.0, 5)  /* Pound - Ounce    */
        return

        REM - Error Exit
L01400:   raw_err% = err%

          goto exit_sub                                   /* (EWD001) */
        REM - Error Exit
L01410:   raw_err% = err%                                 /* (EWD001) */
/*(AWD001)*/
REM          call "SHOSTAT" ("Data not correct in Item Master for " & raw$)  stop

        exit_sub
        end
