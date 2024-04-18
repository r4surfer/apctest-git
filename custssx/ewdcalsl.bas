        REM *************************************************************~
            *                                                           *~
            *  EEEEE  W   W  DDDD    CCC    AAA   L      SSSSS  L       *~
            *  E      W   W  D   D  C   C  A   A  L      S      L       *~
            *  EEEE   W   W  D   D  C      AAAAA  L      SSSSS  L       *~
            *  E      W W W  D   D  C   C  A   A  L          S  L       *~
            *  EEEEE   W W   DDDD    CCC   A   A  LLLLL  SSSSS  LLLLLL  *~
            *                  ( SUBROUTINE )                           *~
            *-----------------------------------------------------------*~
            * EWDCALSL - Calculate Cut Lineal Dimensions for all Special*~
            *            Shapes. To use for bending and Fabrication.    *~
            *                                                           *~
            *     - SHAPE%   - Special Shapes Code               ( In  )*~
            *                                                           *~
            *     - dt_rec$  - Planning Detail Record            ( In ) *~
            *                                                           *~
            *     - sh()     - Data Entry Values                 ( In ) *~
            *                                                           *~
            *     - sh_entry$- Names of Fields Entered           ( In ) *~
            *                                                           *~
            *     - sh_cross$- 'SHAPCROSS' Code Value            ( In ) *~
            *                                                           *~
            *     - shb()    - Calculated Bend Values (1 - 6)           *~
            *                                                           *~
            *     - sha()    - Calculated Angle Values (1 - 4)          *~
            *                                                           *~
            *     - sh_bend$ - Bend Fields (1 - 6 ) for sh_shb()        *~
            *                  (BHLR12)                                 *~
            *                                                           *~
            *     - sh_angle$- Angle Fields (1 - 4) for sh_sha()        *~
            *                  (ABCD)                                   *~
            *                                                           *~
            *     - #1       - Channel for (GENCODES) File       ( In ) *~
            *                                                           *~
            *     - #2       - Channel for (AWDSPECB) File       ( In ) *~
            *     - ERR%     - Error Code                        ( Out )*~
            *                  (0%) All Ok                              *~
            *                  (1%) Error with Width                    *~
            *                  (2%) Error with Height                   *~
            *                  (3%) Error with Radius                   *~
            *                  (4%) Error with Leg                      *~
            *                  (5%) Error with Profile Table            *~
            *                  (6%) Error No Equation                   *~
            *                  (7%) Error Updating 'AWDSPECB'           *~
            *                                                           *~
            *    Note - Special Mod to 'OCTAGON' for Window wizard      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/23/04 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 04/20/04 ! last update of Equations                 !     *~
            * 06/30/04 ! (AWD001) Correct Angle 'B' on Doghouse   ! RHH *~
            * 07/15/04 ! (AWD002) correct the 'C' angle for the   ! RHH *~
            *          !    Left and right triangels.             !     *~
            * 01/01/06 ! (PAR000) No Change                       ! RHH *~
            * 01/10/08 ! (AWD003) mods for casing/bullnose shapes ! CMG *~
            *          !    nailing fin and casing bends          !     *~
            * 01/22/08 ! (AWD004) mods for formulas               ! CMG *~
            * 03/25/08 ! (AWD005) mods for trim kits              ! CMG *~
            *04/24/2012! (AWD006) new flag for nailing fin on 8900! CMG *~
            *02/27/2013! (AWD007) sub_part & foam flag            ! CMG *~
            *06/08/2016!(SR75267) mod for nailing fin series      ! CMG *~            
            *************************************************************

        sub "EWDCALSL"   (scr_sel%,      /* Screen Selection           */~
                          sh_model$,     /* Model Code                 */~
                          shape%,        /* Shape Code                 */~
                          dt_rec$,       /* APCPLNDT Record            */~
                          sh(),          /* Data Entry Values          */~
                          sh_entry$,     /* Name of Data Field Entered */~
                          sh_cross$,     /* 'SHAPCROSS' Code           */~
                          specialmull$,  /* Casing/Bullnose (AWD097)   */~
                          sub_part$,     /* Subpart (AWD007)           */~
                          series$,       /* Series      (SR75267)      */~
                          #1,            /* GENCODES File              */~
                          #2,            /* AWDSPECB - New Bending Data*/~
                          err% )         /* Error Code 0 = Ok, 0 <> err*/

        dim                                                              ~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30,                    /* Use for GENCODES Look-Up   */~
            savekey$50,                  /* Save GENCODES Key (AWD006) */~
            shape$3,                     /* Shape Code                 */~
            dt_rec$256,                  /* APCPLNDT Detail Record     */~
            sort_key$60,                 /* Planning sort Key          */~
            bd_rec$256,                  /* Special Shape Bend Record  */~
            bd_key$36, bd_key1$23,       /* Primary and Secondary Key  */~
            sh(10%),                     /* Data Entry Values          */~
            shb(6%),                     /* Calulated Bend Values      */~
            sha(4%),                     /* Calculated Angle Values    */~
            sh_model$3,                  /* Model Code                 */~
            sh_bend$6,                   /* Bend Fields                */~
            sh_angle$4,                  /* Angle Fields               */~
            sh_cross$2,                  /* 'SHAPCROSS' Code           */~
            sh_entry$7,                  /* Name of data field entered */~
            sh_special$7                 /* Special Notes              */~

        dim                                                              ~
            sh$(7%)12,                  /* Data Entered Values         */~
            shb$(7%)12,                 /* Bend Calculated Values      */~
            temp(7%), temp$(7%)12,      /* Debug Values                */~
            header$62,                  /* Debug Header                */~
            p$12, q$12,                 /* Profile Adj and Glazing Beed*/~
            date$8,                     /* todays Date                 */~
            cursor%(2%),                /* Cursor Location for Edit    */~
            i$(24%)80,                  /* Screen Image                */~
            inpmessage$79,              /* Informational Message       */~
            sz$100,                     /* Fractions for 16th of inch  */~
            pf$(3%)79,                  /* PF Screen Literals          */~
            pfkeys$32                   /* PF Key Hex Values           */

         dim specialmull$1               /* Casing/bullnose  (AWD003)    */

         dim part_num$25,                /* Part Number       (AWD006) */~
             locks$1                     /* Locks Code        (AWD006) */

/* (SR75267) */
         dim table$9,                    /* Table To Read              */~ 
             genkey$15,                  /* GENCODES Key to Read       */~
             descr1$30,                  /* Description                */~
             codeLen$2,                  /* Code Length                */~
             descr2$30,                  /* Description                */~
             descr3$30,                  /* Description                */~
             series$16                   /* Series Description         */
             

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 03/24/04 Calculate Special Shapes Bends"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master System Table File                 *~
            * #2  ! AWDSPECB ! Special Shapes Bending Info Database     *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            debug% = 0%
/*(AWD005)*/
            trim%  = 0%
            if sh_model$ = "B70" or sh_model$ = "B71" then trim% = 1%
/* (AWD006) */
            part_num$ = str(dt_rec$,189%,25%)
            locks$ = str(part_num$,12%,1%)
            mdl8900% = 0%
/* + (SR75267) */
            init(" ") table$, genkey$, codeLen$, descr1$, descr2$, descr3$
            generr% = 0%
            table$ = "APPLNAIL"
            genkey$ = series$
            call "GENREAD" (table$, genkey$, descr1$, codeLen$,       ~
                            descr2$, descr3$, generr%)
            if generr% = 0% then mdl8900% = 1%

/* - (SR75267) */
            
REM            GOSUB LOAD_8900_MDLS
REM            GOSUB CHECK_8900_MDL
            rhh$ = "  "
            convert shape% to rhh$, pic(###)

            gosub check_shape_on
            if shape_on% = 0% then goto exit_program
                                                     /* Temporary      */
            if scr_sel% <> 2% then goto skip_1

/* (AWD005) */
            if trim% = 1% and shape% = 51% then specialmull$ = "R"

            call "AWDPLN9B"   (dt_rec$,      /* APCPLNDT Sort Record   */~
                               sort_key$,    /* Reporting Sort Key     */~
                               specialmull$, /* (AWD003)               */~
                               #1)           /* GENCODES               */

                                             /* Replace Sorted Data    */
            str(dt_rec$,66%,30%) = str(sort_key$,1%,30%)

/*(AWD005) */
            if trim% = 1% and shape% = 51% then specialmull$ = " "

skip_1:
                                                     /* Get Model Code */
            if scr_sel% = 2% then sh_model$ = str(dt_rec$,189%,3%)


                                                     /* Include Special*/
                                                     /* Planning Sort  */

            debug% = 0%                              /* (RHHTEST)      */
            u3% = 0%

        REM - NEAREST 16TH OF AN INCH
        REM   sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        REM~ 3/4 13/16 7/8 15/16     "

           sz$ = ".0625.1250.1875.2500.3125.3750.4375.5000.5625.6250.6875~
        ~.7500.8125.8750.9375     "


            err% = 0%                                /* SET ERROR FLAG */
            init(" ") sh_bend$, sh_angle$, sh_special$

            width, height, radius, leg1, leg2, leg3, leg4, leg5 = 0.0
            mat shb = zer
            mat sha = zer

           for kk%=1% to 6%
               if sh(kk%) < .1 then goto entry_next
                  gosub load_entry_data


entry_next: next kk%

            p, q, factor1, factor2  = 0.00
            factor3, factor4, factor5, factor6 = 0.00
            roy, roy1, roy2, roy3, roy4, roy5, roy6 = 0.0

            deg_radians = 57.29578
            rad_deg     = .0174532
            pie         = 3.14159265

        REM *************************************************************~
            *            B E G I N   C A L C U L A T I O N S            *~
            *                                                           *~
            *************************************************************

        if shape% =  0% then gosub calc_picture                /* Done */
        if shape% = 01% then gosub calc_right_irreg_pentagon
        if shape% = 02% then gosub calc_left_irreg_pentagon
        if shape% = 03% then gosub calc_right_trapezoid        /* Done */
        if shape% = 04% then gosub calc_left_trapezoid         /* Done */
        if shape% = 05% then gosub calc_right_triangle         /* Done */
        if shape% = 06% then gosub calc_left_triangle          /* Done */
        if shape% = 07% then gosub calc_isoc_triangle          /* Done */
        if shape% = 15% then gosub calc_doghouse_pentagon
        if shape% = 25% then gosub calc_octagon                /* Done */
        if shape% = 51% then gosub calc_circle                 /* Done */
        if shape% = 60% then gosub calc_eyebrow                /*?Done */
        if shape% = 63% then gosub calc_colonial_arch          /* Done */
        if shape% = 64% then gosub calc_half_round             /* Done */
        if shape% = 66% then gosub calc_right_qtr_round        /* Done */
        if shape% = 67% then gosub calc_left_qtr_round         /* Done */
        if shape% = 70% then gosub calc_half_right_eyebrow     /*?Done */
        if shape% = 71% then gosub calc_half_left_eyebrow      /*?Done */
        if shape% = 73% then gosub calc_half_rt_colonial_arch  /* Done */
        if shape% = 74% then gosub calc_half_lf_colonial_arch  /* Done */
                                          /* No Equation Defined       */
        err% = 6%
        return clear all
        goto exit_program


        calc_right_irreg_pentagon         /* Shape Code (01)           */
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p + q)
                                          /* Short Leg - leg2          */
                                          /* Top Leg   - leg3          */

                                          /* Chop Diagonal             */
            factor1 = SQR( ( ( height - leg2)**2) + ( (width - leg3)**2) )

                                          /* Segment Climb             */
            factor2 = (height - leg2)

                                          /* Pitch                     */
            factor3 = ( (height - leg2) / (width - leg3) )*12

                                          /* Calculated width          */
            factor4 = width

                                          /* Calculated Height         */
            factor5 = height

                                          /* Short Leg Angle          */
            roy = (ARCTAN((height - leg2)/(width - leg3))*deg_radians) + 90.0

                                          /* Peak Angle               */
            roy1 = (90.0 - (roy - 90.0)) + 90.0

                                          /* Short Leg Bisect Angle   */
            roy2 = roy/2

                                          /* Calculated Top Leg       */
            roy3 = leg3 - ((TAN((90.0 - (roy1/2))*rad_deg)*(1.0)) + (0.0))

                                          /* Calculated Climb         */
            roy4 = (factor4 - roy3) * TAN((Roy - 90.0) * rad_deg)

                                          /* Calculated Glass short leg */
            roy5 = (factor5 - roy4)


            shb(1%) = width               /* Base Value Width          */
            shb(2%) = height              /* Head Value Arch Radius    */
            shb(3%) = roy5                /* Left Leg Value            */
            shb(4%) = roy3                /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "BHLR12"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = roy                 /* Calc for Angle 'B'        */
            sha(3%) = roy1                /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"


        return clear all
        goto exit_program

        calc_left_irreg_pentagon          /* Shape Code (02)           */
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p + q)
                                          /* Short Leg - leg1          */
                                          /* Top Leg   - leg3          */

                                          /* Chop Diagonal             */
            factor1 = SQR( ( ( height - leg1)**2) + ( (width - leg3)**2) )

                                          /* Segment Climb             */
            factor2 = (height - leg1)

                                          /* Pitch                     */
            factor3 = ( (height - leg1) / (width - leg3) )*12

                                          /* Calculated width          */
            factor4 = width

                                          /* Calculated Height         */
            factor5 = height

                                          /* Short Leg Angle          */
            roy = (ARCTAN((height - leg1)/(width - leg3))*deg_radians) + 90.0

                                          /* Peak Angle               */
            roy1 = (90.0 - (roy - 90.0)) + 90.0

                                          /* Short Leg Bisect Angle   */
            roy2 = roy/2

                                          /* Calculated Top Leg       */
            roy3 = leg3 - ((TAN((90.0 - (roy1/2))*rad_deg)*(1.0)) + (0.0))

                                          /* Calculated Climb         */
            roy4 = (factor4 - roy3) * TAN((Roy - 90.0) * rad_deg)

                                          /* Calculated short leg     */
            roy5 = (factor5 - roy4)


            shb(1%) = width               /* Base Value Width          */
            shb(2%) = height              /* Head Value Arch Radius    */
            shb(3%) = roy5                /* Left Leg Value            */
            shb(4%) = roy3                /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "BHLR12"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = roy                 /* Calc for Angle 'B'        */
            sha(3%) = roy1                /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"


        return clear all
        goto exit_program


        calc_doghouse_pentagon            /* Shape Code (15)           */
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

                                          /* Pentoid Diagonal         */
            factor1 = SQR( ( (height - leg2)**2) + ( (width/2.0)**2) )

                                          /* Climb                    */
            factor2 = height - leg2
                                          /* Short Leg Angle           */
            factor3 = (ARCTAN((height - leg2)/( (width/2.0) ) )*deg_radians) + 90.0

                                          /* Peak Angle                */
            factor4 = (90.0 - (factor3 - 90.0) ) *2
                                          /* Calculated Width          */
            width   = width

                                          /* Calculated Height         */
            height  = height - SQR( ( (TAN( (90 - (factor4/2.0) )*rad_deg)*1.0)**2) ~
                            + (0.0) ) - 0.0

                                          /* Calculated Climb          */
            factor5 = (+width/2.0) * TAN( ( factor3 - 90.0) * rad_deg)

                                          /* Calculated short Leg      */
            leg2 = height - factor5
                                          /* Calculated Glass Diagonal */
        REM leg3 = SQR( ( ( width/2.0)**2) + (factor5**2) )
                                          /* (AWD001)                  */
            factor3 = factor3/2           /* Angle 'B'                 */

            shb(1%) = width               /* Base Value Width          */
            shb(2%) = height              /* Head Value Arch Radius    */
            shb(3%) = leg2                /* Left Leg Value            */
            shb(4%) = leg2                /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "BHLR12"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor3             /* Calc for Angle 'B'        */
            sha(3%) = factor4             /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"


        return clear all
        goto exit_program

        calc_half_round                   /* Shape Code (64)           */
                                          /* Check for Elliptical      */
            factor1_nail, factor1_case = 0.00       /* (AWD003) */
            radius_nail, radius_case = 0.00         /* (AWD003) */
            width_nail               = 0.00         /* (AWD003) */
            if str(sh_cross$,1%,1%) = "C" then goto calc_elliptical

            radius = (height)             /* Calculate Radius          */
            height = (radius)             /* Calulate Height           */
                                          /* Calulate the Width        */

            factor1 = pie * (width / 2.0) + 11.0
/*(AWD003) beg */
            if specialmull$ <> "C" then goto no_nail_casing
              factor1_nail = pie * ((width + 4.26) / 2.0) + 11.0
              width_nail   = width + 2.125
REM              radius_nail  = radius + 2.125
              radius_nail  = width_nail / 2.0

              factor1_case = pie * ((width + 1.808) / 2.0) + 11.0
REM              radius_case  = radius + 0.875
              radius_case  = width / 2.0

no_nail_casing:
/*(AWD003) end*/

            if sh_model$ = "726" then factor1 = pie * (width / 2.0) + 14.0
/* (AWD005) - begin */
            if trim% <> 1% then goto not_b70_hr
               radius  = width - 1.25
               factor1 = factor1 + 4.00
               width   = width + 3.25
               shb(6%) = width
not_b70_hr:
/* (AWD005) - end */
                                          /* leg1 contains the Radius  */
            leg1    = radius

            shb(1%) = factor1             /* Base Cut Length           */
            shb(2%) = leg1                /* Head Arch Radius          */
            shb(5%) = height              /* Height overall Height     */

            sh_bend$    = "BH??1?"
                                          /* Calculate Angles          */
            angle = 0.0
            if width > 51.0 then angle = 44.0
            if width >= 50.0 and width <= 50.99 then angle = 43.5
            if width >= 35.0 and width <= 49.99 then angle = 43.0
            if width >= 31.0 and width <= 34.99 then angle = 42.5
            if width <= 30.99 then angle = 42.0

            sha(1%) = angle
            sha(2%) = angle

            sh_angle$ = "AB??"


            if width  <= 0.0 then err% = 1%
            if height <= 0.0 then err% = 2%
            if leg1   <= 0.0 then err% = 3%

            gosub adjust_cut_length

        return clear all
        goto exit_program


        calc_elliptical                   /* Shape Code (64)           */
                                          /* Calculated Radius         */
            radius = ( (width/2)**2 + (width/3)**2) /(2*(width/3))
                                          /* Calculated height         */
            height = (width/3)

            factor1 = (2 * height)
            factor_exp = (height * height)
            factor2 = ((radius * factor1) - factor_exp)

            width = SQR(factor2)
            width = (width * 2)


                                          /* Calulate the Width        */
                                          /* for Elliptical            */
            factor3 = pie * (width / 2.5) + 6.0

                                          /* leg1 contains the Radius  */
            leg1    = radius

/* (AWD005) - begin */

            if trim% <> 1%  then goto not_b70_el
               radius  = width - 1.25
               factor3 = factor3 + 4.00
               width   = width + 3.50
               shb(6%) = width
not_b70_el:
/* (AWD005) - end */

            shb(1%) = factor3             /* Base Cut Length           */
            shb(2%) = leg1                /* Head Arch Radius          */
            shb(5%) = height              /* Height overall Height     */

            sh_bend$    = "BH??1?"
                                          /* Calculate Angles          */
            angle = 0.0

            if width > 51.0 then angle = 32.0
            if width >= 50.0 and width <= 50.99 then angle = 31.5
            if width >= 35.0 and width <= 49.99 then angle = 31.0
            if width >= 31.0 and width <= 34.99 then angle = 30.5
            if width <= 30.99 then angle = 30.0

            sha(1%) = angle
            sha(2%) = angle

            sh_angle$ = "AB??"


            if width  <= 0.0 then err% = 1%
            if height <= 0.0 then err% = 2%
            if leg1   <= 0.0 then err% = 3%

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_colonial_arch                /* Shape Code (63)          */
            factor1_nail, factor1_case = 0.00       /* (AWD003) */
            radius_nail, radius_case = 0.00         /* (AWD003) */
            width_nail               = 0.00         /* (AWD003) */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* Calculated Glass Width    */
            factor1 = (width  - 2*(p + q))


            width  = (width )             /* Calculate width           */
            height = (height )            /* Calulate Height           */
                                          /* Arch Radius               */
            radius = (width / 2)

/*(AWD003) beg */
            if specialmull$ <> "C" then goto no_nail_casing1
              factor1_nail = pie * ((width + 4.26) / 2.0) + 11.0
              width_nail   = width + 2.125
REM              radius_nail  = radius + 2.125
              radius_nail  = width_nail / 2.0

              factor1_case = pie * ((width + 1.808) / 2.0) + 11.0
REM              radius_case  = radius + 0.875
              radius_case  = width / 2.0

/*(AWD003) end */
no_nail_casing1:
                                          /* Calulate the leg1         */
            leg1  =  (height - radius )
                                          /* Leg overlap triangle      */
            factor2 = ((width - factor1)/2)
                                          /* Bisect Angle              */
            factor3 = 90.0 - (ARCTAN( factor2/(p + q))*deg_radians)
                                          /* Calc Cut length           */
            factor4 = pie * (width / 2.0) + 11.0

            if sh_model$ = "726" then factor4 = pie * (width / 2.0) + 14.0

            if sh_model$ = "727" then factor4 = pie * (width / 2.0) + 13.0

/* (AWD005) - begin */

            if trim% <> 1% then goto not_b70_ca
               radius  = width - 1.25
               factor4 = factor4 + 4.00
               width   = width + 3.25
               shb(6%) = width
not_b70_ca:
/* (AWD005) - end */

            shb(1%) = factor4             /* Base Cut length           */
            shb(2%) = radius              /* Head Value Arch Radius    */
            shb(3%) = leg1                /* Left Leg Value            */
            shb(4%) = leg1                /* Right Leg Value           */
            shb(5%) = height              /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BHLR1?"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor3             /* Calc for Angle 'B'        */
            sha(3%) = factor3             /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

                                          /* Calculate Angles          */
            sh_angle$  = "ABCD"

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_eyebrow                      /* Shape Code (60)           */
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p + q)

            s1 = ABS((height - leg5))     /* Segment Climb             */

            roy = leg5                    /* Save Segment Leg          */

                                          /* Segment Radius            */
            factor1 = ((4*(s1**2)) + (width**2)) / (8*s1)

            radius = factor1

                                          /* Window Width             */
            factor2 = width
                                          /* Calc cut length          */
                                          /* Special Logic            */
            factor  = pie * (width / 3.0) + 7.0

            if width = 71.50 then factor = factor + 6.0
            if width = 71.25 and height = 15.5 then factor = factor + 4.0
            if width = 71.25 and height = 23.5 then factor = factor + 6.0


                                          /* Special Logic            */


                                          /* Glass width              */
            roy1    = width - (pq + pq)
                                          /* Window Height            */

            factor3 = height
                                          /* Glass Height             */
            roy2    = (height - (pq + pq))
                                          /* Window ARC Radius        */
            factor4 = radius
                                          /* Glass Arc Radius         */
            roy3    = (radius - pq)
                                          /* Window ARC Rise          */
        REM factor5 = factor4 - (0.5*(SQR((4*(factor4**2)) - (factor2**2))))

                                          /* Glass Arc Rise           */
            roy4    = roy3 - (0.5*(SQR((4*(roy3**2)) - (roy1**2))))

                                          /* Window short Leg          */
            factor6 = height - (factor5)
                                          /* Glass short Leg           */
            roy5    = height - (pq + pq + roy4)
                                          /* Rise of Leg less Glass leg*/
            roy6    = (roy - (roy5 + pq))
                                          /* Miter Angle               */
            factor7 = 90 - (ARCTAN(roy6/pq)*deg_radians)

/* (AWD005) - begin */

            if trim% <> 1% then goto not_b70_eb
REM               factor4  = (4 * ((height-leg5)**2) + (width**2) / (8*(height-leg5)))-0.625
               factor4  = factor4 - 0.625
               factor   = factor + 4.00
               shb(6%)  = width
not_b70_eb:
/* (AWD005) - end */

            shb(1%) = factor              /* Base cut length           */
            shb(2%) = factor4             /* Head Value Arch Radius    */
            shb(3%) = leg5                /* Left Leg Value            */
            shb(4%) = leg5                /* Right Leg Value        ?? */
            shb(5%) = factor3             /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BHLR1?"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor7             /* Calc for Angle 'B'        */
            sha(3%) = factor7             /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

                                         /* Calculate Angles          */

            sh_angle$  = "ABCD"

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_half_right_eyebrow           /* Shape Code (70)           */
            debug% = 0%

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* Calculated Glass Width    */
            pq = (p + q)

            s1 = ABS((height - leg2))     /* Segment Climb             */

            roy = leg2                    /* Save Segment Leg          */

                                          /* Segment Radius            */
            factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                          /* Window Width              */
            factor2 = (width )

            gosub debug_display_screen    /* (RHHTEST)                 */

                                          /* Calc Cut length           */
                                          /* Special Logic             */
            factor = pie * (width / 3.0) + 7.0

            if width = 23.25 then factor = factor + 4.0

            if width = 27.50 then factor = factor + 1.0

                                          /* Special Logic             */

                                          /* window Height             */
            height  = (height )
                                          /* Glass Height              */
            roy1    = ( height - (pq + pq))

                                          /* Window Arc Radius         */
            factor3 = (factor1)

                                          /* Glass Arc Radius          */
            roy2    = (factor1 - pq)


                                          /* Window ARC Rise           */

            factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - 0.0)**2))))

                                          /* Glass Arc Rise            */
            roy3    = roy2 - (0.5*(SQR((4*(roy2**2)) - (((width*2) - (2*pq))**2))))


                                          /* Window short Leg          */
            leg2    = height - factor4
                                          /* Glass Short leg           */
            roy4    = roy1 - roy3
                                          /* Glass Rise ?? 2*pq        */
            roy5 = roy - (roy4 + pq)

                                          /* Miter Angle Calc          */
            factor5 = 90 - (ARCTAN(roy5/pq)*deg_radians)


                                          /* Special Correction        */
            factor6 = 45.0


/* (AWD005) - begin */

            if trim% <> 1% then goto not_b70_hreb
REM               factor3  = (4 * ((height-leg5)**2) + (width**2) / (8*(height-leg5)))-0.625
               factor3  = factor3 - .0625
               factor   = factor + 4.00
               shb(6%)  = width
not_b70_hreb:
/* (AWD005) - end */

            shb(1%) = factor              /* Base Cut Length           */
            shb(2%) = factor3             /* Head Value Arch Radius    */
            shb(3%) = height              /* Left Leg Value            */
            shb(4%) = leg2                /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BHLR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor6             /* Calc for Angle 'B'        */
            sha(3%) = factor5             /* Calc for Angle 'C'        */
            sha(4%) = 45.00               /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"

            gosub adjust_cut_length

        return clear all
        goto exit_program

    calc_half_left_eyebrow            /* Shape Code (71)           */
            debug% = 0%

        gosub read_glass_profile      /* Get Profile and Glazing   */
                                      /* Bead Deduction            */
                                      /* Calculated Glass Width    */
        pq = (p + q)

        s1 = ABS((height - leg1))     /* Segment Climb             */

        roy = leg1                    /* Save Leg                  */

                                      /* Segment Radius            */
        factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                      /* Window Width              */
        factor2 = (width )

            gosub debug_display_screen    /* (RHHTEST)                 */


                                          /* Calc Cut length           */
                                          /* Special Logic             */
            factor = pie * (width / 3.0) + 7.0

            if width = 23.25 then factor = factor + 4.0

            if width = 27.50 then factor = factor + 1.0

                                          /* Special Logic             */

                                      /* window Height             */
        height  = (height )
                                      /* Glass Height              */
        roy1    = (height - (pq + pq))
                                      /* Window Arc Radius         */
        factor3 = (factor1 )
                                      /* Glass Arc Radius          */
        roy2    = (factor1 - pq)
                                      /* Window ARC Rise           */

        factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - 0.0)**2))))

                                          /* Glass Arc Rise            */
            roy3    = roy2 - (0.5*(SQR((4*(roy2**2)) - (((width*2) - (2*pq))**2))))

                                          /* Short Leg                 */
            leg1    = height - factor4
                                          /* Glass short Leg           */
            roy4    = roy1 - roy3
                                          /* Glass Rise ?? 2*pq        */
            roy5    = roy - (roy4 + pq)
                                          /* Miter Angle Calc          */
            factor5 = 90 - (ARCTAN(roy5/pq)*deg_radians)

                                          /* Special Correction        */
            factor6 = 45.0

/* (AWD005) - begin */

            if trim% <> 1% then goto not_b70_hleb
REM               factor3  = (4 * ((height-leg5)**2) + (width**2) / (8*(height-leg5)))-0.625
               factor3  = factor3 - .0625
               factor   = factor + 4.00
               shb(6%)  = width
not_b70_hleb:
/* (AWD005) - end */


            shb(1%) = factor              /* Base Cut Length           */
            shb(2%) = factor3             /* Head Value Arch Radius    */
            shb(3%) = leg1                /* Left Leg Value            */
            shb(4%) = height              /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BHLR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor5             /* Calc for Angle 'B'        */
            sha(3%) = factor6             /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_octagon                      /* Shape Code (25)          */

            width  = (width )             /* Calculate width          */
            height = (height )            /* Calulate Height          */
                                          /* For Window Wizard        */

            leg1   = round (  height * .413, 4)
            leg2   = round (  height * .413, 4)

/* (AWD004) -beg octagon*/
REM            leg1   = leg1 + .1875
REM            leg2   = leg2 + .1875
/* (AWD004) - end*/

            shb(1%) = width               /* Base Value Width          */
            shb(2%) = width               /* Head Value Arch Radius    */
            shb(3%) = leg1                /* Left Leg Value            */
            shb(4%) = leg2                /* Right Leg Value           */
            shb(5%) = height              /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "BHLR1?"

            sha(1%) = 22.5                /* Calc for Angle 'A'        */
            sha(2%) = 22.5                /* Calc for Angle 'B'        */
            sha(3%) = 22.5                /* Calc for Angle 'C'        */
            sha(4%) = 22.5                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"

        return clear all
        goto exit_program

        calc_right_qtr_round              /* Right Quarter Round   (66)*/

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* Calculated Glass Width    */
            pq = (p + q)
                                          /* Calc Cut length           */

            factor1 = pie * (width / 2.0) + 11.0

            if sh_model$ = "726" then factor1 = pie * (width / 2.0) + 8.0

            factor2 = height              /* Qtr Radius entered        */

            radius  = factor2             /* Window Arch Radius        */

            roy     = radius - pq         /* Glass Arc Radius          */

                                          /* Caluclated Glass width    */
        REM roy1 = ( (2*(SQR((2*roy*(roy - pq) ) - ( (roy - pq)**2))))/2) - pq

                                          /* Calculated Glass Height   */
            roy2 = ( (2*(SQR((2*roy*(roy - pq) ) - ( (roy - pq)**2))))/2) - pq

                                          /* Calculate Leg Overlap     */
            roy3 = (( factor2 - (roy2 + pq) ) )

                                          /* Bisect Angle              */
            roy4 =  90 - (ARCTAN(roy3/pq)*deg_radians)

/* (AWD005) - begin */
            if trim% <> 1% then goto not_b70_rqr
               radius  = width - 1.25
               factor1 = factor1 + 4.00
               width   = width + 3.25
               shb(6%) = width
not_b70_rqr:
/* (AWD005) - end */

            shb(1%) = factor1             /* Base cut length           */
            shb(2%) = radius              /* Head Value Arch Radius    */
            shb(3%) = factor2             /* Left Leg Value            */
            shb(4%) = 0.0                 /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BHL???"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = roy4                /* Calc for Angle 'B'        */
            sha(3%) = roy4                /* Calc for Angle 'C'        */
            sha(4%) = 0.0                 /* Calc for Angle 'D'        */

            sh_angle$  = "ABC?"

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_left_qtr_round               /* Left Quarter Round    (67)*/

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* Calculated Glass Width    */
            pq = (p + q)
                                          /* Calc cut length           */
            factor1 = pie * (width / 2.0) + 11.0

            if sh_model$ = "726" then factor1 = pie * (width / 2.0) + 8.0

            factor2 = height              /* Qtr Radius entered        */

            radius  = factor2             /* Window Arch Radius        */

            roy     = radius - pq         /* Glass Arc Radius          */

                                          /* Caluclated Glass width    */
        REM roy1 = ( (2*(SQR((2*roy*(roy - pq) ) - ( (roy - pq)**2))))/2) - pq

                                          /* Calculated Glass Height   */
            roy2 = ( (2*(SQR((2*roy*(roy - pq) ) - ( (roy - pq)**2))))/2) - pq

                                          /* Calculate Leg Overlap     */
            roy3 = (( factor2 - (roy2 + pq) ) )

                                          /* Bisect Angle              */
            roy4 =  90 - (ARCTAN(roy3/pq)*deg_radians)

/* (AWD005) - begin */
            if trim% <> 1% then goto not_b70_lqr
               radius  = width - 1.25
               factor1 = factor1 + 4.00
               width   = width + 3.25
               shb(6%) = width
not_b70_lqr:
/* (AWD005) - end */

            shb(1%) = factor1             /* Base cut length           */
            shb(2%) = radius              /* Head Value Arch Radius    */
            shb(3%) = 0.0                 /* Left Leg Value            */
            shb(4%) = factor2             /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BH?R??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = roy4                /* Calc for Angle 'B'        */
            sha(3%) = roy4                /* Calc for Angle 'C'        */
            sha(4%) = 0.0                 /* Calc for Angle 'D'        */

            sh_angle$  = "ABC?"

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_picture                      /* Shape Code (00)           */

            width  = (width )             /* Calculate width           */
            height = (height )            /* Calulate Height           */
/* (AWD004) - begin picture */
            shb(1%) = width               /* Base Value Width          */
            shb(2%) = width               /* Head Value Arch Radius    */
            shb(3%) = height              /* Left Leg Value            */
            shb(4%) = height              /* Right Leg Value           */

REM            shb(1%) = width + 0.25         /* Base Value Width          */
REM            shb(2%) = width + 0.25         /* Head Value Arch Radius    */
REM            shb(3%) = height + 0.25        /* Left Leg Value            */
REM            shb(4%) = height + 0.25        /* Right Leg Value           */
/* (AWD004) - end */

            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "BHLR??"

            sha(1%) = 90.0                /* Calc for Angle 'A'        */
            sha(2%) = 90.0                /* Calc for Angle 'B'        */
            sha(3%) = 90.0                /* Calc for Angle 'C'        */
            sha(4%) = 90.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"

        return clear all
        goto exit_program

        calc_circle                       /* Shape Code (51)           */

            width  = (width)              /* Calculate cut length      */
            factor1 = pie * (width/2.0) + 11.0

            if sh_model$ = "726" then factor1 = pie * (width / 2.0) + 8.0

            radius = (width/2)

/* (AWD005) - begin */
            if trim% <> 1%  then goto not_b70_cr
               radius  = width - 1.25
               factor1 = factor1 + 4.00
               width   = width + 3.25
               shb(6%) = width
not_b70_cr:
/* (AWD005) - end */

            shb(1%) = factor1             /* Base cut length           */
            shb(2%) = radius              /* Head Value Arch Radius    */
            shb(3%) = 0.0                 /* Left Leg Value            */
            shb(4%) = 0.0                 /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BH????"

            sha(1%) = 0.0                 /* Calc for Angle 'A'        */
            sha(2%) = 0.0                 /* Calc for Angle 'B'        */
            sha(3%) = 0.0                 /* Calc for Angle 'C'        */
            sha(4%) = 0.0                 /* Calc for Angle 'D'        */

            sh_angle$  = "????"

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_half_rt_colonial_arch        /* 1/2 Right Colonial Arch(73)*/
                                          /* Same as 1/2 Right Eyebrow  */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* Calculated Glass Width    */
            pq = (p + q)

            s1 = ABS((height - leg2))     /* Segment Climb             */

            roy = leg2                    /* Save leg                  */

                                          /* Segment Radius            */
            factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                          /* Window cut length         */
            factor2 = pie * (width / 2.0 ) + 11.0

            if sh_model$ = "726" then factor2 = pie * (width / 2.0) + 8.0

                                          /* Window                    */
            height  = (height )
                                          /* Glass Height              */
            roy1    = (height -(pq + pq) )

                                          /* Arc Radius                */
            factor3 = (factor1 )
                                          /* Glass Arc Radius          */
            roy2    = (factor1 - pq)


                                          /* Window ARC Rise           */

            factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - 0.0 )**2))))

                                          /* Glass ARC Rise            */

            roy3    = roy2 - (0.5*(SQR((4*(roy2**2)) - (((width*2) - (2*pq))**2))))


                                          /* Window short Leg          */
            leg2    = height - factor4
                                          /* Glass Short leg           */
            roy4    = roy1 - roy3
                                          /* Rise of leg less glass    */
            roy5    = roy - (roy4 + pq)

                                          /* Miter Angle Calc          */
            factor5 = 90 - (ARCTAN(roy5/pq)*deg_radians)

                                          /* (AWD003)                  */
            factor3 = width

            leg2 = ABS(height - width)

                                          /* (AWD003)                  */

/* (AWD005) - begin */

            if trim% <> 1% then goto not_b70_hrca
               factor3  = width - 1.25
               factor2 = factor2 + 4.00
               width   = width + 3.25
               shb(6%) = width
not_b70_hrca:
/* (AWD005) - end */


            shb(1%) = factor2             /* Base cut length           */
            shb(2%) = factor3             /* Head Value Arch Radius    */
            shb(3%) = height              /* Left Leg Value            */
            shb(4%) = leg2                /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BHLR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor5             /* Calc for Angle 'B'        */
            sha(3%) = factor5             /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"

            gosub adjust_cut_length

        return clear all
        goto exit_program

        calc_half_lf_colonial_arch        /* 1/2 Left Colonial Arch (74)*/
                                          /* Same as 1/2 Left Eyebrow   */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* Calculated Glass Width    */
            pq = (p + q)

            s1 = ABS((height - leg1))     /* Segment Climb             */

            roy = leg1                    /* Save leg                  */

                                          /* Segment Radius            */
            factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                          /* Window cut length         */
            factor2 = pie * (width / 2.0) + 11.0

            if sh_model$ = "726" then factor2 = pie * (width / 2.0) + 8.0

                                          /* Window                    */
            height  = (height )
                                          /* Glass Height              */
            roy1    = (height -(pq + pq) )

                                          /* Arc Radius                */
            factor3 = (factor1 )
                                          /* Glass Arc Radius          */
            roy2    = (factor1 - pq)


                                          /* Window ARC Rise           */

            factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - 0.0 )**2))))

                                          /* Glass ARC Rise            */

            roy3    = roy2 - (0.5*(SQR((4*(roy2**2)) - (((width*2) - (2*pq))**2))))


                                          /* Window short Leg          */
            leg1    = height - factor4
                                          /* Glass Short leg           */
            roy4    = roy1 - roy3
                                          /* Rise of leg less glass    */
            roy5    = roy - (roy4 + pq)

                                          /* Miter Angle Calc          */
            factor5 = 90 - (ARCTAN(roy5/pq)*deg_radians)

                                          /* (AWD003)                  */
            factor3 = width

            leg1 = ABS(height - width)

                                          /* (AWD003)                  */

/* (AWD005) - begin */

            if trim% <> 1% then goto not_b70_hlca
               factor3  = width - 1.25
               factor2 = factor2 + 4.00
               width   = width + 3.25
               shb(6%) = width
not_b70_hlca:
/* (AWD005) - end */

            shb(1%) = factor2             /* Base cut length           */
            shb(2%) = factor3             /* Head Value Arch Radius    */
            shb(3%) = leg1                /* Left Leg Value            */
            shb(4%) = height              /* Right Leg Value           */
            shb(5%) = 0.0                 /* Overall Height Value      */
            if trim% <> 1% then shb(6%) = 0.0   /* Overall width Value */

            sh_bend$    = "BHLR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor5             /* Calc for Angle 'B'        */
            sha(3%) = factor5             /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"

            gosub adjust_cut_length

        return clear all
        goto exit_program


        calc_right_trapezoid              /* Right Trapezoid       (03)*/
                                          /* Chg to leg2               */

            deg_radians = 57.29578

                                          /* Calculated Climb          */
            factor1 =ABS(height - leg2)

            roy = leg2                    /* Save Leg                  */

                                          /* Trapezoid diagonal        */
            factor2 = SQR(((factor1)**2) + (width**2))


                                          /* Short Leg Angle           */
            factor3 = (ARCTAN((factor1)/(width))*deg_radians)+90.0

                                          /* Peak Angle                */
            factor4 = (90.0 - (factor3 - 90.0) )
                                          /* Peak angle Bisect    (B)  */
            factor4a = (factor4/2.0)
                                          /* Short Leg Angle Bisect (C)*/
            factor3a = (factor3/2.0)

                                          /* Calculated Width         */
            width   = width

                                          /* Calculated Height        */
        REM    height  = height - ((SQR((((TAN((factor3 - 90.0)*rad_deg))*pq)**2) ~
        REM                    +(pq**2)))+((TAN((factor3 - 90.0)*rad_deg))*pq)+pq)

                                          /* Calaulated Glass Climb    */
            factor5 = (+width)*TAN((factor3 - 90.0)*rad_deg)

                                          /* Calculated short Leg      */
            leg2 = height - factor5

/* (AWD004) - beg right trap */
            shb(1%) = width               /* Base Value Width          */
            shb(2%) = factor2             /* Head Value Diagonal  s    */
            shb(3%) = height              /* Left Leg Value            */
            shb(4%) = leg2                /* Right Leg Value           */


REM            shb(1%) = width + 0.25        /* Base Value Width          */
REM            shb(2%) = factor2 + 0.25      /* Head Value Diagonal  s    */
REM            shb(3%) = height + 0.25       /* Left Leg Value            */
REM            shb(4%) = leg2 + 0.25         /* Right Leg Value           */
/* (AWD004) - end*/
            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "BHLR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor4a            /* Calc for Angle 'B'        */
            sha(3%) = factor3a            /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"


        return clear all
        goto exit_program

        calc_left_trapezoid               /* Left Trapezoid        (04)*/
                                          /* Chg to leg1               */
                                          /* Calculated Climb          */
            factor1 = height - leg1

                                          /* Trapezoid diagonal        */
            factor2 = SQR(((factor1)**2) + (width**2))

                                          /* Short Leg Angle           */
            factor3 = (ARCTAN((factor1)/(width))*deg_radians)+90.0

                                          /* Peak Angle                */
            factor4 = (90.0 - (factor3 - 90.0) )

                                          /* Peak angle Bisect    (B)  */
            factor4a = (factor4/2.0)
                                          /* Short Leg Angle Bisect (C)*/
            factor3a = (factor3/2.0)


                                          /* Calculated Width          */
            width   = width

                                          /* Calculated Height         */
        REM    height  = height - ((SQR((((TAN((factor3 - 90.0)*rad_deg))*pq)**2) ~
        REM                    +(pq**2)))+((TAN((factor4 - 90.0)*rad_deg))*pq)+pq)

                                          /* Calaulated Glass Climb    */
            factor5 = (+width)*TAN((factor3 - 90.0)*rad_deg)


                                          /* Calculated short Leg*/
            leg1 = height - factor5
/* (AWD004) - beg left trap */

            shb(1%) = width               /* Base Value Width          */
            shb(2%) = factor2             /* Head Value Arch Radius    */
            shb(3%) = leg1                /* Left Leg Value            */
            shb(4%) = height              /* Right Leg Value           */

REM            shb(1%) = width + 0.25        /* Base Value Width          */
REM            shb(2%) = factor2 + 0.25      /* Head Value Arch Radius    */
REM            shb(3%) = leg1 + 0.25         /* Left Leg Value            */
REM            shb(4%) = height + 0.25       /* Right Leg Value           */

/* (AWD004) - end */
            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "BHLR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor3a            /* Calc for Angle 'B'        */
            sha(3%) = factor4a            /* Calc for Angle 'C'        */
            sha(4%) = 45.0                /* Calc for Angle 'D'        */

            sh_angle$  = "ABCD"


        return clear all
        goto exit_program

        calc_right_triangle               /* Right Triangle        (05)*/

                                          /* Short Leg Angle          */
            factor1 = (ARCTAN( (height) / (width))* deg_radians)

            factor2 = (90 - (factor1))    /* Peak Angle               */
                                          /* Peak Angle Bisect    (B) */
            factor2a = (factor2/ 2.0)
                                          /* (AWD002)                 */
                                          /* Base Angle           (C) */
            factor3a = ( 180 - (90. + factor2) ) / 2.0

                                          /* (AWD002)                 */
                                          /* Calc Width               */
        REM    width = width - ( (TAN((90 - factor1) * rad_deg) *pq) +   ~
        REM           (SQR((pq**2) + (TAN( (factor2) * rad_deg) * pq)**2) ) + pq)
                                          /* Calc Height               */
        REM    height = TAN((factor1) * rad_deg) * width

                                          /* Calc Diagonal             */
            factor3 = SQR( (height**2) + (width**2) )

/* (AWD004) - begin triangle */
            shb(1%) = width               /* Base Value Width          */
            shb(2%) = 0.0                 /* Head Value Arch Radius    */
            shb(3%) = height              /* Left Leg Value            */
            shb(4%) = factor3             /* Right Leg Value           */

REM            shb(1%) = width + 0.25        /* Base Value Width          */
REM            shb(2%) = 0.0                 /* Head Value Arch Radius    */
REM            shb(3%) = height + 0.25       /* Left Leg Value            */
REM            shb(4%) = factor3 + 0.25      /* Right Leg Value           */

/* (AWD004) - end */
            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "B?LR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor2a            /* Calc for Angle 'B'        */
                                          /* (AWD002)                  */
            sha(3%) = factor3a            /* Calc for Angle 'C'        */
                                          /* (AWD002)                  */
            sha(4%) = 0.0                 /* Calc for Angle 'D'        */

            sh_angle$  = "ABC?"

        return clear all
        goto exit_program

        calc_left_triangle                /* Right Triangle        (06)*/

                                          /* Short Leg Angle          */
            factor1 = (ARCTAN( (height) / (width))* deg_radians)

            factor2 = (90 - (factor1))    /* Peak Angle               */
                                          /* Peak Angle Bisect    (B) */
            factor2a = (factor2/ 2.0)
                                          /* (AWD002)                 */
                                          /* Base Angle           (C) */
            factor3a = ( 180 - (90. + factor2) ) / 2.0

                                          /* (AWD002)                 */

                                          /* Calc Width               */
        REM    width = width - ( (TAN((90 - factor1) * rad_deg) *pq) +   ~
        REM           (SQR((pq**2) + (TAN( (factor2) * rad_deg) * pq)**2) ) + pq)
                                          /* Calc Height               */
        REM    height = TAN((factor1) * rad_deg) * width

                                          /* Calc Diagonal             */
            factor3 = SQR( (height**2) + (width**2) )

/* (AWD004) - begin left triangle */
            shb(1%) = width               /* Base Value Width          */
            shb(2%) = 0.0                 /* Head Value Arch Radius    */
            shb(3%) = factor3             /* Left Leg Value            */
            shb(4%) = height              /* Right Leg Value           */

REM            shb(1%) = width + 0.25        /* Base Value Width          */
REM            shb(2%) = 0.0                 /* Head Value Arch Radius    */
REM            shb(3%) = factor3 + 0.25      /* Left Leg Value            */
REM            shb(4%) = height + 0.25       /* Right Leg Value           */
/* (AWD004) - end */

            shb(5%) = 0.0                 /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "B?LR??"

            sha(1%) = 45.0                /* Calc for Angle 'A'        */
            sha(2%) = factor2a            /* Calc for Angle 'B'        */
                                          /* (AWD002)                  */
            sha(3%) = factor3a            /* Calc for Angle 'C'        */
                                          /* (AWD002)                  */
            sha(4%) = 0.0                 /* Calc for Angle 'D'        */

            sh_angle$  = "ABC?"

        return clear all
        goto exit_program

        calc_isoc_triangle                /* ISOC Triangle         (07)*/

                                          /* Short Leg Angle           */
            factor1 = (ARCTAN( (height) / ((width/2)))* deg_radians)


            factor2 = (90 - (factor1))    /* Peak Angle               */
                                          /* Peak Angle Bisect        */
            factor2a = (factor2/ 2.0)
                                          /* Short leg Angle Bisect   */
            factor1a = (factor1/ 2.0)
                                          /* Calc Width                */

        REM    width = width - ( ( TAN( ( 90 - factor1) * rad_deg) * 1.0) + ~
        REM       (SQR((TAN((90 - factor1) * rad_deg) * 1.0)**2))) * 2

                                          /* Calc Height              */
        REM    height = TAN((factor1) * rad_deg) * (width / 2 )
                                          /* Calc Diagnonal           */
        REM    factor2 = SQR((height**2) + ((width/2.0)**2))
                                          /* Peak Angle               */
            factor3 = (180.0 - (2*factor1) )

/* (AWD004) - begin isoc  */
            shb(1%) = width               /* Base Value Width          */
            shb(2%) = 0.0                 /* Head Value Arch Radius    */
            shb(3%) = factor2             /* Left Leg Value            */
            shb(4%) = factor2             /* Right Leg Value           */

REM            shb(1%) = width + 0.25        /* Base Value Width          */
REM            shb(2%) = 0.0                 /* Head Value Arch Radius    */
REM            shb(3%) = factor2 + 0.25      /* Left Leg Value            */
REM            shb(4%) = factor2 + 0.25      /* Right Leg Value           */

/* (AWD004) - end */
            shb(5%) = height              /* Overall Height Value      */
            shb(6%) = 0.0                 /* Overall width Value       */

            sh_bend$    = "B?LR1?"

            sha(1%) = factor1a            /* Calc for Angle 'A'        */
            sha(2%) = factor2a            /* Calc for Angle 'B'        */
            sha(3%) = factor1a            /* Calc for Angle 'C'        */
            sha(4%) = 0.0                 /* Calc for Angle 'D'        */

            sh_angle$  = "ABC?"


        return clear all
        goto exit_program


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *                                                           *~
            *************************************************************

            read_glass_profile
               p = 0.0 : q = 0.0
               init(" ") readkey$
               str(readkey$,1%,9%)   = "SHPHFOFF "
               str(readkey$,10%,15%) = sh_model$       /* Model        */

                                    /* Load Equation for Field Calc.   */
               read #1,key = readkey$, using L60000, descr$,             ~
                                                    eod goto no_profile

               convert str(descr$,1%,6%)  to p, data goto no_profile

               convert str(descr$,24%,6%) to q, data goto no_profile

            return

            no_profile
               err% = 5%
            return

            check_shape_on
               shape_on% = 0%
               init(" ") readkey$
               convert shape% to shape$, pic(000)

               str(readkey$,1%,9%)   = "SHAP BEND"
               str(readkey$,10%,15%) = shape$          /* Shape Code   */

               read #1,key = readkey$, using L60000, descr$,             ~
                                                    eod goto not_on
L60000:           FMT POS(25), CH(30)

                  shape_on% = 1%

               return

            not_on
               shape_on% = 0%
            return

        load_entry_data
            if str(sh_entry$,kk%,1%) = "W" then                ~
               width = sh(kk%)                /* Entered Width */

            if str(sh_entry$,kk%,1%) = "H" then                ~
               height = sh(kk%)               /* Entered Height*/

            if str(sh_entry$,kk%,1%) = "R" then                ~
               radius = sh(kk%)               /* Entered Radius*/

            if str(sh_entry$,kk%,1%) = "S" then                ~
               leg1 = sh(kk%)            /* Entered Left Side Leg*/

            if str(sh_entry$,kk%,1%) = "Z" then                ~
               leg2 = sh(kk%)            /* Entered Right Side Leg*/

            if str(sh_entry$,kk%,1%) = "T" then                ~
               leg3 = sh(kk%)            /* Entered Top Leg    */

            if str(sh_entry$,kk%,1%) = "X" then                ~
               leg4 = sh(kk%)       /* Entered Side Leg Height */

            if str(sh_entry$,kk%,1%) = "L" then                ~
               leg5 = sh(kk%)            /* Entered Leg Height */

        return

        exit_program
           if shape_on% = 1% then gosub update_awdspecb

           gosub debug_display_screen

        end

        update_awdspecb
           if scr_sel% <> 2% then return
           n_c_pass% = 0%                          /* (AWD003) */

           gosub convert_sixteen               /* Nearest 1/16 of Inch */
           gosub convert_angles                    /* Round Up/Down    */

           init(" ") bd_rec$, sh_special$
           if sh_model$ = "765" or sh_model$ = "766" then sh_special$ = "Operabl"
           if sh_model$ = "767" or sh_model$ = "515" then sh_special$ = "Operabl"
           if sh_model$ = "516" or sh_model$ = "517" then sh_special$ = "Operabl"
           if sh_model$ = "B50" or sh_model$ = "B51" then sh_special$ = "Operabl"
           if sh_model$ = "B52" or sh_model$ = "B53" then sh_special$ = "Operabl"
           if sh_model$ = "B54" or sh_model$ = "B55" then sh_special$ = "Operabl"
           if trim% = 1% and str(dt_rec$,195%,2%) = "00" ~
                                 then sh_special$ = "Straigh"
           if trim% = 1% and str(dt_rec$,195%,2%) = "00" ~
                                 then sh_special$ = "Straigh"

/* (AWD006) */
           if mdl8900% = 1% and locks$ = "6" then sh_special$ = "NAIL"
/* (AWD007) */
           if mdl8900% = 1% and str(sub_part$,5%,1%) = "3"      ~
                                              then sh_special$ = "FOAM"
           if mdl8900% = 1% and str(sub_part$,5%,1%) = "4"      ~
                                              then sh_special$ = "FOAM"
           if mdl8900% = 1% and locks$ = "6" and                      ~
                 str(sub_part$,5%,1%) = "3" then sh_special$ = "NAIL/FM"
           if mdl8900% = 1% and locks$ = "6" and                      ~
                 str(sub_part$,5%,1%) = "4" then sh_special$ = "NAIL/FM"
/* (\AWD007) */

                                                   /* Production Date  */
           str(bd_rec$,1%,6%)   = str(dt_rec$,47%,6%)
                                                   /* Production Sort  */
           str(bd_rec$,7%,30%)  = str(dt_rec$,66%,30%)
                                                   /* 'SHAPCROSS' Code */
           str(bd_rec$,35%,2%)  = sh_cross$
                                                   /* Customer code    */
           str(bd_rec$,37%,9%)  = str(dt_rec$,124%,9%)
                                                   /* Customer S.O.    */
           str(bd_rec$,46%,8%)  = str(dt_rec$,24%,8%)
                                                   /* Prod. Shift Code */
           str(bd_rec$,54%,2%)  = str(dt_rec$,104%,2%)
                                                   /* Production Barcod*/
           str(bd_rec$,140%,18%)= str(dt_rec$,24%,18%)
                                                   /* prod Department  */
           str(bd_rec$,158%,3%) = str(dt_rec$,42%,3%)
                                                   /* Prod Process     */
           str(bd_rec$,161%,2%) = str(dt_rec$,45%,2%)
                                                   /* Prod Status      */
           str(bd_rec$,163%,2%) = str(dt_rec$,64%,2%)
                                                   /* MFG Part Number  */
           str(bd_rec$,165%,25%)= str(dt_rec$,189%,25%)
                                                   /* Prod Seq. No.    */
           str(bd_rec$,190%,5%) = str(dt_rec$,111%,5%)
                                                   /* Prod Load No.    */
           str(bd_rec$,195%,5%) = str(dt_rec$,1%,5%)
                                                   /* Line Item Text Id*/
           str(bd_rec$,200%,4%) = str(dt_rec$,236%,4%)
                                                   /* Wood Mull Code   */
           str(bd_rec$,204%,3%) = str(dt_rec$,217%,3%)
                                                   /* Special Notes    */
           str(bd_rec$,207%,7%) = sh_special$
                                                   /* Filler Area      */
           str(bd_rec$,214%,43%)= " "
                                                   /* ConvertBends     */
           jj% = 56%
           for kk% = 1% to 6%
             str(bd_rec$,jj%,10%) = "          "
             if shb(kk%) <= 0.0 then goto L60100
                convert shb(kk%) to str(bd_rec$,jj%,10%),pic(####.####-)

L60100:         jj% = jj% + 10%
           next kk%
                                                   /* Convert Angles   */
           jj% = 116%
           for kk% = 1% to 4%
             str(bd_rec$,jj%,6%) = "      "
             if sha(kk%) <= 0.0 then goto L60200
                convert sha(kk%) to str(bd_rec$,jj%,6%),pic(###.##)

L60200:         jj% = jj% + 6%
           next kk%

write_awdspecb:                                /* (AWD003) */
                                              /* Check Primary Key  1st  */
            init(" ") bd_key$, bd_key1$
            bd_key$ = str(bd_rec$,1%,36%)
            read #2,hold,key 0% = bd_key$, eod goto L60300
               delete #2
L60300:
                                              /* Check Secendary Key 2nd */
            bd_key1$ = str(bd_rec$,140%,23%)

            read #2,hold,key 1% = bd_key1$, eod goto L60400
               delete #2
L60400:
            put #2, using L60500, bd_rec$      /* Special Bend Record    */
L60500:        FMT CH(256)
                                               /*Create Sorted Bend Data */
            write #2, eod goto L60600

/* (AWD003) */
/*(AWD005)*/
            if specialmull$ <> "C" and trim% <> 1% then return
               if trim% = 1% then goto update_trim
               n_c_pass% = n_c_pass% + 1%
               if n_c_pass% = 3% then return

               if n_c_pass% <> 1% then goto not_1
               convert factor1_case to str(bd_rec$,56%,10%),pic(####.####-)

               convert radius_case  to str(bd_rec$,66%,10%),pic(####.####-)
               str(bd_rec$,207%,7%) = "CASE"
REM               str(bd_rec$,9,1) = "2"

               shape_cnt% = 0%
               convert str(bd_rec$,30%,5%) to shape_cnt%, data goto bad_cnt
               shape_cnt% = shape_cnt% + 1%

               convert shape_cnt% to str(bd_rec$,30%,5), pic(00000)

bad_cnt:
               str(bd_rec$,161%,2%) = "99"
               goto write_awdspecb

not_1:
               convert factor1_nail to str(bd_rec$,56%,10%),pic(####.####-)

               convert radius_nail  to str(bd_rec$,66%,10%),pic(####.####-)

               convert width_nail   to str(bd_rec$,106%,10%), pic(####.####-)

               convert str(bd_rec$,20%,10) to change_width, data goto bad_width
                             /* have to take away becasue 1000 has been added*/
                change_width = change_width - 2.125

                convert change_width to str(bd_rec$,20%,10%), pic(####.####-)

bad_width
               str(bd_rec$,207%,7%) = "NAIL"
REM               str(bd_rec$,9,1) = "1"                      /* Part of Tool Set Seq */

               shape_cnt% = 0%
               convert str(bd_rec$,30%,5%) to shape_cnt%, data goto bad_cnt1
               shape_cnt% = shape_cnt% + 1%

               convert shape_cnt% to str(bd_rec$,30%,5), pic(00000)

bad_cnt1:
               str(bd_rec$,161%,2%) = "98"
               goto write_awdspecb

REM        return
L60600:     err% = 7%
        return

        update_trim
           if shape% <> 51% then return
               shape_cnt% = 0%
               convert str(bd_rec$,30%,5%) to shape_cnt%, data goto bad_cnt2
               shape_cnt% = shape_cnt% + 1%

               convert shape_cnt% to str(bd_rec$,30%,5), pic(00000)

bad_cnt2:
               str(bd_rec$,161%,2%) = "99"
               trim% = 0%
               goto write_awdspecb



        convert_angles                         /* Round up or Down       */
             if shape% = 25% then return       /* Skip Octagon           */

             for rr% = 1% to 4%
                 r1% = 0% : r1, r2 = 0.0
                 r1 = int(sha(rr%))            /* Interger Value         */
                 if r1 <= 0.0 then goto R_1    /* Skip if Zero           */
                    r2 = sha(rr%) - r1         /* Save Decimal Value     */

                    if r2 > .499 then r1% = 1% /* Set Round Up Flag      */

                    if r1% = 1% then sha(rr%) = r1 + 1.0                   ~
                                else sha(rr%) = r1   /* Round Down       */
R_1:         next rr%
        return

        adjust_cut_length
             r1 = int(shb(1%))
             r2 = shb(1%) - r1
             if r2 >= .5 then shb(1%) = r1 + 1.0                          ~
                          else shb(1%) = r1
        return

        convert_sixteen
           r1, r2 = 0.0
        REM - NEAREST 16TH OF AN INCH
        REM   sz$ = ".0625.1250.1875.2500.3125.3750.4375.5000.5625.6250.6875~
        REM~.7500.8125.8750.9375     "

           for rr% = 2% to 6%
               if shb(rr%) <= 0.0 then goto C_3
               calc = shb(rr%)
               calc = round( calc, 4 )
               a% = int(calc)
               b% = int((calc - a%) * 10000)
               if b% = 0% then goto C_1                    /****************/
                  d% = int(b%/625)                         /* Conversion of*/
                  if mod(b%,625) <> 0 then d% = d% + 1%    /* Decimals to  */
                     b% = d%                               /*  Sixteen's   */
                     if b% <> 16% then goto C_1            /****************/
                        a% = a% + 1% : b% = 0%             /* A% = WHOLE PART */
C_1:
               r1, r2 = 0.0
               if b% = 0% then goto C_2                     /* Check Fraction */
                  r1$ = str(sz$,(b%*5%) - 4%, 5%)           /* Pull Decimal   */
                  convert r1$ to r1, data goto C_2

C_2:
              r2 = a% + r1


              shb(rr%) = r2
C_3:
              next rr%
        return

        debug_display_screen
            if debug% = 0% then return             /* Debug Turned Off */

            init(" ") sh$(), shb$(), p$, q$, temp$(6%), pf$(), date$, temp$()

            header$ = "AWD S.S. 'Debug' Shape Code (XX) Barcode = " &    ~
                      str(dt_rec$,24%,18%)


            if scr_sel% <> 2% then header$ = "AWD S.S. 'Debug' Shape Code (XX) "


            convert shape% to str(header$,30%,2%), pic(00)


            mat temp = zer

            inpmessage$ = str(dt_rec$,111%,5%) &"  Press <Return> to Continue?"

            date$ = date
            call "DATEFMT" (date$)

            convert p to p$, pic(######.####-)

            convert q to q$, pic(######.####-)
                                                     /* Temp Data Area     */

            temp(1%) = sha(1%)
            temp(2%) = sha(2%)
            temp(3%) = sha(3%)
            temp(4%) = sha(4%)
            temp(5%) = sha(5%)

                                                     /* Temp Data Area    */

            for kk% = 1% to 6%

                convert sh(kk%) to sh$(kk%), pic(######.####-)

                convert shb(kk%) to shb$(kk%), pic(######.####-)

                convert temp(kk%) to temp$(kk%), pic(######.####-)

            next kk%
            gosub set_keys

            accept                                                       ~
               at (01,02), fac(hex(84)), header$                , ch(62),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), "<Data Entered>",                             ~
               at (03,20), "<Data Calculated>",                          ~
               at (04,03), fac(hex(84)), sh$(1%)                , ch(12),~
               at (04,21), fac(hex(84)), shb$(1%)               , ch(12),~
                                                                         ~
               at (05,03), fac(hex(84)), sh$(2%)                , ch(12),~
               at (05,21), fac(hex(84)), shb$(2%)               , ch(12),~
                                                                         ~
               at (06,03), fac(hex(84)), sh$(3%)                , ch(12),~
               at (06,21), fac(hex(84)), shb$(3%)               , ch(12),~
                                                                         ~
               at (07,03), fac(hex(84)), sh$(4%)                , ch(12),~
               at (07,21), fac(hex(84)), shb$(4%)               , ch(12),~
                                                                         ~
               at (08,03), fac(hex(84)), sh$(5%)                , ch(12),~
               at (08,21), fac(hex(84)), shb$(5%)               , ch(12),~
                                                                         ~
               at (09,03), fac(hex(84)), sh$(6%)                , ch(12),~
               at (09,21), fac(hex(84)), shb$(6%)               , ch(12),~
                                                                         ~
               at (11,02), "Label Field Flags         :",                ~
               at (11,30), fac(hex(84)), sh_bend$               , ch(06),~
                                                                         ~
               at (12,02), "Location of Field Data    :",                ~
               at (12,30), fac(hex(84)), sh_angle$              , ch(04),~
                                                                         ~
               at (13,02), "Name of Data Field Entered:",                ~
               at (13,30), fac(hex(84)), sh_entry$              , ch(07),~
                                                                         ~
               at (12,02), "Profile Adjustment        :",                ~
               at (12,30), fac(hex(84)), p$                     , ch(12),~
                                                                         ~
               at (13,02), "Glazing Bead Adjustment   :",                ~
               at (13,30), fac(hex(84)), q$                     , ch(12),~
                                                                         ~
               at (14,02), "Temp Value (1)            :",                ~
               at (14,30), fac(hex(84)), temp$(1%)              , ch(12),~
                                                                         ~
               at (15,02), "Temp Value (2)            :",                ~
               at (15,30), fac(hex(84)), temp$(2%)              , ch(12),~
                                                                         ~
               at (16,02), "Temp Value (3)            :",                ~
               at (16,30), fac(hex(84)), temp$(3%)              , ch(12),~
                                                                         ~
               at (17,02), "Temp Value (4)            :",                ~
               at (17,30), fac(hex(84)), temp$(4%)              , ch(12),~
                                                                         ~
               at (18,02), "Temp Value (5)            :",                ~
               at (18,30), fac(hex(84)), temp$(5%)              , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

               if keyhit% = 16% then goto debug_display_screen

            return

        set_keys
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
        return

