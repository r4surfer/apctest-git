        REM *************************************************************~
            *                                                           *~
            *  EEEEE  W   W  DDDD    CCC    AAA   L      SSSSS  SSSSS   *~
            *  E      W   W  D   D  C   C  A   A  L      S      S       *~
            *  EEEE   W   W  D   D  C      AAAAA  L      SSSSS  SSSSS   *~
            *  E      W W W  D   D  C   C  A   A  L          S      S   *~
            *  EEEEE   W W   DDDD    CCC   A   A  LLLLL  SSSSS  SSSSS   *~
            *                  ( SUBROUTINE )                           *~
            *-----------------------------------------------------------*~
            * EWDCALSS - Calculate Cut Glass Dimensions for all Special *~
            *            Shapes.                                        *~
            *  Last Modified Date- 06/30/2019                           *~
            *                                                           *~
            *     - SHAPE%   - Special Shapes Code               ( In  )*~
            *                                                           *~
            *     - shc()    - Calculated Values                 ( Out )*~
            *                                                           *~
            *     - sh_fields$- Flags used to tell which field to ( Out)*~
            *                   print on Special Shapes Label.          *~
            *                                                           *~
            *     - sh_position$ - Location of print data in the shc()  *~
            *                      Calculated array.              ( Out )*~
            *                                                           *~
            *     - sh_entry$    - Names of Fields Entered       ( In  )*~
            *                                                           *~
            *     - #1       - Channel for (GENCODES) File       ( In  )*~
            *                                                           *~
            *     - ERR%     - Error Code                        ( Out )*~
            *                  (0%) All Ok                              *~
            *                  (1%) Error with Width                    *~
            *                  (2%) Error with Height                   *~
            *                  (3%) Error with Radius                   *~
            *                  (4%) Error with Leg                      *~
            *                  (5%) Error with Profile Table            *~
            *                  (6%) Error No Equation                   *~
            *                                                           *~
            *    Note - Special Mod to 'OCTAGON' for Window wizard      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/16/04 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 02/23/04 ! (AWD001) Special Adjustment for Half     ! RHH *~
            *          !          round Brick Mull (553, 554)     !     *~
            * 03/02/04 ! (AWD002) Add Irregular Left/Right Pent   ! RHH *~
            *          !          Update left/Right Trapezoid     !     *~
            *          !                                          !     *~
            * 05/06/04 !          Last Update of Equations        ! RHH *~
            * 05/28/04 ! (AWD003) Special change to add a remake  ! RHH *~
            *          !          for Selection (5). To do special!     *~
            *          !          when Glass Custom sizes entered !     *~
            * 06/25/04 ! (AWD004) Mod to fix problems with Eyebrow! RHH *~
            *          !          1/2 Left and Right Eyebrow when !     *~
            *          !          glass dimensions are enteres.   !     *~
            * 07/15/04 ! (AWD005) Mod to Fix Remake Glass Probs   ! RHH *~
            *          !          for TRIPLES. For "T" do not Calc!     *~
            *          !          glass Radius                    !     *~
            * 08/04/04 ! (AWD006) Mod to correct Half Round and   ! RHH *~
            *          !          elliptical when 'Y' Used Sel=5  !     *~
            * 08/31/04 ! (AWD007) Mod to Fix 1/2 Left and Right   ! RHH *~
            * 11/03/04 !          Colonial Arch.                  !     *~
            * 03/17/05 ! (AWD008) Mod to increase correction for  ! RHH *~
            *          !          Brick Mold for Ellipticals      !     *~
            * 01/01/06 ! (PAR000) No Change                       ! RHH *~
            * 03/29/07 ! (AWD009) mod for spec adjustment on Casem! CMG *~
            * 02/05/08 ! (AWD010) mod for non-j half round        ! CMG *~
            *03/25/2009! (AWD011) add .3750 to florida HR         ! CMG *~
            *05/01/2012! (AWD012) adjustment for 8900 HR          ! CMG *~
            *04/09/2015! (IM8010) move hardcode values to gencodes! CMG *~
            *12/01/2016! (CR793) mod for colonial arch formula    ! CMG *~
            *02/02/2017! (CR503) Mods for GED calc values         ! CMN *~
            *06/30/2019! (CR2109) mod for operable shapes         ! CMN *~            
            *************************************************************

        sub "EWDCALSS"   (shape%,        /* Shape Code                 */~
                          sh_model$,     /* Model Code                 */~
                          sh(),          /* Data Entry Values          */~
                          shc(),         /* Calculated Values          */~
                          sh_fields$,    /* Label Field Flags          */~
                          sh_position$,  /* Location of Field Data     */~
                          sh_entry$,     /* Name of Data Field Entered */~
                          dt_bar$,       /* Barcode for Debug          */~
                          sh_cross$,     /* 'SHAPCROSS' Code           */~
/*(CR503)*/               ged_shape$,    /* GED Shape Code             */~
/*(CR503)*/               ged_fields$,   /* GED Shape Field Entries    */~
/*(CR503)*/               ged_shc(),     /* GED Shape Calculated Value */~
/*(CR2109)*/              glswidth,      /* Bottom Sash Width          */~
/*(CR2109)*/              glsheight,     /* Bottom Sash Height         */~
/*(CR2109)*/              operableshape%,/* Operable Shape Flag        */~
                          #1,            /* GENCODES File              */~
                          err% )         /* Error Code 0 = Ok, 0 <> err*/

        dim                                                              ~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30,                    /* Use for GENCODES Look-Up   */~
            dt_bar$18,                   /* Barcode                    */~
            sh_model$3,                  /* Model Code                 */~
            shc(10%),                    /* Calculated Values          */~
            ged_shape$10,                /* Ged Shape Code     (CR503) */~
            ged_fields$10,               /* Data Entry Values  (CR503) */~
            ged_shc(10%),                /* Calculated Values  (CR503) */~
            sh_entry$7,                  /* Name of Data field Entered */~
            sh_fields$7,                 /* Print fields for Label 1-6 */~
            sh_position$7,               /* Location of Print Fields Data*/~
            sh_cross$2                   /* 'SHAPCROSS' Code Value     */

        dim                                                              ~
            sh$(7%)12,                  /* Data Entered Values         */~
            shc$(7%)12,                 /* data Calculated Values      */~
            temp(7%), temp$(7%)12,      /* Debug Values                */~
            header$62,                  /* Debug Header                */~
            p$12, q$12,                 /* Profile Adj and Glazing Beed*/~
            date$8,                     /* todays Date                 */~
            cursor%(2%),                /* Cursor Location for Edit    */~
            i$(24%)80,                  /* Screen Image                */~
            inpmessage$79,              /* Informational Message       */~
            pf$(3%)79,                  /* PF Screen Literals          */~
            pfkeys$32                   /* PF Key Hex Values           */
/* (CR503) */
         dim table$9,                    /* Table To Read              */~
             genkey$15,                  /* GENCODES Key to Read       */~
             descr1$30,                  /* Description                */~
             codeLen$2,                  /* Code Length                */~
             descr2$30,                  /* Description                */~
             descr3$30                   /* Description                */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 07/15/04 Calculate Special Shapes Glass"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master System Table File                 *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************
            debug% = 0%                              /* (RHHTEST)       */

                                             /* (AWD003)                */
                                             /* (AWD005)                */
                                             /* Use for Selection 5     */
                                             /* Glass Cust size Entered */
            remake_flag% = 0%
            if str(sh_entry$,7%,1%) = "Y" then remake_flag% = 1%

            if str(sh_entry$,7%,1%) = "T" then remake_flag% = 2%

                                             /* (AWD003)                */
                                             /* (AWD005)                */
            u3% = 0%

            err% = 0%                                 /* SET ERROR FLAG */
            init(" ") sh_fields$, sh_position$, ged_shape$, ged_fields$, ~
                      descr1$, descr2$, descr3$, codeLen$

            final = 0.0
            width, height, radius, leg1, leg2, leg3, leg4, leg5 = 0.0
            mat shc = zer

           for kk%=1% to 6%
               if sh(kk%) < .1 then goto entry_next
                  gosub load_entry_data


entry_next: next kk%


             p, q, factor1, factor2, factor_exp  = 0.00
             factor3, factor4, factor5, factor6 = 0.00

             roy, roy1, roy2, roy3, roy4, roy5 = 0.0

             deg_radians = 57.29577951
             rad_deg     = 0.0174532
             sixth       = 0.0625


        REM *************************************************************~
            *            B E G I N   C A L C U L A T I O N S            *~
            *                                                           *~
            *************************************************************

        if shape% =  0% then gosub calc_picture                /* 01-STD */
        if shape% = 01% then gosub calc_right_irreg_pentagon   /* 03-STD */
        if shape% = 02% then gosub calc_left_irreg_pentagon    /* 03-STD */
        if shape% = 03% then gosub calc_right_trapezoid        /* 05-STD */
        if shape% = 04% then gosub calc_left_trapezoid         /* 05-STD */
        if shape% = 05% then gosub calc_right_triangle         /* 02-STD */
        if shape% = 06% then gosub calc_left_triangle          /* 02-STD */
        if shape% = 07% then gosub calc_isoc_triangle          /* 02-STD */
        if shape% = 15% then gosub calc_doghouse_pentagon      /* 03-STD */
        if shape% = 25% then gosub calc_octagon                /* 10-STD */
        if shape% = 51% then gosub calc_circle                 /*  6-STD */
        if shape% = 60% then gosub calc_eyebrow                /*  4-STD */
        if shape% = 63% then gosub calc_colonial_arch          /*  4-STD */
        if shape% = 64% then gosub calc_half_round             /*  7-STD */
        if shape% = 66% then gosub calc_right_qtr_round        /*  8-STD */
        if shape% = 67% then gosub calc_left_qtr_round         /*  8-STD */
        if shape% = 70% then gosub calc_half_right_eyebrow     /*  8-STD */
        if shape% = 71% then gosub calc_half_left_eyebrow      /*  8-STD */
        if shape% = 73% then gosub calc_half_rt_colonial_arch  /*  8-STD */
        if shape% = 74% then gosub calc_half_lf_colonial_arch  /*  8-STD */
                                          /* No Equation Defined       */
        err% = 6%
        return clear all
        goto exit_program


        calc_right_irreg_pentagon         /* Shape Code (01)  (AWD002) */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p + q)
                                          /* Short Leg - leg2          */
                                          /* Top Leg   - leg3          */
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_right_1
               factor4 = width
               factor5 = height
               roy5    = leg2
               roy3    = leg3
               goto calc_right_2

calc_right_1:

                                          /* Chop Diagonal             */
            factor1 = SQR( ( ( height - leg2)**2) + ( (width - leg3)**2) )

                                          /* Segment Climb             */
            factor2 = (height - leg2)

                                          /* Pitch                     */
            factor3 = ( (height - leg2) / (width - leg3) )*12

                                          /* Calculated Glass width    */
            factor4 = width - (2*pq)

                                          /* Calculated Glass Height  */
            factor5 = height - (2*pq)

                                          /* Short Leg Angle          */
            roy = (ARCTAN((height - leg2)/(width - leg3))*deg_radians) + 90.0

                                          /* Peak Angle               */
            roy1 = (90.0 - (roy - 90.0)) + 90.0

                                          /* Short Leg Bisect Angle   */
            roy2 = roy/2

                                          /* Calculated Glass Top Leg */
            roy3 = leg3 - ((TAN((90.0 - (roy1/2))*rad_deg)*(2*pq)) + (2*pq))

                                          /* Calculated Glass Climb  */
            roy4 = (factor4 - roy3) * TAN((roy - 90.0) * rad_deg)

                                          /* Calculated Glass short leg */
            roy5 = (factor5 - roy4)

calc_right_2:                             /* (AWD003)                */
            shc(1%) = factor4
            shc(2%) = factor5
            shc(3%) = roy5
            shc(4%) = roy3

            sh_fields$    = "WHZTNNN"
            sh_position$  = "1234000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)


            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor4)
            factor4 = final
            gosub '051(factor5)
            factor5 = final
            gosub '051(roy5)
            roy5 = final
            gosub '051(roy3)
            roy3 = final

            ged_shc(1%) = factor4
            ged_shc(2%) = factor5
            ged_shc(3%) = roy5              /* DimA    */
            ged_shc(4%) = roy5              /* DimB    */
            ged_shc(5%) = roy3              /* DimC    */
            ged_shc(6%) = roy3              /* DimD    */
            ged_shc(7%) = (width - roy3)    /* DimE    */

                                            /* (CR503)  -                */

        return clear all
        goto exit_program

        calc_left_irreg_pentagon          /* Shape Code (02) (AWD002)  */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p + q)
                                          /* Short Leg - leg1          */
                                          /* Top Leg   - leg3          */
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_left_1
               factor4 = width
               factor5 = height
               roy5    = leg1
               roy3    = leg3
               goto calc_left_2

calc_left_1:

                                          /* Chop Diagonal             */
            factor1 = SQR( ( ( height - leg1)**2) + ( (width - leg3)**2) )

                                          /* Segment Climb             */
            factor2 = (height - leg1)

                                          /* Pitch                     */
            factor3 = ( (height - leg1) / (width - leg3) )*12

                                          /* Calculated Glass width    */
            factor4 = width - (2*pq)

                                          /* Calculated Glass Height  */
            factor5 = height - (2*pq)

                                          /* Short Leg Angle          */
            roy = (ARCTAN((height - leg1)/(width - leg3))*deg_radians) + 90.0

                                          /* Peak Angle               */
            roy1 = (90.0 - (roy - 90.0)) + 90.0

                                          /* Short Leg Bisect Angle   */
            roy2 = roy/2

                                          /* Calculated Glass Top Leg */
            roy3 = leg3 - ((TAN((90.0 - (roy1/2))*rad_deg)*(2*pq)) + (2*pq))

                                          /* Calculated Glass Climb  */
            roy4 = (factor4 - roy3) * TAN((Roy - 90.0) * rad_deg)

                                          /* Calculated Glass short leg */
            roy5 = (factor5 - roy4)

calc_left_2:                              /* (AWD003)               */
            shc(1%) = factor4
            shc(2%) = roy5
            shc(3%) = factor5
            shc(4%) = roy3


            sh_fields$    = "WSHTNNN"
            sh_position$  = "1234000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor4)
            factor4 = final
            gosub '051(factor5)
            factor5 = final
            gosub '051(roy5)
            roy5 = final
            gosub '051(roy3)
            roy3 = final

            ged_shc(1%) = factor4
            ged_shc(2%) = factor5
            ged_shc(3%) = roy5              /* DimA    */
            ged_shc(4%) = roy5              /* DimB    */
            ged_shc(5%) = roy3              /* DimC    */
            ged_shc(6%) = roy3              /* DimD    */
            ged_shc(7%) = (width - roy3)    /* DimE    */

                                            /* (CR503)  -                */

        return clear all
        goto exit_program


        calc_doghouse_pentagon            /* Shape Code (15)           */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

            pq2 = 2*(p + q)               /* leg2 = Short Leg          */

            deg_radians = 57.29577951
            rad_deg     = .0174532

                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_doghouse_1
               width  = width
               height = height
               leg2   = leg2
               leg3   = leg2

               goto calc_doghouse_2

calc_doghouse_1:
                                          /* Pentoid Diagonal         */
            factor1 = SQR( ( (height - leg2)**2) + ( (width/2.0)**2) )

                                          /* Climb                    */
            factor2 = height - leg2
                                          /* Short Leg Angle           */
            factor3 = (ARCTAN((height - leg2)/( (width/2.0) ) )*deg_radians) + 90.0

                                          /* Peak Angle                */
            factor4 = (90.0 - (factor3 - 90.0) ) *2
                                          /* Calculated Glass Width    */
            width   = width - (2*pq)

                                          /* Calculated Glass Height   */
            height  = height - SQR( ( (TAN( (90 - (factor4/2.0) )*rad_deg)*pq)**2) ~
                            + (pq**2) ) - pq

                                          /* Calculated Glass Climb    */
            factor5 = (+width/2.0) * TAN( ( factor3 - 90.0) * rad_deg)

                                          /* Calculated Glass short Leg*/
            leg2 = height - factor5
                                          /* Calculated Glass Diagonal */
            leg3 = SQR( ( ( width/2.0)**2) + (factor5**2) )

            leg4 = (width / 2.0)          /* (SR79949)    */

calc_doghouse_2:                          /* (AWD003)                  */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = height              /* Glass height              */
            shc(3%) = leg2                /* Glass Short Leg           */
            shc(4%) = leg3                /* Glass Diagonal            */

            sh_fields$    = "WHZTNNN"
            sh_position$  = "1234000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(leg2)
            leg2 = final
            gosub '051(leg3)
            leg3 = final
            gosub '051(leg4)
            leg4 = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = leg2              /* DimA    */
            ged_shc(4%) = leg2              /* DimB    */
REM GED_SHC(5%) = LEG3              /* DIMC  (SR79949)  */
REM GED_SHC(6%) = LEG3              /* DIMD  (SR79949)  */
REM GED_SHC(7%) = (WIDTH - LEG3)    /* DIME   (SR79949) */
            ged_shc(5%) = leg4              /* DimC  (SR79949)  */
            ged_shc(6%) = leg4              /* DimD  (SR79949)  */
            ged_shc(7%) = leg4              /* DimE  (SR79949)  */

                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if leg2    <= 0.0 then err% = 4%
            if leg3    <= 0.0 then err% = 8%


        return clear all
        goto exit_program

        calc_half_round                   /* Shape Code (64)           */
                                          /* Test for Elliptical       */
            if str(sh_cross$,1%,1%) = "C" then goto calc_elliptical

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* (AWD003)                  */
                                          /* (AWD005) Always Calc Radius*/
            if remake_flag% = 0% then goto calc_half_1

               width   = width             /* Entered Glass Width       */
               height  = height            /* Entered Glass Height      */
                                           /* Calculate glass Radius    */
               leg1    = height + p + q    /* Glass Radius-Corrected    */
                                           /* (AWD006)                  */
               goto calc_half_2



calc_half_1:

            radius = (height - p - q)     /* Calculate Radius          */
/* (IM8010) */
            height = (radius - p - q)     /* Calculate Height          */
            height = height + ph          /* Calculate Height          */
                                          /* Calculate the Width       */
            factor1 = (2 * height)
            factor_exp = (height * height)
            factor2 = ((radius * factor1) - factor_exp)

            width = SQR(factor2)
            width = (width * 2)
            width = width + pw
                                          /* leg1 contains the Radius  */
            leg1    = radius
                                          /* (AWD001) Brick Mull Height*/
                                          /* Adjustment                */
REM            IF SH_MODEL$ = "553" THEN HEIGHT = HEIGHT + .1250
REM            IF SH_MODEL$ = "554" THEN HEIGHT = HEIGHT + .1250
REM            IF SH_MODEL$ = "485" THEN HEIGHT = HEIGHT + .5000

/*(AWD009)*/
REM            IF SH_MODEL$ = "194" THEN HEIGHT = HEIGHT + .5000
REM            IF SH_MODEL$ = "164" THEN HEIGHT = HEIGHT + .5000
/* NEW CASEMENT MODEL */
REM            IF SH_MODEL$ = "184" THEN HEIGHT = HEIGHT + .5000
REM            IF SH_MODEL$ = "174" THEN HEIGHT = HEIGHT + .5000
/*(AWD009/)*/

/*(AWD010)*/
REM            IF SH_MODEL$ = "B42" THEN HEIGHT = HEIGHT + .5000
/*(AWD010/)*/
/* (AWD011) */
REM           IF SH_MODEL$ = "F66" THEN HEIGHT = HEIGHT + .3750
REM           IF SH_MODEL$ = "F76" THEN HEIGHT = HEIGHT + .3750
REM           IF SH_MODEL$ = "F86" THEN HEIGHT = HEIGHT + .3750
REM           IF SH_MODEL$ = "F96" THEN HEIGHT = HEIGHT + .3750
/* (/AWD011) */
/* (AWD012) */
REM           IF SH_MODEL$ = "E66" THEN HEIGHT = HEIGHT + .3750
/* (/AWD012) */
                                          /* (AWD001) Adjustment       */
calc_half_2:
            shc(1%) = height              /* Glass height              */
            shc(3%) = leg1                /* Glass Radius              */
            shc(4%) = width               /* Glass Width               */

            sh_fields$    = "HRWNNNN"
            sh_position$  = "1340000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            ged_shc(1%) = width
            ged_shc(2%) = height
                                            /* (CR503)  -                */

            if width  <= 0.0 then err% = 1%
            if height <= 0.0 then err% = 2%
            if leg1   <= 0.0 then err% = 3%

        return clear all
        goto exit_program

        calc_elliptical                   /* Shape Code (64)           */

            gosub read_glass_profile      /* Get Profile and Glazing   */

                                          /* (AWD003)                  */
                                          /* (AWD005) Always Calc Radius*/
            if remake_flag% = 0% then goto calc_elliptical_1

               width   = width             /* Entered Glass Width       */
               height  = height            /* Entered Glass Height      */
                                           /* Calculate glass Radius    */
        REM       leg1    = height + p + q /* Glass Radius              */

               factor1 = (width/2.0)**2
               factor2 = (height**2)
               factor3 = (2 * height)
                                           /* Frame Radius              */
               factor4 = (factor1 + factor2) / factor3

               leg1 = factor4              /* Glass Radius              */
                                           /* (AWD006)                  */

               goto calc_elliptical_2

calc_elliptical_1:

                                          /* Bead Deduction            */
            radius = ( (width/2)**2 + (width/3)**2) / (2*(width/3) )
                                          /* Glass Radius              */
            radius = (radius - (p + q))

                                          /* Calculated Height         */
            height = (width/3) - (2*p) - q
/* (IM8010) */
            height = height + ph
                                          /* Calculate the Width        */
            factor1 = (2 * height)
            factor_exp = (height * height)
            factor2 = ((radius * factor1) - factor_exp)

            width = SQR(factor2)
            width = (width * 2)
/* (IM8010) */
            width = width + pw
                                          /* leg1 contains the Radius  */
            leg1    = radius
                                          /* (AWD001) Brick Mull Height*/
                                          /* Adjustment                */
                                          /* (AWD008)Reduce adjustment */
                                          /* for width and height      */
REM            IF SH_MODEL$ = "558" THEN HEIGHT = HEIGHT - .2500
REM            IF SH_MODEL$ = "559" THEN HEIGHT = HEIGHT - .2500
REM            IF SH_MODEL$ = "558" THEN WIDTH  = WIDTH - .5000
REM            IF SH_MODEL$ = "559" THEN WIDTH  = WIDTH - .5000
                                          /*(AWD008) Width and Height Adj.*/
                                          /* (AWD001) Adjustment       */

calc_elliptical_2:                        /* (AWD003)                  */
            shc(1%) = height              /* Glass height              */
            shc(3%) = leg1                /* Glass Radius              */
            shc(4%) = width               /* Glass Width               */

            sh_fields$    = "HRWNNNN"
            sh_position$  = "1340000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            ged_shc(1%) = width
            ged_shc(2%) = height
                                            /* (CR503)  -                */

            if width  <= 0.0 then err% = 1%
            if height <= 0.0 then err% = 2%
            if leg1   <= 0.0 then err% = 3%

        return clear all
        goto exit_program


        calc_colonial_arch                /* Shape Code (63)           */
            if operableshape% = 1% and glsheight > 0 then goto          ~
                               calc_operable_colonial_arch /* (CR2109) */
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            if remake_flag% = 0% then goto calc_colonial_1
               width  = width
               height = height
               leg1   = leg1

               if remake_flag% = 2% then goto calc_colonial_3:
                                         /* Don't Calc Radius          */
               goto calc_colonial_2

calc_colonial_1:

            width  = (width  - 2*(p + q)) /* Calculate width           */
            width  = (width + pw)         /* (CR793)                   */
            height = (height - 2*(p + q)) /* Calculate Height          */
            height = (height + ph)        /* (CR793)                   */

calc_colonial_2:
                                          /* Calculate the leg1         */
            leg1  =  (height - (width/2.0))

calc_colonial_3:
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = height              /* Glass Height              */
            shc(3%) = leg1                /* Glass Leg                 */

            sh_fields$    = "WHLNNNN"
            sh_position$  = "1230000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(leg1)
            leg1 = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = leg1                     /* DimA    */
            ged_shc(4%) = (height - leg1)          /* DimB    */
                                            /* (CR503)  -                */

            if width  <= 0.0 then err% = 1%
            if height <= 0.0 then err% = 2%
            if leg1   <= 0.0 then err% = 4%


        return clear all
        goto exit_program



        calc_eyebrow                      /* Shape Code (60)           */
            if operableshape% = 1% and glsheight > 0 then goto          ~
                                     calc_operable_eyebrow /* (CR2109) */
            debug% = 0%
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p+ q)
                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            if remake_flag% = 0% then goto calc_eyebrow_1
                                          /* (AWD004)                  */
               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg5)**2)

               factor3 = 2*(height - leg5)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width
                                             /* Glass Height           */
               factor3 = height
                                             /* Glass Radius           */
               factor4 = radius
                                             /* Glass Leg              */
               factor6 = leg5
                                             /* (AWD004)               */
               if remake_flag% = 2% then factor4 = leg5
                                             /* (AWD005)               */
               goto calc_eyebrow_2

calc_eyebrow_1:

            s1 = ABS((height - leg5))     /* Segment Climb             */

                                          /* Segment Radius            */
            factor1 = ((4*(s1**2)) + (width**2)) / (8*s1)

            radius = factor1

                                          /* Glass Width              */
            factor2 = width - (pq + pq)
                                          /* Glass Height             */
            factor3 = height - (pq + pq)
/* (IM8108) */
            factor3 = factor3 + ph
                                          /* Glass ARC Radius         */
            factor4 = radius - pq
                                          /* Glass ARC Rise           */
            factor5 = factor4 - (0.5*(SQR((4*(factor4**2)) - (factor2**2))))
                                          /* Glass short Leg          */
/* (IM8108) */
            factor6 = height - (pq + pq + factor5)
            factor6 = factor6 + ph

calc_eyebrow_2:                          /* (AWD003)                  */
            shc(1%) = factor2            /* Glass Width               */
            shc(2%) = factor3            /* Glass Height              */

                                         /* Special Label Mod         */
            shc(4%) = factor6            /* Glass Short Leg           */
                                         /* ** for Label Only ***     */
                                         /* Special label Mod         */

            shc(5%) = factor4            /* Glass Arc Radius          */

            sh_fields$    = "WHRLNNN"
            sh_position$  = "1254000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(factor6)
            factor6 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = factor3
            ged_shc(3%) = factor6                  /* DimA    */
            ged_shc(4%) = (factor3 - factor6)      /* DimB    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if factor3 <= 0.0 then err% = 2%
            if factor4 <= 0.0 then err% = 3%
            if factor6 <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_half_right_eyebrow           /* Shape Code (70)           */
                                                           /* (CR2109) */
            if operableshape% = 1% and glsheight > 0 then goto         ~
                                         calc_operable_half_right_eyebrow
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            radius_sav = radius

            if remake_flag% = 0% then goto calc_eyebrow_right_1
                                          /* (AWD004)                  */
               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg2)**2)

               factor3 = 2*(height - leg2)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width
                                             /* Glass Height           */
               height  = height
                                             /* Glass Radius           */
               factor3 = radius
                                             /* Glass Leg              */
               leg2    = leg2
                                             /* (AWD004)               */
                                             /* (AWD005)               */
               if remake_flag% = 2% then factor3 = radius_sav
                                             /* No calc radius         */
               goto calc_eyebrow_right_2

calc_eyebrow_right_1:

            s1 = ABS((height - leg2))     /* Segment Climb             */

                                          /* Segment Radius            */
            factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                          /* Glass Width               */
            factor2 = (width -(pq + pq) )

                                          /* Glass Height              */
            height  = (height -(pq + pq) )
            height  = height + ph            

                                          /* Glass Arc Radius          */
            factor3 = (factor1 - pq)

                                          /* Glass ARC Rise            */

            factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - (2*pq))**2))))

                                          /* Glass short Leg           */

            leg2    = height - factor4

calc_eyebrow_right_2:                     /* (AWD003)                  */
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = height              /* Glass Height              */
            shc(3%) = leg2                /* Glass Short Leg           */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$    = "WHZRNNN"
            sh_position$  = "1235000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg2)
            leg2 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = height
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg2                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg2    <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_half_left_eyebrow            /* Shape Code (71)           */
                                                           /* (CR2109) */
            if operableshape% = 1% and glsheight > 0 then goto          ~
                                          calc_operable_half_left_eyebrow
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            radius_sav = radius

            if remake_flag% = 0% then goto calc_eyebrow_left_1
                                          /* (AWD004)                  */
               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg1)**2)

               factor3 = 2*(height - leg1)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width
                                             /* Glass Height           */
               height  = height
               height  = height + ph               
                                             /* Glass Radius           */
               factor3 = radius
                                             /* Glass Leg              */
               leg1    = leg1
                                             /* (AWD004)               */
                                             /* (AWD005)               */
               if remake_flag% = 2% then factor3 = radius_sav
                                             /* No Calc Radius         */
               goto calc_eyebrow_left_2

calc_eyebrow_left_1:


            s1 = ABS((height - leg1))     /* Segment Climb             */

                                          /* Segment Radius            */
            factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                          /* Glass Width               */
            factor2 = (width -(pq + pq) )
                                          /* Glass Height              */
            height  = (height -(pq + pq) )
                                          /* Glass Arc Radius          */
            factor3 = (factor1 - pq)
                                          /* Glass ARC Rise            */

            factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - (2*pq))**2))))
                                          /* Glass short Leg           */

            leg1    = height - factor4

calc_eyebrow_left_2:                      /* (AWD003)                  */
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = leg1                /* Glass Short Leg           */
            shc(3%) = height              /* Glass Height              */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$    = "WSHRNNN"
            sh_position$  = "1235000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg1)
            leg1 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = height
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg1                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg1    <= 0.0 then err% = 4%


        return clear all
        goto exit_program


        calc_octagon                      /* Shape Code (25)          */
            gosub read_glass_profile      /* Get Profile and Glazing  */
                                          /* Bead Deduction           */

                                          /* (AWD003)                 */
                                          /* (AWD005)                 */
            if remake_flag% = 0% then goto calc_octagon_1
               width   = width
               height  = height
               leg1    = round(height * .413, 5)
               leg2    = round(height * .413, 5)

               goto calc_octagon_2

calc_octagon_1:


            width  = (width  - 2*(p + q)) /* Calculate width          */
            height = (height - 2*(p + q)) /* Calculate Height          */
                                          /* For Window Wizard        */

            leg1   = round(height * .413, 5)
            leg2   = round(height * .413, 5)

calc_octagon_2:                           /* (AWD003)                 */
            shc(1%) = width               /* Glass Width              */
            shc(2%) = height              /* Glass Height             */
            shc(3%) = leg1                /* Glass Left Leg           */
            shc(4%) = leg2                /* Glass Right leg          */

            sh_fields$    = "WHSZNNN"
            sh_position$  = "1234000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(leg1)
            leg1 = final
            gosub '051(leg2)
            leg2 = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = leg1                     /* DimA    */
            ged_shc(4%) = leg2                     /* DimB    */
            ged_shc(5%) = ((height - leg1) / 2)    /* DimC    */
            ged_shc(6%) = (leg1 + ged_shc(5%))     /* DimD    */
            ged_shc(7%) = ((width - leg2) / 2)     /* DimE    */
            ged_shc(8%) = (leg2 + ged_shc(7%))     /* DimF    */

                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if leg1    <= 0.0 then err% = 3%
            if leg2    <= 0.0 then err% = 4%

        return clear all
        goto exit_program

        calc_right_qtr_round              /* Right Quarter Round   (66)*/
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p + q)
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_right_qtr_1
               width   = width
               height  = height
               radius  = radius

               goto calc_right_qtr_2

calc_right_qtr_1:
            factor1 = width               /* Qtr Width Entered         */

            factor2 = height              /* Qtr Radius entered        */

            radius  = factor2 - pq        /* Glass Arch Radius         */

                                          /* Caluclated Glass width    */
            width  = ( (2*(SQR((2*radius*(radius - pq) ) - ( (radius - pq)**2))))/2.0) - pq

                                          /* Calculated Glass Height   */
            height = ( (2*(SQR((2*radius*(radius - pq) ) - ( (radius - pq)**2))))/2.0) - pq

calc_right_qtr_2:                         /* (AWD003)                  */
            shc(1%) = width               /* Glass Width               */
            shc(3%) = height              /* Glass Height              */
            shc(4%) = radius              /* Glass ARC Radius          */

            sh_fields$    = "WHRNNNN"
            sh_position$  = "1340000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(radius)
            radius = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = radius                   /* DimA    */
            ged_shc(4%) = 0.000                    /* DimB    */
            ged_shc(5%) = 0.000                    /* DimC    */
            ged_shc(6%) = width                    /* DimD    */
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if radius  <= 0.0 then err% = 3%


        return clear all
        goto exit_program

        calc_left_qtr_round               /* Left Quarter Round    (67)*/
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p + q)
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_left_qtr_1
               width   = width
               height  = height
               radius  = radius

               goto calc_left_qtr_2

calc_left_qtr_1:

            factor1 = width               /* Qtr Width Entered         */

            factor2 = height              /* Qtr Radius entered        */

            radius  = factor2 - (p + q)   /* Glass Arch Radius         */

                                          /* Caluclated Glass width    */
            width  = ( (2*(SQR((2*radius*(radius - pq) ) - ( (radius - pq)**2))))/2.0) - pq

                                          /* Calculated Glass Height   */
            height = ( (2*(SQR((2*radius*(radius - pq) ) - ( (radius - pq)**2))))/2.0) - pq

calc_left_qtr_2:                          /* (AWD003)                  */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = height              /* Glass Height              */
            shc(4%) = radius              /* Glass Arch Radius         */

            sh_fields$    = "WHRNNNN"
            sh_position$  = "1240000"


                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(radius)
            radius = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = radius                   /* DimA    */
            ged_shc(4%) = 0.000                    /* DimB    */
            ged_shc(5%) = 0.000                    /* DimC    */
            ged_shc(6%) = width                    /* DimD    */
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if radius  <= 0.0 then err% = 3%


        return clear all
        goto exit_program

        calc_picture                      /* Shape Code (00)           */
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_picture_1
               width   = width
               height  = height

               goto calc_picture_2

calc_picture_1:

            width  = (width  - 2*(p + q)) /* Calculate width           */
            height = (height - 2*(p + q)) /* Calculate Height           */

calc_picture_2:                           /* (AWD003)                  */
            shc(1%) = width               /* Glass width               */
            shc(2%) = height              /* Glass Height              */

            sh_fields$    = "WHNNNNN"
            sh_position$  = "1200000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)
            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            ged_shc(1%) = width
            ged_shc(2%) = height
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%

        return clear all
        goto exit_program

        calc_circle                       /* Shape Code (51)           */
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_circle_1
               width   = width

               goto calc_circle_2

calc_circle_1:

            width  = (width  - 2*(p + q)) /* Calculate width           */

calc_circle_2:                            /* (AWD003)                  */
            shc(1%) = width               /* Glass Width               */

            sh_fields$    = "WNNNNNN"
            sh_position$  = "1000000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            ged_shc(1%) = width
            ged_shc(2%) = width
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%

        return clear all
        goto exit_program

        calc_half_rt_colonial_arch        /* 1/2 Right Colonial Arch(73)*/
                                          /* Same as 1/2 Right Eyebrow  */
                                                           /* (CR2109) */
            if operableshape% = 1% and glsheight > 0 then goto          ~
                                      calc_operable_half_rt_colonial_arch
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

                                          /* (AWD003)                  */
                                          /* (AWD007) No Calc Radius   */
            radius_sav = radius

            if remake_flag% = 0% then goto calc_colonial_right_1

               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg2)**2)

               factor3 = 2*(height - leg2)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */

               factor2 = width

               height  = height

               leg2    = leg2

               factor3 = radius

               if remake_flag% = 2% then factor3 = radius_sav
                                          /* No Calc Radius            */
                                          /* (AWD007)                  */
               goto calc_colonial_right_2

calc_colonial_right_1:

        REM    s1 = ABS((height - leg2))     /* Segment Climb             */

        REM                                  /* Segment Radius            */
        REM    factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                          /* Glass Width               */
            factor2 = (width -(pq + pq) )

                                          /* Glass Height              */
            factor1 = (height -(pq + pq) )

                                          /* (AWD007) Correct Short Leg*/
            leg2 = ABS((height - width) - pq )
                                          /* (AWD007) Correct Arc Radius*/
            factor3 = ABS(factor2 + pq)

                                          /* (AWD007)                  */


calc_colonial_right_2:                    /* (AWD003)                  */
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = factor1             /* Glass Height              */
            shc(3%) = leg2                /* Short Leg                 */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$    = "WHZRNNN"
            sh_position$  = "1235000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor1)
            factor1 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg2)
            leg2 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = factor1
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg2                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg2    <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_half_lf_colonial_arch        /* 1/2 Left Colonial Arch (74)*/
                                          /* Same as 1/2 Left Eyebrow   */
/* (CR2109) */
            if operableshape% = 1% and glsheight > 0 then goto           ~
                                       calc_operable_half_lf_colonial_arch                                            
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */
                                          /* (AWD003)                  */
                                          /* (AWD007) No Calc Radius   */
            radius_sav = radius

            if remake_flag% = 0% then goto calc_colonial_left_1

               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg2)**2)

               factor3 = 2*(height - leg2)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width

               height  = height

               leg1    = leg1

               factor3 = radius

               if remake_flag% = 2% then factor3 = radius_sav
                                          /* No Calc Radius            */
                                          /* (AWD007)                  */
               goto calc_colonial_left_2

calc_colonial_left_1:

        REM    s1 = ABS((height - leg1))     /* Segment Climb             */

        REM                                  /* Segment Radius            */
        REM    factor1 = ( (4 *(s1**2) ) + ( (width * 2)**2) ) / (8*s1)

                                          /* Glass Width               */
            factor2 = (width -(pq + pq) )
                                          /* Glass Height              */
            factor1  = (height -(pq + pq) )
                                          /* (AWD007) Correct Short Leg*/
            leg1 = ABS((height - width) - pq )
                                          /* (AWD007) Correct Arc Radius*/
            factor3 = ABS(factor2 + pq)

                                          /* (AWD007)                  */
                                          /* (RHHTST)                  */

calc_colonial_left_2:                     /* (AWD003)                  */
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = leg1                /* Glass Short Leg           */
            shc(3%) = factor1             /* Glass Height              */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$   = "WSHRNNN"
            sh_position$ = "1235000"


                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor1)
            factor1 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg1)
            leg1 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = factor1
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg1                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg1    <= 0.0 then err% = 4%


        return clear all
        goto exit_program


        calc_right_trapezoid              /* Right Trapezoid       (03)*/
                                          /* (AWD002) Chg to leg2      */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

            pq2 = 2*(p + q)

            deg_radians = 57.29578
            rad_deg     = 0.0174532
                                          /* (AWD003)                  */
                                          /* (AWd005)                  */
            if remake_flag% = 0% then goto calc_trap_right_1
               width   = width
               height  = height
               leg2    = leg2

               goto calc_trap_right_2

calc_trap_right_1:

                                          /* Calculated Climb          */
            factor1 = ABS(height - leg2)

                                          /* Trapezoid diagonal        */
            factor2 = SQR(((factor1)**2) + (width**2))


                                          /* Short Leg Angle           */
            factor3 = (ARCTAN((factor1)/(width))*deg_radians)+90.0

                                          /* Peak Angle                */
            factor4 = (90.0 - (factor3 - 90.0) )

                                          /* Calculated Glass Width    */
            width   = width - (2*pq)

                                          /* Calculated Glass Height   */
            height  = height - ((SQR((((TAN((factor3 - 90.0)*rad_deg))*pq)**2) ~
                            +(pq**2)))+((TAN((factor3 - 90.0)*rad_deg))*pq)+pq)

                                          /* Calaulated Glass Climb    */
            factor5 = (+width)*TAN((factor3 - 90.0)*rad_deg)

                                          /* Calculated Glass short Leg*/
            leg2 = ABS(height - factor5)

calc_trap_right_2:                        /* (AWD003)                  */
                                          /* Adjustment Correction     */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = height              /* Glass Height              */
            shc(3%) = leg2                /* Glass Short Leg  Right    */

            sh_fields$    = "WHZNNNN"
            sh_position$  = "1230000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(leg2)
            leg2 = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = leg2
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if leg2    <= 0.0 then err% = 4%

        return clear all
        goto exit_program

        calc_left_trapezoid               /* Left Trapezoid        (04)*/
                                          /* (AWD002) Chg to leg1      */

            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

            pq2 = 2*(p + q)

            deg_radians = 57.29578
            rad_deg     = .0174532
                                          /* (AWD003)                  */
                                          /* (AWd005)                  */
            if remake_flag% = 0% then goto calc_trap_left_1
               width   = width
               height  = height
               leg1    = leg1

               goto calc_trap_left_2

calc_trap_left_1:
                                          /* Calculated Climb      ok  */
            factor1 = ABS(height - leg1)

                                          /* Trapezoid diagonal    ok  */
            factor2 = SQR(((factor1)**2) + (width**2))

                                          /* Short Leg Angle           */
            factor3 = (ARCTAN((factor1)/(width))*deg_radians)+90.0

                                          /* Peak Angle                */
            factor4 = (90.0 - (factor3 - 90.0) )
                                          /* Calculated Glass Width    */
            width   = width - (2*pq)

                                          /* Calculated Glass Height   */
            height  = height - ((SQR((((TAN((factor3 - 90.0)*rad_deg))*pq)**2) ~
                            +(pq**2)))+((TAN((factor3 - 90.0)*rad_deg))*pq)+pq)



                                          /* Calaulated Glass Climb    */
            factor5 = (+width)*TAN((factor3 - 90.0)*rad_deg)

                                          /* Calculated Glass short Leg*/
           leg1 = ABS(height - factor5)


calc_trap_left_2:                         /* (AWD003)                  */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = leg1                /* Glass Short Leg Left      */
            shc(3%) = height              /* Glass Height              */

            sh_fields$    = "WSHNNNN"
            sh_position$  = "1230000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(leg1)
            leg1 = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = leg1
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if leg1    <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_right_triangle               /* Right Triangle      (05)*/
            gosub read_glass_profile      /* Get Profile and Glazing */
                                          /* Bead Deduction          */

            pq = (p + q)                  /* Adjustment Variable     */


            degs     = 57.29577951
            rad_ians = 0.0174532
                                          /* (AWD003)                */
                                          /* (AWD005)                */
            if remake_flag% = 0% then goto calc_triangle_right_1
               width   = width
               height  = height

               goto calc_triangle_right_2

calc_triangle_right_1:
                                          /* Short Leg Angle          */
            factor1 = (ARCTAN( (height) / (width))* degs)

            factor2 = (90 - (factor1))    /* Peak Angle               */

                                          /* Calc Glass Width         */

            width = width - ( (TAN((90 - factor1) * rad_ians) *pq) +   ~
                   (SQR((pq**2) + (TAN( (factor2) * rad_ians) * pq)**2) ) + pq)

                                          /* Calc Glass Height         */
            height = TAN((factor1) * rad_ians) * width

calc_triangle_right_2:                    /* (AWD003)                  */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = height              /* Glass Height              */


            sh_fields$    = "WHNNNNN"
            sh_position$  = "1200000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = 0.00
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%

        return clear all
        goto exit_program

        calc_left_triangle                /* Right Triangle      (06)*/
            gosub read_glass_profile      /* Get Profile and Glazing */
                                          /* Bead Deduction          */

            pq = (p + q)                  /* Adjustment Variable     */


            degs     = 57.29577951
            rad_ians = 0.0174532
                                          /* (AWD003)                */
                                          /* (AWD005)                */
            if remake_flag% = 0% then goto calc_triangle_left_1
               width   = width
               height  = height

               goto calc_triangle_left_2

calc_triangle_left_1:
                                          /* Short Leg Angle          */
            factor1 = (ARCTAN( (height) / (width))* degs)

            factor2 = (90 - (factor1))    /* Peak Angle               */

                                          /* Calc Glass Width         */

            width = width - ( (TAN((90 - factor1) * rad_ians) *pq) +   ~
                   (SQR((pq**2) + (TAN( (factor2) * rad_ians) * pq)**2) ) + pq)

                                          /* Calc Glass Height        */
            height = TAN((factor1) * rad_ians) * width

calc_triangle_left_2:                     /* (AWD003)                 */
            shc(1%) = width               /* Glass Width              */
            shc(3%) = height              /* Glass Height             */

            sh_fields$    = "WHNNNNN"
            sh_position$  = "1300000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = 0.00
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%

        return clear all
        goto exit_program

        calc_isoc_triangle                /* ISOC Triangle         (07)*/
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

            degs     = 57.29577951
            rad_ians = 0.0174532
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            if remake_flag% = 0% then goto calc_triangle_isoc_1
               width   = width
               height  = height

               goto calc_triangle_isoc_2

calc_triangle_isoc_1:

                                          /* Short Leg Angle           */
            factor1 = (ARCTAN( (height) / ((width/2)))* degs)

        REM    factor2 = (SQR((pq*pq) + (TAN((90 - factor1)* rad_ians) * pq)))

                                          /* Calc Glass Width          */
        REM    width = width - ((TAN((90 - factor1)*rad_ians)*pq) + ((factor2*factor2)+pq)) * 2

            width = width - ( ( TAN( ( 90 - factor1) * rad_ians) * pq) + ~
               (SQR((pq**2) + (TAN((90 - factor1) * rad_ians) * pq)**2))) * 2

                                          /* Calc Glass Height         */
            height = TAN((factor1) * rad_ians) * (width / 2 )

calc_triangle_isoc_2:                     /* (AWD003)                  */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = height              /* Glass Height              */


            sh_fields$    = "WHNNNNN"
            sh_position$  = "1200000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            factor3 = (height/2)
            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor3)
            factor3 = final
            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = factor3
                                            /* (CR503)  -                */

            if width   <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%

        return clear all
        goto exit_program
/*======Operable Shapes + (CR2109)======*/

        calc_operable_colonial_arch       /* Shape Code (63)           */
            gosub '051(glswidth)          /* Series 130 Model FDH      */
            glswidth = final
            
            gosub '051(glsheight)
            glsheight = final         
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            if remake_flag% = 0% then goto calc_operable_colonial_1
               width  = width
               height = height
               leg1   = leg1

               if remake_flag% = 2% then goto calc_operable_colonial_3
                                         /* Don't Calc Radius          */
               goto calc_operable_colonial_2

calc_operable_colonial_1:

            width  = (width  - 2*(p + q)) /* Calculate width           */
            width  = (width + pw)         /* (CR793)                   */
            height = (height - 2*(p + q)) /* Calculate Height          */
            height = (height + ph)        /* (CR793)                   */
            height = height - glsheight - y + z 

calc_operable_colonial_2:
                                          /* Calculate the leg1         */
            leg1  =  (height - (width/2.0))

calc_operable_colonial_3:
                                          /* (AWD003)                  */
                                          /* (AWD005)                  */
            shc(1%) = width               /* Glass Width               */
            shc(2%) = height              /* Glass Height              */
            shc(3%) = leg1                /* Glass Leg                 */

            sh_fields$    = "WHLNNNN"
            sh_position$  = "1230000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(leg1)
            leg1 = final

            ged_shc(1%) = width
            ged_shc(2%) = height
            ged_shc(3%) = leg1                     /* DimA    */
            ged_shc(4%) = (height - leg1)          /* DimB    */
                                            /* (CR503)  -                */

            if width  <= 0.0 then err% = 1%
            if height <= 0.0 then err% = 2%
            if leg1   <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_operable_eyebrow             /* Shape Code (60)  (CR2109)    */
            gosub '051(glswidth)          /* Series 130 Model FAH         */
            glswidth = final
            
            gosub '051(glsheight)
            glsheight = final            
            debug% = 0%
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */
            pq = (p+ q)
                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            if remake_flag% = 0% then goto calc_operable_eyebrow_1
                                          /* (AWD004)                  */
               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg5)**2)

               factor3 = 2*(height - leg5)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width
                                             /* Glass Height           */
               factor3 = height
                                             /* Glass Radius           */
               factor4 = radius
                                             /* Glass Leg              */
               factor6 = leg5
                                             /* (AWD004)               */
               if remake_flag% = 2% then factor4 = leg5
                                             /* (AWD005)               */
               goto calc_operable_eyebrow_2

calc_operable_eyebrow_1:
/* Notes with Chuck 06/14/2019 */
/* s1 = rise height - leg height */
            s1 = ABS((height - leg5))     /* glass rise Segment Climb      */

/* Notes with Chuck 06/14/2019 */

                                          /* Frame Radius Segment Radius   */
            factor1 = ((4*(s1**2)) + (width**2)) / (8*s1)

            radius = factor1

                                          /* Glass Width  - WINIG     */
            factor2 = width - (pq + pq)
                                          /* Glass Height - WINIG     */
            factor3 = height - (pq + pq)
/* (IM8108) */                            /* Glass Radius - WINIG     */
            factor3 = factor3 + ph        /* to remove head profile */

            factor3 = factor3 - glsheight - y + z         /*(CR2109)*/
                                          /* Glass ARC Radius - WINIG  */
            factor4 = radius - pq
                                          /* Glass ARC Rise - glass leg  */
                                          /* WINIG */
            factor5 = factor4 - (0.5*(SQR((4*(factor4**2)) - (factor2**2))))
                                          /* Glass short Leg          */
/* (IM8108) */
            factor6 = height - (pq + pq + factor5)    /* glass without rise */
            factor6 = factor6 + ph
            factor6 = factor6 - y - (glsheight - z)         /*(CR2109)*/

calc_operable_eyebrow_2:                    /* (AWD003)                  */
            shc(1%) = factor2            /* Glass Width               */
            shc(2%) = factor3            /* Glass Height              */

                                         /* Special Label Mod         */
            shc(4%) = factor6            /* Glass Short Leg           */
                                         /* ** for Label Only ***     */
                                         /* Special label Mod         */

            shc(5%) = factor4            /* Glass Arc Radius          */

            sh_fields$    = "WHRLNNN"
            sh_position$  = "1254000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(factor6)
            factor6 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = factor3
            ged_shc(3%) = factor6                  /* DimA    */
            ged_shc(4%) = (factor3 - factor6)      /* DimB    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if factor3 <= 0.0 then err% = 2%
            if factor4 <= 0.0 then err% = 3%
            if factor6 <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_operable_half_right_eyebrow  /* Shape Code (70)           */
            gosub '051(glswidth)          /* Series 130 Model FBH       */
            glswidth = final
            
            gosub '051(glsheight)
            glsheight = final            
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            radius_sav = radius

            if remake_flag% = 0% then goto calc_operable_half_eyebrow_right_1
                                          /* (AWD004)                  */
               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg2)**2)

               factor3 = 2*(height - leg2)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width
                                             /* Glass Height           */
               height  = height
                                             /* Glass Radius           */
               factor3 = radius
                                             /* Glass Leg              */
               leg2    = leg2
                                             /* (AWD004)               */
                                             /* (AWD005)               */
               if remake_flag% = 2% then factor3 = radius_sav
                                             /* No calc radius         */
               goto calc_operable_half_eyebrow_right_2

calc_operable_half_eyebrow_right_1:

            s1 = ABS((height - leg2))     /* rise Segment Climb         */

                                          /* Frame Radis Segment Radius */
                     /* Width * 2 to get overall width or whole eyebrow */
            factor1 = ((4*(s1**2)) + ((width * 2)**2)) / (8*s1)

                                          /* Glass Width               */
            factor2 = width - (pq + pq)

                                          /* Glass Height              */
            height  = height -(pq + pq)
            height  = height + ph
            height  = height - glsheight - y + z  /*(CR2109)*/

                                          /* Glass Arc Radius          */
            factor3 = (factor1 - pq)

                                          /* Glass ARC Rise            */

            factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - (2*pq))**2))))

                                          /* Glass short Leg           */

            leg2    = height - factor4


calc_operable_half_eyebrow_right_2:       /* (AWD003)                  */
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = height              /* Glass Height              */
            shc(3%) = leg2                /* Glass Short Leg           */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$    = "WHZRNNN"
            sh_position$  = "1235000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg2)
            leg2 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = height
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg2                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg2    <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_operable_half_left_eyebrow   /* Shape Code (71)           */
            gosub '051(glswidth)          /* Series 130 Model FCH      */
            glswidth = final
            
            gosub '051(glsheight)
            glsheight = final            
            gosub read_glass_profile      /* Get Profile and Glazing   */
                                          /* Bead Deduction            */

            pq = (p + q)                  /* Adjustment Variable       */

                                          /* (AWD003)                  */
                                          /* (AWD005) No Calc Radius   */
            radius_sav = radius

            if remake_flag% = 0% then goto calc_operable_half_eyebrow_left_1
                                          /* (AWD004)                  */
               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg1)**2)

               factor3 = 2*(height - leg1)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width
                                             /* Glass Height           */
               height  = height
                                             /* Glass Radius           */
               factor3 = radius
                                             /* Glass Leg              */
               leg1    = leg1
                                             /* (AWD004)               */
                                             /* (AWD005)               */
               if remake_flag% = 2% then factor3 = radius_sav
                                             /* No Calc Radius         */
               goto calc_operable_half_eyebrow_left_2

calc_operable_half_eyebrow_left_1:

            s1 = ABS((height - leg1))     /* Segment Climb   - rise     */

                                          /* Segment Radius            */
            factor1 = ((4 *(s1**2)) + ((width * 2)**2)) / (8*s1)

                                          /* Glass Width               */
            factor2 = width - (pq + pq)
                                          /* Glass Height              */
            height  = height -(pq + pq)
            height  = height + ph
            height  = height - glsheight - y + z  /*(CR2109)*/
                                          /* Glass Arc Radius          */
            factor3 = (factor1 - pq)
                                          /* Glass ARC Rise            */

            factor4 = factor3 - (0.5*(SQR((4*(factor3**2)) - (((width*2) - (2*(pq)))**2))))
                                          /* Glass short Leg           */

            leg1    = height - factor4    /* Short Leg */
           

calc_operable_half_eyebrow_left_2:        /* (AWD003)                  */
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = leg1                /* Glass Short Leg           */
            shc(3%) = height              /* Glass Height              */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$    = "WSHRNNN"
            sh_position$  = "1235000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg1)
            leg1 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = height
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg1                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg1    <= 0.0 then err% = 4%


        return clear all
        goto exit_program
        
        calc_operable_half_rt_colonial_arch /* 1/2 Right Colonial Arch(73)*/
                                            /* Same as 1/2 Right Eyebrow  */
            gosub '051(glswidth)            /* Series 130 Model FFH       */
            glswidth = final
            
            gosub '051(glsheight)
            glsheight = final                                                        
            gosub read_glass_profile        /* Get Profile and Glazing   */
                                            /* Bead Deduction            */

            pq = (p + q)                    /* Adjustment Variable       */

                                            
            radius_sav = radius

            if remake_flag% = 0% then goto calc_operable_colonial_right_1

               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg2)**2)

               factor3 = 2*(height - leg2)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */

               factor2 = width

               height  = height

               leg2    = leg2

               factor3 = radius

               if remake_flag% = 2% then factor3 = radius_sav
                                          /* No Calc Radius            */
                                          /* (AWD007)                  */
               goto calc_operable_colonial_right_2

calc_operable_colonial_right_1:

        REM    S1 = ABS((HEIGHT - LEG2))     /* SEGMENT CLIMB             */

        REM                                  /* SEGMENT RADIUS            */
        REM    FACTOR1 = ( (4 *(S1**2) ) + ( (WIDTH * 2)**2) ) / (8*S1)

                                          /* Glass Width               */
            factor2 = (width -(pq + pq) )

                                          /* Glass Height              */
            factor1 = (height -(pq + pq) )
            factor1 = factor1 + ph
            factor1 = factor1 - glsheight - y + z  /*(CR2109)*/            

                                          /* Correct Short Leg (CR2109) */
            leg2 = ABS((height - width) - pq ) - y - glsheight + z + ph
                                          /* Correct Arc Radius*/
REM  FACTOR3 = ABS(FACTOR2 + PQ)                   /*(CR2109)*/
            factor3 = width - ((pq + pq) / 2 )


calc_operable_colonial_right_2:                    
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = factor1             /* Glass Height              */
            shc(3%) = leg2                /* Short Leg                 */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$    = "WHZRNNN"
            sh_position$  = "1235000"

                                            /* (CR503)    +              */
            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor1)
            factor1 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg2)
            leg2 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = factor1
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg2                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */
                                            /* (CR503)  -                */

            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg2    <= 0.0 then err% = 4%


        return clear all
        goto exit_program

        calc_operable_half_lf_colonial_arch /* 1/2 Left Colonial Arch (74)*/
                                            /* Same as 1/2 Left Eyebrow   */
            gosub '051(glswidth)            /* Series 130 Model FFH       */
            glswidth = final
            
            gosub '051(glsheight)
            glsheight = final                                                        
            gosub read_glass_profile        /* Get Profile and Glazing   */
                                            /* Bead Deduction            */

            pq = (p + q)                    /* Adjustment Variable       */

                                            /* No Calc Radius   */
            radius_sav = radius

            if remake_flag% = 0% then goto calc_operable_colonial_left_1

               factor1 = ((width / 2.0)**2)

               factor2 = ((height - leg2)**2)

               factor3 = 2*(height - leg2)
                                             /* Glass Radius           */
               radius = ((factor1 + factor2) / factor3)
                                             /* Glass Width            */
               factor2 = width

               height  = height

               leg1    = leg1

               factor3 = radius

               if remake_flag% = 2% then factor3 = radius_sav
                                          /* No Calc Radius            */

               goto calc_operable_colonial_left_2

calc_operable_colonial_left_1:

        REM    S1 = ABS((HEIGHT - LEG1))     /* SEGMENT CLIMB             */

        REM                                  /* SEGMENT RADIUS            */
        REM    FACTOR1 = ( (4 *(S1**2) ) + ( (WIDTH * 2)**2) ) / (8*S1)

                                          /* Glass Width               */
            factor2 = (width -(pq + pq) )
                                          /* Glass Height              */
            factor1  = (height -(pq + pq) )
            factor1  = factor1 + ph
            factor1  = factor1 - glsheight - y + z  /*(CR2109)*/            
                                          /* Correct Short Leg*/
            leg1 = ABS((height - width) - pq ) - y - glsheight + z + ph
                                          /* Correct Arc Radius*/
REM          FACTOR3 = ABS(FACTOR2 + PQ)  /*(CR2109) */
            factor3 = width - ((pq + pq) / 2 )


calc_operable_colonial_left_2:  
            shc(1%) = factor2             /* Glass Width               */
            shc(2%) = leg1                /* Glass Short Leg           */
            shc(3%) = factor1             /* Glass Height              */
            shc(5%) = factor3             /* Glass ARC Radius          */

            sh_fields$   = "WSHRNNN"
            sh_position$ = "1235000"



            init(" ") table$, genkey$, ged_shape$, ged_fields$
            mat ged_shc = zer
            table$ = "SHAPEGED"
            genkey$ = sh_cross$
            gosub genRead
            ged_shape$  = str(descr1$,1%,6%)
            ged_fields$ = str(descr1$,15%,10%)

            gosub '051(width)
            width = final
            gosub '051(height)
            height = final
            gosub '051(factor2)
            factor2 = final
            gosub '051(factor1)
            factor1 = final
            gosub '051(factor3)
            factor3 = final
            gosub '051(leg1)
            leg1 = final

            ged_shc(1%) = factor2
            ged_shc(2%) = factor1
            ged_shc(3%) = factor3                  /* DimA    */
            ged_shc(4%) = leg1                     /* DimB    */
            ged_shc(5%) = sixth                    /* DimC    */
            ged_shc(6%) = (factor2 - sixth)        /* DimD    */


            if factor2 <= 0.0 then err% = 1%
            if height  <= 0.0 then err% = 2%
            if factor3 <= 0.0 then err% = 3%
            if leg1    <= 0.0 then err% = 4%


        return clear all
        goto exit_program

      
/* - (CR2109) */
      REM *************************************************************~
          *           S P E C I A L   S U B R O U T I N E S           *~
          *                                                           *~
          *************************************************************

        read_glass_profile
           p = 0.0 : q = 0.0 : pw = 0.0 : ph = 0.0 : y = 0.00 : z = 0.00

           init(" ") readkey$
           str(readkey$,1%,9%)   = "SHPHFOFF "
           str(readkey$,10%,15%) = sh_model$       /* Model        */

                                    /* Load Equation for Field Calc.   */
           read #1,key = readkey$, using L60000, descr$, descr1$,          ~
                                                    eod goto no_profile
L60000:           FMT POS(25), CH(30), XX(02), CH(30)

/* P = the frame thickness, the distance from the edge of the frame to the glazing pocket (or sill) */
/* Q = edge of the glazing pocket to the edge of the glass or an offset - clearance between glass and frame */
/* PW = additional width offset on alumn shapes bc alum sill is different stacking, high performance*/
/* PH = additional height offset on alumn shapes bc alum sill is different stacking, high performance*/
/* Y = Offset between top and bottom glass - meeting rail (CR2109) */
/* Z = PW Offset - SH Offset from sill to bottom glass edge (CR2109) */
/* (IM8010) */
REM               CONVERT STR(DESCR$,1%,6%)  TO P, DATA GOTO NO_PROFILE

REM               CONVERT STR(DESCR$,24%,6%) TO Q, DATA GOTO NO_PROFILE

           convert str(descr$,1%,6%) to p, data goto no_profile

           convert str(descr$,8%,6%) to q, data goto no_profile

           convert str(descr$,15%,7%) to pw, data goto no_extra_width
no_extra_width:

           convert str(descr$,23%,7%) to ph, data goto no_extra_height

no_extra_height:
/* (\IM8010) */
/*+(CR2109)*/
           convert str(descr1$,2%,7%) to y, data goto noMeetingRail
noMeetingRail:

           convert str(descr1$,10%,7%) to z, data goto noSillOffset

noSillOffset:
/*-(CR2109)*/
        return

        no_profile
           err% = 5%
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

        genRead
            init(" ") codeLen$, descr1$, descr2$, descr3$
            generr% = 0%
            call "GENREAD" (table$, genkey$, descr1$, codeLen$,       ~
                            descr2$, descr3$, generr%)

            if generr% <> 0% then goto genReadErr
        return
        genReadErr
           descr1$ = " ?-ERR"
        return

        exit_program

           gosub debug_display_screen

        end

       deffn'051(final)

/*       roundToSixteenth */
/* divide decimal or entire umber by 16th (0.0625) - tells how many 16th */
/* round up to nearest whole number                                      */
/* multiply rounded whole number by 0.0625                               */
         ged   = 0.00
         whl   = 0.00
         dec   = 0.00
         fctnl = 0.00


         if final <= 0.00 then return
            ged   = final
            fctnl = (ged / sixth)
            dec   = (fctnl - int(fctnl))         /*dec=0;already 16th*/
            if dec <= 0.00 then return
               whl = (int(fctnl) + 1)             /* round up */
               ged = (whl * sixth)
         final = ged
       return

       debug_display_screen
            if debug% = 0% then return             /* Debug Turned Off */

            init(" ") sh$(), shc$(), p$, q$, temp$(6%), pf$(), date$, temp$()

            header$ = "AWD S.S. 'Debug' Shape Code (XX) Barcode = " &dt_bar$

            convert shape% to str(header$,30%,2%), pic(00)


            mat temp = zer

            inpmessage$ = "Press <Return> to Continue?"

            date$ = date
            call "DATEFMT" (date$)

            convert p to p$, pic(######.####-)

            convert q to q$, pic(######.####-)
                                                     /* Temp Data Area     */

            temp(1%) = factor1
            temp(2%) = factor2
            temp(3%) = factor3
            temp(4%) = factor4
            temp(5%) = factor5
            temp(6%) = factor6

                                                     /* Temp Data Area    */

            for kk% = 1% to 6%

                convert sh(kk%) to sh$(kk%), pic(######.####-)

                convert shc(kk%) to shc$(kk%), pic(######.####-)

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
               at (04,21), fac(hex(84)), shc$(1%)               , ch(12),~
                                                                         ~
               at (05,03), fac(hex(84)), sh$(2%)                , ch(12),~
               at (05,21), fac(hex(84)), shc$(2%)               , ch(12),~
                                                                         ~
               at (06,03), fac(hex(84)), sh$(3%)                , ch(12),~
               at (06,21), fac(hex(84)), shc$(3%)               , ch(12),~
                                                                         ~
               at (07,03), fac(hex(84)), sh$(4%)                , ch(12),~
               at (07,21), fac(hex(84)), shc$(4%)               , ch(12),~
                                                                         ~
               at (08,03), fac(hex(84)), sh$(5%)                , ch(12),~
               at (08,21), fac(hex(84)), shc$(5%)               , ch(12),~
                                                                         ~
               at (09,03), fac(hex(84)), sh$(6%)                , ch(12),~
               at (09,21), fac(hex(84)), shc$(6%)               , ch(12),~
                                                                         ~
               at (11,02), "Label Field Flags         :",                ~
               at (11,30), fac(hex(84)), sh_fields$             , ch(07),~
                                                                         ~
               at (12,02), "Location of Field Data    :",                ~
               at (12,30), fac(hex(84)), sh_position$           , ch(07),~
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
               at (19,02), "Temp Value (6)            :",                ~
               at (19,30), fac(hex(84)), temp$(6%)              , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

               stop                                    /* (RHHTEST)   */

               if keyhit% = 16% then goto debug_display_screen

            return

        set_keys
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
        return



