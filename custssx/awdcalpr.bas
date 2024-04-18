        REM *************************************************************~
            *                                                           *~
            *   AAA   W   W  DDDD    CCC    AAA   L      PPPP   RRRR    *~
            *  A   A  W   W  D   D  C   C  A   A  L      P   P  R   R   *~
            *  AAAAA  W   W  D   D  C      AAAAA  L      PPPP   RRRR    *~
            *  A   A  W W W  D   D  C   C  A   A  L      P      R R     *~
            *  A   A   W W   DDDD    CCC   A   A  LLLLL  P      R  R    *~
            *                  ( SUBROUTINE )                           *~
            *-----------------------------------------------------------*~
            * AWDCALPR - Calculate the CUSTOM Price for Glass and Grid. *~
            *            and Update Custom Glass Database               *~
            *                                                           *~
            *                                                           *~
            *     - scr_sel$  - Screen Selection                 ( In ) *~
            *                                                           *~
            *     - ct_rec$   - Custom Database Record           ( In ) *~ 
            *                                                           *~
            *     - ct_price$ - Calculated Glass Price           (Out ) *~
            *                                                           *~
            *                                                           *~   
            *     - #1       - Channel for (GENCODES) File       ( In ) *~
            *     - #2       - Channel for (AWDGLSCT) File       ( In ) *~ 
            *                                                           *~
            *     - ERR%     - Error Code                        ( Out) *~
            *                  (0%) All Ok                              *~
            *                  (1%) Pricing Error                       *~
            *                                                           *~
            *     - Key Tables 'CUSTGLASS' - Glass Codes each have upto *~
            *                                'NNNNNN' Six Flags         *~
            *                                (1,1) = Clear Glass (Y/N)  *~
            *                                (2,1) = OBS Glass   (Y/N)  *~
            *                                (3,1) = Low E Glass (Y/N)  *~
            *                                (4,1) = Argon Gas   (Y/N)  *~
            *                                (5,1) = Tempered Gls(Y/N)  *~
            *                                (6,1) = Growth             *~
            *                                                           *~
            *                  'CUSTLITES' = Grid Codes with value for  *~
            *                                Number of Lites.           *~
            *                                                           *~
            *                  'CUSTEXCEP' = Custom Glass Clear Glass   *~
            *                                Exceptions. Deduct for     *~
            *                                Clear Glass.               *~
            *                                                           *~     
            *                                                           *~   
            *    Notes - Special Pricing Rules                          *~
            *                                                           *~
            *            Width and Height - Rounded up to the nearest   *~
            *                  Even Inch.                               *~
            *            Square Foot - Round to the nearest Hundreth    *~
            *                  of a Foot.                               *~
            *            Price Up Charge - Rounded to the nearest Penney*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/03/04 ! New Subroutine for (APC) - LAST MOD DATE ! RHH *~
            * 11/08/04 ! (AWD001) Final Price Changes to agree    ! RHH *~
            *          !          with Custom Glass's Pricing.    !     *~
            * 12/15/04 ! (AWD002) Mod to Calulate the Custom Price! RHH *~
            *          !        for report and warranty (AWDRPT03)!     *~
            * 04/04/05 ! (AWD003) Mod for three new grid codes    ! RHH *~
            *          !        HE, HF, HG                        !     *~
            * 09/02/05 ! (AWD004) Mod to fix some problems with   ! RHH *~
            *          !        Grid Only Pricing for certain     !     *~
            *          !        Shapes.                           !     *~
            * 01/01/06 ! (PAR000) CR347 Mod for new sub Part No.  ! RHH *~ 
            * 03/24/08 ! (AWD005) mods for sdl pricing            ! CMG *~    
            * 07/15/08 ! (AWD006) mods for Custom 6% price increas! CMG *~
            *************************************************************

        sub "AWDCALPR"   (been_here%,    /* Initialize Arrays          */~
                          scr_sel$,      /* Screen Selection           */~
                          ct_rec$,       /* Custom Data Record         */~
                          ct_price$,     /* Custom Glass Price         */~  
                          #1,            /* GENCODES File              */~
                          #2,            /* AWDGLSCT Custom Database   */~
                          #3,            /* APCPLNDT Planning Database */~
                          err% )         /* Error Code 0 = Ok, 0 <> err*/

        dim                                                              ~
            readkey$30,                  /* GENCODES Look-Up Key       */~
            descr$30,                    /* Use for GENCODES Look-Up   */~
            scr_sel$1,                   /* Screen Selection           */~
            ct_key1$12,                  /* Alt Key 1                  */~
            ct_rec$256,                  /* Custom Record              */~
            ct_price$10,                 /* Custom Glass Price         */~
            ct_wind_prc$10,              /* Window Price               */~
            ct_part$25,                  /* MFG Part Number            */~
            ct_cross$2,                  /* 1st of Grid + 2nd of Hinge */~
            ct_pct$8,                    /* Glass Price Percent of Sale*/~
            ct_grid$2,                   /* Glass Grid Code            */~
            ct_glass$2,                  /* Glass Type Code            */~
            thick$1,                     /* Glass Thickness            */~ 
            ct_grid_size$1,              /* Glass Grid Size            */~
            glass_only$1,                /* Glass Only                 */~
            grid_only$1,                 /* Grid Only (Y)es or (N)o    */~
            ct_gl_type$6,                /* Glass Type Flags - XXXXXX  */~
            ct_gl_lites$2,               /* Grid No. of Lites          */~
            calc_glass_sq_foot$1,        /* Glass Only Calc Flag       */~
            ct_wood$1,                   /* Wood Surround (Y/N)        */~
            ct_descr$10,                 /* Calc Process Description   */~
            ct_special$10,               /* Special Product Flags      */~        
            contour$1,                   /* Contour Grid Y or N        */~
            gothic$1,                    /* Gothic Grid Y or N         */~
            spoke$1,                     /* Spoke thru Hub Y or N      */~
            hub$2,                       /* Number of Hubs - Half Round*/~ 
            dt_key4$8,                   /* Planning Alt key 4         */~
            ct_sq_foot$8,                /* Window Sq Foot Rounded up  */~
            width_f$9, height_f$9,       /* Actual Width and Height    */~
            radius_f$9, leg1_f$9,        /* Actual Radius and Short Leg*/~
            width$5, height$5,           /* window Width and Height    */~
            radius$5, leg1$5,            /* window Radius and Short leg*/~ 
            ct_price_type$2              /* SDL Price Type (AWD005)    */

            
        dim                                                              ~
            header$75,                  /* Debug Header                */~
            date$8,                     /* todays Date                 */~
            cursor%(2%),                /* Cursor Location for Edit    */~
            i$(24%)80,                  /* Screen Image                */~
            inpmessage$79,              /* Informational Message       */~
            sz$100,                     /* Fractions for 16th of inch  */~
            pf$(3%)79,                  /* PF Screen Literals          */~
            pfkeys$32                   /* PF Key Hex Values           */

        dim                             /* Pricing Dimensions          */~
            no_g$(25%)2,                /* No Grid Codes               */~
            eye$(25%)6,                 /* Grid Code Definitions       */~
            sb(5%,3%),                  /* Half Round SunBurst Pricing */~
            gt(5%,3%),                  /* Half Round Gothic Pricing   */~
            so(5%,3%),                  /* Half Round SunBurst Grid only*/~
            got(5%,3%),                 /* Half Round Gothic Grid Only */~
            gsw(4%),                    /* Glass only Width Dimensions */~
            gsh(4%),                    /* Glass Only Height Dimensions*/~
            gsp(4%,4%),                 /* Glass Width and Height Price*/~
            qtr(4%,3%)                  /* Quarter Round Glass & Grid  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "01.00.00 01/01/06 Calculate Custom Glass/Grid Prc"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master System Table File                 *~
            * #2  ! AWDGLSCT ! Custom Maqster Database                  *~
            * #3  ! APCPLNDT ! Planning Master Data File                *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************
                                             /* Only Custom Glass       */
                                             /* (AWD002)                */
                                             /* 2 = Shape Glass Dept=43 */
                                             /* 3 = Special Shape Glass */
                                             /* 9 = Report (AWDRPT03)   */
            if scr_sel$ <> "2" and scr_sel$ <> "5" and scr_sel$ <> "9"   ~
                                                  then goto BEGIN_PRICING

            if been_here% <> 0% then goto BEGIN_PRICING
               been_here% = 1%
                                               /* No Grid Codes         */
            no_g$(1%) = "00"  :  no_g$(10%)= "M0"
            no_g$(2%) = "A0"  :  no_g$(11%)= "N0"
            no_g$(3%) = "C0"  :  no_g$(12%)= "K0"
            no_g$(4%) = "D0"  :  no_g$(13%)= "L0"
            no_g$(5%) = "E0"  :  no_g$(14%)= "P0"
            no_g$(6%) = "F0"  :  no_g$(15%)= "T0"
            no_g$(7%) = "H0"  :  no_g$(16%)= "U0"
            no_g$(8%) = "I0"  :  no_g$(17%)= "R0"
            no_g$(9%) = "J0"  :  no_g$(18%)= "S0"

            no_g_max% = 18%
                                              /* Initialize Values      */
                                              /* Half Round, Elliptical */
                                              /* (1,2) = Grid Code      */
                                              /* (4,1) = No. of Hubs    */
            eye_max% = 0%                     /*         '*' No Grid    */
                                              /* (6,1) = Spokes thru Hub*/ 
            eye$(1%) = "C0 * N"               /* No Grid          Ellip */
            eye$(2%) = "C1 0 N"               /* Sunburst No Hub  Ellip */
            eye$(3%) = "C2 1 N"               /* Sunburst 1 Hub   Ellip */
            eye$(4%) = "C3 2 N"               /* Sunburst 2 Hub   Ellip */

            eye$(5%) = "H0 * N"               /* No Grid          Half  */
            eye$(6%) = "H1 0 N"               /* Sunburst         Half  */
            eye$(7%) = "H2 1 N"               /* Sunburst 1 Hub   Half  */
            eye$(8%) = "H3 1 Y"               /* Sunburst 1 Hub SpHalf  */
            eye$(9%) = "H4 2 N"               /* Sunburst 2 Hub   Half  */
            eye$(10%)= "H5 2 Y"               /* Sunburst 2 Hub SpHalf  */
            eye$(11%)= "H6 3 N"               /* Sunburst 3 Hub   Half  */
            eye$(12%)= "H7 3 Y"               /* Sunburst 3 Hub SpHalf  */
            eye$(13%)= "HC 3 N"               /* Sunburst 3 Hub   Half  */
            eye$(14%)= "HD 3 Y"               /* Sunburst 3 Hub SpHalf  */

            eye$(15%)= "H6 3 N"               /* Gothic   3 Lit   Half  */                        
            eye$(16%)= "H7 5 N"               /* Gothic   5 Lit   Half  */                        
            eye$(17%)= "H8 9 N"               /* Gothic   9 Lit   Half  */                        
            eye$(18%)= "H9 13N"               /* Gothic  13 lit   Half  */                        
            eye$(19%)= "HA 20N"               /* Gothic  20 Lit   Half  */                        
            eye$(20%)= "HB 36N"               /* Gothic  36 Lit   Half  */
                                              /* (AWD003)               */
            eye$(21%)= "HE 0 N"               /* Sunburst         Half  */
            eye$(22%)= "HF 2 Y"               /* Sunburst 2 Hub SpHalf  */
            eye$(23%)= "HG 2 N"               /* Sunburst 2 Hub   Half  */  

            eye_max% = 23%                    /* Max Active Codes       */
                                              /* (AWD003)               */

                                              /* (AWD004)               */ 
                                              /* Prices Half Round and  */
                                              /* Elliptical Shapes      */
            /*          <=       5/8, 3/4 Inch       1 Inch             */
            /*          Width   Colonal             Contour  Sunburst   */
            sb(1%,1%)  = 36.0 : sb(1%,2%) = 14.07 : sb(1%,3%) = 18.87      
            sb(2%,1%)  = 48.0 : sb(2%,2%) = 25.16 : sb(2%,3%) = 31.30
            sb(3%,1%)  = 60.0 : sb(3%,2%) = 41.85 : sb(3%,3%) = 49.57
            sb(4%,1%)  = 80.0 : sb(4%,2%) = 56.76 : sb(4%,3%) = 72.73
            sb(5%,1%)  = 99.0 : sb(5%,2%) = 56.76 : sb(5%,3%) = 72.73

            /*          <=       5/8, 3/4 Inch       1 Inch             */
            /*          Width   Colonal             Contour  Gothic     */
            gt(1%,1%)  = 36.0 : gt(1%,2%) = 45.18 : gt(1%,3%) = 60.93      
            gt(2%,1%)  = 60.0 : gt(2%,2%) = 80.78 : gt(2%,3%) = 99.38
            gt(3%,1%)  = 80.0 : gt(3%,2%) =112.37 : gt(3%,3%) =140.92
            gt(4%,1%)  = 99.0 : gt(4%,2%) =112.37 : gt(4%,3%) =140.92


            /* Add $3.06 Spokes thru Hub                                */

            /* Add $6.12 Per Hub for Two (2) Hubs or More               */ 
 
                                              /* Prices Half Round and  */
                                              /* Elliptical Shapes      */
                                              /* No Glass (Grid Only)   */
            /*          <=       5/8, 3/4 Inch       1 Inch             */
            /*          Width   Colonal             Contour  Sunburst   */
            so(1%,1%)  = 36.0 : so(1%,2%) =  7.67 : so(1%,3%) = 12.46      
            so(2%,1%)  = 48.0 : so(2%,2%) =  8.86 : so(2%,3%) = 15.00
            so(3%,1%)  = 60.0 : so(3%,2%) = 10.93 : so(3%,3%) = 18.65
            so(4%,1%)  = 80.0 : so(4%,2%) = 13.18 : so(4%,3%) = 23.15
            so(5%,1%)  = 99.0 : so(5%,2%) = 13.18 : so(5%,3%) = 23.15
                                              /* No Glass (Grid Only)   */
            /*          <=       5/8, 3/4 Inch       1 Inch             */
            /*          Width   Colonal             Contour  Gothic     */
            got(1%,1%)  = 36.0 : got(1%,2%) = 23.18 : got(1%,3%) = 32.35      
            got(2%,1%)  = 60.0 : got(2%,2%) = 46.25 : got(2%,3%) = 56.73
            got(3%,1%)  = 80.0 : got(3%,2%) = 68.54 : got(3%,3%) = 84.33
            got(4%,1%)  = 99.0 : got(4%,2%) = 68.54 : got(4%,3%) = 84.33

                                              /* Glass Only All Shapes  */
                                              /* Except Half Round and  */
                                              /* 1/4 Quarter Round      */  
            /* Glass Width () thru (4)        Glass Height (1) thru (4) */
            gsw(1%) = 32.0            :       gsh(1%) = 32.0
            gsw(2%) = 48.0            :       gsh(2%) = 48.0
            gsw(3%) = 60.0            :       gsh(3%) = 60.0
            gsw(4%) = 80.0            :       gsh(4%) = 80.0
 
            /* Width (1) - Height (1) thru (4) */ 
            /* price = gsp( gsw [Subscript], gsh [Subscript] ) */

            gsp(1%,1%) = 90.91 : gsp(1%,2%) = 119.59 : gsp(1%,3%) = 132.41 : gsp(1%,4%) = 161.39

            /* Width (2) - Height (1) thru (4) */
            gsp(2%,1%) =119.59 : gsp(2%,2%) = 143.43 : gsp(2%,3%) = 163.09 : gsp(2%,4%) = 196.54

            /* Width (3) - Height (1) thru (4) */
            gsp(3%,1%) =132.41 : gsp(3%,2%) = 163.09 : gsp(3%,3%) = 188.16 : gsp(3%,4%) = 239.36

            /* Width (4) - Height (1) thru (4) */
            gsp(4%,1%) =161.39 : gsp(4%,2%) = 196.54 : gsp(4%,3%) = 239.36 : gsp(4%,4%) = 295.69

                                              /* 1/4 Round Glass and Grid*/
            /*          <=       5/8, 3/4 Inch       1 Inch              */
            /*          Width   Colonal             Contour              */
            qtr(1%,1%)  = 28.0 : qtr(1%,2%) = 65.43 : qtr(1%,3%) = 76.20      
            qtr(2%,1%)  = 48.0 : qtr(2%,2%) = 84.58 : qtr(2%,3%) = 97.28
            qtr(3%,1%)  = 80.0 : qtr(3%,2%) =123.15 : qtr(3%,3%) =148.87
            qtr(4%,1%)  = 99.0 : qtr(4%,2%) =123.15 : qtr(4%,3%) =148.87
                                              /* (AWD004)                */        

        REM - NEAREST 16TH OF AN INCH
        REM   sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        REM~ 3/4 13/16 7/8 15/16     "

           sz$ = ".0625.1250.1875.2500.3125.3750.4375.5000.5625.6250.6875~
        ~.7500.8125.8750.9375     "

        REM *************************************************************~
            *            B E G I N   C A L C U L A T I O N S            *~
            *                                                           *~
            *************************************************************

BEGIN_PRICING:
        debug% = 0%                             /* Debug Display Flag  */ 

        u3%      = 0%
        ct_price = 0.0                          /* Init Price          */
        ct_clear = 0.0                          /* Glass Only Clear    */
        ct_obs   = 0.0                          /* Glass Only Obscure  */
        ct_lowe  = 0.0                          /* Glass Only Lowe     */

        calc_glass_sq_foot$ = "N"               /* Init Glass Flag     */       
        convert ct_price to ct_price$, pic(####.####-)


        price% = 0%
        if scr_sel$ <> "2" and scr_sel$ <> "5" and scr_sel$ <> "9"      ~
           and scr_sel$ <> "0" then  goto exit_program

                                                     /* Get Shape Code */       
        convert str(ct_rec$,41%,3%) to shape%, data goto BEGIN_ERR

        gosub convert_dimensions                     /* Get Values     */

        gosub debug_display_screen              /* Display Before Calc */
/* (AWD005) begin */
           if ct_grid_size$ <> "6" then goto find_shape
                  gosub calc_sdl
                  return clear all
                  goto exit_program
/* (AWD005 end */

find_shape:

        if shape% =  0% then gosub calc_picture              
        if shape% = 01% then gosub calc_right_irreg_pentagon
        if shape% = 02% then gosub calc_left_irreg_pentagon
        if shape% = 03% then gosub calc_right_trapezoid       
        if shape% = 04% then gosub calc_left_trapezoid       
        if shape% = 05% then gosub calc_right_triangle       
        if shape% = 06% then gosub calc_left_triangle        
        if shape% = 07% then gosub calc_isoc_triangle        
        if shape% = 15% then gosub calc_doghouse_pentagon
        if shape% = 25% then gosub calc_octagon               
        if shape% = 51% then gosub calc_circle               
        if shape% = 60% then gosub calc_eyebrow              
        if shape% = 63% then gosub calc_colonial_arch        
        if shape% = 64% then gosub calc_half_round       /* In Process */          
        if shape% = 66% then gosub calc_right_qtr_round      
        if shape% = 67% then gosub calc_left_qtr_round       
        if shape% = 70% then gosub calc_half_right_eyebrow   
        if shape% = 71% then gosub calc_half_left_eyebrow    
        if shape% = 73% then gosub calc_half_rt_colonial_arch 
        if shape% = 74% then gosub calc_half_lf_colonial_arch

                                          /* No Equation Defined       */
BEGIN_ERR:
        err% = 6%
        return clear all
        goto exit_program

        calc_picture                      /* Shape Code (00)           */
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)

            price% = 1%
        return clear all
        goto exit_program

        calc_right_irreg_pentagon         /* Shape Code (01)           */
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)

            price% = 1%
        return clear all
        goto exit_program

        calc_left_irreg_pentagon          /* Shape Code (02)           */
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)

            price% = 1%
        return clear all
        goto exit_program

        calc_right_trapezoid              /* Right Trapezoid       (03)*/

                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)


            price% = 1%
        return clear all
        goto exit_program

        calc_left_trapezoid               /* Left Trapezoid        (04)*/

                                          /* Glass Only No Grid        */ 
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot 
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)


            price% = 1%
        return clear all
        goto exit_program

        calc_right_triangle               /* Right Triangle        (05)*/
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)

            
            price% = 1%
        return clear all
        goto exit_program

        calc_left_triangle                /* Right Triangle        (06)*/
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)


            price% = 1%
        return clear all
        goto exit_program

        calc_isoc_triangle                /* ISOC Triangle         (07)*/
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)


            price% = 1%
        return clear all
        goto exit_program

        calc_doghouse_pentagon            /* Shape Code (15)           */
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
                                          /* ?? Glass and Grid         */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_sq_foot

                                          /* Glass/Grid Lites Upcharge */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                              ct_price = ct_price + (ct_gl_lites% * 1.50)

                                          /* ?? Price of Grid only     */ 
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                             ct_price = (ct_gl_lites% * 1.50)

            price% = 1%
        return clear all
        goto exit_program

        calc_octagon                      /* Shape Code (25)           */
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only
            price% = 1%
        return clear all
        goto exit_program

        calc_circle                       /* Shape Code (51)           */
                                          /* Price Glass Only          */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only
            price% = 1%
        return clear all
        goto exit_program

        calc_eyebrow                      /* Shape Code (60)           */

                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */  
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only
            price% = 1%
        return clear all
        goto exit_program

        calc_colonial_arch                /* Shape Code (63)          */
                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only
            price% = 1%
        return clear all
        goto exit_program

        calc_half_round                   /* Shape Code (64)           */
                                          /* (AWD004)                  */
                                          /* Check for Elliptical      */
                                          /* (AWD001) Price is Correct */
            if str(ct_grid$,1%,1%) = "C" then goto calc_elliptical
                                          /* Sunburst Grid Only        */                                            
            if grid_only$ = "Y" and gothic$ = "N" and ct_grid$ <> "H0" then ~
                                               gosub calc_half_grid_only
                                          /* Gothic Grid Only          */
            if grid_only$ = "Y" and gothic$ = "Y" and ct_grid$ <> "H0" then ~
                                               gosub calc_half_grid_gothic

                                          /* (AWD004)                  */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot
            
            if grid_only$ = "N" and glass_only$ = "N" and gothic$ = "N" ~
                                          then gosub calc_half_sunburst

            if grid_only$ = "N" and glass_only$ = "N" and gothic$ = "Y" ~
                                          then gosub calc_half_gothic

            if glass_only$ = "N" and grid_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" and grid_only$ = "N" then gosub glass_argon_upcharge

            price% = 1%
        return clear all
        goto exit_program

        calc_elliptical                   /* Shape Code (64)           */
                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only
            price% = 1%
        return clear all
        goto exit_program

        calc_right_qtr_round              /* Right Quarter Round   (66)*/
                                          /* Price Glass Only          */
                                          /* (AWD001) Price is Correct */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Price Glass and Grid      */  
            if grid_only$ = "N" and glass_only$ = "N" then               ~              
                                          gosub calc_quarter_round

                                          /* For Glass Other Than Clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge

                                          /* ?? Price of Grid only     */ 

            price% = 1%
        return clear all
        goto exit_program

        calc_left_qtr_round               /* Left Quarter Round    (67)*/
                                          /* Price Glass Only          */
                                          /* (AWD001) Price is Correct */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Price Glass and Grid      */  
            if grid_only$ = "N" and glass_only$ = "N" then               ~              
                                          gosub calc_quarter_round

                                          /* For Glass Other Than Clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge

                                          /* ?? Price of Grid only     */ 
         
            price% = 1%
        return clear all
        goto exit_program

        calc_half_right_eyebrow           /* Shape Code (70)           */
                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only

            price% = 1%
        return clear all
        goto exit_program

	calc_half_left_eyebrow            /* Shape Code (71)           */
                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only

            price% = 1%
        return clear all
        goto exit_program

        calc_half_rt_colonial_arch        /* 1/2 Right Colonial Arch(73)*/
                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only
            price% = 1%
        return clear all
        goto exit_program
 
        calc_half_lf_colonial_arch        /* 1/2 Left Colonial Arch (74)*/
                                          /* Glass Only No Grid        */
                                          /* (AWD001) Price Correct    */
            if glass_only$ = "Y" and grid_only$ = "N" then gosub calc_glass_sq_foot

                                          /* Glass and Grid            */  
            if grid_only$ = "N" and glass_only$ = "N" then              ~
                                          gosub calc_glass_only

                                         /* Glass Grid & Contour      */
                                         /* After Glass Upcharge      */ 
            if grid_only$ = "N" and glass_only$ = "N" and contour$ = "Y"~
                             then ct_price = ct_price + (ct_price * .15)

                                           /* For Glass other than clear*/
            if glass_only$ = "N" then gosub glass_type_upcharge

                                          /* Glass Argon Upcharge      */
            if glass_only$ = "N" then gosub glass_argon_upcharge
 
                                          /* Grid Only & No Glass      */
            if grid_only$ = "Y" and glass_only$ = "N" then              ~
                                          gosub calc_grid_only

            price% = 1%
        return clear all
        goto exit_program

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *                                                           *~
            *************************************************************

        update_custom_data
            put str(ct_rec$,127%,8%), using UPDATE_1, ct_price
UPDATE_1:        FMT PD(14,4)

                                                       /* Glass Exception */
            ct_obs    = 0.0
            ct_lowe   = 0.0
            gosub lookup_glass_exceptions
            
            if cust_except% = 0% then ct_clear = 0.0   /* Otherwise don't Clear   */
            if cust_except% = 0% then goto UPDATE_1A
                                                       /* Deduct Clear from Price */


UPDATE_1A:
                                                        /* (AWD002)       */
                                                        /* Clear Glass Only*/ 
            put str(ct_rec$,220%,8%), using UPDATE_1, ct_clear
                                                        /* OBS Glass Only  */ 
            put str(ct_rec$,228%,8%), using UPDATE_1, ct_obs
                                                        /* Lowe Glass Only */
            put str(ct_rec$,236%,8%), using UPDATE_1, ct_lowe 

REM            if scr_sel$ = "9" then return               /* (No Update    )*/
                                                        /* (AWD002)       */

            ct_pct =  (ct_price / ct_wind_prc ) * 100.0

            put str(ct_rec$,138%,8%), using UPDATE_1, ct_wind_prc

            put str(ct_rec$,146%,8%), using UPDATE_1, ct_pct

            ct_key1$ = all(hex(00))
            ct_key1$ = str(ct_rec$,12%,12%)
            read #2,hold,key 1% = ct_key1$, using UPDATE_2, ct_rec$,         ~
                                                         eod goto UPDATE_3
UPDATE_2:      FMT CH(256)

            delete #2
UPDATE_3:
            write #2, using UPDATE_2, ct_rec$, eod goto UPDATE_4

        return
UPDATE_4:
             err% = 1% 
        return
 
        exit_program

            ct_price = round(ct_price, 2)

/* (AWD006) mod for 6% custom price increase.   do not add to 6 or sdl pricing */

            if ct_grid_size$ <> "6" and ct_price > 0.00 then     ~           
                  ct_price = round(ct_price * 1.06,2)
            
            convert ct_price to ct_price$, pic(###.####-)

            gosub debug_display_screen /* Display After Price Calculated */

            if price% = 1% and scr_sel$ <> "0" then gosub update_custom_data  

        end

        REM *************************************************************~
            *           C a l c   P a r a m e t e r s                   *~
            *                                                           *~
            *  width   - Window Width Size to Nearest Even Inch.        *~
            *  height  - Window Height Size to Nearest Even Inch.       *~
            *  radius  - Window Radius Size to Nearest Even Inch.       *~
            *  leg1    - Window Short Leg Size to Nearest Even Inch.    *~
            *                                                           *~
            *  ct_grid$    - Window Grid Code                           *~
            *                                                           *~
            *  ct_glass$   - Window Glass Code                          *~
            *                                                           *~
            *  thick$      - Glass Thickness 1%=3/4, 2%=13/16, 3%=5/8   *~
            *                                                           *~
            *  ct_grid_size$ - 1=1, 2=3/4, 3=5/8  (0 = No Grid)         *~
            *                                                           *~
            *  grid_only$  - Price Grid Only Flag (Not Glass)           *~
            *                                                           *~
            *  glass_only$ - No Grid, Price Glass Only                  *~
            *                                                           *~
            *  contour$    - Contour Grid (Y or N)                      *~
            *                                                           *~
            *  ct_sq_foot  - (Dec.) Sq Footage to Nearest Hundreth      *~
            *                                                           *~
            *  ct_gl_type$ - Glass Typ Description Flags (NNNNNN)       *~
            *                Order Cl, OBS, LE, ARG, TP, Avail          *~
            *                                                           *~
            *  ct_gl_lites%- Number of Lites Associated with grid. XX   *~
            *                                                           *~
            *  ct_cross$   - Shape Cross Reference (Grid and Hinge)     *~
            *                                                           *~
            *************************************************************

        convert_dimensions            /* Convert to the nearest Even Inch*/
                                      /* Odd with Fraction  - Add 1 Inch */
                                      /* Even with Fraction - Add 2 Inch */
                                      /* 1 St Base Width                 */
           init(" ") width_f$, height_f$, radius_f$, leg1_f$

           width, height, radius, leg1 = 0.0
                                                /* Convert Base Width    */
           width_f$ = str(ct_rec$,51%,9%)       /* Actual width          */           
           a%, b%, odd% = 0% 
           convert str(ct_rec$,51%,3%) to a%, data goto C_1
C_1:
           width = a%
                                                /* Check for Fraction    */ 
           if str(ct_rec$,55%,5%) = "     " then b% = 0%                   ~
                                            else b% = 1%
           if mod(a%,2%) <> 0% then odd% = 1%   /* Dimension Odd         */
                                                /* Even with no Fraction */
           if odd% = 0% and b% = 0%  then width = a%
                                                /* Even with Fraction    */ 
           if odd% = 0% and b% <> 0% then width = a% + 2%
                                                /* Odd with no Fraction  */       
           if odd% = 1% and b% = 0%  then width = a% + 1%
                                                /* Odd with Fraction     */
           if odd% = 1% and b% <> 0% then width = a% + 1% 
 
           convert width to width$, pic(##.##)
                                                /* Convert Glass Height  */
           height_f$ = str(ct_rec$,60%,9%)      /* Actual Height         */
           a%, b%, odd% = 0% 
           convert str(ct_rec$,60%,3%) to a%, data goto C_2

C_2:
           height = a%
                                                /* Check for Fraction    */ 
           if str(ct_rec$,64%,5%) = "     " then b% = 0%                   ~
                                            else b% = 1%
 
           if mod(a%,2%) <> 0% then odd% = 1%   /* Dimension Odd         */
         
                                                /* Even with no Fraction */
           if odd% = 0% and b% = 0%  then height = a%
                                                /* Even with Fraction    */ 
           if odd% = 0% and b% <> 0% then height = a% + 2%
                                                /* Odd with no Fraction  */       
           if odd% = 1% and b% = 0%  then height = a% + 1%
                                                /* Odd with Fraction     */
           if odd% = 1% and b% <> 0% then height = a% + 1% 

           convert height to height$, pic(##.##)
                                                /* Convert Glass Radius/ot*/
           radius_f$ = str(ct_rec$,70%,9%)      /* Actual Radius          */
           a%, b%, odd% = 0% 
           convert str(ct_rec$,70%,3%) to a%, data goto C_3
C_3:
           radius = a%
           if a% = 0% then goto C_3a            /* Nor Radius            */
 
                                                /* Check for Fraction    */ 
           if str(ct_rec$,74%,5%) = "     " then b% = 0%                   ~
                                            else b% = 1%
           if mod(a%,2%) <> 0% then odd% = 1%   /* Dimension Odd         */
                                                /* Even with no Fraction */
           if odd% = 0% and b% = 0%  then radius = a%
                                                /* Even with Fraction    */ 
           if odd% = 0% and b% <> 0% then radius = a% + 2%
                                                /* Odd with no Fraction  */       
           if odd% = 1% and b% = 0%  then radius = a% + 1%
                                                /* Odd with Fraction     */
           if odd% = 1% and b% <> 0% then radius = a% + 1%
C_3a:
           convert radius to radius$, pic(##.##) 
                                                /* Convert Glass Leg 1   */
           leg1_f$ = str(ct_rec$,80%,9%)        /* Actual Short Leg      */
           a%, b%, odd% = 0% 
           convert str(ct_rec$,80%,3%) to a%, data goto C_4
C_4:
           leg1 = a% 
           if a% = 0% then goto c_4a            /* No Leg1               */

                                                /* Check for Fraction    */ 
           if str(ct_rec$,84%,5%) = "     " then b% = 0%                   ~
                                            else b% = 1%
           if mod(a%,2%) <> 0% then odd% = 1%   /* Dimension Odd         */
                                                /* Even with no Fraction */
           if odd% = 0% and b% = 0%  then leg1 = a%
                                                /* Even with Fraction    */ 
           if odd% = 0% and b% <> 0% then leg1 = a% + 2%
                                                /* Odd with no Fraction  */       
           if odd% = 1% and b% = 0%  then leg1 = a% + 1%
                                                /* Odd with Fraction     */
           if odd% = 1% and b% <> 0% then leg1 = a% + 1%
C_4a:       
           convert leg1 to leg1$, pic(##.##) 

           thick% = 0%                         /* Thickness             */
                                               /* 3/4 Inch              */
           if str(ct_rec$,90%,6%) = ".75000" then thick% = 1%
                                               /* 13/16 Inch            */
           if str(ct_rec$,90%,6%) = ".81250" then thick% = 2%
                                               /* 5/8 Inch              */ 
           if str(ct_rec$,90%,6%) = ".62500" then thick% = 3% 
           
           convert thick% to thick$, pic(#)    /* Glass Thickness       */
                                   
           ct_grid$ = str(ct_rec$,47%,2%)      /* Grid Code             */

           ct_glass$ = str(ct_rec$,49%,2%)     /* Glass Code            */
                                               /* Grid Size             */
           ct_grid_size$ = "0"                 /* No Grid (Glass Only)  */
                                               /* (PAR000)              */
           if str(ct_rec$,44%,3%) = "1  " then ct_grid_size$ = "1"
           if str(ct_rec$,44%,3%) = "3/4" then ct_grid_size$ = "2"
           if str(ct_rec$,44%,3%) = "5/8" then ct_grid_size$ = "3"
           if str(ct_rec$,44%,3%) = "18 " then ct_grid_size$ = "4"
/* (AWD005) */
           if str(ct_rec$,44%,3%) = "SDL" then ct_grid_size$ = "6"

           grid_only$  = "N"

           glass_only$ = "N"                   /* No Grid in Glass      */
           for jj% = 1% to no_g_max%           /* Glass Only            */
               if no_g$(jj%) = ct_grid$ then glass_only$ = "Y"
               if glass_only$ = "Y" then goto C_5 
           next jj%
C_5:
                                               /* Check for Tempered    */
                                               /* Override              */
           if str(ct_rec$,126%,1%) = "Y" then grid_only$ = "Y"
                                               /* (PAR000)              */
           contour$ = str(ct_rec$,183%,1%)     /* Contour Grid Y/N      */
           if contour$ = "Y" and                                        ~
                       (ct_grid_size$ <> "0" and ct_grid_size$ <> "4")  ~
                                         then ct_grid_size$ = "1"

                                               /* Square Foot           */
           if shape% = 51% then height  = width
           if shape% = 51% then height$ = width$

           rhh =  ( width * height) / 144.0

           ct_sq_foot = round(rhh, 2)          /* Nearest Hundreth      */

           convert ct_sq_foot to ct_sq_foot$, pic(####.##-)
    
                                               /* Cut Glass Types       */
           gosub lookup_glass 
                                               /* Get No. Of Lites      */
           gosub lookup_lites
                                               /* (AWD002)              */    
           if scr_sel$ <> "9" then goto C_6
              ct_part$ = str(ct_rec$,195%,25%)
                                               /* Grid and Hinge        */
              ct_cross$ = str(ct_part$,7%,1%) & str(ct_part$,10%,1%)
                                               /* Woood Surround Y/N    */
              str(ct_special$,4%,1%) = str(ct_rec$,184%,1%)
              goto C_7
C_6:
                                               /* Get Window Price      */
           if scr_sel$ <> "0" then gosub lookup_window_price
C_7:
                                               /* Check for Half Rounds */
           gosub check_grid

                                               /* Set Wood Surround Y/N */
           ct_wood$ = str(ct_special$,4%,1%)

           ct_descr$ = "Glass&Grid"
           if glass_only$ = "Y" then ct_descr$ = "Glass Only"
           if grid_only$  = "Y" then ct_descr$ = "Grid Only "

           if  str(ct_grid$,2%,1%) = "Z" then ct_descr$ = "Specl Grid"

           if ct_wind_prc < .01 and scr_sel$ <> "9" then ct_descr$ = "No Charge "

           str(ct_rec$,184%,1%)  = ct_wood$
           str(ct_rec$,185%,10%) = ct_descr$
           str(ct_rec$,195%,25%) = ct_part$
                                               /* (RHHTEST) - Special Test     */
        REM   grid_only$  = "Y"                /* For Grid Only                */

        REM   glass_only$ = "Y"                /*  For Glass Only              */

                                               /* (RHHTEST) - Special Test     */ 

        return

        lookup_glass                           /* Cust Glass Flags     */
           init(" ") readkey$, desc$, ct_gl_type$
           ct_gl_type$ = "NNNNNN"

           str(readkey$, 1%,9%)  = "CUSTGLASS"
           str(readkey$,10%,15%) = ct_glass$
           read #1,key = readkey$, using LOOKUP_1, descr$,               ~
                                       eod goto LOOKUP_2
LOOKUP_1:       FMT POS(25), CH(30)

           ct_gl_type$ = str(descr$,1%,6%)
LOOKUP_2:
        return

        lookup_lites                           /* Cust Glass Flags     */
           init(" ") readkey$, desc$, ct_gl_lites$, ct_price_type$
           ct_gl_lites$ = "00"
           ct_gl_lites% = 0%

           str(readkey$, 1%,9%)  = "CUSTLITES"
           str(readkey$,10%,15%) = ct_grid$
           read #1,key = readkey$, using LOOKUP_1, descr$,               ~
                                       eod goto LOOKUP_3
           ct_gl_lites$ = str(descr$,1%,2%)
           convert ct_gl_lites$ to ct_gl_lites%, data goto LOOKUP_3

LOOKUP_3:

           ct_price_type$ = str(descr$,22,2)
        return

        lookup_window_price                    /* (APCPLNDT) Find Price */
           ct_wind_prc = 0.1                   /* Prevent Division by Zero */

           if scr_sel$ = "9" then return       /* Run from Report       */

           init(" ") dt_key4$, ct_wind_prc$, ct_part$, ct_cross$

           dt_key4$ = str(ct_rec$,12%,8%)      /* Warranty Id.          */
           read #3,key 4% = dt_key4$, using LOOKUP_4, ct_wind_prc, ct_part$, ~
                                              ct_special$,eod goto LOOKUP_5
LOOKUP_4:     FMT POS(133), PD(14,4), POS(189), CH(25), POS(220), CH(10)

LOOKUP_5:
           convert ct_wind_prc to ct_wind_prc$, pic(####.####-)

           ct_cross$ = str(ct_part$,7%,1%) & str(ct_part$,10%,1%)

        return     

        check_grid                             /* Half Round, Elliptical   */
                                               /* (1%,2%) - Grid Code      */
                                               /* (4%,2%) - Number of Hubs */
                                               /* (6%,1%) - Spokes Thru Y/N*/

            hub%      = 0%                     /* Number of Hubs           */
            spoke$    = "N"                    /* Spoke thru Hub Y or N    */
            gothic$   = "N"                    /* Gothic Y or N            */
            for jj% = 1% to eye_max%
                if ct_grid$ = str(eye$(jj%),1%,2%) then goto LOOKUP_6
            next jj%
        return
LOOKUP_6:
                                               /* Number of Hubs        */
            convert str(eye$(jj%),4%,2%) to hub%, data goto LOOKUP_7
LOOKUP_7:
            spoke$ = str(eye$(jj%),6%,1%)
                                               /* (AWD003)              */
            if jj% > 14%  and jj% < 21% then gothic$ = "Y"
            
            convert hub% to hub$, pic(##)

        return 
     
        lookup_glass_exceptions
           init(" ") readkey$, desc$ 
           cust_except% = 0%
           str(readkey$, 1%,9%)  = "CUSTEXCEP"
           str(readkey$,10%,15%) = ct_glass$
           read #1,key = readkey$, using LOOKUP_1, descr$,               ~
                                       eod goto LOOKUP_8
           cust_except% = 1%
LOOKUP_8:
        return

        REM *************************************************************~
            *           Special Shape Pricing Tables                    *~
            *                                                           *~
            *  calc_half_sunburst   -  Half Round, Ellipticle           *~
            *                                                           *~
            *  calc_half_gothic     -  Half Round, Ellipticle           *~
            *                                                           *~
            *  calc_half_grid_only  -  Half Round, Ellipticle           *~
            *                                                           *~
            *  calc_half_grid_gothic-  Half Round, Ellipticle           *~
            *                                                           *~
            *  calc_glass_sq_foot   -                                   *~
            *                                                           *~
            *  calc_glass_only      -                                   *~
            *                                                           *~
            *  calc_quarter_round   -                                   *~
            *                                                           *~
            *  glass_type_upcharge  -                                   *~
            *                                                           *~
            *  glass_argon_upcharge -                                   *~
            *                                                           *~
            *  calc_grid_only       -                                   *~
            *                                                           *~
            *************************************************************

        calc_half_sunburst
                                          /* Sunburst Three Spokes     */
            for jj% = 1% to 5%
               if width <= sb(jj%,1%) then goto CALC_1
            next jj%
CALC_1:                                   /* Colonial or Contoured     */
                                          /* 1   Inch use Contoured    */
            if ct_grid_size$ = "1" then ct_price = sb(jj%,3%) 
                                          /* 3/4 Inch use Colonial     */
            if ct_grid_size$ = "2" then ct_price = sb(jj%,2%)
                                          /* 5/8 Inch use Colonial     */
            if ct_grid_size$ = "3" then ct_price = sb(jj%,2%)

                                          /* 18mm Inch use Colonial     */
            if ct_grid_size$ = "4" then ct_price = sb(jj%,3%)

                                          /* Adjust for Spokes         */
            if spoke$ = "Y" then ct_price = ct_price + 3.06
                                          /* Adjust for Hubs           */
            if hub% > 1% then                               ~
               ct_price = ct_price +( (hub% - 1%)* 6.12 )
             
                                              /* (RHHTEST) */
        REM    convert jj% to rh$, pic(##)

        REM    convert ct_price to ct_price$, pic(####.####-)

        REM    call "SHOSTAT" ("Sunburst jj " & rh$ & " Hub " & hub$ & ~
        REM                     " Sz " & ct_grid_size$ )
        REM    stop
                                              /* (RHHTEST) */
        return

        calc_half_gothic
           for jj% = 1% to 5%
               if width <= gt(jj%,1%) then goto CALC_2
           next jj%
CALC_2:
                                          /* Colonial or Contoured     */
                                          /* 1   Inch use Contoured    */
           if ct_grid_size$ = "1" then ct_price = gt(jj%,3%)
                                          /* 3/4 Inch use Colonial     */
           if ct_grid_size$ = "2" then ct_price = gt(jj%,2%)
                                          /* 5/8 Inch use Colonial     */
           if ct_grid_size$ = "3" then ct_price = gt(jj%,2%)

                                          /* 18mm Inch use Contoured    */
           if ct_grid_size$ = "4" then ct_price = gt(jj%,3%)

                                              /* (RHHTEST) */
        REM   convert jj% to rh$, pic(##)

        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Gothic jj " & rh$ & " Sz " & ct_grid_size$ )
        REM   stop
                                              /* (RHHTEST) */
        return

        calc_half_grid_only               /* Sunburst Grid Only        */
           if gothic$ = "Y" then goto calc_half_grid_gothic
           
           for jj% = 1% to 5%
               if width <= so(jj%,1%) then goto CALC_3
           next jj%
CALC_3:
                                          /* Colonial or Contoured     */
                                          /* 1   Inch use Contoured    */
           if ct_grid_size$ = "1" then ct_price = so(jj%,3%)
                                          /* 3/4 Inch use Colonial     */
           if ct_grid_size$ = "2" then ct_price = so(jj%,2%)
                                          /* 5/8 Inch use Colonial     */
           if ct_grid_size$ = "3" then ct_price = so(jj%,2%)

                                          /* 18mm Inch use Contoured    */
           if ct_grid_size$ = "4" then ct_price = so(jj%,3%)

                                          /* Adjust for Spokes         */
           if spoke$ = "Y" then ct_price = ct_price + 3.06
                                          /* Adjust for Hubs           */
           if hub% > 1% then                               ~
               ct_price = ct_price +( (hub% - 1%)* 6.12 )
         
                                              /* (RHHTEST) */
        REM   convert jj% to rh$, pic(##)

        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Sun Grid Only jj " & rh$ & " Hub " & hub$ & ~
        REM                     " Sz " & ct_grid_size$ )
        REM   stop
                                              /* (RHHTEST) */
        return

        calc_half_grid_gothic             /* Gothic Grid Only          */
           for jj% = 1% to 5%
               if width <= got(jj%,1%) then goto CALC_4
           next jj%
CALC_4:
                                          /* Colonial or Contoured     */
                                          /* 1   Inch use Contoured    */
           if ct_grid_size$ = "1" then ct_price = got(jj%,3%)
                                          /* 3/4 Inch use Colonial     */
           if ct_grid_size$ = "2" then ct_price = got(jj%,2%)
                                          /* 5/8 Inch use Colonial     */
           if ct_grid_size$ = "3" then ct_price = got(jj%,2%)


                                          /* 18mm Inch use Contoured    */
           if ct_grid_size$ = "4" then ct_price = got(jj%,3%)
                                              /* (RHHTEST) */
        REM   convert jj% to rh$, pic(##)

        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Gothic Grid Only jj " & rh$ & " Sz " & ct_grid_size$ )
        REM   stop
                                              /* (RHHTEST) */

        return

       calc_glass_sq_foot     /* Glass price Only          */
                              /* Don't Calc Glass Twice    */
           calc_glass_sq_foot$ = "Y"  
                              /* Test for over 16 Sq Foot  */
           if ct_sq_foot > 16.0 then goto CALC_5
                                            /* Clear Glass */     
              if str(ct_gl_type$,1%,1%) = "Y" then          ~
                   ct_price, ct_clear = (ct_sq_foot * 2.80)
                                            /* OBS Glass   */
              if str(ct_gl_type$,2%,1%) = "Y" then          ~
                   ct_price, ct_obs = (ct_sq_foot * 3.06)
                                            /* Low-E Glass */
              if str(ct_gl_type$,3%,1%) = "Y" then          ~
                   ct_price, ct_lowe = (ct_sq_foot * 3.82)


              goto CALC_7
CALC_5:
                             /* Test for over 35 Sq Foot  */
           if ct_sq_foot > 35.0 then goto CALC_6                                             
                                            /* Clear Glass */     
           if str(ct_gl_type$,1%,1%) = "Y" then          ~
                   ct_price, ct_clear = (ct_sq_foot * 3.16)
                                            /* OBS Glass   */
           if str(ct_gl_type$,2%,1%) = "Y" then          ~
                   ct_price, ct_obs = (ct_sq_foot * 3.52)
                                            /* Low-E Glass */
           if str(ct_gl_type$,3%,1%) = "Y" then          ~
                   ct_price, ct_lowe = (ct_sq_foot * 4.18)
           goto CALC_7
CALC_6:
                            /* Over 35.0 and Less than 50 Sq Foot*/
                                            /* Clear Glass */     
           if str(ct_gl_type$,1%,1%) = "Y" then          ~
                   ct_price, ct_clear = (ct_sq_foot * 3.31)
                                            /* OBS Glass   */
           if str(ct_gl_type$,2%,1%) = "Y" then          ~
                   ct_price, ct_obs = (ct_sq_foot * 3.67)
                                            /* Low-E Glass */
           if str(ct_gl_type$,3%,1%) = "Y" then          ~
                   ct_price, ct_lowe = (ct_sq_foot * 4.33)

CALC_7:
                                            /* Argon Gas   */
           if str(ct_gl_type$,4%,1%) = "Y" then          ~
                   ct_price = ct_price + 3.00
                                            /* Tempered Gls*/
           if str(ct_gl_type$,5%,1%) = "Y" then          ~
                   ct_price = ct_price + (ct_sq_foot * 2.00)

                                              /* (RHHTEST) */
        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Glass Sq. Foot " & ct_sq_foot$ & ~
        REM                     " Price " & ct_price$ )
        REM   stop
                                              /* (RHHTEST) */
        return
                              /* Glass and Grid Non-Standard */
        calc_glass_only

           for jj% = 1% to 4%
               if width <= gsw(jj%) then goto CALC_8
           next jj%
CALC_8:
           for kk% = 1% to 4%
               if height <= gsh(kk%) then goto CALC_9
           next kk%
CALC_9:
           ct_price = gsp(jj%,kk%)
 
                                              /* (RHHTEST) */
        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Glass/Grid Non-Std " & width$ & "-" & height$ & ~
        REM                     " Price " & ct_price$ )
        REM   stop
                                              /* (RHHTEST) */
        return

        calc_quarter_round                /* Quarter Round Glass/Grid  */
           for jj% = 1% to 4%
               if width <= qtr(jj%,1%) then goto CALC_10
           next jj%
CALC_10:
                                          /* Colonial or Contoured     */
                                          /* 1   Inch use Contoured    */
           if ct_grid_size$ = "1" then ct_price = qtr(jj%,3%)
                                          /* 3/4 Inch use Colonial     */
           if ct_grid_size$ = "2" then ct_price = qtr(jj%,2%)
                                          /* 5/8 Inch use Colonial     */
           if ct_grid_size$ = "3" then ct_price = qtr(jj%,2%)

                                          /* 18mm Inch use Contoured    */
           if ct_grid_size$ = "4" then ct_price = qtr(jj%,3%)

                                              /* (RHHTEST) */
        REM   convert jj% to rh$, pic(##)

        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Quarter Round jj " & rh$ & " Sz " & ct_grid_size$ & ~
        REM                     " Price " & ct_price$ )
        REM   stop
                                              /* (RHHTEST) */
        return


        glass_type_upcharge
           if grid_only$ = "Y" then return  /* No Glass     */
                                            /* Obscure Glass*/
           if str(ct_gl_type$,2%,1%) = "Y" then              ~
              ct_price = ct_price + (.35 * ct_sq_foot)
                                           /* Low E Glass   */
           if str(ct_gl_type$,3%,1%) = "Y" then              ~
              ct_price = ct_price + (1.00 * ct_sq_foot)
                                           /* Tempered      */
           if str(ct_gl_type$,5%,1%) = "Y" then              ~
              ct_price = ct_price + (2.00 * ct_sq_foot)

                                              /* (RHHTEST) */

        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Glass Type Up Charge " & ct_gl_type$ &~
        REM                                " Price " & ct_price$ )
        REM   stop
                                              /* (RHHTEST) */

        return

        glass_argon_upcharge
                                           /* Argon Gas     */
           if str(ct_gl_type$,4%,1%) = "Y" then              ~
              ct_price = ct_price + 3.00
                                              /* (RHHTEST) */

        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Argon Glass Up Charge " &~
        REM                                " Price " & ct_price$ )
        REM   stop
                                              /* (RHHTEST) */

        return

                                           /* Grid Only Calc*/ 
        calc_grid_only                     /* All Shapes    */
           ct_price = 0.0                  /*except Half Rnd*/
           gosub calc_glass_only
                                           /* Calc Muttin   */
                                           /* Up Charge     */
           if contour$ = "Y" then                             ~
              ct_price = ct_price + (ct_price *.15)

                                           /* Calc Grid Only*/
           ct_price = ct_price - (ct_price * .50)
                                              /* (RHHTEST) */

        REM   convert ct_price to ct_price$, pic(####.####-)

        REM   call "SHOSTAT" ("Calc Grid Only-Contour " & contour$ & ~
        REM                                " Price " & ct_price$ )
        REM   stop
                                              /* (RHHTEST) */

        return

/* (AWD005) */
        calc_sdl
REM          take out, this was custom pricing
REM           if ct_price_type$ = "CO" then ct_price = (ct_gl_lites% * 2.8925)
REM           if ct_price_type$ = "AS" then ct_price = (ct_gl_lites% * 4.0170)
REM           if ct_price_type$ = "CS" then ct_price = (ct_gl_lites% * 3.3150)
           base_price, lite_price, ct_price = 0.00

REM        Per lite price 2.50 except for rectangular "P" which is 2.00
           per_lite = 2.50
           if str(ct_grid$,1,1) = "P" then per_lite = 2.00

REM        can not calculate if do not know number of lites           
           if ct_gl_lites% = 0% then return

REM            base price is 2.35 * sq footage
           base_price = round(ct_sq_foot * 2.35, 4)
           lite_price = round(ct_gl_lites% * per_lite, 4)

REM          Infini-lite gave Atrium 0.43 multiplier             
           ct_price   = round( (base_price + lite_price) * 0.43, 4)

REM           multiply by two to get price for pair
           ct_price   = round(ct_price * 2, 4)
        return
/* (AWD005) */
                                 
        debug_display_screen
            if debug% = 0% then return             /* Debug Turned Off */

            header$ = "(AWDCALPR)Debug Shp(XX) Bar xxxxxxxxx Seq xxxxx Mod xxx XXXXXXXX"

            convert shape% to str(header$,21%,2%), pic(00)

            str(header$,29%,9%) = str(ct_rec$,12%,9%)     /* Barcode     */

            str(header$,43%,5%) = str(ct_rec$,7%,5%)      /* Seq. No.    */

            str(header$,53%,3%) = str(ct_rec$,32%,3%)     /* Model Code  */

            str(header$,57%,8%) = str(ct_rec$,24%,8%)     /* Sales Order */

            inpmessage$ = "Press <Return> to Continue?" 

            date$ = date
            call "DATEFMT" (date$)

            gosub set_keys

            ct_pct =  (ct_price / ct_wind_prc ) * 100.0

            convert ct_pct to ct_pct$, pic(###.###-)

            accept                                                       ~
               at (01,02), fac(hex(84)), header$                , ch(75),~
               at (01,67), "Today",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), "Window Width     :",                         ~
               at (03,21), fac(hex(84)), width$                 , ch(05),~
               at (03,30), fac(hex(84)), width_f$               , ch(09),~
                                                                         ~
               at (04,02), "Window Height    :",                         ~
               at (04,21), fac(hex(84)), height$                , ch(05),~
               at (04,30), fac(hex(84)), height_f$              , ch(09),~
                                                                         ~
               at (05,02), "Window Radius    :",                         ~
               at (05,21), fac(hex(84)), radius$                , ch(05),~
               at (05,30), fac(hex(84)), radius_f$              , ch(09),~
                                                                         ~
               at (06,02), "Window Short Leg :",                         ~
               at (06,21), fac(hex(84)), leg1$                  , ch(05),~
               at (06,30), fac(hex(84)), leg1_f$                , ch(09),~
                                                                         ~
               at (07,02), "Glass Grid Code  :",                         ~
               at (07,21), fac(hex(84)), ct_grid$               , ch(02),~
               at (07,30), fac(hex(84)), gothic$                , ch(02),~
               at (07,40), fac(hex(84)), hub$                   , ch(02),~
                                                                         ~
               at (08,02), "Glass Code       :",                         ~
               at (08,21), fac(hex(84)), ct_glass$              , ch(02),~
                                                                         ~
               at (09,02), "Grid Size Code   :",                         ~
               at (09,21), fac(hex(84)), ct_grid_size$          , ch(01),~
                                                                         ~
               at (10,02), "Price Grid Only  :",                         ~
               at (10,21), fac(hex(84)), grid_only$             , ch(01),~
                                                                         ~
               at (11,02), "No Grid, Prc Gls :",                         ~
               at (11,21), fac(hex(84)), glass_only$            , ch(01),~
                                                                         ~
               at (12,02), "Glass Square Foot:",                         ~
               at (12,21), fac(hex(84)), ct_sq_foot$            , ch(08),~
                                                                         ~
               at (13,02), "Glass Type Descr :",                         ~
               at (13,21), fac(hex(84)), ct_gl_type$            , ch(06),~
                                                                         ~
               at (14,02), "Grid No. of Lites:",                         ~
               at (14,21), fac(hex(84)), ct_gl_lites$           , ch(02),~
                                                                         ~
               at (15,02), "Glass Thickness  :",                         ~
               at (15,21), fac(hex(84)), thick$                 , ch(01),~
                                                                         ~ 
               at (17,02), "Glass Price      :",                         ~
               at (17,21), fac(hex(84)), ct_price$              , ch(10),~
                                                                         ~
               at (18,02), "Window Price     :",                         ~
               at (18,21), fac(hex(84)), ct_wind_prc$           , ch(10),~
                                                                         ~
               at (19,02), "Glass PCT of Prc :",                         ~
               at (19,21), fac(hex(84)), ct_pct$                , ch(08),~
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


 
