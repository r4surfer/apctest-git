        REM *************************************************************~
            *  ( Used in 'APCPLN06' and 'EWDCALSL'-Sub                  *~
            *                                                           *~ 
            *  Program Name      - AWDPLN9B                             *~
            *  Creation Date     - 02/27/04                             *~
            *  Last Modified Date- 04/20/2013                           *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Description       - This Program returns a sort key for  *~
            *                      the Special Shapes Department.       *~
            *                      Customized sort key                  *~
            *                                                           *~
            *                      Special Note - The sort Key is not   *~
            *                          compressed.                      *~
            *                                                           *~ 
            *  Special Comments  - Subroutine                           *~
            *                                                           *~
            *             (SHAPCROSS)  Shape Cross Reference with 25    *~
            *                          digit part number. <LH>          *~
            *                          L = 1st Digit of Liting Code     *~
            *                          H = 2nd Digit of Hinge Code      *~
            *                          Description = xx Shape Code      *~
            *                                                           *~
            *              Note       (C0)  = Elliptical                *~
            *                         (H0)  = True Half Round           *~
            *                                                           *~
            *                         ( Test Based on Decimal Width)    *~
            *   Sort Order      000 - No Bend Shapes                    *~
            *                   100 - Tool Set ( 59.25 - 77.25 )        *~
            *                   200 - Tool Set ( 39.25 - 59.2499 )      *~
            *                   300 - Tool Set ( 28.25 - 39.2499 )      *~
            *                   400 - Tool Set ( 23.25 - 28.2499 )      *~
            *                   500 - Eyebrow and Elliptical over 77.25 *~
            *                   500 - Half Rounds over 77.250           *~
            *                                                           *~
            *                   700 - Put all FGO's                     *~
            *                         after all other tool sets.        *~
            *                                                           *~ 
            *   Format        SSS T MMM CC DDD NNNNN                    *~
            *                                                           *~
            *                 SSS -  Tool Seq. 000 - 600                *~
            *                                                           *~  
            *                   T -  Tool Set 0 to 6                    *~
            *                                                           *~  
            *                 MMM -  Shape Model Code                   *~
            *                                                           *~
            *                  CC -  Special Shapes Code                *~
            *                                                           *~
            *                 DDD -  Special Shapes Department          *~
            *                                                           *~
            *          wwww.wwww  -  Width Sort Larget to Smallest      *~
            *                                                           *~ 
            *               NNNNN -  Shapes Piece Counter               *~
            *                                                           *~
            *  Special Logic for Tool Sets Using Arch Radius for        *~
            *                                                           *~
            *          1/2 Eyebrow Left/Right       2 Times Arch Radius *~
            *          Eyebrow                      2 Times Arch Radius *~
            *          Left/Right Quarter Round     2 Times Width       *~
            *          1/2 Colonial Arch Left/Right 2 Times width       *~
            *          Colonial Arch                2 Time Arch Radius  *~
            *          Elliptical                   2 Times Arch Radius *~
            *                                                           *~
            *          Special Subs 'convert_width'                     *~
            *                       'convert_height_leg'                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/06/04 ! New Program for (AWD) - Last Mod Date    ! RHH *~ 
            * 03/10/08 ! (AWD001) - mod for casing sort           ! CMG *~
            *04/20/2013! (AWD002) - mod to use sort_rec (APCPLNDT ! CMG *~
            *          !                 dept code                !     *~
            *************************************************************
        sub "AWDPLN9B"   (sort_rec$,     /* APCPLNDT Sort Record       */~
                          sort_key$,     /* Reporting Sort Key         */~
                          specialmull$,  /* (Awd001) casing            */~
                          #1)            /* GENCODES                   */

        dim sort_key$60,                 /* Reporting Sort Key         */~
            s$1,                         /* Screen Code Flag           */~
            sort_rec$256,                /* APCPLNDT - DETAIL RECORD   */~
            t_set(10%,2%),               /* Special Shapes Tool Sets   */~
            n_bend$(20%)2,               /* No Bend Shapes             */~
            s_table$(10%)2,              /* Segment table Shapes       */~ 
            dt_part$25,                  /* MFG Part Number            */~
            model$3,                     /* Model Code                 */~
            shape_cross$2,               /* liting 1st, Hinge 2nd      */~
            sh_config$2,                 /* Shape Code                 */~
            tool$,                       /* Tool set 0 to 5            */~
            tool_seq$3,                  /* Tool seq code              */~
            shape_cnt$5,                 /* Unique Counter             */~
            width$10,                    /* Width Sort Large to Small  8/~    
            pd$40,                       /* pad Characters '0'         */~
            dt_in$40,                    /* Uncompressed Sort Data     */~
            dt_out$30,                   /* Compressed Sort Data       */~
            errormsg$40,                 /* Error Display              */~ 
            readkey$24, desc$30,         /* GENCODES File Read Key     */~
            been_here_before$1,          /* Flag for array loaded      */~
            specialmull$1                /* (AWD001) casing            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Special Shapes Sort Index 05/06/04"
            pname$ = "APCPLN9B - Rev: R1.00"
            pd$ = "0000000000000000000000000000000000000000"
            
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! GENCODES ! System Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            init(" ") sort_key$, readkey$

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *-----------------------------------------------------------*~
            * Processes data from calling program to produce key.       *~
            *************************************************************
        REM BUILD_KEY
            if  been_here_before$   = "Y"       then goto L00100
                been_here_before$   = "Y"
                shape_cnt% = 0%
                                                 /* Tool Set (1)        */ 
                t_set(1%,1%) = 59.25  :   t_set(1%,2%) = 77.2499

                                                 /* Tool Set (2)        */
                t_set(2%,1%) = 39.25  :   t_set(2%,2%) = 59.2499

                                                 /* Tool Set (3)        */
                t_set(3%,1%) = 28.25  :   t_set(3%,2%) = 39.2499

                                                 /* Tool Set (4)        */

                t_set(4%,1%) = 00.00  :   t_set(4%,2%) = 28.2499

                                                 /* Tool Set (5)        */
                                                 /* Segment Table       */
                t_set(5%,1%) = 77.25            /* Any Eyebrow or      */
                                                 /* Elliptical over     */

                t_set(5%,2%) = 77.250            /* Any Half Round over */  

                                                 /* Tool Set (0)        */
                                                 /* Non Bend Shapes     */
                n_bend$(1%) = "03"               /* Trapezoid           */
                n_bend$(2%) = "04"               /* Trapezoid           */
                n_bend$(3%) = "00"               /* Picture Window      */
                n_bend$(4%) = "25"               /* Octagon             */
                n_bend$(5%) = "05"               /* Triagle             */
                n_bend$(6%) = "06"               /* Triangle            */
                n_bend$(7%) = "07"               /* Triangle ISOC       */
                n_bend$(8%) = "01"               /* Pentagons           */
                n_bend$(9%) = "02"               /* Pentagons           */
                n_bend$(10%)= "15"               /* Pentagons           */ 
                n_bend$(11%)= "99"               /* Special             */
 
                n_bend% = 11%
                                                 /* Tool Set (5) the    */
                                                 /* Segment Table       */
                s_table$(1%) = "60"              /* Eyebrow             */
                s_table$(2%) = "70"              /* Eyebrow             */
                s_table$(3%) = "71"              /* Eyebrow             */
                s_table$(4%) = "64"              /* Half Round          */ 

                s_table% = 4%
  

L00100:                                          /* Convert the Width to*/
            init(" ") width$                     /* a Decimal Value     */
            dt_part$ = str(sort_rec$,189%,25%)

            s$       = str(dt_part$,11%,1%)  /* Set Screen Code      */
                                             /* 6 FGO's at the end   */
     
            a1, a2 = 0.0
            width  = 0.0
            convert str(dt_part$,13%,3%) to a1, data goto L00110
L00110:
            convert str(dt_part$,16%,1%) to a2, data goto L00120
L00120:
            width = a1 + (a2/8.0)                /* Width Decimal Value */

            rhh = 1000.0000 - width              /* Convert for Largest */
                                                 /* to samllest         */
            convert rhh to width$, pic(####.####-) 

            gosub lookup_shape                   /* Get the Shape Code  */
                                                 /* using the LITING and*/
                                                 /* HINGE Code          */
           gosub convert_width 
                                               
                                                 /* 1st check FGO       */
            tool_seq$ = "700"
            tool$     = "7"
            if s$ = "6" then goto build_sort     /* Screen Code         */
 
                                                 /* 2nd check Non-Bend  */
            gosub non_bend
            if check% = 1% then goto build_sort  /* Non Bend Found      */

                                                 /* 3nd Segment Table   */
            gosub segment_table
            if check% = 1% then goto build_sort  /* Segment Table Found */

                                                 /* 4th Check Tool Sets */
            gosub tools
            if check% = 1% then goto build_sort  /* Tool Set Found      */

                                                 /* Catch All           */
            gosub segment_table_3
            goto build_sort

            goto exit_sub 

            non_bend
               check% = 0%       
               for ii% = 1% to n_bend%
                   if sh_config$ = n_bend$(ii%) then goto no_bend_found
               next ii% 
            return
            no_bend_found
               tool$     = "0"                   /* No Bend Tool Set    */
               tool_seq$ = "000"                 /* Tool set Sequence   */

               check%    = 1%
            return

            segment_table
               check% = 0%       
               for ii% = 1% to s_table%
                   if sh_config$ = s_table$(ii%) then goto segment_table_found
               next ii% 
            return
            segment_table_found                  /* Eyebrow and       */
                                                 /* Ellipticals Over  */
                                                 /* 77.25             */
                                          /* Special Test Ellipticals */ 
               if sh_config$ = "64" and str(shape_cross$,1%,1%) <> "C"   ~
                                              then goto segment_table_2

                                          /* If a Half Round then    */
                                          /* must be ellipticle      */

                  if width > t_set(5%,1%) then goto segment_table_3
                  return

            segment_table_2                      /* Check Half Rounds */
                                                 /* Over 77.25        */ 
               if width > t_set(5%,2%) then goto segment_table_3 
               return

            segment_table_3
               tool$     = "5"                   /* Segment Table Tool Set*/
               tool_seq$ = "500"                 /* Tool set Sequence   */          
               check%    = 1%
            return

            tools
               check% = 0%
               for ii% = 1% to 4%
                    
                   if width >= t_set(ii%,1%) and width <= t_set(ii%,2%) ~
                                       then goto tools_found
               next ii%


            return

            tools_found
               convert ii% to tool$, pic(#)      /* Tool Set            */ 

               tool_seq$ = tool$ & "00"          /* Tool set Sequence   */          
               check%    = 1%
            return

            lookup_shape
               init(" ") readkey$, desc$, sh_config$, shape_cross$, model$
               model$ = str(dt_part$,1%,3%)      /* Save Model          */

                                                 /* 1st Digit of LITING */
               str(shape_cross$,1%,1%) = str(dt_part$,7%,1%)
                                                
                                                 /* 2nd Digit of HINGE  */
               str(shape_cross$,2%,1%) = str(dt_part$,10%,1%)
                                                  /* For Elipticles      */
               if str(shape_cross$,1%,1%) = "C" then str(shape_cross$,2%,1%) = "0"
                                                 /* For Cirlces         */
               if str(shape_cross$,1%,1%) = "E" then str(shape_cross$,2%,1%) = "0"
                                                 /* For Octagons        */
               if str(shape_cross$,1%,1%) = "I" then str(shape_cross$,2%,1%) = "0"


               gosub lookup_specials                 /* Check for Octagon   */
                                                     /* and Cirles Until WW */
                                                     /* Fixed (Left Out)    */
                                               
               str(readkey$, 1%,9%)  = "SHAPCROSS"  /* Shape Codes      */
               str(readkey$,10%,15%) = shape_cross$
               read #1,key = readkey$, using L01000, desc$,               ~
                                       eod goto lookup_shape_done
L01000:           FMT POS(25), CH(30)

               sh_config$     = str(desc$,1%,2%) /* Shape Code           */
            return

            lookup_shape_done
               sh_config$ = "99"                 /* Special Not On File  */
            return                                

            build_sort
              shape_cnt% = shape_cnt% + 1%
              convert shape_cnt% to shape_cnt$, pic(00000)

              str(sort_key$,1%,3%)  = tool_seq$  /* Tool Seq. 000-500    */
              
              str(sort_key$,4%,1%)  = tool$      /* Tool Set 0 to 5      */
                                                 /* Temp Chg 1 to 7      */ 

              str(sort_key$,5%,3%)  = model$     /* Shape Model Code     */
     
              str(sort_key$,8%,3%)  = "0" & sh_config$ /* Special Shapes Code*/
REM Mod to use dt_dept  (AWD002)              
                                                     /* Special Shapes Depart*/
              str(sort_key$,11%,3%) = str(sort_rec$,42%,3%)
 
              str(sort_key$,14%,10%)= width$     /* Width Sort           */

              str(sort_key$,24%,5%) = shape_cnt$ /* Shape Count          */

            goto exit_sub

        lookup_specials                         /* Octagon/Circles     */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%) = "SHAPEXTRA"
           str(readkey$,10%,15%) = model$
           read #1,key = readkey$, using L01000, desc$,               ~
                                    eod goto lookup_specials_done
                shape_cross$ = str(desc$,1%,2%)

        lookup_specials_done

        return

        convert_width                        /* Convert to Arch Radius */
            save_width = 0.0

            save_width = width
                                             /* Check for Elliptical   */
            if shape_cross$ <> "C0" then goto C_1
                                             /* Arch Radius            */
               width = ( (width/2)**2 + (width/3)**2) /(2*(width/3))
               
               width = 2 * width             /* Twice Arch Radius      */
               return
C_1:
                                             /* Check Eyebrows 1/2 Lft/Rt */
            if sh_config$ <> "70" and sh_config$ <> "71" and sh_config$ <> "60" ~
                                                           then goto C_2

               gosub convert_height_leg
               s1 = ABS((height - leg))     /* Segment Climb             */
               width = ((4*(s1**2)) + (width**2)) / (8*s1)
               
               width = 2 * width            /* Twice Arch Radius         */ 
               return
C_2:
                                             /* Colonial 1/2 Lft/Rt */
            if sh_config$ <> "73" and sh_config$ <> "74"   then goto C_3
               
               width = 2 * width            /* Twice Width              */
               return
C_3:
                                             /* Check Colonial          */
            if sh_config$ <> "63" then goto C_4
               width = (width/2)
         
               width = 2 * width             /* Twice Arch Radius      */
               return 
C_4:
                                            /* Check Qtr 1/2 Lft/Rt   */
            if sh_config$ <> "66" and sh_config$ <> "67" then return

               width = 2 * width            /* Twice Width           */

        return        
 
        convert_height_leg
                                               /* Convert Leg       */ 
            a1, a2 = 0.0
            leg    = 0.0
            convert str(dt_part$,20%,2%) to a1, data goto Convert_1
Convert_1:
            convert str(dt_part$,22%,1%) to a2, data goto Convert_2
Convert_2:
            leg = a1 + (a2/8.0)                /* Leg Decimal Value */

            a1, a2 = 0.0                       /* Convert Height    */
            height = 0.0
            convert str(dt_part$,17%,2%) to a1, data goto Convert_3
Convert_3:
            convert str(dt_part$,19%,1%) to a2, data goto Convert_4
Convert_4:
            height = a1 + (a2/8.0)            /* Height Decimal Value */

        return
  
        
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_sub                             /* New Data Compression   */
            init(" ") dt_in$, dt_out$        /* Conversion of Index    */  
            dt_in% = 40% : dt_out% = 0%      /* Max Characters (40)    */
            dt_in$ = str(sort_key$,1%,40%)
            ln% = len(dt_in$)                /* Find out Actual length */
            ln1% = 40% - ln%                 /* Calc the No. of Pad    */
                                             /* Characters needed      */
            str(dt_in$,ln%+1%,ln1%) = str(pd$,1%,ln1%) 
            call "COMPRESS" (dt_in$,dt_in%,dt_out$,dt_out%, ret%)
            if ret% <> 0% then gosub L03000  /* Error has Occurred     */
        REM    init(" ") sort_key$
                                             /* Special Mod to sort    */ 
        REM    str(sort_key$,1%,30%) = dt_out$
                                             /* Sort key is not compressed */
                                             /* current length is (28) */
/*(AWD001) */
            if specialmull$ = "C" then shape_cnt% = shape_cnt% + 2%
/*(AWD001) */
            if specialmull$ = "R" then shape_cnt% = shape_cnt% + 1%
        end
L03000: errormsg$ = "Sort Error - "& str(sort_rec$,24%,18%)
        call "SHOSTAT" (errormsg$) : stop
        dt_out$ = sort_key$
        return


