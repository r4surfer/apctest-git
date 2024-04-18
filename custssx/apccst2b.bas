*       ****************************************************************~
*                           ( As of 05/29/2014 - CMG )                 *~
*        ( Special Note ) Same Glass routine that is in in (APCPLA45). *~
*                         also uses the same Screen logic as in        *~
*                         (APCPLN47). Only two programs where Glass    *~
*                         and Screen are Calculated.                   *~
*                                                                      *~
*        APCCST2B - Subroutine to Calculate the Width and Height Cut   *~
*                   Sizes for Glass and Screen. The Cost of Materials  *~
*                   for Glass, Swiggle, and Screen including Scrap is  *~
*                   also Calculated.                                   *~
*                                                                      *~
*                   (Glass) - (Calc) -   Tables 'COST 01GM','COST 01GS'*~
*                                               'COST 01SG'            *~
*                           - GS_QTY() - (1) Top Glass Sq Inches       *~
*                                        (2) Bottom Glass Sq Inches(1) *~
*                                        (3) Bottom Glass Sq Inches(2) *~
*                             SWG()    - (1) Top Swiggle Per Foot      *~
*                                        (2) Bot Swiggle Per Foot (1)  *~
*                                        (3) Bot Swiggle Per Foot (2)  *~
*                           - (Cost)                                   *~
*                           - GS_QTY() - (1) Total Raw Mat'l (1)       *~
*                                        (2) Total Raw Mat'l (2)     ) *~
*                                        (3) Total Swigle              *~
*                             GS_RAW() - (1) 1st Raw Material          *~
*                                        (2) 2nd Raw Material          *~
*                                        (3) Swiggle                   *~
*                             GS_COST()- (1) 1st Raw Material          *~
*                                        (2) 2nd Raw Material          *~
*                                        (3) Swiggle                   *~
*                                                                      *~
*                   (Screen)- (Calc) -   Tables 'COST 02SC'            *~
*                           - GS_QTY() - (1) Screen in Per Sq Foot     *~
*                                        (2) Thru (5) Per Inch         *~
*                                        (6) Per Foot                  *~
*                                         Note - No Mesh for 500 Series*~
*                                                Mil Finish.           *~
*                                                                      *~
*                             GS_RAW() - (1) Screen Raw Material       *~
*                                        (2) Width   - (1)             *~
*                                        (3) Width   - (2)             *~
*                                        (4) Height  - (3)             *~
*                                        (5) Height  - (4)             *~
*                                        (6) Parimeter (5)             *~
*                                                                      *~
*                   (Scrap) - (Calc) -   Tables 'COST 01SP'            *~
*                                        Glass and Screen Scrap        *~
*                                        Calculated.                   *~
*                                                                      *~
*                   Note - (1) Uses Subroutine (APCCST9B) to Calculate *~
*                              the Unit Cost Associated with a Raw     *~
*                              Material.                               *~
*                                                                      *~
*                        - (2) Mod for Calculating Double Stength      *~
*                              Glass and Swiggle for Tempered Glass.   *~
*                                                                      *~
*                     PART$         = MFG Part Number                  *~
*                     TYP%          = 0% = Glass, 1% = Screen          *~
*                     GS_QTY()      = G/S Square Inches Mat'l (1-8)    *~
*                     GS_RAW$()     = G/S Raw Mat'l (1-8)              *~
*                     GS_DESC$()    = G/S Raw Mat'l Description (1-8)  *~
*                     GS_COST()     = G/S Cost Mat'l (1-8)             *~
*                     GS_UNITS%()   = G/S UNITS OF MEASURE             *~
*                     GS_UNIT()     = G/S Unit Cost of Material        *~
*                     GS_VINYL      = G/S Total Cost Vinyl             *~
*                     GS_MISC       = G/S Total Cost Misc              *~
*                     GS_TOTAL      = G/S Total Cost                   *~
*                     GS_CNT%       = G/S Number of Raw Mat'l Items    *~
*                     GS_QTY_S()    = G/S Sq Inch of Mat'l (1-8) Scrap *~
*                     GS_COST_S()   = G/S Scrap Cost of Mat'l (1-8)    *~
*                     GS_VINYL_S    = G/S Total Cost Scrap Vinyl       *~
*                     GS_MISC_S     = G/S Total Cost Scrap Misc        *~
*                     GS_TOTAL_S    = G/S Total Cost Scrap             *~
*                     RM_EQ$()      = Save Calc Type and Equation No.  *~
*                     RM_PH$()      = Save Phantom Number              *~
*                     GS_FRT        = Save Amount of Freight Cost      *~
*                     GS_VINYL_D    = Save Amount of Vinyl Discount    *~
*----------!----------------------------------------------------!------*~
*   DATE   !                MODIFICATION                        ! WHO  *~
*----------!----------------------------------------------------!------*~
* 03/27/98 ! y2k compliant                                      !  DJD *~
* 12/03/99 ! Mod to take out "model = 1" code.  EWD001          !  CMG *~
* 10/28/02 ! (EWD002) - Mod for new patio door models.          !  CMG *~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC            ! CMG *~
*----------!----------------------------------------------------!------*~
************************************************************************

        sub "APCCST2B" (part$,           /* MFG Part Number           */ ~
                        typ%,            /* 0% = Glass, 1% = Screen   */ ~
                        gs_qty(),        /* G/S Sq Inches Mat'l (1-8) */ ~
                        gs_raw$(),       /* G/S Raw Mat'l (1-8)       */ ~
                        gs_desc$(),      /* G/S Raw Description (1-8) */ ~
                        gs_cost(),       /* G/S Cost Mat'l (1-8)      */ ~
                        gs_units%(),     /* G/S Units of Measure (1-8)*/ ~
                        gs_unit(),       /* G/S Unit Cost of Material */ ~
                        gs_vinyl,        /* G/S Total Cost Mat'ls Viny*/ ~
                        gs_misc,         /* G/S Total Cost Mat'ls Misc*/ ~
                        gs_total,        /* G/S Total Cost Mat'ls     */ ~
                        gs_cnt%,         /* G/S No. of Raw Mat'l Items*/ ~
                        gs_qty_s(),      /* G/S Sq Inch Mat'l Scrap   */ ~
                        gs_cost_s(),     /* G/S Scrap Cost Mat'l      */ ~
                        gs_vinyl_s,      /* G/S Total Cost Scrap Vinyl*/ ~
                        gs_misc_s,       /* G/S Total Cost Scrap Misc */ ~
                        gs_total_s,      /* G/S Total Cost Scrap      */ ~
                        rm_eq$(),        /* Save Calc Type and Eq. No.*/ ~
                        rm_ph$(),        /* Save Phantom Number       */ ~
                        gs_frt,          /* Save Cost of Freight      */ ~
                        gs_vinyl_d,      /* Save Vinyl Discount Amt   */ ~
                              #1,        /* (APCEQUAT) - FILE         */ ~
                              #2,        /* (AMTBOMCD) - File         */ ~
                              #3,        /* (GENCODES) - File         */ ~
                              #5,        /* (HNYQUAN ) - File         */ ~
                              #6,        /* (HNYMASTR) - File         */ ~
                        gs_err% )        /* 0% = Ok, Non 0% = Error   */

        dim part$25,                     /* MFG Part Number           */ ~
            tab$(10%)9,                  /* Glass Table Names          */~
            gs_qty(9%), gs_qty_s(9%),    /* Square Inches             */ ~
            gs_raw$(9%)25,               /* G/S Raw Material No's     */ ~
            gs_desc$(9%)32,              /* G/S Raw Desc's            */ ~
            gs_cost(9%), gs_cost_s(9%),  /* G/S Raw Material Costs    */ ~
            rm_eq$(100%)3,               /* Calc Type and Eq. No.     */ ~
            rm_ph$(100%)5,               /* Save Phantom Number       */ ~
            mm$1, model$3,               /* Product and Model         */ ~
            phantom$25,                  /* Phantom Part Number       */ ~
            ss$(20%)20,                  /* Screen Special Process    */ ~
            view$3,                      /* View Top or Bot           */ ~
            cl$1, cl1$1,                 /* Product Color             */ ~
            gl$2,                        /* Glass Code                */ ~
            lt$2,                        /* Liting Code               */ ~
            hg$2, hgl$3,                 /* Hinge Code                */ ~
            sc$1,                        /* Screen Code               */ ~
            lk$1,                        /* Locks Code                */ ~
            vv$3,                        /* RAW MAT'L VINYL CODE      */ ~
            width$4, calc$5,             /* Product Width             */ ~
            height$3, calc_out$9,        /* Product Height            */ ~
            clmr$3,                      /* Clmr                      */ ~
            wallwidt$3,                  /* Wallwidt                  */ ~
            raw_err$(20%)20,             /*                           */ ~
            gs_units%(100%),             /* GLASS/SCREEN UNITS-MEASURE*/ ~
            gs_unit(100%),               /* GLASS/SCREEN UNIT Cost    */ ~
            sz$100, zz$100,              /* CONVERT STRING            */ ~
            wt$2,                        /* Window Type Code          */ ~
            wcode$1,                     /* Window Code for Screen    */ ~
            ph$(4%)4, skey$4,            /* Save Phantom Codes/Lookup */ ~
            sqty$2,                      /* Number of Screens         */ ~
            readkey$24,                  /* Generic Key               */ ~
            descr$30                     /*                           */

            raw_err$( 1%) = "No (HNYQUAN) Record "
            raw_err$( 2%) = "No (HNYMASTR) Record"
            raw_err$( 3%) = "No Units Defined    "
            raw_err$( 4%) = "No Unit Code Defined"
            raw_err$( 5%) = "Sq Foot Calc Error  "
            raw_err$( 6%) = "Sq Inch Calc Error  "
            raw_err$( 7%) = "Per Foot Calc Error "
            raw_err$( 8%) = "Per Inch Calc Error "
            raw_err$( 9%) = "N/A Unit Code Used  "
            raw_err$(10%) = "Per Pound Calc Error"
            raw_err$(11%) = "Per Ounce Calc Error"
            raw_err$(12%) = "Material Calc Error "
            raw_err$(13%) = "Screen Calc Lookup  "
            raw_err$(14%) = "Screen Phantom Load "
            raw_err$(15%) = "Glass/Screen Calc Er"
            raw_err$(16%) = "Max Glass Panels Exc"
            raw_err$(17%) = "Special Screen Error"
            raw_err$(18%) = "Available           "
            raw_err$(19%) = "Available           "
            raw_err$(20%) = "Available           "

           sz$ = "01/1601/8 03/1601/4 05/16 3/8 07/1601/2 09/1605/8 11/16~
        ~03/4 13/1607/8 15/16     "

        REM    = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        REM  3/4 13/16 7/8 15/16     "
           zz$ = ".0625.125 .1875.25  .3125.375 .4375.50  .5625.625 .6875~
        ~.75  .8125.875 .9375     "
             /* TAB% = 1%  (GLASS01)  - GLASS QUANTITIES FOR TOP/BOT    */
             /* TAB% = 2%  (GLASS02)  - TABLE FOR TOP GLASS ONLY        */
             /* TAB% = 3%  (GLASS03)  - TABLE FOR TWO BOTTOMS           */
             /* TAB% = 4%  (GLASS04)  - Table for no Glass Required     */
             /* TAB% = 5%  (GLASS05)  - TABLE FOR '8' SERIES WITH GLASS */
             /* TAB% = 6%  (SCREEN01) - TABLE FOR VALID TWO SCREEN MODS */
             /* TAB% = 7%  (SCREEN03) - TABLE SPEC SCREEN 1/3,1/3,1/3   */
             /* TAB% = 8%  (SCREEN04) - Master Screen Control Table     */

            tab$(1%) = "GLASS01  "   : tab$(6%) = "SCREEN01 "
            tab$(2%) = "GLASS02  "   : tab$(7%) = "SCREEN03 "
            tab$(3%) = "GLASS03  "   : tab$(8%) = "SCREEN04 "
            tab$(4%) = "GLASS04  "
            tab$(5%) = "GLASS05  "

            ss$( 1%) = "ANOH4001400300000000"
            ss$( 2%) = "ANOF4101410340004200"
            ss$( 3%) = "ACOH4001401300000000"
            ss$( 4%) = "ACOF4101410340004210"
            ss$( 5%) = "AORH4001402300000000"
            ss$( 6%) = "AORF4101410340004220"
            ss$( 7%) = "ASPH4001TABL00000000"      /* SCREEN06 - LEFT  */
            ss$( 8%) = "ASPF410141034000TABL"      /* SCREEN06 - RIGHT */

            ss$( 9%) = "BNOH4001400300000000"
            ss$(10%) = "BNOF410141034000CTRS"

            ss$(11%) = "CNO44001400300000000"      /* 1/4, 1/2, 1/4    */
            ss$(12%) = "CNO34004400300000000"      /* 1/3, 1/3, 1/3    */

            ss$(13%) = "DNOH4001400300000000"      /* GLASS01          */
            ss_max% = 13%

            gs_total, gs_total_s = 0.0
            gs_vinyl, gs_vinyl_s, gs_misc, gs_misc_s = 0.0
            gs_frt, gs_vinyl_d, raw_frt, raw_vinyl = 0.0
            mat gs_qty = zer     : mat gs_cost = zer
            mat gs_units% = zer  : mat gs_unit   = zer
            mat gs_qty_s = zer   : mat gs_cost_s = zer

            s_width, s_height = 0.0
            gs_err% = 0%
            init(" ") gs_raw$(), gs_desc$(), rm_eq$(), rm_ph$()

            mm$ = str(part$,1%,1%)
            p% = pos("012345678" = mm$)
            if p% = 0% then goto exit_program
            if len(part$) < 19% then goto exit_program
               gosub convert_fields

               if mm$ <> "8" then goto L02240
                  tab% = 5% : gosub check_table  /* Valid 800 Series   */
                  if code% = 0% then goto exit_program
L02240: REM - BEGIN CALCULATIONS
            gosub scrap_pcnt                     /* Load Scrap Percent */
            if typ% <> 0% then goto L02350
               if gl$ = "00" then goto exit_program        /* NO GLASS */
                  tab% = 4% : gosub check_table  /* No Glass Required  */
                  if code% = 1% then goto exit_program
               gosub non_insulated              /* Check Glass         */
               gosub calc_glass                 /* Calc Standard Glass */
               gosub calc_glass_cost            /* Cost Standard Glass */
               goto exit_program

L02350:     p1% = pos("12389BCD" = sc$)         /* No Screen Skip Part */
            if p1% = 0% then goto exit_program
            gosub calc_screen
            gosub calc_screen_cost
            goto exit_program

        calc_glass
               gs_cnt% = 0%
               t1% = 1% : b1% = 1%
               if mm$ = "8" then gosub case_glass

                                                    /*    (EWD002)       */
                                                    /* One Top, One Bot  */
               if model$ <> "312" and model$ <> "332" then goto L02490  
                  if hg$ <> "42" then goto L02490
                     t1% = 1% : b1% = 2%
                                                    /* Two Tops, One Bot */
L02490:        if model$ <> "313" and model$ <> "333" then goto L02510  
                  t1% = 2% : b1% = 1%
                                                   /* Two Tops, Two Bots */
L02510:        if model$ <> "314" and model$ <> "334" then goto L02540  
                  t1% = 2% : b1% = 2%

L02540:   for k% = 1% to t1%
               view$ = " " : cal% = 1%           /* Check Bottom Only  */
               if sc$ = "5" then t1% = 0%
                 if t1% = 0% then goto L02900
                  view$ = "TOP"  : cal% = 1%
                  gosub lookup_phantom_glass            /* Top Glass   */
               gosub calc_double
               gs_cnt%  = gs_cnt% + 1%                  /* TOT GLASS-1 */
               if gs_cnt% > 9% then gosub freeze_cnt
               gs_qty(gs_cnt%) = round( width * height, 4)
               rm_eq$(gs_cnt%) = "NA "          /* Eq. No. not Applic. */
               rm_ph$(gs_cnt%) = str(phantom$,1%,5%)  /* Phantom Number*/

               if insulated% = 1% then goto L02820      /* NON-INSULATED */

                  gs_cnt% = gs_cnt% + 1%                /* TOT GLASS-2 */
                  if gs_cnt% > 9% then gosub freeze_cnt
                  gs_qty(gs_cnt%) = round( width * height, 4)
                  rm_eq$(gs_cnt%) = "NA "       /* Eq. No. not Applic. */
                  rm_ph$(gs_cnt%) = str(phantom$,1%,5%)   /*Phantom No.*/

                  gs_cnt% = gs_cnt% + 1%                /* TOP SWIG - 1*/
                  if gs_cnt% > 9% then gosub freeze_cnt
                  gs_qty(gs_cnt%) =                                      ~
                              round( ((width + height)*2.0) / 12.0, 4)
                  rm_eq$(gs_cnt%) = "NA "       /* Eq. No. not Applic. */
                  rm_ph$(gs_cnt%) = str(phantom$,1%,5%)  /*Phantom No. */

L02820:        gosub calc_raw_material
               gosub calc_stock
          next k%

                  tab% = 2% : gosub check_table  /* Check for No Bottom*/
                  if code% = 1% then return
                  if sc$ = "4" or sc$ = "6" then return /* Top Only    */

L02900:   for k% = 1% to b1%
              if b1% = 0% then return
                     view$ = "BOT"
                     gosub lookup_phantom_glass
               gosub calc_double
               gs_cnt%  = gs_cnt% + 1%                  /* BOT GLASS-3 */
               if gs_cnt% > 9% then gosub freeze_cnt
               gs_qty(gs_cnt%) = round( width * height, 4)
               rm_eq$(gs_cnt%) = "NA "          /* Eq. No. not Applic. */
               rm_ph$(gs_cnt%) = str(phantom$,1%,5%)  /* Phantom Number*/

               if insulated% = 1% then goto L03160      /* NON-INSULATED */

                  gs_cnt% = gs_cnt% + 1%                /* BOT GLASS-4 */
                  if gs_cnt% > 9% then gosub freeze_cnt
                  gs_qty(gs_cnt%) = round( width * height, 4)
                  rm_eq$(gs_cnt%) = "NA "       /* Eq. No. not Applic. */
                  rm_ph$(gs_cnt%) = str(phantom$,1%,5%)  /*Phantom No. */

                  gs_cnt% = gs_cnt% + 1%                /* BOT SWIG -2 */
                  if gs_cnt% > 9% then gosub freeze_cnt
                  gs_qty(gs_cnt%) =                                      ~
                                 round( ((width + height)*2.0) / 12.0, 4)
                  rm_eq$(gs_cnt%) = "NA "       /* Eq. No. not Applic. */
                  rm_ph$(gs_cnt%) = str(phantom$,1%,5%)  /*Phantom No. */

L03160:        gosub calc_raw_material
               gosub calc_stock
                     if sc$ = "5" then return
                        tab% = 3% : gosub check_table   /* For Two Bots*/
                        if code% = 0% then goto L03460

               gosub calc_double
               gs_cnt%  = gs_cnt% + 1%                  /* 2ND BOTT -5 */
               if gs_cnt% > 9% then gosub freeze_cnt
               gs_qty(gs_cnt%) = round( width * height, 4)
               rm_eq$(gs_cnt%) = "NA "          /* Eq. No. not Applic. */
               rm_ph$(gs_cnt%) = str(phantom$,1%,5%)     /*Phantom No. */

               if insulated% = 1% then goto L03440      /* NON-INSULATED */

                  gs_cnt% = gs_cnt% + 1%                /* 2ND BOTT -6 */
                  if gs_cnt% > 9% then gosub freeze_cnt
                  gs_qty(gs_cnt%) = round( width * height, 4)
                  rm_eq$(gs_cnt%) = "NA "       /* Eq. No. not Applic. */
                  rm_ph$(gs_cnt%) = str(phantom$,1%,5%)  /*Phantom No. */

                  gs_cnt% = gs_cnt% + 1%                /* BOT SWIG -3 */
                  if gs_cnt% > 9% then gosub freeze_cnt
                  gs_qty(gs_cnt%) =                                      ~
                                 round( ((width + height)*2.0) / 12.0, 4)
                  rm_eq$(gs_cnt%) = "NA "       /* Eq. No. not Applic. */
                  rm_ph$(gs_cnt%) = str(phantom$,1%,5%)  /*Phantom No. */

L03440:        gosub calc_raw_material
               gosub calc_stock
L03460:   next k%
        return

        calc_double                      /* Calc Double Strength Glass */
          if ty% = 6% or ty% = 7% or ty% = 10% or ty% = 12% or           ~
             ty% = 15% then return       /* DOUBLE STRENGTH/TEMP N/A   */

          gosub lookup_double
          x, y, z = 0.0
          x = width
          y = height
          if double% <> 0% then goto calc_tempered

          w = x - int(x)
          if w > 0.0 then x = x + 1.0
          w = y - int(y)
          if w > 0.0 then y = y + 1.0
          z = (x * y)/ 144.0
          if z > 20.0 then goto L03690                /* Square Feet     */
          if x > 60.0 then goto L03690                /* Width Greater   */
          if y > 60.0 then goto L03690                /* Height Greater  */
          if (x + y) > 100.0 then goto L03690         /* Tot United Inch */
          goto calc_tempered
L03690:      double% = 1%

        calc_tempered
          gosub lookup_temp
          if x > 48 or y > 48 then tempered% = 1%
        return

        convert_sixteen
              calc = round( calc, 4 ) : calc_out$, calc$ = " "
              calc$ = "00000"
              a% = int(calc) : b% = int((calc - a%) * 10000)
              if b% = 0% then goto L03860                /****************/
                 d% = int(b%/625)                      /* Conversion of*/
                 if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                 b% = d%                               /*  Sixteenth's */
                 if b% <> 16% then goto L03860           /****************/
                    a% = a% + 1% : b% = 0%          /* A% = Whole Part */
L03860:       convert a% to str(calc$,1%,3%), pic(000)
              calc_out$ = str(calc$,1%,3%) & ".0000"
              if b% = 0% then goto L03910
                 str(calc$,4%,2%) = str(sz$,(b%*5%) - 4%, 2% )
                 str(calc_out$,4%,5%) = str(zz$,(b%*5%) - 4%, 5% )
L03910:       calc_out = 0.0
              convert calc_out$ to calc_out, data goto L03930
L03930:
        return

        calc_stock
                                                    /* DISABLE FOR NOW */
         return

        calc_raw_material
REM         if str(model$,1%,1%) = "1" then goto L04090  /*  EWD001 */
               if insulated% = 0% then goto L04060
                  gs_units%(gs_cnt%) = 3%     /* SQ INCH GLASS         */
                  goto L04090                   /* NON_INSULATED GLASS   */

L04060:        gs_units%(gs_cnt%-2%) = 3%     /* SQ INCH GLASS - IN    */
               gs_units%(gs_cnt%-1%) = 3%     /* SQ INCH GLASS - OUT   */
               gs_units%(gs_cnt%)    = 4%     /* PER FOOT SWIGGLE      */
L04090:     readkey$ = " "
            str(readkey$,1%,9%) = "COST 01GM"    /* GLASS RAW MATERIAL */
                                              /* DOUBLE STRENGTH GLASS */
            if double% <> 0% then str(readkey$,1%,9%) = "COST 02GM"
            str(readkey$,10%,15%) = gl$
            read #3,key = readkey$, using L04150, descr$, eod goto L04290
L04150:         FMT POS(25), CH(30)
REM         if str(model$,1%,1%) <> "1" then goto L04220  /*  EWD001 */
               goto L04220                                /*  EWD001 */
               gs_raw$(1%) = str(descr$,1%,14%)
               if model$ = "100" then return
                  gs_raw$(2%) = str(descr$,16%,14%)
                  return

L04220:     if insulated% = 0% then goto L04260
               gs_raw$(gs_cnt%) = str(descr$,1%,14%)  /* NON-INSULATED */
               return                                 /* NO SWIGGLE    */

L04260:     gs_raw$(gs_cnt%-2%) = str(descr$,1%,14%)
            gs_raw$(gs_cnt%-1%) = str(descr$,16%,14%)

L04290:        readkey$ = " "
               str(readkey$,1%,9%) = "COST 01GS" /* GLASS SWIGGLE      */
                                             /* SPECIAL TEMPERED GLASS */
               if tempered% = 1% then str(readkey$,1%,9%) = "COST 02GS"

               str(readkey$,10%,15%) = model$
               read #3,key = readkey$, using L04150, descr$, eod goto L04420
               gs_raw$(gs_cnt%) = str(descr$,1%,10%)
          if double% = 1% then gs_raw$(gs_cnt%)   = str(descr$,12%,10%)
          if tempered% = 1% then gs_raw$(gs_cnt%) = str(descr$,1%,10%)
        return

        calc_glass_cost
L04420:  for i% = 1% to gs_cnt%
            vv$ = str(gs_raw$(i%),2%,3%)
            call "APCCST9B" (gs_raw$(i%), gs_qty(i%), gs_units%(i%),     ~
                             gs_cost(i%), gs_desc$(i%), gs_unit(i%),     ~
                             raw_frt, raw_vinyl, #5, #6, raw_err% )
            if raw_err% <> 0% then gs_desc$(i%) = raw_err$(raw_err%)
            gs_qty_s(i%)  = round( gs_qty(i%)  * gls_scrp, 4)
            gs_cost_s(i%) = round( gs_cost(i%) * gls_scrp, 4)
            if vv$ <> "105" and vv$ <> "110" and vv$ <> "115" then       ~
                                                          goto L04550
               gs_vinyl   = round(gs_vinyl + gs_cost(i%), 4)
               gs_vinyl_s = round(gs_vinyl_s + gs_cost_s(i%), 4)
               goto L04580
L04550:     gs_misc   = round(gs_misc + gs_cost(i%), 4)
            gs_misc_s = round(gs_misc_s + gs_cost_s(i%), 4)

L04580:     gs_total   = round(gs_total + gs_cost(i%), 4)
            gs_total_s = round(gs_total_s + gs_cost_s(i%), 4)
            gs_frt     = round(gs_frt + raw_frt, 5)
            gs_vinyl_d = round(gs_vinyl_d + raw_vinyl, 5)
         next i%
        return

        calc_screen
            gosub check_screen
            if check% = 0% then goto exit_program
            gosub load_phantom
            sav_width_1 = 0.0 : sav_height = 0.0
            str(phantom$,1%,4%) = ph$(1%)
            view$ = "   "
            cal% = 2% : gosub lookup_phantom_a
            sav_width_1 = width
            rm_eq$(1%) = "NA "                  /* Eq. No. not Applic. */
            rm_ph$(1%) = str(phantom$,1%,5%)             /*Phantom No. */
            rm_eq$(2%), rm_eq$(3%), rm_eq$(7%) = rm_eq$(1%)
            rm_ph$(2%), rm_ph$(3%), rm_ph$(7%) = rm_ph$(1%)

            cal% = 3%
            str(phantom$,1%,4%) = ph$(2%)
            if str(ph$(2%),1%,4%) = "TABL" then gosub spec_half          ~
                                           else gosub lookup_phantom_a
            sav_height = height
            width = sav_width_1
            rm_eq$(4%) = "NA "                  /* Eq. No. not Applic. */
            rm_ph$(4%) = str(phantom$,1%,5%)             /*Phantom No. */
            rm_eq$(5%), rm_eq$(6%) = rm_eq$(4%)
            rm_ph$(5%), rm_ph$(6%) = rm_ph$(4%)

          gs_x% = 1.0
          if sqty% <> 0%  then gs_x% = sqty%   /* Nunber of Screens    */

          gs_qty(1%) = round( (width * height) / 144.0, 4)  /* SQ FOOT */
                                               /* Test for Full Screen */
          gs_qty(2%) = width
          gs_qty(3%) = width
          gs_qty(4%) = height
          gs_qty(5%) = height
          gs_qty(6%) = round( ((width + height) * 2.0) / 12.0, 4)
          gs_qty(7%) = width
          if sc$ <> "2" and sc$ <> "C" then goto L05100
             cal% = 3%                         /* Full Screen          */
             str(phantom$,1%,4%) = ph$(4%)
             if str(ph$(4%),1%,4%) = "TABL" then gosub spec_full         ~
                                            else gosub lookup_phantom_a
             gs_qty(7%) = height
             rm_eq$(7%) = rm_eq$(4%)
             rm_ph$(7%) = rm_ph$(4%)

L05100:   for i% = 1% to 6%
              gs_qty(i%) = gs_qty(i%) * gs_x%
          next i%

        return

        spec_half                                    /* MOD - 06/04/97 */
             height =((s_height/2.0) - s_half)                           ~
                                             + ((s_height/2.0) - s_clmr )
             init(" ") phantom$
        return
        spec_full
             height =((s_height/2.0) - s_full)                           ~
                                            - ((s_height/2.0)  - s_clmr )
             init(" ") phantom$
        return

        calc_screen_cost
           gs_cnt% = 7%
           gs_units%(1%) = 2%                               /* SQ FOOT */
           gs_units%(2%) = 5%                               /* PER INCH*/
           gs_units%(3%) = 5%
           gs_units%(4%) = 5%
           gs_units%(5%) = 5%
           gs_units%(6%) = 4%                               /* PER FOOT*/
           gs_units%(7%) = 5%                               /* PER FOOT*/
           cl1$ = "0"
           for i% = 0% to 6%
              convert i% to i$, pic(0)
              if i% <> 0% then cl1$ = cl$
              readkey$ = " "
              str(readkey$,1%,9%)   = "COST 02SC"
              str(readkey$,10%,15%) = model$ & cl1$ & i$
              read #3,key = readkey$, using L04150, descr$, eod goto L05460
              gs_raw$(i%+1%) = str(descr$,1%,10%)
           next i%
L05460:
           for i% = 1% to gs_cnt%
              vv$ = str(gs_raw$(i%),2%,3%)
              if i% <> 7% then goto L05520
                 if sc$ <> "2" then gs_qty(i%) = 0.0  /* No Crossbar   */

L05520:       if i% <> 1% then goto L05570           /* No Screen Mesh   */
                 if mm$ <> "5" then goto L05570      /* Only Vinyl Prime */
                    if cl$ <> "1" then goto L05570   /* Only MIL Finish  */
                       gs_qty(i%) = 0.0

L05570:       call "APCCST9B" (gs_raw$(i%), gs_qty(i%), gs_units%(i%),   ~
                               gs_cost(i%), gs_desc$(i%), gs_unit(i%),   ~
                               raw_frt, raw_vinyl, #5, #6, raw_err% )
              if raw_err% <> 0% then gs_desc$(i%) = raw_err$(raw_err%)
           gs_qty_s(i%)  = round( gs_qty(i%)  * scr_scrp, 4)
           gs_cost_s(i%) = round( gs_cost(i%) * scr_scrp, 4)
           if vv$ <> "105" and vv$ <> "110" and vv$ <> "115" then        ~
                                                             goto L05680
              gs_vinyl   = round(gs_vinyl + gs_cost(i%), 4)
              gs_vinyl_s = round(gs_vinyl_s + gs_cost_s(i%), 4)
              goto L05710
L05680:    gs_misc   = round(gs_misc + gs_cost(i%), 4)
           gs_misc_s = round(gs_misc_s + gs_cost_s(i%), 4)

L05710:    gs_total   = round(gs_total + gs_cost(i%), 4)
           gs_total_s = round(gs_total_s + gs_cost_s(i%), 4)
           gs_frt     = round(gs_frt + raw_frt, 5)
           gs_vinyl_d = round(gs_vinyl_d + raw_vinyl, 5)
           next i%
           str(gs_desc$(1%),29%,4%) = "-SCR"
           str(gs_desc$(2%),29%,4%) = "-WD1"
           str(gs_desc$(3%),29%,4%) = "-WD1"
           str(gs_desc$(4%),29%,4%) = "-HT1"
           str(gs_desc$(5%),29%,4%) = "-HT1"
           str(gs_desc$(6%),29%,4%) = "-SPL"
           str(gs_desc$(7%),29%,4%) = "-CB "
        return

        check_screen                     /* Done once per MFG Product  */
            init(" ") readkey$, wt$, wcode$, skey$, sqty$
            check% = 0% : sqty% = 1%
            str(readkey$,1%,9%)   = tab$(8%)    /* Scrn Master Control */
            str(readkey$,10%,15%) = model$      /* Product Code        */
            read #3,key = readkey$,using L05920 , wt$, wcode$,             ~
                                                     eod goto L06240
L05920:       FMT POS(25), CH(3), XX(3), CH(1) /* WT$ - Window Type    */
                                               /* WCODE$ - A thru E    */
            if wcode$ = "E" then return        /* No Screen Required   */

            str(skey$,1%,1%) = wcode$          /* Set Window Type Code */
            str(skey$,2%,2%) = "NO"            /* Set to (NO)rmal      */
                                               /* Check Hinge Code     */
            if wcode$ <> "A" then goto L06030    /* 1st Check Type 'A'   */
               if str(hgl$,1%,2%) = "CO" or str(hgl$,1%,2%) = "OR" then  ~
                                    str(skey$,2%,2%) = str(hgl$,1%,2%)

L06030:     str(skey$,4%,1%) = "H"             /* Set to Half Screen   */
            if sc$ = "2" then str(skey$,4%,1%) = "F"  /* Full Screen   */
            if sc$ = "C" then str(skey$,4%,1%) = "F"  /* Full Screen   */
            if wcode$ <> "C" then goto L06110
               sqty% = 2%
               str(skey$,4%,1%) = "4"          /* Special 1/4,1/2,1/4  */
               if triple% <> 0% then str(skey$,4%,1%) = "3" /* Special */

L06110:     if wcode$ <> "D" then goto L06220    /* Multiple Screens     */
               str(skey$,4%,1%) = "H"          /* Half Screen Only     */
               init(" ") readkey$
               str(readkey$,1%,9%)   = tab$(1%)    /* Screen Quantities*/
               str(readkey$,10%,3%)  = model$      /* Product Code     */
               str(readkey$,13%,12%) = hg$         /* Hinge Code       */
               read #3,key = readkey$, using L06180 , sqty$, eod goto L06220
L06180:           FMT POS(28), CH(2)
               convert sqty$ to sqty%, data goto L06200
L06200:
            if sqty% = 0% then return          /* No Screen Required   */
L06220: check% = 1%
        return
L06240:     gs_err% = 13%
        return

        load_phantom                         /* Find Phantom's for Calc*/
            if s_clmr <> 0.0 then gosub get_special
            for ss% = 1% to ss_max%
                if str(ss$(ss%),1%,4%) = skey$ then goto L06340
            next ss%
        gs_err% = 14%                        /* Product Error for MFG  */
        return
L06340:     ph$(1%) = str(ss$(ss%),5%,4%)     /* Cut Width Calc        */
            ph$(2%) = str(ss$(ss%),9%,4%)     /* Cut Height Calc       */
            ph$(3%) = str(ss$(ss%),13%,4%)    /* CB Cut Length         */
            ph$(4%) = str(ss$(ss%),17%,4%)    /* CB Cut Location       */
        return                                /* Note '0000' = N/A     */

        lookup_phantom_a
            width = 0.0 : height = 0.0         /* 1% -Width and Height */
            err%  = 0%                         /* 2% -Width Only       */
                                               /* 3% -Height Only      */
            if str(phantom$,1%,4%) = "0000" then goto L06550
                  call "APCCALSB" (cal%,           /* Calc Type 1%,2%,3*/~
                                   part$,          /* Part Number      */~
                                   0%, 0%, 0%,     /* (CUT001) */        ~
                                   phantom$,       /* Phantom Designato*/~
                                   width,          /* Exact Width Dec  */~
                                   height,         /* Exact Height     */~
                                   #2,             /* AMTBOMCD Eq File */~
                                   err% )          /* Error Code 0%-Ok */
            calc = width  : gosub convert_sixteen : width  = calc_out
            calc = height : gosub convert_sixteen : height = calc_out
        return
L06550:     gs_err% = 15%
        return

        lookup_phantom_glass                 /* Special Code for Model */
           if view$ <> "TOP" then goto L06660  /* 830, 883               */
              phantom$ = "2008"
              if str(hgl$,1%,2%) = "OR"  then phantom$ = "2203"
              if str(hgl$,1%,2%) = "CO"  then phantom$ = "2103"
              if str(hgl$,1%,3%) = "1/4" then phantom$ = "2008"
              if str(hgl$,1%,3%) = "1/3" then phantom$ = "2108"
              goto L06710
L06660:    phantom$ = "3008"
           if str(hgl$,1%,2%) = "OR"  then phantom$ = "3203"
           if str(hgl$,1%,2%) = "CO"  then phantom$ = "3103"
           if str(hgl$,1%,3%) = "1/4" then phantom$ = "3008"
           if str(hgl$,1%,3%) = "1/3" then phantom$ = "3108"
L06710: gosub lookup_phantom_a    /* CAL% 1%=W/H, 2%=W Only, 3%=H Only */

        return

        convert_fields
            double% = 0%            /* DOUBLE%=0%,DOUBLE STRENGTH = 1% */
            tempered% = 0%
            s_width = 0.0 : s_height = 0.0 : s_clmr = 0.0
            s_half  = 0.0 : s_full   = 0.0
            model$   = str(part$,1%,3%)                /* Model Number */
            cl$      = str(part$,4%,1%)                /* Color        */
            gl$      = str(part$,5%,2%)                /* Glass        */
            lt$      = str(part$,7%,2%)                /* Liting       */
            hg$      = str(part$,9%,2%)                /* Hinge        */
            sc$      = str(part$,11%,1%)               /* Screen       */
            lk$      = str(part$,12%,1%)               /* Locks        */
            width$   = str(part$,13%,4%)               /* Width        */
            height$  = str(part$,17%,3%)               /* Height       */
            clmr$    = str(part$,20%,3%)               /* CLMR         */
            wallwidt$= str(part$,23%,3%)               /* WALLWIDT     */
            ty% = 0%
            convert gl$ to ty%, data goto L06930
L06930:
               a1, a2 = 0.0
               convert str(width$,1%,3%) to a1, data goto L06960
L06960:
               convert str(width$,4%,1%) to a2, data goto L06980
L06980:
               s_width = a1 + (a2/8.0)
               a1, a2 = 0.0
               convert str(height$,1%,2%) to a1, data goto L07020
L07020:
               convert str(height$,3%,1%) to a2, data goto L07040
L07040:
               s_height = a1 + (a2/8.0)
               a1, a2 = 0.0
               gosub lookup_hinge
            if len(part$) < 22 then return
               convert str(clmr$,1%,2%) to a1, data goto L07125

               convert str(clmr$,3%,1%) to a2, data goto L07120
L07120:
L07125:        s_clmr = a1 + (a2/8.0)
               if s_clmr <= 8.0 then  s_clmr = 0.0    /* Wood Surround */
        return

        scrap_pcnt                           /* Load Scrap Percentages */
            gls_scrp = 0.0 : scr_scrp = 0.0
            init(" ") readkey$
            str(readkey$,1%,9%) = "COST 01SP"
            str(readkey$,10%,15%) = str(part$,1%,4%) /* MODEL AN COLOR */
            read #3,key = readkey$,using L07230 , descr$,eod goto L07300
L07230:        FMT POS(25), CH(30)
            convert str(descr$,11%,9%)  to gls_scrp, data goto L07250
L07250:
            convert str(descr$,21%,9%)  to scr_scrp, data goto L07270
L07270:
            gls_scrp = round( gls_scrp / 100.0, 4)
            scr_scrp = round( scr_scrp / 100.0, 4)
L07300: return

        non_insulated
            insulated% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)    = "COST 01SG"      /* NON-INSULATED */
            str(readkey$,10%,15%)  = model$           /* MODEL CODE    */
            read #3,key = readkey$, eod goto L07400

            insulated% = 1%
L07400: return

        case_glass
            str(readkey$,1%,9%)   = "GLASS01  "
            str(readkey$,10%,3%)  = model$
            str(readkey$,13%,12%) = str(part$,9%,2%)
            read #3,key = readkey$, using L07470, descr$, eod goto L07530
L07470:        FMT POS(25), CH(30)

               convert str(descr$,1%,2%) to t1%, data goto L07500
L07500:
               convert str(descr$,4%,2%) to b1%, data goto L07520
L07520:
L07530: return

        freeze_cnt
            gs_cnt% = 9%
            gs_err% = 16%
        return

        check_table                   /* Check Glass and Screen Tables */
            code% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = tab$(tab%)
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$, eod goto L07670
            code% = 1%
L07670: return

        lookup_temp
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN TEMP"
            str(readkey$,10%,15%) = gl$
            read #3,key = readkey$, eod goto L07750
               tempered% = 1%
L07750: return
        lookup_double
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN DBLE"
            str(readkey$,10%,15%) = gl$
            read #3,key = readkey$, eod goto L07820
               double% = 1%
L07820: return
        lookup_hinge                                  /* Look Up Hinge */
            triple% = 0%
            init(" ") hgl$, descr$, readkey$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hg$
            read #3,key = readkey$,using L07890 , descr$,eod goto L07980
L07890:        FMT POS(25), CH(30)
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            hgl$ = str(descr$,1%,p% - 1%)    /* Left Side Description  */
            if model$ <> "830" and model$ <> "883" then goto L07970
               p1% = pos(descr$ = "/")
               if p1% = 0% then return
               hgl$ = str(descr$,p1%-1%,3%)
L07970:     if str(hgl$,1%,3%) = "1/3" then triple% = 1%
L07980: return

        get_special                           /* Specified Meeting Rail*/
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = "SCREEN06 "
            str(readkey$,10%,15%) = model$
            read #3,key = readkey$, using L08050 , descr$, eod goto L08130
L08050:        FMT POS(25), CH(30)
            convert str(descr$,1%,9%) to s_half, data goto L08070
L08070:
            convert str(descr$,11%,9%) to s_full, data goto L08090
L08090:
                                              /* Specified Ctr Bar     */
            str(skey$,2%,2%) = "SP"           /* 'SP' = Specified      */
        return
L08130:     gs_err% = 17%
        return

        exit_program
            if gs_err% <> 0% then gs_desc$(9%) = raw_err$(gs_err%)
        end

