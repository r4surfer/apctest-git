************************************************************************~
*                           ( As of 11/11/97 - RHH )                   *~
*                          ( Last Mod Date - 07/14/00 - CMG )          *~
*        ( Same as (APCCSTRM) with Exception to return RM_EQ$() and    *~
*                             RM_PH$() )                               *~
*        APCCST1B - (Pass 1) Calculate the Cost of all Raw Material    *~
*                   Cuts for Specified Window Passed to Linealmate     *~
*                   ( Types 1 and 2 ). Also CHECK_OBSOLETE Routine     *~
*                   for Replacing Old Raw Material Parts.              *~
*                                                                      *~
*                   (Pass 2) Calculate the Cost of Misc. Raw Material  *~
*                   Parts (Types A and B ) which are not Passed to     *~
*                   Linealmate.                                        *~
*                                                                      *~
*                   (Pass 3) Calculate the Cost of Grid/Liting Raw     *~
*                   Materials. Also Misc. Eq. Types C and D.           *~
*                   ( Use New Subroutine 'APCGSLIT' for Grid an Lites  *~
*                                                                      *~
*                   (Pass 4) Calculate the Cost of Pads Associated     *~
*                   with Grid and Liting.                              *~
*                   ( Table = COST 01PD ) Use Code '00' to get the     *~
*                     Raw Material Numbers for Pads/Fin Cross.         *~
*                     Calc. Pads Using Verticals/Horizontals and the   *~
*                     Number of Lits (LL%).                            *~
*                   ( Mod to Check for Top Sash, Bottom Sash, and      *~
*                     Fixed Glass only when doing Pads. )              *~
*                                                                      *~
*                   (Pass 5) Calculate the Number of Low-E and Argon   *~
*                     Labels needed. [ CHECK_LOWE_ARGON ]              *~
*                                                                      *~
*                   (Pass 6) Check for Stock and see if Shrink Wrap    *~
*                     is needed. [ CHECK_SHRINK_WRAP ]                 *~
*                                                                      *~
*                   (Pass 7) Calculate the Amount of Scrap Material    *~
*                   and the Cost Associated with each Raw Material.    *~
*                   ( Table = COST 01SP )                              *~
*                                                                      *~
*                   Note - (1) Uses Subroutine (APCCST9B) to Calculate *~
*                              the Unit Cost Associated with a Raw     *~
*                              Material.                               *~
*                                                                      *~
*                        - (2) Rebar Calc. The Array's P_BW$, P_BH$    *~
*                              contain an '*' which turns the Applic.  *~
*                              Equation Off. Based on the Width and    *~
*                              Heighth criteria the Equation is Turned *~
*                              On. Only Done for Product Line (6)      *~
*                              Double Hung.                            *~
*                              Note - Coco always gets the max Rebar.  *~
*                                                                      *~
*                        - (3) Use new (APCCST9B) to Calc the Freight  *~
*                              Cost and the Vinyl Discount Amount.     *~
*                                                                      *~
*                     PART$         = MFG Part Number                  *~
*                     RM_PART$()    = Inventory Raw Material Part      *~
*                     RM_CUTS()     = Total Cuts Decimal Inches        *~
*                     RM_COST()     = Total Cost of Part Material      *~
*                     RM_DESC$()    = Raw Material Description         *~
*                     RM_UNITS%(),  = RAW MAT'L UNITS OF MEASURE       *~
*                     RM_UNIT(),    = RAW MAT'L Unit Cost              *~
*                     RM_VINYL      = Vinyl Raw Material Cost          *~
*                     RM_MISC       = Misc Raw Material Cost           *~
*                     RM_TOTAL      = Raw Material Total Cost          *~
*                     RM_CNT%       = Raw Material Count (Pieces)      *~
*                     RM_CUTS_S()   = Total Custs Decimal Inch Scrap   *~
*                     RM_COST_S()   = Total Cost of Part Scrap         *~
*                     RM_VINYL_S    = Vinyl Raw Material Cost Scrap    *~
*                     RM_MISC_S     = Misc Raw Material Cost Scrap     *~
*                     RM_TOTAL_S    = Raw Material Total Scrap         *~
*                     RM_EQ$()      = Save Calc Type and Equation No.  *~
*                     RM_PH$()      = Save Phantom Number              *~
*                     RM_ERR%       = 0% = Ok, Non 0% = Error          *~
*                     RM_FRT        = Save Freight Cost                *~
*                     RM_VINYL_D    = Save Vinyl Discount Amount       *~
*                                                                      *~
*                                                                      *~
*----------!----------------------------------------------------!------*~
*   DATE   !                MODIFICATION                        ! WHO  *~
*----------!----------------------------------------------------!------*~
* 03/27/98 ! y2k compliant                                      !  DJD *~
* 07/14/00 ! Check new cross-reference file for actual cut qtys !  CMG *~
*          !       because of gang cutting. (EWD001)            !      *~
*----------!----------------------------------------------------!------*~
************************************************************************

        sub "APCCST1B" (part$,           /* MFG Part Number           */ ~
                        rm_part$(),      /* Inv. Raw Material Part    */ ~
                        rm_cuts(),       /* Total Inches in Decimal   */ ~
                        rm_cost(),       /* Total Cost Raw Mat. Part  */ ~
                        rm_desc$(),      /* Raw Material Description  */ ~
                        rm_units%(),     /* RAW MAT'L UNITS OF MEASURE*/ ~
                        rm_unit(),       /* RAW MAT'L Unit Cost       */ ~
                        rm_vinyl,        /* Raw Material Vinyl Cost   */ ~
                        rm_misc,         /* Raw Material Misc  Cost   */ ~
                        rm_total,        /* Raw Material Total Cost   */ ~
                        rm_cnt%,         /* Raw Material Count        */ ~
                        rm_cuts_s(),     /* Total Inch Decimal Scrap  */ ~
                        rm_cost_s(),     /* Total Cost Raw Mat. Scrap */ ~
                        rm_vinyl_s,      /* Raw Mat'l Vinyl Cost Scrap*/ ~
                        rm_misc_s,       /* Raw Mat'l Misc  Cost Scrap*/ ~
                        rm_total_s,      /* Raw Mat'l Total Cost Scrap*/ ~
                        rm_eq$(),        /* Save Calc Type and Eq. No.*/ ~
                        rm_ph$(),        /* Save Phantom Number       */ ~
                        rm_frt,          /* Save Freight Cost         */ ~
                        rm_vinyl_d,      /* Save Vinyl Discount Amount*/ ~
                              #1,        /* (APCCUTEQ) - FILE         */ ~
                              #2,        /* (GENCODES) - File         */ ~
                              #3,        /* (HNYMASTR) - File         */ ~
                              #4,        /* (HNYQUAN ) - File         */ ~
                              #5,        /* (AMTBOMCD) - File         */ ~
                              #6,        /* (APCSTOCK) - File         */ ~
                        rm_err% )        /* 0% = Ok, Non 0% = Error   */

        dim part$25,                     /* MFG Part Number           */ ~
            raw$25,                      /* Raw Mat. Part No.         */ ~
            rm_part$(100%)10,            /* Inv. Raw Material         */ ~
            rm_desc$(100%)32,            /* Raw Material Desc         */ ~
            rm_cuts(100%),               /* Total Inches in Decimal   */ ~
            rm_cost(100%),               /* Total Cost Raw Material   */ ~
            rm_cuts_s(100%),             /* Total Inch Decimal Scrap  */ ~
            rm_cost_s(100%),             /* Total Cost Raw Mat. Scrap */ ~
            rm_units%(100%),             /* Raw Material Units        */ ~
            rm_unit(100),                /* Raw Material Unit Cost    */ ~
            rm_eq$(100%)3,               /* Save Type and Equation No.*/ ~
            rm_ph$(100%)5,               /* Save Eq. Phantom Code     */ ~
            eq$(100%)8,                  /* Equation Codes            */ ~
            ct$(100%)9,                  /* Cut Widths and Heights    */ ~
            cr$(100%)10,                 /* Cut Raw Material          */ ~
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece (Y) or (N)      */ ~
            co$(100%)25,                 /* Cut Description           */ ~
            ct(100%),                    /* Calc Cut in Decimal Inch  */ ~
            sh$(100%)1,                  /* Sash Type (W, H, N)       */ ~
            ph$(100%)5,                  /* Save Phantom Numbers      */ ~
            raw_err$(20%)20,             /* Error Messages            */ ~
            tw$1,                        /* Width Type Code           */ ~
            th$1,                        /* Height Type Code          */ ~
            lt$2,                        /* Grid/Liting               */ ~
            cl$1,                        /* Color                     */ ~
            g_l$(4%)2,                   /* Grid/Liting Quantities    */ ~
            muttin$8,                    /* Glass Muttin Code         */ ~
            ll$1,                        /* Number of Lits 1, 2, 3    */ ~
            vv$3, scr_prod$,             /* TEST RAW MAT'L FOR VINYL  */ ~
            p_bw$50, p_bh$50,            /* '*' Width and Height Eq.  */ ~
            mod$3,                       /* Model Code                */ ~
            sc$1, stock$1,               /* Screen Code / Stock Code  */ ~
            skip$(4%)1, tb$1, hg$1,      /* Use for Misc A,B,C,D      */ ~
            readkey$50, descr$30,        /* Generic Key               */ ~
                                         /* (EWD001)                  */ ~
            gc_key$31, gc_qty$2          /* Gang Cut Readkey & Qty    */

/* (EWD001) */

            select # 7, "EWDCSTGC",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  31

            call "OPENCHCK" (#7, x%, x%, 0%, "  ")                        

/* (EWD001) */

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
            raw_err$(18%) = "Number of Pads Error"
            raw_err$(19%) = "Available           "
            raw_err$(20%) = "Available           "


            init(" ") readkey$, rm_part$(), rm_desc$(), raw$, mod$,      ~
                      p_bw$, p_bh$
            init(" ") rm_eq$(), rm_ph$()

            rm_err%, x_err% = 0%          /* 1%=Mat,2%=Lit,3%=Equation */
            mat rm_cuts = zer    :  mat rm_cuts_s = zer
            mat rm_cost = zer    :  mat rm_cost_s = zer
            mat rm_units% = zer  :  mat rm_unit   = zer
            rm_vinyl, rm_vinyl_s = 0.0
            rm_misc,   rm_misc_s = 0.0
            rm_total, rm_total_s = 0.0
            rm_vinyl_d, rm_frt, raw_vinyl, raw_frt = 0.0
            rm_cnt%, cw%, ch% = 0%
            scr_prod$ = str(part$,1%,1%)
            mod$ = str(part$,1%,3%)
            cl$  = str(part$,4%,1%)
            sc$  = str(part$,11%,1%)
            w%, h% = 0%
            convert str(part$,13%,4%) to w%, data goto L01810
L01810:
            convert str(part$,17%,3%) to h%, data goto L01830
L01830:
            gosub lookup_hinge
        REM - Calc all Cut Raw Materials                  /* Pass (1)  */
            tw$ = "1" : th$ = "2"                         /* Unit% = 5 */
            k%, k_max% = 0%
            rh% = 0% : gosub get_pieces
            rm_cnt% = k_max%

        REM - Calc all Misc. Raw Materials                /* Pass (2)  */
            tw$ = "A" : th$ = "B"                         /* Unit% = 5 */
            rh% = 1% : gosub get_pieces
            rm_cnt% = k_max%
            t_unit% = rm_cnt%

            raw_err% = 0%
        REM - Calc Grid/Liting Material Cost              /* Pass (3)  */
            lt% = 0%                                      /* Unit% = 5 */
            lt$ = str(part$,7%,2%)                        /*Grid/Liting*/
            if lt$ = "00" then goto L02140
               lt% = 1%
               gosub lookup_grid
               if lt% = 0% then goto L02140                  /* Not Found*/

            tw$ = "C" : th$ = "D"
            rh% = 1% : gosub get_pieces
            rm_cnt% = k_max%
            t_unit% = rm_cnt%

        REM - Load Pads for Grid/Liting                   /* Pass (4)  */
            gosub load_pads                               /* Unit% = 1 */

L02140: REM - Check for Low-E and Argon Gas (Labels)      /* Pass (5)  */
            gosub check_lowe_argon                        /* Unit% = 1 */

        REM - Check for Shrink Wrap                       /* Pass (6)  */
            gosub check_shrink_wrap                       /* Unit% = 3 */

            raw_err% = 0%
        REM - Load Scrap Percentages                       /* Pass (7) */
            gosub scrap_pcnt
            for i% = 1% to rm_cnt%
                raw$ = rm_part$(i%)
                vv$ = str(raw$,2%,3%)
                rm_units%(i%) = 5%
                if i% > t_unit% then rm_units%(i%) = 1%
                if i% = stock% then rm_units%(i%) = 3%       /* Shrink */

                call "APCCST9B" (raw$, rm_cuts(i%), rm_units%(i%),       ~
                                 rm_cost(i%), rm_desc$(i%), rm_unit(i%), ~
                                 raw_frt, raw_vinyl, #4, #3, raw_err%)
                if raw_err% = 0% then goto L02360
                   rm_desc$(i%) = raw_err$(raw_err%)
                   x_err% = 1%                       /* Material Error */
L02360:         rm_total = round(rm_total + rm_cost(i%), 4)
                rm_cuts_s(i%) = round(rm_cuts(i%) * mat_scrp, 4)
                rm_cost_s(i%) = round(rm_cost(i%) * mat_scrp, 4)
                rm_total_s = round(rm_total_s + rm_cost_s(i%), 4)
                if vv$ <> "105" and vv$ <> "110" and vv$ <> "115" then   ~
                                                 goto L02450
                   rm_vinyl   = round(rm_vinyl + rm_cost(i%), 4)
                   rm_vinyl_s = round(rm_vinyl_s + rm_cost_s(i%), 4)
                   goto L02480
L02450:         rm_misc    = round(rm_misc + rm_cost(i%), 4)
                rm_misc_s  = round(rm_misc_s + rm_cost_s(i%), 4)

L02480:         rm_frt     = round(rm_frt + raw_frt, 4)
                rm_vinyl_d = round(rm_vinyl_d + raw_vinyl, 4)
            next i%

        goto exit_sub

        get_pieces
            mfg% = rh%                               /* RH% = 0% or 1% */
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err%)

            call "APCCSTCC" (part$, mfg%, cw%, ch%, eq$(), ct$(), cr$(), ~
                             cp$(), cc$(), co$(), ct(), sh$(), ph$(),    ~
                                           tw$, th$, #1, #5, #2, err%)
            if err% <> 0% then x_err% = 3%           /* Equation Error */
            if rh% = 1% then goto L02670
               if pos("46" = str(mod$,1%,1%)) <> 0 then                  ~
                                                   gosub check_rebar
               if str(mod$,1%,1%) = "7" then gosub check_rebar_7

L02670:     eq% = cw% + ch%
            for i% = 1% to eq%
                if rh% = 1% then gosub check_misc
                if cc$(i%) = " " then goto L02870       /* Skip - No Ref */
                   gosub check_obsolete
                   if tw$ = "C" then cp$(i%) = g_l$(i%)
                   p% = 1%                            /* Get No. Pieces*/
                   convert cp$(i%) to p%, data goto L02750
L02750:
                   gosub check_qty                    /* (EWD001) */
                   if k_max% = 0% then goto L02810
                   for k% = 1% to k_max%
                       if rm_part$(k%) <> cr$(i%) then goto L02800
                          goto L02860                 /* Found in Memory */
L02800:            next k%
L02810:            k_max% = k_max% + 1%
                   k% = k_max%
                   rm_part$(k%) = cr$(i%)             /* Raw Material  */
                   rm_eq$(k%)   = str(eq$(i%),6%,3%)  /* Type & Eq. No.*/
                   rm_ph$(k%)   = ph$(i%)             /* Eq. Phantom No*/
L02860:            rm_cuts(k%)  = rm_cuts(k%) + ( ct(i%) * p% )
L02870:     next i%
        return

        scrap_pcnt                           /* LOAD SCRAP PERCENTAGES */
            mat_scrp = 0.0
            readkey$ = " "
            str(readkey$,1%,9%) = "COST 01SP"
            str(readkey$,10%,15%) = str(part$,1%,4%) /* MODEL AN COLOR */
            read #2,key = readkey$,using L02960   , descr$,eod goto L03000
L02960:        FMT POS(25), CH(30)
            convert str(descr$,1%,9%)  to mat_scrp, data goto L02980
L02980:
            mat_scrp = round( mat_scrp / 100.0, 4)
L03000: return

        lookup_grid                           /* Load Grid Quantities  */
            er%, ll%, vert%, horz% = 0%
            g_l$(1%), g_l$(2%) = "0"          /* Top/Bot Width Qty     */
            g_l$(3%), g_l$(4%) = "0"          /* Top/Bot Height Qty    */
            call "APCGSLIT" (part$,           /* MFG Part Number       */~
                             muttin$,         /* Muttin Code           */~
                             ll$,             /* Number of Lits        */~
                             "T",             /* Lookup Top 1st        */~
                             vert%,           /* Number of Verticals   */~
                             horz%,           /* Number of Horizontals */~
                             #2,              /* (GENCODES) - Tables   */~
                             er% )            /* 0% = Ok               */
            if er% <> 0% then goto L03400
               convert horz% to g_l$(1%), pic(##)     /* Top Width 1st */

               convert vert% to g_l$(3%), pic(##)     /* Top Height    */

            v1% = vert%
            h1% = horz%
            call "APCGSLIT" (part$,           /* MFG Part Number       */~
                             muttin$,         /* Muttin Code           */~
                             ll$,             /* Number of Lits        */~
                             "B",             /* Lookup Top 1st        */~
                             vert%,           /* Number of Verticals   */~
                             horz%,           /* Number of Horizontals */~
                             #2,              /* (GENCODES) - Tables   */~
                             er% )            /* 0% = Ok               */
            if er% <> 0% then goto L03400
               convert horz% to g_l$(2%), pic(##)     /* Bot Width 2nd */

               convert vert% to g_l$(4%), pic(##)     /* Bot Height    */

            v2% = vert%
            h2% = horz%
            convert ll$ to ll%, data goto L03370
L03370:
            if ll% > 2% then ll% = 3%
        return
L03400:     lt% = 0%
            x_err% = 2%                         /* Liting Error        */
        return

        load_pads                               /* Load Liting Pads    */
            readkey$ = " "                      /* Obtail Raw Material */
            str(readkey$,1%,9%) = "COST 01PD"   /* Numbers from Code   */
            str(readkey$,10%,15%) = "000"       /* '00' Always         */
            read #2,key = readkey$,using L03490   , descr$,eod goto L03810
L03490:        FMT POS(25), CH(30)
            if sc$ = "4" then goto L03650            /* Bottom Sash Only */
               rm_cnt% = rm_cnt% + 1%              /* Top Panel        */
               i% = rm_cnt%
               rm_part$(i%) = str(descr$,1%,10%)
               rm_cuts(i%) = ((v1% + h1%) * 2.0 )     /* Calc No. Pads */
               rm_eq$(i%)  = "NA "                    /* Type & Eq. No.*/
               rm_ph$(i%)  = "Wired"                  /* Eq. Phantom No*/

               rm_cnt% = rm_cnt% + 1%
               i% = rm_cnt%
               rm_part$(i%) = str(descr$,15%,10%)
               rm_cuts(i%) = (v1% * h1%)           /* Calc Fin Crosses */
               rm_eq$(i%)  = "NA "                    /* Type & Eq. No.*/
               rm_ph$(i%)  = "Wired"                  /* Eq. Phantom No*/

L03650:     if sc$ = "4" or sc$ = "6" then goto L03800
               rm_cnt% = rm_cnt% + 1%              /* Bottom Panel      */
               i% = rm_cnt%
               rm_part$(i%) = str(descr$,1%,10%)
               rm_cuts(i%) = ((v2% + h2%) * 2.0 )     /* Calc Pads     */
               rm_eq$(i%)  = "NA "                    /* Type & Eq. No.*/
               rm_ph$(i%)  = "Wired"                  /* Eq. Phantom No*/

               rm_cnt% = rm_cnt% + 1%
               i% = rm_cnt%
               rm_part$(i%) = str(descr$,15%,10%)
               rm_cuts(i%) = (v2% * h2%)              /* Calc Fin Cros'*/
               rm_eq$(i%)  = "NA "                    /* Type & Eq. No.*/
               rm_ph$(i%)  = "Wired"                  /* Eq. Phantom No*/

L03800: return
L03810:     lt% = 0%
            x_err% = 13%
        return

        check_obsolete
          readkey$ = " "    /* When Found Replace with Valid Raw Mat'l */
          str(readkey$,1%,9%)   = "APCMATOBS"
          str(readkey$,10%,15%) = cr$(i%)
          read #2,key = readkey$, using L03900, cr$(i%), eod goto L03910
L03900:      FMT POS(25), CH(10)
L03910: return

        check_rebar
                                          /* 'O' = Equation Turned On. */
                                          /* '*' = Equation Turned Off.*/
                                          /* Note - All Rebar Off      */
                               /* Width Equations  - 23,26,27,28,29,30 */
        p_bw$ = "OOOOOOOOOOOOOOOOOOOOOO*OO*****OOOOOOOOOOOOOOOOOOOO"
                               /* Height Equations - 18,20,21,23,24,25 */
        p_bh$ = "OOOOOOOOOOOOOOOOO*O**O***OOOOOOOOOOOOOOOOOOOOOOOOO"
            gosub check_picture                   /* Picture Window    */
            if p_bar% = 1% then goto analyize_rebar

            gosub check_slider                    /* Horizintal Slider */
            if h_bar% = 1% then goto check_horizontal

            goto check_standard

        check_horizontal
           if h% > 237% then goto L04130                       /* Height */
              str(p_bh$,20%,1%) = "O"
              goto L04190
L04130:    if h% > 357% then goto L04160
              str(p_bh$,20%,2%) = "OO"
              goto L04190
L04160:    str(p_bh$,20%,2%) = "OO"
           str(p_bh$,24%,2%) = "OO"

L04190: REM IF CL$ = "5" THEN GOTO 4120
           if w% >= 600% then goto L04220                      /* Width  */
              goto L04240
L04220:    str(p_bw$,29%,2%) = "OO"

L04240:    goto analyize_rebar

        check_standard
           if w% > 237% then goto L04300                       /* Width  */
              str(p_bw$,23%,1%) = "O"
              goto L04370
L04300:    if w% > 357% then goto L04340
              str(p_bw$,23%,1%) = "O"
              str(p_bw$,28%,1%) = "O"
              goto L04370
L04340:    str(p_bw$,23%,1%) = "O"
           str(p_bw$,26%,3%) = "OOO"

L04370: REM  IF CL$ = "5" THEN GOTO 4310
           if h% >= 600% then goto L04400                      /* Height */
              goto analyize_rebar
L04400:    str(p_bh$,18%,1%) = "O"
           str(p_bh$,23%,1%) = "O"

        analyize_rebar
           for i% = 1% to cw%                                /* Width  */
               if str(p_bw$,i%,1%) = "*" then cc$(i%) = " "
           next i%
           j% = cw%                                          /* Height */
           for i% = 1% to ch%
               if str(p_bh$,i%,1%) = "*" then cc$(j%+i%) = " "
           next i%
        return

        check_rebar_7
           if w% > 27 then return
              cc$(14%) = " "
        return

        check_slider
          readkey$ = " " : h_bar% = 0%
          str(readkey$,1%,9%)   = "COST HBAR"
          str(readkey$,10%,15%) = mod$
          read #2,key = readkey$, eod goto L04640
          h_bar% = 1%
L04640: return

        check_picture
          readkey$ = " " : p_bar% = 0%
          str(readkey$,1%,9%)   = "COST PBAR"
          str(readkey$,10%,15%) = mod$
          read #2,key = readkey$, eod goto L04720
          p_bar% = 1%
L04720: return

        check_lowe_argon                           /* Check for Labels */
          readkey$ = " "
          str(readkey$,1%,9%)   = "COST STIK"
          str(readkey$,10%,15%) = str(part$,5%,2%)       /* GLASS CODE */
          read #2,key = readkey$, using L04790  , descr$, eod goto L04960
L04790:      FMT POS(25), CH(30)
          if str(descr$,1%,1%) = " " then goto L04880
               rm_cnt% = rm_cnt% + 1%
               i% = rm_cnt%
               rm_part$(i%) = str(descr$,1%,10%)
               rm_cuts(i%)  = ll%                     /* No. of Lits  '*/
               rm_eq$(i%)   = "NA "                   /* Type & Eq. No.*/
               rm_ph$(i%)   = "Wired"                 /* Eq. Phantom No*/

L04880:   if str(descr$,16%,1%) = " " then goto L04960
               rm_cnt% = rm_cnt% + 1%
               i% = rm_cnt%
               rm_part$(i%) = str(descr$,16%,10%)
               rm_cuts(i%)  = ll%                     /* No. of Lits  '*/
               rm_eq$(i%)   = "NA "                   /* Type & Eq. No.*/
               rm_ph$(i%)   = "Wired"                 /* Eq. Phantom No*/

L04960: return

        lookup_hinge                                  /* Look Up Hinge */
            init(" ") hg$, descr$, readkey$, tb$
            tb$ = str(part$,11%,1%)
            p% = pos("2389" = tb$)
            if p% <> 0% then tb$ = "2"                /* Full Screen   */
            init(" ") hg$, descr$, readkey$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = str(part$,9%,2%)
            read #2,key = readkey$,using L05070 , descr$,eod goto L05120
L05070:        FMT POS(25), CH(30)
            if str(descr$,1%,2%) = "CO"  then hg$ = "1" /* Cottage     */
            if str(descr$,1%,2%) = "OR"  then hg$ = "2" /* Oriel       */
            if str(descr$,1%,3%) = "1/3" then hg$ = "3" /* 1/3 1/3 1/3 */
            if str(descr$,1%,3%) = "1/4" then hg$ = "4" /* 1/4 1/2 1/4 */
L05120: return

        check_misc
            if cc$(i%) = " " then return
            for jj% = 1% to 4%                           /* SAVE CODES */
                skip$(jj%) = str(co$(i%),jj%,1%)
            next jj%
            if skip$(1%) = "A" then goto L05280    /* CHECK SASHES T/B/F */
               if skip$(1%) <> "T" then goto L05230
                  if tb$ <> "5" then goto L05280
                  goto L05600
L05230:        if skip$(1%) <> "B" then goto L05260
                  if tb$ <> "4" and tb$ <> "6" then goto L05280
                  goto L05600
L05260:        if skip$(1%) <> "X" then L05280
               if tb$ = "4" or tb$ = "5" or tb$ = "6" then goto L05600
L05280:     if skip$(2%) = "A" then goto L05380       /* CHECK COT/ORIEL */
               if skip$(2%) <> "C" then goto L05320
                  if hg$ = "1" then goto L05380
                  goto L05600
L05320:        if skip$(2%) <> "O" then goto L05350
                  if hg$ = "2" then goto L05380
                  goto L05600
L05350:        if skip$(2%) <> "X" then goto L05380
                  if hg$ = "1" or hg$ = "2" then goto L05600

L05380:     if skip$(3%) = "A" then goto L05480     /* CHECK 1/3 AND 1/4 */
               if skip$(3%) <> "3" then goto L05420
                  if hg$ = "3" then goto L05480
                  goto L05600
L05420:        if skip$(3%) <> "4" then goto L05450
                  if hg$ = "2" then goto L05480
                  goto L05600
L05450:        if skip$(3%) <> "X" then goto L05480
                  if hg$ = "3" or hg$ = "4" then goto L05600

L05480:     if skip$(4%) = "A" then return   /* CHECK SCREEN MATERIALS */
               if tb$ = "0" then goto L05600
               if skip$(4%) <> "1" then goto L05530
                  if tb$ = "1" then return
                  goto L05600
L05530:        if skip$(4%) <> "2" then goto L05560
                  if tb$ = "2" then return
                  goto L05600
L05560:        if skip$(4%) <> "B" then return
               if tb$ = "1" or tb$ = "2" then return
                  goto L05600
        return
L05600:     cc$(i%) = " "                       /* Skip Misc. Material */
        return

        check_shrink_wrap                     /* Check Standard Stock  */
            stock$ = "N"
            stock% = 0%
            readkey$ = " "
            str(readkey$,1%,9%) = "COST SHRK"
            str(readkey$,10%,15%) = mod$
            read #2,key = readkey$, eod goto L05720
               goto L05790                      /* Specific Models       */

L05720:     if pos("467" = scr_prod$) = 0 then return
            readkey$ = all(hex(00))
            str(readkey$,1%,25%) = part$
            read #6,key 1% > readkey$, using L05770   , readkey$,          ~
                                                      eod goto L05930
L05770:        FMT XX(7), CH(32)
            if part$ <> str(readkey$,1%,25%) then goto L05930
L05790:        stock$ = "Y"

            if w% > 470% and h% > 750% then goto L05930
            if w% > 470% and h% > 750% then goto L05930
               gosub calc_w_h
               rm_cnt% = rm_cnt% + 1%
               i% = rm_cnt%
               rm_part$(i%) = "2310000044"
               x = ( ((s_width + 4)*(s_height + 4)) * .0138888) * .0292
               rm_cuts(i%)  = round( x, 4)            /* SHRINK WRAP   */
               rm_eq$(i%)   = "NA "                   /* Type & Eq. No.*/
               rm_ph$(i%)   = "Wired"                 /* Eq. Phantom No*/
               stock% = rm_cnt%

L05930: return

        calc_w_h
            a1 = 0.0 : a2 = 0.0
            convert str(part$,13%,3%) to a1, data goto L05980
L05980:
            convert str(part$,16%,1%) to a2, data goto L06000
L06000:
            s_width = a1 + (a2/8.0)
            a1 = 0.0 : a2 = 0.0
            convert str(part$,17%,2%) to a1, data goto L06040
L06040:
            convert str(part$,19%,1%) to a2, data goto L06060
L06060:
            s_height = a1 + (a2/8.0)
        return

/* (EWD001) */

        check_qty
           init(" ") gc_key$
           str(gc_key$,1%,3%)  = str(part$,1%,3%)
           str(gc_key$,4,25%)  = cr$(i%)
           str(gc_key$,29%,3%) = str(eq$(i%),6%,3%)
           read #7, key = gc_key$, using L06100, gc_qty$, eod goto L06110
L06100:         FMT XX(31), CH(02)

           convert gc_qty$ to p%, data goto L06110
           
L06110: return

/* (EWD001) */

        exit_sub
            if x_err% <> 0% then rm_desc$(rm_cnt%) = raw_err$(x_err%)
            if x_err% <> 0% then rm_err% = 2%
        end