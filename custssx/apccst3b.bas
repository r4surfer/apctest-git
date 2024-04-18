*       ****************************************************************~
*                           ( As of 11/11/97 - RHH )                   *~
*        ( Same as (APCCSTKK) with Exception to Return RM_UNIT() with  *~
*               Raw Mat'l Unit Cost.                                   *~
*                                                                      *~
*        APCCST3B - Subroutine to Calculate the Cost of Locks and      *~
*                   Keeper Rail. Special Code for Vinyl (312) Patio    *~
*                   Door for Brass Handles and Locks.                  *~
*                                                                      *~
*           Table - Uses Code Table 'COST 06LK'                        *~
*                   ( Note - Lock and Keeper Raw Material )            *~
*                                                                      *~
*           Table - Uses Code Table 'COST 07LK'                        *~
*                   ( Note - Lock and Keeper Screws )                  *~
*                                                                      *~
*           Table - Uses Code Table 'COST SLID'                        *~
*                   ( Note - Check Sliders )                           *~
*                                                                      *~
*            Note - (1) Uses Subroutine (APCCST9B) to Calculate the    *~
*                       Unit Cost Associated with a Raw Material.      *~
*                                                                      *~
*                 - (2) All 700's have Two (2) Locks. Mod to Set       *~
*                       LK$ = 2 for 700's.                             *~
*                                                                      *~
*                 - (3) All 700's have Keepers and 711, 721, 731       *~
*                       have only One (1) Spring.                      *~
*                                                                      *~
*                 - (4) Special Logic for Sliders, uses table          *~
*                       COST SLID                                      *~
*                                                                      *~
*                     PART$         = MFG Part Number                  *~
*                     RM_PART$()    = Inventory Raw Material Part      *~
*                     RM_CUTS()     = Total Cuts Decimal Inches        *~
*                     RM_COST()     = Total Cost of Part Material      *~
*                     RM_DESC$()    = Raw Material Description         *~
*                     GS_UNITS%()   = Raw Mat'l Unit of Measure        *~
*                     RM_UNIT()     = Raw Mat'l Unit Cost              *~
*                     RM_VINYL      = Raw Material VINYL Cost          *~
*                     RM_MISC       = Raw Material MISC. Cost          *~
*                     RM_TOTAL      = Raw Material Total Cost          *~
*                     RM_CNT%       = Raw Material Count (Pieces)      *~
*                     RM_ERR%       = 0% = Ok, Non 0% = Error          *~
*                     RM_FRT        = Save Freight Cost Amount         *~
*                     RM_VINYL_D    = Save Vinyl Discount Amount       *~
*                                                                      *~
*----------!----------------------------------------------------!------*~
*   DATE   !                MODIFICATION                        ! WHO  *~
*----------!----------------------------------------------------!------*~
* 03/27/98 ! y2k compliant                                      !  DJD *~
* 10/28/02 ! (EWD001) - Mod for new patio door models.          !  CMG *~
*          !                                                    !      *~
*----------!----------------------------------------------------!------*~
*       ****************************************************************

        sub "APCCST3B" (part$,           /* MFG Part Number           */ ~
                        rm_part$(),      /* Inv. Raw Material Part    */ ~
                        rm_cuts(),       /* Total Inches in Decimal   */ ~
                        rm_cost(),       /* Total Cost Raw Mat. Part  */ ~
                        rm_desc$(),      /* Raw Material Description  */ ~
                        rm_units%(),     /* Raw Mat'l Units of Measure*/ ~
                        rm_unit(),       /* Raw Mat'l Unit Cost       */ ~
                        rm_vinyl,        /* Raw Mat'l Cost Vinyl      */ ~
                        rm_misc,         /* Raw Mat'l Cost Misc.      */ ~
                        rm_total,        /* Raw Material Total Cost   */ ~
                        rm_cnt%,         /* Raw Material Count        */ ~
                        rm_frt,          /* Save Freight Cost Amount  */ ~
                        rm_vinyl_d,      /* Save Vinyl Discount Amount*/ ~
                              #1,        /* (GENCODES) - File         */ ~
                              #2,        /* (HNYMASTR) - File         */ ~
                              #3,        /* (HNYQUAN ) - File         */ ~
                        rm_err% )        /* 0% = Ok, Non 0% = Error   */

        dim part$25,                     /* MFG Part Number           */ ~
            raw$25,                      /* Raw Mat. Part No.         */ ~
            rm_part$(100%)10,            /* Inv. Raw Material         */ ~
            rm_desc$(100%)32,            /* Raw Material Desc         */ ~
            rm_units%(100%),             /* Raw Material Units        */ ~
            rm_unit(100%),               /* Raw Mat'l Unit Cost       */ ~
            rm_cuts(100%),               /* Total Inches in Decimal   */ ~
            rm_cost(100%),               /* Total Cost Raw Material   */ ~
            lk$1, mod$3, cl$1,           /* Lock Field , Model, Color */ ~
            hg$2, sc$1,                  /* Hinge Code / Screen Code  */ ~
            wd$4, ht$3,                  /* Width and Height          */ ~
            raw_err$(20%)20,             /* Error Messages            */ ~
            prod$1,                      /* Product Line              */ ~
            vv$3, mm$2,                  /* SAVE RAW MAT'L VINYL      */ ~
            readkey$24, descr$30         /* Generic Key               */

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


            init(" ") readkey$, rm_part$(), rm_desc$(), raw$
            mat rm_cuts   = zer : mat rm_cost = zer
            mat rm_units% = zer : mat rm_unit = zer
            rm_vinyl, rm_misc, rm_total = 0.0
            rm_frt, rm_vinyl_d, raw_frt, raw_vinyl = 0.0

            sc$   = str(part$,11%,1%)               /* Screen Code     */
            lk$   = str(part$,12%,1%)               /* Lock Code       */
            mod$  = str(part$,1%,3%)                /* Model Code      */
            mm$   = str(part$,1%,2%)                /* Model 1,2, Char */
            cl$   = str(part$,4%,1%)                /* Color Code      */
            prod$ = str(part$,1%,1%)                /* Product Line    */
            hg$   = str(part$,9%,2%)                /* Hinge Code      */
            wd$   = str(part$,13%,4%)               /* Width           */
            ht$   = str(part$,17%,3%)               /* Height          */

            raw_err% = 0%
            if prod$ = "5" then goto exit_sub       /* Locks for the   */
                                                    /* 500 Series are  */
                                                    /* in 'Hardware'   */
            if sc$ = "6" then goto exit_sub         /* Fixed Glass Only*/
                                                    /* No Locks/Keepers*/

            if prod$ <> "6" and prod$ <> "7" then goto L01320
               lk$ = "1"
               if wd$ > "0232" then lk$ = "2"       /* Lock Criteria   */
                                                    /* Locks AN Keeper */
                                                    /* Check Three Lit */
L01320:     gosub check_sliders                     /* Check Height    */

            gosub load_locks
            if rm_cnt% = 0% then goto exit_sub
            for i% = 1% to rm_cnt%
                vv$ = str(raw$,2%,3%)
                raw$ = rm_part$(i%)
                rm_units%(i%) = 1%
                call "APCCST9B" (raw$, rm_cuts(i%), rm_units%(i%),       ~
                                 rm_cost(i%), rm_desc$(i%), rm_unit(i%), ~
                                 raw_frt, raw_vinyl, #3, #2, raw_err%)
                if raw_err% = 0% then goto L01450
                   rm_desc$(i%) = raw_err$(raw_err%)
L01450:         rm_total   = round(rm_total + rm_cost(i%), 4)
                rm_frt     = round(rm_frt   + raw_frt, 5)
                rm_vinyl_d = round(rm_vinyl_d + raw_vinyl, 5)
                if vv$ <> "105" and vv$ <> "110" and vv$ <> "115" then   ~
                                                              goto L01520
                   rm_vinyl = round(rm_vinyl + rm_cost(i%), 4)
                   goto L01530
L01520:         rm_misc = round(rm_misc + rm_cost(i%), 4)
L01530:     next i%

        exit_sub
        end

        load_locks                           /* Lock Raw Material No.s */
            rm_cnt% = 0%
            unit% = 1%
                                                    /* Check Vinyl Patio */
                                                    /*    (EWD001)       */
            if mm$ <> "31" and mm$ <> "33" then goto L01650         
               if lk$ <> "7" then cl$ = "0"
               if hg$ = "42" then unit% = 2%
               goto L01720
L01650:     if mm$ <> "32" then goto L01680         /* Check Hinged Patio*/
               if lk$ <> "7" then cl$ = "0"
               goto L01720
L01680:     p = pos("1234" = lk$)
            if p = 0 then return
            if lk$ = "2" then unit% = 2%
                                       /* Lock and Keeper Raw Material */
L01720:     rm_cnt% = 4%
                                                    /*    (EWD001)     */
            if mm$ = "31" or mm$ = "32" or mm$ = "33" then rm_cnt% = 1%
            readkey$ = " "
            str(readkey$,1%,9%) = "COST 06LK"
            str(readkey$,10%,15%) = mod$ & cl$       /* Model an Color */
            read #1,key = readkey$,using L01780 , descr$,eod goto L02120
L01780:        FMT POS(25), CH(30)
            rm_part$(1%) = str(descr$,1%,10%)                /* Locks  */
                                                    /*    (EWD001)     */
            if mm$ <> "31" and mm$ <> "32" and mm$ <> "33" then goto L01830
               if lk$ = "7" or lk$ = "8" or lk$ = "9" then goto L01830
                  rm_part$(1%) = str(descr$,12%,10)
L01830:     rm_cuts(1%)  = unit%
                                                    /*    (EWD001)     */
            if mm$ = "31" or mm$ = "32" or mm$ = "33" then return
               rm_part$(3%) = str(descr$,12%,10%)            /* Keepers*/
               rm_cuts(3%)  = unit%
                                       /* Lock and Keeper Screws       */
            readkey$ = " "
            str(readkey$,1%,9%) = "COST 07LK"
            str(readkey$,10%,15%) = mod$ & cl$       /* Model an Color */
            read #1,key = readkey$,using L01920 , descr$,eod goto L02120
L01920:        FMT POS(25), CH(30)
            rm_part$(2%) = str(descr$,1%,10%)         /* Lock Screws   */
            rm_cuts(2%)  = 2% * unit%
            if mod$ = "711" or mod$ = "721" or mod$ = "731" then         ~
               rm_cuts(2%) = unit%

               rm_part$(4%) = str(descr$,12%,10%)     /* Keeper Screws */
               rm_cuts(4%)  = 2% * unit%

             if sc$ <> "4" then goto L02070
                rm_part$(1%) = rm_part$(3%)           /* Top Sash Only */
                rm_cuts(1%) = rm_cuts(3%)
                rm_part$(2%) = rm_part$(4%)
                rm_cuts(2%) = rm_cuts(4%)
                goto L02080
L02070:      if sc$ <> "5" then goto L02120             /* Bot Sash Only */
L02080:         rm_cnt% = 2%
                rm_cuts(3%) , rm_cuts(4%) = 0.0
                rm_part$(3%), rm_part$(4%) = " "

L02120: return

        check_sliders
          readkey$ = " "
          str(readkey$,1%,9%)   = "COST SLID"
          str(readkey$,10%,15%) = mod$
          read #1,key = readkey$, eod goto L02210
             lk$ = "1"
             if ht$ > "232" then lk$ = "2"
L02210: return

