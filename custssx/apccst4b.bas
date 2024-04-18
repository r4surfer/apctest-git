*       ****************************************************************~
*                           ( As of 11/11/97 - RHH )                   *~
*                           ( Last Mod 07/26/00 - CMG )                *~
*        ( Same as (APCCSTHH) with Exception to return RM_UNIT() with  *~
*                the Raw Mat'l Unit Cost.                              *~
*                                                                      *~
*        APCCST4B - Subroutine to Calculate the Cost of Hardware and   *~
*                   Packaging. Also Routine for Calculating Cost of    *~
*                   Balance Tubes and Special Balance Tube Requirements*~
*                                                                      *~
*                   Note - Table Used (COST 05BL)                      *~
*                                and  (COST 06BL) Special Models with  *~
*                                One (1) Raw Material                  *~
*                                                                      *~
*                     PART$         = MFG Part Number                  *~
*                     CST_TYPE$     = '0'=Hardware, '1'=Packaging      *~
*                     RM_RAW$()     = Inventory Raw Material Part      *~
*                     RM_CUTS()     = Total Cuts Decimal Inches        *~
*                     RM_COST()     = Total Cost of Part Material      *~
*                     RM_DESC$()    = Raw Material Description         *~
*                     RM_UNITS%()   = Raw MAT'L Units of Measure       *~
*                     RM_UNIT()     = Raw Mat'l Unit Cost              *~
*                     RM_VINYL      = Raw Material Total Cost Vinyl    *~
*                     RM_MISC       = Raw Material Total Cost Misc     *~
*                     RM_TOTAL      = Raw Material Total Cost          *~
*                     RM_CNT%       = Raw Material Count (Pieces)      *~
*                         ( Seven (7) - Scrap Fields Not Applicable )  *~
*                     RM_FRT        = Save the Freight Amount          *~
*                     RM_VINYL_D    = Save the Vinyl Disc. Amt         *~
*                     RM_ERR%       = 0% = Ok, Non 0% = Error          *~
*----------!----------------------------------------------------!------*~
*   DATE   !                MODIFICATION                        ! WHO  *~
*----------!----------------------------------------------------!------*~
* 03/27/98 ! y2k compliant                                      !  DJD *~
*          !                                                    !      *~
* 07/26/00 ! Change so that if model is in table COST 06BL, the !  CMG *~
*          !     pgm will not exclude the second raw mat (EWD001)!     *~
*----------!----------------------------------------------------!------*~
*                                                                      *~
************************************************************************

        sub "APCCST4B" (part$,           /* MFG Part Number           */ ~
                        cst_type$,       /* '0'=Hardware,'1'=Packaging*/ ~
                        rm_raw$(),       /* Inv. Raw Material Part    */ ~
                        rm_cuts(),       /* Total Inches in Decimal   */ ~
                        rm_cost(),       /* Total Cost Raw Mat. Part  */ ~
                        rm_desc$(),      /* Raw Material Description  */ ~
                        rm_units%(),     /* Raw MAT'L Units of Measure*/ ~
                        rm_unit(),       /* Raw Mat'l Unit Cost       */ ~
                        rm_vinyl,        /* Raw Mat'l Vinyl Cost      */ ~
                        rm_misc,         /* Raw Mat'l Misc Cost       */ ~
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
                              #1,        /* (APCCSTHP) - File         */ ~
                              #2,        /* (GENCODES) - File         */ ~
                              #3,        /* (HNYQUAN ) - File         */ ~
                              #4,        /* (HNYMASTR) - File         */ ~
                        rm_err% )        /* 0% = Ok, Non 0% = Error   */

        dim part$25,                     /* MFG Part Number           */ ~
            raw$25, raw_err$(20%)20,     /* Balance Tubing            */ ~
            cst_type$1,                  /* '0'=Hardware,'1'=Packaging*/ ~
            rm_raw$(100%)14,             /* Inv. Raw Material         */ ~
            rm_desc$(100%)32,            /* Raw Material Desc         */ ~
            rm_unit(100%),               /* Raw Mat'l Unit Cost       */ ~
            rm_cuts(100%),               /* Total Inches in Decimal   */ ~
            rm_cost(100%),               /* Total Cost Raw Material   */ ~
            rm_cuts_s(100%),             /* Total Inches in Dec. Scrap*/ ~
            rm_cost_s(100%),             /* Total Cost Raw Mat'l Scrap*/ ~
            rm_units%(100%),             /* Raw Material Units        */ ~
            rm_units$(100%)1,            /* Raw Material Units        */ ~
            rm_eq$(100%)3,               /* Sav Typ and Eq. No. N/A   */ ~
            rm_ph$(100%)5,               /* Sav Phantom Cose N/A      */ ~
            readkey$24, descr$32,        /* Gencodes Key              */ ~
            sav_descr$32,                /* Save Description - Tubes  */ ~
            sav_gen$12, ht_wd$7,         /* Save Key and Part HT_WD   */ ~
            r$(2%)14,                    /* Sav Tube Raw Material     */ ~
            mod$3, prod$1,               /* Model and Product Line    */ ~
            sc$1, vv$3,                  /* Save Screen Code          */ ~
            cst_key$20, sav_key$6        /* Hardware, Packaging       */

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


            init(" ") rm_raw$(), rm_desc$(), cst_key$, sav_key$, r$(),   ~
                      readkey$, descr$, sav_descr$, ht_wd$, sav_gen$,    ~
                      mod$, prod$
            init(" ") rm_eq$(), rm_ph$()
            mat rm_cuts = zer   : mat rm_unit = zer
            mat rm_cost = zer
            mat rm_units% = zer
            mat rm_cuts_s = zer
            mat rm_cost_s = zer
            rm_vinyl, rm_misc, rm_total = 0.0
            rm_frt, rm_vinyl_d, raw_frt, raw_vinyl = 0.0
            rm_vinyl_s, rm_misc_s, rm_total_s = 0.0
            rm_cnt%, rm_err% = 0%
            mod$  = str(part$,1%,3%)
            prod$ = str(part$,1%,1%)
            sc$ =   str(part$,11%,1%)                  /* Top/Bot Sash */

           i% = 1%
           cst_key$ = all(hex(00))
           str(cst_key$,1%,1%) = cst_type$
           str(cst_key$,2%,4%) = str(part$,1%,4%)
           str(cst_key$,6%,1%) = "0"                   /* Standard     */
           if sc$ = "4" then str(cst_key$,6%,1%) = "1" /* Top Sash Only*/
           if sc$ = "5" then str(cst_key$,6%,1%) = "2" /* Bot Sash Only*/
           if sc$ = "6" then str(cst_key$,6%,1%) = "1" /* Top Sash Only*/
           sav_key$ = str(cst_key$,1%,6%)
           read #1,key > cst_key$, using L01370 , cst_key$, rm_desc$(i%),  ~
                         rm_units$(i%), rm_cost(i%), rm_cuts(i%),        ~
                                                     eod goto hdr_done
           goto L01380
        hdr_next
           read #1, using L01370 , cst_key$, rm_desc$(i%), rm_units$(i%),  ~
                          rm_cost(i%), rm_cuts(i%), eod goto hdr_done
L01370:       FMT CH(20), CH(25), CH(1), PD(14,4), PD(14,4)
L01380:    if str(cst_key$,1%,6%) <> sav_key$ then goto hdr_done
              rm_raw$(i%) = str(cst_key$,7%,14%)
              i% = i% + 1%
              goto hdr_next
        hdr_done
        rm_cnt% = i% - 1%
           init(" ") rm_desc$(i%), rm_units$(i%)
           rm_cost(i%), rm_cuts(i%) = 0.0

           for i% = 1% to rm_cnt%
               convert rm_units$(i%) to rm_units%(i%), data goto L01490
L01490:
               rm_unit(i%) = rm_cost(i%)        /* Raw Mat'l Unit Cost */
               rm_cost(i%) = round(rm_cuts(i%) * rm_cost(i%), 4)
               if str(rm_raw$(i%),2%,3%) <> "105" then goto L01550
                  rm_vinyl = round( rm_vinyl + rm_cost(i%), 4)
                  goto L01570
L01550:        rm_misc = round( rm_misc + rm_cost(i%), 4)

L01570:        rm_total    = round(rm_total + rm_cost(i%), 4)
           next i%

           if cst_type$ = "1" then goto exit_sub          /* Packaging */
                                                 /* Calc Balance Tubes */
           if prod$ <> "5" then goto L01660
                                      /* Only Two Models have Bal Tube */
              if mod$ <> "510" and mod$ <> "511" then goto exit_sub

L01660: REM - No Balance Tubes for T/S/F
           p% = pos("456" = sc$)
           if p% <> 0% then goto exit_sub

           str(ht_wd$,1%,3%) = str(part$,17%,3%)
           str(ht_wd$,4%,4%) = str(part$,13%,4%)
           str(readkey$,1%,9%)  = "COST 05BL"
           str(readkey$,10%,3%) = str(part$,1%,3%)
           sav_gen$ = str(readkey$,1%,12%)
        bal_next
           read #2,key > readkey$, using  L01780, readkey$, descr$,        ~
                                                        eod goto bal_done
L01780:       FMT CH(24), CH(32)
           if sav_gen$ <> str(readkey$,1%,12%) then goto bal_done
           if str(readkey$,13%,7%) > ht_wd$ then goto bal_done
              sav_descr$ = descr$
              goto bal_next
        bal_done
           if sav_descr$ <> " " then goto L01860
              goto exit_sub                        /* No Balance Tubes */
L01860:    r% = 0%
           p% = pos(sav_descr$ = "-")
           r% = r% + 1%
           r$(r%) = str(sav_descr$,1%,p%-1%)
           if str(sav_descr$,p%+1%,1%) = "X" then goto L01980
                                          /* Only One (1) Balance Tube */
REM  (EWD001)          gosub check_tubes             /* Raw Material Number.      */
REM  (EWD001)          if tube% = 1% then goto L01980

              r% = r% + 1%
              r$(r%) = str(sav_descr$,p%+1%,14% )
              
            gosub check_tubes                        /* (EWD001)  */
REM  (EWD001)   if tube% = 1% then goto L01980
            
L01980:    for i% = 1% to r%
              vv$ = str(raw$,2%,3%)
              raw$ = r$(i%)
              call "APCCST9B" (raw$, qty, 1%, cost, descr$, rm_unitc,    ~
                                      raw_frt, raw_vinyl, #3, #4, e% )
              rm_cnt% = rm_cnt% + 1%
              rm_raw$(rm_cnt%) = raw$
              rm_cuts(rm_cnt%) = qty
              rm_cost(rm_cnt%) = cost
              rm_desc$(rm_cnt%) = descr$
              rm_unit(rm_cnt%)  = rm_unitc
              if e% <> 0% then rm_desc$(rm_cnt%) = raw_err$(e%)
              rm_units%(rm_cnt%) = 1%
              if vv$ <> "105" and vv$ <> "110" and vv$ <> "115" then     ~
                                                                goto L02150
                 rm_vinyl = round( rm_vinyl + cost, 4)
                 goto L02170
L02150:       rm_misc = round( rm_misc + cost, 4)

L02170:       rm_total   = round(rm_total   + cost, 4)
              rm_frt     = round(rm_frt     + raw_frt, 5)
              rm_vinyl_d = round(rm_vinyl_d + raw_vinyl, 5)
           next i%

        exit_sub
        end

        check_tubes
           qty = 1.0 : tube% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)   = "COST 06BL"
           str(readkey$,10%,15%) = mod$
           read #2,key = readkey$, using L02310, descr$, eod goto no_tubes
L02310:       FMT POS(24), CH(32)
           convert str(descr$,1%,5%) to qty, data goto L02360

           tube% = 1%
L02360
        no_tubes
        return









        