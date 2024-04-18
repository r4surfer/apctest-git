        REM *************************************************************~
            *                                                           *~
            *  Special Note - Used by Programs (EWDPLN58) and (AWDPLN60)*~
            *                                                           *~
            *  Program Name      - AWDPLE60                             *~
            *  Creation Date     - 07/19/2014                           *~
            *  Last Modified Date- 10/21/2015                           *~
            *  Last Modified By  - Christie Sanders                     *~
            *                                                           *~
            *  Description       - Subroutine to calc balance data      *~
            *  Defined Errors                                           *~
            *    err% = 1% -> error converting wdt or hdt frm part      *~
            *    err% = 2% -> error converting values in GLASS10        *~
            *    err% = 3% -> max weight not reached                    *~
            *    err% = 4% -> no balance found to match height & weight *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *08/01/2014! New Program for (AWD) - Last Mod Date    ! CMG *~
            *05/21/2015! (IM7691) mod for litelift balances. Added! CMG *~
            *          !          additional weight checks.       !     *~            
            *10/21/2015! (SR70007) mod to correct code when lite  ! CMG *~
            *          !     lift balance is ordered              !     *~
            *04/12/2016! (SR74117) mod for lite lift balance      ! CMG *~
            *05/09/2016! (SR74730) mod to remove temporary file.  ! PWW *~
            *05/12/2016! (SR74452) mod to fix bug on within_range%! PWW *~
            *  /  /    !           and GENCODES Read.             ! PWW *~
            *06/27/2016! SR70007R  mod to fix more issues with    ! PWW *~
            *  /  /    !           SR70007.                       !     *~
            *************************************************************

         sub "AWDPLE60"  (opt%,          /* Calling Program            */~
                          part$,         /* PartNumber                 */~
                          subpart$,      /* Subpart                    */~
                          dim1es,        /* Dim1ES                     */~
                          dim2es,        /* Dim2ES                     */~
                          dim3es,        /* Dim3ES                     */~
                          dec_height,    /* Dec Height                 */~
                          total_weight,  /* Total Weight Returned      */~
                          balance$,      /* Bot Balance - SH ONLY      */~
                          err%)          /* Errors                     */


        dim prod$4,                      /* Prodcut Key                */~
            vendor$1,                    /* Vendor for balance         */~
            vendor2$1,                   /* Vendor for balance         */~
            bal_type$1,                  /* Type of balance for product*/~
            bal_type2$1,                 /* Type of balance for product*/~
            bal_key$27,                  /* AWDPLNWC Key               */~
            bal_key2$35,                 /* AWDPLNWC Key 2             */~
            bal_rec$256,                 /* BALANCE Record             */~
            strength$1,                  /* Glass Strength             */~
            balance$8,                   /* Balance                    */~
            bal_top$8,                   /* Top Balance                */~
            bal_bot$8,                   /* Bottom Balance             */~
            bal3_top$16,                 /* Top Balance                */~
            bal3_bot$16,                 /* Bottom Balance             */~
            td$2,                        /* Tube Diameter              */~
            v$1,                         /* View Top or Bottom         */~
            tdi_top$2,                   /* Tube Diameter Code         */~
            tdi_bot$2,                   /* Tube Diameter Code         */~
            wgt_top$14,                  /* Weight Diameter Code       */~
            wgt_bot$14,                  /* Weight Diameter Code       */~
            hgt_top$14,                  /* Sash Height                */~
            hgt_bot$14                   /* Sash Height Bottom         */

        dim                              /* EWDGLSSB Subroutine        */~
            ctt$(10%,3%)9, dept$3,       /* 1-5=Top, 6-10=Bot          */~
            gdd$(12%)10,                 /* 1-6=Top, 7-12=Bot          */~
            ajj$(10%,2%)6,               /* 1-5=Top, 6-10=Bot          */~
            dcc$(10%,2%)8                /* 1-5=Top, 6-10=Bot          */

        dim readbc$5,                    /* AWDPLNBC Readkey           */~
            unit_type$1,                 /* Unit Type S, Cottage, Oriel*/~
            static$1                     /* Is it static               */
            
REM            LOC_TOP$14,                  /* TOP BALANCE                */~
REM            LOC_BOT$14                   /* BOTTOM BALANCE             */

       dim                               /*                            */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            part$25,                     /* Mfg. Part No.              */~
            subpart$20,                  /* Sub Part No.               */~
            mdl$3,                       /* Model Number               */~
            hinge$2,                     /* Hinge Code                 */~
            scrn$1,                      /* (SR70007) screen field     */~
            wdt$7,                       /* Width - PartNumber Window  */~
            hgt$6,                       /* Height - PartNumber Window */~
            sav_mdl$3,                   /* Save Model Number          */~
            co_flag$2                    /* Flag for Cottage/Oriel     */

        dim f2%(32%),                    /* = 0 if the file is open    */~
            f1%(32%),                    /* = 1 if READ was successful */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */

        dim wdtkey$8,                    /* Width Key                  */~
            hgtkey$8                     /* Height Key                 */



            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AMTBOMCD ! Master Equation File                     *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! AWDPLNWC ! Production Windings & Coil File          *~
            * #18 ! AWDPLNBL ! Product and Balance Cross-Reference      *~
            * #19 ! AWDPLNBC ! Balance Location File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
/* (SR74117) temporarily change to TSTPLNWC */
/* (SR74730) remove temporary file */
/*          select #5,  "TSTPLNWC",                                      ~*/
            select #5,  "AWDPLNWC",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen = 27,                       ~
                        alt key  1, keypos =    1, keylen =  62,         ~
                            key  2, keypos =   28, keylen =  35

            select #18, "AWDPLNBL"                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  1,   keylen = 4

REM            select #19, "AWDPLNBC",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen =  5
                        
                        
            select #19, "AWDPLNBC",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen =  5                        

           err% = 0%
           if beenHereBefore% = 1% then goto NoFileOpen
            beenHereBefore% = 1%

            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%), 0%, rslt$(19%))

NoFileOpen:

            if opt% <> 0% then goto endSubroutine

            init(" ") bal_top$, bal_bot$, balance$

            mdl$     = str(part$,,3)
            hinge$   = str(part$,9,2)
/* (SR70007) */                        
            scrn$    = str(part$,11,1)
            if len(part$) < 19 then goto partError
            if scrn$ = "4" or scrn$ = "5" then goto partError
            if scrn$ = "6" or scrn$ = "7" then goto partError
/* (SR70007\) */            	
            wdt$     = str(part$,13,4)
            hgt$     = str(part$,17,3)
            sav_mdl$ = mdl$
REM ~~ convert window width & height to decimal
            convert wdt$ to wdt, data goto SizeErrorExit

            gosub lookup_model
REM ~~ Width
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)
REM ~~ Height
            convert hgt$ to hgt, data goto SizeErrorExit
            hgt = hgt / 10
            convert hgt to hgtkey$, pic(000.0)
            fctnl = hgt - int(hgt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            hgt = int(hgt) + fctnl         /* Reused in calc_tube_hgts */
            convert hgt to hgt$, pic(##0.000)

            gosub check_ctg_orl

            gosub calc_glass_size
            gosub find_balance
            goto endSubroutine



SizeErrorExit:
           err% = 1%
           goto endSubroutine
glass10ErrorExit:
           err% = 2%
           goto endSubroutine

maxWeightNotHit:
           err% = 3%
           goto endSubroutine
/* (SR70007) */           
partError:
           err% = 5%
           goto endSubroutine
/* (SR70007\) */           
endSubroutine:
           balance$ = bal_bot$
           if balance$ <> " " and balance% = 1% then goto BalanceFound
              err% = 4%
              balance$ = "NO BAL"
BalanceFound:
           end

        lookup_model
          init(" ") readkey$
          div_fact% = 0%
          fact = 0.00
          str(readkey$,1%,9%)   = "PLAN TMDL"
          str(readkey$,10%,15%) = mdl$
          gosub readGencdsDesc
            if found% = 0% then goto no_model

          mdl$ = str(desc$,1%,3%)
          convert str(desc$,12%,1%) to div_fact%, data goto no_model

          convert str(desc$,22%,5%) to fact, data goto no_model

          wdt = wdt / div_fact%
          wdt = wdt - fact
        no_model
        return

        readGencdsDesc
           found% = 0%
           init(" ") desc$
           found% = 0%
           read #4, key = readkey$, using GENFMT, desc$, eod goto noGencdsDesc
GENFMT:        FMT POS(25), CH(30)
           found% = 1%
noGencdsDesc:
        return

        readGencdsNoDesc
           found% = 0%
           read #4, key = readkey$, eod goto noGencdsNoDesc
           found% = 1%
noGencdsNoDesc:
        return

        check_ctg_orl
          init(" ") readkey$, desc$, co_flag$, unit_type$
          unit_type$ = "S"
          str(readkey$,1%,9%)   = "HINGE    "
          str(readkey$,10%,15%) = hinge$
          gosub readGencdsDesc
            if found% = 0% then goto no_hinge

            co_flag$ = str(desc$,1%,2%)
            if co_flag$ <> "CO" and co_flag$ <> "OR" then co_flag$ = " "
            if co_flag$ = "CO" then unit_type$ = "C"
            if co_flag$ = "OR" then unit_type$ = "O"
        no_hinge
        return


        calc_glass_size
          init(" ") ctt$(), gdd$(), ajj$(), dcc$(), dept$
          opts%, g_cnt%, ct%, er%, spType% = 0%

          call "EWDGLSSB" (opts%,      /* Options 0% = No Calc       */~
                           part$,      /* MFG Part Number            */~
                           subpart$,   /* Subpart                    */~
                           dim1es,     /*                            */~
                           dim2es,     /*                            */~
                           dim3es,     /*                            */~
                           dept$,      /* Department Code            */~
                           ct%,        /* Glass Piece Count          */~
                           ctt$(),     /* 1-5 = Top, 6-10 = Bot      */~
                           gdd$(),     /* 1-6 = Top, 7-12 = Bot      */~
                           ajj$(),     /* Window Adjustment (GED) Top*/~
                           dcc$(),     /* Decimal 1-5=Top, 6-10=Bot  */~
                           wdt$,       /* Window width Eights        */~
                           hgt$,       /* window Height Eights       */~
                           g_cnt%,     /* Glass Piece Cut Count      */~
                           spType%,    /* Spacer Type Sg,Db,Trp,Door */~
                           #4,         /* (GENCODES) Master Tables   */~
                           #1,         /* (AMTBOMCD) Equations       */~
                           er% )       /* 0%=Ok, Non-Zero = Error    */


REM          for c% = 1% to 12%

REM             call "SHOSTAT" ("ctt$(c%,1%) --> " & ctt$(c%,1%))
REM             call "SHOSTAT" ("dcc$(c%,1%) --> " & dcc$(c%,1%))
REM             call "SHOSTAT" ("ctt$(c%,2%) --> " & ctt$(c%,2%))
REM             call "SHOSTAT" ("dcc$(c%,2%) --> " & dcc$(c%,2%))
REM             call "SHOSTAT" ("gdd$(c%)    --> " & gdd$(c%))

REM          next c%

        return

        find_balance
          for gl% = 1% to 10% step 5%
            init(" ") v$
            if gl% <= 5% then v$ = "T"
            if gl% >  5% then v$ = "B"

            convert dcc$(gl%, 1%) to dec_width, data goto next_gls

            convert dcc$(gl%, 2%) to dec_height, data goto next_gls
            dec_height2 = dec_height  /* pwww testing  */

            if v$ = "T" then cvr_height = dec_height

            if unit_type$ <> "C" and unit_type$ <> "O" then           ~
                                             goto not_cott_oriel
               if v$ <> "B" then goto not_cott_oriel
                  cvr_height = dec_height

not_cott_oriel:
               gosub check_sclmr
               if s_clmr% = 1% then gosub set_sclmr_height
               gosub lookup_awdplnbl
               if bal% = 0% then goto next_gls
                  gosub calc_balance
next_gls:

               gosub lookup_awdplnbc
                 if loc% = 0% then goto next_loc
REM               GOSUB CALC_LOCATION
next_loc:
REM              GOSUB CALC_BALANCE_COVER_NEW

            next gl%
        return

        set_sclmr_height
REM        call "SHOSTAT" ( "I AM HERE AT SETTING HEIGHT")  stop
          if v$ = "T" then dec_height = top_height
          if v$ = "B" then dec_height = bot_height
        return

        check_sclmr                            /* Both - Cottage/Oriel */
          s_clmr% = 0%
          if s_clmr < 1.0 then return

          a1 = 0.0 : a2 = 0.0
          convert str(part$,17%,2%) to a1, data goto L50150
L50150:
          convert str(part$,19%,1%) to a2, data goto L50160
L50160:
          s_height = a1 + (a2/8.0)               /* Decimal Height   */

          init(" ") readkey$
          readkey$   = "GLASS10  " & str(part$,1%,3%)
          gosub readGencdsDesc
            if found% = 0% then return


          t_clmr = 0.0 : b_clmr = 0.0 : tb_clmr = 0.0
          convert str(desc$,1%,8%) to t_clmr, data goto glass10ErrorExit

          convert str(desc$,12%,8%) to b_clmr, data goto glass10ErrorExit

          convert str(desc$,22%,8%) to tb_clmr, data goto L50190
L50190

          top_height =((s_height/2.0) - t_clmr)                     ~
                             - (((s_height/2.0) + tb_clmr) - s_clmr)

          bot_height =((s_height/2.0) - b_clmr)                      ~
                             + (((s_height/2.0) + tb_clmr) - s_clmr)
REM            call "SHOSTAT" ( "I AM HERE AT CALCULATING HEIGHT")  stop
             s_clmr% = 1%
        return

        lookup_awdplnbl
           bal% = 0%
           init(" ") prod$, vendor$, bal_type$, vendor2$, bal_type2$
           max_weight, range_max1, range_max2, range_max3, range_max4,  ~
             range_max1_wt, range_max2_wt, range_max3_wt, range_max4_wt = 0.00

           prod$ = str(part$,1%,3%) & "T"
           if v$ = "B" then str(prod$,4%,1%) = "B"

           read #18, key = prod$, eod goto no_bal

              get #18, using PLNBL_FMT, vendor$, bal_type$,               ~
                                friction, weight, perimeter,              ~
                                vendor2$, bal_type2$, max_weight,         ~
/*IM7691 + */                   range_max1, range_max2, range_max3,       ~
                                range_max4, range_max1_wt, range_max2_wt, ~
/*IM7691 - */                   range_max3_wt, range_max4_wt
                                    

PLNBL_FMT:    FMT POS(5), CH(01), CH(01), PD(14,4), PD(14,4), PD(14,4), ~
                  CH(01), CH(01), PD(14,4),                             ~
/*IM7691 + */     PD(14,4), PD(14,4), PD(14,4), PD(14,4),               ~
/*IM7691 - */     PD(14,4), PD(14,4), PD(14,4), PD(14,4)


           bal% = 1%
        no_bal
        return

        calc_balance
REM             call "SHOSTAT" (" CALC BALANCE " )  stop
           gosub calc_sash_weight
balanceReset:
           if bal_type$ = "2" or bal_type$ = "6" then gosub cal_coil
           if bal_type$ <> "2" and bal_type$ <> "6" then gosub calc_amesbury

/* (SR70007) */
           if max_weight = 0.00 then goto noWeightCheck
           if vendor2$ = " " or bal_type2$ = " " then goto noWeightCheck
              if total_weight >= max_weight then goto LiteLift
/*IM7691 +      within_range% = 1%                                          */
/*SR70007R */   within_range% = 0%
                gosub window_height_range
/*IM7691 -      if within_range% = 1% then goto maxWeightNotHit  SR74452    */
/*IM7691 - */   if within_range% = 0% then goto maxWeightNotHit/*SR74452    */
              
LiteLift:              
/* RESET VENDOR AND BAL_TYPE TO SECOND TYPE */
              vendor$   = vendor2$
              bal_type$ = bal_type2$
/* SET TO BLANK SO WILL NOT CHECK MORE THAN ONCE */
              init(" ") vendor2$, bal_type2$
              goto balanceReset
noWeightCheck:
           if bal_type$ = "0" then gosub calc_sprial_unique
           gosub read_awdplnwc

           if v$ = "T" then                                   ~
                convert total_weight to wgt_top$, pic(-######.####)
           if v$ = "T" then                                   ~
                convert dec_height to hgt_top$, pic(-######.####)

           if v$ = "B" then                                   ~
                convert total_weight to wgt_bot$, pic(-######.####)
           if v$ = "B" then                                   ~
                convert dec_height to hgt_bot$, pic(-######.####)
        return
        
/*IM7691 + Check additional range for weight limit  */

        window_height_range
/* (SR70007) */         
REM             IF HGT >  RANGE_MAX1 THEN NEXT_HEIGHT1
REM                IF TOTAL_WEIGHT <  RANGE_MAX1_WT THEN WITHIN_RANGE% = 1%
REM                GOTO WEIGHT_HEIGHT_RANGE_END
REM        NEXT_HEIGHT1
REM             IF HGT >  RANGE_MAX2 THEN NEXT_HEIGHT2
REM                IF TOTAL_WEIGHT <  RANGE_MAX2_WT THEN WITHIN_RANGE% = 1%
REM                GOTO WEIGHT_HEIGHT_RANGE_END
REM        NEXT_HEIGHT2
REM             IF HGT >  RANGE_MAX3 THEN NEXT_HEIGHT3
REM                IF TOTAL_WEIGHT <  RANGE_MAX3_WT THEN WITHIN_RANGE% = 1%
REM                GOTO WEIGHT_HEIGHT_RANGE_END
REM        NEXT_HEIGHT3
REM             IF HGT >  RANGE_MAX4 THEN WEIGHT_HEIGHT_RANGE_END
REM                IF TOTAL_WEIGHT <  RANGE_MAX4_WT THEN WITHIN_RANGE% = 1%
REM        WEIGHT_HEIGHT_RANGE_END

/*SR70007*/  if hgt >  range_max1 then next_height1    
/*SR70007    if total_weight <=  range_max1_wt then within_range% = 0% */
/*SR70007R*/  if total_weight >=  range_max1_wt then within_range% = 1%                	
                goto weight_height_range_end
         next_height1
/*SR70007*/  if hgt >  range_max2 then next_height2    
/*SR70007    if total_weight <=  range_max2_wt then within_range% = 0% */
/*SR70007R*/  if total_weight >=  range_max2_wt then within_range% = 1%
                goto weight_height_range_end
         next_height2
/*SR70007*/  if hgt >  range_max3 then next_height3     
/*SR70007    if total_weight <=  range_max3_wt then within_range% = 0% */
/*SR70007R*/  if total_weight >=  range_max3_wt then within_range% = 1%	
                goto weight_height_range_end
         next_height3
/*SR70007*/  if hgt >  range_max4 then weight_height_range_end    
/*SR70007    if total_weight <=  range_max4_wt then within_range% = 0% */
/*SR70007R*/  if total_weight >=  range_max4_wt then within_range% = 1%                	
         weight_height_range_end
/* (SR70007\) */
        return
/*IM7691 - */        

        calc_sash_weight
           gosub lookup_gls_type
           gosub lookup_triple
           gosub lookup_laminate

           strength = 2.42         /* Default Single Strength */

           if strength$ = "3" and triple% = 1% then strength = 3.63
REM             if found% <> 1% then return
           if strength$ = "3" then return
           if strength$ = " " then return
           if strength$ = "4" then strength = 3.24
           if strength$ = "4" and triple% = 1% then strength = 4.875
           if strength$ = "4" and laminate% = 1% then strength = 5.55

        return


        calc_amesbury
          vinyl        = (((dec_width * weight) + (dec_height * weight)) * 2)
          vinyl        = vinyl + perimeter

          gls_surface  = ((dec_width * dec_height) / 144)
          gls_weight   = (gls_surface * strength)
          gls_friction = (dec_height * friction)

          total_weight = ((vinyl + gls_weight) - gls_friction)
          total_weight2 = total_weight   /* pwww testing  */
        return


        calc_sprial_unique
           gls_height_sclmr = 0.00
           gosub lookup_awdplnbc
           gls_height_sclmr = (dec_height * cott_oriel)
        return

        cal_coil

          vinyl        = (((dec_width * weight) + (dec_height * weight)) * 2)
          vinyl        = vinyl + perimeter

          gls_surface  = ((dec_width * dec_height) / 144)
          gls_weight   = (gls_surface * strength)
          gls_friction = ((2 * dec_height) * friction)

          total_weight = ((vinyl + gls_weight) - gls_friction)
        return


        lookup_gls_type
          strengthType% = 2%
          readkey$ = "TEMP GED " & str(part$,5,2)
/*        gosub readGencdsNoDesc          SR74452                           */
          gosub readGencdsDesc         /* SR74452                           */
          if found% = 1% then goto setStrength

          readkey$ = "OBS GED  " & str(part$,5,2)
          gosub readGencdsDesc
          if found% = 1% then goto setStrength

          strengthType% = 1%
          readkey$ = "GED 001  " & str(part$,5,2)
          gosub readGencdsDesc
          if found% = 1% then goto setStrength
        return
        setStrength
          if strengthType% = 1% then strength$ = str(desc$,3%,1%)
          if strengthType% = 2% and v$ = "T" then strength$ = str(desc$, 3%,1%)
          if strengthType% = 2% and v$ = "B" then strength$ = str(desc$,20%,1%)
        return
        lookup_triple
          triple% = 0%
          readkey$ = "PLANTRIPL" & str(part$,5,2)
          gosub readGencdsNoDesc
          if found% = 1% then triple% = 1%
        return
        lookup_laminate
          laminate% = 0%
          readkey$ = "PLAN LAMN" & str(part$,5,2)
          gosub readGencdsNoDesc
          if found% = 1% then laminate% = 1%
        return

        read_awdplnwc
           balance% = 0%
           init(" ") bal_key$, bal_key2$, td$, bal_rec$
           bal_key2$ = vendor$ & bal_type$
        awdplnwc_next
           read #5, key 2% > bal_key2$, using L53000, bal_rec$,   ~
                                   eod goto awdplnwc_done
L53000:                 FMT CH(256)

             str(bal_key2$,1%,35) = str(bal_rec$,28%,35%)
             str(td$,1%,2%)       = str(bal_rec$,63%,2%)

             get str(bal_key2$) using L53010, value1, value2,  ~
                         value3, value4

L53010:      FMT XX(03), PD(14,4), PD(14,4), PD(14,4), PD(14,4)

             if str(bal_key2$,1%,1%) <> vendor$ then goto awdplnwc_done
             if str(bal_key2$,2%,1%) <> bal_type$ then goto awdplnwc_done
             if str(bal_key2$,3%,1%) <> v$        then goto awdplnwc_next

             if value3 <> 0.00 and value4 <> 0.00 then gosub check_4

             if value3 = 0.00  and value4 =  0.00 then gosub check_2

             if awdplnwc_rec% = 0% then goto awdplnwc_next

REM                   IF STR(WK_KEY$,10%,5%) = "01012" THEN               ~
                      CALL "SHOSTAT" (" I AM HERE AT BOTTOM " )
REM                   IF STR(WK_KEY$,10%,5%) = "01012" THEN STOP
REM                CALL "SHOSTAT"("BAL KEY --> " & STR(BAL_REC$,4%,8%))  STOP
REM                CALL "SHOSTAT"("TD      --> " & TD$     )  STOP

             if bal_type$ = "0" then gosub set_sprial
             if bal_type$ <> "0" then gosub set_bal
                if v$ = "T" and td$ <> "99" then tdi_top$ = td$
                if v$ = "B" and td$ <> "99" then tdi_bot$ = td$
        awdplnwc_done
        return

        check_4
           if bal_type$ = "0" then goto check_4_sclmr
           awdplnwc_rec% = 0%
REM           height = dec_height
           if dec_height < value3 or dec_height > value4 then return

REM           weight = total_weight
           if total_weight < value1 or total_weight > value2 then return

           awdplnwc_rec% = 1%
        return
        check_4_sclmr
           awdplnwc_rec% = 0%
          if gls_height_sclmr < value3 or gls_height_sclmr > value4 then return

           if total_weight < value1 or total_weight > value2 then return

           awdplnwc_rec% = 1%
        return

        check_2
           awdplnwc_rec% = 0%

           if total_weight < value1 or total_weight > value2 then return
           awdplnwc_rec% = 1%
        return
        set_sprial
          if v$ = "B" then goto set_sprial_bot
            str(bal_top$,1%,3%) = str(bal_rec$,12%,3%)
            str(bal_top$,5%,3%) = str(bal_rec$,4%,3%)
            str(bal_top$,4%,1%) = "-"
        return
        set_sprial_bot
          str(bal_bot$,1%,3%) = str(bal_rec$,12%,3%)
          str(bal_bot$,5%,3%) = str(bal_rec$,4%,3%)
          str(bal_bot$,4%,1%) = "-"
        return

        set_bal
          if v$ = "T" then bal_top$ = str(bal_rec$,4%,8%)

          if v$ = "T" then bal3_top$ = str(bal_rec$,12%,16%)

          if v$ = "B" then bal_bot$ = str(bal_rec$,4%,8%)

          if v$ = "B" then bal3_bot$ = str(bal_rec$,12%,16%)

          if v$ = "B" then balance% = 1%
        return


        lookup_awdplnbc
          loc% = 0%
          init(" ") readbc$, static$
          limit, over_limit, under_limit, static_value,           ~
                cott_oriel = 0.00
          readbc$ = str(part$,1%,3%) & str(v$) & str(unit_type$)
REM           call "SHOSTAT" (" BC READKEY " & readbc$)  stop
REM          IF SC_DEPT$ = "049" AND STR(PART$,1,3) = "267"    ~
                             THEN STR(READBC$,1,3) = "215"

          read #19, key = readbc$, eod goto no_bc

            get #19, using L53050, limit, over_limit, under_limit, ~
                                   static$, static_value, cott_oriel
L53050:     FMT POS(6), PD(14,4), PD(14,4), PD(14,4), CH(1),    ~
                PD(14,4), PD(14,4)

          loc% = 1%
        no_bc
        return

REM        CALC_LOCATION
REM        CALL "SHOSTAT" (" SET LOCATION  " ) STOP
REM          SHOW% = 0%
REM          SASH_STOP_BOT, LOCATION = 0.00
REM          GOTO NOT_STATIC
REM
REM          IF STATIC$ = "N" THEN GOTO NOT_STATIC
REM            LOCATION = STATIC_VALUE
REM            GOTO SET_TOP_BOT
REM NOT_STATIC
REM                       CALL "SHOSTAT" (" CALC LOCATIONS " )   STOP
REM          IF UNIT_TYPE$ = "S" AND V$ = "T" THEN GOSUB SET_STANDARD_TOP
REM          IF UNIT_TYPE$ = "S" AND V$ = "B" THEN GOSUB SET_STANDARD_BOT
REM          IF UNIT_TYPE$ = "C" AND V$ = "T" THEN GOSUB SET_COTTAGE_TOP
REM          IF UNIT_TYPE$ = "C" AND V$ = "B" THEN GOSUB SET_COTTAGE_BOT
REM          IF UNIT_TYPE$ = "O" AND V$ = "T" THEN GOSUB SET_ORIEL_TOP
REM          IF UNIT_TYPE$ = "O" AND V$ = "B" THEN GOSUB SET_ORIEL_BOT
REM
REM SET_TOP_BOT
REM          IF SHOW% = 1% AND BAL_TYPE$ = "2" THEN  STR(INF$,15%,1%) = "*"
REM
REM          IF V$ = "T" AND BAL_TYPE$ = "2" THEN            ~
REM             CONVERT LOCATION TO LOC_TOP$, PIC(-#####0.00##)
REM          IF V$ = "B"  AND BAL_TYPE$ = "2" THEN            ~
REM             CONVERT LOCATION TO LOC_BOT$, PIC(-#####0.00##)
REM
REM          IF V$ = "B"  AND BAL_TYPE$ = "2" THEN            ~
REM             CONVERT SASH_STOP_BOT TO SASH_STOP_BOT$, PIC(-#####0.00##)
REM
REM          IF SHOW% = 1% AND BAL_TYPE$ = "6" THEN STR(INF$,15%,1%) = "*"
REM
REM          IF V$ = "T" AND BAL_TYPE$ = "6" THEN            ~
REM             CONVERT LOCATION TO LOC_TOP$, PIC(-#####0.00##)
REM          IF V$ = "B"  AND BAL_TYPE$ = "6" THEN            ~
REM             CONVERT LOCATION TO LOC_BOT$, PIC(-#####0.00##)
REM          IF V$ = "B"  AND BAL_TYPE$ = "6" THEN            ~
REM             CONVERT SASH_STOP_BOT TO SASH_STOP_BOT$, PIC(-#####0.00##)
REM        RETURN

REM        SET_STANDARD_TOP
REM          SHOW% = 0%
REM
REM          LOCATION = ((HGT / 2) + OVER_LIMIT)
REM          LOCATION = INT(LOCATION) + 1      /* ROUND UP TOP */
REM        RETURN

REM        SET_STANDARD_BOT
REM          IF DEC_HEIGHT < LIMIT THEN GOTO BOT_CALC
REM
REM          LOCATION = STATIC_VALUE  /* EQUAL TO OR GREATER */
REM          SHOW% = 1%
REM        RETURN
REM BOT_CALC
                                     /* ROUND DOWN */
REM          LOCATION = INT(((HGT / 2) + OVER_LIMIT))
REM        RETURN

REM        SET_COTTAGE_TOP
REM          SAV_DEC_HEIGHT = 0.00
REM          SAV_DEC_HEIGHT = DEC_HEIGHT
REM
REM          LOCATION = ((HGT) - (DEC_HEIGHT + OVER_LIMIT))
REM          LOCATION = INT(LOCATION) + 1    /* ROUND UP */
REM          SHOW% = 1%
REM        RETURN

REM        SET_COTTAGE_BOT
REM          IF DEC_HEIGHT >= LIMIT THEN LOCATION = STATIC_VALUE
REM                                        /* ROUND DOWN */
REM          IF DEC_HEIGHT <  LIMIT THEN                   ~
REM             LOCATION = INT(((HGT) - (DEC_HEIGHT + OVER_LIMIT)))
REM
REM
REM          SASH_STOP_BOT = 0.00
REM          SASH_STOP_BOT = ((DEC_HEIGHT - SAV_DEC_HEIGHT) + 7.4375)
REM
REM USE SET SIZES OF 0-5 USE 5; 5.1-10 USE 10; 10.1-15 USE 15;
REM ELSE EXACT MEASUREMENT ROUNDED UP
REM
REM          IF SASH_STOP_BOT <= 5 THEN SASH_STOP_BOT = 5
REM          IF SASH_STOP_BOT > 5 AND SASH_STOP_BOT <= 10 THEN SASH_STOP_BOT = 10
REM          IF SASH_STOP_BOT > 10 AND SASH_STOP_BOT <= 15 THEN SASH_STOP_BOT = 15
REM          IF SASH_STOP_BOT > 15 THEN SASH_STOP_BOT = INT(SASH_STOP_BOT + 1)
REM
REM          SHOW% = 1%
REM        RETURN

REM        SET_ORIEL_TOP
REM          LOCATION = ((HGT) - (DEC_HEIGHT + OVER_LIMIT))
REM          LOCATION = INT(LOCATION) + 1   /* ROUND UP */
REM          SHOW% = 1%
REM        RETURN

REM        SET_ORIEL_BOT
REM          IF DEC_HEIGHT >= LIMIT THEN LOCATION = STATIC_VALUE
REM                                 /* ROUND DOWN */
REM          IF DEC_HEIGHT <  LIMIT THEN                   ~
REM                 LOCATION = INT((DEC_HEIGHT  + OVER_LIMIT))
REM          SHOW% = 1%
REM        RETURN

REM        CALC_BALANCE_COVER_NEW
REM          IF STR(PART$,11%,1%) = "4" THEN RETURN
REM          IF STR(PART$,11%,1%) = "5" THEN RETURN
REM          IF STR(PART$,11%,1%) = "6" THEN RETURN
REM
REM          INIT(" ") READBC$
REM          BAL_COVER = 0.00
REM          BAL_COVER% = 0%
REM          READBC$ = STR(PART$,1%,3%) & STR(V$) & "A"
REM           CALL "SHOSTAT" (" BC READKEY " & READBC$)  STOP
REM          IF SC_DEPT$ = "049" AND STR(PART$,1,3) = "267"    ~
REM                             THEN STR(READBC$,1,3) = "215"
REM
REM          READ #19, KEY = READBC$, EOD GOTO NO_BC_CVR
REM
REM            GET #19, USING L53060, VALUE4
REM L53060       FMT POS(47), PD(14,4)

REM            BAL_COVER = CVR_HEIGHT + VALUE4
REM            BAL_COVER% = INT(BAL_COVER)
REM            IF V$ = "T" THEN CONVERT BAL_COVER% TO STR(CUS$,1%,4%), PIC(####)
REM            IF V$ = "B" THEN CONVERT BAL_COVER% TO STR(CUS$,6%,4%), PIC(####)
REM        NO_BC_CVR
REM        RETURN





