        REM *************************************************************~
            *  Called By - (APCPLA58-Sub)(EWDPLN66-Prog)               *~
            *                                                           *~
            *  Program Name      - EWDGLSSB - Subroutine                *~
            *  Creation Date     - 08/10/98                             *~
            *  Last Modified Date- 05/01/2017                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Calculate Glass Cut Data             *~
            *                                                           *~
            *       Note  - cut$(x%,y%)  x% = 1% to 5%  (Tops)          *~
            *                            x% = 6% to 10% (Bots)          *~
            *                            y% = 1% (Calc Width)           *~
            *                            y% = 2% (Calc Height)          *~
            *                            y% = 3% (Calc Clmr)            *~
            *                                                           *~
            *               ged$(x%)     x% = 1% to 6%  (For all Tops)  *~
            *                            x% = 7% to 12% (For all Bots)  *~
            *                                                           *~
            *               adj$(X%,y%)  x% = 1% to 5%  (Tops)          *~
            *                            x% = 6% to 10% (Bots)          *~
            *                            y% = 1% (Width)                *~
            *                            y% = 2% (Height)               *~
            *                                                           *~
            *               dec$(X%,y%)  x% = 1% to 5%  (Tops)          *~
            *                            x% = 6% to 10% (Bots)          *~
            *                            y% = 1% (Width)                *~
            *                            y% = 2% (height)               *~
            *                                                           *~
            *                                                           *~
            *    single strength                spType% = 1%            *~
            *    double strength                spType% = 2%            *~
            *    tempered double strength       spType% = 3%            *~
            *    SDL Single strength            spType% = 4%            *~
            *    SDL Double strength            spType% = 5%            *~
            *    SDL Temp Double strength       spType% = 6%            *~
            *    Triple Strength > 25SqFt 3/16  spType% = 7%            *~
            *    Door 5/32 Triple Strength      spType% = 8%            *~
            *    Laminate Glass                 spType% = 9%            *~
            *    Laminate Strengthened Glass    spType% =10%            *~
            *    Single Glazed Tempered         spType% =11%            *~
            *    Laminate 5/32                  spType% =12%            *~
            *    Laminate Strengthened 5/32     spType% =13%            *~
            *    Laminate 3/16                  spType% =14%            *~
            *    Laminate Strengthened 3/16     spType% =15%            *~
            *    STC 1/8 and 3/16               spType% =16%            *~
            *    tempered triple strength       spType% =17% (CR1988)   *~
            *    tempered quad   strength       spType% =18% (CR1988)   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/10/98 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/25/99 ! Mods for new Glass Routine               ! RHH *~
            * 10/28/02 ! (EWD001) - Mod for new patio door models.! CMG *~
            * 09/15/03 ! (EWD002) - Mod for tempered glass to be  ! CMG *~
            *          !            double strength               !     *~
            * 06/14/07 ! (AWD003) - mod for new casement          ! CMG *~
            * 12/10/08 ! (AWD004) - mods for casement hinge codes ! CMG *~
            *03/31/2010! (AWD005) - mod for 5/32 & 3/16 glass     ! CMG *~
            *02/10/2012! (AWD006) - mod for 5/32 & 3/16 glass     ! CMG *~
            *04/23/2013! (AWD007) - mod for lamn glass            ! CMG *~
            *07/02/2013! (AWD008) - OGO change ecn 2013-032       ! CMG *~
            *05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
            *07/21/2014! (AWD009) mod to check for dept 008 mdls  ! CMG *~
            *10/22/2014! (AWD010) mods for patio glass            ! CMG *~
            *05/01/2017! (CR838)  mos for glass top & bot qty     ! CMN *~
            *11/29/2017! (CR1173) Mods for STC Larson Window      ! CMN *~
            *07/31/2018! CR1578  remove DIM1ES reset to 0         ! RDB *~
            *05/01/2019! (CR1988) mod for Triple, Quad, STC glass ! CMN *~
            *************************************************************

        sub "EWDGLSSB" (opts%,           /* Options 0% = No Calc       */~
                        part$,           /* MFG Part Number            */~
                        subpart$,        /* Subpart Number (AWD005)    */~
                        dim1es,          /* (AWD009)                   */~
                        dim2es,          /* (AWD009)                   */~
                        dim3es,          /* (AWD009)                   */~
                        dept$,           /* Department Code            */~
                        ct%,             /* Buffer Count               */~
                        cut$(),          /* 1-5 = Top, 6-10 = Bot      */~
                        ged$(),          /* 1-6 = Top, 7-12 = Bot      */~
                        adj$(),          /* Window Adjustment (GED) Top*/~
                        dec$(),          /* Decimal 1-5=Top, 6-10=Bot  */~
                        wd$,             /* Window width Eights        */~
                        ht$,             /* window Height Eights       */~
                        g_cnt%,          /* Glass Piece Cut Count      */~
/*(AWD005)*/            spType%,         /* Spacer Type Sg,Db,Trp,Door */~
                        #1,              /* (GENCODES) Master Tables   */~
                        #2,              /* (AMTBOMCD) Equations       */~
                        err% )           /* 0% = Ok, Non-Zero = Error  */

        dim tab$(40%)9,                  /* Gencodes Table files       */~
            dept$3,                      /* Department Code            */~
            cut$(10%,3%)9,               /*                            */~
            ged$(12%)10,                 /*                            */~
            adj$(10%,2%)6,               /*                            */~
            dec$(10%,2%)8,               /*                            */~
            view$3,                      /* Top/Bot Glass              */~
            code$3,                      /* Table Lookup               */~
            readkey$24, descr$30,        /* Table Lookup               */~
            part$25,                     /* MFG Part Number            */~
            subpart$20,                  /* (AWD005) Subpart           */~
            model$3,                     /* Model Code                 */~
            cl$1,                        /* Color Code                 */~
            ty$2,                        /* Glass Type Code            */~
            lt$2,                        /* Grid Liting Code           */~
            hg$2,                        /* Hinge Codes                */~
            sc$1,                        /* Screen codes and Sash      */~
            lk$1,                        /* Lock and Fin codes         */~
            wd$7,                        /* Actual Width Window Eights */~
            ht$6,                        /* Actual Height Window Eights*/~
            sze$30,                      /* Save Eights                */~
            sz$100,                      /* Save Sixteenths            */~
            wd1$9,                       /* Calculated Width           */~
            wd2$9,                       /* CLMR FOR SCREEN            */~
            ht1$8,                       /* Calculated Height          */~
            hgl$15,                      /* Hinge Code Left Discript   */~
            hgr$15,                      /* Hinge Code Right Descript  */~
            width_d$8,                   /* Save Width Calc in Decimal */~
            height_d$8,                  /* Save Height Calc Decimal   */~
            t_k$6,                       /* Thickness                  */~
            s_s$6,                       /* Single Strength Spacer     */~
            s_d$6,                       /* Double Strength Spacer     */~
            s_sdl_s$6,                   /* Single Strength SDL(AWD005)*/~
            s_sdl_d$6,                   /* Double Strength SDL(AWD005)*/~
            s_5_32$6,                    /* 5/32 Spacer (AWD005)       */~
            s_3_16$6,                    /* 3/16 Spacer (AWD005)       */~
            muttin$8, lits$1, mut$8,     /* Muttin Code Vert/Horiz     */~
            l_lt$6,                      /* Liting Left Descr          */~
            r_lt$6,                      /* Liting Right Descr         */~
            sandwich$10,                 /* Glass Sandwich             */~
            tpSand$10,                   /* Tempered SandWich  (AWD005)*/~
            spacer$6, space_d$10,        /* Sort Spacer Thickness      */~
            w_adj$6, h_adj$6,            /* Width and Height Adjustment*/~
            phantom$25                   /* Phantom Designator         */

            init(" ") cut$(), ged$(), adj$(), dec$(), view$

            err% = 0%

            tab$(1%) = "GLASS01  "   /* Glass Quantities for Top/Bot   */
            tab$(2%) = "GLASS02  "   /* Glass-Models Top Glass Only    */
            tab$(3%) = "GLASS03  "   /* Glass-Models Two Bottoms       */
            tab$(4%) = "GLASS04  "   /* Glass-Models No Glass Required */
            tab$(5%) = "GLASS05  "   /* Glass-Valid Casement Models 800*/

            tab$(6%) = "GLASS06  "   /* Glass-Models No Glass Label    */
            tab$(7%) = "GLASS07  "   /* Glass-Models for Sliders       */
            tab$(8%) = "GLASS08  "   /* Glass-Models Spec. shapes Skip */
            tab$(9%) = "GLASS09  "   /* Glass-Depts Bilco Not Used     */
            tab$(10%)= "GLASS10  "   /* Glass-Spec CLMR Values Top/Bot */
            tab$(11%)= "GED 002  "   /* GED-Thickness/Spacer Sngl/Dbl  */
            tab$(12%)= "GED 002S "   /* GED Thick/Spacer SDL   (AWD005)*/
            tab$(13%)= "GED 002B "   /* GED Th/Sp 5/32 & 3/16  (AWD005)*/
            tab$(14%)= "PLAN DOOR"   /* Door Model Numbers     (AWD005)*/
            tab$(15%)= "OBS GED"     /* Obscure Glass Codes    (AWD006)*/
            tab$(16%)= "PLAN LAMN"   /* Lamn Glass             (AWD007)*/
            tab$(17%)= "PLAN STC "   /* STC Larson Glass       (CR1173)*/
            tab$(18%)= "PLAN TRIP"   /* Triple Stength         (CR1988)*/
            tab$(19%)= "PLAN QUAD"   /* Quad Strength          (CR1988)*/

        REM - NEAREST 8TH INCH
           sze$ = "1/81/43/81/25/83/47/8         "

        REM - NEAREST 16TH OF AN INCH
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "
/* (AWD005) */
            sdl% = 0%
            trpThick% = 0%
            doorThick% = 0%
            door% = 0%
            sqFeet = 0.00
/* (AWD005\) */
            model$ = str(part$,1%,3%)
/*(AWD005) */
            code$ = model$
            tab% = 14%
            gosub check_code
            if code% = 1% then door% = 1%
/*(AWD005\) */
/* (AWD006) */
            obs% = 0%
            init(" ") code$
            code$ = str(part$,5%,2%)
            tab% = 15%
            gosub check_code
            if code% = 1% then obs% = 1%
/* (AWD006\) */
/* (AWD007) */
            lamn% = 0%
            init(" ") code$
            code$ = str(part$,5%,2%)
            tab% = 16%
            gosub check_code
            if code% = 1% then lamn% = 1%
/* (AWD007\) */
/* (CR1173) */
            stc% = 0%
            init(" ") code$
            code$ = str(part$,5%,2%)                /* Glass */
            tab% = 17%
            gosub check_code
            if code% = 1% then stc% = 1%
/* (CR1173\) */
/* + (CR1988) */
            gls_trip% = 0%
            init(" ") code$
            code$ = str(part$,5%,2%)                /* TripleStrength Glass */
            tab% = 18%
            gosub check_code
            if code% = 1% then gls_trip% = 1%

            gls_quad% = 0%
            init(" ") code$
            code$ = str(part$,5%,2%)                /* QuadStrength Glass */
            tab% = 19%
            gosub check_code
            if code% = 1% then gls_quad% = 1%


/* - (CR1988) */
                                                   /* Begin-Calc Data*/
            g_cnt% = 0%                            /* Count Pieces   */
            opts% = 0%                             /* No Glass Calc  */
            code$ = model$                         /* Set Model Code */
            t1% = 1% : ct% = 0%                    /* No. of Tops    */
            b1% = 1%                               /* No. of Bots    */
            tab% = 11%                             /* Spacer/Thicknes*/
            gosub check_thickness                  /* t_k$,s_s$,s_d$ */
/* (AWD005) */
            tab% = 12%
            gosub sdlThickness
            tab% = 13%
            gosub tripleThickness

/* (AWD005\) */
            tab% = 4%                              /* Models that    */
            gosub check_code                       /* Require No Glas*/
            if code% = 1% then goto exit_sub       /* No Calc        */

REM IF DEPT$ <> "008" THEN GOTO L60000      /* NOT CASEMENT DP*/
/* (AWD008) */
            tab% = 5%
            gosub check_code                    /* Valid Casements*/
            if code% = 1% then dept$ = "008"
REM IF CODE% = 0% THEN GOTO EXIT_SUB   /* NO CALC        */
/*(\AWD008)*/

            t1% = 1% : ct% = 0%                  /* Init Cut Counter  */
            b1% = 1%                                 /* No. of Bots  */
            gosub convert_fields
            gosub case_glass                    /*Multiple Lits 800's*/
REM IF DOOR% = 1% THEN /*(AWD005)*/ GOSUB CHECK_PATIO /* NUM OF PANEL */
            if sc$ = "5" then t1% = 0%          /* No Top Glass      */
            if sc$ = "7" then t1% = 0%          /* No Top Glass (AWD008)*/
                                                /* Process TOP Glass */
            if t1% = 0% then goto L63000        /*No Top Glass Needed*/
               view$ = "TOP" :  cal% = 1%       /* Process All Top   */
               gosub lookup_phantom             /* Calculate Glass   */
               for k% = 1% to t1%
                   ct% = ct% + 1%
                   gosub update_work            /* update cut$()     */
               next k%
               tab% = 2%                        /* See if Top Glass  */
               gosub check_code                 /* Only              */
               if code% = 1% then goto exit_sub /* Glass Only-No Bot */
                                                /* Sash's Top Only   */
               if sc$ = "4" or sc$ ="6" then goto exit_sub
                                                /* Process BOT Glass */
L63000:        if b1% = 0% then goto exit_sub   /* No Bottom Glass   */
                  view$ = "BOT" : cal% = 1%     /* Process Bottom Gls*/
                  if sc$ = "5" then goto L63010
                  if sc$ = "7" then goto L63010 /* (AWD008)          */
                     tab% = 3%                  /* Check Two Bottoms */
                     gosub check_code
                     if code% = 1% then b1% = b1% + 1%
                                                /*    (EWD001)       */
                                                /* Check Two Bottoms */
/* (AWD005) */
L63010:
REM IF MODEL$ <> "312" AND MODEL$ <> "332" THEN GOTO L63020
REM IF DOOR% = 0% THEN GOTO L63020
         goto L63020                                 /* (CR838) */
               if model$ = "312" or model$ = "332" then goto patioBottom
               if model$ = "378" or model$ = "388" then goto patioBottom
                  goto L63020
patioBottom:
                     if hg$ = "42" or hg$ = "37" then b1% = 2%
/* (AWD005\) */
L63020:           gosub lookup_phantom          /* Calc glass        */
                  ct% = 5%                   /* Init for Bottoms     */
                  for k% = 1% to b1%         /* Multiple Bottoms Gls */
                      ct% = ct% + 1%
                      gosub update_work
                  next k%
                                             /* Finished             */
        exit_sub
REM CONVERT SPTYPE% TO CMG$, PIC(00)
REM CALL "SHOSTAT" ("END SPTYPE -> " & CMG$)  STOP

        end

        update_work                         /* Update Work */
            gosub calc_wd                   /* Ctr line Meeting Rail     */
            gosub calc_wd_ht        /* Calculated Width/Height Long Form */
            gosub lookup_double             /* double% = 1% then DBL     */
            gosub lookup_temp               /*  (EWD002)                 */
            gosub calc_double               /* gls_double%, double%      */
            gosub get_ged_adjust            /* w_adj$ / h_adj$           */
            gosub get_muttin                /* muttin$ - Grid Vert/Horiz */
                                            /* lits$ - No. of Lits       */
            gosub change_muttin
            gosub check_ged_glass           /* sandwich$       */
            spacer$ = s_s$                  /* Single Strength */
            spType% = 1%                    /* Single Strength */
            if gls_double% = 1% then spacer$ = s_d$  /* Double Strength  */
            if gls_temp% = 1%   then spacer$ = s_d$  /* Dbl Str (EWD002) */
/* (AWD005) */
            if gls_double% = 1% then spType% = 2%
            if gls_temp%   = 1% then spType% = 3%
            sdl% = 0%
            trpThick% = 0%
            doorThick% = 0%
            strengthened% = 0%
            if str(subpart$,8%,2%) = "11" then sdl% = 1%
/* (AWD006) */
REM IF DOOR% = 0% AND SQFEET > 25 THEN TRPTHICK% = 1%
            if door% = 0% and obs% = 1% and sqFeet > 25 then trpThick% = 1%
            if door% = 0% and obs% = 0% and sqFeet > 25 then doorThick% = 1%
/* (AWD006\) */
            if str(part$,13%,4%)    = "946" then doorThick% = 1%
            if str(subpart$,14%,1%) = "5" then doorThick% = 1%
/* (AWD007) */
            if sdl% = 1% then spacer$ = s_sdl_s$
            if sdl% = 1% then spType% = 4%
            if sdl% = 1% and gls_double% = 1% then spacer$ = s_sdl_d$
            if sdl% = 1% and gls_double% = 1% then spType% = 5%
            if sdl% = 1% and gls_temp% = 1% then spacer$ = s_sdl_d$
            if sdl% = 1% and gls_temp% = 1% then spType% = 6%

            if door% = 0% and trpThick% = 1% then spacer$ = s_3_16$
            if door% = 0% and trpThick% = 1% then spType% = 7%

            if doorThick% = 1% then spacer$ = s_5_32$
            if doorThick% = 1% then spType% = 8%
/* (AWD005\) */
/* (AWD007) */
            if lamn% = 0% then goto checkWindowSize
            spacer$ = s_d$
            spType% = 9%
            if door% = 1% then goto checkDoorSize
REM IF (WIDTH > 48 OR HEIGHT > 72) THEN STRENGTHENED% = 1%
REM IF STRENGTHENED% = 1% THEN SPACER$ = S_3_16$
REM IF STRENGTHENED% = 1% THEN SPTYPE% = 10%
/*(AWD009)*/
REM IF OBS% = 1% AND SQFEET > 25 THEN SPTYPE% = 14%
REM IF OBS% = 1% AND SQFEET > 25 AND STRENGTHENED% = 1% ~
                                               THEN SPTYPE% = 15%
REM IF OBS% = 0% AND SQFEET > 25 THEN SPTYPE% = 12%
REM IF OBS% = 0% AND SQFEET > 25 AND STRENGTHENED% = 1% ~
                                               THEN SPTYPE% = 13%
REM IF SPTYPE% >= 12% THEN SPACER$ = S_5_32$
REM IF SPTYPE% >= 14% THEN SPACER$ = S_3_16$
               goto checkWindowSize
checkDoorSize:
/* (\AWD007) */
REM            IF STR(PART$,13%,4%) > "0794" THEN STRENGTHENED% = 1%
REM            IF STR(PART$,17%,3%) > "794" THEN STRENGTHENED% = 1%
REM            IF STRENGTHENED% = 1% THEN SPACER$ = S_3_16$
REM            IF STRENGTHENED% = 1% THEN SPTYPE% = 10%

checkWindowSize:
            if ty$ = "AZ" then spType% = 11%
            if stc% = 1% then spacer$ = s_3_16$   /* (CR1173)  */
            if stc% = 1% then spType% = 16%       /* (CR1173)  */

            if gls_trip% = 1% then spacer$ = s_5_32$   /* (CR1988)  */
            if gls_trip% = 1% then spType% = 17%       /* (CR1988)  */
            if gls_quad% = 1% then spacer$ = s_3_16$   /* (CR1988)  */
            if gls_quad% = 1% then spType% = 18%       /* (CR1988)  */
/*(CR1988) laminate?*/

            gosub get_spacer_desc               /* space_d$ = Description*/
            gosub check_ged_special              /* ged_special%         */
                                                 /* Test - Special Shapes*/
                                                 /* Glass Calc Values    */
            cut$(ct%,1%) = wd1$                  /* Calculated Width (9) */
            cut$(ct%,2%) = ht1$                  /* Calculated Height(8) */
            cut$(ct%,3%) = wd2$                  /* Calc Center line (9) */
                                                 /* ct% = 1% - 5%  Top   */
                                                 /* ct% = 6% - 10% Bot   */
            adj$(ct%,1%) = w_adj$                /* Width Adjustment     */
            adj$(ct%,2%) = h_adj$                /* Height Adjustment    */

            dec$(ct%,1%) = width_d$              /* Width Decimal        */
            dec$(ct%,2%) = height_d$             /* Height Decimal       */

            if ct% <> 1% then goto L63040
               ged$(1%) = t_k$                   /* Thickness   All Tops */
               ged$(2%) = sandwich$              /* Sandwich             */
               ged$(3%) = spacer$                /* Spacer Single/Double */
               ged$(4%) = lits$                  /* Number of Lits       */
               ged$(5%) = muttin$                /* Muttin Code          */
               ged$(6%) = space_d$               /* Spacer Description   */
L63040:     if ct% <> 6% then goto L63050

               gosub check_ged_glass           /* sandwich$       */
               spacer$ = s_s$                  /* Single Strength */
               spType% = 1%                    /* Single Strength */
               if gls_double% = 1% then spacer$ = s_d$  /* Double Strength  */
               if gls_temp% = 1%   then spacer$ = s_d$  /* Dbl Str (EWD002) */

/* (AWD005) */
               if gls_double% = 1% then spType% = 2%
               if gls_temp%   = 1% then spType% = 3%
               sdl% = 0%
               trpThick% = 0%
               doorThick% = 0%
               strengthened% = 0%
               if str(subpart$,8%,2%) = "11" then sdl% = 1%
/* (AWD006) */
REM IF DOOR% = 0% AND SQFEET > 25 THEN TRPTHICK% = 1%
               if door% = 0% and obs% = 1% and sqFeet > 25 then trpThick% = 1%
               if door% = 0% and obs% = 0% and sqFeet > 25 then doorThick% = 1%
/* (AWD006\) */

               if str(subpart$,14%,1%) = "5" then doorThick% = 1%

               if sdl% = 1% then spacer$ = s_sdl_s$
               if sdl% = 1% then spType% = 4%
               if sdl% = 1% and gls_double% = 1% then spacer$ = s_sdl_d$
               if sdl% = 1% and gls_double% = 1% then x = 5%
               if sdl% = 1% and gls_temp% = 1% then spacer$ = s_sdl_d$
               if sdl% = 1% and gls_temp% = 1% then spType% = 6%

               if trpThick% = 1% then spacer$ = s_3_16$
               if trpThick% = 1% then spType% = 7%

               if doorThick% = 1% then spacer$ = s_5_32$
               if doorThick% = 1% then spType% = 8%
/* (AWD005\) */

               if lamn% = 0% then goto checkWindowSizeBot
               spacer$ = s_d$
               spType% = 9%
               if door% = 1% then goto checkDoorSizeBot
REM IF (WIDTH > 48 OR HEIGHT > 72) THEN STRENGTHENED% = 1%
REM IF STRENGTHENED% = 1% THEN SPACER$ = S_3_16$
REM IF STRENGTHENED% = 1% THEN SPTYPE% = 10%
                  goto checkWindowSizeBot
checkDoorSizeBot:
/* (\AWD007) */
REM IF STR(PART$,13%,4%) > "0794" THEN STRENGTHENED% = 1%
REM IF STR(PART$,17%,3%) > "794" THEN STRENGTHENED% = 1%
REM IF STRENGTHENED% = 1% THEN SPACER$ = S_3_16$
REM IF STRENGTHENED% = 1% THEN SPTYPE% = 10%

checkWindowSizeBot:

               if ty$ = "AZ" then spType% = 11%
               if stc% = 1% then spacer$ = s_3_16$   /* (CR1173)  */
               if stc% = 1% then spType% = 16%       /* (CR1173)  */


               if gls_trip% = 1% then spacer$ = s_5_32$   /* (CR1988)  */
               if gls_trip% = 1% then spType% = 17%       /* (CR1988)  */
               if gls_quad% = 1% then spacer$ = s_3_16$   /* (CR1988)  */
               if gls_quad% = 1% then spType% = 18%       /* (CR1988)  */

               gosub get_spacer_desc             /* space_d$ = Description*/
               ged$(7%)  = t_k$                  /* Thickness All Bottoms */
               ged$(8%)  = sandwich$
               ged$(9%)  = spacer$
               ged$(10%) = lits$
               ged$(11%) = muttin$
               ged$(12%) = space_d$
L63050: return

        check_code
            code% = 0%
            init(" ") readkey$, descr$
            str(readkey$,1%,9%) = tab$(tab%)
            str(readkey$,10%,15%) = code$
            read #1,key = readkey$, using CDE_FMT, descr$,   ~
                          eod goto check_code_done
CDE_FMT:     FMT POS(25), CH(30)
            code% = 1%
        check_code_done
        return

        case_glass
          door8ft% = 0%
          str(readkey$,1%,9%)   = tab$(1%)
          str(readkey$,10%,3%)  = code$                /* Model*/
          if door% <> 0% then gosub case_glass_door    /* (CR838) */
          if door8ft% = 1% then goto convertQty        /* (CR838) */
          str(readkey$,13%,12%) = hg$
          read #1,key = readkey$, using L64000, descr$, eod goto L64020
L64000:       FMT POS(25), CH(30)

convertQty:                                           /*(CR838)*/
            convert str(descr$,1%,2%) to t1%, data goto L64005
L64005:
            convert str(descr$,4%,2%) to b1%, data goto L64010
L64010:
REM IF SC$ <> "4" AND SC$ <> "5" AND SC$ <> "6" THEN RETURN
            if sc$ = "4" then goto case_tso
            if sc$ = "5" then goto case_bso
            if sc$ = "6" then goto case_tso
            if sc$ = "7" then goto case_bso
L64020: return
        case_tso
           t1% = 1%
           b1% = 0%
        return
        case_bso
           t1% = 0%
           b1% = 1%
        return
        case_glass_door                                 /*(CR838)+*/
          door8ft% = 0%
          str(readkey$,13%,3%) = "**"
          str(readkey$,15%,4%) = str(part$,13%,4%)
          read #1,key = readkey$, using L64000, descr$, eod goto not8ftdoor
             door8ft% = 1%
        not8ftdoor
        return                                          /*(CR838)-*/


        convert_fields
            init(" ") cl$, ty$, lt$, hg$, sc$, lk$
            s_width = 0.0 : s_height = 0.0 : s_clmr = 0.0
            cl$       = str(part$,4%,1%)           /* Color            */
            ty$       = str(part$,5%,2%)           /* Glass            */
            lt$       = str(part$,7%,2%)           /* Liting           */
            hg$       = str(part$,9%,2%)           /* Hinge            */
            sc$       = str(part$,11%,1%)          /* Screen           */
            lk$       = str(part$,12%,1%)          /* Locks            */
            gosub std_wd_ht                        /* WD$ - Width Prt  */
                                                   /* HT$ - Height Prt */
            gosub lookup_grid                      /* l_lt$, r_lt$, lt%*/
            gosub lookup_hinge                     /* hgl$, hgr$       */
            a1 = 0.0 : a2 = 0.0
            convert str(part$,13%,3%) to a1, data goto L64030
L64030:
            convert str(part$,16%,1%) to a2, data goto L64040
L64040:
            s_width = a1 + (a2/8.0)                /* Decimal Width    */
            a1 = 0.0 : a2 = 0.0
            convert str(part$,17%,2%) to a1, data goto L64050
L64050:
            convert str(part$,19%,1%) to a2, data goto L64060
L64060:
            s_height = a1 + (a2/8.0)               /* Decimal Height   */

            a1 = 0.0 : a2 = 0.0
            if len(part$) < 22 then return
               convert str(part$,20%,2%) to a1, data goto L64064

               convert str(part$,22%,1%) to a2, data goto L64062
L64062:
               s_clmr = a1 + (a2/8.0)
L64064:     if s_clmr <= 8.0 then s_clmr = 0.0
        return

        std_wd_ht /* Convert Standard Width/Height to Fraction in 8'ths*/
                  /* F0% = Width Fraction, F1% = Height Fraction       */
                  /* WD$ = Width & Fraction, HT$ = Height & Fraction   */
           str(wd$,1%,3%) = str(part$,13%,3%)            /* Width  (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(part$,17%,2%)            /* Height (2) */
           f0% = 0% : f1% = 0%                      /* Build Fractions */
           convert str(part$,16%,1%) to f0%,data goto L64070   /*Width */

           convert str(part$,19%,1%) to f1%,data goto L64070   /*Height*/

           goto L64080
L64070:      f0% = 8% : f1% = 8%
L64080:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%
           str(wd$,4%,1%) = " "          /* Build Width with Fraction  */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
        return

        get_muttin
            vert% = 0% : horz% = 0% : er% = 0%
            if lt$ <> "00" then goto L64082
               muttin$ = "        " : lits$ = "0"
               return

L64082:     call "APCGSLIT" ( part$,            /* MFG Part Number     */~
                              muttin$,          /* Grid Vert/Horiz Code*/~
                              lits$,            /* No. of Lits         */~
                              str(view$,1%,1%), /* T or B              */~
                              vert%,            /* Number of Verticals */~
                              horz%,            /* Number of Horizontal*/~
                              #1,               /* (GENCODES)          */~
                              er% )             /* Error Code          */
            if er% = 0% then return
               if er% = 1% then err% = 4%       /* GED Lits Error      */
               if er% = 2% then err% = 5%       /* GED Hinge Error     */
               if er% = 3% then err% = 6%       /* GED Muttin Error    */
        return

        change_muttin
            mut$ = " "                         /* SWITCH VERT - HORZ */
            if len(muttin$) < 5 then goto L64084
               xx% = pos(muttin$ = "x")
               cc% = pos(muttin$ = "C")
               ii% = (cc% - xx%)
            mut$ = str(muttin$,xx%+1%,ii%) & "x" & str(muttin$,1%,xx%-1%)
L64084:     if lt% > 82% then mut$ = l_lt$
        return

        check_patio                              /* Note - GLASS01   */
                                                 /* One Top, One Bot */
            gosub case_glass                     /* (AWD010) */
                                                 /*  (EWD001) - BEG  */
            check_patio_mod% = 0%       /* Used -if one of tested models (AWD010)  */
            if model$ <> "312" and model$ <> "332" then goto L64090
               check_patio_mod% = 1%             /* (AWD010) */
               if hg$ <> "42" then goto L64090
                  t1% = 1% : b1% = 2%            /* (AWD010) */

/* (AWD005)*/
                                                  /* Two Tops, One Bot*/
L64090:
REM IF MODEL$ <> "313" AND MODEL$ <> "333" THEN GOTO L64100
            if model$ <> "313" then goto L64100
               check_patio_mod% = 1%             /* (AWD010) */
               t1% = 1% : b1% = 1%               /* (AWD010) */
                                                 /* Two Tops, Two Bots*/
L64100:     if model$ <> "314" and model$ <> "334" then goto L64110
               check_patio_mod% = 1%             /* (AWD010) */
               t1% = 2% : b1% = 0%               /* (AWD010) */
                                                 /*  (EWD001) - END  */
/* (AWD005) */
L64110:     if model$ <> "333" then goto L64111
               check_patio_mod% = 1%            /* (AWD010) */
               t1% = 2% : b1% = 1%

L64111:     if model$ <> "378" and model$ <> "388" then goto L64112
               check_patio_mod% = 1%             /* (AWD010) */
               if hg$ <> "42" then goto L64130
                  t1% = 1% : b1% = 2%
L64112:

            if check_patio_mod% = 0% then return
REM if one of tested models and tso or bso set quantity to 1
               if sc$ = "4" then t1% = 1%
               if sc$ = "5" then b1% = 1%
/* (AWD005\) */
        return

        lookup_phantom                       /* Special Code for Model*/
           sqFeet = 0.00                     /* (AWD005) */
/* CR1578          dim1es, dim2es, dim3es = 0       (CUT001) */
           if view$ <> "TOP" then goto L64120 /* 830, 883             */
              phantom$ = "2008"
              if str(hgl$,1%,2%) = "OR"  then phantom$ = "2203"
              if str(hgl$,1%,2%) = "CO"  then phantom$ = "2103"
              if str(hgl$,1%,3%) = "1/4" then phantom$ = "2008"
              if str(hgl$,1%,3%) = "1/3" then phantom$ = "2108"
              goto L64130
L64120:    phantom$ = "3008"
           if str(hgl$,1%,2%) = "OR"  then phantom$ = "3203"
           if str(hgl$,1%,2%) = "CO"  then phantom$ = "3103"
           if str(hgl$,1%,3%) = "1/4" then phantom$ = "3008"
           if str(hgl$,1%,3%) = "1/3" then phantom$ = "3108"
L64130: REM                       /* CAL% 1%=W/H, 2%=W Only, 3%=H Only */
            call "APCCALSB" (cal%,                 /* Calc Type 1%,2%,3*/~
                             part$,                /* Part Number      */~
                             dim1es,               /* (CUT001) */        ~
                             dim2es,               /* (CUT001) */        ~
                             dim3es,               /* (CUT001) */        ~
                             phantom$,             /* Phantom Designato*/~
                             width,                /* Exact width      */~
                             height,               /* Exact Height     */~
                             #2,                   /* AMTBOMCD Equation*/~
                             er% )                 /* Error Code 0%-Ok */
            if er% <> 0% then goto L64140
            opts% = 1%                             /* Glass Calculated */
            g_cnt% = g_cnt% + 1%


/* (AWD005) calc square feet */
REM if the decimal is not zero then add 1 else just take whole number
            intWidth%, intHeight% = 0%
            decWidth, decHeight = 0.00
            decWidth = width - int(width)
            decHeight = height - int(height)
            if decWidth > 0.00 then intWidth%  = int(width) + 1 ~
              else intWidth% = int(width)
            if decHeight > 0.00 then intHeight% = int(height) + 1 ~
              else  intHeight% = int(height)
            sqFeet = 0.00
REM SQFEET = ((WIDTH*HEIGHT) / 144 )
            sqFeet = ((intWidth%*intHeight%) / 144 )
/* (AWD005\) */

            if str(hgl$,1%,2%) <> "CO" and str(hgl$,1%,2%) <> "OR" then  ~
                                                         return
               tab% = 10%
               gosub calc_clmr_1
        return
L64140:     err% = 2%                              /* Equation Error   */
        return

        calc_clmr_1                            /* Both - Cottage/Oriel */
          if s_clmr < 1.0 then return
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = tab$(tab%)
            str(readkey$,10%,15%) = code$
            read #1,key = readkey$, using L64000, descr$,                ~
                                                          eod goto L64170
            t_clmr = 0.0 : b_clmr = 0.0 : tb_clmr = 0.0
            convert str(descr$,1%,8%) to t_clmr, data goto L64180

            convert str(descr$,12%,8%) to b_clmr, data goto L64180

            convert str(descr$,22%,8%) to tb_clmr, data goto L64160
L64160
            if view$ = "TOP" then                           /* GLASS   */~
               height =((s_height/2.0) - t_clmr)                         ~
                                  - (((s_height/2.0) + tb_clmr) - s_clmr)
            if view$ = "BOT" then                           /* GLASS   */~
                height =((s_height/2.0) - b_clmr)                        ~
                                  + (((s_height/2.0) + tb_clmr) - s_clmr)

/* (AWD005) calc square feet */
REM if the decimal is not zero then add 1 else just take whole number
            intWidth%, intHeight% = 0%
            decWidth, decHeight = 0.00
            decWidth = width - int(width)
            decHeight = height - int(height)
            if decWidth > 0.00 then intWidth%  = int(width) + 1 ~
              else intWidth% = int(width)
            if decHeight > 0.00 then intHeight% = int(height) + 1 ~
              else  intHeight% = int(height)
            sqFeet = 0.00
REM SQFEET = ((WIDTH*HEIGHT) / 144 )
            sqFeet = ((intWidth%*intHeight%) / 144 )
/* (AWD005\) */

L64170: return
L64180:      err% = 3%                              /* CLMR Calc Error */
        return

        calc_wd_ht /* Convert Calculated Width/Height With Fract. 16'th*/
                   /* WD1$ = Width & Fract(9), HT1$ = Height & Fract(8)*/
           calc = width                          /* Convert Width  (3) */
           gosub convert_sixteen : wd1$ = "         "
           convert a% to str(wd1$,1%,3%), pic(###)

           if b% = 0% then goto L64190           /* Check For Fraction */
              str(wd1$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L64190:    calc = height                         /* Convert Height (2) */
           gosub convert_sixteen : ht1$ = "        "
           convert a% to str(ht1$,1%,2%), pic(##)

           if b% = 0% then goto L64200           /* Check For Fraction */
              str(ht1$,4%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L64200:    init(" ") width_d$, height_d$
           x = round(width,4)
           convert width to width_d$, pic(00#.####)
           x = round(height,4)
           convert height to height_d$, pic(00#.####)
        return

        calc_wd   /* Convert Center Line Meeting Rail to Long Form     */
           init(" ") wd2$                            /* CLMR For Glass */
           if s_clmr < 1 then return
           calc = s_clmr                             /* Decimal CLMR   */
           gosub convert_sixteen
           convert a% to str(wd2$,1%,3%),pic(###)    /* Size (3) CLMR  */

           if b% = 0% then goto L64210               /* Check Fraction */
              str(wd2$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
L64210: return

        convert_sixteen
            calc = round( calc, 4 )
            a% = int(calc)
            b% = int((calc - a%) * 10000)
            if b% = 0% then goto L64220              /****************/
               d% = int(b%/625)                      /* Conversion of*/
               if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                  b% = d%                            /*  Sixteen's   */
                  if b% <> 16% then goto L64220      /****************/
                     a% = a% + 1% : b% = 0%       /* A% = WHOLE PART */
L64220: return

        check_thickness                            /* Thickness/Spacer */
           init(" ") readkey$, t_k$, s_s$, s_d$
           str(readkey$,1%,9%)   = tab$(tab%)
           str(readkey$,10%,15%) = code$
           read #1,key = readkey$, using L64000, descr$, eod goto L64240
           t_k$ = str(descr$,1%,6%)          /* Thickness of Spacer    */
           s_s$ = str(descr$,9%,6%)          /* Single Strength Spacer */
           s_d$ = str(descr$,17%,6%)         /* Double Strength Spacer */
        return
L64240:    err% = 1%                         /* Thickness Error        */
        return

/* (AWD005) */
        sdlThickness                               /* Thickness/Spacer */
           init(" ") readkey$, t_k$, s_sdl_s$, s_sdl_d$
           str(readkey$,1%,9%)   = tab$(tab%)
           str(readkey$,10%,15%) = code$
           read #1,key = readkey$, using L64000, descr$, eod goto noSdlThick
           s_sdl_s$ = str(descr$,9%,6%)          /* Single Strength Spacer */
           s_sdl_d$ = str(descr$,17%,6%)         /* Double Strength Spacer */
        noSdlThick
        return

        tripleThickness                            /* Thickness/Spacer */
           init(" ") readkey$, t_k$, s_5_32$, s_3_16$
           str(readkey$,1%,9%)   = tab$(tab%)
           str(readkey$,10%,15%) = code$
           read #1,key = readkey$, using L64000, descr$, eod goto noTripThick
           s_5_32$ = str(descr$,9%,6%)          /* Single Strength Spacer */
           s_3_16$ = str(descr$,17%,6%)         /* Double Strength Spacer */
        noTripThick
        return
/* (AWD005\) */

        lookup_grid                                   /* Look Up Grid  */
            lt% = 0%
            init(" ") descr$, readkey$, l_lt$, r_lt$
            str(readkey$,1%,9%)   = "LITING   "
            str(readkey$,10%,15%) = lt$
            read #1,key = readkey$,using L64000, descr$,eod goto L64260
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            l_lt$ = str(descr$,1%,p%-2%) & "   "  /* LEFT MUTTIN - TOP */
            r_lt$ = str(descr$,p%+2%,6%) & "   "  /* RIGHT MUTTIN - BOT*/
            convert lt$ to lt%,data goto L64260

L64260: return
        lookup_hinge                                  /* Look Up Hinge */
            init(" ") hgl$, hgr$, descr$, readkey$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hg$
            read #1,key = readkey$,using L64000, descr$,eod goto L64270
            p% = pos(descr$ = "-")
            if p% = 0% then p% = 4%
            hgl$ = str(descr$,1%,p% - 1%)    /* Left Side Description  */
            hgr$ = str(descr$,p% + 2%)       /* Right Side Description */


/* (AWD004) */
            if hg$ >= "30" and hg$ <= "39" then gosub get_hgl_cde
            if hg$ >= "A6" and hg$ <= "B6" then gosub get_hgl_cde
                   return
/* (AWD004/) */

            if model$ <> "830" and model$ <> "883" and model$ <> "861"   ~
                                                       then return
               p1% = pos(descr$ = "/")
               if p1% = 0% then return
               hgl$ = str(descr$,p1%-1%,3%)
L64270: return

/* (AWD004) */
        get_hgl_cde
            if model$ = "830" or model$ = "883"  then get_hgl_desc
            if model$ = "861" then get_hgl_desc

            if model$ = "843" or model$ = "833"  then get_hgl_desc
            if model$ = "818" or model$ = "808"  then get_hgl_desc

            if model$ = "D30" or model$ = "D83"  then get_hgl_desc
            if model$ = "D61" then get_hgl_desc

            if model$ = "D43" or model$ = "D33"  then get_hgl_desc
            if model$ = "D18" or model$ = "D08"  then get_hgl_desc

            if model$ = "JP3" or model$ = "JT3"  then get_hgl_desc /*(CR1988)*/
             return
        get_hgl_desc
          p1% = pos(descr$ = "/")
          if p1% = 0% then return
          hgl$ = str(descr$,p1%-1%,3%)
        return
/* (AWD004/) */


        lookup_double
            gls_double% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN DBLE"
            str(readkey$,10%,15%) = ty$
            read #1,key = readkey$, eod goto L64280
               gls_double% = 1%
L64280:
            if lamn% = 1% then gls_double% = 1%  /* (AWD007) */
        return
        lookup_temp                               /*   (EWD002)      */
            gls_temp% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN TEMP"
            str(readkey$,10%,15%) = ty$
            read #1,key = readkey$, eod goto not_temp
               gls_temp% = 1%
               gosub lookup_temp_spec
        not_temp
        return
        lookup_temp_spec
            init(" ") readkey$, tpSand$
            p% = 0%
            topTemp%, botTemp% = 0%
            str(readkey$,1%,9%) = "TEMP GED "
            str(readkey$,10%,15%) = ty$
            read #1,key = readkey$, using L64000, descr$, eod goto not_temp_spec

/* (AWD005) */

            if view$ = "TOP" then tpSand$ = str(descr$,1,10)
            if view$ = "BOT" then tpSand$ = str(descr$,18,10)
            convert str(descr$,15%,1%) to topTemp%, data goto badTopTemp

badTopTemp:
            convert str(descr$,30%,1%) to botTemp%, data goto badBotTemp

badBotTemp:
                if str(descr$,3,1) <> "4" then gls_double% = 0%
REM P% = POS(TPSAND$ = "T")
REM P1% = POS(TPSAND$ = "PR")
REM IF P% = 0% AND P1% = 0% THEN GLS_TEMP% = 0%

                if view$ = "TOP" and topTemp% = 0% then gls_temp% = 0%
                if view$ = "BOT" and botTemp% = 0% then gls_temp% = 0%
REM IF VIEW$ = "BOT" THEN GOTO NOT_TEMP_SPEC
REM GLS_TEMP% = 0%

/* (AWD005\) */
        not_temp_spec
        return

                                                    /*   (EWD002)      */
        check_ged_glass                            /* Thickness/Spacer */
           init(" ") readkey$, sandwich$, tty$
           tty$ = ty$ : tty% = 0%
           convert tty$ to tty%, data goto L64300
L64300:
           if double% = 0% then goto L64310
              if tty% = 0% then goto L64310
                 if tty% > 50% then goto L64310
                    tty% = tty% + 50%
                    convert tty% to tty$, pic(00)

L64310:    str(readkey$,1%,9%)   = "GED 001  "
           str(readkey$,10%,15%) = tty$
           read #1,key = readkey$, using L64000, descr$, eod goto L64330
           sandwich$ = str(descr$,1%,10%)
           tty$ = ty$
                                            /* CORRECTION FOR 'CO' AND */
                                            /* OB GLASS TYPES          */
            if ty$ <> "02" and ty$ <> "14" then return
               if str(view$,1%,1%) <> "B" then return
                  if ty$ = "02" then sandwich$ = "IG3OB3CL"
                  if ty$ = "14" then sandwich$ = "IG3CL3CL"
                  gosub lookup_temp
/* (AWD007) */
                  if gls_temp% = 0% then goto updte_bot_sand
                  tab% = 15%
                  code$ = ty$
                  gosub check_code
                  if code% = 1% then goto updte_bot_sand
/* (\AWD007) */
        done_ged_glass
        return
L64330:   sandwich$ = "ERR-07 ***" : err% = 7%
        return
        updte_bot_sand
          sandwich$ = str(descr$,18%,10%)
        return

        get_ged_adjust                       /* Get Width/Height Adj   */
           w_adj, h_adj = 0.0                          /*-O = No Adj.  */
           init(" ") w_adj$, h_adj$, readkey$          /*-T = Top Adj. */
           str(readkey$,1%,9%)   = "GED 004  "         /*-B = Bot Adj. */
           str(readkey$,10%,15%) = model$ & "-" & str(view$,1%,1%)
           read #1,key = readkey$, using L64000, descr$, eod goto L64335
              w_adj$ = str(descr$,1%,6%) : h_adj$ = str(descr$,8%,6%)
L64335:       convert w_adj$ to w_adj, data goto L64340

L64340:       convert h_adj$ to h_adj, data goto L64350

L64350:    if w_adj < 0.0 then convert w_adj to w_adj$, pic(-0.###)      ~
                          else convert w_adj to w_adj$, pic(0.####)
           if h_adj < 0.0 then convert h_adj to h_adj$, pic(-0.###)      ~
                          else convert h_adj to h_adj$, pic(0.####)
        return

        calc_double                      /* Calc Double Strength Glass */
          double% = 0%
          x = 0.0 : y = 0.0 : z = 0.0
          if gls_double% = 1% then goto L64390

          convert str(wd1$,1%,3%) to x, data goto L64370
L64370:
          convert str(ht1$,1%,2%) to y, data goto L64380
L64380:
          if str(wd1$,7%,1%) = "/" then x = x + 1.0
          if str(ht1$,6%,1%) = "/" then y = y + 1.0
          z = (x * y)/ 144.0
          if z > 20.0 then goto L64390               /* Square Feet     */
          if x > 60.0 then goto L64390               /* Width Greater   */
          if y > 60.0 then goto L64390               /* Height Greater  */
          if (x + y) > 100.0 then goto L64390        /* Tot United Inch */
        return
L64390:   double% = 1%
          gls_double% = 1%
        return

        get_spacer_desc                      /* Get Spacer Description */
           init(" ") space_d$, readkey$
           str(readkey$,1%,9%)   = "GED 000  "
           str(readkey$,10%,15%) = str(spacer$,2%,5%)  /* Skip Decimal */
           read #1,key = readkey$, using L64500, space_d$, eod goto L64510
L64500:       FMT POS(25), CH(10)
        return
L64510:    space_d$ = "ERR-08 ***" : err% = 8%
        return

        check_ged_special
            ged_special% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)   = tab$(8%)
            str(readkey$,10%,15%) = model$
            read #1,key = readkey$, eod goto L64520
               ged_special% = 1%
L64520: return

