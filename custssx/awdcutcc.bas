*       ****************************************************************~
*                           ( As of 07/22/2013 - CMG )                 *~
*         Note - Special Code for 1/3,1/3,1/3 Set Equations (17, 20)   *~
*                to Not Applicable.                                    *~
*                                                                      *~
*              - FIN is only Applicable for 600 and 800 Series Products*~
*                ( Special Logic ) Updated with new Lock Codes.        *~
*       ****************************************************************~
*        AWDCUTCC - Calculate ( Width and Height Cuts )       CT$()    *~
*                   ( Primary Subroutine 'CALCULATE' )        CT()     *~
*                     Note (1) 01 thru 100  Contains Cuts              *~
*                              1st - Width Cuts                        *~
*                              2nd - Height Cuts                       *~
*                              MFG% = 0% = Window Cuts Only (MFG)      *~
*                                     1% = Window Parts Only           *~
*                              CW%  = Number of Width Calcs            *~
*                              CH%  = Number of Height Calcs           *~
*                              EQ%  = ( CW% + CH% )                    *~
*                          (2) When Applicable Handle Special Calcs,   *~
*                              for Types (3) thru (9).                 *~
*                          (3) Special Routine to check 'H' Mull for   *~
*                              Models (602 and 603). When Glass Type   *~
*                              is (61) Turn on 'I' Mull Equation.      *~
*                                                                      *~
*                   Save Equation Code                        EQ$()    *~
*                   Save Width and Height Cuts                CT$()    *~
*                   Save Raw Material Assoc. with Cut.        CR$()    *~
*                   Save Additional Raw material data         CR_ADDL$()*~
*                   Save Number of Pieces to Cut.             CP$()    *~
*                   Save Cut Piece Yes or No                  CC$()    *~
*                   Save Cut Descriptions                     CO$()    *~
*                   Save Cut Width and Height Cuts Decimal    CT()     *~
*                   Save Sash Values ( W, H, N )              SH$()    *~
*                                                                      *~
*                   MFG% = 1% Use Width Type Code             TW$      *~
*                             Use Height Type Code            TH$      *~
*                                                                      *~
*                   MFG% = 2% Use Width Type Code Screen      TW$      *~
*                             Use Height Type Code Screen     TH$      *~
*                                                                      *~
*       ****************************************************************~
*                   Equation  Types -  (1) = Width Cut                 *~
*                   (SA_KEY$,5%,1%)    (2) = Height Cut                *~
*                                      (3) = Cottage Cut    (Replace)  *~
*                                      (4) = Oriel Cut      (Replace)  *~
*                                      (5) = 1/3, 1/3, 1/3  (Replace)  *~
*                                      (6) = TSO - Width               *~
*                                      (7) = TSO - Height              *~
*                                      (8) = BSO - Width               *~
*                                      (9) = BSO - Height              *~
*                                      (A) = Width Parts Costing       *~
*                                      (B) = Height Parts Costing      *~
*                                      (C) = Width Grid/Liting Costing *~
*                                      (D) = Height Grid/Liting Costing*~
*                                      (K) = Width casing              *~
*                                      (L) = Height casing             *~
*                                      (M) = Width Bullnose            *~
*                                      (N) = Height bullnose           *~
*       ****************************************************************~
*             Programs and Subroutines that use (THIS) Subroutine.     *~
*                                                                      *~
*             Programs    -  APCCUTSC, APCCUTSX, APCCUTSH, APCCSTSC    *~
*                                                                      *~
*             Subroutines -  APCCUTSB, APCCUT6B, APCCUT7B, APCCUTBS    *~
*                            APCCUTMM, APCCUTWL, APCCUT8B              *~
*                                                                      *~
*             Subs Costing-  APCCSTRM, APCCSTDD                        *~
*                                                                      *~
* 04/03/08 ! (AWD001) mod for SDL glazing bead                   ! CMG *~
*05/21/2012! (AWD002) mod for additional part data               ! CMG *~
*03/04/2013! (AWD003) mod for sash / frame flag & die number     ! CMG *~
*07/22/2013! (AWD004) mod for new dimensions                     ! CMG *~
*07/19/2017! (CR1002) mod for T/B on cut sheets NTX              ! MES *~
*       ****************************************************************

        sub "AWDCUTCC" (partno$,         /* MFG Part Number           */ ~
                        dim1es,          /* (AWD004) dim1es           */ ~
                        dim2es,          /* (AWD004) dim2es           */ ~
                        dim3es,          /* (AWD004) dim3es           */ ~
                        mfg%,            /* 0% = MFG, or 1% = Parts   */ ~
                        cw%,             /* Number of Width Cuts      */ ~
                        ch%,             /* Number of Heights Cuts    */ ~
                        csw%,            /* num of subpart width cuts */ ~
                        csh%,            /* num of subpart height cuts*/ ~
                        eq$(),           /* Equation Codes            */ ~
                        ct$(),           /* Cut Widths and Heights    */ ~
                        cr$(),           /* Raw Material Parts        */ ~
                        cr_addl$(),      /* Addl Raw Material Data (AWD002)*/ ~
                        cp$(),           /* Number of Pieces to Cut   */ ~
                        cc$(),           /* Cut Piece (Y)es, (N)o     */ ~
                        co$(),           /* Cut Descriptions          */ ~
                        ct(),            /* Cut Width/Height Decimal  */ ~
                        sh$(),           /* Sash Type ( W, H, N )     */ ~
                        tw$,             /* Parts and Screen Wd Type  */ ~
                        th$,             /* Parts and Screen Ht Type  */ ~
                        s_f$(),          /* Sash / Frame (AWD003)     */ ~
                        die$(),          /* Die (AWD003)              */ ~
                        adj(),           /* Adjustment amts (AWD003)  */ ~
                        t_b$(),          /* T/B cuts NTX cut sheet(CR1002)*/ ~
                        #1,              /* (APCCUTEQ) Saw Cross-Ref  */ ~
                        #2,              /* (AMTBOMCD) Equation File  */ ~
                        #3,              /* (GENCODES)TABLES          */ ~
                        err% )           /* 0% = Calculate, 1% = Exit */

        dim eq$(500%)8,                  /* Prim Key(s) for (APCCUTEQ)*/ ~
            ct(500%),                    /* Calculated Cuts Decimal   */ ~
            ct$(500%)9,                  /* Calculated Cut Widths     */ ~
            cr$(500%)10,                 /* Associated Raw Material   */ ~
            cr_addl$(500%)5,             /* Additional Part Data (AWD002) */ ~
            cp$(500%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(500%)1,                  /* Cut Piece (Y)es, (N)o     */ ~
            co$(500%)25,                 /* Cut Descriptions          */ ~
            sh$(500%)1,                  /* SASH TYPE ( W, H, N )     */ ~
            s_f$(500%)1,                 /* Sash / Frame Flag (AWD003)*/ ~
            die$(500%)15,                /* Die Number        (AWD003)*/ ~
            adj(500%),                   /* Adjustment amt    (AWD003)*/ ~
            tw$1,                        /* Width Parts Type Code     */ ~
            th$1,                        /* Height Parts Type Code    */ ~
            sz$100,                      /* FRACTIONS FOR 16'THS      */ ~
            f$(3%)3,                     /* With Fin Cuts, Equations  */ ~
            prod$1,                      /* Product Line              */ ~
            mod$3,                       /* Model Code for Product    */ ~
            gl$2,                        /* Glass Code for Product    */ ~
            typ$1,                       /* Equation Type             */ ~
            e$2,                         /* Equation Number           */ ~
            tst$3,                       /* Test Fin                  */ ~
            sash$1,                      /* Check for TSO/BSO/FSO     */ ~
            scan$5,                      /* SCAN VALUE                */ ~
            sa_key$7, sa_key1$8,         /* Primary Equation Key      */ ~
/*AWD002*/  sa_rec$64,                   /* Equation Record           */ ~
            partno$45,                   /* MFG Part Number           */ ~
            phantom$25,                  /* Phantom Designator        */ ~
            cot_or_phantom$25,           /* Phantom Designator        */ ~
            calc$9,                      /* Temp for Conversion       */ ~
            readkey$24,                  /* Lookup Key                */ ~
            desc$32,                     /* GENCODES - DESCRIPTION    */ ~
            co_or$2,                     /* Cottage/Oriel Flag DESC   */ ~
            c_o$1,                       /* Cottage/Oriel Flag        */ ~
            s_eq$2,                      /* Specified Cottage/Oriel   */ ~
            clmr$3,                      /* Specified CLMR            */ ~
            height$3,                    /* Window Height             */ ~
            casing$1,                    /* casing / bullnose         */ ~
            t_b$(500%)1                  /* T/B cut (CR1002)          */ ~


           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "

           init(" ") eq$(), ct$(), cr$(), cp$(), cc$(), co$(), scan$,    ~
                     sh$(), cr_addl$(), s_f$(), die$(), t_b$()

           mat ct = zer
           cot_or_tso_bso% = 0%
           sdl% = 0%                              /* (AWD001) */
           if mfg% = 3% then sdl% = 1%            /* (AWD001) */
           if mfg% = 3% then mfg% = 0%            /* (AWD001) */


           if err% = 1% then goto exit_program
           str(scan$,1%,4%) = str(partno$,1%,4%)
           prod$ = str(partno$,1%,1%)
           if mfg% = 2% then prod$ = "0"
           sash$ = str(partno$,11%,1%)
           c_o$ = "0" : sash% = 0%
           mod$ = str(partno$,1%,3%)                     /* Model Code */
           gl$  = str(partno$,5%,2%)                     /* Glass Code */
           if sash$ = "4" or sash$ = "6" then sash% = 4%   /* FGO, TSO */
           if sash$ = "5" then sash% = 5%                       /* BSO */
           casing$ = str(partno$,31%,5%)

           if mfg% = 1% then goto calc_parts
           if mfg% = 2% then goto calc_screen

           if sash% = 0% then goto L01480
              typ$ = "6"                 /* Calc Width - Model, Color  */
              if sash% = 5% then typ$ = "8"
              j% = 0% : cal% = 2%
REM                gosub lookup_hinge
REM                if c_o$ = "3" or c_o$ = "4" then typ$ = c_o$
REM                   gosub lookup_equatcoor
              gosub calculate

              typ$ = "7"                 /* Calc Height - Model, Color */
              if sash% = 5% then typ$ = "9"
              j% = cw% : cal% = 3%
REM                gosub lookup_hinge
REM                if c_o$ = "3" or c_o$ = "4" then typ$ = c_o$
REM                   gosub lookup_equatcoor
              gosub calculate
              goto exit_program

L01480:    gosub lookup_fin
           typ$ = "1"                    /* Calc Width - Model, Color  */
           j% = 0% : cal% = 2%
           gosub calculate

           typ$ = "2"                    /* Calc Height - Model, Color */
           j% = cw% : cal% = 3%
           gosub calculate

           gosub lookup_hinge             /* Check For Cottage/Oriel   */
           if c_o$ = "0" then goto L01630
              typ$ = c_o$                 /* Calc Cot/Or - Model, Color*/
              j% = cw% : cal% = 3%
              gosub calculate

L01630:    REM if triple% = 0% then goto exit_program
           if triple% = 0% then goto check_casing
              typ$ = "5"                              /* 1/3, 1/3, 1/3 */
              j% = 0% : cal% = 2% : c_o$ = "X"
              gosub calculate
        REM   EQ$(17%),CT$(17%),CR$(17%),CP$(17%),CC$(17%),CO$(17%) = " "
        REM   EQ$(20%),CT$(20%),CR$(20%),CP$(20%),CC$(20%),CO$(20%) = " "
        REM   CT(17%), CT(20%) = 0.0

check_casing:
           if casing$ <> "1" then goto check_bullnose
           typ$ = "K"
           j% = cw% + ch% : cal% = 2%
           gosub calculate

           typ$ = "L"
           j% = cw% + ch% + csw% : cal% = 3%
           gosub calculate


check_bullnose:

           if casing$ <> "2" then goto not_option

           typ$ = "M"
           j% = cw% + ch% : cal% = 2%
           gosub calculate

           typ$ = "N"
           j% = cw% + ch% + csw% : cal% = 3%
           gosub calculate



not_option


           goto exit_program

        calculate
           init(" ") sa_key$, sa_key1$
           str(scan$,5%,1%) = typ$
           str(sa_key$,1%,5%) = scan$
           if mfg% = 2% then goto L01800
              read #1,key > sa_key$, using L01860, sa_rec$, eod goto L02160
              goto L01870
L01800:    str(sa_key1$,1%,1%) = "2"
           str(sa_key1$,2%,5%) = scan$
           read #1,key 1% > sa_key1$, using L01860, sa_rec$, eod goto L02160
           goto L01870
        calculate_next
           read #1, using L01860, sa_rec$, eod goto L02160
L01860:      FMT CH(64)
L01870:    if scan$ <> str(sa_rec$,2%,5%) then goto L02160
               if f% <> 0% then goto L01920
                  tst$ = str(sa_rec$,6%,3%)    /* Skip Fins - Without  */
                  if tst$ = f$(1) or tst$ = f$(2) or tst$ = f$(3)        ~
                     then goto calculate_next
L01920:        e$ = str(sa_rec$,7%,2%)         /* Convert Equation No. */
               convert e$ to i%, data goto L01970
               if typ$ <> "5" then goto L01980
                  if i% = 1% then i% = 17
                  if i% = 2% then i% = 20%
L01970:
L01980:        k% = j% + i%                    /* Set Cut Index Pointer*/
               eq$(k%) = str(sa_rec$,1%,8%)
               cr$(k%) = str(sa_rec$,9%,10%)
               cp$(k%) = str(sa_rec$,29%,2%)
               cc$(k%) = str(sa_rec$,31%,1%)
               cr_addl$(k%) = str(sa_rec$,33%,5%)           /* (AWD002) */
               s_f$(k%) = str(sa_rec$,38%,1%)               /* (AWD003) */
               die$(k%) = str(sa_rec$,39%,15%)              /* (AWD003) */
               t_b$(k%) = str(sa_rec$,62%,1%)               /* (CR1002) */
/*AWD003*/     get sa_rec$ using PD_FMT, adj(k%)
PD_FMT:         FMT POS(54), PD(14,4)
                init(" ") mes$                                /*CR1002*/
                mes$= t_b$(k%)
               if typ$ = "2" then gosub check_hmull

               gosub lookup_equation
               gosub lookup_equatcoor
               if cot_or_tso_bso% = 0% then phantom$ = str(sa_rec$,24%,5%) ~
                  else phantom$ = cot_or_phantom$
               gosub lookup_equascoor

/* (AWD001) */
               if sdl% = 1% then gosub lookup_sdl_glz

               if specified% = 0% then gosub calc_cut                      ~
               else gosub calc_specified

               if e% <> 0% then goto calculate_next
                  calc = width
                  if cal% = 3% then calc = height
                  gosub con_fract
                  ct$(k%) = calc$
                  ct(k%)  = calc
                  sh$(k%) = str(sa_rec$,32%,1%)
                  goto calculate_next
L02160: return

        con_fract                            /* Convert to Sixteenth's */
              calc = round( calc, 4 ) : calc$ = " "
              a% = int(calc) : b% = int((calc - a%) * 10000)
              if b% = 0% then goto L02270                /****************/
                 d% = int(b%/625)                      /* Conversion of*/
                 if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                 b% = d%                               /*  Sixteenth's */
                 if b% <> 16% then goto L02270           /****************/
                    a% = a% + 1% : b% = 0%          /* A% = Whole Part */
L02270:       convert a% to str(calc$,1%,3%), pic(###)
              if b% <> 0% then                                           ~
                              str(calc$,5%,5%) = str(sz$,(b%*5%) - 4%,5%)
        return

        calc_cut
                                               /* 1% -WIDTH AND HEIGHT */
                                               /* 2% -WIDTH ONLY       */
                                               /* 3% -HEIGHT ONLY      */
                  call "APCCALSB" (cal%,           /* CALC TYPE 1%,2%,3*/~
                                   str(partno$,1,25), /* PART NUMBER   */~
                                   dim1es,         /* Dim1es (AWD004)  */~
                                   dim2es,         /* Dim2es (AWD004)  */~
                                   dim3es,         /* Dim3es (AWD004)  */~                                   
                                   phantom$,       /* PHANTOM DESIGNATO*/~
                                   width,          /* EXACT WIDTH      */~
                                   height,         /* EXACT HEIGHT     */~
                                   #2,             /* AMTBOMCD EQUATION*/~
                                   e% )            /* ERROR CODE 0%-OK */
          ct$(k%) = "No Equat"
        return

        lookup_hinge                              /* FOR COTTAGE/ORIEL */
           triple% = 0%                           /* 1/4,1/2 - 1/3,1/3 */
           init(" ") readkey$
           readkey$ = "HINGE    " & str(partno$,9%,2%)
           read #3,key = readkey$, using L02510, desc$, eod goto L02580
L02510:       FMT POS(25), CH(32)
           co_or$ = str(desc$,1%,2%)
           if co_or$ = "CO" then c_o$ = "3"
           if co_or$ = "OR" then c_o$ = "4"
           p% = pos(desc$ = "-")
           if p% < 5% then return
           if str(desc$,p%-4%,3%) = "1/3" then triple% = 1%
L02580: return

        lookup_equation
          init(" ") readkey$
          str(readkey$,1%,15%)  = "EQUATIONS" & prod$ & "-" & typ$ & "-" ~
                                 & e$
          read #3,key = readkey$, using L02650, co$(k%), eod goto L02660
L02650:      FMT POS(25), CH(25)
L02660: return

        lookup_fin                                   /* Check For Fins */
                                      /* FIN Only Applicable for 600's */
                                      /* and 800's Series Products     */
           if str(partno$,1%,1%) = "6" then goto L02750
           if str(partno$,1%,1%) = "8" then goto L02750
               f% = 1%
               return
L02750:    f% = pos("34678CEGRSUVX" = str(partno$,12%,1%)) /* With Fin */
           if f% <> 0% then return
              f% = 0% : f$(1%) = "105" : f$(2%) = "106" /* Without Fin */
                        f$(3%) = "202"                  /* Type, Eq No.*/
        return

        check_hmull
           if gl$ <> "61" then return             /* Only for 'I' Mull */
           if mod$ <> "602" and mod$ <> "603" and mod$ <> "702" and      ~
                                mod$ <> "703" then return
           if i% <> 3% and i% <> 4% then return
           if i% = 3% then cc$(k%) = "N"
           if i% = 4% then cc$(k%) = "Y"
        return

        lookup_equatcoor

          init(" ") clmr$
          clmr$ = str(partno$,20%,3%)
          if str(clmr$,1%,1%) >= "A" then goto not_clmr
          if clmr$ <> "   " and clmr$ <> "000" then return
not_clmr
REM          call "SHOSTAT" (" I am here --> " & clmr$) stop

          cot_or_tso_bso% = 0%
          if sash% = 0% then return

          gosub lookup_hinge
          if c_o$ = "0" then return


          init(" ") readkey$, desc$, cot_or_phantom$
          str(readkey$,1%,15%)  = "EQUATCOOR" & prod$ & "-" & typ$ & "-" ~
                                 & e$
          read #3,key = readkey$, using L02510, desc$, eod goto not_equatcoor
                                                      /* 3 = Cottage */
                                                      /* 4 = Oriel   */
                 if c_o$ = "3" then cot_or_phantom$ = str(desc$,17%,14%)  ~
                    else cot_or_phantom$ = str(desc$,1%,14%)
                 cot_or_tso_bso% = 1%
        not_equatcoor
        return


        lookup_equascoor
          specified% = 0%


          init(" ") readkey$, desc$, s_eq$, clmr$
          clmr$ = str(partno$,20%,3%)         /* CLMR          */
                                              /* Not Specified */
          if str(clmr$,1%,3%) = "   " or str(clmr$,1%,3%) = "000" then return
          if str(clmr$,1%,1%) >= "A" then return


          gosub lookup_hinge
          if c_o$ = "0" then return


          s_clmr,s_factor, r_factor, g_factor = 0.00
REM          convert clmr$ to s_clmr, data goto L02940
REM L02940

          convert str(clmr$,3%,1%) to cdec,data goto L30300

          convert str(clmr$,1%,2%) to calc,data goto L30300

          calc = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
          s_clmr = calc
L30300:

          str(readkey$,1%,15%)  = "EQUASCOOR" & prod$ & "-" & typ$ & "-" ~
                                 & e$
          read #3,key = readkey$, using L02510, desc$, eod goto not_equascoor

               s_eq$ = str(desc$,1%,2%)



               specified% = 1%
        not_equascoor
        return


        calc_specified
              gosub get_glass10
              if s_eq$ = "ST" then gosub calc_top_stile
              if s_eq$ = "SB" then gosub calc_bot_stile

              if s_eq$ = "GT" then gosub calc_top_glaz
              if s_eq$ = "GB" then gosub calc_bot_glaz

              if s_eq$ = "RT" then gosub calc_top_rebar
              if s_eq$ = "RB" then gosub calc_bot_rebar

              if s_eq$ = "BT" then gosub calc_top_cover
              if s_eq$ = "BB" then gosub calc_bot_cover

              if s_eq$ = "FT" then gosub calc_top_frame
              if s_eq$ = "FB" then gosub calc_bot_frame

        return

        calc_top_stile

              gosub get_s_factor
              convert str(desc$,1%,7%) to s_factor, data goto L02955
L02955:
              calc = ((s_height/2.0)-t_clmr)-                         ~
                             (((s_height/2.0)+tb_clmr)-s_clmr)+s_factor
              height = calc
        return

        calc_bot_stile

              gosub get_s_factor
              convert str(desc$,17%,7%) to s_factor, data goto L02965
L02965:
              calc  = ((s_height/2.0)-b_clmr)+                        ~
                             (((s_height/2.0)+tb_clmr)-s_clmr)+s_factor

              height = calc
        return


        calc_top_glaz
              gosub get_g_factor
              convert str(desc$,1%,7%) to g_factor, data goto L03050
L03050:
              calc = ((s_height/2.0)-t_clmr)-                         ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+g_factor
              height = calc
        return

        calc_bot_glaz
              gosub get_g_factor
              convert str(desc$,17%,7%) to g_factor, data goto L03100
L03100:
              calc  = ((s_height/2.0)-b_clmr)+                        ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+g_factor

              height = calc
        return

        calc_top_rebar
              gosub get_r_factor
              convert str(desc$,1%,7%) to r_factor, data goto L03150
L03150:
              calc = ((s_height/2.0)-t_clmr)-                         ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+r_factor
              height = calc
        return

        calc_bot_rebar
              gosub get_r_factor
              convert str(desc$,17%,7%) to r_factor, data goto L03200
L03200:
              calc  = ((s_height/2.0)-b_clmr)+                        ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+r_factor

              height = calc
        return


        calc_top_cover
              gosub get_b_factor
              convert str(desc$,1%,7%) to b_factor, data goto L03300
L03300:
              calc = ((s_height/2.0)-t_clmr)-                         ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+b_factor
              height = calc
        return

        calc_bot_cover
              gosub get_b_factor
              convert str(desc$,17%,7%) to b_factor, data goto L03350
L03350:
              calc  = ((s_height/2.0)-b_clmr)+                        ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+b_factor

              height = calc
        return


        calc_top_frame
              gosub get_f_factor
              convert str(desc$,1%,7%) to f_factor, data goto L03400
L03400:
              calc = ((s_height/2.0)-t_clmr)-                         ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+f_factor
              height = calc
        return

        calc_bot_frame
              gosub get_f_factor
              convert str(desc$,17%,7%) to f_factor, data goto L03450
L03450:
              calc  = ((s_height/2.0)-b_clmr)+                        ~
                    (((s_height/2.0)+tb_clmr)-s_clmr)+f_factor

              height = calc
        return

        get_glass10
            init(" ") readkey$, desc$
            t_clmr, b_clmr, tb_clmr, s_height, calc = 0.00
            str(readkey$,1%,9%) = "GLASS10"
            str(readkey$,10%,3%) = str(partno$,1%,3%)
            read #3, key = readkey$, using L02510, desc$, eod goto glass10_done



                 convert str(desc$,1%,8%) to t_clmr, data goto L02960
L02960:
                 convert str(desc$,12%,8%) to b_clmr, data goto L02970
L02970:
                 convert str(desc$,22%,8%) to tb_clmr, data goto L02980
L02980:
                 gosub convert_height

        glass10_done
        return

        get_s_factor
            init(" ") readkey$, desc$
            str(readkey$,1%,9%) = "EQUATPROF"
            str(readkey$,10%,3%) = str(partno$,1%,3%)
            read #3, key = readkey$, using L02510, desc$, eod goto s_factor_done
        s_factor_done
        return

        get_g_factor
            init(" ") readkey$, desc$
            str(readkey$,1%,9%) = "EQUATGLAZ"
            str(readkey$,10%,3%) = str(partno$,1%,3%)
            read #3, key = readkey$, using L02510, desc$, eod goto g_factor_done
        g_factor_done
        return

        get_r_factor
            init(" ") readkey$, desc$
            str(readkey$,1%,9%) = "EQUATREBA"
            str(readkey$,10%,3%) = str(partno$,1%,3%)
            read #3, key = readkey$, using L02510, desc$, eod goto r_factor_done
        r_factor_done
        return

        get_b_factor
            init(" ") readkey$, desc$
            str(readkey$,1%,9%) = "EQUATBALA"
            str(readkey$,10%,3%) = str(partno$,1%,3%)
            read #3, key = readkey$, using L02510, desc$, eod goto b_factor_done
        b_factor_done
        return

        get_f_factor
            init(" ") readkey$, desc$
            str(readkey$,1%,9%) = "EQUATFRAM"
            str(readkey$,10%,3%) = str(partno$,1%,3%)
            read #3, key = readkey$, using L02510, desc$, eod goto f_factor_done
        f_factor_done
        return

        convert_height
           init(" ") height$
           height$ = str(partno$,17%,19%)
           calc$ = height$ : err% = 0%
           convert str(calc$,3%,1%) to cdec,data goto L03000

           convert str(calc$,1%,2%) to calc,data goto L03000

           calc = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
           s_height = calc
           convert calc to calc$, pic(0000.000)
        return
L03000:    err% = 2%
        return

/* (AWD001) */
        lookup_sdl_glz
          init(" ") readkey$, desc$
          str(readkey$,1%,17%)  = "EQUATSDL " & str(partno$,1%,3%) & "-" & ~
                                   typ$ & "-" & e$
          read #3,key = readkey$, using L02510, desc$, eod goto not_sdl_phan
                 phantom$ = str(desc$,1,14)
        not_sdl_phan
        return

        calc_parts
           f% = 1%                              /* Width Parts Costing */
           typ$ = tw$
           j% = 0% : cal% = 2%                  /* Calc Width Parts    */
           gosub calculate

           typ$ = th$
           j% = cw% : cal% = 3%                 /* Calc Height Parts   */
           gosub calculate
           goto exit_program

        calc_screen
           f% = 1%                              /* Width Screen Costing*/
           typ$ = tw$
           j% = 0% : cal% = 2%                  /* Calc Width SCREEN   */
           gosub calculate

           typ$ = th$
           j% = cw% : cal% = 3%                 /* Calc Height SCREEN  */
           gosub calculate

        exit_program
           mfg% = 0%
        end
