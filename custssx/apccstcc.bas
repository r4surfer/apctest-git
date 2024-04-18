*       ****************************************************************~
*                           ( As of 05/29/2014 - CMG  )                *~
*        Note  - FIN is only Applicable for 600 and 800 Series Products*~
*                ( Special Logic )                                     *~
*       ****************************************************************~
*        Special Version of (APCCUTCC), Phantom No. is returned        *~
*                in PH$(). All Cut Calculations are convert to the     *~
*                nearest 16th of an inch. ( CT(), CT$() )              *~
*                                                                      *~
*        Special Note - Subroutine is only called by two (2)           *~
*                Subroutines. (APCCST1B, AND APCCST6B)                 *~
*                                                                      *~
*        APCCSTCC - Calculate ( Width and Height Cuts )       CT$()    *~
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
*                              is (61) Turn on 'I' Mull Equation fo    *~
*                              Models (702 and 703).                   *~
*                                                                      *~
*                   Save Equation Code                        EQ$()    *~
*                   Save Width and Height Cuts                CT$()    *~
*                   Save Raw Material Assoc. with Cut.        CR$()    *~
*                   Save Number of Pieces to Cut.             CP$()    *~
*                   Save Cut Piece Yes or No                  CC$()    *~
*                   Save Cut Descriptions                     CO$()    *~
*                   Save Cut Width and Height in Decimal      CT()     *~
*                        Convert to Nearest 16th of Inch.              *~
*                   Save Sash Values ( W, H, N )              SH$()    *~
*                   Save Phantom Codes                        PH$()    *~
*                                                                      *~
*                   MFG% = 1% Use Width Type Code for Parts   TW$      *~
*                             Use Height Type Code            TH$      *~
*                                                                      *~
*                   MFG% = 2% Use Width Type Code for Screen  TW$      *~
*                             Use Height Type Code            TH$      *~
*                                                                      *~
*----------!----------------------------------------------------!------*~
*   DATE   !                MODIFICATION                        ! WHO  *~
*----------!----------------------------------------------------!------*~
* 03/27/98 ! y2k compliant                                      !  DJD *~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC            ! CMG *~
*----------!----------------------------------------------------!------*~
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
*       ****************************************************************~
*             Programs and Subroutines that use (APCCSTCC, APCCUTCC)   *~
*                                                                      *~
*             Programs    -  APCCUTSC, APCCUTSX, APCCUTSH, APCCUTXX    *~
*                            APCCST02, APCCST04, APCCST05, APCCSTSC    *~
*                            APCCST06                                  *~
*             Subroutines -  APCCUTSB, APCCUT6B, APCCUT7B, APCCUTBS    *~
*                            APCCUTMM, APCCUTWL, APCCUT8B              *~
*                                                                      *~
*                                                                      *~
*             Subs Costing-  APCCST0B(N), APCCST1B(N), APCCST2B(N)     *~
*                            APCCST3B(N), APCCST4B(N), APCCST6B(N)     *~
*                                                                      *~
*  04/02/98 - y2k compliant djd                                        *~
*                                                                      *~
*                                                                      *~
*       ****************************************************************

        sub "APCCSTCC" (partno$,         /* MFG Part Number           */ ~
                        mfg%,            /* 0% = MFG, or 1% = Parts   */ ~
                        cw%,             /* Number of Width Cuts      */ ~
                        ch%,             /* Number of Heights Cuts    */ ~
                        eq$(),           /* Equation Codes            */ ~
                        ct$(),           /* Cut Widths and Heights    */ ~
                        cr$(),           /* Raw Material Parts        */ ~
                        cp$(),           /* Number of Pieces to Cut   */ ~
                        cc$(),           /* Cut Piece (Y)es, (N)o     */ ~
                        co$(),           /* Cut Descriptions          */ ~
                        ct(),            /* Cut W/H Decimal to 16ths  */ ~
                        sh$(),           /* Sash Type ( W, H, N )     */ ~
                        ph$(),           /* Save Phantom Codes        */ ~
                        tw$,             /* Parts and Screen Type Code*/ ~
                        th$,             /* Parts and Screen Type Code*/ ~
                        #1,              /* (APCCUTEQ) Saw Cross-Ref  */ ~
                        #2,              /* (AMTBOMCD) Equation File  */ ~
                        #3,              /* (GENCODES)TABLES          */ ~
                        err% )           /* 0% = Calculate, 1% = Exit */

        dim eq$(100%)8,                  /* Prim Key(s) for (APCCUTEQ)*/ ~
            ct(100%),                    /* Calc Decimal 16ths Inch   */ ~
            ct$(100%)9,                  /* Calculated Cut Widths     */ ~
            cr$(100%)10,                 /* Associated Raw Material   */ ~
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece (Y)es, (N)o     */ ~
            co$(100%)25,                 /* Cut Descriptions          */ ~
            sh$(100%)1,                  /* SASH TYPE ( W, H, N )     */ ~
            ph$(100%)5,                  /* Save Phantom Codes        */ ~
            tw$1,                        /* Width Parts Type Code     */ ~
            th$1,                        /* Height Parts Type Code    */ ~
            sz$100, zz$100,              /* FRACTIONS FOR 16'THS      */ ~
            f$(3%)3,                     /* With Fin Cuts, Equations  */ ~
            prod$1,                      /* Product Line              */ ~
            mod$3,                       /* Model Code for Product    */ ~
            gl$2,                        /* Glass Code for Product    */ ~
            typ$1,                       /* Equation Type             */ ~
            e$2,                         /* Equation Number           */ ~
            tst$3,                       /* Test Fin                  */ ~
            sash$1,                      /* Check for TSO/BSO/FSO     */ ~
            scan$5,                      /* SCAN VALUE                */ ~
            sa_key$7,                    /* Primary Equation Key      */ ~
            sa_rec$32,                   /* Equation Record           */ ~
            partno$25,                   /* MFG Part Number           */ ~
            phantom$25,                  /* Phantom Designator        */ ~
            calc$9, calc_out$9,          /* Temp for Conversion       */ ~
            readkey$24,                  /* Lookup Key                */ ~
            desc$32,                     /* GENCODES - DESCRIPTION    */ ~
            co_or$2,                     /* Cottage/Oriel Flag DESC   */ ~
            c_o$1                        /* Cottage/Oriel Flag        */

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "
           zz$ = ".0625.125 .1875.25  .3125.375 .4375.50  .5625.625 .6875~
        ~.75  .8125.875 .9375     "

           init(" ") eq$(), ct$(), cr$(), cp$(), cc$(), co$()
           init(" ") scan$, sh$(), ph$()
           mat ct = zer
           if err% = 1% then goto exit_program
           str(scan$,1%,4%) = str(partno$,1%,4%)
           prod$ = str(partno$,1%,1%)
           sash$ = str(partno$,11%,1%)
           c_o$ = "0" : sash% = 0%
           mod$ = str(partno$,1%,3%)                     /* Model Code */
           gl$  = str(partno$,5%,2%)                     /* Glass Code */
           if sash$ = "4" or sash$ = "6" then sash% = 4%   /* FGO, TSO */
           if sash$ = "5" then sash% = 5%                       /* BSO */

           if mfg% = 1% then goto calc_parts
           if mfg% = 2% then goto calc_screen

           if sash% = 0% then goto L01600
              typ$ = "6"                 /* Calc Width - Model, Color  */
              if sash% = 5% then typ$ = "8"
              j% = 0% : cal% = 2%
              gosub calculate

              typ$ = "7"                 /* Calc Height - Model, Color */
              if sash% = 5% then typ$ = "9"
              j% = cw% : cal% = 3%
              gosub calculate
              goto exit_program

L01600:    gosub lookup_fin
           typ$ = "1"                    /* Calc Width - Model, Color  */
           j% = 0% : cal% = 2%
           gosub calculate

           typ$ = "2"                    /* Calc Height - Model, Color */
           j% = cw% : cal% = 3%
           gosub calculate

           gosub lookup_hinge             /* Check For Cottage/Oriel   */
           if c_o$ = "0" then goto L01750
              typ$ = c_o$                 /* Calc Cot/Or - Model, Color*/
              j% = cw% : cal% = 3%
              gosub calculate

L01750:    if triple% = 0% then goto exit_program
              typ$ = "5"                              /* 1/3, 1/3, 1/3 */
              j% = 0% : cal% = 2% : c_o$ = "X"
              gosub calculate
           goto exit_program

        calculate
           str(scan$,5%,1%) = typ$
           str(sa_key$,1%,5%) = scan$
           read #1,key > sa_key$, using L01880, sa_rec$, eod goto L02180
           goto L01890
        calculate_next
           read #1, using L01880, sa_rec$, eod goto L02180
L01880:      FMT CH(32)
L01890:    if scan$ <> str(sa_rec$,2%,5%) then goto L02180
               if f% <> 0% then goto L01940
                  tst$ = str(sa_rec$,6%,3%)    /* Skip Fins - Without  */
                  if tst$ = f$(1) or tst$ = f$(2) or tst$ = f$(3)        ~
                     then goto calculate_next
L01940:        e$ = str(sa_rec$,7%,2%)         /* Convert Equation No. */
               convert e$ to i%, data goto L01990
               if typ$ <> "5" then goto L02000
                  if i% = 1% then i% = 17
                  if i% = 2% then i% = 20%
L01990:
L02000:        k% = j% + i%                    /* Set Cut Index Pointer*/
               eq$(k%) = str(sa_rec$,1%,8%)
               cr$(k%) = str(sa_rec$,9%,10%)
               cp$(k%) = str(sa_rec$,29%,2%)
               cc$(k%) = str(sa_rec$,31%,1%)
               if typ$ = "2" then gosub check_hmull

               gosub lookup_equation
               phantom$ = str(sa_rec$,24%,5%)
               gosub calc_cut
               if e% <> 0% then goto calculate_next
                  calc = width
                  if cal% = 3% then calc = height
                  gosub con_fract
                  ct$(k%) = calc$
                  ct(k%)  = calc_out      /* Converted to Nearest 16th */
                  sh$(k%) = str(sa_rec$,32%,1%)
                  goto calculate_next
L02180: return

        con_fract                            /* Convert to Sixteenth's */
              calc = round( calc, 4 ) : calc$, calc_out$ = " "
              a% = int(calc) : b% = int((calc - a%) * 10000)
              if b% = 0% then goto L02290                /****************/
                 d% = int(b%/625)                      /* Conversion of*/
                 if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                 b% = d%                               /*  Sixteenth's */
                 if b% <> 16% then goto L02290           /****************/
                    a% = a% + 1% : b% = 0%          /* A% = Whole Part */
L02290:       convert a% to str(calc$,1%,3%), pic(###)
              calc_out$ = str(calc$,1%,3%) & ".0000"
              if b% = 0% then goto L02340
                 str(calc$,5%,5%)     = str(sz$,(b%*5%) - 4%,5%)
                 str(calc_out$,4%,5%) = str(zz$,(b%*5%) - 4%,5%)
L02340:       calc_out = 0.0
              convert calc_out$ to calc_out, data goto L02360
L02360:
        return

        calc_cut
                                               /* 1% -WIDTH AND HEIGHT */
                                               /* 2% -WIDTH ONLY       */
                                               /* 3% -HEIGHT ONLY      */
                  call "APCCALSB" (cal%,           /* CALC TYPE 1%,2%,3*/~
                                   partno$,        /* PART NUMBER      */~
                                   0%, 0%, 0%,     /* (CUT001) */        ~
                                   phantom$,       /* PHANTOM DESIGNATO*/~
                                   width,          /* EXACT WIDTH      */~
                                   height,         /* EXACT HEIGHT     */~
                                   #2,             /* AMTBOMCD EQUATION*/~
                                   e% )            /* ERROR CODE 0%-OK */
          ct$(k%) = "No Equat"
          ph$(k%) = str(phantom$,1%,5%)
        return

        lookup_hinge                              /* FOR COTTAGE/ORIEL */
           triple% = 0%                           /* 1/4,1/2 - 1/3,1/3 */
           init(" ") readkey$
           readkey$ = "HINGE    " & str(partno$,9%,2%)
           read #3,key = readkey$, using L02590, desc$, eod goto L02660
L02590:       FMT POS(25), CH(32)
           co_or$ = str(desc$,1%,2%)
           if co_or$ = "CO" then c_o$ = "3"
           if co_or$ = "OR" then c_o$ = "4"
           p% = pos(desc$ = "-")
           if p% < 5% then return
           if str(desc$,p%-4%,3%) = "1/3" then triple% = 1%
L02660: return

        lookup_equation
          init(" ") readkey$
          str(readkey$,1%,15%)  = "EQUATIONS" & prod$ & "-" & typ$ & "-" ~
                                 & e$
          read #3,key = readkey$, using L02730, co$(k%), eod goto L02740
L02730:      FMT POS(25), CH(25)
L02740: return

        lookup_fin                                   /* Check For Fins */
                                      /* FIN Only Applicable for 600's */
                                      /* and 800's Series Products     */
           if str(partno$,1%,1%) = "6" then goto L02830
           if str(partno$,1%,1%) = "8" then goto L02830
               f% = 1%
               return
L02830:    f% = pos("34678CEGRSUVX" = str(partno$,12%,1%)) /* With Fin */
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
