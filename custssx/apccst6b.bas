        REM *************************************************************~
            *            ( Costmate Exception Load )                    *~
            *  Subroutine Name   - APCCST6B                             *~
            *  Creation Date     - 10/02/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - Subroutine to Load and check Costmate*~
            *                                                           *~
            *  Special Note      - Process - 01 = Top Sash Only         *~
            *   (COST PROC)                  02 = Bottom Sash Only      *~
            *                                03 = Fixed Glass Only      *~
            *                                                           *~
            *                                16 - 26 = Standards        *~
            *                                27 = Stock Only Process    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/02/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
	    * 03/27/98 ! Y2K Compliant			  	  ! DJD *~
	    * 11/05/99 ! Mods to fix Costmate Calcs. (EWD001)     ! CMG *~
            *************************************************************

            sub "APCCST6B" ( area$,         /* Area To Assign Def      */~
                             part$,         /* MFG Part Number         */~
                             x_raw$(),      /* Inv Raw Material Part No*/~
                             x_cuts(),      /* Total Inchec in Decimal */~
                             x_cost(),      /* Total Cost Rsw Mat'l Par*/~
                             x_desc$(),     /* Raw Material Description*/~
                             x_units%(),    /* Raw Mat'l Units of Meas */~
                             x_unit(),      /* Raw Mat'l Unit Cost     */~
                             x_vinyl,       /* Raw Mat'l Vinyl Cost    */~
                             x_misc,        /* Raw Mat'l Misc Cost     */~
                             x_total,       /* Raw Mat'l Total Cost    */~
                             x_cnt%,        /* Raw Mat'l Count (Pieces)*/~
                             x_cuts_s(),    /* Tot Inch Decimal Scrap  */~
                             x_cost_s(),    /* Tot Cost Raw Mat'l Scrap*/~
                             x_vinyl_s,     /* Raw Mat'l Vinyl Scrap Co*/~
                             x_misc_s,      /* Raw Mat'l Misc Scrap Cos*/~
                             x_total_s,     /* Raw Mat'l Tot Cost Scrap*/~
                             x_eq$(),       /* Save Type and Eq. No.   */~
                             x_ph$(),       /* Save Eq. Phantom Code   */~
                             x_frt,         /* Save Freight Cost       */~
                             x_vinyl_d,     /* Save Vinyl Discount Amt */~
                             #1,            /*   (APCCUTEQ)            */~
                             #2,            /*   (HNYMASTR)            */~
                             #3,            /*   (HNYQUAN)             */~
                             #4,            /*   (GENCODES)            */~
                             #5,            /*   (AMTBOMCD)            */~
                             #7,            /*   (APCEMPLY)            */~
                             #8,            /*   (APCEQUAT)            */~
                             #9,            /*   (APCCSTHP)            */~
                             #10,           /*   (APCCSTLR)            */~
                             #17,           /*   (APCCSTEX)            */~
                             #18,           /*   (APCSTOCK)            */~
                             x_err% )       /* 0% = Ok, 1% = Error     */


        dim                                                              ~
            part$25,                     /* MFG Part Number            */~
            area$1,                      /* Area for Processing        */~
            cst_key$9,                   /* COSTMATE PRIMARY KEY       */~
            raw$25,                      /* Use for CALC_COST          */~
            vv$3,                        /* Use for Vinyl Test         */~
            apc_mod$3,                   /* Product/Model Codes        */~
            apc_color$1,                 /* Product Color Codes        */~
            apc_assign$1,                /* Costing Assignment Area    */~
            apc_proc$2,                  /* Exception Process Codes    */~
            apc_field$2,                 /* Product Field Code         */~
            apc_codes$1,                 /* CODES N = All, Y = Yes     */~
            apc_codes$(50%)4,            /* Assoc. Codes for Field No. */~
            apc_eq$1,                    /* Linealmate A, B, C, D's    */~
            apc_eq$(25%)3,               /* Selected A, B, C, D's      */~
            apc_hardware$1,              /* Select Hardware            */~
            apc_hardware$(25%)15,        /* Hardware Calcs             */~
            apc_package$1,               /* Select Packaging           */~
            apc_package$(25%)15,         /* Packaging Calcs            */~
            apc_mat_cal1$2,              /* Material Calc Process (1)  */~
            apc_mat_cal2$2,              /* Material Calc Process (2)  */~
            apc_lab_calc$2,              /* Labor Calc Process Code    */~
            apc_mat_val$8,               /* Material Process Value     */~
            apc_lab_val$8,               /* Labor Process Value        */~
            apc_lab_valu$8,              /* Labor UPMH Value           */~
            apc_lab_dept$2,              /* Labor Calc Department Code */~
            apc_scrap$8,                 /* Definition Scrap Percent   */~
            apc_fil$22,                  /* Filler Area                */~
            x_raw$(100%)14,              /* Inv. Raw Material         */ ~
            x_cuts(100%),                /* Total Inches in Decimal   */ ~
            x_cost(100%),                /* Total Cost Raw Material   */ ~
            x_desc$(100%)32,             /* Raw Material Desc         */ ~
            x_units%(100%),              /* Raw Material Units        */ ~
            x_units$(100%)1,             /* Raw Material Units        */ ~
            x_unit(100%),                /* Raw Mat'l Unit Cost       */ ~
            x_cuts_s(100%),              /* Total Inch Decimal Scrap  */ ~
            x_cost_s(100%),              /* Total Cost Raw Mat'l Scrap*/ ~
            x_eq$(100%)3,                /* Save Type and Equation No.*/ ~
            x_ph$(100%)5,                /* Save Eq. Phantom Code     */ ~
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
            tw$1, th$1,                  /* Width and Height Type Code*/ ~
            readkey$50, desc$32,         /* Gencodes Key              */ ~
            scr_prod$1, sav_key$5,       /* Save Key                  */ ~
            cst_type$1,                  /* 0 = Hardware, 1 = Package */ ~
            tb$1, hg$1,                  /* Save Screen and Hinge Code*/ ~
            stock$1,                     /* STOCK FLAG                */ ~
            hp_key$20                    /* Save Hardware and Packag  */ 
            
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


            process% = 0%                     /* Set Single Process Flg*/
            x_err% = 0%                       /* Set Error Code        */
            sav_cnt% = x_cnt%                 /* Save Number Coming In */
            gosub lookup_specials
            cst_key$ = " "
            str(cst_key$,1%,3%) = str(part$,1%,3%)        /* Part No.  */
            str(cst_key$,4%,1%) = str(part$,4%,1%)        /* Color     */
            str(cst_key$,5%,1%) = area$                   /* MFG Area  */
            sav_key$ = str(cst_key$,1%,5%)
        scan_next
            if process% <> 0% then goto exit_sub       /* Only Process */
            gosub scan_data                            /* (1) Special  */
            if hit% = 0% then goto  exit_sub
            gosub load_data
            if hit% = 0% then goto scan_next     /* SKIP SPECIALS      */
            if apc_field$ = "00" then goto L01530  /* WHEN NOT APPLICABLE*/
               gosub check_field
               if field% = 0% then goto scan_next
L01530:     if apc_eq$ = "N" then goto L01600
               gosub load_linealmate                    /* Linealmate */
               if l_max% = 0% then goto scan_next
               gosub compare_cal1
               if compare% <> 1% then scan_next
               gosub get_linealmate
               gosub calc_cost
               
L01600:     if apc_hardware$ = "N" then goto L01670
               gosub load_hardware
               if h_max% = 0% then goto scan_next
               hp_max% = h_max%
               gosub compare_cal1
               if compare% <> 1% then scan_next
               gosub calc_h_p
               
L01670:     if apc_package$ = "N" then goto L01740
               gosub load_package
               if p_max% = 0% then goto scan_next
               hp_max% = p_max%
               gosub compare_cal1
               if compare% <> 1% then scan_next
               gosub calc_h_p
                              
L01740:     goto scan_next
            if apc_mat_cal2$ = "00" then goto L01770
               area% = 4%                                /* SPECIAL    */
               goto exit_sub
L01770:     if apc_lab_calc$ = "00" then goto L01790
               area% = 5%                                /* LABOR      */
            
L01790: exit_sub
        end

        scan_data
            hit% = 0%
            read #17,key > cst_key$,using L01860   , cst_key$,             ~
                                                   eod goto L01890
L01860:        FMT CH(9)
            if sav_key$ <> str(cst_key$,1%,5%) then goto L01890
        hit% = 1%
L01890: return

        load_data
           get #17, using L02320  ,                                        ~
                                 apc_mod$,      /* Prod/Model Code     */~
                                 apc_color$,    /* Prod Color Code     */~
                                 apc_assign$,   /* Area To Assign Def  */~
                                 apc_proc$,     /* Process Code        */~
                                 apc_field$,    /* Field Number Number */~
                                 apc_codes$,    /* N=N/A, Y = Selected */~
                                 apc_codes$(),  /* Selected Codes      */~
                                 apc_eq$,       /* Linealmate E and F  */~
                                 apc_eq$(),     /* Selected Eq's       */~
                                 apc_hardware$, /* Selecte Hardware    */~
                                 apc_hardware$(),/* Selected Hardware  */~
                                 apc_package$,  /* Select Packaging    */~
                                 apc_package$(),/* Selected Packaging  */~
                                 apc_mat_cal1$, /* Material Calc Code 1*/~
                                 apc_mat_cal2$, /* Material Calc Code 2*/~
                                 apc_mat_val$,  /* Material Calc Value */~
                                 apc_lab_calc$, /* Labor Calc Code     */~
                                 apc_lab_val$,  /* Labor Calc Value    */~
                                 apc_lab_valu$, /* Labor UPMH Value    */~
                                 apc_lab_dept$, /* Labor Department    */~
                                 apc_scrap$,    /* Def Scrap % Opt     */~
                                 apc_fil$       /* Filler Area         */
            apc_proc% = 0%
            convert apc_proc$ to apc_proc%, data goto L02170
L02170:
            if apc_proc% <> 27% then goto L02210        /* (EWD001)    */
               gosub lookup_stock
               if stock$ = "N" then hit% = 0%           /* SKIP RECORD */
L02210:     if apc_proc% > 15% then return
               if apc_proc% = 1% and tb$ = "4" then process% = 1%
               if apc_proc% = 2% and tb$ = "5" then process% = 1%
               if apc_proc% = 3% and tb$ = "6" then process% = 1%
            if process% = 0% then hit% = 0%             /* SKIP RECORD */
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L02320:     FMT CH(3),         /* APC_MOD$= Prod/Model Code   MODEL    */~
                CH(1),         /* APC_COLOR$= Prod Color Code COLOR    */~
                CH(1),         /* APC_ASSIGN$   = ASSIGN DEF  COST AREA*/~
                CH(2),         /* APC_PROC$ = Process Code    COST PROC*/~
                CH(2),         /* APC_FIELD$ = Field Number   APC_FIELD*/~
                CH(1),         /* APC_CODES$ = N = ALL, Y = APCCS1SB   */~
                50*CH(4),      /* APC_CODES$() = Selected Codes        */~
                CH(1),         /* APC_EQ$ = Linealmate A, B, C, D's    */~
                25*CH(3),      /* APC_EQ$() = Selected Eq's E and F    */~
                CH(1),         /* APC_HARDWARE$ = Misc Hardware        */~
                25*CH(15),     /* APC_HARDWARE$() = Selected Equations */~
                CH(1),         /* APC_PACKAGE$ = Misc Packaging        */~
                25*CH(15),     /* APC_PACKAGE$ = Selected Equations    */~
                CH(2),         /* APC_MAT_CAL1$ = Material Calc Proc(1)*/~
                CH(2),         /* APC_MAT_CAL2$ = Material Calc Proc(2)*/~
                CH(8),         /* APC_MAT_VAL$ = Material Calc Value   */~
                CH(2),         /* APC_LAB_CALC$ = Labor Calc Process   */~
                CH(8),         /* APC_LAB_VAL$ = Labor Calc Value      */~
                CH(8),         /* APC_LAB_VALU$ = Labor UPMH Value     */~
                CH(2),         /* APC_LAB_DEPT$ = Labor Department     */~
                CH(8),         /* APC_SCRAP$ = Definition Scrap % Opt  */~
                CH(22)         /* Filler Area                          */

        check_field
          field% = 0%
          readkey$ = " "
          str(readkey$,1%,9%)   = "APC_FIELD"
          str(readkey$,10%,15%) = apc_field$
          read #4,key = readkey$, using L02610  , desc$, eod goto L02730
L02610:      FMT POS(25), CH(32)

          bg%, ed% = 0%
          convert str(desc$,14%,2%) to bg%, data goto L02730

          convert str(desc$,17%,2%) to ed%, data goto L02730

          for i% = 1% to 50%
            if str(apc_codes$(i%),1%,1%) = " " then goto L02730
            if str(part$,bg%,ed%) = str(apc_codes$(i%),1%,ed%)           ~
                                                  then goto L02740
          next i%
L02730: return
L02740:   field% = 1%
        return

        load_linealmate
          l_max% = 0%
          for i% = 1% to 25%
            if str(apc_eq$(i%),1%,1%) = " " then goto L02830
               l_max% = l_max% + 1%
          next i%
L02830: return

        load_hardware
          cst_type$ = "0" :  h_max% = 0%
          for i% = 1% to 25%
            if str(apc_hardware$(i%),1%,1%) = " " then goto L02910
               h_max% = h_max% + 1%
          next i%
L02910: return

        load_package
          cst_type$ = "1" :  p_max% = 0%
          for i% = 1% to 25%
            if str(apc_package$(i%),1%,1%) = " " then goto L02990
               p_max% = p_max% + 1%
          next i%
L02990: return

        get_linealmate
            tw$ = "E" : th$ = "F"
            scr_prod$ = str(part$,1%,1%)
            k%, k_max% = x_cnt%
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #4, err%)
                                                    /* 1% = Parts Only */
            call "APCCSTCC" (part$, 1%, cw%, ch%, eq$(), ct$(), cr$(),   ~
                             cp$(), cc$(), co$(), ct(), sh$(), ph$(),    ~
                                           tw$, th$, #1, #5, #4, err%)
            eq% = cw% + ch%
            for i% = 1% to eq%
               for jj% = 1% to l_max%
                  if str(eq$(i%),6%,3%) = apc_eq$(jj%) then goto L03170
               next jj%
               goto L03330

L03170:         if cc$(i%) = " " then goto L03330       /* Skip - No Ref */
                   gosub check_obsolete
                   p% = 1%                            /* Get No. Pieces*/
                   convert cp$(i%) to p%, data goto L03210
L03210:
                   if k_max% = 0% then goto L03270
                   for k% = 1% to k_max%
                       if x_raw$(k%) <> cr$(i%) then goto L03260
                          goto L03320                 /* Found in Memory */
L03260:            next k%
L03270:            k_max% = k_max% + 1%
                   k% = k_max%
                   x_raw$(k%)  = cr$(i%)             /* Raw Material  */
                   x_eq$(k%)   = str(eq$(i%),6%,3%)  /* Type & Eq. No.*/
                   x_ph$(k%)   = ph$(i%)             /* Eq. Phantom No*/
L03320:            x_cuts(k%)  = x_cuts(k%) + ( ct(i%) * p% )
L03330:     next i%
        x_cnt% = k_max%
        return

        scrap_pcnt                           /* LOAD SCRAP PERCENTAGES */
            mat_scrp = 0.0
            readkey$ = " "
            str(readkey$,1%,9%) = "COST 01SP"
            str(readkey$,10%,15%) = str(part$,1%,4%) /* MODEL AN COLOR */
            read #4,key = readkey$,using L03430   , desc$,eod goto L03470
L03430:        FMT POS(25), CH(30)
            convert str(desc$,1%,9%)  to mat_scrp, data goto L03450
L03450:
            mat_scrp = round( mat_scrp / 100.0, 4)
L03470: return

        check_obsolete
          readkey$ = " "    /* When Found Replace with Valid Raw Mat'l */
          str(readkey$,1%,9%)   = "APCMATOBS"
          str(readkey$,10%,15%) = cr$(i%)
          read #4,key = readkey$, using L03540  , cr$(i%), eod goto L03550
L03540:      FMT POS(25), CH(10)
L03550: return

        calc_h_p                             /* Hardware and Packaging */
                                             /* 3's and 4's Only       */
           hp_key$ = all(hex(00))
           str(hp_key$,1%,1%) = cst_type$
           str(hp_key$,2%,4%) = str(part$,1%,4%)
           for jj% = 1% to hp_max%
             if cst_type$ = "0" then                                     ~
                           str(hp_key$,6%,15%) = apc_hardware$(jj%)
             if cst_type$ = "1" then                                     ~
                           str(hp_key$,6%,15%) = apc_package$(jj%)
             i% = sav_cnt% + jj%
             read #9,key = hp_key$, using L03710   , hp_key$, x_desc$(i%), ~
                                   x_units$(i%), x_cost(i%), x_cuts(i%), ~
                                                     eod goto calc_done
L03710:        FMT CH(20), CH(25), CH(1), PD(14,4), PD(14,4)
           x_raw$(i%) = str(hp_key$,7%,14%)
           next jj%
        calc_done
        x_cnt% = sav_cnt% + hp_max%
           for i% = (sav_cnt% + 1%) to x_cnt%
               convert x_units$(i%) to x_units%(i%), data goto L03780
L03780:
               x_unit(i%) = x_cost(i%)          /* Raw Mat'l Unit Cost */
               x_cost(i%) = round(x_cuts(i%) * x_cost(i%), 4)
               vv$ = str(x_raw$(i%),2%,3%)
               if vv$ <> "105" and vv$ <> "110" and vv$ <> "115"         ~
                                                           then goto L03860
                  x_vinyl = round( x_vinyl + x_cost(i%), 4)
                  goto L03880
L03860:        x_misc = round( x_misc + x_cost(i%), 4)

L03880:        x_total    = round(x_total + x_cost(i%), 4)
               x_eq$(i%)  = "N/A"               /* Not Applicable */
               x_ph$(i%)  = "*N/A*"             /* Not Applicalle */
               x_cuts_s(i%) = 0.0               /* Not Applicable */
               x_cost_s(i%) = 0.0               /* Not Applicable */
           next i%
        sav_cnt% = x_cnt%
        return

        calc_cost
            raw_err% = 0%
        REM Load Scrap Percentages
            gosub scrap_pcnt

            for i% = (sav_cnt% + 1%) to x_cnt%
                raw$ = x_raw$(i%)
                vv$ = str(raw$,2%,3%)
                x_units%(i%) = 5%                  /* DEFAULT INCHES   */
                call "APCCST9B" (raw$, x_cuts(i%), x_units%(i%),         ~
                                 x_cost(i%), x_desc$(i%), x_unit(i%),    ~
                                 raw_frt, raw_vinyl, #3, #2, raw_err%)
                if raw_err% = 0% then goto L04120
                   x_desc$(i%) = raw_err$(raw_err%)
                   x_err% = x_err% + 1%                /* COUNT ERRORS */
L04120:         x_total = round(x_total + x_cost(i%), 4)
                x_cuts_s(i%) = round(x_cuts(i%) * mat_scrp, 4)
                x_cost_s(i%) = round(x_cost(i%) * mat_scrp, 4)
                x_total_s = round(x_total_s + x_cost_s(i%), 4)
                if vv$ <> "105" and vv$ <> "110" and vv$ <> "115" then   ~
                                                 goto L04210
                  x_vinyl   = round(x_vinyl + x_cost(i%), 4) /* Vinyl */
                  x_vinyl_s = round(x_vinyl_s + x_cost_s(i%), 4) /*Viny*/
                   goto L04240
L04210:         x_misc    = round(x_misc + x_cost(i%), 4)
                x_misc_s  = round(x_misc_s + x_cost_s(i%), 4)

L04240:         x_frt     = round(x_frt + raw_frt, 4)
                x_vinyl_d = round(x_vinyl_d + raw_vinyl, 4)
            next i%
        sav_cnt% = x_cnt%                   /* Re-Set Limit to Current */
        return

        lookup_specials                               /* Look Up Hinge */
            init(" ") hg$, desc$, readkey$, tb$
            tb$ = str(part$,11%,1%)
            p% = pos("2389" = tb$)
            if p% <> 0% then tb$ = "2"                /* Full Screen   */
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = str(part$,9%,2%)
            read #4,key = readkey$,using L04380 , desc$,eod goto L04430
L04380:        FMT POS(25), CH(30)
            if str(desc$,1%,2%) = "CO"  then hg$ = "1" /* Cottage     */
            if str(desc$,1%,2%) = "OR"  then hg$ = "2" /* Oriel       */
            if str(desc$,1%,3%) = "1/3" then hg$ = "3" /* 1/3 1/3 1/3 */
            if str(desc$,1%,3%) = "1/4" then hg$ = "4" /* 1/4 1/2 1/4 */
L04430: return

        lookup_stock                          /* Check Standard Stock  */
            stock$ = "N"
            readkey$ = all(hex(00))
            str(readkey$,1%,25%) = part$
            read #18,key 1% > readkey$, using L04510   , readkey$,         ~
                                                         eod goto L04540
L04510:        FMT XX(7), CH(32)
            if part$ <> str(readkey$,1%,25%) then goto L04540
               stock$ = "Y"
L04540: return

        compare_cal1
          compare% = 0%
          convert apc_mat_cal1$ to apc_mat_cal1%, data goto L50020
                   
          convert apc_mat_val$ to apc_mat_val, data goto L50020

                                      /* Standard add to value */
          if apc_mat_cal1% <> 0% then goto L50030
             compare% = 1%
          return
                                     
L50030:   if apc_mat_cal1% > 3% then goto L50000
             gosub convert_width     /* Test width            */  
             if err% <> 0% then return       
             if apc_mat_cal1% = 1% then gosub great      
             if apc_mat_cal1% = 2% then gosub less   
             if apc_mat_cal1% = 3% then gosub equal               
L50020:   return
                                     
L50000:   if apc_mat_cal1% > 6% then goto L50010
             gosub convert_height    /* Test width            */ 
             if err% <> 0% then return         
             if apc_mat_cal1% = 4% then gosub great      
             if apc_mat_cal1% = 5% then gosub less   
             if apc_mat_cal1% = 6% then gosub equal               
          return
                                     /* Test width & height   */
L50010:   gosub convert_width
          if err% <> 0% then return        
             if apc_mat_cal1% = 8% then gosub great      
             if apc_mat_cal1% = 7% then gosub less                 
          gosub convert_height
          if err% <> 0% then return          
             if apc_mat_cal1% = 8% then gosub great      
             if apc_mat_cal1% = 7% then gosub less                 
        return

        convert_width
           calc$ = str(part$,13%,4%) : err% = 0%
           convert str(calc$,4%,1%) to cdec,data goto L60140

           convert str(calc$,1%,3%) to calc,data goto L60140

           calc = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
           convert calc to test$, pic(0000.00)
        
           convert test$ to calc, data goto L60150

L60150: return
L60140:    err% = 1%
        return

        convert_height
           calc$ = str(part$,17%,3%) : err% = 0%
           convert str(calc$,3%,1%) to cdec,data goto L60260

           convert str(calc$,1%,2%) to calc,data goto L60260

           calc = calc + (cdec/8.0)           /* CONVERT TO DECIMAL */
           convert calc to test$, pic(0000.00)
           convert test$ to calc, data goto L60250

L60250: return
L60260:    err% = 2%
        return
       
        great
         if calc > apc_mat_val then compare% = 1%
        return
         
        less
         if calc < apc_mat_val then compare% = 1%
        return

        equal
         if calc = apc_mat_val then compare% = 1%
        return