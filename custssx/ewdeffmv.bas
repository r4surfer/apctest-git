        REM *************************************************************~
            *                                                           *~
            * Program Name  - EWDEFFMV                                  *~
            * Creation Date - 11/16/2022                                *~ 
            *                                                           *~
            * Description   - Use input file to make manual updates to  *~
            *                 the Efficiency file.                      *~
            *                                                           *~
            * Driver file   - GENCODE - EFFCOMBIN                       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/16/22 ! New Program - Last Mod Date              ! RDB *~
            *************************************************************                  
        sub "EWDEFFMV"   (eff_proc$,            /* Process Flag           */~
                          sc_yr$,               /* Year                   */~
                          sc_wk$,               /* Week Number            */~
                          sc_day$,              /* Day of the week        */~
						  bb_dte$,              /* Production Date(6)     */~
                          #1,                   /* Master Code File       */~
                          #2,                   /* Efficiency Master File */~
						  #6)                   /* Prodtn Tracking Audit  */

        dim                              /*                            */~
            sc_yr$4,                     /* Year                       */~
            sc_wk$2,                     /* Week Number                */~
            sc_day$                      /* Day of the week            */
 			
        dim                              /*                            */~
            readkey$24,                  /* key for Gencodes           */~
            eff_desc$25,                 /* Gencode Description        */~
            input_dept$3,                /* Eff dept to find           */~
            input_model$3,               /* Eff model in dept to find  */~
            update_dept$3,               /* Dept to be updated         */~
            update_model$3,              /* Model of dept to be updated*/~
			reset_input$1,               /* Reset Input to zero        */~
            cnt$8                        /* Display counts             */
                  
        dim                              /* EWDEFFCY                   */~
            eff_key$13,                  /* Primary KEY                */~
            eff_proc$1,                  /* EFF Process Flag           */~
            eff_wk$2,                    /* Production Week            */~
            eff_day$1,                   /* Production Day             */~
            eff_model$(100%)3,           /* Product Models             */~
            cont_mod$3,                  /* Cont. Head/Sill Model      */~
            effect_unt(100%),            /* Effective Scanned Units    */~
            effect_rmk(100%),            /* Effective Remake Units     */~
            pr_dte$6,                    /* Production Date            */~
            bb_dte$6,                    /* Production Date            */~			
            eff_unt(100%),               /* Product Units              */~
            eff_unts(100%),              /* Product Units              */~
            eff_untss(100%),             /* Product Units              */~
            eff_untpp(100%),             /* Product Units              */~
            eff_unta(100%),              /* Product Units              */~
            eff_untb(100%),              /* Product Units              */~
            eff_untc(100%),              /* Product Units              */~
            eff_value$14                 /* Product Value              */
			
	    dim                              /* Hold EWDEFFCY              */~
		    inp_model$(100%)3,           /* Product Models             */~
            inp_unt(100%),               /* Product Units              */~
            inp_unts(100%),              /* Product Units              */~
            inp_untss(100%),             /* Product Units              */~
            inp_untpp(100%)              /* Product Units              */

        dim ad_key$33, ad_rec$64,        /* Audit Key and Record       */~
            ad_time$8,                   /* Scanned Time               */~
            ad_dte$6,                    /* Scanned Date               */~
            ad_shft$2,                   /* Scanned Shift              */~
            ad_st$2                      /* Scanned Status             */
			
        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 Open, -1 doesn't exist */~
                                         /*   0 if not checked         */~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *  FILE SECTION                                             *~
            *************************************************************                  
REM         select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

REM         select #2,  "EWDEFFCY",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    7, keylen =    13,                   ~
                        alt key  1, keypos =    1, keylen =  19      

            select #22,  "EWDEFFCY",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    7, keylen =    13,                   ~
                        alt key  1, keypos =    1, keylen =  19   						

            filename$ = "EWDEFFCY" : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error
			
        REM *************************************************************~
            *          M a i n   L i n e   P r o c e s s i n g          *~
            *                                                           *~
            *************************************************************                    
            cnt% = 0% : rcnt% = 0% : acnt% =0% : ucnt% = 0% : zcnt% = 0%
                  
REM            eff_proc$ = "P"
REM            sc_yr$    = "2022"      /* set for testing */
REM            sc_wk$    = "45"
REM            sc_day$   = "1"
                  
            gosub read_process_updates
			sc_dept$ = "106"
			gosub calc_scanned_ship
 
            goto exit_program

        REM *************************************************************~
		    * Initialize                                                *~
            *************************************************************
        init_values
			init(" ") eff_model$(), inp_model$(), input_dept$, input_model$, ~ 
			          update_dept$, update_model$, reset_input$ 
			
            mat eff_unt = zer    :  mat eff_unts = zer
            mat eff_untss = zer  :  mat eff_untpp = zer
			
            mat inp_unt = zer    :  mat inp_unts = zer
            mat inp_untss = zer  :  mat inp_untpp = zer
	    return
			
        REM *************************************************************~
		    * Read input and process request                            *~
            *************************************************************
        read_process_updates

            init(" ") readkey$, eff_des$

         read_nxt
            str(readkey$, 1%, 9%)   = "EFFCOMBIN"

            read #1,key > readkey$, using L100005, readkey$, eff_desc$,      ~
                                                           eod goto L199999
L100005:   FMT  CH(24), CH(30)                                                                                             
            
              if str(readkey$,1%,9%) <> "EFFCOMBIN" then goto L199999			
              rcnt% = rcnt% + 1%
			  
			  gosub init_values
						  
              input_dept$   = str(readkey$,10%,3%)
              input_model$  = str(readkey$,13%,3%)
              update_dept$  = str(eff_desc$,1%,3%)
              update_model$ = str(eff_desc$,4%,3%)
			  reset_input$  = str(eff_desc$,7%,1%)
              
              inp_read% = 1%
              sc_dept$ = input_dept$
              gosub read_eff_file
              /* save input if found */
              if eff_found% = 0% then goto L100100

              sc_dept$ = update_dept$
              gosub read_eff_file   /* read for updates */
              if eff_found% = 0% then goto L100100

              gosub process_input
               
              tcnt% = tcnt% + 1%
L100100:

           goto read_nxt
L199999:
        return                 
                                    
        REM **********************************************************~            
            * Read file to find input                                *~
            **********************************************************
                  
        read_eff_file
            call "SHOSTAT" ("Loading Department Data.")
            eff_found% = 0%

            str(eff_key$,1%,1%) = eff_proc$
            str(eff_key$,2%,4%) = sc_yr$
            str(eff_key$,6%,2%) = sc_wk$
            str(eff_key$,8%,1%) = sc_day$
            str(eff_key$,9%,3%) = sc_dept$
            str(eff_key$,12%,2%) = "00"

            read #2,key = eff_key$, eod goto L29999
			
            if inp_read% = 1% then goto L20300

            get #2, using L35040, pr_dte$, eff_proc$, eff_year$,       ~
                    eff_wk$, eff_day$, sc_dept$, eff_shift$,           ~
                    eff_model$(), eff_unt(), eff_unts(), eff_untss(),  ~
                    eff_untpp(), eff_unta(), eff_untb(), eff_untc(),   ~
                    eff_col1, eff_col2, eff_col3, eff_col4, eff_col5,  ~
                    eff_col6, eff_col7, eff_col8, eff_col9, eff_col10, ~
                    eff_col11, eff_col12, eff_col13, eff_hrsm,         ~
                    eff_paym,  eff_glass(), eff_scan, mistake_weight, ~
                    eff_hrse, eff_avg, eff_value
		    goto L20500

L20300:					
            get #2, using L35050, inp_model$(), inp_unt(), inp_unts(), ~
			        inp_untss(), inp_untpp()
		    inp_read% = 0%
					
L20500:					
            eff_found% = 1%  
L29999:
        return

                                    
        REM **********************************************************~            
            *  Process the models to find matches on input and update*~
            **********************************************************
		process_input

            save_index% = 0% : i% = 0% : r% = 0%
          /* find model to change for moving  */
		  
		    for i% = 1% to 100%
                if inp_model$(i%) = input_model$ then goto move_units
                if inp_model$(i%) = " " then goto L39999    /* no model found*/
            next i%
            goto L39999
						
move_units:
         /* find model to be updated    */
		    for r% = 1% to 100%
                if eff_model$(r%) = update_model$ then goto update_units
				if eff_model$(r%) = " " then save_index% = r%
				if save_index% > 0% then goto to_be_added				
			next r%
			/* not found, do add */
			goto L39999

update_units:			
            sc_dept$ = update_dept$
			
			if inp_unt(i%) = 0 and inp_unts(i%) = 0 and ~
			   inp_untss(i%) = 0 and inp_untpp(i%) = 0  then goto L39999	

            eff_unt(r%)   = eff_unt(r%) + inp_unt(i%)
            eff_unts(r%)  = eff_unts(r%) + inp_unts(i%)
            eff_untss(r%) = eff_untss(r%) + inp_untss(i%)
            eff_untpp(r%) = eff_untpp(r%) + inp_untpp(i%)
		  
            gosub dataputrewrite             
            ucnt% = ucnt% + 1%			
	        goto L39999
			
to_be_added:
            sc_dept$ = update_dept$
			
			if inp_unt(i%) = 0 and inp_unts(i%) = 0 and ~
			   inp_untss(i%) = 0 and inp_untpp(i%) = 0  then goto L39999
			
			eff_model$(save_index%) = update_model$
            eff_unt(save_index%)    = inp_unt(i%)
            eff_unts(save_index%)   = inp_unts(i%)
            eff_untss(save_index%)  = inp_untss(i%)
            eff_untpp(save_index%)  = inp_untpp(i%)
			
            gosub dataputrewrite    
            acnt% = acnt% + 1%
			
L39999:		
            if reset_input$ = "R" and inp_model$(i%) > " "   ~
                 then gosub reset_input_dept
				 
            if inp_model$(i%) = " " then cnt% = cnt% + 1%
		return
                                    
        REM **********************************************************~            
            * Rewrite Updated record                                 *~
            **********************************************************
        dataputrewrite
          call "SHOSTAT" ("Updating Department Data")
          init(" ") eff_key$
          str(eff_key$,1%,1%)  = eff_proc$
          str(eff_key$,2%,4%)  = eff_year$
          str(eff_key$,6%,2%)  = sc_wk$
          str(eff_key$,8%,1%)  = sc_day$
          str(eff_key$,9%,3%)  = sc_dept$
          str(eff_key$,12%,2%) = "00"

          read #22, hold, key = eff_key$, eod goto L49999

          put #22, using L35050, eff_model$(), eff_unt(), eff_unts(), ~
                   eff_untss(), eff_untpp()	  

          rewrite #22, eod goto exit_program
L49999:	
        return
		
        REM *************************************************************~
            *  Reset input values to 0                                  *~
            *************************************************************			
        reset_input_dept
		  
		  if inp_unt(i%) = 0 and inp_unts(i%) = 0 and ~
			   inp_untss(i%) = 0 and inp_untpp(i%) = 0  then goto L59999
		  
          sc_dept$ = input_dept$

          init(" ") eff_key$
          str(eff_key$,1%,1%)  = eff_proc$
          str(eff_key$,2%,4%)  = eff_year$
          str(eff_key$,6%,2%)  = sc_wk$
          str(eff_key$,8%,1%)  = sc_day$
          str(eff_key$,9%,3%)  = sc_dept$
          str(eff_key$,12%,2%) = "00"

          read #22, hold, key = eff_key$, eod goto L59999

            get #22, using L35060, eff_unt(), eff_unts(), ~
                   eff_untss(), eff_untpp()	

		    eff_unt(i%)   = 0
		    eff_unts(i%)  = 0
		    eff_untss(i%) = 0
		    eff_untpp(i%) = 0
            put #22, using L35060, eff_unt(), eff_unts(), ~
                   eff_untss(), eff_untpp()	

          rewrite #22, eod goto exit_program
L59999:	
		  zcnt% = zcnt% + 1%
		
		return
		
		REM *************************************************************~
		    * Count units for shipping depts 106 and 108                *~
            *************************************************************
		
        calc_scanned_ship
		   
		   call "SHOSTAT" ("Loading Shipping/Staging")
		   init(" ") ad_key$
		   ss106_tot% = 0% : ss108_tot% = 0%
		   call "DATE" addr("G+",bb_dte$, 1%,ee_dte$,err%)
		   
	       str(ad_key$,1%,6%) = bb_dte$
		   str(ad_key$,7%,3%) = sc_dept$
           read #6, key > ad_key$, using L62450, ad_rec$,                  ~
                                              eod goto calc_scanned_done
           goto calc_read_first
		   
        calc_scanned_nxt
           read #6, using L62450, ad_rec$, eod goto calc_scanned_done
L62450:        FMT CH(64) 

        calc_read_first 
           ad_key$ = str(ad_rec$,19%,33%)
                            /* 1st Check Specified Prod. */
           ad_st$   = str(ad_rec$,32%,2%)          /* Scan Status Code */
           ad_dept$ = str(ad_rec$,25%,3%)          /* Scan Department  */
           ad_shft$ = str(ad_rec$,30%,2%)          /* Scan Shift Code  */
           ad_time$ = str(ad_rec$,52%,8%)          /* Scan Time        */
           ad_dte$  = str(ad_rec$,19%,6%)          /* Scan Date        */
           xx_dte$  = ad_dte$
           if xx_dte$ > ee_dte$ then goto calc_scanned_done
        REM  CHECK DEPARTMENT
REM           if sc_dept$ <> ad_dept$ then goto calc_scanned_nxt
           if ad_dept$ <> "108" and ad_dept$ <> "106"   ~
		        then goto calc_scanned_nxt

L62740:
            hr% = 0%                            /* Scanned Product     */
            convert str(ad_time$,1%,2%) to hr%, data goto L63120
L63120:
            ap$ = str(ad_time$,7%,2%)           /* Scanned AM or PM    */
            if xx_dte$ = ee_dte$ then goto L63230
                                                /*Check Specified First*/
                                                /* All of 1st and 2nd  */
                                                /* part of 3rd shift   */
               if ap$ = "PM" then goto L63260
               if hr% < 7% or hr% = 12% then goto calc_scanned_nxt
                  goto L63260                    /*Check Next Day Second*/
                                                /*Only last part of 3rd*/
                                               /*shift.Midnight to 7 AM*/
L63230:
            if ap$ = "PM" then goto calc_scanned_nxt
            if hr% < 7% or hr% = 12% then goto L63260    /* Midnight to */
                  goto calc_scanned_nxt                 /* 6 59AM      */
          
L63260:    if ad_dept$ <> "108" then goto L63265  
              ss108_tot% = ss108_tot% + 1%    /* Scanned Total       */      
              goto calc_scanned_nxt
                  
L63265:    if ad_dept$ <> "106" then goto calc_scanned_nxt
           if ad_dept$ = "106" and ad_st$ <> "14" then calc_scanned_nxt
              ss106_tot% = ss106_tot% + 1%    /* Scanned Total       */       
              goto calc_scanned_nxt
			  
        calc_scanned_done

           gosub load_shpstg_units
		   
        return			  
		
		REM *************************************************************~
		    * Load units for shipping depts 106 and 108                *~
            *************************************************************	
        load_shpstg_units

  /* Shipping */
          init(" ") eff_key$, eff_model$() 
		  eff_unt(1) = 0% : eff_unts(1) = 0% 
		  eff_untss(1)= 0% : eff_untpp(1) = 0%
		  
          str(eff_key$,1%,1%)  = eff_proc$
          str(eff_key$,2%,4%)  = eff_year$
          str(eff_key$,6%,2%)  = sc_wk$
          str(eff_key$,8%,1%)  = sc_day$
          str(eff_key$,9%,3%)  = "012"
          str(eff_key$,12%,2%) = "00"

          read #22, hold, key = eff_key$, using L35050, eff_model$(),   ~
		       eff_unt(), eff_unts(), eff_untss(), eff_untpp(),         ~	  
		         eod goto L79990

          eff_model$(1)  = "001"
          eff_unt(1)     = ss108_tot%		  
          put #22, using L35050, eff_model$(), eff_unt(), eff_unts(), ~
                   eff_untss(), eff_untpp()	  

          rewrite #22, eod goto L79990
		  
/* Staging */
          init(" ") eff_key$, eff_model$() 
		  eff_unt(1) = 0% : eff_unts(1) = 0% 
		  eff_untss(1)= 0% : eff_untpp(1) = 0%
		  
          str(eff_key$,1%,1%)  = eff_proc$
          str(eff_key$,2%,4%)  = eff_year$
          str(eff_key$,6%,2%)  = sc_wk$
          str(eff_key$,8%,1%)  = sc_day$
          str(eff_key$,9%,3%)  = "082"
          str(eff_key$,12%,2%) = "00"

          read #22, hold, key = eff_key$, using L35050, eff_model$(),   ~
		       eff_unt(), eff_unts(), eff_untss(), eff_untpp(),         ~	  
		         eod goto L79990
				 
          eff_model$(1)  = "999"
          eff_unt(1)     = ss106_tot%	
          put #22, using L35050, eff_model$(), eff_unt(), eff_unts(), ~
                   eff_untss(), eff_untpp()	  

          rewrite #22, eod goto L79999
		  
		  goto load__shpstg_done

L79990:
           comp% = 2%
           hdr$  = "***  STOP   ***"
           msg$(1%) = " - - - - - - - - - - - - - - "
           msg$(2%) = " Shipping failed to load "
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
		   goto load__shpstg_done
L79999:
           comp% = 2%
           hdr$  = "***  STOP   ***"
           msg$(1%) = " - - - - - - - - - - - - - - "
           msg$(2%) = " Staging failed to load "
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))

        load__shpstg_done
		
        return 

		
        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************                  
L35040: FMT                       /* File = (EWDEFFCY)               */~
            CH(06),               /* Production Date                 */~
            CH(01),               /* Process Flag P=Prod S=Scrap     */~
            CH(04),               /* Production Year                 */~
            CH(02),               /* Production Week                 */~
            CH(01),               /* Production Day                  */~
            CH(03),               /* Production Department           */~
            CH(02),               /* Production Shift 00-Default     */~
            100*CH(3),            /* Production Models by Dept       */~
            100*BI(2),            /* Production Scanning Units       */~
            100*BI(2),            /* Production Sample/Displays      */~
            100*BI(2),            /* Production Sash Units           */~
            100*BI(2),            /* Production Part Units           */~
            100*BI(2),            /* Production Extra Units          */~
            100*BI(2),            /* Production Extra Units          */~
            100*BI(2),            /* Production Extra Units          */~
            PD(14,4),             /* Total Effective Unt or Scrap Lbs*/~
            PD(14,4),             /* Reg Hrs or Act. Scrap Lbs       */~
            PD(14,4),             /* OT Hrs or Scrap Lbs/Unit        */~
            PD(14,4),             /* Tot Hrs or Lbs/Unit GOAL        */~
            PD(14,4),             /* Act. Units/Hrs or Tot Mat. Used */~
            PD(14,4),             /* Plan Units or Mat Into Product  */~
            PD(14,4),             /* Var Unit/Hrs or In Process Scrap*/~
            PD(14,4),             /* Labor Dollars or Mistake Scrap  */~
            PD(14,4),             /* Lab Dol/Unit or Mstke Scrap %   */~
            PD(14,4),             /* Lab Dol Goal/Unit or Tot Scrap% */~
            PD(14,4),             /* Var Lab Dol or Tot Scrap% Goal  */~
            PD(14,4),             /* Prod Dol or Tot Scrap Var       */~
            PD(14,4),             /* Labor % of Prod $ or Tot Scrap$ */~
            PD(14,4),             /* Manager/TeamLeaders Hours       */~
            PD(14,4),             /* Manager/TeamLeaders Pay         */~ 
            6*BI(2),              /* Total Scanned For IG 'In House' */~
            PD(14,4),             /* Actual Scanned Units            */~
            PD(14,4),             /* Mstke + In Proc * Unts Weight   */~
            PD(14,4),             /* Eff. Earned Hours or UPMH Goal  */~
            PD(14,4),             /* Avg Weight for All mods in dept */~
            PD(14,4)              /* Eff Value                       */
                  
L35045: FMT POS(20), 100*CH(3), 100*BI(2), 100*BI(2), 100*BI(2), 100*BI(2), ~
            100*BI(2), 100*BI(2), 100*BI(2), PD(14,4), PD(14,4),       ~
            PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),~
            PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),~
            PD(14,4), 6*BI(2), PD(14,4), PD(14,4), PD(14,4), PD(14,4), ~
            PD(14,4)                  

L35050: FMT POS(20), 100*CH(3), 100*BI(2), 100*BI(2), 100*BI(2), 100*BI(2)

L35060: FMT POS(320), 100*BI(2), 100*BI(2), 100*BI(2), 100*BI(2)

        REM **********************************************************           
           comp% = 2%
           hdr$  = "***  STOP   ***"
           msg$(1%) = " - - - - - - - - - - - - - - "
           msg$(2%) = " "
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
      
        REM ********************************************************** 
		
		results_display
              convert rcnt% to cnt$,  pic(########)
              call "SHOSTAT" ("Reads  "  & cnt$)
              STOP
                    
              convert ucnt% to cnt$,  pic(########)
              call "SHOSTAT" ("Updated "  & cnt$)
              STOP
                    
              convert acnt% to cnt$,  pic(########)
              call "SHOSTAT" ("Added  "  & cnt$)
              STOP
                    
              convert cnt% to cnt$,  pic(########)
              call "SHOSTAT" ("No Model to Add/Update  "  & cnt$)
              STOP
			                      
              convert zcnt% to cnt$,  pic(########)
              call "SHOSTAT" ("Reset input to zero  "  & cnt$)
              STOP
			  
              convert tcnt% to cnt$,  pic(########)
              call "SHOSTAT" ("Total  "  & cnt$)
              STOP
        return 
		
        exit_program
		   testing% = 0%
           if testing% = 1% then gosub results_display
		   close #22
           open_error			  
           end
		   