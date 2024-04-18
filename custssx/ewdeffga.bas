        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDEFFGA - New program to calculate  *~
            *                                 scanned glass             *~
            *  Creation Date     - 03/07/99                             *~
            *  Last Modified Date- 01/01/06                             *~
            *  Description       - For the IG or Remake Dept  Calculate *~
            *                      the Scanned Glass Units for a        *~
            *                      Specified Production Day.            *~
            *                                                           *~
            *  Subroutine Used By - (EWDEFFTB)                          *~
            *                                                           *~
            *  Special Notes -                                          *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/07/99 ! New Program for (EWD) -                  ! CMG *~
            * 10/28/02 ! (EWD001) - Mod for new patio door models.! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mod for sub part          ! CMG *~
            *          !                                          !     *~
            *************************************************************

            sub "EWDEFFGA" ( bg_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day - Beg */~
                             ed_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day - End */~
                             rm%(),      /* Scanned Units              */~
                             #1,         /* (APCPLNGR) Remake File     */~
                             #2)         /* (APCPLNGA) Glass Scan File */


       dim                               /* Glass Cnt  - Variables     */~
/*PAR000*/  rm_ky$33, rm_rec$(2%)192,    /* Remake Key and Record      */~
            rm_cnt$16, rm%(6%,3%),       /* Process Counter            */~
	    tot%(6%),                    /* Total For Buckets          */~
            bg_dte$6, ed_dte$6,          /* BG_DTE$ Yesterday, Today   */~
            testdate$10,                 /* test date                  */~
            rm_st$1,                     /* Gls stat '0'=Rmk,          */~
                                         /*  '1'=Shed, '2'=Complete    */~
            rm_model$3,                  /* Model Code                 */~
            rm_gls$2,                    /* Glass Code Type            */~
            rm_bar$9,                    /* Glass Barcode              */~
            rm_lt$6,                     /* Liting Description Grid    */~
            rm_wd_d$8,                   /* Glass Calc Width- Decimal  */~
            rm_ht_d$8,                   /* Glass Calc Height- Decimal */~
            rm_temp$3,                   /* Glass Tempered flag '*'    */~
            lt_comp$(10%)6,              /* Liting Compare Values      */~
            gls_comp$(41%)2,             /* Glass  Compare Values      */~
            rm_num$3,                    /* Remake Number, Start at 00 */~
            rm_time$8,                   /* Time of Last Status Change */~
            ap$2,                        /* 'AM' or 'PM' From time     */~
            rm_reason$2                  /* Remake Glass Reason Code   */
			
       dim                               /* Glass Cnt Rmk - Variables  */~
            rma_ky$12, rma_rec$64,       /* Remake Key and Record      */~
            rma_num$3,                   /* Remake Number              */~ 
            rma_sve$12,                  /* Remake Save Key            */~
            rma_dte$6,                   /* Date Glass Scanned for Rmk */~   
            rma_time$8,                  /* Time Glass Scanned for Rmk */~
            rma_comp$4,                  /* Total Hrs and Mins to Comp */~ 
	    rma_reason$2                 /* Remake Reason              */


       
     	    init(" ") rm_ky$, rm_rec$(), rm_model$, rm_gls$, rm_lt$, rm_num$
            init(" ") rm_wd_d$, rm_ht_d$, rm_temp$, rm_time$, rm_st$
            init(" ") rm_reason$

            testdate$ = bg_dte$
            call "DATFMTC" (testdate$)      

            call "SHOSTAT" ("Analyzing Completed Glass for "& testdate$)
            rm_cnt$  = "Checked [xxxxxx]"
            cnt%     = 0%
            mat rm%  = zer
            mat tot% = zer

            lt_comp$( 1%) = "GCOL  "   : lt_comp$( 5%) = "LEAD  "
            lt_comp$( 2%) = "GDIAM "   : lt_comp$( 6%) = "PRAIR "
            lt_comp$( 3%) = "GDPR  "   : lt_comp$( 7%) = "DIAGRD"
            lt_comp$( 4%) = "GFLO  "   : lt_comp$( 8%) = "DIAFLK"
            lt_comp$( 9%) = "SPCLIT"   : lt_comp$(10%) = "CG    "

          gls_comp$( 1%) = "TP" : gls_comp$(15%) = "PT" : gls_comp$(29%) = "EI"
          gls_comp$( 2%) = "TA" : gls_comp$(16%) = "PU" : gls_comp$(30%) = "EJ"
          gls_comp$( 3%) = "TE" : gls_comp$(17%) = "PW" : gls_comp$(31%) = "EK"
          gls_comp$( 4%) = "TB" : gls_comp$(18%) = "PX" : gls_comp$(32%) = "EL"
          gls_comp$( 5%) = "TL" : gls_comp$(19%) = "PY" : gls_comp$(33%) = "EM"
          gls_comp$( 6%) = "AT" : gls_comp$(20%) = "PZ" : gls_comp$(34%) = "EN"
          gls_comp$( 7%) = "AE" : gls_comp$(21%) = "EA" : gls_comp$(35%) = "EP"
          gls_comp$( 8%) = "TZ" : gls_comp$(22%) = "EB" : gls_comp$(36%) = "EQ"
          gls_comp$( 9%) = "SL" : gls_comp$(23%) = "EC" : gls_comp$(37%) = "ER"
          gls_comp$(10%) = "OT" : gls_comp$(24%) = "ED" : gls_comp$(38%) = "ES"
          gls_comp$(11%) = "TX" : gls_comp$(25%) = "EE" : gls_comp$(39%) = "ET"
          gls_comp$(12%) = "SP" : gls_comp$(26%) = "EF" : gls_comp$(40%) = "EU" 
          gls_comp$(13%) = "PR" : gls_comp$(27%) = "EG" : gls_comp$(41%) = "EV"
          gls_comp$(14%) = "PS" : gls_comp$(28%) = "EH"  

                                          /* 1st Check Specified Prod. */
                                          /* date from 7 AM until Mid. */
            str(rm_ky$,1%,6%) = bg_dte$

            read #1,key 1% > rm_ky$, using L64000  , rm_rec$(),          ~
                                                     eod goto exit_sub           
            goto L64010
        analysis_done
            init(" ") rm_ky$, rm_rec$(), rm_model$, rm_gls$, rm_lt$, rm_num$
            init(" ") rm_wd_d$, rm_ht_d$, rm_temp$, rm_time$, rm_st$
            init(" ") rm_reason$
            rma_flag% = 0%	               /* Flag for Org or Rmk     */
                                               /* 0=Org, 1=Rmk, & 2=Extra */  
            hr%       = 0%                     /* Hour Completed          */
	    rm_wd_d%  = 0%                     /* Glass Width Decimal     */
	    rm_ht_d%  = 0%                     /* Glass Height Decimal    */   
            k%        = 1%	               /* Array for Org or Rmk    */
                                               /* 1 = Org  2= Rmk         */			
            
                read #1,  using L64000  , rm_rec$(), eod goto exit_sub  
             
L64000:        FMT 2*CH(192)                           /*(PAR000) */

L64010: 
               cnt% = cnt% + 1%
               if mod(cnt%,100%) <> 0 then goto L64020
                  convert cnt% to str(rm_cnt$,10%,6%), pic(######)
                  print at(02,33%);hex(84);rm_cnt$;

L64020:     rm_ky$     = str(rm_rec$(),7%,27%)
            rm_bar$    = str(rm_rec$(),22%,9%)
            rm_st$     = str(rm_rec$(),13%,1%)
            rm_num$    = str(rm_rec$(),31%,3%)			
            rm_reason$ = str(rm_rec$(),34%,2%)
	    rm_time$   = str(rm_rec$(),14%,8%)			
            rm_model$  = str(rm_rec$(),72%,3%)
            rm_gls$    = str(rm_rec$(),77%,2%)
            rm_lt$     = str(rm_rec$(),79%,6%)
            rm_wd_d$   = str(rm_rec$(),191%,8%)
            rm_ht_d$   = str(rm_rec$(),199%,8%)
            rm_temp$   = str(rm_rec$(),219%,1%)


            str(rma_ky$,1%,9%) = rm_bar$               /* Set Rmk Barcode & Rmk Num */
            str(rma_ky$,10%,3%) = "000"                /* for reading on Rmks.      */	

            rma_sve$ = str(rma_ky$,1%,9%)		/* Save BARCODE to copare if */
	                                                /* finished reading.         */	
												                                               
            if str(rm_ky$,1%,6%) > ed_dte$ then goto exit_sub
            if rm_st$ <> "2" then goto analysis_done        /* '2' = Complete, only */
							    /* want glass complete  */
            			
	   convert str(rm_wd_d$,1%,3%) to rm_wd_d%, data goto L64140

L64140:
	   convert str(rm_ht_d$,1%,3%) to rm_ht_d%, data goto L64141
		   
L64141:
           convert str(rm_time$,1%,2%) to hr%, data goto L64142

L64142:
           if str(rm_num$,3%,1%) <> "0" then goto analysis_done1
			
           ap$ = str(rm_time$,7%,2%)            /* Scanned 'AM' or 'PM'  */
           if str(rm_ky$,1%,6%) = ed_dte$ then goto L64150
                                                /* Check Current Scan Day*/
                                                /* First. 1st, 2nd and   */
                                                /* part of 3rd shift     */
                                                /* 7 AM to 12 Midnight   */
               if ap$ = "PM" then goto L64250   /* Part of Prev Day Prod.*/
                  if hr% < 7% or hr% = 12% then goto analysis_done
                  goto L64250
                                                /* Second Check Next Day */
                                                /* Scanned Data. Last    */
                                                /* part of 3rd Shift.    */
                                                /* Midnight to 6 59 'AM' */
L64150:    if ap$ = "PM" then goto analysis_done  
           if hr% < 7% or hr% = 12% then goto L64250 /* Midnight to 6 59A*/
              goto analysis_done                     /* Current Day      */

L64250:     init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN SHAP"
            str(readkey$,10%,13%) = rm_model$   
            read #2,key = readkey$, eod goto L64030
                              
                goto L64050   
                                                           /*  (EWD001)  */
L64030:    REM if rm_model$ < "311" or rm_model$ > "314" then goto L64040
            if rm_model$ < "311" or rm_model$ > "314" and                 ~
               rm_model$ < "332" or rm_model$ > "334" then goto L64040

              if rm_temp$ = "*" then goto L64080  
                                                           
L64050:        rm%(5%,k%) = rm%(5%,k%) + 1%
            goto L64300
                
L64040:     for i% = 1% to 41%
                if gls_comp$(i%) = rm_gls$ then goto L64050
            next i%


            for i% = 1% to 10%
                if lt_comp$(i%) = rm_lt$ then goto L64050
            next i%
            
            rm_lt1$ = str(rm_lt$,1%,1%) & str(rm_lt$,4%,1%)

               if rm_wd_d% < 40% and rm_ht_d% < 40% then goto L64080
           	  if rm_lt1$ = "00" then rm%(1%,k%) = rm%(1%,k%) + 1%   ~
                                    else rm%(2%,k%) = rm%(2%,k%) + 1%
                   goto L64300

L64080:     if rm_lt1$ = "00" then rm%(3%,k%) = rm%(3%,k%) + 1%            ~ 
                              else rm%(4%,k%) = rm%(4%,k%) + 1%

            
L64300:     if rma_flag% = 0% then goto analysis_done                      
               if rma_flag% = 1% then goto analysis_done1                      
                  if rma_flag% = 2% then goto L64265                      
           
        analysis_done1
	    init(" ") rma_rec$, rma_time$, rma_dte$, rma_comp$ 
            init(" ") jdate3$, xx_dte$, rma_num$
	     	
            rma_flag% = 1%                        /* Remake Flag          */
                                /* If Flag = 0 then goto analysis_done    */
                                /* else goto analysis_done1               */  
            sc_hr%  = 0%                          /* Scan Hour            */
            sc_mn%  = 0%                          /* Scan Minute          */
            co_hr%  = 0%                          /* Complete Hour        */
            co_mn%  = 0%                          /* Complete Minute      */ 
            to_hr%  = 0%                          /* Total Hour           */
            to_mn%  = 0%                          /* Total Minute         */ 
            ad_mn%  = 0%                          /* Adjust Minute        */
            ad_mn1% = 0%                          /* Adjust Minute 1      */
            j3%     = 0%                          /* Julian Rmk Date      */			 
            k%      = 2%                          /* Array for Org or Rmk */
                                                  /* 1 = Org  2= Rmk      */  
			            			 
            read #2 ,key 1% > rma_ky$, using L64005  , rma_rec$,           ~
                                                 eod goto analysis_done
L64005:		FMT CH(64)
            
            cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L64006
              convert cnt% to str(rm_cnt$,10%,6%), pic(######)
              print at(02,33%);hex(84);rm_cnt$;
L64006:						  
            rma_ky$     = str(rma_rec$,7%,12%)
            rma_num$    = str(rma_rec$,16%,3%)
            rma_dte$    = str(rma_rec$,1%,6%)
            rma_reason$ = str(rma_rec$,37%,2%)
	    rma_time$   = str(rma_rec$,19%,8%)
	    rma_comp$   = str(rma_rec$,30%,4%)			
             
			                   /* if <> then done reading.     */
            if str(rma_ky$,1%,9%) <> rma_sve$ then goto analysis_done

                                                  /* sc_hr & sc_mn = scan */
                                                  /* hour and minute      */
            convert str(rma_time$,1%,2%) to sc_hr%, data goto L64261

L64261:
            convert str(rma_time$,3%,2%) to sc_mn%, data goto L64262

L64262:                                           /* co_hr & co_mn = complete*/
                                                  /* hour and minute         */
            convert str(rma_comp$,1%,2%) to co_hr%, data goto L64263

L64263:
            convert str(rma_comp$,3%,2%) to co_mn%, data goto L64264

L64264:     if str(rma_num$,3%,1%) = "1" then goto add_extra
L64265:        rma_flag% = 1%                        /* Remake Flag          */
               k%      = 2%                          /* Array for Org or Rmk */
                                                     /* 1 = Org  2= Rmk      */  

             			            /* Dont count these reason codes */
                                            /* (EWD0005)                     */
            if rma_reason$ > "25" and rma_reason$ < "50" then k% = 3%
                                                   /* Total hours & minutes */
            to_hr%  = sc_hr% + co_hr%
	    to_mn%  = sc_mn% + co_mn%
			
    		                  /* Convert to Julian date so can add to day   */
				  /* if hour > 24.  */
            call "DATE" addr("GJ", str(rma_dte$,,6%), str(jdate3$,,5%), x%)
            call "DATJULCV" (jdate3$) 
  		
            convert str(jdate3$,5%,3%) to j3%, data goto L64260

L64260:   if to_mn% < 60% then goto L64280
		ad_mn%  = to_mn% / 60%
		ad_mn1% = mod(to_mn%,60%)
	
		to_hr%  = to_hr% + ad_mn%
		to_mn%  = ad_mn1%
			
L64280:   if to_hr% < 24% then goto L64270   /* if < 24 then dont add to day */
     	        to_hr% = to_hr% - 24%
                j3%    = j3% + 1%
          if to_hr% > 24% then goto L64280   /* if > 24 add to day again     */
			
	                                  /* Convert date back, so can    */
					  /* compare beg & end date.      */
L64270:     convert j3% to str(jdate3$,5%,3%), pic(000)
              
	    call "DATJULCV" (jdate3$)
            call "DATE" addr("JG", str(jdate3$,,5%), xx_dte$, x%)

	                             /* if xx_dte$ < or > than beg or ed*/
                                     /* date values then dont count.    */			  
            if xx_dte$ < bg_dte$ then goto analysis_done1
            if xx_dte$ > ed_dte$ then goto analysis_done1
	    if xx_dte$ = bg_dte$ then goto L64275
	    if xx_dte$ = ed_dte$ then goto L64290
                                              /* Check Current Scan Day*/
                                              /* First. 1st, 2nd and   */
                                              /* part of 3rd shift     */
                                              /* 7 AM to 12 Midnight   */
                                              /* Part of Prev Day Prod.*/
L64275:    if to_hr% < 7% then goto analysis_done1   
               goto L64250
                                              /* Second Check Next Day */
                                              /* Scanned Data. Last    */
                                              /* part of 3rd Shift.    */
L64290:    if to_hr% < 7% then goto L64250    /* time to 6 59A         */
              goto analysis_done1             /* Current Day           */
 
                                       

        add_extra
           rma_flag% = 2%                        /* Remake Flag          */
           k%      = 1%                          /* Array for Org or Rmk */
                                                 /* 1 = Org  2= Rmk      */
            if rma_dte$ < bg_dte$ then goto L64265
            if rma_dte$ > ed_dte$ then goto L64265

	    if rma_dte$ = bg_dte$ then goto L64285
	    if rma_dte$ = ed_dte$ then goto L64295
                                              /* Check Current Scan Day*/
                                              /* First. 1st, 2nd and   */
                                              /* part of 3rd shift     */
                                              /* 7 AM to 12 Midnight   */
                                              /* Part of Prev Day Prod.*/
L64285:    if sc_hr% < 7% then goto L64265  
               goto L64250
                                              /* Second Check Next Day */
                                              /* Scanned Data. Last    */
                                              /* part of 3rd Shift.    */
L64295:    if sc_hr% < 7% then goto L64250    /* time to 6 59A         */
              goto L64265                     /* Current Day           */


         exit_sub
         end
