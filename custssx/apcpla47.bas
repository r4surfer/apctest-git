*       ****************************************************************~
*                            ( L i n e a l m a t e )                   *~
*                     Called by (APCPLN51) or (APCPLN47)               *~
*                           ( As of 11/17/98 - RHH )                   *~
*        APCPLA47 - Create File with Data for Saw Optimization         *~
*                   (Same as 'APCPLC42') Used by Screen Department.    *~
*                                                                      *~
*            Note - Primary Subroutine 'BUILD_SAW_RECS'                *~
*                   (1) For the Screen Department                      *~
*                   (2) MFG% is Set to '2%' for Level Two for Screens. *~
*                       Special Mod to (APCCUTCC)                      *~
*                                                                      *~
*            Note - Special Mod to handle the change for DT_REF$8 and  *~
*                   DT_SEQ$5. Future need to change File Layout to     *~
*                   handle size changes.                               *~
*                                                                      *~
*            Note - Modification made to support new Bilco machines    *~
*                   Added fields 14 and 15. To set the Machine Codes.  *~
*                                                                      *~
*            Note - When changes are made to the following Tables, the *~
*                   subroutine will need to be modified.               *~
*                   BUILD_DESCRIPT uses (SCREEN,LOCK,HINGE) Tables     *~
*                                                                      *~
*            Note - Special mod for Various Colors for Screens.        *~
*                   A = 2 (White) - B = 2 (White) - C = 2 (White)      *~
*                   D = 2 (White) - E = 2 (White) - F = 6 (Beige)      *~
*                   ( SCR_PART$ )                                      *~
*           05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
*           04/10/2105! (IM5733) mod to turn on linealmate and use     *~
*                                data from AWDPLNSR              ! MES *~
*       ****************************************************************

        sub "APCPLA47" (size%,           /* Batch Size (No. Windows)  */ ~
                        sched%,          /* Starting Schedule Number  */ ~
                        scr_dte$,        /* Production Date UnFormatted */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6,              /* (AMTBOMIF) Validity File  */ ~
                        #7 )     /* (IM5733)(AWDPLNSR) ASM Cut Sheet  */

        dim readkey$24,                  /* GENCODES Primary Key      */ ~
            desc$32,                     /* GENCODES Description      */ ~
            sched$3,                     /* Schedule Numbers          */ ~
            tsched$3,                    /* Starting Schedule Number  */ ~
            co$30, c_o$2,                /* Linealmate Descriptive Not*/ ~
            bat_rec$122,                 /* Batch Record              */ ~
            scr_dte$8,                   /* Completion Date Unformated*/ ~
            scr_dte1$8,                  /* Date formated             */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            wrk_rec$(2%)256,             /* WORK RECORD               */ ~
            seq$3,                       /* Record Number Key         */ ~
            scr_part$25,                 /* MFG Part Number           */ ~
            save_part$25,                /* MFG Part Number           */ ~
            ssq$3, sr_seq$5,             /* Daily Sequence Number     */ ~
            sa_d2$30,                    /* MFG Part Description      */ ~
            sa_type$(100%)2,             /* RECORD TYPE 'SA' OR 'LA'  */ ~
            sa_piece$(100%)4,            /* Number of Pieces's to Cut */ ~
            sa_cut$(100%)10,              /* Cut Size for Piece's      */ ~
            sa_part$(100%)15,            /* Raw Material Part Number  */ ~
            sa_rack$(100%)18,            /* Bin Loc and No. of Pieces */ ~
            sa_d1$(100%)16,              /* Raw Material Description  */ ~
            file$30,                     /* Schedule Title Name       */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79                   /* ASKUSER Info Text         */ ~
  
       dim  sr_key$47,		   			 /* AWDPLNSR Key			  */ ~
            sr_date$8,                   /* Production Date (IM5733)  */ ~
            sr_batch$20,				 /* Batch Name		(IM5733)  */ ~
            sr_bnum$5,					 /* Screen Batch #  (IM5733)  */ ~
            sr_dept$3,					 /* Secondary Dept  (IM5733)  */ ~
            sr_model$3,					 /* Model           (IM5733)  */ ~
            sr_color$1,					 /* Color           (IM5733)  */ ~
            sr_type$1,					 /* Screen Type E   (IM5733)  */ ~
            sr_wfrc$10,					 /* Screen Wdth Frc (IM5733)  */ ~
            sr_hfrc$10,					 /* Screen Hght Frc (IM5733)  */ ~
            sr_part$25,					 /* Part Number		(IM5733)  */ ~
            sr_hf$1,					 /* Half or full scr(IM5733)  */ ~
            sr_bar$8,    				 /* Dt_ref or warranty(IM5733)*/ ~
            clr$2,                       /* Color                     */ ~
            batch$5                      /* Temporary batch #         */ 


        dim f2%(5%),                     /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAW001@",                                        ~
                                consec , recsize = 222   /* (IM5733)  */

REM            cw%, ch% = 0%
                                                 /* Create Saw Batches */
            call "OPENFILE" (#3, "IO   ", f2%(3%), rslt$(3%), axd$ )
            if f2%(3%) <> 0% then goto L01080
               gosub file_exists
               if comp% <> 16% then goto L01040
                  call "FILEBGON" addr(#3)
                  goto L01080

L01040:        close #3
               call "OPENFILE" (#3, "EXTND", f2%(3%), rslt$(3%), axd$ )
               goto L01140

L01080:     str(rslt$(3%),1%,6%)  = "OUTPTP"
            str(rslt$(3%),7%,8%)  = "00001000"
            str(rslt$(3%),15%,3%) = "100"
            str(rslt$(3%),18%,3%) = "100"
            call "OPENFILE" (#3, "OUTPT", f2%(3%), rslt$(3%), axd$ )

REM            bat_no% = 1% : 
L01140:     count% = 0% : save_part$ = " "
        REM SCHED% = 100%                             /* MOD - 09/04/92 */
            convert sched% to tsched$, pic(###)

            call "SHOSTAT" ("Starting Schedule ("&tsched$&") for Dept ("&~
                                                           scr_dept$&")")
                                                           
                   
            
REM            tw$ = "I"  th$ = "J"             /* LOAD CUT DESCRIPTIONS */
REM            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
REM            if err% <> 0% then goto exit_program
               xcount% = 0% : new_sched% = 0%
REM            wrk_key1$ = all (hex(00))             /* Screen Department */
REM            read #1,key > wrk_key1$, using L01320 , wrk_rec$,              ~
REM                                                     eod goto create_done
			init(" ") sr_key$
			scr_prod$ = "1"
            sr_key$ = all(hex(00))
            str(sr_key$,2%,9%)  = scr_dte$
REM            str(sr_key$,1%,7%)  = scr_dept$
            str(sr_key$,1%,1%) = scr_prod$	
            
            read #7, key 1% > sr_key$, using sr_fmt, wrk_rec$(),                ~
            					eod goto create_done 	
            					
            scr_dte1$ = scr_dte$
               call "DATEFMT" (scr_dte1$)						
            
            goto L01330
            					
            create_next
              read #7,key 1% > sr_key$, using sr_fmt, wrk_rec$(),                ~
            					eod goto create_done 		

            goto L01330


/*(IM5733)*/

sr_fmt:       FMT 2*CH(256)
			
L01330:     
			sr_key$ = str(wrk_rec$(),7%,47%)
			if str(sr_key$,1%,1%) <> scr_prod$ then goto create_done
			if str(sr_key$,2%,6%) > scr_dte$ then goto create_done
			
			sr_date$  = str(wrk_rec$(),1%,6%)	 	/* Production Date	 */
			sr_batch$ = str(wrk_rec$(),14%,20%)		/* Batch Name		 */
			sr_bnum$  = str(wrk_rec$(),34%,5%)		/* Screen Batch #	 */
			sr_dept$  = str(wrk_rec$(),39%,3%)		/* secondary Dept	 */
			sr_seq$   = str(wrk_rec$(),54%,5%)		/* Seq #			 */
			sr_model$ = str(wrk_rec$(),59%,3%)		/* Model			 */
			sr_color$ = str(wrk_rec$(),62%,1%)		/* Color			 */
			sr_type$  = str(wrk_rec$(),64%,1%)		/* Type-only extruded*/
			sr_wfrc$  = str(wrk_rec$(),98%,10%)		/* Width decimal     */
			sr_hfrc$  = str(wrk_rec$(),108%,10%)	/* Height decimal    */
			sr_part$  = str(wrk_rec$(),138%,25%)	/* Part Number       */
			sr_hf$    = str(wrk_rec$(),65%,1%)		/*Screen half or full*/
			sr_bar$	  = str(wrk_rec$(),42%,8%)		/*DT_REF		     */ 
			ssq$	  = str(sr_seq$,3%,5%)					
			
REM            if len(dtl_part$) < 19 then goto create_next
/*(IM5733)*/
			if sr_type$ <> "E" then goto create_next
			if sr_dept$ = "000" or sr_dept$ = "023" or sr_dept$ = "056"      ~
			    	then goto create_next	

			if sr_bnum$ <> batch$ then gosub build_schedule   /* CREATE BATCH */
			init(" ") batch$
			batch$ = sr_bnum$

               gosub build_saw_recs
               if sa_max% = 0% then goto create_next
                  seq% = 0%
                  sa_max%=2%
                  for sa% = 1% to sa_max%
                      gosub build_detail
                  next sa%
                  count% = count% + 1%
                  if sr_bnum$ = batch$  then goto create_next
                     gosub build_end
                     count% = 0% 
                     new_sched% = 0%
                     goto create_next
        create_done
            gosub build_end
            close #3
REM            convert (bat_no% - 1%) to bat$, pic(##)
        goto exit_program

        build_schedule                         /* SET-UP SCHEDULE REV. */
          if new_sched% <> 0% then gosub build_end
          init(" ") bat_rec$

/*(IM5733)*/ file$ = "Screen " &scr_dte1$& " B" &sr_bnum$& ~ 
					  " Dpt " &sr_dept$
          str(bat_rec$,1%,2%)   = "FR"                  /* FILE REV.   */
          str(bat_rec$,3%,6%)   = "910318"              /* REVISION NO */
          str(bat_rec$,9%,114%) = " "                   /* LINE FEED   */
          write #3, bat_rec$, eod goto L01740

          gosub build_schedule_no
          gosub build_schedule_title
          new_sched% = 1%                        /* New Schedule Build */
        return
L01740:   call "SHOSTAT" ("(Error) - Writing Revision ????") : stop
        return

        build_schedule_no
          init(" ") bat_rec$
          gosub assign_schedule
          convert sched% to sched$, pic(000)

          str(bat_rec$,1%,2%) = "SN"                /* SCHEDULE NUMBER */
          str(bat_rec$,3%,3%) = sched$              /* SCHED (100-999) */
          str(bat_rec$,6%,117%) = " "               /* LINE FEED       */
          write #3, bat_rec$, eod goto L01870
        return
L01870:   call "SHOSTAT" ("(Error) - Writing Schedule Number ??? ")
          stop
        return

        build_schedule_title                        /* SCHEDULE TITLE  */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TI"              /* SCHEDULE TITLE  */
          str(bat_rec$,3%,30%)  = file$             /* TITLE DESCRIPT  */
          str(bat_rec$,34%,89%) = " "               /* LINE FEED       */
          write #3, bat_rec$, eod goto L01980
        return
L01980:   call "SHOSTAT" ("(Error) - Writing Schedule Title ??? ")
          stop
        return

        build_detail
          xcount% = xcount% + 1%
          init(" ") bat_rec$
          seq% = seq% + 1%
          convert seq% to seq$, pic(###)
          str(bat_rec$,1%,2%)   = sa_type$(sa%)            /* 'SA' */
          str(bat_rec$,3%,8%)   = sr_bar$               /* Load Number */
          str(bat_rec$,11%,2%)   = "  "
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = sa_piece$(sa%)        /* Unit Qty    */
          str(bat_rec$,20%,10%)  = sa_cut$(sa%)          /* Piece Cut   */
          str(bat_rec$,29%,9%)  = "        "            /* Reserved    */
          str(bat_rec$,38%,15%) = sa_part$(sa%)         /* Part No.    */
          str(bat_rec$,58%,4%)  = "    "                /* Reserved    */
          str(bat_rec$,57%,18%) = sa_rack$(sa%)         /* Harp Rack/  */
                                                        /* Bin Location*/
          str(bat_rec$,75%,16%) = sa_d1$(sa%)           /* Part Desc   */
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,2%) = " "                   /* Line Feed   */
          write #3, bat_rec$, eod goto L02230
        return
L02230:   call "SHOSTAT" ("(Error) - Writing Schedule Detail ??? ")
          stop
        return

        build_end
          if count% = 0% then return
          init(" ") bat_rec$
          str(bat_rec$,1%,2%) = "**"                /* END OF BATCH    */
          str(bat_rec$,3%,3%) = "END"               /* LINE FEED       */
          str(bat_rec$,6%,117%) = " "               /* LINE FEED       */
          write #3, bat_rec$, eod goto L02360
REM          bat_no% = bat_no% + 1%
        return
L02360:   call "SHOSTAT" ("(Error)-Unable to Create Batch End?")
          stop
        return

        build_saw_recs
          if save_part$ <> sr_part$ then goto L02470     /*(IM5733)  */
             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,13%)  = ssq$& "-A" &ssq$& "/01"
             next i%
             return

L02470:   save_part$ = sr_part$
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$
REM          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
REM                                                                    err%)
          sa_d2$ = "                              "      /*(IM5733)  */

          sa% = 0%                     /* (MFG%) SWITCH IS SET TO = 2% */
          scr_part$ = sr_part$                 /* Special Screen Mod  */
/*(IM5733) sub to lookup color for raw matl and description*/
		  gosub lookup_color        



REM          call "APCCUTCC" (scr_part$, 0%, 0%, 0%, /* (CUT001) */         ~
REM                     2%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
REM                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
REM                                                        #4, #5, #2, err%)
          gosub build_descript
REM          eq% = cw% + ch%					/*IM5733 no need to check eq*/
          for i% = 1% to 2%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */


           sa% = sa% + 1%
               sa_type$(sa%)		  = "SA"

               sa_piece$(sa%)	  = "  01" /* No Pieces Needed (IM5733) */
/*(IM5733)  */
			   if sa% = 1% then                                               ~
			   	 sa_cut$(sa%)		   = sr_wfrc$         /* Length of Cut  */ ~
			   else sa_cut$(sa%)		   = " "& sr_hfrc$
		   
               str(sa_part$(sa%),1%,2%)  	= clr$        /* Color raw matl*/
               
               if sr_hf$ = "F" then                                        ~
               		str(sa_part$(sa%),3%,1%)  = sr_hf$     /* Color raw matl*/ ~
               	else str(sa_part$(sa%),3%,1%) = "X"
               	
               str(sa_part$(sa%),4%,3%)       = sr_model$
               if sa% = 1% then 						 /* Width or Height */ ~
               		str(sa_part$(sa%),7%,1%)  = 	"W" 					~
               	else str(sa_part$(sa%),7%,1%) = 	"H"
               
               str(sa_part$(sa%),11%,5%)      = "     "
               str(sa_rack$(sa%),1%,4%)       = "@F A"    /* Bin Location */
               str(sa_rack$(sa%),5%,9%)      = ssq$& "-A" &ssq$& "/"
               str(sa_rack$(sa%),14%,2%) 	 = "01" /* Pieces (IM5733)*/
               str(sa_rack$(sa%),16%,3%) 	 = " "
               str(sa_d1$(sa%),1%,2%)  	     = "0/"     /* No. of Labels    */
        REM    STR(SA_D1$(SA%),3%,12%)   = STR(COL$(I%),1%,12%)
               str(sa_d1$(sa%),3%,12%)   = str(co$,1%,12%)
REM             str(sa_d1$(sa%),3%,12%)   	 = "            "  /*(IM5733)  */
               str(sa_d1$(sa%),15%,2%)  	 	 = "/-"
REM               if dt_samp$ = "1" or dt_samp$ = "2" then                  ~
REM                                    str(sa_d1$(sa%),15%,2%) = "/S"
REM               if str(co$,1%,1%) = "9" then                              ~
REM                                    str(sa_d1$(sa%),15%,2%) = "/B" 
      next i%
             sa_max% = sa%
/*(\IM5733) */	
        return

        build_descript				
/*(IM5733) replace dtl_part with sr_part */
            init(" ") co$,  readkey$, c_o$
            c_o$ = "XX"                          /* NOT SPECIAL        */
            cnt% = 3%                            /* 1st Set Model Code */
            str(co$,1%,3%)      = sr_model$
            str(co$,cnt%+1%,3%) = "/" & clr$
			              
/*(IM5733) removed lock logic*/
	        cnt% = cnt% + 4%
			str(co$,cnt%+1%,2%) =  "/" & sr_type$  /* Set Screen type (IM5733)*/
			str(co$,cnt%+3%,2%) = "/" & sr_hf$    /* Set Half or Full (IM5733)*/

/*(IM5733) removed sash only and with fin logic */ 
            cnt% = cnt% + 5%
		    str(readkey$,1%,9%)   = "HINGE    "    /* Check Cot/Oriel  */
            str(readkey$,10%,15%) = str(sr_part$,9%,2%)
            read #2,key = readkey$, using L03440, desc$, eod goto L03600
L03440:        FMT POS(25), CH(32)
            p% = pos(desc$ = "-")
            if str(desc$,1%,2%) <> "CO" and str(desc$,1%,2%) <> "OR"     ~
                                        then goto L03530
               str(co$,cnt%+1%,3%) = "/CO"         /* Set as Default   */
               if str(desc$,1%,2%) = "OR" then str(co$,cnt%+1%,3%)="/OR"
               c_o$ = "CO"
               if str(desc$,1%,2%) = "OR" then c_o$ = "OR"
               cnt% = cnt% + 3%
L03530:     if p% = 0% then goto L03600
            if str(desc$, p%+2%, 4%) <> "TWIN" and                       ~
               str(desc$, p%+2%, 4%) <> "TRPL" then goto L03600
                  str(co$, cnt%+1%,3%) = "/TW"     /* Set as Default   */
                  if str(desc$,p%+2%,4%) = "TRPL" then                   ~
                     str(co$, cnt%+1%,3%) = "/TR"
               cnt% = cnt% + 3%
L03600:     if str(sr_part$,9%,2%) <> "09" then goto L03630
               str(co$,cnt%+1%,3%) = "/33"                 /* 1/3,1/3  */
               cnt% = cnt% + 3%
L03630:     if p% < 5% then goto L03670
               if str(desc$,p%-4%,3%) = "1/2" then c_o$ = "44"
               if str(desc$,p%-4%,3%) = "1/3" then c_o$ = "33"

L03670: return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "        The File (@SAW001@) Already Exists.       "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        assign_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #2,hold,key = readkey$, using L03840 , sched$,            ~
                                                           eod goto L03920
L03840:       FMT POS(25), CH(3)
            convert sched$ to sched%, data goto L03920

            convert (sched% + 1%) to sched$, pic(000)
            if (sched% + 1%) > 999% then sched$ = "100"
            put #2, using L03840 , sched$
            rewrite #2
        return
L03920:    call "SHOSTAT" ("Error- Assigning Schedule Number?")
           stop
        return


        exit_program
          if xcount% > 0% then goto L04020
             call "SHOSTAT" ("N O   L I N E A L M A T E   D A T A")
             call "PAUSE" addr(300%)

L04020: end             		
/*(IM5733)*/             
        lookup_color
REM        	init("  ") clr$ 
        	str(readkey$,1%,9%)   = "COLOR    "    /* Check Color  */
            str(readkey$,10%,15%) = sr_color$
            read #2,key = readkey$, using L04010, desc$, eod goto L04015
L04010:        FMT POS(25), CH(32)
			clr$ = str(desc$,1%,2%)
L04015:	return

            
					
			


