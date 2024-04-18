*       ****************************************************************~
*                            ( L i n e a l m a t e )                   *~
*                  (PAR000) 09/02/2011              Dept (048)         *~
*                                                                      *~
*        APCPLR42 - Create File with Data for Saw Optimization         *~
*                   (Same as 'APCPLC42' Except for Revision No.)       *~
*                   ( Line No. = 1670   Revision No. = '910318')       *~
*                                                                      *~
*            Note - Primary Subroutine 'BUILD_SAW_RECS'                *~
*                                                                      *~
*            Note - Special Mod to handle the change for DT_REF$8 and  *~
*                   DT_SEQ$5. Future need to change File Layout to     *~
*                   handle size changes.                               *~
*                                                                      *~
*            Note - Modification made to support Same Material being   *~
*                   cut on different saws. (EWD001)                    *~
*                                                                      *~
*            Note - Modification made to support Same Material being   *~
*                   cut on different, Turn off for models beginning    *~
*                   with a '1'. (EWD002) and Change to special logic   *~
*                   for Products beginning with a '1' and 1/3, 1/3 or  *~
*                   1/4, 1/2, 1/4                                      *~
*                                                                      *~
*            Note - When changes are made to the following Tables, the *~
*                   subroutine will need to be modified.               *~
*                   BUILD_DESCRIPT uses (SCREEN,LOCK,HINGE) Tables     *~
*                                                                      *~
*            Note - New Subroutine to Assign the Next Schedule Number  *~
*                   for Linealmate. 'ASSIGN_SCHEDULE'                  *~
*                                                                      *~
*----------------------------------------------------------------------*~
* 02/17/2003  !  (EWD001) - Mod to set up new 130 3SL Slider same! CMG *~
*             !             as 131 3SL.                                *~
* 10/27/2003  !  (EWD002) - Mod to set up new 136 and 137 3SL    ! CMG *~
*             !             Slider same as 131 3SL.                    *~
* 08/31/2004  !  (EWD003) - Mod to put 3 and 4 to end of raw     ! CMG *~
*             !   material number on model 136 and 137.          !     *~
* 12/16/2005  !  (EWD004) - Mod for New Brick Mold 2SL, 3SL for  ! RHH *~
*             !             Models 128, 138 and Fix Description  !     *~
* 01/15/2006  !  (PAR000) - CR347 Mods for New Part Number       ! RHH *~ 
* 09/02/2008  !  (AWD005) - mods for new sliders S23 & S33       ! CMG *~
* 03/08/2013  !  (AWD006) - mod to increase array size           ! CMG *~
* 05/19/2014  ! (CUT001) mod to add dim fields to CUTCC          ! CMG *~
* 12/03/2015  ! (SR70732) - mod to change file format            ! MES *~
* 08/11/2017  ! (CR1002)  - new top bottom parameter             ! RDB *~
* 10/8/2018   ! (CR1721)  - remove sash only                     ! MES *~  
* 10/8/2018   ! (CR1675)  - add jamb to gt F for foam$           ! MES *~
* 01/17/2023  ! CR3224 - Missing foam on some orders            ! RDB *~
*       ****************************************************************

        sub "APCPLR42" (size%,           /* Batch Size (No. Windows)  */ ~
                        sched%,          /* Starting Schedule Number  */ ~
                        scr_dte$,        /* Production Date Formatted */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        scr_load$,       /* Production Load Number    */ ~
                        scr_type$,       /* (A), (B), (C), (D)       */ ~
                        lk_fn$(),        /* 1,2 Lock with Fin (PAR000)*/ ~
                        #1,              /* (APCCUTWK) Saw Work File  */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6 )             /* (AMTBOMIF) Validity File  */

        dim readkey$24,                  /* GENCODES Primary Key      */ ~
                                         /* (PAR000)                  */ ~
            lk_fn$(5%)30,                /* 1%=1Lock,2%=2Lock,3%=WFin */ ~
            desc$32,                     /* GENCODES Description      */ ~
            model$3,                     /* Product Code              */ ~ 
            mods$(50%)3,                 /* Valid Models and Channels */ ~
            sched$3,                     /* Schedule Numbers          */ ~
            tsched$3,                    /* Starting Schedule Number  */ ~
            co$30,                       /* Linealmate Descriptive Not*/ ~
            bat_no$26,                   /* Batch Identifiers ( A-Z ) */ ~
            bat_rec$222,                 /* Batch Record              */ ~
            bat$2,                       /* Number of Batches         */ ~
            apc_scr$120,                 /* Screen Text               */ ~
            apc_prt$60,                  /* Print Text                */ ~
            apc_sze$20,                  /* Size                      */ ~
            scr_dte$8,                   /* Completion Date           */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            scr_load$5,                  /* Production Load           */ ~
            scr_type$1,                  /* Use to create file        */ ~  
            wrk_key1$51,                 /* WORK KEY                  */ ~
            wrk_rec$200,                 /* WORK RECORD               */ ~
            seq$3,                       /* Record Number Key         */ ~
            dtl_load$5,                  /* Load Number               */ ~
            dtl_part$25,                 /* MFG Part Number           */ ~
            hinge$2,                     /* MFG Hinge Code    (AWD005)*/ ~
            save_part$25,                /* MFG Part Number           */ ~
            ref_no$5, dt_ref$8,          /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP      */ ~
/* (AWD006) change all array size from 100 to 200  */                    ~            
            col$(200%)25,                /* Cut Description           */ ~
            eq$(200%)8,                  /* Equation Codes            */ ~
            ct$(200%)9,                  /* Cut Widths and Heights    */ ~
            ct(200%),                    /* Cut Wid/Height Decimal    */ ~
            sh$(200%)1,                  /* Sash Type                 */ ~
            cr$(200%)10,                 /* Raw Material Part Number  */ ~
            cp$(200%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(200%)1,                  /* Cut Piece Yes or No       */ ~
            tw$1,                        /* WIDTH CUT PARTS           */ ~
            th$1,                        /* HEIGHT CUT PARTS          */ ~
            sa_d2$30,                    /* MFG Part Description      */ ~
            sa_type$(200%)2,             /* RECORD TYPE 'SA' OR 'LA'  */ ~
            sa_piece$(200%)4,            /* Number of Pieces's to Cut */ ~
            sa_cut$(200%)9,              /* Cut Size for Piece's      */ ~
            sa_part$(200%)15,            /* Raw Material Part Number  */ ~
            sa_rack$(200%)18,            /* Bin Loc and No. of Pieces */ ~
            sa_d1$(200%)16,              /* Raw Material Description  */ ~
            sa_m$(100%)8,                /* WINDOW TYPE,PROFILE TYPE  */ ~
            sa_s$(100%)1,                /* SAW SET-UP NUMBER         */ ~
            sa_cut_type$(100%)1,         /* 2 = Frame, 1 = Sash       */ ~
            type$1,                      /* Test for Fame or Sash     */ ~
            machine$3, set_up$1, lk$1,   /* Store Window Type code    */ ~
            saw_no$2, saw_no$(100%)2,    /* Machine Saw Number        */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$10,                      /* Schedule File Identifier  */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            sub_part$20,                 /* Subpart (SR70732)         */ ~
            dtl_new_part$45,             /* (SR70732) New Part Number */ ~
            save_newpart_s$45,           /* (SR70732) MFG Part Number */ ~
            foam$1,						 /* (SR70732) Foam            */ ~
            sa_jpart$(100%)20,           /* (SR70732) Urban cluster   */ ~
            sa_jmodel$(100%)20,          /* (SR70732) joseph model    */ ~            
            jpart$20,                    /* (SR70732) Urban cluster   */ ~
            cr_addl$(100%)5,             /* (SR70732) addl raw material */~
            die$(100%)15,                /* (SR70732) Die Number      */~
            saw_type$(2%)3,              /* Saw Type Frame or Sash    */~
            saw_err$(2%)50,              /* Saw Error Message         */ ~
            tb_w$(500%)1,                /* T/B cut (CR1002)          */ ~
            s$1                           /* Sash (CR1721)             */

        dim f2%(20%),                     /* = 0 if the file is open    */~
            axd$4,                        /*   doesn't exist, or 0 if   */~
            name$(20%)8,                  /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                  /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
                                            /* A, B, C, D Respectfully */
            select #7,  "@SAW15@",                                       ~
                                consec , recsize = 222 /*(SR70732)*/

            select #8,  "@SAW16@",                                       ~
                                consec , recsize = 222  /*(SR70732)*/

            select #9,  "@SAW17@",                                       ~
                                consec , recsize = 222  /*(SR70732)*/

            select #10,  "@SAW18@",                                      ~
                                consec , recsize = 222  /*(SR70732)*/

            select #11,  "@SAW32@",               /* (EWD002) */         ~
                                consec , recsize = 222  /*(SR70732)*/

            select #12,  "@SAW33@",               /* (AWD005) */         ~
                                consec , recsize = 222  /*(SR70732)*/

            init(" ") name$()
            name$( 7%) = "@SAW15@"          /* A = Channel% = 7%      */
            name$( 8%) = "@SAW16@"          /* B = Channel% = 8%      */
            name$( 9%) = "@SAW17@"          /* C = Channel% = 9%      */
            name$(10%) = "@SAW18@"          /* D = Channel% = 10%     */    
            name$(11%) = "@SAW32@"          /* E = Channel% = 11%     */    
            name$(12%) = "@SAW33@"          /* F = Changel% = 12% (AWD005) */

            gosub load_wegoma                /* MODS$() - VALID MODELS */
            
                                            /* Saw Setup Information  */
                                            /* 1% = Sash, 2% = Frame  */   
            saw_type$(1%) = "@S "           /* Sash Saw           (03)*/
            saw_type$(2%) = "@F "           /* Frame Saw          (03)*/

            saw_err$(1%) = "Error Writing Saw Sash Record??"
            saw_err$(2%) = "Error Writing Saw Frame Record??" 
            
            gosub create_files               /* Create all (6) files   */


            cw%, ch%, csw%, csh% = 0%

            bat_no$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            bat_no% = 1% : count% = 0% : save_part$ = " "
            tsched$ = "000"

            init(" ") tsw$, tsh$     /* (AWD002) */
            tw$ = "1" : th$ = "2"             /* LOAD CUT DESCRIPTIONS */
REM            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
REM            if err% <> 0% then goto exit_program
            xcount% = 0%
            wrk_key1$ = all(hex(00))
            read #1,key 1% > wrk_key1$, using L01410 , wrk_key1$, wrk_rec$,~
                                                     eod goto create_done
            goto L01420
        create_next
            read #1, using L01410 , wrk_key1$, wrk_rec$,                   ~
                                                     eod goto create_done
L01410:          FMT POS(6), CH(51), CH(200)
L01420:     dt_ref$   = str(wrk_rec$,1%,8%)      /*(SR70732)     */
			dt_seq$   = str(wrk_rec$,9%,5%)		 /*(SR70732)     */
			dtl_load$ = str(wrk_rec$,29,5%)      /*(SR70732)     */

			dtl_part$ = str(wrk_rec$,38%,25%)
			scr_prod$ = str(dtl_part$,1%,1%)     /*(SR70732)     */
            call "AWDCUTLD" (scr_prod$, cw%, ch%, csw%, csh%, tw$, ~ 
                            th$, tsw$, tsh$,#2, err% )/*(SR70732)     */
            if err% <> 0% then goto create_next		  /*(SR70732)     */	
            
            model$    = str(dtl_part$,1%,3%)
            hinge$    = str(dtl_part$,9%,2)
           	sub_part$ = str(wrk_rec$,81%,20%) /* Subpart (SR70732)     */
           	dtl_new_part$ = str(dtl_part$,1%,25%) & str(sub_part$,1%,20%)
           					/* (SR70732)  */
            
            s$        =str(dtl_part$, 11%, 1%)   /* (CR1721)       */
            
            for kk% = 1% to mod_max%
                if model$ = mods$(kk%) then goto L01470
            next kk%
            goto create_next

L01470:     if len(dtl_part$) < 19 then goto create_next
            if (s$ ="4" or s$ = "5" or s$ = "6") then goto create_next /*(CR1721)*/
 
            
            
            
            dt_samp$  = str(wrk_rec$,77%,1%)
            ref_no$   = str(dt_ref$,4%,5%)
            ssq$      = str(dt_seq$,3%,3%)
            foam$     = str(sub_part$,5%,1%)
            hng$      = str(dtl_part$,9%,2%)
            if count% = 0% then gosub build_schedule   /* CREATE BATCH */
               gosub build_saw_recs
              	   j% = 1%                    /* Frame Weld Record(SR70732)  */
                   type$ = "2"
                   type% = 2%
                   
               if sa_max% = 0% then goto create_next
                  seq% = 0%
                  for sa% = 1% to sa_max%
                      gosub build_detail_saw
                  next sa%
                  
                   j% = 2%                    /* Sash Weld Record(SR70732)   */
                   type$ = "1"
                   type% = 1%
                   
                if sa_max% = 0% then goto create_next
                  seq% = 0%
                  for sa% = 1% to sa_max%
                      gosub build_detail_saw
                  next sa%
                     
                  count% = count% + 1%
                  if count% < size%  then goto create_next
                     gosub build_end
                     count% = 0%
                     goto create_next
        create_done
            gosub build_end
            close #m_chn%
            convert (bat_no% - 1%) to bat$, pic(##)
        goto exit_program

        build_schedule                         /* SET-UP SCHEDULE REV. */
          init(" ") bat_rec$
          inc$ = " (" & str(bat_no$,bat_no%,1%) & ")"
          file$ = "("&scr_dept$&")   Saw for " &scr_dte$& inc$
          if str(scr_load$,1%,1%) <> "N" then str(file$,6%,2%) = "/U"
          str(bat_rec$,1%,2%)   = "FR"                  /* FILE REV.   */
          str(bat_rec$,3%,6%)   = "910318"              /* REVISION NO */
          str(bat_rec$,9%,114%) = " "                   /* LINE FEED   */
          write #m_chn%, bat_rec$, eod goto L01810

          gosub build_schedule_no
          gosub build_schedule_title
        return
L01810:   call "SHOSTAT" ("(Error) - Writing Revision ????") : stop
        return

        build_schedule_no
          init(" ") bat_rec$
          gosub assign_schedule
          convert sched% to sched$, pic(000)

          str(bat_rec$,1%,2%) = "SN"                /* SCHEDULE NUMBER */
          str(bat_rec$,3%,3%) = sched$              /* SCHED (100-999) */
          str(bat_rec$,6%,117%) = " "               /* LINE FEED       */
          write #m_chn%, bat_rec$, eod goto L01940
          if tsched$ = "000" then                                       ~
            call "SHOSTAT" ("Starting Schedule ("&sched$&") for Dept ("&~
                                                           scr_dept$&")")
          tsched$ = sched$ 
        return
L01940:   call "SHOSTAT" ("(Error) - Writing Schedule Number ??? ")
          stop
        return

        build_schedule_title                        /* SCHEDULE TITLE  */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TI"              /* SCHEDULE TITLE  */
          str(bat_rec$,3%,30%)  = file$             /* TITLE DESCRIPT  */
          str(bat_rec$,33%,90%) = " "               /* LINE FEED       */
          write #m_chn%, bat_rec$, eod goto L02050
        return
L02050:   call "SHOSTAT" ("(Error) - Writing Schedule Title ??? ")
          stop
        return

        build_detail_saw
         if sa_cut_type$(sa%) <> type$ then return /* type$ = 2 Frame */
                                                    /* type$ = 1 Sash  */
                                                    /*(SR70732)*/
          xcount% = xcount% + 1%
          init(" ") bat_rec$
          seq% = seq% + 1%
          convert seq% to seq$, pic(###)
          str(bat_rec$,1%,2%)   = sa_type$(sa%)         /* 'SA' OR 'LA'*/
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = sa_piece$(sa%)        /* Unit Qty    */
          str(bat_rec$,20%,9%)  = sa_cut$(sa%)          /* Piece Cut   */
          str(bat_rec$,29%,9%)  = "        "            /* Reserved    */
          str(bat_rec$,38%,15%) = sa_part$(sa%)         /* Part No.    */
          str(bat_rec$,58%,4%)  = "    "                /* Reserved    */
          str(bat_rec$,57%,18%) = sa_rack$(sa%)         /* Harp Rack/  */
                                                        /* Bin Location*/
          str(bat_rec$,57%,3%)  = saw_type$(type%)		/* Saw Type    */
          str(bat_rec$,75%,16%) = sa_d1$(sa%)           /* Part Desc   */
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,1%) = " "                   /* Line Feed(SR70732)*/
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels(SR70732)*/
          str(bat_rec$,124%,15%)= "               "   /* License Plat(SR70732)*/
          str(bat_rec$,139%,8%) = sa_m$(sa%)          /* Machine Code(SR70732)*/
          str(bat_rec$,147%,2%) = saw_no$(sa%)          /* Saw Number(SR70732)*/
          str(bat_rec$,149%,1%) = sa_s$(sa%)            /* Saw Set-up(SR70732)*/
          str(bat_rec$,183%,20%) = sa_jpart$(sa%)      /* Urban Cluster (AWD002)*/
          str(bat_rec$,203%,20%) = sa_jmodel$(sa%)       /* Urban Part/Fixture (AWD002)*/
                                                        /*  Number     */
          write #m_chn%, bat_rec$, eod goto L02300
        return
REM L02300   call "SHOSTAT" ("(Error) - Writing Schedule Detail ??? ")
L02300:  errormsg$ = saw_err$(type%)
          stop
        return

        build_end
          if count% = 0% then return
          init(" ") bat_rec$
          str(bat_rec$,1%,2%) = "**"                /* END OF BATCH    */
          str(bat_rec$,3%,3%) = "END"               /* LINE FEED       */
          str(bat_rec$,6%,117%) = " "               /* LINE FEED       */
          write #m_chn%, bat_rec$, eod goto L02430
          bat_no% = bat_no% + 1%
        return
L02430:   call "SHOSTAT" (errormsg$) : stop
        return

        build_saw_recs
REM          if save_part$ <> dtl_part$ then goto L02530
			if save_newpart_s$ <> dtl_new_part$ then goto L02530
             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
REM             return           /* CR3224 */

L02530:   save_newpart_s$ = dtl_new_part$						/*NEWFAMILY*/
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$(), sa_s$(), saw_no$(), ~
                    sa_cut_type$()
                    
          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
          init(" ") tb_w$()   /* CR1002 */
REM          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
REM                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
REM                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
REM                                                        #4, #5, #2, err%)
 			call "AWDCUTCC" (dtl_new_part$, 0%, 0%, 0%, /* (CUT001) */      ~
                     0%, cw%, ch%, csw%, csh%, eq$(), ct$(), cr$(),         ~
                     cr_addl$(), cp$(), cc$(), col$(), ct(), sh$(),         ~
                     tw$, th$, s_f$(), die$(), adj(), tb_w$(), #4, #5, #2, err%)          
                                                       


          gosub build_descript
          eq% = cw% + ch%
          for i% = 1% to eq%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */
            if cc$(i%) = "N" or cc$(i%) = " " then goto L02940

            cut% = 0%                    /* Skip - No Equation Records */
            convert str(ct$(i%),1%,3%) to cut%, data goto L02720
L02720:
            if cut% = 0% then goto L02940
/*(SR70732)*/               
            eqNum$ = str(eq$(i%),7%,2%)
            	gosub check_cut						/* Check NEWFAMILY */
            	if check% = 0% then goto L02940
/*\SR70732)*/            	
            	sa% = sa% + 1%
               sa_type$(sa%)  = "SA"
               if cc$(i%) = "N" then sa_type$(sa%) = "LA"

               sa_piece$(sa%) = "  " & cp$(i%)     /* No Pieces Needed */
               sa_cut$(sa%)   = ct$(i%)            /* Length of Cut    */
               str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
                                                   /* (EWD002)         */
REM            if str(dtl_part$,1%,1%) <> "1" then goto L02730
REM            if str(dtl_part$,1%,1%) <> "1" and str(dtl_part$,1%,1%) <> "B" ~
REM                                                            then goto L02730

                   str(sa_part$(sa%),11%,3%) = "   " 
/*(SR7032)*/
			 

REM L02735        str(sa_part$(sa%),11%,3%) = scr_dept$  /*500 series only*/

                                                   /* (EWD002)         */
/*L02740:*/        str(sa_part$(sa%),14%,2%) = "  "    /* (EWD001)         */

                                                   /* Special Part No. */ 
               str(sa_rack$(sa%),1%,4%)  = "@F A"      /* Bin Location */
               str(sa_rack$(sa%),5%,9%)  = ssq$& "-A" &ssq$& "/"
               str(sa_rack$(sa%),14%,2%) = cp$(i%)           /* Pieces */
               str(sa_rack$(sa%),16%,3%) = "   "
               str(sa_d1$(sa%),1%,2%)    = "0/"    /* No. of Labels    */
        REM    STR(SA_D1$(SA%),3%,12%)   = STR(COL$(I%),1%,12%)
               str(sa_d1$(sa%),3%,12%)   = str(co$,1%,12%)
               str(sa_d1$(sa%),15%,2%)   = "/-"
               if dt_samp$ = "1" or dt_samp$ = "2" then                  ~
                                    str(sa_d1$(sa%),15%,2%) = "/S"
               if str(co$,1%,1%) = "9" then                              ~
                                    str(sa_d1$(sa%),15%,2%) = "/B"
                                    
/*(SR70732)*/
				if cut_type% <> 2% then goto NoFab  /* Only Frames  */
					triplefab% = 0%
					if model$ = "130" or model$ ="131" or model$ ="136"    ~
						or model$ = "137" or model$ = "S33" ~
						then  triplefab% = 1%
REM					if i% > cw% then goto NoFab  /*slider jambs CR1675*/	
						p% = 0%
                 	 p% = pos(sa_part$(sa%) = " ")
                 if foam$ = "3" then  /* (AWD002) */    ~
                                              str(sa_part$(sa%),p%,1%) = "F"
                 if foam$ = "4" then  /* (AWD002) */    ~
                                              str(sa_part$(sa%),p%,1%) = "F"	 
                 if triplefab% = 1% then gosub TripleFab
                 
NoFab:
			   sa_m$(sa%)        = machine$ & "     "
               sa_s$(sa%)        = set_up$         /* From Table      */
               saw_no$(sa%)      = saw_no$         /* From Table      */
               sa_cut_type$(sa%) = cut_type$       /* Frame or Sash   */
               sa_jpart$(sa%)    = jpart$          /*(AWD002)         */
               sa_jmodel$(sa%)   = die$(i%)        /*(AWD002)         *
/*(SR70732\*/
                                    
L02940:      next i%
             sa_max% = sa%
        return
/*(SR70732)*/
REM Triple fabrication changes
TripleFab
		 if i% > cw% then return
		 if eqNum$ <> "01" then return
		 if hng$ = "09" then str(jpart$,9%,1%) = "3" 
		 return
        
        build_descript
            init(" ") co$, x$, s$, readkey$
            cnt% = 3%                            /* 1st Set Model Code */
            str(co$,1%,3%) = str(dtl_part$,1%,3%)
REM            s$ = str(dtl_part$,11%,1%)           /* Set Screen Code    */
            x$ = str(dtl_part$,12%,1%)           /* Set Lock Code      */
                                                 /* (EWD004)           */
                                                 /* (PAR000)           */
            p% = pos(lk_fn$(1%) = x$)            /* 1 Lock Codes       */
            if p% = 0% then goto L03080
               str(co$,cnt%+1%,3%) = "/1K"
               lk$ = "1"
               goto L03110
                                                 /* (EWD004)           */
                                                 /* (PAR000)           */
L03080:     p% = pos(lk_fn$(2%) = x$)            /* 2 Lock Codes       */
            if p% = 0% then goto L03120
               str(co$,cnt%+1%,3%) = "/2K"
               lk$ = "2"
L03110:     cnt% = cnt% + 3%
L03120:     p% = pos("456" = s$)
            if p% = 0% then goto L03180
               str(co$,cnt%+1%,3%) = "/TS"         /* Set as Default   */
               if s$ = "5" then str(co$,cnt%+1%,3%) = "/BS"
               if s$ = "6" then str(co$,cnt%+1%,3%) = "/FG"
               cnt% = cnt% + 3%
                                                   /* (EWD004)         */
                                                   /* (PAR000)         */
L03180:     p% = pos(lk_fn$(3%) = x$)              /* With Fin Codes   */
            if p% = 0% then goto L03220
               str(co$,cnt%+1%,3%) = "/WF"         /* Set With Fin     */
               cnt% = cnt% + 3%
L03220:     str(readkey$,1%,9%)   = "HINGE    "    /* Check Cot/Oriel  */
            str(readkey$,10%,15%) = str(dtl_part$,9%,2%)
            read #2,key = readkey$, using L03250, desc$, eod goto L03390
L03250:        FMT POS(25), CH(32)
            p% = pos(desc$ = "-")
            if str(desc$,1%,2%) <> "CO" and str(desc$,1%,2%) <> "OR"     ~
                                        then goto L03320
               str(co$,cnt%+1%,3%) = "/CO"         /* Set as Default   */
               if str(desc$,1%,2%) = "OR" then str(co$,cnt%+1%,3%)="/OR"
               cnt% = cnt% + 3%
L03320:     if p% = 0% then goto L03390
            if str(desc$, p%+2%, 4%) <> "TWIN" and                       ~
               str(desc$, p%+2%, 4%) <> "TRPL" then goto L03390
                  str(co$, cnt%+1%,3%) = "/TW"     /* Set as Default   */
                  if str(desc$,p%+2%,4%) = "TRPL" then                   ~
                     str(co$, cnt%+1%,3%) = "/TR"
               cnt% = cnt% + 3%
L03390:     if str(dtl_part$,9%,2%) <> "09" then goto L03395
               str(co$,cnt%+1%,3%) = "/33"                 /* 1/3,1/3  */
               cnt% = cnt% + 3%
                                                   /* (EWD002)         */
L03395:     if str(dtl_part$,9%,2%) <> "08" then goto L03430        
               str(co$,cnt%+1%,3%) = "/44"                 /* 1/4,1/2  */
               cnt% = cnt% + 3%                    /* (EWD002)         */
L03430: return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "        The File (@SAWS@) Already Exists.       "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

/*(SR70732)*/

check_cut                                    /* 1st Frame      */
                                                     /* 2nd Sash       */
            cut_type% = 0%
            check% = 0%
            init(" ") readkey$, machine$, set_up$, saw_no$, desc$,     ~
                 cut_type$, jpart$						/*(AWD002)     */
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "F"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */


            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
            
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04240
L04180:       FMT POS(25), CH(30)

              machine$  = str(desc$,1%,3%)           /* Machine Code   */
                                                     /* Cottage        */
            if i% > cw% and c_o$ = "CO" then str(machine$,3%,1%)  = "3"
                                                     /* oriel          */
            if i% > cw% and c_o$ = "OR" then str(machine$,3%,1%)  = "4"

              set_up$   = str(desc$,5%,1%)           /* Set-Up Code    */
              saw_no$   = str(desc$,7%,2%)           /* Saw Number     */
              jpart$ = str(desc$,10%,20%)           /* Urban prt (AWD002)*/        
              check%    = 1%                         /* Valid Equation */
              cut_type% = 2%                         /* Frame Part     */
              cut_type$ = "2"                        /* Frame Part     */
            return
                                                     /* Sash Logic     */
L04240:     init(" ") readkey$, machine$, set_up$, saw_no$, desc$, jpart$
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            
/* (AWD001) */
            if s$ = "4" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "6"
            if s$ = "4" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "7"
            if s$ = "5" and str(readkey$,14%,1%) = "W" then ~
                         str(readkey$,14%,1%) = "8"
            if s$ = "5" and str(readkey$,14%,1%) = "H" then ~
                         str(readkey$,14%,1%) = "9"
                         
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L04180 , desc$, eod goto L04260

               machine$ = str(desc$,1%,3%)           /* Machine Code   */
               set_up$   = str(desc$,5%,1%)          /* Set-Up Code    */
               saw_no$   = str(desc$,7%,2%)          /* Saw Number     */
               jpart$ = str(desc$,10%,20%)           /* Urban prt (AWD002)*/               
               check%    = 1%                        /* Valid Equation */
               cut_type% = 1%                        /* Sash Part      */
               cut_type$ = "1"                       /* Sash Part      */ 
L04260: return
/*(SR70732\*/

        assign_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #2,hold,key = readkey$, using L03600 , sched$,            ~
                                                           eod goto L03680
L03600:       FMT POS(25), CH(3)
            convert sched$ to sched%, data goto L03680

            convert (sched% + 1%) to sched$, pic(000)
            if (sched% + 1%) > 999% then sched$ = "100"
            put #2, using L03600 , sched$
            rewrite #2
        return
L03680:    call "SHOSTAT" ("Error- Assigning Schedule Number?")
           stop
        return

        create_files                     /* Create (1) of the 4 Files */
                call "SHOSTAT" ("Creating Saw File - "& name$(m_chn%) )
                call "OPENFILE" (#m_chn%,"IO   ", f2%(m_chn%), rslt$(m_chn%), axd$ )
                if f2%(m_chn%) <> 0% then goto L30100
                   str(msg$(1%),19%,8%) = name$(m_chn%)
                   gosub file_exists
                   if comp% <> 16% then goto L30000
                   call "FILEBGON" addr(#m_chn%)
                   goto L30100

L30000:        close #m_chn%
               call "OPENFILE" (#m_chn%,"EXTND",f2%(m_chn%),rslt$(m_chn%), axd$ )
               goto L30200

L30100:     str(rslt$(m_chn%),1%,6%)  = "OUTPTP"
            str(rslt$(m_chn%),7%,8%)  = "00001000"
            str(rslt$(m_chn%),15%,3%) = "100"
            str(rslt$(m_chn%),18%,3%) = "100"
            call "OPENFILE" (#m_chn%, "OUTPT", f2%(m_chn%), rslt$(m_chn%), axd$ )
L30200: return

        load_wegoma
          mod_max% = 0% : m_chn% = 7%
          init(" ") readkey$, mods$(), desc$
          str(readkey$,1%,9%) = "EWDWEGOMA"
        load_wegoma_nxt
          read #2,key > readkey$, using L40000, readkey$, desc$,          ~
                                             eod goto load_wegoma_done
L40000:      FMT CH(24), CH(30)
          if str(readkey$,1%,9%) <> "EWDWEGOMA" then goto load_wegoma_done
          if scr_type$ <> str(desc$,1%,1%) then goto load_wegoma_nxt
 
             mod_max% = mod_max% + 1%
             mods$(mod_max%) = str(readkey$,10%,3%) /* Set Valid Models */
          goto load_wegoma_nxt
        load_wegoma_done
             if scr_type$ = "A" then m_chn% = 7%   
             if scr_type$ = "B" then m_chn% = 8%
             if scr_type$ = "C" then m_chn% = 9%
             if scr_type$ = "D" then m_chn% = 10%
             if scr_type$ = "E" then m_chn% = 11%     /* (EWD002) */
             if scr_type$ = "F" then m_chn% = 12%     /* (AWD005) */
        return

        exit_program
          if xcount% > 0% then goto L60000
             call "SHOSTAT" ("N O   L I N E A L M A T E   D A T A")
             call "PAUSE" addr(100%)

L60000: end
