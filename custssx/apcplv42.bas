REM     ****************************************************************~
*                      New -( S a m p s o n   W e l d e r )            *~
*                  (PAR000) 01/15/2006 CR347   Depts 25, 26, 36        *~
*                                                                      *~
*        APCPLV42 - Creates Sampson Sash Weld file, Builds             *~
*                   one bridge file for Specific Products.             *~
*                   for Department (036) 712 Sashs Only Double Stack   *~
*                   same as frames for 211 Line.                       *~ 
*                                                                      *~
*            Note - 'APCWELD  ' - Table Contains only Valid Weld       *~
*                                 Models.                              *~
*                    ** Deduct 1/4 Inch from Width and Height, Also    *~
*                       Mod to Sort. Same Sequence as the Production   *~
*                       Consolidation Report. ( By Load )              *~
*                                                                      *~
*            Note - Special Mod for Size change of DT_REF$8 and        *~
*                   DT_SEQ$5. Warranty Number and Daily sequence       *~
*                   Number.                                            *~
*                                                                      *~
*          EWD001 - Fix Double Stack problem 01/28/00                  *~
*          EWD002 - Mods for New Dept '025' and '026' 04/01/2003       *~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC             ! CMG *~
*       ****************************************************************

        sub "APCPLV42" (count%,          /* Number of Windows         */ ~
			size%,                                          ~
			sched%,                                          ~
			scr_dte$,                                        ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        lk_fn$(),        /* 1,2 Lk Fin                */ ~
                        #1,              /* (APCCUTWK) Saw Work File  */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6 )             /* (AMTBOMIF) Validity File  */

        dim readkey$24,descr$30,         /* GENCODES Primary Key      */ ~
            file$60,                     /* (PAR000)                  */ ~
            seq_w$3,                     /* (PAR000)                  */ ~
            lk_fn$(5%)30,                /* 1%=1Lock,2%=2Lock,3%=WFin */ ~
            cl$2,                        /* Color                     */ ~
            hnge$20, hh$8,               /* Hinge code and Descript   */ ~
            model$3, date$8,             /* Model Code                */ ~
            notes$14, title$50,          /* Detail Notes              */ ~
            size$3,                      /* Sizing System Number      */ ~
            tool$3,                      /* Tool Set-up Number        */ ~
            width$(2%)9,                 /* Width Size                */ ~
            height$(2%)9,                /* Height Size               */ ~
            weld_rec$71,                 /* Weld Record               */ ~
          /*bat_rec$149,                 ** New Weld Record           */ ~
            bat_rec$71,                  /* New Weld Record           */ ~
            mods$(100%)3,                /* Valid Models              */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            wrk_key$5, chk_key$5,        /* Primary Key               */ ~
            wrk_key1$51,chk_key1$51,     /* Work Key                  */ ~
            wrk_rec$200, chk_rec$200,    /* Work Record               */ ~
            part$25, chk_sav$5,          /* MFG Part Number           */ ~
            chk_part$25,                 /* MFG Test Part Number      */ ~
            chk_model$4, chk_size$7,     /* Model/Color - width&Height*/ ~  
            save_part$25,                /* Save MFG Part Number      */ ~
            save_cl$1,                   /* Save Color                */ ~
            ref$5, dt_ref$8,             /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            tw$1,                        /* Width Parts               */ ~
            th$1,                        /* Height Parts              */ ~
            col$(100%)25,                /* Cut Description           */ ~
            eq$(100%)8,                  /* Equation Codes            */ ~
            ct$(100%)9,                  /* Cut Widths and Heights    */ ~
            ct(100%),                    /* Cut Wid/Height Decimal    */ ~
            cr$(100%)10,                 /* Raw Material Part Number  */ ~
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece Yes or No       */ ~
            sh$(100%)1,                  /* Sash Type ( W, H, N )     */ ~
            hdr$40,                      /* Askuser Header Text       */ ~
            msg$(3%)79,                  /* Askuser Info Text         */ ~
            raw_mat$(5%)15,              /* Raw Material              */ ~
            style$(2%)15,                /* Style Information         */ ~
            welder$(2%)2,                /* Welder Numbers            */ ~
            welder_set$(2%)2,            /* Welder Setup Codes        */ ~
            welder_type$(2%)3,           /* Welder Type Frame or Sash */ ~
            welder_err$(2%)50            /* Welder Error Message      */             

        dim f2%(15%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
            name$(26%)8,                 /*   not yet checked (OPENCHCK*/~
            rslt$(26%)20                 /* Text from file opening     */
                                         /* Sash Files, No Frame Files */
            select #7, "@WELDE1@",                                       ~
                                consec , recsize = 71

            select #8, "@WELDE2@",                                       ~
                                consec , recsize = 71

            select #9, "@WELDE3@",                                       ~
                                consec , recsize = 71

            select #10,"@WELDE4@",                                       ~
                                consec , recsize = 71

/*  (EWD002)  --  BEG  */
            select #12,"@WELDR1@",                                       ~
                                consec , recsize = 71

            select #13,"@WELDR2@",                                       ~
                                consec , recsize = 71

            select #14,"@WELDR3@",                                       ~
                                consec , recsize = 71

            select #15,"@WELDR4@",                                       ~
                                consec , recsize = 71

            select #17,"@WELDS1@",                                       ~
                                consec , recsize = 71

            select #18,"@WELDS2@",                                       ~
                                consec , recsize = 71

            select #19,"@WELDS3@",                                       ~
                                consec , recsize = 71

            select #20,"@WELDS4@",                                       ~
                                consec , recsize = 71

            select #26,"@SAW036@",                                       ~
                                consec , recsize = 71
/*  (EWD002)  --  END  */
            ff% = 6%                    /* Starting Sash Channel (S)  */
            if scr_dept$ = "025" then ff% = 11%    /*  (EWD002)       */
            if scr_dept$ = "026" then ff% = 16%    /*  (EWD002)       */

	    Dept36_Weld$ = "N"
	    if scr_dept$ = "036" then Dept36_Weld$ = "Y"
            if scr_dept$ = "036" then ff% = 26%    /*  (EWD002)       */

            init(" ") name$()

            name$( 7%) = "@WELDE1@"
            name$( 8%) = "@WELDE2@"
            name$( 9%) = "@WELDE3@"
            name$(10%) = "@WELDE4@"
/*  (EWD002)  --  BEG  */
            name$(12%) = "@WELDR1@"
            name$(13%) = "@WELDR2@"
            name$(14%) = "@WELDR3@"
            name$(15%) = "@WELDR4@"
            name$(17%) = "@WELDS1@"
            name$(18%) = "@WELDS2@"
            name$(19%) = "@WELDS3@"
            name$(20%) = "@WELDS4@"
	    name$(26%) = "@SAW036@"
/*  (EWD002)  --  END  */
            count_hold% = size% 

            gosub load_weld                  /* MODS$() - VALID MODELS */

            msg$(1%)= "       The File (@WELDE1@) Already Exists.       "

            fcnt%,count% = 0%
            call "SHOSTAT" ("Creating Sampson Weld Files ("&scr_dept$&")")
            cw%, ch% = 0%                     /* Load Cut Descriptions */
            tw$ = "1" : th$ = "2"

            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            if err% <> 0% then goto exit_program
            init(" ") chk_sav$, chk_part$, chk_model$, chk_size$
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))
        create_next                       /* All Welded Sash Double Hung */
            read #1,key > wrk_key$, using L01360, wrk_key$, wrk_key1$,     ~
                                             wrk_rec$, eod goto create_done
L01360:          FMT CH(5), CH(51), CH(200)
            part$  = str(wrk_rec$,38%,25%)
            model$ = str(part$,1%,3%)
            chk_sav$ = wrk_key$                     /* EWD001          */
            for i% = 1% to mod_max%
               if model$ = mods$(i%) then goto L01430
            next i%
            goto create_next

L01430:     if len(part$) < 19 then goto create_next
               gosub check_next_window 
               gosub build_weld
               if width$(1) = " " and height$(1) = " " then              ~
                                                         goto create_next
                  dt_ref$ = str(wrk_rec$,1%,8%)
                  dt_seq$ = str(wrk_rec$,9%,5%)
                  ref$    = str(dt_ref$,4%,5%)
                  ssq$    = str(dt_seq$,3%,3%)
                  gosub lookup_color
                  gosub lookup_hinge
                  gosub build_detail
                  goto create_next
        create_done
 
	    if Dept36_Weld$ = "Y" then                                     ~
	       gosub build_end

REM            if ff% <> 6%  then close #ff%
                                                  /*  (EWD002)          */
            if ff% <> 6% or ff% <> 11% or ff% <> 16% then close #ff%
        goto exit_program

        check_next_window                         /* Look Ahead (1) Rec */
                                                  /* (EWD002) - Do Not  */
                                                  /* Double Stack       */
            if cont_head% <> 0% then return
                                                  /* (EWD002)           */
            chk_key$ = wrk_key$                   /* EWD001             */
            read #1,key > chk_key$, using L01360, chk_key$, chk_key1$,    ~
                                          chk_rec$, eod goto create_done
            chk_part$  = str(chk_rec$,38%,25%)
            chk_model$ = str(chk_part$,1%,4%)
            chk_size$  = str(chk_part$,13%,7%)
            if chk_model$ = str(part$,1%,4%) and                          ~
               chk_size$ = str(part$,13%,7%) then chk_sav$ = chk_key$
        return

        build_detail                           /* No Frame             */
	    if Dept36_Weld$ <> "Y" then goto NotDept36                       
	      
		j% = 1%
                gosub check_options               /* Tooling Data         */
		gosub check_count
                goto build_detail_weld 
	       
        NotDept36:
          init(" ") weld_rec$                  /* Sash is always  (1%) */
          for j% = 1% to weld%
             gosub check_options               /* Tooling Data         */
             gosub check_count
             str(weld_rec$,1%,5%)  = dt_seq$        /* Sequence No.    */
             str(weld_rec$,6%,1%)  = " "            /* Blank           */
             str(weld_rec$,7%,9%)  = width$(j%)     /* Width Dimension */
             str(weld_rec$,16%,1%) = " "            /* Blank           */
             str(weld_rec$,17%,9%) = height$(j%)    /* Height Dimension*/
             str(weld_rec$,26%,1%) = " "            /* Blank           */
             str(weld_rec$,27%,14%)= notes$         /* Notes           */
             str(weld_rec$,41%,18%)= " "            /* Blank Padding   */
             str(weld_rec$,59%,3%) = tool$          /* Tooling Setup No*/
             str(weld_rec$,62%,3%) = size$          /* Sizing Sys No.  */
             str(weld_rec$,65%,7%) = "       "      /* Padding         */

                                                    /*  (EWD002) - BEG */
             sash% = 1%
REM             if model$ = "773" or model$ = "775" then sash% = 2%
REM             if model$ = "787" or model$ = "789" then sash% = 2%
             if cont_head% = 2% then sash% = 2%
             for n% = 1% to sash%

             if opt1% <> 0% then goto L01870        /* Sash (S) File   */
                   write #ff%, weld_rec$, eod goto L01900
                                                   /* (EWD001)        */ 
                                                   /* (EWD002)        */ 
                   if cont_head% = 0% then wrk_key$ = chk_sav$
                   init(" ") chk_sav$, chk_model$, chk_part$, chk_size$
                                                    /* (2) the same size*/
             next n%

                                                    /*  (EWD002) - END */
L01870: next j%

        return
L01900:   call "SHOSTAT" ("(Error)-Writing Sampson Weld Detail?")
          stop
          goto L01870

        build_weld
          if part$ = save_part$ then return
             save_part$ = part$

	    if Dept36_Weld$ = "Y" then                                   ~
              gosub check_welder

          init(" ") width$() , height$()
          w%, h%, weld%, cont_head% = 0%                     /* (EWD002) */
          call "APCCUTCC" (part$, 0%, 0%, 0%, /* (CUT001) */             ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
          eq% = cw% + ch%
          for i% = 1% to eq%
            if sh$(i%) = "W" or sh$(i%) = "H" then goto L02080
               goto L12200
                                                  /* Deduct 1/4 Inch   */
L02080:     a  = ct(i%) - .25                     /* From Decimal Size */

            if sh$(i%) = "W" then w% = w% + 1%      /* Min Width (12") */
            if a < 12.0 and sh$(i%) = "W" then goto L02230
            if sh$(i%) = "H" then h% = h% + 1%      /* Max Height (96")*/
            if a > 96.0 and sh$(i%) = "H" then goto L02230
            if sh$(i%) <> "W" then goto L02180
                                                   /*  (EWD002)        */
               if str(part$,1%,3%) = "772" or str(part$,1%,3%) = "774"  ~
                                       then gosub calc_twin
               if str(part$,1%,3%) = "786" or str(part$,1%,3%) = "788"  ~
                                       then gosub calc_twin
               if str(part$,1%,3%) = "773" or str(part$,1%,3%) = "775"  ~
                                       then gosub calc_triple
               if str(part$,1%,3%) = "787" or str(part$,1%,3%) = "789"  ~
                                       then gosub calc_triple
                                                   /*  (EWD002)        */
               convert a to width$(w%), pic(###.####)

               goto L12200
L02180:     convert a to height$(h%), pic(###.####) /* Must be Height  */

L12200:   next i%
          weld% = w%
	    if Dept36_Weld$ = "Y" then                                   ~
               goto build_schedule
	   
        return
L02230:   init(" ") width$() , height$()        /* Max Limits Exceeded */
        return

REM         +---------------------------------------------------------------+
REM         | New format for weld files.                                    |
REM         +---------------------------------------------------------------+

        build_schedule                         /* SET-UP SCHEDULE REV. */
          if hit% = 1% then return             /* Schedule has not been*/
          j% = 1%
          gosub check_options               /* Tooling Data         */
          gosub check_count
          init(" ") bat_rec$                   /* Completed Yet.       */
          inc$  = " (" & bat$ & ")"                   /*  Batch No (5) */
          inc1$ = "(" & scr_dept$ & ") "              /* Dept Code (6) */
          inc2$ = "(" & ssq$ & ")"             /* Sarting Seq. No. (5) */  

          file$ = inc2$ & inc1$ & "WD-SW " & scr_dte$ & inc$
          if str(scr_load$,1%,1%) <> "N" then str(file$,6%,2%) = "/U"

          date$ = date : call "DATEFMT" (date$)
          file$ = "(712) Sampson Sash  Welder (E) for ( "&date$&" )"
          if opt1% = 1% then str(file$,15%,16%) = "Frame Welder (F)"

          str(bat_rec$,1%,2%)   = "FR"                  /* FILE REV.   */
          str(bat_rec$,3%,6%)   = "930318"              /* REVISION NO */
          str(bat_rec$,9%,141%) = " "                   /* LINE FEED   */
          write #ff%, bat_rec$, eod goto L02070

          gosub build_schedule_no
          gosub build_schedule_title
          gosub build_schedule_quantity
          hit% = 1%
        return
L02070:   errormsg$ = "(Error) When Writing Revision Record??"
          gosub error_prompt
        return

        build_schedule_no
          init(" ") bat_rec$
          gosub assign_schedule
          convert sched% to sched$, pic(000)

          str(bat_rec$,1%,2%) = "SN"                /* SCHEDULE NUMBER */
          str(bat_rec$,3%,3%) = sched$              /* SCHED (100-999) */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02200
        return
L02200:   errormsg$ = "(Error) When writing Schedule Record??"
          gosub error_prompt
        return

        assign_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #2,hold,key = readkey$, using L04320 , sched$,            ~
                                                           eod goto L04400
L04320:       FMT POS(25), CH(3)
            convert sched$ to sched%, data goto L04400

            convert (sched% + 1%) to sched$, pic(000)
            if (sched% + 1%) > 999% then sched$ = "100"
            put #2, using L04320 , sched$
            rewrite #2
        return
L04400:    errormsg$ = "(Error) When assigning New Schedule Number??"
           gosub error_prompt
        return

        build_schedule_title                        /* SCHEDULE TITLE  */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TI"              /* SCHEDULE TITLE  */
          str(bat_rec$,3%,50%)  = file$             /* TITLE DESCRIPT  */
          str(bat_rec$,53%,97%)= " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02310
        return
L02310:   errormsg$ = "(Error) When Writing Schedule Title??"
          gosub error_prompt
        return

        build_schedule_quantity                     /* SCHEDULE Quantity*/
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TW"              /* SCHEDULE Quantity*/
REM       str(bat_rec$,3%,3%)   = size$             /* Batch Size       */
          convert count_hold% to str(bat_rec$,3%,3%), pic(000)
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L12320
        return
L12320:   errormsg$ = "(Error) When Writing Schedule Quantity"
          gosub error_prompt
        return

        build_detail_weld                         /* j% = 1%   (Frame) */
                                                  /* j% = 2%   (Sash)  */
          init(" ") bat_rec$
          convert (count_hold% + 1%) to seq_w$, pic(###)
          str(bat_rec$,1%,2%)   = "WE"                  /* 'WE' Weld   */
          str(bat_rec$,3%,5%)   = ref_no$               /* Load Number */
          str(bat_rec$,8%,5%)   = "     "
          str(bat_rec$,13%,3%)  = seq_w$                /* Window No.  */
          str(bat_rec$,16%,4%)  = "0001"                /* Unit Qty    */
           if j% = 2% and model$ = "421" then                             ~
             str(bat_rec$,16%,4%) = "0002"              /* Change Qty  */

          if j% = 2% and model$ = "441" then                             ~
             str(bat_rec$,16%,4%) = "0002"              /* Change Qty  */

          if j% = 2% and model$ = "431" then                             ~
             str(bat_rec$,16%,4%) = "0003"              /* Change Qty  */

          if j% = 2% and model$ = "451" then                             ~
             str(bat_rec$,16%,4%) = "0002"              /* Change Qty  */

          str(bat_rec$,20%,9%)  = width$(j%)            /* Width Cut   */
          str(bat_rec$,29%,9%)  = height$(j%)           /* Height Cut  */
                                                 
                                                        /* Frame/Sash  */
          str(bat_rec$,38%,15%) = style$(j%)            /* Style Code  */

          str(bat_rec$,53%,4%)  = cl$ & "  "            /* color Code  */
                                                        /* Cart Type Pieces */
          str(bat_rec$,57%,18%) = sa_rack$(1%)
          str(bat_rec$,57%,3%)  = welder_type$(j%)
                                                        /* Bin Location*/
          str(bat_rec$,75%,16%) = "STD - Window    "    /* Part Desc   */
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,1%) = " "                   /* lable Format*/
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels  */
          str(bat_rec$,124%,15%)= "               "     /* License Plat*/
          str(bat_rec$,139%,8%) = "        "            /* Machine Code*/
          str(bat_rec$,147%,2%) = welder$(j%)           /* Welder No.  */
          str(bat_rec$,149%,1%) = welder_set$(j%)       /* Welder Setup*/
                                                        /*  Number     */
          write #ff%, bat_rec$, eod goto L02640
        return
L02640: errormsg$ = welder_err$(j%)
        gosub error_prompt
        return

        build_end
          /* if count_hold% = 0% then return */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%) = "**"                /* END OF BATCH    */
          str(bat_rec$,3%,3%) = "END"               /* LINE FEED       */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02750
          bat_no% = bat_no% + 1%
          convert bat_no% to bat$, pic(00)

        return
L02750:   errormsg$ = "(Error) Building End Record??"
          gosub error_prompt
        return

error_prompt:
	return

        check_welder                                 /* 1st Frame      */
                                                     /* 2nd Sash       */
            check% = 0%
            init(" ") readkey$, style$(), welder$(), welder_set$(), desc$
            str(readkey$,1%,9%)  = "NEWFLEXWD"
            str(readkey$,10%,1%) = "F"               /* F = Frame Welder*/
                                                     /* S = Sash Welder */
                                                     /* Model No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04290
L04280:       FMT POS(25), CH(25)

              style$(1%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(1%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(1%) = str(desc$,20%,1%)    /* Set Up Code    */

              check%    = 1%                         /* Valid Equation */
                                                     /* Sash Logic     */
L04290:     init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "NEWFLEXWD"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
                                                     /* Mode No.       */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            read #2,key = readkey$, using L04280 , desc$, eod goto L04300

              style$(2%)      = str(desc$,1%,15%)    /* Style Code     */
              welder$(2%)     = str(desc$,17%,2%)    /* Welder No.     */
              welder_set$(2%) = str(desc$,20%,1%)    /* Set Up Code    */

              check%    = 1%                         /* Valid Equation */
L04300: return


REM******************************************************************

        check_count                                /* Max Line Counter */
           if opt1% <> 0% then  goto L02320
              fcnt% = fcnt% + 1%                   /* Sash (S) Counter */
              if fcnt% = 1% then goto L02350       /* 1st Time         */
                 if mod(fcnt%,240) <> 0 then return /* Batch Size 240  */
                    goto L02350
L02320:    return                                 /* Frame (F) Counter */
                                                  /* No Frames         */
L02350:          gosub create_file                 /* Create File and  */
        return                                     /* Build Header     */

        create_file
           if opt1% <> 0% then goto L02450         /* Sash (S) File FF%*/
	   if Dept36_Weld$ <> "Y" then goto L02400                  
              fx% = ff%
              fy% = ff%
               if fcnt% = 1% then goto L02510
              return

L02400:
               fx% = ff%
               ff% = ff% + 1%                      /* Next File        */
               fy% = ff%
               if fcnt% = 1% then goto L02510
               goto L02490
L02450:    return                                  /* Frame (F) File FS%*/
                                                   /* No Frames         */
L02490:    gosub create_file_a                    /* Close Old File 1st */
        return
L02510:    gosub create_file_b                    /* 1st Time for Entry */
        return

        file_exists
            comp% = 2%
            hdr$ = "**** Sampson Weld File 712 ***"
        REM MSG$(1) = "        The File (@SAMPS1@) Already Exists.     "
            msg$(2) = "             B u i l d   S a m p s o n           "
            msg$(3) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        lookup_color
          if str(part$,4%,1%) = save_cl$ then return
             save_cl$ = str(part$,4%,1%)
          init(" ") readkey$, cl$
          str(readkey$,1%,9%)   = "COLOR    "
          str(readkey$,10%,15%) = str(part$,4%,1%)
          read #2,key = readkey$, using L02700  , cl$, eod goto L02710
L02700:      FMT POS(25), CH(2)
L02710: return

        check_options
          if j% = 1% then goto L02820 else goto L02860
        return
L02820:   opt1% = 0%                                    /* Sash        */
          tool$ = "001"
          size$ = "005"
        return
L02860:   opt1% = 1%                                    /* No Frames   */
          tool$ = "001"
          size$ = "005"
        return

        exit_program

        end

        create_file_a                       /* Close Old File First */
            close #fx%
        create_file_b                       /* Open New File        */
            call "SHOSTAT" ("Creating Weld File - "& name$(fy%) )
            call "OPENFILE" (#fy%,"IO   ", f2%(fy%), rslt$(fy%), axd$ )
            if f2%(fy%) <> 0% then goto L03110
               str(msg$(1%),19%,8%) = name$(fy%)
               gosub file_exists
               if comp% <> 16% then goto L03070
                  call "FILEBGON" addr(#fy%)
                  goto L03110

L03070:        close #fy%
               call "OPENFILE" (#fy%,"EXTND",f2%(fy%),rslt$(fy%), axd$ )
               goto L03160

L03110:     str(rslt$(fy%),1%,6%)  = "OUTPTP"
            str(rslt$(fy%),7%,8%)  = "00001000"
            str(rslt$(fy%),15%,3%) = "100"
            str(rslt$(fy%),18%,3%) = "100"
            call "OPENFILE" (#fy%, "OUTPT", f2%(fy%), rslt$(fy%), axd$ )
L03160:     gosub build_header
        return

        build_header
            init(" ") weld_rec$, title$, date$
            date$ = date : call "DATEFMT" (date$)
            title$ = "(712) Sampson Sash  Welder (E) for ( "&date$&" )"
            if opt1% = 1% then str(title$,15%,16%) = "Frame Welder (F)"
            str(weld_rec$,1%,6%)  = "940920"    /* File Version Number */
            str(weld_rec$,7%,3%)  = "071"       /* Record Length       */
            str(weld_rec$,10%,62%)= " "         /* Padding             */
            /*if opt1% = 0% then      Sash (S) File       */
	    if Dept36_Weld$ = "N" and opt1% = 0% then                     ~
               write #ff%, weld_rec$, eod goto L03410

L03320:     init(" ") weld_rec$
            str(weld_rec$,1%,50%) = title$      /* Weld Batch Title    */
            str(weld_rec$,51%,21%)= " "         /* Padding             */

            /*if opt1% = 0% then      Sash (A) File       */
	    if Dept36_Weld$ = "N" and opt1% = 0% then                     ~
               write #ff%, weld_rec$, eod goto L03410
        return
L03410:   call "SHOSTAT" ("(Error)-Writing Sampson (S) Header?")
          stop
          goto L03320

        load_weld
          mod_max% = 0%
          init(" ") readkey$, mods$()
          str(readkey$,1%,9%) = "APCWELD  "

L03530:   read #2,key > readkey$, using L03540, readkey$, eod goto L03590
L03540:      FMT CH(24)
          if str(readkey$,1%,9%) <> "APCWELD  " then goto L03590
             mod_max% = mod_max% + 1%
             if mod_max% > 100% then mod_max% = 100%
          mods$(mod_max%) = str(readkey$,10%,3%)
          goto L03530
L03590: return

        lookup_hinge                                  /* Look Up Hinge */
            init(" ") readkey$, descr$, hnge$, hh$, notes$
            notes$ = "STANDARD WIND."
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = str(part$,9%,2%)
            read #2,key = readkey$, using L03910 , descr$, eod goto L03960
L03910:        FMT POS(25), CH(30)
            p% = pos(descr$ = "-")
            hnge$ = str(descr$,1%,p%-2%)
            if str(hnge$,1%,2%) = "CO" then hh$ = "COTTAGE "
            if str(hnge$,1%,2%) = "OR" then hh$ = "ORIEL   "
L03960:     if len(hh$) > 2 then str(notes$,1%,8%) = hh$
            if str(part$,11%,1%) = "4" then str(notes$,10%,5%) = "TSO  "
            if str(part$,11%,1%) = "5" then str(notes$,10%,5%) = "BSO  "
            if str(part$,11%,1%) = "6" then str(notes$,10%,5%) = "FGO  "
        return

        calc_twin                                   /*  (EWD002)        */
             a = round((a / 2) - 0.375,4)
             cont_head% = 1%
        return

        calc_triple
             a = round((a / 3) - 0.5,4)
             cont_head% = 2%
        return                                     /*  (EWD002)        */



