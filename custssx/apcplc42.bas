*       ****************************************************************~
*                                  ( L i n e a l m a t e )             *~
*                                                (Not Usesd)           *~
*                  (PAR000) 01/15/2006 CR347          Dept???          *~
*        APCPLC42 - Create File with Data for Saw Optimization         *~
*                   (Same as 'APCPLJ42' Except for Revision No.)       *~
*                   ( Line No. = 1670   Revision No. = '910318')       *~
*                                                                      *~
*            Note - Primary Subroutine 'BUILD_SAW_RECS'                *~
*                   (1) 600 Series by Load Uses Key '0'                *~
*                   (2) 700 Series by Load Uses Key '1'                *~
*                   (3) 800 Series by Load Uses Key '0' (Hopper)       *~
*                   (4) 400 Series by Load Uses Key '0' New Family     *~
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
*            Note - New Subroutine to Assign the Next Schedule Number  *~
*                   for Linealmate. 'ASSIGN_SCHEDULE'                  *~
*                                                                      *~
*            (EWD001) - Mod to use different Bridge file name for      *~
*                       Department (002). 06/18/1999                   *~   
*                                                                      *~ 
*            (EWD002) - Mod to remove logic for Department 002         *~
*                       09/23/1999.                                    *~   
*                                                                      *~ 
*            (EWD003) - Mod new logic for Dept. 007                    *~
*                       04/25/2000                                     *~   
*                                                                      *~ 
*            (EWD004) - Mod new logic Beige                            *~
*                       04/27/2000                                     *~   
*                                                                      *~ 
*           05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
*      ****************************************************************

        sub "APCPLC42" (size%,           /* Batch Size (No. Windows)  */ ~
                        sched%,          /* Starting Schedule Number  */ ~
                        scr_dte$,        /* Production Date Formatted */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        scr_load$,       /* Production Load Number    */ ~
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
            sched$3,                     /* Schedule Numbers          */ ~
            tsched$3,                    /* Starting Schedule Number  */ ~
            co$30,                       /* Linealmate Descriptive Not*/ ~
            bat_no$26,                   /* Batch Identifiers ( A-Z ) */ ~
            bat_rec$122,                 /* Batch Record              */ ~
            bat$2,                       /* Number of Batches         */ ~
            apc_scr$120,                 /* Screen Text               */ ~
            apc_prt$60,                  /* Print Text                */ ~
            apc_sze$20,                  /* Size                      */ ~
            scr_dte$8,                   /* Completion Date           */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            scr_load$5,                  /* Production Load           */ ~
            wrk_key1$51,                 /* WORK KEY                  */ ~
            wrk_rec$200,                 /* WORK RECORD               */ ~
            seq$3,                       /* Record Number Key         */ ~
            dtl_load$5,                  /* Load Number               */ ~
            dtl_part$25,                 /* MFG Part Number           */ ~
            save_part$25,                /* MFG Part Number           */ ~
            ref_no$5, dt_ref$8,          /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP      */ ~
            col$(100%)25,                /* Cut Description           */ ~
            eq$(100%)8,                  /* Equation Codes            */ ~
            ct$(100%)9,                  /* Cut Widths and Heights    */ ~
            ct(100%),                    /* Cut Wid/Height Decimal    */ ~
            sh$(100%)1,                  /* Sash Type                 */ ~
            cr$(100%)10,                 /* Raw Material Part Number  */ ~
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece Yes or No       */ ~
            tw$1,                        /* WIDTH CUT PARTS           */ ~
            th$1,                        /* HEIGHT CUT PARTS          */ ~
            sa_d2$30,                    /* MFG Part Description      */ ~
            sa_type$(100%)2,             /* RECORD TYPE 'SA' OR 'LA'  */ ~
            sa_piece$(100%)4,            /* Number of Pieces's to Cut */ ~
            sa_cut$(100%)9,              /* Cut Size for Piece's      */ ~
            sa_part$(100%)15,            /* Raw Material Part Number  */ ~
            sa_rack$(100%)18,            /* Bin Loc and No. of Pieces */ ~
            sa_d1$(100%)16,              /* Raw Material Description  */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$10,                      /* Schedule File Identifier  */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79                  /* Error Message Text        */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAWS@",                                        ~
                                consec , recsize = 122
                                               /* (EWD001) - Dept 002  */
                                               /* (EWD002) No longer   */
                                               /*   used by Dept 002   */ 
            cw%, ch% = 0%
            xx% = 3%
                                                 /* Create Saw Batches */
            call "OPENFILE" (#xx%, "IO   ", f2%(xx%), rslt$(xx%), axd$ )
            if f2%(xx%) <> 0% then goto L01110
               gosub file_exists
               if comp% <> 16% then goto L01070
                  call "FILEBGON" addr(#xx%)
                  goto L01110

L01070:        close #xx%
               call "OPENFILE" (#xx%, "EXTND", f2%(xx%), rslt$(xx%), axd$ )
               goto L01170

L01110:     str(rslt$(xx%),1%,6%)  = "OUTPTP"
            str(rslt$(xx%),7%,8%)  = "00001000"
            str(rslt$(xx%),15%,3%) = "100"
            str(rslt$(xx%),18%,3%) = "100"
            call "OPENFILE" (#xx%, "OUTPT", f2%(xx%), rslt$(xx%), axd$ )

L01170:     bat_no$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            bat_no% = 1% : count% = 0% : save_part$ = " "
        REM SCHED% = 100%                             /* MOD - 09/04/92 */
            convert sched% to tsched$, pic(###)

            call "SHOSTAT" ("Starting Schedule ("&tsched$&") for Dept ("&~
                                                           scr_dept$&")")
            tw$ = "1" : th$ = "2"             /* LOAD CUT DESCRIPTIONS */
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            if err% <> 0% then goto exit_program
            xcount% = 0%
            wrk_key1$ = all(hex(00))
            if scr_prod$ <> "7" then goto L01340
            read #1,key 1% > wrk_key1$, using L01410 , wrk_key1$, wrk_rec$,~
                                                     eod goto create_done
            goto L01420
                                             /* DOUBLE HUNG AND HOPPER */
L01340:     read #1,key > wrk_key1$, using L01410 , wrk_key1$, wrk_rec$,   ~
                                                     eod goto create_done

            goto L01420
        create_next
            read #1, using L01410 , wrk_key1$, wrk_rec$,                   ~
                                                     eod goto create_done
L01410:          FMT POS(6), CH(51), CH(200)
L01420:     dt_ref$   = str(wrk_rec$,1%,8%)
            dt_seq$   = str(wrk_rec$,9%,5%)
            dtl_load$ = str(wrk_rec$,29,5%)
            dtl_part$ = str(wrk_rec$,38%,25%)
            dt_samp$  = str(wrk_rec$,77%,1%)
            ref_no$   = str(dt_ref$,4%,5%)
            ssq$      = str(dt_seq$,3%,3%)
            if len(dtl_part$) < 19 then goto create_next
            if count% = 0% then gosub build_schedule   /* CREATE BATCH */
               gosub build_saw_recs
               if sa_max% = 0% then goto create_next
                  seq% = 0%
                  for sa% = 1% to sa_max%
                      gosub build_detail
                  next sa%
                  count% = count% + 1%
                  if count% < size%  then goto create_next
                     gosub build_end
                     count% = 0%
                     goto create_next
        create_done
            gosub build_end
            close #xx%
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
          write #xx%, bat_rec$, eod goto L01810

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
          write #xx%, bat_rec$, eod goto L01940
        return
L01940:   call "SHOSTAT" ("(Error) - Writing Schedule Number ??? ")
          stop
        return

        build_schedule_title                        /* SCHEDULE TITLE  */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TI"              /* SCHEDULE TITLE  */
          str(bat_rec$,3%,30%)  = file$             /* TITLE DESCRIPT  */
          str(bat_rec$,33%,90%) = " "               /* LINE FEED       */
          write #xx%, bat_rec$, eod goto L02050
        return
L02050:   call "SHOSTAT" ("(Error) - Writing Schedule Title ??? ")
          stop
        return

        build_detail
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
          str(bat_rec$,75%,16%) = sa_d1$(sa%)           /* Part Desc   */
          str(bat_rec$,91%,30%) = sa_d2$                /* Label Desc  */
          str(bat_rec$,121%,2%) = " "                   /* Line Feed   */
          write #xx%, bat_rec$, eod goto L02300
        return
L02300:   call "SHOSTAT" ("(Error) - Writing Schedule Detail ??? ")
          stop
        return

        build_end
          if count% = 0% then return
          init(" ") bat_rec$
          str(bat_rec$,1%,2%) = "**"                /* END OF BATCH    */
          str(bat_rec$,3%,3%) = "END"               /* LINE FEED       */
          str(bat_rec$,6%,117%) = " "               /* LINE FEED       */
          write #xx%, bat_rec$, eod goto L02430
          bat_no% = bat_no% + 1%
        return
L02430:   call "SHOSTAT" (errormsg$) : stop
        return

        build_saw_recs
          if save_part$ <> dtl_part$ then goto L02530
             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
             return

L02530:   save_part$ = dtl_part$
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$
          
          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
          gosub build_descript
          sash_flag% = 0%                       /* (EWD003)            */
          eq% = cw% + ch%
          for i% = 1% to eq%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */
            if cc$(i%) = "N" or cc$(i%) = " " then goto L02940

            cut% = 0%                    /* Skip - No Equation Records */
            convert str(ct$(i%),1%,3%) to cut%, data goto L02720
L02720:
            if cut% = 0% then goto L02940
               sa% = sa% + 1%
               sa_type$(sa%)  = "SA"
               if cc$(i%) = "N" then sa_type$(sa%) = "LA"

               sa_piece$(sa%) = "  " & cp$(i%)     /* No Pieces Needed */
               sa_cut$(sa%)   = ct$(i%)            /* Length of Cut    */
               str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
                                                   /* (EWD003)         */
               if scr_dept$ <> "007" then goto L02730
                  if str(sa_part$(sa%),1%,10%) = "6115200490" or         ~
/* (EWD004) */       str(sa_part$(sa%),1%,10%) = "6115600490" then       ~
                     sash_flag% = sash_flag% + 1%

                  if s$ = "5" then sash_flag% = 2% 
                  if sash_flag% <> 2% then goto L02730 
                     str(sa_part$(sa%),11%,1%) = "1" 
                     str(sa_part$(sa%),12%,4%) = "     "
                     goto L02740

L02730:        str(sa_part$(sa%),11%,5%) = "     "
                                                       /* (EWD003)         */    
L02740:        str(sa_rack$(sa%),1%,4%)  = "@F A"      /* Bin Location */
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
L02940:      next i%
             sa_max% = sa%
        return

        build_descript
            init(" ") co$, x$, s$, readkey$
            cnt% = 3%                            /* 1st Set Model Code */
            str(co$,1%,3%) = str(dtl_part$,1%,3%)
            s$ = str(dtl_part$,11%,1%)           /* Set Screen Code    */
            x$ = str(dtl_part$,12%,1%)           /* Set Lock Code      */
                                                 /* (PAR000)           */
            p% = pos(lk_fn$(1%) = x$)            /* 1 Lock Codes       */
            if p% = 0% then goto L03080
               str(co$,cnt%+1%,3%) = "/1K"
               goto L03110
                                                 /* (PAR000)           */
L03080:     p% = pos(lk_fn$(2%) = x$)            /* 2 Lock Codes       */
            if p% = 0% then goto L03120
               str(co$,cnt%+1%,3%) = "/2K"
L03110:     cnt% = cnt% + 3%
L03120:     p% = pos("456" = s$)
            if p% = 0% then goto L03180
               str(co$,cnt%+1%,3%) = "/TS"         /* Set as Default   */
               if s$ = "5" then str(co$,cnt%+1%,3%) = "/BS"
               if s$ = "6" then str(co$,cnt%+1%,3%) = "/FG"
               cnt% = cnt% + 3%
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
L03390:     if str(dtl_part$,9%,2%) <> "09" then goto L03430
               str(co$,cnt%+1%,3%) = "/33"                 /* 1/3,1/3  */
               cnt% = cnt% + 3%

L03430: return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "        The File (@SAWS@) Already Exists.       "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

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

        exit_program
          if xcount% > 0% then goto L03770
             call "SHOSTAT" ("N O   L I N E A L M A T E   D A T A")
             call "PAUSE" addr(300%)

L03770: end
