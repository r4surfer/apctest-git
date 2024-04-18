*       ****************************************************************~
*          Department (036)  ( L i n e a l m a t e )                   *~
*                                                                      *~
*                  (PAR000) 01/15/2006 CR347          Dept (048)       *~
*        APCPLW42 - Create File with Data for Saw Optimization         *~
*                   (Same as 'APCPLJ42' Except for Revision No.)       *~
*                   ( Rev. No. '930318')                               *~
*                                                                      *~
*            Note - Primary Subroutine 'BUILD_SAW_RECS'                *~
*                   for the New Family of Windows, for Frames.         *~
*                   fo Department '712'                                *~
*                   SAW_NO$ = "09" for Sash's     Saw Type (1)         *~
*                   SAW_NO$ = "12" for Frame's    Saw Type (2)         *~
*                   saw_no$ = "13" for Header's   Saw Type (3)         *~ 
*                   BAT_REC$,147%,2%) = SAW_NO$                        *~
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
*            Note - New Subroutine to Determine Frame and Sash Saw     *~
*                   Cuts for New Family Products. 712 Dept             *~
*                                                                      *~
*            (EWD001) - Mode Put all batches in (1) File @SAW09@       *~
*                       @SAW12@ and @SAW13@ are currently not active   *~
*                                                                      *~   
*            (EWD002) - Mode Put all batches into three seperate       *~
*                       files. @SAW09@, @SAW12@ and @SAW13@            *~
*                       (as of 07/16/99)                               *~   
*05/19/2014! (CUT001) mod to add dim fields to CUTCC             ! CMG *~
*       ****************************************************************

        sub "APCPLW42" (size%,           /* Batch Size (No. Windows)  */ ~
                        sched%,          /* Starting Schedule Number  */ ~
                        scr_dte$,        /* Production Date Formatted */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        scr_load$,       /* Production Load           */ ~
                        saw_type$,       /* Sash-1,Frame-2,Header-3   */ ~
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
            co$30,                       /* Linealmate Descriptive Not*/ ~
            c_o$2,                       /* COTTAGE, ORIEL CODE-CO,OR */ ~
            bat_no$26,                   /* Batch Identifiers ( A-Z ) */ ~
            bat_rec$149,                 /* Batch Record              */ ~
            bat$2,                       /* Number of Batches         */ ~
            apc_scr$120,                 /* Screen Text               */ ~
            apc_prt$60,                  /* Print Text                */ ~
            apc_sze$20,                  /* Size                      */ ~
            scr_dte$8,                   /* Completion Date           */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            scr_load$5,                  /* Production Load           */ ~
            show_msg$40, err_msg$40,     /* Shostat Text              */ ~
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
            sa_m$(100%)8,                /* WINDOW TYPE,PROFILE TYPE   */~
            machine$3, set_up$1,         /* Store Window Type code    */ ~
            saw_no$2,                    /* Machine Saw Number        */ ~
            saw_type$1, file_type$1,     /* Sash, Frame,Header        */ ~
            ff_nam$(3%)7, ff%(3%),       /* Bridge File Name/Channel  */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$3,                       /* Schedule File Identifier  */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79                   /* ASKUSER Info Text         */

        dim f2%(11%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #7,  "@SAW09@",                                       ~
                                consec , recsize = 149      /* Sash's  */

            select #8,  "@SAW12@",                                       ~
                                consec , recsize = 149      /* Frame's */

            select #9,  "@SAW13@",                                       ~
                                consec , recsize = 149      /* Header's*/
       
            ff%(1%) = 7%  :  ff_nam$(1%) = "@SAW09@"
            ff%(2%) = 8%  :  ff_nam$(2%) = "@SAW12@"
            ff%(3%) = 9%  :  ff_nam$(3%) = "@SAW13@"

            if saw_type$ = "1" then file_type$ = "S"
            if saw_type$ = "2" then file_type$ = "F"
            if saw_type$ = "3" then file_type$ = "H"

            saw_type% = 1%
            convert saw_type$ to saw_type%, data goto L01000

L01000:     convert saw_type% to saw_type$, pic(#)
 
            str(err_msg$,1%,12%) = "Error-Sash  "
            if saw_type$ = "2" then str(err_msg$,7%,6%) = "Frame " 
            if saw_type$ = "3" then str(err_msg$,7%,6%) = "Header"
 
          
            for kk% = 1% to 3%                        /* For Dept (036)  */
                                                      /* '712' line      */
                if saw_type% <> kk% then goto L01310  /* Do Not Open     */

                xx% = ff%(kk%)             /* Temperary Test on Using a  */
                                           /* single file. All in One    */

                                           /* (EWD002) Use 3 Files       */
        REM     xx% = 7%                   /* Set to Channel (7)         */
                                           /* (EWD001)-8 and 9 not Active*/

                cw%, ch% = 0%
                init(" ") rslt$(), axd$
                                                   /* Create Saw Batches */
                call "OPENFILE" (#xx%,"IO   ",f2%(xx%),rslt$(xx%),axd$ )
                if f2%(xx%) <> 0% then goto L01250
                if saw_type$ <> "1" then goto L01210
                                            /* Temperary Force append    */
                                            /* mode when not File Type(1)*/    
                gosub file_exists
                if comp% <> 16% then goto L01210
                   call "FILEBGON" addr(#xx%)
                   goto L01250

L01210:            close #xx%
                call "OPENFILE" (#xx%,"EXTND",f2%(xx%),rslt$(xx%),axd$ )
                goto L01310

L01250:         str(rslt$(xx%),1%,6%)  = "OUTPTP"
                str(rslt$(xx%),7%,8%)  = "00001000"
                str(rslt$(xx%),15%,3%) = "100"
                str(rslt$(xx%),18%,3%) = "100"
                call "OPENFILE" (#xx%, "OUTPT", f2%(xx%), rslt$(xx%), axd$ )

L01310:     next kk%

            bat_no$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

            bat_no% = 1% : count% = 0% : save_part$ = " "
            hit% = 0%
            show_msg$ = "Starting Sched. xxx for Dept(xxx) xxxxxx"
            str(show_msg$,17%,3%) = "xxx"
            str(show_msg$,30%,3%) = scr_dept$
            str(show_msg$,35%,6%) = "Sash  "
            if saw_type$ = "2" then str(show_msg$,35%,6%) = "Frame " 
            if saw_type$ = "3" then str(show_msg$,35%,6%) = "Header"
 
            call "SHOSTAT" (show_msg$)

            tw$ = "1" : th$ = "2"             /* Load Cut Descriptions */
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            if err% <> 0% then goto exit_program
            xcount% = 0%
            wrk_key1$ = all(hex(00))
                                             /* New Family of Windows  */
            read #1,key > wrk_key1$, using L01500 , wrk_key1$, wrk_rec$,   ~
                                                     eod goto create_done
            goto L01510
        create_next
            read #1, using L01500 , wrk_key1$, wrk_rec$,                   ~
                                                     eod goto create_done
L01500:          FMT POS(6), CH(51), CH(200)
L01510:     dt_ref$   = str(wrk_rec$,1%,8%)
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
                     count% = 0% : hit% = 0%
                     goto create_next
        create_done
            gosub build_end
            close #xx%
            convert (bat_no% - 1%) to bat$, pic(##)
        goto exit_program

        build_schedule                         /* SET-UP SCHEDULE REV. */
          if hit% = 1% then return
          init(" ") bat_rec$
          inc$ = "(" & str(bat_no$,bat_no%,1%) & ")"
          file$ = "(xxx)   Saw xx/xx/xx (x)xxxxxx"
          str(file$,2%,3%)  = scr_dept$
          str(file$,13%,8%) = scr_dte$
          str(file$,22%,3%) = inc$
          str(file$,25%,6%) = "Sash  "
          if saw_type$ = "2" then str(file$,25%,6%) = "Frame " 
          if saw_type$ = "3" then str(file$,25%,6%) = "Header"
     
          if str(scr_load$,1%,1%) <> "N" then str(file$,6%,2%) = "/U"
          str(bat_rec$,1%,2%)   = "FR"                  /* FILE REV.   */
          str(bat_rec$,3%,6%)   = "930318"              /* REVISION NO */
          str(bat_rec$,9%,141%) = " "                   /* LINE FEED   */
          write #xx%, bat_rec$, eod goto L01920

          gosub build_schedule_no
          gosub build_schedule_title
          hit% = 1%
        return
L01920: str(err_msg$,14%,27%) = "-Writing Revision No.??    "
        goto error_display
  
        build_schedule_no
          init(" ") bat_rec$
          gosub assign_schedule
          convert sched% to sched$, pic(000)

          str(bat_rec$,1%,2%) = "SN"                /* SCHEDULE NUMBER */
          str(bat_rec$,3%,3%) = sched$              /* SCHED (100-999) */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #xx%, bat_rec$, eod goto L02050
        return
L02050:   str(err_msg$,14%,27%) = "-Writing Schedule No.??    "
          goto error_display


        build_schedule_title                        /* SCHEDULE TITLE  */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TI"              /* SCHEDULE TITLE  */
          str(bat_rec$,3%,30%)  = file$             /* TITLE DESCRIPT  */
          str(bat_rec$,33%,117%)= " "               /* LINE FEED       */
          write #xx%, bat_rec$, eod goto L02160
        return
L02160:   str(err_msg$,14%,27%) = "-Writing Schedule Title??  "  
          goto error_display

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
          str(bat_rec$,121%,1%) = " "                   /* Line Feed   */
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels  */
          str(bat_rec$,124%,15%)= "               "     /* License Plat*/
          str(bat_rec$,139%,8%) = sa_m$(sa%)            /* Machine Code*/
          str(bat_rec$,147%,2%) = saw_no$               /* Saw Number  */
          str(bat_rec$,149%,1%) = set_up$               /* Setup Number*/
          write #xx%, bat_rec$, eod goto L02460
        return
L02460:   str(err_msg$,14%,27%) = "-Writing Schedule Detail?? "
          goto error_display

        build_end
          if count% = 0% then return
          init(" ") bat_rec$
          str(bat_rec$,1%,2%) = "**"                /* END OF BATCH    */
          str(bat_rec$,3%,3%) = "END"               /* LINE FEED       */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #xx%, bat_rec$, eod goto L02590
          bat_no% = bat_no% + 1%
        return
L02590:   str(err_msg$,14%,27%) = "-Writing End-Of-Batch??    "
          goto error_display

        build_saw_recs
          if save_part$ <> dtl_part$ then goto L02690
             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
             return

L02690:   save_part$ = dtl_part$
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$()
          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
          gosub build_descript
          eq% = cw% + ch%
          for i% = 1% to eq%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */
            if cc$(i%) = "N" or cc$(i%) = " " then goto L03160

            cut% = 0%                    /* Skip - No Equation Records */
            convert str(ct$(i%),1%,3%) to cut%, data goto L02880
L02880:
            if cut% = 0% then goto L03160
               gosub check_cut                 /* Check New Family */
               if check% = 0% then goto L03160
               sa% = sa% + 1%
               sa_type$(sa%)  = "SA"

               sa_piece$(sa%) = "  " & cp$(i%)     /* No Pieces Needed */
               sa_cut$(sa%)   = ct$(i%)            /* Length of Cut    */
               str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
               str(sa_part$(sa%),11%,5%) = "     "
 
               str(sa_rack$(sa%),1%,4%)  = "@F A"      /* Bin Location */
               str(sa_rack$(sa%),5%,9%)  = ssq$& "-A" &ssq$& "/"
               str(sa_rack$(sa%),14%,2%) = cp$(i%)           /* Pieces */
               str(sa_rack$(sa%),16%,3%) = "   "
               str(sa_d1$(sa%),1%,2%)    = "0/"    /* No. of Labels    */
               str(sa_d1$(sa%),3%,12%)   = str(co$,1%,12%)
               str(sa_d1$(sa%),15%,2%)   = "/-"
               if dt_samp$ = "1" or dt_samp$ = "2" then                  ~
                                    str(sa_d1$(sa%),15%,2%) = "/S"
               if str(co$,1%,1%) = "9" then                              ~
                                    str(sa_d1$(sa%),15%,2%) = "/B"
                                                  /* Set Machine Codes */
               if i% <= cw% then str(sa_m$(sa%),1%,8%) = "H       "      ~
                            else str(sa_m$(sa%),1%,8%) = "J       "

L03160:      next i%
             sa_max% = sa%
        return

        build_descript
            init(" ") co$, x$, s$, readkey$, c_o$
            cnt% = 3%                            /* 1st Set Model Code */
            str(co$,1%,3%) = str(dtl_part$,1%,3%)
            s$ = str(dtl_part$,11%,1%)           /* Set Screen Code    */
            x$ = str(dtl_part$,12%,1%)           /* Set Lock Code      */
                                                 /* (PAR000)           */
            p% = pos(lk_fn$(1%) = x$)            /* 1 Lock Codes       */
            if p% = 0% then goto L03300
               str(co$,cnt%+1%,3%) = "/1K"
               goto L03330
                                                 /* (PAR000)           */
L03300:     p% = pos(lk_fn$(2%) = x$)            /* 2 Lock Codes       */
            if p% = 0% then goto L03340
               str(co$,cnt%+1%,3%) = "/2K"
L03330:     cnt% = cnt% + 3%
L03340:     p% = pos("456" = s$)
            if p% = 0% then goto L03400
               str(co$,cnt%+1%,3%) = "/TS"         /* Set as Default   */
               if s$ = "5" then str(co$,cnt%+1%,3%) = "/BS"
               if s$ = "6" then str(co$,cnt%+1%,3%) = "/FG"
               cnt% = cnt% + 3%
                                                   /* (PAR000)         */
L03400:     p% = pos(lk_fn$(3%) = x$)              /* With Fin Codes   */
            if p% = 0% then goto L03440
               str(co$,cnt%+1%,3%) = "/WF"         /* Set With Fin     */
               cnt% = cnt% + 3%
L03440:     str(readkey$,1%,9%)   = "HINGE    "    /* Check Cot/Oriel  */
            str(readkey$,10%,15%) = str(dtl_part$,9%,2%)
            read #2,key = readkey$, using L03470, desc$, eod goto L03630
L03470:        FMT POS(25), CH(32)
            p% = pos(desc$ = "-")
            if str(desc$,1%,2%) <> "CO" and str(desc$,1%,2%) <> "OR"     ~
                                        then goto L03560
               str(co$,cnt%+1%,3%) = "/CO"         /* Set as Default   */
               c_o$ = "CO"
               if str(desc$,1%,2%) = "OR" then str(co$,cnt%+1%,3%)="/OR"
               if str(desc$,1%,2%) = "OR" then c_o$ = "OR"
               cnt% = cnt% + 3%
L03560:     if p% = 0% then goto L03630
            if str(desc$, p%+2%, 4%) <> "TWIN" and                       ~
               str(desc$, p%+2%, 4%) <> "TRPL" then goto L03630
                  str(co$, cnt%+1%,3%) = "/TW"     /* Set as Default   */
                  if str(desc$,p%+2%,4%) = "TRPL" then                   ~
                     str(co$, cnt%+1%,3%) = "/TR"
               cnt% = cnt% + 3%
L03630:     if str(dtl_part$,9%,2%) <> "09" then goto L03670
               str(co$,cnt%+1%,3%) = "/33"                 /* 1/3,1/3  */
               cnt% = cnt% + 3%

L03670: return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "        The File ("&ff_nam$(kk%)&") Already Exists.   "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_cut
            check% = 0%
            init(" ") readkey$, set_up$, saw_no$, desc$, machine$
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = file_type$    /* 1 - S = Sash Saws  */
                                                 /* 2 - F = Frame Saws */
                                                 /* 3 - H = Header Saws*/
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
            if c_o$ = "CO" then str(readkey$,14%,1%) = "3"  /* COTTAGE */
            if c_o$ = "OR" then str(readkey$,14%,1%) = "4"  /* ORIEL   */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */
            read #2,key = readkey$, using L03910 , desc$, eod goto L03970
L03910:       FMT POS(25), CH(10)
               machine$ = str(desc$,1%,3%)           /* Machine Code   */
               set_up$  = str(desc$,5%,1%)           /* Set-Up Code    */
               saw_no$  = str(desc$,7%,2%)           /* Saw Number     */
               check%   = 1%                         /* Valid Equation */

L03970: return

        assign_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #2,hold,key = readkey$, using L04050 , sched$,            ~
                                                           eod goto L04130
L04050:       FMT POS(25), CH(3)
            convert sched$ to sched%, data goto L04130

            str(show_msg$,17%,3%) = sched$
            convert (sched% + 1%) to sched$, pic(000)
            if (sched% + 1%) > 999% then sched$ = "100"
            put #2, using L04050 , sched$
            rewrite #2
            call "SHOSTAT" (show_msg$) 
        return
L04130:    str(err_msg$,14%,27%) = "-Assigning Schedule No.??  "
           goto error_display

        error_display
           call "SHOSTAT" (err_msg$)
           stop
        return
 
        exit_program
          if xcount% > 0% then goto L04230
             call "SHOSTAT" ("N O   L I N E A L M A T E   D A T A")
             call "PAUSE" addr(300%)

L04230: end
