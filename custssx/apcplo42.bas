*       ****************************************************************~
*          New Family - - -  ( L i n e a l m a t e )   Dept (08)       *~
*                                                                      *~
*                  (PAR000) 01/15/06 - CR348     RHH Checked Rev: 01.00*~
*        APCPLO42 - Create File with Data for Saw Optimization         *~
*                   (Same as 'APCPLJ42' with the same Revision No.)    *~
*                                                                      *~
*            Note(1)Primary Subroutine 'BUILD_SAW_RECS'                *~
*                   for the Casement Department Sash's.                *~
*                   Department = '008'                                 *~
*                                                                      *~
*            Note(2)Special Mod to handle the change for DT_REF$8 and  *~
*                   DT_SEQ$5. Future need to change File Layout to     *~
*                   handle size changes.                               *~
*                                                                      *~
*            Note(3)Modification made to support new Bilco machines    *~
*                   Added fields 14, 15, 16 and 17. For Machine codes, *~
*                   Saw Number, and Saw Set-Up Number.                 *~
*                                                                      *~
*            Note(4)When changes are made to the following Tables, the *~
*                   subroutine will need to be modified.               *~
*                   BUILD_DESCRIPT uses (SCREEN,LOCK,HINGE) Tables     *~
*                                                                      *~
*            Note(5)Force CP$() which is the number of pieces to cut   *~
*                   to one (1), use SET_UP$ to set the number of tracks*~
*                   to either (2) or (4).                              *~
*                                                                      *~
*            Note(6)Adjust Schedule by (100%) Subtract from Schedule   *~
*                   for both Departments 049 and 052.                  *~
*                                                                      *~
*            Note(7)Mod to By-Pass all Sash Windows, Screen Codes      *~
*                   4, 5, and 6. (Not sent to Linealmate)              *~
*                                                                      *~
*            Note(8)Mod to Add new Subroutine CHECK_CUT to Check for   *~
*                   Sash and Frame Cuts.                               *~
*                                                                      *~
* (EWD001) - Mod to turn off forcing qty to one.          07/20/03 CMG *~
* (AWD002) - Mod to make size always 25                   11/09/04 CMG *~
* (AWD003) - mod to add F for Foam option               06/19/2012 CMG *~
* (CUT001) - mod to add dim fields to CUTCC             06/03/2014 MES *~
*       ****************************************************************

        sub "APCPLO42" (size%,           /* Batch Size (No. Windows)  */ ~
                        sched%,          /* Starting Schedule Number  */ ~
                        scr_dte$,        /* Production Date Formatted */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        scr_load$,       /* Production Load           */ ~
                        lk_fn$(),        /* 1,2 Lock with Fin (PAR000)*/ ~
                        #1,              /* (APCCUTWK) Saw Work File  */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6 )             /* (AMTBOMIF) Validity File  */

        dim readkey$24,                  /* GENCODES Primary Key      */ ~
            desc$32,                     /* GENCODES Description      */ ~
                                         /* (PAR000)                  */ ~
            lk_fn$(5%)30,                /* 1%=1Lock,2%=2Lock,3%=WFin */ ~
            sched$3,                     /* Schedule Numbers          */ ~
            tsched$3,                    /* Starting Schedule Number  */ ~
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
            wrk_key1$51,                 /* WORK KEY                  */ ~
            wrk_rec$200,                 /* WORK RECORD               */ ~
            seq$3,                       /* Record Number Key         */ ~
            dtl_load$5,                  /* Load Number               */ ~
            dtl_part$25,                 /* MFG Part Number           */ ~
            dtl_subpart$20,              /* (AWD003) subpart          */ ~            
            save_part$25,                /* MFG Part Number           */ ~
            ref_no$5, dt_ref$8,          /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            dt_samp$1,                   /* 0=NA, 1=SAMP, 2=DISP      */ ~
            col$(100%)25,                /* Cut Description           */ ~
            eq$(100%)8,                  /* Equation Codes and Number */ ~
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
            sa_s$(100%)1,                /* SAW SET-UP NUMBER          */~
            machine$3, set_up$1,         /* Store Window Type code    */ ~
            saw_no$2,                    /* Machine Saw Number        */ ~
            ff_nam$7%,                   /* Bridge File Name          */ ~
            file$30,                     /* Schedule Title Name       */ ~
            inc$10, s$1, x$2, y$1,       /* Schedule File Identifier  */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79                  /* Error Message Text        */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SAW05@",                                       ~
                                consec , recsize = 149

            ff% = 3% : ff_nam$ = "@SAW05@"                 /* DEPT 008 */

            size% = 0% 
            cw%, ch% = 0%
            size1% = 25%                                   /* (AWD002)  */
            init(" ") rslt$(), axd$
                                                 /* Create Saw Batches */
            call "OPENFILE" (#ff%, "IO   ", f2%(ff%), rslt$(ff%), axd$ )
            if f2%(ff%) <> 0% then goto L01280
               gosub file_exists
               if comp% <> 16% then goto L01240
                  call "FILEBGON" addr(#ff%)
                  goto L01280

L01240:        close #ff%
            call "OPENFILE" (#ff%, "EXTND", f2%(ff%), rslt$(ff%), axd$ )
               goto L01340

L01280:     str(rslt$(ff%),1%,6%)  = "OUTPTP"
            str(rslt$(ff%),7%,8%)  = "00001000"
            str(rslt$(ff%),15%,3%) = "100"
            str(rslt$(ff%),18%,3%) = "100"
            call "OPENFILE" (#ff%, "OUTPT", f2%(ff%), rslt$(ff%), axd$ )

L01340:     bat_no$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

        REM - Adhust Schedule
            hit% = 0%                        /* Set to (1) When Header */
            bat_no% = 1% : count% = 0% : save_part$ = " "  /* is Built */
            convert sched% to tsched$, pic(###)

            call "SHOSTAT" ("Starting Schedule ("&tsched$&") for Dept ("&~
                                                           scr_dept$&")")
            tw$ = "1" : th$ = "2"             /* Load Cut Descriptions */
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            if err% <> 0% then goto exit_program
            xcount% = 0%
            wrk_key1$ = all(hex(00))
                                             /* Casement Windows       */
            read #1,key > wrk_key1$, using L01550 , wrk_key1$, wrk_rec$,   ~
                                                     eod goto create_done
            goto L01560
        create_next
            read #1, using L01550 , wrk_key1$, wrk_rec$,                   ~
                                                     eod goto create_done
L01550:          FMT POS(6), CH(51), CH(200)
L01560:     dt_ref$   = str(wrk_rec$,1%,8%)
            dt_seq$   = str(wrk_rec$,9%,5%)
            dtl_load$ = str(wrk_rec$,29,5%)
            dtl_part$ = str(wrk_rec$,38%,25%)
            dtl_subpart$ = str(wrk_rec$,81%,20%)                  /*(AWD003)*/            
            dt_samp$  = str(wrk_rec$,77%,1%)
            ref_no$   = str(dt_ref$,4%,5%)
            ssq$      = str(dt_seq$,3%,3%)
            s$        = str(dtl_part$,11%,1%)    /* Set Screen Code    */
            p% = pos("456" = s$)
            if p% <> 0% then goto create_next
            if len(dtl_part$) < 19 then goto create_next
            if count% = 0% then gosub build_schedule   /* CREATE BATCH */
               gosub build_saw_recs
               if sa_max% = 0% then goto create_next
                  seq% = 0%
                  for sa% = 1% to sa_max%
                      gosub build_detail
                  next sa%
                  count% = count% + 1%
                  if count% < size1%  then goto create_next   /* (AWD002) */
                     gosub build_end
                     count% = 0% : hit% = 0%
                     goto create_next
        create_done
            gosub build_end
            close #ff%
            convert (bat_no% - 1%) to bat$, pic(##)
        goto exit_program

        build_schedule                         /* SET-UP SCHEDULE REV. */
          if hit% = 1% then return             /* Schedule has not been*/
          init(" ") bat_rec$                   /* Completed Yet.       */
          inc$ = " (" & str(bat_no$,bat_no%,1%) & ")"
          file$ = "("&scr_dept$&")   Saw for " &scr_dte$& inc$
          if str(scr_load$,1%,1%) <> "N" then str(file$,6%,2%) = "/U"
          str(bat_rec$,1%,2%)   = "FR"                  /* FILE REV.   */
          str(bat_rec$,3%,6%)   = "930318"              /* REVISION NO */
          str(bat_rec$,9%,141%) = " "                   /* LINE FEED   */
          write #ff%, bat_rec$, eod goto L02000

          gosub build_schedule_no
          gosub build_schedule_title
          hit% = 1%
        return
L02000:   call "SHOSTAT" ("(Error) - Writing Revision ????") : stop
        return

        build_schedule_no
          init(" ") bat_rec$
          gosub assign_schedule
          convert sched% to sched$, pic(000)

          str(bat_rec$,1%,2%) = "SN"                /* SCHEDULE NUMBER */
          str(bat_rec$,3%,3%) = sched$              /* SCHED (100-999) */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02130
        return
L02130:   call "SHOSTAT" ("(Error) - Writing Schedule Number ??? ")
          stop
        return

        build_schedule_title                        /* SCHEDULE TITLE  */
          init(" ") bat_rec$
          str(bat_rec$,1%,2%)   = "TI"              /* SCHEDULE TITLE  */
          str(bat_rec$,3%,30%)  = file$             /* TITLE DESCRIPT  */
          str(bat_rec$,33%,117%)= " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02240
        return
L02240:   call "SHOSTAT" ("(Error) - Writing Schedule Title ??? ")
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
          str(bat_rec$,121%,1%) = " "                   /* Line Feed   */
          str(bat_rec$,122%,2%) = "  "                  /* No. Labels  */
          str(bat_rec$,124%,15%)= "               "     /* License Plat*/
          str(bat_rec$,139%,8%) = sa_m$(sa%)            /* Machine Code*/
          str(bat_rec$,147%,2%) = saw_no$               /* Saw Number  */
          str(bat_rec$,149%,1%) = sa_s$(sa%)            /* Saw Set-up  */
                                                        /*  Number     */
          write #ff%, bat_rec$, eod goto L02550
        return
L02550:   call "SHOSTAT" ("(Error) - Writing Schedule Detail ??? ")
          stop
        return

        build_end
          if count% = 0% then return
          init(" ") bat_rec$
          str(bat_rec$,1%,2%) = "**"                /* END OF BATCH    */
          str(bat_rec$,3%,3%) = "END"               /* LINE FEED       */
          str(bat_rec$,6%,144%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L02680
          bat_no% = bat_no% + 1%
        return
L02680:   call "SHOSTAT" (errormsg$) : stop
        return

        build_saw_recs
          if save_part$ <> dtl_part$ then goto L02780
             for i% = 1% to sa_max%
                 str(sa_rack$(i%),5%,9%)  = ssq$& "-A" &ssq$& "/"
             next i%
             return

L02780:   save_part$ = dtl_part$
          init(" ") sa_type$(), sa_piece$(), sa_cut$(), sa_part$(),      ~
                    sa_rack$(), sa_d1$(), sa_d2$, sa_m$(), sa_s$()
          call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, #6,  ~
                                                                    err%)
          sa_d2$ = str(apc_prt$,1%,30%)

          sa% = 0%
          call "APCCUTCC" (dtl_part$,  0%, 0%, 0%, /* (CUT001) */         ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                   ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,         ~
                                                        #4, #5, #2, err%)
          gosub build_descript
          eq% = cw% + ch%
          for i% = 1% to eq%
        REM *RHH*                               /* REMOVE 'LA' RECORDS */
            if cc$(i%) = "N" or cc$(i%) = " " then goto L03360

            cut% = 0%                    /* Skip - No Equation Records */
            convert str(ct$(i%),1%,3%) to cut%, data goto L02970
L02970:
            if cut% = 0% then goto L03360
               gosub check_cut
               if check% = 0% then goto L03360          /* Skip Equation */

               sa% = sa% + 1%
               sa_type$(sa%)  = "SA"
               if cc$(i%) = "N" then sa_type$(sa%) = "LA"
        REM - Special Mod for Saws                    /*  (EWD001)      */
REM               cp$(i%) = "01"                      /* Force Quantity   */
                                                   /* of One (1)       */
               sa_piece$(sa%) = "  " & cp$(i%)     /* No Pieces Needed */
               sa_cut$(sa%)   = ct$(i%)            /* Length of Cut    */
               if set_up$ <> "1" then goto L03170
                  ct(i%) = ct(i%) + .8125          /* ADD      13/16   */
                  init(" ") sa_cut$(sa%)
                  convert ct(i%) to str(sa_cut$(sa%),1%,7%), pic(###.###)



L03170:        str(sa_part$(sa%),1%,10%) = cr$(i%) /* Raw Material Part*/
               str(sa_part$(sa%),11%,5%) = "     "
               if str(dtl_subpart$,5%,1%) = "3" then  /* (AWD003) */    ~
                                           str(sa_part$(sa%),11%,1%) = "F"
               if str(dtl_subpart$,5%,1%) = "4" then  /* (AWD003) */    ~
                                           str(sa_part$(sa%),11%,1%) = "F"
                                              
               str(sa_rack$(sa%),1%,4%)  = "@F A"      /* Bin Location */
               str(sa_rack$(sa%),5%,9%)  = ssq$& "-A" &ssq$& "/"
               str(sa_rack$(sa%),14%,2%) = cp$(i%)           /* Pieces */
               str(sa_rack$(sa%),16%,3%) = "   "
               str(sa_d1$(sa%),1%,2%)    = "0/"    /* No. of Labels    */
        REM    STR(SA_D1$(SA%),3%,12%)   = STR(COL$(I%),1%,12%)
               str(sa_d1$(sa%),3%,12%)   = str(co$,1%,12%)
               str(sa_d1$(sa%),15%,2%)   = "/-"
        REM    IF DT_SAMP$ = "1" OR DT_SAMP$ = "2" THEN                  ~
        REM                         STR(SA_D1$(SA%),15%,2%) = "/S"
        REM    IF STR(CO$,1%,1%) = "9" THEN                              ~
        REM                         STR(SA_D1$(SA%),15%,2%) = "/B"
               if set_up$ = "1" then str(sa_d1$(sa%),15%,2%) = "4"       ~
                                else str(sa_d1$(sa%),15%,2%) = "2"

               sa_m$(sa%) = machine$ & "     "
               sa_s$(sa%) = set_up$
L03360:      next i%
             sa_max% = sa%
        return

        build_descript
            init(" ") co$, x$, s$, readkey$, c_o$, y$
            cnt% = 3%                            /* 1st Set Model Code */
            str(co$,1%,3%) = str(dtl_part$,1%,3%)
            s$ = str(dtl_part$,11%,1%)           /* Set Screen Code    */
            y$ = str(dtl_part$,12%,1%)           /* Lock Code          */
            x$ = str(dtl_part$,9%,2%)            /* Set hinge Code     */

            str(co$,cnt%+1%,3%) = "/" & x$       /* Use the Actual Hinge*/
            cnt% = cnt% + 3%                     /* Code in Descript   */
            p% = pos("456" = s$)
            if p% = 0% then goto L03600
               str(co$,cnt%+1%,3%) = "/TS"         /* Set as Default   */
               if s$ = "5" then str(co$,cnt%+1%,3%) = "/BS"
               if s$ = "6" then str(co$,cnt%+1%,3%) = "/FG"
               cnt% = cnt% + 3%
                                                   /* (PAR000)         */
L03600:     p% = pos(lk_fn$(3%) = y$)              /* With Fin Codes   */
            if p% = 0% then goto L03640
               str(co$,cnt%+1%,3%) = "/WF"         /* Set With Fin     */
               cnt% = cnt% + 3%
L03640:     str(readkey$,1%,9%)   = "HINGE    "    /* Check Cot/Oriel  */
            str(readkey$,10%,15%) = str(dtl_part$,9%,2%)
            read #2,key = readkey$, using L03670, desc$, eod goto L03830
L03670:        FMT POS(25), CH(32)
            p% = pos(desc$ = "-")
            if str(desc$,1%,2%) <> "CO" and str(desc$,1%,2%) <> "OR"     ~
                                        then goto L03760
               str(co$,cnt%+1%,3%) = "/CO"         /* Set as Default   */
               c_o$ = "CO"
               if str(desc$,1%,2%) = "OR" then str(co$,cnt%+1%,3%)="/OR"
               if str(desc$,1%,2%) = "OR" then c_o$ = "OR"
               cnt% = cnt% + 3%
L03760:     if p% = 0% then goto L03830
            if str(desc$, p%+2%, 4%) <> "TWIN" and                       ~
               str(desc$, p%+2%, 4%) <> "TRPL" then goto L03830
                  str(co$, cnt%+1%,3%) = "/TW"     /* Set as Default   */
                  if str(desc$,p%+2%,4%) = "TRPL" then                   ~
                     str(co$, cnt%+1%,3%) = "/TR"
               cnt% = cnt% + 3%
L03830:     if str(dtl_part$,9%,2%) <> "09" then goto L03870
               str(co$,cnt%+1%,3%) = "/33"                 /* 1/3,1/3  */
               cnt% = cnt% + 3%

L03870: return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%)= "        The File ("&ff_nam$&") Already Exists.   "
            msg$(2%)= "             O P T I M I Z A T I O N             "
            msg$(3%)= "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_cut
            check% = 0%
            init(" ") readkey$, machine$, set_up$, saw_no$, desc$
            str(readkey$,1%,9%)  = "NEWFAMILY"
            str(readkey$,10%,1%) = "S"               /* F = Frame Saws */
                                                     /* S = Sash Saws  */
            str(readkey$,11%,3%) = str(dtl_part$,1%,3%)
            str(readkey$,14%,1%) = "W"               /* W = Width Cut  */
            if c_o$ = "CO" then str(readkey$,14%,1%) = "3" /* COTTAGE  */
            if c_o$ = "OR" then str(readkey$,14%,1%) = "4" /* ORIEL    */
            if i% > cw% then str(readkey$,14%,1%) = "H"/* H=Height Cut */
            str(readkey$,15%,2%) = str(eq$(i%),7%,2%)  /* Equation No. */

            read #2,key = readkey$, using L04110 , desc$, eod goto L04170
L04110:       FMT POS(25), CH(10)
               machine$ = str(desc$,1%,3%)           /* Machine Code   */
               set_up$  = str(desc$,5%,1%)           /* Set-Up Code    */
               saw_no$  = str(desc$,7%,2%)           /* Saw Number     */
               check%   = 1%                         /* Valid Equation */

L04170: return

        assign_schedule
            init(" ") readkey$, sched$
            str(readkey$,1%,9%)   = "PLANSCHED"
            str(readkey$,10%,15%) = "LINEALMA"
            read #2,hold,key = readkey$, using L04250 , sched$,            ~
                                                           eod goto L04330
L04250:       FMT POS(25), CH(3)
            convert sched$ to sched%, data goto L04330

            convert (sched% + 1%) to sched$, pic(000)
            if (sched% + 1%) > 999% then sched$ = "100"
            put #2, using L04250 , sched$
            rewrite #2
        return
L04330:    call "SHOSTAT" ("Error- Assigning Schedule Number?")
           stop
        return


        exit_program
          if xcount% > 0% then goto L04430
             call "SHOSTAT" ("N O   L I N E A L M A T E   D A T A")
             call "PAUSE" addr(300%)

L04430: end
