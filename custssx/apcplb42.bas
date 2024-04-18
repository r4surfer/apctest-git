*       ****************************************************************~
*                      NEW -( S a s h   B u i l d )                    *~
*                   (PAR000)  01/15/2006 CR347  Dept 006,033,048       *~
*                                                                      *~
*        (240 WINDOW LIMIT) ( As of 03/02/98 - RHH Check for R6.04.03  *~
*                   Note - Valid Sash Machine Models in (PLAN SASH)    *~
*                                                                      *~ 
*        APCPLB42 - Create a File for Building Sash's, Pass File to    *~
*                   to Sash Controler.                                 *~
*                                                                      *~
*                   Note (1)- Special Sort for Sash Machine, Uses the  *~
*                             same Sort Sequence as Production         *~
*                             Consolidation Report. By Load - Key '0'  *~
*                                                                      *~
*                   Note (2)- Special Code for Top/Bot Sash only       *~
*                             Do not pass Min (10.00) inches and       *~
*                                         Max (60.00) inches.          *~
*                                         Width and Height             *~
*                                                                      *~
*         ----->    Note (3)- Special Correction for DT_REF$8 and the  *~
*                             Sequence No. DT_SEQ$5. Future need to    *~
*                             Change the Sash Record.                  *~
*                                                                      *~
*           05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
*       ****************************************************************

        sub "APCPLB42" (count%,          /* Number of Sash's          */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Production Line (0 thru 6)*/ ~
                        lk_fn$(),        /* 1,2 Lk Fin                */ ~
                        #1,              /* (APCCUTWK) Saw Work File  */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6 )             /* (AMTBOMIF) Validity File  */

        dim readkey$24,                  /* GENCODES Primary Key      */ ~
                                         /* (PAR000)                  */ ~
            lk_fn$(5%)30,                /* 1%=1Lock,2%=2Lock,3%=WFin */ ~
            desc$32,                     /* GENCODES Description      */ ~
            cl$2,                        /* Color                     */ ~
            gl$2,                        /* Glass Type                */ ~
            model$3,                     /* Model Code                */ ~
            locks$3,                     /* Lock Code                 */ ~
            lit$7,                       /* Liting Code               */ ~
            width$4,                     /* Width Size                */ ~
            height$4,                    /* Height Size               */ ~
            b_width$4,                   /* Width Size                */ ~
            b_height$4,                  /* Height Size               */ ~
            sash_rec$80,                 /* SASH RECORD               */ ~
            scr_prod$1,                  /* Product Line              */ ~
            scr_dept$3,                  /* Department Code           */ ~
            wrk_key$5,                   /* PRIMARY KEY               */ ~
            wrk_key1$51,                 /* WORK KEY                  */ ~
            wrk_rec$200,                 /* WORK RECORD               */ ~
            load$5,                      /* Load Number               */ ~
            part$25,                     /* MFG Part Number           */ ~
            save_part$25,                /* SAVE MFG PART NUMBER      */ ~
            save_cl$1,                   /* SAVE COLOR                */ ~
            save_gl$2,                   /* SAVE GLASS                */ ~
            save_lt$2,                   /* SAVE LITING               */ ~
            save_lk$1,                   /* SAVE LOCKS                */ ~
            ref$5, dt_ref$8,             /* Part Reference Number     */ ~
            ssq$3, dt_seq$5,             /* Daily Sequence Number     */ ~
            x$1,                         /* DELEIMETER                */ ~
            y$1,                         /* DELEIMETER                */ ~
            tw$1,                        /* WIDTH PARTS               */ ~
            th$1,                        /* HEIGHT PARTS              */ ~
            col$(100%)25,                /* Cut Description           */ ~
            eq$(100%)8,                  /* Equation Codes            */ ~
            ct$(100%)9,                  /* Cut Widths and Heights    */ ~
            ct(100%),                    /* Cut Wid/Height Decimal    */ ~
            cr$(100%)10,                 /* Raw Material Part Number  */ ~
            cp$(100%)2,                  /* Number of Pieces to Cut   */ ~
            cc$(100%)1,                  /* Cut Piece Yes or No       */ ~
            sh$(100%)1,                  /* Sash Type ( W, H, N )     */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79                   /* ASKUSER Info Text         */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

                                               /* SET-UP SAW FILE(S)   */
            select #3,  "@SASH@",                                        ~
                                consec , recsize = 80

            select #7,  "@SASH1@",                                       ~
                                consec , recsize = 80

            select #8,  "@SASH2@",                                       ~
                                consec , recsize = 80

            select #9,  "@SASH3@",                                       ~
                                consec , recsize = 80

            count% = 0%
                                                 /* Create Sash File   */
            call "OPENFILE" (#3, "IO   ", f2%(3%), rslt$(3%), axd$ )
            if f2%(3%) <> 0% then goto L01040
               gosub file_exists
               if comp% <> 16% then goto L01000
                  call "FILEBGON" addr(#3)
                  goto L01040

L01000:        close #3
               call "OPENFILE" (#3, "EXTND", f2%(3%), rslt$(3%), axd$ )
               goto L01100

L01040:     str(rslt$(3%),1%,6%)  = "OUTPTP"
            str(rslt$(3%),7%,8%)  = "00001000"
            str(rslt$(3%),15%,3%) = "100"
            str(rslt$(3%),18%,3%) = "100"
            call "OPENFILE" (#3, "OUTPT", f2%(3%), rslt$(3%), axd$ )

L01100:    call "SHOSTAT" ("Creating Sash File For Dept ("&scr_dept$&")")

            ff% = 3%                         /* SET FOR @SASH@ CHANNEL */
            cw%, ch% = 0%                     /* LOAD CUT DESCRIPTIONS */
            tw$ = "1" : th$ = "2"
            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            if err% <> 0% then goto exit_program
            init(" ") save_part$, save_cl$, save_gl$, save_lt$, save_lk$
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))
            read #1,key > wrk_key$, using L01260 , wrk_key1$, wrk_rec$,    ~
                                                     eod goto create_done
            goto L01270
        create_next
            read #1, using L01260 , wrk_key1$, wrk_rec$,                   ~
                                                     eod goto create_done
L01260:          FMT POS(6), CH(51), CH(200)
L01270:     part$  = str(wrk_rec$,38%,25%)
            if len(part$) < 19 then goto create_next

               model$ = str(part$,1%,3%)
               gosub valid_sash              /* Check for Valid Models */
               if valid% = 0% then goto create_next
               gosub build_sash
               if width$ = " " and height$ = " " then goto create_next
                  gosub check_sash
                  if check% <> 0% then goto create_next
                     dt_ref$ = str(wrk_rec$,1%,8%)
                     dt_seq$ = str(wrk_rec$,9%,5%)
                     load$   = str(wrk_rec$,29%,5%)
                     ref$ = str(dt_ref$,4%,5%)
                     ssq$ = str(dt_seq$,3%,3%)
                     gosub lookup_color
                     gosub lookup_glass
                     gosub lookup_liting
                     gosub lookup_locks
                     gosub build_detail
                     count% = count% + 1%
                     if mod(count%,240) <> 0 then goto create_next
                        if count% = 240 then gosub create_file_1
                        if count% = 480 then gosub create_file_2
                        if count% = 720 then gosub create_file_3

                     goto create_next
        create_done
            close #ff%
        goto exit_program

        build_detail
          init(" ") sash_rec$
          x$ = bin(34%,1) : y$ = ","           /* STUFF QUOTE INTO X$  */
          str(sash_rec$,1%,4%)  = ssq$ & y$             /* SEQUENCE NO */
          str(sash_rec$,5%,8%)  = x$ & ref$ & x$ & y$   /* REF NO.     */
          str(sash_rec$,13%,8%) = x$ & load$ & x$ & y$  /* LOAD NO.    */
          str(sash_rec$,21%,6%) = x$ & model$ & x$ & y$ /* MODEL CODE  */
          str(sash_rec$,27%,5%) = x$ & cl$ & x$ & y$    /* COLOR       */
          str(sash_rec$,32%,6%) = x$ & locks$ & x$ & y$ /* LOCKS       */
          str(sash_rec$,38%,5%) = x$ & gl$ & x$ & y$    /* GLASS TYPE  */
          str(sash_rec$,43%,6%) = x$ & str(lit$,1%,5%)  /* LITING      */
          str(sash_rec$,49%,4%) = "TP" & x$ & y$        /* TOP         */
          str(sash_rec$,53%,2%) = "1" & y$              /* QUANTITY    */
          str(sash_rec$,55%,5%) = width$ & y$           /* WIDTH       */
          str(sash_rec$,60%,4%) = height$               /* HEIGHT      */

          if sash% = 2% then goto L01770           /* BSO - ONLY         */
          write #ff%, sash_rec$, eod goto L01820   /* CREATE TWO RECORDS */
                                                 /* FOR EACH SASH      */
L01770:   gosub adjust_sash

          if sash% = 1% then goto L01810           /* TSO - ONLY         */
          write #ff%, sash_rec$, eod goto L01820
L01810: return
L01820:   stop "(Error) - Writing Sash Record ??? "
        return

        adjust_sash
          str(sash_rec$,55%,4%) = b_width$              /* WIDTH       */
          str(sash_rec$,60%,4%) = b_height$             /* HEIGHT      */
          str(sash_rec$,49%,2%) = "BT"                  /* BOTTOM      */
        return

        build_sash
          if part$ = save_part$ then return
             save_part$ = part$

          init(" ") width$, height$, b_width$, b_height$
          call "APCCUTCC" (part$, 0%, 0%, 0%, /* (CUT001) */            ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                 ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,       ~
                                                        #4, #5, #2, err%)
          eq% = cw% + ch%
          for i% = 1% to eq%
            if sh$(i%) = "W" or sh$(i%) = "H" then goto L02030
               goto L02290
L02030:     a  = ct(i%)                                /* CONVERT SIZE */
            a% = int(a)                                /* WHOLE SIZE   */
            b  = ( a - a% ) * 100.0
            b% = int(b)                                /* FRACTION SIZE*/
            convert a% to str(size$,1%,2%), pic(00)
            convert b% to str(size$,3%,2%), pic(00)

            if sh$(i%) = "W" then width$  = size$
            if sh$(i%) = "H" then height$ = size$

            if sh$(i%) <> "W" then goto L02190   /* SUB 1/2 INCH FR WIDTH*/
               a = ct(i%) - .5
               if model$ <> "620" and model$ <> "625" and model$ <> "670"~
                                      then goto L02190
                  a = a + .25                  /* SLIDERS ADD 1/4 INCH */

L02190:     if sh$(i%) = "H" then a = ct(i%) + .125    /* ADD 1/8 INCH */
            a% = int(a)                                /* WHOLE SIZE   */
            b  = ( a - a% ) * 100.0
            b% = int(b)                                /* FRACTION SIZE*/
            convert a% to str(size$,1%,2%), pic(00)
            convert b% to str(size$,3%,2%), pic(00)

            if sh$(i%) = "W" then b_width$  = size$
            if sh$(i%) = "H" then b_height$ = size$

L02290:   next i%
        return

        file_exists
            comp% = 2%
            hdr$ = "****** Sash File Exists ******"
            msg$(1) = "        The File (@SASH@ ) Already Exists.      "
            msg$(2) = "             B u i l d   S a s h ' s             "
            msg$(3) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        valid_sash                            /* Check Table for Valid */
          valid% = 0%                         /* Models to be Produced */
          init(" ") readkey$                  /* on the 'Sash Machine' */
          str(readkey$,1%,9%)   = "PLAN SASH"
          str(readkey$,10%,15%) = model$
          read #2,key = readkey$, eod goto L02480
             valid% = 1%
L02480: return

        lookup_color
          if str(part$,4%,1%) = save_cl$ then return
             save_cl$ = str(part$,4%,1%)
          init(" ") readkey$
          str(readkey$,1%,15%)  = "COLOR    " & str(part$,4%,1%)
          read #2,key = readkey$, using L02560, cl$, eod goto L02570
L02560:      FMT POS(25), CH(2)
L02570: return

        lookup_glass
          if str(part$,5%,2%) = save_gl$ then return
             save_gl$ = str(part$,5%,2%)
          init(" ") readkey$
          str(readkey$,1%,15%)  = "GLASS    " & str(part$,5%,2%)
          read #2,key = readkey$, using L02650, gl$, eod goto L02660
L02650:      FMT POS(25), CH(2)
L02660: return

        lookup_liting
          if str(part$,7%,2%) = save_lt$ then return
             save_lt$ = str(part$,7%,2%)
          init(" ") readkey$
          str(readkey$,1%,15%)  = "LITING   " & str(part$,7%,2%)
          read #2,key = readkey$, using L02740, desc$, eod goto L02770
L02740:      FMT POS(25), CH(32)
          p% = pos(desc$ = "-")
          lit$ = str(desc$,1%,p%-2%)
L02770: return

        lookup_locks
                                                        /* (PAR000)           */
          if str(part$,12%,1%) = save_lk$ then return
             save_lk$ = str(part$,12%,1%)
             locks$ = "0LK"

             p% = pos(lk_fn$(1%) = save_lk$)            /* 1 Lock Codes       */
             if p% <> 0% then locks$ = "1LK"

             p% = pos(lk_fn$(2%) = save_lk$)            /* 2 Lock Codes       */
             if p% <> 0% then locks$ = "2LK"
                                                        /* (PAR000)           */
        return

        check_sash
           a%, b% = 0%
           check% = 1%
           convert width$ to a%, data goto L02940
L02940:
           convert height$ to b%, data goto L02960
L02960:
           if a% < 1000% or a% > 6000% then return
           if b% < 1000% or b% > 6000% then return
           check% = 0%

           sash% = 0%
           if str(wrk_rec$,20%,3%) = "TSO" then sash% = 1%
           if str(wrk_rec$,20%,3%) = "BSO" then sash% = 2%
        return

        exit_program

        end

        create_file_1
            close #ff%
            call "SHOSTAT" ("Creating a Second File @SASH1@")
            ff% = 7%                       /* SET FOR - @SASH1@ CHANNEL */
            call "OPENFILE" (#7, "IO   ", f2%(7%), rslt$(7%), axd$ )
            if f2%(7%) <> 0% then goto L03260
               str(msg$(1%),19%,7%) = "@SASH1@"
               gosub file_exists
               if comp% <> 16% then goto L03220
                  call "FILEBGON" addr(#7)
                  goto L03260

L03220:        close #7
               call "OPENFILE" (#7, "EXTND", f2%(7%), rslt$(7%), axd$ )
               goto L03310

L03260:     str(rslt$(7%),1%,6%)  = "OUTPTP"
            str(rslt$(7%),7%,8%)  = "00001000"
            str(rslt$(7%),15%,3%) = "100"
            str(rslt$(7%),18%,3%) = "100"
            call "OPENFILE" (#7, "OUTPT", f2%(7%), rslt$(7%), axd$ )
L03310: return

        create_file_2
            close #ff%
            call "SHOSTAT" ("Creating a Third File @SASH2@")
            ff% = 8%                       /* SET FOR - @SASH2@ CHANNEL */
            call "OPENFILE" (#8, "IO   ", f2%(8%), rslt$(8%), axd$ )
            if f2%(8%) <> 0% then goto L03490
               str(msg$(1%),19%,7%) = "@SASH2@"
               gosub file_exists
               if comp% <> 16% then goto L03450
                  call "FILEBGON" addr(#8)
                  goto L03490

L03450:        close #8
               call "OPENFILE" (#8, "EXTND", f2%(8%), rslt$(8%), axd$ )
               goto L03540

L03490:     str(rslt$(8%),1%,6%)  = "OUTPTP"
            str(rslt$(8%),7%,8%)  = "00001000"
            str(rslt$(8%),15%,3%) = "100"
            str(rslt$(8%),18%,3%) = "100"
            call "OPENFILE" (#8, "OUTPT", f2%(8%), rslt$(8%), axd$ )
L03540: return

        create_file_3
            close #ff%
            call "SHOSTAT" ("Creating a Fourth File @SASH3@")
            ff% = 9%                       /* SET FOR - @SASH3@ CHANNEL */
            call "OPENFILE" (#9, "IO   ", f2%(9%), rslt$(9%), axd$ )
            if f2%(9%) <> 0% then goto L03720
               str(msg$(1%),19%,7%) = "@SASH3@"
               gosub file_exists
               if comp% <> 16% then goto L03680
                  call "FILEBGON" addr(#9)
                  goto L03720

L03680:        close #9
               call "OPENFILE" (#9, "EXTND", f2%(9%), rslt$(9%), axd$ )
               goto L03770

L03720:     str(rslt$(9%),1%,6%)  = "OUTPTP"
            str(rslt$(9%),7%,8%)  = "00001000"
            str(rslt$(9%),15%,3%) = "100"
            str(rslt$(9%),18%,3%) = "100"
            call "OPENFILE" (#9, "OUTPT", f2%(9%), rslt$(9%), axd$ )
L03770: return

