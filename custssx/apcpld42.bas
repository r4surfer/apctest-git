*       ****************************************************************~
*                      New -( W e g o m a   W e l d e r )              *~
*                  (PAR000) CR347  01/15/2006   Depts 006, 033         *~
*                                                                      *~
*                  ( As of 11/14/97 - RHH Checked for R6.04.03 )       *~
*                  ( As of 11/17/98 - RHH Change Array Sizes to 100%)  *~        
*                                                                      *~
*        APCPLD42 - Create a File for Wegoma Welder, Pass File to the  *~
*                   Welder Controler.                                  *~
*                                                                      *~
*            Note - 'APCWELD  ' - Table Contains only Valid Weld       *~
*                                 Models.                              *~
*                   'APCWELDFR' - Table Contains Valid Models for      *~
*                                 Frames.                              *~
*                   'APCWELDSH' - Table Contains Valid Models for      *~
*                                 Sash's.                              *~
*                                                                      *~
*                    ** Deduct 1/4 Inch from Width and Height, Also    *~
*                       Mod to Sort. Same Sequence as the Production   *~
*                       Consolidation Report. ( By Load )              *~
*            Note - Special Mod for Size change of DT_REF$8 and        *~
*                   DT_SEQ$5. Warranty Number and Daily sequence       *~
*                   Number.                                            *~
*                                                                      *~
*           05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
*       ****************************************************************

        sub "APCPLD42" (count%,          /* Number of Windows         */ ~
                        scr_dept$,       /* PRODUCTION DEPARTMENT     */ ~
                        scr_prod$,       /* PRODUCT LINE              */ ~
                        lk_fn$(),        /* 1,2 Lk Fin                */ ~
                        #1,              /* (APCCUTWK) Saw Work File  */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6 )             /* (AMTBOMIF) Validity File  */

        dim readkey$24,                  /* GENCODES Primary Key      */ ~
                                         /* (PAR000)                  */ ~
            lk_fn$(5%)30,                /* 1%=1Lock,2%=2Lock,3%=WFin */ ~
            mf$(100%)3, ms$(100%)3,      /* FRAMS AND SASH ONLY'S     */ ~
            opt1$1, opt2$1, opt3$1,      /* OPTIONS                   */ ~
            cl$2,                        /* Color                     */ ~
            model$3,                     /* Model Code                */ ~
            width$(2%)6,                 /* Width Size                */ ~
            height$(2%)6,                /* Height Size               */ ~
            size$6,                      /* SIZE                      */ ~
            weld_rec$45,                 /* WELD RECORD               */ ~
            mods$(100%)3,                /* VALID MODELS              */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            wrk_key$5,                   /* PRIMARY KEY               */ ~
            wrk_key1$51,                 /* WORK KEY                  */ ~
            wrk_rec$200,                 /* WORK RECORD               */ ~
            part$25,                     /* MFG Part Number           */ ~
            save_part$25,                /* SAVE MFG PART NUMBER      */ ~
            save_cl$1,                   /* SAVE COLOR                */ ~
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

        dim f2%(15%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
            name$(15%)8,                 /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

            select #7, "@WELDF1@",                                       ~
                                consec , recsize = 45

            select #8, "@WELDF2@",                                       ~
                                consec , recsize = 45

            select #9, "@WELDF3@",                                       ~
                                consec , recsize = 45

            select #10,"@WELDF4@",                                       ~
                                consec , recsize = 45

            select #12, "@WELDS1@",                                      ~
                                consec , recsize = 45

            select #13, "@WELDS2@",                                      ~
                                consec , recsize = 45

            select #14, "@WELDS3@",                                      ~
                                consec , recsize = 45

            select #15, "@WELDS4@",                                      ~
                                consec , recsize = 45

            ff% = 6%                     /* Starting Frame Channel */
            fs% = 11%                    /* Starting Sash Channel  */

            init(" ") name$()

            name$( 7%) = "@WELDF1@"
            name$( 8%) = "@WELDF2@"
            name$( 9%) = "@WELDF3@"
            name$(10%) = "@WELDF4@"
            name$(12%) = "@WELDS1@"
            name$(13%) = "@WELDS2@"
            name$(14%) = "@WELDS3@"
            name$(15%) = "@WELDS4@"

            gosub load_weld                  /* MODS$() - VALID MODELS */
            gosub load_frame                 /* MF$()   - FRAME        */
            gosub load_sash                  /* MS$()   - SASH         */

            msg$(1%)= "        The File (@WELD1@) Already Exists.       "

        REM - NOTE - MODLES (710), (720), (730) HAVE BOTH A FRAM AND SASH

            fcnt%,scnt%,count% = 0%
            call "SHOSTAT" ("Creating Weld File For (" & scr_dept$ & ")")
            cw%, ch% = 0%                     /* Load Cut Descriptions */
            tw$ = "1" : th$ = "2"

            call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
            if err% <> 0% then goto exit_program
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))
            if scr_prod$ <> "7" then goto L01330
                                                  /* Vinyl Prime       */
            read #1,key 1% > wrk_key1$, using L01390, wrk_key1$, wrk_rec$, ~
                                                     eod goto create_done
            goto L01400
                                                  /* Vinyl Double Hung */
L01330:     read #1,key > wrk_key$, using L01390, wrk_key1$, wrk_rec$,     ~
                                                     eod goto create_done
            goto L01400
        create_next
            read #1, using L01390   , wrk_key1$, wrk_rec$,                 ~
                                                     eod goto create_done
L01390:          FMT POS(6), CH(51), CH(200)
L01400:     part$  = str(wrk_rec$,38%,25%)
            model$ = str(part$,1%,3%)
            for i% = 1% to mod_max%
               if model$ = mods$(i%) then goto L01470
            next i%
            goto create_next

L01470:     if len(part$) < 19 then goto create_next
               gosub build_weld
               if width$(1) = " " and height$(1) = " " then              ~
                                                         goto create_next
                  dt_ref$ = str(wrk_rec$,1%,8%)
                  dt_seq$ = str(wrk_rec$,9%,5%)
                  ref$    = str(dt_ref$,4%,5%)
                  ssq$    = str(dt_seq$,3%,3%)
                  gosub lookup_color
                  gosub build_detail
                  goto create_next
        create_done
            if ff% <> 6%  then close #ff%
            if fs% <> 11% then close #fs%
        goto exit_program

        build_detail
          x$ = bin(34%,1) : y$ = ","           /* STUFF QUOTE INTO X$  */
          init(" ") weld_rec$
          gosub check_options

         for j% = 1% to weld%

          if weld% = 1% then goto L01730
             opt1$ = "0"                            /* SASH  - 2ND     */
             if j% = 1% then opt1$ = "1"            /* FRAME - 1ST     */
L01730:   gosub check_count
          str(weld_rec$,1%,1%)  = "K"               /* DESIGNATOR      */
          str(weld_rec$,2%,10%) = ssq$&"/"&ref$&" " /* SEQ / REF NO.   */
          str(weld_rec$,12%,1%) = "S"               /* SERIES          */
          str(weld_rec$,13%,3%) = model$            /* MODEL CODE      */
          str(weld_rec$,16%,1%) = opt1$             /* FRAME/SASH      */
          str(weld_rec$,17%,1%) = opt2$             /* NO FIN OR FIN   */
          str(weld_rec$,18%,1%) = opt3$             /* SINGLE OR DOUBLE*/
          str(weld_rec$,19%,1%) = "C"               /* COLOR           */
          str(weld_rec$,20%,2%) = cl$               /* COLOR CODE      */
          str(weld_rec$,22%,1%) = "B"               /* BIN LOC         */
          str(weld_rec$,23%,3%) = ssq$              /* SEQ NO OURS     */
          str(weld_rec$,26%,1%) = "Q"               /* QUANTITY DESIG  */
          str(weld_rec$,27%,3%) = "001"             /* QUANTITY        */
          str(weld_rec$,30%,1%) = "H"               /* HEIGHT          */
          str(weld_rec$,31%,6%) = height$(j%)       /* HEIGHT          */
          str(weld_rec$,37%,1%) = "W"               /* WIDTH           */
          str(weld_rec$,38%,6%) = width$(j%)        /* WIDTH           */
          str(weld_rec$,44%,1%) = "F"               /* PROCESS FLAG    */
          str(weld_rec$,45%,1%) = "0"               /* ZERO            */

          if opt1$ = "1" then                   /* Write Frame Record */ ~
             write #ff%, weld_rec$, eod goto L02020
          if opt1$ = "0" then                   /* Write Sash Record  */ ~
             write #fs%, weld_rec$, eod goto L02020

L01990: next j%

        return
L02020:   stop "(Error) - Writing WELD Record ??? "
          goto L01990

        build_weld
          if part$ = save_part$ then return
             save_part$ = part$

          init(" ") width$() , height$()
          w%, h%, weld% = 0%
          call "APCCUTCC" (part$, 0%, 0%, 0%, /* (CUT001) */             ~
                     0%, cw%, ch%, eq$(), ct$(), cr$(),                  ~
                     cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,        ~
                                                        #4, #5, #2, err%)
          eq% = cw% + ch%
          for i% = 1% to eq%
            if sh$(i%) = "W" or sh$(i%) = "H" then goto L02190
               goto L02340
                                                    /* DEDUCT 1/4 INCH */
L02190:     a  = ct(i%) - .25                       /* CONVERT SIZE    */
            a% = int(a)                             /* WHOLE SIZE      */
            b  = ( a - a% ) * 100.0
            b% = int(b)                                /* FRACTION SIZE*/
            convert a% to str(size$,1%,3%), pic(000)
            convert b% to str(size$,4%,2%), pic(00)
            str(size$,6%,1%) = "0"

            if sh$(i%) = "W" then w% = w% + 1%
            if a% < 12% and sh$(i%) = "W" then goto L02370
            if sh$(i%) = "H" then h% = h% + 1%
            if a% > 96% and sh$(i%) = "H" then goto L02370
            if sh$(i%) = "W" then width$(w%)  = size$
            if sh$(i%) = "H" then height$(h%) = size$

L02340:   next i%
          weld% = w%
        return
L02370:   init(" ") width$() , height$()        /* Max Limits Exceeded */
        return

        check_count                                /* Max Line Counter */
           if opt1$ <> "1" then  goto L02460
              fcnt% = fcnt% + 1%                   /* Frame Counter */
              if fcnt% = 1% then goto L02490            /* 1st Time      */
                 if mod(fcnt%,240) <> 0 then return
                    goto L02490
L02460:    scnt% = scnt% + 1%                         /* Sash Counter  */
           if scnt% = 1% then goto L02490               /* 1st Time      */
              if mod(scnt%,240) <> 0 then return
L02490:          gosub create_file
        return

        create_file
           if opt1$ <> "1" then goto L02590                /* Frame File  */
               fx% = ff%
               ff% = ff% + 1%                            /* Next File   */
               fy% = ff%
               if fcnt% = 1% then goto L02650
               goto L02630
L02590:    fx% = fs%                                     /* Sash File  */
           fs% = fs% + 1%                                /* Next File  */
           fy% = fs%
           if scnt% = 1% then goto L02650
L02630:    gosub create_file_a
        return
L02650:    gosub create_file_b                   /* 1st Time for Entry */
        return

        file_exists
            comp% = 2%
            hdr$ = "****** Weld File Exists ******"
        REM MSG$(1) = "        The File (@WELD@) Already Exists.       "
            msg$(2) = "             B u i l d   W E G O M A             "
            msg$(3) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        lookup_color
          if str(part$,4%,1%) = save_cl$ then return
             save_cl$ = str(part$,4%,1%)
          init(" ") readkey$
          str(readkey$,1%,15%)  = "COLOR    " & str(part$,4%,1%)
          read #2,key = readkey$, using L02830  , cl$, eod goto L02840
L02830:      FMT POS(25), CH(2)
L02840: return

        check_options
          opt1$ = "1"                                   /* FRAM ONLY   */
          for i% = 1% to mf_max%                        /* ONLY        */
             if model$ = mf$(i%) then goto L02960
          next i%
          opt1$ = "0"
          for i% = 1% to ms_max%                        /* SASH ONLY  */
             if model$ = ms$(i%) then goto L02960
          next i%

L02960:   opt2$ = "0"                              /* CHECK FIN OPTION */
          fn$ = str(part$,12%,1%)
                                                   /* (PAR000)         */
          p%  = pos(lk_fn$(3%) = fn$)              /* With Fin Codes   */
          if p% = 0% then goto L03010
             opt2$ = "1"

L03010:   opt3$ = "1"                                         /* COLOR */
          if cl$ = "WH" then opt3$ = "0"                      /* WHITE */
        return

        exit_program

        end

        create_file_a                       /* Close Old File First */
            close #fx%
        create_file_b                       /* Open New File        */
            call "SHOSTAT" ("Creating Weld File - "& name$(fy%) )
            call "OPENFILE" (#fy%,"IO   ", f2%(fy%), rslt$(fy%), axd$ )
            if f2%(fy%) <> 0% then goto L03250
               str(msg$(1%),19%,8%) = name$(fy%)
               gosub file_exists
               if comp% <> 16% then goto L03210
                  call "FILEBGON" addr(#fy%)
                  goto L03250

L03210:        close #fy%
               call "OPENFILE" (#fy%,"EXTND",f2%(fy%),rslt$(fy%), axd$ )
               goto L03300

L03250:     str(rslt$(fy%),1%,6%)  = "OUTPTP"
            str(rslt$(fy%),7%,8%)  = "00001000"
            str(rslt$(fy%),15%,3%) = "100"
            str(rslt$(fy%),18%,3%) = "100"
            call "OPENFILE" (#fy%, "OUTPT", f2%(fy%), rslt$(fy%), axd$ )
L03300: return

        load_weld
          mod_max% = 0%
          init(" ") readkey$, mods$()
          str(readkey$,1%,9%) = "APCWELD  "

L03370:   read #2,key > readkey$, using L03380, readkey$, eod goto L03430
L03380:      FMT CH(24)
          if str(readkey$,1%,9%) <> "APCWELD  " then goto L03430
             mod_max% = mod_max% + 1%
             if mod_max% > 100% then mod_max% = 100% 
          mods$(mod_max%) = str(readkey$,10%,3%)
          goto L03370
L03430: return

        load_frame
          mf_max% = 0%
          init(" ") readkey$, mf$()
          str(readkey$,1%,9%) = "APCWELDFR"

L03500:   read #2,key > readkey$, using L03510, readkey$, eod goto L03560
L03510:      FMT CH(24)
          if str(readkey$,1%,9%) <> "APCWELDFR" then goto L03560
             mf_max% = mf_max% + 1%
             if mf_max% > 100% then mf_max% = 100%
          mf$(mf_max%) = str(readkey$,10%,3%)
          goto L03500
L03560: return

        load_sash
          ms_max% = 0%
          init(" ") readkey$, ms$()
          str(readkey$,1%,9%) = "APCWELDSH"

L03630:   read #2,key > readkey$, using L03640, readkey$, eod goto L03690
L03640:      FMT CH(24)
          if str(readkey$,1%,9%) <> "APCWELDSH" then goto L03690
             ms_max% = ms_max% + 1%
             if ms_max% > 100% then ms_max% = 100%
          ms$(ms_max%) = str(readkey$,10%,3%)
          goto L03630
L03690: return

