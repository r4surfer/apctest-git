*       ****************************************************************~
*                      New -( S a m p s o n   W e l d e r )            *~
*                  (EWD004) 01/09/08             Dept (048)            *~    
*                  ( As of 12/16/05 - for Slider Department (048)      *~
*                         Special Change for Multiple Products/Models  *~
*                         in a Department 'scr_prod$                   *~ 
*                                                                      *~
*        APCPLT42 - Create a File for Sampson Sash Welder, Build       *~
*                   bridge file for Specific Products.                 *~
*                                                                      *~
*            Note - 'APCWELD  ' - Table Contains only Valid Weld       *~
*                                 Models.                              *~
*                   'APCSAMPSC' - Table Contains Valid Models for      *~
*                                 Sash Welder (C). Slider Department   *~
*                                                                      *~
*                    (Note)     - In Position (17,4) of the table      *~
*                                 Description is the Quantity,         *~
*                                 otherwise default = (1)              *~   
*                                                                      *~
*                                                                      *~
*                    ** Deduct 1/4 Inch from Width and Height, Also    *~
*                       Mod to Sort. Same Sequence as the Production   *~
*                       Consolidation Report. ( By Load )              *~
*                                                                      *~
*            Note - Special Mod for Size change of DT_REF$8 and        *~
*                   DT_SEQ$5. Warranty Number and Daily sequence       *~
*                   Number.                                            *~
*                                                                      *~
*----------------------------------------------------------------------*~
* 02/17/2003  !  (EWD001) - Mod to set up new 120 2SL & 130 3SL. ! CMG *~
* 10/27/2003  !  (EWD002) - Mod to set up new 126, 127, 136, and ! CMG *~
*             !             137 2SL and 3SL Brickmould Sliders   !     *~
* 12/16/2005  !  (EWD003) - Mod to set for new 128, 138 2SL and  ! RHH *~
*             !             3SL for New 450 Brick Mold           !     *~ 
* 01/09/2008  ! (AWD004) - Mod for new non-j bmdh sliders        ! CMG *~
* 07/21/2008  ! (AWD005) - Mod to take out tso, bso              ! CMG *~
* 05/19/2014  ! (CUT001) mod to add dim fields to CUTCC          ! CMG *~
*       ****************************************************************

        sub "APCPLT42" (count%,          /* Number of Windows         */ ~
                        scr_dept$,       /* Production Department     */ ~
                        scr_prod$,       /* Product Line              */ ~
                        lk_fn$(),        /* 1,2 Lk Fin                */ ~
                        #1,              /* (APCCUTWK) Saw Work File  */ ~
                        #2,              /* (GENCODES)                */ ~
                        #4,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #5,              /* (AMTBOMCD) Equation File  */ ~
                        #6 )             /* (AMTBOMIF) Validity File  */

        dim readkey$24,descr$30,         /* GENCODES Primary Key      */ ~
                                         /* (PAR000)                  */ ~
            lk_fn$(5%)30,                /* 1%=1Lock,2%=2Lock,3%=WFin */ ~
            mS$(100%)3, ms%(100%),       /* Sash Only's, Weld Quantity*/ ~
            cl$2,                        /* Color                     */ ~
            hnge$20, hh$8,               /* Hinge code and Descript   */ ~
            model$3, date$8,             /* Model Code                */ ~
            notes$14, title$50,          /* Detail Notes              */ ~
            size$3,                      /* Sizing System Number      */ ~
            tool$3,                      /* Tool Set-up Number        */ ~
            width$(2%)9,                 /* Width Size                */ ~
            height$(2%)9,                /* Height Size               */ ~
            weld_rec$71,                 /* Weld Record               */ ~
            mods$(100%)3,                /* Valid Models              */ ~
            scr_prod$1, scr_dept$3,      /* Product Line              */ ~
            sav_prod$1,                  /* (EWD001) - Save Product   */ ~
            wrk_key$5,                   /* Primary Key               */ ~
            wrk_key1$51,                 /* Work Key                  */ ~
            wrk_rec$200,                 /* Work Record               */ ~
            part$25,                     /* MFG Part Number           */ ~
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
            msg$(3%)79                   /* Askuser Info Text         */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
            name$(30%)8,                 /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

            select #7, "@SAMPD1@",                                       ~
                                consec , recsize = 71

            select #8, "@SAMPD2@",                                       ~
                                consec , recsize = 71

            select #9, "@SAMPD3@",                                       ~
                                consec , recsize = 71

            select #10,"@SAMPD4@",                                       ~
                                consec , recsize = 71

            select #11,"@SAMPD5@",                                       ~
                                consec , recsize = 71

            select #12,"@SAMPD6@",                                       ~
                                consec , recsize = 71

            select #13,"@SAMPD7@",                                       ~
                                consec , recsize = 71

            select #14,"@SAMPD8@",                                       ~
                                consec , recsize = 71

            select #15,"@SAMPD9@",                                       ~ 
                                consec , recsize = 71  

            select #16,"@SAMPD10@",                                       ~ 
                                consec , recsize = 71  

            select #17,"@SAMPD11@",                                       ~ 
                                consec , recsize = 71  

            select #18,"@SAMPD12@",                                       ~ 
                                consec , recsize = 71  

            select #19,"@SAMPD13@",                                       ~ 
                                consec , recsize = 71  

            select #20,"@SAMPD14@",                                       ~ 
                                consec , recsize = 71  

            select #21,"@SAMPD15@",                                       ~ 
                                consec , recsize = 71  

            select #22,"@SAMPD16@",                                       ~ 
                                consec , recsize = 71  

            select #23,"@SAMPD17@",                                       ~ 
                                consec , recsize = 71  

            select #24,"@SAMPD18@",                                       ~ 
                                consec , recsize = 71  

            select #25,"@SAMPD19@",                                       ~ 
                                consec , recsize = 71  

            ff% = 6%                     /* Starting Sash Channel (B) */

            init(" ") name$()

            name$( 7%) = "@SAMPD1@"
            name$( 8%) = "@SAMPD2@"
            name$( 9%) = "@SAMPD3@"
            name$(10%) = "@SAMPD4@"
            name$(11%) = "@SAMPD5@"
            name$(12%) = "@SAMPD6@"
            name$(13%) = "@SAMPD7@"
            name$(14%) = "@SAMPD8@"
            name$(15%) = "@SAMPD9@" 
            name$(16%) = "@SAMPD10@" 
            name$(17%) = "@SAMPD11@" 
            name$(18%) = "@SAMPD12@" 
            name$(19%) = "@SAMPD13@" 
            name$(20%) = "@SAMPD14@" 
            name$(21%) = "@SAMPD15@" 
            name$(22%) = "@SAMPD16@" 
            name$(23%) = "@SAMPD17@" 
            name$(24%) = "@SAMPD18@" 
            name$(25%) = "@SAMPD19@" 

            sav_prod$ = " "                  /* (EWD001) - Don't Mix   */
    
            gosub load_weld                  /* MODS$() - VALID MODELS */
            gosub load_sash_c                /* MS$()   - Sash File (C)*/
                                             /* MS%()   - Weld Quantity*/

            msg$(1%)= "       The File (@SAMPD1@) Already Exists.       "

            count%, scnt% = 0%
            call "SHOSTAT" ("Creating Sampson Weld File ("&scr_dept$&")")
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))
                                        /* All Welded Sash Double Hung */
            read #1,key > wrk_key$, using L01360, wrk_key1$, wrk_rec$,     ~
                                                     eod goto create_done
            goto L01370
        create_next
            read #1, using L01360   , wrk_key1$, wrk_rec$,                 ~
                                                     eod goto create_done
L01360:          FMT POS(6), CH(51), CH(200)
L01370:     part$  = str(wrk_rec$,38%,25%)
            model$ = str(part$,1%,3%)
            for i% = 1% to mod_max%
               if model$ = mods$(i%) then goto L01430
            next i%
            goto create_next
L01430:     gosub check_options
            if opt1% = 99% then goto create_next

            if len(part$) < 19 then goto create_next

/* (AWD005) next two stmts */

            if str(part$,11,1) = "4" or str(part$,11,1) = "5"  ~
                                     then goto create_next
            if str(part$,11,1) = "6" then goto create_next

               cw%, ch% = 0%                     /* Load Cut Descriptions */
               tw$ = "1" : th$ = "2"
               scr_prod$ = str(model$,1%,1%)
               if sav_prod$ = " " then sav_prod$ = scr_prod$
                                               /* (EWD001) - Save Product */
               call "APCCUTLD" (scr_prod$, cw%, ch%, tw$, th$, #2, err% )
               if err% <> 0% then goto exit_program

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
            if ff% <> 6%  then close #ff%
        goto exit_program

        build_detail
          init(" ") weld_rec$
          x% = 2%                                   /* Sash is always  */
          if weld% = 1% then x% = 1%                /* 2nd Value, Excep*/
          if ms_qty% < 1% then ms_qty% = 1%

          for kk% = 1% to ms_qty%                   /* Default = 1     */
                                                    /* Quantity from   */
                                                    /* (ACSAMPSC) Table*/
           for j% = x% to weld%
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

                                                    /* Sash (C) File   */
             write #ff%, weld_rec$, eod goto L01900

L01870:    next j%
          next kk%
        return
L01900:   call "SHOSTAT" ("(Error)-Writing Sampson Weld Detail?")
          stop
          goto L01870

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

            if sh$(i%) = "F" or sh$(i%) = "S" then goto L02080
               goto L02200
                                                  /* Deduct 1/4 Inch   */
L02080:     a  = ct(i%) - .25                     /* From Decimal Size */
            if i% <= cw% then sh$(i%) = "W"       /* Set Width         */ 
            if i% > cw% then sh$(i%) = "H"        /* Set Height        */ 

            if sh$(i%) = "W" then w% = w% + 1%      /* Min Width (12") */
            if a < 12.0 and sh$(i%) = "W" then goto L02230
            if sh$(i%) = "H" then h% = h% + 1%      /* Max Height (96")*/
            if a > 96.0 and sh$(i%) = "H" then goto L02230
            if sh$(i%) <> "W" then goto L02180
               convert a to width$(w%), pic(###.####)

               goto L02200
L02180:     convert a to height$(h%), pic(###.####) /* Must be Height  */

L02200:   next i%
          weld% = w%
        return
L02230:   init(" ") width$() , height$()        /* Max Limits Exceeded */
        return

        check_count                                /* Max Line Counter */
          if scnt% <> 0% then return
          scnt% = scnt% + 1%                       /* Sash (A) Counter */
          if scnt% = 1% then goto L02350           /* 1st Time         */
                                                   /* (EWD001) - Check */
             if scr_prod$ = sav_prod$ then goto L02340
                scnt% = 240% 
                sav_prod$ = scr_prod$

L02340:      if mod(scnt%,240) <> 0 then return    /* Batch Size 240   */
L02350:          gosub create_file                 /* Creat File and   */
        return                                     /* Build Header     */

        create_file
           fx% = ff%                               /* Sash (C) File FF%*/
           ff% = ff% + 1%                          /* Next File        */
           fy% = ff%
           if scnt% = 1% then goto L02510
              gosub create_file_a                /* Close Old File 1st */
        return
L02510:    gosub create_file_b                   /* 1st Time for Entry */
        return

        file_exists
            comp% = 2%
            hdr$ = "***** Sampson Weld File ******"
        REM MSG$(1) = "        The File (@SAMPD1@) Already Exists.     "
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
          init(" ") tool$, size$
          opt1% = 99%
          for i% = 1% to ms_max%                        /* Valid Models*/
             if model$ = ms$(i%) then goto L02820
          next i%
        return
L02820:   opt1% = 0%                                    /* Sash (C)    */
          tool$ = "007"                     /* 122,123,124,132,133,134 */
          if str(model$,1%,1%) = "4" then tool$ = "001"
          if str(model$,1%,1%) = "7" then tool$ = "002"
          if str(model$,1%,1%) = "6" then tool$ = "003"
          if model$ = "121" or model$ = "131" then tool$ = "006"
                                                        /*  (EWD001)   */
          if model$ = "120" or model$ = "130" then tool$ = "006"

                                                        /*  (EWD002)   */
          if model$ = "126" then tool$ = "006"
          if model$ = "127" then tool$ = "006"
                                                        /*  (EWD002)   */
          if model$ = "136" then tool$ = "006"
          if model$ = "137" then tool$ = "006"
                                                        /*  (EWD003)   */
          if model$ = "128" then tool$ = "008"
          if model$ = "138" then tool$ = "008"
                                                        /*  (EWD030)   */   

/* (AWD004) */
          if model$ = "B32" then tool$ = "008"
          if model$ = "B33" then tool$ = "008"
/* (AWD004) */


          size$ = "999"                     /* 122,123,124,132,133,134 */
          if str(model$,1%,1%) = "4" then size$ = "999"
          if str(model$,1%,1%) = "7" then size$ = "999"
          if str(model$,1%,1%) = "6" then size$ = "600"
          if model$ = "121" or model$ = "131" then size$ = "999"
                                                        /*  (EWD001)   */
          if model$ = "120" or model$ = "130" then size$ = "999"
                                                        /*  (EWD002)   */
          if model$ = "126" then size$ = "999"
          if model$ = "127" then size$ = "999"
                                                        /*  (EWD002)   */
          if model$ = "136" then size$ = "999"
          if model$ = "137" then size$ = "999"
                                                        /*  (EWD003)   */
          if model$ = "128" then size$ = "999"
          if model$ = "138" then size$ = "999"
                                                        /*  (EWD003)   */
                                                        /*  (AWD004)   */
          if model$ = "B32" then size$ = "999"
          if model$ = "B33" then size$ = "999"
                                                        /*  (AWD004)   */
            ms_qty% = ms%(i%)
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
            title$ = "Slider Dept Sampson Welder (D) for ( "&date$&" )"
            if opt1% = 1% then str(title$,29%,1%) = "B"
            str(weld_rec$,1%,6%)  = "940920"    /* File Version Number */
            str(weld_rec$,7%,3%)  = "071"       /* Record Length       */
            str(weld_rec$,10%,62%)= " "         /* Padding             */
                                                /* Sash (C) File       */
               write #ff%, weld_rec$, eod goto L03410

            init(" ") weld_rec$
            str(weld_rec$,1%,50%) = title$      /* Weld Batch Title    */
            str(weld_rec$,51%,21%)= " "         /* Padding             */

                                                /* Sash (C) File       */
               write #ff%, weld_rec$, eod goto L03410

        return
L03410:   call "SHOSTAT" ("(Error)-Writing Sampson (C) Header?")
          stop
          return

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

        load_sash_c
          ms_max% = 0%
          mat ms% = zer
          init(" ") readkey$, ms$(), descr$
          str(readkey$,1%,9%) = "APCSAMPSC"

L03780:   read #2,key > readkey$, using L03790, readkey$, descr$,       ~
                                                       eod goto L03830
L03790:      FMT CH(24), CH(30)
          if str(readkey$,1%,9%) <> "APCSAMPSC" then goto L03830
             ms_max% = ms_max% + 1%
             if ms_max% > 100% then ms_max% = 100% 
          ms$(ms_max%) = str(readkey$,10%,3%)
          ms_qty% = 1%
          convert str(descr$,17%,4%) to ms_qty%, data goto L03800
L03800:
           ms%(ms_max%) = ms_qty%
          goto L03780
L03830: return

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

