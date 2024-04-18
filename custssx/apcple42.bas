*       ****************************************************************~
*                       NEW -( R e p o r t s )                         *~
*                                                                      *~
*                  ( As of 04/24/2012 - CMG Checked for R6.04.03 )     *~
*        APCPLE42 - Cut Sheet Report(s).                               *~
*                     300 Series - Vinyl Patio and Hinged Patio        *~
*            Note - Primary Subroutine 'CALC_CUTS' at Line ( 3040 )    *~
*                   SPEC% = 0% - Vinyl Patio Door                      *~
*                   SPEC% = 1% - Hinged Patio Door                     *~
*                   SPEC% = 2% - Special Products                      *~
*                   SPEC% = 3% - Department 211                        *~
*                   SPEC% = 4% - Department 411                        *~
*                   SPEC% = 5% - 500 Cont Head          Dept-51(EWD038)*~
*                   SPEC% = 6% - 450 Cont Brick Mold DH Dept-18(EWD038)*~ 
*                   SPEC% = 7% - 8900 Line              Dept-14(AWD006)*~
* 03/28/98 ! Y2K                                                LDJ    *~
* 11/07/02 ! (EWD001) Modification for Title and Shostat      ! CMG    *~
*          !   message    for new 500 cont head/sill          !        *~
* 12/16/05 ! (EWD002) Mod for New Dept 18 450 Cont Brick Mold ! RHH    *~
*          !          Double Hung and Fix header for Dept 51  !        *~  
* 09/25/2007 (AWD003) mod for CLMR on report                  ! CMG    *~
*03/23/2010! (AWD004) mods for patio door report              ! CMG    *~
*03/30/2011! (AWD005) mods for sash limiter on report         ! CMG    *~
*04/24/2012! (AWD006) mods for new 8900 line                  ! CMG    *~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC          ! CMG    *~
*10/09/2017| (CR1157) mod to add Coastal HDW for Patio        ! MES    *~
*04/13/2018! (CR1453) decrease line counts for current printers!RDB    *~
*       ****************************************************************

        sub "APCPLE42" (scr_dte$,        /* Production Date           */ ~
                        prod_dte$,       /* Completion Date           */ ~
                        spec%,           /* 0%=Patio,1%=Hinged,2%=Spec*/ ~
                        #1,              /* (APCCUTWK) Cut Sheet Work */ ~
                        #3,              /* (GENCODES) Table File     */ ~
                        #4,              /* (AMTBOMIF) Validity File  */ ~
                        #5,              /* (HNYMASTR) Part Master    */ ~
                        #6,              /* (TXTFILE ) Text File      */ ~
                        #7,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #8 )             /* (AMTBOMCD) Equation File  */

        dim scr_dte$8, userid$3,         /* Production Date            */~
            prod_dte$8,                  /* Completion Date            */~
            wrk_key1$51,                 /* Cut Sheet Work Key         */~
            wrk_key$51,                  /* Cut Sheet Work Key         */~
            wrk_rec$200,                 /* Cut Sheet Work Record      */~
            readkey$50,                  /* Code Table Key             */~
            descr$32,                    /* Code Table Description     */~
            scr_msg$40,                  /* Header Text                */~
            sav_mod$3,                   /* Save Model Code            */~
            sav_cl$1,                    /* Save Color Code            */~
            sav_part$25,                 /* Save Part Number           */~
            sav_load$5,                  /* Save Load Number           */~
            model$3,                     /* Model Code                 */~
            cl$1,                        /* Color Code                 */~
            color$6,                     /* Color Description          */~
            dt_ref$8,                    /* Product Warranty Number    */~
            dt_seq$5,                    /* Production Sequence Number */~
            c_o$2,                       /* Cottage/Oriel              */~
            t_t$4,                       /* Twin/Triple                */~
            locks$3,                     /* Number of Locks            */~
            fin$3,                       /* With or Without Fin        */~
            dtl_load$5,                  /* Load Number                */~
            dtl_txt$4,                   /* Product Text Id            */~
            dtl_part$25,                 /* Part Number                */~
            xw$7,                        /* Converted Window Width     */~
            xh$7,                        /* Converted Window Height    */~
            tw$1,                        /* WIDTH CUT PARTS            */~
            th$1,                        /* HEIGHT CUT PARTS           */~
            col$(100%)25,                /* Cut Descriptions           */~
            eq$(100%)8,                  /* PRIMARY CROSS REF KEYS     */~
            ct$(100%)9,                  /* WIDTH AND HEIGHT CUTS      */~
            ct(100%),                    /* WIDTH AND HEIGHT CUTS-DECIM*/~
            sh$(100%)1,                  /* SASH TYPE ( W, H, N )      */~
            cr$(100%)10,                 /* RAW MATERIAL PARTS         */~
            cp$(100%)2,                  /* NUMBER OF PIECES TO CUT    */~
            cc$(100%)1,                  /* CUT PIECE (Y) OR (N)       */~
            c1$(100%)19,                 /* Cut Descriptions for Report*/~
            c2$(100%)9                   /* Cuts Associated            */

        dim sav_key1$11,                 /* Save Text Key              */~
            text_key$11,                 /* Text Key                   */~
            text$(2%)70,                 /* Text Description           */~
            text_desc$60, text_d$(2%)60, /* Text Description           */~
            textid$4,                    /* Text Id                    */~
            text_flag$1                  /* Text Flag                  */


        dim clmr$3,                      /* (AWD003) CLMR              */~
            hg$2,                        /* (AWD003) HINGE             */~
            hinge$10,                    /* (AWD004) Hinge Desc        */~
            sz$100,                      /* (AWD003) Size              */~
            calc$9,                      /* (AWD003) fraction size     */~
            gl$2,                        /* (AWD004) glass code        */~
            glass$10                     /* (AWD004) Glass desc        */
            
        dim tripane$10,                  /* (AWD006)                   */~
            cstl$1,                      /* (CR1157) Hardware          */~
            coastal$22                   /* (CR1157) HDW Desc          */
        
            
        dim                                                              ~
            sub_part$20                  /* Subpart (AWD005)          */



            call "EXTRACT" addr("ID", userid$)
            pass% = 0%                   /* 1st Pass for Width         */
                                         /* 2nd Pass for Height        */

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "                /* (AWD003)         */

            if spec% = 0% then                                           ~
                   scr_msg$ = " Vinyl Patio Door Cut Sheet For Width   "
            if spec% = 1% then                                           ~
                   scr_msg$ = "  Hinged Patio Door Cut Sheet For Width "
            if spec% = 2% then                                           ~
                   scr_msg$ = "  Special Products Cut Sheet For Width  "
            if spec% = 3% then                                           ~
                   scr_msg$ = "  Vinyl Department (211) Cut Sheet Width"
            if spec% = 4% then                                           ~
                   scr_msg$ = "  Vinyl Department (411) Cut Sheet Width"
                                                        /* (AWD002)     */
            if spec% = 5% then                                           ~
                   scr_msg$ = "500 Continuous Head Cut Sheet For Width "
            if spec% = 6% then                                           ~
                   scr_msg$ = "450 Cont Brick Mold Cut Sheet For Width "
/*(AWD006)*/                   
            if spec% = 7% then                                           ~
                   scr_msg$ = "8900 DH/SLDR Cut Sheet For Width "                   
                                                        /* (AWD002)     */
/* (EWD001)  */

            if spec% = 0% then                                           ~
                   call "SHOSTAT" ("Print Vinyl Patio Door (Width)")
            if spec% = 1% then                                           ~
                   call "SHOSTAT" ("Print Hinged Patio Door (Width)")
            if spec% = 2% then                                           ~
                   call "SHOSTAT" ("Special Products (Width)")
            if spec% = 3% then                                           ~
                   call "SHOSTAT" ("Department ( 211 )(Width)")
            if spec% = 4% then                                           ~
                   call "SHOSTAT" ("Department ( 411 )(Width)")
                                                        /* (AWD002)     */
            if spec% = 5% then                                           ~
                   call "SHOSTAT" ("500 Continuous Head(Width)")
            if spec% = 6% then                                           ~
                   call "SHOSTAT" ("450 Cont Brick Mold(Width)")
/*(AWD006)*/       
            if spec% = 7% then                                           ~
                   scr_msg$ = "8900 DH/SLDR Cut Sheet For Width "                   
                                                        /* (AWD002)     */
/* (EWD001)  */


L00880:     pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)
            init(" ") sav_cl$, wrk_rec$, sav_mod$, sav_part$, sav_load$
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))
            read #1,key > wrk_key$, using L01060 , wrk_key1$, wrk_rec$,    ~
                                                    eod goto print_a_done
            sav_mod$   = str(wrk_key1$,6%,3%)
            cl$        = str(wrk_key1$,9%,1%)

            gosub lookup_color
            gosub lookup_model
            sav_load$ = str(wrk_rec$,29%,5%)
            goto L01070
        print_a_next
            read #1, using L01060 , wrk_key1$, wrk_rec$,                   ~
                                                    eod goto print_a_done
L01060:       FMT POS(6), CH(51), CH(200)
L01070:     model$    = str(wrk_key1$,6%,3%)        /* MODEL       */
            cl$       = str(wrk_key1$,9%,1%)        /* COLOR       */

            dt_ref$= str(wrk_rec$,1%,8%)            /* REF NO      */
            dt_seq$= str(wrk_rec$,9%,5%)            /* SEQUENCE NO */
            c_o$   = str(wrk_rec$,14%,2%)           /* COTTAGE/ORIE*/
            t_t$   = str(wrk_rec$,16%,4%)           /* TWIN/TRIPLE */
            t_b$   = str(wrk_rec$,20%,3%)           /* TSO, BSO,FSO*/
            locks$ = str(wrk_rec$,23%,3%)           /* LOCKS       */
            fin$   = str(wrk_rec$,26%,3%)           /* WITH FIN    */
            dtl_load$ = str(wrk_rec$,29%,5%)        /* LOAD NUMBER */
            dtl_txt$  = str(wrk_rec$,34%,4%)        /* TEXT ID     */
            dtl_part$ = str(wrk_rec$,38%,25%)       /* PART NO.    */

            clmr$ = str(dtl_part$,20,3)             /* CLMR (AWD003) */
            hg$   = str(dtl_part$,9,2)              /* Hinge (AWD003)*/
            gl$   = str(dtl_part$,5,2)              /* Glass(AWD004)*/
            sub_part$ = str(wrk_rec$,81%,20%) /* Subpart (AWD005)   */
            cstl$ = str(sub_part$,4,1)             /* (CR1157)     */
            gosub lookupHinge                       /* (AWD004)     */
            gosub lookupGlass                       /* (AWD004)     */
            gosub lookup3PGlass                     /* (AWD006)     */
            gosub lookupHardware                    /* (CR1157)     */


            xw$ = str(wrk_rec$,63%,7%)              /* WIDHT       */
            xh$ = str(wrk_rec$,70%,7%)              /* HEIGHT      */
            if cl$ <> sav_cl$ then gosub lookup_color
            if sav_mod$ <> model$ then gosub lookup_model
            if len(dtl_part$) > 18 then goto L01280
               xw$, xh$ = "PART    "
               init(" ") c_o$, t_t$, t_b$, locks$, fin$

L01280:     total% = total% + 1%
            gosub detail_a
            goto print_a_next
        print_a_done
            print using L02590
            call "SETPRNT" ("APCC", " ", 0%, 1%)
            pass% = pass% + 1%
            if pass% > 1% then goto L01510
               if spec% = 0% then                                        ~
                   scr_msg$ = " Vinyl Patio Door Cut Sheet For Height  "
               if spec% = 1% then                                        ~
                   scr_msg$ = " Hinged Patio Door Cut Sheet For Height "
               if spec% = 2% then                                        ~
                   scr_msg$ = "  Special Products Cut Sheet For Height "
               if spec% = 3% then                                        ~
                   scr_msg$ = "Vinyl Department (211) Cut Sheet Height "
               if spec% = 4% then                                        ~
                   scr_msg$ = "Vinyl Department (411) Cut Sheet Height "
                                                        /* (AWD002)     */
               if spec% = 5% then                                        ~
                   scr_msg$ = "500 Continuous Head Cut Sheet For Height"
               if spec% = 6% then                                        ~
                   scr_msg$ = "450 Cont Brick Mold Cut Sheet For Height"
                                                        /* (AWD002) */
/*(AWD006)*/
               if spec% = 7% then                                        ~
                   scr_msg$ = "8900 DH/SLDR Cut Sheet For Height"   
                                                        /* (EWD001) */

               if spec% = 0% then                                        ~
                   call "SHOSTAT" ("Print Vinyl Patio Door (Height)")
               if spec% = 1% then                                        ~
                   call "SHOSTAT" ("Print Hinged Patio Door (Height)")
               if spec% = 2% then                                        ~
                   call "SHOSTAT" ("Special Products (Height)")
               if spec% = 3% then                                        ~
                   call "SHOSTAT" ("Department (211) Products (Height)")
               if spec% = 4% then                                        ~
                   call "SHOSTAT" ("Department (411) Products (Height)")
                                                        /* (AWD002)     */
               if spec% = 5% then                                        ~
                   call "SHOSTAT" ("Print 500 Continuous Head (Height)")
               if spec% = 6% then                                        ~
                   call "SHOSTAT" ("Print 450 Cont Brick Mold (Height)")
                                                        /* (AWD002)     */
/*(AWD006)*/
               if spec% = 7% then                                        ~
                   call "SHOSTAT" ("Print 8900 DH/SLDR (Height)")                                                        
/* (EWD001) */

               goto L00880
L01510: return clear all
        goto exit_end

        lookup_model                                  /* Look Up Color */
            readkey$ = all(hex(00))
            readkey$ = "MODEL    " & sav_mod$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
        return

        lookup_color                                  /* Look Up Color */
            sav_cl$ = cl$
            readkey$ = all(hex(00))
            readkey$ = "COLOR    " & cl$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            color$ = str(descr$,6%,6%)
            if x% = 0% then color$ = "N/A"
        return

/*(AWD004) */
        lookupHinge                                   /* Look Up Hinge */
            readkey$ = all(hex(00))
            readkey$ = "HINGE    " & hg$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            hinge$ = str(descr$,1%,10%)
            if x% = 0% then hinge$ = "N/A"
        return

        lookupGlass                                   /* Look Up Glass */
            readkey$ = all(hex(00))
            readkey$ = "GLASS    " & gl$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            glass$ = str(descr$,1%,10%)
            if x% = 0% then glass$ = "N/A"
        return
/* (AWD004\) */


/*(AWD006) */
        lookup3PGlass                                 /* Look Up Triple Glass */
            init(" ") tripane$
            readkey$ = all(hex(00))
            readkey$ = "PLANTRIPL" & gl$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            tripane$ = str(descr$,1%,10%)
REM         if x% = 0% then tripane$ = "N/A"
        return
            
/* (AWD006\) */

/*(CR1157) */
        lookupHardware
            init(" ") coastal$
            readkey$ = all(hex(00))
            readkey$ = "HARDWARE " & cstl$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            if str(descr$,1%,7%) = "Coastal" then coastal$ = str(descr$,1%,22%)
            
         if x% = 0% then tripane$ = "N/A"
        return 

/* (CR1157\) */

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        lookup_text                           /* Look Up Text Id       */
            if textid$ = dtl_txt$ then return
            init(" ") text_desc$, textid$, text_key$, sav_key1$, text$(),~
                      text_d$()
            textid$ = dtl_txt$
            text_flag$ = "N"
            gosub'099(textid$)
            if txt% = 0% then return
            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$ = text_key$
            read #6,key > text_key$, eod goto L01990
               get #6, using L01920 , text_key$,text$()
L01920:          FMT CH(11), POS(64), 2*CH(70)
            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then return
            if text$(1%) <> " " then text_desc$ = str(text$(1%),1%,60%)  ~
                                else text_desc$ = str(text$(2%),1%,60%)
            if text_desc$ <> " " then text_flag$ = "Y"
            text_d$(1%) = str(text$(1%),1%,60%)
            text_d$(2%) = str(text$(2%),1%,60%)
L01990: return

L02010: %! No Calculation for: #########################                 ~
        ~                                                                !

L02040: %!Sq(#####)Model:### Color: ###### WR######## Width: ######## Hei~
        ~ght: ####### Co/Or: ## Twin/Triple: #### T/B: ### Lks:### Fn:###!

        %!####################:(########) ####################:(########)~
        ~  ####################:(########) ####################:(########)~


L02045: %!Hg:##########      Gl:##########   #########                   ~
        ~  Cstl:######################                      Load: #####  !

L02100:   FMT CH(1),CH(19),CH(2),CH(9),CH(2),CH(19),CH(2),CH(9),CH(2),   ~
              CH(19),CH(2),CH(9),CH(2),CH(19),CH(2),CH(9),CH(2)

L02130: %!TXT:###########################################################~
        ~#                                                               ! 

L02135: %!SCLMR:  ( ######### )                                          ~
        ~                                                                !
/* (AWD005) */
L02140: %!Sash Accessory : ###############                               ~
        ~                                                                !

L02160: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

        detail_a                                       /* C1$(), C2$() */
             if lcnt% > 55% then gosub header          /* CR1453 */
             if spec% = 0% then goto L02250  /* (AWD004) only break at lcnt% */
             if sav_load$ = dtl_load$ then goto L02250
                sav_load$ = dtl_load$
                gosub header

L02250:      print using L02160
             print using L02040, dt_seq$, model$, color$, dt_ref$, xw$,    ~
                               xh$, c_o$, t_t$, t_b$, locks$, fin$
             lcnt% = lcnt% + 2%
REM             if spec% <> 0% then goto notPatio           /* (AWD004) */
/* (AWD006) */
             if spec% <> 0% and spec% <> 7% then goto notPatio       
/* (AWD006) */             
                if tripane$ <> " " then               ~
                     print using L02045, hinge$, glass$, "TRIPLE"
                if tripane$ = " " then               ~
                     print using L02045, hinge$, glass$, " ", coastal$                     
                lcnt% = lcnt% + 1%
  
notPatio:
                                                        /* (AWD004\) */
             if xw$ = "PART    " then goto L02480
             if sav_part$ = dtl_part$ then goto L02340
                gosub calc_cuts
                sav_part$ = dtl_part$

L02340:      if j% <> 0% then goto L02380
                print using L02010, dtl_part$         /* No Calculations */
                lcnt% = lcnt% + 1%
                goto L02480
L02380:      i% = 1%

L02400:          print using L02100,  "!", c1$(i%), ":(", c2$(i%), ") ",   ~
                                 c1$(i% + 1%), ":(", c2$(i% + 1%), ") ", ~
                                 c1$(i% + 2%), ":(", c2$(i% + 2%), ") ", ~
                                 c1$(i% + 3%), ":(", c2$(i% + 3%), ")!"
                 lcnt% = lcnt% + 1%
             i% = i% + 4%
             if i% > j% then goto L02480
                goto L02400
L02480:      if pass% <> 1% then goto L02490
                if hg$ < "70" or hg$ > "97" then goto L02490
                if clmr$ = "000" then goto L02490
                    convert clmr$ to clmr, data goto L02490
                    clmr = clmr / 10.0
                    a% = int(clmr) : b = (clmr - a%) / 0.8
                    clmr = a% + b
                    calc = clmr

                    gosub con_fract
                    print using L02135, calc$
 
L02490:      if str(sub_part$,16,1) = "0" then goto L02500
               if str(sub_part$,16,1) = "1" then print using L02140, "Night Latch"
               if str(sub_part$,16,1) = "2" then print using L02140, "Sash Limiter"
                
L02500:      gosub lookup_text
             if text_flag$ <> "Y" then return
                if text_d$(1%) <> " " then                               ~
                   print using L02130, text_d$(1%)
                if text_d$(2%) <> " " then                               ~
                   print using L02130, text_d$(2%)
                lcnt% = lcnt% + 1%
                if text_d$(1%) <> " " and text_d$(2%) <> " " then        ~
                   lcnt% = lcnt% + 1%           /* Add Additional Line */
        return

L02590: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L02620: %!               Production Date: ##########                     ~
        ~                        Completion Date: ##########             !
                                                                                    /* (Y2K, LDJ) */
L02650: %! Load No.: #####                        #######################~
        ~#################                                    Page: #### !
L02655: %!                                        #######################~
        ~#################                                    Page: #### !

        %! New Version - (###)                                           ~
        ~                                                                !

        header
          if lcnt% <> 99% then print using L02590
          pageno% = pageno% + 1%
          print page
          print using L02590
          print using L02620, scr_dte$, prod_dte$
/*(AWD004) */
          if spec% = 0% then print using L02655, scr_msg$, pageno% ~
             else print using L02650, sav_load$, scr_msg$, pageno%
          lcnt% = 3%
          sav_load$ = dtl_load$
        return

        calc_cuts
          cw%, ch% = 0%
          tw$ = "1" : th$ = "2"
          pd$ = str(dtl_part$,1%,1%)
          call "APCCUTLD" (pd$, cw%, ch%, tw$, th$, #3, e%)

          init(" ") c1$(), c2$()
          j% = 0% : x% = 1% : eq% = cw%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                      0%, cw%, ch%, eq$(), ct$(), cr$(),                 ~
                      cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,       ~
                                                         #7, #8, #3, e% )
          if pass% = 1% then goto L02960
             goto L02970                          /* Load Cuts For Width */

L02960:   eq% = cw% + ch% : x% = cw% + 1%       /* Load Cuts for Height*/
L02970:   for i% = x% to eq%
              if cc$(i%) = " " then goto L03020   /* Skip - No Cross-Ref */
                 j% = j% + 1%                   /*  Defined for Part   */
                 c1$(j%) = str(col$(i%),1%,25%)
                 c2$(j%) = ct$(i%)
L03020:   next i%
        return

        con_fract                            /* Convert to Sixteenth's */
              calc = round( calc, 4 ) : calc$ = " "
              a% = int(calc) : b% = int((calc - a%) * 10000)
              if b% = 0% then goto L02270                /****************/
                 d% = int(b%/625)                      /* Conversion of*/
                 if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                 b% = d%                               /*  Sixteenth's */
                 if b% <> 16% then goto L02270           /****************/
                    a% = a% + 1% : b% = 0%          /* A% = Whole Part */
L02270:       convert a% to str(calc$,1%,3%), pic(###)
              if b% <> 0% then                                           ~
                              str(calc$,5%,5%) = str(sz$,(b%*5%) - 4%,5%)
        return

        exit_end
        end
