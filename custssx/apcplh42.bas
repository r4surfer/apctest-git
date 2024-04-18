*       ****************************************************************~
*                       NEW -( R e p o r t s )                         *~
*                                                                      *~ 
*                                                                      *~
*                  ( As of 11/14/97 - RHH Checked for R6.04.03 )       *~
*        APCPLH42 - Cut Sheet Report for Vinyl Casement (800) Series   *~
*                                                                      *~
*            Note - Primary Subroutine 'CALC_CUTS' at Line ( 3040 )    *~
*                   Mod to Change Sort by Load Uses Key '0'            *~
*                                                                      *~
*----------------------------------------------------------------------*
*Mod Date! Description                                             !By *
*--------+---------------------------------------------------------+---*
*03/29/98! Y2K                                                     !LDJ*
*09/25/07! (AWD001) mod for sclmr on report                        !CMG*~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC               !CMG*~
*04/13/2018! (CR1453) decrease line counts for current printers    !RDB*~
*       ****************************************************************

        sub "APCPLH42" (scr_dte$,        /* Production Date           */ ~
                        prod_dte$,       /* Completion Date           */ ~
                        #1,              /* (APCCUTW1) Cut Sheet Work */ ~
                        #2,              /* (APCCUTW2) Windings Work  */ ~
                        #3,              /* (GENCODES) Table File     */ ~
                        #4,              /* (AMTBOMIF) Validity File  */ ~
                        #5,              /* (HNYMASTR) Part Master    */ ~
                        #6,              /* (TXTFILE ) Text File      */ ~
                        #7,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #8 )             /* (AMTBOMCD) Equation File  */

        dim scr_dte$8,                   /* Production Date            */~
            prod_dte$8,                  /* Completion Date            */~
            wrk_key1$51, wrk_key$51,     /* Cut Sheet Work Key         */~
            wrk_rec$200,                 /* Cut Sheet Work Record      */~
            readkey$50,                  /* Code Table Key             */~
            descr$32,                    /* Code Table Description     */~
            scr_msg$40,                  /* Header Text                */~
            sav_mod$3,                   /* Save Model Code            */~
            sav_cl$1,                    /* Save Color Code            */~
            sav_part$25,                 /* Save Part Number           */~
            model$3,                     /* Model Code                 */~
            cl$1,                        /* Color Code                 */~
            color$6,                     /* Color Description          */~
            dt_ref$8,                    /* Product WARRANTY Number    */~
            dt_seq$5,                    /* Production Sequence Number */~
            c_o$2,                       /* Cottage/Oriel              */~
            t_t$4,                       /* Twin/Triple                */~
            locks$3,                     /* Number of Locks            */~
            fin$3,                       /* With or Without Fin        */~
            dtl_load$5, sav_load$5,      /* Load Number                */~
            dtl_txt$4,                   /* Product Text Id            */~
            dtl_part$25,                 /* Part Number                */~
            xw$7,                        /* Converted Window Width     */~
            xh$7,                        /* Converted Window Height    */~
            tw$1,                        /* WIDTH PARTS                */~
            th$1,                        /* HEIGHT PARTS               */~
            col$(100%)25,                /* Cut Descriptions           */~
            eq$(100%)8,                  /* PRIMARY CROSS REF KEYS     */~
            ct$(100%)9,                  /* WIDTH AND HEIGHT CUTS      */~
            ct(100%),                    /* WIDTH AND HEIGHT CUTS-DECIM*/~
            sh$(100%)1,                  /* SASH TYPE ( W, H, N )      */~
            cr$(100%)10,                 /* RAW MATERIAL PARTS         */~
            cp$(100%)2,                  /* NUMBER OF PIECES TO CUT    */~
            cc$(100%)1,                  /* CUT PIECE (Y) OR (N)       */~
            c1$(20%)19,                  /* Cut Descriptions for Report*/~
            c2$(20%)9                    /* Cuts Associated            */

        dim sav_key1$11,                 /* Save Text Key              */~
            text_key$11,                 /* Text Key                   */~
            text$(2%)70,                 /* Text Description           */~
            text_desc$60, text_d$(2%)60, /* Text Description           */~
            textid$4,                    /* Text Id                    */~
            text_flag$1                  /* Text Flag                  */

        dim clmr$3,                      /* (AWD001) CLMR              */~
            hg$2,                        /* (AWD001) HINGE             */~
            sz$100,                      /* (AWD001) Size              */~
            calc$9                       /* (AWD001) fraction size     */

            cw%, ch% = 0%
            tw$ = "1" : th$ = "2"
            call "APCCUTLD" ("8", cw%, ch%, tw$, th$, #3, e%)

            pass% = 0%                   /* 1st Pass for Width         */
            xcount% = 0%                 /* 2nd Pass for Height        */

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "                /* (AWD001)         */

            scr_msg$ = "   Vinyl Casement Cut Sheet for Width   "
            call "SHOSTAT" ("Printing for 800 Series (Width)")

L00760:     pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)
            init(" ") sav_cl$, wrk_rec$, sav_mod$, sav_part$, sav_load$
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))
            read #1,key > wrk_key$, using L00930  , wrk_key1$, wrk_rec$,    ~
                                                    eod goto print_a_done
            sav_mod$   = str(wrk_key1$,6%,3%)
            cl$        = str(wrk_key1$,9%,1%)
            gosub lookup_color
            gosub lookup_model
            sav_load$ = str(wrk_rec$,29%,5%)
            goto L00940
        print_a_next
            read #1, using L00930  , wrk_key1$, wrk_rec$,                   ~
                                                    eod goto print_a_done
L00930:       FMT POS(6), CH(51), CH(200)
L00940:     model$    = str(wrk_key1$,6%,3%)        /* MODEL       */
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

            clmr$ = str(dtl_part$,20,3)             /* CLMR (AWD001) */
            hg$   = str(dtl_part$,9,2)              /* Hinge (AWD001)*/


            xw$ = str(wrk_rec$,63%,7%)              /* WIDHT       */
            xh$ = str(wrk_rec$,70%,7%)              /* HEIGHT      */
            if cl$ <> sav_cl$ then gosub lookup_color
            if sav_mod$ <> model$ then gosub lookup_model
            if len(dtl_part$) > 18 then goto L01150
               xw$, xh$ = "PART    "
               init(" ") c_o$, t_t$, t_b$, locks$, fin$

L01150:     total% = total% + 1%
            gosub detail_a
            goto print_a_next
        print_a_done
            print using L02340
            call "SETPRNT" ("APCC", " ", 0%, 1%)
            pass% = pass% + 1%
            if pass% > 1% then goto L01270
               scr_msg$ = "   Vinyl Casement Cut Sheet For Height  "
               call "SHOSTAT" ("Printing for 800 Series (Height)")
               goto L00760

L01270: goto exit_program

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
            read #6,key > text_key$, eod goto L01740
               get #6, using L01670 , text_key$,text$()
L01670:          FMT CH(11), POS(64), 2*CH(70)
            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then return
            if text$(1%) <> " " then text_desc$ = str(text$(1%),1%,60%)  ~
                                else text_desc$ = str(text$(2%),1%,60%)
            if text_desc$ <> " " then text_flag$ = "Y"
            text_d$(1%) = str(text$(1%),1%,60%)
            text_d$(2%) = str(text$(2%),1%,60%)
L01740: return

L01760: %! No Calculation for: #########################                 ~
        ~                                                                !

L01790: %!Sq(#####)Model:### Color: ###### Wr######## Width: ######## Hei~
        ~ght: ####### Co/Or: ## Twin/Triple: #### T/B: ### Lks:### Fn:###!

        %!####################:(########) ####################:(########)~
        ~  ####################:(########) ####################:(########)~

L01850:   FMT CH(1),CH(19),CH(2),CH(9),CH(2),CH(19),CH(2),CH(9),CH(2),   ~
              CH(19),CH(2),CH(9),CH(2),CH(19),CH(2),CH(9),CH(2)

L01880: %!TXT:###########################################################~
        ~#                                                               !

L02135: %!SCLMR:  ( ######### )                                          ~
        ~                                                                !

L01910: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

        detail_a                                       /* C1$(), C2$() */
             if lcnt% > 55% then gosub header          /* CR1453 */
             if sav_load$ = dtl_load$ then goto L02000
                sav_load$ = dtl_load$
                gosub header

L02000:      print using L01910
             print using L01790, dt_seq$, model$, color$, dt_ref$, xw$,    ~
                               xh$, c_o$, t_t$, t_b$, locks$, fin$
             lcnt% = lcnt% + 2%
             if xw$ = "PART    " then goto L02230
             if sav_part$ = dtl_part$ then goto L02090
                gosub calc_cuts
                sav_part$ = dtl_part$

L02090:      if j% <> 0% then goto L02130
                print using L01760, dtl_part$         /* No Calculations */
                lcnt% = lcnt% + 1%
                goto L02230
L02130:      i% = 1%
             xcount% = xcount% + 1%
L02150:          print using L01850,  "!", c1$(i%), ":(", c2$(i%), ") ",   ~
                                 c1$(i% + 1%), ":(", c2$(i% + 1%), ") ", ~
                                 c1$(i% + 2%), ":(", c2$(i% + 2%), ") ", ~
                                 c1$(i% + 3%), ":(", c2$(i% + 3%), ")!"
                 lcnt% = lcnt% + 1%
             i% = i% + 4%
             if i% > j% then goto L02230
                goto L02150

L02230:      
                if pass% <> 1% then goto L02235
                if hg$ < "70" or hg$ > "97" then goto L02235
                if clmr$ = "000" then goto L02235
                    convert clmr$ to clmr, data goto L02235
                    clmr = clmr / 10.0
                    a% = int(clmr) : b = (clmr - a%) / 0.8
                    clmr = a% + b
                    calc = clmr

                    gosub con_fract
                    print using L02135, calc$


L02235:      gosub lookup_text
             if text_flag$ <> "Y" then return
                if text_d$(1%) <> " " then                               ~
                   print using L01880, text_d$(1%)
                if text_d$(2%) <> " " then                               ~
                   print using L01880, text_d$(2%)
                lcnt% = lcnt% + 1%
                if text_d$(1%) <> " " and text_d$(2%) <> " " then        ~
                   lcnt% = lcnt% + 1%           /* Add Additional Line */
        return

L02340: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L02370: %!               Production Date: ##########                     ~
        ~                        Completion Date: ##########             !
                                                                                /* (Y2K, LDJ) */
L02400: %! Load No.: #####                        #######################~
        ~#################                                    Page: #### !

        header
          if lcnt% <> 99% then print using L02340
          pageno% = pageno% + 1%
          print page
          print using L02340
          print using L02370, scr_dte$, prod_dte$
          print using L02400, sav_load$, scr_msg$, pageno%
          lcnt% = 3%
        return

        calc_cuts
          init(" ") c1$(), c2$()
          j% = 0% : x% = 1% : eq% = cw%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                      0%, cw%, ch%, eq$(), ct$(), cr$(),                 ~
                      cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,       ~
                                                         #7, #8, #3, e% )
          if pass% = 1% then goto L02620
             goto L02630                          /* Load Cuts For Width */

L02620:   eq% = cw% + ch% : x% = cw% + 1%       /* Load Cuts for Height*/
L02630:   for i% = x% to eq%
              if cc$(i%) = " " then goto L02680   /* Skip - No Cross-Ref */
                 j% = j% + 1%                   /*  Defined for Part   */
                 c1$(j%) = str(col$(i%),1%,25%)
                 c2$(j%) = ct$(i%)
L02680:   next i%



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

        exit_program
          if xcount% > 0% then goto L02760
          call "SHOSTAT" ("N O   L I N E A L M A T E   D A T A")
          call "PAUSE" addr(300%)

L02760: end

