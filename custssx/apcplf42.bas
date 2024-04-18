*       ****************************************************************~
*                       NEW -( R e p o r t s )                         *~
*                                                                      *~
*                  ( As of 04/21/10 - RHH Checked for Rev 01.00.00     *~
*                                                                      *~
*                  Report Type 5 is New Sash Reports                   *~ 
*        APCPLF42 - Cut Sheet and Windings Report(s).                  *~
*                     600 Series - Vinyl Replacement and               *~
*                                  New Construction  and               *~
*                     400 Series   New Family                          *~
*            Note - Primary Subroutine 'CALC_CUTS' at Line ( 3040 )    *~
*                   SPEC% = 0% - (Avail)                               *~
*                   SPEC% = 1% - New Construction                      *~
*                   SPEC% = 2% - Vinyl Slider and Picture              *~
*                   SPEC% = 3% - All Welded Replacement                *~
*                   SPEC% = 4% - All Welded DH New Construction        *~
*                   SPEC% = 9% - 450 Brick Mold DH                     *~ 
*                   SPEC% =11% - 267C DH Line                          *~ 
*                                                                      *~
*                   SPEC% = 5% - Glazing Bead Report                   *~
*                   SPEC% = 6% - Rebar Report                          *~
*                   SPEC% = 7% - Frame Snap In Report                  *~
*                   SPEC% = 8% - Department Defined                    *~
*                   spec% = 10%- New Sash Report for All Dept's        *~  
*                                                                      *~
*                   Mod to Sorts for By Load, The Cut Sheet Uses       *~
*                     Key '0' from File #1. The Tube Wind Report Uses  *~
*                     Key '0' from File #2. (Has Special Sort )        *~
*                                                                      *~
*                   Mods for Special Cut sheet reports, uses new table *~
*                     (APCCUTRPT). Loads valid equations in SEL$().    *~
*                     Uses Sub. CHECK_SPECIAL to verify cuts.          *~
*                     Max Definitions per Report (4000%)               *~
*                                                                      *~
*                   (EWD001)                                           *~
*                   Specials Definition - (APCCUTRPT) Key = RMMMCTEQ   *~
*                     R   = Report Type 1, 2, 3, 4 or 5                *~
*                     MMM = Model Code                                 *~
*                     C   = Color Code                                 *~
*                     T   = Cut Equation ex. 1=Width, 2=Height, etc.   *~
*                     EQ  = Cut Equation Number                        *~ 
*                                                                      *~
*                   Special Report Name by Department                  *~
*                     (APCCUTDPT) - Key = RDPT, Decsr = Report Name    *~
*                     R    = Report Type 1, 2, 3, 4, 5                 *~
*                     DPT  = Department Code Assoc. With Report        *~
*                     Desc = Report Name                               *~
*                                                                      *~  
*----------------------------------------------------------------------*~
*Mod Date  ! Description                                           !By *~
*--------__+-------------------------------------------------------+---*~
*03/29/98  ! Y2K                                                   !LDJ*~
*09/16/98  ! (EWD001) Allow all Departments to Have Special Reprt  !RHH*~
*12/18/98  ! (EWD002) Mods for Screen Only Dept                    !RHH*~
*10/13/03  ! (EWD003) Mod to use color in part number instead color!CMG*~
*          !              in table 'APCCUTRPT'.                    !   *~
*10/10/04  ! (AWD004) Mod not to print NO CALCULATIONS             !CMG*~
*12/16/05  ! (AWD005) Mod for 450 Brick Mold DH            (EWD038)!RHH*~
*04/17/06  ! (AWD006) Sub Part Number and Info fields added to     !RHH*~
*          !          (APCCUTWK) and (APCCUTW2) work files.        !   *~
*05/23/06  ! (AWD007) New Sash Report - Spec% = 10%                !RHH*~
*          !          Key Change in 'load_specials'                !   *~
*09/15/06  ! (AWD008) Mod to fix an initialization bug             !CMG*~
*09/25/07  ! (AWD009) mod for sclmr on report                      !CMG*~
*03/05/08  ! (AWD010) mod for sdl                                  !CMG*~
*04/03/08  ! (AWD011) mod for sdl equation phantom numbers         !CMG*~
*04/21/10  ! (AWD012) mod for dept 5 - 267C                        !CMG*~
*03/30/2011! (AWD013) mods for sash limiter on report              !CMG*~
*06/03/2014! (CUT001)  mod to add dim fields to CUTCC              ! MES *~
*04/12/2018! (CR1360) decrease line counts for current printers    !RDB*~
************************************************************************

        sub "APCPLF42" (scr_dte$,        /* Production Date           */ ~
                        prod_dte$,       /* Completion Date           */ ~
                        spec%,           /* 0%-4%,9% Reg,5%-8%,10%Spec*/ ~
                        scr_dept$,       /* Department Code           */ ~ 
                        #1,              /* (APCCUTWK) Cut Sheet Work */ ~
                        #2,              /* (APCCUTW2) Windings Work  */ ~
                        #3,              /* (GENCODES) Table File     */ ~
                        #4,              /* (AMTBOMIF) Validity File  */ ~
                        #5,              /* (HNYMASTR) Part Master    */ ~
                        #6,              /* (TXTFILE ) Text File      */ ~
                        #7,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #8 )             /* (AMTBOMCD) Equation File  */

        dim sel$(4000%)8, sel$1,         /* Special Report Equations   */~
            scr_dte$8, userid$3,         /* Production Date            */~
            scr_dept$3,                  /* Department Code            */~
            prod_dte$8,                  /* Completion Date            */~
            wrk_key1$51, dtl_key1$25,    /* Cut Sheet Work Key         */~
            wrk_key$51,                  /* Cut Sheet Work Key         */~
            wrk_rec$200,                 /* Cut Sheet Work Record      */~
            readkey$50,                  /* Code Table Key             */~
            prt_desc1$32,                /*                            */~
            prt_desc2$40,                /*                            */~
            part_desc$32,                /*                            */~
            apc_prt$60,                  /*                            */~
            apc_scr$120,                 /*                            */~
            apc_sze$20,                  /*                            */~
            apc_sod$40,                  /*                            */~
            sub_scr$120,                 /* Subpart Descr      (AWD006)*/~
            sub_prt$60,                  /* Subpart Print Descr(AWD006)*/~
            descr$32,                    /* Code Table Description     */~
            scr_msg$40, title$40,        /* Header Text                */~
            sav_mod$3,                   /* Save Model Code            */~
            sav_cl$1,                    /* Save Color Code            */~
            sav_part$25,                 /* Save Part Number           */~
            sav_part_new$45,             /* Save New Part No.  (AWD006)*/~
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
            dtl_sub_part$20,             /* New sub Part No    (AWD006)*/~
            dtl_sub_info$20,             /* New Sub Info Fields(AWD006)*/~
            dtl_new_part$45,             /* New Part Number    (AWD006)*/~ 
            prod$1,                      /* Product Code               */~
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
            c2$(100%)9,                  /* Cuts Associated            */~
            w1$3, w2$3, w3$3,            /* TUBE WINDING REPORT        */~
            c_wind$9, t_wind$9           /* CHECK FOR BREAK            */

        dim sav_key1$11,                 /* Save Text Key              */~
            text_key$11,                 /* Text Key                   */~
            text$(2%)70,                 /* Text Description           */~
            text_desc$60, text_d$(2%)60, /* Text Description           */~
            textid$4,                    /* Text Id                    */~
            text_flag$1                  /* Text Flag                  */

        dim clmr$3,                      /* (AWD009) CLMR              */~
            hg$2,                        /* (AWD009) HINGE             */~
            sz$100,                      /* (AWD009) Size              */~
            calc$9                       /* (AWD009) fraction size     */

/* (AWD008) */
	    sel_max% = 0% 

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "                /* (AWD009)         */
                                         /* (EWD001)                   */
                                         /* (AWD007)                   */
                                         /* Spec% = 5%,6%,7%,8%,10%    */
/* (AWD012) */
            if spec% <= 4% then goto noSpecials
            if spec% = 9% then goto noSpecials
            if spec% = 11% then goto noSpecials
                 gosub load_specials
REM         if spec% > 4% and spec% <> 9% then gosub load_specials
noSpecials:          
/* (AWD012/) */
                                         /* (AWD007)                   */
            call "EXTRACT" addr("ID", userid$)

            pass% = 0%                   /* 1st Pass for Width         */
                                         /* 2nd Pass for Height        */
                                         /* Test for Skip Messages    */
/* (AWD012) */                           
REM         if spec% > 4% and spec% <> 9% then goto L01390
            if spec% <= 4% then goto headingMessage
            if spec% = 9% then goto headingMessage
            if spec% = 11% then goto headingMessage
                     goto L01390
headingMessage:
/* (AWD012\) */
                                                        /* (AWD006)     */ 
            if spec% = 0% then                                           ~
                   scr_msg$ = "A V A I L A B L E                     "
            if spec% = 1% then                                           ~
                   scr_msg$ = "New Construction Cut Sheet For Width  "
            if spec% = 2% then                                           ~
                   scr_msg$ = "Vinyl Slider/Pict. Cut Sheet For Width"
            if spec% = 3% then                                           ~
                 scr_msg$ = "All Welded Replace Cut Sheet For Width  "
            if spec% = 4% then                                           ~
                 scr_msg$ = "All Welded DH NewCon Cut Sheet For Width"
                                                        /* (AWD005)     */
            if spec% = 9% then                                           ~
                 scr_msg$ = "450 Brick Mold DH Cut Sheet For Width   " 
                                                        /* (AWD005)     */
            if spec% = 11% then                         /*(AWD012) */    ~
                 scr_msg$ = "267C DH Cut Sheet For Width   " 
                                                        
                                                        
                                                        /* (AWD006)     */
            if spec% = 0% then                                           ~
                   call "SHOSTAT" ("A V A I L A B L E      (Width)")
            if spec% = 1% then                                           ~
                   call "SHOSTAT" ("Print New Construction (Width)")
            if spec% = 2% then                                           ~
                   call "SHOSTAT" ("Print Vinyl Slider & Pict (Width)")
            if spec% = 3% then                                           ~
                 call "SHOSTAT" ("Print All Welded Replacement (Width)")
            if spec% = 4% then                                           ~
                call "SHOSTAT" ("Print All Welded DH New Constr (Width)")
                                                        /* (AWD005)     */
            if spec% = 9% then                                           ~
                 call "SHOSTAT" ("Print 450 Brick Mold DH (Width)")
                                                        /* (AWD005)     */  
            if spec% = 11% then                         /* (AWD012) */   ~
                 call "SHOSTAT" ("Print 267C DH (Width)")

L01390:     pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)
            init(" ") sav_cl$, wrk_rec$, sav_mod$, sav_part$, sav_load$
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))

            if spec% = 2% then goto L01510
               read #1,key > wrk_key$, using L01630 , wrk_key1$, wrk_rec$, ~
                                                    eod goto print_a_done
               sav_mod$   = str(wrk_key1$,6%,3%)
               cl$        = str(wrk_key1$,9%,1%)
               goto L01560
L01510:     read #1,key 1% > wrk_key1$, using L01630, wrk_key1$, wrk_rec$, ~
                                                    eod goto print_a_done
            sav_mod$   = str(wrk_key1$,17%,3%)
            cl$        = str(wrk_key1$,20%,1%)

L01560:     gosub lookup_color
            gosub lookup_model
            sav_load$ = str(wrk_rec$,29%,5%)
            goto L01640
        print_a_next
            read #1, using L01630 , wrk_key1$, wrk_rec$,                   ~
                                                    eod goto print_a_done
L01630:       FMT POS(6), CH(51), CH(200)
L01640:     if spec% = 2% then goto L01680
               model$    = str(wrk_key1$,6%,3%)     /* MODEL       */
               cl$       = str(wrk_key1$,9%,1%)     /* COLOR       */
               goto L01710
L01680:     model$    = str(wrk_key1$,17%,3%)       /* MODEL       */
            cl$       = str(wrk_key1$,20%,1%)       /* COLOR       */

L01710:     if spec% <> 7% then goto L01720         /* (EWD002)    */
               if scr_dept$ = "000" and str(model$,1%,1%) <> "0" then ~
                                        goto print_a_next

L01720:     dt_ref$= str(wrk_rec$,1%,8%)            /* REF NO      */
            dt_seq$= str(wrk_rec$,9%,5%)            /* SEQUENCE NO */
            c_o$   = str(wrk_rec$,14%,2%)           /* COTTAGE/ORIE*/
            t_t$   = str(wrk_rec$,16%,4%)           /* TWIN/TRIPLE */
            t_b$   = str(wrk_rec$,20%,3%)           /* TSO, BSO,FSO*/
            locks$ = str(wrk_rec$,23%,3%)           /* LOCKS       */
            fin$   = str(wrk_rec$,26%,3%)           /* WITH FIN    */
            dtl_load$ = str(wrk_rec$,29%,5%)        /* LOAD NUMBER */
            dtl_txt$  = str(wrk_rec$,34%,4%)        /* TEXT ID     */
            dtl_part$ = str(wrk_rec$,38%,25%)       /* PART NO.    */
            clmr$ = str(dtl_part$,20,3)             /* CLMR (AWD009) */
            hg$   = str(dtl_part$,9,2)              /* Hinge (AWD009)*/

            xw$ = str(wrk_rec$,63%,7%)              /* WIDHT       */
            xh$ = str(wrk_rec$,70%,7%)              /* HEIGHT      */
                                                    /* (AWD006)    */
            dtl_sub_part$ = str(wrk_rec$,81%,20%)
            dtl_sub_info$ = str(wrk_rec$,101%,20%)
            str(dtl_new_part$,1%,25%)  = dtl_part$
            str(dtl_new_part$,26%,20%) = dtl_sub_part$
                                                    /* (AWD006)    */
            if cl$ <> sav_cl$ then gosub lookup_color
            if sav_mod$ <> model$ then gosub lookup_model
            if len(dtl_part$) > 18 then goto L01890
               xw$, xh$ = "PART    "
               init(" ") c_o$, t_t$, t_b$, locks$, fin$

L01890:     total% = total% + 1%
            gosub detail_a
            goto print_a_next
        print_a_done
            print using L04340
            lcnt% = lcnt% + 1%
	    call "SETPRNT" ("APCC", " ", 0%, 1%)
            close printer
            if sel_max% <> 0% then goto exit_sub/* Finished With Spec */
                                                /* Report             */
            pass% = pass% + 1%
            if pass% > 1% then goto L02240
                                                 /* (AWD006)            */
               if spec% = 0% then                                        ~
                   scr_msg$ = "A V A I L A B L E              Height   "
               if spec% = 1% then                                        ~
                   scr_msg$ = "New Construction Cut Sheet For Height   "
               if spec% = 2% then                                        ~
                   scr_msg$ = "Vinyl Slider/Pict Cut Sheet For Height  "
               if spec% = 3% then                                        ~
                   scr_msg$ = "All Welded Replacement Cut Sheet Height "
               if spec% = 4% then                                        ~
                   scr_msg$ = "All Welded DH New Const Cut Sheet Height"
                                                 /* (AWD005)            */
               if spec% = 9% then                                        ~
                   scr_msg$ = "450 Brick Mold DH   Cut Sheet For Height"   
                                                 /* (AWD005)            */
               if spec% = 11% then               /* (AWD012)      */     ~
                   scr_msg$ = "267C DH Cut Sheet For Height "
                   
                                                 /* (AWD006)            */  
               if spec% = 0% then                                        ~
                   call "SHOSTAT" ("A V A I L A B L E      (Height)")
               if spec% = 1% then                                        ~
                   call "SHOSTAT" ("Print New Construction (Height)")
               if spec% = 2% then                                        ~
                   call "SHOSTAT" ("Print Vinyl Slider & Pict (Height)")
               if spec% = 3% then                                        ~
                 call "SHOSTAT" ("Print All Welded Replacement (Height)")
               if spec% = 4% then                                        ~
               call "SHOSTAT" ("Print All Welded DH New Constr (Height)")
                                                 /* (AWD005)            */
               if spec% = 9% then                                        ~
                   call "SHOSTAT" ("Print 450 Brick Mold DH (Height)")
                                                 /* (AWD005)            */
               if spec% = 11% then                /*(AWD012)  */         ~
                   call "SHOSTAT" ("Print 267C DH (Height)")

               goto L01390

L02240:     pageno% = 0% : lcnt% = 99%

            if spec% = 3% then goto exit_sub
            if spec% = 11% then goto exit_sub     /* (AWD012)  */
                                                  /* (AWD006)           */ 
            if spec% = 0% then                                           ~
                   scr_msg$ = " Vinyl Tube Winding Report (Available)  "
            if spec% = 1% then                                           ~
                   scr_msg$ = " Vinyl Tube Winding Report (New Const.) "
            if spec% = 2% then                                           ~
                   scr_msg$ = " Vinyl Tube Winding Report ( Sliders )"
            if spec% = 4% then                                           ~
                   scr_msg$ = " Vinyl Tube Winding Report ( 412 )"
                                                  /* (AWD005)           */
            if spec% = 9% then                                           ~
                   scr_msg$ = " Vinyl Tube Winding Report ( 450 )"
                                                  /* (AWD005)           */
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)
                                                   /* (AWD006)          */
            if spec% = 0% then                                           ~
                   call "SHOSTAT" ("A V A I L A B L E Tube Winding")
            if spec% = 1% then                                           ~
                   call "SHOSTAT" ("Print New Construction Winding")
            if spec% = 2% then                                           ~
                   call "SHOSTAT" ("Print Vinyl Sliders Winding")
            if spec% = 4% then                                           ~
                   call "SHOSTAT" ("Print Vinyl (412) Winding")
                                                   /* (AWD005)          */
            if spec% = 9% then                                           ~
                   call "SHOSTAT" ("Print Vinyl (450) Winding")
                                                   /* (AWD005)          */
            init(" ") sav_cl$, sav_load$, sav_part$, sav_mod$, c_wind$,  ~
                      t_wind$

            report_Flag% = 0%                      /* (AWD005)          */
            dtl_key1$ = all(hex(00))
                                                   /* (AWD006)          */
            read #2,key > dtl_key1$, using L02590 , dt_seq$, dtl_load$,  ~
                                     dtl_part$, dt_ref$, w1$, w2$, w3$,  ~
                                     dtl_sub_part$, dtl_sub_info$,       ~
                                                eod goto report_b_done
            sav_mod$ = str(dtl_part$,1%,3%)
            sav_load$ = dtl_load$
            goto L02600
        print_b_next
            read #2, using L02590 , dt_seq$, dtl_load$, dtl_part$, dt_ref$,~
                                    w1$, w2$, w3$, dtl_sub_part$, dtl_sub_info$, ~
                                                      eod goto report_b_done
L02590:        FMT POS(26), CH(5), CH(5), CH(25), CH(8), 3*CH(3), CH(20), CH(20)
                                                    /* (AWD006)          */   
L02600:     
            model$ = str(dtl_part$,1%,3%)
            cl$ = str(dtl_part$,4%,1%)
            init(" ") prt_desc1$, prt_desc2$
            if sav_cl$ <> cl$ then gosub lookup_color
            str(dtl_new_part$,1%,25%)  = dtl_part$       /* (AWD006)     */
            str(dtl_new_part$,26%,20%) = dtl_sub_part$   /* (AWD006)     */
            if sav_part_new$ = dtl_new_part$ then goto L02700
        REM       call "APCLDSUB" (dtl_part$, apc_sod$, #4, err%)
               call "AWDDESCR" (dtl_part$, dtl_sub_part$, apc_scr$, apc_prt$,~
                                   sub_scr$, sub_prt$, apc_sze$, #11, err% ) 
               apc_sod$ = apc_sod$ & sub_prt$
                                                         /* (AWD006)     */
               gosub lookup_part
               prt_desc1$ = part_desc$
               prt_desc2$ = apc_sod$
               if len(dtl_part$) < 19 then prt_desc2$ =" **** PART **** "
L02700:     if sav_mod$  = model$ then goto L02740
               sav_mod$  = model$
               sav_load$ = dtl_load$
               gosub header
L02740:     gosub detail_b

           goto print_b_next
        report_b_done
            if report_flag% = 0% then gosub header  /* (AWD005)        */

            print using L04340
	    lcnt% = lcnt% + 1%
            call "SETPRNT" ("APCC", " ", 0%, 1%)
            close printer
        goto exit_sub                              /* Finished         */

L02830: %!Seq.  Mod Color  Warranty   Load  <------ Part Description ----~
        ~--> !  <-------- Special Description ---------> ! HT1 - HT2 - WD!
L02850: %!----- --- ------ --------- -----  -----------------------------~
        ~--- !  ---------------------------------------- ! --------------!
L02870: %!##### ### ###### ########  #####  #############################~
        ~### !  ######################################## ! ###   ###  ###!
L02890: %!---------------------------------------------------------------~
        ~----!-------------------------------------------!---------------!


        detail_b
            report_flag% = 99%                     /* (AWD005)         */ 
            if lcnt% > 55% then gosub header            /* CR1360 */
            if sav_load$ = dtl_load$ then goto L02980
               sav_load$ = dtl_load$
               gosub header

L02980:     t_wind$ = w1$ & w2$ & w3$
            if c_wind$ = t_wind$ then goto L03040
               print using L02890
               lcnt% =lcnt% + 1%
               c_wind$ = t_wind$

L03040:     print using L02870 , dt_seq$, model$, color$,dt_ref$,dtl_load$,~
                               prt_desc1$, prt_desc2$, w1$, w2$, w3$
            lcnt% = lcnt% + 1%
        return


        lookup_part                           /* Check HNYMASTR        */
            sav_part_new$ = dtl_new_part$
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$
            read #5,key = dtl_part$,using L03150 , part_desc$, apc_prt$,   ~
                                                apc_sze$, eod goto L03170
L03150:        FMT XX(25), CH(32), POS(606), CH(60), CH(20)
            goto L03270
L03170:        err% = 0%
               if len(dtl_part$) > 18% then goto L03230
                  part_desc$, apc_prt$ = "COMPONENT PART"
                  gosub lookup_text
                  if text_flag$ = "Y" then part_desc$,apc_prt$=text_desc$
                  goto L03270
L03230: 
                                                      /* (AWD006)      */ 
               call "AWDDESCR" (dtl_part$, dtl_sub_part$, apc_scr$, apc_prt$,~
                                   sub_scr$, sub_prt$, apc_sze$, #4, err% )
               str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
               str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L03270: return

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
            read #6,key > text_key$, eod goto L03740
               get #6, using L03670 , text_key$,text$()
L03670:          FMT CH(11), POS(64), 2*CH(70)
            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then return
            if text$(1%) <> " " then text_desc$ = str(text$(1%),1%,60%)  ~
                                else text_desc$ = str(text$(2%),1%,60%)
            if text_desc$ <> " " then text_flag$ = "Y"
            text_d$(1%) = str(text$(1%),1%,60%)
            text_d$(2%) = str(text$(2%),1%,60%)
L03740: return
                                                           /* (AWD006)  */
L03760: %! No Calculation for: #########################  ###############~
        ~#####                                                           !

L03790: %!Sq(#####)Model:### Color: ###### Wr######## Width: ######## Hei~
        ~ght: ####### Co/Or: ## Twin/Triple: #### T/B: ### Lks:### Fn:###!

        %!####################:(########) ####################:(########)~
        ~  ####################:(########) ####################:(########)~

/* (AWD010) beg sdl */
L03795: %!SDL:###                                                        ~
        ~                                                                !

L03850:   FMT CH(1),CH(19),CH(2),CH(9),CH(2),CH(19),CH(2),CH(9),CH(2),   ~
              CH(19),CH(2),CH(9),CH(2),CH(19),CH(2),CH(9),CH(2)

L03880: %!TXT:###########################################################~
        ~#                                                               !

L03885: %!SCLMR:  ( ######### )                                          ~
        ~                                                                !
        
/* (AWD013) */
L03888: %!Sash Accessory : ###############                               ~
        ~                                                                !

L03910: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

        detail_a                                       /* C1$(), C2$() */
             if xw$ = "PART    " then goto no_calc
REM             if sav_part$ = dtl_part$ then goto no_calc
                gosub calc_cuts
                if spec% = 8% and j% = 0% then return
                sav_part_new$ = dtl_new_part$
                
no_calc:

             if lcnt% > 55% then gosub header          /* CR1360 */
                goto L04000
             if sav_load$ = dtl_load$ then goto L04000
                sav_load$ = dtl_load$
                gosub header

L04000:      print using L03910
             print using L03790, dt_seq$, model$, color$, dt_ref$, xw$,    ~
                               xh$, c_o$, t_t$, t_b$, locks$, fin$
             lcnt% = lcnt% + 2%
/* (AWD010) */
             if str(dtl_sub_part$,8,1) <> "1" then goto not_sdl
              print using L03795, "YES"
              lcnt% = lcnt% + 1%
not_sdl:
             if xw$ = "PART    " then goto L04230
REM             if sav_part$ = dtl_part$ then goto L04090
REM                gosub calc_cuts
REM                sav_part$ = dtl_part$

             if j% <> 0% then goto L04130
                if spec% = 8% then return              /* (AWD004)  */
                                                       /* No Calculations */
                print using L03760, dtl_part$, dtl_sub_part$
                lcnt% = lcnt% + 1%
                goto L04230
L04130:      i% = 1%

L04150:          print using L03850,  "!", c1$(i%), ":(", c2$(i%), ") ",   ~
                                 c1$(i% + 1%), ":(", c2$(i% + 1%), ") ", ~
                                 c1$(i% + 2%), ":(", c2$(i% + 2%), ") ", ~
                                 c1$(i% + 3%), ":(", c2$(i% + 3%), ")!"
                 lcnt% = lcnt% + 1%
             i% = i% + 4%
             if i% > j% then goto L04230
                goto L04150

L04230:      if pass% <> 1% then goto L04235
                if hg$ < "70" or hg$ > "97" then goto L04235
                if clmr$ = "000" then goto L04235
                    convert clmr$ to clmr, data goto L04235
                    clmr = clmr / 10.0
                    a% = int(clmr) : b = (clmr - a%) / 0.8
                    clmr = a% + b
                    calc = clmr

                    gosub con_fract
                    print using L03885, calc$
                    lcnt% = lcnt% + 1%
 

/* (AWD013) */
L04235:      if str(dtl_sub_part$,16,1) = "0" then goto L04240
               if str(dtl_sub_part$,16,1) = "1" then print using L03888, "Night Latch"
	       if str(dtl_sub_part$,16,1) = "1" then lcnt% = lcnt% + 1%
               if str(dtl_sub_part$,16,1) = "2" then print using L03888, "Sash Limiter"
	       if str(dtl_sub_part$,16,1) = "2" then lcnt% = lcnt% + 1%

L04240:      gosub lookup_text
             if text_flag$ <> "Y" then return
                if text_d$(1%) <> " " then                               ~
                   print using L03880, text_d$(1%)
                if text_d$(2%) <> " " then                               ~
                   print using L03880, text_d$(2%)
                 if text_d$(1%) <> " " then lcnt% = lcnt% + 1%
                 if text_d$(2%) <> " " then lcnt% = lcnt% + 1% /*AddlLine */
        return

L04340: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L04370: %!               Production Date: ##########                     ~
        ~                        Completion Date: ##########             !
                                                                                
L04400: %! Load No.: #####                        #######################~
        ~#################                                    Page: #### !

L04430: %! New Version - (###)                                           ~
        ~                                                                !

        header
          if lcnt% <> 99% then print using L04340
	  if lcnt% <> 99% then lcnt% = lcnt% + 1%

            title$ = scr_msg$
            call "FMTTITLE" (title$, " ", 12%)

          pageno% = pageno% + 1%
          print page
          print using L04340
          print using L04370, scr_dte$, prod_dte$
          print using L04400, " ", scr_msg$, pageno%
          lcnt% = 3%
          sav_load$ = dtl_load$
          if pass% < 2% then return
             print using L03910                   /* Tube Winding Header */
             print using L04430, userid$
             print using L03910
             print using L02830
             print using L02850
             lcnt% = lcnt% + 5%
             c_wind$ = w1$ & w2$ & w3$
        return

        calc_cuts
          cw% = 0%  : ch% = 0%
          tw$ = "1" : th$ = "2"
          init(" ") c1$(), c2$(), prod$
          prod$ = str(dtl_part$,1%,1%)
          call "APCCUTLD" (prod$, cw%, ch%, tw$, th$, #3, e%)

          j% = 0% : x% = 1% : eq% = cw%
/* (AWD011) */
          mfg% = 0%
          if str(dtl_sub_part$,8,1) = "1" and str(dtl_sub_part$,9,1) <> "2" ~
                                            then mfg% = 3%
          call "APCCUTCC" (dtl_part$,0%, 0%, 0%, /* (CUT001) */         ~ 
                      mfg%, cw%, ch%, eq$(), ct$(), cr$(),              ~
                      cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,       ~
                                                         #7, #8, #3, e% )
          if sel_max% = 0% then goto L04810       /* No Special Report   */
             x% = 1%                              /* For Specials Do the */
             eq% = cw% + ch%                      /* Width and Height in */
             goto L04850                          /* One Pass.           */

L04810:   if pass% = 1% then goto L04840
             goto L04850                          /* Load Cuts For Width */

L04840:   eq% = cw% + ch% : x% = cw% + 1%         /* Load Cuts for Height*/
L04850:   for i% = x% to eq%
              if cc$(i%) = " " then goto L04930   /* Skip - No Cross-Ref */
                 if sel_max% = 0% then goto L04900
                                                  /* (AWD007)            */
                    gosub check_special           /* (EWD001)            */
                    if check% = 0% then goto L04930
L04900:          j% = j% + 1%                     /*  Defined for Part   */
                 c1$(j%) = str(col$(i%),1%,25%)
                 c2$(j%) = ct$(i%)
L04930:   next i%
        return

        check_special                           /* Check for Special */
          check% = 0%                           /* Report Equations  */

          for kk% = 1% to sel_max%              /* From (APCCUTRPT)  */
                                                /* (EWD003)          */
            str(sel$(kk%),5%,1%) = str(dtl_part$,4%,1%)
            if str(eq$(i%),2%,7%) = str(sel$(kk%),2%,7%) then goto L05020
                                                /* (EWD001) No Match */
          next kk%
        return
L05020:   check% = 1%
        return

        load_specials                     /* (EWD001) Can be used by  */
          sel_max% = 0%                   /*   all Departments        */
          init(" ") readkey$, descr$, sel$(), sel$
          convert (spec% - 4%) to sel$, pic(#)
                                          /* (AWD007)                 */
          if spec% = 10% then sel$ = "5"  /* New Sash Report          */  
                                          /* (AWD007)                 */
          str(readkey$,1%,9%)   = "APCCUTRPT"
          str(readkey$,10%,15%) = sel$
        load_spec_nxt
          read #3,key > readkey$, using L05160 , readkey$,                 ~
                                               eod goto load_spec_done
L05160:      FMT CH(24)
          if str(readkey$,1%,9%) <> "APCCUTRPT" then goto load_spec_done
          if str(readkey$,10%,1%) <> sel$ then goto load_spec_done
             if scr_dept$ = "000" and str(readkey$,11%,1%) <> "0" then     ~
                goto load_spec_nxt
                                                /* 12/18/98 - EWD002     */ 
             sel_max% = sel_max% + 1%
             if sel_max% > 4000% then sel_max% = 4000%
             str(sel$(sel_max%),1%,1%) = sel$
             str(sel$(sel_max%),2%,7%) = str(readkey$,11%,7%)
             goto load_spec_nxt
        load_spec_done
          if sel_max% = 0% then goto L05260

          gosub load_rpt_name 
        return
L05260:   call "SHOSTAT" ("Error-Unable to Load Special Report Data")
          stop
        return

        load_rpt_name                                    /* (AWD007)  */ 
          init(" ") readkey$, descr$, scr_msg$
          str(readkey$,1%,9%)   = "APCCUTDPT"
          str(readkey$,10%,1%)  = sel$
          str(readkey$,11%,14%) = scr_dept$
          read #3,key = readkey$, using L05300, descr$,eod goto L05310
L05300:      FMT POS(25), CH(30)
        rhh% = len(descr$)
        scr_msg$ = str(descr$,1%,rhh%)
        call "SHOSTAT" (scr_msg$)
        return
L05310:     if spec% = 5% then                                           ~
                   scr_msg$ = "            Glazing Bead Report          "
            if spec% = 6% then                                           ~
                   scr_msg$ = "             Re - Bar Report             "
            if spec% = 7% then                                           ~
                   scr_msg$ = "           Frame Snap In Report          "
            if spec% = 8% then                                           ~
                   scr_msg$ = "            Department Defined           "
                                                          /* (AWD007)   */
            if spec% = 10% then                                          ~
                   scr_msg$ = "            Special Sash Report          "

            call "SHOSTAT" (scr_msg$)
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



        exit_sub

        end

