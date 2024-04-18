*       ****************************************************************~
*                       NEW -( R e p o r t s )                         *~
*                                                                      *~
*                  ( As of 02/18/2013 - CMG                            *~
*        NTXLIN01 - Cut Sheets, including weld sizes                   *~
*                     North Texas Report                               *~
*                                                                      *~
*            Note - Primary Subroutine 'CALC_CUTS' at Line ( 3040 )    *~
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
*                     R   = Report Type 1, 2, 3, 4, 5                  *~
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
*                   New Equation Subroutines                           *~
*                     AWDCUTLD - Set Equation Pointers                 *~
*                     AWDCUTCC - Load and get Equations for Product    *~
*                                                                      *~
*----------------------------------------------------------------------*~
*Mod Date  ! Description                                           !By *~
*----------+-------------------------------------------------------+---*~
*02/18/2013! New Subroutine                                        !CMG*~
*03/13/2014!(AWD001) Added SDL Comment                             !PWW*~
*06/13/2014!(CUT001) mod to add dim fields to CUTCC                !MES*~
*06/13/2014!(AWD002) mod for SDL phantoms                          !MES*~
*06/11/2014!(AWD003) Added 3 new files (APCCUTF1),(APCCUTF2) &     !PWW*~
*          !(APCCUTF3 so that we could capture report data.        !   *~
*          ! CyberQuery will be used so that usere can format      !   *~
*          ! their own reports to their desire.                    !   *~
*10/06/2014!(AWD004) Added a purge routine to totally purge all    !PWW*~
*          !         records for this Prod Date & Dept. This should!   *~
*          !         resolve Issue #7649 where duplicate seq number!   *~
*          !         were showing for loads that were re-routed.   !   *~
*05/08/2017! (CR838) For 8/0 312,378,332,388 do not print door sash!MES*~ 
*07/19/2017!(CR1002) T/B cut indication for TX cut sheets          !MES*~
*04/23/2019!(CR1994) Add PlyGem Series to Cut Sheet                !MES*~
*05/15/2019!(CR2029) Sash Limiter word change                      !MES*~
************************************************************************

        sub "NTXLIN01" (scr_dte$,        /* Production Date           */ ~
                        prod_dte$,       /* Completion Date           */ ~
                        spec%,           /* 0%= ALL DEPT 5%-8%,10%=Spec*/~
                        scr_dept$,       /* Department Code           */ ~
                        #1,              /* (APCCUTWK) Cut Sheet Work */ ~
                        #3,              /* (GENCODES) Table File     */ ~
                        #4,              /* (AMTBOMIF) Validity File  */ ~
                        #5,              /* (HNYMASTR) Part Master    */ ~
                        #6,              /* (TXTFILE ) Text File      */ ~
                        #7,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #8 )             /* (AMTBOMCD) Equation File  */

        dim scr_dte$8, userid$3,         /* Production Date            */~
            scr_dept$3,                  /* Department Code            */~
            prod_dte$8,                  /* Completion Date            */~
            wrk_key1$51,                 /* Cut Sheet Work Key         */~
            wrk_key$51,                  /* Cut Sheet Work Key         */~
            wrk_rec$200,                 /* Cut Sheet Work Record      */~
            readkey$50,                  /* Code Table Key             */~
            part_desc$32,                /*                            */~
            apc_prt$60,                  /*                            */~
            apc_scr$120,                 /*                            */~
            apc_sze$20,                  /*                            */~
            sub_scr$120,                 /* Subpart Descr              */~
            sub_prt$60,                  /* Subpart Print Descr        */~
            descr$32,                    /* Code Table Description     */~
            scr_msg$40, title$40,        /* Header Text                */~
            sav_mod$3,                   /* Save Model Code            */~
            sav_cl$1,                    /* Save Color Code            */~
            sav_part$25,                 /* Save Part Number           */~
            sav_part_new$45,             /* Save New Part No.          */~
            sav_load$5,                  /* Save Load Number           */~
            sav_hg$2,                    /* Save Hinge                 */~
            model$3,                     /* Model Code                 */~
            cl$1,                        /* Color Code                 */~
            gls$2,                       /* Glass Code                 */~
            strength$10,                 /* Strength                   */~
            color$15,                    /* Color Description          */~
            hinge$15,                    /* hinge Description          */~
            dt_ref$8,                    /* Product Warranty Number    */~
            dt_seq$5,                    /* Production Sequence Number */~
            c_o$2,                       /* Cottage/Oriel              */~
            t_t$4,                       /* Twin/Triple                */~
            locks$3,                     /* Number of Locks            */~
            fin$3,                       /* With or Without Fin        */~
            fin_removal$30,              /* Fin Removal                */~
            addl_data$72,                /* Addl Data                  */~
            sill$1,                      /* Sill in SubPart            */~
            sillopt$20,                  /* Sill Option                */~
            dtl_load$5,                  /* Load Number                */~
            dtl_txt$4,                   /* Product Text Id            */~
            dtl_part$25,                 /* Part Number                */~
            dtl_sub_part$20,             /* New sub Part No            */~
            dtl_sub_info$20,             /* New Sub Info Fields        */~
            dtl_new_part$45,             /* New Part Number            */~
            prod$1,                      /* Product Code               */~
            xw$7,                        /* Converted Window Width     */~
            xh$7,                        /* Converted Window Height    */~
            tw$1,                        /* WIDTH CUT PARTS            */~
            th$1,                        /* HEIGHT CUT PARTS           */~
            tsw$1,                       /* Width Type Sub Part        */~
            tsh$1,                       /* Heigth Typ Sub Part        */~
            casing$1,                    /* casing opt subpart         */~
            col$(500%)25,                /* Cut Descriptions           */~
            eq$(500%)8,                  /* PRIMARY CROSS REF KEYS     */~
            ct$(500%)9,                  /* WIDTH AND HEIGHT CUTS      */~
            ct(500%),                    /* WIDTH AND HEIGHT CUTS-DECIM*/~
            sh$(500%)1,                  /* SASH TYPE ( W, H, N )      */~
            cr$(500%)10,                 /* RAW MATERIAL PARTS         */~
            cr_addl$(500%)5,             /* (AWD010) addl raw material */~
            cp$(500%)2,                  /* NUMBER OF PIECES TO CUT    */~
            cc$(500%)1,                  /* CUT PIECE (Y) OR (N)       */~
            f_c1$(500%)19,               /* Cut Descriptions for Report*/~
            f_c2$(500%)9,                /* Cuts Associated frame      */~
            f_c3$(500%)15,               /* Die Number                 */~
            s_c1$(500%)19,               /* Cut Descriptions for Report*/~
            s_c2$(500%)9,                /* Cuts Associated sash       */~   
            s_c3$(500%)15,               /* Die Number                 */~   
            o_c1$(500%)19,               /* Cut Descriptions for Report*/~
            o_c2$(500%)9,                /* Cuts Associated other      */~ 
            o_c3$(500%)15,               /* Die Number                 */~                 
            s_f$(500%)1,                 /* Sash / Frame               */~
            die$(500%)15,                /* Die Number                 */~
            width$(10%)9,                /* Width Size - Weld          */~
            height$(10%)9,               /* Height Size - Weld         */~
            adj(500%),                   /* Adjustment amt             */~
            tb_w$(500%)1,                /* T/B cut indication(CR1002) */~ 
            width_t$(10%)9,              /* Width - Weld Top (CR1002)  */~
            height_t$(10%)9,             /* Height - Weld Top (CR1002) */~  
            width_b$(10%)9,              /* Width - Weld Bot (CR1002)  */~
            height_b$(10%)9             /* Height - Weld Bot (CR1002) */~  

        dim sav_key1$11,                 /* Save Text Key              */~
            text_key$11,                 /* Text Key                   */~
            text$(2%)70,                 /* Text Description           */~
            text_desc$60, text_d$(2%)60, /* Text Description           */~
            textid$4,                    /* Text Id                    */~
            text_flag$1                  /* Text Flag                  */


        dim clmr$3,                      /* CLMR                       */~
            hg$2,                        /* HINGE                      */~
            sashacc$1,                   /* Sash Accessory             */~
            safety$15,                   /* Safety sash accessory      */~
            sz$100,                      /* Size                       */~
            calc$9,                      /* fraction size              */~
            f1_rec$(1)256, f2_rec$128, f1_key$48, f2_key$48,/*<AWD003> */~
            rec_type$1, f3_rec$128, f3_key$48               /*<AWD003> */
            
        dim salesorder$8,                /* Sales Order Number (CR1994)*/~
            soLne$3,                     /* SO Line            (CR1994)*/~
            l1desc$250,                  /* Line text 1                */~    
            l2descA$250,                 /* Line text                  */~
            l2descB$250,                 /* Line text part 2           */~           
            l1mutype$50,                 /* L1 Mu Type                 */~
            l3mulltype$25,               /* L3 Mull Type               */~
            pgdprating$10,               /* PlyGem DP Rating           */~
            pgcpd$30,                    /* PlyGem NFRC CPD            */~
            pgseries$25,                 /* PlyGem Series              */~ 
            pgperlbl$4,                  /* PlyGem Performance Label   */~ 
            pgbuyout$2,                  /* PlyGem Buyout product      */~
            pgthdline$3,                 /* PlyGem The Home Depot Line */~
            pgtdi$8,                     /* PlyGem TDI number          */~
            pgfla$12                     /* PlyGem Florida Approval    */            
            
        dim series_style$15, sash$1, door2$ 
/*<AWD003> +*/
        dim scr_deptx$3, scr_dtex$8, sash_width$16, sash_height$16,      ~
            scr_dtein$8, prod_dtex$8, prod_dtein$8, f1_rail_component$4
            
/*(CR1994)*/
        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */
            
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                       <AWD003>                            *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #10 ! APCCUTF1 ! Header File for Cut Sheet Data           *~
            * #11 ! APCCUTF2 ! Detail File for Cut Sheet Data           *~
            * #12 ! APCCUTF3 !                                          *~
            * #21 ! ORADESC2 ! WW PlyGem Desc                           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #10,  "APCCUTF1",                                     ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =    40                          
                       

            select #11,  "APCCUTF2",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   42 

            select #12,  "APCCUTF3",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   40 
                        
            select #21, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11            
                        

            if opened_this% = 1% then L10000
                call "OPENCHCK" (#10, 0%, 0%, 2000%, " ")
                call "OPENCHCK" (#11, 0%, 0%, 2000%, " ")
                call "OPENCHCK" (#12, 0%, 0%, 2000%, " ")
                call "OPENCHCK" (#21, fs%(21%), f2%(21%), 25%, rslt$(21%))
                opened_this% = 1%
L10000
            scr_dtex$ = scr_dte$
            date% = 0%
            call "DATEOK" (scr_dtex$, date%, errormsg$)
            if date% = 0% then stop
            call "DATUNFMT" (scr_dtex$)
            scr_dtein$ = scr_dtex$

            prod_dtex$ = prod_dte$
            date% = 0%
            call "DATEOK" (prod_dtex$, date%, errormsg$)
            if date% = 0% then stop
            call "DATUNFMT" (prod_dtex$)
            prod_dtein$ = prod_dtex$
            
/*AWD003*/  gosub purge_f1_f2_f3

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "               
           
            call "EXTRACT" addr("ID", userid$)

            pass% = 0%                   /* 1st Pass for Width         */
                                         /* 2nd Pass for Height        */
            gosub lookup_dpt_desc                                         
            scr_msg$ = " " & descr$ & " Cut Sheets "
            call "SHOSTAT" ("Print " & descr$ & " Cut Sheets ") 

            pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)

            init(" ") sav_cl$, wrk_rec$, sav_mod$, sav_part$, sav_load$, ~
                      sav_hg$
            dim1es, dim2es, dim3es = 0.00
            wrk_key$  = all(hex(00))
            wrk_key1$ = all(hex(00))
            read #1,key > wrk_key$, using L01630 , wrk_key1$, wrk_rec$, ~
                                                    eod goto print_a_done
            sav_mod$   = str(wrk_key1$,6%,3%)
            cl$        = str(wrk_key1$,9%,1%)

            gosub lookup_series_style
            gosub lookup_model
            gosub lookup_color
            sav_load$ = str(wrk_rec$,29%,5%)
            goto L01640
        print_a_next
            read #1, using L01630 , wrk_key1$, wrk_rec$,                   ~
                                                    eod goto print_a_done
L01630:       FMT POS(6), CH(51), CH(200)
L01640:
              model$    = str(wrk_key1$,6%,3%)      /* MODEL       */
              cl$       = str(wrk_key1$,9%,1%)      /* COLOR       */

            if spec% <> 7% then goto L01720        
               if scr_dept$ = "000" and str(model$,1%,1%) <> "0" then ~
                                        goto print_a_next

L01720:     dt_ref$= str(wrk_rec$,1%,8%)            /* REF NO      */
            dt_seq$= str(wrk_rec$,9%,5%)            /* SEQUENCE NO */
            
REM            if dt_seq$ <> "00033" then goto not_seq
            
REM             call "SHOSTAT" ("SEQ --> "& dt_seq$) stop
              
not_seq

            c_o$   = str(wrk_rec$,14%,2%)           /* COTTAGE/ORIE*/
            t_t$   = str(wrk_rec$,16%,4%)           /* TWIN/TRIPLE */
            t_b$   = str(wrk_rec$,20%,3%)           /* TSO, BSO,FSO*/
            locks$ = str(wrk_rec$,23%,3%)           /* LOCKS       */
            fin$   = str(wrk_rec$,26%,3%)           /* WITH FIN    */
            dtl_load$ = str(wrk_rec$,29%,5%)        /* LOAD NUMBER */
            dtl_txt$  = str(wrk_rec$,34%,4%)        /* TEXT ID     */
            dtl_part$ = str(wrk_rec$,38%,25%)       /* PART NO.    */
            gls$ = str(dtl_part$,5%,2%)

            clmr$ = str(dtl_part$,20,3)             /* CLMR        */
            hg$   = str(dtl_part$,9,2)              /* Hinge       */
            sash$ = str(dtl_part$,11,1)             /* Screen      */

            xw$ = str(wrk_rec$,63%,7%)              /* WIDHT       */
            xh$ = str(wrk_rec$,70%,7%)              /* HEIGHT      */
                                                   
            dtl_sub_part$ = str(wrk_rec$,81%,20%)
            sashacc$ = str(dtl_sub_part$,16%,1%)
            sill$ = str(dtl_sub_part$,18%,1%)            
            dtl_sub_info$ = str(wrk_rec$,101%,20%)
            salesorder$ = str(wrk_rec$,147%,8%)
            soLne$ = str(wrk_rec$,155%,2%)
            str(dtl_new_part$,1%,25%)  = dtl_part$
            str(dtl_new_part$,26%,20%) = dtl_sub_part$

            
            convert str(wrk_rec$,130%,8%) to dim1es, data goto badDim1es
badDim1es:
            convert str(wrk_rec$,139%,8%) to dim2es, data goto badDim2es
badDim2es:
            casing$ = str(dtl_sub_part$,6%,1%)
                                                   

            if cl$ <> sav_cl$ then gosub lookup_color
            if str(model$,1%,1%) <> "J" and sav_mod$ = model$    /*(CR1994)*/ ~
                   then skip_mdl_updte
               sav_mod$ = model$
               gosub lookup_series_style
               gosub lookup_model
skip_mdl_updte: 

            if sav_hg$ <> hg$ then gosub lookup_hinge 
            gosub lookup_sashacc           
            if len(dtl_part$) > 18 then goto L01890
               xw$, xh$ = "PART    "
               init(" ") c_o$, t_t$, t_b$, locks$, fin$

L01890:     gosub lookgls_thickness
            init(" ") fin_removal$, sillopt$, addl_data$
            if fin$ <> "YES" then gosub lookupFinRemoval
            if sill$ <> "0" then gosub lookupSillOpt
            addl_data$ = fin_removal$ & sillopt$
            
            total% = total% + 1%
            gosub detail_a
            goto print_a_next
        print_a_done
            print using L04340
            call "SETPRNT" ("APCC", " ", 0%, 1%)
            close printer
            goto exit_sub

                                                      

L02830: %!Seq.  Mod Color  Warranty   Load  <------ Part Description ----~
        ~--> !  <-------- Special Description ---------> ! HT1 - HT2 - WD!
L02850: %!----- --- ------ --------- -----  -----------------------------~
        ~--- !  ---------------------------------------- ! --------------!


        


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
            color$ = str(descr$,6%,15%)
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
                                                       
L03760: %! No Calculation for: #########################  ###############~
        ~#####                                                           !

L03855: %!################### die################:(#########)            ~
        ~         ################### die################: (#########)   !  
           
L03860: %!################### die################:(#########)            ~
        ~                                                                !  

L03880: %!TXT:###########################################################~
        ~#                                                               !

L03885: %!SCLMR:  ( ######### )                                          ~
        ~                                                                !
        
  REM            10        20        30        40        50        60   65
  REM    12345678901234567890123456789012345678901234567890123456789012345
L03910: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
        
L03915: %!                                                               ~
        ~                                                                !

L04010: %!Sq(#####)   Model:###      Series:###############   ###########~
        ~####  ###############  ###############  Lks:###  ##########     !  
L04020: %!######      W:#########    H:#########                         ~
        ~                                                                !
L04022: %!######  ###    W:#########    H:#########     ###  W:######### ~
        ~   H:#########                                                  ! 
/*(CR1002)*/        
L04025: %!Remainig Cuts                                                  ~
        ~                                                                !
L04030: %!Addl Data  ####################################################~
        ~####################                                            !        
/*<AWD001>  */        
L04032: %!SDL                                                            ~
        ~                                                                !  
/*<AWD001>  */                
        detail_a                                       /* C1$(), C2$() */
             addl_data% = 0%
             f2_seq% = 0                               /*<AWD003> */
             f1_head$ = "   "                          /*<AWD003> */
             f1_rail_component$ = "NNNN"               /*<AWD003> */
             if xw$ = "PART    " then goto no_calc
                                                       
                gosub calc_cuts
                if spec% = 8% and j% = 0% then return
                sav_part_new$ = dtl_new_part$

no_calc:

             if lcnt% > 57% then gosub header
                goto L04000
             if sav_load$ = dtl_load$ then goto L04000
                sav_load$ = dtl_load$
                gosub header

L04000:      
REM             print using L03910
             print using L04010, dt_seq$, model$, series_style$, color$, hinge$, ~
                                 safety$, locks$, strength$
                                 
             if sash$ = "4" or sash$ = "5" or sash$ = "6" then goto print_sash
             print using L04020, "Window", width$(1%), height$(1%)  /*Frame */
             lcnt% = lcnt% + 3%
             for m% = 1 to j%         /* j% number of frame parts */
               if f_c1$(m%) = " " then goto print_sash

               if lcnt% > 59% then gosub header
               if f_c1$(m%+1%) <> " " then                                      ~
               print using L03855, f_c1$(m%), "#" & f_c3$(m%), f_c2$(m%),       ~
                                 f_c1$(m%+1%), "#" & f_c3$(m%+1%), f_c2$(m%+1%) 
                                 
               if f_c1$(m%+1%) = " " then                                       ~
               print using L03860, f_c1$(m%), " #" & f_c3$(m%), f_c2$(m%)                                         
                                   
               rec_type$ = "1"                  /*<AWD003> (W)indow */
               f1_head$ = "1  "                 /*<AWD003> Header Flag 1 */
/*<AWD003>*/   gosub apccutf2_detail1
               if f_c1$(m%+1%) = " " then goto next_m1
               m% = m% + 1%
/*<AWD003>*/   gosub apccutf2_detail1
next_m1:       lcnt% = lcnt% + 1%
             next m%
print_sash:

             if s_c1$(1%) = " " then goto print_txt
             if lcnt% > 59% then gosub header

/*(CR838)*/  door80% = 0%
             door2$ = str(dtl_part$,1,3)
             if (door2$ = "312" or door2$ = "332")                          ~
                and str(dtl_part$,13,4) = "0946" then door80% = 1%
             if door80% = 1% and (sash$ <> "5" and sash$ <> "6")            ~
                 then goto print_other
/*(CR838)*/
             print using L03915
/*(CR1002)*/ if scr_dept$ <> "045" then goto print_sash1
             if scr_dept$ = "045" and (sash$ = "4" or sash$ = "5")           ~
                then goto print_sash1 /* sash only print 1 entry*/
             if sash2% = 0% then goto print_sash1   /* no T ot B only print 1*/
             if scr_dept$ = "045" then goto print_sash2
/*(CR1002) end*/             
             
print_sash1:  print using L04020, "Sash  ", width$(2%), height$(2%)  /*Sash*/
             goto print_scuts 

print_sash2: print using L04022, "Sash  ", "Top", width_t$(3%), height_t$(2%),    ~
                "Bot", width_b$(2%), height_b$(3%)/*Sash*/
                
print_scuts: lcnt% = lcnt% + 2%                                   /*(\CR1002)*/       
             for m% = 1 to k%         /* k% number of frame parts */
               if s_c1$(m%) = " " then goto print_other

               if lcnt% > 59% then gosub header
               if s_c1$(m%+1%) <> " " then                                      ~
               print using L03855, s_c1$(m%), "#" & s_c3$(m%), s_c2$(m%),       ~
                                 s_c1$(m%+1%), "#" & s_c3$(m%+1%), s_c2$(m%+1%)
                                 
               if s_c1$(m%+1%) = " " then                                       ~
               print using L03860, s_c1$(m%), "#" & s_c3$(m%), s_c2$(m%)
                                                                       
               rec_type$ = "2"                  /*<AWD003> (S)ash   */
               str(f1_head$,2%,1%) = "2"        /*<AWD003> Header Flag 2 */
/*<AWD003>*/   gosub apccutf2_detail2
               if s_c1$(m%+1%) = " " then goto next_m2
               m% = m% + 1%
/*<AWD003>*/   gosub apccutf2_detail2
next_m2:       lcnt% = lcnt% + 1%
             next m%
             
print_other:
             if o_c1$(1%) = " " then goto print_txt /* check to print other */
             if lcnt% > 59% then gosub header

             print using L04025
             lcnt% = lcnt% + 1%
             for m% = 1 to j%         /* k% number of frame parts */
               if o_c1$(m%) = " " then goto print_txt
  
               if lcnt% > 59% then gosub header
               if o_c1$(m%+1%) <> " " then                                       ~
               print using L03855, o_c1$(m%), "#" & o_c3$(m%), o_c2$(m%),        ~
                                 o_c1$(m%+1%), "#" & o_c3$(m%+1%), o_c2$(m%+1%)  
                                 
               if o_c1$(m%+1%) = " " then                                        ~
               print using L03860, o_c1$(m%), "#" & o_c3$(m%), o_c2$(m%)
                                                                   
               rec_type$ = "3"                  /*<AWD003> (O)ther  */
               str(f1_head$,3%,1%) = "3"        /*<AWD003> Header Flag 3 */
/*<AWD003>*/   gosub apccutf2_detail3
               if o_c1$(m%+1%) = " " then goto next_m3
               m% = m% + 1%
/*<AWD003>*/   gosub apccutf2_detail3
next_m3:       lcnt% = lcnt% + 1%
             next m%
print_txt:   

             if addl_data$ = " " then goto noFinInfo
             if lcnt% > 59% then gosub header             
                addl_data% = 1%
                print using L03915
                print using L04030, addl_data$
                rec_type$ = "4"                  /*<AWD003> Addl data*/
/*<AWD003>*/    gosub apccutf3_detail1
                str(f1_head$,4%,1%) = "4"        /*<AWD003> Header Flag 4 */
                lcnt% = lcnt% + 2%
/*<AWD001>  */
noFinInfo:
             if str(dtl_sub_part$,8,1) <> "1" then goto no_Sdl_Info
             if str(dtl_sub_part$,9,1) =  "2" then goto no_Sdl_Info             
                   print using L04032 
                   rec_type$ = "4"                  /*<AWD003> SDL    */
/*<AWD003>*/       gosub apccutf3_detail2
                   str(f1_head$,4%,1%) = "4"        /*<AWD003> Header Flag 4 */
                   lcnt% = lcnt% + 1%
/*<AWD001>  */
no_Sdl_Info:
/*<AWD003>*/    calc$ = "          "
                if hg$ < "70" or hg$ > "97" then goto notSpecified
                if clmr$ = "000" then goto notSpecified
                
                if lcnt% > 59% then gosub header                
REM                 call "SHOSTAT" (" I AM HERE" & dt_seq$)  stop
                    convert clmr$ to clmr, data goto notSpecified
                    clmr = clmr / 10.0
                    a% = int(clmr) : b = (clmr - a%) / 0.8
                    clmr = a% + b
                    calc = clmr

                    gosub con_fract
                    if addl_data% = 0% then print using L03915
                    if addl_data% = 0% then print using L04030, " "
                    print using L03885, calc$ 
                    rec_type$ = "4"                  /*<AWD003> calc$   */
/*<AWD003>*/        gosub apccutf3_detail3
                    str(f1_head$,4%,1%) = "4"        /*<AWD003> Header Flag 4 */
                    if addl_data% = 0% then lcnt% = lcnt% + 3% ~
                      else lcnt% = lcnt% + 1%
                    
notSpecified:
/*<AWD003>+ */
             for m% = 1% to 4%
               if str(f1_head$,m%,1%) = " " then goto next_head
               rec_type$ = str(f1_head$,m%,1%)
               gosub apccutf1_header
next_head:   next m%
/*<AWD003>- */
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

            title$ = scr_msg$
            call "FMTTITLE" (title$, " ", 12%)

          pageno% = pageno% + 1%
          print page
          print using L04340
          print using L04370, scr_dte$, prod_dte$
          print using L04400, dtl_load$, scr_msg$, pageno%
          print using L03910
          lcnt% = 4%
          sav_load$ = dtl_load$
          if pass% < 2% then return
             print using L03910                   /* Tube Winding Header */
             print using L04430, userid$
             print using L03910
             print using L02830
             print using L02850
             lcnt% = lcnt% + 5%
        return

        calc_cuts
          cw%, ch%, csw%, csh%, w%, h% = 0%
          tw$ = "1" : th$ = "2"
          init(" ") f_c1$(), f_c2$(), f_c3$(), s_c1$(), s_c2$(), s_c3$(),  ~
                    o_c1$(), o_c2$(), o_c3$(), prod$
          prod$ = str(dtl_part$,1%,1%)
          casing% = 0%                           /* Convert Blank      */
          convert casing$ to casing%, data goto CALC_CUT1
CALC_CUT1:
          convert casing% to casing$, pic(#)

          gosub get_subpart_cuts
          j%, k%, l% = 0%                                 
          x% = 1%
          eq% = cw% + ch% + csw% + csh%           
          gosub calc_subpart_cuts

          for i% = x% to eq%
              cc$ = cc$(i%)
              if cc$(i%) = " " then goto L04930   /* Skip - No Cross-Ref */
                 s_f$ = s_f$(i%)
                 if s_f$(i%) = "F" then j% = j% + 1%   /*  Defined for Part   */
                 if s_f$(i%) = "F" then f_c1$(j%) = str(col$(i%),1%,25%)
                 if s_f$(i%) = "F" then f_c2$(j%) = ct$(i%)
                 if s_f$(i%) = "F" then f_c3$(j%) = die$(i%)
                 
                 if s_f$(i%) = "S" then k% = k% + 1%   /*  Defined for Part   */
                 if s_f$(i%) = "S" then s_c1$(k%) = str(col$(i%),1%,25%)
                 if s_f$(i%) = "S" then s_c2$(k%) = ct$(i%)
                 if s_f$(i%) = "S" then s_c3$(k%) = die$(i%) 

                 
                 if s_f$(i%) <> "F" and s_f$(i%) <> "S" then l% = l% + 1%
                 if s_f$(i%) <> "F" and s_f$(i%) <> "S" then          ~
                                         o_c1$(l%) = str(col$(i%),1%,25%)
                 if s_f$(i%) <> "F" and s_f$(i%) <> "S" then          ~
                                                      o_c2$(l%) = ct$(i%)  
                 if s_f$(i%) <> "F" and s_f$(i%) <> "S" then          ~
                                                      o_c3$(l%) = die$(i%)     
                                                      
                 if sh$(i%) = "W" or sh$(i%) = "H" then goto L50000                                                                      
                           
L04930:   next i%
        return
/* Weld Sizes 1 = frame 2 = sash    */    
L50000:     calc = 0.00                        /* substract burn off    */
            ct  = ct(i%)
            adj = adj(i%)
            calc = ct(i%) + adj(i%)            /* Welder Sizes are in   */
                                               /* Decimal               */
            sash2% = 0%                                   
            gosub con_fract
            if sh$(i%) = "W" then w% = w% + 1%
            if sh$(i%) = "H" then h% = h% + 1%
            
            if (sash$ = "4" or sash$ = "5" or sash$ = "6") and sh$(i%) = "W" ~
                 then w% = w% + 1%
            if (sash$ = "4" or sash$ = "5" or sash$ = "6") and sh$(i%) = "H" ~
                 then h% = h% + 1%
                 
            if sh$(i%) = "W" then width$(w%)  = calc$
            if sh$(i%) = "H" then height$(h%) = calc$   
/*(CR1002)*/
           
            if tb_w$(i%) = "T" or tb_w$(i%) = "B" then sash2%=1%
            /*if not set up then it will not put bad information*/
             if sash2% = 0% then L04930
         
            if sh$(i%) = "W" and tb_w$(i%) = "T" then width_t$(w%)  = calc$
            if sh$(i%) = "W" and tb_w$(i%) = "B" then width_b$(w%)  = calc$
            if sh$(i%) = "H" and tb_w$(i%) = "T" then height_t$(h%)  = calc$
            if sh$(i%) = "H" and tb_w$(i%) = "B" then height_b$(h%)  = calc$
/*(CR1002) end*/            
            
            goto L04930
                   

        get_subpart_cuts
           tw$ = "1" : th$ = "2"
           if casing$ <> "1" then goto not_casing
              tsw$ = "K" : tsh$ = "L"

not_casing
           if casing$ <> "2" then goto not_bullnose
              tsw$ = "M" : tsh$ = "N"

not_bullnose
           call "AWDCUTLD" (prod$, cw%, ch%, csw%, csh%, tw$, th$, tsw$, tsh$,~
                             #3, e% )
        return

        calc_subpart_cuts                                /* (AWD010) */
        mfg% = 0%                                        /*(AWD002)*/
        if str(dtl_sub_part$,8,1) = "1" and str(dtl_sub_part$,9,1) <> "2" ~
                  then mfg% = 3%   
        call "AWDCUTCC" ( dtl_new_part$, dim1es, dim2es, dim3es, /*(CUT001)*/~
                             mfg%, cw%, ch%, csw%, csh%,                ~
                             eq$(), ct$(), cr$(), cr_addl$(), cp$(),    ~
                             cc$(), col$(), ct(), sh$(), tw$, th$,      ~
                             s_f$(), die$(), adj(), tb_w$(), #7, #8, #3, e%)
                             /*(CR1002)*/
        return
                                                         /* (AWD006)     */

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
        
        lookup_dpt_desc
          readkey$ = all(hex(00))
          readkey$ = "PLAN DEPT" & scr_dept$
          call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )        
        return
        
        lookup_series_style
          init(" ") series_style$
          if str(sav_mod$,1%,1%) = "J" then goto plygem_series    /*(CR1994)*/
          readkey$ = all(hex(00))        : REM 08 Atrium Brand
          readkey$ = "ELLISON05" & sav_mod$ & "08"  
          
          call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
          series_style$ = str(descr$,1,15)
          goto seriesDone
          
        plygem_series                                            /*(CR1994)*/
          gosub readOradesc2
           series_style$ = str(pgseries$,1,14)
          
          
        seriesDone  
        return
        
        lookup_hinge                                 
            sav_hg$ = hg$
            readkey$ = all(hex(00))
            readkey$ = "HINGE    " & hg$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            p% = pos(descr$ = "-")
            hinge$ = str(descr$,(p%+2%),15%)
            if x% = 0% then init(" ") hinge$ 
            if hg$ = "00" then init(" ") hinge$ 
        return
        
        lookup_sashacc
            init(" ") safety$                            
            if sashacc$ = "0" then return
            readkey$ = all(hex(00))
            readkey$ = "SASHACC  " & sashacc$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            safety$ = str(descr$,1%,15%)
            if str(model$,1%,1%) <> "J" then goto not1100         /*CR2029*/
            if sashacc$ = "2" then safety$ = "WOCD Install  "
            if sashacc$ = "4" then safety$ = "WOCD Prepare  "
not1100:            
            if x% = 0% then init(" ") safety$ 
        return        

        lookgls_thickness
            triple% = 0%
            readkey$ = "PLANTRIPL" & gls$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            if x% <> 0% then triple% = 1%       
            if triple% = 1% then goto get_desc2
            
            readkey$ = "TEMP GED " & gls$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            if x% <> 0% then goto get_desc2

            readkey$ = "OBS GED  " & gls$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            if x% <> 0% then goto get_desc2

            readkey$ = "GED 001  " & gls$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            if x% <> 0% then goto get_desc2           
        return
        get_desc2
          strength$ = "SB GlS"
          if str(descr$,3%,1%) = "4" then strength$ = "DS GLS"
          if triple% = 1% then strength$ = "TR GLS"     
        return
        
        lookupSillOpt
            readkey$ = "SILLOPT  " & sill$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )        
            sillopt$ = "SILL: " & descr$
        return
                
        lookupFinRemoval
            readkey$ = "FIN      " & str(dtl_sub_info$,4%,1%)
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )        
            fin_removal$ = "FIN: " & descr$
        return

/*CR1994*/
        readOradesc2
              linetypeid% = 0%
              pgufactor%  = 0%
              pgshgc%     = 0%
              pgvistrans% = 0%
              init(" ") l1desc$, l2descA$, l2descB$, l1mutype$, ~
                        l3mulltype$, pgdprating$,               ~
                        pgcpd$, pgseries$, pgperlbl$,           ~
                        pgbuyout$, pgthdline$, pgtdi$, pgfla$
                        
              init(" ") readkey$
              solne% = 0%
               convert soLne$ to solne%, data goto badLine

               convert solne% to soLne$, pic(##0)


              
              str(readkey$,01%,08%) = salesorder$
              str(readkey$,09%,03%) = soLne$
              
              read #21, key = readkey$, using ORADESC2_FMT,                   ~ 
                              or_width, or_height, l1desc$,                   ~
                              l2descA$, l2descB$, linetypeid%, l1mutype$,     ~
                              l3mulltype$, pgdprating$, pgufactor%, pgshgc%,  ~
                              pgvistrans%, pgcpd$, pgseries$, pgperlbl$,      ~
                              pgbuyout$, pgthdline$,                          ~
                              pgtdi$, pgfla$,                                 ~ 
                                                         eod goto oradesc2Done

ORADESC2_FMT:     FMT POS(55), PD(14,4), PD(14,4), CH(250), CH(250), CH(250), ~
                        CH(02), CH(50), CH(25), CH(10), PD(14,4), PD(14,4),   ~
                        PD(14,4), CH(30), CH(25), CH(04), CH(02), CH(03),     ~
                        CH(08), CH(12)    
 badLine:   
         solne% = 0% 
          oradesc2Done
          return


        REM *************************************************************~
            *  Subroutines to extract cut sheet date and write to       *~
            *  apccutf1(header) & apccutf2(detail) & apccutf3(detail)   *~
            *  AWD003                                                   *~
            *************************************************************
        apccutf1_header
            init (" ") f1_rec$()
            sash_only% = 0
            if sash$ = "4" or sash$ = "5" or sash$ = "6" then sash_only% = 1

            str(f1_rec$(),1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f1_rec$(),7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
            str(f1_rec$(),10%,2%) = str(wrk_rec$,165%,2%)      /*shift*/
            str(f1_rec$(),12%,5%) = dtl_load$                  /*Load Number*/
            str(f1_rec$(),17%,5%) = str(wrk_rec$,9%,5%)        /*seq*/
            str(f1_rec$(),22%,18%) = str(wrk_rec$,147%,18%)    /*bar code*/
            scr_deptx$ = scr_dept$     /*temp*/
            str(f1_rec$(),40%,1%) = rec_type$                  /*rec type*/
            str(f1_rec$(),41%,8%) = str(wrk_rec$,1%,8%)        /*REF NO*/
            str(f1_rec$(),49%,25%) = str(wrk_rec$,38%,25%)     /*Part No*/
            str(f1_rec$(),74%,20%) = str(wrk_rec$,81%,20%)     /*Sub Part*/
            if sash_only% = 1  or rec_type$ > "3" then goto N10000
            str(f1_rec$(),114%,9%) =  str(width$(1%),1%,9%)    /*Frame Width*/
            str(f1_rec$(),123%,9%) = str(height$(1%),1%,9%)    /*Frame Height*/
N10000
            str(f1_rec$(),132%,30%)= scr_msg$                  /*Header Text */
            str(f1_rec$(),162%,15%)= hinge$               /*Hinge Description*/
            str(f1_rec$(),177%,3%) = locks$           /* Number of Locks */
            str(f1_rec$(),180%,10%)= strength$         /* Strength */
            str(f1_rec$(),190%,6%) = prod_dtein$        /*Completion Date*/

            if s_c1$(1%) = " " or rec_type$ > "3" then goto N10010
            if rec_type$ <> "2" then goto N10010
            sash_width$ = str(width$(2%),2%,9%)    /*temp*/
            sash_height$ = str(height$(2%),2%,9%)    /*temp*/
/*          str(f1_rec$(),128%,9%) = str(width$(2%),2%,9%)     Sash Width*/
/*          str(f1_rec$(),137%,9%) = str(height$(2%),2%,9%)    Sash Height*/
/*  Made a decission to have up to 3 header records, one each for ...    ~
              rec_type$ --->  1=(W)indow, 2=(S)ash and 3=(O)ther  */
              
            str(f1_rec$(),114%,9%) = str(width$(2%),2%,9%)   /*Sash Width*/
            str(f1_rec$(),123%,9%) = str(height$(2%),2%,9%)  /*Sash Height*/
N10010:     str(f1_rec$(),196%,1%) = str(f1_rail_component$,m%,1%)
            f1_key$ = str(f1_rec$(),1%,40%)
            read #10, hold, key = f1_key$, using f1_fmt, f1_key$, eod  ~
                                           goto new_f1
f1_fmt:     FMT POS(1), CH(40)
f1_fmta:    FMT 1*CH(256)
f2_fmt:     FMT POS(1), CH(42)
f2_fmta:    FMT CH(128)
f3_fmt:     FMT POS(1), CH(40)
f3_fmta:    FMT CH(128)
            rewrite #10, using f1_fmta, f1_rec$()
            return
            
        new_f1
            write #10, using f1_fmta, f1_rec$()
            return
                                      
        apccutf2_detail1
            init (" ") f2_rec$
            str(f2_rec$,1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f2_rec$,7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
            str(f2_rec$,10%,2%) = str(wrk_rec$,165%,2%)      /*shift*/
            str(f2_rec$,12%,5%) = dtl_load$                  /*Load Number*/
            str(f2_rec$,17%,5%) = str(wrk_rec$,9%,5%)        /*seq*/
            str(f2_rec$,22%,18%) = str(wrk_rec$,147%,18%)    /*bar code*/
            str(f2_rec$,40%,1%) = rec_type$                  /*rec type*/
            f2_seq% = f2_seq% + 1
            convert f2_seq% to str(f2_rec$,41%,2%), pic(00)    /*detail seq*/
            str(f2_rec$,43%,19%) = f_c1$(m%)              /*cut description*/
            str(f2_rec$,62%,15%) = f_c3$(m%)                   /*die # */
            str(f2_rec$,106%,9%) = f_c2$(m%)                    /*cuts frame*/
            if pos(f_c1$(m%) = "rail") = 0% then goto no_f2_1_rail
            str(f1_rail_component$,1%,1%) = "Y"
            str(f2_rec$,115%,1%) = "Y"
        no_f2_1_rail
            f2_key$ = str(f2_rec$,1%,42%)
            read #11, hold, key = f2_key$, using f2_fmt, f2_key$, eod  ~
                                           goto new_f2_1
            rewrite #11, using f2_fmta, f2_rec$
            return
            
        new_f2_1
            write #11, using f2_fmta, f2_rec$
            return
            
        apccutf2_detail2
            init (" ") f2_rec$
            str(f2_rec$,1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f2_rec$,7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
            str(f2_rec$,10%,2%) = str(wrk_rec$,165%,2%)      /*shift*/
            str(f2_rec$,12%,5%) = dtl_load$                  /*Load Number*/
            str(f2_rec$,17%,5%) = str(wrk_rec$,9%,5%)        /*seq*/
            str(f2_rec$,22%,18%) = str(wrk_rec$,147%,18%)    /*bar code*/
            str(f2_rec$,40%,1%) = rec_type$                  /*rec type*/
            f2_seq% = f2_seq% + 1
            convert f2_seq% to str(f2_rec$,41%,2%), pic(00)    /*detail seq*/
            str(f2_rec$,43%,19%) = s_c1$(m%)              /*cut description*/
            str(f2_rec$,62%,15%) = s_c3$(m%)                   /*die # */
            str(f2_rec$,106%,9%) = s_c2$(m%)                    /*cuts frame*/
            if pos(s_c1$(m%) = "rail") = 0% then goto no_f2_2_rail
            str(f1_rail_component$,2%,1%) = "Y"
            str(f2_rec$,115%,1%) = "Y"
        no_f2_2_rail
            f2_key$ = str(f2_rec$,1%,42%)
            read #11, hold, key = f2_key$, using f2_fmt, f2_key$, eod  ~
                                           goto new_f2_2
            rewrite #11, using f2_fmta, f2_rec$
            return
            
        new_f2_2
            write #11, using f2_fmta, f2_rec$
            return

        apccutf2_detail3
            init (" ") f2_rec$
            str(f2_rec$,1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f2_rec$,7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
            str(f2_rec$,10%,2%) = str(wrk_rec$,165%,2%)      /*shift*/
            str(f2_rec$,12%,5%) = dtl_load$                  /*Load Number*/
            str(f2_rec$,17%,5%) = str(wrk_rec$,9%,5%)        /*seq*/
            str(f2_rec$,22%,18%) = str(wrk_rec$,147%,18%)    /*bar code*/
            str(f2_rec$,40%,1%) = rec_type$                  /*rec type*/
            f2_seq% = f2_seq% + 1
            convert f2_seq% to str(f2_rec$,41%,2%), pic(00)    /*detail seq*/
            str(f2_rec$,43%,19%) = o_c1$(m%)              /*cut description*/
            str(f2_rec$,62%,15%) = o_c3$(m%)                   /*die # */
            str(f2_rec$,106%,9%) = o_c2$(m%)                    /*cuts frame*/
            if pos(o_c1$(m%) = "rail") = 0% then goto no_f2_3_rail
            str(f1_rail_component$,3%,1%) = "Y"
            str(f2_rec$,115%,1%) = "Y"
        no_f2_3_rail
            f2_key$ = str(f2_rec$,1%,42%)
            read #11, hold, key = f2_key$, using f2_fmt, f2_key$, eod  ~
                                           goto new_f2_3
            rewrite #11, using f2_fmta, f2_rec$
            return
            
        new_f2_3
            write #11, using f2_fmta, f2_rec$
            return

        apccutf3_detail1
            init (" ") f3_rec$
            str(f3_rec$,1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f3_rec$,7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
            str(f3_rec$,10%,2%) = str(wrk_rec$,165%,2%)      /*shift*/
            str(f3_rec$,12%,5%) = dtl_load$                  /*Load Number*/
            str(f3_rec$,17%,5%) = str(wrk_rec$,9%,5%)        /*seq*/
            str(f3_rec$,22%,18%) = str(wrk_rec$,147%,18%)    /*bar code*/
            str(f3_rec$,40%,1%) = rec_type$                  /*rec type*/
            str(f3_rec$,43%,72%) = addl_data$        /*additional data*/
            f3_key$ = str(f3_rec$,1%,40%)
            read #12, hold, key = f3_key$, using f3_fmta, f3_rec$, eod  ~
                                           goto new_f3_1
            str(f3_rec$,43%,72%) = addl_data$        /*additional data*/
            rewrite #12, using f3_fmta, f3_rec$
            return
            
        new_f3_1
            write #12, using f3_fmta, f3_rec$
            return
            
        apccutf3_detail2
            init (" ") f3_rec$
            str(f3_rec$,1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f3_rec$,7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
            str(f3_rec$,10%,2%) = str(wrk_rec$,165%,2%)      /*shift*/
            str(f3_rec$,12%,5%) = dtl_load$                  /*Load Number*/
            str(f3_rec$,17%,5%) = str(wrk_rec$,9%,5%)        /*seq*/
            str(f3_rec$,22%,18%) = str(wrk_rec$,147%,18%)    /*bar code*/
            str(f3_rec$,40%,1%) = rec_type$                  /*rec type*/
            str(f3_rec$,115%,4%) = "*SDL"             /*SDL Info */
            f3_key$ = str(f3_rec$,1%,40%)
            read #12, hold, key = f3_key$, using f3_fmta, f3_rec$, eod  ~
                                           goto new_f3_2
            str(f3_rec$,115%,4%) = "*SDL"             /*SDL Info */
            rewrite #12, using f3_fmta, f3_rec$
            return
            
        new_f3_2
            write #12, using f3_fmta, f3_rec$
            return
            
        apccutf3_detail3
            init (" ") f3_rec$
            str(f3_rec$,1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f3_rec$,7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
            str(f3_rec$,10%,2%) = str(wrk_rec$,165%,2%)      /*shift*/
            str(f3_rec$,12%,5%) = dtl_load$                  /*Load Number*/
            str(f3_rec$,17%,5%) = str(wrk_rec$,9%,5%)        /*seq*/
            str(f3_rec$,22%,18%) = str(wrk_rec$,147%,18%)    /*bar code*/
            str(f3_rec$,40%,1%) = rec_type$                  /*rec type*/
            str(f3_rec$,119%,9%) = calc$             /*fraction size  */
            f3_key$ = str(f3_rec$,1%,40%)
            read #12, hold, key = f3_key$, using f3_fmta, f3_rec$, eod  ~
                                           goto new_f3_3
            str(f3_rec$,119%,9%) = calc$             /*fraction size  */
            rewrite #12, using f3_fmta, f3_rec$
            return
            
        new_f3_3
            write #12, using f3_fmta, f3_rec$
            return
            
        REM *************************************************************~
            *  Subroutine to purge all records in all 3 APCCUTF files   *~
            *  for this particular Prod Date and Department. There may  *~
            *  be records in these files that no longer apply.          *~
            *                   AWD004                                  *~
            *************************************************************
        purge_f1_f2_f3  
            init (" ") f1_rec$(), f2_rec$, f3_rec$, f1_key$, f2_key$, f3_key$

            str(f1_key$,1%,6%) = scr_dtein$                  /*Prod Date*/
            str(f1_key$,7%,3%) = str(scr_dept$,1%,3%)        /*dept*/
        purge_next_f1
            read #10, hold, key > f1_key$, using f1_fmt, f1_key$, eod  ~
                                  goto purge_f1_f2_f3_done
            if str(f1_key$,1%,6%) <> scr_dtein$ then                   ~
                                     goto purge_f1_f2_f3_done
            if str(f1_key$,7%,3%) <> str(scr_dept$,1%,3%) then         ~
                                     goto purge_f1_f2_f3_done
           delete #10 
           gosub purge_f2
           gosub purge_f3
           goto purge_next_f1

        purge_f1_f2_f3_done
           return
           
        purge_f2
           f2_key$ = all(hex(00))
           f2_key$ = str(f1_key$,1%,39%)
        purge_next_f2
           read #11,hold,key > f2_key$, using f2_fmt, f2_key$,           ~
                               eod goto end_purge_f2
           if str(f2_key$,1%,39%) <> str(f1_key$,1%,39%) then goto end_purge_f2
           delete #11
           goto purge_next_f2
           
        end_purge_f2
           return
           
        purge_f3
           f3_key$ = all(hex(00))
           f3_key$ = str(f1_key$,1%,39%) & "4"
           read #12,hold,key = f3_key$, using f3_fmt, f3_key$,            ~
                               eod goto end_purge_f3
           delete #12
        end_purge_f3
           return


        exit_sub

        end

