        REM *************************************************************~
            *            (RHHTEST)    Test Print Turned (off)           *~
            *                                                           *~
            *      Note- Bay-Bow Test for Line = 25%  in yy$(??)        *~
            *      Note- UPC Test for Line     = 86% (Turned Off)       *~
            *                                        (AWD028)           *~
            *      Note- 100% Inspection test for Line = 27% in yy$(??) *~
            *                                 and Line = 102% (Text)    *~
            *  Subroutine Name   - EWDPLS43                             *~
            *  Creation Date     - 04/08/99                             *~
            *  Last Modified Date- 10/17/2007                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Mod's By     - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      production label.                    *~
            *                                                           *~
            *                      Print File  = MFGPROD                *~
            *                      Script File = MFGPROD                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLS43 - Generates the label format and data to print   *~
            *            production labels. The resulting file is routed*~
            *            to the label printer via a script.             *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                     9% - Schema Lookup Error              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *07/02/2018! CR1598 New McCoy stock labels            ! RDB *~
            *08/27/2019! CR2192 New Layout to McCoy label         ! RDB *~
            *06/21/2020! CR2594 Remove the actual size            ! RDB *~
			*03/10/2023! CR3273 New black 150 series SH           ! RDB *~	
            *07/03/2023! CR3344 New V clay color code             ! RDB *~            
            *************************************************************

        sub "EWDPLS43" (been_here%,      /* Zero (Only 1st Time)       */~
                        rec$(),          /* Prod Label Data    (PAR002)*/~
                        #1,              /* GENCODES           (EWD018)*/~
                        #2,              /* APCPLNDT           (EWD022)*/~
                        #4,              /* BCKLINES           (AWD007)*/~
                        #3,              /* BCKSUBPT           (PAR000)*/~
                        #7,              /* AWDSKUXR           (PAR000)*/~
                        #8,              /* AWDPCMST           (EWD022)*/~
                        sc_sel$,         /* Label Type         (PAR000)*/~
                        sc_clr$,         /* Color Code                 */~
                        lbl%,            /* Label Type         (PAR000)*/~
                        error%)          /* Return Code                */
      
        dim                                                              ~
            tmp_bar$18,                  /* (PAR003) Schema Switch     */~
            schema$8,                    /* (PAR003) Schema Switch     */~
            a$128, b$128,                /* Print Lines for Label      */~
            lbl$(60%)80,                 /* Label Data Array   (EWD001)*/~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            lb_text$210,                 /* Mfg. Text Holding  (EWD001)*/~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            dq$1,                        /* double quotes              */~
            sq$1,                        /* single quotes              */~
            dt_key0$23,                  /* Primary Key        (EWD022)*/~
            dt_seq$5,                    /* Sequence No.       (EWD022)*/~
            rec$(4%)256                  /* Rec Array Prod. Lbl(PAR002)*/
       
       dim  flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            nominal$7, p_nominal$35,     /* Nominal Size               */~
            dt_cust$9,                   /* Customer code              */~            
            dt_sub_part$20,              /* New Sub Part No.           */~
            dt_sub_info$20,              /* New Sub Info Fields (9)+11 */~
            series$16, style$10          /* series & style             */
 
        dim lb_mdl$3,                    /* Model No.                  */~
            lb_send$21, qty_of$10,       /* Send-To Info (Mfg.)       */~
            lb_txt$(4%)30,               /* Mfg. Text Array    (EWD001)*/~
            lb_serno$8,                  /* Series No.                 */~
            lb_bc$18,                    /* Production Barcode ID      */~
            lb_sernm$10,                 /* Private Label Name         */~
            lb_seq$5,                    /* Sequence Number            */~
            lb_cust$19,                  /* Customer Name              */~
            lb_po$16,                    /* Purchase Order No.         */~
            lb_itmno$3,                  /* Item No. (X in X of Y)     */~
            lb_itmtot$3,                 /* Item Total (Y in X of Y)   */~
            lb_so$8,                     /* Sales Order No.            */~
            lb_city$18,                  /* Customer's City            */~
            lb_state$2,                  /* Customer's State           */~
            lb_cont$16,                  /* Contractor                 */~
            lb_job$10,                   /* Job No./Name               */~
            lb_room$7,                   /* Room Name                  */~
            lb_nomsz$6,                  /* Nominal Size               */~
            lb_opnsz$19,                 /* Opening Size               */~
            temp$20,                     /* Holding for opening size   */~
            lb_exsz$19,                  /* Exact Size                 */~
            lb_info$(2%)18, prod_desc$77,/* Other Info Array   (PAR002)*/~
            lb_oth1$24,                  /* New Sub Part No.   (PAR002)*/~
            lb_oth2$40,                  /* New Sub Part Descr (PAR002)*/~
            lb_sku$15,                   /* SKU No.                    */~
            lb_duedt$10,                 /* Due Date (Formatted)       */~
            lb_part$25,                  /* Mfg. Part No.              */~
            op_part$25,                  /* Mfg. Part No.              */~
            lb_drop$2,                   /* Drop Code                  */~
            lb_load$5,                   /* Load Code                  */~
            lb_makedt$10,                /* Make Date (Formatted)      */~
            lb_wnty$10,                  /* Warranty ID                */~
            lb_upc$11,                   /* UPC No.                    */~
            lb_dept$3,                   /* Department Code            */~
            lb_pms$5,                    /* Pull/Make/Stock            */~
            lb_wood$6,                   /* Wood Surround Code         */~
            lb_w$1,                      /* (EWD004) W or Blank        */~
/*AWD001*/  part$25,                     /* For Bay Bow        (EWD007)*/~
/*AWD001*/  model$3,                     /* Sku Model                  */~
/*AWD001*/  sku_key$16,                  /* Sku Key                    */~
            lb_samp$1,                   /* 0=No,1=Samp,2=Disp (EWD008)*/~
            lb_foam$1,                   /* (Y)es or (N)o      (EWD008)*/~
            lb_fin$1,                    /* (Y)es or (N)o      (EWD010)*/~
            lb_frame$1,                  /* Frame (Y)es or (N)o(EWD014)*/~
            lb_cust_code$9,              /* Customer Code      (EWD018)*/~
            lb_inspect$6,                /* 100% Inspection    (EWD018)*/~
            lb_config$2,                 /* Config code        (EWD021)*/~
            lb_config_txt$17,            /* Config String      (EWD021)*/~
            lb_config_txt3$17,           /* Line (3)           (EWD022)*/~
            lb_mull$3,                   /* Save Mull Code     (AWD025)*/~
            lb_screen$1,                 /* Save Screen Code   (AWD025)*/~
            testdate$10,                 /* Calulate Day       (EWD024)*/~
            day$1, frac$(8)4,            /* Day of the Week    (EWD024)*/~
            lb_sub_info$10,              /* New Info Fields    (PAR002)*/~
            lb_ups$1,                    /* UPS Flag -         (AWD031)*/~
            lb_specialmull$1,            /* Special Mull Code  (PAR006)*/~
            yy$(110%)128,                /* Buffer                     */~
            xx$(110%)128,                /* Buffer                     */~
            l_model$12,                  /* UPC                        */~
            l_desc$70,                   /* label desc                 */~
            clr_desc$7,                  /* Label Color description    */~
            gls_desc$22,                 /* Glass description          */~
            sc_sel$2,                    /* Screen Sel                 */~
            exSize$30,                   /* Area to center Exact Size  */~
/* CR2594   acSize$30,                      Area to center Actual Size */~
            ibeg$5,                      /* Intermec Beg Command       */~
            iend$5                       /* Intermec End Command       */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate Production Labels        "
            pname$ = "EWDPLS43 - Rev: R2.00"

        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! MFGLOWE  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "MFGMCCOY", varc, consec, recsize =   128


            twice% = 0%                           /* (EWD009)      */
                                                  /* Flag to print */
                                                  /* Two Labels    */
L10000:                                           /* (EWD009)      */
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            sel% = error%
            error%     = 0%
            nbr_lines% = 0%
            fs$ = "^FS"
            ibeg$ = "^XA"
            iend$ = "^FS"

            frac$(1) = "    "
            frac$(2) = "-1/8"
            frac$(3) = "-1/4"
            frac$(4) = "-3/8"
            frac$(5) = "-1/2"
            frac$(6) = "-5/8"
            frac$(7) = "-3/4"
            frac$(8) = "-7/8"
                                                  /* Fix Bay/Bow Prob */
                                                  /* (03/15/06)       */


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
            init(" ") xx$()
            if been_here% > 0% then goto L01000
               gosub load_label           /* Build Label Format yy$() */
               gosub set_file_name        /* Create Label Print File  */
               gosub open_file            /* Open Label Print file    */

L01000:        if sel% = 99% then goto exit_print

                  been_here% = been_here% + 1%  
                  copy yy$() to xx$()
                  gosub begin_process

                  goto exit_sub

        set_file_name                            
            init(" ") file$, script$
                                              
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */

            if err% = 0% then goto SS_1
               errormsg$ = "(Error) Schema Lookup Error)"
               gosub error_prompt
               error% = 9%
               end

SS_1:   
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "MFGMCCOY"
               script$  = "MFGMCCOY"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto fileset
                                                     /* North East      */
SS_2:
               file$    = "NEMCCOY"
               script$  = "NEMCCOY"

               library$ = "NEDATA  "
               volume$  = "NE    "

fileset:

        return                                    /* (PAR002)        */

        open_file
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists
               if comp% <> 16% then goto exit_sub
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return


        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process                             
        
         get str(rec$()), using L35000, lb_date, lb_mdl$, lb_send$, lb_text$,  ~
                lb_serno$, lb_bc$, lb_sernm$, lb_seq$, lb_cust$, lb_po$, ~
                lb_itmno$, lb_itmtot$, lb_so$, lb_city$, lb_state$,      ~
                lb_cont$, lb_job$, lb_room$, lb_nomsz$, lb_opnsz$,       ~
                lb_exsz$, lb_info$(), lb_sku$, lb_duedt$, lb_part$,      ~
                lb_drop$, lb_load$, lb_makedt$, lb_wnty$, lb_upc$,       ~
                lb_dept$, lb_pms$, lb_wood$, lb_samp$, lb_foam$, lb_fin$,~
                lb_oth1$, lb_frame$, lb_config$, lb_ups$,                ~
                lb_sub_info$, lb_oth2$, lb_specialmull$, lb_cust_code$,  ~
                eod goto end_process, data goto bad_data_flag


                                                  /* lb_oth1$ = New Sub*/
                                                  /* Part Number       */
            init (" ") lbl$(), lb_inspect$, lb_mull$, lb_screen$
            str(qty_of$,1%,3%) = str(lb_bc$,12%,3%)  /* Line Item Piece*/
            str(qty_of$,8%,3%) = str(lb_bc$,16%,3%)  /* Line Item Total*/
            lb_serno$ = lb_serno$ & "        "
            lb_sernm$ = lb_sernm$ & "          "
            lbl$(13%) = lb_sernm$             & fs$
            lb_cust$ = lb_cust$ & "                   "
            lb_po$ = lb_po$ & "                "
            lb_city$ = lb_city$ & "                  "

            init(" ") lb_config_txt$

            lb_config_txt$ = str(lb_so$,6%,3%) & "-" & lb_config$ &      ~
                             "    " & str(lb_bc$,12%,3%) & "-"           ~
                                    & str(lb_bc$,16%,3%)

            lb_config_txt3$ = "                 "     
            gosub calculate_day

            lb_opnsz$ = lb_opnsz$ & "          "
                                                       /* Exact Size 19*/
            lb_exsz$ = lb_exsz$ & "          "
            prod_desc$ = lb_info$(1%) & lb_info$(2%) & " " & lb_oth2$
                                                       /* lb_oth2$ New Descr */
     
            if len(lb_part$) < 19 then prod_desc$ = lb_txt$(1%) & lb_txt$(2%)

            lb_sku$ = lb_sku$ & "               "
                                                       /* (PAR002)     */
                                                       /* Sku not Printed */
            lb_part$ = lb_part$ & "                "

            lb_part% = 0%                               /* (PAR006)      */
            lb_part% = len(lb_part$)                   /* Part Number Length */
REM            if lb_specialmull$ = "C" then                                 ~
                                     str(lb_part$,lb_part%-2%,3%) = "   "
  
                                        /* Continous Head Mull Test     */
          gosub lookup_seq
          gosub lookup_sku
          if sku% = 0% then goto end_process
/* Print old labels for these SKU */
          if dt_sku$ = "13403058" then goto end_process
          if dt_sku$ = "13403004" then goto end_process
          if dt_sku$ = "13402914" then goto end_process
          if dt_sku$ = "13402848" then goto end_process
          if dt_sku$ = "13237106" then goto end_process
          if dt_sku$ = "13231258" then goto end_process
          if dt_sku$ = "13231256" then goto end_process
          if dt_sku$ = "13231249" then goto end_process
          if dt_sku$ = "13231248" then goto end_process
          if dt_sku$ = "13231237" then goto end_process
          if dt_sku$ = "13231224" then goto end_process
          if dt_sku$ = "13231222" then goto end_process
          if dt_sku$ = "13231220" then goto end_process
          if dt_sku$ = "13231070" then goto end_process
          if dt_sku$ = "13231062" then goto end_process
          if dt_sku$ = "1340984"  then goto end_process
          if dt_sku$ = "1340972"  then goto end_process
          if dt_sku$ = "1340964"  then goto end_process
          if dt_sku$ = "1340956"  then goto end_process
          
          gosub lookup_sub_part
          series$ = str(bcksubpt_rec$,169%,16%)
          style$  = str(bcksubpt_rec$,185%,10%)
          dt_sub_part$ = str(bcksubpt_rec$,48%,20%)
          color$ = str(lb_part$,4%,1%)   /* Color description */
          convert sc_sel$ to sc_sel%, data goto check_sel
          check_sel
              on sc_sel% goto sel_01,          /*Series 130 SH          */ ~
                              sel_02,          /*Series 130 SH w/grid   */ ~
                              sel_03,          /*Series 130 SL          */ ~
                              sel_04,          /*Series 130 SL w/grid   */ ~
                              sel_05,          /*Series 160 SH          */ ~
                              sel_06,          /*Series 160 SH w/grid   */ ~
                              sel_07,          /*Series 160 SL          */ ~
                              sel_08,          /*Series 160 SL w/grid   */ ~
                              sel_09,          /*Series 300 SH          */ ~
                              sel_10,          /*Series 300 SH w/grid   */ ~
                              sel_11,          /*Series 300 SL          */ ~
                              sel_12,          /*Series 300 SL w/grid   */ ~
                              sel_13,          /*Series 5700 SH         */ ~
                              sel_14,          /*Series 5700 SH w/grid  */ ~  
                              sel_15,          /*Series 5700 SL         */ ~
                              sel_16,          /*Series 8300 DH         */ ~
                              sel_17,          /*Series 8300 SL         */ ~
/* CR3273 */ ~							  
                              sel_18,          /*Series 150 SH Black    */ ~
                              sel_19,          /*Series 150 SH Black w/g*/ ~ 
                              sel_20,          /*Series 150 SH Black    */ ~
                              sel_21           /*Series 150 SH Black w/g*/ 							  
          goto end_process

/* add color restrictions by series based on requirements   */
/* CR3344 add V clay color code                             */
          
          sel_01                                           /* Series 130 SH */
              if str(series$,1%,8%) = "130     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 (color$ = "6" or color$ = "7" or            ~
                  color$ = "V" or color$ = "2") and          ~
                 (str(style$,1%,8%) = "SHHP    ")      then goto good_rec
              goto end_process
              
          sel_02                                       /*Series 130 SH w/grid*/
              if str(series$,1%,8%) = "130     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 (color$ = "6" or color$ = "7" or            ~
                  color$ = "V" or color$ = "2") and          ~                 
                 (str(style$,1%,8%) = "SHHP    " )     then goto good_rec
              goto end_process
              
          sel_03                                           /* Series 130 SL */
              if str(series$,1%,8%) = "130     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 (color$ = "6" or color$ = "7" or            ~
                  color$ = "V" or color$ = "2") and          ~
                 str(style$,1%,8%) = "2SL     "        then goto good_rec
              goto end_process
              
          sel_04                                       /*Series 130 SL w/grid*/
              if str(series$,1%,8%) = "130     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 (color$ = "6" or color$ = "7" or            ~
                  color$ = "V" or color$ = "2") and          ~                 
                 str(style$,1%,8%) = "2SL     "       then goto good_rec
              goto end_process
                            
          sel_05                                          /*Series 160 SH */
              if str(series$,1%,8%) = "160     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = "2"                    and                         ~
                 str(style$,1%,8%) = "SH      "   then goto good_rec
              goto end_process
              
          sel_06                                      /*Series 160 SH w/grid*/
              if str(series$,1%,8%) = "160     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 color$ = "2"                    and                         ~
                 str(style$,1%,8%) = "SH      "   then goto good_rec
              goto end_process   
  
          sel_07                                          /*Series 160 SL */
              if str(series$,1%,8%) = "160     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = "2"                    and                         ~
                 str(style$,1%,8%) = "2SL     "   then goto good_rec
              goto end_process
              
          sel_08                                      /*Series 160 SL w/grid*/
              if str(series$,1%,8%) = "160     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 color$ = "2"                    and                         ~
                 str(style$,1%,8%) = "2SL     "   then goto good_rec
              goto end_process   
                
          sel_09                                           /*Series 300 SH */
              if str(series$,1%,8%) = "300     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = sc_clr$                and                         ~
                (color$ = "Q" or color$ = "2")   and                         ~
                (str(style$,1%,8%) = "SH      "  or                          ~
                 str(style$,1%,8%) = "SHHP    ")     then goto good_rec
              goto end_process
              
          sel_10                                      /*Series 300 SH w/grid */
              if str(series$,1%,8%) = "300     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 color$ = sc_clr$                and                         ~
                (color$ = "Q" or color$ = "2")   and                         ~
                (str(style$,1%,8%) = "SH      "  or                          ~
                 str(style$,1%,8%) = "SHHP    ")      then goto good_rec
              goto end_process
              
          sel_11                                           /*Series 300 SL */
              if str(series$,1%,8%) = "300     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = sc_clr$                and                         ~
                (color$ = "Q" or color$ = "2")   and                         ~
                (str(style$,1%,8%) = "2SL     "  or                          ~
                 str(style$,1%,8%) = "2SLHP   ")      then goto good_rec
              goto end_process
              
          sel_12                                      /*Series 300 SL w/grid */
              if str(series$,1%,8%) = "300     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 color$ = sc_clr$                and                         ~
                (color$ = "Q" or color$ = "2")   and                         ~
                (str(style$,1%,8%) = "2SL     "  or                          ~
                 str(style$,1%,8%) = "2SLHP   ")      then goto good_rec
              goto end_process
                                      
          sel_13                                           /*Series 5700 SH */
              if str(series$,1%,8%) = "5700    " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                (color$ = "6" or color$ = "7" or             ~
                 color$ = "V" or color$ = "2") and           ~ 
                 str(style$,1%,8%) = "SH      "     then goto good_rec
              goto end_process
               
          sel_14                                      /*Series 5700 SH w/grid*/
              if str(series$,1%,8%) = "5700    " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                (color$ = "6" or color$ = "7" or             ~
                 color$ = "V" or color$ = "2") and           ~ 
                 str(style$,1%,8%) = "SH      "      then goto good_rec
              goto end_process
              
          sel_15                                          /*Series 5700 SL  */
              if str(series$,1%,8%) = "5700    " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                (color$ = "6" or color$ = "7" or             ~
                 color$ = "V" or color$ = "2") and           ~ 
                 str(style$,1%,8%) = "2SL     "      then goto good_rec
              goto end_process
      
          sel_16                                           /*Series 8300 DH  */
              if str(series$,1%,8%) = "8300    " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = "2"                    and                         ~
                 str(style$,1%,8%) = "DH      "     then goto good_rec
              goto end_process   

          sel_17                                           /*Series 8300 SL  */
              if str(series$,1%,8%) = "8300    " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = "2"                    and                         ~
                 str(style$,1%,8%) = "2SL     "     then goto good_rec
              goto end_process   
			  
          sel_18                            /* Series 150 SH Black Lam       */
              if str(series$,1%,8%) = "150     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = "4"                    and                         ~
                 str(style$,1%,8%) = "SH      "     then goto good_rec
              goto end_process   
			  
          sel_19                            /*Series 150 SH Black Lam w/grid */
              if str(series$,1%,8%) = "150     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 color$ = "4"                    and                         ~
                 str(style$,1%,8%) = "SH      "     then goto good_rec
              goto end_process   
			  
          sel_20                            /* Series 150 SL Black Lam       */
              if str(series$,1%,8%) = "150     " and                         ~
                 str(part$,7,2) = "00"           and                         ~
                 color$ = "4"                    and                         ~
                 str(style$,1%,8%) = "2SL     "     then goto good_rec
              goto end_process   
			  
          sel_21                            /*Series 150 SL Black Lam w/grid */
              if str(series$,1%,8%) = "150     " and                         ~
                 str(part$,7,2) <> "00"          and                         ~
                 color$ = "4"                    and                         ~
                 str(style$,1%,8%) = "2SL     "     then goto good_rec
              goto end_process   
			  			  
good_rec:              
   
REM 13,3 & 16,3 for exact size
          wh% = 0%
          wf% = 0%
          hh% = 0%
          hf% = 0%
          convert str(lb_part$,14,2) to wh%, data goto bad_part
          convert str(lb_part$,16,1) to wf%, data goto bad_part
          convert str(lb_part$,17,2) to hh%, data goto bad_part
          convert str(lb_part$,19,1) to hf%, data goto bad_part

bad_part:
          oWidth$  = "  "
          eWidth$  = "  "
          oHeight$ = "  "
          eHeight$ = "  "
      /* convert exact to opening */
          convert wh% to eWidth$,  pic (#0)
          convert hh% to eHeight$, pic (#0)
          if wf% > 0% then eWidth$ = eWidth$ & frac$(wf% + 1%)
          if hf% > 0% then eHeight$ = eHeight$ & frac$(hf% + 1%)

            sp% = 0%
            pc_c$ = "0000"  :  pc_cm$ = "00"
            size$ = "E" 
            err% = 0%
            call"APCPR5SB" (size$,         /* (E)xact or (O)pening        */~
                            lb_part$,      /* MFG Part Always Exact       */~
                            op_part$,      /* MFG Part Always Opening     */~
                            dt_cust$,      /* Customer Code               */~
                            sp%,           /* Special Flag 0%=Cat,1%=Spc  */~
                            pc_c$,         /* Special Catalog Code or 0000*/~
                            pc_cm$,        /* Special Catalog Method or 00*/~
                            #8,            /* (AWDPCMST) - File           */~
                            #1,            /* (GENCODES) - File           */~
                            err% )         /* Error Non Zero Value        */
            if err% <> 0% then goto bad_part2

          wh% = 0%
          wf% = 0%
          hh% = 0%
          hf% = 0%
          convert str(op_part$,14,2) to wh%, data goto bad_part2
          convert str(op_part$,16,1) to wf%, data goto bad_part2
          convert str(op_part$,17,2) to hh%, data goto bad_part2
          convert str(op_part$,19,1) to hf%, data goto bad_part2
bad_part2:
          convert wh% to oWidth$,  pic (#0)
          convert hh% to oHeight$, pic (#0)
          if wf% > 0% then oWidth$ = oWidth$ & frac$(wf% + 1%)
          if hf% > 0% then oHeight$ = oHeight$ & frac$(hf% + 1%)
          lb_date$ = "00000000"
          convert lb_date to lb_date$, pic (00000000)

          call "EWDNOMSZ" ("E", lb_part$, op_part$, nominal$, dt_cust$,  ~
                                                          #8, #1, p_err%)
                                                          
          p_nominal$ = str(nominal$,1%,3%) & "x " & str(nominal$,4%,6%) & iend$
          if p_err% > 0% then p_nominal$ = "     00 x 00" & iend$
          
      dq$ = bin(34%,1%)
      sq$ = bin(39%,1%)
REM      str(p_nominal$,1%,8%)  = str(nominal$,1%,1%) & sq$ & " - " & ~
REM                             str(nominal$,2%,2%) & dq$ 
REM     str(p_nominal$,9%,7%)  = " in. x "
REM     str(p_nominal$,16%,6%) = str(nominal$,5%,1%) & sq$ & " - " & ~
REM                              str(nominal$,6%,2%) & dq$
REM     str(p_nominal$,26%,4%) = " in."
REM
REM      if p_err% > 0%  then p_nominal$ = "0 - 00 in. X 0 - 00 in." & iend$ 

          goto format_mccoys

format_mccoys:
REM   01 = upc
      lbl$(01%) = l_model$ & iend$
REM   02 = desc,1,15
      lbl$(02%) = str(l_desc$,1%,15%) & iend$
REM   03 = Customer Name
      lbl$(03%) = str(lb_cust$,1%,13%) & iend$
REM   04 = sku
      lbl$(04%) = dt_sku$ & iend$
REM   05 = sku
      lbl$(05%) = dt_sku$ & iend$
REM   06 = Make Date
      lbl$(06%) = lb_makedt$ & iend$
REM   07 = Make Date
      lbl$(07%) = lb_makedt$ & iend$
REM   08 = Load
      lbl$(08%) = lb_load$ & iend$
REM   09 = Load
      lbl$(09%) = lb_load$ & iend$
REM   10 = Dept
      lbl$(10%) = lb_dept$ & iend$
REM   11 = Dept
      lbl$(11%) = lb_dept$ & iend$
REM   12 = Customer Name
      lbl$(12%) = str(lb_cust$,1%,13%) & iend$
REM   13 = upc
      lbl$(13%) = l_model$ & iend$
REM   14 = PO Number
      lbl$(14%) = lb_po$ & iend$
REM   15 = PO Number
      lbl$(15%) = lb_po$ & iend$
REM   16 = Sales Order & Item
      lbl$(16%) = lb_so$ & "-" & lb_itmno$ & iend$
REM   17 = Sales Order & Item
      lbl$(17%) = lb_so$  & "-" & lb_itmno$ & iend$
REM   18 = desc,16,15
      lbl$(18%) = str(l_desc$,16%,15%) & iend$
REM   19 = desc,31,15
      lbl$(19%) = str(l_desc$,31%,15%) & iend$
REM   20 = desc,46,15
      lbl$(20%) = str(l_desc$,46%,15%) & iend$
REM   21 = desc,61,15
      lbl$(21%) = str(l_desc$,61%,9%) & iend$
REM   22 = Exact Width
      lbl$(22%) = eWidth$ & iend$
REM   23 = Exact Height
      lbl$(23%) = eHeight$ & iend$
REM   24 = desc,1,15
      lbl$(24%) = str(l_desc$,1%,15%) & iend$
REM   25 = desc,16,15
      lbl$(25%) = str(l_desc$,16%,15%) & iend$
REM   26 = desc,31,15
      lbl$(26%) = str(l_desc$,31%,15%) & iend$
REM   27 = desc,46,15
      lbl$(27%) = str(l_desc$,46%,15%) & iend$
REM   28 = desc,61,15
      lbl$(28%) = str(l_desc$,61%,9%) & iend$
REM   29 = Sequence
      lbl$(29%) = lb_seq$ & iend$
REM   30 = Sequence
      lbl$(30%) = lb_seq$ & iend$
REM   31 = Exact Width
      lbl$(31%) = eWidth$ & iend$
REM   32 = Exact Height
      lbl$(32%) = eHeight$ & iend$
REM   33 = eSize
      exSize$ = eWidth$ & " in. x " & eHeight$ & " in." & iend$
      call "STRING" addr("CT", exSize$, 30%)
      lbl$(33%) = exSize$
REM   34 = Nominal size 
      call "STRING" addr("CT", p_nominal$, 32%)  
      if p_err% = 0% then lbl$(34%) = p_nominal$
REM   35 = model$          
      lbl$(35%) = model$ & iend$
      
      szfrc% = 0%
      t% = 0%: p% = 0%: r% = 0%
      for t% = 1% to 20%
        if str(lb_opnsz$,t%,1%) = "/" then str(lb_opnsz$,t%-2,1%) = "-"
        if str(lb_opnsz$,t%,1%) = "/" then szfrc% = 1%
      next t%
      p% = pos(lb_opnsz$ = "X")
      temp$ = str(lb_opnsz$, p% + 2%, 20%)
      r% = len(temp$)
      if szfrc% = 0% then ~
        lbl$(36%) = "     " & str(lb_opnsz$,1,p% - 1%) & " in. " & "x " & ~
                  str(lb_opnsz$,p% + 2%, r%) & " in." & iend$     ~
      else    ~
        lbl$(36%) = str(lb_opnsz$,1,p% - 1%) & " in. " & "x " & ~
                  str(lb_opnsz$,p% + 2%, r%) & " in." & iend$    
                  
      lbl$(37%) = str(l_model$,1%,1%) & iend$
      lbl$(38%) = str(l_model$,2%,1%) & iend$      
      lbl$(39%) = str(l_model$,3%,1%) & iend$
      lbl$(40%) = str(l_model$,4%,1%) & iend$
      lbl$(41%) = str(l_model$,5%,1%) & iend$
      lbl$(42%) = str(l_model$,6%,1%) & iend$
      lbl$(43%) = str(l_model$,7%,1%) & iend$
      lbl$(44%) = str(l_model$,8%,1%) & iend$
      lbl$(45%) = str(l_model$,9%,1%) & iend$
      lbl$(46%) = str(l_model$,10%,1%) & iend$
      lbl$(47%) = str(l_model$,11%,1%) & iend$
      lbl$(48%) = str(l_model$,12%,1%) & iend$
      lbl$(49%) = str(l_model$,1%,11%) & iend$
          
      init(" ") clr_desc$
      gosub color_desc
      lbl$(50%) = clr_desc$ & iend$
      
      lbl$(51%) = "        "  & iend$     /* Grid Type Description */
      if str(dt_sub_part$,1%,1%) = "1" then lbl$(51%) = "COLONIAL" & iend$
      if str(dt_sub_part$,1%,1%) = "2" then lbl$(51%) = "CONTOUR " & iend$
                
      init(" ") gls_desc$            /* Glass Type Description */
      gosub glass_desc
      call "STRING" addr("CT", gls_desc$, 22%)
      lbl$(52%) = gls_desc$ & iend$

      lbl$(53%) = "  "& fs$          /* High Performance */
      if str(style$,1%,4%) = "SHHP" or str(style$,1%,4%) = "2SLHP" then ~
           lbl$(53%) = "HIGH PERFORMANCE" & iend$

      goto L20700

L20700:
      goto mccoys_lbl

mccoys_lbl:        
    gosub mcoys_fmt
        copy yy$() to xx$()
    lbl% = lbl% + 1%
    goto read_loop
    
    read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
    skip_data

        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */
        goto read_loop

    bad_data_flag
        error% = 7%

    end_process

        return

        lookup_seq
            init(" ") dt_key0$, dt_seq$, dt_sku$

            str(dt_key0$,1%,18%) = lb_bc$
            str(dt_key0$,19%,3%) = "044"
            str(dt_key0$,22%,2%) = "01"
            read #2,key = dt_key0$, using LSEQ, dt_seq$, dt_cust$, dt_sku$,  ~
                                                   eod goto lookup_seq_done
LSEQ:         FMT POS(111), CH(05), POS(124), CH(09), POS(245), CH(09)
                                                    
              if lb_w$ = "W" then str(lb_config_txt3$,12%,6%) = "W" & dt_seq$

              if lb_w$ = "D" then str(lb_config_txt3$,12%,6%) = "D" & dt_seq$
                                                    
              init(" ") lbl$(24%)
              lbl$(24%) = lb_config_txt3$ & fs$
            lookup_seq_done
        return


        lookup_sku
            init(" ") dt_key0$
            sku% = 0%
            if dt_sku$ > "        " then goto skip_sku_read
            str(dt_key0$,1%,18%) = lb_bc$
            str(dt_key0$,19%,3%) = "000"
            str(dt_key0$,22%,2%) = "00"
            read #2,key >= dt_key0$, using LSEQ2, tmp_bar$,dt_seq$, dt_sku$,   ~
                                                   eod goto AWDSKUXR
LSEQ2:      FMT POS(24), CH(18), POS(111), CH(05), POS(245), CH(09)
            if tmp_bar$ <> lb_bc$ then goto AWDSKUXR
            
skip_sku_read:
          sku_key$ = "X_MB" & dt_sku$ & "   "

          read #7, key = sku_key$, using AWDSKUXR, model$, l_model$, part$,~
                                           l_desc$,   eod goto AWDSKUXR
              sku% = 1%

AWDSKUXR:     FMT POS(82), CH(03), POS(21), CH(12), POS(37), CH(25),   ~
                  POS(88), CH(70)
        return
                                                  

        calculate_day                         
            testdate$ =  lb_makedt$
            call "DATUFMTC" (testdate$)
            call "DAY" addr(str(testdate$,,6%), day%)

            day% = day% - 1%
            if day% = 0% then day% = 7%

            convert day% to day$, pic(#)

            str(lb_config_txt3$,1%,5%) = "DAY-" & day$

        return
        
        /* CR1598  lookup series */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(),           ~
                      dt_sub_part$, dt_sub_info$
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"
            err1% = 0%

            convert str(lb_bc$,1%,8%) to so_inv%, data goto convert_alpha
            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(lb_bc$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)
order_converted:
            convert str(lb_bc$,9%,2%) to item_no%, data goto sub_part2
sub_part2:
            convert item_no% to item_no$, pic(###)

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #3,            /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */

            return

        color_desc
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "COLOR"
            str(readkey$,10%,15%) = color$
            read #1,key = readkey$, using L51500, desc$, eod goto L51510
L51500:        FMT POS(25), CH(30)
/* CR3273 */
              if color$ = "4" then clr_desc$ = str(desc$,6%,6%)  ~
			                  else clr_desc$ = str(desc$,6%,7%)
L51510: return

/* Glass Window Wizard description now available in SKU file */
        glass_desc

          init(" ") gls_desc$
          sku_key$ = "G_MB" & str(part$,5%,2%) & "   "

          read #7, key = sku_key$, using L51550, l_desc$,   eod goto L51575

L51550:     FMT POS(88), CH(35)

              gls_desc$ = l_desc$
              
L51575: return

REM   01 = upc
REM   02 = desc,1,15
REM   03 = Customer Name
REM   04 = sku
REM   05 = sku
REM   06 = Make Date
REM   07 = Make Date
REM   08 = Load
REM   09 = Load
REM   10 = Dept
REM   11 = Dept
REM   12 = Customer Name
REM   13 = upc
REM   14 = PO Number
REM   15 = PO Number
REM   16 = Sales Order & Item
REM   17 = Sales Order & Item
REM   18 = desc,16,15
REM   19 = desc,31,15
REM   20 = desc,46,15
REM   21 = desc,61,15
REM   22 = Exact Width
REM   23 = Exact Height
REM   34 = desc,1,15
REM   25 = desc,16,15
REM   26 = desc,31,15
REM   27 = desc,46,15
REM   28 = desc,61,15
REM   29 = Sequence
REM   30 = Sequence
REM   31 = Exact Width
REM   32 = Exact Height
REM   33 = oSize
REM   34 = eSize
REM   35 = model    


mcoys_fmt
      init(" ") yy$()  
 
      yy$(01) = "^JO^XA^EG^XZ^XA^PMN^MNY^MMT^MD0"
      yy$(02) = "^SZ2^JMA^PW1127~JSN^JZY^LH0,0^LRN^XZ"
      
      yy$(03) = "^XA"
      yy$(04) = "50^FT212,1440^A0N,37,36^FD"           /* Color           */
      yy$(05) = "01^FO146,1523^BY3^BUN,102,N,N,N^FD"   /* UPC 40006554727 */
      yy$(06) = "38^FT179,1652^CI0^A0N,34,46^FD"
      yy$(07) = "39^FT200,1652^A0N,34,46^FD"
      yy$(08) = "40^FT222,1652^A0N,34,46^FD"
      yy$(09) = "41^FT244,1652^A0N,34,46^FD"
      yy$(10) = "42^FT265,1652^A0N,34,46^FD"
      yy$(11) = "43^FT296,1652^A0N,34,46^FD"
      yy$(12) = "44^FT316,1652^A0N,34,46^FD"
      yy$(13) = "45^FT339,1652^A0N,34,46^FD"
      yy$(14) = "46^FT360,1652^A0N,34,46^FD"
      yy$(15) = "47^FT382,1652^A0N,34,46^FD"
      yy$(16) = "37^FT119,1652^A0N,25,34^FD"
      yy$(17) = "48^FT441,1652^A0N,25,34^FD"
      yy$(18) = "^FT146,1500^A0N,34,31^FDSKU #^FS"
      yy$(19) = "04^FT235,1498^A0N,45,51^FD"           /* SKU 1234567 */
      
      yy$(20) = "^FT197,1859^A0N,23,22^FDMODEL #^FS"
      yy$(21) = "35^FT286,1859^A0N,23,22^FD"           /* Model # */
      
/* CR2594 move exact section */      
      yy$(22) = "^FT235,1931^A0N,34,35^FDEXACT^FS"
      yy$(23) = "^FT159,1969^A0N,34,31^FDWIDTH      HEIGHT^FS"
      yy$(24) = "33^FT100,2005^A0N,37,35^FD"           /* Exact */
      
/* CR2594      yy$(25) = "^FT197,2033^A0N,34,35^FDActual Size:^FS" */
/* CR2594      yy$(26) = "34^FT104,2069^A0N,37,37^FD"             Actual */
 
/* CR2594 move fit section */ 
      yy$(25) = "^FT184,2082^A0N,31,33^FDFits Openings:^FS"
      yy$(26) = "36^FT121,2126^A0N,37,33^FD"            /* Fit */
      
      yy$(27) = "^FT58,2204^A0N,25,18^FDProduction Date^FS"
      yy$(28) = "06^FT184,2204^A0N,25,18^FD"            /* Prod Date */
      
      yy$(29) = "^FT286,2204^A0N,25,18^FDDept.#^FS"
      yy$(30) = "10^FT336,2204^A0N,25,18^FD"            /* Dept # */
      
      yy$(31) = "^FT375,2204^A0N,25,18^FDSequence #^FS"
      yy$(32) = "29^FT463,2204^A0N,25,18^FD"            /* Sequence # */
      
      yy$(33) = "^FT181,1824^A0N,28,22^FDPO#^FS"
      yy$(34) = "14^FT223,1826^A0N,28,29^FD"            /* PO # */
      
      yy$(35) = "51^FT210,1699^A0N,28,38^FD"            /* Grid Type  */
      yy$(36) = "52^FT96,1737^A0N,28,38^FD"             /* Glass Type  */
      yy$(37) = "53^FT134,1775^A0N,28,38^FD"            /* High Performance */
      
      yy$(38) = "50^FT768,1440^A0N,37,36^FD"            /* Color 2   */
      yy$(39) = "01^FO692,1523^BUN,102,N,N,N^FD"        /* barcode 2 */
      yy$(40) = "38^FT725,1652^A0N,34,46^FD"
      yy$(41) = "39^FT746,1652^A0N,34,46^FD"
      yy$(42) = "40^FT768,1652^A0N,34,46^FD"
      yy$(43) = "41^FT790,1652^A0N,34,46^FD"
      yy$(44) = "42^FT811,1652^A0N,34,46^FD"
      yy$(45) = "43^FT842,1652^A0N,34,46^FD"
      yy$(46) = "44^FT862,1652^A0N,34,46^FD"
      yy$(47) = "45^FT885,1652^A0N,34,46^FD"
      yy$(48) = "46^FT906,1652^A0N,34,46^FD"
      yy$(49) = "47^FT928,1652^A0N,34,46^FD"
      yy$(50) = "37^FT665,1652^A0N,25,34^FD"
      yy$(51) = "48^FT987,1652^A0N,25,34^FD"
      yy$(52) = "^FT702,1494^A0N,34,31^FDSKU #^FS"
      yy$(53) = "04^FT793,1498^A0N,45,51^FD"            /* SKU 2*/
      
      yy$(54) = "^FT742,1859^A0N,23,22^FDMODEL #^FS"
      yy$(55) = "35^FT831,1859^A0N,23,22^FD"            /* Model # 2 */

/* CR2594 move exact section */        
      yy$(56) = "^FT781,1931^A0N,34,35^FDEXACT^FS"
      yy$(57) = "^FT704,1969^A0N,34,31^FDWIDTH      HEIGHT^FS"
      yy$(58) = "33^FT646,2005^A0N,37,35^FD"            /* Exact 2 */
      
/*  CR2594      yy$(61) = "^FT743,2033^A0N,34,35^FDActual Size:^FS"
/*  CR2594      yy$(62) = "34^FT650,2069^A0N,37,37^FD"              Actual 2 */
 
/* CR2594 move fit section */ 
      yy$(59) = "^FT730,2084^A0N,34,35^FDFits Openings:^FS"
      yy$(60) = "36^FT679,2126^A0N,37,31^FD"            /* Fits Size 2 */
      
      yy$(61) = "^FT603,2203^A0N,25,18^FDProduction Date^FS"
      yy$(62) = "06^FT730,2203^A0N,25,18^FD"            /* Prod Date 2 */
      
      yy$(63) = "^FT831,2203^A0N,25,18^FDDept.#^FS"
      yy$(64) = "10^FT882,2203^A0N,25,18^FD"            /* Dept # 2 */
      
      yy$(65) = "^FT920,2203^A0N,25,18^FDSequence #^FS"
      yy$(66) = "29^FT1009,2203^A0N,25,18^FD"           /* Sequence # 2 */
      
      yy$(67) = "^FT726,1824^A0N,28,22^FDPO#^FS"
      yy$(68) = "14^FT768,1826^A0N,28,29^FD"            /* PO # 2 */
      
      yy$(69) = "51^FT768,1699^A0N,28,38^FD"            /* Grid Type 2 */
      yy$(70) = "52^FT629,1737^A0N,28,38^FD"            /* Glass Type 2 */
      yy$(71) = "53^FT667,1775^A0N,28,38^FD"            /* High Performance 2*/
      
      yy$(72) = "^PQ1,0,1,Y"
      yy$(73) = "^XZ"   
      
      return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                                       /* (PAR002)     */
L35000:     FMT                          /* Production Label Data      */~
                PD(11,1),                /* Date                       */~
                XX(29),                  /* Skip Primary Key           */~
                CH(3),                   /* Model No.                  */~
                CH(21),                  /* Send-To Info (Mfg.)        */~
                CH(210),                 /* Mfg. Text Array  (EWD001)  */~
                CH(8),                   /* Series No.                 */~
                CH(18),                  /* Production Barcode ID      */~
                XX(5),           /* Alternate Key Fields(EWD001)*/~
                CH(10),                  /* Private Label Name         */~
                CH(5),                   /* Sequence Number            */~
                CH(19),                  /* Customer Name              */~
                CH(16),                  /* Purchase Order No.         */~
                CH(3),                   /* Item No. (X in X of Y)     */~
                CH(3),                   /* Item Total (Y in X of Y)   */~
                CH(8),                   /* Sales Order No.            */~
                CH(18),                  /* Customer's City            */~
                CH(2),                   /* Customer's State           */~
                CH(16),                  /* Contractor                 */~
                CH(10),                  /* Job No./Name               */~
                CH(7),                   /* Room Name                  */~
                CH(6),                   /* Nominal Size               */~
                CH(19),                  /* Opening Size               */~
                CH(19),                  /* Exact Size                 */~
                2*CH(18),                /* Other Info Array           */~
                CH(15),                  /* SKU No.                    */~
                CH(10),                  /* Due Date (Formatted)       */~
                CH(25),                  /* Mfg. Part No.              */~
                CH(2),                   /* Drop Code                  */~
                CH(5),                   /* Load Code                  */~
                CH(10),                  /* Make Date (Formatted)      */~
                CH(10),                  /* Warranty ID                */~
                CH(11),                  /* UPC No.                    */~
                CH(03),                  /* Department Code            */~
                CH(05),                  /* Pull/Make/Stock            */~
                CH(06),                  /* Wood Surround code (EWD004)*/~
                CH(01),                  /* 0=No,1=Samp,2=Disp (EWD008)*/~
                CH(01),                  /* Foam (Y)es or (N)o (EWD008)*/~
                CH(01),                  /* NoFin(Y)es or (N)o (EWD010)*/~
                CH(24),                  /* New Sub Part No    (PAR002)*/~
                CH(01),                  /* Shape Frame Y or N (EWD014)*/~
                CH(06),                  /* S.O. Customer Code (EWD018)*/~
                CH(02),                  /* W.W Config Line No (PAR002)*/~
                CH(01),                  /* UPS Flag 0 or 1    (PAR002)*/~
                CH(10),                  /* New Par Info Fields(PAR002)*/~
                CH(40),                  /* New Sub Part Descr (PAR002)*/~
                CH(01),                  /* New Special Mull code(PAR006)*/~
                CH(09)                   /* New Customer Code          */

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(128)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

        print_line
            write #5, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub

            if twice% = 1% then goto L10000
        end

        exit_print
            lb1% = 0% : lb2% = 0%

            close #5

            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGPROD'        */
                                          /*      or 'NEPROD'         */
        end


        load_label
        init(" ") yy$()                          

        return

        file_exists
          comp% = 2%
          hdr$ = "*** Production File Exists ***"
                                          
          if schema% = 1% then                                          ~
             msg$(1%) = "        The File (MFGPROD) Already Exists.      "~
                          else                                             ~
             msg$(1%) = "        The File (NEPROD) Already Exists.       "
                                         
          msg$(2%) = "       P r o d u c t i o n   L a b e l s         "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                    
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

