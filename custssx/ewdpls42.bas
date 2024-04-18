        REM *************************************************************~
            *            (RHHTEST)    Test Print Turned (off)            *~
            *                                                           *~
            *      Note- Bay-Bow Test for Line = 25%  in yy$(??)        *~
            *      Note- UPC Test for Line     = 86% (Turned Off)       *~
            *                                        (AWD028)           *~
            *      Note- 100% Inspection test for Line = 27% in yy$(??) *~
            *                                 and Line = 102% (Text)    *~
            *  Subroutine Name   - EWDPLA71                             *~
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
            * EWDPLA71 - Generates the label format and data to print   *~
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
            *11/24/2008! Original - Copied & Mod (sub) EWDPLA71.  ! DES *~
            *09/25/2013! (AWD001) Series 85 modifications         ! CMG *~
            *10/09/2019! (CR2192) TX McCoy labels prt in ewdpln43 ! RDB *~
            *04/27/2020! SR95193   Add PW to old label print       ! RDB *~
            *************************************************************

        sub "EWDPLS42" (been_here%,      /* Zero (Only 1st Time)       */~
                        rec$(),          /* Prod Label Data    (PAR002)*/~
                        #1,              /* GENCODES           (EWD018)*/~
                        #2,              /* APCPLNDT           (EWD022)*/~
                        #4,              /* BCKLINES           (AWD007)*/~
                        #3,              /* BCKSUBPT           (PAR000)*/~
                        #7,              /* AWDSKUXR           (PAR000)*/~
                        #8,              /* APCPCMST           (EWD022)*/~
                        sc_sel$,         /* Label Type         (PAR000)*/~
                        lbl%,            /* Label Type         (PAR000)*/~
                        error%)          /* Return Code                */

        dim                                                              ~
            tmp_bar$18,                  /* (PAR003) Schema Switch     */~
            schema$8,                    /* (PAR003) Schema Switch     */~
            a$128, b$128,                /* Print Lines for Label      */~
            lbl$(50%)80,                 /* Label Data Array   (EWD001)*/~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            lb_text$210,                 /* Mfg. Text Holding  (EWD001)*/~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            dt_key0$23,                  /* Primary Key        (EWD022)*/~
            dt_seq$5,                    /* Sequence No.       (EWD022)*/~
            rec$(4%)256                  /* Rec Array Prod. Lbl(PAR002)*/
            
/* CR2192 */
        dim bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
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
            lb_cust_code$6,              /* Customer Code      (EWD018)*/~
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
            sc_sel$2,                    /* Screen Sel                 */~
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
            pname$ = "EWDPLA71 - Rev: R8.00"

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

            select #5, "MFGLOWE", varc, consec, recsize =   128


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
            ibeg$ = "<STX>"
            iend$ = "<ETX>"

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

                  been_here% = been_here% + 1%  /* (EWD020)           */
                  copy yy$() to xx$()
                  gosub begin_process

                  goto exit_sub

        set_file_name                             /* (RHHTEST)       */
            init(" ") file$, script$
                                                  /* (PAR003)        */
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

SS_1:                                                /* (PAR002)        */
                                                     /* (EWD014)        */
                                                     /* (RHHTEST)       */
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "MFGLOWE"
               script$  = "MFGLOWE"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */
SS_2:
               file$    = "NELOWE"
               script$  = "NELOWE"
/* (AWD001) */
               if sc_sel$ = "10" then file$   = "MNELOWE"
               if sc_sel$ = "10" then script$ = "MNELOWE"
               if sc_sel$ = "11" then file$   = "ALUMLOWE"
               if sc_sel$ = "11" then script$ = "ALUMLOWE"
               if sc_sel$ = "12" then file$   = "ALUMLOWE"
               if sc_sel$ = "12" then script$ = "ALUMLOWE"
/* (\AWD001) */
               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
        REM    file$   = "MFGTEST"                /* (EWD010)        */
       REM   script$ = "MFGTEST"

        REM    file$   = "NEATEST"
        REM    script$ = "NEATEST"

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
        begin_process                             /* (EWD001)          */
                                                  /* (EWD008) 11/19/99 */
                                                  /* (AWD031) 06/15/05 */
                                                  /* (PAR002) 02/20/06 */
         get str(rec$()), using L35000, lb_date, lb_mdl$, lb_send$, lb_text$,  ~
                lb_serno$, lb_bc$, lb_sernm$, lb_seq$, lb_cust$, lb_po$, ~
                lb_itmno$, lb_itmtot$, lb_so$, lb_city$, lb_state$,      ~
                lb_cont$, lb_job$, lb_room$, lb_nomsz$, lb_opnsz$,       ~
                lb_exsz$, lb_info$(), lb_sku$, lb_duedt$, lb_part$,      ~
                lb_drop$, lb_load$, lb_makedt$, lb_wnty$, lb_upc$,       ~
                lb_dept$, lb_pms$, lb_wood$, lb_samp$, lb_foam$, lb_fin$,~
                lb_oth1$, lb_frame$, lb_cust_code$, lb_config$, lb_ups$, ~
                lb_sub_info$, lb_oth2$, lb_specialmull$,                 ~
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

            lb_config_txt3$ = "                 "      /* (EWD024)     */
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
/* CR2192 */
          if schema% = 1%  then goto L10200   
          if sc_sel$ <> "10" then goto L10200
          
/* Skip for TX as McCoy labels printed in ewdpln43 except these */
          gosub lookup_sub_part      
          series$ = str(bcksubpt_rec$,169%,16%)
          style$  = str(bcksubpt_rec$,185%,10%)
          dt_sub_part$ = str(bcksubpt_rec$,48%,20%) 
          if len(lb_part$) < 19 and style$ = "SH" and                       ~
           (str(series$,1%,3%) = "85 " or str(series$,1%,4%) = "300 ") then ~
               goto L10200
          if style$ = "2MO" and (str(series$,1%,3%) = "85 " or                ~
             str(series$,1%,4%) = "105 " or str(series$,1%,4%) = "130 " or    ~
             str(series$,1%,4%) = "300 " or str(series$,1%,4%) = "5700") then ~
               goto L10200
/* Print old labels for these SKU */
          if dt_sku$ = "13403058" then goto L10200
          if dt_sku$ = "13403004" then goto L10200
          if dt_sku$ = "13402914" then goto L10200
          if dt_sku$ = "13402848" then goto L10200
          if dt_sku$ = "13237106" then goto L10200
          if dt_sku$ = "13231258" then goto L10200
          if dt_sku$ = "13231256" then goto L10200
          if dt_sku$ = "13231249" then goto L10200
          if dt_sku$ = "13231248" then goto L10200
          if dt_sku$ = "13231237" then goto L10200
          if dt_sku$ = "13231224" then goto L10200
          if dt_sku$ = "13231222" then goto L10200
          if dt_sku$ = "13231220" then goto L10200
          if dt_sku$ = "13231070" then goto L10200
          if dt_sku$ = "13231062" then goto L10200
          if dt_sku$ = "1340984"  then goto L10200
          if dt_sku$ = "1340972"  then goto L10200
          if dt_sku$ = "1340964"  then goto L10200
          if dt_sku$ = "1340956"  then goto L10200
          if dt_sku$ = "1340150"  then goto L10200    /* SR95193 CR2533 */
          
          goto end_process           
          
L10200:
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
            call"APCPR5SB" (size$,       /* (E)xact or (O)pening        */~
                            lb_part$,       /* MFG Part Always Exact       */~
                            op_part$,     /* MFG Part Always Opening     */~
                            lb_cust_code$, /* Customer Code               */~
                            sp%,         /* Special Flag 0%=Cat,1%=Spc  */~
                            pc_c$,       /* Special Catalog Code or 0000*/~
                            pc_cm$,      /* Special Catalog Method or 00*/~
                            #8,          /* (APCPCMST) - File           */~
                            #1,          /* (GENCODES) - File           */~
                            err% )       /* Error Non Zero Value        */
            if err% <> 0% then end

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

          if sc_sel$ = "10" then goto format_mccoys
/* (AWD001) */
          if sc_sel$ = "11" then goto format_mccoys
          if sc_sel$ = "12" then goto format_mccoys
/* (\AWD001) */
REM   01 = sku
          lbl$(01%) = dt_sku$ & fs$
REM   02 = l_model
          lbl$(02%) = l_model$ & fs$
REM   03 = oSize
          lbl$(03%) = oWidth$ & " in. x " & oHeight$ & " in." & fs$
REM   04 = eSize
          lbl$(04%) = eWidth$ & " in. x " & eHeight$ & " in." & fs$
REM   05 = oWidth
          lbl$(05%) = oWidth$ & fs$
REM   06 = oHeight
          lbl$(06%) = oHeight$ & fs$
REM   07 = eWidth
          lbl$(07%) = eWidth$ & fs$
REM   08 = eHeight
          lbl$(08%) = eHeight$ & fs$
REM   09 = oWidth  w/in
          lbl$(09%) = oWidth$ & " in." & fs$
REM   10 = oHeight w/in
          lbl$(10%) = oHeight$ & " in." & fs$
REM   11 = eWidth  w/in
          lbl$(11%) = eWidth$ & " in. x" & fs$
REM   12 = eHeight w/in
          lbl$(12%) = eHeight$ & " in." & fs$
REM   13 = oSizeSp
          lbl$(13%) = oWidth$ & " x " & oHeight$ & " pulg." & fs$
REM   14 = eSizeSp
          lbl$(14%) = eWidth$ & " x " & eHeight$ & " pulg." & fs$
REM   15 = date
          lbl$(15%) = str(lb_date$,5,2) & "-" & str(lb_date$,7,2) & "-" &  ~
      str(lb_date$,3,2) & fs$
REM   16 = dept
          lbl$(16%) = lb_dept$ & fs$
REM   17 = sequence
          lbl$(17%) = lb_seq$ & fs$
REM   18 = model - check dgt
          lbl$(18%) = str(l_model$,1,11) & fs$

      goto L20700

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
REM   34 = desc,1,15
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
/* (AWD001) */
REM   33 = oSize
      lbl$(33%) = oWidth$ & " IN. x " & oHeight$ & " IN." & iend$
REM   34 = eSize
      lbl$(34%) = eWidth$ & " IN. x " & eHeight$ & " IN." & iend$
/* (\AWD001) */
          goto L20700

L20700:

        if sc_sel$ = "10" then goto mccoys_lbl
/* (AWD001) */
        if sc_sel$ = "11" then goto series85_lbl
        if sc_sel$ = "12" then goto series85_lbl
/* (\AWD001) */
        if model$ <> "754" and model$ <> "5W1" then goto L20710
    gosub model_3100
        copy yy$() to xx$()
    lbl% = lbl% + 1%
    goto read_loop

L20710: if model$ <> "417" then goto L20720
    gosub model_450
        copy yy$() to xx$()
    lbl% = lbl% + 1%
    goto read_loop

L20720:  if sc_sel$ = "10" then goto mccoys_lbl
         if sc_sel$ = "11" then goto series85_lbl
         if sc_sel$ = "12" then goto series85_lbl
    gosub model_150
        copy yy$() to xx$()
    lbl% = lbl% + 1%
    goto read_loop

mccoys_lbl:
    gosub mcoys_fmt
        copy yy$() to xx$()
    lbl% = lbl% + 1%
    goto read_loop

series85_lbl:
    gosub series85_fmt
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
                                           /* (AWD019)                  */
REM     if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

                                           /* (EWD020)                   */
REM     if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop



        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */
        goto read_loop



    bad_data_flag
        error% = 7%


    end_process
REM     if nbr_lines% = 0% then error% = 8%
        return

                                                     /* (EWD022)        */
        lookup_seq
            init(" ") dt_key0$, dt_seq$, dt_sku$

            str(dt_key0$,1%,18%) = lb_bc$
            str(dt_key0$,19%,3%) = "044"
            str(dt_key0$,22%,2%) = "01"
            read #2,key = dt_key0$, using LSEQ, dt_seq$, dt_sku$,     ~
                                                   eod goto lookup_seq_done
LSEQ:         FMT POS(111), CH(05), POS(245), CH(09)
                                                    /* (AWD026)         */
              if lb_w$ = "W" then str(lb_config_txt3$,12%,6%) = "W" & dt_seq$

              if lb_w$ = "D" then str(lb_config_txt3$,12%,6%) = "D" & dt_seq$
                                                    /* (AWD026)         */
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
          sku_key$ = "X_LO" & dt_sku$ & "   "
          if sc_sel$ = "10" then sku_key$ = "X_MB" & dt_sku$ & "   "

          read #7, key = sku_key$, using AWDSKUXR, model$, l_model$, part$,~
                                           l_desc$,   eod goto AWDSKUXR
              sku% = 1%
              if sc_sel$ = "10" then goto good_sel
/* (AWD001) */
              if sc_sel$ <> "11" and sc_sel$ <> "12" then goto check_others
                 if str(sku_key$,1%,4%) <> "X_LO" then goto bad_sel
REM sc_sel$ = 12 then with grid Series 85
                 if str(part$,7,2) = "00" and sc_sel$ = "12" then goto bad_sel
                 if str(part$,7,2) <> "00" and sc_sel$ = "11" then goto bad_sel
                   if model$  = "3W1" then goto good_sel
                   if model$  = "3H1" then goto good_sel
                     goto bad_sel
check_others:
/* (\AWD001) */
              if sc_sel$ = "03" and model$ = "126" then goto good_sel
              if sc_sel$ = "03" then goto good_sel
              if str(part$,7,2) = "00" then goto no_grid
              if sc_sel$ = "02" and model$ = "511" then goto good_sel
              if sc_sel$ = "05" and model$ = "417" then goto good_sel
              if sc_sel$ = "07" and model$ = "754" then goto good_sel
              if sc_sel$ = "09" and model$ = "5W1" then goto good_sel
              goto bad_sel
no_grid:
              if sc_sel$ = "01" and model$ = "511" then goto good_sel
              if sc_sel$ = "04" and model$ = "417" then goto good_sel
              if sc_sel$ = "06" and model$ = "754" then goto good_sel
              if sc_sel$ = "08" and model$ = "5W1" then goto good_sel

bad_sel:      sku% = 0%
good_sel:
AWDSKUXR:     FMT POS(82), CH(03), POS(21), CH(12), POS(37), CH(25),   ~
                  POS(88), CH(70)
        return
                                                    /* (EWD022)         */

        calculate_day                               /* (EWD024)         */
            testdate$ =  lb_makedt$
            call "DATUFMTC" (testdate$)
            call "DAY" addr(str(testdate$,,6%), day%)

            day% = day% - 1%
            if day% = 0% then day% = 7%

            convert day% to day$, pic(#)

            str(lb_config_txt3$,1%,5%) = "DAY-" & day$

        return
        
/* CR2192  lookup series */
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
REM   01 = sku
REM   02 = l_model
REM   03 = oSize
REM   04 = eSize
REM   05 = oWidth
REM   06 = oHeight
REM   07 = eWidth
REM   08 = eHeight
REM   09 = oWidth  w/in
REM   10 = oHeight w/in
REM   11 = eWidth  w/in
REM   12 = eHeight w/in
REM   13 = oSizeSp
REM   14 = eSizeSp
REM   15 = date
REM   16 = dept
REM   17 = sequence

model_3100
      init(" ") yy$()                           /* (EWD009)         */
REM move left 1/8" (25 points)
      yy$(01) = "^JO^XA^EG^XZ^XA^PMN^MNY^MMT^MD0"
      yy$(02) = "^LH0,0^PR4^JMA"
      yy$(03) = "^FO108,0646^CI0^A0N,65,095^FR^FD#^FS"
      yy$(04) = "01^FO162,0646^CI0^A0N,140,170^FR^FD"
      yy$(05) = "18^FO270,0806^BY3,3.20,100^BUN,200,Y,N^FR^FD"
      yy$(06) = "02^FO230,1051^C10^A0N,24,28^FR^FDMODEL/MODELO # "
      yy$(07) = "03^FO110,1106^CI0^A0N,100,110^FR^FD"
      yy$(08) = "11^FO065,1206^CI0^A0N,24,20^FR^FDExact Size: "
      yy$(09) = "12^FO065,1232^CI0^A0N,24,20^FR^FD"
      yy$(10) = "03^FO065,1273^CI0^A0N,24,20^FR^FDReplaces: "
      yy$(11) = "^FO065,1299^CI0^A0N,24,20^FR^FDOpenings^FS "
      yy$(12) = "11^FO420,1206^CI0^A0N,24,20^FR^FDTama¤o Exacto: "
      yy$(13) = "12^FO420,1232^CI0^A0N,24,20^FR^FD"
      yy$(14) = "13^FO420,1273^CI0^A0N,24,20^FR^FDSustituye: "
      yy$(15) = "^FO420,1299^CI0^A0N,24,20^FR^FDAperatus^FS "
      yy$(16) = "15^FO065,1336^CI0^A0N,24,20^FR^FDProduction Date: "
      yy$(17) = "16^FO360,1336^CI0^A0N,24,20^FR^FDDept. #"
      yy$(18) = "17^FO520,1336^CI0^A0N,24,20^FR^FD Sequence #"
      yy$(19) = "15^FO065,1361^CI0^A0N,24,20^FR^FDFecha de produccion: "
      yy$(20) = "16^FO360,1361^CI0^A0N,24,20^FR^FDDept. #"
      yy$(21) = "17^FO520,1361^CI0^A0N,24,20^FR^FD Secuencia #"
      yy$(22) = "^FO0805,0646^CI0^A0N,65,095^FR^FD#^FS "
      yy$(23) = "01^FO0859,0646^CI0^A0N,140,170^FR^FD"
      yy$(24) = "18^FO0967,0806^BY3,3.20,100^BUN,200,Y,N^FR^FD"
      yy$(25) = "02^FO0927,1051^CI0^A0N,24,28^FR^FDMODEL/MODELO # "
      yy$(26) = "03^FO0812,1106^CI0^A0N,100,100^FR^FD"
      yy$(27) = "11^FO0812,1206^CI0^A0N,24,20^FR^FDExact Size: "
      yy$(28) = "12^FO0812,1232^CI0^A0N,24,20^FR^FD"
      yy$(29) = "03^FO0812,1273^CI0^A0N,24,20^FR^FDReplaces: "
      yy$(30) = "^FO0812,1299^CI0^A0N,24,20^FR^FDOpenings^FS "
      yy$(31) = "11^FO1137,1206^CI0^A0N,24,20^FR^FDTama¤o Exacto: "
      yy$(32) = "12^FO1137,1232^CI0^A0N,24,20^FR^FD"
      yy$(33) = "13^FO1137,1273^CI0^A0N,24,20^FR^FDSustituye: "
      yy$(34) = "^FO1137,1299^CI0^A0N,24,20^FR^FDAperatus^FS "
      yy$(35) = "15^FO0812,1336^CI0^A0N,24,20^FR^FDProduction Date: "
      yy$(36) = "16^FO1107,1336^CI0^A0N,24,20^FR^FDDept. #"
      yy$(37) = "17^FO1217,1336^CI0^A0N,24,20^FR^FD Sequence #"
      yy$(38) = "15^FO0812,1361^CI0^A0N,24,20^FR^FDFecha de produccion: "
      yy$(39) = "16^FO1107,1361^CI0^A0N,24,20^FR^FDDept. #"
      yy$(40) = "17^FO1217,1361^CI0^A0N,24,20^FR^FD Secuencia #"
      yy$(41) = "^PQ1^XZ"
        return

model_150
model_450
      init(" ") yy$()                           /* (EWD009)         */
      yy$(01) = "^JO^XA^EG^XZ^XA^PMN^MNY^MMT^MD0"
      yy$(02) = "^LH0,0^PR4^JMA"
      yy$(03) = "^FO0126,0648^CI0^A0N,120,150^FR^FD#^FS"
      yy$(04) = "01^FO0198,0648^CI0^A0N,220,250^FR^FD"
      yy$(05) = "18^FO0428,0848^BY3,3.0,100^BUN,200,Y,N^FR^FD"
      yy$(06) = "02^FO0328,1088^C10^A0N,36,36^FR^FDMODEL/MODELO # "
      yy$(07) = "03^FO0133,1143^CI0^A0N,150,150^FR^FD"
      yy$(08) = "03^FO0068,1293^CI0^A0N,25,22^FR^FDOpening Size: "
      yy$(09) = "04^FO0068,1328^CI0^A0N,25,22^FR^FDActual Size: "
      yy$(10) = "03^FO0588,1293^CI0^A0N,25,22^FR^FDTama¤o de la Apertura: "
      yy$(11) = "04^FO0588,1328^CI0^A0N,25,22^FR^FDTama¤o Exacto: "
      yy$(12) = "15^FO0068,1388^CI0^A0N,25,22^FR^FDProduction Date: "
      yy$(13) = "16^FO0518,1388^CI0^A0N,25,22^FR^FDDept. #"
      yy$(14) = "17^FO0743,1388^CI0^A0N,25,22^FR^FD Sequence #"
      yy$(15) = "15^FO0068,1423^CI0^A0N,25,22^FR^FDFecha de produccion: "
      yy$(16) = "16^FO0518,1423^CI0^A0N,25,22^FR^FDDpto. #"
      yy$(17) = "17^FO0743,1423^CI0^A0N,25,22^FR^FD Serie #"
      yy$(18) = "^FO1235,0648^CI0^A0N,120,90^FR^FD#^FS"
      yy$(19) = "01^FO1280,0648^CI0^A0N,220,150^FR^FD"
      yy$(20) = "18^FO1333,0848^BY3,3.0,100^BUN,200,Y,N^FR^FD"
      yy$(21) = "02^FO1233,1088^C10^A0N,36,36^FR^FDMODEL/MODELO # "
      yy$(22) = "03^FO1229,1143^CI0^A0N,150,90^FR^FD"
      yy$(23) = "03^FO1233,1293^CI0^A0N,25,16^FR^FDOpening Size: "
      yy$(24) = "04^FO1233,1328^CI0^A0N,25,16^FR^FDActual Size: "
      yy$(25) = "03^FO1483,1293^CI0^A0N,25,16^FR^FDTama¤o de la Apertura: "
      yy$(26) = "04^FO1483,1328^CI0^A0N,25,16^FR^FDTama¤o Exacto: "
      yy$(27) = "15^FO1233,1388^CI0^A0N,25,16^FR^FDProduction Date: "
      yy$(28) = "16^FO1483,1388^CI0^A0N,25,16^FR^FDDept. #"
      yy$(29) = "17^FO1598,1388^CI0^A0N,25,16^FR^FD Sequence #"
      yy$(30) = "15^FO1233,1423^CI0^A0N,25,16^FR^FDFecha de produccion: "
      yy$(31) = "16^FO1483,1423^CI0^A0N,25,16^FR^FDDpto. #"
      yy$(32) = "17^FO1598,1423^CI0^A0N,25,16^FR^FD Serie #"
      yy$(33) = "^PQ1^XZ"
        return

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


mcoys_fmt
      init(" ") yy$()
      yy$(01) = ibeg$ & "R" & iend$
      yy$(02) = "<xpml><page quantity='0' pitch='228.6 mm'></xpml>"
      yy$(03) = ibeg$ & "<ESC>C<SI>W1124<SI>h" & iend$
      yy$(04) = "<xpml></page></xpml><xpml><page quantity='1' pitch~
                ~='228.6 mm'></xpml>"
      yy$(05) = ibeg$ & "<ESC>P" & iend$
      yy$(06) = ibeg$ & "F*" & iend$
      yy$(07) = "01" & ibeg$ & "B1;f1;o619,1088;c7,1,3;w4;h258;i1;d3,"
      yy$(08) = "02" & ibeg$ & "H2;f1;o1025,1107;c61;b0;h21;w21;d3,"
      yy$(09) = ibeg$ & "H3;f1;o1700,1106;c61;b0;h21;w21;d3,Date :" & iend$
      yy$(10) = "03" & ibeg$ & "H4;f1;o1982,1107;c61;b0;h25;w25;d3,"
      yy$(11) = ibeg$ & "H5;f1;o2225,1107;c61;b0;h29;w29;d3,P.O. #  " & iend$
      yy$(12) = ibeg$ & "H6;f1;o2413,1107;c61;b0;h25;w25;d3,ORDER & LINE " & iend$
      yy$(13) = ibeg$ & "H7;f1;o2225,525;c61;b0;h29;w29;d3,P.O. #  " & iend$
      yy$(14) = ibeg$ & "H8;f1;o2413,525;c61;b0;h25;w25;d3,ORDER & LINE " & iend$
      yy$(15) = ibeg$ & "H9;f1;o311,994;c61;b0;h34;w34;d3,SKU #" & iend$
      yy$(16) = "04" & ibeg$ & "H10;f1;o405,1088;c61;b0;h34;w34;d3,"
      yy$(17) = "05" & ibeg$ & "H11;f1;o405,507;c61;b0;h34;w34;d3,"
      yy$(18) = ibeg$ & "H12;f1;o311,432;c61;b0;h34;w34;d3,SKU #" & iend$
      yy$(19) = "06" & ibeg$ & "H13;f1;o1700,993;c61;b0;h21;w24;d3,"
      yy$(20) = ibeg$ & "H14;f1;o1700,543;c61;b0;h21;w21;d3,Date :" & iend$
      yy$(21) = "07" & ibeg$ & "H15;f1;o1700,413;c61;b0;h21;w24;d3,"
      yy$(22) = ibeg$ & "H16;f1;o1757,1103;c61;b0;h21;w21;d3,Load:" & iend$
      yy$(23) = "08" & ibeg$ & "H17;f1;o1757,993;c61;b0;h21;w24;d3,"
      yy$(24) = "09" & ibeg$ & "H18;f1;o1757,413;c61;b0;h21;w24;d3,"
      yy$(25) = ibeg$ & "H19;f1;o1757,540;c61;b0;h21;w21;d3,Load:" & iend$
      yy$(26) = ibeg$ & "H20;f1;o1813,1101;c61;b0;h21;w21;d3,Dept:" & iend$
      yy$(27) = "10" & ibeg$ & "H21;f1;o1813,993;c61;b0;h21;w24;d3,"
      yy$(28) = "11" & ibeg$ & "H22;f1;o1813,413;c61;b0;h21;w24;d3,"
      yy$(29) = ibeg$ & "H23;f1;o1813,538;c61;b0;h21;w21;d3,Dept:"& iend$
      yy$(30) = "12" & ibeg$ & "H24;f1;o1982,544;c61;b0;h25;w25;d3,"
      yy$(31) = "13" & ibeg$ & "B25;f1;o619,525;c7,1,3;w4;h258;i1;d3,"
      yy$(32) = "14" & ibeg$ & "H26;f1;o2300,1107;c61;b0;h25;w25;d3,"
      yy$(33) = "15" & ibeg$ & "H27;f1;o2300,525;c61;b0;h25;w25;d3,"
      yy$(34) = "16" & ibeg$ & "H28;f1;o2469,1107;c61;b0;h25;w25;d3,"
      yy$(35) = "17" & ibeg$ & "H29;f1;o2469,525;c61;b0;h25;w25;d3,"
      yy$(36) = "18" & ibeg$ & "H30;f1;o1082,1107;c61;b0;h21;w21;d3,"
      yy$(37) = "19" & ibeg$ & "H31;f1;o1138,1107;c61;b0;h21;w21;d3,"
      yy$(38) = "20" & ibeg$ & "H32;f1;o1194,1107;c61;b0;h21;w21;d3," & iend$
      yy$(39) = "21" & ibeg$ & "H33;f1;o1250,1107;c61;b0;h21;w21;d3," & iend$
      yy$(40) = "22" & ibeg$ & "H34;f1;o1402,1107;c61;b0;h17;w17;d3,Exact Width: "
      yy$(41) = "23" & ibeg$ & "H35;f1;o1442,1107;c61;b0;h17;w17;d3,Exact Height: "
      yy$(42) = "24" & ibeg$ & "H36;f1;o1025,525;c61;b0;h21;w21;d3,"
      yy$(43) = "25" & ibeg$ & "H37;f1;o1082,525;c61;b0;h21;w21;d3,"
      yy$(44) = "26" & ibeg$ & "H38;f1;o1138,525;c61;b0;h21;w21;d3,"
      yy$(45) = "27" & ibeg$ & "H39;f1;o1194,525;c61;b0;h21;w21;d3," & iend$
      yy$(46) = "28" & ibeg$ & "H40;f1;o1250,525;c61;b0;h21;w21;d3," & iend$

      yy$(47) = ibeg$ & "H41;f1;o1869,1099;c61;b0;h21;w21;d3,Seq :" & iend$
      yy$(48) = "29" & ibeg$ & "H42;f1;o1869,993;c61;b0;h21;w24;d3,"
      yy$(49) = ibeg$ & "H43;f1;o1869,537;c61;b0;h21;w21;d3,Seq :" & iend$
      yy$(50) = "30" & ibeg$ & "H44;f1;o1869,413;c61;b0;h21;w24;d3,"



      yy$(51) = "31" & ibeg$ & "H45;f1;o1402,525;c61;b0;h17;w17;d3,Exact Width: "
      yy$(52) = "32" & ibeg$ & "H46;f1;o1442,525;c61;b0;h17;w17;d3,Exact Height: "
      yy$(53) = ibeg$ & "D0"& iend$
      yy$(54) = ibeg$ & "R"& iend$
      yy$(55) = ibeg$ & "<SI>l13" & iend$
      yy$(56) = ibeg$ & "<ESC>E*,1<CAN>" & iend$
      yy$(57) = ibeg$ & "<RS>1<US>1<ETB>"& iend$
      yy$(58) = ibeg$ & "<xpml></page></xpml><xpml><end/></xpml>"

      return
/* (AWD001) */
series85_fmt
      init(" ") yy$()
      yy$(01) = ibeg$ & "R" & iend$
      yy$(02) = "<xpml><page quantity='0' pitch='257.2 mm'></xpml>"
      yy$(03) = ibeg$ & "<ESC>C<SI>W1162<SI>h" & iend$
      yy$(04) = "<xpml></page></xpml><xpml>"
      yy$(05) = "<page quantity='1' pitch='257.2 mm'></xpml>"
      yy$(06) = ibeg$ & "<ESC>P" & iend$
      yy$(07) = ibeg$ & "F*" & iend$
      yy$(08) = "01" & ibeg$ & "B1;f1;o565,1059;c7,0,3;w3;h188;i1;d3,"
      yy$(09) = "01" & ibeg$ & "H2;f1;o786,1173;c61;b0;h18;w18;d3,MODEL/MODELO# "
      yy$(10) = "33" & ibeg$ & "H3;f1;o827,1126;c61;b0;h34;w34;d3,"
      yy$(11) = "33" & ibeg$ & "H4;f1;o917,1160;c61;b0;h25;w17;d3,Opening Size:  "
      yy$(12) = "13" & ibeg$ & "H5;f1;o786,597;c61;b0;h18;w18;d3,MODEL/MODELO# "
      yy$(13) = "33" & ibeg$ & "H6;f1;o979,1160;c61;b0;h15;w15;d3,Tamano de la Aperturna:  "
      yy$(14) = "34" & ibeg$ & "H7;f1;o1088,1160;c61;b0;h15;w15;d3,Tamano Exacto:  "
      yy$(15) = "34" & ibeg$ & "H8;f1;o1025,1160;c61;b0;h25;w17;d3,Actual Size:  "
      yy$(16) = "06" & ibeg$ & "H9;f1;o1124,1160;c61;b0;h17;w17;d3,Production Date:  "
      yy$(17) = "10" & ibeg$ & "H10;f1;o1168,1160;c61;b0;h17;w17;d3,Dept:  #"
      yy$(18) = "29" & ibeg$ & "H11;f1;o1168,897;c61;b0;h17;w17;d3,Seq:  #"
      yy$(19) = "04" & ibeg$ & "H12;f1;o457,440;c61;b0;h43;w43;d3,"
      yy$(20) = ibeg$ & "H13;f1;o469,481;c61;b0;h25;w25;d3,#" & iend$
      yy$(21) = "13" & ibeg$ & "B14;f1;o565,467;c7,0,3;w3;h188;i1;d3,"
      yy$(22) = ibeg$ & "H15;f1;o461,1073;c61;b0;h25;w25;d3,#" & iend$
      yy$(23) = "05" & ibeg$ & "H16;f1;o457,1030;c61;b0;h43;w43;d3,"
      yy$(24) = "33" & ibeg$ & "H17;f1;o827,535;c61;b0;h34;w34;d3,"
      yy$(25) = "33" & ibeg$ & "H18;f1;o913,565;c61;b0;h25;w17;d3,Opening Size:  "
      yy$(26) = "33" & ibeg$ & "H19;f1;o976,565;c61;b0;h15;w15;d3,Tamano de la Aperturna:  "
      yy$(27) = "34" & ibeg$ & "H20;f1;o1084,565;c61;b0;h15;w15;d3,Tamano Exacto:  "
      yy$(28) = "34" & ibeg$ & "H21;f1;o1022,565;c61;b0;h25;w17;d3,Actual Size:  "
      yy$(29) = "07" & ibeg$ & "H22;f1;o1124,565;c61;b0;h17;w17;d3,Production Date:  "
      yy$(30) = "11" & ibeg$ & "H23;f1;o1168,565;c61;b0;h17;w17;d3,Dept:  #"
      yy$(31) = "30" & ibeg$ & "H24;f1;o1168,298;c61;b0;h17;w17;d3,Seq:  #"
      yy$(32) = ibeg$ & "D0" & iend$
      yy$(33) = ibeg$ & "R" & iend$
      yy$(34) = ibeg$ & "<SI>l13" & iend$
      yy$(35) = ibeg$ & "<ESC>E*,1<CAN>" & iend$
      yy$(36) = ibeg$ & "<RS>1<US>1" & iend$
      yy$(37) = ibeg$ & "<ETB><FF>" & iend$
      yy$(38) = "<xpml></page></xpml><xpml><end/></xpml>"

      return
/* (\AWD001) */


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
                CH(01)                   /* New Special Mull code(PAR006)*/

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
        init(" ") yy$()                           /* (EWD009)         */

        return

        file_exists
          comp% = 2%
          hdr$ = "*** Production File Exists ***"
                                          /* (PAR003)             */
          if schema% = 1% then                                          ~
             msg$(1%) = "        The File (MFGPROD) Already Exists.      "~
                          else                                             ~
             msg$(1%) = "        The File (NEPROD) Already Exists.       "
                                          /* (PAR003)             */
          msg$(2%) = "       P r o d u c t i o n   L a b e l s         "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                      /* (PAR003)     */
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

                                                      /* (PAR003)     */




