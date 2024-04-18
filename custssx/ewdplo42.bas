        REM *************************************************************~
            *            (RHHTEST)    Test Print Turned (off)            *~
            *                                                           *~
            *      Note- Bay-Bow Test for Line = 25%  in yy$(??)        *~
            *      Note- UPC Test for Line     = 86% (Turned Off)       *~
            *                                        (AWD028)           *~
            *      Note- 100% Inspection test for Line = 27% in yy$(??) *~
            *                                 and Line = 102% (Text)    *~
            *  Subroutine Name   - EWDPLO42                             *~
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
            * EWDPLO42 - Generates the label format and data to print   *~
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
            *11/24/2008! Original - Copied & Mod (sub) EWDPLO42.  ! DES *~
            *09/25/2013! (AWD001) Series 85 modifications         ! CMG *~
            *07/29/2014! (AWD002) New Lowes Stock                 ! PWW *~
            *03/15/2018! (SR84033) Allow B orders to find bcksubpt! RDB *~
            *************************************************************

        sub "EWDPLO42" (been_here%,      /* Zero (Only 1st Time)       */~
                        rec$(),          /* Prod Label Data    (PAR002)*/~
                        #1,              /* GENCODES           (EWD018)*/~
                        #2,              /* APCPLNDT           (EWD022)*/~
                        #4,              /* BCKLINES           (AWD007)*/~
                        #3,              /* BCKSUBPT           (PAR000)*/~
                        #7,              /* AWDSKUXR           (PAR000)*/~
                        #8,              /* APCPCMST           (EWD022)*/~
                        sc_sel$,         /* Label Type         (PAR000)*/~
                        sc_sel_p$,       /* Printer Number             */~
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
            rec$(4%)256                  /* Rec Array Prod. Lbl(PAR002)*/~

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
            filler$6,                    /* Old Customer Code  (EWD018)*/~
            blank$6,             /* New Customer Code  (EWD018)*/~
            lb_cust_code$9,              /* New Customer Code  (EWD018)*/~
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
            sc_sel_p$1,                  /* Printer Number             */~
            ibeg$5,                      /* Intermec Beg Command       */~
            iend$5                       /* Intermec End Command       */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20,                 /* Text from file opening     */~
            sc_sel%
            
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            dt_sub_part$20,              /* New Sub Part No.           */~
            dt_sub_info$20,              /* New Sub Info Fields (9)+11 */~
            series$16, style$10          /* series & style             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate Production Labels        "
            pname$ = "EWDPLP42 - Rev: R8.00"

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
               if sc_sel_p$ = "2" then script$ = "NELOWE2"     
                  
/* (AWD001) */
/*             if sc_sel$ = "10" then file$   = "MNELOWE"  */
/*             if sc_sel$ = "10" then script$ = "MNELOWE"  */
/*             if sc_sel$ = "11" then file$   = "ALUMLOWE"
/*             if sc_sel$ = "11" then script$ = "ALUMLOWE"*/
/*             if sc_sel$ = "12" then file$   = "ALUMLOWE"*/
/*             if sc_sel$ = "12" then script$ = "ALUMLOWE"*/
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
                lb_oth1$, lb_frame$, blank$, filler$, lb_ups$, ~
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

/*        if sc_sel$ = "10" then goto format_mccoys     */
/* (AWD001) */
/*        if sc_sel$ = "11" then goto format_mccoys     */
/*        if sc_sel$ = "12" then goto format_mccoys     */
/* (\AWD001) */
REM   01 = sku Field "A" - Top 6 digit number
          lbl$(01%) = dt_sku$ & fs$
REM   02 = l_model
          lbl$(02%) = l_model$ & fs$
REM   03 = oSize Field "C" - Opening Larger font WxH
          lbl$(03%) = oWidth$ & " in x " & oHeight$ & " in Openings" & fs$
REM   04 = eSize Field "D" - Exact Size
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
          lbl$(13%) = "Aberturas de " & oWidth$ & " pulg x " & oHeight$ & " pulg" & fs$
REM   14 = eSizeSp
          lbl$(14%) = eWidth$ & " pulg x " & eHeight$ & " pulg" & fs$
REM   15 = date
          lbl$(15%) = str(lb_date$,5,2) & "/" & str(lb_date$,7,2) & "/" &  ~
      str(lb_date$,3,2) & fs$
REM   16 = dept
          lbl$(16%) = lb_dept$ & fs$
REM   17 = sequence
          lbl$(17%) = lb_seq$ & fs$
REM   18 = model - check dgt Field "B" Barcode
          lbl$(18%) = str(l_model$,1,11) & fs$
/*        lbl$(18%) = "04997314010" & fs$       temp for testing*/
          
REM   19 = Nominal Size - lb_nomsz$
          lbl$(19%) = lb_nomsz$ & fs$

REM   20 = Series number
          lbl$(20%) = series$ & fs$
          
REM   21 - 33 = UPC individual numbers          
      i% = 21%
      for r% = 1% to 12%
           lbl$(i%) = str(l_model$,r%,1%) & fs$
           i% = i% + 1%
      next r%          

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

      if sc_sel$ <> "01" and                                                ~
         sc_sel$ <> "02" and                                                ~
         sc_sel$ <> "08" and                                                ~
         sc_sel$ <> "09" then goto narrow_label
         
         gosub series_450
             copy yy$() to xx$()
         lbl% = lbl% + 1%
         goto read_loop
         
narrow_label:
         gosub series_3100
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
/*          dt_sku$ = "123456" : sku% = 1%        temp for testing*/
/*          lbl$(18%) = "04997314010" & fs$       temp for testing*/
/*          goto good_sel                         temp for testing*/
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
/*        if sc_sel$ = "10" then sku_key$ = "X_MB" & dt_sku$ & "   "  */

          read #7, key = sku_key$, using AWDSKUXR, model$, l_model$, part$,~
                                           l_desc$,   eod goto AWDSKUXR
              sku% = 1%
/*
 cmg === below is where my selection logic begins. Am I on the right track?
         And do I need the logic directly above?

 pww -> yes it looks like you are on the correct path.
 one observation that has create a bug for me before
   I usually would put a str() test around series and style becuase if there
   was a series 4500 then I believe the second if statement would be true as well
     if series$ = "3201" and style$ = "DH" then goto good_sel
     if series$ = "450" and style$ = "DH" then goto good_sel


     if the series and style becomes a headache or doesn't work out well
     you can always us the model number but it is better to use series bc
     multiple models can be one series.


     to look at series and style in cyberquery run
        list modelnumbermap:model
        product
        series
        style
        description
        notes
        paneloptiontitle
        brand
        revision
        manfplants
        dateeffective
        mapid
        rec_create_date
        rec_update_date
        label
        created_by
        updated_by


*/
              init(" ") so_inv$, item_no$     /* CR1403 */
              so_inv$  = str(lb_bc$,1%,8%)   /* CR1403 */
              item_no$ = str(lb_bc$,9%,2%)   /* CR1403 */
              gosub lookup_sub_part
              series$ = str(bcksubpt_rec$,169%,16%)
              style$  = str(bcksubpt_rec$,185%,10%)
              convert sc_sel$ to sc_sel%, data goto check_sel
/*            goto good_sel                          temp for testing */
          check_sel
              on sc_sel% goto sel_01,          /*Series 3201 DH*/            ~
                              sel_02,          /*Series 3201 DH w/grid*/     ~
                              sel_03,          /*Series 3050 SH*/            ~
                              sel_04,          /*Series 3050 SH w/grid*/     ~
                              sel_05,          /*Series 3050 Slide*/         ~
                              sel_06,          /*Series 3100 SH*/            ~
                              sel_07,          /*Series 3100 SH w/grid*/     ~
                              sel_08,          /*Series 450 DHHP*/           ~
                              sel_09,          /*Series 450 DHHP w/grid*/    ~
                              sel_10,          /*Series 105 SH*/             ~
                              sel_11,          /*Series 105 SH w/grid*/      ~
                              sel_12,          /*Series 105 Slider*/         ~
                              sel_13,          /*Series 150 SH*/             ~
                              sel_14,          /*Series 150 SH w/grid*/      ~
                              sel_15,          /*Series 85 SH*/              ~
                              sel_16,          /*Series 85 SH w/grid*/       ~
                              sel_17,          /*Series 85 SH Bronze*/       ~
                              sel_18,          /*Series 85 SH w/grid Bronze*/~
                              sel_19,          /*Series 85 Slider*/          ~
                              sel_20,          /*Series 151 Slider*/         ~
/*SR70000*/                   sel_21           /*Series 130 Slider*/
              goto bad_sel
              
          sel_01                                           /*Series 3201 DH*/
              if str(series$,1%,8%) = "3201    " and                         ~
              str(style$,1%,8%) = "DH      " and                             ~
              str(part$,7,2) = "00" then goto good_sel  
              goto bad_sel
              
          sel_02                                    /*Series 3201 DH w/grid*/
              if str(series$,1%,8%) = "3201    " and                         ~
              str(style$,1%,8%) = "DH      " and                             ~
              str(part$,7,2) <> "00" then goto good_sel
              goto bad_sel
             
          sel_03                                           /*Series 3050 SH*/               
              if (str(series$,1%,8%) = "3050    " or                         ~
                  str(series$,1%,8%) = "8050    ") and                       ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) = "00" then goto good_sel
              goto bad_sel
          
          sel_04                                    /*Series 3050 SH w/grid*/
              if (str(series$,1%,8%) = "3050    " or                         ~
                  str(series$,1%,8%) = "8050    ") and                       ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) <> "00" then goto good_sel
              goto bad_sel
          
          sel_05                                        /*Series 3050 Slide*/
              if (str(series$,1%,8%) = "3050    " or                         ~
                  str(series$,1%,8%) = "8050    ") and                       ~
              str(style$,1%,8%) = "2SL     " and                             ~
              str(part$,7,2) = "00" then goto good_sel
              goto bad_sel
          
          sel_06                                           /*Series 3100 SH*/
              if str(series$,1%,8%) = "3100    " and                         ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) = "00" then goto good_sel
              goto bad_sel
          
          sel_07                                    /*Series 3100 SH w/grid*/
              if str(series$,1%,8%) = "3100    " and                         ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) <> "00" then goto good_sel
              goto bad_sel
          
          sel_08                                          /*Series 450 DHHP*/
              if str(series$,1%,8%) = "450     " and                         ~
              str(style$,1%,8%) = "DHHP    " and                             ~
              str(part$,7,2) = "00" then goto good_sel
              goto bad_sel
          
          sel_09                                   /*Series 450 DHHP w/grid*/
              if str(series$,1%,8%) = "450     " and                         ~
              str(style$,1%,8%) = "DHHP    " and                             ~
              str(part$,7,2) <> "00" then goto good_sel
              goto bad_sel
          
          sel_10                                            /*Series 105 SH*/
              if str(series$,1%,8%) = "105     " and                         ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) = "00" then goto good_sel
              goto bad_sel
          
          sel_11                                     /*Series 105 SH w/grid*/
              if str(series$,1%,8%) = "105     " and                         ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) <> "00"then goto good_sel
              goto bad_sel
          
          sel_12                                         /*Series 105 Slider*/
              if str(series$,1%,8%) = "105     " and                         ~
              str(style$,1%,8%) = "2SL     " and                             ~
              str(part$,7,2) = "00"then goto good_sel
              goto bad_sel
          
          sel_13                                            /*Series 150 SH*/
              if str(series$,1%,8%) = "150     " and                         ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) = "00" then goto good_sel
              goto bad_sel
          
          sel_14                                     /*Series 150 SH w/grid*/
              if str(series$,1%,8%) = "150     " and                         ~
              str(style$,1%,8%) = "SH      " and                             ~
              str(part$,7,2) <> "00" then goto good_sel
              goto bad_sel
          
          sel_15                                             /*Series 85 SH*/
              if (str(series$,1%,8%) = "85      " or                         ~
                  str(series$,1%,8%) = "300     ") and                       ~
                 (str(style$,1%,8%) = "SH      "  or                         ~
                  str(style$,1%,8%) = "SHHP    ")  and                       ~
              str(part$,4,1) <> "Q" and                                      ~
              str(part$,7,2) = "00" then goto good_sel
              goto bad_sel
          
          sel_16                                      /*Series 85 SH w/grid*/
              if (str(series$,1%,8%) = "85      " or                         ~
                  str(series$,1%,8%) = "300     ") and                       ~
                 (str(style$,1%,8%) = "SH      "  or                         ~
                  str(style$,1%,8%) = "SHHP    ")  and                       ~
              str(part$,4,1) <> "Q" and                                      ~
              str(part$,7,2) <> "00" then goto good_sel
              goto bad_sel
          
          sel_17                                      /*Series 85 SH Bronze*/
              if (str(series$,1%,8%) = "85      " or                         ~
                  str(series$,1%,8%) = "300     ") and                       ~
                 (str(style$,1%,8%) = "SH      "  or                         ~
                  str(style$,1%,8%) = "SHHP    ")  and                       ~
              str(part$,7,2) = "00" and                                      ~
              str(part$,4,1) = "Q" then goto good_sel
              goto bad_sel
          
          sel_18                               /*Series 85 SH w/grid Bronze*/
              if (str(series$,1%,8%) = "85      " or                         ~
                  str(series$,1%,8%) = "300     ") and                       ~
                 (str(style$,1%,8%) = "SH      "  or                         ~
                  str(style$,1%,8%) = "SHHP    ")  and                       ~
              str(part$,7,2) <> "00" and                                     ~
              str(part$,4,1) = "Q" then goto good_sel
              goto bad_sel
          
          sel_19                                         /*Series 85 Slider*/
              if str(series$,1%,8%) = "85      " and                         ~
              str(style$,1%,8%) = "2SL     " then goto good_sel
              goto bad_sel
          
          sel_20                                        /*Series 151 Slider*/
              if str(series$,1%,8%) = "150     " and                         ~
              str(style$,1%,8%) = "2SL     " then goto good_sel
              goto bad_sel
              
          sel_21                                        /*Series 130 Slider*/
              if str(series$,1%,8%) = "130     " and                         ~
              str(style$,1%,8%) = "2SL     " then goto good_sel
              goto bad_sel
/*
 cmg === This is the end of my selection logic.  But you mentioned to me that
         if series is = "" then use current logic so should I leave this logic below?
 pww -> i thought you were asking about the NFRC labels not the lowes stock labels
    different project, i was confused.
*/
          
/*            if sc_sel$ = "10" then goto good_sel     */
/* (AWD001) */
/*            if sc_sel$ <> "11" and sc_sel$ <> "12" then goto check_others
                 if str(sku_key$,1%,4%) <> "X_LO" then goto bad_sel
REM sc_sel$ = 12 then with grid Series 85
                 if str(part$,7,2) = "00" and sc_sel$ = "12" then goto bad_sel
                 if str(part$,7,2) <> "00" and sc_sel$ = "11" then goto bad_sel
                   if model$  = "3W1" then goto good_sel
                   if model$  = "3H1" then goto good_sel
                     goto bad_sel
check_others:  */
/* (\AWD001) */
/*            if sc_sel$ = "03" and model$ = "126" then goto good_sel
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
*/
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


/* Narrow label */
series_85
series_105
series_151
series_150
series_3050
series_3100
      init(" ") yy$()                           /* (EWD009)         */
REM move left 1/8" (25 points)

      yy$(01) = "^JO^XA^EG^XZ^XA^PMN^MNY^MMT^MD0"
      yy$(02) = "^LH0,20^PR4^JMA"
      yy$(03) = "01^FO0154,2413^CI0^A0N,158,154^FR^FD"
      yy$(04) = "01^FO0858,2413^CI0^A0N,158,154^FR^FD"  
      yy$(05) = "18^FO0232,2540^BY3^BUN,122,Y,N,Y^FR^FD"
      yy$(06) = "18^FO0935,2540^BY3^BUN,123,Y,N,Y^FR^FD"
      yy$(07) = "19^FO0226,2765^CI0^A0N,158,158^FR^FD"
      yy$(08) = "19^FO0927,2765^CI0^A0N,158,158^FR^FD"
      yy$(09) = "^FO183,2919^CI0^A0N,31,33^FR^FDWIDTH^FS"
      yy$(10) = "^FO0884,2919^CI0^A0N,31,33^FR^FDWIDTH^FS"
      yy$(11) = "^FO0473,2919^CI0^A0N,31,33^FR^FDHEIGHT^FS"
      yy$(12) = "^FO1177,2919^CI0^A0N,31,33^FR^FDHEIGHT^FS"
      yy$(13) = "05^FO0152,2970^CI0^A0N,110,110^FR^FD"
      yy$(14) = "06^FO0426,2970^CI0^A0N,110,110^FR^FD"
      yy$(15) = "05^FO0856,2970^CI0^A0N,110,110^FR^FD"
      yy$(16) = "06^FO1129,2970^CI0^A0N,110,110^FR^FD"
      yy$(17) = "^FO0271,2970^CI0^A0N,68,68^FR^FDin  X^FS"
      yy$(18) = "^FO0545,2970^CI0^A0N,68,68^FR^FDin^FS"
      yy$(19) = "^FO0978,2970^CI0^A0N,68,68^FR^FDin  X^FS"
      yy$(20) = "^FO1250,2970^CI0^A0N,68,68^FR^FDin^FS"

      yy$(21) = "^FO0106,3088^CI0^A0N,17,19^FR^FDExact Size:^FS"
      yy$(22) = "^FO0392,3088^CI0^A0N,17,19^FR^FDTamano exacto:^FS"
      yy$(23) = "^FO0811,3088^CI0^A0N,17,19^FR^FDExact Size:^FS"
      yy$(24) = "^FO1096,3088^CI0^A0N,17,19^FR^FDTamano exacto:^FS"
      yy$(25) = "04^FO0107,3113^CI0^A0N,17,17^FR^FD"
      yy$(26) = "14^FO0395,3113^CI0^A0N,17,17^FR^FD"
      yy$(27) = "04^FO0811,3113^CI0^A0N,17,17^FR^FD"
      yy$(28) = "14^FO1098,3112^CI0^A0N,17,17^FR^FD"
      yy$(29) = "^FO0107,3149^CI0^A0N,17,19^FR^FDReplaces:^FS"
      yy$(30) = "^FO0394,3149^CI0^A0N,17,19^FR^FDReemplaza:^FS"
      yy$(31) = "^FO0810,3149^CI0^A0N,17,19^FR^FDReplaces:^FS"
      yy$(32) = "^FO1096,3149^CI0^A0N,17,19^FR^FDReemplaza:^FS"
      yy$(33) = "03^FO0109,3175^CI0^A0N,17,19^FR^FD"
      yy$(34) = "13^FO0393,3175^CI0^A0N,17,19^FR^FD"
      yy$(35) = "03^FO0812,3175^CI0^A0N,17,19^FR^FD"
      yy$(36) = "13^FO1098,3175^CI0^A0N,17,19^FR^FD"
      yy$(37) = "^FO0106,3215^CI0^A0N,17,19^FR^FDProduction Date:^FS"
      yy$(38) = "^FO0105,3242^CI0^A0N,17,19^FR^FDFecha de produccion:^FS"
      yy$(39) = "15^FO0235,3216^CI0^A0N,17,19^FR^FD"
      yy$(40) = "15^FO0273,3241^CI0^A0N,17,19^FR^FD"
      yy$(41) = "^FO0378,3215^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(42) = "^FO0378,3242^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(43) = "16^FO0434,3216^CI0^A0N,17,19^FR^FD"
      yy$(44) = "16^FO0434,3242^CI0^A0N,17,19^FR^FD"
      yy$(45) = "^FO0502,3215^CI0^A0N,17,19^FR^FDSequence #^FS"
      yy$(46) = "^FO0502,3241^CI0^A0N,17,19^FR^FDSecuencia #^FS"
/*    yy$(47) = "17^FO0594,3216^CI0^A0N,17,19^FR^FD"  */
      yy$(47) = "17^FO0594,3216^CI0^A0N,21,23^FR^FD"
      yy$(48) = "17^FO0596,3242^CI0^A0N,21,23^FR^FD"
      yy$(49) = "^FO0808,3215^CI0^A0N,17,19^FR^FDProduction Date:^FS"
      yy$(50) = "15^FO0939,3215^CI0^A0N,17,19^FR^FD"
      yy$(51) = "^FO0808,3241^CI0^A0N,17,19^FR^FDFecha de produccion:^FS"
      yy$(52) = "15^FO0976,3241^CI0^A0N,17,20^FR^FD"
      yy$(53) = "^FO1083,3215^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(54) = "^FO1083,3242^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(55) = "16^FO1141,3216^CI0^A0N,17,17^FR^FD"
      yy$(56) = "16^FO1141,3242^CI0^A0N,17,17^FR^FD"
      yy$(57) = "^FO1206,3216^CI0^A0N,17,19^FR^FDSequence #^FS"
      yy$(58) = "^FO1206,3242^CI0^A0N,17,19^FR^FDSecuencia #^FS"
      yy$(59) = "17^FO1297,3216^CI0^A0N,21,23^FR^FD"
      yy$(60) = "17^FO1299,3242^CI0^A0N,21,23^FR^FD"
      yy$(61) = "^PQ1^XZ"
        return

        
        /* Wide label */
series_3201
series_450
      init(" ") yy$()                           /* (EWD009)         */
      yy$(01) = "^JO^XA^EG^XZ^XA^PMN^MNY^MMT^MD0"
      yy$(02) = "^LH100,20^PR4^JMA"
      yy$(03) = "01^FO0201,2413^CI0^A0N,147,143^FR^FD"
      yy$(04) = "01^FO1154,2413^CI0^A0N,147,143^FR^FD"  
      yy$(05) = "18^FO0263,2540^BY3^BUN,123,Y,N,Y^FR^FD"
      yy$(06) = "18^FO1217,2540^BY3^BUN,123,Y,N,Y^FR^FD"
      yy$(07) = "19^FO0258,2765^CI0^A0N,158,158^FR^FD"
      yy$(08) = "19^FO1212,2765^CI0^A0N,158,158^FR^FD"
      yy$(09) = "^FO223,2919^CI0^A0N,31,33^FR^FDWIDTH^FS"
      yy$(10) = "^FO1178,2919^CI0^A0N,31,33^FR^FDWIDTH^FS"
      yy$(11) = "^FO0478,2919^CI0^A0N,31,33^FR^FDHEIGHT^FS"
      yy$(12) = "^FO1428,2919^CI0^A0N,31,33^FR^FDHEIGHT^FS"
      yy$(13) = "05^FO0197,2970^CI0^A0N,110,110^FR^FD"
      yy$(14) = "06^FO0452,2970^CI0^A0N,110,110^FR^FD"
      yy$(15) = "05^FO1147,2970^CI0^A0N,110,110^FR^FD"
      yy$(16) = "06^FO1402,2970^CI0^A0N,110,110^FR^FD"
      yy$(17) = "^FO0311,2970^CI0^A0N,68,68^FR^FDin  X^FS"
      yy$(18) = "^FO0563,2970^CI0^A0N,68,68^FR^FDin^FS"
      yy$(19) = "^FO1259,2970^CI0^A0N,68,68^FR^FDin  X^FS"
      yy$(20) = "^FO1516,2970^CI0^A0N,68,68^FR^FDin^FS"

      yy$(21) = "^FO0157,3088^CI0^A0N,17,19^FR^FDExact Size:^FS"
      yy$(22) = "^FO0426,3088^CI0^A0N,17,19^FR^FDTamano exacto:^FS"
      yy$(23) = "^FO1109,3088^CI0^A0N,17,19^FR^FDExact Size:^FS"
      yy$(24) = "^FO1376,3088^CI0^A0N,17,19^FR^FDTamano exacto:^FS"
      yy$(25) = "04^FO0157,3112^CI0^A0N,17,17^FR^FD"
      yy$(26) = "14^FO0426,3112^CI0^A0N,17,17^FR^FD"
      yy$(27) = "04^FO1108,3112^CI0^A0N,17,17^FR^FD"
      yy$(28) = "14^FO1376,3112^CI0^A0N,17,17^FR^FD"
      yy$(29) = "^FO0157,3147^CI0^A0N,17,19^FR^FDReplaces:^FS"
      yy$(30) = "^FO0426,3147^CI0^A0N,17,19^FR^FDReemplaza:^FS"
      yy$(31) = "^FO1108,3147^CI0^A0N,17,19^FR^FDReplaces:^FS"
      yy$(32) = "^FO1376,3147^CI0^A0N,17,19^FR^FDReemplaza:^FS"
      yy$(33) = "03^FO0156,3176^CI0^A0N,17,19^FR^FD"
      yy$(34) = "13^FO0426,3176^CI0^A0N,17,19^FR^FD"
      yy$(35) = "03^FO1109,3176^CI0^A0N,17,19^FR^FD"
      yy$(36) = "13^FO1375,3176^CI0^A0N,17,19^FR^FD"
      yy$(37) = "^FO0157,3216^CI0^A0N,17,19^FR^FDProduction Date:^FS"
      yy$(38) = "^FO0157,3242^CI0^A0N,17,19^FR^FDFecha de produccion:^FS"
      yy$(39) = "15^FO0288,3216^CI0^A0N,17,19^FR^FD"
      yy$(40) = "15^FO0326,3241^CI0^A0N,17,19^FR^FD"
      yy$(41) = "^FO0412,3215^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(42) = "^FO0412,3240^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(43) = "16^FO0468,3215^CI0^A0N,17,19^FR^FD"
      yy$(44) = "16^FO0468,3240^CI0^A0N,17,19^FR^FD"
      yy$(45) = "^FO0519,3215^CI0^A0N,17,19^FR^FDSequence #^FS"
      yy$(46) = "^FO0519,3240^CI0^A0N,17,19^FR^FDSecuencia #^FS"
      yy$(47) = "17^FO0610,3215^CI0^A0N,21,23^FR^FD"
      yy$(48) = "17^FO0610,3240^CI0^A0N,21,23^FR^FD"
      yy$(49) = "^FO1108,3216^CI0^A0N,17,19^FR^FDProduction Date:^FS"
      yy$(50) = "15^FO1238,3216^CI0^A0N,17,19^FR^FD"
      yy$(51) = "^FO1109,3239^CI0^A0N,17,19^FR^FDFecha de produccion:^FS"
      yy$(52) = "15^FO1277,3240^CI0^A0N,17,20^FR^FD"
      yy$(53) = "^FO1362,3216^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(54) = "^FO1362,3240^CI0^A0N,17,19^FR^FDDept. #^FS"
      yy$(55) = "16^FO1418,3215^CI0^A0N,17,17^FR^FD"
      yy$(56) = "16^FO1418,3240^CI0^A0N,17,17^FR^FD"
      yy$(57) = "^FO1474,3216^CI0^A0N,17,19^FR^FDSequence #^FS"
      yy$(58) = "^FO1471,3240^CI0^A0N,17,19^FR^FDSecuencia #^FS"
      yy$(59) = "17^FO1565,3216^CI0^A0N,21,23^FR^FD"
      yy$(60) = "17^FO1566,3239^CI0^A0N,21,23^FR^FD"
      yy$(61) = "^PQ1^XZ"
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
                CH(06),                  /* Old Customer Code  (EWD018)*/~
                CH(02),                  /* W.W Config Line No (PAR002)*/~
                CH(01),                  /* UPS Flag 0 or 1    (PAR002)*/~
                CH(10),                  /* New Par Info Fields(PAR002)*/~
                CH(40),                  /* New Sub Part Descr (PAR002)*/~
                CH(01),                  /* New Special Mull code(PAR006)*/~
		POS(727), CH(09)         /* New Customer Code */                           

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

        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(),           ~
                      dt_sub_part$, dt_sub_info$
            dim1es, dim2es, dim3es = 0.00       /* (AWD048)          */
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"
            err1% = 0%

/*          convert lb_so$ to so_inv%, data goto convert_alpha */
            convert str(lb_bc$,1%,8%) to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
/*          convert str(lb_so$,2%,7%) to so_inv%, data goto sub_part1*/
            convert str(lb_bc$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)
order_converted:
/*          convert lb_itmno$ to item_no%, data goto sub_part2*/
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

/* (AWD048) */          
            get bcksubpt_rec$ using dimFmt, dim1es, dim2es
dimFMT:               FMT POS(153), PD(14,4), PD(14,4)
             
            if err1% = 0% then goto no_subpart_error             
               str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
               str(bcksubpt_rec$,132%,20%) = "00000000000000000000"
               
no_subpart_error:
               dt_sub_part$ = str(bcksubpt_rec$,48%,20%)

               dt_sub_info$ = str(bcksubpt_rec$,132%,20%)

               if err1% = 0% then return

            return
