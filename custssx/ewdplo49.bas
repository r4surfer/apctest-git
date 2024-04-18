        REM *************************************************************~
            *            (RHHTEST)    Test Print Turned (off)            *~
            *                                                           *~
            *      Note- Bay-Bow Test for Line = 25%  in yy$(??)        *~
            *      Note- UPC Test for Line     = 86% (Turned Off)       *~
            *                                        (AWD028)           *~
            *      Note- 100% Inspection test for Line = 27% in yy$(??) *~
            *                                 and Line = 102% (Text)    *~
            *  Subroutine Name   - EWDPLO49                             *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      production label.                    *~
            *                                                           *~
            *                      Print File  = MFLOWA/NELOWA/NELOWAS  *~
            *                      Script File = MFLOWA/NELOWA/NELOWAS  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLO49 - Generates the label format and data to print   *~
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
            *04/24/2021! Original                                 | RDB *~
            *07/09/2021! CR2860 New label layout for Lowes        ! RDB *~
            *10/12/2021! CR2924 Fix call size                     ! RDB *~
            *10/19/2021! CR2802 150 Black laminate                ! RDB *~            
            *************************************************************

        sub "EWDPLO49" (been_here%,      /* Zero (Only 1st Time)       */~
                        rec$(),          /* Prod Label Data    (PAR002)*/~
                        #1,              /* GENCODES           (EWD018)*/~
                        #2,              /* APCPLNDT           (EWD022)*/~
                        #4,              /* BCKLINES           (AWD007)*/~
                        #3,              /* BCKSUBPT           (PAR000)*/~
                        #7,              /* AWDSKUXR           (PAR000)*/~
                        #8,              /* APCPCMST           (EWD022)*/~
                        sc_sku$,         /* Requested SKU              */~
                        upc$,            /* UPC                        */~
                        qty%,            /* Quantity                   */~
                        lb_wd_nrw$,      /* Lowes wide or narrow       */~
                        prtnbr$,         /* Printer number by script   */~
                        error%)          /* Return Code                */

        dim                                                              ~
            schema$8,                    /* (PAR003) Schema Switch     */~
            a$128, b$128,                /* Print Lines for Label      */~
            lbl$(50%)80,                 /* Label Data Array   (EWD001)*/~
            sc_sku$10,                   /* SKU                        */~
            upc$12,                      /* UPC                        */~
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
            lb_wd_nrw$10,                /* Lowes wide or narrow label */~
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
            sc_sel$2,                    /* Screen Sel                 */~
            errormsg$45,                 /* Display error message      */~
            ibeg$5,                      /* Intermec Beg Command       */~
            iend$5                       /* Intermec End Command       */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */
            
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
            series$16, style$10,         /* series & style             */~
            p_part$25, nominal$7         /* EWDDNOMSZ part & nominal   */            

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
            * #5  ! MFGLOWA  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "MFGLOWA", varc, consec, recsize =   128


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

SS_1:
            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "MFGLOWA"
               script$  = "MFGLOWA"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */                                                 
SS_2:
               file$    = "NELOWA"
               script$  = "NELOWA"
               if prtnbr$ = "2" then script$ = "NELOWA2"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
        REM    file$   = "MFGTEST"                /* (EWD010)        */
        REM    script$ = "MFGTEST"
 
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
        begin_process                             
        
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

                                        /* Continous Head Mull Test     */
          gosub lookup_seq
          gosub lookup_sku
          if sku% = 0% then goto bad_sku
          
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

/* p_part$ is a return value with the opening size */
/* CR2924 Fix */
          if str(series$,1%,4%) = "3201" then L12000
          call "EWDNOMSZ" ("E", lb_part$, p_part$, nominal$,   ~ 
                             lb_cust_code$, #8, #1, err%)
          goto L12050
L12000:
          call "EWDNOMSZ" ("E", op_part$, p_part$, nominal$,   ~ 
                             lb_cust_code$, #8, #1, err%)
L12050:                             
          call "SPCSMASH" (nominal$)
          lbl$(33%) = nominal$ & fs$ 
          
REM   01 = sku Field "A" - Top 6 digit number
          lbl$(01%) = sc_sku$ & fs$
REM   02 = upc
          lbl$(02%) = upc$ & fs$
REM   03 = oSize Field "C" - Opening Larger font WxH 
           lbl$(03%) = oWidth$ & " in"
           call "STRING" addr("RJ", lbl$(03%), 10%)
           lbl$(03%) = lbl$(03%) & fs$
           lbl$(35%) = oHeight$ & " in" & fs$  
           
REM   04 = eSize Field "D" - Exact Size
          lbl$(04%) = eWidth$ & " in x " & eHeight$ & " in" & fs$
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
          lbl$(18%) = str(upc$,1,11) & fs$
         
REM   19 = Nominal Size - lb_nomsz$
          lbl$(19%) = lb_nomsz$ & fs$

REM   20 = Series number
          lbl$(20%) = series$ & fs$
          
REM   21 - 33 = UPC individual numbers
      i% = 21%
      for r% = 1% to 12%
           lbl$(i%) = str(upc$,r%,1%) & fs$
           i% = i% + 1%
      next r%
/* processing the type of label then looping through for substitutions */

      convert qty% to qty$, pic(00000)
      lb_wd_nrw$ = lbl_type$
      if lbl_type$ = "NARROW" then goto narrow_label
         
         gosub series_wide
             copy yy$() to xx$()
         lbl% = lbl% + 1%
         goto read_loop
         
narrow_label:
         gosub series_narrow
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

bad_sku:
        errormsg$ = "Not Valid Print SKU"
        gosub error_prompt
        error% = 10%
        goto L70000
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
              init(" ") so_inv$, item_no$  
              sku% = 1%              
              so_inv$  = str(lb_bc$,1%,8%)   
              item_no$ = str(lb_bc$,9%,2%)   
              gosub lookup_sub_part
              series$ = str(bcksubpt_rec$,169%,16%)
              style$  = str(bcksubpt_rec$,185%,10%)

              lbl_type$ = "NARROW"
         /* NOT narrow label */
              if str(series$,1%,8%) = "3201    " or   ~
                 str(series$,1%,8%) = "3100    " or   ~
                 str(series$,1%,8%) = "3050    " or   ~
                 str(series$,1%,8%) = "8050    "  then lbl_type$ = "WIDE" 
              
              if (str(series$,1%,8%) = "3201    " and ~
                  str(style$,1%,8%) = "DH      " ) or   ~
                 (str(series$,1%,8%) = "3050    " and ~
                  str(style$,1%,8%) = "SH      " ) or   ~
                 (str(series$,1%,8%) = "8050    " and ~
                  str(style$,1%,8%) = "SH      " ) or   ~
                 (str(series$,1%,8%) = "3050    " and ~
                  str(style$,1%,8%) = "2SL     ") or    ~
                 (str(series$,1%,8%) = "8050    " and ~
                  str(style$,1%,8%) = "2SL     " ) or   ~           
                 (str(series$,1%,8%) = "3100    " and ~
                  str(style$,1%,8%) = "SH      " ) or   ~
                 (str(series$,1%,8%) = "450     " and ~
                  str(style$,1%,8%) = "DHHP    " ) or   ~
                 (str(series$,1%,8%) = "105     " and ~
                  str(style$,1%,8%) = "SH      " ) or   ~
                 (str(series$,1%,8%) = "105     " and ~
                  str(style$,1%,8%) = "2SL     " ) or   ~
                 (str(series$,1%,8%) = "150     " and ~
                  str(style$,1%,8%) = "SH      " ) or   ~
                 (str(series$,1%,8%) = "85      " and ~
                  str(style$,1%,8%) = "SH      " ) or   ~
                 (str(series$,1%,8%) = "300     " and ~
                  str(style$,1%,8%) = "SH      " ) or   ~
                 (str(series$,1%,8%) = "150     " and ~
                  str(style$,1%,8%) = "2SL     " ) or   ~
                 (str(series$,1%,8%) = "130     " and ~
                  str(style$,1%,8%) = "2SL     " )     then goto good_sel

              goto bad_sel
/*
 cmg === This is the end of my selection logic.  But you mentioned to me that
         if series is = "" then use current logic so should I leave this logic below?
 pww -> i thought you were asking about the NFRC labels not the lowes stock labels
    different project, i was confused.
*/
          
                 if str(sku_key$,1%,4%) <> "X_LO" then goto bad_sel
REM sc_sel$ = 12 then with grid Series 85
                 if str(part$,7,2) = "00" and sc_sel$ = "12" then goto bad_sel
                 if str(part$,7,2) <> "00" and sc_sel$ = "11" then goto bad_sel
                   if model$  = "3W1" then goto good_sel
                   if model$  = "3H1" then goto good_sel
                     goto bad_sel
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
REM   02 = upc
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
series_narrow 
      init(" ") yy$()                       
REM move left 1/8" (25 points)

      yy$(001) = "^JO^XA^SZ2^JMA^MCY"
      yy$(002) = "^PMN^PW1228~JSN^JZY"
      yy$(003) = "^LH0,0^LRN^XZ"
      yy$(004) = "^XA"
      yy$(005) = "^FT184,2470^CI0^A0N,70,40^FDSeries ^FS"
      yy$(006) = "^FT806,2470,^A0N,70,40^FDSeries ^FS"
      yy$(007) = "20^FT324,2470^A0N,70,54^FR^FD"              /* Series Fields */
      yy$(008) = "20^FT933,2470^A0N,70,54^FR^FD" 
                 
      yy$(009) = "^FT50,2554^A0N,34,36^FDCall Size:^FS"
      yy$(010) = "^FT659,2554^A0N,34,36^FDCall Size:^FS"
          
      yy$(011) = "33^FT215,2572^A0N,85,115^FR^FD"          /* Call Size Fields */
      yy$(012) = "33^FT824,2572^A0N,85,115^FR^FD"
          
      yy$(013) = "^FT50,2636^A0N,56,31^FDR/O:       Width              Height^FS"
      yy$(014) = "^FT659,2636^A0N,56,31^FDR/O:      Width              Height^FS"
          
      yy$(015) = "03^FT84,2719^A0N,79,49^FR^FD"            /* Open Size Fields */
      yy$(016) = "^FT291,2719^A0N,79,45^FD X ^FS"
      yy$(017) = "35^FT367,2719^A0N,79,49^FR^FD"
      yy$(018) = "03^FT684,2719^A0N,79,49^FR^FD"
      yy$(019) = "^FT886,2719^A0N,79,45^FD X ^FS"
      yy$(020) = "35^FT963,2719^A0N,79,49^FR^FD"      
          
      yy$(021) = "^FT50,2792^A0N,34,46^FDFrame: ^FS"
      yy$(022) = "^FT654,2792^A0N,34,46^FDFrame: ^FS"
          
      yy$(023) = "04^FT202,2792^A0N,31,40^FR^FD"          /* Exact Size Fields */
      yy$(024) = "04^FT790,2792^A0N,31,40^FR^FD"
          
      yy$(025) = "^FT273,2872^A0N,51,69^FDSKU^FS"
      yy$(026) = "^FT869,2872^A0N,51,65^FDSKU^FS"
          
      yy$(027) = "01^FT197,2940^A0N,70,95^FR^FD"                  /* SKU Field */
      yy$(028) = "01^FT793,2940^A0N,70,95^FR^FD"
          
      yy$(029) = "02^FO146,2956^BY4^BUN,127,N,N,N^FR^FD"      /* Full UPC code */
      yy$(030) = "22^FT186,3118^A0N,42,57^FR^FD"                 /* second nbr */
      yy$(031) = "23^FT215,3118^A0N,42,57^FR^FD"                  /* third nbr */
      yy$(032) = "24^FT245,3118^A0N,42,57^FR^FD"                    /* 4th nbr */
      yy$(033) = "25^FT274,3118^A0N,42,57^FR^FD"                    /* 5th nbr */
      yy$(034) = "26^FT302,3118^A0N,42,57^FR^FD"                    /* 6th nbr */
      yy$(035) = "27^FT342,3118^A0N,42,57^FR^FD"                    /* 7th nbr */
      yy$(036) = "28^FT370,3118^A0N,42,57^FR^FD"                    /* 8th nbr */
      yy$(037) = "29^FT399,3118^A0N,42,57^FR^FD"                    /* 9th nbr */
      yy$(038) = "30^FT428,3118^A0N,42,57^FR^FD"                   /* 10th nbr */
      yy$(039) = "31^FT457,3118^A0N,42,57^FR^FD"                   /* 11th nbr */
      yy$(040) = "21^FT111,3117^A0N,31,42^FR^FD"                  /* first nbr */
      yy$(041) = "32^FT541,3117^A0N,31,42^FR^FD"                   /* last nbr */
      yy$(042) = "02^FO768,2956^BUN,127,N,N,N^FR^FD"          /* Full UPC Code */
      yy$(043) = "22^FT808,3118^A0N,42,57^FR^FD"
      yy$(044) = "23^FT837,3118^A0N,42,57^FR^FD"
      yy$(045) = "24^FT866,3118^A0N,42,57^FR^FD"
      yy$(046) = "25^FT895,3118^A0N,42,57^FR^FD"
      yy$(047) = "26^FT924,3118^A0N,42,57^FR^FD"
      yy$(048) = "27^FT964,3118^A0N,42,57^FR^FD"
      yy$(049) = "28^FT992,3118^A0N,42,57^FR^FD"
      yy$(050) = "29^FT1021,3118^A0N,42,57^FR^FD"
      yy$(051) = "30^FT1050,3118^A0N,42,57^FR^FD"
      yy$(052) = "31^FT1079,3118^A0N,42,57^FR^FD"
      yy$(053) = "21^FT732,3117^A0N,31,42^FR^FD"
      yy$(054) = "32^FT1163,3117^A0N,31,42^FR^FD"
      yy$(055) = "^PQ" & qty$ & ",0,1,Y"
      yy$(056) = "^XZ"
        return

        
/* Wide label   */
series_wide
      init(" ") yy$()                          
      yy$(001) = "^JO^XA^SZ2^JMA^MCY"
      yy$(002) = "^PMN^PW1716~JSN^JZY"
      yy$(003) = "^LH0,0^LRN^XZ"
      yy$(004) = "^XA"
      yy$(005) = "^FT299,2483^CI0^A0N,70,40^FDSeries ^FS"
      yy$(006) = "^FT1123,2483^A0N,70,40^FDSeries ^FS"
      yy$(007) = "20^FT438,2483^A0N,70,54^FR^FD"
      yy$(008) = "20^FT1276,2483^A0N,70,54^FR^FD"
      yy$(009) = "^FT172,2554^A0N,34,36^FDCall Size:^FS"
      yy$(010) = "^FT1009,2554^A0N,34,34^FDCall Size:^FS"
      yy$(011) = "33^FT337,2572^A0N,85,115^FR^FD"
      yy$(012) = "33^FT1161,2572^A0N,85,115^FR^FD"
      yy$(013) = "^FT172,2637^A0N,56,31^FDR/O:       Width               Height^FS"
      yy$(014) = "^FT1009,2636^A0N,56,31^FDR/O:       Width               Height^FS"
      
      yy$(015) = "03^FT210,2719^A0N,79,49^FR^FD"
      yy$(016) = "^FT413,2719^A0N,79,45^FD X ^FS"
      yy$(017) = "35^FT489,2719^A0N,79,49^FR^FD"     
      yy$(018) = "03^FT1047,2719^A0N,79,49^FR^FD"
      yy$(019) = "^FT1250,2719^A0N,79,45^FD X ^FS"
      yy$(020) = "35^FT1326,2719^A0N,79,49^FR^FD"

      yy$(021) = "^FT172,2795^A0N,34,46^FDFrame: ^FS"
      yy$(022) = "^FT1009,2795^A0N,34,46^FDFrame: ^FS"
      yy$(023) = "04^FT311,2795^A0N,34,46^FR^FD"
      yy$(024) = "04^FT1149,2795^A0N,34,46^FR^FD"
      yy$(025) = "^FT400,2872^A0N,51,69^FDSKU^FS"
      yy$(026) = "^FT1225,2872^A0N,51,65^FDSKU^FS"
      yy$(027) = "01^FT324,2940^A0N,70,95^FR^FD"
      yy$(028) = "01^FT1148,2940^A0N,70,95^FR^FD"
      yy$(029) = "02^FO261,2956^BY4^BUN,127,N,N,N^FR^FD"
      yy$(030) = "22^FT301,3118^A0N,42,57^FR^FD"
      yy$(031) = "23^FT330,3118^A0N,42,57^FR^FD"
      yy$(032) = "24^FT359,3118^A0N,42,57^FR^FD"
      yy$(033) = "25^FT388,3118^A0N,42,57^FR^FD"
      yy$(034) = "26^FT417,3118^A0N,42,57^FR^FD"
      yy$(035) = "27^FT457,3118^A0N,42,57^FR^FD"
      yy$(036) = "28^FT485,3118^A0N,42,57^FR^FD"
      yy$(037) = "29^FT514,3118^A0N,42,57^FR^FD"
      yy$(038) = "30^FT543,3118^A0N,42,57^FR^FD"
      yy$(039) = "31^FT572,3118^A0N,42,57^FR^FD"
      yy$(040) = "21^FT225,3117^A0N,31,42^FR^FD"
      yy$(041) = "32^FT656,3117^A0N,31,42^FR^FD"
      yy$(042) = "02^FO1111,2956^BUN,127,N,N,N^FR^FD"
      yy$(043) = "22^FT1151,3118^A0N,42,57^FR^FD"
      yy$(044) = "23^FT1180,3118^A0N,42,57^FR^FD"
      yy$(045) = "24^FT1209,3118^A0N,42,57^FR^FD"
      yy$(046) = "25^FT1238,3118^A0N,42,57^FR^FD"
      yy$(047) = "26^FT1267,3118^A0N,42,57^FR^FD"
      yy$(048) = "27^FT1307,3118^A0N,42,57^FR^FD"
      yy$(049) = "28^FT1335,3118^A0N,42,57^FR^FD"
      yy$(050) = "29^FT1364,3118^A0N,42,57^FR^FD"
      yy$(051) = "30^FT1393,3118^A0N,42,57^FR^FD"
      yy$(052) = "31^FT1422,3118^A0N,42,57^FR^FD"
      yy$(053) = "21^FT1075,3117^A0N,31,42^FR^FD"
      yy$(054) = "32^FT1506,3117^A0N,31,42^FR^FD"
      yy$(055) = "^PQ" & qty$ & ",0,1,Y"
      yy$(056) = "^XZ"
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

L70000:     call "FILEBGON" (#5) 
            
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
