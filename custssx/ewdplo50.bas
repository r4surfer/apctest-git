        REM *************************************************************~
            *  Subroutine Name   - EWDPLO50                             *~
            *  Creation Date     - 06/27/2022                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *  Last Mod's By     -                                      *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      production label.                    *~
            *                                                           *~
            *                      Print File  = MFGLOWS/NELOWS         *~
            *                      Script File = MFGLOW2/NELOWS         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLO48 - Generates the label format and data to print   *~
            *            production labels. The resulting file is routed*~
            *            to the label printer via a script.             *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Invalid style/series/color       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                     9% - Schema Lookup Error              *~
			*                    10% - SKU invalid                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *12/02/2022! CR3199 new print sleeve window.          ! RDB *~
            *************************************************************

        sub "EWDPLO50" (been_here%,      /* Zero (Only 1st Time)       */~
                        rec$(),          /* Prod Label Data            */~
                        #1,              /* GENCODES                   */~
                        #2,              /* APCPLNDT                   */~
                        #4,              /* BCKLINES                   */~
                        #3,              /* BCKSUBPT                   */~
                        #7,              /* AWDSKUXR                   */~
                        #8,              /* APCPCMST                   */~
						#10,             /* NFRCDATA                   */~
						#11,             /* NFRCMDL                    */~
						#12,             /* NFRCGLS                    */~
                        sc_info$,         /* Label Info                 */~
                        sc_sel_p$,       /* Printer Number             */~
                        lbl%,            /* Label Type                 */~
						scnr$,           /* Handheld scanner user flag */~
						sc_nbr_lbl$,     /* Number of labels to print  */~
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
			scnr$1,                      /* Handheld scanner user flag */~
			cdesc$25,                    /* Description in Gencode Read*/~
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
            yy$(200%)128,                /* Buffer                     */~
            xx$(200%)128,                /* Buffer                     */~
            l_model$12,                  /* UPC                        */~
            l_desc$70,                   /* label desc                 */~
            sc_info$70,                  /* Label Info                 */~
            sc_sel_p$1,                  /* Printer Number             */~
            color$1,                     /* Color Code CR2802          */~
			grid$10,                     /* Grid or No Grid            */~
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
            series$16, style$10,         /* series & style             */~
			prt_sku$9,                   /* Print Sku Center           */~
            p_part$25, nominal$7         /* EWDDNOMSZ part & nominal   */
			
	    dim                                                              ~
		    ufactor$10,                      /* UFactor                 */~
            sheat$10,                        /* Solar Heat              */~
            vtranmit$10,                     /* Visible Transmittance   */~
            cpdnumber$30,                    /* Cpd Number              */~
            warehouse$4,                     /* Warehouse               */~
			sc_nbr_lbl$1,                    /* Number of labels        */~
			mapnbr$2                         /* Energy Star Map Number  */			

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate Lowe's Stock Labels        "
            pname$ = "EWDPLO50 - Rev: R1.00"

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
            * #5  ! MFGLOWS  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "MFGLOWS", varc, consec, recsize =   128

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
               file$    = "MFGLOWS"
               script$  = "MFGLOWS"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_Cont
                                                     /* North East      */
SS_2:
               file$    = "NELOWS"
               script$  = "NELOWS" 

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_Cont:
        return                                   

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
REM            if lb_specialmull$ = "C" then                                 ~
                                     str(lb_part$,lb_part%-2%,3%) = "   "


                                        /* Continous Head Mull Test     */
          gosub lookup_seq
          gosub lookup_sku
          if sku% = 0% then error% = 10%
          if sku% = 0% then goto end_process
          
/* CR3017 */          
          if sku% <> 0% then gosub confirm_size
          if sizewrong% = 1% then goto end_process
          
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
          f% = pos("346MO" = str(lb_part$,12%,1%))
          if str(series$,1%,4%) = "3201" and f% = 0% then L12000
		  
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
          prt_sku$ = dt_sku$
          lsku% = len(dt_sku$) 
/* center SKU on label */		  
		  if lsku% = 5 then prt_sku$ = " " & prt_sku$
		  if lsku% = 4 then prt_sku$ = "  " & prt_sku$		  
          lbl$(01%) = prt_sku$ & fs$
REM   02 = l_model
          lbl$(02%) = l_model$ & fs$
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

/* wide_label 4 inches */
         gosub series_wide
             copy yy$() to xx$()
         lbl% = lbl% + 1%

    read_loop
        init(" ") a$
		maxmap% = 131%      /* array area for maps */
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " and nbr_lines% > maxmap% then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
		if nbr_lines% < maxmap% then goto skip_data  /* Just copy not line edits */
		
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
	    nbr_lines% = 0%
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
       
	    REM ***********************************************************~
		    * Color description lookup                                *~
			***********************************************************
	    lookup_color
           init(" ") readkey$, cdesc$
           str(readkey$,1%,9%)   = "COLOR"
           str(readkey$,10%,15%) = color$
           read #1,key = readkey$, using L17055, cdesc$, eod goto L17075
L17055:       FMT POS(25), CH(30)

L17075: return
		
        lookup_sku
            init(" ") dt_key0$
            sku% = 0%
            if dt_sku$ > "        " then goto skip_sku_read
            str(dt_key0$,1%,18%) = lb_bc$
            str(dt_key0$,19%,3%) = "000"
            str(dt_key0$,22%,2%) = "00"
            read #2,key >= dt_key0$, using LSEQ2, tmp_bar$,dt_seq$, dt_sku$,  ~
                                                   eod goto AWDSKUXR
LSEQ2:      FMT POS(24), CH(18), POS(111), CH(05), POS(245), CH(09)
            if tmp_bar$ <> lb_bc$ then goto AWDSKUXR
skip_sku_read:
          sku_key$ = "X_LO" & dt_sku$ & "   "

          read #7, key = sku_key$, using AWDSKUXR, model$, l_model$, part$,~
                                           l_desc$,   eod goto AWDSKUXR
              sku% = 1%

              init(" ") so_inv$, item_no$     
              so_inv$  = str(lb_bc$,1%,8%)   
              item_no$ = str(lb_bc$,9%,2%)    
              gosub lookup_sub_part
			  gosub NFRC_data
              series$ = str(bcksubpt_rec$,169%,16%)
			  if str(series$,1%,5%) = "3201N" then series$ = "3201    "
              style$  = str(bcksubpt_rec$,185%,10%)
              if str(part$,7,2) = "00" then grid$ = "NO GRID   "  ~
			                           else grid$ = "GRID      "
              color$  = str(lb_part$,4%,1%)
			  gosub lookup_color
REM              if color$ = "4" and str(series$,1%,8%) <> "150     " then ~
                goto badr_sel
              
                                                  /*Series 3201 DH*/
              if (str(series$,1%,8%) = "3201    " or                         ~
			      str(series$,1%,8%) = "3201N   ") and                       ~
                str(style$,1%,8%) = "DH      "  then goto good_sel
             
                                                  /*Series 3050 SH*/               
              if (str(series$,1%,8%) = "3050    " or                         ~
                  str(series$,1%,8%) = "8050    ") and                       ~
                  str(style$,1%,8%) = "SH      "  then goto good_sel  
          
                                                  /*Series 3050 Slide*/
              if (str(series$,1%,8%) = "3050    "  or                        ~
                  str(series$,1%,8%) = "8050    ") and                       ~
                  str(style$,1%,8%) = "2SL     "  then goto good_sel 
           
                                                  /*Series 3100 SH*/
              if str(series$,1%,8%) = "3100    " and                         ~
                 str(style$,1%,8%) = "SH      " then goto good_sel 
          
                                                  /*Series 450 DHHP*/
              if str(series$,1%,8%) = "450     " and                         ~
                 str(style$,1%,8%) = "DHHP    " then goto good_sel
          
          
                                                  /*Series 105 SH*/
              if str(series$,1%,8%) = "105     " and                         ~
                 str(style$,1%,8%) = "SH      " then goto good_sel

          
                                                  /*Series 105 Slider*/
              if str(series$,1%,8%) = "105     " and                         ~
                 str(style$,1%,8%) = "2SL     " then goto good_sel

         
                                                  /*Series 150 SH*/
              if str(series$,1%,8%) = "150     " and                         ~
                 str(style$,1%,8%) = "SH      "  and                         ~
                 color$ = "2"                   then goto good_sel

                                                  /*Series 151 Slider*/
              if str(series$,1%,8%) = "150     " and                         ~
                 color$ = "2"                    and                         ~              
                 str(style$,1%,8%) = "2SL     " then goto good_sel
              
                                                  /*Series 130 Slider*/
              if str(series$,1%,8%) = "130     " and                         ~
                 str(style$,1%,8%) = "2SL     " then goto good_sel

                                                  /*Series 105 Black SH*/
              if str(series$,1%,8%) = "150     " and                         ~
                 str(style$,1%,8%) = "SH      "  and                         ~
                 color$ = "4"                  then goto good_sel

bad_sel:      sku% = 0%
              error% = 6%
good_sel:
              str(sc_info$, 1%, 9%) = dt_sku$
			  str(sc_info$,10%, 8%) = series$
			  str(sc_info$,18%, 8%) = style$
			  str(sc_info$,26%, 3%) = model$
			  str(sc_info$,29%,10%) = grid$
			  str(sc_info$,39%,20%) = cdesc$
			  
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
 
REM     *********************************************************************~
        * Confirm size from sales order to size in sku file    CR3017       *~
        *********************************************************************
        
        confirm_size
        
          sizewrong% = 1%            /* default size flag as wrong */
    /* if size width and height match order to sku, flag size correct */
          if str(lb_part$,13%,4%) = str(part$,13%,4%) and ~
             str(lb_part$,17%,3%) = str(part$,17%,3%)  then sizewrong% = 0%
             
          if sizewrong% = 0% then return
		  error% = 11%
		  if scnr$ = "Y" then return

L64000:    k% = 2%
           hdr$     = "***** Size Error Printing *****"
           msg$(1%) = "This barcode could not print due to size match issue."
          msg$(2%) = "SO " & str(lb_bc$,1%,8%) & " Line " & str(lb_bc$,9%,2%) ~
                      & " SKU " & dt_sku$           
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"

           call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if k% <>  0% then goto L64000
             
        return
        
/* New argon check */        
REM        check_argon                      /* Glass Code for Argon */
REM          init(" ") readkey$, gs$
REM          str(readkey$,1%,9%)   = "PLANARGON"
REM          str(readkey$,10%,15%) = str(lb_part$,5%,2%)
REM          read #3,key = readkey$, eod goto noArgon
REM             gs$ = "G1"
REM        noArgon
REM        return

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

series_wide
      init(" ") yy$()                            
      yy$(001) = "^JO^XA^SZ2"
      yy$(002) = "^JMA^MCY"
      yy$(003) = "^PMN^PW1158~JSN^JZY"
      yy$(004) = "^LH0,0^LRN^XZ"
	  
	  /* test code mapnbr$ = "1" */
	  
/* Energy Star Image */	  
      if mapnbr$ <> "0" then goto map01

	  yy$(005) = " "
	  yy$(006) = " "
	  yy$(007) = " "
	  yy$(008) = " "
	  yy$(009) = " "
	  yy$(010) = " "
	  yy$(011) = " "
	  yy$(012) = " "
	  yy$(013) = " "
	  yy$(014) = " "
	  yy$(015) = " "
	  yy$(016) = " "
	  yy$(017) = " "
	  yy$(018) = " "
	  yy$(019) = " "
	  yy$(020) = " "
	  yy$(021) = " "
	  yy$(022) = " "
	  yy$(023) = " "
	  yy$(024) = " "
	  yy$(025) = " "
	  yy$(026) = " "
	  yy$(027) = " "
	  yy$(028) = " "
	  yy$(029) = " "
	  yy$(030) = " "
	  yy$(031) = " "
	  yy$(032) = " "
	  yy$(033) = " "
      yy$(034) = " "  
      yy$(035) = " "  
      yy$(036) = " "  
      yy$(037) = " "  
      yy$(038) = " "  
      yy$(039) = " "  
      yy$(040) = " "  
      yy$(041) = " "  
      yy$(042) = " "  
      yy$(043) = " "  
      yy$(044) = " "  
      yy$(045) = " "  
      yy$(046) = " "  
      yy$(047) = " "  
      yy$(048) = " "  
      yy$(049) = " "  
      yy$(050) = " "  
      yy$(051) = " "  
      yy$(052) = " "  
      yy$(053) = " "  
      yy$(054) = " "  
      yy$(055) = " "  
      yy$(056) = " "  
      yy$(057) = " "  
      yy$(058) = " "  
      yy$(059) = " "  
      yy$(060) = " "  
      yy$(061) = " "  
      yy$(062) = " "  
      yy$(063) = " "  
      yy$(064) = " "  
      yy$(065) = " "  
      yy$(066) = " "  
      yy$(067) = " "  
      yy$(068) = " "  
      yy$(069) = " "  
      yy$(070) = " "  
      yy$(071) = " "  
      yy$(072) = " "  
      yy$(073) = " "  
      yy$(074) = " "  
      yy$(075) = " "  
      yy$(076) = " "  
      yy$(077) = " "  
      yy$(078) = " "  
      yy$(079) = " "  
      yy$(080) = " "  
      yy$(081) = " "  
      yy$(082) = " "  
      yy$(083) = " "  
      yy$(084) = " "  
      yy$(085) = " "  
      yy$(086) = " "  
      yy$(087) = " "  
      yy$(088) = " "  
      yy$(089) = " "  
      yy$(090) = " "  
      yy$(091) = " "  
      yy$(092) = " "  
      yy$(093) = " "  
      yy$(094) = " "  
      yy$(095) = " "  
      yy$(096) = " "  
      yy$(097) = " "  
      yy$(098) = " "  
      yy$(099) = " "  
      yy$(100) = " "  
      yy$(101) = " "  
      yy$(102) = " "  
      yy$(103) = " "  
      yy$(104) = " "  
      yy$(105) = " "  
      yy$(106) = " "  
      yy$(107) = " "  
      yy$(108) = " "  
      yy$(109) = " "  
      yy$(110) = " "  
      yy$(111) = " "  
      yy$(112) = " "  
      yy$(113) = " "  
      yy$(114) = " "  
      yy$(115) = " "  
	  yy$(116) = " "
	  yy$(117) = " "
	  yy$(118) = " "
	  yy$(119) = " "
	  yy$(120) = " "
	  yy$(121) = " "
	  yy$(122) = " "
	  yy$(123) = " "
	  yy$(124) = " "
	  yy$(125) = " "
	  yy$(126) = " "
	  yy$(127) = " "
	  yy$(128) = " "
	  yy$(129) = " "
	  yy$(130) = " "
	  yy$(131) = " "

	  goto label_info
map01:
      if mapnbr$ <> "1" then goto map02
yy$(005) = " ~DGR:SSGFX000.GRF,33178,53,:Z64:eJztnU1sG2d6x9/hMCQD0BLdGNsxdiKN7AU2BXKg4gJhasU"
yy$(006) = "U7aDbQ4FisZeculKEXV+6CIWgsAxL1ZgSSB8UMgWKwgUMu+2l1wTtwcUaNm0aYtFurUMvPRgSGS6iPQ"
yy$(007) = "QrqlpE2krl9P2ceb9myFHaLQqEmBCMzD/48Kf/+zzP+0HK8+Lfmo+P9/wrm2+Ncp0RRGC0m9HkX3Zk0"
yy$(008) = "eHOin/974pOFd6pQGhEzmnCGy5SQRgvTyOqnyK8EUQqiOyl04im3fjhZaaGilQQ87V8fFG3WogdnlVK"
yy$(009) = "loeJFBCt7HQnrsiAoi8ASMULL5FxagBEvS0VRMqar9oALLtxRZYLspEiObxUrpR8rQkykeFpQORbLki"
yy$(010) = "fRvS9SJFCr5Jx3JlMK1KkBbFQ/ys3ROF5bohofvN8M0Q0X9UOjVIyd+2TyTCRofXeAgQxVnjmhIgSwN"
yy$(011) = "WI0na+Ba/fjRJpRq6Tdp3Gt8JEhgYEvE0UIIsbYe9pR181xpBowQ1RZfVpeSxXSmV+ECYKKQBGO5tvZ"
yy$(012) = "2Zjinaz0+3sXIRIV9TWMhdSWSdEUg4pn9butv3jT0Je5t/CRAv1UHpmWHi5EjhX+gO9KJHT2Qjg1Ney"
yy$(013) = "j+KL2vbSpl51Vus9lPre2c3c3NWL3tF7DxTm69Z8/XwHPkw5ksa4H0Kv0IODt24h0ZhSdpJh9LrJXGk"
yy$(014) = "994dJAOycGqAexLiN+zr7wAW33FFFadIMWotNcFNh6ITQg4O3knD+OZf/YvwXctY0j0Po4dRXsxf/cr"
yy$(015) = "X94idNUVSqh3kP50ur2yj8fX1ZEoEw78Gym5tN5XoN95K9qhPpQJhZnMVmPlorFDS+iBaVd++VZdFYP"
yy$(016) = "oIeRPvk13t/PfeFJCpG0YPXQeM89JPUVJjJEHoZIjreQibsS6IwellIbz6Zaz/4ztP13HsjgqA2yl5q"
yy$(017) = "b3zaGnNGp4euj/D9nCgJpecCSK+S+c9KBpVgQdPSlU98IyC69zFGVxDNhs41zDp+el0jAqFzDfN5bgo"
yy$(018) = "OXjdXeg5z4GggQMpFNhqDFMrwYj+9hSylr7n0/ZIRz4uS5TaMNDkVRg8YFcupjEGAcxW/UGXBbSgaux"
yy$(019) = "lGDxgIwc0qMSF9qcUqSmrpsJEL6OCtknvaZXaxKNR7ABWOZO5SEt1fS1rkxZ8mc80oeqgG5FsTfQwiT"
yy$(020) = "7IYLMStjeEiu0xEbfxMVL03EPLw8KDlKnYe3VvOmotEn2eceoq+UgSIP71PhzB6K2n0ADapxjoTLe0/"
yy$(021) = "0Ihu1KkIQgNj8EEBojNfsPCOOpMaej9Io/vvJ3Nl+pM84p1iIDrNyzp60y2ay+APEJEO4On1qjqRvU9"
yy$(022) = "FNjIefNxkIhxer1GU6SWh94xmBgO8AmcEkCeb9lEQi20FxHVsIAJioYnHMnQdlFWYqFyblES47FYLXQ"
yy$(023) = "xw4YXVIyL4MmssvL4l0wPXkPHOwuK7mMzlG94BHMJ4pm2eZSCObBkEIFlshlzTe4/ydDRyHcvMLVk0k"
yy$(024) = "WfooOXyXALMhY5c+EpOBV6WBemtZZxv34GjGPivFLZcYlS5LFZ9HY7AHkD0zAcRaywmTJaBCD0ekDnD"
yy$(025) = "NqN38ooSXiK9CtFdzcLygUpwKVnwaw6jl1VAgJvtCbok1UP3021XEnWyCnLk0E/51axK8HZJeJ267D2"
yy$(026) = "A/NKsJMYTwHmGAHIzS+a9Xdl7AKCpl7XQsOZrEMT5+Q36Y2csSpRCout16llfVB5n4e23Fe8BSCybuw"
yy$(027) = "npoZlvUDvmDH8Q1lUQ8M3/Hh62eamIMu91Mqoo53sPi2Z5EUksYFVDL3OxmQAXrDlsP8CLMIjlkrpUh"
yy$(028) = "+au77QgiE1Eb0F4T8R1Pb3I6r6wFpDI+oD9+KNG1Poe8tvM7HvJ760jev5b+u1tn95jFQQgrkNNCwLh"
yy$(029) = "/9iJEuHlgmyBeY//F5r3BhrvJTLQdRkXjl9ITxRhELl5OYXlOggEmnR8RFKfKgJAyXt9XCb8yttzlfC"
yy$(030) = "gSK4aOWQ5p0SMh7oXXkRBSOUTUgBszJKrOVwEFXNYNM1Ec0p4h8ovN5EpVBA9J5FCGS+dEUT6FJYIkh"
yy$(031) = "cGMcM3cEx0IIpga8SLFqBt2Xsy/JnaSUmkd3Y9D4dt6ZUcpAfLxw/GktfYP12s+3kPiDay37qIu+UWA"
yy$(032) = "XEv357w/ynvj1xJRKFNExHqVWxBhMPrinkvh3NdBSSeZaYSCQB7lXXO5a5fcwUbMQSNJXSP/HTOJz5V"
yy$(033) = "DSnUuF5Av+GkBwsuvPdFwQqVlPfysLUrpY9gi1LCE95S8py/HDmV9AvAGA/C4PI+tVHb8hmF0EOtHX2"
yy$(034) = "uKgroncwJ9OoZNFrhVc1MkQeVYDbu01vdF2xk06RfPd9lg9Dv6AN68vSuWw1KLRFp6HneAy68V3KdJB"
yy$(035) = "2w85Teb72v0hNTmNm0Gb032SD0kQfek/Je60xZQt4KvLfI6PUeC/TMOpmpOZUCo8cv5mpTWOr8NQbiO"
yy$(036) = "nuwNFSEiPXY08n1c0GEw/ukJBS1nJE8O5uirTKmd/a5ICIgPGGpbhNMbYIEfv/T2gKgyXtGGzergsgS"
yy$(037) = "RLqitobvYdVIWA6Z6lYuqCCk8onXpgAtAOT6sSLafr0rrlARUa8eJkJhDcZd4Zd71kX/ZPW2/gI5cBN"
yy$(038) = "h/FwBcTQutm54ZQAOxanWIwhiv20vtsdU0d6EKLL7QccCJ2v2YksU4YZlMCGE9yw772bzAFvuA+y6BL"
yy$(039) = "9YSkC8XL4hprDr1QIZe/MvKIsfcc0RFS3xIiNlHdQLdPAygAvc6iAJ76Swyue9VG7fJpaDLV+ePnjKi"
yy$(040) = "zAIcMKDaIobuGUyAWvKIvMhL5rTiW7yImw6s88PDcfMMMtlLn+OR+7tzBNXArGzIuS9bW741ZdoAtyS"
yy$(041) = "kYs1d26MF1F63bb8yxWHxhye5FJo6zNN8sDkw1Mn/FYOFwDc5o3Bfo+ks7dlepIItHl6MzQHKvSE8LK"
yy$(042) = "g6tPLOmsTTUTvjtNRQAgFIBWA6FYLu5vkQTNalAhE+1Vqwm61zIl0icVi9CYNWHZtRO9pkhdhBN6AB0"
yy$(043) = "Fq7k/XKIvL5EFHFu1NciLbEOoFe6DQcwEXng2w38ofkH4v43wOlsAdRwaxLRSACwD7bRMaaAmnvpndF"
yy$(044) = "1a3BmSRNceJLqaI34L73Tqkx4tw3it63Mg9yzrkp+zBLCjfTvEiBGKwOuAGoW0S121xZtrbyMuikz2+"
yy$(045) = "5hJ6Nuzx/MqLWj45vJOiONdAW7p3nIpdpgU367iZ8yqIDgdiqkLf//UAB1oriURuuHyR0Yrob5X75Rp"
yy$(046) = "ZCu37rG9B1xgvwjbaneBtxKxj84lMoScadgSROjQyU27mgolX9nx6wgoz6/f4mostZ8kgqrJoe8Z4wI"
yy$(047) = "vQwlRXEq3L4Q1m+Jka7PGeZhV6SRnE4E+4FSpjAyLoD6d38uWVQGTSddFh9E6WvcB7pGSsifRcld728"
yy$(048) = "qEAwlLpdbOKqPDRpCyS6B1s8CJMDxQ4eiUBGlmemvmjn/MiTA/McPT8Nx+AcHrZgiw6atu3BHqSCHYs"
yy$(049) = "Q7xn0iLL0MHH9/IJjfeEAqDS6+q8pxP59BZo9xIZHqF3lvktd7D1Lq65MgihfPIg0A9ZGXCjRGmBHp3"
yy$(050) = "nwgfNqPA6Ab13mjgH4hI8xYsUEHSOhu4fcBhrc1GigkjPv/KcSAkvF9CbzeE1Fro6Ktuolb2soWf1OR"
yy$(051) = "D557IIAJ0Ijd9AtCmFB0XByE0weokJtLIH//cOMaEEYvsmt9Dp0zsvsRiOHDU486GiEHo5I+2U3iP0b"
yy$(052) = "MLTDUR67y3C2rmWFUC0JJGEfLqNRS1JFEUPgJu9DXC70cQbag7znrMm08uVghRmLbwoiNzookFHEhV6"
yy$(053) = "HPJePVgc4K+OGJ5Az0JVA22l5Rg94r1cORBRdD/V2IiB2JLm/Ez0SOc9iV5WDq8peA8N2yQkNkvo0SW"
yy$(054) = "XsUBE1pZnJwUbdUm7simAWGjizTVftGTsyKIFVVQvNPnwgKl4T6VXGr83x9sIXNLlvSxt3VjjN90SvJ"
yy$(055) = "f+NEoUTD0E7xV7Hk+P1QtKz2E/Eei9nOwpIOYDEAVpHLJ+ryfmPdGwgcgdQq8U0Mv7PxFs1AEKCDLVz"
yy$(056) = "74lTF9F7wGl5toBPVWk8x5rV2YJtKv+cp/gvZW+4j0OxPtaevB6EEFPTn3heY+uTUF66aB7EUGoNTcA"
yy$(057) = "cckH0RxdNK2KlPA4evgkJDD8JT5XAqGUz8VgqstAsJlDiKgbiAq9uipSwssL9KDxNuStDQbC04L4VLQ"
yy$(058) = "RkET3A+9dkztEvwQDMbwBVzUuEHo5iMslI5fSswIRSWHcpEusmtxq3Zwsmn8wXNSRwusfKt6b8umJxg"
yy$(059) = "tA8KujYSCaiuhxPBGO60D0XoWcocLnMT6g9Lij4AxET+5YmOsCEEARlSY1IrQ66ovUeW6fm6lx9MZmu"
yy$(060) = "kmH0hNmNQQEN1OjGf9Nsq+zFD7P5UcuFdmkWJS1ImVoXCUtCiD34JlUMgIQnqe0brKNXFm0zSMP8Z4g"
yy$(061) = "QmGdlDl6M/vgHN7ROIehsWErhIfLJ7/B07J/7dPjbDSniHYF5NO7tl9qI3YkeXowvyUmllxCL5PZvUv"
yy$(062) = "dqNCTG9+F+jJNd3Wrt53T0lNFVeswOISGz7Qo9JSqAdsVY3yG0QM3fqbznmQjsVHJ4+3tS6OJOORvSN"
yy$(063) = "M7DT2HbAmtI2gWNt4b8L45AghmI/9+9jSiofTwlQlal6xmjUUP4l8E7ynzXH6uYYSJ5PCEuYa/JIUna"
yy$(064) = "PgMVSan8d7LnNQty3OG+9JiE51r+KKETlQPzrEE9PaD1VFDWl3B1znhDKS6uchv5vJX+TSiOSW8zn2h"
yy$(065) = "3yP0KgG9fEXciCMFYFYEoV4djQj4eS+lF7mCCId3UvK9l+CXp4ILCCLZewkJGnmwGS0CYsGloueiSDt"
yy$(066) = "y8cre33KLVK4oIjO16zsqCL4S5DUirs3Ri4AownEdJX3vBcTyHD1JhBF0s5cjQVxyNSL+4JVOVJZeCY"
yy$(067) = "fX2yqq9C5kyMlblPckEQbBHSbTgrirE5V3fXpjOpErieSOxT9C4PcqSfnosn+O5Va0jXSivZgi4roHm"
yy$(068) = "qoxy6a6MO/pQKwEHUsA4v1wEHLVCERL3Wh63NAI6OX2/X1JHb2jz4OjeDlu2O6z6YajE3FtjlApWNmV"
yy$(069) = "RXhkdIKqkasEAxak7zlyu6IF8S0fxF3cumgHIVkUPBSHu8+NLizXJRFOe4taepjbTBdW4YQOBDdtIHO"
yy$(070) = "GMncYBT5+Syfiai554iLX6S22td474WoumZQZmxmDmhCA3TvyL1de30uxmWotqKM9Hb2dfE8Z7mR6S4"
yy$(071) = "/N6+jBmivSw+tRbJ0qLO9xBaDM2Si4tCJPFCnTNE14R/+RkeixsguUiYYP4mU5KABi1aRdyLpOlB8i2"
yy$(072) = "pBEKKwBnKCoVQNdV6PoeZL3RqG3+/iyLNJsqOV9EakankBvzRX2Ihm9QIRttO8xGxnonb/rD8IfkQfE"
yy$(073) = "sRuiKO/3e1i0NM8tzPgic0MMLzg7akC/mUDejsTPNZO+CINw+RQ2/jcqQPQv2bwoagV7avApS4HI1os"
yy$(074) = "oPS+gZxefca4L6EmroztBCjOr1n5Nad1c/NyqKFrhRd3qUBGpuX54ZvKsIaU+Ru+sRK/rj9xUmU7QZn"
yy$(075) = "gQs+i5b0rre4EoudgOcqQvws+1p5sh4QHDpWss78CpLtvSJU/+HVcAsbQgeE8pn+ipluGK9ILhzokK/"
yy$(076) = "hG+BVkkdSyvcNAW1inDq+ipwYeQlbwnLA3vswfoqe9FiBZ1NRc99fc5kdQSZKY65PwP/oBG+42AnsOJ"
yy$(077) = "JBsZ7MMqXVov0P0cempBFi0vBCJaL7pc3sMiZUfyKMkSCzTeQhPgYz8BxiAyHkTw0Qvkm35PmmvgpyZ"
yy$(078) = "kUdAty2nSFxn8K8ndMmKVAXiBFHfLANoPWcFQ6AXd8hR+5/u+mbZR89FE0eXDRYRYcHAUdyxAFhHv+e"
yy$(079) = "eWpwix5z66wjUyTeOaFuUI8pvk/V/iWPSfoWeqW+hf+t2y7DrYtyzKH9Fl3vOP2eBP5lbYLmQlc5Hsi"
yy$(080) = "UsiyXvybB99zHnpFCLlKzKUocHq7DlCb/Y9ueAGILYeskG4QaExEP0WXSDOKKL0Q4mesCKIRTlehPu9"
yy$(081) = "dL8Y0Ev5CyzkY5JkkissqeLm4/VfsRTW1WzK4mf+kyJ693CoSFljOVn111hYj0cXWJ767YoC4sg/TGb"
yy$(082) = "qmj31PUWK2gE9Rw7vaJWdQEuI1TapLhH4ILbP+yfQmI0K3KxVdQTZkQTxRNR0svfwEkFa0+z5II5bWe"
yy$(083) = "o9/zxKLxi/yvfUsH7vvizia4cqkoYGoH67GixPGUARiYPw1R49trcU2Kg2VHTACm5gQuVbfuTwDI9lv"
yy$(084) = "K4/0VhXRWL5NBtHQdPGdx6RoqwvKrMH+InihwfkmvuQGg8UXMBtpWk+ccDV3ODQ8s1uUDvBXKTI339k"
yy$(085) = "Z247GpEUnqGs7M3iJzoqCC9oPvKtR3l+O5b1bapolxO1bUlUDqH3NKDnpMQ+uQKUGwVh3djxU5gt5i8"
yy$(086) = "rTLQ944vm69YIIhTWYLnIwnvaeIig+R9PS9LUlRREOIU1J4KFzoE4CJ+RJ6ZU0dr9QCSlPs33itC893"
yy$(087) = "cP/JELezSBXlXnclwAwE6wRYFmDhwOzRcdsaWFw3giTG+1yIUH3uO20pJK1vNtNHElABEsUiE/LbqhI"
yy$(088) = "lcQGZxoWtUw77U8Pjwwhzcy8FqBRsO8t8qDQFsb+DP1i9wOuEZU6DV4UZbQ64mr+HJ4oDnOh0ePj95Y"
yy$(089) = "13wZSwAC1icOBDs+OsFtZmtEKekj0WTGYYd8bxj95U6MC/QA3sbFX0egF2EbFYsCPUA2aDvRopm3ZRH"
yy$(090) = "6LP5sqAiHd2VSDg8YKT064IMo8h8ewDfTyoe9Jb/fs29JoqgbOxgMxuXwIkW0+RiXQIwgkrw3Unh8Sz"
yy$(091) = "CSiKT+J14sELTgvhabHsx88entrHbi09tZHo7cUcLrG6eg51qy98RbXrcjaRSiRYttWwlvkD4aGp4gw"
yy$(092) = "jaa3JcHofgmqrqTGCCmiMQ1hN56rjTLi+g8dyg9VeSB6JELRWp4nhvtPafyKuBFGMS7/UgbGXSRShId"
yy$(093) = "Ro5cQ/xCOf+zQntR4aEmsMmLSN57PJSeo4hWh4rOKKeABt+9vzqMnjLPHZb3EL27cUWmznuDXCnyl2u"
yy$(094) = "I8zVGz4sEkRQX56now2h6ybzmJAaZ5YaHlxRnu76Nhg5C9bNCo4gcKTxvNTo8A/fPnIggeDSc3i9kUf"
yy$(095) = "vu5WGGzSc5EY4r7U5GhZdw3Mx3UpwIgzCbw7zX3bBl0etrQ0ZuYX5DDm9QTDei6X1ayvLhIRCD1Yl2J"
yy$(096) = "IhyJeuYkujow4lWpGj686xjcCLaUEWGB3JXgeMvlzMQ1o3o5iNpTbmgKYnAYnTNzaJvoJoTw/NAPzot"
yy$(097) = "Z94EgoissTyKthG+cedhyTz30Qj9HicavaGST96uRA9C6XZK0dfo94YUgP8R0anCwzYaCYQ/NtjIjSn"
yy$(098) = "CQ8MYJTx/QFEQt5+cgh6/tDCKiLSjwW74SKJRG18A5D21kURyeEfmVnzvNZWZ2giitSvx6Q3Sp/HeSD"
yy$(099) = "biHDFKzVVEv+GR29Z8i1eUiNRc5YueokWk5rqnmOcOqbna8AbFRnRRk0W05m7Fpje05urCOyl68cIbZ"
yy$(100) = "dqgF70eU0RNF28a/pg7iaHbEtJc9C9ENMlMzRvtNqCf1xgf8fnkRlcJyPk9zXEK7cVq7lZ8evsH8b13"
yy$(101) = "7Lrj8UeuC759mhWqYnzvzaTjdyzbV5RFwRHC06zvRYpC1vdGEF2fOMX6XuIUNfd063txRRjbkHmuIvo"
yy$(102) = "N93urT06R94pvnyLvzXz3FFVj2FRIFlHvNeLT+3Ai/urooKgLL6GXcCvzEgjU95xgE8P2rKkXFUxZhE"
yy$(103) = "5jLaLhfAZ9Q5o2PKVqoFfq44NfNfRVPLKI7mtIec9E0zd80qL95FgNj3Qs8so8EtWIyDVVEV8NOBH8P"
yy$(104) = "3I8QyvST7rSqKbuo7MCH7sPQkQzB6roxbtdNJzP/KPiLhqeNd9Q6B0V92te26vd3XH1IHT0+nuftX6Z"
yy$(105) = "gl3dJTU8KnpNEe3vfdZtjh23QRi9ljWu0Ot6+x34O2/fuRgCwpNXRyGI9kq3D9I7H3s7IaIVeTkfimo"
yy$(106) = "r3ZewBJ051BxAIJ3BbU+h1/C6g+VJr9ZPu3oQg/FjBcS39257v7wCbVRUwyML0vJMzUQnW4zjL6EoeV"
yy$(107) = "kVkRakqHhvUOynvaNVr91Q/o4KHYRw9imDOCmW395Zgt57GuK9bUveFUqv9P8YfPXDMvReZxwoorCR2"
yy$(108) = "x8Yg9WuF0FP472Hg/Tgcg8OwomeGh4WVeS5hrn3KZy+YdHkgSoKG7ldrzlRhKmlfV/5IwzURv91qNDr"
yy$(109) = "7SylJxE9Nx0iUlpsJFo2G0s3ds40lT/84y/eKiO35XlPGyerXm2t4epBKFXD3Kse73W3BtBGk2F5T6k"
yy$(110) = "a5l79eO+z5x9OQOQakb5qmCje/Y4JR+6DsAKg1Nz0SgM2z5to5LZD6G3PqKIaFO1C0ZlaGD2l5qbRkb"
yy$(111) = "T9Pz8yvZqt/GkYCmLcVrwHh+Vnxh5MYfZHanhheQ+JwN59KFpURfqhYQ5geH00oNtuKwSE0rql/+ztn"
yy$(112) = "ZX9zqvQe61aiEjZz00vI9HPABy5vQZQRPqhkUalHYpWI2qu0rqZMOPtPXyWgjYahHkvRNS5BpHfD/Ge"
yy$(113) = "ZuSirY7jznwt3Huamrt8A/5kc2lh5+O1mKK1ZXPnDP9VkdHhpeGY9TxjYEbkPRXEl2j5CXgwhZ1oymc"
yy$(114) = "EvcfOFupYNIYNoYfDQ7WtHReEa0Dv1UYXLWGRBb23G8N7KLwm8t7zmPT6JvTeRkx6fViLQ0duGL3WCc"
yy$(115) = "x7uhwRRa+9PL7z8ZPDmCLU762FdSxh9OB/NXd0ehhE50Nko3E1vEh6k1D06JYqihi53tEk8t7o3TJKY"
yy$(116) = "Ydfvos6lrCaq4pQsjx8BkVn3JCORUMPpWWvCe9qd0fPe6gAPO400Ixi9LxHRGjk6mwUQg+fs0ZbWbHm"
yy$(117) = "GuPoXN/rkF5zdHqo5uKF6jMHo+c9VC08FGPNO3RHBlFFP8EgNK1blKgfl16L0buneaUwEOhLpv4dee8"
yy$(118) = "f2vFEz7H3QjoWHb0uCm8y3sjdQ1/R1U+jQRiDHvrgQgGJ7sWgB6cZg2w6pveOfrjy1feQ97ohHYtO9K"
yy$(119) = "viylfXsPdGz3seHLqDPDRg7WTcHRkEnKk9LuxCEEeaFBYlshdQshy9Y/E86LvqRURv9KqBpqyH7Xei5"
yy$(120) = "ho6ESwZTVRzw+YaOnroqPWRFbXGogPR33t8ZCAbxfAeEh2vxRy5yHxo+GoNGwriJRyBH0J6j3ZjiFDS"
yy$(121) = "+wlaY1H+xl4EPfTb3Y05cjEF5D3dckm4qAUVMUeu53UgOnzFALEDu7d/PdxpHIausWhFqOzufBy2vqe"
yy$(122) = "GF3n7f3GW4JuTGN/Q+5qib+h9HdH/Db3buq9D0lxCeKPeBBCjXs3/Bv92jCM=:BB2C"
yy$(123) = " "
yy$(124) = " "
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "
	  
	  goto label_info
map02:
      if mapnbr$ <> "2" then goto map03
yy$(005) = " ~DGR:SSGFX000.GRF,30855,51,:Z64:eJztnV9sG0d+x2e5W3KT20obIEA3CEuu7g69Fjig1KUP"
yy$(006) = "NOKLVnav1wOK9oo+NA9FS0VN07dSEIqzcUG4lg3ZKAwy6MPhgBp2cH04oE9pnwI0SBhRtQpcEOWt"
yy$(007) = "92RTIRAHaICQUFHRDc3t/NvZ+bvkGpc3LwRZpvnRzO87v/nNb347SydJ0WtYmBCQh46/ucRXi0Om"
yy$(008) = "YKnrCfIrQILiyLniyFpcGCkXRqz9wh2znHbhjjU+LIpUG3eLIm54viASnOw3CyLNrf1mVAyBU/W5"
yy$(009) = "PvCKIAOvMYhBuThiF0EO3DAeFkOg+cEQPNUviLRBUARBwa1WahRBRtB8bzSKCyBIMe/ywNBKJRmr"
yy$(010) = "SOiGe+CzylsG5L22injQ/O396g9NvdJ58gghwakBiXzdFHP8k9Jv/KIQ8vyxt363Hhs61qDIbMwh"
yy$(011) = "1sBbP/L6euSlVLHpsMMhe+7anglZSc2f3N3IEOgz14O/fstgTIqMugJysh+0DLYw5LTHI/7pr/kX"
yy$(012) = "/AXIWf19DrFrXw68l/Xvr4UUGTc7POK1B97f6pGXUoeZCkjJBVeRZ+iuC+m4jM/zSLC1/9zoUG/M"
yy$(013) = "BTb6cxHpNke39NHsotZhQBCtBpurfh/+6MjIK3oEm38RoDiLOcF8zRRDSBUiO8AL4cIhI9VW6pYh"
yy$(014) = "b0vlVrjnxsC7BEA7VM2higF+9D3oMFv7Deu/gPUTzYpOkBPBx9wthLxsg6bdNSKngi0ApWr9az5I"
yy$(015) = "rver0vs3mY8JSAXGJffyXuOFY2fnWCRWOmMjsrI+cJ7vP7vzhYiUvdRhvhSQMgxlVXAl+K1yu3Fk"
yy$(016) = "VMzhRQbQ/J19f/zpxicf/2EsvN2NUmQoiAywYsHZ/XOjWFoCMmTiCAhUbM3x7xy/+QEAIqKNyQD5"
yy$(017) = "WGNQbQx+8+HPY2CFAmK/q4nJGbLywn/EuoROmyjeCPd8GP/tfowD1zLICVIsiMrgCmwy1iNTIIsM"
yy$(018) = "v6J7wILvNyBjEXEd/08dP+oCC0iIxxxmPBaQ8iVkPn1zxCPlapv5mDAuoI0R2o+06TDmOyYjwxJ0"
yy$(019) = "sxL5OaSvrXx9yCMjCUGhLHgFIPOt1JYWjh9sFRvNNcgWRuw0aerjUWXI5EMJQTnGOvkxGxiEjN5i"
yy$(020) = "45KICEoYvkNkpZK5AHwN8u8N06BkSchFiNRQP1j+1wd4lIDBYWC0dsK9GukiecGCSCMXab66H/w9"
yy$(021) = "1RgHmVX0Yt+QXRAEJhhbN4i22P1hkzbIRfyLjv9DbAYxHjmPiMx2dOY3Yjz0aZ6JvaFJ18p5W/Rk"
yy$(022) = "glTXKdIHWd6cLhbzNy5LrUQlGPyvEASM8aiQy6WtPNKav32dKAb+B5tgedRDTYrhrKxJkIsx+AD7"
yy$(023) = "Wy5SvYj26VWCvPBZsE0GMw8B34ZJaeMoDvFv/nFyC84XjNTShW/aOJKQKkIG2Ecs8MyRm0Ynll2M"
yy$(024) = "gS0hT4V7Xoh6j7cMEUh1ZtnF5Oa6hFT2A+hmLfSjEC9ZdnE6PychNkS6CIkQAndn1SFRLzAjjj8B"
yy$(025) = "PuoQsgd26teJYCy7OEsuSwhY/+JdFM9B6pg98jLLLs5muzJy+agGbBch+NcHn5KXmcjj9raMtKC4"
yy$(026) = "Jc9Pp2+L+xcq8o6M/EGwFaPB6COf9/kwS4fSlkcflFEcgzu/PopjAb/+GWsX3/Q38W+OUErms5gZ"
yy$(027) = "980IdJgmCPwYCAljJdYvr/jyoY+BKkzhcChKs78yaJgRsOd+E74zIoanE9kB6VDOvI6MQPP3owOX"
yy$(028) = "FD+Ym3kMGXsbOsS3bjQwAme8jExuKkgJzUorDp4BPJJ17Oy+gsCNTwM6RwWHVxZgKreYj91/X4dA"
yy$(029) = "uWwxu7Dfa+W0UkLrqUuWZZf/F7rwfSQjwwo0ny1IbkNBJl0JCT5DSAiRlqGVsRyTA2BDxYhCekRZ"
yy$(030) = "+AaeDc2P0ne7noLMfywiUK42jwjlD72Pwa3l7+25YYaUFiJo1xNsXaMeXCq3GQKnKkEeiNmFdfMS"
yy$(031) = "yi6u0Q7Z3ZAhoxSxYwGpXMMVWou20gIhP8/IKgZEkavrV1E5AqcXHorJDCm7dIrNg3XeljIJ+zju"
yy$(032) = "QeQAZBlWJc35Z693BAQmY6iCgf3LRZEibNF/KqXOP5sJijlrOB3dvo/Nx0tZJCsmIR5GmqObIURw"
yy$(033) = "btVYhFS3oFyV5qYTssjSVxQTbXnNo+YjozHEIl+mWFVISL5/kyKOgmSKiePyrRgr5oaZM6ZIphgQ"
yy$(034) = "tjzn8L4SflkRhfwUyXxMdMvmFkXAMEX6C5C2BRUro1mZapshIz1ytzyGDoYcJpbNZyLLCGgTxSyG"
yy$(035) = "RAuQvVa855ZgHAOstLoICaL9YITNTyewP1yEWN3gpIsQOBvxgkwykxzEBysBWEGKpTOLRX4TMkAK"
yy$(036) = "oDimL/nrkItYJIjEPk3zbWXuSwhKWaFugCX8wFLC+KOz+7xbNrE+wXa32Sip+YjWLQNcG22OugEU"
yy$(037) = "2e4GkUYxCfFR9gLOnwDwsYUSMxjT1PXlnSnfMbvajon5VTSDS3AmqshZIiDe+oGD3Cz20kn/rAbh"
yy$(038) = "O/b1Etjz3tpD45duP9XkStxXBj+CDkN8LK2St1YURIj8UF46K0G6e/lGV0aEmpLvwzgWXcNrZZon"
yy$(039) = "vrYvI8K+ch3PrzEO4yEx/+mVsYzM+ThGglgbI+tUMW8qI4KP1fs3YBCLsGIuVax8KRdZCWgcg+P5"
yy$(040) = "Zhvb4paqeUi5nCF23CQIUJHVDLFt/wOo2O/7m1iwKwT5G0WxIENKVma+B0pDaoyK8D6WIS4o1aiB"
yy$(041) = "MnJ6jx/9gZsq9jZoPUU082RkdCw4DLL9LqrEomWsj21ROjbiStA2ifwIwbPkcz1yeswjDZRa/Mmb"
yy$(042) = "aK1khivI5B5vPioPepdGXuNjNE/gkjx0VFv4MpQDVrFiVz3kYBGc0atthw+C2goJUqz1prtG/laf"
yy$(043) = "/bbD7y/o6F/nRv/KT8nCd0LnSBDyOZ/Ox+z4HXw/BQZ/+p7QOp+PWFixXafaTk0O4VY0khE+KOEl"
yy$(044) = "7+hWY1BFtzojQDIrZVaqCBYNZMEolJF/+z9uXEKU7cOJKSwuLRmJuXGx99H9R1xUVFvIkD4XlBiC"
yy$(045) = "41hoQkbH3FDiUP8BKirmtcKXoL3GyFsf3EQ1VU4xxfxEQI48HP2q2c59ERJev1EqIcX4DqkIt1Za"
yy$(046) = "xFvSMG6yhS9BIwdji4VZMT7yb6WKbeYqNv6SNx/afpAqliKRjIgik2SMV2w1HyEJPwr+WYdaf6wg"
yy$(047) = "/D2LdEpyipXAZiwhjyacyCKCFSsLyaWaXaiKnf6nqpiQjTPF0l1uOVVNsEX0MUkxjDTyEEmx2s4h"
yy$(048) = "+kONY3rF4D7xu0MnWIicZAhMyXy6f/2LHKRqpYr9AJBcHDfwRzIyaxyniJ0pxp8/+Y6MTLPyYEVU"
yy$(049) = "LL2sHKSUKdYib8HCccOf1vqywl2mWJ/YQhqLJGQ0b6rIdkyGke8f30pWUdxMFUMO76Qd4zyGzpf5"
yy$(050) = "rqLY0TstoL2U8qBNFVs/8nKRKRhxisVwvOO1oRvlIZOyMMVOuk205aGKkUUjkJCkx7sldK3m6CNc"
yy$(051) = "hCybEc4twer5XQge4ooi8zEuH1cQuzrB5r89SM0P8feKhHC3BvDtXSKaoJgtIjPu1oDtwv0R2YlH"
yy$(052) = "fCvZnZsU4SIMKVzIPpYVFghy2u1okJggdAs7NCsGHQyGMlTnj4RWGun8VxUjtj9NUwVmC6hZ43zE"
yy$(053) = "yxCqQvXhIYfwNSU0K0XF6B9sW66Jycz8u4ItzGcIwp+7yJB9fooBYDSfKvY9qNihdIpggWLY/D56"
yy$(054) = "QahbLIcod1IVhMYxDykWL4eAtG6DvpAtWZwMeWSsiEwR3aEwFSGKoWh26KvnLjIk61iFM/9AfG+f"
yy$(055) = "R86WQmIDQhVDX7h8YUKk7OKEV4yZk2b+2uxCj3g8wnuyQyL/D+D3a1gxoWLNEH6+cOa/PQDg+2z5"
yy$(056) = "8wQkuX1Zpxi6Ld5nxfdARDiHcTLFYkkxEyKYH3Gj3zIiLo+0zD7G7yvTZAwpFpk7NgUVg2L99I0l"
yy$(057) = "BbkqIaRw7WYFy76MTNlaCcgWiS9c4yuWEC67EM1nQ69u3rPsItAjqmJZduGmWyT/9Jp4gEJExlM5"
yy$(058) = "VYDmV3e/8Jj5LUXkxkhBDmCCUW2nSKQg4AE3K6/gLRKclSU3qz3LCJcqUPN3RPMtFUn0yNZSiGul"
yy$(059) = "ikHpmAW5yOq71Py7SAejLfNL2WEwu7pDELwZb5iQmXDwYC25AUp+uI/cLKCIci9ptrPf4c1/cM+n"
yy$(060) = "DlO+FC2n2Fb3deJmLQBG1xYieD7G4OzQoYpVmouQdOF78Yv0SIB9PuIymLy18ihFsE/m7ZJY5Hfw"
yy$(061) = "rEwvV0L4mMxWvSo2PyCKKZWrPKRlQIYrbFzYwxHPYsWwaJGQZtB0tKM33w3xAacI8Fc+giiCKDu+"
yy$(062) = "eUdVLE5D2UKkmWZW+KsslOsFJDEg5LRGrCKP/vevmGLhJklfHXeTn5ixhPBrJTP/F6SsqsZLBbnI"
yy$(063) = "r5WoEovfpt4U5m7YIcWeRlr9A64r7pFWXAVJzOZvYUOiQgg0PNbc4JZmJVYsoM4GFiID2fxQOy65"
yy$(064) = "SEsrMo9cgEI5SLF/5BTr5yKBar6UXJuRJvMxay8fER9cOkQ+Zjn5iGy+bPySSEODcCeH04Q//FmW"
yy$(065) = "xEYaZKwJStT8iDvTxSMnjhER354hXJ7M5ApJSmYy/0xj/ihNyRYhlqhYw4hkHbPS9DVsZXFMh0yy"
yy$(066) = "bbVGMa3zZyeHSxlCHoXoO3qEGxckF04tolN8+iIyIIloPj1E8ZCcoOtrbdEjMB9r4BM++Qj2MZSM"
yy$(067) = "hQduaN/GNX8dwt3hLTPzX8a1aKibr7Nllplf5qYwRk72G9KuR66QoENQ/p9do552EVRHQL7o+pJV"
yy$(068) = "rZHhK5PMYao7ffEImWFWrh9lbrZzsAhBiq2A6z/FP5TxxHTzkTINSqMPs3LElkYxDbLdy5AFoc+h"
yy$(069) = "kf8COnfhX+Bq1wvMh6Jls0wflFTE45EwF3GRUCUa/ElAW4SsUKtfxQ7zKln48m1JkW1usViAuFSx"
yy$(070) = "77HgLxNKquBzU4x8Aa0t3K2BhoBUOcQRELivZK1Aia722WY83MvODIvIKTsSgBzmPD/LWuk7QxHJ"
yy$(071) = "Dh6Us6AnIZHUCkOgRMMGv1xGBmTCOgbt/ferTIEqH8ZDAZmGmWIr66OlEJAOJfKrC2koQ98zxBeQ"
yy$(072) = "7A5vmW4Pd1j1hm1eHBHhfIzsj3aXR/6yTgoXf45DGRQQiHs3DbL138RVAlSIPiBhn5wHZHOTpgqs"
yy$(073) = "2pOWrKsolB2A7BGoPU9EuLMK1LXC8Of4RCf8It4VSz6WnVDlXGWE9pXp+zh7qI+lNViry4d9Dsk5"
yy$(074) = "RdClroUOq6zkVXuyQ618AF/XP5krI+ntXbJR4o6B9CVkzB7NCO1bv3OQbZTMrbDnkixo9W1uisVp"
yy$(075) = "A+WGjAAOQUtqcMJNScSVfQlhFRJ8663KSmQ5imU+RqzGd9/SzAqPIlfE0CDtLI6B9lWKPGNESlio"
yy$(076) = "fxmCGzQZ+5R6pbyvnLHswiZC3abmR+h+gKy0lF1QpNtMEWDHuMap7PdZdmHR8PVdGsRs6NtyEivZ"
yy$(077) = "4pBtDp+LXo7zEU9CfNOtgRnLYNNTPVdp+hoC+SEzBbH4/RE0X24hQ3YNSHb3PJYRwCkWHWZhn814"
yy$(078) = "5TZ6wha+KjX/KFsoyRXJCMv5JYQNiaMgzHyfJGCkOKbkrtpW1ri6zdb+c4gp6W1JZCQL+xRRRGZH"
yy$(079) = "NdaoYqRwkSHquLBa37ep+ceeLnXnkPnrDa2P5SCZLS5VDJd6DB/jICFbgvnAbAscl44GuYnfoXxg"
yy$(080) = "gJTDkL2kKRPjEXa06VYL39sl5rfykJ+0KVJrc8Ux8g75Xj1F2NGmjSmpj/0M5hXlXKSfIufuk0Q0"
yy$(081) = "3N7XVMZ4ZJRuRdfvEKRhdykS4u+RqhibyHfe3CR5fpnW6dW25DD+InMV/uMISioyYcg6Q8ijuOS+"
yy$(082) = "tgZhHSvR4hhroYSDsZWDwDB+xrwLX2T6D80ds9nB7PQi7qwqNubi2Afizkjok4DQcUk31LHyRhlh"
yy$(083) = "W1FS5F8GYeNSO5fg+Si/LTQjANRPBPMD8od6pJlDwI4OyekYwE+9bWajHajlHpqOPpchYEyfFsFX"
yy$(084) = "bGqFPzoLyF5ScztUQMb80Vl8kn8hIj6X1NzCtzbQaryvTGOG9ETkJv3J9ozFrlPhtK23iYNLAwAN"
yy$(085) = "kKWjPGIT8301tnLI9Jec+cAiyHnrah4iPY6Nv1eVvCIH+cYt0j/9BzhI6SjtGTo2Jlc5ZCQRECLV"
yy$(086) = "UE+YkBJMy/NHP/lSQmynEeUj9YmEpA+9GZHZKv+hIuR1E5Aif/f5hoTkXATpyI+WL0YS+QH2hcij"
yy$(087) = "JLnz1bcyS4rb8hjmA/lpwYUIzGDl0Zcu9Tm+my8sQNRWFE8WLz5hXg6Bq+F2ceSVgghcQC8URJzi"
yy$(088) = "tjwOEu4VRaz9hY9l/SoQcUtOkCu5iKczv9opiszfyEX4umKKzE43FpjPh3SCXLmzANlWkNfyEZ1i"
yy$(089) = "v/t+HoKStFBGQK4teLMpIfPbuQiqLsoHD8iNdyOCdo435FbWFiLKk1yDfAQ9bikj+W6JzOfGkiB3"
yy$(090) = "JvcXIX0JAf36AsXWZMQSn0eWLuxjoYjMwnI+0lSRN+q5PlYON/stEZm/lDv6DtwHgWII2jpxf1sK"
yy$(091) = "eV7dWM06+cjeM5/IB6dnW7mKgT31IVbp+UrlihotxRa4B8lD0L1n6eB0UpM/IEW6NEi+J+P9hVUQ"
yy$(092) = "oS0JyBLZhYyMFyORhOQ6v3TRjZX88QWLkb762R2LEOufFyMtAZnZDwoj9VHRjs1XThcjbGdCg5L8"
yy$(093) = "UT+ai02Y5RWTWjkrdYqKXHz058msMLKxRAIv3uXBrv8VIGLHFqyVWmT3eLH5LRFZEMc0rSyKYzrE"
yy$(094) = "a9wvPi5fiWJRceQxWhGRx7CFXsWQh4WR6/2is3JeKTwrZ5VrhX3spXovWfoaEmQDIfPLD24v8cUj"
yy$(095) = "hWx5Y6Oo88/ahZE5mBdFErDE8iohN/nPg10OeQwfexxPfoIUrikVRuqFkflqcVvi4shwozAyzk/g"
yy$(096) = "dUhSeOGDby48K3W26EvWDMFFOA5BCS0OB6hqHOuQuZ2oyPQcRbStzCoSgn7FGJdy6yako0FOxgS5"
yy$(097) = "p7dFdkvUsQH++NK6s7o8cn2KZOyBuh65fVlFVqdIsl5Tr1iyIo2+hVbpCfLv7ot68+cVCYHmzzYm"
yy$(098) = "1x89SGq79/WKfWtFQaadydG0ktT4qgWPdOp1uWPjZDIAEAF6xZRVzEK3/SZju5703jPYokFGEKlA"
yy$(099) = "JOloFYM5jIIMkskUvT7Xjwt0mLpsfi85xa8PDciP7imK9WDXpp2kNpjrbbHUoazP7GQKd8+1jhZJ"
yy$(100) = "5EK3Bc0brqCbpT0TcmNN9rH5Rr/WgTu0nqNXTOPJs449x8h0Q9+KjNgw4agkGxCpdZacL8hykGxM"
yy$(101) = "EPKRHhkcyoqNk36HIHqHSUAsu+U0ma4ipOcabJG3CRbS196AQwknslaxefVrsmLQ8it3oMN0N0xB"
yy$(102) = "qS6bD5FJDyImW/TI6UfnEaL3MaWmZCVHsJXD1boxWiqVK4IMLTjFqnrFNAiccpPT/ipEDIrJ2zeC"
yy$(103) = "/Ot4Nek2pU8cN+347OQYIk/B7tV2DcimIyOolRhOotq2wcdU50dIH/6emr105IfiPhzDlaq3YvCx"
yy$(104) = "97XIFEcNQxx7qSO7JUI+BDlrZacum19H/82B18lZK2UE7QEn/cb1pJYYfUwaSrxtHLaPjJ4MHeaO"
yy$(105) = "ZAtCTuHi3ltdGkFyrCfQx66ZFJP2L6RjT0G9emNDGN+SFz6MoKhbP7mjRxS3xAjabJoiv4KQjsXQ"
yy$(106) = "YWqGcJG8J93gJkjfNo++xi3xuDgdNJG1iqkIbmV8Aa6VgSGHkRFi/ri9AWdlT2/+1r5OsaNLG0l1"
yy$(107) = "Vz8rDYodzWArHf3cT9xQM19QxEx6G/q0B75bh5wgxNXHMZooywjapfdi01oZS4phBO2C68ahrOgQ"
yy$(108) = "tBOqH+nzsWRcl0cfvfA59GRTPpbgmoqCfAJf7TVMy6viY3eQYpDrnTPk/PJeDAfYZHjHnMHOviF9"
yy$(109) = "gDCO/MkQtlLbXXZWJkd0fGvqf5xg8DGMYJE3l3R+giANCsxKjPSQWxpyfhVBXvxlDylmWPg60lEN"
yy$(110) = "nCoQ801rpbpYYGQDIYZZqUR+fGwPPeFiakUTxqfIWTvmXZIJgX7Uqy4b+VGn5pcqhdZKKOJ8BSL1"
yy$(111) = "LYOPbSu7JBiV5ii41TaX9jGEXEQ+lhh2SWo6CjfV8x04A2CyqEWSqnRnBO0sknkTOlrvl0vn/Eiq"
yy$(112) = "6gP4x/UlPdnGz910X4DmV0wBVkVgp47OmWelrmMoUX4OItHyO3Hol9N/KjQr0WfZzOxCsxJ7/lXz"
yy$(113) = "rNSZj+77HJvniw5BNZVj86zUdQx94GmRVnDtAiPG2oUOGeDhNO2StMgJaWXZccEf+ZYk9/PWSh3S"
yy$(114) = "Qa/VDRmstmOkcrV0K7nXE+QJ8gR5gjwG8hj/D++S1/AxkP8H7+UcmQ==:6053"
yy$(115) = " "
yy$(116) = " "
yy$(117) = " "
yy$(118) = " "
yy$(119) = " "
yy$(120) = " "
yy$(121) = " "
yy$(122) = " "
yy$(123) = " "
yy$(124) = " "
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "

	  goto label_info
map03:
      if mapnbr$ <> "3" then goto map04
yy$(005) = " ~DGR:SSGFX000.GRF,28518,49,:Z64:eJztnM9vI0d2x6vZDbIdNCQamwMnyx322EBy8YHKHEwn"
yy$(006) = "zJDaMbIIkEv+A9I6LHIKmTmsBP9gjyRIDjCQckp8EEbI/gEL55YAht0SDXEPC+uQQw672CHdwOoW"
yy$(007) = "U2GAoSKOmHpV1dX1q5vkOgv4sA1BQ436w6737fdevXrV1Hy+2jEUXr988eHiLxEYHZTaC79EIEBL"
yy$(008) = "HK3vNhAuAwy/DbDykCiQXxXwskdmAIZLA/St29mArlI7WBqgZ7ZLq9lge3+2GlDwHq4GfO2uLQ2E"
yy$(009) = "+Od8rX2UCagqrZUAyBeXAmBIXnHTaaGMUak2VPueP0QbKwFVDARLA26A/CmylwNC/HMe55IaWk8f"
yy$(010) = "kqoSAGVrbTWVUP8fw6VtKO151X69O1waeB2r1C+nAF431AALD8bezZnfvTAf6hF3b7Rfi7BQpsNy"
yy$(011) = "qnrElaKjWvsgLSaKhoizj9HmwfeDNIAO6ZsoAdB4z/v00Ax0xgw4sASg0/c2vgyNwMbQBLwTuta+"
yy$(012) = "eURWEAPf50bjGNrGU1PHCDixa3xe7gkAljUFQDFwsiMMySlOzoqtNIAO6VlXAFDfa+95xrPrTQYE"
yy$(013) = "IxnoDk6MQLXFgDPxPqBdt331b+bxVBlwdSmolC9tHdV6pyZgt2ia4/LYnUo/Poag21KBqmmOc4pD"
yy$(014) = "VHyMqgbgjappnrauPEg2v6gj6xMFaMQBdCLKag0A6O6VUeHEl4GNGAhEX7ID1w+CTm7dz7stXSgC"
yy$(015) = "DI4lo9sH90ZHta27v4jqGkCNPp4rAMRd9NU7/V9JJ+fWYpVORaMRTk3FnxRaL/ebAxnI9+IAkgAL"
yy$(016) = "S3TpPRx41s/z1RQbnj1RgIG3cfWrnUFO9nKXA5JKCP51ravp/HD3PQlwOBDagtEIEnj7YOebn5Va"
yy$(017) = "BQkoXMRGR8c60Lg6KrXk0sb+yuhLCG06pU3n/k+Oi03ZYOvLosmXUER86WLt4QChR2aVZMAmQP9k"
yy$(018) = "Y2AotyhwJwE5ItTTmW8qzigwLIhGI1I13/vXg1JLOft+hxl9JgMTAGojCQABEl/aO5NUmoBKWCtJ"
yy$(019) = "paY4pNNrCUCfDsA78FdiNI4nAagoQIcDxH/wUQ086Qo3MgB3+O9dZOOXrKCrkmvxvHT4S8lolC8T"
yy$(020) = "7yi1gyr1JpzncTlhcdc421MA6k6lUVC1yHVtBaD3IRmS42w69TOnaJH3xse79J4HsQ2zhgyAxb98"
yy$(021) = "BrUTzitwgSme1X3B6LueDvTLGMDvD0FXwL9ycbj6aSsUH7uTW0O2ayGSMsHypgi8fGEw+sfYaPy6"
yy$(022) = "zLQNiwQgRt8ovkSBrYPSEL9eQ/QqIdw5lu6vaUwnQ4LU9D3bKcJb8OoM3/MKu3HRyRPFaBxAe0f4"
yy$(023) = "OxbSphe28buj9+OJ/e5WBjZIauoQAA3JiH4IgB+kqFTCM0ngbgcuAHPkNqkZyX2gR2K0RxzpmKiU"
yy$(024) = "v3d11EI0B/JKIDrqSQAqjagvHWAYRfCLMfz3/RYDQtSQVfIgNSF0xuPHIt//OHbvAK3LQIEEUB/8"
yy$(025) = "p4VI6JBfvhcDx9fHMmAzgJjq4CFN4aahTR5xk0sFQH8+xCq6PqLvbhETkB9f4YvbF7LRKDrdJpkG"
yy$(026) = "whSPfyuIbWBz3G1FBUAlAIjB9oAClWGcl/oVeUiojYqWU+rmE4XYfWAq2YpKCFv8rLpXr9Ks4Sa/"
yy$(027) = "EGcgHfCYQ2jAtKECWMOS/x6L4+QIU3wJV/kHpZ2D7a0DfAq2gV3IDsapPQHroPS3EHSS0TZKmePg"
yy$(028) = "d07RPixtPZWAPIpv3EwDctgvJpdv/Yt0CY8X68GWCuQx0BmUO+eiyQLQ1q6AtezsoW3RLmFIB5HB"
yy$(029) = "aAig+9vw2okNO4rnuC+mBgDnpdFRCWSNa33rBS/Wd5QhFbFKm/vFdoENJTnokJ53FeDnYPS4X9/o"
yy$(030) = "k6FowDMF2LggKkECJ0MJVCBQnM+3cz7ORjnXp7KGKjCSk7F1UIxYF61GLtBKAGr05FgBSiMB2NQB"
yy$(031) = "1ZfeLf4VUWkTLw1R9d24CkiGJAM2pGFSAZWr57D+4Db4cTw8ktK9Xa5yoI+sPcdigBOvdqdlqaq0"
yy$(032) = "XTfw/N01Hwf0GxA8/D7E6X6KxKoy57aPwC+2wOjq5j7ibQ77gE1Z03BdAAqgTwzUIoSQxwEWcdcj"
yy$(033) = "0WjbxZXSD536ZL/4uvM2vEvsrZbD4iGaSMCe5/fXuoPyEBsdIeFwqszo6KUIvA59hLUu9SUyzFI2"
yy$(034) = "cO+neAIHOztBCabyBOA37joS00x5DEbjuf05qARDjwFu9Lh9KABkeQwSHcWAqwFIdG+njlV6jH2p"
yy$(035) = "gBM4DInnJa7S195DAdg4BKOxa2zERsedGm607HzdPepICcBVMnRO8OFbUz/AEYfe3GWD11RiBzM6"
yy$(036) = "X2vHfsGMFlfjptUuntpq0TJArBK6djpPHAg3LBcSEpk6JAZUQ68brrEYCpFypACBBPjZAF5/w4F/"
yy$(037) = "OmeDr6YA7PJrJBeB0XiOI4qKjUVi9CupDGVADS94MdDErzVgSp2PDcmDxqOVr798Vm062hXIkIaS"
yy$(038) = "86HwY/zteuA9jNaqu2D6GyoQeQ8koOznIJdtXHl4YQLFaFM1+lha0uz9UStHhnKQY4K2slW6Fx3R"
yy$(039) = "ZHwv2idgvhTKRs+/mPUEABoCNHtHzxgwVABpSE7ZdtAZ+FKxT2PHGiZXYFOWYLTXibw4Gbv0Hncn"
yy$(040) = "qg3iEp/nesje1NruXAXERVO+de76bHTxavpSU0lwDftiu82mrGJEozNoJ4AecfZRLQZKTNZA6CFr"
yy$(041) = "EWc93Yd1Lvkqn/2CnBQUVJXIbWBAkBi95z1kdjVVlYR6qdnt87zksdnNUq8g1UtUIh+f9rULRboJ"
yy$(042) = "OJgIvpSPd9Lub28V2VIgAWJfSoDmWinO3rUtWmYEeOEkA2K9FL4GEv1ov9jcLz5G5FTxCoZ6iXay"
yy$(043) = "x32yLMMrS0+qf7R6ySL9pX69EwPVARKb/yn1UtCq5mjhZt9GgViS6Z2T4UWJugZe7cLCZBe1kncy"
yy$(044) = "1UvDn5G2CfEoAHJ4PeOqgOBLKKiTvO11utidyH8VAiHfa7mV9Ioe4jXZZZ2u42BKXFMBodVCgA0M"
yy$(045) = "wNTodSiQzwBw9g5c9xz6uslZjgK8GvxHYjR40XPWCGkfkLIdqUbPNoUCi9eTAqDKOhOT8VOn2Hw3"
yy$(046) = "Djq9JU9tWLsVjK5B1R0HHTU6pwKnoqwbSV7C2XsxgBMjiYKUfRQ9e7eTWT1VJbFXSdLRQkBYi3o4"
yy$(047) = "HbVZXkpXSYgHW7C4730aCItjAbh/mwJ0AHA6YYZKCLwIFia7fOwuClTgi0QlV97nB6M7eaHCokZf"
yy$(048) = "JQA/NVZpvRYiFZhuChMK0UfwJUfezGOVgKUbTUrFT2kpo7pGVN4wqeTvutVB2QRISQAOopLr73rD"
yy$(049) = "HCkbUgDZ6FL7xYcv92k2fk1VKQHyNa7SFu3ownVfC40A/KqQqPSE1NDUMxKADuljPwZy3C928Zos"
yy$(050) = "1j+uajggNKTEcOMAqqmAz4GAqeRL+7uuAgxenmoq3aNtbHEtnRj9xa0O1LKA0wnvkcWxtlajfW+q"
yy$(051) = "UpIr6ZCOEiC2eFsyWgWEhhQD1joSgBQgtBsxgAMtPOlc3JdShTDJUeD6tJcYfYSL6A9GZJprxTon"
yy$(052) = "GrM57kXifFtQUtY4QFWyFEBw7/poH0IMV5VnVCV6aigP6fMJBy5xytBdI6mNacQJVaXZl1AnFQhc"
yy$(053) = "aHpDw/FjcgJLfDUJuJGTMYshyTX4TK2tUFKANdROUYkt3za5SvS8/If/LaokAJLRws2+zAvAy+dP"
yy$(054) = "zIDgS3ExoGU+OmSbDB7SkTrVEWAsdE5Uo20F0GpvCpQ5oB4pKjlYJVwSGIAUlWgPGzpq/I553woY"
yy$(055) = "JrMoNgQvDA+4SmLcJUCoqrTI6LN0WZO3FoHDdJV8BdDmadVos6xpgOBLtgQcJ0CgqBQfORGYbh6a"
yy$(056) = "jW7nYYlPjvhiYrtIB3COCul5+ZoITCeHikolqlLynAAfXbZr1LmoxeWAMm+BVFOBgCTwXapSMnv6"
yy$(057) = "MnD1omc2+oANRU3GI6fBgJwC1KisjxUgRJW5NMeBShZWyVE3weNl2Sie4ywpzbzFUpOlztOn1wMV"
yy$(058) = "2GAdXXKHtWXZ6U280wTWnYNWD7CrYpVyrnTbOMAn9sTodyLS1yU5WCul+3sVDYD+Ei7CiXdoZWho"
yy$(059) = "xypZtFgqbeaLyKlHWDEinTqkJGtwo/34hVHWpIkgywoN8JYJSHwJ5g/SOcH6wPdA6tkpABht10b7"
yy$(060) = "SWEJK49AtkGvKiOxc4JVUtYctO99JtTej19sI6ISdqc22UbUh6RM7IPPx7vM6FvaYmqqwMe8IUUe"
yy$(061) = "OCl3B2UqK37RMVxBijgQ58E38QNP9ufTbJXyceg8j1PTUc3gGgoA4hwRAH/frxmcLxkSrwHiF0aV"
yy$(062) = "xF6l5BrEl3ImIKVeSgfkzglUSm7AXpiG9Cp6oarUPjLMWonR0TwDcA1AsiWSj8Vxk5JJH9L8eq4Z"
yy$(063) = "XesL05yWNQxAUmM5hit8Kj1YQHJRKX5hGpIwZZmNFjrl6hy3ZgLyaogKq13eDXDZC1BJtEKNh6Lu"
yy$(064) = "GlJTehnAydwdABciyR3shBfmeikJIDD6nriUayNXKizViNNUagfyykmNOHhGF2Y3KpGVEnELVKpm"
yy$(065) = "AVUN8LOvgJCi0qLsnU9SN/sK5L6G2quUgVSVkikLxHE2uUeBSvKkpTb7hdpHfIpEBZInBvl5nUzg"
yy$(066) = "NJEVsdWLv0tqS+1Q+0sGlUxp5ioD2DcBfV5VgjJ/CIH2l7FHGYdkUOnJIFMl3l/i4nQG5QwgeeDJ"
yy$(067) = "R7T/hpr2SXUXxZspKpD4Erf1Ak9ztVSjDQCZDqM0ILqMh/QulJSg1Zc4I5WjZ2rnMa4EbNHoy9g1"
yy$(068) = "zr0b/N0yAZYIDE64L21grQzA5NfviyrtPatSf8D/sWeWlRttkXTUfs5ba6WtE1PEyQAuAGojQatU"
yy$(069) = "ICB73MR/cF3FKvBmxo0DG8TykH0tBwghaqgEBACOPXAnn77OqV0Bo0qxxUSrQDo/RaUlgVil189Y"
yy$(070) = "aqqmRtznPB4kc/tpRkdeHHEOnHEoAT4FQhEQCyw8qX0dZ6RcmkpTviVigy9s054ANbpFz+NzOzV6"
yy$(071) = "cCsCUuubDWVNWh4nD8TaTrHq0SeZk+cEDCpxwKme1yXviB/aUhav6/HGET5jfawAjgHgIer6e0E1"
yy$(072) = "IKkpkB5bUobE9+Ms7EhJPsZyleSHL/hqd50DYuqupQLclwqH23RXN4y739ICCGnx4HUjYuslibgw"
yy$(073) = "eZqiKPU1kse3kxVchwNNOM+/kIDx33GV2P+77+Clirx7Iqk0UIyu4ewyCvCP8WncA9nitc+ANxN9"
yy$(074) = "KMAsbioqHcQ7r98Li8yLUPFJkNSTCUCHxPd2y2w3G7QqdwO42SQXqz2BMgc2+slzAt3duBOpAc/j"
yy$(075) = "7WYXHf5BZ9dFP4VA83dViTjAu3BkZZ/0QuAMYsa7itHc+WARlwDEXT2TSuN1NqQ2TtcFvtNkGJG6"
yy$(076) = "xCctEJ5j9qDNwYs/EUicD6spAO6DRQCsSvzoExpr2Jew8/lIXwPd8DY2a4GwtgB6vK99BIwu8Udx"
yy$(077) = "247m7S2WmvBQaqq/UoBP7PRRk+Z6kaukfrCE2vBZXCQ+o7Pbxh4xGs/Wdtb6Abszmw7Z9m6AEDK2"
yy$(078) = "WqLTWKUA5SACHtCBQ7NJ/YwjAa4v55LR7UNmdHyoS/xrvp2glJTxWWoTIXkWDrolWJ88K7zjs9T+"
yy$(079) = "0jQGbOhPJpk4jEcTqip5TxKgvAwQq4SdjKjkk9+pfX4BmM9NRj/jZ6X3l2TAZDRb0rCq0hFVajZN"
yy$(080) = "H9GUq8q8qJI71F0vBuLau6CWVlWkbsnJzpdjKsGaN7emF96aSrwwpC9MNlCjX5qAUQZwzMqfUpOq"
yy$(081) = "RNa8o4y8dMr249Y/o+Y+jF3D1+428yWWvQuf0dS9Qdcz56hlBvgDHoXKkHhb9Tcu8tz07P3KZgGU"
yy$(082) = "e/s3pEL8p9KI7Gyrm3Hc6KsXzJfe/C/epYRp7oxew9GAF3ESaI9ZrNkDhKc5S/+glbIW9TrnbHny"
yy$(083) = "ELoBDgV8DThOAL7UBcClpYP+mCQHXLT+Mf7eyrl0NZf25OYgDiD6aRK8iKM5poXKKsCWZYcyAO5U"
yy$(084) = "JIC24Rz3vemQrE2ntUmf9Ka/1kakPHNSeDQn9dKnrN2d1iObxx/vsd2Nr2mxTsfO2iaWBsQRh8jW"
yy$(085) = "AGISIa1tgrS8hN+NuAabyVvNptnoSQJAatqKE/1Qt4GVDgM+JFikPObepl0gVkl4ALAw7K9V6Utb"
yy$(086) = "7fRzQPrsQIfm7SI8jK4PSanI4PhTlIOMVDR/6J8tmoQHAPH7kidtq1Zg+sQyNVp8YhA5NVwJtHDC"
yy$(087) = "MP4lAqWqpEYg2K98q2D8qwJ0SPtvioANH4tDvdI4HZAfACyU4Qm+/yz56cBU/sge2XM4N51u8CVy"
yy$(088) = "4NELj4NKh9o5YYMqDtP+pIDaq2RHUW3vKUPqqcCF+TPgMTBuaJ9pMgSnAAx7eyqQetB6ad6Xjc44"
yy$(089) = "aL00/9/VgGh+s9qQovn17xgYzUerAdO2vZrRU1TIBtTthNnmesaQMle7pmO93A1XAh5LHzlYAkD6"
yy$(090) = "jp8WQNKBJzEBMEecCiAzkDKk6mbWHorheOyJf8VkOZVWuwJC4kqL1kuZRpd0o6c/WhWIsoZUlLpM"
yy$(091) = "NIBusoC+rtJv3AVAqAKlLCDQlzSzf19gtLrgmFkLgJoCjP/m/1Mli+9dJsDkn1cE7tYXqJTSRAjN"
yy$(092) = "QKkt7vAOqfN9tRowK6AslZpOKYkImiq9LAAb/YuyDIyRlQE8Lle/9GTgepyl0huuHyrAzXWmSqN8"
yy$(093) = "Mem10ADKBqJ80ZaBm+usISGrKNw56hrTLCBfsoMkb1Cj3Y0sQFpDMFmz7gOSNpBZ39vJMFo+qC+p"
yy$(094) = "f4BgEcCOtCFJB3PvhaWD+tDWeEXgLlMlw5CGCyZ2Yf1KA2i0vgiQH1edWZVFQ+IAvXGVhQA/qGs8"
yy$(095) = "aqwINFa8wtQ+XWQ0X7qz8ud4NWA+zKqXDENiE8TywMs7fzXgerHzyQCdT1YweroYkJ3vZtWIm3w2"
yy$(096) = "WQ3Izt5GYMWIm0YLXYPP1MuqxI/fUqXfOXC7KjBf1ejZfKGsCvBIW4tmD2lcJw8Afn77zcKvz8gV"
yy$(097) = "RvXufNmDAJPpNvzzfOdq8RddvF6LH8Ndwoab0fIArSpH0xVVKqyYNWbrKy5e79b7qwGv5uLfhcs+"
yy$(098) = "2MS+MvDbufd3EAiXAZZY7RqAlfPSuLEqUFkRuCusqtLZqiqN0WoRh4EVhxQ8kAF/14WnsXdT+95I"
yy$(099) = "MRr/MIVBzt7/Rv0Li3TKUtdxMEqYA3AymRqBnqwSzKnDawqoPVTjHAfAABLJ9FHNCKi+BMDhGF92"
yy$(100) = "9gFS+8bmeMA/VIbYwWbbe0Yb5vUnKnDX+Bo72GxnYATu6pYKzBpRZf7FbOfSrFIHqUZP55PjXzdm"
yy$(101) = "H/zArFI36CnA9XzSDxrTumY0zd43cvYmQNRHjdlHmqy0JzCbqjZE82jo99LutAEYzCM8j816Q3Ub"
yy$(102) = "n5UOGnCMrzHFwFPjFW7mM9Xo4/kWrk1nH1lmlSahAtyt39nz/nxaf8cMKDUfBir/8wM8qNm2BtA7"
yy$(103) = "rTyzjrBnDOcESPEl96EKPLJmvQE2LQXQvHX2UWHWO8TON9zOAASjp/P8BwB8ODMabQKCR3hI07o5"
yy$(104) = "RNXOCQDTtR4YbY44rXMCAhUa1wAYXUOrveHJ/X4FAztm59M6JwBEp9NUQO2cWNA4n/zDSS81L2kA"
yy$(105) = "vsLk52uN6aMdc15SOicWbJRGFzjiPugYfUnrnFDAr8y2Q3MAqZ0TArw9rsy6gxRZ1fsAAJZ61j02"
yy$(106) = "AmrnxIL958kZ5FbtrwgbOyfWvIJvzrCCfcmcvVX3psC4gL1826iS1jmBaj/65EHqDDR5dW0AvGoq"
yy$(107) = "cD1V8xKe9CbBXzfSgJvraU8z+vbsojL7oJfiS2oiw8BddF6Z1hspvqRmbwDenFVmH2nzA/Ml1QYA"
yy$(108) = "Cq8uZ73A7EufqCoBgAuW2Y7Z+XRfagDQwDG9bp7jEFKMxsAcrc9TVWrvy8AdAEEBVwLTJX0JgNDC"
yy$(109) = "V2im+JIacdjo+bA5T4s4HTiFH4c4gC6WBOAKx+PerHu5ZMQBMLjDeenOGHG6rAD05z0ccWYgCnVf"
yy$(110) = "mj8l9VKKSk/1AJqHUMKZqxmtIUUAHCOz3kuzStOKCQBf8s0Avd+K0aHRBjplnZmAc3yR+mnKHKf6"
yy$(111) = "EgzpDA9pRwPojWuodxoexwgBMKs0LndTgG1ziBqzN1UJGSuB6aZaVQKAo2jWM09ZakPKou2dBlxk"
yy$(112) = "yYiLKDDbOVmy9gZgTFQy56WZmu4J0EsF9OwNiW0MxfpXRpX07A1vMDKuH1KyNwAwx6WtULTsjd9g"
yy$(113) = "1qmY1kAp2XuGFSpXoKo02jCZqtkb3+VXXiM1kY1RVQXwu1+Aa5irSj3NALAFNoxSsndeAXAE4ToD"
yy$(114) = "q3TPCLDSQVBpfji/u3eJK4HKktkbYtS7wkZ/s2QpDQHxbGuFdA9+Eb4JEZd1BdFofGPGf5K62jUA"
yy$(115) = "2FVv8RzXOFky4sgSe32FiAPfwPNWWgCZgMqKAFRMp5DIllyhkD8mcZoacSagD3c7LeIMKsGUcZoa"
yy$(116) = "cSYbSAym1XwacHP1fHv9+c7VSXeQ/YzuEodRpazDqNKywMpD+g4C4TLA71VaBvi9SssA30qlpQBx"
yy$(117) = "SDiplBd+iVdY6hj+H9VNOXs=:6454"	 
yy$(118) = " "
yy$(119) = " "
yy$(120) = " "
yy$(121) = " "
yy$(122) = " "
yy$(123) = " "
yy$(124) = " "
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " " 
	  goto label_info	  
map04:
      if mapnbr$ <> "4" then goto map05
yy$(005) = " ~DGR:SSGFX000.GRF,31876,52,:Z64:eJztnc+PG0d2x6tJguxgOzMt5NJeMxpqfckhwHLgC4XMih"
yy$(006) = "xMYP8ZIXeAJJcAI+zBMiCbbY0w8mEwynEPhrx/Qo4+GHaPaGiygGHlHBhx021kLkFEeoKIilvs1Ktf"
yy$(007) = "XT+bpJzFYoMtjCn++rDrfftV1XtV1e2i2LjEmyMqE3Qn6/zJzNLv3Fvj71hmcrRW8f5QmOV6TP3HMm"
yy$(008) = "vWzcbU42rGpoGXbM6g4QYM08DvVjNW3TZheN2ijZkI7VQzFg0itLUxg1BzA4ZqEKNWDI/r2UMYLNpr"
yy$(009) = "SeV5tfvbsNJ/7EwPNf017ZGYoOIc2Zm9Sl8wdYMv4xOUrmmPaKetSt92tdN/7mzChPAw+rK3pj2ECe"
yy$(010) = "Bhu3FnE4aUfSewd2XRzYtxjdEH9xzMuIgtur0dobfQ6ImDqVt1q8HD4F7irJ61fzuBx9RBeDGz5+WV"
yy$(011) = "xIBFCM1WMYuZrNsAHhwO5004k6ZjyXd2E9Q5bzuYJbcnO+1LTPcx6iSBo26BYM768jmtoc7A3fkw5v"
yy$(012) = "nFjsQ0wlE8eNvJMHu+f6gxCL0R24kpZ2Y7CgMP1xxHEf6W7sm64dI9uG7v4uqvc3/7b43BAcoWVhyX"
yy$(013) = "jsa0IuGjudZOO2h7bvWERhhb23YDHWF1nuDeB3mnOiR8R2OiIf7sJJrEqH6mI1eCea7UjfQJjWsPcH"
yy$(014) = "evtyOv4LotfVkD6tjNSfsI3ZrHGiR8J0UGE+5eBLcne1Q9pSiMEod0d5/c62enf5K6mLliD2ndByd3"
yy$(015) = "s0e3L3aONCLjGlwVBjM52Tv80j+4rjFdwVwqDC2Nm+fJ9YP2QDvOPtcto+rx92MyDj999MGzrdecGk"
yy$(016) = "zbir8llLnev2xf+1AFHku6Ke00Jo+tT66GrP8uyy8FsxgZWuPh+2rZG+mBzC+e2/2NlSj7qneoG9Mv"
yy$(017) = "HEyHMsmiO7J0js64F5+jn7Rnk7YlyrKPpw3C1KuZ5U+UutUJ47+e3sOAEV1w3TxVg3Niz+gkGsUaUP"
yy$(018) = "rOQj0/6Ie4ZBr8zf37+GFXaD37XmXIoNBIG+FtLBN/c9BSdMuWfVW3ITapddqdtLtCgzp6M5HtudSZ"
yy$(019) = "LjBtYGLEWsMWCepqGiP5QYhr1OqgGohWhhcpMLFopzum7zRAgxOUsFEFHw4HGtGh0CC1MGhKGRLSev"
yy$(020) = "CITdspSq0tfo3+pRHuN7hwW7Q/an8r+jek+5sHvgPZaJ3Fswv83mNZt+K6xnSBCUC3Jmm2yMPuHZ0o"
yy$(021) = "jK391HHgF0Aj7yDqdVFT1vrlc4MZYK9j/sbFDskPMXuWdzIbM9rCylK55FiTMvmRqdsAvheSQO66xH"
yy$(022) = "jnjFmofTy3J8K6dbAB26RK1AnrPH57cflIZ7BWdf8G1hqM/jVxoqADTNutW0x+OyHHQXcRhI08MnEx"
yy$(023) = "zWgA/9whGvjvPP1titDfKho4mejwFIsWeV/dGSLWJkQckl89tOiGTThoRHBeGtBwKVPGIVgPzd9iUv"
yy$(024) = "Wg+yH1HSGmiENS5Fl8FJjdmDD4b0LeKeOQ+eK2zd9QvdmJufT0F8s4ZF7sGfZsL7z7KDuJeBp5l1da"
yy$(025) = "jKcQJCnM6KR3eIqZHj0v3gn11DIOyYqb+nF2Mdbwjhsh7ay8Y3YYEYdk+W3d37CzRN3jIE2gg/Ri0Z"
yy$(026) = "uWcYg/sjDt3UlwlIBLe7E0RPI+HmV63YhTenE3psqbTH74jaXfaZ382eGjiJ5dXqKqOSE4Oyc/PTyL"
yy$(027) = "aEo85EyrgoFeLQnOT0P6kjO9VuwcT0k5ehj0nhBLRP2Gor8uPjOZAT5Hj4PuE3J6jvm7iWDytlE3Yt"
yy$(028) = "O3Uf1Y1FQ7zgKZDK4O7qse3EyVd7vC32ZJ38Gc9DLl3Z4YS6ZPd2x1a4cH92kjF66zI/ztkuQ/ahzS"
yy$(029) = "wRWZ/Ok/PQwUDbY/FWPjI525gDMCY8kFYSayMsyvH+n21KFp+rGPjn1uncbMJ5o9TTRIQIPRyWtT8k"
yy$(030) = "ZSfuaIQ3AZxpSJKDM0mFyLd7ATQK8ehPv3Q6KbnAdxf9vWfKdDzjzW4GgCGtyIpRSX+9vnGkO/whn/"
yy$(031) = "SD6Oo4+PfUb6w9qHNK5nBQ8qjBlr4+n0jHS+TapBWG96KfsEuxKzZ2eq6jZoAuNBHBJlMBX1Gv8ER8"
yy$(032) = "JsDPZMraHgOOSL8Bx8TUzJdcUYXFdzpi/YkyOiQVdm6rx/W8z6sm4eSeIGrTZhyKSAYA4Ek/dlDWjv"
yy$(033) = "NEDoL2r4/KtJxs+41vldRest0gV8xXwngRciyMYZBLPnrqJ1QIaA+xLTKJlDwSjH8Xk+1kKDL0L1ON"
yy$(034) = "0Bj0OWSv8Wpswe4jvEQmFUqRtStA4fc+aCM7GpG1LOaSgcLID+DEndG/oZ97dlpORZIemjY+Zvmm5c"
yy$(035) = "g2JPnkfC30gps9XjTNtktLYQsdyq0Rs0aDuVM0HruND5V6ZBeHHKNDiRGGue1XkcgabABKsYXrcObj"
yy$(036) = "EkU8BhX4e+dW8VA7/bFe10iLRi16CL6yKYlKiykmFjKLRTRHVzMqKv2qJWIfQAh//UhlW6QXCMmeNg"
yy$(037) = "94IzD0zmmVo34pKdmv/uxO9S32sZzDLcl320SeeEwv3mL6aPQIbDE3mWjLW5UPHrpj9qoH18Gpu96V"
yy$(038) = "k0amLG0sd3Z7JfN8BvBkS32wHTsGsw2VJpc95HnQn4QYhzpoTaEAx1DdT8tIud7S3yZHJdMEd2htXN"
yy$(039) = "7zR5Q601/ZjWZ2Yw82JHaT8ePaXY+i06KkbSXJctP60TT2HMGWXu/GAwSn5a+kkjPPiaPf0bgymC0n"
yy$(040) = "dqrdkA0UQRxyEL+r1kr2Qs/tZsDEumzXLGpJPYmVyrGj7m3vZvyJOBV8l453GHVRwPpmc9/kXdnlz2"
yy$(041) = "6/M4LJn7rGerdwzdQokpa9GIPmjwEXuoM6kyl0ZyHqLB02B38gn9HcMPso9kfyMaAQPtNGm7GHWenB"
yy$(042) = "ynQyYM2GrdQGoMtnnyxKe1Jn3VKVesZHiepTDEq6HzJQz2PF9Rxhr3kmDAx+FHk+T1W+GvrExQ6sb8"
yy$(043) = "AHy7S4KQ1rOrOJEYZs8MyQyRKMR2D2ngMkFHJSGY/5Fif8+/U6pHSg11LYw6fw1nxB96bYjF6Jz3Vi"
yy$(044) = "J1ivZ5cvxfb4DrtMWZpsVHVQYKxGkJHhd4QCW1EEv/VoehpEafYA0S+kJaPhF9lcbQJ13K1JGUmEj5"
yy$(045) = "wlirm1xq6kvGqLFLAzIfYjoMCswIwx6kxP4BYi4JujmZdKozdIkB6xa6GHmevAkTbkOhwWOXPZcKg7"
yy$(046) = "2ms5oxz+lq3WR/A6P3ka6bVHhfNeprDNfNzcyk8xOWj5JuJpNZcnTmb0QD7yvTd2TdJEbotvvAnNuw"
yy$(047) = "rX1Q9+8gYHqW+ZAXEhOV9jDdfnoYm36w7GU6I+vWyyxMjox5y5X+psX+pQb3/A5tdsr7lJkEdgZ2LL"
yy$(048) = "VvWxm7v9XQ9rPPiph2dNLMkMk0Snu8672Mp4P18mBmX8U6J9CtvkXmiOkwkVYwgXScL8IBZ7yOzmTB"
yy$(049) = "m0IDUfF6kE6CAW8+5ey/OW/pSQzWbchfRToz829Y2qljWVz428L0g2tsXYbWrdwE45y3hC0sbG6dfr"
yy$(050) = "umM5fGvGUIfVaDjKf0257OZMvbhm5vxqBByZTaMKY9MpgjWH0OOuIIicbM0KFWtzf8ciUrtjIL76Jk"
yy$(051) = "vEZvdNb7LYnFpPWsgWZPnn1TMvWz6LwRHgmmxuuqMnJbCAfEtWFAlXUrz7Alz7oQtfGpblJgItnz4l"
yy$(052) = "KK+e4pzFC8ClRmYVl3Lgs7TlNl8iN1fieiMjENGCNGe9a/3dXyeoXhnpY2nboh2kgxgxTdULptYbR2"
yy$(053) = "iiKqGy87i0SyZz0mmMnMy4u2XbfYtjWrzvtrm24wLsTI2NrGdLMwSDD6LhbLWILKvJ6uZ+kjqlM3jx"
yy$(054) = "xuEsgMa6pO3SSmW8XIugHjU16vm50Rbq+uO6u+sxhYdYNZcSczU7VWdROloTDZp3YNyMzQ0G6PHIe4"
yy$(055) = "mdDC6P7ms9kkXroK80JlFN1Ep8ufMHt6ZTttqsz9cJ8zI4XR45DyLO6fRmJNgtvljEPqTIPePWESf8"
yy$(056) = "L6qou2i+kq78gamP5GS03a5dB0M03FntGZGOOFMPY4pGSyM7rGUs2oZf80HOjvWeIQbgldoiu7q0Sz"
yy$(057) = "x1w/lZgOecPI0We/1tdPeYfrmxsVzTik1CDsgG6sqUoxnxmHlExviNAhZu7bGTMOgYKTU++UbHVQ8h"
yy$(058) = "n7+umAfCfBgX+bpvbIEsfLcUjJILY9AqoVGYwZh9TYrEjE7PANRopDQANp7uMMtnuoxe5vJfPGeozS"
yy$(059) = "1XoNppuLoTHshV9qQHzHni/IDP7e7cclc+TMMaxxSIz/q/UeO/YjyXEIqX3G7T58+N5UTzfN/SGEGT"
yy$(060) = "V7dPkvO4mysnlbNFDiXjoiHOC/h1WMmjO9GbPwvwPLU9K+9IrrFzqUucD+pp6hVXl9s4P4EoPGLKOR"
yy$(061) = "qsGQf47HRtrmDHvk2CVQmIwwnp1Rz+mAPzmw7JFmuv3nLVMD8uNbsAtBi3j4Xra+g8EqB8MqJjcZ6m"
yy$(062) = "y+jcnHKgMa8J6G9W+GPRLTUBhvLUatG3Lq9sOTcmzsKgzp5kEDo6+Sx2CVwd9uA1Mz+qqFOedQFh/m"
yy$(063) = "YeWpJKbbe9q6cyJXH/xNHu0cbQ4Y/tNkW7Dcga0aT+Erq/s3NcmmMWyg7F+xXvchnUDGKL+zqp36yj"
yy$(064) = "+VTILKXgMWqRSTKnTjTLZ6XBCF/zaMCwMnQzToaPbU1xgXgKlrTOhmrOcUe2cc2ZjFoclwDUYnWpIh"
yy$(065) = "1qZ27EwHmK6VmQ3MtlDqZj9O5tKAdPOJ1XcuXUy4ihF1UzaE4EHF2r/NBaO6NNXAodvoc87E/KMtWG"
yy$(066) = "LAzGHTPjbOLPtDeIUGehIo9g2OC6mPr1Hz2x1a2cRqz2Xbxvi7dDOrnXH5W6xW08EQDXzxRZJrhrax"
yy$(067) = "cXnnymRIGYFoPXXbnGOf6kA8AWezHkffp+p9ikSDrbeVnljY80LTre7vJqJz9PVLGOy6eXxZgr5KVz"
yy$(068) = "C06gnd7kEHyWY0tWggMXXGhPR7hAlH1YwodBgdIJhNUutWvW+QlnXiXqTFRJVMWTel/h20FiN+OYYZ"
yy$(069) = "tbCmMBXjQsl06xsxiKQMyM2ABqTnaHEZ4JHmTKgMRYz1BQvDJ1M1poxHSdM5kuV25AvLXiq3ny3FAO"
yy$(070) = "E4A9WeF0VfZpSfFZd+eCpzKe+n0IaOAdIKZ8ReD1z7v+wg3mXX6Do3EUaM9zw/FUxEJZCYiDE1jUn9"
yy$(071) = "0h665ioYNikgFbFPtdRNy6r8IXsSafNI5bUiNzjDTYd9OR3lt8R+cs5cq9HTXuOMcz6k9Lf2jYRW2m"
yy$(072) = "fqCZ+uqfNI8n5LdnZqAVNP7FCtBxrzsdoWSKEmO+dDFtc4w1XyybZ6HCTDnBDQge5v5T7VEZcLWz/F"
yy$(073) = "j03OlGEfH09PdQYMJk7pXj8Ve3XZDBAtR/cRuxZI7n3M/b27sfSLT4T7mPNIl9L+kJrRoWmFxyEXvP"
yy$(074) = "0MUHTQQMJTUm6buTY1SziDR4TRVmn6ADHeMxj1epkGUuWy9285Er6DqyIN074F5L7ziDF0qrtcziHX"
yy$(075) = "vpAKGroVheRvvjRZoGZYboa0M74MfNblRuj2aEx4ILYkbdHNpCaT39mXdVMHuoH+jh6H8F5GbKc6Qt"
yy$(076) = "LylGzPIuPjgmjJnOmgoGtlSnuESoPyO+usL8h1AZdwrLG862KGTobMQAqmozHigAqzmBhz3jH7mL82"
yy$(077) = "5i3LvL6hMkRG+1zn0szrKWO9ewLT7TbP632kaFCOEZKRYjxljHYpVreCEecnRGZMpBdn/EbsoRfW6L"
yy$(078) = "9hj3sFA5dbOZhFat/rTjdb2pn8Dl+bAnuGiIsMAYm50K/PWyqMdxIZCZOugeRvdOLpvgmUzDO+J2sg"
yy$(079) = "f0g2yMSueFTs43qbn/mYMBNYDnUwdL8YrtvPP+HHxw8RuaiN7VQ1mG7GdOtfxXTlFPJL2BTdRCOd4f"
yy$(080) = "u4+PlpP6PMVi+B6yh7mEkcGoi20P6HhDBBeIQa+w0zCCmZSZv7W5DSDybtbivoHgfGbgozr/f45cCT"
yy$(081) = "4KhOGPY9k3kuzmk8ROyHO9Q7zZtbGOuAMSKb6GEnUhcWAe9TxjKXVs4F4LMSI7YTCbYuxbTrsewNlx"
yy$(082) = "nyk2FIRcOE4XP6fEiNj4QTtivc0yZ3SntmvJ022IiAGZqW3ohdGoj1hQb6e/LVjhaNWM6PmKeIDv+N"
yy$(083) = "1K3Jl7GM+xLY2lyrSxm6BMoZr5Kh5a/552oKaTK0ryK7Pvkm02D32GBs+WkkM7djF7M0rmtjX/XdWi"
yy$(084) = "vXtdVjyEmp3TfMqpXxW79ksFbkqklgjEnLkpl+uSPVrVb2bFN9EaNk1GvUcOW6jyG6ClsmYYt7oSTg"
yy$(085) = "b6jpHz2uYtR90UOcJmDmH4/SCqaMe0mJ6jHu3VvTRzbEtc/hEDTwbKJJjDbWH4BrmleWKEyxVRTK2A"
yy$(086) = "hb8b62I2rOJDFe0vasoiFXDItL2nYQFUxv39JCZXuWfeM6ce1SVQuzk+pMRWHx2/ZirGpQVVhM8TTf"
yy$(087) = "nCn0+xKsZvBRdja1J3/Pdi+DambxCsdZFHl/Yw1aRvy2kjHWKNdglki/LlQtXiMcSq9Ym7usZNQru9"
yy$(088) = "ZYz0L6VNI685Z6qVh3Xo/ZSLc17BmWr9bWINmUUcuP0O3ZKzCTV9DNW8kMy6d8D+BKJtGZ/O54Y+bF"
yy$(089) = "036VBnRfmygszzquZLRpSMa0+pvr1l9pj3kdS7iSMedDeisZBSdfJulPpQbm/Nuk0t/s82/H//ftFL"
yy$(090) = "5ozEHezD5exWzpjCfm7JxFPw7bD+tuczcS47qPvGa7fkFhAk9nbu1UazBEvs4sV7SfjiXPWu5VM17c"
yy$(091) = "HW7MIHP8yW17ZKQS7D6QehGmwZPq8RQnqtIgJMbTlee0LGLuqZqBdFhbK1j2VvibhVnDryWH22BcMP"
yy$(092) = "aP6veVshR9XaaYbawbTmo3YPj8dXXsYmXS+uZMUl+tm7ZXN69PNme2zfs9VcDUd/58HSZh//J+Z3Om"
yy$(093) = "QAevoDVarYEo3A9s13StYj7sb6zbeL6GvyXsX55nFdXtlJSBzmysW75OOxWlvj6jtdO85m+u20Z9oh"
yy$(094) = "jntlcyIkpas0+0HOd3ppvOjDc5p/VX9oPlRsf5EfnC03TzdvogJu10k3vB5624X6xfmG6tnY2ZWz8n"
yy$(095) = "/pad9db4u8nyhf7Oprotlrf6m+r2KkzefX+DujF/C9fp33jh7XSTcZv5zgfKfY/XY57lxaYasPJ7z+"
yy$(096) = "vV8qNz9N99Xv8KTP4Kur1S/7b9ChqgV2ASyz58Z+HrJf7G7RR/e+N2au7VhfUsRK53Vi/Vg8LXF8bf"
yy$(097) = "qMwYv2zBJziwS+yModsYTgCAUwjUbMyypfsO/npO7tc9hwUlmz35toVZkDnT+dXXDmZsqduMeMc8NW"
yy$(098) = "4u7GqnY7guCmp8NTP+1wWuNoeZi/ywGBfZ7NDBfH5lMg/yUfGymF6OUzvzQM/rMbO97GGl55d9hwYt"
yy$(099) = "k1n2l3vvnhfzxw4N8m1/R68bMF3Prduy/47B5P38fXStuHrp8J3lrfFDnVngs9YcYX+7sjP5+/2PdW"
yy$(100) = "aGme1JMc3PUyuzKD4zNJhBz/KgmC+37PYsjPx0jGX+jjDZtkO3/tLQYFIk/WK7mE8vHbpFQ4O5WHoF"
yy$(101) = "/qWr2aVdA/16Z8L8x+sXuMbZwuVvCBn+tp30L/I+1k26+4PCPPvcaHPbSfFwMcb+9pvK9qMwO+nTp8"
yy$(102) = "Bc5Gu3uWV/0YLdnvNLR39QvKbH8WMsP4Ldnu526qXbOjMu4gxrnbkYMy8BZvGr4iFup89TO9PQ59LG"
yy$(103) = "2PzlTfCdC1f/RkIxg+ldXVQxfV23BWQ4cQZ3vXf4mzH/Nn4BUWrsFVfziUMDYy5tPAcGp+HZ/NzBmM"
yy$(104) = "cB5u/6COvGNs3pTP5WW29zeNxZdvoTrMGeQwNjrhOYoteHleK9oyomV5llrw7jwpFDNyNvHF/BGaiN"
yy$(105) = "sb9ldg0sfo2Pk7+Dj569WJ+Z4KPvxjvu8bQw+ze4VrZ3flExnhZ6vkCYPYR955nT327pbYEwQQf3iW"
yy$(106) = "5/szH/9XGE/c01LuyN9Xb6FL/OsO9ki185Gdtxpv0Z7t/0SyvEfK9Vg/SvIA5xjY3/ft3GfEd8p+30"
yy$(107) = "UVvdYBZrnrp0MxnQYI5956p47tDA9FEYX2f4lzbxNzImN7aL6dyhGyu6BsvG29gPorXHBXI9991j3E"
yy$(108) = "7b9nZaXOr3MSPMojh29wcW3SiD47fLuV0Dcx4J2lwxw2Jms5GTsR4H/9B05hpPi4XNdyC4nBefu3T7"
yy$(109) = "cGzTGvztuSPuZUG5bg8sc83T2MEsWjoDAWr+EOJe6Y5Sij1k24bexxczrEH2/TsOJjb8DZgFtscZ91"
yy$(110) = "ryBXL/9TeLqrj3kc7Ajefya3BnbL9yLMk1pqhX5AsWPyA7CR9g3RaONrcM93VmQdy9Ks8y2ylhrkC3"
yy$(111) = "m2kVo8UUxfLNwt1+8Fii+xukUTgixVq7xuCxMS5A8AyJlpOxjAtkM8s2+NsNuwaWcYEwLWin+07GqB"
yy$(112) = "u8ge2ZvnTEb3ic09spJJoP8YfzZ452ahlPwZY4Bd9x+Zs5nuKwsvjwEDPfuvytd2gwuEP48g1whalD"
yy$(113) = "A7N/g2Xyywj8zRXzmX4Nzpl9hHXL+6mdmQSGBrhxT09BA0c7LZ7p9yiEHLBYfIfRzDGeWtoCdEX5L8"
yy$(114) = "eb5fUQYDfxeLpw+I6NAcc+wfHbS8cYbGPAeeJ+VfxmtFPCzMZV7dTC4E5nVtm2LXWbkMHR2U6tzDmZ"
yy$(115) = "y7t6rt/WsEqDlDDOPMvKwEmF8XT9OASScGKPK+61MpCgr2a0ebEHxQ8bMdffvfzYu3z07mUCN6pcTw"
yy$(116) = "Nyy5l9uBevc87OYKrKH/7+N7X8/5wn/6Nuf9Tt96DbK2m9yVrohiX+X9Tdstc=:7936"
yy$(117) = " "
yy$(118) = " "
yy$(119) = " "
yy$(120) = " "
yy$(121) = " "
yy$(122) = " "
yy$(123) = " "
yy$(124) = " "
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "
	  goto label_info
map05:
      if mapnbr$ <> "5" then goto map06
yy$(005) = " ~DGR:SSGFX000.GRF,31059,51,:Z64:eJztnd9vG9l1x++QDDktBhSNosEY4JrjTYDkIQ9UHKDcRi"
yy$(006) = "tS1SLbhz7kPygZtgj6VAr7YBnZXY4lgTJQgQr6lAfD3j8h6FMfFru0xhADNLDe2j4ENekpohQoagoq"
yy$(007) = "YBpLc3rPvXfu3F/DHy4SFGgGXC4l88OZ851zzz3nzB0qitbd5uIPX3z1coWHiMxt72CFR19Apju50g"
yy$(008) = "qPWyKCVtqK6yN5AZn9X0Xe3vzsEsQgcsFf/cCYLeXB2ojb/K0izJbxTxcjBsXGjxYjBsWsr69+YNSW"
yy$(009) = "Y+e7ayJW4Gytuxff3l4dIbbcclvY/PwCRFMs77b6HfycjmiK4dGNOvh5DVs6gVMdodJgDaSKkQmy1k"
yy$(010) = "Fge4is0WoIsaXotnplVOykI5pigLjjftX89mxtpCv2PQiJe79I2Uv2/anZ/LPtzUnKUdmO0ceqQXlz"
yy$(011) = "kGaJAUE4th/ezx6lEKWcYVTevOrtt44/9s2ImzOMytJuv9bq1VKOrPwb06jM9tHOIRqZkY2J8exPAu"
yy$(012) = "cqSD+XJmQvcPaC2lrIe2fIO0gfMKbIX9zHPmOnIlSxaSghtbDn/sL8/pufMcUmo66A5ErXT9Hfm5F3"
yy$(013) = "fsYObOx3BVtQ4HTOUsKMdcKQ8ERG7M5wGXLZr4vIgV0dfrEEeXlaEWzJu+1+7e4zcjJ91fz/ZIpN6j"
yy$(014) = "IS9mvtZ/vwoepe3HhUTmqyYh08MP0yfpnV4lmTHRhFYlusIR5lrnfsYMc1hECKvNrSEKc6LDdR2aCA"
yy$(015) = "yceyPs67UC1fHqH/Gi1GEvOxj7XwFPClFR7Lb8+8MzUmVwTBbhY+/oPrY19GyhOm2OxaRBCEsuGgdn"
yy$(016) = "eam3QHKQc2QpL5YLvjXbwzOnV+sg5SHb5z+7mzJyNZjoSSYuAlPeRFX1TmD+/IkmX/wY7dUjIfEcV6"
yy$(017) = "W9eP9sc9JQbcGzHFXpqQ7fBZTUN45L+WEbSTc3dyd149rz7JKee/xG0pK6Oyeu5Uw5OrEIuwkmKIKh"
yy$(018) = "bYE4wE/kqIbUOu4NWbGeRlPOn9hThcyEEJzC/jSH4XRJATumyKjyHUhjfX2jqSwT+ay4TrnIPdrEUe"
yy$(019) = "PzTbIo8XrH6VjjJ4xME5Y5NZOj77kSMjOI5xJD4xBftj+F88V05lxbBaGZJiZ3AwzyW2+wj9NEYmja"
yy$(020) = "5kCxll5TExP/TZwaICfr4XK3Z1XUlFxj+jpiDiLdzHwkhBoOZyn+BohhMzh34IHkdVwccu53XZFjD8"
yy$(021) = "oozlwtJ9k/zme7zkWIAMMVLGyB1iTIeaz5FwpiDwib2yl3Hx8PwGvLMwwp+DhAObNDXFsO0/7tGxRs"
yy$(022) = "4K5kr4rLp8rrQiAxIypIl/cRv/N8DvL8eKzXYiVTEs15OjkgXDEyaaBvyHT2zZkI8J5jsdbP6FU40/"
yy$(023) = "qomFztoLka0OUaz6b3F2nhcU05DS4AHKe52MAzGtEP82kyDz/ZZiy0YNvKVPzb85QtUmzF90I4rNOq"
yy$(024) = "guI0WK1AjiXp7iKRKR6bbyc6oYRpTzQhxsB9UaoJi7e+EOcM4F/8CzC80WlCdx6Z9wNIMS4AoTNEG/"
yy$(025) = "cZKKZOl0CQjOZquN2CmtBcgD/B6v56B30W1fkMs6Sq/Ex0csmmHz0Qb+DPqB7gnPx041hMjVxkjfhX"
yy$(026) = "OYpf/6TjxXTlBBRXYuoGGCctUdpwQHlaWFQ2HCY3JWsQVtVYcnLC6dwVuTkp4NMWdTRcrVwzINgB1A"
yy$(027) = "HKQi8z0VKXl+FWVP/XftEWglZKfxXPmJasuNnrvXu9E6BxEaeRSH2WaHKRZG76uIBXKBYvh5nEce+/"
yy$(028) = "Vonyk2nrVUJJcrfXjk7KLqbq7UyMRunOUxeYTaqi35arDhnWI3w6HJ4dnfQgQPTLvDopnDGy0JMs+q"
yy$(029) = "PgYphr0/Pqj55BRmNOTNK83HLDLEQjoweWGeRX+R3uwiCM5+IaC1k8L8HycpcyVsRyXsXX9U+Boeno"
yy$(030) = "3kTNZHsfOXdVvA9qC4eQk+kyBZHpPlhCRGOkGxSpGeKD8V2VWRb6LP8JTl570i2UFeQ67O67It+S1m"
yy$(031) = "PgxMt+WLu6eKjYcVGSm6OxJiCRBV7PJUQUjY37FLO0cIepV4olAP7PJRXbZlEpTjVAGPSjy+NCR8qJ"
yy$(032) = "jfCW6x95OB2STzi6yYll0cQB6GH3AiDv19HZki+ezjhL/NZj2SYDzktuDkyjhXWmE/QXAS2z7hSG2c"
yy$(033) = "4mNPeqSp+xQrVrIKtR2bt8ywvxlj8rHdBKHsUeD8PLD3ArtWjP/pzNSIwMd1zOTt8GfulkGMnM5ExS"
yy$(034) = "xcJfk2On7UwR6cveVlbJsjPpuR59lBV7ClQAzHQ/IhTJSQMJzwIRYrNkfSXFlkCJ1eYdI84c0ft8Xm"
yy$(035) = "ympWVKxSBbmOSqgIzzBp5sR+ETmw6bQumr8JebvjBc4dUIxI4amKTeddCYkdkr9IkFix6Uw+Ly9sCP"
yy$(036) = "U+8kY2zJiIR+REsdnH0tnPb8XeVWPm93jd6zZYdiEj/dJY8EnymtdKXLGSlI99gK6IYlgo8lzayXPF"
yy$(037) = "Yh/D7xXPfvP+VpVqFcbm871w8zMSsjcq8yFJ3az65+p5mV0+EpHbWJYM8apj2yMv3rVVRBkveUS1It"
yy$(038) = "kFNV9ETJG/iHb7pV1RscQtkbFrDVsO5zc4fDk7HxAfQ+JmGmI4AUM08p/H5v9gGfIiRrhiehiXEftP"
yy$(039) = "4dk7sGs9u4keeAixgiRdsSLpWubfCy/KLZbGi5WyUTFcCiELD+TeVuscOj9hX0RMijkuzm+tpzgZ22"
yy$(040) = "40Sk/6teujnIAY41hQ7qAkhsNDm/heD6W5sho83BtSN2POttlzFUTx5OrhEfo1KOb55ar/AF7k7YGM"
yy$(041) = "zJCUjed5seP+NfMxK0wQGseqYV1A4hqcRDNXLki5YleRuJdctUFqcDJLkuevla5UxaQisdhMDC9OqP"
yy$(042) = "n3txcjHQFhPna/qCKhgOQKo7N4VKIijRMeGqrIRFQs29tPItKjHxLzxYtlbK4UplfLYqOYNvxrFMmq"
yy$(043) = "is2E6dXySUZBFMNpaA0Us8T0gvnYl4mPWf4eNx/72AVk/pu+jkhumSCXzubwIUGE6x4mhJRFLo78hX"
yy$(044) = "9Hm37NQ/Zt/cBm3xfHy9ddUuywuvJuDydaImLILgZFAWFphngNh1VJJeG8+AXwq90yZLAg1wB1LCTk"
yy$(045) = "cHpylUUZYn7tkEYk+97wi01fQ4ZCCpcl/bGg/O04u3j/ZX/TFwKZnvWRwsPzPfvMbuDp0retczQWej"
yy$(046) = "4x8ryS2NJ8xlyFi9a6QMLGuj1iBtv8Ik7DYqR9KtZicV0pjcrSTha7VnmSo6I52NmECKt3rkhBffuS"
yy$(047) = "FTvU0zyxF633x7IOf2fywoAInownalwgNREtDTwfJkJPryujSHAYGFOPajwopeT8r6N6ghDXWoDEPV"
yy$(048) = "hRMYfkFTSOUcVEgh1Y8HmiWMGpBc5INV87+we2iGxS75IQ7bz4wnnBDvgZQj9ilZHta9duKYKEbg+k"
yy$(049) = "UmFPNl+ckWIfeyUh5YWIPle6RKuliokRhjpMKJm/OUg5+xTZNfjYnUBDJnKVhCdHUbEc2kfq9CqNly"
yy$(050) = "R3Tcz/ZyRcJWTXkoRGt4b0t0KDYqFJsRxRDP+mForJhXQBImWIbZ7ZBh8TEUWxYRkmAh0J5wnyLnmm"
yy$(051) = "ih3Y3x4++smB1Ltm2UVJ6FrXEvMff/Lqea11DEOskCBF9vF8VOYTpM3bHb42V4o5zEaJK/bhAAd8nP"
yy$(052) = "BDBitMFprDuLzMGVYm8WtsSzMdsRIkqd8REk6/jvhMMS/vesIJrBoRYkuGO0xc+PSV1S5srmwf60iN"
yy$(053) = "Iy0knhi9o8gV27ZoreTgykhE2BVed08z//txLuoFYi6eNO11ZC+QFLMU5HJ+j/vYARSP3o/sUUa6xq"
yy$(054) = "0hSUcxQ+JY/5NroU4E85MFT/GVkW8lCHGtmoi0JYQpNtlNFPvo2R/jZ3ohCRTLE8WSXCHuw4Tc/JfE"
yy$(055) = "6rPvVBXFRjLyyrnmiHAVSVRMRYS5khwAzvYfxElsPE12FET2MV6Aiz7Gg59WJclIm/sYnsZzomICwn"
yy$(056) = "wMysmW6GM4U84LBzZ9kUx8scPgxHVLMj+79ZUvIEKzS0BkxTKVKz9lLz5tVfi87xprlkcCstj8R9R8"
yy$(057) = "3rtdTTH28SmKWclEyRQzVuLTwNHMd7hiHdbqlxGkK5b4GMk0FiI+Cf7ZpFMN+h2oyLyWTHxZYdaj5p"
yy$(058) = "MVAk3Eg0yqYgnCr/M6qymWKzXos3JgbEtRjMR/+KeqhMxNiKCYjojZBVUsEysmaOXpiNl8fpk7jjEs"
yy$(059) = "u0gWUeTNyCCJZKxrLY/KRLHdIk4tmGKxaFqqYCV5Aih25+LzKjU/a6+ACL3uQIiwBHkzfiQphrhimS"
yy$(060) = "oVKhmhPLtIFFPMz7MXahyTsgsZ6XOET8lEsflW4slsomTF+G4c1tQDE30MGc0/0yaLFZDqAgSREppE"
yy$(061) = "E+8gY1hFGF9Ler5EMaHgZdlF61RHILvgignLibXsIk8lwrVkbXxUyqYqJl6v5Hn+cOsrPgmeadl4+P"
yy$(062) = "mejgTlPZ6SnWuIlF3Y6AwUgw5G9hZTTFyvomUXYP4+7b6KQUlAWHYR3RaQvjQqkzgmKTZKsguSr4Ji"
yy$(063) = "lrT03aAYv/iYfVn2wF5n89CRhpiCzIWEhCb5Q9bk76Qh0YbgY8V7L2wcsknPp0hssKVldNpcCYq996"
yy$(064) = "pPLqVh8x/vk4vjEJRkxaKIu2UR5IK+KxuVkJvhFyNVMWEvNGrl37++z2bJ9nNokQkia6kCOMwZmPyT"
yy$(065) = "JJP/PFmuY0JKSQYrJv8a8krq8yMYj1QkB2dlnu9Iy+81xegyYDF97Sm9iJSVw4sQTTGHJWM7+WSitK"
yy$(066) = "QEQ4sw3PzvSjOgWla/HiYttVIgemMyaaqKCdkF+bgHiNWX9J2uoa4UKr4U84WzT7vWtfYyxQSE9WC7"
yy$(067) = "so9xxehjM1eqKgcWkXZystY6UYwGJbcaeKsgQoJR1vOxaJYg8HHZ+AWb7VzvWzqS2JI3mL93JF4Y0h"
yy$(068) = "QzIYZuj3heuFMRxW5QxaRGlHb2Tea772qIeGXErJiGiJ5MFANHg/8yxOyMm4pw82+OkzaXMr6QYVRy"
yy$(069) = "xURkoCkmIlwxMjz3aGIm7UWzhdteEzJStBDpBEJnbDUEFCPZF3TG6BjN1FIRYotQTnLz5SX0mmImpC"
yy$(070) = "khmmIflOLOWJL2y6untQReNp8+TIiQwJsQ14Ao6y6Ir1dxPc4SV9NexryhanHzH6WZTxVLlgEnSD/u"
yy$(071) = "9pgVS3J+i2cUX6+1cqillkj8wIQEvsrNP9zuLFAslEclUwznY1pqkaaYb3sIstzb4QMvw3UznRfZfP"
yy$(072) = "K4TFUsWWysIDS1aBsUS3zMoitgEbtS+WSjhl/8meHA1FH5eZJWQWqxZIjRdGIvQeC1jrwRsgusFY78"
yy$(073) = "753ZxOYHno9f/6GOCOstsz0ax94bn/N4/t4LXbFZp1EXEbiQFCaIO9YVm+3zUjTL5koUPI0nTfWuLt"
yy$(074) = "X8nJYnLA1Kb4HkqWI4+JMXEMc8ubWoxbFYsbQ8AelxLFZMRGoyYvSxG0/isWmljsrEFmLvqWz+8WLz"
yy$(075) = "HWFUxo/sYoSs6hnFGSur8RcrhofYjlDywAMp91ux7EIalX1V4aa0q1vsvbEnY4k6MB7JQjvy3GB1O/"
yy$(076) = "c0ml0MeK8P27s94QqQmxridzsiEoV8MRieHPsdCUFxiiUjwsID75jU6XniYyxDMkXLl3yl/e2eLFfP"
yy$(077) = "bcSrrONWd1xXxkhLXHTRYyUVQby4fo/ryjpD7hxPaCU+gucyHZINaS9qTC7vxbGok5hPtoHc7Unmyn"
yy$(078) = "JHR+jHn8uKRY+jSIhjCIL/5oDcByH2FSWE+1hieDzxIXVTR6U4GEMRSW5SVEdl26fj0cUBf/epqwWx"
yy$(079) = "5MC2Y1ti8w+hutwjTRuUMyHJOlgnifmBs4kVO2dvVC8MCetgvV9nmj5Nw2A9J3+jqtjVL+vcfHr5jH"
yy$(080) = "naGMwHc6zEfKrYf/BVBGNYcuCKPSKGJGm/ug7WyqFGMWkqbsamC/fNKSsiLMgQvhHwtXYuOEzDZEsY"
yy$(081) = "T3wE2RQz2CFdPpJTEe7JFvYr17t8ANcr8eaX/+bX2u1/8fKGKDFfvCbeflQlhyUW71Sx2a6EQMuC57"
yy$(082) = "FkCTzS+mMRz/pI1oqsPov8jaPSCCmbEpOzLGm/E/vM6MxVGQNCvIshRfwJTRPyJvqCIWwY2h5VKVNE"
yy$(083) = "GcczIThV6OpDjFSU+Fm90V65Z0QtwMdqVRUrJuQwPM+nj7F6WUQ3v6CYDw9ii9oefH2Zggw5orY7ps"
yy$(084) = "rdgrxB7/A3qwi5Jmoyv8aLyoai2Jwvak1Fmopicz5XxpURbVwUyIypisxWqsRBSXaY7+qZFUe4YqqP"
yy$(085) = "rYDg1Eu46Eaa29q9zErkT0YWPJ/rZThXbD80IW1D5Y4W+xjNZs2KTUdxdrGL7f2OXFObbXl9GYsct6"
yy$(086) = "k3hcbFIvMx8rdTZEFz7OYZ8nzfI4e+RLH3X/XcJ2yivEEC1I0nKqKshnIj1riAiVKYXsXpT1kN5f7l"
yy$(087) = "U5Ln26WPSGqBJ00NUXwMsWWc2OrJC/KCStzQEL60yeJI3IMdkbdplzgFBI3ug2I4r2ADc6RaH1fiPB"
yy$(088) = "2F+sjQ5tL6Y1ixBFFSODoeRUTNLpJ1UPRhiUNYPDAeYQq2R6zm2cK7KqAjTCiOpF4Y4ogzfw162g16"
yy$(089) = "U6qYhygIN99tnU4l86nS0mjWvougoCqmbSyDFdZcFZ7wS+HQ8M9pCD2wF+IaxbJo/o8yKQgfYrCJly"
yy$(090) = "mdZpr5ElIk6QS58RnZ+k70/hipE0k4ugWNCIP5xtXpFCGr2U/SFJtVRCRf2iUJf0O9SVw8sKl0Jxe7"
yy$(091) = "YnvhdM7TkZF2azkgZeW2eglR7kbHmfEBQqd+zUTEk8VFRbAF0SZPJdSiHtnYnfXynVxsQftIdxbYWA"
yy$(092) = "b7WELQTrFkIfNhJeloXbIF3cCRfLQQGTuy+VjnctXgKwKirFFEcKON2hZTkJl6CzOqibeuyRvLYEMV"
yy$(093) = "cbVvuuDbW3wBl/Fq9SrIxZt1kfnGaG2kIn3byZKNZrB1XlavsNHeRXe+BkKzi0i9TX4p8np95C32Mu"
yy$(094) = "uq3xIgb7gCGiU/UcVqo4XmY2SQ/KRdSzJtA8P9lQgtPPuG1VBRsLEEMVwTX+zJi/tjxm3cuyn8tNIX"
yy$(095) = "1o17i7IL47ZspYq+Weubn10fgWLTNyOptrSkJQErKdYyXOG9v1QxAckLyDrmv/m0vgzRisT5QgQUUy"
yy$(096) = "+jz0l/aKH5reQndkfqQvNt+eIIVexbC5GcXCvRoPThQlsKcqlIg9LhUsWEDsZqinm+rbTU5ltLFWsm"
yy$(097) = "g4wq5i73scQYqtj7i5FGvlRKkLg5vGxUakubhtoXpKiIdgeE5S9CcP7fEH6giqFBZZEt7XxtoKy1nt"
yy$(098) = "eyC81H+aqAsBV39YWI+wMxlWVT0sKzj8qdojBdUB9bgpDCR0ZmiwcyRZoSsszHOEa2+D7xhSKTjZ//"
yy$(099) = "Wysj8pURthJyrXxsqCTwKyDRQk9OQR4vtYX7Mov8V8vN52OMzZWT5QivGdiBra/YCy2B1zeez1NkYP"
yy$(100) = "iGkMUIXda3VgY7d9dBaD5WWQehPlYI1z77wQrmyyuHo9HaihmqpAUb9bH5aLnD8I36WHcFH+Mbi2Pr"
yy$(101) = "J/DdtYfYbH2Ezq6rBqX4Cu/ygfwWcUw+sLeIY9OMvS7y21KMb8W3RGZrIfTAaDCbG5ZPGB7n0dob3c"
yy$(102) = "HFdG2EFe/KVaGUB3WYjcUJiUnkSmYNhJ39WyDzemX12sjs00plTWT+cffxuubXuhurI1Tk0nRdxaL7"
yy$(103) = "03XNjwISydZCLomPvUVCslbkXwv5XfbHfpfIWj7WXVuxt4hjs431bTk0If5CZFBXEPgelSz8/YJUZJ"
yy$(104) = "pVRiVoAkMoGuLPUxDWg1UnviiuHQzIrVRkSkonA8IypVuKLfinCTmyMPy7kRF5opqPj2lMkaBgROhX"
yy$(105) = "3Cl7CeZwtsJBClJRz34EX8r1Idjy8K5qS8pcCcg0fP4alymRryA0jmmeDHuetSIrGp5oe0kZL+Cqs/"
yy$(106) = "1pFtuSNdoSXahVUhdPILP9DkaCihk5VnMYSOumH6NjuC5nROYb+tm/iqbzjVN89oVv7KEbVezWNzQk"
yy$(107) = "xBPV40o0nO76CkI9ZbtuQqD/O5xuqHthc2VFs+U4GnTxKU4zfxqpSDc6fXMISVcYPDYis+6XFXUvp9"
yy$(108) = "PClziMpCHYYbRRWRnU63gvw2lBtYUqtqV+gy7+FL97OgVE+BYCurEMtqEhlfv42LAnP9bOS9y1VhwG"
yy$(109) = "72X0HJDwwnz28fBWfWxWnxQIMkzxsVdfqp6Miw3rAhRL82R9vOAdIFjsOBzsqrZod3IlyBS+VnhYMY"
yy$(110) = "/K6F80kSf4xzYgpylx7FDtXGHfj2Z7d4bpilnqEOvioTW7Z2HkMsXHbO28YGS6jwv0cFgwIvOtP1EV"
yy$(111) = "A+T1UxxhzrOqLczHuibkfI6R8ke+glAf2zYhs0b9GiPaBdiU/hhMEvNmHWLya6Mten+sC8h+FiL/sR"
yy$(112) = "HRazHYy+xjiBrhqRHRK77oFCN38QelzZUpSLtXAcRXEKqYVooSpIUK6XMl3VTF9t0gCl+m+JiGgGLY"
yy$(113) = "Y3axw6SE8aiuKgZ32/4rPvthkIJsdVXzwbanMItNf6zaEvuYaj58xKD+Oj2ObWt7IQiexfFkoe6F+d"
yy$(114) = "i2yZZfEIcx2zLVkC4gT8FhUhSbfWpUDK6UhxcpPrZ112TLBBzmv1NGZWnHhExtjJy0fQVJ6V3QvbjY"
yy$(115) = "x07kO7/4gWmNCGrLvBCFz+4ZbYku1dtLqY/BrH8xNSO6J5Pvi+mCyIeLEGW8RHDVezhImSuNQyyaEI"
yy$(116) = "f5sa8gaYrBgUFenzYqzREmwoJhH7tvtGUWlFUfAwRWyYRB2sSnVq9kL1NsYPjLlOk1GqmKQe9zcgwZ"
yy$(117) = "bFrO/8SIFKLUDDaaFUzIFJCTtroXlvPX1bNPbgKHUZk27wcqAnMlyevDYMOIGHJ+0vodRukZbEX9Xu"
yy$(118) = "uIVPOXmPrVtWoL61q3VB+bsr0My8J3TtItrnVUH5syHYcPtb0w81WHoQj+Zeqo1IZYFzqGBDm/uSJC"
yy$(119) = "3j7dSD8vuKxWzSd7AVteX6q2sMhvRC6zcMEwZa7UIz+UZ1kLxr4WLdMiP8S+UpA+xAyRH75qwG1j5L"
yy$(120) = "JgRGaf6nvBIeZhC3wsxS23tYmPOMsdCEra35lhit3VEOzCgYuRSYpiJb0WG8PfN8LI/VfqXtLiGMl6"
yy$(121) = "f5U+xKYa0gW/nP1VelkdvYzX8yd7wWF/nuumjkq2SbaAX2Jk+CYljhkQOP2DbuqoNCIbRLXhyUfqXl"
yy$(122) = "IrcZgurtYalfhEMuSTFZGIHNXVgmhpsgWfy6+M4yVdsRk5KcM3r3wFSVcMarcF3R6TLfMNuIF4ydkX"
yy$(123) = "kD1chBzDd6q0huZ7RnRk0fb7HuzvFfu9Yv8/Fftf/h3eFf+mcE9AIghcyx/PorW3+f8Ae"
yy$(124) = "fkK9g==:0FD1"
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "
  	  goto label_info
map06:
      if mapnbr$ <> "6" then goto map07
yy$(005) = " ~DGR:SSGFX000.GRF,32084,52,:Z64:eJztXU1sG1d+fyNyyQFKiEx76BidFUfdAu2Rag5lYCWk4G3"
yy$(006) = "aw6K97q1UdOiVRFBERuTVRDKkHFwpxz2k8fbSc485GNbEDMzLIrr2sLCo5SLsKaKgNqYhmtP3f1/zPv"
yy$(007) = "lh7LY97IMsSyR/fu//m//3ezNO06XHSP7l6untIl8j6ZdX3VJtkS95nglabNQkzPQNMC/+l+ZZVJ62x"
yy$(008) = "Nt4Qcxvi7dkCQznrb/8PPnWEhgmT6E+B2Ph7f1wDsbCW1RaYm1MntoP4sUxlLcCKi6BmTJMbgkMlac9"
yy$(009) = "F2PwVmqjcoSJmIExeMOklX8xG2PTNy9E3tcLYghvMEGIckdLzoPmKILBW4D/bAJ77mHw5uM/b+PXYzf"
yy$(010) = "G4C2P/+ALNEu37X6nsuPS00LQdPi3+gcuTPVB35wH5EFltObA5EeJyRuZoOjXXMIkNjtt4T/95nZsh3"
yy$(011) = "hxYLPTBL5t/8KFsdpPfWsVRWXX4j5rWXkbxCiKXRdoP7FeH5AI/dSBCexxAU9Rib5xYJA9LoRtjHnux"
yy$(012) = "tjiQgnLH+XcGLa214q+rWKRNhyAOte3SfsjWd/ydaxUiR2zy/Vt1NpX9RrjHJhHHHPdraZKnCvVVmMr"
yy$(013) = "xHvEebvuranxtLTRiyMr5mGF8XbT0+bxo+NPrRj0iPM2rDYkeTyELmPv5nMrZo/L8+uwIfHmoWAboZs"
yy$(014) = "T+HlHx9S5vo3ChsIbOKxWEX4a6JgKX9t4ryHLs5Hgb/naMEa5f7csj/L2Kn1P5q1NMb0QFX9uYcKZh/"
yy$(015) = "gr0WGIVj9vmRhrXIhBpOLzem+y9+2e+vkg5bxdPJDtlGDql0f14S/rg19pk9Q5ZlpR9Q2P9ctymKK3e"
yy$(016) = "v/hWtsIKbxhub0EbfQ23z482dU+H3PeRgeKvnFM+GF3VQsqG884b9cGb1h/7g6P0+NCS8Xc6Qp9U3iD"
yy$(017) = "UcBOe2/4RX1HC5Ll+30F80LFrO7dnNS3j1VMIeD6dqPO0yTf8w9uflLZysdIH4y3rsJbjV6jbtjulWr"
yy$(018) = "PDAzjDSm8KRgz6x7Zrg8ivKHjzejIj1QSKljZmR7UFXkK9Ht4eRRsH6nJQiBi40X6MjXqhUJAMIH6qh"
yy$(019) = "aDs5VvNglmKx9u5SuGPAxzperB3yMUIw+XR5iDHn/xXilC8Drj7XV3qvi3lQBHSB3Txakd5rPJeBvrv"
yy$(020) = "Pkwz699mTfMPlyDA+F3TPvBGQwQAF/01yLJt4pC3wabDZM3hqnvxOTXEsQYSd8GadWcx6th0rbyNZbS"
yy$(021) = "NjXebghGkQdzMCQcdEt98gr+vhFLvF2nelwATC/EmA1CFyIJEE7qsP1yO/3OmAePw0+jA3/9wCcYnDv"
yy$(022) = "irEHKQ6y84eiAOTgOtkmAaEMqGEvyTNC+zhtWmYjq2/YJo4CEchFPp0j3B3Rg3rwKTn/oIOq+lzj0jc"
yy$(023) = "oTA2mYg7djmtaVYLIu4+0Mz6HzhsdzxtuHCU3QQsDEjLdpavg38Itf+9Eh8LbeJx+lkwsOpoHG2wroI"
yy$(024) = "64AqNltElHg0mRxbtzXecsdAcY7oRhs5Pjy3CEYHk/HbWIL0jwedQmrla9+UNkq5tMRxvyQvDRmGEyA"
yy$(025) = "xV9juXOYNPyFQsgay+T1X7ZcvBGiiKkSfSuJy+M9nBFP++Q7UTklnX1kj6fSoBwg7zOs2E3iYD9tCZ8"
yy$(026) = "4vND0read4MyvnmGqlOhNkb+hnD5PLTfFlz36qlDZekheqNLXG7HAIF2esh91SwiaQKs4ZcQzZkGSxY"
yy$(027) = "XY13nDbqaHMSHmjWCCDMPslDh5vR9S8KPcl9FKiM0zIjkDXzbBDNOOhbf89tEud/McU/C4/QynGzpvM"
yy$(028) = "C5PKG84FvDGyqrADKYb+jxYebxL7J0Lle18pSleDpHg+llNlwcUbnOD6E6ti6IClyXjDUW2PhLB9AgG"
yy$(029) = "SRgWFzxD32Cs+aiwjkPDgYphuvOOqW/4Y7mTYOcoxBy0BCiThw6VtxaW6RsMqBMMH6v1/gy9jogTbP5"
yy$(030) = "kd0vmrfia61t6bsqDvcxqrfuoTXiDtI3S2eO8jYvvGbz5UGmwXqUvMAi58xC6+Ojgb71nwJvSX+N5vO"
yy$(031) = "bfvCJwkCvguPBTwkHLwAy/aGj+LSYY8ImUt5qJOa3qvFWAtzwoG+EtMDA351VVngB7X8JByGiQ+wmOO"
yy$(032) = "isgDagM05YxjLejqsEbYKKDIMK8rZRIm0Rd2xjt6/JArPkBySlIWuEbmClS87fdAcF4HHMpyvxCIHoB"
yy$(033) = "FVXfajTHqeQrnXytWURfxdzrAIf2fi9bSkA5uN8tfZLwt7qirlfjQosGAhQSTIjV1JMxPC6cyfP02d8"
yy$(034) = "h8Panazg0kHiqcnA8lnnzSO8gKuzeYH07egfclYBkHOSUeJojTv1HR5UdwhtgDgwMnqAhzZN7BN83iL"
yy$(035) = "5t5OseVrlDjsnztU12yzJvJV5blfpcd34khOG8PdlVeCu1xQ8csyNjCG+TPeX6+Bzj1zBvK9hO/Q8k2"
yy$(036) = "tjapg8UPSAlQkRyCuqrLByMp/uKfyPJV0vGHJmYyb48D60QYJ4C5g3sdOueyRtS/E5AmkgYU/rLQ85B"
yy$(037) = "bPCGlDrLr/0d422jG9J0eVvGjCzzoL/gP6w/CzBp+Etp4ck1uuAtH2BZQZ6jXVYy/Njg+mWq1VnB1ir"
yy$(038) = "EAm6ngdopdMSFdcq1h5r5AHhDBmZK0jG5L4ZLqohxQHhT/LWt/1aqQFAIZN48GWPN34gegEvwDkpRDL"
yy$(039) = "xZ4oIqT26VsbfzkJdaRyZG7YfkSiTpPaoPQN+CAUTiubyRXRZIRMrJX+fDnfPNG1wt2jGZfyv7eDL89"
yy$(040) = "9uYgPOwg4snuSVp4y2PK4UIylKShBDesiqV8zatKf2DEvqAFggoOi5Q3qKu5OS5Lcj+Da3c/6QQEBru"
yy$(041) = "DChvMdrTMGOk1gsFfDkoJuCYzZbOQbIp+zfEgxr2bIdM394zMNepJE9RLJ1KT76fS0GY1gs3V5Kd5gK"
yy$(042) = "4pBCDSVwgmPax1LxjfUu5DvZ+zt/EjH2DiJ1GJSlBYPnoJJNnhYYSWCCWfsg48Ns6pl+ReUsyTH3wS8"
yy$(043) = "a1gVHidtLkb3qo7r2oUDv9Tx0zOZT0rY84ByD925Q3T0oqqL7hpDzjrQUuh1RVUCyAgmKMvCdks1OCI"
yy$(044) = "TL96Jk/+s4npZaEmVlneRfBztNwC3PwGSrpmBeB2UdqkbiwQ0vUGBV0zMSXeGsxSaEYAdJGxGaNeUar"
yy$(045) = "kr4lpF9E7HTULTH1keWx5G8JybuK+HupQzCd3qOanCha+uQxXBqy/hIJCn9wOxnXkKFvwwvJfoIO+e7"
yy$(046) = "5aP0ogLTii5/+02dy4mvJe/0PKeYIdBQwmL3PZEdKMZcn1WyePNnLqqEe2qLBNF+5NzIxowOJt0MQt5"
yy$(047) = "2gqJsX5xRa8p4qr7OkeuEQzGA3hvqUAnB0QDlpnhHTNEnfYn75cFlKjfSZj8z6NFXiKabII37EoxwQ3"
yy$(048) = "THkoeNFhiFaIGGQyfXLVPGjiPVlIHN7iHmDvLmpY25k/wbylOgPgjckD8pbZ5jpmxdjHQ0g50kEbyqG"
yy$(049) = "8ibnIR7iuiXzJg+6tljyOyvIe1Rpcd62OW+6PC/jSsYbdMhrLY03E5MOJH+QJ8tTebPkIay3LuKpR1P"
yy$(050) = "jJOOt5hm8yfGUEqDxpmBcfSRf4U3d7uY9IbseUAIeBjvPg1jHyH1LWrmVVEz8loHpS9cnL81DeSt+fm"
yy$(051) = "3yNjX3S2Te7p7XW/LrlLeb1M4bDQqYNwXD46mdNxgQSGz69lLSA1/l4OTByy9Ad1oa5vZhkPEmYXInL"
yy$(052) = "IWzYMZI71tSZA5nbl8j9GfQDxlpGDmPz0fiva8wAQEkFNjmPGnHktYLnZ7wb6VI8NaleQiuTwMl/hj6"
yy$(053) = "ls/WdliH+vTAT6BiMuJcKvEWZxzwpBfqgJyOucj8m5dhAhkjOVI2z342T7Y2yNy2ico1kTzkXqemb1G"
yy$(054) = "X9TZGXfi1pfL2dBQjrY+ES4YOx7Q1DOuH/MbSt+xDZSp4k07aMH2bdDTefLSt8oYqGmY47VxwfYuhCN"
yy$(055) = "1+BGuZiRlMazJvq5Wtf4ZryHj7MeFNx1x/J8lTDNef+dEJqU8kDrIDFoy3ZzXBW7sXQlXlyxhfwfD9r"
yy$(056) = "B0xz/2Ev6fylikc8293Xgp5HlaInsLyFQ50DNUdwRspxAVmh2KKOkbSazaijLevCW9Z6Wivs+goKPqG"
yy$(057) = "8+FI5k2yUxkTylyjX/Ncntb1bbNvif/NPOMtop2N4tlIWhvGZP5a4kDE0yZ55+ZTCXNL91wFbyWGycm"
yy$(058) = "8YRr9GbwxP5L3CG/37HZq4S2Bq8h7kCyBrUm8ndn3ZQqI95FC2u+tybwhk7cElkhII9lvzLjMeEMqb8"
yy$(059) = "JnQv8t+EfS4dDlUerTDONTzI6++0w5ON+08oYIb++T1HcJ3ngeQnbtmxJvtnOqieA6FHV9U+LNNk9Ml"
yy$(060) = "nigqpyyNrWPpPLW4Rx4CkbKQzyVt0uupigLJ9zHN6y8oUoLq6adt2ulj8Q5yJPkoLtZI9tTSJRNzE4d"
yy$(061) = "+uaJ+jSRegSuPITzVrg//JTpm3BWRh7iZfLAclgOe1RBWXVGMQ8zO9UwrB8C26kVBSPnIVLiCbzdY6W"
yy$(062) = "WvGxb31KSR+hbIr1l5CHkH6KXtqBgsmLTqW90rBRY6htLGMPveApG8GbmFBepwhsHSLxZ8hDXPBlvks"
yy$(063) = "IxHe2WZXkYBzENj3becH2KTN7wRyMrhu9n6fu0HhwdDFuI8/ZJJg63hb2OTZ6tQsYB0jFSHqJwkBeYP"
yy$(064) = "zQxaV2bh0M5b6ad3qR1k7cISbzJp5JYP+R53eQNMEUbhvO2LubZjTlv+K+VktA3fW0TNBDyHAVb2eJ9"
yy$(065) = "2VepmOkdoW94DE7E24I3EyPpGxlhn/1QGZzjSTFv5r6ZZD/Einsh2wPOga+CLVSj33smxVPqd/2NHsF"
yy$(066) = "ARAg7GsaZhyCfzEOabz0/OpTeoX6nre0DAnH3YkTPOWwV3r08utPXMd9JdRbHyLxdylvPDt4EphIDae"
yy$(067) = "irfMWwU4m3rDYiHPjCXWm8KefwdcwhxczpW0ojF0P2e4jZM9am7ctQeQhvCXJw8Errk0sY5MIY9anAI"
yy$(068) = "KJsFWXLnsWFWk7ogVpae8JUpRcpb7tD5Rw+HfAv5xhmQ8aw/tt+ap8HIcabfByS2VzDwRviFWdoYF68"
yy$(069) = "m9p5Exgj5zPmUQYmzSsY/m061eRJhDCcN/kfofG0lltTeBMYSIvI/oKMMfJegmlK8yDgrSVjyNpe5y3"
yy$(070) = "n34SzBw4SA5N+s6nypmOybc0Mo9upgsF2eg+5ME7eera+pXbfh8IbbGmpZ0jn5SEk9T1Sz4Ma8kS6PK"
yy$(071) = "A7fSeG8NZaDmPnDRtpAVkwt1reG0uf0Db0kNiXUfPeLB/Kk9OqXRVj502eRysWxNqkvmWkyONTDmyYc"
yy$(072) = "ebfWsrbgRvTt5xD4UNLQgRGrxdAHtKIY6mviuF9S7VeIJhOQjA9v23lzVIv4I+vsJ/svEn1QhNRi8aY"
yy$(073) = "4JLucxesHEzqgjfAcF3xTsin7Ripzmoqbwd0RjdGlwdBnRThz1h5u1brBaJvxJ5XSu3YwNj1bSX7QLT"
yy$(074) = "i4E2yHyLPbp+9XYGbD5TAyDEv9fPktZ2HAlPZObFh0lSt6yUjxX9y39j6IVJeRWvdBHnSHsnGuSoPjQ"
yy$(075) = "ttcf8Cx6CuuJClDc22aVy4P+Tz0EUpa1GTXjTD71TEd5wJJyYG85YqvEUyZt2KMeZR1ublK7ETA/JQD"
yy$(076) = "Ui4mwcwtlMVo8cFDQPfS3Y71edpyvNg3txrE/Ko93Pt2DlQz4cg1advw1loJGVC1nnUsNZkvAc2zATx"
yy$(077) = "bWAkcZCTFIoOva4nGDQbY+m/rdJD64h9X1HMVqztNRJ5iA93CymfyBxVU5FnvLfPeINb9wYLYYZkL5T"
yy$(078) = "OQ60hK22zoBApGOXcBjk06HF5oDXM5OEXyLx/gaZ3CqagYlh9+i2fp8y2/ARGiq3qOfyRiHPi+BXp8W"
yy$(079) = "LvDpkooaGkcTBB/H5ALnEBXTJMjWEK4tgii9uB4Q8Ic4WsN+9pvSdJ36SBvTtO+aGr0dTemHF//XPAt"
yy$(080) = "BmG7XNlvOl2KohaQdy56eeRpL5l5ZJzgMf2qtYJkTETcT6kco9u+dEbAk4yjNFHGmXnQzyyO89iaL6+"
yy$(081) = "pQdTgbn+httPgjbksvIcukFEX6XtUP18SEKO34vREx0kCcPyEHE+JFGpyxlBQaxNOR8it29Jx5KI9IG"
yy$(082) = "BUc6HwEwCc1yjmB0do/V7C+ISVrazLRwrBuSJAVF5n7/d3RwlytlrGOw8+SH3bwSDciFT4u5q+7mBYf"
yy$(083) = "rW0/Saa9fBKvo34157i78mLfGE/dw5uqOcWRcYNQ+R2+hQ0fetGHUeOcutXOYrv0E2zKSt3g+IROOZ3"
yy$(084) = "PehY1geot4PKDD0Fi0d4+ojRfSvE3LYEqFZ52oKSHbqaGV3W0/eBOac86ZjxM0LRi9tLM6HcG/GRg7x"
yy$(085) = "TohR108+NPxbk3IgNrOM8yF6nxzxkiYSGFm9R2xROm9sFWRfhkhmcCD2s3zOQUzeFZun6r9EMEd8f6H"
yy$(086) = "EMPRyeLMw4v5TH/EoACOnd3plzK3BGxkleRMwG+x+Z95HUk2SdDlNjKZvkfKmT5u91rUp+6eSzV3O4C"
yy$(087) = "DrW0JJtRgms9M/Zj0mwrUP52ryovdrYM5Sll4xVRth0KpxSpcOFhf4OdXGkJ6KysPt5QNjZ0HiLTsPW"
yy$(088) = "/hzupKV9QNE7pTx100MXVtvj/nrQo3eUpR7uNnMSTsLBRNzlPV7++LuwVUJY+5NyefFRmQeHFU7JdKx"
yy$(089) = "XHVhEmE/iagRSu2u1HlrWnh7KuLpWwnDdOidMmRnQXZVel0Pp8vgbdhWoCd72W6EjGHXlD/PgbsK6eS"
yy$(090) = "b+VQQihm8TjP/pmEMCMNccn0roCgiGL4ds+XEZOe4ivTQNL+9SLTHE5M3cY7rWTiiGH7YMjIxlLfxCr"
yy$(091) = "8+7GAyu20hOig16eeaxtr6Un5AntizLk6GJPKnZcy30v4C6X5kp0mcHAg7JetCSJxQNfJXHcP8G7vLi"
yy$(092) = "t4JqHZgZd5upfNv6LDEMBBMTQ/ieH4I82zYTvu250awPD6U5AEl3qZ3Otuf3MPzXvn5IRnmZAZmFDSk"
yy$(093) = "eXBkANKKYdvU6Qxzrd7XVvPISfJv6R3ixmD9N+2+tkNyp/NQPS4ohp73Mszn5J4FK8Ke90IfCHOwakY"
yy$(094) = "4GTNQnh+C6IkaO2kCMzLODUahzUJljPG8AFI1JA6Mnvfy8VYSKv5JHkzfjPuqZw13/3ouZj9N9f2FeZ"
yy$(095) = "iLV0vPM61e6rzNGjR/q/Zt57hcg+Yhjf9aZh6aU6TSfR8LYq7p5tQs3sy8N7U9z0EZxj2ek1R/noMx5"
yy$(096) = "GBPedvXn+egjmLYMfbrJ+98uzRvUzSj74+IBzf30RGyPD9kDib23kCvFX9tGzLXfF/G1g9xjTn7WXPX"
yy$(097) = "5pSnYql/2HDyVkGWe5fn+wMT46gX+GiqvzLeZj+nsanWTIvyZtZZvdnyRDZ5jub6NxNDbkdf0r/tz5Y"
yy$(098) = "nVtvTrD59PJO3RMWwfu/ZvHmMtb06ri4tz/Xxu/N4M+77eEVC4+x5dMykM18e/XwI9m/z9M3Tz+FPa7"
yy$(099) = "OvD59LXlv6cL4efJWBeD9knp2amN7kYo4/kDN5ivnXZ9WZ8oRRzjiHj5KZcaFQaq3o5wbxombPA1ann"
yy$(100) = "Rucrln61/Jo4YIgUDEv1xqz9Q16iiUVM63O1zfJy1NMozFHD2LZ81DeGo+Nc6ozBuWtXnw8T9+kweNp"
yy$(101) = "eVk7/Z5ilsvf6DVdLp7OiQt0NPkP/PzbAvE04T/wuLAAbwLD4tx4Ad7E2himvjxv6RvwNsqdLcvb03Z"
yy$(102) = "3AX0TGGangwWuj7q2abmzQFxQ95m+L1/PzN+s83jXC8yjYfrdZeSh+jbKLZ+/TR393hlrS9P20nrw8v"
yy$(103) = "G4uDRv1fR0ad7uXi3jd6i+3b1a3r81FvAHWduOxQXtvOXsQXl7A//2Jn50fpxTBosL5v0LM8bivOlrq"
yy$(104) = "y7P2/fVB8vbaSNd3k6Z017mefhnN+S5BIt+9ckEh31ipys+b1fO/KK2XUwaS/vr8hvoW7W8tD+YNk6X"
yy$(105) = "ttMnD8onS+vbZuN0aX0LG8dL81bafIM8ZHdp3tJPxsv4axoXrm+W7iOx8bvvv13w/YXf7Ty2507OGP+"
yy$(106) = "X///CQphpdXnepuU3mMdbnrezvh4X+F6BuRGKxHmksuYPwDTIRtL446svrWvDkmhxDkzjmGDIXQduzA"
yy$(107) = "sVUy5TzMiOmeh5bwPIhHxm/LO9XTtmVD5T5cGYyV14iM1kt7WpY1h9qvOGy9UvPz4kRyBiAzOyX58q/"
yy$(108) = "E8tMbx2/9jOgakHDaAaF63741cXDg4eq+cGAfO6exF8NG2MX/+LA2PodQMuT9hIGuNd5OAt1e+jxJhy"
yy$(109) = "ehoi2C6383am78sAb2u3p37l6sn9np23NBdXdd5wvlmOa3BHSF3H0LUVTT3AH652R1h3dtt2TDkw9G3"
yy$(110) = "80cUPh910PEV23qbV6qk+zxhWhzE/8xzXdP/0VOftMh1V0wHmbWCYL42nH5+e6LwNnraGZ8dY326e6B"
yy$(111) = "iev53qvA2m3hBfo8mua23V9FiXpzsqX6ZVLJaDg3RVr08b6XG/2nkJmLqDa6OuJ5hrmOf+xKFvxnkXL"
yy$(112) = "OAkd4PXPOmYbo/5N30/CyvbE+/mrIh5c+mb8ZxgrGxpv4aZwStz6JtpP5j+SZD20vEkcHBg9N8AMw0B"
yy$(113) = "88rl37p6/wAwafjRZTruuPwoSjT7acCdVCcRJu2+gzez/7Y2Tp/enqLa1ZNdU0epvgViv56N6phYFdq"
yy$(114) = "f7PbtvE3XqjpvsLY/qcb743HTzpvZf2v8CmOCc+yrhid2zJRi5HngwZpBcbo/7pw61mb03xog50kJjg"
yy$(115) = "OdOOJCQ++Tr8GJkVP0MeYtcMQFo56rQhPiFHu9ye7YYadG3dgATNgvY5t77dAdoz5tQNAOt49nxNNT/"
yy$(116) = "f4FgvncQ/uYP4e+0SHzBms7X6mnk49d+vbx1RcW3q6q6B+etFOnf3us8QZru632q5PdsoO3tYbur6EZ"
yy$(117) = "NX48aoynvp0DrG/nGm+wuzP+GxxPU/O/HOL9Xn0eaBKNcpCHOOLptFHV7RTWdoMgDyk7eLt7qtsp8DZ"
yy$(118) = "Bd6+edJy8NXTeBuT3MtwS4uLtXV2eHsEUcWxsOew0X9H1jeyKHeawT3TY9tSMC8DbNIfScftrh52iA5"
yy$(119) = "23a3IJ+jgunDvstGPkIcDbd+9dY96uQh3jqBeIvk2rt3iebxeOC4TrIjw61qE76Yn03BUJk7vYd+chJ"
yy$(120) = "m8kIccpp9NOTb9DeIOE1J33Rgc23q4QPNrXzPkYb1Obf5v+EeS9hy4fMtrX5IFYMgF5nHnvBdJ5Ax9P"
yy$(121) = "MOljB2aa068PeeAy/pfG9x851jZSnnvM1wYYd9470vcX1sbwegz1wsrC9QJZG55s4vS9RUPfSKvs9Yy"
yy$(122) = "a6ftVQ99eYMzr/8aY8edOO7XlIaA7mDeHnUaGnRLFAH1bwk7TU/z6KTky7dI3g7cUFpsj12dhOyWHWT"
yy$(123) = "6Bx0I78reXj424APO8SqAEeOCqF4y4AAn3qI8xe6HDThtlnTc4XtCv7bv17WnDiAu33ae3wwDzdj9x6"
yy$(124) = "JsRf6AkSQchnmfflb9VCQeKPNi4r78GDrYc8bRq8rabprsDnPtPf+jgbS00+gfg2HYwZq+6sH+DnGH6"
yy$(125) = "zhnmzdQdlvfq9y+s4aBwexU8nlHXm/kb8XjoFPtR83+ic9fbWMLLU7ed0lt9tX5I7+I1yfm+WKKuxxF"
yy$(126) = "1jHnrOOzUtJ8Gae9gp+O2U6P/hmkckbzuiQXjtFO4qFOY5/kSvMGROKwOY1c8NXj7q4sHsFHReDCeVG"
yy$(127) = "ZitH5VmTjeJfJe/Osh4W3flffaeDu7xPU+ttOhPX8z54HHQLbIDQ+u+xdMeWYOB28LY37b59/k8f++3"
yy$(128) = "/t73n7P2+95U3l7tsxeKBtv8H+QLzpG/wNAhvfK:0B63"
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "
  	  goto label_info
map07:
      if mapnbr$ <> "7" then goto map08
yy$(005) = " ~DGR:SSGFX000.GRF,33443,53,:Z64:eJztXc9vG8t9n9WyJBEQ1Bq57EsYcdUUaA8FSsGH8sF6Jt"
yy$(006) = "UHNNf+CRRU5B1L4h1s18/lRhLkHAwpRx8Mu8cee0yBB2dlPog9FFFzy6GoqDCIbxYFoRAdUdzOd37t"
yy$(007) = "/OQPISmKNmOZpkl+ODOf/f6emVWaLt/Gd8CooFGvVFvk55c3F2/FD9otRov8/KvcE1qwhTIovgPo+i"
yy$(008) = "6gO/W0+JwU9hZsxd8Ve/VlQJy9zbv0lF8KxF7rzQWZ7Hl780BW9uJlhkc/nEOjJUCUPS9eCiRkr70M"
yy$(009) = "iLxSmX91Dfb8IvI2YWIzmsGej/9+B6HS0nPyY7S2KOhaAu0s3RNm3V8YxOeEBnPmZLAHD21Uac0A2T"
yy$(010) = "W3joLBsnNC+dJ27ACU6wOH3cvXzxMHaPWrgcvutX3n8Joti+yF8LBfqrlAiU1zscCi+/lS4ASFNvY+"
yy$(011) = "jVFrJefsKa5Z5uTlQ0yd99wFeizmdCazV2/hh9XYAera2SMiVEhcXfGLO72RJaKAhamEZoEsXsOvzA"
yy$(012) = "FZZa+IVtwIlMleV5aIrf3Az4cOxGbCQOPNz2TZ2zqof77iMklC9sbtqip7WK3+1gH6N36dxp0jRZ+g"
yy$(013) = "feMAnYSMvZ+e91Wf66GNE4fGn3DZuxn2tZ7iaFp0gCI2vKvnypxaqL6NPjtO6HOtCSIuj6oye3iqW2"
yy$(014) = "jj+AU8N5zBJgeNyip7HnlGJmXoVcAv7mRzVZGIdgwa6q9iYX9hmRZl7+snqcxeBTQQa2gfob4NZJU9"
yy$(015) = "IkFxiOIBer9r68miT7TFX+TRo/7jq0QDdF12D7eVsFcufnlQ/1IDlTJjedrQe/Ir3k8ql7lKVwNlw5"
yy$(016) = "sglT38N/DRqBfX+/iyKC2IOXs/Rasye3CBAjCz6HG/ol3d2jvO3lhlD1HQ40Z1vPsdzQB+8o4P79LC"
yy$(017) = "HkJrz77493Dnlfri6hNOxJXJHm7lJ79O0LZ2dfOCPXtPhckvItQsI6Mx9p5eyHMCIvBUvt9/2YpL92"
yy$(018) = "MTRGVPZY8R0fKTVlLaGBggxp56nRgo8lGEipEWk+0K9g5UfYqpHj1oomj7QDWA4TOX7DGbEuGpbR+o"
yy$(019) = "BrBUt3sNJZzycqapteca3m9geLUEu+VS7ef81RUOZ16jo+YaXh9AGzEB9fmrvVKLzpeyN0Wa7JFgh5"
yy$(020) = "qXSMRWCTFNcSRUQ2UPfwCHliALgXeAZ05agSQUhcfChB1WNdkDENUQASqTn4Ike6u67OWYLuInX9FX"
yy$(021) = "AsEq8xrpqS4RuN0jX7LRYyKLO9ygRFD2/uWmb8nU7iXw2OnR74dIphOTMIdrrumf+GDWd4sEDFkVtb"
yy$(022) = "x8Tv+ps9cEIvCXh95huE1AIxIQS3Ma5Qz28hAqMlDMpuTDhGpNcZ0Mu4doxhZ4uaAZUDbx31jyT9OO"
yy$(023) = "YY2gQfYFZQGQ8xUySTzQPmfvZ/Y8F39kBUCb9DkGNUnyNzPPHeDRQC3hL2KavYWgYYKI9KyrzWkbDy"
yy$(024) = "cf4a8HIo7aNMtpoVoGmnQOdPZ2doEIDxUBdPAYlUMyxIA4LwpqG1KOCFke7iofbOUbx/2QixWPjaab"
yy$(025) = "kS4RIbX77XwOiiqFKcTpq+SlDwN3laBGvraWRwBCxHdQ0El9bpUgh+q7RcXb7M/wuXn+sxZun2BGEG"
yy$(026) = "LUlSK3z2WgIuZt5wBBFrEWE9yLlvM6IbB45J9i8DmlsUFfFrI3ifb0OXl+FSuDt4KCXpmMq/T39I1u"
yy$(027) = "LLyGN9Vlr19cjwFU61UIk+wigORyr4G6OnsJyBv0tBtGAJLGzq1RpWtKBIinl5SxGGGIhyygR7rPjY"
yy$(028) = "CrFTQoh+cHQQbKo0SAXhk9RWDpfNTKBce5IBtcwRNxeXrYsEjEKsoBey9qvaxaE0LcSSOWTvzQormQ"
yy$(029) = "qYRBX3IckM3WBXuBIXs5sD73sE86wMqLmM7SWJBJBPrUok/EbCGcuB2EzbgmQEKMDp7pskcaWLx8HY"
yy$(030) = "MS8VYpA5lSTicV13y/tpWTQuYyGjh8LjSffK2fW4eKKIr4F/oX8Yz6nk8mUECdYwCt85exO+Y+13dp"
yy$(031) = "ro/FAtiTpYirRr6hzYl+qIX/LSNgb8UEjfK63fNEmEdALaU+xmSvr9u9ZolHAgWE2WuKi5uBPg51nx"
yy$(032) = "sGe4QIH/8h7LXkN6nPHWo+1y96PQZCexhUjJoSqMh6eq725HM3QT6D2YvksMo1J+rN8URwD5iI7dgE"
yy$(033) = "mewlxLe3iKnAId8QmSA9NsohOgU6+2YOgwIDpEdhhX6Upe97pfv9MtESMtt+0RHv1fZCAPlEMpJSp1"
yy$(034) = "8CE0tb31pjoQRkPcXYa3zLk0my6lNJzIYQ8egg3BGRYj5gSng7nDZk9ork4yStACs3VEAh19zjxLxO"
yy$(035) = "EA/+DfzzvSQXbIkXc2J4KFEyAFLG9isRvThkLSMrfPY4e0V0KrHnlTioAuxhr6GBCHvTSlVh7wAhHg"
yy$(036) = "zhRhZAlBIrjSO6VYU9YVAx0ZgIkL19g4jJo6rCXk8Bgd3bPjBB3YbSk3IlUYA1d0u8ItibPuzKc/JJ"
yy$(037) = "cY9cmRpnL/sOzt6Hhw+UClUJnKwKkthjsjdFSqa2QiIAAVoB9rLFBJ6pTZCqT1lxDxRLJQIsEwVtqd"
yy$(038) = "GyJ4oQZQ46lKmhYnR9plhYLxNPGCKwt2eCVH3ykfcOtcXF+hyI6MQyyFbfIyW+gqALQOsyyB3viSES"
yy$(039) = "9iy2XNPcY/GJHKlWYSKUwpg13qMZSUKIaGmUO3vaJmW6GFFJwxmJxGcG+plSHUUR6am00SeFty9ACb"
yy$(040) = "9U5mT1uZ2ESGAPdbDG9ysd/JNIIDt76wmze/cx6FfYWL4S3t05p1a+leRJnp9r0eDD4jUmw0NFc5Ny"
yy$(041) = "bXuffqrOQHthpIM0KcdmlIcAFZQjsuehmgbS9KlHc3QKD+kabpRIa3qUvUr7SGIPg1YQqy1zUHtS0d"
yy$(042) = "lT89x3pBc21mL0DmSv/UEqlvJi0xtpTnXZGg0OCBH7jYEGuqUgUZmXZMa7pKC4roO0KsGmHD0FlD0U"
yy$(043) = "GqBxTZoT45YS8a7SJkS0kAwyK1R1GZSUGch/mYF4jaWb9SQLDB7oY6K5kS+9zAPfbjanshQRYhvzmh"
yy$(044) = "JhSER6LWsuM6YR/YLC88cU1NZBmpRnDUe+39Q94nONnlJVc4kVacKTFiqNjih7khgx9rzU8Lk/QCQ2"
yy$(045) = "wInGi44GouyxgIr3U+RTA1C09xInoS1ksEdDNzanEg3ByDIP7vYg7BxsbiOTCBokMvZWaIwDE8dPfB"
yy$(046) = "yuHH56Hst1fSbl/SPFRsBDFFAycoF/1hkgRYwISImWa9SlxUWaboDRe7epBOYsU5Oj5VoP3l+PSzUO"
yy$(047) = "6vQqLdnEMs0dSlIe7CUAwhaFmD4seN5ueSCDGHuKNYrwX38f7AOhnkQscRZactANku0e8OYJUJmAuK"
yy$(048) = "pIoBRJUo7CmNbSaeyABc/HHt3MqBXZC+DDwvaUsPvcwBGLMifqL1Ip3qsVpPdRyaMguSeL3QuAtDj7"
yy$(049) = "yG5xfVddIeLWSGYPLv6WmNOKi4jzU0n2EDqn0RsFEcp3LKBE9RodOj4WeQF7WbScgWLZa2BtjbOgix"
yy$(050) = "ERyZkkZe/HNYm9Fv450UE22UsVG4HqsXiao+xF5vDk1QbiMoKMCBotxyZItnv0KmagFRdItnvs0scS"
yy$(051) = "e4Vg6xtLptax1ZYpEVT2RvLmFue6hgAR9iotqcziXtfgDdi7368oK7ycva7KnipGb55hE9bUQLdXM9"
yy$(052) = "gjsncem7J3NYM9rLmq3maV+dTJ3rcvvm731GVaFrF0Tq3s4UivBMFe36K5WmykgF6PsCphzfUG2vCU"
yy$(053) = "KKwlEdGCRe6QlEs83Wso8d4gA0HAgUE7AJK3Bpmyl9A3APQJ/heHK59DJqMQIYPoK+q06J49pCwvUf"
yy$(054) = "bepJnPlUAJ+F8LyIyWiwoICizfirRFXSZ7uXuyNWJE5EGHzgkRSElJuatBqVahipF3OBs0zoVdtSfE"
yy$(055) = "NwIdE/boKypoIlVH+Zz8CmevTIiQ3DvzuenpQ5k9yLKYoRPsSSBWodLqsNjQ/TRiH4AvIOxFOntKbT"
yy$(056) = "kAeTts01XJcyJ7TSSv9fM1tfBaipbPXyCxlEnZSxCSIirGHqpnmpvj9GnslTTQBF0JiSi1FVDGngQy"
yy$(057) = "KlSQ80T0WjZl2UP3NPZk2WONqzdmr0xljy+OuWLYmGZ3ihhZQLey7HkMVFNA6DEXP6ruTw4ampQLM5"
yy$(058) = "yxh9o8PqKZWl2yRsVsTk0kixHqT0cSex82oyNdcxlIYg/5mzWJvenDJ1bNVdnLmpU9oraIiigm4gsq"
yy$(059) = "eybIYE8Cgd0za2G265RjIGz3Amz3LKCitKYmzYkRAVUCWBkXKkXZQ5LPDRWQz0C0StBi5BD2HF4Dvh"
yy$(060) = "izF0KmFss9keFdqytd3HcyIjpEedlcdSKuHaBtDvIsIH01nLO3nQuUpQ1eY5H0SWWPE1E7kUFMczOf"
yy$(061) = "a4JKOohnajPYi38s2FOGN9LqexIREHkI9mTQ7eXPG4K9lhvUUnqy72MhzT+sH3P2mgroo2mNBBGX/R"
yy$(062) = "KTPZFOUPYmqXU1HEBhW4D+ir9O2Xv5xNoTguIedRzAnhi9NWKRiWCZ2oFUJVRAVvZQjtf38hqIllRd"
yy$(063) = "e+ahxpKTNxhb63vQAk4EkyS5IEHZS032AJQQEAKB3YilLahOrxHo7BkVKlmfWkiLWKFQgIlo6aCrw0"
yy$(064) = "8V9uh7udkgGua42PMIe4kOmphr1LALqF+h5SJgT65Is1yjbaznwnaoPuYNQG1IN6T3eKZmrBwz9k6A"
yy$(065) = "NGBvX3qPZwBZxNLiUuYLLIhfzgDJq+EqiLSytbasrbt70iM0Kns6aFwLxZxKkTynFSII3GtwEMvU7o"
yy$(066) = "m9BJU26bA2A8Tre1lshNDemtQTebTZvVtaWaBzaqJwuJ/NCfNCldeYk5znNvHfVaiFUvZ2cMxsB8lS"
yy$(067) = "ThuO+gR7W6C8JmhqaG6p847Pya90bF7jQ13Pc2MeYhNr4qnbCZjPrdu8hpSR9IuRIuVEMx4Rec3yJ0"
yy$(068) = "J0JhH5gCRrViKuLSC4th7L8KygjD11VzXWXPwTOEDqnHKcCMxCiWojB1ntHvH9JRUkfR2VvbemhZXM"
yy$(069) = "cDGimzHU4Sleo8nnJECUiMQOulZBtBc3qDnTa3i5nC1ieftAzCmzDEIiCBGyGFH2pg31vEYGWmGgth"
yy$(070) = "mxkCVqJ3sI2DPtHi3vySv84n1CRGX7sO4AXTtBONurOUC2noA0zF5TGTgFlWqiQmVhD7sPsgE0A+nR"
yy$(071) = "sg3U7ldiGWREy6ow0BYpltwRLWtEYBO2YwHJmmsFtZwgF3soFwYWkKRP2ZwyVa31tc3B+oqkHaRgZu"
yy$(072) = "3fk75dO+620Jzyn8ROkI095qgX6UlruYr6f8PuEYtfE3Nqkk+daCC6dzSze4uA7OzppyD21f/a9+aQ"
yy$(073) = "1oKHiPRundMos3tFFUQGamdvZJ44UJp2HMVeHaXbHSE8bpL/aydf2B4qzWsw0C4DeVb2HHVYobDauF"
yy$(074) = "kUlmluNucW4ttXHERkOzFkENo5mCF7Uk6YXVcyHfpx63Wa1NTKfEs865PwT1NCQ/ZIa4uZJxXModeL"
yy$(075) = "FZBd9rL6ZEw2rsQqyNAnMqdyvZnlmvWtvOw7kUVzKQgNpVL8zoENNOloPlfJy2AHlmV4chTGB3Wffj"
yy$(076) = "l59GzsfdisiQoVB60/d4Moew8tEQu1mrH0XB2ezF7E5iR/bpYJuxagsnpaeTG7p52ZzQUzQPBCorzN"
yy$(077) = "QlLtvLPhNRYBGbIXI0tbpL4H+sPWxsgjIyKngG6po84qVPsqiOUMoQLSe4KKq0JgID3qID4nTbsR+m"
yy$(078) = "vyKOgwVoUShH5knKShVzrh/7XU99TtcSoF0vAmSNYnOdRgnybjXRFdMRNGDnkAe5iu8qYBCimoqYAu"
yy$(079) = "5R0z8naunPJPSwF9FKddPL6nwXnUFmXrGnwvgUiabCDOKvMaT3kd1ovZRQpjE8S9pOwAYE4r9R/SN9"
yy$(080) = "aoLdNkQQaNET9hms94HQCIjiiCh5LYV8HEKNdQpJw0mJ6uswookz3eItjEQo7nIX5qJQOxvQRGhaqF"
yy$(081) = "ihsJj/Q0kDPeg2nEKKA6C+M2K1TC5+aVnOL8NewiMI74MtAhZy8nb3tCO6/D830jV+aUSztmmhLIe6"
yy$(082) = "FVVxTQH2f6VGr7fE4YRSp79HUJRGVv5zljrwZFLAnUL20wUCUbgm3XdyKNZEXdRSAP76ov6RPtgj5f"
yy$(083) = "IWUmIhYtHaTsApJB+brYNWqARub+cvY83OJnJJs6aBJx9sghW7bRElq/8mVCSZElQttLwEDvMlAnUX"
yy$(084) = "bmE3xKDR+XvYiOTBSsdyvrA6Q3LnupNieWmkGZqU53VBhzou3aBjo/CM8dPaV6TywJzXlbudwv7KCH"
yy$(085) = "NxfZnJTSLRRKDCeir6lpoMAKmlGhIm3AqyvGTgzp9Fisqly+zctMlp1NnD1PAxVEbcrccJrtxIjV0Z"
yy$(086) = "XpYQ2EzBMH07psjUgnTE/r2cqiDKL1PdnuyaCaFUTZC9tvNPYYqL1b1JcjxfBEhQp6kciVinRFHZQe"
yy$(087) = "81N+vgryY62yJ4NEVqNV//MoaOrbMARobLBHRxMGvVJg3tyGVagM9tiRlaCnVvZoY17DiJYpKIaCtI"
yy$(088) = "M99eSieixoW6+VaCB17yjtbgbosb0nLG8z2BMZQOmKAmkIlZDyej82QGqmVkoTBqIGEBUqeoEFiRVJ"
yy$(089) = "Hht5cOCPtDpiMqXU5OXhif173uGEHSbFETKx4q0DjZ0MdMbY8/Yr/ARqvUkkfdsJSrLIcpOCMGNJkz"
yy$(090) = "1xDC/bvxeI0/c+jliKbZ+tVQTyzCh7ceY1pO1+sNe+V6Q5rwLi8Z4ke9K+J1hKswTPfFWIWyNM9E/g"
yy$(091) = "MUGUBdj47iBC3oFGLQgB0Z175q1FZq7nwjKuZ4lmdc2NqR0i3W3Auo7tFiEsYhGZmkdvgkH23ALoHQ"
yy$(092) = "MpdDD2uoK95+SGDKAeykKk5aTVZK0h5tTlb9HdAJp9kkBShYqf78vRRfDAt9wExrLuzhLAYAvYQ77v"
yy$(093) = "AimrQtRHsd0AiWV0thoLOTxN+M4iRLXZouU65D6Yuj+HLaO2+9pY44gEwfnkbTjgXbeNj4FOu6kUsQ"
yy$(094) = "wwKA+gnZkg9TQmlbatUuBZQmUB0vZHUPEr1XYcNwFjsrcun9cAM1TEoHavaAdZdn3T17G/eLD3Z1aM"
yy$(095) = "iJar8pwQFrl8sL36cn026I3MHirXjw/r23lTZ2XQpbpnHhrZMjoTZJlT5GIBiWg50nfewoZbF0bsY2"
yy$(096) = "loPZmZljG8dL+rz2m/2JoNutVkj7bmvJ7sdm82yFzPnQ0i7L3+eIe7F65Sw7dwT2R0hUl16TlN/nRa"
yy$(097) = "NdmbB+qmy/c0bVjvKOIG0TOST2x3FHE2wt6kkT5fmr25c7JtC5jsGPdjmQ+a6ufULM1cz0Xy+SdLa2"
yy$(098) = "ods4hltuxpoN/ZnTOtzZyTVh21teguPTmH5/xUrJZDFrvvaKISuBh78YzhxTYAawZ7txSzJHtX83v6"
yy$(099) = "EwP0T/Pn9EMZRHh78XQ2e7HqDyh7NHJb0u49ejNfIhINNNk+mskeGVpLB8Wrd5C9T65nzklKYimIsB"
yy$(100) = "c+mskegHy9OjpFC7AnxVaLsycl1hw0V3PlKgZLurrzezJKqq+682SvZdRYbLsflZaEbctJq4/63QuN"
yy$(101) = "tmIciEh+bdxbS2lBKzZPUSQkhp3hNVrKW9wBVOfbiEgDrTnvkkJbvog1I9szQ9lbq6Yz2SuEXiLZI8"
yy$(102) = "petbqA5mblJjq86jzZg6tUVkHTxmzNpU3beZv65bnsyY1TXpg/p0xiKXsYND9iybLW4uI9afuWU+2+"
yy$(103) = "o/Pawl7DAOH2fnn2pr9dJApLBIjmuaMF2NNWJPHAFmFPG94lWp69dJCz30lJbUKhmBjxmtPMFqmg8v"
yy$(104) = "hiqTkR9lZHd8g10HgBzZV6oiascgfZG+S6y7OXWu+/N2d46cB57lNq4jvZKb/xIrInQMzu3SHXSKtn"
yy$(105) = "i0iECpo2zu6gudV0+Z44aPZHMxNG2SvVFsnUBGgZu6cNj21AiOd+XAJNffWOIov19PtjTwJRn/vkDp"
yy$(106) = "q7bE+p1Pj5qHk/dI3rOgW7l549u17o5zEBDSeN5UF7I3Jx/3HBX5VBf7+GN1hEczX2Vo/voLmF50bV"
yy$(107) = "bS4IRyynS8vetLF6B5+7Vp0bwyogugvowfI1lrTYuAN7ycPlNff2nJS1ltTcSyp7y4HSu2juvCqB1n"
yy$(108) = "6/NRYX6H/t73Z5eBf2qndhr3CHOU199XzuYj0l3TuwN76vxXv+5tXp5rdP8SPqmEuzfB+Lfu/HlJQ/"
yy$(109) = "cTtOxWqtaEz2dJ8LoPFXMOghOWRnG14aa5rrYZt9OfkMPw7TjwMr6HZUbqjseaDO//FZepYOf/3Pdl"
yy$(110) = "BKb14o9QTD6xEbMEyQA4R0iQDQ0fhoikED3yCCRcu67PngiMdA6fHw7xzsGbKHe5pWx730Kh2+7zqG"
yy$(111) = "Z+iTBwXnyXa6lw5PPnOCNNnDoPGjSXvkzyBCPxkCwxul43ECIPP3qFDQXrJqzGmIQej76fDGFCPKnq"
yy$(112) = "fnuZi94dvRh1Lv4jgdOtjz9UwN99RLx9Pqajq8PXYMr3Cky16Kg26ctWDQ5I/sIOxzLwz2VtOYgE5W"
yy$(113) = "naD3BnvV1MMXOB0ev3cMr5r2jTlVJz6sKAzP+w72qqmuuW8n1ZeNs6+fXhyPTFDR1dOkgdLqGGIt3z"
yy$(114) = "E8/9Bgb9L10+olBp22HaBE3sfCQP+w9eZ7AOqPZ4IU9sZYnd+MMRH91AEa1wx9GqUT/xTYG+w52Jvq"
yy$(115) = "tWX/7eXFW6//9YOL44Hnsnum5p7jRLEPF3fQcwzvWq+xeFher0fvQYycczqfrurs9eAmN+lR1w1KWg"
yy$(116) = "WdPQwan/t9fJ1cIBQZ+gQ2/Ar10uFVz8Fe2dPuBeT/rH/xdnQVr18cn5sgVpkvGOzhAHD8y7GXDocu"
yy$(117) = "a1SVzucyIvArk1dg9w437SCshF2NvWsAoe4pBtVdPb1JdfYA1PpLLLDfuKScgaQ5wWLoGDKQ4cA0YY"
yy$(118) = "4813/73Yu34w8Ia+6ly+4ZeS7t6TcIS3l64Rieked64H0mO34VLrIDNNTzXO8aQG3veTo8e+YAsZaB"
yy$(119) = "fNJTO+zNkL001SOWKXR9VdnAEuHyGmtPDfYe4Nd7X29g9lwRS1XPamhP+9jtD8cFx/CqJOnS2UvjLt"
yy$(120) = "anqUNgsewd6ZoLoJ98gYn9uct9Vo+ea+yRNXVwdcPEFRJU31jnlOBIZnjuYg8TYWPvVyB7V06vYWdv"
yy$(121) = "gBrYRuw5hucd2GQvHXvY7vU8O4idDDHYG9/Ddu8ktIO4amQgInvp6KsCtrAudcdKaJvTEAfsw+uJgz"
yy$(122) = "0jU6PsDdNrHO/918Jeg7LXBRvxI8fwDH2iRIwAlLjYG+p7CYjmpmOwRtOGHeRibwKyd3s7GyTPCUBw"
yy$(123) = "A9jh6NjBXuXxc4vduxmu4WjZ4qgZexPDP0FPgze4p98UHMNLBw2dPdDK0Q8w6MRFRBorv0+Ng8ZfzA"
yy$(124) = "T52nUiTi0dA3sn9xyg8Xd1u0dAU+I1zOCDsveykVp8LvbFmL2Ry+fGhuxBSJDCxNwOwDdlj+jKe4j3"
yy$(125) = "vrKDpr7+u11IxAIhCybCpYQW2Rty2XNqbtHQXLIHlbD3vutg77VeHSXx3s0HkL3h54vHeyO4fFW4Tq"
yy$(126) = "6IxaK5YyERn9hB067pNXD4mm76ZzOuk+k1SOae96Cn77lAhi0nPfmlWbK3ZnqNSfXtzc4hZu/SVHeX"
yy$(127) = "1yC5b9gH2XN5DYvPBVBlB0yYO+ky2ANL9KpzBhVkR0/fPjJkD7Sw/8ksn1tE+m+9gOwzHb5KZ2Rqxn"
yy$(128) = "ouznN7b2/OXzydYfdMC5tCsjEuQVbjytSM3N0jW60n+a5bjG7tVYI0bYK6O0JsU8p9WjZKZmmuRZ/S"
yy$(129) = "D1WyhXw4fLq4z8UCeXPxETTXVHen5pKrCz29nzM8hYi0dwY7ntzlEtoUn3v27Dx9dnX2bB5Iq1CN6P"
yy$(130) = "Cc6m6dE9bdGwJamL10gfqe0dPrJ+9f++/xY2KrJNrnxG6hSu5J5jqDYrA3u/1P15b/UJn/A3u/s5"
yy$(131) = "7+X7NX/7/O3mF9uNhPeoc2/m+o6h9v:5FF8"	 
  	  goto label_info
map08:
      if mapnbr$ <> "8" then goto map09
yy$(005) = " ~DGR:SSGFX000.GRF,29950,50,:Z64:eJztnU9sI9d9x99wGJIFJhSbQzHbMqsReuilB6p7KA2rKy"
yy$(006) = "oLxJcCveZIWkX3WAoGCi0il2NpS+1BoNxbDsba6KHnHnMI7NFyu7wEZk5FDkZ2GAYW0IOXjIDuLHaW"
yy$(007) = "0/d7f2bev+GfReJD0wEtyxQ/eu/3nd/v937vz8jPP3z5nL2un2ff579Okk2vhfD9zXhvjdddgZihda"
yy$(008) = "6SQEQbE5u3sTkRrkVUv2XLGdFYn2C9Ot6Y6K9PhOQdawWha2XZ67dBLS9awYZEfQOC9spF/7Eh0Voh"
yy$(009) = "r6aVu4ow+VV37TaI5T5Co6WQiSgj9HQ9gvcK38KHmxH4hrjrESF/M0R7+YQxBtvo79drg1ge4H/q7n"
yy$(010) = "ADwgdid00CemUBgSbn/toEquF/bB/7ivH6MNG1ah4gdM9x22ailxi0uoe1KtZy2kAGywuVBvaSgmP+"
yy$(011) = "eMWcr57hL3Uz4Ri9pOjjL7fNRLXEiOlQIEoYqtm+kbD/hxO+4FdFF79yCGvGtBpi2TK/gltCvMV40T"
yy$(012) = "YGTcFyNKw3ik64gtgTiY/q3ZJjjlu7IhHSiGMeE7B5lAj2ehJRQPZnRsJ5xogw7CXiOFhyD58aA7c+"
yy$(013) = "ZFpNJnKvynXr6xMTUbnH2ljciJa3kHeKohF8+1cqYR45WxUPe/05/in6QCFKjrkC+An5CmpNfJkox0"
yy$(014) = "YCArCAfnT4GNkXqiEhIyZ9UStQqYDcwwEqX3gK0WZaBZpfFaxGqzw7eabfetrGxZFoOVzO7vDicHH3"
yy$(015) = "A72OoMRlohHd0Se9Uf2Dn8sfP3MkQvQrF53f3rMejeU2rPGCESNZK2Lh96cf35qM1BGuzWPwuRCDjH"
yy$(016) = "BxYE77Tc0OrpXm7aiY/OqfrVJJ7lXqJT4SLae5yr6LLW+MFDu45Re7IlFixNGo3pUJ9OITo5cUKOH+"
yy$(017) = "zb87nu/JRlTMnkgstzqP3U7flW+6mzMOBkCgw8fuYd/9B7NWyZbURhuiCtknqFWsFVWCthHZkpe40H"
yy$(018) = "m72h06jaGc5M+5Vr4lESRZ2fBxkfBAXR5Rz7pSr2jSvUCnjnda5W9W4Neccq1GcU8iSJEx74Pl9332"
yy$(019) = "5vS5aMdoIcYg3EOPEx1WolQ6l76g1Vhug16/KdYOirUaG3vqyAqF7HMZyRFlQeReEMv33qW/+ktnN0"
yy$(020) = "BoK9Wqq8Sgj4k6Ieq7hLCGhBjXed61dG9Hd9Ep8vyKFxDitLIjtjE/NxCHxPJJn3jWr9zOJRL9SopB"
yy$(021) = "5r0dRkButL5xOwOXEEvmOO127V6xZhUhcO0AHbhFbTwX8waWsUm9pDHA/0m+KZFgY6PBA5Vw4A5QAs"
yy$(022) = "IQdOuWM2KuZwbcRhHXKN5ZxTsHi/GI0sY1aplF7SyU8i7XqoRj8NakfyukKvgWQt99SYlX00tZK2gC"
yy$(023) = "tYHAWrkhdiog4AetPK1AUQ9XZw/BtRK083Gx9kP2oxytaCCVHRKGt+fXe9hdlhMWJ0ZYq3rvBdZqTy"
yy$(024) = "KouopWpKboV7wB2i3gLrbJ77jgnljcTtR8BUF11a93+s1OqdYB+5FYM6AtWSv4OH5hoYA4b+KvRCr3"
yy$(025) = "kNdXH5W1+wGJCocsLmYPiu0WoiVE5YDH4KtrzUvGWOI9bPku8ZXuN+ztNGpHClE8OnOPfJzbnTuEmN"
yy$(026) = "Mul6opca30CkcITuyVdr8C3nVaYW/bPMONorFCYB+ysUT/5XYucALCBrO3Z1wr+1LRCs8R7D7EHbz6"
yy$(027) = "7vvZT6qM+FRvw3pY61wUWwi7FhIyPG0j/ms1ouCaDX/eHlHXSotynncXWtTCTcefPWJEjb0b1nNHNb"
yy$(028) = "jjzTP30HZwPvee8HfDaj4BNt46Or91TeIqYG+1bJ7htpQYpDkZe5Q7Bddyff62zbXa0dqAe3Z8DzVC"
yy$(029) = "yPDZ27wNv2HSCmfn7td7JGvxtwI+/xjkEqO6REy5VpdNk+X2KfgVdiovJf6RVxmjxz2FaIFfu51nNR"
yy$(030) = "JUu9xyN5QIQSu3TQmSr3AABulv4jFYU9voYuI7eFZYu8Jzw6I442GziR3Zcst3qOWNJyRZP2mrRGQp"
yy$(031) = "WvmOR7Wi6X3YUIl43+RXeOxrnIJWO09qKqH6Fa3iiOXwOherLGN9ZZ1xggTgdJASbjsnt1NHqtYOUK"
yy$(032) = "2DatNzxH8ZVo+2cVuuAOpYdmyrS6KvgVPWyGc/GbLxPHak6hVVrACIGq0ZgEAKMUNSzVCh6xOoCVqh"
yy$(033) = "3aCyc8aJFrM8mkt2kEWGCrJwLYpfuFrof48TtQOJCEXCaU6qoNUhEZgTzQ7Vaq60AV+cGs5ZWKsj+L"
yy$(034) = "1ieUnamEaJYLlF7KxRvxq62Et203kLt1wmUEpMibomYv6xNA6mM4h/w1q5EIPpJC/VCkk1Q4FJXKpT"
yy$(035) = "L+n00ySaakUJrlUlZIQ7YYQrEESr6OA7Qq8sMhBDvrJwhdmBfJVp1ZJ8N/USXkePmOWNL/lPrutGol"
yy$(036) = "zlMnMi9atT84zF7rNenfk0Bj1xyddAFNCf45FctPzhn+YQISdwpX/wSNRKnBXmxiD0CpLVAby8nDa4"
yy$(037) = "5Wd8DXKoWZ5L0IhKid08gvfqI5994z/yTnFbuCL1lxMdkKCErPNjbvkttY1oein6FRkuSlBTYeJw0J"
yy$(038) = "wO3hHaqLJfLmkF3S7BDCSs1v5svDe93PWVNkIkZZ+WM6P16+hnd4jl3WFFIa4daWUicNrP6g2I2vrR"
yy$(039) = "kM10PMXycST2Cg8FfoF9UyAxeOaoBJ3dpX7V9Ukclljq6fRrk5mRYFr1eV5OR4N+c/5TWatk+JXQRo"
yy$(040) = "vdcYQ9ql+Dr8U9YUmD1QyeYHkj3c7ABSWzfD9QiF+K4+CdAvsZ0TWHoPNz1qsmJ3CM3ieFqPdrVyVE"
yy$(041) = "v/JIqrGI5e+d/4ha3jETTKJiRhzizxLiMCP0GHSyaqrWRXsWicF7nyltxHcyy6sos3yvcf4JnRV2FS"
yy$(042) = "IQ1mScjIAyv55DNCRv59fo0U54G7R6v6ISeKqUSCMnsRwdg81Uq5+ohFRfEQIGkSadPgOh+tVYaINK"
yy$(043) = "ZZXh60EJtfDYUfRabaUNcQ3ZoeVXGJCVgqHz7uj2Z1NHJS6EWtSFoQt7cACeuAvJ6vu4ylIJce5cIU"
yy$(044) = "QBu3ylUNnBxdVLv/u+GudJWM4IC2bXsMqJajSc3pk8btJpsEDciHNnQrQRrj9ZDE4GImHI7SSgMN1q"
yy$(045) = "0cRe+4hsWMhtPH8tWN7wyYwW7VHLlTUpwypOBQj4nXVrCSHmqwokBuqMp6CV9xEOw5JCLIa+4Ff8py"
yy$(046) = "2WfW5N+JJBRsRSzVBQCFK6K1otkJgZsgtqUdAKK6baMRC0QugJU3ipVklG1NBwDUK8gw2IJlUrtIxo"
yy$(047) = "499N7GwLWr2vEqJfBQgdSoQ7McydZ4Y1ZFEr9AO1jSAbB0lKUC3/WrNcWEO2MyL1q91RXSW09XZZK/"
yy$(048) = "vsc5UQ19tb/EeZVo/dr1XiOsm00giY4wgRRbWaqr0iBBbKqlnFvRtf86sQLdPqzOAlImHLxAiXDSa/"
yy$(049) = "eqyuX6VavXj84NfCmgwy+FW2cdUuNaePccF03NFjkF2hTgxIYsdEoGgl+m5G8Nobu9a9UGljkcVgkF"
yy$(050) = "k+oy5CXUshHnmXBqIhEE2FCJCyqlYg/5C5886pSaupv8WJ7GeHWbWEXwpxnbzQtKp1+NxZJvh6+31N"
yy$(051) = "KzcokhgkYXhgKW1oK3fY8qFkeVa/sgzXlQnvDO0EMhEoWlnnUq9wgurCV7LO8ISUWAoRlj8VtHJxeY"
yy$(052) = "hTs8VsPiZfWzIx3/oi1coqwaLplBLcrzr99KYbzsnwW2hRrSxERsOG3Aa9iOXlrHwWLW9EMhFn6+1l"
yy$(053) = "l7TiqMQvJeLGuN7OtNqhWg0kYibsc2Z+ZUl+VZeISNhLBb+iStqSX93iN2TZ+SumFa0c/jgs52jF9C"
yy$(054) = "XpRrR8uPc6WEnQ9StObEcC8duKtgJJBBBiMHMtbZ0h08pGolZpUGnrDCwGvZRQw5Bo9eblc7VXdBmt"
yy$(055) = "hViJJYRhnuWEuJeu3AlhuJSwVxFarwpcJfqSepXZoWmlWl5iv13Syl+plXHfmWtV51o5YhsPd1XLkW"
yy$(056) = "55UyTkPXqL30hCZJBOaL3CWhUqbaYVP/clVgCzVFamFWx1dXgk8qGQEG+EPXpGeIxoTvoKYVxnyLTC"
yy$(057) = "DTxlWgVIaCOU1xmyzO98wFbP8csXieu6tM6QHR50jlKh0mWZZTsN2GXaBeQpS1gawSxvkc6JMWgiRK"
yy$(058) = "3aqbrspeT2m/G+3gZcP+ROVUZyG4+8LcHydA1AsFwdB31SAShaOSKhzZ2PRoJWWRtYKxaAbYUYJddG"
yy$(059) = "O6zUcl8l4g80rdzpw4zQ5oNCzZC2ATfggGulzQcHs5FBK4+XlYbK0t8xEejUySVCe2zUCqVaqcQsO5"
yy$(060) = "eRWS5q1VYJwa+aEjHQCW0cTLWCb6yHtGCo5eyY0Cq83s1mZmnqUYmpcLImQCib0TgPIPoMhLpHnypT"
yy$(061) = "wep6UDyoxKvJw55IWNVmSCzf6b/zcmDS6o2gFbsO4fQCicFBc7pcK95MkWpVQ3iOIx2J1LSCn6VrXl"
yy$(062) = "ma9vOJQCbOGNFWCG2Ok4kmD4KcmCHDeQawA4ceW5BSeqXsTVDnbeQQdG/iyHS+BJH9arLWV4OVJsmO"
yy$(063) = "KM9ydupDj6g4n6ibiUi7g2SUInJ5pyXvtKAS8xzCAtdy7xvWGeQ9L3o1qOU7RKtW9j7RKp6Pc3uFK9"
yy$(064) = "IfFJFAaOOgj8RqhmvVXEE4KqHtTbymI+fM1AYOQGnDZMXIaZGUddj3cogwj3joC+8tf24CagGslSW+"
yy$(065) = "p0VUakfAPoEtv7OScESiK0618yyvcBQu73QNQrQDEhDKI0IkjR0Z4YvvrXzGBGJQIoz7g4JWlnjwQy"
yy$(066) = "AWPzYTDjndZyIiQ9XHulIRjvYJRHb+iljuCR+QE3tKXKXzwZJKyAt9KK0Z8rU6QMpZd1MVTi7ifQ7R"
yy$(067) = "yllNkG2AAFGtukbC1CsgikQrE5FpVcwsn5CvHWTUKkxzOyHoRzr9molgZ7b/JRHbYJaz741aKX7FCH"
yy$(068) = "RG/MM22WEifKg4yNp7PmEYB4EoCAMtJ4SagfQ9C7qii4uHjk5EilYw9WPr9HBcyxCDM3U+KBwiwmVD"
yy$(069) = "S29DWOsjRY7jsdQM7rIzXq4VSTQOX8shld/IFIOKVhVohreBjDFoUDc1pSSW4GhJbk+J6qb5CkkbE/"
yy$(070) = "mWC8OUOQZ1Ilu0NcegrlV2SUX7SqJNS3edYCssofBBTyAOJaLKDJDaaAnTdItrxd9RKwD43WXh5yjN"
yy$(071) = "V3WZ8ERiVBG04oTliISw5+Xxx5dSFXDNAP+2KxIh7tFToM0N6vAD620DEdKjjxcqQXM7V4z6lXguvF"
yy$(072) = "Rmn6VfD3jR3pLaEM/1OelITrrPLS/LlvvqWTWJYHmuLRLZHr1vyUR2WLQiEcmnnODrCTSKAnKemHxU"
yy$(073) = "XpPJYtAasIrwkBxrn6J0StSU1mRMMUgyHF1sp91pS5bHcgVAriHJ6veE44yS5emKl0ffxvaXK57teP"
yy$(074) = "wAZE0hjHv0mGArV0h8lIl5Cd8/Tx8x9eH7W3Qt2EcoO8XFiU+4X1VrNFORT9EDkC6SCG2P3hUSu0UO"
yy$(075) = "i9LkrbQhPLVlk+UXtvNFpivwwBvS9ia4J8Kk5ihIl2XyCWnEAYFZuVgArdLPikQo7Qln+TA9oidEMl"
yy$(076) = "uZKGf5yhIeNyTPAoCnWdljT0a/SheS7j2r00m0kFiVTE2PnDses8M5GlUaATcul2hZxS4n4OxHYLI8"
yy$(077) = "ec16xRrnD/5W0KmZmIvPAtDzLvSnnap7X33Ui862Q2HuTG43Jwbu4ZcKkbePQ6+DoHZQVQiTVig9Mu"
yy$(078) = "KEQ+FUlEBMteqVPb5UbSgTHE7c5FXIdb7CoM6dZ6NLRrSZHSysj/jTNeoJiGydoS1rNeGEYAydO6fj"
yy$(079) = "oCe1YV3xQfB7GUG1crRn4qjl6aJ5zo67RpTziedmrUhiN6qb3fOWZEdTm9px4g29ISGZEAiEayL0Wj"
yy$(080) = "TzqwJZ6FO7StqYSGfV4GrBF/vMyfOS66+YurZESFsSOZbLg2BDWbkyEZId1jOj5Ww3I2Z+hX9Oega/"
yy$(081) = "2FbXm+mlPOeF8ym5SB4kiV376wDMr/iZ7V5E8/64wldjhLOMIoFYzVB4cMz2Eut85QqpFyEW2Zlt9i"
yy$(082) = "hfAI/ekdOP5D8LKpGMnjPC2mJ2HDxFV9mWhKsTCfMrfuLO7QxQx0xQrbK9IhbQtVapRrTSn99WIspJ"
yy$(083) = "90SZi9AnP8r5RAXZrkQEjplIT71UqJNY2Z6g9iA6JdQzLAX9UIJC0BlkCOc96LtLCOUZ3nb5n+DddP"
yy$(084) = "+0pBN8V4ZZXj9akLe55ex5Z1sjEq4VlyXTKqcNMaLYjrO017ycKIuWX7RaOcQX3K9oQVFNCS9Hq9lf"
yy$(085) = "yPeDCNXBAahLxWsfcS+1xp5/+MtllkvP4+DI5kS96+cQ8vM4PqqQsso70z6eEqPbvUSsr4jlV9mJBA"
yy$(086) = "OhPI9ToseumgaAz3GkM6lgPdbqplQzEfocB8EyCVh+Ihzz1YhfKM/wHpHtG7WmFIlIPW0YyP6qE8Ie"
yy$(087) = "Pb2aHcMz3hKRZH5FrvrhgE1UtCtvjtPSnnOW2/hCOgOJwAhvKbHYnihEIe/zvFravlJ7tYpIftHblO"
yy$(088) = "Dj4BoXfR4nMT3vvKyNaaI87/x7IOZJtKFW0X1/hR0NlaAnZcOcj+Phzcv+i9Wi5Q17ZXpyUrjyzyfm"
yy$(089) = "aqXvjBq8fVWv3pYI8zql1+1v3cYGXvLWxO9Oq1a2WILW00om1tGqldurJZZrO6PbmxKL8qZaRaa91H"
yy$(090) = "WIMP9jQilAtPrt+5u2Ma32llhOfn9LIeorCU8mbmgby3qlELP3Vtkh/GGZdbRqSQTbd17VhpUNi/Se"
yy$(091) = "b62MKPU07gq/qgXFbHFprRis+cJ/UKIsn/1QLg8XgrZMxDbaX+pX2hwnNp2ZkLql9ipuGtZFxeuh8F"
yy$(092) = "eh6Iw+XuZXuPrtC7mB7mBFK/zqTHCstWIQZiGuiQjXIujehOmUhXKl4xpf91nlV9laUYndj02Jirey"
yy$(093) = "VwWJMJx7XdIY89ve8nwlXCy3b1pf4Q/3Vlluy8RCXcVZSUSmvyeztFfh1oZEbKl/I8VwBezf1K/Khn"
yy$(094) = "My6uWLbUT7o5WWK73aHq4kZC+J7TV6JRFvri5XEy2RYE/RhWsR1K8WK/KV1qtkMdvQcphPbEa8SlZn"
yy$(095) = "BrlX0dsS4VpEdd02sr1uQjjGv+IhXYqXrJGv0kvMiZtYvnG++v1oJbex+T1nJzM3r9sXD64fr359Qz"
yy$(096) = "88Tta+yCwt3j+Df93AI/MrX+SvG8fbm97B+K75WeR8Yr4n/eXIdYj4pLehl8wW+xsSc0KEaxHUr+bx"
yy$(097) = "hm3EaHX2kYlFeVMi2VL+OuVqgi47/+7mzvL1hzR3zifCtYi3zFfR/qaWx9ubEgt7Y8uvFAKeL23C3+"
yy$(098) = "+4yNvzmhV7klbk6U7S3x4MR9JVZRb0pDaAuLo2E6xOVEY1sGs0pLqrerBV5wca8foygLcWe10TsUCK"
yy$(099) = "5WVYXSVEpP1FbHoH1ZUJIPZDsOP4ymgHnUbJWsW93/SAGBm1ilW/KoO4T/fjXvxgbLZcjSgb1jEH+/"
yy$(100) = "+9H70amAl11dmGtdLBdrAfzbQ/0k2Ju7tar0bJpY/2o4Vj1Cre0+/5ZTIK3Nx7HjXlZwEY8ayJiZlR"
yy$(101) = "q+jkb7U2tnG/YDX6wGj5KzpGiZbjAeWSrBRbRmKiE3FvsZUMk+jGVePMPA6WMRFAv6LoHWMbUaKMg2"
yy$(102) = "VsArrBRHx8aNYqUcbBcjJffPcGWx9/qHki1eqDQO3VPN6eEyIw30G1srSTebQVY7Wi17GRSPedM2L0"
yy$(103) = "+iwGy6daTmKEso9TxsL6daJVxdyGHh/4ww7GcJ/Uv+9szu1AxKXeHBNTc75Knqta4YlVH4gHOdln6z"
yy$(104) = "PVSzAxuDPHXmImFkVN3W3YNPyqFyWxUatYrdvLQFx+jHrRYmb2KzKhFwnwmtEA9eLj0OxXM1UrIP5z"
yy$(105) = "3MBE+hc8Ja20NRlCPH0v6sVHY3V/jcWglq8wMcCOEE0fG++5tn5lQxsDBP2tG/NV9NMfG3o19ntJFD"
yy$(106) = "fNuV1dhytDo9fBVn6+QspaHyX+ZDd3HIw7fUMbo1oDiDVzOxDfWIP93HHQSCT/OtqObnrmGFzEid6r"
yy$(107) = "xafT7SjaN/tVtFDtAOv+LtqPT8o5fqXmK3CbGH+Nezn5KlLWryiBvSQ+8Y138FUcq5aTqMFvxXeNdr"
yy$(108) = "yJj02EN8ZRu2/U6lWk5nbo1QKHYvR6kRODXxkIeDY7PpmYtWoVDVolPh459ajlZ7ZNbVw9wsRTcxat"
yy$(109) = "H6mWwz1/N+xF04GZ+FytGQixj0fO6cJcM9BL8SvIWdH8Z2vmdkKcQ7WEzFpNBz09BglxcrCmVoQIkt"
yy$(110) = "wYjFBNsZx6P1SW5lyS1LsqAXf0iFSWOVqFiuWE+CPQ6jInzpU9YWrHBU49D8bmmnqm/N3aMrEDp6T4"
yy$(111) = "WCPYOZltU69OgbjKqcITxXKyTxPm19TJd40EqJuYczubFSnjIBTOeTkxuj9Q7jk59bXI1ypqq7mdEF"
yy$(112) = "+C5SPzOKj5FSEC0saaMUiICX7rtXkcZJdUXzFirv1POvKqpSG9S3n341Wi5nZyei3Yx3XiS7NWr7Ra"
yy$(113) = "dALnmTFxYq6v9NwOz7bOGvsQ52vmdnKzm3DP3ZyaQc3tEOCvLoCIjVrpuZ2khCf7mDB7uz4OQlkeX+"
yy$(114) = "OaOm8cnAxUraACv4P/Oe6tma9ICr01xoT2/9ow73nZhHDHuTX1Ql11tkkKDXFhnaztV+BYs1H+/NxM"
yy$(115) = "4GDGNbU5X9FL0moCU3TIV8YYNLUBI+F+fgzqEUXm7F9sEoPkj0N/imuGi7W1gv8xBvarWU5OzCXi48"
yy$(116) = "7aWiVnxBvz1mRMbUxIG3lrMgbLkxDcOXrxubmyVAlrVD8K6vD/MVl1Znud61tb63vrNv4A10X/X6u1"
yy$(117) = "iP+LWoXfArF5rxavX3y++vVlsum1+F9yZWkP:7FF4"
yy$(118) = " "
yy$(119) = " "
yy$(120) = " "
yy$(121) = " "
yy$(122) = " "
yy$(123) = " "
yy$(124) = " "
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "
  	  goto label_info
map09:
      if mapnbr$ <> "9" then goto map10
yy$(005) = " ~DGR:SSGFX000.GRF,29600,50,:Z64:eJztXc9vG0l2rmb3kj0AI9LIpSdLi+31NUBaGCCgMx6Lmg"
yy$(006) = "mSSxAskHtARsjO3iJmDiPDM6MeWaB8EKjrHgz7X5jcNsBEps2BeBlYh1xyGIzIZbJCgMmajJGIgmh2"
yy$(007) = "6tWvrl9NidjZxSLYAk1JJD9Wva/ee/Xeq+p2kizZBtLvl6+OrvEYSH+Ne8Xo6kco94eu0xoSovs7iR"
yy$(008) = "hcCxFKXF0T8X1wtbU0orYswgmujxhQxMpihMlVuXAFwujDd5eVAzmD5RBOjJZE4NZd8HkLVxXoZVHT"
yy$(009) = "uXJ95P5sMULrI4f/FRcOzCZHHqFfXB/h4394CsPl+igs1F+dK5jwEjCW2XSuHHhx4C5CGHqFW/Sjov"
yy$(010) = "3T+WKGL1mpZSBunnftiMKje3ZEMLQg6uStjQwhYotegc1GjXpoBThxZNpgsQVacjPKQGzZuCI/7Aj0"
yy$(011) = "rVXy7QEq1//NjtiJbQgsSdD4OkN063zg+S6jjAlBVt/uglN0MhEW3+4Ww7i8Zv94hevV656iVxhxy4"
yy$(012) = "4IOFc9R9WrIMzbEf/JEf3gPJG9jx8WYyuiy9k9CZQ+kJflrmPO1enqeioHpqld+ajnWwDOQYNx9c1n"
yy$(013) = "91KuHOT0/Fu9Ch2ging8YFx1txSuoA2s41rlo6II/oUghBe6eMwFfb26bUds7oFfdF5imi3CMC15Iy"
yy$(014) = "Ma8JRz8hEqdTMRiS5HUEDRD9FHPe3jRa5XSaLHDLUfoGZpPOnFCsBJBpkxw1o376zH20+1Yfl8VP+r"
yy$(015) = "zHkd/3Ojn6xsv7/y3gDpjXGFzlXJ8Tp1+2DrJ/maisCqyxDHN1S9Ak0pdLz5yttZiL4hB56LnZO7Wz"
yy$(016) = "01SCmBlhCuTpX4Ck+0h9Af3H8dNHqqky88bjCuTpQ+ypSWy0EZ7WrLQixG9akph3N89wsUfHgtrhCi"
yy$(017) = "a3otQsFmNwth9oE8PLryhkrHBkeMeyZXxWoXQdgtv/bW0cCuVw0YU/FtglBX3UrDrldhTJ7xI+cbTt"
yy$(018) = "66qpUiHs0EzcdcsryCmFcVRD7gCL/ZYa+5HZj+LvdwA9Un0jmsk6e/YC81PRXxucqVhz+ei2ClKrbY"
yy$(019) = "9/w0OgEt4Vw9H36m2GAeI/IKolCM+oDgvv1ktqPNB+4jJIJQ3lDBD/dkyU9m2pxjlXO/AiPdbFNdrD"
yy$(020) = "Xbta6EOJyem4gOuI7NNnFyBUDUiVdlurut98EnoO79IfwooQ3vxzJC54pMOJ6TsovTORDkzsPiGnyh"
yy$(021) = "z/3V2D3SuMKIjS6O4StRD+Zlq1dcw0jnLterad/SB7CUw6so6GJj1ycr42p9Qa7mlAPkBc12gBFOG/"
yy$(022) = "9C+l2U3TmPIop4gJUKIzosQCWIy0S3c0QN18MUlf8yxoyVN2L2OkFMda7cAMyiANFA1CuNXey4imN4"
yy$(023) = "I2Bc/XzgzBWuXGKsuQDUqVd50K9sMWO826VcTSc2roD9XCWMg3dfFcNduvgcDLIkx0I4ZF5a7WDYvj"
yy$(024) = "/MB5tEjm42u12iWnnUBARWqmCTv5yFqEMfOQ+tAV2B47FIXutDlYPGZJWoX4xOsDmNQSzngNvguLeq"
yy$(025) = "rYNRYZYi+sW1Pp2OdW6DA1TS+kCxT+gKwh/5YdsvphEE97sFTY4VQlcb3WpHzXal2Qk0RH860hAesY"
yy$(026) = "V9jACimjHSECezMxvCIU5+w/NEUO6I+OrNmS4H+8LKWg8kZ/664HYZV/1JR+MKvr6LLZ0h6lw6boPd"
yy$(027) = "zw8NrvB0YUTR38Vc8RcDjGBcOfuGfSD0YycOfvAkECpCUw2K+O5t06JgHdwNiL0GDQMxXzcQwE4DeS"
yy$(028) = "5eCj2W+EBI3c3UK/iQi9b281sgOZ+/UmWQXZOpg3XceOg1ZUThCder6ZnRB2lgVZir3TJ/QcRX4zVD"
yy$(029) = "jjLIWcDiMJclGmM30hC3cVKGEZDlAuKnBuK4Zth5A3TLwbIAV39lIE7v7qj+Cg0IAsV5BNU6Ob5m8d"
yy$(030) = "Xf3lO4yqGIcIXpdwwE1XatD0T0yiPPoFdvXSlHgHIx1QknH4ANxgairCJ8hiOITWyGBkKPGWiDeXPw"
yy$(031) = "ytYJRPGozhDz6b4y51s9nqvg5x64afZGodKw61XDIZ+ljz5eCnl8WShmxFcKY2co3OVRL6yotvyDyw"
yy$(032) = "lriBNvy3rV5NldqXauI8IPSEDldGQb9DliuqLEog3WgQeINvh2jvA4u1PkSHK48Dr6kGkJlIn7HJEv"
yy$(033) = "hmwd3DqRc2eeCRbgmVhtXedqOpG5ykmxeoyJwnrFEYKryaXCFeEypAIR71PhoxJcaQg31hAmV5PLRN"
yy$(034) = "ErjiD0YBv0TK4mryQ58mX4NAy9yCXnCCI54QrJMUOueJchAo7goXiBx+1ThGS9ygdsVMTNYq725OCd"
yy$(035) = "ytGWPbVfOGAIoIBwNdYRyakcLfmogwNRgkCZiETlCjsqDxA5xhUqZyCE5KgXkAoTBOMgeU9O7K01Gb"
yy$(036) = "RLufI4Qq4CWWwwJ6orOcZVLJcCbDXkfUwRGXoOM9a2+vZzhStEEv9gs1PrYbvqQMxwFVdOkfThlf8G"
yy$(037) = "I46hj+3FXDnoh9gsXuDfVsM6avUr+KFz9UzJBx20dozWMF0IBxfFLYzoVeoaV/P78jqYwybFyIloNp"
yy$(038) = "kLpHIWGdUYKb7EWUXuI/LbCosAtgomQvJXPsRixJ9gLb5FEJuHGmLSVlbOdBsAp89/jvWqvNnXuTp7"
yy$(039) = "k8rhdD2JmSLRkso41rg6nUvr4HGqRU6XIPor0oRY8sF/T7XI+cIHvdp17+qjknO1mHwfl7xJJBf5sE"
yy$(040) = "AoVYMoRXz9R8MMhLwORuUUgdAYbLCMqjpCXgfFhEMj2U1EUzuZK6XOEOQFooIKKxOCeEdCmLqbtlVU"
yy$(041) = "aG9hrh76boqw2UfI5HB+gd4erhLJ9w3J76QInyKAsToOrjp2RFeK4ain8ZiLckBry46uu8mgmcoR5c"
yy$(042) = "gn20QjcXY3AkV5J0VQrnpvUr2KSMEZB7qwcmLESWXaK/6phCB9yDFcSBYc4qvwsx8Wnrz10JemiOUf"
yy$(043) = "d1M58hUqf4P8aLZvPHe3m490REeKGXI3YxLoks0fqol/ZonI5HoimQqE3ieqtQF6VUGBjhhIMUMIQB"
yy$(044) = "gZ+ZNtHssWRfPBQqpXUY95RZI+V0ANlSoW5epCyp1DzJIjuMEGiBNnuYrFMy9JDjZmug9A/VXzZzri"
yy$(045) = "ZKoifI7wGcJg16z7iD68MhQZ9Bmcj/4rlaMODo47oBxiG+06V+HzNGZoQHzFEXnPRJA+5BiuznJa/g"
yy$(046) = "HCVajLsZfWd7Gbdslirkge64jDT2WE02a/wOZtBuIktXOxHSNxpXBorIP0bSF5FleXqb/SEMAVXgp1"
yy$(047) = "rpKxHO8SiSWufPTLNLS0eGqxv0slJ1xtNwc6ol3LQmCuRp3FXIkmSJu0Tb1KTjO5wkT5Fq6enWdyVW"
yy$(048) = "z1rXrV1HNnPoo8DkqwGfpSad8WM3DJi2dPPx16N0BLnixANARiN+D+ylwHJ16aQYqOnBzJB70iuKyS"
yy$(049) = "zlWqV2w6igxMyivggNIRG7FoOUVgcN+C0HW3LLiC315A8QpsMEWwUdlqluCjuOTtSENIFa+Qv7OxJy"
yy$(050) = "Nq2QjRaCEUO3bi3vUa2ain1ZaKNJkSXEU6V6//Q9SvAoag+2MCcaxxRatqUh8+m0nB1Z42qpOpXIe7"
yy$(051) = "vRIM21SLheQVDbFf6wmucrXRXnljRUOs6lylNTIHlesoVfcNqleeqyHG7lMuR4UuYCzIEJKn236Uq0"
yy$(052) = "n/ieAqIsOJshG6XvHXkcrVbkMdVWLxJaSRgoya5hiIUPidooL4R76iUhv8qpzRh9CrctWrSYipFDOU"
yy$(053) = "EZe8rEheSX4lcfXzcW8u6ZUVUbx/LHH15vJVNle7/BFnS86aTyWvcUUJshGgEbGJkCVX83PS6sw7bH"
yy$(054) = "gR5WqDR6dGTYZxVReSi0dX5kqqMwRXIuDLZy1jD7JOf2CK/o5zFWZIjmjgapE8WpqrLETaKFeozLlC"
yy$(055) = "MkKdc0XyNStXR2NlH0dCQOLMENy9s3q7HjPwUcFSxblSRiXtOzNaheT73AE9lhHnh6nkDfrNFJEjAQ"
yy$(056) = "NNn2OlD1vMQPt4/ZJxJUIIFosaXCEiOV6huZYIN0q4+rLZ07nSEaLkbq1fKY0R9ZCLwSIAD2lcCckd"
yy$(057) = "boAq4nXnThZXIuFGCkLWK9YHErIQrqQKFuWKIrgcLFSKaFc9a8yAQRJXKsIlXMUpgnz7xdlTqQ/14E"
yy$(058) = "oOuDKivi7ZJVPlgMJNDgQGyQ9MRMHgqtmpDRFHxDqiH/WNPnBqUycUbXjlOtIRZ2/6JldYIFhmiOQS"
yy$(059) = "hO1NvDk2uULsmIKOoNo+7WdyBSmhWevrX5hyAAUkVLRKLkUAmzzBAUQ+EzFwDjmC+iVHGDxJc+Rjfq"
yy$(060) = "wmcyLpVf+xeNfpMsklBI9FlRpySxw+6ldCUvSSEZK2c65QsMt/y/lAnYUrw+9ufkslR2jl0+dY8q6G"
yy$(061) = "mEjnZBgrHhwGAoRX3rTkOBPjnAyiATzl7evyRkVDSPvnxFzLChS4CiXJtfqViXD1ipeNK6Xl/IgdZp"
yy$(062) = "BGpXNFyKxTyVlB6goEzz6yEZPDOwqCNBg5cEX8VVFDqPs4fHtMllzK1ahvjx5yvXLtCCMf3LLspcbs"
yy$(063) = "J+Eq1CWf6nolyWGvGkx3lkZk9EEaduxNpCMuxJwTySNdcrPePhF6pSKANDj+IVsUja8iEV+REp/EDO"
yy$(064) = "VK/js7d4ZGvltOcDhiks+IwosUERgIdedHbxteZCJS+yBywCdSdSluDWSEvjdhIrC/Ujq32mDIoVR8"
yy$(065) = "ecavzHGI5PtoWURnWQRSTzHrXHlcDqEuSDuoR/3VxRnniiAaCqKiIWxcQUsVTDbALDli5SPaFQs8Zu"
yy$(066) = "C+3b82YgFX2mFsiniuno2qI+oOGVfqEUXK1b+qZyBDBNumYQa7pI8TkyvPR4u4SmNq4Q59tMK3DmyS"
yy$(067) = "pycgAv4RH0ehw2xEypXog2wxI0sz6+1cDngixxmtejXpG+dkqIL1fZPdLL2ijcQMoWVUqV4RflbLrP"
yy$(068) = "4TI6fDCsSLEflgIBCQD5qI1yfvaXpVT9f0smOpIUt1hjq8GHF+GvCk7Gzz3Hkg1kGGQDRfpAiLXl1c"
yy$(069) = "qOtgqLh3LTTVJQ/lN6l+OAffM0JprOTuZyJADoKKpNg1wwbTuqgFUVmsV2TkoYLw1ZDOJgfExVRm8k"
yy$(070) = "y3ZIRd0dz5dEdGQLkyRbBNHFdBaH0IPaJrOg3ac3UZMU/rDF3p22hjoZLYOtBrMli/HyoAx0AQ3ZVj"
yy$(071) = "uLzaBeLBVahI3r+fylHcjvlsw4qQZ6GSWD61PS/4jHBZZNVZyUCQ3XDaR12wRBpPBsX2BIsyZutUDt"
yy$(072) = "XzkxIeS+2EdrEz2wO2l+p0WZxDvXooEG6KoBrFuVIR+ZDu4wBCq8l09XNkTh72KW/vpVx9/khBzHkU"
yy$(073) = "zhXEQdhloc0DQNAjk6IkrOmV5JoaNDf0yu+zvyyIgcrSFoJiAUsG0xHrOU43Rax1i60XiCaDjoQg33"
yy$(074) = "5xrtg5a5i3Xc5VrI1qsCbkGLATMtD+Jz1KtaUhutE5R2zGaa6xCYeQKUKvyUhnB2U9gUzQKwPC0RGn"
yy$(075) = "n+wIrm5AGsyyoBoUZAg2lYPVGf6a6RXUrB6mCFJApixoXBlnIJlq16B4pSFYnUE6A5lDt/gQnE7w97"
yy$(076) = "RyJV32xSQvywi3IxC1UZ4gVnSEdgZS6GO+MqKlYz0fTM9+hJTeY37e57A47klypVxdcN8OhoRH8ZDt"
yy$(077) = "5mA1bKU7ODJXQndD9jIz0fCFf+crDWH6XUILRTr7OA2u2RCTlzU7oqPsdskIORZVzgcpVT4JkcYMde"
yy$(078) = "VNcjyoK8lFuie5s4ivNESL1wWN8wyGnfPGtyQauhwiFo2Z5LSvQp7VC+TgMgNBv5Ns0NsR8vVRXtrH"
yy$(079) = "DanMriL0OgNvf6zuDDLJCVehiK/k5mL3VlkzEMTt9ox9HIqQtm/UUYm4nV4rwRsUkLVAVCBOdQT54t"
yy$(080) = "oCRKJzRUeMHbuTwRXXq3T5QMQ7Ry8yuBLXmCgIPLStYwuC9MFrZLmG9m7sC1ekycEkd0+ZExjQd/OB"
yy$(081) = "lgYLBD/14p6yU2SbMdGlfNDM4IojnP6UCUIRSKmIyghxru+rl8RKXbQWk3npmlf/Un/VFOdkOIKc7v"
yy$(082) = "Od5zQgUE6YEr1KdVdcsE0ronG6Za+OCnG/6wfiXYxv7pWlrEJBtHnM4PNkkB5UOwh6GQhpH8ftivdg"
yy$(083) = "EaTBRWgghC+hW8hMW7ABOuY1+JSrEfftDak8D/tjrpbaca7S6znrcnkeG2BO/7xYB7lFtemFqLxovs"
yy$(084) = "cmwjwjLBAePUYLpz7g9ONBgIxm1Bm22HeWHc/i2FOE5K8IxAvpxpVrXo3Nr11K/RWNQum24NYHhpln"
yy$(085) = "79GTra40OtRHxW2QDyvEPnpTLW5qCPV0Wx2FTptcLtHOvt5Z7oMepS5vxOUN21XoFDH+UJWjjn30St"
yy$(086) = "QzxUaCqw/kOgNc2RP2Vta6FSuCjueW3EcAbje34utZszyql8o1Ji4xpw+7FhURiI5y3YSzBzo4PF6E"
yy$(087) = "oFEfRxCXbidKIPpSLErGVYxeZN2cgnI13NCuHyyruz0KgmqUvtaSSw4WjWps5DhxkHUvBJbjrJuIxX"
yy$(088) = "2oerW4ZeQ4Cxrl6oFZv8pGwJfP1i3XVC8c1bj6+ZJyTJ5+oV/DexUimS7bR8Jz52s0wtU/XXx3bzmu"
yy$(089) = "cB87y3F1hRyWXZlJJ1rAlbOXXo7LEVNkvc6LI9QkkSBmZGt7Kb1qLatXS+nuNezDQcqiwHz7zmJEzk"
yy$(090) = "B8/zYoO/lr3P9KWxN+I1whm+SHi7jKKSEUQxSu6EMu5lG9ai2SQ1upaI7TWsQVP+vOEUTbp1dxFWqj"
yy$(091) = "muQXSQ5NrzO87qwvifjvl1f1ITVmg4vkIJKn38auH7ySKwlxPa6i9D1arXUXzbnYlpQQ5/uLJfflm6"
yy$(092) = "2pGWRG85B+pih59sv1BXI4bXnOqQ068SLf7nwlpxN0HURoUR/1vVgyXep3y8EiOfLoeawhxrOFeuUh"
yy$(093) = "Y8d9OrtCr4zcYLa9eM67st1SG9w27r2jtLpc/6B6dUUfSmNrFNq5yj4aCmK8NOI6faijmpvnXrOb5f"
yy$(094) = "pBexN+jvn2+SK9Ik1MIrOP8ZVyCCNksWh5ScmZ+1kGMb46ZhBBOdUrdHXMkCLIoBzjvh/ZCDrn1ZMl"
yy$(095) = "5RiXDpdETAvLIuboZEmunsnXeV2JSKi6L2mDWIQltWRyOl0SMU1mSyJmD5bwJVSvdq62wRRBRrXzm/"
yy$(096) = "BXKuI6/urX7WO2jBy/Na7G6v0ZroGYJKSTY3FFyaLHIFmyvWEI4ksupYg8+0Hv8fsuqV8tcY/fWZXM"
yy$(097) = "4BJcTVeDZfVqXltWr2Z3l0VMr16jRKN6tTVfFF9pCNLH1hL5OdV2d1nvkxQGS9YZMGJJ3U2SZWMG+V"
yy$(098) = "rLqxGSIl8P8evltb8NxOBaCKpXC3NnHUH0ammuxtWdJeWYlpaW3LEg6gsRAwguJTmgz4fwxujZpd4z"
yy$(099) = "qyHnVBsERAmMbGSOlccMqg1WcaxSKtkRbBdAu0/RzfPTT3ZKb85PJ6N9OyLW2MV9TD8uJOdJa0huG2"
yy$(100) = "IgZnqdoQprCrw47BbsiHVtzrEIo/la8iwZHTwwJKfroKZXpaPL3tHaq9nl6GDHQJA+dL3Co9qf15I4"
yy$(101) = "GXWMPuz2gRHVpPKJk0y6rhVx/qSm+qubcGfI1Zpz2hoeWiVPikjvY74+X0UfJMOMGZwF2pyXcBgxX9"
yy$(102) = "/v4zmfGJITrmovNL06muLFsd+5HH3pGAiiVp98puvVFNKew2Q0NW4SSyOAOdkFkOUYJ4P1ZD+ZjA7t"
yy$(103) = "iG/vfqxxNZoPn+KOWz07V9PE6GM0c04ww8PndsRMz9VKyWhQ7eMXR7OCITmtXx3rXPUH90bf3bscPb"
yy$(104) = "5lIEgfSIsZqsn+YH2EVXp0OLRKPtfPt1eTw2nhDAeDk759zs/3tLvQ3kwOZ+4Z7iNTr0zdLSXdTeCq"
yy$(105) = "6y5CSJLP119N1zBsdPCRITnNnc90rt59NbuTYL06zPBXha6mV3gq5rWT/cSCYPUrLaYGRFJBWNfPTq"
yy$(106) = "0IIwq/CVHHKto8bY3s/mq6fVPjiiAiJxn27HZu1GRKePLm7+wg7EvMkxy0JqP7qy8fvDpaqz7CetUy"
yy$(107) = "EHRUur/CK+M8As94YCDstaUq3G6sgleuybBnRYy/1lcDjDhfxerZGrLbUup96OySPiBsHiYnVoRRhy"
yy$(108) = "tdwHnCXsm6qtFYtK/p1T/jP2v90uXoyFw5rbpLRlXzbuG1NmMdNOwDEE8qa5k2+PrbTzSusIdOfrUe"
yy$(109) = "J61RYpV8OtP9FSCG691kOC5ZEbNt3V/BqMbVGfZXhiYyvfqVxtWE3GQmuRx9Yz9TNNX7qMKtQMcu1q"
yy$(110) = "uuXXJTjhHRhGx/NdERNwHxTXknaZ1l+PZvbH3AvtZwZHhqylWo6xWwm3hYr9q3Dclp/QpZuLpoFy4t"
yy$(111) = "FpXh22E+pomL7fy+dVSmXsGoZjtnmCtDd6lvtyK+eQ/HV6P0AuHFfcCoZlUSX1n7mLV6Fr2aFXB8NT"
yy$(112) = "OjJTtXYIPJ/tHl6F/MKIN5Bk2vyM0OH4JeGVEGq4tuWXzJHHu4ycAux3miVVLBXyXf/An2V8fV2NpH"
yy$(113) = "ou2lVscgOV5Oh8c7VsR8qtVFSxA5zsA+zDiR1fpOLL595mKuDuwxQ9LV/RVx3cBVnMGVEVMTBFaVye"
yy$(114) = "unVoSR49wkdo+dbyvJ8FfVu1ofBDEBrlpWhOnbCQLk6JgWRc9A7poxw9El4cpug0ZNv0putl6w9pEV"
yy$(115) = "+xwyvcqI21l2kL53kyCcJDPHMeqipI8EkSmxIkzfDgWtYRfW83cMye2+HU6vdXs4Fo3temX6dlhkv9"
yy$(116) = "7EfXy7YZXc9O2AOL4D026P28eGb4f/gwv+w47Wa/uqZukDhzLDAxwzjDO42nqhc4UR42McU8f23Nn0"
yy$(117) = "V0fTfzia4gB1tJfBleGvIA2d3V7PtsEn2n3oq+S+/y5eBxN7LGr49pvJaZVk1K0zu+QW3cU5FLi9LB"
yy$(118) = "tkTZI8SXqkjJJlg8Z5Bpw748jqTbYNGn1UIaMHo1nCBpPpx4DItkGdK6hMnG6f49zZXjWw9TGnupVh"
yy$(119) = "gzaucIJzaa+wZHGV7F+eH10/FoXrqMllFi+yzmzrckCrI3v7Ha71LT4noyOW7uP/VV3091xdC/F7ro"
yy$(120) = "gcu9Jdu7IesczVhNws+aqHzNX12uD/AKbSw1k=:6C88"
yy$(121) = " "
yy$(122) = " "
yy$(123) = " "
yy$(124) = " "
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "
  	  goto label_info
map10:
      if mapnbr$ <> "10" then goto label_info
yy$(005) = " ~DGR:SSGFX000.GRF,30855,51,:Z64:eJztnc+PG0d2x6unGZK725nhIpc2lku21odcgqAnAhIqOy"
yy$(006) = "tyIiC+LJD8CeTOwcfMxECixdpiWxYoAxGG8c1IFtK/kGMOgd0WBU2AGFKOOQQrTuh4FsHC4kRAhgO1"
yy$(007) = "2KnfXfWqimRrd31yYURyOP1h9/f1q1evXnVTeV62zUsTGpIdnTQ3+IkVZIE2aioy3ww5/NqR19Dy9S"
yy$(008) = "Bcy/YaxCL/NRC/BMK1XElXIxb5UXkE9TdHmBYP3VyNmPL7aK8sglBzc4RqwSYONtdCkS2Ebq0+NzaL"
yy$(009) = "vYlQdUNEaPFS1OxvpkV2sSnqlEe8FQdmWCwlb6JaspkW2cWayC+PoA0RqcW7G6f2zb9/kZryY/JwHE"
yy$(010) = "/tSDizILR1PAeC0p5pMXLeK6hadyox5ftBhP2yGjqIxIKg7T4Kva1/cCF9axwje3CdmKFV/krkbxNn"
yy$(011) = "HPud1KXfFse8CfJuB30nYnOYk7r3QRDbt687fKyOnb9hRwKBnD8aqmd/MMKnx458ISx2mqpIY3+EvL"
yy$(012) = "XIHQ0hDz+2I4+ExZ6Phrk2ik2C+ZZdi3CYp00DuXliRe4dcuTJ9a6O3EY/ej+xIb8SPjZrdVUt3qiB"
yy$(013) = "ao0pflnpAeRt4TDTpoqEg4QE2qhCD11vbwpkflM7L336WG0h9McWJ2BIdqmefd4lg6MEvbAIsjkMi+"
yy$(014) = "G9+u7D350/BtvXI3d2kTwO0PPt/lNA5FNnPuYfo37wnt9/T0fCAyH/xbEqn0jxt9HpOB6gS5f8KRrm"
yy$(015) = "Zj5WQ4eN/wN7KZBI1UK8q4m85l/fPYyB23zrQyH/JIcI/ri9wzw5TBMN2fm5cJinBkJ29PwfEzbeKM"
yy$(016) = "d6XyBnhsUihP7iafiZkZ18Jiw2MxDc3hr/wWcIxoCew2Ls+Ht+ktpioEDg2acRk+io6IgMSv+qGZkc"
yy$(017) = "DD0Bxg4Q+kXfFceoB1giRv2eNfJjV65QLTg0JWCgDT1HquB/RR5x3/SU7T2R13DkqYZ4KX3Uj8mrxi"
yy$(018) = "qSebrFvCSQ2ZI4Mc13yN/lKLaARk4DlvxFSByc9+P4CXYY6WPTaAiQCrOzPEpyknCa7ksfmy27psPs"
yy$(019) = "CrnsuAYj8hmnwsfObMib/LnDHgejTqr4mBURcZweYDXerxA2lZE/A1rIaNU8JK4fspnGziSIf4noiW"
yy$(020) = "Va0kObxWLWW6ixr02CXWYHjsyhkYlhA57H+ExQ/UrSMxBVCz71VfrxfWplbxQO7pJ9ynzsxRgi+KO9"
yy$(021) = "CkNwdop8jIzI65k1uxB7Yac9Qgi/71ca+9vcd1bMKwMcGqgn7hFTYItxf2BasiOA+NTOKUO2cRaAkU"
yy$(022) = "MVWRh9nyiXid/O1P8fdLs+Ja/DAhnqWsIBFZRS+e8s9y5G4dspeetdbrFMj/y4NShCtsc/p+O92YgM"
yy$(023) = "E0jJLqwWY418di1KK40a/fWR22JsChsS+dUgfojls+lWYEtHuYXpD0W2gvjLID7pUO0yu7Cel7owWh"
yy$(024) = "39IUJjajD0ueiVp6O24TBdtk0PxdTB2tRgaCizC1SD8rGJSFqO5TeIgx13evT9QYH4sFc+4kbrEf8M"
yy$(025) = "e0ExaDDkYjEztZCt/4h6Z/xvakLLB77MQCJyFoPdhG40EYhfIMt3LAieL/C4lyBmC1TDPiTi2BHUgo"
yy$(026) = "+9heIKjQJV7i0kgsp09N4AIgHxgH6A+hWK8HeLVGGO3oII2ZL4ZES3/HMDybynFot56R2UUmQi5vN1"
yy$(027) = "OUvK3rE4DN6SZGXkxYmYNOFjXV3t+TY2Fd34tninVnfnYzQix+heSM7EVtgXx3ohg9IDK4JPTB0fHL"
yy$(028) = "aYQIo4Nn/DQFgLWNpTIEhYbBEDLV5I9pJiJVT+446C8MjfAYjfJBp6iJ0JT1qsQE7gNAEFouBBNx4F"
yy$(029) = "BnJ2v6traXi3qfwK5942tJwdAwT3SvJ5YuyXPlYgM7AXD9RvwhsGMm0A+Qm1VIooWkGzbxtaYHbBS2"
yy$(030) = "QcCeKZKOZtyZwf5DBV5vIirwqjHwhPrjWlW+oDn4+n+xXqM3y47Ig/VQ6cA19INmY/KEwLBA3sKVwF"
yy$(031) = "aQ1HgUbfsNiDhaqlxsIcyw2CGCvyI6FSRv5tLbuoUVf0m0cUIYmSLJfVBJJtIdXHqEX9/2Khoqrajt"
yy$(032) = "qSIf3qUNFCK12kRlI4TCrlc4st5p+q8nen/NM7K5CFZjEaVbVZtUQqwmKXmRZhbgjE5xuh70gtdS4f"
yy$(033) = "ID3Vi+lr02ILHXloFpQMiy1O9VRhP+Fa+lU2loVTQz5CWhc7SDmSGsisQLS9DMReesJiEhEWy5uaw3"
yy$(034) = "wgtYhu8p2+gtjC+Ps4VjdpDh5y3TW0BqFtiyHcUJEDkVp61VAWbpl89cBsXSwmG+hIugYp+mBDJGb/"
yy$(035) = "7kCkFlocT+rotpRvRn6AUL/6oHl0EmKnjpJNENrwlrc/5um133AgsnRzBW9YJVud05h2cNw4WiM/9E"
yy$(036) = "YN/w5BcM/xUWO/gkRCIpHsxRPNx3wxCcWPzGKDKkSQFsbTauzx836Y0iev8xOgZaEhcdqUlSReV/Zb"
yy$(037) = "v5foyFKS9C1kNL+taBH5mKKlTmdgYhuqpTJ7ArTok8RtmoIm/DVHspXIjnJEQY8h/rsGoswr6/77yn"
yy$(038) = "Ez+dVtxWN4dqHULgJ2LBH7RFbsqgV7EFHnlfqiCOvC1fqfQuRVrXAYX4nGOLqe0ufBCGrJnylIUvy1"
yy$(039) = "jxrv0F8Pjg1EcUu1ZDFFne8RpLK/bUdkHBPyd0L0mObh1eAq1JJdUxGasJCIiQP+Q1pUqAVXE4Aswg"
yy$(040) = "LZYiFMlmDIGamq61G83NEsfIz/kcwlQv57igaG/JmSj3kJn9eyJAFtdWYfWhA966NIT0aIH13sof0m"
yy$(041) = "RJ5/UvhYhRVeEk+Yu9rw76oVXz7l2SuQgC9xymi/1fBOgkOIzNQMNppSu6nR/qt637CYNooRFVO+G/"
yy$(042) = "Ias4O/h1rmKkLCKwn11NmcSKZ4Mk0O4CLPPoLIUvOxHntSRn/flK+6ZYUEl1RFGjbk+XIVQreDyJka"
yy$(043) = "x+ifU7kHIp+W8XUtMy3nJ5PutYieXYQ9Y1nQtJgaxusie9Ms1oda7ijVnlAsO2gWMxC90I2CCOwFmT"
yy$(044) = "52oUSYHn0U8mPEHMaQn69BwtkGiNiE9c03ZggiRkFV1+JNmlOoxSx0q0h4ZaKuXjNkkq9A/L2bW3zs"
yy$(045) = "UJCzlfJxqmBabDWCbEaeGYhs/jHdUQMi6NyqhVQF69Ex8THYX7K63WJv45SMxgI/uAkQnlpTZEtB0u"
yy$(046) = "L1uwDJDkayv/DYQrbe5pNEUn++D7UoDhMXCJ9XxqQmuXKsNBoZMvaVQdcYKxUtfflyImK71JIvXz63"
yy$(047) = "ID0FiSHCan0AUaZXieljZq0vZXl+lWqphoNRcS65w9zbNZCOQEKKdCCSGVW4LelVDVTBeW8IkOfLma"
yy$(048) = "YlSj78gXr8WP7nQItSUcSIHx6d/PxQpGTExzDyBCCThYJEZPFFX0JLEDoGyGmyL7Xsb3f+TOYjhcVa"
yy$(049) = "QMscfSCR2R0y5eELT3w+jpE3Eh15qY6VrO5aHBX1scp3AWL1MaVh+ROJGGOliTQY0i+HJEgs9gjk1c"
yy$(050) = "UvhqqWPsvKEtVi4XsCYpNjsDSgIzFDDp6sQnrgQiXiY439vKlaTFnmULT0NIsF8aP18nviCedjcdEx"
yy$(051) = "XYhSiiJb1qNi7LDOxPsMSZTJNVkYElM6+vH7FWgxxJCKimgWQ4bFYKNGU5BlPYJagoJMqAAo/5OXpR"
yy$(052) = "HFYuJAipcUiQqJLosV8tluic8gRctKpCqQexpyqV2poluMHSKxmIYoMz5TPuLy30xULWm8ASKisjUf"
yy$(053) = "s7So6JZmdoHUE1LpzD4mL72RXBwxswvie32JBDx3JT6mIzPrXpDilpXGXyIV0asKtNU5mUr5hSX4tm"
yy$(054) = "uQCkQyEzFaET94HHs2VOX3CvlCizHw5brFGOJtiuhNWgxWeyz9RcSTlD2x/FzRgkMfXH4KAWJMeZTs"
yy$(055) = "wtEsiG/K3z/u9KSWHtRyWmQXoTya/VHRE9ULMLiPmWt8+pYmomQXiA2tIe9rXL460TKyCweCIDIxVk"
yy$(056) = "Xr9qG5QE7RDSn/1LaZqWVerPCGOLsojkYaGUEkV7OLasdEfBPRfOw2XQ2hWhoTsxpvr8EePeTy45Nm"
yy$(057) = "tBGCd9HqC4vtfkkeE4AsTouBr8HE7ouRkQTwRENEqtDVkIQYQW5Dgl4FIuZeYAsAYumV3GKsm+gWcJ"
yy$(058) = "egXwNxNmOsFPL1aochP3vxdLgGgfJhdkEt5rwQWCAOLbHxQmjRF1MMpGdBaKG3nMWywmI2LYmlV16W"
yy$(059) = "R/TVN9Y0i/UQRBQt6h+Fq/S2zNXqf/mZRFKknmqOmGtJ7vNia+rkvVizIA9KuS/RSn/sHM6OVyMdA1"
yy$(060) = "E9WbYGcjSjVyZIld9nT6mhZR2insjfTK+0aFEHfeSQ7wmELw8F6xG4F+REpHx4NffUqcWJ9NYjzkhh"
yy$(061) = "IKYWeTWQqWVuuQx4DWK/chiedh3R62NUfm+1Fv0yYIbcKIzQW48ggNj2UiTwijt5x6u0FL1SRUbhKU"
yy$(062) = "MSC+KwWGMfvqMimhbqkT3Er6AQF5zqWsA1ijRSpEim+rEFcfdKS1YGfYzK90U1CEenG3egm8IuRiOF"
yy$(063) = "P+LX1/k02+9ZeuUrZXhlR6MGMRjQKHIJLEZuFyDr2rwFFvnz1ER2VyPZ8qZhMauvFMgrKF9EFeutVn"
yy$(064) = "aLOTa2I7Q5s2oDUeWzSM5PSGRqUZCYI4GKbOBjW2Iv9XV70Qc+2KRBmMMoU1HS+tq2ZJ0IKSu/VotN"
yy$(065) = "NaSxAhHyiY0V+ZM61SIXOXk+dqQg0U8kQuR7EzYPlcm/Uec3b6+8DQIaQwZJId871juIuNoQFiKUBT"
yy$(066) = "v/uMglIoYkFkS9igDewVjhFpMxTcwrhZaICw/lYzyh73igdPNUXnggZjoG4gNkelMgMb8bEa6koi1Q"
yy$(067) = "USxicpW7hyeqYTiYs4tBDvRilxwrPeHo/W2iuq8gonBlvwOCNn52PGEx2dyzJHGTWcymlqm1OsqMLB"
yy$(068) = "uPfvGEmILf1GBDtJbANxTk4gHX4onpJBbOvAu/Q14rQYdbLBAWSzq9qkDoRB4jU6S5ER/4wsIt+8q0"
yy$(069) = "y5MWUy47Ysh/Kxe1qmtApHOxsc+4IkK9ikCLWn2tmKYh9PIGjkTKn/HrCIZnnl2IKyLk/vnBJYgXq4"
yy$(070) = "05slwT93TEk4hx2cnUvOKOtQpZSAIHxidW6sCHjz8WTDSrs/K+OUnc5UjAEHF7CLl5xIEYPqacBmVo"
yy$(071) = "NhGlv7zBn7GfbMcAMcO4NhQHlhsG+eQd3jShtp4NKe7mkCvU/LmPzHoXi8mpivSQPo4lglWRfKHf/G"
yy$(072) = "VpBqJZLCn+KruOMa1ezBwWk5/tr0TU1pN7QQDJnsOzX4U7hFqW2rS6JxH7/fUWpGj29IdZbHafn32y"
yy$(073) = "/+JoQisHivZ9LoAFwLiQZSDq1R10L85puESWL21aHOkl75XG0sCKFnNPHpZFTsVVBKkmnza7xYr62I"
yy$(074) = "ukQPg0/NiKFJ48/EKZubMtjDumOSKuU9rLE/Z+E2SkUAvOLhiyFV5yBE+pmiuRE5GQND7iCMhaEwMZ"
yy$(075) = "iHUxb0doqfJCdF95VLQsZYHIrzFDNWRmktoRrFnsxZggMKRnIGeKj+0VG+K2ZX5fBgjjATeRRKphtA"
yy$(076) = "YRUSiRezFTjFjvYobHG6mc0St3R7sA6dGnvhu5chfcGc27Z2Qgi0L+WD+61Dw+EMYrqBboSN2BpEqv"
yy$(077) = "1L8epm7q5/mYMorRin0h3yBsY+Wd0gg5MnpG6xsgIsKkXAaC00NVC50lSOSxPCbrdzjw0KddOsv7bs"
yy$(078) = "NRKGPIXMktSWM3lib2r7BhWk4DDUnpfgYj+g0GDgTcl8RuResamZiKqNfBEjFhgh9bju8vERlsriF0"
yy$(079) = "Y9dXXvA4dl3bC1dkJ0SvhJlS6CXaHRymFrPY5d2xuIqKmHcKpxUXIer8V3OgJXBK+XVL0L921drZ2F"
yy$(080) = "jJLlYohbS1LxXZDJmX1TIfLkojedYtiSxy2z3vajPWXtcjZtK7GvEsGezi+mr5vBahasnqKxN44NQr"
yy$(081) = "M1jHPql39fRr4cxmWeBe6ZZb7gXuMj62vr+Yq2+rkXp5BLQNe6W6yLeh/K/BYkR+UvzKtIzLW8wrbT"
yy$(082) = "G2mFDKx7K18o1emf3vsLTF6PBSSsuL7ZUIGJgp8nK7W3YvC2/t2TcuCZh11yHGjWzL8hYL1zkM6qfy"
yy$(083) = "V161XoNsmcj6XhlFALF+20nRqjjZKBIaNku6cWcV4hEkBIiXrET0nJEhzTUx2a8o55KF8b0frkQOyX"
yy$(084) = "e46Uh2fbUnR6ijzK4ZcmvN2dfyP+YwwzU+FiFlena40V60xixWj7ol+wse+NplgxK7H7kUYmSwq9rm"
yy$(085) = "YTyCyEpPtu5lsV6LPJ28i83XW0xGMz6xKm2xfK7dWb+mMYeZrktITMQrnfQuW7N1EQaB68aX7Q0Q2Z"
yy$(086) = "jD7JRH9u+W1ZKXtxj2/tKevFxbT1ba5j6mIziIl+6Vw9L9hV3Y9ZtHehqyvG6twTra68ax5WvEMfBV"
yy$(087) = "DKvbb9ViOkKH8NIO87pIdLu+wY+akGzY6OQ4P2O3DTx772KDH4bU6Ez8kt1Ns+6H9codulhfSn6r/F"
yy$(088) = "h5vVXax4bt0j62KI/cHLbL+ljnVlkty/phWYvlaF52L/lkPiypBdwovRHCXbQ88vUUu37L9bGvB2Hz"
yy$(089) = "qnI+1i6NZDvlLbZ+Xqk0XrV+AJDO7LjjH+PHcOZYfZtX9I5M9kpreXmb3ENvQzIwihFkTq+PG+diPQ"
yy$(090) = "5oySITOV0Q04/vD+3ItJsbyITmzuPjHTsCLUZ+H9O8pq19+UmhJfMtSPt851WetwZ2i2VtE1liBL/X"
yy$(091) = "2rVbzBj4PBJ0L+9ie42vOSwGu5hHapmXk7OdfBw6LGZB5vn56RQjyGGxMzBLYgi5wak9e2DXUgMJiU"
yy$(092) = "du6T9fhDvYYZ5YkcyBXG9jxH5elq3YcBiMvMSWHP+ya9WyvN59AJG7+fvE+OPEbrHsVvdTiLSxF2HH"
yy$(093) = "HG87kLxtaGkvamS1sF3r2uXnz0zkn/6KIpOWHRkuIZJ10XJ4jpFp22GxBbRYNvSXXTyrGe+pXz9UaM"
yy$(094) = "nr4OtwFaR9Ykdg1RobK0+73VOMdB2efArmlV5+ni9+v03kXxlbteQXz2AXO8dddYyRlvmfaNgjv5fj"
yy$(095) = "Y/psjDPb1lXHXj41+gtWfvqMOIxLyxjMkihy+QiPCOO2A0EJPC+TPD//ooaRT+1IBitXHrle/fxLhL"
yy$(096) = "vYM/vZz25uQyPjlPb8DHfkVt1usezdNrQYQT7CztyK7RZbwsqVRyL+5XdzHGE6Z1Ytyz0rQrxofG1m"
yy$(097) = "RYz6GEOm+HDHvj2OZX/ThfIxck5OVvuuI/LDiZVHBrzz/ySd8+x+GeRzPD9vO8ZKoz5GkcXjatc9Vt"
yy$(098) = "7/KZRPkOnBjtNheAPIq+nB2OmWeICDFiMHenJzhuOYXUs2vA7k0yD1d3gMbc8ckf/WLYgQp/tW/haO"
yy$(099) = "Y47If31osVhOHeaf7Wcf+9gDCxI9xUjgCOPmXsjvCU4HxjuOXjk0tJDfU+Iww2ebWowY/WGY59+/dG"
yy$(100) = "QXfZiP0b2QxfX2xw6LwaoCkz8/wBEmNP6TFnvtgiGnU4LYe+UyiC0+lk9wOnTctfd93gwfw/+aQ/Df"
yy$(101) = "IDjDOJX/GP9rec7IDz2ZHBiZnreRY6yEy7XcYl13Buuw2GKHpKN/4kJs54Xkj+OKIx9jqTeIY/lj/G"
yy$(102) = "Y7sudjWClEyLXnD/GbraOlA0ngeSHnJMUH1rrqyvl9aDFyYPNx7s5gaT5iR5AjIaE1FQ25K4zsyGCX"
yy$(103) = "ZgJP7+1qkzDu6JW+fs8IG8QZ8uIrO/I98DXFHHlAcn7j/05x9cqZ0HLN0V/q0acAIffPLUgcu3ZhR1"
yy$(104) = "hTEVKeIhcwjN94vgrRkyvm/C3XLAkuczCEjMgtx7zSqPR69KD+o+aeV1oiP/6UZRMj46Y9jFsiP0Yy"
yy$(105) = "nyDm/0/livw4xCwPiFved2QXRhgnIpa7eNftV458zILgPTTfIgn8J3bk1tCGBAdkmuCwWNOcJX2Q56"
yy$(106) = "MD9yhmiWPEL2dXyVhpn1gZq28MOf3I3SuzmdFfiMdkIemVrjhmuGU+x6c/pLbeGCGVbI/EsZXjiz4T"
yy$(107) = "79IvpXP2SgtCspgT0sU27pU5zpFJZBr/cONeSaav+XOsxTVLsiEThmw+S6InBmtpX9ijpVULPjEnK6"
yy$(108) = "o9NgRbuSyy3MFaXo7v/wwemKOiGA5GDW/UGYzCfcfF+Qayqn1Tg/3GYt9Y7BuLZeUXH7Ofnt3f4Gee"
yy$(109) = "l27z/wdH+cPv:B287"
yy$(110) = " "
yy$(111) = " "
yy$(112) = " "
yy$(113) = " "
yy$(114) = " "
yy$(115) = " "
yy$(116) = " "
yy$(117) = " "	  
yy$(118) = " "
yy$(119) = " "
yy$(120) = " "
yy$(121) = " "
yy$(122) = " "
yy$(123) = " "
yy$(124) = " "
yy$(125) = " "
yy$(126) = " "
yy$(127) = " "
yy$(128) = " "
yy$(129) = " "
yy$(130) = " "
yy$(131) = " "
 
/* Variables for the label */
/* manually changed lines 140 and 141 for spacing */
label_info:
      yy$(132) = "^XA"
      yy$(133) = "^FT504,530^CI0^A0B,62,36^FDSeries ^FS"
      yy$(134) = "20^FT510,410^A0B,62,48^FR^FD"
      yy$(135) = "^FT573,745^A0B,34,37^FDCall Size:^FS"
      yy$(136) = "33^FT606,554^A0B,113,153^FR^FD"
      yy$(137) = "^FT657,745^A0B,34,37^FDR/O:^FS"
      yy$(138) = "^FT654,582^A0B,34,37^FDWidth^FS"
      yy$(139) = "^FT654,351^A0B,34,37^FDHeight^FS"
      yy$(140) = "^FT733,439^A0B,79,45^FD X ^FS"
      yy$(141) = "03^FT730,656^A0B,79,49^FR^FD"
      yy$(142) = "35^FT730,380^A0B,79,49^^FR^FD"
      yy$(143) = "^FT791,745^A0B,34,46^FDFrame:^FS"
      yy$(144) = "04^FT800,604^A0B,45,46^FR^FD"
      yy$(145) = "^FT842,472^A0B,34,46^FDSKU^FS"
      yy$(146) = "01^FT932,683^A0B,113,153^FR^FD"
      yy$(147) = "02^FO943,221^BY4^BUB,120,N,N,N^FR^FD"
      yy$(148) = "22^FT1099,560^A0B,42,58^FR^FD"
      yy$(149) = "23^FT1099,531^A0B,42,58^FR^FD"
      yy$(150) = "24^FT1099,502^A0B,42,58^FR^FD"
      yy$(151) = "25^FT1099,473^A0B,42,58^FR^FD"
      yy$(152) = "26^FT1099,444^A0B,42,58^FR^FD"
      yy$(153) = "27^FT1099,404^A0B,42,58^FR^FD"
      yy$(154) = "28^FT1099,375^A0B,42,58^FR^FD"
      yy$(155) = "29^FT1099,347^A0B,42,58^FR^FD"
      yy$(156) = "30^FT1099,318^A0B,42,58^FR^FD"
      yy$(157) = "31^FT1099,289^A0B,42,58^FR^FD"
      yy$(158) = "21^FT1099,636^A0B,31,44^FR^FD"
      yy$(159) = "32^FT1099,205^A0B,31,44^FR^FD"

/*   2931 increase size of production date line */  
/*   updated the date size                      */        
      yy$(160) = "^FT1148,731^A0B,37,18^FDProduction Date ^FS"
      yy$(161) = "15^FT1148,587^A0B,34,28^FR^FD"
      yy$(162) = "^FT1151,457^A0B,34,28^FDDept ^FS"
      yy$(163) = "16^FT1151,399^A0B,31,57^FR^FD"
      yy$(164) = "^FT1147,269^A0B,28,28^FDSeq # ^FS"
      yy$(165) = "17^FT1147,193^A0B,31,32^FR^FD"
	  yy$(166) = "^FO23,107^XGR:SSGFX000.GRF,1,1^FS"
	  yy$(167) = "^PQ2,0,1,Y^XZ"
	  if sc_nbr_lbl$ = "1" then yy$(167) = "^PQ1,0,1,Y^XZ" 

	  yy$(168) = "^XA"
      yy$(169) = "^IDR:SSGFX000.GRF^XZ"
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

REM            if twice% = 1% then goto L10000
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
         
		  if scnr$ = "Y" then goto L04000
		  
          if schema% = 1% then                                          ~
             msg$(1%) = "        The File (MFGLOWS) Already Exists.      "~
                          else                                             ~
             msg$(1%) = "        The File (NELOWS) Already Exists.       "
                                          /* (PAR003)             */
          msg$(2%) = "       P r o d u c t i o n   L a b e l s         "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
L04000:
          comp% = 16%
		  error% = 11%
		return
		
        error_prompt
           comp% = 2%
		   if scnr$ = "Y" then return 
		   
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
            dim1es, dim2es = 0.00       /* (AWD048)          */
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
			        
        REM *************************************************************~
            *   Caculate and set all NFRC data                          *~
            *************************************************************        
        NFRC_data
 
           init(" ") warehouse$, series$, style$, spacercode$,         ~
              igthickness$, pane1$, pane2$, pane3$, gridcode$,         ~
              gridsize$, gapfill1$, gapfill2$, framecode$, sashcode$,  ~
              lb_cpdnum$

           ufactor, sheat, vtranmit, U_Fac, SHGC = 0.00
           s = 0
           sc = 0
           nc = 0
           n = 0
           mapnbr$ = "0"

           call "AWDPLA64" (lb_part$, dt_sub_part$, ufactor$, sheat$,    ~
                            vtranmit$, cpdnumber$, warehouse$,           ~
                            series$, style$, spacercode$, igthickness$,  ~
                            pane1$, pane2$, pane3$, gridcode$, gridsize$,~
                            gapfill1$, gapfill2$, framecode$, sashcode$, ~
                            #10, #11, #12, #1, err%)

           err% = 0%
           lb_cpdnum$ = cpdnumber$
           convert ufactor$ to ufactor, data goto bad_ufactor

bad_ufactor:
           convert sheat$ to sheat, data goto bad_sheat

bad_sheat:
           convert vtranmit$ to vtranmit, data goto bad_vtranmit

bad_vtranmit:
           if ufactor > 0.00 then convert ufactor to lb_resu$,pic(0.00)
           if sheat > 0.00 then convert sheat to lb_resheat$, pic(0.00)
           if vtranmit > 0.00 then convert vtranmit to lb_resvisible$, pic(0.00)

           U_Fac = ufactor
           SHGC  = sheat
           gosub lookup_Door
           
REM lookupIsEnergy
           if U_Fac = 0.00 or SHGC = 0.00 then goto no_map

           if door% = 1% then goto isDoor

           if U_Fac <= 0.27 then energy_star% = 1% /* enable in 2016 */
           if U_Fac <= 0.30 and SHGC <= 0.40 then energy_star% = 1%
   
           if U_Fac =  0.30 and SHGC >= 0.42 then energy_star% = 1%
           if U_Fac =  0.29 and SHGC >= 0.37 then energy_star% = 1%
           if U_Fac =  0.28 and SHGC >= 0.32 then energy_star% = 1%
           
           if U_Fac <= 0.30 and SHGC <= 0.25 then energy_star% = 1%
           if U_Fac <= 0.40 and SHGC <= 0.25 then energy_star% = 1%
           goto setcodes

isDoor:
           if U_Fac <= 0.30 and SHGC <= 0.40 then energy_star% = 1%
           
setcodes:
           if door% = 0% then goto awindow

           if U_Fac > 0.30 or SHGC > 0.40 then goto copy_map   
   
           if U_Fac > 0.30 or SHGC >  0.25 then goto skip_ssc
           s = 1
           sc = 1
skip_ssc:
           if U_Fac >  0.30 or SHGC > 0.40 then goto skip_nnc
           nc = 1
           n = 1
           goto skip_nnc

awindow:                           
           if U_Fac <= 0.40 and SHGC <= 0.25 then s  = 1
           if U_Fac <= 0.30 and SHGC <= 0.25 then sc = 1
           if U_Fac <= 0.30 and SHGC <= 0.40 then nc = 1
           if U_Fac <= 0.27                  then n  = 1 /*enable in 2016 */
           if U_Fac  = 0.28 and SHGC >= 0.32 then n  = 1 /*enable in 2016 */
           if U_Fac  = 0.29 and SHGC >= 0.37 then n  = 1 /*enable in 2016 */
           if U_Fac  = 0.30 and SHGC >= 0.42 then n  = 1 /*enable in 2016 */
skip_nnc:
copy_map: 
           if n = 0 and nc = 0 and sc = 0 and s = 0 then mapnbr$ = "0"
           if n = 0 and nc = 0 and sc = 0 and s = 1 then mapnbr$ = "10"
           if n = 0 and nc = 0 and sc = 1 and s = 0 then mapnbr$ = "7"
           if n = 0 and nc = 0 and sc = 1 and s = 1 then mapnbr$ = "9"
           if n = 0 and nc = 1 and sc = 0 and s = 0 then mapnbr$ = "4"
           if n = 0 and nc = 1 and sc = 1 and s = 0 then mapnbr$ = "6"
           if n = 0 and nc = 1 and sc = 1 and s = 1 then mapnbr$ = "8"
           if n = 1 and nc = 0 and sc = 0 and s = 0 then mapnbr$ = "2"
           if n = 1 and nc = 1 and sc = 0 and s = 0 then mapnbr$ = "3"
           if n = 1 and nc = 1 and sc = 1 and s = 0 then mapnbr$ = "5"
           if n = 1 and nc = 1 and sc = 1 and s = 1 then mapnbr$ = "1"
no_map:		   
        return		   

        lookup_Door
          door% = 0%
          str(readkey$,1%,9%)   = "PLAN DOOR"
          str(readkey$,10%,15%) = str(lb_part$,1%,3%)
          read #4,key = readkey$, eod goto notDoor
             door% = 1%
        notDoor
        return
