        REM *************************************************************~
            *  Subroutine Name   - EWDPLS45                             *~
            *  Creation Date     - 04/08/99                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *  Last Mod's By     -                                      *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      production label.                    *~
            *                                                           *~
            *                      Print File  = MFGSUTR/NESUTR         *~
            *                      Script File = MFGSUTR/NESUTR         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLA45 - Generates the label format and data to print   *~
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
            *                                                           *~            
            * Vendors -  01 - Sutherland  X_SU                          *~
            *                                                           *~            
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *09/22/2020! CR2682 Wrong labels, chg to new size     ! RDB *~
            *************************************************************

        sub "EWDPLS45" (been_here%,      /* Zero (Only 1st Time)       */~
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
            readkey$50, desc$30,         /* GENCODES Lookup            */~
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
            part$25,                     /* For Bay Bow        (EWD007)*/~
            model$3,                     /* Sku Model                  */~
            sku_key$16,                  /* Sku Key                    */~
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
            yy$(130%)128,                /* Buffer                     */~
            xx$(130%)128,                /* Buffer                     */~
            l_model$12,                  /* UPC                        */~
            l_desc$70,                   /* label desc                 */~
            screen_code$1,               /* Part Screen Code           */~
            sc_sel$2,                    /* Screen Sel                 */~
            dspace$5,                    /* Description 1/1 or Div Lite*/~
            glslowe$10,                  /* Glass LOW E Description    */~
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
            apc$   = "(AWD) Generate Stock Labels             "
            pname$ = "EWDPLS45 - Rev: R1.00"

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
            * #5  ! MFGSUTR  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "MFGSUTR", varc, consec, recsize =   128


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

/*                if schema% = 1% then iend$ = fs$   */
                  iend$ = fs$                        /* CR2586 */

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
               file$    = "MFGSUTR"
               script$  = "MFGSUTR"

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */
SS_2:
               file$    = "NESUTR"
               script$  = "NESUTR"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:

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

            lb_config_txt3$ = "                 "      
            gosub calculate_day

            lb_opnsz$ = lb_opnsz$ & "          "
                                                       /* Exact Size 19*/
            lb_exsz$ = lb_exsz$ & "          "
            prod_desc$ = lb_info$(1%) & lb_info$(2%) & " " & lb_oth2$
                                                       /* lb_oth2$ New Descr */

            if len(lb_part$) < 19 then prod_desc$ = lb_txt$(1%) & lb_txt$(2%)

            lb_sku$ = lb_sku$ & "               "
                                                       
            lb_part$ = lb_part$ & "                "

            lb_part% = 0%                                
            lb_part% = len(lb_part$)                   /* Part Number Length */

                                        /* Continous Head Mull Test     */
          gosub lookup_seq
          gosub lookup_sku
          if sku% = 0% then goto end_process
          
          gosub lookup_sub_part      
          series$ = str(bcksubpt_rec$,169%,16%)
          style$  = str(bcksubpt_rec$,185%,10%)
          dt_sub_part$ = str(bcksubpt_rec$,48%,20%) 

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
          
          gosub find_color
          
          init (" ") dspace$ 
          for i% = 1 to 27 
            if str(l_desc$,i%,3%) = "1/1" then dspace$ = "1/1"
            if str(l_desc$,i%,3%) = "D/L" then dspace$ = "D/L"
            if str(l_desc$,i%,3%) = "LOW" then glslowe$ = "LOW E"  ~
                                          else glslowe$ = " "
          next i%   
            
/* No screen codes are 04567EKOVYZ */
          p% = 0%  :  withscreen% = 0%
          screen_code$ = str(lb_part$,11%,1%)
          p% = pos("04567EKOVYZ" = screen_code$)     
          if p% = 0% then withscreen% = 1%          
          
          if sc_sel$ = "1" then goto format_sutherld

format_sutherld:
REM   01 = upc
      lbl$(01%) = l_model$ & iend$
REM   02 = desc,1,20
      lbl$(02%) = str(l_desc$,1%,20%) & iend$
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
REM   18 = color
      lbl$(18%) = str(desc$,1%,15%) & iend$
REM   19 = spacing
      lbl$(19%) = iend$
      if dspace$ > " " then lbl$(19%) = dspace$ & iend$
REM   20 = with screen
      lbl$(20%) = iend$
      if withscreen% = 1% then lbl$(20%) = "With Screen" & iend$
REM   21 = lowe
      if glslowe$ > " " then lbl$(21%) = glslowe$ & iend$
REM   22 = Exact Width
      lbl$(22%) = eWidth$ & iend$
REM   23 = Exact Height
      lbl$(23%) = eHeight$ & iend$
REM   24 = desc,1,20
      lbl$(24%) = str(l_desc$,1%,20%) & iend$
REM   25 = color
      lbl$(25%) = str(desc$,1%,15%) & iend$
REM   26 = spacing
      lbl$(26%) = iend$
      if dspace$ > " " then lbl$(26%) = dspace$ & iend$
REM   27 = with screen 
      lbl$(27%) = iend$
      if withscreen% = 1% then lbl$(27%) = "With Screen" & iend$
REM   28 = desc,61,15
      if glslowe$ > " " then lbl$(28%) = glslowe$ & iend$
REM   29 = Sequence
      lbl$(29%) = lb_seq$ & iend$
REM   30 = Sequence
      lbl$(30%) = lb_seq$ & iend$
REM   31 = Exact Width
      lbl$(31%) = eWidth$ & iend$
REM   32 = Exact Height
      lbl$(32%) = eHeight$ & iend$

REM   33 = oSize
      lbl$(33%) = oWidth$ & " IN. x " & oHeight$ & " IN." & iend$
REM   34 = eSize
      lbl$(34%) = eWidth$ & " IN. x " & eHeight$ & " IN." & iend$
REM   35 = Opening size 
      lbl$(35%) = lb_opnsz$ & " IN. " & iend$

      goto L20700

/* possible future need */
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
  
L20700: 
/*      if schema% = 1% then gosub suthzpl_fmt   ~
                      else gosub sutherld_fmt           */                      
      gosub suthzpl_fmt                                 /* CR2586 */

      copy yy$() to xx$()
      lbl% = lbl% + 1%

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
          sku_key$ = "X_SU" & dt_sku$ & "   "
          if sc_sel$ = "01" then sku_key$ = "X_SU" & dt_sku$ & "   "

          read #7, key = sku_key$, using AWDSKUXR, model$, l_model$, part$,~
                                           l_desc$,   eod goto AWDSKUXR
              sku% = 1%

AWDSKUXR:     FMT POS(82), CH(03), POS(21), CH(12), POS(37), CH(25),   ~
                  POS(88), CH(70)
        return
                                                 
        find_color

            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "COLOR"
            str(readkey$,10%,15%) = str(lb_part$,4%,1%)
            read #1,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
L51010: return       

        calculate_day                                
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

sutherld_fmt                /* intermec printer format */
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
      yy$(38) = "20" & ibeg$ & "H32;f1;o1194,1107;c61;b0;h21;w21;d3,"
      yy$(39) = "21" & ibeg$ & "H33;f1;o1250,1107;c61;b0;h21;w21;d3,"
      yy$(40) = "22" & ibeg$ & "H34;f1;o1402,1107;c61;b0;h17;w17;d3,Exact Width: "
      yy$(41) = "23" & ibeg$ & "H35;f1;o1442,1107;c61;b0;h17;w17;d3,Exact Height: "
      yy$(42) = "24" & ibeg$ & "H36;f1;o1025,525;c61;b0;h21;w21;d3,"
      yy$(43) = "25" & ibeg$ & "H37;f1;o1082,525;c61;b0;h21;w21;d3,"
      yy$(44) = "26" & ibeg$ & "H38;f1;o1138,525;c61;b0;h21;w21;d3," 
      yy$(45) = "27" & ibeg$ & "H39;f1;o1194,525;c61;b0;h21;w21;d3,"
      yy$(46) = "28" & ibeg$ & "H40;f1;o1250,525;c61;b0;h21;w21;d3,"

      yy$(47) = ibeg$ & "H41;f1;o1869,1099;c61;b0;h21;w21;d3,Seq :" & iend$
      yy$(48) = "29" & ibeg$ & "H42;f1;o1869,993;c61;b0;h21;w24;d3,"
      yy$(49) = ibeg$ & "H43;f1;o1869,537;c61;b0;h21;w21;d3,Seq :" & iend$
      yy$(50) = "30" & ibeg$ & "H44;f1;o1869,413;c61;b0;h21;w24;d3,"

      yy$(51) = "31" & ibeg$ & "H45;f1;o1402,525;c61;b0;h17;w17;d3,Exact Width: "
      yy$(52) = "32" & ibeg$ & "H46;f1;o1442,525;c61;b0;h17;w17;d3,Exact Height: "
      
      yy$(53) = "35" & ibeg$ & "H47;f1;o1482,1107;c61;b0;h17;w17;d3,Fit Size: "
      yy$(54) = "35" & ibeg$ & "H48;f1;o1482,525;c61;b0;h17;w17;d3,Fit Size: "

      yy$(55) = ibeg$ & "D0"& iend$
      yy$(56) = ibeg$ & "R"& iend$
      yy$(57) = ibeg$ & "<SI>l13" & iend$
      yy$(58) = ibeg$ & "<ESC>E*,1<CAN>" & iend$
      yy$(59) = ibeg$ & "<RS>1<US>1<ETB>"& iend$
      yy$(60) = ibeg$ & "<xpml></page></xpml><xpml><end/></xpml>"

      return
      
suthzpl_fmt:                        /* Zebra ZPL format - CR2682 layout chg */
      init(" ") yy$()
      yy$(01)  = "^XA"
REM      yy$(02)  = "^SZ2^JMA"
REM      yy$(03)  = "^MCY"
REM      yy$(04)  = "^PMN"
REM      yy$(05)  = "^PW1127~JSN"
      yy$(02)  = "^SZ2"
      yy$(03)  = "^JMA"
      yy$(04)  = "^MCY"
      yy$(05)  = "^PMN~JSN"
      yy$(06)  = "^JZY"
      yy$(07)  = "^LH0,0^LRN^XZ"
      yy$(08)  = "^XA"
      yy$(09)  = "^FT197,421"
      yy$(10)  = "^CI0"
      yy$(11)  = "^A0N,34,46^FDSKU #^FS"
      yy$(12)  = "^FT159,489"
      yy$(13)  = "04^A0N,39,53^FR^FD"      /* SKU Nbr */
      yy$(14)  = "^FT70,891"
      yy$(15)  = "02^A0N,34,46^FR^FD"      /* Description 1 */
      yy$(16)  = "^FT70,942" 
      yy$(17)  = "18^A0N,34,46^FR^FD"      /* Desc 2 */
      yy$(18)  = "^FT70,992"
      yy$(19)  = "19^A0N,34,46^FR^FD"      /* Desc 3 */
      yy$(20)  = "^FT70,1043"
      yy$(21)  = "20^A0N,34,46^FR^FD"      /* Desc 4 */
      yy$(22)  = "^FT70,1217"
      yy$(23)  = "^A0N,28,38^FDExact Width:^FS"
      yy$(24)  = "^FT70,1267"
      yy$(25)  = "^A0N,28,38^FDExact Height:    ^FS"
      yy$(26)  = "^FT70,1318"
      yy$(27)  = "^A0N,28,38^FDFit Size:          ^FS"
      yy$(28)  = "^FT273,1216"
      yy$(29)  = "22^A0N,28,38^FR^FD"       /* Width */
      yy$(30)  = "^FT286,1267" 
      yy$(31)  = "23^A0N,28,38^FR^FD"       /* Height */
      yy$(32)  = "^FT223,1318"
      yy$(33)  = "35^A0N,28,38^FR^FD"       /* Fit Size */
      yy$(34)  = "^FT70,1423"
      yy$(35)  = "^A0N,34,46^FDDate:^FS"
      yy$(36)  = "^FT70,1474"
      yy$(37)  = "^A0N,34,46^FDLoad:^FS"
      yy$(38)  = "^FT70,1525"
      yy$(39)  = "^A0N,34,46^FDDept:^FS"
      yy$(40)  = "^FT70,1579"
      yy$(41)  = "^A0N,34,46^FDSeq :^FS"
      yy$(42)  = "^FT184,1423"
      yy$(43)  = "06^A0N,34,46^FR^FD"    /*11/01/2020*/
      yy$(44)  = "^FT184,1474"
      yy$(45)  = "08^A0N,34,46^FR^FD"    /* Load */
      yy$(46)  = "^FT184,1525"
      yy$(47)  = "10^A0N,34,46^FR^FD"    /* Dept */
      yy$(48)  = "^FT184,1576"
      yy$(49)  = "29^A0N,34,46^FR^FD"    /* Sequence Nbr */
      yy$(50)  = "^FT70,1677"
      yy$(51)  = "12^A0N,34,46^FR^FD"    /* Customer Name */
      yy$(52)  = "^FT70,1779"
      yy$(53)  = "^A0N,34,46^FDP.O. #^FS"
      yy$(54)  = "^FT83,1830"
      yy$(55)  = "14^A0N,34,46^FR^FD"    /* PO Nbr */
      yy$(56)  = "^FT70,1931"
      yy$(57)  = "^A0N,34,46^FDORDER & LINE^FS"
      yy$(58)  = "^FT70,1982"
      yy$(59)  = "16^A0N,34,46^FR^FD"    /* Sales order & Line */
      
/* Side 2 */
      yy$(60)  = "^FT730,421"
      yy$(61)  = "^A0N,34,46^FDSKU #^FS"
      yy$(62)  = "^FT692,490"
      yy$(63)  = "04^A0N,39,53^FR^FD"      /* SKU Nbr */
      yy$(64)  = "^FT616,891"
      yy$(65)  = "02^A0N,34,46^FR^FD"      /* Desc 1 */
      yy$(66)  = "^FT616,942"
      yy$(67)  = "18^A0N,34,46^FR^FD"      /* Desc 2 */
      yy$(68)  = "^FT616,992"
      yy$(69)  = "19^A0N,34,46^FR^FD"      /* Desc 3 */
      yy$(70)  = "^FT616,1043"
      yy$(71)  = "20^A0N,34,46^FR^FD"      /* Desc 4 */
      yy$(72)  = "^FT616,1217"
      yy$(73)  = "^A0N,28,38^FDExact Width:^FS"
      yy$(74)  = "^FT616,1268"
      yy$(75)  = "^A0N,28,38^FDExact Height:    ^FS"
      yy$(76)  = "^FT616,1318"
      yy$(77)  = "^A0N,28,38^FDFit Size:          ^FS"
      yy$(78)  = "^FT819,1217"
      yy$(79)  = "22^A0N,28,38^FR^FD"       /* Width */
      yy$(80)  = "^FT831,1267"
      yy$(81)  = "23^A0N,28,38^FR^FD"       /* Height */
      yy$(82)  = "^FT768,1318"
      yy$(83)  = "35^A0N,28,38^FR^FD"       /* Fit Size */
      yy$(84)  = "^FT616,1424"
      yy$(85)  = "^A0N,34,46^FDDate:^FS"
      yy$(86)  = "^FT616,1474"
      yy$(87)  = "^A0N,34,46^FDLoad:^FS"
      yy$(88)  = "^FT616,1525"
      yy$(89)  = "^A0N,34,46^FDDept:^FS"
      yy$(90)  = "^FT616,1576"
      yy$(91)  = "^A0N,34,46^FDSeq :^FS"
      yy$(92)  = "^FT730,1424"
      yy$(93)  = "06^A0N,34,46^FR^FD"         /* 11-01-2020 */
      yy$(94)  = "^FT730,1474"
      yy$(95)  = "08^A0N,34,46^FR^FD"         /* Load */
      yy$(96)  = "^FT730,1525"
      yy$(97)  = "10^A0N,34,46^FR^FD"         /* Dept */
      yy$(98)  = "^FT730,1576"
      yy$(99)  = "29^A0N,34,46^FR^FD"         /* Sequence Nbr */
      yy$(100) = "^FT616,1677"
      yy$(101) = "12^A0N,34,46^FR^FD"         /* Customer Name */
      yy$(102) = "^FT616,1779"
      yy$(103) = "^A0N,34,46^FDP.O. #^FS"
      yy$(104) = "^FT628,1830"
      yy$(105) = "14^A0N,34,46^FR^FD"          /* PO Nbr */
      yy$(106) = "^FT616,1931"
      yy$(107) = "^A0N,34,46^FDORDER & LINE^FS"
      yy$(108) = "^FT616,1982"
      yy$(109) = "16^A0N,34,46^FR^FD"          /* Sales order & Line */
      
      yy$(110) = "^FO121,571"
      yy$(111) = "01^BY3^BCN,129,N,N^FR^FD>;"   /* UPC Barcode */
      yy$(112) = "^FT165,727"
      yy$(113) = "01^A0N,28,38^FR^FD"           /* UPC Nbr */
      yy$(114) = "^FO654,571"
      yy$(115) = "01^BY3^BCN,129,N,N^FR^FD>;"   /* UPC Bardcode */
      yy$(116) = "^FT697,726"
      yy$(117) = "01^A0N,28,38^FR^FD"           /* UPC Nbr */
      yy$(118) = "^FT70,1094"
      yy$(119) = "21^A0N,34,46^FR^FD"           /* Desc 5 */
      yy$(120) = "^FT616,1094"
      yy$(121) = "21^A0N,34,46^FR^FD"           /* Desc D */
      yy$(122) = "^PQ1,0,1,Y"
      yy$(123) = "^XZ"      
 
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
             msg$(1%) = "        The File (MFGSUTR) Already Exists.      "~
                          else                                             ~
             msg$(1%) = "        The File (NESUTR) Already Exists.       "
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
