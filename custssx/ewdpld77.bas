        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLD77 - Non Energy Star           *~
            *  Creation Date     - 03/12/2022                           *~ 
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *  Modifications By  -                                      *~
            *                                                           *~
            *  Description       - Write records to a file to print the *~
            *                      new Energy Star Labels with special  *~
            *                      instructions to DO NOT APPLY. CR3059 *~
            *                                                           *~
            *                      Print File  = MFENERGY               *~
            *                      Script File = MFENERGY               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLD77 - Generates the label format with Do Not Apply   *~
            *            across the label with the sequence and sales   *~
            *            number at the bottom of label.                 *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                                                           *~
            *          - Special Data File with label format            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/21/22 ! Original - Copied & Mod (sub) EWDPLC77.  ! RDB *~
            *************************************************************

        sub "EWDPLD77" (been_here%,      /* Zero (Only 1st Time)       */~
                        lb_brand$,       /* Brand Name                 */~
                        lb_series$,      /* Series Number              */~
                        lb_style$,       /* Vinyl Window Style         */~
                        lb_style2$,      /* Vinyl Window Style         */~
                        lb_glass_op$,    /* Glass Option               */~
                        lb_glass_op1$,   /* Glass Option 2             */~
                        lb_resu$,        /* Resident U-Factor          */~
                        lb_nonresu$,     /* Non-Resident U-Factor      */~
                        lb_resheat$,     /* Resident Heat Coeff        */~
                        lb_nonresheat$,  /* Non-Resident Heat Coeff    */~
                        lb_resvisible$,  /* Resident Visible Trans(EWD002)*/~
                        lb_nonresvisible$,/* Non-Resident Visible Trans*/~
                        lb_dp$,          /* Design Pressure            */~
                        lb_width$,       /* Window Width               */~
                        lb_height$,      /* Window Height              */~
                        lb_seq$,         /* Sequence Number            */~
                        lb_dep_so$,      /* Department Sales Order     */~
                        lb_ld_mod$,      /* Load Model Number          */~
                        lb_pd_dte$,      /* Production Date            */~
                        lb_zone$,        /* Wind Zone            EWD006*/~
                        lb_pressure1$,   /* Positive Pressure    EWD006*/~
                        lb_pressure2$,   /* Negitive Pressure    EWD006*/~
                        lb_txt1$,        /* No Grid,5/8,3/4,1,3/8PAR000*/~
                        lb_txt2$,        /* Dual Glazed          EWD009*/~
                        lb_barcode$,     /* Prod. Barcode        EWD012*/~
                        lb_thick$,       /* Glass Thickness      EWD013*/~
                        part$,           /* Part                       */~
                        sub_part$,       /* Part                       */~
                        lb_cpdnum$,      /* (AWD021) CPD Number        */~
                        #5,              /* (MFENERGY)                 */~
                        #2,              /* (AWDSKUXR)           AWD016*/~
                        #4,              /* (GENCODES)           AWD017*/~
                        lb_fl$,          /* Florida Approval Code      */~
                        lb_tdi$,         /* (AWD022) TDI Number     */~
                        energy_star%,    /* (AWD018) which region      */~
                        door%,           /* (AWD022) mod for door map  */~
                        stc$,            /* (AWD024) mod for door map  */~
                        ctrlflag$,       /* CR9999 control backfeed    */~
                        error%)          /* Return Code                */

REM         a$80, b$80,                  /* Print Lines for Label      */
        dim                                                              ~
            a$100, b$100,                  /* Print Lines for Label      */~
            lbl$(60%)65,                 /* Label Data Array           */~
            sku_key$45,                  /* Label Data Array           */~
            fs$3                         /* End of Lbl Records         */

        dim lb_brand$10,                 /* Brand Name                 */~
/*          lb_series$12,                   Series Number              */~
/*CR1251*/  lb_series$24,                /* Series Number              */~
            lb_style$24,                 /* Vinyl Window Style         */~
            lb_style2$24,                /* Vinyl Window Style         */~
            lb_glass_op$24,              /* Glass Option               */~
            lb_glass_op1$24,             /* Glass Option 2             */~
            lb_resu$4,                   /* Resident U-Factor          */~
            lb_nonresu$4,                /* Non-Resident U-Factor      */~
            lb_resheat$4,                /* Resident Heat Coeff        */~
            lb_nonresheat$4,             /* Non-Resident Heat Coeff    */~
            lb_resvisible$4,             /* Resident Visible (EWD002)  */~
            lb_nonresvisible$5,          /* Non-Resident Visible(EWD002)*/~
            lb_dp$2,                     /* Design Pressure            */~
            lb_width$7, lb_x$4,          /* Window Width               */~
            lb_height$6,                 /* Window Height              */~
            lb_seq$16,                   /* Sequence Number            */~
            lb_dep_so$24,                /* Department Sales Order     */~
            lb_ld_mod$24,                /* Load Model Number          */~
            lb_pd_dte$24,                /* Production Date            */~
            lb_zone$3,                   /* Wind Zone          (EWD006)*/~
            lb_pressure1$6,              /* Positive Pressure  (EWD006)*/~
            lb_pressure2$6,              /* Negitive Pressure  (EWD006)*/~
            lb_txt1$10, lb_txt2$10,      /* Grid or Dual Glazed(EWD009)*/~
            lb_barcode$18,               /* Prod. Barcode      (EWD012)*/~
            prt_barcode$21,              /* Formatted Barcode  (EWD012)*/~
            lb_thick$5,                  /* Glass Thickness    (EWD013)*/~
            low$(950%)100,               /* Lowe's Label format        */~
            yy$(950%)100,                /* Label format               */~
            xx$(950%)100                 /* Buffer                     */
            
            
        dim lb_fl$12,                    /* Florida Approval Code      */~
            lb_tdi$12                    /* TDI Number                 */    

        dim part$25, sub_part$20
        dim gen_key$24, gen_data$30
        dim stc$3, oitc$2                /*<AWD024>                    */
        dim ctrlflag$1                   /* CR99999                    */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                            /* (PAR001) */
            apc$   = "(AWD)Create Energy Star Labels 03/21/22"
            pname$ = "EWDPLD77 - Rev: R7.00"

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
            * #5  ! MFENERGY ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            init(" ") axd$, rslt$()
            nbr_lines% = 0%
            fs$ = "^FS"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            init(" ") xx$()

            if been_here% > 0% then goto L01000     /* (EWD010)         */
               gosub load_label

L01000:
/* (AWD018)  240 lower region glass */
            if energy_star% <> 3% then goto check_lowes

            if ctrlflag$ = "N" then goto skipctrl
             low$(002%) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */ 
skipctrl:
            copy low$() to xx$()
        lowes% = 1%
            been_here% = been_here% + 1%
            gosub begin_process
            goto exit_sub

check_lowes:
             /* if Lowe's use low$ else yy$  */
/*  #2  AWD016
 sku#       1- 16 ch(16) = key0
 upc#      17- 36 ch(20) = key1
 part      37- 61 ch(25) = key2
 sub part  62- 81 ch(20)
 model     82- 87 ch(06)
 desc      88-122 ch(35)
filler    123-160 ch(38)
*/
          init(" ") sku_key$, low_sku$
          str(sku_key$,01,25) = part$
REM         str(sku_key$,26,20) = sub_part$
          str(sku_key$,26,11) = sub_part$
          read #2,key 2 = sku_key$, using AWDSKUXR, low_sku$, eod goto copy_reg
AWDSKUXR:   FMT POS(05), CH(11)
            if ctrlflag$ = "N" then goto skipctr2
             low$(002%) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
             low$(010%) = "^LL813^XB"  
skipctr2:
            copy low$() to xx$()
            lowes% = 1%
            been_here% = been_here% + 1%
            gosub begin_process
            goto exit_sub

copy_reg:
            if ctrlflag$ = "N" then goto skipctr3
             yy$(002%) = "^XA^EG^XB^XZ"             /* CR1051 suppress start */
             yy$(010%) = "^LL813^XB"    /* chgit */
skipctr3:
            copy yy$() to xx$()
            lowes% = 0%
            been_here% = been_here% + 1%            /* (EWD010)         */
            gosub begin_process
            goto exit_sub

        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process
        
            init (" ") lbl$()
                                               /* Bramd Name       (10)*/
            lbl$(01%) = lb_brand$              & fs$
            if str(lb_brand$,1%,10%) = "WindowNati"  then ~
                 lbl$(01%) = "WindowNation" & fs$
            lb_brand_test$ = lb_brand$   /* pwww T E M P  */
                                               /* Series Number    (12)*/
            lbl$(02%) = lb_series$             & fs$
            lb_series_test$ = lb_series$ /* pwww T E M P  */
                                               /* Vinyl Window Sty (24)*/
            lbl$(03%) = lb_style$              & fs$
            lb_style_test$ = lb_style$   /* pwww T E M P  */
                                               /* Glass Option     (24)*/
            lbl$(04%) = lb_glass_op$           & fs$
                                               /* Res U-Factor     (04)*/
                                               /* (AWD014)             */
REM            if str(lb_resu$,2%,1%) <> "N" then                          ~
                          lbl$(05%) = "0" & str(lb_resu$,2%,3%)    & fs$~
                  else lbl$(05%) = str(lb_resu$,2%,3%) & fs$
/* (AWD015) */
            if str(lb_resu$,2%,1%) <> "-" then                          ~
                          lbl$(05%) = "0" & str(lb_resu$,2%,3%)    & fs$~
                  else lbl$(05%) = str(lb_resu$,2%,3%) & fs$

                                               /* Non-Res U-Factor (04)*/
            lbl$(06%) = str(lb_nonresu$,2%,3%) & fs$
            lb_nonresu_test$ = lb_nonresu$     /* pwww T E M P  */
                                               /* Res Heat Coeff   (04)*/
                                               /* (AWD014)             */
REM            if str(lb_resheat$,2%,1%) <> "N" then                       ~
                          lbl$(07%) = "0" & str(lb_resheat$,2%,3%) & fs$~
                  else lbl$(07%) = str(lb_resheat$,2%,3%) & fs$

/* (AWD015) */
            if str(lb_resheat$,2%,1%) <> "-" then                       ~
                          lbl$(07%) = "0" & str(lb_resheat$,2%,3%) & fs$~
                  else lbl$(07%) = str(lb_resheat$,2%,3%) & fs$
            lb_resheat_test$ = lb_resheat$    /* pwww T E M P  */

                                               /* Non-Res Heat Coef(04)*/
            lbl$(08%) = str(lb_nonresheat$,2%,3%) & fs$
                                               /* Design Pres Big  (02)*/
            lbl$(09%) = lb_dp$                  & fs$
            lb_dp_test$ = lb_dp$              /* pwww T E M P  */
                                               /* Design Pres Small(02)*/
            lbl$(10%) = lb_dp$ & "."
                                               /* Width an Height  (07)*/
            lb_x$ = hex(20) & "X" & hex(20) & hex(20)
            lbl$(11%) = lb_width$ & hex(22) & lb_x$ & lb_height$&hex(22)
            if len(lb_width$) < 2 then init(" ") lbl$(11%)
            if len(lb_width$) < 2 then lbl$(11%) = " "

                                               /* (12) Avail           */
                                               /* Seq Number       (16)*/
            lbl$(13%) = lb_seq$                 & fs$
                                               /* Dept and S.O.    (24)*/
            lbl$(14%) = lb_dep_so$              & fs$
                                               /* Load and Model   (24)*/
            lbl$(15%) = lb_ld_mod$              & fs$
                                               /* Production DTE   (24)*/
            lbl$(16%) = lb_pd_dte$              & fs$
                                               /* Glass Option 2       */
            lbl$(17%) = lb_glass_op1$           & fs$
                                               /* cpdnum  AWD021       */
            lbl$(47%) = lb_cpdnum$              & fs$
                                               /* (EWD002) - Vis Trans */
                                               /*   Res Visible    (03)*/
                                               /* (AWD014)             */
REM            if str(lb_resvisible$,2%,1%) <> "N" then                    ~
                       lbl$(18%) = "0" & str(lb_resvisible$,2%,3%) & fs$~
                else lbl$(18%) = str(lb_resvisible$,2%,3%) & fs$

/* (AWD015) */
            if str(lb_resvisible$,2%,1%) <> "-" then                    ~
                       lbl$(18%) = "0" & str(lb_resvisible$,2%,3%) & fs$~
                else lbl$(18%) = str(lb_resvisible$,2%,3%) & fs$

                                               /* (EWD002) - Vis Trans */
                                               /*   Non-res Visible(03)*/
            lbl$(19%) = str(lb_nonresvisible$,1%,4%) & fs$

                                               /* Text Line (1)        */
            lbl$(20%) = "This window has been tested in accordance with" ~
                                                      & fs$
                                               /* Text Line (2)        */
            lbl$(21%) = "AAMA/NWWDA 101 / I.S. 2-97, and has a Design  " ~
                                                      & fs$
                                               /* Text Line (3)        */
            lbl$(22%) = "Pressure of         Applies to windows up to  " ~
                                                      & fs$
                                               /* Text Line (4)        */
            lbl$(23%) = str(lbl$(11%),1%,17%) &"         in size.      " ~
                                                      & fs$
            str(lbl$(22%),14%,4%) = str(lbl$(10%),1%,4%)  /* Design Small */

                                              /* Wind Zone (EWD006)    */
                                              /* Wind Text (1)         */
            lbl$(24%) = "This fenestration product complies with the NEW" & fs$
                                              /* Wind Text (2)         */
            lbl$(25%) = "FLORIDA BUILDING CODE for residential buildings" & fs$
                                              /* Wind Text (3)         */
            lbl$(26%) = "with a mean roof height of 30 ft. or less," & fs$
                                              /* Wind Text (4)         */
            lbl$(27%) = "Exposure "& hex(22)&"B"&hex(22)&" and Wall Zone " ~
                         &hex(22)&"5."&hex(22)  & fs$
                                              /* Wind Zone (EWD007)     */
            lbl$(28%) = lb_zone$ & fs$
                                              /* Wind Pos/Neg Pressure  */
                                              /* (EWD007)               */
            lbl$(29%) = lb_pressure1$ & " / " & lb_pressure2$ & fs$
                                              /* Note may have Special  */
                                              /* Case Blank Lines       */
            if lb_pressure1$ = "      " and lb_pressure2$ = "      " then ~
          lbl$(29) = fs$
                                              /* (EWD009)               */
            lbl$(30%) = lb_txt1$ & fs$        /* No Grid, 5-8 or 1      */

            lbl$(31%) = lb_txt2$ & fs$        /* Dual Glazed            */
                                              /* (EWD009)               */
                                              /* (EWD012) Prod Barcode  */
           prt_barcode$ = str(lb_barcode$,1%,8%) & "-" & str(lb_barcode$,9%,2%) ~
                          & "-" & str(lb_barcode$,11%,4%) & "-"                 ~
                          & str(lb_barcode$,15%,4%)

           lbl$(32%) = prt_barcode$  & fs$
                                              /* (EWD012)               */
/* (AWD018) */
           trip% = 0%
           init(" ") gen_key$
           str(gen_key$,1%,9%)  = "PLANTRIPL"
           str(gen_key$,10%,2%) = str(part$,5%,2%)
           read #4, key = gen_key$, eod goto no_triple
             trip% = 1%
             lb_txt2$ = "TrplGlazed"
             lbl$(31%) = lb_txt2$ & fs$        /* Dual Glazed            */              
no_triple:
/* (\AWD018) */                                              
/* <AWD017> */
        gosub check_model

        s = 0
        sc = 0
        nc = 0
        n = 0
REM        d = 0
REM        door = 0
        U_Fac = 0.00
        SHGC  = 0.00
        map$ = "0"
        convert str(lb_resu$,2%,3%) to U_Fac, data goto bad_U
bad_U:
        convert str(lb_resheat$,2%,3%) to SHGC, data goto bad_H
bad_H:

/* (AWD022) */
        if door% = 0% then goto window

/*      if U_Fac > 0.32 or SHGC > 0.30 then goto copy_map  */
        if U_Fac > 0.30 or SHGC > 0.40 then goto copy_map  /*AWD025 */
REM     if U_Fac <= 0.32 and SHGC <= 0.30 then n  = 1.....
/*      s  = 1       AWD025                                        ~
        sc = 1                                                     ~
        nc = 1                                                     ~
        n  = 1                                                     ~
        map$ = "1"                      AWD025 */
/*AWD025 + */
        if U_Fac >  0.30 or SHGC >  0.25 then goto skip_ssc
        s = 1
        sc = 1
skip_ssc:
        if U_Fac >  0.30 or SHGC > 0.40 then goto skip_nnc
        nc = 1
        n = 1
        goto skip_nnc
/*      goto copy_map    */
/*AWD025 - */

window:                           /* SR67154 */
/*      if U_Fac <= 0.60 and SHGC <= 0.27 then s  = 1~
        if U_Fac <= 0.35 and SHGC <= 0.30 then sc = 1~
        if U_Fac <= 0.32 and SHGC <= 0.40 then nc = 1 */
/*      if U_Fac <= 0.30                  then n  = 1   disable in 2016 */
/*      if U_Fac  = 0.31 and SHGC >= 0.35 then n  = 1   disable in 2016 */
/*      if U_Fac  = 0.32 and SHGC >= 0.40 then n  = 1   disable in 2016 */
/*AWD025 SR67154 + */
        if U_Fac <= 0.40 and SHGC <= 0.25 then s  = 1
        if U_Fac <= 0.30 and SHGC <= 0.25 then sc = 1
        if U_Fac <= 0.30 and SHGC <= 0.40 then nc = 1
        if U_Fac <= 0.27                  then n  = 1 /*enable in 2016 */
        if U_Fac  = 0.28 and SHGC >= 0.32 then n  = 1 /*enable in 2016 */
        if U_Fac  = 0.29 and SHGC >= 0.37 then n  = 1 /*enable in 2016 */
        if U_Fac  = 0.30 and SHGC >= 0.42 then n  = 1 /*enable in 2016 */
skip_nnc:
copy_map: 
        map$ = "11"

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
                                           /* (EWD011)                   */
REM        if nbr_lines% < 404% then goto skip_data      /* chgit */

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
                                           /* (EWD010)                   */
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)

        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop

        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */
        goto read_loop

    end_process
        if nbr_lines% = 0% then error% = 8%
        return

        pgmchk
            convert nbr_lines% to nbrl$, pic(###0)
            call "SHOSTAT" ("line " & nbrl$ & xx$(nbr_lines%)) : STOP
        return
        
        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(80)

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

        end

/* <AWD017> */
check_model

            str(lbl$(33%),1%,29%)  = "FL Prd Approval: " & lb_fl$ 
            str(lbl$(33%),30%,30%) = "       TDI: " & lb_tdi$ & fs$ 
            
                                    
            lbl$(34%) = "Glazing complies with ASTM E 1300" & fs$

            init(" ") gen_data$
            gen_key$ = "ELLISON05" & str(part$,1,3) & "00"          
            
            read #4, key >= gen_key$, using GENCODE, gen_key$,            ~
                                                gen_data$, eod goto no_code
            if str(part$,1,3) <> str(gen_key$,10,3) then goto no_code
/*IM8005*/  goto yes_code
            
no_code:    pp% = pos(lb_series$ = " ")
            gen_key$ = "FLORIDACD" & str(lb_style2$,1%, 10%) /*IM8008*/
            goto lookup_flcd   /*IM8008*/

yes_code:   x = 0.0
            y = 0.0
            for l = 1 to 16
               if str(gen_data$,l,1) = " " and x > 0 then y = l
               if str(gen_data$,l,1) = " " and x = 0 then x = l
               if x > 0 and y > 0 then l = 16
            next l
            gen_key$ = "FLORIDACD" & str(gen_data$,x+1,y - x)

lookup_flcd
            init(" ") gen_data$
REM         if lb_fl$ = " " then goto no_code
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                                gen_data$, eod goto no_flcode
GENCODE:    FMT CH(24), CH(30)
no_flcode:
            x = 1
            y = 1
            for l = 15 to 1 step -1
              x = l
              if str(gen_data$,l,1) > " " then l = 1
            next l
            for l = 30 to 16 step -1
              y = l - 15
              if str(gen_data$,l,1) > " " then l = 16
            next l

            lbl$(36%) = str(gen_data$,01,x) & " Glazing" & fs$   
            lbl$(35%) = str(gen_data$,16,y) & " Glazing" & fs$
            if str(gen_data$,16%,y) = " " then lbl$(35%) = lbl$(36%)
            if str(part$,11,1) = "4" then goto skipGenClear1
            if str(gen_data$,1,15) = "              " then         ~
                     lbl$(36%) = fs$
skipGenClear1:

            if str(part$,11,1) = "5" or str(part$,11,1) = "6" then ~
               skipGenClear2                     
            if str(gen_data$,16,15) = "              " then         ~
                     lbl$(35%) = fs$
skipGenClear2:
                     
REM     36^Glazing              35^Glazing
REM     38^Pane1                37^Pane1
REM     41^Airspace             42^Airspace
REM     40^Pane2                39^Pane2
REM     43^Airspace             44^Airspace
REM     45^Pane3                46^Pane3

            lbl$(37%) = "Single-Strength Annealed" & fs$
            lbl$(41%) = "Airspace" & fs$
            lbl$(42%) = "Airspace" & fs$
/* (AWD020) */
            lamn% = 0%
            lbl$(43%) = fs$
            lbl$(44%) = fs$
            lbl$(45%) = fs$
            lbl$(46%) = fs$
            if trip% = 1% then lbl$(43%) = "Airspace" & fs$
            if trip% = 1% then lbl$(44%) = "Airspace" & fs$
/* (\AWD020) */

            gen_key$ = "PLAN DBLE" & str(part$,5,2)
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_double

            lbl$(37%) = "Double-Strength Annealed" & fs$
skip_double:

            gen_key$ = "PLAN LAMN" & str(part$,5,2)
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_lamn
            lamn% = 1%
            lbl$(37%) = "Double-Strength Annealed" & fs$
skip_lamn:

            gen_key$ = "PLAN TEMP" & str(part$,5,2)
            read #4, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_tempered

            lbl$(37%) = "Double-Strength Tempered" & fs$
skip_tempered:
/* (AWD020) */
            if trip% = 1% then lbl$(45%) = lbl$(37%)
            if trip% = 1% then lbl$(46%) = lbl$(37%)
/* (\AWD020) */
            lbl$(38%) = lbl$(37%)
            if lamn% = 0% then lbl$(39%) = lbl$(37%)
            if lamn% = 0% then lbl$(40%) = lbl$(37%)
            if lamn% = 1% then lbl$(39%) = "Laminated Annealed" & fs$
            if lamn% = 1% then lbl$(40%) = "Laminated Annealed" & fs$

            if str(lbl$(35%),1,3) <> fs$ then goto not_blank
            lbl$(37%) = fs$
            lbl$(39%) = fs$
            lbl$(42%) = fs$
/* (AWD020) */
            lbl$(44%) = fs$
            lbl$(46%) = fs$
/* (\AWD020) */
not_blank:  if str(lbl$(36%),1,3) <> fs$ then goto not_blank2
            lbl$(38%) = fs$
            lbl$(40%) = fs$
            lbl$(42%) = fs$
not_blank2: if str(part$,11,1) <> "4" then goto not_4
            lbl$(35%) = fs$
            lbl$(37%) = fs$
            lbl$(42%) = fs$
            lbl$(39%) = fs$
            lbl$(44%) = fs$
            lbl$(46%) = fs$            
        goto not_5_6

not_4:      if str(part$,11,1) <> "5" and                          ~
               str(part$,11,1) <> "6" then goto not_5_6
            lbl$(36%) = fs$
            lbl$(38%) = fs$
            lbl$(41%) = fs$
            lbl$(40%) = fs$
            lbl$(43%) = fs$
            lbl$(45%) = fs$

not_5_6:

        if str(part$,5,2) <> "AZ" then goto notSingleGlaze
            lbl$(41%) = fs$
            lbl$(40%) = fs$
            lbl$(43%) = fs$
            lbl$(45%) = fs$

            lbl$(42%) = fs$
            lbl$(39%) = fs$
            lbl$(44%) = fs$
            lbl$(46%) = fs$
            
notSingleGlaze:
/*<AWD024>+ */
/*      stc$ = "34" : oitc$ = "28" : goto ttemp    temp for testing */
        init (" ") oitc$ : lb_stc$ = "   " : lb_stc% = 0 
           lbl$(48%) = fs$
           if stc$ <= " " then goto no_apr
           if str(stc$,1%,1%) = "0" then goto no_apr
           for stci% = 1 to 3
               if str(stc$,stci%,1%) = " " then goto next_stci
               lb_stc% = lb_stc% + 1
               str(lb_stc$,lb_stc%,1%) = str(stc$,stci%,1%)
next_stci: next stci%
           if str(lb_stc$,3%,1%) = " " then str(lb_stc$,3%,1%) = hex(00)
           stc$ = lb_stc$
           oitc$ = "28"
ttemp:   lbl$(48%) = "STC: (" & stc$ & ")" & "  OITC: (" & oitc$ & ")" &    ~
                     " EWR: (  )" & fs$
        
no_apr:
/*<AWD024>- */
       return
       
REM     36^Glazing              35^Glazing
REM     38^Pane1                37^Pane1
REM     41^Airspace             42^Airspace
REM     40^Pane2                39^Pane2
REM     43^Airspace             44^Airspace
REM     45^Pane3                46^Pane3       
/* </AWD017> */

        REM *************************************************************~
            *      L O A D   L A B E L   F O R M A T                    *~
            *************************************************************

                                                              /* (EWD011)     */
        load_label
          init  (" ") low$()

        low$(001%) = "^JO"
        low$(002%) = "^XA^EG^XZ" 
        low$(003%) = "^XA"
        low$(004%) = "^PMN"             
        low$(005%) = "^MNY"
        low$(006%) = "^MMT"
        low$(007%) = "^MTT"
        low$(008%) = "^MD0"
        low$(009%) = "^LH0,0"
        low$(010%) = "^LL813"                          /* (EWD010) 6 Inch/Sec */
                                                       /* PR Label Print Speed*/
                                                       /* For NE Labels.      */
        
         low$(011%) = "^PR6,4,8"                       /* PR Label Print Speed*/
                                                       /* For ATRIUM Labels.  */
        
                                                       /* a = 3 Print Speed   */
                                                       /* b = 4 Slew  SPeed   */
                                                       /* c = 8 Back Feed Spee*/
         low$(012%) = "^JMA"
         low$(013%) = "^COY,362"                       /* 256 + 128 Mem */
                                                       /* (-) 22k Intern*/        
         low$(014%) = "^FO563,96^FR^XGPIC1    ,1,1^FS"
         low$(015%) = ","
         low$(016%) = ","
         low$(017%) = "^FO175,37^CI0^A0R,140,120^FDDO NOT APPLY^FS"
         low$(018%) = "13^FO122,37^CI0^A0R,30,26^FR^FD"
         low$(019%) = "14^FO91,37^CI0^A0R,26,22^FR^FD"
         low$(020%) = "15^FO61,37^CI0^A0R,26,22^FR^FD"
         low$(021%) = "16^FO30,37^CI0^A0R,26,22^FR^FD"
         low$(022%) = "32^FO73,284^CI0^A0R,37,37^FR^FD"
         low$(023%) = "^PQ1"
         low$(024%) = "^XZ"
         
          init  (" ") yy$()

          yy$(001%) = "^JO"
          yy$(002%) = "^XA^EG^XZ"   
          yy$(003%) = "^XA"
          yy$(004%) = "^PMN"
          yy$(005%) = "^MNY"
          yy$(006%) = "^MMT"
          yy$(007%) = "^MTT"
          yy$(008%) = "^MD0"
          yy$(009%) = "^LH0,0"
          yy$(010%) = "^LL813"
          yy$(011%) = "^PR6,4,8"                     /* PR Label Print Speed*/
          yy$(012%) = "^JMA"
          yy$(013%) = "^COY,362"               /* 256 + 128 Mem (-) 22k Intern*/
          yy$(014%) = "^FO563,96^FR^XGPIC1    ,1,1^FS"
          yy$(015%) = ","
          yy$(016%) = ","
          yy$(017%) = "^FO175,37^CI0^A0R,140,120^FDDO NOT APPLY^FS"
          yy$(018%) = "13^FO122,37^CI0^A0R,30,26^FR^FD"
          yy$(019%) = "14^FO91,37^CI0^A0R,26,22^FR^FD"
          yy$(020%) = "15^FO61,37^CI0^A0R,26,22^FR^FD"
          yy$(021%) = "16^FO30,37^CI0^A0R,26,22^FR^FD"
          yy$(022%) = "32^FO73,284^CI0^A0R,37,37^FR^FD"
          yy$(023%) = "^PQ1"
          yy$(024%) = "^XZ"
          
        return            
