        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLA77 - Generic Label             *~
            *  Creation Date     - 03/13/00                             *~ 
            *  Last Modified Date- 05/31/2012                           *~ 
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  -                                      *~
            *                                                           *~
            *  Description       - Write records to a file to print the *~
            *                      new Non-Energy Star Labels.          *~
            *                                                           *~
            *                      Print File  = MFENERGY               *~
            *                      Script File = MFENERGY               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLA77 - Generates the label format and data to print   *~
            *            Non-Energy Star Labels. The resulting file is  *~
            *            routed to the label printer via a script.      *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                                                           *~
            *          - No Private Label or Logo                       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/11/00 ! Original - Copied & Mod (sub) EWDPLA71.  ! RHH *~
            * 08/31/00 ! (EWD001) - Mods for Form Change          ! RHH *~
            * 11/14/00 ! (EWD002) - Mods Visible Transmittance    ! RHH *~
            * 12/07/00 ! (EWD003) - Mods for form change (Mistake)! RHH *~
            * 01/22/01 ! (EWD004) - Mods for new labels with Pre- ! RHH *~
            *          !              Printed data.               !     *~
            * 07/26/01 ! (EWD005) - Mod to put 4 lines of text on ! RHH *~
            *          !              part of label.              !     *~
            * 06/24/02 ! (EWD006) - Mod for Wind Zone Design      ! RHH *~
            *          !            pressure.                     !     *~
            * 12/06/02 ! (EWD007) - Mod for new Air Leakage Value ! RHH *~
            *          !                                          !     *~
            * 02/28/03 ! (EWD009) - Mode for 5-8 and 1 Inch Grid  ! RHH *~
            *          !            also Multiple labels for      !     *~
            *          !            Continuous Head and Seal Prod !     *~
            * 06/17/03 ! (EWD010) - Mods to improve speed         ! RHH *~
            *          !            PR6,4,8 for New Faster Printer!     *~
            *          !            PR3,4,8 for Pld Slower Printers!    *~
            *          !                                          !     *~
            *          ! (Special Note) lb_nonresvisible contains !     *~
            *          !   the Value for Air Leakage              !     *~
            * 09/04/03 ! (EWD011) - Mod for New Energy Star Map   ! RHH *~
            *          !            Also the New Energy Star Logo !     *~
            * 07/16/04 ! (EWD012) - Mod to put printable version  ! RHH *~
            *          !            of the Barcode on the bottom  !     *~
            *          !            of the Label                  *     *~
            * 08/12/05 ! (EWD013) - Mod to 6 fields for NFRC      * RHH *~
            *          !            label format change.          *     *~
            * 10/31/05 ! (AWD014) - Mod for Label format change.  * RHH *~
            *          !            Have the Air Leakage text     *     *~
            *          !            removed fro the label. Alos,  *     *~
            *          !            Put leading Zero's in front of*     *~
            *          !            three ratings. Except for N/A *     *~
            * 01/01/06 ! (PAR000) - CR347 New Sub Part Number     * RHH *~
            * 03/03/06 ! (PAR001) - Mod for Sub Part Number No Chg* RHH *~
            * 08/26/08 ! (AWD015) - mod to replace NA with '-'    * CMG *~
            *01/16/2009! (AWD017) - Florida codes                 * DES *~
            *05/08/2012! (AWD018) - mod for triple pane           * CMG *~
            *05/31/2012! (AWD019) - mod for CPD Number            * CMG *~
            *04/05/2013! (AWD020) - mod for TDI Number            ! CMG *~
            *09/08/2014! (AWD021) Added Acoustical Performance    ! PWW *~
            *          !          Ratings (STS & OITC)            !     *~
            *12/22/2014! (AWD022) NFRC Reg Changes.               ! PWW *~
            *04/03/2015! (IM8005) Use bcksubpt Series & Style.    ! PWW *~
            *04/15/2015! (IM8020) Change Air Leakage syntax.      ! PWW *~
            *04/28/2015! (IM8020) Fix Series & Style issues.      ! PWW *~
            *12/10/2015! SR67154  2016 NFRC Reg Changes.          ! PWW *~
            *07/18/2016! CR00532  Increase lb_series$ + 1 char.   ! PWW *~
            *08/01/2017! CR1051   Suppress backfeed added         ! RDB *~   
            *01/15/2018! CR1251   Increase lb_series$ for AmerClas! RDB *~
            *01/29/2019! CR1906   WindowNation                    ! RDB *~
            *************************************************************

        sub "EWDPLA77" (been_here%,      /* Zero (1st Time Only)       */~
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
                        lb_txt1$,        /* No Grid,5/8,1,3/4,3/8PAR000*/~
                        lb_txt2$,        /* Dual Glazed          EWD009*/~
                        lb_barcode$,     /* Prod. Barcode        EWD012*/~
                        lb_thick$,       /* Glass Thickness      EWD013*/~
                        part$,           /* Glass Thickness      EWD013*/~
                        sub_part$,       /* Glass Thickness      EWD013*/~
                        lb_cpdnum$,      /* (AWD019) CPD Number        */~
                        #5,              /* (MFENERGY)                 */~
                        #2,              /* (AWDSKUXR)                 */~
                        #4,              /* (GENCODES)                 */~
                        lb_fl$,          /*                            */~
                        lb_tdi$,         /* (AWD020) TDI Number        */~
                        stc$,            /* (AWD021) mod for door map  */~
                        ctrlflag$,       /* CR9999 control backfeed    */~
                        error%)          /* Return Code                */

        dim                                                              ~
            a$100, b$100,                /* Print Lines for Label      */~
            lbl$(60%)65,                 /* Label Data Array           */~
            fs$3                         /* End of Lbl Records         */

        dim lb_brand$10,                 /* Brand Name                 */~
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
/*          lb_nonresvisible$4,             Non-Resident Visible(EWD002)*/~
/*AWD022*/  lb_nonresvisible$5,          /* Non-Resident Visible(EWD002)*/~
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
            yy$(60%)100,                 /* Label format               */~
            xx$(60%)100                  /* Buffer                     */

        dim lb_fl$12,                    /* Florida Approval Code      */~
            lb_tdi$12                    /* TDI Number                 */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim gen_key$24, gen_data$30
        dim part$25, sub_part$20
        dim cmg_part$25, cmg_sub_part$20
        dim stc$3, oitc$2                /*<AWD021>                    */
        dim ctrlflag$1                   /* CR99999                    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                            /* (PAR001) */
            apc$   = "(AWD)Create Energy Star Labels 03/03/06"
            pname$ = "EWDPLA77 - Rev: R7.00"

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
            * #5  ! MFENERGY ! Print Energy Star Labels                 *~
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

            cmg_part$=part$
            cmg_sub_part$=sub_part$

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            init(" ") xx$()
            if been_here% > 0% then goto L01000
               gosub load_label

L01000:     
            if ctrlflag$ = "N" then goto skipctrl
              yy$( 2%) = "^XA^EG^XB^XZ"                /* Added; CR1051 add  */
              yy$(10%) = "^LL813^XB"                  /* CR1051 add suppress*/
skipctrl:
            copy yy$() to xx$()
            been_here% = been_here% + 1%       /* (EWD010)              */
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
                                               /* Series Number    (12)*/
            lbl$(02%) = lb_series$             & fs$
                                               /* Vinyl Window Sty (24)*/
            lbl$(03%) = lb_style$              & fs$
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
                                               /* Res Heat Coeff   (04)*/
                                               /* (AWD014)             */
REM            if str(lb_resheat$,2%,1%) <> "N" then                       ~
                          lbl$(07%) = "0" & str(lb_resheat$,2%,3%) & fs$~
                  else lbl$(07%) = str(lb_resheat$,2%,3%) & fs$

/* (AWD015) */
            if str(lb_resheat$,2%,1%) <> "-" then                       ~
                          lbl$(07%) = "0" & str(lb_resheat$,2%,3%) & fs$~
                  else lbl$(07%) = str(lb_resheat$,2%,3%) & fs$

                                               /* Non-Res Heat Coef(04)*/
            lbl$(08%) = str(lb_nonresheat$,2%,3%) & fs$
                                               /* Design Pres Big  (02)*/
            lbl$(09%) = lb_dp$                  & fs$
                                               /* Design Pres Small(02)*/
            lbl$(10%) = lb_dp$ & "."
                                               /* Width an Height  (07)*/
                                               /* lb_x             (04)*/
                                               /* Height           (06)*/
            lb_x$ = hex(20) & "X" & hex(20) & hex(20)
            lbl$(11%) = lb_width$ & hex(22) & lb_x$ & lb_height$ &hex(22)
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
                                               /* More Glass       (24)*/
            lbl$(17%) = lb_glass_op1$           & fs$
                                               /* AWD019               */
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
/*          lbl$(19%) = str(lb_nonresvisible$,2%,3%)  & fs$ */
            lbl$(19%) = str(lb_nonresvisible$,1%,4%)  & fs$  /*<AWD022>*/

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
                                              /* (EWD012)               */
/* <AWD017> */
check_model
REM  str(sku_key$,01,25) = part$
/* read AWDPLNEX table @@@ */

REM            LBL$(33%) = "FLORIDA PRODUCT APPROVAL " & LB_FL$ & FS$
/* (AWD022) */
            str(lbl$(33%),1%,29%)  = "FL Prd Approval: " & lb_fl$
            str(lbl$(33%),30%,30%) = "        TDI: " & lb_tdi$ & fs$


            lbl$(34%) = "Glazing complies with ASTM E 1300" & fs$

            init(" ") gen_data$
            gen_key$ = "ELLISON05" & str(part$,1,3) & "00"
            part2$ = part$


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
/* (AWD018) */
            lamn% = 0%
            lbl$(43%) = fs$
            lbl$(44%) = fs$
            lbl$(45%) = fs$
            lbl$(46%) = fs$
            if trip% = 1% then lbl$(43%) = "Airspace" & fs$
            if trip% = 1% then lbl$(44%) = "Airspace" & fs$
/* (\AWD018) */

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
            lbl$(38%) = lbl$(37%)
            if lamn% = 0% then lbl$(39%) = lbl$(37%)
            if lamn% = 0% then lbl$(40%) = lbl$(37%)
            if lamn% = 1% then lbl$(39%) = "Laminated Annealed" & fs$
            if lamn% = 1% then lbl$(40%) = "Laminated Annealed" & fs$

/* (AWD018) */
            if trip% = 1% then lbl$(45%) = lbl$(37%)
            if trip% = 1% then lbl$(46%) = lbl$(37%)
/* (\AWD018) */
            if str(lbl$(35%),1,3) <> fs$ then goto not_blank
            lbl$(37%) = fs$
            lbl$(39%) = fs$
            lbl$(42%) = fs$
/* (AWD018) */
            lbl$(44%) = fs$
            lbl$(46%) = fs$
/* (\AWD018) */
not_blank:  if str(lbl$(36%),1,3) <> fs$ then goto not_blank2
            lbl$(38%) = fs$
            lbl$(40%) = fs$
            lbl$(41%) = fs$
/* (AWD018) */
            lbl$(43%) = fs$
            lbl$(45%) = fs$
/* (\AWD018) */
not_blank2: if str(part$,11,1) <> "4" then goto not_4
            lbl$(35%) = fs$
            lbl$(37%) = fs$
            lbl$(42%) = fs$
            lbl$(39%) = fs$
/* (AWD018) */
            lbl$(44%) = fs$
            lbl$(46%) = fs$
/* (\AWD018) */
        goto not_5_6

not_4:      if str(part$,11,1) <> "5" and                          ~
               str(part$,11,1) <> "6" then goto not_5_6
            lbl$(36%) = fs$
            lbl$(38%) = fs$
            lbl$(41%) = fs$
            lbl$(40%) = fs$
/* (AWD018) */
            lbl$(43%) = fs$
            lbl$(45%) = fs$
/* (\AWD018) */
REM     36^Glazing              35^Glazing
REM     38^Pane1                37^Pane1
REM     41^Airspace             42^Airspace
REM     40^Pane2                39^Pane2
REM     43^Airspace             44^Airspace
REM     45^Pane3                46^Pane3
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
/*<AWD021>+ */
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
/*<AWD021>- */

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
                                           /* (AWD010)                  */
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

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(100)                             /* (EWD006)       */

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

        REM *************************************************************~
            *      L O A D   L A B E L   F O R M A T                    *~
            *************************************************************


       load_label

           init(" ") yy$()

           yy$( 1%) = "^JO"
           yy$( 2%) = "^XA^EG^XZ"
           yy$( 3%) = "^XA"
           yy$( 4%) = "^PMN"
           yy$( 5%) = "^MNY"
           yy$( 6%) = "^MMT"
           yy$( 7%) = "^MTT"
           yy$( 8%) = "^MD0" 
           yy$( 9%) = "^LH0,0"
           yy$(10%) = "^LL813"
                                                      /* (EWD010) 6 Inch/Sec */
        REM   yy$(11%) = "^PR4,4,8"                   /* PR Label Print Speed*/
                                                      /* For NE Labels.      */

           yy$(11%) = "^PR6,4,8"                      /* PR Label Print Speed*/
                                                      /* For ATRIUM Labels.  */

                                                      /* a = 6 Print Speed   */
                                                      /* b = 4 Slew  SPeed   */
                                                      /* c = 8 Back Feed Spee*/

           yy$(12%) = "^JMA"
           yy$(13%) = "^COY,362"                      /* 256 + 128 Mem */
                                                      /* (-) 22k Intern*/


                                                      /* U- Factor (U.S.-I-P)  */
                                                      /* (AWD014)              */
           yy$(14%) = "05^FO1138,189^CI0^A0R,71,71^FR^FD"
                                                      /* Visible Transmittance */
                                                      /* (1)          (EWD013) */
        REM   yy$(15%) = "18^FO949,189^CI0^A0R,71,71^FR^FD"
                                                      /* (AWD014)              */
           yy$(15%) = "18^FO961,189^CI0^A0R,71,71^FR^FD"
                                                      /* Design Pressure Big   */
                                                      /* (3)          (EWD013) */
        REM   yy$(16%) = "09^FO388,130^CI0^A0R,61,61^FR^FD"
           yy$(16%) = "09^FO376,148^CI0^A0R,61,61^FR^FD"
                                                      /* Text Line (3)         */
                                                      /* (4)          (EWD013) */
        REM   yy$(17%) = "22^FO402,207^CI0^A0R,22,22^FR^FD"
           yy$(17%) = "22^FO414,231^CI0^A0R,22,22^FR^FD"
                                                      /* Text Line (4)         */
                                                      /* (5)          (EWD013) */
        REM   yy$(18%) = "23^FO374,207^CI0^A0R,22,22^FR^FD"
           yy$(18%) = "23^FO386,231^CI0^A0R,22,22^FR^FD"
                                                      /* Wind Zone             */
           yy$(19%) = "28^FO221,445^CI0^A0R,33,33^FR^FD"
                                                      /* Wind Pressure Vales   */
           yy$(20%) = "29^FO189,478^CI0^A0R,33,33^FR^FD"
                                                      /* Sequence Number       */
           yy$(21%) = "13^FO122,37^CI0^A0R,30,26^FR^FD"
                                                      /* Dept. and S.O.        */
           yy$(22%) = "14^FO91,37^CI0^A0R,26,22^FR^FD"
                                                      /* Load and Model        */
           yy$(23%) = "15^FO61,37^CI0^A0R,26,22^FR^FD"
                                                      /* Production Date       */
           yy$(24%) = "16^FO30,37^CI0^A0R,26,22^FR^FD"
                                                      /* Required Field        */
REM        yy$(25%) = "^FO1329,693^CI0^A0R,26,26^FR^FDRES97^FS"
                                                      /* Air Leakage           */
                                                      /* (2)          (EWD013) */
        REM   yy$(26%) = "19^FO949,541^CI0^A0R,71,71^FR^FD"
/*         yy$(25%) = "19^FO961,541^CI0^A0R,71,71^FR^FD"  */
           yy$(25%) = "19^FO961,491^CI0^A0R,71,71^FR^FD"   /*AWD022*/
                                                      /* Solor Heat Gain Coeff */
                                                      /* (AWD014)              */
           yy$(26%) = "07^FO1138,516^CI0^A0R,71,71^FR^FD"
                                                      /* Grid Text (1)         */
           yy$(27%) = "30^FO1544,240^CI0^A0R,35,33^FR^FD"
                                                      /* Grid Text (2)         */
           yy$(28%) = "31^FO1544,445^CI0^A0R,35,33^FR^FD"
                                                      /* Glass (2)             */
           yy$(29%) = "17^FO1360,240^CI0^A0R,35,33^FR^FD"
                                                      /* Glass (1)             */
           yy$(30%) = "04^FO1390,240^CI0^A0R,35,33^FR^FD"
                                                      /* Vinyl Window Style    */
           yy$(31%) = "03^FO1420,240^CI0^A0R,35,33^FR^FD"
                                                      /* Window Series         */
           yy$(32%) = "02^FO1450,240^CI0^A0R,35,33^FR^FD"
                                                      /* Private Label Brand   */
           yy$(33%) = "01^FO1480,240^CI0^A0R,35,33^FR^FD"
                                                      /* (EWD012) Prod Barcode */
                                                      /* (6)           (EWD013)*/
        REM   yy$(35%) = "32^FO67,284^CI0^A0R,43,43^FR^FD"
           yy$(34%) = "32^FO73,284^CI0^A0R,37,37^FR^FD"
                                                      /* (EWD012)              */
/* <AWD017> */
           yy$(35%) = "33^FO340,5^CI0^A0R,26,26^FR^FD"
           yy$(36%) = "34^FO318,5^CI0^A0R,26,26^FR^FD"
           yy$(37%) = "36^FO296,5^CI0^A0R,26,26^FR^FD"
           yy$(38%) = "35^FO296,380^CI0^A0R,26,26^FR^FD"
REM Pane1
           yy$(39%) = "38^FO274,5^CI0^A0R,26,26^FR^FD"
           yy$(40%) = "37^FO274,380^CI0^A0R,26,26^FR^FD"
REM           yy$(41%) = "40^FO252,5^CI0^A0R,26,26^FR^FD"
REM           yy$(42%) = "39^FO252,380^CI0^A0R,26,26^FR^FD"
REM Airspace
           yy$(41%) = "41^FO252,5^CI0^A0R,26,26^FR^FD"
           yy$(42%) = "42^FO252,380^CI0^A0R,26,26^FR^FD"
REM pane 2 desc
           yy$(43%) = "40^FO230,5^CI0^A0R,26,26^FR^FD"
           yy$(44%) = "39^FO230,380^CI0^A0R,26,26^FR^FD"

/* (AWD018) */
REM Airspace
           yy$(45%) = "43^FO208,5^CI0^A0R,26,26^FR^FD"
           yy$(46%) = "44^FO208,380^CI0^A0R,26,26^FR^FD"
REM triple pane descr
           yy$(47%) = "45^FO186,5^CI0^A0R,26,26^FR^FD"
           yy$(48%) = "46^FO186,380^CI0^A0R,26,26^FR^FD"
/* (\AWD018) */

           yy$(49%) = "^FO395,5^CI0^A0R,26,26^FR^FD+/-^FS"
/* (AWD019) */
           yy$(50%) = "47^FO1330,240^CI0^A0R,35,33^FR^FD"
/* (\AWD019) */

           yy$(51%) = "48^FO0204,5^CI0^A0R,26,26^FR^FD"   /* AWD021 */
/*AWD022 */
           yy$(52%) = "^FO1031,481^CI0^A0R,26,26^FR^FDAir Leakage (U.S./I-P)^FS"
           yy$(53%) = "^FO971,504^GB0,44,06^FS"      /* IM8020 */
           yy$(54%) = "^PQ1"
           yy$(55%) = "^XZ"

        return

