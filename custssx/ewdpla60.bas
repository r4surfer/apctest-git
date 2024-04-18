        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLA60                             *~
            *  Creation Date     - 08/24/98                             *~
            *  Last Modified Date- 09/02/04                             *~
            *  Written By        - Brian W. Sanders                     *~
            *                                                           *~
            *  Description       - Winding/Coil Data Report. Called from*~
            *                      EWDPLN60 to generate report.         *~
            *                      Prod. Date & Dept. Range Specified.  *~
            *                                                           *~
            *  Code Tables Used  - PLAN TDLU, PLAN DEPT, PLAN DBLE,     *~
            *                      PLAN BALC, PLAN LAMN, PLAN TLDF      *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/24/98 ! (New) Program - copied & mod EWDPLN60.   ! BWS *~
            * 09/15/98 ! Add workfile logic to control sort order.! BWS *~
            * 09/29/98 ! Mod to logic for Cottage/Oriel.    EWD001! BWS *~
            * 11/19/98 ! Mods to logic for Cottage/Oriel.   EWD002! BWS *~
            * 01/29/99 ! Mods for Tube Length Diff Calc.    EWD003! BWS *~
            * 02/04/99 ! Mods to lookup of table PLAN BALC.-EWD004! BWS *~
            *          ! Also mod to skip " " APCPLNWC values.    !     *~
            * 03/12/99 ! Add summary rpt for tube reqrmnts.-EWD005! BWS *~
            *          ! Also add ability to run historical rpt.  !     *~
            * 04/12/99 ! Mods to run summary rpt for coils.-EWD006! BWS *~
            * 04/20/99 ! Mods for Laminate Glass&ALL Depts.-EWD007! BWS *~
            * 04/20/00 ! (EWD008) Mod to Switch Primary file for  ! RHH *~
            *          !    History.                              !     *~      
            * 06/05/00 ! (EWD009) Mod to Use (APCPLNSC) for history!CMG *~
            *          !     if date range greater than three     !     *~
            *          !     weeks otherwise use (APCPLNDT) for   !     *~      
            *          !     daily detail data.                   !     *~
            * 02/11/02 ! (EWD010) Mod to change report to reflect ! TBM *~
            *          !     block and tackle for 712 and 411     !     *~
            * 02/25/02 ! (EWD011) Mods for the new 411 balance    ! TBM *~
            *          !     tubes with Cottage/Oriel             !     *~
            * 02/22/02 ! (EWD012) Mods for the 421 & 431 to lookup! CMG *~
            *          !     411 data.                            !     *~
            * 03/12/03 ! (EWD013) Mods for new depts 25 and 26    ! CMG *~
            *          !          to be treated same as dept 36.  !     *~
            * 10/24/03 ! (EWD014) Mods for new depts 27 and 28    ! CMG *~
            *          !          to be treated same as dept 36.  !     *~
            * 03/03/04 ! (EWD015) Mod to look up block and tackle ! CMG *~
            *          !          departments by gencodes table   !     *~
            *          !          PLAN TBLK                       !     *~
            * 09/02/04 ! (AWD016) Mod to take off screen balances ! CMG *~
            *          !   for sclmr.                             !     *~
            * 09/22/04 ! (AWD017) Mod to add tso,bso,fgo to rpt   ! CMG *~
            *************************************************************

            sub "EWDPLA60" (#2, #3, #6, #4, #5, #9, minth%, maxth%)

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            gen_key$50,                  /* Second Gencodes Key (EWD015)*/~
            sav_wc_key$15,               /* Use for Loading Table      */~
            wdtkey$5, hgtkey$5,          /* Width & Height for WC Key  */~
            sc_dept$3, sc_dept_d$30,     /* Department Code & Descr    */~
            sc_prddate_fr$10,            /* Production Date            */~
            sc_prddate_to$10,            /*       Range                */~
            sc_data_flg$1,               /* Flag to print addtl. data  */~
            rpt_date_rnge$24,            /* Date Range for Printing    */~
            prddate$10, sav_prddate$10,  /* Production Date from DT    */~
            sc_key$33, dept$3, seq$5,    /* SC Alt key (2)     (EWD008)*/~
            dt_key$57,                   /* DT Alt key (1)     (EWD009)*/~
            pl_key$15,                   /* Alt (3) (APCPLNDP)         */~
            wc_key$15,                   /* WC key                     */~
            wk_key$51, wk_rec$256,       /* Work file key, record area */~
            part$25,                     /* Mfg. Part No.              */~
            hinge$2,                     /* Hinge Code from Part No.   */~
            sngl$1,                      /* Single-Hung Window Flag    */~
            tddiff$1,                    /* Tube Diameter Diff Flag    */~
            windcl$(6)6,                 /* Winding/Coil Data(APCPLNWC)*/~
            tubedi$(6)2,                 /* Tube Diam Codes (APCPLNWC) */~
/*EWD001*/  co_flag$2,                   /* Flag for Cottage/Oriel     */~
/*EWD011*/  co_flg1$2,                   /* Second Flag for CO/OR      */~
/*EWD005*/  hd_flag$1,                   /* Flag for Heavy Duty Tube   */~
/*EWD005*/  sav_tht$9,                   /* Tube Height(s) for Compare */~
/*EWD005*/  hist_flag$9,                 /* Flag for Historical Report */~
            billto$9,                    /* Bill-To Customer Code      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
/*EWD005*/  date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
/*EWD009*/  prddate_fr$6,                /* Production Date            */~
/*EWD009*/  prddate_to$6,                /*       Range                */~
            clmr$                        /* SCLMR              (AWD016)*/

        dim seqn$5,                      /* Production Sequence No.    */~
            mdl$3,                       /* Model No.                  */~
            sav_mdl$3,                   /* Save Model No.     (EWD012)*/~
            wdt$7,                       /* Width                      */~
            hgt$7,                       /* Height                     */~
            gls$2,                       /* Glass No.                  */~
            scr$1,                       /* Screen No.                 */~
            wcl$6,                       /* Winds/Coil                 */~
            tht$9,                       /* Tube Heights               */~
            tdi$2,                       /* Tube Diameter Code         */~
            cus$9,                       /* Customer Code              */~
            inf$19                       /* Model/Customer Info        */~


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Tube Windings/Coil Report         "
            pname$ = "EWDPLA60 - Rev: R7.00"       /*^Don't^Use - EWD005*/

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! APCPLNDT ! Production Master Detail file            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! APCPLNWC ! Production Windings & Coil File          *~
            * #6  ! APCPLNSC ! Planning Mawster Schedule file           *~
            * #7  ! APCPLNWK ! Work File                                *~
/*EWD005*/  * #8  ! APCPLWK2 ! Work File #2                             *~
            * #9  ! APCPLNDP ! Planning Master Dept File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #7, "APCPLNWK",                                       ~
                        varc,     indexed,  recsize =   95,              ~
                        keypos =  1,   keylen = 51

/*EWD005*/  select #8, "APCPLWK2",                                       ~
/*EWD005*/              varc,     indexed,  recsize =   32,              ~
/*EWD006*/              keypos =  1,   keylen =  9


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
/*EWD005*/  call "DATEOKC" (date$, today%, errormsg$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
/*EWD005*/  time$ = " "  :  call "TIME" (time$)
            pg% = 0%  :  ln% = 99%
            select printer (134)

/*EWD005*/  work%, work2% = 0%      /* Work File has been opened flags */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
/*EWD005*/        if keyhit%  =  9% then goto  print_report /* Summary */
/*EWD005*/        if keyhit%  = 10% then goto  print_report /* Detail */
                  if keyhit%  = 16% then goto  exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
                                                 /* Print Winding/Coil */ 
        print_report                             /* Production Report  */
            rpt_date_rnge$ = sc_prddate_fr$ & " TO " & sc_prddate_to$
            call "DATUFMTC" (sc_prddate_fr$)
            call "DATUFMTC" (sc_prddate_to$)
            sc_key$, dt_key$, wk_key$ = all(hex(00)) 
            str(sc_key$,,6) = sc_prddate_fr$
            str(dt_key$,,6) = sc_prddate_fr$
            call "SHOSTAT" ("Printing Winding/Coil Report...")
            cnt% = 0%
            seq% = 0%                            /* (EWD008)           */
/*EWD005*/  mode% = 1%  :  f% = 7%  :  gosub open_work
/*EWD005*/  if keyhit% <> 9% then L20100
/*EWD005*/      call "SHOSTAT" ("Printing Summary Report...")
/*EWD005*/      mode2% = 1%  :  f% = 8%  :  gosub open_work
L20100:     days%, result% = 0%                            /* (EWD009)  */
            prddate_fr$ = str(sc_prddate_fr$,1%,6%)        /* (EWD009)  */
            prddate_to$ = str(sc_prddate_to$,1%,6%)        /* (EWD009)  */
            call "DATE" addr("G-", prddate_fr$, prddate_to$, days%, result%)
            days% = abs(days%)
                                                           /* (EWD009)  */
            if days% <= 21% then gosub load_data_detail                   ~
                            else gosub load_data           /* (EWD009)  */
            goto exit_program

/* (EWD009)  -- Begin  */
        load_data                                       /* (EWD008)     */      
            read #6, key 2% > sc_key$, using L20210, wk_rec$,             ~
                    eod goto read_work_file
L20210:         fmt ch(128)
            if str(wk_rec$,1%,33%) <= sc_key$ then bad_data_key
            sc_key$ = str(wk_rec$,1%,33%)
/*EWD005*/  mdl$  = str(wk_rec$,34%,3%)                 /* Model        */
            part$ = str(wk_rec$,34%,25%)
            if len(part$) < 19 then goto load_data
            if str(sc_key$,,6%) > sc_prddate_to$ then goto read_work_file
                                                        /* (EWD008)      */
                                                        /* Find Dept     */
            gosub find_dept 
            if dept% = 0% then goto load_data                
/*EWD005*/  if dept$ <> sc_dept$ and sc_dept$ <> "ALL" then               ~
                                                           goto load_data
                                                     /*Hist% = 0% = Std */
                                                     /*      = 1% = Hist*/
            if str(wk_rec$,110%,2%) > "11" and hist% = 0%                 ~
                then goto load_data /* Cmpltd  */

            if str(wk_rec$,110%,2%) < "03" then goto load_data
                                                     /* Not Planned    */ 
            if keyhit% <> 9% or sc_dept$ <> "ALL" then L20250  /*EWD005*/
                init(" ") readkey$                             /* Begin*/ 
/*EWD007*/      str(readkey$,1%,9%)   = "PLAN TWAD"
/*EWD007*/      str(readkey$,10%,15%) = dept$         
                read #4, key = readkey$, eod goto load_data    /* End  */ 
L20250:     get str(wk_rec$,70%,2%), using L20255, sc_mqty%
L20255:         FMT BI(2)

            if sc_mqty% < 1% then goto load_data     /* (EWD008)       */

            cnt% = cnt% + 1%                         /* (EWD008)       */ 
            convert sc_mqty% to temp$, pic(####0)
            gosub write_work_file
            goto load_data


        load_data_detail
            read #3, key 1 > dt_key$, using L20215, wk_rec$,             ~
                    eod goto read_work_file
L20215:         fmt ch(256)
            if str(wk_rec$,47,57) <= dt_key$ then bad_data_key
            dt_key$ = str(wk_rec$,47,57)
/*EWD005*/  mdl$ = str(wk_rec$,189,3)
            if str(dt_key$,,6) > sc_prddate_to$ then goto read_work_file
/*EWD005*/     gosub find_dept 
            if dept% = 0% then goto load_data_detail
            if str(dt_key$,13,3) <> sc_dept$ and sc_dept$ <> "ALL"       ~
                then goto load_data_detail
            if str(dt_key$,18,2) > "11" and hist% = 0%         /*EWD005*/~
                then goto load_data_detail /* Cmpltd  */
            if keyhit% <> 9% or sc_dept$ <> "ALL" then L20260  /*EWD005*/
                init(" ") readkey$                             /* Begin*/ 
/*EWD007*/      str(readkey$,1%,9%)   = "PLAN TWAD"
/*EWD007*/      str(readkey$,10%,15%) = str(dt_key$,13%,3%)    /*EWD005*/
                read #4, key = readkey$, eod goto load_data_detail  /* End  */ 
L20260:     cnt% = cnt% + 1%
            convert cnt% to temp$, pic(####0)
            gosub write_work_file_detail
            goto load_data_detail


        write_work_file_detail
            write #7, using L20300,                                      ~
                str(wk_rec$,47,6),          /* Production Date      */   ~
                str(wk_rec$,59,3),          /* Department Code      */   ~
                str(wk_rec$,111,5),         /* Prod. Sequence No.   */   ~
                str(wk_rec$,66,30),         /* User Defined Index   */   ~
                str(wk_rec$,104,2),         /* Shift Code           */   ~
                str(temp$,,5),              /* Count 'em up 1,2,3...*/   ~
                str(wk_rec$,24,10),         /* S.O. & Line Item     */   ~
                str(wk_rec$,124,9),         /* Customer Code        */   ~
                str(wk_rec$,189,25),        /* Part No.             */   ~
            eod goto L20310, data goto L20315

L20315:     return

        write_work_file
            seq% = seq% + 1%
            convert seq% to seq$, pic(00000)

            write #7, using L20300,                                      ~
                str(wk_rec$,1%,6%),         /* Production Date      */   ~
                dept$,                      /* Department Code      */   ~
                seq$,                       /* Prod. Sequence No.   */   ~
                " ",                        /* User Defined Index-30*/   ~
                "01",                       /* Shift Code           */   ~
                str(temp$,,5%),             /* Line Item Qty        */   ~
                str(wk_rec$,24%,10%),       /* S.O. & Line Item     */   ~
                str(wk_rec$,59%,9%),        /* Customer Code        */   ~
                part$,                      /* Part No.             */   ~
            eod goto L20310, data goto L20310

L20300:     fmt ch(6), ch(3), ch(5), ch(30), ch(2), ch(5), ch(10), ch(9),~
                ch(25)
L20310:     return

/* (EWD009)  -- End  */


        bad_data_key
           errormsg$ = "Key Sequence Error: " & str(wk_rec$,1%,33%)
            call "SHOSTAT" (errormsg$)
            stop
            goto exit_program

        read_work_file

            if mode% = 2% then goto L20400
/*EWD005*/      mode% = 2%  :  f% = 7%  :  gosub open_work
L20400:     gosub clear_line
            read #7, key > wk_key$, using L20410, wk_key$,               ~
                cus$, part$, eod goto L20998
L20410:             fmt ch(51), xx(10), ch(9), ch(25)
            sc_mqty% = 0% 
            convert str(wk_key$,47%,5%) to sc_mqty%, data goto L20415
L20415: 
            seqn$ = str(wk_key$,10%,5%)
            prddate$ = str(wk_key$,,6)
            call "DATFMTC" (prddate$)
            mdl$   = str(part$,,3)
            gls$   = str(part$,5,2)
            hinge$ = str(part$,9,2)
            scr$   = str(part$,11,1)
            wdt$   = str(part$,13,4)
            hgt$   = str(part$,17,3)
            sav_mdl$ = mdl$                                   /* (EWD012) */
            convert wdt$ to wdt, data goto read_work_file
            
            convert wdt$ to sav_wdt, data goto read_work_file /* (EWD012) */
            gosub lookup_model                                /* (EWD012) */
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)
            convert hgt$ to hgt, data goto read_work_file
            hgt = hgt / 10              
            convert hgt to hgtkey$, pic(000.0)
            fctnl = hgt - int(hgt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            hgt = int(hgt) + fctnl         /* Reused in calc_tube_hgts */ 
            convert hgt to hgt$, pic(##0.000)
            gosub find_lkup_diam
            
            co_flg1$ = " "                                    /*EWD011*/
REM            if co_flag$ = "CO" and mdl$ = "411" then co_flg1$ = "C1"
REM            if co_flag$ = "OR" and mdl$ = "411" then co_flg1$ = "O1"
            for x% = 1% to 2%                                 /*EWD011*/
             sav_wc_key$ = all(hex(00))                               
             if  x% = 1% then                                               ~
                  str(sav_wc_key$,,10)=str(mdl$) & str(tdi$) & wdtkey$      ~
              else                                                          ~
                  str(sav_wc_key$,,10)=str(mdl$) & co_flg1$ & wdtkey$         
                                                             /*EWD011*/                                
            read #5, key > sav_wc_key$, using L20500, wc_key$,           ~
                    eod goto L20570
L20500:         fmt ch(15)                  /* 1st Pass Finds Width... */
            if str(wc_key$,,5) <> str(sav_wc_key$,,5) then goto L20570
            sav_wc_key$, wc_key$ = str(wc_key$,,10) & hgtkey$ /*EWD004*/
next_hgt:   read #5, key >= wc_key$, using L20540, wc_key$,   /*EWD004*/ ~
                    windcl$(), tubedi$(), eod goto read_work_file
L20540:         fmt ch(15), 6*ch(6), 6*ch(2)/* 2nd Pass Finds Height.. */
            if str(wc_key$,,10) <> str(sav_wc_key$,,10)                  ~
                 then goto read_work_file   /* Height Undefined...     */
L20570:     gosub find_gls_type
            if windcl$(j%) <> " " then L20580                 /*EWD005*/
                wc_key$ = str(wc_key$,,14) & bin(val(str(wc_key$,15,1))+1)
                goto next_hgt                                 /*EWD005*/
L20580:     if x% <> 2% then wcl$ = windcl$(j%)                /*EWD011*/~
            else str(wcl$,4%,6%) = windcl$(j%)
            if co_flg1$ = " " then x% = 2%
            next x%                                          /*EWD011*/
            tdi$ = tubedi$(j%)
            if wcl$ = " " then wcl$ = "."
            if tdi$ = " " then tdi$ = "99"          /* 99 = Coil       */
            call "STRING" addr("RJ", wcl$, 6%)      /* Both steps reqd.*/
            call "STRING" addr("CT", wcl$, 6%)      /* to center data. */
            if tdi$ <> "99" then gosub calc_tube_hgts
/*EWD002*/  if tht$ = "." and co_flag$ <> " " then tht$ = co_flag$
            if inf$ <= "." then gosub load_color

/* (AWD017) - Begin   */
            if str(part$,11%,1%) = "4" or str(part$,11%,1%) = "5"  ~
                  then gosub add_tso_desc
            if str(part$,11%,1%) = "6" then gosub add_tso_desc 
/* (AWD017) - END     */

            mdl$ = sav_mdl$                       /* (EWD012) - BEG */
            wdt = sav_wdt
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)     /* (EWD012) - END */

/*EWD005*/  on keyhit% - 8% gosub build_summary, generate_report
            sav_prddate$ = prddate$
            goto read_work_file
           
L20998:     if cnt% = 0% then goto L20999
/*EWD005*/  if keyhit% = 9% then gosub generate_summary_report
            return

L20999:     errormsg$ = "No data to print for this date range/department."
            gosub error_prompt
            return clear all
            goto inputmode



        find_lkup_diam
/*EWD002*/  gosub check_ctg_orl
                                               /*  (AWD016)  - BEG */
            if co_flag$ = " " then goto L21010
            if str(clmr$,1%,1%) > "9" then goto L21015
            init(" ") clmr$ 
            clmr$ = str(part$,20%,3%)
            if clmr$ <> "   " and clmr$ <> "000" then co_flag$ = "SP" 
                                               /*  (AWD016)  - END */
L21015:
/*  |   */  if co_flag$ = " " then goto L21010
/*  |   */      tdi$ = co_flag$
/*EWD002*/      return

L21010:     read #2, key = cus$, using L21020, billto$, eod goto L21160
L21020:         fmt pos(780), ch(9)
            readkey$ = "PLAN TDLU" & str(mdl$) & " " & billto$
            gosub read_gencds1
            if found% = 1% then return

L21160:     readkey$ = "PLAN TDLU" & mdl$
            gosub read_gencds1
            if found% = 1% then return

            tdi$ = "00"             /* Default to Standard */
            return


            read_gencds1
               found% = 0%
               read #4, key = readkey$, using L21270, tdi$, inf$,        ~
                        eod goto L21290
L21270:             fmt pos(25), ch(2), xx(3), ch(19)
               found% = 1%
L21290:        return



        find_gls_type
            j% =1%
            
            readkey$ = "PLAN DBLE" & gls$
            gosub read_gencds2
            if found% = 0% then goto L22070
                j% = 2%
                return

L22070:     readkey$ = "PLAN LAMN" & gls$       /*EWD007*/
            gosub read_gencds2
            if found% = 0% then goto L22110
                j% = 3%
                return                

L22110:     readkey$ = "PLAN xxxx" & gls$
*           gosub read_gencds2
*           if found% = 0% then goto L22150
*               j% = 4%
*               return

*L22150:    readkey$ = "PLAN xxxx" & gls$
*           gosub read_gencds2
*           if found% = 0% then goto L22190
*               j% = 5%
*               return

/*L22190:*/ return                              /*EWD007*/


            read_gencds2
               found% = 0%
               read #4, key = readkey$, eod goto L22290                    
               found% = 1%
L22290:        return



        calc_tube_hgts
            ht1%, ht2% = 999%  :  factor = 0
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = "PLAN BALC"
            str(readkey$,10%,3%) = mdl$                       /*EWD004*/
            for x% = 1% to 2%                                 /* Begin*/
              if x% = 2% then str(readkey$,13%,2%) = tdi$                ~
                 else str(readkey$,13%,2%) = " "                        
              read #4,key=readkey$, using L22500, descr$, eod goto L22510
L22500:          FMT POS(25), CH(30)
L22510:     next x%                                           /*EWD004*/
            if descr$ = " " then goto L22580                  /* End  */
            convert str(descr$,,7) to factor, data goto L22580
            sngl$ = str(descr$,10,1)
            ht2% = int((hgt/2) - factor)
            factor% = 2%                    /*  EWD003 - Begin  */
            init(" ") readkey$, descr$                      
            str(readkey$,1%,9%)   = "PLAN TLDF"
            str(readkey$,10%,15%) = mdl$ & tdi$
            read #4,key = readkey$, using L22500, descr$, eod goto L22540
            convert str(descr$,,3) to factor%, data goto L22540
L22540:     ht1% = ht2% - factor%           /*   EWD003 - End   */  
*EWD002         --- Previous location of code in check_ctg_orl ---
            if co_flag$ = "CO" then ht1% = ht1% * 0.8/*Cottage*/ /*EWD*/
*EWD002     if co_flag$ = "OR" then ht2% = ht2% * 1.2/* Oriel */ /*001*/
            if ht1% < minth% then ht1% = minth%
            if ht1% > maxth% then ht1% = maxth%
            if ht2% < minth% then ht2% = minth%
            if ht2% > maxth% then ht2% = maxth%
            tht$ = "    -xxx "
            if co_flag$ <> " "                                /*EWD001*/ ~
                then str(tht$,5,1) = "/"       /* Denotes diff in calc */
L22580:     if sngl$ <> "S" or co_flag$ <> " "                /*EWD001*/ ~
                then convert ht1% to str(tht$,,3), pic(##0)
            convert ht2% to str(tht$,6,3), pic(##0)
            return


        load_color
            readkey$ = "COLOR    " & str(part$,4,1)
            read #4, key = readkey$, using L22600, inf$,                 ~
                     eod goto L22610
L22600:          fmt pos(30), ch(15)
L22610:     return


        check_ctg_orl   /* EWD002 - Was part of calc_tube_hgts;    */
                        /*          Now called from find_lkup_diam */
            init(" ") readkey$, descr$, co_flag$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hinge$
            read #4,key = readkey$, using L22700, descr$, eod goto L22710
L22700:        FMT POS(25), CH(30)                          /* EWD002^*/
            co_flag$ = str(descr$,1%,2%)                    /* EWD001 */
            if co_flag$ <> "CO" and co_flag$ <> "OR" then co_flag$ = " "
L22710:     return    /* <- EWD002 */       /* ^ EWD001 ^ */


/* (AWD017)  -  BEGIN  */
        add_tso_desc
            p% = 0%
            p% = len(inf$) + 1%
            q% = (15% - p%)
                        
            if str(part$,11%,1%) = "4" then        ~
                 str(inf$,p%,q%) = " / TSO"
            if str(part$,11%,1%) = "5" then        ~
                 str(inf$,p%,q%) = " / BSO"
            if str(part$,11%,1%) = "6" then        ~
                 str(inf$,p%,q%) = " / FGO"

        return
/* (AWD017)  -  END    */

        clear_line
            seqn$, mdl$, wdt$, hgt$, gls$, scr$, tdi$, windcl$(),        ~
/*EWD001*/      tubedi$(), tddiff$, co_flag$ = " " 
            wcl$, tht$, cus$, inf$ = "."
            sav_mdl$ = " "                                /*EWD012*/
            return



        build_summary                             /* EWD005 - New */
      
          coil% = 0%                              /*  EWD010      */
*EWD006   if tdi$ = "99" then L23400    /*Coil*/  
          for x% = 1% to 6% step 5%
            readkey$ = " "  :  tot%, hit% = 0%      /*EWD006*/
            str(readkey$,,3%) = str(tht$,x%,3%)
            if readkey$ = " " then L23300
            str(readkey$,7%,2%) = tdi$              /*EWD006*/
/*EWD006*/  if pos(wcl$ = "H") <> 0% then str(readkey$,9%,1%) = "H"
/*EWD006*/  if tdi$ = "99" then str(readkey$,,6%) = str(wcl$)
/*EWD010*/

                                                    /* (EWD015) - BEG  */
                                                    /* Take out table driven */
REM            if str(wk_key$,7%,3%) = "036" then str(readkey$,,6%) = wcl$   
/*EWD013*/
REM            if str(wk_key$,7%,3%) = "025" then str(readkey$,,6%) = wcl$   
REM            if str(wk_key$,7%,3%) = "026" then str(readkey$,,6%) = wcl$   

/*EWD014*/
REM            if str(wk_key$,7%,3%) = "027" then str(readkey$,,6%) = wcl$   
REM            if str(wk_key$,7%,3%) = "028" then str(readkey$,,6%) = wcl$   
REM            if str(wk_key$,7%,3%) = "052" then str(readkey$,,6%) = wcl$   
REM            if str(wk_key$,7%,3%) = "051" then str(readkey$,,6%) = wcl$
               gosub lookup_block_tackle   

                                                     /* (EWD015) - END  */
 

           if str(wk_key$,7%,3%) <> "050" then goto writewrkfile  /*EWD010*/
           goto writecoil
          
      writewrkfile

            read #8, hold, key = readkey$, using L23100, readkey$, tot%, ~
                eod goto L23200, data goto L23400
L23100:             fmt CH(09), BI(4)               /*EWD006*/
            hit% = 1%                               /*EWD006*/
L23200:                                             /* (EWD008)         */ 

            
                        

   
            if days% <= 21% then tot% = tot% + 1%                       ~
                            else tot% = tot% + sc_mqty%   /* Line Item Qty    */
                                                   /* (EWD009)  */

            if tdi$ = "99" then tot% = tot% + sc_mqty% /* Line item Qty */
                                                 /*x2 for Coils - EWD006*/




            put #8, using L23100, readkey$, tot%
/*EWD006*/  if hit% = 0% then write #8, eod goto L23400, data goto L23400 ~
                else rewrite #8, data goto L23400   /*EWD006*/
            if tdi$ = "99" then L23400              /*EWD006*/
            if str(wk_key$,7%,3%) = "50" then goto writecoil
L23300:   next x%            
L23400:   return


          writecoil
          init (" ")  readkey$                                                          /*EWD010*/
          if coil% > 2% then return  
          coil% = coil% + 1%
             if coil% = 1% then str(readkey$,1%,6%) = str(wcl$,1%,3%)     ~
                else str(readkey$,1%,6%) = str(wcl$,4%,6%) 
             str(readkey$,7%,2%) = tdi$
             goto writewrkfile
         
                                                                            /*EWD010*/

        lookup_model                                      /* (EWD012) */
            init(" ") readkey$, descr$          
            div_fact% = 0%
            fact = 0.00
            str(readkey$,1%,9%)   = "PLAN TMDL"
            str(readkey$,10%,15%) = mdl$
            read #4,key = readkey$, using L22700, desc$, eod goto no_model

            mdl$ = str(desc$,1%,3%)     
            convert str(desc$,12%,1%) to div_fact%, data goto no_model

            convert str(desc$,22%,5%) to fact, data goto no_model

            wdt = wdt / div_fact%
            wdt = wdt - fact

        no_model
        return                                            /* (EWD012) */

        lookup_block_tackle                               /* (EWD015) */
            init(" ") gen_key$      
            str(gen_key$,1%,9%)   = "PLAN TBLK"
            str(gen_key$,10%,15%) = str(wk_key$,7%,3%)
            read #4,key = gen_key$, eod goto not_block_tackle

REM                  convert x% to cmg$, pic(0)
REM                  call "SHOSTAT" (" X --> " & cmg$)  stop

                  init(" ") wcl$
                  if x% = 1 then str(wcl$,4%,3%) = str(windcl$(j%),1%,3%) ~ 
                     else str(wcl$,4%,3%) = str(windcl$(j%),4%,3%)

                  str(readkey$,,6%) = wcl$ 
REM                  call "SHOSTAT" (" READKEY --> " & readkey$)  stop
        not_block_tackle
        return                                            /* (EWD015) */


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            if fieldnr% = 3% and sc_data_flg$ = " " then sc_data_flg$="Y"
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Production Date Range                                ",~
         "Enter a Valid Department Code or 'ALL' (Summary Report Only) ",~
         "Enter 'Y' for Glass, Screen & Model/Cust Info else Enter 'N'."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      prddate$, part$, windcl$(), billto$, sc_data_flg$, ~
/*EWD005*/            seqn$, mdl$, wdt$, hgt$, gls$, scr$, hist_flag$,   ~
                      tdi$, sc_prddate_fr$, sc_prddate_to$, tubedi$(),   ~
/* (EWD009) */        prddate_fr$, prddate_to$

            init(" ") wcl$, tht$, cus$, inf$ 

            f% = 7%                                     /*EWD005*/
            if work%  <> 0% then gosub delete_work
            f% = 8%                                     /*EWD005*/
            if work2% <> 0% then gosub delete_work      /*EWD005*/

        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40170,          /* sc_prddate$        */~
                                L40160,          /* sc_dept$           */~
                                L40160           /* sc_data_flg$       */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
/*EWD005*/     at (01,64), "Today:",                                     ~
/*EWD005*/     at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(39),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Prod. Date Range:",                          ~
               at (03,25), fac(lfac$(1%)), sc_prddate_fr$       , ch(10),~
               at (03,40), fac(lfac$(1%)), sc_prddate_to$       , ch(10),~
/*EWD005*/     at (03,55), fac(hex(84)),   hist_flag$           , ch(09),~
                                                                         ~
               at (04,02), "Dept Code:",                                 ~
               at (04,25), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,02), "Print Addtl. Data?:",                        ~
               at (05,25), fac(lfac$(3%)), sc_data_flg$         , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
/*EWD005*/  pf$(2) = "                  (9)Print Summary Repor" &        ~
                     "t                      (15)Print Screen"
/*EWD005*/  pf$(3) = "                 (10)Print Detail Report" &        ~
                     "                       (16)Exit Program"
/*EWD005*/  pfkeys$ = hex(01ffffffffffffff090affffffff0f1000)
/*EWD005*/  if sc_dept$ <> "ALL" then L40690
/*EWD005*/      str(pf$(3),18,23) = " "  :  str(pfkeys$,10,1) = hex(ff)
L40690:     return

L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return




        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Production Date Range  */~
                              L50050,        /* Department Code        */~
                              L50090         /* Print Addtl. Data?     */

            return

L50010: Rem Enter a Production Date Range          sc_prddate_..$
/*EWD005*/  hist% = 0%  :  hist_flag$ = " "
            call "DATEOKC" (sc_prddate_fr$, date_fr%, errormsg$)/*EWD005*/
                if errormsg$ <> " " then goto L50015
            if sc_prddate_to$ = " " then sc_prddate_to$ = sc_prddate_fr$
            call "DATEOKC" (sc_prddate_to$, date_to%, errormsg$)/*EWD005*/
                if errormsg$ <> " " then goto L50015
            if date_fr% > date_to% then                        /*EWD005*/~
                errormsg$ = "'TO' date must be < or = 'FROM' date."
            if errormsg$ <> " " then goto L50015                /*EWD005*/
/*EWD005*/  if date_fr% > today% and date_to% > today% then goto L50015
/* Begin*/          /*  ^---- EWD006          ^---- EWD006  */
ask_again:  k% = 2%
 /*EWD006*/ call "ASKUSER" (k%,"**Select Format**","One/both of the dat" ~
 /*EWD006*/   & "es you have entered are today or before.","Press <PF1>" ~
              & " to Create a Historical Report -OR-","Press <ENTER> to" ~
              & " Create a Standard Report.")
            if k% = 1% then hist% = 1%
/*EWD005*/  if k% <> 0% and hist% = 0% then goto ask_again
/* End  */  if hist% <> 0% then hist_flag$ = "(HISTORY)"

L50015:     return

L50050: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if sc_dept$ <> "ALL" then goto L50060       /*EWD005*/
                sc_dept_d$ = "*** All Departments"      /*EWD005*/
                return                                  /*EWD005*/
L50060:     gosub check_dept
            if dept% = 0% then goto L50080
               sc_dept_d$ = desc$
        return
L50080:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return

L50090: Rem Enter 'Y' for Additional Data or 'N'    sc_data_flg$
            if sc_data_flg$ = "Y" or sc_data_flg$ = "N" then goto L50095
                errormsg$ = "Please Enter 'Y' for Yes or 'N' for No."
L50095:     return


        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #4,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return

        find_dept
            dept% = 0%
            init(" ") pl_key$, dept$
            str(pl_key$,1%,3%) = mdl$
        find_dept_nxt
            read #9,key 3% > pl_key$, using L51015,pl_key$, eod goto L51020
L51015:        FMT CH(15)
            if str(pl_key$,1%,3%) <> mdl$ then goto L51020
               gosub check_support
               if supp% = 1% then goto find_dept_nxt
               dept$ = str(pl_key$,11%,3%)               /* Dept Found     */
               dept% = 1%
L51020: return
 
        check_support
           supp% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = str(pl_key$,11%,3%)
           read #4,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return

        REM *************************************************************~
            *          R E P O R T   G E N E R A T I O N                *~
            *************************************************************

        generate_report
            if ln% > 58% then gosub print_headings
            if prddate$ <> sav_prddate$ and ln% > 5%                     ~
                then gosub print_headings
            if sc_data_flg$ = "N" then gls$, scr$, cus$, inf$ = " "
/*EWD003*/  readkey$ = "PLAN TDHL" & "D" & sc_dept$ & tdi$
/* Begin*/      gosub read_gencds2
                if found% <> 0% then tddiff$ = "*"
            readkey$ = "PLAN TDHL" &"M"& mdl$ & tdi$
/*EWD003*/      gosub read_gencds2
/* End  */      if found% <> 0% then tddiff$ = "*"
            print using L64040,seqn$, mdl$, wdt$, hgt$, gls$, scr$, wcl$,~
                tht$, tdi$, tddiff$, cus$, inf$
            ln% = ln% + 1%
            return

        print_headings
/*EWD005*/  if pg% = 0% then apc$ = apc$ & hist_flag$
            pg% = pg% + 1%
            print page
            print using L64000, date$, time$, apc$, userid$, pg%
            print using L64010, prddate$, sc_dept$, sc_dept_d$,          ~
                    rpt_date_rnge$
            print
            if keyhit% <> 9% then goto L63300   /*EWD005 - Begin*/
                print using L64070
                print using L64080
                goto L63350                     /*EWD005 - End  */
L63300:     if sc_data_flg$ = "Y"                                        ~
                then print using L64020                                  ~
                else print using L64050
            if sc_data_flg$ = "Y"                                        ~
                then print using L64030                                  ~
                else print using L64060
L63350:     ln% = 5%
            return
    

        generate_summary_report                 /* EWD005 - New */
            mode2% = 2%  :  f% = 8%  :  gosub open_work
            apc$ = "(EWD) Tube/Coil Reqrmts Summary         "   /*EWD006*/
            apc$ = apc$ & hist_flag$
            prddate$ = "N/A"
            sav_tht$ = " "
            readkey$ = all(hex(00))
          summary_rpt_loop
            tht$, tdi$, hd_flag$ = " "
            read #8, key > readkey$, using L63400, readkey$, tot%,       ~
                eod goto summary_rpt_done, data goto summary_rpt_loop
L63400:             fmt CH(9), BI(4)            /*EWD006*/
            tht$ = str(readkey$,,6%)            /*   |  */
            tdi$ = str(readkey$,7%,2%)          /*   |  */
            hd_flag$ = str(readkey$,9%,1%)      /*EWD006*/
            if tht$ = sav_tht$ or ln% <= 5% then goto L63450             
                print  :  ln% = ln% + 1%
L63450:     if ln% > 58% then gosub print_headings
            print using L64090, tht$, tdi$, hd_flag$, tot%
            ln% = ln% + 1%
            sav_tht$ = tht$
            goto summary_rpt_loop

          summary_rpt_done
            return

            
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L64000: %  Date: ##########  Time: ########       #######################~
        ~################# (EWDPLA60)              User: ###  Page: ####

L64010: %  Production Date: ##########     Dept. ###  ###################~
        ~###########     Production Date Range: ######################## 

L64020: % Seq.  Mdl Width   Height  Gl S Wnd/Cl Tube Hgts TD   Customer  ~
        ~Model/Customer Info

L64030: % ----- --- ------- ------- -- - ------ --------- --   --------- ~
        ~-------------------

L64040: % ##### ### ####### ####### ## # ###### ######### ## # ######### ~
        ~###################

L64050: % Seq.  Mdl Width   Height       Wnd/Cl Tube Hgts TD             ~
        ~                 

L64060: % ----- --- ------- -------      ------ --------- --             ~
        ~

L64070: % Tube/Cl   TD  *   Pairs Reqd.                                  ~
        ~

L64080: % --------  --  -   -----------                                  ~
        ~

L64090: % ######   ##  #     #######-                                   ~
        ~


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

*       display_codes
*           call "APCPLN1B" (tab%, #4)
*       return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_work                               /* EWD005 - Begin */
            if (mode% = 1% and f% = 7%) or (mode2% = 1% and f% = 8%)     ~
                then mode$ = "OUTPT" 
            if (mode% = 2% and f% = 7%) or (mode2% = 2% and f% = 8%)     ~
                then mode$ = "INPUT"
            if (mode% = 3% and f% = 7%) or (mode2% = 3% and f% = 8%)     ~
                then mode$ = "SHARE"            /* EWD005 - End */

            call "WORKOPN2" (#f%, mode$, 3000%, f2%)    /*EWD005*/
            if f2% <> 0% then goto L64580
            if f% = 7% then work%  = 1%                 /*EWD005*/
            if f% = 8% then work2% = 1%                 /*EWD005*/
        return
L64580:     errormsg$ = "ERROR - CANNOT OPEN (APCPLNWK) for " & mode$
            if f% = 8% then str(errormsg$,27%,3%) = "WK2"   /*EWD005*/
            call "SHOSTAT" (errormsg$) : stop
        return clear all
        goto inputmode

        delete_work
            call "FILEBGON" addr(#f%)           /*EWD005*/
            if f% = 7% then work%  = 0%         /*EWD005*/
            if f% = 8% then work2% = 0%         /*EWD005*/
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            close printer
/*EWD005*/  f% = 7%
            if work%  <> 0% then gosub delete_work
/*EWD005*/  f% = 8%
/*EWD005*/  if work2% <> 0% then gosub delete_work
            end

