REM                                                                     ~
************************************************************************~
*                            ( R e p o r t s )                         *~
*                                                                      *~
*                           ( As of 01/03/2018 - CMN )                 *~
*        EWDEFF1B - Efficiency Detail Table Report                     *~
*----------!-----------------------------------------------------!-----*~
*          !                                                     !     *~
* 01/31/01 ! Mod to change column 5 to overtime % per Keith EWD001 CMG *~
* 02/28/01 ! Mod to add table by year & wk to get month.    EWD002 CMG *~
* 09/19/03 ! Mod to add new indirect dept                   EWD003 CMG *~
*          !            just like '044'                          !     *~
* 08/10/04 ! (EWD005) Mod to get rid of extra line and space so  ! CMG *~
*          !          report will print on one page.             !     *~
* 01/04/05 ! (AWD006) Mod to add glass back to subtotal but not  ! CMG *~
*          !          plant total                                !     *~
* 01/04/05 ! (AWD007) Changes to column 5 and scrap              ! CMG *~
*          !          column 10 per Libby Shadix                 !     *~
* 02/01/05 ! (AWD008) Mod to have actual production dollar       ! CMG *~
* 02/25/07 ! (AWD009) Mod to have mulls back in subtotal         ! CMG *~
* 08/27/07 ! (AWD010) Mod to add net scrap summary               ! DES *~
* 09/18/08 ! (AWD011) Mod to values of scrap summary             ! DES *~
*02/19/2010! (AWD012) drop samples from dept 021 plant total     ! DES *~
*01/12/2010! (AWD013) add dept 42 & 71                           ! DES *~
*08/12/2011! (AWD014) modify variance lobor dollar calculations  ! DES *~
*02/03/2012! (AWD015) remove plan units & eff % add UPMH act&goal! DES *~
*04/19/2012! (AWD016) move RGA before total & include            ! DES *~
*04/29/2014! (AWD017) Change UPMH goals back to weighted avg.  & ! PWW *~
*          !          add dept 8900 back to summary totals.      !     *~
*05/30/2014! (AWD018) Change the way in which summary total scrap! PWW *~
*          !          dollars accumulate back to original.       !     *~
*12/12/2014! (AWD019) Since we are printing the variable tot_cl10! PWW *~
*          !          (labor goal/unit) then we should use it in !     *~
*          !          the calculations. Sometimes tot_col10 is 0 !     *~
*          !          in which case we will use tot_cl10.        !     *~
*02/04/2016! SR72515  Modified the above mod AWD019 to always use! PWW *~
*          !          (labor goal/unit) tot_cl10 both while      !     *~
*          !          printing and in the calculations.          !     *~
*01/04/2017! SR79058  Per Doug E.  add department 9& 74 back into! PWW *~
*          !          the sub total for Column #1.               !     *~
*          !                                                     !     *~
*02/16/2017! SR79464  Per Doug E.  add department 19&53 back into! PWW *~
*          !          scrap summary.                             !     *~
*          !                                                     !     *~
*01/03/2018! CR1253   Add Utility to INDIRECT auto_tab$ dept     ! CMG *~   
*01/10/2019! CR1864   Add department 000 to the indirect array   ! RDB *~
*          !          per request from Doug Everhart.            !     *~  
*10/05/2020! CR2692   Add MPO calculation to the report          ! RDB *~ 
************************************************************************

        sub "EWDEFF1B" (sc_yr$,        /* Production Year       */~
                        sc_wk$,        /* Production Week       */~
                        sc_dy$,        /* Production Day        */~
                        sc_wk_dte$,    /* Screen Date           */~
                        pr_wk_dte$,    /* Production Date       */~
                        schema%,       /* Schema code           */~
                        #1,            /* EWDEFFCY              */~
                        #2,            /* GENCODES              */~
                        #13 )          /* EWDEFFEX              */



        dim sc_yr$4, ex_year$4,        /*   Screen Year              */~
            sc_wk$2,                   /*   Screen Week              */~
/*EWD002*/  rd_wk$2,                   /*   Read Screen Week         */~
            sc_dy$1,                   /*   Screen Day               */~
            sc_wk_dte$8,               /*   Screen Work Date         */~
            yr_wk_dte$8,               /*   Screen Work Date         */~
            pr_wk_dte$8,               /*   Production Work Date     */~
            sc_date$10,                /*   Formated Screen Date     */~
            pr_date$10,                /*   Formated Prod Work Date  */~
            mon_wk$8,                  /* Begin Date of First Of Mon */~
            eff_key$19, eff_key1$19,   /*   Read Key                 */~
            eff_wk_dte$6,              /* Production date - EWDEFFCY */~
            eff_year$4,                /* Production Year            */~
            eff_wk$2,                  /* Production Week            */~
            eff_day$1,                 /* Production Day             */~
            eff_shift$2,               /* Production Shift - '00'    */~
            eff_model$(100%)3,         /* Production Models          */~
            eff_glass(6%),             /* Process Counter            */~
            sc_dept$3,                 /* Production Dept            */~
            report$(4%)8,              /*   Report 1-4               */~
            rpt_end$7,                 /*   Report Date End          */~
            rpt_type$(2%)1,            /*   Report Type P or S       */~
            rpt_type$1, eff_proc$1,    /*   Report Type              */~
            ex_upmh(12%),              /* Planning UPMH              */~
            readkey$50,                /*   Generic Readkey          */~
            desc$30,                   /*   Generic Description      */~
            readkey1$50,               /*   Generic Readkey 1        */~
            sav_key1$9,                /*   Generic Savekey          */~
            hh$40,                     /* Open Error Header Msg      */~
            msg$(3%)79,                /* ASKUSER TEXT               */~
            errormsg$79,               /* Error message              */~
            auto_tab$(15%)3,           /* AUTO LOAD DEPT'S           */~
            ind_dept$(40%)3,           /* Indirect Department Values */~
            col1$(5%)14,               /* Daily,Weekly,Monthly,Yearly*/~
            col2$(4%)7,                /* Data Description           */~
            dept$(300%)3,              /*   Department               */~
         /* dept%, rpt%                                           */ ~
            plt_tot_hrs(4%),          /* Hours per Department   AWD015 */~
            mrg_tot_hrs(4%),          /* Hours per Department   AWD015 */~
            tot_tot_hrs(300%,4%),          /* Hours per Department   AWD015 */~
            plt_tot_unt(4%),          /* Units per Department   AWD015 */~
            tot_tot_unt(300%,4%),          /* Units per Department   AWD015 */~
            plt_pln_unt(4%),          /* Planned Units per Dept AWD015 */~
            mrg_pln_unt(4%),          /* Planned Units per Dept AWD015 */~
            tot_pln_unt(300%,4%),          /* Planned Units per Dept AWD015 */~
            dept_desc$(300%)30,        /* Rpt Desc by Dept           */~
            dept_desc$11               /* Rpt Desc for Plant Total   */

/* <AWD010> */
        dim cnv_tbl$(100)10         /* 1-3 = dept                 */
                    /* 4 = BU,  4=glass, 5=plant  */
                    /* 5 = type VAWG              */
                    /* 6-10 = for future use      */
        dim type_tot(5)             /* type=Vinyl, Glass, Wood, Alum, Tot */
        dim type_cost(5)
/* </AWD010> */
        dim message$256

        dim indir_per(3%,4%),          /* Save col7 for dept 13,15,24*/~
            indir_wg(3%,4%),           /* Indirect Save Percents     */~
            tot_col1(300%,4%),         /* Total Col1 for Prod & Scrap*/~
            tot_col2(300%,4%),         /* Total Col2 - by Dept       */~
            tot_col3(300%,4%),         /* Total Col3                 */~
            tot_col4(300%,4%),         /* Total Col4                 */~
            tot_col8(300%,4%),         /* Total Col5                 */~
            tot_col5(300%,4%),         /* Total Col6                 */~
            tot_col6(300%,4%),         /* Total Col7                 */~
            tot_col7(300%,4%),         /* Total Col8                 */~
            tot_col9(300%,4%),         /* Total Col9                 */~
            tot_col10(300%,4%),        /* Total Col10                */~
            tot_col11(300%,4%),        /* Total Col11                */~
            tot_col12(300%,4%),        /* Total Col12                */~
            tot_col13(300%,4%),        /* Total Col13                */~
            tot_col14(300%,4%),        /* MPO CR2692                 */~
            tot_avg(300%,4%),          /* Total Avg Weight by Dept   */~
            tot_weight(300%,4%),       /* Total Mistake Weight - Dept*/~
            tot_hrse(300%,4%),         /* Total Hours Earned         */~
            pnt_col1(4%),              /* Plant Col1 for Prod & Scrap*/~
            pnt_col2(4%),              /* Plant Col2 - by Dept       */~
            pnt_col3(4%),              /* Plant Col3                 */~
            pnt_col4(4%),              /* Plant Col4                 */~
            pnt_col8(4%),              /* Plant Col5                 */~
            pnt_col5(4%),              /* Plant Col6                 */~
            pnt_col6(4%),              /* Plant Col7                 */~
            pnt_col7(4%),              /* Plant Col8                 */~
            pnt_col9(4%),              /* Plant Col9                 */~
            pnt_col10(4%),             /* Plant Col10                */~
            pnt_col11(4%),             /* Plant Col11                */~
            pnt_col12(4%),             /* Plant Col12                */~
            pnt_col13(4%),             /* Plant Col13                */~
            pnt_col14(4%),             /* MPO calculation   CR2692   */~
            pnt_avg(4%),               /* Plant Average              */~
            tot_wgi(4%),               /* Total Wage Indirect        */~
            pnt_hrse(4%),              /* Plant Hours                */~
            tot_wgt(4%),               /* Total Wages                */~
            tot_hrsd(4%),              /* Total Hours                */~
            pnt_weight(4%),            /* PlanT Weight               */~
            indir_wg(4%),              /* Indirect Wages             */~
            pnt_col1_dir(4%),          /* Plant Col1 Direct          */~
            pnt_col2_dir(4%),          /* Plant Col2 Direct          */~
            pnt_col11_dir(4%),         /* Plant Col11 Direct         */~
            pnt_wg(4%),                /* Plant Wages                */~
            pnt_col8_dir(4%),          /* Plant Col8 Direct          */~
            pnt_dir(4%),               /* Plant Direct               */~
            pnt_ind(4%),               /* Plant Indirect             */~
            tot_dir(4%),               /* Total Direct               */~
            indir_per(4%),             /* Indirect Per               */~
            scrap_cnt%(4%),            /* Scrap Counter              */~
            scrap_dir%(4%)             /* Scrap Direct Counter       */

        dim goal$(1000)16
        dim sub_goal(99)
/* <AWD015> */
            mat sub_tot_hrs   = zer
            mat sub_tot_unt   = zer
            mat sub_pln_unt   = zer
            dept_05% = 0%
            dept_07% = 0%
            dept_49% = 0%
/* </AWD015> */

REM            ind_col% = 21%             /* Where indirect starts in dept */
REM - DAILY, WEEKLY, MONTH, YEAR
            col1$(1%) = "  D a i l y   "  : col1$(3%) = "M o n t h l y "
            col1$(2%) = " W e e k l y  "  : col1$(4%) = " Y e a r l y  "

            col2$(1%) = " Daily "         : col2$(2%) = "Weekly "
            col2$(3%) = "Monthly"         : col2$(4%) = "Yearly "

REM - AUTO TAB IS INDIRECT DEPTS & IND_DEPT IS SUPPORT & INDIRECT DEPTS
            auto_tab$( 1%) = "015"   : auto_tab$( 8%) = "030"
            auto_tab$( 2%) = "024"   : auto_tab$( 9%) = "035"
            auto_tab$( 3%) = "013"   : auto_tab$(10%) = "001"
            auto_tab$( 4%) = "039"   : auto_tab$(11%) = "071"
            auto_tab$( 5%) = "031"   : auto_tab$(12%) = "026" /*(CR1253) */
            auto_tab$( 6%) = "061"   : auto_tab$(13%) = "082"
            auto_tab$( 7%) = "045"   : auto_tab$(14%) = "   "
            auto_tab$(15%) = "   "
   
            if schema% = 2% then goto not_TX_indirect
            ind_dept$(1%)  = "012" : ind_dept$(2%)  = "013"
            ind_dept$(3%)  = "015" : ind_dept$(4%)  = "022"
            ind_dept$(5%)  = "024" : ind_dept$(6%)  = "030"
            ind_dept$(7%)  = "031" : ind_dept$(8%)  = "032"
            ind_dept$(9%)  = "034" : ind_dept$(10%) = "035"
            ind_dept$(11%) = "037" : ind_dept$(12%) = "038"
            ind_dept$(13%) = "039" : ind_dept$(14%) = "045"
            ind_dept$(15%) = "061" : ind_dept$(16%) = "010"
            ind_dept$(17%) = "001" : ind_dept$(18%) = "074"
            ind_dept$(19%) = "060" : ind_dept$(20%) = "041"
            ind_dept$(21%) = "057" : ind_dept$(22%) = "026" /* (CR1253) */
            ind_dept$(21%) = "000" : ind_dept$(22%) = "082" /* CR1864 */
            ind_dept$(23%) = "   " : ind_dept$(24%) = "   "
            ind_dept$(25%) = "   " : ind_dept$(26%) = "   "
            ind_dept$(27%) = "   " : ind_dept$(28%) = "   "
            ind_dept$(29%) = "   " : ind_dept$(30%) = "   "
            ind_dept$(31%) = "   " : ind_dept$(32%) = "   "
            ind_dept$(33%) = "   " : ind_dept$(34%) = "   "
            ind_dept$(35%) = "   " : ind_dept$(36%) = "   "
            ind_dept$(37%) = "   " : ind_dept$(38%) = "   "
            ind_dept$(39%) = "   " : ind_dept$(40%) = "   "

             goto indirect_set
not_TX_indirect:
            init(" ") ind_dept$()
            ind_dept$(1%)  = "082" : ind_dept$(2%)  = "078"
            ind_dept$(3%)  = "079" : ind_dept$(4%)  = "084"
            ind_dept$(5%)  = "086" : ind_dept$(6%)  = "007"
            ind_dept$(7%)  = "067" : ind_dept$(8%)  = "068"
            ind_dept$(9%)  = "072" : ind_dept$(10%) = "075"
            ind_dept$(11%) = "074" : ind_dept$(12%) = "096"
            ind_dept$(13%) = "092" : ind_dept$(14%) = "001"
            ind_dept$(15%) = "   " : ind_dept$(16%) = "   "
            ind_dept$(17%) = "   " : ind_dept$(18%) = "   "
            ind_dept$(19%) = "   " : ind_dept$(20%) = "   "
            ind_dept$(21%) = "   " : ind_dept$(22%) = "   "
            ind_dept$(23%) = "   " : ind_dept$(24%) = "   "
            ind_dept$(25%) = "   " : ind_dept$(26%) = "   "
            ind_dept$(27%) = "   " : ind_dept$(28%) = "   "
            ind_dept$(29%) = "   " : ind_dept$(30%) = "   "
            ind_dept$(31%) = "   " : ind_dept$(32%) = "   "
            ind_dept$(33%) = "   " : ind_dept$(34%) = "   "
            ind_dept$(35%) = "   " : ind_dept$(36%) = "   "
            ind_dept$(37%) = "   " : ind_dept$(38%) = "   "
            ind_dept$(39%) = "   " : ind_dept$(40%) = "   "

indirect_set:

/* (EWD003) */
            init(" ") sc_date$, pr_date$, mon_wk$
            mon_wk%, sc_wk% = 0%
            tot_ex = 0.0

/* <AWD013> */
        first_p = 1
        first_s = 1
/* </AWD013> */

/* <AWD010> */
            /* 1-3 = dept                 */
            /* 4 = BU,  4=glass, 5=plant  */
            /* 5 = type VAWG              */
            /* 6-10 = for future use      */

            init(" ") cnv_tbl$
        cnv_tbl$(01) = "0002A     "
        cnv_tbl$(02) = "0015A     "
        cnv_tbl$(03) = "0023V     "
        cnv_tbl$(04) = "0051V     "
        cnv_tbl$(05) = "0073V     "
        cnv_tbl$(06) = "0081V     "
        cnv_tbl$(07) = "0092V     "
        cnv_tbl$(08) = "0173V     "
        cnv_tbl$(09) = "0183V     "
        cnv_tbl$(10) = "0231V     "
        cnv_tbl$(11) = "0261V     "
        cnv_tbl$(12) = "0271V     "
        cnv_tbl$(13) = "0281V     "
        cnv_tbl$(14) = "0361V     "
        cnv_tbl$(15) = "0374G     "
        cnv_tbl$(16) = "0384G     "
        cnv_tbl$(17) = "0432V     "
        cnv_tbl$(18) = "0442W     "
        cnv_tbl$(19) = "0472V     "
        cnv_tbl$(20) = "0482V     "
        cnv_tbl$(21) = "0493V     "
        cnv_tbl$(22) = "0414G     "
        cnv_tbl$(22) = "0145V     "       /*<AWD017>*/
        cnv_tbl$(23) = "0195V     "       /*SR79464*/
        cnv_tbl$(24) = "0535V     "       /*SR79464*/
            max_cnvt% = 025%
/* </AWD010> */

        sum_col7 = 0.0
        cnt_col7 = 0.0
        tot_col7 = 0.0
        num_col7 = 0.0
            gosub get_upmhs_goal
            s_goal% = 1%
            sc_date$ = sc_wk_dte$   /* Get beginning week date & */
            pr_date$ = pr_wk_dte$   /* Actual production date    */
            call"DATFMTC"(sc_date$)
            call"DATFMTC"(pr_date$)
            ex_year$ = sc_yr$

            month$ = str(sc_date$,1%,2%)   /* Get current Screen Month */
            convert month$ to month%, data goto L00010
L00010:
            convert ex_year$ to ex_year%, data goto L00020
L00020:
            month% = month% - 1%
            if month% = 0% then ex_year% = ex_year% - 1%
            if month% = 0% then month% = 12%

            convert ex_year% to ex_year$, pic(0000)

REM - GET BEGINNING DATE FOR FIRST WEEK IN MONTH
            convert sc_wk$ to sc_wk%, data goto week_error
            convert sc_wk% to rd_wk$, pic(00)

            if month% = 11% and sc_wk% = 1% then ex_year% = ex_year% - 1%
            convert ex_year% to ex_year$, pic(0000)

            init(" ") readkey$
            str(readkey$,1%,9%) = "APC EFFWK"
            str(readkey$,10%,4%) = sc_yr$
            str(readkey$,14%,2%) = rd_wk$

            read #2, key = readkey$, using L01000, desc$,      ~
                                           eod goto week_error
L01000:          FMT XX(24), CH(30)
            convert str(desc$,1%,2%) to mon_wk%, data goto week_error

            convert mon_wk% to mon_wk$, pic(00)

            init(" ") readkey$    :       mon_wk% = 0%
            str(readkey$,1%,9%) = "APC EFFMN"
            str(readkey$,10%,4%) = sc_yr$
            str(readkey$,14%,2%) = str(mon_wk$,1%,2%)
/* (EWD002) */
            read #2, key = readkey$, using L01000, desc$,      ~
                                           eod goto month_error

            convert str(desc$,1%,8%) to mon_wk%, data goto month_error

            convert mon_wk% to mon_wk$, pic(########)

            call"DATFMTC"(mon_wk$)
            call"DATUFMTC"(mon_wk$)
REM - SET ALL 4 REPORT BEGINNING POINTS -  END IS ALWAY CURRENT PROD DATE
            report$(1%) = pr_wk_dte$
            report$(2%) = sc_wk_dte$
            report$(3%) = mon_wk$
/* (EWD002) */
            init(" ") readkey$    :  mon_wk% = 0%
            str(readkey$,1%,9%) = "APC EFFMN"
            str(readkey$,10%,4%) = sc_yr$
            str(readkey$,14%,2%) = "01"

            read #2, key = readkey$, using L01000, desc$,      ~
                                          eod goto month_error

            convert str(desc$,1%,8%) to mon_wk%, data goto month_error

            convert mon_wk% to yr_wk_dte$, pic(########)
/* (EWD002) */
            call"DATFMTC"(yr_wk_dte$)
            call"DATUFMTC"(yr_wk_dte$)
            report$(4%) = yr_wk_dte$
            rpt_end$    = pr_wk_dte$
            rpt_type$(1%) = "P"       /* Rpt for Production and Scrap */
            rpt_type$(2%) = "S"

         gosub initialize_plant
         for j% = 1% to 2%
           tot_ex = 0.0
           p_no% = 0%
           gosub set_initial
           gosub initialize_variables
           init(" ") eff_key$
           rga_sw = 0
           rpt_type$ = rpt_type$(j%)
           str(eff_key$,1%,6%) = report$(4%)
         begin_process_next1
           read #1, key 1% > eff_key$, using L01030, eff_key$, ~
                                      eod goto process_done1
L01030:        FMT CH(19)
           if str(eff_key$,1%,6%) > rpt_end$ then goto process_done1
           if str(eff_key$,7%,1%) <> rpt_type$ then goto begin_process_next1
/*SR72515  gosub get_record            read until passed ending date */
           for dept% = 1% to p_no%
               if dept$(dept%) = str(eff_key$,15%,3%) then goto dept_found
           next dept%
           goto begin_process_next1            /* No dept Found  */
        dept_found
           gosub get_record         /* read until passed ending date */
           indirect% = 0%
           for q% = 1% to 15%
REM               if dept$(k%) = auto_tab$(q%) then indirect% = 1%
               if sc_dept$ = auto_tab$(q%) then indirect% = 1%
           next q%
           rpt% = 4%
           if dept$ = "037" and schema% = 1% then gosub get_glass
           if dept$ = "074" and schema% = 2% then gosub get_glass
           if dept$ = "075" and schema% = 2% then gosub get_glass

           gosub convert_data
           if str(eff_key$,1%,6%) >= str(report$(3%),1%,6%)  ~
                                      then gosub convert_month
           if str(eff_key$,1%,6%) >= str(report$(2%),1%,6%)  ~
                                      then gosub convert_week
           if str(eff_key$,1%,6%) >= str(report$(1%),1%,6%)  ~
                                      then gosub convert_day

           goto begin_process_next1
        process_done1
            if rpt_type$ = "P" then gosub convert_prod
            if rpt_type$ = "S" then gosub convert_scrap
            gosub initialize_variables

          next j%
          goto exit_sub

        get_record
            get #1, using L01170, eff_wk_dte$, eff_proc$, eff_year$,   ~
                    eff_wk$, eff_day$, sc_dept$, eff_shift$,           ~
                    eff_model$(), eff_unt(), eff_unts(), eff_untss(),  ~
                    eff_untpp(), eff_unta(), eff_untb(), eff_untc(),   ~
                    eff_col1, eff_col2, eff_col3, eff_col4, eff_col5,  ~
                    eff_col6, eff_col7, eff_col8, eff_col9, eff_col10, ~
                    eff_col11, eff_col12, eff_col13, eff_hrsm,         ~
                    eff_paym,  eff_glass(), eff_scan, mistake_weight, ~
                    eff_hrse, eff_avg, eff_value             /* (AWD008) */
/*SR72515 + */
REM         gosub get_labor_goald
REM         if rpt_type$ <> "P" or tot_cl10 =0 or eff_col1 = 0     ~
REM                                then goto skip_col11s

REM           eff_col10 = tot_cl10
REM           eff_col11 = round((eff_col10 - eff_col9) * eff_col1,2)
        skip_col11s
/*SR72515 - */

        return
REM - ADD UP ALL TOTALS
        convert_month
          rpt% = 3%
          goto convert_data
        convert_week
          rpt% = 2%
          goto convert_data
        convert_day
          rpt% = 1%
          call "SHOSTAT" (" DAILY " )
        convert_data
REM AWD016 if dept$(dept%) = "032" then goto L20020  /* DO NOT INCLUDE RGA IN PLANT */
                                                     /* DO NOT INCLUDE RGA IN PLANT */
         if dept$(dept%) = "021" and schema% = 1% then goto L20020
         if dept$(dept%) = "007" and schema% = 2% then goto L20020

            tot_avg(dept%,rpt%)  =  tot_avg(dept%,rpt%) + eff_avg
            tot_weight(dept%,rpt%) = tot_weight(dept%,rpt%) + mistake_weight
            tot_col10(dept%,rpt%) = tot_col10(dept%,rpt%) + eff_col10

            tot_col11(dept%,rpt%) = tot_col11(dept%,rpt%) + eff_col11
REM            init(" ") cmg$

REM            convert tot_col11(dept%,rpt%) to cmg$, pic(#######.####-)
REM            if sc_dept$ = "050" and rpt% = 4% then                      ~
REM               call "SHOSTAT" (" I am adding dept 050 " & eff_year$ & ~
REM                                 " " & eff_wk$ & " " & eff_day$ & " " &cmg$)
REM            if sc_dept$ = "050" and rpt% = 4% then stop
REM - GET INDIRECT & DIRECT VALUES FOR PRODUCTION CALC PNT_COL7
            if indirect% = 0% then pnt_hrse(rpt%) =  pnt_hrse(rpt%) + eff_hrse
            if indirect% = 0% then tot_hrsd(rpt%) =  tot_hrsd(rpt%) + eff_col4
            if sc_dept$ = "013" then tot_wgi(rpt%) =  tot_wgi(rpt%) + eff_col4
            if sc_dept$ = "015" then tot_wgi(rpt%) =  tot_wgi(rpt%) + eff_col4
            if sc_dept$ = "024" then tot_wgi(rpt%) =  tot_wgi(rpt%) + eff_col4
            if indirect% = 1% then tot_wgt(rpt%) =  tot_wgt(rpt%) + eff_col4
            pnt_col2(rpt%)  = pnt_col2(rpt%) + eff_col2
            pnt_col3(rpt%)  = pnt_col3(rpt%) + eff_col3
            pnt_col4(rpt%)  = pnt_col4(rpt%) + eff_col4
            pnt_col8(rpt%)  = pnt_col8(rpt%) + eff_col8
            pnt_col5(rpt%)  = pnt_col5(rpt%) + eff_col5
            pnt_col6(rpt%)  = pnt_col6(rpt%) + eff_col6
            pnt_col7(rpt%)  = pnt_col7(rpt%) + eff_col7
            pnt_col9(rpt%)  = pnt_col9(rpt%) + eff_col9

/* <AWD015> */
             if rpt_type$ <> "P" then goto skip_calc
             plt_tot_hrs(rpt%) = plt_tot_hrs(rpt%) + eff_col4
             plt_tot_unt(rpt%) = plt_tot_unt(rpt%) + eff_col1
             plt_pln_unt(rpt%) = plt_pln_unt(rpt%) + eff_col6
             if schema% <> 1% then goto skp_nc_dpt_ck
             if dept$(dept%) = "005" then dept_05% = dept%
             if dept$(dept%) = "007" then dept_07% = dept%
             if dept$(dept%) = "049" then dept_49% = dept%
             if dept$(dept%) <> "005" and dept$(dept%) <> "007" and  ~
                dept$(dept%) <> "049" then goto skip_calc
skp_nc_dpt_ck:
             mrg_tot_hrs(rpt%) = mrg_tot_hrs(rpt%) + eff_col4
             mrg_pln_unt(rpt%) = mrg_pln_unt(rpt%) + eff_col6
skip_calc:
/* </AWD015> */
REM - GET VALUES FOR ONLY CALC DIRECT DEPTS ON COL 3,4, 9, 10, 11 ON SCRAP RPT
          for q% = 1% to 40%
            if dept$(dept%) = ind_dept$(q%) then goto L20010
          next q%
          if rpt_type$ <> "S" then goto L20030
            pnt_weight(rpt%) = pnt_weight(rpt%) + mistake_weight
            pnt_avg(rpt%)  =  pnt_avg(rpt%) + eff_avg
            pnt_col2_dir(rpt%) = pnt_col2_dir(rpt%) + eff_col2
            pnt_col8_dir(rpt%) = pnt_col8_dir(rpt%) + eff_col8
            pnt_col1_dir(rpt%) = pnt_col1_dir(rpt%) + eff_col1
            scrap_dir%(rpt%) = scrap_dir%(rpt%) + 1%

          if eff_col11 < .01 then goto L20030
            scrap_cnt%(rpt%) = scrap_cnt%(rpt%) + 1%
            pnt_col11_dir(rpt%) = pnt_col11_dir(rpt%) + eff_col11
            pnt_col10(rpt%) = pnt_col10(rpt%) + eff_col10

REM - Only Include Depts Up To Spec Shapes for Plant Col1
L20030:   if schema% = 2% then goto skp_nc_dpt_ck2
          if dept$(dept%) = "009" or dept$(dept%) = "044" then goto L20010
          if dept$(dept%) = "046" or dept$(dept%) = "000" then goto L20010
          if dept$(dept%) = "054" then goto L20010        /*  (EWD004) */
REM       if dept$(dept%) = "041" then goto L20010        /*  (AWD011) */
          if dept$(dept%) = "029" then goto L20010
/* skip these dept. in Total Plant */
          if dept$(dept%) = "037" or dept$(dept%) = "038" then goto L20010
/* <AWD013> */
          if dept$(dept%) = "042" or dept$(dept%) = "071" then goto L20010
/* </AWD013> */
          if dept$(dept%) = "041" then goto L20010
          if dept$(dept%) = "026" then goto L20010   /* (CR1253) */
          if dept$(dept%) = "082" then goto L20010   /* (CR1253) */
skp_nc_dpt_ck2:
            pnt_col1(rpt%) = pnt_col1(rpt%) + eff_col1

L20010:     if eff_col11 > 0.00 and rpt_type$ = "S" then tot_col11(dept%,rpt%) = eff_col11
/*SR72515   pnt_col11(rpt%) = pnt_col11(rpt%) + eff_col11               */
            pwww_col11 = pnt_col11(rpt%)
            pnt_col12(rpt%) = pnt_col12(rpt%) + eff_col12
            pnt_col13(rpt%) = pnt_col13(rpt%) + eff_col13
            pnt_wg(rpt%)   =  pnt_wg(rpt%)  + eff_col8

L20020:     if indirect% = 0% then                   ~
               tot_hrse(dept%,rpt%) =  tot_hrse(dept%,rpt%) + eff_hrse

            tot_col1(dept%,rpt%)  = tot_col1(dept%,rpt%) + eff_col1
            tot_col2(dept%,rpt%)  = tot_col2(dept%,rpt%) + eff_col2
            tot_col3(dept%,rpt%)  = tot_col3(dept%,rpt%) + eff_col3
            tot_col4(dept%,rpt%)  = tot_col4(dept%,rpt%) + eff_col4
            tot_col8(dept%,rpt%)  = tot_col8(dept%,rpt%) + eff_col8
REM - DO NOT SHOW PAY ON DEPT LINE IF MANGERS HR AND PAY = DAILY VALUE
          if rpt_type$ <> "P" then goto L20000
            if eff_col10 > 0.00 then tot_col10(dept%,rpt%) = eff_col10
            pnt_col10(rpt%) = pnt_col10(rpt%) + (eff_col10 * eff_col1)

          if eff_col2 <> eff_hrsm then goto L20000
          if eff_col8 <> eff_paym or rpt% <> 1% then goto L20000
             eff_col2 = 0
             eff_col3 = 0
             eff_col4 = 0
             eff_col8 = 0
                                                 /* (EWD001) Begin */
L20000:     if rpt_type$ = "S" then                                 ~
                     tot_col5(dept%,rpt%)  = tot_col5(dept%,rpt%) + eff_col5
                                                 /*  (AWD007)    - BEG */
REM            if rpt_type$ = "P" then                                 ~
                     tot_col5(dept%,rpt%)  = round(eff_col3 / eff_col2 * 100, 1)
            if rpt_type$ = "P" then                                 ~
                     tot_col5(dept%,rpt%)  = round(eff_col3 / eff_col4 * 100, 1)
                                                 /*  (AWD007)    - END */


                                                /* (EWD001) End */
            tot_col6(dept%,rpt%)  = tot_col6(dept%,rpt%) + eff_col6
            tot_col7(dept%,rpt%)  = tot_col7(dept%,rpt%) + eff_col7
            tot_col9(dept%,rpt%)  = tot_col9(dept%,rpt%) + eff_col9
            tot_col12(dept%,rpt%) = tot_col12(dept%,rpt%) + eff_col12
            tot_col13(dept%,rpt%) = tot_col13(dept%,rpt%) + eff_col13

        return

        convert_subtotal
            sub_col1, sub_col2, sub_col3, sub_col4, sub_col5,  ~
            sub_col6, sub_col7, sub_col8, sub_col9, sub_col10, ~
            sub_col11, sub_col12, sub_col13, sub_hrse = 0.00

            sub_tot_hrs   = 0
            sub_tot_unt   = 0
            sub_pln_unt   = 0

         for s% = beg% to (x% - 1%)
REM AWD01  if dept$(s%) = "032" then goto LS0020  /* DO NOT INCLUDE RGA IN PLANT */
            sub_col2  = sub_col2 + tot_col2(s%,n%)
            sub_col3  = sub_col3 + tot_col3(s%,n%)
            sub_col4  = sub_col4 + tot_col4(s%,n%)
            sub_col8  = sub_col8 + tot_col8(s%,n%)
                                               /*   (AWD007)  - BEG  */
REM SUB_COL5 = ROUND(SUB_COL3 / SUB_COL2 * 100, 2)
            sub_col5 = round(sub_col3 / sub_col4 * 100, 2)
                                               /*   (AWD007)  - END  */

/* <AWD015> */
            if rpt_type$ <> "P" then goto skip_calc2
             sub_tot_hrs  = sub_tot_hrs + tot_col4(s%,n%)    /*Skip Cutting */
            if dept$(s%) = "038" and schema% = 1% then goto skip_calc2             
             sub_tot_unt  = sub_tot_unt + tot_col1(s%,n%)
             sub_pln_unt  = sub_pln_unt + tot_col6(s%,n%)
skip_calc2:
/* </AWD015> */

          
            sub_col6  = sub_col6 + tot_col6(s%,n%)
            sub_col7  = sub_col7 + tot_col7(s%,n%)
            sub_col9  = sub_col9 + tot_col9(s%,n%)
            sub_col10  = sub_col10 + (tot_col10(s%,n%) * tot_col1(s%,n%))
            sub_hrse =  sub_hrse + tot_hrse(s%,n%)
                                  /*  (AWD006)  */
          if dept$(s%) = "037" and schema% = 1% then goto add_glass
REM IF DEPT$(S%) = "038" AND SCHEMA% = 1% THEN GOTO ADD_GLASS
REM IF DEPT$(S%) = "074" AND SCHEMA% = 2% THEN GOTO ADD_GLASS     
/*SR79058*/if dept$(s%) = "074" then goto add_glass
/*temp*/  if dept$(s%) = "075" and schema% = 2% then goto add_glass
          for q% = 1% to 40%
            if dept$(s%) = ind_dept$(q%) then goto LS0010
          next q%

/*SR79058*/  if schema%<> 1% and dept$(s%) = "009" then goto LS0010          
REM IF DEPT$(S%) = "009" OR DEPT$(S%) = "044" THEN GOTO LS0010
REM IF DEPT$(S%) = "009" THEN GOTO LS0010        
/*(AWD009)*/
REM IF DEPT$(S%) = "046" OR DEPT$(S%) = "000" THEN GOTO LS0010
REM IF DEPT$(S%) = "000" THEN GOTO LS0010
/*(AWD009/)*/
REM IF DEPT$(S%) = "029" THEN GOTO LS0010
REM IF DEPT$(S%) = "054" THEN GOTO LS0010        
add_glass:                                       
REM IF DEPT$(S%) = "042" OR DEPT$(S%) = "071" THEN GOTO LS0010
            
            sub_col1  = sub_col1 + tot_col1(s%,n%)

REM            sub_hrse =  sub_hrse + tot_hrse(s%,n%)
LS0010:
            sub_col11  = sub_col11 + tot_col11(s%,n%)
            sub_col12  = sub_col12 + tot_col12(s%,n%)
            sub_col13  = sub_col13 + tot_col13(s%,n%)
LS0020:  next s%

            sub_col7 = round((sub_hrse / sub_col4) * 100, 2)
            sub_col9  = round(sub_col8 / sub_col1, 2)
            sub_col13 = round((sub_col8 / sub_col12) * 100, 2)
            sub_col10  = round((sub_col10 / sub_col1), 2)

/* <AWD015> */
            if rpt_type$ <> "P" then goto skip_calc3
            sub_col6 = sub_tot_unt / sub_tot_hrs
REM         sub_col7 = sub_pln_unt / sub_tot_hrs
            sub_col7 = sum_col7 / cnt_col7
            sub_col7 = sub_col1 / sum_col7          /*<AWD017> */
/*<AWD017>  sub_col7 = sub_goal(s_goal%)              <AWD017> */
            s_goal% = s_goal% + 1%
***** SUBTOTAL GOAL CHANGES
REM         if dept$(x%) < "000" or dept$(x%) > "999" then skip_calc3
REM         gosub get_upmh_goal
            sum_col7 = 0.0
            cnt_col7 = 0.0
skip_calc3:
/* CR2692 */        
            sub_co114 = 0
            if sub_col1 > 0 then ~
                sub_col14 = round((sub_col4 * 60 ) / sub_col1, 2)
/* </AWD015> */
/* CR2692 */
            print using L03050 , dept_desc$(x%), sub_col1,    ~
                     sub_col2, sub_col3, sub_col4,            ~
                     sub_col5, sub_col14, sub_col6, sub_col7, ~
                     sub_col8, sub_col9, sub_col10,           ~
                     sub_col11, sub_col12
            cnt% = cnt% + 1%

        return

        convert_subtotal_scrap
            sub_col1, sub_col2, sub_col3, sub_col4, sub_col5,  ~
            sub_col6, sub_col7, sub_col8, sub_col9, sub_col10, ~
            sub_col11, sub_col12, sub_col13, sub_col2_dir,     ~
            sub_col8_dir, sub_col1_dir, sub_avg, sub_weight,   ~
            sub_col11_dir = 0.00

            scrap_dir_sub%, scrap_cnt_sub% = 0%
         for s% = beg% to (x% - 1%)
REM AWD016 if dept$(s%) = "032" then goto LS0030  /* DO NOT INCLUDE RGA IN PLANT */

            sub_col2  = sub_col2 + tot_col2(s%,n%)
            sub_col4  = sub_col4 + tot_col4(s%,n%)
            sub_col8  = sub_col8 + tot_col8(s%,n%)

            sub_col5  = sub_col5 + tot_col5(s%,n%)
            sub_col6  = sub_col6 + tot_col6(s%,n%)
            sub_col7  = sub_col7 + tot_col7(s%,n%)
            sub_col9  = sub_col9 + tot_col9(s%,n%)

                                  /*  (AWD006)  */
          if dept$(s%) = "037" and schema% = 1% then goto add_glass_sub
          if dept$(s%) = "074" and schema% = 2% then goto add_glass_sub
          if dept$(s%) = "075" and schema% = 2% then goto add_glass_sub
          for q% = 1% to 40%
            if dept$(s%) = ind_dept$(q%) then goto LS0040
          next q%

            sub_col2_dir = sub_col2_dir + tot_col2(s%,n%)
            sub_col8_dir = sub_col8_dir + tot_col8(s%,n%)
            sub_col1_dir = sub_col1_dir + tot_col1(s%,n%)
            scrap_dir_sub% = scrap_dir_sub% + 1%
            sub_avg  =  sub_avg + tot_avg(s%,n%)

            sub_weight = sub_weight + tot_weight(s%,n%)

add_glass_sub:                                          /* (AWD006) */
          if tot_col11(s%,n%) < .01 then goto LS0050
            scrap_cnt_sub% = scrap_cnt_sub% + 1%
            sub_col11_dir = sub_col11_dir + tot_col11(s%,n%)
            sub_col10 = sub_col10 + tot_col10(s%,n%)


LS0050:   
REM IF DEPT$(S%) = "009" OR DEPT$(S%) = "044" THEN GOTO LS0040
             if dept$(s%) = "009" then goto LS0040
/*(AWD009)*/
REM IF DEPT$(S%) = "046" OR DEPT$(S%) = "000" THEN GOTO LS0040
REM IF DEPT$(S%) = "000" THEN GOTO LS0040
/*(AWD009/)*/
REM IF DEPT$(S%) = "029" THEN GOTO LS0040

            sub_col1  = sub_col1 + tot_col1(s%,n%)

LS0040:
            sub_col11  = sub_col11 + tot_col11(s%,n%)
            sub_col12  = sub_col12 + tot_col12(s%,n%)
            sub_col13  = sub_col13 + tot_col13(s%,n%)
LS0030:  next s%

            sub_col3  = round((sub_col2_dir / sub_col1_dir), 2)
            sub_col11 = round(sub_col11_dir / scrap_cnt_sub%, 2)
            sub_avg   = round(sub_avg / scrap_dir_sub%, 2)
            sub_col4  = round((sub_col11 / 100) * sub_avg, 2)
            if sub_weight < .01 then goto LS0060
              sub_col9  = round((sub_col8_dir / sub_weight) * 100, 2)
                                                       /*  (AWD007)  - BEG  */
REM              sub_col10 = round((sub_col2_dir / sub_weight) * 100, 2)
              sub_col10 = round((sub_col13 / sub_col1), 2)
                                                       /* (AWD007)   - END  */
LS0060:       sub_col12 = round(sub_col11 - sub_col10, 2)

            print using L03100 , dept_desc$(x%), sub_col1,    ~
                     sub_col2, sub_col3, sub_col4,    ~
                     sub_col5, sub_col6, sub_col7,    ~
                     sub_col8, sub_col9, sub_col10,   ~
                     sub_col11, sub_col12, sub_col13
            cnt% = cnt% + 1%
            call "SHOSTAT" ( " Scrap Subtotal ")
        return

L01170:  FMT                       /* File = (EWDEFFCY)               */~
            CH(06),               /* Production Date                 */~
            CH(01),               /* Process Flag P=Prod S=Scrap     */~
            CH(04),               /* Production Year                 */~
            CH(02),               /* Production Week                 */~
            CH(01),               /* Production Day                  */~
            CH(03),               /* Production Department           */~
            CH(02),               /* Production Shift 00-Default     */~
            100*CH(3),            /* Production Models by Dept       */~
            100*BI(2),            /* Production Scanning Units       */~
            100*BI(2),            /* Production Sample/Displays      */~
            100*BI(2),            /* Production Sash Units           */~
            100*BI(2),            /* Production Part Units           */~
            100*BI(2),            /* Production Extra Units          */~
            100*BI(2),            /* Production Extra Units          */~
            100*BI(2),            /* Production Extra Units          */~
            PD(14,4),             /* Total Effective Unt or Scrap Lbs*/~
            PD(14,4),             /* Reg Hrs or Act. Scrap Lbs       */~
            PD(14,4),             /* OT Hrs or Scrap Lbs/Unit        */~
            PD(14,4),             /* Tot Hrs or Lbs/Unit GOAL        */~
            PD(14,4),             /* Act. Units/Hrs or Tot Mat. Used */~
            PD(14,4),             /* Plan Units or Mat Into Product  */~
            PD(14,4),             /* Var Unit/Hrs or In Process Scrap*/~
            PD(14,4),             /* Labor Dollars or Mistake Scrap  */~
            PD(14,4),             /* Lab Dol/Unit or Mstke Scrap %   */~
            PD(14,4),             /* Lab Dol Goal/Unit or Tot Scrap% */~
            PD(14,4),             /* Var Lab Dol or Tot Scrap% Goal  */~
            PD(14,4),             /* Prod Dol or Tot Scrap Var       */~
            PD(14,4),             /* Labor % of Prod $ or Tot Scrap$ */~
            PD(14,4),             /* Manager/TeamLeaders Hours       */~
            PD(14,4),             /* Manager/TeamLeaders Pay         */~
            6*BI(2),              /* Total Scanned For IG 'In House' */~
            PD(14,4),             /* Actual Scanned Units            */~
            PD(14,4),             /* Mstke + In Proc * Unts Weight   */~
            PD(14,4),             /* Eff. Earned Hours or UPMH Goal  */~
            PD(14,4),             /* Avg Weight for All mods in dept */~
            CH(141)               /* Filler                          */

        REM - Format Statements

L02750: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L02790: %|Production Week: ##                  P r o d u c t i o n   E f ~
        ~f i c i e n c y   R e p o r t                           Page: ###~
        ~  |

L02830: %|Production Day : ##                                      ######~
        ~########                                                   ######~
        ~##|

L02870: %|---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--|

L02880: %|                                                               ~
        ~                                                                 ~
        ~  |
/* CR2692 */                                                /* Header (1) */                                                         
L02910: %|           |          |          |          |          | Over|   ~
        ~     |      |       |        | Labor | Labor |Variance |         |
/* Old Layout */
LX2910: %|           |          |          |          |          | Over|   ~
        ~   |      |         | Labor | Labor |Variance |         | Labor  |
L03050X: %|###########|#######.#-|######.##-|######.##-|######.##-|##.#-|###~
        ~##-|###.##-|#######-|###.##-|###.##-|########-|########-|###.##- |
/* <AWD015> */                                            /* Header (2) */
L02960X: %|Department |Production| Regular  | Overtime |  Total   | Time|Pla~
        ~n  | Eff.  | Labor  | Dollar| $ Goal| Labor   |Productio| % of   |

                                                          /* Header (3) */
L03010X: %|           |  Units   |  Hours   |  Hours   |  Hours   |  %  |Uni~
        ~ts |   %   | Dollars| / Unit| / Unit| Dollar  | Dollars |Prod. $ |

/* CR2692 */
                                                          /* Header (2) */
L02960: %|Department |Production| Regular  | Overtime |  Total   | Time|   ~
        ~     | UPMH | UPMH  | Labor  | Dollar| $ Goal| Labor   |Productio|

                                                          /* Header (3) */
L03010: %|           |  Units   |  Hours   |  Hours   |  Hours   |  %  |   ~
        ~MPO  |Actual| Goal  | Dollars| / Unit| / Unit| Dollar  | Dollars |

/* Old Layout */       
                                                          /* Header (2) */
LX2960: %|Department |Production| Regular  | Overtime |  Total   | Time| UP~
        ~MH | UPMH  | Labor  | Dollar| $ Goal| Labor   |Productio| % of   |

                                                          /* Header (3) */
LX3010: %|           |  Units   |  Hours   |  Hours   |  Hours   |  %  |Act~
        ~ual| Goal  | Dollars| / Unit| / Unit| Dollar  | Dollars |Prod. $ |
/* </AWD015> */
/* CR2692 */
L03050: %|###########|#######.#-|######.##-|######.##-|######.##-|##.#-|##~
        ~##.##-|##.###|###.###|#######-|###.##-|###.##-|########-|########-|
/* Old layout */        
LX3050: %|###########|#######.#-|######.##-|######.##-|######.##-|##.#-|##.~
        ~###|###.###|#######-|###.##-|###.##-|########-|########-|###.##- |

REM - SCRAP FORMAT STATMENTS
L03060: %|Production Week: ##                          S c r a p     E f ~
        ~f i c i e n c y   R e p o r t                           Page: ###~
        ~  |


                                                          /* Header (1) */
L03070: %|           |          | Total  | Scrap | Lbs / |  Total   | Mater~
        ~ial  |   In    |        |       | Scrap | Total | Total | Total  |

                                                          /* Header (2) */
L03080: %|Department |Production| Scrap  | Lbs / | Unit  | Material |    In~
        ~to   | Process | Mistake|Mistake|Dollars| Scrap | Scrap | Scrap  |


                                                          /* Header (3) */
L03090: %|           |  Units   | Weight | Unit  | Goal  |   Used   |  Prod~
        ~uct  |   Scrap | Scrap  |Scrap %|PerUnit| $ Goal|Varianc| Dollar |

L03100: %|###########|#######.#-|#######-|###.##-|###.##-|#########-|#######~
        ~###-|########-|#######-|###.##-|###.##-|###.##-|###.##-|#######-|

        print_header
          pageno% = pageno% + 1%
          cnt%    = 0%
/* <AWD013> */
          cmd$ = hex(1b) & "E" & hex(1b) & "&l8D" & hex(1b) & "&k2S"
      print_pcl = 0
      if rpt_type$ = "S" and first_s = 1 then         ~
          print_pcl = 1
      if rpt_type$ = "P" and first_p = 1 then         ~
          print_pcl = 1
          if print_pcl = 1 then       ~
          print cmd$
/* </AWD013> */
          print page
          print using L02750
          if rpt_type$ = "S" then goto scrap_header
          first_p = 0    /* AWD013 */
          print using L02790 , sc_wk$, pageno%
          print using L02830 , sc_dy$, col1$(n%), pr_date$
          print using L02870
          print using L02910 , col2$(n%), col2$(n%), col2$(n%)
          print using L02960
          print using L03010
          print using L02870
          cnt% = cnt% + 7%
        return
        scrap_header
          first_s = 0 /* AWD013 */
          print using L03060 , sc_wk$, pageno%
          print using L02830 , sc_dy$, col1$(n%), pr_date$
          print using L02870
          print using L03070
          print using L03080
          print using L03090
          print using L02870
          cnt% = cnt% + 7%
        return

        print_detail
         call "SHOSTAT" ( " PRINT DETAIL ")
         for x% = 1% to (p_no% - 2%)    /*(AWD009) change from 1 to 2 */
            if cnt% > 50% or cnt% = 99% then gosub print_header
            if rpt_type$ = "S" then goto scrap_detail

            if dept$(x%) <> "LLL" then goto L03110
               print using L02870
               goto L03120
L03110:     if dept$(x%) <> "SSS" then goto L03130
               convert str(dept_desc$(x%),28%,3%) to beg%, data goto L03120

               gosub convert_subtotal
               goto L03120
L03130:
/* <AWD015> */
            tot_cl6 = tot_col6(x%,n%)
            tot_cl7 = tot_col7(x%,n%)
            if rpt_type$ = "S" then goto L03137
            tot_cl6 = tot_tot_unt(x%,n%) / tot_tot_hrs(x%,n%)
            tot_cl7 = tot_pln_unt(x%,n%) / tot_tot_hrs(x%,n%)
            if str(dept_desc$(x%),1,5) = "Total" then goto L03137
REM         if dept$(x%) <>"005" and dept$(x%) <> "007" and ~
REM         dept$(x%) <> "049" then L03137
REM         tot_cl7 = mrg_pln_unt(n%) / mrg_tot_hrs(n%)
            if dept$(x%) < "000" or dept$(x%) > "999" then L03137
            gosub get_upmh_goal
            gosub get_labor_goal
/*<AWD019> + */
/*SR72515  if tot_col10(x%,n%) <> 0% then goto skip_awd019_mod   */
/*SR72515  if tot_col1(x%,n%) = 0% then goto skip_awd019_mod     */
/*         if tot_cl10 = 0 then goto skip_awd019_mod             */
/*pwww      tot_col11(x%,n%) = round((tot_cl10 - tot_col9(x%,n%))~
                                        * tot_col1(x%,n%), 2)    */
/*SR72515   tot_col10(x%,n%) = tot_cl10                          */
/*      skip_awd019_mod                                          */
/*<AWD019> - */

L03137:
/* CR2692 */        
            tot_col14(x%,n%) = 0
            
            if tot_col1(x%,n%) > 0 then ~
           tot_col14(x%,n%) = round((tot_col4(x%,n%) * 60 )/ tot_col1(x%,n%), 2)
/* </AWD015> */
/* CR2692 */
            print using L03050 , dept_desc$(x%), tot_col1(x%,n%),    ~
                     tot_col2(x%,n%), tot_col3(x%,n%),     ~
                     tot_col4(x%,n%), tot_col5(x%,n%),     ~
                     tot_col14(x%,n%), tot_cl6, tot_cl7,   ~
                     tot_col8(x%,n%), tot_col9(x%,n%), tot_col10(x%,n%),   ~
                     tot_col11(x%,n%), tot_col12(x%,n%)
              /* tot_col13(x%,n%), */ 
            cnt% = cnt% + 1%

REM                  tot_col5(x%,n%), tot_col6(x%,n%), tot_col7(x%,n%),
L03120:  next x%

        return

        print_detail_plant
            if cnt% > 50% or cnt% = 99% then gosub print_header
            if rpt_type$ = "S" then goto scrap_detail_plant
            pnt_col10(n%) = (pnt_col10(n%) / pnt_col1(n%))
                                                        /*  (AWD007)  - BEG */
REM            pnt_col5(n%) = round((pnt_col3(n%) / pnt_col2(n%)) * 100,1)
            pnt_col5(n%) = round((pnt_col3(n%) / pnt_col4(n%)) * 100,1)
                                                        /*  (AWD007)  - END */
/* <AWD015> */
            if rpt_type$ = "S" then goto L03135
REM         pnt_col6(n%) = plt_tot_unt(n%) / plt_tot_hrs(n%)
            pnt_col6(n%) = pnt_col1(n%) / plt_tot_hrs(n%)
REM         pnt_col7(n%) = plt_pln_unt(n%) / plt_tot_hrs(n%)
            pnt_col7(n%) = tot_col7 / num_col7
            pnt_col7(n%) = pnt_col1(n%) / tot_col7            /*<AWD017> */
            tot_col7 = 0.0
            num_col7 = 0.0
            sum_col7 = 0.0                              /*<AWD017>*/
            cnt_col7 = 0.0                              /*<AWD017>*/
/*<AWD017>  pnt_col7(n%) = sub_goal(s_goal%)                <AWD017> */
            s_goal% = 1%
REM         if dept$(x%) < "000" or dept$(x%) > "999" then L03135
REM         gosub get_upmh_goal
L03135:
/* CR2692 */
            pnt_col14(n%) = 0
            if pnt_col1(n%) > 0 then pnt_col14(n%) = ~
                 round((pnt_col4(n%) * 60 ) / pnt_col1(n%), 2)
/* </AWD015> */
/* CR2692 */
            print using L03050 , dept_desc$, pnt_col1(n%), pnt_col2(n%),   ~
                  pnt_col3(n%), pnt_col4(n%), pnt_col5(n%), pnt_col14(n%), ~
                  pnt_col6(n%), pnt_col7(n%), pnt_col8(n%), pnt_col9(n%),  ~
                  pnt_col10(n%), pnt_col11(n%), pnt_col12(n%)
                 /* pnt_col13(n%), */
            cnt% = cnt% + 1%
        return
        print_rga
            x% = p_no% - 1%                 /* (AWD009 minor changes */
            if rga_sw = 0 then x% = x% + 1%  /* AWD016 */
            if rpt_type$ = "S" then goto print_rga_scrap

REM         for x% = x% to (x%+1%) AWD016
            if cnt% > 50% or cnt% = 99% then gosub print_header
REM            print using L02880                           /*  (EWD005)  */
/* <AWD015> */
            tot_cl6 = tot_col6(x%, n%)
            tot_cl7 = tot_col7(x%, n%)
            tot_cl6 = tot_tot_unt(x%, n%) / tot_tot_hrs(x%, n%)
            tot_cl7 = tot_pln_unt(x%, n%) / tot_tot_hrs(x%, n%)
            if str(dept_desc$(x%),1,5) = "Total" then goto L03136
            if schema% = 1% then skp_nc_dpt_ck3
            if dept$(x%) <>"005" and dept$(x%) <> "007" and ~
           dept$(x%) <> "049" then L03136
skp_nc_dpt_ck3:
            tot_cl7 = mrg_pln_unt(n%) / mrg_tot_hrs(n%)
            if dept$(x%) < "000" or dept$(x%) > "999" then L03136
            gosub get_upmh_goal
            gosub get_labor_goal
L03136:
/* CR2692 */        
            tot_col14(x%,n%) = 0
            if tot_col1(x%,n%) > 0 then tot_col14(x%,n%) = ~
                 round((tot_col4(x%,n%) * 60 ) / tot_col1(x%,n%), 2)
/* </AWD015> */
            print using L03050 , dept_desc$(x%), tot_col1(x%,n%),     ~
                     tot_col2(x%,n%), tot_col3(x%,n%),                ~
                     tot_col4(x%,n%), tot_col5(x%,n%),                ~
                     tot_col14(x%,n%), tot_cl6, tot_cl7,              ~
                     tot_col8(x%,n%), tot_col9(x%,n%), tot_cl10,      ~
                     tot_col11(x%,n%), tot_col12(x%,n%)
                     /*  tot_col13(x%,n%), */ ~

REM                  tot_col5(x%,n%), tot_col6(x%,n%), tot_col7(x%,n%)
REM         next x%
            if rga_sw = 0 then print using L02750
            cnt% = cnt% + 4%
        return

        print_rga_scrap
REM         for x% = x% to (x%+1%) AWD016
            if cnt% > 50% or cnt% = 99% then gosub print_header
REM            print using L02880                           /*  (EWD005)  */
            print using L03100 , dept_desc$(x%), tot_col1(x%,n%),    ~
                     tot_col2(x%,n%), tot_col3(x%,n%), tot_col4(x%,n%),    ~
                     tot_col5(x%,n%), tot_col6(x%,n%), tot_col7(x%,n%),    ~
                     tot_col8(x%,n%), tot_col9(x%,n%), tot_col10(x%,n%),   ~
                     tot_col11(x%,n%), tot_col12(x%,n%), tot_col13(x%,n%)
REM         next x%  AWD016

            if rga_sw = 0 then print using L02750
            cnt% = cnt% + 4%
        return

        scrap_detail_plant
         for x% = 1% to (p_no% - 2%) /*(AWD009) change from 1 to 2 */
            if cnt% > 50% or cnt% = 99% then gosub print_header
            print using L03100 , dept_desc$, pnt_col1(n%), pnt_col2(n%),   ~
                  pnt_col3(n%), pnt_col4(n%), pnt_col5(n%), pnt_col6(n%), pnt_col7(n%),~
                  pnt_col8(n%), pnt_col9(n%), pnt_col10(n%), pnt_col11(n%), pnt_col12(n%),~
                  pnt_col13(n%)
          plant_tot = pnt_col1(n%)
            cnt% = cnt% + 1%
        return

print_summary:
         print
         print using SUMHDR
         cnt% = cnt% + 7%
REM  for x% = 1% to 5%
     for x% = 1% to 1%
        if x% = 1% then sum_desc$ = "Vinyl"
        if x% = 2% then sum_desc$ = "Glass"
        if x% = 3% then sum_desc$ = "Aluminum"
        if x% = 4% then sum_desc$ = "Wood "
        if x% = 5% then sum_desc$ = "Total"
            print using SUMDTL, type_tot(x%), sum_desc$,                    ~
             round(type_cost(x%),0),     ~
             round(type_cost(x%) / plant_tot,2)
        type_tot(x%) = 0.00
        type_cost(x%) = 0.00
         next x%
        return
SUMHDR: %                                                Summary by Commodity
SUMDTL: %                                     ##########- ##################~
        ~ $###########- $######.##-

        scrap_detail
        call "SHOSTAT" (" SCRAP DETAIL " )
        for x% = 1% to (p_no% - 2%) /*(AWD009) change from 1 to 2 */
            if cnt% > 50% or cnt% = 99% then gosub print_header
            if dept$(x%) <> "LLL" then goto L03140
               print using L02870
               goto L03160
L03140:     if dept$(x%) <> "SSS" then goto L03150
               convert str(dept_desc$(x%),28%,3%) to beg%, data goto L03160

               gosub convert_subtotal_scrap
               goto L03160
L03150:     print using L03100 , dept_desc$(x%), tot_col1(x%,n%),    ~
                     tot_col2(x%,n%), tot_col3(x%,n%), tot_col4(x%,n%),    ~
                     tot_col5(x%,n%), tot_col6(x%,n%), tot_col7(x%,n%),    ~
                     tot_col8(x%,n%), tot_col9(x%,n%), tot_col10(x%,n%),   ~
                     tot_col11(x%,n%), tot_col12(x%,n%), tot_col13(x%,n%)
/* <AWD010>  collect type data */
            gosub compute_type_data
/* </AWD010> */
            cnt% = cnt% + 1%

L03160:  next x%
        return

/* <AWD010>  collect type data */
        compute_type_data
            srch_dept$ = dept$(x%)
        srch_type$ = " "
            for x1% = 1% to max_cnvt%
               if str(cnv_tbl$(x1%),1,3) <> dept$(x%) then goto skip_cnvt_rec
               srch_type$ = str(cnv_tbl$(x1%),5,1)
               x1% = max_cnvt% + 1%
skip_cnvt_rec:
        type_sub% = 0%
            next x1%
        type_sub% = 0%
        if srch_type$ = " " then return
        if srch_type$ = "V" then type_sub% = 1%
        if srch_type$ = "G" then type_sub% = 2%
        if srch_type$ = "A" then type_sub% = 3%
        if srch_type$ = "W" then type_sub% = 4%
            type_tot(type_sub%) = type_tot(type_sub%) + tot_col7(x%,n%)
            type_tot(type_sub%) = type_tot(type_sub%) + tot_col8(x%,n%)
            type_tot(5%) = type_tot(5%) + tot_col7(x%,n%)
            type_tot(5%) = type_tot(5%) + tot_col8(x%,n%)
        /* COST CALCULATION NEED CHANGING */
REM         rates from fall 2007
REM     tmp_rate = 0.34
REM         if srch_dept$ = "000" then tmp_rate = 1.99
REM         if srch_dept$ = "044" then tmp_rate = 1.19
REM         if srch_dept$ = "037" then tmp_rate = 0.52
REM         if srch_dept$ = "038" then tmp_rate = 0.223
REM         if srch_dept$ = "001" then tmp_rate = 1.56
/* <AWD011> */
REM         +-------------------------------------------+
REM         | default = vinyl                           |
REM         | dept 000 = Aluminum scrap                 |
REM         | dept 001 = Aluminum RB                    |
REM         | dept 044 = Wood                           |
REM         | dept 037 = Glass IG Lines (front)         |
REM         | dept 038 = Glass IG Cut                   |
REM         | dept 041 = Glass IG Back                  |
REM         +-------------------------------------------+
REM         rates from fall 2008
        tmp_rate = 1.14
            if srch_dept$ = "000" and schema% = 1% then tmp_rate = 1.30
            if srch_dept$ = "044" and schema% = 1% then tmp_rate = 0.80
            if srch_dept$ = "037" and schema% = 1% then tmp_rate = 0.39
            if srch_dept$ = "038" and schema% = 1% then tmp_rate = 0.229
            if srch_dept$ = "041" and schema% = 1% then tmp_rate = 0.39
            if srch_dept$ = "001" and schema% = 1% then tmp_rate = 0.78
/* </AWD011> */
        tmp_cost = round(tot_col7(x%,n%) * tmp_rate,2)
        tmp_cost = tmp_cost + round(tot_col8(x%,n%) * tmp_rate,2)
REM<AWD018> type_cost(type_sub%) = type_cost(type_sub%) + tmp_cost
REM<AWD018> type_cost(5%) = type_cost(5%) + tmp_cost

/*<AWD018>*/type_cost(type_sub%) = type_cost(type_sub%) + tot_col13(x%,n%)
/*<AWD018>*/type_cost(5%) = type_cost(5%) + tot_col13(x%,n%)
        return
/* </AWD010> */

        initialize_variables
          init(" ")readkey$, desc$,                     ~
            eff_wk_dte$, eff_proc$, eff_year$, eff_wk$, ~
            eff_day$, sc_dept$, eff_shift$, eff_model$()

            eff_col1, eff_col2, eff_col3, eff_col4,     ~
            eff_col5, eff_col6, eff_col7, eff_col8,     ~
            eff_col9, eff_col10, eff_col11, eff_col12,  ~
            eff_col13, eff_hrsm, eff_paym, eff_scan,    ~
            mistake_weight, eff_hrse, eff_avg = 0.0

            mat eff_glass = zer   :   mat eff_untpp = zer
            mat eff_unta = zer     :   mat eff_untb = zer
            mat eff_untc = zer     :   mat eff_unt = zer
            mat eff_unts = zer     :   mat eff_untss = zer
            mat tot_hrse = zer
            indirect% = 0%
        return

        initialize_plant

            mat pnt_col1 = zer      :   mat pnt_col7 = zer
            mat pnt_col2 = zer      :   mat pnt_col8 = zer
            mat pnt_col3 = zer      :   mat pnt_col9 = zer
            mat pnt_col4 = zer      :   mat pnt_col10 = zer
            mat pnt_col5 = zer      :   mat pnt_col11 = zer
            mat pnt_col6 = zer      :   mat pnt_col12 = zer
            mat pnt_col13 = zer     :   mat pnt_col14 = zer

            mat plt_tot_hrs   = zer
            mat plt_tot_unt   = zer
            mat plt_pln_unt   = zer

            mat pnt_avg = zer       :   mat tot_wgi = zer
            mat pnt_hrse = zer      :   mat tot_wgt = zer
            mat tot_hrsd = zer      :   mat pnt_weight = zer
            mat indir_wg = zer      :   mat pnt_col1_dir = zer
            mat pnt_col2_dir = zer  :   mat pnt_col11_dir = zer
            mat pnt_wg = zer        :   mat pnt_col8_dir = zer
            mat pnt_dir = zer       :   mat pnt_ind = zer
            mat tot_dir = zer       :   mat indir_per = zer
            mat indir_per = zer     :   mat indir_wg = zer
            mat tot_weight = zer    :   mat tot_avg = zer
            mat scrap_cnt% = zer    :   mat scrap_dir% = zer
       return

       initialize_totals
            mat tot_col1 = zer    :   mat tot_col2 = zer
            mat tot_col3 = zer    :   mat tot_col4 = zer
            mat tot_col5 = zer    :   mat tot_col6 = zer
            mat tot_col7 = zer    :   mat tot_col8 = zer
            mat tot_col9 = zer    :   mat tot_col10 = zer
            mat tot_col11 = zer   :   mat tot_col12 = zer
            mat tot_col13 = zer   :   mat tot_col14 = zer

            mat tot_tot_hrs   = zer
            mat tot_tot_unt   = zer
            mat tot_pln_unt   = zer

       return

       convert_prod  /* RECALC SOME COLUMNS FOR PRODUCTION RPT */

        call "SHOSTAT" ( " CONVERT PROD ")
        for n% = 1% to 4%
        for i% = 1% to p_no%
          indirect% = 0%
          for q% = 1% to 15%
              if dept$(i%) = auto_tab$(q%) then indirect% = 1%
          next q%
         if tot_col4(i%,n%) < .01 then goto L31010
         if dept$(i%) = "037" then                       ~
            tot_col1(i%,n%) = tot_col1(i%,n%) + tot_ex
REM      if dept$(i%) = "038" then                       ~
REM         tot_col1(i%,n%) = tot_col1(i%,n%) + tot_ex
REM  */  if dept$(i%) = "041" then                       ~
REM         tot_col1(i%,n%) = tot_col1(i%,n%) + tot_ex
                                                          /* (EWD001) */
                                                        /*  (AWD007)  - BEG */
REM            tot_col5(i%,n%) = round(tot_col3(i%,n%) / tot_col2(i%,n%) * 100, 2)
            tot_col5(i%,n%) = round(tot_col3(i%,n%) / tot_col4(i%,n%) * 100, 2)
                                                        /*  (AWD007)  - END */
         if dept$(i%) = "037"  and schema% = 1% then     ~
            tot_col1(i%,n%) = tot_col1(i%,n%) - tot_ex

         if dept$(i%) = "074"  and schema% = 2% then     ~
            tot_col1(i%,n%) = tot_col1(i%,n%) - tot_ex
         if dept$(i%) = "075"  and schema% = 2% then     ~
            tot_col1(i%,n%) = tot_col1(i%,n%) - tot_ex

         if indirect% = 1% then goto L31010
            tot_col7(i%,n%) = round((tot_hrse(i%,n%) / tot_col4(i%,n%)) * 100, 2)
L31010:  if tot_col1(i%,n%) < .01 then goto L31020
            tot_col9(i%,n%)  = round(tot_col8(i%,n%) / tot_col1(i%,n%), 2)
L31020:   /* <AWD014> */
/*SR72515 + */
            tot_col11(i%,n%) = round((tot_col10(i%,n%) - tot_col9(i%,n%))~
                                        * tot_col1(i%,n%), 2)
            gosub get_labor_goald
           if tot_col1(i%,n%) = 0% then goto skip_awd019_mod
           if tot_cl10 = 0 then goto skip_awd019_mod
            tot_col11(i%,n%) = round((tot_cl10 - tot_col9(i%,n%))~
                                        * tot_col1(i%,n%), 2)
            tot_col10(i%,n%) = tot_cl10
        skip_awd019_mod

/*SR72515 - */
          nz = tot_col1(i%,n%)
          des$ = dept_desc$(i%) /*temp*/
          des1$ = dept$(i%)
          des1 = tot_col10(i%,n%)
          des2 = tot_col1(i%,n%)
          des3 = tot_col8(i%,n%)
          des4 = tot_col9(i%,n%)
          des5 = tot_col11(i%,n%) /*temp*/
          if rpt_type$ = "S" then goto L03133
          tot_tot_unt(i%, n%) = tot_tot_unt(i%,n%) + tot_col1(i%, n%)
          tot_pln_unt(i%, n%) = tot_pln_unt(i%,n%) + tot_col6(i%, n%)
          tot_tot_hrs(i%, n%) = tot_tot_hrs(i%,n%) + tot_col4(i%, n%)
L03133:
      if nz = 0 then             ~
            tot_col11(i%,n%) = round(( - tot_col8(i%,n%)), 2)
           pnt_col11(n%) = pnt_col11(n%) + tot_col11(i%,n%)

 /* </AWD014> */
         if tot_col12(i%,n%) < .01 then goto L31030
            tot_col13(i%,n%) = round((tot_col8(i%,n%) / tot_col12(i%,n%))~
                                       * 100, 2)
L31030:  if schema% = 1% then goto skp_nc_dpt_ck4
         if dept$(i%) = "013" then indir_per(1%,n%) =  tot_col7(i%,n%)
         if dept$(i%) = "015" then indir_per(2%,n%) =  tot_col7(i%,n%)
         if dept$(i%) = "024" then indir_per(3%,n%) =  tot_col7(i%,n%)
         if dept$(i%) = "013" then indir_wg(n%) =  indir_wg(n%) + tot_col8(i%,n%)
         if dept$(i%) = "015" then indir_wg(n%) =  indir_wg(n%) + tot_col8(i%,n%)
         if dept$(i%) = "024" then indir_wg(n%) =  indir_wg(n%) + tot_col8(i%,n%)
         if dept$(i%) = "013" then indir_wg(1%,n%) =  tot_col4(i%,n%)
         if dept$(i%) = "015" then indir_wg(2%,n%) =  tot_col4(i%,n%)
         if dept$(i%) = "024" then indir_wg(3%,n%) =  tot_col4(i%,n%)
skp_nc_dpt_ck4:

        next i%
            call "SHOSTAT" (" CALC_INDIRECT ")
            gosub calc_indirect
            gosub print_detail
        rga_sw = 1
            gosub print_rga   /* AWD016 */
            print using L02870
            dept_desc$ = "Total Plant"
            gosub convert_prod_pnt
            gosub print_detail_plant
REM            print using L02870        /*  (EWD005)  */
            print using L02870
        rga_sw = 0
            gosub print_rga /* AWD016 */

            cnt% = 99%
            s_goal% = 1%
        next n%
            call "SETPRNT" ("APCC", " ", 0%, 1%)
            gosub initialize_totals
            gosub initialize_plant
       return

        convert_prod_pnt /* RECALC SOME COLUMNS FOR PRODUCTION PLANT RPT */

          if pnt_col2(n%) < .01 then goto L31110        /* (EWD001) */
                                                        /*  (AWD007)  - BEG */
REM             pnt_col5(n%)  = round(pnt_col3(n%) / pnt_col2(n%) * 100 , 2)
             pnt_col5(n%)  = round(pnt_col3(n%) / pnt_col4(n%) * 100 , 2)
                                                        /*  (AWD007)  - END */
L31110:   if pnt_col1(n%) < .01 then goto L31120
             pnt_col9(n%)  = round(pnt_col8(n%) / pnt_col1(n%), 2)
L31120:
          if pnt_col12(n%) < .01 then goto L31130
             pnt_col13(n%) = round((pnt_col8(n%) / pnt_col12(n%)) * 100, 2)
L31130:
          tot_dir(n%) = round((pnt_hrse(n%) / tot_hrsd(n%)) * 100, 2)
          for r% = 1% to 3%
              indir_per(n%) = round(indir_per(n%) + (indir_wg(r%,n%) / tot_wgi(n%)) ~
                           * indir_per(r%,n%), 2)
          next r%
          pnt_dir(n%) = round((tot_hrsd(n%) / pnt_col4(n%)) * tot_dir(n%), 2)
          pnt_ind(n%) = round((tot_wgt(n%) / pnt_col4(n%)) * indir_per(n%), 2)
          pnt_col7(n%) = pnt_dir(n%) + pnt_ind(n%)
REM L31140
        return

        convert_scrap        /* RECALC SOME COLUMNS FOR SCRAP RPT */
        for n% = 1% to 4%
        for i% = 1% to p_no%
           if dept$ = "037" and schema% = 1% then       ~
              tot_col1(i%,n%) = tot_col1(i%,n%) + tot_ex

           if dept$ = "074" and schema% = 2% then       ~
              tot_col1(i%,n%) = tot_col1(i%,n%) + tot_ex
           if dept$ = "075" and schema% = 1% then       ~
              tot_col1(i%,n%) = tot_col1(i%,n%) + tot_ex

           if tot_col1(i%,n%) < .01 then goto L31210
              tot_col3(i%,n%)  = round(tot_col2(i%,n%) / tot_col1(i%,n%), 2)
           if dept$ = "037" and schema% = 1% then      ~
              tot_col1(i%,n%) = tot_col1(i%,n%) - tot_ex

           if dept$ = "074" and schema% = 1% then      ~
              tot_col1(i%,n%) = tot_col1(i%,n%) - tot_ex
           if dept$ = "075" and schema% = 1% then      ~
              tot_col1(i%,n%) = tot_col1(i%,n%) - tot_ex

L31210:       tot_col4(i%,n%)  = round((tot_col11(i%,n%) / 100) * tot_avg(i%,n%), 2)
              tot_col8(i%,n%) = tot_col2(i%,n%) - tot_col7(i%,n%)
           if tot_weight(i%,n%) < .01 then goto L31220
              tot_col9(i%,n%)  = round((tot_col8(i%,n%) / tot_weight(i%,n%)) ~
                                 * 100, 2)
                                                           /*   (AWD007)  - BEG  */
REM              tot_col10(i%,n%) = round((tot_col2(i%,n%) / tot_weight(i%,n%)) ~
                                 * 100, 2)
              tot_col10(i%,n%) = round((tot_col13(i%,n%) / tot_col1(i%,n%)), 2)
                                                           /*   (AWD007)  - END  */
L31220:       tot_col12(i%,n%) = round(tot_col11(i%,n%) - tot_col10(i%,n%), 2)
        next i%
            gosub print_detail
            rga_sw = 1
            gosub print_rga /* AWD016 */
            print using L02870
            dept_desc$ = "Total Plant"
            gosub convert_scrap_pnt
            gosub print_detail_plant
/* <AWD010> get Total Plant Production Units and store in tot_plant */
            plant_tot = pnt_col1(n%)
/* </AWD010> */
REM            print using L02870            /*  (EWD005)  */
            print using L02870
        rga_sw = 0
            gosub print_rga /* AWD016 */
/* <AWD010> print summary by commodity */
            gosub print_summary
/* </AWD010> */
REM            p_no% = 0%
            cnt% = 99%
        next n%
            call "SETPRNT" ("APCC", " ", 0%, 1%)
            gosub initialize_totals
            gosub initialize_plant
        return

        convert_scrap_pnt    /* RECALC SOME COLUMNS FOR SCRAP PLANT RPT */
REM          for i% = 1 to 4%
           if scrap_cnt%(n%) = 0% then goto L31300
              pnt_col11(n%) = round(pnt_col11_dir(n%) / scrap_cnt%(n%), 2)
              pnt_avg(n%)   = round(pnt_avg(n%) / scrap_dir%(n%), 2)
              pnt_col4(n%)  = round((pnt_col11(n%) / 100) * pnt_avg(n%), 2)
              goto L31300
L31300:    if pnt_col1(n%) < .01 then goto L31310
              pnt_col3(n%)  = round(pnt_col2_dir(n%) / pnt_col1_dir(n%), 2)
              pnt_col8(n%) = pnt_col2(n%) - pnt_col7(n%)
L31310:    if pnt_weight(n%) < .01 then goto L31320
              pnt_col9(n%)  = round((pnt_col8_dir(n%) / pnt_weight(n%)) * 100, 2)
                                                     /*   (AWD007)   -   BEG  */
REM              pnt_col10(n%) = round((pnt_col2_dir(n%) / pnt_weight(n%)) * 100, 2)
              pnt_col10(n%) = round((pnt_col13(n%) / pnt_col1(n%)), 2)
                                                   /*   (AWD007)   -   END  */

L31320:       pnt_col12(n%) = round(pnt_col11(n%) - pnt_col10(n%), 2)

REM          next i%
        return

        calc_indirect    /* RECALC INDIRECT PERCENT (NOT DAILY RPT) */
            if rpt_type$ <> "P" then return
REM            lab_hrs = 0.0
REM            report% = 1%
REM            readkey1$ = "APC EFFDP"
REM            convert ind_col% to str(readkey1$,10%,3%), pic(000)
REM            gosub get_dept_next
REM         for x% = ind_col% to p_no%
         for x% = 1% to p_no%
            for q% = 1% to 15%
                if dept$(x%) = auto_tab$(q%) then goto L50780
            next q%
            goto L50760
L50780:     init(" ") ex_key$
            str(ex_key$,1%,4%) = ex_year$
            str(ex_key$,5%,3%) = dept$(x%)
            str(ex_sav$,1%,7%) = str(ex_key$,1%,7%)
            read #13, key > ex_key$, using L50770, ex_key$, ex_upmh(), ~
                                     eod goto L50760
L50770:      FMT CH(12), POS(109), 12*PD(14,4)
            if str(ex_key$,1%,7%) <> str(ex_sav$,1%,7%) then goto L50760

REM            for y% = 1% to 4%
              lab_hrs(n%)  = ((tot_col8(x%,n%) / pnt_wg(n%)) * 100)
              tot_col7(x%,n%) = ((ex_upmh(month%) / lab_hrs(n%)) * 100)
              if schema% = 1% then goto skp_nc_dpt_ck5
              if dept$(x%) = "013" then indir_per(1%,n%) =  tot_col7(x%,n%)
              if dept$(x%) = "015" then indir_per(2%,n%) =  tot_col7(x%,n%)
              if dept$(x%) = "024" then indir_per(3%,n%) =  tot_col7(x%,n%)
skp_nc_dpt_ck5:
REM            next y%
L50760:
         next x%

        return

        get_dept                /* GET DEPTS FOR RPT  */
           init(" ") readkey1$, sav_key1$, desc$, dept$()
           p_no% = 1%
           str(readkey1$,1%,9%) = "APC EFFDP"
           sav_key1$ = str(readkey1$,1%,9%)
        get_dept_next
           read #2, key > readkey1$, using L50000, readkey1$, desc$,      ~
                                                  eod goto get_dept_done
L50000:           FMT CH(24), CH(30)
           if str(readkey1$,1%,9%) <> sav_key1$ then goto get_dept_done

           if str(readkey1$,13%,3%) <> "DDD" then goto L50010
REM              if report% <> 0% then return
REM           if i% = 1% then goto process_done
REM              goto end_report
                 p_no% = p_no% - 1%
                 return
L50010:  REM if report% <> 0% then return
           dept$(p_no%) = str(readkey1$,13%,3%)
           dept_desc$(p_no%) = desc$
           p_no% = p_no% + 1%
           goto get_dept_next
        get_dept_done
        return

        get_upmh_goal            /* LOAD GOAL TABLE        */
           init(" ") readkey1$, sav_key1$, desc$, goal$()
           e_no% = 1%
           str(readkey1$,1%,9%) = "EFF GOAL "
           str(readkey1$,10%,3%) = dept$(x%)
           sav_key1$ = str(readkey1$,1%,9%)
        next_eff_goal
           read #2, key = readkey1$, using L50000, readkey1$, desc$,      ~
                                                  eod goto eff_goal_done
/*<AWD017>+ */
           convert str(desc$,1%,8%) to goal, data goto eff_goal_done
REM - Only Include Depts Up To Spec Shapes for Plant Col7
          goto skp_nc_dpt_ck6    /*<AWD017> skip unless needed later */
          for q% = 1% to 40%
            if dept$(x%) = ind_dept$(q%) then goto N20010
          next q%

N20030:   if schema% = 2% then goto skp_nc_dpt_ck6
          if dept$(x%) = "009" or dept$(x%) = "044" then goto N20010
          if dept$(x%) = "046" or dept$(x%) = "000" then goto N20010
          if dept$(x%) = "054" then goto N20010        /*  (EWD004) */
REM       if dept$(x%) = "041" then goto N20010        /*  (AWD011) */
          if dept$(x%) = "029" then goto N20010
/* skip these dept. in Total Plant */
          if dept$(x%) = "037" or dept$(x%) = "038" then goto N20010
/* <AWD013> */
          if dept$(x%) = "042" or dept$(x%) = "071" then goto N20010
/* </AWD013> */
          if dept$(x%) = "041" then goto N20010
          if dept$(x%) = "026" then goto N20010     /*(CR1253)*/
          if dept$(x%) = "082" then goto N20010     /*(CR1253)*/
skp_nc_dpt_ck6:
/*<AWD017> -*/
REM        sum_col7 = sum_col7 + goal
           if goal <> 0 then                                          ~
              sum_col7 = sum_col7 + (tot_col1(x%,n%)/goal)  /*<AWD017>*/
/*         tot_col8(x%,n%) = (tot_col1(x%,n%)/goal)  Temp  ***   <AWD017>*/
           cnt_col7 = cnt_col7 + 1.0
REM        tot_col7 = sum_col7 + goal
REM        tot_col7 = tot_col7 + goal     /*<AWD017> */
           if goal <> 0 then                                          ~
              tot_col7 = tot_col7 + (tot_col1(x%,n%)/goal)  /*<AWD017>*/
REM        num_col7 = cnt_col7 + 1.0
           num_col7 = num_col7 + 1.0      /*<AWD017> */
N20010:    tot_col7(x%,n%) = goal
           tot_cl7 = goal
        eff_goal_done
        return

        get_labor_goald           /* LOAD GOAL TABLE        */
           init(" ") readkey1$, sav_key1$, desc$, goal$()
           e_no% = 1%
           str(readkey1$,1%,9%) = "APC EFFMC         "
           str(readkey1$,10%,3%) = dept$(i%)
           sav_key1$ = str(readkey1$,1%,9%)
           tot_cl10 = 0.0
        next_eff_labord
           read #2, key = readkey1$, using L50000, readkey1$, desc$,      ~
                                                  eod goto eff_labor_doned

           des$ = str(desc$,8%,8%)
           convert str(desc$,8%,8%) to goal, data goto eff_labor_doned
           tot_cl10 = goal
        eff_labor_doned
        return

        get_labor_goal           /* LOAD GOAL TABLE        */
           init(" ") readkey1$, sav_key1$, desc$, goal$()
           e_no% = 1%
           str(readkey1$,1%,9%) = "APC EFFMC         "
           str(readkey1$,10%,3%) = dept$(x%)
           sav_key1$ = str(readkey1$,1%,9%)
           tot_cl10 = 0.0
        next_eff_labor
           read #2, key = readkey1$, using L50000, readkey1$, desc$,      ~
                                                  eod goto eff_labor_done

           des$ = str(desc$,8%,8%)
           convert str(desc$,8%,8%) to goal, data goto eff_goal_done
           tot_cl10 = goal
        eff_labor_done
        return

        get_upmhs_goal            /* LOAD GOAL TABLE        */
           init(" ") readkey1$, sav_key1$, desc$
           mat sub_goal = zer
           s_goal% = 1%
           str(readkey1$,1%,9%) = "EFF GOALS"
           sav_key1$ = str(readkey1$,1%,9%)
        next_effs_goal
           read #2, key > readkey1$, using L50000, readkey1$, desc$,      ~
                                                  eod goto eff_goals_done

           if str(readkey1$,1,9) <> str(sav_key1$,1,9) then goto eff_goals_done
           convert str(desc$,1%,8%) to sub_goal(s_goal%), data goto eff_goals_done

           s_goal% = s_goal% + 1%
           goto next_effs_goal
        eff_goals_done
        return

       get_glass              /* add remakes to glass dept */
            ex_col1 = 0.0     /* don't do on daily b/c already added */
            str(eff_key1$,1%,19%) = str(eff_key$,1%,19%) /* in EWDEFFTB */
            str(eff_key1$,7%,1%) = "P"
            str(eff_key1$,15%,3%) = "034"
            read #1, key 1% = eff_key1$, using L50840, ex_col1, eod goto L50850
L50840:     FMT POS(1720), PD(14,4)

            tot_ex = tot_ex + ex_col1
L50850: return

        month_error
         errormsg$ = "No First Week of Production Month in Table APC EFFMN"
         gosub error_prompt
         goto exit_sub

        week_error           /*  (EWD002)  */
         errormsg$ = "No First Week of Production Month in Table APC EFFWK"
         gosub error_prompt
         goto exit_sub

        set_initial        /* SET UP PRINTER */
           call "SETPRNT" ("APCC", " ", 0%, 0%)
           select printer (134)
           cnt%    = 99%
           pageno% = 0%
           gosub get_dept  /* GET FIRST DEPT - PRIMER */
REM           call "SHOSTAT" ("Creating "&col1$(i%)&"   Report")
           call "SHOSTAT" ("Creating Efficiency Reports")
        return

        error_prompt
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return


        exit_sub
        end

