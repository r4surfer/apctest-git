        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCCST5B                             *~
            *  Creation Date     - 10/28/93                             *~
            *  Last Modified Date- 01/22/98                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Description       - Subroutine to Calculate both the     *~
            *                      Direct and Indirect labor associated *~
            *                      with a Manufactured Product.         *~
            *  Special Comments  - Subroutine                           *~
            *                                                           *~
            *                                                           *~
            * ( Same as (APCCSTLB) with the Exception that AVG_PAY() and*~
            *           UPH() are returned.                             *~
            * APCCST5B - Subroutine to Calculate the The Direct Labor   *~
            *            and Indirect Labor associated with Product.    *~
            * Note - ( Only 'A' = Actual is used )                      *~
            *        ( AVG_PAY(2%) For the Screen Department (001) is)  *~
            *        (   pulled from Department (000) in APCCSTLR    )  *~
            *                                                           *~
            *                                                Tables     *~
            *        PART$     = MFG Part Number                        *~
            *        LB_TYP$   = 'A'=Actual,'S'=Standard   (COST_06ST)  *~
            *        MFG_LABOR = MFG Direct Labor          (APCPLNDP )  *~
            *                    (Screen )                 (APCPLNDP )  *~
            *                    (Wood Surround)           (APCPLNDP )  *~
            *        GLS_LABOR = Dir. Glass Labor Insulated(COST 01LB)  *~
            *                  = Dir. Glass Labor Cut      (COST-01LB)  *~
            *                  = Grid Labor for IG         (COST 06LB)  *~
            *        SCR_LABOR = LABOR(3%)                 (APCPLNDP )  *~
            *        MAT_LABOR = (Indirect Labor/ 3.0)  Old(COST 03LB)  *~
            *        STG_LABOR = (Indirect Labor/ 3.0)  Old(COST 04LB)  *~
            *        LOD_LABOR = (Indirect Labor/ 3.0)  Old(COST 05LB)  *~
            *                                                           *~
            *        IND_LABOR = MFG Labor * Percentage(8%)(COST 07ST)  *~
            *        SUB_LABOR = Total ( Direct and Ind )  ( CalC    )  *~
            *        OVR_LABOR = SUB_LABOR * Percentage    ( Calc    )  *~
            *        TOT_LABOR = Total Labor                            *~
            *        LAB_ERR%  = Error Code ( 0% = Ok, Non 0% = Error)  *~
            *                                                           *~
            * Note - The Product Department Code is Pulled from the     *~
            *    ( APCPLNDP  ) Product Department Associated with Prod. *~
            * Old( COST_06ST ) Average Hourly Rate Associated with Dept.*~
            *    ( COST 07ST ) (XXX) Standard Indirect Labor Percent    *~
            *    ( COST 07ST ) (XX1) Standard Overhead Labor Percent    *~
            *    ( COST 06LB ) (XXX) Additional Glass Labor for Grid    *~
            *                                                           *~
            *  - 'A' = Actual, When selected the Department Average     *~
            *    Hourly Rate will be obtained from the file (APCCSTLR). *~
            *    The value Calculated from the last Calculation Process *~
            *    will be used.                                          *~
            *                                                           *~
            *  - STD_LABOR() - 1% Standard Overhead Prcent       (Used) *~
            *    (Max = 10)  - 2% Top Sash Direct Labor Percent         *~
            *                - 3% Bot Sash Direct Labor Percent         *~
            *                - 4% Fixed Glass Direct Labor Percent      *~
            *                - 5% Cottage Direct Labor Percent          *~
            *                - 6% Oriel Direct Labor Percent            *~
            *                - 7% 1/3 1/3 1/3 Direct Labor Percent      *~
            *                - 8% Standard Indirect Percent      (Used) *~
            *                - 9%                                       *~
            *                                                           *~
            *  - Wood Surround Labor   DP$(12), AVG_PAY(12), UPH(12)    *~
            *                  Calc Using LOOKUP_WOOD_MULL. Direct      *~
            *                  Labor Stored in LABOR(12%)               *~
            *                                                           *~
            *----------!-----------------------------------------!------*~
            *   DATE   !                MODIFICATION             ! WHO  *~
            *----------!-----------------------------------------!------*~
            * 03/27/98 ! y2k compliant                           !  DJD *~
            *02/13/2019! CR-1894 Increase the EMP DEPT size to 3 ! DES  *~
            *----------!-----------------------------------------!------*~
            *************************************************************

        sub "APCCST5B" (part$,           /* MFG Part Number           */ ~
                        lb_typ$,         /* 'A'=Actual, 'S'=Standard  */ ~
                        mfg_labor,       /* Direct Labor Product Line */ ~
                        gls_labor,       /* Direct Glass Labor        */ ~
                        scr_labor,       /* Direct Screen Labor       */ ~
                        mat_labor,       /* Indirect Material Labor   */ ~
                        stg_labor,       /* Indirect Staging Labor    */ ~
                        lod_labor,       /* Indirect Loading Labor    */ ~
                        ind_labor,       /* Total Indirect            */ ~
                        sub_labor,       /* Total Direct and Indirect */ ~
                        ovr_labor,       /* Overhead Labor From Table */ ~
                        tot_labor,       /* Total Labor               */ ~
                        avg_pay(),       /* Average Pay Per Dept      */ ~
                        uph(),           /* Unit Per Manhour by Dept  */ ~
                               #1,       /* (APCEMPLY) Employee Mast  */ ~
                               #2,       /* (GENCODES) Master Tables  */ ~
                               #3,       /* (APCCSTLR) AHR Dept Rates */ ~
                               #4,       /* (APCPLNDP) Master Dept    */ ~
                        lab_err% )       /* Error Code 0% = Ok        */

        dim part$25,                     /* MFG Part Number           */ ~
            lb_typ$1,                    /* Labor Type Code 'A' or 'S'*/ ~
            avg_pay(15%),                /* Average Dept Pay Rates    */ ~
            dp$(15%)2,                   /* Product Departments       */ ~
            dp_tab$(15%)9,               /* Dept Cost Table           */ ~
            labor(15%), upmh$(3%)2,      /* Labor Dollars             */ ~
            uph(15%),                    /* Unit Per Manhour by Dept  */ ~
            std_labor(10%),              /* Labor Percent (COST 07ST) */ ~
            model$3,                     /* Model Code                */ ~
            gls$2,                       /* Glass Code                */ ~
            scr$1, lt$2,                 /* Screen Code               */ ~
            hg$2, date$8, month$2,       /* Hinge Code                */ ~
            wood$3, dp_key$15, sav_key$7,/* Save Wood Surround Code   */ ~
            readkey$24, descr$32,        /* Generic Key               */ ~
            gen_key$24, value$8          /* Code Table Key            */

            init(" ") dp$(), dp_tab$()
            mat avg_pay = zer         /* Average Pay - Standard/Actual */
            mat labor   = zer         /* Calculated Labor Dollars      */
            mat uph     = zer         /* Calculated Manhours Per Unit  */
            mat std_labor = zer       /* I% = 1%(Overhead),8%(Indirect)*/

            date$ = date
            call "DATFMTC" (date$)              /* Y2K */
            month$ = str(date$,1%,2%)

            model$ = str(part$,1%,3%)
            gls$   = str(part$,5%,2%)
            lt$    = str(part$,7%,2%)
            scr$   = str(part$,11%,1%)
            ins    = 2.0                    /* Ins Gls (2) Panels/Wind */
            sc% = pos("456" = scr$)
            if sc% <> 0% then ins = 1.0
            cut    = 4.0                    /* Cut Gls (4) Pieces/Wind */
            if sc% <> 0% then cut = 2.0
                                            /* Load Product Dept Code  */
            gosub load_dept
                                                     /* Direct Labor   */
                               dp_tab$( 1%) = " "    /* Window         */
                               dp_tab$( 2%) = " "    /* Screen         */
                               dp_tab$( 3%) = " "    /* Wood/Surr      */
                                                     /* Indirect Labor */
            dp$( 4%) = "37"  : dp_tab$( 4%) = "COST 01LB"  /* INSULATED*/
            dp$( 5%) = "38"  : dp_tab$( 5%) = "COST-01LB"  /* CUTTING  */

            ind_labor = 0.0 : sub_labor = 0.0 : ovr_labor = 0.0
            if gls$ <> "00" then goto L01370
               dp$(4%) = "XX" : dp$(5%) = "XX" /* No Glass for Product */

L01370:     std_max% = 5%
            if lb_typ$ = "S" then gosub load_standard                    ~
                             else gosub load_actual
             for i% = 1% to std_max%
               if dp$(i%) = "XX" then goto L01550
                  if i% < 4% then gosub load_upmh                        ~
                             else gosub load_upmh_table
                  if i% > 3% then goto L01470
                     labor(i%) = round(avg_pay(i%) * units_per_hour, 4)
                     goto L01540
L01470:           if i% = 4% then                        /* Insulated */ ~
                     units_per_hour = round(units_per_hour / ins, 4)
                  if i% = 5% then                        /* Cutting   */ ~
                          units_per_hour = round(units_per_hour / cut, 4)
                  if units_per_hour <> 0 then                            ~
                     labor(i%) = round( avg_pay(i%) / units_per_hour, 4)

L01540:           uph(i%)   = units_per_hour /* Dept Units per Manhour */
L01550:      next i%
                                             /* Direct Product Labor   */
                                             /* and Wood Surround      */
        mfg_labor = round( labor(1%) + labor(3%), 4)
                                             /* Direct Glass Labor     */
        gls_labor = round( labor(4%) + labor(5%), 4)
             gosub load_grid                 /* Adjust for Grid        */
        scr_labor = labor(2%)                /* Direct Screen          */

        REM
        REM   ( ALL  Special Labor Calc's )
        REM
        if hg$ <> "3" then goto L01720
           mfg_labor = round(mfg_labor * std_labor(7%), 4)
           gls_labor = round(gls_labor * std_labor(7%), 4)
           scr_labor = round(scr_labor * std_labor(7%), 4)

L01720: mat_labor = 0.0 : stg_labor = 0.0 : lod_labor = 0.0
        ind_labor =                          /* Calc for Standard Labor*/~
            round(( mfg_labor + gls_labor + scr_labor) * std_labor(8%),4)
                                             /* Calc for Actual Labor  */
           mat_labor, stg_labor, lod_labor = round( ind_labor/3.0 , 4)
        sub_labor =                          /* Total Direct + Indirect*/~
             round( mfg_labor + gls_labor + scr_labor + ind_labor, 4)
        ovr_labor = round( sub_labor * std_labor(1%),4)  /*Tot Overhead*/
        tot_labor = round( sub_labor + ovr_labor, 4) /* Total Labor    */

        goto exit_sub

        load_upmh
           pl_units = 1.0
           init(" ") dp_key$, gen_key$, descr$
           str(dp_key$,1%,3%)  = "0" & dp$(i%)      /* Department Code */
           str(dp_key$,4%,2%)  = "01"               /* Process Code    */
           str(dp_key$,6%,2%)  = "01"               /* Shift Code - Def*/
           str(dp_key$,8%,3%)  = model$             /* Model Code      */
           str(dp_key$,11%,2%) = upmh$(i%)          /* Plan Units Code */
           read #4,key = dp_key$, using L01930, pl_units, eod goto L01940
L01930:       FMT POS(23), PD(14,4)
L01940:    units_per_hour = round(1.0/pl_units, 4)

           wt = 100.0                            /* Assume 100 Percent */
           str(gen_key$,1%,9%)  = "PLAN UPMH"           /* Table Name  */
           str(gen_key$,10%,4%) = "0" & dp$(i%) & "-"   /* Department  */
           str(gen_key$,14%,3%) = month$ & "-"          /* Current Mon */
           str(gen_key$,17%,8%) = "01"                  /* Shift Code  */
           read #2,key = gen_key$, using L02020 , descr$, eod goto L02050
L02020:        FMT POS(25), CH(6)
            convert str(descr$,1%,6%) to wt, data goto L02040
L02040:
L02050:     wt = round(wt/100.0, 4)              /* Calc Weighted UPMH */
            wt1 = round( (1.0 + (1.0 - wt)) * units_per_hour, 4)
            units_per_hour = round(wt1, 4)
        return

        load_upmh_table
            init(" ") readkey$ : value$ = "0.0"
            str(readkey$,1%,9%)   = dp_tab$(i%)      /* Special Tables */
            str(readkey$,10%,15%) = model$
            read #2,key = readkey$, using L02150 , value$, eod goto L02160
L02150:        FMT POS(25), CH(8)
L02160:     convert value$ to units_per_hour, data goto L02170
L02170:
        return

        load_standard
           for k% = 1% to std_max%
              if dp$(k%) = "XX" then goto L02300
              init(" ")  readkey$
              str(readkey$,1%,9%)   = "COST_06ST"
              str(readkey$,10%,15%) = dp$(k%)
              read #2,key = readkey$, using L02270, descr$, eod goto L02300
L02270:          FMT POS(25), CH(9)
              convert str(descr$,1%,9%) to avg_pay(k%), data goto L02300

L02300:    next k%
        load_percent                               /* I% = 1% Overhead%*/
              init(" ") readkey$, descr$ : i% = 0% /* I% = 8% Indirect%*/
              str(readkey$,1%,9%) = "COST 07ST"
        load_percent_next
              read #2,key > readkey$, using L02370, readkey$, descr$,      ~
                                                            eod goto L02450
L02370:          FMT CH(24), CH(6)
              if str(readkey$,1%,9%) <> "COST 07ST" then return
                 i% = i% + 1%
                 convert str(descr$,1%,6%) to std_labor(i%),             ~
                                                        data goto L02420
L02420:
                 std_labor(i%) = round(std_labor(i%) / 100.0, 4)
                 goto load_percent_next
L02450: return

        load_actual                    /* For Applic Departments DP$() */
           for k% = 1% to std_max%
              if dp$(k%) = "XX" then goto L02560
                 init(" ") readkey$
                 str(readkey$,1%,3%) = "0" & dp$(k%)
                 if dp$(k%) = "01" then str(readkey$,1%,3%)="000"
                 read #3,key = readkey$, using L02550, avg_pay(k%),        ~
                                                            eod goto L02560
L02550:              FMT POS(72), PD(14,4)
L02560:    next k%
           gosub load_percent
        return

        load_grid                               /* Add Grid Avg Pay to */
              if lt$ = "00" then return         /* Insulated Glass     */
              init(" ") readkey$
              str(readkey$,1%,9%)   = "COST 06LB"
              str(readkey$,10%,15%) = "XXX"
              read #2,key = readkey$, using L02660, descr$, eod goto L02700
L02660:          FMT POS(25), CH(5)
              convert str(descr$,1%,5%) to x , data goto L02700

              gls_labor = round(gls_labor + x, 4)
L02700: return

        load_dept
                                                 /* DP$(1%) = "???"    */
            upmh$(1%) = "00" : dp$( 1%) = "XX"   /* Set To Window      */
            upmh$(2%) = "00" : dp$( 2%) = "XX"   /* Set Screen Value   */
            upmh$(3%) = "00" : dp$( 3%) = "XX"   /* Wood/Surround      */
            if len(part$) > 18 then goto L02800
               upmh$(1%) = "15"                  /* Set for a Part     */
               goto find_dept_prim
L02800:     if sc% = 1% then upmh$(1%) = "09"    /* Top Sash           */
            if sc% = 2% then upmh$(1%) = "11"    /* Bottom Sash        */
            if sc% = 3% then upmh$(1%) = "13"    /* Fixed Glass        */
            if upmh$(1%) <> "00" then goto find_dept_prim
               gosub lookup_co_or
               gosub lookup_wood
            if pos("2C" = scr$) <> 0 then upmh$(2%) = "17"/* Full Scr  */
            if pos("1B" = scr$) <> 0 then upmh$(2%) = "19"/* Half Scr  */
            if pos("9D" = scr$) <> 0 then upmh$(2%) = "21"/* Half Scr  */
            if upmh$(1%) = "00" then upmh$(1%) = "01"
            if upmh$(2%) <> "00" then dp$( 2%) = "01" /* Screen Dept   */
            if str(model$,1%,1%) = "0" then dp$(2%) = "XX"
                                                      /* Screen Only   */
        find_dept_prim
            init(" ") dp_key$, sav_key$
            str(dp_key$,1%,2%) = upmh$(1%)
            str(dp_key$,3%,3%) = model$
            str(dp_key$,6%,2%) = "01"                    /* Shift One  */
            sav_key$ = str(dp_key$,1%,7%)
        find_dept_next                                   /* (APCPLNDP) */
            read #4,key 2% > dp_key$, using L03020 , dp_key$,              ~
                                                        eod goto L03070
L03020:        FMT POS(4), CH(12)
            if sav_key$ <> str(dp_key$,1%,7%) then return
            gosub check_supp
            if supp% = 1% then goto find_dept_next
               dp$(1%) = str(dp_key$,9%,2%)      /* Primary Dept        */
L03070: return

        check_supp
            init(" ") gen_key$ : supp% = 0%
            str(gen_key$,1%,9%) = "PLAN SUPP"
            str(gen_key$,10%,15%) = str(dp_key$,8%,3%)
            read #2,key = gen_key$, eod goto L03150
               supp% = 1%
L03150: return

        lookup_co_or                             /* Cottage/Oriel      */
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = str(part$,9%,2%)
            read #2,key = readkey$,using L03230 ,descr$,                   ~
                                               eod goto lookup_co_or_done
L03230:        FMT POS(25), CH(3)
            if str(descr$,1%,2%) = "CO" or str(descr$,1%,2%) = "OR" then ~
                                 upmh$(1%) = "07"

            if str(descr$,1%,3%) = "1/3" then hg$ = "3" /* 1/3,1/3,1/3 */
        lookup_co_or_done
        return

        lookup_wood                              /* Flags 31 thru 98   */
            wood% = 0%                           /* Old Values. A00    */
            init(" ") readkey$                   /* thru Z00 New Values*/
            if str(part$,1%,1%) = "9" then return
            if len(part$) < 22 then return
            if len(part$) = 22 then wood$ = str(part$,20%,3%)            ~
                               else wood$ = str(part$,23%,3%)
            if wood$ = "000" then return
               str(readkey$,1%,9%) = "APC WOOD "
               str(readkey$,10%,3%) = wood$
               read #2,key = readkey$, eod goto L03480
                  convert wood$ to wood%, data goto L03460   /* Old Code */

                  convert (30% + wood%) to upmh$(3%), pic(00)
                  goto L03470
L03460:        upmh$(3%) = str(wood$,1%,1%) & "0"     /* New Codes A00 */
L03470: dp$( 3%) = "44"                               /*   thru Z00    */
L03480: return

        exit_sub
        end
