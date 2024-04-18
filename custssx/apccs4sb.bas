        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCCS4SB                             *~
            *  Creation Date     - 10/10/95                             *~
            *  Last Modified Date- 11/11/97                             *~
            *  Description       - Subroutine to Create Month End Gross *~
            *                      Margin Report by Shipto Account.     *~
            *                                                           *~
            *  Code Tables Used  - (SLS CODE2) - Code Table             *~
            *                                                           *~
            *  Special Comments  - CST(1%) = Product Material Cost      *~
            *                      CST(2%) = Product Labor Cost         *~
            *                      CST(3%) = Product Overhead Cost      *~
            *                      CST(4%) = Product Freight Cost       *~
            *                      CST(5%) = Product Vinyl Discount     *~
            *                      CST(6%) = Product Total MFG Cost     *~
            *                      CST(7%) = Product Total Trans Cost   *~
            *                      CST(8%) = Product Total Price W/Disc *~
            *                      CST(9%) = Product Total Quantity     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/12/98 ! y2k checked                              ! DJD *~
            *************************************************************

        sub "APCCS4SB" (#1,#4,b_mnth$,e_mnth$,b_year$,e_year$,b_mlyr$,   ~
                       e_mlyr$,b_lyr$,e_lyr$,per%,rpt_value$,rpt_sum$ )

        dim                                                              ~
            b_lyr$6,                     /* START DATE LAST YEAR       */~
            b_mlyr$6,                    /* START DATE THIS MNTH LST YR*/~
            b_mnth$6,                    /* START DATE THIS MONTH      */~
            b_year$6,                    /* START DATE THIS YEAR       */~
            rpt_value$9,                 /* SPECIFIED CUSTOMER         */~
            company$60,                  /* Company or Division Name   */~
            code$3,                      /* S.A. SORT CODE             */~
            date$8,                      /* Date for screen display    */~
            e_lyr$6,                     /* END DTE LAST YEAR          */~
            e_mlyr$6,                    /* END DTE THIS MNTH LAST YEAR*/~
            e_mnth$6,                    /* END DTE THIS MONTH         */~
            e_year$6,                    /* END DATE FOR THIS  YEAR    */~
            oldprodcde$3,                /* SAVE VALUE FOR PROD CODE   */~
            oldshipto$9,                 /* SAVE VALUE FOR SHIPTO CUST */~
            oldshiptoname$30,            /* SAVE VALUE FOR CUTOMER NAME*/~
            oldslsman$4,                 /* SAVE VALUE FO SALESMAN     */~
            postdate$6,                  /* INVOICE POSTING DATE       */~
            prodcode$(40%)3,             /* PRODUCT CODES ARRAY        */~
            prod_d$(40%)26, cc$24,       /* PROD CODE DESCRIPTIONS     */~
            prtcust$9,                   /* PRINT VARIABLE FOR SHIPTO  */~
            prtcustname$30,              /* PRINT VAR FOR SHIPTO NAME  */~
            readkey$27,                  /* READ KEY FOR APCSADTL      */~
            rpt_sum$1,                   /* (S)UMMARY OR (D)ETAIL      */~
            title$60,                    /* Report Title               */~
            saveshipto$9,                /* SAVE VALUE FOR SHIPTO      */~
            saveslsman$4,                /* SAVE VALUE FOR SALESMAN    */~
            shiptoname$30,               /* SHIPTO CUSTOMER NAME       */~
            time$8                       /* System Time                */

        dim f2%(5%)                      /* = 0 if the file is open    */

        dim                                                              ~
            cst(9%), cst_dd$40,          /* Costing Values By Line Item*/~
            tcst(6%), cst_d$(6%)10,      /* Line Item Cost Buckets     */~
            mtd(6%), lmtd(6%),           /* Current / Last Year MTD    */~
            ytd(6%), lytd(6%),           /* Current / Last Year YTD    */~
            lyear(6%),                   /* Total Last Year            */~
            slsmtd(6%), slslmtd(6%),     /* START DATE THIS YEAR       */~
            slsytd(6%), slslytd(6%),     /* SPECIFIED CUSTOMER         */~
            slslyear(6%),                /* Company or Division Name   */~
            cusmtd(6%), cuslmtd(6%),     /* START DATE LAST YEAR       */~
            cusytd(6%), cuslytd(6%),     /* START DATE THIS MNTH LST YR*/~
            cuslyear(6%),                /* START DATE THIS MONTH      */~
            summtd(40%,6%), sumlmtd(40%,6%), /*                        */~
            sumytd(40%,6%), sumlytd(40%,6%), /*                        */~
            sumlyear(40%,6%),            /* Company or Division Name   */~
            totmtd(6%), totlmtd(6%),     /* Current / Last Year MTD    */~
            totytd(6%), totlytd(6%),     /* Current / Last Year YTD    */~
            totlyear(6%)                 /* Total Last Year            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Month-End Gross Margin Report     "
            pname$ = "APCCS4SB - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCSLSDT ! APC Sales Analysis Detail File           *~
            * #4  ! GENCODES ! TABLE MASTER FILE                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
            company$ = "APC Building Products, Inc."
            call "FMTTITLE" (company$, " ", 12%)
            call "SHOSTAT" (" Printing Month End Gross Margin Report by "~
                              & "Shipto Customer")
            title$ =                                                     ~
                 "Month End Gross Margin Report - Detail by Customer"
            call "FMTTITLE" (title$, " ", 12%)

           oldprodcde$,oldshipto$,oldslsman$,oldshiptoname$ = all(hex(20))
            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            init (" ") prodcode$(),oldshipto$,oldslsman$,oldprodcde$,    ~
                       prod_d$(), cc$, code$
            cst_d$(1%) = "Sls Units "
            cst_d$(2%) = "MFG Sales "
            cst_d$(3%) = "MFG Cost  "
            cst_d$(4%) = "Gross Marg"
            cst_d$(5%) = "Pcnt Marg."
            cst_d$(6%) = "Trans Cost"

        REM - LOAD SORT ARRAY
            gosub load_sort
            last_cnt% = 1%
            select printer(134)
            date$ = " "  : date$ = date : call "DATEFMT" (date$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head
            readkey$ = all(hex(20))
            if str(rpt_value$,1%,3%) = "ALL" then goto L01470
               str(readkey$,1%,6%) = str(rpt_value$,1%,6%)
               call "SHOSTAT" ("Processing Customer (" &                 ~
                     str(readkey$,1%,6%) & ")" )
L01470:     read #1,key 2% > readkey$,using L01530 ,postdate$, shiptoname$,~
                             readkey$, cst(), eod goto end_report
            goto L01560
        read_loop
            read #1,using L01530 ,postdate$, shiptoname$, readkey$,        ~
                                cst(), eod goto end_report
L01530:        FMT CH(6), POS(101), CH(30), POS(340), CH(27), POS(402),  ~
                   9*PD(14,4)

L01560:     mat tcst = zer
            if str(rpt_value$,1%,3%) = "ALL" then goto L01610
               if str(readkey$,1%,6%) <> str(rpt_value$,1%,6%) then      ~
                                                          goto end_report

L01610:     tcst(1%)= round(cst(9%), 2)      /* Tot Quantity Shipped   */
            tcst(2%)= round(cst(8%), 2)      /* Tot Sale Price W/Disc  */
            tcst(3%)= round(cst(6%), 2)      /* Tot MFG Cost           */
            tcst(6%)= round(cst(7%), 2)      /* Tot Transportation Cost*/

            if str(readkey$,1%,9%) = oldshipto$ then goto L01700
               call "SHOSTAT" ("Processing Customer (" &                 ~
                     str(readkey$,1%,6%) & ")" )

L01700:     if str(readkey$,1%,9%) <> oldshipto$ then gosub shipto_brk
            if str(readkey$,10%,4%) <> oldslsman$ then gosub slsman_brk
            if str(readkey$,14%,3%) <> oldprodcde$ then gosub prod_brk
            if postdate$ < b_year$ then last_year
            if postdate$ > e_year$ then read_loop
            if postdate$ > e_mnth$ then read_loop
               for i% = 1% to 6%
                  ytd(i%) = round(ytd(i%) + tcst(i%), 2)
               next i%
               if postdate$ < b_mnth$ then read_loop
                  for i% = 1% to 6%
                     mtd(i%) = round(mtd(i%) + tcst(i%), 2)
                  next i%
                  goto read_loop
        last_year
            if postdate$ < b_lyr$ then read_loop
               for i% = 1% to 6%
                  lyear(i%) = round(lyear(i%) + tcst(i%), 2)
               next i%
               if postdate$ > e_mlyr$ then read_loop
                  for i% = 1% to 6%
                     lytd(i%) = round(lytd(i%) + tcst(i%), 2)
                  next i%
                  if postdate$ < b_mlyr$ then read_loop
                     for i% = 1% to 6%
                        lmtd(i%) = round(lmtd(i%) + tcst(i%), 2)
                     next i%
        goto read_loop
        prod_brk
           if oldprodcde$ <> " " then L02010
              goto L02310
L02010:    init(" ") prtcust$,prtcustname$,prtsls$
           if oldslsman$ = saveslsman$ then L02050
              prtsls$ = oldslsman$
              saveslsman$ = oldslsman$
L02050:    if oldshipto$ = saveshipto$ then L02170
              prtcust$ = oldshipto$
              prtcustname$ = oldshiptoname$
              saveshipto$ = oldshipto$
              for i% = 1% to 6%
                  mtd(i%)   = round(mtd(i%),2)
                  lmtd(i%)  = round(lmtd(i%),2)
                  ytd(i%)   = round(ytd(i%),2)
                  lytd(i%)  = round(lytd(i%),2)
                  lyear(i%) = round(lyear(i%),2)
              next i%
                                         /* Remove Product Detail      */
L02170:      if rpt_sum$ = "S" then goto L02300  /* Go to BUILD_SUMREC   */
                gosub calc_product
                for i% = 1% to 6%
                    if lcntr% > 55% then gosub page_head
                    print using L03900, str(prtcust$,1%,6%),               ~
                                      str(prtcustname$,1%,24%),          ~
                                      prtsls$, oldprodcde$, cst_d$(i%),  ~
                                      mtd(i%), lmtd(i%), ytd(i%),        ~
                                      lytd(i%), lyear(i%)
                    lcntr% = lcntr% + 1%
                    init(" ") prtcust$,prtcustname$,prtsls$
                next i%

L02300:    gosub build_sumrec
L02310:    oldprodcde$ = str(readkey$,14%,3%)
           for i% = 1% to 6%
               if i% = 4% or i% = 5% then goto L02390
                  slslyear(i%) = round(slslyear(i%) + lyear(i%), 2)
                  slslytd(i%)  = round(slslytd(i%)  + lytd(i%), 2)
                  slslmtd(i%)  = round(slslmtd(i%)  + lmtd(i%), 2)
                  slsmtd(i%)   = round(slsmtd(i%)   + mtd(i%), 2)
                  slsytd(i%)   = round(slsytd(i%)   + ytd(i%), 2)
L02390:    next i%
           mat lyear = zer : mat lytd = zer : mat lmtd = zer
           mat mtd   = zer : mat ytd  = zer

        return
        slsman_brk
           if oldslsman$ <> " " then  L02470
              goto L02480
L02470:    gosub prod_brk
L02480:    oldslsman$ = str(readkey$,10%,4%)
           for i% = 1% to 6%
              if i% = 4% or i% = 5% then goto L02560
                 cuslyear(i%) = round(cuslyear(i%) + slslyear(i%), 2)
                 cuslytd(i%)  = round(cuslytd(i%)  + slslytd(i%), 2)
                 cuslmtd(i%)  = round(cuslmtd(i%)  + slslmtd(i%), 2)
                 cusmtd(i%)   = round(cusmtd(i%)   + slsmtd(i%), 2)
                 cusytd(i%)   = round(cusytd(i%)   + slsytd(i%), 2)
L02560:    next i%
           mat slslyear = zer : mat slslytd = zer : mat slslmtd = zer
           mat slsmtd   = zer : mat slsytd  = zer

        return
        shipto_brk
           if oldshipto$ <> " " then  L02640
              goto L02850
L02640:    gosub slsman_brk
           if lcntr% > 55% then gosub page_head
           if rpt_sum$ = "S" then goto L02850
                                        /* Calculate Margin and Margin */
                                        /* and Margin Percent          */
           gosub calc_customer
           cst_dd$ = " **** Total for Customer ######### **** "
           str(cst_dd$,26%,9%) = oldshipto$
           print using L03940
           lcntr% = lcntr% + 1%
           for i% = 1% to 6%
               if lcntr% > 55% then gosub page_head
               print using L03960, cst_dd$, cst_d$(i%), cusmtd(i%),        ~
                                 cuslmtd(i%), cusytd(i%), cuslytd(i%),   ~
                                 cuslyear(i%)
                lcntr% = lcntr% + 1%
                init(" ") cst_dd$
           next i%
           print
           lcntr% = lcntr% + 1%

L02850:    oldshipto$ = str(readkey$,1%,9%)
           oldshiptoname$ = shiptoname$
           mat cuslyear = zer : mat cuslytd = zer : mat cuslmtd = zer
           mat cusmtd   = zer : mat cusytd  = zer
        return

        build_sumrec
          if prodcode$(last_cnt%) <> oldprodcde$ then last_cnt% = 1%
          for cnt% = last_cnt% to 40%
            if cnt% > sort_cnt% then gosub load_descript
            if prodcode$(cnt%) <> oldprodcde$ then L03070
              for i% = 1% to 6%
                if i% = 4% or i% = 5% then goto L03030
              sumlyear(cnt%,i%) = round(sumlyear(cnt%,i%) + lyear(i%), 2)
              sumlytd(cnt%,i%)  = round(sumlytd(cnt%,i%)  + lytd(i%), 2)
              sumlmtd(cnt%,i%)  = round(sumlmtd(cnt%,i%)  + lmtd(i%), 2)
              summtd(cnt%,i%)   = round(summtd(cnt%,i%)   + mtd(i%), 2)
              sumytd(cnt%,i%)   = round(sumytd(cnt%,i%)   + ytd(i%), 2)
L03030:       next i%

             last_cnt% = cnt%
             cnt% = 40%
L03070:   next cnt%
        return
        print_summary
            mat totmtd = zer : mat totlmtd = zer : mat totytd = zer
            mat totlytd = zer : mat totlyear = zer
            if lcntr% > 55% then gosub page_head
               print skip (2)
               print using L03990
               lcntr% = lcntr% + 3%
               for xcnt% = 1% to 40%
                   if prodcode$(xcnt%) <> " " then goto L03210
                      xcnt% = 40%
                      goto L03360

L03210:          gosub calc_sumrec
                 for i% = 1% to 6%
                   if lcntr% > 55% then gosub page_head
                   print using L04010 ,prod_d$(xcnt%), cst_d$(i%),         ~
                                     summtd(xcnt%,i%), sumlmtd(xcnt%,i%),~
                                     sumytd(xcnt%,i%), sumlytd(xcnt%,i%),~
                                     sumlyear(xcnt%,i%)
               totmtd(i%)   = round(totmtd(i%) + summtd(xcnt%,i%), 2)
               totlmtd(i%)  = round(totlmtd(i%) + sumlmtd(xcnt%,i%), 2)
               totytd(i%)   = round(totytd(i%) + sumytd(xcnt%,i%), 2)
               totlytd(i%)  = round(totlytd(i%) + sumlytd(xcnt%,i%), 2)
               totlyear(i%) = round(totlyear(i%) + sumlyear(xcnt%,i%), 2)
                   lcntr% = lcntr% + 1%
                   prod_d$(xcnt%) = " "
                 next i%
L03360:        next xcnt%
               gosub calc_totals
               for i% = 1% to 6%
                 if lcntr% > 55 then gosub page_head
                 print using L03940
                 print using L04010 , " ", cst_d$(i%), totmtd(i%),         ~
                                    totlmtd(i%), totytd(i%), totlytd(i%),~
                                    totlyear(i%)
               next i%

            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            mat totmtd = zer : mat totlmtd = zer : mat totytd = zer
            mat totlytd = zer : mat totlyear = zer
        return
        end_report                /* Report Ending Routine */
            gosub shipto_brk
            gosub print_summary
            print skip(2)
            print using L04060      /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto exit_sub

        page_head              /* Page Heading Print Routine */
            time$ = " " : call "TIME" (time$)
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L03790 , date$, time$, company$, "APCCS4SB"
            print using L03830 ,per%,title$, pcntr%
            print
            print using L03850
            print using L03870
            lcntr% = 5%
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L03790: %Run ######## @ ########              ###########################~
        ~#################################                ########:APCSLS3

*       * Header Line 2
L03830: %Period: ##                           ###########################~
        ~#################################                     Page: ####
L03850: %Customer Customer Name            SLSM PRD Description   $ MTD  ~
        ~       $ Last YR MTD     $ YTD      $ Last YTD     Tot $ LST YR
L03870: %-------- ------------------------ ---- --- ----------- ---------~
        ~----- -------------- -------------- -------------- --------------

L03900: % ######  ######################## #### ###  ##########  #,###,##~
        ~#.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-
        %  **** Total for Salesman #### ****                    ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
L03940: %                                            ---------- ---------~
        ~----- -------------- -------------- -------------- --------------
L03960: % ########################################   ########## ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-

L03990: %                                     ***** Month End Gross Margi~
        ~n Summary - by Product for (All) Customers
L04010: %               ##########################   ########## ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
        %** Report Title for page 0
        %############################################################

L04060: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        load_sort                        /* MFG Products into Array in */
            sort_cnt%, cnt% = 0%         /*  Sorted order from GENCODES*/
            init(" ") cc$                /*  Table (SLS CODE2)         */
            str(cc$,1%,9%)   = "SLS CODE2"
        load_nxt_sort
            read #4,key > cc$,using L04150,cc$ ,eod goto L04250
L04150:         FMT CH(24)
            if str(cc$,1%,9%) <> "SLS CODE2" then goto L04250
               code$ = str(cc$,10%,3%)
               if code$ = "XXX" then goto L04250
               cnt% = cnt% + 1%
               get #4,using L04210, prod_d$(cnt%)
L04210:           FMT POS(25), CH(20)
               str(prod_d$(cnt%),21%,6%) = " (" & code$ & ")"
               prodcode$(cnt%) = code$
               goto load_nxt_sort
L04250: sort_cnt% = cnt%                 /* Total Number of Products   */
        return

        load_descript
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
            str(cc$,10%,15%) = oldprodcde$
            read #4,key = cc$,using L04330,prod_d$(cnt%) ,eod goto L04370
L04330:         FMT POS(25), CH(20)
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
            prodcode$(cnt%) = str(cc$,10%,3%)
        return
L04370:     str(prod_d$(cnt%),1%,20%) = "Code Not Defined ???"
            prodcode$(cnt%) = oldprodcde$
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
        return

        calc_product
                                                             /* MTD()  */
              mtd(4%) = round(mtd(2%) - mtd(3%), 2)
              if mtd(2%) = 0 then goto L04480
                mtd(5%) = round((mtd(4%)/mtd(2%))*100, 2)
                                                             /* LMTD() */
L04480:       lmtd(4%) = round(lmtd(2%) - lmtd(3%), 2)
              if lmtd(2%) = 0 then goto L04520
                lmtd(5%) = round((lmtd(4%)/lmtd(2%))*100, 2)
                                                             /* YTD()  */
L04520:       ytd(4%) = round(ytd(2%) - ytd(3%), 2)
              if ytd(2%) = 0 then goto L04560
                ytd(5%) = round((ytd(4%)/ytd(2%))*100, 2)
                                                             /* LYTD() */
L04560:       lytd(4%) = round(lytd(2%) - lytd(3%), 2)
              if lytd(2%) = 0 then goto L04600
                lytd(5%) = round((lytd(4%)/lytd(2%))*100, 2)
                                                     /* XX5() = LYEAR()*/
L04600:       lyear(4%) = round(lyear(2%) - lyear(3%), 2)
              if lyear(2%) = 0 then goto L04630
                lyear(5%) = round((lyear(4%)/lyear(2%))*100,2)
L04630: return

        calc_customer
                                                          /* CUSMTD()  */
              cusmtd(4%) = round(cusmtd(2%) - cusmtd(3%), 2)
              if cusmtd(2%) = 0 then goto L04710
                cusmtd(5%) = round((cusmtd(4%)/cusmtd(2%))*100, 2)
                                                          /* CUSLMTD() */
L04710:       cuslmtd(4%) = round(cuslmtd(2%) - cuslmtd(3%), 2)
              if cuslmtd(2%) = 0 then goto L04750
                cuslmtd(5%) = round((cuslmtd(4%)/cuslmtd(2%))*100, 2)
                                                          /* CUSYTD()  */
L04750:       cusytd(4%) = round(cusytd(2%) - cusytd(3%), 2)
              if cusytd(2%) = 0 then goto L04790
                cusytd(5%) = round((cusytd(4%)/cusytd(2%))*100, 2)
                                                          /* CUSLYTD() */
L04790:       cuslytd(4%) = round(cuslytd(2%) - cuslytd(3%), 2)
              if cuslytd(2%) = 0 then goto L04830
                cuslytd(5%) = round((cuslytd(4%)/cuslytd(2%))*100, 2)
                                                          /* CUSLYEAR()*/
L04830:       cuslyear(4%) = round(cuslyear(2%) - cuslyear(3%), 2)
              if cuslyear(2%) = 0 then goto L04860
                cuslyear(5%) = round((cuslyear(4%)/cuslyear(2%))*100,2)
L04860: return

        calc_totals
                                                          /* TOTMTD()  */
              totmtd(4%) = round(totmtd(2%) - totmtd(3%), 2)
              if totmtd(2%) = 0 then goto L04940
                totmtd(5%) = round((totmtd(4%)/totmtd(2%))*100, 2)
                                                          /* TOTLMTD() */
L04940:       totlmtd(4%) = round(totlmtd(2%) - totlmtd(3%), 2)
              if totlmtd(2%) = 0 then goto L04980
                totlmtd(5%) = round((totlmtd(4%)/totlmtd(2%))*100, 2)
                                                          /* TOTYTD()  */
L04980:       totytd(4%) = round(totytd(2%) - totytd(3%), 2)
              if totytd(2%) = 0 then goto L05020
                totytd(5%) = round((totytd(4%)/totytd(2%))*100, 2)
                                                          /* TOTLYTD() */
L05020:       totlytd(4%) = round(totlytd(2%) - totlytd(3%), 2)
              if totlytd(2%) = 0 then goto L05060
                totlytd(5%) = round((totlytd(4%)/totlytd(2%))*100, 2)
                                                          /* TOTLYEAR()*/
L05060:       totlyear(4%) = round(totlyear(2%) - totlyear(3%), 2)
              if totlyear(2%) = 0 then goto L05090
                totlyear(5%) = round((totlyear(4%)/totlyear(2%))*100,2)
L05090: return

        calc_sumrec
                                                  /* SUMMTD(XCNT%, ?)   */
          summtd(xcnt%,4%) =                                             ~
                     round(summtd(xcnt%,2%) - summtd(xcnt%,3%), 2)
          if summtd(xcnt%,2%) = 0 then goto L05190
          summtd(xcnt%,5%) =                                             ~
                     round((summtd(xcnt%,4%)/summtd(xcnt%,2%))*100, 2)
                                                 /* SUMLMTD(XCNT%, ?)   */
L05190:   sumlmtd(xcnt%,4%) =                                            ~
                     round(sumlmtd(xcnt%,2%) - sumlmtd(xcnt%,3%), 2)
          if sumlmtd(xcnt%,2%) = 0 then goto L05250
          sumlmtd(xcnt%,5%) =                                            ~
                     round((sumlmtd(xcnt%,4%)/sumlmtd(xcnt%,2%))*100, 2)
                                                 /* SUMYTD(XCNT%, ?)   */
L05250:   sumytd(xcnt%,4%) =                                             ~
                     round(sumytd(xcnt%,2%) - sumytd(xcnt%,3%), 2)
          if sumytd(xcnt%,2%) = 0 then goto L05310
          sumytd(xcnt%,5%) =                                             ~
                     round((sumytd(xcnt%,4%)/sumytd(xcnt%,2%))*100, 2)
                                                 /* SUMLYTD(XCNT%, ?)  */
L05310:   sumlytd(xcnt%,4%) =                                            ~
                     round(sumlytd(xcnt%,2%) - sumlytd(xcnt%,3%), 2)
          if sumlytd(xcnt%,2%) = 0 then goto L05370
          sumlytd(xcnt%,5%) =                                            ~
                     round((sumlytd(xcnt%,4%)/sumlytd(xcnt%,2%))*100, 2)
                                                 /* SUMLYEAR(XCNT%, ?) */
L05370:   sumlyear(xcnt%,4%) =                                           ~
                     round(sumlyear(xcnt%,2%) - sumlyear(xcnt%,3%), 2)
          if sumlyear(xcnt%,2%) = 0 then goto L05420
          sumlyear(xcnt%,5%) =                                           ~
                    round((sumlyear(xcnt%,4%)/sumlyear(xcnt%,2%))*100, 2)
L05420: return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_sub

            end
