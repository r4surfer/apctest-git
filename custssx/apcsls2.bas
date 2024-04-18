        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCSLS2                              *~
            *  Creation Date     - 02/10/95                             *~
            *  Last Modified Date- 08/08/00                             *~
            *  Description       - Subroutine to Create Sales Analysis  *~
            *                      Report By Bill To Account            *~
            *                                                           *~
            *  Code Tables Used  - (SLS CODE2) - Table with Sales       *~
            *                          Analysis Sort Code Descriptions. *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 02/24/95 ! Print S.A. Code Descriptions             !     *~
            * 03/17/95 ! Mod to Sort Summary - ( LOAD_SORT )      !     *~
            * 11/11/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 06/05/00 ! Mod to use linecom for amount instead of ! CMG *~
            *          !     of calculating. (EWD001)             !     *~
            * 07/05/00 ! Mod to use EWDSLSDT instead of APCSLSDT  ! CMG *~
            *          !     (EWD002)                             !     *~
            * 08/08/00 ! Mod to take out error code 8 (EWD003)    ! CMG *~     
            * 12/20/18 ! CR1828 EWDSLSDT file conversion, Dallas  ! DES *~     
            *************************************************************

        sub "APCSLS2" (#1,#4,#5,b_mnth$,e_mnth$,b_year$,e_year$,b_mlyr$, ~
                       e_mlyr$,b_lyr$,e_lyr$,per%,rpt_value$,rpt_sum$)
                                         /* (EWD00) - ADD CUSTOMER FILE */
        dim                                                              ~
            b_lyr$6,                     /* START DATE LAST YEAR       */~
            b_mlyr$6,                    /* START DATE THIS MNTH LST YR*/~
            b_mnth$6,                    /* START DATE THIS MONTH      */~
            b_year$6,                    /* START DATE THIS YEAR       */~
            billto$9,                    /* BILLTO CUSTOMER            */~
            billtoname$30,               /* BILLTO CUSTOMER NAME       */~
            code$3,                      /* S.A. SORT CODE             */~
            savename$30,                 /* SAVE BILLTO NAME           */~
            savebillto$9,                /* SAVE BILLTO CUSTOMER       */~
            rpt_value$9,                 /* SPECIFIED ACCOUNT OR ALL   */~
            company$60,                  /* Company or Division Name   */~
            date$8,                      /* Date for screen display    */~
            e_lyr$6,                     /* END DTE LAST YEAR          */~
            e_mlyr$6,                    /* END DTE THIS MNTH LAST YEAR*/~
            e_mnth$6,                    /* END DTE THIS MONTH         */~
            e_year$6,                    /* END DATE FOR THIS  YEAR    */~
            summtd(99%),sumlmtd(99%),    /* SUMMARY BUCKETS FOR TOTALS */~
            sumytd(99%),sumlytd(99%),    /* SUMMARY BUCKETS FOR TOTALS */~
            sumlyear(99%),               /* SUMMARY BUCKET FOR TOTALS  */~
            oldbillto$9,                 /* SAVE VALUE FOR BILLTO CUST */~
            oldprodcde$3,                /* SAVE VALUE FOR PROD CODE   */~
            shipto$9,                    /* SHIPTO CUST                */~            
            oldshipto$9,                 /* SAVE VALUE FOR SHIPTO CUST */~
            oldshiptoname$30,            /* SAVE VALUE FOR CUTOMER NAME*/~
            oldslsman$4,                 /* SAVE VALUE FO SALESMAN     */~
            postdate$6,                  /* INVOICE POSTING DATE       */~
            prodcode$(99%)3,             /* PRODUCT CODES ARRAY        */~
            prod_d$(99%)26, cc$24,       /* SALES ANALYSIS CODE DESC   */~
            prtcust$9,                   /* PRINT VARIABLE FOR SHIPTO  */~
            prtcustname$30,              /* PRINT VAR FOR SHIPTO NAME  */~
            readkey$45,                  /* READ KEY FOR EWDSLSDT      */~
            rpt_sum$1,                   /* (S)UMMARY OR (D)ETAIL      */~
            rpttitle$60,                 /* Report Title               */~
            saveshipto$9,                /* SAVE VALUE FOR SHIPTO      */~
            saveslsman$4,                /* SAVE VALUE FOR SALESMAN    */~
            shiptoname$30,               /* SHIPTO CUSTOMER NAME       */~
            time$8,                      /* System Time                */~
            gl_acct$9,                   /* Sales Account Number       */~
/*(EWD003)*/err_code$2                   /* Error Code From EWDSLS00   */            

        dim f2%(5%)                      /* = 0 if the file is open    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Sales Analysis Report by Account  "
            pname$ = "APCSLS2  - Rev: R6.04"

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
            * #1  ! EWDSLSDT ! APC Sales Analysis Detail File           *~
            * #4  ! GENCODES ! TABLES MASTER FILE                       *~
            * #5  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

            call "COMPNAME" (12%,company$,ret%)
            if ret% <> 0% then company$ = " "
            call "SHOSTAT" (" Printing Sales Analysis: ACCOUNT X CUSTOME"~
                              & "R X SALESMAN X PRODUCT")
            rpttitle$ = all(hex(20))
            str(rpttitle$,9%,35%) = "Month End Sales Report - Detail by "
            str(rpttitle$,44%,8%) = "Account "
            init(" ")  oldbillto$,savebillto$,billtoname$,savename$
            init(" ")  oldprodcde$,oldshipto$,oldslsman$,oldshiptoname$
            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            init (" ") prodcode$(), prod_d$(), cc$, code$
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            date$ = " "  :  date$ = date : call "DATEFMT" (date$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */

        REM - LOAD SORT ARRAY
            gosub load_sort : last_cnt% = 1%

            readkey$ = all(hex(20))
            if str(rpt_value$,1%,3%) = "ALL" then L01250
               str(readkey$,1%,2%) = str(rpt_value$,1%,2%)
               call "SHOSTAT" ("Processing Account (" &                  ~
                     str(readkey$,1%,6%) & ")" )

/* (EWD001) - Begin */
REM L01250     read #1,key 1% > readkey$,using L01360 ,postdate$,             ~
REM                             shiptoname$, billtoname$, invdisc, linedisc,~
REM                             lineext, readkey$, eod goto end_report

L01250:     read #1,key 1% > readkey$,using L01360,postdate$,err_code$,gl_acct$,~
                                      readkey$,linecom,eod goto end_report
                                                     /*  (EWD003)  */  

            gosub get_billshipto                     /* (EWD002) */
            gosub read_billto
            gosub page_head
            goto L01380
        read_loop
            read #1,using L01360,postdate$,err_code$,gl_acct$,readkey$,linecom, ~
                          eod goto end_report
                                                    /*  (EWD003)  */

REM         FMT CH(6),POS(54),CH(02),CH(9),POS(76),CH(45),POS(195),PD(14,4)
L01360:     FMT CH(6),POS(57),CH(02),CH(9),POS(79),CH(45),POS(198),PD(14,4)

            gosub get_billshipto                  /* (EWD002) */
L01380:     if str(gl_acct$,1%,8%) = "3650-313" then goto read_loop
            if str(gl_acct$,1%,2%) <> "36" then goto read_loop
            if str(err_code$,1%,2%) = "08" then goto read_loop  /*  (EWD003)  */
                                                 /* (EWD002) */
            if str(rpt_value$,1%,3%) = "ALL" then goto L01430
               if str(readkey$,1%,2%) <> str(rpt_value$,1%,2%) then      ~
                                                          goto end_report
                                    /* RE-CALC NET INVOICE AMOUNT     */
                                    /* PRICE AFTER LINE ITEM DISCOUNT */
REM L01430      amount = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                    /* PRICE AFTER ORDER DISCOUNT     */
REM             amount = round(amount * (1.0 - (invdisc/100)), 2)

L01430:         amount = round(linecom, 2)
/* (EWD001) - End */

            if str(readkey$,1%,9%) = oldbillto$ then goto L01510
               call "SHOSTAT" ("Processing Account (" &                  ~
                     str(readkey$,1%,6%) & ")" )

L01510:     if str(readkey$,1%,9%) <> oldbillto$ then gosub billto_brk
            if str(readkey$,10%,9%) <> oldshipto$ then gosub shipto_brk
            if str(readkey$,19%,4%) <> oldslsman$ then gosub slsman_brk
            if str(readkey$,32%,3%) <> oldprodcde$ then gosub prod_brk
            if postdate$ < b_year$ then last_year
            if postdate$ > e_year$ then read_loop
            if postdate$ > e_mnth$ then read_loop
               ytd = round(ytd + amount, 2)
               if postdate$ < b_mnth$ then read_loop
                  mtd = round(mtd + amount, 2)
                  goto read_loop
        last_year
            if postdate$ < b_lyr$ then read_loop
               lyear = round(lyear + amount, 2)
               if postdate$ > e_mlyr$ then read_loop
                  lytd = round(lytd + amount, 2)
               if postdate$ < b_mlyr$ then read_loop
                  lmtd = round(lmtd + amount, 2)
        goto read_loop

        prod_brk
           if oldprodcde$ <> " " then L01740
              goto L01930
L01740:    if lcntr% > 60% then gosub page_head
           init(" ") prtcust$,prtcustname$,prtsls$
           if oldslsman$ = saveslsman$ then L01790
              prtsls$ = oldslsman$
              saveslsman$ = oldslsman$
L01790:    if oldshipto$ = saveshipto$ then L01880
              prtcust$ = oldshipto$
              prtcustname$ = oldshiptoname$
              saveshipto$ = oldshipto$
              mtd   =  round(mtd,2)
              lmtd  =  round(lmtd,2)
              ytd   =  round(ytd,2)
              lytd  =  round(lytd,2)
              lyear =  round(lyear,2)
L01880:    if rpt_sum$ = "S" then goto L01920
              print using L03520   ,prtcust$,prtcustname$,prtsls$,         ~
                                  oldprodcde$,mtd,lmtd,ytd,lytd,lyear
              lcntr% = lcntr% + 1%
L01920:    gosub build_sumrec
L01930:    oldprodcde$ = str(readkey$,32%,3%)
           slslyear = round( slslyear + lyear, 2)
           slslytd  = round( slslytd + lytd, 2)
           slslmtd  = round( slslmtd + lmtd, 2)
           slsmtd   = round( slsmtd + mtd, 2)
           slsytd   = round( slsytd + ytd, 2)
           lyear,lytd,lmtd,mtd,ytd = 0.0
        return
        slsman_brk
           if oldslsman$ <> " " then  L02040
              goto L02050
L02040:    gosub prod_brk
L02050:    oldslsman$ = str(readkey$,19%,4%)
           cuslyear = round( cuslyear + slslyear, 2)
           cuslytd  = round( cuslytd + slslytd, 2)
           cuslmtd  = round( cuslmtd + slslmtd, 2)
           cusmtd   = round( cusmtd + slsmtd, 2)
           cusytd   = round( cusytd + slsytd, 2)
           slslyear,slslytd,slslmtd,slsmtd,slsytd = 0.0
        return
        shipto_brk
           if oldshipto$ <> " " then  L02160
              goto L02240
L02160:    gosub slsman_brk
           if lcntr% > 60% then gosub page_head
           if rpt_sum$ = "S" then goto L02240
              print using L03560
              print using L03580 ,oldshipto$,cusmtd,cuslmtd,cusytd,cuslytd,~
                                cuslyear
              print
              lcntr% = lcntr% + 3%
L02240:    oldshipto$ = str(readkey$,10%,9%)
           oldshiptoname$ = shiptoname$
           actlyear = round( actlyear + cuslyear, 2)
           actlytd  = round( actlytd + cuslytd, 2)
           actlmtd  = round( actlmtd + cuslmtd, 2)
           actmtd   = round( actmtd + cusmtd, 2)
           actytd   = round( actytd + cusytd, 2)
           cuslyear,cuslytd,cuslmtd,cusmtd,cusytd = 0.0
        return
        build_sumrec
           if prodcode$(last_cnt%) <> oldprodcde$ then last_cnt% = 1%
           for cnt% = last_cnt% to 99%
             if cnt% > sort_cnt% then gosub load_descript
             if prodcode$(cnt%) <> oldprodcde$ then L02450
                summtd(cnt%)   = round(summtd(cnt%) + mtd, 2)
                sumlmtd(cnt%)  = round(sumlmtd(cnt%) + lmtd, 2)
                sumytd(cnt%)   = round(sumytd(cnt%) + ytd, 2)
                sumlytd(cnt%)  = round(sumlytd(cnt%) + lytd, 2)
                sumlyear(cnt%) = round(sumlyear(cnt%) + lyear, 2)
                last_cnt% = cnt%
                cnt% = 99%
L02450:    next cnt%
        return
        print_summary
            if lcntr% > 58% then gosub page_head
               print skip (2)
               if oldbillto$ = hex(ff) then oldbillto$ = billto$
               print using L03620 ,oldbillto$
               lcntr% = lcntr% + 3%
               for xcnt% = 1% to 99%
                   if prodcode$(xcnt%) <> " " then L02570
                      xcnt% = 99%
                      goto L02620
L02570:            if lcntr% > 60% then gosub page_head
                   print using L03640 ,prod_d$(xcnt%)  ,summtd(xcnt%),     ~
                                     sumlmtd(xcnt%),sumytd(xcnt%),       ~
                                     sumlytd(xcnt%),sumlyear(xcnt%)
                   lcntr% = lcntr% + 1%
L02620:        next xcnt%
            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            init (" ") prodcode$(), prod_d$(), cc$, code$
            gosub load_sort : last_cnt% = 1%
        return
        billto_brk
           if oldbillto$ <> " " then  L02710
              goto L02780
L02710:    gosub shipto_brk
           gosub print_summary
           if lcntr% > 60% then gosub page_head
           print using L03560
           print using L03600 ,oldbillto$,actmtd,actlmtd,actytd,actlytd,   ~
                             actlyear
           lcntr% = lcntr% + 2%
L02780:    oldbillto$ = str(readkey$,1%,9%)
           savebillto$ = oldbillto$
           savename$ = billtoname$
           if str(oldbillto$,1%,1%) = hex(ff) then L02830
           if lcntr% < 54% then gosub print_headings else gosub page_head
L02830:    actlyear,actlytd,actlmtd,actmtd,actytd = 0.0
        return
        end_report                /* Report Ending Routine */
            billto$ = str(readkey$,1%,9%)
            str(readkey$,1%,9%) = all(hex(ff))
            gosub billto_brk
            print skip(2)
            print using L03690      /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto exit_end

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L03400 , date$, time$, company$, "APCSLS2 "
            print using L03440 , per%,rpttitle$, pcntr%
            print
            if oldbillto$ = savebillto$ then no_break
            print using L03460 ,str(readkey$,1%,9%),billtoname$
            print
            print using L03470
            print using L03490
            lcntr% = 7%
            return

        no_break
            print using L03460 ,str(savebillto$,1%,9%),savename$
            print
            print using L03470
            print using L03490
            lcntr% = 7%
            return

        read_billto
           savebillto$ = str(readkey$,1%,9%)
           savename$ = billtoname$
        return

        print_headings         /* Heading Print Routine */
            if lcntr% = 7% then return
            if lcntr% > 54% then goto page_head
            print skip(2)
            print using L03460 ,str(readkey$,1%,9%),billtoname$
            print
            print using L03470
            print using L03490
            lcntr% = lcntr% + 6%
            return

        get_billshipto                             /* (EWD002) */
            billto$ = str(readkey$,1%,9%)
            shipto$ = str(readkey$,10%,9%)
            
             read #5, key = shipto$, using L03010,shiptoname$,           ~
                                      eod goto L03000
L03010:          FMT POS(253),CH(30)

L03000:      read #5,key = billto$,  using L03010,billtoname$,           ~
                                      eod goto no_billshipto
        no_billshipto
        return                                     /* (EWD002) */

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L03400: %Run ######## @ ########              ###########################~
        ~#################################                ########:APCSLS2

*       * Header Line 2
L03440: %Period: ##                           ###########################~
        ~#################################                     Page: ####
L03460: %Account: #########  ##############################
L03470: %Customer  Customer Name                  Salesman  PRD   $ MTD  ~
        ~       $ Last YR MTD     $ YTD      $ Last YTD     Tot $ LST YR
L03490: %--------- ------------------------------ --------  --- ---------~
        ~----- -------------- -------------- -------------- --------------

L03520: % ######### ##############################  ####    ###  #,###,##~
        ~#.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-
        %  **** Total for Salesman #### ****                    ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
L03560: %                                                       ---------~
        ~----- -------------- -------------- -------------- --------------
L03580: %       **** Total for Customer ######### ****          ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
L03600: %            **** Total for Account ######### ****      ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
L03620: %                                     ***** Month End Summary by ~
        ~Product for Account ######### ****
L03640: %                            ########################## ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
        %** Report Title for page 0
        %############################################################

L03690: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        load_sort
            sort_cnt%, cnt% = 0%
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
        load_nxt_sort
            read #4,key > cc$,using L03780,cc$ ,eod goto L03880
L03780:         FMT CH(24)
            if str(cc$,1%,9%) <> "SLS CODE2" then goto L03880
               code$ = str(cc$,10%,3%)
               if code$ = "XXX" then goto L03880
               cnt% = cnt% + 1%
               get #4,using L03840, prod_d$(cnt%)
L03840:           FMT POS(25), CH(20)
               str(prod_d$(cnt%),21%,6%) = " (" & code$ & ")"
               prodcode$(cnt%) = code$
               goto load_nxt_sort
L03880: sort_cnt% = cnt%
        return

        load_descript
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
            str(cc$,10%,15%) = oldprodcde$
            read #4,key = cc$,using L03960,prod_d$(cnt%) ,eod goto L04000
L03960:         FMT POS(25), CH(20)
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
            prodcode$(cnt%) = str(cc$,10%,3%)
        return
L04000:     str(prod_d$(cnt%),1%,20%) = "Code Not Defined ???"
            prodcode$(cnt%) = oldprodcde$
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_end

            end