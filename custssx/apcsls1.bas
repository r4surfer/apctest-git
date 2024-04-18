        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCSLS1                              *~
            *  Creation Date     - 02/10/95                             *~
            *  Last Modified Date- 08/08/00                             *~
            *  Description       - Subroutine to Create Sales Analysis  *~
            *                      Report By Salesman.                  *~
            *                                                           *~
            *  Code Tables Used  - (SLS CODE2) - Table contains the     *~
            *                          descriptions Assoc. with the     *~
            *                          Sales Analysis Product Sort Codes*~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/17/95 ! Mod to Sort Summary ( LOAD_SORT )        !     *~
            * 11/11/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 06/05/00 ! Mod to use linecom for amount instead of ! CMG *~
            *          !     of calculating. (EWD001)             !     *~
            * 07/05/00 ! Mod to use EWDSLSDT instead of APCSLSDT  ! CMG *~
            *          !     (EWD002)                             !     *~
            * 08/08/00 ! Mod to take out error code 8 (EWD003)    ! CMG *~     
            * 12/20/18 ! CR1828 EWDSLSDT conversion for Dallas    ! DES *~     
            *************************************************************

        sub "APCSLS1" (#1,#4,#5,b_mnth$,e_mnth$,b_year$,e_year$,b_mlyr$, ~
                       e_mlyr$,b_lyr$,e_lyr$,per%,rpt_value$,rpt_sum$)
                                         /* (EWD002) - ADD CUSTOMER FILE */
        dim                                                              ~
            code_key$24,                 /* HOME CENTER'S              */~
            b_lyr$6,                     /* START DATE LAST YEAR       */~
            b_mlyr$6,                    /* START DATE THIS MNTH LST YR*/~
            b_mnth$6,                    /* START DATE THIS MONTH      */~
            b_year$6,                    /* START DATE THIS YEAR       */~
            rpt_value$9,                 /* SPECIFIED SALESMAN OR ALL  */~
            company$60,                  /* Company or Division Name   */~
            date$8,                      /* Date for screen display    */~
            e_lyr$6,                     /* END DTE LAST YEAR          */~
            e_mlyr$6,                    /* END DTE THIS MNTH LAST YEAR*/~
            e_mnth$6,                    /* END DTE THIS MONTH         */~
            e_year$6,                    /* END DATE FOR THIS  YEAR    */~
            summtd(99%),sumlmtd(99%),    /* SUMMARY BUCKETS FOR TOTALS */~
            sumytd(99%),sumlytd(99%),    /* SUMMARY BUCKETS FOR TOTALS */~
            sumlyear(99%),               /* SUMMARY BUCKET FOR TOTALS  */~
            hommtd(99%),homlmtd(99%),    /* HOM CTR BUCKETS FOR TOTALS */~
            homytd(99%),homlytd(99%),    /* HOM CTR BUCKETS FOR TOTALS */~
            homlyear(99%),               /* HOM CTR BUCKET FOR TOTALS  */~
            oldprodcde$3,                /* SAVE VALUE FOR PROD CODE   */~
            shipto$9,                    /* SHIPTO CUST                */~
            oldshipto$9,                 /* SAVE VALUE FOR SHIPTO CUST */~
            oldshiptoname$30,            /* SAVE VALUE FOR CUTOMER NAME*/~
            oldslsman$4,                 /* SAVE VALUE FO SALESMAN     */~
            postdate$6,                  /* INVOICE POSTING DATE       */~
            prodcode$(99%)3,             /* PRODUCT CODES ARRAY        */~
            prod_d$(99%)26, cc$24,       /* PRODUCT CODE DESCRIPTIONS  */~
            prod_h$(99%)26,              /* PRODUCT CODE DESCRIPTIONS  */~
            homecode$(99%)3,             /* PRODUCT CODES ARRAY        */~
            prtcust$9,                   /* PRINT VARIABLE FOR SHIPTO  */~
            prtcustname$30,              /* PRINT VAR FOR SHIPTO NAME  */~
            readkey$27,                  /* READ KEY FOR APCSADTL      */~
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
            apc$   = "(New) Sales Analysis Report By Salesman "
            pname$ = "APCSLS1  - Rev: R6.04"

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
            * #4  ! GENCODES ! MASTER TABLE FILE                        *~
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
            call "SHOSTAT" (" Printing Sales Analysis: Salesman X Custom"~
                              & "er X Product")
            rpttitle$ = all(hex(20))
            str(rpttitle$,9%,35%) = "Month End Sales Report - Detail by "
            str(rpttitle$,44%,8%) = "Salesman"
            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            mat hommtd = zer : mat homlmtd = zer : mat homytd = zer
            mat homlytd = zer : mat homlyear = zer
            init (" ") prodcode$(),postdate$,shiptoname$,readkey$,       ~
                       prtcust$,prtcustname$,saveslsman$,oldslsman$,     ~
                       oldprodcde$,oldshipto$, prod_d$(), prod_h$()
            home% = 0% : gosub load_sort : last_cnt% = 1%
            home% = 1% : gosub load_sort : last_home_cnt% = 1%

            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            date$ = " "  :  date$ = date : call "DATEFMT" (date$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            home%, home_build%, home_flag% = 0%
            homlyear,homlytd,homlmtd,hommtd,homytd = 0.0
            if lcntr% > 56% then gosub page_head

            readkey$ = all(hex(00))
            if str(rpt_value$,1%,3%) = "ALL" then goto L01280
               str(readkey$,1%,4%) = str(rpt_value$,1%,4%)
               call "SHOSTAT" ("Processing Salesman ("&                  ~
                     str(readkey$,1%,4%) & ")")
/* (EWD001) - Begin */
REM L01280      read #1,key 4% > readkey$,using L01370 ,postdate$,shiptoname$, ~
REM                             invdisc, linedisc, lineext, readkey$,       ~
REM                             eod goto end_report

L01280:
           read #1,key 3% > readkey$,using L01370,postdate$,err_code$,gl_acct$,~
                                      readkey$, linecom, eod goto end_report
                                                         /*  (EWD003)  */
            gosub get_shipto
            goto L01390
        read_loop
            read #1,using L01370 ,postdate$,err_code$,gl_acct$,readkey$,linecom,   ~
                                    eod goto end_report
                                                            /*  (EWD003)  */
REM           FMT CH(6),POS(54),CH(02),CH(9),POS(94),CH(27),POS(195),PD(14,4)
L01370:       FMT CH(6),POS(57),CH(02),CH(9),POS(97),CH(27),POS(198),PD(14,4)

            gosub get_shipto                                /* (EWD002)  */
/* (EWD001) - End */                                        /* (EWD002)  */
L01390:     if str(gl_acct$,1%,8%) = "3650-313" then goto read_loop
            if str(gl_acct$,1%,2%) <> "36" then goto read_loop
            if str(err_code$,1%,2%) = "08" then goto read_loop  /*  (EWD003)  */
            if str(rpt_value$,1%,3%) = "ALL" then goto L01450
               if str(readkey$,1%,4%) <> str(rpt_value$,1%,4%) then      ~
                                                           goto end_report

                                    /* RE-CALC NET INVOICE AMOUNT     */
                                    /* PRICE AFTER LINE ITEM DISCOUNT */
                                            /* (EWD001) - Begin */
L01450: REM    amount = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                    /* PRICE AFTER ORDER DISCOUNT     */
        REM    amount = round(amount * (1.0 - (invdisc/100)), 2)

               amount = round(linecom, 2)    /* (EWD001) - End */

            if str(readkey$,1%,4%) = oldslsman$ then goto L01530
               call "SHOSTAT" ("Processing Salesman ("&                  ~
                     str(readkey$,1%,4%) & ")")

L01530:     if str(readkey$,1%,4%) <> oldslsman$ then gosub slsman_brk
            if str(readkey$,5%,9%) <> oldshipto$ then gosub shipto_brk
            if str(readkey$,14%,3%) <> oldprodcde$ then gosub prod_brk
            if postdate$ < b_year$ then last_year
            if postdate$ > e_year$ then read_loop
            if postdate$ > e_mnth$ then read_loop
               ytd = round( ytd + amount, 2)
               if postdate$ < b_mnth$ then read_loop
               mtd = round( mtd + amount, 2)
               goto read_loop
        last_year
            if postdate$ < b_lyr$ then read_loop
                  lyear = round( lyear + amount, 2)
               if postdate$ > e_mlyr$ then read_loop
                  lytd  = round( lytd + amount, 2)
               if postdate$ < b_mlyr$ then read_loop
                  lmtd  = round( lmtd + amount, 2)
        goto read_loop
        prod_brk
           if oldprodcde$ <> " " then L01750
              oldprodcde$ = str(readkey$,14%,3%)
              goto L01930
L01750:    if lcntr% > 55% then gosub page_head
           init(" ") prtcust$,prtcustname$,prtsls$
           if oldslsman$ = saveslsman$ then L01800
              prtsls$ = oldslsman$
              saveslsman$ = oldslsman$
L01800:    if oldshipto$ = saveshipto$ then goto L01890
              prtcust$ = oldshipto$
              prtcustname$ = oldshiptoname$
              saveshipto$ = oldshipto$
              mtd   = round(mtd,2)
              lmtd  = round(lmtd,2)
              ytd   = round(ytd,2)
              lytd  = round(lytd,2)
              lyear = round(lyear,2)
L01890:    if rpt_sum$ = "S" then goto L01930
           print using L04450 ,prtsls$,prtcust$,prtcustname$,           ~
                                oldprodcde$,mtd,lmtd,ytd,lytd,lyear
              lcntr% = lcntr% + 1%
L01930:    gosub build_sumrec
           gosub build_home_center                      /* HOME CENTER */
           oldprodcde$ = str(readkey$,14%,3%)
           cuslyear = round(cuslyear + lyear, 2)
           cuslytd  = round(cuslytd + lytd, 2)
           cuslmtd  = round(cuslmtd + lmtd, 2)
           cusmtd   = round(cusmtd + mtd, 2)
           cusytd   = round(cusytd + ytd, 2)
           lyear,lytd,lmtd,mtd,ytd = 0.0
        return
        shipto_brk
           if oldshipto$ <> " " then  L02070
              gosub check_home_center                   /* HOME CENTER */
              goto L02270
L02070:    gosub prod_brk
           if lcntr% > 55% then gosub page_head
           if rpt_sum$ = "S" then goto L02150
              print using L04490
              print using L04470 ,oldshipto$,cusmtd,cuslmtd,cusytd,cuslytd,~
                                cuslyear
              print
              lcntr% = lcntr% + 3%
L02150:    gosub check_home_center                      /* HOME CENTER */
           if home% = 0% and home_build% = 0% then goto L02270
              if home% = 1% and home_build% = 0% then goto L02270
              if home% = 0% and home_build% = 1% then                    ~
                                                 gosub print_home_center
              if home% = 1% and home_build% = 1% then goto L02220
                 goto L02270
L02220:       if str(oldshipto$,1%,2%) = str(readkey$,5%,2%) then        ~
                                                               goto L02270
                 gosub print_home_center
                 gosub check_home_center                /* HOME CENTER */

L02270:    oldshipto$ = str(readkey$,5%,9%)
           oldshiptoname$ = shiptoname$
           slslyear = round(slslyear + cuslyear, 2)
           slslytd  = round(slslytd + cuslytd, 2)
           slslmtd  = round(slslmtd + cuslmtd, 2)
           slsmtd   = round(slsmtd + cusmtd, 2)
           slsytd   = round(slsytd + cusytd, 2)
           cuslyear,cuslytd,cuslmtd,cusmtd,cusytd = 0.0
        return
        slsman_brk
           if oldslsman$ <> " " then  L02390
              goto L02470
L02390:    gosub shipto_brk
           gosub print_summary
           gosub check_home_center           
           if lcntr% > 55% then gosub page_head
           print using L04490
           print using L04510 ,oldslsman$,slsmtd,slslmtd,slsytd,slslytd,   ~
                             slslyear
           print skip(2)
           lcntr% = lcntr% + 4%
L02470:    oldslsman$ = str(readkey$,1%,4%)
           slslyear,slslytd,slslmtd,slsmtd,slsytd = 0.0
        return
        build_sumrec
            if prodcode$(last_cnt%) <> oldprodcde$ then last_cnt% = 1%
            for cnt% = last_cnt% to 99%
                if cnt% > sort_cnt% then gosub load_descript_p
                if prodcode$(cnt%) <> oldprodcde$ then L02620
                   summtd(cnt%)   = round( summtd(cnt%) + mtd, 2)
                   sumlmtd(cnt%)  = round( sumlmtd(cnt%) + lmtd, 2)
                   sumytd(cnt%)   = round( sumytd(cnt%) + ytd, 2)
                   sumlytd(cnt%)  = round( sumlytd(cnt%) + lytd, 2)
                   sumlyear(cnt%) = round( sumlyear(cnt%) + lyear, 2)
                   last_cnt% = cnt%
                   cnt% = 99%
L02620:     next cnt%
        return
        print_summary
            if lcntr% > 53% then gosub page_head
               print
               print using L04540 ,oldslsman$
               lcntr% = lcntr% + 2%
               for xcnt% = 1% to 99%
                  if prodcode$(xcnt%) <> " " then L02770
                      xcnt% = 99%
                      goto L02810
                   if lcntr% > 55% then gosub page_head
                      print
                      print using L04540 , oldslsman$
                      lcntr% = lcntr% + 2%
L02770:            print using L04560 ,prod_d$(xcnt%)  ,summtd(xcnt%),     ~
                                     sumlmtd(xcnt%),sumytd(xcnt%),       ~
                                     sumlytd(xcnt%),sumlyear(xcnt%)
                   lcntr% = lcntr% + 1%
L02810:        next xcnt%
            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            init(" ") prodcode$(), prod_d$()
            home% = 0% : gosub load_sort : last_cnt% = 1%
        return
        end_report                /* Report Ending Routine */
            gosub slsman_brk
            if lcntr% > 55% then gosub page_head
            print skip(2)
            print using L04610      /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto exit_program

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L04300 , date$, time$, company$, "APCSLS1 "
            print using L04340 , per%,rpttitle$, pcntr%
            print
            if home_flag% = 0% then goto L03050
               print using L04370 , str(oldshipto$,1%,2%)
               print
L03050:     print using L04400
            print using L04420
            lcntr% = 5%
            return

        build_home_center
            if home% = 0% then return
            home_build% = 1%
            if homecode$(last_home_cnt%) <> oldprodcde$ then             ~
                                                      last_home_cnt% = 1%
            for cnt% = last_home_cnt% to 99%
                if cnt% > home_cnt% then gosub load_descript_h
                if homecode$(cnt%) <> oldprodcde$ then L03250
                   hommtd(cnt%)   = round( hommtd(cnt%) + mtd, 2)                
                   homlmtd(cnt%)  = round( homlmtd(cnt%) + lmtd, 2)
                   homytd(cnt%)   = round( homytd(cnt%) + ytd, 2)
                   homlytd(cnt%)  = round( homlytd(cnt%) + lytd, 2)
                   homlyear(cnt%) = round( homlyear(cnt%) + lyear, 2)
                   last_home_cnt% = cnt%
                   cnt% = 99%
L03250:     next cnt%
            homlyear = round(homlyear + lyear, 2)
            homlytd  = round(homlytd + lytd, 2)
            homlmtd  = round(homlmtd + lmtd, 2)
            hommtd   = round(hommtd + mtd, 2)
            homytd   = round(homytd + ytd, 2)
        return

        print_home_center
             home_flag% = 1%
             gosub page_head
             for xcnt% = 1% to 99%
                 if homecode$(xcnt%) <> " " then L03400
                    xcnt% = 99%
                    goto L03440
L03400:          print using L04560 ,prod_h$(xcnt%)  ,hommtd(xcnt%),       ~
                                   homlmtd(xcnt%),homytd(xcnt%),         ~
                                   homlytd(xcnt%),homlyear(xcnt%)
                 lcntr% = lcntr% + 1%
L03440:        next xcnt%
           print using L04490
           print using L04510 ,oldslsman$,hommtd,homlmtd,homytd,homlytd,   ~
                             homlyear
            homlyear,homlytd,homlmtd,hommtd,homytd = 0.0
            mat hommtd = zer : mat homlmtd = zer : mat homytd = zer
            mat homlytd = zer : mat homlyear = zer
            init(" ") homecode$(), prod_h$()
            home% = 1% : gosub load_sort : last_home_cnt% = 1%
            home%, home_build%, home_flag% = 0%
            lcntr% = 99%
        return

        check_home_center
            home% = 0%
            init(" ") code_key$
            str(code_key$,1%,9%) = "HOME CTRS"
            str(code_key$,10%,15%) = str(readkey$,5%,9%)
            read #4,key = code_key$, eod goto L03640
            home% = 1%
L03640: return

        load_sort
            if home% = 0% then sort_cnt% = 0%
            if home% = 1% then home_cnt% = 0%
            cnt% = 0%
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
        load_nxt_sort
            read #4,key > cc$,using L03740,cc$ ,eod goto L03900
L03740:         FMT CH(24)
            if str(cc$,1%,9%) <> "SLS CODE2" then goto L03900
               code$ = str(cc$,10%,3%)
               if code$ = "XXX" then goto L03900
               cnt% = cnt% + 1%
               if home% = 1% then goto L03850
                  get #4,using L03810, prod_d$(cnt%)
L03810:              FMT POS(25), CH(20)
                  str(prod_d$(cnt%),21%,6%) = " (" & code$ & ")"
                  prodcode$(cnt%) = code$
                  goto load_nxt_sort
L03850:        get #4,using L03810, prod_h$(cnt%)
               str(prod_h$(cnt%),21%,6%) = " (" & code$ & ")"
               homecode$(cnt%) = code$
               goto load_nxt_sort

L03900: if home% = 0% then sort_cnt% = cnt%
        if home% = 1% then home_cnt% = cnt%
        return

        load_descript_p
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
            str(cc$,10%,15%) = oldprodcde$
            read #4,key = cc$,using L03990,prod_d$(cnt%) ,eod goto L04030
L03990:         FMT POS(25), CH(20)
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
            prodcode$(cnt%) = str(cc$,10%,3%)
        return
L04030:     str(prod_d$(cnt%),1%,20%) = "Code Not Defined ???"
            prodcode$(cnt%) = oldprodcde$
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
        return

        load_descript_h                        /* SPECIAL HOME CENTERS */
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
            str(cc$,10%,15%) = oldprodcde$
            read #4,key = cc$,using L04130,prod_h$(cnt%) ,eod goto L04170
L04130:         FMT POS(25), CH(20)
            str(prod_h$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
            homecode$(cnt%) = str(cc$,10%,3%)
        return
L04170:     str(prod_h$(cnt%),1%,20%) = "Code Not Defined ???"
            homecode$(cnt%) = oldprodcde$
            str(prod_h$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
        return

        get_shipto                                    /* (EWD002) */
            shipto$ = str(readkey$,5%,9%)
            
            read #5, key = shipto$, using L04180,shiptoname$,           ~
                                      eod goto no_shipto
L04180:          FMT POS(253),CH(30)

        no_shipto
        return                                         /* (EWD002) */

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L04300: %Run ######## @ ########              ###########################~
        ~#################################                ########:APCSLS1

*       * Header Line 2
L04340: %Period: ##                           ###########################~
        ~#################################                     Page: ####

L04370: %                                                  H O M E   C E ~
        ~N T E R   (##)

L04400: %Salesman  Customer  Customer Name                  PRD   $ MTD  ~
        ~       $ Last YR MTD     $ YTD      $ Last YTD     Tot $ LST YR
L04420: %--------  --------- ------------------------------ --- ---------~
        ~----- -------------- -------------- -------------- --------------

L04450: % ####     ######### ############################## ###  #,###,##~
        ~#.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-
L04470: %  **** Total for Customer ######### ****               ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
L04490: %                                                       ---------~
        ~----- -------------- -------------- -------------- --------------
L04510: %       **** Total for Salesman #### ****               ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-

L04540: %                                     ***** Month End Summary by ~
        ~Product for Salesman #### *****
L04560: %                           ##########################  ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
        %** Report Title for page 0
        %############################################################

L04610: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        exit_program

            end
