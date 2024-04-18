        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCSLS3                              *~
            *  Creation Date     - 02/10/95                             *~
            *  Last Modified Date- 08/08/00                             *~
            *  Description       - Subroutine to Create Sales Analysis  *~
            *                      Report By Shipto Account.            *~
            *                                                           *~
            *  Code Tables Used  - (SLS CODE2) - Code Table             *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 02/24/95 ! PRINT S.A. DESCRIPTIONS                  !     *~
            * 03/17/95 ! Mod to Sort Summary - ( LOAD_SORT )      !     *~
            * 11/11/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 06/05/00 ! Mod to use linecom for amount instead of ! CMG *~
            *          !     of calculating. (EWD001)             !     *~
            * 07/05/00 ! Mod to use EWDSLSDT instead of APCSLSDT  ! CMG *~
            *          !     (EWD002)                             !     *~
            * 07/05/00 ! Mod to use EWDSLSDT instead of APCSLSDT  ! CMG *~
            *          !     (EWD003)                             !     *~
            * 08/08/00 ! Mod to take out error code 8 (EWD004)    ! CMG *~     
            * 12/20/18 ! CR1828 EWDSLSDT conversion for Dallas    ! DES *~     
            *************************************************************

        sub "APCSLS3" (#1,#4,#5,b_mnth$,e_mnth$,b_year$,e_year$,b_mlyr$, ~
                       e_mlyr$,b_lyr$,e_lyr$,per%,rpt_value$,rpt_sum$ )
                                         /* (EWD003) - ADD CUSTOMER FILE */                       

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
            summtd(99%),sumlmtd(99%),    /* SUMMARY BUCKETS FOR TOTALS */~
            sumytd(99%),sumlytd(99%),    /* SUMMARY BUCKETS FOR TOTALS */~
            sumlyear(99%),               /* SUMMARY BUCKET FOR TOTALS  */~
            oldprodcde$3,                /* SAVE VALUE FOR PROD CODE   */~
            oldshipto$9,                 /* SAVE VALUE FOR SHIPTO CUST */~
            oldshiptoname$30,            /* SAVE VALUE FOR CUTOMER NAME*/~
            oldslsman$4,                 /* SAVE VALUE FO SALESMAN     */~
            postdate$6,                  /* INVOICE POSTING DATE       */~
            prodcode$(99%)3,             /* PRODUCT CODES ARRAY        */~
            prod_d$(99%)26, cc$24,       /* PROD CODE DESCRIPTIONS     */~
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
/*(EWD004)*/err_code$2                   /* Error Code From EWDSLS00   */            

        dim f2%(5%)                      /* = 0 if the file is open    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Sales Analysis by Ship To Account "
            pname$ = "APCSLS3  - Rev: R6.04"

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
            * #4  ! GENCODES ! TABLE MASTER FILE                        *~
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
            call "SHOSTAT" (" Printing Sales Analysis: CUSTOMER X SALESM"~
                              & "AN X Product")
            rpttitle$ = all(hex(20))
            str(rpttitle$,9%,35%) = "Month End Sales Report - Detail by "
            str(rpttitle$,44%,8%) = "Customer"
           oldprodcde$,oldshipto$,oldslsman$,oldshiptoname$ = all(hex(20))
            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            init (" ") prodcode$(),oldshipto$,oldslsman$,oldprodcde$,    ~
                       prod_d$(), cc$, code$
        REM - LOAD SORT ARRAY
            gosub load_sort
            last_cnt% = 1%
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            date$ = " "  : date$ = date : call "DATEFMT" (date$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head
            readkey$ = all(hex(20))
            if str(rpt_value$,1%,3%) = "ALL" then goto L01190
               str(readkey$,1%,6%) = str(rpt_value$,1%,6%)
               call "SHOSTAT" ("Processing Customer (" &                 ~
                     str(readkey$,1%,6%) & ")" )

/* (EWD001) - Begin */
REM L01190      read #1,key 2% > readkey$,using L01270 ,postdate$, shiptoname$,~
REM                              invdisc, linedisc, lineext, readkey$,       ~
REM                              eod goto end_report

L01190:     read #1,key 2% > readkey$,using L01270 ,postdate$,err_code$,gl_acct$,~
                                          readkey$,linecom, eod goto end_report
                                                   /*  (EWD004)  */
            gosub get_shipto
            goto L01300
        read_loop
            read #1,using L01270 ,postdate$,err_code$,gl_acct$,readkey$,linecom, ~
                                  eod goto end_report
                                                   /*  (EWD004)  */

REM              FMT CH(6),POS(54),CH(02),CH(9),POS(85),CH(36),POS(195),PD(14,4)
L01270:          FMT CH(6),POS(57),CH(02),CH(9),POS(88),CH(36),POS(198),PD(14,4)

            gosub get_shipto                         /* (EWD003) */
L01300:     if str(gl_acct$,1%,8%) = "3650-313" then goto read_loop
            if str(gl_acct$,1%,2%) <> "36" then goto read_loop
                                                     /* (EWD003) */
            if str(err_code$,1%,2%) = "08" then goto read_loop
                                                     /*  (EWD004)  */
            if str(rpt_value$,1%,3%) = "ALL" then goto L01350
               if str(readkey$,1%,6%) <> str(rpt_value$,1%,6%) then      ~
                                                          goto end_report
                                    /* RE-CALC NET INVOICE AMOUNT     */
                                    /* PRICE AFTER LINE ITEM DISCOUNT */
REM L01350      amount = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                    /* PRICE AFTER ORDER DISCOUNT     */
REM             amount = round(amount * (1.0 - (invdisc/100)), 2)

L01350:         amount = round(linecom, 2)

/* (EWD001) - End */
            if str(readkey$,1%,9%) = oldshipto$ then goto L01430
               call "SHOSTAT" ("Processing Customer (" &                 ~
                     str(readkey$,1%,6%) & ")" )

L01430:     if str(readkey$,1%,9%) <> oldshipto$ then gosub shipto_brk
            if str(readkey$,10%,4%) <> oldslsman$ then gosub slsman_brk
            if str(readkey$,23%,3%) <> oldprodcde$ then gosub prod_brk
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
                  lytd  = round(lytd + amount, 2)
                  if postdate$ < b_mlyr$ then read_loop
                      lmtd  = round(lmtd + amount, 2)
        goto read_loop
        prod_brk
           if oldprodcde$ <> " " then L01640
              goto L01830
L01640:    if lcntr% > 55% then gosub page_head
           init(" ") prtcust$,prtcustname$,prtsls$
           if oldslsman$ = saveslsman$ then L01690
              prtsls$ = oldslsman$
              saveslsman$ = oldslsman$
L01690:    if oldshipto$ = saveshipto$ then L01780
              prtcust$ = oldshipto$
              prtcustname$ = oldshiptoname$
              saveshipto$ = oldshipto$
              mtd   = round(mtd,2)
              lmtd  = round(lmtd,2)
              ytd   = round(ytd,2)
              lytd  = round(lytd,2)
              lyear = round(lyear,2)
L01780:    if rpt_sum$ = "S" then goto L01820
              print using L02990   ,prtcust$,prtcustname$,prtsls$,         ~
                                  oldprodcde$,mtd,lmtd,ytd,lytd,lyear
              lcntr% = lcntr% + 1%
L01820:    gosub build_sumrec
L01830:    oldprodcde$ = str(readkey$,23%,3%)
           slslyear = round(slslyear + lyear, 2)
           slslytd  = round(slslytd + lytd, 2)
           slslmtd  = round(slslmtd + lmtd, 2)
           slsmtd   = round(slsmtd + mtd, 2)
           slsytd   = round(slsytd + ytd, 2)
           lyear,lytd,lmtd,mtd,ytd = 0.0
        return
        slsman_brk
           if oldslsman$ <> " " then  L01940
              goto L01950
L01940:    gosub prod_brk
L01950:    oldslsman$ = str(readkey$,10%,4%)
           cuslyear   = round(cuslyear + slslyear, 2)
           cuslytd    = round(cuslytd + slslytd, 2)
           cuslmtd    = round(cuslmtd + slslmtd, 2)
           cusmtd     = round(cusmtd + slsmtd, 2)
           cusytd     = round(cusytd + slsytd, 2)
           slslyear,slslytd,slslmtd,slsmtd,slsytd = 0.0
        return
        shipto_brk
           if oldshipto$ <> " " then  L02060
              goto L02140
L02060:    gosub slsman_brk
           if lcntr% > 55% then gosub page_head
           if rpt_sum$ = "S" then goto L02140
              print using L03030
              print using L03050 ,oldshipto$,cusmtd,cuslmtd,cusytd,cuslytd,~
                                cuslyear
              print
              lcntr% = lcntr% + 3%
L02140:    oldshipto$ = str(readkey$,1%,9%)
           oldshiptoname$ = shiptoname$
           cuslyear,cuslytd,cuslmtd,cusmtd,cusytd = 0.0
        return
        build_sumrec
            if prodcode$(last_cnt%) <> oldprodcde$ then last_cnt% = 1%
            for cnt% = last_cnt% to 99%
                if cnt% > sort_cnt% then gosub load_descript
                if prodcode$(cnt%) <> oldprodcde$ then L02300
                   summtd(cnt%)   = round(summtd(cnt%) + mtd, 2)
                   sumlmtd(cnt%)  = round(sumlmtd(cnt%) + lmtd, 2)
                   sumytd(cnt%)   = round(sumytd(cnt%) + ytd, 2)
                   sumlytd(cnt%)  = round(sumlytd(cnt%) + lytd, 2)
                   sumlyear(cnt%) = round(sumlyear(cnt%) + lyear, 2)
                   last_cnt% = cnt%
                   cnt% = 99%
L02300:     next cnt%
        return
        print_summary
            totmtd,totlmtd,totytd,totlytd,totlyear = 0.0
            if lcntr% > 55% then gosub page_head
               print skip (2)
               print using L03080
               lcntr% = lcntr% + 3%
               for xcnt% = 1% to 99%
                   if prodcode$(xcnt%) <> " " then L02420
                      xcnt% = 99%
                      goto L02520
L02420:            if lcntr% > 55% then gosub page_head
                   print using L03100 ,prod_d$(xcnt%)  ,summtd(xcnt%),     ~
                                     sumlmtd(xcnt%),sumytd(xcnt%),       ~
                                     sumlytd(xcnt%),sumlyear(xcnt%)
                   totmtd   = round(totmtd + summtd(xcnt%), 2)
                   totlmtd  = round(totlmtd + sumlmtd(xcnt%), 2)
                   totytd   = round(totytd + sumytd(xcnt%), 2)
                   totlytd  = round(totlytd + sumlytd(xcnt%), 2)
                   totlyear = round(totlyear + sumlyear(xcnt%), 2)
                   lcntr% = lcntr% + 1%
L02520:        next xcnt%
                   if lcntr% > 55 then gosub page_head
                   print using L03030
                   print using L03100 , " ",totmtd,totlmtd,totytd,totlytd, ~
                                          totlyear
            mat summtd = zer : mat sumlmtd = zer : mat sumytd = zer
            mat sumlytd = zer : mat sumlyear = zer
            totmtd,totlmtd,totytd,totlytd,totlyear = 0.0
        return
        end_report                /* Report Ending Routine */
            gosub shipto_brk
            gosub print_summary
            print skip(2)
            print using L03150      /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto exit_end

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L02880 , date$, time$, company$, "APCSLS3 "
            print using L02920 ,per%,rpttitle$, pcntr%
            print
            print using L02940
            print using L02960
            lcntr% = 5%
            return

        get_shipto                                      /* (EWD003) */
            shipto$ = str(readkey$,1%,9%)
            
            read #5, key = shipto$, using L04180,shiptoname$,           ~
                                      eod goto no_shipto
L04180:          FMT POS(253),CH(30)

        no_shipto
        return                                         /* (EWD003) */

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L02880: %Run ######## @ ########              ###########################~
        ~#################################                ########:APCSLS3

*       * Header Line 2
L02920: %Period: ##                           ###########################~
        ~#################################                     Page: ####
L02940: %Customer  Customer Name                  Salesman  PRD   $ MTD  ~
        ~       $ Last YR MTD     $ YTD      $ Last YTD     Tot $ LST YR
L02960: %--------- ------------------------------ --------  --- ---------~
        ~----- -------------- -------------- -------------- --------------

L02990: % ######### ##############################  ####    ###  #,###,##~
        ~#.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-  #,###,###.##-
        %  **** Total for Salesman #### ****                    ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-
L03030: %                                                       ---------~
        ~----- -------------- -------------- -------------- --------------
L03050: %       **** Total for Customer ######### ****          ##,###,##~
        ~#.##- ##,###,###.##- ##,###,###.##- ##,###,###.##- ##,###,###.##-

L03080: %                                     ***** Month End Summary by ~
        ~Product for All Customers *****
L03100: %                       ########################## ###,###,##~
        ~#.##- ###,###,###.##- ###,###,###.##- ###,###,###.##- ###,###,###.##-
        %** Report Title for page 0
        %############################################################

L03150: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

        load_sort
            sort_cnt%, cnt% = 0%
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
        load_nxt_sort
            read #4,key > cc$,using L03240,cc$ ,eod goto L03340
L03240:         FMT CH(24)
            if str(cc$,1%,9%) <> "SLS CODE2" then goto L03340
               code$ = str(cc$,10%,3%)
               if code$ = "XXX" then goto L03340
               cnt% = cnt% + 1%
               get #4,using L03300, prod_d$(cnt%)
L03300:           FMT POS(25), CH(20)
               str(prod_d$(cnt%),21%,6%) = " (" & code$ & ")"
               prodcode$(cnt%) = code$
               goto load_nxt_sort
L03340: sort_cnt% = cnt%
        return

        load_descript
            init(" ") cc$
            str(cc$,1%,9%)   = "SLS CODE2"
            str(cc$,10%,15%) = oldprodcde$
            read #4,key = cc$,using L03420,prod_d$(cnt%) ,eod goto L03460
L03420:         FMT POS(25), CH(20)
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
            prodcode$(cnt%) = str(cc$,10%,3%)
        return
L03460:     str(prod_d$(cnt%),1%,20%) = "Code Not Defined ???"
            prodcode$(cnt%) = oldprodcde$
            str(prod_d$(cnt%),21%,6%) = " (" & oldprodcde$ & ")"
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_end

            end
