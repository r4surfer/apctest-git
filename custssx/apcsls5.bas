        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCSLS5                              *~
            *  Creation Date     - 07/27/95                             *~
            *  Last Modified Date- 08/08/00                             *~
            *  Description       - Subroutine to Create Special Sales   *~
            *                      Report for Home Centers HQ and Lowes *~
            *                                                           *~
            *  Code Tables Used  - (SLS CODE4) - Table contains the     *~
            *                                  Sales Report Card Codes. *~
            *                      (SLS CODE5) - The Descriptions Assoc.*~
            *                                  with codes in (SLS CODE4)*~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/27/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 09/24/96 ! Mod to Correct Prob with Store (HQ8361)  ! RHH *~
            *          !   Corr. mad to 'UPDATE_CUSTOMER'         !     *~
            * 11/11/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            *          !                                          !     *~
            * 06/05/00 ! Mod to use linecom for amount instead of ! CMG *~
            *          !     of calculating. (EWD001)             !     *~
            * 07/05/00 ! Mod to use EWDSLSDT instead of APCSLSDT  ! CMG *~
            *          !     (EWD002)                             !     *~
            * 08/08/00 ! Mod to take out error code 8 (EWD003)    ! CMG *~     
            * 12/20/18 ! CR1828 convertsion of EWDSLSDT for Dallas! DES *~     
            *************************************************************

        sub "APCSLS5" (#1,#4,b_mnth$,e_mnth$,b_year$,e_year$,b_mlyr$,    ~
                       e_mlyr$,b_lyr$,e_lyr$,per%,rpt_value$,rpt_sum$)
        dim                                                              ~
            b_lyr$6,                     /* START DATE LAST YEAR       */~
            b_mlyr$6,                    /* START DATE THIS MNTH LST YR*/~
            b_mnth$6,                    /* START DATE THIS MONTH      */~
            b_year$6,                    /* START DATE THIS YEAR       */~
            city$18, st$2,               /* Customer City and State    */~
            cc$24,code$3,                /* Use to Check for Aluminum  */~
            cust_code$9, cust_save$9, cs$2,  /* Customer Code          */~
            date$8,                      /* Date for screen display    */~
            dollars$12,                  /* Year to Date Dollar Amount */~
            e_lyr$6,                     /* END DTE LAST YEAR          */~
            e_mlyr$6,                    /* END DTE THIS MNTH LAST YEAR*/~
            e_mnth$6,                    /* END DTE THIS MONTH         */~
            e_year$6,                    /* END DATE FOR THIS  YEAR    */~
            name$15,                     /* Salesman Name              */~
            postdate$6,                  /* INVOICE POSTING DATE       */~
            sls_rec$50, sls_key$25,      /* Work File                  */~
            readkey$50,                  /* GENERIC KEY                */~
            rpt_sum$1,                   /* (S)UMMARY OR (D)ETAIL      */~
            rptid$4,                     /* Print Queue ID             */~
            rptitle$50,                  /* Report Title               */~
            rpt_value$9,                 /* SPECIFIED SALESMAN OR ALL  */~
            salesman$4, sls_save$4,      /* Salesman Code              */~
            time$8,                      /* System Time                */~
            gl_acct$9,                   /* Sales Account Number       */~
/*(EWD003)*/err_code$2                   /* Error Code From EWDSLS00   */

        dim f2%(10%),                    /* = 0 if the file is open    */~
                                         /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Sales Analysis by Home Center     "
            pname$ = "APCSLS5  - Rev: R6.04"


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! EWDSLSDT ! APC Sales Analysis Detail File           *~
            * #4  ! GENCODES ! General Systems Code Tables              *~
            * #5  ! APCSLSWK ! Sort Work File for Report                *~
            * #6  ! CUSTOMER ! Customer Master File                     *~
            * #7  ! SLMMASTR ! Salesman master file                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5,  "APCSLSWK",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  23,                     ~
                        alt key 1, keypos =  15, keylen = 9

            select #6,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #7,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4


            call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))

            mode$ = "OUTPT" : call "WORKOPN2" (#5,mode$, 500%, f2%)
            if f2% <> 0% then goto L01100
            mode$ = "SHARE" : call "WORKOPN2" (#5,mode$, 500%, f2%)
            if f2% <> 0% then goto L01100
         goto mainline
L01100:     call "SHOSTAT" ("Error - Cannot Open (APCSLSWK)") : stop
            goto rpt_exit

        mainline
        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

            call "SHOSTAT" ("Sorting Data for S.A. Report")
            init (" ") postdate$,readkey$, time$, date$, salesman$,      ~
                       cust_code$, cust_save$, cs$, dollars$, sls_save$, ~
                       city$, st$, name$
        REM - Mainline
            gosub create_data
            gosub rpt_1
            gosub rpt_2
            gosub rpt_3
            gosub rpt_4
            goto rpt_exit


        create_data
            init(" ") readkey$
            str(readkey$,1%,2%) = "HQ"
            if str(rpt_value$,1%,3%) = "ALL" then goto L01410
               str(readkey$,1%,2%) = str(rpt_value$,1%,2%)
               call "SHOSTAT" ("Processing Home Center ("&               ~
                                str(readkey$,1%,2%) & ")")
/* (EWD001) - Begin */
REM L01410    read #1,key 2% > readkey$,using L01500 ,postdate$, invdisc,    ~
REM                              linedisc, lineext, readkey$,                ~
REM                                                     eod goto create_done

L01410:   read #1,key 2% > readkey$,using L01500,postdate$,err_code$,gl_acct$, ~
                                    readkey$,  linecom, eod goto create_done
                                                              /*  (EWD003)  */
                                      
            cust_save$ = str(readkey$,1%,9%)      /* Customer Code     */
            sls_save$  = str(readkey$,10%,4%)     /* Salesman          */
            goto L01520
        read_loop
           read #1,using L01500, postdate$,err_code$,gl_acct$,readkey$,linecom,~
                               eod goto create_done
                                                               /*  (EWD003)  */

REM          FMT CH(6),POS(54),CH(02),CH(9),POS(85),CH(36),POS(195),PD(14,4)
L01500:      FMT CH(6),POS(57),CH(02),CH(9),POS(88),CH(36),POS(198),PD(14,4)

L01520:     if str(gl_acct$,1%,8%) = "3650-313" then goto read_loop
            if str(gl_acct$,1%,2%) <> "36" then goto read_loop
                                                  /* (EWD002)          */
            if str(err_code$,1%,2%) = "08" then goto read_loop  /*  (EWD003)  */                                                  
            cs$ = str(readkey$,1%,2%)             /* 1ST TWO DIGITS    */
            if cs$ <> "HQ" then goto L01560
               gosub check_product
               if check% = 0% then goto read_loop
L01560:     if cs$ > "LO" then goto create_done
            if cs$ <> "HQ" and cs$ <> "LO" then goto read_loop

               cust_code$ = str(readkey$,1%,9%)   /* Customer Code     */
               salesman$  = str(readkey$,10%,4%)  /* Salesman          */
               if str(rpt_value$,1%,3%) = "ALL" then goto L01650
                  if cs$ <> str(rpt_value$,1%,2%) then goto create_done
                                     /* Re-Calc Net Invoice Amount     */
                                     /* Price After Line Item Discount */
REM L01650         amount     = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                     /* Price After Order Discount     */
REM                amount     = round(amount * (1.0 - (invdisc/100.0)), 2)

L01650:            amount     = round(linecom, 2)
/* (EWD001) - End */

            if cust_save$ = cust_code$ then goto L01740
               gosub update_customer          /* Write Customer Totals */
               cust_save$ = cust_code$
               sls_save$  = salesman$

L01740:     if postdate$ < b_year$ then last_year
            if postdate$ > e_year$ then read_loop
            if postdate$ > e_mnth$ then read_loop
               ytd = round( ytd + amount, 2)
               if postdate$ < b_mnth$ then goto read_loop
               mtd = round( mtd + amount, 2)
               goto read_loop
        last_year
            if postdate$ < b_lyr$ then goto read_loop
                  lyear = round( lyear + amount, 2)
               if postdate$ > e_mlyr$ then goto read_loop
                  lytd  = round( lytd + amount, 2)
               if postdate$ < b_mlyr$ then goto read_loop
                  lmtd  = round( lmtd + amount, 2)
        goto read_loop

        create_done               /* Report Ending Routine */
            gosub update_customer
        return

        check_product                    /* Lookup Sort Code in Table  */
            init(" ") cc$, code$         /* Aluminum for HQ Only       */
            check% = 0%
            str(cc$,1%,9%)   = "SLS CODE4"
            str(cc$,10%,15%) = str(readkey$,23%,3%)  /* Sort Code      */
            read #4,key = cc$, using L02000, code$, eod goto L02030
L02000:         FMT POS(25), CH(3)
            if code$ = "01I" then return              /* Skip Aluminum */
               check% = 1%
L02030: return

        update_customer
           x = mtd + lmtd + ytd + lytd
           if x = 0.0 then return
              call "SHOSTAT" ("Updating Home Center ("&cust_save$&")")
           init(" ") sls_rec$, dollars$
           x = round(90000000.00 - ytd, 2)              /* Largest 1st */
           convert x to dollars$, pic(########.##-)
                                                       /* 09-24-96 Fix */
           str(sls_rec$,1%,2%)  = str(cust_save$,1%,2%)  /* HQ or LO   */
           str(sls_rec$,3%,12%) = dollars$               /* YTD Total  */
           str(sls_rec$,15%,9%) = cust_save$             /* Customer   */
           str(sls_rec$,24%,4%) = sls_save$              /* Salesman   */

           write #5,using L02200 , sls_rec$,mtd, lmtd, ytd, lytd, lyear,   ~
                                                     " ", eod goto L02230
L02200:        FMT CH(50), 5*PD(14,4), CH(10)
           mtd, lmtd, ytd, lytd, lyear = 0.0
        return
L02230:    call "SHOSTAT" ("Error Dup Customer ("&cust_save$&")") : stop
           mtd, lmtd, ytd, lytd, lyear = 0.0
        return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L02340: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

        %!                                                               ~
        ~                                                                !

L02400: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L02430: %!Run ######## @ ########          ##############################~
        ~################                          Period: ##   Page: ## !


L02470: %!  Store    ! <--- Location ------>!Salesman Name  !Current M-T-D!~
        ~ Last M-T-D !Var +/- %!Current  Y-T-D! Last  Y-T-D  !Var +/- %!

        %!            !                     !               !             !~
        ~            !         !              !              !         !

M
L02530: %! ######### !##################, ##!###############! $###,###.##-!~
        ~$###,###.##-! ###.##- !$#,###,###.##-!$#,###,###.##-! ###.##- !

 
L02560: %!-----------!----------------------!---------------!-------------!~
        ~------------!---------!--------------!--------------!---------!

        select_printer
            select printer(134)
            init(" ") date$, time$
            call "TIME" (time$)
            date$ = date : call "DATEFMT" (date$)
            call "SETPRNT" (" ",rptid$, 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
        return
        close_printer
            print using L02340
            call "SETPRNT" (" ",rptid$, 0%, 1%)
            close printer
        return

        page_head              /* Page Heading Print Routine */
            if lcntr% <> 99% then print using L02340
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L02340
            print using L02430 , date$, time$, rptitle$, per%, pcntr%
            print using L02400
            print using L02470
            lcntr% = 4%
        return

        prt_dtl
            if lcntr% > 57% then gosub page_head
               print using L02560
               print using L02530, str(cust_code$,1%,9%), city$, st$,      ~
                                 name$, mtd, lmtd, mvar, ytd, lytd, yvar
               lcntr% = lcntr% + 2%
        return

        lookup_customer
           cust_code$ = str(sls_rec$,15%,9%)
           read #6,key = cust_code$, using L02960 , city$, st$,            ~
                                                  eod goto L02970
L02960:       FMT POS(403), CH(18), CH(2)
L02970: return

        lookup_salesman
           salesman$ = str(sls_rec$,24%,4%)
           read #7,key = salesman$, using L03020 , name$, eod goto L03030
L03020:       FMT POS(5), CH(15)
L03030: return

        rpt_1
           rpt% = 1%
           rptid$ = "SLS1"
           gosub select_printer
           rptitle$ = "Home Quarter Sales(Vinyl): Largest to Smallest"
           call "SHOSTAT" (rptitle$)
           sls_key$ = " "
           str(sls_key$,1%,2%) = "HQ"
           read #5,key > sls_key$, using L03560 , sls_rec$, mtd, lmtd,     ~
                                                ytd, lytd, lyear,        ~
                                                eod goto rpt_done
           goto L03570
        rpt_2
           rpt% = 2%
           rptid$ = "SLS2"
           gosub select_printer
           rptitle$ = "Home Quarter Sales(Vinyl): Store Order        "
           call "SHOSTAT" (rptitle$)
           sls_key$ = " "
           str(sls_key$,1%,2%) = "HQ"
           read #5,key 1% > sls_key$, using L03560 , sls_rec$, mtd, lmtd,  ~
                                                   ytd, lytd, lyear,     ~
                                                   eod goto rpt_done
           goto L03570
        rpt_3
           rpt% = 3%
           rptid$ = "SLS3"
           gosub select_printer
           rptitle$ = "Lowe's Sales      (All)  : Largest to Smallest"
           call "SHOSTAT" (rptitle$)
           sls_key$ = " "
           str(sls_key$,1%,2%) = "LO"
           read #5,key > sls_key$, using L03560 , sls_rec$, mtd, lmtd,     ~
                                                ytd, lytd, lyear,        ~
                                                eod goto rpt_done
           goto L03570
        rpt_4
           rpt% = 4%
           rptid$ = "SLS4"
           gosub select_printer
           rptitle$ = "Lowe's Sales      (All)  : Store Order        "
           call "SHOSTAT" (rptitle$)
           sls_key$ = " "
           str(sls_key$,1%,2%) = "LO"
           read #5,key 1% > sls_key$, using L03560 , sls_rec$, mtd, lmtd,  ~
                                                   ytd, lytd, lyear,     ~
                                                   eod goto rpt_done
           goto L03570
        rpt_nxt
           read #5,using L03560 , sls_rec$, mtd, lmtd, ytd, lytd, lyear,   ~
                                                eod goto rpt_done
L03560:       FMT CH(50), 5*PD(14,4)
L03570:    on rpt% goto L03590, L03620, L03650, L03680
                                                /* Largest to Smallest */
L03590:       if str(sls_rec$,1%,2%) <> "HQ" then goto rpt_done          ~
                                             else goto L03700
                                                /* Store Order         */
L03620:       if str(sls_rec$,15%,2%) <> "HQ" then goto rpt_done         ~
                                              else goto L03700
                                                /* Largest to Smallest */
L03650:       if str(sls_rec$,1%,2%) <> "LO" then goto rpt_done          ~
                                             else goto L03700
                                                /* Store Order         */
L03680:       if str(sls_rec$,15%,2%) <> "LO" then goto rpt_done

L03700:       mvar = round( ((mtd / lmtd) - 1.0) * 100.0 , 2)
              yvar = round( ((ytd / lytd) - 1.0) * 100.0 , 2)
              gosub lookup_customer
              gosub lookup_salesman
              gosub prt_dtl
              goto rpt_nxt
        rpt_done
           gosub close_printer
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        rpt_exit
            call "FILEBGON" (#5)
        end