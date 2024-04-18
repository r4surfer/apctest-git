        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR    QQQ    CCC   H   H  K   K   SSS   BBBB    *~
            *  A   A  R   R  Q   Q  C      H   H  K  K   S      B   B   *~
            *  AAAAA  RRRR   Q   Q  C      HHHHH  KK      SSS   BBBB    *~
            *  A   A  R   R  Q Q Q  C      H   H  K  K       S  B   B   *~
            *  A   A  R   R   QQQ    CCC   H   H  K   K   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARQCHKSB - Displays and prints data regarding invoices.   *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/30/86 ! Original                                 ! ERN *~
            * 08/23/89 ! Added capability to toggle between STAT  ! MLJ *~
            *          !  TRAN values when Multi-Currency in use. !     *~
            * 11/10/89 ! Added currency toggle to check detail.   ! MLJ *~
            * 02/15/90 ! Added rounding of check amts before dsply! JDH *~
            * 01/20/93 ! PRR 12758 Corrected PF5 next screen logic! JDH *~
            * 07/11/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARQCHKSB" (billto$,         /* Bill-to Customer for disp  */~
                        asofu$,          /* Display As Of date- unfmtd */~
                        #3,              /* CUSTOMER channel           */~
                        #7, #8,          /* CRCMASTR & CRCLINES        */~
                        ret%,            /* In/Out Status Flag         */~
                        chknr$,          /* Check for Display          */~
                        curr$,           /* Milti-Currency Usage Flag  */~
                        stat_curr_code$) /* STATUTORY Currency Code    */~

*        RET% is passed as 0% for Bill-to Level inquiry, 1% for display
*          of a specific check (check specified by CHKNR$).  It is
*          passed back as sent if no errors occurred, else it is
*          returned as 2% (bill-to or check not on file).
*        MULTI-CURRENCY:  CURR$ is the multi-currency flag.  "Y" if on,
*           "N" if off.   STAT_CURR_CODE$ is the statutory currency code,
*           all blanks if CURR$ = "N".


        dim                                                              ~
            adjdue$8,                    /* Adjusted Due Date          */~
            asof$10,                     /* As of date                 */~
            banknr$10,                   /* Bank Number                */~
            billto$9, billtoname$30,     /* Bill-to Customer and Name  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cash_acct$12,                /* Cash-in-Bank Account       */~
            chkamt_ld$(14)10,            /* Check Amt - Lines Display  */~
            chkdisca_ld$(14)10,          /* Check Discount Allowed     */~
            chkdiscu_ld$(14)10,          /* Check Discount Unallowed   */~
            chkdate$8,                   /* Check Date                 */~
            chkhdr$79,                   /* Check Header Display       */~
            chknr$8,                     /* Check Number               */~
            chk_range$(4)8,              /* Check Range for print      */~
            ckdisp$(14)79,               /* Check Display Line         */~
            company$60,                  /* Company Name               */~
            cr_acct$12,                  /* Credit Account             */~
            curr$1,                      /* Multi-Currency Usage Flag  */~
            currkey$17,                  /* CRCMSCUR Read Key          */~
            currmst$4,                   /* CURMASTR Read Key          */~
            cursor%(2),                  /* Cursor location for edit   */~
            curr_lit$37,                 /* STATUTORY Report Literal   */~
            date$8,                      /* Date for screen display    */~
            dfac$(30)1,                  /* Display FACs               */~
            disca_acct$12,               /* Allowed Discs Account      */~
            discu_acct$12,               /* Unallwd Discs Account      */~
            disp$(14)79,                 /* STATUTORY Display Array    */~
            entered$8,                   /* Check Entry Date           */~
            errormsg$79,                 /* Error message              */~
            exchg_lit$9,                 /* Currency Exchange Codes    */~
            exchg_rate$10,               /* Currency Exchange Rate     */~
            findchknr$8,                 /* Find Argument              */~
            from_date$10,from_dateu$6,   /* Print From Date            */~
            hdr1$(7)12, hdr2$(8)10,      /* Screen Column Headings     */~
            hdr3$(9)11,                  /*                            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastchk$8,                   /* Last Check Number          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linekey$50,                  /* Line Plow Key              */~
            lines$(500)79,               /* Line Summary Display       */~
            mcdisp$(14)79,               /* TRANSACTION Summary Array  */~
            mc_display$1,                /* Multi-Currency Display Flag*/~
            mclines$(500)79,             /* TRANSACTION Lines Array    */~
            mchdr$79,                    /* TRANSACTION Check Header   */~
            pf$(3)79, pfkeys$20,         /* PF Literals and Keys       */~
            po$16,                       /* PO Number                  */~
            postdate$8,                  /* Post Date                  */~
            postmark$8,                  /* Post Mark Date             */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            readcrcl$21,                 /* CRCLNCUR Read key          */~
            readkey$50,                  /* Misc. Read Key             */~
            report$30, rptid$6,          /* Report to Print            */~
            rpthdr$80, rpthdr2$80,       /* Report Heading Lines       */~
            rptplow$50,                  /* Plow Key for Report        */~
            runtime$8,                   /* Report Run Time            */~
            save_chknr$8,                /* Save it for later          */~
            seq$4,                       /* Line Item Sequence Number  */~
            srcedoc$8,                   /* Source Doc Number          */~
            stat_curr_code$4,            /* STATUTORY Currency Code    */~
            stlines$(500)79,             /* STATUTORY Lines Array      */~
            sthdr$79,                    /* STATUTORY Check Header     */~
            stlmnt1$14, stlmnt2$14,      /* Settlement Number          */~
            tran_curr_code$4,            /* TRANSACTION Currency Code  */~
            tran_curr_desc$30,           /* TRANSACTION Currency Desc  */~
            type$4                       /* Line Item Type             */

        dim f2%(44),                     /* = 0 if the file is open    */~
            f1%(44),                     /* = 1 if READ was successful */~
            fs%(44),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(44)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! CUSTOMER ! Customer Master File (passed in)         *~
            * # 4 ! GLMAIN   ! General Ledger Chart Of Accounts File.   *~
            * # 7 ! CRCMASTR ! Check Master - Headers                   *~
            * # 8 ! CRCLINES ! Check Master- Line Items                 *~
            * #42 ! CRCLNCUR ! Shadow - Line Items                      *~
            * #43 ! CURMASTR ! Multi-Currency Master File               *~
            * #44 ! CRCMSCUR ! Shadow - Check Master                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 4, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select #42, "CRCLNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  21,                     ~
                        alt key 1,  keypos =  1,  keylen =  25

            select #43, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   4                      ~

            select #44, "CRCMSCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  17,                     ~
                        alt key 1,  keypos =  1,  keylen =  21

            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))

            if curr$ = "N" then goto L09060
                call "OPENCHCK" (#42, fs%(42), f2%(42), 0%, rslt$(42))
                call "OPENCHCK" (#43, fs%(43), f2%(43), 0%, rslt$(43))
                call "OPENCHCK" (#44, fs%(44), f2%(44), 0%, rslt$(44))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

L09060
*        Perform start-up tasks required regardless of tasks
            blankdate$ = " "
            call "DATUFMTC" (blankdate$)
            date$ = date : call "DATEFMT" (date$)
            asof$ = asofu$  :  call "DATFMTC" (asof$)
            curr_lit$ = "ALL AMOUNTS ARE IN STATUTORY CURRENCY"
            call "DESCRIBE" (#3, billto$, billtoname$, 0%, f1%(3))
            if f1%(3) = 1% then L09120
                ret% = 2%  :  goto exit_program
L09120:     str(line2$,62%) = "ARQCHKSB: " & str(cms2v$,,8%)

*        If a specific check has been requested, handle it here
            if ret% <> 1% then L09270
                gosub load_check
                if chkonfile% = 1% then L09230
                     ret% = 2%
                     call "ASKUSER" (2%, "CHECK DISPLAY",                ~
                               "The Check requested is not on file.",    ~
                               " ", "Press any PF Key to Continue...")
                     goto exit_program
L09230:         gosub display_check
                goto  exit_program


L09270:     goto first_screen

        REM *************************************************************~
            *                  M A I N   S C R E E N                    *~
            *-----------------------------------------------------------*~
            * Handles Check Summary Listing Screen.                     *~
            *************************************************************

        main_screen
            ret% = 0%
            gosub'101(0%)
            errormsg$ = " "
                if keyhit%  =  0% then show_check
                if keyhit%  =  2% then first_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  =  8% then find_check
                if keyhit%  = 12% then print_listing
                if keyhit%  = 14% then print_checks
                if keyhit%  = 16% then exit_program
                if keyhit%  = 32% then exit_program
                goto main_screen

        first_screen
            lastchk$ = hex(00)
            first%   = 1%
            goto L10290
        next_screen
            first% = 0%
L10290:     gosub load_summary
            goto  main_screen

        find_check
            findchknr$ = " "
            gosub'101(1%)
                errormsg$ = " "
                if keyhit%  =  1% then main_screen
                     lastchk$ = findchknr$ addc all(hex(ff))
                     findchknr$ = " "
                     goto next_screen


        REM *************************************************************~
            *             R E P O R T    P R I N T I N G                *~
            *-----------------------------------------------------------*~
            * Get Report Parameters for both Listing and Detail.        *~
            *************************************************************

        print_checks:  report$ = "CHECK DETAIL"  : rptid$  = "ARM012"
                       goto L11200

        print_listing: report$ = "CHECK LISTING" : rptid$  = "ARM011"

L11200:     chk_range$(1) = "ALL" : chk_range$(2) = " " : gosub'152(1%)
            from_date$ = "19010101"
            call "DATEOKC" (from_date$, 0%, " ")
            from_dateu$ = blankdate$

L11280:     inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            lastfieldnr% = 0%
            gosub'102(0%)
                  if keyhit%  =  1% then main_screen
                  if keyhit%  = 16% then print_report
                  if keyhit%  = 32% then exit_program
                  if keyhit% <>  0% then L11280
L11360:     fieldnr% = cursor%(1) - 5
                if fieldnr% < 1% or fieldnr% > 2% then L11280
                if fieldnr%  = lastfieldnr% then L11280
            gosub'052(fieldnr%)
L11400:     gosub'102(fieldnr%)
                  if keyhit%  =  1 then main_screen
                  if keyhit% <>  0 then L11400
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11400
                     lastfieldnr% = fieldnr%
                     goto L11360

        print_report
            gosub report_control
            goto  main_screen


        REM *************************************************************~
            *               C H E C K    D I S P L A Y                  *~
            *-----------------------------------------------------------*~
            * Display Check Detail.                                     *~
            *************************************************************

        show_check     /* Check Detail display from Main Screen        */
            i% = cursor%(1) - 5%
            if i% < 1% or i% > disp% then main_screen
            chknr$  = str(disp$(i%),,8)
            gosub load_check
            if chkonfile% = 0% then main_screen
                gosub display_check
                goto main_screen


        display_check
            inpmessage$, errormsg$ = " "
            str(line2$,,61%) = "Bill-to: " & billto$ & "  " &            ~
                                             billtoname$
            t% = 0%

        check_detail
            inpmessage$ = " "
            gosub'120
              inpmessage$ = " "
              if keyhit% = 16 then       return
              if keyhit% =  2 then t% = 0%
              if keyhit% =  3 then t% = maxlines%-12%
              if keyhit% =  4 then t% = t% - 12%
              if keyhit% =  5 then t% = min(t%+12%, maxlines%-12%)
              if keyhit% =  6 then t% = t% - 1%
              if keyhit% =  7 then t% = min(t%+1%, maxlines%-12%)
                                   t% = max(0, t%)
              if keyhit% = 14 then gosub print_check
              if keyhit% = 32 then       return
              if keyhit% >  0 then check_detail

        print_check:
            report$ = "CHECK DETAIL"
            rptid$  = "ARM012"
            save_chknr$  = chknr$
            chk_range$(1) = chknr$ : chk_range$(2) = " " : gosub'152(1%)
            from_date$  = "19010101"
            call "DATECONV" (from_date$)
            from_dateu$ = from_date$
            call "DATFMTC" (from_date$)
            gosub report_control
            chknr$  = save_chknr$
            return


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Prints Listing of Checks per Selection Criteria Entered.  *~
            *************************************************************
        report_control:
            page%, chkcount% = 0%  :  line% = 857%
            total_disca, total_discu, total_net = 0
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            call "SHOSTAT"  ("Printing " & report$)
            rptplow$ = str(billto$,,9) & chk_range$(3)
            rpthdr$   = report$ & " for Bill-to " & billto$ & "  " &     ~
                                                    billtoname$
            rpthdr2$ = "For All Checks"
            if chk_range$(1) <> "ALL" then                               ~
                rpthdr2$ = "For Check Numbers: " &                       ~
                           chk_range$(1) & "  to " & chk_range$(2)
            rpthdr2$ = rpthdr2$ & ".  For Post Dates " & from_date$ &    ~
                                  " to " & asof$
            call "STRING" addr("CT", rpthdr$ , 80%)
            call "STRING" addr("CT", rpthdr2$, 80%)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            select printer (134)
            report%   = 1%
            if rptid$ = "ARM012" then report% = 2%
            gosub check_loop
            close printer  :  select ws
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if line% <> 857% then return
                call "ASKUSER" (2%, report$,                             ~
                          "No checks were found within the criteria",    ~
                          "specified.  Press any PF Key to Continue...", ~
                          " ")
                return


        check_loop
            call "PLOWNEXT" (#7, rptplow$, 9%, f1%(7))
            if f1%(7) = 0% then end_report

            get #7 using L14450, chknr$, chkdate$, banknr$, postmark$,    ~
                                chkamt, disca, discu, cash_acct$,        ~
                                disca_acct$, discu_acct$, entered$,      ~
                                postdate$
L14450:         FMT XX(9), CH(8), CH(6), CH(10), CH(6), 3*PD(14,4),      ~
                    XX(8), 3*CH(9), XX(3), 2*CH(6)

            chkamt = round(chkamt, 2)
            disca  = round(disca,  2)
            discu  = round(discu,  2)

            if chknr$ > chk_range$(4) then end_report
            if postdate$ < from_dateu$ or postdate$ > asofu$             ~
                                                         then check_loop

            call "DATEFMT" (chkdate$ )
            call "DATEFMT" (postdate$)
            call "DATEFMT" (postmark$)
            call "DATEFMT" (entered$ )
            call "GLFMT"   (disca_acct$)
            call "GLFMT"   (discu_acct$)
            call "GLFMT"   (cash_acct$ )
            total_disca = total_disca + disca
            total_discu = total_discu + discu
            total_net   = total_net   + chkamt
            if line%    > 55% then gosub page_heading
            if report%  = 2% then gosub check_heading
            print using L15610, chknr$, chkdate$, postmark$, postdate$,   ~
                               entered$, banknr$, disca, discu, chkamt,  ~
                               disca_acct$, discu_acct$, cash_acct$
            if curr$ = "N" or report% = 1% then L14660
                currkey$ = str(billto$) & str(chknr$)
                call "READ100" (#44, currkey$, f1%(44))
                if f1%(44) = 0% then L14643
                    get #44 using L14640, tran_curr_code$, exchg_rate
L14640:                FMT CH(4), POS(36), PD(14,7)
                    call "CONVERT" (exchg_rate, 2.7, exchg_rate$)
                    goto L14645
L14643:         exchg_rate$ = "      1.00"
                tran_curr_code$ = str(stat_curr_code$)
L14645:         currmst$ = str(tran_curr_code$)
                call "READ100" (#43, currmst$, f1%(43))
                if f1%(43) = 0% then L14660
                    get #43 using L14649, tran_curr_desc$
L14649:                FMT POS(5), CH(30)
                exchg_lit$ = str(tran_curr_code$) & "/" &                ~
        str(stat_curr_code$)
                print using L15602, tran_curr_desc$,exchg_rate$,exchg_lit$
                line% = line% + 1%
L14660:         line% = line% + 1%
            chkcount%  = chkcount% + 1%
            if report% = 1% then goto check_loop

            print  :  line% = line% + 1%
            gosub line_heading
            linekey$ = str(billto$) & str(chknr$) & hex(00)

          chk_line_loop
            call "PLOWNEXT" (#8, linekey$, 17%, f1%(8))
            if f1%(8) = 1% then L14810
                print : print using L15810
                line% = line% + 2%
                goto check_loop

L14810:     get #8 using L14840, seq$, type$, stlmnt1$, stlmnt2$,         ~
                                srcedoc$, po$, cashpct, adjdue$,         ~
                                net, disca, discu, cr_acct$
L14840:         FMT XX(17), CH(4), CH(1), 2*CH(12), CH(8), XX(2), CH(16),~
                    PD(14,4), CH(6), XX(6), 3*PD(14,4), CH(9)

            net   = round(net,   2)
            disca = round(disca, 2)
            discu = round(discu, 2)

            if type$ = "P" then type$ = "Pay "
            if type$ = "A" then type$ = "Adj "
            if type$ = "B" then type$ = "BF  "
            if type$ = "U" then type$ = "Unap"
            if type$ = "G" then type$ = "G/L "
            if type$ = "S" then type$ = "Sale"
            if type$ = "D" then type$ = "Dist"
            if stlmnt1$ <> " " then stlmnt1$ = str(stlmnt1$,,8) & "-" &  ~
                              str(stlmnt1$,9,2) & "-" & str(stlmnt1$,11)
            if stlmnt2$ <> " " then stlmnt2$ = str(stlmnt2$,,8) & "-" &  ~
                                               str(stlmnt2$,9,2) & "-00"
            call "DATEFMT" (adjdue$)
            call "GLFMT"   (cr_acct$)

            if line% > 52% then gosub line_heading
            print using L15780, seq$, type$, stlmnt1$, stlmnt2$, srcedoc$,~
                               po$, adjdue$, cashpct, disca, discu, net, ~
                               cr_acct$
            line% = line% + 1%
            goto chk_line_loop


        end_report
            if line% = 857% then return
                if report% = 2% then L15140
                     print using L15640
                     print using L15660, chkcount%, total_disca,          ~
                                        total_discu, total_net
L15140:         print "**END OF REPORT**"
                return

        line_heading
            if line% > 53% then gosub page_heading
            print using L15690
            print using L15720
            print using L15750
            line% = line% + 3%
            return

        check_heading
            if line% > 53% then gosub page_heading
            print using L15520
            print using L15550
            print using L15580
            line% = line% + 3%
            return

        page_heading
            page% = page% + 1%  :  line% = 4%
            print page
            print using L15430, date$, runtime$, company$, rptid$
            print using L15460, asof$, rpthdr$, page%
            print using L15490, rpthdr2$
            if curr$ = "N" then L15385
               print using L15505, curr_lit$ :  line% = line% + 1%
L15385:     print
            if report% = 1% then gosub check_heading
            return


L15430: %RUN DATE: ######## ########               ######################~
        ~######################################               ARQCHKSB:###~
        ~###
L15460: %   AS OF: ##########           #################################~
        ~###############################################          PAGE: ##~
        ~###
L15490: %                               #################################~
        ~###############################################
L15505: %                                                     ###########~
        ~###############################################

L15520: %          CHECK     POST     DATE     DATE                 ALLOW~
        ~ED    UNALLOWED               ---------  G/L DISTRIBUTION  ------~
        ~---
L15550: %CHECK NO   DATE    MARKED   POSTED   ENTERED BANK NO.     DISCOU~
        ~NTS   DISCOUNTS  CHECK AMOUNT ALLOWED DISC  UNALWD DISC CASH-IN-B~
        ~ANK
L15580: %-------- -------- -------- -------- -------- ---------- --------~
        ~--- ----------- ------------- ------------ ------------ ---------~
        ~---
L15602: %         TRANSACTION CHECK CURRENCY  ###########################~
        ~###  EXCHANGE RATE ########## #########
L15610: %######## ######## ######## ######## ######## ########## -#######~
        ~.## -#######.## -#########.## ############ ############ #########~
        ~###
L15640: %                                                        --------~
        ~--- ----------- -------------
L15660: %                          REPORT TOTALS (#### CHECKS)   -#######~
        ~.## -#######.## -#########.##

L15690: %                            ----------------  A P P L I E D   T ~
        ~O  -----------------   ALLOWED   UNALLOWED

L15720: %   LINE TYPE SETTLEMENT NO. SETTLEMENT NO. SRCE DOC CUSTOMER PO ~
        ~      ADJ DUE  DISC%  DISCOUNTS  DISCOUNTS      PAYMENT  G/L ACCO~
        ~UNT
L15750: %   ---- ---- -------------- -------------- -------- ------------~
        ~---- -------- ------ ----------- ----------- ---------- ---------~
        ~---
L15780: %   #### #### ############## ############## ######## ############~
        ~#### ######## ###### -#######.## -#######.## -######.## #########~
        ~###
L15810: %================================================================~
        ~=================================================================~
        ~===



        REM *************************************************************~
            *                 D E F A U L T S                           *~
            *-----------------------------------------------------------*~
            * Set Input Messages for Report Screen.                     *~
            *************************************************************

        deffn'052(fieldnr%)
            on fieldnr% gosub L21100, L21130
            return

L21100:     inpmessage$ = "Enter Check Number Range to Print."
            return

L21130:     inpmessage$ = "Enter Earliest Post Date to Print."
            return

        REM *************************************************************~
            *                L O A D   S U M M A R Y                    *~
            *-----------------------------------------------------------*~
            * Loads data for summary screen display.                    *~
            *************************************************************
        load_summary
            init(" ") disp$(), mcdisp$(), chkamt_ld$()
            disp% = 0%
            plowkey$ = str(billto$,,9) & lastchk$

        load_loop
            call "PLOWNEXT" (#7, plowkey$, 9%, eof%)
            if eof% = 1% then L30160
                if disp% < 14% then disp$(disp%+1%) = "*EOF*"
                if disp% < 14% then mcdisp$(disp%+1%) = "*EOF*"
                return

L30160:     get #7 using L30180, chknr$, chkdate$, banknr$, postmark$,    ~
                                chkamt, disca, discu, postdate$
L30180:         FMT XX(9), CH(8), CH(6), CH(10), CH(6), 3*PD(14,4),      ~
                    POS(108), CH(6)
            if postdate$ > asofu$ then load_loop

            disp% = disp% + 1%  :  lastchk$ = chknr$
            call "DATEFMT" (chkdate$ )
            call "DATEFMT" (postdate$)
            call "DATEFMT" (postmark$)
            disp$(disp%) = str(chknr$,,8) & "  " & str(chkdate$) & "  " &~
                           str(postmark$,,8) & " " & str(postdate$)
            if disca <> 0 then                                           ~
            convert round(disca,2) to str(disp$(disp%),39,13),           ~
                                                       pic(-#,###,###.##)
            if discu <> 0 then                                           ~
            convert round(discu,2) to str(disp$(disp%),53,13),           ~
                                                       pic(-#,###,###.##)
            convert round(chkamt,2) to str(disp$(disp%),67,13),          ~
                                                       pic(-#,###,###.##)
            if curr$ = "N" then L30350
                mcdisp$(disp%) = disp$(disp%)
                str(currkey$) = str(billto$) & str(chknr$)
                call "READ100" (#44, currkey$, f1%(44))
                if f1%(44) <> 0% then L30334
                    str(mcdisp$(disp%),21,8) = stat_curr_code$
                    convert chkamt to chkamt_ld$(disp%), pic(#######.##)
                    goto L30350
L30334:         get #44 using L30337, str(mcdisp$(disp%),21,8), mcamt,    ~
                                     mcdisca, mcdiscu
L30337:             FMT CH(4), POS(44), PD(14,4), PD(14,4), PD(14,4)
                    convert mcamt to str(mcdisp$(disp%),67,13),          ~
                                                      pic(-#,###,###.##)
                    convert mcamt to chkamt_ld$(disp%), pic(#######.##)
                if mcdisca <> 0% then L30342 else L30345
L30342:             convert mcdisca to str(mcdisp$(disp%),39,13),        ~
                                                      pic(-#,###,###.##)
                   convert mcdisca to chkdisca_ld$(disp%),pic(-######.##)
L30345:         if mcdiscu <> 0% then L30347 else L30350
L30347:             convert mcdiscu to str(mcdisp$(disp%),53,13),        ~
                                                      pic(-#,###,###.##)
                   convert mcdiscu to chkdiscu_ld$(disp%),pic(-######.##)
L30350:     if disp% < 14% then load_loop else return

        REM *************************************************************~
            *                L O A D   C H E C K                        *~
            *-----------------------------------------------------------*~
            * Loads a specific check for display.                       *~
            *************************************************************
        load_check
            readkey$ = str(billto$) & chknr$
            call "READ100" (#7, readkey$, chkonfile%)
            if chkonfile% = 0% then return

            print at(03,02), "Loading Check..."
            get #7 using L31076, chknr$, chkdate$, banknr$, postmark$,    ~
                                chkamt, disca, discu, postdate$
L31076:         FMT XX(9), CH(8), CH(6), CH(10), CH(6), 3*PD(14,4),      ~
                    POS(108), CH(6)
            call "DATEFMT" (chkdate$ )
            call "DATEFMT" (postmark$)
            call "DATEFMT" (postdate$)
            sthdr$  = str(chknr$,,8) & " " & str(chkdate$)  & " " &      ~
                      str(postmark$) & " " & str(postdate$) & " " &      ~
                      banknr$
            call "CONVERT" (disca  , 2.2, str(sthdr$,48,10))
            call "CONVERT" (discu  , 2.2, str(sthdr$,59,10))
            call "CONVERT" (chkamt , 2.2, str(sthdr$,70,10))

*        Check for multi-currency usage. If request is from Trial Balance
*          TRAN curr header info not yet available, must read to get

            if curr$ = "N" then L31250
                mchdr$ = sthdr$
                str(mchdr$,19,8) = str(mcdisp$(i%),21,8)
                if str(mchdr$,19,8) = " " then L31130
                    str(mchdr$,70,10) = str(chkamt_ld$(i%))
                    str(mchdr$,48,10) = str(chkdisca_ld$(i%))
                    str(mchdr$,59,10) = str(chkdiscu_ld$(i%))
                    goto L31250
L31130:         currkey$ = str(billto$) & str(chknr$)
                call "READ100" (#44, currkey$, f1%(44))
                if f1%(44) = 1% then L31150
                    str(mchdr$,19,8)  = stat_curr_code$
                    str(mchdr$,70,10) = str(sthdr$,70,10)
                    goto L31250
L31150:         get #44 using L31152, str(mchdr$,19,8), tr_chkamt,        ~
                                     mcdisca, mcdiscu
L31152:             FMT CH(4), POS(44), PD(14,4), PD(14,4), PD(14,4)
                call "CONVERT" (tr_chkamt, 2.2, str(mchdr$,70,10))
                call "CONVERT" (mcdisca, 2.2, str(mchdr$, 48,10))
                call "CONVERT" (mcdiscu, 2.2, str(mchdr$, 59,10))

L31250
*        Now load up line items
            maxlines%, c% = 0%
            init(" ") stlines$(), mclines$()
            readkey$ = str(billto$) & str(chknr$) & hex(00)
                if curr$ = "Y" then readcrcl$ = readkey$
L31290:     call "PLOWNEXT" (#8, readkey$, 17%, f1%(8))
            if f1%(8) = 0% then return
                maxlines%, c% = maxlines% + 1%
                get #8 using L31390, str(stlines$(c%), 1, 4), /* Line#  */~
                                    str(stlines$(c%), 6, 4), /* Type   */~
                                    str(stlines$(c%),11,11), /* Stlmnt */~
                                    str(stlines$(c%),23, 8), /* Srce D */~
                                    cashpct,                             ~
                                    str(stlines$(c%),32, 8), /* Disc D */~
                                    net, disca, discu
L31390:              FMT POS(18), CH(4), CH(1), XX(12), CH(12), CH(8),   ~
                          XX(18), PD(14,4), CH(6), POS(93), 3*PD(14,4)
                on pos("PABUGSD" = str(stlines$(c%),6,4)) gosub L31440,   ~
                           L31450, L31460, L31470, L31480, L31490, L31500
                goto L31510
L31440:              str(stlines$(c%),6,4) = "Pay "  :  return
L31450:              str(stlines$(c%),6,4) = "Adj "  :  return
L31460:              str(stlines$(c%),6,4) = "BF  "  :  return
L31470:              str(stlines$(c%),6,4) = "Unap"  :  return
L31480:              str(stlines$(c%),6,4) = "G/L "  :  return
L31490:              str(stlines$(c%),6,4) = "Sale"  :  return
L31500:              str(stlines$(c%),6,4) = "Dist"  :  return
L31510:         if str(stlines$(c%),11,11) <> " " then                   ~
                   str(stlines$(c%),11,11) = str(stlines$(c%),11,8) & "-"~
                                           & str(stlines$(c%),19,2)
                call "DATEFMT" (str(stlines$(c%),32,8))
                call "CONVERT" (cashpct, 2.2, str(stlines$(c%),41, 6))
                if disca <> 0 then                                       ~
                   call "CONVERT" (disca  , 2.2, str(stlines$(c%),48,10))
                if discu <> 0 then                                       ~
                   call "CONVERT" (discu  , 2.2, str(stlines$(c%),59,10))
                call "CONVERT" (net    , 2.2, str(stlines$(c%),70,10))
                if curr$ = "N" then L31700
                   mclines$(c%) = stlines$(c%)
                   call "PLOWNEXT" (#42, readcrcl$, 17%, f1%(42))
                   if f1%(42) = 0% then L31690
                     get #42 using L31660, str(mclines$(c%),32,8), mclamt,~
                                          mcldisca, mcldiscu
L31660:                 FMT CH(4), POS(48), PD(14,4), PD(14,4), PD(14,4)
                    call "CONVERT" (mclamt, 2.2, str(mclines$(c%),70,10))
                    if mcldisca <> 0% then L31672 else L31673
L31672:           call "CONVERT" (mcldisca, 2.2, str(mclines$(c%),48,10))
L31673:             if mcldiscu <> 0% then L31674 else L31680
L31674:           call "CONVERT" (mcldiscu, 2.2, str(mclines$(c%),59,10))
L31680:            goto L31700
L31690:         str(mclines$(c%),32,8) = stat_curr_code$
L31700:         goto L31290

        REM *************************************************************~
            *               M A I N   S C R E E N                       *~
            *-----------------------------------------------------------*~
            * Check Summary screen.                                     *~
            *************************************************************

        deffn'101(opt%)
            hdr1$(1) = "Check #"      :  hdr1$(5) = "Allowed Disc"
            hdr1$(2) = "Check Date"   :  hdr1$(6) = "Unallwd Disc"
            hdr1$(3) = "PostMark"     :  hdr1$(7) = "Check Amount"
            hdr1$(4) = " Posted"
            str(line2$,,61%) = "Bill-to: " & billto$ & "  " & billtoname$

            init(hex(86)) dfac$()  :  init(hex(84)) lfac$()
            if opt% = 0% then L40105
                init(hex(8c)) dfac$(), lfac$()
                lfac$(1) = hex(81)
L40105:     gosub setpf1

L40107:     mc_display$ = "N"
*        Check Setup - STATUTORY Currency
            hdr1$(3) = "PostMark"
            mat ckdisp$ = disp$
            goto L40116
L40112
*        Check Setup - TRANSACTION Currency
            hdr1$(3) = "Currency"
            mat ckdisp$ = mcdisp$

L40116:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: CHECK LISTING",            ~
               at (01,62), "As Of:",                                     ~
               at (01,69), fac(hex(8c)), asof$                  , ch(10),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr1$(1)               , ch(08),~
               at (05,11), fac(hex(ac)), hdr1$(2)               , ch(10),~
               at (05,22), fac(hex(ac)), hdr1$(3)               , ch(08),~
               at (05,31), fac(hex(ac)), hdr1$(4)               , ch(08),~
               at (05,41), fac(hex(ac)), hdr1$(5)               , ch(12),~
               at (05,55), fac(hex(ac)), hdr1$(6)               , ch(12),~
               at (05,69), fac(hex(ac)), hdr1$(7)               , ch(12),~
                                                                         ~
               at (06,02), fac(hex(80))  , ckdisp$( 1)          , ch(79),~
               at (06,02), fac(dfac$( 1)), ckdisp$( 1)          , ch(79),~
               at (07,02), fac(dfac$( 2)), ckdisp$( 2)          , ch(79),~
               at (08,02), fac(dfac$( 3)), ckdisp$( 3)          , ch(79),~
               at (09,02), fac(dfac$( 4)), ckdisp$( 4)          , ch(79),~
               at (10,02), fac(dfac$( 5)), ckdisp$( 5)          , ch(79),~
               at (11,02), fac(dfac$( 6)), ckdisp$( 6)          , ch(79),~
               at (12,02), fac(dfac$( 7)), ckdisp$( 7)          , ch(79),~
               at (13,02), fac(dfac$( 8)), ckdisp$( 8)          , ch(79),~
               at (14,02), fac(dfac$( 9)), ckdisp$( 9)          , ch(79),~
               at (15,02), fac(dfac$(10)), ckdisp$(10)          , ch(79),~
               at (16,02), fac(dfac$(11)), ckdisp$(11)          , ch(79),~
               at (17,02), fac(dfac$(12)), ckdisp$(12)          , ch(79),~
               at (18,02), fac(dfac$(13)), ckdisp$(13)          , ch(79),~
               at (19,02), fac(dfac$(14)), ckdisp$(14)          , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               at (23,34), fac(lfac$( 1)), findchknr$           , ch(08),~
                     keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L40332

               if keyhit% <>  7 then L40332
                  if mc_display$ = "Y" then L40329
                     mc_display$ = "Y"
                     str(pf$(1),12,19) = "(7)Statutory Curr  "
                     goto L40112
L40329:           str(pf$(1),12,19) = "(7)Transaction Curr"
                  goto L40107

L40332:        if keyhit% <> 13 then L40350
                  call "MANUAL" ("ARQCHKSB")
                  goto L40116

L40350:        if keyhit% <> 15 then L40370
                  call "PRNTSCRN"
                  goto L40116

L40370:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf1: if opt% > 1% then L40480
           inpmessage$ = "Position Cursor and Press Return to see"  &    ~
                         " Check Details."
               if curr$ = "N" then                                       ~
           pf$(1) = "(2)First                                          "&~
                    "             (13)Instructions"                      ~
               else                                                      ~
           pf$(1) = "(2)First   (7)Transaction Curr                    "&~
                    "             (13)Instructions"
           pf$(2) = "(5)Next    (8)Find Check Number xxxxxxxxx (12)Prin"&~
                    "t Listing    (15)Print Screen"
           pf$(3) = "                                          (14)Prin"&~
                    "t Checks     (16)Exit Display"
           pfkeys$ = hex(ff02ffff05ff0708ffffff0c0d0e0f10ffff2000)
           if curr$  = "Y"  then L40440  : str(pf$(1),12,19) = " "
                                          str(pfkeys$, 7,1) = hex(ff)
L40440:    if first% = 0%   then L40450  : str(pf$(1), 1,8)  = " "
                                          str(pfkeys$, 2,1) = hex(ff)
L40450:    if eof%   = 1%   then L40470  : str(pf$(2), 1,8)  = " "
                                          str(pfkeys$, 5,1) = hex(ff)
L40470:    return

L40480:    inpmessage$ = "Enter Check Number or Press PF-1 to Return" &  ~
                         " to Listing."
           pf$(1) = "(1)Return                                         "&~
                    "             (13)Instructions"
           pf$(2) = "              Find Check Number xxxxxxxxx         "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return


        REM *************************************************************~
            *        R E P O R T   P A R A M E T E R S                  *~
            *-----------------------------------------------------------*~
            * Get Report Parameters.                                    *~
            *************************************************************

        deffn'102(fieldnr%)
            str(line2$,,61%) = "Bill-to: " & billto$ & "  " & billtoname$
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)
            gosub setpf2

L41130:     accept                                                       ~
               at (01,02), "A/R Trial Balance Inquiry: Print",           ~
               at (01,35), fac(hex(8c)), report$                , ch(30),~
               at (01,61), "As Of:",                                     ~
               at (01,69), fac(hex(8c)), asof$                  , ch(10),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Check Numbers to Print",                     ~
               at (06,30), fac(lfac$( 1)), chk_range$(1)        , ch(08),~
               at (06,40), "to",                                         ~
               at (06,43), fac(lfac$( 1)), chk_range$(2)        , ch(08),~
                                                                         ~
               at (07,02), "Starting Post Date",                         ~
               at (07,30), fac(lfac$( 2)), from_date$           , ch(10),~
               at (07,42), "to",                                         ~
               at (07,45), fac(hex(8c))  , asof$                , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L41600
                  call "MANUAL" ("ARQCHKSB")
                  goto L41130

L41600:        if keyhit% <> 15 then L41640
                  call "PRNTSCRN"
                  goto L41130

L41640:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf2
           pf$(1) = "(1)Exit Report Function                           "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Print Report"
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ffff2000)
           if fieldnr% = 0% then return
                pf$(3) = " "
                str(pfkeys$,16,1) = hex(ff)
                return


        REM *************************************************************~
            *           C H E C K   D E T A I L   S C R E E N           *~
            *-----------------------------------------------------------*~
            * Show Check and list it's line items.                      *~
            *************************************************************

        deffn'120
            hdr2$(1) = "Check #"   :  hdr2$(5) = " Bank No."
            hdr2$(2) = "Chk Date"  :  hdr2$(6) = "   Allowed"
            hdr2$(3) = "PostMark"  :  hdr2$(7) = " Unallowed"
            hdr2$(4) = " Posted"   :  hdr2$(8) = " Net Check"

            hdr3$(1) = "Line"           :  hdr3$(6) = "Disc %"
            hdr3$(2) = "Type"           :  hdr3$(7) = "   Allowed"
            hdr3$(3) = "Settlement #"   :  hdr3$(8) = " Unallowed"
            hdr3$(4) = "Srce Doc"       :  hdr3$(9) = " Net Check"
            hdr3$(5) = "Disc Due"

            gosub setpf20

L42165:     mc_display$ = "N"
*        Detail setup - Statutory Currency
            hdr2$(3) = "PostMark"
            hdr3$(5) = "Disc Due"
            chkhdr$ = sthdr$
            mat lines$ = stlines$
            goto L42200
L42172
*        Detail setup - Transaction Currency
            hdr2$(3) = "Currency"
            hdr3$(5) = "Currency"
            chkhdr$ = mchdr$
            mat lines$ = mclines$

L42200:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: CHECK DETAIL",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), hdr2$(1)               , ch(08),~
               at (04,11), fac(hex(ac)), hdr2$(2)               , ch(08),~
               at (04,20), fac(hex(ac)), hdr2$(3)               , ch(08),~
               at (04,29), fac(hex(ac)), hdr2$(4)               , ch(08),~
               at (04,38), fac(hex(ac)), hdr2$(5)               , ch(10),~
               at (04,49), fac(hex(ac)), hdr2$(6)               , ch(10),~
               at (04,60), fac(hex(ac)), hdr2$(7)               , ch(10),~
               at (04,71), fac(hex(ac)), hdr2$(8)               , ch(10),~
               at (05,02), fac(hex(86)),  chkhdr$               , ch(79),~
                                                                         ~
               at (07,02), fac(hex(ac)), hdr3$(1)               , ch(04),~
               at (07,07), fac(hex(ac)), hdr3$(2)               , ch(04),~
               at (07,12), fac(hex(ac)), hdr3$(3)               , ch(11),~
               at (07,24), fac(hex(ac)), hdr3$(4)               , ch(08),~
               at (07,33), fac(hex(ac)), hdr3$(5)               , ch(08),~
               at (07,42), fac(hex(ac)), hdr3$(6)               , ch(06),~
               at (07,49), fac(hex(ac)), hdr3$(7)               , ch(10),~
               at (07,60), fac(hex(ac)), hdr3$(8)               , ch(10),~
               at (07,71), fac(hex(ac)), hdr3$(9)               , ch(10),~
                                                                         ~
               at (08,02), fac(hex(86)),  lines$(t% +  1%)      , ch(79),~
               at (09,02), fac(hex(86)),  lines$(t% +  2%)      , ch(79),~
               at (10,02), fac(hex(86)),  lines$(t% +  3%)      , ch(79),~
               at (11,02), fac(hex(86)),  lines$(t% +  4%)      , ch(79),~
               at (12,02), fac(hex(86)),  lines$(t% +  5%)      , ch(79),~
               at (13,02), fac(hex(86)),  lines$(t% +  6%)      , ch(79),~
               at (14,02), fac(hex(86)),  lines$(t% +  7%)      , ch(79),~
               at (15,02), fac(hex(86)),  lines$(t% +  8%)      , ch(79),~
               at (16,02), fac(hex(86)),  lines$(t% +  9%)      , ch(79),~
               at (17,02), fac(hex(86)),  lines$(t% + 10%)      , ch(79),~
               at (18,02), fac(hex(86)),  lines$(t% + 11%)      , ch(79),~
               at (19,02), fac(hex(86)),  lines$(t% + 12%)      , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L42670

               if keyhit% <>  8 then L42670
                  if mc_display$ = "Y" then L42659
                     mc_display$ = "Y"
                     str(pf$(1),43,19) = "(8)Statutory Curr  "
                     goto L42172
L42659:           str(pf$(1),43,19) = "(8)Transaction Curr"
                  goto L42165

L42670:        if keyhit% <> 13 then L42710
                  call "MANUAL" ("ARQCHKSB")
                  goto L42200

L42710:        if keyhit% <> 15 then L42750
                  call "PRNTSCRN"
                  goto L42200

L42750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf20
           inpmessage$ = " "
           if curr$ = "N" then                                           ~
           pf$(1) = "(2)First   (5)Next                                "&~
                    "             (13)Instructions"                      ~
           else                                                          ~
           pf$(1) = "(2)First   (5)Next                        (8)Trans"&~
                    "action Curr  (13)Instructions"

           pf$(2) = "(3)Last    (6)Down                        (14)Prin"&~
                    "t Check      (15)Print Screen"

           pf$(3) = "(4)Prev    (7)Up                                  "&~
                    "             (16)Exit Display"
           pfkeys$ = hex(ff02030405060708ffffffff0d0e0f10ffff20ff)
           if curr$ = "Y" then L42880  :  str(pf$(1),42,19) = " "
                                         str(pfkeys$,8,1)  = hex(ff)
L42880:    if t% <> 0% then L42920
                str(pf$(1),,8), str(pf$(3),,8), str(pf$(2),12,7) = " "
                str(pfkeys$,2,1), str(pfkeys$,4,1), str(pfkeys$,6,1)     ~
                                                              = hex(ff)
L42920:    if t% + 12% < maxlines% then L42960
                str(pf$(2),,8), str(pf$(1),12,7), str(pf$(3),12,7) = " "
                str(pfkeys$,3,1), str(pfkeys$,5,1), str(pfkeys$,7,1)     ~
                                                              = hex(ff)
L42960:    return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51210,         /* Check Range      */~
                                    L51270          /* From Post Date   */
                  return

L51210
*        Check Range                           CHK_RANGE$()
            call "TESTRNGE" (chk_range$(1), chk_range$(2),               ~
                             chk_range$(3), chk_range$(4),               ~
                             errormsg$)
            return

L51270
*        From Post Date                        FROM_DATE$
            call "DATEOKC" (from_date$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUFMTC" (from_date$) : from_dateu$ = from_date$
                if from_date$ > asofu$ then                              ~
                     errormsg$ = "From Date must be on or before As Of"
                call "DATFMTC" (from_date$)
                return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end
