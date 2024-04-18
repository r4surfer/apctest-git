        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L       CCC   H   H  PPPP   RRRR   TTTTT   *~
            *  P   P  R   R  L      C   C  H   H  P   P  R   R    T     *~
            *  PPPP   RRRR   L      C      HHHHH  PPPP   RRRR     T     *~
            *  P      R   R  L      C   C  H   H  P      R   R    T     *~
            *  P      R   R  LLLLL   CCC   H   H  P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLCHPRT - Prints Payroll Checks And Direct Deposit Slips.*~
            *            Subroutine contains code most likely to be     *~
            *            customized, to allow easy rerelease of driver. *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/07/86 ! ORIGINAL                                 ! HES *~
            * 07/08/88 ! Modified to allow reporting pay rates for! MDE *~
            *          !  various earnings types if user requires !     *~
            *          !  reporting rate of pay                   !     *~
            * 09/20/88 ! Fixed MODE% test to only do GETPARM once ! MJB *~
            *          !  per type of call.                       !     *~
            * 01/15/89 !  Truncation, Deposit Slip Spools Without ! Kab *~
            *          !  Warning.                                !     *~
            * 08/03/89 !  Fixed carryover of Rate when current    ! MJB *~
            *          !   earnings = 0.  Also changed rate to    !     *~
            *          !   4 dec places.                          !     *~
            * 02/06/94 !  Correct possible array overrun,         ! JBK *~
            *          !   especially for the UNIX platforms.     !     *~
            * 09/29/97 !  Changes for the year 2000.              ! DXL *~
            *************************************************************

            sub "PRLCHPRT" (mode%, empcode$, checknr$, checkdate_arg$,   ~
                            etype$(), estak(), dtype$(), dstak(), name$, ~
                            address$(), firstpaydate_arg$, lastpaydate_arg$,~
                            ssnumber$, curjob$, curjobeeo$, union$,      ~
                            direct$(), dirbank$(), diramt(),rateflag$)

        dim                                                              ~
            address$(3)30,               /* EMPLOYEE'S HOME ADDRESS    */~
            checkdate$10,                /* Check Date                 */~
            checknr$8,                   /* CHECK NUMBER THIS CHECK    */~
            deductions(2),               /* EMPLOYEE TOTALS            */~
            dstak(10,2),                 /* DEDUCTON PRINT AMOUNT STACK*/~
            dtype$(10)12,                /* DEDUCTON DATA READY FOR PRT*/~
            direct$(5)17,                /* DIRECT DEPOSIT ACCOUNTS    */~
            diramt(6,2),                 /* DIRECT DEPOSIT AMOUNTS     */~
            dirbank$(5)4,                /* DIRECT DEPOSIT BANK CODES  */~
            estak(09,4),                 /* EARNINGS PRINT AMOUNT STACK*/~
            etype$(09)12,                /* EARNINGS DATA READY FOR PRT*/~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            firstpaydate$10,             /* FIRST DATE THIS PAY PERIOD */~
            grosspay(2),                 /* EMPLOYEE TOTALS            */~
            lastpaydate$10,              /* LAST DATE THIS PAY PERIOD  */~
            name$62,                     /* NAME OF EMPLOYEE           */~
            print$(5)10,                 /* FORMATTED STUB PRINT INFO  */~
            prtnetpay$15,                /* NET PAY FOR PRINTING OUT   */~
            prtnetdescr$50,              /* NET PAY DESCRIPTION        */~
            ratemsg$8,                   /* RATE OF PAY Print Message  */~
            rateofp$10                   /* RATE OF PAY (EARNINGS)     */

        dim                      /* PERSONNEL STUFF                    */~
            ssnumber$11,         /* Social security number             */~
            curjob$16,           /* Current job title - personnel syst */~
            curjobeeo$16,        /* EEO class of current job - personn */~
            rateflag$1,          /* Print rates of pay on checks       */~
            union$16             /* Union status - personnel system    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *         I N I A L I Z A T I O N   S E C T I O N           *~
            *                                                           *~
            * Selects printer.                                          *~
            *************************************************************

            checkdate$    = checkdate_arg$
            call "DATFMTC"( checkdate$ )
            lastpaydate$  = lastpaydate_arg$
            call "DATFMTC"( lastpaydate$ )
            firstpaydate$ = firstpaydate_arg$
            call "DATFMTC"( firstpaydate$ )

            if mode% = lastmode% and mode% > 0% then L10000
            REM Note; MODE% = 0 is no-no. Also, caller closes print files
            select printer (134)
            if mode% = -1 then call "SETPRNT" ("PRL011", "MSKC", 0%, 0%)
            if mode% = -2 then call "SETPRNT" ("PRL013", "MSKS", 0%, 0%)
            if mode% = 1 then call "SETPRNT" ("PRL011", "CHCK", 0%, 0%)
            if mode% = 2 then call "SETPRNT" ("PRL013", "SLIP", 0%, 0%)
*          IF ABS(MODE%) = 2 THEN CALL "SET" ADDR("PM", "H")
            lastmode% = mode%

L10000: REM *************************************************************~
            *         P R I N T   H A N D L E R   R O U T I N E         *~
            *                                                           *~
            * TOTALS UP THE DATA AND PRINTS PAYROLL CHECK OR DIRECT     *~
            * DEPOSIT SLIP.                                             *~
            *************************************************************

            mat grosspay = zer : mat deductions = zer

            if mode% > 0 then L10230
                init ("X") etype$(), dtype$(), empcode$, checknr$
                init ("X") checkdate$, name$, address$(), firstpaydate$
                init ("X") lastpaydate$, ssnumber$, curjob$, curjobeeo$
                init ("X") union$, direct$(), dirbank$()
                           /* Multi stmnts at compilers insistance */

                mat estak = con : mat estak = (999999.99)*estak
                mat dstak = con : mat dstak = (999999.99)*dstak
                mat diramt = con : mat diramt = (999999.99)*diramt
                grosspay(1), grosspay(2), deductions(1), deductions(2) = ~
                                                                999999.99
                goto L12000  /* This is a mask */

L10230:     REM Totaling.  Calculate Check Amount...
            for i% = 1 to 9
                grosspay(1) = grosspay(1) + estak(i%,2)
                grosspay(2) = grosspay(2) + estak(i%,3)
            next i%

            for i% = 1 to 10
                deductions(1) = deductions(1) + dstak(i%,1)
                deductions(2) = deductions(2) + dstak(i%,2)
            next i%

L12000: REM =============================================================~
            REM Actual printing of document is here.                     ~
            REM First Format And Print Each Of The 10 Stub Lines...      ~
        REM =============================================================

            print using stub_01, " "
            print using stub_02, " "
            print using stub_03, " "
            print using stub_04, " "
            print using stub_05, empcode$, name$, ssnumber$, checkdate$, ~
                        checknr$
            print using stub_06, " "
            print using stub_07, " "

            gosub'99(1%,1%)
            print using stub_08, etype$(01), print$(1), print$(2),       ~
                        print$(3), dtype$(01), print$(4), print$(5)

            if rateflag$ = "Y" then rate_pay
        REM THIS IS THE NORMAL CHECK PRINTNTING ROUTINE
            gosub'99(2%,2%)
            print using stub_09, etype$(02), print$(1), print$(2),       ~
                        print$(3), dtype$(02), print$(4), print$(5)

            gosub'99(3%,3%)
            print using stub_10, etype$(03), print$(1), print$(2),       ~
                        print$(3), dtype$(03), print$(4), print$(5)

            gosub'99(4%,4%)
            print using stub_11, etype$(04), print$(1), print$(2),       ~
                        print$(3), dtype$(04), print$(4), print$(5)

            gosub'99(5%,5%)
            print using stub_12, etype$(05), print$(1), print$(2),       ~
                        print$(3), dtype$(05), print$(4), print$(5)

            gosub'99(6%,6%)
            print using stub_13, etype$(06), print$(1), print$(2),       ~
                        print$(3), dtype$(06), print$(4), print$(5)

            gosub'99(7%,7%)
            print using stub_14, etype$(07), print$(1), print$(2),       ~
                        print$(3), dtype$(07), print$(4), print$(5)

            gosub'99(8%,8%)
            print using stub_15, etype$(08), print$(1), print$(2),       ~
                        print$(3), dtype$(08), print$(4), print$(5)

            gosub'99(9%,9%) /* Returns to Normal Printing */
            print using stub_16, etype$(09), print$(1), print$(2),       ~
                        print$(3), dtype$(09), print$(4), print$(5)
            gosub'99(10%,10%)
            print using stub_17, dtype$(10), print$(4), print$(5)
        REM IF USER WANTS THE RATE OF PAY PRINTED ON CHECKS
        rate_pay: if rateflag$ <> "Y" then L12720

            gosub'99(1%,2%)  /* Rate1 deduction 2 */
            print using stub_22, ratemsg$, rateofp$, dtype$(02),         ~
                        print$(4), print$(5)
            gosub'99(2%,3%)  /* earntype 2 deduction 3 */
            print using stub_13, etype$(02), print$(1), print$(2),       ~
                        print$(3), dtype$(03), print$(4), print$(5)
            gosub'99(2%,4%)  /* rate 2 deduction 4 */
            print using stub_22, ratemsg$, rateofp$, dtype$(04),         ~
                        print$(4), print$(5)
            gosub'99(3%,5%)  /* earntype 3 deduction 5 */
            print using stub_13, etype$(03), print$(1), print$(2),       ~
                        print$(3), dtype$(05), print$(4), print$(5)
            gosub'99(3%,6%)  /* rate 3 deduction 6 */
            print using stub_22, ratemsg$, rateofp$, dtype$(06),         ~
                        print$(4), print$(5)
            gosub'99(4%,7%)  /* earntype 4 deduction 7 (accum. earn 4) */
            print using stub_13, etype$(04), print$(1), print$(2),       ~
                        print$(3), dtype$(07), print$(4), print$(5)
            gosub'99(4%,8%)  /* rate 4 deduction 8 (Accumulated rate4) */
            print using stub_22, ratemsg$, rateofp$, dtype$(08),         ~
                        print$(4), print$(5)
            gosub'99(9%,9%)
            print using stub_24,  dtype$(09), print$(4), print$(5)


            gosub'99(10%,10%)
            print using stub_17, dtype$(10), print$(4), print$(5)
            /* Returns to normal printing */
L12720:     grosspay(1) = round(grosspay(1),2)
            convert grosspay(1) to print$(1), pic(###,###.##)
            grosspay(2) = round(grosspay(2),2)
            convert grosspay(2) to print$(2), pic(###,###.##)
            print using stub_18, print$(1), print$(2)

            netpay = round(grosspay(1) - deductions(1), 2)
            ytdnetpay = round(grosspay(2) - deductions(2), 2)
            netpay = round(netpay, 2)
            convert netpay to print$(1), pic(###,###.##)
            ytdnetpay = round(ytdnetpay, 2)
            convert ytdnetpay to print$(2), pic(###,###.##)
            deductions(1) = round(deductions(1), 2)
            convert deductions(1) to print$(3), pic(###,###.##)
            deductions(2) = round(deductions(2), 2)
            convert deductions(2) to print$(4), pic(###,###.##)
            print using stub_19, lastpaydate$, print$(1), print$(2),     ~
                        print$(3), print$(4)

            print using stub_20, " "
            print using stub_21, " "

        REM =============================================================~
                Format employee information and print header stuff.      ~
            =============================================================

            print using body_01, " "
            print using body_02, " "
            print using body_03, " "
            print using body_04, " "
            print using body_05, checknr$
            print using body_06, " "
            print using body_07, checkdate$
            netpay = round(netpay, 2)
            convert netpay to prtnetpay$, pic(-$##,###,###.##)

            if abs(mode%) <> 2% then L13360
             REM Direct deposit slip have a slightly different body...
                gosub'98(1%)
                print using slip_08, dirbank$(1), direct$(1), print$(1), ~
                                                      print$(2)

                gosub'98(2%)
                print using slip_09, dirbank$(2), direct$(2), print$(1), ~
                                                      print$(2)

                gosub'98(3%)
                print using slip_10, dirbank$(3), direct$(3), print$(1), ~
                                          print$(2), prtnetpay$

                gosub'98(4%)
                print using slip_11, dirbank$(4), direct$(4), print$(1), ~
                                                      print$(2)

                gosub'98(5%)
                print using slip_12, dirbank$(5), direct$(5), print$(1), ~
                                                      print$(2)
                goto L13550

L13360:     REM Check Body print continues...
            print using body_08, " "
            print using body_09, " "

            on pos(-prtnetpay$ = " ") + 1 goto L13430, L13420
            str(prtnetpay$, pos(-prtnetpay$ = " "), 1) = "*"
L13420:     str(prtnetpay$, pos(-prtnetpay$ = " "), 1) = "*"
L13430:     prtnetdescr$ = "**"

            convert int(netpay) to str(prtnetdescr$,3), pic(-##,###,###)
            call "SPCSMASH" (prtnetdescr$)
            prtnetdescr$ = prtnetdescr$ & " DOLLARS AND"
            temp% = (netpay - int(netpay) + .005) * 100
            convert temp% to temp$, pic(00)
            prtnetdescr$ = prtnetdescr$ & " " & str(temp$,,2) & " CENTS"
            print using body_10, prtnetdescr$, prtnetpay$
            print using body_11, " "
            print using body_12, " "

L13550:     REM Our body comes back together here...
            print using body_13, " "
            print using body_14, name$
            print using body_15, address$(1)
            print using body_16, address$(2)
            print using body_17, address$(3)
            print using body_18, " "
            print using body_19, " "
            print using body_20, " "
            print using body_21, " "
            REM All Done - race ya to the bank!!
            goto L65000

        REM =============================================================~
        =       Line For Line, The Stub Image Statements Follow...      =~
        =================================================================~
                 0....!....1....!....2....!....3....!....4....!....5....!~
        ....6....!....7....!....8....!....9

        stub_01: %#
        stub_02: %#
        stub_03: %#
        stub_04: %#
        stub_05: % ############   #############################   #######~
        ~####   ##########   ########
        stub_06: %#
        stub_07: %#
        stub_08: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_09: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_10: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_11: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_12: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_13: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_14: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_15: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_16: % ############  ###### ########## ##########     #######~
        ~#####  ########## ##########
        stub_17: %                                                #######~
        ~#####  ########## ##########
        stub_18: %                      ########## ##########
        stub_19: % ##########           ########## ##########            ~
        ~       ########## ##########
        stub_20: % ########
        stub_21: %#

        stub_22: %########   ##########                           #######~
        ~#####  ########## ##########
        stub_24: %                                                #######~
        ~#####  ########## ##########

        REM Following Lines are provided to help with customization...
        REM  03  % EMPLOYEE ID    EMPLOYEE NAME                   S-S-N  ~
                DATE        CHECK NO
        REM  04  % ------------   -----------------------------   -------~
        ---     --------    --------
        REM  06  % TYPE           UNITS     AMOUNT      Y-T-D     DEDUCTI~
        ON         AMOUNT      Y-T-D
        REM  07  % ============  ====== ========== ==========     =======~
        ==     ========== ==========
        REM  18  %          TOTAL GROSS ########## ##########
        REM  19  %ENDING ########   NET ########## ##########        DEDU~
        CTIONS ########## ##########

        REM =============================================================~
        =       Line For Line, The Body Image Statements Follow...      =~
        =================================================================~
                 0....!....1....!....2....!....3....!....4....!....5....!~
        ....6....!....7....!....8....!....9

        body_01: %#
        body_02: %#
        body_03: %#
        body_04: %#
        body_05: %                                                       ~
        ~                    ########
        body_06: %#
        body_07: %                                                       ~
        ~                    ##########
        body_08: %#
        body_09: %#
        body_10: %   ##################################################  ~
        ~             ###############
        body_11: %#
        body_12: %#
        body_13: %#
        body_14: %          #################################
        body_15: %          #################################
        body_16: %          #################################
        body_17: %          #################################
        body_18: %#
        body_19: %#
        body_20: %#
        body_21: %#

        REM =============================================================~
        =          Direct deposits Body Is slightly different...        =~
        =================================================================~
                 0....!....1....!....2....!....3....!....4....!....5....!~
        ....6....!....7....!....8....!....9

        slip_08: % ####   #################   ##########   ##########
        slip_09: % ####   #################   ##########   ##########
        slip_10: % ####   #################   ##########   ##########    ~
        ~             ###############
        slip_11: % ####   #################   ##########   ##########
        slip_12: % ####   #################   ##########   ##########

        REM *************************************************************~
            *               F O R M A T    A M O U N T S                *~
            *                                                           *~
            * Formats data for print.                                   *~
            *************************************************************

            deffn'99(x%, z%)   /* Format stub data... */
                  print$() = " "
                  rateofp  =  0 : rateofp$ = " " : ratemsg$ = " "
                  if x% > 9% then L50480            /* Skip ESTAK */
                  if etype$(x%) = " " then L50480
                     ratemsg$ = "Pay Rate"
                     estak(x%,1) = round(estak(x%,1), 2)
                     if estak(x%,1) <> 0 then call "CONVERT"             ~
                                    (estak(x%,1), 0.2, str(print$(1),,6))
                     estak(x%,2) = round(estak(x%,2), 2)
                     if estak(x%,2) <> 0 then convert estak(x%,2) to     ~
                                               print$(2), pic(###,###.##)
                     if estak(x%,2) < 0 then call "CONVERT"              ~
                                   (estak(x%,2), 2.2, str(print$(2),,10))
                     estak(x%,3) = round(estak(x%,3), 2)
                     convert estak(x%,3) to print$(3), pic(###,###.##)
                     if estak(x%,3) < 0 then call "CONVERT"              ~
                                   (estak(x%,3), 2.2, str(print$(3),,10))
                     if estak(x%,1) > 0 then                             ~
                                      rateofp = estak(x%,2) / estak(x%,1)
                     if rateofp <  0 then call "CONVERT"                 ~
                              (rateofp, 2.4, rateofp$)
                     rateofp = round(rateofp, 4)
                     if rateofp <> 0 then convert rateofp to rateofp$,   ~
                     pic(#,###.####)

L50480:           if dtype$(z%) = " " then return
                     dstak(z%,1) = round(dstak(z%,1), 2)
                     if dstak(z%,1) <> 0 then convert dstak(z%,1) to     ~
                                               print$(4), pic(###,###.##)
                     if dstak(z%,1) < 0 then call "CONVERT"              ~
                                   (dstak(z%,1), 2.2, str(print$(4),,10))
                     dstak(z%,2) = round(dstak(z%,2), 2)
                     if dstak(z%,2) <> 0 then convert dstak(z%,2) to     ~
                                               print$(5), pic(###,###.##)
                     if dstak(z%,2) < 0 then call "CONVERT"              ~
                                   (dstak(z%,2), 2.2, str(print$(5),,10))
                     REM Unfortunatly CONVERT sub won't add commas...
                     return

            deffn'98(x%)   /* Format Direct Deposit data... */
                  print$() = " "
                  if direct$(x%) = " " then return
                     diramt(x%,1) = round(diramt(x%,1), 2)
                     if diramt(x%,1) <> 0 then                           ~
                       convert diramt(x%,1) to print$(1), pic(###,###.##)
                     diramt(x%,2) = round(diramt(x%,2), 2)
                     convert diramt(x%,2) to print$(2), pic(###,###.##)
                     return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            end

