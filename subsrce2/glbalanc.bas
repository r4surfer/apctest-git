        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      BBBB    AAA   L       AAA   N   N   CCC    *~
            *  G      L      B   B  A   A  L      A   A  NN  N  C   C   *~
            *  G GGG  L      BBBB   AAAAA  L      AAAAA  N N N  C       *~
            *  G   G  L      B   B  A   A  L      A   A  N  NN  C   C   *~
            *   GGG   LLLLL  BBBB   A   A  LLLLL  A   A  N   N   CCC    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLBALANC - PASSING A DATE, THE SUBROUTINE RETURNS A       *~
            *            CURRENT GL BALANCE FOR A GIVEN ACCOUNT         *~
            *            UP TO AND INCLUDING TRANSACTIONS ALREADY POSTED*~
            *            FOR THE PASSED DATE                            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/28/84 ! ORIGINAL                                 ! ECR *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "GLBALANC"(#1, #2, #3, account$, indate$, balance, errormsg$)

        dim                                                              ~
           account$9,                    /* GL ACCOUNT NUMBER          */~
           balance(32),                  /* GL PERIOD BALANCES         */~
           date$8,                       /* DATE BALANCE REQSTD (UNFMT)*/~
           errormsg$79,                  /* ERROR MESSAGE TEXT         */~
           errormsg$(2)79,               /* ERROR MESSAGE TEXT LIST    */~
           fdates$(32)8,                 /* GL PERIOD ENDING DATES     */~
           glenddate$6,                  /* END GL PER DATE FOR INDATE$*/~
           gldetailkey$23,               /* GLDETAIL FILE PLOW KEY     */~
           indate$8                      /* DATE BALANCE REQUESTED     */~

        dim f1%(03)                      /* RECORD-ON-FILE FLAGS       */~


        REM *************************************************************~
            *                  M A I N    L O O P                       *~
            *                                                           *~
            * GETS THE GL PERIOD DATES ('FISCAL DATES' IN SYSFILE2) AND *~
            * BALANCES, THEN DETAIL. FINALLY COMPUTES BALANCE           *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.01 04/04/86 Physical inventory and miscel   "
        REM *************************************************************
           errormsg$(1) = "G/L Periods Have Not Been Established"
           errormsg$(2) = "G/L Account Does Not Exist: " & account$
           errormsg$    = " "

           date$ = indate$
           call "DATEOK"   (date$, error%, errormsg$)
                if errormsg$ <> " " then end

           call "DATUNFMT" (date$)
           balance = 0

           gosub get_fiscal_dates
                 if error% <> 0% then exit_on_error

           gosub get_gl_balances
                 if error% <> 0% then exit_on_error

           gosub scan_detail

           gosub compute_balance

           end


        exit_on_error:
           errormsg$ = errormsg$(error%)
           end
        REM *************************************************************~
            *    G E T   T H E   G L P E R I O D    S T R U C T U R E   *~
            *                                                           *~
            * GETS THE GL PERIOD DATES ('FISCAL DATES' IN SYSFILE2) AND *~
            * UNFORMATS THEM, STOPPING AT THE DATE > DATE BALANCE REQSTD*~
            * ERROR IF FISCAL DATES RECORD NOT ON FILE.                 *~
            *************************************************************

        get_fiscal_dates:
           error% = 1%
           call "READ100" (#1, "FISCAL DATES", f1%(1))
                 if f1%(1) <> 1% then return

           get #1, using L12800, numglperiods%, fdates$()
L12800:            FMT XX(20), BI(2), XX(138), 32*CH(8)

        REM SCAN THE GLPERIOD ENDING DATES FOR 1ST ONE LATER THAN DATE$  ~
            SKIP THE 13TH MONTH IF NECESSARY AND CLOSING BUCKET

           for period% = 1% to 32%
               if numglperiods% = 12% and period% = 14% then L13800
                  if period% = 15% then L13800
                     if fdates$(period%) > date$ then L14000
L13800:        next period%

L14000:        glperiod%  = period%
               glenddate$ = fdates$(period%)
               error%    = 0%
               return

        REM *************************************************************~
            *     G E T   G L B A L A N C E S   O N   F I L E           *~
            *                                                           *~
            * GETS THE GL BALANCES FROM GLMAIN FILE.  ERROR IF NOT ON   *~
            * FILE.                                                     *~
            *************************************************************

        get_gl_balances:

           error% = 2%
           call "READ100" (#2, account$, f1%(2))
                 if f1%(2) <> 1% then return

           get #2, using L15800, balance()
L15800:            FMT XX(44), 32*PD(14,4)

           error% = 0%
           return
        REM *************************************************************~
            *     S C A N    G / L   D E T A I L   R E C O R D S        *~
            *                                                           *~
            * SCANS THE GLDETAIL FILE FOR DETAIL DATED AFTER REQUESTED  *~
            * DATE BUT NO LATER THAN THE END OF THE GL PERIOD!!!        *~
            *************************************************************

        scan_detail:

           gldetailkey$ = str(account$,,9%)
           str(gldetailkey$,17) = str(date$,,6%) & hex(ffffffffffffffff)

L16500:    call "PLOWNEXT" (#3, gldetailkey$, 16%, f1%(3))
                if f1%(3) <> 1% then return

                   get #3, using L16900, gldetdate$, gldebit, glcredit
L16900:                    FMT XX(16), CH(6), XX(6), 2*PD(14,4)

                  if gldetdate$ > glenddate$ then return
                     balance = round(balance - (gldebit - glcredit), 2%)
                     goto L16500


        REM *************************************************************~
            *     C O M P U T E   T H E   G / L   B A L A N C E         *~
            *                                                           *~
            * Compute the G/L Balance as of INDATE$.  Variable BALANCE  *~
            * already has the negative of transactions after today      *~
            * within the period. (PERIOD . )                            *~
            *************************************************************

        compute_balance:
           for period% = 1% to glperiod%
               balance = round(balance + balance(period%), 2%)
           next period%
           return

