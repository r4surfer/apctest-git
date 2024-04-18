        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   RRRR   L      W   W  H   H  TTTTT  X   X          *~
            *  P   P  R   R  L      W   W  H   H    T     X X           *~
            *  PPPP   RRRR   L      W   W  HHHHH    T      X            *~
            *  P      R   R  L      W W W  H   H    T     X X           *~
            *  P      R   R  LLLLL   W W   H   H    T    X   X          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLWHTX  - COMPUTE WITHOLDING TAXES BASED ON PASSED TABLE *~
            *            ID USING THE TRIED AND TRUE U S GOVT BRACKET   *~
            *            METHOD.  GENERALIZED TO WORK FOR STATES AND    *~
            *            LOCALITIES USING THE SAME METHOD.              *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/10/83 !  GENERALIZED WITHHOLDING TYPE DED        ! KEN *~
            *          !  LINK INTO PRLDDUCT                      !     *~
            * 07/26/88 !  Fixed 'Cannot find' error message       ! JDH *~
            *************************************************************

            sub "PRLWHTX"  (taxable_income, exemptions, amount, payfreq$,~
                                                      tableid$, #1)

            dim                                                          ~
                minimum(12),             /* AT LEAST AMOUNTS           */~
                maximum(12),             /* BUT LESS THAN              */~
                base(12),                /* PLUS AMOUNT                */~
                percent(12),             /* PLUS PERCENT OF OVER       */~
                tableid$9,               /* TABLE CODE$                */~
                payfreq$1                /* PAY FREQUENCY              */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10122
            cms2v$ = "R5.01.01 09/27/88 Patch Release                  "
L10122: REM *************************************************************
            amount = 0
            call "READ100"  (#1, tableid$, f1%)
            if f1%=0 then L11510
            get #1, using L10190, allow, bracket%, minimum(), base(),     ~
                                         percent(), maximum()
L10190:     FMT XX(37), PD(14,4), BI(4), 48*PD(14,4)

        REM *************************************************************~
            *  A N N U A L I Z E   C U R R E N T   P A Y                *~
            *                                                           *~
            * USE PAY FREQUENCY TO ANNUALIZE PAY AND SET FACTOR         *~
            *************************************************************~

            REM COMPUTE AMOUNT TO DEDUCT
                amount, factor = 0

            REM FIRST FIGURE ANNUALIZED PAY
                REM DAILY
                    if payfreq$ <> "8" then L11150
                    annualpay = taxable_income * 260
                    factor = 260
                    go to L12060
L11150:         REM WEEKLY
                    if payfreq$ <> "1" then L11200
                    annualpay = taxable_income * 52
                    factor = 52
                    go to L12060
L11200:         REM BIWEEKLY
                    if payfreq$ <> "2" then L11250
                    annualpay = taxable_income * 26
                    factor = 26
                    go to L12060
L11250:         REM SEMIMONTHLY
                    if payfreq$ <> "3" then L11300
                    annualpay = taxable_income * 24
                    factor = 24
                    go to L12060
L11300:         REM MONTHLY
                    if payfreq$ <> "4" then L11350
                    annualpay = taxable_income * 12
                    factor = 12
                    go to L12060
L11350:         REM QUARTERLY
                    if payfreq$ <> "5" then L11400
                    annualpay = taxable_income * 4
                    factor = 4
                    go to L12060
L11400:         REM SEMIANNUAL
                    if payfreq$ <> "6" then L11450
                    annualpay = taxable_income * 2
                    factor = 2
                    go to L12060
L11450:         REM ANNUAL
                    if payfreq$ <> "7" then L11500
                    annualpay = taxable_income * 1
                    factor = 1
                    go to L12060
L11500:         REM ERROR
L11510: REM CANT DO IT ERROR MESSAGE
                    accept "CANNOT FIND PAY FREQUENCY FOR EMPLOYEE OR CAN~
        ~NOT FIND TABLE INDICATED:",                                      ~
                           fac(hex(8c)),str(tableid$,2,6),        ch(06),~
                           "NO TAX WILL BE WITHHELD.",                   ~
                           "PRESS (ENTER) TO ACKNOWLEDGE AND EXIT."

                    end

        REM *************************************************************~
            *   C A L C U L A T E   A N N U A L   W I T H O L D I N G   *~
            *                                                           *~
            * USE ANNUAL TAX TABLES TO FIGURE YEARLY DEDUCTION          *~
            *************************************************************~

L12060:     REM NOW FIGURE ALLOWANCE ...
                allowance =  allow * exemptions

            REM ... AND ADJUST ANNUAL PAY
                annualpay = annualpay - allowance
                if annualpay <= 0 then L50070

            REM CALCULATE WITHHOLDING
                for i% = 1 to bracket%
                minimum=minimum(i%)
                base   =base   (i%)
                percent=percent(i%)
                if annualpay < maximum(i%) then L12330
                next i%
L12330:         amount=(base +(((annualpay-minimum)*percent)/100))

        REM *************************************************************~
            *   D E A N N U A L I Z E   D E D U C T I O N               *~
            *                                                           *~
            * USE FACTOR TO FIGURE DEDUCTION FOR THIS PERIOD.  NOTE USE *~
            * OF ROUNDING FUNCTION, AND ADDITION OF EMPLOYEE 'EXTRA'.   *~
            *************************************************************~

L50070:     REM WE ARE DONE...
                amount = amount/factor
                if amount < 0 then amount = 0
                end

        REM *************************************************************~
            *    N O T E   O N   C O M P U T A T I O N   M E T H O D    *~
            *                                                           *~
            * THE FIT IS CALCULATED IN THIS ROUTINE BY FIRST ANNUALIZING*~
            * THE CURRENT GROSS PAY, SUBTRACTING THE ALLOWANCE, THEN    *~
            * FINDING THE CORRECT LUMP AND OVERFLOW PERCENTAGE IN THE   *~
            * TABLE COPIED FROM THE CIRCULAR E FROM THE IRS.  THE NEXT  *~
            * STEP IS TO DIVIDE THE CALCULATED ANNUAL DEDUCTION BY THE  *~
            * CORRECT FACTOR, TO REFLECT THE ACTUAL PAY FREQUENCY.  AS  *~
            * A FINISHING TOUCH, AMOUNT TWO IS ADDED TO REFLECT ANY     *~
            * ADDITIONAL DOLLARS TO DEDUCTED BY EMPLOYEE REQUEST.       *~
            *************************************************************

