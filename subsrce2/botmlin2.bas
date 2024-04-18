        REM *************************************************************~
            *                                                           *~
            *  BBBB    OOO   TTTTT  M   M  L      IIIII  N   N  2222    *~
            *  B   B  O   O    T    MM MM  L        I    NN  N      2   *~
            *  BBBB   O   O    T    M M M  L        I    N N N  2222    *~
            *  B   B  O   O    T    M   M  L        I    N  NN  2       *~
            *  BBBB    OOO     T    M   M  LLLLL  IIIII  N   N  22222   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOTMLIN2 - TAKES THE G/L ACCOUNT PASSED AND RETURNS THE   *~
            *            YEAR-TO-DATE AND MONTH-TO-DATE                 *~
            *            BALANCES IN A NUMERIC FORMAT AND IN THE ALPHA  *~
            *            FORMAT READY FOR PRINTING. THE ROUTINE WILL    *~
            *            RETURN -+ , (), OR DR CR BY SPECIFYING IN THE  *~
            *            CALLING PARAMTERS.                             *~
            *            IF BALANCEFLAG = 3 THEN THE ONLY ACCOUNTS THAT *~
            *            GET () AROUND IT ARE THE ACCOUNTS THAT HAVE    *~
            *            AN ABNORMAL BALANCE. IE IF A CASH ACCOUNT HAS  *~
            *            A CREDIT BALANCE INSTEAD OF ITS NORMAL DEBIT   *~
            *            THIS ACCOUNT BALANCE WILL RETURN WITH ().      *~
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
            * 11/16/82 ! ORIGINAL (FROM BOTMLINE)                 ! ECR *~
            *************************************************************

        sub "BOTMLIN2" (acct$,           /* ACCOUNT NUMBER             */~
                        descr$,          /* TO BE RETURNED TO MAIN PROG*/~
                        mtdbal,          /* MONTH TO DATE BALANCE      */~
                        ytdbal,          /* YEAR TO DATE BALANCE       */~
                        mtdbalance$,     /* FORMATTED MONTH TO DATE BAL*/~
                        ytdbalance$,     /* FORMATTED YEAR TO DATE BAL.*/~
                        balanceflag%,    /* 0 = -+ FORMATTED BALANCE   */~
                                         /* 1 = -+ FORMATTED BALANCE   */~
                                         /* 2 = DR CR FORMAT BALANCE   */~
                                         /* 3 = () FORMAT ON ABNORMAL B*/~
                        monthopen%,      /* MONTH TO REPORT ON         */~
                        #1,              /* GLMAIN UFB ADDRESS         */~
                        f2%,             /* STATUS FLAG FOR GLMAIN     */~
                        returncode%)     /* 0 = GOOD 99 = NO GO        */
        dim                                                              ~
           acct$9,                       /* ACCOUNT NUMBER             */~
           descr$32,                     /* ACCOUNT DESCRIPTION        */~
           mo$(3)2,                      /* DR, CR CHARACTERS          */~
           monthlybal(32),               /* NUMERIC FOR ASSAULTING     */~
           mtdbalance$20,                /* FORMATTED CURRENT ACTIVITY */~
           type$1,                       /* ACCOUNT TYPE               */~
           ytdbalance$20                 /* YEAR TO DATE BALANCE PRINT */~

        returncode% = 99

        REM INITIALIZE VARIABLES
            init(" ") descr$, mtdbalance$, ytdbalance$
            mat monthlybal = zer

        REM CHECK TO SEE IF THE GLMAIN FILE IS ALREADY OPEN IF NOT, OPEN

           if f2% = 0 then L12140
              end

L12140: REM CALL FOR THE RECORD ON THE GENERAL LEDGER MAIN FILE
                   call "READ100" (#1, acct$, f1%)
                   if f1% = 0 then end

        REM RETRIVE THE PERTINENT INFORMATION FROM GLMAIN
                   get #1, using L12260, descr$, type$, monthlybal()

L12260:            FMT XX(9), CH(30), CH(1), XX(4), 32*PD(14,4)

        REM SET MULTIPLIER
            if type$ = "$" then multiply =  1
            if type$ = "A" then multiply =  1
            if type$ = "L" then multiply = -1
            if type$ = "C" then multiply = -1
            if type$ = "R" then multiply = -1
            if type$ = "E" then multiply =  1

        REM COMPUTE YEAR-TO-DATE BALANCE
            ytdbal  = 0
            for i% = 1 to monthopen% + 15
            ytdbal  = ytdbal + monthlybal(i%)
            next i%
            temp = round(ytdbal * multiply, 2)

        REM SET MONTH-TO-DATE BALANCE
            mtdbal = round (monthlybal(monthopen%+15), 2)
            temp1 = mtdbal * multiply

        REM FORMAT BALANCES ACCORDING TO BALANCEFLAG
            if balanceflag% = 2 then L20000  /* DR, CR FLAG */
            if balanceflag% = 3 then L30000  /* () FLAG     */

        REM FORMATS BALANCES WITH - OR + SIGN HERE (BALANCEFLAG = 0 OR 1)

            convert temp to ytdbalance$, pic (+###,###,###.##)
            convert temp1 to mtdbalance$, pic(+###,###,###.##)

            go to L50000

L20000: REM DO THE DR, CR BALANCE FLAG ROUTINE HERE (BALANCEFLAG = 2)

            if type$ = "$" then mo$(1) = "DR"
            if type$ = "A" then mo$(1) = "DR"
            if type$ = "L" then mo$(1) = "CR"
            if type$ = "C" then mo$(1) = "CR"
            if type$ = "R" then mo$(1) = "CR"
            if type$ = "E" then mo$(1) = "DR"

        REM Use multiplier to set sign and then add debit/credit indicator

            convert temp to ytdbalance$, pic(-###,###,###.##)
            str(ytdbalance$, 17, 2) = mo$(1)

            convert temp1 to mtdbalance$, pic(-###,###,###.##)
            str(mtdbalance$, 17, 2) = mo$(1)

        REM AND GO TO EXIT THE SUBROUTINE
            go to L50000

L30000: REM NOW WE'RE GOING TO TAKE CARE OF () (BALANCEFLAG = 3)

        REM PROCESS FOR CREDIT BALANCE AS THE NORMAL ACCOUNT BALANCE

            convert abs(temp) to ytdbalance$, pic(###,###,###.##)
            call "NUMPAREN" (temp, ytdbalance$)

            convert abs(temp1) to mtdbalance$, pic(###,###,###.##)
            call "NUMPAREN" (temp1, mtdbalance$)
            goto L50000

L50000: REM SET THE RETURNCODE HERE AND EXIT THE SUBROUTINE
            returncode% = 0
            end
