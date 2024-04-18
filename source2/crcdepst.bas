        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   RRRR    CCC   DDDD   EEEEE  PPPP    SSS   TTTTT   *~
            *  C   C  R   R  C   C  D   D  E      P   P  S        T     *~
            *  C      RRRR   C      D   D  EEEE   PPPP    SSS     T     *~
            *  C   C  R   R  C   C  D   D  E      P          S    T     *~
            *   CCC   R   R   CCC   DDDD   EEEEE  P       SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CRCDEPST - Prints Cash Receipts Deposit Slip.             *~
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
            * 12/15/86 ! Original                                 ! ERN *~
            * 10/10/88 !  Closed printer so RPTPARAM will work.   ! JDH *~
            * 02/14/90 ! Rounded check amount before printing.    ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            banknr$10,                   /* Check Bank Number          */~
            billto$9,                    /* Bill-to Customer Code      */~
            cash_acct$12,                /* Cash-in-Bank Account       */~
            cash_acct_descr$60,          /*              Description   */~
            check$8,                     /* Check Number               */~
            check_date$8,                /* Check Date                 */~
            compname$30,                 /* Company Name               */~
            date$8,                      /* Run Date                   */~
            name$30,                     /* Bill-to Name               */~
            plowkey$50,                  /* Plow Key                   */~
            postdate$8,                  /* Session Post Date          */~
            runtime$8,                   /* Report Run Time            */~
            session$6,                   /* Session ID                 */~
            userid$3                     /* Current User Id            */~

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.01 04/13/90 Patch Release w/ Auto Replace   "
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
            * #1  ! CRCDEPRF ! Cash Receipts Deposit Report File        *~
            * #2  ! GLMAIN   ! General Ledger Chart of Accounts         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CRCDEPRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  26

            select #2,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  9

            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Printing Cash Receipts Deposit Slip")
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (runtime$)
            call "COMPNAME" (02%, compname$, f1%(2))
            call "SETPRNT" ("CRC002", " ", 0%, 0%)
            select printer

        REM *************************************************************~
            *                P R I N T    R E P O R T                   *~
            *-----------------------------------------------------------*~
            * Print a report for each session on file for this User.    *~
            *************************************************************


        session_loop
            plowkey$ = str(userid$) & hex(00)
            call "PLOWNEXT" (#1, plowkey$, 3%, f1%(1))
            if f1%(1) = 0% then exit_program

            get #1 using L10130, session$, cash_acct$, postdate$
L10130:         FMT POS(4), CH(6), POS(27), CH(9), CH(6)
            call "DESCRIBE" (#2, cash_acct$, cash_acct_descr$, 0, f1%(2))
            call "GLFMT" (cash_acct$)
            cash_acct_descr$ = "CASH ACCOUNT: " & cash_acct$ & "  " &    ~
                               cash_acct_descr$
            call "STRING" addr("CT", cash_acct_descr$, 60%)
            call "DATEFMT" (postdate$)
            page%   = 0%
            deposit = 0
            checks% = 0%
            gosub page_heading


        report_loop
            call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
            if f1%(1) = 1% then L10340
                gosub report_totals
                plowkey$ = str(userid$) & session$
                call "DELETE" (#1, plowkey$, 9%)
                goto session_loop

L10340:     get #1 using L10360, billto$, check$, name$, check_date$,     ~
                                banknr$, check_amt
L10360:         FMT XX(9), CH(9), CH(8), CH(30), CH(6), CH(10), PD(14,4)
            check_amt = round(check_amt, 2)
            call "DATEFMT" (check_date$)
            deposit = deposit + check_amt
            checks% = checks% + 1%
            if line% > 55% then gosub page_heading
            print using L10880, billto$, name$, check$, check_date$,      ~
                               banknr$, check_amt
            line% = line% + 1%
            goto report_loop


            report_totals
                print using L10910
                print using L10940, checks%, deposit
                print "** END OF REPORT **"
                return


            page_heading
                page% = page% + 1%
                line% = 7%
                print page
                print using L10700, date$, runtime$, compname$
                print using L10730, postdate$, session$, page%
                print using L10760, cash_acct_descr$
                print
                print using L10790
                print using L10820
                print using L10850
                return




L10700: %RUN DATE: ######## ########                   ##################~
        ~############                   CRCDEPST-CRC002

L10730: %    POST: ######## (######)                      D E P O S I T  ~
        ~ S L I P                           PAGE: ####

L10760: %                               #################################~
        ~###########################

L10790: %            BILL-TO                                  CHECK      ~
        ~CHECK

L10820: %           CUSTOMER  CUSTOMER NAME                   NUMBER     ~
        ~ DATE     BANK NO.     CHECK AMOUNT

L10850: %          ---------  ------------------------------  --------  -~
        ~-------  ----------   -------------

L10880: %          #########  ##############################  ########  #~
        ~#######  ##########  -##,###,###.##

L10910: %                                                                ~
        ~                      -------------

L10940: %                                                                ~
        ~ ##,### CHECKS       -##,###,###.##


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
            call "SHOSTAT" ("One Moment Please")
            close printer
            call "SETPRNT" ("CRC002", " ", 0%, 1%)
            end
