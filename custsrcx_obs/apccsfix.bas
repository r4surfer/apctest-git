        REM *************************************************************~
            *-----------------------------------------------------------*~
            * APCCSFIX - Changes Customer Open Balance to Match ARMTRIAL*~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/26/96 ! Original                                 ! HEA *~
            * 11/20/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            *************************************************************

        dim                                                              ~
            opn_ar(4%),                  /* Open Balance               */~
            cusname$30,                  /* Customer Name              */~
            billto$9,                    /* Bill-to Code and Balances  */~
            date$8,                      /* Date for screen display    */~
            current_date$6,              /* TODAYS DATE UNFORMATTED    */~
            end_of_file$3,               /* END OF FILE INDICATOR      */~
            plowkey$50,                  /* A Plow Key                 */~
            post_date$6,                 /* Item Post Date             */~
            userid$3                     /* Current User Id            */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* File Read Status Flag      */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Correct Customer Open Balances    "
            pname$ = "APCCSFIX - Rev: R6.04"

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
            * #1  ! CUSTOMER ! Customer Master                          *~
            * #2  ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #3  ! CRCMASTR ! Accounts Balances for Customer           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #2,  "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen = 21

            select #3,  "CCRMASTR",                                      ~
                        varc,     indexed,  recsize =   200,             ~
                        keypos =    1, keylen =   9

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%),100%, rslt$(3%))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            current_date$ = date$
            call "DATEFMT" (date$)

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Print Report.                                             *~
            *************************************************************

            call "SHOSTAT" ("Changing Customer Balances")
            select printer  (134)
            call "SETPRNT"  ("AR0001", " ", 0%, 0%)
            plowkey$ = "         "
            line% = 99%
            gosub read_armtrial
            sav_cust$ = billto$
            tot_bal = 0.0000
            goto armtrial_loop

        print_line

            if line% > 56% then gosub page_heading

            print using L13140, cusnbr$, cusname$, cusbal$, opn_ar$,      ~
                  diff$
            print

            line% = line% + 2%
            return

        armtrial_loop
            if sav_cust$ <> billto$ then gosub customer_change

            tot_bal = tot_bal + trans_amt
            gosub read_armtrial
            if end_of_file$ = "EOF" then goto end_report
            goto armtrial_loop

        customer_change
            call "SHOSTAT" ("Changing Bal for ("&sav_cust$&")")
            gosub read_customer
            if  tot_bal =  opn_ar(3) then L12470
            diff = opn_ar(3) - tot_bal
            convert tot_bal to opn_ar$, pic(#,###,###.##)
            convert opn_ar(3) to cusbal$, pic(#,###,###.##)
            convert diff to diff$, pic(#,###,###.##)
            gosub print_line

            gosub update_customer

L12470:     sav_cust$ = billto$
            tot_bal = 0.0000
            return

        read_armtrial
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
            if f1%(2) <> 0% then L12570
               end_of_file$ = "EOF"
               return

L12570:     get #2 using L12600, billto$, trans_amt, post_date$
            return

L12600:     FMT CH(9), POS(68), PD(14,4), POS(97), CH(6)

        read_customer
            cusnbr$ = sav_cust$
            call "READ100" (#1, cusnbr$, f1%(1%))
            if f1%(1%) = 0% then L12720
            get #1 using L12740, cusnbr$, cusname$

            call "READ100" (#3, cusnbr$, f1%(3%) )
            if f1%(3%) = 0% then goto L12720
               get #3, using L12750, opn_ar()

L12720:     return

L12740:     FMT CH(9), POS(40), CH(30)
L12750:     FMT POS(114), 4*PD(14,4)

        end_report
            gosub customer_change
            close printer
            goto exit_program

        update_customer
            cusnbr$ = sav_cust$
            call "READ101" (#3, cusnbr$, f1%(3%))
            if f1%(3%) = 0% then L12900
               get #3 using L12920, opn_ar()
               opn_ar(3%) = tot_bal
               put #3 using L12920, opn_ar()
            rewrite #3
L12900:     return

L12920:     FMT POS(114), 4*PD(14,4)

        page_heading
            page% = page% + 1%  : line% = 5%
            print page
            print using L13090, date$, page%

            print
            print using L13110
            print
            return

        REM *************************************************************~
            *          I M A G E   S T A T E M E N T S                  *~
            *************************************************************


L13090: % Report Title: ARMTRIAL Balances Different from Customer Master ~
        ~     Date- as of ########   Page ###
L13110: % Customer    Customer Name                  Customer Balance ART~
        ~RIAL Balance        Difference

L13140: % #########  ###############################     ############    ~
        ~############      ############



        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("ARGL01", " ", 0%, 1%)
            end
