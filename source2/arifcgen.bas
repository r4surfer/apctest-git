        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAAA  RRRR   IIIII  FFFFF   CCC    GGG   EEEEE  N   N   *~
            *  A    A R   R    I    F      C      G      E      NN  N   *~
            *  AAAAAA RRRR     I    FFFF   C      G  GG  EEE    N N N   *~
            *  A    A R  R     I    F      C      G   G  E      N  NN   *~
            *  A    A R   R  IIIII  F       CCC    GGG   EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARIFCGEN - Automatic generation of Finance Charges.       *~
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
            * 09/18/86 ! ORIGINAL                                 ! ERN *~
            * 11/26/86 ! Add Trial Balance File & Settlements.    ! ERN *~
            * 12/14/87 ! Added Multi-currency, CURCONVR, ARILNCUR.! JIM *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 10/02/92 ! Added Billto, Shipto, Invoice to ARIBUFFR! JDH *~
            * 04/25/95 ! PRR - 13283 Additional Key to ARIBUFFR.  ! RJH *~
            * 07/24/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        dim                                                              ~
            acctxref$9,                  /* Customer Account X-ref     */~
            aracct$9,                    /* A/R Account                */~
            balance(2),                  /* Customer Balance           */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            billxref$9,                  /* Customer Bill-to X-ref     */~
            blank$(30)6,                 /* Array for Writes           */~
            convdate$6,                  /* Currency conversion date   */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4,                  /* Currency code              */~
            cursor%(2),                  /* Cursor Location for Edit   */~
            cuscode$9,                   /* Customer Code              */~
            date$8,                      /* Today's Date               */~
            dfltaracct$12,               /* Default A/R Account        */~
            dfltfcacct$12,               /* Default FC Account         */~
            edtmessage$79,               /* Edit Screen Message        */~
            errormsg$79,                 /* Error Message              */~
            fcacct$9,                    /* Finance Charge Acct (CR)   */~
                                         /* Finance Charge Tables      */~
            fc_date$8,                   /* 'As Of' Date               */~
            fct_bases(12,2),             /*   Table Base Amounts       */~
            fct_brackets%(2),            /*   # of Brackets in Table   */~
            fct_effec$(2)6,              /*   Table Effective Date     */~
            fct_min_charge(2),           /*   Minimum Finance Charge   */~
            fct_maxs(12,2),              /*   Table Maximum Amounts    */~
            fct_mins(12,2),              /*   Table Minimum Amounts    */~
            fct_pastdue$(2)3,            /*   Past Due Days            */~
            fct_pastdue%(2),             /*   Past Due Days            */~
            fct_pcts(12,2),              /*   Table APR Amounts        */~
            fct_fin_on_fin$(2)1,         /*   Apply FCs on FCs?        */~
            fct_periods%(2),             /*   Periods per Year         */~
            fct_temp(48),                /*   Temp Array for Read      */~
            fct_toolate$(2)6,            /*   Past Due Dates           */~
                                                                         ~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input Message              */~
            invnr$8,                     /* Invoice Number for FC      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Screen Line         */~
            netdue$6,                    /* Next Due Date (00 trans)   */~
            pf$(3)79, pfkeys$20,         /* PF Keys on screen          */~
            postdate$8,                  /* Posting Date               */~
            session$6,                   /* Session Number             */~
            shipto$180,                  /* Ship-to Address            */~
            soldto$180,                  /* Sold-to Address            */~
            statutory$4,                 /* Statutory currency code    */~
            store$3, storedescr$30,      /* Store for Invoice #s       */~
            syskey$20,                   /* SYSFILE2 Key               */~
            table$1,                     /* Table from Customer Record */~
            tables$3, tablesdescr$30,    /* Selected Table or 'ALL'    */~
            tbkey$30,                    /* Trial Balance plow key     */~
            terms$10,                    /* Settlement for last terms  */~
            type$2,                      /* Doc Source and Types Codes */~
            zeroes(30)                   /* Array for Write            */


        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            fs%(64),                     /* File Status                */~
            rslt$(64)20                  /* RETURN CODE FROM "OPENCHCK"*/


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! STORNAME ! Store Master                             *~
            * #03 ! CUSTOMER ! Customer Master                          *~
            * #04 ! SYSFILE2 ! System File (For FC Tables, etc.)        *~
            * #05 ! ARIMASTR ! Invoice Master File - Headers            *~
            * #06 ! ARINUMBR ! Duplicate Invoice Number Control File    *~
            * #08 ! ARMTRIAL ! A/R Trial Balance                        *~
            * #09 ! ARIBUFFR ! Invoice Buffer- Headers                  *~
            * #10 ! ARIBUF2  ! Invoice Buffer- Lines                    *~
            * #41 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #42 ! ARIMSCUR ! Currency-specific ARI Master             *~
            * #43 ! ARILNCUR ! Currency-specific ARI lines              *~
            *************************************************************

            select #01, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #03, "CUSTOMER"                                       ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #04, "SYSFILE2"                                       ~
                        varc, indexed, recsize = 500,                    ~
                        keypos =    1, keylen =   20

            select  #05,"ARIMASTR",                                      ~
                        varc, indexed, recsize = 2000,                   ~
                        keypos =    1, keylen =  17,                     ~
                        alternate key 1, keypos = 10, keylen =  8, dup,  ~
                                  key 2, keypos = 18, keylen = 16, dup,  ~
                                  key 3, keypos = 34, keylen = 16, dup

            select  #06,"ARINUMBR",                                      ~
                        varc, indexed, recsize = 17,                     ~
                        keypos =  1,   keylen = 17,                      ~
                        alt key  1, keypos =    10, keylen =  8, dup

            select  #08,"ARMTRIAL",                                      ~
                        varc, indexed, recsize = 256,                    ~
                        keypos = 1, keylen = 21

            select #09, "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos = 1, keylen =   17,                       ~
                        alt key  1, keypos = 2001, keylen =   24,        ~
                            key  2, keypos =   34, keylen =   16, dup

            select #10, "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =   1,  keylen = 20

            select #41, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #42, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5,  keylen = 17,                      ~
                        alt key  1, keypos =   1, keylen =  21

            select #43, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   5,  keylen = 20,                      ~
                        alt key  1, keypos =   1, keylen =  24

        call "SHOSTAT"  ("Opening Files, One Moment Please.")
            call "OPENCHCK" (#01, fs%(1 ), f2%(1 ),   0%, rslt$(1 ))
            call "OPENCHCK" (#03, fs%(3 ), f2%(3 ),   0%, rslt$(3 ))
            call "OPENCHCK" (#04, fs%(4 ), f2%(4 ),   0%, rslt$(4 ))
            call "OPENCHCK" (#05, fs%(5 ), f2%(5 ),   0%, rslt$(5 ))
            call "OPENCHCK" (#06, fs%(6 ), f2%(6 ), 100%, rslt$(6 ))
            call "OPENCHCK" (#08, fs%(8 ), f2%(8 ),   0%, rslt$(8 ))
            call "OPENCHCK" (#09, fs%(9 ), f2%(9 ), 200%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 400%, rslt$(10))

*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#04, "SWITCHS.CUR", f1%(4))
                if f1%(4) = 0% then L09000
            get #04 using L02800, curr$, statutory$, currtype$
L02800:         FMT POS(21), CH(1), CH(4), POS(29), CH(1)
            if curr$ <> "Y" then goto L09000
                call "OPENCHCK"(#41, fs%(41), f2%(41), f1%(41), rslt$(41))
                call "OPENCHCK"(#42, fs%(42), f2%(42),    200%, rslt$(42))
                call "OPENCHCK"(#43, fs%(43), f2%(43),    400%, rslt$(43))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr ("ID", userid$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press (RETURN)."

*        Get which Session to put these invoices into
            u3% = 1%
            call "UPDUSRLG" ("ARIUPDTE", "ARIFCGEN",                     ~
                             "Automatic Finance Charges",                ~
                             "1", session$, u3%, postdate$, " ")
            if u3% <> 0% then L65000

*        Now set up some misc. variables
            line2$ = "     Session: " & session$
            str(line2$,62) = "ARIFCGEN: " & str(cms2v$,,8)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, fc_date$, store$, tables$, ~
                      storedescr$, tablesdescr$

            for fieldnr% = 1% to 3%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10190
L10130:         gosub'101(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10130
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
L10190:         next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

L11060:     gosub'101(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       start_processing
                  if keyhit% <>  0 then       L11060
L11100:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1% or fieldnr% > 3% then L11060
            gosub'051(fieldnr%)
L11130:     gosub'101(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
                     if fieldnr% = cursor%(1) - 5% then L11060 else L11100
            goto L11060


        REM *************************************************************~
            *     G E N E R A T E   F I N A N C E   C H A R G E S       *~
            * --------------------------------------------------------- *~
            * Calculate finance charges.                                *~
            *************************************************************

        start_processing
            call "SHOSTAT" ("Generating Finance Charges")
            call "ARMGLGET" (4%, " ", " ", " ", " ", store$, " ",        ~
                #04, #05, #05, #05, #05, dfltaracct$)
            call "ARMGLGET" (8%, " ", " ", " ", " ", store$, " ",        ~
                #04, #05, #05, #05, #05, dfltfcacct$)
            call "GLUNFMT" (dfltaracct$)
            call "GLUNFMT" (dfltfcacct$)
            call "DATUNFMT" (fc_date$)


        next_customer
            call "PLOWNEXT" (#03, cuscode$, 0%, f1%(3%))
            if f1%(3%) = 0% then L65000

            get #03 using L12220, cuscode$, table$, currency$
L12220:         FMT CH(9), POS(1044), CH(1), CH(4)

            if tables$ <> "ALL" and table$ <> tables$ then next_customer

                gosub load_table
                if tableonfile% = 0% then next_customer

                     gosub age_customer

                     gosub calculate_finance_charge

                     if fcamt <= 0 then goto next_customer

                     gosub finance_charge_conversion

                     gosub write_invoice

                     goto next_customer


        REM *************************************************************~
            *         P R O C E S S I N G   R O U T I N E S             *~
            * --------------------------------------------------------- *~
            * The guys who do the real work.                            *~
            *************************************************************

        load_table  /* Move FC Table into arrays.                      */
            syskey$ = "FINANCECHARGETABLE" & table$
            call "READ100" (#04, syskey$, tableonfile%)
            if tableonfile% = 0% then return
                t% = 1%  :  gosub get_table_record

                syskey$ = "FINANCECHARGETABLE" & table$ & "*"
                call "READ100" (#04, syskey$, staronfile%)
                if staronfile% = 0% then return
                     t% = 2%  :  gosub get_table_record
                     return


            get_table_record
                get #04 using L12670,                                     ~
                          fct_effec$     (t%),     /* Effective Date   */~
                          fct_periods%   (t%),     /* Number of Periods*/~
                          fct_min_charge (t%),     /* Minimum FC AMount*/~
                          fct_pastdue$   (t%),     /* Past Due Reqmnt  */~
                          fct_fin_on_fin$(t%),     /* Apply FCs on FCs?*/~
                          fct_brackets%  (t%),     /* # of Brackets    */~
                          fct_temp()               /* Bracket Info     */

L12670:              FMT XX(50), CH(6), XX(8), BI(4), PD(14,4), CH(3),   ~
                         CH(1), XX(1), BI(4), 48*PD(14,4)

                fct_pastdue%(t%) = 0%
                convert fct_pastdue$(t%) to fct_pastdue%(t%),            ~
                                                         data goto L12740
                fct_pastdue%(t%) = -fct_pastdue%(t%)
L12740:         call "DATE" addr("G+", str(fc_date$,,6),                 ~
                                 fct_pastdue%(t%), fct_toolate$(t%), u3%)

                for n% = 1% to 12%
                     fct_mins (n%, t%) = fct_temp(n%     )
                     fct_bases(n%, t%) = fct_temp(n% + 12)
                     fct_pcts (n%, t%) = fct_temp(n% + 24)
                     fct_maxs (n%, t%) = fct_temp(n% + 36)
                next n%
                return


*       ***************************************************************
        age_customer
*        Determine the Customer's Balance that is applicable to Finance
*        Charges.  We determine the balance per the FC Date that we are
*        doing the pass for (i.e., excluding invoices and checks that
*        are after the FC Date.

            balance(1), balance(2) = 0   /* 1st = for Current Table    */
                                         /* 2nd = for '*' Table        */
            tbkey$ = str(cuscode$,,9) & hex(00)
            terms$ = all(hex(ff))  /* Where last terms came from  */

          tb_loop
            call "PLOWNEXT" (#08, tbkey$, 9%, f1%(8))
            if f1%(8) = 0% then return

            get #08 using L13050, amount, type$, postdate$
            if str(tbkey$,10,10) <> terms$ then                          ~
                                             get #08 using L13060, netdue$
L13050:         FMT POS(68), PD(14,4), POS(87), CH(2), POS(97), CH(6)
L13060:         FMT POS(37), CH(6)
            terms$ = str(tbkey$,10,10)
            t% = 1% : if postdate$ < fct_effec$(1) and staronfile% = 1%  ~
                      /* Determine which FC Table */       then t% = 2%
            if type$ <> "IF" or fct_fin_on_fin$(t%) = "Y" then L13130
                str(tbkey$,20,2) = hex(ff)  /* Skip over FC trans */
                goto tb_loop
L13130:     if postdate$ > fc_date$ then tb_loop  /* Future trans */

                if netdue$ < fct_toolate$(t%) then                       ~
                                       balance(t%) = balance(t%) + amount
                goto tb_loop


*       ***************************************************************
        calculate_finance_charge

            t% = 1% : amt = balance(1)
                      gosub L13360
                      fcamt = fc

            if staronfile% = 1% then t% = 2%
                      amt = balance(2)
                      gosub L13360
                      fcamt = fcamt + fc

            fcamt = max(0, fcamt)
            if fcamt > 0 then fcamt = max(fct_min_charge(1), fcamt)
            return

L13360
*        Routine to Calculate Finance Charge
           fc = 0  :  if amt = 0 then return

           for i% = 1% to fct_brackets%(t%)    /* Find table entry     */
                if amt < fct_maxs(i%,t%) then L13440   /* Found it      */
           next i%
           i% = fct_brackets%(t%) /* Default to Last */

L13440:    amt = amt - fct_mins(i%,t%)             /* Remove Minimum   */
           fc  = (amt * fct_pcts(i%,t%) * .01) / fct_periods%(t%)
           fc  = round(fc + fct_bases(i%,t%), 2)    /* Add Base Amount */
           return


*       *****************************************************************
        finance_charge_conversion
            convdate$ = " " : conveqv, convunt = 1
            if curr$ <> "Y" then return
            if currency$ = " " then currency$ = statutory$
            if currency$ = statutory$ then return

            call "DATREVRS" ( fc_date$, rev_date$, errormsg$ )
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#41, currkey$, 5%, f1%(41))
            if f1%(41) <> 0% then get #41 using L13650, convdate$,        ~
                conveqv, convunt
L13650:         FMT POS(12), CH(6), 2*PD(14,7)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr%  gosub      L20100,         /* FC Date          */~
                                    L20200,         /* Store Code       */~
                                    L20400          /* FC Table(s)      */
            return

L20100
*        Default/Enable for FINANCE CHARGE DATE
            if fc_date$ = " " or fc_date$ = blankdate$ then fc_date$ = date$
            inpmessage$ = "Enter Finance Charge Date"
            return

L20200
*        Default/Enable for STORE
            inpmessage$ = "Enter Store to use when assigning Invoice" &  ~
                          " Numbers."
            return

L20400
*        Default/Enable for FC TABLE ID
            if tables$ = " " then tables$ = "ALL"
            inpmessage$ = "Enter Selected Table ID or 'ALL'."
            return


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
L29945:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29945
                return clear all
                goto inputmode


        REM *************************************************************~
            *                 D A T A   S A V E                         *~
            * --------------------------------------------------------- *~
            * Write out Finance Charge Invoice to Buffer files          *~
            *************************************************************
        write_invoice

*        First Load any more data that is required
            get #03 using L30100, soldto$, shipto$, aracct$, acctxref$,   ~
                                billxref$, fcacct$
L30100:         FMT XX(39), CH(180), POS(253), CH(180), POS(472), CH(9), ~
                    POS(771), 2*CH(9), POS(1035), CH(9)
            if aracct$ = " " then aracct$ = dfltaracct$
            if fcacct$ = " " then fcacct$ = dfltfcacct$

            call "ARINEXT" (#01, #04, #06, #08, "F", store$, cuscode$,   ~
                                                       billxref$, invnr$)

*        Optionally write line item to ARIBUF2 'shadow' file ARILNCUR ...
            if curr$ <> "Y" then goto L30330
            if currency$ = statutory$ then goto L30330
                write #43 using L30250, currency$, cuscode$, invnr$,      ~
                    "  1", fcamt, 0, 0, fcamt, convdate$, conveqv,       ~
                    convunt, " "
L30250:             FMT CH(4), CH(9), CH(8), CH(3), 4*PD(14,4), CH(6),   ~
                        2*PD(14,7), CH(22)

*        ... as well as to the ARIBUFFR 'shadow' file ARIMSCUR
                zeroes(1) = 0
                write #42 using L30320, currency$, cuscode$, invnr$,      ~
                    fcamt, 0, 0, 0, fcamt, 0, convdate$, conveqv,        ~
                    convunt, zeroes(), " "
L30320:                FMT CH(4), CH(9), CH(8), 6*PD(14,4), CH(6),       ~
                           2*PD(14,7), 30*PD(14,4), CH(69)

                fcamt = fcamt * conveqv

L30330
*        Write the Line Item to ARIBUF2
            zeroes(1) = 1
            write #10 using L31170,                                       ~
                cuscode$, invnr$, "  1", " ", "Finance Charge",          ~
                "Finance Charge", " ", 0, 1, 0, fcamt, "EACH", "EACH",   ~
                1, fcamt, 0, 0, fcamt, "N", fcacct$, fcacct$, " ", " ",  ~
                " ", blank$(), zeroes(), "Y", " "

*        Write the Header Record to ARIBUFFR
            zeroes(1) = 0
            write #09 using L30560,                                       ~
                cuscode$, invnr$, " ", " ", " ", shipto$, soldto$,       ~
                " ", " ", " ", " ", 0, 0, " ", " ", " ", " ", 0%, 0%, 0%,~
                " ", "Q", fc_date$, " ", " ", date, userid$, " ",        ~
                aracct$, " ", " ", fcacct$, fcacct$, fcamt, 0, 0, 0, 0,  ~
                fcamt, fcamt, billxref$, " ", store$, " ", 0, "F",       ~
                " ", " ", "N", " ", "A", "NET", zeroes(), zeroes(),      ~
                blank$(), blank$(), " ", acctxref$, currency$,           ~
                billxref$, cuscode$, invnr$, " ",                        ~
                session$, "F", cuscode$, invnr$
            return


L30560: FMT                 /* FILE #09 -- ARIBUFFR                    */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Invoice Number                          */~
            CH(16),         /* Purchase Order Number                   */~
            CH(16),         /* Sales Order Number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(180),        /* Ship To Name and Address                */~
            CH(180),        /* Sold-To Name and Address                */~
            CH(6),          /* Ship Date                               */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            CH(6),          /* Shipping Carrier Code                   */~
            PD(14,4),       /* Number of Cartons                       */~
            PD(14,4),       /* Shipment Weight                         */~
            CH(20),         /* Freight/ Air Bill Number                */~
            3*CH(4),        /* Salesman Id                             */~
            3*BI(01),       /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region Code                             */~
            CH(1),          /* Price Code                              */~
            CH(6),          /* Invoice Date                            */~
            CH(6),          /* Recurring Expiration Date               */~
            CH(6),          /* Post Date                               */~
            CH(6),          /* Entry Date                              */~
            CH(3),          /* User ID who entered transaction         */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(9),          /* Net Invoice Distr. Account (A/R)        */~
            CH(9),          /* Freight Account Code                    */~
            CH(9),          /* Sales Tax Account Code                  */~
            CH(9),          /* Sales Distribution Account              */~
            CH(9),          /* Sales Discounts Distribution Account    */~
            PD(14,4),       /* Gross Invoice Amount                    */~
            PD(14,4),       /* Discount Percentage                     */~
            PD(14,4),       /* Discount amount                         */~
            PD(14,4),       /* Freight Amount                          */~
            PD(14,4),       /* Sales Tax Amount                        */~
            PD(14,4),       /* Net Invoice Amount                      */~
            PD(14,4),       /* Current Balance                         */~
            CH(9),          /* Bill-to Cross Reference                 */~
            CH(12),         /* Settlement Code                         */~
            CH(3),          /* Store Code                              */~
            CH(10),         /* Sales Tax Code                          */~
            PD(14,4),       /* Sales Tax Percent                       */~
            CH(1),          /* Invoice Type                            */~
            CH(9),          /* Invoice Reason Code                     */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Post Inventory with this Transaction?   */~
            CH(1),          /* Post G/L with this Transaction?         */~
            CH(1),          /* A/R Type (A/C/E)                        */~
            CH(20),         /* Payment Terms                           */~
            30*PD(14,4),    /* Payment Amount Due                      */~
            30*PD(14,4),    /* Cash Discount Percent                   */~
            30*CH(6),       /* Discount Terms                          */~
            30*CH(6),       /* Net Payment Terms                       */~
            CH(2),          /* Customer Type                           */~
            CH(9),          /* Account X-Ref                           */~
            CH(4),          /* Currency code                           */~
            CH(9),          /* Bill-to X-Ref                           */~
            CH(9),          /* Customer code                           */~
            CH(8),          /* Invoice Number                          */~
            CH(192),        /* Filler (Internal, unused space)         */~
            CH(6),          /* Session ID                              */~
            CH(1),          /* Invoice Type                            */~
            CH(9),          /* Customer Code                           */~
            CH(8)           /* Invoice Number                          */

L31170: FMT                 /* FILE #10 -- ARIBUF2                     */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Invoice Number                          */~
            CH(3),          /* General purpose sequence number         */~
            CH(3),          /* Purchase Order Line Number              */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part description                        */~
            CH(4),          /* Category Code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped                        */~
            PD(14,4),       /* Open Quantity                           */~
            PD(14,4),       /* Unit Price- at Stocking UOM             */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (buy to sell)         */~
            PD(14,4),       /* Unit Price at Pricing UOM               */~
            PD(14,4),       /* Line Item Discount                      */~
            PD(14,4),       /* Discount Amount                         */~
            PD(14,4),       /* Extension                               */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(9),          /* Sales Account Number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Project Number                          */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* General purpose sequence number         */~
            30*CH(6),       /* Lot Number                              */~
            30*PD(14,4),    /* Quantity in corresponding lot           */~
            CH(1),          /* Non-Stock Flag (Y=Non-Stock else blank) */~
            CH(133)         /* Filler (Internal, unused space)         */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%, edit%)
                  init(hex(8c)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  on fieldnr% gosub L40170,         /* FC Date          */~
                                    L40170,         /* Store            */~
                                    L40170          /* FC Table(s)      */
                  gosub setpf1
                  goto L40240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02), "Automatic Finance Charge Generation",        ~
               at (01,62), "Post Date: ",                                ~
               at (01,73), fac(hex(8c)), postdate$              , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "FC (Invoice) Date",                          ~
               at (06,30), fac(lfac$( 1)), fc_date$             , ch(08),~
                                                                         ~
               at (07,02), "Store (for Invoice #s)",                     ~
               at (07,30), fac(lfac$( 2)), store$               , ch(03),~
               at (07,49), fac(hex(8c)),   storedescr$          , ch(30),~
                                                                         ~
               at (08,02), "Finance Charge Table",                       ~
               at (08,30), fac(lfac$( 3)), tables$              , ch(03),~
               at (08,49), fac(hex(8c)),   tablesdescr$         , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                   keys(pfkeys$), key(keyhit%)


               if keyhit% <> 13 then L40650
                  call "MANUAL" ("ARIFCGEN")
                  goto L40240

L40650:        if keyhit% <> 15 then L40685
                  call "PRNTSCRN"
                  goto L40240

L40685:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf1
        if edit% = 2% then L40775         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ffffff00)
           return

L40775:  if fieldnr% > 0% then L40830     /* Edit Mode- Select Field    */
           inpmessage$ = edtmessage$
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)GENERATE FCs"
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ffffff00)
           return

                                         /* Edit Mode- Field Enabled   */
L40830:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* FC Date          */~
                                    L50200,         /* Store            */~
                                    L50400          /* FC Table(s)      */
                  return


L50100
*        Test data for FINANCE CHARGE DATE
            call "DATEOK" (fc_date$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (fc_date$)
                if fc_date$ > date then errormsg$ =                      ~
                     " Finance Charge Date may not be after Today."
                call "DATEFMT" (fc_date$)
                return

L50200
*        Test data for STORE CODE
            call "GETCODE" (#01, store$, storedescr$, 0%, 0, f1%(1))
            if f1%(1) = 1% then return
                errormsg$ = "Invalid Store Code."
                return


L50400
*        Test data for FC TABLE
            tablesdescr$ = " "
            if tables$ = "ALL" then return
                if len(tables$) > 1% then L50440
                syskey$ = "FINANCECHARGETABLE" & tables$
                call "DESCRIBE" (#04, syskey$, tablesdescr$, 0%, f1%(4))
                if f1%(4) = 1% then return
L50440:              errormsg$ = "Invalid Finance Charge Table"
                     return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One moment, please")
            u3% = 2%
            call "UPDUSRLG" ("ARIUPDTE", " ", " ", " ", session$, u3%,   ~
                                                                " ", " ")
            end
