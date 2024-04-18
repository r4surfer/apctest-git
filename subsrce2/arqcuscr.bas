        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   AAA   RRRR    QQQ    CCC   U   U   SSS    CCC   RRRR    *~
            *  A   A  R   R  Q   Q  C   C  U   U  S      C   C  R   R   *~
            *  AAAAA  RRRR   Q   Q  C      U   U   SSS   C      RRRR    *~
            *  A   A  R   R  Q Q Q  C   C  U   U      S  C   C  R   R   *~
            *  A   A  R   R   QQQ    CCC    UUU    SSS    CCC   R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARQCUSCR - Gathers, computes and displays the Customer    *~
            *            Credit information for a Bill-To customer.     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/12/92 ! Original                                 ! JIM *~
            * 09/23/94 ! Resetting Avg Days ONLY from CUSINPUT.   ! JDH *~
            * 09/27/95 ! Corrected calc of net sales figure.      ! JDH *~
            *          ! Added ability to see Bill-to info, if    !     *~
            *          !  customer is a ship-to.                  !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "ARQCUSCR" (billto$)

        dim                                                              ~
            aging%(10),                  /* Aging Parameters           */~
            ageper$1,                    /* Aging Per Disc or Net Date */~
            asof$6,                      /* As Of Date                 */~
            avg_days$5,                  /* Average Days to Pay        */~
            billopen$14,                 /* Bill-To Open Order Amount  */~
            billto$9,                    /* Requested Bill-To Customer */~
            bill_to$9,                   /* Real Bill-To Customer      */~
                                         /* BILLTO$ is a misnomer;     */~
                                         /*  it stand for the customer */~
                                         /*  passed in from caller.    */~
            cbalance$14,                 /* Current Balance            */~
            contpers$20,                 /* Contact Person             */~
            crlimit$12,                  /* Credit Limit               */~
            cursor%(2),                  /* Cursor location for edit   */~
            curr$1,                      /* Multi-Currency Yes or No?  */~
            customer$9,                  /* Save for Bill-to view reset*/~
            date$8,                      /* Date for screen display    */~
            dsply$(15)79,                /* Screen display lines       */~
            hicrlimt$12, hicrdate$8,     /* High Credit Limit & Date   */~
            high_a_r$14, hiardate$8,     /* High A/R & Date            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Screen Line #2             */~
            line4$79,                    /* Screen Line #4             */~
            line5$79,                    /* Screen Line #5             */~
            linvamnt$13,                 /* Last Invoice Amount        */~
            linvdate$8,                  /* Last Invoice Date          */~
            linvnmbr$8,                  /* Last Invoice Number        */~
            lpayamnt$13,                 /* Last Payment Amount        */~
            lpaychek$10,                 /* Last Payment Check #       */~
            lpaydate$8,                  /* Last Payment Date          */~
            moduser$3, moddate$8,        /* Last modified by ... on    */~
            nbrpmnts$5,                  /* # Payments in Average Days */~
            netsales$14,                 /* Tot Sales - Tot Credits    */~
            nsf$3,                       /* Edited # of NSFs           */~
            nsf_amnt(100), nsf_amnt$12,  /* NSF Check Amount           */~
            nsf_atot(1),                 /* NSF Check Amount total     */~
            nsf_chek$(100)10,            /* NSF Check Number           */~
            nsf_date$(100)8,             /* NSF Check Date             */~
            nsf_dtpd$(100)8,             /* Date Paid                  */~
            nsf_invc$(100)8,             /* Invoice Number             */~
            nsf_net$15,                  /* Net of NSFs outstanding    */~
            nsf_ones(1, 100),            /* NSF $ total work area      */~
            nsf_paid(100), nsf_paid$12,  /* NSF Check Amount paid      */~
            nsf_ptot(1),                 /* NSF Check Amount paid total*/~
            nsf_sqnc$(100)3,             /* CCRLINES Key Sequence #    */~
            nsfsummy$13,                 /* Total Outstanding NSF $$$  */~
            opendate$8,                  /* Date Account Opened        */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$33,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prog$8,                      /* Calling Program            */~
            shipopen$14,                 /* Ship-To Open Order Amount  */~
            shipto$(6)31,                /* Ship-To Name & Address     */~
            soldto$(6)31,                /* Sold-To Name & Address     */~
            sortname$32,                 /* CUSTOMER file Sort Name    */~
            stat$4,                      /* Statutory Currency code    */~
            telephon$10,                 /* Telephone                  */~
            termcode$20, termdesc$32,    /* Terms Code & Description   */~
            totcredt$14,                 /* Total Credits to Date      */~
            totsales$14,                 /* Total Sales to Date        */~
            userid$3                     /* Current User Id            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
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
            * #01 ! CUSTOMER ! Customer Master File                     *~
            * #02 ! CCRMASTR ! Customer Credit Master file              *~
            * #03 ! CCRLINES ! Customer Credit Line Items (NSFs)        *~
            * #04 ! SYSFILE2 ! System File                              *~
            * #10 ! ARMTERMS ! A/R Payment Terms                        *~
            * #11 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup,    ~
                            key  6, keypos = 1189, keylen =   3, dup

            select #02, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #03, "CCRLINES",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    1, keylen =   12

            select #04, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 20

            select #10, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   1,  keylen = 20

            select #11, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen = 21

            if beenherebefore% <> 0% then goto L09280
*        One-time-only initializations.
                beenherebefore% = 1%
                call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
                call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
                call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
                call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
                call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
                call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Further one-time-only initializations.
            call "EXTRACT" addr("ID", userid$, "CF", prog$)
            date$ = date : call "DATEFMT" (date$)
            str(line2$,62%) = "ARQCUSCR: " & str(cms2v$,,8%)
            l% = dim(dsply$(), 1%)   /* # NSF display lines on summary */
            line5$ = "Seq NSF Date   Check Amt Check#     Invoice#    A"&~
                "mt Paid  Date Pd  Outst. Amt"
            mat nsf_ones = con           /* All 1's for MAT Multiplies */
            aging%(1%) =   0% : aging%( 2%) =  30% : aging%(3%) =  60%
            aging%(4%) =  90% : aging%(10%) =   4%
            aging%(8%) = -99999% : aging%(5%), aging%(9%) = 99999%
            asof$ = date
            ageper$ = "N"

*        Check for Multi-Currency usage and get STATUTORY currency code
            curr$ = "N"
            stat$ = " "
            call "READ100" (#04, "SWITCHS.CUR", f1%(4%))
            if f1%(4%) = 0% then L09280       /* Multi-Currency not used */
                get #2 using L09260, curr$, stat$
L09260:             FMT POS(21), CH(1), CH(4)

L09280
*        Perform the following initializations each & every time.
        reset_start
            arqtbsub% = 0%

        REM *************************************************************~
            *    A L L   T H E   W O R K   I S   D O N E   H E R E      *~
            *************************************************************

            gosub initialize_variables
            gosub access_customer_file
                if f1%(1%) = 0% then goto exit_program     /* Disaster */
            gosub access_credit_files

        display_main_screen
            gosub L40000                   /* Display the gathered data */
            if keyhit% =  2% then goto display_nsf_details
            if keyhit% =  8% then gosub reset_avg_to_zero
            if keyhit% = 10% then gosub summary_aging
            if keyhit% = 11% then goto display_sold_ship
            if keyhit% = 16% and bill_to_mode% = 1% then goto reset_cust
            if keyhit% = 16% then goto exit_program
            if keyhit% = 26% then goto  see_bill_to_data
            goto display_main_screen

        display_nsf_details
            p% = 1%                        /* Begin at first NSF event */
        display_the_nsfs
            gosub L42000
            if keyhit% =  2% then p% = 1%                /* First page */
            if keyhit% =  3% then p% = z%                 /* Last page */
            if keyhit% =  4% then p% = max(1%, p%-l%) /* Previous page */
            if keyhit% =  5% then p% = min(z%, p%+l%)     /* Next page */
            if keyhit% =  6% then p% = max(1%, p%-1%)          /* Down */
            if keyhit% =  7% then p% = min(z%, p%+1%)            /* Up */
            if keyhit% = 10% then gosub summary_aging
            if keyhit% = 11% then goto display_sold_ship
            if keyhit% = 16% then goto display_main_screen
            goto display_the_nsfs

        display_sold_ship
            gosub L44000
            if keyhit% =  2% then goto display_nsf_details
            if keyhit% = 10% then gosub summary_aging
            if keyhit% = 16% then goto display_main_screen
            goto display_sold_ship

        summary_aging
            call "ARQTBSUB" (billto$, aging%(), asof$, ageper$, #11, #01,~
                "N", arqtbsub%, " ", " ", " ", curr$, stat$, " ", " ",   ~
                " ")
            return

        reset_avg_to_zero
            avg_days$, nbrpmnts$ = "    0"
            avgdays = 0 : nbrpmnts% = 0%
            call "READ101" (#02, billto$, f1%(2%))         /* CCRMASTR */
                if f1%(2%) = 0% then return               /* Disaster! */
            put #02 using L35355, 0, 0%
            rewrite #02
            return

        see_bill_to_data
*        Set customer as real Bill-to and start again
            customer$ = billto$
            billto$ = bill_to$
            bill_to_mode% = 1%
            goto reset_start

        reset_cust
*        Reset customer to original and start again
            billto$ = customer$
            bill_to_mode% = 0%
            goto reset_start

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        access_customer_file
            call "READ100" (#01, billto$, f1%(1%))         /* CUSTOMER */
                if f1%(1%) = 0% then return                /* Disaster */
            get #01 using L35040, sortname$, soldto$(), opendate$,        ~
                shipto$(), contpers$, telephon$, crlimit, termcode$,     ~
                bill_to$
            if bill_to$ = billto$ then bill_to$ = " "
            if str(shipto$(6%),17%,1%) <> " " or str(shipto$(6%),16%,1%) ~
                <> " " or pos(str(shipto$(6%),27%,4%) = " ") > 0%        ~
                     then goto L15150
            temp$ = str(shipto$(6%),27%,4%)
                str(shipto$(6%),28%,4%) = temp$
                str(shipto$(6%),27%,1%) = "-"
L15150:     if str(soldto$(6%),17%,1%) <> " " or str(soldto$(6%),16%,1%) ~
                <> " " or pos(str(soldto$(6%),27%,4%) = " ") > 0%        ~
                     then goto L15210
            temp$ = str(soldto$(6%),27%,4%)
                str(soldto$(6%),28%,4%) = temp$
                str(soldto$(6%),27%,1%) = "-"
L15210:     call "LINSMASH" (soldto$())
            call "LINSMASH" (shipto$())
            call "PUTPAREN" (sortname$)
            str(line2$,,61%) = "Customer: " & billto$ & " " & sortname$
            call "DATEFMT" (opendate$)
            call "READ100" (#10, termcode$, f1%(10%))      /* ARMTERMS */
            if f1%(10%) = 0%                                             ~
                then termdesc$ = "Unknown Terms Code"                    ~
                else get #10 using L15300, termdesc$
L15300:              FMT POS(21), CH(30)
            call "PUTPAREN" (termdesc$)
            convert round(crlimit ,0) to crlimit$,  pic (####,###,###)
            return

        access_credit_files
            nsf% = 0%           /* Initialize number of NSF line items */
            call "READ100" (#02, billto$, f1%(2%))         /* CCRMASTR */
                if f1%(2%) = 0% then goto access_credit_files_exit
            get #02 using L35140, totsales, totcredt, hicrlimt, hicrdate$,~
                linvamnt, linvnmbr$, lpayamnt, lpaychek$, avgdays,       ~
                nbrpmnts%, linvdate$, lpaydate$, hiardate$, billopen,    ~
                shipopen, cbalance, high_a_r, moduser$, moddate$
            call "STRING" addr ("RJ", linvnmbr$, len(str(linvnmbr$)))
            call "STRING" addr ("RJ", lpaychek$, len(str(lpaychek$)))
            convert totsales to totsales$, pic (###,###,###.##)
            convert totcredt to totcredt$, pic (###,###,###.##)
            convert round(hicrlimt, 0) to hicrlimt$, pic (####,###,###)
            convert linvamnt to linvamnt$, pic (##,###,###.##)
            convert lpayamnt to lpayamnt$, pic (##,###,###.##)
            convert int(avgdays) to avg_days$, pic (####0)
            convert nbrpmnts% to nbrpmnts$, pic (####0)
            call "DATEFMT" (hicrdate$)
            call "DATEFMT" (linvdate$)
            call "DATEFMT" (lpaydate$)
            call "DATEFMT" (hiardate$)
            call "DATEFMT" (moddate$)
            netsales = totsales + totcredt  /* CRs stored as Negative */
            if netsales < 0                                              ~
                then convert netsales to netsales$, pic (-##,###,###.##) ~
                else convert netsales to netsales$, pic (###,###,###.##)
            convert round(cbalance,2) to cbalance$, pic (-##,###,###.##)
            convert round(high_a_r,2) to high_a_r$, pic (-##,###,###.##)
            convert round(billopen,2) to billopen$, pic (-##,###,###.##)
            convert round(shipopen,2) to shipopen$, pic (-##,###,###.##)

*        If there are line items (NSF events), put 'em in the arrays.
            plowkey$ = xor plowkey$
            str(plowkey$,,9%) = billto$

        plow_the_nsfs
            call "PLOWNEXT" (#03, plowkey$, 9%, f1%(3%))   /* CCRLINES */
                if f1%(3%) = 0% then goto access_credit_files_exit
            nsf% = nsf% + 1%
            if nsf% <= dim(nsf_amnt(), 1%) then goto L15760
                nsf% = nsf% - 1%
                goto access_credit_files_exit
L15760:     get #03 using L35360, nsf_sqnc$(nsf%), nsf_date$(nsf%),       ~
                nsf_chek$(nsf%), nsf_invc$(nsf%), nsf_amnt(nsf%),        ~
                nsf_paid(nsf%), nsf_dtpd$(nsf%)
            call "DATEFMT" (nsf_date$(nsf%))
            call "DATEFMT" (nsf_dtpd$(nsf%))
            goto plow_the_nsfs

        access_credit_files_exit    /* The ONLY exit from here, please */
            z% = max(1%, nsf% - l% + 1%)  /* Highest NSF top of screen */
            convert nsf% to nsf$, pic(###)
            call "STRING" addr ("LJ", nsf$, len(str(nsf$)))
            mat nsf_atot = nsf_ones * nsf_amnt
            mat nsf_ptot = nsf_ones * nsf_paid
            nsf_net = nsf_atot(1%) - nsf_ptot(1%)
            if nsf_net < 0                                               ~
                then convert nsf_net to nsf_net$, pic (-$##,###,###.##)  ~
                else convert nsf_net to nsf_net$, pic ($###,###,###.##)
            if nsf_net < 0                                               ~
                then convert nsf_net to nsfsummy$, pic (-#,###,###.##)   ~
                else convert nsf_net to nsfsummy$, pic (##,###,###.##)
            call "STRING" addr ("LJ", nsf_net$, len(str(nsf_net$)))
            str(line4$,,61%) = nsf$ & " NSF Check items; " & nsf_net$ &  ~
                " outstanding"
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") inpmessage$, avg_days$, linvdate$, cbalance$,      ~
                contpers$, crlimit$, hicrlimt$, hicrdate$, high_a_r$,    ~
                hiardate$, linvamnt$, linvdate$, linvnmbr$, lpayamnt$,   ~
                lpaychek$, lpaydate$, netsales$, nsf$, nsf_amnt$,        ~
                nsf_chek$(), nsf_date$(), nsf_dtpd$(), nsf_invc$(),      ~
                nsf_net$, nsf_paid$, nsf_sqnc$(), nsfsummy$, opendate$,  ~
                shipto$(), soldto$(), sortname$, telephon$, termcode$,   ~
                termdesc$, totcredt$, totsales$, nbrpmnts$, bill_to$
            mat nsf_amnt = zer
            mat nsf_paid = zer
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *        R E C O R D   L A Y O U T   F O R M A T S          *~
            *************************************************************

L35040:     FMT /* File #01- CUSTOMER Master file (partial)            */~
                POS(10), CH(30),         /* Sort Name                  */~
                POS(40), 6*CH(30),       /* Sold To                    */~
                POS(220), CH(6),         /* Date Account Opened        */~
                POS(253), 6*CH(30),      /* Ship To                    */~
                POS(433), CH(20),        /* Customer Contact           */~
                POS(453), CH(10),        /* Telephone Number           */~
                POS(526), PD(14,4),      /* Credit Limit               */~
                POS(543), CH(20),        /* Terms Code                 */~
                POS(780), CH(9)          /* Bill To Customer           */

L35140:     FMT /* File #02- CCRMASTR Master file                      */~
                XX(9),         /*   1/ 9-   Customer Code (Key)        */~
                PD(14,4),      /*  10/ 8-   Total Sales                */~
                PD(14,4),      /*  18/ 8-   Total Credits              */~
                PD(14,4),      /*  26/ 8-   High Credit Limit          */~
                CH(6),         /*  34/ 6-   High Credit Limit Date     */~
                PD(14,4),      /*  40/ 8-   Last Invoice Amount        */~
                CH(8),         /*  48/ 8-   Last Invoice Number        */~
                PD(14,4),      /*  56/ 8-   Last Payment Amount        */~
                CH(10),        /*  64/10-   Last Payment Check #       */~
                PD(14,4),      /*  74/ 8-   Average # Days to Pay      */~
                BI(2),         /*  82/ 2-   # Payments in Average Days */~
                               /*  84/30-   'Dynamic' dates fr CUSTOMER*/~
                XX(6),                   /* Last Update by Computer    */~
                CH(6),                   /* Last Invoice Date          */~
                CH(6),                   /* Last Payment Date          */~
                XX(6),                   /* Date Last Changed          */~
                CH(6),                   /* High A/R Balance Date      */~
                4*PD(14,4),    /* 114/32-   B/T OO, S/T OO, Open, High */~
                CH(3),         /* 146/ 3-   User Last Modified         */~
                CH(6),         /* 149/ 6-   Date Last Modified         */~
                CH(46)         /* 155/46-   Filler                     */

L35355:     FMT /* File #02- CCRMASTR (Reset Avg Days & # Payments)    */~
                POS(74), PD(14,4), BI(2)

L35360:     FMT /* File #03- CCRLINES NSF Event file                   */~
                XX(9),                   /* Customer Code (Key)        */~
                CH(3),                   /* Key tie-breaker (Seq #)    */~
                CH(6),                   /* NSF Check Date             */~
                CH(10),                  /* NSF Check Number           */~
                CH(8),                   /* NSF Check Invoice #        */~
                PD(14,4),                /* NSF Check Amount           */~
                PD(14,4),                /* NSF Amount Check Paid      */~
                CH(6),                   /* NSF Date Check Paid        */~
                XX(3),                   /* User Last Modified         */~
                XX(6),                   /* Date Last Modified         */~
                XX(33)                   /* Filler                     */

L40000: REM *************************************************************~
            *           M A I N   S C R E E N   P A G E   1             *~
            *************************************************************

            gosub set_pf1

L40060:     accept                                                       ~
                at (01,02), "Customer Credit Information- Main Screen",  ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                at (02,02), fac(hex(ac)),   line2$              , ch(79),~
                                                                         ~
                at (05,02), "Date Account Opened",                       ~
                at (05,24), fac(hex(84)),   opendate$           , ch(08),~
                                                                         ~
                at (06,02), "Last Payment Amt",                          ~
                at (06,19), fac(hex(84)),   lpayamnt$           , ch(13),~
                at (06,34), "Check Number",                              ~
                at (06,47), fac(hex(84)),   lpaychek$           , ch(10),~
                at (06,59), "Check Date",                                ~
                at (06,73), fac(hex(84)),   lpaydate$           , ch(08),~
                                                                         ~
                at (07,02), "Total Sales",                               ~
                at (07,18), fac(hex(84)),   totsales$           , ch(14),~
                at (07,34), "Credits",                                   ~
                at (07,43), fac(hex(84)),   totcredt$           , ch(14),~
                at (07,59), "Net",                                       ~
                at (07,67), fac(hex(84)),   netsales$           , ch(14),~
                                                                         ~
                at (08,02), "Outstanding NSFs",                          ~
                at (08,19), fac(hex(84)),   nsfsummy$           , ch(13),~
                at (08,34), "# of NSF items",                            ~
                at (08,56), fac(hex(84)),   nsf$                , ch(03),~
                                                                         ~
                at (09,02), "Average Days to Pay",                       ~
                at (09,27), fac(hex(84)),   avg_days$           , ch(05),~
                at (09,34), "Applied Payments",                          ~
                at (09,52), fac(hex(84)),   nbrpmnts$           , ch(05),~
                                                                         ~
                at (10,02), "Credit Limit",                              ~
                at (10,20), fac(hex(84)),   crlimit$            , ch(12),~
                at (10,34), "High Limit",                                ~
                at (10,45), fac(hex(84)),   hicrlimt$           , ch(12),~
                at (10,59), "High C/L Date",                             ~
                at (10,73), fac(hex(84)),   hicrdate$           , ch(08),~
                                                                         ~
                at (11,02), "Last Invoice Amt",                          ~
                at (11,19), fac(hex(84)),   linvamnt$           , ch(13),~
                at (11,34), "Invoice Number",                            ~
                at (11,49), fac(hex(84)),   linvnmbr$           , ch(08),~
                at (11,59), "Invoice Date",                              ~
                at (11,73), fac(hex(84)),   linvdate$           , ch(08),~
                                                                         ~
                at (12,02), "Current Balance",                           ~
                at (12,18), fac(hex(84)),   cbalance$           , ch(14),~
                at (12,34), "High A/R",                                  ~
                at (12,43), fac(hex(84)),   high_a_r$           , ch(14),~
                at (12,59), "High A/R Date",                             ~
                at (12,73), fac(hex(84)),   hiardate$           , ch(08),~
                                                                         ~
                at (13,02), "Bill To Orders",                            ~
                at (13,18), fac(hex(84)),   billopen$           , ch(14),~
                at (13,34), "Ship To",                                   ~
                at (13,43), fac(hex(84)),   shipopen$           , ch(14),~
                                                                         ~
                at (14,02), "Terms Code",                                ~
                at (14,20), fac(hex(84)),   termcode$           , ch(20),~
                at (14,42), fac(hex(84)),   termdesc$           , ch(32),~
                                                                         ~
                at (15,02), "Contact Person",                            ~
                at (15,20), fac(hex(84)),   contpers$           , ch(20),~
                at (15,42), "Phone",                                     ~
                at (15,49), fac(hex(84)),   str(telephon$,1%,3%), ch(03),~
                at (15,53), fac(hex(84)),   str(telephon$,4%,3%), ch(03),~
                at (15,57), fac(hex(84)),   str(telephon$,7%,4%), ch(04),~
                                                                         ~
                at (19,02), "Last Modified on",                          ~
                at (19,19), fac(hex(8c)),   moddate$            , ch(08),~
                at (19,28), "by",                                        ~
                at (19,31), fac(hex(8c)),   moduser$            , ch(03),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                at (24,19), fac(hex(84)),   bill_to$            , ch(09),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L40880
                call "MANUAL" ("ARQCUSCR") : goto L40060

L40880:     if keyhit% <> 15% then L40910
                call "PRNTSCRN" : goto L40060

L40910:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
            pf$(1%) = "                              (8)Reset A" &       ~
                      "vg Days to Zero        (13)Instructions"
            pf$(2%) = "(2)See NSF Details           (10)See Sum" &       ~
                      "mary Aging             (15)Print Screen"
            pf$(3%) = "(26)See Bill-To:             (11)See Sol" &       ~
                      "d-To & Ship-To         (16)Return      "
            pfkeys$ = hex(ff02ffffffffff08ff0a0bff0dff0f101a)
            if nsf% <> 0% then goto L41050
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,2%,1%) = hex(ff)
L41050:     if prog$ <> "CUSINPUT" then L41060
            if avgdays <> 0 or nbrpmnts% <> 0% then goto L41070
L41060:         str(pf$(1%),31%,25%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41070:     if bill_to$ <> " " then L41090
                str(pf$(3%), 1%,16%) = " " : str(pfkeys$,17%,1%) = hex(ff)
L41090:     return

L42000: REM *************************************************************~
            *    S C R E E N   P A G E   2-   N S F   D E T A I L S     *~
            *************************************************************

            gosub set_pf2

            init (" ") dsply$()
            for t% = 1% to min(l%, nsf%)
                str(dsply$(t%), 1%, 3%) = nsf_sqnc$(p%+t%-1%)
                str(dsply$(t%), 5%, 8%) = nsf_date$(p%+t%-1%)
                convert nsf_amnt(p%+t%-1%) to str(dsply$(t%),13%,12%),   ~
                     pic (#########.##)
                str(dsply$(t%),26%,10%) = nsf_chek$(p%+t%-1%)
                str(dsply$(t%),37%, 8%) = nsf_invc$(p%+t%-1%)
                convert nsf_paid(p%+t%-1%) to str(dsply$(t%),45%,12%),   ~
                     pic (#########.##)
                str(dsply$(t%),58%, 8%) = nsf_dtpd$(p%+t%-1%)
                if nsf_amnt(p%+t%-1%) - nsf_paid(p%+t%-1%) < 0           ~
                    then convert nsf_amnt(p%+t%-1%) - nsf_paid(p%+t%-1%) ~
                        to str(dsply$(t%),66%,12%), pic (-########.##)   ~
                    else convert nsf_amnt(p%+t%-1%) - nsf_paid(p%+t%-1%) ~
                        to str(dsply$(t%),66%,12%), pic (#########.##)
            next t%

L42240:     accept                                                       ~
                at (01,02), "Customer Credit Information- NSF Check Summa~
        ~ry",                                                             ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(8c)), line4$                , ch(79),~
                at (05,02), fac(hex(ac)), line5$                , ch(79),~
                                                                         ~
                at (06,02), fac(hex(8c)), dsply$( 1%)           , ch(79),~
                at (07,02), fac(hex(8c)), dsply$( 2%)           , ch(79),~
                at (08,02), fac(hex(8c)), dsply$( 3%)           , ch(79),~
                at (09,02), fac(hex(8c)), dsply$( 4%)           , ch(79),~
                at (10,02), fac(hex(8c)), dsply$( 5%)           , ch(79),~
                at (11,02), fac(hex(8c)), dsply$( 6%)           , ch(79),~
                at (12,02), fac(hex(8c)), dsply$( 7%)           , ch(79),~
                at (13,02), fac(hex(8c)), dsply$( 8%)           , ch(79),~
                at (14,02), fac(hex(8c)), dsply$( 9%)           , ch(79),~
                at (15,02), fac(hex(8c)), dsply$(10%)           , ch(79),~
                at (16,02), fac(hex(8c)), dsply$(11%)           , ch(79),~
                at (17,02), fac(hex(8c)), dsply$(12%)           , ch(79),~
                at (18,02), fac(hex(8c)), dsply$(13%)           , ch(79),~
                at (19,02), fac(hex(8c)), dsply$(14%)           , ch(79),~
                at (20,02), fac(hex(8c)), dsply$(15%)           , ch(79),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L42590
                call "MANUAL" ("ARQCUSCR") : goto L42240

L42590:     if keyhit% <> 15% then L42620
                call "PRNTSCRN" : goto L42240

L42620:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf2
            pf$(1%) = "(2)1st Page    (5)Next Page             " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(3)Last Page   (6)Down       (10)See Sum" &       ~
                      "mary Aging             (15)Print Screen"
            pf$(3%) = "(4)Prev Page   (7)Up         (11)See Sol" &       ~
                      "d-To & Ship-To         (16)Main Screen "
            pfkeys$ = hex(ff020304050607ffff0a0bff0dff0f10ff)
            if p% <> 1% then goto L42790
                str(pf$(1%),,11%), str(pf$(3%),1%,12%),                  ~
                     str(pf$(2%),16%,7%) = " "
                str(pfkeys$,2%,1%), str(pfkeys$,4%,1%),                  ~
                     str(pfkeys$,6%,1%) = hex(ff)
L42790:     if p% <> z% then goto L42840
                str(pf$(2%),,12%), str(pf$(1%),16%,12%),                 ~
                     str(pf$(3%),16%,5%) = " "
                str(pfkeys$,3%,1%), str(pfkeys$,5%,1%),                  ~
                     str(pfkeys$,7%,1%) = hex(ff)
L42840:     inpmessage$ = " "
            return

L44000: REM *************************************************************~
            *  S C R E E N   P A G E   3-   S O L D   &   S H I P   T O *~
            *************************************************************

            gosub set_pf3

L44060:     accept                                                       ~
                at (01,02), "Customer Credit Information- Sold To, Ship T~
        ~o",                                                              ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                                                                         ~
                at (04,02), "Ship",                                      ~
                at (04,08), fac(hex(8c)),   shipto$(1%)         , ch(31),~
                at (05,04), "to",                                        ~
                at (05,08), fac(hex(8c)),   shipto$(2%)         , ch(31),~
                at (06,08), fac(hex(8c)),   shipto$(3%)         , ch(31),~
                at (07,08), fac(hex(8c)),   shipto$(4%)         , ch(31),~
                at (08,08), fac(hex(8c)),   shipto$(5%)         , ch(31),~
                at (09,08), fac(hex(8c)),   shipto$(6%)         , ch(31),~
                                                                         ~
                at (04,42), "Sold",                                      ~
                at (04,48), fac(hex(8c)),   soldto$(1%)         , ch(31),~
                at (05,44), "to",                                        ~
                at (05,48), fac(hex(8c)),   soldto$(2%)         , ch(31),~
                at (06,48), fac(hex(8c)),   soldto$(3%)         , ch(31),~
                at (07,48), fac(hex(8c)),   soldto$(4%)         , ch(31),~
                at (08,48), fac(hex(8c)),   soldto$(5%)         , ch(31),~
                at (09,48), fac(hex(8c)),   soldto$(6%)         , ch(31),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L44410
                call "MANUAL" ("ARQCUSCR") : goto L44060

L44410:     if keyhit% <> 15% then L44440
                call "PRNTSCRN" : goto L44060

L44440:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf3
            pf$(1%) = "                                        " &       ~
                    "                       (13)Instructions"
            pf$(2%) = "(2)See NSF Details           (10)See Sum" &       ~
                      "mary Aging             (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Main Screen "
            pfkeys$ = hex(ff02ffffffffffffff0affff0dff0f10ff)
            if nsf% <> 0% then goto L44580
                str(pf$(2%),1%,18%) = " " : str(pfkeys$,2%,1%) = hex(ff)
L44580:     return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "ALLFREE"
            end
