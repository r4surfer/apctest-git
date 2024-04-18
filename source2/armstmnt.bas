        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M   SSS   TTTTT  M   M  N   N  TTTTT   *~
            *  A   A  R   R  MM MM  S        T    MM MM  NN  N    T     *~
            *  AAAAA  RRRR   M M M   SSS     T    M M M  N N N    T     *~
            *  A   A  R   R  M   M      S    T    M   M  N  NN    T     *~
            *  A   A  R   R  M   M   SSS     T    M   M  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMSTMNT - Prints A/R Customer Statements.                *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of Caelus, Inc., Spokane, WA, embodying       *~
            * substantial creative efforts  and confidential            *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1986, an unpublished work by Caelus, Inc.,  *~
            * Spokane, WA.  All rights reserved.                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/23/86 ! Original                                 ! ERN *~
            * 04/12/88 ! Added '-' in ZIP Code  eg. 12345-6789    ! MJB *~
            * 04/22/88 ! Fixed up BF print, added detail option   ! ERN *~
            * 05/05/88 ! Allow statement cut off to = last settle ! HES *~
            * 05/05/88 ! Altered MIN_BAL$ logic, adding NON_ZERO  ! HES *~
            * 06/09/88 ! Added age per selection                  ! ERN *~
            * 06/24/88 ! Added Continued... for multi page stmnt  ! MJB *~
            * 10/10/88 ! Deleted line that made MIN_BAL$ disappear! JDH *~
            * 04/12/89 ! Mods to Support Multi-Currency           ! RJM *~
            * 11/10/89 ! Rewrote MC statement generation          ! MJB *~
            * 11/22/89 ! Zip fmt & adjusted print criteria.       ! JDH *~
            * 02/27/92 ! PRR 12170  Fix cust code printed on last ! JDH *~
            *          !   page when only Total Balance prints.   !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging, etc ! KAB *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$(2)12, acctu$(2)12,     /* Selected A/R Account       */~
            acctie$1, acct$9,            /* Selected A/R Account       */~
            addr$(6)31,                  /* Bill-to Address            */~
            ageamts$(9)15,               /* FORMATED BALANCES          */~
            ageper$1,                    /* D/N/I                      */~
            aging%(10), aging$(7)20,     /* Aging Parameters           */~
            asof$8, asofu$8,             /* As Of Date                 */~
            bals(3),                     /* Item Balances in Statutory */~
            baltype$1,                   /* Customer Balance Type      */~
            billto$9, billto(9),         /* Bill-to Code and Balances  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cbals(3),                    /* Item Balances in Currency  */~
            cdate$6,                     /* CURRENCY EFFECTIVITY DATE  */~
            compaddr$(4)30,              /* Company Name and Address   */~
            cursor%(2),                  /* Cursor location for edit   */~
            curcode$4,                   /* Test Currency Code         */~
            curr$1,                      /* Multi-Currency Inuse Flag  */~
            currage$(50)76, currage(9),  /* Currency & aged amts array */~
            currency$4,                  /* Current Currency Code      */~
            currdesc$32,                 /* CURRENCY CODE DESCRIPTION  */~
            cus_range$(4)9,              /* Bill-to Customer Range     */~
            date$8,                      /* Date for screen display    */~
            desc$40,                     /* PRINTING TEXT              */~
            docdate$8,                   /* Source Doc Date            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            item_amt$13,                 /* FORMATED ITEM AMOUNT       */~
            last_stl$8,                  /* Last Settling Date         */~
            last_stmnt$8, last_stmntu$8, /* Date of Last Statements    */~
            lastcust$9,                  /* Last Customer Code         */~
            lastcurr$9,                  /* Last Currency Code         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            mccurr$4,                    /* Another Currency Code var  */~
            min_bal$10,                  /* Minimum Balance            */~
            netdue$8, netdue00$6,        /* Srce Doc Net Due Date      */~
            pastdue$1,                   /* Past Due Items Only?       */~
            pf16$16, pf4$18, pf8$26,     /* PF Key Literals            */~
            plowkey$50,                  /* A Plow Key                 */~
            po$16,                       /* Customer PO Number         */~
            postdate$6,                  /* Item Post Date             */~
            print$1,                     /* Customer Print Flag        */~
            readkey$50,                  /* Read Key                   */~
            srcedoc$8,                   /* Source Document Number     */~
            saveage(9),                  /* Aging Balances             */~
            save_billto$9,               /* Printing save variable     */~
            stat$4,                      /* Statutory Currency         */~
            stlmnt$14,                   /* Settlement Number          */~
            summary$1,                   /* Summary (S) or Detail (D)  */~
            type$10,                     /* Source Document Type       */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* File Read Status Flag      */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            * #1  ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * #2  ! SYSFILE2 ! System File                              *~
            * #3  ! CUSTOMER ! Customer Master                          *~
            * #4  ! GLMAIN   ! General Ledger Main                      *~
            * #5  ! CRCBUF2  ! Cash Receipts Buffer - Distribution      *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #43 ! CRCLNCUR ! Multi-Currency Line Information          *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #45 ! CRCL2CUR ! Multi-Currency Line Information          *~
            * #50 ! WORKFILE ! Sorted Workfile                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen = 21

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #4,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =   9

            select #5,  "CRCBUF2",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 21,                      ~
                        alt key  1, keypos =  201, keylen =  33

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #43, "CRCLNCUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #44, "ARMTBCEX",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #45, "CRCL2CUR",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    5, keylen = 21,                      ~
                        alt key  1, keypos =  1, keylen =  25

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 25

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))

            if min(fs%()) < 0% then exit_program

            call "OPENCHCK" (#40,  fs%(40), f2%(40), 0%, rslt$(40))
            call "OPENCHCK" (#43,  fs%(43), f2%(43), 0%, rslt$(43))
            call "OPENCHCK" (#44,  fs%(44), f2%(44), 0%, rslt$(44))
            call "OPENCHCK" (#45,  fs%(45), f2%(45), 0%, rslt$(45))

            recnbr% = val(str(rslt$(1),17,4),4)
            call "WORKOPEN" (#50, "IO   ", recnbr%, f2%(50))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


            readkey$ = "ARM.LAST.SETTLING"
            last_stl$ = blankdate$
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L09180, last_stl$
L09180:         FMT XX(20), CH(6)
            call "DATEFMT" (last_stl$)
            curr$ = "N"
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
                if f1%(2) = 1% then get #2 using L09230, curr$, stat$
L09230:             FMT POS(21), CH(1), CH(4)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf8$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, asof$, cus_range$(),       ~
                      summary$, pastdue$, min_bal$, aging$(),            ~
                      last_stmnt$, ageper$, acct$(), acctie$
            mat aging% = zer
                aging%(1) =  30% : aging%( 2) =  60% : aging%(3) =  90%
                aging%(4) = 120% : aging%(10) =   5%
                aging%(8) = -99999%  :  aging%(5), aging%(9) = 99999%


            for fieldnr% = 1% to 8%
L10170:         gosub'051(fieldnr%, 1%) /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10290
L10190:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%, 1%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10220
L10270:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10190
L10290:         gosub'151(fieldnr%, 1%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
            next fieldnr%

            gosub aging_parameters


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf4$  = " " : pf8$ = "(8)Modify Aging Parameters"
            pf16$ = "(16)Print Report"
            lastfieldnr% = 0%
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then gosub aging_parameters
                  if keyhit%  = 16 then       select_records
                  if keyhit% <>  0 then       edtpg1
L11160:     fieldnr% = min(max(1%, cursor%(1) - 5%), 9%)
            if fieldnr% = lastfieldnr% then edtpg1
            if fieldnr% <> 9% then L11220
                  gosub aging_parameters  :  goto L11300
L11220:     gosub'051(fieldnr%, 2%)     /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf8$, pf16$ = " "
L11250:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11250
            gosub'151(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11250
L11300:              lastfieldnr% = fieldnr%
                     goto L11160


        aging_parameters
            call "ARMDATES" (asofu$, "A/R SUMMARY AGING REPORT",         ~
                             aging%(), 0%, 0%, aging$(), u3%)
            if u3% <> 1% then return
                return clear all
                goto   inputmode


        REM *************************************************************~
            *             R E C O R D   S E L E C T I O N               *~
            *-----------------------------------------------------------*~
            * Select Records and write work file                        *~
            *************************************************************

        select_records
            call "SHOSTAT" ("Selecting Records for Statements")
            if min_bal$ = "ALL" then L12220
            if min_bal$ = "NON-ZERO" then L12220
               convert min_bal$ to min_bal
L12220:     last_stmntu$ = last_stmnt$ : call "DATUNFMT" (last_stmntu$)
            readkey$ = "COMPANY TITLE"
                call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0% then L12360
                     get #2 using L12320, compaddr$()
L12320:                   FMT XX(20), 4*CH(30)
                     call "LINSMASH" (compaddr$())
L12360:     select printer  (90)
            call "SETPRNT"  ("ARM005", " ", 0%, 0%)
            plowkey$ = cus_range$(3)

        select_loop
*        Get next record and test against selected customer range.
            bfbal = 0
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then print_statements
            if str(plowkey$,,9) <= cus_range$(4) then L12600
            goto print_statements

L12600
*        Now get Customer, test and age and test against criteria.
            billto$ = str(plowkey$,,9) : str(plowkey$,10) = hex(ff)
            call "READ100" (#3, billto$, f1%(3))
            if f1%(3) = 0% then select_loop
            get #3 using L12700, baltype$, print$
L12700:         FMT POS(1021), 2*CH(1)
            if print$ = "N" or print$ = "L" then select_loop
            if curr$ = "Y" then cyc% = 2% else cyc% = 0%
            call "ARMAGING" (billto$, " ", asofu$, ageper$, aging%(),    ~
                  #1, billto(), #44, #2, cyc%, currency$, currage$(),    ~
                  acctie$, acctu$(1), acctu$(2))

            if cyc% = 2% then curr_cust_loop
            if pastdue$  = "Y" and billto(9) = 0 then select_loop
            if min_bal$ = "ALL" then load_addr
                if min_bal$ <> "NON-ZERO" then L12940
                     if billto(8) <> 0 then load_addr
                         goto select_loop
L12940:         if billto(8) > min_bal then load_addr else select_loop

        curr_cust_loop
            for j% = 1% to 50%
                get str(currage$(j%),,4) using L13040, mccurr$
L13040:              FMT CH(4)
                if mccurr$ = hex(00000000) then select_loop
                get str(currage$(j%), 61, 8) using L13100, currage8
L13100:             FMT PD(14,4)
                get str(currage$(j%), 69, 8) using L13100, currage9
                if pastdue$  = "Y" and currage9 = 0 then L13260
                if min_bal$ = "ALL" then load_addr
                     if min_bal$ <> "NON-ZERO" then L13240
                        if currage8 <> 0 then load_addr
                            goto L13260
L13240:         if currage8 > min_bal then load_addr
L13260:     next j%


        load_addr
            get #3 using L13360, addr$()
L13360:         FMT POS(40), 6*CH(30)
            if str(addr$()) = "BILL-TO" then str(addr$()) = " "
            if str(addr$()) = " " then get #3 using L13420, addr$()
L13420:         FMT POS(253), 6*CH(30)
            if pos(str(addr$(6),22,9) = " ") > 0% then L13480
               addr$(6) = str(addr$(6),,26) & "-" & str(addr$(6),27,4)
L13480:     call "LINSMASH" (addr$())

*        Take care of Balance Forward Line
            if baltype$ <> "B" then end_bf_loop
            plowkey$ = str(billto$) & hex(00)
            bfbal = 0

        bf_loop
            curcode$ = " "
            call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then end_bf_loop

            get #1 using L13720, stlmnt$, acct$, postdate$
L13720:         FMT XX(9), CH(12), POS(76), CH(9), POS(97), CH(6)

            str(plowkey$,20,2) = hex(ffff)
            if str(stlmnt$,11,2) <> "00" then bf_loop

                if acct$(1) = "ALL" then L13800
                   if acctie$ = "I" then L13780
                     if acct$ < acctu$(1) then L13800
                     if acct$ > acctu$(2) then L13800
                        goto bf_loop
L13780:              if acct$ < acctu$(1) then bf_loop
                     if acct$ > acctu$(2) then bf_loop

L13800:     call "ARMCBLNC" (billto$, str(stlmnt$,,10), last_stmntu$,    ~
                             10%, "N", #1, #5, bals(), #44, #43, #45,    ~
                             curcode$, cdate$, coneqv, conunt, cbals())

            if curcode$ = " " then currency$ = stat$                     ~
                              else currency$ = curcode$
            if cyc% = 2% and currency$ <> mccurr$ then bf_loop
            bfbal = bfbal + cbals(1)
            goto bf_loop

        end_bf_loop
            if cyc% = 2% then currency$ = mccurr$
            if cyc% = 2% then get str(currage$(j%),5,72)                 ~
                                   using L14100, currage()
L14100:         FMT 9*PD(14,4)
            if cyc% <> 2% then mat currage = billto
            write #50 using L14180, billto$, currency$, " ", addr$(),     ~
                                   currage(), bfbal, baltype$
L14180:         FMT CH(9), CH(4), CH(12), 6*CH(31), 10*PD(14,4), CH(1)


*        Now get open items not part of balance forward.
                plowkey$ = str(billto$) & hex(00)
                if baltype$ = "B" or summary$ = "D" then tb_detail_loop

*        Print Summary of Open Items.
        tb_loop
            curcode$ = " "
            call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
                if f1%(1) = 0% then end_tb_loop

            get #1 using L14480, stlmnt$, netdue$, po$, acct$, type$,     ~
                                srcedoc$, postdate$, docdate$
L14480:         FMT XX(9), CH(12), POS(37), CH(6), CH(16), POS(76),      ~
                    CH(9), POS(87), CH(2), CH(8), CH(6), CH(6)
            str(plowkey$,20,2) = hex(ffff)
            if str(stlmnt$,11,2) <> "00" then tb_loop

                if acct$(1) = "ALL" then L14560
                   if acctie$ = "I" then L14549
                     if acct$ < acctu$(1) then L14560
                     if acct$ > acctu$(2) then L14560
                        goto tb_loop
L14549:              if acct$ < acctu$(1) then tb_loop
                     if acct$ > acctu$(2) then tb_loop

L14560:     if postdate$ <= last_stmntu$ and baltype$ = "B" then tb_loop

            call "ARMCBLNC" (billto$, str(stlmnt$,,12), asofu$,          ~
                  10%, "N", #1, #5, bals(), #44, #43, #45, curcode$,     ~
                  cdate$, coneqv, conunt, cbals())

            if abs(cbals(1)) < .01 or abs(bals(1)) < .01 then tb_loop
            if curcode$ = " " then curcode$ = stat$
            if cyc% = 2% and curcode$ <> mccurr$ then tb_loop
            if cyc% = 2% then currency$ = mccurr$                        ~
                         else currency$ = curcode$

            write #50 using L14840, billto$, currency$, stlmnt$, srcedoc$,~
                      docdate$, type$, po$, netdue$, cbals(1)
L14840:         FMT CH(9), CH(4), CH(12), CH(8), CH(6), CH(2), CH(16),   ~
                    CH(6), PD(14,4)
            goto tb_loop



*        Print Detail of open items (not part of balance forward).
        tb_detail_loop
            curcode$ = " "
            call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
                if f1%(1) = 0% then end_tb_loop
            get #1 using L15120, stlmnt$, netdue$, po$, cbals(1), acct$,  ~
                                type$, srcedoc$, postdate$, docdate$

L15120:         FMT XX(9), CH(12), POS(37), CH(6), CH(16), POS(68),      ~
                    PD(14,4), POS(76), CH(9), POS(87), CH(2), CH(8),     ~
                    CH(6), CH(6)
            if str(stlmnt$,11,2) <> "00" then L15180
                if acct$(1) = "ALL" then L15180
                   if acctie$ = "I" then L15174
                     if acct$ < acctu$(1) then L15180
                     if acct$ > acctu$(2) then L15180
L15171:                 str(plowkey$,20,2) = hex(ffff)
                        goto tb_detail_loop
L15174:              if acct$ < acctu$(1) then L15171
                     if acct$ > acctu$(2) then L15171

L15180:     if str(stlmnt$,11,2) = "00" then netdue00$ = netdue$
            if postdate$ <= last_stmntu$ and baltype$ = "B" then         ~
                            tb_detail_loop
            if postdate$ > asofu$ then tb_detail_loop
            if curr$ <> "Y" then L15460
                call "READ100" (#44, key(#1), f1%(1))
                     if f1%(1) = 0% then L15360
                     get #44 using L15340, curcode$, cbals(1)
L15340:                   FMT CH(4), POS(26), PD(14,4)
L15360:         if curcode$ = " " then curcode$ = stat$
                if curcode$ <> mccurr$ then tb_detail_loop
                if cyc% = 2% then currency$ = mccurr$                    ~
                             else currency$ = curcode$

L15460:     write #50 using L14840, billto$, currency$, stlmnt$, srcedoc$,~
                      docdate$, type$, po$, netdue$, cbals(1)

            goto tb_detail_loop

        end_tb_loop
            plowkey$ = str(billto$) & hex(ff)
            if cyc% = 2% then goto L13260
            goto select_loop

        REM *************************************************************~
            *             P R I N T   S T A T E M E N T S               *~
            *-----------------------------------------------------------*~
            * Reads work file and prints statements.                    *~
            *************************************************************
        print_statements
            line% = 99%  :  page% = 0%
            lastcurr$, lastcust$ = " "
            init(hex(00)) readkey$
            call "SHOSTAT" ("Printing Customer Statements")
            call "READ104" (#50, readkey$, f1%(50))
            goto L16150

        work_loop
            call "READNEXT" (#50, f1%(50))
L16150:         if f1%(50) = 0 then wrap_up
            get #50 using L16170, billto$, currency$, stlmnt$
L16170:         FMT CH(9), CH(4), CH(12)
            if billto$ <> lastcust$ or currency$ <> lastcurr$ then       ~
                          gosub print_footer
            lastcust$ = billto$  :  lastcurr$ = currency$
            if stlmnt$ <> " " then L16360
                get #50 using L16230, addr$(), saveage(), bfbal, baltype$
L16230:             FMT POS(26), 6*CH(31), 10*PD(14,4), CH(1)
                gosub page_head
                if baltype$ <> "B" then work_loop

*       **** Format & print Balance Forward line
                ageamts$(9) = " "
                call "CURRFMT" (bfbal, currency$, ageamts$(9), "N")
                print using L18070, last_stmnt$, ageamts$(9)
                print skip(1)
                line% = line% + 2%
                goto work_loop

*       **** Regular line item here
L16360:     get #50 using L16380, stlmnt$, srcedoc$, docdate$, type2$,    ~
                                 po$, netdue$, cbals(1)
L16380:         FMT POS(14), CH(12), CH(8), CH(6), CH(2), CH(16),        ~
                    CH(6), PD(14,4)
            if baltype$ = "B" or summary$ = "D" then                     ~
               gosub print_detail_line else gosub print_summary_line
            goto work_loop

        wrap_up
            if lastcust$ <> " " then gosub print_footer
            call "FILEBGON" (#50)
            close printer
            readkey$ = "ARM.LAST.STATEMENTS"
            call "READ101" (#2, readkey$, f1%(2))
            put #2 using L16510, readkey$, asofu$, " ", " "
L16510:         FMT CH(20), CH(6), CH(250), CH(224)
            if f1%(2) = 0% then write #2 else rewrite #2
            goto  exit_program


        print_detail_line
*       **** Format & print Bal Forward and Detail lines
            if line% > 55% then gosub page_head
            if str(stlmnt$,11,2) <> "00" then netdue$ = netdue00$
            call "DATEFMT" (netdue$)
            if baltype$ <> "B" and str(stlmnt$,11,2) <> "00"             ~
                                   then netdue$ = " "
            call "DATEFMT" (docdate$)
            if baltype$ = "B" or str(stlmnt$,11,2) = "00" then           ~
                stlmnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$, 9,2)      ~
                                          & "-" & str(stlmnt$,11,2)      ~
                else stlmnt$ = "            "   & str(stlmnt$,11,2)
            gosub printit
            return

        print_summary_line
*       **** Format & print Summary lines
            if line% > 55% then gosub page_head
            call "DATEFMT" (netdue$)
            call "DATEFMT" (docdate$)
            stlmnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2)           ~
                                      & "-" & str(stlmnt$,11)
            gosub printit
            return

        printit
            if type2$ = "II" then type$ = "Invoice   "
            if type2$ = "IA" then type$ = "Inv Adj   "
            if type2$ = "IC" then type$ = "CR Memo   "
            if type2$ = "IF" then type$ = "Fin Charge"
            if type2$ = "CP" then type$ = "Payment   "
            if type2$ = "CU" then type$ = "Payment   "
            if type2$ = "CA" then type$ = "Pay Adj   "
            if type2$ = "CB" then type$ = "Payment   "
            if type2$ = "CD" then type$ = "Payment   "
            item_amt$ = " "
            call "CURRFMT" (cbals(1), currency$, item_amt$, "N")

            print using L18090, stlmnt$, srcedoc$, docdate$, type$, po$,  ~
                               netdue$, item_amt$
            line% = line% + 1%
            return

*       **** Print page heading
        page_head
            if page% < 1% then L17040
                print skip(6)
                print using L18110
L17040:     page% = page% + 1%  : line% = 22%
            if curr$ <> "Y" then L17100
                if currency$ = " " then currency$ = stat$
                call "DESCRIBE" (#40, lastcurr$, currdesc$, 0%, f1%(40))
                if currdesc$ = " " then currdesc$ = "(" & currency$ & ")"

L17100:     print page
            print skip(3)
            print using L18040, billto$, asof$, page%
            print skip(2)
            if page% <> 1% then print skip(6)
            if page% <> 1% then L17190
                for a% = 1% to 6%
                     print using L18060, addr$(a%)
                next a%
L17190:     if curr$ <> "Y" then L17240
                print skip(1)
                print "CURRENCY: " & lastcurr$ & " (" & currdesc$ & ")"
                print skip(2)
                return
L17240:     print skip(4)
            return

        REM Print the aging line footer
        print_footer
            if lastcust$ = " " then return
            save_billto$ = billto$ : billto$ = lastcust$
            if line% > 55% then gosub page_head
            billto$ = save_billto$
            print skip(1)
            desc$ = " "
            call "CURRFMT" (saveage(8), lastcurr$, ageamts$(8), "Y")
            if curr$ <> "Y" then L17370
                desc$ = currdesc$
                call "STRING" addr("RJ", desc$, 40%)
L17370:     print using L18170, desc$, ageamts$(8)
            line% = line% + 2%
            print skip(60% - line%)
            print using L18130, aging%(1)+1%, aging%(1)+1%,               ~
                               aging%(2), aging%(2)+1%, aging%(3),       ~
                               aging%(3)+1%, aging%(4), aging%(4)
            print skip(1)
            for i% = 1% to 5%
                ageamts$(i%) = " "
                call "CURRFMT" (saveage(i%), lastcurr$, ageamts$(i%),"N")
            next i%

            print using L18150, ageamts$(1), ageamts$(2), ageamts$(3),    ~
                               ageamts$(4), ageamts$(5)
            page% = 0%
            return

        REM *************************************************************~
            *          I M A G E   S T A T E M E N T S                  *~
            *************************************************************

L18040: %                                                      ##########~
        ~  ########  ###
L18060: %          ###############################
L18070: %  BALANCE FORWARD AS OF ########                                ~
        ~    ###############
L18090: %############## ######## ######## ########## ################ ###~
        ~##### #############
L18110: %                                                                ~
        ~    Continued...
L18130: %       ####        ####   ####     ####   ####    ####   ####   ~
        ~       ####
L18150: % ############### ############### ############### ###############~
        ~   ###############
L18170: %  ########################################  ** TOTAL BALANCE    ~
        ~    ###############


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%, edit%)
            enabled% = 1%
                  on fieldnr% gosub L20170,         /* As Of Date       */~
                                    L20230,         /* Last Stmnt Date  */~
                                    L20350,         /* Bill-to Range    */~
                                    L20400,         /* Summary Option   */~
                                    L20450,         /* Past Due Only?   */~
                                    L20500,         /* Minimum Balance  */~
                                    L20600,         /* Age Per          */~
                                    L20700          /* GL Range         */
                     return

L20170
*        As Of Date                            ASOF$
            inpmessage$ = "Transactions posted after this date are"  &   ~
                          " ignored by the report."
            if asof$ = " " or asof$ = blankdate$ then asof$ = date$
            return

L20230
*        Last Statement Date                   LAST_STMNT$
            inpmessage$ = "Date determines cut-off for 'Balance Forward"&~
                          "' line on Statements."
            if last_stmnt$ <> " " and last_stmnt$ <> blankdate$ then return
                readkey$ = "ARM.LAST.STATEMENTS"
                call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0% then return
                     get #2 using L20310, last_stmnt$
L20310:                   FMT XX(20), CH(6)
                     call "DATEFMT" (last_stmnt$)
                     return

L20350
*        Bill-to Customer Range                CUS_RANGE$()
            inpmessage$ = "Enter range to print (ALL, FIRST, LAST)."
            if str(cus_range$()) = " " then cus_range$(1) = "ALL"
            return

L20400
*        Summary (S) or Detail (D)             SUMMARY$
            inpmessage$ = "Summary prints net balances due only."
            if summary$ = " " then summary$ = "S"
            return

L20450
*        Past Due Items Only?                  PASTDUE$
            inpmessage$ = "Enter 'Y' to list Past Due Customers Only."
            if pastdue$ = " " then pastdue$ = "N"
            return

L20500
*        Minimum Balance                       MIN_BAL$
            inpmessage$ = "Only Balances Over This Amount Will Pri" &    ~
                          "nt.  'ALL' & 'NON-ZERO' Are Also Valid."
            if edit% = 1% then min_bal$ = "NON-ZERO"
            return


L20600
*        Age Per Criteria                      AGEPER$
            inpmessage$ = "Enter 'D' to age per Discount Date, 'N' by" & ~
                          " Net Date, or 'I' by Invoice date."
            if ageper$ = " " then ageper$ = "N"
            return


L20700
*        Selected Account
            inpmessage$ = "Enter G/L Account Range or 'ALL' & Include" & ~
                          "/Exclude selection (via I or E)."
            if acctie$  = " " then acctie$  = "I"
            if acct$(1) <> " " then return
               acct$(1) = "ALL"
               acct$(2) = " "
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%, edit%)
                  str(line2$,62%) = "ARMSTMNT: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40220,         /* As Of Date       */~
                                    L40220,         /* Last Stmnt Date  */~
                                    L40220,         /* Bill-to Range    */~
                                    L40220,         /* Summary Option   */~
                                    L40220,         /* Past Due Only?   */~
                                    L40220,         /* Minimum Balance  */~
                                    L40220,         /* Age Per          */~
                                    L40220          /* GL Range         */
                  goto L40290

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40290:     accept                                                       ~
               at (01,02),                                               ~
                  "PRINT CUSTOMER STATEMENTS",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "As Of Date",                                 ~
               at (06,30), fac(lfac$( 1)), asof$                , ch(08),~
               at (06,49), "Last Settled on ",                           ~
               at (06,65), fac(hex(8c)), last_stl$              , ch(08),~
                                                                         ~
               at (07,02), "Date of Last Statements",                    ~
               at (07,30), fac(lfac$( 2)), last_stmnt$          , ch(08),~
                                                                         ~
               at (08,02), "Bill-to Customer Range", at(08,41), "to",    ~
               at (08,30), fac(lfac$( 3)), cus_range$(1)        , ch(09),~
               at (08,45), fac(lfac$( 3)), cus_range$(2)        , ch(09),~
                                                                         ~
               at (09,02), "Summary (S) or Detail (D)",                  ~
               at (09,30), fac(lfac$( 4)), summary$             , ch(01),~
                                                                         ~
               at (10,02), "Past Due Customers Only?",                   ~
               at (10,30), fac(lfac$( 5)), pastdue$             , ch(01),~
                                                                         ~
               at (11,02), "Minimum Balance",                            ~
               at (11,30), fac(lfac$( 6)), min_bal$             , ch(10),~
                                                                         ~
               at (12,02), "Age Per Date (Disc/Net/Inv)",                ~
               at (12,30), fac(lfac$( 7)), ageper$              , ch(01),~
                                                                         ~
               at (13,02), "Include/Exclude G/L Accts:",                 ~
               at (13,30), fac(lfac$( 8)), acct$(1)             , ch(12),~
               at (13,43), "to",                                         ~
               at (13,46), fac(lfac$( 8)), acct$(2)             , ch(12),~
               at (13,60), "Include/Exclude",                            ~
               at (13,76), fac(lfac$( 8)), acctie$              , ch(01),~
                                                                         ~
               at (15,02), "Aging Parameters:",                          ~
               at (15,20), "1)", at(16,20), "2)", at(17,20), "3)",       ~
               at (18,20), "4)", at(19,20), "5)", at(15,30), "Days",     ~
               at (15,23), fac(lfac$( 9)), aging%( 1)       ,pic(-#####),~
               at (16,23), fac(lfac$( 9)), aging%( 2)       ,pic(-#####),~
               at (17,23), fac(lfac$( 9)), aging%( 3)       ,pic(-#####),~
               at (18,23), fac(lfac$( 9)), aging%( 4)       ,pic(-#####),~
               at (19,23), fac(lfac$( 9)), aging%( 5)       ,pic(-#####),~
               at (15,37), fac(lfac$( 9)), aging$( 1)           , ch(20),~
               at (16,37), fac(lfac$( 9)), aging$( 2)           , ch(20),~
               at (17,37), fac(lfac$( 9)), aging$( 3)           , ch(20),~
               at (18,37), fac(lfac$( 9)), aging$( 4)           , ch(20),~
               at (19,37), fac(lfac$( 9)), aging$( 5)           , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$,                           ~
               at (22,20), fac(hex(8c)), pf8$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104080d0f10)), key(keyhit%)


               if keyhit% <> 13 then L40860
                  call "MANUAL" ("ARMSTMNT")
                  goto L40290

L40860:        if keyhit% <> 15 then L40900
                  call "PRNTSCRN"
                  goto L40290

L40900:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%, edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* As Of Date       */~
                                    L50300,         /* Last Stmnt Date  */~
                                    L50340,         /* Bill-to Range    */~
                                    L50400,         /* Summary Option   */~
                                    L50450,         /* Past Due Only?   */~
                                    L50500,         /* Minimum Balance  */~
                                    L50600,         /* Age Per          */~
                                    L50700          /* GL Range         */
                  return

L50160
*        As Of Date                            ASOF$
            call "DATEOK" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (asof$)      :  asofu$ = asof$
                call "DATUNFMT" (last_stl$)
                if asof$ < last_stl$ then errormsg$ =                    ~
                            "As Of can not be before last settling date."
                call "DATEFMT" (asof$)
                call "DATEFMT" (last_stl$)
                if edit% = 2% then                                       ~
                     call "ARMDATES" (asofu$, " ", aging%(), 1%, 1%,     ~
                                                           aging$(), 99%)
                return

L50300
*        Last Statement Date                   LAST_STMNT$
            call "DATEOK" (last_stmnt$, u3%, errormsg$)
            return

L50340
*        Bill-to Customer Range                CUS_RANGE$()
            call "TESTRNGE" (cus_range$(1), cus_range$(2),               ~
                             cus_range$(3), cus_range$(4),               ~
                             errormsg$)
            return

L50400
*        Summary (S) or Detail (D)             SUMMARY$
            if summary$ = "D" or summary$ = "S" then return
                errormsg$ = "Enter 'S' or 'D'."
                return

L50450
*        Past Due Items Only?                  PASTDUE$
            if pastdue$ = "N" or pastdue$ = "Y" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50500
*        Minimum Balance                       MIN_BAL$
            if min_bal$ = "ALL" then return
            if min_bal$ = "NON-ZERO" then return
                convert min_bal$ to temp%, data goto L50530 : goto L50560
L50530:              errormsg$ = "Enter Code or enter Minimum Balance" & ~
                                 " (-99999999 to +999999999)."
                     return
L50560:         if temp% < -9999999 or temp% > 999999999 then L50530
                convert temp% to min_bal$, pic(-#########)
                return

L50600
*        Age Per                               AGEPER$
            if pos("DIN" = ageper$) > 0% then return
                errormsg$ = "Enter 'D', 'N', or 'I'."
                return

L50700
*        Select G/L Account
            if acct$(1) = "ALL" then L50860
            if (pos("IE" = acctie$) = 0%) then acctie$ = "I"
                call "GLVALID" (acct$(1), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "From Account.."
                call "GETCODE" (#4, acct$(1), temp$, 0%, 0, f1%(4))
                if acct$(2) = " " then acct$(2) = acct$(1)
                call "GLVALID" (acct$(2), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "To Account...."
                call "GETCODE" (#4, acct$(2), temp$, 0%, 0, f1%(4))
                if acct$(2) < acct$(1) then errormsg$ = "Invalid Range."
                acctu$(1) = acct$(1) : call "GLUNFMT" (acctu$(1))
                acctu$(2) = acct$(2) : call "GLUNFMT" (acctu$(2))
                return
L50860:     acct$(2) = " " : acctu$(1) = "ALL" : acctu$(2) = " "
            acctie$ = "I"
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
            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("ARM005", " ", 0%, 1%)
            end
