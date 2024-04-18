        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  L       AAA   TTTTT  RRRR   PPPP    *~
            *  A   A  R   R  MM MM  L      A   A    T    R   R  P   P   *~
            *  AAAAA  RRRR   M M M  L      AAAAA    T    RRRR   PPPP    *~
            *  A   A  R   R  M   M  L      A   A    T    R  R   P       *~
            *  A   A  R   R  M   M  LLLLL  A   A    T    R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMLATRP - Prints Late Notices.                           *~
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
            * 12/02/86 ! Original                                 ! ERN *~
            * 06/10/88 ! Allowed specification of age by date.    ! ERN *~
            * 04/12/89 ! Change Args to ARMAGING for Int'l        ! RJM *~
            * 11/06/89 ! Changed method for printing MC notices   ! MJB *~
            * 11/22/89 ! Zip fmt, Late Msg, & print criteria chngs! JDH *~
            * 11/28/89 ! Changed message for page continuation.   ! JDH *~
            * 11/06/90 ! Added test for blank Bill-to Address to  ! MJB *~
            *          !  print Ship-to.                          !     *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            * 04/26/92 ! G/L Range Include/exclude for aging      ! KAB *~
            * 02/23/95 ! PRRs 12586,12819-Move Shostat out of loop! RJH *~
            * 02/23/95 ! PRRs 12096,13320-Fix looping problem when! RJH *~
            *          !   text lines are > 22.                   !     *~
            * 02/23/95 ! PRR 11794 - paid invoices that were due  ! RJH *~
            *          !   at the as of date are no longer listed.!     *~
            * 02/24/95 ! PRR 12491 - Added PF5 to modify Company &! RJH *~
            *          !   Address for printing.                  !     *~
	    * 10/15/97 ! Fix spelling error of receivables.       ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$(2)12, acctu$(2)12,     /* GL Range                   */~
            acctie$1,                    /* GL Range                   */~
            addr$(6)31,                  /* Bill-to Address            */~
            ageper$1,                    /* Age per (D/N/I)            */~
            aging%(10),                  /* Aging Parameters           */~
            asof$8, asofu$8,             /* As Of Date                 */~
            bals(3),                     /* Item Balances in Statutory */~
            billto$9, billto(9),         /* Bill-to Code and Balances  */~
            cage$(50)76,                 /* Currency age for ARMAGING  */~
            cbals(3),                    /* Item Balances in Currency  */~
            cdate$6,                     /* not used                   */~
            compaddr$(4)30,              /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            curr$1,                      /* Multi-Currency Enabled?    */~
            curcode$4,                   /* Currency Code              */~
            currency$4,                  /* Current Currency Code      */~
            currdesc$32,                 /* Currency Code Description  */~
            cus_range$(4)9,              /* Bill-to Customer Range     */~
            date$8, dateu$6,             /* Date for screen display    */~
            days%(5),                    /* Late Notice Days           */~
            docdate$8,                   /* Source Doc Date            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdrdate$18,                  /* Header Date                */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inpmessage2$79,              /* Informational Message      */~
            item_amt$14,                 /* Formated Item Amount       */~
            last_stl$8,                  /* Last Settling Date         */~
            lastcust$9,                  /* Last Customer Code         */~
            lastcurr$4,                  /* Last Currency Code         */~
            lfac$(20)1,lfac_pf5$1,       /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            min_bal$10,                  /* Minimum Balance            */~
            netdue$8,                    /* Srce Doc Net Due Date      */~
            pf16$16, pf4$18,pf5$18,      /* PF Key Literals            */~
            plowkey$50,                  /* A Plow Key                 */~
            po$16,                       /* Customer PO Number         */~
            print$1,                     /* Customer Print Flag        */~
            print_datetype$17,           /* Ageper Print Variable      */~
            readkey$50,                  /* Read Key                   */~
            srcedoc$8,                   /* Source Document Number     */~
            stat$4,                      /* Statutory Currency Code    */~
            statdesc$30,                 /* Statutory Currency Descr   */~
            stlmnt$14,                   /* Settlement Number          */~
            textid$(5)4,                 /* Late Notice Text IDs       */~
            txta$(28)70, txth$64,        /* Text Array & Header        */~
            type$10,                     /* Source Document Type       */~
            type2$2,                     /* 2 Ch Document Type         */~
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
            * #4  ! TXTFILE  ! System Text File                         *~
            * #5  ! CRCBUF2  ! Cash Receipts Buffer - Distribution      *~
            * #6  ! GLMAIN   ! General Ledger Master file               *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #43 ! CRCLNCUR ! Multi-Currency Line Information          *~
            * #44 ! ARMTBCEX ! Multi-Currency Trial Balance Exposure    *~
            * #45 ! CRCL2CUR ! Multi-Currency Line Information          *~
            * #50 ! WORKFILE ! Workfile for selection / printing        *~
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

            select #4,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =   11

            select #5,  "CRCBUF2",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 21,                      ~
                        alt key  1, keypos =  201, keylen =  33

            select #6,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen = 9

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
                        varc,     indexed,  recsize = 260,               ~
                        keypos =    1, keylen = 25

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))

            if min(fs%()) < 0% then exit_program

            call "OPENCHCK" (#40,  fs%(40), f2%(40), 0%, rslt$(40))
            call "OPENCHCK" (#43,  fs%(43), f2%(43), 0%, rslt$(43))
            call "OPENCHCK" (#44,  fs%(44), f2%(44), 0%, rslt$(44))
            call "OPENCHCK" (#45,  fs%(45), f2%(45), 0%, rslt$(45))

            recnbr% = val(str(rslt$(1),17,4),4)
*            We will open the workfile when we need it.

            call "GETUFBRS" addr(#4, txth$)
            textsize% = val(str(txth$,,2), 2)
            textsize% = textsize% - 64%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$, dateu$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            coneqv, conunt = 0

*        Get Date of Last Settling Run
            readkey$ = "ARM.LAST.SETTLING"
            last_stl$ = "000000"
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L09170, last_stl$
L09170:         FMT XX(20), CH(6)
            call "DATEFMT" (last_stl$)

*        Load in Days and Text IDs
            readkey$ = "ARM.LATE.NOTICE"
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then L09290
L09240:         call "ASKUSER" (2%, "LATE NOTICES",                      ~
                                "Late Notice Text must be set up prior", ~
                                "to running Late Notices.",              ~
                                "Press any PF Key to exit.")
                goto exit_program
L09290:     get #2 using L09300, days%(), textid$()
L09300:         FMT XX(20), 5*BI(4), 5*CH(4)
            if pos(hex(ff) <> str(textid$())) = 0% then L09240

*        Now determine which Days have associated text
            for d% = 1% to 5%
                if textid$(d%) = hex(ffffffff) then L09460
                     plowkey$ = "M   " & str(textid$(d%)) & "1" &        ~
                                                                hex(0000)
L09380:              call "PLOWNEXT" (#4, plowkey$, 9%, f1%(4))
                     if f1%(4) = 0% then L09460
                          get #4, str(txth$,,64), str(txta$(),,textsize%)

         /* Better Test*/ if str(txth$,15,2) = hex(0000)   then L09460
         /* Old Test   */ if str(txta$(),,textsize%) = " " then L09380
                               aging%(10) = aging%(10) + 1%
                               aging% (aging%(10)) = days%  (d%)
                               textid$(aging%(10)) = textid$(d%)

L09460:     next d%
            if aging%(10) = 0% then L09240

*        Set any other parameters required before we get crackin'
            aging%(8) = aging%(1)
            if aging%(10) = 1% then L09560
                for d% = 1% to aging%(10) - 1%
                     aging%(d%) = aging%(d%+1%)
                next d%
L09560:     aging%(aging%(10)), aging%(9) =  99999%

            curr$ = "N"
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
                if f1%(2) = 0% then  L09650
                     get #2 using L09620, curr$, stat$
L09620:                   FMT POS(21), CH(1), CH(4)
                     call "DESCRIBE" (#40, stat$, statdesc$, 0%, f1%(40))

L09650:     gosub company_address
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf16$="(16)Exit Program"
            pf5$ = " "
            init(" ") errormsg$, inpmessage$, asof$, cus_range$(),       ~
                      min_bal$, ageper$, acct$(), acctu$(), acctie$

            for fieldnr% = 1% to 5%
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10290
L10190:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10220
L10270:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10190
L10290:         gosub'151(fieldnr%, 1%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
            next fieldnr%



        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf16$ = "(16)Print Report"  :  pf4$ = " "
            pf5$  = "(5)Set Company "
            lastfieldnr% = 0%
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then gosub set_company
                  if keyhit%  = 16% then       select_records
                  if keyhit% <>  0% then       edtpg1
L11160:     fieldnr% = min(max(1%, cursor%(1) - 5%), 5%)
            if fieldnr% = lastfieldnr% then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf16$ = " "
L11250:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11250
            gosub'151(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11250
                     lastfieldnr% = fieldnr%
                     goto L11160


        set_company
          inpmessage2$ ="For this Run only, Modify the Company & Address."
L11350:     gosub'102
                  if key2hit%  =  1% then gosub company_address
                  if key2hit%  = 16% then L11400
                  if key2hit% <>  0% then L11350

L11400:     call "LINSMASH" (compaddr$())

            return

        company_address
            readkey$ = "COMPANY TITLE"
                call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0% then return
                     get #2 using L11550, compaddr$()
L11550:                   FMT XX(20), 4*CH(30)
                     call "LINSMASH" (compaddr$())

            return

        select_records
*        First open the workfile, I told you that I would.
            call "SHOSTAT" ("Selecting Overdue Items")
            call "WORKOPEN" (#50, "IO   ", recnbr% / 2%, f2%(50))
*        And off we go!
            if min_bal$ <> " " then convert min_bal$ to min_bal
            plowkey$ = cus_range$(3)

        read_loop
*        Get next record and test against selected customer range.
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
            if f1%(1) = 0% then print_notices
            if str(plowkey$,,9) > cus_range$(4) then print_notices

*        Now get agings and test against report criteria.
            billto$ = str(plowkey$,,9) : str(plowkey$,10) = hex(ff)
            call "READ100" (#3, billto$, f1%(3))
            if f1%(3) = 0% then read_loop
            get #3 using L12480, addr$(), print$
L12480:          FMT POS(40), 6*CH(30), POS(1022), CH(1)
            if print$ = "N" or print$ = "S" then read_loop
            if str(addr$()) = "BILL-TO" or str(addr$()) = " " then       ~
                               get #3 using L12496, addr$()
L12496:         FMT POS(253), 6*CH(30)
            if pos(str(addr$(6),22,9) = " ") > 0% then L12500
               addr$(6) = str(addr$(6),,26) & "-" & str(addr$(6),27,4)
L12500:     call "LINSMASH" (addr$())

            if curr$ <> "Y" then cyc% = 0% else cyc% = 1%
            call "ARMAGING" (billto$, " ", asofu$, ageper$, aging%(), #1,~
                             billto(), #44, #2, cyc%, currency$, cage$(),~
                             acctie$, acctu$(1), acctu$(2))

            if billto(9) = 0 then read_loop   /* Nothing Past Due */
            if min_bal$ <> " " and billto(8) <= min_bal                  ~
                               then read_loop

*        Determine if to print and, if so, which text to print.
            for text% = aging%(10) to 1% step -1%
                if billto(text%) >= .01 then L13050
            next text%
            goto read_loop

*        Now check for past due items and write work file
L13050:     plowkey$ = str(billto$) & hex(00)

        tb_loop
            call "PLOWNEXT" (#1, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then end_tb_loop

            get #1 using L13120, stlmnt$, netdue$, po$, acct$, type$,     ~
                                srcedoc$, docdate$
L13120:         FMT XX(9), CH(12), POS(37), CH(6), CH(16), POS(76),      ~
                    CH(9), POS(87), CH(2), CH(8), XX(6), CH(6)

            str(plowkey$,20,2) = hex(ffff)
            if str(stlmnt$,11,2) <> "00" then tb_loop

                if acct$(1) = "ALL" then L13160
                   if acctie$ = "I" then L13148
                     if acct$ < acctu$(1) then L13160
                     if acct$ > acctu$(2) then L13160
                        goto tb_loop
L13148:              if acct$ < acctu$(1) then tb_loop
                     if acct$ > acctu$(2) then tb_loop

L13160:     if netdue$ > asofu$ then tb_loop
                call "ARMCBLNC" (billto$, str(stlmnt$,,10), dateu$,      ~
                                 10%, "N", #1, #5, bals(), #44, #43, #45,~
                                 curcode$, cdate$, coneqv, conunt,       ~
                                 cbals())
            if bals(1) < .01 or cbals(1) < .01 then tb_loop

                call "ARMCBLNC" (billto$, str(stlmnt$,,10), asofu$,      ~
                                 10%, "N", #1, #5, bals(), #44, #43, #45,~
                                 curcode$, cdate$, coneqv, conunt,       ~
                                 cbals())
            if bals(1) < .01 or cbals(1) < .01 then tb_loop

                if curcode$ = " " then currency$ = stat$                 ~
                                  else currency$ = curcode$

            write #50 using L13230, billto$, currency$, stlmnt$, srcedoc$,~
                      docdate$, type$, po$, netdue$, cbals(1), addr$(),  ~
                      text%
L13230:         FMT CH(9), CH(4), CH(12), CH(8), CH(6), CH(2), CH(16),   ~
                    CH(6), PD(14,4), 6*CH(31), BI(2)
            goto tb_loop

        end_tb_loop
            plowkey$ = str(billto$) & hex(ff)
            goto read_loop

        print_notices
            gosub print_page_zero
            line% = 99%  :  page% = 1%  :  cont% = 0%
            lastcurr$, lastcust$ = " "
            init(hex(00)) readkey$
            call "SHOSTAT" ("Printing Late Notices")
            call "READ104" (#50, readkey$, f1%(50))
            goto L14060

        work_loop
            call "READNEXT" (#50, f1%(50))
L14060:         if f1%(50) = 0 then wrap_up
            get #50 using L13230, billto$, currency$, stlmnt$, srcedoc$,  ~
                    docdate$, type2$, po$, netdue$, cbals(1), addr$(),   ~
                    text%
            if billto$ <> lastcust$ or currency$ <> lastcurr$ then       ~
                          page% = 1%
            if billto$ <> lastcust$ or currency$ <> lastcurr$ then       ~
                          gosub new_page
            lastcust$ = billto$  :  lastcurr$ = currency$
            if line% < 56% then L14130
                page% = page% + 1%  :  cont% = 1%
                gosub new_page      :  cont% = 0%
L14130:     call "DATEFMT" (netdue$)
            call "DATEFMT" (docdate$)
            stlmnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2) & "-" &   ~
                      str(stlmnt$,11)
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

            print using L17820, stlmnt$, srcedoc$, docdate$, type$, po$,  ~
                               netdue$, item_amt$
            line% = line% + 1%
            goto work_loop

        wrap_up
            call "FILEBGON" (#50)
            if lastcust$ <> " " then print using L17860
            close printer
            goto inputmode

        print_page_zero
            call "DATE" addr("HL", str(readkey$,,45))
            hdrdate$ = str(readkey$,13,18)
            select printer  (90)
            call "SETPRNT"  ("ARM006", " ", 0%, 0%)

*        First Print Selection Criteria Page
            print page
            print using L17650
            print skip(2)
            print using L17651, date$
            print using L17660, asof$
            if cus_range$(1) = "ALL" then                                ~
                print using L17670, "ALL", " ", " "   else                ~
                print using L17670, cus_range$(1), "TO", cus_range$(2)
            if min_bal$ = " " then print using L17680, "NONE"             ~
                              else print using L17680, min_bal$
            if ageper$ = "D" then print_datetype$ =  "DISCOUNT DUE DATE"
            if ageper$ = "N" then print_datetype$ =  "NET DUE DATE"
            if ageper$ = "I" then print_datetype$ =  "INVOICE DATE"
            print using L17690, print_datetype$
*          IF ACCT$(1) = "ALL" THEN 14980
            if acctie$ = "I" then print using L17695, acct$(1), acct$(2)  ~
                             else print using L17696, acct$(1), acct$(2)

            return

        REM *************************************************************~
            *          P A G E   H E A D I N G                          *~
            *************************************************************
        new_page
            if lastcust$ = " " then L17070
            if cont% = 0% then print using L17860                         ~
                              else print using L17870
L17070:     print page
            gosub page_heading
            gosub print_currency
            if page% = 1% then gosub print_text
            if line% < 43% then  L17130
                 print
                 print using L17870
                 page% = page% + 1%
                 goto L17070
L17130:     gosub print_col_titles

            return

        page_heading
            print skip(2)
            print using L17700
            print skip(1)
            if page% = 1% then                                           ~
                print using L17710, compaddr$(1), hdrdate$, " " else      ~
                print using L17710, compaddr$(1), hdrdate$, page%
            print using L17710, compaddr$(2), " ", " "
            print using L17710, compaddr$(3), " ", " "
            print using L17710, compaddr$(4), " ", " "
            print skip(1)
            print using L17710, billto$, " ", " "
            print skip(1)
            for a% = 1% to 6%
                print using L17730, addr$(a%)
            next a%
            print skip(2)
            line% = 19%

            return

        print_currency
            if curr$ <> "Y" then return
                call "DESCRIBE" (#40, currency$, currdesc$, 0%, f1%(40))
                print using L17880, currdesc$
                print skip(1)
                line% = line% + 2

            return

        print_text
*       *** Print Late Notice Text
            u3% = 0%
L17470:     call "TXTPRINT" (#4, f2%(4), 90%, textid$(text%), "ARM006",  ~
                             11%, line%, 58%, "N", " ", u3%)
            if u3% = 0% then return
                page% = page% + 1%
                print page
                gosub page_heading
                goto L17470

            return

        print_col_titles
            print skip(1)
            print using L17740
            if ageper$ = "D" then  print using L17900, asof$
            if ageper$ = "N" then  print using L17910, asof$
            if ageper$ = "I" then  print using L17920, asof$
            print skip(1)
            print using L17760
            print using L17780
            print using L17800
            line% = line% + 6%
            return

        REM *************************************************************~
            *          I M A G E   S T A T E M E N T S                  *~
            *************************************************************


L17650: %---------- LATE NOTICES: SELECTION CRITERIA ----------
L17651: %               RUN DATE:  ########
L17660: %             AS OF DATE:  ########
L17670: %          FOR CUSTOMERS:  ######### ## #########
L17680: %        MINIMUM BALANCE:  ##########
L17690: %              AGING PER:  ########################
L17695: % INCLUDING G/L ACCOUNTS:  ############ TO ############
L17696: % EXCLUDING G/L ACCOUNTS:  ############ TO ############

L17700: %N O T I C E   O F   L A T E   P A Y M E N T

L17710: %    ##############################                              ~
        ~################## ###

L17730: %          ###############################

L17740: %Our records indicate that the following past due items remain op~
        ~en:


L17760: %      Our      ----- Source Document ------                    N~
        ~et

L17780: % Reference No.  Number    Date      Type    Your PO Number   Due~
        ~ Date    Balance Due
L17800: %-------------- -------- -------- ---------- ---------------- ---~
        ~-----  -------------
L17820: %############## ######## ######## ########## ################ ###~
        ~##### ##############
        %                                                                ~
        ~         Continued...
L17860: %** END OF LIST **

L17870: %** Continued on Next Page **

L17880: %All Amounts are in ##############################

L17900: %Receivables with Discount Due Dates newer than ######## are not ~
        ~shown.
L17910: %Receivables with Net Due Dates newer than ######## are not shown.

L17920: %Receivables with Invoice Dates newer than ######## are not shown.



        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            enabled% = 1%
                  on fieldnr% gosub L20160,         /* As Of Date       */~
                                    L20220,         /* Bill-to Range    */~
                                    L20410,         /* Minimum Balance  */~
                                    L20600,         /* Age Per Criteria */~
                                    L20700          /* GL Range         */
                     return

L20160
*        As Of Date                            ASOF$
            inpmessage$ = "Receivables posted after this date are"  &    ~
                          " ignored by the report."
            if asof$ = " " then asof$ = date$
            return

L20220
*        Bill-to Customer Range                CUS_RANGE$()
            inpmessage$ = "Enter range to print (ALL, FIRST, LAST)."
            if str(cus_range$()) = " " then cus_range$(1) = "ALL"
            return

L20410
*        Minimum Balance                       MIN_BAL$
            inpmessage$ = "Enter Minimum Balance to List.  Leave blank" &~
                          " to list all balances."
            return

L20600
*        Age Per                               AGEPER$
            inpmessage$ = "Enter aging date: 'D'iscount Due Date, 'N'et"&~
                          " Due Date, or 'I'nvoice Date."
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
                  str(line2$,62%) = "ARMLATRP: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40220,         /* As Of Date       */~
                                    L40220,         /* Bill-to Range    */~
                                    L40250,         /* Minimum Balance  */~
                                    L40220,         /* Age Per          */~
                                    L40220          /* GL Range         */
                  goto L40290

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40250:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40290:     accept                                                       ~
               at (01,02), "LATE NOTICE PRINTING",                       ~
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
               at (07,02), "Bill-to Customer Range", at(07,41), "to",    ~
               at (07,30), fac(lfac$( 2)), cus_range$(1)        , ch(09),~
               at (07,45), fac(lfac$( 2)), cus_range$(2)        , ch(09),~
                                                                         ~
               at (08,02), "Minimum Balance",                            ~
               at (08,30), fac(lfac$( 3)), min_bal$             , ch(10),~
                                                                         ~
               at (09,02), "Age Per Date (Disc/Net/Inv)",                ~
               at (09,30), fac(lfac$( 4)), ageper$              , ch(01),~
                                                                         ~
               at (10,02), "Include/Exclude G/L Accts:",                 ~
               at (10,30), fac(lfac$( 5)), acct$(1)             , ch(12),~
               at (10,43), "to",                                         ~
               at (10,46), fac(lfac$( 5)), acct$(2)             , ch(12),~
               at (10,60), "Include/Exclude",                            ~
               at (10,76), fac(lfac$( 5)), acctie$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$,                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf5$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104050d0f10)), key(keyhit%)

               if keyhit% <> 13% then L40905
                  call "MANUAL" ("ARMLATRP")
                  goto L40290

L40905:        if keyhit% <> 15% then L40925
                  call "PRNTSCRN"
                  goto L40290

L40925:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *       E D I T   M O D E   P F ( 5 )  C O M P A N Y        *~
            *                                                           *~
            *************************************************************

            deffn'102
                  init(hex(84)) lfac_pf5$

                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac_pf5$ = hex(81)

L41110:     accept                                                       ~
               at (01,02),                                               ~
                  "Modify Company Name",                                 ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Company Name",                                        ~
               at (06,30), fac(lfac_pf5$), compaddr$(1%)        , ch(30),~
               at (07,02),                                               ~
                  "Address:",                                            ~
               at (07,30), fac(lfac_pf5$), compaddr$(2%)        , ch(30),~
               at (08,30), fac(lfac_pf5$), compaddr$(3%)        , ch(30),~
               at (09,30), fac(lfac_pf5$), compaddr$(4%)        , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage2$         , ch(79),~
               at (22,02), "(1)Reset Company",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return   ",                              ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (key2hit%)

               if key2hit% <> 13% then L41390
                  call "MANUAL" ("ARMLATRP")
                  goto L41110

L41390:        if key2hit% <> 15% then L41430
                  call "PRNTSCRN"
                  goto L41110

L41430:        close ws
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
                                    L50300,         /* Bill-to Range    */~
                                    L50520,         /* Minimum Balance  */~
                                    L50700,         /* Age Per          */~
                                    L50800          /* GL Range         */
                  return

L50160
*        As Of Date                            ASOF$
            call "DATEOK" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (asof$)      :  asofu$ = asof$
                call "DATUNFMT" (last_stl$)
                if asof$ <= last_stl$ then errormsg$ =                   ~
                     "As Of can not be on or before last settling date."
                call "DATEFMT" (asof$)
                call "DATEFMT" (last_stl$)
                return

L50300
*        Bill-to Customer Range                CUS_RANGE$()
            call "TESTRNGE" (cus_range$(1), cus_range$(2),               ~
                             cus_range$(3), cus_range$(4),               ~
                             errormsg$)
            return

L50520
*        Minimum Balance                       MIN_BAL$
            if min_bal$ = " " then return
                convert min_bal$ to temp%, data goto L50550 : goto L50580
L50550:              errormsg$ = "Leave Blank or enter Minimum Balance" &~
                                 " (-99999999 to +999999999)."
                     return
L50580:         if temp% < -9999999 or temp% > 999999999 then L50550
                convert temp% to min_bal$, pic(-#########)
                return


L50700
*        Age Per                               AGEPER$
            if pos("NID" = ageper$) > 0% then return
                errormsg$ = "Enter 'D', 'N', or 'I'."
                return


L50800
*        Select G/L Account
            if acct$(1) = "ALL" then L50960
            if (pos("IE" = acctie$) = 0%) then acctie$ = "I"
                call "GLVALID" (acct$(1), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "From Account.."
                call "GETCODE" (#6, acct$(1), temp$, 0%, 0, f1%(6))
                if acct$(2) = " " then acct$(2) = acct$(1)
                call "GLVALID" (acct$(2), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "To Account...."
                call "GETCODE" (#6, acct$(2), temp$, 0%, 0, f1%(6))
                if acct$(2) < acct$(1) then errormsg$ = "Invalid Range."
                acctu$(1) = acct$(1) : call "GLUNFMT" (acctu$(1))
                acctu$(2) = acct$(2) : call "GLUNFMT" (acctu$(2))
                return
L50960:     acct$(2) = " " : acctu$(1) = "ALL" : acctu$(2) = " "
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
            call "SETPRNT"  ("ARM006", " ", 0%, 1%)
            end
